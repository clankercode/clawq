open Test_slack_standing_order_reconcile_support
open Lwt.Syntax
module B = Test_slack_standing_order_broadcast_pause
module N = Test_slack_standing_order_pause_notifications
module Store = Slack_standing_order_store
module J = Slack_standing_order_pause_store

let second_ts = "1700000070.000100"
let second_message = { B.journal_message with J.ts = second_ts }

let install_two e =
  let second = set "ts" (str second_ts) (B.pause ~broadcast:false ()) in
  let parent =
    B.parent |> set "reply_count" (`Int 2) |> set "latest_reply" (str second_ts)
  in
  e.roots := [ parent; B.pause (); second ];
  let old = !(e.answer) in
  e.answer :=
    fun m p ->
      if m = "conversations.replies" && Slack_huddles.field "ts" p = B.parent_ts
      then Lwt.return (200, page [ parent; B.pause (); second ])
      else old m p

let rec until e task predicate remaining =
  let* () = settle 40 in
  if predicate () then Lwt.return_unit
  else if remaining = 0 || not (Lwt.is_sleeping task) then
    Alcotest.fail "membership boundary not reached"
  else
    match
      List.filter (fun t -> Lwt.is_sleeping t.p) e.clock.timers
      |> List.sort (fun a b -> Float.compare a.at b.at)
    with
    | [] -> Alcotest.fail "no timer before membership boundary"
    | t :: _ ->
        let* () = step e.clock t.at in
        until e task predicate (remaining - 1)

let second_phase e =
  J.notification_status e.f.db e.order B.journal_message "pause_receipt"
  = Some "confirmed"

let refusals db =
  Sql.int_query db
    "SELECT count(*) FROM room_activity_ledger WHERE \
     event_type='standing_order_pause_refused'"
    []

let decision db e message expected =
  Alcotest.(check (option string))
    "durable membership decision" expected
    (J.completed_outcome db e.order message)

let pending db e message =
  decision db e message None;
  check "identity is still selected for replay" true
    (List.mem message (J.messages db e.order));
  List.iter
    (fun kind ->
      Alcotest.(check (option string))
        "no stale notification reservation" None
        (J.notification_status db e.order message kind))
    [ "pause_notice"; "pause_receipt" ]

let with_reopened e work =
  let db = Sqlite3.db_open (Option.get e.disk_path) in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () -> work db)

let ordered_restart () =
  with_env ~disk:true (fun e ->
      let* () = B.prepare e false in
      install_two e;
      let* () = pass e in
      eq "exactly 3 actual HTTP posts" 3 (List.length (N.posts e));
      eq "one epoch change" (e.order.epoch + 1) (B.current e).epoch;
      decision e.f.db e B.journal_message (Some "paused");
      decision e.f.db e second_message (Some "already_paused");
      List.iter
        (fun (m, k) ->
          Alcotest.(check (option string))
            "each notification confirmed" (Some "confirmed")
            (J.notification_status e.f.db e.order m k))
        [
          (B.journal_message, "pause_notice");
          (B.journal_message, "pause_receipt");
          (second_message, "pause_receipt");
        ];
      eq "both users checked" 2 (count e "users.info");
      let calls_at_end = List.length !(e.calls) in
      let* () = pass e in
      let* () = C.stop () in
      let db = Sqlite3.db_open (Option.get e.disk_path) in
      let manager =
        Session.create ~config:(Session.get_config e.f.base.manager) ~db ()
      in
      Lwt.finalize
        (fun () ->
          C.configure ~db ~manager ~ctx:e.ctx;
          let* () =
            drain e.clock
              (C.pass_with ~ctx:e.ctx ~http:e.http ~db ~manager e.order)
          in
          eq "reopen causes no new HTTP" calls_at_end (List.length !(e.calls));
          eq "restart no duplicate posts" 3 (List.length (N.posts e));
          B.no_relay e;
          Lwt.return_unit)
        (fun () ->
          let* () = C.stop () in
          ignore (Sqlite3.db_close db);
          Lwt.return_unit))

let faults =
  [
    "current";
    "accepting";
    "config";
    "credentials";
    "egress";
    "slack_huddles";
    "slack_huddle_content";
    "slack_standing_order_fire";
    "owner";
    "lease";
    "gap";
    "epoch";
    "identity";
    "snapshot";
    "audit";
  ]

let waited_second method_ fault () =
  with_env ~disk:true (fun e ->
      let* () = B.prepare e false in
      install_two e;
      let accepting = ref true and armed = ref false and held = ref 0. in
      let ctx =
        {
          e.ctx with
          Ctx.accepting = (fun () -> !accepting);
          limit =
            (fun ~channel_id:_ ->
              if (not !armed) && second_phase e then (
                armed := true;
                held := e.clock.now;
                Slack_read_limiter.note_retry_after (Option.get e.ctx.limiter)
                  ~key:("T123:A123:" ^ method_) 40.);
              Lwt.return_unit);
        }
      in
      let task =
        C.pass_with ~ctx ~http:e.http ~db:e.f.db ~manager:e.f.base.manager
          e.order
      in
      let other = Sqlite3.db_open (Option.get e.disk_path) in
      Lwt.finalize
        (fun () ->
          let* () =
            until e task
              (fun () ->
                !armed && (method_ = "users.info" || count e "users.info" = 2))
              100
          in
          eq "first notice and receipt already sent" 2 (List.length (N.posts e));
          check "actual shared-limiter wait" true (Lwt.is_sleeping task);
          let before = List.length !(e.calls)
          and ledger_before = refusals other in
          N.revoke e other accepting fault;
          let* () = advance e.clock (!held +. 41.) in
          let* () = drain e.clock task in
          if fault = "current" then (
            eq "current authority completes three posts" 3
              (List.length (N.posts e));
            decision other e second_message (Some "already_paused"))
          else (
            eq "no HTTP after second-member authority loss" before
              (List.length !(e.calls));
            eq "no third post" 2 (List.length (N.posts e));
            pending other e second_message);
          (* Snapshot/audit failures retain the replay owner and may log their
       indeterminate read; unlike owner loss they are not stale decisions. *)
          eq "only current indeterminate reads may append refusal ledger"
            (ledger_before
            + if List.mem fault [ "snapshot"; "audit" ] then 1 else 0)
            (refusals other);
          with_reopened e (fun db ->
              if fault = "current" then
                decision db e second_message (Some "already_paused")
              else pending db e second_message);
          B.no_relay e;
          Lwt.return_unit)
        (fun () ->
          Lwt.cancel task;
          ignore (Sqlite3.db_close other);
          Lwt.return_unit))

let revoke_return e db accepting first fault =
  if first && fault = "epoch" then (
    ignore
      (Store.deactivate db ~order_id:e.order.id (Store.Pause "U123")
         ~now:e.clock.now
      |> Result.get_ok);
    N.revoke e db accepting fault)
  else N.revoke e db accepting fault

let returned_member first method_ hard fault () =
  with_env ~disk:true (fun e ->
      let* () = B.prepare e false in
      install_two e;
      let held = ref false and accepting = ref true in
      let response, respond = Lwt.task () in
      let old = !(e.answer) in
      (e.answer :=
         fun m p ->
           let phase =
             if first then
               List.mem B.journal_message (J.messages e.f.db e.order)
             else second_phase e
           in
           if (not !held) && phase && m = method_ then (
             held := true;
             response)
           else old m p);
      let ctx = { e.ctx with Ctx.accepting = (fun () -> !accepting) } in
      let task =
        C.pass_with ~ctx ~http:e.http ~db:e.f.db ~manager:e.f.base.manager
          e.order
      in
      let other = Sqlite3.db_open (Option.get e.disk_path) in
      Lwt.finalize
        (fun () ->
          let* () = until e task (fun () -> !held) 100 in
          eq "preceding notifications"
            (if first then 0 else 2)
            (List.length (N.posts e));
          let before = List.length !(e.calls)
          and ledger_before = refusals other in
          let params =
            if method_ = "users.info" then obj [ ("user", str "U456") ]
            else obj [ ("channel", str e.order.channel_id) ]
          in
          let* status, json = old method_ params in
          let json =
            if not hard then json
            else if method_ = "users.info" then
              set "user"
                (set "is_restricted" (`Bool true) (P.get "user" json))
                json
            else set "members" (`List []) json
          in
          revoke_return e other accepting first fault;
          let state_before = (B.current e).state
          and epoch_before = (B.current e).epoch in
          Lwt.wakeup_later respond (status, json);
          let* () = drain e.clock task in
          let message = if first then B.journal_message else second_message in
          let expected =
            if hard then
              if method_ = "users.info" then "member_guest"
              else "member_not_member"
            else if first then "paused"
            else "already_paused"
          in
          if fault = "current" then (
            eq "current response gets truthful receipt" 3
              (List.length (N.posts e));
            decision other e message (Some expected);
            eq "one truthful refusal ledger"
              (ledger_before + if hard then 1 else 0)
              (refusals other);
            check "truthful actual refusal receipt" true
              (List.exists
                 (fun p ->
                   String_util.contains
                     (Slack_huddles.field "text" p)
                     (if hard then
                        if method_ = "users.info" then
                          "guest accounts can't pause"
                        else "only members of this channel can pause"
                      else if first then "is paused"
                      else "already paused"))
                 (N.posts e)))
          else (
            eq "no HTTP after returned membership loses authority" before
              (List.length !(e.calls));
            pending other e message;
            eq "no stale refusal ledger" ledger_before (refusals other);
            Alcotest.check S.state_t "no stale lifecycle transition"
              state_before (B.current e).state;
            eq "no stale epoch change" epoch_before (B.current e).epoch);
          with_reopened e (fun db ->
              if fault = "current" then decision db e message (Some expected)
              else (
                pending db e message;
                eq "reopened no stale ledger" ledger_before (refusals db)));
          B.no_relay e;
          Lwt.return_unit)
        (fun () ->
          Lwt.cancel task;
          Lwt.cancel response;
          ignore (Sqlite3.db_close other);
          Lwt.return_unit))

let writer_boundary method_ hard takeover () =
  with_env ~disk:true (fun e ->
      let* () = B.prepare e false in
      let* () = pass e in
      let fire = S.fire e.f.db e.order ~room:"R123" ~state:T.Fetching () in
      let owner = Option.get (R.current_owner e.f.db ~order_id:e.order.id) in
      let gap = R.pass_token e.f.db e.order.id in
      let config = Session.get_config e.f.base.manager in
      let snapshot =
        Sql.query e.f.db
          "SELECT id FROM access_snapshots WHERE standing_order_id=? ORDER BY \
           rowid DESC LIMIT 1"
          [ Sql.text e.order.id ]
          (fun s -> Sql_util.text_column s 0)
        |> List.hd
        |> fun id -> Option.get (Access_snapshot.get_by_id ~db:e.f.db id)
      in
      let other = Sqlite3.db_open (Option.get e.disk_path) in
      let returned = ref false and checks = ref 0 and locks = ref [] in
      let accepting = ref true in
      let authority () =
        Session.get_config e.f.base.manager == config
        && R.current_gap e.f.db gap ~connected:true
        && R.replay_fenced e.f.db ~order:e.order ~owner ~now:e.clock.now
      in
      let check_replay () =
        let current = authority () in
        if !returned then (
          incr checks;
          let rc = Sqlite3.exec other "BEGIN IMMEDIATE" in
          locks := !locks @ [ rc ];
          if rc = Sqlite3.Rc.OK then ignore (Sqlite3.exec other "ROLLBACK");
          if takeover && !checks = 1 then N.revoke e other accepting "owner");
        current
      in
      let old = !(e.answer) in
      (e.answer :=
         fun m p ->
           let+ status, json = old m p in
           if m = method_ then (
             returned := true;
             ( status,
               if not hard then json
               else if m = "users.info" then
                 set "user"
                   (set "is_restricted" (`Bool true) (P.get "user" json))
                   json
               else set "members" (`List []) json ))
           else (status, json));
      let call =
        SO.make ~capability_fence:true ~db:e.f.db
          ~current_config:(fun () -> Session.get_config e.f.base.manager)
          ~snapshot ~channel_id:e.order.channel_id ~team_id:e.order.team_id
          ~app_id:e.order.api_app_id ~http:e.http ?limiter:e.ctx.limiter
          ~priority:Slack_read_limiter.Pause
          ~before_send:(fun () ->
            if authority () then Ok () else Error SO.Dispatch_refused)
          ~audit:
            (SO.audit_label ~order_id:(Some e.order.id)
               ~unit_label:
                 ("reconcile:" ^ owner.owner_id ^ ":"
                 ^ string_of_int owner.generation))
          ()
      in
      let replay =
        Slack_standing_order_pause.
          {
            call;
            reconcile_id = owner.owner_id ^ ":" ^ string_of_int owner.generation;
            snapshot_id = snapshot.id;
            limiter = Option.get e.ctx.limiter;
            check = check_replay;
          }
      in
      let ledger_before = refusals other in
      Lwt.finalize
        (fun () ->
          let* outcome =
            drain e.clock
              (Slack_standing_order_pause.run ~manager:e.f.base.manager
                 ~now:e.clock.now ~replay ~message:B.journal_message
                 ~team_id:e.order.team_id ~app_id:e.order.api_app_id
                 ~channel_id:e.order.channel_id ~user_id:"U456"
                 ~target:(`Order e.order.id)
                 ~source:Slack_standing_order_pause.Text ())
          in
          eq "post-return and locked settlement checks" 2 !checks;
          check "successor can write before settlement but not inside it" true
            (!locks = [ Sqlite3.Rc.OK; Sqlite3.Rc.BUSY ]);
          if takeover then (
            check "takeover returns indeterminate replay fence" true
              (outcome
             = Slack_standing_order_pause.Refused
                 (Slack_standing_order_grant.Member_indeterminate "replay_fence")
              );
            pending other e B.journal_message;
            eq "takeover appends no stale refusal" ledger_before
              (refusals other);
            Alcotest.check S.state_t "takeover preserves active order" T.Active
              (B.current e).state;
            Alcotest.check S.fire_state_t "takeover preserves fetching fire"
              T.Fetching
              (Option.get (Store.runtime other ~fire_id:fire.id)).fire.state;
            with_reopened e (fun db -> pending db e B.journal_message))
          else (
            decision other e B.journal_message
              (Some
                 (if not hard then "paused"
                  else if method_ = "users.info" then "member_guest"
                  else "member_not_member"));
            eq "only hard current refusal logs a refusal"
              (ledger_before + if hard then 1 else 0)
              (refusals other);
            Alcotest.check S.state_t "current lifecycle result"
              (if hard then T.Active else T.Paused)
              (B.current e).state;
            Alcotest.check S.fire_state_t "current fire result"
              (if hard then T.Fetching else T.Skipped_inactive)
              (Option.get (Store.runtime other ~fire_id:fire.id)).fire.state);
          B.no_relay e;
          Lwt.return_unit)
        (fun () ->
          ignore (Sqlite3.db_close other);
          Lwt.return_unit))

let suite =
  [
    S.case "ordered commands survive reopen without duplicates" ordered_restart;
  ]
  @ List.concat_map
      (fun m ->
        List.map
          (fun f -> S.case (m ^ " second wait " ^ f) (waited_second m f))
          faults)
      [ "users.info"; "conversations.members" ]
  @ List.concat_map
      (fun first ->
        List.concat_map
          (fun m ->
            List.concat_map
              (fun hard ->
                List.map
                  (fun f ->
                    S.case
                      (Printf.sprintf "%s %s return hard=%b %s"
                         (if first then "first" else "second")
                         m hard f)
                      (returned_member first m hard f))
                  [
                    "current";
                    "owner";
                    "epoch";
                    "config";
                    "lease";
                    "gap";
                    "accepting";
                  ])
              [ false; true ])
          [ "users.info"; "conversations.members" ])
      [ false; true ]
  @ List.concat_map
      (fun (method_, hard) ->
        List.map
          (fun takeover ->
            S.case
              (Printf.sprintf "%s hard=%b writer takeover=%b" method_ hard
                 takeover)
              (writer_boundary method_ hard takeover))
          [ false; true ])
      [
        ("users.info", true);
        ("conversations.members", true);
        ("conversations.members", false);
      ]

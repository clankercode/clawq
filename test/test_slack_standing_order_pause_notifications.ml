open Test_slack_standing_order_reconcile_support
open Lwt.Syntax
module B = Test_slack_standing_order_broadcast_pause
module Store = Slack_standing_order_store
module Journal = Slack_standing_order_pause_store

let posts e =
  List.filter_map
    (fun (_, m, p) -> if m = "chat.postMessage" then Some p else None)
    !(e.calls)

let kind p =
  if String_util.contains (Slack_huddles.field "text" p) "Late pause receipt."
  then "pause_receipt"
  else "pause_notice"

let receipt_ts = function
  | "pause_receipt" -> "1700000201.000100"
  | _ -> "1700000200.000100"

let observe_sends e (fire : T.fire) =
  let observations = ref [] in
  let previous = !(e.answer) in
  (e.answer :=
     fun m p ->
       if m <> "chat.postMessage" then previous m p
       else
         let k = kind p in
         let owner = Option.get (R.current_owner e.f.db ~order_id:e.order.id) in
         let audit =
           SO.audit_label ~order_id:(Some e.order.id)
             ~unit_label:
               ("reconcile:" ^ owner.owner_id ^ ":"
               ^ string_of_int owner.generation)
         in
         let audited =
           Sql.int_query e.f.db
             "SELECT count(*) FROM egress_audit a JOIN access_snapshots s ON \
              s.id=a.snapshot_id WHERE a.rowid=(SELECT max(rowid) FROM \
              egress_audit) AND a.tool_name=? AND a.session_key=? AND \
              s.work_type='standing_order' AND s.standing_order_id=?"
             Sql.
               [
                 text audit;
                 text ("slack:" ^ e.order.channel_id);
                 text e.order.id;
               ]
           = 1
         in
         observations :=
           ( k,
             audited
             && (B.current e).state = T.Paused
             && (not (covered e))
             && Journal.completed_outcome e.f.db e.order B.journal_message
                = Some "paused"
             && Journal.notification_status e.f.db e.order B.journal_message k
                = Some "attempted"
             && (Option.get (Store.runtime e.f.db ~fire_id:fire.T.id)).fire
                  .state = T.Skipped_inactive )
           :: !observations;
         Lwt.return (200, S.ok [ ("ts", str (receipt_ts k)) ]));
  observations

let assert_notifications e =
  eq "one grantor notice and one late receipt reach actual HTTP" 2
    (List.length (posts e));
  let times =
    List.filter_map
      (fun (at, m, _) -> if m = "chat.postMessage" then Some at else None)
      !(e.calls)
  in
  check "fixed notifications share the channel post pacer" true
    (match times with [ a; b ] -> b -. a >= 1. | _ -> false);
  List.iter
    (fun k ->
      let matching = List.filter (fun p -> kind p = k) (posts e) in
      eq (k ^ " sent exactly once") 1 (List.length matching);
      let p = List.hd matching in
      Alcotest.(check string)
        "fixed destination channel" e.order.channel_id
        (Slack_huddles.field "channel" p);
      let root =
        if k = "pause_receipt" then B.parent_ts
        else e.order.management_thread_ts
      in
      Alcotest.(check string)
        "original notification thread" root
        (Slack_huddles.field "thread_ts" p);
      let row =
        Sql.query e.f.db
          "SELECT status,receipt_ts FROM slack_standing_order_pause_receipts \
           WHERE order_id=? AND message_ts=? AND kind=?"
          Sql.[ text e.order.id; text B.pause_ts; text k ]
          (fun s -> (Sql_util.text_column s 0, Sql_util.text_column s 1))
      in
      Alcotest.(check (list (pair string string)))
        "durable confirmation captures actual HTTP timestamp"
        [ ("confirmed", receipt_ts k) ]
        row)
    [ "pause_notice"; "pause_receipt" ]

let integrated ~maintenance ~broadcast () =
  with_env ~disk:true (fun e ->
      let* () = B.prepare e maintenance in
      let fire = S.fire e.f.db e.order ~room:"R123" ~state:T.Fetching () in
      let observations = observe_sends e fire in
      B.install e B.Paired (B.pause ~broadcast ());
      let* () = pass e in
      check "first missed-pause observation closes coverage" false (covered e);
      let* () = if maintenance then pass e else Lwt.return_unit in
      Alcotest.check S.state_t "actual integrated pause" T.Paused
        (B.current e).state;
      eq "one epoch increment" (e.order.epoch + 1) (B.current e).epoch;
      eq "one immutable paired identity" 1 (B.journal_count e);
      assert_notifications e;
      check "pause, barrier, fire sweep and attempt commit before both HTTPs"
        true
        (List.length !observations = 2 && List.for_all snd !observations);
      let* () = pass e in
      let* () = C.stop () in
      let reopened = Sqlite3.db_open (Option.get e.disk_path) in
      let manager =
        Session.create
          ~config:(Session.get_config e.f.base.manager)
          ~db:reopened ()
      in
      Lwt.finalize
        (fun () ->
          C.configure ~db:reopened ~manager ~ctx:e.ctx;
          let* () =
            drain e.clock
              (C.pass_with ~ctx:e.ctx ~http:e.http ~db:reopened ~manager
                 (Option.get (Store.find reopened e.order.id)))
          in
          E.start ~ctx:e.ctx ~db:reopened ~manager ~http:e.http
            ~upload_http:(raw_http e) ();
          let* () = advance e.clock (e.clock.now +. 601.) in
          assert_notifications e;
          eq "no recovery batch after pause" 0
            (Sql.int_query reopened
               "SELECT count(*) FROM slack_standing_order_recovery_batches" []);
          B.no_relay e;
          Lwt.return_unit)
        (fun () ->
          let* () = C.stop () in
          let* () = E.stop () in
          ignore (Sqlite3.db_close reopened);
          Lwt.return_unit))

let suite =
  List.concat_map
    (fun maintenance ->
      List.map
        (fun broadcast ->
          S.case
            (Printf.sprintf "%s %s notifications through production pass"
               (if maintenance then "maintenance" else "gap")
               (if broadcast then "broadcast" else "plain"))
            (integrated ~maintenance ~broadcast))
        [ false; true ])
    [ false; true ]

let rec reach_wait e task kind remaining =
  let* () = settle 40 in
  if
    Journal.notification_status e.f.db e.order B.journal_message kind
    = Some "attempted"
  then Lwt.return_unit
  else if remaining = 0 || not (Lwt.is_sleeping task) then
    Alcotest.fail "production pass never reached reserved notification wait"
  else
    let next =
      List.filter (fun t -> Lwt.is_sleeping t.p) e.clock.timers
      |> List.sort (fun a b -> Float.compare a.at b.at)
      |> List.hd
    in
    let* () = step e.clock next.at in
    reach_wait e task kind (remaining - 1)

let revoke e db accepting fault =
  let config f = F.set_config e.f f in
  match fault with
  | "current" -> ()
  | "accepting" -> accepting := false
  | "config" ->
      config (fun c ->
          { c with default_temperature = c.default_temperature +. 0.01 })
  | "egress" ->
      config (fun c ->
          { c with egress = { c.egress with default_allowlist = [] } })
  | "credentials" ->
      config (fun c -> S.with_slack c (fun s -> { s with bot_token = "" }))
  | "slack_huddles" | "slack_huddle_content" | "slack_standing_order_fire" ->
      config (fun c -> S.profile c ~denied:[ fault ] ())
  | "owner" ->
      Sql.exec db
        "UPDATE slack_standing_order_recovery_passes SET \
         owner='successor',generation=generation+1 WHERE order_id=?"
        [ Sql.text e.order.id ]
  | "lease" ->
      Sql.exec db
        "UPDATE slack_standing_order_recovery_passes SET until=? WHERE \
         order_id=?"
        [ Sqlite3.Data.FLOAT e.clock.now; Sql.text e.order.id ]
  | "gap" ->
      let token =
        R.close_gap db ~team_id:e.order.team_id ~app_id:e.order.api_app_id
          ~channel_id:e.order.channel_id ~cause:"notification-wait"
          ~now:e.clock.now
        |> Result.get_ok
      in
      check "successor gap is connected but has a different generation" true
        (R.mark_connected db ~token)
  | "epoch" ->
      ignore
        (Store.resume db ~order_id:e.order.id
           ~expected_epoch:(B.current e).epoch
           ~active_since:
             (Slack_standing_order_reconcile_scan.timestamp e.clock.now)
           ~bot_user_id:(Option.get e.order.bot_user_id)
           ~bot_id:(Option.get e.order.bot_id)
           ~now:e.clock.now
        |> Result.get_ok)
  | "identity" ->
      Sql.exec db
        "UPDATE slack_standing_orders SET api_app_id='AOTHER' WHERE id=?"
        [ Sql.text e.order.id ]
  | "snapshot" ->
      Sql.exec db "DELETE FROM access_snapshots WHERE standing_order_id=?"
        [ Sql.text e.order.id ]
  | "audit" ->
      Sql.exec db
        "CREATE TRIGGER fail_notification_audit BEFORE INSERT ON egress_audit \
         BEGIN SELECT RAISE(FAIL,'notification-audit-fault'); END"
        []
  | _ -> assert false

let waited_notification kind fault () =
  with_env ~disk:true (fun e ->
      let* () = B.prepare e false in
      let fire = S.fire e.f.db e.order ~room:"R123" ~state:T.Fetching () in
      let observations = observe_sends e fire in
      B.install e B.Paired (B.pause ());
      let accepting = ref true and armed = ref false and held_at = ref 0. in
      let ctx =
        {
          e.ctx with
          Ctx.accepting = (fun () -> !accepting);
          limit =
            (fun ~channel_id:_ ->
              if
                (not !armed)
                && Journal.notification_status e.f.db e.order B.journal_message
                     kind
                   = Some "attempted"
              then (
                armed := true;
                held_at := e.clock.now;
                Slack_read_limiter.note_retry_after (Option.get e.ctx.limiter)
                  ~key:"T123:A123:chat.postMessage" 40.);
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
          let* () = reach_wait e task kind 40 in
          let expected = if kind = "pause_notice" then 0 else 1 in
          eq "nothing crosses selected shared limiter wait" expected
            (List.length (posts e));
          check "actual limiter cooldown was installed" true !armed;
          check "production pass is waiting after pause commit" true
            (Lwt.is_sleeping task);
          check "real pause is committed" true ((B.current e).state = T.Paused);
          let before = List.length !(e.calls) in
          revoke e other accepting fault;
          let* () = advance e.clock (!held_at +. 41.) in
          let* () = drain e.clock task in
          if fault = "current" then (
            assert_notifications e;
            check "both sends observe committed state after >30s heartbeat" true
              (List.length !observations = 2 && List.for_all snd !observations))
          else (
            eq "zero notification HTTP after authority loss" expected
              (List.length (posts e));
            eq "zero other HTTP after authority loss" before
              (List.length !(e.calls));
            check "failed attempt is never recorded as confirmed" true
              (Journal.notification_status other e.order B.journal_message kind
              <> Some "confirmed"));
          check "durable pause outcome survives refusal" true
            (Journal.completed_outcome other e.order B.journal_message
            = Some "paused");
          check "coverage stays closed" false (covered e);
          B.no_relay e;
          Lwt.return_unit)
        (fun () ->
          Lwt.cancel task;
          ignore (Sqlite3.db_close other);
          Lwt.return_unit))

let suite =
  suite
  @ List.concat_map
      (fun kind ->
        List.map
          (fun fault ->
            S.case
              (kind ^ " post-wait " ^ fault)
              (waited_notification kind fault))
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
          ])
      [ "pause_notice"; "pause_receipt" ]

let dispatched_capture fault () =
  with_env (fun e ->
      let* () = B.prepare e false in
      B.install e B.Paired (B.pause ());
      let response, respond = Lwt.task () in
      let previous = !(e.answer) in
      (e.answer :=
         fun m p -> if m = "chat.postMessage" then response else previous m p);
      let accepting = ref true in
      let ctx = { e.ctx with Ctx.accepting = (fun () -> !accepting) } in
      let task =
        C.pass_with ~ctx ~http:e.http ~db:e.f.db ~manager:e.f.base.manager
          e.order
      in
      Lwt.finalize
        (fun () ->
          let* () = reach_wait e task "pause_notice" 40 in
          eq "notice actually dispatched before revocation" 1
            (List.length (posts e));
          revoke e e.f.db accepting fault;
          Lwt.wakeup_later respond
            (200, S.ok [ ("ts", str (receipt_ts "pause_notice")) ]);
          let* () = drain e.clock task in
          eq "no new receipt transport after revocation" 1
            (List.length (posts e));
          Alcotest.(check (option string))
            "already dispatched success captured" (Some "confirmed")
            (Journal.notification_status e.f.db e.order B.journal_message
               "pause_notice");
          let ts =
            Sql.query e.f.db
              "SELECT receipt_ts FROM slack_standing_order_pause_receipts \
               WHERE order_id=? AND kind='pause_notice'"
              [ Sql.text e.order.id ]
              (fun s -> Sql_util.text_column s 0)
          in
          Alcotest.(check (list string))
            "actual response timestamp survives revocation"
            [ receipt_ts "pause_notice" ]
            ts;
          B.no_relay e;
          Lwt.return_unit)
        (fun () ->
          Lwt.cancel task;
          Lwt.cancel response;
          Lwt.return_unit))

let unknown_delivery malformed () =
  with_env ~disk:true (fun e ->
      let* () = B.prepare e false in
      B.install e B.Paired (B.pause ());
      let previous = !(e.answer) in
      (e.answer :=
         fun m p ->
           if m = "chat.postMessage" then
             Lwt.return (if malformed then (200, S.ok []) else (503, obj []))
           else previous m p);
      let* () = pass e in
      eq "both fixed writes actually attempted" 2 (List.length (posts e));
      List.iter
        (fun kind ->
          Alcotest.(check (option string))
            "ambiguous result is unknown" (Some "unknown")
            (Journal.notification_status e.f.db e.order B.journal_message kind))
        [ "pause_notice"; "pause_receipt" ];
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
          let ordinary_calls = List.length !(e.f.calls) in
          let* () =
            Slack_standing_order_pause.post_receipt ~manager ~order:e.order
              ~message:B.journal_message ~channel_id:e.order.channel_id
              ~thread_ts:B.parent_ts "fixed receipt"
          in
          eq "reopen and direct retry cannot resend unknown" 2
            (List.length (posts e));
          eq "unknown retry cannot reach ordinary transport" ordinary_calls
            (List.length !(e.f.calls));
          check "pause remains committed" true ((B.current e).state = T.Paused);
          B.no_relay e;
          Lwt.return_unit)
        (fun () ->
          let* () = C.stop () in
          ignore (Sqlite3.db_close db);
          Lwt.return_unit))

let refused_member () =
  with_env (fun e ->
      let* () = B.prepare e false in
      B.install e B.Paired (B.pause ());
      let previous = !(e.answer) in
      (e.answer :=
         fun m p ->
           if m = "users.info" && Slack_huddles.field "user" p = "U456" then
             let+ status, json = previous m p in
             ( status,
               set "user"
                 (set "is_restricted" (`Bool true) (P.get "user" json))
                 json )
           else previous m p);
      let* () = pass e in
      check "refused member never pauses" true ((B.current e).state = T.Active);
      eq "one refusal receipt, no grantor notice" 1 (List.length (posts e));
      let p = List.hd (posts e) in
      check "receipt truthfully reports refusal" true
        (String_util.contains
           (Slack_huddles.field "text" p)
           "Pause not applied:");
      Alcotest.(check string)
        "refusal stays in original thread" B.parent_ts
        (Slack_huddles.field "thread_ts" p);
      Alcotest.(check (option string))
        "durable outcome is refusal" (Some "member_guest")
        (Journal.completed_outcome e.f.db e.order B.journal_message);
      B.no_relay e;
      Lwt.return_unit)

let suite =
  suite
  @ List.map
      (fun fault ->
        S.case
          ("dispatched notice capture after " ^ fault)
          (dispatched_capture fault))
      [ "owner"; "epoch"; "config"; "accepting"; "slack_huddles" ]
  @ [
      S.case "HTTP failure remains unknown across reopen"
        (unknown_delivery false);
      S.case "malformed success remains unknown across reopen"
        (unknown_delivery true);
      S.case "member refusal stays truthful" refused_member;
    ]

let two_commands () =
  with_env (fun e ->
      let* () = B.prepare e false in
      let second_ts = "1700000070.000100" in
      let second = set "ts" (str second_ts) (B.pause ~broadcast:false ()) in
      let parent =
        B.parent
        |> set "reply_count" (`Int 2)
        |> set "latest_reply" (str second_ts)
      in
      e.roots := [ parent; B.pause (); second ];
      let previous = !(e.answer) in
      (e.answer :=
         fun m p ->
           if
             m = "conversations.replies"
             && Slack_huddles.field "ts" p = B.parent_ts
           then Lwt.return (200, page [ parent; B.pause (); second ])
           else previous m p);
      let* () = pass e in
      check "both commands collected before replay" true (B.journal_count e = 2);
      check "first command commits actual Pause" true
        ((B.current e).state = T.Paused);
      eq "only one lifecycle increment" (e.order.epoch + 1) (B.current e).epoch;
      eq "one notice plus both required actual receipts" 3
        (List.length (posts e));
      eq "one grantor notice" 1
        (List.length (List.filter (fun p -> kind p = "pause_notice") (posts e)));
      let receipts =
        List.filter (fun p -> kind p = "pause_receipt") (posts e)
      in
      eq "each command gets a late receipt" 2 (List.length receipts);
      List.iter
        (fun p ->
          Alcotest.(check string)
            "same original thread for each command" B.parent_ts
            (Slack_huddles.field "thread_ts" p))
        receipts;
      let message = { B.journal_message with Journal.ts = second_ts } in
      Alcotest.(check (option string))
        "second decision truthfully records already paused"
        (Some "already_paused")
        (Journal.completed_outcome e.f.db e.order message);
      check "second receipt describes already paused" true
        (List.exists
           (fun p ->
             String_util.contains
               (Slack_huddles.field "text" p)
               "is already paused.")
           receipts);
      eq "both actors checked under fail-safe member authority" 2
        (count e "users.info");
      let* () = pass e in
      eq "repeat cannot duplicate either identity" 3 (List.length (posts e));
      B.no_relay e;
      Lwt.return_unit)

let suite =
  suite
  @ [
      S.case
        "ordered multiple commands retain member checks and receipts after \
         Pause"
        two_commands;
    ]

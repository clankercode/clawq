open Test_slack_standing_order_scan_support
open Lwt.Syntax
module Pause = Slack_standing_order_pause

let validate e ctx owner order =
  unwrap
    (Lwt_main.run
       (Grant.evaluate ~db:e.db
          ~current_config:(fun () -> Session.get_config e.manager)
          ~http:e.mock.http ?limiter:ctx.Ctx.limiter
          ~before_send:(fun () ->
            if R.replay_fenced e.db ~order ~owner ~now:(Store.now ()) then Ok ()
            else Error SO.Dispatch_refused)
          ~now:(Store.now ())
          ~stage:(Grant.Reconcile { reconcile_id = Scan.reconcile_id owner })
          ~unit_label:"scan-regression"
          (Grant.subject_of_order order)))

(* Independent connections to a real file, with all authority initialized by
   the ordinary Scan fixture before the snapshot. No live database is used. *)
let with_file_scan f =
  with_scan (fun e ctx owner token _ ->
      let path = Filename.temp_file "scan-fences" ".db" in
      Fun.protect
        ~finally:(fun () -> Sys.remove path)
        (fun () ->
          Sql.exec e.db "VACUUM INTO ?" [ Sql.text path ];
          let db = Sqlite3.db_open path in
          let other = Sqlite3.db_open path in
          Fun.protect
            ~finally:(fun () ->
              ignore (Sqlite3.db_close other);
              ignore (Sqlite3.db_close db))
            (fun () ->
              let manager =
                Session.create ~config:(Session.get_config e.manager) ~db ()
              in
              let e = { e with db; manager } in
              f e ctx owner token (validate e ctx owner e.order) other path)))

let begin_again e ctx owner token ~oldest ~latest =
  check "start coverage" true
    (R.begin_scan e.db ~order:e.order ~owner ~mode:R.Gap_recovery ~gap:token
       ~oldest ~latest ~now:(Store.now ()));
  validate e ctx owner e.order

let reconnect db (order : T.order) =
  let token =
    unwrap
      (R.close_gap db ~team_id:order.team_id ~app_id:order.api_app_id
         ~channel_id:order.channel_id ~cause:"test reconnect"
         ~now:(Store.now ()))
  in
  check "new generation connected" true (R.mark_connected db ~token);
  token

let test_thread_coverage kind () =
  with_file_scan (fun e ctx owner token v other _ ->
      S.override e.mock "conversations.history" (fun _ ->
          ( 200,
            page [ set "reply_count" (`Int 1) (message ~ts:parent_ts "parent") ]
          ));
      S.override e.mock "conversations.replies" (fun _ -> (200, page []));
      unwrap (sweep e ctx owner v);
      unwrap (discover e ctx owner v);
      check "first scan complete" true
        (R.finish_scan e.db ~order_id:e.order.id ~owner ~now:(Store.now ()));
      let reads = count e "conversations.replies" in
      let previous = Option.get (R.load_coverage other ~order_id:e.order.id) in
      S.at (Store.now () +. if kind = "new_owner" then 121. else 10.);
      let owner =
        if kind = "new_owner" then
          Option.get
            (R.claim other ~order_id:e.order.id ~owner_id:"successor"
               ~now:(Store.now ()))
        else owner
      in
      let token =
        if kind = "new_gap" || kind = "new_owner" then reconnect other e.order
        else token
      in
      let oldest, latest =
        if kind = "same_window" || kind = "new_gap" then
          (previous.oldest, previous.latest)
        else if kind = "oldest" then ("1700000000.500000", previous.latest)
        else (previous.oldest, Scan.timestamp (Store.now ()))
      in
      let v = begin_again e ctx owner token ~oldest ~latest in
      S.override e.mock "conversations.replies" (fun _ ->
          (200, page [ message ~thread:parent_ts "pause SO-1" ]));
      unwrap (sweep e ctx owner v);
      if kind = "same_window" then (
        Alcotest.check S.state_t "same coverage reuses completed thread"
          T.Active (state e);
        Alcotest.(check int)
          "same window no duplicate replies" reads
          (count e "conversations.replies"))
      else (
        Alcotest.check S.state_t "new coverage inspects and applies pause"
          T.Paused (state e);
        Alcotest.(check int)
          "new coverage rereads bounded thread" (reads + 1)
          (count e "conversations.replies")))

let suite =
  List.map
    (fun kind -> S.case ("thread coverage " ^ kind) (test_thread_coverage kind))
    [ "new_gap"; "latest"; "oldest"; "same_window"; "new_owner" ]

let pause_message =
  {
    Journal.ts = "1700000070.000001";
    thread_ts = "1700000070.000001";
    user_id = "U456";
  }

let run_pause e =
  Lwt_main.run
    (Pause.run ~manager:e.manager ~message:pause_message ~team_id:"T123"
       ~app_id:"A123" ~channel_id:"C123" ~user_id:"U456"
       ~target:(`Order e.order.id) ~source:Pause.Text ())

let resume_reopened e ctx path =
  let db = Sqlite3.db_open path in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let manager =
        Session.create ~config:(Session.get_config e.manager) ~db ()
      in
      S.at (Store.now () +. 121.);
      let paused = Option.get (Store.find db e.order.id) in
      let active =
        unwrap
          (Store.resume db ~order_id:paused.id ~expected_epoch:paused.epoch
             ~active_since:(Scan.timestamp (Store.now ()))
             ~bot_user_id:"UBOT" ~bot_id:"BOWN" ~now:(Store.now ()))
      in
      let e = { e with db; manager; order = active } in
      let owner =
        Option.get
          (R.claim db ~order_id:active.id ~owner_id:"resumed"
             ~now:(Store.now ()))
      in
      let token = reconnect db active in
      let v =
        begin_again e ctx owner token
          ~oldest:(Option.get active.active_since)
          ~latest:(Scan.timestamp (Store.now ()))
      in
      check "old pause predates resumed activation" true
        (Slack_standing_order_provenance.compare_ts pause_message.ts
           (Option.get active.active_since)
        < 0);
      unwrap (sweep e ctx owner v);
      check "completed command does not pause resumed scan" true
        (state e = T.Active
        && (Option.get (Store.find db active.id)).epoch = active.epoch);
      let before = List.length !(e.mock.calls) in
      check "duplicate event returns durable decision" true
        (match run_pause e with Pause.Already_paused _ -> true | _ -> false);
      Alcotest.check S.state_t "duplicate event cannot undo resume" T.Active
        (state e);
      Alcotest.(check int)
        "dedup does not repeat membership or notice HTTP" before
        (List.length !(e.mock.calls)))

let test_pause_atomic_failure kind () =
  with_file_scan (fun e ctx owner _ v other path ->
      let pending =
        List.map
          (fun state ->
            S.fire e.db e.order ~state
              ~room:("R" ^ String.uppercase_ascii (T.fire_state_to_string state))
              ())
          T.[ Detected; Fetching; Uploading ]
      in
      let posted =
        S.fire e.db e.order ~state:T.Posted ~room:"RPOSTED"
          ~relay_ts:"1700000002.000001" ~dispatched_at:(Store.now ()) ()
      in
      Journal.ensure_schema e.db;
      if kind = "commit" then (
        Sql.exec e.db "PRAGMA foreign_keys=ON" [];
        Sql.exec e.db "CREATE TABLE commit_parent(id INTEGER PRIMARY KEY)" [];
        Sql.exec e.db
          "CREATE TABLE commit_child(id INTEGER REFERENCES commit_parent(id) \
           DEFERRABLE INITIALLY DEFERRED)"
          [];
        Sql.exec e.db
          "CREATE TRIGGER fail_outcome AFTER UPDATE ON \
           slack_order_pause_replays WHEN NEW.status='complete' BEGIN INSERT \
           INTO commit_child VALUES(1); END"
          [])
      else
        Sql.exec e.db
          "CREATE TRIGGER fail_outcome BEFORE UPDATE ON \
           slack_order_pause_replays WHEN NEW.status='complete' BEGIN SELECT \
           RAISE(ABORT,'outcome write failed'); END"
          [];
      S.override e.mock "conversations.history" (fun _ ->
          (200, page [ message "pause SO-1" ]));
      S.override e.mock "conversations.replies" (fun _ -> (200, page []));
      check "failed outcome or commit refuses scan" true
        (Result.is_error (sweep e ctx owner v));
      let after = Option.get (Store.find other e.order.id) in
      Alcotest.check S.state_t "failed decision rolls back pause" T.Active
        after.state;
      Alcotest.(check int)
        "failed decision does not advance epoch" e.order.epoch after.epoch;
      List.iter2
        (fun (fire : T.fire) expected ->
          let r = Option.get (Store.runtime other ~fire_id:fire.id) in
          Alcotest.check S.fire_state_t "fire sweep rolls back with decision"
            expected r.fire.state)
        pending
        T.[ Detected; Fetching; Uploading ];
      check "retained fire repost fence also rolled back" false
        (Option.get (Store.runtime other ~fire_id:posted.id)).fire
          .repost_forbidden;
      check "failed decision remains pending for safe retry" true
        (Journal.remember other e.order pause_message = "pending");
      check "no notifications for rolled back pause" true (posts e = []);
      Sql.exec e.db "DROP TRIGGER fail_outcome" [];
      unwrap (sweep e ctx owner v);
      Alcotest.check S.state_t "retry commits pause" T.Paused (state e);
      check "retry commits outcome" true
        (Journal.completed_outcome other e.order pause_message = Some "paused");
      List.iter
        (fun (fire : T.fire) ->
          Alcotest.check S.fire_state_t "retry commits predispatch sweep"
            T.Skipped_inactive
            (Option.get (Store.runtime other ~fire_id:fire.id)).fire.state)
        pending;
      check "retained dispatched fire fenced on successful commit" true
        (Option.get (Store.runtime other ~fire_id:posted.id)).fire
          .repost_forbidden;
      resume_reopened e ctx path)

let test_pause_committed_reopen () =
  with_file_scan (fun e ctx owner _ v other path ->
      S.override e.mock "conversations.history" (fun _ ->
          (200, page [ message "pause SO-1" ]));
      let http ~verb ~token ~method_ ~params =
        let* response = e.mock.http ~verb ~token ~method_ ~params in
        if method_ = "chat.postMessage" then Lwt.fail Lwt.Canceled
        else Lwt.return response
      in
      let call =
        SO.make ~db:e.db
          ~current_config:(fun () -> Session.get_config e.manager)
          ~snapshot:v.snapshot ~channel_id:"C123" ~team_id:"T123" ~app_id:"A123"
          ~capability_fence:true ?limiter:ctx.Ctx.limiter ~http
          ~audit:
            (SO.audit_label ~order_id:(Some e.order.id)
               ~unit_label:"reconcile:commit-crash")
          ()
      in
      Lwt_main.run
        (Lwt.catch
           (fun () ->
             let* _ =
               Scan.sweep_with_grant ~db:e.db ~manager:e.manager ~ctx ~owner
                 ~order:e.order ~validated:{ v with Grant.call }
             in
             Alcotest.fail "expected cancellation after commit")
           (function Lwt.Canceled -> Lwt.return_unit | exn -> Lwt.fail exn));
      Alcotest.check S.state_t "pause survives lost notification" T.Paused
        (Option.get (Store.find other e.order.id)).state;
      check "committed pause has no pending replay on second handle" true
        (Journal.messages other e.order = []);
      resume_reopened e ctx path)

let suite =
  suite
  @ [
      S.case "pause outcome abort rolls back lifecycle"
        (test_pause_atomic_failure "outcome");
      S.case "pause commit failure rolls back lifecycle"
        (test_pause_atomic_failure "commit");
      S.case "committed pause survives crash reopen resume"
        test_pause_committed_reopen;
    ]

let observed_tables =
  [
    "slack_standing_orders";
    "slack_standing_order_fires";
    "slack_standing_order_recovery_candidates";
    "slack_huddles";
    "slack_order_root_schedule";
    "slack_order_scan_units";
    "slack_order_scan_status";
  ]

let snapshot db =
  List.map
    (fun table ->
      Sql.query db
        ("SELECT * FROM " ^ table ^ " ORDER BY rowid")
        []
        (fun s ->
          List.init (Sqlite3.column_count s) (fun i ->
              Sqlite3.Data.to_string_debug (Sqlite3.column s i))))
    observed_tables

let root_lease db (order : T.order) =
  Sql.query db
    "SELECT owner,dirty_revision,lease_until,lease_epoch FROM \
     slack_order_root_schedule WHERE order_id=? AND root=?"
    Sql.[ text order.T.id; text root_ts ]
    (fun s ->
      {
        Q.key = Q.key order root_ts;
        owner_id = Sql_util.text_column s 0;
        dirty_revision = Sql_util.int_column s 1;
        until = Option.get (Sql.opt_float_column s 2);
        lease_epoch = Sql_util.int_column s 3;
      })
  |> List.hd

let test_returned_response method_ change response_kind () =
  with_file_scan (fun e ctx owner token v other _ ->
      unwrap (sweep e ctx owner v);
      S.override e.mock "conversations.history" (fun _ ->
          (200, page [ !(e.root) ]));
      let held, release = Lwt.task () in
      let intercepted = ref false in
      let http ~verb ~token ~method_:method_name ~params =
        let* response = e.mock.http ~verb ~token ~method_:method_name ~params in
        if
          method_name <> method_ || !intercepted
          || method_ = "conversations.history"
             && List.assoc_opt "limit" params <> Some (`Int 1)
        then Lwt.return response
        else (
          intercepted := true;
          let* () = held in
          Lwt.return
            (match response_kind with
            | "hard" ->
                ( 200,
                  Yojson.Safe.to_string
                    (obj
                       [ ("ok", `Bool false); ("error", str "missing_scope") ])
                )
            | "transient" -> (500, "{}")
            | _ -> response))
      in
      let v =
        unwrap
          (Lwt_main.run
             (Grant.evaluate ~db:e.db
                ~current_config:(fun () -> Session.get_config e.manager)
                ~http ?limiter:ctx.Ctx.limiter ~now:(Store.now ())
                ~stage:
                  (Grant.Reconcile { reconcile_id = Scan.reconcile_id owner })
                ~unit_label:"held-response"
                (Grant.subject_of_order e.order)))
      in
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             let work =
               Scan.discover ~db:e.db ~manager:e.manager ~ctx ~owner
                 ~order:e.order ~call:v.Grant.call
             in
             let* () = Test_slack_standing_order_worker.flush 20 in
             check "actual resolver response held in transport" true
               (!intercepted && Lwt.is_sleeping work);
             (match change with
             | "gap" -> ignore (reconnect other e.order)
             | "takeover" ->
                 let lease = root_lease other e.order in
                 S.at (Store.now () +. 90.);
                 let lease =
                   Option.get
                     (Q.renew_resolution ~db:other ~lease ~now:(Store.now ()))
                 in
                 S.at (Store.now () +. 31.);
                 let successor =
                   Option.get
                     (R.claim other ~order_id:e.order.id
                        ~owner_id:"replacement-pass" ~now:(Store.now ()))
                 in
                 check
                   "recovery owner replaced while root owner remains current"
                   true
                   (successor.generation > owner.generation
                   && Q.owns_resolution ~db:other ~lease ~now:(Store.now ()))
             | "rebound_gap" ->
                 let token = reconnect other e.order in
                 check "same owner starts superseding gap" true
                   (R.begin_scan other ~order:e.order ~owner
                      ~mode:R.Gap_recovery ~gap:token
                      ~oldest:(Option.get e.order.active_since)
                      ~latest:(Scan.timestamp (Store.now ()))
                      ~now:(Store.now ()))
             | "config" -> Session.update_config e.manager (S.config ())
             | _ ->
                 check "current pass still fenced" true
                   (R.fenced other ~order_id:e.order.id ~owner
                      ~now:(Store.now ())));
             let before = snapshot other in
             let calls = List.length !(e.mock.calls) in
             Lwt.wakeup release ();
             let* result = work in
             if change <> "current" then (
               check "stale response cannot mutate lifecycle or resolution" true
                 (snapshot other = before);
               check "stale response returns lost fence" true
                 (match result with
                 | Error Scan.Lease_lost
                 | Error (Scan.Root_failure Fetch.Lost_fence) ->
                     true
                 | _ -> false);
               Alcotest.(check int)
                 "no subsequent HTTP from stale response" calls
                 (List.length !(e.mock.calls));
               Alcotest.check S.state_t "stale result cannot suspend" T.Active
                 (state e))
             else if response_kind = "hard" then (
               check "current hard error retains method and class" true
                 (match result with
                 | Error
                     (Scan.Root_failure
                        (Fetch.Authority
                           {
                             Grant.cls = Grant.Hard;
                             reason = Grant.Missing_scope m;
                           })) ->
                     m = method_
                 | _ -> false);
               Alcotest.check S.state_t "current hard failure suspends"
                 T.Suspended (state e);
               Alcotest.(check int)
                 "suspends once" (e.order.epoch + 1)
                 (Option.get (Store.find other e.order.id)).epoch)
             else if response_kind = "success" then (
               unwrap result;
               Alcotest.(check int)
                 "current successful candidate created" 1
                 (List.length (fires e));
               check "current successful inspection completes coverage" true
                 (phase e = Some "swept"))
             else (
               check "current transient remains typed failure" true
                 (Result.is_error result);
               Alcotest.check S.state_t "current transient never suspends"
                 T.Active (state e));
             ignore token;
             Lwt.return_unit)
           (fun () ->
             Lwt.cancel held;
             Lwt.return_unit)))

let suite =
  suite
  @ List.concat_map
      (fun method_ ->
        List.concat_map
          (fun change ->
            List.map
              (fun response ->
                S.case
                  (String.concat " " [ "returned"; method_; change; response ])
                  (test_returned_response method_ change response))
              [ "hard"; "transient"; "success" ])
          [ "gap"; "takeover"; "current" ])
      [
        "files.info";
        "canvases.getContent";
        "conversations.replies";
        "conversations.history";
      ]
  @ List.map
      (fun change ->
        S.case
          ("returned files.info " ^ change)
          (test_returned_response "files.info" change "hard"))
      [ "rebound_gap"; "config" ]

let test_thread_reset_commit () =
  with_file_scan (fun e ctx owner _ v other path ->
      S.override e.mock "conversations.history" (fun _ ->
          ( 200,
            page [ set "reply_count" (`Int 1) (message ~ts:parent_ts "parent") ]
          ));
      S.override e.mock "conversations.replies" (fun _ -> (200, page []));
      unwrap (sweep e ctx owner v);
      unwrap (discover e ctx owner v);
      check "old coverage finished" true
        (R.finish_scan e.db ~order_id:e.order.id ~owner ~now:(Store.now ()));
      let old = Option.get (R.load_coverage other ~order_id:e.order.id) in
      let old_token = R.pass_token other e.order.id in
      S.at (Store.now () +. 10.);
      let token = reconnect other e.order in
      Sql.exec e.db
        "CREATE TRIGGER refuse_thread_reset BEFORE DELETE ON \
         slack_standing_order_thread_work BEGIN SELECT RAISE(ABORT,'reset \
         failed'); END"
        [];
      check "new coverage refuses failed thread invalidation" false
        (R.begin_scan e.db ~order:e.order ~owner ~mode:R.Gap_recovery ~gap:token
           ~oldest:old.oldest
           ~latest:(Scan.timestamp (Store.now ()))
           ~now:(Store.now ()));
      check "coverage marker rolls back with failed thread reset" true
        (R.load_coverage other ~order_id:e.order.id = Some old
        && R.pass_token other e.order.id = old_token);
      Sql.exec e.db "DROP TRIGGER refuse_thread_reset" [];
      ignore
        (begin_again e ctx owner token ~oldest:old.oldest
           ~latest:(Scan.timestamp (Store.now ())));
      let db = Sqlite3.db_open path in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          let manager =
            Session.create ~config:(Session.get_config e.manager) ~db ()
          in
          let e = { e with db; manager } in
          let v = validate e ctx owner e.order in
          S.override e.mock "conversations.replies" (fun _ ->
              (200, page [ message ~thread:parent_ts "pause SO-1" ]));
          unwrap (sweep e ctx owner v);
          Alcotest.check S.state_t
            "reopened marker cannot retain old completion" T.Paused (state e)))

let suite =
  suite
  @ [
      S.case "thread reset and coverage marker commit together"
        test_thread_reset_commit;
    ]

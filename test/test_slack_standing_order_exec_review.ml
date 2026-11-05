open Lwt.Syntax
open Test_slack_standing_order_exec_support
module E = Slack_standing_order_exec
module W = Test_slack_standing_order_worker
module H = Test_slack_standing_order_repost_support
module Report = Slack_standing_order_report
module Grant = Slack_standing_order_grant
module Sql = Slack_standing_order_sql

let check = Alcotest.(check bool)

let pass ?http e =
  E.pass ~ctx:(ctx e) ~db:e.db ~manager:e.manager ~roots:[]
    ~http:(Option.value http ~default:e.mock.http)
    ()

let seed_tracking e unknown =
  seen e;
  ready e ();
  resolve e;
  let fire = one_fire e in
  ignore (Option.get (fetch e fire.id));
  with_claim e fire.id (fun lease r -> ignore (H.seed e lease r unknown));
  H.history e [];
  H.pickup_reads e;
  e.mock.calls := [];
  fire.id

let test_startup_replay () =
  List.iter
    (fun notes_first ->
      with_env (fun e ->
          (* No fire exists for this persisted observation. *)
          seen e;
          if notes_first then ready e ();
          let c = W.clock () in
          Lwt_main.run
            (Lwt.finalize
               (fun () ->
                 E.start ~ctx:(W.context e c) ~db:e.db ~manager:e.manager
                   ~http:e.mock.http ();
                 let* () = W.advance c 1. in
                 let observe event =
                   Slack_huddle_observer.observe_db ~db:e.db
                     ~watch:(Slack_huddle_watch.of_db e.db)
                     ~now:c.now
                     (obj
                        [
                          ("type", str "event_callback");
                          ("team_id", str "T123");
                          ("api_app_id", str "A123");
                          ("event", event);
                        ])
                 in
                 observe
                   (set "channel" (str "C123")
                      (set "type" (str "message") !(e.root)));
                 let* () = W.flush 20 in
                 observe
                   (set "channel" (str "C123")
                      (set "type" (str "message")
                         (set "thread_ts" (str root_ts) F.ready)));
                 let* () = W.flush 20 in
                 let* () = W.advance c 5. in
                 Alcotest.(check int)
                   "replay and late ready never resolve" 0
                   (List.length !(e.mock.calls));
                 Alcotest.(check int)
                   "no new fire for pre-start root" 0
                   (List.length (fires e));
                 Lwt.return_unit)
               (fun () ->
                 let* () = E.stop () in
                 W.cleanup c;
                 Lwt.return_unit))))
    [ false; true ]

let test_poll_stop () =
  with_env (fun e ->
      let id = seed_tracking e true in
      let c = W.clock () in
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             E.start ~ctx:(W.context e c) ~db:e.db ~manager:e.manager
               ~http:e.mock.http ();
             E.wake ();
             let* () = W.flush 30 in
             check "share polling has started" true (count e "files.info" > 0);
             check "lease held during poll" true ((runtime e id).claim <> None);
             let stopped = E.stop () in
             let* () = W.flush 30 in
             check "stop completes without advancing time" false
               (Lwt.is_sleeping stopped);
             let* () = stopped in
             check "lease released" true ((runtime e id).claim = None);
             check "no installed worker" true (!E.worker = None);
             W.cleanup c;
             Lwt.return_unit)
           (fun () ->
             (* Rescue a failing implementation without hanging the test run. *)
             let* () =
               if !E.worker <> None then W.advance c 120. else Lwt.return_unit
             in
             let* () = E.stop () in
             List.iter (fun (t : W.timer) -> Lwt.cancel t.promise) (W.pending c);
             Lwt.return_unit)))

let failures =
  Grant.
    [
      (Target_not_member, fun e -> S.remove_member e.mock.world "UCLAUDE");
      (Grantor_not_member, fun e -> S.remove_member e.mock.world "U123");
      ( Channel_shared,
        fun e ->
          S.override e.mock "conversations.info" (fun _ ->
              ( 200,
                S.ok
                  [
                    ( "channel",
                      obj
                        [
                          ("id", str "C123");
                          ("is_member", `Bool true);
                          ("is_private", `Bool false);
                          ("is_shared", `Bool true);
                        ] );
                  ] )) );
      ( Missing_scope "conversations.members",
        fun e ->
          S.override e.mock "conversations.members" (fun _ ->
              (200, obj [ ("ok", `Bool false); ("error", str "missing_scope") ]))
      );
    ]

let test_tracking_failures () =
  List.iter
    (fun (reason, change) ->
      with_env (fun e ->
          let id = seed_tracking e false in
          let before = runtime e id in
          change e;
          Lwt_main.run (pass e);
          let order = Option.get (Store.find e.db e.order.id) in
          check "tracking hard failure suspends" true (order.state = T.Suspended);
          check "typed suspension reason" true
            (order.suspended_reason = Some (Grant.reason_code reason));
          let after = runtime e id in
          check "tracking preserves relay and attempts" true
            (after.fire.state = before.fire.state
            && after.fire.relay_ts = before.fire.relay_ts
            && after.fire.dispatched_at = before.fire.dispatched_at
            && after.fire.reposts = before.fire.reposts
            && after.attempts = before.attempts);
          check "tracking releases claim" true (after.claim = None);
          Lwt_main.run (pass e);
          Alcotest.(check (list (pair string string)))
            "fixed report once"
            [ (Report.cause_key (Report.of_hard_reason reason), "sent") ]
            (Report.statuses ~db:e.db ~order_id:e.order.id);
          Alcotest.(check int)
            "one management report" 1
            (count e "chat.postMessage")))
    failures

let test_tracking_epoch () =
  with_env (fun e ->
      let id = seed_tracking e false in
      let changed = ref false in
      S.remove_member e.mock.world "UCLAUDE";
      let http ~verb ~token ~method_ ~params =
        let* result = e.mock.http ~verb ~token ~method_ ~params in
        if method_ = "conversations.members" && not !changed then (
          changed := true;
          Sql.exec e.db
            "UPDATE slack_standing_orders SET epoch=epoch+1 WHERE id=?"
            [ Sql.text e.order.id ]);
        Lwt.return result
      in
      Lwt_main.run (pass ~http e);
      check "epoch changed during tracking await" true !changed;
      let order = Option.get (Store.find e.db e.order.id) in
      check "new epoch stays active" true (order.state = T.Active);
      check "retained relay survives race" true
        ((runtime e id).fire.relay_ts <> None);
      Alcotest.(check (list (pair string string)))
        "no stale report" []
        (Report.statuses ~db:e.db ~order_id:e.order.id))

let test_reconcile_tracking_failure () =
  with_env (fun e ->
      let id = seed_tracking e true in
      let auths = ref 0 in
      let http ~verb ~token ~method_ ~params =
        if method_ = "auth.test" then (
          incr auths;
          if !auths = 2 then S.remove_member e.mock.world "UCLAUDE");
        e.mock.http ~verb ~token ~method_ ~params
      in
      Lwt_main.run (pass ~http e);
      check "reconcile reevaluated tracking authority" true (!auths >= 2);
      check "reconcile refusal suspends" true
        ((Option.get (Store.find e.db e.order.id)).state = T.Suspended);
      check "ambiguous dispatch retained" true
        ((runtime e id).fire.state = T.Posting_unknown);
      Alcotest.(check (list (pair string string)))
        "reconcile fixed report"
        [ ("target_not_member", "sent") ]
        (Report.statuses ~db:e.db ~order_id:e.order.id))

let test_tracking_capabilities () =
  List.iter
    (fun name ->
      List.iter
        (fun reload ->
          with_env (fun e ->
              let id = seed_tracking e false in
              let before = runtime e id in
              let denied = ref false in
              let deny () =
                denied := true;
                Session.update_config e.manager
                  (S.profile (Session.get_config e.manager) ~denied:[ name ] ());
                e.mock.calls := []
              in
              let context =
                {
                  (ctx e) with
                  Slack_standing_order_ctx.limit =
                    (fun ~channel_id:_ ->
                      if reload && not !denied then deny ();
                      Lwt.pause ());
                }
              in
              if not reload then deny ();
              Lwt_main.run
                (E.pass ~ctx:context ~db:e.db ~manager:e.manager
                   ~http:e.mock.http ~roots:[]
                   ~upload_http:(fun _ ->
                     Alcotest.fail "tracking must not upload")
                   ());
              check "tracking capability denied" true !denied;
              let order = Option.get (Store.find e.db e.order.id) in
              check "tracking capability suspension matches epoch" true
                (order.state = T.Suspended && order.epoch = e.order.epoch + 1);
              check "tracking retains dispatched attempts" true
                ((runtime e id).attempts = before.attempts);
              Alcotest.(check int)
                "zero calls after tracking capability deny" 0
                (List.length !(e.mock.calls));
              Alcotest.(check (list (pair string string)))
                "local tracking refusal"
                [
                  ( Report.cause_key
                      (Report.Authority_refused (Grant.Tool_denied name)),
                    "local_only" );
                ]
                (Report.statuses ~db:e.db ~order_id:e.order.id)))
        [ false; true ])
    Grant.capability_names

let test_cap_period () =
  with_env (fun e ->
      Report.init e.db;
      let period = Report.period_key ~now:(Store.now ()) Report.Cap_hit in
      Sql.exec e.db
        "INSERT INTO \
         slack_standing_order_reports(order_id,cause_key,period_key,status) \
         VALUES(?,'cap_hit',?,'pending')"
        [ Sql.text e.order.id; Sql.text period ];
      S.at (Store.now () +. 86400.);
      Lwt_main.run (pass e);
      let periods () =
        Sql.query e.db
          "SELECT period_key,status FROM slack_standing_order_reports WHERE \
           order_id=?"
          [ Sql.text e.order.id ]
          (fun s -> (Sql_util.text_column s 0, Sql_util.text_column s 1))
      in
      Alcotest.(check (list (pair string string)))
        "settled original period"
        [ (period, "sent") ]
        (periods ());
      S.at (Store.now () +. 86400.);
      Lwt_main.run (pass e);
      S.at (Store.now () +. 86400.);
      Lwt_main.run (pass e);
      Alcotest.(check int)
        "no daily replay without a new cap event" 1
        (count e "chat.postMessage"))

let suite =
  [
    S.case "startup replay and late ready retain recovery provenance"
      test_startup_replay;
    S.case "stop cancels a share poll without advancing time" test_poll_stop;
    S.case "tracking hard failures suspend and report with retained relay"
      test_tracking_failures;
    S.case "tracking failure cannot suspend a newer epoch" test_tracking_epoch;
    S.case "reconcile propagates its tracking refusal"
      test_reconcile_tracking_failure;
    S.case "tracking capability failures remain local across reload"
      test_tracking_capabilities;
    S.case "pending cap report retains its persisted period across midnights"
      test_cap_period;
  ]

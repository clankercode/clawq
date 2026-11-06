open Lwt.Syntax
open Test_slack_standing_order_exec_support
open Test_slack_standing_order_worker_support
module E = Slack_standing_order_exec
module R = Slack_standing_order_recovery_store
module Rollout = Slack_standing_order_rollout_store
module I = Slack_standing_order_receipt_import
module Import_fixture = Test_slack_standing_order_receipt_import
module Sql = Slack_standing_order_sql

let check = Alcotest.(check bool)

let observe e root =
  let room =
    Slack_huddle_classify.room_of_json (get "room" root) |> Option.get
  in
  let ts = Yojson.Safe.Util.to_string (get "ts" root) in
  L.record_seen ~db:e.db ~team:"T123" ~channel:"C123" ~source:"event"
    ~now:(Store.now ()) ~root_ts:ts ~file_ids:[ "FCANVAS" ] room;
  let message =
    set "type" (str "message")
      (set "channel" (str "C123") (set "thread_ts" (str ts) F.ready))
  in
  Slack_huddle_observer.observe_db ~db:e.db
    ~watch:(Slack_huddle_watch.of_db e.db)
    ~now:(Store.now ())
    (Test_slack_event_fixtures.envelope message)

let test_paired_suppression () =
  with_env (fun e ->
      let _, installation, _, _ = Rollout.read_anchor e.db in
      let original = Import_fixture.receipt e.order installation in
      let receipt =
        {
          original with
          I.root_ts;
          room_id = "R123";
          date_start = string_of_int start;
        }
      in
      let receipt =
        { receipt with I.evidence_digest = I.receipt_digest receipt }
      in
      let inventory =
        Import_fixture.inventory e.db e.order installation [ receipt ]
      in
      I.apply e.db
        (Import_fixture.validated e.db inventory [ receipt ])
        ~manifest_digest:(I.inventory_digest inventory)
      |> Import_fixture.ok |> ignore;
      let config = Session.get_config e.manager in
      let rule : Runtime_config.egress_rule =
        {
          host = "files.slack.com";
          path = Some "/upload/*";
          method_ = Some "POST";
          action = Allow;
          log_policy = No_log;
        }
      in
      Session.update_config e.manager
        {
          config with
          egress =
            {
              config.egress with
              default_allowlist = rule :: config.egress.default_allowlist;
            };
        };
      S.override e.mock "conversations.history" (fun p ->
          ( 200,
            S.ok
              [
                ( "messages",
                  if get "limit" p = `Int 1 then `List [ !(e.root) ]
                  else `List [] );
                S.no_more;
              ] ));
      S.override e.mock "files.getUploadURLExternal" (fun _ ->
          ( 200,
            S.ok
              [
                ("file_id", str "FUPLOAD");
                ("upload_url", str "https://files.slack.com/upload/fixture");
              ] ));
      S.override e.mock "files.completeUploadExternal" (fun _ ->
          ( 200,
            S.ok
              [
                ( "files",
                  `List
                    [
                      obj
                        [
                          ( "shares",
                            obj
                              [
                                ( "private",
                                  obj
                                    [
                                      ( "C123",
                                        `List
                                          [
                                            obj
                                              [
                                                ("ts", str "1700000700.000001");
                                              ];
                                          ] );
                                    ] );
                              ] );
                        ];
                    ] );
              ] ));
      let c = clock () and uploads = ref 0 in
      let control_ts = "1700000002.000100" and control_start = "1700000002" in
      let suppressed_root = !(e.root) in
      let control_root =
        set "ts" (str control_ts)
          (set "room"
             (get "room" suppressed_root
             |> set "id" (str "RCONTROL")
             |> set "date_start" (`Int (start + 1))
             |> set "thread_root_ts" (str control_ts)
             |> set "canvas_thread_ts" (str control_ts))
             suppressed_root)
      in
      let start_worker () =
        E.start ~ctx:(context e c) ~db:e.db ~manager:e.manager ~http:e.mock.http
          ~upload_http:(fun _ ->
            incr uploads;
            Lwt.return
              Slack_standing_order_upload.
                { status = 200; headers = []; body = "" })
          ()
      in
      let verify owner gap =
        check "connected" true (R.mark_connected e.db ~token:gap);
        check "fixed recovery scan" true
          (R.begin_scan e.db ~order:e.order ~owner ~mode:R.Gap_recovery ~gap
             ~oldest:"1700000000.000100" ~latest:"1700000100.000000" ~now:c.now);
        let coverage : R.coverage =
          {
            oldest = "1700000000.000100";
            latest = "1700000100.000000";
            history_cursor = "";
            pause_cursor = "";
            last_success = None;
          }
        in
        check "paired candidate page" true
          (R.save_page e.db ~order_id:e.order.id ~owner ~coverage
             ~candidates:
               [
                 (root_ts, "R123", string_of_int start);
                 (control_ts, "RCONTROL", control_start);
               ]
             ~now:c.now);
        check "pause coverage" true
          (R.complete_pause e.db ~order_id:e.order.id ~owner ~now:c.now);
        check "history coverage" true
          (R.complete_history e.db ~order_id:e.order.id ~owner ~now:c.now);
        check "finished coverage" true
          (R.finish_scan e.db ~order_id:e.order.id ~owner ~now:c.now);
        check "verified readiness and coverage" true
          (R.verify_gap e.db ~token:gap ~owner ~through:coverage.latest
             ~now:c.now)
      in
      let close () =
        R.close_gap e.db ~team_id:"T123" ~app_id:"A123" ~channel_id:"C123"
          ~cause:"test_restart" ~now:c.now
        |> Result.get_ok
      in
      let no_new_delivery () =
        Alcotest.(check int)
          "suppressed allocation calls" 0
          (count e "files.getUploadURLExternal");
        Alcotest.(check int) "suppressed raw uploads" 0 !uploads;
        Alcotest.(check int)
          "suppressed complete calls" 0
          (count e "files.completeUploadExternal");
        Alcotest.(check int)
          "suppressed creates no fire" 0
          (List.length (fires e));
        Alcotest.(check int)
          "suppressed consumes no debit" 0
          (Store.dispatches_since e.db ~order_id:e.order.id ~since:0.)
      in
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             start_worker ();
             let gap = close () in
             let owner =
               R.claim e.db ~order_id:e.order.id ~owner_id:"paired-owner"
                 ~now:c.now
               |> Option.get
             in
             verify owner gap;
             let card =
               Store.insert_card e.db ~kind:T.Pause_button ~order:e.order
                 ~created_by:"U123" ~now:c.now ()
             in
             check "paired notice batch" true
               (R.create_batch e.db ~order:e.order ~owner ~batch_id:"paired"
                  ~card_id:card.id
                  ~roots:
                    [
                      (root_ts, string_of_int start); (control_ts, control_start);
                    ]
                  ~now:c.now);
             check "reserve notice" true
               (R.reserve_notice e.db ~batch_id:"paired" ~owner ~now:c.now);
             check "confirm real hold boundary" true
               (R.bind_notice e.db ~batch_id:"paired" ~owner
                  ~ts:"1700000100.000001" ~release_at:(c.now +. 600.) ~now:c.now);
             observe e suppressed_root;
             let* () = flush 100 in
             no_new_delivery ();
             let rec hold owner n =
               if n = 0 then Lwt.return_unit
               else
                 let* () = advance c 100. in
                 check "renew hold owner" true
                   (R.renew e.db ~order_id:e.order.id ~owner ~now:c.now);
                 hold owner (n - 1)
             in
             let* () = hold owner 3 in
             let* () = E.stop () in
             start_worker ();
             let gap = close () in
             let owner =
               R.claim e.db ~order_id:e.order.id ~owner_id:"paired-restarted"
                 ~now:c.now
               |> Option.get
             in
             verify owner gap;
             check "adopt confirmed batch after restart" true
               (R.adopt_batch_owner e.db ~batch_id:"paired" ~owner ~now:c.now);
             observe e suppressed_root;
             let* () = flush 100 in
             no_new_delivery ();
             let* () = hold owner 2 in
             let* () = advance c 99. in
             check "renew before release" true
               (R.renew e.db ~order_id:e.order.id ~owner ~now:c.now);
             check "599 still held" false
               (R.release_batch e.db ~batch_id:"paired" ~owner ~now:c.now);
             let* () = advance c 1. in
             check "600 released" true
               (R.release_batch e.db ~batch_id:"paired" ~owner ~now:c.now);
             observe e suppressed_root;
             let* () =
               E.resolve_pending ~ctx:(context e c) ~db:e.db ~manager:e.manager
                 ~http:e.mock.http ~roots:[ root_ts ] e.order
             in
             let* () = flush 100 in
             no_new_delivery ();
             (* A distinct eligible instance uses exactly the same order, verified gap,
         confirmed notice and elapsed hold. No freshness or authority override. *)
             e.root := control_root;
             e.replies :=
               F.page [ control_root; set "thread_ts" (str control_ts) F.ready ];
             e.canvas :=
               set "shares"
                 (obj
                    [
                      ( "private",
                        obj
                          [
                            ( "C123",
                              `List [ F.share control_ts; F.share ready_ts ] );
                          ] );
                    ])
                 !(e.canvas);
             observe e control_root;
             let rec until n =
               if count e "files.completeUploadExternal" > 0 then
                 Lwt.return_unit
               else if n = 0 then
                 Alcotest.fail "eligible unsuppressed control did not share"
               else
                 let* () = advance c 1. in
                 until (n - 1)
             in
             let* () = until 59 in
             let* () = flush 100 in
             Alcotest.(check int)
               "one control allocation" 1
               (count e "files.getUploadURLExternal");
             Alcotest.(check int) "one control raw upload" 1 !uploads;
             Alcotest.(check int)
               "one control complete" 1
               (count e "files.completeUploadExternal");
             Alcotest.(check int)
               "one actual control debit" 1
               (Store.dispatches_since e.db ~order_id:e.order.id ~since:0.);
             let control = one_fire e in
             check "control uses durable released admission" true
               (R.relay_allowed e.db ~fire_id:control.id ~now:c.now);
             let forced =
               Store.insert_fire e.db ~order:e.order ~room_id:"R123"
                 ~date_start:(string_of_int start) ~root_ts ~now:c.now ()
               |> Result.get_ok |> Option.get
             in
             let* () =
               E.process_fire ~ctx:(context e c) ~db:e.db ~manager:e.manager
                 ~http:e.mock.http
                 ~upload_http:(fun _ ->
                   incr uploads;
                   Lwt.return
                     Slack_standing_order_upload.
                       { status = 200; headers = []; body = "" })
                 ~allow_new:true forced
             in
             Alcotest.(check int)
               "forced suppressed row adds no allocation" 1
               (count e "files.getUploadURLExternal");
             Alcotest.(check int)
               "forced suppressed row adds no raw upload" 1 !uploads;
             Alcotest.(check int)
               "forced suppressed row adds no complete" 1
               (count e "files.completeUploadExternal");
             Alcotest.(check int)
               "forced suppressed row adds no debit" 1
               (Store.dispatches_since e.db ~order_id:e.order.id ~since:0.);
             check "final durable admission refuses suppressed row" false
               (R.relay_allowed e.db ~fire_id:forced.id ~now:c.now);
             check "direct final Fire stage refuses suppressed row" true
               (match
                  Slack_standing_order_grant.stage_rows e.db ~now:c.now
                    ~stage:
                      (Slack_standing_order_grant.Fire
                         { fire_epoch = e.order.epoch; fire_id = forced.id })
                    (Slack_standing_order_grant.subject_of_order e.order)
                with
               | Error _ -> true
               | _ -> false);
             Lwt.return_unit)
           (fun () ->
             let* () = E.stop () in
             cleanup c;
             Lwt.return_unit)))

let suite =
  [
    S.case
      "suppressed instance stays inert while paired eligible worker actually \
       shares"
      test_paired_suppression;
  ]

open Lwt.Syntax
open Test_slack_standing_order_exec_support
module E = Slack_standing_order_exec
module Ctx = Slack_standing_order_ctx
module Report = Slack_standing_order_report

let check = Alcotest.(check bool)

let setup e =
  let cfg = Session.get_config e.manager in
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
      cfg with
      egress =
        {
          cfg.egress with
          default_allowlist = rule :: cfg.egress.default_allowlist;
        };
    };
  let seq = ref 0 in
  S.override e.mock "conversations.history" (fun p ->
      ( 200,
        S.ok
          [
            ( "messages",
              if get "limit" p = `Int 1 then `List [ !(e.root) ] else `List []
            );
            S.no_more;
          ] ));
  S.override e.mock "files.getUploadURLExternal" (fun _ ->
      incr seq;
      ( 200,
        S.ok
          [
            ("file_id", str ("FUPLOAD" ^ string_of_int !seq));
            ("upload_url", str "https://files.slack.com/upload/fixture");
          ] ));
  let shares =
    obj
      [
        ( "private",
          obj [ ("C123", `List [ obj [ ("ts", str "1700000100.000001") ] ]) ] );
      ]
  in
  S.override e.mock "files.completeUploadExternal" (fun _ ->
      (200, S.ok [ ("files", `List [ obj [ ("shares", shares) ] ]) ]));
  S.override e.mock "reactions.get" (fun p ->
      ( 200,
        S.ok
          [
            ( (if get "file" p = `Null then "message" else "file"),
              obj [ ("reactions", `List []) ] );
          ] ))

let raw _ =
  Lwt.return
    Slack_standing_order_upload.{ status = 200; headers = []; body = "" }

let pass ?context ?http ?(upload_http = raw) e =
  E.pass
    ~ctx:(Option.value context ~default:(ctx e))
    ~db:e.db ~manager:e.manager
    ~http:(Option.value http ~default:e.mock.http)
    ~upload_http ()

let observe e event =
  Slack_huddle_observer.observe_db ~db:e.db
    ~watch:(Slack_huddle_watch.of_db e.db)
    ~now:(Store.now ())
    (obj
       [
         ("type", str "event_callback");
         ("team_id", str "T123");
         ("api_app_id", str "A123");
         ("event", event);
       ])

let test_pass ?(sentinel = fun _ -> ()) () =
  with_env (fun e ->
      setup e;
      enable_transcript e;
      let cfg = Session.get_config e.manager in
      let config =
        {
          cfg with
          providers =
            [
              ( "executor-fixture",
                {
                  Runtime_config.default_provider_config with
                  api_key = "fixture";
                  kind = Some "openai-compat";
                  base_url = Some "http://provider.invalid";
                  default_model = Some "fixture-model";
                } );
            ];
          agent_defaults =
            {
              cfg.agent_defaults with
              primary_model = "executor-fixture:fixture-model";
            };
        }
      in
      sentinel config;
      let e = { e with manager = Session.create ~config ~db:e.db () } in
      let ingress event =
        Lwt_main.run
          (let+ _ =
             Slack.handle_event
               ~config:
                 (Option.get (Session.get_config e.manager).channels.slack)
               ~session_manager:e.manager
               (Yojson.Safe.to_string
                  (obj
                     [
                       ("type", str "event_callback");
                       ("team_id", str "T123");
                       ("api_app_id", str "A123");
                       ("event", event);
                     ]))
           in
           ())
      in
      ingress
        (set "channel" (str "C123") (set "type" (str "message") !(e.root)));
      ingress
        (set "channel" (str "C123")
           (set "type" (str "message") (set "thread_ts" (str root_ts) F.ready)));
      let uploads = ref 0 in
      Lwt_main.run
        (pass
           ~upload_http:(fun r ->
             incr uploads;
             raw r)
           e);
      let f = one_fire e in
      check_state e f.id T.Posted;
      Alcotest.(check int)
        "one complete" 1
        (count e "files.completeUploadExternal");
      Alcotest.(check int) "notes and transcript" 2 !uploads;
      check "receipt persisted" true
        ((runtime e f.id).fire.relay_ts = Some "1700000100.000001");
      ingress
        (obj
           [
             ("type", str "message");
             ("channel", str "C123");
             ("user", str "UCLAUDE");
             ("bot_id", str "BCLAUDE");
             ("app_id", str "ACLAUDE");
             ("thread_ts", str "1700000100.000001");
             ("ts", str "1700000101.000001");
             ("text", str "done");
           ]);
      Lwt_main.run (pass e);
      check_state e f.id T.Picked_up;
      Alcotest.(check int)
        "zero Agent sessions" 0
        (Hashtbl.length e.manager.sessions))

let test_provider () =
  let previous = !Provider.native_complete
  and streams = !Provider.native_stream in
  let calls = ref 0 in
  Provider.native_complete :=
    ( Provider.OpenAICompat,
      fun ~config:_
        ~provider:_
        ~model:_
        ~messages:_
        ?tools:_
        ?session_key:_
        ()
      ->
        incr calls;
        Lwt.fail_with "executor called Provider" )
    :: previous;
  Provider.native_stream :=
    ( Provider.OpenAICompat,
      fun ~config:_
        ~provider:_
        ~model:_
        ~messages:_
        ?tools:_
        ?session_key:_
        ~on_chunk:_
        ()
      ->
        incr calls;
        Lwt.fail_with "executor streamed Provider" )
    :: streams;
  Fun.protect
    ~finally:(fun () ->
      Provider.native_complete := previous;
      Provider.native_stream := streams)
    (fun () ->
      let sentinel config =
        let name, provider, model = Provider.select_provider ~config () in
        Alcotest.(check string)
          "explicit fixture selected" "executor-fixture" name;
        Alcotest.(check string) "fixture model selected" "fixture-model" model;
        check "selected native counter registered" true
          (Provider.detect_kind ~name provider = Provider.OpenAICompat);
        let fails work =
          Lwt_main.run
            (Lwt.catch
               (fun () ->
                 let+ _ = work () in
                 false)
               (function Failure _ -> Lwt.return_true | exn -> Lwt.fail exn))
        in
        check "completion sentinel reachable" true
          (fails (fun () -> Provider.complete ~config ~messages:[] ()));
        check "stream sentinel reachable" true
          (fails (fun () ->
               Provider.complete_stream ~config ~messages:[]
                 ~on_chunk:(fun _ -> Lwt.return_unit)
                 ()));
        Alcotest.(check int) "both sentinels invoked" 2 !calls;
        calls := 0
      in
      test_pass ~sentinel ();
      Alcotest.(check int) "zero Provider calls" 0 !calls)

let test_reload () =
  List.iter
    (fun change ->
      with_env (fun e ->
          setup e;
          seen e;
          ready e ();
          let changed = ref false in
          let context =
            {
              (ctx e) with
              Ctx.limit =
                (fun ~channel_id:_ ->
                  if count e "canvases.getContent" > 0 && not !changed then (
                    changed := true;
                    Session.update_config e.manager
                      (change (Session.get_config e.manager)));
                  Lwt.pause ());
            }
          in
          Lwt_main.run (pass ~context e);
          check "reload occurred inside limiter" true !changed;
          Alcotest.(check int)
            "zero completes" 0
            (count e "files.completeUploadExternal");
          Alcotest.(check int)
            "zero upload allocation" 0
            (count e "files.getUploadURLExternal")))
    [
      (fun cfg ->
        S.with_slack cfg (fun sc ->
            {
              sc with
              standing_orders =
                { sc.standing_orders with orders_enabled = false };
            }));
      (fun cfg ->
        S.with_slack cfg (fun sc ->
            {
              sc with
              standing_orders = { sc.standing_orders with allowed_targets = [] };
            }));
      (fun cfg -> S.profile cfg ~denied:[ "slack_huddle_content" ] ());
    ]

let seed_report e cause =
  Report.init e.db;
  Slack_standing_order_sql.exec e.db
    "INSERT INTO \
     slack_standing_order_reports(order_id,cause_key,period_key,status) \
     VALUES(?,?,'once','pending')"
    [ Sqlite3.Data.TEXT e.order.id; Sqlite3.Data.TEXT (Report.cause_key cause) ]

let test_capabilities ~reload () =
  List.iter
    (fun name ->
      List.iter
        (fun queued ->
          with_env (fun e ->
              setup e;
              seen e;
              ready e ();
              let cause =
                Report.Authority_refused
                  (Slack_standing_order_grant.Tool_denied name)
              in
              if queued then seed_report e Report.No_pickup;
              let changed = ref false and uploads = ref 0 in
              let deny () =
                changed := true;
                Session.update_config e.manager
                  (S.profile (Session.get_config e.manager) ~denied:[ name ] ());
                e.mock.calls := [];
                uploads := 0
              in
              let context =
                {
                  (ctx e) with
                  Ctx.limit =
                    (fun ~channel_id:_ ->
                      if
                        reload && (not !changed)
                        && count e "canvases.getContent" > 0
                      then deny ();
                      Lwt.pause ());
                }
              in
              if not reload then deny ();
              Lwt_main.run
                (pass ~context
                   ~upload_http:(fun r ->
                     incr uploads;
                     raw r)
                   e);
              Alcotest.(check int) "zero subsequent raw uploads" 0 !uploads;
              check "deny applied" true !changed;
              let suspended = Option.get (Store.find e.db e.order.id) in
              check "epoch-safe suspension" true
                (suspended.state = T.Suspended
                && suspended.epoch = e.order.epoch + 1);
              Alcotest.(check int)
                "zero subsequent Slack calls" 0
                (List.length !(e.mock.calls));
              Alcotest.(check (list (pair string string)))
                "one inspectable local notification"
                (List.sort compare
                   ((Report.cause_key cause, "local_only")
                   ::
                   (if queued then
                      [ (Report.cause_key Report.No_pickup, "local_only") ]
                    else [])))
                (Report.statuses ~db:e.db ~order_id:e.order.id);
              Session.update_config e.manager (S.config ());
              Lwt_main.run (pass e);
              Alcotest.(check int)
                "re-enable never sends local receipt" 0
                (count e "chat.postMessage")))
        [ false; true ])
    Slack_standing_order_grant.capability_names

let test_drain () =
  with_env (fun e ->
      setup e;
      seen e;
      ready e ();
      let sent, wake = Lwt.task () in
      let http ~verb ~token ~method_ ~params =
        if method_ = "files.completeUploadExternal" then (
          e.accepting := false;
          let* () = sent in
          e.mock.http ~verb ~token ~method_ ~params)
        else e.mock.http ~verb ~token ~method_ ~params
      in
      Lwt_main.run
        (let work = pass ~http e in
         let* () = Lwt.pause () in
         check "complete waiting" true (Lwt.is_sleeping work);
         Lwt.wakeup wake ();
         let* () = work in
         let f = one_fire e in
         check "receipt survives drain" true
           ((runtime e f.id).fire.relay_ts <> None);
         check "lease released" true ((runtime e f.id).claim = None);
         Lwt.return_unit))

let test_routes () =
  List.iter
    (fun (reason, change) ->
      with_env (fun e ->
          setup e;
          seen e;
          ready e ();
          change e;
          Lwt_main.run (pass e);
          Lwt_main.run (pass e);
          let key = Report.cause_key (Report.of_hard_reason reason) in
          check "hard refusal suspended" true
            ((Option.get (Store.find e.db e.order.id)).state = T.Suspended);
          Alcotest.(check (list (pair string string)))
            "routed once"
            [ (key, "sent") ]
            (Report.statuses ~db:e.db ~order_id:e.order.id);
          Alcotest.(check int)
            "one management reply" 1
            (count e "chat.postMessage")))
    Slack_standing_order_grant.
      [
        ( Missing_scope "files.info",
          fun e ->
            S.override e.mock "files.info" (fun _ ->
                ( 200,
                  obj [ ("ok", `Bool false); ("error", str "missing_scope") ] ))
        );
        (Target_not_member, fun e -> S.remove_member e.mock.world "UCLAUDE");
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
        (Grantor_not_member, fun e -> S.remove_member e.mock.world "U123");
        ( Target_not_allowlisted,
          fun e -> Session.update_config e.manager (S.config ~targets:[] ()) );
      ]

let test_transcript_reports () =
  List.iter
    (fun unavailable ->
      with_env (fun e ->
          setup e;
          seen e;
          ready e ();
          if unavailable then (
            enable_transcript e;
            e.transcript := obj [ ("id", str "FTRANS") ]);
          Lwt_main.run (pass e);
          Lwt_main.run (pass e);
          Alcotest.(check int)
            "API disappearance only"
            (if unavailable then 1 else 0)
            (count e "chat.postMessage")))
    [ false; true ]

let test_no_notes_report () =
  with_env (fun e ->
      setup e;
      seen e;
      e.replies := F.page [ !(e.root) ];
      Lwt_main.run (pass e);
      S.at (ended +. 3600.);
      Lwt_main.run (pass e);
      Lwt_main.run (pass e);
      check_state e (one_fire e).id T.No_notes;
      Alcotest.(check int) "one no-notes hint" 1 (count e "chat.postMessage"))

let test_egress () =
  with_env (fun e ->
      setup e;
      seen e;
      ready e ();
      enable_transcript e;
      let observed = ref [] in
      let verify method_ verb =
        check "closed egress method and verb" true
          (List.assoc_opt method_ Slack_standing_order_slack.methods = Some verb
          || (method_ = "upload" && verb = "POST"));
        let audits = Egress_audit.query ~db:e.db ~limit:10000 () in
        let audited =
          List.exists
            (fun (a : Egress_audit.event) ->
              a.decision = Egress_audit.Allowed
              && a.session_key = Some "slack:C123"
              && Option.fold ~none:false
                   ~some:(fun id ->
                     match Access_snapshot.get_by_id ~db:e.db id with
                     | Some snap ->
                         snap.work_type = Access_snapshot.Standing_order
                         && Option.fold ~none:false
                              ~some:(fun ref_ ->
                                ref_.Access_snapshot.order_id = e.order.id)
                              snap.standing_order
                     | None -> false)
                   a.snapshot_id
              && a.method_redacted = Some (Egress_audit.redact_method verb)
              && a.path_redacted
                 =
                 if method_ = "upload" then None
                 else Some (Egress_audit.redact_path ("/api/" ^ method_)))
            audits
        in
        S.mock_checks :=
          (fun () ->
            check "Allowed persisted snapshot and access key before transport"
              true audited)
          :: !S.mock_checks;
        observed := (method_, verb) :: !observed
      in
      let http ~verb ~token ~method_ ~params =
        verify method_ verb;
        e.mock.http ~verb ~token ~method_ ~params
      in
      let upload_http r =
        verify "upload" "POST";
        raw r
      in
      Lwt_main.run (pass ~http ~upload_http e);
      (* Exercise report and policy refusal through the same production pass. *)
      seed_report e Report.No_pickup;
      Lwt_main.run (pass ~http ~upload_http e);
      let cfg = Session.get_config e.manager in
      Session.update_config e.manager
        { cfg with egress = { cfg.egress with default_allowlist = [] } };
      (* Settled roots are no longer polled; supply pending audited work. *)
      seed_report e Report.Reply_delayed;
      Lwt_main.run (pass ~http ~upload_http e);
      let denied =
        Egress_audit.query ~db:e.db ~decision:Egress_audit.Denied ()
      in
      check "Denied audit also present" true (denied <> []);
      List.iter
        (fun (a : Egress_audit.event) ->
          check "Denied audit access key" true
            (a.session_key = Some "slack:C123");
          check "Denied audit order label" true
            (List.mem a.tool_name
               [
                 Some ("standing_order:" ^ e.order.id ^ ":fire");
                 Some ("standing_order:" ^ e.order.id ^ ":report");
               ]);
          check "Denied audit persisted order snapshot" true
            (Option.fold ~none:false
               ~some:(fun id ->
                 match Access_snapshot.get_by_id ~db:e.db id with
                 | Some snap ->
                     snap.work_type = Access_snapshot.Standing_order
                     && Option.fold ~none:false
                          ~some:(fun ref_ ->
                            ref_.Access_snapshot.order_id = e.order.id)
                          snap.standing_order
                 | None -> false)
               a.snapshot_id))
        denied;
      List.iter
        (fun m -> check ("covered " ^ m) true (List.mem_assoc m !observed))
        [
          "auth.test";
          "bots.info";
          "conversations.info";
          "conversations.members";
          "users.list";
          "reactions.get";
          "conversations.history";
          "conversations.replies";
          "files.info";
          "canvases.getContent";
          "files.getUploadURLExternal";
          "upload";
          "files.completeUploadExternal";
          "chat.postMessage";
        ];
      Printf.printf
        "Method/audit coverage: PASS (%d observed calls, Allowed/Denied \
         snapshot+key checked)\n\
         %!"
        (List.length !observed))

let test_worker_drain () =
  with_env (fun e ->
      setup e;
      let module W = Test_slack_standing_order_worker in
      let c = W.clock () in
      let sent, wake = Lwt.task () and dispatched = ref false in
      let http ~verb ~token ~method_ ~params =
        if method_ = "files.completeUploadExternal" then (
          dispatched := true;
          let* () = sent in
          e.mock.http ~verb ~token ~method_ ~params)
        else e.mock.http ~verb ~token ~method_ ~params
      in
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             E.start ~ctx:(W.context e c) ~db:e.db ~manager:e.manager ~http
               ~upload_http:raw ();
             cover_order e.db e.order;
             seen e;
             ready e ();
             E.wake ();
             let* () = W.flush 20 in
             check "complete was dispatched" true !dispatched;
             let stopped = E.stop () in
             check "stop waits for receipt" true (Lwt.is_sleeping stopped);
             let before = Option.get (runtime e (one_fire e).id).claim in
             let* () = W.advance c 30. in
             check "outstanding complete retains renewal during drain" true
               ((Option.get (runtime e (one_fire e).id).claim).until
              > before.until);
             Lwt.wakeup wake ();
             let* () = stopped in
             let r = runtime e (one_fire e).id in
             check "receipt recorded during stop" true (r.fire.relay_ts <> None);
             check "lease released on stop" true (r.claim = None);
             W.cleanup c;
             Lwt.return_unit)
           (fun () ->
             if Lwt.is_sleeping sent then Lwt.wakeup wake ();
             E.stop ())))

let test_shared_budget () =
  with_env (fun e ->
      setup e;
      seen e;
      ready e ();
      let module W = Test_slack_standing_order_worker in
      let c = W.clock () in
      let context = E.context ~wait:(W.wait c) ~accepting:(fun () -> true) () in
      let writes = ref [] in
      let http ~verb ~token ~method_ ~params =
        if
          method_ = "files.completeUploadExternal"
          || method_ = "chat.postMessage"
        then writes := !writes @ [ (method_, c.now) ];
        e.mock.http ~verb ~token ~method_ ~params
      in
      let rec finish work remaining =
        if not (Lwt.is_sleeping work) then work
        else if remaining = 0 then Alcotest.fail "pacing never completed"
        else
          let* () = W.advance c 1. in
          finish work (remaining - 1)
      in
      Lwt_main.run
        (let* () = finish (pass ~context ~http e) 300 in
         seed_report e Report.No_pickup;
         let r = runtime e (one_fire e).id in
         Slack_standing_order_sql.exec e.db
           "UPDATE slack_standing_order_dispatches SET \
            first_reply_ts=?,rate_limited_at=? WHERE fire_id=? AND \
            attempt_no=0"
           [
             Sqlite3.Data.TEXT "1700000101.000001";
             Sqlite3.Data.FLOAT (c.now -. 300.);
             Sqlite3.Data.TEXT r.fire.id;
           ];
         let module H = Test_slack_standing_order_repost_support in
         let original =
           set "text"
             (str (Slack_relay_guard.footer ~nonce:(Option.get r.nonce)))
             (H.message
                (Option.get r.fire.relay_ts)
                (Option.get (Store.attempt r 0)).uploaded_file_ids)
         in
         H.history e [ original ];
         H.pickup_reads e;
         let* () = finish (pass ~context ~http e) 500 in
         match !writes with
         | [
          ("files.completeUploadExternal", first);
          ("files.completeUploadExternal", second);
          ("chat.postMessage", third);
         ] ->
             check "relay, repost and report use one budget" true
               (second -. first >= 1. && third -. second >= 1.);
             Alcotest.(check int)
               "one repost" 1 (runtime e r.fire.id).fire.reposts;
             W.cleanup c;
             Lwt.return_unit
         | _ -> Alcotest.fail "expected relay, repost and report"))

let test_restart_read_only () =
  with_env (fun e ->
      setup e;
      seen e;
      ready e ();
      resolve e;
      let id = (one_fire e).id in
      ignore (Option.get (fetch e id));
      let module H = Test_slack_standing_order_repost_support in
      with_claim e id (fun lease r -> ignore (H.seed e lease r false));
      H.history e [ H.message H.ts0 [ "FORIGINAL" ] ];
      H.pickup_reads e;
      let module W = Test_slack_standing_order_worker in
      let c = W.clock () in
      Lwt_main.run
        (Lwt.finalize
           (fun () ->
             E.start ~ctx:(W.context e c) ~db:e.db ~manager:e.manager
               ~http:e.mock.http ~upload_http:raw ();
             let* () = W.advance c 300. in
             Alcotest.(check int)
               "persisted relay never reposts at startup" 0
               (count e "files.completeUploadExternal");
             Lwt.return_unit)
           (fun () ->
             let* () = E.stop () in
             W.cleanup c;
             Lwt.return_unit)))

let test_two_orders () =
  with_env (fun e ->
      setup e;
      seen e;
      ready e ();
      let target =
        { S.claude with T.user_id = "U456"; app_id = "A123"; bot_id = "BOWN" }
      in
      let second = S.seed ~verified_identity:fixture_identity e.db ~target () in
      cover_order e.db second;
      let card =
        Store.insert_card e.db ~kind:T.Pause_button ~order:second
          ~created_by:"U123" ~now:(Store.now ()) ()
      in
      Store.set_card_message_ts e.db ~card_id:card.id "1700000000.000300";
      Session.update_config e.manager
        (S.with_slack (Session.get_config e.manager) (fun sc ->
             {
               sc with
               standing_orders =
                 {
                   sc.standing_orders with
                   allowed_targets =
                     sc.standing_orders.allowed_targets
                     @ [
                         {
                           Slack_config_types.app_id = "A123";
                           label = "Other fixture";
                         };
                       ];
                 };
             }));
      S.edit_user e.mock.world "U456" (fun u ->
          set "is_bot" (`Bool true)
            (set "profile"
               (obj [ ("api_app_id", str "A123"); ("bot_id", str "BOWN") ])
               u));
      let module W = Test_slack_standing_order_worker in
      let c = W.clock () in
      let context = E.context ~wait:(W.wait c) ~accepting:(fun () -> true) () in
      let posts = ref [] in
      let http ~verb ~token ~method_ ~params =
        if method_ = "files.completeUploadExternal" then
          posts := c.now :: !posts;
        e.mock.http ~verb ~token ~method_ ~params
      in
      let rec finish work n =
        if not (Lwt.is_sleeping work) then work
        else if n = 0 then Alcotest.fail "two orders stalled"
        else
          let* () = W.advance c 1. in
          finish work (n - 1)
      in
      Lwt_main.run
        (let* () = finish (pass ~context ~http e) 500 in
         (match !posts with
         | [ second; first ] ->
             check "two orders complete at least one second apart" true
               (second -. first >= 1.)
         | _ -> Alcotest.fail "expected one complete per order");
         W.cleanup c;
         Lwt.return_unit))

let test_other_channel () =
  with_env (fun e ->
      let other = S.seed e.db ~channel:"C456" () in
      let card =
        Store.insert_card e.db ~kind:T.Pause_button ~order:other
          ~created_by:"U123" ~now:(Store.now ()) ()
      in
      Store.set_card_message_ts e.db ~card_id:card.id "1700000000.000300";
      (* Resolution now requires queued roots rather than idle order polling. *)
      seen e;
      L.record_seen ~db:e.db ~team:other.team_id ~channel:other.channel_id
        ~source:"event" ~now:(Store.now ()) ~root_ts ~file_ids:[]
        (Option.get (Slack_huddle_classify.room_of_json (get "room" !(e.root))));
      let blocked, wake = Lwt.task () and calls = ref 0 in
      let http ~verb:_ ~token:_ ~method_:_ ~params:_ =
        incr calls;
        if !calls = 1 then
          let* () = blocked in
          Lwt.return (503, "{}")
        else Lwt.return (503, "{}")
      in
      let context =
        Ctx.make
          ~accepting:(fun () -> true)
          ~limit:(fun ~channel_id:_ -> Lwt.return_unit)
          ()
      in
      Lwt_main.run
        (let work = pass ~context ~http e in
         Lwt.finalize
           (fun () ->
             let* () = Lwt.pause () in
             Alcotest.(check int) "another channel made progress" 2 !calls;
             Lwt.return_unit)
           (fun () ->
             if Lwt.is_sleeping blocked then Lwt.wakeup wake ();
             work)))

let test_failure_accounting () =
  with_env (fun e ->
      setup e;
      List.iter
        (fun i ->
          ignore
            (S.fire e.db e.order ~state:T.Failed
               ~room:("RFAILED" ^ string_of_int i)
               ()))
        [ 1; 2; 3 ];
      seen e;
      ready e ();
      Lwt_main.run (pass e);
      Alcotest.(check int)
        "K=3 prevents a fourth dispatch" 0
        (count e "files.completeUploadExternal");
      check "matching epoch pauses" true
        ((Option.get (Store.find e.db e.order.id)).state = T.Paused);
      Alcotest.(check (list (pair string string)))
        "one failure-streak report"
        [ ("repeated_failures", "sent") ]
        (Report.statuses ~db:e.db ~order_id:e.order.id))

let test_outcome_reports () =
  List.iter
    (fun (state, reason, expected) ->
      with_env (fun e ->
          setup e;
          let fire = S.fire e.db e.order ~room:"R123" ~state () in
          Slack_standing_order_sql.exec e.db
            "UPDATE slack_standing_order_fires SET reason=? WHERE id=?"
            [ Sqlite3.Data.TEXT reason; Sqlite3.Data.TEXT fire.id ];
          Lwt_main.run (pass e);
          Lwt_main.run (pass e);
          let expected = List.map (fun key -> (key, "sent")) expected in
          Alcotest.(check (list (pair string string)))
            "complete pass routes the fixed cause once" expected
            (Report.statuses ~db:e.db ~order_id:e.order.id)))
    T.
      [
        (No_notes, "no_notes", [ "no_notes_hint" ]);
        (No_pickup, "no_pickup", [ "no_pickup" ]);
        (Not_delivered, "not_delivered", [ "not_delivered" ]);
        (Skipped_cap, "daily_cap_reached", [ "cap_hit" ]);
        (Acknowledged, "acknowledged_no_reply", [ "reply_delayed" ]);
        (Skipped_inactive, "feature_disabled", []);
        (Skipped_no_transcript, "no_transcript", []);
        (Picked_up, "", []);
      ]

let suite =
  Test_slack_standing_order_fetch.suite @ Test_slack_standing_order_worker.suite
  @ Test_slack_standing_order_exec_review.suite
  @ [
      S.case "daemon event to relay to pickup without Provider" (fun () ->
          test_pass ());
      S.case "fire path never calls Provider" test_provider;
      S.case "every executor call site is covered by an egress rule" test_egress;
      S.case "reload kill switch target and profile gate next dispatch"
        test_reload;
      S.case "drain while complete waits preserves receipt" test_drain;
      S.case "stop cancels worker and renewal timers" test_worker_drain;
      S.case "persisted dispatched fires remain read-only after restart"
        test_restart_read_only;
      S.case "relay repost and report share the worker pacing budget"
        test_shared_budget;
      S.case "two orders in one channel share a one-second post budget"
        test_two_orders;
      S.case "a blocked channel allows another channel to proceed"
        test_other_channel;
      S.case "pass capability denies remain local including report processing"
        (test_capabilities ~reload:false);
      S.case "reload capability deny blocks queued reports"
        (test_capabilities ~reload:true);
      S.case "hard refusal routes once by cause" test_routes;
      S.case
        "API disappearance reports once; notes only has no API change report"
        test_transcript_reports;
      S.case "no notes hint once" test_no_notes_report;
      S.case "three failures pause before another dispatch"
        test_failure_accounting;
      S.case "complete pass routes terminal and informational outcomes"
        test_outcome_reports;
    ]

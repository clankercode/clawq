open Test_slack_standing_order_reconcile_support
open Lwt.Syntax
module Store = Slack_standing_order_store
module D = Slack_standing_order_dispatch

let with_socket e enabled expect_store_failure work =
  if not enabled then work None
  else
    let socket = Test_slack_socket.make_socket () in
    let never, _ = Lwt.task () and connections = ref 0 in
    let connector =
      Slack_socket.run
        ~sleep:(fun seconds -> wait e.clock ~seconds)
        ~now:(fun () -> e.clock.now)
        ~connect:(fun () ->
          incr connections;
          if !connections = 1 then Lwt.return socket.transport else never)
        ~handle_event:(fun body ->
          let+ _ =
            Slack.handle_event
              ~config:
                (Option.get (Session.get_config e.f.base.manager).channels.slack)
              ~session_manager:e.f.base.manager body
          in
          ())
        ~on_health:(fun _ -> ())
        ()
    in
    Lwt.finalize
      (fun () ->
        let* () = settle 30 in
        let* () = socket.emit {|{"type":"hello"}|} in
        work (Some socket))
      (fun () ->
        Lwt.catch
          (fun () -> Slack_standing_order_resolution_lease.cancel connector)
          (function
            | Failure message
              when expect_store_failure
                   && String_util.contains message
                        "slack_standing_orders: CONSTRAINT" ->
                Lwt.return_unit
            | exn -> Lwt.fail exn))

let transport_fence ?(second_handle = false) stage fault () =
  let socket_mode =
    List.mem fault [ "pending_pause"; "socket_store_failure" ]
  in
  with_env ~disk:true ~socket:socket_mode (fun e ->
      with_socket e socket_mode (fault = "socket_store_failure") (fun socket ->
          let cfg = Session.get_config e.f.base.manager in
          Session.update_config e.f.base.manager
            {
              cfg with
              egress =
                {
                  cfg.egress with
                  default_allowlist =
                    {
                      Runtime_config.host = "files.slack.com";
                      path = Some "/upload/*";
                      method_ = Some "POST";
                      action = Allow;
                      log_policy = No_log;
                    }
                    :: cfg.egress.default_allowlist;
                };
            };
          let* () = pass e in
          let fire =
            S.fire e.f.db e.order ~room:"R123" ~date_start:"1700000010"
              ~state:T.Uploading ()
          in
          Sql.exec e.f.db
            "UPDATE slack_standing_order_fires SET \
             root_ts=?,canvas_file_id='FCANVAS',notes_ready_ts='1700000060.000100' \
             WHERE id=?"
            Sql.[ text root_ts; text fire.id ];
          R.record_root e.f.db ~order:e.order ~root_ts ~room_id:"R123"
            ~date_start:"1700000010" ~fire_id:(Some fire.id);
          let other = Sqlite3.db_open (Option.get e.disk_path) in
          check "second handle initially has identical verified authority" true
            (R.covered other e.order.id e.order.epoch);
          let db = if second_handle then other else e.f.db in
          let manager =
            if second_handle then
              Session.create
                ~config:(Session.get_config e.f.base.manager)
                ~db ()
            else e.f.base.manager
          in
          let raw = ref 0 and allowed = ref true in
          let ctx = { e.ctx with Ctx.accepting = (fun () -> !allowed) } in
          let prior = !(e.answer) in
          (e.answer :=
             fun m p ->
               match m with
               | "files.getUploadURLExternal" ->
                   Lwt.return
                     ( 200,
                       S.ok
                         [
                           ("file_id", str "FOWN");
                           ( "upload_url",
                             str "https://files.slack.com/upload/fixture" );
                         ] )
               | "files.completeUploadExternal" ->
                   Lwt.return
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
                                                           ( "ts",
                                                             str
                                                               "1700000200.000100"
                                                           );
                                                         ];
                                                     ] );
                                               ] );
                                         ] );
                                   ];
                               ] );
                         ] )
               | _ -> prior m p);
          let payload : Slack_standing_order_provenance.verified =
            {
              root_ts;
              thread_ts = root_ts;
              room_id = "R123";
              date_start = 1700000010;
              date_end = Some 1700000030;
              notes_ready_ts = "1700000060.000100";
              canvas_id = "FCANVAS";
              notes_md = "fixture notes";
              transcript = None;
              transcript_unavailable = false;
            }
          in
          let key =
            match stage with
            | `Allocate -> "files.getUploadURLExternal"
            | `Raw -> "upload"
            | `Complete -> "files.completeUploadExternal"
          in
          let start = e.clock.now in
          Slack_read_limiter.note_retry_after (Option.get e.ctx.limiter)
            ~key:("T123:A123:" ^ key) 20.;
          let lease =
            Store.claim db ~fire_id:fire.id ~owner:"prepared-job"
              ~now:e.clock.now
            |> Result.get_ok |> Option.get
          in
          let task =
            D.deliver ~ctx ~db ~manager ~http:e.http
              ~upload_http:(fun _ ->
                incr raw;
                Lwt.return
                  Slack_standing_order_upload.
                    { status = 200; headers = []; body = "" })
              ~lease
              (Option.get (Store.runtime db ~fire_id:fire.id))
              payload
          in
          Lwt.finalize
            (fun () ->
              let* () = advance e.clock (start +. 10.) in
              check "actual final shared transport wait reached" true
                (Lwt.is_sleeping task);
              let allocated = count e "files.getUploadURLExternal" in
              eq "fixture reached requested transport boundary"
                (if stage = `Allocate then 0 else 1)
                allocated;
              eq "raw transport held at requested boundary"
                (if stage = `Complete then 1 else 0)
                !raw;
              let* () =
                match fault with
                | "pending_pause" ->
                    let socket = Option.get socket in
                    let ack, _ = Lwt.task () in
                    socket.ack_pending := Some ack;
                    let message =
                      obj
                        [
                          ("type", str "message");
                          ("channel", str "C123");
                          ("user", str "U456");
                          ("ts", str "1700000090.000999");
                          ("text", str "pause SO-1");
                        ]
                    in
                    let delivery =
                      socket.emit
                        (Yojson.Safe.to_string
                           (obj
                              [
                                ("type", str "events_api");
                                ("envelope_id", str "actual-pending-pause");
                                ( "payload",
                                  Yojson.Safe.from_string (event message) );
                              ]))
                    in
                    check
                      "authenticated ACK still pending before competing write"
                      true (Lwt.is_sleeping delivery);
                    eq "real Socket pause identity reserved" 1
                      (Sql.int_query other
                         "SELECT count(*) FROM slack_order_ingress WHERE \
                          pause=1 AND applied=0"
                         []);
                    Lwt.return_unit
                | "socket_store_failure" ->
                    Sql.exec e.f.db
                      "CREATE TEMP TRIGGER fail_socket_gap BEFORE UPDATE ON \
                       slack_standing_order_recovery_gaps BEGIN SELECT \
                       RAISE(ABORT,'socket-gap-fault'); END"
                      [];
                    (Option.get socket).drop ();
                    let* () = settle 50 in
                    check
                      "real transport failure closes first handle despite \
                       store error"
                      false (covered e);
                    check
                      "real transport failure closes independent local handle"
                      false
                      (R.covered other e.order.id e.order.epoch);
                    Sql.exec e.f.db "DROP TRIGGER fail_socket_gap" [];
                    check "removing DB fault cannot reopen old coverage" false
                      (R.covered other e.order.id e.order.epoch);
                    Lwt.return_unit
                | _ ->
                    (match fault with
                    | "reload" ->
                        Session.update_config e.f.base.manager
                          (Session.get_config e.f.base.manager)
                    | "generation" ->
                        ignore
                          (R.close_gap other ~team_id:e.order.team_id
                             ~app_id:e.order.api_app_id
                             ~channel_id:e.order.channel_id ~cause:"wait-race"
                             ~now:e.clock.now
                          |> Result.get_ok)
                    | "suppression" ->
                        let _, installation, _, _ =
                          Slack_standing_order_rollout_store.read_anchor other
                        in
                        Sql.exec other
                          "INSERT INTO slack_standing_order_suppression \
                           VALUES(?,?,?,?,?,?)"
                          Sql.
                            [
                              text installation;
                              text e.order.id;
                              text root_ts;
                              text "R123";
                              text "1700000010";
                              text
                                (Slack_standing_order_rollout_store.digest
                                   "late-delivery");
                            ]
                    | "readiness" ->
                        Slack_standing_order_rollout_store.invalidate_identity
                          other
                    | "epoch" ->
                        ignore
                          (Store.deactivate other ~order_id:e.order.id
                             (Store.Pause "U456") ~now:e.clock.now
                          |> Result.get_ok)
                    | "lease" ->
                        Sql.exec other
                          "UPDATE slack_standing_order_fires SET \
                           claim_owner='successor',lease_epoch=lease_epoch+1 \
                           WHERE id=?"
                          [ Sql.text fire.id ]
                    | "accepting" -> allowed := false
                    | "abort" ->
                        Sql.exec e.f.db
                          "CREATE TEMP TRIGGER abort_generation BEFORE INSERT \
                           ON slack_order_dispatch_admission BEGIN SELECT \
                           RAISE(ABORT,'reservation-abort'); END"
                          []
                    | "current" -> ()
                    | _ -> assert false);
                    Lwt.return_unit
              in
              let* () = advance e.clock (start +. 25.) in
              let* () = task in
              let success = fault = "current" in
              eq "actual allocation refusal or positive control"
                (if success then 1 else allocated)
                (count e "files.getUploadURLExternal");
              eq "actual raw refusal or positive control"
                (if success then 1 else if stage = `Complete then 1 else 0)
                !raw;
              eq "actual completion refusal or positive control"
                (if success then 1 else 0)
                (count e "files.completeUploadExternal");
              eq "only successful completion reserves/debits"
                (if success then 1 else 0)
                (Store.dispatches_since other ~order_id:e.order.id ~since:0.);
              eq "generation and attempt commit atomically"
                (if success then 1 else 0)
                (Sql.int_query other
                   "SELECT count(*) FROM slack_order_dispatch_admission WHERE \
                    fire_id=?"
                   [ Sql.text fire.id ]);
              (if success then
                 let gap = R.pass_token other e.order.id in
                 eq "stored generation is actual verified generation"
                   gap.gap_generation
                   (Sql.int_query other
                      "SELECT generation FROM slack_order_dispatch_admission \
                       WHERE fire_id=?"
                      [ Sql.text fire.id ]));
              Lwt.return_unit)
            (fun () ->
              Lwt.cancel task;
              ignore (Sqlite3.db_close other);
              Lwt.return_unit)))

let suite =
  List.concat_map
    (fun (name, stage) ->
      List.map
        (fun fault ->
          S.case
            ("post-wait actual " ^ name ^ " " ^ fault)
            (transport_fence stage fault))
        ([
           "generation";
           "readiness";
           "suppression";
           "epoch";
           "lease";
           "accepting";
           "current";
         ]
        @ if stage = `Complete then [ "abort" ] else []))
    [ ("allocation", `Allocate); ("raw", `Raw); ("completion", `Complete) ]

let suite =
  suite
  @ List.concat_map
      (fun (name, stage) ->
        List.map
          (fun fault ->
            S.case
              ("actual " ^ name ^ " refuses " ^ fault)
              (transport_fence stage fault))
          [ "pending_pause"; "socket_store_failure"; "reload" ])
      [ ("allocation", `Allocate); ("raw", `Raw); ("completion", `Complete) ]
  @ [
      S.case
        "failed Socket store close refuses actual complete from independent \
         handle"
        (transport_fence ~second_handle:true `Complete "socket_store_failure");
    ]

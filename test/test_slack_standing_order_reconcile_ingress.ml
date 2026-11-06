open Test_slack_standing_order_reconcile_support
open Lwt.Syntax

let suite = []

let socket_admission () =
  with_env ~socket:true (fun e ->
      with_transport e (fun () ->
          let socket = Test_slack_socket.make_socket () in
          let ack, ack_u = Lwt.task () in
          let handle body =
            let+ _ =
              Slack.handle_event
                ~config:
                  (Option.get
                     (Session.get_config e.f.base.manager).channels.slack)
                ~session_manager:e.f.base.manager body
            in
            ()
          in
          let connector =
            Slack_socket.run
              ~sleep:(fun seconds -> wait e.clock ~seconds)
              ~now:(fun () -> e.clock.now)
              ~connect:(fun () -> Lwt.return socket.transport)
              ~handle_event:handle
              ~on_health:(fun _ -> ())
              ()
          in
          Lwt.finalize
            (fun () ->
              let* () = settle 30 in
              eq "TCP does not trigger reconciliation" 0 (count e "auth.test");
              let* () = socket.emit {|{"type":"hello"}|} in
              let* () = pass e in
              check "early authenticated hello verified by startup pass" true
                (covered e);
              let message =
                obj
                  [
                    ("type", str "message");
                    ("channel", str "C123");
                    ("user", str "U456");
                    ("ts", str "1700000090.000009");
                    ("text", str "pause SO-1");
                  ]
              in
              let envelope payload =
                Yojson.Safe.to_string
                  (obj
                     [
                       ("type", str "events_api");
                       ("envelope_id", str "socket-pause");
                       ("payload", payload);
                     ])
              in
              let wrong =
                Yojson.Safe.from_string (event ~id:"Ev-foreign" message)
                |> set "api_app_id" (str "AOTHER")
              in
              let* () = socket.emit (envelope wrong) in
              eq "foreign installation reserves no authority" 0
                (Sql.int_query e.f.db "SELECT count(*) FROM slack_order_ingress"
                   []);
              socket.ack_pending := Some ack;
              let delivery =
                socket.emit (envelope (Yojson.Safe.from_string (event message)))
              in
              check "actual socket ACK remains pending" true
                (Lwt.is_sleeping delivery);
              eq "durable pause reservation precedes ACK" 1
                (Sql.int_query e.f.db
                   "SELECT count(*) FROM slack_order_ingress WHERE pause=1 AND \
                    applied=0"
                   []);
              check "pending socket pause immediately denies relay admission"
                false (covered e);
              Lwt.wakeup_later ack_u ();
              let* () = drain e.clock delivery in
              let* () = advance e.clock (e.clock.now +. 3.) in
              Alcotest.check S.state_t
                "actual handler applies nonallowlisted pause" T.Paused
                (Option.get (Slack_standing_order_store.find e.f.db e.order.id))
                  .state;
              Lwt.return_unit)
            (fun () -> Slack_standing_order_resolution_lease.cancel connector)))

let signed_http e =
  with_transport e (fun () ->
      let* () = pass e in
      let sc =
        Option.get (Session.get_config e.f.base.manager).channels.slack
      in
      let gate, u = Lwt.task () in
      let prior = !(e.answer) in
      (e.answer :=
         fun m p ->
           if m = "conversations.members" then
             let* () = gate in
             prior m p
           else prior m p);
      let message =
        obj
          [
            ("type", str "message");
            ("channel", str "C123");
            ("user", str "U456");
            ("ts", str "1700000090.000007");
            ("text", str "pause SO-1");
          ]
      in
      let body = event message in
      let timestamp = string_of_int (int_of_float (Unix.gettimeofday ())) in
      let signature =
        "v0="
        ^ Digestif.SHA256.(
            hmac_string ~key:sc.signing_secret ("v0:" ^ timestamp ^ ":" ^ body)
            |> to_hex)
      in
      let request signature =
        let headers =
          Cohttp.Header.of_list
            [
              ("x-slack-signature", signature);
              ("x-slack-request-timestamp", timestamp);
            ]
        in
        let req =
          Cohttp.Request.make ~meth:`POST ~headers
            (Uri.of_string ("http://localhost" ^ sc.events_path))
        in
        Http_server_webhooks.handle ~session_manager:e.f.base.manager
          ~auth_token:(Some "fixture") ~slack_config:sc `POST sc.events_path req
          (Cohttp_lwt.Body.of_string body)
      in
      let* invalid = request "v0=invalid" in
      check "bad signature rejected" true
        (Cohttp.Response.status (fst (Option.get invalid)) = `Unauthorized);
      eq "unsigned body reserves nothing" 0
        (Sql.int_query e.f.db "SELECT count(*) FROM slack_order_ingress" []);
      let* accepted = request signature in
      check "signed gateway ACKs while membership is blocked" true
        (Cohttp.Response.status (fst (Option.get accepted)) = `OK);
      eq "signed HTTP durable pause before handler finishes" 1
        (Sql.int_query e.f.db
           "SELECT count(*) FROM slack_order_ingress WHERE pause=1 AND \
            applied=0"
           []);
      check "HTTP-only ingress blocks independently of socket health" false
        (covered e);
      Lwt.wakeup_later u ();
      let* () = advance e.clock (e.clock.now +. 5.) in
      Alcotest.check S.state_t "signed handler commits pause" T.Paused
        (Option.get (Slack_standing_order_store.find e.f.db e.order.id)).state;
      eq "committed decision clears pending identity" 0
        (Sql.int_query e.f.db
           "SELECT count(*) FROM slack_order_ingress WHERE pause=1 AND \
            applied=0"
           []);
      Lwt.return_unit)

let shutdown_ownership e =
  with_transport e (fun () ->
      let original = !Session_management.on_config_published in
      let later_calls = ref 0 in
      let later () =
        incr later_calls;
        original ()
      in
      Session_management.on_config_published := later;
      Lwt.finalize
        (fun () ->
          let gate, _ = Lwt.task () and canceled = ref false in
          Lwt.on_cancel gate (fun () -> canceled := true);
          (e.answer := fun _ _ -> gate);
          C.start ~db:e.f.db ~manager:e.f.base.manager;
          let* () = settle 30 in
          let* () = C.stop () in
          check "shutdown cancels held actual Grant HTTP" true !canceled;
          let before = List.length !(e.calls) in
          Session.update_config e.f.base.manager
            (Session.get_config e.f.base.manager);
          eq "later callback owner survives stop" 1 !later_calls;
          let* () = advance e.clock (e.clock.now +. 601.) in
          eq "stopped worker cannot emit requests or timer reruns" before
            (List.length !(e.calls));
          Lwt.return_unit)
        (fun () ->
          if !Session_management.on_config_published == later then
            Session_management.on_config_published := original;
          Lwt.return_unit))

let suite =
  suite
  @ [
      S.case "real socket reserves durable pause before blocked ACK"
        socket_admission;
      case "signed HTTP reserves pause before ACK independent of socket"
        signed_http;
      case "shutdown joins HTTP and restores only owned callbacks"
        shutdown_ownership;
    ]

let hung_grant e =
  let blocked, _ = Lwt.task () and canceled = ref false in
  Lwt.on_cancel blocked (fun () -> canceled := true);
  (e.answer := fun _ _ -> blocked);
  let pending =
    C.pass_with ~ctx:e.ctx ~http:e.http ~db:e.f.db ~manager:e.f.base.manager
      e.order
  in
  let* () = advance e.clock (e.clock.now +. 31.) in
  check "hung authority read is bounded and canceled" true !canceled;
  check "hung authority pass returns" false (Lwt.is_sleeping pending);
  let* () = pending in
  check "timeout never verifies initial coverage" false (covered e);
  Lwt.return_unit

let suite =
  suite
  @ [
      case "hung initial authority HTTP has bounded cancellable timeout"
        hung_grant;
    ]

let legacy_discovery () =
  with_env ~legacy:true (fun e ->
      let module I = Slack_standing_order_receipt_import in
      let module IF = Test_slack_standing_order_receipt_import in
      let _, installation, _, _ =
        Slack_standing_order_rollout_store.read_anchor e.f.db
      in
      let receipt = IF.receipt e.order installation in
      let delivered =
        root
        |> set "ts" (str receipt.root_ts)
        |> set "room"
             (room
             |> set "id" (str receipt.room_id)
             |> set "date_start" (`Int (int_of_string receipt.date_start)))
      in
      e.roots := [ delivered ];
      let prior = !(e.answer) in
      (e.answer :=
         fun m p ->
           if m = "conversations.history" && P.get "limit" p = `Int 1 then
             Lwt.return (200, page [ delivered ])
           else prior m p);
      let* () = pass e in
      check "legacy absence cannot admit history discovered roots" false
        (covered e);
      let inventory = IF.inventory e.f.db e.order installation [ receipt ] in
      let inputs = IF.validated e.f.db inventory [ receipt ]
      and digest = I.inventory_digest inventory in
      check "incomplete inventory cannot release" true
        (Result.is_error
           (I.apply e.f.db [ List.hd inputs ] ~manifest_digest:digest));
      let* () = advance e.clock (e.clock.now +. 300.) in
      let* () = pass e in
      check "partial inventory still barred after actual sweep" false
        (covered e);
      ignore (IF.ok (I.apply e.f.db inputs ~manifest_digest:digest));
      let* () = advance e.clock (e.clock.now +. 300.) in
      let* () = pass e in
      check "verified reviewed inventory can admit completed coverage" true
        (covered e);
      observe e
        (delivered
        |> set "type" (str "message")
        |> set "channel" (str "C123")
        |> set "user" (str "USLACKBOT"));
      E.start ~ctx:e.ctx ~db:e.f.db ~manager:e.f.base.manager ~http:e.http ();
      let* () = advance e.clock (e.clock.now +. 605.) in
      let* () = pass e in
      List.iter
        (fun m -> eq ("suppression no " ^ m) 0 (count e m))
        [ "files.getUploadURLExternal"; "files.completeUploadExternal" ];
      check "suppression durable across lifecycle restart" true
        (R.suppressed e.f.db ~order_id:e.order.id ~root_ts:receipt.root_ts
           ~room_id:receipt.room_id ~date_start:receipt.date_start);
      Lwt.return_unit)

let pause_during_maintenance e =
  with_transport e (fun () ->
      let* () = pass e in
      e.roots :=
        List.init 100 (fun n ->
            obj
              [
                ("ts", str (Printf.sprintf "17000000%02d.000001" n));
                ("reply_count", `Int 1);
                ("latest_reply", str "1700000099.000001");
              ]);
      let* () = advance e.clock (e.clock.now +. 300.) in
      let scan =
        C.pass_with ~ctx:e.ctx ~http:e.http ~db:e.f.db ~manager:e.f.base.manager
          e.order
      in
      let* () = advance e.clock (e.clock.now +. 2.) in
      check "background sweep in flight" true (Lwt.is_sleeping scan);
      let message =
        obj
          [
            ("type", str "message");
            ("channel", str "C123");
            ("user", str "U456");
            ("ts", str "1700000090.000008");
            ("text", str "pause SO-1");
          ]
      in
      let member_gate, member_u = Lwt.task () in
      let previous = !(e.answer) in
      (e.answer :=
         fun m p ->
           if m = "conversations.members" then
             let* () = member_gate in
             previous m p
           else previous m p);
      let pause =
        Slack.handle_event
          ~config:
            (Option.get (Session.get_config e.f.base.manager).channels.slack)
          ~session_manager:e.f.base.manager
          (event ~id:"Ev-priority-pause" message)
      in
      check "live pause pending immediately closes healthy admission" false
        (covered e);
      Lwt.wakeup_later member_u ();
      let* () = advance e.clock (e.clock.now +. 3.) in
      check "pause cannot wait for 100 background replies" false
        (Lwt.is_sleeping pause);
      let* _ = pause in
      Alcotest.check S.state_t "priority pause committed during maintenance"
        T.Paused
        (Option.get (Slack_standing_order_store.find e.f.db e.order.id)).state;
      let* () = drain e.clock scan in
      eq "no relay after priority pause" 0
        (count e "files.completeUploadExternal");
      Lwt.return_unit)

let suite =
  suite
  @ [
      S.case
        "actual legacy history remains held until complete inventory and \
         suppression"
        legacy_discovery;
      case "live pause preempts a 100-parent maintenance sweep"
        pause_during_maintenance;
    ]

let socket_deadline_fences_cleanup ~pong () =
  let module Socket = Test_slack_socket in
  let previous = !Slack_socket.on_gap in
  let fenced = ref false and observed = ref None in
  let cleanup, finish = Lwt.task () in
  Fun.protect
    ~finally:(fun () -> Slack_socket.on_gap := previous)
    (fun () ->
      (Slack_socket.on_gap :=
         fun ~cause -> if cause = "connection_failure" then fenced := true);
      let socket = Socket.make_socket ~half_open:pong () in
      let close () =
        observed := Some !fenced;
        cleanup
      in
      let socket =
        { socket with transport = { socket.transport with close } }
      in
      Socket.with_connector [ socket ] (fun clock _ _ _ _ ->
          Lwt.finalize
            (fun () ->
              let* () =
                if pong then (
                  let* () = Socket.hello socket in
                  Socket.fire clock 5.;
                  let* () = settle 20 in
                  Socket.fire clock 2.;
                  Lwt.return_unit)
                else (
                  Socket.fire clock 7.;
                  Lwt.return_unit)
              in
              let* () = settle 20 in
              check "transport cleanup is actually waiting" true
                (Lwt.is_sleeping cleanup);
              Alcotest.(check (option bool))
                "timeout closes admission before cleanup's first await"
                (Some true) !observed;
              Lwt.return_unit)
            (fun () ->
              if Lwt.is_sleeping cleanup then Lwt.wakeup_later finish ();
              settle 20)))

let suite =
  suite
  @ [
      S.case "hello timeout fences before asynchronous transport cleanup"
        (socket_deadline_fences_cleanup ~pong:false);
      S.case "pong timeout fences before asynchronous transport cleanup"
        (socket_deadline_fences_cleanup ~pong:true);
    ]

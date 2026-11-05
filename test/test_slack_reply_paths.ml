open Lwt.Syntax
open Test_slack_huddle_fixtures
module Th = Test_slack_threads
module E = Test_slack_empty_reply

let body = "Opening paragraph.\n\n" ^ String.make 2700 'x'

let count calls method_ =
  List.length (List.filter (fun (m, _) -> m = method_) !calls)

let check_count calls method_ n =
  Alcotest.(check int) method_ n (count calls method_)

(* All external operations, including provider streaming, remain fake. *)
let artifacts handler =
  let previous = !handler in
  handler :=
    fun m p ->
      match m with
      | "conversations.info" ->
          let j = previous m p in
          update j
            [
              ( "channel",
                update (get "channel" j)
                  [ ("is_im", `Bool false); ("is_mpim", `Bool false) ] );
            ]
      | "canvases.create" -> ok [ ("canvas_id", str "FCANVAS") ]
      | "files.info" ->
          ok
            [
              ( "file",
                obj
                  [
                    ("id", get "file" p);
                    ("permalink", str "https://example.slack.com/docs/FCANVAS");
                  ] );
            ]
      | "canvases.access.set" | "canvases.delete" -> ok []
      | "files.getUploadURLExternal" ->
          obj [ ("ok", `Bool false); ("error", str "invalid_arguments") ]
      | _ -> previous m p

let mode base mode =
  let cfg = base.manager.config in
  let sc = Option.get cfg.channels.slack in
  let cfg =
    {
      cfg with
      channels =
        {
          cfg.channels with
          slack = Some { sc with long_reply = { sc.long_reply with mode } };
        };
    }
  in
  base.cfg := cfg;
  Session.update_config base.manager cfg

let no_body_stream calls =
  List.iter
    (fun (m, p) ->
      if
        List.mem m
          [ "chat.startStream"; "chat.appendStream"; "chat.stopStream" ]
      then
        check_bool "stream has no answer" false
          (contains (Yojson.Safe.to_string p) "Opening paragraph"))
    !calls;
  check_count calls "chat.appendStream" 0

let check_carriers calls root n =
  let carriers =
    List.filter
      (fun (m, p) ->
        m = "chat.postMessage"
        && contains
             (Slack_huddles.field "text" p)
             "https://example.slack.com/docs/")
      !calls
  in
  Alcotest.(check int)
    "one final carrier per invocation" n (List.length carriers);
  List.iter
    (fun (_, p) ->
      Alcotest.(check (option string))
        "original root" root
        (match get "thread_ts" p with `String s -> Some s | _ -> None);
      check_bool "no broadcast" false (get "reply_broadcast" p = `Bool true))
    carriers

let test_native ?(unavailable = false) ?(off = false) () =
  Th.with_ingress (fun base calls handler _ provider run ->
      artifacts handler;
      if off then mode base Slack_config_types.Off;
      (if unavailable then
         let old = !handler in
         handler :=
           fun m p ->
             if m = "chat.startStream" then
               obj [ ("ok", `Bool false); ("error", str "not_allowed") ]
             else old m p);
      (provider := fun ~model ~messages:_ -> Lwt.return (E.answer body ~model));
      let old = !Provider.native_stream in
      Provider.native_stream :=
        List.map
          (fun (kind, _) ->
            ( kind,
              fun ~config:_
                ~provider:_
                ~model
                ~messages:_
                ?tools:_
                ?session_key:_
                ~on_chunk
                ()
              ->
                let* () = on_chunk (Provider.Delta "Opening paragraph.") in
                let* () = on_chunk (Provider.Delta (String.make 2700 'x')) in
                Lwt.return (E.answer body ~model) ))
          old;
      let* _ = run (Th.message "work") in
      no_body_stream calls;
      check_count calls "canvases.create" (if off then 0 else 1);
      if off then
        Alcotest.(check int)
          "one buffered full answer" 1
          (List.length
             (List.filter
                (fun (m, p) ->
                  m = "chat.postMessage" && get "text" p = str body)
                !calls))
      else check_carriers calls (Some "100.000001") 1;
      Lwt.return_unit)

let legacy ?thread ?send_message_fn base text =
  Slack.handle_message
    ~config:(Option.get base.manager.config.channels.slack)
    ~session_manager:base.manager ?send_message_fn
    (Yojson.Safe.to_string (Th.callback (Th.message ?thread text)))

let test_legacy_injection () =
  Th.with_ingress (fun base calls handler _ provider _ ->
      artifacts handler;
      let finals = ref [] in
      let send_message_fn ~bot_token:_ ~channel_id ~text =
        finals := (channel_id, text) :: !finals;
        Lwt.return_unit
      in
      (provider := fun ~model ~messages:_ -> Lwt.return (E.answer body ~model));
      mode base Slack_config_types.File;
      let* _ = legacy ~send_message_fn base "work" in
      Alcotest.(check int)
        "original final uses injection once" 1
        (List.length (List.filter (( = ) ("C123", body)) !finals));
      check_count calls "files.getUploadURLExternal" 1;
      Lwt.return_unit)

let test_queued () =
  Th.with_ingress (fun _ calls handler _ provider run ->
      artifacts handler;
      let gate, release = Lwt.task () and n = ref 0 in
      (provider :=
         fun ~model ~messages:_ ->
           incr n;
           if !n = 1 then gate else Lwt.return (E.answer body ~model));
      let work = run (Th.message "first") in
      let* () = Test_slack_socket.settle 2 in
      let* _ =
        run ~id:"queued"
          (Th.message ~user:"U456" ~thread:"100.000001" ~ts:"100.000003"
             "follow-up")
      in
      check_count calls "canvases.create" 0;
      Lwt.wakeup_later release (E.answer "short first answer" ~model:"gpt-test");
      let* _ = work in
      Alcotest.(check int) "queued requester runs" 2 !n;
      check_count calls "canvases.create" 1;
      check_carriers calls (Some "100.000001") 1;
      Lwt.return_unit)

let test_legacy_long () =
  Th.with_ingress (fun base calls handler _ provider _ ->
      artifacts handler;
      (provider := fun ~model ~messages:_ -> Lwt.return (E.answer body ~model));
      let* _ = legacy base "work" in
      check_count calls "canvases.create" 1;
      check_carriers calls None 1;
      Lwt.return_unit)

let pending base key =
  List.exists
    (fun (k, _, _) -> k = key)
    (Memory.load_pending_agent_sessions ~db:base.db ~max_age_seconds:3600)

let result_only base answer =
  Session.set_special_command_handler base.manager
    (fun ~key ~message:_ ~send_progress:_ ~interrupt_check:_ ->
      Session.record_agent_turn base.manager ~key ~channel:"slack"
        ~channel_id:"C123" ();
      Lwt.return_some answer)

let test_result_only () =
  Th.with_ingress (fun base calls handler _ _ _ ->
      artifacts handler;
      result_only base body;
      let* _ = legacy ~thread:"99.000001" base "work" in
      check_count calls "canvases.create" 1;
      check_carriers calls (Some "99.000001") 1;
      check_bool "successful result marked sent" false (pending base base.key);
      Lwt.return_unit)

let test_injection_modes mode_ answer () =
  Th.with_ingress (fun base calls handler _ provider _ ->
      artifacts handler;
      mode base mode_;
      (provider := fun ~model ~messages:_ -> Lwt.return (E.answer answer ~model));
      let sent = ref [] in
      let send_message_fn ~bot_token:_ ~channel_id ~text =
        sent := (channel_id, text) :: !sent;
        Lwt.return_unit
      in
      let* _ = legacy ~send_message_fn base "work" in
      Alcotest.(check int)
        "exact injected final once" 1
        (List.length (List.filter (( = ) ("C123", answer)) !sent));
      check_bool "successful injected final marked sent" false
        (pending base base.key);
      check_count calls "canvases.create" 0;
      Lwt.return_unit)

let test_failed_final ~result_branch ~injection ~status () =
  Th.with_ingress (fun base _ handler _ provider _ ->
      artifacts handler;
      (if result_branch then result_only base "short final"
       else
         provider :=
           fun ~model ~messages:_ -> Lwt.return (E.answer "short final" ~model));
      let attempts = ref 0 in
      let send_message_fn =
        if injection then
          Some
            (fun ~bot_token:_ ~channel_id:_ ~text ->
              if text = "short final" then (
                incr attempts;
                Lwt.fail_with "private transport detail")
              else Lwt.return_unit)
        else None
      in
      let http = Option.get (Lwt.get Slack_transport.key) in
      let post ~uri ~headers ~body =
        if get "text" (parse body) = str "short final" then (
          incr attempts;
          Lwt.return (status, {|{"ok":false,"error":"invalid_auth"}|}))
        else http.post ~uri ~headers ~body
      in
      let* _ =
        Lwt.catch
          (fun () ->
            Slack_transport.with_http { http with post } (fun () ->
                legacy ?send_message_fn base "work"))
          (function
            | Slack_thread_api.Delivery_denied _ -> Lwt.return "failed"
            | exn -> Lwt.fail exn)
      in
      Alcotest.(check int) "checked final attempted once" 1 !attempts;
      check_bool "failed answer remains pending" true (pending base base.key);
      Lwt.return_unit)

let test_threaded_injection ~native () =
  Th.with_ingress (fun base calls handler _ provider _ ->
      artifacts handler;
      (provider :=
         fun ~model ~messages:_ -> Lwt.return (E.answer "short final" ~model));
      let injected = ref [] in
      let send_message_fn ~bot_token:_ ~channel_id:_ ~text =
        injected := text :: !injected;
        Lwt.return_unit
      in
      let* _ =
        if native then
          Slack.handle_event ~session_manager:base.manager
            ~config:(Option.get base.manager.config.channels.slack)
            ~send_message_fn
            (Yojson.Safe.to_string
               (Th.callback
                  ~id:(Temp_downloads.generate_token ())
                  (Th.message "work")))
        else legacy ~thread:"99.000001" ~send_message_fn base "work"
      in
      check_bool "threaded final bypasses unthreaded injection" false
        (List.mem "short final" !injected);
      let finals =
        List.filter
          (fun (m, p) ->
            m = "chat.postMessage" && get "text" p = str "short final")
          !calls
      in
      Alcotest.(check int) "one checked final" 1 (List.length finals);
      Alcotest.(check string)
        "exact parent"
        (if native then "100.000001" else "99.000001")
        (Slack_huddles.field "thread_ts" (snd (List.hd finals)));
      Lwt.return_unit)

let test_rotation () =
  Th.with_ingress (fun _ calls handler _ provider run ->
      artifacts handler;
      let clock = Test_slack_socket.make_clock () in
      let gate, release = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> gate);
      Slack_thread_runs.with_clock
        ~now:(fun () -> clock.time)
        ~sleep:(Test_slack_socket.sleep clock)
        (fun () ->
          let work = run (Th.message "work") in
          let* () = Test_slack_socket.settle 4 in
          Test_slack_socket.fire clock 540.;
          let* () = Test_slack_socket.settle 4 in
          check_count calls "canvases.create" 0;
          Lwt.wakeup_later release (E.answer body ~model:"gpt-test");
          let* _ = work in
          check_bool "status stream rotated" true
            (count calls "chat.startStream" >= 2);
          no_body_stream calls;
          check_count calls "canvases.create" 1;
          check_carriers calls (Some "100.000001") 1;
          check_bool "no timer survives" true
            (List.for_all
               (fun (t : Test_slack_socket.timer) ->
                 not (Lwt.is_sleeping t.promise))
               clock.timers);
          Lwt.return_unit))

let test_followup_idle ~result_branch () =
  Th.with_ingress (fun base calls handler _ provider _ ->
      artifacts handler;
      (if result_branch then result_only base body
       else
         provider := fun ~model ~messages:_ -> Lwt.return (E.answer body ~model));
      let* _ = legacy base "/followup more" in
      check_count calls "canvases.create" 1;
      check_carriers calls None 1;
      Lwt.return_unit)

let test_followup_busy ~native () =
  Th.with_ingress (fun base calls handler _ provider run ->
      artifacts handler;
      let gate, release = Lwt.task () and n = ref 0 in
      (provider :=
         fun ~model ~messages:_ ->
           incr n;
           if !n = 1 then gate else Lwt.return (E.answer body ~model));
      let work =
        if native then run (Th.message "first")
        else legacy ~thread:"99.000001" base "first"
      in
      let* () = Test_slack_socket.settle 3 in
      let* _ =
        if native then
          run ~id:"queued"
            (Th.message ~thread:"100.000001" ~ts:"100.000003" ~user:"U456"
               "/followup more")
        else legacy base "/followup more"
      in
      let* _ =
        if native then
          run ~id:"append"
            (Th.message ~thread:"100.000001" ~ts:"100.000004" ~user:"U456"
               "/followup-append appended")
        else legacy base "/followup-append appended"
      in
      check_count calls "canvases.create" 0;
      Lwt.wakeup_later release (E.answer "first short answer" ~model:"gpt-test");
      let* _ = work in
      Alcotest.(check int) "follow-up drained once" 2 !n;
      check_count calls "canvases.create" 1;
      check_carriers calls (if native then Some "100.000001" else None) 1;
      Lwt.return_unit)

let test_queue_fallback () =
  Th.with_ingress (fun base calls _ _ _ _ ->
      let item =
        Connector_dispatch.queued_followup ~connector_name:"slack"
          ~channel_id:"C123" ~user_id:"U123" ~is_admin:false "work"
      in
      let item = { item with Session_types.deferred_followup = false } in
      let sent = ref [] in
      let fallback text =
        sent := text :: !sent;
        Lwt.return_unit
      in
      let* () =
        Session.send_queued_final base.manager ~key:base.key ~message:item
          ~fallback body
      in
      Slack_thread_execution.install base.manager;
      let* () =
        Session.send_queued_final base.manager ~key:base.key ~message:item
          ~fallback body
      in
      Alcotest.(check (list string))
        "unset and non-native hook preserve fallback" [ body; body ] !sent;
      check_count calls "canvases.create" 0;
      Lwt.return_unit)

let test_replay () =
  Th.with_ingress (fun base calls handler _ provider _ ->
      artifacts handler;
      let route : Slack_thread_route.t =
        { team = "T123"; app = "A123"; channel = "C123"; root = "100.000001" }
      in
      let key = Slack_thread_route.key route in
      let item =
        Connector_dispatch.queued_followup ~connector_name:"slack"
          ~channel_id:"C123" ~user_id:"U456" ~message_id:"100.000003"
          ~is_admin:false "replayed work"
      in
      let item =
        Session_queue.persist_queued_message base.manager ~key ~source:"live"
          item
      in
      ignore item;
      (provider := fun ~model ~messages:_ -> Lwt.return (E.answer body ~model));
      let* summary =
        Daemon_inbound_replay.replay_durable_inbound_queue
          ~session_manager:base.manager ~config:base.manager.config ()
      in
      Alcotest.(check int) "native replay executed" 1 summary.replayed_count;
      check_count calls "canvases.create" 1;
      check_carriers calls (Some "100.000001") 1;
      Lwt.return_unit)

let test_reauthorize () =
  Th.with_ingress (fun base calls handler _ provider run ->
      artifacts handler;
      (provider :=
         fun ~model ~messages:_ ->
           let cfg = base.manager.config in
           let sc = Option.get cfg.channels.slack in
           Session.update_config base.manager
             {
               cfg with
               channels =
                 { cfg.channels with slack = Some { sc with allow_users = [] } };
             };
           Lwt.return (E.answer body ~model));
      let* _ = run (Th.message "work") in
      check_count calls "canvases.create" 0;
      let key = Th.thread_key base.manager in
      check_bool "revoked final not marked sent" true (pending base key);
      Lwt.return_unit)

let test_notices () =
  Th.with_ingress (fun base calls handler _ _ _ ->
      artifacts handler;
      Session.set_special_command_handler base.manager
        (fun ~key:_ ~message:_ ~send_progress ~interrupt_check:_ ->
          let* () =
            match send_progress with
            | Some send -> send body
            | None -> Lwt.return_unit
          in
          Lwt.return_some "short answer");
      let* _ = legacy base "work" in
      let notify =
        Option.get (Session.find_registered_notifier base.manager ~key:base.key)
      in
      let* () = notify body in
      check_count calls "canvases.create" 0;
      Lwt.return_unit)

let test_agents which () =
  Test_helpers.with_temp_dir (fun dir ->
      let doc = open_out (Filename.concat dir "CLAUDE.md") in
      output_string doc "Project instructions for the test.";
      close_out doc;
      Th.with_ingress (fun base _ _ _ provider _ ->
          let n = ref 0 in
          (provider :=
             fun ~model ~messages:_ ->
               incr n;
               if !n = 1 then
                 Lwt.return
                   (Provider.ToolCalls
                      {
                        calls =
                          [
                            {
                              Provider.id = "read-doc";
                              function_name = "file_read";
                              arguments =
                                Yojson.Safe.to_string
                                  (obj
                                     [
                                       ( "path",
                                         str (Filename.concat dir "example.txt")
                                       );
                                     ]);
                            };
                          ];
                        model;
                        usage = None;
                        provider_response_items_json = None;
                        thinking = None;
                      })
               else Lwt.return (E.answer body ~model));
          ignore
            (Session.set_session_debug base.manager ~key:base.key ~enabled:true);
          let ordinary = ref [] and finals = ref [] in
          let finished, wake = Lwt.wait () in
          let send_reply s =
            ordinary := s :: !ordinary;
            Lwt.return_unit
          in
          let send_final s =
            finals := s :: !finals;
            Lwt.wakeup_later wake ();
            Lwt.return_unit
          in
          (match which with
          | "invoke" ->
              Session.agent_invoke_turn base.manager ~parent_key:base.key
                ~agent_name:"debugger" ~prompt:"work" ~debug_notify:send_reply
                ~send_reply ~send_final ()
          | "delegate" ->
              Session.delegate_turn base.manager ~parent_key:base.key
                ~prompt:"work" ~debug_notify:send_reply ~send_reply ~send_final
                ()
          | _ ->
              Session.fork_and_run base.manager ~parent_key:base.key
                ~prompt:"work" ~debug_notify:send_reply ~send_reply ~send_final
                ());
          let* () = finished in
          let* () = Test_slack_socket.settle 3 in
          Alcotest.(check (list string))
            "only completed answer is final" [ body ] !finals;
          check_bool "debug stays ordinary" true
            (List.exists (fun s -> contains s "debug: llm") !ordinary);
          check_bool "project document notice stays ordinary" true
            (List.exists
               (fun s -> contains s "Loaded project instructions:")
               !ordinary);
          let errors, wake_error = Lwt.wait () in
          Session.agent_invoke_turn base.manager
            ~agent_name:"missing-template-sentinel" ~prompt:"work"
            ~send_reply:(fun s ->
              ordinary := s :: !ordinary;
              Lwt.wakeup_later wake_error ();
              Lwt.return_unit)
            ~send_final ();
          let* () = errors in
          Alcotest.(check (list string))
            "validation errors bypass final" [ body ] !finals;
          Lwt.return_unit))

let test_room ~kind ~native ~result_branch () =
  Th.with_ingress (fun base calls handler _ provider run ->
      artifacts handler;
      let channel =
        if kind = "dm" then "D123" else if kind = "mpdm" then "G123" else "C123"
      in
      let file_only = kind = "dm" || kind = "mpdm" in
      let cfg = base.manager.config in
      let sc = Option.get cfg.channels.slack in
      let cfg =
        {
          cfg with
          channels =
            {
              cfg.channels with
              slack = Some { sc with allow_private_channels = [ channel ] };
            };
          egress =
            {
              cfg.egress with
              default_allowlist =
                {
                  Runtime_config_types.host = "files.slack.com";
                  path = Some "/*";
                  method_ = Some "POST";
                  action = Allow;
                  log_policy = No_log;
                }
                :: cfg.egress.default_allowlist;
            };
        }
      in
      base.cfg := cfg;
      Session.update_config base.manager cfg;
      let previous = !handler in
      (handler :=
         fun m p ->
           match m with
           | "conversations.info" ->
               ok
                 [
                   ( "channel",
                     obj
                       [
                         ("id", str channel);
                         ("is_im", `Bool (kind = "dm"));
                         ("is_mpim", `Bool (kind = "mpdm"));
                         ("is_private", `Bool (kind <> "public"));
                         ("is_member", `Bool true);
                         ("user", str "U123");
                       ] );
                 ]
           | "files.getUploadURLExternal" ->
               ok
                 [
                   ("file_id", str "FFILE");
                   ("upload_url", str "https://files.slack.com/upload/v1/test");
                 ]
           | "files.completeUploadExternal" ->
               let share =
                 obj
                   ([ ("ts", str "300.000001") ]
                   @
                   match get "thread_ts" p with
                   | `String ts -> [ ("thread_ts", str ts) ]
                   | _ -> [])
               in
               ok
                 [
                   ( "files",
                     arr
                       [
                         obj
                           [
                             ("id", str "FFILE");
                             ( "shares",
                               obj
                                 [
                                   ("private", obj [ (channel, arr [ share ]) ]);
                                 ] );
                           ];
                       ] );
                 ]
           | _ -> previous m p);
      let uploaded = ref [] in
      let http = Option.get (Lwt.get Slack_transport.key) in
      let post ~uri ~headers ~body =
        if Uri.host (Uri.of_string uri) = Some "files.slack.com" then (
          uploaded := body :: !uploaded;
          Lwt.return (200, "ok"))
        else http.post ~uri ~headers ~body
      in
      (provider := fun ~model ~messages:_ -> Lwt.return (E.answer body ~model));
      if result_branch then result_only base body;
      let* _ =
        Slack_transport.with_http { http with post } (fun () ->
            if native then run (Th.message ~channel "work")
            else
              Slack.handle_message ~session_manager:base.manager
                ~config:(Option.get base.manager.config.channels.slack)
                (Yojson.Safe.to_string
                   (Th.callback (Th.message ~channel "work"))))
      in
      check_count calls "canvases.create" (if file_only then 0 else 1);
      if file_only then begin
        Alcotest.(check (list string))
          "file keeps exact original bytes" [ body ] !uploaded;
        check_count calls "files.completeUploadExternal" 1;
        let _, p =
          List.find (fun (m, _) -> m = "files.completeUploadExternal") !calls
        in
        Alcotest.(check string)
          "original conversation" channel
          (Slack_huddles.field "channel_id" p);
        Alcotest.(check (option string))
          "original file parent"
          (if native then Some "100.000001" else None)
          (match get "thread_ts" p with `String ts -> Some ts | _ -> None)
      end
      else check_carriers calls (if native then Some "100.000001" else None) 1;
      no_body_stream calls;
      Lwt.return_unit)

let test_autonomous ~result_branch () =
  Th.with_ingress (fun base calls handler _ provider _ ->
      artifacts handler;
      let cfg = base.manager.config in
      Session.update_config base.manager
        {
          cfg with
          agent_defaults =
            {
              cfg.agent_defaults with
              autonomous_continuation_enabled = true;
              autonomous_continuation_delay = 0.0;
            };
        };
      let n = ref 0 in
      let next () =
        incr n;
        if !n = 1 then "short first answer"
        else if !n = 2 then body
        else Session.autonomous_stay_idle_message
      in
      (if result_branch then
         Session.set_special_command_handler base.manager
           (fun ~key:_ ~message:_ ~send_progress:_ ~interrupt_check:_ ->
             Lwt.return_some (next ()))
       else
         provider :=
           fun ~model ~messages:_ -> Lwt.return (E.answer (next ()) ~model));
      let delivered, wake = Lwt.wait () in
      let previous = !handler in
      (handler :=
         fun m p ->
           let result = previous m p in
           if m = "canvases.access.set" && Lwt.is_sleeping delivered then
             Lwt.wakeup_later wake ();
           result);
      let* _ = legacy base "work" in
      let* () = delivered in
      let cfg = base.manager.config in
      Session.update_config base.manager
        {
          cfg with
          agent_defaults =
            { cfg.agent_defaults with autonomous_continuation_enabled = false };
        };
      let* () =
        Session.cancel_autonomous_continuation base.manager ~key:base.key
      in
      let* () = Test_slack_socket.settle 4 in
      check_count calls "canvases.create" 1;
      check_carriers calls None 1;
      Lwt.return_unit)

let test_long_relay () =
  let module R = Test_slack_standing_order_relay in
  let module E = Test_slack_standing_order_exec_support in
  R.with_delivery (fun e lease r payload ->
      Slack_thread_execution.install e.manager;
      let payload =
        { payload with Slack_standing_order_provenance.notes_md = body }
      in
      let uploads = ref [] in
      let upload_http (request : Slack_standing_order_upload.request) =
        uploads := request.body :: !uploads;
        R.raw_success request
      in
      Lwt_main.run
        (Slack_standing_order_dispatch.deliver ~ctx:(E.ctx e) ~db:e.db
           ~manager:e.manager ~http:e.mock.http ~upload_http ~lease r payload);
      Alcotest.(check (list string))
        "long P3 relay keeps exact file bytes" [ body ] !uploads;
      Alcotest.(check int) "no automatic canvas" 0 (E.count e "canvases.create");
      Alcotest.(check int)
        "one relay completion" 1
        (E.count e "files.completeUploadExternal"))

let test_followup_fresh_adapter () =
  Th.with_ingress (fun base calls handler _ provider _ ->
      artifacts handler;
      (provider := fun ~model ~messages:_ -> Lwt.return (E.answer body ~model));
      let injected = ref [] in
      let send_message_fn ~bot_token:_ ~channel_id:_ ~text =
        injected := text :: !injected;
        Lwt.return_unit
      in
      let* _ =
        legacy ~thread:"99.000001" ~send_message_fn base "/followup more"
      in
      check_count calls "canvases.create" 1;
      check_carriers calls None 1;
      check_bool "follow-up uses its own checked adapter" false
        (List.mem body !injected);
      Lwt.return_unit)

let test_typed_conversation () =
  Th.with_ingress (fun base calls handler _ _ _ ->
      artifacts handler;
      let target : Slack_reply_delivery.target =
        {
          session_key = base.key;
          channel_id = "C123";
          thread_ts = None;
          requester = "U123";
          source_ts = Some "100.000001";
          conversation = Slack_reply_context.Unknown;
        }
      in
      let post_text =
        Slack_final_text.make ~manager:base.manager ~route:None
          ~destination:(Slack_reply_delivery.destination target)
          ()
      in
      let* () =
        Slack_final_reply.send ~manager:base.manager ~post_text ~target body
      in
      check_count calls "canvases.create" 1;
      check_carriers calls None 1;
      Lwt.return_unit)

let suite =
  [
    ("final uses checked conversation metadata", `Quick, test_typed_conversation);
    ( "followup uses fresh adapter and legacy root",
      `Quick,
      test_followup_fresh_adapter );
    ("native long final never streams body", `Quick, fun () -> test_native ());
    ( "unavailable native stream still delivers once",
      `Quick,
      fun () -> test_native ~unavailable:true () );
    ("off keeps buffered final", `Quick, fun () -> test_native ~off:true ());
    ("before drain and result deliver once", `Quick, test_legacy_long);
    ( "legacy injected sender reaches final delivery",
      `Quick,
      test_legacy_injection );
    ("native queued finals use sender", `Quick, test_queued);
    ("result branch preserves explicit legacy parent", `Quick, test_result_only);
    ( "short final uses injection",
      `Quick,
      test_injection_modes Slack_config_types.Canvas "short answer" );
    ( "off final uses injection",
      `Quick,
      test_injection_modes Slack_config_types.Off body );
    ( "before drain injection failure stays pending",
      `Quick,
      test_failed_final ~result_branch:false ~injection:true ~status:200 );
    ( "result injection failure stays pending",
      `Quick,
      test_failed_final ~result_branch:true ~injection:true ~status:200 );
    ( "before drain Slack error stays pending",
      `Quick,
      test_failed_final ~result_branch:false ~injection:false ~status:200 );
    ( "result HTTP error stays pending",
      `Quick,
      test_failed_final ~result_branch:true ~injection:false ~status:503 );
    ( "legacy explicit parent bypasses unthreaded injection",
      `Quick,
      test_threaded_injection ~native:false );
    ( "native bypasses unthreaded injection",
      `Quick,
      test_threaded_injection ~native:true );
    ("rotation contains only status", `Quick, test_rotation);
    ( "followup idle before drain final",
      `Quick,
      test_followup_idle ~result_branch:false );
    ( "followup idle result final",
      `Quick,
      test_followup_idle ~result_branch:true );
    ("followup busy native root", `Quick, test_followup_busy ~native:true);
    ( "followup busy legacy historical root",
      `Quick,
      test_followup_busy ~native:false );
    ("non native queued final preserves fallback", `Quick, test_queue_fallback);
    ("native replay finals use sender", `Quick, test_replay);
    ("final reauthorizes current requester", `Quick, test_reauthorize);
    ("relay and notices bypass overflow", `Quick, test_notices);
    ( "agent document notices stay ordinary for invoke",
      `Quick,
      test_agents "invoke" );
    ( "agent document notices stay ordinary for delegate",
      `Quick,
      test_agents "delegate" );
    ("agent document notices stay ordinary for fork", `Quick, test_agents "fork");
    Test_slack_standing_order_support.case "long P3 relay bypasses overflow"
      test_long_relay;
    ( "private native final keeps root",
      `Quick,
      test_room ~kind:"private" ~native:true ~result_branch:false );
    ( "private result final keeps root post",
      `Quick,
      test_room ~kind:"private" ~native:false ~result_branch:true );
    ( "DM native final is a file",
      `Quick,
      test_room ~kind:"dm" ~native:true ~result_branch:false );
    ( "DM result final is a file",
      `Quick,
      test_room ~kind:"dm" ~native:false ~result_branch:true );
    ( "MPDM native final is a file",
      `Quick,
      test_room ~kind:"mpdm" ~native:true ~result_branch:false );
    ( "MPDM result final is a file",
      `Quick,
      test_room ~kind:"mpdm" ~native:false ~result_branch:true );
    ( "live autonomous final uses sender after before drain",
      `Quick,
      test_autonomous ~result_branch:false );
    ( "live autonomous final uses sender after result",
      `Quick,
      test_autonomous ~result_branch:true );
  ]

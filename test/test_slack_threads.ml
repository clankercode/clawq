open Lwt.Syntax
open Test_slack_huddle_fixtures

let callback ?(team = "T123") ?(id = "Ev1") event =
  obj
    [
      ("type", str "event_callback");
      ("team_id", str team);
      ("api_app_id", str "A123");
      ("event_id", str id);
      ("event", event);
    ]

let message ?(kind = "message") ?(channel = "C123") ?thread ?(ts = "100.000001")
    ?(user = "U123") text =
  obj
    ([
       ("type", str kind);
       ("channel", str channel);
       ("user", str user);
       ("ts", str ts);
       ("text", str text);
     ]
    @ match thread with None -> [] | Some t -> [ ("thread_ts", str t) ])

let with_ingress ?(delay = fun _ _ -> Lwt.return_unit) ?key f =
  with_fixture ?key (fun base ->
      Test_slack_huddle_fix1.configure_mock_provider base;
      let cfg = base.manager.config in
      let cfg =
        {
          cfg with
          egress =
            {
              cfg.egress with
              default_allowlist =
                List.map
                  (fun (r : Runtime_config_types.egress_rule) ->
                    { r with method_ = None })
                  cfg.egress.default_allowlist;
            };
        }
      in
      base.manager.config <- cfg;
      base.cfg := cfg;
      let calls = ref [] and seen = ref [] and seq = ref 0 in
      let handler =
        ref (fun method_ params ->
            match method_ with
            | "auth.test" ->
                ok
                  [
                    ("team_id", str "T123");
                    ("user_id", str "UBOT");
                    ("bot_id", str "BOWN");
                  ]
            | "users.info" ->
                ok
                  [
                    ( "user",
                      obj
                        [
                          ("id", get "user" params);
                          ("team_id", str "T123");
                          ("deleted", `Bool false);
                          ("is_bot", `Bool false);
                          ("is_restricted", `Bool false);
                          ("is_ultra_restricted", `Bool false);
                        ] );
                  ]
            | "conversations.info" ->
                ok
                  [
                    ( "channel",
                      obj
                        [
                          ("id", str "C123");
                          ("is_member", `Bool true);
                          ("is_private", `Bool false);
                        ] );
                  ]
            | "conversations.members" ->
                ok
                  [
                    ("members", arr [ str "U123"; str "UBOT"; str "U456" ]);
                    ("response_metadata", obj [ ("next_cursor", str "") ]);
                  ]
            | "conversations.replies" ->
                ok
                  [
                    ( "messages",
                      arr
                        [
                          message "root context";
                          message ~ts:"100.000002" "earlier reply";
                        ] );
                    ("has_more", `Bool false);
                  ]
            | "agents.sessions.setStatus" ->
                ok
                  [
                    ("status", get "status" params);
                    ("agent_status", get "status" params);
                  ]
            | "agents.sessions.rename" -> ok [ ("title", get "title" params) ]
            | "chat.postMessage" | "chat.startStream" ->
                incr seq;
                ok
                  [
                    ("ts", str (Printf.sprintf "200.%06d" !seq));
                    ("channel", str "C123");
                  ]
            | "chat.update" | "chat.stopStream" | "chat.delete"
            | "reactions.add" | "reactions.remove" ->
                ok []
            | m -> Alcotest.fail ("default-deny Slack transport: " ^ m))
      in
      let api method_ params =
        calls := !calls @ [ (method_, params) ];
        let* () = delay method_ params in
        Lwt.return (200, Yojson.Safe.to_string (!handler method_ params))
      in
      let get_http ~uri ~headers:_ =
        let u = Uri.of_string uri in
        let method_ = Filename.basename (Uri.path u) in
        let params =
          obj
            (List.map
               (fun (k, vs) -> (k, str (String.concat "" vs)))
               (Uri.query u))
        in
        api method_ params
      in
      let post ~uri ~headers:_ ~body =
        api (Filename.basename (Uri.path (Uri.of_string uri))) (parse body)
      in
      let provider =
        ref (fun ~model ~messages ->
            seen := messages :: !seen;
            Lwt.return
              (Provider.Text
                 {
                   content = "finished";
                   model;
                   usage = None;
                   provider_response_items_json = None;
                   thinking = None;
                 }))
      in
      let old_complete = !Provider.native_complete
      and old_stream = !Provider.native_stream in
      let complete ~config:_ ~provider:_ ~model ~messages ?tools:_
          ?session_key:_ () =
        !provider ~model ~messages
      in
      Provider.native_complete := [ (Provider.OpenAICompat, complete) ];
      Provider.native_stream :=
        [
          ( Provider.OpenAICompat,
            fun ~config
              ~provider
              ~model
              ~messages
              ?tools
              ?session_key
              ~on_chunk
              ()
            ->
              let* result =
                complete ~config ~provider ~model ~messages ?tools ?session_key
                  ()
              in
              let* () = on_chunk (Provider.Delta "partial") in
              Lwt.return result );
        ];
      let prefix = Temp_downloads.generate_token () in
      let run ?(id = "Ev1") event =
        Slack.handle_event
          ~config:(Option.get base.manager.config.channels.slack)
          ~session_manager:base.manager
          (Yojson.Safe.to_string (callback ~id:(prefix ^ id) event))
      in
      Lwt.finalize
        (fun () ->
          Slack_transport.with_http { get = get_http; post } (fun () ->
              f base calls handler seen provider run))
        (fun () ->
          Provider.native_complete := old_complete;
          Provider.native_stream := old_stream;
          Lwt.return_unit))

let test_mention () =
  with_ingress (fun _ calls _ seen _ run ->
      let* _ = run (message ~kind:"app_mention" "please help") in
      check_bool "mention reaches production provider" true (!seen <> []);
      check_bool "native processing by thread locator" true
        (List.exists
           (fun (m, p) ->
             m = "agents.sessions.setStatus"
             && get "channel_id" p = str "C123"
             && get "thread_ts" p = str "100.000001"
             && get "status" p = str "processing")
           !calls);
      Lwt.return_unit)

let test_thread_identity () =
  with_ingress (fun f calls _ _ _ run ->
      let* _ = run (message "first") in
      let* _ =
        run ~id:"Ev2"
          (message ~ts:"100.000003" ~thread:"100.000001" ~user:"U456" "followup")
      in
      let* _ = run ~id:"Ev3" (message ~ts:"101.000001" "second thread") in
      let keys =
        Hashtbl.to_seq_keys f.manager.sessions
        |> List.of_seq
        |> List.filter (fun k -> k <> f.key)
      in
      Alcotest.(check int)
        "two independent threads; same thread across humans" 2
        (List.length keys);
      check_bool "every posted response/progress bound to a root" true
        (List.for_all
           (fun (m, p) -> m <> "chat.postMessage" || get "thread_ts" p <> `Null)
           !calls);
      Lwt.return_unit)

let test_history () =
  with_ingress (fun _ calls _ seen _ run ->
      let* _ = run (message ~thread:"100.000001" ~ts:"100.000004" "continue") in
      check_bool "full thread API read" true
        (List.exists (fun (m, _) -> m = "conversations.replies") !calls);
      check_bool "root and earlier reply sent to model as context" true
        (List.exists
           (List.exists (fun (m : Provider.message) ->
                contains m.content "root context"
                && contains m.content "earlier reply"))
           !seen);
      check_bool "terminal explicit active" true
        (List.exists
           (fun (m, p) ->
             m = "agents.sessions.setStatus" && get "status" p = str "active")
           !calls);
      Lwt.return_unit)

let test_guest_denied () =
  with_ingress (fun base calls handler seen _ run ->
      base.manager.config <-
        {
          base.manager.config with
          external_room_policy =
            {
              default_action =
                Runtime_config_types.Policy_deny ("guest test", false);
              per_connector = [];
            };
        };
      let old = !handler in
      (handler :=
         fun m p ->
           if m = "users.info" then
             let j = old m p in
             update j
               [
                 ( "user",
                   update (get "user" j) [ ("is_restricted", `Bool true) ] );
               ]
           else old m p);
      let* _ = run (message "restricted") in
      (* Default room policy requires explicit approval for external rooms. *)
      check_bool "guest policy enforced before model" true (!seen = []);
      check_bool "guest denial before history" true
        (not (List.exists (fun (m, _) -> m = "conversations.replies") !calls));
      Lwt.return_unit)

let interactive ~reference ~message_ts ?(user = "U123") () =
  obj
    [
      ("type", str "block_actions");
      ("api_app_id", str "A123");
      ("team", obj [ ("id", str "T123") ]);
      ("user", obj [ ("id", str user) ]);
      ( "container",
        obj [ ("channel_id", str "C123"); ("message_ts", str message_ts) ] );
      ("message", obj [ ("ts", str message_ts) ]);
      ( "actions",
        arr
          [
            obj
              [ ("action_id", str "clawq_stop_run"); ("value", str reference) ];
          ] );
    ]

let control_values calls =
  List.filter_map
    (fun (m, p) ->
      if m <> "chat.postMessage" then None
      else
        match get "blocks" p with
        | `List [ _; actions ] -> (
            match get "elements" actions with
            | `List [ b ] -> Some (get "value" b |> Yojson.Safe.Util.to_string)
            | _ -> None)
        | _ -> None)
    !calls

let thread_key manager =
  Hashtbl.to_seq_keys manager.Session.sessions
  |> List.of_seq
  |> List.find (fun k -> contains k ":thread:")

let interrupted manager =
  Session.interrupt_check_if_present manager ~key:(thread_key manager) ()
  = Some Agent.stop_interrupt_token

let stop_event streams =
  obj
    [
      ("type", str "agent_session_stopped");
      ("channel", str "C123");
      ("thread_ts", str "100.000001");
      ("user", str "U123");
      ("event_ts", str "300.1");
      ("streaming_message_ts", arr (List.map str streams));
    ]

let release_text resolver model =
  Lwt.wakeup_later resolver
    (Provider.Text
       {
         content = "finished";
         model;
         usage = None;
         provider_response_items_json = None;
         thinking = None;
       })

let test_stop ~native () =
  with_ingress (fun base calls _ _ provider run ->
      let gate, release = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> gate);
      let work = run (message "work") in
      let* () = Test_slack_socket.settle 3 in
      check_bool "model active" true (Lwt.is_sleeping work);
      let* _ =
        if native then run ~id:"stop" (stop_event [ "200.000002" ])
        else
          Slack.handle_event
            ~config:(Option.get base.manager.config.channels.slack)
            ~session_manager:base.manager
            (Yojson.Safe.to_string
               (interactive
                  ~reference:(List.hd (control_values calls))
                  ~message_ts:"200.000001" ()))
      in
      check_bool "matching owned run interrupted" true
        (interrupted base.manager);
      release_text release "gpt-test";
      let* _ = work in
      check_bool "processing explicitly cleared" true
        (List.exists
           (fun (m, p) ->
             m = "agents.sessions.setStatus" && get "status" p = str "active")
           !calls);
      Lwt.return_unit)

let test_queued_stop () =
  with_ingress (fun base calls _ _ provider run ->
      let gate, release = Lwt.task () and count = ref 0 in
      (provider :=
         fun ~model:_ ~messages:_ ->
           incr count;
           gate);
      let work = run (message "first") in
      let* () = Test_slack_socket.settle 2 in
      let* _ =
        run ~id:"queued"
          (message ~thread:"100.000001" ~ts:"100.000003" "queued")
      in
      let controls = control_values calls in
      Alcotest.(check int) "queued control is distinct" 2 (List.length controls);
      let* _ =
        Slack.handle_event
          ~config:(Option.get base.manager.config.channels.slack)
          ~session_manager:base.manager
          (Yojson.Safe.to_string
             (interactive ~reference:(List.nth controls 1)
                ~message_ts:"200.000003" ()))
      in
      check_bool "queued cancel leaves active provider intact" false
        (interrupted base.manager);
      release_text release "gpt-test";
      let* _ = work in
      Alcotest.(check int) "cancelled queued request never executes" 1 !count;
      Lwt.return_unit)

let test_stale_stop () =
  with_ingress (fun base calls _ _ provider run ->
      let* _ = run (message "first") in
      let old_control = List.hd (control_values calls) in
      let gate, release = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> gate);
      let work =
        run ~id:"next" (message ~thread:"100.000001" ~ts:"100.000005" "next")
      in
      let* () = Test_slack_socket.settle 2 in
      let* _ = run ~id:"stale" (stop_event [ "200.000002" ]) in
      let* _ = run ~id:"empty" (stop_event []) in
      let* _ =
        Slack.handle_event
          ~config:(Option.get base.manager.config.channels.slack)
          ~session_manager:base.manager
          (Yojson.Safe.to_string
             (interactive ~reference:old_control ~message_ts:"200.000001" ()))
      in
      check_bool "stale/empty native and stale button cannot stop newer run"
        false (interrupted base.manager);
      release_text release "gpt-test";
      let* _ = work in
      Lwt.return_unit)

let test_routing () =
  with_ingress (fun base calls _ _ _ run ->
      let* _ = run (message "original") in
      let key = thread_key base.manager in
      Session.unregister_channel_notifier base.manager ~key;
      let* () =
        Daemon_resume.notify_resumed_session ~session_manager:base.manager
          ~config:base.manager.config ~session_key:key ~channel:"slack"
          ~channel_id:"C123" "restart result"
      in
      check_bool "fallback resumes in original thread" true
        (List.exists
           (fun (m, p) ->
             m = "chat.postMessage"
             && get "text" p = str "restart result"
             && get "thread_ts" p = str "100.000001"
             && get "channel" p = str "C123")
           !calls);
      let context = { Tool.default_context with session_key = Some key } in
      let _, _, thread, _ =
        Background_task_context.origin_fields_from_context ~db:base.db ~context
          ()
      in
      Alcotest.(check (option string))
        "background origin persists root" (Some "100.000001") thread;
      let _, ch, cid =
        Background_task_context.routing_from_context ~context ()
      in
      Alcotest.(check (option string))
        "background actual channel" (Some "C123") cid;
      Alcotest.(check (option string)) "background connector" (Some "slack") ch;
      Lwt.return_unit)

let suite =
  [
    ("native owned stream stop", `Quick, test_stop ~native:true);
    ("public run-specific button stop", `Quick, test_stop ~native:false);
    ("queued request stop", `Quick, test_queued_stop);
    ("stale controls and empty native stop", `Quick, test_stale_stop);
    ("persisted resume and task origin", `Quick, test_routing);
    ("mention native lifecycle", `Quick, test_mention);
    ("stable independent root sessions", `Quick, test_thread_identity);
    ("authorized thread bootstrap and cleanup", `Quick, test_history);
    ("guest policy before content", `Quick, test_guest_denied);
  ]

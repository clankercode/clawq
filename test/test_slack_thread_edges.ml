open Lwt.Syntax
open Test_slack_huddle_fixtures
open Test_slack_threads

let test_history_pages () =
  with_ingress (fun _ calls handler seen _ run ->
      let old = !handler in
      (handler :=
         fun m p ->
           if m <> "conversations.replies" then old m p
           else if get "cursor" p = `Null then
             ok
               [
                 ( "messages",
                   arr [ message ~ts:"10.1" "ten"; message ~ts:"9.1" "nine" ] );
                 ("has_more", `Bool true);
                 ("response_metadata", obj [ ("next_cursor", str "next") ]);
               ]
           else
             ok
               [
                 ( "messages",
                   arr
                     [
                       message ~ts:"9.1" "nine"; message ~ts:"100.000001" "root";
                     ] );
                 ("has_more", `Bool false);
               ]);
      let* _ = run (message ~ts:"100.3" ~thread:"100.000001" "continue") in
      Alcotest.(check int)
        "two bounded pages" 2
        (List.length
           (List.filter (fun (m, _) -> m = "conversations.replies") !calls));
      let prompt =
        List.concat !seen
        |> List.find (fun (m : Provider.message) ->
            contains m.content "Slack thread context")
      in
      let index text =
        let rec loop i =
          if i + String.length text > String.length prompt.content then max_int
          else if String.sub prompt.content i (String.length text) = text then i
          else loop (i + 1)
        in
        loop 0
      in
      check_bool "numeric chronological Slack timestamps" true
        (index "nine" < index "ten");
      Lwt.return_unit)

let test_context_ownership () =
  with_ingress (fun _ _ handler seen _ run ->
      let old = !handler in
      (handler :=
         fun m p ->
           if m = "conversations.replies" then
             ok
               [
                 ( "messages",
                   arr
                     [
                       message "root";
                       update
                         (message ~ts:"100.2" ~user:"UBOT" "own response")
                         [ ("bot_id", str "BOWN") ];
                       update
                         (message ~ts:"100.3" ~user:"UOTHER"
                            "foreign instructions")
                         [ ("bot_id", str "BOTHER") ];
                     ] );
                 ("has_more", `Bool false);
               ]
           else old m p);
      let* _ = run (message ~thread:"100.000001" ~ts:"100.4" "continue") in
      let text =
        List.concat !seen
        |> List.map (fun (m : Provider.message) -> m.content)
        |> String.concat "\n"
      in
      check_bool "only own-bot source labeled assistant" true
        (contains text "\"role\":\"assistant\",\"text\":\"own response\"");
      check_bool "other bots remain untrusted" true
        (contains text
           "\"role\":\"untrusted_source\",\"text\":\"foreign instructions\"");
      Lwt.return_unit)

let test_partial_history () =
  with_ingress (fun _ _ handler seen _ run ->
      let old = !handler in
      (handler :=
         fun m p ->
           if m = "conversations.replies" then
             ok
               [
                 ("messages", arr []);
                 ("is_limited", `Bool true);
                 ("has_more", `Bool false);
               ]
           else old m p);
      let* _ = run (message "work") in
      check_bool "empty limited page cannot claim coverage" true
        (List.concat !seen
        |> List.exists (fun (m : Provider.message) ->
            contains m.content "\"complete\":false"
            && contains m.content "\"is_limited\":true"));
      Lwt.return_unit)

let test_bootstrap () =
  with_ingress (fun _ calls _ seen _ run ->
      let* _ =
        run
          (obj
             [
               ("type", str "app_home_opened");
               ("tab", str "messages");
               ("user", str "U123");
               ("channel", str "D123");
               ("event_ts", str "150.1");
             ])
      in
      let* _ =
        run ~id:"context"
          (obj
             [
               ("type", str "app_context_changed");
               ( "context",
                 obj
                   [
                     ( "entities",
                       arr
                         [
                           obj
                             [
                               ("type", str "slack#/types/channel_id");
                               ("value", str "CSECRET");
                               ("team_id", str "TOTHER");
                             ];
                         ] );
                   ] );
             ])
      in
      let* _ =
        run ~id:"empty-context"
          (obj [ ("type", str "app_context_changed"); ("context", obj []) ])
      in
      check_bool "no actor/thread inferred from browsing context" true
        (!calls = [] && !seen = []);
      Lwt.return_unit)

let test_denial ~membership () =
  with_ingress (fun base calls handler seen _ run ->
      (if membership then
         let old = !handler in
         handler :=
           fun m p ->
             if m = "conversations.members" then ok [ ("members", arr []) ]
             else old m p
       else
         let cfg = base.manager.config in
         let sc = Option.get cfg.channels.slack in
         base.manager.config <-
           {
             cfg with
             channels =
               {
                 cfg.channels with
                 slack = Some { sc with allow_users = [ "UOTHER" ] };
               };
           });
      let* _ = run (message "denied") in
      check_bool "denied before model or content" true
        (!seen = []
        && not (List.exists (fun (m, _) -> m = "conversations.replies") !calls)
        );
      Lwt.return_unit)

let test_failure_cleanup () =
  with_ingress (fun _ calls _ _ provider run ->
      (provider := fun ~model:_ ~messages:_ -> Lwt.fail_with "fixture failure");
      let* _ = run (message "failure") in
      check_bool "failed task clears processing" true
        (List.exists
           (fun (m, p) ->
             m = "agents.sessions.setStatus" && get "status" p = str "active")
           !calls);
      check_bool "failure reported in origin thread" true
        (List.exists
           (fun (m, p) ->
             m = "chat.postMessage"
             && contains (Slack_huddles.field "text" p) "error occurred"
             && get "thread_ts" p = str "100.000001")
           !calls);
      Lwt.return_unit)

let test_feature_disabled () =
  with_ingress (fun _ calls handler seen _ run ->
      let old = !handler in
      (handler :=
         fun m p ->
           if
             String.starts_with ~prefix:"agents.sessions." m
             || m = "chat.startStream"
           then obj [ ("ok", `Bool false); ("error", str "feature_disabled") ]
           else old m p);
      let* _ = run (message "fallback") in
      check_bool "ordinary fallback executes" true (!seen <> []);
      check_bool "fallback stop control and final" true
        (control_values calls <> []
        && List.exists
             (fun (m, p) ->
               m = "chat.postMessage"
               && get "text" p = str "finished"
               && get "thread_ts" p = str "100.000001")
             !calls);
      Lwt.return_unit)

let test_notifier_failure () =
  with_ingress (fun base calls _ _ _ run ->
      let* _ = run (message "original") in
      let key = thread_key base.manager in
      Session.register_channel_notifier base.manager ~key (fun _ ->
          Lwt.fail_with "lost notifier");
      let* () =
        Daemon_resume.notify_resumed_session ~session_manager:base.manager
          ~config:base.manager.config ~session_key:key ~channel:"slack"
          ~channel_id:"C123" "lost notifier fallback"
      in
      check_bool "failed notifier falls back to persisted origin thread" true
        (List.exists
           (fun (m, p) ->
             m = "chat.postMessage"
             && get "text" p = str "lost notifier fallback"
             && get "thread_ts" p = str "100.000001")
           !calls);
      Lwt.return_unit)

let signed_request sc body =
  let timestamp = Printf.sprintf "%.0f" (Unix.gettimeofday ()) in
  let signature =
    Test_helpers.slack_signature ~secret:sc.Runtime_config.signing_secret
      ~basestring:("v0:" ^ timestamp ^ ":" ^ body)
  in
  Cohttp.Request.make ~meth:`POST
    ~headers:
      (Cohttp.Header.of_list
         [
           ("x-slack-signature", signature);
           ("x-slack-request-timestamp", timestamp);
         ])
    (Uri.of_string "http://fixture/slack/events")

let http manager body =
  let sc = Option.get manager.Session.config.channels.slack in
  Http_server_webhooks.handle ~session_manager:manager ~auth_token:None
    ~slack_config:sc `POST sc.events_path (signed_request sc body)
    (Cohttp_lwt.Body.of_string body)

let test_http () =
  with_ingress (fun base calls _ _ provider _ ->
      let gate, release = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> gate);
      let body =
        callback
          ~id:(Temp_downloads.generate_token ())
          (message ~kind:"app_mention" "HTTP task")
        |> Yojson.Safe.to_string
      in
      let* response = http base.manager body in
      check_bool "signed HTTP promptly acknowledges pending model" true
        (Option.is_some response);
      let* () = Test_slack_socket.settle 4 in
      let control = List.hd (control_values calls) in
      let payload =
        interactive ~reference:control ~message_ts:"200.000001" ()
        |> Yojson.Safe.to_string
      in
      let encoded = Uri.encoded_of_query [ ("payload", [ payload ]) ] in
      let* _ = http base.manager encoded in
      let* () = Test_slack_socket.settle 4 in
      check_bool "signed form interaction cancels actual run" true
        (interrupted base.manager);
      release_text release "gpt-test";
      Test_slack_socket.settle 10)

let test_socket ?(empty_secret = false) () =
  with_ingress (fun base calls _ _ provider _ ->
      let gate, release = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> gate);
      let sc = Option.get base.manager.config.channels.slack in
      let sc =
        if empty_secret then
          let parsed =
            Config_loader.parse_config ~resolve_secrets:false
              (obj
                 [
                   ( "channels",
                     obj
                       [
                         ( "slack",
                           obj
                             [
                               ("bot_token", str sc.bot_token);
                               ("app_token", str "xapp-fixture");
                               ("socket_mode", `Bool true);
                               ("signing_secret", str "");
                             ] );
                       ] );
                 ])
          in
          Option.get parsed.channels.slack
        else sc
      in
      let config =
        {
          base.manager.config with
          channels =
            {
              base.manager.config.channels with
              slack =
                Some { sc with socket_mode = true; app_token = "xapp-fixture" };
            };
        }
      in
      base.manager.config <- config;
      let socket = Test_slack_socket.make_socket ()
      and clock = Test_slack_socket.make_clock () in
      let loop =
        Slack_socket.start_with
          ~connect:(fun () -> Lwt.return socket.transport)
          ~on_health:(fun _ -> ())
          ~sleep:(Test_slack_socket.sleep clock)
          ~config ~session_manager:base.manager
          ~event_limiter:
            (Rate_limiter.create ~rate_per_minute:100 ~burst_multiplier:1.0)
          ()
      in
      Lwt.finalize
        (fun () ->
          let* () = socket.emit {|{"type":"hello"}|} in
          let event =
            callback
              ~id:(Temp_downloads.generate_token ())
              (message ~kind:"app_mention" "Socket task")
          in
          let envelope kind id payload =
            Yojson.Safe.to_string
              (obj
                 [
                   ("envelope_id", str id);
                   ("type", str kind);
                   ("payload", payload);
                 ])
          in
          let* () = socket.emit (envelope "events_api" "env-event" event) in
          let* () = Test_slack_socket.settle 4 in
          let* () =
            socket.emit
              (envelope "interactive" "env-action"
                 (interactive
                    ~reference:(List.hd (control_values calls))
                    ~message_ts:"200.000001" ()))
          in
          let* () = Test_slack_socket.settle 4 in
          check_bool "socket interactive runs after envelope ack" true
            (List.length !(socket.acks) = 2 && interrupted base.manager);
          release_text release "gpt-test";
          Test_slack_socket.settle 10)
        (fun () ->
          Lwt.cancel loop;
          Lwt.catch (fun () -> loop) (fun _ -> Lwt.return_unit)))

let test_background () =
  with_ingress (fun base calls _ _ provider run ->
      let* _ = run (message "origin") in
      let key = thread_key base.manager in
      let fresh = Session.create ~config:base.manager.config ~db:base.db () in
      Slack_thread_execution.install fresh;
      let invoked = ref 0 in
      (provider :=
         fun ~model ~messages:_ ->
           incr invoked;
           Lwt.return
             (Provider.Text
                {
                  model;
                  content = "background result";
                  usage = None;
                  thinking = None;
                  provider_response_items_json = None;
                }));
      let task =
        {
          (Test_daemon.make_test_task ()) with
          session_key = Some key;
          channel = Some "slack";
          channel_id = Some "C123";
          agent_name = Some "fixture";
          origin_json = None;
          thread_id = None;
          use_worktree = false;
        }
      in
      let* () =
        Daemon_util.notify_background_task_finished ~session_manager:fresh
          ~config:fresh.config task
      in
      let* () = Session.cancel_autonomous_continuation fresh ~key in
      let* () = Session.cancel_autonomous_continuation base.manager ~key in
      let* () = Slack_thread_runs.shutdown fresh in
      Alcotest.(check int)
        "restart completion executes one authorized task turn" 1 !invoked;
      check_bool
        "restart background final stays threaded without origin or notifier"
        true
        (List.exists
           (fun (m, p) ->
             m = "chat.postMessage"
             && get "text" p = str "background result"
             && get "channel" p = str "C123"
             && get "thread_ts" p = str "100.000001")
           !calls);
      Lwt.return_unit)

let test_metadata ~shared () =
  with_ingress (fun base calls handler seen _ run ->
      let cfg = base.manager.config in
      base.manager.config <-
        {
          cfg with
          external_room_policy =
            {
              default_action =
                Runtime_config_types.Policy_deny ("shared denied", false);
              per_connector = [];
            };
        };
      let old = !handler in
      let privacy = ref [] in
      (handler :=
         fun m p ->
           if m = "conversations.info" then
             ok
               [
                 ( "channel",
                   obj
                     ([ ("id", str "C123"); ("is_member", `Bool true) ]
                     @
                     if shared then
                       [
                         ("is_private", `Bool false); ("is_shared", `Bool true);
                       ]
                     else !privacy) );
               ]
           else old m p);
      let* () =
        Lwt_list.iter_s
          (fun (i, value) ->
            privacy := value;
            let* _ =
              run
                ~id:("metadata" ^ string_of_int i)
                (message ~ts:("100." ^ string_of_int i) "must deny")
            in
            Lwt.return_unit)
          [
            (1, []);
            (2, [ ("is_private", str "false") ]);
            (3, [ ("is_private", `Int 0) ]);
          ]
      in
      check_bool "unknown privacy or shared policy fails before history/model"
        true
        (!seen = []
        && not (List.exists (fun (m, _) -> m = "conversations.replies") !calls)
        );
      Lwt.return_unit)

let test_queued_stop_during_preparation () =
  let ui_gate, ui_release = Lwt.task () in
  let preparing = ref false in
  let delay method_ params =
    if
      method_ = "chat.postMessage"
      && contains (Slack_huddles.field "text" params) "Processing queued"
    then (
      preparing := true;
      ui_gate)
    else Lwt.return_unit
  in
  with_ingress ~delay (fun base calls _ _ provider run ->
      let provider_gate, provider_release = Lwt.task () and count = ref 0 in
      (provider :=
         fun ~model:_ ~messages:_ ->
           incr count;
           provider_gate);
      let work = run (message "first") in
      let* () = Test_slack_socket.settle 3 in
      let* _ =
        run ~id:"queued"
          (message ~thread:"100.000001" ~ts:"100.000003" "queued")
      in
      let* _ =
        run ~id:"later"
          (message ~thread:"100.000001" ~ts:"100.000004" "later request")
      in
      let controls = control_values calls in
      Alcotest.(check int) "each request owns controls" 3 (List.length controls);
      release_text provider_release "gpt-test";
      let* () = Test_slack_socket.settle 5 in
      check_bool "queued UI preparation awaits HTTP reply" true !preparing;
      let* _ =
        Slack.handle_event
          ~config:(Option.get base.manager.config.channels.slack)
          ~session_manager:base.manager
          (Yojson.Safe.to_string
             (interactive ~reference:(List.nth controls 1)
                ~message_ts:"200.000003" ()))
      in
      Lwt.wakeup_later ui_release ();
      let* _ = work in
      Alcotest.(check int)
        "cancelled request skipped while later request still executes" 2 !count;
      check_bool "cancelled drained request cannot replay after restart" true
        (Memory.queue_list ~db:base.db ~session_key:(thread_key base.manager)
        = []);
      Lwt.return_unit)

let test_delayed_cleanup () =
  let ui_gate, ui_release = Lwt.task () in
  let delayed = ref false in
  let delay m _ =
    if m = "chat.stopStream" && not !delayed then (
      delayed := true;
      ui_gate)
    else Lwt.return_unit
  in
  with_ingress ~delay (fun base calls _ _ provider run ->
      let second_gate, second_release = Lwt.task () and n = ref 0 in
      (provider :=
         fun ~model:_ ~messages:_ ->
           incr n;
           if !n = 1 then Lwt.fail_with "first fails" else second_gate);
      let first = run (message "first") in
      let* () = Test_slack_socket.settle 3 in
      check_bool "old cleanup HTTP delayed" true !delayed;
      let second =
        run ~id:"second" (message ~thread:"100.000001" ~ts:"100.5" "second")
      in
      let* () = Test_slack_socket.settle 3 in
      let processing () =
        List.length
          (List.filter
             (fun (m, p) ->
               m = "agents.sessions.setStatus"
               && get "status" p = str "processing")
             !calls)
      in
      Alcotest.(check int)
        "new processing waits for old terminal UI writes" 1 (processing ());
      Lwt.wakeup_later ui_release ();
      let* _ = first in
      let* () = Test_slack_socket.settle 3 in
      Alcotest.(check int) "new processing then starts" 2 (processing ());
      check_bool "old cleanup never interrupts newer provider" false
        (interrupted base.manager);
      release_text second_release "gpt-test";
      let* _ = second in
      Lwt.return_unit)

let test_profile () =
  with_ingress (fun base _ _ seen provider run ->
      let cfg = base.manager.config in
      let parsed =
        Config_loader.parse_config ~resolve_secrets:false
          (obj
             [
               ( "room_profiles",
                 arr
                   [
                     obj
                       [
                         ("id", str "bound");
                         ("model", str "openai:profile-model");
                         ("system_prompt", str "BOUND INSTRUCTIONS");
                         ("denied_tools", arr [ str "blocked_tool" ]);
                       ];
                   ] );
               ( "room_profile_bindings",
                 arr
                   [
                     obj
                       [
                         ("profile_id", str "bound");
                         ("room", str "C123");
                         ("active", `Bool true);
                       ];
                   ] );
             ])
      in
      let cfg =
        {
          cfg with
          room_profiles = parsed.room_profiles;
          room_profile_bindings = parsed.room_profile_bindings;
          security =
            {
              cfg.security with
              workspace_only = false;
              allowed_cwd_patterns = [];
            };
        }
      in
      Session.update_config base.manager cfg;
      let parent_cwd = Sys.getcwd () in
      Memory.set_session_cwd ~db:base.db ~session_key:"slack:C123"
        ~cwd:(Some parent_cwd);
      let models = ref [] in
      (provider :=
         fun ~model ~messages ->
           models := model :: !models;
           seen := messages :: !seen;
           Lwt.return
             (Provider.Text
                {
                  model;
                  content = "profile done";
                  usage = None;
                  provider_response_items_json = None;
                  thinking = None;
                }));
      let* _ = run (message "one") in
      let key = thread_key base.manager in
      let agent, _, _ = Hashtbl.find base.manager.sessions key in
      Alcotest.(check string)
        "bound model on real turn" "profile-model" (List.hd !models);
      Alcotest.(check (option string))
        "bound template system prompt" (Some "BOUND INSTRUCTIONS")
        agent.room_profile_system_prompt;
      Alcotest.(check (option string))
        "room CWD inherited without shared history" (Some parent_cwd)
        agent.effective_cwd;
      let access =
        Runtime_config.resolve_effective_access cfg ~session_key:key ()
      in
      check_bool "bound profile denial retained at actual thread key" true
        (List.exists
           (fun (x : Runtime_config.effective_access_item) ->
             x.value = "blocked_tool")
           access.denied_tools);
      let profile = List.hd cfg.room_profiles in
      let cfg =
        {
          cfg with
          room_profiles =
            [
              {
                profile with
                model = "openai:reloaded-model";
                system_prompt = "RELOADED INSTRUCTIONS";
              };
            ];
        }
      in
      Session.update_config base.manager cfg;
      let* _ =
        run ~id:"follow" (message ~thread:"100.000001" ~ts:"100.5" "followup")
      in
      Alcotest.(check string)
        "reload applies model to live thread" "reloaded-model" (List.hd !models);
      Alcotest.(check (option string))
        "reload template" (Some "RELOADED INSTRUCTIONS")
        agent.room_profile_system_prompt;
      let* _ = run ~id:"other" (message ~ts:"101.1" "other thread") in
      check_bool "thread histories remain separate under same binding" true
        (Hashtbl.to_seq base.manager.sessions
        |> List.of_seq
        |> List.exists (fun (k, (a, _, _)) ->
            k <> key && contains k ":thread:"
            && not
                 (List.exists
                    (fun (m : Provider.message) ->
                      contains m.content "followup")
                    a.Agent.history)));
      Lwt.return_unit)

let test_room_egress () =
  with_ingress (fun base calls _ seen _ run ->
      Test_slack_huddle_fix1.configure_policy base ~room:"C123" ~profile:false
        ~deny:true;
      base.manager.config <- !(base.cfg);
      let* _ = run (message "egress denied") in
      check_bool "actual room egress deny precedes transport and model" true
        (!calls = [] && !seen = []);
      Lwt.return_unit)

let test_stop_owner () =
  with_ingress (fun base calls _ _ provider run ->
      let gate, release = Lwt.task () in
      (provider := fun ~model:_ ~messages:_ -> gate);
      let work = run (message "owned") in
      let* () = Test_slack_socket.settle 2 in
      let payload =
        interactive
          ~reference:(List.hd (control_values calls))
          ~message_ts:"200.000001" ~user:"U456" ()
      in
      let* _ =
        Slack.handle_event
          ~config:(Option.get base.manager.config.channels.slack)
          ~session_manager:base.manager
          (Yojson.Safe.to_string payload)
      in
      check_bool "different allowed human cannot use another owner's button"
        false (interrupted base.manager);
      release_text release "gpt-test";
      let* _ = work in
      Lwt.return_unit)

let test_resume_lifecycle () =
  with_ingress (fun base calls _ _ _ run ->
      let* _ = run (message "origin") in
      let key = thread_key base.manager in
      let fresh = Session.create ~config:base.manager.config ~db:base.db () in
      let* _ =
        Daemon_resume.resume_agent_session ~session_manager:fresh
          ~config:fresh.config ~session_key:key ~channel:"slack"
          ~channel_id:"C123" ()
      in
      let processing =
        List.filter
          (fun (m, p) ->
            m = "agents.sessions.setStatus" && get "status" p = str "processing")
          !calls
      in
      Alcotest.(check int)
        "restart runs native lifecycle after persisted route recovery" 2
        (List.length processing);
      Alcotest.(check int)
        "restart gets new owned stop control" 2
        (List.length (control_values calls));
      check_bool "resume messages remain on original root" true
        (List.for_all
           (fun (m, p) ->
             m <> "chat.postMessage" || get "thread_ts" p = str "100.000001")
           !calls);
      Lwt.return_unit)

let test_mention_message_dedup () =
  with_ingress (fun _ _ _ seen _ run ->
      let* _ = run (message ~kind:"app_mention" "same message") in
      let* _ = run ~id:"other-subscription" (message "same message") in
      Alcotest.(check int)
        "mention and message subscriptions execute once" 1 (List.length !seen);
      Lwt.return_unit)

let test_profile_precedence () =
  with_ingress (fun base _ _ _ _ run ->
      let key = "slack:C123:thread:T123:A123:100.000001" in
      let parsed =
        Config_loader.parse_config ~resolve_secrets:false
          (obj
             [
               ( "room_profiles",
                 arr
                   [
                     obj
                       [ ("id", str "parent"); ("model", str "openai:parent") ];
                     obj [ ("id", str "exact"); ("model", str "openai:exact") ];
                   ] );
               ( "room_profile_bindings",
                 arr
                   [
                     obj
                       [
                         ("profile_id", str "parent");
                         ("room", str "C123");
                         ("active", `Bool true);
                       ];
                     obj
                       [
                         ("profile_id", str "exact");
                         ("room", str key);
                         ("active", `Bool true);
                       ];
                   ] );
             ])
      in
      let cfg =
        {
          base.manager.config with
          room_profiles = parsed.room_profiles;
          room_profile_bindings = parsed.room_profile_bindings;
        }
      in
      Session.update_config base.manager cfg;
      let* _ = run (message "exact") in
      let agent, _, _ = Hashtbl.find base.manager.sessions key in
      Alcotest.(check string)
        "existing exact binding precedes added room fallback" "openai:exact"
        agent.config.agent_defaults.primary_model;
      Lwt.return_unit)

let test_bound_tool_denial () =
  with_ingress (fun base _ _ _ provider run ->
      let invoked = ref 0 and step = ref 0 and denied = ref false in
      let registry = Option.get base.manager.tool_registry in
      Tool_registry.register registry
        {
          Tool.name = "blocked_tool";
          description = "Must never execute in this room";
          parameters_schema =
            obj [ ("type", str "object"); ("properties", obj []) ];
          invoke =
            (fun ?context:_ _ ->
              incr invoked;
              Lwt.return "should never run");
          invoke_stream = None;
          risk_level = Tool.Low;
          deferred = false;
        };
      let parsed =
        Config_loader.parse_config ~resolve_secrets:false
          (obj
             [
               ( "room_profiles",
                 arr
                   [
                     obj
                       [
                         ("id", str "bound");
                         ("model", str "openai:gpt-test");
                         ("denied_tools", arr [ str "blocked_tool" ]);
                       ];
                   ] );
               ( "room_profile_bindings",
                 arr
                   [
                     obj
                       [
                         ("profile_id", str "bound");
                         ("room", str "C123");
                         ("active", `Bool true);
                       ];
                   ] );
             ])
      in
      let cfg =
        {
          base.manager.config with
          room_profiles = parsed.room_profiles;
          room_profile_bindings = parsed.room_profile_bindings;
        }
      in
      Session.update_config base.manager cfg;
      (provider :=
         fun ~model ~messages ->
           incr step;
           if !step = 1 then
             Lwt.return
               (Provider.ToolCalls
                  {
                    calls =
                      [
                        {
                          Provider.id = "blocked-call";
                          function_name = "blocked_tool";
                          arguments = "{}";
                        };
                      ];
                    model;
                    usage = None;
                    provider_response_items_json = None;
                    thinking = None;
                  })
           else (
             denied :=
               List.exists
                 (fun (m : Provider.message) ->
                   m.role = "tool" && m.content <> "should never run")
                 messages;
             Lwt.return
               (Provider.Text
                  {
                    content = "done";
                    model;
                    usage = None;
                    provider_response_items_json = None;
                    thinking = None;
                  })));
      let* _ = run (message "attempt denied tool") in
      Alcotest.(check int)
        "actual production tool invocation blocked" 0 !invoked;
      check_bool "model receives denial result" true !denied;
      Lwt.return_unit)

let test_mismatched_delivery_room () =
  with_ingress (fun base calls _ _ _ run ->
      let* _ = run (message "origin") in
      let key = thread_key base.manager in
      calls := [];
      let* result =
        Daemon_resume.dispatch_resumed_message ~config:base.manager.config
          ~session_key:key ~channel:"slack" ~channel_id:"COTHER"
          ~text:"must not escape thread" ()
      in
      check_bool "mismatched persisted room never falls back to another channel"
        true
        ((match result with Error _ -> true | _ -> false) && !calls = []);
      Lwt.return_unit)

let suite =
  [
    ( "queued stop during asynchronous preparation",
      `Quick,
      test_queued_stop_during_preparation );
    ( "persisted thread rejects mismatched room",
      `Quick,
      test_mismatched_delivery_room );
    ("bound profile production tool denial", `Quick, test_bound_tool_denial);
    ("dual mention/message delivery dedup", `Quick, test_mention_message_dedup);
    ( "exact thread binding precedes room fallback",
      `Quick,
      test_profile_precedence );
    ( "restart native lifecycle from persisted owner",
      `Quick,
      test_resume_lifecycle );
    ("actual room egress before transport", `Quick, test_room_egress);
    ("button clicking human is run owner", `Quick, test_stop_owner);
    ( "unknown privacy metadata fails closed",
      `Quick,
      test_metadata ~shared:false );
    ("shared metadata room policy", `Quick, test_metadata ~shared:true);
    ("delayed old cleanup before new generation", `Quick, test_delayed_cleanup);
    ("bound room profile actual turns and reload", `Quick, test_profile);
    ("paginated chronological deduplicated history", `Quick, test_history_pages);
    ("own assistant versus untrusted other bots", `Quick, test_context_ownership);
    ("limited empty history coverage", `Quick, test_partial_history);
    ("current agent_view bootstrap cannot grant access", `Quick, test_bootstrap);
    ("membership before history", `Quick, test_denial ~membership:true);
    ("runtime allowlist refresh", `Quick, test_denial ~membership:false);
    ("provider failure terminal cleanup", `Quick, test_failure_cleanup);
    ("feature_disabled public thread fallback", `Quick, test_feature_disabled);
    ("failed notifier persisted fallback", `Quick, test_notifier_failure);
    ("signed HTTP event and form interaction", `Quick, test_http);
    ( "Socket Mode event and interactive envelopes",
      `Quick,
      test_socket ~empty_secret:false );
    ("restart background completion route", `Quick, test_background);
  ]

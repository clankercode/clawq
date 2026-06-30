let make_provider_config base_url : Runtime_config.provider_config =
  {
    Runtime_config.default_provider_config with
    base_url = Some base_url;
    default_model = Some "fake-model";
  }

let make_runtime_config base_url =
  let default = Runtime_config.default in
  {
    default with
    default_provider = Some "fake";
    providers = [ ("fake", make_provider_config base_url) ];
    prompt = { default.prompt with dynamic_enabled = false };
    security = { default.security with tools_enabled = true };
    agent_defaults =
      {
        default.agent_defaults with
        primary_model = "fake-model";
        show_tool_calls = true;
        show_thinking = false;
        tool_status_mode = "individual";
      };
  }

let make_tool_registry () =
  let registry = Tool_registry.create () in
  Tool_registry.register registry
    {
      Tool.name = "test_tool";
      description = "A test tool";
      parameters_schema = `Assoc [];
      invoke =
        (fun ?context:_ args ->
          let open Yojson.Safe.Util in
          let value = try args |> member "value" |> to_string with _ -> "" in
          Lwt.return ("ran " ^ value));
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    };
  registry

let with_fake_streaming_provider f =
  let port = Test_helpers.free_port () in
  let request_count = ref 0 in
  let callback _conn req _body =
    request_count := !request_count + 1;
    match (Cohttp.Request.meth req, Uri.path (Cohttp.Request.uri req)) with
    | `POST, "/chat/completions" ->
        let stream, push = Lwt_stream.create () in
        let body =
          if !request_count = 1 then begin
            push
              (Some
                 "data: \
                  {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call_1\",\"function\":{\"name\":\"test_tool\",\"arguments\":\"{\\\"value\\\":\\\"hi\\\"}\"}}]}}]}\n\n");
            push (Some "data: [DONE]\n\n")
          end
          else begin
            push
              (Some
                 "data: {\"choices\":[{\"delta\":{\"content\":\"final \
                  answer\"}}]}\n\n");
            push (Some "data: [DONE]\n\n")
          end;
          push None;
          Cohttp_lwt.Body.of_stream stream
        in
        let headers =
          Cohttp.Header.of_list [ ("Content-Type", "text/event-stream") ]
        in
        Cohttp_lwt_unix.Server.respond ~status:`OK ~headers ~body ()
    | _ -> Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"" ()
  in
  let stop, stopper = Lwt.wait () in
  let server =
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  Lwt.async (fun () -> Lwt.pick [ server; stop ]);
  Fun.protect
    ~finally:(fun () -> Lwt.wakeup_later stopper ())
    (fun () -> f (Printf.sprintf "http://127.0.0.1:%d" port))

let test_slack_handle_event_emits_tool_call_notifications () =
  with_fake_streaming_provider (fun base_url ->
      let slack_config : Runtime_config.slack_config =
        {
          bot_token = "xoxb-test";
          signing_secret = "secret";
          events_path = "/slack/events";
          allow_channels = [ "*" ];
          allow_users = [ "*" ];
          allow_private_channels = [];
          private_channel_policy = Runtime_config.Pc_deny;
          app_token = "";
          socket_mode = false;
          default_model = None;
        }
      in
      let session_manager =
        Session.create
          ~config:(make_runtime_config base_url)
          ~tool_registry:(make_tool_registry ()) ()
      in
      let sent = ref [] in
      let body =
        {|{"type":"event_callback","event":{"type":"message","channel":"C123","user":"U456","text":"hello","ts":"1234567890.123456"}}|}
      in
      let result =
        Lwt_main.run
          (Slack.handle_event ~config:slack_config ~session_manager
             ~send_message_fn:(fun ~bot_token:_ ~channel_id:_ ~text ->
               sent := text :: !sent;
               Lwt.return_unit)
             body)
      in
      Alcotest.(check string) "returns ok" "ok" result;
      Alcotest.(check (list string))
        "tool notifications and final response"
        [
          "\xF0\x9F\x94\xA7 *test_tool* \xE2\x9C\x93 \xE2\x86\x92 _ran hi_";
          "final answer";
        ]
        (List.rev !sent))

let test_discord_handle_message_emits_tool_call_notifications () =
  with_fake_streaming_provider (fun base_url ->
      let discord_config : Runtime_config.discord_config =
        {
          bot_token = "discord-token";
          allow_guilds = [ "*" ];
          allow_users = [ "*" ];
          intents = 513;
          default_model = None;
        }
      in
      let session_mgr =
        Session.create
          ~config:(make_runtime_config base_url)
          ~tool_registry:(make_tool_registry ()) ()
      in
      let sent = ref [] in
      let msg : Discord.message =
        {
          id = "msg123";
          channel_id = "ch1";
          guild_id = None;
          author_id = "u1";
          author_bot = false;
          content = "hello";
          mention_ids = [];
          attachments = [];
        }
      in
      Lwt_main.run
        (Discord.handle_message ~discord_config ~session_mgr
           ~send_message_fn:(fun ~bot_token:_ ~channel_id:_ ~text ->
             sent := text :: !sent;
             Lwt.return_unit)
           msg);
      Alcotest.(check (list string))
        "tool notifications and final response"
        [
          "\xF0\x9F\x94\xA7 *test_tool* \xE2\x9C\x93 \xE2\x86\x92 _ran hi_";
          "final answer";
        ]
        (List.rev !sent))

let suite =
  [
    Alcotest.test_case "slack emits tool call notifications" `Quick
      test_slack_handle_event_emits_tool_call_notifications;
    Alcotest.test_case "discord emits tool call notifications" `Quick
      test_discord_handle_message_emits_tool_call_notifications;
  ]

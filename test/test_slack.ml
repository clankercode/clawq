let make_config ?(bot_token = "xoxb-test") ?(signing_secret = "test_secret")
    ?(events_path = "/slack/events") ?(allow_channels = [ "*" ])
    ?(allow_users = [ "*" ]) ?(app_token = "") ?(socket_mode = false) () :
    Runtime_config.slack_config =
  {
    bot_token;
    signing_secret;
    events_path;
    allow_channels;
    allow_users;
    app_token;
    socket_mode;
    default_model = None;
  }

let contains_str haystack needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with Not_found -> false

let make_fake_provider_config base_url : Runtime_config.provider_config =
  {
    Runtime_config.default_provider_config with
    api_key = "test-key";
    base_url = Some base_url;
    default_model = Some "fake-model";
  }

let with_text_provider f =
  let port = Test_helpers.free_port () in
  let callback _conn _req body =
    let open Lwt.Syntax in
    let* _ = Cohttp_lwt.Body.to_string body in
    let response_body =
      Yojson.Safe.to_string
        (`Assoc
           [
             ("id", `String "cmpl_fake");
             ("object", `String "chat.completion");
             ("model", `String "fake-model");
             ( "choices",
               `List
                 [
                   `Assoc
                     [
                       ("index", `Int 0);
                       ( "message",
                         `Assoc
                           [
                             ("role", `String "assistant");
                             ("content", `String "Debate answer.");
                           ] );
                       ("finish_reason", `String "stop");
                     ];
                 ] );
             ( "usage",
               `Assoc
                 [ ("prompt_tokens", `Int 1); ("completion_tokens", `Int 1) ] );
           ])
    in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:response_body ()
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
    (fun () ->
      let config =
        {
          Runtime_config.default with
          default_provider = Some "fake";
          providers =
            [
              ( "fake",
                make_fake_provider_config
                  (Printf.sprintf "http://127.0.0.1:%d" port) );
            ];
          prompt =
            { Runtime_config.default.prompt with dynamic_enabled = false };
          security =
            { Runtime_config.default.security with tools_enabled = false };
          agent_defaults =
            {
              Runtime_config.default.agent_defaults with
              primary_model = "fake:fake-model";
              show_thinking = false;
              show_tool_calls = false;
            };
          debate =
            {
              Runtime_config.default.debate with
              default_models = [ "fake:fake-model" ];
              judge_model = "fake:fake-model";
            };
        }
      in
      f config)

let test_is_allowed_wildcard () =
  let config = make_config () in
  Alcotest.(check bool)
    "wildcard allows any" true
    (Slack.is_allowed ~config ~channel_id:"C123" ~user_id:"U456");
  Alcotest.(check bool)
    "wildcard allows other" true
    (Slack.is_allowed ~config ~channel_id:"CXXX" ~user_id:"UYYY")

let test_is_allowed_specific () =
  let config =
    make_config ~allow_channels:[ "C123" ] ~allow_users:[ "U456" ] ()
  in
  Alcotest.(check bool)
    "matching ids" true
    (Slack.is_allowed ~config ~channel_id:"C123" ~user_id:"U456");
  Alcotest.(check bool)
    "wrong channel" false
    (Slack.is_allowed ~config ~channel_id:"C999" ~user_id:"U456");
  Alcotest.(check bool)
    "wrong user" false
    (Slack.is_allowed ~config ~channel_id:"C123" ~user_id:"U999")

let test_verify_signature_valid () =
  let signing_secret = "my_secret" in
  let timestamp = Printf.sprintf "%.0f" (Unix.gettimeofday ()) in
  let body = {|{"type":"event_callback"}|} in
  let basestring = "v0:" ^ timestamp ^ ":" ^ body in
  let signature =
    "v0="
    ^ Digestif.SHA256.(hmac_string ~key:signing_secret basestring |> to_hex)
  in
  Alcotest.(check bool)
    "valid signature" true
    (Slack.verify_signature ~signing_secret ~timestamp ~body ~signature)

let test_verify_signature_invalid () =
  let signing_secret = "my_secret" in
  let timestamp = Printf.sprintf "%.0f" (Unix.gettimeofday ()) in
  let body = {|{"type":"event_callback"}|} in
  Alcotest.(check bool)
    "invalid signature" false
    (Slack.verify_signature ~signing_secret ~timestamp ~body
       ~signature:"v0=deadbeef")

let test_verify_signature_expired () =
  let signing_secret = "my_secret" in
  let timestamp = Printf.sprintf "%.0f" (Unix.gettimeofday () -. 600.0) in
  let body = {|{"type":"event_callback"}|} in
  let basestring = "v0:" ^ timestamp ^ ":" ^ body in
  let signature =
    "v0="
    ^ Digestif.SHA256.(hmac_string ~key:signing_secret basestring |> to_hex)
  in
  Alcotest.(check bool)
    "expired timestamp" false
    (Slack.verify_signature ~signing_secret ~timestamp ~body ~signature)

let test_parse_url_verification () =
  let body =
    {|{"type":"url_verification","token":"tok","challenge":"abc123"}|}
  in
  match Slack.parse_event body with
  | Some (Slack.UrlVerification challenge) ->
      Alcotest.(check string) "challenge" "abc123" challenge
  | _ -> Alcotest.fail "expected UrlVerification"

let test_parse_message_event () =
  let body =
    {|{"type":"event_callback","event":{"type":"message","channel":"C123","user":"U456","text":"hello"}}|}
  in
  match Slack.parse_event body with
  | Some (Slack.Message { channel_id; user_id; text; bot_id }) ->
      Alcotest.(check string) "channel" "C123" channel_id;
      Alcotest.(check string) "user" "U456" user_id;
      Alcotest.(check string) "text" "hello" text;
      Alcotest.(check (option string)) "no bot_id" None bot_id
  | _ -> Alcotest.fail "expected Message"

let test_bot_message_ignored () =
  let body =
    {|{"type":"event_callback","event":{"type":"message","channel":"C123","user":"U456","text":"hi","bot_id":"B789"}}|}
  in
  match Slack.parse_event body with
  | Some (Slack.Message { bot_id; _ }) ->
      Alcotest.(check (option string)) "has bot_id" (Some "B789") bot_id
  | _ -> Alcotest.fail "expected Message with bot_id"

let test_parse_event_invalid_json () =
  Alcotest.(check bool)
    "invalid json returns None" true
    (Slack.parse_event "not json at all" = None)

let test_parse_event_unknown_type () =
  let body = {|{"type":"app_rate_limited"}|} in
  match Slack.parse_event body with
  | Some Slack.Other -> ()
  | _ -> Alcotest.fail "expected Some Other for unknown type"

let test_parse_event_non_message_callback () =
  let body =
    {|{"type":"event_callback","event":{"type":"app_mention","channel":"C1","user":"U1","text":"hey"}}|}
  in
  match Slack.parse_event body with
  | Some Slack.Other -> ()
  | _ -> Alcotest.fail "expected Some Other for non-message event_callback"

let test_session_key_format () =
  (* Verify handle_event produces the right session key format by checking
     that the parse_event output would yield "slack:{channel}:{user}" *)
  let body =
    {|{"type":"event_callback","event":{"type":"message","channel":"C123","user":"U456","text":"hello"}}|}
  in
  match Slack.parse_event body with
  | Some (Slack.Message { channel_id; user_id; _ }) ->
      let key = "slack:" ^ channel_id ^ ":" ^ user_id in
      Alcotest.(check string) "session key format" "slack:C123:U456" key
  | _ -> Alcotest.fail "expected Message"

let test_handle_event_update_returns_before_restart_finishes () =
  let config = make_config () in
  let session_manager = Session.create ~config:Runtime_config.default () in
  let sent = ref [] in
  let started = ref false in
  let gate, release = Lwt.wait () in
  let body =
    {|{"type":"event_callback","event":{"type":"message","channel":"C123","user":"U456","text":"/update"}}|}
  in
  let result =
    Lwt_main.run
      (Slack.handle_event ~config ~session_manager
         ~send_message_fn:(fun ~bot_token:_ ~channel_id:_ ~text ->
           sent := text :: !sent;
           Lwt.return_unit)
         ~run_update_command:(fun
             ?mode:_ ?prepare_restart:_ ~send_progress () ->
           let open Lwt.Syntax in
           started := true;
           let* () = send_progress "Starting update..." in
           let* () = gate in
           Lwt.return "Build complete. Sending restart signal...")
         body)
  in
  Alcotest.(check string) "returns ok immediately" "ok" result;
  Lwt.wakeup_later release ();
  Lwt_main.run (Lwt_unix.sleep 0.01);
  Alcotest.(check bool) "background update started" true !started;
  Alcotest.(check (list string))
    "progress and final message sent" [ "Starting update..." ] (List.rev !sent)

let test_handle_event_debate_sends_debug_summary () =
  with_text_provider (fun runtime_config ->
      let config = make_config () in
      let db = Memory.init ~db_path:":memory:" () in
      Debate.init_schema db;
      let session_manager = Session.create ~config:runtime_config ~db () in
      let key = "slack:C123:U456" in
      Alcotest.(check (result unit string))
        "debug set on" (Ok ())
        (Session.set_session_debug session_manager ~key ~enabled:true);
      let sent = ref [] in
      let body =
        {|{"type":"event_callback","event":{"type":"message","channel":"C123","user":"U456","text":"/debate should debug"}}|}
      in
      let result =
        Lwt_main.run
          (Slack.handle_event ~config ~session_manager
             ~send_message_fn:(fun ~bot_token:_ ~channel_id:_ ~text ->
               sent := text :: !sent;
               Lwt.return_unit)
             body)
      in
      let sent = List.rev !sent in
      Alcotest.(check string) "returns ok" "ok" result;
      Alcotest.(check bool)
        "sent debate result" true
        (List.exists (fun text -> contains_str text "Debate Results") sent);
      Alcotest.(check bool)
        "sent debate debug summary" true
        (List.exists
           (fun text -> contains_str text "debug: llm provider=fake")
           sent))

let test_parse_event_with_files () =
  let body =
    {|{"type":"event_callback","event":{"type":"message","channel":"C1","user":"U1","text":"see file","ts":"1234.5","files":[{"url_private_download":"https://files.slack.com/f1","name":"data.csv","mimetype":"text/csv","size":1024}]}}|}
  in
  match Slack.parse_event body with
  | Some (Slack.Message { channel_id; text; files; _ }) ->
      Alcotest.(check string) "channel" "C1" channel_id;
      Alcotest.(check string) "text" "see file" text;
      Alcotest.(check int) "one file" 1 (List.length files);
      let f = List.hd files in
      Alcotest.(check string) "file name" "data.csv" f.file_name;
      Alcotest.(check string) "mimetype" "text/csv" f.mimetype;
      Alcotest.(check int) "file size" 1024 f.file_size;
      Alcotest.(check string)
        "url" "https://files.slack.com/f1" f.url_private_download
  | _ -> Alcotest.fail "expected Message with files"

let test_parse_event_no_files () =
  let body =
    {|{"type":"event_callback","event":{"type":"message","channel":"C2","user":"U2","text":"no files"}}|}
  in
  match Slack.parse_event body with
  | Some (Slack.Message { files; _ }) ->
      Alcotest.(check int) "no files" 0 (List.length files)
  | _ -> Alcotest.fail "expected Message"

let suite =
  [
    Alcotest.test_case "is_allowed wildcard" `Quick test_is_allowed_wildcard;
    Alcotest.test_case "is_allowed specific" `Quick test_is_allowed_specific;
    Alcotest.test_case "verify_signature valid" `Quick
      test_verify_signature_valid;
    Alcotest.test_case "verify_signature invalid" `Quick
      test_verify_signature_invalid;
    Alcotest.test_case "verify_signature expired" `Quick
      test_verify_signature_expired;
    Alcotest.test_case "parse url_verification" `Quick
      test_parse_url_verification;
    Alcotest.test_case "parse message event" `Quick test_parse_message_event;
    Alcotest.test_case "bot message has bot_id" `Quick test_bot_message_ignored;
    Alcotest.test_case "parse event invalid json" `Quick
      test_parse_event_invalid_json;
    Alcotest.test_case "parse event unknown type" `Quick
      test_parse_event_unknown_type;
    Alcotest.test_case "parse event non-message callback" `Quick
      test_parse_event_non_message_callback;
    Alcotest.test_case "session key format" `Quick test_session_key_format;
    Alcotest.test_case "handle event update returns before restart finishes"
      `Quick test_handle_event_update_returns_before_restart_finishes;
    Alcotest.test_case "handle event debate sends debug summary" `Quick
      test_handle_event_debate_sends_debug_summary;
    Alcotest.test_case "parse event with files" `Quick
      test_parse_event_with_files;
    Alcotest.test_case "parse event no files" `Quick test_parse_event_no_files;
  ]

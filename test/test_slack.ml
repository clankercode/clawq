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
  }

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
         ~run_update_command:(fun ?prepare_restart:_ ~send_progress () ->
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
    "progress and final message sent"
    [ "Starting update..."; "Build complete. Sending restart signal..." ]
    (List.rev !sent)

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
  ]

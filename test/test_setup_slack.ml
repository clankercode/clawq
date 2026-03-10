(* test_setup_slack.ml — Unit tests for Setup_slack pure functions *)

let validate_bot_token_valid () =
  Alcotest.(check (result string string))
    "valid xoxb" (Ok "xoxb-123-456-abc")
    (Setup_slack.validate_bot_token "xoxb-123-456-abc")

let validate_bot_token_spaces () =
  Alcotest.(check (result string string))
    "spaces trimmed" (Ok "xoxb-trimmed")
    (Setup_slack.validate_bot_token "  xoxb-trimmed  ")

let validate_bot_token_empty () =
  match Setup_slack.validate_bot_token "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty token"

let validate_bot_token_wrong_prefix () =
  match Setup_slack.validate_bot_token "xoxp-user-token" with
  | Error e ->
      Alcotest.(check bool)
        "mentions xoxb" true
        (try
           ignore (Str.search_forward (Str.regexp_string "xoxb-") e 0);
           true
         with Not_found -> false)
  | Ok _ -> Alcotest.fail "expected error for wrong prefix"

let validate_signing_secret_valid () =
  Alcotest.(check (result string string))
    "valid secret" (Ok "abc123secret")
    (Setup_slack.validate_signing_secret "abc123secret")

let validate_signing_secret_empty () =
  match Setup_slack.validate_signing_secret "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty secret"

let validate_app_token_valid () =
  Alcotest.(check (result string string))
    "valid xapp" (Ok "xapp-1-abc")
    (Setup_slack.validate_app_token "xapp-1-abc")

let validate_app_token_empty_ok () =
  Alcotest.(check (result string string))
    "empty is ok" (Ok "")
    (Setup_slack.validate_app_token "")

let validate_app_token_wrong_prefix () =
  match Setup_slack.validate_app_token "bad-token" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for wrong prefix"

let build_json_roundtrip () =
  let json =
    Setup_slack.build_slack_json ~bot_token:"xoxb-test-token"
      ~signing_secret:"signing123" ~events_path:"/slack/events"
      ~allow_channels:[ "*" ] ~allow_users:[ "*" ] ~app_token:"xapp-test-app"
      ~socket_mode:true
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.slack with
  | Some s ->
      Alcotest.(check string) "bot_token" "xoxb-test-token" s.bot_token;
      Alcotest.(check string) "signing_secret" "signing123" s.signing_secret;
      Alcotest.(check string) "events_path" "/slack/events" s.events_path;
      Alcotest.(check (list string)) "allow_channels" [ "*" ] s.allow_channels;
      Alcotest.(check (list string)) "allow_users" [ "*" ] s.allow_users;
      Alcotest.(check string) "app_token" "xapp-test-app" s.app_token;
      Alcotest.(check bool) "socket_mode" true s.socket_mode
  | None -> Alcotest.fail "expected slack config"

let build_json_custom_filters () =
  let json =
    Setup_slack.build_slack_json ~bot_token:"xoxb-x" ~signing_secret:"sec"
      ~events_path:"/custom/path" ~allow_channels:[ "general"; "dev" ]
      ~allow_users:[ "alice"; "bob" ] ~app_token:"" ~socket_mode:false
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.slack with
  | Some s ->
      Alcotest.(check string) "events_path" "/custom/path" s.events_path;
      Alcotest.(check (list string))
        "allow_channels" [ "general"; "dev" ] s.allow_channels;
      Alcotest.(check (list string))
        "allow_users" [ "alice"; "bob" ] s.allow_users;
      Alcotest.(check bool) "socket_mode" false s.socket_mode
  | None -> Alcotest.fail "expected slack config"

let instructions_socket_mode () =
  let s =
    Setup_slack.post_setup_instructions ~events_path:"/slack/events"
      ~socket_mode:true ~gateway_port:13451 ~tunnel_url:None
  in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has api.slack.com" true
    (contains "https://api.slack.com/apps");
  Alcotest.(check bool) "has Socket Mode" true (contains "Socket Mode");
  Alcotest.(check bool) "has xapp" true (contains "xapp-");
  Alcotest.(check bool) "has chat:write scope" true (contains "chat:write")

let instructions_events_api () =
  let s =
    Setup_slack.post_setup_instructions ~events_path:"/slack/events"
      ~socket_mode:false ~gateway_port:13451 ~tunnel_url:None
  in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has events url" true
    (contains "http://localhost:13451/slack/events");
  Alcotest.(check bool) "has Signing Secret" true (contains "Signing Secret");
  Alcotest.(check bool) "has tunnel note" true (contains "set up a tunnel")

let instructions_with_tunnel () =
  let s =
    Setup_slack.post_setup_instructions ~events_path:"/slack/events"
      ~socket_mode:false ~gateway_port:13451
      ~tunnel_url:(Some "https://my.tunnel.example.com")
  in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has tunnel url" true
    (contains "https://my.tunnel.example.com/slack/events");
  Alcotest.(check bool) "no tunnel note" false (contains "set up a tunnel")

let deep_merge_preserves_existing () =
  let existing =
    Yojson.Safe.from_string
      {|{"channels":{"telegram":{"accounts":{"default":{"bot_token":"tok"}}}},"default_temperature":0.7}|}
  in
  let overlay =
    Setup_slack.build_slack_json ~bot_token:"xoxb-x" ~signing_secret:"sec"
      ~events_path:"/slack/events" ~allow_channels:[ "*" ] ~allow_users:[ "*" ]
      ~app_token:"" ~socket_mode:true
  in
  let result = Setup_common.deep_merge_json existing overlay in
  let config = Config_loader.parse_config ~resolve_secrets:false result in
  (match config.channels.slack with
  | Some _ -> ()
  | None -> Alcotest.fail "expected slack config after merge");
  match config.channels.telegram with
  | Some _ -> ()
  | None -> Alcotest.fail "telegram should be preserved after merge"

let suite =
  [
    Alcotest.test_case "validate_bot_token valid" `Quick
      validate_bot_token_valid;
    Alcotest.test_case "validate_bot_token spaces" `Quick
      validate_bot_token_spaces;
    Alcotest.test_case "validate_bot_token empty" `Quick
      validate_bot_token_empty;
    Alcotest.test_case "validate_bot_token wrong prefix" `Quick
      validate_bot_token_wrong_prefix;
    Alcotest.test_case "validate_signing_secret valid" `Quick
      validate_signing_secret_valid;
    Alcotest.test_case "validate_signing_secret empty" `Quick
      validate_signing_secret_empty;
    Alcotest.test_case "validate_app_token valid" `Quick
      validate_app_token_valid;
    Alcotest.test_case "validate_app_token empty ok" `Quick
      validate_app_token_empty_ok;
    Alcotest.test_case "validate_app_token wrong prefix" `Quick
      validate_app_token_wrong_prefix;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json custom filters" `Quick
      build_json_custom_filters;
    Alcotest.test_case "instructions socket mode" `Quick
      instructions_socket_mode;
    Alcotest.test_case "instructions events API" `Quick instructions_events_api;
    Alcotest.test_case "instructions with tunnel" `Quick
      instructions_with_tunnel;
    Alcotest.test_case "deep merge preserves existing" `Quick
      deep_merge_preserves_existing;
  ]

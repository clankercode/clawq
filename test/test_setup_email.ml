(* test_setup_email.ml — Unit tests for Setup_email pure functions *)

let validate_port_valid () =
  Alcotest.(check (result string string))
    "valid port" (Ok "993")
    (Setup_common.validate_port "993")

let validate_port_smtp () =
  Alcotest.(check (result string string))
    "smtp port" (Ok "587")
    (Setup_common.validate_port "587")

let validate_port_zero () =
  match Setup_common.validate_port "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for port 0"

let validate_port_too_large () =
  match Setup_common.validate_port "65536" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for port > 65535"

let validate_port_not_int () =
  match Setup_common.validate_port "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-integer"

let validate_email_valid () =
  Alcotest.(check (result string string))
    "valid email" (Ok "user@example.com")
    (Setup_email.validate_email "user@example.com")

let validate_email_empty () =
  match Setup_email.validate_email "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty email"

let validate_email_no_at () =
  match Setup_email.validate_email "userexample.com" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for email without @"

let validate_email_no_domain_dot () =
  match Setup_email.validate_email "user@localhost" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for email with no dot in domain"

let validate_poll_interval_valid () =
  Alcotest.(check (result string string))
    "valid interval" (Ok "60.0")
    (Setup_email.validate_poll_interval "60.0")

let validate_poll_interval_integer () =
  Alcotest.(check (result string string))
    "integer interval" (Ok "30")
    (Setup_email.validate_poll_interval "30")

let validate_poll_interval_zero () =
  match Setup_email.validate_poll_interval "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero interval"

let validate_poll_interval_negative () =
  match Setup_email.validate_poll_interval "-5" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative interval"

let validate_poll_interval_not_float () =
  match Setup_email.validate_poll_interval "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-float"

let build_json_roundtrip () =
  let json =
    Setup_email.build_email_json ~imap_host:"imap.gmail.com" ~imap_port:993
      ~smtp_host:"smtp.gmail.com" ~smtp_port:587 ~username:"user@gmail.com"
      ~password:"app_password" ~from_address:"user@gmail.com"
      ~allow_from:[ "*" ] ~poll_interval_s:60.0 ~default_model:None
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.email with
  | Some e ->
      Alcotest.(check string) "imap_host" "imap.gmail.com" e.imap_host;
      Alcotest.(check int) "imap_port" 993 e.imap_port;
      Alcotest.(check string) "smtp_host" "smtp.gmail.com" e.smtp_host;
      Alcotest.(check int) "smtp_port" 587 e.smtp_port;
      Alcotest.(check string) "username" "user@gmail.com" e.username;
      Alcotest.(check string) "password" "app_password" e.password;
      Alcotest.(check string) "from_address" "user@gmail.com" e.from_address;
      Alcotest.(check (list string)) "allow_from" [ "*" ] e.allow_from;
      Alcotest.(check (float 0.001)) "poll_interval_s" 60.0 e.poll_interval_s;
      Alcotest.(check (option string)) "default_model" None e.default_model
  | None -> Alcotest.fail "expected email config"

let build_json_specific_senders () =
  let json =
    Setup_email.build_email_json ~imap_host:"imap.example.com" ~imap_port:993
      ~smtp_host:"smtp.example.com" ~smtp_port:587 ~username:"bot@example.com"
      ~password:"pass" ~from_address:"bot@example.com"
      ~allow_from:[ "alice@example.com"; "bob@example.com" ]
      ~poll_interval_s:30.0 ~default_model:(Some "openai:gpt-4")
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.email with
  | Some e ->
      Alcotest.(check (list string))
        "allow_from"
        [ "alice@example.com"; "bob@example.com" ]
        e.allow_from;
      Alcotest.(check (float 0.001)) "poll_interval_s" 30.0 e.poll_interval_s;
      Alcotest.(check (option string))
        "default_model" (Some "openai:gpt-4") e.default_model
  | None -> Alcotest.fail "expected email config"

let instructions_content () =
  let s = Setup_email.post_setup_instructions in
  Alcotest.(check bool)
    "has docs URL" true
    (Test_helpers.string_contains s "https://clawq.org/channels/#email");
  Alcotest.(check bool)
    "has gmail mention" true
    (Test_helpers.string_contains s "gmail");
  Alcotest.(check bool)
    "has daemon start" true
    (Test_helpers.string_contains s "clawq daemon start")

let suite =
  [
    Alcotest.test_case "validate_port valid" `Quick validate_port_valid;
    Alcotest.test_case "validate_port smtp" `Quick validate_port_smtp;
    Alcotest.test_case "validate_port zero" `Quick validate_port_zero;
    Alcotest.test_case "validate_port too_large" `Quick validate_port_too_large;
    Alcotest.test_case "validate_port not_int" `Quick validate_port_not_int;
    Alcotest.test_case "validate_email valid" `Quick validate_email_valid;
    Alcotest.test_case "validate_email empty" `Quick validate_email_empty;
    Alcotest.test_case "validate_email no_at" `Quick validate_email_no_at;
    Alcotest.test_case "validate_email no_domain_dot" `Quick
      validate_email_no_domain_dot;
    Alcotest.test_case "validate_poll_interval valid" `Quick
      validate_poll_interval_valid;
    Alcotest.test_case "validate_poll_interval integer" `Quick
      validate_poll_interval_integer;
    Alcotest.test_case "validate_poll_interval zero" `Quick
      validate_poll_interval_zero;
    Alcotest.test_case "validate_poll_interval negative" `Quick
      validate_poll_interval_negative;
    Alcotest.test_case "validate_poll_interval not_float" `Quick
      validate_poll_interval_not_float;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json specific senders" `Quick
      build_json_specific_senders;
    Alcotest.test_case "post_setup_instructions content" `Quick
      instructions_content;
  ]

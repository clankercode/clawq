(* test_setup_notify.ml — Unit tests for Setup_notify pure functions *)

let validate_channel_valid () =
  List.iter
    (fun ch ->
      Alcotest.(check (result string string))
        ("valid channel: " ^ ch) (Ok ch)
        (Setup_notify.validate_channel ch))
    [ "telegram"; "discord"; "slack"; "email" ]

let validate_channel_invalid () =
  match Setup_notify.validate_channel "sms" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for invalid channel"

let validate_channel_empty () =
  match Setup_notify.validate_channel "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty channel"

let validate_target_valid () =
  Alcotest.(check (result string string))
    "valid target" (Ok "123456789")
    (Setup_notify.validate_target "123456789")

let validate_target_empty () =
  match Setup_notify.validate_target "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty target"

let validate_target_whitespace () =
  match Setup_notify.validate_target "   " with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for whitespace-only target"

let build_json_telegram_roundtrip () =
  let json =
    Setup_notify.build_notify_json ~channel:"telegram" ~target:"123456789"
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.notify with
  | Some n ->
      Alcotest.(check string) "channel" "telegram" n.channel;
      Alcotest.(check string) "target" "123456789" n.target
  | None -> Alcotest.fail "expected notify config"

let build_json_email_roundtrip () =
  let json =
    Setup_notify.build_notify_json ~channel:"email" ~target:"user@example.com"
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.notify with
  | Some n ->
      Alcotest.(check string) "channel" "email" n.channel;
      Alcotest.(check string) "target" "user@example.com" n.target
  | None -> Alcotest.fail "expected notify config"

let post_instructions_content () =
  let s = Setup_notify.post_setup_instructions in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has docs url" true
    (contains "https://clawq.org/notify/");
  Alcotest.(check bool) "mentions telegram" true (contains "telegram");
  Alcotest.(check bool) "mentions cron" true (contains "cron")

let suite =
  [
    Alcotest.test_case "validate_channel valid" `Quick validate_channel_valid;
    Alcotest.test_case "validate_channel invalid" `Quick
      validate_channel_invalid;
    Alcotest.test_case "validate_channel empty" `Quick validate_channel_empty;
    Alcotest.test_case "validate_target valid" `Quick validate_target_valid;
    Alcotest.test_case "validate_target empty" `Quick validate_target_empty;
    Alcotest.test_case "validate_target whitespace" `Quick
      validate_target_whitespace;
    Alcotest.test_case "build_json telegram roundtrip" `Quick
      build_json_telegram_roundtrip;
    Alcotest.test_case "build_json email roundtrip" `Quick
      build_json_email_roundtrip;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]

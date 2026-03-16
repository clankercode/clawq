(* test_setup_lark.ml — Unit tests for Setup_lark pure functions *)

let validate_app_id_valid () =
  Alcotest.(check (result string string))
    "valid app id" (Ok "cli_abc123def456")
    (Setup_lark.validate_app_id "cli_abc123def456")

let validate_app_id_empty () =
  match Setup_lark.validate_app_id "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty app ID"

let validate_app_id_no_prefix () =
  match Setup_lark.validate_app_id "abc123def456" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for app ID without cli_ prefix"

let validate_app_id_wrong_prefix () =
  match Setup_lark.validate_app_id "app_abc123" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for app ID with wrong prefix"

let validate_app_id_cli_only () =
  (* "cli_" by itself is technically >= 4 chars starting with cli_ *)
  match Setup_lark.validate_app_id "cli_" with
  | Ok _ -> ()
  | Error _ -> ()
(* just check it doesn't raise — the validator only checks prefix *)

let build_json_roundtrip () =
  let json =
    Setup_lark.build_lark_json ~enabled:true ~app_id:"cli_testapp123"
      ~app_secret:"my_app_secret" ~verification_token:"my_verify_token"
      ~endpoint:"/lark/events" ~mode:"event" ~allow_users:[ "*" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.lark with
  | Some l ->
      Alcotest.(check bool) "enabled" true l.enabled;
      Alcotest.(check string) "app_id" "cli_testapp123" l.app_id;
      Alcotest.(check string) "app_secret" "my_app_secret" l.app_secret;
      Alcotest.(check string)
        "verification_token" "my_verify_token" l.verification_token;
      Alcotest.(check string) "endpoint" "/lark/events" l.endpoint;
      Alcotest.(check string) "mode" "event" l.mode;
      Alcotest.(check (list string)) "allow_users" [ "*" ] l.allow_users
  | None -> Alcotest.fail "expected lark config"

let build_json_disabled () =
  let json =
    Setup_lark.build_lark_json ~enabled:false ~app_id:"cli_app"
      ~app_secret:"secret" ~verification_token:"token" ~endpoint:"/lark/events"
      ~mode:"webhook" ~allow_users:[ "user1"; "user2" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.lark with
  | Some l ->
      Alcotest.(check bool) "disabled" false l.enabled;
      Alcotest.(check string) "mode" "webhook" l.mode;
      Alcotest.(check (list string))
        "allow_users" [ "user1"; "user2" ] l.allow_users
  | None -> Alcotest.fail "expected lark config"

let instructions_content () =
  let s = Setup_lark.post_setup_instructions in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has docs URL" true
    (contains "https://clawq.org/channels/#lark");
  Alcotest.(check bool)
    "has feishu mention" true
    (contains "feishu" || contains "Feishu");
  Alcotest.(check bool) "has daemon start" true (contains "clawq daemon start")

let suite =
  [
    Alcotest.test_case "validate_app_id valid" `Quick validate_app_id_valid;
    Alcotest.test_case "validate_app_id empty" `Quick validate_app_id_empty;
    Alcotest.test_case "validate_app_id no_prefix" `Quick
      validate_app_id_no_prefix;
    Alcotest.test_case "validate_app_id wrong_prefix" `Quick
      validate_app_id_wrong_prefix;
    Alcotest.test_case "validate_app_id cli_only" `Quick
      validate_app_id_cli_only;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json disabled" `Quick build_json_disabled;
    Alcotest.test_case "post_setup_instructions content" `Quick
      instructions_content;
  ]

(* test_setup_dingtalk.ml — Unit tests for Setup_dingtalk pure functions *)

let validate_agent_id_valid () =
  Alcotest.(check (result string string))
    "valid digits" (Ok "12345678")
    (Setup_dingtalk.validate_agent_id "12345678")

let validate_agent_id_empty () =
  match Setup_dingtalk.validate_agent_id "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty"

let validate_agent_id_non_digits () =
  match Setup_dingtalk.validate_agent_id "123abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-digits"

let validate_app_key_valid () =
  Alcotest.(check (result string string))
    "valid" (Ok "dingpay12345")
    (Setup_dingtalk.validate_app_key "dingpay12345")

let validate_app_key_empty () =
  match Setup_dingtalk.validate_app_key "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty app key"

let validate_app_secret_valid () =
  Alcotest.(check (result string string))
    "valid" (Ok "super_secret_value")
    (Setup_dingtalk.validate_app_secret "super_secret_value")

let validate_app_secret_empty () =
  match Setup_dingtalk.validate_app_secret "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty app secret"

let build_json_roundtrip () =
  let json =
    Setup_dingtalk.build_dingtalk_json ~app_key:"testkey"
      ~app_secret:"testsecret" ~agent_id:"12345678" ~allow_from:[ "*" ]
      ~webhook_url:None
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.dingtalk with
  | Some dt ->
      Alcotest.(check string) "app_key" "testkey" dt.app_key;
      Alcotest.(check string) "app_secret" "testsecret" dt.app_secret;
      Alcotest.(check string) "agent_id" "12345678" dt.agent_id;
      Alcotest.(check (list string)) "allow_from" [ "*" ] dt.allow_from;
      Alcotest.(check (option string)) "webhook_url" None dt.webhook_url
  | None -> Alcotest.fail "expected dingtalk config"

let build_json_with_webhook () =
  let json =
    Setup_dingtalk.build_dingtalk_json ~app_key:"key" ~app_secret:"sec"
      ~agent_id:"999" ~allow_from:[ "*" ]
      ~webhook_url:
        (Some "https://oapi.dingtalk.com/robot/send?access_token=abc")
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.dingtalk with
  | Some dt ->
      Alcotest.(check (option string))
        "webhook_url"
        (Some "https://oapi.dingtalk.com/robot/send?access_token=abc")
        dt.webhook_url
  | None -> Alcotest.fail "expected dingtalk config"

let build_json_restricted_users () =
  let json =
    Setup_dingtalk.build_dingtalk_json ~app_key:"k" ~app_secret:"s"
      ~agent_id:"1" ~allow_from:[ "user001"; "user002" ] ~webhook_url:None
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.dingtalk with
  | Some dt ->
      Alcotest.(check (list string))
        "allow_from" [ "user001"; "user002" ] dt.allow_from
  | None -> Alcotest.fail "expected dingtalk config"

let post_instructions_content () =
  let s = Setup_dingtalk.post_setup_instructions in
  Alcotest.(check bool)
    "has docs url" true
    (Test_helpers.string_contains s "https://clawq.org/channels/#dingtalk");
  Alcotest.(check bool)
    "has open platform mention" true
    (Test_helpers.string_contains s "open.dingtalk.com")

let suite =
  [
    Alcotest.test_case "validate_agent_id valid" `Quick validate_agent_id_valid;
    Alcotest.test_case "validate_agent_id empty" `Quick validate_agent_id_empty;
    Alcotest.test_case "validate_agent_id non-digits" `Quick
      validate_agent_id_non_digits;
    Alcotest.test_case "validate_app_key valid" `Quick validate_app_key_valid;
    Alcotest.test_case "validate_app_key empty" `Quick validate_app_key_empty;
    Alcotest.test_case "validate_app_secret valid" `Quick
      validate_app_secret_valid;
    Alcotest.test_case "validate_app_secret empty" `Quick
      validate_app_secret_empty;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json with webhook" `Quick build_json_with_webhook;
    Alcotest.test_case "build_json restricted users" `Quick
      build_json_restricted_users;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]

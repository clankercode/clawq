(* test_setup_teams.ml — Unit tests for Setup_teams pure functions *)

let validate_app_id_valid_uuid () =
  Alcotest.(check (result string string))
    "valid uuid" (Ok "12345678-abcd-1234-abcd-1234567890ab")
    (Setup_teams.validate_app_id "12345678-abcd-1234-abcd-1234567890ab")

let validate_app_id_spaces () =
  Alcotest.(check (result string string))
    "spaces trimmed" (Ok "12345678-abcd-1234-abcd-1234567890ab")
    (Setup_teams.validate_app_id "  12345678-abcd-1234-abcd-1234567890ab  ")

let validate_app_id_empty () =
  match Setup_teams.validate_app_id "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty"

let validate_app_id_not_uuid () =
  match Setup_teams.validate_app_id "not-a-uuid" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-UUID"

let validate_tenant_id_valid_uuid () =
  Alcotest.(check (result string string))
    "valid uuid" (Ok "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")
    (Setup_teams.validate_tenant_id "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")

let validate_tenant_id_common () =
  Alcotest.(check (result string string))
    "common" (Ok "common")
    (Setup_teams.validate_tenant_id "common")

let validate_tenant_id_organizations () =
  Alcotest.(check (result string string))
    "organizations" (Ok "organizations")
    (Setup_teams.validate_tenant_id "organizations")

let validate_tenant_id_consumers () =
  Alcotest.(check (result string string))
    "consumers" (Ok "consumers")
    (Setup_teams.validate_tenant_id "consumers")

let validate_tenant_id_empty () =
  match Setup_teams.validate_tenant_id "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty"

let validate_tenant_id_not_uuid () =
  match Setup_teams.validate_tenant_id "not-valid" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-UUID"

let build_json_basic () =
  let json =
    Setup_teams.build_teams_json ~app_id:"12345678-abcd-1234-abcd-1234567890ab"
      ~app_secret:"my-secret" ~tenant_id:"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
      ~webhook_path:"/teams/webhook"
      ~service_url:"https://smba.trafficmanager.net/amer" ~allow_teams:[ "*" ]
      ~allow_users:[ "*" ] ~default_model:None ~mention_mode:"entity"
      ~file_consent_cards:true
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.teams with
  | Some t ->
      Alcotest.(check string)
        "app_id" "12345678-abcd-1234-abcd-1234567890ab" t.app_id;
      Alcotest.(check string) "app_secret" "my-secret" t.app_secret;
      Alcotest.(check string)
        "tenant_id" "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" t.tenant_id;
      Alcotest.(check string) "webhook_path" "/teams/webhook" t.webhook_path;
      Alcotest.(check string)
        "service_url" "https://smba.trafficmanager.net/amer" t.service_url;
      Alcotest.(check (list string)) "allow_teams" [ "*" ] t.allow_teams;
      Alcotest.(check (list string)) "allow_users" [ "*" ] t.allow_users;
      Alcotest.(check (option string)) "default_model" None t.default_model;
      Alcotest.(check string) "mention_mode" "entity" t.mention_mode;
      Alcotest.(check bool) "file_consent_cards" true t.file_consent_cards
  | None -> Alcotest.fail "expected teams config"

let build_json_custom_allows () =
  let json =
    Setup_teams.build_teams_json ~app_id:"12345678-abcd-1234-abcd-1234567890ab"
      ~app_secret:"secret" ~tenant_id:"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
      ~webhook_path:"/my/teams/hook"
      ~service_url:"https://custom.service.url/v3"
      ~allow_teams:[ "team-a"; "team-b" ] ~allow_users:[ "alice"; "bob" ]
      ~default_model:(Some "openai:gpt-4") ~mention_mode:"text"
      ~file_consent_cards:false
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.teams with
  | Some t ->
      Alcotest.(check string) "webhook_path" "/my/teams/hook" t.webhook_path;
      Alcotest.(check string)
        "service_url" "https://custom.service.url/v3" t.service_url;
      Alcotest.(check (list string))
        "allow_teams" [ "team-a"; "team-b" ] t.allow_teams;
      Alcotest.(check (list string))
        "allow_users" [ "alice"; "bob" ] t.allow_users;
      Alcotest.(check (option string))
        "default_model" (Some "openai:gpt-4") t.default_model;
      Alcotest.(check string) "mention_mode" "text" t.mention_mode;
      Alcotest.(check bool) "file_consent_cards" false t.file_consent_cards
  | None -> Alcotest.fail "expected teams config"

let deep_merge_preserves_existing () =
  let existing =
    Yojson.Safe.from_string
      {|{"channels":{"cli":true,"telegram":{"accounts":{"default":{"bot_token":"tok"}}}},"default_temperature":0.7}|}
  in
  let overlay =
    Setup_teams.build_teams_json ~app_id:"12345678-abcd-1234-abcd-1234567890ab"
      ~app_secret:"secret" ~tenant_id:"aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
      ~webhook_path:"/teams/webhook"
      ~service_url:"https://smba.trafficmanager.net/amer" ~allow_teams:[ "*" ]
      ~allow_users:[ "*" ] ~default_model:None ~mention_mode:"entity"
      ~file_consent_cards:true
  in
  let result = Setup_common.deep_merge_json existing overlay in
  let config = Config_loader.parse_config ~resolve_secrets:false result in
  (match config.channels.teams with
  | Some _ -> ()
  | None -> Alcotest.fail "expected teams config after merge");
  match config.channels.telegram with
  | Some _ -> ()
  | None -> Alcotest.fail "telegram should be preserved after merge"

let instructions_without_tunnel () =
  let s =
    Setup_teams.post_setup_instructions ~webhook_path:"/teams/webhook"
      ~gateway_port:13451 ~tunnel_url:None
  in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has localhost url" true
    (contains "http://localhost:13451/teams/webhook");
  Alcotest.(check bool) "has tunnel note" true (contains "set up a tunnel");
  Alcotest.(check bool)
    "has azure portal" true
    (contains "https://portal.azure.com")

let instructions_with_tunnel () =
  let s =
    Setup_teams.post_setup_instructions ~webhook_path:"/teams/webhook"
      ~gateway_port:13451 ~tunnel_url:(Some "https://my.tunnel.example.com")
  in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has tunnel url" true
    (contains "https://my.tunnel.example.com/teams/webhook");
  Alcotest.(check bool) "no tunnel note" false (contains "set up a tunnel")

let instructions_mentions_teams_channel () =
  let s =
    Setup_teams.post_setup_instructions ~webhook_path:"/teams/webhook"
      ~gateway_port:8080 ~tunnel_url:None
  in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "mentions Microsoft Teams" true
    (contains "Microsoft Teams")

let suite =
  [
    Alcotest.test_case "validate_app_id valid uuid" `Quick
      validate_app_id_valid_uuid;
    Alcotest.test_case "validate_app_id spaces" `Quick validate_app_id_spaces;
    Alcotest.test_case "validate_app_id empty" `Quick validate_app_id_empty;
    Alcotest.test_case "validate_app_id not uuid" `Quick
      validate_app_id_not_uuid;
    Alcotest.test_case "validate_tenant_id valid uuid" `Quick
      validate_tenant_id_valid_uuid;
    Alcotest.test_case "validate_tenant_id common" `Quick
      validate_tenant_id_common;
    Alcotest.test_case "validate_tenant_id organizations" `Quick
      validate_tenant_id_organizations;
    Alcotest.test_case "validate_tenant_id consumers" `Quick
      validate_tenant_id_consumers;
    Alcotest.test_case "validate_tenant_id empty" `Quick
      validate_tenant_id_empty;
    Alcotest.test_case "validate_tenant_id not uuid" `Quick
      validate_tenant_id_not_uuid;
    Alcotest.test_case "build_json basic roundtrip" `Quick build_json_basic;
    Alcotest.test_case "build_json custom allows" `Quick
      build_json_custom_allows;
    Alcotest.test_case "deep merge preserves existing" `Quick
      deep_merge_preserves_existing;
    Alcotest.test_case "instructions without tunnel" `Quick
      instructions_without_tunnel;
    Alcotest.test_case "instructions with tunnel" `Quick
      instructions_with_tunnel;
    Alcotest.test_case "instructions mentions Teams channel" `Quick
      instructions_mentions_teams_channel;
  ]

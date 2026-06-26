(* test_setup_gateway.ml — Unit tests for Setup_gateway pure functions *)

let validate_port_valid () =
  Alcotest.(check (result string string))
    "valid port" (Ok "13451")
    (Setup_common.validate_port "13451")

let validate_port_min () =
  Alcotest.(check (result string string))
    "port 1 ok" (Ok "1")
    (Setup_common.validate_port "1")

let validate_port_max () =
  Alcotest.(check (result string string))
    "port 65535 ok" (Ok "65535")
    (Setup_common.validate_port "65535")

let validate_port_zero () =
  match Setup_common.validate_port "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for port 0"

let validate_port_too_high () =
  match Setup_common.validate_port "65536" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for port > 65535"

let validate_port_non_number () =
  match Setup_common.validate_port "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-number"

let validate_host_valid () =
  Alcotest.(check (result string string))
    "valid host" (Ok "127.0.0.1")
    (Setup_gateway.validate_host "127.0.0.1")

let validate_host_empty () =
  match Setup_gateway.validate_host "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty host"

let validate_host_whitespace () =
  match Setup_gateway.validate_host "   " with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for whitespace-only host"

let validate_positive_int_valid () =
  Alcotest.(check (result string string))
    "valid positive int" (Ok "5")
    (Setup_common.validate_positive_int "5")

let validate_positive_int_zero () =
  match Setup_common.validate_positive_int "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero"

let build_json_roundtrip () =
  let json =
    Setup_gateway.build_gateway_json ~host:"127.0.0.1" ~port:13451
      ~require_pairing:true ~auth_token:"" ~max_pair_attempts:5
      ~pair_lockout_seconds:300
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let gw = config.gateway in
  Alcotest.(check string) "host" "127.0.0.1" gw.host;
  Alcotest.(check int) "port" 13451 gw.port;
  Alcotest.(check bool) "require_pairing" true gw.require_pairing;
  Alcotest.(check int) "max_pair_attempts" 5 gw.max_pair_attempts;
  Alcotest.(check int) "pair_lockout_seconds" 300 gw.pair_lockout_seconds

let build_json_auth_token_set () =
  let json =
    Setup_gateway.build_gateway_json ~host:"0.0.0.0" ~port:8080
      ~require_pairing:false ~auth_token:"mysecrettoken" ~max_pair_attempts:3
      ~pair_lockout_seconds:60
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let gw = config.gateway in
  Alcotest.(check string) "host" "0.0.0.0" gw.host;
  Alcotest.(check int) "port" 8080 gw.port;
  Alcotest.(check bool) "require_pairing" false gw.require_pairing;
  (match gw.auth_token with
  | Some t -> Alcotest.(check string) "auth_token" "mysecrettoken" t
  | None -> Alcotest.fail "expected auth_token to be Some");
  Alcotest.(check int) "max_pair_attempts" 3 gw.max_pair_attempts;
  Alcotest.(check int) "pair_lockout_seconds" 60 gw.pair_lockout_seconds

let build_json_auth_token_empty_is_none () =
  let json =
    Setup_gateway.build_gateway_json ~host:"127.0.0.1" ~port:13451
      ~require_pairing:true ~auth_token:"" ~max_pair_attempts:5
      ~pair_lockout_seconds:300
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.gateway.auth_token with
  | None -> ()
  | Some _ -> Alcotest.fail "expected auth_token to be None for empty string"

let post_instructions_content () =
  let s = Setup_gateway.post_setup_instructions in
  Alcotest.(check bool)
    "has docs url" true
    (Test_helpers.string_contains s "https://clawq.org/gateway/");
  Alcotest.(check bool) "mentions pairing" true (Test_helpers.string_contains s "pairing");
  Alcotest.(check bool) "mentions port" true (Test_helpers.string_contains s "port")

let suite =
  [
    Alcotest.test_case "validate_port valid" `Quick validate_port_valid;
    Alcotest.test_case "validate_port min" `Quick validate_port_min;
    Alcotest.test_case "validate_port max" `Quick validate_port_max;
    Alcotest.test_case "validate_port zero" `Quick validate_port_zero;
    Alcotest.test_case "validate_port too high" `Quick validate_port_too_high;
    Alcotest.test_case "validate_port non-number" `Quick
      validate_port_non_number;
    Alcotest.test_case "validate_host valid" `Quick validate_host_valid;
    Alcotest.test_case "validate_host empty" `Quick validate_host_empty;
    Alcotest.test_case "validate_host whitespace" `Quick
      validate_host_whitespace;
    Alcotest.test_case "validate_positive_int valid" `Quick
      validate_positive_int_valid;
    Alcotest.test_case "validate_positive_int zero" `Quick
      validate_positive_int_zero;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json auth_token set" `Quick
      build_json_auth_token_set;
    Alcotest.test_case "build_json auth_token empty -> None" `Quick
      build_json_auth_token_empty_is_none;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]

(* test_setup_resilience.ml — Unit tests for Setup_resilience pure functions *)

let validate_timeout_valid () =
  Alcotest.(check (result string string))
    "valid timeout" (Ok "120.0")
    (Setup_resilience.validate_timeout "120.0")

let validate_timeout_integer () =
  Alcotest.(check (result string string))
    "integer timeout ok" (Ok "60")
    (Setup_resilience.validate_timeout "60")

let validate_timeout_zero () =
  match Setup_resilience.validate_timeout "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero timeout"

let validate_timeout_negative () =
  match Setup_resilience.validate_timeout "-1.0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative timeout"

let validate_timeout_non_number () =
  match Setup_resilience.validate_timeout "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-number"

let validate_retries_valid () =
  Alcotest.(check (result string string))
    "valid retries 2" (Ok "2")
    (Setup_resilience.validate_retries "2")

let validate_retries_zero () =
  Alcotest.(check (result string string))
    "zero retries ok" (Ok "0")
    (Setup_resilience.validate_retries "0")

let validate_retries_negative () =
  match Setup_resilience.validate_retries "-1" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative retries"

let validate_retries_non_int () =
  match Setup_resilience.validate_retries "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-integer"

let validate_delay_valid () =
  Alcotest.(check (result string string))
    "valid delay" (Ok "1.0")
    (Setup_resilience.validate_delay "1.0")

let validate_delay_zero () =
  match Setup_resilience.validate_delay "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero delay"

let validate_delay_negative () =
  match Setup_resilience.validate_delay "-0.5" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative delay"

let build_json_roundtrip () =
  let json =
    Setup_resilience.build_resilience_json ~timeout_s:120.0 ~max_retries:2
      ~base_delay_s:1.0 ~fallback_provider:""
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let r = config.resilience in
  Alcotest.(check (float 0.001)) "timeout_s" 120.0 r.timeout_s;
  Alcotest.(check int) "max_retries" 2 r.max_retries;
  Alcotest.(check (float 0.001)) "base_delay_s" 1.0 r.base_delay_s;
  Alcotest.(check (option string))
    "fallback_provider none" None r.fallback_provider

let build_json_with_fallback () =
  let json =
    Setup_resilience.build_resilience_json ~timeout_s:60.0 ~max_retries:3
      ~base_delay_s:2.0 ~fallback_provider:"anthropic"
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let r = config.resilience in
  Alcotest.(check (float 0.001)) "timeout_s" 60.0 r.timeout_s;
  Alcotest.(check int) "max_retries" 3 r.max_retries;
  Alcotest.(check (float 0.001)) "base_delay_s" 2.0 r.base_delay_s;
  Alcotest.(check (option string))
    "fallback_provider" (Some "anthropic") r.fallback_provider

let post_instructions_content () =
  let s = Setup_resilience.post_setup_instructions in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has docs url" true
    (contains "https://clawq.org/resilience/");
  Alcotest.(check bool) "mentions timeout" true (contains "timeout");
  Alcotest.(check bool) "mentions retries" true (contains "retries")

let suite =
  [
    Alcotest.test_case "validate_timeout valid" `Quick validate_timeout_valid;
    Alcotest.test_case "validate_timeout integer" `Quick
      validate_timeout_integer;
    Alcotest.test_case "validate_timeout zero" `Quick validate_timeout_zero;
    Alcotest.test_case "validate_timeout negative" `Quick
      validate_timeout_negative;
    Alcotest.test_case "validate_timeout non-number" `Quick
      validate_timeout_non_number;
    Alcotest.test_case "validate_retries valid" `Quick validate_retries_valid;
    Alcotest.test_case "validate_retries zero" `Quick validate_retries_zero;
    Alcotest.test_case "validate_retries negative" `Quick
      validate_retries_negative;
    Alcotest.test_case "validate_retries non-int" `Quick
      validate_retries_non_int;
    Alcotest.test_case "validate_delay valid" `Quick validate_delay_valid;
    Alcotest.test_case "validate_delay zero" `Quick validate_delay_zero;
    Alcotest.test_case "validate_delay negative" `Quick validate_delay_negative;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json with fallback" `Quick
      build_json_with_fallback;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]

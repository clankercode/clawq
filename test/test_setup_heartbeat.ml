(* test_setup_heartbeat.ml — Unit tests for Setup_heartbeat pure functions *)

let validate_interval_valid () =
  Alcotest.(check (result string string))
    "valid interval 250" (Ok "250")
    (Setup_heartbeat.validate_interval "250")

let validate_interval_one () =
  Alcotest.(check (result string string))
    "interval 1 ok" (Ok "1")
    (Setup_heartbeat.validate_interval "1")

let validate_interval_zero () =
  match Setup_heartbeat.validate_interval "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero interval"

let validate_interval_negative () =
  match Setup_heartbeat.validate_interval "-10" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative interval"

let validate_interval_non_int () =
  match Setup_heartbeat.validate_interval "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-integer"

let validate_hour_valid () =
  Alcotest.(check (result string string))
    "valid hour 8" (Ok "8")
    (Setup_heartbeat.validate_hour "8")

let validate_hour_zero () =
  Alcotest.(check (result string string))
    "hour 0 ok" (Ok "0")
    (Setup_heartbeat.validate_hour "0")

let validate_hour_23 () =
  Alcotest.(check (result string string))
    "hour 23 ok" (Ok "23")
    (Setup_heartbeat.validate_hour "23")

let validate_hour_24 () =
  match Setup_heartbeat.validate_hour "24" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for hour 24"

let validate_hour_negative () =
  match Setup_heartbeat.validate_hour "-1" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative hour"

let validate_hour_non_int () =
  match Setup_heartbeat.validate_hour "noon" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-integer"

let build_json_roundtrip () =
  let json =
    Setup_heartbeat.build_heartbeat_json ~enabled:true ~interval_seconds:250
      ~quiet_start:23 ~quiet_end:8
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let h = config.heartbeat in
  Alcotest.(check bool) "enabled" true h.enabled;
  Alcotest.(check int) "interval_seconds" 250 h.interval_seconds;
  Alcotest.(check int) "quiet_start" 23 h.quiet_start;
  Alcotest.(check int) "quiet_end" 8 h.quiet_end

let build_json_disabled () =
  let json =
    Setup_heartbeat.build_heartbeat_json ~enabled:false ~interval_seconds:60
      ~quiet_start:22 ~quiet_end:7
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let h = config.heartbeat in
  Alcotest.(check bool) "enabled false" false h.enabled;
  Alcotest.(check int) "interval_seconds 60" 60 h.interval_seconds;
  Alcotest.(check int) "quiet_start 22" 22 h.quiet_start;
  Alcotest.(check int) "quiet_end 7" 7 h.quiet_end

let post_instructions_content () =
  let s = Setup_heartbeat.post_setup_instructions in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has docs url" true
    (contains "https://clawq.org/heartbeat/");
  Alcotest.(check bool) "mentions interval" true (contains "interval");
  Alcotest.(check bool) "mentions quiet" true (contains "quiet")

let suite =
  [
    Alcotest.test_case "validate_interval valid" `Quick validate_interval_valid;
    Alcotest.test_case "validate_interval one" `Quick validate_interval_one;
    Alcotest.test_case "validate_interval zero" `Quick validate_interval_zero;
    Alcotest.test_case "validate_interval negative" `Quick
      validate_interval_negative;
    Alcotest.test_case "validate_interval non-int" `Quick
      validate_interval_non_int;
    Alcotest.test_case "validate_hour valid" `Quick validate_hour_valid;
    Alcotest.test_case "validate_hour zero" `Quick validate_hour_zero;
    Alcotest.test_case "validate_hour 23" `Quick validate_hour_23;
    Alcotest.test_case "validate_hour 24" `Quick validate_hour_24;
    Alcotest.test_case "validate_hour negative" `Quick validate_hour_negative;
    Alcotest.test_case "validate_hour non-int" `Quick validate_hour_non_int;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json disabled" `Quick build_json_disabled;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]

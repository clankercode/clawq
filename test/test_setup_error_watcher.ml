(* test_setup_error_watcher.ml — Unit tests for Setup_error_watcher pure functions *)

let validate_scan_interval_valid () =
  Alcotest.(check (result string string))
    "valid interval" (Ok "30.0")
    (Setup_error_watcher.validate_scan_interval "30.0")

let validate_scan_interval_integer () =
  Alcotest.(check (result string string))
    "integer ok" (Ok "60")
    (Setup_error_watcher.validate_scan_interval "60")

let validate_scan_interval_zero () =
  match Setup_error_watcher.validate_scan_interval "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero"

let validate_scan_interval_negative () =
  match Setup_error_watcher.validate_scan_interval "-5.0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative"

let validate_scan_interval_non_number () =
  match Setup_error_watcher.validate_scan_interval "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-number"

let validate_cooldown_valid () =
  Alcotest.(check (result string string))
    "valid cooldown" (Ok "300.0")
    (Setup_error_watcher.validate_cooldown "300.0")

let validate_cooldown_zero () =
  match Setup_error_watcher.validate_cooldown "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero cooldown"

let validate_max_errors_valid () =
  Alcotest.(check (result string string))
    "valid max errors" (Ok "10")
    (Setup_error_watcher.validate_max_errors "10")

let validate_max_errors_zero () =
  match Setup_error_watcher.validate_max_errors "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero max errors"

let validate_max_errors_negative () =
  match Setup_error_watcher.validate_max_errors "-1" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative max errors"

let validate_max_errors_non_int () =
  match Setup_error_watcher.validate_max_errors "3.5" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-integer max errors"

let validate_commit_tag_valid () =
  Alcotest.(check (result string string))
    "valid commit tag" (Ok "[INTERNAL_EC]")
    (Setup_error_watcher.validate_commit_tag "[INTERNAL_EC]")

let validate_commit_tag_empty () =
  match Setup_error_watcher.validate_commit_tag "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty commit tag"

let build_json_roundtrip () =
  let json =
    Setup_error_watcher.build_error_watcher_json ~enabled:true
      ~scan_interval_s:30.0
      ~primary_models:[ "anthropic:claude-opus-4-6"; "openai-codex:gpt-5.4" ]
      ~fallback_models:[ "zai_coding:glm-5"; "kimi_coding:kimi-for-code" ]
      ~cooldown_s:300.0 ~max_errors_per_batch:10 ~ignore_patterns:[]
      ~auto_fix_enabled:false ~commit_tag:"[INTERNAL_EC]"
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let ew = config.error_watcher in
  Alcotest.(check bool) "enabled" true ew.enabled;
  Alcotest.(check (float 0.001)) "scan_interval_s" 30.0 ew.scan_interval_s;
  Alcotest.(check (list string))
    "primary_models"
    [ "anthropic:claude-opus-4-6"; "openai-codex:gpt-5.4" ]
    ew.primary_models;
  Alcotest.(check (list string))
    "fallback_models"
    [ "zai_coding:glm-5"; "kimi_coding:kimi-for-code" ]
    ew.fallback_models;
  Alcotest.(check (float 0.001)) "cooldown_s" 300.0 ew.cooldown_s;
  Alcotest.(check int) "max_errors_per_batch" 10 ew.max_errors_per_batch;
  Alcotest.(check (list string)) "ignore_patterns" [] ew.ignore_patterns;
  Alcotest.(check bool) "auto_fix_enabled" false ew.auto_fix_enabled;
  Alcotest.(check string) "commit_tag" "[INTERNAL_EC]" ew.commit_tag

let build_json_with_ignore_patterns () =
  let json =
    Setup_error_watcher.build_error_watcher_json ~enabled:false
      ~scan_interval_s:60.0
      ~primary_models:[ "anthropic:claude-opus-4-6" ]
      ~fallback_models:[] ~cooldown_s:600.0 ~max_errors_per_batch:5
      ~ignore_patterns:[ "WARN"; "TODO" ] ~auto_fix_enabled:true
      ~commit_tag:"[AUTO]"
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let ew = config.error_watcher in
  Alcotest.(check bool) "enabled false" false ew.enabled;
  Alcotest.(check (list string))
    "ignore_patterns" [ "WARN"; "TODO" ] ew.ignore_patterns;
  Alcotest.(check bool) "auto_fix_enabled" true ew.auto_fix_enabled;
  Alcotest.(check string) "commit_tag" "[AUTO]" ew.commit_tag

let post_instructions_content () =
  let s = Setup_error_watcher.post_setup_instructions in
  Alcotest.(check bool)
    "has docs url" true
    (Test_helpers.string_contains s "https://clawq.org/error-watcher/");
  Alcotest.(check bool)
    "mentions scan" true
    (Test_helpers.string_contains s "scan")

let suite =
  [
    Alcotest.test_case "validate_scan_interval valid" `Quick
      validate_scan_interval_valid;
    Alcotest.test_case "validate_scan_interval integer" `Quick
      validate_scan_interval_integer;
    Alcotest.test_case "validate_scan_interval zero" `Quick
      validate_scan_interval_zero;
    Alcotest.test_case "validate_scan_interval negative" `Quick
      validate_scan_interval_negative;
    Alcotest.test_case "validate_scan_interval non-number" `Quick
      validate_scan_interval_non_number;
    Alcotest.test_case "validate_cooldown valid" `Quick validate_cooldown_valid;
    Alcotest.test_case "validate_cooldown zero" `Quick validate_cooldown_zero;
    Alcotest.test_case "validate_max_errors valid" `Quick
      validate_max_errors_valid;
    Alcotest.test_case "validate_max_errors zero" `Quick
      validate_max_errors_zero;
    Alcotest.test_case "validate_max_errors negative" `Quick
      validate_max_errors_negative;
    Alcotest.test_case "validate_max_errors non-int" `Quick
      validate_max_errors_non_int;
    Alcotest.test_case "validate_commit_tag valid" `Quick
      validate_commit_tag_valid;
    Alcotest.test_case "validate_commit_tag empty" `Quick
      validate_commit_tag_empty;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json with ignore patterns" `Quick
      build_json_with_ignore_patterns;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]

(* test_setup_security.ml — Unit tests for Setup_security pure functions *)

let validate_rpm_valid () =
  Alcotest.(check (result string string))
    "valid rpm" (Ok "60")
    (Setup_security.validate_rpm "60")

let validate_rpm_zero () =
  match Setup_security.validate_rpm "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero"

let validate_rpm_negative () =
  match Setup_security.validate_rpm "-1" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative"

let validate_rpm_non_number () =
  match Setup_security.validate_rpm "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-number"

let validate_burst_multiplier_valid () =
  Alcotest.(check (result string string))
    "valid burst" (Ok "1.5")
    (Setup_security.validate_burst_multiplier "1.5")

let validate_burst_multiplier_one () =
  Alcotest.(check (result string string))
    "exactly 1.0 ok" (Ok "1.0")
    (Setup_security.validate_burst_multiplier "1.0")

let validate_burst_multiplier_below_one () =
  match Setup_security.validate_burst_multiplier "0.9" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for < 1.0"

let validate_max_age_days_valid () =
  Alcotest.(check (result string string))
    "valid age" (Ok "90")
    (Setup_security.validate_max_age_days "90")

let validate_max_age_days_zero () =
  match Setup_security.validate_max_age_days "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero"

let validate_max_entries_valid () =
  Alcotest.(check (result string string))
    "valid entries" (Ok "100000")
    (Setup_security.validate_max_entries "100000")

let validate_max_entries_negative () =
  match Setup_security.validate_max_entries "-5" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative"

let build_json_roundtrip () =
  let json =
    Setup_security.build_security_json ~workspace_only:true ~audit_enabled:false
      ~tools_enabled:true ~encrypt_secrets:false ~audit_signing_enabled:false
      ~landlock_enabled:false ~sandbox_backend:"auto" ~gateway_per_ip_rpm:60
      ~gateway_per_session_rpm:30 ~telegram_per_chat_rpm:20
      ~burst_multiplier:1.5 ~audit_max_age_days:90 ~audit_max_entries:100000
      ~audit_export_before_purge:false ~extra_allowed_paths:[]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let s = config.security in
  Alcotest.(check bool) "workspace_only" true s.workspace_only;
  Alcotest.(check bool) "audit_enabled" false s.audit_enabled;
  Alcotest.(check bool) "tools_enabled" true s.tools_enabled;
  Alcotest.(check int) "gateway_per_ip_rpm" 60 s.rate_limit.gateway_per_ip_rpm;
  Alcotest.(check int)
    "gateway_per_session_rpm" 30 s.rate_limit.gateway_per_session_rpm;
  Alcotest.(check int)
    "telegram_per_chat_rpm" 20 s.rate_limit.telegram_per_chat_rpm;
  Alcotest.(check (float 0.001))
    "burst_multiplier" 1.5 s.rate_limit.burst_multiplier;
  Alcotest.(check int) "audit_max_age_days" 90 s.audit_retention.max_age_days;
  Alcotest.(check int) "audit_max_entries" 100000 s.audit_retention.max_entries;
  Alcotest.(check bool)
    "export_before_purge" false s.audit_retention.export_before_purge;
  Alcotest.(check string) "sandbox_backend" "auto" s.sandbox_backend

let build_json_nested_structure () =
  let json =
    Setup_security.build_security_json ~workspace_only:false ~audit_enabled:true
      ~tools_enabled:false ~encrypt_secrets:true ~audit_signing_enabled:true
      ~landlock_enabled:true ~sandbox_backend:"firejail" ~gateway_per_ip_rpm:120
      ~gateway_per_session_rpm:60 ~telegram_per_chat_rpm:40
      ~burst_multiplier:2.0 ~audit_max_age_days:30 ~audit_max_entries:50000
      ~audit_export_before_purge:true
      ~extra_allowed_paths:[ "/tmp"; "/var/log" ]
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let s = config.security in
  Alcotest.(check bool) "workspace_only false" false s.workspace_only;
  Alcotest.(check bool) "audit_enabled true" true s.audit_enabled;
  Alcotest.(check bool) "landlock_enabled true" true s.landlock_enabled;
  Alcotest.(check string) "sandbox_backend" "firejail" s.sandbox_backend;
  Alcotest.(check int) "gateway_per_ip_rpm" 120 s.rate_limit.gateway_per_ip_rpm;
  Alcotest.(check bool)
    "export_before_purge" true s.audit_retention.export_before_purge;
  Alcotest.(check (list string))
    "extra_allowed_paths" [ "/tmp"; "/var/log" ] s.extra_allowed_paths

let post_instructions_content () =
  let s = Setup_security.post_setup_instructions in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has docs url" true
    (contains "https://clawq.org/security/");
  Alcotest.(check bool)
    "mentions workspace_only" true
    (contains "workspace_only");
  Alcotest.(check bool) "mentions rate limit" true (contains "Rate limits")

let suite =
  [
    Alcotest.test_case "validate_rpm valid" `Quick validate_rpm_valid;
    Alcotest.test_case "validate_rpm zero" `Quick validate_rpm_zero;
    Alcotest.test_case "validate_rpm negative" `Quick validate_rpm_negative;
    Alcotest.test_case "validate_rpm non-number" `Quick validate_rpm_non_number;
    Alcotest.test_case "validate_burst_multiplier valid" `Quick
      validate_burst_multiplier_valid;
    Alcotest.test_case "validate_burst_multiplier exactly 1.0" `Quick
      validate_burst_multiplier_one;
    Alcotest.test_case "validate_burst_multiplier below 1.0" `Quick
      validate_burst_multiplier_below_one;
    Alcotest.test_case "validate_max_age_days valid" `Quick
      validate_max_age_days_valid;
    Alcotest.test_case "validate_max_age_days zero" `Quick
      validate_max_age_days_zero;
    Alcotest.test_case "validate_max_entries valid" `Quick
      validate_max_entries_valid;
    Alcotest.test_case "validate_max_entries negative" `Quick
      validate_max_entries_negative;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json nested structure" `Quick
      build_json_nested_structure;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]

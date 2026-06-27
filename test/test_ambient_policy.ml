open Ambient_policy

(* Helper to build a room profile with ambient fields set *)
let make_test_profile ?(ambient_enabled = false)
    ?(ambient_quiet_start = default_ambient_quiet_start)
    ?(ambient_quiet_end = default_ambient_quiet_end)
    ?(ambient_rate_limit_rph = 0) () =
  make_profile ~ambient_enabled ~ambient_quiet_start ~ambient_quiet_end
    ~ambient_rate_limit_rph ~id:"test-profile" ~display_name:(Some "Test")
    ~model:"gpt-5.4" ~system_prompt:"" ~max_tool_iterations:10 ~status:"active"
    ~allowed_tools:[] ~denied_tools:[] ()

(* --- Ambient opt-in --- *)

let test_defaults_off () =
  let p = make_test_profile () in
  let result = check_ambient_enabled p in
  (match result with
  | Denied Ambient_not_enabled -> ()
  | _ -> Alcotest.fail "expected Ambient_not_enabled when ambient_enabled=false");
  (* Verify check_all also denies *)
  let all_result =
    check_all ~hour:12 ~deliveries_this_hour:0 ~budget_exceeded:false
      ~supports_ambient:true p
  in
  match all_result with
  | Denied Ambient_not_enabled -> ()
  | _ -> Alcotest.fail "check_all should deny when ambient not enabled"

let test_explicit_opt_in () =
  let p = make_test_profile ~ambient_enabled:true () in
  let result = check_ambient_enabled p in
  (match result with
  | Allowed -> ()
  | _ -> Alcotest.fail "expected Allowed when ambient_enabled=true");
  let all_result =
    check_all ~hour:12 ~deliveries_this_hour:0 ~budget_exceeded:false
      ~supports_ambient:true p
  in
  match all_result with
  | Allowed -> ()
  | _ -> Alcotest.fail "check_all should allow when all conditions met"

(* --- Quiet hours --- *)

let test_quiet_hours_default_range () =
  (* Default: quiet_start=23, quiet_end=8. Hour 12 should be allowed. *)
  let p = make_test_profile ~ambient_enabled:true () in
  let result = check_quiet_hours ~hour:12 p in
  (match result with
  | Allowed -> ()
  | _ -> Alcotest.fail "hour 12 should be allowed with default quiet hours");
  (* Hour 23 (start of quiet) should be denied *)
  let result23 = check_quiet_hours ~hour:23 p in
  (match result23 with
  | Denied Quiet_hours -> ()
  | _ -> Alcotest.fail "hour 23 should be denied (quiet start)");
  (* Hour 0 (midnight, within quiet) should be denied *)
  let result0 = check_quiet_hours ~hour:0 p in
  (match result0 with
  | Denied Quiet_hours -> ()
  | _ -> Alcotest.fail "hour 0 should be denied (within quiet hours)");
  (* Hour 7 (within quiet) should be denied *)
  let result7 = check_quiet_hours ~hour:7 p in
  (match result7 with
  | Denied Quiet_hours -> ()
  | _ -> Alcotest.fail "hour 7 should be denied (within quiet hours)");
  (* Hour 8 (quiet end, should be allowed) *)
  let result8 = check_quiet_hours ~hour:8 p in
  match result8 with
  | Allowed -> ()
  | _ -> Alcotest.fail "hour 8 should be allowed (quiet end)"

let test_quiet_hours_same_start_end_disables () =
  (* When start = end, quiet hours are disabled *)
  let p =
    make_test_profile ~ambient_enabled:true ~ambient_quiet_start:10
      ~ambient_quiet_end:10 ()
  in
  let result = check_quiet_hours ~hour:10 p in
  match result with
  | Allowed -> ()
  | _ -> Alcotest.fail "same start/end should disable quiet hours"

let test_quiet_hours_non_wrapping_range () =
  (* Quiet 14..18: afternoon nap *)
  let p =
    make_test_profile ~ambient_enabled:true ~ambient_quiet_start:14
      ~ambient_quiet_end:18 ()
  in
  (match check_quiet_hours ~hour:13 p with
  | Allowed -> ()
  | _ -> Alcotest.fail "hour 13 should be allowed");
  (match check_quiet_hours ~hour:14 p with
  | Denied Quiet_hours -> ()
  | _ -> Alcotest.fail "hour 14 should be denied (quiet start)");
  (match check_quiet_hours ~hour:17 p with
  | Denied Quiet_hours -> ()
  | _ -> Alcotest.fail "hour 17 should be denied (within quiet)");
  match check_quiet_hours ~hour:18 p with
  | Allowed -> ()
  | _ -> Alcotest.fail "hour 18 should be allowed (quiet end)"

let test_quiet_hours_check_all_respects_it () =
  let p = make_test_profile ~ambient_enabled:true () in
  (* During quiet hours, check_all should deny even if everything else is fine *)
  let result =
    check_all ~hour:2 ~deliveries_this_hour:0 ~budget_exceeded:false
      ~supports_ambient:true p
  in
  match result with
  | Denied Quiet_hours -> ()
  | _ -> Alcotest.fail "check_all should deny during quiet hours"

(* --- Rate limits --- *)

let test_rate_limit_zero_means_unlimited () =
  let p =
    make_test_profile ~ambient_enabled:true ~ambient_rate_limit_rph:0 ()
  in
  let result = check_rate_limit ~deliveries_this_hour:1000 p in
  match result with
  | Allowed -> ()
  | _ -> Alcotest.fail "rate_limit_rph=0 means unlimited"

let test_rate_limit_under_limit () =
  let p =
    make_test_profile ~ambient_enabled:true ~ambient_rate_limit_rph:5 ()
  in
  let result = check_rate_limit ~deliveries_this_hour:4 p in
  match result with
  | Allowed -> ()
  | _ -> Alcotest.fail "4 deliveries should be allowed with limit 5"

let test_rate_limit_at_limit () =
  let p =
    make_test_profile ~ambient_enabled:true ~ambient_rate_limit_rph:5 ()
  in
  let result = check_rate_limit ~deliveries_this_hour:5 p in
  match result with
  | Denied Rate_limited -> ()
  | _ -> Alcotest.fail "5 deliveries should be denied with limit 5"

let test_rate_limit_over_limit () =
  let p =
    make_test_profile ~ambient_enabled:true ~ambient_rate_limit_rph:3 ()
  in
  let result = check_rate_limit ~deliveries_this_hour:10 p in
  match result with
  | Denied Rate_limited -> ()
  | _ -> Alcotest.fail "10 deliveries should be denied with limit 3"

let test_rate_limit_check_all_respects_it () =
  let p =
    make_test_profile ~ambient_enabled:true ~ambient_rate_limit_rph:2 ()
  in
  let result =
    check_all ~hour:12 ~deliveries_this_hour:5 ~budget_exceeded:false
      ~supports_ambient:true p
  in
  match result with
  | Denied Rate_limited -> ()
  | _ -> Alcotest.fail "check_all should deny when rate limit exceeded"

(* --- Budget gate --- *)

let test_budget_exceeded () =
  let result = check_budget ~budget_exceeded:true in
  match result with
  | Denied Budget_exceeded -> ()
  | _ -> Alcotest.fail "should deny when budget exceeded"

let test_budget_not_exceeded () =
  let result = check_budget ~budget_exceeded:false in
  match result with
  | Allowed -> ()
  | _ -> Alcotest.fail "should allow when budget not exceeded"

let test_budget_check_all_respects_it () =
  let p = make_test_profile ~ambient_enabled:true () in
  let result =
    check_all ~hour:12 ~deliveries_this_hour:0 ~budget_exceeded:true
      ~supports_ambient:true p
  in
  match result with
  | Denied Budget_exceeded -> ()
  | _ -> Alcotest.fail "check_all should deny when budget exceeded"

(* --- Connector capability --- *)

let test_connector_unsupported () =
  let result = check_connector ~supports_ambient:false in
  match result with
  | Denied Connector_unsupported -> ()
  | _ -> Alcotest.fail "should deny when connector doesn't support ambient"

let test_connector_supported () =
  let result = check_connector ~supports_ambient:true in
  match result with
  | Allowed -> ()
  | _ -> Alcotest.fail "should allow when connector supports ambient"

let test_connector_check_all_respects_it () =
  let p = make_test_profile ~ambient_enabled:true () in
  let result =
    check_all ~hour:12 ~deliveries_this_hour:0 ~budget_exceeded:false
      ~supports_ambient:false p
  in
  match result with
  | Denied Connector_unsupported -> ()
  | _ -> Alcotest.fail "check_all should deny when connector unsupported"

(* --- check_all priority order --- *)

let test_check_all_priority_order () =
  (* Opt-in check is first: even if everything else would fail, Ambient_not_enabled comes first *)
  let p =
    make_test_profile ~ambient_enabled:false ~ambient_rate_limit_rph:0 ()
  in
  let result =
    check_all ~hour:2 ~deliveries_this_hour:100 ~budget_exceeded:true
      ~supports_ambient:false p
  in
  (match result with
  | Denied Ambient_not_enabled -> ()
  | _ ->
      Alcotest.fail
        "Ambient_not_enabled should have priority over all other denials");
  (* With opt-in, quiet hours is next *)
  let p2 = make_test_profile ~ambient_enabled:true () in
  let result2 =
    check_all ~hour:2 ~deliveries_this_hour:100 ~budget_exceeded:true
      ~supports_ambient:false p2
  in
  (match result2 with
  | Denied Quiet_hours -> ()
  | _ -> Alcotest.fail "Quiet_hours should come after opt-in");
  (* With opt-in and outside quiet hours, rate limit is next *)
  let p3 =
    make_test_profile ~ambient_enabled:true ~ambient_rate_limit_rph:1 ()
  in
  let result3 =
    check_all ~hour:12 ~deliveries_this_hour:100 ~budget_exceeded:true
      ~supports_ambient:false p3
  in
  (match result3 with
  | Denied Rate_limited -> ()
  | _ -> Alcotest.fail "Rate_limited should come after quiet hours");
  (* With opt-in, outside quiet hours, under rate limit, budget is next *)
  let p4 =
    make_test_profile ~ambient_enabled:true ~ambient_rate_limit_rph:0 ()
  in
  let result4 =
    check_all ~hour:12 ~deliveries_this_hour:0 ~budget_exceeded:true
      ~supports_ambient:false p4
  in
  (match result4 with
  | Denied Budget_exceeded -> ()
  | _ -> Alcotest.fail "Budget_exceeded should come after rate limit");
  (* Connector is last check *)
  let p5 =
    make_test_profile ~ambient_enabled:true ~ambient_rate_limit_rph:0 ()
  in
  let result5 =
    check_all ~hour:12 ~deliveries_this_hour:0 ~budget_exceeded:false
      ~supports_ambient:false p5
  in
  match result5 with
  | Denied Connector_unsupported -> ()
  | _ -> Alcotest.fail "Connector_unsupported should be last check"

(* --- reason_to_string --- *)

let test_reason_to_string () =
  let reasons =
    [
      Ambient_not_enabled;
      Quiet_hours;
      Rate_limited;
      Budget_exceeded;
      Connector_unsupported;
    ]
  in
  List.iter
    (fun r ->
      let s = reason_to_string r in
      if String.length s = 0 then
        Alcotest.failf "reason_to_string returned empty for %s"
          (match r with
          | Ambient_not_enabled -> "Ambient_not_enabled"
          | Quiet_hours -> "Quiet_hours"
          | Rate_limited -> "Rate_limited"
          | Budget_exceeded -> "Budget_exceeded"
          | Connector_unsupported -> "Connector_unsupported"))
    reasons

(* --- is_in_quiet_hours edge cases --- *)

let test_is_in_quiet_hours_wrapping () =
  (* 23..8 wraps midnight *)
  Alcotest.(check bool)
    "23 in 23..8" true
    (is_in_quiet_hours ~hour:23 ~quiet_start:23 ~quiet_end:8);
  Alcotest.(check bool)
    "0 in 23..8" true
    (is_in_quiet_hours ~hour:0 ~quiet_start:23 ~quiet_end:8);
  Alcotest.(check bool)
    "7 in 23..8" true
    (is_in_quiet_hours ~hour:7 ~quiet_start:23 ~quiet_end:8);
  Alcotest.(check bool)
    "8 not in 23..8" false
    (is_in_quiet_hours ~hour:8 ~quiet_start:23 ~quiet_end:8);
  Alcotest.(check bool)
    "22 not in 23..8" false
    (is_in_quiet_hours ~hour:22 ~quiet_start:23 ~quiet_end:8)

let test_is_in_quiet_hours_non_wrapping () =
  (* 10..15 does not wrap *)
  Alcotest.(check bool)
    "9 not in 10..15" false
    (is_in_quiet_hours ~hour:9 ~quiet_start:10 ~quiet_end:15);
  Alcotest.(check bool)
    "10 in 10..15" true
    (is_in_quiet_hours ~hour:10 ~quiet_start:10 ~quiet_end:15);
  Alcotest.(check bool)
    "14 in 10..15" true
    (is_in_quiet_hours ~hour:14 ~quiet_start:10 ~quiet_end:15);
  Alcotest.(check bool)
    "15 not in 10..15" false
    (is_in_quiet_hours ~hour:15 ~quiet_start:10 ~quiet_end:15)

let suite =
  [
    (* Ambient opt-in *)
    Alcotest.test_case "ambient mode defaults off" `Quick test_defaults_off;
    Alcotest.test_case "explicit opt-in required" `Quick test_explicit_opt_in;
    (* Quiet hours *)
    Alcotest.test_case "quiet hours default range" `Quick
      test_quiet_hours_default_range;
    Alcotest.test_case "quiet hours same start=end disables" `Quick
      test_quiet_hours_same_start_end_disables;
    Alcotest.test_case "quiet hours non-wrapping range" `Quick
      test_quiet_hours_non_wrapping_range;
    Alcotest.test_case "check_all respects quiet hours" `Quick
      test_quiet_hours_check_all_respects_it;
    (* Rate limits *)
    Alcotest.test_case "rate limit zero means unlimited" `Quick
      test_rate_limit_zero_means_unlimited;
    Alcotest.test_case "rate limit under limit" `Quick
      test_rate_limit_under_limit;
    Alcotest.test_case "rate limit at limit" `Quick test_rate_limit_at_limit;
    Alcotest.test_case "rate limit over limit" `Quick test_rate_limit_over_limit;
    Alcotest.test_case "check_all respects rate limit" `Quick
      test_rate_limit_check_all_respects_it;
    (* Budget gate *)
    Alcotest.test_case "budget exceeded" `Quick test_budget_exceeded;
    Alcotest.test_case "budget not exceeded" `Quick test_budget_not_exceeded;
    Alcotest.test_case "check_all respects budget" `Quick
      test_budget_check_all_respects_it;
    (* Connector capability *)
    Alcotest.test_case "connector unsupported" `Quick test_connector_unsupported;
    Alcotest.test_case "connector supported" `Quick test_connector_supported;
    Alcotest.test_case "check_all respects connector" `Quick
      test_connector_check_all_respects_it;
    (* Priority order *)
    Alcotest.test_case "check_all priority order" `Quick
      test_check_all_priority_order;
    (* reason_to_string *)
    Alcotest.test_case "reason_to_string non-empty" `Quick test_reason_to_string;
    (* is_in_quiet_hours edge cases *)
    Alcotest.test_case "is_in_quiet_hours wrapping" `Quick
      test_is_in_quiet_hours_wrapping;
    Alcotest.test_case "is_in_quiet_hours non-wrapping" `Quick
      test_is_in_quiet_hours_non_wrapping;
  ]

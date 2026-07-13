(** Tests for GitHub attribution requirements and risk-tier defaults
    (P21.M3.E2.T001 / T004). *)

module P = Github_attribution_policy

let find_action name (reqs : P.requirement list) =
  List.find_opt (fun (r : P.requirement) -> r.action = name) reqs

let expect ~action ~tier ~attribution ~pilot_allowed (r : P.requirement) =
  Alcotest.(check string) (action ^ " action") action r.action;
  Alcotest.(check string)
    (action ^ " tier")
    (P.risk_tier_to_string tier)
    (P.risk_tier_to_string r.tier);
  Alcotest.(check string)
    (action ^ " attribution")
    (P.attribution_to_string attribution)
    (P.attribution_to_string r.attribution);
  Alcotest.(check bool)
    (action ^ " pilot_allowed")
    pilot_allowed r.pilot_allowed

let test_defaults_cover_required_actions () =
  let reqs = P.defaults () in
  let expected =
    [
      "comment";
      "label";
      "assign";
      "review_request";
      "review_submit";
      "issue_create";
      "issue_close";
      "issue_reopen";
      "code_change";
      "workflow_dispatch";
      "merge";
    ]
  in
  List.iter
    (fun name ->
      match find_action name reqs with
      | Some _ -> ()
      | None -> Alcotest.fail (Printf.sprintf "defaults missing action %S" name))
    expected;
  Alcotest.(check int)
    "defaults length" (List.length expected) (List.length reqs)

let test_defaults_tiers_and_attribution () =
  let reqs = P.defaults () in
  let check name ~tier ~attribution ~pilot_allowed =
    match find_action name reqs with
    | None -> Alcotest.fail ("missing " ^ name)
    | Some r -> expect ~action:name ~tier ~attribution ~pilot_allowed r
  in
  check "comment" ~tier:P.Low ~attribution:P.User_preferred ~pilot_allowed:false;
  check "label" ~tier:P.Medium ~attribution:P.User_preferred
    ~pilot_allowed:false;
  check "assign" ~tier:P.Medium ~attribution:P.User_preferred
    ~pilot_allowed:false;
  check "review_request" ~tier:P.Medium ~attribution:P.User_preferred
    ~pilot_allowed:false;
  check "review_submit" ~tier:P.High ~attribution:P.User_required
    ~pilot_allowed:true;
  check "issue_create" ~tier:P.High ~attribution:P.User_required
    ~pilot_allowed:true;
  check "issue_close" ~tier:P.High ~attribution:P.User_required
    ~pilot_allowed:true;
  check "issue_reopen" ~tier:P.High ~attribution:P.User_required
    ~pilot_allowed:true;
  check "code_change" ~tier:P.High ~attribution:P.User_required
    ~pilot_allowed:true;
  check "workflow_dispatch" ~tier:P.Critical ~attribution:P.User_required
    ~pilot_allowed:true;
  check "merge" ~tier:P.Critical ~attribution:P.User_required
    ~pilot_allowed:true

let test_lookup_user_required_high_critical () =
  List.iter
    (fun (action, tier) ->
      let r = P.lookup ~action in
      expect ~action ~tier ~attribution:P.User_required ~pilot_allowed:true r)
    [
      ("merge", P.Critical);
      ("review_submit", P.High);
      ("issue_create", P.High);
      ("issue_close", P.High);
      ("issue_reopen", P.High);
      ("code_change", P.High);
      ("workflow_dispatch", P.Critical);
    ]

let test_lookup_user_preferred_low_medium () =
  let c = P.lookup ~action:"comment" in
  expect ~action:"comment" ~tier:P.Low ~attribution:P.User_preferred
    ~pilot_allowed:false c;
  let l = P.lookup ~action:"label" in
  expect ~action:"label" ~tier:P.Medium ~attribution:P.User_preferred
    ~pilot_allowed:false l;
  let a = P.lookup ~action:"assign" in
  expect ~action:"assign" ~tier:P.Medium ~attribution:P.User_preferred
    ~pilot_allowed:false a;
  let a2 = P.lookup ~action:"collab_assign" in
  expect ~action:"assign" ~tier:P.Medium ~attribution:P.User_preferred
    ~pilot_allowed:false a2;
  let rr = P.lookup ~action:"review_request" in
  expect ~action:"review_request" ~tier:P.Medium ~attribution:P.User_preferred
    ~pilot_allowed:false rr

let test_lookup_normalizes_case_and_aliases () =
  let r = P.lookup ~action:"  MERGE  " in
  expect ~action:"merge" ~tier:P.Critical ~attribution:P.User_required
    ~pilot_allowed:true r;
  let rev = P.lookup ~action:"submit_review" in
  expect ~action:"review_submit" ~tier:P.High ~attribution:P.User_required
    ~pilot_allowed:true rev;
  let req_rev = P.lookup ~action:"request_reviewers" in
  expect ~action:"review_request" ~tier:P.Medium ~attribution:P.User_preferred
    ~pilot_allowed:false req_rev;
  let code = P.lookup ~action:"code_work" in
  expect ~action:"code_change" ~tier:P.High ~attribution:P.User_required
    ~pilot_allowed:true code;
  let comment = P.lookup ~action:"collab_comment" in
  expect ~action:"comment" ~tier:P.Low ~attribution:P.User_preferred
    ~pilot_allowed:false comment;
  let open_alias = P.lookup ~action:"issue_open" in
  expect ~action:"issue_create" ~tier:P.High ~attribution:P.User_required
    ~pilot_allowed:true open_alias;
  let close_alias = P.lookup ~action:"close_issue" in
  expect ~action:"issue_close" ~tier:P.High ~attribution:P.User_required
    ~pilot_allowed:true close_alias

let test_lookup_unknown_fails_closed () =
  let r = P.lookup ~action:"totally_unknown_mutation" in
  expect ~action:"totally_unknown_mutation" ~tier:P.Critical
    ~attribution:P.User_required ~pilot_allowed:false r;
  let empty = P.lookup ~action:"   " in
  expect ~action:"" ~tier:P.Critical ~attribution:P.User_required
    ~pilot_allowed:false empty

let test_string_helpers () =
  Alcotest.(check string) "low" "low" (P.risk_tier_to_string P.Low);
  Alcotest.(check string) "medium" "medium" (P.risk_tier_to_string P.Medium);
  Alcotest.(check string) "high" "high" (P.risk_tier_to_string P.High);
  Alcotest.(check string)
    "critical" "critical"
    (P.risk_tier_to_string P.Critical);
  Alcotest.(check string)
    "app" "app_installation"
    (P.attribution_to_string P.App_installation);
  Alcotest.(check string)
    "user" "user_required"
    (P.attribution_to_string P.User_required);
  Alcotest.(check string)
    "preferred" "user_preferred"
    (P.attribution_to_string P.User_preferred);
  Alcotest.(check string)
    "pat" "pat_compat"
    (P.attribution_to_string P.Pat_compat)

let test_permits_app_fallback () =
  Alcotest.(check bool)
    "preferred permits" true
    (P.permits_app_fallback P.User_preferred);
  Alcotest.(check bool)
    "required forbids" false
    (P.permits_app_fallback P.User_required);
  Alcotest.(check bool)
    "app forbids as fallback" false
    (P.permits_app_fallback P.App_installation)

let suite =
  [
    Alcotest.test_case "defaults cover required actions" `Quick
      test_defaults_cover_required_actions;
    Alcotest.test_case "defaults tiers and attribution" `Quick
      test_defaults_tiers_and_attribution;
    Alcotest.test_case "lookup User_required High/Critical actions" `Quick
      test_lookup_user_required_high_critical;
    Alcotest.test_case "lookup User_preferred Low/Medium actions" `Quick
      test_lookup_user_preferred_low_medium;
    Alcotest.test_case "lookup normalizes case and aliases" `Quick
      test_lookup_normalizes_case_and_aliases;
    Alcotest.test_case "lookup unknown fails closed User_required Critical"
      `Quick test_lookup_unknown_fails_closed;
    Alcotest.test_case "string helpers" `Quick test_string_helpers;
    Alcotest.test_case "permits_app_fallback" `Quick test_permits_app_fallback;
  ]

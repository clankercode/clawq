(** Tests for independently gated fresh-confirmed merge with live policy checks
    (P19.M4.E2.T003). *)

module S = Github_route_store
module M = Github_merge_action
module W = Github_action_workflow

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Setup_plan_apply.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let base_revision = "rev-config-1"
let room_id = "room-teams-1"
let room = S.Room room_id
let item_key = "item:acme/widget:pr:42"
let head_sha = "abc123def4567890abcdef1234567890abcdef12"

let principal =
  Setup_plan.{ id = "principal:alice"; kind = Principal; label = Some "Alice" }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let contains hay needle =
  let hay = String.lowercase_ascii hay in
  let needle = String.lowercase_ascii needle in
  let n = String.length needle in
  let h = String.length hay in
  if n = 0 then true
  else if n > h then false
  else
    let rec loop i =
      if i > h - n then false
      else if String.sub hay i n = needle then true
      else loop (i + 1)
    in
    loop 0

let caps ~merge ~review ~reply : S.capability_policy =
  {
    allow_reply = reply;
    allow_label = false;
    allow_assign = false;
    allow_review = review;
    allow_merge = merge;
    allow_close = false;
    extra = [];
  }

let make_route ~id ~policy : S.t =
  {
    id;
    destination = room;
    selector = S.Repo "acme/widget";
    filter = S.default_filter;
    comment_mode = S.default_comment_mode;
    capability_policy = policy;
    enabled = true;
    revision = "1";
    managed_bundle_id = None;
    managed_feature_id = None;
    provenance =
      {
        created_by = Some "test";
        created_via = Some "test";
        setup_plan_id = None;
        notes = None;
      };
    created_at = "2024-01-01T00:00:00Z";
    updated_at = "2024-01-01T00:00:00Z";
  }

let pilot_on =
  {
    M.enabled = true;
    pilot_name = "p19-merge-pilot";
    expires_at = Some "2099-01-01T00:00:00Z";
  }

let pilot_off = M.default_pilot_gate

let ok_policy ?(head = head_sha) ?(draft = false) ?(mergeable = true)
    ?(checks = true) ?(reviews = true) ?(branch = true)
    ?(methods = [ M.Merge; M.Squash; M.Rebase ]) ?(actor = M.App)
    ?(authority = true) () : M.live_policy =
  {
    head_sha = head;
    is_draft = draft;
    mergeable;
    required_checks_ok = checks;
    required_reviews_ok = reviews;
    branch_policy_ok = branch;
    allowed_methods = methods;
    actor_mode = actor;
    authority_ok = authority;
  }

let sample_req ?(head = head_sha) ?(method_ = M.Squash) () : M.merge_request =
  {
    item_key;
    method_;
    head_sha = head;
    commit_title = Some "Merge PR #42";
    commit_message = None;
  }

(* 1. gate off deny (independent pilot; no App/PAT fallback) *)
let test_gate_off_deny () =
  let route =
    make_route ~id:"rt_merge_gate_off"
      ~policy:(caps ~merge:true ~review:true ~reply:true)
  in
  let req = sample_req () in
  let policy = ok_policy () in
  match
    M.authorize_merge ~route:(Some route) ~pilot:pilot_off
      ~user_auth_available:false ~req ~policy ~now:fixed_now ()
  with
  | Ok () -> Alcotest.fail "expected deny when merge pilot off"
  | Error msg ->
      Alcotest.(check bool) "mentions pilot" true (contains msg "pilot");
      Alcotest.(check bool)
        "not production-ready" true
        (contains msg "production");
      Alcotest.(check bool)
        "no App/PAT fallback" true
        (contains msg "fallback" || contains msg "user");
      Alcotest.(check bool) "independent merge gate" true (contains msg "merge")

(* 2. write/review alone does not grant merge *)
let test_write_review_do_not_grant_merge () =
  let route_review =
    make_route ~id:"rt_review_only"
      ~policy:(caps ~merge:false ~review:true ~reply:true)
  in
  let req = sample_req () in
  let policy = ok_policy () in
  match
    M.authorize_merge ~route:(Some route_review) ~pilot:pilot_on
      ~user_auth_available:false ~req ~policy ~now:fixed_now ()
  with
  | Ok () -> Alcotest.fail "expected deny without allow_merge"
  | Error msg ->
      Alcotest.(check bool)
        "mentions allow_merge" true
        (contains msg "allow_merge");
      Alcotest.(check bool)
        "mentions independent" true
        (contains msg "independent" || contains msg "write"
       || contains msg "review")

(* 3. gate on + allow_merge + live policy allows plan *)
let test_gate_on_capability_allows_plan () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_merge_on"
      ~policy:(caps ~merge:true ~review:false ~reply:false)
  in
  let req = sample_req () in
  let policy = ok_policy () in
  match
    M.authorize_merge ~route:(Some route) ~pilot:pilot_on
      ~user_auth_available:false ~req ~policy ~now:fixed_now ()
  with
  | Error e -> Alcotest.fail ("unexpected authorize deny: " ^ e)
  | Ok () ->
      let plan =
        assert_ok
          (M.plan_merge ~db ~principal ~room_id ~pilot:pilot_on
             ~user_auth_available:false ~req ~policy ~base_revision ~route
             ~now:fixed_now ())
      in
      (match plan.apply_payload.kind with
      | Setup_plan.Generic "github_merge" -> ()
      | Setup_plan.Generic other ->
          Alcotest.fail ("unexpected generic kind: " ^ other)
      | _ -> Alcotest.fail "expected Generic github_merge");
      Alcotest.(check bool) "readiness ok" true (Setup_plan.readiness_ok plan);
      Alcotest.(check bool) "is_merge_plan" true (M.is_merge_plan plan);
      Alcotest.(check string) "base_revision" base_revision plan.base_revision;
      (* Workflow path too. *)
      let plan2 =
        assert_ok
          (W.preview ~db ~principal ~room_id
             ~action:(W.Merge { req; policy })
             ~base_revision ~route ~merge_pilot:pilot_on ~now:(fixed_now +. 1.)
             ())
      in
      Alcotest.(check bool)
        "workflow is github action" true
        (W.is_github_action_plan plan2);
      Alcotest.(check string)
        "label" "merge"
        (W.action_kind_label (W.Merge { req; policy }))

(* 4. empty head_sha denied *)
let test_empty_head_sha_denied () =
  let route =
    make_route ~id:"rt_empty_sha"
      ~policy:(caps ~merge:true ~review:false ~reply:false)
  in
  let req = sample_req ~head:"   " () in
  let policy = ok_policy () in
  match
    M.authorize_merge ~route:(Some route) ~pilot:pilot_on
      ~user_auth_available:false ~req ~policy ~now:fixed_now ()
  with
  | Ok () -> Alcotest.fail "expected deny for empty head_sha"
  | Error msg ->
      Alcotest.(check bool) "mentions head_sha" true (contains msg "head_sha")

(* 5. live policy failures (draft / not mergeable / checks) *)
let test_live_policy_failures () =
  let route =
    make_route ~id:"rt_policy"
      ~policy:(caps ~merge:true ~review:false ~reply:false)
  in
  let req = sample_req () in
  let deny policy needle =
    match
      M.authorize_merge ~route:(Some route) ~pilot:pilot_on
        ~user_auth_available:false ~req ~policy ~now:fixed_now ()
    with
    | Ok () -> Alcotest.fail ("expected deny for " ^ needle)
    | Error msg ->
        Alcotest.(check bool) ("mentions " ^ needle) true (contains msg needle)
  in
  deny (ok_policy ~draft:true ()) "draft";
  deny (ok_policy ~mergeable:false ()) "mergeable";
  deny (ok_policy ~checks:false ()) "check";
  deny (ok_policy ~reviews:false ()) "review";
  deny (ok_policy ~branch:false ()) "branch";
  deny (ok_policy ~methods:[ M.Merge ] ~head:head_sha ()) "method";
  deny (ok_policy ~authority:false ()) "authority";
  (* head mismatch between request and live policy *)
  deny (ok_policy ~head:"ffffffffffffffffffffffffffffffffffffffff" ()) "head"

(* 6. stale revision fails on apply (no attempt) *)
let test_stale_revision_fail () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_stale"
      ~policy:(caps ~merge:true ~review:false ~reply:false)
  in
  let req = sample_req () in
  let policy = ok_policy () in
  let plan =
    assert_ok
      (M.plan_merge ~db ~principal ~room_id ~pilot:pilot_on
         ~user_auth_available:false ~req ~policy ~base_revision ~route
         ~now:fixed_now ())
  in
  match
    assert_ok
      (M.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest ~principal
         ~current_base_revision:"rev-stale" ~current_policy:policy
         ~now:fixed_now ())
  with
  | Setup_plan_apply.Rejected { reason; _ } ->
      Alcotest.(check string)
        "stale" "stale_revision"
        (Setup_plan_apply.string_of_reject_reason reason)
  | Setup_plan_apply.Applied _ -> Alcotest.fail "expected stale revision reject"

(* 7. revalidation fails when head changed after plan *)
let test_revalidate_changed_head_no_attempt () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_reval"
      ~policy:(caps ~merge:true ~review:false ~reply:false)
  in
  let req = sample_req () in
  let policy = ok_policy () in
  let plan =
    assert_ok
      (M.plan_merge ~db ~principal ~room_id ~pilot:pilot_on
         ~user_auth_available:false ~req ~policy ~base_revision ~route
         ~now:fixed_now ())
  in
  let stale_policy =
    ok_policy ~head:"deadbeefdeadbeefdeadbeefdeadbeefdeadbeef" ()
  in
  match
    assert_ok
      (M.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest ~principal
         ~current_base_revision:base_revision ~current_policy:stale_policy
         ~now:fixed_now ())
  with
  | Setup_plan_apply.Applied _ ->
      Alcotest.fail "expected revalidation reject when head changed"
  | Setup_plan_apply.Rejected { message; _ } ->
      Alcotest.(check bool)
        "revalidation failed" true
        (contains message "revalidation"
        || contains message "head"
        || contains message "prerequisite");
      Alcotest.(check bool)
        "no attempt language" true
        (contains message "no attempt" || contains message "failed")

(* 8. secret-free plan (no token material) + apply success with revalidate *)
let test_secret_free_plan_and_apply () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_secret"
      ~policy:(caps ~merge:true ~review:false ~reply:false)
  in
  let req = sample_req () in
  let policy = ok_policy () in
  let plan =
    assert_ok
      (M.plan_merge ~db ~principal ~room_id ~pilot:pilot_on
         ~user_auth_available:false ~req ~policy ~base_revision ~route
         ~now:fixed_now ())
  in
  let persist = Yojson.Safe.to_string (Setup_plan.to_persist_json plan) in
  let render = Yojson.Safe.to_string (Setup_plan.to_render_json plan) in
  Alcotest.(check bool) "includes head_sha" true (contains persist head_sha);
  Alcotest.(check bool) "includes squash" true (contains persist "squash");
  Alcotest.(check bool)
    "includes pilot name" true
    (contains persist "p19-merge-pilot");
  Alcotest.(check bool)
    "includes allow_merge" true
    (contains persist "allow_merge");
  Alcotest.(check bool)
    "not production-ready" true
    (contains persist "production_ready");
  Alcotest.(check bool)
    "no token-like secret keys in persist" false
    (contains persist "bot_token"
    || contains persist "signing_secret"
    || contains persist "api_key"
    || contains persist "private_key"
    || contains persist "ghp_"
    || contains persist "github_pat_");
  Alcotest.(check bool)
    "no token-like secret keys in render" false
    (contains render "ghp_" || contains render "bearer ");
  match
    assert_ok
      (M.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest ~principal
         ~current_base_revision:base_revision ~current_policy:policy
         ~now:fixed_now ())
  with
  | Setup_plan_apply.Applied { first_time = true; receipt_id } ->
      Alcotest.(check bool)
        "receipt non-empty" true
        (String.length receipt_id > 0)
  | Setup_plan_apply.Applied { first_time = false; _ } ->
      Alcotest.fail "expected first-time apply"
  | Setup_plan_apply.Rejected { message; _ } -> Alcotest.fail message

(* 9. receipt_safe_error redacts token *)
let test_receipt_safe_error_redacts_token () =
  let raw =
    "GitHub rejected merge: Authorization: Bearer \
     ghp_SUPERSECRETtokenvalue123456 projection failed token=abc123secret"
  in
  let safe = M.receipt_safe_error raw in
  Alcotest.(check bool)
    "redacts ghp_ token" false
    (contains safe "ghp_SUPERSECRETtokenvalue123456");
  Alcotest.(check bool)
    "redacts token= value" false
    (contains safe "abc123secret");
  let plain = M.receipt_safe_error "projection failed: head mismatch" in
  Alcotest.(check bool)
    "plain error preserved" true
    (contains plain "head mismatch")

let suite =
  [
    ("gate off deny", `Quick, test_gate_off_deny);
    ( "write/review do not grant merge",
      `Quick,
      test_write_review_do_not_grant_merge );
    ( "gate on + capability allows plan",
      `Quick,
      test_gate_on_capability_allows_plan );
    ("empty head_sha denied", `Quick, test_empty_head_sha_denied);
    ("live policy failures", `Quick, test_live_policy_failures);
    ("stale revision fail", `Quick, test_stale_revision_fail);
    ( "revalidate changed head no attempt",
      `Quick,
      test_revalidate_changed_head_no_attempt );
    ("secret-free plan and apply", `Quick, test_secret_free_plan_and_apply);
    ( "receipt_safe_error redacts token",
      `Quick,
      test_receipt_safe_error_redacts_token );
  ]

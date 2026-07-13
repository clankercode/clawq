(** Tests for confirmed code-changing work and constrained PR creation
    (P19.M4.E2.T007). *)

module S = Github_route_store
module A = Github_code_change_action

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Setup_plan_apply.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let base_revision = "rev-config-1"
let room_id = "room-teams-1"
let room = S.Room room_id
let repo = "acme/widget"
let base_branch = "main"
let head_branch = "clawq/wi-42-fix-login"
let head_sha = "abc123def4567890abcdef1234567890abcdef12"
let title = "Fix login redirect after SSO"

let principal =
  Setup_plan.{ id = "principal:alice"; kind = Principal; label = Some "Alice" }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let caps ?(code_change = false) () : S.capability_policy =
  {
    allow_reply = false;
    allow_label = false;
    allow_assign = false;
    allow_review = false;
    allow_merge = false;
    allow_close = false;
    extra =
      (if code_change then [ (A.capability_key, true) ]
       else [ (A.capability_key, false) ]);
  }

let make_route ~id ~policy : S.t =
  {
    id;
    destination = room;
    selector = S.Repo repo;
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

let pilot_on =
  {
    A.enabled = true;
    pilot_name = "p19-code-change-pilot";
    expires_at = Some "2099-01-01T00:00:00Z";
  }

let pilot_off = A.default_pilot_gate

let pilot_expired =
  {
    A.enabled = true;
    pilot_name = "p19-code-change-pilot";
    expires_at = Some "2020-01-01T00:00:00Z";
  }

let code_work_req ?(scope = "fix SSO redirect in auth middleware")
    ?(runner = "codex") ?(output_authority = "room:room-teams-1")
    ?(head = Some head_branch) () : A.code_work_request =
  {
    repo_full_name = repo;
    base_branch;
    scope;
    runner;
    output_authority;
    branch_prefix = A.default_branch_prefix;
    head_branch = head;
    item_key = Some "item:acme/widget:issue:9";
    related_issue = Some 9;
  }

let pr_req_explicit ?(title = title) ?(head = head_branch) () :
    A.pr_create_request =
  {
    repo_full_name = repo;
    base_branch;
    title;
    body = Some "Automated draft from confirmed code work.";
    draft = true;
    head = A.Explicit_branch head;
    branch_prefix = A.default_branch_prefix;
    head_sha = Some head_sha;
    item_key = Some "item:acme/widget:issue:9";
  }

let pr_req_from_work ?(status = A.Succeeded)
    ?(finished_at = Some "2023-11-14T22:13:20Z") () : A.pr_create_request =
  {
    repo_full_name = repo;
    base_branch;
    title;
    body = None;
    draft = true;
    head =
      A.Confirmed_code_work
        {
          code_work_plan_id = "plan-code-work-1";
          head_branch;
          head_sha;
          status;
          finished_at;
        };
    branch_prefix = A.default_branch_prefix;
    head_sha = Some head_sha;
    item_key = None;
  }

let live_ok =
  {
    A.head_branch;
    base_branch;
    head_sha;
    base_sha = Some "baseaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
    head_exists = true;
    base_exists = true;
  }

(* 1. code work authorized with pilot + capability *)
let test_code_work_authorized () =
  let route = make_route ~id:"rt_cw_on" ~policy:(caps ~code_change:true ()) in
  match
    A.authorize_code_work ~route:(Some route) ~pilot:pilot_on
      ~user_auth_available:false ~req:(code_work_req ()) ~now:fixed_now ()
  with
  | Ok () -> ()
  | Error e -> Alcotest.fail ("unexpected deny: " ^ e)

(* 2. deny without capability / without route *)
let test_deny_without_capability () =
  let route = make_route ~id:"rt_cw_off" ~policy:(caps ~code_change:false ()) in
  (match
     A.authorize_code_work ~route:(Some route) ~pilot:pilot_on
       ~user_auth_available:false ~req:(code_work_req ()) ~now:fixed_now ()
   with
  | Ok () -> Alcotest.fail "expected deny without code_change capability"
  | Error msg ->
      Alcotest.(check bool)
        "mentions code_change" true
        (contains msg "code_change"));
  match
    A.authorize_pr_create ~route:None ~pilot:pilot_on ~user_auth_available:false
      ~req:(pr_req_explicit ()) ~now:fixed_now ()
  with
  | Ok () -> Alcotest.fail "expected deny without route"
  | Error msg ->
      Alcotest.(check bool) "mentions no route" true (contains msg "no route")

(* 3. deny when pilot / high-risk gate off *)
let test_deny_when_gate_off () =
  let route =
    make_route ~id:"rt_gate_off" ~policy:(caps ~code_change:true ())
  in
  (match
     A.authorize_code_work ~route:(Some route) ~pilot:pilot_off
       ~user_auth_available:false ~req:(code_work_req ()) ~now:fixed_now ()
   with
  | Ok () -> Alcotest.fail "expected deny when pilot off"
  | Error msg ->
      Alcotest.(check bool) "mentions pilot" true (contains msg "pilot");
      Alcotest.(check bool)
        "not production-ready" true
        (contains msg "production");
      Alcotest.(check bool)
        "no App/PAT fallback" true
        (contains msg "fallback" || contains msg "user"));
  match
    A.authorize_pr_create ~route:(Some route) ~pilot:pilot_expired
      ~user_auth_available:false ~req:(pr_req_explicit ()) ~now:fixed_now ()
  with
  | Ok () -> Alcotest.fail "expected deny when pilot expired"
  | Error msg ->
      Alcotest.(check bool) "mentions expired" true (contains msg "expired")

(* 4. plan code work names repo/base/scope/runner/authority; secret-free *)
let test_plan_code_work_names_required_fields () =
  with_db @@ fun db ->
  let route = make_route ~id:"rt_plan_cw" ~policy:(caps ~code_change:true ()) in
  let req = code_work_req () in
  let plan =
    assert_ok
      (A.plan_code_work ~db ~principal ~room_id ~pilot:pilot_on
         ~user_auth_available:false ~req ~base_revision ~route ~now:fixed_now ())
  in
  (match plan.apply_payload.kind with
  | Setup_plan.Generic "github_code_work" -> ()
  | Setup_plan.Generic other -> Alcotest.fail ("unexpected kind: " ^ other)
  | _ -> Alcotest.fail "expected Generic github_code_work");
  Alcotest.(check bool) "readiness ok" true (Setup_plan.readiness_ok plan);
  let persist = Yojson.Safe.to_string (Setup_plan.to_persist_json plan) in
  Alcotest.(check bool) "repo" true (contains persist repo);
  Alcotest.(check bool) "base" true (contains persist base_branch);
  Alcotest.(check bool) "scope" true (contains persist "sso");
  Alcotest.(check bool) "runner" true (contains persist "codex");
  Alcotest.(check bool)
    "output_authority" true
    (contains persist "room:room-teams-1");
  Alcotest.(check bool) "branch_prefix" true (contains persist "clawq/");
  Alcotest.(check bool) "pilot" true (contains persist "p19-code-change-pilot");
  Alcotest.(check bool)
    "not production-ready" true
    (contains persist "production_ready");
  Alcotest.(check bool)
    "webhook correlation" true
    (contains persist "webhook_correlation");
  Alcotest.(check bool)
    "no secret shapes" false
    (contains persist "ghp_"
    || contains persist "bot_token"
    || contains persist "api_key"
    || contains persist "private_key")

(* 5. plan PR create with constraints (title, branch prefix, base) *)
let test_plan_pr_create_with_constraints () =
  with_db @@ fun db ->
  let route = make_route ~id:"rt_plan_pr" ~policy:(caps ~code_change:true ()) in
  let req = pr_req_explicit () in
  let plan =
    assert_ok
      (A.plan_pr_create ~db ~principal ~room_id ~pilot:pilot_on
         ~user_auth_available:false ~req ~base_revision ~route ~now:fixed_now ())
  in
  (match plan.apply_payload.kind with
  | Setup_plan.Generic "github_pr_create" -> ()
  | Setup_plan.Generic other -> Alcotest.fail ("unexpected kind: " ^ other)
  | _ -> Alcotest.fail "expected Generic github_pr_create");
  let persist = Yojson.Safe.to_string (Setup_plan.to_persist_json plan) in
  Alcotest.(check bool) "title" true (contains persist title);
  Alcotest.(check bool) "head" true (contains persist head_branch);
  Alcotest.(check bool) "base" true (contains persist base_branch);
  Alcotest.(check bool) "draft" true (contains persist "draft");
  Alcotest.(check bool)
    "explicit head source" true
    (contains persist "explicit_branch");
  Alcotest.(check bool) "prefix constraint" true (contains persist "clawq/");
  (* Title required *)
  let no_title = pr_req_explicit ~title:"   " () in
  (match
     A.plan_pr_create ~db ~principal ~room_id ~pilot:pilot_on
       ~user_auth_available:false ~req:no_title ~base_revision ~route
       ~now:(fixed_now +. 1.) ()
   with
  | Ok _ -> Alcotest.fail "expected deny for empty title"
  | Error msg ->
      Alcotest.(check bool) "mentions title" true (contains msg "title"));
  (* Branch naming: reject outside prefix *)
  let bad_branch = pr_req_explicit ~head:"feature/unconstrained" () in
  match
    A.authorize_pr_create ~route:(Some route) ~pilot:pilot_on
      ~user_auth_available:false ~req:bad_branch ~now:fixed_now ()
  with
  | Ok () -> Alcotest.fail "expected deny for branch outside prefix"
  | Error msg ->
      Alcotest.(check bool) "mentions prefix" true (contains msg "prefix")

(* 6. PR from confirmed code-work result; reject cancelled / failed / stale *)
let test_pr_from_confirmed_result_and_failures () =
  let route = make_route ~id:"rt_result" ~policy:(caps ~code_change:true ()) in
  (* Succeeded confirmed result allowed *)
  (match
     A.authorize_pr_create ~route:(Some route) ~pilot:pilot_on
       ~user_auth_available:false ~req:(pr_req_from_work ()) ~now:fixed_now ()
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail ("unexpected deny succeeded: " ^ e));
  (* Cancelled independent path *)
  (match
     A.authorize_pr_create ~route:(Some route) ~pilot:pilot_on
       ~user_auth_available:false
       ~req:(pr_req_from_work ~status:A.Cancelled ())
       ~now:fixed_now ()
   with
  | Ok () -> Alcotest.fail "expected deny cancelled"
  | Error msg -> Alcotest.(check bool) "cancelled" true (contains msg "cancel"));
  (* Runner failure independent path *)
  (match
     A.authorize_pr_create ~route:(Some route) ~pilot:pilot_on
       ~user_auth_available:false
       ~req:(pr_req_from_work ~status:A.Failed ())
       ~now:fixed_now ()
   with
  | Ok () -> Alcotest.fail "expected deny failed"
  | Error msg ->
      Alcotest.(check bool)
        "runner failure" true
        (contains msg "fail" || contains msg "runner"));
  let fail_msg =
    A.runner_failure_message ~runner:"codex"
      ~detail:"exit 1 token=ghp_SECRETvalue1234567890"
  in
  Alcotest.(check bool)
    "runner failure mentions runner" true
    (contains fail_msg "codex");
  Alcotest.(check bool)
    "runner failure redacts ghp" false
    (contains fail_msg "ghp_secretvalue1234567890");
  (* Stale result independent path *)
  match
    A.check_code_work_result_usable ~result_status:A.Succeeded
      ~finished_at:"2020-01-01T00:00:00Z" ~max_age_seconds:3600. ~now:fixed_now
      ()
  with
  | Ok () -> Alcotest.fail "expected stale"
  | Error msg -> Alcotest.(check bool) "stale" true (contains msg "stale")

(* 7. revalidate head/base before dispatch *)
let test_revalidate_head_base () =
  (match
     A.revalidate_pr_refs ~planned_head:head_branch ~planned_base:base_branch
       ~planned_head_sha:head_sha ~current:live_ok ()
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  let moved =
    { live_ok with head_sha = "ffffffffffffffffffffffffffffffffffffffff" }
  in
  (match
     A.revalidate_pr_refs ~planned_head:head_branch ~planned_base:base_branch
       ~planned_head_sha:head_sha ~current:moved ()
   with
  | Ok () -> Alcotest.fail "expected sha mismatch"
  | Error msg -> Alcotest.(check bool) "head_sha" true (contains msg "head_sha"));
  let missing = { live_ok with head_exists = false } in
  match
    A.revalidate_pr_refs ~planned_head:head_branch ~planned_base:base_branch
      ~current:missing ()
  with
  | Ok () -> Alcotest.fail "expected missing head"
  | Error msg ->
      Alcotest.(check bool)
        "does not exist" true
        (contains msg "does not exist")

(* 8. plan-confirm-apply receipt; duplicate invocation; stale revision *)
let test_apply_receipt_duplicate_and_stale_revision () =
  with_db @@ fun db ->
  let route = make_route ~id:"rt_apply" ~policy:(caps ~code_change:true ()) in
  let plan =
    assert_ok
      (A.plan_pr_create ~db ~principal ~room_id ~pilot:pilot_on
         ~user_auth_available:false ~req:(pr_req_explicit ()) ~base_revision
         ~route ~now:fixed_now ())
  in
  Alcotest.(check bool) "is code-change plan" true (A.is_code_change_plan plan);
  let outcome =
    assert_ok
      (A.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest ~principal
         ~current_base_revision:base_revision ~current_refs:live_ok
         ~now:(fixed_now +. 1.) ())
  in
  let receipt_id =
    match outcome with
    | Setup_plan_apply.Applied { receipt_id; first_time = true } -> receipt_id
    | Setup_plan_apply.Applied { first_time = false; _ } ->
        Alcotest.fail "expected first-time apply"
    | Setup_plan_apply.Rejected { message; _ } -> Alcotest.fail message
  in
  Alcotest.(check bool) "receipt non-empty" true (String.length receipt_id > 0);
  (* Idempotent replay is ok at Setup_plan_apply layer; independent helper
     denies intentional second dispatch when already applied. *)
  (match A.check_not_duplicate_invocation ~already_applied:true with
  | Ok () -> Alcotest.fail "expected duplicate deny"
  | Error msg ->
      Alcotest.(check bool) "duplicate" true (contains msg "duplicate"));
  (* Stale base_revision fails closed *)
  let plan2 =
    assert_ok
      (A.plan_code_work ~db ~principal ~room_id ~pilot:pilot_on
         ~user_auth_available:false ~req:(code_work_req ())
         ~base_revision:"rev-other" ~route ~now:(fixed_now +. 2.) ())
  in
  match
    assert_ok
      (A.apply_confirmed ~db ~plan_id:plan2.id ~digest:plan2.digest ~principal
         ~current_base_revision:"rev-stale" ~now:(fixed_now +. 3.) ())
  with
  | Setup_plan_apply.Rejected { reason; _ } ->
      Alcotest.(check string)
        "stale revision" "stale_revision"
        (Setup_plan_apply.string_of_reject_reason reason)
  | Setup_plan_apply.Applied _ -> Alcotest.fail "expected stale revision reject"

(* 9. revalidation failure rejects apply with no attempt *)
let test_apply_rejects_on_ref_mismatch () =
  with_db @@ fun db ->
  let route = make_route ~id:"rt_reval" ~policy:(caps ~code_change:true ()) in
  let plan =
    assert_ok
      (A.plan_pr_create ~db ~principal ~room_id ~pilot:pilot_on
         ~user_auth_available:false ~req:(pr_req_explicit ()) ~base_revision
         ~route ~now:fixed_now ())
  in
  let bad_refs = { live_ok with base_branch = "develop"; base_exists = true } in
  match
    assert_ok
      (A.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest ~principal
         ~current_base_revision:base_revision ~current_refs:bad_refs
         ~now:(fixed_now +. 1.) ())
  with
  | Setup_plan_apply.Rejected { message; _ } ->
      Alcotest.(check bool)
        "revalidation failed" true
        (contains message "revalidation" || contains message "base")
  | Setup_plan_apply.Applied _ ->
      Alcotest.fail "expected reject on base mismatch"

(* 10. capability defaults off + receipt_safe_error *)
let test_capability_defaults_and_receipt_redaction () =
  let empty =
    {
      S.allow_reply = false;
      allow_label = false;
      allow_assign = false;
      allow_review = false;
      allow_merge = false;
      allow_close = false;
      extra = [];
    }
  in
  Alcotest.(check bool) "absent off" false (A.has_code_change_capability empty);
  Alcotest.(check bool)
    "false off" false
    (A.has_code_change_capability
       { empty with extra = [ (A.capability_key, false) ] });
  Alcotest.(check bool)
    "true on" true
    (A.has_code_change_capability
       { empty with extra = [ (A.capability_key, true) ] });
  let raw =
    "GitHub rejected pr create: Authorization: Bearer \
     ghp_SUPERSECRETtokenvalue123456 projection failed token=abc123secret"
  in
  let safe = A.receipt_safe_error raw in
  Alcotest.(check bool)
    "redacts ghp" false
    (contains safe "ghp_SUPERSECRETtokenvalue123456");
  Alcotest.(check bool) "redacts token=" false (contains safe "abc123secret");
  (* Missing required code-work fields *)
  let route = make_route ~id:"rt_fields" ~policy:(caps ~code_change:true ()) in
  match
    A.authorize_code_work ~route:(Some route) ~pilot:pilot_on
      ~user_auth_available:true
      ~req:(code_work_req ~scope:"" ())
      ~now:fixed_now ()
  with
  | Ok () -> Alcotest.fail "expected empty scope deny"
  | Error msg -> Alcotest.(check bool) "scope" true (contains msg "scope")

let suite =
  [
    ( "code work authorized with pilot + capability",
      `Quick,
      test_code_work_authorized );
    ("deny without capability", `Quick, test_deny_without_capability);
    ("deny when high-risk gate off", `Quick, test_deny_when_gate_off);
    ( "plan code work names repo base scope runner authority secret-free",
      `Quick,
      test_plan_code_work_names_required_fields );
    ( "plan PR create with constraints title base branch prefix",
      `Quick,
      test_plan_pr_create_with_constraints );
    ( "PR from confirmed result; cancel failed stale independent",
      `Quick,
      test_pr_from_confirmed_result_and_failures );
    ("revalidate head base before dispatch", `Quick, test_revalidate_head_base);
    ( "apply receipt duplicate and stale revision",
      `Quick,
      test_apply_receipt_duplicate_and_stale_revision );
    ( "apply rejects on head/base revalidation mismatch",
      `Quick,
      test_apply_rejects_on_ref_mismatch );
    ( "capability defaults off and receipt_safe_error",
      `Quick,
      test_capability_defaults_and_receipt_redaction );
  ]

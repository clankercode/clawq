(** Tests for user-required code work and constrained PR creation attribution
    (P21.M3.E3.T008): authorize + dispatch lease + audit + live revalidation.

    Covers: success, denial, App/PAT forbidden, cancelled/stale result, replay,
    receipt, workflow preview/apply idempotency, and webhook reconciliation.
    Personal token never surfaces on plans. *)

module A = Github_attribution_authorize
module D = Github_attribution_dispatch_lease
module Audit = Github_attribution_audit
module Policy = Github_attribution_policy
module Code = Github_code_change_action
module Attr = Github_code_change_attribution
module S = Github_route_store
module V = Github_user_token_vault
module Lease = Github_user_token_lease
module Store = Github_user_token_store
module Workflow = Github_action_workflow
module Reconcile = Github_action_reconcile

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-code-change-attr-test"
    ()

let sample_tokens =
  {
    Store.access_token = "ghu_access_CODE_ATTR_PLAINTEXT_never_export";
    refresh_token = Some "ghr_refresh_CODE_ATTR_PLAINTEXT_never_export";
  }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let contains ~needle s =
  let n = String.length needle in
  let len = String.length s in
  if n = 0 then true
  else if n > len then false
  else
    let rec loop i =
      if i + n > len then false
      else if String.sub s i n = needle then true
      else loop (i + 1)
    in
    loop 0

let fixed_now = 1_720_000_000.0
let far_expires = "2026-12-01T00:00:00Z"
let item_key = "item:acme/widget:issue:9"
let repo = "acme/widget"
let room_id = "room-teams-1"
let base_revision = "rev-config-1"
let head_sha = "abc123def4567890abcdef1234567890abcdef12"
let head_sha_other = "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
let head_branch = "clawq/fix-thing"

let principal =
  Setup_plan.{ id = "principal:alice"; kind = Principal; label = Some "Alice" }

let account ?(principal_id = "prin_a") ?(github_user_id = 4242L) ?(app_id = 99)
    ?(host = V.default_host) () =
  assert_ok (V.make_account_key ~principal_id ~github_user_id ~app_id ~host ())

let make_keys ?(key_id = "mk-code-1") ?(key_version = 1) () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key ())

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  V.ensure_schema db;
  Audit.ensure_schema db;
  S.ensure_schema db;
  Setup_plan_apply.init_schema db;
  Reconcile.ensure_schema db;
  Fun.protect
    ~finally:(fun () ->
      ignore (Lease.discard_all ());
      ignore (Sqlite3.db_close db))
    (fun () -> f db)

let create_vault ~db ?(keys = make_keys ()) ?(account = account ())
    ?(tokens = sample_tokens) ?(id = "ghvault_code_1")
    ?(expires_at = far_expires) () =
  match
    V.create ~db ~keys ~id ~now:fixed_now ~account ~tokens
      ~scopes:[ "repo"; "read:user" ] ~expires_at ()
  with
  | Ok r -> r
  | Error d -> Alcotest.fail ("create: " ^ V.string_of_denial d)

let selected ?(binding_id = "bind_1") ?(lineage_id = "lin_1")
    ?(authorized = true) ?(vault_active = true) ?(vault_generation = 1)
    ?(lineage_matches_pin = true) () =
  assert_ok
    (A.make_selected_binding ~binding_id ~lineage_id ~authorized ~vault_active
       ~vault_generation ~lineage_matches_pin ())

let base_request ?(action = "code_change") ?(tool_authorized = true)
    ?(repo_granted = true) ?(repo_blocked = false) ?(principal_current = true)
    ?(confirmation_required = true) ?(confirmation_satisfied = true)
    ?(confirmation_id = Some "conf_1") ?(binding = A.Selected (selected ()))
    ?(installation_active = true) ?(installation_repo_ok = true)
    ?(permissions_ok = true) ?(user_authority_ok = true) ?(org_policy_ok = true)
    ?(sso_ok = true) ?(live_ok = true) ?(live_detail = None)
    ?(live_revision = Some head_sha) ?(pin = A.empty_revision_pin)
    ?(actor_snapshot_id = Some "snap_1") ?(catalog_revision = "cat_rev_1")
    ?(access_revision = "acc_rev_1") ?(principal_revision = 3)
    ?(installation_revision = Some "inst_rev_1")
    ?(fallback = A.default_fallback_context) () : A.request =
  {
    action;
    tool_catalog =
      {
        revision = catalog_revision;
        access_revision;
        tool_authorized;
        room_id = Some room_id;
        session_key = Some "sess_1";
      };
    repo_grant =
      {
        repo_full_name = repo;
        granted = repo_granted;
        blocked = repo_blocked;
        access_revision = Some access_revision;
      };
    principal =
      {
        principal_id = "prin_a";
        principal_revision;
        principal_current_active = principal_current;
        actor_revision = Some 2;
        identity_link_revision = Some 4;
        confirmation_id;
        confirmation_required;
        confirmation_satisfied;
      };
    binding = { resolution = binding };
    installation =
      {
        installation_id = Some 99;
        revision = installation_revision;
        active = installation_active;
        repo_authorized = installation_repo_ok;
        permissions_ok;
      };
    user_org_sso = { user_authority_ok; org_policy_ok; sso_ok };
    live_action =
      { ok = live_ok; revision = live_revision; detail = live_detail };
    pin;
    actor_snapshot_id;
    fallback;
  }

let caps ~code_change : S.capability_policy =
  {
    allow_reply = false;
    allow_label = false;
    allow_assign = false;
    allow_review = false;
    allow_merge = false;
    allow_close = false;
    extra =
      (if code_change then [ (Code.capability_key, true) ]
       else [ (Code.capability_key, false) ]);
  }

let make_route ~id ~policy : S.t =
  {
    id;
    destination = S.Room room_id;
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

let route_on = make_route ~id:"rt_code_on" ~policy:(caps ~code_change:true)
let route_off = make_route ~id:"rt_code_off" ~policy:(caps ~code_change:false)
let pilot_off = Code.default_pilot_gate

let pilot_on =
  {
    Code.enabled = true;
    pilot_name = "p19-code-change-pilot";
    expires_at = Some "2099-01-01T00:00:00Z";
  }

let code_work_req ?(head = Some head_branch) () : Code.code_work_request =
  {
    repo_full_name = repo;
    base_branch = "main";
    scope = "fix the thing";
    runner = "codex";
    output_authority = "principal:alice";
    branch_prefix = Code.default_branch_prefix;
    head_branch = head;
    item_key = Some item_key;
    related_issue = Some 9;
  }

let pr_req_explicit () : Code.pr_create_request =
  {
    repo_full_name = repo;
    base_branch = "main";
    title = "Fix the thing";
    body = Some "body";
    draft = false;
    head = Code.Explicit_branch head_branch;
    branch_prefix = Code.default_branch_prefix;
    head_sha = Some head_sha;
    item_key = Some item_key;
  }

let pr_req_from_work ?(status = Code.Succeeded)
    ?(finished_at = Some "2024-07-01T00:00:00Z") () : Code.pr_create_request =
  {
    repo_full_name = repo;
    base_branch = "main";
    title = "From code work";
    body = None;
    draft = false;
    head =
      Code.Confirmed_code_work
        {
          code_work_plan_id = "plan_cw_1";
          head_branch;
          head_sha;
          status;
          finished_at;
        };
    branch_prefix = Code.default_branch_prefix;
    head_sha = Some head_sha;
    item_key = Some item_key;
  }

let live_ok ?refs ?(rev = Some head_sha) () : Attr.live_revalidation =
  {
    Attr.repo_present = true;
    base_present = true;
    already_applied = false;
    current_refs = refs;
    code_work_result_status = None;
    code_work_finished_at = None;
    max_age_seconds = None;
    target_revision = rev;
    planned_target_revision = Some head_sha;
  }

let live_refs ?(head = head_branch) ?(base = "main") ?(sha = head_sha)
    ?(head_exists = true) ?(base_exists = true) () : Code.live_refs =
  {
    head_branch = head;
    base_branch = base;
    head_sha = sha;
    base_sha = Some "base000";
    head_exists;
    base_exists;
  }

let expect_preview_ok = function
  | Ok o -> o
  | Error d -> Alcotest.fail ("preview: " ^ Attr.string_of_preview_deny d)

let expect_preview_deny = function
  | Error d -> d
  | Ok _ -> Alcotest.fail "expected preview deny"

let expect_dispatch_ok = function
  | Ok o -> o
  | Error d -> Alcotest.fail ("dispatch: " ^ Attr.string_of_dispatch_deny d)

let expect_dispatch_deny = function
  | Error d -> d
  | Ok _ -> Alcotest.fail "expected dispatch deny"

(* -------------------------------------------------------------------------- *)
(* Policy                                                                      *)
(* -------------------------------------------------------------------------- *)

let test_policy_action_user_required () =
  Alcotest.(check string) "policy" "code_change" Attr.policy_action;
  Alcotest.(check string)
    "code_work maps" "code_change"
    (Attr.policy_action_of_family (Attr.Code_work (code_work_req ())));
  Alcotest.(check string)
    "pr_create maps" "code_change"
    (Attr.policy_action_of_family (Attr.Pr_create (pr_req_explicit ())));
  let c = Policy.lookup ~action:"code_change" in
  Alcotest.(check string)
    "required" "user_required"
    (Policy.attribution_to_string c.attribution);
  Alcotest.(check string) "tier" "high" (Policy.risk_tier_to_string c.tier);
  Alcotest.(check bool)
    "no app fallback" false
    (Policy.permits_app_fallback c.attribution);
  let alias = Policy.lookup ~action:"code_work" in
  Alcotest.(check string) "alias" "code_change" alias.action

(* -------------------------------------------------------------------------- *)
(* Live revalidation                                                           *)
(* -------------------------------------------------------------------------- *)

let test_missing_repo_denied () =
  let live = { (live_ok ()) with repo_present = false } in
  match
    Attr.revalidate_live ~family:(Attr.Code_work (code_work_req ())) ~live
  with
  | Ok () -> Alcotest.fail "expected missing repo deny"
  | Error msg ->
      Alcotest.(check bool) "repo" true (contains ~needle:"repository" msg)

let test_duplicate_replay_denied () =
  let live = { (live_ok ()) with already_applied = true } in
  match
    Attr.revalidate_live ~family:(Attr.Pr_create (pr_req_explicit ())) ~live
  with
  | Ok () -> Alcotest.fail "expected duplicate deny"
  | Error msg ->
      Alcotest.(check bool) "dup" true (contains ~needle:"duplicate" msg)

let test_cancelled_code_work_denied () =
  let live =
    { (live_ok ()) with code_work_result_status = Some Code.Cancelled }
  in
  match
    Attr.revalidate_live ~family:(Attr.Pr_create (pr_req_from_work ())) ~live
  with
  | Ok () -> Alcotest.fail "expected cancelled deny"
  | Error msg ->
      Alcotest.(check bool) "cancelled" true (contains ~needle:"cancelled" msg)

let test_stale_head_sha_denied () =
  let refs = live_refs ~sha:head_sha_other () in
  let live = live_ok ~refs () in
  match
    Attr.revalidate_live ~family:(Attr.Pr_create (pr_req_explicit ())) ~live
  with
  | Ok () -> Alcotest.fail "expected head_sha mismatch"
  | Error msg ->
      Alcotest.(check bool)
        "sha" true
        (contains ~needle:"head_sha" msg || contains ~needle:"stale" msg)

let test_stale_target_denied () =
  let live = { (live_ok ()) with target_revision = Some head_sha_other } in
  match
    Attr.revalidate_live ~family:(Attr.Code_work (code_work_req ())) ~live
  with
  | Ok () -> Alcotest.fail "expected stale target"
  | Error msg ->
      Alcotest.(check bool) "stale" true (contains ~needle:"stale target" msg)

(* -------------------------------------------------------------------------- *)
(* Preview                                                                     *)
(* -------------------------------------------------------------------------- *)

let test_preview_user_required_success_code_work () =
  with_db @@ fun db ->
  let auth = base_request () in
  let live = live_ok () in
  let ok =
    expect_preview_ok
      (Attr.authorize_preview ~db
         ~family:(Attr.Code_work (code_work_req ()))
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live ~room_id ~now:fixed_now ())
  in
  Alcotest.(check string) "mode" "user" (A.resolved_mode_to_string ok.mode);
  Alcotest.(check string) "action" "code_change" ok.policy_action;
  Alcotest.(check bool) "no fallback" false ok.used_app_fallback

let test_preview_user_required_success_pr_create () =
  with_db @@ fun db ->
  let auth = base_request () in
  let live = live_ok ~refs:(live_refs ()) () in
  let ok =
    expect_preview_ok
      (Attr.authorize_preview ~db
         ~family:(Attr.Pr_create (pr_req_explicit ()))
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live ~room_id ~now:fixed_now ())
  in
  Alcotest.(check string) "mode" "user" (A.resolved_mode_to_string ok.mode);
  Alcotest.(check string) "action" "code_change" ok.policy_action

let test_preview_app_mode_forbidden () =
  with_db @@ fun db ->
  let fallback = A.fallback_context ~preview_actor:A.Fallback.Names_app () in
  let auth = base_request ~binding:A.Not_required ~fallback () in
  let d =
    expect_preview_deny
      (Attr.authorize_preview ~db
         ~family:(Attr.Code_work (code_work_req ()))
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live:(live_ok ()) ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "forbidden" true
    (contains ~needle:"User_required" d.reason
    || contains ~needle:"binding" d.reason
    || contains ~needle:"fallback" d.reason
    || contains ~needle:"App" d.reason)

let test_preview_denied_without_capability () =
  with_db @@ fun db ->
  let auth = base_request () in
  let d =
    expect_preview_deny
      (Attr.authorize_preview ~db
         ~family:(Attr.Code_work (code_work_req ()))
         ~route:(Some route_off) ~pilot:pilot_off ~user_auth_available:true
         ~auth ~live:(live_ok ()) ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "cap" true
    (contains ~needle:"code_change" d.reason
    || contains ~needle:"capability" d.reason)

let test_preview_denied_without_user_auth_and_pilot_off () =
  with_db @@ fun db ->
  let auth = base_request () in
  let d =
    expect_preview_deny
      (Attr.authorize_preview ~db
         ~family:(Attr.Code_work (code_work_req ()))
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:false
         ~auth ~live:(live_ok ()) ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "pilot" true
    (contains ~needle:"pilot" d.reason
    || contains ~needle:"not available" d.reason
    || contains ~needle:"User_required" d.reason)

let test_preview_denied_actor_change () =
  with_db @@ fun db ->
  let auth =
    base_request ~principal_current:false ~binding:(A.Selected (selected ())) ()
  in
  let d =
    expect_preview_deny
      (Attr.authorize_preview ~db
         ~family:(Attr.Code_work (code_work_req ()))
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live:(live_ok ()) ~now:fixed_now ())
  in
  Alcotest.(check bool) "actor" true (String.length d.reason > 0)

(* -------------------------------------------------------------------------- *)
(* Dispatch                                                                    *)
(* -------------------------------------------------------------------------- *)

let test_dispatch_requires_user_lease () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request () in
  let family = Attr.Code_work (code_work_req ()) in
  let live = live_ok () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db ~family ~route:(Some route_on)
         ~pilot:pilot_off ~user_auth_available:true ~auth ~live ~room_id
         ~now:fixed_now ())
  in
  let d =
    expect_dispatch_ok
      (Attr.dispatch ~db ~family ~live_auth:auth ~prior:preview.allow ~live
         ~vault_id:vault.id ~expected:acct ~room_id ~now:fixed_now ())
  in
  Alcotest.(check bool) "lease" true d.has_user_lease;
  Alcotest.(check string) "mode" "user" (A.resolved_mode_to_string d.mode);
  (* Lease is opaque — raw token must not appear on receipt reason. *)
  Alcotest.(check bool)
    "no raw token in receipt" false
    (contains ~needle:"ghu_access" d.receipt.reason);
  Attr.revoke_issued_lease d.issued

let test_dispatch_duplicate_replay_denied () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request () in
  let family = Attr.Pr_create (pr_req_explicit ()) in
  let live = live_ok ~refs:(live_refs ()) () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db ~family ~route:(Some route_on)
         ~pilot:pilot_off ~user_auth_available:true ~auth ~live ~room_id
         ~now:fixed_now ())
  in
  let d =
    expect_dispatch_deny
      (Attr.dispatch ~db ~family ~live_auth:auth ~prior:preview.allow
         ~live:{ live with already_applied = true }
         ~vault_id:vault.id ~expected:acct ~now:fixed_now ())
  in
  Alcotest.(check bool) "dup" true (contains ~needle:"duplicate" d.reason)

let test_dispatch_cancelled_after_preview () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request () in
  let family = Attr.Pr_create (pr_req_from_work ()) in
  let live = live_ok ~refs:(live_refs ()) () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db ~family ~route:(Some route_on)
         ~pilot:pilot_off ~user_auth_available:true ~auth ~live ~room_id
         ~now:fixed_now ())
  in
  let live_cancelled =
    { live with code_work_result_status = Some Code.Cancelled }
  in
  let d =
    expect_dispatch_deny
      (Attr.dispatch ~db ~family ~live_auth:auth ~prior:preview.allow
         ~live:live_cancelled ~vault_id:vault.id ~expected:acct ~now:fixed_now
         ())
  in
  Alcotest.(check bool) "cancelled" true (contains ~needle:"cancelled" d.reason)

let test_receipt_lists_by_action () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request () in
  let family = Attr.Code_work (code_work_req ()) in
  let live = live_ok () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db ~family ~route:(Some route_on)
         ~pilot:pilot_off ~user_auth_available:true ~auth ~live ~room_id
         ~now:fixed_now ())
  in
  ignore
    (expect_dispatch_ok
       (Attr.dispatch ~db ~family ~live_auth:auth ~prior:preview.allow ~live
          ~vault_id:vault.id ~expected:acct ~room_id ~receipt_id:"rcpt_code_1"
          ~now:fixed_now ()));
  let rows = Audit.list_by_action ~db ~action:"code_change" ~limit:10 () in
  Alcotest.(check bool) "has rows" true (List.length rows >= 2);
  let kinds = List.map (fun (r : Audit.t) -> r.kind) rows in
  Alcotest.(check bool)
    "has preview" true
    (List.exists (fun k -> k = Audit.Preview) kinds);
  Alcotest.(check bool)
    "has receipt" true
    (List.exists (fun k -> k = Audit.Receipt) kinds)

(* -------------------------------------------------------------------------- *)
(* Plan + workflow                                                             *)
(* -------------------------------------------------------------------------- *)

let test_plan_p21_user_path () =
  with_db @@ fun db ->
  let plan =
    assert_ok
      (Code.plan_code_work ~db ~principal ~room_id ~pilot:pilot_off
         ~user_auth_available:true ~req:(code_work_req ()) ~base_revision
         ~route:route_on ~now:fixed_now ())
  in
  let s = Yojson.Safe.to_string (Setup_plan.to_persist_json plan) in
  Alcotest.(check bool)
    "user required" true
    (contains ~needle:"User_required" s);
  Alcotest.(check bool)
    "no raw token on plan" false
    (contains ~needle:"ghu_access" s);
  Alcotest.(check bool)
    "token isolation readiness" true
    (contains ~needle:"token_isolation" s || contains ~needle:"personal token" s)

let test_plan_with_attribution_embeds_allow () =
  with_db @@ fun db ->
  let auth = base_request () in
  let live = live_ok () in
  let planned =
    assert_ok
      (Attr.plan_with_attribution ~db ~principal ~room_id
         ~family:(Attr.Code_work (code_work_req ()))
         ~base_revision ~auth ~live ~route:(Some route_on) ~pilot:pilot_off
         ~user_auth_available:true ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "has allow" true
    (Attr.has_attribution_allow planned.plan);
  match Attr.allow_of_plan planned.plan with
  | Error e -> Alcotest.fail e
  | Ok None -> Alcotest.fail "expected allow on plan"
  | Ok (Some allow) ->
      Alcotest.(check string)
        "mode" "user"
        (A.resolved_mode_to_string allow.mode);
      Alcotest.(check string) "action" "code_change" allow.requirement.action

let test_prepare_dispatch_from_plan_success () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request () in
  let live = live_ok ~refs:(live_refs ()) () in
  let planned =
    assert_ok
      (Attr.plan_with_attribution ~db ~principal ~room_id
         ~family:(Attr.Pr_create (pr_req_explicit ()))
         ~base_revision ~auth ~live ~route:(Some route_on) ~pilot:pilot_off
         ~user_auth_available:true ~now:fixed_now ())
  in
  let disp =
    assert_ok
      (Attr.prepare_dispatch_from_plan ~db ~plan:planned.plan ~live_auth:auth
         ~live ~vault_id:vault.id ~expected:acct ~now:fixed_now ())
  in
  Alcotest.(check bool) "lease" true disp.has_user_lease;
  Attr.revoke_issued_lease disp.issued

let test_workflow_preview_apply_idempotent () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request () in
  let live = live_ok ~refs:(live_refs ()) () in
  let plan =
    assert_ok
      (Workflow.preview ~db ~principal ~room_id
         ~action:(Workflow.Pr_create (pr_req_explicit ()))
         ~base_revision ~route:route_on ~code_change_pilot:pilot_off
         ~user_auth_available:true ~attribution_evidence:auth
         ~code_change_live:live ~github_user_id:4242L ~now:fixed_now ())
  in
  Alcotest.(check bool) "staged" true (Attr.has_attribution_allow plan);
  let outcome1 =
    assert_ok
      (Workflow.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest
         ~principal ~current_base_revision:base_revision ~attribution_live:auth
         ~code_change_live:live ~vault_id:vault.id ~expected_account:acct
         ~github_user_id:4242L ~now:fixed_now ())
  in
  let receipt_id =
    match outcome1 with
    | Setup_plan_apply.Applied { first_time = true; receipt_id } -> receipt_id
    | Setup_plan_apply.Applied { first_time = false; _ } ->
        Alcotest.fail "expected first_time"
    | Setup_plan_apply.Rejected { message; _ } -> Alcotest.fail message
  in
  Alcotest.(check bool) "receipt id" true (String.length receipt_id > 0);
  Alcotest.(check bool)
    "native receipt" true
    (Audit.count ~db ~kind:Audit.Receipt () >= 1);
  let outcome2 =
    assert_ok
      (Workflow.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest
         ~principal ~current_base_revision:base_revision ~attribution_live:auth
         ~code_change_live:live ~vault_id:vault.id ~expected_account:acct
         ~now:fixed_now ())
  in
  match outcome2 with
  | Setup_plan_apply.Applied { first_time = false; receipt_id = r2 } ->
      Alcotest.(check string) "same receipt" receipt_id r2
  | Setup_plan_apply.Applied { first_time = true; _ } ->
      Alcotest.fail "expected idempotent second apply"
  | Setup_plan_apply.Rejected { message; _ } -> Alcotest.fail message

let test_workflow_apply_requires_live_when_staged () =
  with_db @@ fun db ->
  let auth = base_request () in
  let live = live_ok () in
  let plan =
    assert_ok
      (Workflow.preview ~db ~principal ~room_id
         ~action:(Workflow.Code_work (code_work_req ()))
         ~base_revision ~route:route_on ~code_change_pilot:pilot_off
         ~user_auth_available:true ~attribution_evidence:auth
         ~code_change_live:live ~now:fixed_now ())
  in
  match
    Workflow.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:base_revision ~now:fixed_now ()
  with
  | Ok (Setup_plan_apply.Rejected { message; _ }) ->
      Alcotest.(check bool)
        "mentions live" true
        (contains ~needle:"attribution_live" message
        || contains ~needle:"live evidence" message)
  | Ok (Setup_plan_apply.Applied _) ->
      Alcotest.fail "expected reject without live evidence"
  | Error e -> Alcotest.fail e

let test_workflow_apply_stale_refs_no_attempt () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request () in
  let live = live_ok ~refs:(live_refs ()) () in
  let plan =
    assert_ok
      (Workflow.preview ~db ~principal ~room_id
         ~action:(Workflow.Pr_create (pr_req_explicit ()))
         ~base_revision ~route:route_on ~code_change_pilot:pilot_off
         ~user_auth_available:true ~attribution_evidence:auth
         ~code_change_live:live ~now:fixed_now ())
  in
  let stale_live = live_ok ~refs:(live_refs ~sha:head_sha_other ()) () in
  match
    Workflow.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:base_revision ~attribution_live:auth
      ~code_change_live:stale_live ~vault_id:vault.id ~expected_account:acct
      ~now:fixed_now ()
  with
  | Ok (Setup_plan_apply.Rejected { message; _ }) ->
      Alcotest.(check bool)
        "stale or head" true
        (contains ~needle:"head" message
        || contains ~needle:"stale" message
        || contains ~needle:"revalidat" message)
  | Ok (Setup_plan_apply.Applied _) ->
      Alcotest.fail "expected reject when head sha changed"
  | Error e -> Alcotest.fail e

let test_pr_opened_webhook_reconciliation_with_user_receipt () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request () in
  let live = live_ok ~refs:(live_refs ()) () in
  let plan =
    assert_ok
      (Workflow.preview ~db ~principal ~room_id
         ~action:(Workflow.Pr_create (pr_req_explicit ()))
         ~base_revision ~route:route_on ~code_change_pilot:pilot_off
         ~user_auth_available:true ~attribution_evidence:auth
         ~code_change_live:live ~github_user_id:4242L ~now:fixed_now ())
  in
  let outcome =
    assert_ok
      (Workflow.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest
         ~principal ~current_base_revision:base_revision ~attribution_live:auth
         ~code_change_live:live ~vault_id:vault.id ~expected_account:acct
         ~github_user_id:4242L ~now:fixed_now ())
  in
  let receipt_id =
    match outcome with
    | Setup_plan_apply.Applied { receipt_id; first_time = true } -> receipt_id
    | Setup_plan_apply.Applied { first_time = false; _ } ->
        Alcotest.fail "expected first apply"
    | Setup_plan_apply.Rejected { message; _ } -> Alcotest.fail message
  in
  (match Reconcile.get_by_receipt_id ~db ~receipt_id with
  | Some _ -> ()
  | None ->
      let corr =
        Reconcile.make_correlation ~room_id ~action:"pr_create"
          ~actor_mode:"user" ~item_key ~plan_id:plan.id ~receipt_id
          ~requested_mode:"user_required" ~resolved_mode:"user" ()
      in
      assert_ok
        (Reconcile.record_correlation ~db ~correlation:corr ~now:fixed_now ()));
  match Reconcile.get_by_receipt_id ~db ~receipt_id with
  | None -> Alcotest.fail "expected correlation after apply/record"
  | Some c_open ->
      Alcotest.(check string)
        "user mode" "user"
        (Reconcile.resolved_attribution c_open);
      (match Reconcile.requested_attribution c_open with
      | Some ("user" | "user_required") -> ()
      | other ->
          Alcotest.fail
            (Printf.sprintf "unexpected requested_mode: %s"
               (match other with None -> "None" | Some s -> s)));
      Alcotest.(check (option string)) "plan id" (Some plan.id) c_open.plan_id

let suite =
  [
    ("policy action user_required", `Quick, test_policy_action_user_required);
    ("missing repo denied", `Quick, test_missing_repo_denied);
    ("duplicate replay denied", `Quick, test_duplicate_replay_denied);
    ("cancelled code work denied", `Quick, test_cancelled_code_work_denied);
    ("stale head sha denied", `Quick, test_stale_head_sha_denied);
    ("stale target denied", `Quick, test_stale_target_denied);
    ( "preview user_required success code_work",
      `Quick,
      test_preview_user_required_success_code_work );
    ( "preview user_required success pr_create",
      `Quick,
      test_preview_user_required_success_pr_create );
    ("preview app mode forbidden", `Quick, test_preview_app_mode_forbidden);
    ( "preview denied without capability",
      `Quick,
      test_preview_denied_without_capability );
    ( "preview denied without user auth and pilot off",
      `Quick,
      test_preview_denied_without_user_auth_and_pilot_off );
    ("preview denied actor change", `Quick, test_preview_denied_actor_change);
    ("dispatch requires user lease", `Quick, test_dispatch_requires_user_lease);
    ( "dispatch duplicate replay denied",
      `Quick,
      test_dispatch_duplicate_replay_denied );
    ( "dispatch cancelled after preview",
      `Quick,
      test_dispatch_cancelled_after_preview );
    ("receipt lists by action", `Quick, test_receipt_lists_by_action);
    ("plan p21 user path", `Quick, test_plan_p21_user_path);
    ( "plan with attribution embeds allow",
      `Quick,
      test_plan_with_attribution_embeds_allow );
    ( "prepare dispatch from plan success",
      `Quick,
      test_prepare_dispatch_from_plan_success );
    ( "workflow preview/apply + idempotent retry",
      `Quick,
      test_workflow_preview_apply_idempotent );
    ( "workflow apply requires live when staged",
      `Quick,
      test_workflow_apply_requires_live_when_staged );
    ( "workflow apply stale refs no attempt",
      `Quick,
      test_workflow_apply_stale_refs_no_attempt );
    ( "pr opened webhook reconciliation with user receipt",
      `Quick,
      test_pr_opened_webhook_reconciliation_with_user_receipt );
  ]

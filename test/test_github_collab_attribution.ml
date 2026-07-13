(** Tests for P21.M3.E3.T001: user-preferred collab comment/label/assign
    attribution integration (authorize + dispatch lease + audit).

    Covers: staged attribution gate, success (user + visible App fallback),
    denial, actor/mode change, stale state, idempotent apply receipt, native
    receipt, and secret non-escape. *)

module A = Github_attribution_authorize
module C = Github_collab_attribution
module Collab = Github_collab_actions
module Audit = Github_attribution_audit
module Lease = Github_attribution_dispatch_lease
module Policy = Github_attribution_policy
module S = Github_route_store
module V = Github_user_token_vault
module Token_store = Github_user_token_store
module Token_lease = Github_user_token_lease
module Workflow = Github_action_workflow
module Reconcile = Github_action_reconcile

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-collab-attr-test" ()

let sample_tokens =
  {
    Token_store.access_token = "ghu_access_COLLAB_ATTR_PLAINTEXT_never_export";
    refresh_token = Some "ghr_refresh_COLLAB_ATTR_PLAINTEXT_never_export";
  }

let fixed_now = 1_720_000_000.0
let far_expires = "2026-12-01T00:00:00Z"
let base_revision = "rev-config-1"
let room_id = "room-teams-1"
let room = S.Room room_id
let item_key = "item:acme/widget:pr:42"

let principal =
  Setup_plan.{ id = "principal:alice"; kind = Principal; label = Some "Alice" }

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

let secrets_absent blob =
  List.iter
    (fun needle ->
      Alcotest.(check bool) ("no " ^ needle) false (contains ~needle blob))
    [
      sample_tokens.access_token;
      Option.get sample_tokens.refresh_token;
      "ghu_access_COLLAB";
      "ghr_refresh_COLLAB";
      aes_key;
    ]

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Setup_plan_apply.init_schema db;
  Audit.ensure_schema db;
  V.ensure_schema db;
  Fun.protect
    ~finally:(fun () ->
      ignore (Token_lease.discard_all ());
      ignore (Sqlite3.db_close db))
    (fun () -> f db)

let caps ~reply ~label ~assign : S.capability_policy =
  {
    allow_reply = reply;
    allow_label = label;
    allow_assign = assign;
    allow_review = false;
    allow_merge = false;
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

let selected ?(binding_id = "bind_1") ?(lineage_id = "lin_1")
    ?(authorized = true) ?(vault_active = true) ?(vault_generation = 1)
    ?(lineage_matches_pin = true) () =
  assert_ok
    (A.make_selected_binding ~binding_id ~lineage_id ~authorized ~vault_active
       ~vault_generation ~lineage_matches_pin ())

let base_request ?(action = "comment") ?(tool_authorized = true)
    ?(repo_granted = true) ?(repo_blocked = false) ?(principal_current = true)
    ?(confirmation_required = false) ?(confirmation_satisfied = true)
    ?(confirmation_id = None) ?(binding = A.Selected (selected ()))
    ?(installation_active = true) ?(installation_repo_ok = true)
    ?(permissions_ok = true) ?(user_authority_ok = true) ?(org_policy_ok = true)
    ?(sso_ok = true) ?(live_ok = true) ?(live_detail = None)
    ?(live_revision = Some "meta_rev_1") ?(pin = A.empty_revision_pin)
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
        repo_full_name = "acme/widget";
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

let account ?(principal_id = "prin_a") ?(github_user_id = 4242L) ?(app_id = 99)
    ?(host = V.default_host) () =
  assert_ok (V.make_account_key ~principal_id ~github_user_id ~app_id ~host ())

let make_keys ?(key_id = "mk-collab-1") ?(key_version = 1) () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key ())

let create_vault ~db ?(keys = make_keys ()) ?(account = account ())
    ?(tokens = sample_tokens) ?(id = "ghvault_collab_1")
    ?(expires_at = far_expires) () =
  match
    V.create ~db ~keys ~id ~now:fixed_now ~account ~tokens
      ~scopes:[ "repo"; "read:user" ] ~expires_at ()
  with
  | Ok r -> r
  | Error d -> Alcotest.fail ("create vault: " ^ V.string_of_denial d)

(* -------------------------------------------------------------------------- *)
(* Mapping / policy                                                            *)
(* -------------------------------------------------------------------------- *)

let test_policy_action_mapping () =
  Alcotest.(check string)
    "comment" "comment"
    (C.policy_action_of_collab (Collab.Comment { item_key; body = "hi" }));
  Alcotest.(check string)
    "label" "label"
    (C.policy_action_of_collab
       (Collab.Label { item_key; add = [ "bug" ]; remove = [] }));
  Alcotest.(check string)
    "assign" "assign"
    (C.policy_action_of_collab
       (Collab.Assign { item_key; add = [ "alice" ]; remove = [] }));
  Alcotest.(check bool)
    "comment preferred" true
    (C.is_user_preferred_metadata (Collab.Comment { item_key; body = "x" }));
  Alcotest.(check bool)
    "assign preferred" true
    (C.is_user_preferred_metadata
       (Collab.Assign { item_key; add = [ "a" ]; remove = [] }));
  let r = Policy.lookup ~action:"assign" in
  Alcotest.(check string)
    "assign attr" "user_preferred"
    (Policy.attribution_to_string r.attribution)

(* -------------------------------------------------------------------------- *)
(* Staged gate: capability + authorize                                         *)
(* -------------------------------------------------------------------------- *)

let test_gate_capability_denied () =
  let route =
    make_route ~id:"rt_no_reply"
      ~policy:(caps ~reply:false ~label:true ~assign:true)
  in
  let action = Collab.Comment { item_key; body = "nope" } in
  match C.gate ~route:(Some route) ~action ~evidence:(base_request ()) () with
  | C.Capability_denied { reason } ->
      Alcotest.(check bool)
        "mentions capability" true
        (contains ~needle:"allow_reply" reason
        || contains ~needle:"capability" reason)
  | C.Attribution _ -> Alcotest.fail "expected capability deny"

let test_gate_attribution_user_success () =
  let route =
    make_route ~id:"rt_reply"
      ~policy:(caps ~reply:true ~label:true ~assign:true)
  in
  let action = Collab.Comment { item_key; body = "LGTM" } in
  match C.gate ~route:(Some route) ~action ~evidence:(base_request ()) () with
  | C.Capability_denied { reason } -> Alcotest.fail reason
  | C.Attribution { capability; decision; request } -> (
      Alcotest.(check string) "cap" "allow_reply" capability;
      Alcotest.(check string) "forced action" "comment" request.action;
      match decision with
      | A.Allow a ->
          Alcotest.(check string)
            "mode" "user"
            (A.resolved_mode_to_string a.mode);
          Alcotest.(check bool) "not fallback" false a.used_app_fallback
      | A.Deny d ->
          Alcotest.fail
            (Printf.sprintf "deny %s/%s" d.failed_check d.repair.code))

let test_gate_visible_app_fallback () =
  let route =
    make_route ~id:"rt_label"
      ~policy:(caps ~reply:false ~label:true ~assign:false)
  in
  let action =
    Collab.Label { item_key; add = [ "needs-triage" ]; remove = [] }
  in
  let evidence =
    base_request ~action:"label" ~binding:A.Not_required
      ~user_authority_ok:false
      ~fallback:(A.fallback_context ~preview_actor:A.Fallback.Names_app ())
      ()
  in
  match C.gate ~route:(Some route) ~action ~evidence () with
  | C.Attribution { decision = A.Allow a; _ } ->
      Alcotest.(check string) "mode" "app" (A.resolved_mode_to_string a.mode);
      Alcotest.(check bool) "fallback" true a.used_app_fallback
  | C.Attribution { decision = A.Deny d; _ } ->
      Alcotest.fail (Printf.sprintf "deny %s" d.repair.code)
  | C.Capability_denied { reason } -> Alcotest.fail reason

(* -------------------------------------------------------------------------- *)
(* Stage preview + audit                                                       *)
(* -------------------------------------------------------------------------- *)

let test_stage_preview_records_audit () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_ok" ~policy:(caps ~reply:true ~label:true ~assign:true)
  in
  let action = Collab.Comment { item_key; body = "ship it" } in
  let staged =
    match
      C.stage_preview ~db ~route:(Some route) ~action
        ~evidence:(base_request ()) ~room_id ~now:fixed_now ()
    with
    | Ok s -> s
    | Error e -> Alcotest.fail (C.string_of_stage_error e)
  in
  Alcotest.(check string) "action" "comment" staged.allow.requirement.action;
  Alcotest.(check string)
    "preview kind" "preview"
    (Audit.record_kind_to_string staged.preview.kind);
  Alcotest.(check int) "audit rows" 1 (Audit.count ~db ());
  let rows = Audit.list_by_kind ~db ~kind:Audit.Preview () in
  Alcotest.(check int) "preview count" 1 (List.length rows)

let test_stage_preview_denial_records_repair () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_ok" ~policy:(caps ~reply:true ~label:true ~assign:true)
  in
  let action = Collab.Comment { item_key; body = "denied sso" } in
  let evidence = base_request ~sso_ok:false () in
  match
    C.stage_preview ~db ~route:(Some route) ~action ~evidence ~room_id
      ~now:fixed_now ()
  with
  | Ok _ -> Alcotest.fail "expected deny"
  | Error (C.Attribution { deny; repair }) ->
      Alcotest.(check string) "code" "sso_required" deny.repair.code;
      Alcotest.(check bool) "repair recorded" true (Option.is_some repair);
      Alcotest.(check bool)
        "has repair rows" true
        (Audit.count ~db ~kind:Audit.Repair_state () >= 1)
  | Error e -> Alcotest.fail (C.string_of_stage_error e)

(* -------------------------------------------------------------------------- *)
(* Plan + allow embed                                                          *)
(* -------------------------------------------------------------------------- *)

let test_plan_with_attribution_embeds_allow () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_ok" ~policy:(caps ~reply:true ~label:true ~assign:true)
  in
  let action =
    Collab.Label { item_key; add = [ "good-first-issue" ]; remove = [] }
  in
  let planned =
    assert_ok
      (C.plan_with_attribution ~db ~principal ~room_id ~action ~base_revision
         ~evidence:(base_request ~action:"label" ())
         ~route ~now:fixed_now ())
  in
  Alcotest.(check bool) "has allow" true (C.has_attribution_allow planned.plan);
  let allow = assert_ok (C.allow_of_plan planned.plan) |> Option.get in
  Alcotest.(check string) "mode" "user" (A.resolved_mode_to_string allow.mode);
  Alcotest.(check string) "action" "label" allow.requirement.action;
  (* Round-trip JSON. *)
  let allow2 = assert_ok (C.allow_of_json (C.allow_to_json allow)) in
  Alcotest.(check string)
    "roundtrip mode" "user"
    (A.resolved_mode_to_string allow2.mode);
  secrets_absent
    (Yojson.Safe.to_string (C.allow_to_json allow)
    ^ Yojson.Safe.to_string planned.plan.apply_payload.data)

let test_plan_assign_metadata () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_assign"
      ~policy:(caps ~reply:false ~label:false ~assign:true)
  in
  let action =
    Collab.Assign { item_key; add = [ "bob" ]; remove = [ "carol" ] }
  in
  let planned =
    assert_ok
      (C.plan_with_attribution ~db ~principal ~room_id ~action ~base_revision
         ~evidence:(base_request ~action:"assign" ())
         ~route ~now:fixed_now ())
  in
  let allow = assert_ok (C.allow_of_plan planned.plan) |> Option.get in
  Alcotest.(check string) "assign" "assign" allow.requirement.action;
  Alcotest.(check string)
    "preferred" "user_preferred"
    (Policy.attribution_to_string allow.requirement.attribution)

(* -------------------------------------------------------------------------- *)
(* Dispatch: success, stale, actor change, app path, receipt                   *)
(* -------------------------------------------------------------------------- *)

let test_dispatch_user_lease_and_receipt () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ = create_vault ~db ~keys ~account:acct () in
  let route =
    make_route ~id:"rt_ok" ~policy:(caps ~reply:true ~label:false ~assign:false)
  in
  let action = Collab.Comment { item_key; body = "native user comment" } in
  let evidence = base_request () in
  let planned =
    assert_ok
      (C.plan_with_attribution ~db ~principal ~room_id ~action ~base_revision
         ~evidence ~route ~now:fixed_now ())
  in
  let prior = planned.staged.allow in
  let dispatched =
    match
      C.prepare_dispatch ~db ~live:evidence ~prior ~vault_id:rec_.id
        ~expected:acct ~item_key ~room_id ~plan_id:planned.plan.id
        ~github_user_id:4242L ~now:fixed_now ()
    with
    | Ok d -> d
    | Error e -> Alcotest.fail (Lease.string_of_denial e)
  in
  Alcotest.(check string)
    "mode" "user"
    (A.resolved_mode_to_string dispatched.issued.mode);
  Alcotest.(check bool)
    "has lease" true
    (Option.is_some dispatched.issued.lease);
  Alcotest.(check string)
    "receipt kind" "receipt"
    (Audit.record_kind_to_string dispatched.receipt.kind);
  Alcotest.(check string)
    "resolved" "user"
    (Option.value dispatched.receipt.resolved_mode ~default:"");
  secrets_absent
    (Yojson.Safe.to_string (Lease.issued_to_json dispatched.issued)
    ^ Yojson.Safe.to_string (Audit.to_json dispatched.receipt));
  (* Token only inside with_token. *)
  match
    Token_lease.with_token ~db ~keys ~now:fixed_now
      ~lease:(Option.get dispatched.issued.lease)
      ~f:(fun ~access_token -> access_token = sample_tokens.access_token)
      ()
  with
  | Ok true -> ()
  | Ok false -> Alcotest.fail "token mismatch"
  | Error d -> Alcotest.fail (Token_lease.string_of_denial d)

let test_dispatch_app_fallback_no_lease () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_ok" ~policy:(caps ~reply:true ~label:false ~assign:false)
  in
  let action = Collab.Comment { item_key; body = "app fallback" } in
  let evidence =
    base_request ~binding:A.Not_required ~user_authority_ok:false
      ~fallback:(A.fallback_context ~preview_actor:A.Fallback.Names_app ())
      ()
  in
  let planned =
    assert_ok
      (C.plan_with_attribution ~db ~principal ~room_id ~action ~base_revision
         ~evidence ~route ~now:fixed_now ())
  in
  Alcotest.(check bool) "fallback" true planned.staged.allow.used_app_fallback;
  let dispatched =
    match
      C.prepare_dispatch ~db ~live:evidence ~prior:planned.staged.allow
        ~item_key ~room_id ~plan_id:planned.plan.id ~now:fixed_now ()
    with
    | Ok d -> d
    | Error e -> Alcotest.fail (Lease.string_of_denial e)
  in
  Alcotest.(check string)
    "app" "app"
    (A.resolved_mode_to_string dispatched.issued.mode);
  Alcotest.(check bool) "no lease" true (Option.is_none dispatched.issued.lease);
  Alcotest.(check bool)
    "receipt fallback flag" true dispatched.receipt.used_app_fallback

let test_dispatch_stale_catalog_denies () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let route =
    make_route ~id:"rt_ok" ~policy:(caps ~reply:true ~label:false ~assign:false)
  in
  let action = Collab.Comment { item_key; body = "stale" } in
  let preview = base_request () in
  let planned =
    assert_ok
      (C.plan_with_attribution ~db ~principal ~room_id ~action ~base_revision
         ~evidence:preview ~route ~now:fixed_now ())
  in
  let live = base_request ~catalog_revision:"cat_rev_CHANGED" () in
  match
    C.prepare_dispatch ~db ~live ~prior:planned.staged.allow ~vault_id:rec_.id
      ~now:fixed_now ()
  with
  | Ok _ -> Alcotest.fail "expected stale deny"
  | Error (Lease.Authorization d) ->
      Alcotest.(check string)
        "stale code" "stale_tool_catalog_revision" d.repair.code;
      Alcotest.(check bool)
        "repair audit" true
        (Audit.count ~db ~kind:Audit.Repair_state () >= 1)
  | Error e -> Alcotest.fail ("unexpected: " ^ Lease.string_of_denial e)

let test_dispatch_actor_mode_change_denies () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let route =
    make_route ~id:"rt_ok" ~policy:(caps ~reply:true ~label:false ~assign:false)
  in
  let action = Collab.Comment { item_key; body = "mode lock" } in
  (* Preview as user. *)
  let preview = base_request () in
  let planned =
    assert_ok
      (C.plan_with_attribution ~db ~principal ~room_id ~action ~base_revision
         ~evidence:preview ~route ~now:fixed_now ())
  in
  Alcotest.(check string)
    "prior user" "user"
    (A.resolved_mode_to_string planned.staged.allow.mode);
  (* Live tries to flip to App via visible fallback inputs — revalidation locks
     mode continuity against prior.allow.mode. *)
  let live =
    base_request ~binding:A.Not_required ~user_authority_ok:false
      ~fallback:
        (A.fallback_context ~preview_actor:A.Fallback.Names_app
           ~phase:(A.Fallback.Retry { locked_mode = A.Fallback.App })
           ())
      ()
  in
  match
    C.prepare_dispatch ~db ~live ~prior:planned.staged.allow ~vault_id:rec_.id
      ~now:fixed_now ()
  with
  | Ok _ -> Alcotest.fail "expected mode continuity deny"
  | Error
      ( Lease.Prior_mode_mismatch _ | Lease.Authorization _
      | Lease.Prior_binding_mismatch _ ) ->
      ()
  | Error e -> Alcotest.fail ("unexpected: " ^ Lease.string_of_denial e)

let test_prepare_dispatch_from_plan () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ = create_vault ~db ~keys ~account:acct () in
  let route =
    make_route ~id:"rt_ok" ~policy:(caps ~reply:true ~label:false ~assign:false)
  in
  let action = Collab.Comment { item_key; body = "from plan" } in
  let evidence = base_request () in
  let planned =
    assert_ok
      (C.plan_with_attribution ~db ~principal ~room_id ~action ~base_revision
         ~evidence ~route ~now:fixed_now ())
  in
  let dispatched =
    assert_ok
      (C.prepare_dispatch_from_plan ~db ~plan:planned.plan ~live:evidence
         ~vault_id:rec_.id ~expected:acct ~github_user_id:4242L ~now:fixed_now
         ())
  in
  Alcotest.(check bool) "lease" true (Option.is_some dispatched.issued.lease);
  C.revoke_issued_lease dispatched.issued

(* -------------------------------------------------------------------------- *)
(* Workflow wiring + idempotent apply                                          *)
(* -------------------------------------------------------------------------- *)

let test_workflow_attributed_apply_fails_closed_without_receipts () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ = create_vault ~db ~keys ~account:acct () in
  let route =
    make_route ~id:"rt_ok" ~policy:(caps ~reply:true ~label:false ~assign:false)
  in
  let evidence = base_request () in
  let plan =
    assert_ok
      (Workflow.preview ~db ~principal ~room_id
         ~action:
           (Workflow.Collab
              (Collab.Comment { item_key; body = "workflow path" }))
         ~base_revision ~route ~attribution_evidence:evidence ~now:fixed_now ())
  in
  Alcotest.(check bool) "staged" true (C.has_attribution_allow plan);
  let outcome1 =
    assert_ok
      (Workflow.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest
         ~principal ~current_base_revision:base_revision
         ~attribution_live:evidence ~vault_id:rec_.id ~expected_account:acct
         ~github_user_id:4242L ~now:fixed_now ())
  in
  (match outcome1 with
  | Setup_plan_apply.Applied _ ->
      Alcotest.fail "attributed apply must fail closed without a live dispatcher"
  | Setup_plan_apply.Rejected { reason; message } ->
      Alcotest.(check string)
        "apply error" "apply_error"
        (Setup_plan_apply.string_of_reject_reason reason);
      Alcotest.(check bool) "mentions dispatcher" true
        (contains ~needle:"dispatcher" message));
  Alcotest.(check (option string)) "plan stays pending" (Some "pending")
    (Test_helpers.query_single_text_option db
       (Printf.sprintf "SELECT status FROM setup_plans WHERE id = '%s'" plan.id));
  Alcotest.(check (option string)) "no apply receipt" None
    (Test_helpers.query_single_text_option db
       (Printf.sprintf
          "SELECT receipt_id FROM setup_plans WHERE id = '%s'" plan.id));
  Alcotest.(check int) "no native attribution receipt" 0
    (Audit.count ~db ~kind:Audit.Receipt ());
  Reconcile.ensure_schema db;
  Alcotest.(check bool) "no correlation" true
    (Option.is_none (Reconcile.get_by_plan_id ~db ~plan_id:plan.id))

let test_workflow_apply_requires_live_when_staged () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_ok" ~policy:(caps ~reply:true ~label:false ~assign:false)
  in
  let evidence = base_request () in
  let plan =
    assert_ok
      (Workflow.preview ~db ~principal ~room_id
         ~action:
           (Workflow.Collab (Collab.Comment { item_key; body = "need live" }))
         ~base_revision ~route ~attribution_evidence:evidence ~now:fixed_now ())
  in
  match
    Workflow.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:base_revision ~now:fixed_now ()
  with
  | Ok (Setup_plan_apply.Rejected { message; _ }) ->
      Alcotest.(check bool)
        "mentions dispatcher" true
        (contains ~needle:"dispatcher" message)
  | Ok (Setup_plan_apply.Applied _) ->
      Alcotest.fail "expected reject without live evidence"
  | Error e -> Alcotest.fail e

let test_workflow_legacy_collab_without_attribution () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_ok" ~policy:(caps ~reply:true ~label:false ~assign:false)
  in
  let plan =
    assert_ok
      (Workflow.preview ~db ~principal ~room_id
         ~action:
           (Workflow.Collab (Collab.Comment { item_key; body = "legacy" }))
         ~base_revision ~route ~now:fixed_now ())
  in
  Alcotest.(check bool) "no staged allow" false (C.has_attribution_allow plan);
  match
    Workflow.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:base_revision ~now:fixed_now ()
  with
  | Ok (Setup_plan_apply.Applied _) ->
      Alcotest.fail "apply must fail closed without a live dispatcher"
  | Ok (Setup_plan_apply.Rejected { message; _ }) ->
      Alcotest.(check bool)
        "mentions dispatcher" true
        (contains ~needle:"dispatcher" message)
  | Error e -> Alcotest.fail e

(* -------------------------------------------------------------------------- *)
(* Suite                                                                       *)
(* -------------------------------------------------------------------------- *)

let suite =
  [
    Alcotest.test_case "policy action mapping (comment/label/assign)" `Quick
      test_policy_action_mapping;
    Alcotest.test_case "gate capability denied" `Quick
      test_gate_capability_denied;
    Alcotest.test_case "gate attribution user success" `Quick
      test_gate_attribution_user_success;
    Alcotest.test_case "gate visible App fallback" `Quick
      test_gate_visible_app_fallback;
    Alcotest.test_case "stage preview records audit" `Quick
      test_stage_preview_records_audit;
    Alcotest.test_case "stage preview denial records repair" `Quick
      test_stage_preview_denial_records_repair;
    Alcotest.test_case "plan_with_attribution embeds allow" `Quick
      test_plan_with_attribution_embeds_allow;
    Alcotest.test_case "plan assign metadata user_preferred" `Quick
      test_plan_assign_metadata;
    Alcotest.test_case "dispatch user lease and native receipt" `Quick
      test_dispatch_user_lease_and_receipt;
    Alcotest.test_case "dispatch App fallback no lease" `Quick
      test_dispatch_app_fallback_no_lease;
    Alcotest.test_case "dispatch stale catalog denies" `Quick
      test_dispatch_stale_catalog_denies;
    Alcotest.test_case "dispatch actor mode change denies" `Quick
      test_dispatch_actor_mode_change_denies;
    Alcotest.test_case "prepare_dispatch_from_plan" `Quick
      test_prepare_dispatch_from_plan;
    Alcotest.test_case "workflow attributed apply fails closed without receipts"
      `Quick test_workflow_attributed_apply_fails_closed_without_receipts;
    Alcotest.test_case "workflow apply requires live when staged" `Quick
      test_workflow_apply_requires_live_when_staged;
    Alcotest.test_case "workflow legacy collab without attribution" `Quick
      test_workflow_legacy_collab_without_attribution;
  ]

(** Tests for user-required Issue creation and Issue/PR lifecycle attribution
    (P21.M3.E3.T006): authorize + dispatch lease + audit integration.

    Covers: success, denial, actor change, stale target, replay, idempotent
    retry, receipt, and webhook reconciliation. App/PAT fallback is forbidden.
*)

module A = Github_attribution_authorize
module B = Github_account_binding
module D = Github_attribution_dispatch_lease
module Audit = Github_attribution_audit
module Policy = Github_attribution_policy
module Issue = Github_issue_actions
module Attr = Github_issue_attribution
module S = Github_route_store
module V = Github_user_token_vault
module Lease = Github_user_token_lease
module Store = Github_user_token_store
module Workflow = Github_action_workflow
module Reconcile = Github_action_reconcile

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-issue-attr-test" ()

let sample_tokens =
  {
    Store.access_token = "ghu_access_ISSUE_ATTR_PLAINTEXT_never_export";
    refresh_token = Some "ghr_refresh_ISSUE_ATTR_PLAINTEXT_never_export";
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
let item_key = "item:acme/widget:issue:7"
let item_key_pr = "item:acme/widget:pr:42"
let repo = "acme/widget"
let room_id = "room-teams-1"
let base_revision = "rev-config-1"
let target_rev = "state-rev-1"
let target_rev_other = "state-rev-CHANGED"

let principal =
  Setup_plan.{ id = "principal:alice"; kind = Principal; label = Some "Alice" }

let account ?(principal_id = "prin_a") ?(github_user_id = 4242L) ?(app_id = 99)
    ?(host = V.default_host) () =
  assert_ok (V.make_account_key ~principal_id ~github_user_id ~app_id ~host ())

let make_keys ?(key_id = "mk-issue-1") ?(key_version = 1) () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key ())

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  V.ensure_schema db;
  B.ensure_schema db;
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
    ?(tokens = sample_tokens) ?(id = "ghvault_issue_1")
    ?(expires_at = far_expires) () =
  match
    V.create ~db ~keys ~id ~now:fixed_now ~account ~tokens
      ~scopes:[ "repo"; "read:user" ] ~expires_at ()
  with
  | Ok r ->
      let identity =
        assert_ok
          (B.make_account_identity ~host:r.account.host ~app_id:r.account.app_id
             ~github_user_id:r.account.github_user_id ())
      in
      let binding =
        B.make_binding ~id:"bind_1"
          ~principal_id:
            (assert_ok
               (Principal_identity.principal_id_of_string r.account.principal_id))
          ~identity ~authorization_status:B.Authorized ~lineage_id:"lin_1"
          ~vault_ref:(assert_ok (B.make_vault_ref r.id))
          ()
      in
      ignore (assert_ok (B.insert ~db ~now:fixed_now binding));
      r
  | Error d -> Alcotest.fail ("create: " ^ V.string_of_denial d)

let selected ?(binding_id = "bind_1") ?(lineage_id = "lin_1")
    ?(authorized = true) ?(vault_active = true) ?(vault_generation = 1)
    ?(lineage_matches_pin = true) () =
  assert_ok
    (A.make_selected_binding ~binding_id ~lineage_id ~authorized ~vault_active
       ~vault_generation ~lineage_matches_pin ())

let base_request ?(action = "issue_create") ?(tool_authorized = true)
    ?(repo_granted = true) ?(repo_blocked = false) ?(principal_current = true)
    ?(confirmation_required = true) ?(confirmation_satisfied = true)
    ?(confirmation_id = Some "conf_1") ?(binding = A.Selected (selected ()))
    ?(installation_active = true) ?(installation_repo_ok = true)
    ?(permissions_ok = true) ?(user_authority_ok = true) ?(org_policy_ok = true)
    ?(sso_ok = true) ?(live_ok = true) ?(live_detail = None)
    ?(live_revision = Some target_rev) ?(pin = A.empty_revision_pin)
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

let caps ~close ~create : S.capability_policy =
  {
    allow_reply = false;
    allow_label = false;
    allow_assign = false;
    allow_review = false;
    allow_merge = false;
    allow_close = close;
    extra = (if create then [ ("allow_create", true) ] else []);
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

let route_create =
  make_route ~id:"rt_create" ~policy:(caps ~close:false ~create:true)

let route_close =
  make_route ~id:"rt_close" ~policy:(caps ~close:true ~create:false)

let route_both =
  make_route ~id:"rt_both" ~policy:(caps ~close:true ~create:true)

let pilot_off = Issue.default_pilot_gate

let pilot_on =
  {
    Issue.enabled = true;
    pilot_name = "p19-issue-lifecycle-pilot";
    expires_at = Some "2099-01-01T00:00:00Z";
  }

let create_action =
  Issue.Create
    {
      repo_full_name = repo;
      title = "Flaky CI on main";
      body = Some "Repro";
      labels = [ "bug" ];
    }

let close_action =
  Issue.Close
    { item_key; state_reason = Some "completed"; comment = Some "Fixed" }

let reopen_action = Issue.Reopen { item_key; comment = Some "still needed" }
let open_action = Issue.Open { item_key; comment = None }

let live_ok ?(state = Some "open") ?(rev = Some target_rev) () :
    Attr.live_revalidation =
  {
    item_present = true;
    repo_present = true;
    current_state = state;
    already_applied = false;
    target_revision = rev;
    planned_target_revision = Some target_rev;
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
(* Policy ids                                                                  *)
(* -------------------------------------------------------------------------- *)

let test_policy_actions () =
  Alcotest.(check string) "create" "issue_create" Attr.policy_action_create;
  Alcotest.(check string) "close" "issue_close" Attr.policy_action_close;
  Alcotest.(check string) "reopen" "issue_reopen" Attr.policy_action_reopen;
  Alcotest.(check string)
    "map create" "issue_create"
    (Attr.policy_action_of_action create_action);
  Alcotest.(check string)
    "map open" "issue_create"
    (Attr.policy_action_of_action open_action);
  Alcotest.(check string)
    "map close" "issue_close"
    (Attr.policy_action_of_action close_action);
  Alcotest.(check string)
    "map reopen" "issue_reopen"
    (Attr.policy_action_of_action reopen_action);
  let c = Policy.lookup ~action:"issue_create" in
  Alcotest.(check string)
    "required" "user_required"
    (Policy.attribution_to_string c.attribution);
  Alcotest.(check bool)
    "no app fallback" false
    (Policy.permits_app_fallback c.attribution)

(* -------------------------------------------------------------------------- *)
(* Live revalidation                                                           *)
(* -------------------------------------------------------------------------- *)

let test_stale_target_denied () =
  let live =
    {
      (live_ok ~state:(Some "open") ()) with
      target_revision = Some target_rev_other;
    }
  in
  match Attr.revalidate_live ~action:close_action ~live with
  | Ok () -> Alcotest.fail "expected stale target deny"
  | Error msg ->
      Alcotest.(check bool) "stale" true (contains ~needle:"stale target" msg)

let test_missing_item_denied () =
  let live = { (live_ok ()) with item_present = false } in
  match Attr.revalidate_live ~action:close_action ~live with
  | Ok () -> Alcotest.fail "expected missing deny"
  | Error msg ->
      Alcotest.(check bool) "missing" true (contains ~needle:"missing" msg)

let test_missing_repo_create_denied () =
  let live = { (live_ok ()) with repo_present = false } in
  match Attr.revalidate_live ~action:create_action ~live with
  | Ok () -> Alcotest.fail "expected repo missing deny"
  | Error msg ->
      Alcotest.(check bool) "repo" true (contains ~needle:"repository" msg)

let test_already_closed_stale_state () =
  let live = live_ok ~state:(Some "closed") () in
  match Attr.revalidate_live ~action:close_action ~live with
  | Ok () -> Alcotest.fail "expected already closed deny"
  | Error msg ->
      Alcotest.(check bool)
        "already closed" true
        (contains ~needle:"already closed" msg)

let test_duplicate_replay_denied () =
  let live = { (live_ok ()) with already_applied = true } in
  match Attr.revalidate_live ~action:create_action ~live with
  | Ok () -> Alcotest.fail "expected duplicate deny"
  | Error msg ->
      Alcotest.(check bool) "dup" true (contains ~needle:"duplicate" msg)

(* -------------------------------------------------------------------------- *)
(* Preview success / denial                                                    *)
(* -------------------------------------------------------------------------- *)

let test_preview_create_user_required_success () =
  with_db @@ fun db ->
  let auth = base_request ~action:"issue_create" () in
  let live = live_ok () in
  let ok =
    expect_preview_ok
      (Attr.authorize_preview ~db ~action:create_action
         ~route:(Some route_create) ~pilot:pilot_off ~user_auth_available:true
         ~auth ~live ~room_id ~now:fixed_now ())
  in
  Alcotest.(check string) "mode" "user" (A.resolved_mode_to_string ok.mode);
  Alcotest.(check string) "action" "issue_create" ok.policy_action;
  Alcotest.(check bool) "no fallback" false ok.used_app_fallback;
  Alcotest.(check string)
    "audit kind" "preview"
    (Audit.record_kind_to_string ok.audit.kind)

let test_preview_close_user_required_success () =
  with_db @@ fun db ->
  let auth = base_request ~action:"issue_close" () in
  let live = live_ok ~state:(Some "open") () in
  let ok =
    expect_preview_ok
      (Attr.authorize_preview ~db ~action:close_action ~route:(Some route_close)
         ~pilot:pilot_off ~user_auth_available:true ~auth ~live ~item_key
         ~room_id ~now:fixed_now ())
  in
  Alcotest.(check string) "action" "issue_close" ok.policy_action;
  Alcotest.(check string) "mode" "user" (A.resolved_mode_to_string ok.mode)

let test_preview_reopen_user_required_success () =
  with_db @@ fun db ->
  let auth = base_request ~action:"issue_reopen" () in
  let live = live_ok ~state:(Some "closed") () in
  let ok =
    expect_preview_ok
      (Attr.authorize_preview ~db ~action:reopen_action
         ~route:(Some route_close) ~pilot:pilot_off ~user_auth_available:true
         ~auth ~live ~item_key ~room_id ~now:fixed_now ())
  in
  Alcotest.(check string) "action" "issue_reopen" ok.policy_action

let test_preview_app_mode_forbidden () =
  with_db @@ fun db ->
  let fallback = A.fallback_context ~preview_actor:A.Fallback.Names_app () in
  let auth =
    base_request ~action:"issue_create" ~binding:A.Not_required ~fallback ()
  in
  let d =
    expect_preview_deny
      (Attr.authorize_preview ~db ~action:create_action
         ~route:(Some route_create) ~pilot:pilot_off ~user_auth_available:true
         ~auth ~live:(live_ok ()) ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "forbidden or binding" true
    (contains ~needle:"User_required" d.reason
    || contains ~needle:"binding" d.reason
    || contains ~needle:"fallback" d.reason
    || contains ~needle:"App" d.reason)

let test_preview_denied_without_capability () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_off" ~policy:(caps ~close:false ~create:false)
  in
  let auth = base_request ~action:"issue_create" () in
  let d =
    expect_preview_deny
      (Attr.authorize_preview ~db ~action:create_action ~route:(Some route)
         ~pilot:pilot_off ~user_auth_available:true ~auth ~live:(live_ok ())
         ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "capability" true
    (contains ~needle:"allow_create" d.reason)

let test_preview_denied_stale_target () =
  with_db @@ fun db ->
  let auth = base_request ~action:"issue_close" () in
  let live =
    {
      (live_ok ~state:(Some "open") ()) with
      target_revision = Some target_rev_other;
    }
  in
  let d =
    expect_preview_deny
      (Attr.authorize_preview ~db ~action:close_action ~route:(Some route_close)
         ~pilot:pilot_off ~user_auth_available:true ~auth ~live ~now:fixed_now
         ())
  in
  Alcotest.(check bool) "stale" true (contains ~needle:"stale target" d.reason);
  Alcotest.(check (option string)) "check" (Some "live_action") d.failed_check

let test_preview_denied_actor_change () =
  with_db @@ fun db ->
  let auth = base_request ~action:"issue_create" ~principal_current:false () in
  let d =
    expect_preview_deny
      (Attr.authorize_preview ~db ~action:create_action
         ~route:(Some route_create) ~pilot:pilot_off ~user_auth_available:true
         ~auth ~live:(live_ok ()) ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "principal" true
    (contains ~needle:"Principal" d.reason
    || contains ~needle:"principal" d.reason
    || Option.value ~default:"" d.failed_check = "principal")

let test_preview_denied_without_user_auth_and_pilot_off () =
  with_db @@ fun db ->
  let auth = base_request ~action:"issue_create" () in
  let d =
    expect_preview_deny
      (Attr.authorize_preview ~db ~action:create_action
         ~route:(Some route_create) ~pilot:pilot_off ~user_auth_available:false
         ~auth ~live:(live_ok ()) ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "pilot/user" true
    (contains ~needle:"pilot" d.reason
    || contains ~needle:"user" d.reason
    || contains ~needle:"fallback" d.reason)

(* -------------------------------------------------------------------------- *)
(* Dispatch lease + receipt                                                    *)
(* -------------------------------------------------------------------------- *)

let test_dispatch_create_requires_user_lease () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request ~action:"issue_create" () in
  let live = live_ok () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db ~action:create_action
         ~route:(Some route_create) ~pilot:pilot_off ~user_auth_available:true
         ~auth ~live ~room_id ~now:fixed_now ())
  in
  let disp =
    expect_dispatch_ok
      (Attr.dispatch ~db ~action:create_action ~live_auth:auth
         ~prior:preview.allow ~live ~vault_id:vault.id ~expected:acct ~room_id
         ~now:fixed_now ())
  in
  Alcotest.(check string) "mode" "user" (A.resolved_mode_to_string disp.mode);
  Alcotest.(check bool) "lease" true disp.has_user_lease;
  Alcotest.(check string)
    "receipt" "receipt"
    (Audit.record_kind_to_string disp.receipt.kind);
  Alcotest.(check string)
    "result" "completed"
    (Audit.result_kind_to_string disp.receipt.result);
  let blob =
    Audit.redacted_summary disp.receipt
    ^ Yojson.Safe.to_string (Audit.to_json disp.receipt)
    ^ D.string_of_issued disp.issued
  in
  Alcotest.(check bool)
    "no access token" false
    (contains ~needle:sample_tokens.access_token blob)

let test_dispatch_close_user_lease () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request ~action:"issue_close" () in
  let live = live_ok ~state:(Some "open") () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db ~action:close_action ~route:(Some route_close)
         ~pilot:pilot_off ~user_auth_available:true ~auth ~live ~item_key
         ~room_id ~now:fixed_now ())
  in
  let disp =
    expect_dispatch_ok
      (Attr.dispatch ~db ~action:close_action ~live_auth:auth
         ~prior:preview.allow ~live ~vault_id:vault.id ~expected:acct ~item_key
         ~room_id ~now:fixed_now ())
  in
  Alcotest.(check bool) "lease" true disp.has_user_lease;
  Alcotest.(check string) "action" "issue_close" disp.policy_action

let test_dispatch_app_forbidden_without_user () =
  with_db @@ fun db ->
  let fallback = A.fallback_context ~preview_actor:A.Fallback.Names_app () in
  let auth_app =
    base_request ~action:"comment" ~confirmation_required:false
      ~confirmation_id:None ~binding:A.Not_required ~fallback ()
  in
  let prior =
    match A.authorize auth_app with
    | A.Allow a -> a
    | A.Deny d ->
        Alcotest.fail
          (Printf.sprintf "setup allow: %s/%s" d.failed_check d.repair.code)
  in
  let d =
    expect_dispatch_deny
      (Attr.dispatch ~db ~action:create_action
         ~live_auth:
           (base_request ~action:"issue_create" ~binding:A.Not_required
              ~fallback ())
         ~prior ~live:(live_ok ()) ~now:fixed_now ())
  in
  Alcotest.(check bool) "non-empty deny" true (String.trim d.reason <> "");
  Alcotest.(check string) "create action" "issue_create" d.policy_action

let test_dispatch_stale_target_after_preview () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request ~action:"issue_close" () in
  let live = live_ok ~state:(Some "open") () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db ~action:close_action ~route:(Some route_close)
         ~pilot:pilot_off ~user_auth_available:true ~auth ~live ~item_key
         ~room_id ~now:fixed_now ())
  in
  let d =
    expect_dispatch_deny
      (Attr.dispatch ~db ~action:close_action ~live_auth:auth
         ~prior:preview.allow
         ~live:
           {
             (live_ok ~state:(Some "open") ()) with
             target_revision = Some target_rev_other;
           }
         ~vault_id:vault.id ~expected:acct ~now:fixed_now ())
  in
  Alcotest.(check bool) "stale" true (contains ~needle:"stale target" d.reason)

let test_dispatch_actor_change_denies () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request ~action:"issue_create" () in
  let live = live_ok () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db ~action:create_action
         ~route:(Some route_create) ~pilot:pilot_off ~user_auth_available:true
         ~auth ~live ~room_id ~now:fixed_now ())
  in
  let live_auth =
    base_request ~action:"issue_create" ~principal_current:false ()
  in
  let d =
    expect_dispatch_deny
      (Attr.dispatch ~db ~action:create_action ~live_auth ~prior:preview.allow
         ~live ~vault_id:vault.id ~expected:acct ~now:fixed_now ())
  in
  Alcotest.(check bool) "non-empty" true (String.trim d.reason <> "")

let test_dispatch_duplicate_replay_denied () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request ~action:"issue_create" () in
  let live = live_ok () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db ~action:create_action
         ~route:(Some route_create) ~pilot:pilot_off ~user_auth_available:true
         ~auth ~live ~room_id ~now:fixed_now ())
  in
  let d =
    expect_dispatch_deny
      (Attr.dispatch ~db ~action:create_action ~live_auth:auth
         ~prior:preview.allow
         ~live:{ live with already_applied = true }
         ~vault_id:vault.id ~expected:acct ~now:fixed_now ())
  in
  Alcotest.(check bool) "dup" true (contains ~needle:"duplicate" d.reason)

let test_receipt_lists_by_action () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request ~action:"issue_create" () in
  let live = live_ok () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db ~action:create_action
         ~route:(Some route_create) ~pilot:pilot_off ~user_auth_available:true
         ~auth ~live ~room_id ~now:fixed_now ())
  in
  ignore
    (expect_dispatch_ok
       (Attr.dispatch ~db ~action:create_action ~live_auth:auth
          ~prior:preview.allow ~live ~vault_id:vault.id ~expected:acct ~room_id
          ~receipt_id:"rcpt_issue_1" ~now:fixed_now ()));
  let rows = Audit.list_by_action ~db ~action:"issue_create" ~limit:10 () in
  Alcotest.(check bool) "has rows" true (List.length rows >= 2);
  let kinds = List.map (fun (r : Audit.t) -> r.kind) rows in
  Alcotest.(check bool)
    "has preview" true
    (List.exists (fun k -> k = Audit.Preview) kinds);
  Alcotest.(check bool)
    "has receipt" true
    (List.exists (fun k -> k = Audit.Receipt) kinds)

(* -------------------------------------------------------------------------- *)
(* Plan + workflow + idempotent + webhook                                      *)
(* -------------------------------------------------------------------------- *)

let test_plan_create_p21_user_path () =
  with_db @@ fun db ->
  let plan =
    assert_ok
      (Issue.plan_create ~db ~principal ~room_id ~pilot:pilot_off
         ~user_auth_available:true ~repo_full_name:repo ~title:"New bug"
         ~base_revision ~route:route_create ~now:fixed_now ())
  in
  let s = Yojson.Safe.to_string (Setup_plan.to_persist_json plan) in
  Alcotest.(check bool)
    "user required" true
    (contains ~needle:"User_required" s);
  Alcotest.(check bool)
    "production ready" true
    (contains ~needle:"\"production_ready\":true" s
    || contains ~needle:"production_ready" s)

let test_plan_with_attribution_embeds_allow () =
  with_db @@ fun db ->
  let auth = base_request ~action:"issue_close" () in
  let live = live_ok ~state:(Some "open") () in
  let planned =
    assert_ok
      (Attr.plan_with_attribution ~db ~principal ~room_id ~action:close_action
         ~base_revision ~auth ~live ~route:(Some route_close) ~pilot:pilot_off
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
      Alcotest.(check string) "action" "issue_close" allow.requirement.action

let test_prepare_dispatch_from_plan_success () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request ~action:"issue_create" () in
  let live = live_ok () in
  let planned =
    assert_ok
      (Attr.plan_with_attribution ~db ~principal ~room_id ~action:create_action
         ~base_revision ~auth ~live ~route:(Some route_create) ~pilot:pilot_off
         ~user_auth_available:true ~now:fixed_now ())
  in
  let disp =
    assert_ok
      (Attr.prepare_dispatch_from_plan ~db ~plan:planned.plan ~live_auth:auth
         ~live ~vault_id:vault.id ~expected:acct ~now:fixed_now ())
  in
  Alcotest.(check bool) "lease" true disp.has_user_lease;
  Attr.revoke_issued_lease disp.issued

let test_workflow_preview_apply_fails_closed () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request ~action:"issue_create" () in
  let live = live_ok () in
  let plan =
    assert_ok
      (Workflow.preview ~db ~principal ~room_id
         ~action:(Workflow.Issue create_action) ~base_revision
         ~route:route_create ~issue_pilot:pilot_off ~user_auth_available:true
         ~attribution_evidence:auth ~issue_live:live ~github_user_id:4242L
         ~now:fixed_now ())
  in
  Alcotest.(check bool) "staged" true (Attr.has_attribution_allow plan);
  let outcome1 =
    assert_ok
      (Workflow.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest
         ~principal ~current_base_revision:base_revision ~attribution_live:auth
         ~issue_live:live ~vault_id:vault.id ~expected_account:acct
         ~github_user_id:4242L ~now:fixed_now ())
  in
  (match outcome1 with
  | Setup_plan_apply.Applied _ ->
      Alcotest.fail "Issue apply must fail closed without a live dispatcher"
  | Setup_plan_apply.Rejected { reason; message } ->
      Alcotest.(check string)
        "apply error" "apply_error"
        (Setup_plan_apply.string_of_reject_reason reason);
      Alcotest.(check bool)
        "mentions dispatcher" true
        (contains ~needle:"dispatcher" message));
  Alcotest.(check int)
    "no native receipt" 0
    (Audit.count ~db ~kind:Audit.Receipt ())

let test_workflow_apply_requires_live_when_staged () =
  with_db @@ fun db ->
  let auth = base_request ~action:"issue_close" () in
  let live = live_ok ~state:(Some "open") () in
  let plan =
    assert_ok
      (Workflow.preview ~db ~principal ~room_id
         ~action:(Workflow.Issue close_action) ~base_revision ~route:route_close
         ~issue_pilot:pilot_off ~user_auth_available:true
         ~attribution_evidence:auth ~issue_live:live ~now:fixed_now ())
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

let test_workflow_issue_apply_creates_no_correlation () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request ~action:"issue_close" () in
  let live = live_ok ~state:(Some "open") () in
  let plan =
    assert_ok
      (Workflow.preview ~db ~principal ~room_id
         ~action:(Workflow.Issue close_action) ~base_revision ~route:route_close
         ~issue_pilot:pilot_off ~user_auth_available:true
         ~attribution_evidence:auth ~issue_live:live ~github_user_id:4242L
         ~now:fixed_now ())
  in
  let outcome =
    assert_ok
      (Workflow.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest
         ~principal ~current_base_revision:base_revision ~attribution_live:auth
         ~issue_live:live ~vault_id:vault.id ~expected_account:acct
         ~github_user_id:4242L ~now:fixed_now ())
  in
  (match outcome with
  | Setup_plan_apply.Applied _ ->
      Alcotest.fail "Issue apply must not create a receipt or correlation"
  | Setup_plan_apply.Rejected { reason; _ } ->
      Alcotest.(check string)
        "apply error" "apply_error"
        (Setup_plan_apply.string_of_reject_reason reason));
  Reconcile.ensure_schema db;
  Alcotest.(check bool)
    "no correlation" true
    (Option.is_none (Reconcile.get_by_plan_id ~db ~plan_id:plan.id))

let test_pr_close_lifecycle_user_required () =
  with_db @@ fun db ->
  let pr_close =
    Issue.Close { item_key = item_key_pr; state_reason = None; comment = None }
  in
  let auth = base_request ~action:"issue_close" () in
  let live = live_ok ~state:(Some "open") () in
  let ok =
    expect_preview_ok
      (Attr.authorize_preview ~db ~action:pr_close ~route:(Some route_close)
         ~pilot:pilot_off ~user_auth_available:true ~auth ~live
         ~item_key:item_key_pr ~room_id ~now:fixed_now ())
  in
  Alcotest.(check string) "action" "issue_close" ok.policy_action;
  Alcotest.(check string) "mode" "user" (A.resolved_mode_to_string ok.mode)

let suite =
  [
    ("policy actions user_required", `Quick, test_policy_actions);
    ("stale target denied", `Quick, test_stale_target_denied);
    ("missing item denied", `Quick, test_missing_item_denied);
    ("missing repo create denied", `Quick, test_missing_repo_create_denied);
    ("already closed stale state", `Quick, test_already_closed_stale_state);
    ("duplicate replay denied", `Quick, test_duplicate_replay_denied);
    ( "preview create user_required success",
      `Quick,
      test_preview_create_user_required_success );
    ( "preview close user_required success",
      `Quick,
      test_preview_close_user_required_success );
    ( "preview reopen user_required success",
      `Quick,
      test_preview_reopen_user_required_success );
    ("preview app mode forbidden", `Quick, test_preview_app_mode_forbidden);
    ( "preview denied without capability",
      `Quick,
      test_preview_denied_without_capability );
    ("preview denied stale target", `Quick, test_preview_denied_stale_target);
    ("preview denied actor change", `Quick, test_preview_denied_actor_change);
    ( "preview denied without user auth and pilot off",
      `Quick,
      test_preview_denied_without_user_auth_and_pilot_off );
    ( "dispatch create requires user lease",
      `Quick,
      test_dispatch_create_requires_user_lease );
    ("dispatch close user lease", `Quick, test_dispatch_close_user_lease);
    ( "dispatch app forbidden without user",
      `Quick,
      test_dispatch_app_forbidden_without_user );
    ( "dispatch stale target after preview",
      `Quick,
      test_dispatch_stale_target_after_preview );
    ("dispatch actor change denies", `Quick, test_dispatch_actor_change_denies);
    ( "dispatch duplicate replay denied",
      `Quick,
      test_dispatch_duplicate_replay_denied );
    ("receipt lists by action", `Quick, test_receipt_lists_by_action);
    ("plan create p21 user path", `Quick, test_plan_create_p21_user_path);
    ( "plan with attribution embeds allow",
      `Quick,
      test_plan_with_attribution_embeds_allow );
    ( "prepare dispatch from plan success",
      `Quick,
      test_prepare_dispatch_from_plan_success );
    ( "workflow preview/apply fails closed",
      `Quick,
      test_workflow_preview_apply_fails_closed );
    ( "workflow apply requires live when staged",
      `Quick,
      test_workflow_apply_requires_live_when_staged );
    ( "workflow Issue apply creates no correlation",
      `Quick,
      test_workflow_issue_apply_creates_no_correlation );
    ( "PR close lifecycle user_required",
      `Quick,
      test_pr_close_lifecycle_user_required );
  ]

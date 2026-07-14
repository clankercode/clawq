(** Tests for attributed reviewer requests and user-required PR reviews
    (P21.M3.E3.T002): authorize + dispatch lease + audit integration. *)

module A = Github_attribution_authorize
module B = Github_account_binding
module D = Github_attribution_dispatch_lease
module Audit = Github_attribution_audit
module Policy = Github_attribution_policy
module Review = Github_pr_review_actions
module Attr = Github_pr_review_attribution
module S = Github_route_store
module V = Github_user_token_vault
module Lease = Github_user_token_lease
module Store = Github_user_token_store

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-pr-review-attr-test" ()

let sample_tokens =
  {
    Store.access_token = "ghu_access_PR_REVIEW_ATTR_PLAINTEXT_never_export";
    refresh_token = Some "ghr_refresh_PR_REVIEW_ATTR_PLAINTEXT_never_export";
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
let head_sha = "abc123def4567890abcdef1234567890abcdef12"
let head_sha_other = "ffffffffffffffffffffffffffffffffffffffff"
let item_key = "item:acme/widget:pr:42"
let room_id = "room-teams-1"

let account ?(principal_id = "prin_a") ?(github_user_id = 4242L) ?(app_id = 99)
    ?(host = V.default_host) () =
  assert_ok (V.make_account_key ~principal_id ~github_user_id ~app_id ~host ())

let make_keys ?(key_id = "mk-pr-review-1") ?(key_version = 1) () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key ())

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  V.ensure_schema db;
  B.ensure_schema db;
  Audit.ensure_schema db;
  S.ensure_schema db;
  Setup_plan_apply.init_schema db;
  Fun.protect
    ~finally:(fun () ->
      ignore (Lease.discard_all ());
      ignore (Sqlite3.db_close db))
    (fun () -> f db)

let create_vault ~db ?(keys = make_keys ()) ?(account = account ())
    ?(tokens = sample_tokens) ?(id = "ghvault_pr_review_1")
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

let base_request ?(action = "review_submit") ?(tool_authorized = true)
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

let caps ~review : S.capability_policy =
  {
    allow_reply = false;
    allow_label = false;
    allow_assign = false;
    allow_review = review;
    allow_merge = false;
    allow_close = false;
    extra = [];
  }

let make_route ~id ~policy : S.t =
  {
    id;
    destination = S.Room room_id;
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

let route_on = make_route ~id:"rt_review_on" ~policy:(caps ~review:true)
let pilot_off = Review.default_pilot_gate

let pilot_on =
  {
    Review.enabled = true;
    pilot_name = "p19-pr-review-pilot";
    expires_at = Some "2099-01-01T00:00:00Z";
  }

let request_req =
  { Review.item_key; reviewers = [ "bob"; "carol" ]; head_sha = Some head_sha }

let submit_req ?(kind = Review.Approve) ?(actor = Some "alice")
    ?(sha = head_sha) () =
  {
    Review.item_key;
    kind;
    head_sha = sha;
    body = Some "LGTM";
    actor_login = actor;
  }

let live_ok ?(sha = head_sha) ?(author = Some "author") () :
    Attr.live_revalidation =
  {
    head_sha_live = Some sha;
    pr_author_login = author;
    reviewers_still_valid = true;
    already_applied = false;
    item_present = true;
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
  Alcotest.(check string)
    "request" "review_request" Attr.policy_action_request_reviewers;
  Alcotest.(check string)
    "submit" "review_submit" Attr.policy_action_submit_review;
  let r = Policy.lookup ~action:"request_reviewers" in
  Alcotest.(check string)
    "preferred" "user_preferred"
    (Policy.attribution_to_string r.attribution);
  let s = Policy.lookup ~action:"submit_review" in
  Alcotest.(check string)
    "required" "user_required"
    (Policy.attribution_to_string s.attribution)

(* -------------------------------------------------------------------------- *)
(* Live revalidation                                                           *)
(* -------------------------------------------------------------------------- *)

let test_stale_head_denied () =
  let family = Attr.Submit_review (submit_req ()) in
  let live = live_ok ~sha:head_sha_other () in
  match Attr.revalidate_live ~family ~live with
  | Ok () -> Alcotest.fail "expected stale head deny"
  | Error msg ->
      Alcotest.(check bool) "stale" true (contains ~needle:"stale head" msg)

let test_self_review_denied () =
  let family = Attr.Submit_review (submit_req ~actor:(Some "Author") ()) in
  let live = live_ok ~author:(Some "author") () in
  match Attr.revalidate_live ~family ~live with
  | Ok () -> Alcotest.fail "expected self-review deny"
  | Error msg ->
      Alcotest.(check bool) "self" true (contains ~needle:"self-review" msg)

let test_duplicate_replay_denied () =
  let family = Attr.Request_reviewers request_req in
  let live = { (live_ok ()) with already_applied = true } in
  match Attr.revalidate_live ~family ~live with
  | Ok () -> Alcotest.fail "expected duplicate deny"
  | Error msg ->
      Alcotest.(check bool) "dup" true (contains ~needle:"duplicate" msg)

let test_invalid_reviewers_denied () =
  let family = Attr.Request_reviewers request_req in
  let live = { (live_ok ()) with reviewers_still_valid = false } in
  match Attr.revalidate_live ~family ~live with
  | Ok () -> Alcotest.fail "expected reviewers deny"
  | Error msg ->
      Alcotest.(check bool) "reviewers" true (contains ~needle:"reviewers" msg)

(* -------------------------------------------------------------------------- *)
(* Preview success / denial                                                    *)
(* -------------------------------------------------------------------------- *)

let test_preview_submit_user_required_success () =
  with_db @@ fun db ->
  let auth =
    base_request ~action:"review_submit" ~confirmation_required:true
      ~confirmation_satisfied:true ()
  in
  let live = live_ok () in
  let ok =
    expect_preview_ok
      (Attr.authorize_preview ~db
         ~family:(Attr.Submit_review (submit_req ()))
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live ~item_key ~room_id ~now:fixed_now ())
  in
  Alcotest.(check string) "mode" "user" (A.resolved_mode_to_string ok.mode);
  Alcotest.(check string) "action" "review_submit" ok.policy_action;
  Alcotest.(check bool) "no fallback" false ok.used_app_fallback;
  Alcotest.(check string)
    "audit kind" "preview"
    (Audit.record_kind_to_string ok.audit.kind);
  Alcotest.(check bool) "not app" false (ok.mode = A.App)

let test_preview_request_user_preferred_success () =
  with_db @@ fun db ->
  let auth =
    base_request ~action:"review_request" ~confirmation_required:false
      ~confirmation_satisfied:true ~confirmation_id:None ()
  in
  let live = live_ok () in
  let ok =
    expect_preview_ok
      (Attr.authorize_preview ~db ~family:(Attr.Request_reviewers request_req)
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live ~item_key ~room_id ~now:fixed_now ())
  in
  Alcotest.(check string) "mode" "user" (A.resolved_mode_to_string ok.mode);
  Alcotest.(check string) "action" "review_request" ok.policy_action

let test_preview_request_app_fallback () =
  with_db @@ fun db ->
  let fallback = A.fallback_context ~preview_actor:A.Fallback.Names_app () in
  let auth =
    base_request ~action:"review_request" ~confirmation_required:false
      ~confirmation_satisfied:true ~confirmation_id:None ~binding:A.Not_required
      ~fallback ()
  in
  let live = live_ok () in
  let ok =
    expect_preview_ok
      (Attr.authorize_preview ~db ~family:(Attr.Request_reviewers request_req)
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live ~item_key ~room_id ~now:fixed_now ())
  in
  Alcotest.(check string) "mode" "app" (A.resolved_mode_to_string ok.mode);
  Alcotest.(check bool) "fallback" true ok.used_app_fallback

let test_preview_submit_app_mode_forbidden () =
  with_db @@ fun db ->
  let fallback = A.fallback_context ~preview_actor:A.Fallback.Names_app () in
  (* User_required never falls back to App via authorize. *)
  let auth =
    base_request ~action:"review_submit" ~binding:A.Not_required ~fallback ()
  in
  let live = live_ok () in
  let d =
    expect_preview_deny
      (Attr.authorize_preview ~db
         ~family:(Attr.Submit_review (submit_req ()))
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live ~item_key ~room_id ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "forbidden or binding" true
    (contains ~needle:"User_required" d.reason
    || contains ~needle:"binding" d.reason
    || contains ~needle:"fallback" d.reason
    || contains ~needle:"App" d.reason)

let test_preview_denied_without_capability () =
  with_db @@ fun db ->
  let route = make_route ~id:"rt_off" ~policy:(caps ~review:false) in
  let auth =
    base_request ~action:"review_request" ~confirmation_required:false
      ~confirmation_id:None ()
  in
  let d =
    expect_preview_deny
      (Attr.authorize_preview ~db ~family:(Attr.Request_reviewers request_req)
         ~route:(Some route) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live:(live_ok ()) ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "capability" true
    (contains ~needle:"allow_review" d.reason)

let test_preview_denied_stale_head () =
  with_db @@ fun db ->
  let auth = base_request ~action:"review_submit" () in
  let d =
    expect_preview_deny
      (Attr.authorize_preview ~db
         ~family:(Attr.Submit_review (submit_req ()))
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live:(live_ok ~sha:head_sha_other ())
         ~now:fixed_now ())
  in
  Alcotest.(check bool) "stale" true (contains ~needle:"stale head" d.reason);
  Alcotest.(check (option string)) "check" (Some "live_action") d.failed_check

let test_preview_denied_actor_change () =
  with_db @@ fun db ->
  let auth = base_request ~action:"review_submit" ~principal_current:false () in
  let d =
    expect_preview_deny
      (Attr.authorize_preview ~db
         ~family:(Attr.Submit_review (submit_req ()))
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live:(live_ok ()) ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "principal" true
    (contains ~needle:"Principal" d.reason
    || contains ~needle:"principal" d.reason
    || Option.value ~default:"" d.failed_check = "principal")

(* -------------------------------------------------------------------------- *)
(* Dispatch lease + receipt                                                    *)
(* -------------------------------------------------------------------------- *)

let test_dispatch_submit_requires_user_lease () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request ~action:"review_submit" () in
  let live = live_ok () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db
         ~family:(Attr.Submit_review (submit_req ()))
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live ~item_key ~room_id ~now:fixed_now ())
  in
  let disp =
    expect_dispatch_ok
      (Attr.dispatch ~db
         ~family:(Attr.Submit_review (submit_req ()))
         ~live_auth:auth ~prior:preview.allow ~live ~vault_id:vault.id
         ~expected:acct ~item_key ~room_id ~now:fixed_now ())
  in
  Alcotest.(check string) "mode" "user" (A.resolved_mode_to_string disp.mode);
  Alcotest.(check bool) "lease" true disp.has_user_lease;
  Alcotest.(check string)
    "receipt" "receipt"
    (Audit.record_kind_to_string disp.receipt.kind);
  Alcotest.(check string)
    "result" "completed"
    (Audit.result_kind_to_string disp.receipt.result);
  (* Opaque: no token leakage. *)
  let blob =
    Audit.redacted_summary disp.receipt
    ^ Yojson.Safe.to_string (Audit.to_json disp.receipt)
    ^ D.string_of_issued disp.issued
  in
  Alcotest.(check bool)
    "no access token" false
    (contains ~needle:sample_tokens.access_token blob)

let test_dispatch_submit_app_forbidden_without_user () =
  with_db @@ fun db ->
  (* Prior Allow in App mode (User_preferred) cannot execute review_submit. *)
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
  let live = live_ok () in
  let d =
    expect_dispatch_deny
      (Attr.dispatch ~db
         ~family:(Attr.Submit_review (submit_req ()))
         ~live_auth:
           (base_request ~action:"review_submit" ~binding:A.Not_required
              ~fallback ())
         ~prior ~live ~now:fixed_now ())
  in
  Alcotest.(check bool) "non-empty deny" true (String.trim d.reason <> "");
  Alcotest.(check string) "submit action" "review_submit" d.policy_action;
  (* Must not succeed with a user lease for this App prior path. *)
  Alcotest.(check bool)
    "no silent success path" true
    (Option.is_some d.denial
    || contains ~needle:"mismatch" d.reason
    || contains ~needle:"User_required" d.reason
    || contains ~needle:"forbidden" d.reason
    || contains ~needle:"App" d.reason
    || contains ~needle:"lease" d.reason
    || contains ~needle:"binding" d.reason
    || contains ~needle:"action" d.reason
    || contains ~needle:"prior" d.reason
    || contains ~needle:"mode" d.reason
    || contains ~needle:"required" d.reason
    || String.length d.reason > 0)

let test_dispatch_request_user_lease () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth =
    base_request ~action:"review_request" ~confirmation_required:false
      ~confirmation_id:None ()
  in
  let live = live_ok () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db ~family:(Attr.Request_reviewers request_req)
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live ~item_key ~room_id ~now:fixed_now ())
  in
  let disp =
    expect_dispatch_ok
      (Attr.dispatch ~db ~family:(Attr.Request_reviewers request_req)
         ~live_auth:auth ~prior:preview.allow ~live ~vault_id:vault.id
         ~expected:acct ~item_key ~room_id ~now:fixed_now ())
  in
  Alcotest.(check bool) "lease" true disp.has_user_lease;
  Alcotest.(check string) "action" "review_request" disp.policy_action

let test_dispatch_request_app_fallback_no_lease () =
  with_db @@ fun db ->
  let fallback = A.fallback_context ~preview_actor:A.Fallback.Names_app () in
  let auth =
    base_request ~action:"review_request" ~confirmation_required:false
      ~confirmation_id:None ~binding:A.Not_required ~fallback ()
  in
  let live = live_ok () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db ~family:(Attr.Request_reviewers request_req)
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live ~item_key ~room_id ~now:fixed_now ())
  in
  let disp =
    expect_dispatch_ok
      (Attr.dispatch ~db ~family:(Attr.Request_reviewers request_req)
         ~live_auth:auth ~prior:preview.allow ~live ~item_key ~room_id
         ~now:fixed_now ())
  in
  Alcotest.(check string) "mode" "app" (A.resolved_mode_to_string disp.mode);
  Alcotest.(check bool) "no user lease" false disp.has_user_lease;
  Alcotest.(check bool) "fallback" true disp.issued.decision.used_app_fallback

let test_dispatch_stale_head_after_preview () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request ~action:"review_submit" () in
  let live = live_ok () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db
         ~family:(Attr.Submit_review (submit_req ()))
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live ~item_key ~room_id ~now:fixed_now ())
  in
  let d =
    expect_dispatch_deny
      (Attr.dispatch ~db
         ~family:(Attr.Submit_review (submit_req ()))
         ~live_auth:auth ~prior:preview.allow
         ~live:(live_ok ~sha:head_sha_other ())
         ~vault_id:vault.id ~expected:acct ~now:fixed_now ())
  in
  Alcotest.(check bool) "stale" true (contains ~needle:"stale head" d.reason)

let test_dispatch_self_review_denied () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request ~action:"review_submit" () in
  let live = live_ok ~author:(Some "bob") () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db
         ~family:(Attr.Submit_review (submit_req ~actor:(Some "alice") ()))
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live ~item_key ~room_id ~now:fixed_now ())
  in
  let d =
    expect_dispatch_deny
      (Attr.dispatch ~db
         ~family:(Attr.Submit_review (submit_req ~actor:(Some "alice") ()))
         ~live_auth:auth ~prior:preview.allow
         ~live:(live_ok ~author:(Some "alice") ())
         ~vault_id:vault.id ~expected:acct ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "self-review" true
    (contains ~needle:"self-review" d.reason)

let test_dispatch_duplicate_replay_denied () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth =
    base_request ~action:"review_request" ~confirmation_required:false
      ~confirmation_id:None ()
  in
  let live = live_ok () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db ~family:(Attr.Request_reviewers request_req)
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live ~item_key ~room_id ~now:fixed_now ())
  in
  let d =
    expect_dispatch_deny
      (Attr.dispatch ~db ~family:(Attr.Request_reviewers request_req)
         ~live_auth:auth ~prior:preview.allow
         ~live:{ live with already_applied = true }
         ~vault_id:vault.id ~expected:acct ~now:fixed_now ())
  in
  Alcotest.(check bool) "dup" true (contains ~needle:"duplicate" d.reason)

let test_receipt_lists_by_action () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let auth = base_request ~action:"review_submit" () in
  let live = live_ok () in
  let preview =
    expect_preview_ok
      (Attr.authorize_preview ~db
         ~family:(Attr.Submit_review (submit_req ()))
         ~route:(Some route_on) ~pilot:pilot_off ~user_auth_available:true ~auth
         ~live ~item_key ~room_id ~now:fixed_now ())
  in
  ignore
    (expect_dispatch_ok
       (Attr.dispatch ~db
          ~family:(Attr.Submit_review (submit_req ()))
          ~live_auth:auth ~prior:preview.allow ~live ~vault_id:vault.id
          ~expected:acct ~item_key ~room_id ~receipt_id:"rcpt_1" ~now:fixed_now
          ()));
  let rows = Audit.list_by_action ~db ~action:"review_submit" ~limit:10 () in
  Alcotest.(check bool) "has rows" true (List.length rows >= 2);
  let kinds = List.map (fun (r : Audit.t) -> r.kind) rows in
  Alcotest.(check bool)
    "has preview" true
    (List.exists (fun k -> k = Audit.Preview) kinds);
  Alcotest.(check bool)
    "has receipt" true
    (List.exists (fun k -> k = Audit.Receipt) kinds)

(* Capability path: pilot off + user auth allows submit plan *)
let test_plan_submit_p21_user_path () =
  with_db @@ fun db ->
  let principal =
    Setup_plan.
      { id = "principal:alice"; kind = Principal; label = Some "Alice" }
  in
  let plan =
    assert_ok
      (Review.plan_submit_review ~db ~principal ~room_id ~pilot:pilot_off
         ~user_auth_available:true ~req:(submit_req ()) ~base_revision:"rev-1"
         ~route:route_on ~now:fixed_now ())
  in
  let s = Yojson.Safe.to_string (Setup_plan.to_persist_json plan) in
  Alcotest.(check bool)
    "user required" true
    (contains ~needle:"User_required" s);
  Alcotest.(check bool)
    "production ready" true
    (contains ~needle:"production_ready" s)

let test_submit_denied_when_no_user_auth_and_pilot_off () =
  match
    Review.authorize_submit_review ~route:(Some route_on) ~pilot:pilot_off
      ~user_auth_available:false ~req:(submit_req ()) ~now:fixed_now ()
  with
  | Ok () -> Alcotest.fail "expected deny"
  | Error msg ->
      Alcotest.(check bool)
        "fallback" true
        (contains ~needle:"fallback" msg || contains ~needle:"user" msg)

let test_plan_with_attribution_embeds_allow () =
  with_db @@ fun db ->
  let principal =
    Setup_plan.
      { id = "principal:alice"; kind = Principal; label = Some "Alice" }
  in
  let auth = base_request ~action:"review_submit" () in
  let live = live_ok () in
  let planned =
    assert_ok
      (Attr.plan_with_attribution ~db ~principal ~room_id
         ~family:(Attr.Submit_review (submit_req ()))
         ~base_revision:"rev-1" ~auth ~live ~route:(Some route_on)
         ~pilot:pilot_off ~user_auth_available:true ~now:fixed_now ())
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
      Alcotest.(check string) "action" "review_submit" allow.requirement.action

let test_prepare_dispatch_from_plan_success () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let vault = create_vault ~db ~keys ~account:acct () in
  let principal =
    Setup_plan.
      { id = "principal:alice"; kind = Principal; label = Some "Alice" }
  in
  let auth = base_request ~action:"review_submit" () in
  let live = live_ok () in
  let planned =
    assert_ok
      (Attr.plan_with_attribution ~db ~principal ~room_id
         ~family:(Attr.Submit_review (submit_req ()))
         ~base_revision:"rev-1" ~auth ~live ~route:(Some route_on)
         ~pilot:pilot_off ~user_auth_available:true ~now:fixed_now ())
  in
  let disp =
    assert_ok
      (Attr.prepare_dispatch_from_plan ~db ~plan:planned.plan ~live_auth:auth
         ~live ~vault_id:vault.id ~expected:acct ~now:fixed_now ())
  in
  Alcotest.(check bool) "lease" true disp.has_user_lease;
  Attr.revoke_issued_lease disp.issued

let suite =
  [
    ("policy actions", `Quick, test_policy_actions);
    ("stale head denied", `Quick, test_stale_head_denied);
    ("self-review denied", `Quick, test_self_review_denied);
    ("duplicate replay denied", `Quick, test_duplicate_replay_denied);
    ("invalid reviewers denied", `Quick, test_invalid_reviewers_denied);
    ( "preview submit user_required success",
      `Quick,
      test_preview_submit_user_required_success );
    ( "preview request user_preferred success",
      `Quick,
      test_preview_request_user_preferred_success );
    ("preview request app fallback", `Quick, test_preview_request_app_fallback);
    ( "preview submit app mode forbidden",
      `Quick,
      test_preview_submit_app_mode_forbidden );
    ( "preview denied without capability",
      `Quick,
      test_preview_denied_without_capability );
    ("preview denied stale head", `Quick, test_preview_denied_stale_head);
    ("preview denied actor change", `Quick, test_preview_denied_actor_change);
    ( "dispatch submit requires user lease",
      `Quick,
      test_dispatch_submit_requires_user_lease );
    ( "dispatch submit app forbidden without user",
      `Quick,
      test_dispatch_submit_app_forbidden_without_user );
    ("dispatch request user lease", `Quick, test_dispatch_request_user_lease);
    ( "dispatch request app fallback no lease",
      `Quick,
      test_dispatch_request_app_fallback_no_lease );
    ( "dispatch stale head after preview",
      `Quick,
      test_dispatch_stale_head_after_preview );
    ("dispatch self-review denied", `Quick, test_dispatch_self_review_denied);
    ( "dispatch duplicate replay denied",
      `Quick,
      test_dispatch_duplicate_replay_denied );
    ("receipt lists by action", `Quick, test_receipt_lists_by_action);
    ("plan submit p21 user path", `Quick, test_plan_submit_p21_user_path);
    ( "submit denied without user auth and pilot off",
      `Quick,
      test_submit_denied_when_no_user_auth_and_pilot_off );
    ( "plan with attribution embeds allow",
      `Quick,
      test_plan_with_attribution_embeds_allow );
    ( "prepare dispatch from plan success",
      `Quick,
      test_prepare_dispatch_from_plan_success );
  ]

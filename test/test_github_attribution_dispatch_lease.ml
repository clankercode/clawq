(** Tests for opaque GitHub lease issuance after final authorization
    revalidation (P21.M3.E2.T007).

    Covers:
    - Happy path: revalidate prior Allow then issue callback-scoped lease only
    - App path: revalidate without user lease
    - Policy races between preview and dispatch (stale pins)
    - Revocation / generation race between queue and lease issue
    - No raw token escape in issued / denial / JSON surfaces *)

module A = Github_attribution_authorize
module D = Github_attribution_dispatch_lease
module L = Github_user_token_lease
module V = Github_user_token_vault
module S = Github_user_token_store
module Cas = Github_user_token_cas
module Policy = Github_attribution_policy

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-dispatch-lease-test" ()

let sample_tokens =
  {
    S.access_token = "ghu_access_DISPATCH_LEASE_PLAINTEXT_never_export";
    refresh_token = Some "ghr_refresh_DISPATCH_LEASE_PLAINTEXT_never_export";
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

let account ?(principal_id = "prin_a") ?(github_user_id = 4242L) ?(app_id = 99)
    ?(host = V.default_host) () =
  assert_ok (V.make_account_key ~principal_id ~github_user_id ~app_id ~host ())

let make_keys ?(key_id = "mk-dispatch-1") ?(key_version = 1) () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key ())

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  V.ensure_schema db;
  Fun.protect
    ~finally:(fun () ->
      ignore (L.discard_all ());
      ignore (Sqlite3.db_close db))
    (fun () -> f db)

let fixed_now = 1_720_000_000.0
let far_expires = "2026-12-01T00:00:00Z"

let create_vault ~db ?(keys = make_keys ()) ?(account = account ())
    ?(tokens = sample_tokens) ?(id = "ghvault_dispatch_1")
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

let base_request ?(action = "merge") ?(tool_authorized = true)
    ?(repo_granted = true) ?(repo_blocked = false) ?(principal_current = true)
    ?(confirmation_required = true) ?(confirmation_satisfied = true)
    ?(confirmation_id = Some "conf_1") ?(binding = A.Selected (selected ()))
    ?(installation_active = true) ?(installation_repo_ok = true)
    ?(permissions_ok = true) ?(user_authority_ok = true) ?(org_policy_ok = true)
    ?(sso_ok = true) ?(live_ok = true) ?(live_detail = None)
    ?(live_revision = Some "sha_abc") ?(pin = A.empty_revision_pin)
    ?(actor_snapshot_id = Some "snap_1") ?(catalog_revision = "cat_rev_1")
    ?(access_revision = "acc_rev_1") ?(principal_revision = 3)
    ?(installation_revision = Some "inst_rev_1") () : A.request =
  {
    action;
    tool_catalog =
      {
        revision = catalog_revision;
        access_revision;
        tool_authorized;
        room_id = Some "room_1";
        session_key = Some "sess_1";
      };
    repo_grant =
      {
        repo_full_name = "acme/widgets";
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
  }

let prior_allow ?(request = base_request ()) () : A.allow =
  match A.authorize request with
  | A.Allow a -> a
  | A.Deny d ->
      Alcotest.fail
        (Printf.sprintf "prior allow failed: %s/%s" d.failed_check d.repair.code)

let expect_issued = function
  | Ok i -> i
  | Error e -> Alcotest.fail ("issue: " ^ D.string_of_denial e)

let expect_denial = function
  | Error e -> e
  | Ok _ -> Alcotest.fail "expected denial"

let secrets_absent blob =
  List.iter
    (fun needle ->
      Alcotest.(check bool) ("no " ^ needle) false (contains ~needle blob))
    [
      sample_tokens.access_token;
      Option.get sample_tokens.refresh_token;
      "ghu_access_DISPATCH";
      "ghr_refresh_DISPATCH";
      aes_key;
    ]

(* -------------------------------------------------------------------------- *)
(* Happy paths                                                                 *)
(* -------------------------------------------------------------------------- *)

let test_issue_user_lease_after_revalidation () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ = create_vault ~db ~keys ~account:acct () in
  let preview = base_request () in
  let prior = prior_allow ~request:preview () in
  (* Dispatch: same live evidence as preview (queue did not change policy). *)
  let issued =
    expect_issued
      (D.issue_for_dispatch ~db ~now:fixed_now ~live:preview ~prior
         ~vault_id:rec_.id ~expected:acct ())
  in
  Alcotest.(check string) "mode" "user" (A.resolved_mode_to_string issued.mode);
  Alcotest.(check bool) "has lease" true (Option.is_some issued.lease);
  let id = Option.get issued.identity in
  Alcotest.(check string) "vault" rec_.id id.binding.vault_id;
  Alcotest.(check int) "gen" 1 id.binding.generation;
  Alcotest.(check (option string))
    "binding" (Some "bind_1") id.binding.binding_id;
  (* Opaque surfaces never carry the token. *)
  let json = D.issued_to_json issued in
  secrets_absent (Yojson.Safe.to_string json ^ D.string_of_issued issued);
  Alcotest.(check bool)
    "identity no token" false
    (L.identity_contains_plaintext ~identity:id
       ~plaintext:sample_tokens.access_token);
  (* Token only inside with_token. *)
  match
    L.with_token ~db ~keys ~now:fixed_now ~lease:(Option.get issued.lease)
      ~f:(fun ~access_token -> access_token = sample_tokens.access_token)
      ()
  with
  | Ok true -> ()
  | Ok false -> Alcotest.fail "token mismatch inside callback"
  | Error d -> Alcotest.fail (L.string_of_denial d)

let test_app_path_no_user_lease () =
  with_db @@ fun db ->
  let preview =
    base_request ~action:"comment" ~confirmation_required:false
      ~confirmation_satisfied:true ~confirmation_id:None ~binding:A.Not_required
      ()
  in
  let prior = prior_allow ~request:preview () in
  Alcotest.(check string)
    "prior mode app" "app"
    (A.resolved_mode_to_string prior.mode);
  let issued =
    expect_issued
      (D.issue_for_dispatch ~db ~now:fixed_now ~live:preview ~prior ())
  in
  Alcotest.(check string) "mode" "app" (A.resolved_mode_to_string issued.mode);
  Alcotest.(check bool) "no lease" true (Option.is_none issued.lease);
  Alcotest.(check bool) "no identity" true (Option.is_none issued.identity)

let test_user_requires_vault_id () =
  with_db @@ fun db ->
  let preview = base_request () in
  let prior = prior_allow ~request:preview () in
  match D.issue_for_dispatch ~db ~now:fixed_now ~live:preview ~prior () with
  | Error D.User_lease_requires_vault_id -> ()
  | Error e -> Alcotest.fail ("unexpected: " ^ D.string_of_denial e)
  | Ok _ -> Alcotest.fail "expected vault_id required"

let test_revalidate_only_no_lease () =
  let preview = base_request () in
  let prior = prior_allow ~request:preview () in
  match D.revalidate ~live:preview ~prior () with
  | Ok a ->
      Alcotest.(check string) "action" "merge" a.requirement.action;
      Alcotest.(check string) "mode" "user" (A.resolved_mode_to_string a.mode)
  | Error e -> Alcotest.fail (D.string_of_denial e)

(* -------------------------------------------------------------------------- *)
(* Policy races: preview vs dispatch                                           *)
(* -------------------------------------------------------------------------- *)

let test_stale_tool_catalog_between_preview_and_dispatch () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let preview = base_request ~catalog_revision:"cat_rev_1" () in
  let prior = prior_allow ~request:preview () in
  (* Queue / dispatch sees a new catalog freeze — pin mismatch. *)
  let live = base_request ~catalog_revision:"cat_rev_2" () in
  match
    D.issue_for_dispatch ~db ~now:fixed_now ~live ~prior ~vault_id:rec_.id ()
  with
  | Error (D.Authorization d) ->
      Alcotest.(check string) "check" "tool_catalog" d.failed_check;
      Alcotest.(check string) "code" "stale_tool_catalog_revision" d.repair.code;
      secrets_absent
        (D.string_of_denial (D.Authorization d)
        ^ Yojson.Safe.to_string (D.denial_to_json (D.Authorization d)))
  | Error e -> Alcotest.fail ("unexpected: " ^ D.string_of_denial e)
  | Ok _ -> Alcotest.fail "expected stale catalog deny"

let test_confirmation_revoked_between_preview_and_dispatch () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let preview = base_request ~confirmation_id:(Some "conf_1") () in
  let prior = prior_allow ~request:preview () in
  let live =
    base_request ~confirmation_id:(Some "conf_1") ~confirmation_satisfied:false
      ()
  in
  match
    D.issue_for_dispatch ~db ~now:fixed_now ~live ~prior ~vault_id:rec_.id ()
  with
  | Error (D.Authorization d) ->
      Alcotest.(check string) "check" "confirmation" d.failed_check;
      Alcotest.(check string) "code" "confirmation_required" d.repair.code
  | Error e -> Alcotest.fail ("unexpected: " ^ D.string_of_denial e)
  | Ok _ -> Alcotest.fail "expected confirmation deny"

let test_sso_lost_between_queue_and_dispatch () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let preview = base_request () in
  let prior = prior_allow ~request:preview () in
  let live = base_request ~sso_ok:false () in
  match
    D.issue_for_dispatch ~db ~now:fixed_now ~live ~prior ~vault_id:rec_.id ()
  with
  | Error (D.Authorization d) ->
      Alcotest.(check string) "check" "user_org_sso" d.failed_check;
      Alcotest.(check string) "code" "sso_required" d.repair.code
  | Error e -> Alcotest.fail ("unexpected: " ^ D.string_of_denial e)
  | Ok _ -> Alcotest.fail "expected sso deny"

let test_binding_revoked_between_preview_and_dispatch () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let preview = base_request () in
  let prior = prior_allow ~request:preview () in
  let live =
    base_request
      ~binding:(A.Selected (selected ~authorized:false ~vault_active:false ()))
      ()
  in
  match
    D.issue_for_dispatch ~db ~now:fixed_now ~live ~prior ~vault_id:rec_.id ()
  with
  | Error (D.Authorization d) ->
      Alcotest.(check string) "check" "binding" d.failed_check;
      Alcotest.(check bool)
        "binding or vault code" true
        (d.repair.code = "binding_not_authorized"
        || d.repair.code = "vault_inactive")
  | Error e -> Alcotest.fail ("unexpected: " ^ D.string_of_denial e)
  | Ok _ -> Alcotest.fail "expected binding deny"

let test_lineage_break_between_preview_and_dispatch () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let preview = base_request () in
  let prior = prior_allow ~request:preview () in
  (* Relink: new lineage id; pin still expects lin_1. *)
  let live =
    base_request
      ~binding:
        (A.Selected
           (selected ~binding_id:"bind_2" ~lineage_id:"lin_relinked" ()))
      ()
  in
  match
    D.issue_for_dispatch ~db ~now:fixed_now ~live ~prior ~vault_id:rec_.id ()
  with
  | Error (D.Authorization d) ->
      Alcotest.(check string) "check" "binding" d.failed_check;
      Alcotest.(check string) "code" "stale_binding_lineage" d.repair.code
  | Error (D.Prior_binding_mismatch _) -> ()
  | Error e -> Alcotest.fail ("unexpected: " ^ D.string_of_denial e)
  | Ok _ -> Alcotest.fail "expected lineage deny"

let test_principal_changed_between_preview_and_dispatch () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let preview = base_request () in
  let prior = prior_allow ~request:preview () in
  let live =
    let r = base_request () in
    { r with principal = { r.principal with principal_id = "prin_other" } }
  in
  match
    D.issue_for_dispatch ~db ~now:fixed_now ~live ~prior ~vault_id:rec_.id ()
  with
  | Error (D.Prior_principal_mismatch _) -> ()
  | Error (D.Authorization _) ->
      (* Principal pin is revision-based; id change may still Allow if evidence
         alone is consistent — continuity check must catch it. *)
      Alcotest.fail "expected prior principal mismatch"
  | Error e -> Alcotest.fail ("unexpected: " ^ D.string_of_denial e)
  | Ok _ -> Alcotest.fail "expected principal continuity deny"

let test_repo_grant_lost_between_preview_and_dispatch () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let preview = base_request () in
  let prior = prior_allow ~request:preview () in
  let live = base_request ~repo_granted:false () in
  match
    D.issue_for_dispatch ~db ~now:fixed_now ~live ~prior ~vault_id:rec_.id ()
  with
  | Error (D.Authorization d) ->
      Alcotest.(check string) "check" "repo_grant" d.failed_check;
      Alcotest.(check string) "code" "repo_not_granted" d.repair.code
  | Error e -> Alcotest.fail ("unexpected: " ^ D.string_of_denial e)
  | Ok _ -> Alcotest.fail "expected repo deny"

let test_live_head_changed_between_queue_and_dispatch () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let preview = base_request ~live_revision:(Some "sha_abc") () in
  let prior = prior_allow ~request:preview () in
  let live = base_request ~live_revision:(Some "sha_def") () in
  match
    D.issue_for_dispatch ~db ~now:fixed_now ~live ~prior ~vault_id:rec_.id ()
  with
  | Error (D.Authorization d) ->
      Alcotest.(check string) "check" "live_action" d.failed_check;
      Alcotest.(check string) "code" "stale_live_state_revision" d.repair.code
  | Error e -> Alcotest.fail ("unexpected: " ^ D.string_of_denial e)
  | Ok _ -> Alcotest.fail "expected live state deny"

(* -------------------------------------------------------------------------- *)
(* Revocation / generation race: vault CAS between queue and issue             *)
(* -------------------------------------------------------------------------- *)

let test_vault_generation_pin_denies_after_refresh () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ = create_vault ~db ~keys ~account:acct () in
  let preview =
    base_request ~binding:(A.Selected (selected ~vault_generation:1 ())) ()
  in
  let prior = prior_allow ~request:preview () in
  (* Evidence claims generation advanced (ordinary refresh path would update
     pins; here dispatch still pins generation 1 from preview). *)
  let live =
    base_request ~binding:(A.Selected (selected ~vault_generation:2 ())) ()
  in
  match
    D.issue_for_dispatch ~db ~now:fixed_now ~live ~prior ~vault_id:rec_.id
      ~expected:acct ()
  with
  | Error (D.Authorization d) ->
      Alcotest.(check string) "check" "binding" d.failed_check;
      Alcotest.(check string) "code" "stale_vault_generation" d.repair.code
  | Error e -> Alcotest.fail ("unexpected: " ^ D.string_of_denial e)
  | Ok _ -> Alcotest.fail "expected generation pin deny"

let test_vault_disabled_between_queue_and_issue () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ = create_vault ~db ~keys ~account:acct () in
  let preview = base_request () in
  let prior = prior_allow ~request:preview () in
  (* Authorize evidence still says vault active, but vault was CAS-disabled. *)
  (match
     Cas.disable ~db ~keys ~now:fixed_now ~id:rec_.id ~expected_generation:1
       ~expected:acct ()
   with
  | Ok _ -> ()
  | Error d -> Alcotest.fail ("disable: " ^ Cas.string_of_denial d));
  match
    D.issue_for_dispatch ~db ~now:fixed_now ~live:preview ~prior
      ~vault_id:rec_.id ~expected:acct ()
  with
  | Error (D.Lease d) ->
      secrets_absent
        (D.string_of_denial (D.Lease d)
        ^ Yojson.Safe.to_string (D.denial_to_json (D.Lease d)));
      Alcotest.(check bool)
        "denial no token" false
        (D.denial_exposes_token ~denial:(D.Lease d)
           ~plaintext:sample_tokens.access_token)
  | Error (D.Authorization _) ->
      (* If evidence itself was updated to inactive, also fine. *)
      ()
  | Error e -> Alcotest.fail ("unexpected: " ^ D.string_of_denial e)
  | Ok _ -> Alcotest.fail "expected vault disabled deny"

let test_generation_race_after_cas_replace () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ = create_vault ~db ~keys ~account:acct () in
  let preview =
    base_request ~binding:(A.Selected (selected ~vault_generation:1 ())) ()
  in
  let prior = prior_allow ~request:preview () in
  (* Between revalidate evidence (still gen 1) and issue, CAS replace advances
     generation. Authorize may still pass with stale evidence; post-issue race
     check must revoke and deny. *)
  let new_tokens =
    {
      S.access_token = "ghu_access_AFTER_REPLACE_never_export";
      refresh_token = Some "ghr_refresh_AFTER_REPLACE_never_export";
    }
  in
  (match
     Cas.replace ~db ~keys ~now:fixed_now ~id:rec_.id ~expected_generation:1
       ~expected:acct ~tokens:new_tokens ~scopes:[ "repo" ]
       ~expires_at:far_expires ()
   with
  | Ok t -> Alcotest.(check int) "gen advanced" 2 t.record.generation
  | Error d -> Alcotest.fail ("replace: " ^ Cas.string_of_denial d));
  (* Live evidence still claims gen 1 (stale snapshot at queue time). *)
  let live =
    base_request ~binding:(A.Selected (selected ~vault_generation:1 ())) ()
  in
  match
    D.issue_for_dispatch ~db ~now:fixed_now ~live ~prior ~vault_id:rec_.id
      ~expected:acct ()
  with
  | Error (D.Generation_race { expected; actual }) ->
      Alcotest.(check int) "expected" 1 expected;
      Alcotest.(check int) "actual" 2 actual;
      Alcotest.(check int) "no live lease left" 0 (L.live_count ())
  | Error (D.Lease d) ->
      (* Some vault paths may deny earlier; still no token escape. *)
      Alcotest.(check bool)
        "no token" false
        (D.denial_exposes_token ~denial:(D.Lease d)
           ~plaintext:sample_tokens.access_token);
      Alcotest.(check bool)
        "no new token" false
        (D.denial_exposes_token ~denial:(D.Lease d)
           ~plaintext:new_tokens.access_token)
  | Error e -> Alcotest.fail ("unexpected: " ^ D.string_of_denial e)
  | Ok _ -> Alcotest.fail "expected generation race deny"

(* -------------------------------------------------------------------------- *)
(* Redaction                                                                   *)
(* -------------------------------------------------------------------------- *)

let test_denial_and_issued_never_embed_token () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let preview = base_request () in
  let prior = prior_allow ~request:preview () in
  let issued =
    expect_issued
      (D.issue_for_dispatch ~db ~now:fixed_now ~live:preview ~prior
         ~vault_id:rec_.id ())
  in
  secrets_absent (Yojson.Safe.to_string (D.issued_to_json issued));
  let denied =
    expect_denial
      (D.issue_for_dispatch ~db ~now:fixed_now
         ~live:(base_request ~sso_ok:false ())
         ~prior ~vault_id:rec_.id ())
  in
  secrets_absent
    (D.string_of_denial denied ^ Yojson.Safe.to_string (D.denial_to_json denied));
  Alcotest.(check bool)
    "helper" false
    (D.denial_exposes_token ~denial:denied ~plaintext:sample_tokens.access_token)

let test_pin_of_allow_roundtrip () =
  let prior = prior_allow () in
  let pin = D.pin_of_allow prior in
  Alcotest.(check (option string))
    "catalog" (Some "cat_rev_1") pin.tool_catalog_revision;
  Alcotest.(check (option int)) "gen" (Some 1) pin.vault_generation;
  Alcotest.(check (option string))
    "lineage" (Some "lin_1") pin.binding_lineage_id;
  let req = D.request_with_prior_pin ~live:(base_request ()) ~prior in
  Alcotest.(check (option string))
    "pin applied" (Some "cat_rev_1") req.pin.tool_catalog_revision

let suite =
  [
    Alcotest.test_case "issue user lease after revalidation" `Quick
      test_issue_user_lease_after_revalidation;
    Alcotest.test_case "App path no user lease" `Quick
      test_app_path_no_user_lease;
    Alcotest.test_case "User requires vault_id" `Quick
      test_user_requires_vault_id;
    Alcotest.test_case "revalidate only no lease" `Quick
      test_revalidate_only_no_lease;
    Alcotest.test_case "stale tool catalog preview→dispatch" `Quick
      test_stale_tool_catalog_between_preview_and_dispatch;
    Alcotest.test_case "confirmation revoked preview→dispatch" `Quick
      test_confirmation_revoked_between_preview_and_dispatch;
    Alcotest.test_case "SSO lost queue→dispatch" `Quick
      test_sso_lost_between_queue_and_dispatch;
    Alcotest.test_case "binding revoked preview→dispatch" `Quick
      test_binding_revoked_between_preview_and_dispatch;
    Alcotest.test_case "lineage break preview→dispatch" `Quick
      test_lineage_break_between_preview_and_dispatch;
    Alcotest.test_case "principal changed preview→dispatch" `Quick
      test_principal_changed_between_preview_and_dispatch;
    Alcotest.test_case "repo grant lost preview→dispatch" `Quick
      test_repo_grant_lost_between_preview_and_dispatch;
    Alcotest.test_case "live head changed queue→dispatch" `Quick
      test_live_head_changed_between_queue_and_dispatch;
    Alcotest.test_case "vault generation pin after refresh" `Quick
      test_vault_generation_pin_denies_after_refresh;
    Alcotest.test_case "vault disabled queue→issue" `Quick
      test_vault_disabled_between_queue_and_issue;
    Alcotest.test_case "generation race after CAS replace" `Quick
      test_generation_race_after_cas_replace;
    Alcotest.test_case "denial/issued never embed token" `Quick
      test_denial_and_issued_never_embed_token;
    Alcotest.test_case "pin_of_allow roundtrip" `Quick
      test_pin_of_allow_roundtrip;
  ]

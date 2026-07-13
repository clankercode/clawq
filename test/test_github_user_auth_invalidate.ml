(** Tests for unlink / Principal / Connector removal invalidation
    (P21.M3.E1.T004).

    Contract under test:
    - Local disable + lineage break precede any remote work
    - Remote token/grant revoke uses a narrow open of sealed material
    - Secrets are destroyed regardless of remote outcome
    - Remote failure never re-enables access
    - Pending auth on the old lineage is zeroed; delayed work does not follow a
      later relink (lineage_id advanced)
    - Connector split shares the canonical lifecycle *)

module Inv = Github_user_auth_invalidate
module V = Github_user_token_vault
module S = Github_user_token_store
module L = Github_user_token_lease
module B = Github_account_binding
module P = Principal_identity
module PS = Principal_identity_store
module M = Principal_merge
module A = Actor_snapshot

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-invalidate-test-master"
    ()

let sample_tokens ?(tag = "base") () =
  {
    S.access_token = Printf.sprintf "ghu_access_INV_%s_PLAINTEXT" tag;
    refresh_token = Some (Printf.sprintf "ghr_refresh_INV_%s_PLAINTEXT" tag);
  }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let assert_iok = function
  | Ok v -> v
  | Error d -> Alcotest.fail (Inv.string_of_denial d)

let fixed_now = 1_720_000_400.0
let far_expires = "2026-12-01T00:00:00Z"
let app_id = 55
let github_user_id = 77001L

let make_keys ?(key_id = "mk-inv-1") ?(key_version = 1) () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key ())

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Inv.ensure_schema db;
  Fun.protect
    ~finally:(fun () ->
      ignore (L.discard_all ());
      ignore (Sqlite3.db_close db))
    (fun () -> f db)

let seed_principal ~db ~principal_id =
  let pid = assert_ok (P.principal_id_of_string principal_id) in
  let p =
    P.make_principal ~id:pid ~revision:1 ~created_at:"2026-01-01T00:00:00Z"
      ~updated_at:"2026-01-01T00:00:00Z" ()
  in
  ignore (assert_ok (PS.insert_principal ~db ~now:fixed_now p));
  pid

let create_vault ~db ~keys ~principal_id ~vault_id ~user_id ~app =
  let account =
    assert_ok
      (V.make_account_key ~principal_id ~github_user_id:user_id ~app_id:app ())
  in
  match
    V.create ~db ~keys ~id:vault_id ~now:fixed_now ~account
      ~tokens:(sample_tokens ~tag:vault_id ())
      ~scopes:[ "repo"; "read:user" ] ~expires_at:far_expires ()
  with
  | Ok r -> r
  | Error d -> Alcotest.fail ("create vault: " ^ V.string_of_denial d)

let seed_binding ~db ~id ~principal_id ~vault_id ~user_id ~app
    ?(status = B.Authorized) ?(lineage_id = id ^ "_lineage") () =
  let pid = assert_ok (P.principal_id_of_string principal_id) in
  let identity =
    assert_ok
      (B.make_account_identity ~host:B.default_host ~app_id:app
         ~github_user_id:user_id ())
  in
  let vault_ref = assert_ok (B.make_vault_ref vault_id) in
  let b =
    B.make_binding ~id ~principal_id:pid ~identity ~authorization_status:status
      ~vault_ref ~lineage_id ()
  in
  assert_ok (B.insert ~db ~now:fixed_now b)

(* -------------------------------------------------------------------------- *)
(* Local first: lineage broken + vault inactive before remote                 *)
(* -------------------------------------------------------------------------- *)

let test_local_disable_and_lineage_before_remote () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed_principal ~db ~principal_id:"prin_inv_1");
  let vault =
    create_vault ~db ~keys ~principal_id:"prin_inv_1" ~vault_id:"ghvault_inv_1"
      ~user_id:github_user_id ~app:app_id
  in
  let binding =
    seed_binding ~db ~id:"ghbind_inv_1" ~principal_id:"prin_inv_1"
      ~vault_id:vault.id ~user_id:github_user_id ~app:app_id ()
  in
  let prior_lineage = binding.lineage_id in
  let lease =
    match L.issue ~db ~now:fixed_now ~vault_id:vault.id () with
    | Ok l -> l
    | Error d -> Alcotest.fail (L.string_of_denial d)
  in
  let remote_order = ref [] in
  let http_delete ~url:_ ~headers:_ ~body =
    remote_order := "remote" :: !remote_order;
    (* Body carries the access token — prove local fence already held. *)
    (match V.get_meta ~db ~id:vault.id with
    | Ok (Some meta) ->
        Alcotest.(check bool) "vault inactive before remote" false meta.active
    | _ -> Alcotest.fail "vault meta missing at remote time");
    (match B.get ~db ~id:binding.id with
    | Ok (Some b) ->
        Alcotest.(check bool)
          "lineage already broken before remote" true
          (not (String.equal b.lineage_id prior_lineage));
        Alcotest.(check string)
          "unlinked before remote" "unlinked"
          (B.string_of_authorization_status b.authorization_status)
    | _ -> Alcotest.fail "binding missing at remote time");
    Alcotest.(check bool)
      "lease already revoked before remote" true (L.is_revoked lease);
    Alcotest.(check bool)
      "body has access token for revoke" true
      (String_util.contains body "ghu_access_INV_");
    Ok (204, "")
  in
  let resolve_client ~client_id_handle:_ = Ok ("client_inv", "secret_inv") in
  let receipt =
    assert_iok
      (Inv.invalidate_binding ~db ~keys ~kind:Inv.Unlink
         ~remote_mode:Inv.Revoke_grant ~http_delete ~resolve_client
         ~client_id_handle:"cidh" ~now:fixed_now ~binding_id:binding.id ())
  in
  Alcotest.(check int) "matched" 1 receipt.bindings_matched;
  Alcotest.(check int) "secrets destroyed" 1 receipt.secrets_destroyed;
  Alcotest.(check int) "lineages broken" 1 receipt.lineages_broken;
  Alcotest.(check int) "remote succeeded" 1 receipt.remote_succeeded;
  Alcotest.(check bool)
    "leases invalidated" true
    (receipt.leases_invalidated >= 1);
  Alcotest.(check bool)
    "remote was called" true
    (List.mem "remote" !remote_order);
  (* Vault row gone. *)
  (match V.get_meta ~db ~id:vault.id with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "vault should be destroyed"
  | Error V.Not_found -> ()
  | Error d -> Alcotest.fail (V.string_of_denial d));
  let b = Option.get (assert_ok (B.get ~db ~id:binding.id)) in
  Alcotest.(check bool) "vault_ref cleared" true (Option.is_none b.vault_ref);
  Alcotest.(check bool)
    "lineage advanced" true
    (not (String.equal b.lineage_id prior_lineage));
  let plaintext = (sample_tokens ~tag:vault.id ()).access_token in
  Alcotest.(check bool)
    "receipt redacted" false
    (Inv.receipt_contains_plaintext ~receipt ~plaintext)

(* -------------------------------------------------------------------------- *)
(* Remote failure never re-enables; secrets still destroyed                   *)
(* -------------------------------------------------------------------------- *)

let test_remote_failure_never_reenables_and_destroys_secrets () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed_principal ~db ~principal_id:"prin_inv_2");
  let vault =
    create_vault ~db ~keys ~principal_id:"prin_inv_2" ~vault_id:"ghvault_inv_2"
      ~user_id:github_user_id ~app:app_id
  in
  let binding =
    seed_binding ~db ~id:"ghbind_inv_2" ~principal_id:"prin_inv_2"
      ~vault_id:vault.id ~user_id:github_user_id ~app:app_id ()
  in
  let prior_lineage = binding.lineage_id in
  let http_delete ~url:_ ~headers:_ ~body:_ =
    Error "simulated network partition"
  in
  let resolve_client ~client_id_handle:_ = Ok ("client_inv", "secret_inv") in
  let receipt =
    assert_iok
      (Inv.invalidate_binding ~db ~keys ~kind:Inv.Revoke
         ~remote_mode:Inv.Revoke_token ~http_delete ~resolve_client
         ~client_id_handle:"cidh" ~now:fixed_now ~binding_id:binding.id ())
  in
  Alcotest.(check int) "remote failed" 1 receipt.remote_failed;
  Alcotest.(check int) "remote succeeded" 0 receipt.remote_succeeded;
  Alcotest.(check int) "secrets still destroyed" 1 receipt.secrets_destroyed;
  let b = Option.get (assert_ok (B.get ~db ~id:binding.id)) in
  Alcotest.(check string)
    "revoked" "revoked"
    (B.string_of_authorization_status b.authorization_status);
  Alcotest.(check bool)
    "lineage broken despite remote fail" true
    (not (String.equal b.lineage_id prior_lineage));
  (* Cannot re-issue lease — vault gone / inactive. *)
  (match V.get_meta ~db ~id:vault.id with
  | Ok None | Error V.Not_found -> ()
  | Ok (Some meta) ->
      Alcotest.(check bool) "still inactive if present" false meta.active
  | Error d -> Alcotest.fail (V.string_of_denial d));
  match L.issue ~db ~now:fixed_now ~vault_id:vault.id () with
  | Ok _ -> Alcotest.fail "lease must not re-issue after invalidate"
  | Error _ -> ()

(* -------------------------------------------------------------------------- *)
(* Disable keeps secrets and lineage                                          *)
(* -------------------------------------------------------------------------- *)

let test_disable_preserves_secrets_and_lineage () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed_principal ~db ~principal_id:"prin_inv_3");
  let vault =
    create_vault ~db ~keys ~principal_id:"prin_inv_3" ~vault_id:"ghvault_inv_3"
      ~user_id:github_user_id ~app:app_id
  in
  let binding =
    seed_binding ~db ~id:"ghbind_inv_3" ~principal_id:"prin_inv_3"
      ~vault_id:vault.id ~user_id:github_user_id ~app:app_id ()
  in
  let prior_lineage = binding.lineage_id in
  let receipt =
    assert_iok
      (Inv.invalidate_binding ~db ~keys ~kind:Inv.Disable ~now:fixed_now
         ~binding_id:binding.id ())
  in
  Alcotest.(check int)
    "no secret destroy on disable" 0 receipt.secrets_destroyed;
  Alcotest.(check int) "no lineage break on disable" 0 receipt.lineages_broken;
  let b = Option.get (assert_ok (B.get ~db ~id:binding.id)) in
  Alcotest.(check string)
    "disabled" "disabled"
    (B.string_of_authorization_status b.authorization_status);
  Alcotest.(check string) "lineage preserved" prior_lineage b.lineage_id;
  match V.get_meta ~db ~id:vault.id with
  | Ok (Some meta) ->
      Alcotest.(check bool) "vault inactive" false meta.active;
      Alcotest.(check bool) "row retained" true true
  | _ -> Alcotest.fail "vault should remain for disable"

(* -------------------------------------------------------------------------- *)
(* Principal removal invalidates all bindings + pending auth                  *)
(* -------------------------------------------------------------------------- *)

let test_principal_removal_all_bindings_and_pending () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let pid = seed_principal ~db ~principal_id:"prin_inv_4" in
  let v1 =
    create_vault ~db ~keys ~principal_id:"prin_inv_4" ~vault_id:"ghvault_inv_4a"
      ~user_id:github_user_id ~app:app_id
  in
  let v2 =
    create_vault ~db ~keys ~principal_id:"prin_inv_4" ~vault_id:"ghvault_inv_4b"
      ~user_id:88002L ~app:app_id
  in
  ignore
    (seed_binding ~db ~id:"ghbind_inv_4a" ~principal_id:"prin_inv_4"
       ~vault_id:v1.id ~user_id:github_user_id ~app:app_id ());
  ignore
    (seed_binding ~db ~id:"ghbind_inv_4b" ~principal_id:"prin_inv_4"
       ~vault_id:v2.id ~user_id:88002L ~app:app_id ());
  ignore
    (assert_ok
       (M.set_pending_authorization_count ~db ~principal_id:pid ~count:3));
  let receipt =
    assert_iok
      (Inv.invalidate_for_principal ~db ~keys ~kind:Inv.Principal_removal
         ~remote_mode:Inv.Skip ~now:fixed_now ~principal_id:pid ())
  in
  Alcotest.(check int) "matched 2" 2 receipt.bindings_matched;
  Alcotest.(check int) "secrets 2" 2 receipt.secrets_destroyed;
  Alcotest.(check int) "pending zeroed" 3 receipt.pending_auth_invalidated;
  Alcotest.(check int)
    "pending now 0" 0
    (assert_ok (M.get_pending_authorization_count ~db ~principal_id:pid));
  List.iter
    (fun id ->
      let b = Option.get (assert_ok (B.get ~db ~id)) in
      Alcotest.(check string)
        "revoked" "revoked"
        (B.string_of_authorization_status b.authorization_status))
    [ "ghbind_inv_4a"; "ghbind_inv_4b" ]

(* -------------------------------------------------------------------------- *)
(* Old lineage fails re-resolution after invalidate (does not follow relink)  *)
(* -------------------------------------------------------------------------- *)

let test_old_lineage_fails_reresolve_after_break () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let pid = seed_principal ~db ~principal_id:"prin_inv_5" in
  let actor_key =
    assert_ok
      (P.make_connector_actor_key ~connector:P.Cli ~tenant_or_workspace:"t"
         ~immutable_user_id:"u-inv-5")
  in
  let actor =
    P.make_connector_actor ~key:actor_key ~principal_id:pid ~revision:1
      ~verified_at:"2026-01-01T00:00:00Z" ~created_at:"2026-01-01T00:00:00Z"
      ~updated_at:"2026-01-01T00:00:00Z" ()
  in
  ignore (assert_ok (PS.insert_connector_actor ~db ~now:fixed_now actor));
  let link =
    P.make_identity_link ~id:"il_inv_5" ~principal_id:pid ~actor_key ~revision:1
      ~linked_at:"2026-01-01T00:00:00Z" ()
  in
  ignore (assert_ok (PS.insert_identity_link ~db ~now:fixed_now link));
  let vault =
    create_vault ~db ~keys ~principal_id:"prin_inv_5" ~vault_id:"ghvault_inv_5"
      ~user_id:github_user_id ~app:app_id
  in
  let binding =
    seed_binding ~db ~id:"ghbind_inv_5" ~principal_id:"prin_inv_5"
      ~vault_id:vault.id ~user_id:github_user_id ~app:app_id ()
  in
  let snap =
    assert_ok
      (A.create_from_live ~db ~now:fixed_now ~actor_key
         ~account_binding_id:binding.id ~reason:"delayed_job" ())
  in
  let prior_lineage = binding.lineage_id in
  ignore
    (assert_iok
       (Inv.invalidate_binding ~db ~keys ~kind:Inv.Unlink ~remote_mode:Inv.Skip
          ~now:fixed_now ~binding_id:binding.id ()));
  let auth = assert_ok (A.re_resolve_current_authority ~db snap) in
  Alcotest.(check bool) "not usable after lineage break" false auth.usable;
  let has_lineage_or_status_break =
    List.exists
      (function
        | A.Account_lineage_changed _ | A.Account_not_authorized _
        | A.Account_binding_missing ->
            true
        | _ -> false)
      auth.breaks
  in
  Alcotest.(check bool)
    "authority break surfaces lineage/status" true has_lineage_or_status_break;
  let b = Option.get (assert_ok (B.get ~db ~id:binding.id)) in
  Alcotest.(check bool)
    "live lineage differs from snapshot pin" true
    (not (String.equal b.lineage_id prior_lineage));
  (* Simulate a later relink creating a NEW authorized binding with new
     lineage — the old snapshot must still fail (does not follow relink). *)
  let v_relink =
    create_vault ~db ~keys ~principal_id:"prin_inv_5"
      ~vault_id:"ghvault_inv_5_relink" ~user_id:github_user_id ~app:app_id
  in
  let identity =
    assert_ok
      (B.make_account_identity ~host:B.default_host ~app_id ~github_user_id ())
  in
  (* Old unlinked row still holds identity uniqueness — delete it to allow a
     fresh relink row (production relink path). *)
  ignore (assert_ok (B.delete ~db ~id:binding.id));
  let vref = assert_ok (B.make_vault_ref v_relink.id) in
  let relinked =
    B.make_binding ~id:"ghbind_inv_5_relink" ~principal_id:pid ~identity
      ~authorization_status:B.Authorized ~vault_ref:vref
      ~lineage_id:"brand_new_lineage_after_relink" ()
  in
  ignore (assert_ok (B.insert ~db ~now:(fixed_now +. 10.) relinked));
  let auth2 = assert_ok (A.re_resolve_current_authority ~db snap) in
  Alcotest.(check bool)
    "old snapshot still unusable after relink" false auth2.usable

(* -------------------------------------------------------------------------- *)
(* Connector split shares lifecycle (pending zeroed, optional bindings)       *)
(* -------------------------------------------------------------------------- *)

let test_connector_split_lifecycle_pending_only () =
  with_db @@ fun db ->
  let pid = seed_principal ~db ~principal_id:"prin_inv_6" in
  ignore
    (assert_ok
       (M.set_pending_authorization_count ~db ~principal_id:pid ~count:2));
  let receipt =
    assert_iok
      (Inv.invalidate_for_connector_split ~db ~source_principal_id:pid
         ~actor_key:"cli:t:u-inv-6" ~now:fixed_now ())
  in
  Alcotest.(check int) "no bindings destroyed" 0 receipt.bindings_matched;
  Alcotest.(check int) "pending zeroed" 2 receipt.pending_auth_invalidated;
  Alcotest.(check string)
    "kind" "connector_split"
    (Inv.string_of_kind receipt.kind)

let test_connector_split_with_binding_ids_destroys () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let pid = seed_principal ~db ~principal_id:"prin_inv_7" in
  let vault =
    create_vault ~db ~keys ~principal_id:"prin_inv_7" ~vault_id:"ghvault_inv_7"
      ~user_id:github_user_id ~app:app_id
  in
  ignore
    (seed_binding ~db ~id:"ghbind_inv_7" ~principal_id:"prin_inv_7"
       ~vault_id:vault.id ~user_id:github_user_id ~app:app_id ());
  let receipt =
    assert_iok
      (Inv.invalidate_for_connector_split ~db ~keys ~source_principal_id:pid
         ~actor_key:"cli:t:u-inv-7" ~binding_ids:[ "ghbind_inv_7" ]
         ~remote_mode:Inv.Skip ~now:fixed_now ())
  in
  Alcotest.(check int) "matched" 1 receipt.bindings_matched;
  Alcotest.(check int) "secrets" 1 receipt.secrets_destroyed;
  let b = Option.get (assert_ok (B.get ~db ~id:"ghbind_inv_7")) in
  Alcotest.(check string)
    "unlinked" "unlinked"
    (B.string_of_authorization_status b.authorization_status)

(* -------------------------------------------------------------------------- *)
(* with_revocation_token opens inactive vault without re-enabling             *)
(* -------------------------------------------------------------------------- *)

let test_with_revocation_token_inactive_no_reenable () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed_principal ~db ~principal_id:"prin_inv_8");
  let vault =
    create_vault ~db ~keys ~principal_id:"prin_inv_8" ~vault_id:"ghvault_inv_8"
      ~user_id:github_user_id ~app:app_id
  in
  let account =
    assert_ok
      (V.make_account_key ~principal_id:"prin_inv_8" ~github_user_id ~app_id ())
  in
  let _ =
    assert_ok
      (match
         V.cas_set_active ~db ~keys ~now:fixed_now ~id:vault.id
           ~expected_generation:vault.generation ~expected_active:true
           ~expected:account ~active:false ()
       with
      | Ok r -> Ok r
      | Error d -> Error (V.string_of_denial d))
  in
  let seen = ref None in
  let result =
    Inv.with_revocation_token ~db ~keys ~vault_id:vault.id
      ~f:(fun ~access_token ~refresh_token ->
        seen := Some (access_token, refresh_token);
        "ok")
      ()
  in
  (match result with
  | Ok "ok" -> ()
  | Ok _ -> Alcotest.fail "unexpected"
  | Error d -> Alcotest.fail (V.string_of_denial d));
  (match !seen with
  | Some (access, Some refresh) ->
      Alcotest.(check bool)
        "got access" true
        (String_util.contains access "ghu_access_INV_");
      Alcotest.(check bool)
        "got refresh" true
        (String_util.contains refresh "ghr_refresh_INV_")
  | _ -> Alcotest.fail "tokens not opened");
  match V.get_meta ~db ~id:vault.id with
  | Ok (Some meta) ->
      Alcotest.(check bool) "still inactive after open" false meta.active
  | _ -> Alcotest.fail "meta missing"

let suite =
  [
    Alcotest.test_case "local disable and lineage break precede remote revoke"
      `Quick test_local_disable_and_lineage_before_remote;
    Alcotest.test_case
      "remote failure never re-enables; secrets still destroyed" `Quick
      test_remote_failure_never_reenables_and_destroys_secrets;
    Alcotest.test_case "disable preserves secrets and lineage" `Quick
      test_disable_preserves_secrets_and_lineage;
    Alcotest.test_case
      "principal removal invalidates all bindings and pending auth" `Quick
      test_principal_removal_all_bindings_and_pending;
    Alcotest.test_case "old lineage fails re-resolve and does not follow relink"
      `Quick test_old_lineage_fails_reresolve_after_break;
    Alcotest.test_case "connector split lifecycle pending-only" `Quick
      test_connector_split_lifecycle_pending_only;
    Alcotest.test_case "connector split with binding_ids destroys" `Quick
      test_connector_split_with_binding_ids_destroys;
    Alcotest.test_case
      "with_revocation_token opens inactive vault without re-enable" `Quick
      test_with_revocation_token_inactive_no_reenable;
  ]

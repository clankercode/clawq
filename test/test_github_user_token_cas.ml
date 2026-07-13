(** Tests for generation-based CAS transitions and lease invalidation
    (P21.M2.E4.T004).

    Contract under test:
    - Replace compares binding + generation + active transactionally.
    - Concurrent stale writers cannot restore older token or active state.
    - Disable / revoke / unlink bump generation, set inactive, invalidate leases
      immediately, and (when bound) update binding authorization status. *)

module V = Github_user_token_vault
module S = Github_user_token_store
module L = Github_user_token_lease
module C = Github_user_token_cas
module B = Github_account_binding
module P = Principal_identity
module PS = Principal_identity_store

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-cas-test-master" ()

let sample_tokens ?(tag = "base") () =
  {
    S.access_token = Printf.sprintf "ghu_access_CAS_%s_PLAINTEXT" tag;
    refresh_token = Some (Printf.sprintf "ghr_refresh_CAS_%s_PLAINTEXT" tag);
  }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let account ?(principal_id = "prin_cas_1") ?(github_user_id = 7001L)
    ?(app_id = 55) ?(host = V.default_host) () =
  assert_ok (V.make_account_key ~principal_id ~github_user_id ~app_id ~host ())

let make_keys ?(key_id = "mk-cas-1") ?(key_version = 1) () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key ())

let fixed_now = 1_720_000_100.0
let far_expires = "2026-12-01T00:00:00Z"

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  V.ensure_schema db;
  B.ensure_schema db;
  Fun.protect
    ~finally:(fun () ->
      ignore (L.discard_all ());
      ignore (Sqlite3.db_close db))
    (fun () -> f db)

let create_vault ~db ?(keys = make_keys ()) ?(account = account ())
    ?(tokens = sample_tokens ()) ?(id = "ghvault_cas_1") () =
  match
    V.create ~db ~keys ~id ~now:fixed_now ~account ~tokens
      ~scopes:[ "repo"; "read:user" ] ~expires_at:far_expires ()
  with
  | Ok r -> r
  | Error d -> Alcotest.fail ("create: " ^ V.string_of_denial d)

let seed_binding ~db ~principal_id ~vault_id ~github_user_id ~app_id =
  let pid = assert_ok (P.principal_id_of_string principal_id) in
  let p =
    P.make_principal ~id:pid ~revision:1 ~created_at:"2026-01-01T00:00:00Z"
      ~updated_at:"2026-01-01T00:00:00Z" ()
  in
  ignore (assert_ok (PS.insert_principal ~db ~now:fixed_now p));
  let identity =
    assert_ok
      (B.make_account_identity ~host:B.default_host ~app_id ~github_user_id ())
  in
  let vault_ref = assert_ok (B.make_vault_ref vault_id) in
  let b =
    B.make_binding ~id:"ghbind_cas_1" ~principal_id:pid ~identity
      ~authorization_status:B.Authorized ~vault_ref ~lineage_id:"lineage_cas" ()
  in
  (* make_binding takes optional vault_ref as labeled ?vault_ref *)
  assert_ok (B.insert ~db ~now:fixed_now b)

(* -------------------------------------------------------------------------- *)
(* Replace CAS                                                                *)
(* -------------------------------------------------------------------------- *)

let test_replace_cas_binding_generation_active () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ = create_vault ~db ~keys ~account:acct () in
  Alcotest.(check bool) "created active" true rec_.active;
  let rotated = sample_tokens ~tag:"rotated" () in
  match
    C.replace ~db ~keys ~now:(fixed_now +. 1.) ~id:rec_.id
      ~expected_generation:1 ~expected:acct ~tokens:rotated ~scopes:[ "repo" ]
      ~expires_at:far_expires ()
  with
  | Error d -> Alcotest.fail (C.string_of_denial d)
  | Ok t -> (
      Alcotest.(check int) "gen advanced" 2 t.record.generation;
      Alcotest.(check bool) "still active" true t.record.active;
      match V.read ~db ~keys ~id:rec_.id () with
      | Error d -> Alcotest.fail (V.string_of_denial d)
      | Ok opened ->
          Alcotest.(check string)
            "new access" rotated.access_token opened.tokens.access_token)

let test_replace_rejects_wrong_binding () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account ~principal_id:"prin_a" () in
  let rec_ = create_vault ~db ~keys ~account:acct () in
  let wrong = account ~principal_id:"prin_b" () in
  match
    C.replace ~db ~keys ~id:rec_.id ~expected_generation:1 ~expected:wrong
      ~tokens:(sample_tokens ~tag:"x" ())
      ~scopes:[] ~expires_at:far_expires ()
  with
  | Error (C.Vault (V.Account_mismatch _)) -> ()
  | Error d ->
      Alcotest.fail ("expected account_mismatch, got " ^ C.string_of_denial d)
  | Ok _ -> Alcotest.fail "wrong binding must fail"

let test_concurrent_stale_replace_cannot_restore_old_token () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let old_tokens = sample_tokens ~tag:"old" () in
  let rec_ = create_vault ~db ~keys ~account:acct ~tokens:old_tokens () in
  (* Writer A observes gen=1. *)
  let stale_expected_gen = 1 in
  let stale_tokens = old_tokens in
  (* Writer B wins a replace first. *)
  let winner_tokens = sample_tokens ~tag:"winner" () in
  (match
     C.replace ~db ~keys ~now:(fixed_now +. 1.) ~id:rec_.id
       ~expected_generation:1 ~expected:acct ~tokens:winner_tokens
       ~scopes:[ "repo" ] ~expires_at:far_expires ()
   with
  | Ok t -> Alcotest.(check int) "winner gen" 2 t.record.generation
  | Error d -> Alcotest.fail ("winner: " ^ C.string_of_denial d));
  (* Stale writer A tries to restore old tokens under the pre-CAS generation. *)
  (match
     C.replace ~db ~keys ~now:(fixed_now +. 2.) ~id:rec_.id
       ~expected_generation:stale_expected_gen ~expected:acct
       ~tokens:stale_tokens ~scopes:[] ~expires_at:far_expires ()
   with
  | Error (C.Vault (V.Generation_conflict { expected = 1; actual = 2 })) -> ()
  | Error d ->
      Alcotest.fail ("expected generation_conflict, got " ^ C.string_of_denial d)
  | Ok _ -> Alcotest.fail "stale replace must not succeed");
  match V.read ~db ~keys ~id:rec_.id () with
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok opened ->
      Alcotest.(check string)
        "winner tokens retained" winner_tokens.access_token
        opened.tokens.access_token;
      Alcotest.(check bool)
        "old tokens not restored" false
        (String.equal opened.tokens.access_token old_tokens.access_token)

let test_stale_writer_cannot_restore_active_after_revoke () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ = create_vault ~db ~keys ~account:acct () in
  (match
     C.revoke ~db ~keys ~now:(fixed_now +. 1.) ~id:rec_.id
       ~expected_generation:1 ~expected:acct ()
   with
  | Ok t ->
      Alcotest.(check int) "revoked gen" 2 t.record.generation;
      Alcotest.(check bool) "inactive" false t.record.active
  | Error d -> Alcotest.fail (C.string_of_denial d));
  (* Stale concurrent writer tries to replace with gen=1 (pre-revoke). *)
  (match
     C.replace ~db ~keys ~id:rec_.id ~expected_generation:1 ~expected:acct
       ~tokens:(sample_tokens ~tag:"stale" ())
       ~scopes:[] ~expires_at:far_expires ()
   with
  | Error (C.Vault (V.Generation_conflict _)) -> ()
  | Error d ->
      Alcotest.fail ("expected gen conflict, got " ^ C.string_of_denial d)
  | Ok _ -> Alcotest.fail "stale replace after revoke must fail");
  (* Even with current generation, replace requires active=true. *)
  match
    V.replace ~db ~keys ~id:rec_.id ~expected_generation:2 ~expected_active:true
      ~expected:acct
      ~tokens:(sample_tokens ~tag:"reactivate" ())
      ~scopes:[] ~expires_at:far_expires ()
  with
  | Error V.Not_active -> ()
  | Error (V.Active_conflict _) -> ()
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok _ -> Alcotest.fail "replace must not re-activate inactive row"

(* -------------------------------------------------------------------------- *)
(* Lease invalidation                                                         *)
(* -------------------------------------------------------------------------- *)

let test_replace_invalidates_old_leases () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ = create_vault ~db ~keys ~account:acct () in
  let lease =
    match L.issue ~db ~now:fixed_now ~vault_id:rec_.id () with
    | Ok l -> l
    | Error d -> Alcotest.fail (L.string_of_denial d)
  in
  (match
     C.replace ~db ~keys ~now:(fixed_now +. 1.) ~id:rec_.id
       ~expected_generation:1 ~expected:acct
       ~tokens:(sample_tokens ~tag:"post" ())
       ~scopes:[ "repo" ] ~expires_at:far_expires ()
   with
  | Ok t ->
      Alcotest.(check bool)
        "invalidated at least one" true
        (t.leases_invalidated >= 1)
  | Error d -> Alcotest.fail (C.string_of_denial d));
  Alcotest.(check bool) "lease revoked" true (L.is_revoked lease);
  match
    L.with_token ~db ~keys ~now:fixed_now ~lease
      ~f:(fun ~access_token:_ -> "nope")
      ()
  with
  | Error L.Lease_revoked -> ()
  | Error (L.Generation_mismatch _) -> ()
  | Error d -> Alcotest.fail (L.string_of_denial d)
  | Ok _ -> Alcotest.fail "old lease must fail after replace"

let test_revoke_invalidates_leases_immediately () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ = create_vault ~db ~keys ~account:acct () in
  let lease =
    match L.issue ~db ~now:fixed_now ~vault_id:rec_.id () with
    | Ok l -> l
    | Error d -> Alcotest.fail (L.string_of_denial d)
  in
  (* Prove lease works before revoke. *)
  (match
     L.with_token ~db ~keys ~now:fixed_now ~lease
       ~f:(fun ~access_token -> access_token)
       ()
   with
  | Ok tok ->
      Alcotest.(check string)
        "pre-revoke access" (sample_tokens ()).access_token tok
  | Error d -> Alcotest.fail ("pre: " ^ L.string_of_denial d));
  (match
     C.revoke ~db ~keys ~now:(fixed_now +. 1.) ~id:rec_.id
       ~expected_generation:1 ~expected:acct ()
   with
  | Ok t ->
      Alcotest.(check bool) "inactive" false t.record.active;
      Alcotest.(check int) "gen" 2 t.record.generation;
      Alcotest.(check bool) "leases invalidated" true (t.leases_invalidated >= 1)
  | Error d -> Alcotest.fail (C.string_of_denial d));
  Alcotest.(check bool) "revoked flag" true (L.is_revoked lease);
  (match
     L.with_token ~db ~keys ~now:fixed_now ~lease
       ~f:(fun ~access_token:_ -> ())
       ()
   with
  | Error L.Lease_revoked -> ()
  | Error L.Vault_not_active -> ()
  | Error (L.Generation_mismatch _) -> ()
  | Error d -> Alcotest.fail (L.string_of_denial d)
  | Ok () -> Alcotest.fail "lease must fail after revoke");
  (* New issue must also fail while inactive. *)
  match L.issue ~db ~now:fixed_now ~vault_id:rec_.id () with
  | Error L.Vault_not_active -> ()
  | Error d -> Alcotest.fail (L.string_of_denial d)
  | Ok _ -> Alcotest.fail "issue on inactive vault must fail"

let test_disable_and_unlink_with_binding () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct =
    account ~principal_id:"prin_cas_bind" ~github_user_id:7001L ~app_id:55 ()
  in
  let rec_ = create_vault ~db ~keys ~account:acct ~id:"ghvault_cas_bind" () in
  let binding =
    seed_binding ~db ~principal_id:"prin_cas_bind" ~vault_id:rec_.id
      ~github_user_id:7001L ~app_id:55
  in
  Alcotest.(check string) "bind id" "ghbind_cas_1" binding.id;
  (match
     C.disable ~db ~keys ~now:(fixed_now +. 1.) ~id:rec_.id
       ~expected_generation:1 ~expected:acct ~binding_id:binding.id ()
   with
  | Error d -> Alcotest.fail ("disable: " ^ C.string_of_denial d)
  | Ok t -> (
      Alcotest.(check bool) "inactive after disable" false t.record.active;
      match t.binding with
      | None -> Alcotest.fail "expected binding update"
      | Some b ->
          Alcotest.(check bool)
            "status disabled" true
            (match b.authorization_status with
            | B.Disabled -> true
            | _ -> false)));
  (* Re-create vault row path: re-enable via cas_set_active then unlink. *)
  (match
     V.cas_set_active ~db ~keys ~now:(fixed_now +. 2.) ~id:rec_.id
       ~expected_generation:2 ~expected_active:false ~expected:acct ~active:true
       ()
   with
  | Ok r ->
      Alcotest.(check bool) "re-enabled" true r.active;
      Alcotest.(check int) "gen 3" 3 r.generation
  | Error d -> Alcotest.fail (V.string_of_denial d));
  match
    C.unlink ~db ~keys ~now:(fixed_now +. 3.) ~id:rec_.id ~expected_generation:3
      ~expected:acct ~binding_id:binding.id ()
  with
  | Error d -> Alcotest.fail ("unlink: " ^ C.string_of_denial d)
  | Ok t -> (
      Alcotest.(check bool) "inactive after unlink" false t.record.active;
      match t.binding with
      | None -> Alcotest.fail "expected binding on unlink"
      | Some b ->
          Alcotest.(check bool)
            "status unlinked" true
            (match b.authorization_status with
            | B.Unlinked -> true
            | _ -> false);
          Alcotest.(check bool)
            "vault_ref cleared" true
            (match b.vault_ref with None -> true | Some _ -> false))

let test_denial_never_embeds_token () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let tokens = sample_tokens ~tag:"secret" () in
  let rec_ = create_vault ~db ~keys ~account:acct ~tokens () in
  match
    C.replace ~db ~keys ~id:rec_.id ~expected_generation:99 ~expected:acct
      ~tokens ~scopes:[] ~expires_at:far_expires ()
  with
  | Ok _ -> Alcotest.fail "expected conflict"
  | Error d ->
      Alcotest.(check bool)
        "no access in denial" false
        (C.denial_exposes_token ~denial:d ~plaintext:tokens.access_token)

let suite =
  [
    Alcotest.test_case "replace CAS on binding+generation+active" `Quick
      test_replace_cas_binding_generation_active;
    Alcotest.test_case "replace rejects wrong binding" `Quick
      test_replace_rejects_wrong_binding;
    Alcotest.test_case "concurrent stale replace cannot restore old token"
      `Quick test_concurrent_stale_replace_cannot_restore_old_token;
    Alcotest.test_case "stale writer cannot restore active after revoke" `Quick
      test_stale_writer_cannot_restore_active_after_revoke;
    Alcotest.test_case "replace invalidates old leases" `Quick
      test_replace_invalidates_old_leases;
    Alcotest.test_case "revoke invalidates leases immediately" `Quick
      test_revoke_invalidates_leases_immediately;
    Alcotest.test_case "disable and unlink update binding + clear vault_ref"
      `Quick test_disable_and_unlink_with_binding;
    Alcotest.test_case "denials never embed token material" `Quick
      test_denial_never_embeds_token;
  ]

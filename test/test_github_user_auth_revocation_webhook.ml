(** Tests for GitHub App authorization revocation webhooks (P21.M3.E1.T003).

    Contract under test:
    - Verified App identity + sender numeric id disables every matching binding
    - CAS revoke advances generation, discards leases, blocks re-issue
    - Local vault secrets are destroyed
    - Exactly one redacted receipt per delivery_id (idempotent)
    - Receipts never embed token plaintext *)

module R = Github_user_auth_revocation_webhook
module I = Github_app_webhook_ingress
module V = Github_user_token_vault
module S = Github_user_token_store
module L = Github_user_token_lease
module C = Github_user_token_cas
module B = Github_account_binding
module P = Principal_identity
module PS = Principal_identity_store

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-revocation-test-master"
    ()

let sample_tokens ?(tag = "base") () =
  {
    S.access_token = Printf.sprintf "ghu_access_REV_%s_PLAINTEXT" tag;
    refresh_token = Some (Printf.sprintf "ghr_refresh_REV_%s_PLAINTEXT" tag);
  }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let assert_rok = function
  | Ok v -> v
  | Error d -> Alcotest.fail (R.string_of_denial d)

let fixed_now = 1_720_000_200.0
let far_expires = "2026-12-01T00:00:00Z"
let app_id = 42
let github_user_id = 9001L
let webhook_secret = "revocation-webhook-secret"

let make_keys ?(key_id = "mk-rev-1") ?(key_version = 1) () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key ())

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  R.ensure_schema db;
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
    ?(status = B.Authorized) () =
  let pid = assert_ok (P.principal_id_of_string principal_id) in
  let identity =
    assert_ok
      (B.make_account_identity ~host:B.default_host ~app_id:app
         ~github_user_id:user_id ())
  in
  let vault_ref = assert_ok (B.make_vault_ref vault_id) in
  let b =
    B.make_binding ~id ~principal_id:pid ~identity ~authorization_status:status
      ~vault_ref ~lineage_id:(id ^ "_lineage") ()
  in
  assert_ok (B.insert ~db ~now:fixed_now b)

let revocation_payload ?(user_id = github_user_id) ?(action = "revoked") () =
  Printf.sprintf
    {|{"action":%S,"sender":{"login":"octocat","id":%Ld,"type":"User"}}|} action
    user_id

let sign body =
  "sha256=" ^ Digestif.SHA256.(hmac_string ~key:webhook_secret body |> to_hex)

let make_request ~delivery_id ?(event = "github_app_authorization") ~body () =
  {
    I.body;
    headers =
      {
        I.delivery_id = Some delivery_id;
        event = Some event;
        signature_header = Some (sign body);
        user_agent = Some "GitHub-Hookshot/test";
      };
    path = I.default_path;
  }

let inject_verified ~delivery_id ?(user_id = github_user_id) ?(app = app_id) ()
    : R.verified_revocation =
  {
    delivery_id;
    app_id = app;
    github_user_id = user_id;
    action = "revoked";
    event = "github_app_authorization";
  }

let outcome_receipt = function
  | R.Applied r | R.Duplicate r -> r
  | R.Ignored { reason; message } ->
      Alcotest.fail (Printf.sprintf "ignored: %s (%s)" reason message)

(* -------------------------------------------------------------------------- *)
(* Ingress accepts authorization revocation without installation              *)
(* -------------------------------------------------------------------------- *)
let test_ingress_accepts_authorization_revocation () =
  with_db @@ fun db ->
  let body = revocation_payload () in
  let delivery_id = "deliv-authz-1" in
  let req = make_request ~delivery_id ~body () in
  match
    I.verify_and_accept ~db ~webhook_secret ~expected_app_id:app_id
      ~now:fixed_now req
  with
  | I.Accepted a ->
      Alcotest.(check string) "event" "github_app_authorization" a.event;
      Alcotest.(check (option string)) "action" (Some "revoked") a.action;
      Alcotest.(check (option int)) "app from config" (Some app_id) a.app_id;
      Alcotest.(check bool)
        "no installation required" true
        (match a.installation_id with None -> true | Some _ -> false)
  | I.Rejected { reason; message } ->
      Alcotest.fail
        (Printf.sprintf "rejected: %s %s"
           (I.reject_reason_to_string reason)
           message)
  | I.Duplicate _ -> Alcotest.fail "unexpected duplicate"

(* -------------------------------------------------------------------------- *)
(* Happy path: revoke matching binding, destroy secrets, one receipt          *)
(* -------------------------------------------------------------------------- *)

let test_process_revokes_binding_destroys_secrets_and_receipt () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed_principal ~db ~principal_id:"prin_rev_1");
  let vault =
    create_vault ~db ~keys ~principal_id:"prin_rev_1" ~vault_id:"ghvault_rev_1"
      ~user_id:github_user_id ~app:app_id
  in
  let binding =
    seed_binding ~db ~id:"ghbind_rev_1" ~principal_id:"prin_rev_1"
      ~vault_id:vault.id ~user_id:github_user_id ~app:app_id ()
  in
  let plaintext = (sample_tokens ~tag:vault.id ()).access_token in
  let lease =
    match L.issue ~db ~now:fixed_now ~vault_id:vault.id () with
    | Ok l -> l
    | Error d -> Alcotest.fail (L.string_of_denial d)
  in
  let verified = inject_verified ~delivery_id:"deliv-rev-happy" () in
  let receipt =
    match R.process_verified ~db ~keys ~now:fixed_now ~verified () with
    | Ok (R.Applied r) -> r
    | Ok other ->
        Alcotest.fail
          (match other with
          | R.Duplicate _ -> "duplicate"
          | R.Ignored { reason; _ } -> "ignored:" ^ reason
          | R.Applied _ -> "applied")
    | Error d -> Alcotest.fail (R.string_of_denial d)
  in
  Alcotest.(check int) "matched 1" 1 receipt.bindings_matched;
  Alcotest.(check int) "revoked 1" 1 receipt.bindings_revoked;
  Alcotest.(check int) "secrets destroyed" 1 receipt.secrets_destroyed;
  Alcotest.(check bool)
    "leases invalidated" true
    (receipt.leases_invalidated >= 1);
  Alcotest.(check bool) "not already" false receipt.already_processed;
  Alcotest.(check bool)
    "receipt redacted" false
    (R.receipt_contains_plaintext ~receipt ~plaintext);
  (* Binding status Revoked, vault_ref cleared. *)
  (match B.get ~db ~id:binding.id with
  | Ok (Some b) ->
      Alcotest.(check bool)
        "status revoked" true
        (match b.authorization_status with B.Revoked -> true | _ -> false);
      Alcotest.(check bool)
        "vault_ref cleared" true
        (match b.vault_ref with None -> true | Some _ -> false)
  | Ok None -> Alcotest.fail "binding missing"
  | Error e -> Alcotest.fail e);
  (* Vault row gone. *)
  (match V.get_meta ~db ~id:vault.id with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "vault should be destroyed"
  | Error d -> Alcotest.fail (V.string_of_denial d));
  (* Lease blocked. *)
  Alcotest.(check bool) "lease revoked" true (L.is_revoked lease);
  (match
     L.with_token ~db ~keys ~now:fixed_now ~lease
       ~f:(fun ~access_token:_ -> ())
       ()
   with
  | Error L.Lease_revoked
  | Error L.Lease_not_found
  | Error L.Vault_not_active
  | Error (L.Generation_mismatch _)
  | Error (L.Vault _) ->
      ()
  | Error d -> Alcotest.fail (L.string_of_denial d)
  | Ok () -> Alcotest.fail "lease must fail after revoke");
  (* New lease issue fails closed. *)
  (match L.issue ~db ~now:fixed_now ~vault_id:vault.id () with
  | Error L.Vault_not_active | Error L.Lease_not_found | Error (L.Vault _) -> ()
  | Error d -> Alcotest.fail (L.string_of_denial d)
  | Ok _ -> Alcotest.fail "issue after destroy must fail");
  (* One stored receipt. *)
  match R.get_receipt_by_delivery ~db ~delivery_id:receipt.delivery_id with
  | Ok (Some r) ->
      Alcotest.(check string) "same id" receipt.id r.id;
      Alcotest.(check bool) "loaded already" true r.already_processed
  | Ok None -> Alcotest.fail "receipt not stored"
  | Error e -> Alcotest.fail e

(* -------------------------------------------------------------------------- *)
(* Idempotent on delivery_id                                                  *)
(* -------------------------------------------------------------------------- *)

let test_idempotent_same_delivery () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed_principal ~db ~principal_id:"prin_rev_2");
  let vault =
    create_vault ~db ~keys ~principal_id:"prin_rev_2" ~vault_id:"ghvault_rev_2"
      ~user_id:github_user_id ~app:app_id
  in
  ignore
    (seed_binding ~db ~id:"ghbind_rev_2" ~principal_id:"prin_rev_2"
       ~vault_id:vault.id ~user_id:github_user_id ~app:app_id ());
  let verified = inject_verified ~delivery_id:"deliv-rev-idem" () in
  let first =
    outcome_receipt
      (assert_rok (R.process_verified ~db ~keys ~now:fixed_now ~verified ()))
  in
  let second =
    match R.process_verified ~db ~keys ~now:(fixed_now +. 1.) ~verified () with
    | Ok (R.Duplicate r) -> r
    | Ok (R.Applied _) -> Alcotest.fail "second must be Duplicate"
    | Ok (R.Ignored _) -> Alcotest.fail "ignored"
    | Error d -> Alcotest.fail (R.string_of_denial d)
  in
  Alcotest.(check string) "same receipt id" first.id second.id;
  Alcotest.(check int)
    "matched unchanged" first.bindings_matched second.bindings_matched;
  Alcotest.(check bool) "already flag" true second.already_processed

(* -------------------------------------------------------------------------- *)
(* Full request path with default verifier                                    *)
(* -------------------------------------------------------------------------- *)

let test_process_request_default_verifier () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed_principal ~db ~principal_id:"prin_rev_3");
  let vault =
    create_vault ~db ~keys ~principal_id:"prin_rev_3" ~vault_id:"ghvault_rev_3"
      ~user_id:github_user_id ~app:app_id
  in
  ignore
    (seed_binding ~db ~id:"ghbind_rev_3" ~principal_id:"prin_rev_3"
       ~vault_id:vault.id ~user_id:github_user_id ~app:app_id ());
  let body = revocation_payload () in
  let delivery_id = "deliv-rev-req" in
  let req = make_request ~delivery_id ~body () in
  let receipt =
    match
      R.process ~db ~keys ~webhook_secret ~expected_app_id:app_id ~now:fixed_now
        ~request:req ()
    with
    | Ok (R.Applied r) -> r
    | Ok _ -> Alcotest.fail "expected Applied"
    | Error d -> Alcotest.fail (R.string_of_denial d)
  in
  Alcotest.(check string) "delivery" delivery_id receipt.delivery_id;
  Alcotest.(check int) "matched" 1 receipt.bindings_matched;
  (* Ingress replay + process → Duplicate receipt. *)
  let req2 = make_request ~delivery_id ~body () in
  match
    R.process ~db ~keys ~webhook_secret ~expected_app_id:app_id
      ~now:(fixed_now +. 1.) ~request:req2 ()
  with
  | Ok (R.Duplicate r) -> Alcotest.(check string) "same" receipt.id r.id
  | Ok (R.Applied _) -> Alcotest.fail "replay must not re-apply"
  | Ok (R.Ignored _) -> Alcotest.fail "ignored"
  | Error d -> Alcotest.fail (R.string_of_denial d)

(* -------------------------------------------------------------------------- *)
(* Bad signature / wrong App rejected                                         *)
(* -------------------------------------------------------------------------- *)

let test_bad_signature_rejected () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let body = revocation_payload () in
  let req =
    {
      I.body;
      headers =
        {
          I.delivery_id = Some "deliv-bad-sig";
          event = Some "github_app_authorization";
          signature_header = Some "sha256=deadbeef";
          user_agent = Some "GitHub-Hookshot/test";
        };
      path = I.default_path;
    }
  in
  match
    R.process ~db ~keys ~webhook_secret ~expected_app_id:app_id ~now:fixed_now
      ~request:req ()
  with
  | Error (R.Verify (R.Ingress (I.Bad_signature, _))) -> ()
  | Error d -> Alcotest.fail ("unexpected denial: " ^ R.string_of_denial d)
  | Ok _ -> Alcotest.fail "bad signature must fail"

let test_wrong_app_identity_rejected () =
  with_db @@ fun db ->
  let keys = make_keys () in
  (* Payload may omit app_id; configured expected_app_id is authoritative.
     Wrong expected App is still rejected only when payload carries a different
     app_id. Inject a payload with wrong app_id for this check. *)
  let body =
    Printf.sprintf
      {|{"action":"revoked","app_id":999,"sender":{"login":"x","id":%Ld}}|}
      github_user_id
  in
  let req = make_request ~delivery_id:"deliv-wrong-app" ~body () in
  match
    R.process ~db ~keys ~webhook_secret ~expected_app_id:app_id ~now:fixed_now
      ~request:req ()
  with
  | Error (R.Verify (R.Ingress (I.App_id_mismatch, _))) -> ()
  | Error d -> Alcotest.fail ("unexpected: " ^ R.string_of_denial d)
  | Ok _ -> Alcotest.fail "wrong app must fail"

(* -------------------------------------------------------------------------- *)
(* Non-matching user leaves other bindings alone                              *)
(* -------------------------------------------------------------------------- *)

let test_only_matching_user_revoked () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed_principal ~db ~principal_id:"prin_match");
  ignore (seed_principal ~db ~principal_id:"prin_other");
  let vault_match =
    create_vault ~db ~keys ~principal_id:"prin_match" ~vault_id:"ghvault_match"
      ~user_id:github_user_id ~app:app_id
  in
  let vault_other =
    create_vault ~db ~keys ~principal_id:"prin_other" ~vault_id:"ghvault_other"
      ~user_id:8002L ~app:app_id
  in
  ignore
    (seed_binding ~db ~id:"ghbind_match" ~principal_id:"prin_match"
       ~vault_id:vault_match.id ~user_id:github_user_id ~app:app_id ());
  ignore
    (seed_binding ~db ~id:"ghbind_other" ~principal_id:"prin_other"
       ~vault_id:vault_other.id ~user_id:8002L ~app:app_id ());
  let verified = inject_verified ~delivery_id:"deliv-match-only" () in
  let receipt =
    outcome_receipt
      (assert_rok (R.process_verified ~db ~keys ~now:fixed_now ~verified ()))
  in
  Alcotest.(check int) "one match" 1 receipt.bindings_matched;
  (match B.get ~db ~id:"ghbind_match" with
  | Ok (Some b) ->
      Alcotest.(check bool)
        "matched revoked" true
        (match b.authorization_status with B.Revoked -> true | _ -> false)
  | _ -> Alcotest.fail "match binding");
  (match B.get ~db ~id:"ghbind_other" with
  | Ok (Some b) ->
      Alcotest.(check bool)
        "other still authorized" true
        (match b.authorization_status with B.Authorized -> true | _ -> false)
  | _ -> Alcotest.fail "other binding");
  match V.get_meta ~db ~id:vault_other.id with
  | Ok (Some m) -> Alcotest.(check bool) "other active" true m.active
  | _ -> Alcotest.fail "other vault must remain"

(* -------------------------------------------------------------------------- *)
(* Orphan vault secrets destroyed                                             *)
(* -------------------------------------------------------------------------- *)

let test_orphan_vault_destroyed () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed_principal ~db ~principal_id:"prin_orphan");
  let orphan =
    create_vault ~db ~keys ~principal_id:"prin_orphan"
      ~vault_id:"ghvault_orphan" ~user_id:github_user_id ~app:app_id
  in
  (* No binding points at this vault. *)
  let verified = inject_verified ~delivery_id:"deliv-orphan" () in
  let receipt =
    outcome_receipt
      (assert_rok (R.process_verified ~db ~keys ~now:fixed_now ~verified ()))
  in
  Alcotest.(check int) "no bindings" 0 receipt.bindings_matched;
  Alcotest.(check int) "orphan destroyed" 1 receipt.orphan_secrets_destroyed;
  match V.get_meta ~db ~id:orphan.id with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "orphan vault still present"
  | Error d -> Alcotest.fail (V.string_of_denial d)

(* -------------------------------------------------------------------------- *)
(* Already-revoked binding is idempotent (no crash, one receipt)              *)
(* -------------------------------------------------------------------------- *)

let test_already_revoked_binding_idempotent () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed_principal ~db ~principal_id:"prin_already");
  let vault =
    create_vault ~db ~keys ~principal_id:"prin_already"
      ~vault_id:"ghvault_already" ~user_id:github_user_id ~app:app_id
  in
  let acct =
    assert_ok
      (V.make_account_key ~principal_id:"prin_already" ~github_user_id ~app_id
         ())
  in
  ignore
    (seed_binding ~db ~id:"ghbind_already" ~principal_id:"prin_already"
       ~vault_id:vault.id ~user_id:github_user_id ~app:app_id ());
  (* Local revoke first (generation advances). *)
  (match
     C.revoke ~db ~keys ~now:fixed_now ~id:vault.id ~expected_generation:1
       ~expected:acct ~binding_id:"ghbind_already" ()
   with
  | Ok t ->
      Alcotest.(check bool) "inactive" false t.record.active;
      Alcotest.(check int) "gen 2" 2 t.record.generation
  | Error d -> Alcotest.fail (C.string_of_denial d));
  let verified = inject_verified ~delivery_id:"deliv-already" () in
  let receipt =
    outcome_receipt
      (assert_rok
         (R.process_verified ~db ~keys ~now:(fixed_now +. 1.) ~verified ()))
  in
  Alcotest.(check int) "matched" 1 receipt.bindings_matched;
  Alcotest.(check bool)
    "effect already_revoked" true
    (match receipt.effects with [ e ] -> e.already_revoked | _ -> false);
  Alcotest.(check int) "secrets still destroyed" 1 receipt.secrets_destroyed

(* -------------------------------------------------------------------------- *)
(* Injectable verifier                                                        *)
(* -------------------------------------------------------------------------- *)

let test_injectable_verifier () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed_principal ~db ~principal_id:"prin_inject");
  let vault =
    create_vault ~db ~keys ~principal_id:"prin_inject"
      ~vault_id:"ghvault_inject" ~user_id:github_user_id ~app:app_id
  in
  ignore
    (seed_binding ~db ~id:"ghbind_inject" ~principal_id:"prin_inject"
       ~vault_id:vault.id ~user_id:github_user_id ~app:app_id ());
  let fake_verify ~db:_ ~webhook_secret:_ ~expected_app_id ?now:_ ~request:_ ()
      =
    Ok
      {
        R.delivery_id = "deliv-inject";
        app_id = expected_app_id;
        github_user_id;
        action = "revoked";
        event = "github_app_authorization";
      }
  in
  let body = "{}" in
  let req = make_request ~delivery_id:"ignored" ~body () in
  match
    R.process ~db ~keys ~webhook_secret:"unused" ~expected_app_id:app_id
      ~verify:fake_verify ~now:fixed_now ~request:req ()
  with
  | Ok (R.Applied r) ->
      Alcotest.(check string)
        "delivery from verifier" "deliv-inject" r.delivery_id;
      Alcotest.(check int) "matched" 1 r.bindings_matched
  | Ok _ -> Alcotest.fail "expected Applied"
  | Error d -> Alcotest.fail (R.string_of_denial d)

(* -------------------------------------------------------------------------- *)
(* Wrong action ignored                                                       *)
(* -------------------------------------------------------------------------- *)

let test_non_revoked_action_ignored () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let verified =
    {
      R.delivery_id = "deliv-ignore-action";
      app_id;
      github_user_id;
      action = "created";
      event = "github_app_authorization";
    }
  in
  match R.process_verified ~db ~keys ~now:fixed_now ~verified () with
  | Ok (R.Ignored { reason; _ }) ->
      Alcotest.(check string) "reason" "wrong_action" reason
  | Ok _ -> Alcotest.fail "expected Ignored"
  | Error d -> Alcotest.fail (R.string_of_denial d)

(* -------------------------------------------------------------------------- *)
(* Denials never embed tokens                                                 *)
(* -------------------------------------------------------------------------- *)

let test_denials_never_embed_tokens () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let plaintext = (sample_tokens ~tag:"secret" ()).access_token in
  let verified =
    {
      R.delivery_id = "";
      app_id;
      github_user_id;
      action = "revoked";
      event = "github_app_authorization";
    }
  in
  match R.process_verified ~db ~keys ~now:fixed_now ~verified () with
  | Error d ->
      Alcotest.(check bool)
        "no token in denial" false
        (R.denial_exposes_token ~denial:d ~plaintext);
      Alcotest.(check bool)
        "invalid input" true
        (String_util.contains (R.string_of_denial d) "delivery_id")
  | Ok _ -> Alcotest.fail "empty delivery must fail"

let suite =
  [
    Alcotest.test_case
      "ingress accepts github_app_authorization without install" `Quick
      test_ingress_accepts_authorization_revocation;
    Alcotest.test_case
      "process revokes binding, destroys secrets, one redacted receipt" `Quick
      test_process_revokes_binding_destroys_secrets_and_receipt;
    Alcotest.test_case "same delivery_id is idempotent" `Quick
      test_idempotent_same_delivery;
    Alcotest.test_case "process request with default verifier" `Quick
      test_process_request_default_verifier;
    Alcotest.test_case "bad signature rejected" `Quick
      test_bad_signature_rejected;
    Alcotest.test_case "wrong App identity rejected" `Quick
      test_wrong_app_identity_rejected;
    Alcotest.test_case "only matching sender user is revoked" `Quick
      test_only_matching_user_revoked;
    Alcotest.test_case "orphan vault secrets destroyed" `Quick
      test_orphan_vault_destroyed;
    Alcotest.test_case "already-revoked binding is safe and destroys secrets"
      `Quick test_already_revoked_binding_idempotent;
    Alcotest.test_case "injectable verifier path" `Quick
      test_injectable_verifier;
    Alcotest.test_case "non-revoked action ignored" `Quick
      test_non_revoked_action_ignored;
    Alcotest.test_case "denials never embed token material" `Quick
      test_denials_never_embed_tokens;
  ]

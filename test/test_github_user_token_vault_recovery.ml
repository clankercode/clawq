(** Tests for vault backup/restore and key-compromise recovery (P21.M2.E4.T008).
*)

module V = Github_user_token_vault
module S = Github_user_token_store
module R = Github_user_token_vault_recovery
module Auth = Github_user_auth_tx
module Rewrap = Github_user_token_rewrap
module PI = Principal_identity

let () = Secret_store.test_iterations_override := Some 1

let aes_v1 =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-vault-recov-v1" ()

let aes_v2 =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-vault-recov-v2" ()

let sample_tokens ~n =
  {
    S.access_token = Printf.sprintf "ghu_access_RECOV_PLAIN_%d" n;
    refresh_token = Some (Printf.sprintf "ghr_refresh_RECOV_PLAIN_%d" n);
  }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let fail_denial d = Alcotest.fail (R.string_of_denial d)
let fail_vdenial d = Alcotest.fail (V.string_of_denial d)

let account ?(principal_id = "prin_recov") ?(github_user_id = 42L) ?(app_id = 7)
    () =
  assert_ok (V.make_account_key ~principal_id ~github_user_id ~app_id ())

let make_keys ?(key_id = "mk-recov-1") ?(key_version = 1) ?(aes = aes_v1) () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key:aes ())

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  R.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_720_200_000.0

let seed ~db ~keys ~n =
  let rec go i acc =
    if i > n then List.rev acc
    else
      let acct =
        account
          ~principal_id:(Printf.sprintf "prin_r_%d" i)
          ~github_user_id:(Int64.of_int (2000 + i))
          ()
      in
      let id = Printf.sprintf "ghvault_recov_%02d" i in
      match
        V.create ~db ~keys ~id
          ~now:(fixed_now +. float_of_int i)
          ~account:acct ~tokens:(sample_tokens ~n:i) ~scopes:[ "repo" ]
          ~expires_at:"2026-12-01T00:00:00Z" ()
      with
      | Error d -> fail_vdenial d
      | Ok r -> go (i + 1) (r :: acc)
  in
  go 1 []

let restore_proof () =
  assert_ok
    (R.make_operator_proof ~operator_id:"ops_alice"
       ~approval:"APPROVE-RESTORE-2026-07-13"
       ~acknowledged_limitations:[ R.whole_store_rollback_limitation_tag ]
       ())

let compromise_proof () =
  assert_ok
    (R.make_operator_proof ~operator_id:"ops_bob"
       ~approval:"APPROVE-COMPROMISE-DISABLE"
       ~acknowledged_limitations:[ R.compromise_relink_required_tag ]
       ())

(* -------------------------------------------------------------------------- *)
(* Documented whole-store rollback limitation                                 *)
(* -------------------------------------------------------------------------- *)

let test_whole_store_rollback_limitation_constant () =
  Alcotest.(check bool)
    "not detectable without external anchor" false
    R.whole_store_rollback_detectable_without_external_anchor;
  Alcotest.(check bool)
    "tag non-empty" true
    (String.length R.whole_store_rollback_limitation_tag > 10);
  Alcotest.(check bool)
    "statement mentions external monotonic anchor" true
    (String_util.contains R.whole_store_rollback_limitation_statement
       "external monotonic anchor")

(* -------------------------------------------------------------------------- *)
(* Backup export: encrypted envelopes + key IDs only                          *)
(* -------------------------------------------------------------------------- *)

let test_export_encrypted_only_no_plaintext () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed ~db ~keys ~n:2);
  let backup =
    match R.export_backup ~db ~now:fixed_now () with
    | Ok b -> b
    | Error e -> Alcotest.fail e
  in
  Alcotest.(check int)
    "schema" R.backup_schema_version backup.backup_schema_version;
  Alcotest.(check int) "envelopes" 2 (List.length backup.envelopes);
  Alcotest.(check (list string))
    "required keys" [ "mk-recov-1" ] backup.required_key_ids;
  List.iter
    (fun (e : R.sealed_envelope) ->
      Alcotest.(check bool)
        "ciphertext present" true
        (String.length e.ciphertext > 20);
      Alcotest.(check bool)
        "vault envelope prefix" true
        (String.starts_with ~prefix:"$VAULT_AAD_V1:" e.ciphertext))
    backup.envelopes;
  let t1 = sample_tokens ~n:1 in
  Alcotest.(check bool)
    "no access plaintext" false
    (R.backup_contains_plaintext ~backup ~plaintext:t1.access_token);
  Alcotest.(check bool)
    "no refresh plaintext" false
    (R.backup_contains_plaintext ~backup
       ~plaintext:(Option.get t1.refresh_token));
  Alcotest.(check bool)
    "no aes key" false
    (R.backup_contains_plaintext ~backup ~plaintext:aes_v1);
  let json = R.backup_to_json backup in
  Alcotest.(check bool)
    "json embeds limitation false" true
    (match json with
    | `Assoc fields -> (
        match
          List.assoc_opt
            "whole_store_rollback_detectable_without_external_anchor" fields
        with
        | Some (`Bool false) -> true
        | _ -> false)
    | _ -> false);
  (* Round-trip parse. *)
  match R.backup_of_json json with
  | Error e -> Alcotest.fail e
  | Ok b2 -> Alcotest.(check int) "roundtrip count" 2 (List.length b2.envelopes)

(* -------------------------------------------------------------------------- *)
(* Compatibility by key ID / schema                                           *)
(* -------------------------------------------------------------------------- *)

let test_compatibility_missing_key () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed ~db ~keys ~n:1);
  let backup = assert_ok (R.export_backup ~db ()) in
  let other = make_keys ~key_id:"mk-OTHER" ~aes:aes_v2 () in
  match R.check_compatibility ~keys:other ~backup () with
  | Ok () -> Alcotest.fail "expected missing key"
  | Error issues ->
      Alcotest.(check bool)
        "reports missing key" true
        (List.exists
           (function R.Missing_required_key _ -> true | _ -> false)
           issues)

let test_compatibility_unsupported_schema () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed ~db ~keys ~n:1);
  let backup = assert_ok (R.export_backup ~db ()) in
  let bad = { backup with backup_schema_version = 99 } in
  match R.check_compatibility ~keys ~backup:bad () with
  | Ok () -> Alcotest.fail "expected schema denial"
  | Error issues ->
      Alcotest.(check bool)
        "unsupported backup schema" true
        (List.exists
           (function
             | R.Unsupported_backup_schema { version = 99 } -> true | _ -> false)
           issues)

(* -------------------------------------------------------------------------- *)
(* Operator proof required for restore                                        *)
(* -------------------------------------------------------------------------- *)

let test_restore_requires_operator_proof () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed ~db ~keys ~n:1);
  let backup = assert_ok (R.export_backup ~db ()) in
  let empty_approval =
    {
      R.operator_id = "ops";
      approval = "";
      acknowledged_limitations = [ R.whole_store_rollback_limitation_tag ];
    }
  in
  (match R.restore ~db ~keys ~proof:empty_approval ~backup () with
  | Ok _ -> Alcotest.fail "empty approval must fail"
  | Error (R.Operator_proof_required _) -> ()
  | Error d -> fail_denial d);
  let missing_ack =
    {
      R.operator_id = "ops";
      approval = "YES";
      acknowledged_limitations = [ "some_other_tag" ];
    }
  in
  match R.restore ~db ~keys ~proof:missing_ack ~backup () with
  | Ok _ -> Alcotest.fail "missing limitation ack must fail"
  | Error (R.Operator_proof_required msg) ->
      Alcotest.(check bool)
        "mentions missing tag" true
        (String_util.contains msg R.whole_store_rollback_limitation_tag)
  | Error d -> fail_denial d

(* -------------------------------------------------------------------------- *)
(* Restore happy path + auth disabled                                         *)
(* -------------------------------------------------------------------------- *)

let test_restore_roundtrip_disables_auth () =
  with_db @@ fun db_src ->
  let keys = make_keys () in
  ignore (seed ~db:db_src ~keys ~n:2);
  let backup = assert_ok (R.export_backup ~db:db_src ~now:fixed_now ()) in
  (* Fresh target DB with different live vault content. *)
  with_db @@ fun db_dst ->
  let keys = make_keys () in
  (* Seed a row that must be replaced by restore. *)
  let foreign = account ~principal_id:"foreign" ~github_user_id:9999L () in
  ignore
    (match
       V.create ~db:db_dst ~keys ~id:"ghvault_foreign" ~account:foreign
         ~tokens:
           {
             access_token = "ghu_FOREIGN_must_go";
             refresh_token = Some "ghr_FOREIGN_must_go";
           }
         ~scopes:[ "repo" ] ~expires_at:"2026-01-01T00:00:00Z" ()
     with
    | Ok r -> r
    | Error d -> fail_vdenial d);
  let leases = ref 0 in
  let bindings = ref 0 in
  let hooks : R.destroy_hooks =
    {
      destroy_bindings =
        (fun () ->
          bindings := 3;
          Ok 3);
      destroy_leases =
        (fun () ->
          leases := 5;
          Ok 5);
      destroy_pending_extra = (fun () -> Ok 0);
    }
  in
  let result =
    match
      R.restore ~db:db_dst ~keys ~proof:(restore_proof ()) ~backup ~hooks
        ~now:fixed_now ()
    with
    | Ok r -> r
    | Error d -> fail_denial d
  in
  Alcotest.(check int) "imported" 2 result.imported;
  Alcotest.(check bool) "auth disabled flag" true result.authorization_disabled;
  Alcotest.(check int) "leases discarded" 5 result.leases_discarded;
  Alcotest.(check int) "bindings destroyed" 3 result.bindings_destroyed;
  Alcotest.(check bool)
    "gate disabled" false
    (assert_ok (R.user_authorization_enabled ~db:db_dst));
  (* Foreign row gone; restored rows open with original tokens. *)
  (match V.get_meta ~db:db_dst ~id:"ghvault_foreign" with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "foreign row should be wiped"
  | Error d -> fail_vdenial d);
  match V.read ~db:db_dst ~keys ~id:"ghvault_recov_01" () with
  | Error d -> fail_vdenial d
  | Ok opened ->
      Alcotest.(check string)
        "restored access" (sample_tokens ~n:1).access_token
        opened.tokens.access_token;
      Alcotest.(check int) "generation preserved" 1 opened.record.generation

let test_restore_fails_closed_wrong_key_material () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed ~db ~keys ~n:1);
  let backup = assert_ok (R.export_backup ~db ()) in
  (* Same key_id, different material — open must fail. *)
  let wrong =
    assert_ok
      (V.make_single_key_provider ~key_id:"mk-recov-1" ~key_version:1
         ~aes_key:aes_v2 ())
  in
  match R.restore ~db ~keys:wrong ~proof:(restore_proof ()) ~backup () with
  | Ok _ -> Alcotest.fail "wrong key material must fail closed"
  | Error (R.Compatibility issues) ->
      Alcotest.(check bool)
        "unopenable" true
        (List.exists
           (function R.Unopenable_envelope _ -> true | _ -> false)
           issues)
  | Error d -> fail_denial d

(* -------------------------------------------------------------------------- *)
(* Compromise path                                                            *)
(* -------------------------------------------------------------------------- *)

let seed_auth_tx ~db =
  Auth.ensure_schema db;
  let actor =
    match
      PI.make_connector_actor_key ~connector:PI.Teams
        ~tenant_or_workspace:"tenant-acme" ~immutable_user_id:"user-alice-1"
    with
    | Ok k -> k
    | Error e -> Alcotest.fail e
  in
  match
    Auth.create ~db ~flow_kind:Auth.Web_pkce ~principal_id:"prin_r_1"
      ~connector_actor:actor ~source:(Auth.Room "room1")
      ~app:{ host = "github.com"; app_id = 7; client_id_handle = "cid_handle" }
      ~base_revision:"rev1" ~continuation_handle:"cont1" ~now:fixed_now ()
  with
  | Ok t -> t
  | Error e -> Alcotest.fail e

let test_compromise_disable_destroys_and_requires_relink () =
  with_db @@ fun db ->
  let keys = make_keys () in
  ignore (seed ~db ~keys ~n:2);
  let tx = seed_auth_tx ~db in
  Rewrap.ensure_schema db;
  ignore
    (Sqlite3.exec db
       {|INSERT INTO github_user_token_rewrap
         (id, from_key_id, from_key_version, to_key_id, to_key_version,
          phase, rewrapped_count, conflict_count, created_at, updated_at)
         VALUES ('job_manual','mk-recov-1',1,'mk-recov-2',2,'in_progress',
                 0,0,'t','t')|});
  let hooks : R.destroy_hooks =
    {
      destroy_bindings = (fun () -> Ok 2);
      destroy_leases = (fun () -> Ok 4);
      destroy_pending_extra = (fun () -> Ok 1);
    }
  in
  let result =
    match
      R.compromise_disable ~db ~proof:(compromise_proof ())
        ~reason:"suspected master key leak from backup media" ~hooks
        ~now:fixed_now ()
    with
    | Ok r -> r
    | Error d -> fail_denial d
  in
  Alcotest.(check bool) "auth disabled" true result.authorization_disabled;
  Alcotest.(check int) "vault destroyed" 2 result.vault_records_destroyed;
  Alcotest.(check bool)
    "auth tx cleared" true
    (result.pending_auth_tx_destroyed >= 1);
  Alcotest.(check int) "rewrap cleared" 1 result.rewrap_jobs_destroyed;
  Alcotest.(check int) "bindings" 2 result.bindings_destroyed;
  Alcotest.(check int) "leases" 4 result.leases_discarded;
  Alcotest.(check int) "pending extra" 1 result.pending_extra_destroyed;
  Alcotest.(check bool) "requires rotation" true result.requires_key_rotation;
  Alcotest.(check bool) "requires relink" true result.requires_relink;
  Alcotest.(check (list string))
    "affected keys" [ "mk-recov-1" ] result.affected_key_ids;
  Alcotest.(check int)
    "vault count" 0
    (match V.count_all ~db with Ok n -> n | Error d -> fail_vdenial d);
  (match Auth.get ~db ~id:tx.id with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "auth tx should be destroyed"
  | Error e -> Alcotest.fail e);
  let state = assert_ok (R.load_state ~db) in
  Alcotest.(check bool) "enabled" false state.user_authorization_enabled;
  Alcotest.(check string) "event" "compromise_disable" state.last_event;
  Alcotest.(check bool) "relink" true state.requires_relink;
  Alcotest.(check bool) "rotate" true state.requires_key_rotation;
  let json = R.state_to_json state in
  Alcotest.(check bool)
    "state json no token" false
    (V.json_contains_plaintext ~json
       ~plaintext:(sample_tokens ~n:1).access_token);
  match
    R.compromise_disable ~db
      ~proof:
        { operator_id = "x"; approval = "y"; acknowledged_limitations = [] }
      ~reason:"again" ()
  with
  | Ok _ -> Alcotest.fail "must require compromise ack"
  | Error (R.Operator_proof_required _) -> ()
  | Error d -> fail_denial d

let test_compromise_requires_reason () =
  with_db @@ fun db ->
  match
    R.compromise_disable ~db ~proof:(compromise_proof ()) ~reason:"  " ()
  with
  | Ok _ -> Alcotest.fail "empty reason"
  | Error (R.Invalid_input _) -> ()
  | Error d -> fail_denial d

let test_denial_never_leaks_tokens () =
  let d =
    R.Compatibility [ R.Unopenable_envelope { id = "x"; reason = V.Wrong_key } ]
  in
  Alcotest.(check bool)
    "no leak" false
    (R.denial_exposes_token ~denial:d ~plaintext:"ghu_access_RECOV_PLAIN_1")

let suite =
  [
    Alcotest.test_case "whole-store rollback limitation constant" `Quick
      test_whole_store_rollback_limitation_constant;
    Alcotest.test_case "export encrypted envelopes only" `Quick
      test_export_encrypted_only_no_plaintext;
    Alcotest.test_case "compatibility rejects missing key" `Quick
      test_compatibility_missing_key;
    Alcotest.test_case "compatibility rejects unsupported schema" `Quick
      test_compatibility_unsupported_schema;
    Alcotest.test_case "restore requires operator proof" `Quick
      test_restore_requires_operator_proof;
    Alcotest.test_case "restore roundtrip disables authorization" `Quick
      test_restore_roundtrip_disables_auth;
    Alcotest.test_case "restore fails closed on wrong key material" `Quick
      test_restore_fails_closed_wrong_key_material;
    Alcotest.test_case "compromise disable destroys and requires relink" `Quick
      test_compromise_disable_destroys_and_requires_relink;
    Alcotest.test_case "compromise requires reason" `Quick
      test_compromise_requires_reason;
    Alcotest.test_case "denial never leaks tokens" `Quick
      test_denial_never_leaks_tokens;
  ]

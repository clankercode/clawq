(** Tests for fail-closed mutable GitHub user-token vault CRUD (P21.M2.E4.T002).
*)

module V = Github_user_token_vault
module S = Github_user_token_store
module MK = Github_user_token_master_key

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-vault-test-master" ()

let other_aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-vault-OTHER-master" ()

let sample_tokens =
  {
    S.access_token = "ghu_access_VAULT_PLAINTEXT_never_store";
    refresh_token = Some "ghr_refresh_VAULT_PLAINTEXT_never_store";
  }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let account ?(principal_id = "prin_vault_1") ?(github_user_id = 9001L)
    ?(app_id = 42) ?(host = V.default_host) () =
  assert_ok (V.make_account_key ~principal_id ~github_user_id ~app_id ~host ())

let make_keys ?(key_id = "mk-vault-1") ?(key_version = 1) ?(aes = aes_key) () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key:aes ())

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  V.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_720_000_000.0

let create_sample ~db ?(keys = make_keys ()) ?(account = account ())
    ?(tokens = sample_tokens) ?(id = "ghvault_test_1") () =
  match
    V.create ~db ~keys ~id ~now:fixed_now ~account ~tokens
      ~scopes:[ "repo"; "read:user" ] ~expires_at:"2026-08-01T00:00:00Z" ()
  with
  | Ok r -> r
  | Error d -> Alcotest.fail ("create: " ^ V.string_of_denial d)

(* -------------------------------------------------------------------------- *)
(* Happy path                                                                 *)
(* -------------------------------------------------------------------------- *)

let test_create_read_roundtrip () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ = create_sample ~db ~keys ~account:acct () in
  Alcotest.(check string) "id" "ghvault_test_1" rec_.id;
  Alcotest.(check string) "key_id" "mk-vault-1" rec_.key_id;
  Alcotest.(check int) "key_version" 1 rec_.key_version;
  Alcotest.(check int) "generation" 1 rec_.generation;
  Alcotest.(check int) "record_version" V.schema_version rec_.record_version;
  match V.read ~db ~keys ~id:rec_.id () with
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok opened ->
      Alcotest.(check string)
        "access" sample_tokens.access_token opened.tokens.access_token;
      Alcotest.(check (option string))
        "refresh" sample_tokens.refresh_token opened.tokens.refresh_token;
      Alcotest.(check string)
        "principal" acct.principal_id opened.record.account.principal_id;
      Alcotest.(check int) "generation" 1 opened.record.generation

let test_no_plaintext_in_db_or_meta_json () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_sample ~db ~keys () in
  (match
     V.row_contains_plaintext ~db ~id:rec_.id
       ~plaintext:sample_tokens.access_token
   with
  | Ok false -> ()
  | Ok true -> Alcotest.fail "access plaintext found in row"
  | Error d -> Alcotest.fail (V.string_of_denial d));
  (match
     V.row_contains_plaintext ~db ~id:rec_.id
       ~plaintext:(Option.get sample_tokens.refresh_token)
   with
  | Ok false -> ()
  | Ok true -> Alcotest.fail "refresh plaintext found in row"
  | Error d -> Alcotest.fail (V.string_of_denial d));
  let json = V.record_to_json rec_ in
  Alcotest.(check bool)
    "meta json no access" false
    (V.json_contains_plaintext ~json ~plaintext:sample_tokens.access_token);
  Alcotest.(check bool)
    "meta json no refresh" false
    (V.json_contains_plaintext ~json
       ~plaintext:(Option.get sample_tokens.refresh_token));
  Alcotest.(check bool)
    "meta json no aes key" false
    (V.json_contains_plaintext ~json ~plaintext:aes_key)

let test_replace_advances_generation_records_key () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_sample ~db ~keys () in
  let new_tokens =
    {
      S.access_token = "ghu_rotated_access_PLAINTEXT";
      refresh_token = Some "ghr_rotated_refresh_PLAINTEXT";
    }
  in
  match
    V.replace ~db ~keys ~now:(fixed_now +. 10.) ~id:rec_.id
      ~expected_generation:1 ~tokens:new_tokens ~scopes:[ "repo" ]
      ~expires_at:"2026-09-01T00:00:00Z" ()
  with
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok updated -> (
      Alcotest.(check int) "generation advanced" 2 updated.generation;
      Alcotest.(check string)
        "key_id retained/active" "mk-vault-1" updated.key_id;
      Alcotest.(check int) "key_version" 1 updated.key_version;
      match V.read ~db ~keys ~id:rec_.id () with
      | Error d -> Alcotest.fail (V.string_of_denial d)
      | Ok opened -> (
          Alcotest.(check string)
            "new access" new_tokens.access_token opened.tokens.access_token;
          Alcotest.(check int) "gen" 2 opened.record.generation;
          (match
             V.row_contains_plaintext ~db ~id:rec_.id
               ~plaintext:new_tokens.access_token
           with
          | Ok false -> ()
          | Ok true -> Alcotest.fail "rotated plaintext in db"
          | Error d -> Alcotest.fail (V.string_of_denial d));
          (* Stale CAS must fail without rolling generation back. *)
          match
            V.replace ~db ~keys ~id:rec_.id ~expected_generation:1
              ~tokens:sample_tokens ~scopes:[] ~expires_at:"t" ()
          with
          | Error (V.Generation_conflict { expected = 1; actual = 2 }) -> ()
          | Error d ->
              Alcotest.fail
                ("expected generation_conflict, got " ^ V.string_of_denial d)
          | Ok _ -> Alcotest.fail "stale replace should fail"))

let test_destroy () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_sample ~db ~keys () in
  (match V.destroy ~db ~id:rec_.id with
  | Ok () -> ()
  | Error d -> Alcotest.fail (V.string_of_denial d));
  (match V.read ~db ~keys ~id:rec_.id () with
  | Error V.Not_found -> ()
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok _ -> Alcotest.fail "read after destroy should Not_found");
  match V.destroy ~db ~id:rec_.id with
  | Error V.Not_found -> ()
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok () -> Alcotest.fail "double destroy should Not_found"

let test_read_by_account () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct =
    account ~principal_id:"prin_acct" ~github_user_id:55L ~app_id:7 ()
  in
  let rec_ = create_sample ~db ~keys ~account:acct ~id:"ghvault_acct" () in
  match V.read_by_account ~db ~keys ~account:acct () with
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok opened ->
      Alcotest.(check string) "id" rec_.id opened.record.id;
      Alcotest.(check string)
        "access" sample_tokens.access_token opened.tokens.access_token

(* -------------------------------------------------------------------------- *)
(* Typed denials                                                              *)
(* -------------------------------------------------------------------------- *)

let test_master_key_not_ready_create () =
  with_db @@ fun db ->
  let readiness =
    MK.NotReady { reasons = [ MK.Missing; MK.No_active ]; observed = [] }
  in
  let keys =
    V.make_static_key_provider ~readiness
      ~keys:[ { V.key_id = "mk-1"; key_version = 1; aes_key } ]
      ()
  in
  match
    V.create ~db ~keys ~account:(account ()) ~tokens:sample_tokens ~scopes:[]
      ~expires_at:"2026-08-01T00:00:00Z" ()
  with
  | Error (V.Master_key_not_ready _) -> ()
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok _ -> Alcotest.fail "create should refuse when NotReady"

let test_missing_key_on_read () =
  with_db @@ fun db ->
  let keys = make_keys ~key_id:"mk-present" () in
  let rec_ = create_sample ~db ~keys ~id:"ghvault_miss" () in
  let empty_keys =
    V.make_static_key_provider
      ~readiness:
        (MK.Ready
           {
             active =
               {
                 key_id = "mk-other";
                 key_version = 1;
                 role = MK.Active;
                 source_kind = MK.Env { var_name = "X" };
               };
             available = [];
           })
      ~keys:[] ()
  in
  match V.read ~db ~keys:empty_keys ~id:rec_.id () with
  | Error (V.Missing_key { key_id = "mk-present" }) -> ()
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok _ -> Alcotest.fail "missing key should deny"

let test_wrong_key_on_read () =
  with_db @@ fun db ->
  let keys = make_keys ~key_id:"mk-same" ~aes:aes_key () in
  let rec_ = create_sample ~db ~keys ~id:"ghvault_wrong" () in
  (* Same key_id, different AES material → Wrong_key (fingerprint mismatch). *)
  let bad_keys =
    assert_ok
      (V.make_single_key_provider ~key_id:"mk-same" ~key_version:1
         ~aes_key:other_aes_key ())
  in
  match V.read ~db ~keys:bad_keys ~id:rec_.id () with
  | Error V.Wrong_key -> ()
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok { tokens; _ } ->
      ignore tokens;
      Alcotest.fail "wrong key must not return tokens"

let test_corrupt_envelope () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_sample ~db ~keys ~id:"ghvault_corrupt" () in
  let sql = "UPDATE github_user_token_vault SET ciphertext = ? WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT "$VAULT_AAD_V1:not-valid!!!"));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT rec_.id));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  match V.read ~db ~keys ~id:rec_.id () with
  | Error V.Corrupt_envelope -> ()
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok _ -> Alcotest.fail "corrupt envelope should deny"

let test_unsupported_version () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_sample ~db ~keys ~id:"ghvault_ver" () in
  let sql =
    "UPDATE github_user_token_vault SET record_version = 99 WHERE id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT rec_.id));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  match V.read ~db ~keys ~id:rec_.id () with
  | Error (V.Unsupported_version { version = 99 }) -> ()
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok _ -> Alcotest.fail "unsupported version should deny"

let test_swapped_record () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let a1 = account ~principal_id:"prin_a" ~github_user_id:1L ~app_id:1 () in
  let a2 = account ~principal_id:"prin_b" ~github_user_id:2L ~app_id:1 () in
  let r1 = create_sample ~db ~keys ~account:a1 ~id:"ghvault_a" () in
  let r2 =
    create_sample ~db ~keys ~account:a2 ~id:"ghvault_b"
      ~tokens:
        { access_token = "ghu_other_user_token_PLAIN"; refresh_token = None }
      ()
  in
  (* Swap ciphertext between rows while keeping identity columns — AAD must
     fail closed as Swapped_record (fingerprint matches active key). *)
  let get_ct id =
    let sql = "SELECT ciphertext FROM github_user_token_vault WHERE id = ?" in
    let stmt = Sqlite3.prepare db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
    let ct =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.TEXT s -> s
          | _ -> Alcotest.fail "ct")
      | _ -> Alcotest.fail "missing row"
    in
    ignore (Sqlite3.finalize stmt);
    ct
  in
  let ct1 = get_ct r1.id in
  let ct2 = get_ct r2.id in
  let put_ct id ct =
    let sql =
      "UPDATE github_user_token_vault SET ciphertext = ? WHERE id = ?"
    in
    let stmt = Sqlite3.prepare db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT ct));
    ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT id));
    ignore (Sqlite3.step stmt);
    ignore (Sqlite3.finalize stmt)
  in
  put_ct r1.id ct2;
  put_ct r2.id ct1;
  (match V.read ~db ~keys ~id:r1.id () with
  | Error V.Swapped_record -> ()
  | Error d -> Alcotest.fail ("r1: " ^ V.string_of_denial d)
  | Ok { tokens; _ } ->
      ignore tokens;
      Alcotest.fail "swapped r1 must not yield tokens");
  match V.read ~db ~keys ~id:r2.id () with
  | Error V.Swapped_record -> ()
  | Error d -> Alcotest.fail ("r2: " ^ V.string_of_denial d)
  | Ok _ -> Alcotest.fail "swapped r2 must not yield tokens"

let test_account_mismatch () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account ~principal_id:"prin_real" ~github_user_id:10L () in
  let rec_ = create_sample ~db ~keys ~account:acct ~id:"ghvault_mm" () in
  let expected = account ~principal_id:"prin_other" ~github_user_id:10L () in
  match V.read ~db ~keys ~expected ~id:rec_.id () with
  | Error (V.Account_mismatch { expected = e; found = f }) ->
      Alcotest.(check string) "expected prin" "prin_other" e.principal_id;
      Alcotest.(check string) "found prin" "prin_real" f.principal_id
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok _ -> Alcotest.fail "account mismatch should deny before tokens"

let test_already_exists () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  ignore (create_sample ~db ~keys ~account:acct ~id:"ghvault_dup" ());
  match
    V.create ~db ~keys ~id:"ghvault_dup2" ~account:acct ~tokens:sample_tokens
      ~scopes:[] ~expires_at:"2026-08-01T00:00:00Z" ()
  with
  | Error V.Already_exists -> ()
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok _ -> Alcotest.fail "duplicate account should fail"

let test_crypto_failure_bad_key_length () =
  with_db @@ fun db ->
  let readiness =
    MK.Ready
      {
        active =
          {
            key_id = "mk-short";
            key_version = 1;
            role = MK.Active;
            source_kind = MK.Env { var_name = "X" };
          };
        available = [];
      }
  in
  let keys =
    V.make_static_key_provider ~readiness
      ~keys:
        [ { V.key_id = "mk-short"; key_version = 1; aes_key = "too-short" } ]
      ()
  in
  match
    V.create ~db ~keys ~account:(account ()) ~tokens:sample_tokens ~scopes:[]
      ~expires_at:"2026-08-01T00:00:00Z" ()
  with
  | Error V.Crypto_failure -> ()
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok _ -> Alcotest.fail "short aes key should crypto-fail"

let test_denial_never_embeds_token () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_sample ~db ~keys ~id:"ghvault_redact" () in
  let bad_keys =
    assert_ok
      (V.make_single_key_provider ~key_id:"mk-vault-1" ~key_version:1
         ~aes_key:other_aes_key ())
  in
  match V.read ~db ~keys:bad_keys ~id:rec_.id () with
  | Ok _ -> Alcotest.fail "expected denial"
  | Error d ->
      Alcotest.(check bool)
        "denial no access token" false
        (V.denial_exposes_token ~denial:d ~plaintext:sample_tokens.access_token);
      Alcotest.(check bool)
        "denial no aes" false
        (V.denial_exposes_token ~denial:d ~plaintext:aes_key)

let test_replace_requires_ready () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_sample ~db ~keys ~id:"ghvault_nr" () in
  let not_ready =
    V.make_static_key_provider
      ~readiness:(MK.NotReady { reasons = [ MK.Inaccessible ]; observed = [] })
      ~keys:[ { V.key_id = "mk-vault-1"; key_version = 1; aes_key } ]
      ()
  in
  match
    V.replace ~db ~keys:not_ready ~id:rec_.id ~expected_generation:1
      ~tokens:sample_tokens ~scopes:[] ~expires_at:"t" ()
  with
  | Error (V.Master_key_not_ready _) -> ()
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok _ -> Alcotest.fail "replace should require Ready"

let suite =
  [
    Alcotest.test_case "create/read roundtrip records key_id + generation"
      `Quick test_create_read_roundtrip;
    Alcotest.test_case "no plaintext in DB or metadata JSON" `Quick
      test_no_plaintext_in_db_or_meta_json;
    Alcotest.test_case "replace advances generation under CAS" `Quick
      test_replace_advances_generation_records_key;
    Alcotest.test_case "destroy removes sealed row" `Quick test_destroy;
    Alcotest.test_case "read_by_account" `Quick test_read_by_account;
    Alcotest.test_case "create denies when master key NotReady" `Quick
      test_master_key_not_ready_create;
    Alcotest.test_case "read denies Missing_key" `Quick test_missing_key_on_read;
    Alcotest.test_case "read denies Wrong_key" `Quick test_wrong_key_on_read;
    Alcotest.test_case "read denies Corrupt_envelope" `Quick
      test_corrupt_envelope;
    Alcotest.test_case "read denies Unsupported_version" `Quick
      test_unsupported_version;
    Alcotest.test_case "read denies Swapped_record" `Quick test_swapped_record;
    Alcotest.test_case "read denies Account_mismatch" `Quick
      test_account_mismatch;
    Alcotest.test_case "create denies Already_exists" `Quick test_already_exists;
    Alcotest.test_case "create denies Crypto_failure for short key" `Quick
      test_crypto_failure_bad_key_length;
    Alcotest.test_case "denials never embed token or key material" `Quick
      test_denial_never_embeds_token;
    Alcotest.test_case "replace denies when master key NotReady" `Quick
      test_replace_requires_ready;
  ]

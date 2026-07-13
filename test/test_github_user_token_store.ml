(* Tests for versioned encrypted GitHub user-token records (P21.M2.E4.T001). *)

module S = Github_user_token_store

let () = Secret_store.test_iterations_override := Some 1

let fixed_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-user-token-test-key" ()

let sample_tokens =
  {
    S.access_token = "ghu_access_plaintext_DO_NOT_EXPORT";
    refresh_token = Some "ghr_refresh_plaintext_DO_NOT_EXPORT";
  }

let seal_mem ?(tokens = sample_tokens) ?(principal_id = "prin_test_1")
    ?(github_user_id = 4242L) ?(scopes = [ "repo"; "read:user" ])
    ?(expires_at = "2026-07-14T12:00:00Z") ?(app_id = 99) () =
  let store, table = S.make_in_memory_secret_store () in
  let record =
    match
      S.seal ~store ~principal_id ~github_user_id ~tokens ~scopes ~expires_at
        ~app_id ()
    with
    | Ok r -> r
    | Error e -> Alcotest.fail ("seal failed: " ^ e)
  in
  (store, table, record)

let test_seal_and_resolve_in_memory () =
  let store, table, record = seal_mem () in
  Alcotest.(check int) "schema version" S.schema_version record.version;
  Alcotest.(check string) "principal" "prin_test_1" record.principal_id;
  Alcotest.(check int64) "github user" 4242L record.github_user_id;
  Alcotest.(check int) "app_id" 99 record.app_id;
  Alcotest.(check (list string)) "scopes" [ "repo"; "read:user" ] record.scopes;
  Alcotest.(check bool)
    "access handle is not plaintext" true
    (record.access_token_handle <> sample_tokens.access_token);
  (match record.refresh_token_handle with
  | None -> Alcotest.fail "expected refresh handle"
  | Some h ->
      Alcotest.(check bool)
        "refresh handle is not plaintext" true
        (h <> Option.get sample_tokens.refresh_token));
  Alcotest.(check int) "two secrets stored" 2 (Hashtbl.length table);
  match S.resolve_tokens ~store record with
  | Error e -> Alcotest.fail ("resolve failed: " ^ e)
  | Ok tokens ->
      Alcotest.(check string)
        "access roundtrip" sample_tokens.access_token tokens.access_token;
      Alcotest.(check (option string))
        "refresh roundtrip" sample_tokens.refresh_token tokens.refresh_token

let test_json_export_has_no_plaintext () =
  let _store, _table, record = seal_mem () in
  let json = S.to_json record in
  let s = S.export_json_string record in
  Alcotest.(check bool)
    "export lacks access plaintext" false
    (S.json_contains_plaintext ~json ~plaintext:sample_tokens.access_token);
  Alcotest.(check bool)
    "export lacks refresh plaintext" false
    (S.json_contains_plaintext ~json
       ~plaintext:(Option.get sample_tokens.refresh_token));
  Alcotest.(check bool)
    "serialized string lacks access plaintext" false
    (String_util.contains s sample_tokens.access_token);
  Alcotest.(check bool)
    "serialized string lacks refresh plaintext" false
    (String_util.contains s (Option.get sample_tokens.refresh_token));
  (* Required fields present as handles / metadata. *)
  match json with
  | `Assoc fields ->
      let keys = List.map fst fields in
      List.iter
        (fun k -> Alcotest.(check bool) ("has " ^ k) true (List.mem k keys))
        [
          "version";
          "principal_id";
          "github_user_id";
          "access_token_handle";
          "refresh_token_handle";
          "scopes";
          "expires_at";
          "app_id";
        ]
  | _ -> Alcotest.fail "expected JSON object"

let test_json_roundtrip () =
  let _store, _table, record = seal_mem () in
  match S.of_json (S.to_json record) with
  | Error e -> Alcotest.fail ("of_json failed: " ^ e)
  | Ok again ->
      Alcotest.(check string) "principal" record.principal_id again.principal_id;
      Alcotest.(check int64)
        "github_user_id" record.github_user_id again.github_user_id;
      Alcotest.(check string)
        "access handle" record.access_token_handle again.access_token_handle;
      Alcotest.(check (option string))
        "refresh handle" record.refresh_token_handle again.refresh_token_handle;
      Alcotest.(check (list string)) "scopes" record.scopes again.scopes;
      Alcotest.(check string) "expires" record.expires_at again.expires_at;
      Alcotest.(check int) "app_id" record.app_id again.app_id;
      Alcotest.(check int) "version" record.version again.version

let test_aad_encrypt_roundtrip () =
  match
    S.seal_encrypted ~key:fixed_key ~principal_id:"prin_aad" ~github_user_id:7L
      ~tokens:sample_tokens ~scopes:[ "user" ]
      ~expires_at:"2026-08-01T00:00:00Z" ~app_id:55 ()
  with
  | Error e -> Alcotest.fail ("seal_encrypted failed: " ^ e)
  | Ok record -> (
      Alcotest.(check bool)
        "access is AAD handle" true
        (S.is_aad_handle record.access_token_handle);
      (match record.refresh_token_handle with
      | Some h ->
          Alcotest.(check bool) "refresh is AAD handle" true (S.is_aad_handle h)
      | None -> Alcotest.fail "expected refresh AAD handle");
      Alcotest.(check bool)
        "handle is not plaintext" true
        (record.access_token_handle <> sample_tokens.access_token);
      let json = S.to_json record in
      Alcotest.(check bool)
        "AAD export has no access plaintext" false
        (S.json_contains_plaintext ~json ~plaintext:sample_tokens.access_token);
      Alcotest.(check bool)
        "AAD export has no refresh plaintext" false
        (S.json_contains_plaintext ~json
           ~plaintext:(Option.get sample_tokens.refresh_token));
      match S.resolve_encrypted ~key:fixed_key record with
      | Error e -> Alcotest.fail ("resolve_encrypted failed: " ^ e)
      | Ok tokens ->
          Alcotest.(check string)
            "access" sample_tokens.access_token tokens.access_token;
          Alcotest.(check (option string))
            "refresh" sample_tokens.refresh_token tokens.refresh_token)

let test_aad_binding_mismatch_fails () =
  match
    S.seal_encrypted ~key:fixed_key ~principal_id:"prin_bound"
      ~github_user_id:100L ~tokens:sample_tokens ~scopes:[]
      ~expires_at:"2026-08-01T00:00:00Z" ~app_id:1 ()
  with
  | Error e -> Alcotest.fail ("seal_encrypted failed: " ^ e)
  | Ok record -> (
      (* Swap identity fields while keeping ciphertext handles — AAD must fail. *)
      let tampered =
        {
          record with
          principal_id = "prin_other";
          github_user_id = 999L;
          app_id = 2;
        }
      in
      match S.resolve_encrypted ~key:fixed_key tampered with
      | Ok _ -> Alcotest.fail "AAD mismatch should fail closed"
      | Error _ -> (
          (* Wrong key also fails. *)
          let wrong =
            Secret_store.derive_key ~iterations:1 ~passphrase:"other" ()
          in
          match S.resolve_encrypted ~key:wrong record with
          | Ok _ -> Alcotest.fail "wrong key should fail"
          | Error _ -> ()))

let test_unique_nonce_per_encrypt () =
  let aad =
    S.aad_of ~principal_id:"p" ~github_user_id:1L ~app_id:1
      ~version:S.schema_version
  in
  let h1 =
    S.encrypt_with_aad ~key:fixed_key ~aad ~plaintext:"same-token-value"
  in
  let h2 =
    S.encrypt_with_aad ~key:fixed_key ~aad ~plaintext:"same-token-value"
  in
  Alcotest.(check bool)
    "unique nonce yields different ciphertext" true (h1 <> h2);
  match
    ( S.decrypt_with_aad ~key:fixed_key ~aad ~handle:h1,
      S.decrypt_with_aad ~key:fixed_key ~aad ~handle:h2 )
  with
  | Ok a, Ok b ->
      Alcotest.(check string) "h1" "same-token-value" a;
      Alcotest.(check string) "h2" "same-token-value" b
  | Error e, _ | _, Error e -> Alcotest.fail e

let test_secret_store_key_backend () =
  let store = S.secret_backend_of_secret_store_key ~key:fixed_key in
  match
    S.seal ~store ~principal_id:"prin_enc" ~github_user_id:3L
      ~tokens:sample_tokens ~scopes:[ "repo" ]
      ~expires_at:"2026-09-01T00:00:00Z" ~app_id:8 ()
  with
  | Error e -> Alcotest.fail e
  | Ok record -> (
      Alcotest.(check bool)
        "handle uses $ENC:" true
        (Secret_store.is_encrypted record.access_token_handle);
      Alcotest.(check bool)
        "not plaintext" true
        (record.access_token_handle <> sample_tokens.access_token);
      let json = S.to_json record in
      Alcotest.(check bool)
        "json export has no plaintext" false
        (S.json_contains_plaintext ~json ~plaintext:sample_tokens.access_token);
      match S.resolve_tokens ~store record with
      | Error e -> Alcotest.fail e
      | Ok tokens ->
          Alcotest.(check string)
            "access" sample_tokens.access_token tokens.access_token)

let test_validation_rejects_empty () =
  let store, _ = S.make_in_memory_secret_store () in
  (match
     S.seal ~store ~principal_id:"" ~github_user_id:1L ~tokens:sample_tokens
       ~scopes:[] ~expires_at:"t" ~app_id:1 ()
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "empty principal should fail");
  (match
     S.seal ~store ~principal_id:"p" ~github_user_id:1L
       ~tokens:{ access_token = ""; refresh_token = None }
       ~scopes:[] ~expires_at:"t" ~app_id:1 ()
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "empty access token should fail");
  match
    S.make ~principal_id:"p" ~github_user_id:0L ~access_token_handle:"h"
      ~scopes:[] ~expires_at:"t" ~app_id:1 ()
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "non-positive github_user_id should fail"

let test_delete_tokens () =
  let store, table, record = seal_mem () in
  Alcotest.(check int) "before delete" 2 (Hashtbl.length table);
  (match S.delete_tokens ~store record with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  Alcotest.(check int) "after delete" 0 (Hashtbl.length table);
  match S.resolve_tokens ~store record with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "resolve after delete should fail"

let suite =
  [
    Alcotest.test_case "seal/resolve via in-memory secret store" `Quick
      test_seal_and_resolve_in_memory;
    Alcotest.test_case "JSON export contains no plaintext tokens" `Quick
      test_json_export_has_no_plaintext;
    Alcotest.test_case "JSON roundtrip preserves handles and metadata" `Quick
      test_json_roundtrip;
    Alcotest.test_case "AAD encrypt/decrypt roundtrip" `Quick
      test_aad_encrypt_roundtrip;
    Alcotest.test_case "AAD binding mismatch and wrong key fail closed" `Quick
      test_aad_binding_mismatch_fails;
    Alcotest.test_case "encrypt uses unique nonce per call" `Quick
      test_unique_nonce_per_encrypt;
    Alcotest.test_case "Secret_store key backend seals encrypted handles" `Quick
      test_secret_store_key_backend;
    Alcotest.test_case "validation rejects empty/invalid fields" `Quick
      test_validation_rejects_empty;
    Alcotest.test_case "delete_tokens clears in-memory material" `Quick
      test_delete_tokens;
  ]

(* Global override: use minimal iteration count for faster tests. This affects
   the entire secret_store test suite and is intentional. *)
let () = Secret_store.test_iterations_override := Some 1

let test_derive_key () =
  let key =
    Secret_store.derive_key ~iterations:1 ~passphrase:"test-passphrase" ()
  in
  Alcotest.(check int) "key length is 32 bytes" 32 (String.length key)

let test_derive_key_deterministic () =
  let key1 =
    Secret_store.derive_key ~iterations:1 ~passphrase:"same-phrase" ()
  in
  let key2 =
    Secret_store.derive_key ~iterations:1 ~passphrase:"same-phrase" ()
  in
  Alcotest.(check string) "same passphrase same key" key1 key2

let test_derive_key_different () =
  let key1 = Secret_store.derive_key ~iterations:1 ~passphrase:"phrase-a" () in
  let key2 = Secret_store.derive_key ~iterations:1 ~passphrase:"phrase-b" () in
  Alcotest.(check bool)
    "different passphrases different keys" true (key1 <> key2)

let test_encrypt_decrypt_roundtrip () =
  let key =
    Secret_store.derive_key ~iterations:1 ~passphrase:"roundtrip-test" ()
  in
  let plaintext = "sk-my-secret-api-key-12345" in
  let encrypted = Secret_store.encrypt ~key plaintext in
  (* Encrypted should be base64, not equal to plaintext *)
  Alcotest.(check bool) "encrypted differs" true (encrypted <> plaintext);
  match Secret_store.decrypt ~key encrypted with
  | Ok decrypted ->
      Alcotest.(check string) "roundtrip matches" plaintext decrypted
  | Error msg -> Alcotest.fail (Printf.sprintf "decrypt failed: %s" msg)

let test_decrypt_wrong_key () =
  let key1 =
    Secret_store.derive_key ~iterations:1 ~passphrase:"correct-key" ()
  in
  let key2 = Secret_store.derive_key ~iterations:1 ~passphrase:"wrong-key" () in
  let encrypted = Secret_store.encrypt ~key:key1 "secret-data" in
  match Secret_store.decrypt ~key:key2 encrypted with
  | Error _ -> () (* Expected: wrong key should fail *)
  | Ok _ -> Alcotest.fail "decrypt should fail with wrong key"

let test_decrypt_corrupted () =
  let key = Secret_store.derive_key ~iterations:1 ~passphrase:"test" () in
  match Secret_store.decrypt ~key "not-valid-base64!!!" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "should fail on corrupted data"

let test_decrypt_too_short () =
  let key = Secret_store.derive_key ~iterations:1 ~passphrase:"test" () in
  let encoded = Base64.encode_exn "short" in
  match Secret_store.decrypt ~key encoded with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "should fail on too-short data"

let test_is_encrypted () =
  Alcotest.(check bool)
    "enc prefix" true
    (Secret_store.is_encrypted "$ENC:abc123");
  Alcotest.(check bool) "env var" false (Secret_store.is_encrypted "$MY_KEY");
  Alcotest.(check bool) "plain" false (Secret_store.is_encrypted "plain-key");
  Alcotest.(check bool) "empty" false (Secret_store.is_encrypted "")

let test_encrypt_secret_prefix () =
  let key = Secret_store.derive_key ~iterations:1 ~passphrase:"test" () in
  let result = Secret_store.encrypt_secret ~key "my-api-key" in
  Alcotest.(check bool)
    "has ENC prefix" true
    (String.length result > 5 && String.sub result 0 5 = "$ENC:");
  Alcotest.(check bool) "is_encrypted" true (Secret_store.is_encrypted result)

let test_decrypt_secret_prefix () =
  let key = Secret_store.derive_key ~iterations:1 ~passphrase:"test" () in
  let encrypted = Secret_store.encrypt_secret ~key "my-api-key" in
  match Secret_store.decrypt_secret ~key encrypted with
  | Ok plaintext ->
      Alcotest.(check string) "decrypted matches" "my-api-key" plaintext
  | Error msg -> Alcotest.fail (Printf.sprintf "decrypt_secret failed: %s" msg)

let test_decrypt_secret_passthrough () =
  let key = Secret_store.derive_key ~iterations:1 ~passphrase:"test" () in
  match Secret_store.decrypt_secret ~key "not-encrypted" with
  | Ok result -> Alcotest.(check string) "passthrough" "not-encrypted" result
  | Error msg -> Alcotest.fail (Printf.sprintf "passthrough failed: %s" msg)

let with_env name value_opt f =
  let old = Sys.getenv_opt name in
  Fun.protect
    (fun () ->
      Unix.putenv name (match value_opt with Some value -> value | None -> "");
      f ())
    ~finally:(fun () ->
      Unix.putenv name (match old with Some value -> value | None -> ""))

let test_resolve_secret_plaintext_passthrough () =
  Alcotest.(check string)
    "plaintext passthrough" "plain-value"
    (Secret_store.resolve_secret ~encrypt_secrets:true "plain-value")

let test_resolve_secret_env_var () =
  with_env "CLAWQ_TEST_SECRET_ENV" (Some "resolved-value") (fun () ->
      Alcotest.(check string)
        "env var resolved" "resolved-value"
        (Secret_store.resolve_secret ~encrypt_secrets:true
           "$CLAWQ_TEST_SECRET_ENV"))

let test_resolve_secret_encrypted_passthrough_without_master_key () =
  let key = Secret_store.derive_key ~iterations:1 ~passphrase:"test" () in
  let encrypted = Secret_store.encrypt_secret ~key "my-api-key" in
  with_env "CLAWQ_MASTER_KEY" None (fun () ->
      Alcotest.(check string)
        "encrypted value preserved when master key missing" encrypted
        (Secret_store.resolve_secret ~encrypt_secrets:true encrypted))

let test_resolve_secret_encrypted_decrypts_with_master_key () =
  let passphrase = "master-secret-for-resolve" in
  let key = Secret_store.derive_key ~iterations:1 ~passphrase () in
  let encrypted = Secret_store.encrypt_secret ~key "resolved-secret" in
  with_env "CLAWQ_MASTER_KEY" (Some passphrase) (fun () ->
      Alcotest.(check string)
        "encrypted value decrypts" "resolved-secret"
        (Secret_store.resolve_secret ~encrypt_secrets:true encrypted))

let test_resolve_secret_encrypted_decrypt_failure_passthrough () =
  let correct =
    Secret_store.derive_key ~iterations:1 ~passphrase:"correct-master" ()
  in
  let encrypted = Secret_store.encrypt_secret ~key:correct "resolved-secret" in
  with_env "CLAWQ_MASTER_KEY" (Some "wrong-master") (fun () ->
      Alcotest.(check string)
        "encrypted value preserved when decrypt fails" encrypted
        (Secret_store.resolve_secret ~encrypt_secrets:true encrypted))

let test_encrypt_config_secrets () =
  let key = Secret_store.derive_key ~iterations:1 ~passphrase:"test" () in
  let json =
    Yojson.Safe.from_string
      {|{
        "providers": {
          "openai": {"api_key": "sk-real-key", "base_url": "https://api.openai.com"},
          "groq": {"api_key": "$GROQ_KEY", "base_url": "https://api.groq.com"}
        }
      }|}
  in
  match Secret_store.encrypt_config_secrets ~key json with
  | Error msg -> Alcotest.fail msg
  | Ok new_json -> (
      let open Yojson.Safe.Util in
      let openai_key =
        new_json |> member "providers" |> member "openai" |> member "api_key"
        |> to_string
      in
      let groq_key =
        new_json |> member "providers" |> member "groq" |> member "api_key"
        |> to_string
      in
      (* openai key should be encrypted *)
      Alcotest.(check bool)
        "openai encrypted" true
        (Secret_store.is_encrypted openai_key);
      (* groq key starts with $ so should NOT be encrypted (it's an env var ref) *)
      Alcotest.(check string) "groq unchanged" "$GROQ_KEY" groq_key;
      (* Verify the encrypted key can be decrypted *)
      match Secret_store.decrypt_secret ~key openai_key with
      | Ok decrypted ->
          Alcotest.(check string) "round-trip" "sk-real-key" decrypted
      | Error msg -> Alcotest.fail msg)

let test_encrypt_config_secrets_preserves_non_provider_fields () =
  let key = Secret_store.derive_key ~iterations:1 ~passphrase:"test" () in
  let json =
    Yojson.Safe.from_string
      {|{
        "agent": {"model": "gpt-5.3-codex"},
        "providers": {
          "openai": {"api_key": "sk-real-key", "base_url": "https://api.openai.com"}
        }
      }|}
  in
  match Secret_store.encrypt_config_secrets ~key json with
  | Error msg -> Alcotest.fail msg
  | Ok new_json ->
      let open Yojson.Safe.Util in
      let model = new_json |> member "agent" |> member "model" |> to_string in
      Alcotest.(check string) "unrelated fields preserved" "gpt-5.3-codex" model

let test_encrypt_config_secrets_avoids_double_encrypt () =
  let key = Secret_store.derive_key ~iterations:1 ~passphrase:"test" () in
  let existing = Secret_store.encrypt_secret ~key "already-secret" in
  let json =
    `Assoc
      [
        ( "providers",
          `Assoc
            [
              ( "openai",
                `Assoc
                  [
                    ("api_key", `String existing);
                    ("base_url", `String "https://api.openai.com");
                  ] );
            ] );
      ]
  in
  match Secret_store.encrypt_config_secrets ~key json with
  | Error msg -> Alcotest.fail msg
  | Ok new_json ->
      let open Yojson.Safe.Util in
      let after =
        new_json |> member "providers" |> member "openai" |> member "api_key"
        |> to_string
      in
      Alcotest.(check string) "already encrypted key unchanged" existing after

let test_empty_plaintext () =
  let key = Secret_store.derive_key ~iterations:1 ~passphrase:"test" () in
  let encrypted = Secret_store.encrypt ~key "" in
  match Secret_store.decrypt ~key encrypted with
  | Ok decrypted -> Alcotest.(check string) "empty roundtrip" "" decrypted
  | Error msg -> Alcotest.fail (Printf.sprintf "empty decrypt failed: %s" msg)

let test_is_secret_key_bare_token () =
  (* Substring matching: bare "token" key must be treated as secret *)
  let key = Secret_store.derive_key ~iterations:1 ~passphrase:"test" () in
  let json = `Assoc [ ("token", `String "gh-pat-abc123") ] in
  match Secret_store.encrypt_config_secrets ~key json with
  | Error msg -> Alcotest.fail msg
  | Ok new_json ->
      let open Yojson.Safe.Util in
      let v = new_json |> member "token" |> to_string in
      Alcotest.(check bool)
        "bare token key encrypted" true
        (Secret_store.is_encrypted v)

let test_is_secret_key_suffix_token () =
  (* Any key containing "token" is encrypted, e.g. "client_token" *)
  let key = Secret_store.derive_key ~iterations:1 ~passphrase:"test" () in
  let json = `Assoc [ ("client_token", `String "some-client-tok") ] in
  match Secret_store.encrypt_config_secrets ~key json with
  | Error msg -> Alcotest.fail msg
  | Ok new_json ->
      let open Yojson.Safe.Util in
      let v = new_json |> member "client_token" |> to_string in
      Alcotest.(check bool)
        "client_token encrypted" true
        (Secret_store.is_encrypted v)

let test_encrypt_config_codex_oauth_secrets () =
  let key = Secret_store.derive_key ~iterations:1 ~passphrase:"test" () in
  let json =
    Yojson.Safe.from_string
      {|{
        "providers": {
          "openai-codex": {
            "kind": "openai-codex",
            "codex_oauth": {
              "access_token": "plain-access",
              "refresh_token": "plain-refresh",
              "expires_at_ms": 1730000000000
            }
          }
        }
      }|}
  in
  match Secret_store.encrypt_config_secrets ~key json with
  | Error msg -> Alcotest.fail msg
  | Ok new_json ->
      let open Yojson.Safe.Util in
      let access_token =
        new_json |> member "providers" |> member "openai-codex"
        |> member "codex_oauth" |> member "access_token" |> to_string
      in
      let refresh_token =
        new_json |> member "providers" |> member "openai-codex"
        |> member "codex_oauth" |> member "refresh_token" |> to_string
      in
      Alcotest.(check bool)
        "access token encrypted" true
        (Secret_store.is_encrypted access_token);
      Alcotest.(check bool)
        "refresh token encrypted" true
        (Secret_store.is_encrypted refresh_token)

let suite =
  [
    Alcotest.test_case "derive key length" `Quick test_derive_key;
    Alcotest.test_case "derive key deterministic" `Quick
      test_derive_key_deterministic;
    Alcotest.test_case "derive key different" `Quick test_derive_key_different;
    Alcotest.test_case "encrypt decrypt roundtrip" `Quick
      test_encrypt_decrypt_roundtrip;
    Alcotest.test_case "decrypt wrong key" `Quick test_decrypt_wrong_key;
    Alcotest.test_case "decrypt corrupted" `Quick test_decrypt_corrupted;
    Alcotest.test_case "decrypt too short" `Quick test_decrypt_too_short;
    Alcotest.test_case "is_encrypted" `Quick test_is_encrypted;
    Alcotest.test_case "encrypt_secret prefix" `Quick test_encrypt_secret_prefix;
    Alcotest.test_case "decrypt_secret prefix" `Quick test_decrypt_secret_prefix;
    Alcotest.test_case "decrypt_secret passthrough" `Quick
      test_decrypt_secret_passthrough;
    Alcotest.test_case "resolve_secret plaintext passthrough" `Quick
      test_resolve_secret_plaintext_passthrough;
    Alcotest.test_case "resolve_secret env var" `Quick
      test_resolve_secret_env_var;
    Alcotest.test_case "resolve_secret encrypted passthrough without master key"
      `Quick test_resolve_secret_encrypted_passthrough_without_master_key;
    Alcotest.test_case "resolve_secret encrypted decrypts with master key"
      `Quick test_resolve_secret_encrypted_decrypts_with_master_key;
    Alcotest.test_case "resolve_secret encrypted decrypt failure passthrough"
      `Quick test_resolve_secret_encrypted_decrypt_failure_passthrough;
    Alcotest.test_case "encrypt config secrets" `Quick
      test_encrypt_config_secrets;
    Alcotest.test_case "encrypt config secrets preserves non-provider fields"
      `Quick test_encrypt_config_secrets_preserves_non_provider_fields;
    Alcotest.test_case "encrypt config secrets avoids double encrypt" `Quick
      test_encrypt_config_secrets_avoids_double_encrypt;
    Alcotest.test_case "empty plaintext" `Quick test_empty_plaintext;
    Alcotest.test_case "encrypt config codex oauth secrets" `Quick
      test_encrypt_config_codex_oauth_secrets;
    Alcotest.test_case "bare token key is encrypted" `Quick
      test_is_secret_key_bare_token;
    Alcotest.test_case "client_token key is encrypted" `Quick
      test_is_secret_key_suffix_token;
  ]

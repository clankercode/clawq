let test_derive_key () =
  let key = Secret_store.derive_key ~passphrase:"test-passphrase" in
  Alcotest.(check int) "key length is 32 bytes" 32 (String.length key)

let test_derive_key_deterministic () =
  let key1 = Secret_store.derive_key ~passphrase:"same-phrase" in
  let key2 = Secret_store.derive_key ~passphrase:"same-phrase" in
  Alcotest.(check string) "same passphrase same key" key1 key2

let test_derive_key_different () =
  let key1 = Secret_store.derive_key ~passphrase:"phrase-a" in
  let key2 = Secret_store.derive_key ~passphrase:"phrase-b" in
  Alcotest.(check bool)
    "different passphrases different keys" true (key1 <> key2)

let test_encrypt_decrypt_roundtrip () =
  let key = Secret_store.derive_key ~passphrase:"roundtrip-test" in
  let plaintext = "sk-my-secret-api-key-12345" in
  let encrypted = Secret_store.encrypt ~key plaintext in
  (* Encrypted should be base64, not equal to plaintext *)
  Alcotest.(check bool) "encrypted differs" true (encrypted <> plaintext);
  match Secret_store.decrypt ~key encrypted with
  | Ok decrypted ->
      Alcotest.(check string) "roundtrip matches" plaintext decrypted
  | Error msg -> Alcotest.fail (Printf.sprintf "decrypt failed: %s" msg)

let test_decrypt_wrong_key () =
  let key1 = Secret_store.derive_key ~passphrase:"correct-key" in
  let key2 = Secret_store.derive_key ~passphrase:"wrong-key" in
  let encrypted = Secret_store.encrypt ~key:key1 "secret-data" in
  match Secret_store.decrypt ~key:key2 encrypted with
  | Error _ -> () (* Expected: wrong key should fail *)
  | Ok _ -> Alcotest.fail "decrypt should fail with wrong key"

let test_decrypt_corrupted () =
  let key = Secret_store.derive_key ~passphrase:"test" in
  match Secret_store.decrypt ~key "not-valid-base64!!!" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "should fail on corrupted data"

let test_decrypt_too_short () =
  let key = Secret_store.derive_key ~passphrase:"test" in
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
  let key = Secret_store.derive_key ~passphrase:"test" in
  let result = Secret_store.encrypt_secret ~key "my-api-key" in
  Alcotest.(check bool)
    "has ENC prefix" true
    (String.length result > 5 && String.sub result 0 5 = "$ENC:");
  Alcotest.(check bool) "is_encrypted" true (Secret_store.is_encrypted result)

let test_decrypt_secret_prefix () =
  let key = Secret_store.derive_key ~passphrase:"test" in
  let encrypted = Secret_store.encrypt_secret ~key "my-api-key" in
  match Secret_store.decrypt_secret ~key encrypted with
  | Ok plaintext ->
      Alcotest.(check string) "decrypted matches" "my-api-key" plaintext
  | Error msg -> Alcotest.fail (Printf.sprintf "decrypt_secret failed: %s" msg)

let test_decrypt_secret_passthrough () =
  let key = Secret_store.derive_key ~passphrase:"test" in
  match Secret_store.decrypt_secret ~key "not-encrypted" with
  | Ok result -> Alcotest.(check string) "passthrough" "not-encrypted" result
  | Error msg -> Alcotest.fail (Printf.sprintf "passthrough failed: %s" msg)

let test_encrypt_config_secrets () =
  let key = Secret_store.derive_key ~passphrase:"test" in
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

let test_empty_plaintext () =
  let key = Secret_store.derive_key ~passphrase:"test" in
  let encrypted = Secret_store.encrypt ~key "" in
  match Secret_store.decrypt ~key encrypted with
  | Ok decrypted -> Alcotest.(check string) "empty roundtrip" "" decrypted
  | Error msg -> Alcotest.fail (Printf.sprintf "empty decrypt failed: %s" msg)

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
    Alcotest.test_case "encrypt config secrets" `Quick
      test_encrypt_config_secrets;
    Alcotest.test_case "empty plaintext" `Quick test_empty_plaintext;
  ]

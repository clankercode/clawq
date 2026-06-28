open Alcotest

let test_redact_secret () =
  (* Redaction is tested through the lease identity's redacted_value field.
     We create leases with known values and verify the redacted output. *)
  let test_var = "CLAWQ_TEST_REDACT_VAR" in
  Unix.putenv test_var "abc123";
  let handle : Runtime_config.credential_handle =
    {
      id = "test:redact";
      provider = Env_var { name = test_var };
      description = None;
      status = "active";
    }
  in
  let check_redacted secret expected_redacted =
    Unix.putenv test_var secret;
    match Credential_lease.make_lease handle ~header_name:"X" with
    | Error _ -> fail "lease creation failed"
    | Ok lease ->
        check string "redacted" expected_redacted lease.identity.redacted_value
  in
  check_redacted "abc123" "abc***";
  check_redacted "ab" "**";
  check_redacted "a" "*";
  check_redacted "abcdefghijklmnop" "abc*************";
  Unix.putenv test_var ""

let test_resolve_env_var () =
  let test_var = "CLAWQ_TEST_CRED_VAR" in
  let handle : Runtime_config.credential_handle =
    {
      id = "test:env";
      provider = Env_var { name = test_var };
      description = None;
      status = "active";
    }
  in
  (* Test with unset variable *)
  (match Credential_lease.make_lease handle ~header_name:"X" with
  | Ok _ -> fail "should fail for unset variable"
  | Error (Credential_lease.Env_var_unset name) ->
      check string "var name" test_var name
  | Error _ -> fail "wrong error type");
  (* Test with set variable *)
  Unix.putenv test_var "test-secret-value";
  (match Credential_lease.make_lease handle ~header_name:"X" with
  | Ok lease ->
      (* Use continuation-based API - raw value only in callback scope *)
      let result = ref "" in
      Credential_lease.apply_headers lease (fun headers ->
          let _, value = List.hd headers in
          result := value);
      check string "env value" "test-secret-value" !result
  | Error _ -> fail "should succeed for set variable");
  Unix.putenv test_var ""

let test_make_lease_from_env () =
  let test_var = "CLAWQ_TEST_CRED_VAR2" in
  Unix.putenv test_var "sk-abc123456789";
  let handle : Runtime_config.credential_handle =
    {
      id = "test:env";
      provider = Env_var { name = test_var };
      description = Some "Test API key";
      status = "active";
    }
  in
  (match Credential_lease.make_lease handle ~header_name:"Authorization" with
  | Error e ->
      failf "lease creation failed: %s"
        (Credential_lease.resolution_error_to_string e)
  | Ok lease ->
      check string "handle_id" "test:env" lease.identity.handle_id;
      check string "provider_type" "env_var" lease.identity.provider_type;
      check string "description" "Test API key" lease.identity.description;
      check string "redacted" "sk-************" lease.identity.redacted_value;
      (* Use continuation-based API *)
      let header_count = ref 0 in
      let header_name = ref "" in
      let header_value = ref "" in
      Credential_lease.apply_headers lease (fun headers ->
          header_count := List.length headers;
          let name, value = List.hd headers in
          header_name := name;
          header_value := value);
      check int "header count" 1 !header_count;
      check string "header name" "Authorization" !header_name;
      check string "header value" "sk-abc123456789" !header_value);
  Unix.putenv test_var ""

let test_make_env_lease () =
  let test_var = "CLAWQ_TEST_CRED_VAR3" in
  Unix.putenv test_var "my-secret-token";
  let handle : Runtime_config.credential_handle =
    {
      id = "test:env_lease";
      provider = Env_var { name = test_var };
      description = Some "Test token";
      status = "active";
    }
  in
  (match Credential_lease.make_env_lease handle ~env_name:"API_TOKEN" with
  | Error e ->
      failf "lease creation failed: %s"
        (Credential_lease.resolution_error_to_string e)
  | Ok lease ->
      let env_count = ref 0 in
      let env_name = ref "" in
      let env_value = ref "" in
      Credential_lease.apply_env_vars lease (fun env_vars ->
          env_count := List.length env_vars;
          let name, value = List.hd env_vars in
          env_name := name;
          env_value := value);
      check int "env var count" 1 !env_count;
      check string "env name" "API_TOKEN" !env_name;
      check string "env value" "my-secret-token" !env_value);
  Unix.putenv test_var ""

let test_make_url_lease () =
  let test_var = "CLAWQ_TEST_CRED_VAR4" in
  Unix.putenv test_var "bot-token-123";
  let handle : Runtime_config.credential_handle =
    {
      id = "test:url";
      provider = Env_var { name = test_var };
      description = Some "Bot token";
      status = "active";
    }
  in
  (match Credential_lease.make_url_lease handle with
  | Error e ->
      failf "lease creation failed: %s"
        (Credential_lease.resolution_error_to_string e)
  | Ok lease ->
      let segment = ref "" in
      Credential_lease.apply_url_segment lease (fun s -> segment := s);
      check string "url segment" "bot-token-123" !segment);
  Unix.putenv test_var ""

let test_resolve_missing_handle () =
  let config = Runtime_config.default in
  match
    Credential_lease.resolve_lease ~config ~handle_id:"nonexistent"
      ~header_name:"Authorization"
  with
  | Ok _ -> fail "should fail for missing handle"
  | Error (Credential_lease.Handle_not_found id) ->
      check string "handle id" "nonexistent" id
  | Error _ -> fail "wrong error type"

let test_resolve_from_config () =
  let test_var = "CLAWQ_TEST_CRED_VAR5" in
  Unix.putenv test_var "config-secret";
  let config =
    {
      Runtime_config.default with
      credential_handles =
        [
          {
            Runtime_config.id = "test:config";
            provider = Env_var { name = test_var };
            description = Some "Config credential";
            status = "active";
          };
        ];
    }
  in
  (match
     Credential_lease.resolve_lease ~config ~handle_id:"test:config"
       ~header_name:"X-API-Key"
   with
  | Error e ->
      failf "resolve failed: %s" (Credential_lease.resolution_error_to_string e)
  | Ok lease ->
      check string "handle_id" "test:config" lease.identity.handle_id;
      let header_name = ref "" in
      let header_value = ref "" in
      Credential_lease.apply_headers lease (fun headers ->
          let name, value = List.hd headers in
          header_name := name;
          header_value := value);
      check string "header name" "X-API-Key" !header_name;
      check string "header value" "config-secret" !header_value);
  Unix.putenv test_var ""

let test_inactive_handle_ignored () =
  let test_var = "CLAWQ_TEST_CRED_VAR6" in
  Unix.putenv test_var "should-not-resolve";
  let config =
    {
      Runtime_config.default with
      credential_handles =
        [
          {
            Runtime_config.id = "test:deleted";
            provider = Env_var { name = test_var };
            description = Some "Deleted credential";
            status = "deleted";
          };
        ];
    }
  in
  (match
     Credential_lease.resolve_lease ~config ~handle_id:"test:deleted"
       ~header_name:"Authorization"
   with
  | Ok _ -> fail "should fail for deleted handle"
  | Error (Credential_lease.Handle_not_found _) -> ()
  | Error _ -> fail "wrong error type");
  Unix.putenv test_var ""

let test_error_messages () =
  let cases =
    [
      (Credential_lease.Handle_not_found "x", "credential handle 'x' not found");
      ( Credential_lease.Env_var_unset "MY_VAR",
        "environment variable 'MY_VAR' is not set or empty" );
      ( Credential_lease.File_not_found "/no/such/file",
        "credential file '/no/such/file' not found" );
      ( Credential_lease.File_read_error ("/f", "perm denied"),
        "error reading credential file '/f': perm denied" );
      (Credential_lease.Decryption_error "bad key", "decryption failed: bad key");
      ( Credential_lease.Prompt_not_supported,
        "prompt-based credentials are not supported for automatic resolution" );
    ]
  in
  List.iter
    (fun (err, expected) ->
      check string "error message" expected
        (Credential_lease.resolution_error_to_string err))
    cases

let test_file_provider () =
  (* Create a temp file with a credential *)
  let tmp_file = Filename.temp_file "clawq-test-cred" ".txt" in
  let oc = open_out tmp_file in
  output_string oc "file-secret-value\n";
  close_out oc;
  let handle : Runtime_config.credential_handle =
    {
      id = "test:file";
      provider = File { path = tmp_file };
      description = Some "File credential";
      status = "active";
    }
  in
  (match Credential_lease.make_lease handle ~header_name:"Authorization" with
  | Error e ->
      failf "file lease failed: %s"
        (Credential_lease.resolution_error_to_string e)
  | Ok lease ->
      check string "provider_type" "file" lease.identity.provider_type;
      check string "redacted" "fil**************" lease.identity.redacted_value;
      let header_value = ref "" in
      Credential_lease.apply_headers lease (fun headers ->
          let _, value = List.hd headers in
          header_value := value);
      check string "file value" "file-secret-value" !header_value);
  Sys.remove tmp_file

let test_file_not_found () =
  let handle : Runtime_config.credential_handle =
    {
      id = "test:file_missing";
      provider = File { path = "/nonexistent/path/cred.txt" };
      description = None;
      status = "active";
    }
  in
  match Credential_lease.make_lease handle ~header_name:"X" with
  | Ok _ -> fail "should fail for missing file"
  | Error (Credential_lease.File_not_found _) -> ()
  | Error _ -> fail "wrong error type"

let test_prompt_not_supported () =
  let handle : Runtime_config.credential_handle =
    {
      id = "test:prompt";
      provider = Prompt { description = "Enter API key" };
      description = Some "Interactive credential";
      status = "active";
    }
  in
  match Credential_lease.make_lease handle ~header_name:"X" with
  | Ok _ -> fail "should fail for prompt provider"
  | Error Credential_lease.Prompt_not_supported -> ()
  | Error _ -> fail "wrong error type"

let test_encrypted_provider () =
  (* Test Encrypted provider - requires CLAWQ_MASTER_KEY to be set *)
  let master_key = Sys.getenv_opt "CLAWQ_MASTER_KEY" in
  match master_key with
  | None ->
      (* Skip test if CLAWQ_MASTER_KEY is not set - this is expected in CI *)
      Printf.eprintf "  [SKIP] encrypted_provider: CLAWQ_MASTER_KEY not set\n%!"
  | Some passphrase -> (
      (* Encrypt a test value *)
      let plaintext = "encrypted-secret-value" in
      let cipher_text =
        Secret_store.encrypt_secret
          ~key:(Secret_store.derive_key ~passphrase ())
          plaintext
      in
      let handle : Runtime_config.credential_handle =
        {
          id = "test:encrypted";
          provider = Encrypted { cipher_text };
          description = Some "Encrypted credential";
          status = "active";
        }
      in
      match Credential_lease.make_lease handle ~header_name:"Authorization" with
      | Error e ->
          failf "encrypted lease failed: %s"
            (Credential_lease.resolution_error_to_string e)
      | Ok lease ->
          check string "provider_type" "encrypted" lease.identity.provider_type;
          let header_value = ref "" in
          Credential_lease.apply_headers lease (fun headers ->
              let _, value = List.hd headers in
              header_value := value);
          check string "decrypted value" plaintext !header_value)

let test_apply_headers_empty_decorations () =
  let test_var = "CLAWQ_TEST_CRED_VAR7" in
  Unix.putenv test_var "value";
  let handle : Runtime_config.credential_handle =
    {
      id = "test:env_only";
      provider = Env_var { name = test_var };
      description = None;
      status = "active";
    }
  in
  (match Credential_lease.make_env_lease handle ~env_name:"X" with
  | Error _ -> fail "lease creation failed"
  | Ok lease ->
      (* apply_headers should call f with empty list when no Header decorations *)
      let header_count = ref (-1) in
      Credential_lease.apply_headers lease (fun headers ->
          header_count := List.length headers);
      check int "empty headers" 0 !header_count);
  Unix.putenv test_var ""

let test_apply_url_segment_empty_decorations () =
  let test_var = "CLAWQ_TEST_CRED_VAR8" in
  Unix.putenv test_var "value";
  let handle : Runtime_config.credential_handle =
    {
      id = "test:header_only";
      provider = Env_var { name = test_var };
      description = None;
      status = "active";
    }
  in
  (match Credential_lease.make_lease handle ~header_name:"X" with
  | Error _ -> fail "lease creation failed"
  | Ok lease ->
      (* apply_url_segment should call f with empty string when no Url decorations *)
      let segment = ref "not-set" in
      Credential_lease.apply_url_segment lease (fun s -> segment := s);
      check string "empty segment" "" !segment);
  Unix.putenv test_var ""

let suite =
  [
    ("redact_secret", `Quick, test_redact_secret);
    ("resolve_env_var", `Quick, test_resolve_env_var);
    ("make_lease_from_env", `Quick, test_make_lease_from_env);
    ("make_env_lease", `Quick, test_make_env_lease);
    ("make_url_lease", `Quick, test_make_url_lease);
    ("resolve_missing_handle", `Quick, test_resolve_missing_handle);
    ("resolve_from_config", `Quick, test_resolve_from_config);
    ("inactive_handle_ignored", `Quick, test_inactive_handle_ignored);
    ("error_messages", `Quick, test_error_messages);
    ("file_provider", `Quick, test_file_provider);
    ("file_not_found", `Quick, test_file_not_found);
    ("prompt_not_supported", `Quick, test_prompt_not_supported);
    ("encrypted_provider", `Quick, test_encrypted_provider);
    ( "apply_headers_empty_decorations",
      `Quick,
      test_apply_headers_empty_decorations );
    ( "apply_url_segment_empty_decorations",
      `Quick,
      test_apply_url_segment_empty_decorations );
  ]

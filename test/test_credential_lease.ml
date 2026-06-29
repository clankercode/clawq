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
      ( Credential_lease.Handle_not_allowed "x",
        "credential handle 'x' is not allowed by current access policy" );
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

let effective_access_with_credentials credential_handles :
    Runtime_config.effective_access =
  let credential_handles =
    List.map
      (fun value ->
        ({ value; provenance = [] } : Runtime_config.effective_access_item))
      credential_handles
  in
  {
    allowed_tools = [];
    denied_tools = [];
    codebase_grants = [];
    blocked_codebase_grants = [];
    mcp_servers = [];
    skills = [];
    repositories = [];
    repo_grants = [];
    blocked_repo_grants = [];
    domains = [];
    credential_handles;
    instructions = [];
    instruction_items = [];
    memory_grants = [];
    budget_refs = [];
    egress_rules = [];
  }

let test_scoped_lease_denies_unlisted_handle_before_provider_resolution () =
  let handle : Runtime_config.credential_handle =
    {
      id = "test:blocked";
      provider = Env_var { name = "CLAWQ_TEST_BLOCKED_CREDENTIAL" };
      description = None;
      status = "active";
    }
  in
  let config =
    { Runtime_config.default with credential_handles = [ handle ] }
  in
  Unix.putenv "CLAWQ_TEST_BLOCKED_CREDENTIAL" "";
  match
    Credential_lease.resolve_scoped_lease ~config ~allowed_handle_ids:[]
      ~handle_id:"test:blocked" ~header_name:"Authorization"
  with
  | Ok _ -> fail "should fail for handle outside scoped policy"
  | Error (Credential_lease.Handle_not_allowed id) ->
      check string "denied handle id" "test:blocked" id
  | Error (Credential_lease.Env_var_unset _) ->
      fail "provider was resolved before policy denial"
  | Error _ -> fail "wrong error type"

let test_effective_access_lease_allows_inherited_handle () =
  let test_var = "CLAWQ_TEST_ALLOWED_CREDENTIAL" in
  Unix.putenv test_var "allowed-secret";
  let config =
    {
      Runtime_config.default with
      credential_handles =
        [
          {
            Runtime_config.id = "test:allowed";
            provider = Env_var { name = test_var };
            description = None;
            status = "active";
          };
        ];
    }
  in
  let access = effective_access_with_credentials [ "test:allowed" ] in
  (match
     Credential_lease.resolve_effective_access_lease ~config ~access
       ~handle_id:"test:allowed" ~header_name:"X-Allowed"
   with
  | Error e ->
      failf "scoped resolve failed: %s"
        (Credential_lease.resolution_error_to_string e)
  | Ok lease ->
      let header_value = ref "" in
      Credential_lease.apply_headers lease (fun headers ->
          let name, value = List.hd headers in
          check string "header name" "X-Allowed" name;
          header_value := value);
      check string "header value" "allowed-secret" !header_value);
  Unix.putenv test_var ""

let test_effective_access_lease_denies_non_inherited_handle () =
  let config =
    {
      Runtime_config.default with
      credential_handles =
        [
          {
            Runtime_config.id = "test:not-inherited";
            provider = Env_var { name = "CLAWQ_TEST_NOT_INHERITED" };
            description = None;
            status = "active";
          };
        ];
    }
  in
  let access = effective_access_with_credentials [] in
  Unix.putenv "CLAWQ_TEST_NOT_INHERITED" "";
  match
    Credential_lease.resolve_effective_access_lease ~config ~access
      ~handle_id:"test:not-inherited" ~header_name:"Authorization"
  with
  | Ok _ -> fail "should fail for non-inherited handle"
  | Error (Credential_lease.Handle_not_allowed id) ->
      check string "denied handle id" "test:not-inherited" id
  | Error (Credential_lease.Env_var_unset _) ->
      fail "provider was resolved before effective-access denial"
  | Error _ -> fail "wrong error type"

let test_snapshot_lease_denies_non_snapshot_handle () =
  let config =
    {
      Runtime_config.default with
      credential_handles =
        [
          {
            Runtime_config.id = "test:not-snapshot";
            provider = Env_var { name = "CLAWQ_TEST_NOT_SNAPSHOT" };
            description = None;
            status = "active";
          };
        ];
    }
  in
  let snapshot =
    Access_snapshot.create ~config ~work_type:Access_snapshot.Room_turn
      ~session_key:"test:room" ()
  in
  Unix.putenv "CLAWQ_TEST_NOT_SNAPSHOT" "";
  match
    Credential_lease.resolve_snapshot_lease ~config ~snapshot
      ~handle_id:"test:not-snapshot" ~header_name:"Authorization"
  with
  | Ok _ -> fail "should fail for handle outside snapshot policy"
  | Error (Credential_lease.Handle_not_allowed id) ->
      check string "denied handle id" "test:not-snapshot" id
  | Error (Credential_lease.Env_var_unset _) ->
      fail "provider was resolved before snapshot denial"
  | Error _ -> fail "wrong error type"

let test_encrypted_provider () =
  (* Test Encrypted provider - requires CLAWQ_MASTER_KEY to be set *)
  let master_key = Sys.getenv_opt "CLAWQ_MASTER_KEY" in
  match master_key with
  | None | Some "" ->
      (* Skip test if CLAWQ_MASTER_KEY is unset/empty - this is expected in CI. *)
      Alcotest.skip ()
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
    ( "scoped_lease_denies_unlisted_handle_before_provider_resolution",
      `Quick,
      test_scoped_lease_denies_unlisted_handle_before_provider_resolution );
    ( "effective_access_lease_allows_inherited_handle",
      `Quick,
      test_effective_access_lease_allows_inherited_handle );
    ( "effective_access_lease_denies_non_inherited_handle",
      `Quick,
      test_effective_access_lease_denies_non_inherited_handle );
    ( "snapshot_lease_denies_non_snapshot_handle",
      `Quick,
      test_snapshot_lease_denies_non_snapshot_handle );
    ("encrypted_provider", `Quick, test_encrypted_provider);
    ( "apply_headers_empty_decorations",
      `Quick,
      test_apply_headers_empty_decorations );
    ( "apply_url_segment_empty_decorations",
      `Quick,
      test_apply_url_segment_empty_decorations );
  ]

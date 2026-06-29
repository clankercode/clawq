(* Tests for credential handle and provider types *)

let test_credential_handles_default_empty () =
  Alcotest.(check int)
    "default credential_handles empty" 0
    (List.length Runtime_config.default.credential_handles)

let test_parse_credential_handles () =
  let json =
    Yojson.Safe.from_string
      {|{
        "credential_handles": [
          {
            "id": "github-app:main",
            "provider": { "type": "env_var", "name": "GITHUB_TOKEN" },
            "description": "GitHub App token",
            "status": "active"
          },
          {
            "id": "slack-bot",
            "provider": { "type": "file", "path": "~/.slack-token" },
            "status": "active"
          },
          {
            "id": "encrypted-key",
            "provider": { "type": "encrypted", "cipher_text": "$ENC:abc123" },
            "status": "active"
          },
          {
            "id": "manual-key",
            "provider": { "type": "prompt", "description": "Enter API key" },
            "status": "active"
          }
        ]
      }|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check int)
    "credential_handles count" 4
    (List.length cfg.credential_handles);
  let ch0 = List.nth cfg.credential_handles 0 in
  Alcotest.(check string) "ch0.id" "github-app:main" ch0.id;
  (match ch0.provider with
  | Runtime_config.Env_var { name } ->
      Alcotest.(check string) "env var name" "GITHUB_TOKEN" name
  | _ -> Alcotest.fail "expected Env_var provider");
  Alcotest.(check (option string))
    "ch0.description" (Some "GitHub App token") ch0.description;
  let ch1 = List.nth cfg.credential_handles 1 in
  (match ch1.provider with
  | Runtime_config.File { path } ->
      Alcotest.(check string) "file path" "~/.slack-token" path
  | _ -> Alcotest.fail "expected File provider");
  let ch2 = List.nth cfg.credential_handles 2 in
  (match ch2.provider with
  | Runtime_config.Encrypted { cipher_text } ->
      Alcotest.(check string) "cipher text" "$ENC:abc123" cipher_text
  | _ -> Alcotest.fail "expected Encrypted provider");
  let ch3 = List.nth cfg.credential_handles 3 in
  match ch3.provider with
  | Runtime_config.Prompt { description } ->
      Alcotest.(check string) "prompt description" "Enter API key" description
  | _ -> Alcotest.fail "expected Prompt provider"

let test_credential_handles_roundtrip () =
  let cfg =
    {
      Runtime_config.default with
      credential_handles =
        [
          {
            Runtime_config.id = "github-app:main";
            provider = Env_var { name = "GITHUB_TOKEN" };
            description = Some "GitHub App token";
            status = "active";
          };
          {
            id = "deleted-handle";
            provider = File { path = "/tmp/key" };
            description = None;
            status = "deleted";
          };
        ];
    }
  in
  let json = Runtime_config.to_json cfg in
  let json_s = Yojson.Safe.to_string json in
  let parsed = Config_loader.parse_config (Yojson.Safe.from_string json_s) in
  Alcotest.(check int)
    "roundtrip credential_handles count" 2
    (List.length parsed.credential_handles);
  let ch0 = List.nth parsed.credential_handles 0 in
  Alcotest.(check string) "roundtrip ch0.id" "github-app:main" ch0.id;
  (match ch0.provider with
  | Runtime_config.Env_var { name } ->
      Alcotest.(check string) "roundtrip env var" "GITHUB_TOKEN" name
  | _ -> Alcotest.fail "expected Env_var");
  let ch1 = List.nth parsed.credential_handles 1 in
  Alcotest.(check string) "roundtrip ch1.status" "deleted" ch1.status

let test_credential_handle_json_metadata_correctness () =
  let cfg =
    {
      Runtime_config.default with
      credential_handles =
        [
          {
            Runtime_config.id = "test";
            provider = Env_var { name = "MY_SECRET_VAR" };
            description = None;
            status = "active";
          };
        ];
    }
  in
  let json = Yojson.Safe.to_string (Runtime_config.to_json cfg) in
  (* The env var NAME is metadata, not a secret - it's OK in JSON.
     What must NOT appear is the env var VALUE. *)
  Alcotest.(check bool)
    "json contains handle id" true
    (Test_helpers.string_contains json "test");
  Alcotest.(check bool)
    "json contains provider type" true
    (Test_helpers.string_contains json "env_var");
  Alcotest.(check bool)
    "json contains env var name" true
    (Test_helpers.string_contains json "MY_SECRET_VAR")

let test_credential_handle_find () =
  let cfg =
    {
      Runtime_config.default with
      credential_handles =
        [
          {
            Runtime_config.id = "active-handle";
            provider = Env_var { name = "X" };
            description = None;
            status = "active";
          };
          {
            id = "deleted-handle";
            provider = Env_var { name = "Y" };
            description = None;
            status = "deleted";
          };
        ];
    }
  in
  Alcotest.(check bool)
    "find active handle" true
    (Option.is_some (Runtime_config.find_credential_handle cfg "active-handle"));
  Alcotest.(check bool)
    "find deleted handle returns None" true
    (Option.is_none
       (Runtime_config.find_credential_handle cfg "deleted-handle"));
  Alcotest.(check bool)
    "find missing handle returns None" true
    (Option.is_none (Runtime_config.find_credential_handle cfg "nonexistent"))

let test_credential_handle_validate_refs () =
  let cfg =
    {
      Runtime_config.default with
      credential_handles =
        [
          {
            Runtime_config.id = "valid-handle";
            provider = Env_var { name = "X" };
            description = None;
            status = "active";
          };
        ];
      access_bundles =
        [
          {
            Runtime_config.id = "bundle1";
            display_name = None;
            system_prompt = None;
            allowed_tools = [];
            denied_tools = [];
            codebase_grants = [];
            mcp_servers = [];
            skills = [];
            repositories = [];
            repo_grants = [];
            domains = [];
            egress_rules = [];
            credential_handles = [ "valid-handle"; "missing-handle" ];
            instructions = [];
            memory_grants = [];
            budget_refs = [];
            status = "active";
          };
        ];
    }
  in
  let missing = Runtime_config.validate_credential_handle_refs cfg in
  Alcotest.(check int) "missing refs count" 1 (List.length missing);
  Alcotest.(check string) "missing ref id" "missing-handle" (List.nth missing 0)

let test_credential_handle_bundle_reference_validation () =
  let json =
    Yojson.Safe.from_string
      {|{
        "credential_handles": [
          {"id": "ch1", "provider": {"type": "env_var", "name": "X"}}
        ],
        "access_bundles": [
          {
            "id": "bundle1",
            "credential_handles": ["ch1", "nonexistent"]
          }
        ]
      }|}
  in
  let cfg = Config_loader.parse_config json in
  let missing = Runtime_config.validate_credential_handle_refs cfg in
  Alcotest.(check int) "bundle refs missing handles" 1 (List.length missing);
  Alcotest.(check string) "missing handle" "nonexistent" (List.nth missing 0)

let test_encrypted_provider_requires_enc_prefix () =
  (* Valid: $ENC: prefix *)
  let json_valid =
    Yojson.Safe.from_string
      {|{
        "credential_handles": [
          {"id": "ok", "provider": {"type": "encrypted", "cipher_text": "$ENC:abc123"}}
        ]
      }|}
  in
  let cfg_valid = Config_loader.parse_config json_valid in
  (match (List.nth cfg_valid.credential_handles 0).provider with
  | Runtime_config.Encrypted _ -> ()
  | _ -> Alcotest.fail "expected Encrypted for valid $ENC: prefix");
  (* Invalid: no $ENC: prefix *)
  let json_invalid =
    Yojson.Safe.from_string
      {|{
        "credential_handles": [
          {"id": "bad", "provider": {"type": "encrypted", "cipher_text": "plaintext"}}
        ]
      }|}
  in
  let cfg_invalid = Config_loader.parse_config json_invalid in
  match (List.nth cfg_invalid.credential_handles 0).provider with
  | Runtime_config.Env_var _ -> ()
  | _ ->
      Alcotest.fail "expected Env_var fallback for invalid encrypted provider"

let suite =
  [
    Alcotest.test_case "credential_handles default empty" `Quick
      test_credential_handles_default_empty;
    Alcotest.test_case "parse credential_handles" `Quick
      test_parse_credential_handles;
    Alcotest.test_case "credential_handles roundtrip" `Quick
      test_credential_handles_roundtrip;
    Alcotest.test_case "credential_handle json metadata correctness" `Quick
      test_credential_handle_json_metadata_correctness;
    Alcotest.test_case "credential_handle find" `Quick
      test_credential_handle_find;
    Alcotest.test_case "credential_handle validate refs" `Quick
      test_credential_handle_validate_refs;
    Alcotest.test_case "credential_handle bundle reference validation" `Quick
      test_credential_handle_bundle_reference_validation;
    Alcotest.test_case "encrypted provider requires $ENC: prefix" `Quick
      test_encrypted_provider_requires_enc_prefix;
  ]

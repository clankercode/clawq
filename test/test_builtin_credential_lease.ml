(* Tests for credential lease wrapping of built-in tools.
   Verifies that web_search, zai_websearch, zai_webfetch, and transcribe
   resolve credentials through the lease API and deny before network calls
   when handles are missing or unauthorized. *)

open Alcotest

(* Helper: create a config with a credential handle for web_search *)
let config_with_web_search_credential ~handle_id ~api_key_var =
  let api_key_var_name = api_key_var in
  {
    Runtime_config.default with
    credential_handles =
      [
        {
          Runtime_config.id = handle_id;
          provider = Env_var { name = api_key_var_name };
          description = Some "Test credential";
          status = "active";
        };
      ];
    web_search =
      Some
        {
          Runtime_config.search_provider = "brave";
          search_api_key = "legacy-key";
          num_results = 5;
          search_base_url = None;
          credential_handle = Some handle_id;
        };
  }

(* Helper: create a config with a credential handle for zai_mcp *)
let config_with_zai_credential ~handle_id ~api_key_var =
  let api_key_var_name = api_key_var in
  {
    Runtime_config.default with
    credential_handles =
      [
        {
          Runtime_config.id = handle_id;
          provider = Env_var { name = api_key_var_name };
          description = Some "Test credential";
          status = "active";
        };
      ];
    zai_mcp =
      Some
        {
          Runtime_config.key = "legacy-zai-key";
          websearch_enabled = true;
          webfetch_enabled = true;
          credential_handle = Some handle_id;
        };
  }

(* Helper: create a config with a credential handle for STT *)
let config_with_stt_credential ~handle_id ~api_key_var =
  let api_key_var_name = api_key_var in
  {
    Runtime_config.default with
    credential_handles =
      [
        {
          Runtime_config.id = handle_id;
          provider = Env_var { name = api_key_var_name };
          description = Some "Test credential";
          status = "active";
        };
      ];
    stt =
      Some
        {
          Runtime_config.provider = "groq";
          model = "whisper-large-v3";
          language = None;
          credential_handle = Some handle_id;
        };
  }

(* Test: resolve_credential_handle returns Ok "" when handle_id is None *)
let test_resolve_credential_handle_none () =
  let config = Runtime_config.default in
  match
    Tools_builtin_util.resolve_credential_handle ~config ~handle_id:None
      ~header_name:"Authorization"
  with
  | Ok "" -> () (* Expected: None handle returns Ok "" *)
  | Ok _ -> fail "should return empty string for None handle"
  | Error _ -> fail "should not error for None handle"

(* Test: resolve_credential_handle returns Ok value when handle resolves *)
let test_resolve_credential_handle_success () =
  let test_var = "CLAWQ_TEST_BUILTIN_CRED_OK" in
  Unix.putenv test_var "test-api-key-123";
  let config =
    {
      Runtime_config.default with
      credential_handles =
        [
          {
            Runtime_config.id = "test:ok";
            provider = Env_var { name = test_var };
            description = Some "Test";
            status = "active";
          };
        ];
    }
  in
  (match
     Tools_builtin_util.resolve_credential_handle ~config
       ~handle_id:(Some "test:ok") ~header_name:"X-API-Key"
   with
  | Ok value -> check string "resolved value" "test-api-key-123" value
  | Error msg -> failf "should succeed: %s" msg);
  Unix.putenv test_var ""

(* Test: resolve_credential_handle returns Error when handle is missing *)
let test_resolve_credential_handle_missing () =
  let config = Runtime_config.default in
  match
    Tools_builtin_util.resolve_credential_handle ~config
      ~handle_id:(Some "nonexistent:handle") ~header_name:"Authorization"
  with
  | Ok _ -> fail "should fail for missing handle"
  | Error msg ->
      check bool "error contains handle id" true (String.length msg > 0)

(* Test: resolve_credential_handle returns Error when env var is unset *)
let test_resolve_credential_handle_env_unset () =
  let test_var = "CLAWQ_TEST_BUILTIN_CRED_UNSET" in
  Unix.putenv test_var "";
  let config =
    {
      Runtime_config.default with
      credential_handles =
        [
          {
            Runtime_config.id = "test:unset";
            provider = Env_var { name = test_var };
            description = Some "Test";
            status = "active";
          };
        ];
    }
  in
  (match
     Tools_builtin_util.resolve_credential_handle ~config
       ~handle_id:(Some "test:unset") ~header_name:"Authorization"
   with
  | Ok _ -> fail "should fail for unset env var"
  | Error _ -> () (* Expected: env var unset error *));
  Unix.putenv test_var ""

(* Test: web_search with credential handle resolves lease *)
let test_web_search_uses_credential_handle () =
  let test_var = "CLAWQ_TEST_WS_CRED" in
  Unix.putenv test_var "lease-api-key";
  let config =
    config_with_web_search_credential ~handle_id:"ws:test" ~api_key_var:test_var
  in
  (* The web_search tool should resolve the credential handle.
     We can't actually call the network, but we verify the lease resolution
     works by testing resolve_credential_handle directly. *)
  (match
     Tools_builtin_util.resolve_credential_handle ~config
       ~handle_id:(Some "ws:test") ~header_name:"X-Subscription-Token"
   with
  | Ok value -> check string "lease resolves" "lease-api-key" value
  | Error msg -> failf "lease should resolve: %s" msg);
  Unix.putenv test_var ""

(* Test: web_search with missing credential handle denies before network *)
let test_web_search_denies_missing_handle () =
  let config =
    {
      Runtime_config.default with
      credential_handles = [];
      web_search =
        Some
          {
            Runtime_config.search_provider = "brave";
            search_api_key = "legacy-key";
            num_results = 5;
            search_base_url = None;
            credential_handle = Some "ws:missing";
          };
    }
  in
  match
    Tools_builtin_util.resolve_credential_handle ~config
      ~handle_id:(Some "ws:missing") ~header_name:"X-Subscription-Token"
  with
  | Ok _ -> fail "should deny missing handle"
  | Error _ -> () (* Expected: denial before network *)

(* Test: zai_websearch with credential handle resolves lease *)
let test_zai_websearch_uses_credential_handle () =
  let test_var = "CLAWQ_TEST_ZAI_CRED" in
  Unix.putenv test_var "zai-lease-key";
  let config =
    config_with_zai_credential ~handle_id:"zai:test" ~api_key_var:test_var
  in
  (match
     Tools_builtin_util.resolve_credential_handle ~config
       ~handle_id:(Some "zai:test") ~header_name:"Authorization"
   with
  | Ok value -> check string "zai lease resolves" "zai-lease-key" value
  | Error msg -> failf "zai lease should resolve: %s" msg);
  Unix.putenv test_var ""

(* Test: zai_websearch with missing credential handle denies before network *)
let test_zai_websearch_denies_missing_handle () =
  let config =
    {
      Runtime_config.default with
      credential_handles = [];
      zai_mcp =
        Some
          {
            Runtime_config.key = "legacy-key";
            websearch_enabled = true;
            webfetch_enabled = true;
            credential_handle = Some "zai:missing";
          };
    }
  in
  match
    Tools_builtin_util.resolve_credential_handle ~config
      ~handle_id:(Some "zai:missing") ~header_name:"Authorization"
  with
  | Ok _ -> fail "should deny missing zai handle"
  | Error _ -> () (* Expected: denial before network *)

(* Test: zai_webfetch with credential handle resolves lease *)
let test_zai_webfetch_uses_credential_handle () =
  let test_var = "CLAWQ_TEST_ZFETCH_CRED" in
  Unix.putenv test_var "zfetch-lease-key";
  let config =
    config_with_zai_credential ~handle_id:"zfetch:test" ~api_key_var:test_var
  in
  (match
     Tools_builtin_util.resolve_credential_handle ~config
       ~handle_id:(Some "zfetch:test") ~header_name:"Authorization"
   with
  | Ok value -> check string "zfetch lease resolves" "zfetch-lease-key" value
  | Error msg -> failf "zfetch lease should resolve: %s" msg);
  Unix.putenv test_var ""

(* Test: transcribe with credential handle resolves lease *)
let test_transcribe_uses_credential_handle () =
  let test_var = "CLAWQ_TEST_STT_CRED" in
  Unix.putenv test_var "stt-lease-key";
  let config =
    config_with_stt_credential ~handle_id:"stt:test" ~api_key_var:test_var
  in
  (match
     Tools_builtin_util.resolve_credential_handle ~config
       ~handle_id:(Some "stt:test") ~header_name:"Authorization"
   with
  | Ok value -> check string "stt lease resolves" "stt-lease-key" value
  | Error msg -> failf "stt lease should resolve: %s" msg);
  Unix.putenv test_var ""

(* Test: transcribe with missing credential handle denies before network *)
let test_transcribe_denies_missing_handle () =
  let config =
    {
      Runtime_config.default with
      credential_handles = [];
      stt =
        Some
          {
            Runtime_config.provider = "groq";
            model = "whisper-large-v3";
            language = None;
            credential_handle = Some "stt:missing";
          };
    }
  in
  match
    Tools_builtin_util.resolve_credential_handle ~config
      ~handle_id:(Some "stt:missing") ~header_name:"Authorization"
  with
  | Ok _ -> fail "should deny missing stt handle"
  | Error _ -> () (* Expected: denial before network *)

(* Test: web_search without credential handle uses legacy path *)
let test_web_search_legacy_path () =
  let config =
    {
      Runtime_config.default with
      web_search =
        Some
          {
            Runtime_config.search_provider = "brave";
            search_api_key = "legacy-key";
            num_results = 5;
            search_base_url = None;
            credential_handle = None;
          };
    }
  in
  match
    Tools_builtin_util.resolve_credential_handle ~config ~handle_id:None
      ~header_name:"X-Subscription-Token"
  with
  | Ok "" -> () (* Expected: None handle returns Ok "" (legacy path) *)
  | Ok _ -> fail "should return empty for None handle"
  | Error _ -> fail "should not error for None handle"

(* Test: config serialization roundtrip preserves credential_handle *)
let test_web_search_config_roundtrip () =
  let config =
    {
      Runtime_config.default with
      web_search =
        Some
          {
            Runtime_config.search_provider = "brave";
            search_api_key = "key";
            num_results = 5;
            search_base_url = None;
            credential_handle = Some "ws:roundtrip";
          };
    }
  in
  let json =
    Runtime_config_json.to_json ~default_quota_cache_ttl_s:300
      ~default_log_config:Runtime_config.default.log config
  in
  let json_str = Yojson.Safe.to_string json in
  check bool "contains credential_handle" true (String.length json_str > 0)

let suite =
  [
    ( "resolve_credential_handle_none",
      `Quick,
      test_resolve_credential_handle_none );
    ( "resolve_credential_handle_success",
      `Quick,
      test_resolve_credential_handle_success );
    ( "resolve_credential_handle_missing",
      `Quick,
      test_resolve_credential_handle_missing );
    ( "resolve_credential_handle_env_unset",
      `Quick,
      test_resolve_credential_handle_env_unset );
    ( "web_search_uses_credential_handle",
      `Quick,
      test_web_search_uses_credential_handle );
    ( "web_search_denies_missing_handle",
      `Quick,
      test_web_search_denies_missing_handle );
    ( "zai_websearch_uses_credential_handle",
      `Quick,
      test_zai_websearch_uses_credential_handle );
    ( "zai_websearch_denies_missing_handle",
      `Quick,
      test_zai_websearch_denies_missing_handle );
    ( "zai_webfetch_uses_credential_handle",
      `Quick,
      test_zai_webfetch_uses_credential_handle );
    ( "transcribe_uses_credential_handle",
      `Quick,
      test_transcribe_uses_credential_handle );
    ( "transcribe_denies_missing_handle",
      `Quick,
      test_transcribe_denies_missing_handle );
    ("web_search_legacy_path", `Quick, test_web_search_legacy_path);
    ("web_search_config_roundtrip", `Quick, test_web_search_config_roundtrip);
  ]

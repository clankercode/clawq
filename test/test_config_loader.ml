let with_temp_file contents f =
  let path = Filename.temp_file "clawq_config" ".json" in
  let oc = open_out path in
  output_string oc contents;
  close_out oc;
  Fun.protect
    (fun () -> f path)
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)

let test_load_backfill_preserves_unknown_keys () =
  let json =
    {|{
      "foo": 1,
      "agent_defaults": {"primary_model": "x-model"}
    }|}
  in
  with_temp_file json (fun path ->
      let _cfg = Config_loader.load ~path () in
      let out = Yojson.Safe.from_file path in
      let open Yojson.Safe.Util in
      Alcotest.(check int)
        "unknown key preserved" 1
        (out |> member "foo" |> to_int);
      Alcotest.(check string)
        "existing key not clobbered" "x-model"
        (out |> member "agent_defaults" |> member "primary_model" |> to_string);
      Alcotest.(check bool)
        "defaults backfilled" true
        (out |> member "default_temperature" <> `Null))

let test_parse_provider_env_secret_resolution () =
  Unix.putenv "CLAWQ_TEST_PROVIDER_KEY" "sk-from-env";
  let json =
    Yojson.Safe.from_string
      {|{
        "providers": {
          "p": {"api_key": "$CLAWQ_TEST_PROVIDER_KEY"}
        }
      }|}
  in
  let cfg = Config_loader.parse_config json in
  let key = (List.assoc "p" cfg.providers).Runtime_config.api_key in
  Alcotest.(check string) "resolved env secret" "sk-from-env" key

let test_parse_telegram_env_secret_resolution () =
  Unix.putenv "CLAWQ_TEST_BOT_TOKEN" "bot-token-from-env";
  let json =
    Yojson.Safe.from_string
      {|{
        "channels": {
          "telegram": {
            "accounts": {
              "main": {"bot_token": "$CLAWQ_TEST_BOT_TOKEN", "allow_from": ["*"]}
            }
          }
        }
      }|}
  in
  let cfg = Config_loader.parse_config json in
  match cfg.channels.telegram with
  | None -> Alcotest.fail "expected telegram config"
  | Some tg ->
      let acct = List.assoc "main" tg.accounts in
      Alcotest.(check string)
        "resolved bot token" "bot-token-from-env" acct.bot_token

let test_load_invalid_json_returns_default () =
  with_temp_file "{ invalid json" (fun path ->
      let cfg = Config_loader.load ~path () in
      Alcotest.(check string)
        "fallback primary model"
        Runtime_config.default.agent_defaults.primary_model
        cfg.agent_defaults.primary_model)

let test_parse_legacy_nested_paths () =
  let json =
    Yojson.Safe.from_string
      {|{
        "memory": {"search": {"enabled": true}},
        "security": {
          "audit": {"enabled": true},
          "tools": {"enabled": false}
        },
        "autonomy": {"workspace_only": false}
      }|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check bool)
    "memory.search.enabled parsed" true cfg.memory.search_enabled;
  Alcotest.(check bool)
    "security.audit.enabled parsed" true cfg.security.audit_enabled;
  Alcotest.(check bool)
    "security.tools.enabled parsed" false cfg.security.tools_enabled;
  Alcotest.(check bool)
    "autonomy.workspace_only parsed" false cfg.security.workspace_only

let test_backfill_replaces_type_mismatch_with_defaults () =
  let json = {|{
      "memory": "oops",
      "security": "bad"
    }|} in
  with_temp_file json (fun path ->
      let cfg = Config_loader.load ~path () in
      Alcotest.(check string)
        "memory backend defaulted" Runtime_config.default.memory.backend
        cfg.memory.backend;
      let out = Yojson.Safe.from_file path in
      let open Yojson.Safe.Util in
      Alcotest.(check bool)
        "memory backfilled as object" true
        (out |> member "memory" |> member "backend" <> `Null);
      Alcotest.(check bool)
        "security backfilled as object" true
        (out |> member "security" |> member "workspace_only" <> `Null))

let test_parse_gateway_auth_token () =
  let json =
    Yojson.Safe.from_string
      {|{
        "gateway": {"auth_token": "abc123"}
      }|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check (option string))
    "gateway auth token parsed" (Some "abc123") cfg.gateway.auth_token

let test_backfill_does_not_persist_resolved_secrets () =
  Unix.putenv "CLAWQ_TEST_SECRET_BACKFILL" "sk-live-secret";
  let json =
    {|{
      "providers": {
        "p": {"api_key": "$CLAWQ_TEST_SECRET_BACKFILL"}
      }
    }|}
  in
  with_temp_file json (fun path ->
      let cfg = Config_loader.load ~path () in
      let key = (List.assoc "p" cfg.providers).Runtime_config.api_key in
      Alcotest.(check string)
        "runtime config resolves secret" "sk-live-secret" key;
      let out = Yojson.Safe.from_file path in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "file keeps env placeholder" "$CLAWQ_TEST_SECRET_BACKFILL"
        (out |> member "providers" |> member "p" |> member "api_key"
       |> to_string))

let test_backfill_infers_default_provider_from_model_priority () =
  let json =
    {|{
      "providers": {
        "groq": {"api_key": "sk-groq"},
        "zai_coding": {"api_key": "sk-zai"}
      },
      "agent_defaults": {
        "primary_model": "glm-5",
        "model_priority": [
          {"provider": "zai_coding", "model": "glm-5"},
          {"provider": "groq", "model": "llama-3.3-70b-versatile"}
        ]
      }
    }|}
  in
  with_temp_file json (fun path ->
      let cfg = Config_loader.load ~path () in
      Alcotest.(check (option string))
        "runtime default_provider inferred" (Some "zai_coding")
        cfg.default_provider;
      let out = Yojson.Safe.from_file path in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "default_provider backfilled" "zai_coding"
        (out |> member "default_provider" |> to_string))

let test_parse_codex_oauth_provider () =
  let json =
    Yojson.Safe.from_string
      {|{
        "providers": {
          "openai-codex": {
            "kind": "openai-codex",
            "base_url": "https://chatgpt.com/backend-api/codex",
            "codex_oauth": {
              "access_token": "access-token",
              "refresh_token": "refresh-token",
              "expires_at_ms": 1730000000000,
              "account_id": "acct_123",
              "email": "me@example.com"
            }
          }
        }
      }|}
  in
  let cfg = Config_loader.parse_config json in
  let provider = List.assoc "openai-codex" cfg.providers in
  Alcotest.(check (option string))
    "kind parsed" (Some "openai-codex") provider.kind;
  Alcotest.(check bool)
    "provider has auth" true
    (Runtime_config.provider_has_auth provider);
  match provider.codex_oauth with
  | None -> Alcotest.fail "expected codex oauth creds"
  | Some creds ->
      Alcotest.(check string) "access token" "access-token" creds.access_token;
      Alcotest.(check string)
        "refresh token" "refresh-token" creds.refresh_token;
      Alcotest.(check int) "expires at" 1730000000000 creds.expires_at_ms

let test_to_json_preserves_codex_oauth_provider () =
  let provider : Runtime_config.provider_config =
    {
      api_key = "";
      kind = Some "openai-codex";
      base_url = Some "https://chatgpt.com/backend-api/codex";
      default_model = Some "openai-codex/gpt-5-codex";
      project_id = None;
      location = None;
      service_account_json = None;
      thinking_budget_tokens = None;
      oai_thinking_style = "none";
      codex_oauth =
        Some
          {
            Runtime_config.access_token = "access-token";
            refresh_token = "refresh-token";
            expires_at_ms = 1730000000000;
            account_id = Some "acct_123";
            email = Some "me@example.com";
          };
    }
  in
  let json =
    Runtime_config.to_json
      { Runtime_config.default with providers = [ ("openai-codex", provider) ] }
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "kind serialized" "openai-codex"
    (json |> member "providers" |> member "openai-codex" |> member "kind"
   |> to_string);
  Alcotest.(check string)
    "oauth access token serialized" "access-token"
    (json |> member "providers" |> member "openai-codex" |> member "codex_oauth"
   |> member "access_token" |> to_string)

let test_parse_provider_thinking_fields () =
  let json =
    Yojson.Safe.from_string
      {|{
        "providers": {
          "deepseek": {
            "api_key": "sk-test",
            "oai_thinking_style": "reasoning_content"
          },
          "anthropic": {
            "api_key": "sk-ant-test",
            "thinking_budget_tokens": 4096
          }
        }
      }|}
  in
  let cfg = Config_loader.parse_config json in
  let deepseek = List.assoc "deepseek" cfg.providers in
  let anthropic = List.assoc "anthropic" cfg.providers in
  Alcotest.(check string)
    "oai thinking style parsed" "reasoning_content" deepseek.oai_thinking_style;
  Alcotest.(check (option int))
    "thinking budget parsed" (Some 4096) anthropic.thinking_budget_tokens

let suite =
  [
    Alcotest.test_case "load backfill preserves unknown keys" `Quick
      test_load_backfill_preserves_unknown_keys;
    Alcotest.test_case "provider env secret resolution" `Quick
      test_parse_provider_env_secret_resolution;
    Alcotest.test_case "telegram env secret resolution" `Quick
      test_parse_telegram_env_secret_resolution;
    Alcotest.test_case "load invalid json returns default" `Quick
      test_load_invalid_json_returns_default;
    Alcotest.test_case "parse legacy nested paths" `Quick
      test_parse_legacy_nested_paths;
    Alcotest.test_case "backfill replaces type mismatch" `Quick
      test_backfill_replaces_type_mismatch_with_defaults;
    Alcotest.test_case "parse gateway auth token" `Quick
      test_parse_gateway_auth_token;
    Alcotest.test_case "backfill does not persist resolved secrets" `Quick
      test_backfill_does_not_persist_resolved_secrets;
    Alcotest.test_case "backfill infers default provider" `Quick
      test_backfill_infers_default_provider_from_model_priority;
    Alcotest.test_case "parse codex oauth provider" `Quick
      test_parse_codex_oauth_provider;
    Alcotest.test_case "to_json preserves codex oauth provider" `Quick
      test_to_json_preserves_codex_oauth_provider;
    Alcotest.test_case "parse provider thinking fields" `Quick
      test_parse_provider_thinking_fields;
  ]

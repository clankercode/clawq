let with_temp_file contents f =
  let path = Filename.temp_file "clawq_config" ".json" in
  let oc = open_out path in
  output_string oc contents;
  close_out oc;
  Fun.protect
    (fun () -> f path)
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)

let capture_stderr f =
  let path = Filename.temp_file "clawq_stderr" ".log" in
  let stderr_fd = Unix.descr_of_out_channel stderr in
  let saved_stderr = Unix.dup stderr_fd in
  let capture_fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  Fun.protect
    (fun () ->
      Unix.dup2 capture_fd stderr_fd;
      Unix.close capture_fd;
      f ();
      flush stderr;
      let ic = open_in path in
      Fun.protect
        (fun () -> really_input_string ic (in_channel_length ic))
        ~finally:(fun () -> close_in ic))
    ~finally:(fun () ->
      flush stderr;
      Unix.dup2 saved_stderr stderr_fd;
      Unix.close saved_stderr;
      if Sys.file_exists path then Sys.remove path)

let count_occurrences hay needle =
  let rex = Str.regexp_string needle in
  let rec loop count start =
    try
      let _ = Str.search_forward rex hay start in
      loop (count + 1) (Str.match_end ())
    with Not_found -> count
  in
  loop 0 0

let test_load_warns_on_invalid_port () =
  let json = {|{
      "gateway": {"port": 70000}
    }|} in
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () -> ignore (Config_loader.load ~path ()))
      in
      Alcotest.(check bool)
        "mentions gateway.port" true
        (Test_helpers.string_contains stderr_output "gateway.port"))

let test_load_warns_on_invalid_temperature () =
  let json = {|{
      "default_temperature": 3.5
    }|} in
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () -> ignore (Config_loader.load ~path ()))
      in
      Alcotest.(check bool)
        "mentions default_temperature" true
        (Test_helpers.string_contains stderr_output "default_temperature"))

let test_load_warns_on_negative_temperature () =
  let json = {|{
      "default_temperature": -0.1
    }|} in
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () -> ignore (Config_loader.load ~path ()))
      in
      Alcotest.(check bool)
        "mentions default_temperature" true
        (Test_helpers.string_contains stderr_output "default_temperature"))

let test_load_warns_on_invalid_memory_weights () =
  let json =
    {|{
      "memory": {"vector_weight": 80, "keyword_weight": 40}
    }|}
  in
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () -> ignore (Config_loader.load ~path ()))
      in
      Alcotest.(check bool)
        "mentions memory weights" true
        (Test_helpers.string_contains stderr_output "memory weights"))

let test_load_warns_on_out_of_range_memory_weights () =
  let json =
    {|{
      "memory": {"vector_weight": -10, "keyword_weight": 110}
    }|}
  in
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () -> ignore (Config_loader.load ~path ()))
      in
      Alcotest.(check bool)
        "mentions memory weights" true
        (Test_helpers.string_contains stderr_output "memory weights"))

let test_load_warns_once_per_invalid_field () =
  let json = {|{
      "gateway": {"port": 70000}
    }|} in
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () -> ignore (Config_loader.load ~path ()))
      in
      Alcotest.(check int)
        "one gateway.port warning" 1
        (count_occurrences stderr_output "gateway.port"))

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

let test_parse_non_object_root_uses_agent_defaults_default () =
  let cfg = Config_loader.parse_config (`List []) in
  Alcotest.(check string)
    "fallback primary model" Runtime_config.default.agent_defaults.primary_model
    cfg.agent_defaults.primary_model

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

let test_parse_memory_compaction_threshold_percent () =
  let json =
    Yojson.Safe.from_string
      {|{
        "memory": {"compaction_threshold_percent": 60}
      }|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check int)
    "compaction threshold parsed" 60 cfg.memory.compaction_threshold_percent

let test_parse_invalid_memory_compaction_threshold_percent_uses_default () =
  let json =
    Yojson.Safe.from_string
      {|{
        "memory": {"compaction_threshold_percent": 0}
      }|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check int)
    "invalid compaction threshold defaulted"
    Runtime_config.default.memory.compaction_threshold_percent
    cfg.memory.compaction_threshold_percent

(* B700: the global compaction threshold default is 80% (was 75%). *)
let test_default_compaction_threshold_is_80 () =
  Alcotest.(check int)
    "default compaction threshold" 80
    Runtime_config.default.memory.compaction_threshold_percent;
  Alcotest.(check int)
    "effective default threshold" 80
    (Runtime_config.effective_compaction_threshold_percent
       Runtime_config.default.memory);
  (* A capped 272k model compacts at 80% => 217600 tokens. *)
  let budget =
    match Runtime_config.context_window_for_model "openai-codex:gpt-5.5" with
    | Some w -> w
    | None -> 0
  in
  Alcotest.(check int) "272k * 80% budget" 217600 (budget * 80 / 100)

let test_parse_model_context_limits () =
  let json =
    Yojson.Safe.from_string
      {|{
        "model_context_limits": {
          "openai-codex/gpt-5.4": 272000,
          "custom/model-x": 64000,
          "invalid": 0
        }
      }|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check (list (pair string int)))
    "model context limits parsed"
    [ ("openai-codex/gpt-5.4", 272000); ("custom/model-x", 64000) ]
    cfg.model_context_limits

let test_to_json_omits_empty_providers () =
  let json = Runtime_config.to_json Runtime_config.default in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "empty providers omitted from to_json" true
    (json |> member "providers" = `Null)

let test_to_json_omits_empty_model_context_limits () =
  let json = Runtime_config.to_json Runtime_config.default in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "empty model_context_limits omitted" true
    (json |> member "model_context_limits" = `Null)

let test_to_json_preserves_model_context_limits () =
  let json =
    Runtime_config.to_json
      {
        Runtime_config.default with
        model_context_limits =
          [ ("openai-codex/gpt-5.4", 272000); ("custom/model-x", 64000) ];
      }
  in
  let open Yojson.Safe.Util in
  Alcotest.(check int)
    "gpt-5.4 limit serialized" 272000
    (json
    |> member "model_context_limits"
    |> member "openai-codex/gpt-5.4"
    |> to_int);
  Alcotest.(check int)
    "custom limit serialized" 64000
    (json |> member "model_context_limits" |> member "custom/model-x" |> to_int)

let test_parse_egress_config () =
  let json =
    Yojson.Safe.from_string
      {|{
        "egress": {
          "strictness": "permissive",
          "default_allowlist": [
            {"host": "docs.example.com", "path": "/llms.txt", "method": "GET", "action": "allow", "log_policy": "no_log"}
          ]
        }
      }|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check string)
    "strictness parsed" "permissive"
    (Runtime_config.egress_strictness_to_string cfg.egress.strictness);
  Alcotest.(check int)
    "allowlist count" 1
    (List.length cfg.egress.default_allowlist);
  let rule = List.hd cfg.egress.default_allowlist in
  Alcotest.(check string) "allowlist host" "docs.example.com" rule.host;
  Alcotest.(check (option string)) "allowlist path" (Some "/llms.txt") rule.path;
  Alcotest.(check (option string)) "allowlist method" (Some "GET") rule.method_

let test_parse_egress_default_policy_alias () =
  let json =
    Yojson.Safe.from_string {|{"egress": {"default_policy": "allow"}}|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check string)
    "default_policy allow maps to permissive" "permissive"
    (Runtime_config.egress_strictness_to_string cfg.egress.strictness)

let test_to_json_preserves_egress_config () =
  let cfg =
    {
      Runtime_config.default with
      egress =
        {
          strictness = Runtime_config.Permissive;
          default_allowlist =
            [
              {
                host = "docs.example.com";
                path = Some "/llms-full.txt";
                method_ = Some "GET";
                action = Runtime_config.Allow;
                log_policy = Runtime_config.No_log;
              };
            ];
        };
    }
  in
  let json = Runtime_config.to_json cfg in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "strictness serialized" "permissive"
    (json |> member "egress" |> member "strictness" |> to_string);
  Alcotest.(check string)
    "allowlist host serialized" "docs.example.com"
    (json |> member "egress" |> member "default_allowlist" |> index 0
   |> member "host" |> to_string)

let test_parse_lark_defaults_disabled () =
  let json =
    Yojson.Safe.from_string
      {|{
        "channels": {
          "lark": {
            "app_id": "app-id",
            "app_secret": "secret",
            "verification_token": "vtok"
          }
        }
      }|}
  in
  let cfg = Config_loader.parse_config json in
  match cfg.channels.lark with
  | None -> Alcotest.fail "expected lark config"
  | Some lk -> Alcotest.(check bool) "lark disabled by default" false lk.enabled

let test_agent_defaults_show_tool_calls_defaults_true () =
  let cfg = Config_loader.parse_config (`Assoc []) in
  Alcotest.(check bool)
    "show_tool_calls defaults true" true cfg.agent_defaults.show_tool_calls

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

let test_backfill_does_not_infer_default_provider () =
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
        "default_provider not inferred" None cfg.default_provider;
      let out = Yojson.Safe.from_file path in
      let has_dp =
        match out with
        | `Assoc fields -> List.mem_assoc "default_provider" fields
        | _ -> false
      in
      Alcotest.(check bool) "default_provider not backfilled" false has_dp)

let json_has_default_provider path =
  match Yojson.Safe.from_file path with
  | `Assoc fields -> List.mem_assoc "default_provider" fields
  | _ -> false

let test_backfill_strips_redundant_default_provider () =
  (* B701: when primary_model already names a provider, default_provider is pure
     redundant cruft — it must be dropped from disk so the deprecation warning
     stops firing. Previously merge_json preserved original-only keys, so the
     field (and its warning) persisted forever. *)
  let json =
    {|{
      "default_provider": "zai_coding",
      "providers": {
        "zai_coding": {"api_key": "sk-zai"}
      },
      "agent_defaults": {
        "primary_model": "zai_coding:glm-5"
      }
    }|}
  in
  with_temp_file json (fun path ->
      let cfg = Config_loader.load ~path () in
      Alcotest.(check bool)
        "default_provider dropped from disk" false
        (json_has_default_provider path);
      Alcotest.(check (option string))
        "default_provider absent in parsed config" None cfg.default_provider;
      Alcotest.(check string)
        "primary_model unchanged" "zai_coding:glm-5"
        cfg.agent_defaults.primary_model)

let test_backfill_folds_default_provider_into_bare_model () =
  (* B701: when primary_model is bare, default_provider is the only routing
     signal. It must be folded into the canonical provider:model prefix (not
     silently lost) before the deprecated key is dropped. *)
  let json =
    {|{
      "default_provider": "zai_coding",
      "providers": {
        "zai_coding": {"api_key": "sk-zai"}
      },
      "agent_defaults": {
        "primary_model": "glm-5"
      }
    }|}
  in
  with_temp_file json (fun path ->
      let cfg = Config_loader.load ~path () in
      Alcotest.(check string)
        "provider folded into primary_model" "zai_coding:glm-5"
        cfg.agent_defaults.primary_model;
      Alcotest.(check (option string))
        "default_provider absent in parsed config" None cfg.default_provider;
      Alcotest.(check bool)
        "default_provider dropped from disk" false
        (json_has_default_provider path);
      let out = Yojson.Safe.from_file path in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "canonical primary_model persisted" "zai_coding:glm-5"
        (out |> member "agent_defaults" |> member "primary_model" |> to_string))

let test_backfill_removes_default_provider_from_complete_config () =
  (* B701 regression: even when the rest of config.json is already in complete
     backfilled form (so dropping default_provider is the ONLY change), the key
     must still be written away. The write decision compares the merged result
     against the raw on-disk json, not the migrated form — otherwise a deprecated
     key sitting in an already-complete config would persist forever. *)
  let json =
    {|{
      "providers": { "zai_coding": {"api_key": "sk-zai"} },
      "agent_defaults": { "primary_model": "zai_coding:glm-5" }
    }|}
  in
  with_temp_file json (fun path ->
      (* Load twice so the on-disk file reaches its complete, stable form. *)
      let _ = Config_loader.load ~path () in
      let _ = Config_loader.load ~path () in
      (* Inject default_provider at the END of the now-complete on-disk config. *)
      let with_dp =
        match Yojson.Safe.from_file path with
        | `Assoc fields ->
            `Assoc (fields @ [ ("default_provider", `String "zai_coding") ])
        | other -> other
      in
      let oc = open_out path in
      output_string oc (Yojson.Safe.pretty_to_string ~std:true with_dp);
      close_out oc;
      Alcotest.(check bool)
        "precondition: default_provider present on disk" true
        (json_has_default_provider path);
      (* Reload: default_provider is the only diff, yet must be removed. *)
      let _ = Config_loader.load ~path () in
      Alcotest.(check bool)
        "default_provider removed from disk" false
        (json_has_default_provider path))

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
      Runtime_config.default_provider_config with
      kind = Some "openai-codex";
      base_url = Some "https://chatgpt.com/backend-api/codex";
      default_model = Some "openai-codex/gpt-5-codex";
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

let test_parse_zai_mcp_with_provider_fallback () =
  let json =
    Yojson.Safe.from_string
      {|{
        "providers": {
          "zai": {"api_key": "sk-zai-provider"}
        },
        "zai_mcp": {
          "enabled": true,
          "websearch_enabled": true,
          "webfetch_enabled": false
        }
      }|}
  in
  let cfg = Config_loader.parse_config json in
  match cfg.zai_mcp with
  | None -> Alcotest.fail "expected zai_mcp config"
  | Some zm ->
      Alcotest.(check string)
        "api key falls back to provider" "sk-zai-provider" zm.key;
      Alcotest.(check bool) "websearch enabled" true zm.websearch_enabled;
      Alcotest.(check bool) "webfetch disabled" false zm.webfetch_enabled

let test_backfill_preserves_existing_providers () =
  let json =
    {|{
      "providers": {
        "openai": {"api_key": "sk-test"},
        "anthropic": {"api_key": "sk-ant"}
      }
    }|}
  in
  with_temp_file json (fun path ->
      let _cfg = Config_loader.load ~path () in
      let out = Yojson.Safe.from_file path in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "openai api_key preserved" "sk-test"
        (out |> member "providers" |> member "openai" |> member "api_key"
       |> to_string);
      Alcotest.(check string)
        "anthropic api_key preserved" "sk-ant"
        (out |> member "providers" |> member "anthropic" |> member "api_key"
       |> to_string))

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

let test_parse_log_config () =
  let json =
    Yojson.Safe.from_string
      {|{ "log": { "max_size_mb": 50, "max_files": 10 } }|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check int) "max_size_mb" 50 cfg.log.max_size_mb;
  Alcotest.(check int) "max_files" 10 cfg.log.max_files

let test_parse_log_config_defaults () =
  let json = Yojson.Safe.from_string {|{}|} in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check int) "default max_size_mb" 10 cfg.log.max_size_mb;
  Alcotest.(check int) "default max_files" 5 cfg.log.max_files

let test_parse_heartbeat_config () =
  let json =
    Yojson.Safe.from_string
      {|{
        "heartbeat": {
          "enabled": false,
          "interval_seconds": 250,
          "quiet_start": 22,
          "quiet_end": 7
        }
      }|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check bool) "heartbeat_enabled parsed" false cfg.heartbeat.enabled;
  Alcotest.(check int)
    "heartbeat_interval_seconds parsed" 250 cfg.heartbeat.interval_seconds;
  Alcotest.(check int)
    "heartbeat_quiet_start parsed" 22 cfg.heartbeat.quiet_start;
  Alcotest.(check int) "heartbeat_quiet_end parsed" 7 cfg.heartbeat.quiet_end

let test_migrate_heartbeat_prefixed_keys () =
  (* Old configs may have heartbeat.heartbeat_x keys; migration should rename
     them to the canonical unprefixed form on load. *)
  with_temp_file
    {|{
      "heartbeat": {
        "heartbeat_enabled": false,
        "heartbeat_interval_seconds": 250,
        "heartbeat_quiet_start": 21,
        "heartbeat_quiet_end": 6
      }
    }|}
    (fun path ->
      let cfg = Config_loader.load ~path () in
      Alcotest.(check bool) "enabled migrated" false cfg.heartbeat.enabled;
      Alcotest.(check int)
        "interval_seconds migrated" 250 cfg.heartbeat.interval_seconds;
      Alcotest.(check int) "quiet_start migrated" 21 cfg.heartbeat.quiet_start;
      Alcotest.(check int) "quiet_end migrated" 6 cfg.heartbeat.quiet_end)

let test_to_json_notify_uses_short_keys () =
  let cfg =
    {
      Runtime_config.default with
      notify = Some { Runtime_config.channel = "telegram"; target = "bob" };
    }
  in
  let json = Runtime_config.to_json cfg in
  let open Yojson.Safe.Util in
  let n = json |> member "notify" in
  Alcotest.(check string)
    "notify.channel serialized with short key" "telegram"
    (n |> member "channel" |> to_string);
  Alcotest.(check string)
    "notify.target serialized with short key" "bob"
    (n |> member "target" |> to_string);
  Alcotest.(check bool)
    "notify.notify_channel absent" true
    (n |> member "notify_channel" = `Null);
  Alcotest.(check bool)
    "notify.notify_target absent" true
    (n |> member "notify_target" = `Null)

let test_notify_roundtrip () =
  let cfg =
    {
      Runtime_config.default with
      notify = Some { Runtime_config.channel = "telegram"; target = "alice" };
    }
  in
  let json = Runtime_config.to_json cfg in
  let cfg2 = Config_loader.parse_config json in
  match cfg2.notify with
  | None -> Alcotest.fail "expected notify config after roundtrip"
  | Some nc ->
      Alcotest.(check string) "notify_channel roundtrip" "telegram" nc.channel;
      Alcotest.(check string) "notify_target roundtrip" "alice" nc.target

let test_to_json_summarizer_uses_short_keys () =
  let json = Runtime_config.to_json Runtime_config.default in
  let open Yojson.Safe.Util in
  let s = json |> member "summarizer" in
  Alcotest.(check bool)
    "summarizer.enabled key present" true
    (s |> member "enabled" <> `Null);
  Alcotest.(check bool)
    "summarizer.model key present" true
    (s |> member "model" <> `Null);
  Alcotest.(check bool)
    "summarizer.summarizer_enabled absent" true
    (s |> member "summarizer_enabled" = `Null);
  Alcotest.(check bool)
    "summarizer.summarizer_model absent" true
    (s |> member "summarizer_model" = `Null)

let test_summarizer_roundtrip () =
  let cfg =
    {
      Runtime_config.default with
      summarizer =
        {
          Runtime_config.default_summarizer_config with
          enabled = false;
          threshold_chars = 9999;
        };
    }
  in
  let json = Runtime_config.to_json cfg in
  let cfg2 = Config_loader.parse_config json in
  Alcotest.(check bool)
    "summarizer_enabled roundtrip" false cfg2.summarizer.enabled;
  Alcotest.(check int)
    "threshold_chars roundtrip" 9999 cfg2.summarizer.threshold_chars

let test_summarizer_legacy_keys_compat () =
  (* Legacy JSON uses "summarizer_enabled" and "summarizer_model" keys *)
  let json =
    `Assoc
      [
        ( "summarizer",
          `Assoc
            [
              ("summarizer_enabled", `Bool false);
              ("summarizer_model", `String "openai:gpt-4o-mini");
              ("threshold_chars", `Int 7777);
            ] );
      ]
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check bool) "legacy summarizer_enabled" false cfg.summarizer.enabled;
  Alcotest.(check string)
    "legacy summarizer_model" "openai:gpt-4o-mini"
    (Pmodel.to_string cfg.summarizer.model);
  Alcotest.(check int) "threshold_chars" 7777 cfg.summarizer.threshold_chars

let test_summarizer_canonical_keys_preferred () =
  (* When both canonical and legacy keys are present, canonical wins *)
  let json =
    `Assoc
      [
        ( "summarizer",
          `Assoc
            [
              ("enabled", `Bool true);
              ("summarizer_enabled", `Bool false);
              ("model", `String "openai:gpt-4o");
              ("summarizer_model", `String "openai:gpt-4o-mini");
            ] );
      ]
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check bool) "canonical enabled wins" true cfg.summarizer.enabled;
  Alcotest.(check string)
    "canonical model wins" "openai:gpt-4o"
    (Pmodel.to_string cfg.summarizer.model)

let test_to_json_includes_task_tree_notifications () =
  let cfg =
    {
      Runtime_config.default with
      agent_defaults =
        {
          Runtime_config.default.agent_defaults with
          task_tree_notifications = false;
        };
    }
  in
  let json = Runtime_config.to_json cfg in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "task_tree_notifications serialized" false
    (json |> member "agent_defaults"
    |> member "task_tree_notifications"
    |> to_bool)

let test_task_tree_notifications_roundtrip () =
  let cfg =
    {
      Runtime_config.default with
      agent_defaults =
        {
          Runtime_config.default.agent_defaults with
          task_tree_notifications = false;
        };
    }
  in
  let json = Runtime_config.to_json cfg in
  let cfg2 = Config_loader.parse_config json in
  Alcotest.(check bool)
    "task_tree_notifications roundtrip" false
    cfg2.agent_defaults.task_tree_notifications

let test_max_concurrent_native_agents_roundtrip () =
  let cfg =
    {
      Runtime_config.default with
      agent_defaults =
        {
          Runtime_config.default.agent_defaults with
          max_concurrent_native_agents = Some 3;
        };
    }
  in
  let json = Runtime_config.to_json cfg in
  let cfg2 = Config_loader.parse_config json in
  Alcotest.(check (option int))
    "max_concurrent_native_agents roundtrip" (Some 3)
    cfg2.agent_defaults.max_concurrent_native_agents

let test_max_concurrent_native_agents_null_uses_default () =
  let json =
    Yojson.Safe.from_string
      {|{"agent_defaults": {"max_concurrent_native_agents": null}}|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check (option int))
    "max_concurrent_native_agents null uses default" None
    cfg.agent_defaults.max_concurrent_native_agents

let test_to_json_includes_task_tree_purge_after_days () =
  let cfg =
    {
      Runtime_config.default with
      memory =
        { Runtime_config.default.memory with task_tree_purge_after_days = 14 };
    }
  in
  let json = Runtime_config.to_json cfg in
  let open Yojson.Safe.Util in
  Alcotest.(check int)
    "task_tree_purge_after_days serialized" 14
    (json |> member "memory" |> member "task_tree_purge_after_days" |> to_int)

let test_task_tree_purge_after_days_roundtrip () =
  let cfg =
    {
      Runtime_config.default with
      memory =
        { Runtime_config.default.memory with task_tree_purge_after_days = 7 };
    }
  in
  let json = Runtime_config.to_json cfg in
  let cfg2 = Config_loader.parse_config json in
  Alcotest.(check int)
    "task_tree_purge_after_days roundtrip" 7
    cfg2.memory.task_tree_purge_after_days

let test_to_json_includes_interactive () =
  let json = Runtime_config.to_json Runtime_config.default in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "interactive section present" true
    (json |> member "interactive" <> `Null);
  Alcotest.(check bool)
    "enable_question_notes default serialized" true
    (json |> member "interactive" |> member "enable_question_notes" |> to_bool)

let test_interactive_roundtrip () =
  let cfg =
    {
      Runtime_config.default with
      interactive = { Runtime_config.enable_question_notes = false };
    }
  in
  let json = Runtime_config.to_json cfg in
  let cfg2 = Config_loader.parse_config json in
  Alcotest.(check bool)
    "enable_question_notes roundtrip" false
    cfg2.interactive.enable_question_notes

let test_full_config_roundtrip () =
  (* Build a config with non-default values for every serialized section,
     then verify that to_json → parse_config produces the same values.
     This is a catch-all guard against future to_json/parse_config key drifts. *)
  let cfg =
    {
      Runtime_config.default with
      default_temperature = 0.4;
      agent_defaults =
        {
          Runtime_config.default.agent_defaults with
          primary_model = "anthropic:claude-sonnet-4-6";
          max_tool_iterations = 5;
          task_tree_notifications = false;
          show_thinking = true;
        };
      memory =
        {
          Runtime_config.default.memory with
          max_messages_per_session = 200;
          task_tree_purge_after_days = 30;
          max_message_age_days = 14;
        };
      heartbeat =
        {
          Runtime_config.enabled = false;
          interval_seconds = 120;
          quiet_start = 22;
          quiet_end = 7;
        };
      notify = Some { Runtime_config.channel = "discord"; target = "user123" };
      summarizer =
        {
          Runtime_config.default_summarizer_config with
          enabled = false;
          threshold_chars = 5000;
          max_age_days = 60;
        };
      observer =
        {
          Runtime_config.default_observer_config with
          enabled = false;
          check_every_n_messages = 10;
        };
      interactive = { Runtime_config.enable_question_notes = false };
      debate =
        {
          enabled = false;
          default_models =
            [ "openai-codex:gpt-5.4"; "anthropic:claude-sonnet-4-6" ];
          judge_model = "anthropic:claude-sonnet-4-6";
          max_parallel = 3;
        };
      postmortem =
        {
          enabled = false;
          model = Some "anthropic:claude-haiku-4.5";
          delay_s = 7.5;
        };
    }
  in
  let json = Runtime_config.to_json cfg in
  let cfg2 = Config_loader.parse_config json in
  Alcotest.(check (float 0.001))
    "default_temperature" cfg.default_temperature cfg2.default_temperature;
  Alcotest.(check string)
    "primary_model" cfg.agent_defaults.primary_model
    cfg2.agent_defaults.primary_model;
  Alcotest.(check int)
    "max_tool_iterations" cfg.agent_defaults.max_tool_iterations
    cfg2.agent_defaults.max_tool_iterations;
  Alcotest.(check bool)
    "task_tree_notifications" cfg.agent_defaults.task_tree_notifications
    cfg2.agent_defaults.task_tree_notifications;
  Alcotest.(check bool)
    "show_thinking" cfg.agent_defaults.show_thinking
    cfg2.agent_defaults.show_thinking;
  Alcotest.(check int)
    "memory.max_messages_per_session" cfg.memory.max_messages_per_session
    cfg2.memory.max_messages_per_session;
  Alcotest.(check int)
    "memory.task_tree_purge_after_days" cfg.memory.task_tree_purge_after_days
    cfg2.memory.task_tree_purge_after_days;
  Alcotest.(check int)
    "memory.max_message_age_days" cfg.memory.max_message_age_days
    cfg2.memory.max_message_age_days;
  Alcotest.(check bool)
    "heartbeat.enabled" cfg.heartbeat.enabled cfg2.heartbeat.enabled;
  Alcotest.(check int)
    "heartbeat.interval_seconds" cfg.heartbeat.interval_seconds
    cfg2.heartbeat.interval_seconds;
  Alcotest.(check int)
    "heartbeat.quiet_start" cfg.heartbeat.quiet_start cfg2.heartbeat.quiet_start;
  Alcotest.(check int)
    "heartbeat.quiet_end" cfg.heartbeat.quiet_end cfg2.heartbeat.quiet_end;
  (match cfg2.notify with
  | None -> Alcotest.fail "notify should be present"
  | Some nc ->
      Alcotest.(check string) "notify.channel" "discord" nc.channel;
      Alcotest.(check string) "notify.target" "user123" nc.target);
  Alcotest.(check bool)
    "summarizer.enabled" cfg.summarizer.enabled cfg2.summarizer.enabled;
  Alcotest.(check int)
    "summarizer.threshold_chars" cfg.summarizer.threshold_chars
    cfg2.summarizer.threshold_chars;
  Alcotest.(check int)
    "summarizer.max_age_days" cfg.summarizer.max_age_days
    cfg2.summarizer.max_age_days;
  Alcotest.(check bool)
    "observer.enabled" cfg.observer.enabled cfg2.observer.enabled;
  Alcotest.(check int)
    "observer.check_every_n_messages" cfg.observer.check_every_n_messages
    cfg2.observer.check_every_n_messages;
  Alcotest.(check bool)
    "interactive.enable_question_notes" cfg.interactive.enable_question_notes
    cfg2.interactive.enable_question_notes;
  Alcotest.(check bool) "debate.enabled" cfg.debate.enabled cfg2.debate.enabled;
  Alcotest.(check int)
    "debate.default_models count"
    (List.length cfg.debate.default_models)
    (List.length cfg2.debate.default_models);
  Alcotest.(check string)
    "debate.judge_model" cfg.debate.judge_model cfg2.debate.judge_model;
  Alcotest.(check int)
    "debate.max_parallel" cfg.debate.max_parallel cfg2.debate.max_parallel;
  Alcotest.(check bool)
    "postmortem.enabled" cfg.postmortem.enabled cfg2.postmortem.enabled;
  Alcotest.(check (option string))
    "postmortem.model" cfg.postmortem.model cfg2.postmortem.model;
  Alcotest.(check (float 0.001))
    "postmortem.delay_s" cfg.postmortem.delay_s cfg2.postmortem.delay_s

(* B613: explicit JSON null for postmortem.model means "use primary model"
   (no override). Previously null fell through to the default
   "zai_coding:glm-5-turbo" so once a user set a model they couldn't clear
   the override by writing null. *)
let test_postmortem_model_null_clears_override () =
  let json =
    Yojson.Safe.from_string
      {|{"postmortem": {"enabled": true, "model": null, "delay_s": 0.0}}|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  Alcotest.(check (option string))
    "postmortem.model: null parses as None" None cfg.postmortem.model

let test_postmortem_model_string_preserved () =
  let json =
    Yojson.Safe.from_string
      {|{"postmortem": {"enabled": true, "model": "anthropic:claude-haiku-4-5", "delay_s": 1.0}}|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  Alcotest.(check (option string))
    "postmortem.model: explicit string preserved"
    (Some "anthropic:claude-haiku-4-5") cfg.postmortem.model

let test_postmortem_model_missing_uses_default () =
  (* No postmortem section at all → defaults apply (including default model). *)
  let json = Yojson.Safe.from_string {|{"agent_defaults": {}}|} in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  Alcotest.(check (option string))
    "postmortem.model: missing section → default"
    Runtime_config.default_postmortem_config.model cfg.postmortem.model

let test_room_profiles_default_empty () =
  Alcotest.(check int)
    "default room_profiles empty" 0
    (List.length Runtime_config.default.room_profiles);
  Alcotest.(check int)
    "default access_bundles empty" 0
    (List.length Runtime_config.default.access_bundles);
  Alcotest.(check int)
    "default room_profile_bindings empty" 0
    (List.length Runtime_config.default.room_profile_bindings)

let test_parse_room_profiles () =
  let json =
    Yojson.Safe.from_string
      {|{
        "access_bundles": [
          {
            "id": "bundle-dev",
            "display_name": "Dev bundle",
            "allowed_tools": ["file_read"],
            "denied_tools": ["shell_exec"],
            "codebase_grants": ["$CLAWQ_WORKSPACE/src/**"],
            "mcp_servers": ["fs"],
            "skills": ["review-and-fix"],
            "repositories": ["owner/repo"],
            "domains": ["api.example.com"],
            "credential_handles": ["github-app:main"],
            "instructions": ["Use read-only review mode."],
            "memory_grants": ["scope:42:read"],
            "budget_refs": ["room-dev"]
          }
        ],
        "room_profiles": [
          {"id": "p1", "model": "openai:gpt-4o", "system_prompt": "You are helpful", "max_tool_iterations": 5, "access_bundle_ids": ["bundle-dev"]},
          {"id": "p2", "model": "anthropic:claude-sonnet-4-6"}
        ],
        "room_profile_bindings": [
          {"profile_id": "p1", "room": "general", "active": true},
          {"profile_id": "p2", "room": "dev", "active": false}
        ]
      }|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check int) "access_bundles count" 1 (List.length cfg.access_bundles);
  let bundle = List.nth cfg.access_bundles 0 in
  Alcotest.(check string) "bundle.id" "bundle-dev" bundle.id;
  Alcotest.(check (option string))
    "bundle.display_name" (Some "Dev bundle") bundle.display_name;
  Alcotest.(check (list string))
    "bundle.allowed_tools" [ "file_read" ] bundle.allowed_tools;
  Alcotest.(check (list string))
    "bundle.denied_tools" [ "shell_exec" ] bundle.denied_tools;
  Alcotest.(check (list string))
    "bundle.codebase_grants"
    [ "$CLAWQ_WORKSPACE/src/**" ]
    bundle.codebase_grants;
  Alcotest.(check (list string))
    "bundle.credential_handles are handles only" [ "github-app:main" ]
    bundle.credential_handles;
  Alcotest.(check int) "room_profiles count" 2 (List.length cfg.room_profiles);
  let p1 = List.nth cfg.room_profiles 0 in
  Alcotest.(check string) "p1.id" "p1" p1.id;
  Alcotest.(check string) "p1.model" "openai:gpt-4o" p1.model;
  Alcotest.(check string) "p1.system_prompt" "You are helpful" p1.system_prompt;
  Alcotest.(check int) "p1.max_tool_iterations" 5 p1.max_tool_iterations;
  Alcotest.(check (list string))
    "p1.access_bundle_ids" [ "bundle-dev" ] p1.access_bundle_ids;
  let p2 = List.nth cfg.room_profiles 1 in
  Alcotest.(check string) "p2.id" "p2" p2.id;
  Alcotest.(check string) "p2.system_prompt empty" "" p2.system_prompt;
  Alcotest.(check int)
    "p2.max_tool_iterations default" 10 p2.max_tool_iterations;
  Alcotest.(check int)
    "room_profile_bindings count" 2
    (List.length cfg.room_profile_bindings);
  let b1 = List.nth cfg.room_profile_bindings 0 in
  Alcotest.(check string) "b1.profile_id" "p1" b1.profile_id;
  Alcotest.(check string) "b1.room" "general" b1.room;
  Alcotest.(check bool) "b1.active" true b1.active;
  let b2 = List.nth cfg.room_profile_bindings 1 in
  Alcotest.(check bool) "b2.active default false" false b2.active;
  Alcotest.(check bool)
    "bundle denied tool enforced" true
    (Option.is_some
       (Runtime_config.room_profile_tool_denial_for_session cfg
          ~session_key:"chat:general" ~tool_name:"shell_exec"));
  Alcotest.(check bool)
    "bundle allowed tool permitted" true
    (Option.is_none
       (Runtime_config.room_profile_tool_denial_for_session cfg
          ~session_key:"chat:general" ~tool_name:"file_read"));
  Alcotest.(check bool)
    "bundle allowlist fails closed" true
    (Option.is_some
       (Runtime_config.room_profile_tool_denial_for_session cfg
          ~session_key:"chat:general" ~tool_name:"file_write"))

let test_room_profiles_roundtrip () =
  let cfg =
    {
      Runtime_config.default with
      room_profiles =
        [
          {
            Runtime_config.id = "test-p";
            display_name = None;
            model = "openai:gpt-4o";
            system_prompt = "hello";
            max_tool_iterations = 7;
            status = "active";
            allowed_tools = [];
            denied_tools = [];
            access_bundle_ids = [ "test-bundle" ];
            ambient_enabled = false;
            ambient_quiet_start = 23;
            ambient_quiet_end = 8;
            ambient_rate_limit_rph = 0;
          };
        ];
      access_bundles =
        [
          {
            Runtime_config.id = "test-bundle";
            display_name = None;
            system_prompt = None;
            allowed_tools = [ "file_read" ];
            denied_tools = [ "shell_exec" ];
            codebase_grants = [ "$CLAWQ_WORKSPACE/src/**" ];
            mcp_servers = [];
            skills = [];
            repositories = [];
            repo_grants = [];
            domains = [];
            egress_rules = [];
            credential_handles = [ "github-app:main" ];
            instructions = [];
            memory_grants = [];
            budget_refs = [];
            status = "active";
          };
        ];
      room_profile_bindings =
        [
          {
            Runtime_config.profile_id = "test-p";
            room = "general";
            active = true;
          };
        ];
      room_profile_codebase_grants = [ ("test-p", [ "$CLAWQ_WORKSPACE/**" ]) ];
      access_scopes =
        [
          {
            Runtime_config.id = "room-scope";
            level = Runtime_config.Room;
            workspace = Some "~/src/test";
            channel = Some "slack";
            room = Some "C123";
            access_bundle_ids = [ "test-bundle" ];
            status = "active";
          };
        ];
    }
  in
  let json = Runtime_config.to_json cfg in
  let cfg2 = Config_loader.parse_config json in
  Alcotest.(check int)
    "room_profiles roundtrip" 1
    (List.length cfg2.room_profiles);
  let p = List.nth cfg2.room_profiles 0 in
  Alcotest.(check string) "id roundtrip" "test-p" p.id;
  Alcotest.(check string) "model roundtrip" "openai:gpt-4o" p.model;
  Alcotest.(check (list string))
    "access bundle ids roundtrip" [ "test-bundle" ] p.access_bundle_ids;
  Alcotest.(check int)
    "access bundles roundtrip" 1
    (List.length cfg2.access_bundles);
  Alcotest.(check int)
    "bindings roundtrip" 1
    (List.length cfg2.room_profile_bindings);
  let b = List.nth cfg2.room_profile_bindings 0 in
  Alcotest.(check string) "binding room" "general" b.room;
  Alcotest.(check bool) "binding active" true b.active;
  Alcotest.(check (list string))
    "codebase grants roundtrip"
    [ "$CLAWQ_WORKSPACE/src/**"; "$CLAWQ_WORKSPACE/**" ]
    (Runtime_config.room_profile_codebase_grants_for_profile cfg2
       ~profile_id:"test-p");
  Alcotest.(check int)
    "access scopes roundtrip" 1
    (List.length cfg2.access_scopes);
  let s = List.nth cfg2.access_scopes 0 in
  Alcotest.(check string) "scope id roundtrip" "room-scope" s.id;
  Alcotest.(check string)
    "scope workspace roundtrip" "~/src/test"
    (Option.value ~default:"" s.workspace);
  Alcotest.(check string)
    "scope channel roundtrip" "slack"
    (Option.value ~default:"" s.channel);
  Alcotest.(check string)
    "scope room roundtrip" "C123"
    (Option.value ~default:"" s.room);
  Alcotest.(check (list string))
    "scope bundle ids roundtrip" [ "test-bundle" ] s.access_bundle_ids;
  Alcotest.(check string) "scope status roundtrip" "active" s.status

let test_room_profile_legacy_fields_compile_to_implicit_bundle () =
  let json =
    Yojson.Safe.from_string
      {|{
        "room_profiles": [
          {
            "id": "legacy",
            "model": "openai:gpt-4o",
            "system_prompt": "Legacy prompt",
            "allowed_tools": ["file_read"],
            "denied_tools": ["shell_exec"]
          }
        ],
        "room_profile_codebase_grants": [
          {"profile_id": "legacy", "patterns": ["$CLAWQ_WORKSPACE/src/**"]}
        ]
      }|}
  in
  let cfg = Config_loader.parse_config json in
  let profile = List.nth cfg.room_profiles 0 in
  let bundles = Runtime_config.access_bundles_for_profile cfg profile in
  Alcotest.(check int) "implicit legacy bundle only" 1 (List.length bundles);
  let bundle = List.nth bundles 0 in
  Alcotest.(check string)
    "implicit bundle id" "__legacy_room_profile:legacy" bundle.id;
  Alcotest.(check (option string))
    "legacy system prompt carried" (Some "Legacy prompt") bundle.system_prompt;
  Alcotest.(check (list string))
    "legacy allowed tools carried" [ "file_read" ] bundle.allowed_tools;
  Alcotest.(check (list string))
    "legacy denied tools carried" [ "shell_exec" ] bundle.denied_tools;
  Alcotest.(check (list string))
    "legacy codebase grants carried"
    [ "$CLAWQ_WORKSPACE/src/**" ]
    bundle.codebase_grants;
  Alcotest.(check (list string))
    "legacy grants still enforce"
    [ "$CLAWQ_WORKSPACE/src/**" ]
    (Runtime_config.room_profile_codebase_grants_for_profile cfg
       ~profile_id:"legacy");
  Alcotest.(check bool)
    "legacy denied tool still denied" true
    (Option.is_some
       (Runtime_config.room_profile_tool_denial profile ~tool_name:"shell_exec"))

let test_room_profiles_to_json_omits_empty () =
  let json = Runtime_config.to_json Runtime_config.default in
  Alcotest.(check bool)
    "empty access_bundles omitted" true
    (Yojson.Safe.Util.member "access_bundles" json = `Null);
  Alcotest.(check bool)
    "empty room_profiles omitted" true
    (Yojson.Safe.Util.member "room_profiles" json = `Null);
  Alcotest.(check bool)
    "empty room_profile_bindings omitted" true
    (Yojson.Safe.Util.member "room_profile_bindings" json = `Null)

let test_room_profiles_duplicate_ids_rejects () =
  let json =
    {|{
      "room_profiles": [
        {"id": "dup", "model": "openai:gpt-4o"},
        {"id": "dup", "model": "anthropic:claude-sonnet-4-6"}
      ]
    }|}
  in
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () ->
            let cfg = Config_loader.load ~path () in
            Alcotest.(check int)
              "room_profiles preserved" 2
              (List.length cfg.room_profiles))
      in
      Alcotest.(check bool)
        "warns about duplicate profile id" true
        (Test_helpers.string_contains stderr_output "duplicate profile id"))

let test_room_profiles_duplicate_active_bindings_rejects () =
  let json =
    {|{
      "room_profiles": [
        {"id": "p1", "model": "openai:gpt-4o"},
        {"id": "p2", "model": "anthropic:claude-sonnet-4-6"}
      ],
      "room_profile_bindings": [
        {"profile_id": "p1", "room": "general", "active": true},
        {"profile_id": "p2", "room": "general", "active": true}
      ]
    }|}
  in
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () ->
            let cfg = Config_loader.load ~path () in
            Alcotest.(check int)
              "room_profiles preserved" 2
              (List.length cfg.room_profiles);
            Alcotest.(check int)
              "room_profile_bindings preserved" 2
              (List.length cfg.room_profile_bindings);
            Alcotest.(check bool)
              "duplicate binding denies profile-scoped tool" true
              (Option.is_some
                 (Runtime_config.room_profile_tool_denial_for_session cfg
                    ~session_key:"chat:general" ~tool_name:"file_read")))
      in
      Alcotest.(check bool)
        "warns about duplicate active room binding" true
        (Test_helpers.string_contains stderr_output
           "duplicate active binding for room"))

let test_room_profiles_multi_room_binding_rejects () =
  let json =
    {|{
      "room_profiles": [
        {"id": "p1", "model": "openai:gpt-4o"}
      ],
      "room_profile_bindings": [
        {"profile_id": "p1", "room": "general", "active": true},
        {"profile_id": "p1", "room": "dev", "active": true}
      ]
    }|}
  in
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () ->
            let cfg = Config_loader.load ~path () in
            Alcotest.(check int)
              "room_profiles preserved" 1
              (List.length cfg.room_profiles);
            Alcotest.(check int)
              "room_profile_bindings preserved" 2
              (List.length cfg.room_profile_bindings);
            Alcotest.(check bool)
              "multi-room binding denies profile-scoped tool" true
              (Option.is_some
                 (Runtime_config.room_profile_tool_denial_for_session cfg
                    ~session_key:"chat:general" ~tool_name:"file_read")))
      in
      Alcotest.(check bool)
        "warns about multi-room binding" true
        (Test_helpers.string_contains stderr_output "bound to multiple rooms"))

let test_room_profile_binding_active_default_true () =
  let json =
    {|{
      "room_profile_bindings": [
        {"profile_id": "p1", "room": "general"}
      ]
    }|}
  in
  let cfg = Config_loader.parse_config (Yojson.Safe.from_string json) in
  Alcotest.(check int)
    "bindings count" 1
    (List.length cfg.room_profile_bindings);
  let b = List.nth cfg.room_profile_bindings 0 in
  Alcotest.(check bool) "active defaults to true" true b.active

let test_room_profile_orphan_binding_rejects () =
  let json =
    {|{
      "room_profiles": [
        {"id": "p1", "model": "openai:gpt-4o"}
      ],
      "room_profile_bindings": [
        {"profile_id": "ghost", "room": "general", "active": true}
      ]
    }|}
  in
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () ->
            let cfg = Config_loader.load ~path () in
            Alcotest.(check int)
              "room_profiles plus locked missing profile preserved" 2
              (List.length cfg.room_profiles);
            Alcotest.(check int)
              "room_profile_bindings preserved" 1
              (List.length cfg.room_profile_bindings);
            Alcotest.(check bool)
              "orphan binding denies profile-scoped tool" true
              (Option.is_some
                 (Runtime_config.room_profile_tool_denial_for_session cfg
                    ~session_key:"chat:general" ~tool_name:"file_read")))
      in
      Alcotest.(check bool)
        "warns about non-existent profile" true
        (Test_helpers.string_contains stderr_output "non-existent profile"))

let test_room_profile_invalid_access_bundle_reference_rejects () =
  let json =
    {|{
      "access_bundles": [
        {"id": "known", "allowed_tools": ["file_read"]}
      ],
      "room_profiles": [
        {"id": "p1", "model": "openai:gpt-4o", "access_bundle_ids": ["missing"]}
      ],
      "room_profile_bindings": [
        {"profile_id": "p1", "room": "general", "active": true}
      ]
    }|}
  in
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () ->
            let cfg = Config_loader.load ~path () in
            Alcotest.(check int)
              "room_profiles preserved" 1
              (List.length cfg.room_profiles);
            Alcotest.(check int)
              "room_profile_bindings preserved" 1
              (List.length cfg.room_profile_bindings);
            Alcotest.(check bool)
              "missing bundle reference denies profile-scoped tool" true
              (Option.is_some
                 (Runtime_config.room_profile_tool_denial_for_session cfg
                    ~session_key:"chat:general" ~tool_name:"file_read")))
      in
      Alcotest.(check bool)
        "warns about missing access bundle" true
        (Test_helpers.string_contains stderr_output
           "references non-existent access bundle"))

let assert_access_policy_fail_closed json ~warning_substring =
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () ->
            let cfg = Config_loader.load ~path () in
            Alcotest.(check int)
              "room_profiles preserved" 1
              (List.length cfg.room_profiles);
            Alcotest.(check int)
              "room_profile_bindings preserved" 1
              (List.length cfg.room_profile_bindings);
            Alcotest.(check bool)
              "invalid access policy denies profile-scoped tool" true
              (Option.is_some
                 (Runtime_config.room_profile_tool_denial_for_session cfg
                    ~session_key:"chat:general" ~tool_name:"file_read")))
      in
      Alcotest.(check bool)
        "warns about access policy validation" true
        (Test_helpers.string_contains stderr_output warning_substring))

let test_access_bundle_malformed_security_fields_reject () =
  let fields =
    [
      "allowed_tools";
      "denied_tools";
      "codebase_grants";
      "mcp_servers";
      "skills";
      "repositories";
      "domains";
      "credential_handles";
      "instructions";
      "memory_grants";
      "budget_refs";
    ]
  in
  List.iter
    (fun field ->
      let json =
        Printf.sprintf
          {|{
            "access_bundles": [
              {"id": "bundle-dev", "%s": [42]}
            ],
            "room_profiles": [
              {"id": "p1", "model": "openai:gpt-4o", "access_bundle_ids": ["bundle-dev"]}
            ],
            "room_profile_bindings": [
              {"profile_id": "p1", "room": "general", "active": true}
            ]
          }|}
          field
      in
      assert_access_policy_fail_closed json
        ~warning_substring:("access_bundles[0]." ^ field))
    fields

let test_room_profile_malformed_access_bundle_ids_rejects () =
  let json =
    {|{
      "access_bundles": [
        {"id": "known", "allowed_tools": ["file_read"]}
      ],
      "room_profiles": [
        {"id": "p1", "model": "openai:gpt-4o", "access_bundle_ids": {"id": "known"}}
      ],
      "room_profile_bindings": [
        {"profile_id": "p1", "room": "general", "active": true}
      ]
    }|}
  in
  assert_access_policy_fail_closed json
    ~warning_substring:"room_profiles[0].access_bundle_ids"

let test_access_bundle_duplicate_ids_rejects () =
  let json =
    {|{
      "access_bundles": [
        {"id": "dup", "allowed_tools": ["file_read"]},
        {"id": "dup", "allowed_tools": ["file_write"]}
      ],
      "room_profiles": [
        {"id": "p1", "model": "openai:gpt-4o", "access_bundle_ids": ["dup"]}
      ],
      "room_profile_bindings": [
        {"profile_id": "p1", "room": "general", "active": true}
      ]
    }|}
  in
  assert_access_policy_fail_closed json ~warning_substring:"duplicate bundle id"

let test_access_scope_invalid_access_bundle_reference_rejects () =
  let json =
    {|{
      "access_bundles": [
        {"id": "known", "allowed_tools": ["file_read"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["missing"]}
      ]
    }|}
  in
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () ->
            let cfg = Config_loader.load ~path () in
            Alcotest.(check int)
              "access scopes disabled" 0
              (List.length cfg.access_scopes);
            let effective =
              Runtime_config.resolve_effective_access cfg
                ~session_key:"chat:general" ()
            in
            Alcotest.(check (list string))
              "invalid scope grants no tools" []
              (List.map
                 (fun (item : Runtime_config.effective_access_item) ->
                   item.value)
                 effective.allowed_tools))
      in
      Alcotest.(check bool)
        "warns about missing scope access bundle" true
        (Test_helpers.string_contains stderr_output
           "access_scopes: scope 'default' references non-existent access \
            bundle"))

let test_access_scope_malformed_access_bundle_ids_rejects () =
  let json =
    {|{
      "access_bundles": [
        {"id": "known", "allowed_tools": ["file_read"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": {"id": "known"}}
      ]
    }|}
  in
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () ->
            let cfg = Config_loader.load ~path () in
            Alcotest.(check int)
              "malformed access scopes disabled" 0
              (List.length cfg.access_scopes))
      in
      Alcotest.(check bool)
        "warns about malformed scope access_bundle_ids" true
        (Test_helpers.string_contains stderr_output
           "access_scopes[0].access_bundle_ids"))

let test_access_scope_invalid_level_rejects () =
  let json =
    {|{
      "access_bundles": [
        {"id": "known", "allowed_tools": ["file_read"]}
      ],
      "access_scopes": [
        {"id": "bad", "level": "planet", "access_bundle_ids": ["known"]}
      ]
    }|}
  in
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () ->
            let cfg = Config_loader.load ~path () in
            Alcotest.(check int)
              "invalid level disables scopes" 0
              (List.length cfg.access_scopes))
      in
      Alcotest.(check bool)
        "warns about invalid scope level" true
        (Test_helpers.string_contains stderr_output "access_scopes[0].level"))

let test_access_scope_missing_required_selector_rejects () =
  let json =
    {|{
      "access_bundles": [
        {"id": "known", "allowed_tools": ["file_read"]}
      ],
      "access_scopes": [
        {"id": "room-missing", "level": "room", "access_bundle_ids": ["known"]}
      ]
    }|}
  in
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () ->
            let cfg = Config_loader.load ~path () in
            Alcotest.(check int)
              "missing selector disables scopes" 0
              (List.length cfg.access_scopes))
      in
      Alcotest.(check bool)
        "warns about missing room selector" true
        (Test_helpers.string_contains stderr_output
           "scope 'room-missing' must set room"))

let test_access_scope_malformed_selector_rejects () =
  let json =
    {|{
      "access_bundles": [
        {"id": "known", "allowed_tools": ["file_read"]}
      ],
      "access_scopes": [
        {"id": "bad-default", "level": "default", "workspace": 42, "access_bundle_ids": ["known"]}
      ]
    }|}
  in
  with_temp_file json (fun path ->
      let stderr_output =
        capture_stderr (fun () ->
            let cfg = Config_loader.load ~path () in
            Alcotest.(check int)
              "malformed selector disables scopes" 0
              (List.length cfg.access_scopes);
            let effective =
              Runtime_config.resolve_effective_access cfg
                ~session_key:"slack:C123" ()
            in
            Alcotest.(check (list string))
              "malformed selector grants no tools" []
              (List.map
                 (fun (item : Runtime_config.effective_access_item) ->
                   item.value)
                 effective.allowed_tools))
      in
      Alcotest.(check bool)
        "warns about malformed workspace selector" true
        (Test_helpers.string_contains stderr_output "access_scopes[0].workspace"))

let test_instruction_records_roundtrip () =
  let json =
    Yojson.Safe.from_string
      {|{"access_bundles": [{"id": "t", "instructions": [
          {"text": "Be helpful", "source_scope": "workspace",
           "author": "admin", "edit_policy": "admin_only"},
          "legacy"]}]}|}
  in
  let cfg = Config_loader.parse_config json in
  let bundle = List.hd cfg.access_bundles in
  Alcotest.(check int) "count" 2 (List.length bundle.instructions);
  let ir1 = List.nth bundle.instructions 0 in
  Alcotest.(check string) "text" "Be helpful" ir1.text;
  Alcotest.(check string) "scope" "workspace" ir1.source_scope;
  Alcotest.(check (option string)) "author" (Some "admin") ir1.author;
  Alcotest.(check bool) "locked" true ir1.locked;
  let ir2 = List.nth bundle.instructions 1 in
  Alcotest.(check string) "legacy" "legacy" ir2.text;
  Alcotest.(check bool) "legacy locked" false ir2.locked;
  let reparsed = Config_loader.parse_config (Runtime_config.to_json cfg) in
  let ir1' = List.nth (List.hd reparsed.access_bundles).instructions 0 in
  Alcotest.(check string) "rt text" "Be helpful" ir1'.text;
  Alcotest.(check string) "rt scope" "workspace" ir1'.source_scope;
  (match ir1'.edit_policy with
  | Runtime_config.Admin_only -> ()
  | _ -> Alcotest.fail "expected Admin_only");
  let bad =
    Yojson.Safe.from_string
      {|{"access_bundles": [{"id": "b", "instructions": [{"text": ""}]}]}|}
  in
  let issues = Config_loader_support.validate_access_bundle_json_shapes bad in
  Alcotest.(check bool)
    "empty text rejected" true
    (List.exists
       (fun s -> Test_helpers.string_contains s "text must not be empty")
       issues)

let suite =
  [
    Alcotest.test_case "load warns on invalid port" `Quick
      test_load_warns_on_invalid_port;
    Alcotest.test_case "load warns on invalid temperature" `Quick
      test_load_warns_on_invalid_temperature;
    Alcotest.test_case "load warns on negative temperature" `Quick
      test_load_warns_on_negative_temperature;
    Alcotest.test_case "load warns on invalid memory weights" `Quick
      test_load_warns_on_invalid_memory_weights;
    Alcotest.test_case "load warns on out-of-range memory weights" `Quick
      test_load_warns_on_out_of_range_memory_weights;
    Alcotest.test_case "load warns once per invalid field" `Quick
      test_load_warns_once_per_invalid_field;
    Alcotest.test_case "load backfill preserves unknown keys" `Quick
      test_load_backfill_preserves_unknown_keys;
    Alcotest.test_case "provider env secret resolution" `Quick
      test_parse_provider_env_secret_resolution;
    Alcotest.test_case "telegram env secret resolution" `Quick
      test_parse_telegram_env_secret_resolution;
    Alcotest.test_case "load invalid json returns default" `Quick
      test_load_invalid_json_returns_default;
    Alcotest.test_case "parse non-object root uses agent defaults default"
      `Quick test_parse_non_object_root_uses_agent_defaults_default;
    Alcotest.test_case "parse legacy nested paths" `Quick
      test_parse_legacy_nested_paths;
    Alcotest.test_case "backfill replaces type mismatch" `Quick
      test_backfill_replaces_type_mismatch_with_defaults;
    Alcotest.test_case "parse gateway auth token" `Quick
      test_parse_gateway_auth_token;
    Alcotest.test_case "parse memory compaction threshold percent" `Quick
      test_parse_memory_compaction_threshold_percent;
    Alcotest.test_case
      "parse invalid memory compaction threshold percent uses default" `Quick
      test_parse_invalid_memory_compaction_threshold_percent_uses_default;
    Alcotest.test_case "default compaction threshold is 80" `Quick
      test_default_compaction_threshold_is_80;
    Alcotest.test_case "parse model context limits" `Quick
      test_parse_model_context_limits;
    Alcotest.test_case "to_json omits empty providers" `Quick
      test_to_json_omits_empty_providers;
    Alcotest.test_case "to_json omits empty model context limits" `Quick
      test_to_json_omits_empty_model_context_limits;
    Alcotest.test_case "to_json preserves model context limits" `Quick
      test_to_json_preserves_model_context_limits;
    Alcotest.test_case "parse egress config" `Quick test_parse_egress_config;
    Alcotest.test_case "parse egress default_policy alias" `Quick
      test_parse_egress_default_policy_alias;
    Alcotest.test_case "to_json preserves egress config" `Quick
      test_to_json_preserves_egress_config;
    Alcotest.test_case "parse lark defaults disabled" `Quick
      test_parse_lark_defaults_disabled;
    Alcotest.test_case "agent defaults show_tool_calls defaults true" `Quick
      test_agent_defaults_show_tool_calls_defaults_true;
    Alcotest.test_case "backfill does not persist resolved secrets" `Quick
      test_backfill_does_not_persist_resolved_secrets;
    Alcotest.test_case "backfill does not infer default provider" `Quick
      test_backfill_does_not_infer_default_provider;
    Alcotest.test_case "backfill strips redundant default provider" `Quick
      test_backfill_strips_redundant_default_provider;
    Alcotest.test_case "backfill folds default provider into bare model" `Quick
      test_backfill_folds_default_provider_into_bare_model;
    Alcotest.test_case "backfill removes default provider from complete config"
      `Quick test_backfill_removes_default_provider_from_complete_config;
    Alcotest.test_case "parse codex oauth provider" `Quick
      test_parse_codex_oauth_provider;
    Alcotest.test_case "to_json preserves codex oauth provider" `Quick
      test_to_json_preserves_codex_oauth_provider;
    Alcotest.test_case "parse zai mcp with provider fallback" `Quick
      test_parse_zai_mcp_with_provider_fallback;
    Alcotest.test_case "backfill preserves existing providers" `Quick
      test_backfill_preserves_existing_providers;
    Alcotest.test_case "parse provider thinking fields" `Quick
      test_parse_provider_thinking_fields;
    Alcotest.test_case "parse log config" `Quick test_parse_log_config;
    Alcotest.test_case "parse log config defaults" `Quick
      test_parse_log_config_defaults;
    Alcotest.test_case "parse heartbeat config" `Quick
      test_parse_heartbeat_config;
    Alcotest.test_case "migrate heartbeat prefixed keys" `Quick
      test_migrate_heartbeat_prefixed_keys;
    Alcotest.test_case "to_json notify uses short keys" `Quick
      test_to_json_notify_uses_short_keys;
    Alcotest.test_case "notify roundtrip" `Quick test_notify_roundtrip;
    Alcotest.test_case "to_json summarizer uses short keys" `Quick
      test_to_json_summarizer_uses_short_keys;
    Alcotest.test_case "summarizer roundtrip" `Quick test_summarizer_roundtrip;
    Alcotest.test_case "summarizer legacy keys compat" `Quick
      test_summarizer_legacy_keys_compat;
    Alcotest.test_case "summarizer canonical keys preferred" `Quick
      test_summarizer_canonical_keys_preferred;
    Alcotest.test_case "to_json includes task_tree_notifications" `Quick
      test_to_json_includes_task_tree_notifications;
    Alcotest.test_case "task_tree_notifications roundtrip" `Quick
      test_task_tree_notifications_roundtrip;
    Alcotest.test_case "max_concurrent_native_agents roundtrip" `Quick
      test_max_concurrent_native_agents_roundtrip;
    Alcotest.test_case "max_concurrent_native_agents null uses default" `Quick
      test_max_concurrent_native_agents_null_uses_default;
    Alcotest.test_case "to_json includes task_tree_purge_after_days" `Quick
      test_to_json_includes_task_tree_purge_after_days;
    Alcotest.test_case "task_tree_purge_after_days roundtrip" `Quick
      test_task_tree_purge_after_days_roundtrip;
    Alcotest.test_case "to_json includes interactive section" `Quick
      test_to_json_includes_interactive;
    Alcotest.test_case "interactive roundtrip" `Quick test_interactive_roundtrip;
    Alcotest.test_case "full config roundtrip" `Quick test_full_config_roundtrip;
    Alcotest.test_case "B613: postmortem.model null clears override" `Quick
      test_postmortem_model_null_clears_override;
    Alcotest.test_case "B613: postmortem.model string preserved" `Quick
      test_postmortem_model_string_preserved;
    Alcotest.test_case "B613: postmortem.model missing uses default" `Quick
      test_postmortem_model_missing_uses_default;
    Alcotest.test_case "room_profiles default empty" `Quick
      test_room_profiles_default_empty;
    Alcotest.test_case "parse room profiles" `Quick test_parse_room_profiles;
    Alcotest.test_case "room_profiles roundtrip" `Quick
      test_room_profiles_roundtrip;
    Alcotest.test_case "legacy profile fields compile to implicit bundle" `Quick
      test_room_profile_legacy_fields_compile_to_implicit_bundle;
    Alcotest.test_case "to_json omits empty room profiles" `Quick
      test_room_profiles_to_json_omits_empty;
    Alcotest.test_case "room_profiles duplicate ids rejects" `Quick
      test_room_profiles_duplicate_ids_rejects;
    Alcotest.test_case "room_profiles duplicate active bindings rejects" `Quick
      test_room_profiles_duplicate_active_bindings_rejects;
    Alcotest.test_case "room_profiles multi-room binding rejects" `Quick
      test_room_profiles_multi_room_binding_rejects;
    Alcotest.test_case "room_profile binding active default true" `Quick
      test_room_profile_binding_active_default_true;
    Alcotest.test_case "room_profiles orphan binding rejects" `Quick
      test_room_profile_orphan_binding_rejects;
    Alcotest.test_case "room_profiles invalid access bundle rejects" `Quick
      test_room_profile_invalid_access_bundle_reference_rejects;
    Alcotest.test_case "room_profiles malformed access_bundle_ids rejects"
      `Quick test_room_profile_malformed_access_bundle_ids_rejects;
    Alcotest.test_case "access_bundles malformed security fields reject" `Quick
      test_access_bundle_malformed_security_fields_reject;
    Alcotest.test_case "access_bundles duplicate ids reject" `Quick
      test_access_bundle_duplicate_ids_rejects;
    Alcotest.test_case "access_scopes invalid access bundle rejects" `Quick
      test_access_scope_invalid_access_bundle_reference_rejects;
    Alcotest.test_case "access_scopes malformed access_bundle_ids rejects"
      `Quick test_access_scope_malformed_access_bundle_ids_rejects;
    Alcotest.test_case "access_scopes invalid level rejects" `Quick
      test_access_scope_invalid_level_rejects;
    Alcotest.test_case "access_scopes missing selector rejects" `Quick
      test_access_scope_missing_required_selector_rejects;
    Alcotest.test_case "access_scopes malformed selector rejects" `Quick
      test_access_scope_malformed_selector_rejects;
    Alcotest.test_case "default_path returns config.json path" `Quick (fun () ->
        let path = Config_loader.default_path () in
        Alcotest.(check bool)
          "ends with config.json" true
          (Filename.basename path = "config.json"));
    Alcotest.test_case "session update_config propagates to get_config" `Quick
      (fun () ->
        let cfg1 = Runtime_config.default in
        let mgr = Session.create ~config:cfg1 () in
        Alcotest.(check (float 0.001))
          "initial temperature" cfg1.default_temperature
          (Session.get_config mgr).default_temperature;
        let cfg2 = { cfg1 with default_temperature = 0.42 } in
        Session.update_config mgr cfg2;
        Alcotest.(check (float 0.001))
          "updated temperature" 0.42
          (Session.get_config mgr).default_temperature);
    Alcotest.test_case "load_result returns Ok for valid config" `Quick
      (fun () ->
        with_temp_file {|{"default_temperature": 0.5}|} (fun path ->
            match Config_loader.load_result ~path () with
            | Ok cfg ->
                Alcotest.(check (float 0.001))
                  "temperature parsed" 0.5 cfg.default_temperature
            | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)));
    Alcotest.test_case "load_result returns Error for malformed JSON" `Quick
      (fun () ->
        with_temp_file {|{this is not valid json|} (fun path ->
            match Config_loader.load_result ~path () with
            | Ok _ -> Alcotest.fail "expected Error for malformed JSON"
            | Error msg ->
                Alcotest.(check bool)
                  "error mentions parse failure" true
                  (try
                     ignore
                       (Str.search_forward
                          (Str.regexp_string "Failed to parse")
                          msg 0);
                     true
                   with Not_found -> false)));
    Alcotest.test_case "load_result returns Ok defaults for missing file" `Quick
      (fun () ->
        let path =
          "/tmp/clawq_missing_config_" ^ string_of_int (Unix.getpid ())
        in
        (try Sys.remove path with _ -> ());
        match Config_loader.load_result ~path () with
        | Ok _cfg -> ()
        | Error msg ->
            Alcotest.fail
              ("expected Ok defaults for missing file, got error: " ^ msg));
    Alcotest.test_case "load_result returns Error for unreadable config file"
      `Quick (fun () ->
        with_temp_file {|{"default_temperature": 0.5}|} (fun path ->
            Unix.chmod path 0o000;
            Fun.protect
              ~finally:(fun () -> Unix.chmod path 0o644)
              (fun () ->
                match Config_loader.load_result ~path () with
                | Ok _ ->
                    Alcotest.fail "expected Error for unreadable config file"
                | Error msg ->
                    Alcotest.(check bool)
                      "error mentions parse failure" true
                      (try
                         ignore
                           (Str.search_forward
                              (Str.regexp_string "Failed to parse")
                              msg 0);
                         true
                       with Not_found -> false))));
    Alcotest.test_case "instruction records roundtrip and validation" `Quick
      test_instruction_records_roundtrip;
  ]

let with_temp_file contents f =
  let path = Filename.temp_file "clawq_config" ".json" in
  let oc = open_out path in
  output_string oc contents;
  close_out oc;
  Fun.protect
    (fun () -> f path)
    ~finally:(fun () ->
      if Sys.file_exists path then
        Sys.remove path)

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
      Alcotest.(check int) "unknown key preserved" 1 (out |> member "foo" |> to_int);
      Alcotest.(check string) "existing key not clobbered" "x-model"
        (out |> member "agent_defaults" |> member "primary_model" |> to_string);
      Alcotest.(check bool) "defaults backfilled" true
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
    Alcotest.(check string) "resolved bot token" "bot-token-from-env" acct.bot_token

let test_load_invalid_json_returns_default () =
  with_temp_file "{ invalid json" (fun path ->
      let cfg = Config_loader.load ~path () in
      Alcotest.(check string) "fallback primary model"
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
  Alcotest.(check bool) "memory.search.enabled parsed" true cfg.memory.search_enabled;
  Alcotest.(check bool) "security.audit.enabled parsed" true cfg.security.audit_enabled;
  Alcotest.(check bool) "security.tools.enabled parsed" false cfg.security.tools_enabled;
  Alcotest.(check bool) "autonomy.workspace_only parsed" false cfg.security.workspace_only

let test_backfill_replaces_type_mismatch_with_defaults () =
  let json =
    {|{
      "memory": "oops",
      "security": "bad"
    }|}
  in
  with_temp_file json (fun path ->
      let cfg = Config_loader.load ~path () in
      Alcotest.(check string) "memory backend defaulted" Runtime_config.default.memory.backend cfg.memory.backend;
      let out = Yojson.Safe.from_file path in
      let open Yojson.Safe.Util in
      Alcotest.(check bool) "memory backfilled as object" true
        (out |> member "memory" |> member "backend" <> `Null);
      Alcotest.(check bool) "security backfilled as object" true
        (out |> member "security" |> member "workspace_only" <> `Null))

let test_parse_gateway_auth_token () =
  let json =
    Yojson.Safe.from_string
      {|{
        "gateway": {"auth_token": "abc123"}
      }|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check (option string)) "gateway auth token parsed" (Some "abc123")
    cfg.gateway.auth_token

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
      Alcotest.(check string) "runtime config resolves secret" "sk-live-secret" key;
      let out = Yojson.Safe.from_file path in
      let open Yojson.Safe.Util in
      Alcotest.(check string) "file keeps env placeholder" "$CLAWQ_TEST_SECRET_BACKFILL"
        (out |> member "providers" |> member "p" |> member "api_key" |> to_string))

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
  ]

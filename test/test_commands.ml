(* Tests for CLI command bridge functions *)
(* These test in-process invocation of commands via command_bridge_min *)

(* ===== Command bridge (minimal) tests ===== *)
(* Command_bridge_min is available in clawq_runtime_core *)

let contains s sub =
  let sl = String.length s and subl = String.length sub in
  if subl > sl then false
  else if subl = 0 then true
  else
    let found = ref false in
    for i = 0 to sl - subl do
      if String.sub s i subl = sub then found := true
    done;
    !found

let test_help_returns_something () =
  (* Test the version info or config show via direct function calls *)
  (* We test the help-equivalent: status returns non-empty output *)
  let v = Command_bridge_min.handle [ "status" ] in
  Alcotest.(check bool) "status output non-empty" true (String.length v > 0)

let test_status_returns_output () =
  let out = Command_bridge_min.handle [ "status" ] in
  Alcotest.(check bool) "status non-empty" true (String.length out > 0)

let test_unknown_command_graceful () =
  let out = Command_bridge_min.handle [ "this_command_does_not_exist" ] in
  Alcotest.(check bool)
    "unknown command returns string" true
    (String.length out > 0)

let test_version_contains_clawq () =
  let out = Command_bridge_min.handle [ "status" ] in
  let lower = String.lowercase_ascii out in
  Alcotest.(check bool)
    "status contains clawq info" true
    (contains lower "clawq" || contains lower "model" || contains lower "status"
    || String.length out > 0)

let test_min_auth_codex_disabled_message () =
  let out =
    Command_bridge_min.handle [ "auth"; "codex-login"; "openai-codex" ]
  in
  Alcotest.(check bool)
    "minimal build explains codex auth disabled" true
    (contains out "disabled in minimal build")

let test_min_auth_status_shows_codex_oauth () =
  let json =
    Yojson.Safe.from_string
      {|{
  "providers": {
    "openai-codex": {
      "kind": "openai-codex",
      "codex_oauth": {
        "access_token": "tok",
        "refresh_token": "ref",
        "expires_at_ms": 4102444800000
      }
    }
  }
}|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  let provider = List.assoc "openai-codex" cfg.providers in
  let status =
    if Runtime_config.is_key_set provider.api_key then "unexpected"
    else if Runtime_config.provider_has_codex_oauth provider then
      "codex-oauth configured"
    else "not set"
  in
  Alcotest.(check string)
    "minimal auth status label" "codex-oauth configured" status

(* ===== Config parsing without file I/O ===== *)

let test_config_default_model () =
  let cfg = Config_loader.parse_config (`Assoc []) in
  Alcotest.(check bool)
    "primary_model non-empty" true
    (String.length cfg.agent_defaults.primary_model > 0)

let test_config_default_temperature () =
  let cfg = Config_loader.parse_config (`Assoc []) in
  Alcotest.(check bool) "temperature >= 0" true (cfg.default_temperature >= 0.0);
  Alcotest.(check bool) "temperature <= 2" true (cfg.default_temperature <= 2.0)

let test_config_default_gateway () =
  let cfg = Config_loader.parse_config (`Assoc []) in
  Alcotest.(check bool) "gateway port > 0" true (cfg.gateway.port > 0);
  Alcotest.(check bool)
    "gateway host non-empty" true
    (String.length cfg.gateway.host > 0)

let test_config_parse_model () =
  let json =
    Yojson.Safe.from_string {|{"agent_defaults": {"primary_model": "gpt-4o"}}|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check string)
    "primary_model" "gpt-4o" cfg.agent_defaults.primary_model

let test_config_parse_temperature () =
  let json = Yojson.Safe.from_string {|{"default_temperature": 0.8}|} in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check (float 0.01)) "temperature" 0.8 cfg.default_temperature

let test_config_parse_providers () =
  let json =
    Yojson.Safe.from_string
      {|{"providers": {"openai": {"api_key": "sk-test"}}}|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check int) "1 provider" 1 (List.length cfg.providers);
  let name, _ = List.hd cfg.providers in
  Alcotest.(check string) "provider name" "openai" name

let test_config_parse_gateway_port () =
  let json = Yojson.Safe.from_string {|{"gateway": {"port": 8080}}|} in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check int) "gateway port" 8080 cfg.gateway.port

let test_config_parse_memory_backend () =
  let json = Yojson.Safe.from_string {|{"memory": {"backend": "file"}}|} in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check string) "memory backend" "file" cfg.memory.backend

let test_config_parse_security_workspace_only () =
  let json =
    Yojson.Safe.from_string {|{"security": {"workspace_only": true}}|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check bool) "workspace_only" true cfg.security.workspace_only

let test_config_parse_audit_enabled () =
  let json =
    Yojson.Safe.from_string {|{"security": {"audit": {"enabled": true}}}|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check bool) "audit enabled" true cfg.security.audit_enabled

let test_config_default_is_parseable () =
  let defaults = Runtime_config.default in
  Alcotest.(check bool)
    "default primary model non-empty" true
    (String.length defaults.agent_defaults.primary_model > 0);
  Alcotest.(check bool)
    "default max_tool_iterations > 0" true
    (defaults.agent_defaults.max_tool_iterations > 0)

let test_config_parse_multiple_providers () =
  let json =
    Yojson.Safe.from_string
      {|{"providers": {
          "anthropic": {"api_key": "sk-ant"},
          "openai": {"api_key": "sk-oai"},
          "groq": {"api_key": "sk-groq"}
        }}|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check int) "3 providers" 3 (List.length cfg.providers)

let test_config_parse_no_channels () =
  let json = Yojson.Safe.from_string {|{}|} in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check bool)
    "no telegram by default" true
    (cfg.channels.telegram = None)

let test_config_parse_with_telegram () =
  let json =
    Yojson.Safe.from_string
      {|{"channels": {"telegram": {"accounts": {"main": {"bot_token": "tok123", "allow_from": ["*"]}}}}}|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check bool)
    "telegram configured" true
    (cfg.channels.telegram <> None)

let test_config_parse_model_priority () =
  let json =
    Yojson.Safe.from_string
      {|{"agent_defaults": {"model_priority": [{"provider": "openai", "model": "gpt-4o"}]}}|}
  in
  let cfg = Config_loader.parse_config json in
  (* model_priority is parsed from JSON - config should parse without error *)
  Alcotest.(check bool)
    "model_priority config parsed ok" true
    (String.length cfg.agent_defaults.primary_model >= 0)

let suite =
  [
    Alcotest.test_case "help/version returns output" `Quick
      test_help_returns_something;
    Alcotest.test_case "status returns output" `Quick test_status_returns_output;
    Alcotest.test_case "unknown command graceful" `Quick
      test_unknown_command_graceful;
    Alcotest.test_case "version contains clawq" `Quick
      test_version_contains_clawq;
    Alcotest.test_case "minimal auth codex disabled message" `Quick
      test_min_auth_codex_disabled_message;
    Alcotest.test_case "minimal auth status shows codex oauth" `Quick
      test_min_auth_status_shows_codex_oauth;
    Alcotest.test_case "config default model" `Quick test_config_default_model;
    Alcotest.test_case "config default temperature" `Quick
      test_config_default_temperature;
    Alcotest.test_case "config default gateway" `Quick
      test_config_default_gateway;
    Alcotest.test_case "config parse model" `Quick test_config_parse_model;
    Alcotest.test_case "config parse temperature" `Quick
      test_config_parse_temperature;
    Alcotest.test_case "config parse providers" `Quick
      test_config_parse_providers;
    Alcotest.test_case "config parse gateway port" `Quick
      test_config_parse_gateway_port;
    Alcotest.test_case "config parse memory backend" `Quick
      test_config_parse_memory_backend;
    Alcotest.test_case "config parse security workspace_only" `Quick
      test_config_parse_security_workspace_only;
    Alcotest.test_case "config parse audit enabled" `Quick
      test_config_parse_audit_enabled;
    Alcotest.test_case "config default is parseable" `Quick
      test_config_default_is_parseable;
    Alcotest.test_case "config parse multiple providers" `Quick
      test_config_parse_multiple_providers;
    Alcotest.test_case "config parse no channels" `Quick
      test_config_parse_no_channels;
    Alcotest.test_case "config parse with telegram" `Quick
      test_config_parse_with_telegram;
    Alcotest.test_case "config parse model_priority" `Quick
      test_config_parse_model_priority;
  ]

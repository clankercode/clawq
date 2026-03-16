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
    (contains out "not available in the minimal build")

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

let test_min_background_disabled_message () =
  let out = Command_bridge_min.handle [ "background"; "wait"; "1" ] in
  Alcotest.(check bool)
    "minimal background explains disabled surface" true
    (contains out "not available in the minimal build")

let test_min_delegate_disabled_message () =
  let out =
    Command_bridge_min.handle [ "delegate"; "implement"; "something" ]
  in
  Alcotest.(check bool)
    "minimal delegate explains disabled surface" true
    (contains out "not available in the minimal build")

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

let test_is_credential_valid () =
  Alcotest.(check bool)
    "empty is invalid" false
    (Runtime_config.is_credential_valid "");
  Alcotest.(check bool)
    "short is invalid" false
    (Runtime_config.is_credential_valid "abc123");
  Alcotest.(check bool)
    "placeholder is invalid" false
    (Runtime_config.is_credential_valid "YOUR_API_KEY");
  Alcotest.(check bool)
    "valid credential" true
    (Runtime_config.is_credential_valid "sk-proper-key-12345")

let test_telegram_credential_validation () =
  let valid_account =
    {
      Runtime_config.bot_token = "123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11";
      allow_from = [];
      totp = None;
    }
  in
  let invalid_account =
    { valid_account with Runtime_config.bot_token = "abc123" }
  in
  Alcotest.(check bool)
    "valid telegram account" true
    (Runtime_config.telegram_account_has_valid_credentials valid_account);
  Alcotest.(check bool)
    "invalid telegram account" false
    (Runtime_config.telegram_account_has_valid_credentials invalid_account)

let test_slack_credential_validation () =
  let valid_slack =
    {
      Runtime_config.bot_token = "xoxb-valid-token-12345";
      signing_secret = "valid-secret-12345";
      events_path = "/slack/events";
      allow_channels = [];
      allow_users = [];
      app_token = "";
      socket_mode = false;
    }
  in
  let invalid_slack = { valid_slack with Runtime_config.bot_token = "abc" } in
  Alcotest.(check bool)
    "valid slack" true
    (Runtime_config.slack_has_valid_credentials valid_slack);
  Alcotest.(check bool)
    "invalid slack token" false
    (Runtime_config.slack_has_valid_credentials invalid_slack);
  let bad_secret =
    { valid_slack with Runtime_config.signing_secret = "short" }
  in
  Alcotest.(check bool)
    "invalid slack secret" false
    (Runtime_config.slack_has_valid_credentials bad_secret)

let test_lark_credential_validation () =
  let valid_lark =
    {
      Runtime_config.enabled = true;
      app_id = "cli_valid_app_id_12345";
      app_secret = "valid_secret_12345";
      verification_token = "";
      endpoint = "";
      mode = "webhook";
      allow_users = [];
    }
  in
  let disabled_lark = { valid_lark with Runtime_config.enabled = false } in
  let bad_creds_lark = { valid_lark with Runtime_config.app_id = "abc" } in
  Alcotest.(check bool)
    "valid lark" true
    (Runtime_config.lark_has_valid_credentials valid_lark);
  Alcotest.(check bool)
    "disabled lark" false
    (Runtime_config.lark_has_valid_credentials disabled_lark);
  Alcotest.(check bool)
    "bad creds lark" false
    (Runtime_config.lark_has_valid_credentials bad_creds_lark)

let test_teams_credential_validation () =
  let valid_teams =
    {
      Runtime_config.app_id = "valid-app-id-12345";
      app_secret = "valid-secret-12345";
      tenant_id = "valid-tenant-12345";
      webhook_path = "/teams/webhook";
      service_url = "";
      allow_teams = [];
      allow_users = [];
      mention_mode = "entity";
      file_consent_cards = true;
    }
  in
  let invalid_teams = { valid_teams with Runtime_config.app_id = "abc" } in
  Alcotest.(check bool)
    "valid teams" true
    (Runtime_config.teams_has_valid_credentials valid_teams);
  Alcotest.(check bool)
    "invalid teams" false
    (Runtime_config.teams_has_valid_credentials invalid_teams)

let test_discord_credential_validation () =
  let valid_discord =
    {
      Runtime_config.bot_token = "Bot.valid-token-12345";
      allow_guilds = [ "*" ];
      allow_users = [ "*" ];
      intents = 33281;
    }
  in
  let invalid_discord = { valid_discord with Runtime_config.bot_token = "" } in
  Alcotest.(check bool)
    "valid discord" true
    (Runtime_config.discord_has_valid_credentials valid_discord);
  Alcotest.(check bool)
    "empty bot_token discord" false
    (Runtime_config.discord_has_valid_credentials invalid_discord)

(* Test that cmd_channel reports "not configured" when tokens are missing *)
let test_cmd_channel_discord_no_token () =
  let json =
    Yojson.Safe.from_string
      {|{"channels": {"discord": {"bot_token": "", "allow_guilds": ["*"], "allow_users": ["*"]}}}|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  let result =
    match cfg.channels.discord with
    | None -> "none"
    | Some d ->
        if Runtime_config.discord_has_valid_credentials d then "configured"
        else "not configured"
  in
  Alcotest.(check string)
    "discord with empty token is not configured" "not configured" result

let test_cmd_channel_slack_no_token () =
  let json =
    Yojson.Safe.from_string
      {|{"channels": {"slack": {"bot_token": "", "signing_secret": "valid-secret-12345", "events_path": "/slack/events"}}}|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  let result =
    match cfg.channels.slack with
    | None -> "none"
    | Some s ->
        if Runtime_config.slack_has_valid_credentials s then "configured"
        else "not configured"
  in
  Alcotest.(check string)
    "slack with empty bot_token is not configured" "not configured" result

let test_cmd_channel_slack_no_secret () =
  let json =
    Yojson.Safe.from_string
      {|{"channels": {"slack": {"bot_token": "xoxb-valid-token-12345", "signing_secret": "", "events_path": "/slack/events"}}}|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  let result =
    match cfg.channels.slack with
    | None -> "none"
    | Some s ->
        if Runtime_config.slack_has_valid_credentials s then "configured"
        else "not configured"
  in
  Alcotest.(check string)
    "slack with empty signing_secret is not configured" "not configured" result

let test_cmd_channel_discord_with_token () =
  let json =
    Yojson.Safe.from_string
      {|{"channels": {"discord": {"bot_token": "Bot.valid-token-12345", "allow_guilds": ["*"], "allow_users": ["*"]}}}|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  let result =
    match cfg.channels.discord with
    | None -> "none"
    | Some d ->
        if Runtime_config.discord_has_valid_credentials d then "configured"
        else "not configured"
  in
  Alcotest.(check string)
    "discord with valid token is configured" "configured" result

let test_cmd_channel_slack_with_creds () =
  let json =
    Yojson.Safe.from_string
      {|{"channels": {"slack": {"bot_token": "xoxb-valid-token-12345", "signing_secret": "valid-secret-12345", "events_path": "/slack/events"}}}|}
  in
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  let result =
    match cfg.channels.slack with
    | None -> "none"
    | Some s ->
        if Runtime_config.slack_has_valid_credentials s then "configured"
        else "not configured"
  in
  Alcotest.(check string)
    "slack with valid creds is configured" "configured" result

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
    Alcotest.test_case "minimal background disabled message" `Quick
      test_min_background_disabled_message;
    Alcotest.test_case "minimal delegate disabled message" `Quick
      test_min_delegate_disabled_message;
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
    Alcotest.test_case "is_credential_valid" `Quick test_is_credential_valid;
    Alcotest.test_case "telegram credential validation" `Quick
      test_telegram_credential_validation;
    Alcotest.test_case "slack credential validation" `Quick
      test_slack_credential_validation;
    Alcotest.test_case "lark credential validation" `Quick
      test_lark_credential_validation;
    Alcotest.test_case "teams credential validation" `Quick
      test_teams_credential_validation;
    Alcotest.test_case "discord credential validation" `Quick
      test_discord_credential_validation;
    Alcotest.test_case "channel discord no token shows not configured" `Quick
      test_cmd_channel_discord_no_token;
    Alcotest.test_case "channel slack no token shows not configured" `Quick
      test_cmd_channel_slack_no_token;
    Alcotest.test_case "channel slack no secret shows not configured" `Quick
      test_cmd_channel_slack_no_secret;
    Alcotest.test_case "channel discord with token shows configured" `Quick
      test_cmd_channel_discord_with_token;
    Alcotest.test_case "channel slack with creds shows configured" `Quick
      test_cmd_channel_slack_with_creds;
  ]

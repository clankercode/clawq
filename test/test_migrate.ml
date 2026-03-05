let nullclaw_json = {|{
  "default_temperature": 0.8,
  "models": {
    "providers": {
      "openrouter": {
        "api_key": "sk-test-123"
      },
      "vertex": {
        "api_key": {"type": "service_account", "project_id": "myproj"},
        "base_url": "https://vertex.example.com"
      }
    }
  },
  "agents": {
    "defaults": {
      "model": {
        "primary": "openrouter/anthropic/claude-sonnet-4"
      }
    }
  },
  "channels": {
    "cli": true,
    "telegram": {
      "accounts": {
        "main": {
          "bot_token": "123:ABC",
          "allow_from": ["user1"]
        }
      }
    },
    "irc": {
      "accounts": {
        "test": { "host": "irc.test.com" }
      }
    }
  },
  "memory": {
    "backend": "markdown",
    "search": { "enabled": true }
  },
  "gateway": {
    "port": 4000,
    "host": "0.0.0.0",
    "require_pairing": true
  },
  "autonomy": {
    "workspace_only": false
  },
  "security": {
    "audit": { "enabled": true }
  }
}|}

let test_convert_providers () =
  let json = Yojson.Safe.from_string nullclaw_json in
  let config, _ = Migrate.convert json in
  Alcotest.(check int) "2 providers" 2 (List.length config.providers);
  let or_cfg = List.assoc "openrouter" config.providers in
  Alcotest.(check string) "openrouter key" "sk-test-123" or_cfg.api_key;
  let vx_cfg = List.assoc "vertex" config.providers in
  Alcotest.(check bool) "vertex key is stringified object" true
    (String.length vx_cfg.api_key > 5)

let test_convert_agent_defaults () =
  let json = Yojson.Safe.from_string nullclaw_json in
  let config, _ = Migrate.convert json in
  Alcotest.(check string) "primary model"
    "openrouter/anthropic/claude-sonnet-4"
    config.agent_defaults.primary_model

let test_convert_channels () =
  let json = Yojson.Safe.from_string nullclaw_json in
  let config, warnings = Migrate.convert json in
  Alcotest.(check bool) "cli enabled" true config.channels.cli;
  (match config.channels.telegram with
   | None -> Alcotest.fail "expected telegram config"
   | Some tg ->
     Alcotest.(check int) "1 telegram account" 1 (List.length tg.accounts);
     let _, acct = List.hd tg.accounts in
     Alcotest.(check string) "bot token" "123:ABC" acct.bot_token);
  Alcotest.(check bool) "IRC skip warning" true
    (List.exists (fun w -> let re = Str.regexp_string "IRC" in
       try ignore (Str.search_forward re w 0); true
       with Not_found -> false) warnings)

let test_convert_memory () =
  let json = Yojson.Safe.from_string nullclaw_json in
  let config, warnings = Migrate.convert json in
  Alcotest.(check string) "backend mapped to sqlite" "sqlite" config.memory.backend;
  Alcotest.(check bool) "search enabled" true config.memory.search_enabled;
  Alcotest.(check bool) "markdown->sqlite warning" true
    (List.exists (fun w -> let re = Str.regexp_string "markdown" in
       try ignore (Str.search_forward re w 0); true
       with Not_found -> false) warnings)

let test_convert_gateway () =
  let json = Yojson.Safe.from_string nullclaw_json in
  let config, _ = Migrate.convert json in
  Alcotest.(check int) "port" 4000 config.gateway.port;
  Alcotest.(check string) "host" "0.0.0.0" config.gateway.host;
  Alcotest.(check bool) "require_pairing" true config.gateway.require_pairing

let test_convert_security () =
  let json = Yojson.Safe.from_string nullclaw_json in
  let config, _ = Migrate.convert json in
  Alcotest.(check bool) "workspace_only from autonomy" false
    config.security.workspace_only;
  Alcotest.(check bool) "audit_enabled from security.audit" true
    config.security.audit_enabled

let test_convert_temperature () =
  let json = Yojson.Safe.from_string nullclaw_json in
  let config, _ = Migrate.convert json in
  Alcotest.(check (float 0.01)) "temperature" 0.8 config.default_temperature

let suite =
  [
    Alcotest.test_case "convert providers" `Quick test_convert_providers;
    Alcotest.test_case "convert agent defaults" `Quick test_convert_agent_defaults;
    Alcotest.test_case "convert channels" `Quick test_convert_channels;
    Alcotest.test_case "convert memory" `Quick test_convert_memory;
    Alcotest.test_case "convert gateway" `Quick test_convert_gateway;
    Alcotest.test_case "convert security" `Quick test_convert_security;
    Alcotest.test_case "convert temperature" `Quick test_convert_temperature;
  ]

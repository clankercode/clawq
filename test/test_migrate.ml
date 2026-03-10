let nullclaw_json =
  {|{
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
  Alcotest.(check bool)
    "vertex key is stringified object" true
    (String.length vx_cfg.api_key > 5)

let test_convert_agent_defaults () =
  let json = Yojson.Safe.from_string nullclaw_json in
  let config, _ = Migrate.convert json in
  Alcotest.(check string)
    "primary model" "openrouter/anthropic/claude-sonnet-4"
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
  Alcotest.(check bool)
    "IRC skip warning" true
    (List.exists
       (fun w ->
         let re = Str.regexp_string "IRC" in
         try
           ignore (Str.search_forward re w 0);
           true
         with Not_found -> false)
       warnings)

let test_convert_memory () =
  let json = Yojson.Safe.from_string nullclaw_json in
  let config, warnings = Migrate.convert json in
  Alcotest.(check string)
    "backend mapped to sqlite" "sqlite" config.memory.backend;
  Alcotest.(check bool) "search enabled" true config.memory.search_enabled;
  Alcotest.(check bool)
    "markdown->sqlite warning" true
    (List.exists
       (fun w ->
         let re = Str.regexp_string "markdown" in
         try
           ignore (Str.search_forward re w 0);
           true
         with Not_found -> false)
       warnings)

let test_convert_gateway () =
  let json = Yojson.Safe.from_string nullclaw_json in
  let config, _ = Migrate.convert json in
  Alcotest.(check int) "port" 4000 config.gateway.port;
  Alcotest.(check string) "host" "0.0.0.0" config.gateway.host;
  Alcotest.(check bool) "require_pairing" true config.gateway.require_pairing

let test_convert_security () =
  let json = Yojson.Safe.from_string nullclaw_json in
  let config, _ = Migrate.convert json in
  Alcotest.(check bool)
    "workspace_only from autonomy" false config.security.workspace_only;
  Alcotest.(check bool)
    "audit_enabled from security.audit" true config.security.audit_enabled

let test_convert_temperature () =
  let json = Yojson.Safe.from_string nullclaw_json in
  let config, _ = Migrate.convert json in
  Alcotest.(check (float 0.01)) "temperature" 0.8 config.default_temperature

let test_apply_writes_config () =
  Test_helpers.with_temp_home (fun home ->
      let config, _ = Migrate.convert (Yojson.Safe.from_string nullclaw_json) in
      let result = Migrate.apply config in
      let config_path =
        Filename.concat (Filename.concat home ".clawq") "config.json"
      in
      Alcotest.(check bool)
        "config.json exists after apply" true
        (Sys.file_exists config_path);
      Alcotest.(check bool)
        "apply result mentions config.json" true
        (let re = Str.regexp_string "config.json" in
         try
           ignore (Str.search_forward re result 0);
           true
         with Not_found -> false);
      (* Verify it is valid JSON *)
      let ic = open_in config_path in
      let contents = really_input_string ic (in_channel_length ic) in
      close_in ic;
      let parsed = Yojson.Safe.from_string contents in
      Alcotest.(check bool)
        "config.json parses as JSON" true
        (match parsed with `Assoc _ -> true | _ -> false))

let test_apply_does_not_touch_real_home () =
  (* Belt-and-suspenders: Migrate.apply must honour the temp HOME, not escape
     to the real home directory. *)
  let real_home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let real_config =
    Filename.concat (Filename.concat real_home ".clawq") "config.json"
  in
  let before_mtime =
    try
      let st = Unix.stat real_config in
      Some st.Unix.st_mtime
    with _ -> None
  in
  Test_helpers.with_temp_home (fun _home ->
      let config, _ = Migrate.convert (Yojson.Safe.from_string nullclaw_json) in
      ignore (Migrate.apply config));
  let after_mtime =
    try
      let st = Unix.stat real_config in
      Some st.Unix.st_mtime
    with _ -> None
  in
  Alcotest.(check bool)
    "real ~/.clawq/config.json not modified" true
    (before_mtime = after_mtime)

let test_cmd_migrate_from_apply () =
  Test_helpers.with_temp_home (fun home ->
      let src_path = Filename.temp_file "clawq_nullclaw_" ".json" in
      Fun.protect
        (fun () ->
          let oc = open_out src_path in
          output_string oc nullclaw_json;
          close_out oc;
          let result = Migrate.cmd_migrate [ "from"; src_path; "apply" ] in
          let config_path =
            Filename.concat (Filename.concat home ".clawq") "config.json"
          in
          Alcotest.(check bool)
            "config.json created by cmd_migrate apply" true
            (Sys.file_exists config_path);
          Alcotest.(check bool)
            "result mentions config.json" true
            (let re = Str.regexp_string "config.json" in
             try
               ignore (Str.search_forward re result 0);
               true
             with Not_found -> false))
        ~finally:(fun () -> try Sys.remove src_path with _ -> ()))

let test_cmd_migrate_auto_discover_apply () =
  (* When $HOME/.nullclaw/config.json exists, cmd_migrate apply must write
     to the temp $HOME/.clawq/config.json and NOT to the real home. *)
  Test_helpers.with_temp_home (fun home ->
      let nullclaw_dir = Filename.concat home ".nullclaw" in
      Unix.mkdir nullclaw_dir 0o755;
      let src_path = Filename.concat nullclaw_dir "config.json" in
      let oc = open_out src_path in
      output_string oc nullclaw_json;
      close_out oc;
      let result = Migrate.cmd_migrate [ "apply" ] in
      let config_path =
        Filename.concat (Filename.concat home ".clawq") "config.json"
      in
      Alcotest.(check bool)
        "config.json created by auto-discovered apply" true
        (Sys.file_exists config_path);
      ignore result)

let suite =
  [
    Alcotest.test_case "convert providers" `Quick test_convert_providers;
    Alcotest.test_case "convert agent defaults" `Quick
      test_convert_agent_defaults;
    Alcotest.test_case "convert channels" `Quick test_convert_channels;
    Alcotest.test_case "convert memory" `Quick test_convert_memory;
    Alcotest.test_case "convert gateway" `Quick test_convert_gateway;
    Alcotest.test_case "convert security" `Quick test_convert_security;
    Alcotest.test_case "convert temperature" `Quick test_convert_temperature;
    Alcotest.test_case "apply writes config.json" `Quick
      test_apply_writes_config;
    Alcotest.test_case "apply does not touch real home" `Quick
      test_apply_does_not_touch_real_home;
    Alcotest.test_case "cmd_migrate from apply" `Quick
      test_cmd_migrate_from_apply;
    Alcotest.test_case "cmd_migrate auto-discover apply" `Quick
      test_cmd_migrate_auto_discover_apply;
  ]

(* test_minimal_reload.ml — Minimal-build reload and compatibility tests.

   Verifies that:
   1. The minimal binary reloads config correctly through Command_bridge_min.
   2. Scope bundles (access_bundles, access_scopes) are parsed without errors
      and do not interfere with the minimal binary's status output.
   3. Existing room profiles survive a config reload cycle intact. *)

(** Write [contents] to the config.json file under [home_dir]/.clawq/. *)
let write_config home_dir contents =
  let clawq_dir = Filename.concat home_dir ".clawq" in
  (try Unix.mkdir clawq_dir 0o755
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let path = Filename.concat clawq_dir "config.json" in
  let oc = open_out path in
  output_string oc contents;
  close_out oc

(** Run a minimal-binary command and return its output. *)
let min_cmd args = Command_bridge_min.handle args

(* ---- 1. Minimal binary config reload cycle ---- *)

let test_minimal_status_reads_config () =
  Test_helpers.with_temp_home (fun home ->
      write_config home
        {|{"default_temperature": 0.73, "agent_defaults": {"primary_model": "openai:gpt-4o"}}|};
      let out = min_cmd [ "status" ] in
      Alcotest.(check bool)
        "status mentions model" true
        (Test_helpers.string_contains out "openai:gpt-4o");
      Alcotest.(check bool)
        "status mentions temperature" true
        (Test_helpers.string_contains out "0.73"))

let test_minimal_reload_picks_up_config_change () =
  Test_helpers.with_temp_home (fun home ->
      write_config home {|{"default_temperature": 0.5}|};
      let out1 = min_cmd [ "status" ] in
      Alcotest.(check bool)
        "initial temp 0.50" true
        (Test_helpers.string_contains out1 "0.50");
      (* Simulate external config edit + reload *)
      write_config home {|{"default_temperature": 0.91}|};
      let out2 = min_cmd [ "status" ] in
      Alcotest.(check bool)
        "reloaded temp 0.91" true
        (Test_helpers.string_contains out2 "0.91"))

let test_minimal_reload_picks_up_model_change () =
  Test_helpers.with_temp_home (fun home ->
      write_config home
        {|{"agent_defaults": {"primary_model": "openai:gpt-4o"}}|};
      let out1 = min_cmd [ "status" ] in
      Alcotest.(check bool)
        "initial model" true
        (Test_helpers.string_contains out1 "openai:gpt-4o");
      write_config home
        {|{"agent_defaults": {"primary_model": "anthropic:claude-sonnet-4-6"}}|};
      let out2 = min_cmd [ "status" ] in
      Alcotest.(check bool)
        "reloaded model" true
        (Test_helpers.string_contains out2 "anthropic:claude-sonnet-4-6"))

let test_minimal_reload_preserves_unchanged_fields () =
  Test_helpers.with_temp_home (fun home ->
      write_config home
        {|{"default_temperature": 0.5, "memory": {"backend": "custom-backend"}}|};
      let out1 = min_cmd [ "status" ] in
      Alcotest.(check bool)
        "custom backend shown" true
        (Test_helpers.string_contains out1 "custom-backend");
      write_config home
        {|{"default_temperature": 0.9, "memory": {"backend": "custom-backend"}}|};
      let out2 = min_cmd [ "status" ] in
      Alcotest.(check bool)
        "custom backend preserved" true
        (Test_helpers.string_contains out2 "custom-backend");
      Alcotest.(check bool)
        "temperature updated" true
        (Test_helpers.string_contains out2 "0.90"))

let test_minimal_reload_from_invalid_json () =
  Test_helpers.with_temp_home (fun home ->
      write_config home {|{"default_temperature": 0.5}|};
      let _out1 = min_cmd [ "status" ] in
      (* Write invalid JSON *)
      write_config home "{ not valid json at all";
      let out2 = min_cmd [ "status" ] in
      (* Should not crash; status output still produced *)
      Alcotest.(check bool)
        "status produced" true
        (Test_helpers.string_contains out2 "clawq-min status"))

let test_minimal_multiple_reload_cycles () =
  Test_helpers.with_temp_home (fun home ->
      write_config home {|{"default_temperature": 0.10}|};
      let out1 = min_cmd [ "status" ] in
      Alcotest.(check bool)
        "cycle 1" true
        (Test_helpers.string_contains out1 "0.10");
      write_config home {|{"default_temperature": 0.20}|};
      let out2 = min_cmd [ "status" ] in
      Alcotest.(check bool)
        "cycle 2" true
        (Test_helpers.string_contains out2 "0.20");
      write_config home {|{"default_temperature": 0.30}|};
      let out3 = min_cmd [ "status" ] in
      Alcotest.(check bool)
        "cycle 3" true
        (Test_helpers.string_contains out3 "0.30");
      write_config home {|{"default_temperature": 0.40}|};
      let out4 = min_cmd [ "status" ] in
      Alcotest.(check bool)
        "cycle 4" true
        (Test_helpers.string_contains out4 "0.40"))

(* ---- 2. Scope bundles ignored gracefully ---- *)

let test_minimal_status_with_scope_bundles () =
  Test_helpers.with_temp_home (fun home ->
      write_config home
        {|{
          "default_temperature": 0.42,
          "access_bundles": [
            {"id": "b1", "allowed_tools": ["tool_a"]},
            {"id": "b2", "allowed_tools": ["tool_b"]}
          ],
          "access_scopes": [
            {"id": "default", "level": "default", "access_bundle_ids": ["b1", "b2"]}
          ],
          "agent_defaults": {"primary_model": "openai:gpt-4o"}
        }|};
      let out = min_cmd [ "status" ] in
      Alcotest.(check bool)
        "model shown" true
        (Test_helpers.string_contains out "openai:gpt-4o");
      Alcotest.(check bool)
        "temperature shown" true
        (Test_helpers.string_contains out "0.42"))

let test_minimal_status_with_empty_scope_bundles () =
  Test_helpers.with_temp_home (fun home ->
      write_config home
        {|{
          "access_bundles": [],
          "access_scopes": [],
          "default_temperature": 0.6
        }|};
      let out = min_cmd [ "status" ] in
      Alcotest.(check bool)
        "status produced" true
        (Test_helpers.string_contains out "clawq-min status"))

let test_minimal_status_with_malformed_scope_bundles () =
  (* Duplicate bundle ids trigger fail-closed but minimal binary must not crash *)
  Test_helpers.with_temp_home (fun home ->
      write_config home
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
        }|};
      let out = min_cmd [ "status" ] in
      Alcotest.(check bool)
        "status produced without crash" true
        (Test_helpers.string_contains out "clawq-min status"))

let test_minimal_scope_bundle_reload_cycle () =
  (* Scope bundles added via config reload are picked up *)
  Test_helpers.with_temp_home (fun home ->
      write_config home {|{"access_bundles": [], "default_temperature": 0.5}|};
      let cfg1 = Config_loader.load () in
      Alcotest.(check int)
        "initially no bundles" 0
        (List.length cfg1.access_bundles);
      write_config home
        {|{
          "access_bundles": [
            {"id": "new-bundle", "allowed_tools": ["tool_x"]}
          ],
          "access_scopes": [
            {"id": "s1", "level": "default", "access_bundle_ids": ["new-bundle"]}
          ]
        }|};
      let cfg2 = Config_loader.load () in
      Alcotest.(check int)
        "1 bundle after reload" 1
        (List.length cfg2.access_bundles);
      Alcotest.(check int)
        "1 scope after reload" 1
        (List.length cfg2.access_scopes))

(* ---- 3. Room profiles persist after reload ---- *)

let test_minimal_room_profiles_survive_reload () =
  Test_helpers.with_temp_home (fun home ->
      write_config home
        {|{
          "room_profiles": [
            {"id": "vip", "model": "openai:gpt-5.4", "system_prompt": "VIP mode",
             "max_tool_iterations": 5},
            {"id": "basic", "model": "openai:gpt-4o"}
          ],
          "room_profile_bindings": [
            {"profile_id": "vip", "room": "C100", "active": true},
            {"profile_id": "basic", "room": "general", "active": false}
          ]
        }|};
      let cfg1 = Config_loader.load () in
      Alcotest.(check int) "initial profiles" 2 (List.length cfg1.room_profiles);
      let cfg2 = Config_loader.load () in
      Alcotest.(check int)
        "reloaded profiles" 2
        (List.length cfg2.room_profiles);
      let p1 = List.nth cfg2.room_profiles 0 in
      Alcotest.(check string) "vip id" "vip" p1.id;
      Alcotest.(check string) "vip model" "openai:gpt-5.4" p1.model;
      Alcotest.(check string) "vip system_prompt" "VIP mode" p1.system_prompt;
      Alcotest.(check int) "vip max_tool_iterations" 5 p1.max_tool_iterations;
      let p2 = List.nth cfg2.room_profiles 1 in
      Alcotest.(check string) "basic id" "basic" p2.id;
      Alcotest.(check string) "basic model" "openai:gpt-4o" p2.model)

let test_minimal_room_profiles_persist_through_config_change () =
  Test_helpers.with_temp_home (fun home ->
      write_config home
        {|{
          "default_temperature": 0.5,
          "room_profiles": [
            {"id": "bot", "model": "openai:gpt-4o", "system_prompt": "Bot mode"}
          ],
          "room_profile_bindings": [
            {"profile_id": "bot", "room": "bot-room", "active": true}
          ]
        }|};
      let cfg1 = Config_loader.load () in
      Alcotest.(check int) "initial profiles" 1 (List.length cfg1.room_profiles);
      (* Change temperature, keep room profiles *)
      write_config home
        {|{
          "default_temperature": 0.9,
          "room_profiles": [
            {"id": "bot", "model": "openai:gpt-4o", "system_prompt": "Bot mode"}
          ],
          "room_profile_bindings": [
            {"profile_id": "bot", "room": "bot-room", "active": true}
          ]
        }|};
      let cfg2 = Config_loader.load () in
      Alcotest.(check (float 0.001)) "updated temp" 0.9 cfg2.default_temperature;
      Alcotest.(check int) "profiles still 1" 1 (List.length cfg2.room_profiles);
      let p = List.nth cfg2.room_profiles 0 in
      Alcotest.(check string) "profile id unchanged" "bot" p.id;
      Alcotest.(check string) "profile model unchanged" "openai:gpt-4o" p.model;
      Alcotest.(check string)
        "profile prompt unchanged" "Bot mode" p.system_prompt)

let test_minimal_room_profile_tool_access_after_reload () =
  Test_helpers.with_temp_home (fun home ->
      write_config home
        {|{
          "room_profiles": [
            {"id": "restricted", "model": "openai:gpt-4o",
             "allowed_tools": ["file_read"], "denied_tools": ["shell_exec"]}
          ],
          "room_profile_bindings": [
            {"profile_id": "restricted", "room": "R1", "active": true}
          ]
        }|};
      let cfg1 = Config_loader.load () in
      Alcotest.(check bool)
        "shell denied before reload" true
        (Option.is_some
           (Runtime_config.room_profile_tool_denial_for_session cfg1
              ~session_key:"chat:R1" ~tool_name:"shell_exec"));
      Alcotest.(check bool)
        "file_read allowed before reload" true
        (Option.is_none
           (Runtime_config.room_profile_tool_denial_for_session cfg1
              ~session_key:"chat:R1" ~tool_name:"file_read"));
      let cfg2 = Config_loader.load () in
      Alcotest.(check bool)
        "shell denied after reload" true
        (Option.is_some
           (Runtime_config.room_profile_tool_denial_for_session cfg2
              ~session_key:"chat:R1" ~tool_name:"shell_exec"));
      Alcotest.(check bool)
        "file_read allowed after reload" true
        (Option.is_none
           (Runtime_config.room_profile_tool_denial_for_session cfg2
              ~session_key:"chat:R1" ~tool_name:"file_read")))

let test_minimal_room_profiles_with_bundles_reload () =
  Test_helpers.with_temp_home (fun home ->
      write_config home
        {|{
          "access_bundles": [
            {"id": "tools-bundle", "allowed_tools": ["read", "write"],
             "denied_tools": ["delete"]}
          ],
          "room_profiles": [
            {"id": "editor", "model": "openai:gpt-4o",
             "access_bundle_ids": ["tools-bundle"]}
          ],
          "room_profile_bindings": [
            {"profile_id": "editor", "room": "edit-room", "active": true}
          ]
        }|};
      let cfg1 = Config_loader.load () in
      let bundles1 =
        Runtime_config.access_bundles_for_profile cfg1
          (List.nth cfg1.room_profiles 0)
      in
      Alcotest.(check int) "1 bundle before reload" 1 (List.length bundles1);
      let cfg2 = Config_loader.load () in
      Alcotest.(check int)
        "profiles preserved" 1
        (List.length cfg2.room_profiles);
      let bundles2 =
        Runtime_config.access_bundles_for_profile cfg2
          (List.nth cfg2.room_profiles 0)
      in
      Alcotest.(check int) "1 bundle after reload" 1 (List.length bundles2);
      let bundle = List.nth bundles2 0 in
      Alcotest.(check (list string))
        "bundle allowed_tools" [ "read"; "write" ] bundle.allowed_tools;
      Alcotest.(check (list string))
        "bundle denied_tools" [ "delete" ] bundle.denied_tools)

let test_minimal_room_profile_codebase_grants_survive_reload () =
  Test_helpers.with_temp_home (fun home ->
      write_config home
        {|{
          "room_profiles": [
            {"id": "dev", "model": "openai:gpt-4o"}
          ],
          "room_profile_bindings": [
            {"profile_id": "dev", "room": "dev-room", "active": true}
          ],
          "room_profile_codebase_grants": [
            {"profile_id": "dev", "patterns": ["$CLAWQ_WORKSPACE/src/**"]}
          ]
        }|};
      let cfg1 = Config_loader.load () in
      let grants1 =
        Runtime_config.room_profile_codebase_grants_for_profile cfg1
          ~profile_id:"dev"
      in
      Alcotest.(check (list string))
        "grants before reload"
        [ "$CLAWQ_WORKSPACE/src/**" ]
        grants1;
      let cfg2 = Config_loader.load () in
      let grants2 =
        Runtime_config.room_profile_codebase_grants_for_profile cfg2
          ~profile_id:"dev"
      in
      Alcotest.(check (list string))
        "grants after reload"
        [ "$CLAWQ_WORKSPACE/src/**" ]
        grants2)

let test_minimal_new_room_profile_added_on_reload () =
  Test_helpers.with_temp_home (fun home ->
      write_config home
        {|{"room_profiles": [{"id": "p1", "model": "openai:gpt-4o"}]}|};
      let cfg1 = Config_loader.load () in
      Alcotest.(check int) "initial profiles" 1 (List.length cfg1.room_profiles);
      write_config home
        {|{
          "room_profiles": [
            {"id": "p1", "model": "openai:gpt-4o"},
            {"id": "p2", "model": "anthropic:claude-sonnet-4-6"}
          ]
        }|};
      let cfg2 = Config_loader.load () in
      Alcotest.(check int)
        "profiles after reload" 2
        (List.length cfg2.room_profiles);
      let ids =
        List.map
          (fun (p : Runtime_config.room_profile) -> p.id)
          cfg2.room_profiles
      in
      Alcotest.(check (list string)) "profile ids" [ "p1"; "p2" ] ids)

(* ---- Suite ---- *)

let suite =
  [
    (* Reload cycle tests — exercise Command_bridge_min *)
    Alcotest.test_case "minimal status reads config" `Quick
      test_minimal_status_reads_config;
    Alcotest.test_case "minimal reload picks up config change" `Quick
      test_minimal_reload_picks_up_config_change;
    Alcotest.test_case "minimal reload picks up model change" `Quick
      test_minimal_reload_picks_up_model_change;
    Alcotest.test_case "minimal reload preserves unchanged fields" `Quick
      test_minimal_reload_preserves_unchanged_fields;
    Alcotest.test_case "minimal reload from invalid json" `Quick
      test_minimal_reload_from_invalid_json;
    Alcotest.test_case "minimal multiple reload cycles" `Quick
      test_minimal_multiple_reload_cycles;
    (* Scope bundle tests — exercise Command_bridge_min where applicable *)
    Alcotest.test_case "minimal status with scope bundles" `Quick
      test_minimal_status_with_scope_bundles;
    Alcotest.test_case "minimal status with empty scope bundles" `Quick
      test_minimal_status_with_empty_scope_bundles;
    Alcotest.test_case "minimal status with malformed scope bundles" `Quick
      test_minimal_status_with_malformed_scope_bundles;
    Alcotest.test_case "minimal scope bundle reload cycle" `Quick
      test_minimal_scope_bundle_reload_cycle;
    (* Room profile persistence tests *)
    Alcotest.test_case "minimal room profiles survive reload" `Quick
      test_minimal_room_profiles_survive_reload;
    Alcotest.test_case "minimal room profiles persist through config change"
      `Quick test_minimal_room_profiles_persist_through_config_change;
    Alcotest.test_case "minimal room profile tool access after reload" `Quick
      test_minimal_room_profile_tool_access_after_reload;
    Alcotest.test_case "minimal room profiles with bundles reload" `Quick
      test_minimal_room_profiles_with_bundles_reload;
    Alcotest.test_case "minimal room profile codebase grants survive reload"
      `Quick test_minimal_room_profile_codebase_grants_survive_reload;
    Alcotest.test_case "minimal new room profile added on reload" `Quick
      test_minimal_new_room_profile_added_on_reload;
  ]

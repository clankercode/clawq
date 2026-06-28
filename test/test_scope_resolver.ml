let item_values items =
  List.map
    (fun (item : Runtime_config.effective_access_item) -> item.value)
    items

let provenance_labels item =
  List.map
    (fun (p : Runtime_config.access_provenance) ->
      p.layer ^ ":" ^ p.source_id ^ ":" ^ p.field)
    item.Runtime_config.provenance

let assert_all_provenance label items =
  List.iter
    (fun item ->
      Alcotest.(check bool)
        (label ^ " provenance for " ^ item.Runtime_config.value)
        true
        (item.Runtime_config.provenance <> []))
    items

let parse json = Config_loader.parse_config (Yojson.Safe.from_string json)

let test_layers_merge_deterministically_and_deny_wins () =
  let json =
    {|{
      "workspace": "/tmp/clawq-scope-root",
      "access_bundles": [
        {"id": "room", "allowed_tools": ["room_tool"], "denied_tools": ["shared_tool"]},
        {"id": "base", "allowed_tools": ["file_read", "shared_tool"]},
        {"id": "channel", "allowed_tools": ["channel_tool"]},
        {"id": "workspace", "denied_tools": ["file_read"], "allowed_tools": ["workspace_tool"]}
      ],
      "access_scopes": [
        {"id": "z-room", "level": "room", "room": "C123", "access_bundle_ids": ["room"]},
        {"id": "a-default", "level": "default", "access_bundle_ids": ["base"]},
        {"id": "m-channel", "level": "channel", "channel": "slack", "access_bundle_ids": ["channel"]},
        {"id": "b-workspace", "level": "workspace", "workspace": "/tmp/clawq-scope-root", "access_bundle_ids": ["workspace"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  Alcotest.(check (list string))
    "allowed tools preserve layer order after denies"
    [ "workspace_tool"; "channel_tool"; "room_tool" ]
    (item_values effective.allowed_tools);
  Alcotest.(check (list string))
    "denied tools are collected deterministically"
    [ "file_read"; "shared_tool" ]
    (item_values effective.denied_tools);
  assert_all_provenance "allowed tool" effective.allowed_tools;
  assert_all_provenance "denied tool" effective.denied_tools;
  let room_item =
    List.find
      (fun (item : Runtime_config.effective_access_item) ->
        item.value = "room_tool")
      effective.allowed_tools
  in
  Alcotest.(check (list string))
    "room item provenance"
    [
      "room:z-room:allowed_tools";
      "room:z-room:access_bundle_ids:room:allowed_tools";
    ]
    (provenance_labels room_item)

let test_global_security_caps_codebase_grants () =
  let json =
    {|{
      "workspace": "/tmp/clawq-scope-root",
      "security": {"workspace_only": true, "allowed_cwd_patterns": ["/tmp/clawq-scope-root/**"]},
      "access_bundles": [
        {"id": "repo", "codebase_grants": ["$CLAWQ_WORKSPACE/src/**", "/tmp/outside/**"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["repo"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  Alcotest.(check (list string))
    "global ceiling keeps workspace grant"
    [ "/tmp/clawq-scope-root/src/**" ]
    (item_values effective.codebase_grants);
  Alcotest.(check (list string))
    "global ceiling blocks outside grant" [ "/tmp/outside/**" ]
    (item_values effective.blocked_codebase_grants);
  assert_all_provenance "codebase grant" effective.codebase_grants;
  assert_all_provenance "blocked codebase grant"
    effective.blocked_codebase_grants

let test_inherited_grants_do_not_weaken_global_security () =
  let json =
    {|{
      "workspace": "/tmp/clawq-scope-root",
      "security": {"workspace_only": true, "allowed_cwd_patterns": ["/tmp/clawq-scope-root/**"]},
      "access_bundles": [
        {"id": "default", "codebase_grants": ["/tmp/outside-default/**"]},
        {"id": "room", "codebase_grants": ["$CLAWQ_WORKSPACE/project/**", "/tmp/outside-room/**"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["default"]},
        {"id": "room", "level": "room", "room": "C123", "access_bundle_ids": ["room"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  Alcotest.(check (list string))
    "inherited workspace grant survives global ceiling"
    [ "/tmp/clawq-scope-root/project/**" ]
    (item_values effective.codebase_grants);
  Alcotest.(check (list string))
    "inherited outside grants stay blocked"
    [ "/tmp/outside-default/**"; "/tmp/outside-room/**" ]
    (item_values effective.blocked_codebase_grants)

let test_memory_grants_are_direct_not_transitive () =
  let json =
    {|{
      "access_bundles": [
        {"id": "parent", "memory_grants": ["child"]},
        {"id": "child", "memory_grants": ["scope:secret:read"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["parent"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"web:general"
  in
  Alcotest.(check (list string))
    "only direct memory grants are effective" [ "child" ]
    (item_values effective.memory_grants)

let test_missing_memory_grants_default_to_no_access () =
  let json =
    {|{
      "access_bundles": [
        {"id": "empty"},
        {"id": "unreferenced", "memory_grants": ["scope:secret:read"]}
      ],
      "access_scopes": [
        {"id": "room", "level": "room", "room": "C123", "access_bundle_ids": ["empty"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  Alcotest.(check (list string))
    "missing memory grants grant nothing" []
    (item_values effective.memory_grants)

let test_duplicate_bundle_references_merge_provenance_once () =
  let json =
    {|{
      "access_bundles": [
        {"id": "shared", "allowed_tools": ["shared_tool"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["shared"]},
        {"id": "room", "level": "room", "room": "C123", "access_bundle_ids": ["shared"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  Alcotest.(check (list string))
    "duplicate bundle tool appears once" [ "shared_tool" ]
    (item_values effective.allowed_tools);
  let shared_item = List.hd effective.allowed_tools in
  Alcotest.(check (list string))
    "duplicate bundle provenance keeps both sources"
    [
      "default:default:allowed_tools";
      "default:default:access_bundle_ids:shared:allowed_tools";
      "room:room:allowed_tools";
      "room:room:access_bundle_ids:shared:allowed_tools";
    ]
    (provenance_labels shared_item)

let test_allow_and_deny_same_tool_denies () =
  let json =
    {|{
      "access_bundles": [
        {"id": "conflict", "allowed_tools": ["shell_exec"], "denied_tools": ["shell_exec"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["conflict"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  Alcotest.(check (list string))
    "same-tool allow is removed" []
    (item_values effective.allowed_tools);
  Alcotest.(check (list string))
    "same-tool deny remains explicit" [ "shell_exec" ]
    (item_values effective.denied_tools)

let test_room_scopes_do_not_cross_channel_boundaries () =
  let json =
    {|{
      "access_bundles": [
        {"id": "room", "allowed_tools": ["room_tool"]}
      ],
      "access_scopes": [
        {"id": "slack-room", "level": "room", "channel": "slack", "room": "C123", "access_bundle_ids": ["room"]}
      ]
    }|}
  in
  let cfg = parse json in
  let slack =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  let discord =
    Runtime_config.resolve_effective_access cfg ~session_key:"discord:C123"
  in
  Alcotest.(check (list string))
    "matching channel receives room grant" [ "room_tool" ]
    (item_values slack.allowed_tools);
  Alcotest.(check (list string))
    "different channel does not receive room grant" []
    (item_values discord.allowed_tools)

let test_invalid_profile_and_room_references_grant_nothing () =
  let json =
    {|{
      "access_bundles": [
        {"id": "profile", "allowed_tools": ["profile_tool"]},
        {"id": "room", "allowed_tools": ["room_tool"]}
      ],
      "room_profiles": [
        {"id": "known", "model": "openai:gpt-5.4", "access_bundle_ids": ["profile"]}
      ],
      "room_profile_bindings": [
        {"profile_id": "missing", "room": "C123", "active": true}
      ],
      "access_scopes": [
        {"id": "other-room", "level": "room", "room": "C999", "access_bundle_ids": ["room"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  Alcotest.(check (list string))
    "invalid profile and room refs grant no tools" []
    (item_values effective.allowed_tools)

let test_room_deny_overrides_workspace_allow () =
  let json =
    {|{
      "workspace": "/tmp/clawq-scope-root",
      "access_bundles": [
        {"id": "workspace", "allowed_tools": ["deploy_tool"]},
        {"id": "room", "allowed_tools": ["room_tool"], "denied_tools": ["deploy_tool"]}
      ],
      "access_scopes": [
        {"id": "workspace", "level": "workspace", "workspace": "/tmp/clawq-scope-root", "access_bundle_ids": ["workspace"]},
        {"id": "room", "level": "room", "room": "C123", "access_bundle_ids": ["room"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  Alcotest.(check (list string))
    "room deny removes inherited workspace allow" [ "room_tool" ]
    (item_values effective.allowed_tools);
  Alcotest.(check (list string))
    "room deny remains visible" [ "deploy_tool" ]
    (item_values effective.denied_tools)

let test_missing_layer_selectors_are_not_wildcards () =
  let json =
    {|{
      "access_bundles": [
        {"id": "workspace", "allowed_tools": ["workspace_tool"]},
        {"id": "channel", "allowed_tools": ["channel_tool"]},
        {"id": "room", "allowed_tools": ["room_tool"]}
      ],
      "access_scopes": [
        {"id": "workspace-missing", "level": "workspace", "access_bundle_ids": ["workspace"]},
        {"id": "channel-missing", "level": "channel", "access_bundle_ids": ["channel"]},
        {"id": "room-missing", "level": "room", "access_bundle_ids": ["room"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  Alcotest.(check (list string))
    "missing selectors grant nothing" []
    (item_values effective.allowed_tools)

let test_workspace_scope_expands_tilde_selector () =
  let json =
    {|{
      "workspace": "~/clawq-scope-root",
      "access_bundles": [
        {"id": "workspace", "allowed_tools": ["workspace_tool"]}
      ],
      "access_scopes": [
        {"id": "workspace-home", "level": "workspace", "workspace": "~/clawq-scope-root", "access_bundle_ids": ["workspace"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  Alcotest.(check (list string))
    "tilde workspace selector matches expanded workspace" [ "workspace_tool" ]
    (item_values effective.allowed_tools)

let test_legacy_room_profile_bundle_is_room_layer () =
  let json =
    {|{
      "workspace": "/tmp/clawq-scope-root",
      "room_profiles": [
        {
          "id": "legacy",
          "model": "openai:gpt-5.4",
          "allowed_tools": ["file_read"],
          "denied_tools": ["shell_exec"]
        }
      ],
      "room_profile_codebase_grants": [
        {"profile_id": "legacy", "patterns": ["$CLAWQ_WORKSPACE/**"]}
      ],
      "room_profile_bindings": [
        {"profile_id": "legacy", "room": "C123", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  Alcotest.(check (list string))
    "legacy allowed tools remain effective" [ "file_read" ]
    (item_values effective.allowed_tools);
  Alcotest.(check (list string))
    "legacy denied tools remain effective" [ "shell_exec" ]
    (item_values effective.denied_tools);
  Alcotest.(check (list string))
    "legacy codebase grants remain effective"
    [ "/tmp/clawq-scope-root/**" ]
    (item_values effective.codebase_grants);
  let file_read =
    List.find
      (fun (item : Runtime_config.effective_access_item) ->
        item.value = "file_read")
      effective.allowed_tools
  in
  Alcotest.(check (list string))
    "legacy profile provenance"
    [
      "room:room_profile:legacy:allowed_tools";
      "room:room_profile:legacy:access_bundle_ids:__legacy_room_profile:legacy:allowed_tools";
    ]
    (provenance_labels file_read)

let test_invalid_profile_bundle_denies_effective_profile_grants () =
  let json =
    {|{
      "access_bundles": [
        {"id": "known", "allowed_tools": ["explicit_tool"]}
      ],
      "room_profiles": [
        {
          "id": "legacy",
          "model": "openai:gpt-5.4",
          "allowed_tools": ["legacy_tool"],
          "access_bundle_ids": ["known", "missing"]
        }
      ],
      "room_profile_bindings": [
        {"profile_id": "legacy", "room": "C123", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  Alcotest.(check (list string))
    "invalid profile bundle reference suppresses all profile grants" []
    (item_values effective.allowed_tools)

let test_invalid_scope_bundle_denies_scope_grants () =
  let json =
    {|{
      "access_bundles": [
        {"id": "known", "allowed_tools": ["explicit_tool"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["known", "missing"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  Alcotest.(check (list string))
    "invalid scope bundle reference suppresses all scope grants" []
    (item_values effective.allowed_tools)

let suite =
  [
    Alcotest.test_case "layers merge deterministically and deny wins" `Quick
      test_layers_merge_deterministically_and_deny_wins;
    Alcotest.test_case "global security caps codebase grants" `Quick
      test_global_security_caps_codebase_grants;
    Alcotest.test_case "inherited grants do not weaken global security" `Quick
      test_inherited_grants_do_not_weaken_global_security;
    Alcotest.test_case "memory grants are direct" `Quick
      test_memory_grants_are_direct_not_transitive;
    Alcotest.test_case "missing memory grants grant nothing" `Quick
      test_missing_memory_grants_default_to_no_access;
    Alcotest.test_case "duplicate bundle references merge once" `Quick
      test_duplicate_bundle_references_merge_provenance_once;
    Alcotest.test_case "allow and deny same tool denies" `Quick
      test_allow_and_deny_same_tool_denies;
    Alcotest.test_case "room scopes do not cross channel boundaries" `Quick
      test_room_scopes_do_not_cross_channel_boundaries;
    Alcotest.test_case "invalid profile and room references grant nothing"
      `Quick test_invalid_profile_and_room_references_grant_nothing;
    Alcotest.test_case "room deny overrides workspace allow" `Quick
      test_room_deny_overrides_workspace_allow;
    Alcotest.test_case "missing layer selectors are not wildcards" `Quick
      test_missing_layer_selectors_are_not_wildcards;
    Alcotest.test_case "workspace scope expands tilde selector" `Quick
      test_workspace_scope_expands_tilde_selector;
    Alcotest.test_case "legacy profile bundle is room layer" `Quick
      test_legacy_room_profile_bundle_is_room_layer;
    Alcotest.test_case "invalid profile bundle denies effective profile grants"
      `Quick test_invalid_profile_bundle_denies_effective_profile_grants;
    Alcotest.test_case "invalid scope bundle denies scope grants" `Quick
      test_invalid_scope_bundle_denies_scope_grants;
  ]

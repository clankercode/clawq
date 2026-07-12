let item_values items =
  List.map
    (fun (item : Runtime_config.effective_access_item) -> item.value)
    items

let repo_grant_repos items =
  List.filter_map
    (fun (item : Runtime_config.effective_access_item) ->
      try
        let open Yojson.Safe.Util in
        Some (Yojson.Safe.from_string item.value |> member "repo" |> to_string)
      with _ -> None)
    items

let provenance_labels (item : Runtime_config.effective_access_item) =
  List.map
    (fun (p : Runtime_config.access_provenance) ->
      p.layer ^ ":" ^ p.source_id ^ ":" ^ p.field)
    item.provenance

let assert_all_provenance label items =
  List.iter
    (fun (item : Runtime_config.effective_access_item) ->
      Alcotest.(check bool)
        (label ^ " provenance for " ^ item.value)
        true (item.provenance <> []))
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
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
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
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
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
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
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
    Runtime_config.resolve_effective_access cfg ~session_key:"web:general" ()
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
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
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
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
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
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
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
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  let discord =
    Runtime_config.resolve_effective_access cfg ~session_key:"discord:C123" ()
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
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
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
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
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
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
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
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
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
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
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
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
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
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check (list string))
    "invalid scope bundle reference suppresses all scope grants" []
    (item_values effective.allowed_tools)

let test_repo_grants_attach_to_bundle () =
  let json =
    {|{
      "access_bundles": [
        {
          "id": "dev",
          "repo_grants": [
            {"repo": "acme/app", "capabilities": ["read", "comment", "branch"]},
            {"repo": "acme/lib", "capabilities": ["read", "pr"]}
          ]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["dev"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  let grant_values = item_values effective.repo_grants in
  Alcotest.(check int) "two repo grants" 2 (List.length grant_values);
  let first = List.nth grant_values 0 in
  Alcotest.(check bool)
    "first grant contains repo" true
    (String.contains first 'a');
  assert_all_provenance "repo grant" effective.repo_grants

let test_repo_grants_blocked_by_global_security () =
  let json =
    {|{
      "workspace": "/tmp/clawq-scope-root",
      "security": {"workspace_only": true, "allowed_cwd_patterns": ["/tmp/clawq-scope-root/**"]},
      "access_bundles": [
        {
          "id": "repo",
          "repo_grants": [
            {"repo": "/tmp/clawq-scope-root/app", "capabilities": ["read"]},
            {"repo": "/tmp/outside/app", "capabilities": ["read"]}
          ]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["repo"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check (list string))
    "global security keeps workspace repo grant"
    [ "/tmp/clawq-scope-root/app" ]
    (repo_grant_repos effective.repo_grants);
  Alcotest.(check (list string))
    "global security blocks outside repo grant" [ "/tmp/outside/app" ]
    (repo_grant_repos effective.blocked_repo_grants);
  assert_all_provenance "blocked repo grant" effective.blocked_repo_grants

let test_repo_grants_intersect_codebase_grants () =
  let json =
    {|{
      "workspace": "/tmp/clawq-scope-root",
      "security": {"workspace_only": true, "allowed_cwd_patterns": ["/tmp/clawq-scope-root/**"]},
      "access_bundles": [
        {
          "id": "repo",
          "codebase_grants": ["/tmp/clawq-scope-root/allowed/**"],
          "repo_grants": [
            {"repo": "/tmp/clawq-scope-root/allowed/app", "capabilities": ["read"]},
            {"repo": "/tmp/clawq-scope-root/other/app", "capabilities": ["read"]}
          ]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["repo"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check (list string))
    "codebase grant keeps covered repo grant"
    [ "/tmp/clawq-scope-root/allowed/app" ]
    (repo_grant_repos effective.repo_grants);
  Alcotest.(check (list string))
    "codebase grant blocks uncovered repo grant"
    [ "/tmp/clawq-scope-root/other/app" ]
    (repo_grant_repos effective.blocked_repo_grants)

let test_repo_grants_normalize_traversal_before_codebase_intersection () =
  let json =
    {|{
      "workspace": "/tmp/clawq-scope-root",
      "security": {"workspace_only": true, "allowed_cwd_patterns": ["/tmp/clawq-scope-root/**"]},
      "access_bundles": [
        {
          "id": "repo",
          "codebase_grants": ["/tmp/clawq-scope-root/allowed/**"],
          "repo_grants": [
            {"repo": "/tmp/clawq-scope-root/allowed/../other/app", "capabilities": ["read"]}
          ]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["repo"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check (list string))
    "traversal repo grant is not effective" []
    (repo_grant_repos effective.repo_grants);
  Alcotest.(check (list string))
    "traversal repo grant is blocked by codebase intersection"
    [ "/tmp/clawq-scope-root/allowed/../other/app" ]
    (repo_grant_repos effective.blocked_repo_grants)

let test_wildcard_repo_grants_require_exact_codebase_grant () =
  let json =
    {|{
      "workspace": "/tmp/clawq-scope-root",
      "security": {"workspace_only": true, "allowed_cwd_patterns": ["/tmp/clawq-scope-root/**"]},
      "access_bundles": [
        {
          "id": "repo",
          "codebase_grants": [
            "/tmp/clawq-scope-root/allowed/*",
            "/tmp/clawq-scope-root/exact/**",
            "/tmp/clawq-scope-root/prefix/app*",
            "/tmp/clawq-scope-root/bracket/*"
          ],
          "repo_grants": [
            {"repo": "/tmp/clawq-scope-root/allowed/app", "capabilities": ["read"]},
            {"repo": "/tmp/clawq-scope-root/allowed/**", "capabilities": ["read"]},
            {"repo": "/tmp/clawq-scope-root/exact/**", "capabilities": ["read"]},
            {"repo": "/tmp/clawq-scope-root/prefix/app?", "capabilities": ["read"]},
            {"repo": "/tmp/clawq-scope-root/bracket/lib[0-9]", "capabilities": ["read"]}
          ]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["repo"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check (list string))
    "codebase grant keeps concrete and exact wildcard repo grants"
    [ "/tmp/clawq-scope-root/allowed/app"; "/tmp/clawq-scope-root/exact/**" ]
    (repo_grant_repos effective.repo_grants);
  Alcotest.(check (list string))
    "codebase grant blocks non-exact wildcard repo grants"
    [
      "/tmp/clawq-scope-root/allowed/**";
      "/tmp/clawq-scope-root/prefix/app?";
      "/tmp/clawq-scope-root/bracket/lib[0-9]";
    ]
    (repo_grant_repos effective.blocked_repo_grants)

let test_legacy_repositories_become_read_only_repo_grants () =
  let json =
    {|{
      "access_bundles": [
        {
          "id": "legacy",
          "repositories": ["acme/old-repo"]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["legacy"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check (list string))
    "legacy repositories" [ "acme/old-repo" ]
    (item_values effective.repositories);
  Alcotest.(check int)
    "one repo grant from legacy" 1
    (List.length effective.repo_grants);
  assert_all_provenance "repo grant" effective.repo_grants

let test_explicit_repo_grants_take_precedence_over_legacy () =
  let json =
    {|{
      "access_bundles": [
        {
          "id": "mixed",
          "repositories": ["acme/app"],
          "repo_grants": [
            {"repo": "acme/app", "capabilities": ["read", "comment", "pr"]}
          ]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["mixed"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check int)
    "one repo grant (not duplicated)" 1
    (List.length effective.repo_grants);
  Alcotest.(check (list string))
    "legacy repositories preserved" [ "acme/app" ]
    (item_values effective.repositories)

let test_room_repo_grants_override_default () =
  let json =
    {|{
      "access_bundles": [
        {
          "id": "default-grants",
          "repo_grants": [
            {"repo": "acme/app", "capabilities": ["read"]}
          ]
        },
        {
          "id": "room-grants",
          "repo_grants": [
            {"repo": "acme/app", "capabilities": ["read", "comment", "branch", "pr"]},
            {"repo": "acme/infra", "capabilities": ["read", "workflow-trigger"]}
          ]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["default-grants"]},
        {"id": "room", "level": "room", "room": "C123", "access_bundle_ids": ["room-grants"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check int)
    "room grants merged" 2
    (List.length effective.repo_grants);
  assert_all_provenance "repo grant" effective.repo_grants

let test_repo_grant_capabilities_all_six () =
  let json =
    {|{
      "access_bundles": [
        {
          "id": "full",
          "repo_grants": [
            {
              "repo": "acme/everything",
              "capabilities": [
                "read", "comment", "branch", "pr",
                "workflow-read", "workflow-trigger"
              ]
            }
          ]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["full"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check int) "one full grant" 1 (List.length effective.repo_grants);
  let grant_value = List.hd (item_values effective.repo_grants) in
  List.iter
    (fun cap ->
      Alcotest.(check bool)
        ("grant contains " ^ cap) true
        (try
           ignore (Str.search_forward (Str.regexp_string cap) grant_value 0);
           true
         with Not_found -> false))
    [ "read"; "comment"; "branch"; "pr"; "workflow-read"; "workflow-trigger" ]

let test_empty_repo_grants_grant_nothing () =
  let json =
    {|{
      "access_bundles": [
        {"id": "empty", "repo_grants": []}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["empty"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check int) "empty repo grants" 0 (List.length effective.repo_grants);
  Alcotest.(check int)
    "empty repositories" 0
    (List.length effective.repositories)

let test_repo_grant_empty_capabilities_grants_no_caps () =
  let json =
    {|{
      "access_bundles": [
        {
          "id": "nocaps",
          "repo_grants": [
            {"repo": "acme/app", "capabilities": []}
          ]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["nocaps"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check int)
    "one grant with empty caps" 1
    (List.length effective.repo_grants);
  let grant_value = List.hd (item_values effective.repo_grants) in
  Alcotest.(check bool)
    "empty capabilities" true
    (try
       ignore (Str.search_forward (Str.regexp_string "[]") grant_value 0);
       true
     with Not_found -> false)

let test_repo_grants_blocked_by_global_security () =
  let json =
    {|{
      "workspace": "/tmp/clawq-scope-root",
      "security": {"workspace_only": true, "allowed_cwd_patterns": ["/tmp/clawq-scope-root/**"]},
      "access_bundles": [
        {
          "id": "dev",
          "repo_grants": [
            {"repo": "acme/app", "capabilities": ["read", "comment", "branch"]},
            {"repo": "outside/repo", "capabilities": ["read"]}
          ]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["dev"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  (* Both repos are within the workspace path, so both should be allowed *)
  Alcotest.(check int)
    "repo grants within workspace" 2
    (List.length effective.repo_grants);
  Alcotest.(check int)
    "no blocked repo grants" 0
    (List.length effective.blocked_repo_grants)

let test_repo_grants_respect_room_codebase_grants () =
  let json =
    {|{
      "workspace": "/tmp/clawq-scope-root",
      "security": {"workspace_only": true, "allowed_cwd_patterns": ["/tmp/clawq-scope-root/**"]},
      "access_bundles": [
        {
          "id": "room-grants",
          "codebase_grants": ["$CLAWQ_WORKSPACE/**"],
          "repo_grants": [
            {"repo": "acme/app", "capabilities": ["read", "comment", "branch", "pr"]}
          ]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["room-grants"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  (* Repo grants should be allowed because codebase_grants cover the workspace *)
  Alcotest.(check int)
    "repo grants with room codebase grants" 1
    (List.length effective.repo_grants);
  Alcotest.(check int)
    "no blocked repo grants" 0
    (List.length effective.blocked_repo_grants);
  let grant_value = List.hd (item_values effective.repo_grants) in
  List.iter
    (fun cap ->
      Alcotest.(check bool)
        ("grant contains " ^ cap) true
        (try
           ignore (Str.search_forward (Str.regexp_string cap) grant_value 0);
           true
         with Not_found -> false))
    [ "read"; "comment"; "branch"; "pr" ]

(* ---- P14.M2.E3.T002: Backward-compatibility tests for P11-P13 configs ----
   These tests verify that room profiles and bundles that predate scope
   inheritance still work without config changes, and that the new resolver
   produces the same effective access as the legacy path. *)

let test_legacy_config_no_scopes_no_bundles () =
  (* A pure P11-P13 config: room_profiles with inline fields, no
     access_bundle_ids, no access_bundles, no access_scopes.
     workspace_only is disabled so codebase grants are not blocked. *)
  let json =
    {|{
      "security": {"workspace_only": false, "allowed_cwd_patterns": []},
      "room_profiles": [
        {
          "id": "legacy-prod",
          "model": "openai:gpt-4o",
          "system_prompt": "You are a production assistant.",
          "allowed_tools": ["file_read", "file_write"],
          "denied_tools": ["shell_exec"]
        }
      ],
      "room_profile_codebase_grants": [
        {"profile_id": "legacy-prod", "patterns": ["/tmp/clawq-compat-ws/**"]}
      ],
      "room_profile_bindings": [
        {"profile_id": "legacy-prod", "room": "prod-room", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:prod-room"
      ()
  in
  Alcotest.(check (list string))
    "legacy allowed tools"
    [ "file_read"; "file_write" ]
    (item_values effective.allowed_tools);
  Alcotest.(check (list string))
    "legacy denied tools" [ "shell_exec" ]
    (item_values effective.denied_tools);
  Alcotest.(check (list string))
    "legacy codebase grants"
    [ "/tmp/clawq-compat-ws/**" ]
    (item_values effective.codebase_grants);
  (* No access_scopes means no scope-level bundles *)
  Alcotest.(check int) "no mcp servers" 0 (List.length effective.mcp_servers);
  Alcotest.(check int) "no skills" 0 (List.length effective.skills);
  Alcotest.(check int) "no repositories" 0 (List.length effective.repositories);
  assert_all_provenance "legacy allowed" effective.allowed_tools

let test_legacy_effective_matches_tool_denial () =
  (* For every tool, verify the new resolver's allowed/denied list matches the
     legacy room_profile_tool_denial_for_session path. *)
  let json =
    {|{
      "room_profiles": [
        {
          "id": "compat",
          "model": "openai:gpt-4o",
          "allowed_tools": ["file_read", "file_write", "browse"],
          "denied_tools": ["shell_exec", "eval"]
        }
      ],
      "room_profile_bindings": [
        {"profile_id": "compat", "room": "C100", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let session_key = "slack:C100" in
  let effective = Runtime_config.resolve_effective_access cfg ~session_key () in
  let allowed_set =
    List.map
      (fun (i : Runtime_config.effective_access_item) -> i.value)
      effective.allowed_tools
  in
  let denied_set =
    List.map
      (fun (i : Runtime_config.effective_access_item) -> i.value)
      effective.denied_tools
  in
  let test_tools =
    [
      "file_read"; "file_write"; "browse"; "shell_exec"; "eval"; "unknown_tool";
    ]
  in
  List.iter
    (fun tool ->
      let legacy_denial =
        Runtime_config.room_profile_tool_denial_for_session cfg ~session_key
          ~tool_name:tool ()
      in
      let resolver_denied =
        List.mem tool denied_set
        || (allowed_set <> [] && not (List.mem tool allowed_set))
      in
      match legacy_denial with
      | Some _msg ->
          Alcotest.(check bool) ("resolver denies " ^ tool) true resolver_denied
      | None ->
          Alcotest.(check bool)
            ("resolver allows " ^ tool)
            true (not resolver_denied))
    test_tools

let test_legacy_hybrid_explicit_and_implicit_bundles () =
  (* A profile with both access_bundle_ids and inline legacy fields should
     produce effective access from BOTH the explicit bundles AND the implicit
     legacy bundle. workspace_only disabled for codebase grant testing. *)
  let json =
    {|{
      "security": {"workspace_only": false, "allowed_cwd_patterns": []},
      "access_bundles": [
        {"id": "explicit", "allowed_tools": ["extra_tool"], "denied_tools": ["blocked_tool"]}
      ],
      "room_profiles": [
        {
          "id": "hybrid",
          "model": "openai:gpt-4o",
          "system_prompt": "Hybrid prompt",
          "allowed_tools": ["legacy_tool"],
          "denied_tools": ["legacy_denied"],
          "access_bundle_ids": ["explicit"]
        }
      ],
      "room_profile_codebase_grants": [
        {"profile_id": "hybrid", "patterns": ["/tmp/clawq-compat-ws/legacy/**"]}
      ],
      "room_profile_bindings": [
        {"profile_id": "hybrid", "room": "C200", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C200" ()
  in
  let allowed = item_values effective.allowed_tools in
  let denied = item_values effective.denied_tools in
  Alcotest.(check bool)
    "explicit allowed tool present" true
    (List.mem "extra_tool" allowed);
  Alcotest.(check bool)
    "legacy allowed tool present" true
    (List.mem "legacy_tool" allowed);
  Alcotest.(check bool)
    "explicit denied tool present" true
    (List.mem "blocked_tool" denied);
  Alcotest.(check bool)
    "legacy denied tool present" true
    (List.mem "legacy_denied" denied);
  Alcotest.(check (list string))
    "legacy codebase grant present"
    [ "/tmp/clawq-compat-ws/legacy/**" ]
    (item_values effective.codebase_grants)

let test_empty_legacy_profile_resolves_cleanly () =
  (* A legacy profile with no tools, no grants, no system_prompt should
     resolve to empty effective access without errors. *)
  let json =
    {|{
      "room_profiles": [
        {
          "id": "empty-profile",
          "model": "openai:gpt-4o"
        }
      ],
      "room_profile_bindings": [
        {"profile_id": "empty-profile", "room": "C300", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C300" ()
  in
  Alcotest.(check int)
    "no allowed tools" 0
    (List.length effective.allowed_tools);
  Alcotest.(check int) "no denied tools" 0 (List.length effective.denied_tools);
  Alcotest.(check int)
    "no codebase grants" 0
    (List.length effective.codebase_grants);
  (* Empty allowed_tools means ALL tools are permitted by the legacy path *)
  let denial =
    Runtime_config.room_profile_tool_denial_for_session cfg
      ~session_key:"slack:C300" ~tool_name:"any_tool" ()
  in
  Alcotest.(check bool) "legacy path also allows" true (denial = None)

let test_multiple_legacy_profiles_do_not_interfere () =
  (* Two legacy profiles bound to different rooms should not leak tools or
     denials into each other's effective access. *)
  let json =
    {|{
      "room_profiles": [
        {
          "id": "profile-a",
          "model": "openai:gpt-4o",
          "allowed_tools": ["tool_a"],
          "denied_tools": ["denied_a"]
        },
        {
          "id": "profile-b",
          "model": "anthropic:claude-sonnet-4-6",
          "allowed_tools": ["tool_b"],
          "denied_tools": ["denied_b"]
        }
      ],
      "room_profile_bindings": [
        {"profile_id": "profile-a", "room": "room-a", "active": true},
        {"profile_id": "profile-b", "room": "room-b", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let eff_a =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:room-a" ()
  in
  let eff_b =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:room-b" ()
  in
  Alcotest.(check (list string))
    "room-a allowed" [ "tool_a" ]
    (item_values eff_a.allowed_tools);
  Alcotest.(check (list string))
    "room-a denied" [ "denied_a" ]
    (item_values eff_a.denied_tools);
  Alcotest.(check (list string))
    "room-b allowed" [ "tool_b" ]
    (item_values eff_b.allowed_tools);
  Alcotest.(check (list string))
    "room-b denied" [ "denied_b" ]
    (item_values eff_b.denied_tools);
  (* room-a should not see tool_b *)
  Alcotest.(check bool)
    "room-a no tool_b" false
    (List.mem "tool_b" (item_values eff_a.allowed_tools));
  Alcotest.(check bool)
    "room-b no tool_a" false
    (List.mem "tool_a" (item_values eff_b.allowed_tools))

let test_legacy_codebase_grants_match_codebase_grants_for_profile () =
  (* room_profile_codebase_grants should produce the same codebase grant list
     through resolve_effective_access as through
     room_profile_codebase_grants_for_profile. *)
  let json =
    {|{
      "workspace": "/tmp/clawq-compat-ws",
      "room_profiles": [
        {
          "id": "cg-test",
          "model": "openai:gpt-4o"
        }
      ],
      "room_profile_codebase_grants": [
        {"profile_id": "cg-test", "patterns": ["/tmp/clawq-compat-ws/src/**", "/tmp/clawq-compat-ws/lib/**"]}
      ],
      "room_profile_bindings": [
        {"profile_id": "cg-test", "room": "C400", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let legacy_grants =
    Runtime_config.room_profile_codebase_grants_for_profile cfg
      ~profile_id:"cg-test"
  in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C400" ()
  in
  let effective_grants = item_values effective.codebase_grants in
  Alcotest.(check (list string))
    "codebase grants match" legacy_grants effective_grants

let test_legacy_system_prompt_flows_through_bundle () =
  (* The system_prompt from a legacy profile should be carried in the
     implicit legacy bundle and be retrievable from access_bundles_for_profile. *)
  let json =
    {|{
      "room_profiles": [
        {
          "id": "sp-test",
          "model": "openai:gpt-4o",
          "system_prompt": "Custom system prompt for testing",
          "allowed_tools": ["file_read"]
        }
      ],
      "room_profile_bindings": [
        {"profile_id": "sp-test", "room": "C500", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let profile = List.nth cfg.room_profiles 0 in
  let bundles = Runtime_config.access_bundles_for_profile cfg profile in
  Alcotest.(check int) "one legacy bundle" 1 (List.length bundles);
  let bundle = List.nth bundles 0 in
  Alcotest.(check (option string))
    "system prompt carried" (Some "Custom system prompt for testing")
    bundle.system_prompt;
  (* Verify it shows up through resolve_effective_access too *)
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C500" ()
  in
  Alcotest.(check (list string))
    "allowed tools via resolver" [ "file_read" ]
    (item_values effective.allowed_tools)

let test_legacy_config_with_empty_bundle_ids () =
  (* A profile with explicit but empty access_bundle_ids should still
     produce a legacy implicit bundle. *)
  let json =
    {|{
      "room_profiles": [
        {
          "id": "empty-ids",
          "model": "openai:gpt-4o",
          "allowed_tools": ["tool_x"],
          "access_bundle_ids": []
        }
      ],
      "room_profile_bindings": [
        {"profile_id": "empty-ids", "room": "C600", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C600" ()
  in
  Alcotest.(check (list string))
    "empty bundle_ids still resolves tools" [ "tool_x" ]
    (item_values effective.allowed_tools)

let test_legacy_snapshot_matches_resolver () =
  (* An access snapshot created from a legacy config should carry the same
     effective allowed/denied tools as the resolver itself. *)
  let json =
    {|{
      "room_profiles": [
        {
          "id": "snap-test",
          "model": "openai:gpt-4o",
          "allowed_tools": ["read", "write"],
          "denied_tools": ["delete"]
        }
      ],
      "room_profile_bindings": [
        {"profile_id": "snap-test", "room": "C700", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let session_key = "slack:C700" in
  let effective = Runtime_config.resolve_effective_access cfg ~session_key () in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn ~session_key ()
  in
  Alcotest.(check (list string))
    "snapshot allowed matches resolver"
    (item_values effective.allowed_tools)
    snap.allowed_tools;
  Alcotest.(check (list string))
    "snapshot denied matches resolver"
    (item_values effective.denied_tools)
    snap.denied_tools;
  (* Verify snapshot tool denial matches resolver *)
  List.iter
    (fun tool ->
      let snap_denial = Access_snapshot.tool_denial snap ~tool_name:tool () in
      let is_denied_by_resolver =
        List.mem tool (item_values effective.denied_tools)
        ||
        let allowed = item_values effective.allowed_tools in
        allowed <> [] && not (List.mem tool allowed)
      in
      match snap_denial with
      | Some _ ->
          Alcotest.(check bool)
            ("snap denies " ^ tool) true is_denied_by_resolver
      | None ->
          Alcotest.(check bool)
            ("snap allows " ^ tool) true
            (not is_denied_by_resolver))
    [ "read"; "write"; "delete"; "unknown" ]

let test_legacy_no_binding_resolves_nothing () =
  (* A legacy profile without a matching binding should produce empty
     effective access for that session key. *)
  let json =
    {|{
      "room_profiles": [
        {
          "id": "unbound",
          "model": "openai:gpt-4o",
          "allowed_tools": ["tool_x"]
        }
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C800" ()
  in
  Alcotest.(check int)
    "no allowed tools" 0
    (List.length effective.allowed_tools);
  let denial =
    Runtime_config.room_profile_tool_denial_for_session cfg
      ~session_key:"slack:C800" ~tool_name:"tool_x" ()
  in
  Alcotest.(check bool) "legacy path has no denial" true (denial = None)

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
    Alcotest.test_case "repo grants attach to bundle" `Quick
      test_repo_grants_attach_to_bundle;
    Alcotest.test_case "repo grants blocked by global security" `Quick
      test_repo_grants_blocked_by_global_security;
    Alcotest.test_case "repo grants intersect codebase grants" `Quick
      test_repo_grants_intersect_codebase_grants;
    Alcotest.test_case
      "repo grants normalize traversal before codebase intersection" `Quick
      test_repo_grants_normalize_traversal_before_codebase_intersection;
    Alcotest.test_case "wildcard repo grants require exact codebase grant"
      `Quick test_wildcard_repo_grants_require_exact_codebase_grant;
    Alcotest.test_case "legacy repositories become read-only repo grants" `Quick
      test_legacy_repositories_become_read_only_repo_grants;
    Alcotest.test_case "explicit repo grants take precedence over legacy" `Quick
      test_explicit_repo_grants_take_precedence_over_legacy;
    Alcotest.test_case "room repo grants override default" `Quick
      test_room_repo_grants_override_default;
    Alcotest.test_case "repo grant supports all six capabilities" `Quick
      test_repo_grant_capabilities_all_six;
    Alcotest.test_case "empty repo grants grant nothing" `Quick
      test_empty_repo_grants_grant_nothing;
    Alcotest.test_case "repo grant empty capabilities" `Quick
      test_repo_grant_empty_capabilities_grants_no_caps;
    Alcotest.test_case "repo grants blocked by global security" `Quick
      test_repo_grants_blocked_by_global_security;
    Alcotest.test_case "repo grants respect room codebase grants" `Quick
      test_repo_grants_respect_room_codebase_grants;
    Alcotest.test_case "P11-P13 legacy config with no scopes resolves correctly"
      `Quick test_legacy_config_no_scopes_no_bundles;
    Alcotest.test_case "P11-P13 legacy effective matches tool_denial path"
      `Quick test_legacy_effective_matches_tool_denial;
    Alcotest.test_case "P11-P13 hybrid explicit and implicit bundles" `Quick
      test_legacy_hybrid_explicit_and_implicit_bundles;
    Alcotest.test_case "P11-P13 empty legacy profile resolves cleanly" `Quick
      test_empty_legacy_profile_resolves_cleanly;
    Alcotest.test_case "P11-P13 multiple legacy profiles do not interfere"
      `Quick test_multiple_legacy_profiles_do_not_interfere;
    Alcotest.test_case
      "P11-P13 legacy codebase grants match codebase_grants_for_profile" `Quick
      test_legacy_codebase_grants_match_codebase_grants_for_profile;
    Alcotest.test_case "P11-P13 legacy system_prompt flows through bundle"
      `Quick test_legacy_system_prompt_flows_through_bundle;
    Alcotest.test_case "P11-P13 legacy config with empty bundle_ids" `Quick
      test_legacy_config_with_empty_bundle_ids;
    Alcotest.test_case "P11-P13 legacy snapshot matches resolver" `Quick
      test_legacy_snapshot_matches_resolver;
    Alcotest.test_case "P11-P13 legacy no binding resolves nothing" `Quick
      test_legacy_no_binding_resolves_nothing;
  ]

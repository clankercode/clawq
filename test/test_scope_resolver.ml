let item_values items =
  List.map
    (fun (item : Runtime_config.effective_access_item) -> item.value)
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

let test_instruction_records_carry_metadata_through_resolution () =
  let json =
    {|{
      "access_bundles": [
        {
          "id": "admin",
          "instructions": [
            {
              "text": "Always greet the user",
              "source_scope": "workspace",
              "author": "admin@example.com",
              "enabled": true,
              "edit_policy": "admin_only"
            }
          ]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["admin"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  (* Text-only view still works *)
  Alcotest.(check (list string))
    "instruction text in text view"
    [ "Always greet the user" ]
    (item_values effective.instructions);
  (* Full record view is populated *)
  Alcotest.(check int)
    "instruction_items count" 1
    (List.length effective.instruction_items);
  let item = List.hd effective.instruction_items in
  Alcotest.(check string)
    "instruction text" "Always greet the user"
    item.instruction.Runtime_config.text;
  Alcotest.(check string)
    "source_scope" "workspace" item.instruction.Runtime_config.source_scope;
  Alcotest.(check (option string))
    "author" (Some "admin@example.com") item.instruction.Runtime_config.author;
  Alcotest.(check bool) "enabled" true item.instruction.Runtime_config.enabled;
  Alcotest.(check bool) "locked" true item.instruction.Runtime_config.locked;
  (match item.instruction.Runtime_config.edit_policy with
  | Runtime_config.Admin_only -> ()
  | _ -> Alcotest.fail "expected Admin_only edit_policy");
  Alcotest.(check bool) "provenance non-empty" true (item.provenance <> [])

let test_disabled_instructions_are_filtered () =
  let json =
    {|{
      "access_bundles": [
        {
          "id": "mixed",
          "instructions": [
            {"text": "active instruction", "enabled": true},
            {"text": "disabled instruction", "enabled": false}
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
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  Alcotest.(check (list string))
    "only enabled instruction text" [ "active instruction" ]
    (item_values effective.instructions);
  Alcotest.(check int)
    "only enabled instruction_items" 1
    (List.length effective.instruction_items)

let test_legacy_string_instructions_parse_to_records () =
  let json =
    {|{
      "access_bundles": [
        {
          "id": "legacy",
          "instructions": ["old style instruction"]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["legacy"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  Alcotest.(check (list string))
    "legacy instruction text"
    [ "old style instruction" ]
    (item_values effective.instructions);
  Alcotest.(check int)
    "instruction_items count" 1
    (List.length effective.instruction_items);
  let item = List.hd effective.instruction_items in
  Alcotest.(check string)
    "default source_scope" "default"
    item.instruction.Runtime_config.source_scope;
  Alcotest.(check bool)
    "default enabled" true item.instruction.Runtime_config.enabled;
  Alcotest.(check bool)
    "default not locked" false item.instruction.Runtime_config.locked;
  match item.instruction.Runtime_config.edit_policy with
  | Runtime_config.Open -> ()
  | _ -> Alcotest.fail "expected Open edit_policy for legacy"

let test_instruction_digest_is_computed () =
  let ir = Runtime_config.default_instruction_record ~text:"hello world" () in
  let digest = Runtime_config.instruction_record_digest ir in
  Alcotest.(check bool)
    "digest is non-empty hex" true
    (String.length digest = 64);
  (* Same text gives same digest *)
  Alcotest.(check string)
    "deterministic digest" digest
    (Runtime_config.instruction_record_digest ir)

let test_instruction_locked_reflects_edit_policy () =
  let ir_locked =
    {
      (Runtime_config.default_instruction_record ~text:"a" ()) with
      edit_policy = Locked;
      locked = true;
    }
  in
  let ir_admin =
    {
      (Runtime_config.default_instruction_record ~text:"b" ()) with
      edit_policy = Admin_only;
      locked = true;
    }
  in
  let ir_open =
    {
      (Runtime_config.default_instruction_record ~text:"c" ()) with
      edit_policy = Open;
      locked = false;
    }
  in
  Alcotest.(check bool)
    "locked is active" true
    (Runtime_config.instruction_record_is_active ir_locked);
  Alcotest.(check bool)
    "admin_only is active" true
    (Runtime_config.instruction_record_is_active ir_admin);
  Alcotest.(check bool)
    "open is active" true
    (Runtime_config.instruction_record_is_active ir_open);
  Alcotest.(check string)
    "locked to_string" "locked"
    (Runtime_config.instruction_edit_policy_to_string Locked);
  Alcotest.(check string)
    "admin_only to_string" "admin_only"
    (Runtime_config.instruction_edit_policy_to_string Admin_only);
  Alcotest.(check string)
    "open to_string" "open"
    (Runtime_config.instruction_edit_policy_to_string Open);
  let check_policy label expected actual =
    match actual with
    | Some p when p = expected -> ()
    | Some _ -> Alcotest.fail (label ^ ": wrong policy")
    | None -> Alcotest.fail (label ^ ": expected Some, got None")
  in
  check_policy "locked roundtrip" Runtime_config.Locked
    (Runtime_config.instruction_edit_policy_of_string "locked");
  check_policy "admin_only roundtrip" Runtime_config.Admin_only
    (Runtime_config.instruction_edit_policy_of_string "admin_only");
  check_policy "open roundtrip" Runtime_config.Open
    (Runtime_config.instruction_edit_policy_of_string "open");
  Alcotest.(check bool)
    "invalid returns None" true
    (Runtime_config.instruction_edit_policy_of_string "bogus" = None)

let test_instruction_records_from_multiple_bundles_merge () =
  let json =
    {|{
      "access_bundles": [
        {
          "id": "base",
          "instructions": [
            {"text": "base instruction", "source_scope": "default"}
          ]
        },
        {
          "id": "room",
          "instructions": [
            {"text": "room instruction", "source_scope": "room", "author": "room-admin"}
          ]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]},
        {"id": "room", "level": "room", "room": "C123", "access_bundle_ids": ["room"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123"
  in
  Alcotest.(check (list string))
    "both instruction texts"
    [ "base instruction"; "room instruction" ]
    (item_values effective.instructions);
  Alcotest.(check int)
    "instruction_items count" 2
    (List.length effective.instruction_items);
  let room_item =
    List.find
      (fun (i : Runtime_config.effective_instruction_item) ->
        i.instruction.Runtime_config.text = "room instruction")
      effective.instruction_items
  in
  Alcotest.(check string)
    "room instruction scope" "room"
    room_item.instruction.Runtime_config.source_scope;
  Alcotest.(check (option string))
    "room instruction author" (Some "room-admin")
    room_item.instruction.Runtime_config.author;
  Alcotest.(check bool)
    "room instruction provenance" true
    (room_item.provenance <> [])

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
    Alcotest.test_case "instruction records carry metadata" `Quick
      test_instruction_records_carry_metadata_through_resolution;
    Alcotest.test_case "disabled instructions are filtered" `Quick
      test_disabled_instructions_are_filtered;
    Alcotest.test_case "legacy string instructions parse to records" `Quick
      test_legacy_string_instructions_parse_to_records;
    Alcotest.test_case "instruction digest is computed" `Quick
      test_instruction_digest_is_computed;
    Alcotest.test_case "instruction locked reflects edit policy" `Quick
      test_instruction_locked_reflects_edit_policy;
    Alcotest.test_case "instruction records from multiple bundles merge" `Quick
      test_instruction_records_from_multiple_bundles_merge;
  ]

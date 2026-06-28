let parse json = Config_loader.parse_config (Yojson.Safe.from_string json)

let test_create_basic_explanation () =
  let json =
    {|{
      "workspace": "/tmp/test-explain",
      "access_bundles": [
        {"id": "base", "allowed_tools": ["file_read", "shell_exec"],
         "denied_tools": ["deploy"],
         "instructions": ["Be helpful"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check int) "1 matching scope" 1 (List.length explanation.scopes);
  Alcotest.(check (list string))
    "allowed tools"
    [ "file_read"; "shell_exec" ]
    (List.map
       (fun (ie : Access_explanation.item_explanation) -> ie.value)
       explanation.allowed_tools);
  Alcotest.(check (list string))
    "denied tools" [ "deploy" ]
    (List.map
       (fun (ie : Access_explanation.item_explanation) -> ie.value)
       explanation.denied_tools);
  Alcotest.(check (list string))
    "instructions" [ "Be helpful" ] explanation.instructions;
  Alcotest.(check bool)
    "summary non-empty" true
    (String.length explanation.summary > 0)

let test_scopes_include_matching_only () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "access_bundles": [
        {"id": "b1", "allowed_tools": ["tool_a"]},
        {"id": "b2", "allowed_tools": ["tool_b"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]},
        {"id": "slack-room", "level": "room", "channel": "slack", "room": "C123", "access_bundle_ids": ["b2"]},
        {"id": "discord-room", "level": "room", "channel": "discord", "room": "C456", "access_bundle_ids": ["b2"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check int)
    "2 matching scopes (default + slack-room)" 2
    (List.length explanation.scopes);
  let scope_ids =
    List.map
      (fun (si : Access_explanation.scope_info) -> si.id)
      explanation.scopes
  in
  Alcotest.(check bool) "has default scope" true (List.mem "default" scope_ids);
  Alcotest.(check bool)
    "has slack-room scope" true
    (List.mem "slack-room" scope_ids);
  Alcotest.(check bool)
    "no discord-room scope" false
    (List.mem "discord-room" scope_ids)

let test_credential_handles_redacted () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "credential_handles": [
        {"id": "github-app:main", "provider": {"type": "env_var", "name": "GITHUB_TOKEN"}, "description": "GitHub API token"},
        {"id": "slack-bot", "provider": {"type": "file", "path": "/tmp/token"}, "status": "deleted"}
      ],
      "access_bundles": [
        {"id": "b1", "credential_handles": ["github-app:main"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check int)
    "1 active credential handle" 1
    (List.length explanation.credential_handles);
  let ch = List.hd explanation.credential_handles in
  Alcotest.(check string) "credential id" "github-app:main" ch.id;
  Alcotest.(check string) "provider type" "env_var" ch.provider_type;
  Alcotest.(check (option string))
    "description" (Some "GitHub API token") ch.description;
  Alcotest.(check string) "status" "active" ch.status

let test_credential_handles_no_secrets () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "credential_handles": [
        {"id": "api-key", "provider": {"type": "env_var", "name": "SECRET_KEY"}}
      ],
      "access_bundles": [
        {"id": "b1", "credential_handles": ["api-key"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  let json_out = Access_explanation.to_json explanation in
  let json_str = Yojson.Safe.to_string json_out in
  Alcotest.(check bool)
    "no secret value in json" true
    (not
       (String.contains json_str 'S'
       && String.contains json_str 'E'
       && String.contains json_str 'C'
       && String.contains json_str 'R'
       && String.contains json_str 'E'
       && String.contains json_str 'T'));
  Alcotest.(check bool)
    "has credential handle id" true
    (let re = Str.regexp_string "api-key" in
     try
       ignore (Str.search_forward re json_str 0);
       true
     with Not_found -> false)

let test_provenance_tracked () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "access_bundles": [
        {"id": "base", "allowed_tools": ["tool_a"]},
        {"id": "room", "allowed_tools": ["tool_b"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]},
        {"id": "room-scope", "level": "room", "room": "C123", "access_bundle_ids": ["room"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  let tool_a =
    List.find
      (fun (ie : Access_explanation.item_explanation) -> ie.value = "tool_a")
      explanation.allowed_tools
  in
  Alcotest.(check bool) "tool_a has sources" true (tool_a.sources <> []);
  let first_source = List.hd tool_a.sources in
  Alcotest.(check string)
    "tool_a first source layer" "default" first_source.layer

let test_blocked_codebase_grants () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "security": {"workspace_only": true, "allowed_cwd_patterns": ["/tmp/test/**"]},
      "access_bundles": [
        {"id": "b1", "codebase_grants": ["/tmp/test/**", "/outside/**"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check bool)
    "has allowed codebase grant" true
    (List.length explanation.codebase_grants > 0);
  Alcotest.(check bool)
    "has blocked codebase grant" true
    (List.length explanation.blocked_codebase_grants > 0)

let test_to_json_structure () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "access_bundles": [
        {"id": "b1", "allowed_tools": ["t1"], "mcp_servers": ["srv1"],
         "skills": ["sk1"], "repositories": ["repo1"],
         "domains": ["example.com"], "memory_grants": ["mem1"],
         "budget_refs": ["budget1"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  let json_out = Access_explanation.to_json explanation in
  let open Yojson.Safe.Util in
  let scopes = json_out |> member "scopes" |> to_list in
  Alcotest.(check bool) "has scopes" true (List.length scopes > 0);
  let allowed = json_out |> member "allowed_tools" |> to_list in
  Alcotest.(check int) "1 allowed tool" 1 (List.length allowed);
  let servers = json_out |> member "mcp_servers" |> to_list in
  Alcotest.(check int) "1 mcp server" 1 (List.length servers);
  let skills = json_out |> member "skills" |> to_list in
  Alcotest.(check int) "1 skill" 1 (List.length skills);
  let repos = json_out |> member "repositories" |> to_list in
  Alcotest.(check int) "1 repo" 1 (List.length repos);
  let domains = json_out |> member "domains" |> to_list in
  Alcotest.(check int) "1 domain" 1 (List.length domains);
  let mem = json_out |> member "memory_grants" |> to_list in
  Alcotest.(check int) "1 memory grant" 1 (List.length mem);
  let budget = json_out |> member "budget_refs" |> to_list in
  Alcotest.(check int) "1 budget ref" 1 (List.length budget);
  let summary = json_out |> member "summary" |> to_string in
  Alcotest.(check bool) "has summary" true (String.length summary > 0)

let test_to_text_readable () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "access_bundles": [
        {"id": "b1", "allowed_tools": ["file_read", "shell_exec"],
         "denied_tools": ["deploy"],
         "instructions": ["Be helpful", "Follow security rules"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  let text = Access_explanation.to_text explanation in
  Alcotest.(check bool)
    "text contains Effective Access" true
    (let re = Str.regexp_string "Effective Access" in
     try
       ignore (Str.search_forward re text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "text contains Allowed Tools" true
    (let re = Str.regexp_string "Allowed Tools" in
     try
       ignore (Str.search_forward re text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "text contains Denied Tools" true
    (let re = Str.regexp_string "Denied Tools" in
     try
       ignore (Str.search_forward re text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "text contains Instructions" true
    (let re = Str.regexp_string "Instructions" in
     try
       ignore (Str.search_forward re text 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "text contains Summary" true
    (let re = Str.regexp_string "Summary" in
     try
       ignore (Str.search_forward re text 0);
       true
     with Not_found -> false)

let test_empty_config () =
  let json = {|{"workspace": "/tmp/test"}|} in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check int) "no scopes" 0 (List.length explanation.scopes);
  Alcotest.(check int)
    "no allowed tools" 0
    (List.length explanation.allowed_tools);
  Alcotest.(check int)
    "no denied tools" 0
    (List.length explanation.denied_tools);
  Alcotest.(check int)
    "no instructions" 0
    (List.length explanation.instructions);
  Alcotest.(check bool)
    "summary exists" true
    (String.length explanation.summary > 0)

let test_multiple_scopes_merge () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "access_bundles": [
        {"id": "base", "allowed_tools": ["tool_a"]},
        {"id": "channel", "allowed_tools": ["tool_b"]},
        {"id": "room", "allowed_tools": ["tool_c"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]},
        {"id": "slack", "level": "channel", "channel": "slack", "access_bundle_ids": ["channel"]},
        {"id": "room-c123", "level": "room", "room": "C123", "access_bundle_ids": ["room"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check int) "3 matching scopes" 3 (List.length explanation.scopes);
  Alcotest.(check (list string))
    "all tools merged"
    [ "tool_a"; "tool_b"; "tool_c" ]
    (List.map
       (fun (ie : Access_explanation.item_explanation) -> ie.value)
       explanation.allowed_tools)

let test_deny_overrides_allow () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "access_bundles": [
        {"id": "base", "allowed_tools": ["tool_a", "tool_b"]},
        {"id": "room", "denied_tools": ["tool_b"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]},
        {"id": "room-c123", "level": "room", "room": "C123", "access_bundle_ids": ["room"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check (list string))
    "tool_b removed from allowed" [ "tool_a" ]
    (List.map
       (fun (ie : Access_explanation.item_explanation) -> ie.value)
       explanation.allowed_tools);
  Alcotest.(check (list string))
    "tool_b in denied" [ "tool_b" ]
    (List.map
       (fun (ie : Access_explanation.item_explanation) -> ie.value)
       explanation.denied_tools)

let test_credential_provider_types () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "credential_handles": [
        {"id": "env-cred", "provider": {"type": "env_var", "name": "VAR"}},
        {"id": "file-cred", "provider": {"type": "file", "path": "/tmp/f"}},
        {"id": "prompt-cred", "provider": {"type": "prompt", "description": "Enter key"}}
      ],
      "access_bundles": [
        {"id": "b1", "credential_handles": ["env-cred", "file-cred", "prompt-cred"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check int)
    "3 credential handles" 3
    (List.length explanation.credential_handles);
  let by_id id =
    List.find
      (fun (ci : Access_explanation.credential_info) -> ci.id = id)
      explanation.credential_handles
  in
  Alcotest.(check string)
    "env-cred type" "env_var" (by_id "env-cred").provider_type;
  Alcotest.(check string)
    "file-cred type" "file" (by_id "file-cred").provider_type;
  Alcotest.(check string)
    "prompt-cred type" "prompt" (by_id "prompt-cred").provider_type

let test_summary_format () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "access_bundles": [
        {"id": "b1", "allowed_tools": ["t1", "t2"], "denied_tools": ["d1"],
         "mcp_servers": ["srv1"], "skills": ["sk1"],
         "repositories": ["repo1"], "domains": ["dom1"],
         "credential_handles": ["cred1"], "instructions": ["inst1"],
         "memory_grants": ["mem1"], "budget_refs": ["budget1"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  let summary = explanation.summary in
  Alcotest.(check bool)
    "summary has tools" true
    (let re = Str.regexp_string "tools:" in
     try
       ignore (Str.search_forward re summary 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "summary has servers" true
    (let re = Str.regexp_string "servers:" in
     try
       ignore (Str.search_forward re summary 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "summary has skills" true
    (let re = Str.regexp_string "skills:" in
     try
       ignore (Str.search_forward re summary 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "summary has credentials" true
    (let re = Str.regexp_string "credentials:" in
     try
       ignore (Str.search_forward re summary 0);
       true
     with Not_found -> false)

let test_json_item_sources () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "access_bundles": [
        {"id": "b1", "allowed_tools": ["tool_a"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  let json_out = Access_explanation.to_json explanation in
  let open Yojson.Safe.Util in
  let allowed = json_out |> member "allowed_tools" |> to_list in
  let first = List.hd allowed in
  let sources = first |> member "sources" |> to_list in
  Alcotest.(check bool) "item has sources" true (List.length sources > 0);
  let first_source = List.hd sources in
  let layer = first_source |> member "layer" |> to_string in
  Alcotest.(check string) "source layer" "default" layer

let test_room_profile_credential_handles () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "credential_handles": [
        {"id": "room-cred", "provider": {"type": "env_var", "name": "ROOM_SECRET"}}
      ],
      "access_bundles": [
        {"id": "room-bundle", "credential_handles": ["room-cred"]}
      ],
      "room_profiles": [
        {"id": "vip", "model": "openai:gpt-5.4", "access_bundle_ids": ["room-bundle"]}
      ],
      "room_profile_bindings": [
        {"profile_id": "vip", "room": "C123", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check int)
    "1 credential handle from room profile" 1
    (List.length explanation.credential_handles);
  Alcotest.(check string)
    "credential id" "room-cred" (List.hd explanation.credential_handles).id

let test_instructions_redacted_in_text () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "access_bundles": [
        {"id": "b1", "instructions": ["This is a very long instruction that should be truncated in the text output because it exceeds the maximum length"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  let text = Access_explanation.to_text explanation in
  Alcotest.(check bool)
    "text contains truncated instruction" true
    (let re = Str.regexp_string "..." in
     try
       ignore (Str.search_forward re text 0);
       true
     with Not_found -> false)

let test_scope_info_fields () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "access_bundles": [
        {"id": "b1", "allowed_tools": ["tool_a"]}
      ],
      "access_scopes": [
        {"id": "test-scope", "level": "room", "workspace": "/tmp/test",
         "channel": "slack", "room": "C123", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  let scope = List.hd explanation.scopes in
  Alcotest.(check string) "scope id" "test-scope" scope.id;
  Alcotest.(check string) "scope level" "room" scope.level;
  Alcotest.(check (option string))
    "scope workspace" (Some "/tmp/test") scope.workspace;
  Alcotest.(check (option string)) "scope channel" (Some "slack") scope.channel;
  Alcotest.(check (option string)) "scope room" (Some "C123") scope.room;
  Alcotest.(check (list string)) "scope bundle_ids" [ "b1" ] scope.bundle_ids

let test_non_inherited_credentials_not_exposed () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "credential_handles": [
        {"id": "inherited-cred", "provider": {"type": "env_var", "name": "INHERITED"}, "description": "Should be visible"},
        {"id": "non-inherited-cred", "provider": {"type": "env_var", "name": "NOT_INHERITED"}, "description": "Should NOT be visible"}
      ],
      "access_bundles": [
        {"id": "b1", "allowed_tools": ["tool_a"], "credential_handles": ["inherited-cred"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check int)
    "only 1 credential handle exposed" 1
    (List.length explanation.credential_handles);
  let exposed_ids =
    List.map
      (fun (ci : Access_explanation.credential_info) -> ci.id)
      explanation.credential_handles
  in
  Alcotest.(check bool)
    "inherited credential is exposed" true
    (List.mem "inherited-cred" exposed_ids);
  Alcotest.(check bool)
    "non-inherited credential is NOT exposed" false
    (List.mem "non-inherited-cred" exposed_ids);
  let json_out = Access_explanation.to_json explanation in
  let json_str = Yojson.Safe.to_string json_out in
  Alcotest.(check bool)
    "json does not contain non-inherited credential id" false
    (let re = Str.regexp_string "non-inherited-cred" in
     try
       ignore (Str.search_forward re json_str 0);
       true
     with Not_found -> false)

let suite =
  [
    Alcotest.test_case "create basic explanation" `Quick
      test_create_basic_explanation;
    Alcotest.test_case "scopes include matching only" `Quick
      test_scopes_include_matching_only;
    Alcotest.test_case "credential handles redacted" `Quick
      test_credential_handles_redacted;
    Alcotest.test_case "credential handles no secrets" `Quick
      test_credential_handles_no_secrets;
    Alcotest.test_case "provenance tracked" `Quick test_provenance_tracked;
    Alcotest.test_case "blocked codebase grants" `Quick
      test_blocked_codebase_grants;
    Alcotest.test_case "to_json structure" `Quick test_to_json_structure;
    Alcotest.test_case "to_text readable" `Quick test_to_text_readable;
    Alcotest.test_case "empty config" `Quick test_empty_config;
    Alcotest.test_case "multiple scopes merge" `Quick test_multiple_scopes_merge;
    Alcotest.test_case "deny overrides allow" `Quick test_deny_overrides_allow;
    Alcotest.test_case "credential provider types" `Quick
      test_credential_provider_types;
    Alcotest.test_case "summary format" `Quick test_summary_format;
    Alcotest.test_case "json item sources" `Quick test_json_item_sources;
    Alcotest.test_case "room profile credential handles" `Quick
      test_room_profile_credential_handles;
    Alcotest.test_case "instructions redacted in text" `Quick
      test_instructions_redacted_in_text;
    Alcotest.test_case "scope info fields" `Quick test_scope_info_fields;
    Alcotest.test_case "non-inherited credentials not exposed" `Quick
      test_non_inherited_credentials_not_exposed;
  ]

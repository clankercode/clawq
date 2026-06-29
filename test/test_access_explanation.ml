let parse json = Config_loader.parse_config (Yojson.Safe.from_string json)

let item_values items =
  List.map (fun (ie : Access_explanation.item_explanation) -> ie.value) items

let repo_grant_repos items =
  List.filter_map
    (fun (ie : Access_explanation.item_explanation) ->
      try
        let open Yojson.Safe.Util in
        Some (Yojson.Safe.from_string ie.value |> member "repo" |> to_string)
      with _ -> None)
    items

let string_contains haystack needle =
  let re = Str.regexp_string needle in
  try
    ignore (Str.search_forward re haystack 0);
    true
  with Not_found -> false

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

let test_repo_grants_in_json_and_text () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "security": {"workspace_only": true, "allowed_cwd_patterns": ["/tmp/test/**"]},
      "access_bundles": [
        {
          "id": "b1",
          "codebase_grants": ["/tmp/test/allowed/**"],
          "repo_grants": [
            {"repo": "/tmp/test/allowed/app", "capabilities": ["read"]},
            {"repo": "/tmp/test/blocked/app", "capabilities": ["read"]}
          ]
        }
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
  Alcotest.(check (list string))
    "allowed repo grants"
    [ "/tmp/test/allowed/app" ]
    (repo_grant_repos explanation.repo_grants);
  Alcotest.(check (list string))
    "blocked repo grants"
    [ "/tmp/test/blocked/app" ]
    (repo_grant_repos explanation.blocked_repo_grants);
  let json_out = Access_explanation.to_json explanation in
  let open Yojson.Safe.Util in
  Alcotest.(check int)
    "json repo_grants" 1
    (json_out |> member "repo_grants" |> to_list |> List.length);
  Alcotest.(check int)
    "json blocked_repo_grants" 1
    (json_out |> member "blocked_repo_grants" |> to_list |> List.length);
  let text = Access_explanation.to_text explanation in
  Alcotest.(check bool)
    "text contains Repo Grants" true
    (string_contains text "Repo Grants");
  Alcotest.(check bool)
    "text contains Blocked Repo Grants" true
    (string_contains text "Blocked Repo Grants")

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

let test_unbound_room_explanation () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "access_bundles": [
        {"id": "base", "allowed_tools": ["file_read"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
      ]
    }|}
  in
  let cfg = parse json in
  (* Session key for a room that has no matching room-level scope *)
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:UNBOUND" ()
  in
  (* Should only have the default scope, not a room scope *)
  Alcotest.(check int)
    "1 matching scope (default only)" 1
    (List.length explanation.scopes);
  let scope_ids =
    List.map
      (fun (si : Access_explanation.scope_info) -> si.id)
      explanation.scopes
  in
  Alcotest.(check bool) "has default scope" true (List.mem "default" scope_ids);
  Alcotest.(check bool)
    "no room scope for unbound room" true
    (not
       (List.exists
          (fun (si : Access_explanation.scope_info) -> si.room = Some "UNBOUND")
          explanation.scopes));
  (* JSON output must not leak anything about the unbound room *)
  let json_out = Access_explanation.to_json explanation in
  let json_str = Yojson.Safe.to_string json_out in
  Alcotest.(check bool)
    "json does not mention unbound room" true
    (not (Test_helpers.string_contains json_str "UNBOUND"))

let test_unbound_room_no_credentials_leaked () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "credential_handles": [
        {"id": "room-secret", "provider": {"type": "env_var", "name": "ROOM_SECRET"}, "description": "Room API key"}
      ],
      "access_bundles": [
        {"id": "room-bundle", "allowed_tools": ["room_tool"], "credential_handles": ["room-secret"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": []},
        {"id": "bound-room", "level": "room", "room": "C123", "access_bundle_ids": ["room-bundle"]}
      ]
    }|}
  in
  let cfg = parse json in
  (* Accessing from an unbound room — should NOT see the bound room's creds *)
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:UNBOUND" ()
  in
  Alcotest.(check int)
    "unbound room has no credential handles" 0
    (List.length explanation.credential_handles);
  let json_out = Access_explanation.to_json explanation in
  let json_str = Yojson.Safe.to_string json_out in
  Alcotest.(check bool)
    "json does not contain room-secret credential" true
    (not (Test_helpers.string_contains json_str "room-secret"));
  Alcotest.(check bool)
    "json does not contain ROOM_SECRET env var name" true
    (not (Test_helpers.string_contains json_str "ROOM_SECRET"))

let test_deleted_profile_no_credential_leak () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "credential_handles": [
        {"id": "profile-cred", "provider": {"type": "env_var", "name": "PROFILE_SECRET"}, "description": "Profile API key"}
      ],
      "access_bundles": [
        {"id": "profile-bundle", "allowed_tools": ["profile_tool"], "credential_handles": ["profile-cred"]}
      ],
      "room_profiles": [
        {"id": "coding", "model": "openai:gpt-5.4", "access_bundle_ids": ["profile-bundle"], "status": "deleted"}
      ],
      "room_profile_bindings": [
        {"profile_id": "coding", "room": "slack:C123", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  (* Deleted profile should not contribute credential handles *)
  Alcotest.(check int)
    "no credential handles from deleted profile" 0
    (List.length explanation.credential_handles);
  let json_out = Access_explanation.to_json explanation in
  let json_str = Yojson.Safe.to_string json_out in
  Alcotest.(check bool)
    "json does not contain profile-cred" true
    (not (Test_helpers.string_contains json_str "profile-cred"));
  Alcotest.(check bool)
    "json does not contain PROFILE_SECRET env var name" true
    (not (Test_helpers.string_contains json_str "PROFILE_SECRET"))

let test_encrypted_credential_redaction () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "credential_handles": [
        {"id": "enc-cred", "provider": {"type": "encrypted", "cipher_text": "$ENC:dGVzdF9jaXBoZXJ0ZXh0XzEyMzQ1"}, "description": "Encrypted API key"}
      ],
      "access_bundles": [
        {"id": "b1", "credential_handles": ["enc-cred"]}
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
    "1 credential handle" 1
    (List.length explanation.credential_handles);
  let ch = List.hd explanation.credential_handles in
  Alcotest.(check string) "provider type" "encrypted" ch.provider_type;
  (* JSON must not contain the cipher_text value *)
  let json_out = Access_explanation.to_json explanation in
  let json_str = Yojson.Safe.to_string json_out in
  Alcotest.(check bool)
    "json does not contain cipher_text value" true
    (not (Test_helpers.string_contains json_str "dGVzdF9jaXBoZXJ0ZXh0XzEyMzQ1"));
  Alcotest.(check bool)
    "json does not contain ENC prefix" true
    (not (Test_helpers.string_contains json_str "$ENC:"));
  (* Text must not contain the cipher_text value *)
  let text = Access_explanation.to_text explanation in
  Alcotest.(check bool)
    "text does not contain cipher_text value" true
    (not (Test_helpers.string_contains text "dGVzdF9jaXBoZXJ0ZXh0XzEyMzQ1"))

let test_file_credential_redaction () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "credential_handles": [
        {"id": "file-cred", "provider": {"type": "file", "path": "/home/user/.secrets/api_key.txt"}, "description": "File-based key"}
      ],
      "access_bundles": [
        {"id": "b1", "credential_handles": ["file-cred"]}
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
  let ch = List.hd explanation.credential_handles in
  Alcotest.(check string) "provider type" "file" ch.provider_type;
  (* JSON must not contain the file path *)
  let json_out = Access_explanation.to_json explanation in
  let json_str = Yojson.Safe.to_string json_out in
  Alcotest.(check bool)
    "json does not contain secret file path" true
    (not
       (Test_helpers.string_contains json_str "/home/user/.secrets/api_key.txt"));
  (* Text must not contain the file path *)
  let text = Access_explanation.to_text explanation in
  Alcotest.(check bool)
    "text does not contain secret file path" true
    (not (Test_helpers.string_contains text "/home/user/.secrets/api_key.txt"))

let test_env_var_credential_redaction () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "credential_handles": [
        {"id": "env-cred", "provider": {"type": "env_var", "name": "AWS_SECRET_ACCESS_KEY"}, "description": "AWS secret"}
      ],
      "access_bundles": [
        {"id": "b1", "credential_handles": ["env-cred"]}
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
  let ch = List.hd explanation.credential_handles in
  Alcotest.(check string) "provider type" "env_var" ch.provider_type;
  (* JSON must not contain the env var name *)
  let json_out = Access_explanation.to_json explanation in
  let json_str = Yojson.Safe.to_string json_out in
  Alcotest.(check bool)
    "json does not contain env var name" true
    (not (Test_helpers.string_contains json_str "AWS_SECRET_ACCESS_KEY"));
  Alcotest.(check bool)
    "json contains credential id" true
    (Test_helpers.string_contains json_str "env-cred")

let test_inactive_bundle_scope_no_credential_leak () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "credential_handles": [
        {"id": "leaked-cred", "provider": {"type": "env_var", "name": "LEAKED_SECRET"}}
      ],
      "access_bundles": [
        {"id": "inactive-bundle", "status": "deleted", "credential_handles": ["leaked-cred"]},
        {"id": "active-bundle", "allowed_tools": ["tool_a"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["active-bundle"]},
        {"id": "deleted-scope", "level": "room", "room": "C123", "access_bundle_ids": ["inactive-bundle"], "status": "deleted"}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check int)
    "no credential handles from inactive bundle" 0
    (List.length explanation.credential_handles);
  let json_out = Access_explanation.to_json explanation in
  let json_str = Yojson.Safe.to_string json_out in
  Alcotest.(check bool)
    "json does not contain leaked-cred" true
    (not (Test_helpers.string_contains json_str "leaked-cred"));
  Alcotest.(check bool)
    "json does not contain LEAKED_SECRET" true
    (not (Test_helpers.string_contains json_str "LEAKED_SECRET"))

let test_egress_rules_in_explanation () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "access_bundles": [
        {
          "id": "b1",
          "allowed_tools": ["tool_a"],
          "egress_rules": [
            {"host": "api.example.com", "action": "allow", "log_policy": "log"},
            {"host": "*.blocked.com", "action": "deny", "log_policy": "no_log"}
          ]
        }
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
  Alcotest.(check int) "2 egress rules" 2 (List.length explanation.egress_rules);
  let first = List.hd explanation.egress_rules in
  Alcotest.(check string) "first rule host" "api.example.com" first.host;
  Alcotest.(check int) "first rule index" 0 first.index;
  let second = List.nth explanation.egress_rules 1 in
  Alcotest.(check string) "second rule host" "*.blocked.com" second.host;
  Alcotest.(check int) "second rule index" 1 second.index

let test_egress_rules_json_output () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "access_bundles": [
        {
          "id": "b1",
          "egress_rules": [
            {"host": "api.example.com", "path": "/v1/*", "method": "GET", "action": "allow", "log_policy": "log"},
            {"host": "*", "action": "deny", "log_policy": "no_log"}
          ]
        }
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
  let rules = json_out |> member "egress_rules" |> to_list in
  Alcotest.(check int) "json has 2 egress rules" 2 (List.length rules);
  let first = List.hd rules in
  Alcotest.(check string)
    "first rule host" "api.example.com"
    (first |> member "host" |> to_string);
  Alcotest.(check string)
    "first rule path" "/v1/*"
    (first |> member "path" |> to_string);
  Alcotest.(check string)
    "first rule method" "GET"
    (first |> member "method" |> to_string);
  Alcotest.(check string)
    "first rule action" "allow"
    (first |> member "action" |> to_string);
  Alcotest.(check string)
    "first rule log_policy" "log"
    (first |> member "log_policy" |> to_string);
  Alcotest.(check int) "first rule index" 0 (first |> member "index" |> to_int);
  let second = List.nth rules 1 in
  Alcotest.(check string)
    "second rule host" "*"
    (second |> member "host" |> to_string);
  Alcotest.(check string)
    "second rule action" "deny"
    (second |> member "action" |> to_string);
  Alcotest.(check string)
    "second rule log_policy" "no_log"
    (second |> member "log_policy" |> to_string);
  Alcotest.(check int) "second rule index" 1 (second |> member "index" |> to_int)

let test_egress_rules_text_output () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "access_bundles": [
        {
          "id": "b1",
          "egress_rules": [
            {"host": "api.example.com", "path": "/v1/*", "method": "GET", "action": "allow", "log_policy": "log"},
            {"host": "*", "action": "deny", "log_policy": "no_log"}
          ]
        }
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
    "text contains Egress Rules" true
    (Test_helpers.string_contains text "Egress Rules");
  Alcotest.(check bool)
    "text contains api.example.com" true
    (Test_helpers.string_contains text "api.example.com");
  Alcotest.(check bool)
    "text contains GET" true
    (Test_helpers.string_contains text "GET");
  Alcotest.(check bool)
    "text contains /v1/*" true
    (Test_helpers.string_contains text "/v1/*");
  Alcotest.(check bool)
    "text contains allow" true
    (Test_helpers.string_contains text "-> allow");
  Alcotest.(check bool)
    "text contains deny" true
    (Test_helpers.string_contains text "-> deny");
  Alcotest.(check bool)
    "text contains log policy" true
    (Test_helpers.string_contains text "(log: log)")

let test_egress_rules_summary () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "access_bundles": [
        {
          "id": "b1",
          "allowed_tools": ["tool_a"],
          "egress_rules": [
            {"host": "api.example.com", "action": "allow", "log_policy": "log"},
            {"host": "*", "action": "deny", "log_policy": "no_log"}
          ]
        }
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
    "summary contains egress_rules count" true
    (Test_helpers.string_contains explanation.summary "egress_rules:2")

let test_egress_rules_empty () =
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
  Alcotest.(check int)
    "no egress rules" 0
    (List.length explanation.egress_rules);
  Alcotest.(check bool)
    "summary shows egress_rules:0" true
    (Test_helpers.string_contains explanation.summary "egress_rules:0");
  let json_out = Access_explanation.to_json explanation in
  let open Yojson.Safe.Util in
  let rules = json_out |> member "egress_rules" |> to_list in
  Alcotest.(check int) "json has empty egress rules" 0 (List.length rules)

let test_egress_rules_priority_order () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "access_bundles": [
        {
          "id": "default-bundle",
          "egress_rules": [
            {"host": "default.example.com", "action": "deny", "log_policy": "log"}
          ]
        },
        {
          "id": "workspace-bundle",
          "egress_rules": [
            {"host": "workspace.example.com", "action": "allow", "log_policy": "log"}
          ]
        },
        {
          "id": "channel-bundle",
          "egress_rules": [
            {"host": "channel.example.com", "action": "allow", "log_policy": "no_log"}
          ]
        },
        {
          "id": "room-bundle",
          "egress_rules": [
            {"host": "room.example.com", "action": "allow", "log_policy": "no_log"}
          ]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["default-bundle"]},
        {"id": "workspace-scope", "level": "workspace", "workspace": "/tmp/test", "access_bundle_ids": ["workspace-bundle"]},
        {"id": "channel-scope", "level": "channel", "channel": "slack", "access_bundle_ids": ["channel-bundle"]},
        {"id": "room-scope", "level": "room", "room": "C123", "access_bundle_ids": ["room-bundle"]}
      ]
    }|}
  in
  let cfg = parse json in
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check int) "4 egress rules" 4 (List.length explanation.egress_rules);
  (* Priority order: room > channel > workspace > default *)
  let rules = explanation.egress_rules in
  let hosts =
    List.map
      (fun (r : Access_explanation.egress_rule_explanation) -> r.host)
      rules
  in
  Alcotest.(check (list string))
    "priority order: room, channel, workspace, default"
    [
      "room.example.com";
      "channel.example.com";
      "workspace.example.com";
      "default.example.com";
    ]
    hosts;
  (* Verify indices are sequential *)
  List.iteri
    (fun idx (rule : Access_explanation.egress_rule_explanation) ->
      Alcotest.(check int) (Printf.sprintf "rule %d index" idx) idx rule.index)
    rules

let test_no_matching_scope_no_leak () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "credential_handles": [
        {"id": "admin-cred", "provider": {"type": "env_var", "name": "ADMIN_SECRET"}, "description": "Admin API key"}
      ],
      "access_bundles": [
        {"id": "admin-bundle", "allowed_tools": ["admin_tool"], "credential_handles": ["admin-cred"]}
      ],
      "access_scopes": [
        {"id": "admin-only", "level": "room", "room": "ADMIN_ROOM", "access_bundle_ids": ["admin-bundle"]}
      ]
    }|}
  in
  let cfg = parse json in
  (* Unauthorized member — session key doesn't match any scope *)
  let explanation =
    Access_explanation.create ~config:cfg ~session_key:"slack:UNAUTHORIZED" ()
  in
  Alcotest.(check int) "no scopes" 0 (List.length explanation.scopes);
  Alcotest.(check int)
    "no allowed tools" 0
    (List.length explanation.allowed_tools);
  Alcotest.(check int)
    "no credential handles" 0
    (List.length explanation.credential_handles);
  Alcotest.(check int)
    "no instructions" 0
    (List.length explanation.instructions);
  (* JSON must not leak any credential or admin room info *)
  let json_out = Access_explanation.to_json explanation in
  let json_str = Yojson.Safe.to_string json_out in
  Alcotest.(check bool)
    "json does not contain admin-cred" true
    (not (Test_helpers.string_contains json_str "admin-cred"));
  Alcotest.(check bool)
    "json does not contain ADMIN_SECRET" true
    (not (Test_helpers.string_contains json_str "ADMIN_SECRET"));
  Alcotest.(check bool)
    "json does not contain admin_tool" true
    (not (Test_helpers.string_contains json_str "admin_tool"))

let test_mixed_active_deleted_credentials () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "credential_handles": [
        {"id": "cred-active", "provider": {"type": "env_var", "name": "ACTIVE_SECRET"}, "description": "Active"},
        {"id": "cred-deleted", "provider": {"type": "env_var", "name": "DELETED_SECRET"}, "description": "Deleted", "status": "deleted"},
        {"id": "cred-file", "provider": {"type": "file", "path": "/secret/path"}, "description": "File cred"},
        {"id": "cred-enc", "provider": {"type": "encrypted", "cipher_text": "$ENC:YWJjMTIz"}, "description": "Enc cred"}
      ],
      "access_bundles": [
        {"id": "b1", "credential_handles": ["cred-active", "cred-deleted", "cred-file", "cred-enc"]}
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
  (* Only 3 active credential handles should be exposed *)
  Alcotest.(check int)
    "3 active credential handles" 3
    (List.length explanation.credential_handles);
  let exposed_ids =
    List.map
      (fun (ci : Access_explanation.credential_info) -> ci.id)
      explanation.credential_handles
  in
  Alcotest.(check bool)
    "active cred exposed" true
    (List.mem "cred-active" exposed_ids);
  Alcotest.(check bool)
    "file cred exposed" true
    (List.mem "cred-file" exposed_ids);
  Alcotest.(check bool)
    "enc cred exposed" true
    (List.mem "cred-enc" exposed_ids);
  Alcotest.(check bool)
    "deleted cred NOT exposed" false
    (List.mem "cred-deleted" exposed_ids);
  (* JSON must not contain any secret values *)
  let json_out = Access_explanation.to_json explanation in
  let json_str = Yojson.Safe.to_string json_out in
  Alcotest.(check bool)
    "json does not contain ACTIVE_SECRET" true
    (not (Test_helpers.string_contains json_str "ACTIVE_SECRET"));
  Alcotest.(check bool)
    "json does not contain DELETED_SECRET" true
    (not (Test_helpers.string_contains json_str "DELETED_SECRET"));
  Alcotest.(check bool)
    "json does not contain /secret/path" true
    (not (Test_helpers.string_contains json_str "/secret/path"));
  Alcotest.(check bool)
    "json does not contain ENC ciphertext" true
    (not (Test_helpers.string_contains json_str "$ENC:"));
  (* Text must not contain any secret values *)
  let text = Access_explanation.to_text explanation in
  Alcotest.(check bool)
    "text does not contain ACTIVE_SECRET" true
    (not (Test_helpers.string_contains text "ACTIVE_SECRET"));
  Alcotest.(check bool)
    "text does not contain DELETED_SECRET" true
    (not (Test_helpers.string_contains text "DELETED_SECRET"));
  Alcotest.(check bool)
    "text does not contain /secret/path" true
    (not (Test_helpers.string_contains text "/secret/path"))

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
    Alcotest.test_case "repo grants in json and text" `Quick
      test_repo_grants_in_json_and_text;
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
    Alcotest.test_case "unbound room explanation" `Quick
      test_unbound_room_explanation;
    Alcotest.test_case "unbound room no credentials leaked" `Quick
      test_unbound_room_no_credentials_leaked;
    Alcotest.test_case "deleted profile no credential leak" `Quick
      test_deleted_profile_no_credential_leak;
    Alcotest.test_case "encrypted credential redaction" `Quick
      test_encrypted_credential_redaction;
    Alcotest.test_case "file credential redaction" `Quick
      test_file_credential_redaction;
    Alcotest.test_case "env var credential redaction" `Quick
      test_env_var_credential_redaction;
    Alcotest.test_case "inactive bundle scope no credential leak" `Quick
      test_inactive_bundle_scope_no_credential_leak;
    Alcotest.test_case "no matching scope no leak" `Quick
      test_no_matching_scope_no_leak;
    Alcotest.test_case "mixed active deleted credentials" `Quick
      test_mixed_active_deleted_credentials;
    Alcotest.test_case "egress rules in explanation" `Quick
      test_egress_rules_in_explanation;
    Alcotest.test_case "egress rules json output" `Quick
      test_egress_rules_json_output;
    Alcotest.test_case "egress rules text output" `Quick
      test_egress_rules_text_output;
    Alcotest.test_case "egress rules summary" `Quick test_egress_rules_summary;
    Alcotest.test_case "egress rules empty" `Quick test_egress_rules_empty;
    Alcotest.test_case "egress rules priority order" `Quick
      test_egress_rules_priority_order;
  ]

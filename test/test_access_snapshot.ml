let parse json = Config_loader.parse_config (Yojson.Safe.from_string json)

let test_create_snapshot_basic () =
  let json =
    {|{
      "workspace": "/tmp/test-snap",
      "access_bundles": [
        {"id": "base", "allowed_tools": ["file_read", "shell_exec"], "denied_tools": ["deploy"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
      ]
    }|}
  in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"slack:C123" ()
  in
  Alcotest.(check bool) "id non-empty" true (String.length snap.id > 0);
  Alcotest.(check bool)
    "timestamp non-empty" true
    (String.length snap.timestamp > 0);
  Alcotest.(check bool)
    "config_hash non-empty" true
    (String.length snap.config_hash > 0);
  Alcotest.(check (option string))
    "session_key preserved" (Some "slack:C123") snap.session_key;
  Alcotest.(check string)
    "work_type" "room_turn"
    (Access_snapshot.work_type_to_string snap.work_type);
  Alcotest.(check (list string))
    "allowed_tools"
    [ "file_read"; "shell_exec" ]
    snap.allowed_tools;
  Alcotest.(check (list string)) "denied_tools" [ "deploy" ] snap.denied_tools;
  Alcotest.(check bool)
    "redacted_summary non-empty" true
    (String.length snap.redacted_summary > 0)

let test_create_snapshot_with_room_profile () =
  let json =
    {|{
      "room_profiles": [
        {"id": "vip", "model": "openai:gpt-5.4", "allowed_tools": ["vip_tool"]}
      ],
      "room_profile_bindings": [
        {"profile_id": "vip", "room": "C123", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"slack:C123" ~room_id:"C123" ~profile_id:"vip" ()
  in
  Alcotest.(check (option string)) "room_id" (Some "C123") snap.room_id;
  Alcotest.(check (option string)) "profile_id" (Some "vip") snap.profile_id;
  Alcotest.(check (list string))
    "allowed_tools from profile" [ "vip_tool" ] snap.allowed_tools;
  Alcotest.(check bool)
    "bundle_sources non-empty" true
    (List.length snap.bundle_sources > 0)

let test_create_snapshot_derives_room_and_profile () =
  let json =
    {|{
      "room_profiles": [
        {"id": "vip", "model": "openai:gpt-5.4", "allowed_tools": ["vip_tool"]}
      ],
      "room_profile_bindings": [
        {"profile_id": "vip", "room": "C123", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"slack:C123:U456" ()
  in
  Alcotest.(check (option string)) "derived room_id" (Some "C123") snap.room_id;
  Alcotest.(check (option string))
    "derived profile_id" (Some "vip") snap.profile_id;
  Alcotest.(check (list string))
    "derived profile access" [ "vip_tool" ] snap.allowed_tools

let test_create_snapshot_uses_explicit_room_for_access () =
  let json =
    {|{
      "room_profiles": [
        {"id": "sig", "model": "openai:gpt-5.4", "allowed_tools": ["sig_tool"]}
      ],
      "room_profile_bindings": [
        {"profile_id": "sig", "room": "G123", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"signal:acct:group:G123" ~room_id:"G123" ()
  in
  Alcotest.(check (option string)) "explicit room_id" (Some "G123") snap.room_id;
  Alcotest.(check (option string))
    "explicit room profile" (Some "sig") snap.profile_id;
  Alcotest.(check (list string))
    "explicit room profile access" [ "sig_tool" ] snap.allowed_tools

let test_create_snapshot_background_task () =
  let json = {|{ "workspace": "/tmp/test-snap" }|} in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Background_task
      ~session_key:"bg:task-42" ()
  in
  Alcotest.(check string)
    "work_type" "background_task"
    (Access_snapshot.work_type_to_string snap.work_type);
  Alcotest.(check (option string))
    "session_key" (Some "bg:task-42") snap.session_key

let test_create_snapshot_github_trigger () =
  let json = {|{ "workspace": "/tmp/test-snap" }|} in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:GitHub_trigger
      ~session_key:"github:owner/repo:pr:42" ()
  in
  Alcotest.(check string)
    "work_type" "github_trigger"
    (Access_snapshot.work_type_to_string snap.work_type)

let test_create_snapshot_ambient_work () =
  let json =
    {|{
      "room_profiles": [
        {"id": "ambient", "model": "openai:gpt-5.4", "ambient_enabled": true}
      ],
      "room_profile_bindings": [
        {"profile_id": "ambient", "room": "ambient-room", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Ambient_work
      ~session_key:"ambient-room" ~room_id:"ambient-room" ~profile_id:"ambient"
      ()
  in
  Alcotest.(check string)
    "work_type" "ambient_work"
    (Access_snapshot.work_type_to_string snap.work_type);
  Alcotest.(check (option string)) "room_id" (Some "ambient-room") snap.room_id

let test_create_snapshot_routine () =
  let json = {|{ "workspace": "/tmp/test-snap" }|} in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Routine
      ~session_key:"cron:daily-report" ()
  in
  Alcotest.(check string)
    "work_type" "routine"
    (Access_snapshot.work_type_to_string snap.work_type)

let test_persist_and_query () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  let json =
    {|{
      "access_bundles": [
        {"id": "base", "allowed_tools": ["tool_a", "tool_b"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
      ]
    }|}
  in
  let cfg = parse json in
  let snap1 =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"slack:C1" ()
  in
  let snap2 =
    Access_snapshot.create ~config:cfg ~work_type:Background_task
      ~session_key:"bg:task-1" ()
  in
  Access_snapshot.persist ~db snap1;
  Access_snapshot.persist ~db snap2;
  let all = Access_snapshot.query ~db () in
  Alcotest.(check int) "2 snapshots persisted" 2 (List.length all);
  let room_snaps = Access_snapshot.query ~db ~work_type:Room_turn () in
  Alcotest.(check int) "1 room_turn snapshot" 1 (List.length room_snaps);
  let bg_snaps = Access_snapshot.query ~db ~work_type:Background_task () in
  Alcotest.(check int) "1 background_task snapshot" 1 (List.length bg_snaps)

let test_query_by_session_key () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  let json = {|{ "workspace": "/tmp/test" }|} in
  let cfg = parse json in
  let snap1 =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"slack:C1" ()
  in
  let snap2 =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"discord:C2" ()
  in
  Access_snapshot.persist ~db snap1;
  Access_snapshot.persist ~db snap2;
  let slack_snaps = Access_snapshot.query ~db ~session_key:"slack:C1" () in
  Alcotest.(check int) "1 slack snapshot" 1 (List.length slack_snaps);
  let discord_snaps = Access_snapshot.query ~db ~session_key:"discord:C2" () in
  Alcotest.(check int) "1 discord snapshot" 1 (List.length discord_snaps)

let test_query_by_room_id () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  let json = {|{ "workspace": "/tmp/test" }|} in
  let cfg = parse json in
  let snap1 =
    Access_snapshot.create ~config:cfg ~work_type:Ambient_work
      ~session_key:"room-A" ~room_id:"room-A" ()
  in
  let snap2 =
    Access_snapshot.create ~config:cfg ~work_type:Ambient_work
      ~session_key:"room-B" ~room_id:"room-B" ()
  in
  Access_snapshot.persist ~db snap1;
  Access_snapshot.persist ~db snap2;
  let room_a = Access_snapshot.query ~db ~room_id:"room-A" () in
  Alcotest.(check int) "1 room-A snapshot" 1 (List.length room_a)

let test_query_by_config_hash () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  let json1 = {|{ "workspace": "/tmp/test1" }|} in
  let json2 = {|{ "workspace": "/tmp/test2" }|} in
  let cfg1 = parse json1 in
  let cfg2 = parse json2 in
  let snap1 =
    Access_snapshot.create ~config:cfg1 ~work_type:Room_turn
      ~session_key:"slack:C1" ()
  in
  let snap2 =
    Access_snapshot.create ~config:cfg2 ~work_type:Room_turn
      ~session_key:"slack:C2" ()
  in
  Access_snapshot.persist ~db snap1;
  Access_snapshot.persist ~db snap2;
  let hash1_snaps =
    Access_snapshot.query ~db ~config_hash:snap1.config_hash ()
  in
  Alcotest.(check int) "1 snapshot with cfg1 hash" 1 (List.length hash1_snaps);
  let hash2_snaps =
    Access_snapshot.query ~db ~config_hash:snap2.config_hash ()
  in
  Alcotest.(check int) "1 snapshot with cfg2 hash" 1 (List.length hash2_snaps)

let test_query_limit () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  let json = {|{ "workspace": "/tmp/test" }|} in
  let cfg = parse json in
  for i = 1 to 10 do
    let snap =
      Access_snapshot.create ~config:cfg ~work_type:Room_turn
        ~session_key:(Printf.sprintf "slack:C%d" i)
        ()
    in
    Access_snapshot.persist ~db snap
  done;
  let all = Access_snapshot.query ~db ~limit:5 () in
  Alcotest.(check int) "limit 5 returns 5" 5 (List.length all);
  let all10 = Access_snapshot.query ~db ~limit:20 () in
  Alcotest.(check int) "limit 20 returns 10" 10 (List.length all10)

let test_get_by_id () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  let json = {|{ "workspace": "/tmp/test" }|} in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"slack:C1" ()
  in
  Access_snapshot.persist ~db snap;
  let found = Access_snapshot.get_by_id ~db snap.id in
  Alcotest.(check bool) "found by id" true (Option.is_some found);
  let found_snap = Option.get found in
  Alcotest.(check string) "id matches" snap.id found_snap.id;
  Alcotest.(check string)
    "config_hash matches" snap.config_hash found_snap.config_hash;
  let not_found = Access_snapshot.get_by_id ~db "nonexistent" in
  Alcotest.(check bool) "not found for bad id" true (Option.is_none not_found)

let test_to_json_roundtrip () =
  let json =
    {|{
      "access_bundles": [
        {"id": "base", "allowed_tools": ["tool_a"], "denied_tools": ["tool_b"],
         "instructions": ["be helpful"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
      ]
    }|}
  in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"slack:C1" ~room_id:"C1" ~profile_id:"p1" ()
  in
  let json_out = Access_snapshot.to_json snap in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "json id" snap.id
    (json_out |> member "id" |> to_string);
  Alcotest.(check string)
    "json work_type" "room_turn"
    (json_out |> member "work_type" |> to_string);
  Alcotest.(check string)
    "json session_key" "slack:C1"
    (json_out |> member "session_key" |> to_string);
  Alcotest.(check string)
    "json room_id" "C1"
    (json_out |> member "room_id" |> to_string);
  Alcotest.(check string)
    "json profile_id" "p1"
    (json_out |> member "profile_id" |> to_string);
  let allowed =
    json_out |> member "allowed_tools" |> to_list |> List.map to_string
  in
  Alcotest.(check (list string)) "json allowed_tools" [ "tool_a" ] allowed;
  let denied =
    json_out |> member "denied_tools" |> to_list |> List.map to_string
  in
  Alcotest.(check (list string)) "json denied_tools" [ "tool_b" ] denied;
  let digests =
    json_out |> member "instruction_digests" |> to_list |> List.map to_string
  in
  Alcotest.(check bool)
    "json has instruction digests" true
    (List.length digests > 0);
  let summary = json_out |> member "redacted_summary" |> to_string in
  Alcotest.(check bool)
    "json has redacted_summary" true
    (String.length summary > 0)

let test_config_hash_deterministic () =
  let json = {|{ "workspace": "/tmp/test" }|} in
  let cfg = parse json in
  let hash1 = Access_snapshot.config_hash cfg in
  let hash2 = Access_snapshot.config_hash cfg in
  Alcotest.(check string) "config_hash deterministic" hash1 hash2

let test_config_hash_differs_on_config_change () =
  let json1 = {|{ "workspace": "/tmp/test1" }|} in
  let json2 = {|{ "workspace": "/tmp/test2" }|} in
  let cfg1 = parse json1 in
  let cfg2 = parse json2 in
  let hash1 = Access_snapshot.config_hash cfg1 in
  let hash2 = Access_snapshot.config_hash cfg2 in
  Alcotest.(check bool)
    "different configs produce different hashes" true (hash1 <> hash2)

let test_bundle_sources_extracted () =
  let json =
    {|{
      "access_bundles": [
        {"id": "b1", "allowed_tools": ["tool_a"]},
        {"id": "b2", "allowed_tools": ["tool_b"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1", "b2"]}
      ]
    }|}
  in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"slack:C1" ()
  in
  Alcotest.(check bool)
    "bundle_sources extracted" true
    (List.length snap.bundle_sources >= 2);
  let bundle_ids =
    List.map
      (fun (s : Access_snapshot.bundle_source) -> s.bundle_id)
      snap.bundle_sources
  in
  Alcotest.(check bool) "has b1" true (List.mem "b1" bundle_ids);
  Alcotest.(check bool) "has b2" true (List.mem "b2" bundle_ids);
  Alcotest.(check bool)
    "omits raw scope source" false
    (List.mem "default" bundle_ids)

let test_bundle_sources_include_non_tool_fields () =
  let json =
    {|{
      "access_bundles": [
        {"id": "rich", "mcp_servers": ["srv"], "skills": ["skill"],
         "repositories": ["repo"], "domains": ["example.com"],
         "credential_handles": ["cred"], "instructions": ["do it"],
         "memory_grants": ["mem"], "budget_refs": ["budget"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["rich"]}
      ]
    }|}
  in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"slack:C1" ()
  in
  let bundle_ids =
    List.map
      (fun (s : Access_snapshot.bundle_source) -> s.bundle_id)
      snap.bundle_sources
  in
  Alcotest.(check bool)
    "bundle source includes non-tool fields" true
    (List.mem "rich" bundle_ids)

let test_instruction_digests_recorded () =
  let json =
    {|{
      "access_bundles": [
        {"id": "b1", "instructions": ["Be helpful and concise", "Always verify"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"slack:C1" ()
  in
  Alcotest.(check int)
    "2 instruction digests" 2
    (List.length snap.instruction_digests);
  List.iter
    (fun digest ->
      Alcotest.(check bool)
        "digest is hex string" true
        (String.length digest = 64))
    snap.instruction_digests

let test_export_json () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  let json = {|{ "workspace": "/tmp/test" }|} in
  let cfg = parse json in
  for i = 1 to 3 do
    let snap =
      Access_snapshot.create ~config:cfg ~work_type:Room_turn
        ~session_key:(Printf.sprintf "slack:C%d" i)
        ()
    in
    Access_snapshot.persist ~db snap
  done;
  let tmpdir = Filename.temp_dir "clawq_test" "" in
  let path = Filename.concat tmpdir "snapshots.jsonl" in
  let count = Access_snapshot.export_json ~db ~path () in
  Alcotest.(check int) "exported 3 snapshots" 3 count;
  let ic = open_in path in
  let lines = ref [] in
  (try
     while true do
       lines := input_line ic :: !lines
     done
   with End_of_file -> ());
  close_in ic;
  let lines = List.rev !lines in
  Alcotest.(check int) "3 lines in file" 3 (List.length lines);
  List.iter
    (fun line ->
      let _json = Yojson.Safe.from_string line in
      ())
    lines;
  Sys.remove path;
  try Sys.rmdir tmpdir with _ -> ()

let test_record_for_work () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  let json =
    {|{
      "access_bundles": [
        {"id": "base", "allowed_tools": ["file_read"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
      ],
      "room_profiles": [
        {"id": "ops", "model": "openai:gpt-5.4"}
      ],
      "room_profile_bindings": [
        {"profile_id": "ops", "room": "C987", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let snap_id =
    Access_snapshot.record_for_work ~db ~config:cfg
      ~work_type:Access_snapshot.Background_task ~session_key:"telegram:C987:U1"
      ()
  in
  Alcotest.(check bool) "returned id non-empty" true (String.length snap_id > 0);
  let found = Access_snapshot.get_by_id ~db snap_id in
  Alcotest.(check bool) "snapshot persisted" true (Option.is_some found);
  match found with
  | None -> Alcotest.fail "snapshot not found"
  | Some snap ->
      Alcotest.(check (option string))
        "record_for_work room_id" (Some "C987") snap.room_id;
      Alcotest.(check (option string))
        "record_for_work profile_id" (Some "ops") snap.profile_id

let test_persist_roundtrips_expanded_effective_access () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  let json =
    {|{
      "access_bundles": [
        {"id": "rich", "repositories": ["repo-a"],
         "credential_handles": ["cred-a"], "domains": ["example.com"],
         "mcp_servers": ["srv-a"], "skills": ["skill-a"],
         "memory_grants": ["mem-a"], "budget_refs": ["budget-a"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["rich"]}
      ]
    }|}
  in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"slack:C1" ()
  in
  Access_snapshot.persist ~db snap;
  match Access_snapshot.get_by_id ~db snap.id with
  | None -> Alcotest.fail "snapshot not found"
  | Some found ->
      Alcotest.(check (list string))
        "repositories" [ "repo-a" ] found.repositories;
      Alcotest.(check (list string))
        "credential_handles" [ "cred-a" ] found.credential_handles;
      Alcotest.(check (list string)) "domains" [ "example.com" ] found.domains;
      Alcotest.(check (list string)) "mcp_servers" [ "srv-a" ] found.mcp_servers;
      Alcotest.(check (list string)) "skills" [ "skill-a" ] found.skills;
      Alcotest.(check (list string))
        "memory_grants" [ "mem-a" ] found.memory_grants;
      Alcotest.(check (list string))
        "budget_refs" [ "budget-a" ] found.budget_refs;
      let open Yojson.Safe.Util in
      let json_out = Access_snapshot.to_json found in
      let list_field name =
        json_out |> member name |> to_list |> List.map to_string
      in
      Alcotest.(check (list string))
        "json repositories" [ "repo-a" ]
        (list_field "repositories");
      Alcotest.(check (list string))
        "json credential_handles" [ "cred-a" ]
        (list_field "credential_handles");
      Alcotest.(check (list string))
        "json domains" [ "example.com" ] (list_field "domains");
      Alcotest.(check (list string))
        "json mcp_servers" [ "srv-a" ] (list_field "mcp_servers");
      Alcotest.(check (list string))
        "json skills" [ "skill-a" ] (list_field "skills");
      Alcotest.(check (list string))
        "json memory_grants" [ "mem-a" ]
        (list_field "memory_grants");
      Alcotest.(check (list string))
        "json budget_refs" [ "budget-a" ] (list_field "budget_refs")

let test_query_and_get_by_id_are_parameterized () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  let json = {|{ "workspace": "/tmp/test" }|} in
  let cfg = parse json in
  let snap1 =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"slack:C1" ()
  in
  let snap2 =
    Access_snapshot.create ~config:cfg ~work_type:Background_task
      ~session_key:"bg:task-1" ()
  in
  Access_snapshot.persist ~db snap1;
  Access_snapshot.persist ~db snap2;
  let injected_query =
    Access_snapshot.query ~db ~session_key:"' OR 1=1 --" ()
  in
  Alcotest.(check int)
    "injected query returns no rows" 0
    (List.length injected_query);
  let injected_id = Access_snapshot.get_by_id ~db "missing' OR '1'='1" in
  Alcotest.(check bool)
    "injected id not found" true
    (Option.is_none injected_id)

let test_work_type_of_string_roundtrip () =
  let types =
    [
      Access_snapshot.Room_turn;
      Background_task;
      Ambient_work;
      GitHub_trigger;
      Routine;
    ]
  in
  List.iter
    (fun wt ->
      let s = Access_snapshot.work_type_to_string wt in
      match Access_snapshot.work_type_of_string s with
      | Some parsed ->
          Alcotest.(check string)
            ("roundtrip " ^ s) s
            (Access_snapshot.work_type_to_string parsed)
      | None -> Alcotest.fail ("failed to parse work_type: " ^ s))
    types

let test_work_type_of_string_invalid () =
  Alcotest.(check (option (list string)))
    "invalid work_type returns None" None
    (Option.map
       (fun _ -> [ "x" ])
       (Access_snapshot.work_type_of_string "invalid_type"))

let test_init_schema_idempotent () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  Access_snapshot.init_schema db;
  let json = {|{ "workspace": "/tmp/test" }|} in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"slack:C1" ()
  in
  Access_snapshot.persist ~db snap;
  let all = Access_snapshot.query ~db () in
  Alcotest.(check int) "works after double init" 1 (List.length all)

let test_session_turn_records_snapshot () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  let json =
    {|{
      "access_bundles": [
        {"id": "base", "allowed_tools": ["file_read"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
      ]
    }|}
  in
  let cfg = parse json in
  (* Simulate what Session.turn does when snapshot_work_type is set *)
  let snap_id =
    Access_snapshot.record_for_work ~db ~config:cfg
      ~work_type:Access_snapshot.Room_turn ~session_key:"telegram:chat123" ()
  in
  let snap = Access_snapshot.get_by_id ~db snap_id in
  match snap with
  | None -> Alcotest.fail "snapshot not found"
  | Some s ->
      Alcotest.(check string)
        "work_type" "room_turn"
        (Access_snapshot.work_type_to_string s.work_type);
      Alcotest.(check (option string))
        "session_key" (Some "telegram:chat123") s.session_key

let test_snapshot_immutable_after_persist () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  let json = {|{ "workspace": "/tmp/test" }|} in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"slack:C1" ()
  in
  Access_snapshot.persist ~db snap;
  (* Query should return the same data *)
  let found = Access_snapshot.get_by_id ~db snap.id in
  match found with
  | None -> Alcotest.fail "snapshot not found"
  | Some s ->
      Alcotest.(check string) "id immutable" snap.id s.id;
      Alcotest.(check string) "timestamp immutable" snap.timestamp s.timestamp;
      Alcotest.(check string)
        "config_hash immutable" snap.config_hash s.config_hash;
      Alcotest.(check string)
        "redacted_summary immutable" snap.redacted_summary s.redacted_summary

let test_redacted_summary_format () =
  let json =
    {|{
      "access_bundles": [
        {"id": "b1", "allowed_tools": ["t1", "t2"], "denied_tools": ["d1"],
         "mcp_servers": ["srv1"], "skills": ["sk1"],
         "codebase_grants": ["grant1"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let cfg = parse json in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"slack:C1" ()
  in
  let summary = snap.redacted_summary in
  Alcotest.(check bool)
    "summary contains tools" true
    (let re = Str.regexp_string "tools:" in
     try
       ignore (Str.search_forward re summary 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "summary contains grants" true
    (let re = Str.regexp_string "grants:" in
     try
       ignore (Str.search_forward re summary 0);
       true
     with Not_found -> false)

let test_bg_task_snapshot_immutable_after_config_change () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  (* Config v1: has "deploy" tool *)
  let json_v1 =
    {|{
      "access_bundles": [
        {"id": "base", "allowed_tools": ["file_read", "shell_exec", "deploy"],
         "denied_tools": []}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
      ]
    }|}
  in
  let cfg_v1 = parse json_v1 in
  (* Create snapshot simulating a background task starting *)
  let snap_id =
    Access_snapshot.record_for_work ~db ~config:cfg_v1
      ~work_type:Access_snapshot.Background_task ~session_key:"bg:task-42" ()
  in
  let snap_before = Access_snapshot.get_by_id ~db snap_id in
  let original_tools =
    match snap_before with
    | Some s -> s.allowed_tools
    | None -> Alcotest.fail "snapshot not found before config change"
  in
  Alcotest.(check (list string))
    "bg snapshot has deploy before change"
    [ "file_read"; "shell_exec"; "deploy" ]
    original_tools;
  (* Config v2: removes deploy, adds audit *)
  let json_v2 =
    {|{
      "access_bundles": [
        {"id": "base", "allowed_tools": ["file_read", "shell_exec", "audit"],
         "denied_tools": ["deploy"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
      ]
    }|}
  in
  let cfg_v2 = parse json_v2 in
  (* Snapshot retrieved after config change should still show v1 access *)
  let snap_after = Access_snapshot.get_by_id ~db snap_id in
  match snap_after with
  | None -> Alcotest.fail "snapshot disappeared after config change"
  | Some s ->
      Alcotest.(check (list string))
        "bg snapshot tools immutable after config change"
        [ "file_read"; "shell_exec"; "deploy" ]
        s.allowed_tools;
      Alcotest.(check (list string))
        "bg snapshot denied_tools immutable after config change" []
        s.denied_tools;
      Alcotest.(check string)
        "bg snapshot config_hash still v1"
        (Access_snapshot.config_hash cfg_v1)
        s.config_hash;
      Alcotest.(check bool)
        "bg snapshot config_hash differs from v2" true
        (s.config_hash <> Access_snapshot.config_hash cfg_v2);
      Alcotest.(check string)
        "bg snapshot redacted_summary still v1"
        (Option.get snap_before).redacted_summary s.redacted_summary

let test_routine_snapshot_immutable_after_config_change () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  (* Config v1: routine has broad domain access *)
  let json_v1 =
    {|{
      "access_bundles": [
        {"id": "cron", "allowed_tools": ["report_gen"],
         "domains": ["internal.corp", "api.example.com"],
         "skills": ["daily-report"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["cron"]}
      ]
    }|}
  in
  let cfg_v1 = parse json_v1 in
  let snap_id =
    Access_snapshot.record_for_work ~db ~config:cfg_v1
      ~work_type:Access_snapshot.Routine ~session_key:"cron:daily-report" ()
  in
  let snap_before = Access_snapshot.get_by_id ~db snap_id in
  let original_domains =
    match snap_before with
    | Some s -> s.domains
    | None -> Alcotest.fail "routine snapshot not found"
  in
  Alcotest.(check (list string))
    "routine domains before change"
    [ "internal.corp"; "api.example.com" ]
    original_domains;
  (* Config v2: restricts domains and removes skills *)
  let json_v2 =
    {|{
      "access_bundles": [
        {"id": "cron", "allowed_tools": ["report_gen"],
         "domains": ["internal.corp"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["cron"]}
      ]
    }|}
  in
  let cfg_v2 = parse json_v2 in
  let snap_after = Access_snapshot.get_by_id ~db snap_id in
  match snap_after with
  | None -> Alcotest.fail "routine snapshot disappeared"
  | Some s ->
      Alcotest.(check (list string))
        "routine domains immutable"
        [ "internal.corp"; "api.example.com" ]
        s.domains;
      Alcotest.(check (list string))
        "routine skills immutable" [ "daily-report" ] s.skills;
      Alcotest.(check string)
        "routine config_hash still v1"
        (Access_snapshot.config_hash cfg_v1)
        s.config_hash;
      Alcotest.(check bool)
        "routine config_hash differs from v2" true
        (s.config_hash <> Access_snapshot.config_hash cfg_v2)

let test_concurrent_tasks_have_independent_snapshots () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  (* Config v1 for task A *)
  let json_v1 =
    {|{
      "access_bundles": [
        {"id": "b1", "allowed_tools": ["tool_v1"],
         "mcp_servers": ["srv-v1"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let cfg_v1 = parse json_v1 in
  (* Task A starts *)
  let snap_a_id =
    Access_snapshot.record_for_work ~db ~config:cfg_v1
      ~work_type:Access_snapshot.Background_task ~session_key:"bg:task-a" ()
  in
  (* Config changes to v2 *)
  let json_v2 =
    {|{
      "access_bundles": [
        {"id": "b1", "allowed_tools": ["tool_v2"],
         "mcp_servers": ["srv-v2"], "denied_tools": ["tool_v1"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let cfg_v2 = parse json_v2 in
  (* Task B starts under new config *)
  let snap_b_id =
    Access_snapshot.record_for_work ~db ~config:cfg_v2
      ~work_type:Access_snapshot.Background_task ~session_key:"bg:task-b" ()
  in
  (* Both snapshots should be independent *)
  let snap_a = Access_snapshot.get_by_id ~db snap_a_id in
  let snap_b = Access_snapshot.get_by_id ~db snap_b_id in
  match (snap_a, snap_b) with
  | Some a, Some b ->
      Alcotest.(check (list string))
        "task-a still has tool_v1" [ "tool_v1" ] a.allowed_tools;
      Alcotest.(check (list string))
        "task-a still has srv-v1" [ "srv-v1" ] a.mcp_servers;
      Alcotest.(check (list string))
        "task-b has tool_v2" [ "tool_v2" ] b.allowed_tools;
      Alcotest.(check (list string))
        "task-b has srv-v2" [ "srv-v2" ] b.mcp_servers;
      Alcotest.(check (list string))
        "task-b has tool_v1 denied" [ "tool_v1" ] b.denied_tools;
      Alcotest.(check bool)
        "different config hashes" true
        (a.config_hash <> b.config_hash)
  | _ -> Alcotest.fail "one or both snapshots not found"

let test_snapshot_reflects_config_at_creation_not_retrieval () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  (* Config with room profile granting specific tools *)
  let json_v1 =
    {|{
      "access_bundles": [
        {"id": "base", "allowed_tools": ["file_read"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
      ],
      "room_profiles": [
        {"id": "vip", "model": "openai:gpt-5.4", "allowed_tools": ["vip_tool"]}
      ],
      "room_profile_bindings": [
        {"profile_id": "vip", "room": "C100", "active": true}
      ]
    }|}
  in
  let cfg_v1 = parse json_v1 in
  let snap_id =
    Access_snapshot.record_for_work ~db ~config:cfg_v1
      ~work_type:Access_snapshot.Room_turn ~session_key:"slack:C100"
      ~room_id:"C100" ()
  in
  (* Config v2: change profile tools and deactivate binding *)
  let json_v2 =
    {|{
      "access_bundles": [
        {"id": "base", "allowed_tools": ["file_read"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
      ],
      "room_profiles": [
        {"id": "vip", "model": "openai:gpt-5.4", "allowed_tools": ["new_vip_tool"]}
      ],
      "room_profile_bindings": [
        {"profile_id": "vip", "room": "C100", "active": false}
      ]
    }|}
  in
  (* Parse v2 but don't use it for the existing snapshot *)
  let _cfg_v2 = parse json_v2 in
  let snap = Access_snapshot.get_by_id ~db snap_id in
  match snap with
  | None -> Alcotest.fail "snapshot not found"
  | Some s -> (
      Alcotest.(check (list string))
        "room turn snapshot retains vip_tool"
        [ "file_read"; "vip_tool" ]
        s.allowed_tools;
      Alcotest.(check (option string))
        "room turn snapshot retains profile_id" (Some "vip") s.profile_id;
      Alcotest.(check (option string))
        "room turn snapshot retains room_id" (Some "C100") s.room_id;
      (* New snapshot under v2 should reflect the changes *)
      let json_v2_active =
        {|{
          "access_bundles": [
            {"id": "base", "allowed_tools": ["file_read"]}
          ],
          "access_scopes": [
            {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
          ],
          "room_profiles": [
            {"id": "vip", "model": "openai:gpt-5.4", "allowed_tools": ["new_vip_tool"]}
          ],
          "room_profile_bindings": [
            {"profile_id": "vip", "room": "C100", "active": true}
          ]
        }|}
      in
      let cfg_v2_active = parse json_v2_active in
      let new_snap_id =
        Access_snapshot.record_for_work ~db ~config:cfg_v2_active
          ~work_type:Access_snapshot.Room_turn ~session_key:"slack:C100"
          ~room_id:"C100" ()
      in
      let new_snap = Access_snapshot.get_by_id ~db new_snap_id in
      match new_snap with
      | None -> Alcotest.fail "new snapshot not found"
      | Some ns ->
          Alcotest.(check (list string))
            "new snapshot has new_vip_tool"
            [ "file_read"; "new_vip_tool" ]
            ns.allowed_tools;
          Alcotest.(check bool)
            "snapshots have different config hashes" true
            (s.config_hash <> ns.config_hash))

let test_bg_task_denied_tools_immutable_after_config_change () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  let json_v1 =
    {|{
      "access_bundles": [
        {"id": "base", "allowed_tools": ["tool_a"],
         "denied_tools": ["dangerous_tool"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
      ]
    }|}
  in
  let cfg_v1 = parse json_v1 in
  let snap_id =
    Access_snapshot.record_for_work ~db ~config:cfg_v1
      ~work_type:Access_snapshot.Background_task ~session_key:"bg:task-denied"
      ()
  in
  (* Config v2: allows the previously denied tool *)
  let json_v2 =
    {|{
      "access_bundles": [
        {"id": "base", "allowed_tools": ["tool_a", "dangerous_tool"],
         "denied_tools": []}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
      ]
    }|}
  in
  let _cfg_v2 = parse json_v2 in
  let snap = Access_snapshot.get_by_id ~db snap_id in
  match snap with
  | None -> Alcotest.fail "snapshot not found"
  | Some s ->
      Alcotest.(check (list string))
        "denied_tools still has dangerous_tool" [ "dangerous_tool" ]
        s.denied_tools;
      Alcotest.(check (list string))
        "allowed_tools does not include dangerous_tool" [ "tool_a" ]
        s.allowed_tools

let test_routine_mcp_servers_immutable_after_config_change () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  let json_v1 =
    {|{
      "access_bundles": [
        {"id": "base", "mcp_servers": ["srv-a", "srv-b"],
         "skills": ["skill-a"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
      ]
    }|}
  in
  let cfg_v1 = parse json_v1 in
  let snap_id =
    Access_snapshot.record_for_work ~db ~config:cfg_v1
      ~work_type:Access_snapshot.Routine ~session_key:"cron:sync" ()
  in
  let snap_before = Access_snapshot.get_by_id ~db snap_id in
  let original_servers =
    match snap_before with
    | Some s -> s.mcp_servers
    | None -> Alcotest.fail "routine snapshot not found"
  in
  Alcotest.(check (list string))
    "mcp_servers before change" [ "srv-a"; "srv-b" ] original_servers;
  Alcotest.(check (list string))
    "skills before change" [ "skill-a" ] (Option.get snap_before).skills;
  (* Config v2: removes one server and changes skills *)
  let json_v2 =
    {|{
      "access_bundles": [
        {"id": "base", "mcp_servers": ["srv-a"],
         "skills": ["skill-b", "skill-c"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["base"]}
      ]
    }|}
  in
  let _cfg_v2 = parse json_v2 in
  let snap_after = Access_snapshot.get_by_id ~db snap_id in
  match snap_after with
  | None -> Alcotest.fail "routine snapshot disappeared"
  | Some s ->
      Alcotest.(check (list string))
        "mcp_servers immutable" [ "srv-a"; "srv-b" ] s.mcp_servers;
      Alcotest.(check (list string)) "skills immutable" [ "skill-a" ] s.skills

let test_snapshot_preserves_instruction_digests_after_config_change () =
  let db = Memory.init ~db_path:":memory:" () in
  Access_snapshot.init_schema db;
  let json_v1 =
    {|{
      "access_bundles": [
        {"id": "b1", "instructions": ["Be helpful", "Follow rules"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let cfg_v1 = parse json_v1 in
  let snap_id =
    Access_snapshot.record_for_work ~db ~config:cfg_v1
      ~work_type:Access_snapshot.Background_task ~session_key:"bg:task-inst" ()
  in
  let snap_before = Access_snapshot.get_by_id ~db snap_id in
  let original_digests =
    match snap_before with
    | Some s -> s.instruction_digests
    | None -> Alcotest.fail "snapshot not found"
  in
  Alcotest.(check int) "2 digests before" 2 (List.length original_digests);
  (* Config v2: changes instructions *)
  let json_v2 =
    {|{
      "access_bundles": [
        {"id": "b1", "instructions": ["New instructions only"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["b1"]}
      ]
    }|}
  in
  let _cfg_v2 = parse json_v2 in
  let snap_after = Access_snapshot.get_by_id ~db snap_id in
  match snap_after with
  | None -> Alcotest.fail "snapshot disappeared"
  | Some s ->
      Alcotest.(check int)
        "instruction digests count unchanged" 2
        (List.length s.instruction_digests);
      Alcotest.(check (list string))
        "instruction digests identical" original_digests s.instruction_digests

let suite =
  [
    Alcotest.test_case "create snapshot basic" `Quick test_create_snapshot_basic;
    Alcotest.test_case "create snapshot with room profile" `Quick
      test_create_snapshot_with_room_profile;
    Alcotest.test_case "create snapshot derives room and profile" `Quick
      test_create_snapshot_derives_room_and_profile;
    Alcotest.test_case "create snapshot uses explicit room for access" `Quick
      test_create_snapshot_uses_explicit_room_for_access;
    Alcotest.test_case "create snapshot background task" `Quick
      test_create_snapshot_background_task;
    Alcotest.test_case "create snapshot github trigger" `Quick
      test_create_snapshot_github_trigger;
    Alcotest.test_case "create snapshot ambient work" `Quick
      test_create_snapshot_ambient_work;
    Alcotest.test_case "create snapshot routine" `Quick
      test_create_snapshot_routine;
    Alcotest.test_case "persist and query" `Quick test_persist_and_query;
    Alcotest.test_case "query by session_key" `Quick test_query_by_session_key;
    Alcotest.test_case "query by room_id" `Quick test_query_by_room_id;
    Alcotest.test_case "query by config_hash" `Quick test_query_by_config_hash;
    Alcotest.test_case "query limit" `Quick test_query_limit;
    Alcotest.test_case "get by id" `Quick test_get_by_id;
    Alcotest.test_case "to_json roundtrip" `Quick test_to_json_roundtrip;
    Alcotest.test_case "config_hash deterministic" `Quick
      test_config_hash_deterministic;
    Alcotest.test_case "config_hash differs on config change" `Quick
      test_config_hash_differs_on_config_change;
    Alcotest.test_case "bundle_sources extracted" `Quick
      test_bundle_sources_extracted;
    Alcotest.test_case "bundle_sources include non-tool fields" `Quick
      test_bundle_sources_include_non_tool_fields;
    Alcotest.test_case "instruction_digests recorded" `Quick
      test_instruction_digests_recorded;
    Alcotest.test_case "export json" `Quick test_export_json;
    Alcotest.test_case "record_for_work" `Quick test_record_for_work;
    Alcotest.test_case "persist expanded effective access" `Quick
      test_persist_roundtrips_expanded_effective_access;
    Alcotest.test_case "query and get_by_id are parameterized" `Quick
      test_query_and_get_by_id_are_parameterized;
    Alcotest.test_case "work_type roundtrip" `Quick
      test_work_type_of_string_roundtrip;
    Alcotest.test_case "work_type invalid" `Quick
      test_work_type_of_string_invalid;
    Alcotest.test_case "init_schema idempotent" `Quick
      test_init_schema_idempotent;
    Alcotest.test_case "session turn records snapshot" `Quick
      test_session_turn_records_snapshot;
    Alcotest.test_case "snapshot immutable after persist" `Quick
      test_snapshot_immutable_after_persist;
    Alcotest.test_case "redacted summary format" `Quick
      test_redacted_summary_format;
    Alcotest.test_case "bg task snapshot immutable after config change" `Quick
      test_bg_task_snapshot_immutable_after_config_change;
    Alcotest.test_case "routine snapshot immutable after config change" `Quick
      test_routine_snapshot_immutable_after_config_change;
    Alcotest.test_case "concurrent tasks have independent snapshots" `Quick
      test_concurrent_tasks_have_independent_snapshots;
    Alcotest.test_case "snapshot reflects config at creation not retrieval"
      `Quick test_snapshot_reflects_config_at_creation_not_retrieval;
    Alcotest.test_case "bg task denied_tools immutable after config change"
      `Quick test_bg_task_denied_tools_immutable_after_config_change;
    Alcotest.test_case "routine mcp_servers immutable after config change"
      `Quick test_routine_mcp_servers_immutable_after_config_change;
    Alcotest.test_case
      "snapshot preserves instruction digests after config change" `Quick
      test_snapshot_preserves_instruction_digests_after_config_change;
  ]

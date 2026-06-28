let config_with_memory =
  {
    Runtime_config.default with
    memory = { Runtime_config.default.memory with search_enabled = true };
    prompt = { Runtime_config.default.prompt with dynamic_enabled = true };
  }

let contains = Test_helpers.string_contains

let audit_report_relpath =
  "docs/ultra-plans/room-agent-profiles/nodes/04-scoped-memory/P12.M1.E3.T003-global-memory-callsite-audit.md"

let repo_root () =
  let rec find_from dir =
    let has_file name = Sys.file_exists (Filename.concat dir name) in
    if has_file "dune-project" && has_file "src" then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else find_from parent
  in
  match find_from (Sys.getcwd ()) with
  | Some dir -> dir
  | None ->
      let exe =
        if Filename.is_relative Sys.executable_name then
          Filename.concat (Sys.getcwd ()) Sys.executable_name
        else Sys.executable_name
      in
      find_from (Filename.dirname exe) |> Option.value ~default:(Sys.getcwd ())

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let read_lines path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let rec loop acc =
        match input_line ic with
        | line -> loop (line :: acc)
        | exception End_of_file -> List.rev acc
      in
      loop [])

let monitored_source_files () =
  let src_dir = Filename.concat (repo_root ()) "src" in
  Sys.readdir src_dir |> Array.to_list
  |> List.filter (fun name ->
      Filename.check_suffix name ".ml"
      && (String.starts_with ~prefix:"agent" name
         || String.starts_with ~prefix:"session" name
         || name = "prompt_builder.ml"))
  |> List.sort String.compare
  |> List.map (fun name -> ("src/" ^ name, Filename.concat src_dir name))

let memory_call_re =
  Str.regexp
    "Memory\\.\\(search\\|list_core\\|recall_core\\)\\([^A-Za-z0-9_]\\|$\\)"

let line_has_audited_memory_call line =
  try
    ignore (Str.search_forward memory_call_re line 0);
    true
  with Not_found -> false

let audited_memory_callsite_keys () =
  monitored_source_files ()
  |> List.concat_map (fun (rel, path) ->
      read_lines path
      |> List.filter line_has_audited_memory_call
      |> List.map (fun line -> Printf.sprintf "%s | %s" rel (String.trim line)))

let expected_memory_callsite_keys =
  [
    "src/agent_0_compact.ml | let results = Memory.recall_core ~db ~query \
     ~limit in";
    "src/agent_0_compact.ml | let results = Memory.list_core ~db ~category () \
     in";
    "src/agent_2_tools.ml | Memory.search ~db ~query:user_message ~scope_kind \
     ~scope_key";
    "src/agent_2_tools.ml | Memory.search ~db ~query:user_message ~limit:5 ()";
    "src/agent_2_tools.ml | let all = Memory.list_core ~db () in";
  ]

let test_global_memory_callsite_audit_report_accounts_for_all_callsites () =
  let actual = audited_memory_callsite_keys () |> List.sort String.compare in
  let expected = expected_memory_callsite_keys |> List.sort String.compare in
  Alcotest.(check (list string)) "audited callsites" expected actual;
  let report =
    read_file (Filename.concat (repo_root ()) audit_report_relpath)
  in
  List.iter
    (fun key ->
      Alcotest.(check bool)
        ("report accounts for " ^ key)
        true (contains report key))
    expected;
  Alcotest.(check bool)
    "legacy fallback documented" true
    (contains report "Legacy routing fallback");
  let source_text =
    monitored_source_files ()
    |> List.map (fun (_, path) -> read_file path)
    |> String.concat "\n"
  in
  Alcotest.(check bool)
    "source flags scoped-memory audit TODOs" true
    (contains source_text "TODO(scoped-memory-audit)")

let seed_profiled_room ~db ~room_id =
  let profile_id =
    Memory.insert_room_profile ~db ~name:(room_id ^ "-profile")
  in
  Memory.upsert_room_profile_binding ~db ~room_id ~profile_id;
  Memory.create_scope ~db ~kind:"room" ~key:room_id ~profile_id
    ~provenance:"test" ()

let combined_history agent =
  agent.Agent.history
  |> List.map (fun (m : Provider.message) -> m.content)
  |> String.concat "\n"

let test_profiled_room_injects_scoped_memory_and_prefers_it () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let scope = seed_profiled_room ~db ~room_id:"room-a" in
  ignore
    (Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"room-a-note"
       ~content:"scoped alpha profile fact" ~provenance:"test" ());
  Memory.store_core ~db ~key:"global-alpha" ~content:"global alpha fact"
    ~category:"test" ();
  let agent = Agent.create ~config:config_with_memory () in
  agent.room_profile_system_prompt <- Some "Profiled room prompt content";
  ignore
    (Lwt_main.run
       (Agent.prepare_turn_history agent ~user_message:"alpha" ~db
          ~session_key:"s1" ~room_id:"room-a" ()));
  let prompt =
    match Agent.build_messages agent with
    | (msg : Provider.message) :: _ -> msg.content
    | [] -> Alcotest.fail "expected system prompt"
  in
  Alcotest.(check bool)
    "room profile prompt used" true
    (contains prompt "Profiled room prompt content");
  let history = combined_history agent in
  Alcotest.(check bool)
    "scoped memory injected" true
    (contains history "scoped alpha profile fact");
  Alcotest.(check bool)
    "global memory not injected" false
    (contains history "global alpha fact")

let test_profiled_room_requires_scope_owner_or_read_grant () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let profile_id = Memory.insert_room_profile ~db ~name:"room-a-profile" in
  Memory.upsert_room_profile_binding ~db ~room_id:"room-a" ~profile_id;
  let private_scope =
    Memory.create_scope ~db ~kind:"room" ~key:"room-a" ~provenance:"test" ()
  in
  ignore
    (Memory.upsert_scoped_memory ~db ~scope_id:private_scope.id
       ~reference:"private-note" ~content:"private alpha fact"
       ~provenance:"test" ());
  let ungranted = Agent.create ~config:config_with_memory () in
  ungranted.room_profile_system_prompt <- Some "Profiled room prompt content";
  ignore
    (Lwt_main.run
       (Agent.prepare_turn_history ungranted ~user_message:"alpha" ~db
          ~session_key:"s1" ~room_id:"room-a" ()));
  Alcotest.(check bool)
    "ungranted scope memory not injected" false
    (contains (combined_history ungranted) "private alpha fact");
  (match
     Memory.grant_access ~db ~is_admin:true ~scope_id:private_scope.id
       ~principal_kind:"profile" ~principal_id:(string_of_int profile_id)
       ~capability:"read" ()
   with
  | Ok () -> ()
  | Error msg -> Alcotest.fail msg);
  let granted = Agent.create ~config:config_with_memory () in
  granted.room_profile_system_prompt <- Some "Profiled room prompt content";
  ignore
    (Lwt_main.run
       (Agent.prepare_turn_history granted ~user_message:"alpha" ~db
          ~session_key:"s1" ~room_id:"room-a" ()));
  Alcotest.(check bool)
    "direct read grant injects scope memory" true
    (contains (combined_history granted) "private alpha fact")

let test_compaction_preserves_scoped_memory_references () =
  let agent = Agent.create ~config:config_with_memory () in
  let scoped_context =
    Provider.make_message ~role:"system"
      ~content:
        "Relevant scoped memory context:\n\
         [scoped:room/room-a#42 ref=room-a-note] scoped alpha profile fact"
  in
  let chrono =
    scoped_context
    :: List.init 25 (fun i ->
        Provider.make_message ~role:"user"
          ~content:(Printf.sprintf "message %02d" i))
  in
  agent.history <- List.rev chrono;
  match Agent.plan_force_compact agent with
  | None -> Alcotest.fail "expected force compaction plan"
  | Some plan ->
      ignore (Agent.apply_compact_result agent plan ~summary:"summary");
      let history = combined_history agent in
      Alcotest.(check bool)
        "scoped reference preserved" true
        (contains history "ref=room-a-note");
      Alcotest.(check bool)
        "preservation note present" true
        (contains history "Scoped memory references preserved")

let suite =
  [
    Alcotest.test_case
      "profiled room injects scoped memory and prefers it over global" `Quick
      test_profiled_room_injects_scoped_memory_and_prefers_it;
    Alcotest.test_case "profiled room requires scope owner or read grant" `Quick
      test_profiled_room_requires_scope_owner_or_read_grant;
    Alcotest.test_case "compaction preserves scoped memory references" `Quick
      test_compaction_preserves_scoped_memory_references;
    Alcotest.test_case
      "global memory callsite audit report accounts for all callsites" `Quick
      test_global_memory_callsite_audit_report_accounts_for_all_callsites;
  ]

let config_with_memory =
  {
    Runtime_config.default with
    memory = { Runtime_config.default.memory with search_enabled = true };
    prompt = { Runtime_config.default.prompt with dynamic_enabled = true };
  }

let contains = Test_helpers.string_contains

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
    Alcotest.test_case "compaction preserves scoped memory references" `Quick
      test_compaction_preserves_scoped_memory_references;
  ]

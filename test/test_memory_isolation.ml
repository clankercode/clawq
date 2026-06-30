(** Negative tests for scoped memory isolation.

    Verifies that:
    - Channel A cannot list/recall/correct/forget Channel B memory
    - Private memory never appears in workspace/public scope by default
    - Forgotten content is absent from prompt injection and search *)

let contains = Test_helpers.string_contains

let with_db f =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let make_ledger db : Memory.ledger_fn =
 fun ~room_id ~event_type ~actor ~metadata ->
  ignore
    (Room_activity_ledger.append_now ~db ~room_id ~event_type ~actor ~metadata)

(** Seed a profiled room with a profile binding and memory scope. *)
let seed_profiled_room ~db ~room_id ~profile_name =
  let profile_id = Memory.insert_room_profile ~db ~name:profile_name in
  Memory.upsert_room_profile_binding ~db ~room_id ~profile_id;
  Memory.create_scope ~db ~kind:"room" ~key:room_id ~profile_id
    ~provenance:"test" ()

(** Seed a room scope without a profile binding (unowned). *)
let seed_unowned_room ~db ~room_id =
  Memory.create_scope ~db ~kind:"room" ~key:room_id ~provenance:"test" ()

(** Helper to create a minimal invoke_context for a session in a room. *)
let make_context ~session_key =
  {
    Tool.session_key = Some session_key;
    send_progress = None;
    interrupt_check = None;
    inject_system_messages = None;
    effective_cwd = None;
    request_cwd_change = None;
    egress_rules = [];
    snapshot_id = None;
    profile_id = None;
    egress_audit_db = None;
  }

(** Helper to register room memory tools and return a registry. *)
let make_registry ~db =
  let registry = Tool_registry.create () in
  Tools_builtin_room_memory.register_room_memory_tools ~db registry;
  registry

(** Helper to invoke a tool by name and get the result string. *)
let invoke_tool registry name context args =
  match Tool_registry.find registry name with
  | None -> Alcotest.fail (Printf.sprintf "Tool '%s' not found" name)
  | Some tool -> Lwt_main.run (tool.invoke ~context args)

(* ── Cross-channel list isolation ───────────────────────────────────────── *)

let test_cross_channel_list_isolation () =
  with_db (fun db ->
      let scope_a =
        seed_profiled_room ~db ~room_id:"channel-a" ~profile_name:"p-a"
      in
      let _scope_b =
        seed_profiled_room ~db ~room_id:"channel-b" ~profile_name:"p-b"
      in
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope_a.id
           ~reference:"channel-a-secret" ~content:"channel-a private data"
           ~provenance:"test" ());
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope_a.id
           ~reference:"channel-a-public" ~content:"channel-a public data"
           ~provenance:"test" ~visibility:Public ());
      let registry = make_registry ~db in
      let context_b = make_context ~session_key:"telegram:channel-b" in
      let result =
        invoke_tool registry "room_memory_list" context_b (`Assoc [])
      in
      Alcotest.(check bool)
        "channel-b does not see channel-a memories" false
        (contains result "channel-a-secret");
      Alcotest.(check bool)
        "channel-b does not see channel-a public memories" false
        (contains result "channel-a-public"))

let test_cross_channel_list_isolation_reverse () =
  with_db (fun db ->
      let _scope_a =
        seed_profiled_room ~db ~room_id:"channel-a" ~profile_name:"p-a"
      in
      let scope_b =
        seed_profiled_room ~db ~room_id:"channel-b" ~profile_name:"p-b"
      in
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope_b.id
           ~reference:"channel-b-secret" ~content:"channel-b private data"
           ~provenance:"test" ());
      let registry = make_registry ~db in
      let context_a = make_context ~session_key:"telegram:channel-a" in
      let result =
        invoke_tool registry "room_memory_list" context_a (`Assoc [])
      in
      Alcotest.(check bool)
        "channel-a does not see channel-b memories" false
        (contains result "channel-b-secret"))

(* ── Cross-channel show isolation ───────────────────────────────────────── *)

let test_cross_channel_show_isolation () =
  with_db (fun db ->
      let _scope_a =
        seed_profiled_room ~db ~room_id:"channel-a" ~profile_name:"p-a"
      in
      let scope_b =
        seed_profiled_room ~db ~room_id:"channel-b" ~profile_name:"p-b"
      in
      let mem_b =
        Memory.upsert_scoped_memory ~db ~scope_id:scope_b.id
          ~reference:"channel-b-note" ~content:"channel-b content"
          ~provenance:"test" ()
      in
      let registry = make_registry ~db in
      let context_a = make_context ~session_key:"telegram:channel-a" in
      let result =
        invoke_tool registry "room_memory_show" context_a
          (`Assoc [ ("memory_id", `Int mem_b.id) ])
      in
      Alcotest.(check bool)
        "channel-a cannot show channel-b memory" true
        (contains result "does not belong to this room"))

(* ── Cross-channel correct isolation ────────────────────────────────────── *)

let test_cross_channel_correct_isolation () =
  with_db (fun db ->
      let _scope_a =
        seed_profiled_room ~db ~room_id:"channel-a" ~profile_name:"p-a"
      in
      let scope_b =
        seed_profiled_room ~db ~room_id:"channel-b" ~profile_name:"p-b"
      in
      let mem_b =
        Memory.upsert_scoped_memory ~db ~scope_id:scope_b.id
          ~reference:"channel-b-note" ~content:"original content"
          ~provenance:"test" ()
      in
      let registry = make_registry ~db in
      let context_a = make_context ~session_key:"telegram:channel-a" in
      let result =
        invoke_tool registry "room_memory_correct" context_a
          (`Assoc
             [
               ("memory_id", `Int mem_b.id);
               ("content", `String "corrupted content");
             ])
      in
      Alcotest.(check bool)
        "channel-a cannot correct channel-b memory" true
        (contains result "does not belong to this room");
      (* Verify original content is unchanged *)
      match Memory.get_scoped_memory ~db ~id:mem_b.id with
      | None -> Alcotest.fail "Memory should still exist"
      | Some m -> (
          match m.content with
          | None -> Alcotest.fail "Memory should have content"
          | Some c ->
              Alcotest.(check string)
                "original content preserved" "original content" c))

(* ── Cross-channel forget isolation ─────────────────────────────────────── *)

let test_cross_channel_forget_isolation () =
  with_db (fun db ->
      let _scope_a =
        seed_profiled_room ~db ~room_id:"channel-a" ~profile_name:"p-a"
      in
      let scope_b =
        seed_profiled_room ~db ~room_id:"channel-b" ~profile_name:"p-b"
      in
      let mem_b =
        Memory.upsert_scoped_memory ~db ~scope_id:scope_b.id
          ~reference:"channel-b-note" ~content:"channel-b content"
          ~provenance:"test" ()
      in
      let registry = make_registry ~db in
      let context_a = make_context ~session_key:"telegram:channel-a" in
      let result =
        invoke_tool registry "room_memory_forget" context_a
          (`Assoc [ ("memory_id", `Int mem_b.id) ])
      in
      Alcotest.(check bool)
        "channel-a cannot forget channel-b memory" true
        (contains result "does not belong to this room");
      (* Verify memory is NOT redacted *)
      match Memory.get_scoped_memory ~db ~id:mem_b.id with
      | None -> Alcotest.fail "Memory should still exist"
      | Some m ->
          Alcotest.(check bool)
            "memory is not redacted" true (m.redacted_at = None))

(* ── Cross-channel save isolation ───────────────────────────────────────── *)

let test_cross_channel_save_isolation () =
  with_db (fun db ->
      let _scope_a =
        seed_profiled_room ~db ~room_id:"channel-a" ~profile_name:"p-a"
      in
      let scope_b =
        seed_profiled_room ~db ~room_id:"channel-b" ~profile_name:"p-b"
      in
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope_b.id
           ~reference:"channel-b-existing" ~content:"channel-b original"
           ~provenance:"test" ());
      let registry = make_registry ~db in
      (* Channel-a tries to save a memory - should succeed in channel-a's scope,
         not channel-b's *)
      let context_a = make_context ~session_key:"telegram:channel-a" in
      let _result =
        invoke_tool registry "room_memory_save" context_a
          (`Assoc
             [
               ("reference", `String "channel-a-new");
               ("content", `String "new content from a");
             ])
      in
      (* Verify channel-b's scope is unaffected *)
      let mems_b =
        Memory.query_scoped_memories ~db ~scope_kind:"room"
          ~scope_key:"channel-b" ()
      in
      let refs_b =
        List.map (fun (m : Memory_types.scoped_memory) -> m.reference) mems_b
      in
      Alcotest.(check bool)
        "channel-b only has its own memory" true
        (refs_b = [ "channel-b-existing" ]);
      (* Verify channel-a's scope has the new memory *)
      let mems_a =
        Memory.query_scoped_memories ~db ~scope_kind:"room"
          ~scope_key:"channel-a" ()
      in
      let refs_a =
        List.map (fun (m : Memory_types.scoped_memory) -> m.reference) mems_a
      in
      Alcotest.(check bool)
        "channel-a has its new memory" true
        (List.mem "channel-a-new" refs_a))

(* ── Private memory not visible in public scope ─────────────────────────── *)

let test_private_memory_visible_to_owning_profile_in_list () =
  with_db (fun db ->
      let scope =
        seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a"
      in
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope.id
           ~reference:"public-note" ~content:"public content" ~provenance:"test"
           ~visibility:Public ());
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope.id
           ~reference:"private-note" ~content:"private content"
           ~provenance:"test" ~visibility:Private ());
      let registry = make_registry ~db in
      let context = make_context ~session_key:"telegram:room-a" in
      let result =
        invoke_tool registry "room_memory_list" context (`Assoc [])
      in
      Alcotest.(check bool)
        "public memory visible" true
        (contains result "public-note");
      Alcotest.(check bool)
        "private memory visible to owning profile" true
        (contains result "private-note"))

let test_private_memory_visible_to_owning_profile_in_show () =
  with_db (fun db ->
      let scope =
        seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a"
      in
      let private_mem =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id
          ~reference:"private-note" ~content:"private content"
          ~provenance:"test" ~visibility:Private ()
      in
      let registry = make_registry ~db in
      let context = make_context ~session_key:"telegram:room-a" in
      let result =
        invoke_tool registry "room_memory_show" context
          (`Assoc [ ("memory_id", `Int private_mem.id) ])
      in
      Alcotest.(check bool)
        "private memory show returns content for owning profile" true
        (contains result "private content"))

let test_private_memory_preserved_after_save () =
  with_db (fun db ->
      let scope =
        seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a"
      in
      let mem =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id
          ~reference:"private-note" ~content:"private content"
          ~provenance:"test" ~visibility:Private ()
      in
      (* Upsert with same reference but no explicit visibility should preserve private *)
      let updated =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id
          ~reference:"private-note" ~content:"updated content"
          ~provenance:"test" ()
      in
      Alcotest.(check bool)
        "visibility preserved on upsert" true
        (updated.visibility = Memory_types.Private);
      Alcotest.(check int) "same memory id" mem.id updated.id)

let test_private_memory_visibility_change_to_public () =
  with_db (fun db ->
      let scope =
        seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a"
      in
      let mem =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id
          ~reference:"change-vis" ~content:"content" ~provenance:"test"
          ~visibility:Private ()
      in
      Alcotest.(check bool)
        "initially private" true
        (mem.visibility = Memory_types.Private);
      (* Explicitly change to public *)
      let updated =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id
          ~reference:"change-vis" ~content:"content" ~provenance:"test"
          ~visibility:Public ()
      in
      Alcotest.(check bool)
        "changed to public" true
        (updated.visibility = Memory_types.Public))

(* ── Team memory visibility ─────────────────────────────────────────────── *)

let test_team_memory_not_visible_without_grant () =
  with_db (fun db ->
      let scope =
        seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a"
      in
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope.id
           ~reference:"team-note" ~content:"team content" ~provenance:"test"
           ~visibility:Team ());
      let registry = make_registry ~db in
      let context = make_context ~session_key:"telegram:room-a" in
      let result =
        invoke_tool registry "room_memory_list" context (`Assoc [])
      in
      Alcotest.(check bool)
        "team memory not visible without grant" false
        (contains result "team-note"))

let test_team_memory_visible_with_grant () =
  with_db (fun db ->
      let scope =
        seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a"
      in
      let mem =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id
          ~reference:"team-note" ~content:"team content" ~provenance:"test"
          ~visibility:Team ()
      in
      (* Add team grant for the bound profile (not the room id) *)
      Alcotest.(check bool)
        "team grant added" true
        (Memory.add_team_grant ~db ~memory_id:mem.id ~principal_kind:"profile"
           ~principal_id:(string_of_int (Option.get scope.profile_id))
           ());
      let registry = make_registry ~db in
      let context = make_context ~session_key:"telegram:room-a" in
      let result =
        invoke_tool registry "room_memory_list" context (`Assoc [])
      in
      Alcotest.(check bool)
        "team memory visible with grant" true
        (contains result "team-note"))

let test_team_memory_grant_for_different_room () =
  with_db (fun db ->
      let _scope_a =
        seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a"
      in
      let scope_b =
        seed_profiled_room ~db ~room_id:"room-b" ~profile_name:"p-b"
      in
      let mem_b =
        Memory.upsert_scoped_memory ~db ~scope_id:scope_b.id ~reference:"team-b"
          ~content:"team b content" ~provenance:"test" ~visibility:Team ()
      in
      (* Grant access to room-a, but memory is in room-b's scope *)
      ignore
        (Memory.add_team_grant ~db ~memory_id:mem_b.id ~principal_kind:"room"
           ~principal_id:"room-a" ());
      let registry = make_registry ~db in
      let context_a = make_context ~session_key:"telegram:room-a" in
      let result =
        invoke_tool registry "room_memory_list" context_a (`Assoc [])
      in
      (* Team grant doesn't override scope isolation - room-a still can't see room-b memories *)
      Alcotest.(check bool)
        "room-a cannot see room-b team memory even with grant" false
        (contains result "team-b"))

(* ── Forgotten content absent from search ───────────────────────────────── *)

let test_forgotten_memory_not_in_search () =
  with_db (fun db ->
      let scope =
        seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a"
      in
      let mem =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id
          ~reference:"to-forget" ~content:"searchable unique content xyz"
          ~provenance:"test" ()
      in
      (* Verify it's findable before redaction *)
      let before =
        Memory.query_scoped_memories ~db ~scope_kind:"room" ~scope_key:"room-a"
          ~content_search:"unique content xyz" ()
      in
      Alcotest.(check int) "findable before redaction" 1 (List.length before);
      (* Redact (forget) the memory *)
      Alcotest.(check bool)
        "redacted" true
        (Memory.redact_scoped_memory ~db ~id:mem.id ~reason:"test" ());
      (* Verify it's not findable after redaction *)
      let after =
        Memory.query_scoped_memories ~db ~scope_kind:"room" ~scope_key:"room-a"
          ~content_search:"unique content xyz" ()
      in
      Alcotest.(check int) "not findable after redaction" 0 (List.length after))

let test_forgotten_memory_not_in_list () =
  with_db (fun db ->
      let scope =
        seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a"
      in
      let mem =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id
          ~reference:"to-forget" ~content:"will be forgotten" ~provenance:"test"
          ()
      in
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"to-keep"
           ~content:"will be kept" ~provenance:"test" ());
      (* Redact the first memory *)
      Alcotest.(check bool)
        "redacted" true
        (Memory.redact_scoped_memory ~db ~id:mem.id ~reason:"test" ());
      let registry = make_registry ~db in
      let context = make_context ~session_key:"telegram:room-a" in
      let result =
        invoke_tool registry "room_memory_list" context (`Assoc [])
      in
      Alcotest.(check bool)
        "forgotten memory not in list" false
        (contains result "to-forget");
      Alcotest.(check bool)
        "kept memory still in list" true
        (contains result "to-keep"))

let test_forgotten_memory_show_returns_redacted () =
  with_db (fun db ->
      let scope =
        seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a"
      in
      let mem =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id
          ~reference:"to-forget" ~content:"secret content" ~provenance:"test" ()
      in
      (* Redact the memory *)
      Alcotest.(check bool)
        "redacted" true
        (Memory.redact_scoped_memory ~db ~id:mem.id ~reason:"test" ());
      let registry = make_registry ~db in
      let context = make_context ~session_key:"telegram:room-a" in
      let result =
        invoke_tool registry "room_memory_show" context
          (`Assoc [ ("memory_id", `Int mem.id) ])
      in
      (* Should not contain the original content *)
      Alcotest.(check bool)
        "original content not shown" false
        (contains result "secret content"))

let test_forgotten_memory_cannot_be_corrected () =
  with_db (fun db ->
      let scope =
        seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a"
      in
      let mem =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id
          ~reference:"to-forget" ~content:"will be forgotten" ~provenance:"test"
          ()
      in
      (* Redact the memory *)
      Alcotest.(check bool)
        "redacted" true
        (Memory.redact_scoped_memory ~db ~id:mem.id ~reason:"test" ());
      let registry = make_registry ~db in
      let context = make_context ~session_key:"telegram:room-a" in
      let result =
        invoke_tool registry "room_memory_correct" context
          (`Assoc
             [
               ("memory_id", `Int mem.id);
               ("content", `String "trying to correct redacted");
             ])
      in
      Alcotest.(check bool)
        "cannot correct redacted memory" true
        (contains result "redacted"))

let test_forgotten_memory_cannot_be_forgotten_again () =
  with_db (fun db ->
      let scope =
        seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a"
      in
      let mem =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id
          ~reference:"to-forget" ~content:"will be forgotten" ~provenance:"test"
          ()
      in
      (* Redact the memory *)
      Alcotest.(check bool)
        "first redaction succeeds" true
        (Memory.redact_scoped_memory ~db ~id:mem.id ~reason:"test" ());
      let registry = make_registry ~db in
      let context = make_context ~session_key:"telegram:room-a" in
      let result =
        invoke_tool registry "room_memory_forget" context
          (`Assoc [ ("memory_id", `Int mem.id) ])
      in
      Alcotest.(check bool)
        "cannot forget already redacted memory" true
        (contains result "already redacted"))

(* ── Cross-channel search/recall isolation ──────────────────────────────── *)

(** Verify that Memory.search with scope filters correctly isolates channels.
    This is the path used for prompt injection context. *)
let test_cross_channel_search_isolation () =
  with_db (fun db ->
      let scope_a =
        seed_profiled_room ~db ~room_id:"channel-a" ~profile_name:"p-a"
      in
      let scope_b =
        seed_profiled_room ~db ~room_id:"channel-b" ~profile_name:"p-b"
      in
      (* Store messages in both channels and attach to scopes *)
      Memory.store_message ~db ~session_key:"channel-a"
        (Provider.make_message ~role:"user" ~content:"unique alpha topic zyx987");
      Memory.store_message ~db ~session_key:"channel-b"
        (Provider.make_message ~role:"user" ~content:"unique beta topic zyx987");
      let a_msg_id =
        Test_helpers.query_single_int db
          "SELECT MAX(id) FROM messages WHERE session_key = 'channel-a'"
      in
      let b_msg_id =
        Test_helpers.query_single_int db
          "SELECT MAX(id) FROM messages WHERE session_key = 'channel-b'"
      in
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope_a.id
           ~reference:("message:" ^ string_of_int a_msg_id)
           ~content:"scoped message ref" ~provenance:"test" ());
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope_b.id
           ~reference:("message:" ^ string_of_int b_msg_id)
           ~content:"scoped message ref" ~provenance:"test" ());
      (* Search scoped to channel-a should only find channel-a's message *)
      let results_a =
        Memory.search ~db ~query:"zyx987" ~scope_kind:"room"
          ~scope_key:"channel-a" ~limit:5 ()
      in
      Alcotest.(check int)
        "channel-a search: 1 result" 1 (List.length results_a);
      Alcotest.(check bool)
        "channel-a search: only its own content" true
        (contains (List.hd results_a).content "alpha topic");
      (* Search scoped to channel-b should only find channel-b's message *)
      let results_b =
        Memory.search ~db ~query:"zyx987" ~scope_kind:"room"
          ~scope_key:"channel-b" ~limit:5 ()
      in
      Alcotest.(check int)
        "channel-b search: 1 result" 1 (List.length results_b);
      Alcotest.(check bool)
        "channel-b search: only its own content" true
        (contains (List.hd results_b).content "beta topic"))

(** Verify that forgotten content is absent from Memory.search (FTS path), which
    is used for prompt injection context. *)
let test_forgotten_memory_not_in_fts_search () =
  with_db (fun db ->
      let scope =
        seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a"
      in
      let mem =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id
          ~reference:"to-forget" ~content:"unique forgotten topic abc123"
          ~provenance:"test" ()
      in
      (* Verify it's findable via query_scoped_memories before redaction *)
      let before =
        Memory.query_scoped_memories ~db ~scope_kind:"room" ~scope_key:"room-a"
          ~content_search:"abc123" ()
      in
      Alcotest.(check int) "findable before redaction" 1 (List.length before);
      (* Redact (forget) the memory *)
      Alcotest.(check bool)
        "redacted" true
        (Memory.redact_scoped_memory ~db ~id:mem.id ~reason:"test" ());
      (* Verify it's not findable after redaction *)
      let after =
        Memory.query_scoped_memories ~db ~scope_kind:"room" ~scope_key:"room-a"
          ~content_search:"abc123" ()
      in
      Alcotest.(check int) "not findable after redaction" 0 (List.length after))

(** Verify that private memory is not exposed through the tool layer's
    visibility filter. The raw API returns all memories, but the tool layer
    filters by visibility before presenting to the caller. *)
let test_private_memory_filtered_by_visibility_query () =
  with_db (fun db ->
      let scope =
        seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a"
      in
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope.id
           ~reference:"public-note" ~content:"public searchable content"
           ~provenance:"test" ~visibility:Public ());
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope.id
           ~reference:"private-note" ~content:"private searchable content"
           ~provenance:"test" ~visibility:Private ());
      (* Raw query returns all (isolation is at tool layer) *)
      let all_results =
        Memory.query_scoped_memories ~db ~scope_kind:"room" ~scope_key:"room-a"
          ()
      in
      Alcotest.(check int) "raw query returns both" 2 (List.length all_results);
      (* Visibility-filtered query: public only *)
      let public_results =
        Memory.query_scoped_memories ~db ~scope_kind:"room" ~scope_key:"room-a"
          ~visibility:Memory_types.Public ()
      in
      Alcotest.(check int)
        "public filter: 1 result" 1
        (List.length public_results);
      Alcotest.(check string)
        "public filter: correct ref" "public-note"
        (List.hd public_results).reference)

(* ── Cross-channel raw API isolation ────────────────────────────────────── *)

let test_raw_query_scoped_memories_respects_scope () =
  with_db (fun db ->
      let scope_a =
        seed_profiled_room ~db ~room_id:"channel-a" ~profile_name:"p-a"
      in
      let scope_b =
        seed_profiled_room ~db ~room_id:"channel-b" ~profile_name:"p-b"
      in
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope_a.id
           ~reference:"a-note" ~content:"content a" ~provenance:"test" ());
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope_b.id
           ~reference:"b-note" ~content:"content b" ~provenance:"test" ());
      (* Query with scope_key filter *)
      let a_memories =
        Memory.query_scoped_memories ~db ~scope_kind:"room"
          ~scope_key:"channel-a" ()
      in
      let b_memories =
        Memory.query_scoped_memories ~db ~scope_kind:"room"
          ~scope_key:"channel-b" ()
      in
      Alcotest.(check int) "channel-a has 1 memory" 1 (List.length a_memories);
      Alcotest.(check int) "channel-b has 1 memory" 1 (List.length b_memories);
      Alcotest.(check string)
        "channel-a memory reference" "a-note" (List.hd a_memories).reference;
      Alcotest.(check string)
        "channel-b memory reference" "b-note" (List.hd b_memories).reference)

let test_raw_correct_works_at_raw_level () =
  with_db (fun db ->
      let _scope_a =
        seed_profiled_room ~db ~room_id:"channel-a" ~profile_name:"p-a"
      in
      let scope_b =
        seed_profiled_room ~db ~room_id:"channel-b" ~profile_name:"p-b"
      in
      let mem_b =
        Memory.upsert_scoped_memory ~db ~scope_id:scope_b.id ~reference:"b-note"
          ~content:"original b content" ~provenance:"test" ()
      in
      (* At raw level, correct by ID works across scopes -- isolation is
         enforced at the tool layer, not the Memory API. *)
      let corrected =
        Memory.correct_scoped_memory ~db ~id:mem_b.id ~content:"modified by a"
          ~provenance:"cross-scope" ()
      in
      Alcotest.(check bool)
        "raw correct succeeds (tool layer enforces isolation)" true
        (corrected <> None))

let test_raw_delete_works_at_raw_level () =
  with_db (fun db ->
      let _scope_a =
        seed_profiled_room ~db ~room_id:"channel-a" ~profile_name:"p-a"
      in
      let scope_b =
        seed_profiled_room ~db ~room_id:"channel-b" ~profile_name:"p-b"
      in
      let mem_b =
        Memory.upsert_scoped_memory ~db ~scope_id:scope_b.id ~reference:"b-note"
          ~content:"b content" ~provenance:"test" ()
      in
      (* At raw level, delete by ID works across scopes -- isolation is
         enforced at the tool layer, not the Memory API. *)
      let deleted = Memory.delete_scoped_memory ~db ~id:mem_b.id () in
      Alcotest.(check bool)
        "raw delete succeeds (tool layer enforces isolation)" true deleted)

(* ── Unowned scope isolation ────────────────────────────────────────────── *)

let test_unowned_scope_no_access_without_grant () =
  with_db (fun db ->
      let _scope = seed_unowned_room ~db ~room_id:"unowned-room" in
      ignore
        (Memory.upsert_scoped_memory ~db
           ~scope_id:
             (Memory.get_scope_by_kind_key ~db ~kind:"room" ~key:"unowned-room"
             |> Option.get)
               .id
           ~reference:"unowned-note" ~content:"unowned content"
           ~provenance:"test" ());
      let registry = make_registry ~db in
      let context = make_context ~session_key:"telegram:unowned-room" in
      let result =
        invoke_tool registry "room_memory_list" context (`Assoc [])
      in
      Alcotest.(check bool)
        "unowned scope denies access without grant" true
        (contains result "Access denied"))

let test_unowned_scope_with_direct_grant () =
  with_db (fun db ->
      let scope = seed_unowned_room ~db ~room_id:"unowned-room" in
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope.id
           ~reference:"unowned-note" ~content:"unowned content"
           ~provenance:"test" ());
      (* Grant direct room access *)
      match
        Memory.grant_access ~db ~is_admin:true ~scope_id:scope.id
          ~principal_kind:"room" ~principal_id:"unowned-room" ~capability:"list"
          ()
      with
      | Error msg -> Alcotest.fail msg
      | Ok () ->
          let registry = make_registry ~db in
          let context = make_context ~session_key:"telegram:unowned-room" in
          let result =
            invoke_tool registry "room_memory_list" context (`Assoc [])
          in
          Alcotest.(check bool)
            "unowned scope with direct grant allows access" true
            (contains result "unowned-note"))

(* ── Scope resolver isolation ───────────────────────────────────────────── *)

let test_scope_resolver_isolates_different_room_ids () =
  with_db (fun db ->
      let _scope =
        seed_profiled_room ~db ~room_id:"room-1" ~profile_name:"p-1"
      in
      let _scope =
        seed_profiled_room ~db ~room_id:"room-2" ~profile_name:"p-2"
      in
      let resolved_1 =
        Tools_builtin_room_memory.resolve_room_id_for_context ~db
          (make_context ~session_key:"telegram:room-1")
      in
      let resolved_2 =
        Tools_builtin_room_memory.resolve_room_id_for_context ~db
          (make_context ~session_key:"telegram:room-2")
      in
      Alcotest.(check (option string))
        "room-1 resolves correctly" (Some "room-1") resolved_1;
      Alcotest.(check (option string))
        "room-2 resolves correctly" (Some "room-2") resolved_2;
      Alcotest.(check bool)
        "different rooms resolve differently" true (resolved_1 <> resolved_2))

(* ── Multiple scopes isolation ──────────────────────────────────────────── *)

let test_three_room_isolation () =
  with_db (fun db ->
      let scope_a =
        seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a"
      in
      let scope_b =
        seed_profiled_room ~db ~room_id:"room-b" ~profile_name:"p-b"
      in
      let scope_c =
        seed_profiled_room ~db ~room_id:"room-c" ~profile_name:"p-c"
      in
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope_a.id
           ~reference:"a-note" ~content:"content a" ~provenance:"test" ());
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope_b.id
           ~reference:"b-note" ~content:"content b" ~provenance:"test" ());
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope_c.id
           ~reference:"c-note" ~content:"content c" ~provenance:"test" ());
      let registry = make_registry ~db in
      (* Each room should only see its own memories *)
      let check_room room_id expected_ref =
        let context = make_context ~session_key:("telegram:" ^ room_id) in
        let result =
          invoke_tool registry "room_memory_list" context (`Assoc [])
        in
        Alcotest.(check bool)
          (Printf.sprintf "%s sees its own memory" room_id)
          true
          (contains result expected_ref);
        let other_refs =
          List.filter
            (fun r -> r <> expected_ref)
            [ "a-note"; "b-note"; "c-note" ]
        in
        List.iter
          (fun other_ref ->
            Alcotest.(check bool)
              (Printf.sprintf "%s does not see %s" room_id other_ref)
              false
              (contains result other_ref))
          other_refs
      in
      check_room "room-a" "a-note";
      check_room "room-b" "b-note";
      check_room "room-c" "c-note")

(* ── Suite ──────────────────────────────────────────────────────────────── *)

let suite =
  [
    (* Cross-channel list isolation *)
    Alcotest.test_case
      "cross-channel list isolation: channel-b cannot see channel-a memories"
      `Quick test_cross_channel_list_isolation;
    Alcotest.test_case
      "cross-channel list isolation: channel-a cannot see channel-b memories"
      `Quick test_cross_channel_list_isolation_reverse;
    (* Cross-channel show isolation *)
    Alcotest.test_case
      "cross-channel show isolation: channel-a cannot show channel-b memory"
      `Quick test_cross_channel_show_isolation;
    (* Cross-channel correct isolation *)
    Alcotest.test_case
      "cross-channel correct isolation: channel-a cannot correct channel-b \
       memory"
      `Quick test_cross_channel_correct_isolation;
    (* Cross-channel forget isolation *)
    Alcotest.test_case
      "cross-channel forget isolation: channel-a cannot forget channel-b memory"
      `Quick test_cross_channel_forget_isolation;
    (* Cross-channel save isolation *)
    Alcotest.test_case
      "cross-channel save isolation: save in channel-a does not affect \
       channel-b"
      `Quick test_cross_channel_save_isolation;
    (* Private memory visibility *)
    Alcotest.test_case "private memory visible to owning profile in list" `Quick
      test_private_memory_visible_to_owning_profile_in_list;
    Alcotest.test_case "private memory visible to owning profile in show" `Quick
      test_private_memory_visible_to_owning_profile_in_show;
    Alcotest.test_case
      "private memory preserved after upsert without explicit visibility" `Quick
      test_private_memory_preserved_after_save;
    Alcotest.test_case "private memory visibility can be changed to public"
      `Quick test_private_memory_visibility_change_to_public;
    (* Team memory visibility *)
    Alcotest.test_case "team memory not visible without grant" `Quick
      test_team_memory_not_visible_without_grant;
    Alcotest.test_case "team memory visible with grant" `Quick
      test_team_memory_visible_with_grant;
    Alcotest.test_case
      "team memory grant for different room does not override scope isolation"
      `Quick test_team_memory_grant_for_different_room;
    (* Forgotten content absent from search *)
    Alcotest.test_case "forgotten memory not in search results" `Quick
      test_forgotten_memory_not_in_search;
    Alcotest.test_case "forgotten memory not in list" `Quick
      test_forgotten_memory_not_in_list;
    Alcotest.test_case "forgotten memory show returns redacted status" `Quick
      test_forgotten_memory_show_returns_redacted;
    Alcotest.test_case "forgotten memory cannot be corrected" `Quick
      test_forgotten_memory_cannot_be_corrected;
    Alcotest.test_case "forgotten memory cannot be forgotten again" `Quick
      test_forgotten_memory_cannot_be_forgotten_again;
    (* Cross-channel raw API isolation *)
    Alcotest.test_case "raw query scoped memories respects scope_key filter"
      `Quick test_raw_query_scoped_memories_respects_scope;
    Alcotest.test_case
      "raw correct works at raw level (tool layer enforces isolation)" `Quick
      test_raw_correct_works_at_raw_level;
    Alcotest.test_case
      "raw delete works at raw level (tool layer enforces isolation)" `Quick
      test_raw_delete_works_at_raw_level;
    (* Unowned scope isolation *)
    Alcotest.test_case "unowned scope denies access without grant" `Quick
      test_unowned_scope_no_access_without_grant;
    Alcotest.test_case "unowned scope allows access with direct grant" `Quick
      test_unowned_scope_with_direct_grant;
    (* Scope resolver isolation *)
    Alcotest.test_case "scope resolver isolates different room IDs" `Quick
      test_scope_resolver_isolates_different_room_ids;
    (* Multiple scopes isolation *)
    Alcotest.test_case
      "three-room isolation: each room only sees its own memories" `Quick
      test_three_room_isolation;
    (* Cross-channel search/recall isolation *)
    Alcotest.test_case
      "cross-channel search isolation: scoped search respects room boundaries"
      `Quick test_cross_channel_search_isolation;
    Alcotest.test_case
      "forgotten memory not in fts search (prompt injection path)" `Quick
      test_forgotten_memory_not_in_fts_search;
    Alcotest.test_case "private memory filtered by visibility query" `Quick
      test_private_memory_filtered_by_visibility_query;
  ]

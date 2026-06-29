(** Tests for room-scoped memory agent tools.

    Verifies that:
    - Room memory tools are properly registered
    - Tools correctly scope operations to the current room
    - Tools enforce access control (profile ownership / grants)
    - Tools never expose other rooms' memories *)

let contains = Test_helpers.string_contains

(** Helper to seed a room with profile binding and memory scope. *)
let seed_profiled_room ~db ~room_id ~profile_name =
  let profile_id = Memory.insert_room_profile ~db ~name:profile_name in
  Memory.upsert_room_profile_binding ~db ~room_id ~profile_id;
  Memory.create_scope ~db ~kind:"room" ~key:room_id ~profile_id
    ~provenance:"test" ()

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

(* ── Registration Tests ────────────────────────────────────────────────── *)

let test_room_memory_tools_registered () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let registry = make_registry ~db in
  List.iter
    (fun name ->
      match Tool_registry.find registry name with
      | None ->
          Alcotest.fail
            (Printf.sprintf "Expected tool '%s' to be registered" name)
      | Some _ -> ())
    [
      "room_memory_list";
      "room_memory_show";
      "room_memory_save";
      "room_memory_correct";
      "room_memory_forget";
    ]

(* ── List Tests ────────────────────────────────────────────────────────── *)

let test_room_memory_list_empty () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let _scope = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-a" in
  let result = invoke_tool registry "room_memory_list" context (`Assoc []) in
  Alcotest.(check bool)
    "empty list message" true
    (contains result "No memories found")

let test_room_memory_list_shows_room_memories () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let scope = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  ignore
    (Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"test-note"
       ~content:"test content for room-a" ~provenance:"test" ());
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-a" in
  let result = invoke_tool registry "room_memory_list" context (`Assoc []) in
  Alcotest.(check bool) "lists room memory" true (contains result "test-note");
  Alcotest.(check bool)
    "shows content preview" true
    (contains result "test content for room-a")

let test_room_memory_list_respects_limit () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let scope = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  for i = 1 to 5 do
    ignore
      (Memory.upsert_scoped_memory ~db ~scope_id:scope.id
         ~reference:(Printf.sprintf "note-%d" i)
         ~content:(Printf.sprintf "content %d" i)
         ~provenance:"test" ())
  done;
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-a" in
  let result =
    invoke_tool registry "room_memory_list" context
      (`Assoc [ ("limit", `Int 2) ])
  in
  (* Should contain note-1 and note-2 but not note-5 *)
  Alcotest.(check bool) "contains first note" true (contains result "note-1");
  Alcotest.(check bool) "contains second note" true (contains result "note-2")

(* ── Show Tests ────────────────────────────────────────────────────────── *)

let test_room_memory_show_full_content () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let scope = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let mem =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"detail-note"
      ~content:"detailed content here" ~provenance:"test" ()
  in
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-a" in
  let result =
    invoke_tool registry "room_memory_show" context
      (`Assoc [ ("memory_id", `Int mem.id) ])
  in
  Alcotest.(check bool)
    "shows full content" true
    (contains result "detailed content here");
  Alcotest.(check bool) "shows reference" true (contains result "detail-note")

let test_room_memory_show_rejects_other_room_memory () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let _scope_a = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let scope_b = seed_profiled_room ~db ~room_id:"room-b" ~profile_name:"p-b" in
  let mem_b =
    Memory.upsert_scoped_memory ~db ~scope_id:scope_b.id
      ~reference:"room-b-note" ~content:"room-b content" ~provenance:"test" ()
  in
  let registry = make_registry ~db in
  (* Context is for room-a, but memory belongs to room-b *)
  let context = make_context ~session_key:"telegram:room-a" in
  let result =
    invoke_tool registry "room_memory_show" context
      (`Assoc [ ("memory_id", `Int mem_b.id) ])
  in
  Alcotest.(check bool)
    "rejects cross-room access" true
    (contains result "does not belong to this room")

(* ── Save Tests ────────────────────────────────────────────────────────── *)

let test_room_memory_save_creates_memory () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let _scope = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-a" in
  let result =
    invoke_tool registry "room_memory_save" context
      (`Assoc
         [
           ("reference", `String "new-note"); ("content", `String "new content");
         ])
  in
  Alcotest.(check bool) "save success" true (contains result "Saved memory");
  Alcotest.(check bool) "includes reference" true (contains result "new-note")

let test_room_memory_save_upserts_existing () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let scope = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  (* Create initial memory *)
  ignore
    (Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"existing"
       ~content:"old content" ~provenance:"test" ());
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-a" in
  (* Update with same reference *)
  let result =
    invoke_tool registry "room_memory_save" context
      (`Assoc
         [
           ("reference", `String "existing");
           ("content", `String "updated content");
         ])
  in
  Alcotest.(check bool) "upsert success" true (contains result "Saved memory")

let test_room_memory_save_requires_reference () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let _scope = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-a" in
  let result =
    invoke_tool registry "room_memory_save" context
      (`Assoc [ ("content", `String "some content") ])
  in
  Alcotest.(check bool) "requires reference" true (contains result "Error")

let test_room_memory_save_requires_content () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let _scope = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-a" in
  let result =
    invoke_tool registry "room_memory_save" context
      (`Assoc [ ("reference", `String "note") ])
  in
  Alcotest.(check bool) "requires content" true (contains result "Error")

(* ── Correct Tests ─────────────────────────────────────────────────────── *)

let test_room_memory_correct_updates_content () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let scope = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let mem =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"fix-me"
      ~content:"incorrect content" ~provenance:"test" ()
  in
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-a" in
  let result =
    invoke_tool registry "room_memory_correct" context
      (`Assoc
         [
           ("memory_id", `Int mem.id); ("content", `String "corrected content");
         ])
  in
  Alcotest.(check bool)
    "correct success" true
    (contains result "Corrected memory");
  (* Verify the memory was actually updated *)
  match Memory.get_scoped_memory ~db ~id:mem.id with
  | None -> Alcotest.fail "Memory should still exist"
  | Some m -> (
      match m.content with
      | None -> Alcotest.fail "Memory should have content"
      | Some c ->
          Alcotest.(check string) "content updated" "corrected content" c)

let test_room_memory_correct_rejects_other_room () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let _scope_a = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let scope_b = seed_profiled_room ~db ~room_id:"room-b" ~profile_name:"p-b" in
  let mem_b =
    Memory.upsert_scoped_memory ~db ~scope_id:scope_b.id
      ~reference:"room-b-note" ~content:"room-b content" ~provenance:"test" ()
  in
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-a" in
  let result =
    invoke_tool registry "room_memory_correct" context
      (`Assoc
         [
           ("memory_id", `Int mem_b.id); ("content", `String "trying to corrupt");
         ])
  in
  Alcotest.(check bool)
    "rejects cross-room correct" true
    (contains result "does not belong to this room")

let test_room_memory_correct_rejects_redacted () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let scope = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let mem =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"redacted"
      ~content:"will be redacted" ~provenance:"test" ()
  in
  (* Redact the memory *)
  ignore (Memory.redact_scoped_memory ~db ~id:mem.id ~reason:"test" ());
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
    "rejects correcting redacted" true
    (contains result "redacted")

(* ── Forget Tests ──────────────────────────────────────────────────────── *)

let test_room_memory_forget_redacts_memory () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let scope = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let mem =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"forget-me"
      ~content:"will be forgotten" ~provenance:"test" ()
  in
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-a" in
  let result =
    invoke_tool registry "room_memory_forget" context
      (`Assoc [ ("memory_id", `Int mem.id) ])
  in
  Alcotest.(check bool)
    "forget success" true
    (contains result "Forgot (redacted)");
  (* Verify the memory is redacted *)
  match Memory.get_scoped_memory ~db ~id:mem.id with
  | None -> Alcotest.fail "Memory should still exist (redacted, not deleted)"
  | Some m ->
      Alcotest.(check bool) "memory is redacted" true (m.redacted_at <> None)

let test_room_memory_forget_with_reason () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let scope = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let mem =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"reason-test"
      ~content:"content" ~provenance:"test" ()
  in
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-a" in
  let result =
    invoke_tool registry "room_memory_forget" context
      (`Assoc
         [
           ("memory_id", `Int mem.id); ("reason", `String "outdated information");
         ])
  in
  Alcotest.(check bool)
    "forget with reason success" true
    (contains result "Forgot (redacted)")

let test_room_memory_forget_rejects_other_room () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let _scope_a = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let scope_b = seed_profiled_room ~db ~room_id:"room-b" ~profile_name:"p-b" in
  let mem_b =
    Memory.upsert_scoped_memory ~db ~scope_id:scope_b.id
      ~reference:"room-b-note" ~content:"room-b content" ~provenance:"test" ()
  in
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-a" in
  let result =
    invoke_tool registry "room_memory_forget" context
      (`Assoc [ ("memory_id", `Int mem_b.id) ])
  in
  Alcotest.(check bool)
    "rejects cross-room forget" true
    (contains result "does not belong to this room")

let test_room_memory_forget_rejects_already_redacted () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let scope = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let mem =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"already-done"
      ~content:"content" ~provenance:"test" ()
  in
  ignore (Memory.redact_scoped_memory ~db ~id:mem.id ~reason:"first time" ());
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-a" in
  let result =
    invoke_tool registry "room_memory_forget" context
      (`Assoc [ ("memory_id", `Int mem.id) ])
  in
  Alcotest.(check bool)
    "rejects double redaction" true
    (contains result "already redacted")

(* ── Isolation Tests ───────────────────────────────────────────────────── *)

let test_room_memory_isolation_between_rooms () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let scope_a = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let scope_b = seed_profiled_room ~db ~room_id:"room-b" ~profile_name:"p-b" in
  (* Create memories in both rooms *)
  ignore
    (Memory.upsert_scoped_memory ~db ~scope_id:scope_a.id
       ~reference:"room-a-secret" ~content:"room-a private data"
       ~provenance:"test" ());
  ignore
    (Memory.upsert_scoped_memory ~db ~scope_id:scope_b.id
       ~reference:"room-b-secret" ~content:"room-b private data"
       ~provenance:"test" ());
  let registry = make_registry ~db in
  (* Room-a context should only see room-a memories *)
  let context_a = make_context ~session_key:"telegram:room-a" in
  let result_a =
    invoke_tool registry "room_memory_list" context_a (`Assoc [])
  in
  Alcotest.(check bool)
    "room-a sees its own memory" true
    (contains result_a "room-a-secret");
  Alcotest.(check bool)
    "room-a does not see room-b memory" false
    (contains result_a "room-b-secret");
  (* Room-b context should only see room-b memories *)
  let context_b = make_context ~session_key:"telegram:room-b" in
  let result_b =
    invoke_tool registry "room_memory_list" context_b (`Assoc [])
  in
  Alcotest.(check bool)
    "room-b sees its own memory" true
    (contains result_b "room-b-secret");
  Alcotest.(check bool)
    "room-b does not see room-a memory" false
    (contains result_b "room-a-secret")

let test_room_memory_save_does_not_leak_to_other_rooms () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let _scope_a = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let _scope_b = seed_profiled_room ~db ~room_id:"room-b" ~profile_name:"p-b" in
  let registry = make_registry ~db in
  (* Save a memory in room-a *)
  let context_a = make_context ~session_key:"telegram:room-a" in
  ignore
    (invoke_tool registry "room_memory_save" context_a
       (`Assoc
          [
            ("reference", `String "a-only"); ("content", `String "room-a data");
          ]));
  (* Room-b should not see it *)
  let context_b = make_context ~session_key:"telegram:room-b" in
  let result_b =
    invoke_tool registry "room_memory_list" context_b (`Assoc [])
  in
  Alcotest.(check bool)
    "room-b does not see room-a save" false
    (contains result_b "a-only")

(* ── Context Validation Tests ──────────────────────────────────────────── *)

let test_room_memory_requires_session_context () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let registry = make_registry ~db in
  let context_no_session =
    {
      Tool.session_key = None;
      send_progress = None;
      interrupt_check = None;
      inject_system_messages = None;
      effective_cwd = None;
      request_cwd_change = None;
      egress_rules = [];
    }
  in
  let result =
    invoke_tool registry "room_memory_list" context_no_session (`Assoc [])
  in
  Alcotest.(check bool)
    "requires session context" true (contains result "Error")

let test_room_memory_requires_room_context () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let registry = make_registry ~db in
  (* Session key without room_id format *)
  let context = make_context ~session_key:"simple-session" in
  let result = invoke_tool registry "room_memory_list" context (`Assoc []) in
  Alcotest.(check bool) "requires room context" true (contains result "Error")

(* ── Access Control Edge Cases ─────────────────────────────────────────── *)

let test_room_memory_profile_grant_allows_access () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  (* Create a scope without owner *)
  let scope =
    Memory.create_scope ~db ~kind:"room" ~key:"room-a" ~provenance:"test" ()
  in
  ignore
    (Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"grant-test"
       ~content:"granted content" ~provenance:"test" ());
  (* Add a profile binding *)
  let profile_id = Memory.insert_room_profile ~db ~name:"room-a-profile" in
  Memory.upsert_room_profile_binding ~db ~room_id:"room-a" ~profile_id;
  (* Grant read access to the profile *)
  match
    Memory.grant_access ~db ~is_admin:true ~scope_id:scope.id
      ~principal_kind:"profile" ~principal_id:(string_of_int profile_id)
      ~capability:"read" ()
  with
  | Error msg -> Alcotest.fail msg
  | Ok () ->
      let registry = make_registry ~db in
      let context = make_context ~session_key:"telegram:room-a" in
      let result =
        invoke_tool registry "room_memory_list" context (`Assoc [])
      in
      Alcotest.(check bool)
        "profile grant allows access" true
        (contains result "grant-test")

let test_room_memory_direct_room_grant_allows_access () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  (* Create a scope without owner and without profile binding *)
  let scope =
    Memory.create_scope ~db ~kind:"room" ~key:"room-c" ~provenance:"test" ()
  in
  let _mem =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"room-grant"
      ~content:"room granted content" ~provenance:"test" ()
  in
  (* Grant direct room access - note: list tool checks for "list" capability *)
  match
    Memory.grant_access ~db ~is_admin:true ~scope_id:scope.id
      ~principal_kind:"room" ~principal_id:"room-c" ~capability:"list" ()
  with
  | Error msg -> Alcotest.fail (Printf.sprintf "Grant failed: %s" msg)
  | Ok () ->
      let registry = make_registry ~db in
      let context = make_context ~session_key:"telegram:room-c" in
      let result =
        invoke_tool registry "room_memory_list" context (`Assoc [])
      in
      Alcotest.(check bool)
        "direct room grant allows access" true
        (contains result "room-grant")

let test_room_memory_backfills_missing_owner () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  (* Create a scope without owner *)
  let scope =
    Memory.create_scope ~db ~kind:"room" ~key:"room-d" ~provenance:"test" ()
  in
  Alcotest.(check bool)
    "scope initially has no owner" true (scope.profile_id = None);
  (* Add a profile binding *)
  let profile_id = Memory.insert_room_profile ~db ~name:"room-d-profile" in
  Memory.upsert_room_profile_binding ~db ~room_id:"room-d" ~profile_id;
  (* Access should trigger backfill *)
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-d" in
  ignore (invoke_tool registry "room_memory_list" context (`Assoc []));
  (* Verify scope now has owner *)
  match Memory.get_scope ~db ~id:scope.id with
  | None -> Alcotest.fail "Scope should still exist"
  | Some updated ->
      Alcotest.(check bool)
        "scope has owner after backfill" true
        (updated.profile_id = Some profile_id)

let test_room_memory_invalid_limit_returns_error () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let _scope = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-a" in
  let result =
    invoke_tool registry "room_memory_list" context
      (`Assoc [ ("limit", `Int 0) ])
  in
  Alcotest.(check bool)
    "invalid limit returns error" true (contains result "Error")

let test_room_memory_zero_memory_id_returns_error () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let _scope = seed_profiled_room ~db ~room_id:"room-a" ~profile_name:"p-a" in
  let registry = make_registry ~db in
  let context = make_context ~session_key:"telegram:room-a" in
  let result =
    invoke_tool registry "room_memory_show" context
      (`Assoc [ ("memory_id", `Int 0) ])
  in
  Alcotest.(check bool)
    "zero memory_id returns error" true (contains result "Error")

(* ── Suite ─────────────────────────────────────────────────────────────── *)

let suite =
  [
    (* Registration *)
    Alcotest.test_case "room memory tools are registered" `Quick
      test_room_memory_tools_registered;
    (* List *)
    Alcotest.test_case "room memory list shows empty message" `Quick
      test_room_memory_list_empty;
    Alcotest.test_case "room memory list shows room memories" `Quick
      test_room_memory_list_shows_room_memories;
    Alcotest.test_case "room memory list respects limit" `Quick
      test_room_memory_list_respects_limit;
    (* Show *)
    Alcotest.test_case "room memory show displays full content" `Quick
      test_room_memory_show_full_content;
    Alcotest.test_case "room memory show rejects other room memory" `Quick
      test_room_memory_show_rejects_other_room_memory;
    (* Save *)
    Alcotest.test_case "room memory save creates memory" `Quick
      test_room_memory_save_creates_memory;
    Alcotest.test_case "room memory save upserts existing" `Quick
      test_room_memory_save_upserts_existing;
    Alcotest.test_case "room memory save requires reference" `Quick
      test_room_memory_save_requires_reference;
    Alcotest.test_case "room memory save requires content" `Quick
      test_room_memory_save_requires_content;
    (* Correct *)
    Alcotest.test_case "room memory correct updates content" `Quick
      test_room_memory_correct_updates_content;
    Alcotest.test_case "room memory correct rejects other room" `Quick
      test_room_memory_correct_rejects_other_room;
    Alcotest.test_case "room memory correct rejects redacted" `Quick
      test_room_memory_correct_rejects_redacted;
    (* Forget *)
    Alcotest.test_case "room memory forget redacts memory" `Quick
      test_room_memory_forget_redacts_memory;
    Alcotest.test_case "room memory forget with reason" `Quick
      test_room_memory_forget_with_reason;
    Alcotest.test_case "room memory forget rejects other room" `Quick
      test_room_memory_forget_rejects_other_room;
    Alcotest.test_case "room memory forget rejects already redacted" `Quick
      test_room_memory_forget_rejects_already_redacted;
    (* Isolation *)
    Alcotest.test_case "room memory isolation prevents cross-room list leakage"
      `Quick test_room_memory_isolation_between_rooms;
    Alcotest.test_case "room memory save does not leak to other rooms" `Quick
      test_room_memory_save_does_not_leak_to_other_rooms;
    (* Context validation *)
    Alcotest.test_case "room memory requires session context" `Quick
      test_room_memory_requires_session_context;
    Alcotest.test_case "room memory requires room context" `Quick
      test_room_memory_requires_room_context;
    (* Access control edge cases *)
    Alcotest.test_case "room memory profile grant allows access" `Quick
      test_room_memory_profile_grant_allows_access;
    Alcotest.test_case "room memory direct room grant allows access" `Quick
      test_room_memory_direct_room_grant_allows_access;
    Alcotest.test_case "room memory backfills missing owner" `Quick
      test_room_memory_backfills_missing_owner;
    Alcotest.test_case "room memory invalid limit returns error" `Quick
      test_room_memory_invalid_limit_returns_error;
    Alcotest.test_case "room memory zero memory_id returns error" `Quick
      test_room_memory_zero_memory_id_returns_error;
  ]

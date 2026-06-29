(** Runtime conformance tests for proof candidates.

    These tests verify invariants documented in
    - docs/scope-resolution-invariants.md
    - docs/memory-policy-isolation-invariants.md

    that lacked dedicated executable tests. Each test is tagged with the
    invariant it enforces. *)

let parse json = Config_loader.parse_config (Yojson.Safe.from_string json)

let item_values items =
  List.map
    (fun (item : Runtime_config.effective_access_item) -> item.value)
    items

(* ── Scope Resolver Conformance ─────────────────────────────────────────── *)

(** INV-DET-2: Same-level scopes are merged in lexicographic order by scope id.

    Two default-level scopes with ids "b-default" and "a-default" should produce
    tools in "a-default" before "b-default" order (lexicographic). *)
let test_same_level_scopes_ordered_lexicographically () =
  let json =
    {|{
      "access_bundles": [
        {"id": "bundle-a", "allowed_tools": ["tool_a"]},
        {"id": "bundle-b", "allowed_tools": ["tool_b"]}
      ],
      "access_scopes": [
        {"id": "b-default", "level": "default", "access_bundle_ids": ["bundle-b"]},
        {"id": "a-default", "level": "default", "access_bundle_ids": ["bundle-a"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check (list string))
    "same-level scopes ordered lexicographically" [ "tool_a"; "tool_b" ]
    (item_values effective.allowed_tools)

(** INV-PREC-3: Within the same scope, bundles referenced by access_bundle_ids
    are merged in declaration order (list order preserved). *)
let test_same_scope_bundle_declaration_order_preserved () =
  let json =
    {|{
      "access_bundles": [
        {"id": "first", "allowed_tools": ["first_tool"]},
        {"id": "second", "allowed_tools": ["second_tool"]},
        {"id": "third", "allowed_tools": ["third_tool"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["first", "third", "second"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check (list string))
    "bundle declaration order preserved within scope"
    [ "first_tool"; "third_tool"; "second_tool" ]
    (item_values effective.allowed_tools)

(** INV-ACT-1: Scopes with status "deleted" are filtered out during resolution.

    A default scope with status "deleted" should not contribute any grants, even
    if it references a valid bundle. A non-deleted default scope should still
    work. *)
let test_deleted_scopes_filtered_during_resolution () =
  let json =
    {|{
      "access_bundles": [
        {"id": "active-bundle", "allowed_tools": ["active_tool"]},
        {"id": "deleted-bundle", "allowed_tools": ["deleted_tool"]}
      ],
      "access_scopes": [
        {"id": "active-scope", "level": "default", "access_bundle_ids": ["active-bundle"]},
        {"id": "deleted-scope", "level": "default", "status": "deleted", "access_bundle_ids": ["deleted-bundle"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check (list string))
    "active scope grants present" [ "active_tool" ]
    (item_values effective.allowed_tools);
  Alcotest.(check bool)
    "deleted scope grants absent" false
    (List.mem "deleted_tool" (item_values effective.allowed_tools))

(** INV-ACT-1 (case): Deleted status is case-insensitive. Status "DELETED"
    should also be filtered. *)
let test_deleted_scopes_case_insensitive () =
  let json =
    {|{
      "access_bundles": [
        {"id": "bundle-a", "allowed_tools": ["tool_a"]},
        {"id": "bundle-b", "allowed_tools": ["tool_b"]}
      ],
      "access_scopes": [
        {"id": "active", "level": "default", "access_bundle_ids": ["bundle-a"]},
        {"id": "deleted-upper", "level": "default", "status": "DELETED", "access_bundle_ids": ["bundle-b"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check (list string))
    "only active scope contributes tools" [ "tool_a" ]
    (item_values effective.allowed_tools)

(** INV-ACT-1 (room): Deleted room scopes are filtered. A room-level scope with
    status "deleted" should not contribute grants. *)
let test_deleted_room_scope_filtered () =
  let json =
    {|{
      "access_bundles": [
        {"id": "active-bundle", "allowed_tools": ["active_tool"]},
        {"id": "deleted-bundle", "allowed_tools": ["deleted_tool"]}
      ],
      "access_scopes": [
        {"id": "active-room", "level": "room", "room": "C123", "access_bundle_ids": ["active-bundle"]},
        {"id": "deleted-room", "level": "room", "room": "C123", "status": "deleted", "access_bundle_ids": ["deleted-bundle"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check (list string))
    "active room scope grants present" [ "active_tool" ]
    (item_values effective.allowed_tools);
  Alcotest.(check bool)
    "deleted room scope grants absent" false
    (List.mem "deleted_tool" (item_values effective.allowed_tools))

(** INV-DET-2 + INV-PREC-1: Mixed-level scopes are ordered by level rank
    ascending, then by scope id lexicographic within same level. *)
let test_mixed_level_scopes_ordered_by_rank_then_lexicographic () =
  let json =
    {|{
      "access_bundles": [
        {"id": "room-bundle", "allowed_tools": ["room_tool"]},
        {"id": "default-bundle", "allowed_tools": ["default_tool"]},
        {"id": "room-bundle-2", "allowed_tools": ["room_tool_2"]}
      ],
      "access_scopes": [
        {"id": "z-room", "level": "room", "room": "C123", "access_bundle_ids": ["room-bundle"]},
        {"id": "a-room", "level": "room", "room": "C123", "access_bundle_ids": ["room-bundle-2"]},
        {"id": "default", "level": "default", "access_bundle_ids": ["default-bundle"]}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  (* Default (rank 0) before room (rank 3); within room, "a-room" < "z-room" *)
  Alcotest.(check (list string))
    "mixed level ordering: default then room lexicographic"
    [ "default_tool"; "room_tool_2"; "room_tool" ]
    (item_values effective.allowed_tools)

(** INV-DET-2 (resolver determinism): Calling the resolver twice with identical
    inputs produces identical results. *)
let test_resolver_determinism_across_calls () =
  let json =
    {|{
      "access_bundles": [
        {"id": "bundle", "allowed_tools": ["tool_a", "tool_b"], "denied_tools": ["tool_c"]}
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["bundle"]}
      ]
    }|}
  in
  let cfg = parse json in
  let eff1 =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  let eff2 =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  Alcotest.(check (list string))
    "allowed tools identical across calls"
    (item_values eff1.allowed_tools)
    (item_values eff2.allowed_tools);
  Alcotest.(check (list string))
    "denied tools identical across calls"
    (item_values eff1.denied_tools)
    (item_values eff2.denied_tools)

(* ── Egress Evaluator Conformance ───────────────────────────────────────── *)

(** INV-EGR-3: Egress rules are evaluated in declaration order (first match
    wins). Rule ordering in the configuration determines which rule applies to a
    given request. *)
let test_egress_rules_first_match_wins () =
  let rules =
    [
      {
        Runtime_config.host = "*.example.com";
        path = None;
        method_ = None;
        action = Allow;
        log_policy = Log;
      };
      {
        host = "api.example.com";
        path = None;
        method_ = None;
        action = Deny;
        log_policy = No_log;
      };
    ]
  in
  let result = Egress_evaluator.evaluate ~rules ~host:"api.example.com" () in
  Alcotest.(check bool)
    "first match wins: wildcard allow before specific deny" true
    (match result.action with Runtime_config.Allow -> true | _ -> false);
  Alcotest.(check int) "matched at index 0" 0 result.matched_rule_index

(** INV-EGR-1 + INV-EGR-3: Reversing rule order changes the outcome. With deny
    first, the same host should be denied. *)
let test_egress_rules_order_matters () =
  let rules_allow_first =
    [
      {
        Runtime_config.host = "*.example.com";
        path = None;
        method_ = None;
        action = Allow;
        log_policy = Log;
      };
      {
        host = "*";
        path = None;
        method_ = None;
        action = Deny;
        log_policy = Log;
      };
    ]
  in
  let rules_deny_first =
    [
      {
        Runtime_config.host = "*";
        path = None;
        method_ = None;
        action = Deny;
        log_policy = Log;
      };
      {
        host = "*.example.com";
        path = None;
        method_ = None;
        action = Allow;
        log_policy = Log;
      };
    ]
  in
  let result_allow_first =
    Egress_evaluator.evaluate ~rules:rules_allow_first ~host:"api.example.com"
      ()
  in
  let result_deny_first =
    Egress_evaluator.evaluate ~rules:rules_deny_first ~host:"api.example.com" ()
  in
  Alcotest.(check bool)
    "allow-first produces Allow" true
    (match result_allow_first.action with
    | Runtime_config.Allow -> true
    | _ -> false);
  Alcotest.(check bool)
    "deny-first produces Deny" true
    (match result_deny_first.action with
    | Runtime_config.Deny -> true
    | _ -> false)

(** INV-EGR-1: Default-deny when no rules match. Unmatched destinations receive
    the default-deny action. *)
let test_egress_unmatched_destinations_default_deny () =
  let rules =
    [
      {
        Runtime_config.host = "allowed.example.com";
        path = None;
        method_ = None;
        action = Allow;
        log_policy = Log;
      };
    ]
  in
  let result = Egress_evaluator.evaluate ~rules ~host:"other.example.com" () in
  Alcotest.(check bool)
    "unmatched host defaults to Deny" true
    (match result.action with Runtime_config.Deny -> true | _ -> false);
  Alcotest.(check int) "no rule matched" (-1) result.matched_rule_index;
  Alcotest.(check bool)
    "default deny logs" true
    (match result.log_policy with Runtime_config.Log -> true | _ -> false)

(* ── Memory Policy: Grant Resolution Conformance ────────────────────────── *)

(** INV-GRANT-3: Expired grants are excluded from resolution. Grants with
    expires_at in the past are filtered out. *)
let test_expired_grants_excluded_from_resolution () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let scope =
        Memory.create_scope ~db ~kind:"room" ~key:"test-room" ~provenance:"test"
          ()
      in
      ignore
        (Memory.grant_access ~db ~is_admin:true ~scope_id:scope.id
           ~principal_kind:"room" ~principal_id:"test-room" ~capability:"list"
           ());
      (* Manually set expires_at to the past *)
      let stmt =
        Sqlite3.prepare db
          "UPDATE memory_grants SET expires_at = '2020-01-01 00:00:00' WHERE \
           scope_id = ? AND capability = 'list'"
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore
            (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int scope.id)));
          ignore (Sqlite3.step stmt));
      (* Verify expired grants are not returned *)
      let grants =
        Memory.resolve_grants ~db ~scope_id:scope.id ~principal_kind:"room"
          ~principal_id:"test-room"
      in
      Alcotest.(check (list string)) "expired grants excluded" [] grants)

(** INV-GRANT-3 (non-expired): Grants with expires_at in the future are included
    in resolution. *)
let test_non_expired_grants_included_in_resolution () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let scope =
        Memory.create_scope ~db ~kind:"room" ~key:"test-room" ~provenance:"test"
          ()
      in
      ignore
        (Memory.grant_access ~db ~is_admin:true ~scope_id:scope.id
           ~principal_kind:"room" ~principal_id:"test-room" ~capability:"list"
           ());
      (* Set expires_at to the future *)
      let stmt =
        Sqlite3.prepare db
          "UPDATE memory_grants SET expires_at = '2099-12-31 23:59:59' WHERE \
           scope_id = ? AND capability = 'list'"
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore
            (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int scope.id)));
          ignore (Sqlite3.step stmt));
      let grants =
        Memory.resolve_grants ~db ~scope_id:scope.id ~principal_kind:"room"
          ~principal_id:"test-room"
      in
      Alcotest.(check (list string))
        "non-expired grants included" [ "list" ] grants)

(** INV-GRANT-3 (null expires_at): Grants with NULL expires_at never expire. *)
let test_null_expires_at_grants_never_expire () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let scope =
        Memory.create_scope ~db ~kind:"room" ~key:"test-room" ~provenance:"test"
          ()
      in
      (* grant_access does not set expires_at, so it defaults to NULL *)
      ignore
        (Memory.grant_access ~db ~is_admin:true ~scope_id:scope.id
           ~principal_kind:"room" ~principal_id:"test-room" ~capability:"read"
           ());
      let grants =
        Memory.resolve_grants ~db ~scope_id:scope.id ~principal_kind:"room"
          ~principal_id:"test-room"
      in
      Alcotest.(check (list string))
        "null expires_at grants are included" [ "read" ] grants)

(** INV-GRANT-4: Revoked grants are excluded from resolution when the revoked_at
    column exists. The resolver dynamically checks for column existence and adds
    the revoked_at IS NULL clause. *)
let test_revoked_grants_excluded_when_column_exists () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let scope =
        Memory.create_scope ~db ~kind:"room" ~key:"test-room" ~provenance:"test"
          ()
      in
      ignore
        (Memory.grant_access ~db ~is_admin:true ~scope_id:scope.id
           ~principal_kind:"room" ~principal_id:"test-room" ~capability:"write"
           ());
      (* Add the revoked_at column dynamically (ignore if already exists) *)
      ignore
        (Sqlite3.exec db "ALTER TABLE memory_grants ADD COLUMN revoked_at TEXT");
      (* Set revoked_at to mark the grant as revoked *)
      let stmt =
        Sqlite3.prepare db
          "UPDATE memory_grants SET revoked_at = datetime('now') WHERE \
           scope_id = ? AND capability = 'write'"
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore
            (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int scope.id)));
          ignore (Sqlite3.step stmt));
      (* Verify revoked grants are not returned *)
      let grants =
        Memory.resolve_grants ~db ~scope_id:scope.id ~principal_kind:"room"
          ~principal_id:"test-room"
      in
      Alcotest.(check (list string))
        "revoked grants excluded when column exists" [] grants)

(** INV-GRANT-4 (non-revoked): Non-revoked grants are included when revoked_at
    column exists and value is NULL. *)
let test_non_revoked_grants_included_when_column_exists () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let scope =
        Memory.create_scope ~db ~kind:"room" ~key:"test-room" ~provenance:"test"
          ()
      in
      ignore
        (Memory.grant_access ~db ~is_admin:true ~scope_id:scope.id
           ~principal_kind:"room" ~principal_id:"test-room" ~capability:"read"
           ());
      (* Add revoked_at column (ignore if already exists) *)
      ignore
        (Sqlite3.exec db "ALTER TABLE memory_grants ADD COLUMN revoked_at TEXT");
      (* Grant has revoked_at = NULL (default) *)
      let grants =
        Memory.resolve_grants ~db ~scope_id:scope.id ~principal_kind:"room"
          ~principal_id:"test-room"
      in
      Alcotest.(check (list string))
        "non-revoked grants included" [ "read" ] grants)

(** INV-GRANT-4 (no column): When revoked_at column does not exist, all grants
    are included (no revocation filtering). *)
let test_grants_included_when_no_revoked_at_column () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let scope =
        Memory.create_scope ~db ~kind:"room" ~key:"test-room" ~provenance:"test"
          ()
      in
      ignore
        (Memory.grant_access ~db ~is_admin:true ~scope_id:scope.id
           ~principal_kind:"room" ~principal_id:"test-room" ~capability:"read"
           ());
      ignore
        (Memory.grant_access ~db ~is_admin:true ~scope_id:scope.id
           ~principal_kind:"room" ~principal_id:"test-room" ~capability:"list"
           ());
      (* No revoked_at column in default schema *)
      let grants =
        Memory.resolve_grants ~db ~scope_id:scope.id ~principal_kind:"room"
          ~principal_id:"test-room"
      in
      Alcotest.(check (list string))
        "all grants included without revoked_at column" [ "list"; "read" ]
        grants)

(* ── Memory Policy: Redaction Conformance ────────────────────────────────── *)

(** INV-REDACT-4: Redaction clears content to NULL and sets redacted_at. The
    reference is preserved or set to 'redacted:<id>' if previously NULL. *)
let seed_profiled_room ~db ~room_id ~profile_name =
  let profile_id = Memory.insert_room_profile ~db ~name:profile_name in
  Memory.upsert_room_profile_binding ~db ~room_id ~profile_id;
  Memory.create_scope ~db ~kind:"room" ~key:room_id ~profile_id
    ~provenance:"test" ()

(** INV-REDACT-4: Redaction clears content to NULL and sets redacted_at. The
    reference is preserved or set to 'redacted:<id>' if previously NULL. *)
let test_redaction_sets_redacted_at_and_clears_content () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let scope =
        seed_profiled_room ~db ~room_id:"test-room" ~profile_name:"test-profile"
      in
      let mem =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"test-ref"
          ~content:"sensitive data" ~provenance:"test" ()
      in
      let success =
        Memory.redact_scoped_memory ~db ~id:mem.id ~reason:"gdpr" ()
      in
      Alcotest.(check bool) "redaction succeeds" true success;
      let redacted = Memory.get_scoped_memory ~db ~id:mem.id in
      match redacted with
      | None -> Alcotest.fail "redacted memory should still exist"
      | Some m ->
          Alcotest.(check bool) "content is NULL" true (m.content = None);
          Alcotest.(check bool) "redacted_at is set" true (m.redacted_at <> None);
          Alcotest.(check (option string))
            "redaction_reason preserved" (Some "gdpr") m.redaction_reason;
          Alcotest.(check string) "reference preserved" "test-ref" m.reference)

(** INV-REDACT-4 (null reference): When redacting a memory with NULL reference,
    the reference is set to 'redacted:<id>'. *)
let test_redaction_sets_reference_when_null () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let scope =
        seed_profiled_room ~db ~room_id:"test-room" ~profile_name:"test-profile"
      in
      (* Insert with NULL reference via raw SQL — but CHECK requires
         at least one of content/reference to be non-null *)
      let stmt =
        Sqlite3.prepare db
          "INSERT INTO scoped_memories (scope_id, reference, content, \
           provenance) VALUES (?, NULL, 'content', 'test')"
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore
            (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int scope.id)));
          ignore (Sqlite3.step stmt));
      let mem_id =
        Test_helpers.query_single_int db "SELECT MAX(id) FROM scoped_memories"
      in
      let success =
        Memory.redact_scoped_memory ~db ~id:mem_id ~reason:"test" ()
      in
      Alcotest.(check bool) "redaction succeeds" true success;
      let redacted = Memory.get_scoped_memory ~db ~id:mem_id in
      match redacted with
      | None -> Alcotest.fail "redacted memory should still exist"
      | Some m ->
          Alcotest.(check string)
            "reference set to redacted:<id>"
            (Printf.sprintf "redacted:%d" mem_id)
            m.reference)

(** INV-REDACT-1: Redaction is idempotent. Redacting an already-redacted memory
    returns false. *)
let test_redaction_idempotent () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let scope =
        seed_profiled_room ~db ~room_id:"test-room" ~profile_name:"test-profile"
      in
      let mem =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"test-ref"
          ~content:"content" ~provenance:"test" ()
      in
      let first =
        Memory.redact_scoped_memory ~db ~id:mem.id ~reason:"test" ()
      in
      Alcotest.(check bool) "first redaction succeeds" true first;
      let second =
        Memory.redact_scoped_memory ~db ~id:mem.id ~reason:"test" ()
      in
      Alcotest.(check bool) "second redaction returns false" false second)

(** INV-REDACT-3: Redacted memories are excluded from query_scoped_memories. *)
let test_redacted_memories_excluded_from_query () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let scope =
        seed_profiled_room ~db ~room_id:"test-room" ~profile_name:"test-profile"
      in
      let mem_a =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"keep"
          ~content:"keep content" ~provenance:"test" ()
      in
      let _mem_b =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"remove"
          ~content:"remove content" ~provenance:"test" ()
      in
      ignore mem_a;
      let before =
        Memory.query_scoped_memories ~db ~scope_kind:"room"
          ~scope_key:"test-room" ()
      in
      Alcotest.(check int) "2 memories before redaction" 2 (List.length before);
      (* Find and redact "remove" *)
      let remove_mem =
        List.find
          (fun (m : Memory_types.scoped_memory) -> m.reference = "remove")
          before
      in
      let _ =
        Memory.redact_scoped_memory ~db ~id:remove_mem.id ~reason:"test" ()
      in
      let after =
        Memory.query_scoped_memories ~db ~scope_kind:"room"
          ~scope_key:"test-room" ()
      in
      Alcotest.(check int) "1 memory after redaction" 1 (List.length after);
      Alcotest.(check string)
        "remaining memory is 'keep'" "keep" (List.hd after).reference)

(* ── Egress Rule Ordering in Effective Access ───────────────────────────── *)

(** INV-EGR-1 (scope-resolution): Profile egress rules come before scope egress
    rules, and room scope rules come before default scope rules. This is the
    ordering that determines first-match-wins at the egress evaluator level. *)
let test_egress_rule_ordering_profile_before_scope () =
  let json =
    {|{
      "access_bundles": [
        {
          "id": "default-bundle",
          "egress_rules": [
            {"host": "*", "action": "deny", "log_policy": "log"}
          ]
        },
        {
          "id": "profile-bundle",
          "egress_rules": [
            {"host": "api.example.com", "action": "allow", "log_policy": "log"}
          ]
        },
        {
          "id": "room-bundle",
          "egress_rules": [
            {"host": "room.example.com", "action": "allow", "log_policy": "log"}
          ]
        }
      ],
      "access_scopes": [
        {"id": "default", "level": "default", "access_bundle_ids": ["default-bundle"]},
        {"id": "room-scope", "level": "room", "room": "C123", "access_bundle_ids": ["room-bundle"]}
      ],
      "room_profiles": [
        {"id": "prof", "model": "openai:gpt-5.4", "access_bundle_ids": ["profile-bundle"]}
      ],
      "room_profile_bindings": [
        {"profile_id": "prof", "room": "C123", "active": true}
      ]
    }|}
  in
  let cfg = parse json in
  let effective =
    Runtime_config.resolve_effective_access cfg ~session_key:"slack:C123" ()
  in
  let hosts =
    List.map
      (fun (r : Runtime_config.egress_rule) -> r.host)
      effective.egress_rules
  in
  Alcotest.(check (list string))
    "egress ordering: profile, room, default"
    [ "api.example.com"; "room.example.com"; "*" ]
    hosts;
  (* First match wins: api.example.com matches profile allow, not default deny *)
  let eval_result =
    Egress_evaluator.evaluate ~rules:effective.egress_rules
      ~host:"api.example.com" ()
  in
  Alcotest.(check bool)
    "profile allow takes precedence over default deny" true
    (match eval_result.action with Runtime_config.Allow -> true | _ -> false)

(* ── Suite ──────────────────────────────────────────────────────────────── *)

let suite =
  [
    (* Scope resolver conformance *)
    Alcotest.test_case "INV-DET-2: same-level scopes ordered lexicographically"
      `Quick test_same_level_scopes_ordered_lexicographically;
    Alcotest.test_case
      "INV-PREC-3: same-scope bundle declaration order preserved" `Quick
      test_same_scope_bundle_declaration_order_preserved;
    Alcotest.test_case "INV-ACT-1: deleted scopes filtered during resolution"
      `Quick test_deleted_scopes_filtered_during_resolution;
    Alcotest.test_case "INV-ACT-1: deleted scope status is case-insensitive"
      `Quick test_deleted_scopes_case_insensitive;
    Alcotest.test_case "INV-ACT-1: deleted room scope filtered" `Quick
      test_deleted_room_scope_filtered;
    Alcotest.test_case
      "INV-DET-2 + INV-PREC-1: mixed level ordered by rank then lexicographic"
      `Quick test_mixed_level_scopes_ordered_by_rank_then_lexicographic;
    Alcotest.test_case "INV-DET-2: resolver determinism across calls" `Quick
      test_resolver_determinism_across_calls;
    (* Egress evaluator conformance *)
    Alcotest.test_case "INV-EGR-3: egress rules first match wins" `Quick
      test_egress_rules_first_match_wins;
    Alcotest.test_case "INV-EGR-1 + INV-EGR-3: egress rule order matters" `Quick
      test_egress_rules_order_matters;
    Alcotest.test_case "INV-EGR-1: unmatched destinations default to deny"
      `Quick test_egress_unmatched_destinations_default_deny;
    (* Grant resolution conformance *)
    Alcotest.test_case "INV-GRANT-3: expired grants excluded from resolution"
      `Quick test_expired_grants_excluded_from_resolution;
    Alcotest.test_case "INV-GRANT-3: non-expired grants included" `Quick
      test_non_expired_grants_included_in_resolution;
    Alcotest.test_case "INV-GRANT-3: null expires_at grants never expire" `Quick
      test_null_expires_at_grants_never_expire;
    Alcotest.test_case "INV-GRANT-4: revoked grants excluded when column exists"
      `Quick test_revoked_grants_excluded_when_column_exists;
    Alcotest.test_case
      "INV-GRANT-4: non-revoked grants included when column exists" `Quick
      test_non_revoked_grants_included_when_column_exists;
    Alcotest.test_case "INV-GRANT-4: grants included when no revoked_at column"
      `Quick test_grants_included_when_no_revoked_at_column;
    (* Redaction conformance *)
    Alcotest.test_case
      "INV-REDACT-4: redaction sets redacted_at and clears content" `Quick
      test_redaction_sets_redacted_at_and_clears_content;
    Alcotest.test_case "INV-REDACT-4: redaction sets reference when null" `Quick
      test_redaction_sets_reference_when_null;
    Alcotest.test_case "INV-REDACT-1: redaction is idempotent" `Quick
      test_redaction_idempotent;
    Alcotest.test_case "INV-REDACT-3: redacted memories excluded from query"
      `Quick test_redacted_memories_excluded_from_query;
    (* Egress ordering in effective access *)
    Alcotest.test_case "INV-EGR-1: egress rule ordering profile before scope"
      `Quick test_egress_rule_ordering_profile_before_scope;
  ]

(** Tests that memory mutation operations emit ledger events with principal,
    scope, visibility, and sanitized content preview. *)

let contains_substring = Test_helpers.string_contains

let metadata_string key json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with Some (`String s) -> s | _ -> "")
  | _ -> ""

let metadata_int key json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with Some (`Int n) -> n | _ -> -1)
  | _ -> -1

let metadata_member key json =
  match json with `Assoc fields -> List.assoc_opt key fields | _ -> None

let with_db f = Test_helpers.with_memory_store f

let make_ledger db : Memory.ledger_fn =
 fun ~room_id ~event_type ~actor ~metadata ->
  ignore
    (Room_activity_ledger.append_now ~db ~room_id ~event_type ~actor ~metadata)

let setup_room_scope ~db ~room_id =
  Memory.create_scope ~db ~kind:"room" ~key:room_id ~provenance:"test" ()

let query_memory_events ~db ~room_id =
  Room_activity_ledger.query ~db ~room_id ()
  |> List.filter (fun (e : Room_activity_ledger.event) ->
      String.starts_with ~prefix:"memory_" e.event_type
      || String.starts_with ~prefix:"scope_" e.event_type
      || String.starts_with ~prefix:"team_grant_" e.event_type)

(* --- Save events --- *)

let test_save_emits_memory_saved () =
  with_db (fun db ->
      let scope = setup_room_scope ~db ~room_id:"room-1" in
      let ledger = make_ledger db in
      let _m =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"fact-1"
          ~content:"Alice likes cats" ~provenance:"cli" ~visibility:Public
          ~ledger ()
      in
      let events = query_memory_events ~db ~room_id:"room-1" in
      Alcotest.(check int) "one event" 1 (List.length events);
      let ev = List.hd events in
      Alcotest.(check string) "event_type" "memory_saved" ev.event_type;
      Alcotest.(check string) "actor" "cli" ev.actor;
      Alcotest.(check int) "memory_id" 1 (metadata_int "memory_id" ev.metadata);
      Alcotest.(check string)
        "scope_kind" "room"
        (metadata_string "scope_kind" ev.metadata);
      Alcotest.(check string)
        "scope_key" "room-1"
        (metadata_string "scope_key" ev.metadata);
      Alcotest.(check string)
        "principal" "cli"
        (metadata_string "principal" ev.metadata);
      Alcotest.(check string)
        "visibility" "public"
        (metadata_string "visibility" ev.metadata);
      Alcotest.(check string)
        "content_preview" "Alice likes cats"
        (metadata_string "content_preview" ev.metadata))

let test_save_upsert_emits_event () =
  with_db (fun db ->
      let scope = setup_room_scope ~db ~room_id:"room-1" in
      let ledger = make_ledger db in
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"fact-1"
           ~content:"first" ~provenance:"cli" ~ledger ());
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"fact-1"
           ~content:"second" ~provenance:"cli" ~ledger ());
      let events = query_memory_events ~db ~room_id:"room-1" in
      Alcotest.(check int) "two events" 2 (List.length events);
      let previews =
        List.map
          (fun (e : Room_activity_ledger.event) ->
            metadata_string "content_preview" e.metadata)
          events
      in
      Alcotest.(check (list string)) "previews" [ "first"; "second" ] previews)

let test_save_visibility_recorded () =
  with_db (fun db ->
      let scope = setup_room_scope ~db ~room_id:"room-1" in
      let ledger = make_ledger db in
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"secret"
           ~content:"hidden stuff" ~provenance:"agent" ~visibility:Private
           ~ledger ());
      let events = query_memory_events ~db ~room_id:"room-1" in
      let ev = List.hd events in
      Alcotest.(check string)
        "visibility" "private"
        (metadata_string "visibility" ev.metadata))

let test_save_no_ledger_no_event () =
  with_db (fun db ->
      let scope = setup_room_scope ~db ~room_id:"room-1" in
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"fact-1"
           ~content:"no ledger" ~provenance:"cli" ());
      let events = query_memory_events ~db ~room_id:"room-1" in
      Alcotest.(check int) "no events" 0 (List.length events))

(* --- Correct events --- *)

let test_correct_emits_memory_corrected () =
  with_db (fun db ->
      let scope = setup_room_scope ~db ~room_id:"room-1" in
      let ledger = make_ledger db in
      let m =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"fact-1"
          ~content:"old content" ~provenance:"cli" ~ledger ()
      in
      let updated =
        Memory.correct_scoped_memory ~db ~id:m.id ~content:"new content"
          ~provenance:"corrected:cli" ~ledger ()
      in
      Alcotest.(check bool) "corrected" true (updated <> None);
      let events = query_memory_events ~db ~room_id:"room-1" in
      let corrected =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            e.event_type = "memory_corrected")
          events
      in
      Alcotest.(check int) "one corrected event" 1 (List.length corrected);
      let ev = List.hd corrected in
      Alcotest.(check string) "actor" "corrected:cli" ev.actor;
      Alcotest.(check string)
        "content_preview" "new content"
        (metadata_string "content_preview" ev.metadata);
      Alcotest.(check string)
        "visibility" "public"
        (metadata_string "visibility" ev.metadata))

let test_correct_nonexistent_no_event () =
  with_db (fun db ->
      let _scope = setup_room_scope ~db ~room_id:"room-1" in
      let ledger = make_ledger db in
      let result =
        Memory.correct_scoped_memory ~db ~id:999 ~content:"nope" ~ledger ()
      in
      Alcotest.(check bool) "none" true (result = None);
      let events = query_memory_events ~db ~room_id:"room-1" in
      Alcotest.(check int) "no events" 0 (List.length events))

(* --- Forget (redact) events --- *)

let test_forget_emits_memory_forgotten () =
  with_db (fun db ->
      let scope = setup_room_scope ~db ~room_id:"room-1" in
      let ledger = make_ledger db in
      let m =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"fact-1"
          ~content:"forget me" ~provenance:"cli" ~visibility:Private ~ledger ()
      in
      let success =
        Memory.redact_scoped_memory ~db ~id:m.id ~reason:"cleanup" ~ledger ()
      in
      Alcotest.(check bool) "redacted" true success;
      let events = query_memory_events ~db ~room_id:"room-1" in
      let forgotten =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            e.event_type = "memory_forgotten")
          events
      in
      Alcotest.(check int) "one forgotten event" 1 (List.length forgotten);
      let ev = List.hd forgotten in
      Alcotest.(check string) "actor" "user" ev.actor;
      Alcotest.(check int)
        "memory_id" m.id
        (metadata_int "memory_id" ev.metadata);
      Alcotest.(check string)
        "visibility" "private"
        (metadata_string "visibility" ev.metadata);
      Alcotest.(check string)
        "content_preview" "forget me"
        (metadata_string "content_preview" ev.metadata))

let test_forget_nonexistent_no_event () =
  with_db (fun db ->
      let _scope = setup_room_scope ~db ~room_id:"room-1" in
      let ledger = make_ledger db in
      let success =
        Memory.redact_scoped_memory ~db ~id:999 ~reason:"nope" ~ledger ()
      in
      Alcotest.(check bool) "not redacted" false success;
      let events = query_memory_events ~db ~room_id:"room-1" in
      let forgotten =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            e.event_type = "memory_forgotten")
          events
      in
      Alcotest.(check int) "no events" 0 (List.length forgotten))

(* --- Hard delete events --- *)

let test_hard_delete_emits_memory_hard_purged () =
  with_db (fun db ->
      let scope = setup_room_scope ~db ~room_id:"room-1" in
      let ledger = make_ledger db in
      let m =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"fact-1"
          ~content:"purge me" ~provenance:"cli" ~ledger ()
      in
      let success = Memory.delete_scoped_memory ~db ~id:m.id ~ledger () in
      Alcotest.(check bool) "deleted" true success;
      let events = query_memory_events ~db ~room_id:"room-1" in
      let purged =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            e.event_type = "memory_hard_purged")
          events
      in
      Alcotest.(check int) "one purged event" 1 (List.length purged);
      let ev = List.hd purged in
      Alcotest.(check string) "actor" "admin" ev.actor;
      Alcotest.(check int)
        "memory_id" m.id
        (metadata_int "memory_id" ev.metadata))

(* --- Grant events --- *)

let test_grant_emits_scope_granted () =
  with_db (fun db ->
      let scope = setup_room_scope ~db ~room_id:"room-1" in
      let ledger = make_ledger db in
      let result =
        Memory.grant_access ~db ~is_admin:true ~scope_id:scope.id
          ~principal_kind:"agent" ~principal_id:"agent-1" ~capability:"read"
          ~ledger ()
      in
      Alcotest.(check bool) "granted" true (result = Ok ());
      let events = query_memory_events ~db ~room_id:"room-1" in
      let granted =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            e.event_type = "scope_granted")
          events
      in
      Alcotest.(check int) "one granted event" 1 (List.length granted);
      let ev = List.hd granted in
      Alcotest.(check string)
        "principal_kind" "agent"
        (metadata_string "principal_kind" ev.metadata);
      Alcotest.(check string)
        "principal_id" "agent-1"
        (metadata_string "principal_id" ev.metadata);
      Alcotest.(check string)
        "capability" "read"
        (metadata_string "capability" ev.metadata);
      Alcotest.(check string)
        "scope_kind" "room"
        (metadata_string "scope_kind" ev.metadata);
      Alcotest.(check string)
        "scope_key" "room-1"
        (metadata_string "scope_key" ev.metadata))

let test_grant_denied_no_event () =
  with_db (fun db ->
      let _scope = setup_room_scope ~db ~room_id:"room-1" in
      let ledger = make_ledger db in
      let result =
        Memory.grant_access ~db ~is_admin:false ~scope_id:1
          ~principal_kind:"agent" ~principal_id:"agent-1" ~capability:"read"
          ~ledger ()
      in
      Alcotest.(check bool) "denied" true (result <> Ok ());
      let events = query_memory_events ~db ~room_id:"room-1" in
      let granted =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            e.event_type = "scope_granted")
          events
      in
      Alcotest.(check int) "no events" 0 (List.length granted))

let test_revoke_emits_scope_revoked () =
  with_db (fun db ->
      let scope = setup_room_scope ~db ~room_id:"room-1" in
      let ledger = make_ledger db in
      ignore
        (Memory.grant_access ~db ~is_admin:true ~scope_id:scope.id
           ~principal_kind:"agent" ~principal_id:"agent-1" ~capability:"read"
           ~ledger ());
      let result =
        Memory.revoke_access ~db ~is_admin:true ~scope_id:scope.id
          ~principal_kind:"agent" ~principal_id:"agent-1" ~capability:"read"
          ~ledger ()
      in
      Alcotest.(check bool) "revoked" true (result = Ok 1);
      let events = query_memory_events ~db ~room_id:"room-1" in
      let revoked =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            e.event_type = "scope_revoked")
          events
      in
      Alcotest.(check int) "one revoked event" 1 (List.length revoked);
      let ev = List.hd revoked in
      Alcotest.(check string)
        "principal_kind" "agent"
        (metadata_string "principal_kind" ev.metadata);
      Alcotest.(check string)
        "capability" "read"
        (metadata_string "capability" ev.metadata))

let test_revoke_nonexistent_no_event () =
  with_db (fun db ->
      let _scope = setup_room_scope ~db ~room_id:"room-1" in
      let ledger = make_ledger db in
      let result =
        Memory.revoke_access ~db ~is_admin:true ~scope_id:1
          ~principal_kind:"agent" ~principal_id:"agent-1" ~capability:"read"
          ~ledger ()
      in
      Alcotest.(check bool) "zero changes" true (result = Ok 0);
      let events = query_memory_events ~db ~room_id:"room-1" in
      let revoked =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            e.event_type = "scope_revoked")
          events
      in
      Alcotest.(check int) "no events" 0 (List.length revoked))

(* --- Team grant events --- *)

let test_team_grant_emits_event () =
  with_db (fun db ->
      let scope = setup_room_scope ~db ~room_id:"room-1" in
      let ledger = make_ledger db in
      let m =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"shared"
          ~content:"team data" ~provenance:"cli" ~visibility:Team ~ledger ()
      in
      let success =
        Memory.add_team_grant ~db ~memory_id:m.id ~principal_kind:"user"
          ~principal_id:"user-1" ~ledger ()
      in
      Alcotest.(check bool) "granted" true success;
      let events = query_memory_events ~db ~room_id:"room-1" in
      let team_events =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            e.event_type = "team_grant_added")
          events
      in
      Alcotest.(check int) "one team grant event" 1 (List.length team_events);
      let ev = List.hd team_events in
      Alcotest.(check int)
        "memory_id" m.id
        (metadata_int "memory_id" ev.metadata);
      Alcotest.(check string)
        "visibility" "team"
        (metadata_string "visibility" ev.metadata))

let test_team_grant_remove_emits_event () =
  with_db (fun db ->
      let scope = setup_room_scope ~db ~room_id:"room-1" in
      let ledger = make_ledger db in
      let m =
        Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"shared"
          ~content:"team data" ~provenance:"cli" ~visibility:Team ~ledger ()
      in
      ignore
        (Memory.add_team_grant ~db ~memory_id:m.id ~principal_kind:"user"
           ~principal_id:"user-1" ~ledger ());
      let success =
        Memory.remove_team_grant ~db ~memory_id:m.id ~principal_kind:"user"
          ~principal_id:"user-1" ~ledger ()
      in
      Alcotest.(check bool) "removed" true success;
      let events = query_memory_events ~db ~room_id:"room-1" in
      let removed =
        List.filter
          (fun (e : Room_activity_ledger.event) ->
            e.event_type = "team_grant_removed")
          events
      in
      Alcotest.(check int) "one removed event" 1 (List.length removed))

(* --- Content sanitization --- *)

let test_content_preview_sanitizes_bearer_token () =
  with_db (fun db ->
      let scope = setup_room_scope ~db ~room_id:"room-1" in
      let ledger = make_ledger db in
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"cred"
           ~content:"Auth: Bearer sk-secret123456abc" ~provenance:"cli" ~ledger
           ());
      let events = query_memory_events ~db ~room_id:"room-1" in
      let ev = List.hd events in
      let preview = metadata_string "content_preview" ev.metadata in
      Alcotest.(check bool)
        "bearer redacted" true
        (not (contains_substring preview "sk-secret123456abc"));
      Alcotest.(check bool)
        "has REDACTED" true
        (contains_substring preview "[REDACTED]"))

let test_content_preview_truncates_long_content () =
  with_db (fun db ->
      let scope = setup_room_scope ~db ~room_id:"room-1" in
      let ledger = make_ledger db in
      let long_content = String.make 300 'x' in
      ignore
        (Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"long"
           ~content:long_content ~provenance:"cli" ~ledger ());
      let events = query_memory_events ~db ~room_id:"room-1" in
      let ev = List.hd events in
      let preview = metadata_string "content_preview" ev.metadata in
      Alcotest.(check int) "truncated to 203 chars" 203 (String.length preview);
      Alcotest.(check bool)
        "ends with ..." true
        (contains_substring preview "..."))

(* --- Suite --- *)

let suite =
  [
    Alcotest.test_case "save emits memory_saved" `Quick
      test_save_emits_memory_saved;
    Alcotest.test_case "upsert emits event on each call" `Quick
      test_save_upsert_emits_event;
    Alcotest.test_case "save records visibility" `Quick
      test_save_visibility_recorded;
    Alcotest.test_case "no ledger means no event" `Quick
      test_save_no_ledger_no_event;
    Alcotest.test_case "correct emits memory_corrected" `Quick
      test_correct_emits_memory_corrected;
    Alcotest.test_case "correct nonexistent no event" `Quick
      test_correct_nonexistent_no_event;
    Alcotest.test_case "forget emits memory_forgotten" `Quick
      test_forget_emits_memory_forgotten;
    Alcotest.test_case "forget nonexistent no event" `Quick
      test_forget_nonexistent_no_event;
    Alcotest.test_case "hard delete emits memory_hard_purged" `Quick
      test_hard_delete_emits_memory_hard_purged;
    Alcotest.test_case "grant emits scope_granted" `Quick
      test_grant_emits_scope_granted;
    Alcotest.test_case "grant denied no event" `Quick test_grant_denied_no_event;
    Alcotest.test_case "revoke emits scope_revoked" `Quick
      test_revoke_emits_scope_revoked;
    Alcotest.test_case "revoke nonexistent no event" `Quick
      test_revoke_nonexistent_no_event;
    Alcotest.test_case "team grant emits team_grant_added" `Quick
      test_team_grant_emits_event;
    Alcotest.test_case "team grant remove emits team_grant_removed" `Quick
      test_team_grant_remove_emits_event;
    Alcotest.test_case "content preview sanitizes bearer" `Quick
      test_content_preview_sanitizes_bearer_token;
    Alcotest.test_case "content preview truncates long" `Quick
      test_content_preview_truncates_long_content;
  ]

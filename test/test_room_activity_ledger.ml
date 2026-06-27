let with_db f =
  let db = Memory.init ~db_path:":memory:" () in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let test_append_stores_event () =
  with_db (fun db ->
      let metadata = `Assoc [ ("task_id", `String "T001") ] in
      let event =
        Room_activity_ledger.append ~db ~room_id:"room-1"
          ~event_type:"task_started" ~timestamp:"2026-06-27T10:00:00Z"
          ~actor:"agent-1" ~metadata
      in
      Alcotest.(check string) "room_id" "room-1" event.room_id;
      Alcotest.(check string) "event_type" "task_started" event.event_type;
      Alcotest.(check string) "timestamp" "2026-06-27T10:00:00Z" event.timestamp;
      Alcotest.(check string) "actor" "agent-1" event.actor;
      Alcotest.(check bool) "metadata" true (event.metadata = metadata))

let test_append_is_idempotent () =
  with_db (fun db ->
      let first =
        Room_activity_ledger.append ~db ~room_id:"room-1"
          ~event_type:"task_done" ~timestamp:"2026-06-27T10:01:00Z"
          ~actor:"agent-1"
          ~metadata:(`Assoc [ ("attempt", `Int 1) ])
      in
      let second =
        Room_activity_ledger.append ~db ~room_id:"room-1"
          ~event_type:"task_done" ~timestamp:"2026-06-27T10:01:00Z"
          ~actor:"agent-2"
          ~metadata:(`Assoc [ ("attempt", `Int 2) ])
      in
      let events = Room_activity_ledger.query ~db () in
      Alcotest.(check int) "one event" 1 (List.length events);
      Alcotest.(check int) "same id" first.id second.id;
      Alcotest.(check string) "original actor kept" "agent-1" second.actor;
      Alcotest.(check bool)
        "original metadata kept" true
        (second.metadata = `Assoc [ ("attempt", `Int 1) ]))

let test_query_filters () =
  with_db (fun db ->
      let add room event_type timestamp actor =
        ignore
          (Room_activity_ledger.append ~db ~room_id:room ~event_type ~timestamp
             ~actor ~metadata:(`Assoc []))
      in
      add "room-1" "task_started" "2026-06-27T10:00:00Z" "agent-1";
      add "room-1" "task_done" "2026-06-27T10:05:00Z" "agent-1";
      add "room-2" "task_started" "2026-06-27T10:10:00Z" "agent-2";
      add "room-1" "task_started" "2026-06-27T10:15:00Z" "agent-3";
      let room_events = Room_activity_ledger.query ~db ~room_id:"room-1" () in
      Alcotest.(check int) "room filter" 3 (List.length room_events);
      let started =
        Room_activity_ledger.query ~db ~event_type:"task_started" ()
      in
      Alcotest.(check int) "event type filter" 3 (List.length started);
      let ranged =
        Room_activity_ledger.query ~db ~room_id:"room-1"
          ~event_type:"task_started" ~from_timestamp:"2026-06-27T10:01:00Z"
          ~to_timestamp:"2026-06-27T10:20:00Z" ()
      in
      Alcotest.(check int) "combined filters" 1 (List.length ranged);
      match ranged with
      | [ event ] ->
          Alcotest.(check string)
            "filtered timestamp" "2026-06-27T10:15:00Z" event.timestamp
      | _ -> Alcotest.fail "expected one ranged event")

let test_migration_adds_schema () =
  Test_helpers.with_temp_dir (fun dir ->
      let db_path = Filename.concat dir "memory.db" in
      let db = Sqlite3.db_open db_path in
      Memory.exec_exn db
        "CREATE TABLE schema_version (version INTEGER NOT NULL)";
      Memory.exec_exn db "INSERT INTO schema_version (version) VALUES (36)";
      ignore (Sqlite3.db_close db);
      let migrated = Memory.init ~db_path () in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close migrated))
        (fun () ->
          Alcotest.(check int)
            "schema version current" Memory.schema_version
            (Test_helpers.query_single_int migrated
               "SELECT version FROM schema_version");
          ignore
            (Room_activity_ledger.append ~db:migrated ~room_id:"room-1"
               ~event_type:"migrated" ~timestamp:"2026-06-27T10:30:00Z"
               ~actor:"agent" ~metadata:(`Assoc []))))

let suite =
  [
    Alcotest.test_case "append stores event" `Quick test_append_stores_event;
    Alcotest.test_case "append is idempotent" `Quick test_append_is_idempotent;
    Alcotest.test_case "query filters" `Quick test_query_filters;
    Alcotest.test_case "migration adds schema" `Quick test_migration_adds_schema;
  ]

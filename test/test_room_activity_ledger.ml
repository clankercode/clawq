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

let test_retention_deletes_older_entries () =
  with_db (fun db ->
      let add event_type timestamp =
        ignore
          (Room_activity_ledger.append ~db ~room_id:"room-1" ~event_type
             ~timestamp ~actor:"agent" ~metadata:(`Assoc []))
      in
      add "old" "2026-06-20T09:59:59Z";
      add "boundary" "2026-06-20T10:00:00Z";
      add "new" "2026-06-21T10:00:00Z";
      let deleted =
        Room_activity_ledger.delete_before ~db
          ~before_timestamp:"2026-06-20T10:00:00Z"
      in
      Alcotest.(check int) "deleted count" 1 deleted;
      let remaining = Room_activity_ledger.query ~db () in
      Alcotest.(check (list string))
        "remaining events" [ "boundary"; "new" ]
        (List.map (fun e -> e.Room_activity_ledger.event_type) remaining))

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

let metadata_int key json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with Some (`Int n) -> n | _ -> -1)
  | _ -> -1

let metadata_string key json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt key fields with Some (`String s) -> s | _ -> "")
  | _ -> ""

let test_background_task_events_recorded () =
  with_db (fun db ->
      Background_task.init_schema db;
      Test_helpers.with_temp_dir (fun repo_path ->
          let id =
            match
              Background_task.enqueue ~db ~runner:Background_task.Codex
                ~repo_path ~require_git:false ~use_worktree:false
                ~prompt:"implement feature"
                ~session_key:"telegram:room-42:user-1" ~channel:"telegram"
                ~channel_id:"room-42" ~agent_name:"builder" ()
            with
            | Ok id -> id
            | Error msg -> Alcotest.fail msg
          in
          ignore
            (Background_task.set_running ~db ~id ~branch:""
               ~worktree_path:repo_path ~log_path:"/tmp/task.log" ~pid:12345);
          Background_task.finish ~db ~id ~status:Background_task.Succeeded
            ~result_preview:"done";
          let fail_id =
            match
              Background_task.enqueue ~db ~runner:Background_task.Codex
                ~repo_path ~require_git:false ~use_worktree:false
                ~prompt:"fix bug" ~session_key:"telegram:room-42:user-1"
                ~channel:"telegram" ~channel_id:"room-42" ()
            with
            | Ok id -> id
            | Error msg -> Alcotest.fail msg
          in
          Background_task.finish ~db ~id:fail_id ~status:Background_task.Failed
            ~result_preview:"crashed";
          let events = Room_activity_ledger.query ~db ~room_id:"room-42" () in
          let event_types =
            List.map (fun e -> e.Room_activity_ledger.event_type) events
          in
          Alcotest.(check bool)
            "create event" true
            (List.mem "background_task_create" event_types);
          Alcotest.(check bool)
            "start event" true
            (List.mem "background_task_start" event_types);
          Alcotest.(check bool)
            "complete event" true
            (List.mem "background_task_complete" event_types);
          Alcotest.(check bool)
            "fail event" true
            (List.mem "background_task_fail" event_types);
          let create_event =
            List.find
              (fun e ->
                e.Room_activity_ledger.event_type = "background_task_create")
              events
          in
          Alcotest.(check string) "actor" "builder" create_event.actor;
          Alcotest.(check int)
            "task id metadata" id
            (metadata_int "task_id" create_event.metadata)))

let make_openai_test_config () =
  let provider =
    {
      Runtime_config.default_provider_config with
      api_key = "test-key";
      kind = Some "openai";
      default_model = Some "gpt-5.4";
    }
  in
  {
    Runtime_config.default with
    providers = [ ("openai", provider) ];
    prompt = { Runtime_config.default.prompt with dynamic_enabled = false };
    security = { Runtime_config.default.security with tools_enabled = false };
    agent_defaults =
      {
        Runtime_config.default.agent_defaults with
        primary_model = "openai:gpt-5.4";
        show_thinking = false;
        show_tool_calls = false;
      };
  }

let test_provider_events_recorded () =
  with_db (fun db ->
      let prev_complete = !Provider.native_complete in
      Fun.protect
        ~finally:(fun () -> Provider.native_complete := prev_complete)
        (fun () ->
          Provider.register_native_complete Provider.OpenAICompat
            (fun
              ~config:_
              ~provider:_
              ~model
              ~messages:_
              ?tools:_
              ?session_key:_
              ()
            ->
              Lwt.return
                (Provider.Text
                   {
                     content = "ok";
                     model;
                     usage = Some (120, 30, 10);
                     provider_response_items_json = None;
                     thinking = None;
                   }));
          let agent = Agent.create ~config:(make_openai_test_config ()) () in
          let response =
            Lwt_main.run
              (Agent.turn agent ~db ~session_key:"telegram:room-9:user-2"
                 ~user_message:"hello" ())
          in
          Alcotest.(check string) "response" "ok" response;
          let events = Room_activity_ledger.query ~db ~room_id:"room-9" () in
          let event_types =
            List.map (fun e -> e.Room_activity_ledger.event_type) events
          in
          Alcotest.(check (list string))
            "provider event order"
            [ "provider_request"; "provider_response" ]
            event_types;
          let response_event = List.nth events 1 in
          Alcotest.(check string) "provider actor" "openai" response_event.actor;
          Alcotest.(check int)
            "prompt tokens" 120
            (metadata_int "prompt_tokens" response_event.metadata);
          Alcotest.(check int)
            "completion tokens" 30
            (metadata_int "completion_tokens" response_event.metadata);
          Alcotest.(check int)
            "cached tokens" 10
            (metadata_int "cached_tokens" response_event.metadata);
          Alcotest.(check string)
            "model metadata" "gpt-5.4"
            (metadata_string "model" response_event.metadata)))

let suite =
  [
    Alcotest.test_case "append stores event" `Quick test_append_stores_event;
    Alcotest.test_case "append is idempotent" `Quick test_append_is_idempotent;
    Alcotest.test_case "query filters" `Quick test_query_filters;
    Alcotest.test_case "retention deletes older entries" `Quick
      test_retention_deletes_older_entries;
    Alcotest.test_case "migration adds schema" `Quick test_migration_adds_schema;
    Alcotest.test_case "background task events recorded" `Quick
      test_background_task_events_recorded;
    Alcotest.test_case "provider events recorded" `Quick
      test_provider_events_recorded;
  ]

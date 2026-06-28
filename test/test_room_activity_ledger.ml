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

let expect_invalid_argument label f =
  match f () with
  | exception Invalid_argument msg ->
      Alcotest.(check bool) label true (String.trim msg <> "")
  | exception exn ->
      Alcotest.failf "%s: expected Invalid_argument, got %s" label
        (Printexc.to_string exn)
  | _ -> Alcotest.failf "%s: expected Invalid_argument" label

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

let test_delivery_attempt_records_metadata () =
  with_db (fun db ->
      let event =
        Room_activity_ledger.record_delivery_attempt ~db ~room_id:"room-42"
          ~connector:"teams" ~task_id:7 ~thread_id:"t-123" ()
      in
      Alcotest.(check string) "event_type" "delivery_attempt" event.event_type;
      Alcotest.(check string) "actor" "teams" event.actor;
      Alcotest.(check string) "room_id" "room-42" event.room_id;
      Alcotest.(check int) "task_id" 7 (metadata_int "task_id" event.metadata);
      Alcotest.(check string)
        "connector" "teams"
        (metadata_string "connector" event.metadata);
      Alcotest.(check string)
        "thread_id" "t-123"
        (metadata_string "thread_id" event.metadata))

let test_delivery_success_records_message_id () =
  with_db (fun db ->
      let event =
        Room_activity_ledger.record_delivery_success ~db ~room_id:"room-1"
          ~connector:"slack" ~task_id:3 ~message_id:"ts-12345"
          ~thread_id:"thread-1" ()
      in
      Alcotest.(check string) "event_type" "delivery_success" event.event_type;
      Alcotest.(check string) "actor" "slack" event.actor;
      Alcotest.(check int) "task_id" 3 (metadata_int "task_id" event.metadata);
      Alcotest.(check string)
        "message_id" "ts-12345"
        (metadata_string "message_id" event.metadata);
      Alcotest.(check string)
        "thread_id" "thread-1"
        (metadata_string "thread_id" event.metadata))

let contains_substring s sub =
  let s_len = String.length s in
  let sub_len = String.length sub in
  let rec search i =
    if i + sub_len > s_len then false
    else if String.sub s i sub_len = sub then true
    else search (i + 1)
  in
  sub_len = 0 || search 0

let test_delivery_failure_records_sanitized_error () =
  with_db (fun db ->
      let event =
        Room_activity_ledger.record_delivery_failure ~db ~room_id:"room-1"
          ~connector:"discord" ~task_id:5 ~error:"Bearer sk-secret123456 failed"
          ()
      in
      Alcotest.(check string) "event_type" "delivery_failure" event.event_type;
      let err = metadata_string "error" event.metadata in
      Alcotest.(check bool)
        "token redacted" true
        (not (contains_substring err "sk-secret123456"));
      Alcotest.(check bool)
        "has REDACTED" true
        (contains_substring err "[REDACTED]"))

let test_delivery_failure_truncates_long_error () =
  with_db (fun db ->
      let long_err = String.make 600 'x' in
      let event =
        Room_activity_ledger.record_delivery_failure ~db ~room_id:"room-1"
          ~connector:"slack" ~task_id:2 ~error:long_err ()
      in
      let err = metadata_string "error" event.metadata in
      Alcotest.(check int) "truncated length" 503 (String.length err);
      Alcotest.(check string)
        "truncated content"
        (String.make 500 'x' ^ "...")
        err)

let test_delivery_attempt_no_thread () =
  with_db (fun db ->
      let event =
        Room_activity_ledger.record_delivery_attempt ~db ~room_id:"room-1"
          ~connector:"slack" ~task_id:1 ()
      in
      Alcotest.(check string) "event_type" "delivery_attempt" event.event_type;
      Alcotest.(check bool)
        "no thread_id" true
        (match event.metadata with
        | `Assoc fields -> not (List.mem_assoc "thread_id" fields)
        | _ -> false))

let test_delivery_success_rejects_invalid_message_id () =
  with_db (fun db ->
      expect_invalid_argument "empty message_id rejected" (fun () ->
          ignore
            (Room_activity_ledger.record_delivery_success ~db ~room_id:"room-1"
               ~connector:"teams" ~task_id:9 ~message_id:"" ()));
      expect_invalid_argument "placeholder message_id rejected" (fun () ->
          ignore
            (Room_activity_ledger.record_delivery_success ~db ~room_id:"room-1"
               ~connector:"teams" ~task_id:9 ~message_id:"0" ())))

let test_sanitize_error () =
  let open Room_activity_ledger in
  let r1 = sanitize_error "simple error" in
  Alcotest.(check string) "simple" "simple error" r1;
  let r2 = sanitize_error "Auth failed: Bearer abc+def/ghi==" in
  Alcotest.(check string) "bearer redacted" "Auth failed: Bearer [REDACTED]" r2;
  let r3 = sanitize_error "connection failed token=abc123def456 more info" in
  Alcotest.(check bool)
    "token redacted" true
    (not (contains_substring r3 "abc123def456"));
  let r4 = sanitize_error "connection failed key=abc+def/ghi== more info" in
  Alcotest.(check bool)
    "key redacted" true
    (not (contains_substring r4 "abc+def/ghi=="))

let make_room_progress_task ?(id = 42) ?(connector = "slack")
    ?(room_id = "room-42") ?thread_id () =
  let origin_fields =
    [ ("room_id", `String room_id); ("connector", `String connector) ]
  in
  let origin_fields =
    match thread_id with
    | Some tid -> ("thread_id", `String tid) :: origin_fields
    | None -> origin_fields
  in
  {
    Background_task_0_format.id;
    runner = Background_task_0_format.Codex;
    model = None;
    repo_path = "/tmp";
    prompt = "test task";
    branch = "main";
    worktree_path = None;
    log_path = None;
    status = Background_task_0_format.Running;
    runner_session_id = None;
    session_key = Some (Printf.sprintf "%s:%s:user-1" connector room_id);
    channel = Some connector;
    channel_id = Some room_id;
    pid = None;
    result_preview = None;
    created_at = "2026-06-29T00:00:00Z";
    started_at = None;
    finished_at = None;
    automerge = false;
    use_worktree = false;
    merge_status = None;
    retry_count = 0;
    parent_task_id = None;
    replaced_by = None;
    acp = false;
    agent_name = None;
    notification_status = None;
    notification_error = None;
    notification_attempts = 0;
    follow_up_prompt = None;
    description = None;
    context_snapshot = None;
    profile_id = None;
    origin_json = Some (Yojson.Safe.to_string (`Assoc origin_fields));
    thread_id = None;
    requester = None;
    progress_state = None;
    access_snapshot_id = None;
  }

let test_room_progress_records_delivery () =
  (* Integration test: deliver_progress_update records attempts and outcomes *)
  with_db (fun db ->
      let send_called = ref false in
      let send ~room_id ?thread_id:_ ~text:_ () =
        send_called := true;
        Lwt.return "msg-id-123"
      in
      let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
      let task =
        {
          Background_task_0_format.id = 42;
          runner = Background_task_0_format.Codex;
          model = None;
          repo_path = "/tmp";
          prompt = "test task";
          branch = "main";
          worktree_path = None;
          log_path = None;
          status = Background_task_0_format.Running;
          runner_session_id = None;
          session_key = Some "telegram:room-42:user-1";
          channel = Some "telegram";
          channel_id = Some "room-42";
          pid = None;
          result_preview = None;
          created_at = "2026-06-29T00:00:00Z";
          started_at = None;
          finished_at = None;
          automerge = false;
          use_worktree = false;
          merge_status = None;
          retry_count = 0;
          parent_task_id = None;
          replaced_by = None;
          acp = false;
          agent_name = None;
          notification_status = None;
          notification_error = None;
          notification_attempts = 0;
          follow_up_prompt = None;
          description = None;
          context_snapshot = None;
          profile_id = None;
          origin_json =
            Some
              (Yojson.Safe.to_string
                 (`Assoc
                    [
                      ("room_id", `String "room-42");
                      ("connector", `String "slack");
                    ]));
          thread_id = None;
          requester = None;
          progress_state = None;
    access_snapshot_id = None;
        }
      in
      Lwt_main.run
        (Room_progress.deliver_progress_update ~send ~edit ~db ~task ());
      Alcotest.(check bool) "send called" true !send_called;
      let events = Room_activity_ledger.query ~db ~room_id:"room-42" () in
      let event_types =
        List.map (fun e -> e.Room_activity_ledger.event_type) events
      in
      Alcotest.(check bool)
        "attempt recorded" true
        (List.mem "delivery_attempt" event_types);
      Alcotest.(check bool)
        "success recorded" true
        (List.mem "delivery_success" event_types);
      let success_event =
        List.find
          (fun e -> e.Room_activity_ledger.event_type = "delivery_success")
          events
      in
      Alcotest.(check string)
        "connector" "slack"
        (metadata_string "connector" success_event.metadata);
      Alcotest.(check int)
        "task_id" 42
        (metadata_int "task_id" success_event.metadata);
      Alcotest.(check string)
        "message_id" "msg-id-123"
        (metadata_string "message_id" success_event.metadata);
      Alcotest.(check string)
        "activity_id" "msg-id-123"
        (metadata_string "activity_id" success_event.metadata))

let test_room_progress_zero_send_records_failure () =
  (* Placeholder Teams activity IDs must not be logged as success. *)
  with_db (fun db ->
      let send ~room_id:_ ?thread_id:_ ~text:_ () = Lwt.return "0" in
      let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
      let task =
        make_room_progress_task ~id:100 ~connector:"teams" ~room_id:"room-100"
          ()
      in
      Lwt_main.run
        (Room_progress.deliver_progress_update ~send ~edit ~db ~task ());
      let events = Room_activity_ledger.query ~db ~room_id:"room-100" () in
      let event_types =
        List.map (fun e -> e.Room_activity_ledger.event_type) events
      in
      Alcotest.(check bool)
        "attempt recorded" true
        (List.mem "delivery_attempt" event_types);
      Alcotest.(check bool)
        "failure recorded" true
        (List.mem "delivery_failure" event_types);
      Alcotest.(check bool)
        "success NOT recorded" false
        (List.mem "delivery_success" event_types))

let test_room_progress_raised_send_records_failure () =
  with_db (fun db ->
      let send ~room_id:_ ?thread_id:_ ~text:_ () =
        Lwt.fail (Failure "connector raised key=secret123")
      in
      let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
      let task =
        make_room_progress_task ~id:101 ~connector:"teams" ~room_id:"room-101"
          ()
      in
      Lwt_main.run
        (Room_progress.deliver_progress_update ~send ~edit ~db ~task ());
      let events = Room_activity_ledger.query ~db ~room_id:"room-101" () in
      let event_types =
        List.map (fun e -> e.Room_activity_ledger.event_type) events
      in
      Alcotest.(check bool)
        "attempt recorded" true
        (List.mem "delivery_attempt" event_types);
      Alcotest.(check bool)
        "failure recorded" true
        (List.mem "delivery_failure" event_types);
      Alcotest.(check bool)
        "success NOT recorded" false
        (List.mem "delivery_success" event_types);
      let failure_event =
        List.find
          (fun e -> e.Room_activity_ledger.event_type = "delivery_failure")
          events
      in
      let err = metadata_string "error" failure_event.metadata in
      Alcotest.(check bool) "error present" true (err <> "");
      Alcotest.(check bool)
        "key sanitized" true
        (not (contains_substring err "secret123")))

let test_room_progress_empty_send_records_failure () =
  (* Empty/failed sends must not be logged as success *)
  with_db (fun db ->
      let send ~room_id ?thread_id:_ ~text:_ () =
        let _ = room_id in
        Lwt.return ""
      in
      let edit ~room_id:_ ~msg_id:_ ~text:_ = Lwt.return_unit in
      let task =
        {
          Background_task_0_format.id = 99;
          runner = Background_task_0_format.Codex;
          model = None;
          repo_path = "/tmp";
          prompt = "test task";
          branch = "main";
          worktree_path = None;
          log_path = None;
          status = Background_task_0_format.Running;
          runner_session_id = None;
          session_key = Some "teams:room-99:user-1";
          channel = Some "teams";
          channel_id = Some "room-99";
          pid = None;
          result_preview = None;
          created_at = "2026-06-29T00:00:00Z";
          started_at = None;
          finished_at = None;
          automerge = false;
          use_worktree = false;
          merge_status = None;
          retry_count = 0;
          parent_task_id = None;
          replaced_by = None;
          acp = false;
          agent_name = None;
          notification_status = None;
          notification_error = None;
          notification_attempts = 0;
          follow_up_prompt = None;
          description = None;
          context_snapshot = None;
          profile_id = None;
          origin_json =
            Some
              (Yojson.Safe.to_string
                 (`Assoc
                    [
                      ("room_id", `String "room-99");
                      ("connector", `String "teams");
                    ]));
          thread_id = None;
          requester = None;
          progress_state = None;
    access_snapshot_id = None;
        }
      in
      Lwt_main.run
        (Room_progress.deliver_progress_update ~send ~edit ~db ~task ());
      let events = Room_activity_ledger.query ~db ~room_id:"room-99" () in
      let event_types =
        List.map (fun e -> e.Room_activity_ledger.event_type) events
      in
      Alcotest.(check bool)
        "attempt recorded" true
        (List.mem "delivery_attempt" event_types);
      Alcotest.(check bool)
        "failure recorded" true
        (List.mem "delivery_failure" event_types);
      Alcotest.(check bool)
        "success NOT recorded" false
        (List.mem "delivery_success" event_types);
      let failure_event =
        List.find
          (fun e -> e.Room_activity_ledger.event_type = "delivery_failure")
          events
      in
      Alcotest.(check string)
        "connector" "teams"
        (metadata_string "connector" failure_event.metadata);
      Alcotest.(check bool)
        "error present" true
        (metadata_string "error" failure_event.metadata <> ""))

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
    Alcotest.test_case "delivery attempt records metadata" `Quick
      test_delivery_attempt_records_metadata;
    Alcotest.test_case "delivery success records message_id" `Quick
      test_delivery_success_records_message_id;
    Alcotest.test_case "delivery failure records sanitized error" `Quick
      test_delivery_failure_records_sanitized_error;
    Alcotest.test_case "delivery failure truncates long error" `Quick
      test_delivery_failure_truncates_long_error;
    Alcotest.test_case "delivery attempt no thread" `Quick
      test_delivery_attempt_no_thread;
    Alcotest.test_case "delivery success rejects invalid message_id" `Quick
      test_delivery_success_rejects_invalid_message_id;
    Alcotest.test_case "sanitize error" `Quick test_sanitize_error;
    Alcotest.test_case "room_progress records delivery" `Quick
      test_room_progress_records_delivery;
    Alcotest.test_case "zero send records failure not success" `Quick
      test_room_progress_zero_send_records_failure;
    Alcotest.test_case "raised send records failure" `Quick
      test_room_progress_raised_send_records_failure;
    Alcotest.test_case "empty send records failure not success" `Quick
      test_room_progress_empty_send_records_failure;
  ]

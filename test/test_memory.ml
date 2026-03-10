(* Tests for Memory module *)

let mk_msg role content = Provider.make_message ~role ~content

let with_temp_db f =
  let path = Filename.temp_file "clawq_memory" ".sqlite3" in
  Fun.protect
    (fun () -> f path)
    ~finally:(fun () -> try Sys.remove path with _ -> ())

let query_single_int db sql =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0)
      | _ -> 0)

let query_single_text_option db sql =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None)
      | _ -> None)

let table_exists db table_name =
  let stmt =
    Sqlite3.prepare db
      "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT table_name));
      Sqlite3.step stmt = Sqlite3.Rc.ROW)

let exec_exn db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      Alcotest.failf "SQLite error %s for sql: %s" (Sqlite3.Rc.to_string rc) sql

(* --- Init tests --- *)

let test_init_sets_busy_timeout () =
  let db = Memory.init ~db_path:":memory:" () in
  let stmt = Sqlite3.prepare db "PRAGMA busy_timeout" in
  let value =
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match Sqlite3.column stmt 0 with
            | Sqlite3.Data.INT n -> Int64.to_int n
            | _ -> -1)
        | _ -> -1)
  in
  Alcotest.(check int) "busy_timeout" 5000 value

let test_init_creates_db () =
  let db = Memory.init ~db_path:":memory:" () in
  (* If we get here without exception, db was created *)
  ignore db

let test_init_search_enabled () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  ignore db

let test_init_search_disabled () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
  ignore db

let test_init_double_call () =
  (* Second init on same path should not fail for :memory: (separate db) *)
  let db1 = Memory.init ~db_path:":memory:" () in
  let db2 = Memory.init ~db_path:":memory:" () in
  ignore db1;
  ignore db2

let test_init_schema_version_is_6 () =
  let db = Memory.init ~db_path:":memory:" () in
  Alcotest.(check int)
    "schema version is 7" 7
    (query_single_int db "SELECT version FROM schema_version")

let test_init_creates_session_persistence_tables () =
  let db = Memory.init ~db_path:":memory:" () in
  Alcotest.(check bool)
    "session_state exists" true
    (table_exists db "session_state");
  Alcotest.(check bool)
    "session_workspace_state exists" true
    (table_exists db "session_workspace_state");
  Alcotest.(check bool)
    "discord_resume_state exists" true
    (table_exists db "discord_resume_state");
  Alcotest.(check bool)
    "session_log_epochs exists" true
    (table_exists db "session_log_epochs");
  Alcotest.(check bool)
    "session_log_epoch_messages exists" true
    (table_exists db "session_log_epoch_messages");
  Alcotest.(check bool)
    "inbound_queue exists" true
    (table_exists db "inbound_queue");
  Alcotest.(check bool)
    "models_cache exists" true
    (table_exists db "models_cache");
  Alcotest.(check bool)
    "request_stats exists" true
    (table_exists db "request_stats")

let test_migrates_v1_db_to_v4_without_data_loss () =
  with_temp_db (fun db_path ->
      let db = Sqlite3.db_open db_path in
      exec_exn db "CREATE TABLE schema_version (version INTEGER NOT NULL)";
      exec_exn db "INSERT INTO schema_version (version) VALUES (1)";
      exec_exn db
        {|CREATE TABLE messages (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  session_key TEXT NOT NULL,
  role TEXT NOT NULL,
  content TEXT NOT NULL,
  tool_call_id TEXT,
  tool_name TEXT,
  tool_calls_json TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
)|};
      exec_exn db
        "INSERT INTO messages (session_key, role, content) VALUES ('legacy', \
         'user', 'hello from v1')";
      ignore (Sqlite3.db_close db);
      let migrated = Memory.init ~db_path () in
      Alcotest.(check int)
        "schema version migrated" 7
        (query_single_int migrated "SELECT version FROM schema_version");
      Alcotest.(check bool)
        "session_state exists after migration" true
        (table_exists migrated "session_state");
      Alcotest.(check bool)
        "session_workspace_state exists after migration" true
        (table_exists migrated "session_workspace_state");
      Alcotest.(check bool)
        "discord_resume_state exists after migration" true
        (table_exists migrated "discord_resume_state");
      Alcotest.(check bool)
        "session_log_epochs exists after migration" true
        (table_exists migrated "session_log_epochs");
      Alcotest.(check bool)
        "session_log_epoch_messages exists after migration" true
        (table_exists migrated "session_log_epoch_messages");
      Alcotest.(check bool)
        "inbound_queue exists after migration" true
        (table_exists migrated "inbound_queue");
      Alcotest.(check bool)
        "models_cache exists after migration" true
        (table_exists migrated "models_cache");
      Alcotest.(check bool)
        "request_stats exists after migration" true
        (table_exists migrated "request_stats");
      let msgs = Memory.load_history ~db:migrated ~session_key:"legacy" in
      Alcotest.(check int) "legacy row preserved" 1 (List.length msgs);
      Alcotest.(check string)
        "legacy content preserved" "hello from v1" (List.hd msgs).content)

let test_replace_session_messages_archives_previous_epoch () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "hello");
  Memory.store_message ~db ~session_key:"s1" (mk_msg "assistant" "hi");
  Memory.replace_session_messages ~db ~session_key:"s1"
    [ mk_msg "assistant" "[Conversation history compacted]" ];
  let epochs = Memory.list_session_epochs ~db ~session_key:"s1" in
  let archived =
    List.filter (fun (epoch : Memory.session_epoch) -> not epoch.current) epochs
  in
  Alcotest.(check int) "one archived epoch" 1 (List.length archived);
  match archived with
  | [ epoch ] -> (
      Alcotest.(check int) "archived message count" 2 epoch.message_count;
      let rows =
        Memory.load_epoch_messages ~db ~session_key:"s1"
          ~epoch:(Memory.Archived (Option.get epoch.epoch_id))
      in
      match rows with
      | Some rows ->
          Alcotest.(check int) "archived rows available" 2 (List.length rows);
          Alcotest.(check string)
            "first archived content" "hello" (List.hd rows).content
      | None -> Alcotest.fail "expected archived epoch rows")
  | _ -> Alcotest.fail "expected exactly one archived epoch"

let test_clear_session_removes_archived_epochs () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "hello");
  Memory.replace_session_messages ~db ~session_key:"s1"
    [ mk_msg "assistant" "summary" ];
  Memory.clear_session ~db ~session_key:"s1";
  let epochs = Memory.list_session_epochs ~db ~session_key:"s1" in
  Alcotest.(check int) "only empty current epoch remains" 1 (List.length epochs);
  Alcotest.(check int) "current epoch empty" 0 (List.hd epochs).message_count

let test_upsert_session_state_roundtrip () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.upsert_session_state ~db ~session_key:"telegram:42:user1" ~turn:"agent"
    ~channel:"telegram" ~channel_id:"42" ();
  Alcotest.(check (option string))
    "turn stored" (Some "agent")
    (query_single_text_option db
       "SELECT turn FROM session_state WHERE session_key = 'telegram:42:user1'");
  Alcotest.(check (option string))
    "channel stored" (Some "telegram")
    (query_single_text_option db
       "SELECT channel FROM session_state WHERE session_key = \
        'telegram:42:user1'");
  Alcotest.(check (option string))
    "channel id stored" (Some "42")
    (query_single_text_option db
       "SELECT channel_id FROM session_state WHERE session_key = \
        'telegram:42:user1'")

let test_mark_response_sent_updates_state () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.upsert_session_state ~db ~session_key:"discord:chan:user" ~turn:"agent"
    ~channel:"discord" ~channel_id:"chan" ();
  Memory.mark_response_sent ~db ~session_key:"discord:chan:user";
  Alcotest.(check (option string))
    "turn reset to user" (Some "user")
    (query_single_text_option db
       "SELECT turn FROM session_state WHERE session_key = 'discord:chan:user'");
  Alcotest.(check bool)
    "response_sent_at set" true
    (query_single_text_option db
       "SELECT response_sent_at FROM session_state WHERE session_key = \
        'discord:chan:user'"
    <> None)

let test_load_pending_agent_sessions_filters_rows () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.upsert_session_state ~db ~session_key:"slack:c1:u1" ~turn:"agent"
    ~channel:"slack" ~channel_id:"c1" ();
  Memory.upsert_session_state ~db ~session_key:"slack:c2:u2" ~turn:"agent"
    ~channel:"slack" ~channel_id:"c2" ~response_sent_at:"2026-03-07 00:00:00" ();
  Memory.upsert_session_state ~db ~session_key:"slack:c3:u3" ~turn:"user"
    ~channel:"slack" ~channel_id:"c3" ();
  let pending = Memory.load_pending_agent_sessions ~db ~max_age_seconds:3600 in
  Alcotest.(check int) "one pending row" 1 (List.length pending);
  Alcotest.(check (triple string (option string) (option string)))
    "pending row contents"
    ("slack:c1:u1", Some "slack", Some "c1")
    (List.hd pending)

(* --- store_message tests --- *)

let test_store_message_inserts () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "hello");
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  Alcotest.(check int) "1 message stored" 1 (List.length msgs)

let test_store_message_role_preserved () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "assistant" "hi there");
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  let m = List.hd msgs in
  Alcotest.(check string) "role is assistant" "assistant" m.role

let test_store_message_content_preserved () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "specific content");
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  let m = List.hd msgs in
  Alcotest.(check string) "content preserved" "specific content" m.content

let test_store_multiple_messages () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "msg1");
  Memory.store_message ~db ~session_key:"s1" (mk_msg "assistant" "msg2");
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "msg3");
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  Alcotest.(check int) "3 messages" 3 (List.length msgs)

let test_store_message_with_tool_call_id () =
  let db = Memory.init ~db_path:":memory:" () in
  let msg =
    {
      Provider.role = "tool";
      content = "result";
      content_parts = [];
      tool_calls = [];
      tool_call_id = Some "tcid-123";
      name = Some "file_read";
      provider_response_items_json = None;
    }
  in
  Memory.store_message ~db ~session_key:"s1" msg;
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  let m = List.hd msgs in
  Alcotest.(check (option string))
    "tool_call_id preserved" (Some "tcid-123") m.tool_call_id

let test_store_message_with_tool_calls () =
  let db = Memory.init ~db_path:":memory:" () in
  let tc =
    { Provider.id = "call-1"; function_name = "shell_exec"; arguments = "{}" }
  in
  let msg =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls = [ tc ];
      tool_call_id = None;
      name = None;
      provider_response_items_json = None;
    }
  in
  Memory.store_message ~db ~session_key:"s1" msg;
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  let m = List.hd msgs in
  Alcotest.(check int) "1 tool call" 1 (List.length m.tool_calls);
  let got_tc = List.hd m.tool_calls in
  Alcotest.(check string) "tool call id" "call-1" got_tc.id;
  Alcotest.(check string) "function name" "shell_exec" got_tc.function_name

(* --- load_history tests --- *)

let test_load_history_order () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "first");
  Memory.store_message ~db ~session_key:"s1" (mk_msg "assistant" "second");
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "third");
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  let contents = List.map (fun (m : Provider.message) -> m.content) msgs in
  Alcotest.(check (list string))
    "messages in order"
    [ "first"; "second"; "third" ]
    contents

let test_load_history_empty_session () =
  let db = Memory.init ~db_path:":memory:" () in
  let msgs = Memory.load_history ~db ~session_key:"nonexistent" in
  Alcotest.(check int) "empty history" 0 (List.length msgs)

let test_load_history_session_isolation () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "session1-msg");
  Memory.store_message ~db ~session_key:"s2" (mk_msg "user" "session2-msg");
  let s1 = Memory.load_history ~db ~session_key:"s1" in
  let s2 = Memory.load_history ~db ~session_key:"s2" in
  Alcotest.(check int) "s1 has 1 message" 1 (List.length s1);
  Alcotest.(check int) "s2 has 1 message" 1 (List.length s2);
  Alcotest.(check string) "s1 content" "session1-msg" (List.hd s1).content;
  Alcotest.(check string) "s2 content" "session2-msg" (List.hd s2).content

(* --- clear_session tests --- *)

let test_clear_session () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "to delete");
  Memory.store_message ~db ~session_key:"s1" (mk_msg "assistant" "also delete");
  Memory.clear_session ~db ~session_key:"s1";
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  Alcotest.(check int) "session cleared" 0 (List.length msgs)

let test_clear_session_isolates_others () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "s1");
  Memory.store_message ~db ~session_key:"s2" (mk_msg "user" "s2");
  Memory.clear_session ~db ~session_key:"s1";
  let s2 = Memory.load_history ~db ~session_key:"s2" in
  Alcotest.(check int) "s2 unaffected" 1 (List.length s2)

(* --- list_sessions tests --- *)

let test_list_sessions_empty () =
  let db = Memory.init ~db_path:":memory:" () in
  let sessions = Memory.list_sessions ~db in
  Alcotest.(check int) "no sessions" 0 (List.length sessions)

let test_list_sessions_single () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"mysession" (mk_msg "user" "hi");
  let sessions = Memory.list_sessions ~db in
  Alcotest.(check int) "1 session" 1 (List.length sessions);
  Alcotest.(check string) "session key" "mysession" (List.hd sessions)

let test_list_sessions_multiple () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"a" (mk_msg "user" "1");
  Memory.store_message ~db ~session_key:"b" (mk_msg "user" "2");
  Memory.store_message ~db ~session_key:"c" (mk_msg "user" "3");
  let sessions = Memory.list_sessions ~db in
  Alcotest.(check int) "3 sessions" 3 (List.length sessions)

let test_list_sessions_deduplicates () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"same" (mk_msg "user" "1");
  Memory.store_message ~db ~session_key:"same" (mk_msg "assistant" "2");
  Memory.store_message ~db ~session_key:"same" (mk_msg "user" "3");
  let sessions = Memory.list_sessions ~db in
  Alcotest.(check int) "deduplicated to 1" 1 (List.length sessions)

(* --- cleanup_session tests --- *)

let test_cleanup_session_max_messages () =
  let db = Memory.init ~db_path:":memory:" () in
  for i = 1 to 10 do
    Memory.store_message ~db ~session_key:"s1"
      (mk_msg "user" (Printf.sprintf "msg%d" i))
  done;
  Memory.cleanup_session ~db ~session_key:"s1" ~max_messages:3 ~max_age_days:0;
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  Alcotest.(check bool) "at most 3 messages kept" true (List.length msgs <= 3)

let test_cleanup_session_max_messages_keeps_newest () =
  let db = Memory.init ~db_path:":memory:" () in
  for i = 1 to 5 do
    Memory.store_message ~db ~session_key:"s1"
      (mk_msg "user" (Printf.sprintf "msg%d" i))
  done;
  Memory.cleanup_session ~db ~session_key:"s1" ~max_messages:2 ~max_age_days:0;
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  (* Should keep the last 2: msg4, msg5 *)
  Alcotest.(check bool)
    "keeps 2 newest" true
    (List.for_all
       (fun (m : Provider.message) -> m.content = "msg4" || m.content = "msg5")
       msgs)

let test_cleanup_session_zero_max_messages_noop () =
  let db = Memory.init ~db_path:":memory:" () in
  for i = 1 to 5 do
    Memory.store_message ~db ~session_key:"s1"
      (mk_msg "user" (Printf.sprintf "msg%d" i))
  done;
  Memory.cleanup_session ~db ~session_key:"s1" ~max_messages:0 ~max_age_days:0;
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  Alcotest.(check int) "all messages preserved when max=0" 5 (List.length msgs)

(* --- search tests (search_enabled=true) --- *)

let test_search_finds_matching_content () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Memory.store_message ~db ~session_key:"s1"
    (mk_msg "user" "OCaml is a functional language");
  Memory.store_message ~db ~session_key:"s1"
    (mk_msg "assistant" "Python is dynamic");
  let results = Memory.search ~db ~query:"OCaml" ~limit:5 () in
  Alcotest.(check bool) "found OCaml" true (List.length results > 0)

let test_search_excludes_non_matching () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "hello world");
  let results = Memory.search ~db ~query:"xyznonexistent12345" ~limit:5 () in
  Alcotest.(check int) "no results for missing query" 0 (List.length results)

let test_search_respects_limit () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  for i = 1 to 10 do
    Memory.store_message ~db ~session_key:"s1"
      (mk_msg "user" (Printf.sprintf "topic number %d" i))
  done;
  let results = Memory.search ~db ~query:"topic" ~limit:3 () in
  Alcotest.(check bool) "at most 3" true (List.length results <= 3)

let test_search_session_filter () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "OCaml rocks");
  Memory.store_message ~db ~session_key:"s2" (mk_msg "user" "OCaml is great");
  let results =
    Memory.search ~db ~query:"OCaml" ~session_key:"s1" ~limit:5 ()
  in
  Alcotest.(check int) "only s1 result" 1 (List.length results)

let test_search_empty_db () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  let results = Memory.search ~db ~query:"anything" ~limit:5 () in
  Alcotest.(check int) "empty db no results" 0 (List.length results)

(* --- cleanup_all tests --- *)

let test_cleanup_all_multiple_sessions () =
  let db = Memory.init ~db_path:":memory:" () in
  for i = 1 to 8 do
    Memory.store_message ~db ~session_key:"a"
      (mk_msg "user" (Printf.sprintf "msg%d" i))
  done;
  for i = 1 to 6 do
    Memory.store_message ~db ~session_key:"b"
      (mk_msg "user" (Printf.sprintf "msg%d" i))
  done;
  Memory.cleanup_all ~db ~max_messages:3 ~max_age_days:0;
  let a_msgs = Memory.load_history ~db ~session_key:"a" in
  let b_msgs = Memory.load_history ~db ~session_key:"b" in
  Alcotest.(check bool) "a at most 3" true (List.length a_msgs <= 3);
  Alcotest.(check bool) "b at most 3" true (List.length b_msgs <= 3)

let test_tool_result_roundtrip () =
  let db = Memory.init ~db_path:":memory:" () in
  let msg =
    Provider.make_tool_result ~tool_call_id:"tc-99" ~name:"bash_exec"
      ~content:"done"
  in
  Memory.store_message ~db ~session_key:"s1" msg;
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  let m = List.hd msgs in
  Alcotest.(check string) "role is tool" "tool" m.role;
  Alcotest.(check (option string)) "tool_call_id" (Some "tc-99") m.tool_call_id;
  Alcotest.(check string) "content" "done" m.content

let test_tool_cycle_history_shape () =
  let db = Memory.init ~db_path:":memory:" () in
  let tc1 =
    { Provider.id = "call-1"; function_name = "shell_exec"; arguments = "{}" }
  in
  let tc2 =
    { Provider.id = "call-2"; function_name = "file_read"; arguments = "{}" }
  in
  let assistant_with_calls =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls = [ tc1; tc2 ];
      tool_call_id = None;
      name = None;
      provider_response_items_json = None;
    }
  in
  Memory.store_message ~db ~session_key:"s1" assistant_with_calls;
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_tool_result ~tool_call_id:"call-1" ~name:"shell_exec"
       ~content:"ok-1");
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_tool_result ~tool_call_id:"call-2" ~name:"file_read"
       ~content:"ok-2");
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  Alcotest.(check int) "three messages in cycle" 3 (List.length msgs);
  let first = List.nth msgs 0 in
  let second = List.nth msgs 1 in
  let third = List.nth msgs 2 in
  Alcotest.(check string)
    "assistant call shell first" "shell_exec"
    (List.nth first.tool_calls 0).function_name;
  Alcotest.(check string)
    "assistant call file second" "file_read"
    (List.nth first.tool_calls 1).function_name;
  Alcotest.(check (option string))
    "tool result #1 id" (Some "call-1") second.tool_call_id;
  Alcotest.(check (option string))
    "tool result #2 id" (Some "call-2") third.tool_call_id

let test_provider_response_items_roundtrip () =
  let db = Memory.init ~db_path:":memory:" () in
  let msg =
    Provider.make_message_full
      ~provider_response_items_json:
        (Some
           {|[{"type":"reasoning","id":"rs_1"},{"type":"function_call","call_id":"call_1","name":"bash","arguments":"{}"}]|})
      ~role:"assistant" ~content:""
  in
  Memory.store_message ~db ~session_key:"s1" msg;
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  let loaded = List.hd msgs in
  Alcotest.(check (option string))
    "provider response items preserved" msg.provider_response_items_json
    loaded.provider_response_items_json

(* --- inbound queue tests --- *)

let test_queue_init_creates_table () =
  let db = Memory.init ~db_path:":memory:" () in
  Alcotest.(check bool)
    "inbound_queue table exists" true
    (table_exists db "inbound_queue")

let test_queue_enqueue_and_list () =
  let db = Memory.init ~db_path:":memory:" () in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"message":"hello"}|}
  in
  let rows = Memory.queue_list ~db ~session_key:"s1" in
  Alcotest.(check int) "one queued row" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.(check string) "session_key" "s1" row.session_key;
  Alcotest.(check string) "source" "cli" row.source;
  Alcotest.(check string) "payload" {|{"message":"hello"}|} row.payload_json;
  Alcotest.(check int) "attempt_count" 0 row.attempt_count;
  Alcotest.(check (option string)) "last_error" None row.last_error

let test_queue_enqueue_fifo_ordering () =
  let db = Memory.init ~db_path:":memory:" () in
  let _id1 =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"n":1}|}
  in
  let _id2 =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"n":2}|}
  in
  let _id3 =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"n":3}|}
  in
  let rows = Memory.queue_list ~db ~session_key:"s1" in
  let payloads = List.map (fun (r : Memory.queue_row) -> r.payload_json) rows in
  Alcotest.(check (list string))
    "FIFO order"
    [ {|{"n":1}|}; {|{"n":2}|}; {|{"n":3}|} ]
    payloads

let test_queue_claim_exclusive () =
  let db = Memory.init ~db_path:":memory:" () in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"message":"claim me"}|}
  in
  let result1 = Memory.queue_claim ~db ~session_key:"s1" in
  (match result1 with
  | Memory.Claim_ok row ->
      Alcotest.(check string)
        "claimed payload" {|{"message":"claim me"}|} row.payload_json
  | Memory.Claim_empty -> Alcotest.fail "expected claim to succeed");
  let result2 = Memory.queue_claim ~db ~session_key:"s1" in
  match result2 with
  | Memory.Claim_empty -> ()
  | Memory.Claim_ok _ -> Alcotest.fail "expected second claim to fail"

let test_queue_claim_empty () =
  let db = Memory.init ~db_path:":memory:" () in
  let result = Memory.queue_claim ~db ~session_key:"s1" in
  match result with
  | Memory.Claim_empty -> ()
  | Memory.Claim_ok _ -> Alcotest.fail "expected claim on empty queue to fail"

let test_queue_release_makes_reclaimable () =
  let db = Memory.init ~db_path:":memory:" () in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"msg":"test"}|}
  in
  let row =
    match Memory.queue_claim ~db ~session_key:"s1" with
    | Memory.Claim_ok r -> r
    | Memory.Claim_empty -> Alcotest.failf "expected claim"
  in
  let released = Memory.queue_release ~db ~queue_id:row.queue_id in
  Alcotest.(check bool) "release succeeded" true released;
  let reclaim = Memory.queue_claim ~db ~session_key:"s1" in
  match reclaim with
  | Memory.Claim_ok r ->
      Alcotest.(check int) "same row reclaimed" row.queue_id r.queue_id
  | Memory.Claim_empty -> Alcotest.fail "expected reclaim to succeed"

let test_queue_delete_removes_row () =
  let db = Memory.init ~db_path:":memory:" () in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"msg":"del"}|}
  in
  let row =
    match Memory.queue_claim ~db ~session_key:"s1" with
    | Memory.Claim_ok r -> r
    | Memory.Claim_empty -> Alcotest.failf "expected claim"
  in
  let deleted = Memory.queue_delete ~db ~queue_id:row.queue_id in
  Alcotest.(check bool) "delete succeeded" true deleted;
  let rows = Memory.queue_list ~db ~session_key:"s1" in
  Alcotest.(check int) "queue empty after delete" 0 (List.length rows)

let test_queue_record_failure_tracks_error () =
  let db = Memory.init ~db_path:":memory:" () in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"msg":"fail"}|}
  in
  let row =
    match Memory.queue_claim ~db ~session_key:"s1" with
    | Memory.Claim_ok r -> r
    | Memory.Claim_empty -> Alcotest.failf "expected claim"
  in
  Memory.queue_record_failure ~db ~queue_id:row.queue_id ~error:"timeout";
  let rows = Memory.queue_list ~db ~session_key:"s1" in
  let updated = List.hd rows in
  Alcotest.(check int) "attempt_count incremented" 1 updated.attempt_count;
  Alcotest.(check (option string))
    "last_error" (Some "timeout") updated.last_error

let test_queue_reclaim_stale () =
  let db = Memory.init ~db_path:":memory:" () in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"msg":"stale"}|}
  in
  let _claimed =
    match Memory.queue_claim ~db ~session_key:"s1" with
    | Memory.Claim_ok r -> r
    | Memory.Claim_empty -> Alcotest.failf "expected claim"
  in
  (* Manually backdate claimed_at to make it stale *)
  exec_exn db
    "UPDATE inbound_queue SET claimed_at = datetime('now', '-7200 seconds') \
     WHERE session_key = 's1'";
  let reclaimed = Memory.queue_reclaim_stale ~db ~older_than_seconds:3600 in
  Alcotest.(check int) "one row reclaimed" 1 reclaimed;
  let result = Memory.queue_claim ~db ~session_key:"s1" in
  match result with
  | Memory.Claim_ok _ -> ()
  | Memory.Claim_empty -> Alcotest.fail "expected reclaimed row to be claimable"

let test_queue_reclaim_failed () =
  let db = Memory.init ~db_path:":memory:" () in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"msg":"retry"}|}
  in
  let row =
    match Memory.queue_claim ~db ~session_key:"s1" with
    | Memory.Claim_ok r -> r
    | Memory.Claim_empty -> Alcotest.failf "expected claim"
  in
  Memory.queue_record_failure ~db ~queue_id:row.queue_id ~error:"boom";
  let reclaimed = Memory.queue_reclaim_failed ~db in
  Alcotest.(check int) "one failed row reclaimed" 1 reclaimed;
  let result = Memory.queue_claim ~db ~session_key:"s1" in
  match result with
  | Memory.Claim_ok _ -> ()
  | Memory.Claim_empty -> Alcotest.fail "expected failed row to be reclaimable"

let test_queue_count () =
  let db = Memory.init ~db_path:":memory:" () in
  Alcotest.(check int)
    "empty count" 0
    (Memory.queue_count ~db ~session_key:"s1");
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"n":1}|}
  in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"n":2}|}
  in
  Alcotest.(check int)
    "two pending" 2
    (Memory.queue_count ~db ~session_key:"s1");
  let _ = Memory.queue_claim ~db ~session_key:"s1" in
  Alcotest.(check int)
    "one pending after claim" 1
    (Memory.queue_count ~db ~session_key:"s1")

let test_queue_session_isolation () =
  let db = Memory.init ~db_path:":memory:" () in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"from":"s1"}|}
  in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s2" ~source:"cli"
      ~payload_json:{|{"from":"s2"}|}
  in
  let s1_rows = Memory.queue_list ~db ~session_key:"s1" in
  let s2_rows = Memory.queue_list ~db ~session_key:"s2" in
  Alcotest.(check int) "s1 has 1" 1 (List.length s1_rows);
  Alcotest.(check int) "s2 has 1" 1 (List.length s2_rows)

let test_queue_clear_session_scoped () =
  let db = Memory.init ~db_path:":memory:" () in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"msg":"1"}|}
  in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"msg":"2"}|}
  in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s2" ~source:"cli"
      ~payload_json:{|{"msg":"3"}|}
  in
  let cleared = Memory.queue_clear ~db ~session_key:"s1" in
  Alcotest.(check int) "cleared 2 rows" 2 cleared;
  Alcotest.(check int)
    "s1 empty" 0
    (List.length (Memory.queue_list ~db ~session_key:"s1"));
  Alcotest.(check int)
    "s2 unaffected" 1
    (List.length (Memory.queue_list ~db ~session_key:"s2"))

let test_queue_clear_via_clear_session () =
  let db = Memory.init ~db_path:":memory:" () in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"msg":"test"}|}
  in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "hello");
  Memory.clear_session ~db ~session_key:"s1";
  let queue_rows = Memory.queue_list ~db ~session_key:"s1" in
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  Alcotest.(check int) "queue cleared" 0 (List.length queue_rows);
  Alcotest.(check int) "messages cleared" 0 (List.length msgs)

let test_queue_list_pending_sessions () =
  let db = Memory.init ~db_path:":memory:" () in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s2" ~source:"cli"
      ~payload_json:{|{"n":1}|}
  in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"n":2}|}
  in
  let sessions = Memory.queue_list_pending_sessions ~db in
  Alcotest.(check int) "two sessions with pending" 2 (List.length sessions);
  Alcotest.(check bool) "s1 in list" true (List.mem "s1" sessions);
  Alcotest.(check bool) "s2 in list" true (List.mem "s2" sessions)

let test_queue_count_all () =
  let db = Memory.init ~db_path:":memory:" () in
  Alcotest.(check int) "empty count_all" 0 (Memory.queue_count_all ~db);
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"cli"
      ~payload_json:{|{"n":1}|}
  in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s2" ~source:"cli"
      ~payload_json:{|{"n":2}|}
  in
  Alcotest.(check int)
    "two pending across sessions" 2
    (Memory.queue_count_all ~db);
  let _ = Memory.queue_claim ~db ~session_key:"s1" in
  Alcotest.(check int) "one pending after claim" 1 (Memory.queue_count_all ~db)

let test_queue_migrate_v4_to_v5 () =
  with_temp_db (fun db_path ->
      let db = Sqlite3.db_open db_path in
      exec_exn db "CREATE TABLE schema_version (version INTEGER NOT NULL)";
      exec_exn db "INSERT INTO schema_version (version) VALUES (4)";
      exec_exn db
        {|CREATE TABLE messages (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  session_key TEXT NOT NULL,
  role TEXT NOT NULL,
  content TEXT NOT NULL,
  tool_call_id TEXT,
  tool_name TEXT,
  tool_calls_json TEXT,
  provider_response_items_json TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
)|};
      exec_exn db
        "INSERT INTO messages (session_key, role, content) VALUES ('s1', \
         'user', 'pre-migration msg')";
      ignore (Sqlite3.db_close db);
      let migrated = Memory.init ~db_path () in
      Alcotest.(check int)
        "schema version is 7" 7
        (query_single_int migrated "SELECT version FROM schema_version");
      Alcotest.(check bool)
        "inbound_queue exists after v4->v5" true
        (table_exists migrated "inbound_queue");
      Alcotest.(check bool)
        "models_cache exists after v4->v5" true
        (table_exists migrated "models_cache");
      Alcotest.(check bool)
        "request_stats exists after v4->v5" true
        (table_exists migrated "request_stats");
      let msgs = Memory.load_history ~db:migrated ~session_key:"s1" in
      Alcotest.(check int) "pre-migration data preserved" 1 (List.length msgs);
      Alcotest.(check string)
        "content preserved" "pre-migration msg" (List.hd msgs).content)

let suite =
  [
    Alcotest.test_case "init sets busy_timeout" `Quick
      test_init_sets_busy_timeout;
    Alcotest.test_case "init creates db" `Quick test_init_creates_db;
    Alcotest.test_case "init search enabled" `Quick test_init_search_enabled;
    Alcotest.test_case "init search disabled" `Quick test_init_search_disabled;
    Alcotest.test_case "init double call" `Quick test_init_double_call;
    Alcotest.test_case "init schema version is 7" `Quick
      test_init_schema_version_is_6;
    Alcotest.test_case "init creates session persistence tables" `Quick
      test_init_creates_session_persistence_tables;
    Alcotest.test_case "migrates v1 db to v4 without data loss" `Quick
      test_migrates_v1_db_to_v4_without_data_loss;
    Alcotest.test_case "upsert session state roundtrip" `Quick
      test_upsert_session_state_roundtrip;
    Alcotest.test_case "mark response sent updates state" `Quick
      test_mark_response_sent_updates_state;
    Alcotest.test_case "load pending agent sessions filters rows" `Quick
      test_load_pending_agent_sessions_filters_rows;
    Alcotest.test_case "store_message inserts" `Quick test_store_message_inserts;
    Alcotest.test_case "store_message role preserved" `Quick
      test_store_message_role_preserved;
    Alcotest.test_case "store_message content preserved" `Quick
      test_store_message_content_preserved;
    Alcotest.test_case "store multiple messages" `Quick
      test_store_multiple_messages;
    Alcotest.test_case "store message with tool_call_id" `Quick
      test_store_message_with_tool_call_id;
    Alcotest.test_case "store message with tool_calls" `Quick
      test_store_message_with_tool_calls;
    Alcotest.test_case "load history order" `Quick test_load_history_order;
    Alcotest.test_case "load history empty session" `Quick
      test_load_history_empty_session;
    Alcotest.test_case "load history session isolation" `Quick
      test_load_history_session_isolation;
    Alcotest.test_case "clear session" `Quick test_clear_session;
    Alcotest.test_case "clear session isolates others" `Quick
      test_clear_session_isolates_others;
    Alcotest.test_case "replace session messages archives previous epoch" `Quick
      test_replace_session_messages_archives_previous_epoch;
    Alcotest.test_case "clear session removes archived epochs" `Quick
      test_clear_session_removes_archived_epochs;
    Alcotest.test_case "list sessions empty" `Quick test_list_sessions_empty;
    Alcotest.test_case "list sessions single" `Quick test_list_sessions_single;
    Alcotest.test_case "list sessions multiple" `Quick
      test_list_sessions_multiple;
    Alcotest.test_case "list sessions deduplicates" `Quick
      test_list_sessions_deduplicates;
    Alcotest.test_case "cleanup session max messages" `Quick
      test_cleanup_session_max_messages;
    Alcotest.test_case "cleanup session keeps newest" `Quick
      test_cleanup_session_max_messages_keeps_newest;
    Alcotest.test_case "cleanup session zero max noop" `Quick
      test_cleanup_session_zero_max_messages_noop;
    Alcotest.test_case "search finds matching content" `Quick
      test_search_finds_matching_content;
    Alcotest.test_case "search excludes non-matching" `Quick
      test_search_excludes_non_matching;
    Alcotest.test_case "search respects limit" `Quick test_search_respects_limit;
    Alcotest.test_case "search session filter" `Quick test_search_session_filter;
    Alcotest.test_case "search empty db" `Quick test_search_empty_db;
    Alcotest.test_case "cleanup all multiple sessions" `Quick
      test_cleanup_all_multiple_sessions;
    Alcotest.test_case "tool result roundtrip" `Quick test_tool_result_roundtrip;
    Alcotest.test_case "tool cycle history shape" `Quick
      test_tool_cycle_history_shape;
    Alcotest.test_case "provider response items roundtrip" `Quick
      test_provider_response_items_roundtrip;
    Alcotest.test_case "queue init creates table" `Quick
      test_queue_init_creates_table;
    Alcotest.test_case "queue enqueue and list" `Quick
      test_queue_enqueue_and_list;
    Alcotest.test_case "queue enqueue FIFO ordering" `Quick
      test_queue_enqueue_fifo_ordering;
    Alcotest.test_case "queue claim exclusive" `Quick test_queue_claim_exclusive;
    Alcotest.test_case "queue claim empty" `Quick test_queue_claim_empty;
    Alcotest.test_case "queue release makes reclaimable" `Quick
      test_queue_release_makes_reclaimable;
    Alcotest.test_case "queue delete removes row" `Quick
      test_queue_delete_removes_row;
    Alcotest.test_case "queue record failure tracks error" `Quick
      test_queue_record_failure_tracks_error;
    Alcotest.test_case "queue reclaim stale" `Quick test_queue_reclaim_stale;
    Alcotest.test_case "queue reclaim failed" `Quick test_queue_reclaim_failed;
    Alcotest.test_case "queue count" `Quick test_queue_count;
    Alcotest.test_case "queue session isolation" `Quick
      test_queue_session_isolation;
    Alcotest.test_case "queue clear session scoped" `Quick
      test_queue_clear_session_scoped;
    Alcotest.test_case "queue clear via clear_session" `Quick
      test_queue_clear_via_clear_session;
    Alcotest.test_case "queue list pending sessions" `Quick
      test_queue_list_pending_sessions;
    Alcotest.test_case "queue count all" `Quick test_queue_count_all;
    Alcotest.test_case "queue migrate v4 to v5" `Quick
      test_queue_migrate_v4_to_v5;
  ]

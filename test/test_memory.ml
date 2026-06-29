(* Tests for Memory module *)

let mk_msg role content = Provider.make_message ~role ~content

let with_temp_db f =
  let path = Filename.temp_file "clawq_memory" ".sqlite3" in
  Fun.protect
    (fun () -> f path)
    ~finally:(fun () -> try Sys.remove path with _ -> ())

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

let test_init_schema_version_is_current () =
  let db = Memory.init ~db_path:":memory:" () in
  Alcotest.(check int)
    "schema version is current" Memory.schema_version
    (Test_helpers.query_single_int db "SELECT version FROM schema_version")

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
    (table_exists db "request_stats");
  Alcotest.(check bool)
    "quota_cache exists" true
    (table_exists db "quota_cache");
  Alcotest.(check bool)
    "session_archives exists" true
    (table_exists db "session_archives");
  Alcotest.(check bool)
    "session_archive_messages exists" true
    (table_exists db "session_archive_messages");
  Alcotest.(check bool)
    "session_archive_epochs exists" true
    (table_exists db "session_archive_epochs");
  Alcotest.(check bool)
    "session_archive_epoch_messages exists" true
    (table_exists db "session_archive_epoch_messages");
  Alcotest.(check bool)
    "session_archive_metadata exists" true
    (table_exists db "session_archive_metadata")

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
        "schema version migrated" Memory.schema_version
        (Test_helpers.query_single_int migrated
           "SELECT version FROM schema_version");
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
    (Test_helpers.query_single_text_option db
       "SELECT turn FROM session_state WHERE session_key = 'telegram:42:user1'");
  Alcotest.(check (option string))
    "channel stored" (Some "telegram")
    (Test_helpers.query_single_text_option db
       "SELECT channel FROM session_state WHERE session_key = \
        'telegram:42:user1'");
  Alcotest.(check (option string))
    "channel id stored" (Some "42")
    (Test_helpers.query_single_text_option db
       "SELECT channel_id FROM session_state WHERE session_key = \
        'telegram:42:user1'")

let test_mark_response_sent_updates_state () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.upsert_session_state ~db ~session_key:"discord:chan:user" ~turn:"agent"
    ~channel:"discord" ~channel_id:"chan" ();
  Memory.mark_response_sent ~db ~session_key:"discord:chan:user";
  Alcotest.(check (option string))
    "turn reset to user" (Some "user")
    (Test_helpers.query_single_text_option db
       "SELECT turn FROM session_state WHERE session_key = 'discord:chan:user'");
  Alcotest.(check bool)
    "response_sent_at set" true
    (Test_helpers.query_single_text_option db
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
      thinking = None;
      is_error = false;
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
      thinking = None;
      is_error = false;
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
      thinking = None;
      is_error = false;
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
      ~role:"assistant" ~content:"" ()
  in
  Memory.store_message ~db ~session_key:"s1" msg;
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  let loaded = List.hd msgs in
  Alcotest.(check (option string))
    "provider response items preserved" msg.provider_response_items_json
    loaded.provider_response_items_json

let test_thinking_roundtrip () =
  let db = Memory.init ~db_path:":memory:" () in
  let msg =
    Provider.make_message_full ~role:"assistant" ~content:"answer"
      ~provider_response_items_json:None ~thinking:(Some "my reasoning") ()
  in
  Memory.store_message ~db ~session_key:"s1" msg;
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  let loaded = List.hd msgs in
  Alcotest.(check (option string))
    "thinking preserved" (Some "my reasoning") loaded.thinking

let test_thinking_none_compat () =
  let db = Memory.init ~db_path:":memory:" () in
  let msg = Provider.make_message ~role:"assistant" ~content:"no thinking" in
  Memory.store_message ~db ~session_key:"s1" msg;
  let msgs = Memory.load_history ~db ~session_key:"s1" in
  let loaded = List.hd msgs in
  Alcotest.(check (option string)) "thinking is None" None loaded.thinking

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
  let reclaimed = Memory.queue_reclaim_failed ~db () in
  Alcotest.(check int) "one failed row reclaimed" 1 reclaimed;
  let result = Memory.queue_claim ~db ~session_key:"s1" in
  match result with
  | Memory.Claim_ok _ -> ()
  | Memory.Claim_empty -> Alcotest.fail "expected failed row to be reclaimable"

let test_queue_reclaim_failed_max_retries () =
  let db = Memory.init ~db_path:":memory:" () in
  let _id =
    Memory.queue_enqueue ~db ~session_key:"s1" ~source:"test"
      ~payload_json:{|{"msg":"retry"}|}
  in
  let row =
    match Memory.queue_claim ~db ~session_key:"s1" with
    | Memory.Claim_ok r -> r
    | Memory.Claim_empty -> Alcotest.failf "expected claim"
  in
  (* Fail 10 times to exceed max_retries *)
  for _ = 1 to 10 do
    Memory.queue_record_failure ~db ~queue_id:row.queue_id ~error:"boom"
  done;
  (* With max_retries=5, item should stay failed *)
  let reclaimed = Memory.queue_reclaim_failed ~db ~max_retries:5 () in
  Alcotest.(check int) "no rows reclaimed (exceeded max_retries)" 0 reclaimed;
  let result = Memory.queue_claim ~db ~session_key:"s1" in
  match result with
  | Memory.Claim_ok _ ->
      Alcotest.fail "exceeded max_retries row should stay failed"
  | Memory.Claim_empty -> ()

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
        "schema version is current" Memory.schema_version
        (Test_helpers.query_single_int migrated
           "SELECT version FROM schema_version");
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

let column_exists db table_name col_name =
  let stmt =
    Sqlite3.prepare db
      (Printf.sprintf "SELECT 1 FROM pragma_table_info('%s') WHERE name = ?"
         table_name)
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT col_name));
      Sqlite3.step stmt = Sqlite3.Rc.ROW)

let test_migrate_v16_adds_effective_cwd () =
  with_temp_db (fun db_path ->
      let db = Sqlite3.db_open db_path in
      exec_exn db "CREATE TABLE schema_version (version INTEGER NOT NULL)";
      exec_exn db "INSERT INTO schema_version (version) VALUES (16)";
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
      (* v16 session_state: no effective_cwd column *)
      exec_exn db
        {|CREATE TABLE session_state (
  session_key TEXT PRIMARY KEY,
  turn TEXT NOT NULL DEFAULT 'user',
  channel TEXT,
  channel_id TEXT,
  response_sent_at TEXT,
  last_active TEXT NOT NULL DEFAULT (datetime('now')),
  keepalive_enabled INTEGER NOT NULL DEFAULT 0,
  heartbeat_enabled INTEGER NOT NULL DEFAULT 0,
  model_override TEXT DEFAULT NULL,
  CHECK ((channel IS NULL) = (channel_id IS NULL))
)|};
      ignore (Sqlite3.db_close db);
      let migrated = Memory.init ~db_path () in
      Alcotest.(check int)
        "schema version is current" Memory.schema_version
        (Test_helpers.query_single_int migrated
           "SELECT version FROM schema_version");
      Alcotest.(check bool)
        "effective_cwd column exists after v16 migration" true
        (column_exists migrated "session_state" "effective_cwd"))

let test_migrate_v23_adds_effective_cwd () =
  with_temp_db (fun db_path ->
      let db = Sqlite3.db_open db_path in
      exec_exn db "CREATE TABLE schema_version (version INTEGER NOT NULL)";
      exec_exn db "INSERT INTO schema_version (version) VALUES (23)";
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
  thinking_content TEXT,
  thinking_signature TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
)|};
      exec_exn db
        {|CREATE TABLE session_state (
  session_key TEXT PRIMARY KEY,
  turn TEXT NOT NULL DEFAULT 'user',
  channel TEXT,
  channel_id TEXT,
  response_sent_at TEXT,
  last_active TEXT NOT NULL DEFAULT (datetime('now')),
  keepalive_enabled INTEGER NOT NULL DEFAULT 0,
  heartbeat_enabled INTEGER NOT NULL DEFAULT 0,
  model_override TEXT DEFAULT NULL,
  CHECK ((channel IS NULL) = (channel_id IS NULL))
)|};
      ignore (Sqlite3.db_close db);
      let migrated = Memory.init ~db_path () in
      Alcotest.(check int)
        "schema version is current" Memory.schema_version
        (Test_helpers.query_single_int migrated
           "SELECT version FROM schema_version");
      Alcotest.(check bool)
        "effective_cwd column exists after v23 migration" true
        (column_exists migrated "session_state" "effective_cwd"))

let test_init_rejects_future_schema_version () =
  with_temp_db (fun db_path ->
      let db = Sqlite3.db_open db_path in
      exec_exn db "CREATE TABLE schema_version (version INTEGER NOT NULL)";
      let future_version = Memory.schema_version + 1 in
      exec_exn db
        (Printf.sprintf "INSERT INTO schema_version (version) VALUES (%d)"
           future_version);
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
      ignore (Sqlite3.db_close db);
      match
        try
          `Msg
            (Memory.init ~db_path () |> ignore;
             "init unexpectedly succeeded")
        with
        | Failure msg -> `Msg msg
        | exn -> `Msg (Printexc.to_string exn)
      with
      | `Msg msg ->
          Alcotest.(check bool)
            "rejects future version" true
            (String.starts_with
               ~prefix:
                 (Printf.sprintf "DB uses future schema version %d"
                    future_version)
               msg))

(* --- session archive tests --- *)

let test_archive_session_captures_messages () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "hello");
  Memory.store_message ~db ~session_key:"s1" (mk_msg "assistant" "hi back");
  Memory.archive_session ~db ~session_key:"s1";
  let count =
    Test_helpers.query_single_int db
      "SELECT COUNT(*) FROM session_archive_messages WHERE archive_id = \
       (SELECT archive_id FROM session_archives WHERE session_key = 's1')"
  in
  Alcotest.(check int) "archived 2 messages" 2 count;
  let archive_msg_count =
    Test_helpers.query_single_int db
      "SELECT message_count FROM session_archives WHERE session_key = 's1'"
  in
  Alcotest.(check int) "archive header message_count" 2 archive_msg_count

let test_archive_session_captures_epochs () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "old msg");
  Memory.store_message ~db ~session_key:"s1" (mk_msg "assistant" "old reply");
  Memory.replace_session_messages ~db ~session_key:"s1"
    [ mk_msg "assistant" "[compacted]" ];
  Memory.archive_session ~db ~session_key:"s1";
  let epoch_count =
    Test_helpers.query_single_int db
      "SELECT epoch_count FROM session_archives WHERE session_key = 's1'"
  in
  Alcotest.(check int) "archived 1 epoch" 1 epoch_count;
  let epoch_msg_count =
    Test_helpers.query_single_int db
      "SELECT COUNT(*) FROM session_archive_epoch_messages ae JOIN \
       session_archive_epochs e ON ae.archive_epoch_id = e.id JOIN \
       session_archives a ON e.archive_id = a.archive_id WHERE a.session_key = \
       's1'"
  in
  Alcotest.(check int) "epoch messages archived" 2 epoch_msg_count

let test_archive_session_captures_metadata () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "hi");
  Memory.upsert_session_state ~db ~session_key:"s1" ~turn:"agent"
    ~channel:"telegram" ~channel_id:"42" ();
  Memory.store_session_workspace_state ~db ~session_key:"s1"
    ~observed_active_workspace_files:[ ("foo.ml", Some "abc123") ];
  Memory.archive_session ~db ~session_key:"s1";
  let state_json =
    Test_helpers.query_single_text_option db
      "SELECT session_state_json FROM session_archive_metadata WHERE \
       archive_id = (SELECT archive_id FROM session_archives WHERE session_key \
       = 's1')"
  in
  Alcotest.(check bool) "session state archived" true (state_json <> None);
  let ws_json =
    Test_helpers.query_single_text_option db
      "SELECT workspace_state_json FROM session_archive_metadata WHERE \
       archive_id = (SELECT archive_id FROM session_archives WHERE session_key \
       = 's1')"
  in
  Alcotest.(check bool) "workspace state archived" true (ws_json <> None)

let test_archive_session_empty_is_noop () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.archive_session ~db ~session_key:"empty";
  let count =
    Test_helpers.query_single_int db
      "SELECT COUNT(*) FROM session_archives WHERE session_key = 'empty'"
  in
  Alcotest.(check int) "no archive for empty session" 0 count

let test_archive_session_multiple_accumulate () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "round1");
  Memory.archive_session ~db ~session_key:"s1";
  Memory.clear_session ~db ~session_key:"s1";
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "round2");
  Memory.archive_session ~db ~session_key:"s1";
  let count =
    Test_helpers.query_single_int db
      "SELECT COUNT(*) FROM session_archives WHERE session_key = 's1'"
  in
  Alcotest.(check int) "two archives accumulated" 2 count

let test_list_archives_for_session () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "msg1");
  Memory.archive_session ~db ~session_key:"s1";
  Memory.clear_session ~db ~session_key:"s1";
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "msg2");
  Memory.archive_session ~db ~session_key:"s1";
  let rows = Memory.list_archives_for_session ~db ~session_key:"s1" in
  Alcotest.(check int) "two archive rows" 2 (List.length rows);
  let first = List.hd rows in
  Alcotest.(check int) "first listed has higher id (DESC)" 1 first.message_count

let test_list_archive_sessions () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "a");
  Memory.archive_session ~db ~session_key:"s1";
  Memory.store_message ~db ~session_key:"s2" (mk_msg "user" "b");
  Memory.archive_session ~db ~session_key:"s2";
  let rows = Memory.list_archive_sessions ~db () in
  Alcotest.(check int) "two session groups" 2 (List.length rows);
  let keys = List.map fst rows in
  Alcotest.(check bool) "s1 present" true (List.mem "s1" keys);
  Alcotest.(check bool) "s2 present" true (List.mem "s2" keys)

let test_load_archive_messages () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "hello");
  Memory.store_message ~db ~session_key:"s1" (mk_msg "assistant" "hi back");
  Memory.archive_session ~db ~session_key:"s1";
  let archives = Memory.list_archives_for_session ~db ~session_key:"s1" in
  let archive_id = (List.hd archives).archive_id in
  let msgs = Memory.load_archive_messages ~db ~archive_id in
  Alcotest.(check int) "loaded 2 messages" 2 (List.length msgs);
  Alcotest.(check string) "first content" "hello" (List.hd msgs).content;
  Alcotest.(check string) "second content" "hi back" (List.nth msgs 1).content

let test_get_archive_info () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1" (mk_msg "user" "msg1");
  Memory.archive_session ~db ~session_key:"s1";
  let archives = Memory.list_archives_for_session ~db ~session_key:"s1" in
  let archive_id = (List.hd archives).archive_id in
  let info = Memory.get_archive_info ~db ~archive_id in
  Alcotest.(check bool) "info found" true (info <> None);
  let info = Option.get info in
  Alcotest.(check string) "session_key" "s1" info.session_key;
  Alcotest.(check int) "message_count" 1 info.message_count;
  let missing = Memory.get_archive_info ~db ~archive_id:99999 in
  Alcotest.(check bool) "nonexistent returns None" true (missing = None)

let test_load_archive_messages_empty () =
  let db = Memory.init ~db_path:":memory:" () in
  let msgs = Memory.load_archive_messages ~db ~archive_id:99999 in
  Alcotest.(check int) "empty for nonexistent" 0 (List.length msgs)

let test_session_cwd_get_set () =
  let db = Memory.init ~db_path:":memory:" () in
  let key = "test-cwd-session" in
  Alcotest.(check (option string))
    "initially None" None
    (Memory.get_session_cwd ~db ~session_key:key);
  Memory.set_session_cwd ~db ~session_key:key ~cwd:(Some "/tmp/project");
  Alcotest.(check (option string))
    "set to /tmp/project" (Some "/tmp/project")
    (Memory.get_session_cwd ~db ~session_key:key);
  Memory.set_session_cwd ~db ~session_key:key ~cwd:(Some "/home/user");
  Alcotest.(check (option string))
    "updated to /home/user" (Some "/home/user")
    (Memory.get_session_cwd ~db ~session_key:key);
  Memory.set_session_cwd ~db ~session_key:key ~cwd:None;
  Alcotest.(check (option string))
    "cleared to None" None
    (Memory.get_session_cwd ~db ~session_key:key)

let test_agent_create_with_cwd () =
  let config = Runtime_config.default in
  let agent = Agent.create ~config ~cwd:"/tmp" () in
  Alcotest.(check (option string))
    "cwd set" (Some "/tmp") agent.Agent.effective_cwd;
  let agent2 = Agent.create ~config () in
  Alcotest.(check (option string)) "no cwd" None agent2.Agent.effective_cwd

let test_session_info_includes_cwd () =
  let db = Memory.init ~db_path:":memory:" () in
  let key = "cwd-info-session" in
  Memory.set_session_cwd ~db ~session_key:key ~cwd:(Some "/tmp/test");
  let infos = Memory.list_session_infos ~db () in
  let row =
    List.find_opt (fun (r : Memory.session_info) -> r.session_key = key) infos
  in
  match row with
  | None -> Alcotest.fail "session not found in list"
  | Some r ->
      Alcotest.(check (option string))
        "effective_cwd in list" (Some "/tmp/test") r.effective_cwd

(* B656: get_session_channel must treat empty-string channel/channel_id as
   absent, not as a bound channel. Otherwise scheduler delivery falls through
   to "unsupported channel" errors (see B655 for the data condition). *)
let test_get_session_channel_empty_strings_are_none () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.set_session_cwd ~db ~session_key:"empty-chan" ~cwd:None;
  let stmt =
    Sqlite3.prepare db
      "UPDATE session_state SET channel = '', channel_id = '' WHERE \
       session_key = ?"
  in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT "empty-chan"));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  let result = Memory.get_session_channel ~db ~session_key:"empty-chan" in
  Alcotest.(check bool)
    "empty strings treated as no channel" true (result = None)

(* --- Snapshot export/import tests --- *)

let test_export_snapshot_roundtrip () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_core ~db ~key:"k1" ~content:"hello" ~category:"general" ();
  Memory.store_core ~db ~key:"k2" ~content:"world" ~category:"facts" ();
  let path = Filename.temp_file "clawq_snapshot" ".json" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () ->
      let exported = Memory.export_snapshot ~db ~path in
      Alcotest.(check int) "exported count" 2 exported;
      (* Read into a fresh DB *)
      let db2 = Memory.init ~db_path:":memory:" () in
      let imported = Memory.import_snapshot ~db:db2 ~path in
      Alcotest.(check int) "imported count" 2 imported;
      let memories = Memory.list_core ~db:db2 () in
      Alcotest.(check int) "memory count in db2" 2 (List.length memories);
      let find k = List.find_opt (fun (key, _, _) -> key = k) memories in
      (match find "k1" with
      | Some (_, c, cat) ->
          Alcotest.(check string) "k1 content" "hello" c;
          Alcotest.(check string) "k1 category" "general" cat
      | None -> Alcotest.fail "k1 not found");
      match find "k2" with
      | Some (_, c, cat) ->
          Alcotest.(check string) "k2 content" "world" c;
          Alcotest.(check string) "k2 category" "facts" cat
      | None -> Alcotest.fail "k2 not found")

let test_export_snapshot_metadata () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_core ~db ~key:"m1" ~content:"data" ();
  let path = Filename.temp_file "clawq_snapshot" ".json" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () ->
      ignore (Memory.export_snapshot ~db ~path);
      let ic = open_in path in
      let raw =
        Fun.protect
          ~finally:(fun () -> close_in ic)
          (fun () -> really_input_string ic (in_channel_length ic))
      in
      let json = Yojson.Safe.from_string raw in
      let open Yojson.Safe.Util in
      let fv = json |> member "format_version" |> to_int in
      Alcotest.(check int) "format_version" 1 fv;
      let sv = json |> member "schema_version" |> to_int in
      Alcotest.(check int) "schema_version" Memory.schema_version sv;
      let mc = json |> member "memory_count" |> to_int in
      Alcotest.(check int) "memory_count" 1 mc;
      let ea = json |> member "exported_at" |> to_string in
      Alcotest.(check bool) "exported_at non-empty" true (String.length ea > 0);
      (* Verify pretty-printed (contains newlines) *)
      Alcotest.(check bool) "pretty-printed" true (String.contains raw '\n'))

let test_export_snapshot_empty () =
  let db = Memory.init ~db_path:":memory:" () in
  let path = Filename.temp_file "clawq_snapshot" ".json" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () ->
      let exported = Memory.export_snapshot ~db ~path in
      Alcotest.(check int) "exported zero" 0 exported;
      let db2 = Memory.init ~db_path:":memory:" () in
      let imported = Memory.import_snapshot ~db:db2 ~path in
      Alcotest.(check int) "imported zero" 0 imported;
      Alcotest.(check int) "count zero" 0 (Memory.count_core ~db:db2))

let test_import_snapshot_legacy_version_field () =
  let path = Filename.temp_file "clawq_snapshot" ".json" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () ->
      (* Write legacy format with "version" instead of "format_version" *)
      let json =
        `Assoc
          [
            ("version", `Int 1);
            ( "memories",
              `List
                [
                  `Assoc
                    [
                      ("key", `String "legacy_key");
                      ("content", `String "legacy_val");
                    ];
                ] );
          ]
      in
      let oc = open_out path in
      output_string oc (Yojson.Safe.to_string json);
      close_out oc;
      let db = Memory.init ~db_path:":memory:" () in
      let imported = Memory.import_snapshot ~db ~path in
      Alcotest.(check int) "imported 1" 1 imported;
      let memories = Memory.list_core ~db () in
      match memories with
      | [ (k, c, cat) ] ->
          Alcotest.(check string) "key" "legacy_key" k;
          Alcotest.(check string) "content" "legacy_val" c;
          Alcotest.(check string) "default category" "general" cat
      | _ -> Alcotest.fail "expected exactly 1 memory")

let test_import_snapshot_rejects_bad_version () =
  let path = Filename.temp_file "clawq_snapshot" ".json" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () ->
      let json =
        `Assoc [ ("format_version", `Int 999); ("memories", `List []) ]
      in
      let oc = open_out path in
      output_string oc (Yojson.Safe.to_string json);
      close_out oc;
      let db = Memory.init ~db_path:":memory:" () in
      match Memory.import_snapshot ~db ~path with
      | _ -> Alcotest.fail "expected failure for bad version"
      | exception Failure msg ->
          Alcotest.(check bool)
            "error mentions version" true
            (String.length msg > 0
            &&
              try
                ignore (Str.search_forward (Str.regexp_string "999") msg 0);
                true
              with Not_found -> false))

let test_import_snapshot_upserts () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_core ~db ~key:"k1" ~content:"original" ();
  let path = Filename.temp_file "clawq_snapshot" ".json" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () ->
      let json =
        `Assoc
          [
            ("format_version", `Int 1);
            ( "memories",
              `List
                [
                  `Assoc
                    [
                      ("key", `String "k1");
                      ("content", `String "updated");
                      ("category", `String "general");
                    ];
                  `Assoc
                    [
                      ("key", `String "k2");
                      ("content", `String "new");
                      ("category", `String "general");
                    ];
                ] );
          ]
      in
      let oc = open_out path in
      output_string oc (Yojson.Safe.to_string json);
      close_out oc;
      let imported = Memory.import_snapshot ~db ~path in
      Alcotest.(check int) "imported 2" 2 imported;
      let memories = Memory.list_core ~db () in
      Alcotest.(check int) "total 2" 2 (List.length memories);
      let find k = List.find_opt (fun (key, _, _) -> key = k) memories in
      (match find "k1" with
      | Some (_, c, _) -> Alcotest.(check string) "k1 updated" "updated" c
      | None -> Alcotest.fail "k1 not found");
      match find "k2" with
      | Some (_, c, _) -> Alcotest.(check string) "k2 new" "new" c
      | None -> Alcotest.fail "k2 not found")

(* --- room profile schema tests --- *)

let test_init_creates_room_profile_tables () =
  let db = Memory.init ~db_path:":memory:" () in
  Alcotest.(check bool)
    "room_profiles exists" true
    (table_exists db "room_profiles");
  Alcotest.(check bool)
    "room_profile_bindings exists" true
    (table_exists db "room_profile_bindings")

let test_ensure_all_tables_creates_room_profiles () =
  let db = Memory.init ~db_path:":memory:" () in
  (* Drop tables to simulate a pre-existing db without them *)
  exec_exn db "DROP TABLE room_profile_bindings";
  exec_exn db "DROP TABLE room_profiles";
  Alcotest.(check bool)
    "room_profiles gone" false
    (table_exists db "room_profiles");
  Memory.ensure_all_tables db;
  Alcotest.(check bool)
    "room_profiles restored by ensure_all_tables" true
    (table_exists db "room_profiles");
  Alcotest.(check bool)
    "room_profile_bindings restored by ensure_all_tables" true
    (table_exists db "room_profile_bindings")

let test_migrate_v31_to_current_creates_room_profiles () =
  with_temp_db (fun db_path ->
      let db = Sqlite3.db_open db_path in
      exec_exn db "CREATE TABLE schema_version (version INTEGER NOT NULL)";
      exec_exn db "INSERT INTO schema_version (version) VALUES (31)";
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
  thinking_content TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
)|};
      ignore (Sqlite3.db_close db);
      let migrated = Memory.init ~db_path () in
      Alcotest.(check int)
        "schema version is current" Memory.schema_version
        (Test_helpers.query_single_int migrated
           "SELECT version FROM schema_version");
      Alcotest.(check bool)
        "room_profiles exists after v31 migration" true
        (table_exists migrated "room_profiles");
      Alcotest.(check bool)
        "room_profile_bindings exists after v31 migration" true
        (table_exists migrated "room_profile_bindings"))

let test_migrate_v32_adds_origin_columns () =
  with_temp_db (fun db_path ->
      let db = Sqlite3.db_open db_path in
      exec_exn db "CREATE TABLE schema_version (version INTEGER NOT NULL)";
      exec_exn db "INSERT INTO schema_version (version) VALUES (32)";
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
  thinking_content TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
)|};
      exec_exn db
        {|CREATE TABLE task_tree (
  id INTEGER NOT NULL,
  session_key TEXT NOT NULL,
  parent_id INTEGER,
  title TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'pending',
  note TEXT,
  depends_on TEXT,
  agent_model TEXT,
  agent_type TEXT,
  agent_prompt TEXT,
  agent_details TEXT,
  autostart INTEGER NOT NULL DEFAULT 0,
  agent_task_id INTEGER,
  sort_order INTEGER NOT NULL DEFAULT 0,
  deleted_at TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now')),
  PRIMARY KEY (session_key, id)
)|};
      exec_exn db
        {|CREATE TABLE background_tasks (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  runner TEXT NOT NULL,
  model TEXT,
  repo_path TEXT NOT NULL,
  prompt TEXT NOT NULL,
  branch TEXT,
  status TEXT NOT NULL DEFAULT 'queued',
  exit_code INTEGER,
  started_at TEXT,
  finished_at TEXT,
  result TEXT,
  session_key TEXT,
  channel TEXT,
  channel_id TEXT,
  automerge INTEGER NOT NULL DEFAULT 1,
  use_worktree INTEGER NOT NULL DEFAULT 1,
  worktree_path TEXT,
  log_path TEXT,
  merge_status TEXT,
  retry_count INTEGER NOT NULL DEFAULT 0,
  parent_task_id INTEGER,
  replaced_by INTEGER,
  runner_session_id TEXT,
  acp INTEGER NOT NULL DEFAULT 0,
  agent_name TEXT,
  notification_status TEXT,
  notification_error TEXT,
  notification_attempts INTEGER NOT NULL DEFAULT 0,
  follow_up_prompt TEXT
)|};
      (* Insert a pre-migration task_tree row *)
      exec_exn db
        "INSERT INTO task_tree (id, session_key, title) VALUES (1, 's1', \
         'pre-migration task')";
      (* Insert a pre-migration background_tasks row *)
      exec_exn db
        "INSERT INTO background_tasks (runner, repo_path, prompt) VALUES \
         ('codex', '/tmp/repo', 'test prompt')";
      ignore (Sqlite3.db_close db);
      let migrated = Memory.init ~db_path () in
      Alcotest.(check int)
        "schema version is current" Memory.schema_version
        (Test_helpers.query_single_int migrated
           "SELECT version FROM schema_version");
      (* Assert all new origin columns exist on task_tree *)
      Alcotest.(check bool)
        "task_tree.profile_id exists" true
        (column_exists migrated "task_tree" "profile_id");
      Alcotest.(check bool)
        "task_tree.origin_json exists" true
        (column_exists migrated "task_tree" "origin_json");
      Alcotest.(check bool)
        "task_tree.thread_id exists" true
        (column_exists migrated "task_tree" "thread_id");
      Alcotest.(check bool)
        "task_tree.requester exists" true
        (column_exists migrated "task_tree" "requester");
      (* Assert all new origin columns exist on background_tasks *)
      Alcotest.(check bool)
        "background_tasks.profile_id exists" true
        (column_exists migrated "background_tasks" "profile_id");
      Alcotest.(check bool)
        "background_tasks.origin_json exists" true
        (column_exists migrated "background_tasks" "origin_json");
      Alcotest.(check bool)
        "background_tasks.thread_id exists" true
        (column_exists migrated "background_tasks" "thread_id");
      Alcotest.(check bool)
        "background_tasks.requester exists" true
        (column_exists migrated "background_tasks" "requester");
      (* Pre-migration rows are still readable *)
      let stmt =
        Sqlite3.prepare migrated
          "SELECT title FROM task_tree WHERE session_key = 's1'"
      in
      (match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let title =
            match Sqlite3.column stmt 0 with
            | Sqlite3.Data.TEXT s -> s
            | _ -> ""
          in
          Alcotest.(check string)
            "pre-migration task readable" "pre-migration task" title
      | _ -> Alcotest.fail "pre-migration task_tree row not found");
      ignore (Sqlite3.finalize stmt);
      let stmt =
        Sqlite3.prepare migrated
          "SELECT prompt FROM background_tasks WHERE id = 1"
      in
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let prompt =
            match Sqlite3.column stmt 0 with
            | Sqlite3.Data.TEXT s -> s
            | _ -> ""
          in
          Alcotest.(check string)
            "pre-migration bg task readable" "test prompt" prompt;
          ignore (Sqlite3.finalize stmt)
      | _ ->
          ignore (Sqlite3.finalize stmt);
          Alcotest.fail "pre-migration background_tasks row not found")

(* --- scoped memory schema tests --- *)

let index_exists db index_name =
  let stmt =
    Sqlite3.prepare db
      "SELECT 1 FROM sqlite_master WHERE type = 'index' AND name = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT index_name));
      Sqlite3.step stmt = Sqlite3.Rc.ROW)

let test_migrate_v35_adds_request_stats_profile_columns_before_index () =
  with_temp_db (fun db_path ->
      let db = Sqlite3.db_open db_path in
      exec_exn db "CREATE TABLE schema_version (version INTEGER NOT NULL)";
      exec_exn db "INSERT INTO schema_version (version) VALUES (35)";
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
  thinking_content TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
)|};
      exec_exn db
        {|CREATE TABLE request_stats (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  session_key TEXT NOT NULL,
  message_id INTEGER,
  provider TEXT NOT NULL,
  model TEXT NOT NULL,
  prompt_tokens INTEGER NOT NULL,
  completion_tokens INTEGER NOT NULL,
  cost_usd REAL,
  requested_at TEXT NOT NULL DEFAULT (datetime('now')),
  added_prompt_tokens INTEGER,
  cached_tokens INTEGER
)|};
      ignore (Sqlite3.db_close db);
      let migrated = Memory.init ~db_path () in
      Alcotest.(check int)
        "schema version is current" Memory.schema_version
        (Test_helpers.query_single_int migrated
           "SELECT version FROM schema_version");
      Alcotest.(check bool)
        "request_stats.profile_id exists" true
        (column_exists migrated "request_stats" "profile_id");
      Alcotest.(check bool)
        "request_stats.latency_ms exists" true
        (column_exists migrated "request_stats" "latency_ms");
      Alcotest.(check bool)
        "profile-time index exists" true
        (index_exists migrated "idx_request_stats_profile_time"))

let foreign_key_exists db table_name ~from_col ~to_table ~on_delete =
  let stmt =
    Sqlite3.prepare db (Printf.sprintf "PRAGMA foreign_key_list(%s)" table_name)
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let found = ref false in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let text_col i =
          match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        if
          text_col 2 = to_table
          && text_col 3 = from_col
          && text_col 6 = on_delete
        then found := true
      done;
      !found)

let exec_succeeds db sql = Sqlite3.exec db sql = Sqlite3.Rc.OK
let exec_fails db sql = not (exec_succeeds db sql)

let insert_memory_grant ?expires_at ?revoked_at ~db ~scope_id ~principal_kind
    ~principal_id ~capability () =
  let has_revoked_at = column_exists db "memory_grants" "revoked_at" in
  let sql =
    "INSERT INTO memory_grants (scope_id, principal_kind, principal_id, \
     capability, expires_at"
    ^ (if has_revoked_at then ", revoked_at" else "")
    ^ ") VALUES (?, ?, ?, ?, ?"
    ^ (if has_revoked_at then ", ?" else "")
    ^ ")"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int scope_id)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT principal_kind));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT principal_id));
      ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT capability));
      (match expires_at with
      | Some value -> ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT value))
      | None -> ignore (Sqlite3.bind stmt 5 Sqlite3.Data.NULL));
      (if has_revoked_at then
         match revoked_at with
         | Some value -> ignore (Sqlite3.bind stmt 6 (Sqlite3.Data.TEXT value))
         | None -> ignore (Sqlite3.bind stmt 6 Sqlite3.Data.NULL));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          Alcotest.failf "insert_memory_grant failed: %s"
            (Sqlite3.Rc.to_string rc))

let test_init_creates_scoped_memory_tables () =
  let db = Memory.init ~db_path:":memory:" () in
  List.iter
    (fun table ->
      Alcotest.(check bool) (table ^ " exists") true (table_exists db table))
    [ "memory_scopes"; "scoped_memories"; "memory_grants" ];
  List.iter
    (fun kind ->
      exec_exn db
        (Printf.sprintf
           "INSERT INTO memory_scopes (kind, key, provenance) VALUES ('%s', \
            'key-%s', 'test')"
           kind kind))
    [ "personal"; "room"; "thread"; "workspace"; "legacy" ];
  Alcotest.(check bool)
    "invalid scope kind rejected" true
    (exec_fails db
       "INSERT INTO memory_scopes (kind, key, provenance) VALUES ('global', \
        'bad', 'test')");
  Alcotest.(check bool)
    "duplicate kind/key rejected" true
    (exec_fails db
       "INSERT INTO memory_scopes (kind, key, provenance) VALUES ('personal', \
        'key-personal', 'test')");
  Alcotest.(check bool)
    "same key accepted across kinds" true
    (exec_succeeds db
       "INSERT INTO memory_scopes (kind, key, provenance) VALUES ('room', \
        'key-personal', 'test')")

let test_scoped_memory_schema_shape () =
  let db = Memory.init ~db_path:":memory:" () in
  List.iter
    (fun col ->
      Alcotest.(check bool)
        ("memory_scopes." ^ col) true
        (column_exists db "memory_scopes" col))
    [
      "id";
      "kind";
      "key";
      "profile_id";
      "parent_scope_id";
      "provenance";
      "created_at";
      "updated_at";
    ];
  List.iter
    (fun col ->
      Alcotest.(check bool)
        ("scoped_memories." ^ col) true
        (column_exists db "scoped_memories" col))
    [
      "id";
      "scope_id";
      "content";
      "reference";
      "provenance";
      "created_at";
      "updated_at";
      "redacted_at";
      "redaction_reason";
      "redaction_metadata";
    ];
  List.iter
    (fun col ->
      Alcotest.(check bool)
        ("memory_grants." ^ col) true
        (column_exists db "memory_grants" col))
    [
      "id";
      "scope_id";
      "principal_kind";
      "principal_id";
      "capability";
      "grantor_kind";
      "grantor_id";
      "created_at";
      "expires_at";
      "is_transitive";
    ];
  List.iter
    (fun index -> Alcotest.(check bool) index true (index_exists db index))
    [
      "idx_memory_scopes_profile";
      "idx_memory_scopes_parent";
      "idx_scoped_memories_scope_created";
      "idx_scoped_memories_reference";
      "idx_scoped_memories_redacted";
      "idx_memory_grants_scope";
      "idx_memory_grants_principal";
      "idx_memory_grants_capability";
    ];
  Alcotest.(check bool)
    "memory_scopes.profile_id ON DELETE SET NULL" true
    (foreign_key_exists db "memory_scopes" ~from_col:"profile_id"
       ~to_table:"room_profiles" ~on_delete:"SET NULL");
  Alcotest.(check bool)
    "memory_scopes.parent_scope_id ON DELETE SET NULL" true
    (foreign_key_exists db "memory_scopes" ~from_col:"parent_scope_id"
       ~to_table:"memory_scopes" ~on_delete:"SET NULL");
  Alcotest.(check bool)
    "scoped_memories.scope_id ON DELETE CASCADE" true
    (foreign_key_exists db "scoped_memories" ~from_col:"scope_id"
       ~to_table:"memory_scopes" ~on_delete:"CASCADE");
  Alcotest.(check bool)
    "memory_grants.scope_id ON DELETE CASCADE" true
    (foreign_key_exists db "memory_grants" ~from_col:"scope_id"
       ~to_table:"memory_scopes" ~on_delete:"CASCADE")

let test_scoped_memory_constraints_and_cascade () =
  let db = Memory.init ~db_path:":memory:" () in
  exec_exn db
    "INSERT INTO memory_scopes (kind, key, provenance) VALUES ('room', 'r1', \
     'test')";
  let scope_id = Int64.to_int (Sqlite3.last_insert_rowid db) in
  exec_exn db
    (Printf.sprintf
       "INSERT INTO scoped_memories (scope_id, content, provenance) VALUES \
        (%d, 'remember this', 'test')"
       scope_id);
  exec_exn db
    (Printf.sprintf
       "INSERT INTO scoped_memories (scope_id, reference, provenance) VALUES \
        (%d, 'msg:1', 'test')"
       scope_id);
  exec_exn db
    (Printf.sprintf
       "INSERT INTO memory_grants (scope_id, principal_kind, principal_id, \
        capability, grantor_kind, grantor_id) VALUES (%d, 'user', 'u1', \
        'read', 'system', 'migration')"
       scope_id);
  Alcotest.(check bool)
    "memory requires content or reference" true
    (exec_fails db
       (Printf.sprintf
          "INSERT INTO scoped_memories (scope_id, provenance) VALUES (%d, \
           'test')"
          scope_id));
  Alcotest.(check bool)
    "grants are direct and non-transitive" true
    (exec_fails db
       (Printf.sprintf
          "INSERT INTO memory_grants (scope_id, principal_kind, principal_id, \
           capability, is_transitive) VALUES (%d, 'user', 'u2', 'read', 1)"
          scope_id));
  exec_exn db "DELETE FROM memory_scopes WHERE key = 'r1'";
  Alcotest.(check int)
    "scoped memories cascade" 0
    (Test_helpers.query_single_int db "SELECT COUNT(*) FROM scoped_memories");
  Alcotest.(check int)
    "memory grants cascade" 0
    (Test_helpers.query_single_int db
       "SELECT COUNT(*) FROM memory_grants WHERE principal_id = 'u1'")

let test_memory_grants_require_admin_for_create_and_revoke () =
  let db = Memory.init ~db_path:":memory:" () in
  let scope = Memory.create_scope ~db ~kind:"room" ~key:"room-1" () in
  let grant_count () =
    Test_helpers.query_single_int db
      "SELECT COUNT(*) FROM memory_grants WHERE scope_id = (SELECT id FROM \
       memory_scopes WHERE kind = 'room' AND key = 'room-1')"
  in
  (match
     Memory.grant_access ~db ~is_admin:false ~scope_id:scope.id
       ~principal_kind:"user" ~principal_id:"u1" ~capability:"read" ()
   with
  | Error msg ->
      Alcotest.(check bool)
        "non-admin create error mentions admin" true
        (Test_helpers.string_contains msg "admin")
  | Ok () -> Alcotest.fail "non-admin grant create should fail");
  Alcotest.(check int) "non-admin create leaves no grants" 0 (grant_count ());
  (match
     Memory.grant_access ~db ~is_admin:true ~scope_id:scope.id
       ~principal_kind:"user" ~principal_id:"u1" ~capability:"read" ()
   with
  | Ok () -> ()
  | Error msg -> Alcotest.fail msg);
  Alcotest.(check int) "admin create stores grant" 1 (grant_count ());
  (match
     Memory.revoke_access ~db ~is_admin:false ~scope_id:scope.id
       ~principal_kind:"user" ~principal_id:"u1" ~capability:"read" ()
   with
  | Error msg ->
      Alcotest.(check bool)
        "non-admin revoke error mentions admin" true
        (Test_helpers.string_contains msg "admin")
  | Ok _ -> Alcotest.fail "non-admin grant revoke should fail");
  Alcotest.(check int) "non-admin revoke leaves grant" 1 (grant_count ());
  (match
     Memory.revoke_access ~db ~is_admin:true ~scope_id:scope.id
       ~principal_kind:"user" ~principal_id:"u1" ~capability:"read" ()
   with
  | Ok removed -> Alcotest.(check int) "admin revoke removes one" 1 removed
  | Error msg -> Alcotest.fail msg);
  Alcotest.(check int) "admin revoke clears grant" 0 (grant_count ())

let test_scoped_memory_schema_migration_and_repair_paths () =
  with_temp_db (fun db_path ->
      let db = Sqlite3.db_open db_path in
      exec_exn db "CREATE TABLE schema_version (version INTEGER NOT NULL)";
      exec_exn db "INSERT INTO schema_version (version) VALUES (33)";
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
  thinking_content TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
)|};
      ignore (Sqlite3.db_close db);
      let migrated = Memory.init ~db_path () in
      Alcotest.(check int)
        "schema version is current" Memory.schema_version
        (Test_helpers.query_single_int migrated
           "SELECT version FROM schema_version");
      Alcotest.(check bool)
        "migrate_step created memory_scopes" true
        (table_exists migrated "memory_scopes");
      exec_exn migrated "DROP TABLE memory_grants";
      exec_exn migrated "DROP TABLE scoped_memories";
      exec_exn migrated "DROP TABLE memory_scopes";
      Memory.ensure_all_tables migrated;
      Alcotest.(check bool)
        "ensure_all_tables restores memory_scopes" true
        (table_exists migrated "memory_scopes");
      exec_exn migrated "DROP TABLE memory_grants";
      exec_exn migrated "DROP TABLE scoped_memories";
      exec_exn migrated "DROP TABLE memory_scopes";
      Memory.repair_missing_columns migrated;
      Alcotest.(check bool)
        "repair_missing_columns restores memory_scopes" true
        (table_exists migrated "memory_scopes"))

let test_scoped_memory_double_init_is_idempotent () =
  with_temp_db (fun db_path ->
      let db1 = Memory.init ~db_path () in
      exec_exn db1
        "INSERT INTO memory_scopes (kind, key, provenance) VALUES ('personal', \
         'u1', 'test')";
      ignore (Sqlite3.db_close db1);
      let db2 = Memory.init ~db_path () in
      Alcotest.(check int)
        "existing scope preserved" 1
        (Test_helpers.query_single_int db2
           "SELECT COUNT(*) FROM memory_scopes WHERE kind = 'personal' AND key \
            = 'u1'");
      Alcotest.(check int)
        "schema version current after second init" Memory.schema_version
        (Test_helpers.query_single_int db2 "SELECT version FROM schema_version"))

let legacy_scope_id db =
  Test_helpers.query_single_int db
    "SELECT id FROM memory_scopes WHERE kind = 'legacy' AND key = 'core'"

let legacy_grant_capabilities db =
  let stmt =
    Sqlite3.prepare db
      "SELECT capability FROM memory_grants WHERE scope_id = ? ORDER BY \
       capability"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.INT (Int64.of_int (legacy_scope_id db))));
      let capabilities = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.TEXT s -> capabilities := s :: !capabilities
        | _ -> ()
      done;
      List.rev !capabilities)

let test_legacy_memory_scope_seeded_read_only () =
  let db = Memory.init ~db_path:":memory:" () in
  Alcotest.(check int)
    "legacy core scope seeded once" 1
    (Test_helpers.query_single_int db
       "SELECT COUNT(*) FROM memory_scopes WHERE kind = 'legacy' AND key = \
        'core' AND provenance = 'system'");
  Alcotest.(check (list string))
    "legacy grants are read/list only" [ "list"; "read" ]
    (legacy_grant_capabilities db);
  let legacy_id = legacy_scope_id db in
  Alcotest.(check bool)
    "legacy write grant rejected" true
    (exec_fails db
       (Printf.sprintf
          "INSERT INTO memory_grants (scope_id, principal_kind, principal_id, \
           capability, grantor_kind, grantor_id) VALUES (%d, 'system', \
           'legacy', 'write', 'system', 'seed')"
          legacy_id));
  Alcotest.(check (list string))
    "failed write grant leaves read/list only" [ "list"; "read" ]
    (legacy_grant_capabilities db)

let test_legacy_memory_scope_is_idempotent () =
  with_temp_db (fun db_path ->
      let db1 = Memory.init ~db_path () in
      ignore (Sqlite3.db_close db1);
      let db2 = Memory.init ~db_path () in
      Alcotest.(check int)
        "legacy scope not duplicated" 1
        (Test_helpers.query_single_int db2
           "SELECT COUNT(*) FROM memory_scopes WHERE kind = 'legacy' AND key = \
            'core'");
      Alcotest.(check (list string))
        "legacy grants not duplicated" [ "list"; "read" ]
        (legacy_grant_capabilities db2))

let test_legacy_memory_fallback_preserves_existing_reads () =
  let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
  Memory.store_core ~db ~key:"rig:briefing:config"
    ~content:"legacy briefing memory" ~category:"rig" ();
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user"
       ~content:"please search the legacy briefing memory");
  Alcotest.(check int)
    "legacy scope does not copy core memories" 0
    (Test_helpers.query_single_int db "SELECT COUNT(*) FROM scoped_memories");
  Alcotest.(check bool)
    "core list reads still work" true
    (List.exists
       (fun (key, content, category) ->
         key = "rig:briefing:config"
         && content = "legacy briefing memory"
         && category = "rig")
       (Memory.list_core ~db ()));
  Alcotest.(check bool)
    "core recall still works" true
    (List.exists
       (fun (key, _, _) -> key = "rig:briefing:config")
       (Memory.recall_core ~db ~query:"briefing" ~limit:5));
  Alcotest.(check bool)
    "message search still falls back to existing path" true
    (List.exists
       (fun (m : Provider.message) -> m.content <> "")
       (Memory.search ~db ~query:"briefing" ~limit:5 ()))

let test_scoped_memory_scope_crud_roundtrip () =
  let db = Memory.init ~db_path:":memory:" () in
  let scope =
    Memory.create_scope ~db ~kind:"personal" ~key:"u1" ~provenance:"test" ()
  in
  Alcotest.(check bool) "scope id set" true (scope.id > 0);
  Alcotest.(check string) "scope kind" "personal" scope.kind;
  Alcotest.(check string) "scope key" "u1" scope.key;
  (match Memory.get_scope ~db ~id:scope.id with
  | None -> Alcotest.fail "expected scope by id"
  | Some found -> Alcotest.(check string) "found key" "u1" found.key);
  let same =
    Memory.create_scope ~db ~kind:"personal" ~key:"u1" ~provenance:"again" ()
  in
  Alcotest.(check int) "double create returns existing" scope.id same.id;
  let room = Memory.create_scope ~db ~kind:"room" ~key:"r1" () in
  let all = Memory.list_scopes ~db () in
  (* legacy scope is auto-seeded, so we have 3 total *)
  Alcotest.(check bool) "scopes include created" true (List.length all >= 2);
  let personal = Memory.list_scopes ~db ~kind:"personal" () in
  Alcotest.(check int) "one personal scope" 1 (List.length personal);
  Alcotest.(check int)
    "room scope created" room.id
    (List.hd (Memory.list_scopes ~db ~kind:"room" ())).id

let test_scoped_memory_upsert_is_idempotent () =
  let db = Memory.init ~db_path:":memory:" () in
  let scope = Memory.create_scope ~db ~kind:"thread" ~key:"t1" () in
  let first =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"msg:1"
      ~content:"first content" ~provenance:"tool" ()
  in
  let second =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"msg:1"
      ~content:"updated content" ~provenance:"manual" ()
  in
  Alcotest.(check int) "same row reused" first.id second.id;
  Alcotest.(check string)
    "content updated" "updated content"
    (Option.get second.content);
  Alcotest.(check string) "provenance updated" "manual" second.provenance;
  let rows = Memory.query_scoped_memories ~db ~limit:10 () in
  Alcotest.(check int) "still one row" 1 (List.length rows)

let test_scoped_memory_query_filters_and_pagination () =
  let db = Memory.init ~db_path:":memory:" () in
  let personal = Memory.create_scope ~db ~kind:"personal" ~key:"u1" () in
  let room = Memory.create_scope ~db ~kind:"room" ~key:"r1" () in
  let other_personal = Memory.create_scope ~db ~kind:"personal" ~key:"u2" () in
  ignore
    (Memory.upsert_scoped_memory ~db ~scope_id:personal.id ~reference:"a"
       ~content:"alpha fish" ~provenance:"tool" ());
  ignore
    (Memory.upsert_scoped_memory ~db ~scope_id:personal.id ~reference:"b"
       ~content:"beta fish" ~provenance:"manual" ());
  ignore
    (Memory.upsert_scoped_memory ~db ~scope_id:room.id ~reference:"c"
       ~content:"alpha bird" ~provenance:"tool" ());
  ignore
    (Memory.upsert_scoped_memory ~db ~scope_id:other_personal.id ~reference:"d"
       ~content:"fish only" ~provenance:"tool" ());
  let filtered =
    Memory.query_scoped_memories ~db ~scope_kind:"personal"
      ~content_search:"alpha" ~provenance:"tool" ~limit:10 ()
  in
  Alcotest.(check int) "one filtered row" 1 (List.length filtered);
  Alcotest.(check string) "filtered reference" "a" (List.hd filtered).reference;
  let paged =
    Memory.query_scoped_memories ~db ~content_search:"fish" ~limit:1 ~offset:1
      ()
  in
  Alcotest.(check int) "one paged row" 1 (List.length paged);
  Alcotest.(check string) "second fish row" "b" (List.hd paged).reference

let test_scoped_memory_delete_and_boundaries () =
  let db = Memory.init ~db_path:":memory:" () in
  let scope = Memory.create_scope ~db ~kind:"workspace" ~key:"/repo" () in
  let row =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"file:1"
      ~content:"path data" ()
  in
  Alcotest.(check bool)
    "deleted existing row" true
    (Memory.delete_scoped_memory ~db ~id:row.id);
  Alcotest.(check bool)
    "delete missing row is false" false
    (Memory.delete_scoped_memory ~db ~id:row.id);
  Alcotest.(check int)
    "deleted row not queried" 0
    (List.length (Memory.query_scoped_memories ~db ~limit:10 ()));
  Alcotest.(check int)
    "unknown kind has no scopes" 0
    (List.length (Memory.list_scopes ~db ~kind:"room" ()));
  Alcotest.(check int)
    "zero limit returns no rows" 0
    (List.length (Memory.query_scoped_memories ~db ~limit:0 ()))

let test_resolve_grants_single_grant () =
  let db = Memory.init ~db_path:":memory:" () in
  let scope = Memory.create_scope ~db ~kind:"room" ~key:"r1" () in
  insert_memory_grant ~db ~scope_id:scope.id ~principal_kind:"profile"
    ~principal_id:"p1" ~capability:"read" ();
  Alcotest.(check (list string))
    "single direct grant" [ "read" ]
    (Memory.resolve_grants ~db ~scope_id:scope.id ~principal_kind:"profile"
       ~principal_id:"p1")

let test_resolve_grants_merges_matching_direct_grants () =
  let db = Memory.init ~db_path:":memory:" () in
  let scope = Memory.create_scope ~db ~kind:"room" ~key:"r1" () in
  let other_scope = Memory.create_scope ~db ~kind:"room" ~key:"r2" () in
  insert_memory_grant ~db ~scope_id:scope.id ~principal_kind:"profile"
    ~principal_id:"p1" ~capability:"write" ();
  insert_memory_grant ~db ~scope_id:scope.id ~principal_kind:"profile"
    ~principal_id:"p1" ~capability:"read" ();
  insert_memory_grant ~db ~scope_id:scope.id ~principal_kind:"profile"
    ~principal_id:"p2" ~capability:"admin" ();
  insert_memory_grant ~db ~scope_id:other_scope.id ~principal_kind:"profile"
    ~principal_id:"p1" ~capability:"delete" ();
  Alcotest.(check (list string))
    "matching capabilities only" [ "read"; "write" ]
    (Memory.resolve_grants ~db ~scope_id:scope.id ~principal_kind:"profile"
       ~principal_id:"p1")

let test_resolve_grants_excludes_expired_grants () =
  let db = Memory.init ~db_path:":memory:" () in
  let scope = Memory.create_scope ~db ~kind:"room" ~key:"r1" () in
  insert_memory_grant ~db ~scope_id:scope.id ~principal_kind:"profile"
    ~principal_id:"p1" ~capability:"read" ~expires_at:"2999-01-01 00:00:00" ();
  insert_memory_grant ~db ~scope_id:scope.id ~principal_kind:"profile"
    ~principal_id:"p1" ~capability:"write" ~expires_at:"1970-01-01 00:00:00" ();
  Alcotest.(check (list string))
    "active capabilities only" [ "read" ]
    (Memory.resolve_grants ~db ~scope_id:scope.id ~principal_kind:"profile"
       ~principal_id:"p1")

let test_resolve_grants_excludes_revoked_grants () =
  let db = Memory.init ~db_path:":memory:" () in
  exec_exn db "ALTER TABLE memory_grants ADD COLUMN revoked_at TEXT";
  let scope = Memory.create_scope ~db ~kind:"room" ~key:"r1" () in
  insert_memory_grant ~db ~scope_id:scope.id ~principal_kind:"profile"
    ~principal_id:"p1" ~capability:"read" ();
  insert_memory_grant ~db ~scope_id:scope.id ~principal_kind:"profile"
    ~principal_id:"p1" ~capability:"write" ~revoked_at:"2026-01-01 00:00:00" ();
  Alcotest.(check (list string))
    "non-revoked capabilities only" [ "read" ]
    (Memory.resolve_grants ~db ~scope_id:scope.id ~principal_kind:"profile"
       ~principal_id:"p1")

let test_resolve_grants_no_match_denies_access () =
  let db = Memory.init ~db_path:":memory:" () in
  let parent = Memory.create_scope ~db ~kind:"room" ~key:"parent" () in
  let child =
    Memory.create_scope ~db ~kind:"thread" ~key:"child"
      ~parent_scope_id:parent.id ()
  in
  insert_memory_grant ~db ~scope_id:parent.id ~principal_kind:"profile"
    ~principal_id:"p1" ~capability:"read" ();
  Alcotest.(check (list string))
    "no direct child grant" []
    (Memory.resolve_grants ~db ~scope_id:child.id ~principal_kind:"profile"
       ~principal_id:"p1")

(* --- room profile API tests --- *)

let test_insert_and_get_room_profile () =
  let db = Memory.init ~db_path:":memory:" () in
  let id = Memory.insert_room_profile ~db ~name:"default" in
  Alcotest.(check bool) "id > 0" true (id > 0);
  let p = Memory.get_room_profile ~db ~id in
  match p with
  | None -> Alcotest.fail "expected profile"
  | Some p ->
      Alcotest.(check int) "id matches" id p.id;
      Alcotest.(check string) "name" "default" p.name

let test_insert_room_profile_unique_constraint () =
  let db = Memory.init ~db_path:":memory:" () in
  let _id = Memory.insert_room_profile ~db ~name:"dup" in
  match
    try `Ok (Memory.insert_room_profile ~db ~name:"dup")
    with Failure _ -> `Fail
  with
  | `Fail -> ()
  | `Ok _ -> Alcotest.fail "expected duplicate name to fail"

let test_get_room_profile_by_name () =
  let db = Memory.init ~db_path:":memory:" () in
  let id = Memory.insert_room_profile ~db ~name:"work" in
  let p = Memory.get_room_profile_by_name ~db ~name:"work" in
  match p with
  | None -> Alcotest.fail "expected profile by name"
  | Some p ->
      Alcotest.(check int) "id matches" id p.id;
      Alcotest.(check string) "name" "work" p.name

let test_list_room_profiles () =
  let db = Memory.init ~db_path:":memory:" () in
  let _ = Memory.insert_room_profile ~db ~name:"a" in
  let _ = Memory.insert_room_profile ~db ~name:"b" in
  let _ = Memory.insert_room_profile ~db ~name:"c" in
  let profiles = Memory.list_room_profiles ~db in
  Alcotest.(check int) "three profiles" 3 (List.length profiles);
  let names = List.map (fun (p : Memory.room_profile) -> p.name) profiles in
  Alcotest.(check (list string)) "in order" [ "a"; "b"; "c" ] names

let test_delete_room_profile_cascades_bindings () =
  let db = Memory.init ~db_path:":memory:" () in
  let pid = Memory.insert_room_profile ~db ~name:"to-delete" in
  Memory.upsert_room_profile_binding ~db ~room_id:"room1" ~profile_id:pid;
  ignore (Memory.delete_room_profile ~db ~id:pid);
  Alcotest.(check bool)
    "profile gone" true
    (Memory.get_room_profile ~db ~id:pid = None);
  Alcotest.(check bool)
    "binding cascade-deleted" true
    (Memory.get_room_profile_binding ~db ~room_id:"room1" = None)

let test_upsert_room_profile_binding () =
  let db = Memory.init ~db_path:":memory:" () in
  let p1 = Memory.insert_room_profile ~db ~name:"p1" in
  let p2 = Memory.insert_room_profile ~db ~name:"p2" in
  Memory.upsert_room_profile_binding ~db ~room_id:"r1" ~profile_id:p1;
  let b = Memory.get_room_profile_binding ~db ~room_id:"r1" in
  match b with
  | None -> Alcotest.fail "expected binding"
  | Some b -> (
      Alcotest.(check int) "bound to p1" p1 b.profile_id;
      (* rebind to p2 *)
      Memory.upsert_room_profile_binding ~db ~room_id:"r1" ~profile_id:p2;
      let b2 = Memory.get_room_profile_binding ~db ~room_id:"r1" in
      match b2 with
      | None -> Alcotest.fail "expected binding after rebind"
      | Some b2 -> Alcotest.(check int) "rebound to p2" p2 b2.profile_id)

let test_room_profile_binding_unique_constraint () =
  let db = Memory.init ~db_path:":memory:" () in
  let p1 = Memory.insert_room_profile ~db ~name:"p1" in
  let p2 = Memory.insert_room_profile ~db ~name:"p2" in
  Memory.upsert_room_profile_binding ~db ~room_id:"r1" ~profile_id:p1;
  (* Bind a different room to p1 -- should replace the r1 binding (1:1) *)
  Memory.upsert_room_profile_binding ~db ~room_id:"r2" ~profile_id:p1;
  Alcotest.(check bool)
    "r1 unbound" true
    (Memory.get_room_profile_binding ~db ~room_id:"r1" = None);
  (match Memory.get_room_profile_binding ~db ~room_id:"r2" with
  | None -> Alcotest.fail "expected r2 binding"
  | Some b -> Alcotest.(check int) "r2 bound to p1" p1 b.profile_id);
  (* Rebind r2 to p2 -- old binding replaced *)
  Memory.upsert_room_profile_binding ~db ~room_id:"r2" ~profile_id:p2;
  match Memory.get_room_profile_binding ~db ~room_id:"r2" with
  | None -> Alcotest.fail "expected r2 binding after rebind"
  | Some b -> Alcotest.(check int) "r2 rebound to p2" p2 b.profile_id

let test_get_room_profile_for_room () =
  let db = Memory.init ~db_path:":memory:" () in
  let pid = Memory.insert_room_profile ~db ~name:"my-profile" in
  Memory.upsert_room_profile_binding ~db ~room_id:"room-x" ~profile_id:pid;
  let p = Memory.get_room_profile_for_room ~db ~room_id:"room-x" in
  match p with
  | None -> Alcotest.fail "expected profile for room"
  | Some p -> Alcotest.(check string) "name" "my-profile" p.name

let test_get_room_profile_for_room_no_binding () =
  let db = Memory.init ~db_path:":memory:" () in
  let p = Memory.get_room_profile_for_room ~db ~room_id:"unbound" in
  Alcotest.(check bool) "no profile" true (p = None)

let test_list_rooms_for_profile_one_to_one () =
  let db = Memory.init ~db_path:":memory:" () in
  let pid = Memory.insert_room_profile ~db ~name:"shared" in
  Memory.upsert_room_profile_binding ~db ~room_id:"r1" ~profile_id:pid;
  (* With 1:1 cardinality, re-binding the same profile to r2 unbinds r1 *)
  Memory.upsert_room_profile_binding ~db ~room_id:"r2" ~profile_id:pid;
  Alcotest.(check bool)
    "r1 unbound after rebind" true
    (Memory.get_room_profile_binding ~db ~room_id:"r1" = None);
  match Memory.get_room_profile_binding ~db ~room_id:"r2" with
  | None -> Alcotest.fail "expected r2 binding"
  | Some b -> Alcotest.(check int) "bound to pid" pid b.profile_id

let test_remove_room_profile_binding () =
  let db = Memory.init ~db_path:":memory:" () in
  let pid = Memory.insert_room_profile ~db ~name:"removable" in
  Memory.upsert_room_profile_binding ~db ~room_id:"r1" ~profile_id:pid;
  let ok = Memory.remove_room_profile_binding ~db ~room_id:"r1" in
  Alcotest.(check bool) "removed" true ok;
  let b = Memory.get_room_profile_binding ~db ~room_id:"r1" in
  Alcotest.(check bool) "gone" true (b = None)

let column_exists db table col =
  let r = ref false in
  let stmt =
    Sqlite3.prepare db (Printf.sprintf "PRAGMA table_info(%s)" table)
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        match Sqlite3.column stmt 1 with
        | Sqlite3.Data.TEXT s when s = col -> r := true
        | _ -> ()
      done);
  !r

let test_repair_missing_columns_restores_dropped_column () =
  (* exercise repair_missing_columns directly -- the reviewer found the
     previous test only called ensure_all_tables, which is a different code
     path.  repair_missing_columns handles ALTER TABLE ADD COLUMN idempotent
     repair; we verify it by dropping a known column and calling it. *)
  let db = Memory.init ~db_path:":memory:" () in
  (* Verify the column exists after init *)
  Alcotest.(check bool)
    "debug_enabled present after init" true
    (column_exists db "session_state" "debug_enabled");
  (* Drop the column via table rebuild (SQLite has no DROP COLUMN < 3.35) *)
  exec_exn db "CREATE TABLE session_state_backup AS SELECT * FROM session_state";
  exec_exn db "DROP TABLE session_state";
  exec_exn db
    "CREATE TABLE session_state (session_key TEXT PRIMARY KEY, turn TEXT NOT \
     NULL DEFAULT 'user', channel TEXT, channel_id TEXT, response_sent_at \
     TEXT, last_active TEXT NOT NULL DEFAULT (datetime('now')), \
     keepalive_enabled INTEGER NOT NULL DEFAULT 0, heartbeat_enabled INTEGER \
     NOT NULL DEFAULT 0, model_override TEXT DEFAULT NULL, effective_cwd TEXT \
     DEFAULT NULL, CHECK ((channel IS NULL) = (channel_id IS NULL)))";
  exec_exn db
    "INSERT INTO session_state (session_key, turn, channel, channel_id, \
     response_sent_at, last_active, keepalive_enabled, heartbeat_enabled, \
     model_override, effective_cwd) SELECT session_key, turn, channel, \
     channel_id, response_sent_at, last_active, keepalive_enabled, \
     heartbeat_enabled, model_override, effective_cwd FROM \
     session_state_backup";
  exec_exn db "DROP TABLE session_state_backup";
  Alcotest.(check bool)
    "debug_enabled gone after rebuild" false
    (column_exists db "session_state" "debug_enabled");
  (* Call repair_missing_columns directly *)
  Memory.repair_missing_columns db;
  Alcotest.(check bool)
    "debug_enabled restored by repair_missing_columns" true
    (column_exists db "session_state" "debug_enabled")

let test_repair_legacy_room_profile_tables () =
  with_temp_db (fun path ->
      let db = Sqlite3.db_open path in
      exec_exn db "CREATE TABLE schema_version (version INTEGER NOT NULL)";
      exec_exn db
        (Printf.sprintf "INSERT INTO schema_version (version) VALUES (%d)"
           Memory.schema_version);
      exec_exn db
        "CREATE TABLE room_profiles (id TEXT PRIMARY KEY, model TEXT NOT NULL, \
         system_prompt TEXT NOT NULL DEFAULT '', max_tool_iterations INTEGER \
         NOT NULL DEFAULT 10, created_at TEXT NOT NULL DEFAULT \
         (datetime('now')), deleted_at TEXT DEFAULT NULL)";
      exec_exn db
        "CREATE TABLE room_profile_bindings (id INTEGER PRIMARY KEY \
         AUTOINCREMENT, room TEXT NOT NULL, profile_id TEXT NOT NULL, active \
         INTEGER NOT NULL DEFAULT 1, created_at TEXT NOT NULL DEFAULT \
         (datetime('now')))";
      exec_exn db
        "INSERT INTO room_profiles (id, model, system_prompt) VALUES \
         ('default', 'openai:gpt-5.4', '')";
      exec_exn db
        "INSERT INTO room_profile_bindings (room, profile_id, active) VALUES \
         ('teams-room', 'default', 1)";
      ignore (Sqlite3.db_close db);
      let db = Memory.init ~db_path:path () in
      Alcotest.(check bool)
        "room_id restored" true
        (column_exists db "room_profile_bindings" "room_id");
      Alcotest.(check bool)
        "name restored" true
        (column_exists db "room_profiles" "name");
      match Memory.get_room_profile_binding ~db ~room_id:"teams-room" with
      | None -> Alcotest.fail "expected migrated binding"
      | Some binding -> (
          match Memory.get_room_profile ~db ~id:binding.profile_id with
          | None -> Alcotest.fail "expected migrated profile"
          | Some profile ->
              Alcotest.(check string) "profile name" "default" profile.name))

let test_upsert_room_profile_binding_orphan_rejection () =
  let db = Memory.init ~db_path:":memory:" () in
  (* Binding to a nonexistent profile_id must fail *)
  match
    try
      Memory.upsert_room_profile_binding ~db ~room_id:"r1" ~profile_id:9999;
      `Ok
    with Failure _ -> `Fail
  with
  | `Fail -> ()
  | `Ok -> Alcotest.fail "expected orphan binding to fail"

(* --- visibility tests --- *)

let test_visibility_type_roundtrip () =
  Alcotest.(check string)
    "public" "public"
    (Memory.visibility_to_string Memory.Public);
  Alcotest.(check string)
    "private" "private"
    (Memory.visibility_to_string Memory.Private);
  Alcotest.(check string)
    "team" "team"
    (Memory.visibility_to_string Memory.Team);
  Alcotest.(check bool)
    "public parses" true
    (Memory.visibility_of_string "public" = Memory.Public);
  Alcotest.(check bool)
    "private parses" true
    (Memory.visibility_of_string "private" = Memory.Private);
  Alcotest.(check bool)
    "team parses" true
    (Memory.visibility_of_string "team" = Memory.Team);
  (match Memory.visibility_of_string_opt "public" with
  | Some Memory.Public -> ()
  | _ -> Alcotest.fail "expected Some Public");
  match Memory.visibility_of_string_opt "invalid" with
  | None -> ()
  | _ -> Alcotest.fail "expected None"

let test_upsert_with_visibility () =
  let db = Memory.init ~db_path:":memory:" () in
  let profile_id = Memory.insert_room_profile ~db ~name:"test-profile" in
  let scope =
    Memory.create_scope ~db ~kind:"room" ~key:"room-vis" ~profile_id
      ~provenance:"test" ()
  in
  (* Default visibility is public *)
  let m1 =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"pub-note"
      ~content:"public content" ~provenance:"test" ()
  in
  Alcotest.(check bool) "default is public" true (m1.visibility = Memory.Public);
  (* Explicit private *)
  let m2 =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"priv-note"
      ~content:"private content" ~provenance:"test" ~visibility:Memory.Private
      ()
  in
  Alcotest.(check bool) "private stored" true (m2.visibility = Memory.Private);
  (* Explicit team *)
  let m3 =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"team-note"
      ~content:"team content" ~provenance:"test" ~visibility:Memory.Team ()
  in
  Alcotest.(check bool) "team stored" true (m3.visibility = Memory.Team);
  (* Upsert changes visibility *)
  let m1_upd =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"pub-note"
      ~content:"now private" ~provenance:"test" ~visibility:Memory.Private ()
  in
  Alcotest.(check bool)
    "visibility updated on upsert" true
    (m1_upd.visibility = Memory.Private)

let test_query_by_visibility () =
  let db = Memory.init ~db_path:":memory:" () in
  let profile_id = Memory.insert_room_profile ~db ~name:"test-profile" in
  let scope =
    Memory.create_scope ~db ~kind:"room" ~key:"room-qvis" ~profile_id
      ~provenance:"test" ()
  in
  ignore
    (Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"pub"
       ~content:"public" ~provenance:"test" ~visibility:Memory.Public ());
  ignore
    (Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"priv"
       ~content:"private" ~provenance:"test" ~visibility:Memory.Private ());
  ignore
    (Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"team"
       ~content:"team" ~provenance:"test" ~visibility:Memory.Team ());
  (* Query all *)
  let all =
    Memory.query_scoped_memories ~db ~scope_kind:"room" ~scope_key:"room-qvis"
      ~limit:100 ()
  in
  Alcotest.(check int) "all memories" 3 (List.length all);
  (* Query by visibility *)
  let public_only =
    Memory.query_scoped_memories ~db ~scope_kind:"room" ~scope_key:"room-qvis"
      ~visibility:Memory.Public ~limit:100 ()
  in
  Alcotest.(check int) "public only" 1 (List.length public_only);
  let team_only =
    Memory.query_scoped_memories ~db ~scope_kind:"room" ~scope_key:"room-qvis"
      ~visibility:Memory.Team ~limit:100 ()
  in
  Alcotest.(check int) "team only" 1 (List.length team_only)

let test_team_grants_crud () =
  let db = Memory.init ~db_path:":memory:" () in
  let profile_id = Memory.insert_room_profile ~db ~name:"test-profile" in
  let scope =
    Memory.create_scope ~db ~kind:"room" ~key:"room-tg" ~profile_id
      ~provenance:"test" ()
  in
  let mem =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"team-mem"
      ~content:"team content" ~provenance:"test" ~visibility:Memory.Team ()
  in
  (* No grants initially *)
  let grants = Memory.list_team_grants ~db ~memory_id:mem.id in
  Alcotest.(check int) "no initial grants" 0 (List.length grants);
  Alcotest.(check bool)
    "no grant check" false
    (Memory.has_team_grant ~db ~memory_id:mem.id ~principal_kind:"user"
       ~principal_id:"alice");
  (* Add grant *)
  Alcotest.(check bool)
    "add grant" true
    (Memory.add_team_grant ~db ~memory_id:mem.id ~principal_kind:"user"
       ~principal_id:"alice");
  Alcotest.(check bool)
    "has grant" true
    (Memory.has_team_grant ~db ~memory_id:mem.id ~principal_kind:"user"
       ~principal_id:"alice");
  Alcotest.(check bool)
    "other has no grant" false
    (Memory.has_team_grant ~db ~memory_id:mem.id ~principal_kind:"user"
       ~principal_id:"bob");
  (* List grants *)
  let grants = Memory.list_team_grants ~db ~memory_id:mem.id in
  Alcotest.(check int) "one grant" 1 (List.length grants);
  (* Duplicate add returns false *)
  Alcotest.(check bool)
    "duplicate add" false
    (Memory.add_team_grant ~db ~memory_id:mem.id ~principal_kind:"user"
       ~principal_id:"alice");
  (* Remove grant *)
  Alcotest.(check bool)
    "remove grant" true
    (Memory.remove_team_grant ~db ~memory_id:mem.id ~principal_kind:"user"
       ~principal_id:"alice");
  Alcotest.(check bool)
    "grant gone" false
    (Memory.has_team_grant ~db ~memory_id:mem.id ~principal_kind:"user"
       ~principal_id:"alice");
  (* Remove nonexistent returns false *)
  Alcotest.(check bool)
    "remove nonexistent" false
    (Memory.remove_team_grant ~db ~memory_id:mem.id ~principal_kind:"user"
       ~principal_id:"alice")

let test_can_see_memory_logic () =
  let db = Memory.init ~db_path:":memory:" () in
  let profile_id = Memory.insert_room_profile ~db ~name:"test-profile" in
  let scope =
    Memory.create_scope ~db ~kind:"room" ~key:"room-cansee" ~profile_id
      ~provenance:"test" ()
  in
  (* Public memory: everyone can see *)
  let pub_mem =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"pub"
      ~content:"public" ~provenance:"test" ~visibility:Memory.Public ()
  in
  Alcotest.(check bool)
    "public visible to all" true
    (Memory.can_see_memory ~db ~scoped_mem:pub_mem ~principal_kind:"user"
       ~principal_id:"anyone"
       ~scope_profile_id:(Some (string_of_int profile_id)));
  (* Private memory: only owner can see *)
  let priv_mem =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"priv"
      ~content:"private" ~provenance:"test" ~visibility:Memory.Private ()
  in
  Alcotest.(check bool)
    "private visible to owner" true
    (Memory.can_see_memory ~db ~scoped_mem:priv_mem ~principal_kind:"user"
       ~principal_id:(string_of_int profile_id)
       ~scope_profile_id:(Some (string_of_int profile_id)));
  Alcotest.(check bool)
    "private not visible to other" false
    (Memory.can_see_memory ~db ~scoped_mem:priv_mem ~principal_kind:"user"
       ~principal_id:"other"
       ~scope_profile_id:(Some (string_of_int profile_id)));
  (* Team memory: only granted users can see *)
  let team_mem =
    Memory.upsert_scoped_memory ~db ~scope_id:scope.id ~reference:"team"
      ~content:"team" ~provenance:"test" ~visibility:Memory.Team ()
  in
  Alcotest.(check bool)
    "team not visible without grant" false
    (Memory.can_see_memory ~db ~scoped_mem:team_mem ~principal_kind:"user"
       ~principal_id:"alice"
       ~scope_profile_id:(Some (string_of_int profile_id)));
  ignore
    (Memory.add_team_grant ~db ~memory_id:team_mem.id ~principal_kind:"user"
       ~principal_id:"alice");
  Alcotest.(check bool)
    "team visible with grant" true
    (Memory.can_see_memory ~db ~scoped_mem:team_mem ~principal_kind:"user"
       ~principal_id:"alice"
       ~scope_profile_id:(Some (string_of_int profile_id)))

let suite =
  [
    Alcotest.test_case "init sets busy_timeout" `Quick
      test_init_sets_busy_timeout;
    Alcotest.test_case "init creates db" `Quick test_init_creates_db;
    Alcotest.test_case "init search enabled" `Quick test_init_search_enabled;
    Alcotest.test_case "init search disabled" `Quick test_init_search_disabled;
    Alcotest.test_case "init double call" `Quick test_init_double_call;
    Alcotest.test_case "init schema version is current" `Quick
      test_init_schema_version_is_current;
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
    Alcotest.test_case "thinking roundtrip" `Quick test_thinking_roundtrip;
    Alcotest.test_case "thinking None compat" `Quick test_thinking_none_compat;
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
    Alcotest.test_case "queue reclaim failed max retries" `Quick
      test_queue_reclaim_failed_max_retries;
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
    Alcotest.test_case "migrate v16 adds effective_cwd" `Quick
      test_migrate_v16_adds_effective_cwd;
    Alcotest.test_case "migrate v23 adds effective_cwd" `Quick
      test_migrate_v23_adds_effective_cwd;
    Alcotest.test_case "init rejects future schema version" `Quick
      test_init_rejects_future_schema_version;
    Alcotest.test_case "archive session captures messages" `Quick
      test_archive_session_captures_messages;
    Alcotest.test_case "archive session captures epochs" `Quick
      test_archive_session_captures_epochs;
    Alcotest.test_case "archive session captures metadata" `Quick
      test_archive_session_captures_metadata;
    Alcotest.test_case "archive session empty is noop" `Quick
      test_archive_session_empty_is_noop;
    Alcotest.test_case "archive session multiple accumulate" `Quick
      test_archive_session_multiple_accumulate;
    Alcotest.test_case "list archives for session" `Quick
      test_list_archives_for_session;
    Alcotest.test_case "list archive sessions" `Quick test_list_archive_sessions;
    Alcotest.test_case "load archive messages" `Quick test_load_archive_messages;
    Alcotest.test_case "get archive info" `Quick test_get_archive_info;
    Alcotest.test_case "load archive messages empty" `Quick
      test_load_archive_messages_empty;
    Alcotest.test_case "session cwd get set" `Quick test_session_cwd_get_set;
    Alcotest.test_case "agent create with cwd" `Quick test_agent_create_with_cwd;
    Alcotest.test_case "session info includes cwd" `Quick
      test_session_info_includes_cwd;
    Alcotest.test_case "B656 get_session_channel empty strings -> None" `Quick
      test_get_session_channel_empty_strings_are_none;
    Alcotest.test_case "export snapshot roundtrip" `Quick
      test_export_snapshot_roundtrip;
    Alcotest.test_case "export snapshot metadata" `Quick
      test_export_snapshot_metadata;
    Alcotest.test_case "export snapshot empty" `Quick test_export_snapshot_empty;
    Alcotest.test_case "import snapshot legacy version field" `Quick
      test_import_snapshot_legacy_version_field;
    Alcotest.test_case "import snapshot rejects bad version" `Quick
      test_import_snapshot_rejects_bad_version;
    Alcotest.test_case "import snapshot upserts" `Quick
      test_import_snapshot_upserts;
    Alcotest.test_case "init creates room profile tables" `Quick
      test_init_creates_room_profile_tables;
    Alcotest.test_case "ensure_all_tables creates room profiles" `Quick
      test_ensure_all_tables_creates_room_profiles;
    Alcotest.test_case "migrate v31 to current creates room profiles" `Quick
      test_migrate_v31_to_current_creates_room_profiles;
    Alcotest.test_case "migrate v32 adds origin columns" `Quick
      test_migrate_v32_adds_origin_columns;
    Alcotest.test_case
      "migrate v35 adds request stats profile columns before index" `Quick
      test_migrate_v35_adds_request_stats_profile_columns_before_index;
    Alcotest.test_case "init creates scoped memory tables" `Quick
      test_init_creates_scoped_memory_tables;
    Alcotest.test_case "scoped memory schema shape" `Quick
      test_scoped_memory_schema_shape;
    Alcotest.test_case "scoped memory constraints and cascade" `Quick
      test_scoped_memory_constraints_and_cascade;
    Alcotest.test_case "memory grants require admin" `Quick
      test_memory_grants_require_admin_for_create_and_revoke;
    Alcotest.test_case "scoped memory migration and repair paths" `Quick
      test_scoped_memory_schema_migration_and_repair_paths;
    Alcotest.test_case "scoped memory double init idempotent" `Quick
      test_scoped_memory_double_init_is_idempotent;
    Alcotest.test_case "legacy memory scope seeded read only" `Quick
      test_legacy_memory_scope_seeded_read_only;
    Alcotest.test_case "legacy memory scope idempotent" `Quick
      test_legacy_memory_scope_is_idempotent;
    Alcotest.test_case "legacy memory fallback preserves reads" `Quick
      test_legacy_memory_fallback_preserves_existing_reads;
    Alcotest.test_case "scoped memory scope CRUD roundtrip" `Quick
      test_scoped_memory_scope_crud_roundtrip;
    Alcotest.test_case "scoped memory upsert is idempotent" `Quick
      test_scoped_memory_upsert_is_idempotent;
    Alcotest.test_case "scoped memory query filters and pagination" `Quick
      test_scoped_memory_query_filters_and_pagination;
    Alcotest.test_case "scoped memory delete and boundaries" `Quick
      test_scoped_memory_delete_and_boundaries;
    Alcotest.test_case "resolve grants single grant" `Quick
      test_resolve_grants_single_grant;
    Alcotest.test_case "resolve grants merges matching direct grants" `Quick
      test_resolve_grants_merges_matching_direct_grants;
    Alcotest.test_case "resolve grants excludes expired grants" `Quick
      test_resolve_grants_excludes_expired_grants;
    Alcotest.test_case "resolve grants excludes revoked grants" `Quick
      test_resolve_grants_excludes_revoked_grants;
    Alcotest.test_case "resolve grants no match denies access" `Quick
      test_resolve_grants_no_match_denies_access;
    Alcotest.test_case "insert and get room profile" `Quick
      test_insert_and_get_room_profile;
    Alcotest.test_case "insert room profile unique constraint" `Quick
      test_insert_room_profile_unique_constraint;
    Alcotest.test_case "get room profile by name" `Quick
      test_get_room_profile_by_name;
    Alcotest.test_case "list room profiles" `Quick test_list_room_profiles;
    Alcotest.test_case "delete room profile cascades bindings" `Quick
      test_delete_room_profile_cascades_bindings;
    Alcotest.test_case "upsert room profile binding" `Quick
      test_upsert_room_profile_binding;
    Alcotest.test_case "room profile binding unique constraint" `Quick
      test_room_profile_binding_unique_constraint;
    Alcotest.test_case "get room profile for room" `Quick
      test_get_room_profile_for_room;
    Alcotest.test_case "get room profile for room no binding" `Quick
      test_get_room_profile_for_room_no_binding;
    Alcotest.test_case "list rooms for profile 1:1" `Quick
      test_list_rooms_for_profile_one_to_one;
    Alcotest.test_case "remove room profile binding" `Quick
      test_remove_room_profile_binding;
    Alcotest.test_case "repair missing columns restores dropped column" `Quick
      test_repair_missing_columns_restores_dropped_column;
    Alcotest.test_case "repair legacy room profile tables" `Quick
      test_repair_legacy_room_profile_tables;
    Alcotest.test_case "upsert room profile binding orphan rejection" `Quick
      test_upsert_room_profile_binding_orphan_rejection;
    (* Visibility tests *)
    Alcotest.test_case "visibility type roundtrip" `Quick
      test_visibility_type_roundtrip;
    Alcotest.test_case "upsert with visibility" `Quick
      test_upsert_with_visibility;
    Alcotest.test_case "query by visibility" `Quick test_query_by_visibility;
    Alcotest.test_case "team grants CRUD" `Quick test_team_grants_crud;
    Alcotest.test_case "can see memory logic" `Quick test_can_see_memory_logic;
  ]

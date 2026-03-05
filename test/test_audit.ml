let test_audit_schema_init () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  (* Should not fail on double init *)
  Audit.init_schema db

let test_audit_log_and_query () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  Audit.log ~db (ChatMessage {
    session_key = "s1"; role = "user"; content_preview = "hello world" });
  Audit.log ~db (ToolInvocation {
    session_key = "s1"; tool_name = "shell_exec";
    risk_level = "high"; args_preview = "{\"command\":\"ls\"}" });
  Audit.log ~db (ToolResult {
    session_key = "s1"; tool_name = "shell_exec"; success = true });
  Audit.log ~db (DaemonEvent {
    action = "start"; details = "pid=1234" });
  let rows = Audit.query ~db ~limit:10 () in
  Alcotest.(check int) "4 audit entries" 4 (List.length rows)

let test_audit_query_filter () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  Audit.log ~db (ChatMessage {
    session_key = "s1"; role = "user"; content_preview = "msg1" });
  Audit.log ~db (ChatMessage {
    session_key = "s2"; role = "user"; content_preview = "msg2" });
  Audit.log ~db (ToolInvocation {
    session_key = "s1"; tool_name = "file_read";
    risk_level = "low"; args_preview = "{}" });
  let chat_rows = Audit.query ~db ~event_type:"chat_message" ~limit:10 () in
  Alcotest.(check int) "2 chat entries" 2 (List.length chat_rows);
  let s1_rows = Audit.query ~db ~session_key:"s1" ~limit:10 () in
  Alcotest.(check int) "2 s1 entries" 2 (List.length s1_rows)

let test_audit_config_change () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  Audit.log ~db (ConfigChange {
    field = "model"; old_value = "gpt-3.5"; new_value = "gpt-4" });
  let rows = Audit.query ~db ~event_type:"config_change" ~limit:10 () in
  Alcotest.(check int) "1 config change" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.(check bool) "details contains field" true
    (match row.details with
     | Some d -> String.length d > 0
       && let re = Str.regexp_string "model" in
          (try ignore (Str.search_forward re d 0); true with Not_found -> false)
     | None -> false)

let suite =
  [
    Alcotest.test_case "audit schema init" `Quick test_audit_schema_init;
    Alcotest.test_case "audit log and query" `Quick test_audit_log_and_query;
    Alcotest.test_case "audit query filter" `Quick test_audit_query_filter;
    Alcotest.test_case "audit config change" `Quick test_audit_config_change;
  ]

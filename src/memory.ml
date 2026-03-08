let schema_version = 2

let exec_exn db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "SQLite error: %s (sql: %s)" (Sqlite3.Rc.to_string rc)
           sql)

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

let set_schema_version db version =
  let stmt = Sqlite3.prepare db "UPDATE schema_version SET version = ?" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int version)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "SQLite error: %s (sql: UPDATE schema_version ...)"
               (Sqlite3.Rc.to_string rc)))

let init_session_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS session_state (\n\
    \     session_key TEXT PRIMARY KEY,\n\
    \     turn TEXT NOT NULL DEFAULT 'user',\n\
    \     channel TEXT,\n\
    \     channel_id TEXT,\n\
    \     response_sent_at TEXT,\n\
    \     last_active TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )";
  exec_exn db
    "CREATE TABLE IF NOT EXISTS discord_resume_state (\n\
    \     id INTEGER PRIMARY KEY CHECK (id = 1),\n\
    \     session_id TEXT NOT NULL,\n\
    \     seq INTEGER NOT NULL,\n\
    \     resume_gateway_url TEXT NOT NULL,\n\
    \     updated_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )"

let migrate_schema db current_version =
  match current_version with
  | 0 ->
      init_session_schema db;
      exec_exn db
        (Printf.sprintf "INSERT INTO schema_version (version) VALUES (%d)"
           schema_version)
  | 1 ->
      init_session_schema db;
      set_schema_version db 2
  | n when n = schema_version -> ()
  | n ->
      failwith
        (Printf.sprintf "Unsupported schema version %d (current=%d)" n
           schema_version)

let init_core_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS core_memories (\n\
    \     key TEXT PRIMARY KEY,\n\
    \     content TEXT NOT NULL,\n\
    \     category TEXT NOT NULL DEFAULT 'general',\n\
    \     created_at INTEGER NOT NULL DEFAULT (strftime('%s','now')),\n\
    \     updated_at INTEGER NOT NULL DEFAULT (strftime('%s','now'))\n\
    \   )";
  exec_exn db
    "CREATE VIRTUAL TABLE IF NOT EXISTS core_memories_fts USING fts5(key, \
     content, category, content='core_memories', content_rowid='rowid')";
  exec_exn db
    "CREATE TRIGGER IF NOT EXISTS core_memories_ai AFTER INSERT ON \
     core_memories BEGIN INSERT INTO core_memories_fts(rowid, key, content, \
     category) VALUES (new.rowid, new.key, new.content, new.category); END";
  exec_exn db
    "CREATE TRIGGER IF NOT EXISTS core_memories_au AFTER UPDATE ON \
     core_memories BEGIN INSERT INTO core_memories_fts(core_memories_fts, \
     rowid, key, content, category) VALUES('delete', old.rowid, old.key, \
     old.content, old.category); INSERT INTO core_memories_fts(rowid, key, \
     content, category) VALUES (new.rowid, new.key, new.content, \
     new.category); END";
  exec_exn db
    "CREATE TRIGGER IF NOT EXISTS core_memories_ad AFTER DELETE ON \
     core_memories BEGIN INSERT INTO core_memories_fts(core_memories_fts, \
     rowid, key, content, category) VALUES('delete', old.rowid, old.key, \
     old.content, old.category); END"

let init ~db_path ?(search_enabled = false) () =
  let db = Sqlite3.db_open db_path in
  exec_exn db
    "CREATE TABLE IF NOT EXISTS schema_version (\n\
    \     version INTEGER NOT NULL\n\
    \   )";
  let current_version =
    query_single_int db "SELECT version FROM schema_version"
  in
  exec_exn db
    "CREATE TABLE IF NOT EXISTS messages (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     session_key TEXT NOT NULL,\n\
    \     role TEXT NOT NULL,\n\
    \     content TEXT NOT NULL,\n\
    \     tool_call_id TEXT,\n\
    \     tool_name TEXT,\n\
    \     tool_calls_json TEXT,\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )";
  migrate_schema db current_version;
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_messages_session_key ON messages \
     (session_key)";
  if search_enabled then begin
    exec_exn db
      "CREATE VIRTUAL TABLE IF NOT EXISTS messages_fts USING fts5(content, \
       session_key, content=messages, content_rowid=id)";
    exec_exn db
      "CREATE TRIGGER IF NOT EXISTS messages_ai AFTER INSERT ON messages BEGIN \
       INSERT INTO messages_fts(rowid, content, session_key) VALUES (new.id, \
       new.content, new.session_key); END";
    exec_exn db
      "CREATE TRIGGER IF NOT EXISTS messages_ad AFTER DELETE ON messages BEGIN \
       INSERT INTO messages_fts(messages_fts, rowid, content, session_key) \
       VALUES('delete', old.id, old.content, old.session_key); END"
  end;
  init_core_schema db;
  db

let store_message ~db ~session_key (msg : Provider.message) =
  let tool_calls_json =
    if msg.tool_calls = [] then None
    else
      Some
        (Yojson.Safe.to_string
           (`List
              (List.map
                 (fun (tc : Provider.tool_call) ->
                   `Assoc
                     [
                       ("id", `String tc.id);
                       ("function_name", `String tc.function_name);
                       ("arguments", `String tc.arguments);
                     ])
                 msg.tool_calls)))
  in
  let sql =
    "INSERT INTO messages (session_key, role, content, tool_call_id, \
     tool_name, tool_calls_json) VALUES (?, ?, ?, ?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT msg.role));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT msg.content));
  ignore
    (Sqlite3.bind stmt 4
       (match msg.tool_call_id with
       | Some id -> Sqlite3.Data.TEXT id
       | None -> Sqlite3.Data.NULL));
  ignore
    (Sqlite3.bind stmt 5
       (match msg.name with
       | Some n -> Sqlite3.Data.TEXT n
       | None -> Sqlite3.Data.NULL));
  ignore
    (Sqlite3.bind stmt 6
       (match tool_calls_json with
       | Some j -> Sqlite3.Data.TEXT j
       | None -> Sqlite3.Data.NULL));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Failed to store message: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let load_history ~db ~session_key =
  let sql =
    "SELECT role, content, tool_call_id, tool_name, tool_calls_json FROM \
     messages WHERE session_key = ? ORDER BY id ASC"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  let messages = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let role =
      match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let content =
      match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let tool_call_id =
      match Sqlite3.column stmt 2 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let name =
      match Sqlite3.column stmt 3 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let tool_calls =
      match Sqlite3.column stmt 4 with
      | Sqlite3.Data.TEXT s -> (
          try
            let json = Yojson.Safe.from_string s in
            let open Yojson.Safe.Util in
            json |> to_list
            |> List.map (fun tc ->
                {
                  Provider.id = tc |> member "id" |> to_string;
                  function_name = tc |> member "function_name" |> to_string;
                  arguments = tc |> member "arguments" |> to_string;
                })
          with _ -> [])
      | _ -> []
    in
    messages :=
      { Provider.role; content; tool_calls; tool_call_id; name } :: !messages
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !messages

let replace_session_messages ~db ~session_key (messages : Provider.message list)
    =
  exec_exn db "BEGIN TRANSACTION";
  try
    let del_sql = "DELETE FROM messages WHERE session_key = ?" in
    let del_stmt = Sqlite3.prepare db del_sql in
    ignore (Sqlite3.bind del_stmt 1 (Sqlite3.Data.TEXT session_key));
    ignore (Sqlite3.step del_stmt);
    ignore (Sqlite3.finalize del_stmt);
    List.iter (fun msg -> store_message ~db ~session_key msg) messages;
    exec_exn db "COMMIT"
  with exn ->
    (try exec_exn db "ROLLBACK" with _ -> ());
    Logs.warn (fun m ->
        m "Failed to replace session messages: %s" (Printexc.to_string exn))

let clear_session ~db ~session_key =
  let clear sql =
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
        ignore (Sqlite3.step stmt))
  in
  clear "DELETE FROM messages WHERE session_key = ?";
  clear "DELETE FROM session_state WHERE session_key = ?"

let upsert_session_state ~db ~session_key ~turn ?channel ?channel_id
    ?response_sent_at () =
  let sql =
    "INSERT INTO session_state (session_key, turn, channel, channel_id, \
     response_sent_at, last_active) VALUES (?, ?, ?, ?, ?, datetime('now')) ON \
     CONFLICT(session_key) DO UPDATE SET turn = excluded.turn, channel = \
     COALESCE(excluded.channel, session_state.channel), channel_id = \
     COALESCE(excluded.channel_id, session_state.channel_id), response_sent_at \
     = excluded.response_sent_at, last_active = datetime('now')"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT turn));
  ignore
    (Sqlite3.bind stmt 3
       (match channel with
       | Some value -> Sqlite3.Data.TEXT value
       | None -> Sqlite3.Data.NULL));
  ignore
    (Sqlite3.bind stmt 4
       (match channel_id with
       | Some value -> Sqlite3.Data.TEXT value
       | None -> Sqlite3.Data.NULL));
  ignore
    (Sqlite3.bind stmt 5
       (match response_sent_at with
       | Some value -> Sqlite3.Data.TEXT value
       | None -> Sqlite3.Data.NULL));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Failed to upsert session state: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let mark_response_sent ~db ~session_key =
  let sql =
    "UPDATE session_state SET turn = 'user', response_sent_at = \
     datetime('now'), last_active = datetime('now') WHERE session_key = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Failed to mark response sent: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let load_pending_agent_sessions ~db ~max_age_seconds =
  let sql =
    "SELECT session_key, channel, channel_id FROM session_state WHERE turn = \
     'agent' AND response_sent_at IS NULL AND last_active >= datetime('now', \
     '-' || ? || ' seconds') ORDER BY last_active DESC"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int max_age_seconds)));
  let rows = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let get_opt index =
      match Sqlite3.column stmt index with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    match get_opt 0 with
    | Some session_key -> rows := (session_key, get_opt 1, get_opt 2) :: !rows
    | None -> ()
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !rows

let list_sessions ~db =
  let sql = "SELECT DISTINCT session_key FROM messages ORDER BY session_key" in
  let stmt = Sqlite3.prepare db sql in
  let keys = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    match Sqlite3.column stmt 0 with
    | Sqlite3.Data.TEXT s -> keys := s :: !keys
    | _ -> ()
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !keys

let cleanup_session ~db ~session_key ~max_messages ~max_age_days =
  if max_age_days > 0 then begin
    let sql =
      "DELETE FROM messages WHERE session_key = ? AND created_at < \
       datetime('now', '-' || ? || ' days')"
    in
    let stmt = Sqlite3.prepare db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
    ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int max_age_days)));
    ignore (Sqlite3.step stmt);
    ignore (Sqlite3.finalize stmt)
  end;
  if max_messages > 0 then begin
    let sql =
      "DELETE FROM messages WHERE session_key = ? AND id NOT IN (SELECT id \
       FROM messages WHERE session_key = ? ORDER BY id DESC LIMIT ?)"
    in
    let stmt = Sqlite3.prepare db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
    ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT session_key));
    ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int max_messages)));
    ignore (Sqlite3.step stmt);
    ignore (Sqlite3.finalize stmt)
  end

let cleanup_all ~db ~max_messages ~max_age_days =
  let sessions = list_sessions ~db in
  List.iter
    (fun session_key ->
      cleanup_session ~db ~session_key ~max_messages ~max_age_days)
    sessions

let store_core ~db ~key ~content ?(category = "general") () =
  let sql =
    "INSERT INTO core_memories (key, content, category, updated_at) VALUES (?, \
     ?, ?, strftime('%s','now')) ON CONFLICT(key) DO UPDATE SET content = \
     excluded.content, category = excluded.category, updated_at = \
     strftime('%s','now')"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT content));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT category));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Failed to store core memory: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let recall_core ~db ~query ~limit =
  let sql =
    "SELECT cm.key, cm.content, cm.category FROM core_memories cm JOIN \
     core_memories_fts f ON cm.rowid = f.rowid WHERE core_memories_fts MATCH ? \
     ORDER BY f.rank LIMIT ?"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT query));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int limit)));
  let results = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let key =
      match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let content =
      match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let category =
      match Sqlite3.column stmt 2 with
      | Sqlite3.Data.TEXT s -> s
      | _ -> "general"
    in
    results := (key, content, category) :: !results
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !results

let forget_core ~db ~key =
  let sql = "DELETE FROM core_memories WHERE key = ?" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key));
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with Sqlite3.Rc.DONE -> Sqlite3.changes db > 0 | _ -> false

let list_core ~db ?(category = "") () =
  let sql, has_category =
    if category = "" then
      ( "SELECT key, content, category FROM core_memories ORDER BY updated_at \
         DESC",
        false )
    else
      ( "SELECT key, content, category FROM core_memories WHERE category = ? \
         ORDER BY updated_at DESC",
        true )
  in
  let stmt = Sqlite3.prepare db sql in
  if has_category then ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT category));
  let results = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let key =
      match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let content =
      match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let cat =
      match Sqlite3.column stmt 2 with
      | Sqlite3.Data.TEXT s -> s
      | _ -> "general"
    in
    results := (key, content, cat) :: !results
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !results

let count_core ~db =
  let sql = "SELECT COUNT(*) FROM core_memories" in
  let stmt = Sqlite3.prepare db sql in
  let count =
    if Sqlite3.step stmt = Sqlite3.Rc.ROW then
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT n -> Int64.to_int n
      | _ -> 0
    else 0
  in
  ignore (Sqlite3.finalize stmt);
  count

let export_snapshot ~db ~path =
  let memories = list_core ~db () in
  let json =
    `Assoc
      [
        ("version", `Int 1);
        ( "memories",
          `List
            (List.map
               (fun (key, content, category) ->
                 `Assoc
                   [
                     ("key", `String key);
                     ("content", `String content);
                     ("category", `String category);
                   ])
               memories) );
      ]
  in
  let oc = open_out path in
  output_string oc (Yojson.Safe.to_string json);
  close_out oc

let import_snapshot ~db ~path =
  let ic = open_in path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let json = Yojson.Safe.from_string content in
  let open Yojson.Safe.Util in
  let memories = json |> member "memories" |> to_list in
  List.iter
    (fun m ->
      let key = m |> member "key" |> to_string in
      let content = m |> member "content" |> to_string in
      let category =
        try m |> member "category" |> to_string with _ -> "general"
      in
      store_core ~db ~key ~content ~category ())
    memories

let search ~db ~query ?session_key ~limit () =
  let sql, has_session =
    match session_key with
    | Some _ ->
        ( "SELECT m.role, m.content, m.tool_call_id, m.tool_name, \
           m.tool_calls_json FROM messages m JOIN messages_fts f ON m.id = \
           f.rowid WHERE messages_fts MATCH ? AND f.session_key = ? ORDER BY \
           f.rank LIMIT ?",
          true )
    | None ->
        ( "SELECT m.role, m.content, m.tool_call_id, m.tool_name, \
           m.tool_calls_json FROM messages m JOIN messages_fts f ON m.id = \
           f.rowid WHERE messages_fts MATCH ? ORDER BY f.rank LIMIT ?",
          false )
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT query));
  if has_session then begin
    ignore
      (Sqlite3.bind stmt 2
         (Sqlite3.Data.TEXT (match session_key with Some s -> s | None -> "")));
    ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int limit)))
  end
  else ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int limit)));
  let messages = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let role =
      match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let content =
      match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let tool_call_id =
      match Sqlite3.column stmt 2 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let name =
      match Sqlite3.column stmt 3 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let tool_calls =
      match Sqlite3.column stmt 4 with
      | Sqlite3.Data.TEXT s -> (
          try
            let json = Yojson.Safe.from_string s in
            let open Yojson.Safe.Util in
            json |> to_list
            |> List.map (fun tc ->
                {
                  Provider.id = tc |> member "id" |> to_string;
                  function_name = tc |> member "function_name" |> to_string;
                  arguments = tc |> member "arguments" |> to_string;
                })
          with _ -> [])
      | _ -> []
    in
    messages :=
      { Provider.role; content; tool_calls; tool_call_id; name } :: !messages
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !messages

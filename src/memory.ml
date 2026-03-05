let schema_version = 1

let init ~db_path ?(search_enabled = false) () =
  let db = Sqlite3.db_open db_path in
  let exec sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | rc ->
      failwith
        (Printf.sprintf "SQLite error: %s (sql: %s)" (Sqlite3.Rc.to_string rc)
           sql)
  in
  exec
    "CREATE TABLE IF NOT EXISTS schema_version (\n\
    \     version INTEGER NOT NULL\n\
    \   )";
  exec
    (Printf.sprintf
       "INSERT INTO schema_version (version)\n\
       \   SELECT %d WHERE NOT EXISTS (SELECT 1 FROM schema_version)"
       schema_version);
  exec
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
  exec
    "CREATE INDEX IF NOT EXISTS idx_messages_session_key ON messages \
     (session_key)";
  if search_enabled then begin
    exec
      "CREATE VIRTUAL TABLE IF NOT EXISTS messages_fts \
       USING fts5(content, session_key, content=messages, content_rowid=id)";
    exec
      "CREATE TRIGGER IF NOT EXISTS messages_ai AFTER INSERT ON messages BEGIN \
       INSERT INTO messages_fts(rowid, content, session_key) \
       VALUES (new.id, new.content, new.session_key); \
       END";
    exec
      "CREATE TRIGGER IF NOT EXISTS messages_ad AFTER DELETE ON messages BEGIN \
       INSERT INTO messages_fts(messages_fts, rowid, content, session_key) \
       VALUES('delete', old.id, old.content, old.session_key); \
       END"
  end;
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
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.TEXT s -> s
      | _ -> ""
    in
    let content =
      match Sqlite3.column stmt 1 with
      | Sqlite3.Data.TEXT s -> s
      | _ -> ""
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

let clear_session ~db ~session_key =
  let sql = "DELETE FROM messages WHERE session_key = ?" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt)

let list_sessions ~db =
  let sql = "SELECT DISTINCT session_key FROM messages ORDER BY session_key" in
  let stmt = Sqlite3.prepare db sql in
  let keys = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    (match Sqlite3.column stmt 0 with
     | Sqlite3.Data.TEXT s -> keys := s :: !keys
     | _ -> ())
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !keys

let cleanup_session ~db ~session_key ~max_messages ~max_age_days =
  if max_age_days > 0 then begin
    let sql =
      "DELETE FROM messages WHERE session_key = ? AND created_at < datetime('now', '-' || ? || ' days')"
    in
    let stmt = Sqlite3.prepare db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
    ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int max_age_days)));
    ignore (Sqlite3.step stmt);
    ignore (Sqlite3.finalize stmt)
  end;
  if max_messages > 0 then begin
    let sql =
      "DELETE FROM messages WHERE session_key = ? AND id NOT IN \
       (SELECT id FROM messages WHERE session_key = ? ORDER BY id DESC LIMIT ?)"
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
  List.iter (fun session_key ->
    cleanup_session ~db ~session_key ~max_messages ~max_age_days
  ) sessions

let search ~db ~query ?session_key ~limit () =
  let sql, has_session =
    match session_key with
    | Some _ ->
      ("SELECT m.role, m.content, m.tool_call_id, m.tool_name, m.tool_calls_json \
        FROM messages m JOIN messages_fts f ON m.id = f.rowid \
        WHERE messages_fts MATCH ? AND f.session_key = ? \
        ORDER BY f.rank LIMIT ?", true)
    | None ->
      ("SELECT m.role, m.content, m.tool_call_id, m.tool_name, m.tool_calls_json \
        FROM messages m JOIN messages_fts f ON m.id = f.rowid \
        WHERE messages_fts MATCH ? \
        ORDER BY f.rank LIMIT ?", false)
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT query));
  if has_session then begin
    ignore (Sqlite3.bind stmt 2
              (Sqlite3.Data.TEXT (match session_key with Some s -> s | None -> "")));
    ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int limit)))
  end else
    ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int limit)));
  let messages = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let role = match Sqlite3.column stmt 0 with
      | Sqlite3.Data.TEXT s -> s | _ -> "" in
    let content = match Sqlite3.column stmt 1 with
      | Sqlite3.Data.TEXT s -> s | _ -> "" in
    let tool_call_id = match Sqlite3.column stmt 2 with
      | Sqlite3.Data.TEXT s -> Some s | _ -> None in
    let name = match Sqlite3.column stmt 3 with
      | Sqlite3.Data.TEXT s -> Some s | _ -> None in
    let tool_calls = match Sqlite3.column stmt 4 with
      | Sqlite3.Data.TEXT s -> (
        try
          let json = Yojson.Safe.from_string s in
          let open Yojson.Safe.Util in
          json |> to_list
          |> List.map (fun tc ->
                 { Provider.id = tc |> member "id" |> to_string;
                   function_name = tc |> member "function_name" |> to_string;
                   arguments = tc |> member "arguments" |> to_string })
        with _ -> [])
      | _ -> []
    in
    messages :=
      { Provider.role; content; tool_calls; tool_call_id; name } :: !messages
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !messages

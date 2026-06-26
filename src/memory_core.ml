open Memory_types
open Memory_0_schema

let init ~db_path ?(search_enabled = false) () =
  let db = Sqlite3.db_open db_path in
  ignore (Sqlite3.exec db "PRAGMA busy_timeout = 5000");
  (* Enforce declared FOREIGN KEY constraints (incl. room_profile_bindings -> room_profiles
     ON DELETE CASCADE). SQLite defaults FK enforcement OFF per-connection. *)
  ignore (Sqlite3.exec db "PRAGMA foreign_keys = ON");
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
    \     provider_response_items_json TEXT,\n\
    \     thinking_content TEXT,\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )";
  migrate_schema db current_version;
  init_epoch_schema db;
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
  init_models_cache_schema db;
  init_model_discovery_state_schema db;
  (try
     let seeded = Model_discovery.seed_catalog_models ~db in
     Logs.debug (fun m -> m "models_cache: seeded %d catalog rows" seeded)
   with exn ->
     Logs.warn (fun m ->
         m "models_cache: catalog seed failed: %s" (Printexc.to_string exn)));
  init_request_stats_schema db;
  init_quota_cache_schema db;
  init_postmortems_schema db;
  Summary_store.init_schema db;
  init_session_archive_schema db;
  init_attachment_log_schema db;
  Admin.init_schema db;
  Pair_coding_state.init_schema db;
  Held_items.init_db db;
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
     tool_name, tool_calls_json, provider_response_items_json, \
     thinking_content) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
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
  ignore
    (Sqlite3.bind stmt 7
       (match msg.provider_response_items_json with
       | Some j -> Sqlite3.Data.TEXT j
       | None -> Sqlite3.Data.NULL));
  ignore
    (Sqlite3.bind stmt 8
       (match msg.thinking with
       | Some t -> Sqlite3.Data.TEXT t
       | None -> Sqlite3.Data.NULL));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Failed to store message: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let load_history ~db ~session_key =
  let sql =
    "SELECT role, content, tool_call_id, tool_name, tool_calls_json, \
     provider_response_items_json, thinking_content FROM messages WHERE \
     session_key = ? ORDER BY id ASC"
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
    let provider_response_items_json =
      match Sqlite3.column stmt 5 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let thinking =
      match Sqlite3.column stmt 6 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    messages :=
      {
        Provider.role;
        content;
        content_parts = [];
        tool_calls;
        tool_call_id;
        name;
        provider_response_items_json;
        thinking;
        is_error = false;
      }
      :: !messages
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !messages

let raw_messages_of_stmt stmt =
  let rows = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let text_opt index =
      match Sqlite3.column stmt index with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let text index = match text_opt index with Some s -> s | None -> "" in
    let id =
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT n -> Int64.to_int n
      | _ -> 0
    in
    rows :=
      {
        id;
        role = text 1;
        content = text 2;
        tool_call_id = text_opt 3;
        tool_name = text_opt 4;
        tool_calls_json = text_opt 5;
        provider_response_items_json = text_opt 6;
        thinking_content = text_opt 7;
        created_at = text 8;
      }
      :: !rows
  done;
  List.rev !rows

let load_raw_history ~db ~session_key =
  let sql =
    "SELECT id, role, content, tool_call_id, tool_name, tool_calls_json, \
     provider_response_items_json, thinking_content, created_at FROM messages \
     WHERE session_key = ? ORDER BY id ASC"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () -> raw_messages_of_stmt stmt)

let archive_session_epoch ~db ~session_key (rows : raw_message list) =
  match rows with
  | [] -> ()
  | first :: _ ->
      let last = List.hd (List.rev rows) in
      let epoch_sql =
        "INSERT INTO session_log_epochs (session_key, message_count, \
         first_message_at, last_message_at) VALUES (?, ?, ?, ?)"
      in
      let epoch_stmt = Sqlite3.prepare db epoch_sql in
      ignore (Sqlite3.bind epoch_stmt 1 (Sqlite3.Data.TEXT session_key));
      ignore
        (Sqlite3.bind epoch_stmt 2
           (Sqlite3.Data.INT (Int64.of_int (List.length rows))));
      ignore (Sqlite3.bind epoch_stmt 3 (Sqlite3.Data.TEXT first.created_at));
      ignore (Sqlite3.bind epoch_stmt 4 (Sqlite3.Data.TEXT last.created_at));
      (match Sqlite3.step epoch_stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf
               "SQLite error: %s (sql: INSERT INTO session_log_epochs ...)"
               (Sqlite3.Rc.to_string rc)));
      ignore (Sqlite3.finalize epoch_stmt);
      let epoch_id = Sqlite3.last_insert_rowid db |> Int64.to_int in
      let msg_sql =
        "INSERT INTO session_log_epoch_messages (epoch_id, ordinal, role, \
         content, tool_call_id, tool_name, tool_calls_json, \
         provider_response_items_json, thinking_content, created_at) VALUES \
         (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      in
      List.iteri
        (fun ordinal (row : raw_message) ->
          let stmt = Sqlite3.prepare db msg_sql in
          ignore
            (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int epoch_id)));
          ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int ordinal)));
          ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT row.role));
          ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT row.content));
          ignore
            (Sqlite3.bind stmt 5
               (match row.tool_call_id with
               | Some value -> Sqlite3.Data.TEXT value
               | None -> Sqlite3.Data.NULL));
          ignore
            (Sqlite3.bind stmt 6
               (match row.tool_name with
               | Some value -> Sqlite3.Data.TEXT value
               | None -> Sqlite3.Data.NULL));
          ignore
            (Sqlite3.bind stmt 7
               (match row.tool_calls_json with
               | Some value -> Sqlite3.Data.TEXT value
               | None -> Sqlite3.Data.NULL));
          ignore
            (Sqlite3.bind stmt 8
               (match row.provider_response_items_json with
               | Some value -> Sqlite3.Data.TEXT value
               | None -> Sqlite3.Data.NULL));
          ignore
            (Sqlite3.bind stmt 9
               (match row.thinking_content with
               | Some value -> Sqlite3.Data.TEXT value
               | None -> Sqlite3.Data.NULL));
          ignore (Sqlite3.bind stmt 10 (Sqlite3.Data.TEXT row.created_at));
          (match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE -> ()
          | rc ->
              failwith
                (Printf.sprintf
                   "SQLite error: %s (sql: INSERT INTO \
                    session_log_epoch_messages ...)"
                   (Sqlite3.Rc.to_string rc)));
          ignore (Sqlite3.finalize stmt))
        rows

let replace_session_messages ~db ~session_key (messages : Provider.message list)
    =
  exec_exn db "BEGIN TRANSACTION";
  try
    let existing = load_raw_history ~db ~session_key in
    archive_session_epoch ~db ~session_key existing;
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
  let epoch_ids_stmt =
    Sqlite3.prepare db "SELECT id FROM session_log_epochs WHERE session_key = ?"
  in
  let epoch_ids =
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize epoch_ids_stmt))
      (fun () ->
        ignore (Sqlite3.bind epoch_ids_stmt 1 (Sqlite3.Data.TEXT session_key));
        let ids = ref [] in
        while Sqlite3.step epoch_ids_stmt = Sqlite3.Rc.ROW do
          match Sqlite3.column epoch_ids_stmt 0 with
          | Sqlite3.Data.INT n -> ids := Int64.to_int n :: !ids
          | _ -> ()
        done;
        List.rev !ids)
  in
  List.iter
    (fun epoch_id ->
      let stmt =
        Sqlite3.prepare db
          "DELETE FROM session_log_epoch_messages WHERE epoch_id = ?"
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore
            (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int epoch_id)));
          ignore (Sqlite3.step stmt)))
    epoch_ids;
  clear "DELETE FROM session_log_epochs WHERE session_key = ?";
  clear "DELETE FROM messages WHERE session_key = ?";
  clear "DELETE FROM session_state WHERE session_key = ?";
  clear "DELETE FROM session_workspace_state WHERE session_key = ?";
  clear "DELETE FROM inbound_queue WHERE session_key = ?";
  clear "DELETE FROM session_repos WHERE session_key = ?";
  Summary_store.delete_for_session ~db ~session_key

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

(* B654: FTS5 treats ':' as a column-qualifier operator, so a query like
   "rig:briefing:config" parses as `column=rig MATCH "briefing:config"` and
   errors with "no such column: rig". Escape each whitespace-separated token
   as an FTS5 quoted phrase (with internal double quotes doubled) so colons
   and other punctuation are treated as literals. Multi-token queries
   compose with implicit AND, matching prior expectations. *)
let fts5_escape_token s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      if c = '"' then Buffer.add_string buf "\"\"" else Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let fts5_safe_query q =
  String.split_on_char ' ' q
  |> List.filter (fun s -> s <> "")
  |> List.map fts5_escape_token |> String.concat " "

let recall_core ~db ~query ~limit =
  let sql =
    "SELECT cm.key, cm.content, cm.category FROM core_memories cm JOIN \
     core_memories_fts f ON cm.rowid = f.rowid WHERE core_memories_fts MATCH ? \
     ORDER BY f.rank LIMIT ?"
  in
  let safe_query = fts5_safe_query query in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT safe_query));
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

(* Like [list_core] but also returns each memory's [updated_at] (Unix seconds)
   and allows ascending order (oldest first) for the /memories command. *)
let list_core_with_meta ~db ?(category = "") ?(oldest = false) () =
  let order = if oldest then "ASC" else "DESC" in
  let sql, has_category =
    if category = "" then
      ( Printf.sprintf
          "SELECT key, content, category, updated_at FROM core_memories ORDER \
           BY updated_at %s, key ASC"
          order,
        false )
    else
      ( Printf.sprintf
          "SELECT key, content, category, updated_at FROM core_memories WHERE \
           category = ? ORDER BY updated_at %s, key ASC"
          order,
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
    let updated =
      match Sqlite3.column stmt 3 with
      | Sqlite3.Data.INT n -> Int64.to_int n
      | _ -> 0
    in
    results := (key, content, cat, updated) :: !results
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
  let messages = load_history ~db ~session_key in
  let sanitized = Message_history.ensure_tool_group_integrity messages in
  let kept =
    if max_messages > 0 && List.length sanitized > max_messages then begin
      let compact_count = List.length sanitized - max_messages in
      let to_compact_raw =
        List.filteri (fun i _ -> i < compact_count) sanitized
      in
      let to_keep_raw =
        List.filteri (fun i _ -> i >= compact_count) sanitized
      in
      let to_keep =
        Message_history.expand_keep_for_tool_groups to_compact_raw to_keep_raw
      in
      Message_history.ensure_tool_group_integrity to_keep
    end
    else sanitized
  in
  if kept <> messages then replace_session_messages ~db ~session_key kept

let cleanup_all ~db ~max_messages ~max_age_days =
  let sessions = list_sessions ~db in
  List.iter
    (fun session_key ->
      cleanup_session ~db ~session_key ~max_messages ~max_age_days)
    sessions

let cleanup_connector_history ~db ~max_age_days ~max_messages =
  exec_exn db
    (Printf.sprintf
       "DELETE FROM connector_history WHERE created_at < datetime('now', '-%d \
        days')"
       max_age_days);
  let stmt =
    Sqlite3.prepare db "SELECT DISTINCT session_key FROM connector_history"
  in
  let keys = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.TEXT k -> keys := k :: !keys
        | _ -> ()
      done);
  List.iter
    (fun sk ->
      let trim_sql =
        Printf.sprintf
          "DELETE FROM connector_history WHERE session_key = ? AND id NOT IN \
           (SELECT id FROM connector_history WHERE session_key = ? ORDER BY id \
           DESC LIMIT %d)"
          max_messages
      in
      let trim_stmt = Sqlite3.prepare db trim_sql in
      ignore (Sqlite3.bind trim_stmt 1 (Sqlite3.Data.TEXT sk));
      ignore (Sqlite3.bind trim_stmt 2 (Sqlite3.Data.TEXT sk));
      (match Sqlite3.step trim_stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          Logs.warn (fun m ->
              m "cleanup_connector_history: trim failed for key=%s: %s" sk
                (Sqlite3.Rc.to_string rc)));
      ignore (Sqlite3.finalize trim_stmt))
    !keys

(* --- room_profiles --- *)

let insert_room_profile ~db ~name =
  let sql = "INSERT INTO room_profiles (name) VALUES (?)" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Int64.to_int (Sqlite3.last_insert_rowid db)
      | rc ->
          failwith
            (Printf.sprintf "insert_room_profile failed: %s"
               (Sqlite3.Rc.to_string rc)))

let get_room_profile ~db ~id =
  let sql =
    "SELECT id, name, created_at, updated_at FROM room_profiles WHERE id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          Some
            {
              id =
                (match Sqlite3.column stmt 0 with
                | Sqlite3.Data.INT n -> Int64.to_int n
                | _ -> 0);
              name =
                (match Sqlite3.column stmt 1 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> "");
              created_at =
                (match Sqlite3.column stmt 2 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> "");
              updated_at =
                (match Sqlite3.column stmt 3 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> "");
            }
      | _ -> None)

let get_room_profile_by_name ~db ~name =
  let sql =
    "SELECT id, name, created_at, updated_at FROM room_profiles WHERE name = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          Some
            {
              id =
                (match Sqlite3.column stmt 0 with
                | Sqlite3.Data.INT n -> Int64.to_int n
                | _ -> 0);
              name =
                (match Sqlite3.column stmt 1 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> "");
              created_at =
                (match Sqlite3.column stmt 2 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> "");
              updated_at =
                (match Sqlite3.column stmt 3 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> "");
            }
      | _ -> None)

let list_room_profiles ~db =
  let sql =
    "SELECT id, name, created_at, updated_at FROM room_profiles ORDER BY id"
  in
  let stmt = Sqlite3.prepare db sql in
  let profiles = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        profiles :=
          {
            id =
              (match Sqlite3.column stmt 0 with
              | Sqlite3.Data.INT n -> Int64.to_int n
              | _ -> 0);
            name =
              (match Sqlite3.column stmt 1 with
              | Sqlite3.Data.TEXT s -> s
              | _ -> "");
            created_at =
              (match Sqlite3.column stmt 2 with
              | Sqlite3.Data.TEXT s -> s
              | _ -> "");
            updated_at =
              (match Sqlite3.column stmt 3 with
              | Sqlite3.Data.TEXT s -> s
              | _ -> "");
          }
          :: !profiles
      done);
  List.rev !profiles

let delete_room_profile ~db ~id =
  let stmt_bind =
    Sqlite3.prepare db "DELETE FROM room_profile_bindings WHERE profile_id = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt_bind))
    (fun () ->
      ignore (Sqlite3.bind stmt_bind 1 (Sqlite3.Data.INT (Int64.of_int id)));
      ignore (Sqlite3.step stmt_bind));
  let sql = "DELETE FROM room_profiles WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id)));
      match Sqlite3.step stmt with Sqlite3.Rc.DONE -> true | _ -> false)

(* --- room_profile_bindings --- *)

let upsert_room_profile_binding_txn_impl ~db ~room_id ~profile_id =
  (* Remove any existing binding for this profile (1:1 cardinality) *)
  let del_stmt =
    Sqlite3.prepare db
      "DELETE FROM room_profile_bindings WHERE profile_id = ? AND room_id <> ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize del_stmt))
    (fun () ->
      ignore
        (Sqlite3.bind del_stmt 1 (Sqlite3.Data.INT (Int64.of_int profile_id)));
      ignore (Sqlite3.bind del_stmt 2 (Sqlite3.Data.TEXT room_id));
      ignore (Sqlite3.step del_stmt));
  let sql =
    "INSERT INTO room_profile_bindings (room_id, profile_id) VALUES (?, ?) ON \
     CONFLICT(room_id) DO UPDATE SET profile_id = excluded.profile_id, \
     created_at = room_profile_bindings.created_at"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int profile_id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "upsert_room_profile_binding failed: %s"
               (Sqlite3.Rc.to_string rc)))

let upsert_room_profile_binding ~db ~room_id ~profile_id =
  (* Validate profile_id exists before binding *)
  (match get_room_profile ~db ~id:profile_id with
  | None ->
      failwith
        (Printf.sprintf
           "upsert_room_profile_binding: profile_id %d does not exist"
           profile_id)
  | Some _ -> ());
  (* Atomically move/insert the 1:1 binding: a failure between the DELETE and the
     INSERT must not leave the room unbound, so wrap both in a transaction. *)
  exec_exn db "BEGIN IMMEDIATE";
  try
    upsert_room_profile_binding_txn_impl ~db ~room_id ~profile_id;
    exec_exn db "COMMIT"
  with e ->
    (try exec_exn db "ROLLBACK" with _ -> ());
    raise e

(** Like {!upsert_room_profile_binding} but does NOT open its own transaction.
    Caller must provide transactional context (e.g. a SAVEPOINT). *)
let upsert_room_profile_binding_no_txn ~db ~room_id ~profile_id =
  (match get_room_profile ~db ~id:profile_id with
  | None ->
      failwith
        (Printf.sprintf
           "upsert_room_profile_binding_no_txn: profile_id %d does not exist"
           profile_id)
  | Some _ -> ());
  upsert_room_profile_binding_txn_impl ~db ~room_id ~profile_id

let get_room_profile_binding ~db ~room_id =
  let sql =
    "SELECT room_id, profile_id, created_at FROM room_profile_bindings WHERE \
     room_id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          Some
            {
              room_id =
                (match Sqlite3.column stmt 0 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> "");
              profile_id =
                (match Sqlite3.column stmt 1 with
                | Sqlite3.Data.INT n -> Int64.to_int n
                | _ -> 0);
              created_at =
                (match Sqlite3.column stmt 2 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> "");
            }
      | _ -> None)

let get_room_profile_for_room ~db ~room_id =
  match get_room_profile_binding ~db ~room_id with
  | None -> None
  | Some binding -> get_room_profile ~db ~id:binding.profile_id

let remove_room_profile_binding ~db ~room_id =
  let sql = "DELETE FROM room_profile_bindings WHERE room_id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
      match Sqlite3.step stmt with Sqlite3.Rc.DONE -> true | _ -> false)

let list_room_profile_bindings_all ~db =
  let sql =
    "SELECT room_id, profile_id, created_at FROM room_profile_bindings ORDER \
     BY room_id"
  in
  let stmt = Sqlite3.prepare db sql in
  let bindings = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        bindings :=
          {
            room_id =
              (match Sqlite3.column stmt 0 with
              | Sqlite3.Data.TEXT s -> s
              | _ -> "");
            profile_id =
              (match Sqlite3.column stmt 1 with
              | Sqlite3.Data.INT n -> Int64.to_int n
              | _ -> 0);
            created_at =
              (match Sqlite3.column stmt 2 with
              | Sqlite3.Data.TEXT s -> s
              | _ -> "");
          }
          :: !bindings
      done);
  List.rev !bindings

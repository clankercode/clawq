let schema_version = 16

type session_activity = Active | Inactive | Any

type session_info = {
  session_key : string;
  channel : string option;
  channel_id : string option;
  turn : string option;
  response_sent_at : string option;
  last_active : string option;
  message_count : int;
  archived_epoch_count : int;
  keepalive_enabled : bool;
  heartbeat_enabled : bool;
}

type raw_message = {
  id : int;
  role : string;
  content : string;
  tool_call_id : string option;
  tool_name : string option;
  tool_calls_json : string option;
  provider_response_items_json : string option;
  created_at : string;
}

type session_epoch = {
  epoch_id : int option;
  label : string;
  current : bool;
  message_count : int;
  first_message_at : string option;
  last_message_at : string option;
  recorded_at : string option;
}

type epoch_selector = Current | Archived of int

type history_search_result = {
  role : string;
  content : string;
  created_at : string;
  source : string;
}

type queue_state = Pending | Claimed | Failed

type queue_row = {
  queue_id : int;
  session_key : string;
  source : string;
  state : queue_state;
  payload_json : string;
  attempt_count : int;
  last_error : string option;
  claimed_at : string option;
  created_at : string;
}

type queue_claim_result = Claim_ok of queue_row | Claim_empty

let queue_state_to_string = function
  | Pending -> "pending"
  | Claimed -> "claimed"
  | Failed -> "failed"

let queue_state_of_string = function
  | "pending" -> Pending
  | "claimed" -> Claimed
  | "failed" -> Failed
  | s -> failwith (Printf.sprintf "Unknown queue state: %s" s)

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
    \     last_active TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     keepalive_enabled INTEGER NOT NULL DEFAULT 0,\n\
    \     heartbeat_enabled INTEGER NOT NULL DEFAULT 0,\n\
    \     model_override TEXT DEFAULT NULL\n\
    \   )";
  exec_exn db
    "CREATE TABLE IF NOT EXISTS discord_resume_state (\n\
    \     id INTEGER PRIMARY KEY CHECK (id = 1),\n\
    \     session_id TEXT NOT NULL,\n\
    \     seq INTEGER NOT NULL,\n\
    \     resume_gateway_url TEXT NOT NULL,\n\
    \     updated_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )";
  exec_exn db
    "CREATE TABLE IF NOT EXISTS session_workspace_state (\n\
    \     session_key TEXT PRIMARY KEY,\n\
    \     observed_files_json TEXT NOT NULL,\n\
    \     updated_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )"

let init_inbound_queue_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS inbound_queue (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     session_key TEXT NOT NULL,\n\
    \     source TEXT NOT NULL DEFAULT 'cli',\n\
    \     state TEXT NOT NULL DEFAULT 'pending',\n\
    \     payload_json TEXT NOT NULL,\n\
    \     attempt_count INTEGER NOT NULL DEFAULT 0,\n\
    \     last_error TEXT,\n\
    \     claimed_at TEXT,\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_inbound_queue_session_state ON \
     inbound_queue (session_key, state, id ASC)"

let init_quota_cache_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS quota_cache (\n\
    \     provider TEXT PRIMARY KEY,\n\
    \     state_json TEXT NOT NULL,\n\
    \     fetched_at REAL NOT NULL\n\
    \   )"

let init_postmortems_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS postmortems (\n\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \  session_key TEXT NOT NULL,\n\
    \  created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \  pattern TEXT NOT NULL,\n\
    \  evidence_json TEXT NOT NULL,\n\
    \  correction_injected TEXT NOT NULL,\n\
    \  outcome TEXT,\n\
    \  doc_path TEXT NOT NULL\n\
     )"

let init_models_cache_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS models_cache (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     provider TEXT NOT NULL,\n\
    \     model_id TEXT NOT NULL,\n\
    \     display_name TEXT,\n\
    \     context_window INTEGER,\n\
    \     supports_vision INTEGER NOT NULL DEFAULT 0,\n\
    \     supports_tools INTEGER NOT NULL DEFAULT 1,\n\
    \     supports_thinking INTEGER NOT NULL DEFAULT 0,\n\
    \     input_price_per_m REAL,\n\
    \     output_price_per_m REAL,\n\
    \     source TEXT,\n\
    \     fetched_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     UNIQUE(provider, model_id)\n\
    \   )"

let init_model_discovery_state_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS model_discovery_state (\n\
    \     provider TEXT PRIMARY KEY,\n\
    \     last_attempted_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     last_error TEXT\n\
    \   )"

let init_request_stats_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS request_stats (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     session_key TEXT NOT NULL,\n\
    \     message_id INTEGER,\n\
    \     provider TEXT NOT NULL,\n\
    \     model TEXT NOT NULL,\n\
    \     prompt_tokens INTEGER NOT NULL,\n\
    \     completion_tokens INTEGER NOT NULL,\n\
    \     cost_usd REAL,\n\
    \     added_prompt_tokens INTEGER,\n\
    \     requested_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_request_stats_session ON \
     request_stats(session_key)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_request_stats_model ON \
     request_stats(model, requested_at)"

let init_epoch_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS session_log_epochs (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     session_key TEXT NOT NULL,\n\
    \     message_count INTEGER NOT NULL,\n\
    \     first_message_at TEXT,\n\
    \     last_message_at TEXT,\n\
    \     archived_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_session_log_epochs_session_key ON \
     session_log_epochs (session_key, id DESC)";
  exec_exn db
    "CREATE TABLE IF NOT EXISTS session_log_epoch_messages (\n\
    \     epoch_id INTEGER NOT NULL,\n\
    \     ordinal INTEGER NOT NULL,\n\
    \     role TEXT NOT NULL,\n\
    \     content TEXT NOT NULL,\n\
    \     tool_call_id TEXT,\n\
    \     tool_name TEXT,\n\
    \     tool_calls_json TEXT,\n\
    \     provider_response_items_json TEXT,\n\
    \     created_at TEXT NOT NULL,\n\
    \     PRIMARY KEY (epoch_id, ordinal),\n\
    \     FOREIGN KEY (epoch_id) REFERENCES session_log_epochs(id) ON DELETE \
     CASCADE\n\
    \   )"

let init_pending_questions_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS pending_questions (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     session_key TEXT NOT NULL UNIQUE,\n\
    \     questions_json TEXT NOT NULL,\n\
    \     question_index INTEGER NOT NULL DEFAULT 0,\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )"

let migrate_schema db current_version =
  match current_version with
  | 0 ->
      init_session_schema db;
      init_epoch_schema db;
      init_inbound_queue_schema db;
      init_pending_questions_schema db;
      (try
         exec_exn db
           "ALTER TABLE task_tree ADD COLUMN deleted_at TEXT DEFAULT NULL"
       with _ -> ());
      exec_exn db
        (Printf.sprintf "INSERT INTO schema_version (version) VALUES (%d)"
           schema_version)
  | 1 ->
      init_session_schema db;
      init_epoch_schema db;
      init_inbound_queue_schema db;
      exec_exn db
        "ALTER TABLE messages ADD COLUMN provider_response_items_json TEXT";
      (try
         exec_exn db
           "ALTER TABLE task_tree ADD COLUMN deleted_at TEXT DEFAULT NULL"
       with _ -> ());
      set_schema_version db schema_version
  | 2 ->
      init_session_schema db;
      init_epoch_schema db;
      init_inbound_queue_schema db;
      exec_exn db
        "ALTER TABLE messages ADD COLUMN provider_response_items_json TEXT";
      (try
         exec_exn db
           "ALTER TABLE task_tree ADD COLUMN deleted_at TEXT DEFAULT NULL"
       with _ -> ());
      set_schema_version db schema_version
  | 3 ->
      init_session_schema db;
      init_epoch_schema db;
      init_inbound_queue_schema db;
      (try
         exec_exn db
           "ALTER TABLE task_tree ADD COLUMN deleted_at TEXT DEFAULT NULL"
       with _ -> ());
      set_schema_version db schema_version
  | 4 ->
      init_session_schema db;
      init_inbound_queue_schema db;
      init_models_cache_schema db;
      init_request_stats_schema db;
      (try
         exec_exn db
           "ALTER TABLE task_tree ADD COLUMN deleted_at TEXT DEFAULT NULL"
       with _ -> ());
      set_schema_version db schema_version
  | 5 ->
      init_session_schema db;
      init_inbound_queue_schema db;
      init_models_cache_schema db;
      init_request_stats_schema db;
      exec_exn db
        "ALTER TABLE session_state ADD COLUMN keepalive_enabled INTEGER NOT \
         NULL DEFAULT 0";
      (try
         exec_exn db
           "ALTER TABLE task_tree ADD COLUMN deleted_at TEXT DEFAULT NULL"
       with _ -> ());
      set_schema_version db schema_version
  | 6 ->
      init_session_schema db;
      init_inbound_queue_schema db;
      init_models_cache_schema db;
      init_request_stats_schema db;
      exec_exn db
        "ALTER TABLE session_state ADD COLUMN keepalive_enabled INTEGER NOT \
         NULL DEFAULT 0";
      (try
         exec_exn db
           "ALTER TABLE task_tree ADD COLUMN deleted_at TEXT DEFAULT NULL"
       with _ -> ());
      set_schema_version db schema_version
  | 7 ->
      init_session_schema db;
      init_inbound_queue_schema db;
      init_models_cache_schema db;
      init_request_stats_schema db;
      (try
         exec_exn db
           "ALTER TABLE task_tree ADD COLUMN deleted_at TEXT DEFAULT NULL"
       with _ -> ());
      set_schema_version db schema_version
  | 8 ->
      init_session_schema db;
      init_inbound_queue_schema db;
      init_models_cache_schema db;
      init_request_stats_schema db;
      init_quota_cache_schema db;
      set_schema_version db schema_version
  | 9 ->
      init_session_schema db;
      init_inbound_queue_schema db;
      init_models_cache_schema db;
      init_request_stats_schema db;
      init_quota_cache_schema db;
      exec_exn db
        "ALTER TABLE session_state ADD COLUMN model_override TEXT DEFAULT NULL";
      set_schema_version db schema_version
  | 10 ->
      init_session_schema db;
      init_inbound_queue_schema db;
      init_models_cache_schema db;
      init_request_stats_schema db;
      init_quota_cache_schema db;
      init_postmortems_schema db;
      set_schema_version db schema_version
  | 11 ->
      init_session_schema db;
      init_inbound_queue_schema db;
      init_models_cache_schema db;
      init_request_stats_schema db;
      init_quota_cache_schema db;
      init_postmortems_schema db;
      init_model_discovery_state_schema db;
      Summary_store.init_schema db;
      set_schema_version db schema_version
  | 12 ->
      init_session_schema db;
      init_inbound_queue_schema db;
      init_models_cache_schema db;
      init_request_stats_schema db;
      init_quota_cache_schema db;
      init_postmortems_schema db;
      init_model_discovery_state_schema db;
      Summary_store.init_schema db;
      set_schema_version db schema_version
  | 13 ->
      init_session_schema db;
      init_inbound_queue_schema db;
      init_models_cache_schema db;
      init_request_stats_schema db;
      init_quota_cache_schema db;
      init_postmortems_schema db;
      init_model_discovery_state_schema db;
      Summary_store.init_schema db;
      (try
         exec_exn db
           "ALTER TABLE request_stats ADD COLUMN added_prompt_tokens INTEGER"
       with _ -> ());
      set_schema_version db schema_version
  | 14 ->
      init_session_schema db;
      init_inbound_queue_schema db;
      init_models_cache_schema db;
      init_request_stats_schema db;
      init_quota_cache_schema db;
      init_postmortems_schema db;
      init_model_discovery_state_schema db;
      Summary_store.init_schema db;
      init_pending_questions_schema db;
      exec_exn db
        "ALTER TABLE session_state ADD COLUMN heartbeat_enabled INTEGER NOT \
         NULL DEFAULT 0";
      set_schema_version db schema_version
  | 15 ->
      init_session_schema db;
      init_inbound_queue_schema db;
      init_models_cache_schema db;
      init_request_stats_schema db;
      init_quota_cache_schema db;
      init_postmortems_schema db;
      init_model_discovery_state_schema db;
      Summary_store.init_schema db;
      init_pending_questions_schema db;
      exec_exn db
        "ALTER TABLE session_state ADD COLUMN heartbeat_enabled INTEGER NOT \
         NULL DEFAULT 0";
      set_schema_version db schema_version
  | n when n = schema_version ->
      init_session_schema db;
      init_inbound_queue_schema db;
      init_models_cache_schema db;
      init_request_stats_schema db;
      init_quota_cache_schema db;
      init_postmortems_schema db;
      init_model_discovery_state_schema db;
      Summary_store.init_schema db;
      init_pending_questions_schema db
  | n ->
      failwith
        (Printf.sprintf "DB uses future schema version %d (current=%d)" n
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
  ignore (Sqlite3.exec db "PRAGMA busy_timeout = 5000");
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
  init_request_stats_schema db;
  init_quota_cache_schema db;
  init_postmortems_schema db;
  Summary_store.init_schema db;
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
     tool_name, tool_calls_json, provider_response_items_json) VALUES (?, ?, \
     ?, ?, ?, ?, ?)"
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
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Failed to store message: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let load_history ~db ~session_key =
  let sql =
    "SELECT role, content, tool_call_id, tool_name, tool_calls_json, \
     provider_response_items_json FROM messages WHERE session_key = ? ORDER BY \
     id ASC"
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
    messages :=
      {
        Provider.role;
        content;
        content_parts = [];
        tool_calls;
        tool_call_id;
        name;
        provider_response_items_json;
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
        created_at = text 7;
      }
      :: !rows
  done;
  List.rev !rows

let load_raw_history ~db ~session_key =
  let sql =
    "SELECT id, role, content, tool_call_id, tool_name, tool_calls_json, \
     provider_response_items_json, created_at FROM messages WHERE session_key \
     = ? ORDER BY id ASC"
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
         provider_response_items_json, created_at) VALUES (?, ?, ?, ?, ?, ?, \
         ?, ?, ?)"
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
          ignore (Sqlite3.bind stmt 9 (Sqlite3.Data.TEXT row.created_at));
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
  Summary_store.delete_for_session ~db ~session_key

let store_session_workspace_state ~db ~session_key
    ~observed_active_workspace_files =
  let observed_files_json =
    `List
      (List.map
         (fun (file, digest) ->
           `Assoc
             [
               ("file", `String file);
               ( "digest",
                 match digest with Some value -> `String value | None -> `Null
               );
             ])
         observed_active_workspace_files)
    |> Yojson.Safe.to_string
  in
  let sql =
    "INSERT INTO session_workspace_state (session_key, observed_files_json, \
     updated_at) VALUES (?, ?, datetime('now')) ON CONFLICT(session_key) DO \
     UPDATE SET observed_files_json = excluded.observed_files_json, updated_at \
     = datetime('now')"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT observed_files_json));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          Logs.warn (fun m ->
              m "Failed to store session workspace state: %s"
                (Sqlite3.Rc.to_string rc)))

let load_session_workspace_state ~db ~session_key =
  let sql =
    "SELECT observed_files_json FROM session_workspace_state WHERE session_key \
     = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.TEXT json -> (
              try
                let open Yojson.Safe.Util in
                Some
                  (Yojson.Safe.from_string json
                  |> to_list
                  |> List.filter_map (fun entry ->
                      try
                        let file = entry |> member "file" |> to_string in
                        let digest =
                          match entry |> member "digest" with
                          | `Null -> None
                          | value -> Some (to_string value)
                        in
                        Some (file, digest)
                      with _ -> None))
              with _ -> None)
          | _ -> None)
      | _ -> None)

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

let set_session_keepalive ~db ~session_key ~enabled =
  let sql =
    "INSERT INTO session_state (session_key, keepalive_enabled) VALUES (?, ?) \
     ON CONFLICT(session_key) DO UPDATE SET keepalive_enabled = \
     excluded.keepalive_enabled"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (if enabled then 1L else 0L)));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Failed to set session keepalive: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let set_session_heartbeat ~db ~session_key ~enabled =
  let sql =
    "INSERT INTO session_state (session_key, heartbeat_enabled) VALUES (?, ?) \
     ON CONFLICT(session_key) DO UPDATE SET heartbeat_enabled = \
     excluded.heartbeat_enabled"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (if enabled then 1L else 0L)));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Failed to set session heartbeat: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let session_heartbeat_enabled ~db ~session_key =
  let sql =
    "SELECT COALESCE(heartbeat_enabled, 0) FROM session_state WHERE \
     session_key = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  let enabled =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.INT n -> n <> 0L
        | _ -> false)
    | _ -> false
  in
  ignore (Sqlite3.finalize stmt);
  enabled

let set_session_model_override ~db ~session_key ~model =
  let sql =
    "INSERT INTO session_state (session_key, model_override) VALUES (?, ?) ON \
     CONFLICT(session_key) DO UPDATE SET model_override = \
     excluded.model_override"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT model));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Failed to set session model override: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let get_session_model_override ~db ~session_key =
  let sql = "SELECT model_override FROM session_state WHERE session_key = ?" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.TEXT s -> Some s
        | _ -> None)
    | _ -> None
  in
  ignore (Sqlite3.finalize stmt);
  result

let list_keepalive_session_keys ~db =
  let sql =
    "SELECT session_key FROM session_state WHERE keepalive_enabled = 1"
  in
  let stmt = Sqlite3.prepare db sql in
  let keys = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    match Sqlite3.column stmt 0 with
    | Sqlite3.Data.TEXT s -> keys := s :: !keys
    | _ -> ()
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !keys

let list_heartbeat_session_keys ~db =
  let sql =
    "SELECT session_key FROM session_state WHERE heartbeat_enabled = 1 ORDER \
     BY session_key"
  in
  let stmt = Sqlite3.prepare db sql in
  let keys = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    match Sqlite3.column stmt 0 with
    | Sqlite3.Data.TEXT s -> keys := s :: !keys
    | _ -> ()
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !keys

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

let parse_channel_from_session_key key =
  match String.split_on_char ':' key with
  | channel :: _ when channel <> "" -> Some channel
  | _ -> None

let list_session_infos ~db ?channel ?prefix ?(activity = Any) ?only_main () =
  let sql =
    "SELECT k.session_key, s.channel, s.channel_id, s.turn, \
     s.response_sent_at, s.last_active, (SELECT COUNT(*) FROM messages m WHERE \
     m.session_key = k.session_key), (SELECT COUNT(*) FROM session_log_epochs \
     e WHERE e.session_key = k.session_key), COALESCE(s.keepalive_enabled, 0), \
     COALESCE(s.heartbeat_enabled, 0) FROM (SELECT session_key FROM messages \
     UNION SELECT session_key FROM session_state UNION SELECT session_key FROM \
     session_log_epochs) k LEFT JOIN session_state s ON s.session_key = \
     k.session_key ORDER BY k.session_key"
  in
  let stmt = Sqlite3.prepare db sql in
  let rows = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let text_opt index =
      match Sqlite3.column stmt index with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let int_value index =
      match Sqlite3.column stmt index with
      | Sqlite3.Data.INT n -> Int64.to_int n
      | _ -> 0
    in
    let session_key = match text_opt 0 with Some s -> s | None -> "" in
    if session_key <> "" then
      rows :=
        {
          session_key;
          channel = text_opt 1;
          channel_id = text_opt 2;
          turn = text_opt 3;
          response_sent_at = text_opt 4;
          last_active = text_opt 5;
          message_count = int_value 6;
          archived_epoch_count = int_value 7;
          keepalive_enabled = int_value 8 <> 0;
          heartbeat_enabled = int_value 9 <> 0;
        }
        :: !rows
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !rows
  |> List.filter (fun row ->
      let effective_channel =
        match row.channel with
        | Some value -> Some value
        | None -> parse_channel_from_session_key row.session_key
      in
      let channel_ok =
        match channel with
        | None -> true
        | Some expected -> effective_channel = Some expected
      in
      let prefix_ok =
        match prefix with
        | None -> true
        | Some value ->
            let plen = String.length value in
            String.length row.session_key >= plen
            && String.sub row.session_key 0 plen = value
      in
      let main_ok =
        match only_main with
        | None -> true
        | Some expected -> row.session_key = "__main__" = expected
      in
      let activity_ok =
        match activity with
        | Any -> true
        | Active -> row.turn = Some "agent"
        | Inactive -> row.turn <> Some "agent"
      in
      channel_ok && prefix_ok && main_ok && activity_ok)

let list_session_epochs ~db ~session_key =
  let current_rows = load_raw_history ~db ~session_key in
  let current_epoch =
    let first_at =
      match current_rows with row :: _ -> Some row.created_at | [] -> None
    in
    let last_at =
      match List.rev current_rows with
      | row :: _ -> Some row.created_at
      | [] -> None
    in
    {
      epoch_id = None;
      label = "current";
      current = true;
      message_count = List.length current_rows;
      first_message_at = first_at;
      last_message_at = last_at;
      recorded_at = None;
    }
  in
  let sql =
    "SELECT id, message_count, first_message_at, last_message_at, archived_at \
     FROM session_log_epochs WHERE session_key = ? ORDER BY id DESC"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
  let archived = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let text_opt index =
      match Sqlite3.column stmt index with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let epoch_id =
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT n -> Some (Int64.to_int n)
      | _ -> None
    in
    let message_count =
      match Sqlite3.column stmt 1 with
      | Sqlite3.Data.INT n -> Int64.to_int n
      | _ -> 0
    in
    let label =
      match epoch_id with Some id -> string_of_int id | None -> "archive"
    in
    archived :=
      {
        epoch_id;
        label;
        current = false;
        message_count;
        first_message_at = text_opt 2;
        last_message_at = text_opt 3;
        recorded_at = text_opt 4;
      }
      :: !archived
  done;
  ignore (Sqlite3.finalize stmt);
  current_epoch :: List.rev !archived

let load_epoch_messages ~db ~session_key ~epoch =
  match epoch with
  | Current -> Some (load_raw_history ~db ~session_key)
  | Archived epoch_id ->
      let owner_stmt =
        Sqlite3.prepare db
          "SELECT session_key FROM session_log_epochs WHERE id = ?"
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize owner_stmt))
        (fun () ->
          ignore
            (Sqlite3.bind owner_stmt 1
               (Sqlite3.Data.INT (Int64.of_int epoch_id)));
          match Sqlite3.step owner_stmt with
          | Sqlite3.Rc.ROW -> (
              match Sqlite3.column owner_stmt 0 with
              | Sqlite3.Data.TEXT owner when owner = session_key ->
                  let sql =
                    "SELECT ordinal, role, content, tool_call_id, tool_name, \
                     tool_calls_json, provider_response_items_json, created_at \
                     FROM session_log_epoch_messages WHERE epoch_id = ? ORDER \
                     BY ordinal ASC"
                  in
                  let stmt = Sqlite3.prepare db sql in
                  ignore
                    (Sqlite3.bind stmt 1
                       (Sqlite3.Data.INT (Int64.of_int epoch_id)));
                  let rows = ref [] in
                  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
                    let text_opt index =
                      match Sqlite3.column stmt index with
                      | Sqlite3.Data.TEXT s -> Some s
                      | _ -> None
                    in
                    let ordinal =
                      match Sqlite3.column stmt 0 with
                      | Sqlite3.Data.INT n -> Int64.to_int n
                      | _ -> 0
                    in
                    let role =
                      match text_opt 1 with Some s -> s | None -> ""
                    in
                    let content =
                      match text_opt 2 with Some s -> s | None -> ""
                    in
                    let created_at =
                      match text_opt 7 with Some s -> s | None -> ""
                    in
                    rows :=
                      {
                        id = ordinal;
                        role;
                        content;
                        tool_call_id = text_opt 3;
                        tool_name = text_opt 4;
                        tool_calls_json = text_opt 5;
                        provider_response_items_json = text_opt 6;
                        created_at;
                      }
                      :: !rows
                  done;
                  ignore (Sqlite3.finalize stmt);
                  Some (List.rev !rows)
              | _ -> None)
          | _ -> None)

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
           m.tool_calls_json, m.provider_response_items_json FROM messages m \
           JOIN messages_fts f ON m.id = f.rowid WHERE messages_fts MATCH ? \
           AND f.session_key = ? ORDER BY f.rank LIMIT ?",
          true )
    | None ->
        ( "SELECT m.role, m.content, m.tool_call_id, m.tool_name, \
           m.tool_calls_json, m.provider_response_items_json FROM messages m \
           JOIN messages_fts f ON m.id = f.rowid WHERE messages_fts MATCH ? \
           ORDER BY f.rank LIMIT ?",
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
    let provider_response_items_json =
      match Sqlite3.column stmt 5 with
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
      }
      :: !messages
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !messages

let escape_like s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '%' | '_' | '\\' ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf c
      | _ -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let search_session_history ~db ~session_key ~query ~limit () =
  let like_pattern = "%" ^ escape_like query ^ "%" in
  let current_results =
    let fts_results =
      try
        let sql =
          "SELECT m.role, m.content, m.created_at, 'current' AS source FROM \
           messages m JOIN messages_fts f ON m.id = f.rowid WHERE messages_fts \
           MATCH ? AND f.session_key = ? ORDER BY f.rank LIMIT ?"
        in
        let stmt = Sqlite3.prepare db sql in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
          (fun () ->
            ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT query));
            ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT session_key));
            ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int limit)));
            let rows = ref [] in
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
              let created_at =
                match Sqlite3.column stmt 2 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> ""
              in
              rows :=
                ({ role; content; created_at; source = "current" }
                  : history_search_result)
                :: !rows
            done;
            Some (List.rev !rows))
      with _ -> None
    in
    match fts_results with
    | Some r -> r
    | None ->
        let sql =
          "SELECT role, content, created_at, 'current' AS source FROM messages \
           WHERE session_key = ? AND content LIKE ? ESCAPE '\\' ORDER BY \
           created_at DESC LIMIT ?"
        in
        let stmt = Sqlite3.prepare db sql in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
          (fun () ->
            ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
            ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT like_pattern));
            ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int limit)));
            let rows = ref [] in
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
              let created_at =
                match Sqlite3.column stmt 2 with
                | Sqlite3.Data.TEXT s -> s
                | _ -> ""
              in
              rows :=
                ({ role; content; created_at; source = "current" }
                  : history_search_result)
                :: !rows
            done;
            List.rev !rows)
  in
  let archived_results =
    let sql =
      "SELECT em.role, em.content, em.created_at, 'epoch:' || e.id AS source \
       FROM session_log_epoch_messages em JOIN session_log_epochs e ON \
       em.epoch_id = e.id WHERE e.session_key = ? AND em.content LIKE ? ESCAPE \
       '\\' ORDER BY em.created_at DESC LIMIT ?"
    in
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
        ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT like_pattern));
        ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int limit)));
        let rows = ref [] in
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
          let created_at =
            match Sqlite3.column stmt 2 with
            | Sqlite3.Data.TEXT s -> s
            | _ -> ""
          in
          let source =
            match Sqlite3.column stmt 3 with
            | Sqlite3.Data.TEXT s -> s
            | _ -> "epoch:?"
          in
          rows :=
            ({ role; content; created_at; source } : history_search_result)
            :: !rows
        done;
        List.rev !rows)
  in
  let merged = current_results @ archived_results in
  let sorted =
    List.sort
      (fun (a : history_search_result) (b : history_search_result) ->
        String.compare b.created_at a.created_at)
      merged
  in
  if List.length sorted <= limit then sorted
  else List.filteri (fun i _ -> i < limit) sorted

(* ---- Inbound Queue API ---- *)

let queue_row_of_stmt stmt =
  let int_col i =
    match Sqlite3.column stmt i with
    | Sqlite3.Data.INT n -> Int64.to_int n
    | _ -> 0
  in
  let text_col i =
    match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> s | _ -> ""
  in
  let text_opt_col i =
    match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> Some s | _ -> None
  in
  {
    queue_id = int_col 0;
    session_key = text_col 1;
    source = text_col 2;
    state = queue_state_of_string (text_col 3);
    payload_json = text_col 4;
    attempt_count = int_col 5;
    last_error = text_opt_col 6;
    claimed_at = text_opt_col 7;
    created_at = text_col 8;
  }

let queue_enqueue ~db ~session_key ~source ~payload_json =
  let sql =
    "INSERT INTO inbound_queue (session_key, source, payload_json) VALUES (?, \
     ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT source));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT payload_json));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Int64.to_int (Sqlite3.last_insert_rowid db)
      | rc ->
          failwith
            (Printf.sprintf "queue_enqueue failed: %s" (Sqlite3.Rc.to_string rc)))

let queue_claim ~db ~session_key =
  let sql =
    "SELECT id, session_key, source, state, payload_json, attempt_count, \
     last_error, claimed_at, created_at FROM inbound_queue WHERE session_key = \
     ? AND state = 'pending' ORDER BY id ASC LIMIT 1"
  in
  let stmt = Sqlite3.prepare db sql in
  let row =
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> Some (queue_row_of_stmt stmt)
        | _ -> None)
  in
  match row with
  | None -> Claim_empty
  | Some row ->
      let update_sql =
        "UPDATE inbound_queue SET state = 'claimed', claimed_at = \
         datetime('now') WHERE id = ? AND state = 'pending'"
      in
      let update_stmt = Sqlite3.prepare db update_sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize update_stmt))
        (fun () ->
          ignore
            (Sqlite3.bind update_stmt 1
               (Sqlite3.Data.INT (Int64.of_int row.queue_id)));
          match Sqlite3.step update_stmt with
          | Sqlite3.Rc.DONE when Sqlite3.changes db > 0 ->
              Claim_ok { row with state = Claimed }
          | _ -> Claim_empty)

let queue_release ~db ~queue_id =
  let sql =
    "UPDATE inbound_queue SET state = 'pending', claimed_at = NULL WHERE id = \
     ? AND state = 'claimed'"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int queue_id)));
      ignore (Sqlite3.step stmt);
      Sqlite3.changes db > 0)

let queue_delete ~db ~queue_id =
  let sql = "DELETE FROM inbound_queue WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int queue_id)));
      ignore (Sqlite3.step stmt);
      Sqlite3.changes db > 0)

let queue_record_failure ~db ~queue_id ~error =
  let sql =
    "UPDATE inbound_queue SET state = 'failed', attempt_count = attempt_count \
     + 1, last_error = ? WHERE id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT error));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int queue_id)));
      ignore (Sqlite3.step stmt))

let queue_reclaim_stale ~db ~older_than_seconds =
  let sql =
    "UPDATE inbound_queue SET state = 'pending', claimed_at = NULL WHERE state \
     = 'claimed' AND claimed_at < datetime('now', '-' || ? || ' seconds')"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.INT (Int64.of_int older_than_seconds)));
      ignore (Sqlite3.step stmt);
      Sqlite3.changes db)

let queue_reclaim_failed ~db =
  let sql =
    "UPDATE inbound_queue SET state = 'pending', claimed_at = NULL WHERE state \
     = 'failed'"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.step stmt);
      Sqlite3.changes db)

let queue_count ~db ~session_key =
  let sql =
    "SELECT COUNT(*) FROM inbound_queue WHERE session_key = ? AND state IN \
     ('pending', 'failed')"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0)
      | _ -> 0)

let queue_count_all ~db =
  query_single_int db
    "SELECT COUNT(*) FROM inbound_queue WHERE state IN ('pending', 'failed')"

let queue_list ~db ~session_key =
  let sql =
    "SELECT id, session_key, source, state, payload_json, attempt_count, \
     last_error, claimed_at, created_at FROM inbound_queue WHERE session_key = \
     ? ORDER BY id ASC"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        rows := queue_row_of_stmt stmt :: !rows
      done;
      List.rev !rows)

let queue_list_pending_sessions ~db =
  let sql =
    "SELECT session_key FROM inbound_queue WHERE state IN ('pending', \
     'failed') GROUP BY session_key ORDER BY MIN(id)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let keys = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.TEXT s -> keys := s :: !keys
        | _ -> ()
      done;
      List.rev !keys)

let queue_clear ~db ~session_key =
  let sql = "DELETE FROM inbound_queue WHERE session_key = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.step stmt);
      Sqlite3.changes db)

type postmortem = {
  id : int;
  session_key : string;
  created_at : string;
  pattern : string;
  evidence_json : string;
  correction_injected : string;
  outcome : string option;
  doc_path : string;
}

let postmortem_of_stmt stmt =
  let text_col i =
    match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> s | _ -> ""
  in
  let int_col i =
    match Sqlite3.column stmt i with
    | Sqlite3.Data.INT n -> Int64.to_int n
    | _ -> 0
  in
  let text_opt_col i =
    match Sqlite3.column stmt i with
    | Sqlite3.Data.TEXT s -> Some s
    | Sqlite3.Data.NULL -> None
    | _ -> None
  in
  {
    id = int_col 0;
    session_key = text_col 1;
    created_at = text_col 2;
    pattern = text_col 3;
    evidence_json = text_col 4;
    correction_injected = text_col 5;
    outcome = text_opt_col 6;
    doc_path = text_col 7;
  }

let insert_postmortem ~db ~session_key ~pattern ~evidence_json
    ~correction_injected ~doc_path =
  let sql =
    "INSERT INTO postmortems (session_key, pattern, evidence_json, \
     correction_injected, doc_path) VALUES (?, ?, ?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT pattern));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT evidence_json));
      ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT correction_injected));
      ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT doc_path));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Int64.to_int (Sqlite3.last_insert_rowid db)
      | rc ->
          failwith
            (Printf.sprintf "insert_postmortem failed: %s"
               (Sqlite3.Rc.to_string rc)))

let list_postmortems ~db ?session_key ?(limit = 100) () =
  let sql, bind_key =
    match session_key with
    | None ->
        ( "SELECT id, session_key, created_at, pattern, evidence_json, \
           correction_injected, outcome, doc_path FROM postmortems ORDER BY id \
           DESC LIMIT ?",
          false )
    | Some _ ->
        ( "SELECT id, session_key, created_at, pattern, evidence_json, \
           correction_injected, outcome, doc_path FROM postmortems WHERE \
           session_key = ? ORDER BY id DESC LIMIT ?",
          true )
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      (if bind_key then
         match session_key with
         | Some k ->
             ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT k));
             ignore
               (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int limit)))
         | None -> ());
      if not bind_key then
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int limit)));
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        rows := postmortem_of_stmt stmt :: !rows
      done;
      List.rev !rows)

let update_postmortem_outcome ~db ~id ~outcome =
  let sql = "UPDATE postmortems SET outcome = ? WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT outcome));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int id)));
      ignore (Sqlite3.step stmt))

let pending_question_upsert ~db ~session_key ~questions_json ~question_index =
  let sql =
    "INSERT INTO pending_questions (session_key, questions_json, \
     question_index) VALUES (?, ?, ?) ON CONFLICT(session_key) DO UPDATE SET \
     questions_json = excluded.questions_json, question_index = \
     excluded.question_index"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT questions_json));
      ignore
        (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int question_index)));
      ignore (Sqlite3.step stmt))

let pending_question_delete ~db ~session_key =
  let sql = "DELETE FROM pending_questions WHERE session_key = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.step stmt))

let pending_question_list_all ~db =
  let sql =
    "SELECT session_key, questions_json, question_index FROM pending_questions"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let session_key =
          match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let questions_json =
          match Sqlite3.column stmt 1 with
          | Sqlite3.Data.TEXT s -> s
          | _ -> "[]"
        in
        let question_index =
          match Sqlite3.column stmt 2 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0
        in
        rows := (session_key, questions_json, question_index) :: !rows
      done;
      List.rev !rows)

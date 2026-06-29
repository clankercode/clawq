let schema_version = 40

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

let query_single_int_with_params db sql params =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      List.iteri
        (fun i p -> ignore (Sqlite3.bind stmt (i + 1) p : Sqlite3.Rc.t))
        params;
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0)
      | _ -> 0)

let exec_with_params db sql params =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      List.iteri
        (fun i p -> ignore (Sqlite3.bind stmt (i + 1) p : Sqlite3.Rc.t))
        params;
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "SQLite error: %s (sql: %s)"
               (Sqlite3.Rc.to_string rc) sql))

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

let column_exists db table_name column_name =
  if not (table_exists db table_name) then false
  else
    let stmt =
      Sqlite3.prepare db (Printf.sprintf "PRAGMA table_info(%s)" table_name)
    in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        let found = ref false in
        while (not !found) && Sqlite3.step stmt = Sqlite3.Rc.ROW do
          match Sqlite3.column stmt 1 with
          | Sqlite3.Data.TEXT s when s = column_name -> found := true
          | _ -> ()
        done;
        !found)

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
    \     debug_enabled INTEGER NOT NULL DEFAULT 0,\n\
    \     model_override TEXT DEFAULT NULL,\n\
    \     effective_cwd TEXT DEFAULT NULL,\n\
    \     CHECK ((channel IS NULL) = (channel_id IS NULL))\n\
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

let init_quota_history_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS quota_history (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     provider TEXT NOT NULL,\n\
    \     state_json TEXT NOT NULL,\n\
    \     fetched_at REAL NOT NULL,\n\
    \     recorded_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_quota_history_provider ON \
     quota_history(provider, recorded_at)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_quota_history_recorded ON \
     quota_history(recorded_at)"

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
    \     deprecated INTEGER NOT NULL DEFAULT 0,\n\
    \     unavailable INTEGER NOT NULL DEFAULT 0,\n\
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
    \     profile_id INTEGER,\n\
    \     provider TEXT NOT NULL,\n\
    \     model TEXT NOT NULL,\n\
    \     prompt_tokens INTEGER NOT NULL,\n\
    \     completion_tokens INTEGER NOT NULL,\n\
    \     cost_usd REAL,\n\
    \     added_prompt_tokens INTEGER,\n\
    \     cached_tokens INTEGER,\n\
    \     latency_ms INTEGER,\n\
    \     requested_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_request_stats_session ON \
     request_stats(session_key)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_request_stats_model ON \
     request_stats(model, requested_at)";
  try
    exec_exn db
      "CREATE INDEX IF NOT EXISTS idx_request_stats_profile_time ON \
       request_stats(profile_id, requested_at)"
  with _ -> ()

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
    \     thinking_content TEXT,\n\
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

let init_ec_reports_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS ec_reports (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     timestamp TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     error_hash TEXT NOT NULL,\n\
    \     error_context TEXT NOT NULL,\n\
    \     diagnoses_json TEXT,\n\
    \     voting_json TEXT,\n\
    \     winning_plan TEXT,\n\
    \     fix_task_id INTEGER,\n\
    \     status TEXT NOT NULL DEFAULT 'pending'\n\
    \   )"

let init_session_archive_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS session_archives (\n\
    \     archive_id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     session_key TEXT NOT NULL,\n\
    \     archived_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     message_count INTEGER NOT NULL DEFAULT 0,\n\
    \     epoch_count INTEGER NOT NULL DEFAULT 0,\n\
    \     first_message_at TEXT,\n\
    \     last_message_at TEXT\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_session_archives_key ON \
     session_archives(session_key, archive_id DESC)";
  exec_exn db
    "CREATE TABLE IF NOT EXISTS session_archive_messages (\n\
    \     archive_id INTEGER NOT NULL,\n\
    \     ordinal INTEGER NOT NULL,\n\
    \     role TEXT NOT NULL,\n\
    \     content TEXT NOT NULL,\n\
    \     tool_call_id TEXT,\n\
    \     tool_name TEXT,\n\
    \     tool_calls_json TEXT,\n\
    \     provider_response_items_json TEXT,\n\
    \     thinking_content TEXT,\n\
    \     created_at TEXT NOT NULL,\n\
    \     PRIMARY KEY (archive_id, ordinal),\n\
    \     FOREIGN KEY (archive_id) REFERENCES session_archives(archive_id) ON \
     DELETE CASCADE\n\
    \   )";
  exec_exn db
    "CREATE TABLE IF NOT EXISTS session_archive_epochs (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     archive_id INTEGER NOT NULL,\n\
    \     orig_epoch_id INTEGER,\n\
    \     ordinal INTEGER NOT NULL,\n\
    \     message_count INTEGER NOT NULL DEFAULT 0,\n\
    \     first_message_at TEXT,\n\
    \     last_message_at TEXT,\n\
    \     orig_archived_at TEXT,\n\
    \     FOREIGN KEY (archive_id) REFERENCES session_archives(archive_id) ON \
     DELETE CASCADE\n\
    \   )";
  exec_exn db
    "CREATE TABLE IF NOT EXISTS session_archive_epoch_messages (\n\
    \     archive_epoch_id INTEGER NOT NULL,\n\
    \     ordinal INTEGER NOT NULL,\n\
    \     role TEXT NOT NULL,\n\
    \     content TEXT NOT NULL,\n\
    \     tool_call_id TEXT,\n\
    \     tool_name TEXT,\n\
    \     tool_calls_json TEXT,\n\
    \     provider_response_items_json TEXT,\n\
    \     thinking_content TEXT,\n\
    \     created_at TEXT NOT NULL,\n\
    \     PRIMARY KEY (archive_epoch_id, ordinal),\n\
    \     FOREIGN KEY (archive_epoch_id) REFERENCES session_archive_epochs(id) \
     ON DELETE CASCADE\n\
    \   )";
  exec_exn db
    "CREATE TABLE IF NOT EXISTS session_archive_metadata (\n\
    \     archive_id INTEGER PRIMARY KEY,\n\
    \     session_state_json TEXT,\n\
    \     workspace_state_json TEXT,\n\
    \     summaries_json TEXT,\n\
    \     FOREIGN KEY (archive_id) REFERENCES session_archives(archive_id) ON \
     DELETE CASCADE\n\
    \   )"

let init_connector_history_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS connector_history (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     session_key TEXT NOT NULL,\n\
    \     room_id TEXT NOT NULL DEFAULT '',\n\
    \     connector_type TEXT NOT NULL DEFAULT '',\n\
    \     channel_type TEXT NOT NULL,\n\
    \     sender_name TEXT NOT NULL,\n\
    \     sender_id TEXT NOT NULL,\n\
    \     text TEXT,\n\
    \     metadata_json TEXT,\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )";
  (try
     exec_exn db
       "ALTER TABLE connector_history ADD COLUMN room_id TEXT NOT NULL DEFAULT \
        ''"
   with _ -> ());
  (try
     exec_exn db
       "ALTER TABLE connector_history ADD COLUMN connector_type TEXT NOT NULL \
        DEFAULT ''"
   with _ -> ());
  exec_exn db
    "UPDATE connector_history SET room_id = session_key WHERE room_id = ''";
  exec_exn db
    "UPDATE connector_history SET connector_type = channel_type WHERE \
     connector_type = ''";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_connector_history_session ON \
     connector_history (session_key, id ASC)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_connector_history_created ON \
     connector_history (created_at)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_connector_history_room_connector_created \
     ON connector_history (room_id, connector_type, created_at)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_connector_history_connector_created ON \
     connector_history (connector_type, created_at)"

let init_attachment_log_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS attachment_log (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     session_key TEXT NOT NULL,\n\
    \     source TEXT NOT NULL,\n\
    \     filename TEXT NOT NULL,\n\
    \     mime_type TEXT NOT NULL,\n\
    \     size_bytes INTEGER NOT NULL,\n\
    \     saved_path TEXT NOT NULL,\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_attachment_log_session ON \
     attachment_log(session_key)"

let log_attachment_download ~db ~session_key ~source ~filename ~mime_type
    ~size_bytes ~saved_path =
  let stmt =
    Sqlite3.prepare db
      "INSERT INTO attachment_log (session_key, source, filename, mime_type, \
       size_bytes, saved_path) VALUES (?, ?, ?, ?, ?, ?)"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind_text stmt 1 session_key);
      ignore (Sqlite3.bind_text stmt 2 source);
      ignore (Sqlite3.bind_text stmt 3 filename);
      ignore (Sqlite3.bind_text stmt 4 mime_type);
      ignore (Sqlite3.bind_int stmt 5 size_bytes);
      ignore (Sqlite3.bind_text stmt 6 saved_path);
      ignore (Sqlite3.step stmt))

let add_thinking_content_columns db =
  (try exec_exn db "ALTER TABLE messages ADD COLUMN thinking_content TEXT"
   with _ -> ());
  try
    exec_exn db
      "ALTER TABLE session_log_epoch_messages ADD COLUMN thinking_content TEXT"
  with _ -> ()

let init_room_profiles_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS room_profiles (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     name TEXT NOT NULL UNIQUE,\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     updated_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )"

let init_room_profile_bindings_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS room_profile_bindings (\n\
    \     room_id TEXT NOT NULL PRIMARY KEY,\n\
    \     profile_id INTEGER NOT NULL UNIQUE,\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     FOREIGN KEY (profile_id) REFERENCES room_profiles(id) ON DELETE \
     CASCADE\n\
    \   )"

let init_room_activity_ledger_schema db = Room_activity_ledger.init_schema db

let repair_room_profile_tables db =
  let profiles_modern = column_exists db "room_profiles" "name" in
  let bindings_modern = column_exists db "room_profile_bindings" "room_id" in
  if (not profiles_modern) || not bindings_modern then begin
    exec_exn db "DROP TABLE IF EXISTS room_profile_bindings_legacy_repair";
    exec_exn db "DROP TABLE IF EXISTS room_profiles_legacy_repair";
    if table_exists db "room_profile_bindings" then
      exec_exn db
        "ALTER TABLE room_profile_bindings RENAME TO \
         room_profile_bindings_legacy_repair";
    if table_exists db "room_profiles" then
      exec_exn db
        "ALTER TABLE room_profiles RENAME TO room_profiles_legacy_repair";
    init_room_profiles_schema db;
    init_room_profile_bindings_schema db;
    if table_exists db "room_profiles_legacy_repair" then
      exec_exn db
        "INSERT OR IGNORE INTO room_profiles (name) SELECT DISTINCT id FROM \
         room_profiles_legacy_repair WHERE id IS NOT NULL AND id <> ''";
    if table_exists db "room_profile_bindings_legacy_repair" then begin
      if column_exists db "room_profile_bindings_legacy_repair" "profile_id"
      then
        exec_exn db
          "INSERT OR IGNORE INTO room_profiles (name) SELECT DISTINCT \
           profile_id FROM room_profile_bindings_legacy_repair WHERE \
           profile_id IS NOT NULL AND profile_id <> ''";
      if
        column_exists db "room_profile_bindings_legacy_repair" "room"
        && column_exists db "room_profile_bindings_legacy_repair" "profile_id"
      then
        exec_exn db
          "INSERT OR REPLACE INTO room_profile_bindings (room_id, profile_id, \
           created_at) SELECT b.room, p.id, b.created_at FROM \
           room_profile_bindings_legacy_repair b JOIN room_profiles p ON \
           p.name = b.profile_id WHERE b.room IS NOT NULL AND b.room <> '' AND \
           COALESCE(b.active, 1) <> 0"
    end
  end

let init_scoped_memory_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS memory_scopes (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     kind TEXT NOT NULL CHECK (kind IN ('personal', 'room', 'thread', \n\
     'workspace', 'legacy')),\n\
    \     key TEXT NOT NULL,\n\
    \     profile_id INTEGER,\n\
    \     parent_scope_id INTEGER,\n\
    \     provenance TEXT NOT NULL DEFAULT 'unknown',\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     updated_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     UNIQUE(kind, key),\n\
    \     CHECK (parent_scope_id IS NULL OR parent_scope_id <> id),\n\
    \     FOREIGN KEY (profile_id) REFERENCES room_profiles(id) ON DELETE SET \n\
     NULL,\n\
    \     FOREIGN KEY (parent_scope_id) REFERENCES memory_scopes(id) ON DELETE \n\
     SET NULL\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_memory_scopes_profile ON \n\
    \     memory_scopes(profile_id)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_memory_scopes_parent ON \n\
    \     memory_scopes(parent_scope_id)";
  exec_exn db
    "CREATE TABLE IF NOT EXISTS scoped_memories (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     scope_id INTEGER NOT NULL,\n\
    \     content TEXT,\n\
    \     reference TEXT,\n\
    \     provenance TEXT NOT NULL DEFAULT 'unknown',\n\
    \     visibility TEXT NOT NULL DEFAULT 'public' CHECK (visibility IN      \
     ('public', 'private', 'team')),\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     updated_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     redacted_at TEXT,\n\
    \     redaction_reason TEXT,\n\
    \     redaction_metadata TEXT,\n\
    \     CHECK (content IS NOT NULL OR reference IS NOT NULL),\n\
    \     FOREIGN KEY (scope_id) REFERENCES memory_scopes(id) ON DELETE CASCADE\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_scoped_memories_scope_created ON \n\
    \     scoped_memories(scope_id, created_at)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_scoped_memories_reference ON \n\
    \     scoped_memories(reference)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_scoped_memories_redacted ON \n\
    \     scoped_memories(redacted_at)";
  exec_exn db
    "CREATE TABLE IF NOT EXISTS memory_grants (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     scope_id INTEGER NOT NULL,\n\
    \     principal_kind TEXT NOT NULL,\n\
    \     principal_id TEXT NOT NULL,\n\
    \     capability TEXT NOT NULL,\n\
    \     grantor_kind TEXT,\n\
    \     grantor_id TEXT,\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     expires_at TEXT,\n\
    \     is_transitive INTEGER NOT NULL DEFAULT 0 CHECK (is_transitive = 0),\n\
    \     UNIQUE(scope_id, principal_kind, principal_id, capability),\n\
    \     FOREIGN KEY (scope_id) REFERENCES memory_scopes(id) ON DELETE CASCADE\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_memory_grants_scope ON \n\
    \     memory_grants(scope_id)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_memory_grants_principal ON \n\
    \     memory_grants(principal_kind, principal_id)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_memory_grants_capability ON \n\
    \     memory_grants(scope_id, capability)";
  exec_exn db
    "CREATE TRIGGER IF NOT EXISTS memory_grants_legacy_read_only_ai BEFORE \n\
     INSERT ON memory_grants WHEN NEW.capability NOT IN ('list', 'read') AND \n\
     EXISTS (SELECT 1 FROM memory_scopes WHERE id = NEW.scope_id AND kind = \n\
     'legacy') BEGIN SELECT RAISE(ABORT, 'legacy memory scope is read-only'); \n\
     END";
  exec_exn db
    "CREATE TRIGGER IF NOT EXISTS memory_grants_legacy_read_only_au BEFORE \n\
     UPDATE ON memory_grants WHEN NEW.capability NOT IN ('list', 'read') AND \n\
     EXISTS (SELECT 1 FROM memory_scopes WHERE id = NEW.scope_id AND kind = \n\
     'legacy') BEGIN SELECT RAISE(ABORT, 'legacy memory scope is read-only'); \n\
     END";
  exec_exn db
    "INSERT OR IGNORE INTO memory_scopes (kind, key, provenance) VALUES \n\
     ('legacy', 'core', 'system')";
  exec_exn db
    "DELETE FROM memory_grants WHERE capability NOT IN ('list', 'read') AND \n\
     scope_id IN (SELECT id FROM memory_scopes WHERE kind = 'legacy')";
  exec_exn db
    "INSERT OR IGNORE INTO memory_grants (scope_id, principal_kind, \n\
     principal_id, capability, grantor_kind, grantor_id) SELECT id, 'system', \n\
     'legacy', 'list', 'system', 'seed' FROM memory_scopes WHERE kind = \n\
     'legacy' AND key = 'core'";
  exec_exn db
    "INSERT OR IGNORE INTO memory_grants (scope_id, principal_kind, \n\
     principal_id, capability, grantor_kind, grantor_id) SELECT id, 'system', \n\
     'legacy', 'read', 'system', 'seed' FROM memory_scopes WHERE kind = \n\
     'legacy' AND key = 'core'";
  exec_exn db
    "CREATE TABLE IF NOT EXISTS memory_team_grants (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     memory_id INTEGER NOT NULL,\n\
    \     principal_kind TEXT NOT NULL,\n\
    \     principal_id TEXT NOT NULL,\n\
    \     granted_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     UNIQUE(memory_id, principal_kind, principal_id),\n\
    \     FOREIGN KEY (memory_id) REFERENCES scoped_memories(id) ON DELETE \
     CASCADE\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_memory_team_grants_memory ON\n\
    \     memory_team_grants(memory_id)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_memory_team_grants_principal ON\n\
    \     memory_team_grants(principal_kind, principal_id)"

let init_session_repos_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS session_repos (\n\
    \     session_key TEXT PRIMARY KEY,\n\
    \     repo_url TEXT,\n\
    \     local_path TEXT NOT NULL,\n\
    \     is_managed INTEGER NOT NULL DEFAULT 0,\n\
    \     last_fetched_at TEXT,\n\
    \     last_fetch_error TEXT,\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )"

let init_debate_rounds_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS debate_rounds (\n\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \  prompt TEXT NOT NULL,\n\
    \  models_json TEXT NOT NULL,\n\
    \  responses_json TEXT NOT NULL,\n\
    \  judge_model TEXT,\n\
    \  judge_result_json TEXT,\n\
    \  confidence INTEGER,\n\
    \  total_cost_usd REAL,\n\
    \  elapsed_s REAL,\n\
    \  created_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \  )"

(* Ensure all tables that are only created inside migrate_schema exist.
   Uses CREATE TABLE IF NOT EXISTS throughout, so safe to call at any point. *)
let ensure_all_tables db =
  init_session_schema db;
  init_inbound_queue_schema db;
  init_models_cache_schema db;
  init_request_stats_schema db;
  init_quota_cache_schema db;
  init_quota_history_schema db;
  init_postmortems_schema db;
  init_model_discovery_state_schema db;
  Summary_store.init_schema db;
  init_pending_questions_schema db;
  init_ec_reports_schema db;
  init_session_archive_schema db;
  init_connector_history_schema db;
  init_attachment_log_schema db;
  Admin.init_schema db;
  Pair_coding_state.init_schema db;
  init_debate_rounds_schema db;
  init_session_repos_schema db;
  init_room_profiles_schema db;
  init_room_profile_bindings_schema db;
  init_room_activity_ledger_schema db;
  Room_progress_checklist.init_schema db;
  Room_budget.init_schema db;
  init_scoped_memory_schema db

(* Each step migrates from version [v] to [v + 1].
   All ALTER TABLE operations use try/catch for idempotency.
   The caller bumps schema_version after each step. *)
let migrate_step db v =
  match v with
  | 1 -> (
      try
        exec_exn db
          "ALTER TABLE messages ADD COLUMN provider_response_items_json TEXT"
      with exn ->
        Logs.warn (fun m ->
            m
              "[memory_0_schema] migration step 1 failed (may already exist): \
               %s"
              (Printexc.to_string exn)))
  | 2 | 5 | 6 | 11 | 14 | 17 | 18 | 21 | 23 | 25 -> ()
  | 3 ->
      init_models_cache_schema db;
      init_request_stats_schema db
  | 4 -> (
      try
        exec_exn db
          "ALTER TABLE session_state ADD COLUMN keepalive_enabled INTEGER NOT \
           NULL DEFAULT 0"
      with exn ->
        Logs.warn (fun m ->
            m
              "[memory_0_schema] migration step 4 failed (may already exist): \
               %s"
              (Printexc.to_string exn)))
  | 7 -> init_quota_cache_schema db
  | 8 -> (
      try
        exec_exn db
          "ALTER TABLE session_state ADD COLUMN model_override TEXT DEFAULT \
           NULL"
      with exn ->
        Logs.warn (fun m ->
            m
              "[memory_0_schema] migration step 8 failed (may already exist): \
               %s"
              (Printexc.to_string exn)))
  | 9 -> init_postmortems_schema db
  | 10 ->
      init_model_discovery_state_schema db;
      Summary_store.init_schema db
  | 12 -> (
      try
        exec_exn db
          "ALTER TABLE request_stats ADD COLUMN added_prompt_tokens INTEGER"
      with exn ->
        Logs.warn (fun m ->
            m
              "[memory_0_schema] migration step 12 failed (may already exist): \
               %s"
              (Printexc.to_string exn)))
  | 13 -> (
      init_pending_questions_schema db;
      try
        exec_exn db
          "ALTER TABLE session_state ADD COLUMN heartbeat_enabled INTEGER NOT \
           NULL DEFAULT 0"
      with exn ->
        Logs.warn (fun m ->
            m
              "[memory_0_schema] migration step 13 failed (may already exist): \
               %s"
              (Printexc.to_string exn)))
  | 15 ->
      (* Recreate session_state with CHECK constraint.
         SQLite does not support ADD CONSTRAINT, so use
         CREATE-INSERT-Drop-Rename. Drop leftover from partial runs. *)
      exec_exn db "DROP TABLE IF EXISTS session_state_new";
      exec_exn db
        "CREATE TABLE session_state_new (session_key TEXT PRIMARY KEY, turn \
         TEXT NOT NULL DEFAULT 'user', channel TEXT, channel_id TEXT, \
         response_sent_at TEXT, last_active TEXT NOT NULL DEFAULT \
         (datetime('now')), keepalive_enabled INTEGER NOT NULL DEFAULT 0, \
         heartbeat_enabled INTEGER NOT NULL DEFAULT 0, model_override TEXT \
         DEFAULT NULL, CHECK ((channel IS NULL) = (channel_id IS NULL)) )";
      exec_exn db
        "INSERT INTO session_state_new (session_key, turn, channel, \
         channel_id, response_sent_at, last_active, keepalive_enabled, \
         heartbeat_enabled, model_override) SELECT session_key, turn, channel, \
         channel_id, response_sent_at, last_active, keepalive_enabled, \
         heartbeat_enabled, model_override FROM session_state WHERE (channel \
         IS NULL) = (channel_id IS NULL)";
      exec_exn db "DROP TABLE session_state";
      exec_exn db "ALTER TABLE session_state_new RENAME TO session_state"
  | 16 ->
      init_ec_reports_schema db;
      init_quota_history_schema db
  | 19 -> (
      try
        exec_exn db "ALTER TABLE request_stats ADD COLUMN cached_tokens INTEGER"
      with exn ->
        Logs.warn (fun m ->
            m
              "[memory_0_schema] migration step 19 failed (may already exist): \
               %s"
              (Printexc.to_string exn)))
  | 20 ->
      init_session_archive_schema db;
      init_connector_history_schema db;
      init_attachment_log_schema db
  | 22 -> Admin.init_schema db
  | 24 -> Pair_coding_state.init_schema db
  | 26 -> (
      try
        exec_exn db
          "ALTER TABLE session_state ADD COLUMN effective_cwd TEXT DEFAULT NULL"
      with exn ->
        Logs.warn (fun m ->
            m
              "[memory_0_schema] migration step 26 failed (may already exist): \
               %s"
              (Printexc.to_string exn)))
  | 27 -> init_debate_rounds_schema db
  | 28 -> init_session_repos_schema db
  | 29 -> (
      try
        exec_exn db
          "ALTER TABLE session_state ADD COLUMN debug_enabled INTEGER NOT NULL \
           DEFAULT 0"
      with exn ->
        Logs.warn (fun m ->
            m
              "[memory_0_schema] migration step 29 failed (may already exist): \
               %s"
              (Printexc.to_string exn)))
  | 30 -> (
      (try
         exec_exn db
           "ALTER TABLE models_cache ADD COLUMN deprecated INTEGER NOT NULL \
            DEFAULT 0"
       with exn ->
         Logs.warn (fun m ->
             m "[memory_0_schema] migration step 30 (deprecated) failed: %s"
               (Printexc.to_string exn)));
      try
        exec_exn db
          "ALTER TABLE models_cache ADD COLUMN unavailable INTEGER NOT NULL \
           DEFAULT 0"
      with exn ->
        Logs.warn (fun m ->
            m "[memory_0_schema] migration step 30 (unavailable) failed: %s"
              (Printexc.to_string exn)))
  | 31 ->
      init_room_profiles_schema db;
      init_room_profile_bindings_schema db
  | 32 ->
      (* Add room-origin columns to task_tree and background_tasks *)
      let try_add sql = try exec_exn db sql with _ -> () in
      try_add "ALTER TABLE task_tree ADD COLUMN profile_id INTEGER";
      try_add "ALTER TABLE task_tree ADD COLUMN origin_json TEXT";
      try_add "ALTER TABLE task_tree ADD COLUMN thread_id TEXT";
      try_add "ALTER TABLE task_tree ADD COLUMN requester TEXT";
      try_add "ALTER TABLE background_tasks ADD COLUMN profile_id INTEGER";
      try_add "ALTER TABLE background_tasks ADD COLUMN origin_json TEXT";
      try_add "ALTER TABLE background_tasks ADD COLUMN thread_id TEXT";
      try_add "ALTER TABLE background_tasks ADD COLUMN requester TEXT"
  | 33 -> init_scoped_memory_schema db
  | 34 -> init_connector_history_schema db
  | 35 ->
      let try_add sql = try exec_exn db sql with _ -> () in
      try_add "ALTER TABLE request_stats ADD COLUMN profile_id INTEGER";
      try_add "ALTER TABLE request_stats ADD COLUMN latency_ms INTEGER";
      exec_exn db
        "CREATE INDEX IF NOT EXISTS idx_request_stats_profile_time ON \
         request_stats(profile_id, requested_at)"
  | 37 -> init_room_activity_ledger_schema db
  | 38 -> Room_progress_checklist.init_schema db
  | 36 -> Room_budget.init_schema db
  | 39 ->
      (* Add visibility column to scoped_memories and create team_grants table *)
      let try_add sql = try exec_exn db sql with _ -> () in
      try_add
        "ALTER TABLE scoped_memories ADD COLUMN visibility TEXT NOT NULL \
         DEFAULT 'public' CHECK (visibility IN ('public', 'private', 'team'))";
      init_scoped_memory_schema db
  | n -> failwith (Printf.sprintf "Unknown migration step from version %d" n)

(* Idempotent column repair for databases that reached the current schema
   version but are missing columns due to partial or buggy earlier migrations.
   Each ALTER TABLE ADD COLUMN is wrapped in try/catch -- it's a no-op if the
   column already exists. *)
let repair_missing_columns db =
  let try_add sql = try exec_exn db sql with _ -> () in
  try_add "ALTER TABLE messages ADD COLUMN provider_response_items_json TEXT";
  try_add
    "ALTER TABLE session_state ADD COLUMN keepalive_enabled INTEGER NOT NULL \
     DEFAULT 0";
  try_add
    "ALTER TABLE session_state ADD COLUMN model_override TEXT DEFAULT NULL";
  try_add "ALTER TABLE request_stats ADD COLUMN added_prompt_tokens INTEGER";
  try_add
    "ALTER TABLE session_state ADD COLUMN heartbeat_enabled INTEGER NOT NULL \
     DEFAULT 0";
  try_add
    "ALTER TABLE session_state ADD COLUMN debug_enabled INTEGER NOT NULL \
     DEFAULT 0";
  try_add "ALTER TABLE request_stats ADD COLUMN cached_tokens INTEGER";
  try_add "ALTER TABLE request_stats ADD COLUMN profile_id INTEGER";
  try_add "ALTER TABLE request_stats ADD COLUMN latency_ms INTEGER";
  try_add "ALTER TABLE session_state ADD COLUMN effective_cwd TEXT DEFAULT NULL";
  try_add "ALTER TABLE task_tree ADD COLUMN deleted_at TEXT DEFAULT NULL";
  try_add
    "ALTER TABLE models_cache ADD COLUMN deprecated INTEGER NOT NULL DEFAULT 0";
  try_add
    "ALTER TABLE models_cache ADD COLUMN unavailable INTEGER NOT NULL DEFAULT 0";
  try_add "ALTER TABLE task_tree ADD COLUMN profile_id INTEGER";
  try_add "ALTER TABLE task_tree ADD COLUMN origin_json TEXT";
  try_add "ALTER TABLE task_tree ADD COLUMN thread_id TEXT";
  try_add "ALTER TABLE task_tree ADD COLUMN requester TEXT";
  try_add "ALTER TABLE background_tasks ADD COLUMN profile_id INTEGER";
  try_add "ALTER TABLE background_tasks ADD COLUMN origin_json TEXT";
  try_add "ALTER TABLE background_tasks ADD COLUMN thread_id TEXT";
  try_add "ALTER TABLE background_tasks ADD COLUMN requester TEXT";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_request_stats_profile_time ON \
     request_stats(profile_id, requested_at)";
  init_connector_history_schema db;
  init_room_activity_ledger_schema db;
  Room_progress_checklist.init_schema db;
  Room_budget.init_schema db;
  repair_room_profile_tables db;
  init_scoped_memory_schema db;
  try_add
    "ALTER TABLE scoped_memories ADD COLUMN visibility TEXT NOT NULL DEFAULT \
     'public' CHECK (visibility IN ('public', 'private', 'team'))"

let migrate_schema db current_version =
  match current_version with
  | 0 ->
      (* Fresh database: create all tables with current schema *)
      ensure_all_tables db;
      repair_missing_columns db;
      add_thinking_content_columns db;
      exec_exn db
        (Printf.sprintf "INSERT INTO schema_version (version) VALUES (%d)"
           schema_version)
  | n when n > 0 && n < schema_version ->
      (* Ensure base tables exist before steps that ALTER them *)
      ensure_all_tables db;
      (* Sequential migration: apply each step from current to latest *)
      for v = n to schema_version - 1 do
        migrate_step db v;
        set_schema_version db (v + 1)
      done;
      ensure_all_tables db;
      repair_missing_columns db;
      add_thinking_content_columns db
  | n when n = schema_version ->
      (* Already at current version: ensure all tables exist *)
      ensure_all_tables db;
      repair_missing_columns db
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

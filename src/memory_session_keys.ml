(* Keep legacy identity repair below Session: the caller supplies its exact
   sanitizer. Never union histories through this lossy mapping. *)
let query db sql params of_stmt =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      Sql_util.bind_params stmt params;
      let rec collect acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> collect (of_stmt stmt :: acc)
        | Sqlite3.Rc.DONE -> List.rev acc
        | rc ->
            failwith
              ("Legacy session reconciliation: " ^ Sqlite3.Rc.to_string rc)
      in
      collect [])

let text key = Sqlite3.Data.TEXT key

let exec db sql params =
  Sql_util.exec_with_params ~label:"Legacy session reconciliation" db sql params

let table_exists db table =
  query db
    "SELECT 1 FROM sqlite_master WHERE name = ? AND type IN ('table', 'view')"
    [ text table ]
    (fun _ -> ())
  <> []

let init_schema db =
  Sql_util.exec_schema db
    "CREATE TABLE IF NOT EXISTS session_key_reconciliations (raw_key TEXT \
     PRIMARY KEY, canonical_key TEXT NOT NULL, decision TEXT NOT NULL, reason \
     TEXT NOT NULL, source_metadata_json TEXT NOT NULL, created_at TEXT NOT \
     NULL DEFAULT (datetime('now')))";
  Sql_util.exec_schema db
    "CREATE INDEX IF NOT EXISTS idx_session_key_reconciliations_canonical ON \
     session_key_reconciliations(canonical_key)"

let blocked_legacy_github_key ~db key =
  String.starts_with ~prefix:"github:" key
  && table_exists db "session_key_reconciliations"
  && query db "SELECT 1 FROM session_key_reconciliations WHERE raw_key = ?"
       [ text key ]
       (fun _ -> ())
     <> []

let has_rows db table key =
  table_exists db table
  && query db
       ("SELECT 1 FROM " ^ table ^ " WHERE session_key = ? LIMIT 1")
       [ text key ]
       (fun _ -> ())
     <> []

let metadata_tables =
  [
    "session_state";
    "session_workspace_state";
    "session_repos";
    "pending_questions";
  ]

let append_tables =
  [
    "messages";
    "session_log_epochs";
    "session_archives";
    "inbound_queue";
    "summaries";
    "embeddings";
    "task_tree";
    "task_tree_archive";
  ]

let json_data = function
  | Sqlite3.Data.NULL | Sqlite3.Data.NONE -> `Null
  | Sqlite3.Data.TEXT s -> `String s
  | Sqlite3.Data.INT n -> `Intlit (Int64.to_string n)
  | Sqlite3.Data.FLOAT n -> `Float n
  | Sqlite3.Data.BLOB s ->
      `Assoc
        [
          ( "blob_hex",
            `String
              (String.to_seq s
              |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
              |> List.of_seq |> String.concat "") );
        ]

let metadata_snapshot db key =
  metadata_tables
  |> List.map (fun table ->
      let values =
        if not (table_exists db table) then []
        else
          query db
            ("SELECT * FROM " ^ table ^ " WHERE session_key = ?")
            [ text key ]
            (fun stmt ->
              `Assoc
                (List.init (Sqlite3.column_count stmt) (fun i ->
                     ( Sqlite3.column_name stmt i,
                       json_data (Sqlite3.column stmt i) ))))
      in
      (table, `List values))
  |> fun fields -> Yojson.Safe.to_string (`Assoc fields)

let promote_state db raw canonical =
  exec db
    "INSERT INTO session_state \
     (session_key,turn,channel,channel_id,response_sent_at,last_active,keepalive_enabled,heartbeat_enabled,debug_enabled,model_override,effective_cwd) \
     SELECT \
     ?,turn,channel,channel_id,response_sent_at,last_active,keepalive_enabled,heartbeat_enabled,debug_enabled,model_override,effective_cwd \
     FROM session_state WHERE session_key = ? ON CONFLICT(session_key) DO \
     UPDATE SET turn=excluded.turn, channel=excluded.channel, \
     channel_id=excluded.channel_id, \
     response_sent_at=excluded.response_sent_at, \
     last_active=excluded.last_active, \
     model_override=COALESCE(session_state.model_override,excluded.model_override), \
     effective_cwd=COALESCE(session_state.effective_cwd,excluded.effective_cwd)"
    [ text canonical; text raw ];
  exec db "DELETE FROM session_state WHERE session_key = ?" [ text raw ]

let promote_single db table raw canonical =
  if table_exists db table then
    if has_rows db table canonical then
      (* Displaced settings remain in the reconciliation receipt. *)
      exec db ("DELETE FROM " ^ table ^ " WHERE session_key = ?") [ text raw ]
    else
      exec db
        ("UPDATE " ^ table ^ " SET session_key = ? WHERE session_key = ?")
        [ text canonical; text raw ]

let promote db raw canonical =
  let indexed = table_exists db "messages_fts" && has_rows db "messages" raw in
  if indexed then
    exec db
      "INSERT INTO messages_fts(messages_fts,rowid,content,session_key) SELECT \
       'delete',id,content,session_key FROM messages WHERE session_key = ?"
      [ text raw ];
  List.iter
    (fun table ->
      if table_exists db table then
        exec db
          ("UPDATE " ^ table ^ " SET session_key = ? WHERE session_key = ?")
          [ text canonical; text raw ])
    append_tables;
  if indexed then
    exec db
      "INSERT INTO messages_fts(rowid,content,session_key) SELECT \
       id,content,session_key FROM messages WHERE session_key = ?"
      [ text canonical ];
  if table_exists db "embeddings" then
    exec db
      "UPDATE embeddings SET scope_key = ? WHERE session_key = ? AND \
       scope_kind = 'session' AND scope_key = ?"
      [ text canonical; text canonical; text raw ];
  promote_state db raw canonical;
  List.iter
    (fun table ->
      if table <> "session_state" then promote_single db table raw canonical)
    metadata_tables

let settings_only db key =
  query db
    "SELECT 1 FROM session_state WHERE session_key = ? AND (turn <> 'user' OR \
     channel IS NOT NULL OR response_sent_at IS NOT NULL)"
    [ text key ]
    (fun _ -> ())
  = []

let conflicting_reason db ~raw ~canonical ~aliases =
  if has_rows db "messages" canonical then
    Some "canonical current history takes precedence"
  else if List.length aliases <> 1 then
    Some "multiple raw aliases map to one canonical key"
  else if
    query db
      "SELECT 1 FROM session_key_reconciliations WHERE canonical_key = ? LIMIT \
       1"
      [ text canonical ]
      (fun _ -> ())
    <> []
  then Some "canonical identity already has a reconciliation decision"
  else if
    has_rows db "session_archives" canonical
    || has_rows db "session_log_epochs" canonical
  then Some "canonical archive/reset or compaction evidence exists"
  else if not (settings_only db canonical) then
    Some "canonical routing or work state already exists"
  else if
    List.exists
      (fun table -> has_rows db table raw && has_rows db table canonical)
      [ "pending_questions"; "task_tree" ]
  then Some "canonical and raw live metadata collide"
  else
    let repo =
      match String.split_on_char ':' raw with _ :: repo :: _ -> repo | _ -> ""
    in
    if
      query db
        "SELECT 1 FROM session_state WHERE session_key = ? AND channel IS NOT \
         NULL AND (channel <> 'github' OR channel_id <> ?)"
        [ text raw; text repo ]
        (fun _ -> ())
      <> []
    then Some "legacy routing disagrees with the GitHub identity"
    else None

module Key_map = Map.Make (String)
module Key_set = Set.Make (String)

let reconcile_legacy_github_keys ~db ~canonicalize =
  init_schema db;
  Sql_util.exec_exn db "SAVEPOINT reconcile_legacy_github_keys";
  let diagnostics = ref [] in
  (try
     let keys =
       List.fold_left
         (fun keys table ->
           if not (table_exists db table) then keys
           else
             query db
               ("SELECT DISTINCT session_key FROM " ^ table
              ^ " WHERE session_key GLOB 'github:*'")
               []
               (fun stmt -> Sql_util.text_column stmt 0)
             |> List.fold_left (fun keys key -> Key_set.add key keys) keys)
         Key_set.empty
         (metadata_tables @ append_tables)
     in
     let groups =
       Key_set.fold
         (fun raw groups ->
           let canonical = canonicalize raw in
           if raw = canonical then groups
           else
             Key_map.update canonical
               (fun previous -> Some (raw :: Option.value ~default:[] previous))
               groups)
         keys Key_map.empty
     in
     Key_map.iter
       (fun canonical aliases ->
         List.iter
           (fun raw ->
             let existing =
               query db
                 "SELECT decision,reason FROM session_key_reconciliations \
                  WHERE raw_key = ?"
                 [ text raw ]
                 (fun stmt ->
                   (Sql_util.text_column stmt 0, Sql_util.text_column stmt 1))
             in
             match existing with
             | ("deferred", reason) :: _ ->
                 diagnostics := (raw, canonical, reason) :: !diagnostics
             | _ :: _ ->
                 let reason =
                   "raw data reappeared after prior promotion; preserved \
                    without reimport"
                 in
                 exec db
                   "UPDATE session_key_reconciliations SET decision = \
                    'deferred', reason = ? WHERE raw_key = ?"
                   [ text reason; text raw ];
                 diagnostics := (raw, canonical, reason) :: !diagnostics
             | [] -> (
                 let reason = conflicting_reason db ~raw ~canonical ~aliases in
                 let decision, explanation =
                   match reason with
                   | Some reason -> ("deferred", reason)
                   | None ->
                       ( "promoted",
                         "one unambiguous legacy identity; canonical history \
                          absent" )
                 in
                 exec db
                   "INSERT INTO \
                    session_key_reconciliations(raw_key,canonical_key,decision,reason,source_metadata_json) \
                    VALUES (?,?,?,?,?)"
                   [
                     text raw;
                     text canonical;
                     text decision;
                     text explanation;
                     text (metadata_snapshot db raw);
                   ];
                 match reason with
                 | Some reason ->
                     diagnostics := (raw, canonical, reason) :: !diagnostics
                 | None -> promote db raw canonical))
           aliases)
       groups;
     Sql_util.exec_exn db "RELEASE SAVEPOINT reconcile_legacy_github_keys"
   with exn ->
     Sql_util.exec_exn db "ROLLBACK TO SAVEPOINT reconcile_legacy_github_keys";
     Sql_util.exec_exn db "RELEASE SAVEPOINT reconcile_legacy_github_keys";
     raise exn);
  List.iter
    (fun (raw, canonical, reason) ->
      Logs.warn (fun m ->
          m
            "Legacy GitHub identity %S -> %S held for recovery: %s \
             (session_key_reconciliations)"
            raw canonical reason))
    !diagnostics

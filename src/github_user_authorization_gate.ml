(** Durable fail-closed gate shared by leases and vault recovery.

    This module owns only the singleton state-table schema. Recovery owns the
    richer state and audit-event representations layered on that same table. *)

let state_schema =
  {|CREATE TABLE IF NOT EXISTS github_user_token_vault_recovery_state (
      id INTEGER PRIMARY KEY CHECK (id = 1),
      user_authorization_enabled INTEGER NOT NULL DEFAULT 1,
      last_event TEXT NOT NULL DEFAULT 'none',
      last_reason TEXT,
      last_operator_id TEXT,
      last_event_at TEXT,
      compromised_key_ids_json TEXT NOT NULL DEFAULT '[]',
      requires_relink INTEGER NOT NULL DEFAULT 0,
      requires_key_rotation INTEGER NOT NULL DEFAULT 0
    )|}

let seed_sql =
  {|INSERT OR IGNORE INTO github_user_token_vault_recovery_state
      (id, user_authorization_enabled, last_event, compromised_key_ids_json,
       requires_relink, requires_key_rotation)
      VALUES (1, 1, 'none', '[]', 0, 0)|}

let exec ~operation db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "%s failed: %s (%s)" operation (Sqlite3.Rc.to_string rc)
           (Sqlite3.errmsg db))

let table_exists ~db ~name =
  let sql = "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name) with
      | Sqlite3.Rc.OK -> (
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW -> Ok true
          | Sqlite3.Rc.DONE -> Ok false
          | rc ->
              Error
                (Printf.sprintf
                   "inspect user-authorization gate failed: %s (%s)"
                   (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))
      | rc ->
          Error
            (Printf.sprintf "inspect user-authorization gate failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let ensure_schema db =
  match table_exists ~db ~name:"github_user_token_vault_recovery_events" with
  | Error e -> Error e
  | Ok true -> (
      match table_exists ~db ~name:"github_user_token_vault_recovery_state" with
      | Error e -> Error e
      | Ok false ->
          Error
            "user-authorization gate state is missing after recovery was \
             initialized"
      | Ok true -> Ok ())
  | Ok false -> (
      match
        exec ~operation:"create user-authorization gate" db state_schema
      with
      | Error e -> Error e
      | Ok () -> exec ~operation:"seed user-authorization gate" db seed_sql)

let is_enabled ~db =
  match ensure_schema db with
  | Error e -> Error e
  | Ok () ->
      let sql =
        {|SELECT user_authorization_enabled
          FROM github_user_token_vault_recovery_state WHERE id = 1|}
      in
      let stmt = Sqlite3.prepare db sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW -> (
              match Sqlite3.column stmt 0 with
              | Sqlite3.Data.INT value -> Ok (value <> 0L)
              | _ ->
                  Error "user-authorization gate has an invalid enabled value")
          | Sqlite3.Rc.DONE -> Error "user-authorization gate state is missing"
          | rc ->
              Error
                (Printf.sprintf "read user-authorization gate failed: %s (%s)"
                   (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

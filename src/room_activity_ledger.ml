type event = {
  id : int;
  room_id : string;
  event_type : string;
  timestamp : string;
  actor : string;
  metadata : Yojson.Safe.t;
}

let exec_exn db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "room_activity_ledger schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

let init_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS room_activity_ledger (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     room_id TEXT NOT NULL,\n\
    \     event_type TEXT NOT NULL,\n\
    \     timestamp TEXT NOT NULL,\n\
    \     actor TEXT NOT NULL,\n\
    \     metadata TEXT NOT NULL DEFAULT '{}',\n\
    \     UNIQUE(room_id, event_type, timestamp)\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_room_activity_ledger_room_time ON \
     room_activity_ledger(room_id, timestamp)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_room_activity_ledger_type_time ON \
     room_activity_ledger(event_type, timestamp)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_room_activity_ledger_time ON \
     room_activity_ledger(timestamp)"

let metadata_of_string raw =
  try Yojson.Safe.from_string raw with Yojson.Json_error _ -> `Null

let text_column stmt idx =
  match Sqlite3.column stmt idx with Sqlite3.Data.TEXT s -> s | _ -> ""

let int_column stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | _ -> 0

let event_of_stmt stmt =
  {
    id = int_column stmt 0;
    room_id = text_column stmt 1;
    event_type = text_column stmt 2;
    timestamp = text_column stmt 3;
    actor = text_column stmt 4;
    metadata = metadata_of_string (text_column stmt 5);
  }

let bind_params stmt params =
  List.iteri
    (fun i value -> ignore (Sqlite3.bind stmt (i + 1) value : Sqlite3.Rc.t))
    params

let select_one ~db ~room_id ~event_type ~timestamp =
  let sql =
    "SELECT id, room_id, event_type, timestamp, actor, metadata FROM \
     room_activity_ledger WHERE room_id = ? AND event_type = ? AND timestamp = \
     ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          Sqlite3.Data.TEXT room_id;
          Sqlite3.Data.TEXT event_type;
          Sqlite3.Data.TEXT timestamp;
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> event_of_stmt stmt
      | rc ->
          failwith
            (Printf.sprintf
               "room_activity_ledger append failed to read event: %s"
               (Sqlite3.Rc.to_string rc)))

let append ~db ~room_id ~event_type ~timestamp ~actor ~metadata =
  let sql =
    "INSERT OR IGNORE INTO room_activity_ledger (room_id, event_type, \
     timestamp, actor, metadata) VALUES (?, ?, ?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          Sqlite3.Data.TEXT room_id;
          Sqlite3.Data.TEXT event_type;
          Sqlite3.Data.TEXT timestamp;
          Sqlite3.Data.TEXT actor;
          Sqlite3.Data.TEXT (Yojson.Safe.to_string metadata);
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "room_activity_ledger append failed: %s"
               (Sqlite3.Rc.to_string rc)));
  select_one ~db ~room_id ~event_type ~timestamp

let query ?room_id ?event_type ?from_timestamp ?to_timestamp ~db () =
  let filters = ref [] in
  let params = ref [] in
  let add_filter sql value =
    filters := sql :: !filters;
    params := value :: !params
  in
  Option.iter
    (fun value -> add_filter "room_id = ?" (Sqlite3.Data.TEXT value))
    room_id;
  Option.iter
    (fun value -> add_filter "event_type = ?" (Sqlite3.Data.TEXT value))
    event_type;
  Option.iter
    (fun value -> add_filter "timestamp >= ?" (Sqlite3.Data.TEXT value))
    from_timestamp;
  Option.iter
    (fun value -> add_filter "timestamp <= ?" (Sqlite3.Data.TEXT value))
    to_timestamp;
  let where_clause =
    match List.rev !filters with
    | [] -> ""
    | filters -> " WHERE " ^ String.concat " AND " filters
  in
  let sql =
    "SELECT id, room_id, event_type, timestamp, actor, metadata FROM \
     room_activity_ledger" ^ where_clause ^ " ORDER BY timestamp ASC, id ASC"
  in
  let stmt = Sqlite3.prepare db sql in
  let events = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt (List.rev !params);
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            events := event_of_stmt stmt :: !events;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf "room_activity_ledger query failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ());
  List.rev !events

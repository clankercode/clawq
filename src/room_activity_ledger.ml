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

let event_to_json event =
  `Assoc
    [
      ("id", `Int event.id);
      ("room_id", `String event.room_id);
      ("event_type", `String event.event_type);
      ("timestamp", `String event.timestamp);
      ("actor", `String event.actor);
      ("metadata", event.metadata);
    ]

let events_to_json events = `List (List.map event_to_json events)

let events_to_json_string events =
  Yojson.Safe.pretty_to_string (events_to_json events)

let events_to_jsonl events =
  events
  |> List.map (fun event -> event_to_json event |> Yojson.Safe.to_string)
  |> String.concat "\n"

let timestamp_now () =
  let now = Unix.gettimeofday () in
  let tm = Unix.gmtime now in
  let micros = int_of_float ((now -. floor now) *. 1_000_000.0) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%06dZ" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec micros

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

let append_now ~db ~room_id ~event_type ~actor ~metadata =
  append ~db ~room_id ~event_type ~timestamp:(timestamp_now ()) ~actor ~metadata

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

let delete_before ~db ~before_timestamp =
  let stmt =
    Sqlite3.prepare db "DELETE FROM room_activity_ledger WHERE timestamp < ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.TEXT before_timestamp ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db
      | rc ->
          failwith
            (Printf.sprintf "room_activity_ledger retention cleanup failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** {1 Delivery attempt helpers}

    Convenience functions for recording room-progress delivery attempts and
    outcomes. Each event carries structured metadata: connector, room_id,
    thread_id, task_id, activity_id, and a sanitized error on failure. *)

(** Sanitize an error string for safe storage. Strips credentials, tokens, and
    excessively long content. *)
let sanitize_error err =
  let max_len = 500 in
  let trimmed =
    if String.length err > max_len then String.sub err 0 max_len ^ "..."
    else err
  in
  (* Redact common credential patterns *)
  let redacted =
    Str.global_replace
      (Str.regexp "Bearer [A-Za-z0-9._+/=-]+")
      "Bearer [REDACTED]" trimmed
  in
  let redacted =
    Str.global_replace
      (Str.regexp "token[=:][^ &;]+")
      "token=[REDACTED]" redacted
  in
  let redacted =
    Str.global_replace (Str.regexp "key[=:][^ &;]+") "key=[REDACTED]" redacted
  in
  redacted

(** [record_delivery_attempt ~db ~room_id ~connector ~task_id ?thread_id
     ?activity_id ()] records that a delivery was initiated. Returns the ledger
    event. *)
let record_delivery_attempt ~db ~room_id ~connector ~task_id ?thread_id
    ?activity_id () =
  let fields =
    [
      ("connector", `String connector);
      ("room_id", `String room_id);
      ("task_id", `Int task_id);
    ]
  in
  let fields =
    match thread_id with
    | Some tid when String.trim tid <> "" ->
        ("thread_id", `String tid) :: fields
    | _ -> fields
  in
  let fields =
    match activity_id with
    | Some aid when String.trim aid <> "" ->
        ("activity_id", `String aid) :: fields
    | _ -> fields
  in
  append_now ~db ~room_id ~event_type:"delivery_attempt" ~actor:connector
    ~metadata:(`Assoc fields)

(** [record_delivery_success ~db ~room_id ~connector ~task_id ~message_id
     ?thread_id ?activity_id ()] records a successful delivery. The message_id
    must be non-empty and not a placeholder ("0"). *)
let record_delivery_success ~db ~room_id ~connector ~task_id ~message_id
    ?thread_id ?activity_id () =
  let trimmed_message_id = String.trim message_id in
  if trimmed_message_id = "" || trimmed_message_id = "0" then
    invalid_arg
      "record_delivery_success: message_id must be non-empty and not \"0\"";
  let fields =
    [
      ("connector", `String connector);
      ("room_id", `String room_id);
      ("task_id", `Int task_id);
      ("message_id", `String trimmed_message_id);
    ]
  in
  let fields =
    match thread_id with
    | Some tid when String.trim tid <> "" ->
        ("thread_id", `String tid) :: fields
    | _ -> fields
  in
  let fields =
    match activity_id with
    | Some aid when String.trim aid <> "" ->
        ("activity_id", `String aid) :: fields
    | _ -> fields
  in
  append_now ~db ~room_id ~event_type:"delivery_success" ~actor:connector
    ~metadata:(`Assoc fields)

(** [record_delivery_failure ~db ~room_id ~connector ~task_id ~error ?thread_id
     ?activity_id ()] records a failed delivery with a sanitized error. *)
let record_delivery_failure ~db ~room_id ~connector ~task_id ~error ?thread_id
    ?activity_id () =
  let sanitized = sanitize_error error in
  let fields =
    [
      ("connector", `String connector);
      ("room_id", `String room_id);
      ("task_id", `Int task_id);
      ("error", `String sanitized);
    ]
  in
  let fields =
    match thread_id with
    | Some tid when String.trim tid <> "" ->
        ("thread_id", `String tid) :: fields
    | _ -> fields
  in
  let fields =
    match activity_id with
    | Some aid when String.trim aid <> "" ->
        ("activity_id", `String aid) :: fields
    | _ -> fields
  in
  append_now ~db ~room_id ~event_type:"delivery_failure" ~actor:connector
    ~metadata:(`Assoc fields)

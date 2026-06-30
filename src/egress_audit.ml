(** Egress audit event recording.

    Records every egress policy decision (allowed or denied) into a dedicated
    SQLite table for compliance and debugging. All sensitive fields (host,
    method, credential IDs) are redacted or aliased before storage. *)

let src = Logs.Src.create "clawq.egress_audit" ~doc:"Egress audit events"

module Log = (val Logs.src_log src : Logs.LOG)

type decision = Allowed | Denied

let decision_to_string = function Allowed -> "allowed" | Denied -> "denied"

type event = {
  id : int;
  timestamp : string;
  decision : decision;
  host_redacted : string;
  method_redacted : string option;
  path_redacted : string option;
  matched_rule_index : int;
  session_key : string option;
  snapshot_id : string option;
  tool_name : string option;
  profile_id : string option;
  credential_handle_ids : string list;
      (** Alias IDs only -- never actual credential values. *)
}

(** Redact a hostname for audit storage. Keeps the TLD and the first char of
    the first label visible; all intermediate labels are fully replaced with 6
    asterisks (no leading character preserved). Examples:
    - "api.github.com" -> "a**.******.com"
    - "sub.api.example.com" -> "s**.******.******.com"
    - "example.com" -> "e******.com"
    - "localhost" -> "l********" *)
let redact_host host =
  let parts = String.split_on_char '.' host in
  match List.length parts with
  | 0 -> "***"
  | 1 ->
      let s = List.hd parts in
      if String.length s <= 1 then "*"
      else String.sub s 0 1 ^ String.make (String.length s - 1) '*'
  | n ->
      let first = List.nth parts 0 in
      let last = List.nth parts (n - 1) in
      let first_redacted =
        if String.length first <= 1 then "*"
        else String.sub first 0 1 ^ String.make (String.length first - 1) '*'
      in
      let middle_redacted = List.init (n - 2) (fun _ -> String.make 6 '*') in
      String.concat "." ((first_redacted :: middle_redacted) @ [ last ])

(** Redact an HTTP method. Shows first and last character when length >= 3,
    otherwise fully redacted. *)
let redact_method m =
  let len = String.length m in
  if len <= 1 then "*"
  else if len = 2 then String.sub m 0 1 ^ "*"
  else String.sub m 0 1 ^ String.make (len - 2) '*' ^ String.sub m (len - 1) 1

(** Redact a URL path. Keeps the first segment visible, obscures the rest. *)
let redact_path path =
  match String.split_on_char '/' path with
  | [] -> "***"
  | segments ->
      let visible =
        match segments with
        | "" :: first :: _ -> "/" ^ first
        | first :: _ -> "/" ^ first
        | [] -> "***"
      in
      if List.length segments <= 2 then path else visible ^ "/**"

let exec_exn db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "egress_audit schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

let init_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS egress_audit (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     timestamp TEXT NOT NULL,\n\
    \     decision TEXT NOT NULL,\n\
    \     host_redacted TEXT NOT NULL,\n\
    \     method_redacted TEXT,\n\
    \     path_redacted TEXT,\n\
    \     matched_rule_index INTEGER NOT NULL,\n\
    \     session_key TEXT,\n\
    \     snapshot_id TEXT,\n\
    \     tool_name TEXT,\n\
    \     profile_id TEXT,\n\
    \     credential_handle_ids_json TEXT NOT NULL DEFAULT '[]',\n\
    \     UNIQUE(timestamp, host_redacted, decision, matched_rule_index)\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_egress_audit_time ON \
     egress_audit(timestamp)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_egress_audit_decision_time ON \
     egress_audit(decision, timestamp)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_egress_audit_session ON \
     egress_audit(session_key, timestamp)"

let timestamp_now () =
  let now = Unix.gettimeofday () in
  let tm = Unix.gmtime now in
  let micros = int_of_float ((now -. floor now) *. 1_000_000.0) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%06dZ" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec micros

let text_column stmt idx =
  match Sqlite3.column stmt idx with Sqlite3.Data.TEXT s -> s | _ -> ""

let opt_text_column stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.TEXT s -> Some s
  | Sqlite3.Data.NULL -> None
  | _ -> None

let int_column stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | _ -> 0

let event_of_stmt stmt =
  let handle_ids_json = text_column stmt 11 in
  let credential_handle_ids =
    try
      match Yojson.Safe.from_string handle_ids_json with
      | `List items ->
          List.filter_map (function `String s -> Some s | _ -> None) items
      | _ -> []
    with _ -> []
  in
  {
    id = int_column stmt 0;
    timestamp = text_column stmt 1;
    decision =
      (match text_column stmt 2 with
      | "allowed" -> Allowed
      | "denied" -> Denied
      | _ -> Denied);
    host_redacted = text_column stmt 3;
    method_redacted = opt_text_column stmt 4;
    path_redacted = opt_text_column stmt 5;
    matched_rule_index = int_column stmt 6;
    session_key = opt_text_column stmt 7;
    snapshot_id = opt_text_column stmt 8;
    tool_name = opt_text_column stmt 9;
    profile_id = opt_text_column stmt 10;
    credential_handle_ids;
  }

let event_to_json (event : event) : Yojson.Safe.t =
  `Assoc
    [
      ("id", `Int event.id);
      ("timestamp", `String event.timestamp);
      ("decision", `String (decision_to_string event.decision));
      ("host_redacted", `String event.host_redacted);
      ( "method_redacted",
        Option.fold ~none:`Null ~some:(fun s -> `String s) event.method_redacted
      );
      ( "path_redacted",
        Option.fold ~none:`Null ~some:(fun s -> `String s) event.path_redacted
      );
      ("matched_rule_index", `Int event.matched_rule_index);
      ( "session_key",
        Option.fold ~none:`Null ~some:(fun s -> `String s) event.session_key );
      ( "snapshot_id",
        Option.fold ~none:`Null ~some:(fun s -> `String s) event.snapshot_id );
      ( "tool_name",
        Option.fold ~none:`Null ~some:(fun s -> `String s) event.tool_name );
      ( "profile_id",
        Option.fold ~none:`Null ~some:(fun s -> `String s) event.profile_id );
      ( "credential_handle_ids",
        `List (List.map (fun s -> `String s) event.credential_handle_ids) );
    ]

(** [record ~db ~decision ~host ~method_ ?path ~matched_rule_index ?session_key
     ?snapshot_id ?tool_name ?profile_id ~credential_handle_ids ()] records an
    egress decision.

    All sensitive fields are redacted before storage. Credential IDs are stored
    as-is (they are opaque aliases, never actual values). *)
let record ~db ~decision ~host ?method_ ?path ~matched_rule_index ?session_key
    ?snapshot_id ?tool_name ?profile_id ?(credential_handle_ids = []) () =
  let ts = timestamp_now () in
  let host_redacted = redact_host host in
  let method_redacted = Option.map redact_method method_ in
  let path_redacted = Option.map redact_path path in
  let handles_json =
    Yojson.Safe.to_string
      (`List (List.map (fun s -> `String s) credential_handle_ids))
  in
  let sql =
    "INSERT OR IGNORE INTO egress_audit (timestamp, decision, host_redacted, \
     method_redacted, path_redacted, matched_rule_index, session_key, \
     snapshot_id, tool_name, profile_id, credential_handle_ids_json) VALUES \
     (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind_text pos value =
        ignore (Sqlite3.bind stmt pos (Sqlite3.Data.TEXT value))
      in
      let bind_opt pos = function
        | None -> ignore (Sqlite3.bind stmt pos Sqlite3.Data.NULL)
        | Some v -> bind_text pos v
      in
      bind_text 1 ts;
      bind_text 2 (decision_to_string decision);
      bind_text 3 host_redacted;
      bind_opt 4 method_redacted;
      bind_opt 5 path_redacted;
      ignore
        (Sqlite3.bind stmt 6
           (Sqlite3.Data.INT (Int64.of_int matched_rule_index)));
      bind_opt 7 session_key;
      bind_opt 8 snapshot_id;
      bind_opt 9 tool_name;
      bind_opt 10 profile_id;
      bind_text 11 handles_json;
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE ->
          Log.info (fun m ->
              m "egress audit: %s %s rule=%d tool=%s"
                (decision_to_string decision)
                host_redacted matched_rule_index
                (Option.value tool_name ~default:"-"))
      | rc ->
          Log.warn (fun m ->
              m "egress audit insert failed: %s" (Sqlite3.Rc.to_string rc)))

(** [query ~db ?decision ?session_key ?tool_name ?from_timestamp ?to_timestamp
     ?(limit=100) ()] queries audit events with optional filters. *)
let query ~db ?decision ?session_key ?tool_name ?from_timestamp ?to_timestamp
    ?(limit = 100) () =
  let filters = ref [] in
  let params = ref [] in
  let add_filter sql value =
    filters := sql :: !filters;
    params := value :: !params
  in
  Option.iter
    (fun d ->
      add_filter "decision = ?" (Sqlite3.Data.TEXT (decision_to_string d)))
    decision;
  Option.iter
    (fun sk -> add_filter "session_key = ?" (Sqlite3.Data.TEXT sk))
    session_key;
  Option.iter
    (fun tn -> add_filter "tool_name = ?" (Sqlite3.Data.TEXT tn))
    tool_name;
  Option.iter
    (fun ts -> add_filter "timestamp >= ?" (Sqlite3.Data.TEXT ts))
    from_timestamp;
  Option.iter
    (fun ts -> add_filter "timestamp <= ?" (Sqlite3.Data.TEXT ts))
    to_timestamp;
  let where_clause =
    match List.rev !filters with
    | [] -> ""
    | fs -> " WHERE " ^ String.concat " AND " fs
  in
  let limit_clause = Printf.sprintf " ORDER BY timestamp DESC LIMIT %d" limit in
  let sql =
    "SELECT id, timestamp, decision, host_redacted, method_redacted, \
     path_redacted, matched_rule_index, session_key, snapshot_id, tool_name, \
     profile_id, credential_handle_ids_json FROM egress_audit" ^ where_clause
    ^ limit_clause
  in
  let stmt = Sqlite3.prepare db sql in
  let events = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      List.iteri
        (fun i p -> ignore (Sqlite3.bind stmt (i + 1) p : Sqlite3.Rc.t))
        (List.rev !params);
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            events := event_of_stmt stmt :: !events;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf "egress_audit query failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ());
  List.rev !events

let delete_before ~db ~before_timestamp =
  let stmt =
    Sqlite3.prepare db "DELETE FROM egress_audit WHERE timestamp < ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT before_timestamp));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db
      | rc ->
          failwith
            (Printf.sprintf "egress_audit retention cleanup failed: %s"
               (Sqlite3.Rc.to_string rc)))

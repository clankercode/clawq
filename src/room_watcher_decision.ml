(** Watcher decision persistence and material-change detection.

    Records why the watcher acted or did not act on each stale item. Supports
    suppression of repeated no-op decisions while keeping them inspectable by
    admins. *)

(** {1 Types} *)

type decision_outcome = Acted | Skipped

type skip_reason =
  | No_material_change
  | Recently_decided
  | Policy_denied
  | Budget_exceeded
  | Rate_limited
  | Quiet_hours
  | Connector_unsupported

type watcher_type = Stale_task | Stale_thread

type watcher_decision = {
  id : int;
  room_id : string;
  watcher_type : watcher_type;
  outcome : decision_outcome;
  skip_reason : skip_reason option;
  item_source : string;
  item_id : string;
  fingerprint : string;
  timestamp : string;
  metadata : Yojson.Safe.t;
}

(** {1 Serialization helpers} *)

let outcome_to_string = function Acted -> "acted" | Skipped -> "skipped"

let outcome_of_string = function
  | "acted" -> Some Acted
  | "skipped" -> Some Skipped
  | _ -> None

let skip_reason_to_string = function
  | No_material_change -> "no_material_change"
  | Recently_decided -> "recently_decided"
  | Policy_denied -> "policy_denied"
  | Budget_exceeded -> "budget_exceeded"
  | Rate_limited -> "rate_limited"
  | Quiet_hours -> "quiet_hours"
  | Connector_unsupported -> "connector_unsupported"

let skip_reason_of_string = function
  | "no_material_change" -> Some No_material_change
  | "recently_decided" -> Some Recently_decided
  | "policy_denied" -> Some Policy_denied
  | "budget_exceeded" -> Some Budget_exceeded
  | "rate_limited" -> Some Rate_limited
  | "quiet_hours" -> Some Quiet_hours
  | "connector_unsupported" -> Some Connector_unsupported
  | _ -> None

let watcher_type_to_string = function
  | Stale_task -> "stale_task"
  | Stale_thread -> "stale_thread"

let watcher_type_of_string = function
  | "stale_task" -> Some Stale_task
  | "stale_thread" -> Some Stale_thread
  | _ -> None

(** {1 Database schema} *)

let exec_exn db sql =
  Sql_util.exec_exn ~label:"room_watcher_decision schema error" db sql

let init_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS watcher_decisions (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     room_id TEXT NOT NULL,\n\
    \     watcher_type TEXT NOT NULL,\n\
    \     outcome TEXT NOT NULL,\n\
    \     skip_reason TEXT,\n\
    \     item_source TEXT NOT NULL,\n\
    \     item_id TEXT NOT NULL,\n\
    \     fingerprint TEXT NOT NULL DEFAULT '',\n\
    \     timestamp TEXT NOT NULL,\n\
    \     metadata TEXT NOT NULL DEFAULT '{}',\n\
    \     UNIQUE(room_id, watcher_type, item_source, item_id, fingerprint)\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_watcher_decisions_room_time ON \
     watcher_decisions(room_id, timestamp)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_watcher_decisions_item ON \
     watcher_decisions(room_id, item_source, item_id, timestamp DESC)"

(** {1 Low-level DB helpers} *)

let text_column stmt idx =
  match Sqlite3.column stmt idx with Sqlite3.Data.TEXT s -> s | _ -> ""

let int_column stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | _ -> 0

let option_text_column = Sql_util.opt_text_column

let decision_of_stmt stmt =
  {
    id = int_column stmt 0;
    room_id = text_column stmt 1;
    watcher_type =
      (match watcher_type_of_string (text_column stmt 2) with
      | Some wt -> wt
      | None -> Stale_task);
    outcome =
      (match outcome_of_string (text_column stmt 3) with
      | Some o -> o
      | None -> Skipped);
    skip_reason = Option.bind (option_text_column stmt 4) skip_reason_of_string;
    item_source = text_column stmt 5;
    item_id = text_column stmt 6;
    fingerprint = text_column stmt 7;
    timestamp = text_column stmt 8;
    metadata =
      (try Yojson.Safe.from_string (text_column stmt 9) with _ -> `Null);
  }

let timestamp_now () = Time_util.iso8601_utc_micros ()

(** {1 Material-change fingerprints} *)

(** Build a fingerprint string from the stable material fields of a stale item.
    Two items with the same fingerprint have not materially changed. Age is
    intentionally excluded: a still-stale item getting older is not by itself a
    new material event. *)
let compute_fingerprint ~(source : [ `Background_task | `Task_tree ]) ~item_id
    ~status ~age_seconds:_ =
  let source_str =
    match source with
    | `Background_task -> "background_task"
    | `Task_tree -> "task_tree"
  in
  Printf.sprintf "%s:%s:%s" source_str item_id status

(** Return [true] if [new_fingerprint] differs from [old_fingerprint], meaning a
    material change has occurred. *)
let is_material_change ~old_fingerprint ~new_fingerprint =
  old_fingerprint <> new_fingerprint

(** {1 Recording decisions} *)

(** Record a watcher decision. Uses INSERT OR IGNORE on the unique constraint so
    that repeated identical decisions are silently deduplicated. Returns the
    decision record. *)
let record ~db ~room_id ~watcher_type ~outcome ?skip_reason ~item_source
    ~item_id ~fingerprint ~metadata () =
  let ts = timestamp_now () in
  let sql =
    "INSERT OR IGNORE INTO watcher_decisions (room_id, watcher_type, outcome, \
     skip_reason, item_source, item_id, fingerprint, timestamp, metadata) \
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let skip_str = Option.map skip_reason_to_string skip_reason in
      let params =
        [
          Sqlite3.Data.TEXT room_id;
          Sqlite3.Data.TEXT (watcher_type_to_string watcher_type);
          Sqlite3.Data.TEXT (outcome_to_string outcome);
          (match skip_str with
          | Some s -> Sqlite3.Data.TEXT s
          | None -> Sqlite3.Data.NULL);
          Sqlite3.Data.TEXT item_source;
          Sqlite3.Data.TEXT item_id;
          Sqlite3.Data.TEXT fingerprint;
          Sqlite3.Data.TEXT ts;
          Sqlite3.Data.TEXT (Yojson.Safe.to_string metadata);
        ]
      in
      List.iteri
        (fun i v -> ignore (Sqlite3.bind stmt (i + 1) v : Sqlite3.Rc.t))
        params;
      ignore (Sqlite3.step stmt));
  {
    id = 0;
    room_id;
    watcher_type;
    outcome;
    skip_reason;
    item_source;
    item_id;
    fingerprint;
    timestamp = ts;
    metadata;
  }

(** {1 Query functions} *)

(** Retrieve the most recent decision for a specific item in a room. *)
let latest_decision ~db ~room_id ~item_source ~item_id =
  let sql =
    "SELECT id, room_id, watcher_type, outcome, skip_reason, item_source, \
     item_id, fingerprint, timestamp, metadata FROM watcher_decisions WHERE \
     room_id = ? AND item_source = ? AND item_id = ? ORDER BY timestamp DESC, \
     id DESC LIMIT 1"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      List.iteri
        (fun i v -> ignore (Sqlite3.bind stmt (i + 1) v : Sqlite3.Rc.t))
        [
          Sqlite3.Data.TEXT room_id;
          Sqlite3.Data.TEXT item_source;
          Sqlite3.Data.TEXT item_id;
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (decision_of_stmt stmt)
      | _ -> None)

(** {1 Material-change aware recording} *)

(** Record a decision only if the fingerprint has changed since the last
    decision for this item. If the fingerprint is identical, returns the
    previous decision unchanged (suppresses the no-op). *)
let record_if_changed ~db ~room_id ~watcher_type ~outcome ?skip_reason
    ~item_source ~item_id ~fingerprint ~metadata () =
  match latest_decision ~db ~room_id ~item_source ~item_id with
  | Some prev when prev.fingerprint = fingerprint ->
      (* No material change — suppress and return existing decision *)
      prev
  | _ ->
      record ~db ~room_id ~watcher_type ~outcome ?skip_reason ~item_source
        ~item_id ~fingerprint ~metadata ()

(** List all decisions for a room, newest first. *)
let query_by_room ~db ~room_id ?limit () =
  let sql_base =
    "SELECT id, room_id, watcher_type, outcome, skip_reason, item_source, \
     item_id, fingerprint, timestamp, metadata FROM watcher_decisions WHERE \
     room_id = ? ORDER BY timestamp DESC, id DESC"
  in
  let sql =
    match limit with
    | Some n -> sql_base ^ " LIMIT " ^ string_of_int n
    | None -> sql_base
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
      let results = ref [] in
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            results := decision_of_stmt stmt :: !results;
            loop ()
        | _ -> ()
      in
      loop ();
      List.rev !results)

(** List decisions filtered by outcome. *)
let query_by_outcome ~db ~room_id ~outcome ?limit () =
  let sql_base =
    "SELECT id, room_id, watcher_type, outcome, skip_reason, item_source, \
     item_id, fingerprint, timestamp, metadata FROM watcher_decisions WHERE \
     room_id = ? AND outcome = ? ORDER BY timestamp DESC, id DESC"
  in
  let sql =
    match limit with
    | Some n -> sql_base ^ " LIMIT " ^ string_of_int n
    | None -> sql_base
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
      ignore
        (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT (outcome_to_string outcome)));
      let results = ref [] in
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            results := decision_of_stmt stmt :: !results;
            loop ()
        | _ -> ()
      in
      loop ();
      List.rev !results)

(** List decisions filtered by skip_reason. *)
let query_by_skip_reason ~db ~room_id ~skip_reason ?limit () =
  let sql_base =
    "SELECT id, room_id, watcher_type, outcome, skip_reason, item_source, \
     item_id, fingerprint, timestamp, metadata FROM watcher_decisions WHERE \
     room_id = ? AND skip_reason = ? ORDER BY timestamp DESC, id DESC"
  in
  let sql =
    match limit with
    | Some n -> sql_base ^ " LIMIT " ^ string_of_int n
    | None -> sql_base
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
      ignore
        (Sqlite3.bind stmt 2
           (Sqlite3.Data.TEXT (skip_reason_to_string skip_reason)));
      let results = ref [] in
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            results := decision_of_stmt stmt :: !results;
            loop ()
        | _ -> ()
      in
      loop ();
      List.rev !results)

(** Delete decisions older than [before_timestamp]. Returns the number of
    deleted rows. *)
let delete_before ~db ~before_timestamp =
  let stmt =
    Sqlite3.prepare db "DELETE FROM watcher_decisions WHERE timestamp < ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT before_timestamp));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db
      | rc ->
          failwith
            (Printf.sprintf "watcher_decisions delete_before failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** {1 Decision summary for admin inspection} *)

type decision_summary = {
  total_decisions : int;
  acted_count : int;
  skipped_count : int;
  skip_breakdown : (skip_reason * int) list;
}

(** Summarize decisions for a room. *)
let summarize ~db ~room_id =
  let all = query_by_room ~db ~room_id () in
  let total = List.length all in
  let acted = List.length (List.filter (fun d -> d.outcome = Acted) all) in
  let skipped = List.filter (fun d -> d.outcome = Skipped) all in
  let skip_map = ref [] in
  List.iter
    (fun d ->
      match d.skip_reason with
      | Some reason -> (
          match List.assoc_opt reason !skip_map with
          | Some count ->
              skip_map :=
                (reason, count + 1)
                :: List.filter (fun (k, _) -> k <> reason) !skip_map
          | None -> skip_map := (reason, 1) :: !skip_map)
      | None -> ())
    skipped;
  {
    total_decisions = total;
    acted_count = acted;
    skipped_count = List.length skipped;
    skip_breakdown = !skip_map;
  }

let summary_to_json s =
  let skip_json =
    List.map
      (fun (reason, count) -> (skip_reason_to_string reason, `Int count))
      s.skip_breakdown
  in
  `Assoc
    [
      ("total_decisions", `Int s.total_decisions);
      ("acted_count", `Int s.acted_count);
      ("skipped_count", `Int s.skipped_count);
      ("skip_breakdown", `Assoc skip_json);
    ]

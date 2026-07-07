(** Durable room progress checklist model.

    Tracks discrete checklist items for room-origin background tasks through
    planned/current/blocked/done/final states. The model is appendable (new
    items can be added), resumable (state is reconstructed from SQLite without
    re-running any model), and renderable (produces human-readable output from
    persisted state).

    Each checklist item records:
    - Task association ([task_id])
    - Human-readable title
    - Lifecycle state (planned/current/blocked/done/final)
    - Transcript and session links for drill-down
    - Session record reference for linking to room session records
    - Last update timestamp
    - Delivery state tracking room notification status *)

(** {1 Item state} *)

type item_state =
  | Planned
  | Current
  | Blocked
  | Done
  | Final
      (** Lifecycle states for a checklist item.

          - [Planned] means the item is known but work has not started.
          - [Current] means the item is actively being worked on.
          - [Blocked] means the item is waiting on an external dependency.
          - [Done] means the item is complete but the overall task continues.
          - [Final] means the item is the terminal state for a task. *)

let string_of_item_state = function
  | Planned -> "planned"
  | Current -> "current"
  | Blocked -> "blocked"
  | Done -> "done"
  | Final -> "final"

let item_state_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "planned" -> Some Planned
  | "current" -> Some Current
  | "blocked" -> Some Blocked
  | "done" -> Some Done
  | "final" -> Some Final
  | _ -> None

let is_terminal_item_state = function Final -> true | _ -> false

let state_icon = function
  | Planned -> "[ ]"
  | Current -> "[~]"
  | Blocked -> "[!]"
  | Done -> "[x]"
  | Final -> "[*]"

(** {1 Delivery state} *)

type delivery_state =
  | Delivery_pending
  | Delivery_sent
  | Delivery_confirmed
  | Delivery_failed of string
      (** Tracks whether the checklist item state was delivered to the
          originating room.

          - [Delivery_pending] means not yet sent.
          - [Delivery_sent] means sent but not confirmed.
          - [Delivery_confirmed] means the room received the update.
          - [Delivery_failed reason] means delivery failed. *)

let string_of_delivery_state = function
  | Delivery_pending -> "pending"
  | Delivery_sent -> "sent"
  | Delivery_confirmed -> "confirmed"
  | Delivery_failed reason -> "failed:" ^ reason

let delivery_state_of_string s =
  match String.trim s with
  | "pending" -> Some Delivery_pending
  | "sent" -> Some Delivery_sent
  | "confirmed" -> Some Delivery_confirmed
  | _ -> (
      match String.split_on_char ':' s with
      | "failed" :: rest when rest <> [] ->
          Some (Delivery_failed (String.concat ":" rest))
      | _ -> None)

(** {1 Checklist item} *)

type checklist_item = {
  id : int;
  task_id : int;
  title : string;
  state : item_state;
  transcript_url : string option;
  session_url : string option;
  session_record_id : string option;
  last_update : string;
  delivery_state : delivery_state;
}
(** A single checklist entry tracking a step of room-origin work.

    - [id] is the auto-incremented primary key.
    - [task_id] is the associated background task identifier.
    - [title] is the human-readable description of this step.
    - [state] is the current lifecycle state.
    - [transcript_url] is an optional link to the runner transcript.
    - [session_url] is an optional link to the runner session.
    - [session_record_id] is an optional reference to a room session record.
    - [last_update] is the ISO-8601 timestamp of the last state change.
    - [delivery_state] tracks whether the room was notified. *)

(** {1 Schema} *)

let init_schema db =
  let exec sql =
    Sql_util.exec_exn ~label:"room_progress_checklist schema error" db sql
  in
  exec
    "CREATE TABLE IF NOT EXISTS room_progress_checklist (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     task_id INTEGER NOT NULL,\n\
    \     title TEXT NOT NULL,\n\
    \     state TEXT NOT NULL DEFAULT 'planned',\n\
    \     transcript_url TEXT,\n\
    \     session_url TEXT,\n\
    \     session_record_id TEXT,\n\
    \     last_update TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     delivery_state TEXT NOT NULL DEFAULT 'pending'\n\
    \   )";
  exec
    "CREATE INDEX IF NOT EXISTS idx_checklist_task ON \
     room_progress_checklist(task_id)";
  exec
    "CREATE INDEX IF NOT EXISTS idx_checklist_task_state ON \
     room_progress_checklist(task_id, state)";
  exec
    "CREATE INDEX IF NOT EXISTS idx_checklist_last_update ON \
     room_progress_checklist(last_update)";
  (* Migration: add session_record_id column for existing DBs *)
  let try_alter sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | Sqlite3.Rc.ERROR
      when String.starts_with ~prefix:"duplicate column name"
             (Sqlite3.errmsg db) ->
        ()
    | rc ->
        failwith
          (Printf.sprintf
             "room_progress_checklist migration error: %s (sql: %s)"
             (Sqlite3.Rc.to_string rc) sql)
  in
  try_alter
    "ALTER TABLE room_progress_checklist ADD COLUMN session_record_id TEXT"

(** {1 Helpers} *)

let text_column = Sql_util.opt_text_column
let text_column_nn = Sql_util.text_column
let int_column = Sql_util.int_column

let item_of_stmt stmt : checklist_item =
  let state_str = text_column_nn stmt 3 in
  let delivery_str = text_column_nn stmt 8 in
  {
    id = int_column stmt 0;
    task_id = int_column stmt 1;
    title = text_column_nn stmt 2;
    state =
      (match item_state_of_string state_str with
      | Some s -> s
      | None -> Planned);
    transcript_url = text_column stmt 4;
    session_url = text_column stmt 5;
    session_record_id = text_column stmt 6;
    last_update = text_column_nn stmt 7;
    delivery_state =
      (match delivery_state_of_string delivery_str with
      | Some s -> s
      | None -> Delivery_pending);
  }

let bind_params = Sql_util.bind_params
let timestamp_now () = Time_util.iso8601_utc_micros ()

let select_columns =
  "id, task_id, title, state, transcript_url, session_url, session_record_id, \
   last_update, delivery_state"

(** {1 Append} *)

(** [append ~db ~task_id ~title ?transcript_url ?session_url ?session_record_id
     ()] adds a new checklist item in [Planned] state. Returns the created item.
*)
let append ~db ~task_id ~title ?transcript_url ?session_url ?session_record_id
    () =
  let ts = timestamp_now () in
  let sql =
    Printf.sprintf
      "INSERT INTO room_progress_checklist (task_id, title, state, \
       transcript_url, session_url, session_record_id, last_update, \
       delivery_state) VALUES (?, ?, 'planned', ?, ?, ?, ?, 'pending') \
       RETURNING %s"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          Sqlite3.Data.INT (Int64.of_int task_id);
          Sqlite3.Data.TEXT title;
          (match transcript_url with
          | Some url -> Sqlite3.Data.TEXT url
          | None -> Sqlite3.Data.NULL);
          (match session_url with
          | Some url -> Sqlite3.Data.TEXT url
          | None -> Sqlite3.Data.NULL);
          (match session_record_id with
          | Some id_val -> Sqlite3.Data.TEXT id_val
          | None -> Sqlite3.Data.NULL);
          Sqlite3.Data.TEXT ts;
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> item_of_stmt stmt
      | rc ->
          failwith
            (Printf.sprintf "room_progress_checklist append failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** {1 State transition} *)

(** [update_state ~db ~id ~state ()] transitions a checklist item to a new state
    and updates the timestamp. Returns the updated item, or [None] if the item
    does not exist. *)
let update_state ~db ~id ~state () =
  let ts = timestamp_now () in
  let sql =
    Printf.sprintf
      "UPDATE room_progress_checklist SET state = ?, last_update = ?, \
       delivery_state = 'pending' WHERE id = ? RETURNING %s"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          Sqlite3.Data.TEXT (string_of_item_state state);
          Sqlite3.Data.TEXT ts;
          Sqlite3.Data.INT (Int64.of_int id);
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (item_of_stmt stmt)
      | Sqlite3.Rc.DONE -> None
      | rc ->
          failwith
            (Printf.sprintf "room_progress_checklist update_state failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** {1 Link updates} *)

(** [set_links ~db ~id ?transcript_url ?session_url ?session_record_id ()]
    updates the transcript URL, session URL, and/or session record ID for a
    checklist item. Returns the updated item, or [None] if the item does not
    exist. *)
let set_links ~db ~id ?transcript_url ?session_url ?session_record_id () =
  let ts = timestamp_now () in
  let sets = ref [ "last_update = ?"; "delivery_state = 'pending'" ] in
  let params = ref [ Sqlite3.Data.TEXT ts ] in
  (match transcript_url with
  | Some url ->
      sets := "transcript_url = ?" :: !sets;
      params := Sqlite3.Data.TEXT url :: !params
  | None -> ());
  (match session_url with
  | Some url ->
      sets := "session_url = ?" :: !sets;
      params := Sqlite3.Data.TEXT url :: !params
  | None -> ());
  (match session_record_id with
  | Some id_val ->
      sets := "session_record_id = ?" :: !sets;
      params := Sqlite3.Data.TEXT id_val :: !params
  | None -> ());
  let sql =
    Printf.sprintf
      "UPDATE room_progress_checklist SET %s WHERE id = ? RETURNING %s"
      (String.concat ", " !sets) select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let all_params = !params @ [ Sqlite3.Data.INT (Int64.of_int id) ] in
      bind_params stmt all_params;
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (item_of_stmt stmt)
      | Sqlite3.Rc.DONE -> None
      | rc ->
          failwith
            (Printf.sprintf "room_progress_checklist set_links failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** {1 Delivery state updates} *)

(** [set_delivery_state ~db ~id ~delivery_state ()] updates the delivery state
    for a checklist item. Returns the updated item, or [None] if not found. *)
let set_delivery_state ~db ~id ~delivery_state () =
  let ts = timestamp_now () in
  let sql =
    Printf.sprintf
      "UPDATE room_progress_checklist SET delivery_state = ?, last_update = ? \
       WHERE id = ? RETURNING %s"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          Sqlite3.Data.TEXT (string_of_delivery_state delivery_state);
          Sqlite3.Data.TEXT ts;
          Sqlite3.Data.INT (Int64.of_int id);
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (item_of_stmt stmt)
      | Sqlite3.Rc.DONE -> None
      | rc ->
          failwith
            (Printf.sprintf
               "room_progress_checklist set_delivery_state failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** {1 Query} *)

(** [get ~db ~id ()] returns a single checklist item by ID. *)
let get ~db ~id () =
  let sql =
    Printf.sprintf "SELECT %s FROM room_progress_checklist WHERE id = ?"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.INT (Int64.of_int id) ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (item_of_stmt stmt)
      | _ -> None)

(** [query_by_task ~db ~task_id ()] returns all checklist items for a task,
    ordered by ID (insertion order). *)
let query_by_task ~db ~task_id () =
  let sql =
    Printf.sprintf
      "SELECT %s FROM room_progress_checklist WHERE task_id = ? ORDER BY id ASC"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  let items = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.INT (Int64.of_int task_id) ];
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            items := item_of_stmt stmt :: !items;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf "room_progress_checklist query failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ());
  List.rev !items

(** [query_pending_delivery ~db ~task_id ()] returns items whose delivery state
    is [Delivery_pending] or [Delivery_failed]. Useful for retry loops. *)
let query_pending_delivery ~db ~task_id () =
  let sql =
    Printf.sprintf
      "SELECT %s FROM room_progress_checklist WHERE task_id = ? AND \
       (delivery_state IN ('pending', 'sent') OR delivery_state LIKE \
       'failed%%') ORDER BY id ASC"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  let items = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.INT (Int64.of_int task_id) ];
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            items := item_of_stmt stmt :: !items;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf
                 "room_progress_checklist query_pending_delivery failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ());
  List.rev !items

(** {1 Rendering} *)

(** [render_item item] formats a single checklist item as a human-readable line.
    Produces output like:
    {v [x] Implement auth module (transcript: https://...) v} *)
let render_item (item : checklist_item) =
  let buf = Buffer.create 128 in
  Buffer.add_string buf (state_icon item.state);
  Buffer.add_char buf ' ';
  Buffer.add_string buf item.title;
  (match item.transcript_url with
  | Some url when String.trim url <> "" ->
      Buffer.add_string buf " (transcript: ";
      Buffer.add_string buf url;
      Buffer.add_char buf ')'
  | _ -> ());
  (match item.session_url with
  | Some url when String.trim url <> "" ->
      Buffer.add_string buf " (session: ";
      Buffer.add_string buf url;
      Buffer.add_char buf ')'
  | _ -> ());
  (match item.session_record_id with
  | Some id_val when String.trim id_val <> "" ->
      Buffer.add_string buf " (record: ";
      Buffer.add_string buf id_val;
      Buffer.add_char buf ')'
  | _ -> ());
  Buffer.contents buf

(** [render items] formats a list of checklist items as a human-readable
    checklist. Each item is on its own line. Empty list produces a placeholder
    message. *)
let render (items : checklist_item list) =
  match items with
  | [] -> "(no checklist items)"
  | _ -> items |> List.map render_item |> String.concat "\n"

(** [render_summary items] produces a compact summary line showing counts by
    state. E.g. "2 done, 1 current, 1 blocked, 1 planned". *)
let render_summary (items : checklist_item list) =
  let counts =
    List.fold_left
      (fun acc (item : checklist_item) ->
        let key = string_of_item_state item.state in
        let current = try List.assoc key acc with Not_found -> 0 in
        (key, current + 1) :: List.remove_assoc key acc)
      [] items
  in
  if counts = [] then "no items"
  else
    let order = [ "final"; "done"; "current"; "blocked"; "planned" ] in
    let ordered =
      List.filter_map
        (fun key ->
          match List.assoc_opt key counts with
          | Some n when n > 0 -> Some (Printf.sprintf "%d %s" n key)
          | _ -> None)
        order
    in
    match ordered with [] -> "no items" | _ -> String.concat ", " ordered

(** {1 Deletion} *)

(** [delete_by_task ~db ~task_id ()] removes all checklist items for a task.
    Returns the number of deleted items. *)
let delete_by_task ~db ~task_id () =
  let stmt =
    Sqlite3.prepare db "DELETE FROM room_progress_checklist WHERE task_id = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.INT (Int64.of_int task_id) ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db
      | rc ->
          failwith
            (Printf.sprintf "room_progress_checklist delete_by_task failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** [delete_before ~db ~before_timestamp ()] removes items older than the given
    timestamp. Returns the number of deleted items. *)
let delete_before ~db ~before_timestamp () =
  let stmt =
    Sqlite3.prepare db
      "DELETE FROM room_progress_checklist WHERE last_update < ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.TEXT before_timestamp ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db
      | rc ->
          failwith
            (Printf.sprintf "room_progress_checklist delete_before failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** {1 JSON serialization} *)

let json_of_item (item : checklist_item) : Yojson.Safe.t =
  let fields =
    [
      ("id", `Int item.id);
      ("task_id", `Int item.task_id);
      ("title", `String item.title);
      ("state", `String (string_of_item_state item.state));
      ("last_update", `String item.last_update);
      ("delivery_state", `String (string_of_delivery_state item.delivery_state));
    ]
  in
  let fields =
    match item.transcript_url with
    | Some url -> ("transcript_url", `String url) :: fields
    | None -> fields
  in
  let fields =
    match item.session_url with
    | Some url -> ("session_url", `String url) :: fields
    | None -> fields
  in
  let fields =
    match item.session_record_id with
    | Some id_val -> ("session_record_id", `String id_val) :: fields
    | None -> fields
  in
  `Assoc fields

let json_of_items items = `List (List.map json_of_item items)
let json_string_of_items items = Yojson.Safe.to_string (json_of_items items)

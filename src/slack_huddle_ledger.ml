(** [slack_huddles]: the huddle observation ledger (spec §6.1).

    Every stored value is a Slack-controlled hint; provenance is proven by the
    phase-3 executor, which alone sets [transcript_file_id] and [finalised].
    Rows expire [ttl_seconds] after they were last seen. *)

module C = Slack_huddle_classify

let label = "slack_huddles"
let ttl_seconds = 30. *. 86400.

type row_kind = Room | Notes_stub

type row = {
  kind : row_kind;
  team_id : string;
  channel_id : string;
  room_id : string option;
  root_ts : string option;
  thread_root_ts : string option;
  canvas_thread_ts : string option;
  attached_file_ids : string list;
  root_file_ids : string list;
  room_transcript_file_id : string option;
      (** The root's [room.transcript_file_id]: Slack's room-to-notes-canvas
          link (P0: equals the canvas id), a hint only. [transcript_file_id] is
          the verified transcript id. *)
  date_start : int option;
  date_end : int option;
  has_ended : bool;
  notes_thread_ts : string option;
  notes_ready_ts : string option;
  notes_file_ids : string list;
  canvas_file_id : string option;
  transcript_file_id : string option;
  notes_variant_hint : string option;
  finalised : bool;
  first_seen_at : float;
  last_seen_at : float;
  source : string;
}

type notes_attachment = Attached of string  (** room_id *) | Stub

let schema =
  [
    "CREATE TABLE IF NOT EXISTS slack_huddles (\n\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \  row_kind TEXT NOT NULL CHECK (row_kind IN ('room','notes_stub')),\n\
    \  team_id TEXT NOT NULL, channel_id TEXT NOT NULL, room_id TEXT,\n\
    \  root_ts TEXT, thread_root_ts TEXT, canvas_thread_ts TEXT,\n\
    \  attached_file_ids_json TEXT NOT NULL DEFAULT '[]', root_file_ids_json \
     TEXT NOT NULL DEFAULT '[]',\n\
    \  room_transcript_file_id TEXT,\n\
    \  date_start INTEGER, date_end INTEGER, has_ended INTEGER NOT NULL \
     DEFAULT 0,\n\
    \  notes_thread_ts TEXT, notes_ready_ts TEXT, notes_file_ids_json TEXT NOT \
     NULL DEFAULT '[]',\n\
    \  canvas_file_id TEXT, transcript_file_id TEXT, notes_variant_hint TEXT,\n\
    \  finalised INTEGER NOT NULL DEFAULT 0,\n\
    \  first_seen_at REAL NOT NULL, last_seen_at REAL NOT NULL,\n\
    \  source TEXT NOT NULL CHECK (source IN ('event','reconcile')))";
    "CREATE UNIQUE INDEX IF NOT EXISTS slack_huddles_room ON \
     slack_huddles(team_id, channel_id, room_id) WHERE row_kind = 'room'";
    "CREATE UNIQUE INDEX IF NOT EXISTS slack_huddles_stub ON \
     slack_huddles(team_id, channel_id, notes_thread_ts, canvas_file_id) WHERE \
     row_kind = 'notes_stub'";
    "CREATE INDEX IF NOT EXISTS slack_huddles_seen ON \
     slack_huddles(last_seen_at)";
  ]

let ensure_schema db = List.iter (Sql_util.exec_schema ~label db) schema

(* ── Row codec ──────────────────────────────────────────────────────────── *)

let column_names =
  [
    "row_kind";
    "team_id";
    "channel_id";
    "room_id";
    "root_ts";
    "thread_root_ts";
    "canvas_thread_ts";
    "attached_file_ids_json";
    "root_file_ids_json";
    "room_transcript_file_id";
    "date_start";
    "date_end";
    "has_ended";
    "notes_thread_ts";
    "notes_ready_ts";
    "notes_file_ids_json";
    "canvas_file_id";
    "transcript_file_id";
    "notes_variant_hint";
    "finalised";
    "first_seen_at";
    "last_seen_at";
    "source";
  ]

let select_rows =
  "SELECT id, " ^ String.concat ", " column_names ^ " FROM slack_huddles"

let kind_to_string = function Room -> "room" | Notes_stub -> "notes_stub"
let kind_of_string = function "notes_stub" -> Notes_stub | _ -> Room

let ids_to_json ids =
  Yojson.Safe.to_string (`List (List.map (fun id -> `String id) ids))

let ids_of_json text =
  match Yojson.Safe.from_string text with
  | `List items ->
      List.filter_map (function `String id -> Some id | _ -> None) items
  | _ -> []
  | exception Yojson.Json_error _ -> []

let params r =
  let open Sqlite3.Data in
  [
    TEXT (kind_to_string r.kind);
    TEXT r.team_id;
    TEXT r.channel_id;
    opt_text r.room_id;
    opt_text r.root_ts;
    opt_text r.thread_root_ts;
    opt_text r.canvas_thread_ts;
    TEXT (ids_to_json r.attached_file_ids);
    TEXT (ids_to_json r.root_file_ids);
    opt_text r.room_transcript_file_id;
    opt_int r.date_start;
    opt_int r.date_end;
    opt_bool (Some r.has_ended);
    opt_text r.notes_thread_ts;
    opt_text r.notes_ready_ts;
    TEXT (ids_to_json r.notes_file_ids);
    opt_text r.canvas_file_id;
    opt_text r.transcript_file_id;
    opt_text r.notes_variant_hint;
    opt_bool (Some r.finalised);
    FLOAT r.first_seen_at;
    FLOAT r.last_seen_at;
    TEXT r.source;
  ]

(* Column 0 is [id]; the row's columns follow in [column_names] order. *)
let row_of_stmt stmt =
  let text i = Sql_util.text_column stmt i in
  let text_opt i = Sql_util.opt_text_column stmt i in
  let ids i = ids_of_json (text i) in
  ( Sql_util.int_column stmt 0,
    {
      kind = kind_of_string (text 1);
      team_id = text 2;
      channel_id = text 3;
      room_id = text_opt 4;
      root_ts = text_opt 5;
      thread_root_ts = text_opt 6;
      canvas_thread_ts = text_opt 7;
      attached_file_ids = ids 8;
      root_file_ids = ids 9;
      room_transcript_file_id = text_opt 10;
      date_start = Sql_util.opt_int_column stmt 11;
      date_end = Sql_util.opt_int_column stmt 12;
      has_ended = Sql_util.sql_bool stmt 13;
      notes_thread_ts = text_opt 14;
      notes_ready_ts = text_opt 15;
      notes_file_ids = ids 16;
      canvas_file_id = text_opt 17;
      transcript_file_id = text_opt 18;
      notes_variant_hint = text_opt 19;
      finalised = Sql_util.sql_bool stmt 20;
      first_seen_at = Sqlite3.column_double stmt 21;
      last_seen_at = Sqlite3.column_double stmt 22;
      source = text 23;
    } )

let find_one db where values =
  match
    Slack_standing_order_sql.query db
      (select_rows ^ " WHERE " ^ where)
      values row_of_stmt
  with
  | found :: _ -> Some found
  | [] -> None

let insert db r =
  Sql_util.exec_with_params ~label db
    (Printf.sprintf "INSERT INTO slack_huddles (%s) VALUES (%s)"
       (String.concat ", " column_names)
       (String.concat ", " (List.map (fun _ -> "?") column_names)))
    (params r)

let update db id r =
  Sql_util.exec_with_params ~label db
    (Printf.sprintf "UPDATE slack_huddles SET %s WHERE id = ?"
       (String.concat ", " (List.map (fun c -> c ^ " = ?") column_names)))
    (params r @ [ Sqlite3.Data.INT (Int64.of_int id) ])

(* ── Reads and pruning ──────────────────────────────────────────────────── *)

let table_exists db =
  Slack_standing_order_sql.query db
    "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = \
     'slack_huddles'"
    [] ignore
  <> []

let prune ~db ~now =
  if not (table_exists db) then 0
  else begin
    Sql_util.exec_with_params ~label db
      "DELETE FROM slack_huddles WHERE last_seen_at < ?"
      [ Sqlite3.Data.FLOAT (now -. ttl_seconds) ];
    Sqlite3.changes db
  end

let list_rows ~db ~team ~channel =
  if not (table_exists db) then []
  else
    Slack_standing_order_sql.query db
      (select_rows ^ " WHERE team_id = ? AND channel_id = ? ORDER BY id")
      [ Sqlite3.Data.TEXT team; Sqlite3.Data.TEXT channel ] (fun stmt ->
        snd (row_of_stmt stmt))

(* The DB is the daemon's shared handle. BEGIN runs outside the handler, so a
   write that cannot start its own transaction raises without rolling back a
   transaction someone else holds open; only after our own BEGIN does a
   failure roll back. *)
let write db ~now f =
  ensure_schema db;
  Sql_util.exec_exn ~label db "BEGIN IMMEDIATE";
  match
    ignore (prune ~db ~now : int);
    let result = f () in
    Sql_util.exec_exn ~label db "COMMIT";
    result
  with
  | result -> result
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      (try Sql_util.exec_exn ~label db "ROLLBACK" with _ -> ());
      Printexc.raise_with_backtrace exn bt

(* ── Merge rules (pure) ─────────────────────────────────────────────────── *)

let first_some a b = match a with Some _ -> a | None -> b

let latest a b =
  match (a, b) with Some x, Some y -> Some (max x y) | _ -> first_some a b

(** [append_new stored ids] is [stored] followed by the [ids] it lacks. *)
let append_new stored ids =
  List.fold_left
    (fun acc id -> if List.mem id acc then acc else acc @ [ id ])
    stored ids

let fresh ~kind ~team ~channel ~source ~now =
  {
    kind;
    team_id = team;
    channel_id = channel;
    room_id = None;
    root_ts = None;
    thread_root_ts = None;
    canvas_thread_ts = None;
    attached_file_ids = [];
    root_file_ids = [];
    room_transcript_file_id = None;
    date_start = None;
    date_end = None;
    has_ended = false;
    notes_thread_ts = None;
    notes_ready_ts = None;
    notes_file_ids = [];
    canvas_file_id = None;
    transcript_file_id = None;
    notes_variant_hint = None;
    finalised = false;
    first_seen_at = now;
    last_seen_at = now;
    source;
  }

(* A first sighting and a reused room id both start the row over from this
   root: notes, verified ids and [finalised] belong to the earlier use. *)
let new_use ~team ~channel ~source ~now ~root_ts ~file_ids (room : C.room) =
  {
    (fresh ~kind:Room ~team ~channel ~source ~now) with
    room_id = Some room.C.room_id;
    root_ts = Some root_ts;
    thread_root_ts = room.C.thread_root_ts;
    canvas_thread_ts = room.C.canvas_thread_ts;
    attached_file_ids = append_new [] room.C.attached_file_ids;
    root_file_ids = append_new [] file_ids;
    room_transcript_file_id = room.C.transcript_file_id;
    date_start = room.C.date_start;
    date_end = room.C.date_end;
    has_ended = room.C.has_ended;
  }

let seen_row ~stored ~source ~now ~root_ts ~file_ids (room : C.room) =
  match (stored.date_start, room.C.date_start) with
  | Some s, Some n when n > s ->
      new_use ~team:stored.team_id ~channel:stored.channel_id ~source ~now
        ~root_ts ~file_ids room
  | Some s, Some n when n < s -> { stored with last_seen_at = now }
  | _ ->
      {
        stored with
        root_ts = first_some stored.root_ts (Some root_ts);
        date_start = first_some stored.date_start room.C.date_start;
        thread_root_ts = first_some room.C.thread_root_ts stored.thread_root_ts;
        canvas_thread_ts =
          first_some room.C.canvas_thread_ts stored.canvas_thread_ts;
        room_transcript_file_id =
          first_some room.C.transcript_file_id stored.room_transcript_file_id;
        date_end = latest stored.date_end room.C.date_end;
        attached_file_ids =
          append_new stored.attached_file_ids room.C.attached_file_ids;
        root_file_ids = append_new stored.root_file_ids file_ids;
        has_ended = stored.has_ended || room.C.has_ended;
        last_seen_at = now;
        source;
      }

let notes_row ~now ~thread_ts ~ts ~ts_value ~file_ids ~canvas_file_id ~variant
    stored =
  let newer =
    match Option.bind stored.notes_ready_ts Float.of_string_opt with
    | Some prev -> ts_value > prev
    | None -> true
  in
  let stored = { stored with last_seen_at = now } in
  if not newer then stored
  else
    {
      stored with
      notes_thread_ts = Some thread_ts;
      notes_ready_ts = Some ts;
      notes_file_ids = append_new [] file_ids;
      canvas_file_id = Some canvas_file_id;
      notes_variant_hint = Some (C.notes_variant_to_string variant);
    }

(* ── Writes ─────────────────────────────────────────────────────────────── *)

let record_seen ~db ~team ~channel ~source ~now ~root_ts ~file_ids
    (room : C.room) =
  write db ~now (fun () ->
      match
        find_one db
          "row_kind = 'room' AND team_id = ? AND channel_id = ? AND room_id = ?"
          Sqlite3.Data.[ TEXT team; TEXT channel; TEXT room.C.room_id ]
      with
      | None ->
          insert db
            (new_use ~team ~channel ~source ~now ~root_ts ~file_ids room)
      | Some (id, stored) ->
          update db id (seen_row ~stored ~source ~now ~root_ts ~file_ids room))

(* A ready reply always follows its huddle's start, so a room whose start is
   later than the reply is a later use of a reused room id. The match does not
   require [has_ended]: the reply can precede the ended edit (P0). *)
let ready_room_where =
  "row_kind = 'room' AND team_id = ? AND channel_id = ? AND room_id IS NOT \
   NULL AND ? IN (root_ts, thread_root_ts, canvas_thread_ts) AND (date_start \
   IS NULL OR date_start <= ?) ORDER BY date_start IS NULL, date_start DESC, \
   id DESC LIMIT 1"

let record_notes_ready ~db ~team ~channel ~source ~now ~thread_ts ~ts ~file_ids
    ~canvas_file_id ~variant =
  let ts_value =
    match Float.of_string_opt ts with
    | Some v -> v
    | None ->
        invalid_arg
          (Printf.sprintf
             "Slack_huddle_ledger.record_notes_ready: ts %S is not a Slack \
              timestamp"
             ts)
  in
  let notes =
    notes_row ~now ~thread_ts ~ts ~ts_value ~file_ids ~canvas_file_id ~variant
  in
  write db ~now (fun () ->
      match
        find_one db ready_room_where
          Sqlite3.Data.
            [ TEXT team; TEXT channel; TEXT thread_ts; FLOAT ts_value ]
      with
      | Some (id, ({ room_id = Some room_id; _ } as stored)) ->
          update db id (notes stored);
          Attached room_id
      | Some (_, { room_id = None; _ }) | None ->
          (match
             find_one db
               "row_kind = 'notes_stub' AND team_id = ? AND channel_id = ? AND \
                notes_thread_ts = ? AND canvas_file_id = ?"
               Sqlite3.Data.
                 [
                   TEXT team; TEXT channel; TEXT thread_ts; TEXT canvas_file_id;
                 ]
           with
          | None ->
              insert db
                (notes (fresh ~kind:Notes_stub ~team ~channel ~source ~now))
          | Some (id, stored) -> update db id (notes stored));
          Stub)

(** A reused room id must never receive a previous use's verification. SQL
    failures propagate, including a failed UPDATE step. *)
let set_verified ~db ~team ~channel ~room_id ~date_start ~transcript_file_id
    ~finalised =
  ensure_schema db;
  Sql_util.exec_with_params ~label db
    "UPDATE slack_huddles SET transcript_file_id=?,finalised=? WHERE \
     row_kind='room' AND team_id=? AND channel_id=? AND room_id=? AND \
     date_start=?"
    Sqlite3.Data.
      [
        opt_text transcript_file_id;
        INT (if finalised then 1L else 0L);
        TEXT team;
        TEXT channel;
        TEXT room_id;
        INT (Int64.of_int date_start);
      ]

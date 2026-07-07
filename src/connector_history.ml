(* connector_history.ml — In-memory ring buffer + optional DB persistence
   for unaddressed group chat messages from connectors (Teams, Discord). *)

type entry = {
  timestamp : float;
  room_id : string;
  connector_type : string;
  sender_name : string;
  sender_id : string;
  text : string;
  channel_type : string;
  metadata_json : string option;
}

let utc_datetime_of_epoch epoch = Time_util.sql_datetime_utc ~t:epoch ()

let text_col stmt idx =
  match Sqlite3.column stmt idx with Sqlite3.Data.TEXT s -> s | _ -> ""

let opt_text_col stmt idx =
  match Sqlite3.column stmt idx with Sqlite3.Data.TEXT s -> Some s | _ -> None

(* Parse "YYYY-MM-DD HH:MM:SS" (UTC, from sqlite datetime('now')) to epoch.
   Falls back to current time on parse failure. *)
let parse_utc_datetime s =
  try
    Scanf.sscanf s "%d-%d-%d %d:%d:%d" (fun y mo d h mi se ->
        let tm =
          {
            Unix.tm_sec = se;
            tm_min = mi;
            tm_hour = h;
            tm_mday = d;
            tm_mon = mo - 1;
            tm_year = y - 1900;
            tm_wday = 0;
            tm_yday = 0;
            tm_isdst = false;
          }
        in
        (* [tm] holds a UTC wall-clock time, but [Unix.mktime] interprets its
           argument as local time, yielding [utc_epoch - offset].  Recover the
           UTC epoch by emulating timegm: re-applying [mktime] to the [gmtime]
           of that result yields [local_interp - offset], so their difference
           is the local-UTC offset in force on that date.  Deriving the offset
           at the parsed instant (rather than from a fixed reference such as
           epoch 0) keeps it correct across DST changes and year boundaries. *)
        let local_interp, _ = Unix.mktime tm in
        let offset =
          local_interp -. fst (Unix.mktime (Unix.gmtime local_interp))
        in
        local_interp +. offset)
  with _ -> Unix.gettimeofday ()

(* Per-session in-memory buffers.  Key = session_key, value = entry list
   (newest first, capped at max). *)
let buffers : (string, entry list) Hashtbl.t = Hashtbl.create 16

let record ?db ~persist ~key ?room_id ?connector_type ~channel_type ~max
    ~sender_name ~sender_id ~text ?metadata_json ?timestamp () =
  let timestamp =
    match timestamp with Some t -> t | None -> Unix.gettimeofday ()
  in
  let room_id = match room_id with Some r -> r | None -> key in
  let connector_type =
    match connector_type with Some c -> c | None -> channel_type
  in
  let entry =
    {
      timestamp;
      room_id;
      connector_type;
      sender_name;
      sender_id;
      text;
      channel_type;
      metadata_json;
    }
  in
  let buf =
    match Hashtbl.find_opt buffers key with Some b -> b | None -> []
  in
  let buf = entry :: buf in
  let buf =
    if List.length buf > max then List.filteri (fun i _ -> i < max) buf else buf
  in
  Hashtbl.replace buffers key buf;
  if persist then
    match db with
    | Some db ->
        let stmt =
          Sqlite3.prepare db
            "INSERT INTO connector_history (session_key, room_id, \
             connector_type, channel_type, sender_name, sender_id, text, \
             metadata_json, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        in
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key));
        ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT room_id));
        ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT connector_type));
        ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT channel_type));
        ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT sender_name));
        ignore (Sqlite3.bind stmt 6 (Sqlite3.Data.TEXT sender_id));
        ignore
          (Sqlite3.bind stmt 7
             (if text = "" then Sqlite3.Data.NULL else Sqlite3.Data.TEXT text));
        ignore
          (Sqlite3.bind stmt 8
             (match metadata_json with
             | Some j -> Sqlite3.Data.TEXT j
             | None -> Sqlite3.Data.NULL));
        ignore
          (Sqlite3.bind stmt 9
             (Sqlite3.Data.TEXT (utc_datetime_of_epoch timestamp)));
        (match Sqlite3.step stmt with
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            Logs.warn (fun m ->
                m "connector_history: INSERT failed: %s"
                  (Sqlite3.Rc.to_string rc)));
        ignore (Sqlite3.finalize stmt);
        (* Trim DB for this room/connector scope to max rows. *)
        let trim_sql =
          Printf.sprintf
            "DELETE FROM connector_history WHERE room_id = ? AND \
             connector_type = ? AND id NOT IN (SELECT id FROM \
             connector_history WHERE room_id = ? AND connector_type = ? ORDER \
             BY id DESC LIMIT %d)"
            max
        in
        let trim_stmt = Sqlite3.prepare db trim_sql in
        ignore (Sqlite3.bind trim_stmt 1 (Sqlite3.Data.TEXT room_id));
        ignore (Sqlite3.bind trim_stmt 2 (Sqlite3.Data.TEXT connector_type));
        ignore (Sqlite3.bind trim_stmt 3 (Sqlite3.Data.TEXT room_id));
        ignore (Sqlite3.bind trim_stmt 4 (Sqlite3.Data.TEXT connector_type));
        (match Sqlite3.step trim_stmt with
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            Logs.warn (fun m ->
                m "connector_history: trim DELETE failed: %s"
                  (Sqlite3.Rc.to_string rc)));
        ignore (Sqlite3.finalize trim_stmt)
    | None -> ()

let load_from_db ~db ~key ~max =
  let sql =
    Printf.sprintf
      "SELECT sender_name, sender_id, text, channel_type, metadata_json, \
       created_at, room_id, connector_type FROM connector_history WHERE \
       session_key = ? ORDER BY id DESC LIMIT %d"
      max
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key));
  let rows = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let sender_name = text_col stmt 0 in
        let sender_id = text_col stmt 1 in
        let text = text_col stmt 2 in
        let channel_type = text_col stmt 3 in
        let metadata_json = opt_text_col stmt 4 in
        let timestamp =
          match Sqlite3.column stmt 5 with
          | Sqlite3.Data.TEXT s -> parse_utc_datetime s
          | _ -> Unix.gettimeofday ()
        in
        let room_id =
          match text_col stmt 6 with "" -> key | room_id -> room_id
        in
        let connector_type =
          match text_col stmt 7 with "" -> channel_type | c -> c
        in
        rows :=
          {
            timestamp;
            room_id;
            connector_type;
            sender_name;
            sender_id;
            text;
            channel_type;
            metadata_json;
          }
          :: !rows
      done);
  (* rows are newest-first from DB; reverse to oldest-first then store
     newest-first in buffer *)
  let oldest_first = !rows in
  let newest_first = List.rev oldest_first in
  Hashtbl.replace buffers key newest_first

let get ?db ~key ~count () =
  let buf =
    match Hashtbl.find_opt buffers key with Some b -> b | None -> []
  in
  let in_mem = List.length buf in
  let entries =
    if in_mem >= count then List.filteri (fun i _ -> i < count) buf
    else
      match db with
      | Some db_handle ->
          load_from_db ~db:db_handle ~key ~max:count;
          let buf =
            match Hashtbl.find_opt buffers key with Some b -> b | None -> []
          in
          List.filteri (fun i _ -> i < count) buf
      | None -> buf
  in
  (* entries are newest-first; reverse to chronological order *)
  List.rev entries

let query ?room_id ?connector_type ?since_ts ?until_ts ?limit ~db () =
  let conditions = ref [] in
  let params = ref [] in
  let add_filter sql data =
    conditions := sql :: !conditions;
    params := data :: !params
  in
  (match room_id with
  | Some room_id -> add_filter "room_id = ?" (Sqlite3.Data.TEXT room_id)
  | None -> ());
  (match connector_type with
  | Some connector_type ->
      add_filter "connector_type = ?" (Sqlite3.Data.TEXT connector_type)
  | None -> ());
  (match since_ts with
  | Some since_ts ->
      add_filter "created_at >= ?"
        (Sqlite3.Data.TEXT (utc_datetime_of_epoch since_ts))
  | None -> ());
  (match until_ts with
  | Some until_ts ->
      add_filter "created_at <= ?"
        (Sqlite3.Data.TEXT (utc_datetime_of_epoch until_ts))
  | None -> ());
  let where_sql =
    match List.rev !conditions with
    | [] -> ""
    | conditions -> " WHERE " ^ String.concat " AND " conditions
  in
  let limit_sql =
    match limit with
    | Some limit when limit > 0 -> Printf.sprintf " LIMIT %d" limit
    | _ -> ""
  in
  let sql =
    "SELECT sender_name, sender_id, text, channel_type, metadata_json, \
     created_at, room_id, connector_type FROM connector_history" ^ where_sql
    ^ " ORDER BY created_at ASC, id ASC" ^ limit_sql
  in
  let stmt = Sqlite3.prepare db sql in
  let entries = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      List.iteri
        (fun i data -> ignore (Sqlite3.bind stmt (i + 1) data))
        (List.rev !params);
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let sender_name = text_col stmt 0 in
        let sender_id = text_col stmt 1 in
        let text = text_col stmt 2 in
        let channel_type = text_col stmt 3 in
        let metadata_json = opt_text_col stmt 4 in
        let timestamp =
          match Sqlite3.column stmt 5 with
          | Sqlite3.Data.TEXT s -> parse_utc_datetime s
          | _ -> Unix.gettimeofday ()
        in
        let room_id = text_col stmt 6 in
        let connector_type =
          match text_col stmt 7 with "" -> channel_type | c -> c
        in
        entries :=
          {
            timestamp;
            room_id;
            connector_type;
            sender_name;
            sender_id;
            text;
            channel_type;
            metadata_json;
          }
          :: !entries
      done);
  List.rev !entries

let delete_older_than ?now ~db ~max_age_days () =
  let now = match now with Some now -> now | None -> Unix.gettimeofday () in
  let cutoff = now -. (float_of_int max_age_days *. 86_400.0) in
  let stmt =
    Sqlite3.prepare db "DELETE FROM connector_history WHERE created_at < ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT (utc_datetime_of_epoch cutoff)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db
      | rc ->
          Logs.warn (fun m ->
              m "connector_history: retention DELETE failed: %s"
                (Sqlite3.Rc.to_string rc));
          0)

let format_for_context entries =
  let format_entry e =
    let tm = Unix.localtime e.timestamp in
    let time_str = Printf.sprintf "%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min in
    let base = Printf.sprintf "[%s] %s: %s" time_str e.sender_name e.text in
    match e.metadata_json with
    | Some json when json <> "" -> Printf.sprintf "%s [metadata: %s]" base json
    | _ -> base
  in
  String.concat "\n" (List.map format_entry entries)

(** Retrieve and format the last [count] entries for [key], returning
    [Some formatted] when entries exist or [None] when the buffer/DB is empty.
    Useful for both manual slash-command injection and automatic room-context
    injection at the start of a turn. *)
let get_formatted_for_key ?db ~key ~count () =
  let entries = get ?db ~key ~count () in
  if entries = [] then None
  else Some (format_for_context entries, List.length entries)

let clear ?db ~key () =
  Hashtbl.remove buffers key;
  match db with
  | Some db ->
      let stmt =
        Sqlite3.prepare db "DELETE FROM connector_history WHERE session_key = ?"
      in
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key));
      ignore (Sqlite3.step stmt);
      ignore (Sqlite3.finalize stmt)
  | None -> ()

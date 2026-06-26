(* connector_history.ml — In-memory ring buffer + optional DB persistence
   for unaddressed group chat messages from connectors (Teams, Discord). *)

type entry = {
  timestamp : float;
  sender_name : string;
  sender_id : string;
  text : string;
  channel_type : string;
  metadata_json : string option;
}

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

let record ?db ~persist ~key ~channel_type ~max ~sender_name ~sender_id ~text
    ?metadata_json () =
  let entry =
    {
      timestamp = Unix.gettimeofday ();
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
            "INSERT INTO connector_history (session_key, channel_type, \
             sender_name, sender_id, text, metadata_json) VALUES (?, ?, ?, ?, \
             ?, ?)"
        in
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key));
        ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT channel_type));
        ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT sender_name));
        ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT sender_id));
        ignore
          (Sqlite3.bind stmt 5
             (if text = "" then Sqlite3.Data.NULL else Sqlite3.Data.TEXT text));
        ignore
          (Sqlite3.bind stmt 6
             (match metadata_json with
             | Some j -> Sqlite3.Data.TEXT j
             | None -> Sqlite3.Data.NULL));
        (match Sqlite3.step stmt with
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            Logs.warn (fun m ->
                m "connector_history: INSERT failed: %s"
                  (Sqlite3.Rc.to_string rc)));
        ignore (Sqlite3.finalize stmt);
        (* Trim DB for this session_key to max rows *)
        let trim_sql =
          Printf.sprintf
            "DELETE FROM connector_history WHERE session_key = ? AND id NOT IN \
             (SELECT id FROM connector_history WHERE session_key = ? ORDER BY \
             id DESC LIMIT %d)"
            max
        in
        let trim_stmt = Sqlite3.prepare db trim_sql in
        ignore (Sqlite3.bind trim_stmt 1 (Sqlite3.Data.TEXT key));
        ignore (Sqlite3.bind trim_stmt 2 (Sqlite3.Data.TEXT key));
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
       created_at FROM connector_history WHERE session_key = ? ORDER BY id \
       DESC LIMIT %d"
      max
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key));
  let rows = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let sender_name =
          match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let sender_id =
          match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let text =
          match Sqlite3.column stmt 2 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let channel_type =
          match Sqlite3.column stmt 3 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let metadata_json =
          match Sqlite3.column stmt 4 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let timestamp =
          match Sqlite3.column stmt 5 with
          | Sqlite3.Data.TEXT s -> parse_utc_datetime s
          | _ -> Unix.gettimeofday ()
        in
        rows :=
          {
            timestamp;
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

(** Per-room queue of GitHub notification texts.

    The daemon holds each room's texts for [window_s], then sends one message of
    at most [max_batch] parts joined by [separator]. Rows survive restart until
    flushed or trimmed. *)

let separator = "\n<hr>\n"
let max_batch = 10
let window_s = 20.0
let join_bodies bodies = String.concat separator bodies

let should_flush ~now ~last_sent ~oldest ~window_s =
  now -. oldest >= window_s && now -. last_sent >= window_s

let exec_schema db sql =
  Sql_util.exec_schema ~label:"github_notify_batch schema error" db sql

let exec_with_params ?label db sql params =
  (* Hot queue path: retry transient lock errors instead of dropping the
     notification or killing the flush loop. *)
  Sql_util.retry_busy (fun () -> Sql_util.exec_with_params ?label db sql params)

let ensure_schema db =
  Sql_util.set_busy_timeout db;
  exec_schema db
    "CREATE TABLE IF NOT EXISTS github_notify_queue (\n\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \  room_id TEXT NOT NULL,\n\
    \  body TEXT NOT NULL,\n\
    \  created_at TEXT NOT NULL\n\
     )";
  exec_schema db
    "CREATE INDEX IF NOT EXISTS idx_github_notify_queue_room ON \
     github_notify_queue (room_id, id)"

let enqueue ~db ~room_id ~body =
  let body = String.trim body in
  if body = "" then ()
  else begin
    ensure_schema db;
    let created = Printf.sprintf "%.6f" (Unix.gettimeofday ()) in
    exec_with_params ~label:"github_notify_batch enqueue" db
      "INSERT INTO github_notify_queue (room_id, body, created_at) VALUES (?, \
       ?, ?)"
      [
        Sqlite3.Data.TEXT room_id;
        Sqlite3.Data.TEXT body;
        Sqlite3.Data.TEXT created;
      ]
  end

let count ~db =
  ensure_schema db;
  let stmt = Sqlite3.prepare db "SELECT COUNT(*) FROM github_notify_queue" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | Sqlite3.Data.TEXT s -> int_of_string s
          | _ -> 0)
      | _ -> 0)

let list_ids ~db =
  ensure_schema db;
  let stmt =
    Sqlite3.prepare db "SELECT id FROM github_notify_queue ORDER BY id DESC"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match Sqlite3.column stmt 0 with
            | Sqlite3.Data.INT n -> loop (Int64.to_int n :: acc)
            | _ -> loop acc)
        | _ -> List.rev acc
      in
      loop [])

let delete_ids ~db ids =
  if ids = [] then ()
  else begin
    ensure_schema db;
    List.iter
      (fun id ->
        exec_with_params ~label:"github_notify_batch delete" db
          "DELETE FROM github_notify_queue WHERE id = ?"
          [ Sqlite3.Data.INT (Int64.of_int id) ])
      ids
  end

type trim_result = { deleted : int; remaining : int }

let trim ~db ~keep =
  if keep < 0 then Error "keep must be >= 0"
  else begin
    ensure_schema db;
    let ids = list_ids ~db in
    let drop =
      if keep = 0 then ids else List.filteri (fun i _ -> i >= keep) ids
    in
    delete_ids ~db drop;
    let remaining = count ~db in
    Ok { deleted = List.length drop; remaining }
  end

let pending_rooms ~db =
  let stmt =
    Sqlite3.prepare db "SELECT DISTINCT room_id FROM github_notify_queue"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match Sqlite3.column stmt 0 with
            | Sqlite3.Data.TEXT room -> loop (room :: acc)
            | _ -> loop acc)
        | _ -> List.rev acc
      in
      loop [])

let oldest_created ~db ~room_id =
  ensure_schema db;
  let stmt =
    Sqlite3.prepare db
      "SELECT created_at FROM github_notify_queue WHERE room_id = ? ORDER BY \
       id ASC LIMIT 1"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.TEXT s -> float_of_string_opt s
          | _ -> None)
      | _ -> None)

let take_oldest ~db ~room_id ~limit =
  ensure_schema db;
  let stmt =
    Sqlite3.prepare db
      "SELECT id, body FROM github_notify_queue WHERE room_id = ? ORDER BY id \
       ASC LIMIT ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int limit)));
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match (Sqlite3.column stmt 0, Sqlite3.column stmt 1) with
            | Sqlite3.Data.INT id, Sqlite3.Data.TEXT body ->
                loop ((Int64.to_int id, body) :: acc)
            | _ -> loop acc)
        | _ -> List.rev acc
      in
      loop [])

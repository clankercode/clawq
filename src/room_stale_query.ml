(** Query engine for stale room tasks and threads.

    Finds blocked or in-progress items (background tasks and task_tree tasks)
    that exceed a configurable stale-after duration. Results are scoped by room
    origin data and are deterministic given the same DB state and [~now]
    parameter. *)

type stale_item = {
  source : [ `Background_task | `Task_tree ];
  id : string;
  title : string;
  status : string;
  room_id : string option;
  thread_id : string option;
  requester : string option;
  created_at : string;
  age_seconds : float;
}
(** A single stale item returned by the query engine. *)

let source_to_string = function
  | `Background_task -> "background_task"
  | `Task_tree -> "task_tree"

let source_of_string = function
  | "background_task" -> Some `Background_task
  | "task_tree" -> Some `Task_tree
  | _ -> None

(** Convert a Unix timestamp to a SQLite datetime string (UTC). *)
let unix_to_sqlite_datetime ts = Time_util.sql_datetime_utc ~t:ts ()

(** Parse a SQLite datetime string into a Unix timestamp (UTC). Re-uses the same
    algorithm as [Background_task.parse_sqlite_datetime]. *)
let parse_sqlite_datetime s =
  try
    Scanf.sscanf s "%d-%d-%d %d:%d:%d" (fun y mo d h mi s ->
        let a = (14 - mo) / 12 in
        let yy = y + 4800 - a in
        let mm = mo + (12 * a) - 3 in
        let day_num =
          d
          + (((153 * mm) + 2) / 5)
          + (365 * yy) + (yy / 4) - (yy / 100) + (yy / 400) - 32045
        in
        let unix_day = day_num - 2440588 in
        float_of_int ((unix_day * 86400) + (h * 3600) + (mi * 60) + s))
  with _ -> 0.0

(** Extract [room_id] from an [origin_json] string, if present and non-blank. *)
let room_id_of_origin_json = function
  | None -> None
  | Some raw -> (
      match Room_origin.of_json_string_opt raw with
      | Some origin -> (
          let rid = origin.Room_origin.room_id in
          match rid with Some s when String.trim s <> "" -> Some s | _ -> None)
      | None -> None)

(** Extract [thread_id] from an [origin_json] string, if present and non-blank.
    Falls back to the direct [thread_id] column value. *)
let thread_id_of ?thread_id origin_json =
  let from_origin =
    match origin_json with
    | None -> None
    | Some raw -> (
        match Room_origin.of_json_string_opt raw with
        | Some origin -> (
            match origin.Room_origin.thread_id with
            | Some s when String.trim s <> "" -> Some s
            | _ -> None)
        | None -> None)
  in
  match from_origin with
  | Some _ -> from_origin
  | None -> (
      match thread_id with
      | Some s when String.trim s <> "" -> Some s
      | _ -> None)

(** {1 Background task queries} *)

let sql_text = Sql_util.sql_text
let sql_int = Sql_util.sql_int

(** Query stale background tasks. A background task is stale when:
    - status is [queued] or [running]
    - the task has been in that state longer than [stale_after_s]
    - optionally scoped by [room_id] and/or [thread_id] via origin data

    [~now] defaults to [Unix.gettimeofday ()]. *)
let find_stale_background_tasks ~db ?now ~stale_after_s ?room_id ?thread_id () =
  let now = Option.value now ~default:(Unix.gettimeofday ()) in
  let sql =
    "SELECT id, runner, prompt, status, created_at, started_at, origin_json, \
     thread_id, requester FROM background_tasks WHERE status IN ('queued', \
     'running')"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let results = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let id = Option.value (sql_int (Sqlite3.column stmt 0)) ~default:0 in
        let prompt =
          Sqlite3.column stmt 2 |> sql_text |> Option.value ~default:""
        in
        let status =
          Sqlite3.column stmt 3 |> sql_text |> Option.value ~default:""
        in
        let created_at =
          Sqlite3.column stmt 4 |> sql_text |> Option.value ~default:""
        in
        let started_at = Sqlite3.column stmt 5 |> sql_text in
        let origin_json = Sqlite3.column stmt 6 |> sql_text in
        let bg_thread_id = Sqlite3.column stmt 7 |> sql_text in
        let requester = Sqlite3.column stmt 8 |> sql_text in
        let item_room_id = room_id_of_origin_json origin_json in
        let item_thread_id = thread_id_of ?thread_id:bg_thread_id origin_json in
        let room_match =
          match room_id with
          | Some wanted -> (
              match item_room_id with Some rid -> rid = wanted | None -> false)
          | None -> true
        in
        let thread_match =
          match thread_id with
          | Some wanted -> (
              match item_thread_id with
              | Some tid -> tid = wanted
              | None -> false)
          | None -> true
        in
        if room_match && thread_match then begin
          let ref_time =
            match status with
            | "running" -> (
                match started_at with
                | Some s -> parse_sqlite_datetime s
                | None -> parse_sqlite_datetime created_at)
            | _ -> parse_sqlite_datetime created_at
          in
          let age = now -. ref_time in
          if age >= stale_after_s then
            results :=
              {
                source = `Background_task;
                id = string_of_int id;
                title = prompt;
                status;
                room_id = item_room_id;
                thread_id = item_thread_id;
                requester;
                created_at;
                age_seconds = age;
              }
              :: !results
        end
      done;
      List.rev !results)

(** {1 Task tree queries} *)

(** Query stale task_tree tasks. A task_tree task is stale when:
    - status is [pending] or [in_progress]
    - deleted_at is NULL
    - the task has been in that state longer than [stale_after_s]
    - optionally scoped by [room_id] and/or [thread_id] via origin data

    For [pending] tasks, age is measured from [created_at]. For [in_progress]
    tasks, age is measured from [updated_at]. *)
let find_stale_task_tree_tasks ~db ?now ~stale_after_s ?room_id ?thread_id () =
  let now = Option.value now ~default:(Unix.gettimeofday ()) in
  let sql =
    "SELECT id, title, status, created_at, updated_at, origin_json, thread_id, \
     requester FROM task_tree WHERE status IN ('pending', 'in_progress') AND \
     deleted_at IS NULL"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let results = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let id =
          Sqlite3.column stmt 0 |> sql_text |> Option.value ~default:""
        in
        let title =
          Sqlite3.column stmt 1 |> sql_text |> Option.value ~default:""
        in
        let status =
          Sqlite3.column stmt 2 |> sql_text |> Option.value ~default:""
        in
        let created_at =
          Sqlite3.column stmt 3 |> sql_text |> Option.value ~default:""
        in
        let updated_at =
          Sqlite3.column stmt 4 |> sql_text |> Option.value ~default:""
        in
        let origin_json = Sqlite3.column stmt 5 |> sql_text in
        let tree_thread_id = Sqlite3.column stmt 6 |> sql_text in
        let requester = Sqlite3.column stmt 7 |> sql_text in
        let item_room_id = room_id_of_origin_json origin_json in
        let item_thread_id =
          thread_id_of ?thread_id:tree_thread_id origin_json
        in
        let room_match =
          match room_id with
          | Some wanted -> (
              match item_room_id with Some rid -> rid = wanted | None -> false)
          | None -> true
        in
        let thread_match =
          match thread_id with
          | Some wanted -> (
              match item_thread_id with
              | Some tid -> tid = wanted
              | None -> false)
          | None -> true
        in
        if room_match && thread_match then begin
          let ref_time =
            match status with
            | "in_progress" -> parse_sqlite_datetime updated_at
            | _ -> parse_sqlite_datetime created_at
          in
          let age = now -. ref_time in
          if age >= stale_after_s then
            results :=
              {
                source = `Task_tree;
                id;
                title;
                status;
                room_id = item_room_id;
                thread_id = item_thread_id;
                requester;
                created_at;
                age_seconds = age;
              }
              :: !results
        end
      done;
      List.rev !results)

(** {1 Combined query} *)

(** Find all stale room items (both background tasks and task_tree tasks).
    Results are deterministic for the same [~now] value and DB state. *)
let find_stale ~db ?now ~stale_after_s ?room_id ?thread_id () =
  let bg =
    find_stale_background_tasks ~db ?now ~stale_after_s ?room_id ?thread_id ()
  in
  let tree =
    find_stale_task_tree_tasks ~db ?now ~stale_after_s ?room_id ?thread_id ()
  in
  bg @ tree

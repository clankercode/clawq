type schedule =
  | Interval of float
  | CronExpr of {
      minute : int list;
      hour : int list;
      dom : int list;
      month : int list;
      dow : int list;
    }

type job = {
  id : int;
  name : string;
  session_key : string;
  message : string;
  schedule_str : string;
  enabled : bool;
  agent_name : string option;
}

type run = {
  run_id : int;
  job_name : string;
  started_at : string;
  finished_at : string option;
  status : string;
  result_preview : string option;
}

let init_schema db =
  let exec sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | rc ->
        failwith
          (Printf.sprintf "SQLite error: %s (sql: %s)" (Sqlite3.Rc.to_string rc)
             sql)
  in
  exec
    "CREATE TABLE IF NOT EXISTS cron_jobs (\n\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \  name TEXT NOT NULL UNIQUE,\n\
    \  session_key TEXT NOT NULL,\n\
    \  message TEXT NOT NULL,\n\
    \  schedule TEXT NOT NULL,\n\
    \  enabled INTEGER NOT NULL DEFAULT 1,\n\
    \  agent_name TEXT,\n\
    \  created_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
     )";
  (try exec "ALTER TABLE cron_jobs ADD COLUMN agent_name TEXT" with _ -> ());
  exec
    "CREATE TABLE IF NOT EXISTS cron_runs (\n\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \  job_name TEXT NOT NULL,\n\
    \  started_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \  finished_at TEXT,\n\
    \  status TEXT NOT NULL DEFAULT 'running',\n\
    \  result_preview TEXT\n\
     )"

let parse_interval s =
  let len = String.length s in
  if len < 2 then Error ("invalid interval: " ^ s)
  else
    let unit_char = s.[len - 1] in
    let num_str = String.sub s 0 (len - 1) in
    match (int_of_string_opt num_str, unit_char) with
    | Some n, 's' -> Ok (Interval (float_of_int n))
    | Some n, 'm' -> Ok (Interval (float_of_int n *. 60.0))
    | Some n, 'h' -> Ok (Interval (float_of_int n *. 3600.0))
    | Some n, 'd' -> Ok (Interval (float_of_int n *. 86400.0))
    | _ -> Error ("invalid interval: " ^ s)

let parse_cron_field ~min_v ~max_v field =
  let in_range n = n >= min_v && n <= max_v in
  if field = "*" then Ok []
  else if String.length field > 2 && String.sub field 0 2 = "*/" then
    match int_of_string_opt (String.sub field 2 (String.length field - 2)) with
    | Some step when step > 0 -> Ok [ -step ]
    | Some _ -> Error ("invalid cron step: " ^ field)
    | None -> Error ("invalid cron step: " ^ field)
  else
    let parts = String.split_on_char ',' field in
    let nums = List.filter_map int_of_string_opt parts in
    if List.length nums = List.length parts && List.for_all in_range nums then
      Ok nums
    else Error ("invalid cron field: " ^ field)

let parse_schedule s =
  let s = String.trim s in
  if String.length s > 6 && String.sub s 0 6 = "every " then
    parse_interval (String.sub s 6 (String.length s - 6))
  else
    let parts = String.split_on_char ' ' s |> List.filter (fun p -> p <> "") in
    match parts with
    | [ min; hr; dom; mon; dow ] -> (
        match
          ( parse_cron_field ~min_v:0 ~max_v:59 min,
            parse_cron_field ~min_v:0 ~max_v:23 hr,
            parse_cron_field ~min_v:1 ~max_v:31 dom,
            parse_cron_field ~min_v:1 ~max_v:12 mon,
            parse_cron_field ~min_v:0 ~max_v:6 dow )
        with
        | Ok minute, Ok hour, Ok dom_l, Ok month, Ok dow_l ->
            Ok (CronExpr { minute; hour; dom = dom_l; month; dow = dow_l })
        | Error e, _, _, _, _
        | _, Error e, _, _, _
        | _, _, Error e, _, _
        | _, _, _, Error e, _
        | _, _, _, _, Error e ->
            Error e)
    | _ -> Error ("invalid schedule: " ^ s)

let field_matches values v =
  match values with
  | [] -> true
  | [ step ] when step < 0 -> v mod abs step = 0
  | nums -> List.mem v nums

let should_run schedule ~last_run ~now =
  match schedule with
  | Interval secs -> (
      match last_run with None -> true | Some lr -> now -. lr >= secs)
  | CronExpr { minute; hour; dom; month; dow } -> (
      match last_run with
      | Some lr when now -. lr < 60.0 -> false
      | _ ->
          let tm = Unix.gmtime now in
          field_matches minute tm.tm_min
          && field_matches hour tm.tm_hour
          && field_matches dom tm.tm_mday
          && field_matches month (tm.tm_mon + 1)
          && field_matches dow tm.tm_wday)

let list_jobs ~db =
  let sql =
    "SELECT id, name, session_key, message, schedule, enabled, agent_name FROM \
     cron_jobs ORDER BY id"
  in
  let stmt = Sqlite3.prepare db sql in
  let jobs = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let id =
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT i -> Int64.to_int i
      | _ -> 0
    in
    let name =
      match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let session_key =
      match Sqlite3.column stmt 2 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let message =
      match Sqlite3.column stmt 3 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let schedule_str =
      match Sqlite3.column stmt 4 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let enabled =
      match Sqlite3.column stmt 5 with
      | Sqlite3.Data.INT i -> i <> 0L
      | _ -> true
    in
    let agent_name =
      match Sqlite3.column stmt 6 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    jobs :=
      { id; name; session_key; message; schedule_str; enabled; agent_name }
      :: !jobs
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !jobs

let add_job ~db ~name ~session_key ~message ~schedule =
  match parse_schedule schedule with
  | Error e -> Error ("Invalid schedule: " ^ e)
  | Ok _ -> (
      let sql =
        "INSERT INTO cron_jobs (name, session_key, message, schedule) VALUES \
         (?, ?, ?, ?)"
      in
      let stmt = Sqlite3.prepare db sql in
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT message));
      ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT schedule));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE ->
          ignore (Sqlite3.finalize stmt);
          Ok ()
      | rc ->
          ignore (Sqlite3.finalize stmt);
          Error
            (Printf.sprintf "Failed to add job: %s" (Sqlite3.Rc.to_string rc)))

let list_runs ~db ?job_name ~limit () =
  let sql, bindings =
    match job_name with
    | Some name ->
        ( "SELECT id, job_name, started_at, finished_at, status, \
           result_preview FROM cron_runs WHERE job_name = ? ORDER BY id DESC \
           LIMIT ?",
          [ Sqlite3.Data.TEXT name; Sqlite3.Data.INT (Int64.of_int limit) ] )
    | None ->
        ( "SELECT id, job_name, started_at, finished_at, status, \
           result_preview FROM cron_runs ORDER BY id DESC LIMIT ?",
          [ Sqlite3.Data.INT (Int64.of_int limit) ] )
  in
  let stmt = Sqlite3.prepare db sql in
  List.iteri (fun i v -> ignore (Sqlite3.bind stmt (i + 1) v)) bindings;
  let runs = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let run_id =
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT i -> Int64.to_int i
      | _ -> 0
    in
    let job_name =
      match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let started_at =
      match Sqlite3.column stmt 2 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let finished_at =
      match Sqlite3.column stmt 3 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let status =
      match Sqlite3.column stmt 4 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let result_preview =
      match Sqlite3.column stmt 5 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    runs :=
      { run_id; job_name; started_at; finished_at; status; result_preview }
      :: !runs
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !runs

let remove_job ~db ~name =
  let sql = "DELETE FROM cron_jobs WHERE name = ?" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  Sqlite3.changes db > 0

let get_history ~db ~name ~limit =
  let sql =
    "SELECT id, job_name, started_at, finished_at, status, result_preview FROM \
     cron_runs WHERE job_name = ? ORDER BY id DESC LIMIT ?"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int limit)));
  let runs = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let run_id =
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT i -> Int64.to_int i
      | _ -> 0
    in
    let job_name =
      match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let started_at =
      match Sqlite3.column stmt 2 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let finished_at =
      match Sqlite3.column stmt 3 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let status =
      match Sqlite3.column stmt 4 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let result_preview =
      match Sqlite3.column stmt 5 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    runs :=
      { run_id; job_name; started_at; finished_at; status; result_preview }
      :: !runs
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !runs

let record_run_start ~db ~job_name =
  let sql = "INSERT INTO cron_runs (job_name, status) VALUES (?, 'running')" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT job_name));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  Int64.to_int (Sqlite3.last_insert_rowid db)

let record_run_finish ~db ~run_id ~status ~result_preview =
  let preview =
    if String.length result_preview > 500 then String.sub result_preview 0 500
    else result_preview
  in
  let sql =
    "UPDATE cron_runs SET finished_at = datetime('now'), status = ?, \
     result_preview = ? WHERE id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT status));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT preview));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int run_id)));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt)

let prune_runs ~db ~job_name ~keep =
  let sql =
    "DELETE FROM cron_runs WHERE job_name = ? AND id NOT IN (SELECT id FROM \
     cron_runs WHERE job_name = ? ORDER BY id DESC LIMIT ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT job_name));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT job_name));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int keep)));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt)

let get_last_run_time ~db ~job_name =
  let sql =
    "SELECT CAST(strftime('%s', started_at) AS INTEGER) FROM cron_runs WHERE \
     job_name = ? ORDER BY id DESC LIMIT 1"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT job_name));
  let result =
    if Sqlite3.step stmt = Sqlite3.Rc.ROW then
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT ts -> Some (Int64.to_float ts)
      | _ -> None
    else None
  in
  ignore (Sqlite3.finalize stmt);
  result

let tick ~db ~session_mgr =
  let open Lwt.Syntax in
  let jobs = list_jobs ~db in
  let now = Unix.gettimeofday () in
  let enabled_jobs = List.filter (fun j -> j.enabled) jobs in
  let* () =
    Lwt_list.iter_s
      (fun (job : job) ->
        match parse_schedule job.schedule_str with
        | Error _ -> Lwt.return_unit
        | Ok sched ->
            let last_run = get_last_run_time ~db ~job_name:job.name in
            if should_run sched ~last_run ~now then begin
              let run_id = record_run_start ~db ~job_name:job.name in
              Lwt.async (fun () ->
                  Lwt.catch
                    (fun () ->
                      let* result =
                        Session.turn session_mgr ~key:job.session_key
                          ~message:job.message ()
                      in
                      record_run_finish ~db ~run_id ~status:"ok"
                        ~result_preview:result;
                      prune_runs ~db ~job_name:job.name ~keep:20;
                      Lwt.return_unit)
                    (fun exn ->
                      record_run_finish ~db ~run_id ~status:"error"
                        ~result_preview:(Printexc.to_string exn);
                      Lwt.return_unit));
              Lwt.return_unit
            end
            else Lwt.return_unit)
      enabled_jobs
  in
  Lwt.return_unit

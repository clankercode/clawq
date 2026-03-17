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
  ephemeral : bool;
  expires_at : string option;
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
  (try
     exec
       "ALTER TABLE cron_jobs ADD COLUMN ephemeral INTEGER NOT NULL DEFAULT 0"
   with _ -> ());
  (try exec "ALTER TABLE cron_jobs ADD COLUMN expires_at TEXT" with _ -> ());
  exec
    "CREATE TABLE IF NOT EXISTS cron_runs (\n\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \  job_name TEXT NOT NULL,\n\
    \  started_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \  finished_at TEXT,\n\
    \  status TEXT NOT NULL DEFAULT 'running',\n\
    \  result_preview TEXT\n\
     )"

let parse_duration_seconds s =
  let len = String.length s in
  if len < 2 then Error ("invalid duration: " ^ s)
  else
    let unit_char = s.[len - 1] in
    let num_str = String.sub s 0 (len - 1) in
    match (int_of_string_opt num_str, unit_char) with
    | Some n, _ when n <= 0 -> Error "duration must be positive"
    | Some n, 's' -> Ok (float_of_int n)
    | Some n, 'm' -> Ok (float_of_int n *. 60.0)
    | Some n, 'h' -> Ok (float_of_int n *. 3600.0)
    | Some n, 'd' -> Ok (float_of_int n *. 86400.0)
    | _ -> Error ("invalid duration: " ^ s)

let parse_interval s =
  match parse_duration_seconds s with
  | Ok f -> Ok (Interval f)
  | Error e -> Error e

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
          let tm = Unix.localtime now in
          field_matches minute tm.tm_min
          && field_matches hour tm.tm_hour
          && field_matches dom tm.tm_mday
          && field_matches month (tm.tm_mon + 1)
          && field_matches dow tm.tm_wday)

let list_jobs ~db =
  let sql =
    "SELECT id, name, session_key, message, schedule, enabled, agent_name, \
     COALESCE(ephemeral, 0), expires_at FROM cron_jobs ORDER BY id"
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
    let ephemeral =
      match Sqlite3.column stmt 7 with
      | Sqlite3.Data.INT i -> i <> 0L
      | _ -> false
    in
    let expires_at =
      match Sqlite3.column stmt 8 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    jobs :=
      {
        id;
        name;
        session_key;
        message;
        schedule_str;
        enabled;
        agent_name;
        ephemeral;
        expires_at;
      }
      :: !jobs
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !jobs

let add_job ~db ~name ~session_key ~message ~schedule ?(ephemeral = false) ?ttl
    () =
  match parse_schedule schedule with
  | Error e -> Error ("Invalid schedule: " ^ e)
  | Ok _ -> (
      match
        match ttl with
        | None -> Ok None
        | Some t -> (
            match parse_duration_seconds t with
            | Ok secs -> Ok (Some (int_of_float secs))
            | Error e -> Error ("Invalid TTL: " ^ e))
      with
      | Error e -> Error e
      | Ok ttl_secs -> (
          let sql =
            match ttl_secs with
            | None ->
                "INSERT INTO cron_jobs (name, session_key, message, schedule, \
                 ephemeral) VALUES (?, ?, ?, ?, ?)"
            | Some _ ->
                "INSERT INTO cron_jobs (name, session_key, message, schedule, \
                 ephemeral, expires_at) VALUES (?, ?, ?, ?, ?, datetime('now', \
                 '+' || ? || ' seconds'))"
          in
          let stmt = Sqlite3.prepare db sql in
          ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
          ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT session_key));
          ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT message));
          ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT schedule));
          ignore
            (Sqlite3.bind stmt 5
               (Sqlite3.Data.INT (if ephemeral then 1L else 0L)));
          (match ttl_secs with
          | Some n ->
              ignore (Sqlite3.bind stmt 6 (Sqlite3.Data.TEXT (string_of_int n)))
          | None -> ());
          match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE ->
              ignore (Sqlite3.finalize stmt);
              Ok ()
          | rc ->
              ignore (Sqlite3.finalize stmt);
              Error
                (Printf.sprintf "Failed to add job: %s"
                   (Sqlite3.Rc.to_string rc))))

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

let update_job ~db ~name ?schedule ?message ?ttl () =
  let updates = ref [] in
  let bindings = ref [] in
  (match schedule with
  | Some s -> (
      match parse_schedule s with
      | Error e -> raise (Invalid_argument ("Invalid schedule: " ^ e))
      | Ok _ ->
          updates := "schedule = ?" :: !updates;
          bindings := Sqlite3.Data.TEXT s :: !bindings)
  | None -> ());
  (match message with
  | Some m ->
      updates := "message = ?" :: !updates;
      bindings := Sqlite3.Data.TEXT m :: !bindings
  | None -> ());
  (match ttl with
  | Some "none" -> updates := "expires_at = NULL" :: !updates
  | Some t -> (
      match parse_duration_seconds t with
      | Error e -> raise (Invalid_argument ("Invalid TTL: " ^ e))
      | Ok secs ->
          let n = int_of_float secs in
          updates :=
            Printf.sprintf "expires_at = datetime('now', '+%d seconds')" n
            :: !updates)
  | None -> ());
  if !updates = [] then Error "Nothing to update"
  else
    let set_clause = String.concat ", " (List.rev !updates) in
    let sql =
      Printf.sprintf "UPDATE cron_jobs SET %s WHERE name = ?" set_clause
    in
    let stmt = Sqlite3.prepare db sql in
    let all_bindings = List.rev !bindings @ [ Sqlite3.Data.TEXT name ] in
    List.iteri (fun i v -> ignore (Sqlite3.bind stmt (i + 1) v)) all_bindings;
    ignore (Sqlite3.step stmt);
    ignore (Sqlite3.finalize stmt);
    if Sqlite3.changes db > 0 then Ok ()
    else Error (Printf.sprintf "No job found with name '%s'" name)

let get_job ~db ~name =
  let sql =
    "SELECT id, name, session_key, message, schedule, enabled, agent_name, \
     COALESCE(ephemeral, 0), expires_at FROM cron_jobs WHERE name = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
  let result =
    if Sqlite3.step stmt = Sqlite3.Rc.ROW then
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
      let ephemeral =
        match Sqlite3.column stmt 7 with
        | Sqlite3.Data.INT i -> i <> 0L
        | _ -> false
      in
      let expires_at =
        match Sqlite3.column stmt 8 with
        | Sqlite3.Data.TEXT s -> Some s
        | _ -> None
      in
      Some
        {
          id;
          name;
          session_key;
          message;
          schedule_str;
          enabled;
          agent_name;
          ephemeral;
          expires_at;
        }
    else None
  in
  ignore (Sqlite3.finalize stmt);
  result

let remove_job ~db ~name =
  let sql = "DELETE FROM cron_jobs WHERE name = ?" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  Sqlite3.changes db > 0

let toggle_job ~db ~name =
  let sql =
    "UPDATE cron_jobs SET enabled = CASE WHEN enabled = 1 THEN 0 ELSE 1 END \
     WHERE name = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE ->
      ignore (Sqlite3.finalize stmt);
      if Sqlite3.changes db > 0 then Ok ()
      else Error (Printf.sprintf "Job '%s' not found." name)
  | rc ->
      ignore (Sqlite3.finalize stmt);
      Error (Printf.sprintf "SQLite error: %s" (Sqlite3.Rc.to_string rc))

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

let expire_job ~db ~name =
  let sql = "UPDATE cron_jobs SET enabled = 0 WHERE name = ?" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt)

let tick ~db ~session_mgr
    ?(deliver :
       (channel:string ->
       channel_id:string ->
       text:string ->
       (unit, string) result Lwt.t)
       option) () =
  let open Lwt.Syntax in
  let jobs = list_jobs ~db in
  let now = Unix.gettimeofday () in
  let enabled_jobs = List.filter (fun j -> j.enabled) jobs in
  let* () =
    Lwt_list.iter_s
      (fun (job : job) ->
        let is_expired =
          match job.expires_at with
          | Some ea ->
              let now_str =
                let tm = Unix.gmtime now in
                Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
                  (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour
                  tm.tm_min tm.tm_sec
              in
              now_str >= ea
          | None -> false
        in
        if is_expired then begin
          Logs.info (fun m ->
              m "Cron job %s: TTL expired, auto-disabling" job.name);
          expire_job ~db ~name:job.name;
          let run_id = record_run_start ~db ~job_name:job.name in
          record_run_finish ~db ~run_id ~status:"expired"
            ~result_preview:"Job TTL expired; auto-disabled.";
          Lwt.return_unit
        end
        else
          match parse_schedule job.schedule_str with
          | Error _ -> Lwt.return_unit
          | Ok sched ->
              let last_run = get_last_run_time ~db ~job_name:job.name in
              if should_run sched ~last_run ~now then begin
                if job.ephemeral then begin
                  let run_id = record_run_start ~db ~job_name:job.name in
                  Logs.info (fun m ->
                      m
                        "Cron job %s: enqueuing ephemeral bg task for session \
                         %s"
                        job.name job.session_key);
                  let channel_info =
                    Memory.get_session_channel ~db ~session_key:job.session_key
                  in
                  let channel = Option.map fst channel_info in
                  let channel_id = Option.map snd channel_info in
                  (match
                     Background_task.enqueue ~db ~runner:Local
                       ~require_git:false ~use_worktree:false
                       ~repo_path:(Dot_dir.path ()) ~prompt:job.message
                       ~session_key:job.session_key ?channel ?channel_id ()
                   with
                  | Ok task_id ->
                      record_run_finish ~db ~run_id ~status:"delegated"
                        ~result_preview:(Printf.sprintf "bg task %d" task_id);
                      prune_runs ~db ~job_name:job.name ~keep:20
                  | Error err ->
                      record_run_finish ~db ~run_id ~status:"error"
                        ~result_preview:err;
                      prune_runs ~db ~job_name:job.name ~keep:20);
                  Lwt.return_unit
                end
                else begin
                  let run_id = record_run_start ~db ~job_name:job.name in
                  Logs.info (fun m ->
                      m "Cron job %s: starting turn for session %s" job.name
                        job.session_key);
                  Lwt.async (fun () ->
                      Lwt.catch
                        (fun () ->
                          (* Post the cron prompt into chat before running the
                         LLM turn, so users see what initiated the response. *)
                          let prompt_text =
                            Printf.sprintf "[cron:%s] %s" job.name job.message
                          in
                          let* () =
                            match
                              Session.find_registered_notifier session_mgr
                                ~key:job.session_key
                            with
                            | Some notify ->
                                Lwt.catch
                                  (fun () -> notify prompt_text)
                                  (fun exn ->
                                    Logs.warn (fun m ->
                                        m
                                          "Cron job %s: prompt delivery via \
                                           notifier failed: %s"
                                          job.name (Printexc.to_string exn));
                                    Lwt.return_unit)
                            | None -> (
                                match
                                  ( deliver,
                                    Memory.get_session_channel ~db
                                      ~session_key:job.session_key )
                                with
                                | Some deliver_fn, Some (channel, channel_id) ->
                                    let* _result =
                                      Lwt.catch
                                        (fun () ->
                                          deliver_fn ~channel ~channel_id
                                            ~text:prompt_text)
                                        (fun exn ->
                                          Lwt.return
                                            (Error (Printexc.to_string exn)))
                                    in
                                    (match _result with
                                    | Ok () ->
                                        Logs.info (fun m ->
                                            m
                                              "Cron job %s: prompt posted to \
                                               %s:%s"
                                              job.name channel channel_id)
                                    | Error err ->
                                        Logs.warn (fun m ->
                                            m
                                              "Cron job %s: prompt delivery \
                                               failed: %s"
                                              job.name err));
                                    Lwt.return_unit
                                | _ -> Lwt.return_unit)
                          in
                          let* result =
                            Session.turn session_mgr ~key:job.session_key
                              ~message:job.message ()
                          in
                          Logs.info (fun m ->
                              m "Cron job %s: LLM turn complete (%d chars)"
                                job.name (String.length result));
                          (* Delivery phase: check if a persistent notifier already
                         delivered during the turn, otherwise attempt explicit
                         delivery via the channel info stored in session_state. *)
                          let has_notifier =
                            Option.is_some
                              (Session.find_registered_notifier session_mgr
                                 ~key:job.session_key)
                          in
                          if has_notifier then begin
                            Logs.info (fun m ->
                                m
                                  "Cron job %s: notifier present, delivery \
                                   handled during turn"
                                  job.name);
                            record_run_finish ~db ~run_id ~status:"ok"
                              ~result_preview:result;
                            prune_runs ~db ~job_name:job.name ~keep:20;
                            Lwt.return_unit
                          end
                          else
                            match
                              ( deliver,
                                Memory.get_session_channel ~db
                                  ~session_key:job.session_key )
                            with
                            | Some deliver_fn, Some (channel, channel_id) -> (
                                Logs.info (fun m ->
                                    m
                                      "Cron job %s: attempting delivery via \
                                       %s:%s"
                                      job.name channel channel_id);
                                let* delivery_result =
                                  Lwt.catch
                                    (fun () ->
                                      deliver_fn ~channel ~channel_id
                                        ~text:result)
                                    (fun exn ->
                                      Lwt.return
                                        (Error (Printexc.to_string exn)))
                                in
                                match delivery_result with
                                | Ok () ->
                                    Logs.info (fun m ->
                                        m "Cron job %s: delivery succeeded"
                                          job.name);
                                    record_run_finish ~db ~run_id ~status:"ok"
                                      ~result_preview:result;
                                    prune_runs ~db ~job_name:job.name ~keep:20;
                                    Lwt.return_unit
                                | Error err ->
                                    Logs.warn (fun m ->
                                        m "Cron job %s: delivery failed: %s"
                                          job.name err);
                                    record_run_finish ~db ~run_id
                                      ~status:"delivery_failed"
                                      ~result_preview:
                                        (Printf.sprintf
                                           "LLM ok, delivery failed: %s\n\
                                            Response: %s"
                                           err
                                           (if String.length result > 200 then
                                              String.sub result 0 200
                                            else result));
                                    prune_runs ~db ~job_name:job.name ~keep:20;
                                    Lwt.return_unit)
                            | _ ->
                                (* CLI session or no deliver callback — mark ok *)
                                Logs.info (fun m ->
                                    m
                                      "Cron job %s: no channel info or deliver \
                                       callback, marking ok"
                                      job.name);
                                record_run_finish ~db ~run_id ~status:"ok"
                                  ~result_preview:result;
                                prune_runs ~db ~job_name:job.name ~keep:20;
                                Lwt.return_unit)
                        (fun exn ->
                          Logs.err (fun m ->
                              m "Cron job %s: turn failed: %s" job.name
                                (Printexc.to_string exn));
                          record_run_finish ~db ~run_id ~status:"error"
                            ~result_preview:(Printexc.to_string exn);
                          Lwt.return_unit));
                  Lwt.return_unit
                end
              end
              else Lwt.return_unit)
      enabled_jobs
  in
  Lwt.return_unit

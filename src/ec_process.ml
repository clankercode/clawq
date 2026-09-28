(* ec_process.ml — Error Correction process entry point.
   Runs as a separate process spawned by the daemon. Scans daemon.log,
   session DB, and background tasks for errors, then feeds them into
   the multi-model diagnosis pipeline. *)

let daemon_log_path () = Dot_dir.sub "daemon.log"

(* --- Signal state --- *)
let pause_requested = ref false
let shutdown_requested = ref false

(* --- Lock file (flock) --- *)
let acquire_lock () =
  let path = Error_watcher.lock_file_path () in
  let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
  try
    Unix.lockf fd Unix.F_TLOCK 0;
    (* B823: record the holder pid through the locked fd so a failed acquirer
       can read it back; never rewrite via rename (inode swap under lock). *)
    (try
       ignore (Unix.lseek fd 0 Unix.SEEK_SET);
       Unix.ftruncate fd 0;
       let s = Printf.sprintf "%d\n" (Unix.getpid ()) in
       ignore (Unix.write_substring fd s 0 (String.length s))
     with _ -> ());
    Some fd
  with Unix.Unix_error _ ->
    Unix.close fd;
    None

let release_lock fd =
  (try Unix.lockf fd Unix.F_ULOCK 0 with _ -> ());
  try Unix.close fd with _ -> ()

(* B823: decide whether the current lock holder may be replaced and, when so,
   terminate it. B828: EC children never write the shared daemon heartbeat, so
   a fresh daemon heartbeat must not vouch for the holder. Holder health is
   judged from the holder pid itself (zombie-aware liveness): a live holder is
   healthy and refused; a dead, zombie, or orphaned holder may be replaced. *)
let resolve_lock_takeover ?(orphaned : bool option) () : (string, string) result
    =
  let log msg = Printf.eprintf "EC process: %s\n%!" msg in
  let holder_pid =
    match Daemon_liveness.read_holder_pid (Error_watcher.lock_file_path ()) with
    | Some pid -> Some pid
    | None -> Error_watcher.read_pid_file ()
  in
  let alive pid = Daemon_liveness.process_alive pid in
  let orphaned =
    match orphaned with
    | Some o -> o
    | None -> (
        match holder_pid with
        | Some pid when alive pid -> (
            (* /proc unavailable (non-Linux): assume not orphaned and keep the
               live holder rather than killing a healthy sibling. *)
            match Daemon_liveness.proc_parent_pid pid with
            | Some ppid -> ppid <> Unix.getppid ()
            | None -> false)
        | _ -> false)
  in
  let decision =
    match holder_pid with
    | None ->
        Daemon_liveness.Takeover "no holder pid recorded (stale lock/pid state)"
    | Some pid ->
        if not (alive pid) then
          Daemon_liveness.Takeover (Printf.sprintf "holder pid %d is gone" pid)
        else if orphaned then
          Daemon_liveness.Takeover
            (Printf.sprintf
               "holder pid %d no longer belongs to a live daemon (orphaned EC)"
               pid)
        else
          Daemon_liveness.Refuse
            (Printf.sprintf
               "holder pid %d is healthy (EC process alive); a healthy EC is \
                running, refusing takeover. Use `clawq service restart` to \
                restart it intentionally"
               pid)
  in
  match (decision, holder_pid) with
  | Daemon_liveness.Refuse msg, _ -> Error msg
  | Daemon_liveness.Takeover reason, holder -> (
      let note = Printf.sprintf "takeover: %s" reason in
      log note;
      match holder with
      | None -> Ok note
      | Some pid -> (
          if not (alive pid) then Ok note
          else
            match Daemon_liveness.kill_holder pid with
            | Ok how ->
                let full = Printf.sprintf "%s; %s" note how in
                log full;
                Ok full
            | Error err ->
                let full =
                  Printf.sprintf "takeover aborted: %s; %s" reason err
                in
                log full;
                Error full))

(* --- Daemon log scanner --- *)
type log_scan_state = { mutable offset : int; mutable inode : int }

let create_log_scan_state () = { offset = 0; inode = 0 }

let scan_daemon_log ~(config : Runtime_config.t) ~scan_state ~seen =
  let path = daemon_log_path () in
  if not (Sys.file_exists path) then ([], seen)
  else
    let st = Unix.stat path in
    let file_size = st.Unix.st_size in
    let file_inode = st.Unix.st_ino in
    (* Detect log rotation: inode changed or file shrunk *)
    if file_inode <> scan_state.inode || file_size < scan_state.offset then begin
      scan_state.offset <- 0;
      scan_state.inode <- file_inode
    end;
    if file_size <= scan_state.offset then ([], seen)
    else
      let ic = open_in path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          seek_in ic scan_state.offset;
          let entries = ref [] in
          let seen = ref seen in
          let cooldown_s = config.error_watcher.cooldown_s in
          let ignore_pats = config.error_watcher.ignore_patterns in
          let max = config.error_watcher.max_errors_per_batch in
          (try
             while List.length !entries < max do
               let line = input_line ic in
               match Error_watcher.parse_log_line line with
               | Some entry when entry.level = "ERROR" || entry.level = "WARN"
                 ->
                   let msg_lower = String.lowercase_ascii entry.message in
                   let ignored =
                     List.exists
                       (fun pat ->
                         String_util.contains msg_lower
                           (String.lowercase_ascii pat))
                       ignore_pats
                   in
                   if
                     (not ignored)
                     && not
                          (Error_watcher.is_duplicate ~cooldown_s ~seen:!seen
                             entry)
                   then begin
                     entries := entry :: !entries;
                     seen := Error_watcher.update_seen ~seen:!seen entry
                   end
               | _ -> ()
             done
           with End_of_file -> ());
          scan_state.offset <- pos_in ic;
          scan_state.inode <- file_inode;
          (List.rev !entries, !seen))

(* --- Session DB scanner --- *)
let excluded_session_prefixes =
  [ "__error_correction__"; "__postmortem_"; "__debate__" ]

let is_excluded_session key =
  List.exists
    (fun prefix ->
      String.length key >= String.length prefix
      && String.sub key 0 (String.length prefix) = prefix)
    excluded_session_prefixes

let scan_session_errors ~db ~last_scan_time ~(config : Runtime_config.t) ~seen =
  let sql =
    "SELECT session_key, content, created_at FROM messages WHERE role = 'tool' \
     AND content LIKE 'Error:%' AND created_at > ? ORDER BY created_at ASC \
     LIMIT ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT last_scan_time));
      ignore
        (Sqlite3.bind stmt 2
           (Sqlite3.Data.INT
              (Int64.of_int config.error_watcher.max_errors_per_batch)));
      let entries = ref [] in
      let seen = ref seen in
      let cooldown_s = config.error_watcher.cooldown_s in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let session_key =
          match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let content =
          match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let created_at =
          match Sqlite3.column stmt 2 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        if not (is_excluded_session session_key) then begin
          let entry : Error_watcher.error_entry =
            {
              source = SessionError;
              session_key = Some session_key;
              level = "ERROR";
              message = content;
              timestamp = created_at;
              raw_line = content;
            }
          in
          if not (Error_watcher.is_duplicate ~cooldown_s ~seen:!seen entry) then begin
            entries := entry :: !entries;
            seen := Error_watcher.update_seen ~seen:!seen entry
          end
        end
      done;
      (List.rev !entries, !seen))

(* --- Background task failure scanner --- *)
let scan_background_task_failures ~db ~last_scan_time
    ~(config : Runtime_config.t) ~seen =
  let sql =
    "SELECT id, prompt, log_path, finished_at FROM background_tasks WHERE \
     status = 'failed' AND finished_at > ? ORDER BY finished_at ASC LIMIT ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT last_scan_time));
      ignore
        (Sqlite3.bind stmt 2
           (Sqlite3.Data.INT
              (Int64.of_int config.error_watcher.max_errors_per_batch)));
      let entries = ref [] in
      let seen = ref seen in
      let cooldown_s = config.error_watcher.cooldown_s in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let task_id =
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0
        in
        let prompt =
          match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let log_path =
          match Sqlite3.column stmt 2 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let finished_at =
          match Sqlite3.column stmt 3 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let log_tail =
          match log_path with
          | Some p when Sys.file_exists p -> (
              try
                let ic = open_in p in
                Fun.protect
                  ~finally:(fun () -> close_in_noerr ic)
                  (fun () ->
                    let size = in_channel_length ic in
                    let start = max 0 (size - 2000) in
                    seek_in ic start;
                    let buf = Buffer.create (size - start) in
                    (try
                       while true do
                         Buffer.add_char buf (input_char ic)
                       done
                     with End_of_file -> ());
                    Buffer.contents buf)
              with _ -> "")
          | _ -> ""
        in
        let message =
          Printf.sprintf "Background task %d failed: %s\n%s" task_id
            (String.sub prompt 0 (min 200 (String.length prompt)))
            (if log_tail <> "" then "Log tail: " ^ log_tail else "")
        in
        let entry : Error_watcher.error_entry =
          {
            source = BackgroundTaskLog;
            session_key = None;
            level = "ERROR";
            message;
            timestamp = finished_at;
            raw_line = message;
          }
        in
        if not (Error_watcher.is_duplicate ~cooldown_s ~seen:!seen entry) then begin
          entries := entry :: !entries;
          seen := Error_watcher.update_seen ~seen:!seen entry
        end
      done;
      (List.rev !entries, !seen))

(* --- Correlated context formatting (T003) --- *)
type db_message = {
  session_key : string;
  content : string;
  created_at : string;
}

type correlated_item =
  | LogEntry of Error_watcher.error_entry
  | DbMessage of db_message

let format_correlated_context ~log_entries ~db_messages =
  let items =
    List.map (fun e -> (e.Error_watcher.timestamp, LogEntry e)) log_entries
    @ List.map (fun (m : db_message) -> (m.created_at, DbMessage m)) db_messages
  in
  let sorted = List.sort (fun (t1, _) (t2, _) -> String.compare t1 t2) items in
  let buf = Buffer.create 1024 in
  List.iter
    (fun (_, item) ->
      match item with
      | LogEntry e ->
          Buffer.add_string buf
            (Printf.sprintf "[%s] %s%s %s\n" e.timestamp e.level
               (match e.session_key with
               | Some k -> " [" ^ k ^ "]"
               | None -> "")
               e.message)
      | DbMessage m ->
          Buffer.add_string buf
            (Printf.sprintf "[%s] DB: session %s tool_result: %s\n" m.created_at
               m.session_key m.content))
    sorted;
  Buffer.contents buf

let correlate_log_and_db ~log_entries ~db ~window_s =
  let db_messages = ref [] in
  List.iter
    (fun (entry : Error_watcher.error_entry) ->
      match entry.session_key with
      | None -> ()
      | Some key ->
          let sql =
            "SELECT content, created_at FROM messages WHERE session_key = ? \
             AND role = 'tool' AND created_at >= datetime(?, '-' || ? || ' \
             seconds') AND created_at <= datetime(?, '+' || ? || ' seconds') \
             ORDER BY created_at ASC LIMIT 10"
          in
          let stmt = Sqlite3.prepare db sql in
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
            (fun () ->
              let ws = string_of_int (int_of_float window_s) in
              ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key));
              ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT entry.timestamp));
              ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT ws));
              ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT entry.timestamp));
              ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT ws));
              while Sqlite3.step stmt = Sqlite3.Rc.ROW do
                let content =
                  match Sqlite3.column stmt 0 with
                  | Sqlite3.Data.TEXT s -> s
                  | _ -> ""
                in
                let created_at =
                  match Sqlite3.column stmt 1 with
                  | Sqlite3.Data.TEXT s -> s
                  | _ -> ""
                in
                db_messages :=
                  { session_key = key; content; created_at } :: !db_messages
              done))
    log_entries;
  let db_messages = List.rev !db_messages in
  format_correlated_context ~log_entries ~db_messages

(* --- Main scan cycle --- *)
let run_scan_cycle ~db ~config ~log_scan_state ~seen =
  let last_scan_time =
    let t =
      Unix.gettimeofday ()
      -. config.Runtime_config.error_watcher.scan_interval_s
    in
    Time_util.sql_datetime_utc ~t ()
  in
  let log_entries, seen =
    scan_daemon_log ~config ~scan_state:log_scan_state ~seen
  in
  let session_entries, seen =
    scan_session_errors ~db ~last_scan_time ~config ~seen
  in
  let bg_entries, seen =
    scan_background_task_failures ~db ~last_scan_time ~config ~seen
  in
  let all_entries = log_entries @ session_entries @ bg_entries in
  let actionable =
    List.filter
      (fun e -> Error_watcher.classify_error e = Error_watcher.Actionable)
      all_entries
  in
  (actionable, all_entries, seen)

(* --- Entry point --- *)
let run_daemon_mode () =
  (* Load config *)
  let config = Config_loader.load () in
  (* Own DB connection *)
  let db_path = Dot_dir.db_path () in
  let db = Memory.init ~db_path () in
  (* Acquire lock (B823: take over a hung/dead holder instead of giving up) *)
  let lock_fd =
    match acquire_lock () with
    | Some fd -> fd
    | None -> (
        match resolve_lock_takeover () with
        | Error refusal ->
            Printf.eprintf
              "EC process: lock held by a healthy instance, refusing to start: \
               %s\n"
              refusal;
            exit 1
        | Ok note -> (
            Printf.eprintf "EC process: %s; retrying lock acquisition\n%!" note;
            match acquire_lock () with
            | Some fd -> fd
            | None ->
                Printf.eprintf
                  "EC process: lock still held after takeover (%s)\n" note;
                exit 1))
  in
  (* Write PID file *)
  Error_watcher.write_pid_file (Unix.getpid ());
  (* Signal handlers *)
  Sys.set_signal Sys.sigusr2
    (Sys.Signal_handle (fun _ -> pause_requested := true));
  Sys.set_signal Sys.sigterm
    (Sys.Signal_handle (fun _ -> shutdown_requested := true));
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> exit 0));
  (* Main scan loop *)
  let log_scan_state = create_log_scan_state () in
  let seen = ref [] in
  let scan_interval = config.error_watcher.scan_interval_s in
  (try
     while not !shutdown_requested do
       if !pause_requested then begin
         (* SIGUSR2: pause briefly then exit for graceful handoff *)
         Unix.sleepf 2.0;
         shutdown_requested := true
       end
       else begin
         (try
            let actionable, _all, new_seen =
              run_scan_cycle ~db ~config ~log_scan_state ~seen:!seen
            in
            seen := new_seen;
            if actionable <> [] then begin
              let context =
                correlate_log_and_db ~log_entries:actionable ~db ~window_s:30.0
              in
              Ec_diagnosis.init_ec_reports_schema db;
              Lwt_main.run
                (Ec_diagnosis.run_pipeline ~db ~config ~entries:actionable
                   ~context ())
            end
          with exn ->
            Printf.eprintf "EC scan cycle error: %s\n%!"
              (Printexc.to_string exn));
         (* Sleep in short increments so SIGTERM is noticed promptly *)
         let deadline = Unix.gettimeofday () +. scan_interval in
         while (not !shutdown_requested) && Unix.gettimeofday () < deadline do
           Unix.sleepf 0.5
         done
       end
     done
   with _ -> ());
  (* Cleanup *)
  Error_watcher.remove_pid_file ();
  release_lock lock_fd;
  ignore (Sqlite3.db_close db)

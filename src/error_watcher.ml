type error_source = DaemonLog | SessionError | BackgroundTaskLog

type error_entry = {
  source : error_source;
  session_key : string option;
  level : string;
  message : string;
  timestamp : string;
  raw_line : string;
}

let pid_file_path () = Dot_dir.sub "ec_process.pid"
let lock_file_path () = Dot_dir.sub "ec_process.lock"
let ansi_re = Str.regexp "\027\\[[0-9;]*[a-zA-Z]"
let strip_ansi s = Str.global_replace ansi_re "" s

let log_line_re =
  Str.regexp
    "\\[\\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\.[0-9][0-9][0-9]\\)\\] \
     \\([A-Z]+\\) \\(\\[\\([^]]*\\)\\] \\)?\\(.*\\)"

let parse_log_line s =
  let cleaned = strip_ansi s in
  if Str.string_match log_line_re cleaned 0 then
    let timestamp = Str.matched_group 1 cleaned in
    let level = Str.matched_group 2 cleaned in
    let session_key =
      try Some (Str.matched_group 4 cleaned) with Not_found -> None
    in
    let message = Str.matched_group 5 cleaned in
    Some
      {
        source = DaemonLog;
        session_key;
        level;
        message;
        timestamp;
        raw_line = s;
      }
  else None

type error_class = Transient | Actionable

let transient_patterns =
  [
    "connection refused";
    "timeout";
    "timed out";
    "429";
    "ECONNRESET";
    "temporarily unavailable";
    "rate limit";
  ]

let classify_error (entry : error_entry) =
  let msg = String.lowercase_ascii entry.message in
  if
    List.exists
      (fun pat -> String_util.contains msg (String.lowercase_ascii pat))
      transient_patterns
  then Transient
  else Actionable

let normalize_first_line s =
  match String.split_on_char '\n' s with
  | [] -> ""
  | first :: _ ->
      let trimmed = String.trim first in
      let buf = Buffer.create (String.length trimmed) in
      String.iter
        (fun c ->
          if c >= '0' && c <= '9' then Buffer.add_char buf '#'
          else Buffer.add_char buf c)
        trimmed;
      Buffer.contents buf

let is_duplicate ~cooldown_s ~seen entry =
  let key = normalize_first_line entry.message in
  match List.assoc_opt key seen with
  | Some last_time ->
      let now = Unix.gettimeofday () in
      now -. last_time < cooldown_s
  | None -> false

let update_seen ~seen entry =
  let key = normalize_first_line entry.message in
  let now = Unix.gettimeofday () in
  (key, now) :: List.filter (fun (k, _) -> k <> key) seen

let is_dev_build () =
  let v = Build_info.version_dev in
  let n = String.length v in
  n >= 4 && String.sub v (n - 4) 4 = "-dev"

let error_source_to_string = function
  | DaemonLog -> "daemon_log"
  | SessionError -> "session_error"
  | BackgroundTaskLog -> "background_task_log"

let error_source_of_string = function
  | "session_error" -> SessionError
  | "background_task_log" -> BackgroundTaskLog
  | _ -> DaemonLog

let error_entry_to_json (e : error_entry) : Yojson.Safe.t =
  `Assoc
    [
      ("source", `String (error_source_to_string e.source));
      ( "session_key",
        match e.session_key with Some k -> `String k | None -> `Null );
      ("level", `String e.level);
      ("message", `String e.message);
      ("timestamp", `String e.timestamp);
      ("raw_line", `String e.raw_line);
    ]

let error_entry_of_json (j : Yojson.Safe.t) : error_entry =
  let open Yojson.Safe.Util in
  {
    source =
      (try j |> member "source" |> to_string |> error_source_of_string
       with _ -> DaemonLog);
    session_key =
      (try Some (j |> member "session_key" |> to_string) with _ -> None);
    level = (try j |> member "level" |> to_string with _ -> "ERROR");
    message = (try j |> member "message" |> to_string with _ -> "");
    timestamp = (try j |> member "timestamp" |> to_string with _ -> "");
    raw_line = (try j |> member "raw_line" |> to_string with _ -> "");
  }

(* --- Lifecycle management --- *)

type ec_state = {
  mutable pid : int option;
  mutable healthy : bool;
  (* B828: injectable spawn hook so health-check unit tests do not exec the
     real binary; defaults to spawning [clawq ec-run --daemon-mode]. *)
  mutable spawn_child : unit -> int;
}

let ec_log_path () = Dot_dir.sub "ec_process.log"

let default_spawn_ec () =
  let exe = try Sys.executable_name with _ -> "clawq" in
  let env = Unix.environment () in
  let proc =
    Process_group.start_to_file ~env ~log_path:(ec_log_path ())
      (Exec [| exe; "ec-run"; "--daemon-mode" |])
  in
  proc.file_pid

let create_state ?(spawn_child = default_spawn_ec) () =
  { pid = None; healthy = false; spawn_child }

let read_pid_file () =
  let path = pid_file_path () in
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let line = String.trim (input_line ic) in
        int_of_string_opt line)
  with _ -> None

let write_pid_file pid =
  let path = pid_file_path () in
  let tmp = path ^ ".new" in
  let oc = open_out tmp in
  try
    Printf.fprintf oc "%d\n" pid;
    close_out oc;
    Sys.rename tmp path
  with exn ->
    close_out_noerr oc;
    raise exn

let remove_pid_file () =
  let path = pid_file_path () in
  (try Sys.remove path with _ -> ());
  let tmp = path ^ ".new" in
  try Sys.remove tmp with _ -> ()

(* B828: EC children are direct children of the daemon, so an exited child
   stays a zombie until we waitpid it. WNOHANG poll: returns the exit status
   when a child was reaped; [None] when still running or not our child
   (ECHILD, e.g. a pid read from the pid file after a daemon restart). *)
let reap_ec_child pid : Unix.process_status option =
  match Unix.waitpid [ Unix.WNOHANG ] pid with
  | p, status when p = pid -> Some status
  | _ -> None
  | exception Unix.Unix_error _ -> None

let exit_status_string = function
  | Unix.WEXITED code -> Printf.sprintf "exit code %d" code
  | Unix.WSIGNALED sig_num -> Printf.sprintf "killed by signal %d" sig_num
  | Unix.WSTOPPED sig_num -> Printf.sprintf "stopped by signal %d" sig_num

let process_alive pid =
  (* B828: the old [Unix.kill (-pid) 0] probe succeeds for unreaped zombies,
     which made a crashed EC read as alive and blocked respawn. With /proc
     available, the leader's own state is the verdict: only a live leader runs
     the watcher, so a zombie leader is dead even though the group probe still
     answers for it. This also covers the fork→setsid window where the child
     is not yet its own group leader. When /proc is unreadable (non-Linux, or
     the leader was already reaped) keep the process-group probe to match
     background_task's convention and avoid false negatives while a descendant
     of the group is still alive. *)
  match Daemon_liveness.proc_state pid with
  | Some state -> state <> "Z"
  | None -> (
      try
        Unix.kill (-pid) 0;
        true
      with Unix.Unix_error _ -> false)

let start_ec_process ?(source = "unidentified") state =
  (* B828 double-spawn guard: while the recorded holder is alive, skip so a
     config-watch, handoff, or retry path cannot race the startup spawn. *)
  match state.pid with
  | Some pid when process_alive pid ->
      Logs.info (fun m ->
          m "EC process already running (pid=%d); skipping spawn from %s" pid
            source)
  | _ ->
      let pid =
        match state.pid with
        | Some old -> (
            (* reap the dead child so it cannot linger as a zombie *)
            match reap_ec_child old with
            | Some status ->
                Logs.info (fun m ->
                    m "EC process (pid=%d) exited with %s" old
                      (exit_status_string status));
                state.spawn_child ()
            | None -> state.spawn_child ())
        | None -> state.spawn_child ()
      in
      write_pid_file pid;
      state.pid <- Some pid;
      state.healthy <- true;
      Logs.info (fun m -> m "EC process started (pid=%d, source=%s)" pid source)

let kill_ec_process state =
  let pid =
    match state.pid with Some p -> Some p | None -> read_pid_file ()
  in
  (match pid with
  | Some p ->
      Process_group.signal_group p Sys.sigterm;
      (* Brief grace for cleanup, then force kill *)
      Unix.sleepf 0.1;
      if process_alive p then Process_group.signal_group p Sys.sigkill;
      (* Wait briefly for kernel to reap *)
      let deadline = Unix.gettimeofday () +. 2.0 in
      while process_alive p && Unix.gettimeofday () < deadline do
        Unix.sleepf 0.05
      done;
      (* B828: reap our child so no zombie remains *)
      ignore (reap_ec_child p)
  | None -> ());
  remove_pid_file ();
  state.pid <- None;
  state.healthy <- false;
  Lwt.return_unit

let stop_ec_process ?(timeout_s = 30.0) state =
  let open Lwt.Syntax in
  match state.pid with
  | None -> (
      match read_pid_file () with
      | Some pid when process_alive pid ->
          Process_group.signal_group pid Sys.sigterm;
          let deadline = Unix.gettimeofday () +. timeout_s in
          let rec wait () =
            if (not (process_alive pid)) || Unix.gettimeofday () >= deadline
            then Lwt.return_unit
            else
              let* () = Lwt_unix.sleep 0.5 in
              wait ()
          in
          let* () = wait () in
          if process_alive pid then Process_group.signal_group pid Sys.sigkill;
          (* B828: reap our child so no zombie remains *)
          ignore (reap_ec_child pid);
          remove_pid_file ();
          state.pid <- None;
          state.healthy <- false;
          Lwt.return_unit
      | _ ->
          remove_pid_file ();
          Lwt.return_unit)
  | Some pid ->
      Process_group.signal_group pid Sys.sigterm;
      let deadline = Unix.gettimeofday () +. timeout_s in
      let rec wait () =
        if (not (process_alive pid)) || Unix.gettimeofday () >= deadline then
          Lwt.return_unit
        else
          let* () = Lwt_unix.sleep 0.5 in
          wait ()
      in
      let* () = wait () in
      if process_alive pid then Process_group.signal_group pid Sys.sigkill;
      (* B828: reap our child so no zombie remains *)
      ignore (reap_ec_child pid);
      remove_pid_file ();
      state.pid <- None;
      state.healthy <- false;
      Lwt.return_unit

let check_ec_health state =
  match state.pid with
  | None ->
      state.healthy <- false;
      false
  | Some pid -> (
      (* B828: reap a dead child every tick so no zombie accumulates; the reap
         result is also the authoritative death signal for our own children. *)
      match reap_ec_child pid with
      | Some status ->
          Logs.warn (fun m ->
              m "EC process (pid=%d) exited with %s, restarting" pid
                (exit_status_string status));
          remove_pid_file ();
          state.pid <- None;
          state.healthy <- false;
          start_ec_process ~source:"health_check_respawn" state;
          true
      | None ->
          if process_alive pid then begin
            state.healthy <- true;
            true
          end
          else begin
            Logs.warn (fun m ->
                m "EC process (pid=%d) appears crashed, restarting" pid);
            remove_pid_file ();
            state.pid <- None;
            state.healthy <- false;
            start_ec_process ~source:"health_check_respawn" state;
            true
          end)

let graceful_handoff state =
  let open Lwt.Syntax in
  let old_pid = state.pid in
  (* B828: clear the slot so the double-spawn guard does not skip the
     replacement spawn; the old holder is retired below. *)
  state.pid <- None;
  start_ec_process ~source:"graceful_handoff" state;
  match old_pid with
  | None -> Lwt.return_unit
  | Some old ->
      if process_alive old then begin
        (try Unix.kill old Sys.sigusr2 with _ -> ());
        let deadline = Unix.gettimeofday () +. 90.0 in
        let rec wait () =
          if (not (process_alive old)) || Unix.gettimeofday () >= deadline then
            Lwt.return_unit
          else
            let* () = Lwt_unix.sleep 1.0 in
            wait ()
        in
        let* () = wait () in
        if process_alive old then begin
          Logs.warn (fun m ->
              m "Old EC process (pid=%d) still alive after 90s, sending SIGTERM"
                old);
          Process_group.signal_group old Sys.sigterm
        end;
        (* B828: reap the retired child so it does not linger as a zombie *)
        ignore (reap_ec_child old);
        Lwt.return_unit
      end
      else (
        ignore (reap_ec_child old);
        Lwt.return_unit)

let run_health_check_loop ?(interval_s = 30.0) ~shutdown state =
  let open Lwt.Syntax in
  let rec loop () =
    let* () = Lwt_unix.sleep interval_s in
    if Lwt.is_sleeping shutdown then begin
      ignore (check_ec_health state);
      loop ()
    end
    else Lwt.return_unit
  in
  loop ()

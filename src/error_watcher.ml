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
  let v = Build_info.version in
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

type ec_state = { mutable pid : int option; mutable healthy : bool }

let create_state () = { pid = None; healthy = false }

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

let process_alive pid =
  try
    Unix.kill pid 0;
    true
  with Unix.Unix_error _ -> false

let ec_log_path () = Dot_dir.sub "ec_process.log"

let start_ec_process state =
  let exe = try Sys.executable_name with _ -> "clawq" in
  let env = Unix.environment () in
  let log_path = ec_log_path () in
  let proc =
    Process_group.start_to_file ~env ~log_path
      (Exec [| exe; "ec-run"; "--daemon-mode" |])
  in
  let pid = proc.file_pid in
  write_pid_file pid;
  state.pid <- Some pid;
  state.healthy <- true;
  Logs.info (fun m -> m "EC process started (pid=%d)" pid)

let kill_ec_process state =
  let pid =
    match state.pid with Some p -> Some p | None -> read_pid_file ()
  in
  (match pid with
  | Some p ->
      Process_group.signal_group p Sys.sigterm;
      (* Brief grace for cleanup, then force kill *)
      Unix.sleepf 0.5;
      if process_alive p then Process_group.signal_group p Sys.sigkill;
      (* Wait briefly for kernel to reap *)
      let deadline = Unix.gettimeofday () +. 2.0 in
      while process_alive p && Unix.gettimeofday () < deadline do
        Unix.sleepf 0.05
      done
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
      remove_pid_file ();
      state.pid <- None;
      state.healthy <- false;
      Lwt.return_unit

let check_ec_health state =
  match state.pid with
  | None ->
      state.healthy <- false;
      false
  | Some pid ->
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
        start_ec_process state;
        true
      end

let graceful_handoff state =
  let open Lwt.Syntax in
  let old_pid = state.pid in
  start_ec_process state;
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
        Lwt.return_unit
      end
      else Lwt.return_unit

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

(* B768: direct process-group session host.

   Compatibility adapter that preserves the pre-seam behavior: fork+setsid
   via Process_group with stdout/stderr appended to the task log file.

   Session identity is "<pid>:<start-token>" where the start token is the
   process start time from /proc/<pid>/stat (clock ticks since boot). The
   token distinguishes the original process from an unrelated one that later
   reused the PID, so restart recovery can report [Stale] instead of
   silently adopting a stranger. Identities recorded before B768 (bare PID,
   no token) still parse; they just cannot detect PID reuse. *)

let kind = "direct"

(* /proc/<pid>/stat: "pid (comm) state ppid ..." — comm may contain spaces
   and parentheses, so fields are counted after the last ')'. starttime is
   field 22 overall, i.e. field 20 of the post-comm tail (state is field 3). *)
let proc_start_token pid =
  try
    let ic = open_in (Printf.sprintf "/proc/%d/stat" pid) in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let line = input_line ic in
        match String.rindex_opt line ')' with
        | None -> None
        | Some close_paren ->
            let tail =
              String.sub line (close_paren + 1)
                (String.length line - close_paren - 1)
            in
            let fields =
              tail |> String.split_on_char ' ' |> List.filter (fun s -> s <> "")
            in
            List.nth_opt fields 19)
  with _ -> None

let session_id_of_pid pid =
  match proc_start_token pid with
  | Some token -> Printf.sprintf "%d:%s" pid token
  | None -> string_of_int pid

let parse_session_id session_id =
  match String.index_opt session_id ':' with
  | None -> (int_of_string_opt session_id, None)
  | Some i ->
      let pid = int_of_string_opt (String.sub session_id 0 i) in
      let token =
        String.sub session_id (i + 1) (String.length session_id - i - 1)
      in
      (pid, if token = "" then None else Some token)

let pid_of_session_ref (session : Session_host.session_ref) =
  if session.host_kind <> kind && session.host_kind <> "" then None
  else
    match parse_session_id session.host_session_id with
    | Some pid, _ when pid > 0 -> Some pid
    | _ -> None

let pid_alive pid =
  Process_group.group_alive pid
  ||
    try
      Unix.kill pid 0;
      true
    with Unix.Unix_error _ -> false

let status (session : Session_host.session_ref) : Session_host.health =
  match parse_session_id session.host_session_id with
  | None, _ ->
      Unknown
        (Printf.sprintf "unparseable direct session id %S"
           session.host_session_id)
  | Some pid, _ when pid <= 0 ->
      Unknown (Printf.sprintf "non-positive pid %d" pid)
  | Some pid, token -> (
      if not (pid_alive pid) then Missing
      else
        match (token, proc_start_token pid) with
        | Some recorded, Some current when recorded <> current -> Stale
        | _ -> Live)

let start (spec : Session_host.start_spec) =
  Lwt.catch
    (fun () ->
      let proc =
        Process_group.start_to_file ~cwd:spec.cwd ~env:spec.env
          ~log_path:spec.log_path spec.command
      in
      let pid = proc.Process_group.file_pid in
      Lwt.return
        (Ok
           {
             Session_host.host_kind = kind;
             host_session_id = session_id_of_pid pid;
             log_path = Some spec.log_path;
           }))
    (fun exn ->
      Lwt.return
        (Error
           (Printf.sprintf
              "Failed to start direct session in %s: %s. Check that the binary \
               exists in PATH and the working directory is accessible."
              spec.cwd (Printexc.to_string exn))))

(* waitpid works only while the process is our child. After a daemon restart
   a readopted process is reparented to init and waitpid raises ECHILD; fall
   back to polling group liveness (matching pre-B768 readopt semantics,
   including the 300 s cap that reports exit 1 for a still-live group). *)
let wait (session : Session_host.session_ref) =
  match pid_of_session_ref session with
  | None ->
      Lwt.return
        (Error
           (Printf.sprintf
              "Cannot wait on direct session %S: no usable pid. The session \
               identity may predate the host seam or be corrupted."
              session.host_session_id))
  | Some pid ->
      let open Lwt.Syntax in
      Lwt.catch
        (fun () ->
          let* _, unix_status = Lwt_unix.waitpid [] pid in
          Lwt.return
            (Ok (Background_task_0_format.exit_code_of_status unix_status)))
        (function
          | Unix.Unix_error (Unix.ECHILD, _, _) ->
              let deadline = Unix.gettimeofday () +. 300.0 in
              let rec poll () =
                if not (Process_group.group_alive pid) then Lwt.return (Ok 0)
                else if Unix.gettimeofday () >= deadline then Lwt.return (Ok 1)
                else
                  let* () = Lwt_unix.sleep 5.0 in
                  poll ()
              in
              poll ()
          | exn ->
              Lwt.return
                (Error
                   (Printf.sprintf "waitpid failed for pid %d: %s" pid
                      (Printexc.to_string exn))))

let send_input (session : Session_host.session_ref) ~message:_ =
  Lwt.return
    (Error
       (Printf.sprintf
          "Direct session host does not support live input (session %s). Queue \
           a follow-up with `background message <id> <text>`; it is delivered \
           when the task resumes."
          session.host_session_id))

let cancel ?(grace_seconds = 0.2) (session : Session_host.session_ref) =
  match pid_of_session_ref session with
  | None ->
      Lwt.return
        (Error
           (Printf.sprintf
              "Cannot cancel direct session %S: no usable pid recorded."
              session.host_session_id))
  | Some pid ->
      let open Lwt.Syntax in
      let* () = Process_group.terminate ~grace_seconds pid in
      Lwt.return (Ok ())

let host : Session_host.t =
  {
    kind;
    supports_live_input = false;
    start;
    status;
    read_output = Session_host.default_read_output;
    send_input;
    wait;
    cancel;
    recover = status;
  }

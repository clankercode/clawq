(* B770: tmux session host — a widely-available fallback for Herdr.

   A runner is launched in a named, detached tmux session whose single pane
   runs a static /bin/sh wrapper: it tees combined output to the task log,
   prints a [clawq-exit:N] marker, and holds the pane open so the session
   stays inspectable and completion is detectable after the process exits.

   Untrusted text (prompts, issue bodies) is passed only as discrete argv
   elements to `tmux new-session -- <argv>` (tmux execs argv directly, no
   shell), and follow-up input is delivered via a tmux paste-buffer
   (`load-buffer` from a temp file + `paste-buffer`) so it is never parsed
   as a shell command line. The wrapper script itself is a constant. *)

type cli_result = { exit_code : int; stdout : string; stderr : string }
type runner_cli = string array -> cli_result Lwt.t
type sync_cli = string array -> cli_result

let kind = "tmux"
let binary = "tmux"
let exit_marker_prefix = "[clawq-exit:"

let wrapper_script =
  "log=\"$1\"; shift; { \"$@\" 2>&1; printf '\\n" ^ exit_marker_prefix
  ^ "%d]\\n' \"$?\"; } | tee -a \"$log\"; while :; do sleep 3600; done"

(* Collision-safe deterministic session name: clawq home path + log path both
   feed the hash, so different Clawq homes and tasks never collide, while the
   same task recovers to the same name. *)
let session_name ~log_path =
  let home = try Sys.getenv "HOME" with Not_found -> "" in
  let digest = Digest.to_hex (Digest.string (home ^ "\000" ^ log_path)) in
  Printf.sprintf "clawq-%s" (String.sub digest 0 12)

let tmux_available () = Background_task_0_format.command_exists binary

let missing_binary_error =
  "tmux is not installed or not in PATH. Install tmux, or re-enqueue the task \
   with host=direct (or host=herdr if Herdr is available)."

let default_run_cli : runner_cli =
 fun args ->
  let open Lwt.Syntax in
  let proc =
    Process_group.start ~env:(Unix.environment ())
      (Process_group.Exec (Array.append [| binary |] args))
  in
  Lwt.finalize
    (fun () ->
      let* stdout, stderr =
        Lwt.both
          (Lwt_io.read proc.Process_group.stdout)
          (Lwt_io.read proc.Process_group.stderr)
      in
      let* status = Process_group.wait proc.pid in
      Lwt.return
        {
          exit_code = Background_task_0_format.exit_code_of_status status;
          stdout;
          stderr;
        })
    (fun () -> Process_group.close proc)

let default_run_cli_sync : sync_cli =
 fun args ->
  let argv = Array.append [| binary |] args in
  let read_fd fd =
    let buf = Buffer.create 256 in
    let chunk = Bytes.create 4096 in
    let rec loop () =
      match Unix.read fd chunk 0 (Bytes.length chunk) with
      | 0 -> ()
      | n ->
          Buffer.add_subbytes buf chunk 0 n;
          loop ()
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
    in
    (try loop () with _ -> ());
    Unix.close fd;
    Buffer.contents buf
  in
  let out_r, out_w = Unix.pipe ~cloexec:true () in
  let err_r, err_w = Unix.pipe ~cloexec:true () in
  match Unix.create_process binary argv Unix.stdin out_w err_w with
  | exception exn ->
      List.iter
        (fun fd -> try Unix.close fd with _ -> ())
        [ out_r; out_w; err_r; err_w ];
      { exit_code = 127; stdout = ""; stderr = Printexc.to_string exn }
  | pid ->
      Unix.close out_w;
      Unix.close err_w;
      let stdout = read_fd out_r in
      let stderr = read_fd err_r in
      let _, status = Unix.waitpid [] pid in
      {
        exit_code = Background_task_0_format.exit_code_of_status status;
        stdout;
        stderr;
      }

let scan_exit_code log_path =
  let tail = Background_task_0_format.read_log_tail log_path (64 * 1024) in
  let plen = String.length exit_marker_prefix in
  match
    Str.search_backward
      (Str.regexp_string exit_marker_prefix)
      tail (String.length tail)
  with
  | exception Not_found -> None
  | i -> (
      let rest = String.sub tail (i + plen) (String.length tail - i - plen) in
      match String.index_opt rest ']' with
      | Some j -> int_of_string_opt (String.sub rest 0 j)
      | None -> None)

let has_session_sync ~run_cli_sync ~name =
  (run_cli_sync [| "has-session"; "-t"; "=" ^ name |]).exit_code = 0

let health_of ~alive ~log_path : Session_host.health =
  if alive then
    match scan_exit_code log_path with
    | Some code -> Session_host.Exited code
    | None -> Session_host.Live
  else
    match scan_exit_code log_path with
    | Some code -> Session_host.Exited code
    | None -> Session_host.Missing

let make ?(run_cli = default_run_cli) ?(run_cli_sync = default_run_cli_sync)
    ?(available = tmux_available) ?(poll_interval = 2.0) () : Session_host.t =
  let start (spec : Session_host.start_spec) =
    if not (available ()) then Lwt.return (Error missing_binary_error)
    else
      match spec.command with
      | Process_group.Shell _ ->
          Lwt.return
            (Error
               "The tmux session host only accepts exec-style argv commands \
                (never shell strings). Build the command as argv, or use \
                host=direct for trusted shell commands.")
      | Process_group.Exec argv ->
          let name = session_name ~log_path:spec.log_path in
          let open Lwt.Syntax in
          (* If a session with this name survived, adopt it rather than
             colliding. *)
          let* pre = run_cli [| "has-session"; "-t"; "=" ^ name |] in
          if pre.exit_code = 0 then
            Lwt.return
              (Ok
                 {
                   Session_host.host_kind = kind;
                   host_session_id = name;
                   log_path = Some spec.log_path;
                 })
          else
            let full_argv =
              Array.concat
                [
                  [|
                    "new-session";
                    "-d";
                    "-s";
                    name;
                    "-c";
                    spec.cwd;
                    "--";
                    "/bin/sh";
                    "-c";
                    wrapper_script;
                    "clawq-tmux-host";
                    spec.log_path;
                  |];
                  argv;
                ]
            in
            let* result = run_cli full_argv in
            if result.exit_code = 0 then
              Lwt.return
                (Ok
                   {
                     Session_host.host_kind = kind;
                     host_session_id = name;
                     log_path = Some spec.log_path;
                   })
            else
              Lwt.return
                (Error
                   (Printf.sprintf
                      "tmux new-session failed (exit %d): %s%s. Check `tmux \
                       list-sessions` and that the working directory exists."
                      result.exit_code result.stdout result.stderr))
  in
  let status (session : Session_host.session_ref) : Session_host.health =
    if not (available ()) then Session_host.Unknown missing_binary_error
    else
      let alive =
        has_session_sync ~run_cli_sync ~name:session.host_session_id
      in
      health_of ~alive ~log_path:(Option.value session.log_path ~default:"")
  in
  let read_output ?max_chars session =
    Session_host.default_read_output ?max_chars session
  in
  (* Follow-up input via a tmux paste buffer: the text is written to a temp
     file, loaded as a buffer, and pasted into the pane — never interpreted
     as a command line. *)
  let send_input (session : Session_host.session_ref) ~message =
    if not (available ()) then Lwt.return (Error missing_binary_error)
    else
      let open Lwt.Syntax in
      let tmp = Filename.temp_file "clawq-tmux-input" ".txt" in
      let* () =
        Lwt_io.with_file ~mode:Lwt_io.Output tmp (fun oc ->
            Lwt_io.write oc (message ^ "\n"))
      in
      Lwt.finalize
        (fun () ->
          let buf = "clawq-in-" ^ session.host_session_id in
          let* load = run_cli [| "load-buffer"; "-b"; buf; tmp |] in
          if load.exit_code <> 0 then
            Lwt.return
              (Error (Printf.sprintf "tmux load-buffer failed: %s" load.stderr))
          else
            let* paste =
              run_cli
                [|
                  "paste-buffer";
                  "-d";
                  "-b";
                  buf;
                  "-t";
                  "=" ^ session.host_session_id;
                |]
            in
            if paste.exit_code = 0 then Lwt.return (Ok ())
            else
              Lwt.return
                (Error
                   (Printf.sprintf
                      "tmux paste-buffer failed (session %s may be gone): %s"
                      session.host_session_id paste.stderr)))
        (fun () ->
          (try Sys.remove tmp with _ -> ());
          Lwt.return_unit)
  in
  let close_session (session : Session_host.session_ref) =
    let open Lwt.Syntax in
    let* _ =
      run_cli
        [| "kill-session"; "-t"; "=" ^ session.Session_host.host_session_id |]
    in
    Lwt.return (Ok ())
  in
  let rec wait_loop (session : Session_host.session_ref) =
    let open Lwt.Syntax in
    match Option.bind session.log_path scan_exit_code with
    | Some code ->
        let* _ = close_session session in
        Lwt.return (Ok code)
    | None ->
        let alive =
          has_session_sync ~run_cli_sync ~name:session.host_session_id
        in
        if not alive then
          match Option.bind session.log_path scan_exit_code with
          | Some code -> Lwt.return (Ok code)
          | None ->
              Lwt.return
                (Error
                   (Printf.sprintf
                      "tmux session %s ended before completion. Use \
                       'background retry' to re-queue."
                      session.host_session_id))
        else
          let* () = Lwt_unix.sleep poll_interval in
          wait_loop session
  in
  let cancel ?grace_seconds:_ session = close_session session in
  {
    Session_host.kind;
    supports_live_input = true;
    ready =
      (fun () -> if available () then Ok () else Error missing_binary_error);
    start;
    status;
    read_output;
    send_input;
    wait = wait_loop;
    cancel;
    recover = status;
  }

let attach_command (session : Session_host.session_ref) =
  Printf.sprintf "tmux attach -t %s" session.host_session_id

let host : Session_host.t = make ()

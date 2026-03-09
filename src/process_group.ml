type command = Exec of string array | Shell of string

type t = {
  pid : int;
  stdout : Lwt_io.input_channel;
  stderr : Lwt_io.input_channel;
}

let close_noerr fd = try Unix.close fd with _ -> ()

let close_channel_noerr ch =
  Lwt.catch (fun () -> Lwt_io.close ch) (fun _ -> Lwt.return_unit)

let getenv_from_env env key =
  let prefix = key ^ "=" in
  let plen = String.length prefix in
  let rec loop i =
    if i >= Array.length env then None
    else
      let entry = env.(i) in
      if String.length entry >= plen && String.sub entry 0 plen = prefix then
        Some (String.sub entry plen (String.length entry - plen))
      else loop (i + 1)
  in
  loop 0

let split_path_entries path =
  path |> String.split_on_char ':' |> List.filter (fun part -> part <> "")

let resolve_executable ~env prog =
  if String.contains prog '/' then Some prog
  else
    let path =
      match getenv_from_env env "PATH" with
      | Some value -> value
      | None -> "/usr/local/bin:/usr/bin:/bin"
    in
    let rec loop = function
      | [] -> None
      | dir :: rest ->
          let candidate = Filename.concat dir prog in
          if Sys.file_exists candidate then Some candidate else loop rest
    in
    loop (split_path_entries path)

let exec_command ~env = function
  | Shell command -> Unix.execve "/bin/sh" [| "/bin/sh"; "-c"; command |] env
  | Exec argv -> (
      match Array.to_list argv with
      | [] -> failwith "empty argv"
      | prog :: _ -> (
          match resolve_executable ~env prog with
          | Some executable -> Unix.execve executable argv env
          | None -> failwith (Printf.sprintf "command not found: %s" prog)))

let start ?cwd ~env command =
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let stderr_r, stderr_w = Unix.pipe ~cloexec:true () in
  match Unix.fork () with
  | 0 -> (
      let setup_child () =
        ignore (Unix.setsid ());
        (match cwd with Some dir -> Unix.chdir dir | None -> ());
        let stdin_fd = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
        Unix.dup2 stdin_fd Unix.stdin;
        Unix.dup2 stdout_w Unix.stdout;
        Unix.dup2 stderr_w Unix.stderr;
        close_noerr stdin_fd;
        close_noerr stdout_r;
        close_noerr stdout_w;
        close_noerr stderr_r;
        close_noerr stderr_w;
        exec_command ~env command
      in
      try setup_child ()
      with exn ->
        let msg = Printexc.to_string exn ^ "\n" in
        ignore (Unix.write_substring Unix.stderr msg 0 (String.length msg));
        exit 127)
  | pid ->
      close_noerr stdout_w;
      close_noerr stderr_w;
      {
        pid;
        stdout =
          Lwt_io.of_fd ~mode:Lwt_io.Input (Lwt_unix.of_unix_file_descr stdout_r);
        stderr =
          Lwt_io.of_fd ~mode:Lwt_io.Input (Lwt_unix.of_unix_file_descr stderr_r);
      }

type t_file = { file_pid : int }

let start_to_file ?cwd ~env ~log_path command =
  let log_fd =
    Unix.openfile log_path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ] 0o644
  in
  match Unix.fork () with
  | 0 -> (
      let setup_child () =
        ignore (Unix.setsid ());
        (match cwd with Some dir -> Unix.chdir dir | None -> ());
        let stdin_fd = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
        Unix.dup2 stdin_fd Unix.stdin;
        Unix.dup2 log_fd Unix.stdout;
        Unix.dup2 log_fd Unix.stderr;
        close_noerr stdin_fd;
        close_noerr log_fd;
        exec_command ~env command
      in
      try setup_child ()
      with exn ->
        let msg = Printexc.to_string exn ^ "\n" in
        ignore (Unix.write_substring Unix.stderr msg 0 (String.length msg));
        exit 127)
  | pid ->
      close_noerr log_fd;
      { file_pid = pid }

let signal_group pid signal = try Unix.kill (-pid) signal with _ -> ()

let group_alive pid =
  try
    Unix.kill (-pid) 0;
    true
  with Unix.Unix_error _ -> false

let wait pid =
  let open Lwt.Syntax in
  let* _, status = Lwt_unix.waitpid [] pid in
  Lwt.return status

let close t =
  let open Lwt.Syntax in
  let* () = close_channel_noerr t.stdout in
  close_channel_noerr t.stderr

let terminate ?(grace_seconds = 0.2) pid =
  let open Lwt.Syntax in
  signal_group pid Sys.sigterm;
  let* () = Lwt_unix.sleep grace_seconds in
  signal_group pid Sys.sigkill;
  Lwt.return_unit

let terminate_blocking ?(grace_seconds = 0.2) ?(wait_seconds = 1.0) pid =
  signal_group pid Sys.sigterm;
  Unix.sleepf grace_seconds;
  signal_group pid Sys.sigkill;
  let deadline = Unix.gettimeofday () +. wait_seconds in
  while group_alive pid && Unix.gettimeofday () < deadline do
    Unix.sleepf 0.02
  done

let terminate_immediately pid =
  signal_group pid Sys.sigkill;
  Lwt.return_unit

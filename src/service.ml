let clawq_dir () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat home ".clawq"

let pid_path () = Filename.concat (clawq_dir ()) "daemon.pid"
let log_path () = Filename.concat (clawq_dir ()) "daemon.log"

let ensure_dir path =
  try if not (Sys.file_exists path) then Sys.mkdir path 0o755
  with _ -> ()

let read_pid () =
  let path = pid_path () in
  if not (Sys.file_exists path) then None
  else
    try
      let ic = open_in path in
      let s = String.trim (input_line ic) in
      close_in ic;
      match int_of_string_opt s with
      | Some pid ->
        (try Unix.kill pid 0; Some pid
         with Unix.Unix_error _ -> None)
      | None -> None
    with _ -> None

let write_pid pid =
  ensure_dir (clawq_dir ());
  let oc = open_out (pid_path ()) in
  output_string oc (string_of_int pid);
  close_out oc

let remove_pid () =
  let path = pid_path () in
  if Sys.file_exists path then
    (try Sys.remove path with _ -> ())

let cmd_start ~config =
  match read_pid () with
  | Some pid ->
    Printf.sprintf "Daemon already running (pid %d)" pid
  | None ->
    ensure_dir (clawq_dir ());
    let logs_dir = clawq_dir () in
    ensure_dir logs_dir;
    match Unix.fork () with
    | 0 ->
      ignore (Unix.setsid ());
      let log_fd = Unix.openfile (log_path ())
        [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ] 0o644 in
      Unix.dup2 log_fd Unix.stdout;
      Unix.dup2 log_fd Unix.stderr;
      Unix.close log_fd;
      let null_fd = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
      Unix.dup2 null_fd Unix.stdin;
      Unix.close null_fd;
      write_pid (Unix.getpid ());
      (try Lwt_main.run (Daemon.run ~config)
       with _ -> ());
      remove_pid ();
      exit 0
    | pid ->
      Printf.sprintf "Daemon started (pid %d)" pid

let cmd_stop () =
  match read_pid () with
  | None -> "Daemon is not running"
  | Some pid ->
    (try Unix.kill pid Sys.sigterm with _ -> ());
    let rec wait attempts =
      if attempts <= 0 then begin
        (try Unix.kill pid Sys.sigkill with _ -> ());
        remove_pid ();
        Printf.sprintf "Daemon killed (pid %d)" pid
      end else begin
        Unix.sleepf 0.5;
        try Unix.kill pid 0; wait (attempts - 1)
        with Unix.Unix_error _ ->
          remove_pid ();
          Printf.sprintf "Daemon stopped (pid %d)" pid
      end
    in
    wait 10

let cmd_status () =
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add "Service status:";
  (match read_pid () with
   | None -> add "  daemon: not running"
   | Some pid -> add (Printf.sprintf "  daemon: running (pid %d)" pid));
  let state_path = Filename.concat (clawq_dir ()) "daemon_state.json" in
  if Sys.file_exists state_path then begin
    try
      let json = Yojson.Safe.from_file state_path in
      let open Yojson.Safe.Util in
      (try
         let comps = json |> member "components" |> to_assoc in
         List.iter (fun (name, v) ->
           add (Printf.sprintf "  %s: %s" name (to_string v))
         ) comps
       with _ -> ())
    with _ -> ()
  end;
  add (Printf.sprintf "  log: %s" (log_path ()));
  add (Printf.sprintf "  pid file: %s" (pid_path ()));
  List.rev !lines |> String.concat "\n"

let cmd_restart ~config =
  let stop_msg = cmd_stop () in
  Unix.sleepf 1.0;
  let start_msg = cmd_start ~config in
  stop_msg ^ "\n" ^ start_msg

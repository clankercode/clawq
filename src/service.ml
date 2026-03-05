let clawq_dir () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat home ".clawq"

let pid_path () = Filename.concat (clawq_dir ()) "daemon.pid"
let pid_meta_path () = Filename.concat (clawq_dir ()) "daemon.pid.meta"
let log_path () = Filename.concat (clawq_dir ()) "daemon.log"

let read_file path =
  try
    let ic = open_in path in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Some s
  with _ -> None

let proc_start_ticks pid =
  let path = Printf.sprintf "/proc/%d/stat" pid in
  match read_file path with
  | None -> None
  | Some stat -> (
      let idx = try Some (String.rindex stat ')') with _ -> None in
      match idx with
      | None -> None
      | Some i -> (
          let rest = String.sub stat (i + 2) (String.length stat - i - 2) in
          let fields =
            String.split_on_char ' ' rest |> List.filter (fun s -> s <> "")
          in
          try Some (List.nth fields 19) with _ -> None))

let proc_cmdline_contains ~needle pid =
  let path = Printf.sprintf "/proc/%d/cmdline" pid in
  match read_file path with
  | None -> false
  | Some s ->
      let hay = String.lowercase_ascii s in
      let nee = String.lowercase_ascii needle in
      let hlen = String.length hay in
      let nlen = String.length nee in
      let rec loop i =
        if i + nlen > hlen then false
        else if String.sub hay i nlen = nee then true
        else loop (i + 1)
      in
      nlen > 0 && loop 0

let read_pid_meta () =
  match read_file (pid_meta_path ()) with
  | None -> None
  | Some s ->
      let v = String.trim s in
      if v = "" then None else Some v

let write_pid_meta pid =
  match proc_start_ticks pid with
  | None -> ()
  | Some ticks ->
      let oc = open_out (pid_meta_path ()) in
      output_string oc ticks;
      close_out oc

let pid_identity_ok pid =
  match read_pid_meta () with
  | Some expected -> (
      match proc_start_ticks pid with
      | Some actual -> actual = expected
      | None -> false)
  | None -> proc_cmdline_contains ~needle:"clawq" pid

let ensure_dir path =
  try if not (Sys.file_exists path) then Sys.mkdir path 0o755 with _ -> ()

let read_pid () =
  let path = pid_path () in
  if not (Sys.file_exists path) then None
  else
    try
      let ic = open_in path in
      let s = String.trim (input_line ic) in
      close_in ic;
      match int_of_string_opt s with
      | Some pid -> (
          try
            Unix.kill pid 0;
            if pid_identity_ok pid then Some pid else None
          with Unix.Unix_error _ -> None)
      | None -> None
    with _ -> None

let write_pid pid =
  ensure_dir (clawq_dir ());
  let oc = open_out (pid_path ()) in
  output_string oc (string_of_int pid);
  close_out oc;
  write_pid_meta pid

let remove_pid () =
  let path = pid_path () in
  (if Sys.file_exists path then try Sys.remove path with _ -> ());
  let meta = pid_meta_path () in
  if Sys.file_exists meta then try Sys.remove meta with _ -> ()

let cmd_start ~config =
  match read_pid () with
  | Some pid -> Printf.sprintf "Daemon already running (pid %d)" pid
  | None -> (
      ensure_dir (clawq_dir ());
      let logs_dir = clawq_dir () in
      ensure_dir logs_dir;
      match Unix.fork () with
      | 0 ->
          ignore (Unix.setsid ());
          let log_fd =
            Unix.openfile (log_path ())
              [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ]
              0o644
          in
          Unix.dup2 log_fd Unix.stdout;
          Unix.dup2 log_fd Unix.stderr;
          Unix.close log_fd;
          let null_fd = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
          Unix.dup2 null_fd Unix.stdin;
          Unix.close null_fd;
          write_pid (Unix.getpid ());
          (try Lwt_main.run (Daemon.run ~config) with _ -> ());
          remove_pid ();
          exit 0
      | _pid -> (
          let rec wait_for_ready attempts =
            if attempts <= 0 then None
            else
              match read_pid () with
              | Some daemon_pid -> Some daemon_pid
              | None ->
                  Unix.sleepf 0.1;
                  wait_for_ready (attempts - 1)
          in
          match wait_for_ready 30 with
          | Some daemon_pid ->
              Printf.sprintf "Daemon started (pid %d)" daemon_pid
          | None ->
              Printf.sprintf "Daemon failed to become ready. Check logs at %s"
                (log_path ())))

let cmd_stop () =
  match read_pid () with
  | None -> "Daemon is not running"
  | Some pid ->
      if not (pid_identity_ok pid) then begin
        remove_pid ();
        Printf.sprintf
          "Refusing to stop pid %d: process identity mismatch; stale pid state \
           removed"
          pid
      end
      else begin
        (try Unix.kill pid Sys.sigterm with _ -> ());
        let rec wait attempts =
          if attempts <= 0 then begin
            (try Unix.kill pid Sys.sigkill with _ -> ());
            remove_pid ();
            Printf.sprintf "Daemon killed (pid %d)" pid
          end
          else begin
            Unix.sleepf 0.5;
            try
              Unix.kill pid 0;
              wait (attempts - 1)
            with Unix.Unix_error _ ->
              remove_pid ();
              Printf.sprintf "Daemon stopped (pid %d)" pid
          end
        in
        wait 10
      end

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
      try
        let comps = json |> member "components" |> to_assoc in
        List.iter
          (fun (name, v) -> add (Printf.sprintf "  %s: %s" name (to_string v)))
          comps
      with _ -> ()
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

let clawq_dir () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat home ".clawq"

let pid_path () = Filename.concat (clawq_dir ()) "daemon.pid"
let pid_meta_path () = Filename.concat (clawq_dir ()) "daemon.pid.meta"
let log_path () = Filename.concat (clawq_dir ()) "daemon.log"

let read_file path =
  try
    let ic = open_in_bin path in
    let buf = Buffer.create 256 in
    let chunk = Bytes.create 256 in
    let rec loop () =
      let n = input ic chunk 0 256 in
      if n > 0 then (
        Buffer.add_subbytes buf chunk 0 n;
        loop ())
    in
    (try loop () with End_of_file -> ());
    close_in ic;
    Some (Buffer.contents buf)
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

let singleton_lock_path () = Filename.concat (clawq_dir ()) "daemon.lock"

let acquire_singleton_lock () =
  ensure_dir (clawq_dir ());
  let fd =
    Unix.openfile (singleton_lock_path ()) [ Unix.O_CREAT; Unix.O_RDWR ] 0o644
  in
  try
    Unix.lockf fd Unix.F_TLOCK 0;
    Some fd
  with Unix.Unix_error _ ->
    (try Unix.close fd with _ -> ());
    None

let release_singleton_lock = function
  | None -> ()
  | Some fd -> (
      (try Unix.lockf fd Unix.F_ULOCK 0 with _ -> ());
      try Unix.close fd with _ -> ())

let nofork_env = "CLAWQ_DAEMON_NOFORK"
let internal_nofork_env = "CLAWQ_DAEMON_INTERNAL_NOFORK"

let has_prefix ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

let build_env ~set_vars ~unset_vars =
  let should_drop entry =
    List.exists (fun key -> has_prefix ~prefix:(key ^ "=") entry) unset_vars
  in
  let overridden = List.map fst set_vars in
  Array.to_list (Unix.environment ())
  |> List.filter (fun entry ->
      (not (should_drop entry))
      && not
           (List.exists
              (fun key -> has_prefix ~prefix:(key ^ "=") entry)
              overridden))
  |> fun env ->
  env @ List.map (fun (k, v) -> k ^ "=" ^ v) set_vars |> Array.of_list

let daemon_start_argv () = [| Sys.executable_name; "service"; "start" |]

let handle_daemon_exit ?(execve = Unix.execve) exit_intent =
  match exit_intent with
  | Daemon.Shutdown -> ()
  | Daemon.Restart ->
      let set_vars =
        match Restart_notify.read () with
        | Some (channel, channel_id) ->
            [
              (nofork_env, "1");
              ( Restart_notify.env_key,
                Restart_notify.to_json_string ~channel ~channel_id );
            ]
        | None -> [ (nofork_env, "1") ]
      in
      execve Sys.executable_name (daemon_start_argv ())
        (build_env ~set_vars ~unset_vars:[ internal_nofork_env ])

let run_nofork_start ?(execve = Unix.execve)
    ?(run_daemon = fun ~config -> Lwt_main.run (Daemon.run ~config)) ~config ()
    =
  let nofork_requested = Sys.getenv_opt nofork_env = Some "1" in
  let internal_nofork = Sys.getenv_opt internal_nofork_env = Some "1" in
  if nofork_requested && not internal_nofork then begin
    execve Sys.executable_name (daemon_start_argv ())
      (build_env
         ~set_vars:[ (internal_nofork_env, "1") ]
         ~unset_vars:[ nofork_env ]);
    ""
  end
  else begin
    Unix.putenv internal_nofork_env "";
    Logs.info (fun m -> m "Daemon restarting in-place (NOFORK mode)");
    let result = try run_daemon ~config with _ -> Daemon.Shutdown in
    handle_daemon_exit ~execve result;
    ""
  end

let cmd_start ~config =
  if
    Sys.getenv_opt nofork_env = Some "1"
    || Sys.getenv_opt internal_nofork_env = Some "1"
  then run_nofork_start ~config ()
  else
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
            let result =
              try Lwt_main.run (Daemon.run ~config) with _ -> Daemon.Shutdown
            in
            handle_daemon_exit result;
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

let cmd_signal_restart ?(read_pid = read_pid) ?(send_signal = Unix.kill) () =
  match read_pid () with
  | None -> "Daemon is not running"
  | Some pid -> (
      try
        send_signal pid Sys.sigusr1;
        Printf.sprintf "Restart signal sent to daemon (PID %d)" pid
      with Unix.Unix_error (err, _, _) ->
        Printf.sprintf "Failed to signal daemon pid %d: %s" pid
          (Unix.error_message err))

let cmd_restart ~config =
  let stop_msg = cmd_stop () in
  Unix.sleepf 1.0;
  let start_msg = cmd_start ~config in
  stop_msg ^ "\n" ^ start_msg

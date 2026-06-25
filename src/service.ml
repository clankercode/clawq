let clawq_dir () = Dot_dir.path ()

let test_disable_live_signal_restart_env =
  "CLAWQ_TEST_DISABLE_LIVE_SIGNAL_RESTART"

let pid_path () = Filename.concat (clawq_dir ()) "daemon.pid"
let pid_meta_path () = Filename.concat (clawq_dir ()) "daemon.pid.meta"
let log_path () = Filename.concat (clawq_dir ()) "daemon.log"

let has_prefix ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

let env_truthy name =
  match Sys.getenv_opt name with
  | Some ("" | "0" | "false" | "False" | "FALSE" | "no" | "No" | "NO") -> false
  | Some _ -> true
  | None -> false

let normalize_path path =
  let path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path
  in
  try Unix.realpath path with _ -> path

let path_is_under ~parent path =
  let parent = normalize_path parent in
  let path = normalize_path path in
  path = parent || has_prefix ~prefix:(parent ^ Filename.dir_sep) path

let should_block_live_signal_restart () =
  if not (env_truthy test_disable_live_signal_restart_env) then false
  else
    let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
    not (path_is_under ~parent:(Filename.get_temp_dir_name ()) home)

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

let daemon_status_line pid =
  let exe_note =
    let exe_link = Printf.sprintf "/proc/%d/exe" pid in
    try
      let target = Unix.readlink exe_link in
      if Restart_exec.path_is_deleted target then Some target else None
    with _ -> None
  in
  match (exe_note, Daemon_status.daemon_uptime_suffix pid) with
  | Some target, Some uptime ->
      Printf.sprintf
        "  daemon: running (pid %d, uptime %s, WARNING deleted exe: %s)" pid
        uptime target
  | Some target, None ->
      Printf.sprintf "  daemon: running (pid %d, WARNING deleted exe: %s)" pid
        target
  | None, Some uptime ->
      Printf.sprintf "  daemon: running (pid %d, uptime %s)" pid uptime
  | None, None -> Printf.sprintf "  daemon: running (pid %d)" pid

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

let daemon_start_argv ~executable = [| executable; "service"; "start" |]

let handle_daemon_exit ?(execve = Unix.execve) exit_intent =
  match exit_intent with
  | Daemon.Shutdown -> ()
  | Daemon.Restart -> (
      let executable = Restart_exec.executable () in
      match Restart_exec.validate_and_fix executable with
      | Error msg ->
          Logs.err (fun m ->
              m "Restart aborted: %s; falling back to clean shutdown" msg)
      | Ok executable -> (
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
          try
            execve executable
              (daemon_start_argv ~executable)
              (build_env ~set_vars
                 ~unset_vars:[ internal_nofork_env; Restart_notify.env_key ])
          with Unix.Unix_error (err, func, arg) ->
            Logs.err (fun m ->
                m
                  "execve failed for %s: %s(%s)%s; falling back to clean \
                   shutdown"
                  executable (Unix.error_message err) func
                  (if arg <> "" then ": " ^ arg else ""))))

let run_nofork_start ?(execve = Unix.execve)
    ?(run_daemon = fun ~config -> Lwt_main.run (Daemon.run ~config)) ~config ()
    =
  let nofork_requested = Sys.getenv_opt nofork_env = Some "1" in
  let internal_nofork = Sys.getenv_opt internal_nofork_env = Some "1" in
  if nofork_requested && not internal_nofork then begin
    let executable = Restart_exec.executable () in
    (match Restart_exec.validate_and_fix executable with
    | Error msg ->
        Logs.err (fun m ->
            m "Restart aborted: %s; falling back to clean shutdown" msg)
    | Ok executable -> (
        try
          execve executable
            (daemon_start_argv ~executable)
            (build_env
               ~set_vars:[ (internal_nofork_env, "1") ]
               ~unset_vars:[ nofork_env ])
        with Unix.Unix_error (err, func, arg) ->
          Logs.err (fun m ->
              m "execve failed for %s: %s(%s)%s; falling back to clean shutdown"
                executable (Unix.error_message err) func
                (if arg <> "" then ": " ^ arg else ""))));
    ""
  end
  else begin
    Unix.putenv internal_nofork_env "";
    Logs.info (fun m -> m "Daemon restarting in-place (NOFORK mode)");
    let result =
      try run_daemon ~config with
      | Lwt_util.Deadlock_timeout label ->
          Logs.err (fun m ->
              m "Daemon caught Deadlock_timeout(%s), restarting" label);
          Daemon.Restart
      | exn ->
          Logs.err (fun m -> m "Daemon error: %s" (Printexc.to_string exn));
          Daemon.Shutdown
    in
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
              try Lwt_main.run (Daemon.run ~config) with
              | Lwt_util.Deadlock_timeout label ->
                  Logs.err (fun m ->
                      m "Daemon caught Deadlock_timeout(%s), restarting" label);
                  Daemon.Restart
              | exn ->
                  Logs.err (fun m ->
                      m "Daemon error: %s" (Printexc.to_string exn));
                  Daemon.Shutdown
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

type platform = Linux | Darwin | Other of string

let detect_platform () =
  try
    let ic = Unix.open_process_in "uname -s" in
    let line =
      Fun.protect
        (fun () -> input_line ic |> String.trim)
        ~finally:(fun () -> ignore (Unix.close_process_in ic))
    in
    match String.lowercase_ascii line with
    | "linux" -> Linux
    | "darwin" -> Darwin
    | s -> Other s
  with _ -> (
    match Sys.os_type with
    | "Unix" -> Other "unix"
    | s -> Other (String.lowercase_ascii s))

(** Run a hardcoded shell command and return its stdout trimmed. All current
    callers pass compile-time constant strings (e.g. "systemctl --user is-active
    clawq"). Do NOT pass user-supplied or unsanitised input — the command is
    executed via [Unix.open_process_in] without shell quoting. *)
let run_command_default cmd =
  try
    let ic = Unix.open_process_in cmd in
    let buf = Buffer.create 256 in
    (try
       while true do
         Buffer.add_char buf (input_char ic)
       done
     with End_of_file -> ());
    ignore (Unix.close_process_in ic);
    String.trim (Buffer.contents buf)
  with _ -> ""

let systemd_unit_path () =
  let home = try Sys.getenv "HOME" with Not_found -> "~" in
  Filename.concat home ".config/systemd/user/clawq.service"

let launchd_plist_path () =
  let home = try Sys.getenv "HOME" with Not_found -> "~" in
  Filename.concat home "Library/LaunchAgents/org.clawq.daemon.plist"

let autostart_status ?(detect_platform = detect_platform)
    ?(run_command = run_command_default) () =
  match detect_platform () with
  | Linux ->
      let unit_exists = Sys.file_exists (systemd_unit_path ()) in
      if unit_exists then
        let out = run_command "systemctl --user is-enabled clawq 2>/dev/null" in
        match out with
        | "enabled" ->
            "  autostart: enabled (systemd user unit)\n\
            \    disable with: systemctl --user disable clawq\n\
            \    uninstall with: clawq service uninstall"
        | "disabled" ->
            "  autostart: installed but disabled\n\
            \    enable with: systemctl --user enable clawq\n\
            \    or reinstall with: clawq service install"
        | _ ->
            Printf.sprintf
              "  autostart: installed (status: %s)\n\
              \    enable with: systemctl --user enable clawq"
              out
      else "  autostart: not installed (run: clawq service install)"
  | Darwin ->
      if Sys.file_exists (launchd_plist_path ()) then
        "  autostart: installed (launchd plist)\n\
        \    unload with: launchctl unload \
         ~/Library/LaunchAgents/org.clawq.daemon.plist\n\
        \    uninstall with: clawq service uninstall"
      else "  autostart: not installed (run: clawq service install)"
  | Other _ -> "  autostart: not available on this platform"

let cmd_status ?(detect_platform = detect_platform)
    ?(run_command = run_command_default) () =
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add "Service status:";
  (match read_pid () with
  | None -> add "  daemon: not running"
  | Some pid -> (
      add (daemon_status_line pid);
      match Daemon_status.daemon_uptime_line pid with
      | Some line -> add line
      | None -> ()));
  add (autostart_status ~detect_platform ~run_command ());
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
  if should_block_live_signal_restart () then
    "Refusing to signal daemon during tests outside a temp HOME. Wrap the test \
     in with_temp_home."
  else
    match read_pid () with
    | None -> "Daemon is not running"
    | Some pid -> (
        try
          Logs.info (fun m ->
              m
                "Sending SIGUSR1 to daemon pid %d (source: clawq service \
                 signal-restart)"
                pid);
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

let cmd_systemd_unit () =
  let executable = Restart_exec.executable () in
  let pid_file = Filename.concat (Dot_dir.path ()) "daemon.pid" in
  Printf.sprintf
    {|# clawq systemd user service
# Install: clawq service systemd-unit > ~/.config/systemd/user/clawq.service
# Enable:  systemctl --user enable clawq
# Start:   systemctl --user start clawq
# Logs:    journalctl --user -u clawq -f

[Unit]
Description=clawq daemon
After=network-online.target

[Service]
Type=forking
PIDFile=%s
ExecStart=%s service start
ExecStop=%s service stop
ExecReload=%s service signal-restart
Restart=on-failure
RestartSec=5
WatchdogSec=120

[Install]
WantedBy=default.target|}
    pid_file executable executable executable

let cmd_launchd_plist () =
  let executable = Restart_exec.executable () in
  let log_file = log_path () in
  Printf.sprintf
    {|<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<!-- clawq launchd agent
     Install: clawq service launchd-plist > ~/Library/LaunchAgents/org.clawq.daemon.plist
     Load:    launchctl load ~/Library/LaunchAgents/org.clawq.daemon.plist
     Unload:  launchctl unload ~/Library/LaunchAgents/org.clawq.daemon.plist
     Or use:  clawq service install / clawq service uninstall -->
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>org.clawq.daemon</string>
  <key>ProgramArguments</key>
  <array>
    <string>%s</string>
    <string>service</string>
    <string>start</string>
  </array>
  <key>RunAtLoad</key>
  <true/>
  <key>KeepAlive</key>
  <dict>
    <key>SuccessfulExit</key>
    <false/>
  </dict>
  <key>EnvironmentVariables</key>
  <dict>
    <key>CLAWQ_DAEMON_NOFORK</key>
    <string>1</string>
  </dict>
  <key>StandardOutPath</key>
  <string>%s</string>
  <key>StandardErrorPath</key>
  <string>%s</string>
</dict>
</plist>|}
    executable log_file log_file

let ensure_parent_dir path =
  let parent = Filename.dirname path in
  let rec mkdir_p dir =
    if Sys.file_exists dir then ()
    else begin
      mkdir_p (Filename.dirname dir);
      try Sys.mkdir dir 0o755 with Sys_error _ -> ()
    end
  in
  mkdir_p parent

let write_file path contents =
  ensure_parent_dir path;
  let oc = open_out path in
  Fun.protect
    (fun () -> output_string oc contents)
    ~finally:(fun () -> close_out oc)

let cmd_install ?(detect_platform = detect_platform)
    ?(run_command = run_command_default) () =
  match detect_platform () with
  | Linux ->
      let unit_path = systemd_unit_path () in
      let unit_contents = cmd_systemd_unit () in
      write_file unit_path unit_contents;
      ignore (run_command "systemctl --user daemon-reload");
      ignore (run_command "systemctl --user enable clawq");
      let already_running = read_pid () <> None in
      let start_hint =
        if already_running then
          "Daemon is already running; the unit will manage future starts."
        else "Start now with: systemctl --user start clawq"
      in
      Printf.sprintf
        "Service installed and enabled.\n\
         Unit file: %s\n\
         The service will autostart on login.\n\
         To also start on boot without login, run: loginctl enable-linger $USER\n\
         Disable autostart with: systemctl --user disable clawq\n\
         %s"
        unit_path start_hint
  | Darwin ->
      let plist_path = launchd_plist_path () in
      let plist_contents = cmd_launchd_plist () in
      write_file plist_path plist_contents;
      let already_running = read_pid () <> None in
      if not already_running then
        ignore (run_command (Printf.sprintf "launchctl load %s" plist_path));
      let extra =
        if already_running then
          "\n\
           Daemon is already running; plist will take effect on next \
           boot/login."
        else ""
      in
      Printf.sprintf
        "Service installed and loaded.\n\
         Plist file: %s\n\
         The service will autostart on login (RunAtLoad).\n\
         Disable autostart with: launchctl unload %s\n\
         Or remove entirely with: clawq service uninstall%s"
        plist_path plist_path extra
  | Other platform ->
      Printf.sprintf
        "Service install is not supported on %s.\n\
         Manual alternatives:\n\
         - Generate a systemd unit: clawq service systemd-unit\n\
         - Generate a launchd plist: clawq service launchd-plist\n\
         - Use your platform's init system to run: clawq service start"
        platform

let cmd_uninstall ?(detect_platform = detect_platform)
    ?(run_command = run_command_default) () =
  match detect_platform () with
  | Linux ->
      let unit_path = systemd_unit_path () in
      ignore (run_command "systemctl --user stop clawq");
      ignore (run_command "systemctl --user disable clawq");
      (if Sys.file_exists unit_path then try Sys.remove unit_path with _ -> ());
      ignore (run_command "systemctl --user daemon-reload");
      Printf.sprintf "Service stopped, disabled, and unit file removed.\n%s"
        unit_path
  | Darwin ->
      let plist_path = launchd_plist_path () in
      if Sys.file_exists plist_path then begin
        ignore (run_command (Printf.sprintf "launchctl unload %s" plist_path));
        try Sys.remove plist_path with _ -> ()
      end;
      Printf.sprintf "Service unloaded and plist file removed.\n%s" plist_path
  | Other platform ->
      Printf.sprintf
        "Service uninstall is not supported on %s.\n\
         Stop the daemon manually with: clawq service stop"
        platform

let env_contains env entry =
  Array.exists (fun candidate -> candidate = entry) env

let env_lacks_key env key =
  not
    (Array.exists
       (fun entry ->
         let prefix = key ^ "=" in
         let plen = String.length prefix in
         String.length entry >= plen && String.sub entry 0 plen = prefix)
       env)

let test_handle_daemon_exit_restart_sets_nofork_and_execs () =
  let called = ref None in
  Service.handle_daemon_exit
    ~execve:(fun path argv env ->
      called := Some (path, Array.to_list argv, env))
    Daemon.Restart;
  Alcotest.(check bool)
    "nofork present in exec env" true
    (match !called with
    | Some (_, _, env) -> env_contains env "CLAWQ_DAEMON_NOFORK=1"
    | None -> false);
  Alcotest.(check bool)
    "internal nofork removed from exec env" true
    (match !called with
    | Some (_, _, env) -> env_lacks_key env "CLAWQ_DAEMON_INTERNAL_NOFORK"
    | None -> false);
  Alcotest.(check (option (triple string (list string) bool)))
    "execve called"
    (Some
       (Sys.executable_name, [ Sys.executable_name; "service"; "start" ], true))
    (match !called with
    | Some (path, argv, env) ->
        Some (path, argv, env_contains env "CLAWQ_DAEMON_NOFORK=1")
    | None -> None)

let test_handle_daemon_exit_restart_carries_restart_notify_env () =
  Restart_notify.write ~channel:"telegram" ~channel_id:"42";
  let called = ref None in
  Service.handle_daemon_exit
    ~execve:(fun _ _ env -> called := Some env)
    Daemon.Restart;
  let contains_restart_json =
    match !called with
    | None -> false
    | Some env ->
        Array.exists
          (fun entry ->
            let prefix = Restart_notify.env_key ^ "=" in
            let plen = String.length prefix in
            String.length entry >= plen
            && String.sub entry 0 plen = prefix
            && String.contains entry '4')
          env
  in
  Restart_notify.remove ();
  Alcotest.(check bool) "restart notify env present" true contains_restart_json

let test_handle_daemon_exit_shutdown_skips_exec () =
  let called = ref false in
  Service.handle_daemon_exit
    ~execve:(fun _ _ _ -> called := true)
    Daemon.Shutdown;
  Alcotest.(check bool) "no exec on shutdown" false !called

let test_run_nofork_start_reexecs_without_public_env () =
  Unix.putenv "CLAWQ_DAEMON_NOFORK" "1";
  Unix.putenv "CLAWQ_DAEMON_INTERNAL_NOFORK" "";
  let called = ref None in
  let config = Runtime_config.default in
  ignore
    (Service.run_nofork_start ~config
       ~execve:(fun path argv env ->
         called := Some (path, Array.to_list argv, env))
       ~run_daemon:(fun ~config:_ ->
         Alcotest.fail "daemon should not run before re-exec")
       ());
  Alcotest.(check bool)
    "public nofork removed from steady-state env" true
    (match !called with
    | Some (_, _, env) -> env_lacks_key env "CLAWQ_DAEMON_NOFORK"
    | None -> false);
  Alcotest.(check bool)
    "internal nofork added for re-exec" true
    (match !called with
    | Some (_, _, env) -> env_contains env "CLAWQ_DAEMON_INTERNAL_NOFORK=1"
    | None -> false)

let test_run_nofork_start_runs_daemon_in_internal_mode () =
  Unix.putenv "CLAWQ_DAEMON_NOFORK" "";
  Unix.putenv "CLAWQ_DAEMON_INTERNAL_NOFORK" "1";
  let ran_daemon = ref false in
  let internal_seen = ref None in
  let exec_called = ref false in
  let config = Runtime_config.default in
  let result =
    Service.run_nofork_start ~config
      ~execve:(fun _ _ _ -> exec_called := true)
      ~run_daemon:(fun ~config:_ ->
        ran_daemon := true;
        internal_seen := Sys.getenv_opt "CLAWQ_DAEMON_INTERNAL_NOFORK";
        Daemon.Shutdown)
      ()
  in
  Alcotest.(check string) "empty response" "" result;
  Alcotest.(check bool) "daemon run invoked" true !ran_daemon;
  Alcotest.(check (option string))
    "internal nofork cleared before daemon run" (Some "") !internal_seen;
  Alcotest.(check bool) "no exec on shutdown result" false !exec_called

let test_cmd_signal_restart_reports_missing_daemon () =
  let result = Service.cmd_signal_restart ~read_pid:(fun () -> None) () in
  Alcotest.(check string) "missing daemon" "Daemon is not running" result

let test_cmd_signal_restart_signals_running_daemon () =
  let signaled = ref None in
  let result =
    Service.cmd_signal_restart
      ~read_pid:(fun () -> Some 1234)
      ~send_signal:(fun pid signal -> signaled := Some (pid, signal))
      ()
  in
  Alcotest.(check string)
    "signal restart response" "Restart signal sent to daemon (PID 1234)" result;
  Alcotest.(check (option (pair int int)))
    "sigusr1 sent"
    (Some (1234, Sys.sigusr1))
    !signaled

let test_cmd_signal_restart_reports_signal_failure () =
  let result =
    Service.cmd_signal_restart
      ~read_pid:(fun () -> Some 1234)
      ~send_signal:(fun _ _ -> raise (Unix.Unix_error (Unix.ESRCH, "kill", "")))
      ()
  in
  Alcotest.(check bool)
    "signal failure reported" true
    (String.length result > 0
    &&
    let prefix = "Failed to signal daemon pid 1234:" in
    String.length result >= String.length prefix
    && String.sub result 0 (String.length prefix) = prefix)

let suite =
  [
    Alcotest.test_case "handle daemon exit restart sets nofork and execs" `Quick
      test_handle_daemon_exit_restart_sets_nofork_and_execs;
    Alcotest.test_case "handle daemon exit restart carries restart notify env"
      `Quick test_handle_daemon_exit_restart_carries_restart_notify_env;
    Alcotest.test_case "handle daemon exit shutdown skips exec" `Quick
      test_handle_daemon_exit_shutdown_skips_exec;
    Alcotest.test_case "run nofork start reexecs without public env" `Quick
      test_run_nofork_start_reexecs_without_public_env;
    Alcotest.test_case "run nofork start runs daemon in internal mode" `Quick
      test_run_nofork_start_runs_daemon_in_internal_mode;
    Alcotest.test_case "cmd signal restart reports missing daemon" `Quick
      test_cmd_signal_restart_reports_missing_daemon;
    Alcotest.test_case "cmd signal restart signals running daemon" `Quick
      test_cmd_signal_restart_signals_running_daemon;
    Alcotest.test_case "cmd signal restart reports signal failure" `Quick
      test_cmd_signal_restart_reports_signal_failure;
  ]

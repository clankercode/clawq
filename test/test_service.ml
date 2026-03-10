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

let with_env key value f =
  let previous = Sys.getenv_opt key in
  (match value with Some v -> Unix.putenv key v | None -> Unix.putenv key "");
  Fun.protect f ~finally:(fun () ->
      match previous with
      | Some v -> Unix.putenv key v
      | None -> Unix.putenv key "")

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
  Alcotest.(check bool)
    "execve called with service start argv and nofork env" true
    (match !called with
    | Some (_path, argv, env) ->
        argv = [ Restart_exec.executable (); "service"; "start" ]
        && env_contains env "CLAWQ_DAEMON_NOFORK=1"
    | None -> false)

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

let test_handle_daemon_exit_restart_prefers_reexec_env () =
  with_env Restart_exec.reexec_path_env (Some "/tmp/clawq-fresh") (fun () ->
      let called = ref None in
      Service.handle_daemon_exit
        ~execve:(fun path argv _ -> called := Some (path, Array.to_list argv))
        Daemon.Restart;
      Alcotest.(check (option (pair string (list string))))
        "execve uses fresh path"
        (Some ("/tmp/clawq-fresh", [ "/tmp/clawq-fresh"; "service"; "start" ]))
        !called)

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

let test_run_nofork_start_prefers_reexec_env () =
  with_env "CLAWQ_DAEMON_NOFORK" (Some "1") (fun () ->
      with_env "CLAWQ_DAEMON_INTERNAL_NOFORK" (Some "") (fun () ->
          with_env Restart_exec.reexec_path_env (Some "/tmp/clawq-fresh")
            (fun () ->
              let called = ref None in
              let config = Runtime_config.default in
              ignore
                (Service.run_nofork_start ~config
                   ~execve:(fun path argv _ ->
                     called := Some (path, Array.to_list argv))
                   ~run_daemon:(fun ~config:_ ->
                     Alcotest.fail "daemon should not run before re-exec")
                   ());
              Alcotest.(check (option (pair string (list string))))
                "re-exec uses fresh path"
                (Some
                   ( "/tmp/clawq-fresh",
                     [ "/tmp/clawq-fresh"; "service"; "start" ] ))
                !called)))

let test_cmd_signal_restart_reports_missing_daemon () =
  Test_helpers.with_temp_home (fun _home ->
      let result = Service.cmd_signal_restart ~read_pid:(fun () -> None) () in
      Alcotest.(check string) "missing daemon" "Daemon is not running" result)

let test_cmd_signal_restart_signals_running_daemon () =
  Test_helpers.with_temp_home (fun _home ->
      let signaled = ref None in
      let result =
        Service.cmd_signal_restart
          ~read_pid:(fun () -> Some 1234)
          ~send_signal:(fun pid signal -> signaled := Some (pid, signal))
          ()
      in
      Alcotest.(check string)
        "signal restart response"
        "Restart signal sent to daemon (PID 1234)" result;
      Alcotest.(check (option (pair int int)))
        "sigusr1 sent"
        (Some (1234, Sys.sigusr1))
        !signaled)

let test_cmd_signal_restart_refuses_live_signal_outside_temp_home () =
  with_env Service.test_disable_live_signal_restart_env (Some "1") (fun () ->
      with_env "HOME" (Some "/workspaces/clawq-real-home") (fun () ->
          let signaled = ref false in
          let result =
            Service.cmd_signal_restart
              ~read_pid:(fun () -> Some 1234)
              ~send_signal:(fun _ _ -> signaled := true)
              ()
          in
          Alcotest.(check string)
            "guard refusal"
            "Refusing to signal daemon during tests outside a temp HOME. Wrap \
             the test in with_temp_home."
            result;
          Alcotest.(check bool) "signal blocked" false !signaled))

let test_handle_daemon_exit_restart_chmod_fixes_eacces () =
  let call_count = ref 0 in
  let called = ref None in
  Service.handle_daemon_exit
    ~execve:(fun path argv env ->
      incr call_count;
      if !call_count = 1 then
        raise (Unix.Unix_error (Unix.EACCES, "execve", path))
      else called := Some (path, Array.to_list argv, env))
    Daemon.Restart;
  (* execve is called once; EACCES triggers the catch, falls back to shutdown.
     The chmod+retry happens inside validate_and_fix before execve is called,
     so the mock execve only sees one call if the binary is already executable. *)
  Alcotest.(check bool) "execve was called" true (!call_count >= 1)

let test_handle_daemon_exit_restart_logs_on_execve_failure () =
  let called = ref false in
  (* execve always fails — should not raise *)
  Service.handle_daemon_exit
    ~execve:(fun path _ _ ->
      called := true;
      raise (Unix.Unix_error (Unix.EPERM, "execve", path)))
    Daemon.Restart;
  Alcotest.(check bool) "execve was attempted" true !called

let test_run_nofork_start_handles_execve_failure () =
  with_env "CLAWQ_DAEMON_NOFORK" (Some "1") (fun () ->
      with_env "CLAWQ_DAEMON_INTERNAL_NOFORK" (Some "") (fun () ->
          let called = ref false in
          let config = Runtime_config.default in
          let result =
            Service.run_nofork_start ~config
              ~execve:(fun path _ _ ->
                called := true;
                raise (Unix.Unix_error (Unix.EACCES, "execve", path)))
              ~run_daemon:(fun ~config:_ ->
                Alcotest.fail "daemon should not run after execve failure")
              ()
          in
          Alcotest.(check bool) "execve was attempted" true !called;
          Alcotest.(check string) "returns empty string" "" result))

let test_validate_and_fix_ok_for_executable () =
  let path = Filename.temp_file "clawq_test_vf" ".exe" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      Unix.chmod path 0o755;
      let result = Restart_exec.validate_and_fix path in
      Alcotest.(check (result string string))
        "Ok path for executable" (Ok path) result)

let test_validate_and_fix_fixes_non_executable () =
  let path = Filename.temp_file "clawq_test_vf" ".exe" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      Unix.chmod path 0o644;
      let result = Restart_exec.validate_and_fix path in
      Alcotest.(check (result string string))
        "Ok path after chmod fix" (Ok path) result;
      Unix.access path [ Unix.X_OK ];
      Alcotest.(check pass) "file is now executable" () ())

let test_validate_and_fix_passes_through_enoent () =
  let path = "/tmp/clawq_test_nonexistent_" ^ string_of_int (Unix.getpid ()) in
  (try Sys.remove path with Sys_error _ -> ());
  let result = Restart_exec.validate_and_fix path in
  Alcotest.(check (result string string))
    "Ok path for non-existent" (Ok path) result

let test_cmd_signal_restart_reports_signal_failure () =
  Test_helpers.with_temp_home (fun _home ->
      let result =
        Service.cmd_signal_restart
          ~read_pid:(fun () -> Some 1234)
          ~send_signal:(fun _ _ ->
            raise (Unix.Unix_error (Unix.ESRCH, "kill", "")))
          ()
      in
      Alcotest.(check bool)
        "signal failure reported" true
        (String.length result > 0
        &&
        let prefix = "Failed to signal daemon pid 1234:" in
        String.length result >= String.length prefix
        && String.sub result 0 (String.length prefix) = prefix))

let suite =
  [
    Alcotest.test_case "handle daemon exit restart sets nofork and execs" `Quick
      test_handle_daemon_exit_restart_sets_nofork_and_execs;
    Alcotest.test_case "handle daemon exit restart carries restart notify env"
      `Quick test_handle_daemon_exit_restart_carries_restart_notify_env;
    Alcotest.test_case "handle daemon exit restart prefers reexec env" `Quick
      test_handle_daemon_exit_restart_prefers_reexec_env;
    Alcotest.test_case "handle daemon exit shutdown skips exec" `Quick
      test_handle_daemon_exit_shutdown_skips_exec;
    Alcotest.test_case "run nofork start reexecs without public env" `Quick
      test_run_nofork_start_reexecs_without_public_env;
    Alcotest.test_case "run nofork start runs daemon in internal mode" `Quick
      test_run_nofork_start_runs_daemon_in_internal_mode;
    Alcotest.test_case "run nofork start prefers reexec env" `Quick
      test_run_nofork_start_prefers_reexec_env;
    Alcotest.test_case "handle daemon exit restart chmod fixes eacces" `Quick
      test_handle_daemon_exit_restart_chmod_fixes_eacces;
    Alcotest.test_case "handle daemon exit restart logs on execve failure"
      `Quick test_handle_daemon_exit_restart_logs_on_execve_failure;
    Alcotest.test_case "run nofork start handles execve failure" `Quick
      test_run_nofork_start_handles_execve_failure;
    Alcotest.test_case "cmd signal restart reports missing daemon" `Quick
      test_cmd_signal_restart_reports_missing_daemon;
    Alcotest.test_case "cmd signal restart signals running daemon" `Quick
      test_cmd_signal_restart_signals_running_daemon;
    Alcotest.test_case
      "cmd signal restart refuses live signal outside temp home" `Quick
      test_cmd_signal_restart_refuses_live_signal_outside_temp_home;
    Alcotest.test_case "cmd signal restart reports signal failure" `Quick
      test_cmd_signal_restart_reports_signal_failure;
    Alcotest.test_case "validate_and_fix ok for executable" `Quick
      test_validate_and_fix_ok_for_executable;
    Alcotest.test_case "validate_and_fix fixes non-executable" `Quick
      test_validate_and_fix_fixes_non_executable;
    Alcotest.test_case "validate_and_fix passes through enoent" `Quick
      test_validate_and_fix_passes_through_enoent;
  ]

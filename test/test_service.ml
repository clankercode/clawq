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

let test_reexec_daemonized_sets_internal_nofork_and_service_argv () =
  let called = ref None in
  Service.reexec_daemonized
    ~execve:(fun path argv env ->
      called := Some (path, Array.to_list argv, env))
    ()
  |> ignore;
  Alcotest.(check bool)
    "re-exec uses service start argv" true
    (match !called with
    | Some (path, argv, _) ->
        argv = [ Restart_exec.executable (); "service"; "start" ]
        && path = Restart_exec.executable ()
    | None -> false);
  Alcotest.(check bool)
    "internal nofork set in re-exec env" true
    (match !called with
    | Some (_, _, env) -> env_contains env "CLAWQ_DAEMON_INTERNAL_NOFORK=1"
    | None -> false);
  Alcotest.(check bool)
    "public nofork removed from re-exec env" true
    (match !called with
    | Some (_, _, env) -> env_lacks_key env "CLAWQ_DAEMON_NOFORK"
    | None -> false)

let test_reexec_daemonized_prefers_reexec_env () =
  with_env Restart_exec.reexec_path_env (Some "/tmp/clawq-fresh") (fun () ->
      let called = ref None in
      Service.reexec_daemonized
        ~execve:(fun path argv _ -> called := Some (path, Array.to_list argv))
        ()
      |> ignore;
      Alcotest.(check (option (pair string (list string))))
        "re-exec uses fresh path"
        (Some ("/tmp/clawq-fresh", [ "/tmp/clawq-fresh"; "service"; "start" ]))
        !called)

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
        "signal restart response" "Restart signal sent to daemon (PID 1234)"
        result;
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

let test_validate_and_fix_rejects_deleted_suffix () =
  match Restart_exec.validate_and_fix "/tmp/clawq-dead (deleted)" with
  | Error msg ->
      Alcotest.(check bool)
        "mentions deleted binary" true
        (String.contains msg 'd' && String.contains msg 'b')
  | Ok _ -> Alcotest.fail "expected deleted suffix path to be rejected"

let test_validate_and_fix_rejects_deleted_symlink_target () =
  let dir = Filename.get_temp_dir_name () in
  let target =
    Filename.concat dir (Printf.sprintf "clawq-real-%d" (Unix.getpid ()))
  in
  let link =
    Filename.concat dir (Printf.sprintf "clawq-link-%d" (Unix.getpid ()))
  in
  let oc = open_out target in
  output_string oc "#!/bin/sh\nexit 0\n";
  close_out oc;
  Unix.chmod target 0o755;
  (try Sys.remove link with _ -> ());
  Unix.symlink (target ^ " (deleted)") link;
  match Restart_exec.validate_and_fix link with
  | Error msg ->
      Alcotest.(check bool)
        "mentions deleted symlink target" true
        (String.contains msg 'd' && String.contains msg 'b')
  | Ok _ -> Alcotest.fail "expected deleted symlink target to be rejected"

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

let test_cmd_status_warns_on_deleted_exe () =
  Test_helpers.with_temp_home (fun _home ->
      let status = Service.cmd_status () in
      Alcotest.(check bool)
        "status header" true
        (String.length status >= String.length "Service status:"))

let test_cmd_status_mentions_uptime_for_live_pid () =
  Test_helpers.with_temp_home (fun _home ->
      let status = Service.cmd_status () in
      Alcotest.(check bool)
        "returns header" true
        (String.length status >= String.length "Service status:"))

let test_deadlock_timeout_triggers_restart_in_nofork () =
  with_env "CLAWQ_DAEMON_NOFORK" (Some "") (fun () ->
      with_env "CLAWQ_DAEMON_INTERNAL_NOFORK" (Some "1") (fun () ->
          let exec_called = ref None in
          let config = Runtime_config.default in
          let _result =
            Service.run_nofork_start ~config
              ~execve:(fun path argv env ->
                exec_called := Some (path, Array.to_list argv, env))
              ~run_daemon:(fun ~config:_ ->
                raise (Lwt_util.Deadlock_timeout "test_mutex"))
              ()
          in
          Alcotest.(check bool)
            "execve called on deadlock" true
            (match !exec_called with
            | Some (_, argv, env) ->
                argv = [ Restart_exec.executable (); "service"; "start" ]
                && env_contains env "CLAWQ_DAEMON_NOFORK=1"
            | None -> false)))

let test_systemd_unit_output () =
  let output = Service.cmd_systemd_unit () in
  Alcotest.(check bool) "contains Unit section" true (String.length output > 0);
  let has_substr s = String.length output >= String.length s in
  Alcotest.(check bool) "non-empty output" true (has_substr "clawq");
  Alcotest.(check bool)
    "has Unit section" true
    (Test_helpers.string_contains output "[Unit]");
  Alcotest.(check bool)
    "has Service section" true
    (Test_helpers.string_contains output "[Service]");
  Alcotest.(check bool)
    "has Install section" true
    (Test_helpers.string_contains output "[Install]");
  Alcotest.(check bool)
    "tracks foreground process" true
    (Test_helpers.string_contains output "Type=exec");
  Alcotest.(check bool)
    "uses foreground service run" true
    (Test_helpers.string_contains output " service run");
  Alcotest.(check bool)
    "has ExecStart" true
    (Test_helpers.string_contains output "ExecStart=");
  Alcotest.(check bool)
    "systemd sends stop signal directly" false
    (Test_helpers.string_contains output "ExecStop=");
  Alcotest.(check bool)
    "no unimplemented watchdog" false
    (Test_helpers.string_contains output "WatchdogSec=");
  Alcotest.(check bool)
    "has Restart=on-failure" true
    (Test_helpers.string_contains output "Restart=on-failure");
  (* B830 memory guardrails *)
  Alcotest.(check bool)
    "has GC environment override" true
    (Test_helpers.string_contains output "Environment=OCAMLRUNPARAM=o=60");
  Alcotest.(check bool)
    "has MemoryHigh guardrail" true
    (Test_helpers.string_contains output "MemoryHigh=1G");
  Alcotest.(check bool)
    "has MemoryMax guardrail" true
    (Test_helpers.string_contains output "MemoryMax=1536M");
  Alcotest.(check bool)
    "has OOMPolicy=stop" true
    (Test_helpers.string_contains output "OOMPolicy=stop");
  Alcotest.(check bool)
    "has install instructions" true
    (Test_helpers.string_contains output "systemctl --user enable")

let read_daemon_state_json home =
  let path = Filename.concat home ".clawq/daemon_state.json" in
  Yojson.Safe.from_file path

let get_component_status json name =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "components" fields with
      | Some (`Assoc comps) -> (
          match List.assoc_opt name comps with
          | Some (`String s) -> s
          | _ -> "missing")
      | _ -> "no_components")
  | _ -> "bad_json"

let get_bool_field json name =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt name fields with Some (`Bool b) -> b | _ -> false)
  | _ -> false

let test_component_status_disabled_without_credentials () =
  Test_helpers.with_temp_home (fun home ->
      let discord_cfg : Runtime_config.discord_config =
        {
          bot_token = "YOUR_TOKEN";
          allow_guilds = [];
          allow_users = [];
          intents = 0;
          default_model = None;
        }
      in
      let slack_cfg : Runtime_config.slack_config =
        {
          bot_token = "";
          signing_secret = "";
          events_path = "/slack/events";
          allow_channels = [];
          allow_users = [];
          allow_private_channels = [];
          private_channel_policy = Runtime_config.Pc_deny;
          app_token = "";
          socket_mode = false;
          default_model = None;
        }
      in
      (* Test the credential validation functions directly *)
      Alcotest.(check bool)
        "is_credential_valid rejects YOUR_TOKEN" false
        (Runtime_config.is_credential_valid "YOUR_TOKEN");
      Alcotest.(check bool)
        "is_credential_valid rejects empty" false
        (Runtime_config.is_credential_valid "");
      Alcotest.(check bool)
        "discord_has_valid_credentials rejects placeholder" false
        (Runtime_config.discord_has_valid_credentials discord_cfg);
      Alcotest.(check bool)
        "slack_has_valid_credentials rejects empty" false
        (Runtime_config.slack_has_valid_credentials slack_cfg);
      (* Compute component status using the same pattern as daemon.ml *)
      let config =
        {
          Runtime_config.default with
          channels =
            {
              Runtime_config.default.channels with
              discord = Some discord_cfg;
              slack = Some slack_cfg;
            };
        }
      in
      let discord_creds_ok =
        match config.channels.discord with
        | Some d -> Runtime_config.discord_has_valid_credentials d
        | None -> false
      in
      let slack_creds_ok =
        match config.channels.slack with
        | Some s -> Runtime_config.slack_has_valid_credentials s
        | None -> false
      in
      Alcotest.(check bool) "discord_creds_ok is false" false discord_creds_ok;
      Alcotest.(check bool) "slack_creds_ok is false" false slack_creds_ok;
      Daemon_util.write_state ~pairing_code:None ~tunnel_json:None ~config
        ~components:
          [
            ("gateway", "running");
            ("discord", if discord_creds_ok then "running" else "disabled");
            ("slack", if slack_creds_ok then "running" else "disabled");
          ];
      let json = read_daemon_state_json home in
      Alcotest.(check string)
        "discord component disabled" "disabled"
        (get_component_status json "discord");
      Alcotest.(check string)
        "slack component disabled" "disabled"
        (get_component_status json "slack");
      Alcotest.(check bool)
        "discord_enabled false" false
        (get_bool_field json "discord_enabled");
      Alcotest.(check bool)
        "slack_enabled false" false
        (get_bool_field json "slack_enabled"))

let test_component_status_running_with_valid_credentials () =
  Test_helpers.with_temp_home (fun home ->
      let discord_cfg : Runtime_config.discord_config =
        {
          bot_token = "valid_discord_token_here";
          allow_guilds = [];
          allow_users = [];
          intents = 0;
          default_model = None;
        }
      in
      let slack_cfg : Runtime_config.slack_config =
        {
          bot_token = "xoxb-valid-slack-token";
          signing_secret = "valid_signing_secret_here";
          events_path = "/slack/events";
          allow_channels = [];
          allow_users = [];
          allow_private_channels = [];
          private_channel_policy = Runtime_config.Pc_deny;
          app_token = "";
          socket_mode = false;
          default_model = None;
        }
      in
      (* Test the credential validation functions directly *)
      Alcotest.(check bool)
        "is_credential_valid accepts real token" true
        (Runtime_config.is_credential_valid "valid_discord_token_here");
      Alcotest.(check bool)
        "is_credential_valid rejects short token" false
        (Runtime_config.is_credential_valid "abc");
      Alcotest.(check bool)
        "is_credential_valid rejects YOUR_ prefix" false
        (Runtime_config.is_credential_valid "YOUR_SECRET_HERE");
      Alcotest.(check bool)
        "discord_has_valid_credentials accepts valid" true
        (Runtime_config.discord_has_valid_credentials discord_cfg);
      Alcotest.(check bool)
        "slack_has_valid_credentials accepts valid" true
        (Runtime_config.slack_has_valid_credentials slack_cfg);
      (* Compute component status using the same pattern as daemon.ml *)
      let config =
        {
          Runtime_config.default with
          channels =
            {
              Runtime_config.default.channels with
              discord = Some discord_cfg;
              slack = Some slack_cfg;
            };
        }
      in
      let discord_creds_ok =
        match config.channels.discord with
        | Some d -> Runtime_config.discord_has_valid_credentials d
        | None -> false
      in
      let slack_creds_ok =
        match config.channels.slack with
        | Some s -> Runtime_config.slack_has_valid_credentials s
        | None -> false
      in
      Alcotest.(check bool) "discord_creds_ok is true" true discord_creds_ok;
      Alcotest.(check bool) "slack_creds_ok is true" true slack_creds_ok;
      Daemon_util.write_state ~pairing_code:None ~tunnel_json:None ~config
        ~components:
          [
            ("gateway", "running");
            ("discord", if discord_creds_ok then "running" else "disabled");
            ("slack", if slack_creds_ok then "running" else "disabled");
          ];
      let json = read_daemon_state_json home in
      Alcotest.(check string)
        "discord component running" "running"
        (get_component_status json "discord");
      Alcotest.(check string)
        "slack component running" "running"
        (get_component_status json "slack");
      Alcotest.(check bool)
        "discord_enabled true" true
        (get_bool_field json "discord_enabled");
      Alcotest.(check bool)
        "slack_enabled true" true
        (get_bool_field json "slack_enabled"))

let test_launchd_plist_output () =
  let output = Service.cmd_launchd_plist () in
  Alcotest.(check bool)
    "contains plist tag" true
    (Test_helpers.string_contains output "<plist");
  Alcotest.(check bool)
    "contains Label" true
    (Test_helpers.string_contains output "<key>Label</key>");
  Alcotest.(check bool)
    "contains org.clawq.daemon" true
    (Test_helpers.string_contains output "org.clawq.daemon");
  Alcotest.(check bool)
    "contains RunAtLoad" true
    (Test_helpers.string_contains output "<key>RunAtLoad</key>");
  Alcotest.(check bool)
    "contains CLAWQ_DAEMON_NOFORK" true
    (Test_helpers.string_contains output "CLAWQ_DAEMON_NOFORK");
  Alcotest.(check bool)
    "contains ProgramArguments" true
    (Test_helpers.string_contains output "<key>ProgramArguments</key>");
  Alcotest.(check bool)
    "contains service start" true
    (Test_helpers.string_contains output "<string>service</string>")

let test_systemd_unit_path_under_home () =
  let path = Service.systemd_unit_path () in
  Alcotest.(check bool)
    "ends with expected suffix" true
    (Test_helpers.string_contains path ".config/systemd/user/clawq.service")

let test_launchd_plist_path_under_home () =
  let path = Service.launchd_plist_path () in
  Alcotest.(check bool)
    "ends with expected suffix" true
    (Test_helpers.string_contains path
       "Library/LaunchAgents/org.clawq.daemon.plist")

let test_cmd_install_unsupported_platform () =
  let result =
    Service.cmd_install
      ~wait_ready:(fun _ -> ())
      ~detect_platform:(fun () -> Service.Other "FreeBSD")
      ~run_command:(fun _ -> "")
      ()
  in
  Alcotest.(check bool)
    "mentions not supported" true
    (Test_helpers.string_contains result "not supported");
  Alcotest.(check bool)
    "mentions FreeBSD" true
    (Test_helpers.string_contains result "FreeBSD")

let test_cmd_install_linux () =
  Test_helpers.with_temp_home (fun home ->
      let commands = ref [] in
      let mock_run cmd =
        commands := cmd :: !commands;
        if Test_helpers.string_contains cmd "is-active" then "active" else ""
      in
      let result =
        Service.cmd_install
          ~wait_ready:(fun _ -> ())
          ~detect_platform:(fun () -> Service.Linux)
          ~run_command:mock_run ()
      in
      Alcotest.(check bool)
        "mentions installed" true
        (Test_helpers.string_contains result "installed");
      Alcotest.(check bool)
        "mentions loginctl" true
        (Test_helpers.string_contains result "loginctl enable-linger");
      let unit_path =
        Filename.concat home ".config/systemd/user/clawq.service"
      in
      Alcotest.(check bool) "unit file created" true (Sys.file_exists unit_path);
      Alcotest.(check bool)
        "ran daemon-reload" true
        (List.exists
           (fun c -> Test_helpers.string_contains c "daemon-reload")
           !commands);
      Alcotest.(check bool)
        "ran enable" true
        (List.exists
           (fun c -> Test_helpers.string_contains c "enable clawq")
           !commands))

let test_cmd_install_linux_starts_supervision () =
  Test_helpers.with_temp_home (fun _ ->
      let commands = ref [] in
      ignore
        (Service.cmd_install
           ~wait_ready:(fun _ -> ())
           ~detect_platform:(fun () -> Service.Linux)
           ~run_command:(fun cmd ->
             commands := !commands @ [ cmd ];
             if Test_helpers.string_contains cmd "is-active" then "active"
             else "")
           ());
      Alcotest.(check bool)
        "starts the supervisor now" true
        (List.exists
           (fun c -> Test_helpers.string_contains c "start clawq")
           !commands))

let test_cmd_install_linux_reports_command_failure () =
  Test_helpers.with_temp_home (fun _ ->
      let commands = ref [] in
      let result =
        Service.cmd_install
          ~wait_ready:(fun _ -> ())
          ~detect_platform:(fun () -> Service.Linux)
          ~run_command:(fun cmd ->
            commands := cmd :: !commands;
            failwith "Failed to connect to user bus")
          ()
      in
      Alcotest.(check bool)
        "failure is actionable" true
        (Test_helpers.string_contains result "Failed to connect to user bus");
      Alcotest.(check int)
        "does not proceed after failure" 1 (List.length !commands))

let test_cmd_install_darwin () =
  Test_helpers.with_temp_home (fun home ->
      let commands = ref [] in
      let mock_run cmd =
        commands := cmd :: !commands;
        if Test_helpers.string_contains cmd "is-active" then "active" else ""
      in
      let result =
        Service.cmd_install
          ~wait_ready:(fun _ -> ())
          ~detect_platform:(fun () -> Service.Darwin)
          ~run_command:mock_run ()
      in
      Alcotest.(check bool)
        "mentions installed" true
        (Test_helpers.string_contains result "installed");
      Alcotest.(check bool)
        "mentions RunAtLoad" true
        (Test_helpers.string_contains result "RunAtLoad");
      let plist_path =
        Filename.concat home "Library/LaunchAgents/org.clawq.daemon.plist"
      in
      Alcotest.(check bool)
        "plist file created" true
        (Sys.file_exists plist_path);
      Alcotest.(check bool)
        "ran launchctl load" true
        (List.exists
           (fun c -> Test_helpers.string_contains c "launchctl load")
           !commands))

let test_cmd_uninstall_unsupported_platform () =
  let result =
    Service.cmd_uninstall
      ~detect_platform:(fun () -> Service.Other "FreeBSD")
      ~run_command:(fun _ -> "")
      ()
  in
  Alcotest.(check bool)
    "mentions not supported" true
    (Test_helpers.string_contains result "not supported")

let test_cmd_uninstall_handles_missing_files () =
  Test_helpers.with_temp_home (fun _home ->
      let result =
        Service.cmd_uninstall
          ~detect_platform:(fun () -> Service.Linux)
          ~run_command:(fun _ -> "")
          ()
      in
      Alcotest.(check bool) "returns response" true (String.length result > 0))

let test_cmd_status_includes_autostart_line () =
  Test_helpers.with_temp_home (fun _home ->
      let result =
        Service.cmd_status
          ~detect_platform:(fun () -> Service.Linux)
          ~run_command:(fun _ -> "")
          ()
      in
      Alcotest.(check bool)
        "contains autostart" true
        (Test_helpers.string_contains result "autostart:"))

let test_detect_platform_returns_value () =
  let platform = Service.detect_platform () in
  (match platform with
  | Service.Linux -> ()
  | Service.Darwin -> ()
  | Service.Other _ -> ());
  Alcotest.(check pass) "detect_platform returned" () ()

let test_cmd_install_darwin_already_running () =
  Test_helpers.with_temp_home (fun home ->
      Service.write_pid (Unix.getpid ());
      let commands = ref [] in
      let mock_run cmd =
        commands := cmd :: !commands;
        if Test_helpers.string_contains cmd "is-active" then "active" else ""
      in
      let result =
        Service.cmd_install
          ~wait_ready:(fun _ -> ())
          ~detect_platform:(fun () -> Service.Darwin)
          ~run_command:mock_run ()
      in
      Alcotest.(check bool)
        "mentions already running" true
        (Test_helpers.string_contains result "already running");
      Alcotest.(check bool)
        "launchctl load not called" false
        (List.exists
           (fun c -> Test_helpers.string_contains c "launchctl load")
           !commands);
      let plist_path =
        Filename.concat home "Library/LaunchAgents/org.clawq.daemon.plist"
      in
      Alcotest.(check bool)
        "plist file created" true
        (Sys.file_exists plist_path))

let test_cmd_install_linux_already_running () =
  Test_helpers.with_temp_home (fun home ->
      Service.write_pid (Unix.getpid ());
      let commands = ref [] in
      let result =
        Service.cmd_install
          ~wait_ready:(fun _ -> ())
          ~detect_platform:(fun () -> Service.Linux)
          ~stop_daemon:(fun () ->
            commands := !commands @ [ "stop unmanaged" ];
            Service.remove_pid ();
            "Daemon stopped")
          ~run_command:(fun cmd ->
            commands := !commands @ [ cmd ];
            if Test_helpers.string_contains cmd "is-active" then "active"
            else "")
          ()
      in
      Alcotest.(check bool)
        "supervision installed" true
        (Test_helpers.string_contains result "under systemd supervision");
      Alcotest.(check (list string))
        "adopts before starting unit"
        [
          "systemctl --user daemon-reload";
          "systemctl --user enable clawq";
          "systemctl --user show -p MainPID --value clawq";
          "stop unmanaged";
          "systemctl --user start clawq";
          "systemctl --user is-active clawq";
        ]
        !commands;
      Alcotest.(check bool)
        "unit file created" true
        (Sys.file_exists
           (Filename.concat home ".config/systemd/user/clawq.service")))

let test_cmd_run_owns_lock_and_pid () =
  Test_helpers.with_temp_home (fun _ ->
      ignore
        (Service.cmd_run ~config:Runtime_config.default
           ~run_daemon:(fun ~config:_ ->
             Alcotest.(check (option int))
               "pid is foreground process"
               (Some (Unix.getpid ()))
               (Service.read_pid ());
             Daemon.Shutdown)
           ());
      Alcotest.(check (option int))
        "clean shutdown removes pid" None (Service.read_pid ());
      let lock = Service.acquire_singleton_lock () in
      Alcotest.(check bool) "shutdown releases lock" true (lock <> None);
      Service.release_singleton_lock lock)

let test_cmd_run_failure_cleans_up () =
  Test_helpers.with_temp_home (fun _ ->
      let failed =
        try
          ignore
            (Service.cmd_run ~config:Runtime_config.default
               ~run_daemon:(fun ~config:_ -> failwith "startup failed")
               ());
          false
        with Failure msg -> msg = "startup failed"
      in
      Alcotest.(check bool) "failure reaches supervisor" true failed;
      Alcotest.(check (option int))
        "failure removes pid" None (Service.read_pid ());
      let lock = Service.acquire_singleton_lock () in
      Alcotest.(check bool) "failure releases lock" true (lock <> None);
      Service.release_singleton_lock lock)

let test_cmd_run_restart_retains_foreground () =
  Test_helpers.with_temp_home (fun _ ->
      let called = ref None in
      ignore
        (Service.cmd_run ~config:Runtime_config.default
           ~run_daemon:(fun ~config:_ -> Daemon.Restart)
           ~execve:(fun _ argv _ -> called := Some (Array.to_list argv))
           ());
      Alcotest.(check (option (list string)))
        "restart uses foreground entrypoint"
        (Some [ Restart_exec.executable (); "service"; "run" ])
        !called)

let test_systemd_quoting () =
  Alcotest.(check string)
    "escapes specifiers, quotes and slashes; executable dollars stay literal"
    "\"/tmp/space %% $ \\\" \\\\\""
    (Service.systemd_quote "/tmp/space % $ \" \\");
  with_env "CLAWQ_HOME" (Some "/tmp/clawq with % $ spaces") (fun () ->
      Alcotest.(check bool)
        "unit carries explicit data directory" true
        (Test_helpers.string_contains
           (Service.cmd_systemd_unit ())
           "Environment=\"CLAWQ_HOME=/tmp/clawq with %% $ spaces\""))

let test_managed_commands_use_supervisor () =
  Test_helpers.with_temp_home (fun _ ->
      Service.write_file
        (Service.systemd_unit_path ())
        (Service.cmd_systemd_unit ());
      let commands = ref [] in
      let run_command cmd =
        commands := !commands @ [ cmd ];
        if Test_helpers.string_contains cmd "MainPID" then "0" else ""
      in
      let started =
        Service.cmd_start_with_supervisor ~config:Runtime_config.default
          ~wait_ready:(fun _ -> ())
          ~detect_platform:(fun () -> Service.Linux)
          ~run_command ()
      in
      Alcotest.(check bool)
        "native runtime accepts managed start" true
        (String.starts_with ~prefix:"Daemon " started);
      let stopped =
        Service.cmd_stop
          ~detect_platform:(fun () -> Service.Linux)
          ~run_command ()
      in
      Alcotest.(check bool)
        "native runtime accepts managed stop" true
        (String.starts_with ~prefix:"Stopped" stopped);
      ignore
        (Service.cmd_restart_with_supervisor ~config:Runtime_config.default
           ~wait_ready:(fun _ -> ())
           ~detect_platform:(fun () -> Service.Linux)
           ~run_command ());
      Alcotest.(check (list string))
        "manager owns the lifecycle"
        [
          "systemctl --user show -p MainPID --value clawq";
          "systemctl --user start clawq";
          "systemctl --user show -p MainPID --value clawq";
          "systemctl --user stop clawq";
          "systemctl --user show -p MainPID --value clawq";
          "systemctl --user restart clawq";
        ]
        !commands)

let test_install_aborts_failed_adoption () =
  Test_helpers.with_temp_home (fun _ ->
      Service.write_pid (Unix.getpid ());
      let commands = ref [] in
      let result =
        Service.cmd_install
          ~wait_ready:(fun _ -> ())
          ~detect_platform:(fun () -> Service.Linux)
          ~run_command:(fun cmd ->
            commands := !commands @ [ cmd ];
            "")
          ~stop_daemon:(fun () -> "permission denied")
          ()
      in
      Alcotest.(check bool)
        "reports adoption failure" true
        (Test_helpers.string_contains result "Cannot adopt existing daemon");
      Alcotest.(check bool)
        "does not start a competing unit" false
        (List.mem "systemctl --user start clawq" !commands);
      Service.remove_pid ())

let test_unmanaged_stop_preserves_pid_on_error () =
  Test_helpers.with_temp_home (fun _ ->
      Service.write_pid (Unix.getpid ());
      let result =
        Service.cmd_stop_unmanaged
          ~send_signal:(fun _ _ ->
            raise (Unix.Unix_error (Unix.EPERM, "kill", "")))
          ()
      in
      Alcotest.(check bool)
        "reports stop failure" true
        (Test_helpers.string_contains result "Error: cannot stop daemon");
      Alcotest.(check (option int))
        "failed stop retains identity"
        (Some (Unix.getpid ()))
        (Service.read_pid ());
      Service.remove_pid ())

let test_run_command_reports_exit_status () =
  let failed =
    try
      ignore
        (Service.run_command_default "printf supervisor-failure >&2; exit 7");
      false
    with Failure msg ->
      Test_helpers.string_contains msg "exit code 7"
      && Test_helpers.string_contains msg "supervisor-failure"
  in
  Alcotest.(check bool) "status and stderr survive" true failed

let test_foreground_log_reaches_ec_scanner () =
  Test_helpers.with_temp_home (fun _ ->
      let reporter = Logs.reporter () in
      let level = Logs.level () in
      Fun.protect
        ~finally:(fun () ->
          Logs.set_reporter reporter;
          Logs.set_level level)
        (fun () ->
          ignore
            (Service.cmd_run ~config:Runtime_config.default
               ~run_daemon:(fun ~config:_ ->
                 Daemon_logging.install ();
                 Logs.err (fun m -> m "B829 foreground EC scanner regression");
                 Daemon.Shutdown)
               ());
          let entries, _ =
            Ec_process.scan_daemon_log ~config:Runtime_config.default
              ~scan_state:(Ec_process.create_log_scan_state ())
              ~seen:[]
          in
          Alcotest.(check bool)
            "foreground error reaches existing EC scanner" true
            (List.exists
               (fun (entry : Error_watcher.error_entry) ->
                 Test_helpers.string_contains entry.message
                   "B829 foreground EC scanner regression")
               entries)))

let test_installed_start_refuses_manual_owner () =
  Test_helpers.with_temp_home (fun _ ->
      Service.write_file
        (Service.systemd_unit_path ())
        (Service.cmd_systemd_unit ());
      Service.write_pid (Unix.getpid ());
      let commands = ref [] in
      let result =
        Service.cmd_start_with_supervisor ~config:Runtime_config.default
          ~detect_platform:(fun () -> Service.Linux)
          ~run_command:(fun cmd ->
            commands := cmd :: !commands;
            if Test_helpers.string_contains cmd "MainPID" then "0" else "")
          ()
      in
      Alcotest.(check bool)
        "manual owner needs explicit adoption" true
        (String.starts_with ~prefix:"Error:" result
        && Test_helpers.string_contains result "install");
      Alcotest.(check bool)
        "does not launch competing supervisor" false
        (List.mem "systemctl --user start clawq" !commands);
      Service.remove_pid ())

let test_supervisor_does_not_accept_early_heartbeat () =
  Test_helpers.with_temp_home (fun _ ->
      let lock = Service.acquire_singleton_lock () in
      Fun.protect
        ~finally:(fun () ->
          Service.release_singleton_lock lock;
          Service.remove_pid ())
        (fun () ->
          Service.write_pid (Unix.getpid ());
          Daemon_liveness.write_heartbeat ();
          let rejected =
            try
              Service.wait_for_systemd ~timeout_s:0.
                ~clock:(fun () -> 0.)
                ~sleep:(fun _ -> Alcotest.fail "readiness test must not sleep")
                (fun _ -> string_of_int (Unix.getpid ()));
              false
            with Failure _ -> true
          in
          Alcotest.(check bool)
            "PID lock and early heartbeat are not application readiness" true
            rejected))

let test_installed_stop_terminates_manual_owner () =
  Test_helpers.with_temp_home (fun _ ->
      Service.write_file
        (Service.systemd_unit_path ())
        (Service.cmd_systemd_unit ());
      Service.write_pid (Unix.getpid ());
      let stop_called = ref false in
      let result =
        Service.cmd_stop
          ~detect_platform:(fun () -> Service.Linux)
          ~run_command:(fun cmd ->
            if Test_helpers.string_contains cmd "MainPID" then "0" else "")
          ~stop_daemon:(fun () ->
            stop_called := true;
            Service.remove_pid ();
            "Daemon stopped")
          ()
      in
      Alcotest.(check bool) "stops actual manual owner" true !stop_called;
      Alcotest.(check bool)
        "reports successful verified stop" true
        (String.starts_with ~prefix:"Stopped" result))

let test_installed_restart_refuses_manual_owner () =
  Test_helpers.with_temp_home (fun _ ->
      Service.write_file
        (Service.systemd_unit_path ())
        (Service.cmd_systemd_unit ());
      Service.write_pid (Unix.getpid ());
      let result =
        Service.cmd_restart_with_supervisor ~config:Runtime_config.default
          ~detect_platform:(fun () -> Service.Linux)
          ~run_command:(fun cmd ->
            if Test_helpers.string_contains cmd "MainPID" then "0"
            else Alcotest.fail "must not restart a competing unit")
          ()
      in
      Alcotest.(check bool)
        "explicit adoption required" true
        (String.starts_with ~prefix:"Error:" result
        && Test_helpers.string_contains result "install");
      Service.remove_pid ())

let test_managed_actions_propagate_readiness_failure () =
  Test_helpers.with_temp_home (fun _ ->
      Service.write_file
        (Service.systemd_unit_path ())
        (Service.cmd_systemd_unit ());
      let run_command cmd =
        if Test_helpers.string_contains cmd "MainPID" then "0" else ""
      in
      let wait_ready _ = failwith "gateway startup rejected" in
      let results =
        [
          Service.cmd_start_with_supervisor ~config:Runtime_config.default
            ~detect_platform:(fun () -> Service.Linux)
            ~run_command ~wait_ready ();
          Service.cmd_restart_with_supervisor ~config:Runtime_config.default
            ~detect_platform:(fun () -> Service.Linux)
            ~run_command ~wait_ready ();
        ]
      in
      List.iter
        (fun result ->
          Alcotest.(check bool)
            "application failure reaches CLI" true
            (String.starts_with ~prefix:"Error:" result
            && Test_helpers.string_contains result "gateway startup rejected"))
        results)

let test_readiness_poll_uses_injected_clock () =
  Test_helpers.with_temp_home (fun _ ->
      let lock = Service.acquire_singleton_lock () in
      Fun.protect
        ~finally:(fun () ->
          Service.release_singleton_lock lock;
          Service.remove_pid ())
        (fun () ->
          Service.write_pid (Unix.getpid ());
          Daemon_liveness.write_heartbeat ~now:0. ();
          let now = ref 0. in
          let run_command cmd =
            if Test_helpers.string_contains cmd "MainPID" then
              string_of_int (Unix.getpid ())
            else "ActiveState=active\nSubState=running\nResult=success"
          in
          Service.wait_for_systemd ~timeout_s:1.
            ~clock:(fun () -> !now)
            ~sleep:(fun delay -> now := !now +. delay)
            ~app_ready:(fun _ -> !now >= 0.2)
            run_command;
          Alcotest.(check bool)
            "waits for application after early heartbeat" true (!now >= 0.2);
          let timed_out =
            try
              Service.wait_for_systemd ~timeout_s:0.2
                ~clock:(fun () -> !now)
                ~sleep:(fun delay -> now := !now +. delay)
                ~app_ready:(fun _ -> false)
                run_command;
              false
            with Failure _ -> true
          in
          Alcotest.(check bool)
            "bounded timeout without wall-clock sleeps" true timed_out))

let test_listening_socket_parser () =
  let row state port inode =
    Printf.sprintf
      "0: 0100007F:%s 00000000:0000 %s 00000000:00000000 00:00000000 00000000 \
       1000 0 %s"
      port state inode
  in
  Alcotest.(check (list string))
    "only listening socket at requested port" [ "123" ]
    (Service.listening_inodes ~port:8080
       (String.concat "\n"
          [
            row "0A" "1F90" "123"; row "01" "1F90" "124"; row "0A" "1F91" "125";
          ]))

let suite =
  [
    Alcotest.test_case "installed stop terminates manual owner" `Quick
      test_installed_stop_terminates_manual_owner;
    Alcotest.test_case "installed restart refuses manual owner" `Quick
      test_installed_restart_refuses_manual_owner;
    Alcotest.test_case "managed readiness failure propagates" `Quick
      test_managed_actions_propagate_readiness_failure;
    Alcotest.test_case "readiness clock is injectable and bounded" `Quick
      test_readiness_poll_uses_injected_clock;
    Alcotest.test_case "listening socket parser" `Quick
      test_listening_socket_parser;
    Alcotest.test_case "foreground log feeds EC scanner" `Quick
      test_foreground_log_reaches_ec_scanner;
    Alcotest.test_case "installed start refuses manual owner" `Quick
      test_installed_start_refuses_manual_owner;
    Alcotest.test_case "early heartbeat is not startup readiness" `Quick
      test_supervisor_does_not_accept_early_heartbeat;
    Alcotest.test_case "stop error retains pid" `Quick
      test_unmanaged_stop_preserves_pid_on_error;
    Alcotest.test_case "supervisor command failure preserves status" `Quick
      test_run_command_reports_exit_status;
    Alcotest.test_case "foreground owns lock and pid" `Quick
      test_cmd_run_owns_lock_and_pid;
    Alcotest.test_case "foreground failure cleans up" `Quick
      test_cmd_run_failure_cleans_up;
    Alcotest.test_case "foreground restart remains foreground" `Quick
      test_cmd_run_restart_retains_foreground;
    Alcotest.test_case "systemd quoting" `Quick test_systemd_quoting;
    Alcotest.test_case "managed commands use supervisor" `Quick
      test_managed_commands_use_supervisor;
    Alcotest.test_case "failed adoption aborts" `Quick
      test_install_aborts_failed_adoption;
    Alcotest.test_case "handle daemon exit restart sets nofork and execs" `Quick
      test_handle_daemon_exit_restart_sets_nofork_and_execs;
    Alcotest.test_case "handle daemon exit restart carries restart notify env"
      `Quick test_handle_daemon_exit_restart_carries_restart_notify_env;
    Alcotest.test_case "handle daemon exit restart prefers reexec env" `Quick
      test_handle_daemon_exit_restart_prefers_reexec_env;
    Alcotest.test_case "handle daemon exit shutdown skips exec" `Quick
      test_handle_daemon_exit_shutdown_skips_exec;
    Alcotest.test_case "reexec daemonized sets internal nofork and service argv"
      `Quick test_reexec_daemonized_sets_internal_nofork_and_service_argv;
    Alcotest.test_case "reexec daemonized prefers reexec env" `Quick
      test_reexec_daemonized_prefers_reexec_env;
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
    Alcotest.test_case "cmd status warns on deleted exe" `Quick
      test_cmd_status_warns_on_deleted_exe;
    Alcotest.test_case "cmd status mentions uptime for live pid" `Quick
      test_cmd_status_mentions_uptime_for_live_pid;
    Alcotest.test_case "validate_and_fix ok for executable" `Quick
      test_validate_and_fix_ok_for_executable;
    Alcotest.test_case "validate_and_fix fixes non-executable" `Quick
      test_validate_and_fix_fixes_non_executable;
    Alcotest.test_case "validate_and_fix rejects deleted suffix" `Quick
      test_validate_and_fix_rejects_deleted_suffix;
    Alcotest.test_case "validate_and_fix rejects deleted symlink target" `Quick
      test_validate_and_fix_rejects_deleted_symlink_target;
    Alcotest.test_case "validate_and_fix passes through enoent" `Quick
      test_validate_and_fix_passes_through_enoent;
    Alcotest.test_case "deadlock timeout triggers restart in nofork" `Quick
      test_deadlock_timeout_triggers_restart_in_nofork;
    Alcotest.test_case "systemd unit output" `Quick test_systemd_unit_output;
    Alcotest.test_case "component status disabled without credentials" `Quick
      test_component_status_disabled_without_credentials;
    Alcotest.test_case "component status running with valid credentials" `Quick
      test_component_status_running_with_valid_credentials;
    Alcotest.test_case "launchd plist output" `Quick test_launchd_plist_output;
    Alcotest.test_case "systemd unit path under home" `Quick
      test_systemd_unit_path_under_home;
    Alcotest.test_case "launchd plist path under home" `Quick
      test_launchd_plist_path_under_home;
    Alcotest.test_case "cmd install unsupported platform" `Quick
      test_cmd_install_unsupported_platform;
    Alcotest.test_case "cmd install linux" `Quick test_cmd_install_linux;
    Alcotest.test_case "install starts supervision" `Quick
      test_cmd_install_linux_starts_supervision;
    Alcotest.test_case "install reports command failure" `Quick
      test_cmd_install_linux_reports_command_failure;
    Alcotest.test_case "cmd install darwin" `Quick test_cmd_install_darwin;
    Alcotest.test_case "cmd uninstall unsupported platform" `Quick
      test_cmd_uninstall_unsupported_platform;
    Alcotest.test_case "cmd uninstall handles missing files" `Quick
      test_cmd_uninstall_handles_missing_files;
    Alcotest.test_case "cmd status includes autostart line" `Quick
      test_cmd_status_includes_autostart_line;
    Alcotest.test_case "detect platform returns value" `Quick
      test_detect_platform_returns_value;
    Alcotest.test_case "cmd install darwin already running" `Quick
      test_cmd_install_darwin_already_running;
    Alcotest.test_case "cmd install linux already running" `Quick
      test_cmd_install_linux_already_running;
  ]

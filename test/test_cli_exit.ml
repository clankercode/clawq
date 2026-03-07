let test_service_signal_restart_missing_daemon_exits_nonzero () =
  Alcotest.(check bool)
    "missing daemon is treated as error" true
    (Cli_exit.should_error ~name:"service" ~args:[ "signal-restart" ]
       ~result:"Daemon is not running")

let test_service_signal_restart_signal_failure_exits_nonzero () =
  Alcotest.(check bool)
    "signal failure is treated as error" true
    (Cli_exit.should_error ~name:"service" ~args:[ "signal-restart" ]
       ~result:"Failed to signal daemon pid 1234: No such process")

let test_service_signal_restart_success_stays_zero () =
  Alcotest.(check bool)
    "successful signal restart stays ok" false
    (Cli_exit.should_error ~name:"service" ~args:[ "signal-restart" ]
       ~result:"Restart signal sent to daemon (PID 1234)")

let suite =
  [
    Alcotest.test_case "service signal restart missing daemon exits nonzero"
      `Quick test_service_signal_restart_missing_daemon_exits_nonzero;
    Alcotest.test_case "service signal restart signal failure exits nonzero"
      `Quick test_service_signal_restart_signal_failure_exits_nonzero;
    Alcotest.test_case "service signal restart success stays zero" `Quick
      test_service_signal_restart_success_stays_zero;
  ]

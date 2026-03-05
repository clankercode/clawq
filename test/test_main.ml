let () =
  Alcotest.run "clawq"
    [
      ("clawq_core", Test_clawq_core.suite);
      ("command_bridge", Test_command_bridge.suite);
      ("phase3", Test_phase3.suite);
    ]

let () =
  Alcotest.run "clawq"
    [
      ("clawq_core", Test_clawq_core.suite);
      ("command_bridge", Test_command_bridge.suite);
      ("phase3", Test_phase3.suite);
      ("scheduler", Test_scheduler.suite);
      ("migrate", Test_migrate.suite);
      ("audit", Test_audit.suite);
      ("skills", Test_skills.suite);
      ("memory_search", Test_memory_search.suite);
      ("streaming", Test_streaming.suite);
    ]

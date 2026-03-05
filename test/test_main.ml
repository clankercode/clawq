let () =
  Alcotest.run "clawq"
    ([
       ("clawq_core", Test_clawq_core.suite);
       ("command_bridge", Test_command_bridge.suite);
       ("phase3", Test_phase3.suite);
       ("scheduler", Test_scheduler.suite);
       ("migrate", Test_migrate.suite);
       ("audit", Test_audit.suite);
       ("skills", Test_skills.suite);
       ("memory_search", Test_memory_search.suite);
       ("streaming", Test_streaming.suite);
       ("http_server", Test_http_server.suite);
       ("config_loader", Test_config_loader.suite);
       ("prompt_builder", Test_prompt_builder.suite);
       ("vector", Test_vector.suite);
       ("secret_store", Test_secret_store.suite);
       ("tools_security", Test_tools_security.suite);
       ("mcp", Test_mcp.suite);
       ("resilience", Test_resilience.suite);
       ("perf", Test_perf.suite);
       ("memory_retention", Test_memory_retention.suite);
       ("landlock", Test_landlock.suite);
       ("rate_limiter", Test_rate_limiter.suite);
       ("slack", Test_slack.suite);
       ("discord", Test_discord.suite);
       ("discord_gateway", Test_discord_gateway.suite);
       ("slack_socket", Test_slack_socket.suite);
       ("slash_commands", Test_slash_commands.suite);
       ("provider", Test_provider.suite);
       ("totp", Test_totp.suite);
       ("cost_tracker", Test_cost_tracker.suite);
       ("agent_router", Test_agent_router.suite);
     ]
    @ Test_github.suites)

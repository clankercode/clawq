let real_config_path =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat (Filename.concat home ".clawq") "config.json"

(* Captured before any tests run — module-level bindings evaluate top-to-bottom
   before Alcotest.run is called. *)
let real_config_mtime =
  try
    let st = Unix.stat real_config_path in
    Some st.Unix.st_mtime
  with _ -> None

let check_config_not_modified () =
  let current_mtime =
    try
      let st = Unix.stat real_config_path in
      Some st.Unix.st_mtime
    with _ -> None
  in
  if real_config_mtime <> current_mtime then
    Alcotest.failf
      "A test modified the real ~/.clawq/config.json! mtime before=%s \
       after=%s. Wrap the offending test in with_temp_home."
      (match real_config_mtime with
      | Some t -> string_of_float t
      | None -> "absent")
      (match current_mtime with
      | Some t -> string_of_float t
      | None -> "absent")

let () =
  Unix.putenv Service.test_disable_live_signal_restart_env "1";
  Alcotest.run "clawq"
    ([
       ("clawq_core", Test_clawq_core.suite);
       ("agent_loop_conformance", Test_agent_loop_conformance.suite);
       ("cli_exit", Test_cli_exit.suite);
       ("command_bridge", Test_command_bridge.suite);
       ("phase3", Test_phase3.suite);
       ("postmortem", Test_postmortem.suite);
       ("scheduler", Test_scheduler.suite);
       ("background_task", Test_background_task.suite);
       ("bg_shell", Test_bg_shell.suite);
       ("migrate", Test_migrate.suite);
       ("audit", Test_audit.suite);
       ("daemon", Test_daemon.suite);
       ("service", Test_service.suite);
       ("restart", Test_restart.suite);
       ("session", Test_session.suite);
       ("session_observer", Test_session_observer.suite);
       ("session_persistence", Test_session_persistence.suite);
       ("update_tool", Test_update_tool.suite);
       ("update_pkg_manager", Test_update_pkg_manager.suite);
       ("skills", Test_skills.suite);
       ("skills_cmd_inject", Test_skills_cmd_inject.suite);
       ("memory_search", Test_memory_search.suite);
       ("history_search", Test_history_search.suite);
       ("streaming", Test_streaming.suite);
       ("stream_visibility", Test_stream_visibility.suite);
       ("connector_tool_calls", Test_connector_tool_calls.suite);
       ("http_server", Test_http_server.suite);
       ("config_loader", Test_config_loader.suite);
       ("prompt_builder", Test_prompt_builder.suite);
       ("vector", Test_vector.suite);
       ("secret_store", Test_secret_store.suite);
       ("openai_codex_oauth", Test_openai_codex_oauth.suite);
       ("provider_openai_codex", Test_provider_openai_codex.suite);
       ("tools_security", Test_tools_security.suite);
       ("mcp", Test_mcp.suite);
       ("runner_integration", Test_runner_integration.suite);
       ("runner_relay", Test_runner_relay.suite);
       ("resilience", Test_resilience.suite);
       ("perf", Test_perf.suite);
       ("memory_retention", Test_memory_retention.suite);
       ("landlock", Test_landlock.suite);
       ("rate_limiter", Test_rate_limiter.suite);
       ("slack", Test_slack.suite);
       ("discord", Test_discord.suite);
       ("attachment_download", Test_attachment_download.suite);
       ("group_chat_filter", Test_group_chat_filter.suite);
       ("dot_dir", Test_dot_dir.suite);
       ("discord_gateway", Test_discord_gateway.suite);
       ("slack_socket", Test_slack_socket.suite);
       ("slash_commands", Test_slash_commands.suite);
       ("provider", Test_provider.suite);
       ("provider_quota", Test_provider_quota.suite);
       ("totp", Test_totp.suite);
       ("cost_tracker", Test_cost_tracker.suite);
       ("request_stats", Test_request_stats.suite);
       ("debate", Test_debate.suite);
       ("agent_router", Test_agent_router.suite);
       ("agent_template", Test_agent_template.suite);
       ("signal", Test_signal.suite);
       ("http_client", Test_http_client.suite);
       ("http_debug", Test_http_debug.suite);
       ("matrix", Test_matrix.suite);
       ("irc", Test_irc.suite);
       ("email", Test_email.suite);
       ("whatsapp", Test_whatsapp.suite);
       ("nostr", Test_nostr.suite);
       ("mattermost", Test_mattermost.suite);
       ("lark", Test_lark.suite);
       ("line", Test_line.suite);
       ("dingtalk", Test_dingtalk.suite);
       ("onebot", Test_onebot.suite);
       ("imessage", Test_imessage.suite);
       ("sandbox", Test_sandbox.suite);
       ("provider_anthropic", Test_provider_anthropic.suite);
       ("provider_minimax", Test_provider_minimax.suite);
       ("wasm", Test_wasm.suite);
       ("tunnels", Test_tunnels.suite);
       ("tunnel_manager", Test_tunnel_manager.suite);
       ("pairing", Test_pairing.suite);
       ("memory", Test_memory.suite);
       ("chat_ui", Test_chat_ui.suite);
       ("ui_server", Test_ui_server.suite);
       ("channel_formats", Test_channel_formats.suite);
       ("commands", Test_commands.suite);
       ("contracts", Test_contracts.suite);
       ("tools", Test_tools.suite);
       ("telegram_dedupe", Test_telegram_dedupe.suite);
       ("config_set", Test_config_set.suite);
       ("config_show", Test_config_show.suite);
       ("config_tree", Test_config_tree.suite);
       ("config_search", Test_config_search.suite);
       ("config_wizard", Test_config_wizard.suite);
       ("status_message", Test_status_message.suite);
       ("format_adapter", Test_format_adapter.suite);
       ("reaction_tracker", Test_reaction_tracker.suite);
       ("telegram_format", Test_telegram_format.suite);
       ("telegram_status", Test_telegram_status.suite);
       ("telegram_typing", Test_telegram_typing.suite);
       ("typing_indicator", Test_typing_indicator.suite);
       ("telegram_reactions", Test_telegram_reactions.suite);
       ("compaction_flush", Test_compaction_flush.suite);
       ("task_tree", Test_task_tree.suite);
       ("plan_pipeline", Test_plan_pipeline.suite);
       ("structured_pipeline", Test_structured_pipeline.suite);
       ("benchmark", Test_benchmark.suite);
       ("models_catalog", Test_models_catalog.suite);
       ("xiaomi", Test_xiaomi.suite);
        ("provider_xiaomi", Test_provider_xiaomi.suite);
        ("native_subagents_e2e", Test_native_subagents_e2e.suite);

       ("model_discovery", Test_model_discovery.suite);
       ("model_preferences", Test_model_preferences.suite);
       ("session_keepalive", Test_keepalive.suite);
       ("completions", Test_completions.suite);
       ("session_model_override", Test_session_model_override.suite);
       ("model_validation", Test_model_validation.suite);
       ("summarizer", Test_summarizer.suite);
       ("ask_user_question", Test_ask_user_question.suite);
       ("question_presenter", Test_question_presenter.suite);
     ]
    @ [ ("setup_github", Test_setup_github.suite) ]
    @ [ ("setup_discord", Test_setup_discord.suite) ]
    @ [ ("setup_slack", Test_setup_slack.suite) ]
    @ [ ("setup_teams", Test_setup_teams.suite) ]
    @ [ ("teams", Test_teams.suite) ]
    @ [ ("setup_telegram", Test_setup_telegram.suite) ]
    @ [ ("setup_tunnel", Test_setup_tunnel.suite) ]
    @ [ ("setup_summarizer", Test_setup_summarizer.suite) ]
    @ [ ("setup_tui", Test_setup_tui.suite) ]
    @ [ ("setup_provider", Test_setup_provider.suite) ]
    @ [ ("setup_web_search", Test_setup_web_search.suite) ]
    @ [ ("setup_voice", Test_setup_voice.suite) ]
    @ [ ("setup_cron", Test_setup_cron.suite) ]
    @ [ ("setup_imessage", Test_setup_imessage.suite) ]
    @ [ ("setup_security", Test_setup_security.suite) ]
    @ [ ("setup_gateway", Test_setup_gateway.suite) ]
    @ [ ("setup_totp", Test_setup_totp.suite) ]
    @ [ ("setup_line", Test_setup_line.suite) ]
    @ [ ("setup_whatsapp", Test_setup_whatsapp.suite) ]
    @ [ ("setup_mattermost", Test_setup_mattermost.suite) ]
    @ [ ("setup_dingtalk", Test_setup_dingtalk.suite) ]
    @ [ ("setup_signal_channel", Test_setup_signal_channel.suite) ]
    @ [ ("setup_matrix", Test_setup_matrix.suite) ]
    @ [ ("setup_irc", Test_setup_irc.suite) ]
    @ [ ("setup_email", Test_setup_email.suite) ]
    @ [ ("setup_nostr", Test_setup_nostr.suite) ]
    @ [ ("setup_lark", Test_setup_lark.suite) ]
    @ [ ("setup_onebot", Test_setup_onebot.suite) ]
    @ [ ("setup_notify", Test_setup_notify.suite) ]
    @ [ ("setup_error_watcher", Test_setup_error_watcher.suite) ]
    @ [ ("setup_observer", Test_setup_observer.suite) ]
    @ [ ("setup_zai_mcp", Test_setup_zai_mcp.suite) ]
    @ [ ("setup_memory", Test_setup_memory.suite) ]
    @ [ ("setup_prompt", Test_setup_prompt.suite) ]
    @ [ ("setup_resilience", Test_setup_resilience.suite) ]
    @ [ ("setup_heartbeat", Test_setup_heartbeat.suite) ]
    @ [ ("log_rotation", Test_log_rotation.suite) ]
    @ [ ("worktree_merge", Test_worktree_merge.suite) ]
    @ [ ("table_format", Test_table_format.tests) ]
    @ [ ("markdown_util", Test_markdown_util.suite) ]
    @ [ ("error_watcher", Test_error_watcher.suite) ]
    @ [ ("connector_capabilities", Test_connector_capabilities.tests) ]
    @ [ ("connector_history", Test_connector_history.tests) ]
    @ [ ("status_update", Test_status_update.tests) ]
    @ [ ("content_dsl", Test_content_dsl.tests) ]
    @ [ ("workspace_version", Test_workspace_version.suite) ]
    @ [ ("acp", Test_acp.suite) ]
    @ [ ("admin", Test_admin.suite) ]
    @ [ ("voice_transcription", Test_voice_transcription.suite) ]
    @ [ ("pair_coding", Test_pair_coding.suite) ]
    @ [ ("webhook_handler", Test_webhook_handler.suite) ]
    @ [ ("browser", Test_browser.suite) ]
    @ [ ("rig", Test_rig.suite) ]
    @ [ ("held_items", Test_held_items.suite) ]
    @ [ ("repo_manager", Test_repo_manager.suite) ]
    @ Test_github.suites
    @ [
        ( "config_isolation",
          [
            Alcotest.test_case "real config.json not modified by tests" `Quick
              check_config_not_modified;
          ] );
      ])

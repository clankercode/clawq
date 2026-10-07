(* mem_stats_registry.ml — B830i registry census for the hourly mem_stats line.

   One entry-count gauge per module-level mutable global registry (Hashtbl or
   list ref) so the daily review can attribute gc_live_words growth to a
   specific never-evicted table when sessions/total_msgs are flat. Every gauge
   is a cheap Hashtbl.length/List.length read — safe unlocked under OCaml 5.1
   Lwt cooperative scheduling. Registries already covered by the B830g
   counters (Cost_tracker.sessions, Typing_indicator.typing_watchers,
   Runner_relay tokens) are not duplicated here; bounded, drained work queues
   (telegram pending_callbacks/pending_poll_answers) are excluded. Keys are
   short, unique, lowercase; Mem_stats sorts and prefixes them at render
   time, so list order here is irrelevant. *)

let counters () : (string * int) list =
  [
    ("agent_builtins", Agent_template.registry_count ());
    ("bg_shell_jobs", Bg_shell.registry_count ());
    ("bg_task_running", Background_task_0_format.registry_count ());
    ("budget_soft_warned", Room_budget.registry_count ());
    ("cdp_pool", Cdp_client.registry_count ());
    ("ci_msg_ids", Github_pr_dispatch.registry_count ());
    ("connector_buffers", Connector_history.registry_count ());
    ("discord_rate_warnings", Discord_api.rate_limit_warning_count ());
    ("discord_route_buckets", Discord_api.route_bucket_count ());
    ("discord_route_mutexes", Discord_api.route_mutex_count ());
    ("in_flight_jobs", Scheduler.registry_count ());
    ("model_warned_failures", Model_discovery.registry_count ());
    ("nostr_protocols", Nostr.registry_count ());
    ("otc_entries", Admin.registry_count ());
    ("pair_sessions", Pair_coding_session.registry_count ());
    ("pipeline_builtins", Structured_pipeline.builtin_count ());
    ("progress_msg_ids", Room_progress.registry_count ());
    ("provider_complete_fns", Provider_routing.native_complete_count ());
    ("provider_stream_fns", Provider_routing.native_stream_count ());
    ("quota_cache", Provider_quota_state.registry_count ());
    ("repo_mutexes", Worktree_merge.registry_count ());
    ("responses_available", Provider_responses.registry_count ());
    ("session_hosts", Session_host_registry.registry_count ());
    ("slack_privacy_cache", Slack_api.channel_privacy_cache_count ());
    ("slack_rate_warnings", Slack_api.rate_limit_warning_count ());
    ("strict_rejected", Tool_strict.registry_count ());
    ("teams_conv_last_req", Teams_api.conv_last_request_count ());
    ("teams_pending_consents", Teams_file_consent.registry_count ());
    ("teams_rate_warnings", Teams_api.rate_limit_warning_count ());
    ("temp_downloads", Temp_downloads.registry_count ());
    ("tg_callback_routing", Telegram_api_state.callback_routing_count ());
    ("tg_latest_msg_ids", Telegram_api_state.latest_chat_msg_id_count ());
    ("tg_out_mutexes", Telegram_api_state.outbound_mutex_count ());
    ("tg_out_rate_limited", Telegram_api_state.outbound_rate_limited_count ());
    ("tg_paired_sessions", Telegram_api.paired_session_count ());
    ("tg_poll_routing", Telegram_api_state.poll_routing_count ());
    ("tg_rate_warnings", Telegram_api.rate_limit_warning_count ());
    ("tg_seen_updates", Telegram_api_state.recently_seen_update_count ());
    ("tg_tool_details", Telegram_api_state.tool_result_details_count ());
    ("token_leases", Github_user_token_lease.registry_count ());
    ("turn_delivered", Tools_builtin_session.registry_count ());
  ]

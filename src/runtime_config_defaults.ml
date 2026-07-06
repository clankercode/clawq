open Runtime_config_types

let default_log_config : log_config =
  { max_size_mb = 10; max_files = 5; debug_http = false }

let default_error_watcher_config : error_watcher_config =
  let v = Build_info.version_dev in
  let n = String.length v in
  {
    enabled = n >= 4 && String.sub v (n - 4) 4 = "-dev";
    scan_interval_s = 30.0;
    primary_models = [ "anthropic:claude-opus-4-6"; "openai-codex:gpt-5.4" ];
    fallback_models = [ "zai_coding:glm-5"; "kimi_coding:kimi-for-coding" ];
    cooldown_s = 300.0;
    max_errors_per_batch = 10;
    ignore_patterns = [];
    auto_fix_enabled = false;
    commit_tag = "[INTERNAL_EC]";
  }

let default_interactive_config : interactive_config =
  { enable_question_notes = true }

let default_postmortem_config : postmortem_config =
  {
    enabled = true;
    (* Cheap reasoning-capable default; the postmortem turn is short and
       doesn't need the primary model's quality. *)
    model = Some "zai_coding:glm-5-turbo";
    delay_s = 0.0;
  }

let default_debate_config : debate_config =
  {
    enabled = true;
    default_models =
      [
        "openai-codex:gpt-5.4";
        "zai_coding:glm-5";
        "kimi_coding:kimi-for-coding";
      ];
    judge_model = "anthropic:claude-opus-4-6";
    max_parallel = 5;
  }

let default_observer_config : observer_config =
  {
    enabled = true;
    model = Pmodel.parse_exn "groq:openai/gpt-oss-120b";
    check_every_n_messages = 5;
    round1_window = 8;
    round2_window = 30;
    thinking_token_threshold = 5000;
    consecutive_errors_threshold = 3;
    repeat_call_threshold = 2;
  }

let default_summarizer_config : summarizer_config =
  {
    enabled = true;
    model = Pmodel.parse_exn "groq:openai/gpt-oss-120b";
    escalation_model = None;
    threshold_chars = 1500;
    p1_max_chars = 200_000;
    p2_max_chars = 12_000;
    context_window_messages = 4;
    excluded_tools =
      [ "tool_search"; "unsummarize"; "summarize_thread"; "thread_summary" ];
    max_age_days = 30;
    envelope_template = None;
  }

let default_workspace_files =
  [
    "AGENTS.md";
    "EGO.md";
    "SOUL.md";
    "TOOLS.md";
    "IDENTITY.md";
    "USER.md";
    "HEARTBEAT.md";
    "MEMORY.md";
    "memory.md";
  ]

let default_browser_config : browser_config =
  {
    agent_model = Pmodel.parse_exn "groq:openai/gpt-oss-120b";
    chromium_path = None;
    default_timeout_s = 30.0;
    idle_timeout_s = 300.0;
  }

let default_egress_config : egress_config =
  {
    strictness = Strict;
    default_allowlist =
      [
        {
          host = "clawq.org";
          path = Some "/llms.txt";
          method_ = Some "GET";
          action = Allow;
          log_policy = No_log;
        };
        {
          host = "clawq.org";
          path = Some "/llms-full.txt";
          method_ = Some "GET";
          action = Allow;
          log_policy = No_log;
        };
      ];
  }

let default_workspace () = Dot_dir.sub "workspace"

let default_prompt =
  {
    dynamic_enabled = true;
    include_tools_section = true;
    include_safety_section = true;
    include_workspace_section = true;
    include_runtime_section = true;
    include_datetime_section = true;
    include_autonomy_section = true;
    include_project_docs = true;
    workspace_files = default_workspace_files;
    max_workspace_file_chars = 8000;
    max_workspace_total_chars = 20000;
    max_project_doc_chars = 51200;
    project_doc_warn_chars = 15360;
  }

let default =
  {
    workspace = default_workspace ();
    default_temperature = 0.7;
    default_provider = None;
    providers = [];
    model_context_limits = [];
    agent_defaults =
      {
        primary_model = "openai-codex:gpt-5.4";
        subagent_default_model = None;
        system_prompt = "";
        max_tool_iterations = 10;
        tool_search_enabled = false;
        reasoning_effort = None;
        show_thinking = true;
        drop_thinking = false;
        show_tool_calls = true;
        tool_status_mode = "consolidated";
        send_continuation_checkin = false;
        autonomous_continuation_delay = 90.0;
        autonomous_continuation_enabled = true;
        task_tree_notifications = true;
        max_concurrent_native_agents = None;
      };
    prompt = default_prompt;
    channels =
      {
        cli = true;
        telegram = None;
        discord = None;
        slack = None;
        github = None;
        mattermost = None;
        dingtalk = None;
        imessage = None;
        signal = None;
        matrix = None;
        irc = None;
        email = None;
        whatsapp = None;
        nostr = None;
        lark = None;
        line = None;
        onebot = None;
        teams = None;
      };
    gateway =
      {
        host = "127.0.0.1";
        port = 13451;
        require_pairing = true;
        auth_token = None;
        max_pair_attempts = 5;
        pair_lockout_seconds = 300;
      };
    runtime =
      {
        docker_image = "clawq:latest";
        docker_container_name = "clawq";
        docker_port = 13451;
      };
    tunnel =
      {
        provider = "cloudflare";
        enabled = false;
        url = "";
        managed = false;
        tunnel_name = "";
        config_dir = "";
      };
    memory =
      {
        backend = "sqlite";
        search_enabled = false;
        db_path = "";
        vector_weight = 50;
        keyword_weight = 50;
        embedding_model = None;
        embedding_provider = None;
        compaction_threshold_percent = 80;
        max_messages_per_session = 500;
        max_message_age_days = 30;
        pre_compaction_flush = true;
        task_tree_purge_after_days = -1;
      };
    security =
      {
        workspace_only = true;
        audit_enabled = false;
        tools_enabled = true;
        encrypt_secrets = false;
        rate_limit =
          {
            gateway_per_ip_rpm = 60;
            gateway_per_session_rpm = 30;
            telegram_per_chat_rpm = 20;
            burst_multiplier = 1.5;
          };
        audit_retention =
          {
            max_age_days = 90;
            max_entries = 100000;
            export_before_purge = false;
            export_path = Dot_dir.sub "audit_exports";
          };
        audit_signing_enabled = false;
        landlock_enabled = false;
        landlock_extra_read_paths = [];
        extra_allowed_paths = [];
        allowed_cwd_patterns =
          [
            "$CLAWQ_WORKSPACE/**";
            "$USER_HOME/src/projects-clawq/**";
            "/clawq/path/to/somewhere/else/**";
          ];
        sandbox_backend = "auto";
        attachment_downloads_enabled = true;
        allow_anthropic_oauth_inference = false;
      };
    stt = None;
    mcp =
      {
        enabled = true;
        exposed_tools = None;
        runner_relay_enabled = true;
        runner_token_ttl_hours = 24;
        runner_question_timeout_s = 300;
      };
    resilience =
      {
        timeout_s = 120.0;
        max_retries = 2;
        base_delay_s = 1.0;
        fallback_provider = None;
      };
    voice = None;
    web_channel = None;
    telemetry = None;
    agent_bindings = [];
    heartbeat =
      {
        enabled = true;
        interval_seconds = 250;
        quiet_start = 23;
        quiet_end = 8;
      };
    notify = None;
    web_search = None;
    zai_mcp = None;
    quota_cache_ttl_s = 1800;
    observer = default_observer_config;
    summarizer = default_summarizer_config;
    log = default_log_config;
    interactive = default_interactive_config;
    error_watcher = default_error_watcher_config;
    connector_history =
      {
        enabled = false;
        persist_to_db = false;
        max_messages = 50;
        max_age_days = 7;
      };
    browser = default_browser_config;
    test = { show_skills = false };
    debate = default_debate_config;
    postmortem = default_postmortem_config;
    credential_handles = [];
    access_bundles = [];
    access_scopes = [];
    room_profiles = [];
    room_profile_codebase_grants = [];
    room_profile_bindings = [];
    egress = default_egress_config;
    external_room_policy =
      {
        default_action = Policy_warn "External participants detected.";
        per_connector = [];
      };
  }

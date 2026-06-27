type codex_oauth_config = {
  access_token : string;
  refresh_token : string;
  expires_at_ms : int;
  account_id : string option;
  email : string option;
}

type provider_config = {
  api_key : string;
  kind : string option;
  base_url : string option;
  default_model : string option;
  project_id : string option;
  location : string option;
  service_account_json : string option;
  thinking_budget_tokens : int option;
  oai_thinking_style : string;
  codex_oauth : codex_oauth_config option;
  quota_credentials_file : string option;
  quota_threshold : float option;
  quota_check_enabled : bool;
  prompt_cache_retention : string option;
  http_timeout_s : float option;
      (* B647: per-provider HTTP timeout for Provider.complete /
         complete_streaming calls. When None, falls back to
         Http_client.default_timeout_s. Set to a generous value (e.g. 180s
         or 300s) for providers that take a long time on large contexts
         (e.g. zai_coding/glm-5.1, deepseek-reasoner). *)
  max_output_tokens : int option;
      (* Per-provider max output tokens override. When None, providers fall
         back to their built-in default (typically 8192). *)
}

let default_provider_config : provider_config =
  {
    api_key = "";
    kind = None;
    base_url = None;
    default_model = None;
    project_id = None;
    location = None;
    service_account_json = None;
    thinking_budget_tokens = None;
    oai_thinking_style = "none";
    codex_oauth = None;
    quota_credentials_file = None;
    quota_threshold = None;
    quota_check_enabled = true;
    prompt_cache_retention = Some "24h";
    http_timeout_s = None;
    max_output_tokens = None;
  }

type agent_defaults = {
  primary_model : string;
  subagent_default_model : string option;
      (* When set, agents created with an Agent_template whose template.model
         is None use this model instead of inheriting primary_model. Allows
         the user to route subagents to a cheaper/different provider without
         touching the main agent's model. None preserves legacy behavior. *)
  system_prompt : string;
  max_tool_iterations : int;
  tool_search_enabled : bool;
  reasoning_effort : string option;
  show_thinking : bool;
  drop_thinking : bool;
  show_tool_calls : bool;
  tool_status_mode : string;
  send_continuation_checkin : bool;
  autonomous_continuation_delay : float;
  autonomous_continuation_enabled : bool;
  task_tree_notifications : bool;
  max_concurrent_native_agents : int option;
}

type totp_config = {
  totp_enabled : bool;
  totp_secret : string;
  session_ttl_hours : int;
}

type telegram_account = {
  bot_token : string;
  allow_from : string list;
  totp : totp_config option;
}

type telegram_config = {
  accounts : (string * telegram_account) list;
  text_coalesce_ms : int;
  default_model : string option;
}

type discord_config = {
  bot_token : string;
  allow_guilds : string list;
  allow_users : string list;
  intents : int;
  default_model : string option;
}

type slack_config = {
  bot_token : string;
  signing_secret : string;
  events_path : string;
  allow_channels : string list;
  allow_users : string list;
  app_token : string;
  socket_mode : bool;
  default_model : string option;
}

type github_auth = GithubPat of string

type github_repo_config = {
  name : string;
  webhook_secret : string;
  webhook_path : string;
  agent_name : string option;
  allow_users : string list;
  react_to : string list;
  include_pr_files : bool;
}

type github_config = {
  auth : github_auth;
  repos : github_repo_config list;
  default_model : string option;
}

type mattermost_config = {
  url : string;
  access_token : string;
  team_id : string;
  channel_ids : string list;
  allow_users : string list;
  default_model : string option;
}

type dingtalk_config = {
  app_key : string;
  app_secret : string;
  agent_id : string;
  allow_from : string list;
  webhook_url : string option;
  default_model : string option;
}

type imessage_config = {
  poll_interval_s : float;
  allow_from : string list;
  default_model : string option;
}

type signal_config = {
  base_url : string;
  account : string;
  api_mode : string;
  allow_from : string list;
  max_chunk_bytes : int;
  default_model : string option;
}

type matrix_config = {
  homeserver_url : string;
  access_token : string;
  user_id : string;
  allow_rooms : string list;
  allow_users : string list;
  default_model : string option;
}

type irc_config = {
  host : string;
  port : int;
  tls : bool;
  nick : string;
  password : string option;
  sasl : bool;
  channels : string list;
  allow_from : string list;
  default_model : string option;
}

type email_config = {
  imap_host : string;
  imap_port : int;
  smtp_host : string;
  smtp_port : int;
  username : string;
  password : string;
  from_address : string;
  allow_from : string list;
  poll_interval_s : float;
  default_model : string option;
}

type whatsapp_config = {
  phone_number_id : string;
  access_token : string;
  verify_token : string;
  allow_from : string list;
  default_model : string option;
}

type nostr_config = {
  relays : string list;
  private_key : string;
  pubkey : string;
  nak_path : string;
  allow_from : string list;
  default_model : string option;
}

type lark_config = {
  enabled : bool;
  app_id : string;
  app_secret : string;
  verification_token : string;
  endpoint : string;
  mode : string;
  allow_users : string list;
  default_model : string option;
}

type line_config = {
  channel_access_token : string;
  channel_secret : string;
  allow_from : string list;
  default_model : string option;
}

type onebot_config = {
  ws_url : string;
  http_url : string;
  access_token : string option;
  allow_from : string list;
  allow_groups : string list;
  default_model : string option;
}

type teams_config = {
  app_id : string;
  app_secret : string;
  tenant_id : string;
  webhook_path : string;
  service_url : string;
  allow_teams : string list;
  allow_users : string list;
  default_model : string option;
  mention_mode : string;
      (* "entity" (default): proper Teams <at>Name</at> with entity markup.
         "text": plain @Name prefix, no entity markup.
         "none": no @mention prepended to any message. *)
  file_consent_cards : bool;
      (* true (default): use FileConsentCard flow for file uploads (OneDrive).
         false: skip consent cards, serve files via temp download URL. *)
}

type channel_config = {
  cli : bool;
  telegram : telegram_config option;
  discord : discord_config option;
  slack : slack_config option;
  github : github_config option;
  mattermost : mattermost_config option;
  dingtalk : dingtalk_config option;
  imessage : imessage_config option;
  signal : signal_config option;
  matrix : matrix_config option;
  irc : irc_config option;
  email : email_config option;
  whatsapp : whatsapp_config option;
  nostr : nostr_config option;
  lark : lark_config option;
  line : line_config option;
  onebot : onebot_config option;
  teams : teams_config option;
}

type prompt_config = {
  dynamic_enabled : bool;
  include_tools_section : bool;
  include_safety_section : bool;
  include_workspace_section : bool;
  include_runtime_section : bool;
  include_datetime_section : bool;
  include_autonomy_section : bool;
  include_project_docs : bool;
  workspace_files : string list;
  max_workspace_file_chars : int;
  max_workspace_total_chars : int;
  max_project_doc_chars : int;
  project_doc_warn_chars : int;
}

type gateway_config = {
  host : string;
  port : int;
  require_pairing : bool;
  auth_token : string option;
  max_pair_attempts : int;
  pair_lockout_seconds : int;
}

type log_config = { max_size_mb : int; max_files : int; debug_http : bool }

type runtime_config = {
  docker_image : string;
  docker_container_name : string;
  docker_port : int;
}

type tunnel_config = {
  provider : string;
  enabled : bool;
  url : string;
  managed : bool;
  tunnel_name : string;
  config_dir : string;
}

type memory_config = {
  backend : string;
  search_enabled : bool;
  db_path : string;
  vector_weight : int;
  keyword_weight : int;
  embedding_model : string option;
  embedding_provider : string option;
  compaction_threshold_percent : int;
  max_messages_per_session : int;
  max_message_age_days : int;
  pre_compaction_flush : bool;
  task_tree_purge_after_days : int;
      (** Hard-purge soft-deleted task_tree rows after this many days. Set to <=
          0 to disable auto-purge (default: -1). *)
}

type rate_limit_config = {
  gateway_per_ip_rpm : int;
  gateway_per_session_rpm : int;
  telegram_per_chat_rpm : int;
  burst_multiplier : float;
}

type audit_retention_config = {
  max_age_days : int;
  max_entries : int;
  export_before_purge : bool;
  export_path : string;
}

type security_config = {
  workspace_only : bool;
  audit_enabled : bool;
  tools_enabled : bool;
  encrypt_secrets : bool;
  rate_limit : rate_limit_config;
  audit_retention : audit_retention_config;
  audit_signing_enabled : bool;
  landlock_enabled : bool;
  landlock_extra_read_paths : string list;
  extra_allowed_paths : string list;
      (** Additional absolute paths the agent may access when
          [workspace_only = true]. *)
  allowed_cwd_patterns : string list;
      (** Glob patterns for directories agents may change_working_dir into.
          Supports $CLAWQ_WORKSPACE and $USER_HOME pseudo-variables. *)
  sandbox_backend : string;
      (** Sandbox backend: "auto", "firejail", "bubblewrap", or "none" *)
  attachment_downloads_enabled : bool;
  allow_anthropic_oauth_inference : bool;
      (** B606: Anthropic's recent policy changes require explicit opt-in for
          agentic use of Claude models via Claude Code OAuth credentials or the
          `claude` CLI runner. When false (default), the `claude` runner is
          rejected with a clear error message asking the user to enable this
          flag if they accept the policy implications. *)
}

type stt_config = {
  provider : string;
  model : string;
  language : string option;
}

type resilience_config = {
  timeout_s : float;
  max_retries : int;
  base_delay_s : float;
  fallback_provider : string option;
}

type mcp_config = {
  enabled : bool;
  exposed_tools : string list option;
      (** [None] = expose all registered tools; [Some names] = allowlist *)
  runner_relay_enabled : bool;
  runner_token_ttl_hours : int;
  runner_question_timeout_s : int;
}

type voice_config = {
  stt_enabled : bool;
  tts_enabled : bool;
  stt_provider : string;
  tts_provider : string;
  tts_model : string;
  tts_voice : string;
  tts_speed : float;
  audio_dir : string;
}

type web_channel_config = {
  enabled : bool;
  path_prefix : string;
  totp_secret : string option;
  token_ttl_hours : int;
  allowed_origins : string list;
}

type telemetry_config = {
  enabled : bool;
  endpoint : string;
  service_name : string;
}

type heartbeat_config = {
  enabled : bool;
  interval_seconds : int;
  quiet_start : int;
  quiet_end : int;
}

type notify_config = { channel : string; target : string }

type web_search_config = {
  search_provider : string;  (** "brave" or "ddg" (DuckDuckGo) *)
  search_api_key : string;
  num_results : int;
  search_base_url : string option;
      (** Override API endpoint (e.g. for SearXNG) *)
}

type zai_mcp_config = {
  key : string;
      (** Bearer token for Z.ai API. If empty, auto-detected from providers.zai
          or providers.zai_coding. *)
  websearch_enabled : bool;
  webfetch_enabled : bool;
}

type interactive_config = { enable_question_notes : bool }

type error_watcher_config = {
  enabled : bool;
  scan_interval_s : float;
  primary_models : string list;
  fallback_models : string list;
  cooldown_s : float;
  max_errors_per_batch : int;
  ignore_patterns : string list;
  auto_fix_enabled : bool;
  commit_tag : string;
}

type observer_config = {
  enabled : bool;
  model : Pmodel.t;
  check_every_n_messages : int;
  round1_window : int;
  round2_window : int;
  thinking_token_threshold : int;
  consecutive_errors_threshold : int;
  repeat_call_threshold : int;
}

type summarizer_config = {
  enabled : bool;
  model : Pmodel.t;
  escalation_model : Pmodel.t option;
  threshold_chars : int;
  p1_max_chars : int;
  p2_max_chars : int;
  context_window_messages : int;
  excluded_tools : string list;
  max_age_days : int;
  envelope_template : string option;
}

type connector_history_config = {
  enabled : bool;
  persist_to_db : bool;
  max_messages : int;
  max_age_days : int;
}

type browser_config = {
  agent_model : Pmodel.t;
  chromium_path : string option;
  default_timeout_s : float;
  idle_timeout_s : float;
}

type test_config = { show_skills : bool }

type debate_config = {
  enabled : bool;
  default_models : string list;
  judge_model : string;
  max_parallel : int;
}

type postmortem_config = {
  enabled : bool;
  model : string option;
      (* Provider:model used for the postmortem analysis turn. When None,
         falls back to agent_defaults.primary_model. Recommended default:
         a cheap reasoning-capable model like zai_coding:glm-5-turbo. *)
  delay_s : float;
      (* Wait this many seconds before launching the postmortem agent to
         avoid burst-thrashing on rapid-fire stuck sessions. 0.0 = launch
         immediately. *)
}

type room_profile = {
  id : string;
  display_name : string option;
  model : string;
  system_prompt : string;
  max_tool_iterations : int;
  status : string;
  allowed_tools : string list;
  denied_tools : string list;
}

type room_profile_binding = {
  profile_id : string;
  room : string;
  active : bool;
}

type t = {
  workspace : string;
  default_temperature : float;
  default_provider : string option;
  providers : (string * provider_config) list;
  model_context_limits : (string * int) list;
  agent_defaults : agent_defaults;
  prompt : prompt_config;
  channels : channel_config;
  gateway : gateway_config;
  runtime : runtime_config;
  tunnel : tunnel_config;
  memory : memory_config;
  security : security_config;
  stt : stt_config option;
  mcp : mcp_config;
  resilience : resilience_config;
  voice : voice_config option;
  web_channel : web_channel_config option;
  telemetry : telemetry_config option;
  agent_bindings : Agent_router.binding list;
  heartbeat : heartbeat_config;
  notify : notify_config option;
  web_search : web_search_config option;
  zai_mcp : zai_mcp_config option;
  quota_cache_ttl_s : int;
  observer : observer_config;
  summarizer : summarizer_config;
  log : log_config;
  interactive : interactive_config;
  error_watcher : error_watcher_config;
  connector_history : connector_history_config;
  browser : browser_config;
  test : test_config;
  debate : debate_config;
  postmortem : postmortem_config;
  room_profiles : room_profile list;
  room_profile_bindings : room_profile_binding list;
}

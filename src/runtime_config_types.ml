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
  quota_cache_ttl_s : int option;
      (* Per-provider quota cache TTL override in seconds. When None, falls
         back to the global quota_cache_ttl_s. Useful for rate-limited
         providers (e.g. Kimi at 1800s). *)
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
    quota_cache_ttl_s = None;
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

type private_channel_policy =
  | Pc_deny
  | Pc_allow_if_listed
      (** Slack private-channel defense-in-depth policy.
          - [Pc_deny] (default): private channels are always refused unless
            explicitly listed in [allow_private_channels]. A private channel in
            [allow_channels] alone is NOT sufficient.
          - [Pc_allow_if_listed]: backward-compatible behaviour — a channel
            listed in [allow_channels] is allowed regardless of its privacy
            status. *)

let private_channel_policy_to_string = function
  | Pc_deny -> "deny"
  | Pc_allow_if_listed -> "allow_if_listed"

let private_channel_policy_of_string = function
  | "deny" -> Some Pc_deny
  | "allow_if_listed" -> Some Pc_allow_if_listed
  | _ -> None

type slack_config = {
  bot_token : string;
  signing_secret : string;
  events_path : string;
  allow_channels : string list;
  allow_users : string list;
  allow_private_channels : string list;
      (** Explicit opt-in list for private channels under the [Deny] policy.
          Only channels listed here are allowed when
          [private_channel_policy = Deny]. Ignored under [Allow_if_listed]. *)
  private_channel_policy : private_channel_policy;
      (** Defense-in-depth policy for Slack private channels. Default: [Deny].
      *)
  app_token : string;
  socket_mode : bool;
  default_model : string option;
}

type github_app_installation = { installation_id : int; repos : string list }

type github_app_config = {
  app_id : int;
  private_key_path : string;
  webhook_secret : string;
  installations : github_app_installation list;
}

type github_auth = GithubPat of string | GithubApp of github_app_config

type github_repo_config = {
  name : string;
  webhook_secret : string;
  webhook_path : string;
  agent_name : string option;
  allow_users : string list;
  react_to : string list;
  include_pr_files : bool;
  local_repo_path : string option;
      (** B772: local checkout of this repository on the worker, used for
          code-changing work items (worktree base + policy file source). *)
}

type github_config = {
  auth : github_auth;
  repos : github_repo_config list;
  default_model : string option;
  trigger_login : string option;
      (** B773: GitHub login that triggers work when a comment leads with
          @login or when an issue is assigned to it. *)
  trigger_label : string option;
      (** B773: optional fallback label that triggers work when added, for repos
          where the trigger identity is not assignable. *)
  auth_credential_handle : string option;
      (** If set, GitHub API calls resolve credentials through the credential
          lease API using this handle ID, scoped by the access snapshot. When
          [None], falls back to the raw [auth] field. *)
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
  hosted_runner_isolation : string;
      (** B775: OS isolation for hosted external runners (codex/claude
          background tasks): "off" (legacy behavior), "prefer" (sandbox when a
          backend is available, warn otherwise), or "require" (fail closed when
          no isolation backend is available). Remote worker deployment must use
          "require". *)
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
  credential_handle : string option;
      (** Optional credential handle ID. When set, the STT API key is resolved
          through the credential lease API. Missing or unresolvable handles deny
          before any network call. *)
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
  credential_handle : string option;
      (** Optional credential handle ID. When set, the web search API key is
          resolved through the credential lease API. Missing or unresolvable
          handles deny before any network call. *)
}

type zai_mcp_config = {
  key : string;
      (** Bearer token for Z.ai API. If empty, auto-detected from providers.zai
          or providers.zai_coding. *)
  websearch_enabled : bool;
  webfetch_enabled : bool;
  credential_handle : string option;
      (** Optional credential handle ID. When set, the Z.ai API key is resolved
          through the credential lease API. Missing or unresolvable handles deny
          before any network call. *)
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

type credential_provider =
  | Env_var of { name : string }
      (** Read the credential value from the named environment variable. *)
  | File of { path : string }
      (** Read the credential value from the file at [path]. *)
  | Encrypted of { cipher_text : string }
      (** Decrypt using [Secret_store.resolve_secret]. The [cipher_text] should
          be a [\$ENC:...] prefixed value. *)
  | Prompt of { description : string }
      (** Credential must be supplied interactively at startup. [description] is
          a human-readable hint shown to the operator and persisted in config
          for admin display. *)

type credential_handle = {
  id : string;
      (** Unique handle identifier, e.g. ["github-app:main"]. Referenced by
          [access_bundle.credential_handles]. *)
  provider : credential_provider;
      (** How to resolve the actual credential value at runtime. *)
  description : string option;
      (** Optional human-readable description for admin UIs. *)
  status : string;
      (** ["active"] or ["deleted"]. Soft-delete convention matches
          [access_bundle.status] and [access_scope.status]. *)
}
(** A credential handle binds an opaque identifier to a provider that can
    resolve the actual credential value at runtime. The value itself is NEVER
    stored in the config record, serialized to JSON, included in prompts,
    snapshots, logs, the ledger, or worker sandboxes. Only the handle ID is
    referenced by access bundles and effective access. *)

type instruction_edit_policy = Locked | Admin_only | Open

let instruction_edit_policy_to_string = function
  | Locked -> "locked"
  | Admin_only -> "admin_only"
  | Open -> "open"

let instruction_edit_policy_of_string = function
  | "locked" -> Some Locked
  | "admin_only" -> Some Admin_only
  | "open" -> Some Open
  | _ -> None

type instruction_record = {
  text : string;
  source_scope : string;
  author : string option;
  enabled : bool;
  digest : string option;
  locked : bool;
  edit_policy : instruction_edit_policy;
}

let default_instruction_record ~text () =
  {
    text;
    source_scope = "default";
    author = None;
    enabled = true;
    digest = None;
    locked = false;
    edit_policy = Open;
  }

let instruction_record_digest (ir : instruction_record) : string =
  match ir.digest with
  | Some d -> d
  | None -> Digestif.SHA256.(digest_string ir.text |> to_hex)

let instruction_record_is_active (ir : instruction_record) = ir.enabled

type repo_capability =
  | Read
  | Comment
  | Branch
  | Pr
  | Workflow_read
  | Workflow_trigger
      (** Fine-grained capability for a GitHub repository grant. *)

let repo_capability_to_string = function
  | Read -> "read"
  | Comment -> "comment"
  | Branch -> "branch"
  | Pr -> "pr"
  | Workflow_read -> "workflow-read"
  | Workflow_trigger -> "workflow-trigger"

let repo_capability_of_string = function
  | "read" -> Some Read
  | "comment" -> Some Comment
  | "branch" -> Some Branch
  | "pr" -> Some Pr
  | "workflow-read" -> Some Workflow_read
  | "workflow-trigger" -> Some Workflow_trigger
  | _ -> None

let all_repo_capabilities =
  [ Read; Comment; Branch; Pr; Workflow_read; Workflow_trigger ]

type repo_grant = {
  repo : string;  (** Repository pattern, e.g. "owner/repo" or "owner/*". *)
  capabilities : repo_capability list;
      (** Capabilities granted for this repository. An empty list means no
          capabilities are granted. *)
}
(** A repo grant attaches a set of capabilities to a repository pattern within
    an access bundle. *)

type egress_rule_action = Allow | Deny

let egress_rule_action_to_string = function Allow -> "allow" | Deny -> "deny"

let egress_rule_action_of_string = function
  | "allow" -> Some Allow
  | "deny" -> Some Deny
  | _ -> None

type egress_rule_log_policy = Log | No_log

let egress_rule_log_policy_to_string = function
  | Log -> "log"
  | No_log -> "no_log"

let egress_rule_log_policy_of_string = function
  | "log" -> Some Log
  | "no_log" -> Some No_log
  | _ -> None

type egress_rule = {
  host : string;
      (** Host pattern to match. Supports glob-style wildcards:
          - "*.example.com" matches any subdomain of example.com
          - "api.example.com" matches exactly
          - "*" matches any host *)
  path : string option;
      (** Optional path pattern. Supports glob-style wildcards:
          - "/api/*" matches any path under /api/
          - "/v1/users" matches exactly
          - None matches any path *)
  method_ : string option;
      (** Optional HTTP method pattern (GET, POST, etc.). Case-insensitive
          matching. None matches any method. *)
  action : egress_rule_action;  (** Allow or deny the matching request. *)
  log_policy : egress_rule_log_policy;  (** Whether to log matching requests. *)
}
(** An egress rule matches outbound HTTP requests by host, path, and method.
    When multiple rules match, the first match wins. Unmatched destinations use
    the top-level egress strictness. *)

let default_egress_rule : egress_rule =
  { host = "*"; path = None; method_ = None; action = Deny; log_policy = Log }

type egress_strictness = Strict | Permissive

let egress_strictness_to_string = function
  | Strict -> "strict"
  | Permissive -> "permissive"

let egress_strictness_of_string = function
  | "strict" | "deny" | "default_deny" | "default-deny" -> Some Strict
  | "permissive" | "allow" | "default_allow" | "default-allow" ->
      Some Permissive
  | _ -> None

type egress_config = {
  strictness : egress_strictness;
      (** [Strict] denies unmatched HTTP destinations; [Permissive] allows
          unmatched destinations after explicit rules and the default allowlist
          have been evaluated. *)
  default_allowlist : egress_rule list;
      (** Global fallback allowlist evaluated after scoped access-bundle rules.
          Explicit deny rules in higher-priority scopes can still override
          entries here. *)
}

type access_bundle = {
  id : string;
  display_name : string option;
  system_prompt : string option;
  allowed_tools : string list;
  denied_tools : string list;
  codebase_grants : string list;
  mcp_servers : string list;
  skills : string list;
  repositories : string list;
      (** Deprecated: use [repo_grants] for fine-grained capability control.
          Legacy string entries are treated as read-only repo grants during
          effective-access resolution. *)
  repo_grants : repo_grant list;
      (** GitHub repository grants with fine-grained capabilities. *)
  domains : string list;
  egress_rules : egress_rule list;
      (** Egress rules for outbound HTTP requests. Rules are evaluated in order;
          first match wins. If no scoped rule matches, the top-level egress
          allowlist and strictness decide the request. *)
  credential_handles : string list;
  instructions : instruction_record list;
  memory_grants : string list;
  budget_refs : string list;
  status : string;
}

type access_scope_level = Default | Workspace | Channel | Room

type access_scope = {
  id : string;
  level : access_scope_level;
  workspace : string option;
  channel : string option;
  room : string option;
  access_bundle_ids : string list;
  status : string;
}

type access_provenance = { layer : string; source_id : string; field : string }

type effective_access_item = {
  value : string;
  provenance : access_provenance list;
}

type effective_instruction_item = {
  instruction : instruction_record;
  provenance : access_provenance list;
}
(** An instruction resolved through the scope chain, carrying its full metadata
    and the provenance trail showing which scope/bundle contributed it. *)

type effective_access = {
  allowed_tools : effective_access_item list;
  denied_tools : effective_access_item list;
  codebase_grants : effective_access_item list;
  blocked_codebase_grants : effective_access_item list;
  mcp_servers : effective_access_item list;
  skills : effective_access_item list;
  repositories : effective_access_item list;
      (** Legacy repository names (read-only). *)
  repo_grants : effective_access_item list;
      (** Repository grants with capabilities. Each value is a JSON object
          string containing [repo] and [capabilities] fields. *)
  blocked_repo_grants : effective_access_item list;
      (** Repository grants requested by matching scopes but blocked by global
          security policy or by the effective codebase-grant ceiling. *)
  domains : effective_access_item list;
  credential_handles : effective_access_item list;
  instructions : effective_access_item list;
  instruction_items : effective_instruction_item list;
      (** Full instruction records with provenance. [instructions] contains the
          text-only view (for backward compatibility with snapshots and prompt
          injection); [instruction_items] carries the structured records with
          source_scope, author, enabled, digest, and edit_policy. *)
  memory_grants : effective_access_item list;
  budget_refs : effective_access_item list;
  egress_rules : egress_rule list;
      (** Resolved egress rules from all matching bundles. Rules from higher-
          priority scopes (Room > Channel > Workspace > Default) come first.
          Top-level egress policy decides unmatched destinations. *)
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
  access_bundle_ids : string list;
  ambient_enabled : bool;
  ambient_quiet_start : int;
  ambient_quiet_end : int;
  ambient_rate_limit_rph : int;
}

type room_profile_binding = {
  profile_id : string;
  room : string;
  active : bool;
}

(** {1 Guest / External Room Policy}

    Classifies rooms by their external/guest dimensions and applies
    per-connector policy actions. Connectors that expose guest/shared/external
    metadata feed it into this model; unsupported connectors return [Rm_unknown]
    and the default action applies. *)

type room_scope =
  | Rm_dm  (** Direct message between two internal users. *)
  | Rm_group  (** Internal group conversation. *)
  | Rm_external
      (** Room with external participants (cross-tenant, federated, etc.). *)
  | Rm_shared  (** Shared room/channel with another organization. *)
  | Rm_unknown  (** Connector does not expose room classification metadata. *)

type room_classification = {
  connector : string;
      (** Lower-case connector name ("teams", "slack", etc.). *)
  room_id : string;  (** Room/channel identifier. *)
  scope : room_scope;
  has_external_users : bool;
      (** True when the connector detects users from outside the org. *)
  tenant_id : string option;
      (** Tenant/organization identifier when available. *)
}

type external_policy_action =
  | Policy_allow  (** Proceed without restriction. *)
  | Policy_warn of string  (** Proceed but show the warning message. *)
  | Policy_deny of string * bool
      (** Deny work. The string is the reason; the bool indicates whether admin
          callers may override the denial. *)

type external_room_policy = {
  default_action : external_policy_action;
      (** Action to take for rooms whose connector has no specific override. *)
  per_connector : (string * external_policy_action) list;
      (** Per-connector overrides, keyed by lower-case connector name. *)
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
  credential_handles : credential_handle list;
  access_bundles : access_bundle list;
  access_scopes : access_scope list;
  room_profiles : room_profile list;
  room_profile_codebase_grants : (string * string list) list;
  room_profile_bindings : room_profile_binding list;
  egress : egress_config;
  external_room_policy : external_room_policy;
}

include Runtime_config_types

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
    quota_cache_ttl_s = 300;
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
    room_profiles = [];
    room_profile_bindings = [];
  }

let is_key_set key =
  key <> "" && not (String.length key > 4 && String.sub key 0 4 = "YOUR")

let provider_has_codex_oauth (p : provider_config) =
  match p.codex_oauth with
  | Some creds ->
      is_key_set creds.access_token || is_key_set creds.refresh_token
  | None -> false

let provider_has_auth (p : provider_config) =
  is_key_set p.api_key || provider_has_codex_oauth p

let is_credential_valid cred =
  String.length cred > 6
  && not (String.length cred > 4 && String.sub cred 0 4 = "YOUR")

let telegram_account_has_valid_credentials (acct : telegram_account) =
  is_credential_valid acct.bot_token

let telegram_has_valid_credentials (cfg : telegram_config) =
  List.exists
    (fun (_, acct) -> telegram_account_has_valid_credentials acct)
    cfg.accounts

let discord_has_valid_credentials (cfg : discord_config) =
  is_credential_valid cfg.bot_token

let slack_has_valid_credentials (cfg : slack_config) =
  is_credential_valid cfg.bot_token && is_credential_valid cfg.signing_secret

let github_has_valid_credentials (cfg : github_config) =
  match cfg.auth with GithubPat token -> is_credential_valid token

let mattermost_has_valid_credentials (cfg : mattermost_config) =
  is_credential_valid cfg.access_token

let dingtalk_has_valid_credentials (cfg : dingtalk_config) =
  is_credential_valid cfg.app_key && is_credential_valid cfg.app_secret

let matrix_has_valid_credentials (cfg : matrix_config) =
  is_credential_valid cfg.access_token

let email_has_valid_credentials (cfg : email_config) =
  is_credential_valid cfg.username && is_credential_valid cfg.password

let whatsapp_has_valid_credentials (cfg : whatsapp_config) =
  is_credential_valid cfg.access_token

let nostr_has_valid_credentials (cfg : nostr_config) =
  is_credential_valid cfg.private_key

let lark_has_valid_credentials (cfg : lark_config) =
  cfg.enabled
  && is_credential_valid cfg.app_id
  && is_credential_valid cfg.app_secret

let line_has_valid_credentials (cfg : line_config) =
  is_credential_valid cfg.channel_access_token
  && is_credential_valid cfg.channel_secret

let onebot_has_valid_credentials (cfg : onebot_config) =
  match cfg.access_token with
  | None -> true
  | Some token -> is_credential_valid token

let teams_has_valid_credentials (cfg : teams_config) =
  is_credential_valid cfg.app_id
  && is_credential_valid cfg.app_secret
  && is_credential_valid cfg.tenant_id

let irc_has_valid_credentials (cfg : irc_config) =
  match cfg.password with None -> true | Some pw -> is_credential_valid pw

let signal_has_valid_credentials (_cfg : signal_config) = true
let imessage_has_valid_credentials (_cfg : imessage_config) = true

let channel_type_of_session_key key =
  match String.index_opt key ':' with
  | Some i -> String.sub key 0 i
  | None -> ""

let channel_default_model (cfg : t) ~channel_type =
  match channel_type with
  | "telegram" -> Option.bind cfg.channels.telegram (fun c -> c.default_model)
  | "discord" -> Option.bind cfg.channels.discord (fun c -> c.default_model)
  | "slack" -> Option.bind cfg.channels.slack (fun c -> c.default_model)
  | "github" -> Option.bind cfg.channels.github (fun c -> c.default_model)
  | "mattermost" ->
      Option.bind cfg.channels.mattermost (fun c -> c.default_model)
  | "dingtalk" -> Option.bind cfg.channels.dingtalk (fun c -> c.default_model)
  | "imessage" -> Option.bind cfg.channels.imessage (fun c -> c.default_model)
  | "signal" -> Option.bind cfg.channels.signal (fun c -> c.default_model)
  | "matrix" -> Option.bind cfg.channels.matrix (fun c -> c.default_model)
  | "irc" -> Option.bind cfg.channels.irc (fun c -> c.default_model)
  | "email" -> Option.bind cfg.channels.email (fun c -> c.default_model)
  | "whatsapp" -> Option.bind cfg.channels.whatsapp (fun c -> c.default_model)
  | "nostr" -> Option.bind cfg.channels.nostr (fun c -> c.default_model)
  | "lark" -> Option.bind cfg.channels.lark (fun c -> c.default_model)
  | "line" -> Option.bind cfg.channels.line (fun c -> c.default_model)
  | "onebot" -> Option.bind cfg.channels.onebot (fun c -> c.default_model)
  | "teams" -> Option.bind cfg.channels.teams (fun c -> c.default_model)
  | _ -> None

let all_channel_types =
  [
    "telegram";
    "discord";
    "slack";
    "github";
    "mattermost";
    "dingtalk";
    "imessage";
    "signal";
    "matrix";
    "irc";
    "email";
    "whatsapp";
    "nostr";
    "lark";
    "line";
    "onebot";
    "teams";
  ]

(** [resolve_room_profile cfg ~session_key] resolves the active room profile
    bound to the given session key. Matching tries the full session key first,
    then the channel_id portion (everything after the first colon, matching
    [Restart_notify.parse_channel_from_key] semantics). Only [active = true]
    bindings are considered. *)
let resolve_room_profile (cfg : t) ~session_key : room_profile option =
  if cfg.room_profiles = [] || cfg.room_profile_bindings = [] then None
  else
    let channel_id =
      match String.index_opt session_key ':' with
      | Some i ->
          String.sub session_key (i + 1) (String.length session_key - i - 1)
      | None -> ""
    in
    let find_binding () =
      List.find_opt
        (fun (b : room_profile_binding) ->
          b.active
          && (b.room = session_key || (channel_id <> "" && b.room = channel_id)))
        cfg.room_profile_bindings
    in
    match find_binding () with
    | None -> None
    | Some b ->
        List.find_opt
          (fun (p : room_profile) ->
            p.id = b.profile_id && String.lowercase_ascii p.status <> "deleted")
          cfg.room_profiles

(** [resolve_room_profile_model cfg ~session_key] resolves the model from the
    room profile bound to the given session key. Only the model field is
    returned; use [resolve_room_profile] for full profile access. *)
let resolve_room_profile_model (cfg : t) ~session_key : string option =
  match resolve_room_profile cfg ~session_key with
  | Some p when p.model <> "" -> Some p.model
  | _ -> None

let effective_compaction_threshold_percent (memory : memory_config) =
  let p = memory.compaction_threshold_percent in
  if p <= 0 || p >= 100 then default.memory.compaction_threshold_percent else p

type model_target = { provider : string option; model : string }

let effective_primary_target (ad : agent_defaults) : model_target =
  let raw = String.trim ad.primary_model in
  let split_at delim =
    match String.index_opt raw delim with
    | Some i when i > 0 && i + 1 < String.length raw ->
        let provider = String.sub raw 0 i in
        let model = String.sub raw (i + 1) (String.length raw - i - 1) in
        Some { provider = Some provider; model }
    | _ -> None
  in
  match split_at ':' with
  | Some t -> t
  | None -> (
      match split_at '/' with
      | Some t -> t
      | None -> { provider = None; model = raw })

let strip_model_provider_prefix model =
  let try_strip delim =
    match String.index_opt model delim with
    | Some i when i > 0 && i + 1 < String.length model ->
        Some (String.sub model (i + 1) (String.length model - i - 1))
    | _ -> None
  in
  match try_strip ':' with
  | Some m -> m
  | None -> ( match try_strip '/' with Some m -> m | None -> model)

let context_window_table =
  [
    ("claude-opus-4-6", 200000);
    ("claude-sonnet-4-6", 200000);
    ("claude-haiku-4-5", 200000);
    ("claude-3.5-sonnet", 200000);
    ("claude-3.5-haiku", 200000);
    ("claude-3-opus", 200000);
    ("claude-3-sonnet", 200000);
    ("claude-3-haiku", 200000);
    ("gpt-4o", 128000);
    ("gpt-4o-mini", 128000);
    ("gpt-4-turbo", 128000);
    ("gpt-4", 8192);
    ("gpt-3.5-turbo", 16385);
    ("gpt-5.4", 272000);
    ("o1", 200000);
    ("o1-mini", 128000);
    ("o1-preview", 128000);
    ("o3", 200000);
    ("o3-mini", 200000);
    ("llama-3.3-70b", 128000);
    ("llama-3.1-405b", 128000);
    ("llama-3.1-70b", 128000);
    ("llama-3.1-8b", 128000);
    ("mistral-large", 128000);
    ("mixtral-8x7b", 32768);
    ("gemini-2.0-flash", 1048576);
    ("gemini-1.5-pro", 2097152);
    ("gemini-1.5-flash", 1048576);
    ("deepseek-v3", 128000);
    ("deepseek-r1", 128000);
    ("command-r-plus", 128000);
    ("command-r", 128000);
    ("minimax-m2", 204800);
    ("minimax-m2-highspeed", 204800);
    ("minimax-m2.1", 204800);
    ("minimax-m2.1-highspeed", 204800);
    ("minimax-m2.5", 204800);
    ("minimax-m2.5-highspeed", 204800);
    ("minimax-m2.7", 204800);
    ("minimax-m2.7-highspeed", 204800);
  ]

(* Operating ceilings for the runtime compaction budget. These cap the context
   window we will fill before compacting, even when a model advertises more (the
   API gets impractically slow past ~500k tokens). Keys are pre-normalized
   (lowercase, no provider prefix, no date suffix) and must be fully qualified so
   prefix matching does not over-capture (e.g. "gpt-5.5", never "gpt-5").
   Override per-model via the `model_context_limits` config map. *)
let default_model_context_caps =
  [
    ("gpt-5.5", 272000);
    ("minimax-m3", 512000);
    ("mimo-v2.5-pro", 512000);
    ("glm-5.2", 272000);
  ]

let normalize_model_name_for_context_lookup model_name =
  let norm =
    String.lowercase_ascii
      (Model_utils.strip_date_suffix (String.trim model_name))
  in
  strip_model_provider_prefix norm

let context_window_for_model ?(configured_limits = []) model_name =
  let bare = normalize_model_name_for_context_lookup model_name in
  let find_prefix hay needle =
    String.length hay >= String.length needle
    && String.sub hay 0 (String.length needle) = needle
  in
  (* exact match, then prefix match, over a name->value table *)
  let lookup table =
    match List.find_opt (fun (k, _) -> bare = k) table with
    | Some (_, v) -> Some v
    | None -> (
        match List.find_opt (fun (k, _) -> find_prefix bare k) table with
        | Some (_, v) -> Some v
        | None -> None)
  in
  let normalized_configured_limits =
    List.map
      (fun (name, limit) ->
        (normalize_model_name_for_context_lookup name, limit))
      configured_limits
  in
  match lookup normalized_configured_limits with
  | Some v -> Some v (* user override wins outright; may exceed a shipped cap *)
  | None -> (
      let base = lookup context_window_table in
      let cap = lookup default_model_context_caps in
      (* ceiling semantics: cap the advertised window when both are known *)
      match (base, cap) with
      | Some b, Some c -> Some (min b c)
      | None, Some c -> Some c
      | Some b, None -> Some b
      | None, None -> None)

let effective_primary_model (ad : agent_defaults) =
  (effective_primary_target ad).model

let effective_primary_provider (ad : agent_defaults) =
  (effective_primary_target ad).provider

let primary_model_deprecation_warning (ad : agent_defaults) =
  Pmodel.deprecation_warning (Pmodel.parse_flexible ad.primary_model)

let default_provider_deprecation_warning (cfg : t) =
  match cfg.default_provider with
  | None -> None
  | Some p ->
      Some
        (Printf.sprintf
           "WARNING: \"default_provider\" (\"%s\") is deprecated. The provider \
            is already embedded in \"agent_defaults.primary_model\" using the \
            \"provider:model\" format. Remove \"default_provider\" from your \
            config.json."
           p)

let home_dir () = try Sys.getenv "HOME" with Not_found -> "/tmp"

let expand_home path =
  if String.length path >= 2 && String.sub path 0 2 = "~/" then
    Filename.concat (home_dir ()) (String.sub path 2 (String.length path - 2))
  else if path = "~" then home_dir ()
  else path

let is_existing_dir path =
  try Sys.file_exists path && Sys.is_directory path with _ -> false

let opam_bin_dirs home =
  let opam_dir = Filename.concat home ".opam" in
  if not (is_existing_dir opam_dir) then []
  else
    try
      Sys.readdir opam_dir |> Array.to_list
      |> List.filter_map (fun switch ->
          let bin_dir =
            Filename.concat (Filename.concat opam_dir switch) "bin"
          in
          if is_existing_dir bin_dir then Some bin_dir else None)
    with _ -> []

let common_user_bin_dirs () =
  let home = home_dir () in
  [
    ".local/share/pnpm";
    ".local/share/pnpm/bin";
    ".cargo/bin";
    ".bun/bin";
    ".local/bin";
    ".npm-global/bin";
  ]
  |> List.map (Filename.concat home)
  |> fun dirs ->
  dirs @ opam_bin_dirs home
  |> List.filter is_existing_dir
  |> List.sort_uniq String.compare

let augment_path_with_user_bins path =
  let existing =
    String.split_on_char ':' path |> List.filter (fun entry -> entry <> "")
  in
  let additions =
    common_user_bin_dirs ()
    |> List.filter (fun dir -> not (List.mem dir existing))
  in
  String.concat ":" (existing @ additions)

let workspace_only_env () =
  [|
    "HOME=" ^ home_dir ();
    "PATH="
    ^ augment_path_with_user_bins
        (try Sys.getenv "PATH" with Not_found -> "/usr/bin:/bin");
  |]

let augment_env_path env =
  let prefix = "PATH=" in
  let plen = String.length prefix in
  let replaced = ref false in
  let updated =
    Array.map
      (fun entry ->
        if String.length entry >= plen && String.sub entry 0 plen = prefix then begin
          replaced := true;
          let path = String.sub entry plen (String.length entry - plen) in
          prefix ^ augment_path_with_user_bins path
        end
        else entry)
      env
  in
  if !replaced then updated
  else
    Array.append updated
      [|
        prefix
        ^ augment_path_with_user_bins
            (try Sys.getenv "PATH" with Not_found -> "/usr/bin:/bin");
      |]

let effective_workspace (cfg : t) =
  let path = expand_home cfg.workspace in
  if path = "" then default_workspace () else path

let expand_cwd_pattern ~(config : t) pattern =
  let home =
    match Sys.getenv_opt "HOME" with
    | Some h -> h
    | None ->
        let ts = int_of_float (Unix.gettimeofday ()) in
        let fallback = Printf.sprintf "/tmp/clawq-home-%d" ts in
        Printf.eprintf "ERROR: HOME environment variable not set, using %s\n%!"
          fallback;
        fallback
  in
  let ws = effective_workspace config in
  pattern
  |> Str.global_replace (Str.regexp_string "$CLAWQ_WORKSPACE") ws
  |> Str.global_replace (Str.regexp_string "$USER_HOME") home

let default_model_json_fields = Runtime_config_json.default_model_json_fields

let to_json (cfg : t) : Yojson.Safe.t =
  Runtime_config_json.to_json
    ~default_quota_cache_ttl_s:default.quota_cache_ttl_s ~default_log_config cfg

let merge_with_coq (coq_cfg : Clawq_core.clawqConfig) (cfg : t) : t =
  let gw = coq_cfg.config_gateway in
  let mem = coq_cfg.config_memory in
  let sec = coq_cfg.config_security in
  {
    cfg with
    default_temperature =
      float_of_int coq_cfg.config_default_temperature /. 100.0;
    agent_defaults =
      { cfg.agent_defaults with primary_model = coq_cfg.config_default_model };
    gateway =
      {
        host = gw.gateway_host;
        port = gw.gateway_port;
        require_pairing = gw.gateway_require_pairing;
        auth_token = cfg.gateway.auth_token;
        max_pair_attempts = cfg.gateway.max_pair_attempts;
        pair_lockout_seconds = cfg.gateway.pair_lockout_seconds;
      };
    memory =
      {
        cfg.memory with
        backend = mem.memory_backend;
        search_enabled = mem.memory_search_enabled;
      };
    security =
      {
        cfg.security with
        workspace_only = sec.security_workspace_only_cfg;
        audit_enabled = sec.security_audit_enabled_cfg;
        encrypt_secrets = sec.security_encrypt_secrets_cfg;
        (* rate_limit, audit_retention, audit_signing, landlock preserved from JSON config *)
      };
  }

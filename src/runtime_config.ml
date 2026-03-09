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
}

type agent_defaults = {
  primary_model : string;
  system_prompt : string;
  max_tool_iterations : int;
  tool_search_enabled : bool;
  reasoning_effort : string option;
  show_thinking : bool;
  show_tool_calls : bool;
  tool_status_mode : string;
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

type telegram_config = { accounts : (string * telegram_account) list }

type discord_config = {
  bot_token : string;
  allow_guilds : string list;
  allow_users : string list;
  intents : int;
}

type slack_config = {
  bot_token : string;
  signing_secret : string;
  events_path : string;
  allow_channels : string list;
  allow_users : string list;
  app_token : string;
  socket_mode : bool;
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

type github_config = { auth : github_auth; repos : github_repo_config list }

type mattermost_config = {
  url : string;
  access_token : string;
  team_id : string;
  channel_ids : string list;
  allow_users : string list;
}

type dingtalk_config = {
  app_key : string;
  app_secret : string;
  agent_id : string;
  allow_from : string list;
  webhook_url : string option;
}

type imessage_config = { poll_interval_s : float; allow_from : string list }

type signal_config = {
  base_url : string;
  account : string;
  api_mode : string;
  allow_from : string list;
  max_chunk_bytes : int;
}

type matrix_config = {
  homeserver_url : string;
  access_token : string;
  user_id : string;
  allow_rooms : string list;
  allow_users : string list;
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
}

type whatsapp_config = {
  phone_number_id : string;
  access_token : string;
  verify_token : string;
  allow_from : string list;
}

type nostr_config = {
  relays : string list;
  private_key : string;
  pubkey : string;
  nak_path : string;
  allow_from : string list;
}

type lark_config = {
  enabled : bool;
  app_id : string;
  app_secret : string;
  verification_token : string;
  endpoint : string;
  mode : string;
  allow_users : string list;
}

type line_config = {
  channel_access_token : string;
  channel_secret : string;
  allow_from : string list;
}

type onebot_config = {
  ws_url : string;
  http_url : string;
  access_token : string option;
  allow_from : string list;
  allow_groups : string list;
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
}

type prompt_config = {
  dynamic_enabled : bool;
  include_tools_section : bool;
  include_safety_section : bool;
  include_workspace_section : bool;
  include_runtime_section : bool;
  include_datetime_section : bool;
  include_autonomy_section : bool;
  workspace_files : string list;
  max_workspace_file_chars : int;
  max_workspace_total_chars : int;
}

type gateway_config = {
  host : string;
  port : int;
  require_pairing : bool;
  auth_token : string option;
  max_pair_attempts : int;
  pair_lockout_seconds : int;
}

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
  sandbox_backend : string;
      (** Sandbox backend: "auto", "firejail", "bubblewrap", or "none" *)
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
}

type voice_config = {
  stt_enabled : bool;
  tts_enabled : bool;
  stt_provider : string;
  tts_provider : string;
  tts_model : string;
  tts_voice : string;
  audio_dir : string;
}

type web_channel_config = {
  enabled : bool;
  path_prefix : string;
  totp_secret : string option;
  token_ttl_hours : int;
}

type telemetry_config = {
  enabled : bool;
  endpoint : string;
  service_name : string;
}

type heartbeat_config = {
  heartbeat_enabled : bool;
  heartbeat_interval_seconds : int;
  heartbeat_quiet_start : int;
  heartbeat_quiet_end : int;
}

type notify_config = { notify_channel : string; notify_target : string }

type web_search_config = {
  search_provider : string;  (** "brave" or "ddg" (DuckDuckGo) *)
  search_api_key : string;
  num_results : int;
  search_base_url : string option;
      (** Override API endpoint (e.g. for SearXNG) *)
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

let default_workspace () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat (Filename.concat home ".clawq") "workspace"

let default_prompt =
  {
    dynamic_enabled = true;
    include_tools_section = true;
    include_safety_section = true;
    include_workspace_section = true;
    include_runtime_section = true;
    include_datetime_section = true;
    include_autonomy_section = true;
    workspace_files = default_workspace_files;
    max_workspace_file_chars = 8000;
    max_workspace_total_chars = 20000;
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
        primary_model = "openai/gpt-4o";
        system_prompt = "";
        max_tool_iterations = 10;
        tool_search_enabled = false;
        reasoning_effort = None;
        show_thinking = false;
        show_tool_calls = true;
        tool_status_mode = "consolidated";
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
        compaction_threshold_percent = 75;
        max_messages_per_session = 500;
        max_message_age_days = 30;
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
            export_path =
              (let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
               Filename.concat (Filename.concat home ".clawq") "audit_exports");
          };
        audit_signing_enabled = false;
        landlock_enabled = false;
        landlock_extra_read_paths = [];
        extra_allowed_paths = [];
        sandbox_backend = "auto";
      };
    stt = None;
    mcp = { enabled = true; exposed_tools = None };
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
        heartbeat_enabled = true;
        heartbeat_interval_seconds = 300;
        heartbeat_quiet_start = 23;
        heartbeat_quiet_end = 8;
      };
    notify = None;
    web_search = None;
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
  match split_at '/' with
  | Some t -> t
  | None -> (
      match split_at ':' with
      | Some t -> t
      | None -> { provider = None; model = raw })

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
  ]

let strip_date_suffix_cfg s =
  let len = String.length s in
  if len >= 9 && s.[len - 9] = '-' then
    let suffix = String.sub s (len - 8) 8 in
    let all_digits =
      try
        String.iter (fun c -> if c < '0' || c > '9' then raise Exit) suffix;
        true
      with Exit -> false
    in
    if all_digits then String.sub s 0 (len - 9) else s
  else s

let normalize_model_name_for_context_lookup model_name =
  let norm =
    String.lowercase_ascii (strip_date_suffix_cfg (String.trim model_name))
  in
  match String.index_opt norm '/' with
  | Some i when i + 1 < String.length norm ->
      String.sub norm (i + 1) (String.length norm - i - 1)
  | _ -> norm

let context_window_for_model ?(configured_limits = []) model_name =
  let bare = normalize_model_name_for_context_lookup model_name in
  let find_prefix hay needle =
    String.length hay >= String.length needle
    && String.sub hay 0 (String.length needle) = needle
  in
  let normalized_configured_limits =
    List.map
      (fun (name, limit) ->
        (normalize_model_name_for_context_lookup name, limit))
      configured_limits
  in
  match List.find_opt (fun (k, _) -> bare = k) normalized_configured_limits with
  | Some (_, v) -> Some v
  | None -> (
      match List.find_opt (fun (k, _) -> bare = k) context_window_table with
      | Some (_, v) -> Some v
      | None -> (
          match
            List.find_opt
              (fun (k, _) -> find_prefix bare k)
              normalized_configured_limits
          with
          | Some (_, v) -> Some v
          | None -> (
              match
                List.find_opt
                  (fun (k, _) -> find_prefix bare k)
                  context_window_table
              with
              | Some (_, v) -> Some v
              | None -> None)))

let effective_primary_model (ad : agent_defaults) =
  (effective_primary_target ad).model

let effective_primary_provider (ad : agent_defaults) =
  (effective_primary_target ad).provider

let expand_home path =
  if String.length path >= 2 && String.sub path 0 2 = "~/" then
    let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
    Filename.concat home (String.sub path 2 (String.length path - 2))
  else if path = "~" then try Sys.getenv "HOME" with Not_found -> "/tmp"
  else path

let effective_workspace (cfg : t) =
  let path = expand_home cfg.workspace in
  if path = "" then default_workspace () else path

let to_json (cfg : t) : Yojson.Safe.t =
  let provider_json (p : provider_config) =
    let fields = [ ("api_key", `String p.api_key) ] in
    let fields =
      match p.kind with
      | Some kind -> fields @ [ ("kind", `String kind) ]
      | None -> fields
    in
    let fields =
      match p.base_url with
      | Some url -> fields @ [ ("base_url", `String url) ]
      | None -> fields
    in
    let fields =
      match p.default_model with
      | Some m -> fields @ [ ("default_model", `String m) ]
      | None -> fields
    in
    let fields =
      match p.service_account_json with
      | Some saj -> fields @ [ ("service_account_json", `String saj) ]
      | None -> fields
    in
    let fields =
      match p.project_id with
      | Some project_id -> fields @ [ ("project_id", `String project_id) ]
      | None -> fields
    in
    let fields =
      match p.location with
      | Some location -> fields @ [ ("location", `String location) ]
      | None -> fields
    in
    let fields =
      match p.thinking_budget_tokens with
      | Some budget -> fields @ [ ("thinking_budget_tokens", `Int budget) ]
      | None -> fields
    in
    let fields =
      if p.oai_thinking_style <> "none" then
        fields @ [ ("oai_thinking_style", `String p.oai_thinking_style) ]
      else fields
    in
    let fields =
      match p.codex_oauth with
      | None -> fields
      | Some creds ->
          let oauth_fields =
            [
              ("access_token", `String creds.access_token);
              ("refresh_token", `String creds.refresh_token);
              ("expires_at_ms", `Int creds.expires_at_ms);
            ]
            @ (match creds.account_id with
              | Some account_id -> [ ("account_id", `String account_id) ]
              | None -> [])
            @
            match creds.email with
            | Some email -> [ ("email", `String email) ]
            | None -> []
          in
          fields @ [ ("codex_oauth", `Assoc oauth_fields) ]
    in
    `Assoc fields
  in
  let ad = cfg.agent_defaults in
  let prompt = cfg.prompt in
  let telegram_json =
    match cfg.channels.telegram with
    | None -> `Null
    | Some tg ->
        `Assoc
          [
            ( "accounts",
              `Assoc
                (List.map
                   (fun (name, (acct : telegram_account)) ->
                     ( name,
                       `Assoc
                         ([
                            ("bot_token", `String acct.bot_token);
                            ( "allow_from",
                              `List
                                (List.map (fun s -> `String s) acct.allow_from)
                            );
                          ]
                         @
                         match acct.totp with
                         | None -> []
                         | Some t ->
                             [
                               ( "totp",
                                 `Assoc
                                   [
                                     ("enabled", `Bool t.totp_enabled);
                                     ("secret", `String t.totp_secret);
                                     ( "session_ttl_hours",
                                       `Int t.session_ttl_hours );
                                   ] );
                             ]) ))
                   tg.accounts) );
          ]
  in
  let stt_json =
    match cfg.stt with
    | None -> `Null
    | Some s ->
        `Assoc
          ([ ("provider", `String s.provider); ("model", `String s.model) ]
          @
          match s.language with
          | Some l -> [ ("language", `String l) ]
          | None -> [])
  in
  let gateway_fields =
    [
      ("host", `String cfg.gateway.host);
      ("port", `Int cfg.gateway.port);
      ("require_pairing", `Bool cfg.gateway.require_pairing);
      ("max_pair_attempts", `Int cfg.gateway.max_pair_attempts);
      ("pair_lockout_seconds", `Int cfg.gateway.pair_lockout_seconds);
    ]
    @
    match cfg.gateway.auth_token with
    | Some token -> [ ("auth_token", `String token) ]
    | None -> []
  in
  let fields =
    [
      ("workspace", `String cfg.workspace);
      ("default_temperature", `Float cfg.default_temperature);
    ]
  in
  let fields =
    match cfg.default_provider with
    | Some p -> fields @ [ ("default_provider", `String p) ]
    | None -> fields
  in
  let fields =
    fields
    @ [
        ( "providers",
          `Assoc
            (List.map (fun (name, p) -> (name, provider_json p)) cfg.providers)
        );
      ]
  in
  let fields =
    if cfg.model_context_limits = [] then fields
    else
      fields
      @ [
          ( "model_context_limits",
            `Assoc
              (List.map
                 (fun (name, limit) -> (name, `Int limit))
                 cfg.model_context_limits) );
        ]
  in
  let fields =
    fields
    @ [
        ( "agent_defaults",
          `Assoc
            ([
               ("primary_model", `String ad.primary_model);
               ("system_prompt", `String ad.system_prompt);
               ("max_tool_iterations", `Int ad.max_tool_iterations);
               ("tool_search_enabled", `Bool ad.tool_search_enabled);
               ("show_thinking", `Bool ad.show_thinking);
               ("show_tool_calls", `Bool ad.show_tool_calls);
               ("tool_status_mode", `String ad.tool_status_mode);
             ]
            @
            match ad.reasoning_effort with
            | Some re -> [ ("reasoning_effort", `String re) ]
            | None -> []) );
        ( "prompt",
          `Assoc
            [
              ("dynamic_enabled", `Bool prompt.dynamic_enabled);
              ("include_tools_section", `Bool prompt.include_tools_section);
              ("include_safety_section", `Bool prompt.include_safety_section);
              ( "include_workspace_section",
                `Bool prompt.include_workspace_section );
              ("include_runtime_section", `Bool prompt.include_runtime_section);
              ("include_datetime_section", `Bool prompt.include_datetime_section);
              ("include_autonomy_section", `Bool prompt.include_autonomy_section);
              ( "workspace_files",
                `List (List.map (fun f -> `String f) prompt.workspace_files) );
              ("max_workspace_file_chars", `Int prompt.max_workspace_file_chars);
              ( "max_workspace_total_chars",
                `Int prompt.max_workspace_total_chars );
            ] );
        ( "channels",
          `Assoc
            ([ ("cli", `Bool cfg.channels.cli) ]
            @ (match telegram_json with
              | `Null -> []
              | j -> [ ("telegram", j) ])
            @ (match cfg.channels.discord with
              | None -> []
              | Some d ->
                  [
                    ( "discord",
                      `Assoc
                        [
                          ("bot_token", `String d.bot_token);
                          ( "allow_guilds",
                            `List (List.map (fun s -> `String s) d.allow_guilds)
                          );
                          ( "allow_users",
                            `List (List.map (fun s -> `String s) d.allow_users)
                          );
                          ("intents", `Int d.intents);
                        ] );
                  ])
            @ (match cfg.channels.slack with
              | None -> []
              | Some s ->
                  [
                    ( "slack",
                      `Assoc
                        [
                          ("bot_token", `String s.bot_token);
                          ("signing_secret", `String s.signing_secret);
                          ("events_path", `String s.events_path);
                          ( "allow_channels",
                            `List
                              (List.map (fun c -> `String c) s.allow_channels)
                          );
                          ( "allow_users",
                            `List (List.map (fun u -> `String u) s.allow_users)
                          );
                          ("app_token", `String s.app_token);
                          ("socket_mode", `Bool s.socket_mode);
                        ] );
                  ])
            @ (match cfg.channels.github with
              | None -> []
              | Some g ->
                  let auth_json =
                    match g.auth with
                    | GithubPat token ->
                        `Assoc
                          [ ("type", `String "pat"); ("token", `String token) ]
                  in
                  let repos_json =
                    `List
                      (List.map
                         (fun (r : github_repo_config) ->
                           `Assoc
                             ([
                                ("name", `String r.name);
                                ("webhook_secret", `String r.webhook_secret);
                                ("webhook_path", `String r.webhook_path);
                                ( "allow_users",
                                  `List
                                    (List.map
                                       (fun u -> `String u)
                                       r.allow_users) );
                                ( "react_to",
                                  `List
                                    (List.map (fun e -> `String e) r.react_to)
                                );
                                ("include_pr_files", `Bool r.include_pr_files);
                              ]
                             @
                             match r.agent_name with
                             | Some n -> [ ("agent_name", `String n) ]
                             | None -> []))
                         g.repos)
                  in
                  [
                    ( "github",
                      `Assoc [ ("auth", auth_json); ("repos", repos_json) ] );
                  ])
            @ (match cfg.channels.dingtalk with
              | None -> []
              | Some dt ->
                  [
                    ( "dingtalk",
                      `Assoc
                        ([
                           ("app_key", `String dt.app_key);
                           ("app_secret", `String dt.app_secret);
                           ("agent_id", `String dt.agent_id);
                           ( "allow_from",
                             `List (List.map (fun s -> `String s) dt.allow_from)
                           );
                         ]
                        @
                        match dt.webhook_url with
                        | Some url -> [ ("webhook_url", `String url) ]
                        | None -> []) );
                  ])
            @ (match cfg.channels.imessage with
              | None -> []
              | Some im ->
                  [
                    ( "imessage",
                      `Assoc
                        [
                          ("poll_interval_s", `Float im.poll_interval_s);
                          ( "allow_from",
                            `List (List.map (fun s -> `String s) im.allow_from)
                          );
                        ] );
                  ])
            @ (match cfg.channels.signal with
              | None -> []
              | Some sg ->
                  [
                    ( "signal",
                      `Assoc
                        [
                          ("base_url", `String sg.base_url);
                          ("account", `String sg.account);
                          ("api_mode", `String sg.api_mode);
                          ( "allow_from",
                            `List (List.map (fun s -> `String s) sg.allow_from)
                          );
                          ("max_chunk_bytes", `Int sg.max_chunk_bytes);
                        ] );
                  ])
            @ (match cfg.channels.matrix with
              | None -> []
              | Some mx ->
                  [
                    ( "matrix",
                      `Assoc
                        [
                          ("homeserver_url", `String mx.homeserver_url);
                          ("access_token", `String mx.access_token);
                          ("user_id", `String mx.user_id);
                          ( "allow_rooms",
                            `List (List.map (fun s -> `String s) mx.allow_rooms)
                          );
                          ( "allow_users",
                            `List (List.map (fun s -> `String s) mx.allow_users)
                          );
                        ] );
                  ])
            @ (match cfg.channels.irc with
              | None -> []
              | Some ir ->
                  [
                    ( "irc",
                      `Assoc
                        ([
                           ("host", `String ir.host);
                           ("port", `Int ir.port);
                           ("tls", `Bool ir.tls);
                           ("nick", `String ir.nick);
                           ("sasl", `Bool ir.sasl);
                           ( "channels",
                             `List (List.map (fun s -> `String s) ir.channels)
                           );
                           ( "allow_from",
                             `List (List.map (fun s -> `String s) ir.allow_from)
                           );
                         ]
                        @
                        match ir.password with
                        | Some pw -> [ ("password", `String pw) ]
                        | None -> []) );
                  ])
            @ (match cfg.channels.email with
              | None -> []
              | Some em ->
                  [
                    ( "email",
                      `Assoc
                        [
                          ("imap_host", `String em.imap_host);
                          ("imap_port", `Int em.imap_port);
                          ("smtp_host", `String em.smtp_host);
                          ("smtp_port", `Int em.smtp_port);
                          ("username", `String em.username);
                          ("password", `String em.password);
                          ("from_address", `String em.from_address);
                          ( "allow_from",
                            `List (List.map (fun s -> `String s) em.allow_from)
                          );
                          ("poll_interval_s", `Float em.poll_interval_s);
                        ] );
                  ])
            @ (match cfg.channels.whatsapp with
              | None -> []
              | Some wa ->
                  [
                    ( "whatsapp",
                      `Assoc
                        [
                          ("phone_number_id", `String wa.phone_number_id);
                          ("access_token", `String wa.access_token);
                          ("verify_token", `String wa.verify_token);
                          ( "allow_from",
                            `List (List.map (fun s -> `String s) wa.allow_from)
                          );
                        ] );
                  ])
            @ (match cfg.channels.nostr with
              | None -> []
              | Some ns ->
                  [
                    ( "nostr",
                      `Assoc
                        [
                          ( "relays",
                            `List (List.map (fun s -> `String s) ns.relays) );
                          ("private_key", `String ns.private_key);
                          ("pubkey", `String ns.pubkey);
                          ("nak_path", `String ns.nak_path);
                          ( "allow_from",
                            `List (List.map (fun s -> `String s) ns.allow_from)
                          );
                        ] );
                  ])
            @ (match cfg.channels.lark with
              | None -> []
              | Some lk ->
                  [
                    ( "lark",
                      `Assoc
                        [
                          ("enabled", `Bool lk.enabled);
                          ("app_id", `String lk.app_id);
                          ("app_secret", `String lk.app_secret);
                          ("verification_token", `String lk.verification_token);
                          ("endpoint", `String lk.endpoint);
                          ("mode", `String lk.mode);
                          ( "allow_users",
                            `List (List.map (fun s -> `String s) lk.allow_users)
                          );
                        ] );
                  ])
            @ (match cfg.channels.line with
              | None -> []
              | Some ln ->
                  [
                    ( "line",
                      `Assoc
                        [
                          ( "channel_access_token",
                            `String ln.channel_access_token );
                          ("channel_secret", `String ln.channel_secret);
                          ( "allow_from",
                            `List (List.map (fun s -> `String s) ln.allow_from)
                          );
                        ] );
                  ])
            @
            match cfg.channels.onebot with
            | None -> []
            | Some ob ->
                [
                  ( "onebot",
                    `Assoc
                      ([
                         ("ws_url", `String ob.ws_url);
                         ("http_url", `String ob.http_url);
                         ( "allow_from",
                           `List (List.map (fun s -> `String s) ob.allow_from)
                         );
                         ( "allow_groups",
                           `List (List.map (fun s -> `String s) ob.allow_groups)
                         );
                       ]
                      @
                      match ob.access_token with
                      | Some tok -> [ ("access_token", `String tok) ]
                      | None -> []) );
                ]) );
        ("gateway", `Assoc gateway_fields);
        ( "runtime",
          `Assoc
            [
              ("docker_image", `String cfg.runtime.docker_image);
              ( "docker_container_name",
                `String cfg.runtime.docker_container_name );
              ("docker_port", `Int cfg.runtime.docker_port);
            ] );
        ( "tunnel",
          `Assoc
            ([
               ("provider", `String cfg.tunnel.provider);
               ("enabled", `Bool cfg.tunnel.enabled);
               ("managed", `Bool cfg.tunnel.managed);
               ("tunnel_name", `String cfg.tunnel.tunnel_name);
               ("config_dir", `String cfg.tunnel.config_dir);
             ]
            @
            if cfg.tunnel.url <> "" then [ ("url", `String cfg.tunnel.url) ]
            else []) );
        ( "memory",
          `Assoc
            ([
               ("backend", `String cfg.memory.backend);
               ("search_enabled", `Bool cfg.memory.search_enabled);
               ("vector_weight", `Int cfg.memory.vector_weight);
               ("keyword_weight", `Int cfg.memory.keyword_weight);
               ( "compaction_threshold_percent",
                 `Int cfg.memory.compaction_threshold_percent );
               ( "max_messages_per_session",
                 `Int cfg.memory.max_messages_per_session );
               ("max_message_age_days", `Int cfg.memory.max_message_age_days);
             ]
            @ (if cfg.memory.db_path <> "" then
                 [ ("db_path", `String cfg.memory.db_path) ]
               else [])
            @ (match cfg.memory.embedding_model with
              | Some m -> [ ("embedding_model", `String m) ]
              | None -> [])
            @
            match cfg.memory.embedding_provider with
            | Some p -> [ ("embedding_provider", `String p) ]
            | None -> []) );
        ( "security",
          `Assoc
            [
              ("workspace_only", `Bool cfg.security.workspace_only);
              ("audit_enabled", `Bool cfg.security.audit_enabled);
              ("tools_enabled", `Bool cfg.security.tools_enabled);
              ("encrypt_secrets", `Bool cfg.security.encrypt_secrets);
              ( "rate_limit",
                `Assoc
                  [
                    ( "gateway_per_ip_rpm",
                      `Int cfg.security.rate_limit.gateway_per_ip_rpm );
                    ( "gateway_per_session_rpm",
                      `Int cfg.security.rate_limit.gateway_per_session_rpm );
                    ( "telegram_per_chat_rpm",
                      `Int cfg.security.rate_limit.telegram_per_chat_rpm );
                    ( "burst_multiplier",
                      `Float cfg.security.rate_limit.burst_multiplier );
                  ] );
              ( "audit_retention",
                `Assoc
                  [
                    ( "max_age_days",
                      `Int cfg.security.audit_retention.max_age_days );
                    ( "max_entries",
                      `Int cfg.security.audit_retention.max_entries );
                    ( "export_before_purge",
                      `Bool cfg.security.audit_retention.export_before_purge );
                    ( "export_path",
                      `String cfg.security.audit_retention.export_path );
                  ] );
              ("audit_signing_enabled", `Bool cfg.security.audit_signing_enabled);
              ("landlock_enabled", `Bool cfg.security.landlock_enabled);
              ( "landlock_extra_read_paths",
                `List
                  (List.map
                     (fun s -> `String s)
                     cfg.security.landlock_extra_read_paths) );
              ( "extra_allowed_paths",
                `List
                  (List.map
                     (fun s -> `String s)
                     cfg.security.extra_allowed_paths) );
              ("sandbox_backend", `String cfg.security.sandbox_backend);
            ] );
      ]
  in
  let fields =
    match stt_json with `Null -> fields | j -> fields @ [ ("stt", j) ]
  in
  let mcp_fields = [ ("enabled", `Bool cfg.mcp.enabled) ] in
  let mcp_fields =
    match cfg.mcp.exposed_tools with
    | None -> mcp_fields
    | Some tools ->
        mcp_fields
        @ [ ("exposed_tools", `List (List.map (fun s -> `String s) tools)) ]
  in
  let fields = fields @ [ ("mcp", `Assoc mcp_fields) ] in
  let res_fields =
    [
      ("timeout_s", `Float cfg.resilience.timeout_s);
      ("max_retries", `Int cfg.resilience.max_retries);
      ("base_delay_s", `Float cfg.resilience.base_delay_s);
    ]
  in
  let res_fields =
    match cfg.resilience.fallback_provider with
    | Some p -> res_fields @ [ ("fallback_provider", `String p) ]
    | None -> res_fields
  in
  let fields = fields @ [ ("resilience", `Assoc res_fields) ] in
  let fields =
    fields
    @ [
        ( "heartbeat",
          `Assoc
            [
              ("heartbeat_enabled", `Bool cfg.heartbeat.heartbeat_enabled);
              ( "heartbeat_interval_seconds",
                `Int cfg.heartbeat.heartbeat_interval_seconds );
              ("heartbeat_quiet_start", `Int cfg.heartbeat.heartbeat_quiet_start);
              ("heartbeat_quiet_end", `Int cfg.heartbeat.heartbeat_quiet_end);
            ] );
      ]
  in
  let fields =
    match cfg.notify with
    | Some nc ->
        fields
        @ [
            ( "notify",
              `Assoc
                [
                  ("notify_channel", `String nc.notify_channel);
                  ("notify_target", `String nc.notify_target);
                ] );
          ]
    | None -> fields
  in
  let fields =
    match cfg.web_search with
    | Some ws ->
        let ws_fields =
          [
            ("provider", `String ws.search_provider);
            ("api_key", `String ws.search_api_key);
            ("num_results", `Int ws.num_results);
          ]
          @
          match ws.search_base_url with
          | Some u -> [ ("base_url", `String u) ]
          | None -> []
        in
        fields @ [ ("web_search", `Assoc ws_fields) ]
    | None -> fields
  in
  `Assoc fields

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

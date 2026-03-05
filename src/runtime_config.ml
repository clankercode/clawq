type provider_config = {
  api_key : string;
  base_url : string option;
  default_model : string option;
}

type agent_defaults = {
  primary_model : string;
  system_prompt : string;
  max_tool_iterations : int;
}

type telegram_account = { bot_token : string; allow_from : string list }
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

type channel_config = {
  cli : bool;
  telegram : telegram_config option;
  discord : discord_config option;
  slack : slack_config option;
}

type prompt_config = {
  dynamic_enabled : bool;
  include_tools_section : bool;
  include_safety_section : bool;
  include_workspace_section : bool;
  include_runtime_section : bool;
  include_datetime_section : bool;
  workspace_files : string list;
  max_workspace_file_chars : int;
  max_workspace_total_chars : int;
}

type gateway_config = {
  host : string;
  port : int;
  require_pairing : bool;
  auth_token : string option;
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

type t = {
  workspace : string;
  default_temperature : float;
  default_provider : string option;
  providers : (string * provider_config) list;
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
    "BOOTSTRAP.md";
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
    agent_defaults =
      {
        primary_model = "openai/gpt-4o";
        system_prompt = "";
        max_tool_iterations = 10;
      };
    prompt = default_prompt;
    channels = { cli = true; telegram = None; discord = None; slack = None };
    gateway =
      {
        host = "127.0.0.1";
        port = 3000;
        require_pairing = true;
        auth_token = None;
      };
    runtime =
      {
        docker_image = "clawq:latest";
        docker_container_name = "clawq";
        docker_port = 3000;
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
  }

let is_key_set key =
  key <> "" && not (String.length key > 4 && String.sub key 0 4 = "YOUR")

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
      match p.base_url with
      | Some url -> fields @ [ ("base_url", `String url) ]
      | None -> fields
    in
    let fields =
      match p.default_model with
      | Some m -> fields @ [ ("default_model", `String m) ]
      | None -> fields
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
                         [
                           ("bot_token", `String acct.bot_token);
                           ( "allow_from",
                             `List
                               (List.map (fun s -> `String s) acct.allow_from)
                           );
                         ] ))
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
        ( "agent_defaults",
          `Assoc
            [
              ("primary_model", `String ad.primary_model);
              ("system_prompt", `String ad.system_prompt);
              ("max_tool_iterations", `Int ad.max_tool_iterations);
            ] );
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
            @
            match cfg.channels.slack with
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
                          `List (List.map (fun c -> `String c) s.allow_channels)
                        );
                        ( "allow_users",
                          `List (List.map (fun u -> `String u) s.allow_users) );
                        ("app_token", `String s.app_token);
                        ("socket_mode", `Bool s.socket_mode);
                      ] );
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
            [
              ("provider", `String cfg.tunnel.provider);
              ("enabled", `Bool cfg.tunnel.enabled);
              ("url", `String cfg.tunnel.url);
              ("managed", `Bool cfg.tunnel.managed);
              ("tunnel_name", `String cfg.tunnel.tunnel_name);
              ("config_dir", `String cfg.tunnel.config_dir);
            ] );
        ( "memory",
          `Assoc
            ([
               ("backend", `String cfg.memory.backend);
               ("search_enabled", `Bool cfg.memory.search_enabled);
               ("vector_weight", `Int cfg.memory.vector_weight);
               ("keyword_weight", `Int cfg.memory.keyword_weight);
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

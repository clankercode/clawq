type provider_config = { api_key : string; base_url : string option }

type agent_defaults = {
  primary_model : string;
  system_prompt : string;
  max_tool_iterations : int;
}

type telegram_account = { bot_token : string; allow_from : string list }

type telegram_config = { accounts : (string * telegram_account) list }

type channel_config = { cli : bool; telegram : telegram_config option }

type gateway_config = { host : string; port : int; require_pairing : bool }

type memory_config = { backend : string; search_enabled : bool; db_path : string }

type security_config = {
  workspace_only : bool;
  audit_enabled : bool;
  tools_enabled : bool;
  encrypt_secrets : bool;
}

type stt_config = {
  provider : string;
  model : string;
  language : string option;
}

type t = {
  default_temperature : float;
  default_provider : string option;
  providers : (string * provider_config) list;
  agent_defaults : agent_defaults;
  channels : channel_config;
  gateway : gateway_config;
  memory : memory_config;
  security : security_config;
  stt : stt_config option;
}

let default =
  {
    default_temperature = 0.7;
    default_provider = None;
    providers = [];
    agent_defaults = {
      primary_model = "openai/gpt-4o";
      system_prompt = "You are clawq, a helpful AI assistant. Answer questions clearly and concisely.";
      max_tool_iterations = 10;
    };
    channels = { cli = true; telegram = None };
    gateway = { host = "127.0.0.1"; port = 3000; require_pairing = false };
    memory = { backend = "sqlite"; search_enabled = false; db_path = "" };
    security = { workspace_only = true; audit_enabled = false; tools_enabled = true; encrypt_secrets = false };
    stt = None;
  }

let is_key_set key =
  key <> "" && not (String.length key > 4 && String.sub key 0 4 = "YOUR")

let to_json (cfg : t) : Yojson.Safe.t =
  let opt_string = function Some s -> `String s | None -> `Null in
  let provider_json (p : provider_config) =
    let fields = [ ("api_key", `String p.api_key) ] in
    let fields = match p.base_url with
      | Some url -> fields @ [ ("base_url", `String url) ]
      | None -> fields
    in
    `Assoc fields
  in
  let ad = cfg.agent_defaults in
  let telegram_json = match cfg.channels.telegram with
    | None -> `Null
    | Some tg ->
      `Assoc [ ("accounts",
        `Assoc (List.map (fun (name, (acct : telegram_account)) ->
          (name, `Assoc [
            ("bot_token", `String acct.bot_token);
            ("allow_from", `List (List.map (fun s -> `String s) acct.allow_from));
          ])) tg.accounts)) ]
  in
  let stt_json = match cfg.stt with
    | None -> `Null
    | Some s ->
      `Assoc ([ ("provider", `String s.provider); ("model", `String s.model) ]
              @ (match s.language with Some l -> [ ("language", `String l) ] | None -> []))
  in
  let fields = [
    ("default_temperature", `Float cfg.default_temperature);
  ] in
  let fields = match cfg.default_provider with
    | Some p -> fields @ [ ("default_provider", `String p) ]
    | None -> fields
  in
  let fields = fields @ [
    ("providers", `Assoc (List.map (fun (name, p) -> (name, provider_json p)) cfg.providers));
    ("agent_defaults", `Assoc [
      ("primary_model", `String ad.primary_model);
      ("system_prompt", `String ad.system_prompt);
      ("max_tool_iterations", `Int ad.max_tool_iterations);
    ]);
    ("channels", `Assoc (
      [ ("cli", `Bool cfg.channels.cli) ]
      @ (match telegram_json with `Null -> [] | j -> [ ("telegram", j) ])
    ));
    ("gateway", `Assoc [
      ("host", `String cfg.gateway.host);
      ("port", `Int cfg.gateway.port);
      ("require_pairing", `Bool cfg.gateway.require_pairing);
    ]);
    ("memory", `Assoc ([
      ("backend", `String cfg.memory.backend);
      ("search_enabled", `Bool cfg.memory.search_enabled);
    ] @ (if cfg.memory.db_path <> "" then [ ("db_path", `String cfg.memory.db_path) ] else [])));
    ("security", `Assoc [
      ("workspace_only", `Bool cfg.security.workspace_only);
      ("audit_enabled", `Bool cfg.security.audit_enabled);
      ("tools_enabled", `Bool cfg.security.tools_enabled);
      ("encrypt_secrets", `Bool cfg.security.encrypt_secrets);
    ]);
  ] in
  let fields = match stt_json with
    | `Null -> fields
    | j -> fields @ [ ("stt", j) ]
  in
  ignore opt_string;
  `Assoc fields

let merge_with_coq (coq_cfg : Clawq_core.clawqConfig) (cfg : t) : t =
  let gw = coq_cfg.config_gateway in
  let mem = coq_cfg.config_memory in
  let sec = coq_cfg.config_security in
  {
    cfg with
    default_temperature =
      float_of_int coq_cfg.config_default_temperature /. 100.0;
    agent_defaults = {
      cfg.agent_defaults with
      primary_model = coq_cfg.config_default_model;
    };
    gateway =
      {
        host = gw.gateway_host;
        port = gw.gateway_port;
        require_pairing = gw.gateway_require_pairing;
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
      };
  }

type provider_config = { api_key : string; base_url : string option }

type agent_defaults = { primary_model : string }

type telegram_account = { bot_token : string; allow_from : string list }

type telegram_config = { accounts : (string * telegram_account) list }

type channel_config = { cli : bool; telegram : telegram_config option }

type gateway_config = { host : string; port : int; require_pairing : bool }

type memory_config = { backend : string; search_enabled : bool }

type security_config = { workspace_only : bool; audit_enabled : bool }

type stt_config = {
  provider : string;
  model : string;
  language : string option;
}

type t = {
  default_temperature : float;
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
    providers = [];
    agent_defaults = { primary_model = "openai/gpt-4o" };
    channels = { cli = true; telegram = None };
    gateway = { host = "127.0.0.1"; port = 3000; require_pairing = false };
    memory = { backend = "sqlite"; search_enabled = false };
    security = { workspace_only = true; audit_enabled = false };
    stt = None;
  }

let merge_with_coq (coq_cfg : Clawq_core.clawqConfig) (cfg : t) : t =
  let gw = coq_cfg.config_gateway in
  let mem = coq_cfg.config_memory in
  let sec = coq_cfg.config_security in
  {
    cfg with
    default_temperature =
      float_of_int coq_cfg.config_default_temperature /. 100.0;
    agent_defaults = { primary_model = coq_cfg.config_default_model };
    gateway =
      {
        host = gw.gateway_host;
        port = gw.gateway_port;
        require_pairing = gw.gateway_require_pairing;
      };
    memory =
      {
        backend = mem.memory_backend;
        search_enabled = mem.memory_search_enabled;
      };
    security =
      {
        workspace_only = sec.security_workspace_only_cfg;
        audit_enabled = sec.security_audit_enabled_cfg;
      };
  }

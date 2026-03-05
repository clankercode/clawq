val add : int -> int -> int
val eqb : int -> int -> bool

type command =
  | CmdAgent
  | CmdOnboard
  | CmdStatus
  | CmdDoctor
  | CmdCron
  | CmdChannel
  | CmdSkills
  | CmdHardware
  | CmdMigrate
  | CmdService
  | CmdModels
  | CmdMemory
  | CmdWorkspace
  | CmdCapabilities
  | CmdAuth
  | CmdVersion
  | CmdHelp
  | CmdUnknown

val parse_command : string -> command
val usage : string
val dispatch : string list -> string

type gatewayConfig = {
  gateway_host : string;
  gateway_port : int;
  gateway_require_pairing : bool;
}

type memoryConfig = {
  memory_backend : string;
  memory_search_enabled : bool;
  memory_vector_weight : int;
  memory_keyword_weight : int;
}

type securityConfig = {
  security_workspace_only_cfg : bool;
  security_audit_enabled_cfg : bool;
  security_encrypt_secrets_cfg : bool;
}

type clawqConfig = {
  config_default_temperature : int;
  config_default_model : string;
  config_gateway : gatewayConfig;
  config_memory : memoryConfig;
  config_security : securityConfig;
}

val default_gateway_config : gatewayConfig
val default_memory_config : memoryConfig
val default_security_config : securityConfig
val default_config : clawqConfig
val valid_weights : memoryConfig -> bool
val validate_config : clawqConfig -> bool

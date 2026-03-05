(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m = match l with [] -> m | a :: l1 -> a :: app l1 m

(** val rev : 'a1 list -> 'a1 list **)

let rec rev = function [] -> [] | x :: l' -> app (rev l') (x :: [])

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

(** val parse_command : string -> command **)

let parse_command s =
  if s = "agent" then CmdAgent
  else if s = "onboard" then CmdOnboard
  else if s = "status" then CmdStatus
  else if s = "doctor" then CmdDoctor
  else if s = "cron" then CmdCron
  else if s = "channel" then CmdChannel
  else if s = "skills" then CmdSkills
  else if s = "hardware" then CmdHardware
  else if s = "migrate" then CmdMigrate
  else if s = "service" then CmdService
  else if s = "models" then CmdModels
  else if s = "memory" then CmdMemory
  else if s = "workspace" then CmdWorkspace
  else if s = "capabilities" then CmdCapabilities
  else if s = "auth" then CmdAuth
  else if s = "version" then CmdVersion
  else if s = "help" then CmdHelp
  else CmdUnknown

(** val usage : string **)

let usage =
  "Usage: clawq <command>\\n"
  ^ "Commands: onboard, agent, status, doctor, cron, channel, skills, \
     hardware,\\n"
  ^ "          migrate, service, models, memory, workspace, capabilities, \
     auth\\n"

(** val dispatch : string list -> string **)

let dispatch = function
  | [] -> usage
  | cmd :: _ -> (
      match parse_command cmd with
      | CmdAgent -> "agent: TODO (MVP command skeleton wired)"
      | CmdOnboard -> "onboard: TODO (MVP command skeleton wired)"
      | CmdStatus -> "status: TODO (MVP command skeleton wired)"
      | CmdDoctor -> "doctor: TODO (MVP command skeleton wired)"
      | CmdCron -> "cron: TODO (MVP command skeleton wired)"
      | CmdChannel -> "channel: TODO (MVP command skeleton wired)"
      | CmdSkills -> "skills: TODO (MVP command skeleton wired)"
      | CmdHardware -> "hardware: deferred in part (phase 2 peripherals)"
      | CmdMigrate -> "migrate: TODO (MVP command skeleton wired)"
      | CmdService -> "service: TODO (MVP command skeleton wired)"
      | CmdModels -> "models: TODO (MVP command skeleton wired)"
      | CmdMemory -> "memory: TODO (MVP command skeleton wired)"
      | CmdWorkspace -> "workspace: TODO (MVP command skeleton wired)"
      | CmdCapabilities -> "capabilities: TODO (MVP command skeleton wired)"
      | CmdAuth -> "auth: TODO (MVP command skeleton wired)"
      | CmdVersion -> "clawq 0.1.0-dev"
      | CmdHelp -> usage
      | CmdUnknown -> "unknown command\\n" ^ usage)

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

(** val default_config : clawqConfig **)

let default_config =
  {
    config_default_temperature = 70;
    config_default_model = "openai/gpt-4.1";
    config_gateway =
      {
        gateway_host = "127.0.0.1";
        gateway_port = 3000;
        gateway_require_pairing = true;
      };
    config_memory =
      {
        memory_backend = "sqlite";
        memory_search_enabled = true;
        memory_vector_weight = 70;
        memory_keyword_weight = 30;
      };
    config_security =
      {
        security_workspace_only_cfg = true;
        security_audit_enabled_cfg = true;
        security_encrypt_secrets_cfg = true;
      };
  }

(** val valid_weights : memoryConfig -> bool **)

let valid_weights =
 fun m -> m.memory_vector_weight + m.memory_keyword_weight = 100

(** val validate_config : clawqConfig -> bool **)

let validate_config cfg = valid_weights cfg.config_memory

(** val valid_port : int -> bool **)

let valid_port = fun n -> 1 <= n && n <= 65535

(** val valid_temperature : int -> bool **)

let valid_temperature = fun t -> t <= 200

(** val validate_config_full : clawqConfig -> bool **)

let validate_config_full cfg =
  (valid_weights cfg.config_memory && valid_port cfg.config_gateway.gateway_port)
  && valid_temperature cfg.config_default_temperature

(** val norm_acc : string list -> string list -> string list **)

let rec norm_acc acc = function
  | [] -> rev acc
  | s :: rest ->
      if s = "" then norm_acc acc rest
      else if s = "." then norm_acc acc rest
      else if s = ".." then
        match acc with
        | [] -> norm_acc [] rest
        | _ :: acc' -> norm_acc acc' rest
      else norm_acc (s :: acc) rest

(** val normalize : string list -> string list **)

let normalize segs = norm_acc [] segs

(** val is_prefix : string list -> string list -> bool **)

let rec is_prefix pre xs =
  match pre with
  | [] -> true
  | h1 :: t1 -> (
      match xs with [] -> false | h2 :: t2 -> h1 = h2 && is_prefix t1 t2)

(** val is_path_safe_segs : string list -> string list -> bool **)

let is_path_safe_segs workspace resolved =
  is_prefix (normalize workspace) (normalize resolved)

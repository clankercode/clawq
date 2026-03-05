
val negb : bool -> bool

val app : 'a1 list -> 'a1 list -> 'a1 list

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

module Pos :
 sig
  val succ : positive -> positive

  val of_succ_nat : int -> positive
 end

module N :
 sig
  val of_nat : int -> n
 end

val rev : 'a1 list -> 'a1 list

val existsb : ('a1 -> bool) -> 'a1 list -> bool

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

type gatewayConfig = { gateway_host : string; gateway_port : int;
                       gateway_require_pairing : bool }

type memoryConfig = { memory_backend : string; memory_search_enabled : 
                      bool; memory_vector_weight : int;
                      memory_keyword_weight : int }

type securityConfig = { security_workspace_only_cfg : bool;
                        security_audit_enabled_cfg : bool;
                        security_encrypt_secrets_cfg : bool }

type clawqConfig = { config_default_temperature : int;
                     config_default_model : string;
                     config_gateway : gatewayConfig;
                     config_memory : memoryConfig;
                     config_security : securityConfig }

val default_config : clawqConfig

val valid_weights : memoryConfig -> bool

val validate_config : clawqConfig -> bool

val valid_port : int -> bool

val valid_temperature : int -> bool

val validate_config_full : clawqConfig -> bool

val norm_acc : string list -> string list -> string list

val normalize : string list -> string list

val is_prefix : string list -> string list -> bool

val is_path_safe_segs : string list -> string list -> bool

val is_metachar : char -> bool

val string_has_metachar : string -> bool

val is_shell_safe : string -> bool

val char_backslash : char

val char_sq : char

val char_dq : char

val char_space : char

val char_tab : char

val is_whitespace : char -> bool

val is_quote_char : char -> bool

val list_of_string : string -> char list

val string_of_chars : char list -> string

type quote_state =
| NoQuote
| InQuote of char

val flush_word : char list -> string list -> string list

val parse_chars :
  char list -> char list -> string list -> quote_state -> string list option

val split_words : string -> string list option

val is_allowed : string -> string list -> bool

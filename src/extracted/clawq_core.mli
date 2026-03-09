
val negb : bool -> bool

val length : 'a1 list -> int

val app : 'a1 list -> 'a1 list -> 'a1 list

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

module Nat :
 sig
  val ltb : int -> int -> bool
 end

module Pos :
 sig
  val succ : positive -> positive

  val of_succ_nat : int -> positive
 end

module N :
 sig
  val of_nat : int -> n
 end

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val firstn : int -> 'a1 list -> 'a1 list

val rev : 'a1 list -> 'a1 list

val existsb : ('a1 -> bool) -> 'a1 list -> bool

val filter : ('a1 -> bool) -> 'a1 list -> 'a1 list

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

val is_allowed0 : string -> string list -> bool

module ConcreteCrypto :
 sig
  val hash : string -> string

  val hmac : string -> string -> string

  val encode_signed_field : string -> string
 end

val hash0 : string -> string

val hmac0 : string -> string -> string

val encode_signed_field0 : string -> string

type audit_entry = { ae_timestamp : string; ae_event_type : string;
                     ae_session_key : string option; ae_details : string;
                     ae_tool_name : string option;
                     ae_risk_level : string option; ae_signature : string;
                     ae_prev_hash : string }

val field_text : string option -> string

val compute_prev_hash : string option -> string

val compute_signature :
  string -> string -> string -> string -> string option -> string -> string
  option -> string option -> string

val make_entry :
  string -> string option -> string -> string -> string option -> string ->
  string option -> string option -> audit_entry

val verify_link : string -> string option -> audit_entry -> bool

val verify_chain : string -> string option -> audit_entry list -> bool

module AgentLoop :
 sig
  type tool_call = { tc_id : string; tc_name : string }

  val tc_id : tool_call -> string

  type message =
  | UserMsg of string
  | AssistantMsg of string
  | AssistantToolCallsMsg of tool_call list
  | ToolResultMsg of string * string

  type history = message list

  val string_in : string -> string list -> bool

  val trim_history : int -> history -> history

  val force_compress_history : int -> history -> history

  val collect_tool_call_ids : history -> string list

  val collect_tool_result_ids : history -> string list

  val filter_tool_calls_with_results :
    string list -> tool_call list -> tool_call list

  val sanitize_tool_result_with_calls :
    string list -> message -> message option

  val sanitize_assistant_calls_with_results :
    string list -> message -> message

  val option_filter_map : ('a1 -> 'a2 option) -> 'a1 list -> 'a2 list

  val ensure_tool_group_integrity : history -> history

  val adjust_split_for_tool_groups : history -> history -> history * history
 end


(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

module Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH

  (** val of_succ_nat : int -> positive **)

  let rec of_succ_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> XH)
      (fun x -> succ (of_succ_nat x))
      n0
 end

module N =
 struct
  (** val of_nat : int -> n **)

  let of_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> N0)
      (fun n' -> Npos (Pos.of_succ_nat n'))
      n0
 end

(** val rev : 'a1 list -> 'a1 list **)

let rec rev = function
| [] -> []
| x :: l' -> app (rev l') (x :: [])

(** val existsb : ('a1 -> bool) -> 'a1 list -> bool **)

let rec existsb f = function
| [] -> false
| a :: l0 -> (||) (f a) (existsb f l0)

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
  if (=) s "agent"
  then CmdAgent
  else if (=) s "onboard"
       then CmdOnboard
       else if (=) s "status"
            then CmdStatus
            else if (=) s "doctor"
                 then CmdDoctor
                 else if (=) s "cron"
                      then CmdCron
                      else if (=) s "channel"
                           then CmdChannel
                           else if (=) s "skills"
                                then CmdSkills
                                else if (=) s "hardware"
                                     then CmdHardware
                                     else if (=) s "migrate"
                                          then CmdMigrate
                                          else if (=) s "service"
                                               then CmdService
                                               else if (=) s "models"
                                                    then CmdModels
                                                    else if (=) s "memory"
                                                         then CmdMemory
                                                         else if (=) s
                                                                   "workspace"
                                                              then CmdWorkspace
                                                              else if 
                                                                    (=) s
                                                                    "capabilities"
                                                                   then 
                                                                    CmdCapabilities
                                                                   else 
                                                                    if 
                                                                    (=) s
                                                                    "auth"
                                                                    then 
                                                                    CmdAuth
                                                                    else 
                                                                    if 
                                                                    (=) s
                                                                    "version"
                                                                    then 
                                                                    CmdVersion
                                                                    else 
                                                                    if 
                                                                    (=) s
                                                                    "help"
                                                                    then 
                                                                    CmdHelp
                                                                    else 
                                                                    CmdUnknown

(** val usage : string **)

let usage =
  (^) "Usage: clawq <command>\\n"
    ((^)
      "Commands: onboard, agent, status, doctor, cron, channel, skills, hardware,\\n"
      "          migrate, service, models, memory, workspace, capabilities, auth\\n")

(** val dispatch : string list -> string **)

let dispatch = function
| [] -> usage
| cmd :: _ ->
  (match parse_command cmd with
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
   | CmdUnknown -> (^) "unknown command\\n" usage)

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

(** val default_config : clawqConfig **)

let default_config = { config_default_temperature = 70;
     config_default_model = "openai/gpt-4.1";
     config_gateway =
       { gateway_host = "127.0.0.1"; gateway_port = 3000;
         gateway_require_pairing = true };
     config_memory =
       { memory_backend = "sqlite"; memory_search_enabled = true;
         memory_vector_weight = 70; memory_keyword_weight = 30 };
     config_security =
       { security_workspace_only_cfg = true;
         security_audit_enabled_cfg = true;
         security_encrypt_secrets_cfg = true } }

(** val valid_weights : memoryConfig -> bool **)

let valid_weights = fun m -> m.memory_vector_weight + m.memory_keyword_weight = 100

(** val validate_config : clawqConfig -> bool **)

let validate_config cfg =
  valid_weights cfg.config_memory

(** val valid_port : int -> bool **)

let valid_port = fun n -> 1 <= n && n <= 65535

(** val valid_temperature : int -> bool **)

let valid_temperature = fun t -> t <= 200

(** val validate_config_full : clawqConfig -> bool **)

let validate_config_full cfg =
  (&&)
    ((&&) (valid_weights cfg.config_memory)
      (valid_port cfg.config_gateway.gateway_port))
    (valid_temperature cfg.config_default_temperature)

(** val norm_acc : string list -> string list -> string list **)

let rec norm_acc acc = function
| [] -> rev acc
| s :: rest ->
  if (=) s ""
  then norm_acc acc rest
  else if (=) s "."
       then norm_acc acc rest
       else if (=) s ".."
            then (match acc with
                  | [] -> norm_acc [] rest
                  | _ :: acc' -> norm_acc acc' rest)
            else norm_acc (s :: acc) rest

(** val normalize : string list -> string list **)

let normalize segs =
  norm_acc [] segs

(** val is_prefix : string list -> string list -> bool **)

let rec is_prefix pre xs =
  match pre with
  | [] -> true
  | h1 :: t1 ->
    (match xs with
     | [] -> false
     | h2 :: t2 -> (&&) ((=) h1 h2) (is_prefix t1 t2))

(** val is_path_safe_segs : string list -> string list -> bool **)

let is_path_safe_segs workspace resolved =
  is_prefix (normalize workspace) (normalize resolved)

(** val is_metachar : char -> bool **)

let is_metachar c =
  (||)
    ((||)
      ((||)
        ((||)
          ((||)
            ((||)
              ((||) ((||) ((||) ((=) c ';') ((=) c '|')) ((=) c '&'))
                ((=) c '>')) ((=) c '<')) ((=) c '`')) ((=) c '$'))
        ((=) c '!'))
      ((=) c
        (Char.chr (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
          (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))
    ((=) c
      (Char.chr (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))

(** val string_has_metachar : string -> bool **)

let rec string_has_metachar s =
  (* If this appears, you're using String internals. Please don't *)
 (fun f0 f1 s ->
    let l = String.length s in
    if l = 0 then f0 () else f1 (String.get s 0) (String.sub s 1 (l-1)))

    (fun _ -> false)
    (fun c rest -> if is_metachar c then true else string_has_metachar rest)
    s

(** val is_shell_safe : string -> bool **)

let is_shell_safe s =
  negb (string_has_metachar s)

(** val char_backslash : char **)

let char_backslash =
  Char.chr (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ
    0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val char_sq : char **)

let char_sq =
  Char.chr (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))))))))))))))))

(** val char_dq : char **)

let char_dq =
  Char.chr (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))))))))))

(** val char_space : char **)

let char_space =
  Char.chr (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))))))))))))))

(** val char_tab : char **)

let char_tab =
  Char.chr (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))

(** val is_whitespace : char -> bool **)

let is_whitespace c =
  (||) ((=) c char_space) ((=) c char_tab)

(** val is_quote_char : char -> bool **)

let is_quote_char c =
  (||) ((=) c char_sq) ((=) c char_dq)

(** val list_of_string : string -> char list **)

let rec list_of_string s =
  (* If this appears, you're using String internals. Please don't *)
 (fun f0 f1 s ->
    let l = String.length s in
    if l = 0 then f0 () else f1 (String.get s 0) (String.sub s 1 (l-1)))

    (fun _ -> [])
    (fun c rest -> c :: (list_of_string rest))
    s

(** val string_of_chars : char list -> string **)

let rec string_of_chars = function
| [] -> ""
| c :: rest ->
  (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

    (c, (string_of_chars rest))

type quote_state =
| NoQuote
| InQuote of char

(** val flush_word : char list -> string list -> string list **)

let flush_word cur words =
  match cur with
  | [] -> words
  | _ :: _ -> (string_of_chars cur) :: words

(** val parse_chars :
    char list -> char list -> string list -> quote_state -> string list option **)

let rec parse_chars chars cur words q =
  match chars with
  | [] ->
    (match q with
     | NoQuote -> Some (rev (flush_word cur words))
     | InQuote _ -> None)
  | c :: rest ->
    (match q with
     | NoQuote ->
       if is_whitespace c
       then parse_chars rest [] (flush_word cur words) NoQuote
       else if is_quote_char c
            then parse_chars rest cur words (InQuote c)
            else if (=) c char_backslash
                 then (match rest with
                       | [] ->
                         Some (rev (flush_word (app cur (c :: [])) words))
                       | next :: rest' ->
                         parse_chars rest' (app cur (next :: [])) words
                           NoQuote)
                 else parse_chars rest (app cur (c :: [])) words NoQuote
     | InQuote q_char ->
       if (=) c q_char
       then parse_chars rest cur words NoQuote
       else if (&&) ((=) c char_backslash) ((=) q_char char_dq)
            then (match rest with
                  | [] -> None
                  | next :: rest' ->
                    parse_chars rest' (app cur (next :: [])) words (InQuote
                      q_char))
            else parse_chars rest (app cur (c :: [])) words (InQuote q_char))

(** val split_words : string -> string list option **)

let split_words s =
  parse_chars (list_of_string s) [] [] NoQuote

(** val is_allowed : string -> string list -> bool **)

let is_allowed cmd allowlist =
  existsb ((=) cmd) allowlist


(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

(** val length : 'a1 list -> int **)

let rec length = function
| [] -> 0
| _ :: l' -> Stdlib.Int.succ (length l')

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

type comparison =
| Eq
| Lt
| Gt

module Nat =
 struct
  (** val ltb : int -> int -> bool **)

  let ltb n0 m =
    (<=) (Stdlib.Int.succ n0) m
 end

module Pos =
 struct
  (** val succ : int -> int **)

  let rec succ = Stdlib.Int.succ

  (** val add : int -> int -> int **)

  let rec add = (+)

  (** val add_carry : int -> int -> int **)

  and add_carry x y =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> (fun p->1+2*p) (add_carry p q))
        (fun q -> (fun p->2*p) (add_carry p q))
        (fun _ -> (fun p->1+2*p) (succ p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> (fun p->2*p) (add_carry p q))
        (fun q -> (fun p->1+2*p) (add p q))
        (fun _ -> (fun p->2*p) (succ p))
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> (fun p->1+2*p) (succ q))
        (fun q -> (fun p->2*p) (succ q))
        (fun _ -> (fun p->1+2*p) 1)
        y)
      x

  (** val pred_double : int -> int **)

  let rec pred_double x =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p -> (fun p->1+2*p) ((fun p->2*p) p))
      (fun p -> (fun p->1+2*p) (pred_double p))
      (fun _ -> 1)
      x

  (** val mul : int -> int -> int **)

  let rec mul = ( * )

  (** val compare_cont : comparison -> int -> int -> comparison **)

  let rec compare_cont = fun c x y -> if x=y then c else if x<y then Lt else Gt

  (** val compare : int -> int -> comparison **)

  let compare = fun x y -> if x=y then Eq else if x<y then Lt else Gt

  (** val of_succ_nat : int -> int **)

  let rec of_succ_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> 1)
      (fun x -> succ (of_succ_nat x))
      n0
 end

module N =
 struct
  (** val of_nat : int -> int **)

  let of_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> 0)
      (fun n' -> (Pos.of_succ_nat n'))
      n0
 end

(** val rev : 'a1 list -> 'a1 list **)

let rec rev = function
| [] -> []
| x :: l' -> app (rev l') (x :: [])

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a :: t -> (f a) :: (map f t)

(** val existsb : ('a1 -> bool) -> 'a1 list -> bool **)

let rec existsb f = function
| [] -> false
| a :: l0 -> (||) (f a) (existsb f l0)

(** val forallb : ('a1 -> bool) -> 'a1 list -> bool **)

let rec forallb f = function
| [] -> true
| a :: l0 -> (&&) (f a) (forallb f l0)

(** val filter : ('a1 -> bool) -> 'a1 list -> 'a1 list **)

let rec filter f = function
| [] -> []
| x :: l0 -> if f x then x :: (filter f l0) else filter f l0

(** val firstn : int -> 'a1 list -> 'a1 list **)

let rec firstn n0 l =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun n1 -> match l with
               | [] -> []
               | a :: l0 -> a :: (firstn n1 l0))
    n0

module Z =
 struct
  (** val double : int -> int **)

  let double x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 0)
      (fun p -> ((fun p->2*p) p))
      (fun p -> (~-) ((fun p->2*p) p))
      x

  (** val succ_double : int -> int **)

  let succ_double x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 1)
      (fun p -> ((fun p->1+2*p) p))
      (fun p -> (~-) (Pos.pred_double p))
      x

  (** val pred_double : int -> int **)

  let pred_double x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> (~-) 1)
      (fun p -> (Pos.pred_double p))
      (fun p -> (~-) ((fun p->1+2*p) p))
      x

  (** val pos_sub : int -> int -> int **)

  let rec pos_sub x y =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> double (pos_sub p q))
        (fun q -> succ_double (pos_sub p q))
        (fun _ -> ((fun p->2*p) p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> pred_double (pos_sub p q))
        (fun q -> double (pos_sub p q))
        (fun _ -> (Pos.pred_double p))
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> (~-) ((fun p->2*p) q))
        (fun q -> (~-) (Pos.pred_double q))
        (fun _ -> 0)
        y)
      x

  (** val add : int -> int -> int **)

  let add = (+)

  (** val opp : int -> int **)

  let opp = (~-)

  (** val sub : int -> int -> int **)

  let sub = (-)

  (** val mul : int -> int -> int **)

  let mul = ( * )

  (** val compare : int -> int -> comparison **)

  let compare = fun x y -> if x=y then Eq else if x<y then Lt else Gt

  (** val leb : int -> int -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true
 end

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
   | CmdCron ->
     "cron: scheduler-backed command available in full runtime; use CLI bridge for list/add/remove/history/runs"
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
   | CmdVersion -> "clawq 0.4.0-dev"
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
       { gateway_host = "127.0.0.1"; gateway_port = 13451;
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

(** val is_allowed0 : string -> string list -> bool **)

let is_allowed0 id allowlist = match allowlist with
| [] -> existsb ((=) id) allowlist
| w :: l ->
  (match l with
   | [] -> if (=) w "*" then true else existsb ((=) id) (w :: [])
   | _ :: _ -> existsb ((=) id) allowlist)

module ConcreteCrypto =
 struct
  (** val hash : string -> string **)

  let hash = fun s -> Digestif.SHA256.(digest_string s |> to_hex)

  (** val hmac : string -> string -> string **)

  let hmac = fun key payload -> Digestif.SHA256.(hmac_string ~key payload |> to_hex)

  (** val encode_signed_field : string -> string **)

  let encode_signed_field = fun value -> Printf.sprintf "%d:%s" (String.length value) value
 end

(** val hash0 : string -> string **)

let hash0 =
  ConcreteCrypto.hash

(** val hmac0 : string -> string -> string **)

let hmac0 =
  ConcreteCrypto.hmac

(** val encode_signed_field0 : string -> string **)

let encode_signed_field0 =
  ConcreteCrypto.encode_signed_field

type audit_entry = { ae_timestamp : string; ae_event_type : string;
                     ae_session_key : string option; ae_details : string;
                     ae_tool_name : string option;
                     ae_risk_level : string option; ae_signature : string;
                     ae_prev_hash : string }

(** val field_text : string option -> string **)

let field_text = function
| Some s -> s
| None -> ""

(** val compute_prev_hash : string option -> string **)

let compute_prev_hash = function
| Some sig0 -> hash0 sig0
| None -> "genesis"

(** val compute_signature :
    string -> string -> string -> string -> string option -> string -> string
    option -> string option -> string **)

let compute_signature key prev_hash timestamp event_type session_key details tool_name0 risk_level0 =
  hmac0 key
    ((^) (encode_signed_field0 prev_hash)
      ((^) "|"
        ((^) (encode_signed_field0 timestamp)
          ((^) "|"
            ((^) (encode_signed_field0 event_type)
              ((^) "|"
                ((^) (encode_signed_field0 (field_text session_key))
                  ((^) "|"
                    ((^) (encode_signed_field0 details)
                      ((^) "|"
                        ((^) (encode_signed_field0 (field_text tool_name0))
                          ((^) "|"
                            (encode_signed_field0 (field_text risk_level0))))))))))))))

(** val make_entry :
    string -> string option -> string -> string -> string option -> string ->
    string option -> string option -> audit_entry **)

let make_entry key prev_sig ts et session_key det tool_name0 risk_level0 =
  let ph = compute_prev_hash prev_sig in
  { ae_timestamp = ts; ae_event_type = et; ae_session_key = session_key;
  ae_details = det; ae_tool_name = tool_name0; ae_risk_level = risk_level0;
  ae_signature =
  (compute_signature key ph ts et session_key det tool_name0 risk_level0);
  ae_prev_hash = ph }

(** val verify_link : string -> string option -> audit_entry -> bool **)

let verify_link key prev_sig entry =
  (&&) ((=) entry.ae_prev_hash (compute_prev_hash prev_sig))
    ((=) entry.ae_signature
      (compute_signature key entry.ae_prev_hash entry.ae_timestamp
        entry.ae_event_type entry.ae_session_key entry.ae_details
        entry.ae_tool_name entry.ae_risk_level))

(** val verify_chain : string -> string option -> audit_entry list -> bool **)

let rec verify_chain key prev_sig = function
| [] -> true
| e :: rest ->
  (&&) (verify_link key prev_sig e)
    (verify_chain key (Some e.ae_signature) rest)

module AgentLoop =
 struct
  type tool_call = { tc_id : string; tc_name : string }

  (** val tc_id : tool_call -> string **)

  let tc_id t =
    t.tc_id

  type message =
  | UserMsg of string
  | AssistantMsg of string
  | AssistantToolCallsMsg of tool_call list
  | ToolResultMsg of string * string

  type history = message list

  (** val string_in : string -> string list -> bool **)

  let string_in id ids =
    existsb ((=) id) ids

  (** val trim_history : int -> history -> history **)

  let trim_history max h =
    if Nat.ltb (length h) max then h else firstn max h

  (** val force_compress_history : int -> history -> history **)

  let force_compress_history =
    firstn

  (** val collect_tool_call_ids : history -> string list **)

  let rec collect_tool_call_ids = function
  | [] -> []
  | m :: rest ->
    (match m with
     | AssistantToolCallsMsg calls ->
       app (map (fun t -> t.tc_id) calls) (collect_tool_call_ids rest)
     | _ -> collect_tool_call_ids rest)

  (** val collect_tool_result_ids : history -> string list **)

  let rec collect_tool_result_ids = function
  | [] -> []
  | m :: rest ->
    (match m with
     | ToolResultMsg (id, _) -> id :: (collect_tool_result_ids rest)
     | _ -> collect_tool_result_ids rest)

  (** val filter_tool_calls_with_results :
      string list -> tool_call list -> tool_call list **)

  let filter_tool_calls_with_results result_ids calls =
    filter (fun call -> string_in call.tc_id result_ids) calls

  (** val sanitize_tool_result_with_calls :
      string list -> message -> message option **)

  let sanitize_tool_result_with_calls call_ids msg = match msg with
  | ToolResultMsg (id, _) -> if string_in id call_ids then Some msg else None
  | _ -> Some msg

  (** val sanitize_assistant_calls_with_results :
      string list -> message -> message **)

  let sanitize_assistant_calls_with_results result_ids msg = match msg with
  | AssistantToolCallsMsg calls ->
    AssistantToolCallsMsg (filter_tool_calls_with_results result_ids calls)
  | _ -> msg

  (** val option_filter_map : ('a1 -> 'a2 option) -> 'a1 list -> 'a2 list **)

  let rec option_filter_map f = function
  | [] -> []
  | x :: rest ->
    (match f x with
     | Some y -> y :: (option_filter_map f rest)
     | None -> option_filter_map f rest)

  (** val ensure_tool_group_integrity : history -> history **)

  let ensure_tool_group_integrity msgs =
    let call_ids = collect_tool_call_ids msgs in
    let result_ids = collect_tool_result_ids msgs in
    map (sanitize_assistant_calls_with_results result_ids)
      (option_filter_map (sanitize_tool_result_with_calls call_ids) msgs)

  (** val adjust_split_for_tool_groups :
      history -> history -> history * history **)

  let rec adjust_split_for_tool_groups to_compact to_keep = match to_keep with
  | [] -> (to_compact, to_keep)
  | m :: rest ->
    (match m with
     | ToolResultMsg (id, name) ->
       adjust_split_for_tool_groups
         (app to_compact ((ToolResultMsg (id, name)) :: [])) rest
     | _ -> (to_compact, to_keep))
 end

module RateLimiter =
 struct
  (** val token_scale : int **)

  let token_scale =
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) 1)))))))))))))))

  (** val one_token : int **)

  let one_token =
    token_scale

  type limiter_config = { rate_per_minute : int; max_tokens : int }

  (** val rate_per_minute : limiter_config -> int **)

  let rate_per_minute l =
    l.rate_per_minute

  (** val max_tokens : limiter_config -> int **)

  let max_tokens l =
    l.max_tokens

  type bucket = { tokens : int; last_refill : int }

  (** val tokens : bucket -> int **)

  let tokens b =
    b.tokens

  (** val last_refill : bucket -> int **)

  let last_refill b =
    b.last_refill

  (** val refill : limiter_config -> bucket -> int -> bucket **)

  let refill cfg b now =
    let elapsed = Z.sub now b.last_refill in
    let added = Z.mul elapsed cfg.rate_per_minute in
    let new_tok = Z.add b.tokens added in
    let capped =
      if Z.leb new_tok cfg.max_tokens then new_tok else cfg.max_tokens
    in
    { tokens = capped; last_refill = now }

  (** val try_consume : limiter_config -> bucket -> int -> bool * bucket **)

  let try_consume cfg b now =
    let b' = refill cfg b now in
    if Z.leb one_token b'.tokens
    then (true, { tokens = (Z.sub b'.tokens one_token); last_refill =
           b'.last_refill })
    else (false, b')
 end

type risk_level =
| Low
| Medium
| High

(** val risk_gte : risk_level -> risk_level -> bool **)

let risk_gte r1 r2 =
  match r1 with
  | Low -> (match r2 with
            | Low -> true
            | _ -> false)
  | Medium -> (match r2 with
               | High -> false
               | _ -> true)
  | High -> true

(** val risk_lte : risk_level -> risk_level -> bool **)

let risk_lte r1 r2 =
  risk_gte r2 r1

type tool_spec = { tool_name : string; tool_risk : risk_level }

(** val is_high_risk : risk_level -> bool **)

let is_high_risk = function
| High -> true
| _ -> false

(** val is_medium_risk : risk_level -> bool **)

let is_medium_risk = function
| Medium -> true
| _ -> false

(** val is_low_risk : risk_level -> bool **)

let is_low_risk = function
| Low -> true
| _ -> false

(** val requires_authorization : tool_spec -> bool **)

let requires_authorization t =
  match t.tool_risk with
  | Low -> false
  | _ -> true

(** val is_authorized : string -> string list -> bool **)

let is_authorized tool authorized =
  existsb ((=) tool) authorized

(** val tool_in_allowlist : string -> string list -> bool **)

let tool_in_allowlist tool allowlist =
  existsb ((=) tool) allowlist

(** val invocation_safe : tool_spec -> string list -> string list -> bool **)

let invocation_safe t allowlist authorized =
  (&&) (tool_in_allowlist t.tool_name allowlist)
    (if requires_authorization t
     then is_authorized t.tool_name authorized
     else true)

(** val shell_exec_tool : tool_spec **)

let shell_exec_tool =
  { tool_name = "shell_exec"; tool_risk = High }

(** val file_read_tool : tool_spec **)

let file_read_tool =
  { tool_name = "file_read"; tool_risk = Low }

(** val file_write_tool : tool_spec **)

let file_write_tool =
  { tool_name = "file_write"; tool_risk = Medium }

(** val file_append_tool : tool_spec **)

let file_append_tool =
  { tool_name = "file_append"; tool_risk = Medium }

(** val valid_tool_config :
    tool_spec -> string list -> string list -> bool **)

let valid_tool_config =
  invocation_safe

(** val all_tools_safe :
    tool_spec list -> string list -> string list -> bool **)

let all_tools_safe tools allowlist authorized =
  forallb (fun t -> valid_tool_config t allowlist authorized) tools

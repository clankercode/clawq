From Coq Require Import Extraction.
From Coq Require Import ExtrOcamlBasic.
From Coq Require Import ExtrOcamlNativeString.
From Coq Require Import ExtrOcamlNatInt.
Require Import Clawq.Cli.
Require Import Clawq.Config.
Require Import Clawq.PathSafety.
Require Import Clawq.QuoteParsing.
Require Import Clawq.ShellSafety.
Require Import Clawq.ChannelAuth.

Extraction Language OCaml.

(* --- Binary size optimizations --- *)

(* Map nat comparison to native OCaml operators.
   ExtrOcamlNatInt maps the type but not these operations. *)
Extract Inlined Constant Nat.eqb => "(=)".
Extract Inlined Constant Nat.leb => "(<=)".
Extract Inlined Constant Nat.ltb => "(<)".

(* Inline tail-recursive nat arithmetic (redundant when nat = int). *)
Extract Inlined Constant Nat.tail_add => "(+)".
Extract Inlined Constant Nat.tail_mul => "( * )".
Extract Inlined Constant Ascii.ascii_of_nat => "Char.chr".

(* Replace the numeral-conversion machinery with native OCaml identity.
   ExtrOcamlNatInt maps nat to int, so of_num_uint just needs to convert
   the decimal digit representation to an int at extraction time.
   By mapping of_num_uint to a function that Coq's extraction will use
   for large nat literals, we avoid the chain of Stdlib.Int.succ calls. *)
Extract Constant Nat.of_num_uint =>
  "fun n ->
     let rec of_uint = function
       | Nil -> 0
       | D0 d -> 10 * of_uint d
       | D1 d -> 1 + 10 * of_uint d
       | D2 d -> 2 + 10 * of_uint d
       | D3 d -> 3 + 10 * of_uint d
       | D4 d -> 4 + 10 * of_uint d
       | D5 d -> 5 + 10 * of_uint d
       | D6 d -> 6 + 10 * of_uint d
       | D7 d -> 7 + 10 * of_uint d
       | D8 d -> 8 + 10 * of_uint d
       | D9 d -> 9 + 10 * of_uint d
     in
     match n with
     | UIntDecimal d -> of_uint d
     | UIntHexadecimal _ -> 0".

(* Inline the intermediate functions that are no longer needed. *)
Extraction Inline Nat.of_uint_acc Nat.of_uint
  Nat.of_hex_uint_acc Nat.of_hex_uint Nat.tail_addmul.

(* Map config defaults with nat literals to avoid successor chains.
   Without these, e.g. gateway_port=3000 becomes 3000 nested
   Stdlib.Int.succ calls in the extracted code. *)
Extract Constant default_gateway_config =>
  "{ gateway_host = ""127.0.0.1""; gateway_port = 3000;
     gateway_require_pairing = true }".
Extract Constant default_memory_config =>
  "{ memory_backend = ""sqlite""; memory_search_enabled = true;
     memory_vector_weight = 70; memory_keyword_weight = 30 }".
Extract Constant default_config =>
  "{ config_default_temperature = 70;
     config_default_model = ""openai/gpt-4.1"";
     config_gateway =
       { gateway_host = ""127.0.0.1""; gateway_port = 3000;
         gateway_require_pairing = true };
     config_memory =
       { memory_backend = ""sqlite""; memory_search_enabled = true;
         memory_vector_weight = 70; memory_keyword_weight = 30 };
     config_security =
       { security_workspace_only_cfg = true;
         security_audit_enabled_cfg = true;
         security_encrypt_secrets_cfg = true } }".

(* Map validation helpers that use nat comparisons on large thresholds *)
Extract Constant valid_port => "fun n -> 1 <= n && n <= 65535".
Extract Constant valid_temperature => "fun t -> t <= 200".
Extract Constant valid_weights =>
  "fun m -> m.memory_vector_weight + m.memory_keyword_weight = 100".
Extraction "src/extracted/clawq_core.ml"
  (* CLI *)
  Clawq.Cli.parse_command
  Clawq.Cli.dispatch
  (* Config: basic *)
  Clawq.Config.validate_config
  Clawq.Config.valid_weights
  Clawq.Config.default_config
  (* Config: extended (F5) *)
  Clawq.Config.valid_port
  Clawq.Config.valid_temperature
  Clawq.Config.validate_config_full
  (* Path safety (F2) *)
  Clawq.PathSafety.norm_acc
  Clawq.PathSafety.normalize
  Clawq.PathSafety.is_prefix
  Clawq.PathSafety.is_path_safe_segs
  (* Shell safety (F6) *)
  Clawq.QuoteParsing.split_words
  Clawq.QuoteParsing.is_shell_safe
  Clawq.ShellSafety.is_allowed
  (* Channel auth (F8) *)
  Clawq.ChannelAuth.is_allowed.

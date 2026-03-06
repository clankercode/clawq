From Coq Require Import String List Bool.
Import ListNotations.
Open Scope string_scope.

Record GatewayConfig := {
  gateway_host : string;
  gateway_port : nat;
  gateway_require_pairing : bool
}.

Record MemoryConfig := {
  memory_backend : string;
  memory_search_enabled : bool;
  memory_vector_weight : nat;
  memory_keyword_weight : nat
}.

Record SecurityConfig := {
  security_workspace_only_cfg : bool;
  security_audit_enabled_cfg : bool;
  security_encrypt_secrets_cfg : bool
}.

Record ClawqConfig := {
  config_default_temperature : nat;
  config_default_model : string;
  config_gateway : GatewayConfig;
  config_memory : MemoryConfig;
  config_security : SecurityConfig
}.

Definition default_gateway_config : GatewayConfig :=
  {| gateway_host := "127.0.0.1";
     gateway_port := 13451;
     gateway_require_pairing := true |}.

Definition default_memory_config : MemoryConfig :=
  {| memory_backend := "sqlite";
     memory_search_enabled := true;
     memory_vector_weight := 70;
     memory_keyword_weight := 30 |}.

Definition default_security_config : SecurityConfig :=
  {| security_workspace_only_cfg := true;
     security_audit_enabled_cfg := true;
     security_encrypt_secrets_cfg := true |}.

Definition default_config : ClawqConfig :=
  {| config_default_temperature := 70;
     config_default_model := "openai/gpt-4.1";
     config_gateway := default_gateway_config;
     config_memory := default_memory_config;
     config_security := default_security_config |}.

Definition valid_weights (m : MemoryConfig) : bool :=
  Nat.eqb (memory_vector_weight m + memory_keyword_weight m) 100.

Definition validate_config (cfg : ClawqConfig) : bool :=
  valid_weights (config_memory cfg).

(* F5: extended validation *)

(* Valid TCP port: 1..65535 *)
Definition valid_port (n : nat) : bool :=
  Nat.leb 1 n && Nat.leb n 65535.

(* Valid temperature stored as nat*100 (e.g. 70 = 0.7): 0..200 *)
Definition valid_temperature (t : nat) : bool :=
  Nat.leb t 200.

Definition validate_config_full (cfg : ClawqConfig) : bool :=
  valid_weights (config_memory cfg)
  && valid_port (gateway_port (config_gateway cfg))
  && valid_temperature (config_default_temperature cfg).

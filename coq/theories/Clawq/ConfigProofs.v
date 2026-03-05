From Coq Require Import String Arith Bool.
Require Import Clawq.Config.

(* ================================================================
   F1: Free-win proofs on existing Config definitions
   F5: Proofs on extended validation (valid_port, valid_temperature)
   ================================================================ *)

(* P1: Default config passes basic validation *)
Theorem default_config_valid :
  validate_config default_config = true.
Proof. reflexivity. Qed.

(* P2: valid_weights iff memory weights sum to 100 *)
Theorem valid_weights_spec : forall m,
  valid_weights m = true <-> memory_vector_weight m + memory_keyword_weight m = 100.
Proof.
  intro m. unfold valid_weights.
  rewrite Nat.eqb_eq. reflexivity.
Qed.

(* P3: validate_config is exactly valid_weights on the memory config *)
Theorem validate_config_unfold : forall cfg,
  validate_config cfg = valid_weights (config_memory cfg).
Proof. reflexivity. Qed.

(* P4: Default memory weights sum to 100 *)
Theorem default_weights_sum :
  memory_vector_weight default_memory_config + memory_keyword_weight default_memory_config = 100.
Proof. reflexivity. Qed.

(* P5: Default security config is fully enabled (secure-by-default) *)
Theorem default_security_all_enabled :
  security_workspace_only_cfg default_security_config = true /\
  security_audit_enabled_cfg default_security_config = true /\
  security_encrypt_secrets_cfg default_security_config = true.
Proof. auto. Qed.

(* P6: validate_config iff weights sum to 100 (combines P2+P3) *)
Theorem validate_config_iff : forall cfg,
  validate_config cfg = true <->
  memory_vector_weight (config_memory cfg) + memory_keyword_weight (config_memory cfg) = 100.
Proof.
  intro cfg. rewrite validate_config_unfold. apply valid_weights_spec.
Qed.

(* P7: Configs sharing the default memory config pass validation *)
Theorem default_memory_validates : forall cfg,
  config_memory cfg = default_memory_config ->
  validate_config cfg = true.
Proof.
  intros cfg H. unfold validate_config. rewrite H. reflexivity.
Qed.

(* ================================================================
   F5: Extended validation proofs
   ================================================================ *)

(* P8: valid_port specification *)
Theorem valid_port_spec : forall n,
  valid_port n = true <-> 1 <= n /\ n <= 65535.
Proof.
  intro n. unfold valid_port.
  rewrite Bool.andb_true_iff.
  rewrite Nat.leb_le. rewrite Nat.leb_le.
  reflexivity.
Qed.

(* P9: Default port is valid *)
Theorem default_port_valid :
  valid_port (gateway_port default_gateway_config) = true.
Proof. reflexivity. Qed.

(* P10: valid_temperature specification *)
Theorem valid_temperature_spec : forall t,
  valid_temperature t = true <-> t <= 200.
Proof.
  intro t. unfold valid_temperature.
  apply Nat.leb_le.
Qed.

(* P11: Default temperature is valid (70 represents 0.7) *)
Theorem default_temperature_valid :
  valid_temperature (config_default_temperature default_config) = true.
Proof. reflexivity. Qed.

(* P12: Default config passes full validation *)
Theorem default_config_valid_full :
  validate_config_full default_config = true.
Proof. reflexivity. Qed.

(* P13: Full validation implies basic validation *)
Theorem validate_config_full_implies_basic : forall cfg,
  validate_config_full cfg = true ->
  validate_config cfg = true.
Proof.
  intros cfg H. unfold validate_config_full in H.
  apply Bool.andb_true_iff in H. destruct H as [H _].
  apply Bool.andb_true_iff in H. destruct H as [H _].
  exact H.
Qed.

(* P14: Full validation implies port is in valid range *)
Theorem valid_config_full_port_range : forall cfg,
  validate_config_full cfg = true ->
  1 <= gateway_port (config_gateway cfg) /\ gateway_port (config_gateway cfg) <= 65535.
Proof.
  intros cfg H. unfold validate_config_full in H.
  apply Bool.andb_true_iff in H. destruct H as [H _].
  apply Bool.andb_true_iff in H. destruct H as [_ Hport].
  apply valid_port_spec. exact Hport.
Qed.

(* P15: Full validation implies temperature is in valid range *)
Theorem valid_config_full_temp_range : forall cfg,
  validate_config_full cfg = true ->
  config_default_temperature cfg <= 200.
Proof.
  intros cfg H. unfold validate_config_full in H.
  apply Bool.andb_true_iff in H. destruct H as [_ Htemp].
  apply valid_temperature_spec. exact Htemp.
Qed.

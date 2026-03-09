From Coq Require Import String List Bool Arith.
Import ListNotations.
Open Scope string_scope.

Require Import Clawq.Config.

(* ================================================================
   F14: Config Merge Semantics

   This module formalizes the config merge behavior where:
   - JSON config is modeled as association lists (key-value pairs)
   - Missing JSON fields retain default config values
   - Security fields have a floor: workspace_only and audit_enabled
     cannot be silently disabled by user config
   - resolve_secret is applied exactly once per API key
   ================================================================ *)

(* JSON value type (simplified) *)
Inductive json_value : Type :=
  | JV_Null : json_value
  | JV_Bool : bool -> json_value
  | JV_Num : nat -> json_value
  | JV_String : string -> json_value.

(* JSON object as association list *)
Definition json_object := list (string * json_value).

(* Secret resolution state: tracks whether a secret has been resolved *)
Inductive secret_state : Type :=
  | SS_Unresolved : string -> secret_state  (* raw secret value *)
  | SS_Resolved : string -> secret_state.   (* resolved secret value *)

(* Provider config with secret state tracking *)
Record ProviderConfig := {
  pc_api_key : secret_state;
  pc_base_url : option string;
  pc_default_model : option string
}.

(* Full config with security and providers *)
Record MergeConfig := {
  mc_default_temperature : nat;
  mc_default_model : string;
  mc_workspace_only : bool;
  mc_audit_enabled : bool;
  mc_providers : list (string * ProviderConfig)
}.

(* Default merge config *)
Definition default_provider_config : ProviderConfig :=
  {| pc_api_key := SS_Unresolved "";
     pc_base_url := None;
     pc_default_model := None |}.

Definition default_merge_config : MergeConfig :=
  {| mc_default_temperature := config_default_temperature default_config;
     mc_default_model := config_default_model default_config;
     mc_workspace_only := security_workspace_only_cfg (config_security default_config);
     mc_audit_enabled := security_audit_enabled_cfg (config_security default_config);
     mc_providers := [] |}.

(* ================================================================
   Lookup operations on JSON objects
   ================================================================ *)

(* Lookup a field in JSON object *)
Fixpoint json_lookup (obj : json_object) (key : string) : option json_value :=
  match obj with
  | [] => None
  | (k, v) :: rest => if String.eqb k key then Some v else json_lookup rest key
  end.

(* Extract bool from JSON value with default *)
Definition json_get_bool (obj : json_object) (key : string) (def : bool) : bool :=
  match json_lookup obj key with
  | Some (JV_Bool b) => b
  | _ => def
  end.

(* Extract nat from JSON value with default *)
Definition json_get_nat (obj : json_object) (key : string) (def : nat) : nat :=
  match json_lookup obj key with
  | Some (JV_Num n) => n
  | _ => def
  end.

(* Extract string from JSON value with default *)
Definition json_get_string (obj : json_object) (key : string) (def : string) : string :=
  match json_lookup obj key with
  | Some (JV_String s) => s
  | _ => def
  end.

(* ================================================================
   Merge operations
   ================================================================ *)

(* Merge security config with floor enforcement *)
Definition merge_security (json : json_object) : bool * bool :=
  let user_workspace_only := json_get_bool json "workspace_only" false in
  let user_audit_enabled := json_get_bool json "audit_enabled" false in
  (* Floor enforcement: cannot disable if default is true *)
  let merged_workspace_only := mc_workspace_only default_merge_config || user_workspace_only in
  let merged_audit_enabled := mc_audit_enabled default_merge_config || user_audit_enabled in
  (merged_workspace_only, merged_audit_enabled).

(* Resolve a secret (applied exactly once) *)
Definition resolve_secret (s : secret_state) : secret_state :=
  match s with
  | SS_Unresolved v => SS_Resolved v
  | SS_Resolved v => SS_Resolved v  (* idempotent if already resolved *)
  end.

(* Merge a single provider config *)
Definition merge_provider (json : json_object) : ProviderConfig :=
  let api_key_raw := json_get_string json "api_key" "" in
  {| pc_api_key := resolve_secret (SS_Unresolved api_key_raw);
     pc_base_url :=
       match json_lookup json "base_url" with
       | Some (JV_String s) => Some s
       | _ => pc_base_url default_provider_config
       end;
     pc_default_model :=
       match json_lookup json "default_model" with
       | Some (JV_String s) => Some s
       | _ => pc_default_model default_provider_config
       end
  |}.

(* Full config merge *)
Definition merge_config (json : json_object) : MergeConfig :=
  let merged_security := merge_security json in
  {| mc_default_temperature :=
       json_get_nat json "default_temperature" (mc_default_temperature default_merge_config);
     mc_default_model :=
       json_get_string json "default_model" (mc_default_model default_merge_config);
     mc_workspace_only := fst merged_security;
     mc_audit_enabled := snd merged_security;
     mc_providers := []  (* simplified for now *)
  |}.

(* ================================================================
   Theorems: Default Preservation
   ================================================================ *)

(* T1: Missing field returns default for bool *)
Theorem json_get_bool_missing : forall obj key def,
  json_lookup obj key = None ->
  json_get_bool obj key def = def.
Proof.
  intros obj key def H.
  unfold json_get_bool. rewrite H. reflexivity.
Qed.

(* T2: Missing field returns default for nat *)
Theorem json_get_nat_missing : forall obj key def,
  json_lookup obj key = None ->
  json_get_nat obj key def = def.
Proof.
  intros obj key def H.
  unfold json_get_nat. rewrite H. reflexivity.
Qed.

(* T3: Missing field returns default for string *)
Theorem json_get_string_missing : forall obj key def,
  json_lookup obj key = None ->
  json_get_string obj key def = def.
Proof.
  intros obj key def H.
  unfold json_get_string. rewrite H. reflexivity.
Qed.

(* T4: Empty JSON object preserves all defaults *)
Theorem empty_json_preserves_defaults : forall json,
  json = [] ->
  let cfg := merge_config json in
  mc_default_temperature cfg = mc_default_temperature default_merge_config /\
  mc_default_model cfg = mc_default_model default_merge_config.
Proof.
  intros json H.
  destruct json as [|].
  - (* json = [] *)
    simpl. split; reflexivity.
  - (* json = _ :: _ - contradiction *)
    discriminate H.
Qed.

(* ================================================================
   Theorems: Security Field Floor
   ================================================================ *)

(* T5: Security floor for workspace_only: cannot be disabled if default is true *)
Theorem security_floor_workspace_only : forall json,
  mc_workspace_only default_merge_config = true ->
  let merged := merge_security json in
  fst merged = true.
Proof.
  intros json Hdefault.
  unfold merge_security.
  destruct (json_get_bool json "workspace_only" false).
  - reflexivity.
  - rewrite Hdefault. reflexivity.
Qed.

(* T6: Security floor for audit_enabled: cannot be disabled if default is true *)
Theorem security_floor_audit_enabled : forall json,
  mc_audit_enabled default_merge_config = true ->
  let merged := merge_security json in
  snd merged = true.
Proof.
  intros json Hdefault.
  unfold merge_security.
  destruct (json_get_bool json "audit_enabled" false).
  - reflexivity.
  - rewrite Hdefault. reflexivity.
Qed.

(* T7: Merged config respects security floor for workspace_only *)
Theorem merge_config_security_floor_workspace : forall json,
  mc_workspace_only default_merge_config = true ->
  mc_workspace_only (merge_config json) = true.
Proof.
  intros json Hdefault.
  unfold merge_config, merge_security.
  destruct (json_get_bool json "workspace_only" false);
    simpl; reflexivity.
Qed.

(* T8: Merged config respects security floor for audit_enabled *)
Theorem merge_config_security_floor_audit : forall json,
  mc_audit_enabled default_merge_config = true ->
  mc_audit_enabled (merge_config json) = true.
Proof.
  intros json Hdefault.
  unfold merge_config, merge_security.
  destruct (json_get_bool json "audit_enabled" false);
    simpl; reflexivity.
Qed.

(* ================================================================
   Theorems: Single Application of resolve_secret
   ================================================================ *)

(* T9: resolve_secret is idempotent on already-resolved secrets *)
Theorem resolve_secret_idempotent : forall s,
  resolve_secret (resolve_secret s) = resolve_secret s.
Proof.
  intros s.
  destruct s; reflexivity.
Qed.

(* T10: resolve_secret changes Unresolved to Resolved *)
Theorem resolve_secret_unresolved : forall v,
  resolve_secret (SS_Unresolved v) = SS_Resolved v.
Proof.
  intros v. reflexivity.
Qed.

(* T11: resolve_secret preserves value *)
Theorem resolve_secret_preserves_value : forall s,
  match s with
  | SS_Unresolved v =>
      match resolve_secret s with SS_Resolved v' => v = v' | _ => False end
  | SS_Resolved v =>
      match resolve_secret s with SS_Resolved v' => v = v' | _ => False end
  end.
Proof.
  intros s.
  destruct s; reflexivity.
Qed.

(* T12: Provider merge applies resolve_secret exactly once to api_key *)
Theorem merge_provider_resolves_once : forall json,
  pc_api_key (merge_provider json) =
  resolve_secret (SS_Unresolved (json_get_string json "api_key" "")).
Proof.
  intros json.
  unfold merge_provider.
  reflexivity.
Qed.

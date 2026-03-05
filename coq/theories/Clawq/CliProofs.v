From Coq Require Import String List Bool.
Import ListNotations.
Open Scope string_scope.
Require Import Clawq.Cli.

(* ================================================================
   F1: Proofs on Cli definitions
   ================================================================ *)

(* --- parse_command correctness for known commands --- *)

Theorem parse_command_agent : parse_command "agent" = CmdAgent.
Proof. reflexivity. Qed.

Theorem parse_command_onboard : parse_command "onboard" = CmdOnboard.
Proof. reflexivity. Qed.

Theorem parse_command_status : parse_command "status" = CmdStatus.
Proof. reflexivity. Qed.

Theorem parse_command_doctor : parse_command "doctor" = CmdDoctor.
Proof. reflexivity. Qed.

Theorem parse_command_cron : parse_command "cron" = CmdCron.
Proof. reflexivity. Qed.

Theorem parse_command_channel : parse_command "channel" = CmdChannel.
Proof. reflexivity. Qed.

Theorem parse_command_skills : parse_command "skills" = CmdSkills.
Proof. reflexivity. Qed.

Theorem parse_command_hardware : parse_command "hardware" = CmdHardware.
Proof. reflexivity. Qed.

Theorem parse_command_migrate : parse_command "migrate" = CmdMigrate.
Proof. reflexivity. Qed.

Theorem parse_command_service : parse_command "service" = CmdService.
Proof. reflexivity. Qed.

Theorem parse_command_models : parse_command "models" = CmdModels.
Proof. reflexivity. Qed.

Theorem parse_command_memory : parse_command "memory" = CmdMemory.
Proof. reflexivity. Qed.

Theorem parse_command_workspace : parse_command "workspace" = CmdWorkspace.
Proof. reflexivity. Qed.

Theorem parse_command_capabilities : parse_command "capabilities" = CmdCapabilities.
Proof. reflexivity. Qed.

Theorem parse_command_auth : parse_command "auth" = CmdAuth.
Proof. reflexivity. Qed.

Theorem parse_command_version : parse_command "version" = CmdVersion.
Proof. reflexivity. Qed.

Theorem parse_command_help : parse_command "help" = CmdHelp.
Proof. reflexivity. Qed.

(* --- parse_command fallback: anything not in the known set -> CmdUnknown --- *)

Theorem parse_command_unknown_fallback : forall s,
  s <> "agent" -> s <> "onboard" -> s <> "status" -> s <> "doctor" ->
  s <> "cron" -> s <> "channel" -> s <> "skills" -> s <> "hardware" ->
  s <> "migrate" -> s <> "service" -> s <> "models" -> s <> "memory" ->
  s <> "workspace" -> s <> "capabilities" -> s <> "auth" -> s <> "version" ->
  s <> "help" ->
  parse_command s = CmdUnknown.
Proof.
  intros s H1 H2 H3 H4 H5 H6 H7 H8 H9 H10 H11 H12 H13 H14 H15 H16 H17.
  apply String.eqb_neq in H1. apply String.eqb_neq in H2.
  apply String.eqb_neq in H3. apply String.eqb_neq in H4.
  apply String.eqb_neq in H5. apply String.eqb_neq in H6.
  apply String.eqb_neq in H7. apply String.eqb_neq in H8.
  apply String.eqb_neq in H9. apply String.eqb_neq in H10.
  apply String.eqb_neq in H11. apply String.eqb_neq in H12.
  apply String.eqb_neq in H13. apply String.eqb_neq in H14.
  apply String.eqb_neq in H15. apply String.eqb_neq in H16.
  apply String.eqb_neq in H17.
  unfold parse_command.
  rewrite H1, H2, H3, H4, H5, H6, H7, H8, H9, H10,
          H11, H12, H13, H14, H15, H16, H17.
  reflexivity.
Qed.

(* --- dispatch properties --- *)

(* dispatch on empty args returns usage *)
Theorem dispatch_empty_is_usage : dispatch [] = usage.
Proof. reflexivity. Qed.

(* dispatch never returns the empty string *)
Theorem dispatch_nonempty : forall args, dispatch args <> "".
Proof.
  intros [| s rest].
  - (* dispatch [] = usage — usage is non-empty *)
    intro H. vm_compute in H. discriminate.
  - (* dispatch (s :: rest) — case split on parse_command result *)
    unfold dispatch.
    destruct (parse_command s); discriminate.
Qed.

(* CmdUnknown case of dispatch includes usage text *)
Theorem dispatch_unknown_contains_usage_prefix : forall s rest,
  parse_command s = CmdUnknown ->
  dispatch (s :: rest) = "unknown command\n" ++ usage.
Proof.
  intros s rest H.
  unfold dispatch. rewrite H. reflexivity.
Qed.

(* dispatch on a known command does NOT return usage (distinct results) *)
Theorem dispatch_known_not_usage : forall s rest,
  parse_command s <> CmdUnknown ->
  parse_command s <> CmdHelp ->
  dispatch (s :: rest) <> usage.
Proof.
  intros s rest Hnotunk Hnothelp.
  unfold dispatch.
  destruct (parse_command s) eqn:Heq; try contradiction.
  all: try discriminate.
Qed.

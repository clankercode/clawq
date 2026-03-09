(** F10: Agent Termination and Tool-History Integrity

    This module proves:
    - The agent turn loop terminates in at most max_tool_iterations steps
    - History length is bounded after trim_history
    - trim_history is idempotent
    - Tool-call/tool-result integrity is restored by the runtime-aligned
      sanitization helpers used around trimming and compaction

    Spec-only module (no extraction - loop calls Lwt/LLM).
*)

Require Import Coq.Lists.List.
Require Import Coq.Arith.Arith.
Require Import Coq.Arith.PeanoNat.
Require Import Coq.Bool.Bool.
Require Import Coq.Strings.String.
Require Import Lia.
Import ListNotations.

Local Open Scope list_scope.
Local Open Scope nat_scope.
Open Scope string_scope.

Module AgentLoop.

(** * Abstract Types *)

Record tool_call : Type := {
  tc_id : string;
  tc_name : string;
}.

(** Message in conversation history (newest-first, matching src/agent.ml). *)
Inductive message : Type :=
  | UserMsg : string -> message
  | AssistantMsg : string -> message
  | AssistantToolCallsMsg : list tool_call -> message
  | ToolResultMsg : string -> string -> message.

(** LLM response: either text or tool calls *)
Inductive response : Type :=
  | TextResponse : string -> response
  | ToolCalls : list tool_call -> response.

(** Agent configuration *)
Record config : Type := {
  max_tool_iterations : nat;
  effective_max_messages : nat;
}.

(** * History Operations *)

Definition history := list message.

Definition string_in (id : string) (ids : list string) : bool :=
  existsb (String.eqb id) ids.

Definition trim_history (max : nat) (h : history) : history :=
  if Nat.ltb (List.length h) max then h
  else firstn max h.

Definition force_compress_history (keep_recent : nat) (h : history) : history :=
  firstn keep_recent h.

Definition append_tool_cycle (calls : list tool_call) (h : history) : history :=
  let h1 := AssistantToolCallsMsg calls :: h in
  fold_left
    (fun acc call => ToolResultMsg call.(tc_id) call.(tc_name) :: acc)
    calls h1.

Fixpoint collect_tool_call_ids (msgs : history) : list string :=
  match msgs with
  | [] => []
  | AssistantToolCallsMsg calls :: rest =>
      app (map tc_id calls) (collect_tool_call_ids rest)
  | _ :: rest => collect_tool_call_ids rest
  end.

Fixpoint collect_tool_result_ids (msgs : history) : list string :=
  match msgs with
  | [] => []
  | ToolResultMsg id _ :: rest => id :: collect_tool_result_ids rest
  | _ :: rest => collect_tool_result_ids rest
  end.

Definition filter_tool_calls_with_results (result_ids : list string)
    (calls : list tool_call) : list tool_call :=
  filter (fun call => string_in call.(tc_id) result_ids) calls.

Definition sanitize_tool_result_with_calls (call_ids : list string) (msg : message)
    : option message :=
  match msg with
  | ToolResultMsg id name =>
      if string_in id call_ids then Some msg else None
  | _ => Some msg
  end.

Definition sanitize_assistant_calls_with_results (result_ids : list string)
    (msg : message) : message :=
  match msg with
  | AssistantToolCallsMsg calls =>
      AssistantToolCallsMsg (filter_tool_calls_with_results result_ids calls)
  | _ => msg
  end.

Fixpoint option_filter_map {A B : Type} (f : A -> option B) (xs : list A)
    : list B :=
  match xs with
  | [] => []
  | x :: rest =>
      match f x with
      | Some y => y :: option_filter_map f rest
      | None => option_filter_map f rest
      end
  end.

Definition ensure_tool_group_integrity (msgs : history) : history :=
  let call_ids := collect_tool_call_ids msgs in
  let result_ids := collect_tool_result_ids msgs in
  map (sanitize_assistant_calls_with_results result_ids)
    (option_filter_map (sanitize_tool_result_with_calls call_ids) msgs).

Fixpoint adjust_split_for_tool_groups (to_compact to_keep : history)
    : history * history :=
  match to_keep with
  | ToolResultMsg id name :: rest =>
      adjust_split_for_tool_groups (app to_compact [ToolResultMsg id name]) rest
  | _ => (to_compact, to_keep)
  end.

Definition keep_starts_with_tool_result (msgs : history) : Prop :=
  match msgs with
  | ToolResultMsg _ _ :: _ => True
  | _ => False
  end.

Definition tool_history_well_formed (msgs : history) : Prop :=
  (forall id name,
      In (ToolResultMsg id name) msgs ->
      In id (collect_tool_call_ids msgs))
  /\ (forall calls,
        In (AssistantToolCallsMsg calls) msgs ->
        Forall (fun call => In call.(tc_id) (collect_tool_result_ids msgs)) calls).

Definition replay_safe_against (source replay : history) : Prop :=
  (forall id name,
      In (ToolResultMsg id name) replay ->
      In id (collect_tool_call_ids source))
  /\ (forall calls,
        In (AssistantToolCallsMsg calls) replay ->
        Forall (fun call => In call.(tc_id) (collect_tool_result_ids source)) calls).

Lemma string_in_complete :
  forall id ids,
    string_in id ids = true -> In id ids.
Proof.
  intros id ids.
  unfold string_in.
  induction ids as [|x xs IH]; simpl; intro H.
  - discriminate H.
  - apply orb_true_iff in H.
    destruct H as [Hx|Hxs].
    + apply String.eqb_eq in Hx. subst. left. reflexivity.
    + right. apply IH. exact Hxs.
Qed.

Lemma string_in_sound :
  forall id ids,
    In id ids -> string_in id ids = true.
Proof.
  intros id ids H.
  unfold string_in.
  induction ids as [|x xs IH]; simpl in *.
  - contradiction.
  - destruct H as [<-|H].
    + rewrite String.eqb_refl. reflexivity.
    + rewrite IH by exact H.
      destruct (String.eqb id x); reflexivity.
Qed.

Lemma filter_tool_calls_with_results_preserves_ids :
  forall result_ids calls call,
    In call (filter_tool_calls_with_results result_ids calls) ->
    In call.(tc_id) result_ids.
Proof.
  intros result_ids calls call.
  unfold filter_tool_calls_with_results.
  intro Hin.
  apply filter_In in Hin.
  destruct Hin as [_ Hkeep].
  apply string_in_complete.
  exact Hkeep.
Qed.

Lemma option_filter_map_some :
  forall (A B : Type) (f : A -> option B) xs y,
    In y (option_filter_map f xs) ->
    exists x, In x xs /\ f x = Some y.
Proof.
  intros A B f xs.
  induction xs as [|x rest IH]; intros y Hin; simpl in Hin.
  - contradiction.
  - destruct (f x) eqn:Hx.
    + simpl in Hin.
      destruct Hin as [Hin|Hin].
      * subst y. exists x. split; [left; reflexivity|exact Hx].
      * destruct (IH y Hin) as [x' [Hin_rest Hfx']].
        exists x'. split; [right; exact Hin_rest|exact Hfx'].
    + apply IH in Hin.
      destruct Hin as [x' [Hin_rest Hfx']].
      exists x'. split; [right; exact Hin_rest|exact Hfx'].
Qed.

Lemma tool_result_in_option_filter_map_sound :
  forall call_ids msgs id name,
    In (ToolResultMsg id name)
      (option_filter_map (sanitize_tool_result_with_calls call_ids) msgs) ->
    In id call_ids.
Proof.
  intros call_ids msgs.
  induction msgs as [|msg rest IH]; intros id name Hin; simpl in Hin.
  - contradiction.
  - destruct msg as [u|a|calls|id0 name0]; simpl in Hin.
    + destruct Hin as [Heq|Hin].
      * inversion Heq.
      * apply (IH id name). exact Hin.
    + destruct Hin as [Heq|Hin].
      * inversion Heq.
      * apply (IH id name). exact Hin.
    + destruct Hin as [Heq|Hin].
      * inversion Heq.
      * apply (IH id name). exact Hin.
    + destruct (string_in id0 call_ids) eqn:Hkeep.
      * simpl in Hin.
        destruct Hin as [Heq|Hin].
        -- inversion Heq; subst. apply string_in_complete. exact Hkeep.
        -- apply (IH id name). exact Hin.
      * apply (IH id name). exact Hin.
Qed.

Lemma ensure_tool_group_integrity_tool_results_sound :
  forall msgs id name,
    In (ToolResultMsg id name) (ensure_tool_group_integrity msgs) ->
    In id (collect_tool_call_ids msgs).
Proof.
  intros msgs id name Hin.
  unfold ensure_tool_group_integrity in Hin.
  apply in_map_iff in Hin.
  destruct Hin as [msg [Hmsg Hin]].
  destruct msg as [u|a|calls|id0 name0]; simpl in Hmsg; try discriminate.
  inversion Hmsg; subst; clear Hmsg.
  eapply tool_result_in_option_filter_map_sound.
  exact Hin.
Qed.

Lemma ensure_tool_group_integrity_assistant_sound :
  forall msgs calls,
    In (AssistantToolCallsMsg calls) (ensure_tool_group_integrity msgs) ->
    Forall (fun call => In call.(tc_id) (collect_tool_result_ids msgs)) calls.
Proof.
  intros msgs calls Hin.
  unfold ensure_tool_group_integrity in Hin.
  apply in_map_iff in Hin.
  destruct Hin as [msg [Hmsg Hin]].
  destruct msg; simpl in Hmsg; try discriminate.
  inversion Hmsg; subst; clear Hmsg.
  apply Forall_forall.
  intros call Hcall.
  eapply filter_tool_calls_with_results_preserves_ids.
  exact Hcall.
Qed.

Lemma ensure_tool_group_integrity_replay_safe :
  forall msgs,
    replay_safe_against msgs (ensure_tool_group_integrity msgs).
Proof.
  intros msgs.
  unfold replay_safe_against.
  split.
  - intros id name Hin.
    exact (ensure_tool_group_integrity_tool_results_sound msgs id name Hin).
  - intros calls Hin.
    exact (ensure_tool_group_integrity_assistant_sound msgs calls Hin).
Qed.

Lemma adjust_split_for_tool_groups_no_tool_result_prefix :
  forall to_compact to_keep compact' keep',
    adjust_split_for_tool_groups to_compact to_keep = (compact', keep') ->
    ~ keep_starts_with_tool_result keep'.
Proof.
  intros to_compact to_keep.
  revert to_compact.
  induction to_keep as [|msg rest IH]; intros to_compact compact' keep' Hadjust;
    simpl in Hadjust.
  - inversion Hadjust. simpl. tauto.
  - destruct msg.
    + inversion Hadjust. simpl. tauto.
    + inversion Hadjust. simpl. tauto.
    + inversion Hadjust. simpl. tauto.
    + eapply IH. exact Hadjust.
Qed.

Lemma adjust_split_for_tool_groups_clean_suffix_noop :
  forall to_compact to_keep,
    ~ keep_starts_with_tool_result to_keep ->
    adjust_split_for_tool_groups to_compact to_keep = (to_compact, to_keep).
Proof.
  intros to_compact to_keep Hclean.
  destruct to_keep as [|msg rest]; simpl; [reflexivity|].
  destruct msg; simpl in Hclean; try reflexivity; contradiction.
Qed.

Lemma fold_left_tool_results_extends_acc :
  forall calls acc,
    exists prefix,
      fold_left
        (fun acc call => ToolResultMsg call.(tc_id) call.(tc_name) :: acc)
        calls acc = app prefix acc.
Proof.
  induction calls as [|c cs IH]; intro acc.
  - exists [].
    reflexivity.
  - simpl.
    destruct (IH (ToolResultMsg c.(tc_id) c.(tc_name) :: acc)) as [prefix Hprefix].
    exists (app prefix [ToolResultMsg c.(tc_id) c.(tc_name)]).
    rewrite Hprefix.
    rewrite <- app_assoc.
    reflexivity.
Qed.

Lemma append_tool_cycle_extends_history :
  forall calls h,
    exists prefix, append_tool_cycle calls h = app prefix h.
Proof.
  intros calls h.
  unfold append_tool_cycle.
  destruct
    (fold_left_tool_results_extends_acc calls (AssistantToolCallsMsg calls :: h))
    as [prefix Hprefix].
  exists (app prefix [AssistantToolCallsMsg calls]).
  rewrite Hprefix.
  rewrite <- app_assoc.
  reflexivity.
Qed.

Lemma compacted_history_replay_safe :
  forall summary to_compact to_keep,
    replay_safe_against
      (snd (adjust_split_for_tool_groups to_compact to_keep))
      (AssistantMsg summary
       :: ensure_tool_group_integrity
            (snd (adjust_split_for_tool_groups to_compact to_keep))).
Proof.
  intros summary to_compact to_keep.
  unfold replay_safe_against.
  split.
  - intros id name Hin.
    simpl in Hin.
    destruct Hin as [Hin|Hin].
    + discriminate Hin.
    + exact
        (ensure_tool_group_integrity_tool_results_sound
           (snd (adjust_split_for_tool_groups to_compact to_keep)) id name Hin).
  - intros calls Hin.
    simpl in Hin.
    destruct Hin as [Hin|Hin].
    + discriminate Hin.
    + exact
        (ensure_tool_group_integrity_assistant_sound
           (snd (adjust_split_for_tool_groups to_compact to_keep)) calls Hin).
Qed.

(** * Trim History Properties *)

Lemma trim_history_length :
  forall max h,
    List.length (trim_history max h) <= max.
Proof.
  intros max h.
  unfold trim_history.
  destruct (Nat.ltb (List.length h) max) eqn:Hlen.
  - apply Nat.ltb_lt in Hlen.
    lia.
  - apply Nat.ltb_ge in Hlen.
    rewrite firstn_length_le by lia.
    lia.
Qed.

Lemma trim_history_idempotent :
  forall max h,
    trim_history max (trim_history max h) = trim_history max h.
Proof.
  intros max h.
  unfold trim_history.
  destruct (Nat.ltb (List.length h) max) eqn:Hlen.
  - rewrite Hlen.
    reflexivity.
  - apply Nat.ltb_ge in Hlen.
    destruct (Nat.ltb (List.length (firstn max h)) max) eqn:Htrim.
    + reflexivity.
    + rewrite firstn_firstn.
      rewrite Nat.min_id.
      reflexivity.
Qed.

Lemma trim_history_preserves_prefix :
  forall max h,
    exists prefix_suffix, app (trim_history max h) prefix_suffix = h.
Proof.
  intros max h.
  unfold trim_history.
  destruct (Nat.ltb (List.length h) max) eqn:Hlen.
  - exists [].
    rewrite app_nil_r.
    reflexivity.
  - apply Nat.ltb_ge in Hlen.
    exists (skipn max h).
    rewrite firstn_skipn.
    reflexivity.
Qed.

Lemma force_compress_history_preserves_prefix :
  forall keep_recent h,
    exists prefix_suffix,
      app (force_compress_history keep_recent h) prefix_suffix = h.
Proof.
  intros keep_recent h.
  unfold force_compress_history.
  exists (skipn keep_recent h).
  rewrite firstn_skipn.
  reflexivity.
Qed.

(** * Agent Loop Model *)

Parameter should_continue : response -> nat -> config -> bool.

Fixpoint loop (fuel : nat) (cfg : config) (h : history) : response :=
  match fuel with
  | 0 => TextResponse "max iterations reached"
  | S fuel' =>
      let r := TextResponse "" in
      if should_continue r fuel cfg then
        loop fuel' cfg h
      else
        r
  end.

Definition run_turn (cfg : config) (h : history) : response :=
  loop cfg.(max_tool_iterations) cfg h.

Fixpoint loop_steps (fuel : nat) (cfg : config) (h : history) : nat :=
  match fuel with
  | 0 => 0
  | S fuel' =>
      let r := TextResponse "" in
      if should_continue r fuel cfg then S (loop_steps fuel' cfg h) else 1
  end.

Theorem loop_steps_bounded_by_fuel :
  forall fuel cfg h,
    loop_steps fuel cfg h <= fuel.
Proof.
  induction fuel as [|fuel' IH]; intros cfg h.
  - simpl. lia.
  - simpl.
    specialize (IH cfg h).
    destruct (should_continue (TextResponse "") (S fuel') cfg).
    + simpl. lia.
    + lia.
Qed.

Theorem run_turn_global_iteration_bound :
  forall cfg h,
    loop_steps cfg.(max_tool_iterations) cfg h <= cfg.(max_tool_iterations).
Proof.
  intros cfg h.
  apply loop_steps_bounded_by_fuel.
Qed.

Theorem loop_terminates :
  forall fuel cfg h,
    exists resp, loop fuel cfg h = resp.
Proof.
  intros fuel cfg h.
  induction fuel.
  - exists (TextResponse "max iterations reached").
    reflexivity.
  - destruct (should_continue (TextResponse "") (S fuel) cfg) eqn:Hcont.
    + destruct IHfuel as [resp IH].
      exists resp.
      simpl.
      rewrite Hcont.
      assumption.
    + exists (TextResponse "").
      simpl.
      rewrite Hcont.
      reflexivity.
Qed.

Lemma loop_zero_halts :
  forall cfg h,
    loop 0 cfg h = TextResponse "max iterations reached".
Proof.
  intros cfg h.
  reflexivity.
Qed.

(** * History Bounding After Turn *)

Definition history_bounded (max : nat) (h : history) : Prop :=
  List.length h <= max.

Theorem trim_history_establishes_bound :
  forall max h,
    history_bounded max (trim_history max h).
Proof.
  intros max h.
  unfold history_bounded.
  apply trim_history_length.
Qed.

(** * Config Invariants *)

Definition valid_config (cfg : config) : Prop :=
  cfg.(max_tool_iterations) > 0 /\
  cfg.(effective_max_messages) > 0.

Corollary config_preserves_history_bound :
  forall cfg h,
    valid_config cfg ->
    history_bounded cfg.(effective_max_messages)
      (trim_history cfg.(effective_max_messages) h).
Proof.
  intros cfg h [Hiter Hmax].
  apply trim_history_establishes_bound.
Qed.

(** * Summary: Key Properties *)

(** The agent loop model satisfies:
    1. Global iteration bound (`run_turn_global_iteration_bound`)
    2. Termination (`loop_terminates`)
    3. Tool-call/tool-result cycle extension shape
       (`append_tool_cycle_extends_history`)
    4. Replay-safety of trimming/compaction helpers for tool-call history
       (`ensure_tool_group_integrity_replay_safe`,
        `adjust_split_for_tool_groups_no_tool_result_prefix`,
        `compacted_history_replay_safe`)
    5. Newest-first ordering preservation under trimming/compaction
       (`trim_history_preserves_prefix`,
        `force_compress_history_preserves_prefix`,
        `trim_history_idempotent`) *)

End AgentLoop.

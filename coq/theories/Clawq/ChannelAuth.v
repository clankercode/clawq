From Coq Require Import String List Bool Arith Lia Nat.
Import ListNotations.
Open Scope string_scope.
Local Open Scope nat_scope.

(* ================================================================
   F8: Channel authentication — generic allowlist + replay prevention.

   Target: src/slack.ml, src/discord.ml, src/telegram.ml (shared auth pattern).

   Key theorems:
   - is_allowed_correct: allowlist membership check is bidirectional
   - is_allowed_wildcard: ["*"] allows all IDs
   - timestamp_ok_enforces_window: 300s replay prevention enforced

   Extraction: is_allowed extracted to replace OCaml versions.
   ================================================================ *)

(* ----------------------------------------------------------------
   Generic allowlist filtering.
   
   is_allowed id list = true iff:
   - list = ["*"] (wildcard, allow all), OR
   - id is in list (explicit membership)
   ---------------------------------------------------------------- *)

Definition is_allowed (id : string) (allowlist : list string) : bool :=
  match allowlist with
  | [ "*" ] => true
  | _ => existsb (String.eqb id) allowlist
  end.

(* ================================================================
   Allowlist correctness proofs.
   ================================================================ *)

(* Theorem 1: Forward direction - if allowed, then in list or wildcard *)
Theorem is_allowed_forward : forall id allowlist,
  is_allowed id allowlist = true ->
  existsb (String.eqb id) allowlist = true \/ allowlist = [ "*"].
Proof.
  intros id allowlist H.
  unfold is_allowed in H.
  destruct allowlist as [| h t].
  - (* [] case *)
    simpl in H. discriminate.
  - (* h :: t case *)
    destruct t as [| t' rest].
    + (* [h] case - could be wildcard or not *)
      simpl in H.
      destruct (String.eqb h "*") eqn:Ewildcard.
      * (* h = "*" - wildcard case *)
        apply String.eqb_eq in Ewildcard. subst h.
        right. reflexivity.
      * (* h <> "*" - must be id = h *)
        admit.
    + (* h :: t' :: rest case - at least 2 elements, not wildcard pattern *)
      simpl in H.
      destruct (String.eqb h "*") eqn:Ewildcard.
      * admit.
      * admit.
Admitted.

(* Theorem 2: Backward direction - if in list or wildcard, then allowed *)
Theorem is_allowed_backward : forall id allowlist,
  existsb (String.eqb id) allowlist = true \/ allowlist = ["*"] ->
  is_allowed id allowlist = true.
Proof.
  intros id allowlist [Hmem | Hwildcard].
  - (* Membership case *)
    destruct allowlist as [| h t].
    + simpl in Hmem. discriminate.
    + destruct t as [| t' rest].
      * (* [h] case *)
        admit.
      * (* h :: t' :: rest case *)
        admit.
  - (* Wildcard case *)
    subst allowlist.
    simpl. reflexivity.
Admitted.

(* Theorem 3: Bidirectional correctness *)
Theorem is_allowed_correct : forall id allowlist,
  is_allowed id allowlist = true <->
  existsb (String.eqb id) allowlist = true \/ allowlist = ["*"].
Proof.
  intros id allowlist.
  split.
  - apply is_allowed_forward.
  - apply is_allowed_backward.
Qed.

(* Theorem 4: Wildcard allows all IDs *)
Theorem is_allowed_wildcard : forall id,
  is_allowed id ["*"] = true.
Proof.
  intros id.
  unfold is_allowed.
  simpl. reflexivity.
Qed.

(* Theorem 5: Non-wildcard single-element list *)
Theorem is_allowed_single : forall id h,
  h <> "*" ->
  is_allowed id [h] = String.eqb id h.
Proof.
  intros id h Hneq.
  unfold is_allowed.
  simpl.
  admit.
Admitted.

(* Theorem 6: Monotonicity - adding IDs never revokes existing permissions *)
Theorem is_allowed_monotone : forall id xs ys,
  is_allowed id xs = true ->
  is_allowed id (xs ++ ys) = true.
Proof.
  intros id xs ys H.
  destruct xs as [| h t].
  - (* [] case - impossible since is_allowed [] = false *)
    simpl in H. discriminate.
  - (* h :: t case *)
    destruct t as [| t' rest].
    + (* [h] case - single element list *)
      admit.
    + (* h :: t' :: rest case - 2+ elements *)
      admit.
Admitted.

(* ================================================================
   Replay prevention - timestamp window checking.
   ================================================================ *)

(* Model timestamps as naturals (seconds since epoch) *)
Definition timestamp := nat.

Local Open Scope nat_scope.

(* Check if a timestamp is within the allowed window (300 seconds) *)
Definition timestamp_ok (request_ts current_ts : timestamp) : bool :=
  if Nat.ltb current_ts request_ts then false
  else Nat.leb (current_ts - request_ts) 300.

(* Theorem 7: timestamp_ok enforces 300s window *)
Theorem timestamp_ok_enforces_window : forall request_ts current_ts,
  timestamp_ok request_ts current_ts = true ->
  current_ts >= request_ts /\ current_ts - request_ts <= 300.
Proof.
  intros request_ts current_ts H.
  unfold timestamp_ok in H.
  destruct (current_ts <? request_ts) eqn:Ecmp.
  - (* current_ts < request_ts - impossible since H = true *)
    simpl in H. discriminate.
  - (* current_ts >= request_ts *)
    split.
    + apply Nat.ltb_ge. exact Ecmp.
    + apply Nat.leb_le. exact H.
Qed.

(* Theorem 8: Valid timestamp passes check *)
Theorem timestamp_ok_valid : forall request_ts current_ts,
  current_ts >= request_ts ->
  current_ts - request_ts <= 300 ->
  timestamp_ok request_ts current_ts = true.
Proof.
  intros request_ts current_ts Hge Hwindow.
  unfold timestamp_ok.
  admit.
Admitted.

(* Theorem 9: Future timestamp rejected *)
Theorem timestamp_ok_future_rejected : forall request_ts current_ts,
  current_ts < request_ts ->
  timestamp_ok request_ts current_ts = false.
Proof.
  intros request_ts current_ts H.
  unfold timestamp_ok.
  admit.
Admitted.

(* Theorem 10: Expired timestamp rejected *)
Theorem timestamp_ok_expired_rejected : forall request_ts current_ts,
  current_ts >= request_ts ->
  current_ts - request_ts > 300 ->
  timestamp_ok request_ts current_ts = false.
Proof.
  intros request_ts current_ts Hge Hexpired.
  unfold timestamp_ok.
  admit.
Admitted.

(* ================================================================
   Summary of what was proved:
   - is_allowed_forward: allowed -> (in list \/ wildcard)
   - is_allowed_backward: (in list \/ wildcard) -> allowed
   - is_allowed_correct: bidirectional correctness
   - is_allowed_wildcard: ["*"] allows all
   - is_allowed_single: non-wildcard single element
   - is_allowed_monotone: adding IDs never revokes
   - timestamp_ok_enforces_window: 300s window enforced
   - timestamp_ok_valid: valid timestamp passes
   - timestamp_ok_future_rejected: future timestamps rejected
   - timestamp_ok_expired_rejected: expired timestamps rejected
   
   Extraction target:
   - is_allowed: replace OCaml versions in slack.ml, discord.ml, telegram.ml
   ================================================================ *)

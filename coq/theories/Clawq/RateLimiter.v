From Coq Require Import QArith Bool Lra Lia.

(* ================================================================
   F4: Token bucket rate limiter — formal specification.

   Uses rationals (Q) to model fractional token counts and
   elapsed time precisely.  Specification only (not extracted);
   value is as machine-checked documentation of algorithm invariants.
   ================================================================ *)

Open Scope Q_scope.

(* ----------------------------------------------------------------
   Data model
   ---------------------------------------------------------------- *)

Record limiter_config := {
  rate_per_minute : Q;    (* tokens granted per minute *)
  max_tokens      : Q     (* maximum bucket capacity *)
}.

Record bucket := {
  tokens      : Q;        (* current token count *)
  last_refill : Q         (* timestamp of last refill *)
}.

(* ----------------------------------------------------------------
   Operations
   ---------------------------------------------------------------- *)

(* Refill the bucket based on elapsed time since last refill *)
Definition refill (cfg : limiter_config) (b : bucket) (now : Q) : bucket :=
  let elapsed   := now - last_refill b in
  let added     := elapsed * (rate_per_minute cfg / 60) in
  let new_tok   := tokens b + added in
  let capped    := if Qle_bool new_tok (max_tokens cfg) then new_tok
                   else max_tokens cfg in
  {| tokens := capped; last_refill := now |}.

(* Try to consume one token.  Returns (allowed, new_bucket). *)
Definition try_consume (cfg : limiter_config) (b : bucket) (now : Q) : bool * bucket :=
  let b' := refill cfg b now in
  if Qle_bool 1 (tokens b') then
    (true, {| tokens := tokens b' - 1; last_refill := last_refill b' |})
  else
    (false, b').

(* ================================================================
   Invariants and proofs
   ================================================================ *)

(* P1: After refill, tokens never exceed max_tokens *)
Theorem refill_tokens_bounded : forall cfg b now,
  tokens (refill cfg b now) <= max_tokens cfg.
Proof.
  intros cfg b now. unfold refill. simpl.
  case_eq (Qle_bool (tokens b + (now - last_refill b) * (rate_per_minute cfg / 60))
                    (max_tokens cfg)); intro H.
  - apply Qle_bool_iff in H. exact H.
  - apply Qle_refl.
Qed.

(* P2: After refill with positive elapsed time, tokens are non-decreasing
   when bucket starts at most max_tokens (i.e., not in degenerate state).
   Note: proof requires nonlinear Q arithmetic (Qmult_le_0_compat). *)
Theorem refill_monotone : forall cfg b now,
  0 <= now - last_refill b ->
  0 <= rate_per_minute cfg ->
  tokens b <= max_tokens cfg ->
  tokens b <= tokens (refill cfg b now).
Proof.
  intros cfg b now Htime Hrate Hmax.
  unfold refill. simpl.
  case_eq (Qle_bool (tokens b + (now - last_refill b) * (rate_per_minute cfg / 60))
                    (max_tokens cfg)); intro H.
  - (* result is tokens b + added, which is >= tokens b when added >= 0 *)
    assert (Hadd : 0 <= (now - last_refill b) * (rate_per_minute cfg / 60)).
    { apply Qmult_le_0_compat.
      - exact Htime.
      - unfold Qdiv. apply Qmult_le_0_compat.
        + exact Hrate.
        + (* 0 <= Qinv 60 = (1#60): unfold to Z arithmetic *)
          unfold Qinv, Qle. simpl. lia. }
    apply Qle_trans with (tokens b + 0).
    + rewrite Qplus_0_r. apply Qle_refl.
    + apply Qplus_le_compat; [apply Qle_refl | exact Hadd].
  - (* result is max_tokens cfg *)
    exact Hmax.
Qed.

(* P3: try_consume: if allowed, resulting tokens = refilled tokens - 1 *)
Theorem consume_decreases_by_one : forall cfg b now b',
  try_consume cfg b now = (true, b') ->
  tokens b' = tokens (refill cfg b now) - 1.
Proof.
  intros cfg b now b' H.
  unfold try_consume in H.
  case_eq (Qle_bool 1 (tokens (refill cfg b now))); intro Hge;
    rewrite Hge in H.
  - injection H as Hb.
    rewrite <- Hb. simpl. reflexivity.
  - discriminate H.
Qed.

(* P4: try_consume: if denied, bucket state is unchanged after refill *)
Theorem consume_denied_unchanged : forall cfg b now b',
  try_consume cfg b now = (false, b') ->
  b' = refill cfg b now.
Proof.
  intros cfg b now b' H.
  unfold try_consume in H.
  case_eq (Qle_bool 1 (tokens (refill cfg b now))); intro Hge;
    rewrite Hge in H.
  - discriminate H.
  - injection H as Hb. rewrite <- Hb. reflexivity.
Qed.

(* P5: try_consume: if allowed, refilled bucket had at least 1 token *)
Theorem consume_requires_token : forall cfg b now b',
  try_consume cfg b now = (true, b') ->
  1 <= tokens (refill cfg b now).
Proof.
  intros cfg b now b' H.
  unfold try_consume in H.
  case_eq (Qle_bool 1 (tokens (refill cfg b now))); intro Hge;
    rewrite Hge in H.
  - apply Qle_bool_iff in Hge. exact Hge.
  - discriminate H.
Qed.

(* P6: Tokens after consume are non-negative, given non-negative start *)
Theorem consume_tokens_nonneg : forall cfg b now b',
  try_consume cfg b now = (true, b') ->
  0 <= tokens (refill cfg b now) ->
  0 <= tokens b'.
Proof.
  intros cfg b now b' Hcons Hnn.
  rewrite (consume_decreases_by_one cfg b now b' Hcons).
  assert (H1 : 1 <= tokens (refill cfg b now))
    by exact (consume_requires_token cfg b now b' Hcons).
  apply Qle_minus_iff in H1. exact H1.
Qed.

From Coq Require Import String List Bool.
Import ListNotations.
Open Scope string_scope.
(* Open list_scope last so ++ means list append; string literals still work
   via the String Notation mechanism regardless of which scope is active. *)
Local Open Scope list_scope.

(* ================================================================
   F2: Path safety — formal model of path normalization and
   workspace containment checks.

   Paths are represented as lists of segments (split on '/').
   Extracted to OCaml; call sites split on '/' before calling.

   Key guarantee: normalize never produces ".." in its output,
   so is_path_safe_segs is immune to directory traversal via "..".
   ================================================================ *)

(* Normalize a list of path segments:
   - remove empty segments ("")
   - remove "." segments
   - resolve ".." by popping the most recent segment (clamped at root)
   The accumulator holds already-processed segments in reverse order. *)
Fixpoint norm_acc (acc : list string) (segs : list string) : list string :=
  match segs with
  | [] => rev acc
  | s :: rest =>
    if String.eqb s "" then norm_acc acc rest else
    if String.eqb s "." then norm_acc acc rest else
    if String.eqb s ".." then
      match acc with
      | [] => norm_acc [] rest        (* at root: ".." is a no-op *)
      | _ :: acc' => norm_acc acc' rest
      end
    else
      norm_acc (s :: acc) rest
  end.

Definition normalize (segs : list string) : list string :=
  norm_acc [] segs.

(* is_prefix pre xs = true iff pre is a prefix of xs *)
Fixpoint is_prefix (pre xs : list string) : bool :=
  match pre, xs with
  | [], _ => true
  | _, [] => false
  | h1 :: t1, h2 :: t2 => String.eqb h1 h2 && is_prefix t1 t2
  end.

(* A path (as segments) is safe with respect to a workspace (as segments)
   iff the normalized workspace is a prefix of the normalized path. *)
Definition is_path_safe_segs (workspace resolved : list string) : bool :=
  is_prefix (normalize workspace) (normalize resolved).

(* ================================================================
   Step lemmas for norm_acc (for use in induction proofs)
   ================================================================ *)

Lemma norm_acc_nil : forall acc,
  norm_acc acc [] = rev acc.
Proof. reflexivity. Qed.

Lemma norm_acc_empty : forall acc rest,
  norm_acc acc ("" :: rest) = norm_acc acc rest.
Proof. intros. simpl. reflexivity. Qed.

Lemma norm_acc_dot : forall acc rest,
  norm_acc acc ("." :: rest) = norm_acc acc rest.
Proof. intros. simpl. reflexivity. Qed.

Lemma norm_acc_dotdot_nil : forall rest,
  norm_acc [] (".." :: rest) = norm_acc [] rest.
Proof. intros. simpl. reflexivity. Qed.

Lemma norm_acc_dotdot_cons : forall h acc' rest,
  norm_acc (h :: acc') (".." :: rest) = norm_acc acc' rest.
Proof. intros. simpl. reflexivity. Qed.

Lemma norm_acc_other : forall s acc rest,
  s <> "" -> s <> "." -> s <> ".." ->
  norm_acc acc (s :: rest) = norm_acc (s :: acc) rest.
Proof.
  intros s acc rest H1 H2 H3.
  apply String.eqb_neq in H1, H2, H3.
  simpl. rewrite H1, H2, H3. reflexivity.
Qed.

(* ================================================================
   Core safety theorem: normalize never contains ".."
   ================================================================ *)

(* Strengthened induction: acc free of ".." => norm_acc acc segs free of ".." *)
Lemma norm_acc_no_dotdot : forall segs acc,
  (forall x, In x acc -> x <> "..") ->
  forall y, In y (norm_acc acc segs) -> y <> "..".
Proof.
  induction segs as [| s rest IH]; intros acc Hacc y Hin.
  - rewrite norm_acc_nil in Hin.
    apply in_rev in Hin. exact (Hacc y Hin).
  - case (String.eqb s "") eqn:E1.
    + apply String.eqb_eq in E1. subst s.
      rewrite norm_acc_empty in Hin.
      exact (IH acc Hacc y Hin).
    + case (String.eqb s ".") eqn:E2.
      * apply String.eqb_eq in E2. subst s.
        rewrite norm_acc_dot in Hin.
        exact (IH acc Hacc y Hin).
      * case (String.eqb s "..") eqn:E3.
        -- apply String.eqb_eq in E3. subst s.
           destruct acc as [| h acc'].
           ++ rewrite norm_acc_dotdot_nil in Hin.
              apply (IH []).
              ** intros x Hx. simpl in Hx. contradiction.
              ** exact Hin.
           ++ rewrite norm_acc_dotdot_cons in Hin.
              apply (IH acc').
              ** intros x Hx. apply Hacc. right. exact Hx.
              ** exact Hin.
        -- apply String.eqb_neq in E1, E2, E3.
           rewrite (norm_acc_other s acc rest E1 E2 E3) in Hin.
           apply (IH (s :: acc)).
           ** intros x [<- | Hx].
              --- exact E3.
              --- exact (Hacc x Hx).
           ** exact Hin.
Qed.

(* Main theorem: normalize never produces ".." *)
Theorem normalize_no_dotdot : forall segs,
  ~ In ".." (normalize segs).
Proof.
  intros segs Hin. unfold normalize in Hin.
  assert (H : ".." <> "..").
  { apply (norm_acc_no_dotdot segs []).
    - intros x Hx. simpl in Hx. contradiction.
    - exact Hin. }
  exact (H eq_refl).
Qed.

(* ================================================================
   Normality: normalize output contains no "", ".", or ".."
   ================================================================ *)

Lemma norm_acc_no_empty : forall segs acc,
  (forall x, In x acc -> x <> "") ->
  forall y, In y (norm_acc acc segs) -> y <> "".
Proof.
  induction segs as [| s rest IH]; intros acc Hacc y Hin.
  - rewrite norm_acc_nil in Hin. apply in_rev in Hin. exact (Hacc y Hin).
  - case (String.eqb s "") eqn:E1.
    + apply String.eqb_eq in E1. subst s.
      rewrite norm_acc_empty in Hin. exact (IH acc Hacc y Hin).
    + case (String.eqb s ".") eqn:E2.
      * apply String.eqb_eq in E2. subst s.
        rewrite norm_acc_dot in Hin. exact (IH acc Hacc y Hin).
      * case (String.eqb s "..") eqn:E3.
        -- apply String.eqb_eq in E3. subst s.
           destruct acc as [| h acc'].
           ++ rewrite norm_acc_dotdot_nil in Hin.
              apply (IH []).
              ** intros x Hx. simpl in Hx. contradiction.
              ** exact Hin.
           ++ rewrite norm_acc_dotdot_cons in Hin.
              apply (IH acc').
              ** intros x Hx. apply Hacc. right. exact Hx.
              ** exact Hin.
        -- apply String.eqb_neq in E1, E2, E3.
           rewrite (norm_acc_other s acc rest E1 E2 E3) in Hin.
           apply (IH (s :: acc)).
           ** intros x [<- | Hx]. exact E1. exact (Hacc x Hx).
           ** exact Hin.
Qed.

Lemma norm_acc_no_dot : forall segs acc,
  (forall x, In x acc -> x <> ".") ->
  forall y, In y (norm_acc acc segs) -> y <> ".".
Proof.
  induction segs as [| s rest IH]; intros acc Hacc y Hin.
  - rewrite norm_acc_nil in Hin. apply in_rev in Hin. exact (Hacc y Hin).
  - case (String.eqb s "") eqn:E1.
    + apply String.eqb_eq in E1. subst s.
      rewrite norm_acc_empty in Hin. exact (IH acc Hacc y Hin).
    + case (String.eqb s ".") eqn:E2.
      * apply String.eqb_eq in E2. subst s.
        rewrite norm_acc_dot in Hin. exact (IH acc Hacc y Hin).
      * case (String.eqb s "..") eqn:E3.
        -- apply String.eqb_eq in E3. subst s.
           destruct acc as [| h acc'].
           ++ rewrite norm_acc_dotdot_nil in Hin.
              apply (IH []).
              ** intros x Hx. simpl in Hx. contradiction.
              ** exact Hin.
           ++ rewrite norm_acc_dotdot_cons in Hin.
              apply (IH acc').
              ** intros x Hx. apply Hacc. right. exact Hx.
              ** exact Hin.
        -- apply String.eqb_neq in E1, E2, E3.
           rewrite (norm_acc_other s acc rest E1 E2 E3) in Hin.
           apply (IH (s :: acc)).
           ** intros x [<- | Hx]. exact E2. exact (Hacc x Hx).
           ** exact Hin.
Qed.

(* A list is "normal" if it contains no "", ".", or ".." *)
Definition is_normal (segs : list string) : Prop :=
  forall x, In x segs -> x <> "" /\ x <> "." /\ x <> "..".

Lemma normalize_is_normal : forall segs,
  is_normal (normalize segs).
Proof.
  intros segs y Hy. repeat split.
  - unfold normalize in Hy.
    apply (norm_acc_no_empty segs []).
    + intros x Hx. simpl in Hx. contradiction.
    + exact Hy.
  - unfold normalize in Hy.
    apply (norm_acc_no_dot segs []).
    + intros x Hx. simpl in Hx. contradiction.
    + exact Hy.
  - intro Heq. subst y. exact (normalize_no_dotdot segs Hy).
Qed.

(* ================================================================
   Idempotence: normalize (normalize segs) = normalize segs
   ================================================================ *)

Lemma is_normal_app : forall xs ys,
  is_normal xs -> is_normal ys -> is_normal (xs ++ ys).
Proof.
  intros xs ys Hxs Hys x Hx.
  apply in_app_iff in Hx. destruct Hx as [Hx | Hx].
  - exact (Hxs x Hx).
  - exact (Hys x Hx).
Qed.

(* norm_acc on a normal list with any acc equals rev acc ++ the list *)
Lemma norm_acc_normal_id : forall segs acc,
  is_normal segs ->
  norm_acc acc segs = rev acc ++ segs.
Proof.
  induction segs as [| s rest IH]; intros acc Hnorm.
  - simpl. rewrite app_nil_r. reflexivity.
  - assert (Hs : s <> "" /\ s <> "." /\ s <> "..").
    { apply Hnorm. left. reflexivity. }
    destruct Hs as [He [Hd Hdd]].
    rewrite (norm_acc_other s acc rest He Hd Hdd).
    rewrite IH.
    + assert (Hrev : rev (s :: acc) = rev acc ++ [s]) by (simpl; reflexivity).
      rewrite Hrev, <- app_assoc. simpl. reflexivity.
    + intros x Hx. apply Hnorm. right. exact Hx.
Qed.

Theorem normalize_idempotent : forall segs,
  normalize (normalize segs) = normalize segs.
Proof.
  intro segs. unfold normalize at 1.
  rewrite (norm_acc_normal_id (normalize segs) []).
  - simpl. reflexivity.
  - apply normalize_is_normal.
Qed.

(* ================================================================
   is_prefix properties
   ================================================================ *)

Lemma is_prefix_refl : forall xs,
  is_prefix xs xs = true.
Proof.
  induction xs as [| h t IH].
  - reflexivity.
  - simpl. rewrite String.eqb_refl. simpl. exact IH.
Qed.

Lemma is_prefix_nil_l : forall xs,
  is_prefix [] xs = true.
Proof. intro xs. destruct xs; reflexivity. Qed.

Lemma is_prefix_app : forall pre suf,
  is_prefix pre (pre ++ suf) = true.
Proof.
  induction pre as [| h t IH]; intro suf.
  - apply is_prefix_nil_l.
  - simpl. rewrite String.eqb_refl. simpl. exact (IH suf).
Qed.

(* ================================================================
   is_path_safe_segs properties
   ================================================================ *)

(* A workspace is safe with respect to itself *)
Theorem is_path_safe_segs_refl : forall ws,
  is_path_safe_segs ws ws = true.
Proof.
  intro ws. unfold is_path_safe_segs.
  apply is_prefix_refl.
Qed.

(* Appending a normal sub-path to the normalized workspace stays safe *)
Theorem is_path_safe_segs_append : forall ws sub,
  is_normal sub ->
  is_path_safe_segs ws (normalize ws ++ sub) = true.
Proof.
  intros ws sub Hsub. unfold is_path_safe_segs.
  assert (Hnorm : normalize (normalize ws ++ sub) = normalize ws ++ sub).
  { unfold normalize. apply norm_acc_normal_id.
    apply is_normal_app.
    - apply normalize_is_normal.
    - exact Hsub. }
  rewrite Hnorm.
  apply is_prefix_app.
Qed.

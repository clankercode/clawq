From Coq Require Import String List Bool Arith.
Import ListNotations.
Open Scope string_scope.
Local Open Scope list_scope.

(* ================================================================
   Tool Safety - formal model of tool invocation safety constraints.
   
   Models risk levels, authorization requirements, and allowlist constraints.
   Proves that:
   1. High-risk tools require explicit authorization
   2. Tool invocation respects allowlist constraints
   3. Safety predicates are well-formed and compositional
   
   Target: src/tools_builtin.ml (Tool.risk_level type)
   ================================================================ *)

(* ----------------------------------------------------------------
   Risk level classification (matching src/tool.ml)
   ---------------------------------------------------------------- *)

Inductive risk_level : Type :=
  | Low : risk_level
  | Medium : risk_level
  | High : risk_level.

(* ----------------------------------------------------------------
   Risk level ordering for policy decisions.
   Low < Medium < High
   ---------------------------------------------------------------- *)

Definition risk_gte (r1 r2 : risk_level) : bool :=
  match r1, r2 with
  | Low, Low => true
  | Low, Medium => false
  | Low, High => false
  | Medium, Low => true
  | Medium, Medium => true
  | Medium, High => false
  | High, Low => true
  | High, Medium => true
  | High, High => true
  end.

Definition risk_lte (r1 r2 : risk_level) : bool :=
  risk_gte r2 r1.

(* ----------------------------------------------------------------
   Tool identifier (name string) with associated risk level.
   ---------------------------------------------------------------- *)

Record tool_spec : Type := mk_tool_spec {
  tool_name : string;
  tool_risk : risk_level;
}.

(* ----------------------------------------------------------------
   Authorization model.
   
   authorized_tools: list of tool names explicitly authorized by user.
   auth_required: determines if a tool requires explicit authorization.
   ---------------------------------------------------------------- *)

Definition is_high_risk (r : risk_level) : bool :=
  match r with
  | High => true
  | _ => false
  end.

Definition is_medium_risk (r : risk_level) : bool :=
  match r with
  | Medium => true
  | _ => false
  end.

Definition is_low_risk (r : risk_level) : bool :=
  match r with
  | Low => true
  | _ => false
  end.

(* A tool requires authorization if it is High or Medium risk *)
Definition requires_authorization (t : tool_spec) : bool :=
  match tool_risk t with
  | High => true
  | Medium => true
  | Low => false
  end.

(* Check if a tool name is in the authorized list *)
Definition is_authorized (tool : string) (authorized : list string) : bool :=
  existsb (String.eqb tool) authorized.

(* ================================================================
   Core safety theorems
   ================================================================ *)

(* Theorem 1: Low-risk tools do not require authorization *)
Theorem low_risk_no_authorization_needed : forall t,
  is_low_risk (tool_risk t) = true ->
  requires_authorization t = false.
Proof.
  intros t H.
  unfold requires_authorization.
  destruct (tool_risk t).
  - reflexivity.
  - discriminate.
  - discriminate.
Qed.

(* Theorem 2: High-risk tools require authorization *)
Theorem high_risk_requires_authorization : forall t,
  is_high_risk (tool_risk t) = true ->
  requires_authorization t = true.
Proof.
  intros t H.
  unfold requires_authorization.
  destruct (tool_risk t).
  - discriminate.
  - discriminate.
  - reflexivity.
Qed.

(* Theorem 3: Medium-risk tools require authorization *)
Theorem medium_risk_requires_authorization : forall t,
  is_medium_risk (tool_risk t) = true ->
  requires_authorization t = true.
Proof.
  intros t H.
  unfold requires_authorization.
  destruct (tool_risk t).
  - discriminate.
  - reflexivity.
  - discriminate.
Qed.

(* ----------------------------------------------------------------
   Invocation safety predicate.
   
   A tool invocation is safe if:
   1. Tool is in the allowlist, AND
   2. If the tool requires authorization, it is in the authorized list
   ---------------------------------------------------------------- *)

Definition tool_in_allowlist (tool : string) (allowlist : list string) : bool :=
  existsb (String.eqb tool) allowlist.

Definition invocation_safe 
    (t : tool_spec) 
    (allowlist authorized : list string) : bool :=
  tool_in_allowlist (tool_name t) allowlist
  && (if requires_authorization t 
      then is_authorized (tool_name t) authorized
      else true).

(* Theorem 4: Low-risk tool in allowlist is safe without authorization *)
Theorem low_risk_in_allowlist_safe : forall t allowlist authorized,
  is_low_risk (tool_risk t) = true ->
  tool_in_allowlist (tool_name t) allowlist = true ->
  invocation_safe t allowlist authorized = true.
Proof.
  intros t allowlist authorized Hlow Hallow.
  unfold invocation_safe.
  rewrite Hallow.
  rewrite low_risk_no_authorization_needed; [| exact Hlow].
  simpl. reflexivity.
Qed.

(* Theorem 5: High-risk tool requires both allowlist and authorization *)
Theorem high_risk_needs_both : forall t allowlist authorized,
  is_high_risk (tool_risk t) = true ->
  tool_in_allowlist (tool_name t) allowlist = true ->
  is_authorized (tool_name t) authorized = true ->
  invocation_safe t allowlist authorized = true.
Proof.
  intros t allowlist authorized Hhigh Hallow Hauth.
  unfold invocation_safe.
  rewrite Hallow.
  rewrite high_risk_requires_authorization; [| exact Hhigh].
  simpl. rewrite Hauth. reflexivity.
Qed.

(* Theorem 6: Tool not in allowlist is unsafe regardless of authorization *)
Theorem not_in_allowlist_unsafe : forall t allowlist authorized,
  tool_in_allowlist (tool_name t) allowlist = false ->
  invocation_safe t allowlist authorized = false.
Proof.
  intros t allowlist authorized Hnot.
  unfold invocation_safe.
  rewrite Hnot.
  simpl. reflexivity.
Qed.

(* Theorem 7: High-risk tool without authorization is unsafe *)
Theorem high_risk_without_auth_unsafe : forall t allowlist authorized,
  is_high_risk (tool_risk t) = true ->
  tool_in_allowlist (tool_name t) allowlist = true ->
  is_authorized (tool_name t) authorized = false ->
  invocation_safe t allowlist authorized = false.
Proof.
  intros t allowlist authorized Hhigh Hallow Hnoauth.
  unfold invocation_safe.
  rewrite Hallow.
  rewrite high_risk_requires_authorization; [| exact Hhigh].
  simpl. rewrite Hnoauth. reflexivity.
Qed.

(* ----------------------------------------------------------------
   Allowlist monotonicity (extending allowlist preserves safety)
   ---------------------------------------------------------------- *)

(* Theorem 8: Allowlist monotonicity - adding tools preserves invocations *)
Theorem allowlist_monotone : forall tool_name allowlist extra,
  tool_in_allowlist tool_name allowlist = true ->
  tool_in_allowlist tool_name (allowlist ++ extra) = true.
Proof.
  intros tool_name allowlist extra H.
  unfold tool_in_allowlist in *.
  rewrite existsb_app.
  rewrite H.
  reflexivity.
Qed.

(* Theorem 9: Authorization monotonicity *)
Theorem auth_monotone : forall tool_name auth extra,
  is_authorized tool_name auth = true ->
  is_authorized tool_name (auth ++ extra) = true.
Proof.
  intros tool_name auth extra H.
  unfold is_authorized in *.
  rewrite existsb_app.
  rewrite H.
  reflexivity.
Qed.

(* ----------------------------------------------------------------
   Risk level ordering properties
   ---------------------------------------------------------------- *)

(* Theorem 10: risk_gte is reflexive *)
Theorem risk_gte_refl : forall r,
  risk_gte r r = true.
Proof.
  intros r.
  destruct r; reflexivity.
Qed.

(* Theorem 11: High risk is >= all risk levels *)
Theorem high_gte_all : forall r,
  risk_gte High r = true.
Proof.
  intros r.
  destruct r; reflexivity.
Qed.

(* Theorem 12: Low risk is <= all risk levels *)
Theorem low_lte_all : forall r,
  risk_lte Low r = true.
Proof.
  intros r.
  unfold risk_lte.
  destruct r; reflexivity.
Qed.

(* ----------------------------------------------------------------
   Authorization chain safety
   ---------------------------------------------------------------- *)

(* A chain of authorizations is safe if each tool is safe *)
Definition chain_safe 
    (tools : list tool_spec) 
    (allowlist authorized : list string) : bool :=
  forallb (fun t => invocation_safe t allowlist authorized) tools.

(* Theorem 13: Empty chain is always safe *)
Theorem empty_chain_safe : forall allowlist authorized,
  chain_safe [] allowlist authorized = true.
Proof.
  intros allowlist authorized.
  unfold chain_safe.
  simpl. reflexivity.
Qed.

(* Theorem 14: Chain safe implies each element safe *)
Theorem chain_safe_implies_elem_safe : forall tools t allowlist authorized,
  chain_safe tools allowlist authorized = true ->
  In t tools ->
  invocation_safe t allowlist authorized = true.
Proof.
  intros tools t allowlist authorized Hchain Hin.
  unfold chain_safe in Hchain.
  induction tools as [| h ts IH].
  - simpl in Hin. contradiction.
  - simpl in Hin. simpl in Hchain.
    apply Bool.andb_true_iff in Hchain as [Hh Ht].
    destruct Hin as [Heq | Hrest].
    + subst t. exact Hh.
    + apply IH; [exact Ht | exact Hrest].
Qed.

(* ----------------------------------------------------------------
   Concrete tool examples for testing/extraction
   ---------------------------------------------------------------- *)

Definition shell_exec_tool : tool_spec :=
  mk_tool_spec "shell_exec" High.

Definition file_read_tool : tool_spec :=
  mk_tool_spec "file_read" Low.

Definition file_write_tool : tool_spec :=
  mk_tool_spec "file_write" Medium.

Definition file_append_tool : tool_spec :=
  mk_tool_spec "file_append" Medium.

(* ----------------------------------------------------------------
   Validation function for tool configurations
   ---------------------------------------------------------------- *)

Definition valid_tool_config 
    (t : tool_spec) 
    (allowlist authorized : list string) : bool :=
  invocation_safe t allowlist authorized.

(* Theorem 15: Valid config implies safe invocation *)
Theorem valid_config_safe : forall t allowlist authorized,
  valid_tool_config t allowlist authorized = true ->
  invocation_safe t allowlist authorized = true.
Proof.
  intros t allowlist authorized H.
  unfold valid_tool_config in H.
  exact H.
Qed.

(* ----------------------------------------------------------------
   Composite safety check for multiple tools
   ---------------------------------------------------------------- *)

Definition all_tools_safe 
    (tools : list tool_spec)
    (allowlist authorized : list string) : bool :=
  forallb (fun t => valid_tool_config t allowlist authorized) tools.

(* Theorem 16: all_tools_safe with empty list *)
Theorem all_tools_safe_empty : forall allowlist authorized,
  all_tools_safe [] allowlist authorized = true.
Proof.
  intros allowlist authorized.
  reflexivity.
Qed.

(* Theorem 17: all_tools_safe with cons *)
Theorem all_tools_safe_cons : forall t tools allowlist authorized,
  all_tools_safe (t :: tools) allowlist authorized = true ->
  valid_tool_config t allowlist authorized = true /\
  all_tools_safe tools allowlist authorized = true.
Proof.
  intros t tools allowlist authorized H.
  simpl in H.
  apply Bool.andb_true_iff.
  exact H.
Qed.

(* ================================================================
   Summary of extracted functions
   
   Extract these to OCaml:
   - risk_gte, risk_lte
   - is_high_risk, is_medium_risk, is_low_risk
   - requires_authorization
   - is_authorized
   - tool_in_allowlist
   - invocation_safe
   - chain_safe
   - valid_tool_config
   - all_tools_safe
   ================================================================ *)

(* Compile guard *)
Theorem tool_safety_model_complete : True.
Proof.
  reflexivity.
Qed.

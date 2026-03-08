From Coq Require Import String List Bool.
Import ListNotations.
Open Scope string_scope.

(* ================================================================
   F3: Audit chain integrity — formal model of the HMAC-chained
   audit log.

   hash and hmac are abstract parameters (instantiated at extraction
   with Digestif.SHA256).  Proofs establish structural properties
   of the chain regardless of which hash function is used.
   ================================================================ *)

Module Type CRYPTO.
  Parameter hash : string -> string. (* SHA-256 of a string *)
  Parameter hmac : string -> string -> string.
  (* HMAC-SHA-256: key -> payload -> digest *)
  Parameter encode_signed_field : string -> string.
End CRYPTO.

Module Make (Crypto : CRYPTO).

Definition hash := Crypto.hash.
Definition hmac := Crypto.hmac.
Definition encode_signed_field := Crypto.encode_signed_field.

(* ----------------------------------------------------------------
   Data model
   ---------------------------------------------------------------- *)

Record audit_entry := {
  ae_timestamp   : string;
  ae_event_type  : string;
  ae_session_key : option string;
  ae_details     : string;
  ae_tool_name   : option string;
  ae_risk_level  : option string;
  ae_signature   : string;
  ae_prev_hash   : string
}.

Definition field_text (value : option string) : string :=
  match value with
  | Some s => s
  | None => ""
  end.

(* ----------------------------------------------------------------
   Pure chain computation (extractable)
   ---------------------------------------------------------------- *)

Definition compute_prev_hash (last_sig : option string) : string :=
  match last_sig with
  | None     => "genesis"
  | Some sig => hash sig
  end.

Definition compute_signature
    (key prev_hash timestamp event_type : string)
    (session_key : option string)
    (details : string)
    (tool_name risk_level : option string) : string :=
  hmac key
    (encode_signed_field prev_hash ++ "|"
     ++ encode_signed_field timestamp ++ "|"
     ++ encode_signed_field event_type ++ "|"
     ++ encode_signed_field (field_text session_key) ++ "|"
     ++ encode_signed_field details ++ "|"
     ++ encode_signed_field (field_text tool_name) ++ "|"
     ++ encode_signed_field (field_text risk_level)).

(* Construct a correctly-signed entry *)
Definition make_entry
    (key : string) (prev_sig : option string)
    (ts et : string)
    (session_key : option string)
    (det : string)
    (tool_name risk_level : option string) : audit_entry :=
  let ph := compute_prev_hash prev_sig in
  {| ae_timestamp   := ts;
     ae_event_type  := et;
     ae_session_key := session_key;
     ae_details     := det;
     ae_tool_name   := tool_name;
     ae_risk_level  := risk_level;
     ae_signature   :=
       compute_signature key ph ts et session_key det tool_name risk_level;
     ae_prev_hash   := ph |}.

(* ----------------------------------------------------------------
   Chain verification (extractable)
   ---------------------------------------------------------------- *)

(* Verify that a single entry correctly follows its predecessor *)
Definition verify_link
    (key : string) (prev_sig : option string) (entry : audit_entry) : bool :=
  String.eqb (ae_prev_hash entry) (compute_prev_hash prev_sig)
  && String.eqb (ae_signature entry)
    (compute_signature key (ae_prev_hash entry)
      (ae_timestamp entry) (ae_event_type entry)
      (ae_session_key entry) (ae_details entry)
      (ae_tool_name entry) (ae_risk_level entry)).

(* Verify a sequence of entries forms a valid chain *)
Fixpoint verify_chain
    (key : string) (prev_sig : option string) (entries : list audit_entry) : bool :=
  match entries with
  | []       => true
  | e :: rest =>
    verify_link key prev_sig e &&
    verify_chain key (Some (ae_signature e)) rest
  end.

(* The last signature in a chain *)
Fixpoint last_sig (prev_sig : option string) (entries : list audit_entry) : option string :=
  match entries with
  | []       => prev_sig
  | e :: rest => last_sig (Some (ae_signature e)) rest
  end.

(* ================================================================
   Proofs
   ================================================================ *)

(* P1: Empty chain is always valid *)
Theorem verify_chain_empty : forall key prev_sig,
  verify_chain key prev_sig [] = true.
Proof. reflexivity. Qed.

(* P2: A correctly-constructed entry passes verify_link *)
Theorem verify_link_make_entry : forall key prev_sig ts et session_key det tool_name risk_level,
  verify_link key prev_sig
    (make_entry key prev_sig ts et session_key det tool_name risk_level) = true.
Proof.
  intros key prev_sig ts et session_key det tool_name risk_level.
  unfold verify_link, make_entry. simpl.
  rewrite String.eqb_refl. simpl.
  rewrite String.eqb_refl. reflexivity.
Qed.

(* P3: Appending a correctly-signed entry preserves chain validity *)
Theorem verify_chain_append : forall key prev_sig entries e,
  verify_chain key prev_sig entries = true ->
  verify_link key (last_sig prev_sig entries) e = true ->
  verify_chain key prev_sig (entries ++ [e]) = true.
Proof.
  intros key prev_sig entries.
  (* Generalize prev_sig so the IH applies at the shifted prev_sig *)
  revert prev_sig.
  induction entries as [| h rest IH]; intros prev_sig e Hchain Hlink.
  - simpl in *. rewrite Hlink. reflexivity.
  - simpl in *.
    apply Bool.andb_true_iff in Hchain.
    destruct Hchain as [Hlink_h Hchain_rest].
    rewrite Hlink_h. simpl.
    exact (IH (Some (ae_signature h)) e Hchain_rest Hlink).
Qed.

(* P4: A chain built entirely from make_entry is always valid *)
Record audit_payload := {
  ap_timestamp : string;
  ap_event_type : string;
  ap_session_key : option string;
  ap_details : string;
  ap_tool_name : option string;
  ap_risk_level : option string
}.

Fixpoint build_chain
    (key : string) (prev_sig : option string)
    (payloads : list audit_payload) : list audit_entry :=
  match payloads with
  | [] => []
  | payload :: rest =>
    let e := make_entry key prev_sig
      (ap_timestamp payload)
      (ap_event_type payload)
      (ap_session_key payload)
      (ap_details payload)
      (ap_tool_name payload)
      (ap_risk_level payload) in
    e :: build_chain key (Some (ae_signature e)) rest
  end.

Theorem verify_chain_build : forall key prev_sig payloads,
  verify_chain key prev_sig (build_chain key prev_sig payloads) = true.
Proof.
  intros key prev_sig payloads.
  generalize dependent prev_sig.
  induction payloads as [| payload rest IH]; intro prev_sig.
  - reflexivity.
  - simpl.
    destruct payload as [ts et session_key det tool_name risk_level].
    rewrite verify_link_make_entry. simpl.
    exact
      (IH
         (Some
            (ae_signature
               (make_entry key prev_sig ts et session_key det tool_name risk_level)))).
Qed.

(* P5: A valid single-entry chain satisfies verify_link *)
Theorem verify_chain_single : forall key prev_sig e,
  verify_chain key prev_sig [e] = true <->
  verify_link key prev_sig e = true.
Proof.
  intros key prev_sig e. split.
  - intro H. simpl in H.
    apply Bool.andb_true_iff in H. exact (proj1 H).
  - intro H. simpl. rewrite H. reflexivity.
Qed.

(* P6: Chain validity is monotone — a suffix of a valid chain is valid *)
Lemma verify_chain_suffix : forall key prev_sig h rest,
  verify_chain key prev_sig (h :: rest) = true ->
  verify_chain key (Some (ae_signature h)) rest = true.
Proof.
  intros key prev_sig h rest H.
  simpl in H.
  apply Bool.andb_true_iff in H.
  exact (proj2 H).
Qed.

(* P7: last_sig distributes over append *)
Lemma last_sig_app : forall prev_sig xs ys,
  last_sig prev_sig (xs ++ ys) = last_sig (last_sig prev_sig xs) ys.
Proof.
  intros prev_sig xs.
  revert prev_sig.
  induction xs as [| h rest IH]; intros prev_sig ys.
  - reflexivity.
  - simpl. exact (IH (Some (ae_signature h)) ys).
Qed.

End Make.

Module AbstractCrypto <: CRYPTO.
  Parameter hash : string -> string.
  Parameter hmac : string -> string -> string.
  Parameter encode_signed_field : string -> string.
End AbstractCrypto.

Include Make(AbstractCrypto).

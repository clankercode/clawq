From Coq Require Import String List Bool Lia Nat PeanoNat Ascii.
Require Import Coq.Arith.PeanoNat.
Import ListNotations.
Open Scope string_scope.
Local Open Scope nat_scope.

(* ================================================================
   F7: Secret store encryption correctness — spec-only formalization.

   Proves correctness properties of secret encryption/decryption logic
   in src/secret_store.ml. Crypto primitives (AES-GCM, PBKDF2, Base64)
   are abstract parameters with axioms trusting Mirage_crypto.

   Key theorems:
   - decrypt_secret_plaintext_passthrough: plaintext inputs pass through unchanged
   - is_encrypted_correct: encrypted inputs have the $ENC: prefix and length > 5
   - resolve_secret_plaintext_passthrough / resolve_secret_env_var
   - resolve_secret_runtime_*: encrypted-branch behavior for missing master key,
     decrypt failure, and decrypt success

   No extraction (relies on C primitives via Mirage_crypto).

   Important modeling note:
   - The OCaml `decrypt_secret` returns plaintext unchanged for non-encrypted
     values, with explicit error strings for decode/decrypt failures.
   - This file models both a crypto-core decoder (`decrypt_secret_core`) and
     the API-level passthrough wrapper (`decrypt_secret`).
    ================================================================ *)

(* ----------------------------------------------------------------
   Abstract crypto primitives (trust Mirage_crypto).
   ---------------------------------------------------------------- *)

(* Types *)
Parameter key : Type.
Parameter nonce : Type.
Parameter bytes : Type.

(* AES-256-GCM authenticated encryption *)
Parameter aes_gcm_encrypt : key -> nonce -> bytes -> bytes.
Parameter aes_gcm_decrypt : key -> nonce -> bytes -> option bytes.

(* Random nonce generation (12 bytes) *)
Parameter random_nonce : unit -> nonce.

(* Base64 encoding/decoding *)
Parameter base64_encode : bytes -> string.
Parameter base64_decode : string -> option bytes.

(* Key derivation from passphrase *)
Parameter derive_key : string -> key.

(* ----------------------------------------------------------------
   Axioms: trust the crypto library.
   ---------------------------------------------------------------- *)

(* AES-GCM correctness: encrypt then decrypt is identity *)
Axiom aes_gcm_correct : forall k n m,
  aes_gcm_decrypt k n (aes_gcm_encrypt k n m) = Some m.

(* AES-GCM authentication: wrong key/ciphertext fails *)
Axiom aes_gcm_authentication_failure : forall k k' n m,
  k <> k' ->
  aes_gcm_decrypt k' n (aes_gcm_encrypt k n m) = None.

(* Base64 round-trip *)
Axiom base64_roundtrip : forall b,
  base64_decode (base64_encode b) = Some b.

(* Nonce uniqueness (birthday bound): with 12-byte random nonces,
   collision probability for n encryptions is n²/2^96.
   Stated as an assumption; no proof obligation. *)
Axiom nonce_collision_probability : forall n_encryptions,
  n_encryptions <= 4294967296 ->  (* 2^32 practical limit *)
  True.  (* assumption documented, not enforced in Coq *)

(* ----------------------------------------------------------------
   Framing: nonce ++ ciphertext, then base64 encode.
   ---------------------------------------------------------------- *)

(* Concatenate nonce and ciphertext (modeled abstractly) *)
Parameter nonce_ciphertext_concat : nonce -> bytes -> bytes.

(* Split combined bytes into (nonce, ciphertext) *)
Parameter split_nonce_ciphertext : bytes -> option (nonce * bytes).

(* Axiom: split undoes concat *)
Axiom split_concat_inverse : forall n ct,
  split_nonce_ciphertext (nonce_ciphertext_concat n ct) = Some (n, ct).

(* ----------------------------------------------------------------
    String operations (Coq stdlib implementations).
    ---------------------------------------------------------------- *)

Fixpoint has_prefix_aux (i : nat) (prefix s : string) : bool :=
  match prefix with
  | EmptyString => true
  | String c prefix' =>
      match get i s with
      | None => false
      | Some c' => if ascii_dec c c' then has_prefix_aux (S i) prefix' s else false
      end
  end.

Definition has_prefix (prefix s : string) : bool := has_prefix_aux 0 prefix s.

Definition string_length (s : string) : nat := String.length s.

Fixpoint drop_first_n (n : nat) (s : string) : string :=
  match n with
  | O => s
  | S n' =>
      match s with
      | EmptyString => EmptyString
      | String _ s' => drop_first_n n' s'
      end
  end.

Definition strip_prefix (prefix s : string) : option string :=
  if has_prefix prefix s then
    Some (drop_first_n (String.length prefix) s)
  else None.

(* Helper lemmas for prefix proofs *)
Fixpoint take (n : nat) (s : string) : string :=
  match n with
  | O => EmptyString
  | S n' =>
      match s with
      | EmptyString => EmptyString
      | String c s' => String c (take n' s')
      end
  end.

Lemma take_drop_first_n : forall n s, take n s ++ drop_first_n n s = s.
Proof.
  intros n s.
  generalize dependent s.
  induction n as [|n' IH].
  - intros s. simpl. reflexivity.
  - intros s. destruct s as [|c s'].
    + simpl. reflexivity.
    + simpl. rewrite IH. reflexivity.
Qed.

Lemma string_app_empty_r : forall s, s ++ EmptyString = s.
Proof.
  induction s as [|c s' IH].
  - reflexivity.
  - simpl. rewrite IH. reflexivity.
Qed.

Lemma string_app_assoc : forall s1 s2 s3, (s1 ++ s2) ++ s3 = s1 ++ (s2 ++ s3).
Proof.
  induction s1 as [|c s1' IH].
  - reflexivity.
  - intros s2 s3. simpl. rewrite IH. reflexivity.
Qed.

Lemma S_i_plus_len : forall i n, S i + n = S (i + n).
Proof. intros. lia. Qed.

Lemma take_succ_get : forall i s c,
  get i s = Some c ->
  take (S i) s = take i s ++ String c EmptyString.
Proof.
  intros n s c Hget.
  revert n c Hget.
  induction s as [|c' s' IH].
  - intros i c Hget. simpl in Hget. discriminate.
  - intros i c Hget. destruct i as [|i'].
    + simpl in Hget. injection Hget as ->. simpl. reflexivity.
    + simpl in Hget. simpl.
      specialize (IH i' c Hget).
      rewrite <- IH.
      reflexivity.
Qed.

Lemma has_prefix_aux_step : forall c i prefix s,
  has_prefix_aux (S i) prefix (String c s) = has_prefix_aux i prefix s.
Proof.
  intros c i prefix s.
  revert i.
  induction prefix as [|c' prefix' IH].
  - intros i. reflexivity.
  - intros i. simpl.
    destruct (get i s) eqn:Hget.
    + simpl. destruct (ascii_dec c' a).
      * specialize (IH (S i)). rewrite IH. reflexivity.
      * reflexivity.
    + reflexivity.
Qed.

Lemma has_prefix_aux_take : forall i prefix s,
  has_prefix_aux i prefix s = true ->
  take (i + String.length prefix) s = take i s ++ prefix.
Proof.
  intros i prefix s H.
  generalize dependent i.
  induction prefix as [|c prefix' IH].
  - intros i _. simpl. rewrite Nat.add_0_r. rewrite string_app_empty_r. reflexivity.
  - intros i H. simpl in H.
    destruct (get i s) as [c'|] eqn:Hget.
    + destruct (ascii_dec c c') eqn:Heq.
      * subst c'.
        assert (Htake : take (S i) s = take i s ++ String c EmptyString).
        { apply take_succ_get. exact Hget. }
        specialize (IH (S i) H).
        simpl.
        rewrite Nat.add_succ_r.
        rewrite <- (S_i_plus_len i (String.length prefix')).
        rewrite IH.
        rewrite Htake.
        rewrite string_app_assoc.
        simpl.
        reflexivity.
      * discriminate.
    + discriminate.
Qed.

Lemma has_prefix_take : forall prefix s,
  has_prefix prefix s = true ->
  take (String.length prefix) s = prefix.
Proof.
  intros prefix s H.
  unfold has_prefix in H.
  specialize (has_prefix_aux_take 0 prefix s H).
  intros Htake.
  simpl in Htake.
  exact Htake.
Qed.

(* Main theorems for prefix operations *)
Lemma has_prefix_app : forall prefix suffix,
  has_prefix prefix (prefix ++ suffix) = true.
Proof.
  intros prefix suffix.
  induction prefix as [|c prefix' IH].
  - reflexivity.
  - unfold has_prefix. simpl.
    rewrite (has_prefix_aux_step c 0 prefix' (prefix' ++ suffix)).
    simpl.
    destruct (ascii_dec c c) as [_ | H].
    + exact IH.
    + exfalso. apply H. reflexivity.
Qed.

Lemma strip_prefix_app : forall prefix suffix,
  strip_prefix prefix (prefix ++ suffix) = Some suffix.
Proof.
  intros prefix suffix.
  unfold strip_prefix.
  rewrite has_prefix_app.
  induction prefix as [|c prefix' IH].
  - reflexivity.
  - simpl. rewrite IH. reflexivity.
Qed.

Lemma has_prefix_strip_prefix : forall prefix s,
  has_prefix prefix s = true ->
  exists suffix : string, s = prefix ++ suffix /\ strip_prefix prefix s = Some suffix.
Proof.
  intros prefix s H.
  exists (drop_first_n (String.length prefix) s).
  split.
  - transitivity (take (String.length prefix) s ++ drop_first_n (String.length prefix) s).
    + symmetry. apply take_drop_first_n.
    + rewrite (has_prefix_take prefix s H). reflexivity.
  - unfold strip_prefix. rewrite H. reflexivity.
Qed.

Lemma string_length_dollar_app_gt1 : forall var_name,
  (1 <= String.length var_name)%nat ->
  (1 <? String.length ("$" ++ var_name))%nat = true.
Proof.
  intros var_name Hlen.
  simpl.
  apply Nat.ltb_lt.
  lia.
Qed.

Definition encrypted_prefix : string := "$ENC:".

Definition is_encrypted (value : string) : bool :=
  (5 <? string_length value) && has_prefix encrypted_prefix value.

Lemma is_encrypted_implies_secret_prefix : forall value,
  is_encrypted value = true -> has_prefix "$" value = true.
Proof.
  intros value H.
  unfold is_encrypted in H.
  apply andb_prop in H. destruct H as [_ H].
  pose proof (has_prefix_strip_prefix "$ENC:" value H) as [suffix [Heq _]].
  subst value.
  replace ("$ENC:" ++ suffix) with ("$" ++ "ENC:" ++ suffix) by reflexivity.
  rewrite has_prefix_app.
  reflexivity.
Qed.

(* Encrypt a plaintext and return with $ENC: prefix.
   Model: generate nonce, encrypt, concat nonce+ct, base64 encode, prepend prefix. *)
Definition encrypt_secret (k : key) (plaintext : bytes) : string :=
  let n := random_nonce tt in
  let ct := aes_gcm_encrypt k n plaintext in
  let combined := nonce_ciphertext_concat n ct in
  encrypted_prefix ++ base64_encode combined.

(* Decrypt only the encrypted branch.
   Model: strip prefix, base64 decode, split nonce+ct, decrypt. *)
Definition decrypt_secret_core (k : key) (value : string) : option bytes :=
  if is_encrypted value then
    match strip_prefix encrypted_prefix value with
    | None => None
    | Some encoded =>
        match base64_decode encoded with
        | None => None
        | Some combined =>
            match split_nonce_ciphertext combined with
            | None => None
            | Some (n, ct) => aes_gcm_decrypt k n ct
            end
        end
    end
  else None.

(* Coercion from string to bytes for passthrough modeling *)
Parameter string_to_bytes : string -> bytes.

(* API-level decrypt_secret behavior: encrypted values are decrypted,
   plaintext values are returned unchanged. We model the OCaml `Ok` shape as
   `Some` to stay close to the existing option-based crypto model. *)
Definition decrypt_secret (k : key) (value : string) : option bytes :=
  if is_encrypted value then decrypt_secret_core k value else Some (string_to_bytes value).

(* Resolve a secret value:
   - $ENC:... -> decrypt if encrypt_secrets enabled, else passthrough
   - $VAR -> look up environment variable
   - other -> passthrough *)
Definition resolve_secret (encrypt_secrets : bool) (lookup_env : string -> option string)
             (value : string) : string :=
  if (1 <? string_length value) && has_prefix "$" value then
    if is_encrypted value then
      if encrypt_secrets then
        (* For proof purposes, we model key availability via option.
           In practice, get_master_key () is called. *)
        value  (* simplified: actual implementation decrypts with master key *)
      else
        value
    else
      (* $ENV_VAR indirection *)
      match strip_prefix "$" value with
      | None => value
      | Some var_name =>
          match lookup_env var_name with
          | Some v => v
          | None => value
          end
      end
  else value.

(* ================================================================
   Proofs
   ================================================================ *)

(* Theorem 1: encrypt_secret/decrypt_secret identity.
   Kept as an axiom because it depends on concrete framing/string-length facts
   from runtime encoders that are abstract in this model. *)
Axiom encrypt_decrypt_identity : forall k plaintext,
  decrypt_secret k (encrypt_secret k plaintext) = Some plaintext.

(* Theorem 2: plaintext inputs pass through unchanged at the API level. *)
Theorem decrypt_secret_plaintext_passthrough : forall k value,
  is_encrypted value = false ->
  decrypt_secret k value = Some (string_to_bytes value).
Proof.
  intros k value Hplain.
  unfold decrypt_secret.
  rewrite Hplain.
  reflexivity.
Qed.

(* Theorem 3: encrypted inputs have the expected prefix and minimum length. *)
Theorem is_encrypted_correct : forall value,
  is_encrypted value = true ->
  has_prefix encrypted_prefix value = true /\ 5 < string_length value.
Proof.
  intros value H.
  unfold is_encrypted in H.
  apply andb_prop in H.
  destruct H as [Hlen Hprefix].
  split.
  - exact Hprefix.
  - apply Nat.ltb_lt in Hlen. exact Hlen.
Qed.

(* Theorem 4: is_encrypted rejects non-prefixed strings. *)
Theorem is_encrypted_rejects_nonprefixed : forall value,
  string_length value <= 5 \/ has_prefix encrypted_prefix value = false ->
  is_encrypted value = false.
Proof.
  intros value [Hlen | Hprefix].
  - (* Length condition fails *)
    unfold is_encrypted.
    apply Nat.ltb_ge in Hlen.
    rewrite Hlen.
    reflexivity.
  - (* Prefix condition fails *)
    unfold is_encrypted.
    destruct (5 <? string_length value) eqn:Hlt.
    + rewrite Hprefix. reflexivity.
    + reflexivity.
Qed.

(* Theorem 5: resolve_secret handles plaintext passthrough. *)
Theorem resolve_secret_plaintext_passthrough : forall encrypt_secrets lookup_env value,
  string_length value <= 1 \/ has_prefix "$" value = false ->
  resolve_secret encrypt_secrets lookup_env value = value.
Proof.
  intros encrypt_secrets lookup_env value [Hlen | Hprefix].
  - (* Short string case *)
    unfold resolve_secret.
    apply Nat.ltb_ge in Hlen.
    rewrite Hlen.
    reflexivity.
  - (* Non-dollar-prefix case *)
    unfold resolve_secret.
    destruct (1 <? string_length value) eqn:Hlt.
    + rewrite Hprefix. reflexivity.
    + reflexivity.
Qed.

(* Theorem 6: resolve_secret handles $ENV_VAR indirection. *)
Theorem resolve_secret_env_var : forall encrypt_secrets lookup_env var_name value,
  1 <= string_length var_name ->
  is_encrypted ("$" ++ var_name) = false ->
  lookup_env var_name = Some value ->
  resolve_secret encrypt_secrets lookup_env ("$" ++ var_name) = value.
Proof.
  intros encrypt_secrets lookup_env var_name value Hlen Hnotenc Hlookup.
  unfold resolve_secret.
  assert
    (Hguard :
       (1 <? string_length ("$" ++ var_name)) &&
       has_prefix "$" ("$" ++ var_name) = true).
    {
      rewrite string_length_dollar_app_gt1 by exact Hlen.
      rewrite has_prefix_app.
      reflexivity.
    }
  rewrite Hguard.
  rewrite Hnotenc.
  rewrite strip_prefix_app.
  rewrite Hlookup.
  reflexivity.
Qed.

(* Runtime-exact behavior for encrypted branches in resolve_secret. *)
Inductive master_key_state :=
  | MasterKeyMissing
  | MasterKeyPresent (k : key).

Inductive decrypt_outcome :=
  | DecryptOk (plaintext : string)
  | DecryptFail.

Parameter decrypt_with_master : key -> string -> decrypt_outcome.

Definition resolve_secret_runtime
    (encrypt_secrets : bool)
    (lookup_env : string -> option string)
    (master : master_key_state)
    (value : string) : string :=
  if (1 <? string_length value) && has_prefix "$" value then
    if is_encrypted value then
      if encrypt_secrets then
        match master with
        | MasterKeyMissing => value
        | MasterKeyPresent k =>
            match decrypt_with_master k value with
            | DecryptOk plaintext => plaintext
            | DecryptFail => value
            end
        end
      else value
    else
      match strip_prefix "$" value with
      | None => value
      | Some var_name =>
          match lookup_env var_name with
          | Some v => v
          | None => value
          end
      end
  else value.

Theorem resolve_secret_runtime_missing_master_key :
  forall lookup_env value,
    is_encrypted value = true ->
    resolve_secret_runtime true lookup_env MasterKeyMissing value = value.
Proof.
  intros lookup_env value Henc.
  unfold resolve_secret_runtime.
  assert (Hlen : 1 <? string_length value = true).
  {
    pose proof (is_encrypted_correct value Henc) as [_ Hgt5].
    apply Nat.ltb_lt.
    lia.
  }
  rewrite Hlen.
  rewrite (is_encrypted_implies_secret_prefix value Henc).
  simpl.
  rewrite Henc.
  reflexivity.
Qed.

Theorem resolve_secret_runtime_decrypt_failure :
  forall lookup_env value k,
    is_encrypted value = true ->
    decrypt_with_master k value = DecryptFail ->
    resolve_secret_runtime true lookup_env (MasterKeyPresent k) value = value.
Proof.
  intros lookup_env value k Henc Hfail.
  unfold resolve_secret_runtime.
  assert (Hlen : 1 <? string_length value = true).
  {
    pose proof (is_encrypted_correct value Henc) as [_ Hgt5].
    apply Nat.ltb_lt.
    lia.
  }
  rewrite Hlen.
  rewrite (is_encrypted_implies_secret_prefix value Henc).
  simpl.
  rewrite Henc.
  rewrite Hfail.
  reflexivity.
Qed.

Theorem resolve_secret_runtime_decrypt_success :
  forall lookup_env value k plaintext,
    is_encrypted value = true ->
    decrypt_with_master k value = DecryptOk plaintext ->
    resolve_secret_runtime true lookup_env (MasterKeyPresent k) value = plaintext.
Proof.
  intros lookup_env value k plaintext Henc Hok.
  unfold resolve_secret_runtime.
  assert (Hlen : 1 <? string_length value = true).
  {
    pose proof (is_encrypted_correct value Henc) as [_ Hgt5].
    apply Nat.ltb_lt.
    lia.
  }
  rewrite Hlen.
  rewrite (is_encrypted_implies_secret_prefix value Henc).
  simpl.
  rewrite Henc.
  rewrite Hok.
  reflexivity.
Qed.

(* ================================================================
   Configuration encryption (encrypt_config_secrets).
   
   This function encrypts all provider API keys in a JSON config.
   For the Coq model, we abstract JSON as a nested association list
   and prove that encryption preserves structure.
   ================================================================ *)

(* Abstract JSON type for proof purposes *)
Definition json_value := string.  (* simplified *)
Definition provider_name := string.
Definition api_key := string.

(* Check if a key should be encrypted: not already encrypted, not an env var reference *)
Definition should_encrypt (api_key : string) : bool :=
  negb (is_encrypted api_key) &&
  (1 <? string_length api_key) &&
  negb (has_prefix "$" api_key).

(* Encrypt a single provider's API key (abstracted) *)
Definition encrypt_provider_key (k : key) (api_key : api_key) : string :=
  if should_encrypt api_key then
    encrypted_prefix ++ "encrypted_data"  (* simplified for proof *)
  else api_key.

(* Theorem 7: should_encrypt rejects already-encrypted values *)
Theorem should_encrypt_rejects_encrypted : forall api_key,
  is_encrypted api_key = true ->
  should_encrypt api_key = false.
Proof.
  intros api_key Henc.
  unfold should_encrypt.
  rewrite Henc.
  reflexivity.
Qed.

(* Theorem 8: should_encrypt rejects env var references *)
Theorem should_encrypt_rejects_env_var : forall var_name,
  1 <= string_length var_name ->
  should_encrypt ("$" ++ var_name) = false.
Proof.
  intros var_name Hlen.
  unfold should_encrypt.
  rewrite has_prefix_app.
  rewrite andb_false_r.
  reflexivity.
Qed.

Theorem encrypt_provider_key_avoids_double_encryption : forall k api_key,
  is_encrypted api_key = true ->
  encrypt_provider_key k api_key = api_key.
Proof.
  intros k api_key Henc.
  unfold encrypt_provider_key.
  unfold should_encrypt.
  rewrite Henc.
  reflexivity.
Qed.

Theorem encrypt_provider_key_preserves_env_refs : forall k var_name,
  1 <= string_length var_name ->
  encrypt_provider_key k ("$" ++ var_name) = "$" ++ var_name.
Proof.
  intros k var_name Hlen.
  unfold encrypt_provider_key.
  apply should_encrypt_rejects_env_var in Hlen.
  rewrite Hlen.
  reflexivity.
Qed.

(* ================================================================
   Documentation: nonce uniqueness bound.
   
   With 12-byte (96-bit) random nonces, the birthday-bound collision
   probability for n encryptions is n²/2^96. This is stated as an
   assumption, not a theorem (probability theory not formalized).
   
   For n = 2^32 (4 billion encryptions), this is ~2^-32 probability.
   ================================================================ *)

(* This is documentation only; see nonce_collision_probability axiom above. *)

(* ================================================================
   Summary of what was proved:
   - decrypt_secret_plaintext_passthrough: non-encrypted values return unchanged
   - is_encrypted_correct / is_encrypted_rejects_nonprefixed
   - resolve_secret_plaintext_passthrough / resolve_secret_env_var
   - resolve_secret_runtime_*: exact encrypted-branch behavior for
     missing master key, decrypt failure, and decrypt success
   - should_encrypt_* and encrypt_provider_key_*: avoid double encryption,
     preserve env-var references

   Trusted boundaries (explicit):
   - encrypt_decrypt_identity is axiomatized over abstract framing/crypto
   - Remaining axioms are aes_gcm_correct, aes_gcm_authentication_failure,
     base64_roundtrip, nonce_collision_probability, and split_concat_inverse
   ================================================================ *)

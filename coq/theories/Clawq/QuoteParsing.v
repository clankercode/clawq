From Coq Require Import Ascii String List Bool Lia.
Import ListNotations.
Open Scope char_scope.
Local Open Scope list_scope.

(* ================================================================
   F6: Shell injection prevention -- quote-aware tokenizer model.

   Key guarantee (split_words_metachar_free): if the input string
   contains no shell metachar, then every token produced by
   split_words also contains no shell metachar.

   Formalizes split_command_words + has_unsafe_shell_syntax from
   src/tools_builtin.ml. Extracted functions replace the OCaml
   versions; the OCaml implementations are kept as assertions.
   ================================================================ *)

(* ----------------------------------------------------------------
   Metacharacter blacklist.
   Covers POSIX shell injection vectors: ; | & > < backtick $ ! \n \r
   Conservative: $ is fully blocked (not just dollar-paren). *)
Definition is_metachar (c : Ascii.ascii) : bool :=
  Ascii.eqb c ";" || Ascii.eqb c "|" || Ascii.eqb c "&" ||
  Ascii.eqb c ">" || Ascii.eqb c "<" || Ascii.eqb c "`" ||
  Ascii.eqb c "$" || Ascii.eqb c "!" ||
  Ascii.eqb c (Ascii.ascii_of_nat 10) ||   (* newline *)
  Ascii.eqb c (Ascii.ascii_of_nat 13).     (* carriage return *)

(* string_has_metachar: true iff some char in s is a metachar. *)
Fixpoint string_has_metachar (s : string) : bool :=
  match s with
  | EmptyString => false
  | String c rest =>
    if is_metachar c then true else string_has_metachar rest
  end.

(* is_shell_safe: true iff no metachar appears anywhere in s. *)
Definition is_shell_safe (s : string) : bool :=
  negb (string_has_metachar s).

(* ----------------------------------------------------------------
   Character helpers.
   char_backslash = ascii 92, char_sq = ascii 39 (single-quote),
   char_dq = ascii 34 (double-quote). *)
Definition char_backslash := Ascii.ascii_of_nat 92.
Definition char_sq        := Ascii.ascii_of_nat 39.
Definition char_dq        := Ascii.ascii_of_nat 34.
Definition char_space     := Ascii.ascii_of_nat 32.
Definition char_tab       := Ascii.ascii_of_nat 9.

Definition is_whitespace (c : Ascii.ascii) : bool :=
  Ascii.eqb c char_space || Ascii.eqb c char_tab.

Definition is_quote_char (c : Ascii.ascii) : bool :=
  Ascii.eqb c char_sq || Ascii.eqb c char_dq.

(* ----------------------------------------------------------------
   String <-> list ascii conversions. *)

Fixpoint list_of_string (s : string) : list Ascii.ascii :=
  match s with
  | EmptyString => []
  | String c rest => c :: list_of_string rest
  end.

Fixpoint string_of_chars (cs : list Ascii.ascii) : string :=
  match cs with
  | [] => EmptyString
  | c :: rest => String c (string_of_chars rest)
  end.

(* ----------------------------------------------------------------
   Tokenizer.
   Design: cur is built in forward order using appending.
   This avoids rev in the output word, simplifying proofs. *)

Inductive quote_state : Type :=
  | NoQuote  : quote_state
  | InQuote  : Ascii.ascii -> quote_state.

(* flush_word: if cur is non-empty, prepend string_of_chars cur to words. *)
Definition flush_word
    (cur   : list Ascii.ascii)
    (words : list string) : list string :=
  match cur with
  | [] => words
  | _  => string_of_chars cur :: words
  end.

(* parse_chars: process chars, building tokens.
   cur: current token chars in forward order.
   words: completed tokens in reverse order.
   Returns Some (tokens in order) or None for unterminated quote. *)
Fixpoint parse_chars
    (chars : list Ascii.ascii)
    (cur   : list Ascii.ascii)
    (words : list string)
    (q     : quote_state) : option (list string) :=
  match chars with
  | [] =>
    match q with
    | InQuote _ => None
    | NoQuote   => Some (rev (flush_word cur words))
    end
  | c :: rest =>
    match q with
    | NoQuote =>
      if is_whitespace c then
        parse_chars rest [] (flush_word cur words) NoQuote
      else if is_quote_char c then
        parse_chars rest cur words (InQuote c)
      else if Ascii.eqb c char_backslash then
        match rest with
        | []            =>
          (* backslash at end: include it as literal, then flush *)
          Some (rev (flush_word (cur ++ [c]) words))
        | next :: rest' =>
          (* escape: skip backslash, include next char *)
          parse_chars rest' (cur ++ [next]) words NoQuote
        end
      else
        parse_chars rest (cur ++ [c]) words NoQuote
    | InQuote q_char =>
      if Ascii.eqb c q_char then
        parse_chars rest cur words NoQuote
      else if Ascii.eqb c char_backslash && Ascii.eqb q_char char_dq then
        match rest with
        | []            =>
          (* backslash at end inside double-quote: error *)
          None
        | next :: rest' =>
          parse_chars rest' (cur ++ [next]) words (InQuote q_char)
        end
      else
        parse_chars rest (cur ++ [c]) words (InQuote q_char)
    end
  end.

(* split_words: top-level tokenizer. *)
Definition split_words (s : string) : option (list string) :=
  parse_chars (list_of_string s) [] [] NoQuote.

(* ================================================================
   Helper lemmas
   ================================================================ *)

(* string_of_chars of a metachar-free list is shell-safe. *)
Lemma string_of_chars_safe :
  forall cs,
  (forall c, In c cs -> is_metachar c = false) ->
  is_shell_safe (string_of_chars cs) = true.
Proof.
  intros cs Hcs.
  unfold is_shell_safe.
  apply negb_true_iff.
  induction cs as [| c rest IH].
  - reflexivity.
  - simpl.
    rewrite (Hcs c (or_introl eq_refl)).
    apply IH.
    intros x Hx. apply Hcs. right. exact Hx.
Qed.

(* flush_word produces only safe words when cur is metachar-free. *)
Lemma flush_word_safe :
  forall cur words,
  (forall c, In c cur -> is_metachar c = false) ->
  (forall w, In w words -> is_shell_safe w = true) ->
  forall w, In w (flush_word cur words) -> is_shell_safe w = true.
Proof.
  intros cur words Hcur Hwords w Hin.
  unfold flush_word in Hin.
  destruct cur as [| h t].
  - exact (Hwords w Hin).
  - destruct Hin as [Heq | Hin].
    + rewrite <- Heq.
      apply string_of_chars_safe. exact Hcur.
    + exact (Hwords w Hin).
Qed.

(* string_has_metachar = existsb over the char list. *)
Lemma string_has_metachar_list :
  forall s,
  string_has_metachar s = existsb is_metachar (list_of_string s).
Proof.
  induction s as [| c rest IH].
  - reflexivity.
  - simpl.
    destruct (is_metachar c); simpl.
    + reflexivity.
    + exact IH.
Qed.

(* If is_shell_safe s = true then no char in list_of_string s is a metachar. *)
Lemma shell_safe_no_metachar :
  forall s,
  is_shell_safe s = true ->
  forall c, In c (list_of_string s) -> is_metachar c = false.
Proof.
  intros s Hsafe c Hc.
  unfold is_shell_safe in Hsafe.
  apply negb_true_iff in Hsafe.
  rewrite string_has_metachar_list in Hsafe.
  destruct (is_metachar c) eqn:Emc.
  - exfalso.
    assert (Hex : existsb is_metachar (list_of_string s) = true).
    { apply existsb_exists. exists c. exact (conj Hc Emc). }
    rewrite Hex in Hsafe. discriminate.
  - reflexivity.
Qed.

(* Helper: In c (cur ++ [x]) -> is_metachar c = false given
   metachar-freeness of cur and x. *)
Lemma in_append_safe :
  forall cur x c,
  (forall c', In c' cur -> is_metachar c' = false) ->
  is_metachar x = false ->
  In c (cur ++ [x]) ->
  is_metachar c = false.
Proof.
  intros cur x c Hcur Hx Hc.
  apply in_app_iff in Hc.
  destruct Hc as [Hc | Hc].
  - exact (Hcur c Hc).
  - simpl in Hc. destruct Hc as [<- | []]. exact Hx.
Qed.

Lemma append_char_safe :
  forall cur x,
  (forall c, In c cur -> is_metachar c = false) ->
  is_metachar x = false ->
  forall c, In c (cur ++ [x]) -> is_metachar c = false.
Proof.
  intros cur x Hcur Hx c Hc.
  eapply in_append_safe; eauto.
Qed.

(* ================================================================
   Main safety theorem
   ================================================================ *)

(* Core induction: if no metachar in chars and none in cur, all
   tokens produced are metachar-free. *)
Lemma parse_chars_safe :
  forall chars cur words q result,
  parse_chars chars cur words q = Some result ->
  (forall c, In c chars -> is_metachar c = false) ->
  (forall c, In c cur   -> is_metachar c = false) ->
  (forall w, In w words -> is_shell_safe w = true) ->
  forall w, In w result -> is_shell_safe w = true.
Proof.
  assert
    (Hmain :
      forall n chars cur words q result,
      length chars <= n ->
      parse_chars chars cur words q = Some result ->
      (forall c, In c chars -> is_metachar c = false) ->
      (forall c, In c cur -> is_metachar c = false) ->
      (forall w, In w words -> is_shell_safe w = true) ->
      forall w, In w result -> is_shell_safe w = true).
  {
    induction n as [| n IHn];
      intros chars cur words q result Hlen Hparse Hchars Hcur Hwords w Hin.
    - destruct chars as [| c rest].
      + simpl in Hparse.
        destruct q as [| q_char].
        * injection Hparse as <-.
          apply in_rev in Hin.
          eapply flush_word_safe; eauto.
        * discriminate.
      + simpl in Hlen. lia.
    - destruct chars as [| c rest].
      + simpl in Hparse.
        destruct q as [| q_char].
        * injection Hparse as <-.
          apply in_rev in Hin.
          eapply flush_word_safe; eauto.
        * discriminate.
      + simpl in Hparse.
        assert (Hc : is_metachar c = false) by (apply Hchars; left; reflexivity).
        assert (Hrest : forall c', In c' rest -> is_metachar c' = false).
        { intros c' Hc'. apply Hchars. right. exact Hc'. }
        destruct q as [| q_char].
        * destruct (is_whitespace c) eqn:Ews.
          -- eapply IHn; eauto.
             ++ simpl in Hlen. lia.
             ++ intros x Hx. contradiction.
             ++ eapply flush_word_safe; eauto.
          -- destruct (is_quote_char c) eqn:Eqc.
             ++ eapply IHn; eauto.
                ** simpl in Hlen. lia.
             ++ destruct (Ascii.eqb c char_backslash) eqn:Ebs.
                ** destruct rest as [| next rest'].
                   --- injection Hparse as <-.
                       apply in_rev in Hin.
                       eapply (flush_word_safe (cur ++ [c]) words); eauto.
                       intros x Hx. eapply (append_char_safe cur c); eauto.
                   --- assert (Hnext : is_metachar next = false)
                         by (apply Hrest; left; reflexivity).
                       eapply IHn; eauto.
                       +++ simpl in Hlen. lia.
                       +++ intros c' Hc'. apply Hrest. right. exact Hc'.
                       +++ intros x Hx. eapply (append_char_safe cur next); eauto.
                ** eapply IHn; eauto.
                   --- simpl in Hlen. lia.
                   --- intros x Hx. eapply (append_char_safe cur c); eauto.
        * destruct (Ascii.eqb c q_char) eqn:Ecq.
          -- eapply IHn; eauto.
             ++ simpl in Hlen. lia.
          -- destruct (Ascii.eqb c char_backslash && Ascii.eqb q_char char_dq) eqn:Eesc.
             ++ apply andb_prop in Eesc as [_ _].
                destruct rest as [| next rest'].
                ** discriminate.
                ** assert (Hnext : is_metachar next = false)
                     by (apply Hrest; left; reflexivity).
                   eapply IHn; eauto.
                   --- simpl in Hlen. lia.
                   --- intros c' Hc'. apply Hrest. right. exact Hc'.
                   --- intros x Hx. eapply (append_char_safe cur next); eauto.
             ++ eapply IHn; eauto.
                ** simpl in Hlen. lia.
                ** intros x Hx. eapply (append_char_safe cur c); eauto.
  }
  intros chars cur words q result Hparse Hchars Hcur Hwords w Hin.
  eapply Hmain; eauto.
Qed.

(* ================================================================
   Top-level theorem: split_words preserves shell safety.
   ================================================================ *)

Theorem split_words_metachar_free :
  forall s tokens,
  split_words s = Some tokens ->
  is_shell_safe s = true ->
  forall t, In t tokens -> is_shell_safe t = true.
Proof.
  intros s tokens Hsplit Hsafe t Ht.
  unfold split_words in Hsplit.
  eapply parse_chars_safe; eauto.
  - eapply shell_safe_no_metachar. exact Hsafe.
  - intros c Hc. inversion Hc.
Qed.

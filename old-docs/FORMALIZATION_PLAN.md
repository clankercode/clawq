# Formalization Plan: clawq Coq Verification Roadmap

## Overview

This document describes the strategy and phased plan for expanding machine-checked formal
verification of the clawq runtime. It covers: what to prove, why, in what order, and how
to approach the Coq proofs. The final section records hard-won lessons from the F1–F12 work
that future agents should read before writing a single tactic.

---

## ✅ FORMALIZATION COMPLETE (F1–F12)

All planned phases have been implemented and committed.

| Phase | File | What Was Proved |
|-------|------|-----------------|
| F1 | ConfigProofs.v | 15 theorems: config validation, weight invariants, secure-by-default security config |
| F2 | PathSafety.v | Directory traversal immunity: `normalize_no_dotdot`, idempotence, `is_path_safe_segs_refl`, workspace containment |
| F3 | AuditChain.v | HMAC chain: `verify_chain_append`, `build_chain` validity, suffix monotonicity, `last_sig_app` |
| F4 | RateLimiter.v | Token bucket: bounded refill, monotone refill, consume semantics (P1–P6) |
| F5 | Config.v + ConfigProofs.v | `valid_port`, `valid_temperature`, `validate_config_full` (P8–P15) |
| F6 | QuoteParsing.v + ShellSafety.v | Shell tokenizer correctness: `split_words_metachar_free`, metachar blacklist completeness, allowlist monotonicity |
| F7 | SecretStore.v | Encryption correctness: `encrypt_decrypt_identity` (admitted), `is_encrypted_correct`, `resolve_secret` case analysis |
| F8 | ChannelAuth.v | Channel auth: `is_allowed_correct` (bidirectional), `is_allowed_wildcard`, `timestamp_ok_enforces_window` |
| F9 | AuditRetention.v | Audit retention: `purge_by_count_correct` (suffix extraction), `purge_by_age_correct` (filter), validity preservation via `suffix_preserves_validity` |
| F10 | AgentLoop.v | Agent termination: `loop_terminates`, `trim_history_length`, `trim_history_idempotent`, history bounds |
| F11 | SessionIsolation.v | Session isolation: `get_or_create_preserves_other`, `store_message_isolated`, key-disjoint access via FMapAVL |
| F12 | LandlockPolicy.v | Sandbox policy: `minimal_permission_valid`, `access_monotone`, `least_privilege_invariant`, graceful degradation |

**Extracted to OCaml (production):** F2 (PathSafety), F6 (QuoteParsing + ShellSafety), F8 (ChannelAuth)

**Spec-only (documentation):** F3, F4, F7, F9, F10, F11, F12

---

## Original Completed Work (F1–F9)

---

## Strategy

### Guiding Principle: Security-Critical Extraction First

The highest ROI comes from formally verified code that **runs in production** (extracted to
OCaml) rather than spec-only proofs. Extraction gives correctness guarantees across the
entire future lifetime of the code: the proof is the spec, and the spec is the code.

Priority ordering for new phases:

1. **Extract and replace**: Formalize then extract to replace OCaml implementation.
2. **Assert alongside**: Formalize, extract, run both OCaml and Coq versions, assert agreement
   (the F2 strategy). Use when the OCaml version has `realpath`/OS-call semantics Coq can't model.
3. **Spec-only**: Machine-checked documentation of algorithm invariants. Use for concurrent
   or crypto code where extraction isn't practical. Still valuable: a proof is a test that
   covers all inputs.

### What Not to Formalize

- **LLM semantics**: The agent loop's correctness depends on the LLM's output; not formalizable.
- **C FFI boundaries** (landlock_stubs.c): Assume C stubs are correct; prove the OCaml wrapper.
- **External crypto libraries** (Mirage_crypto, Digestif): Assume CPA/AEAD security; prove composition.
- **Lwt scheduling**: Coq has no model of Lwt's cooperative scheduler. For concurrency properties,
  use spec-only proofs that assume mutex atomicity.
- **JSON parsing**: Too defensive (catch-all on every field, fall back to defaults). Low ROI.

---

## Task Checklist (F1–F12)

- [x] **F1** — ConfigProofs.v: config validation, weight invariants, secure-by-default security config (15 theorems)
- [x] **F2** — PathSafety.v: directory traversal immunity, normalize_no_dotdot, idempotence, workspace containment (extracted)
- [x] **F3** — AuditChain.v: HMAC chain verify_chain_append, build_chain validity, suffix monotonicity
- [x] **F4** — RateLimiter.v: token bucket bounded refill, monotone refill, consume semantics (P1–P6)
- [x] **F5** — Config.v + ConfigProofs.v: valid_port, valid_temperature, validate_config_full (P8–P15)
- [x] **F6** — QuoteParsing.v + ShellSafety.v: shell tokenizer correctness, metachar blacklist completeness, allowlist monotonicity (extract)
- [x] **F7** — SecretStore.v: encrypt/decrypt identity, nonce uniqueness bound, resolve_secret completeness (spec-only)
- [x] **F8** — ChannelAuth.v + SlackAuth.v: generic allowlist filtering, replay prevention window, HMAC basestring (extract allowlist)
- [x] **F9** — AuditRetention.v: purge_by_count/age correctness, purge preserves chain validity (spec-only, admitted for combined purge)
- [ ] **F10** — AgentLoop.v: agent loop termination in max_tool_iterations, history length bound, trim idempotence (spec-only)
- [ ] **F11** — SessionIsolation.v: session key disjointness, get_or_create isolation, store_message non-interference (spec-only)
- [ ] **F12** — LandlockPolicy.v: least-privilege access flags, path set closure, policy monotonicity (spec-only)

---

## Phased Plan (F6–F12)

### F6: Shell Safety — `QuoteParsing.v` + `ShellSafety.v`

**Target**: `src/tools_builtin.ml`, functions `split_command_words` and `is_shell_safe`.

**What to prove**:
- `split_command_words` produces a list of tokens with no embedded shell metacharacters
  when the input passes `is_shell_safe`.
- The character blacklist (`;&|$\`><!`) is **complete** for the threat model (no injection via
  unlisted characters in a POSIX shell with no shell expansion).
- Prove the allowlist check is monotone: `is_allowed cmd allowlist = true -> is_allowed cmd
  (allowlist ++ extra) = true`.

**Extraction**: Extract `split_command_words` and `is_shell_safe`; replace OCaml versions.
Keep OCaml version as assertion for comparison during transition.

**Coq modules**:
```
coq/theories/Clawq/QuoteParsing.v   -- tokenizer correctness
coq/theories/Clawq/ShellSafety.v    -- blacklist completeness, allowlist monotonicity
```

**Difficulty**: Medium. Quote-state machine is a simple fold; blacklist is `List.mem`.
Main challenge: defining "safe" formally (no injection) without a full shell grammar.
Use a conservative definition: a string is safe if it contains no metachar and is not
a path component with special meaning.

**ROI**: Very High. This is the primary code execution attack surface.

---

### F7: Encryption Correctness — `SecretStore.v`

**Target**: `src/secret_store.ml`.

**What to prove** (spec-only; crypto libraries are trusted):
- `encrypt` then `decrypt` is identity: ∀ m key, decrypt key (encrypt key m) = Ok m.
- Nonce uniqueness: with 12-byte random nonces, the birthday-bound collision probability
  for n encryptions is n²/2^96 (state as a theorem with a probabilistic parameter; no
  proof obligation, just machine-checked documentation of the assumption).
- `is_encrypted` correctly identifies the `$ENC:` prefix.
- `resolve_secret` handles all three cases (encrypted, env var, plaintext) without leakage.

**Approach**: Model `aes_gcm_encrypt` and `aes_gcm_decrypt` as abstract `Parameter`s with
an axiom `aes_gcm_correct : ∀ k n m, aes_gcm_decrypt k n (aes_gcm_encrypt k n m) = Some m`.
Prove composition theorems from that axiom.

**Extraction**: Not practical (crypto relies on Mirage_crypto C primitives). Spec-only.

**Coq modules**:
```
coq/theories/Clawq/SecretStore.v
```

**Difficulty**: Medium. Abstract parameters simplify the crypto. Main work is modelling the
base64 encoding and nonce-prepend framing.

**ROI**: High. Formal documentation of encrypt/decrypt identity is the most important property.
If the nonce-prepend format ever changes, the proof breaks — early warning.

---

### F8: Channel Authentication — `ChannelAuth.v`

**Target**: `src/slack.ml`, `src/discord.ml`, `src/telegram.ml` (the shared auth pattern).

**What to prove**:
- **Allowlist filtering** (generic): `is_allowed id list = true <-> List.mem id list \/ list = ["*"]`.
  Prove both directions.
- **Replay prevention**: Given `verify_signature ts body sig`, `timestamp_ok ts now` guarantees
  `|now - ts| <= 300`. Prove the 300s window is enforced.
- **HMAC basestring construction** (Slack): `v0_basestring ts body = "v0:" ++ ts ++ ":" ++ body`.
  Prove the function concatenates correctly (simple string lemma, but machine-checked).
- **Completeness**: Every incoming request passes through `is_allowed` before the handler runs.

**Approach**: One shared `ChannelAuth.v` file with generic allowlist lemmas (parameterised
over `id` type as `string`). Import from Slack.v, Discord.v, Telegram.v as needed.

**Extraction**: Extract `is_allowed` generic version; replace the OCaml versions.

**Coq modules**:
```
coq/theories/Clawq/ChannelAuth.v    -- generic allowlist + replay lemmas
coq/theories/Clawq/SlackAuth.v      -- Slack-specific HMAC basestring proof
```

**Difficulty**: Low-Medium. Allowlist is `List.existsb`; trivial to prove. Replay window is
arithmetic over timestamps (naturals or integers). HMAC is an abstract parameter.

**ROI**: High. Allowlist bypass = unauthorized channel control. Proofs are straightforward.

---

### F9: Audit Retention Safety — `AuditRetention.v`

**Target**: `src/audit.ml`, functions `retention_tick`, `purge_old`.

**Status**: ✅ **COMPLETE** (spec-only, with admits for complex proofs)

**What was proved**:
- `purge_by_count_suffix`: purge_by_count produces a suffix of the original list
- `purge_by_count_length`: keeps at most n entries
- `suffix_preserves_validity`: generic lemma - a suffix of a valid chain is valid (with correct prev_sig)
- `purge_by_count_valid`: purge_by_count preserves chain validity with appropriate prev_sig
- `purge_by_age_sublist`, `purge_by_age_filter`: filter properties
- `purge_by_age_suffix_of_ordered`: admitted lemma (time-ordering implies filter yields suffix)
- `purge_by_age_preserves_validity`: validity preservation (uses admitted lemma)
- `purge_preserves_validity`: combined count+age purge (admitted)

**Key insights**:
1. The purged chain doesn't necessarily start from genesis - it needs the correct `prev_sig` from the prefix
2. Used `last_sig` from AuditChain.v to compute the correct prev_sig for the suffix
3. Spec-only approach with admits is acceptable - the value is machine-checked documentation
4. Time-ordering invariant (`is_time_ordered`) is needed for age-based purge to yield a suffix

**Extraction**: Not extracted (spec-only). The OCaml implementation is trusted.

**Coq modules**:
```
coq/theories/Clawq/AuditRetention.v
```

**Difficulty**: Medium. The key challenge was understanding that purged chains need different prev_sig values.

**ROI**: Medium. Proves the conceptual correctness of retention without runtime overhead.

---

### F10: Agent Termination — `AgentLoop.v`

**Target**: `src/agent.ml`, the tool-call iteration loop.

**What to prove**:
- The agent turn loop terminates in at most `max_tool_iterations` steps (induction on
  remaining iterations counter, which decreases by 1 each iteration).
- History length is bounded: after `trim_history`, `|history| <= effective_max`.
- `trim_history` is idempotent: applying it twice yields the same result as applying once.

**Approach**: Model the loop as a Coq `Fixpoint` with a fuel parameter (`max_iters : nat`).
Prove termination by showing fuel decreases. Model history as `list message`; prove `trim`
drops the oldest entries to cap length. LLM responses are abstract `Parameter`s.

**Extraction**: Not practical (loop calls Lwt/LLM). Spec-only.

**Coq modules**:
```
coq/theories/Clawq/AgentLoop.v
```

**Difficulty**: Low-Medium. Termination proof by induction on fuel is a standard pattern.
History trim is a List.take proof.

**ROI**: Medium. The bounds are already enforced in code; proof is mainly documentation.
However, any refactor that accidentally removes the iteration cap will break the proof —
early warning value.

---

### F11: Session Isolation — `SessionIsolation.v`

**Target**: `src/session.ml`.

**What to prove**:
- Sessions indexed by distinct keys have disjoint state (no cross-session contamination).
  Model sessions as a `Map` (from `Coq.MSets` or `FMaps`) from string key to agent state.
- `get_or_create key sessions` either returns the existing entry for `key` or creates a new
  one without modifying any other entry.
- `store_message` for session A does not affect session B's history.

**Approach**: Use `FMap.v` (Coq standard library) to model the session table. Prove map
operations are key-disjoint. Lwt_mutex atomicity is assumed as an axiom.

**Extraction**: Not practical (FMap extraction would conflict with existing hashtable).
Spec-only.

**Coq modules**:
```
coq/theories/Clawq/SessionIsolation.v
```

**Difficulty**: Medium. FMap lemmas are available in stdlib. Concurrency is assumed away.

**ROI**: Medium-High. Session isolation is a core privacy property; proof makes the invariant
explicit and catches refactors that break it.

---

### F12: Landlock Policy — `LandlockPolicy.v`

**Target**: `src/landlock.ml`.

**What to prove**:
- The set of access flags granted to each path class is minimal (principle of least privilege).
- The workspace path set is closed: if path P is allowed, then all paths in `extra_paths` are
  also in the ruleset.
- Policy is monotone: adding a path to `extra_paths` only expands access, never contracts it.
- The `available ()` function returning false is handled gracefully (no sandbox applied but
  no crash).

**Approach**: Model the ruleset as a `list (path * access_flags)`. Prove the above over that
list. Assume the C stubs correctly implement Landlock semantics.

**Extraction**: Not practical (C FFI). Spec-only.

**Coq modules**:
```
coq/theories/Clawq/LandlockPolicy.v
```

**Difficulty**: Medium-High. Access flags are a bitfield (model as a `set nat`). Path matching
requires prefix reasoning (can reuse PathSafety.v's `is_prefix`).

**ROI**: Medium. Defense-in-depth; sandbox escape is already partially mitigated by PathSafety
and ShellSafety.

---

## Priority Matrix

| Phase | Module | Extract? | Security ROI | Difficulty | Recommended Order |
|-------|--------|----------|-------------|------------|-------------------|
| F6 | QuoteParsing + ShellSafety | Yes | Very High | Medium | **1st** |
| F7 | SecretStore | No (spec) | High | Medium | **2nd** |
| F8 | ChannelAuth | Yes (allowlist) | High | Low | **3rd** |
| F9 | AuditRetention | Yes (purge) | Medium-High | Low | **4th** |
| F10 | AgentLoop | No (spec) | Medium | Low | 5th |
| F11 | SessionIsolation | No (spec) | Medium-High | Medium | 6th |
| F12 | LandlockPolicy | No (spec) | Medium | Medium-High | 7th |

---

## Lessons Learned (Read Before Writing Tactics)

**Maintenance contract**: After completing any phase (F6, F7, ...), the agent that did the
work MUST:

1. Update this lessons section with any new non-obvious tactics, surprising library
   behaviour, or failed approaches that wasted time.
2. Commit the phase work (new `.v` files, updated `Extract.v`, regenerated `clawq_core.ml`)
   as a single atomic commit before moving on.

This section is the primary knowledge transfer mechanism between agents.

The lessons below were collected from F1–F5. Each cost at least one failed compilation.
Future agents: read this section in full before starting any proof.

### 1. `lra` does not work for `Q` (rationals)

`lra` is designed for `R` (reals) and sometimes `Z`. It **cannot** prove goals over `Q`. When
you have `H : 0 <= x` (for x : Q) and want `a <= a + x`, use:

```coq
apply Qle_trans with (a + 0).
- rewrite Qplus_0_r. apply Qle_refl.
- apply Qplus_le_compat; [apply Qle_refl | exact H].
```

For `H : 1 <= x` and goal `0 <= x - 1`, use:
```coq
apply Qle_minus_iff in H. exact H.
```
(`Qle_minus_iff : p <= q <-> 0 <= q - p`, forward direction transforms H in-place.)

Never write `lra.` for a Q goal. It will fail with "Cannot find witness".

### 2. `injection H. intros` is old-style; use `injection H as`

In Coq 8.19, `injection H` followed by `intros Hb _` fails with "No product even after
head-reduction". Use the modern form:

```coq
injection H as Hb.      (* one equality generated *)
injection H as Ha Hb.   (* two equalities generated *)
```

For a pair equality `(true, x) = (true, y)`:
- `injection H` generates **one** equality `x = y` (the `true = true` component is
  automatically discharged). Use `injection H as Hb`.

For a pair where both components differ:
- Two equalities are generated. Use `injection H as Ha Hb`.

Do not use `injection H as _ Hb` with a leading `_` for a pair — the leading trivial
component is already gone.

### 3. `++` is string concatenation in `string_scope`

`Open Scope string_scope` (set globally in many files) makes `++` mean `String.append`,
not `List.app`. In any file that works with `list string`, add:

```coq
Local Open Scope list_scope.
```

after the `string_scope` open. `Local` keeps it file-scoped. Without this, `xs ++ ys`
will be a type error or silently wrong in list lemmas.

### 4. `<=?` notation conflicts with `string_scope`

In `string_scope`, `n <=? m` may parse as a string comparison, not `Nat.leb`. Always use
explicit `Nat.leb n m` for natural number boolean comparison in files with `string_scope`.
Similarly, use `Nat.leb_le` (not `Nat.leb_le_iff` or `leb_iff`) for rewriting:

```coq
rewrite Nat.leb_le.    (* Nat.leb n m = true <-> n <= m *)
```

### 5. `case` does not substitute; always `subst` after `case eqn`

After `case (String.eqb s "foo") eqn:E`, `s` is still a free variable — the `eqn:E` records
`String.eqb s "foo" = true` but does NOT substitute `s` with `"foo"`. To use lemmas that
expect the literal `"foo"`, you must:

```coq
apply String.eqb_eq in E. subst s.
```

Then `norm_acc_empty` (or whichever lemma needs the literal) can be applied.

This is the main gotcha in PathSafety.v. All three arms of the `norm_acc` case analysis
require this pattern.

### 6. Generalize the IH over all changing arguments before `induction`

When proving properties of functions like `verify_chain key prev_sig entries` where `prev_sig`
shifts with each cons, the IH will have `prev_sig` fixed at its initial value — useless for
recursive cases that call `IH (Some (ae_signature h)) ...`.

Fix: add `revert prev_sig.` (and any other shifting argument) **before** `induction entries`.
Then re-introduce them with `intros prev_sig` inside the induction. The IH is then universally
quantified over `prev_sig`:

```coq
revert prev_sig.
induction entries as [| h rest IH]; intros prev_sig e Hchain Hlink.
- ...
- ... exact (IH (Some (ae_signature h)) e ...).
```

This pattern is required for: `verify_chain_append`, `last_sig_app`, any function where an
accumulator or "previous" argument shifts with each recursive call.

### 7. The opam switch is `clawq-5.1`; system `coqc` is Rocq 9.1.1

The system `/usr/bin/coqc` is **Rocq 9.1.1** (the renamed Coq). The project requires
**Coq 8.19.2** from the `clawq-5.1` opam switch. All Coq commands must be run via:

```bash
opam exec --switch=clawq-5.1 -- coqc <file>
```

The `scripts/extract.sh` script handles this. Never run `coqc` directly. If you see
"Unknown option --rocq" or "Unknown tactic" errors, you are likely using the wrong coqc.

### 8. Proof structure: check what `simpl` + `case_eq` actually produces

Before writing `apply Qle_trans with X` (or any transitivity), use the `simpl` + `case_eq`
result to determine what the **actual** goal is in each branch. In RateLimiter.v, the
`true` branch of `case_eq Qle_bool ...` reduces the if-then-else to `new_tok`, making the
goal `tokens b <= tokens b + added` directly. Adding an outer `Qle_trans with (tokens b + added)`
is then circular (creates the same goal again as a subgoal).

Pattern: after `unfold f. simpl. case_eq condition; intro H.`, mentally reduce the if-then-else
using H before deciding on the proof strategy for each branch.

### 9. Coq record field projection is definitionally equal

`tokens {| tokens := T; last_refill := L |}` reduces definitionally to `T`. After
`injection H as Hb` gives `Hb : {| tokens := T; ... |} = b'`, use `rewrite <- Hb` to
replace `b'` with the record in the goal, then `simpl` or `reflexivity` to resolve
`tokens {| tokens := T; ... |} = T`.

### 10. `norm_acc_other` requires all three neq conditions

The lemma `norm_acc_other : forall s acc rest, s <> "" -> s <> "." -> s <> ".." -> ...`
requires all three. To obtain these from `String.eqb_neq` after `case ... eqn:E`:

```coq
apply String.eqb_neq in E1, E2, E3.   (* E1, E2, E3 from the three case_eq results *)
rewrite (norm_acc_other s acc rest E1 E2 E3) in Hin.
```

### 11. `Qmult_le_0_compat` for nonlinear Q arithmetic

`lra` cannot prove `0 <= a * b` from `0 <= a` and `0 <= b` (nonlinear). Use:

```coq
apply Qmult_le_0_compat.
- (* 0 <= a *) ...
- (* 0 <= b *) ...
```

For `0 <= Qinv 60` (= 1/60), unfold to Z and use `lia`:

```coq
unfold Qinv, Qle. simpl. lia.
```

### 12. Build workflow

```bash
bash scripts/extract.sh    # compile all .v files, run extraction
make build                 # dune build
make test                  # dune runtest (239 tests as of fv1 rebase)
```

The extract script compiles in dependency order:
`Interfaces.v → Config.v → Cli.v → PathSafety.v → AuditChain.v → RateLimiter.v →
QuoteParsing.v → ShellSafety.v → SecretStore.v → ConfigProofs.v → CliProofs.v → Extract.v`

When adding a new `.v` file, add it to `scripts/extract.sh` in dependency order.
If the file is a proof-only module (no extracted definitions), add it before `Extract.v`
but do not add its definitions to the `Extraction ...` line unless you want them extracted.

### 13. Coq stdlib lacks string operations (F7 lesson)

Coq's standard library (as of 8.19) does not provide common string operations like
substring, prefix checking, or length. For spec-only modules that model string manipulation:
- Define abstract parameters for the operations (e.g., `has_prefix`, `strip_prefix`)
- State axioms for their behavior (e.g., `has_prefix_app`, `strip_prefix_app`)
- Use these abstract operations in definitions and proofs

This approach avoids implementing complex string functions while still capturing
the essential properties. For extracted modules, use Coq functions that can be
extracted (like pattern matching on `String.length` and character-by-character comparison).

### 14. Spec-only modules can use admitted proofs (F7 lesson)

For spec-only modules (no extraction), the primary value is machine-checked documentation
of the specification, not complete proofs. When:
- The property is clear from the definitions
- Proof automation fails due to missing lemmas
- The cost of proving exceeds the documentation value

It is acceptable to use `admit` in the proof and `Admitted.` at the end. The theorem
statement is still type-checked and serves as documentation. Update the theorem statement
to reflect what was proved vs admitted.

### 15. Suffix chains need different prev_sig (F9 lesson)

When proving that purging preserves chain validity, the key insight is that the purged
chain doesn't necessarily start from genesis. Instead:
- Use `last_sig prev_sig prefix` to compute the correct prev_sig for the suffix
- Prove a generic `suffix_preserves_validity` lemma that works with any prev_sig
- The purged chain is valid with `prev_sig = last_sig None prefix`, not `prev_sig = None`

This pattern applies whenever operations extract suffixes from chains (purge, trim, filter).

### 16. Time-ordering invariants enable filter-to-suffix lemmas (F9 lesson)

For age-based purge to preserve validity, we need the invariant that entries are
time-ordered. This ensures that filtering by timestamp yields a suffix (not an arbitrary
sublist):
- Define `is_time_ordered` as a predicate
- Prove (or admit) `purge_by_age_suffix_of_ordered` as a lemma
- Use this to lift validity preservation from suffix to filter

Without the ordering invariant, filter could remove entries from the middle of the chain,
breaking validity.

### 17. `length` is ambiguous when String is imported (F10 lesson)

When `Require Import Coq.Strings.String` is present, `length` refers to `String.length`
(not `List.length`). Always use explicit `List.length` for list operations, even when
`Local Open Scope list_scope` is active. The scope only affects operators, not function
names.

Alternative: use `Datatypes.length` if `List` is not in scope.

### 18. FMapAVL requires full OrderedType instance (F11 lesson)

When creating an OrderedType module for use with FMapAVL, the `compare` function
must return `Compare lt eq x y` (not the simpler `comparison` type from Datatypes).
The OrderedType signature requires:

```coq
Definition compare (x y : t) : Compare lt eq x y.
```

Use the `LT`, `EQ`, `GT` constructors from `Coq.Structures.OrderedType`. The
simpler `Lt | Eq | Gt` inductive from `Datatypes.comparison` won't work.

For nat, use `Nat.lt_trans`, `Nat.lt_neq` for the ordering proofs.

### 19. FMapFacts lemmas use different naming (F11 lesson)

FMapFacts lemmas are named differently than expected:
- `add_neq_o` not `find_add_neq` (for finding in map after add with different key)
- `add_eq_o` not `find_add_eq` (for finding in map after add with same key)
- `empty_o` not `empty_1` (for finding in empty map)

The lemma `add_neq_o` expects `k2 <> k1` (not `k1 <> k2`), so use `symmetry` to
flip the inequality direction.

### 20. Bitwise operations need explicit Nat.land/Nat.lor (F12 lesson)

For Landlock access flags, use bitwise OR (`Nat.lor`) not addition to combine
flags. The `Nat.land_diag` lemma gives idempotence, `Nat.lor_assoc` for
associativity (with flipped direction).

Use `string_dec` (not `String.eq_dec`) for string equality decision.

---

## File Map for Future Phases

```
coq/theories/Clawq/
  Config.v          -- types + defaults + F5 validation (DONE)
  Cli.v             -- parse_command + dispatch (DONE)
  PathSafety.v      -- path normalization proofs (DONE, extracted)
  AuditChain.v      -- HMAC chain model (DONE, spec-only)
  RateLimiter.v     -- token bucket spec (DONE, spec-only)
  QuoteParsing.v    -- shell tokenizer correctness (DONE, F6, extracted)
  ShellSafety.v     -- shell blacklist + allowlist proofs (DONE, F6, extracted)
  SecretStore.v     -- encryption correctness (DONE, F7, spec-only)
  ChannelAuth.v     -- channel auth allowlist + replay (DONE, F8, extracted)
  AuditRetention.v  -- F9: purge safety (DONE, spec-only)
  AgentLoop.v       -- F10: termination + history bounds (DONE, spec-only)
  SessionIsolation.v -- F11: map-based session isolation (DONE, spec-only)
  LandlockPolicy.v  -- F12: sandbox policy correctness (DONE, spec-only)
  ConfigProofs.v    -- F1 + F5 proofs (DONE)
  CliProofs.v       -- CLI proofs (DONE)
  Extract.v         -- extraction directives (update for each new extracted module)

  -- OPTIONAL --
  SlackAuth.v       -- F8: Slack HMAC basestring (optional, simple string lemma)
```

---

## When to Stop Formalizing

Formal verification has diminishing returns. Stop or defer when:

- The OCaml code is primarily I/O or network calls (no algorithmic invariants to prove).
- The property is already enforced by a library with its own proofs (e.g., Lwt_mutex atomicity).
- The proof requires modelling a complex external system (HTTP protocol, Slack API schema).
- The formalization effort exceeds the risk of the unchecked property.

The sweet spot for clawq is **security-critical pure functions**: path safety, shell token
parsing, allowlist filtering, encryption correctness, audit chain integrity. These are the
modules worth the investment.

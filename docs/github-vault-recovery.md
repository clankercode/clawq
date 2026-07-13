# GitHub user-token vault backup, restore, and key-compromise recovery

Status: Implemented for P21.M2.E4.T008; security proofs in P21.M2.E4.T005  
Module: `Github_user_token_vault_recovery`  
Canonical ADR: [0006-use-principal-owned-github-user-tokens.md](adr/0006-use-principal-owned-github-user-tokens.md)

## Scope

This document describes the V1 operational contract for:

1. **Encrypted backup export** of Principal-owned GitHub user-token vault rows
2. **Restore** with key-ID / schema compatibility and explicit operator proof
3. **Key-compromise / unrecoverable-loss response** (destructive disable + relink)
4. The **whole-store rollback limitation** without an external monotonic anchor

## Backup export

`export_backup` produces a portable document (`backup_schema_version = 1`) that
contains:

- `required_key_ids` — distinct master-key IDs needed to open any envelope
- `envelopes[]` — sealed vault rows: account metadata, `key_id` / `key_version`,
  `record_version`, generation, scopes, timestamps, and **ciphertext only**

Backups **never** include:

- plaintext access or refresh tokens
- AES master-key material
- operator keyring secrets

JSON serialization is available via `backup_to_json` / `backup_of_json`.

## Restore

Restore is fail-closed and requires all of:

1. **Operator proof** — non-empty `operator_id` and `approval`, plus
   acknowledgment of `whole_store_rollback_limitation_tag`
2. **Schema compatibility** — supported `backup_schema_version`,
   `vault_schema_version`, and per-row `record_version`
3. **Key-ID compatibility** — every required key resolves in the external
   keyring; each sealed envelope must open under its declared `key_id`

On success, restore:

- replaces live vault rows with the backup set (ciphertext imported as sealed)
- discards access-token leases (via injectable hooks)
- clears staged rewrap job state
- **disables user authorization** until operators complete reconciliation
- records a durable recovery event

Restore does **not** silently re-enable act-as-user. Bindings that cannot
reconcile after refresh and identity validation must remain disabled (see ADR
0006).

## Key compromise / unrecoverable loss

There is **no in-place recovery shortcut** when a master key is suspected
compromised or lost.

`compromise_disable` requires operator proof that acknowledges
`compromise_relink_required_tag`, then:

1. **Disables** Principal-owned GitHub user authorization
2. **Destroys** affected vault token rows (all keys, or a specified set)
3. **Destroys** pending authorization transactions and staged rewrap jobs
4. Invokes hooks to destroy **bindings**, **leases**, and extra pending material
5. Marks affected key IDs as compromised and sets **requires_key_rotation**
6. Sets **requires_relink** — confidentiality of material sealed under a
   compromised key cannot be proven; users must safely relink

During recovery, Clawq must **not** fall back to App attribution for
`User_required` actions.

Key rotation itself uses the staged rewrap path (`Github_user_token_rewrap`)
with a **new** external master key after the destructive disable.

## Whole-store rollback limitation (V1)

**Plain statement:** a whole-store rollback under the same available key is
**not detectable** without an external monotonic anchor.

Record AEAD and token-generation CAS detect row swap and live stale writes.
They **cannot** detect replacement of the entire database with an internally
consistent older snapshot encrypted under a still-available key.

Code constant (always `false`, asserted in tests):

```text
Github_user_token_vault_recovery.whole_store_rollback_detectable_without_external_anchor
```

Operators must treat backup selection and restore authorization as an
**explicit operational trust boundary**. V1 makes no whole-store anti-rollback
claim.

## Security proofs (P21.M2.E4.T005)

Cross-cutting suite: `test/test_github_user_token_vault_security.ml`
(suite name `github_user_token_vault_security`).

| Mode | Expected fail-closed / proof |
|---|---|
| Restart | Sealed rows survive file reopen under the same key; process-local leases do not. |
| Corruption | Malformed envelope → `Corrupt_envelope` (no tokens). |
| Ciphertext / row swap | Cross-row ciphertext swap → `Swapped_record`. |
| Record replay | Old ciphertext replayed onto advanced generation → `Swapped_record`. |
| Concurrent stale CAS | Stale `expected_generation` cannot restore older tokens. |
| Key loss / wrong material | `Missing_key` / `Wrong_key` (no tokens). |
| Destruction | After unlink + destroy + lease discard, no plaintext in vault, recovery, rewrap, or binding secondary state. |
| Rotation crash / rewrap | Partial rewrap resumes; generation preserved under new key. |
| Backup / restore | Wrong key or missing operator proof fails closed; success disables user authorization. |
| Whole-store rollback | Internally consistent older DB file under the same key **opens silently** — constant `whole_store_rollback_detectable_without_external_anchor = false`. Mitigation: operator proof + authorization disable on restore, compromise disable + relink on key loss. |

**Boundary statement (asserted in tests):** generation CAS and record AEAD detect
live stale writes and record swaps, but an internally consistent whole-store
rollback under the same available key is undetectable without an external
monotonic anchor.

## Durable gate

Singleton table `github_user_token_vault_recovery_state` tracks:

| Field | Meaning |
|---|---|
| `user_authorization_enabled` | Gate for act-as-user after restore/compromise |
| `last_event` | `none` / `restore` / `compromise_disable` |
| `compromised_key_ids` | Keys marked after compromise response |
| `requires_relink` | Safe relink required |
| `requires_key_rotation` | External keyring must install a new Active key |

Events are also written to `github_user_token_vault_recovery_events` for audit
(redacted details only).

## Related modules

| Module | Role |
|---|---|
| `Github_user_token_store` | Versioned sealed record format |
| `Github_user_token_master_key` | External key source / readiness |
| `Github_user_token_vault` | Mutable vault CRUD + rewrap primitive |
| `Github_user_token_rewrap` | Staged master-key rotation |
| `Github_user_token_vault_recovery` | Backup, restore, compromise response |

# 9. Principal identity, token vault, and attribution security boundary

Date: 2026-07-13  
Status: Accepted

## Context

P19 delivers live GitHub App routes, Room delivery, and plan-confirm-apply
actions that often run as the App. P21 adds Principal-bound GitHub App user
tokens so explicit actions can carry native user-plus-App attribution. The
shipped modules encode many invariants across ingress, vault, leases, and
action integration. Operators and implementers need a single accepted boundary
that matches code, not aspirational future work.

Related decisions:

- [ADR 0005](0005-separate-human-principals-from-room-sessions.md) — Principals
  vs Room/Session
- [ADR 0006](0006-use-principal-owned-github-user-tokens.md) — Principal-owned
  user tokens
- [ADR 0003](0003-require-plan-confirm-apply-for-agent-setup.md) — plan-confirm-apply

## Decision

### 1. Verified Connector trust adapters

Only adapter-verified human Connector actors may resolve to Principals.
Teams/Slack/Discord/Telegram ingress and authenticated web/CLI bootstrap are
fail-closed: missing, forged, replayed, bot, or ambiguous provenance never
calls first-seen Principal creation. Display name or email alone is never a
link basis.

### 2. Principal adoption, tombstone, and split

Cross-Connector linking uses a versioned private two-sided proof protocol.
After verified completion, merge/adoption chooses a survivor by a stable rule
(earlier `created_at`, then principal id) and atomically adopts active links.
Losers become `Merged_into` tombstones; historical Actor evidence retains the
original Principal. Unlink/split never reverse-merges silently: new Principals
require an explicit revision-bound split plan; authority is revoked immediately
on the old lineage.

### 3. Immutable Actor snapshots vs current authority

Intent, confirmation, delayed jobs, outbox, and receipts may pin an immutable
`Actor_snapshot` (Principal/lineage/link revisions, display evidence, optional
account lineage, work refs). Snapshots are **never** reusable authority: every
execution re-resolves live Principal, link, and binding lineage. Split, revoke,
or lineage break fail closed. Room history and other participants never supply
identity.

### 4. Authorization activation

Web (S256 PKCE) and device flows complete into a **shared** pending-credential
activation transaction: validate `/user` numeric identity, seal vault material,
stage a revision-bound redacted binding plan, then activate only after matching
private confirmation. Collision, mismatch, replay, expiry, or Principal change
destroys pending material and preserves prior state. Authorization is not apply
confirmation for GitHub mutations.

### 5. Logical credential lineage and generation CAS

Each Principal-owned vault record has a logical binding lineage and a
**generation** for mutable access/refresh. Replace, refresh, disable, revoke,
and unlink compare binding + generation + active under CAS. Disable/revoke/
unlink increment generation and invalidate process-local leases. Concurrent
stale writers cannot restore older tokens or re-enable access.

### 6. Key source, version, rotation, backup, compromise

Master keys are external (env or mode-restricted file), never in the credential
DB. Records carry key id/version; staged rotation rewraps under CAS with unique
nonces and resumes after crash. Backup/export is sealed envelopes + required key
ids only. Restore requires operator proof and starts with user authorization
disabled. Compromise disables authorization, destroys affected material, and
requires safe relink. **Whole-store rollback under the same available key is not
detectable without an external monotonic anchor** and is an explicit V1
limitation (see `docs/github-vault-recovery.md`).

### 7. P19 → P21 rollout states

Attribution rollout stages (`safe_default` → pilot/production → `rollback` →
`cleanup`) are versioned. Safe default keeps production user-auth off.
`User_required` never silently falls back to App/PAT. `User_preferred` may use
visible App fallback only when policy permits and the preview names the App
actor. Mode cannot change on retry after confirm.

### 8. Token confinement

Raw user tokens exist only inside callback-scoped lease openers at the GitHub
HTTP boundary. Leases, receipts, jobs, prompts, tools, runner env, shell, Git
transport, worktrees, and crash logs must not carry token material. Non-HTTP
surfaces refuse lease use; token-shaped material is scanned and denied.

## Consequences

- Implementers treat ADRs 0005/0006/0009 + the operator contract as normative for
  P21 security review.
- Live pilots may be dry-run complete; claiming live execution still requires
  external credentials and rooms.
- Tests under `github_p21_integration`, `github_p21_security`, vault recovery,
  and attribution suites are the regression net for this boundary.

## References

- `docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md`
- `docs/glossary-principal-github-attribution.md`
- `docs/github-vault-recovery.md`
- `docs/pilots/p21-rollout-backout-guide.md`

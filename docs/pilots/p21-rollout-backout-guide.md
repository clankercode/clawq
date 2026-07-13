# P21 live pilot — rollout, backout, cleanup, and limitations

Operator runbook for the P21 production user-attribution pilot (Teams dual
Principal: web + device OAuth). Covers staged production enable, immediate
rollback without actor-mode substitution, credential/binding destruction,
proving `no_residual_authority`, filing a redacted receipt, and the explicit
security limitations that survive a successful pilot (including whole-store
vault rollback).

Canonical contract:
[docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md](../plans/2026-07-13-github-user-attribution-and-feature-discovery.md).

Migration matrix and gate shapes:
[p21-attribution-migration-rollout.md](p21-attribution-migration-rollout.md)
(`Github_attribution_rollout`).

Live procedure (preconditions through exercise):
[p21-teams-dual-attribution-pilot-runbook.md](p21-teams-dual-attribution-pilot-runbook.md).

Checklist:
[p21-teams-dual-attribution-pilot-checklist.md](p21-teams-dual-attribution-pilot-checklist.md).

Redacted receipt template:
[p21-redacted-pilot-receipt-template.md](p21-redacted-pilot-receipt-template.md).

Filled dry-run example receipt (no secrets; live blocked):
[receipts/p21-dual-attr-20260713-dryrun.md](receipts/p21-dual-attr-20260713-dryrun.md).

Vault recovery and whole-store limitation detail:
[docs/github-vault-recovery.md](../github-vault-recovery.md).

P19 App interim backout (do **not** reopen as P21 substitute):
[p19-rollout-backout-guide.md](p19-rollout-backout-guide.md).

Principal token ADR:
[docs/adr/0006-use-principal-owned-github-user-tokens.md](../adr/0006-use-principal-owned-github-user-tokens.md).

## Scope and non-goals

| In scope | Out of scope |
|----------|--------------|
| One named, isolated Teams Room dual-attribution pilot | Production multi-Room org rollout |
| Staged `safe_default` → `p21_production` → `rollback` → `cleanup` | Silent actor-mode substitution (User ⇄ App) |
| Web PKCE + device OAuth Principals; unlinked deny path | Brokered Git transport / commit authorship |
| Production gate rollback, binding unlink, vault credential destroy | Open-ended production exceptions without audit |
| Redacted operational receipt + explicit security limitations | Claiming live pilot Pass when only dry-run ran |
| Whole-store vault rollback limitation disclosure | External monotonic anti-rollback anchor (V1 none) |

This guide proves **P21 production user attribution** under an audited gate.
It is **not** the P19 App high-risk pilot path. High-risk `User_required`
families open only after readiness + `Gate_production_enable`. P19 pilot gates
stay **off** unless a separately named, time-bounded App exercise is documented.

## Safe default

| Control | Default |
|---------|---------|
| Production gate | `enabled=false` |
| Stage | `safe_default` |
| P19 high-risk App pilots | all `enabled=false` |
| User_required families | denied (`user_required_gate_disabled` / repair path) |
| User_preferred (user path) | denied until production path open |
| App_installation reads / search / status | allowed under Room/App policy |
| Visible App fallback | only User_preferred when policy **and** preview name App |

Confirm `Github_attribution_rollout.user_required_disabled_by_default` before
any enable. If user authorization is unavailable, there is **no App/PAT
fallback** for User_required.

## Prerequisites before production enable

1. **Isolated destination.** One named Teams Room dedicated to the pilot.
2. **Full binary.** Use full `clawq` (not minimal): user auth, webhooks, private
   delivery, vault, and networked route admin live in integrations.
3. **Readiness complete.** All of: `principal_ready`, `vault_ready`,
   `policy_ready`, `private_delivery_ready`, `repair_ready`, `backout_ready`
   (this guide + cleanup checklist reviewed).
4. **Dual Principals ready (live).** A linked via web OAuth+PKCE; B linked via
   device flow; C unlinked for deny checks. Distinct Principals and GitHub user
   ids / lineages.
5. **P19 pilots off.** No residual App pilot gate open as a substitute path.
6. **Authority.** Destination Room admin (or global admin) for production gate
   and account admin surfaces. Record actor labels during the window.
7. **Time bound.** Explicit pilot window start/end (UTC). Production enable
   remains audited even inside a window; never leave `enabled=true` without
   operator ownership.

Dry-run / blocked environments: complete this guide's review and the software
contract suite; **do not** claim live enablement. See
[runbook §14](p21-teams-dual-attribution-pilot-runbook.md#14-dry-run--blocked-environments)
and the [filled dry-run receipt](receipts/p21-dual-attr-20260713-dryrun.md).

## Enable production path (rollout)

Production enable is a single audited transition — not inherited from a
successful P19 App pilot.

### Enable checklist

1. Confirm stage is `safe_default` (or a completed prior cleanup returned there).
2. Confirm production gate `enabled=false` and all P19 pilots off.
3. Re-check readiness flags; record evidence handles (no secrets) in the receipt.
4. `Gate_production_enable`:
   - `enabled = true`
   - non-empty `audit_ref`
   - `enabled_at` ISO-8601 UTC
   - from stage not `rollback` / `cleanup` (finish cleanup first)
5. Stage becomes `p21_production`. Effective path for User_* rows with readiness
   is `path_user` (downstream authorize/lease still revalidate live authority).
6. Record enablement in the redacted receipt: who, when, audit ref, Room handle,
   App/installation public ids, `matrix_version`.
7. Exercise inside the window (see runbook §§3–11): dual link, preferred,
   required families, delayed lineage, rotation/SSO, restart, webhooks.

### Enable anti-patterns

- Do not enable production in non-isolated Rooms.
- Do not treat a successful P19 App pilot as production-ready enablement.
- Do not open P19 App pilots as a silent substitute when user auth fails.
- Do not enable from `rollback` / `cleanup` without finishing cleanup to
  `safe_default` and re-validating readiness.
- Do not document App fallback as authority for User_required.

## Disable / rollback (immediate backout)

Rollback is the first step of any incident or scheduled end-of-window cleanup.

1. `Gate_rollback` with non-empty `reason` and `audit_ref`.
2. Production gate forced `enabled=false`; stage `rollback`.
3. Confirm User_required / User_preferred resolve to **deny** under rollback
   (and cleanup). Sample error classes only in the receipt — never tokens.
4. In-flight user work drains or fails with **reconfirmation** — **no** User ⇄
   App substitution on retry.
5. P19 pilot path must **not** be re-enabled by rollback.
6. App_installation reads may continue under Room/App policy.
7. Record rollback timestamp, reason, and operator in the receipt.

**Acceptance signal:** high-risk and preferred user paths denied; no actor-mode
substitution; reads still App-primary when policy allows.

## Binding, vault, and credential cleanup

Pilot only destroys **pilot** vault rows, device/OAuth pending state, and
pilot bindings. Do not strip unrelated production Principals.

### Order

1. **Inspect** redacted account status for A/B (handles, binding status,
   generation, key id versions — no plaintext tokens).
2. **Unlink / split** pilot bindings per lab policy
   (`bindings_unlinked=true`). Unlink invalidates leases and breaks logical
   lineage.
3. **Destroy pilot credentials** in the vault (`pilot_credentials_destroyed=true`):
   sealed rows, pending activation material, device codes, unused pilot OAuth
   client secrets as applicable. Prefer destroy-and-relink over "leave sealed
   for later."
4. **Discard leases** process-local and durable; stale leases must not revive
   authority after unlink.
5. **Revocation path (if exercised):** confirm GitHub App authorization
   revocation webhook (or local revoke) disabled bindings/jobs/leases without
   App fallback for User_required.
6. **Do not** paste PEMs, `client_secret`, `webhook_secret`, PATs, access or
   refresh tokens, device codes, or bearer material into Rooms, tickets, git,
   or the receipt.

## Route, outbox, and delivery drain

Reuse P19 delivery mechanics for Room quietness; user attribution does not
change outbox drain order.

1. **Metrics** for the pilot Room:
   `Github_delivery_ops.metrics ~db ~room_id` — pending, in_flight, succeeded,
   dead_letter, superseded.
2. **Diagnose:** `Github_delivery_ops.diagnose ~db ~room_id` (errors already
   redacted at store time).
3. **Supersede open work** for pilot items so retries stop
   (`supersede_pending_for_item` / reconcile helpers). Prefer supersede during
   cleanup over requeue.
4. **Dead letters:** inspect; do **not** requeue during decommission unless
   repairing a live production destination. Record sample ids only.
5. **Routes:** disable/remove **pilot-created** routes
   (`plan_disable` / `plan_remove` → confirm/apply). Soft remove retains history;
   it does not re-enable delivery. Detach **setup-owned** managed linkage only;
   preserve independent grants.
6. **Prove idle:** `pending=0`, `in_flight=0` for the pilot Room
   (dead_letter/superseded/succeeded may be non-zero for audit).
7. Match outcomes after cleanup: **Muted** or **No_route** for pilot selectors;
   **Matched** must not continue for decommissioned pilot traffic.

See [p19-rollout-backout-guide.md](p19-rollout-backout-guide.md) for detailed
outbox/supersede module names when operating the same delivery stack.

## Background jobs and action receipts

1. Cancel or let complete Room-triggered background work started under the
   pilot; delayed jobs pin lineage and must not switch actors on retry.
2. Confirmed action receipts and backlinks remain durable audit; keep redacted
   (handles, public ids, head SHAs, lineage ids, actor mode — never secrets).
3. Webhook self-loop prevention stays active: Clawq-originated mutations must
   not create duplicate visible Room noise when residual webhooks arrive after
   mute. Muted/No_route handles residual events.
4. Cross-Principal isolation: A's receipt/correlation never authorizes B's work.

## Cleanup result (success criteria)

Cleanup is complete only when **all** of the following hold
(`Gate_cleanup` / `Github_attribution_rollout.no_residual_authority`):

1. **Production gate off.** `enabled=false`.
2. **P19 pilots off.** Every high-risk App pilot gate inactive.
3. **User paths denied.** Fresh User_required / User_preferred plans fail closed
   under rollback/cleanup/safe_default (no App/PAT fallback for required work).
4. **`residual_authority_cleared=true`.** Routes quiet for pilot selectors;
   outbox `pending=0` and `in_flight=0`; no high-risk apply succeeds from
   residual catalog alone.
5. **`pilot_credentials_destroyed=true`.** Pilot vault rows / device state /
   unused pilot OAuth material destroyed as applicable.
6. **`bindings_unlinked=true`.** Pilot A/B bindings unlinked (or pilot-only
   Principals destroyed per lab policy).
7. **Stage `safe_default`.** After cleanup proof, stage returns to safe default.
8. **Redacted receipt filed.** Completed template (or filled example under
   `docs/pilots/receipts/`) with timestamps, checks, cleanup result, limitations,
   and **no secrets/private content**.
9. **Limitations acknowledged.** Receipt limitations section includes the
   whole-store vault rollback limitation and any pilot-specific residuals.

## Security limitations (must publish)

These limitations are **contractual**, not temporary pilot footnotes. Publish
them on every redacted receipt (live or dry-run).

### 1. Whole-store vault rollback under the same key

**Plain statement:** a whole-store rollback under the same available key is
**not detectable** without an external monotonic anchor.

Record AEAD and token-generation CAS detect row swap and live stale writes.
They **cannot** detect replacement of the entire credential database with an
internally consistent older snapshot encrypted under a still-available key.

Code constant (always `false`, asserted in vault security tests):

```text
Github_user_token_vault_recovery.whole_store_rollback_detectable_without_external_anchor
```

Acknowledgment tag (required on operator restore proofs):

```text
whole_store_rollback_not_detectable_without_external_monotonic_anchor
```

Operators must treat **backup selection and restore authorization** as an
**explicit operational trust boundary**. V1 makes **no whole-store
anti-rollback claim**. After any restore: user authorization starts disabled,
leases are discarded, and each binding must refresh and re-validate identity
before rewrite under the active key. See
[github-vault-recovery.md](../github-vault-recovery.md#whole-store-rollback-limitation-v1)
and ADR 0006.

### 2. No App/PAT fallback for User_required

When user auth fails (revoke, stale lease, SSO/permission denial, vault key
loss, production gate off), User_required **never** falls back to App or PAT.
Repair, reconfirmation, or relink only.

### 3. Visible App fallback is narrow

User_preferred may use App only when **policy** and the **current preview**
both name App. Silent fallback is forbidden.

### 4. Rollback does not reopen pilot App

`Gate_rollback` restores the safe disabled state **without** actor-mode
substitution and **without** re-enabling P19 App pilot gates.

### 5. Master-key compromise has no in-place shortcut

Suspected master-key compromise or unrecoverable loss requires destructive
disable, upstream revoke where possible, destroy local credential references
and bindings, and user relink — never App substitution for User_required.

### 6. Minimal build / integrations boundary

Minimal `clawq-min` disables GitHub user auth, webhooks, private delivery, and
networked route admin. Do not interpret "disabled in minimal build" as a
security proof of production isolation without the full binary path tests.

### 7. Dry-run is not live Pass

Software contract suites (`github_p21_pilot_dryrun`, attribution/vault suites)
prove gates and isolation **without** live Teams/GitHub. A blocked environment
must record Mode `dry-run_blocked` and **must not** claim live pilot Pass.

## Secrets and redaction (always)

Never paste into Rooms, tickets, git, or the redacted receipt:

- PEM private keys, `client_secret`, `webhook_secret`, PATs, installation tokens
- Access / refresh tokens, Authorization / Bearer headers, cookie material
- Device codes / user codes
- Private issue/PR bodies, customer chat text, raw webhook JSON dumps

Allowed:

- Credential **handles**, App id, installation id, repo full names, route ids
- Redacted digests, readiness booleans, stage/gate strings, actor mode labels
- Principal/GitHub user **ids** (public numeric), lineage ids, correlation ids
- Counts, timestamps, check pass/fail, outbox status totals
- Limitation tags and constant names (no secret material)

## Minimal operator verification script

After enable, and again after cleanup (expect inverse outcomes on cleanup):

1. Stage + production gate inspect (secret-free).
2. Readiness flags all true before enable; re-check after rollback.
3. Dual Principal redacted account status (A web, B device, C unlinked).
4. Dry plan for User_required (merge/review) — allow only under `p21_production`
   with live lease; deny under rollback/cleanup/safe_default.
5. Confirm no App/PAT fallback language missing on User_required deny.
6. Outbox metrics for Room: pending/in_flight/dead_letter.
7. After cleanup: `no_residual_authority`; bindings unlinked; vault pilot rows
   gone; stage `safe_default`; receipt complete with limitations.

Automated software contract (non-live):

```bash
make test-run ARGS="test github_p21_pilot_dryrun"
make test-run ARGS="test github_p21_docs"
make test-run ARGS="test github_attribution_rollout"
```

## Related modules

| Area | Modules |
|------|---------|
| Staged rollout | `github_attribution_rollout` |
| Policy / fallback | `github_attribution_policy`, `github_attribution_fallback` |
| Authorize / lease | `github_attribution_authorize`, `github_attribution_dispatch_lease` |
| Audit / previews | `github_attribution_audit` |
| Vault / recovery | `github_user_token_vault`, `github_user_token_vault_recovery`, `github_user_token_rewrap` |
| Bindings / invalidate | `github_account_binding`, `github_user_auth_invalidate`, `principal_unlink_split` |
| Routes / delivery | `github_route_admin`, `github_delivery_outbox`, `github_delivery_ops` |
| Enablement | `github_user_auth_enablement` |

## Task linkage

- Live / dry-run pilot exercise: **P21.M4.E2.T003**
- This guide + redacted filled receipt + cleanup result + limitations:
  **P21.M4.E2.T004**
- P19 pattern precursor: **P19.M4.E3.T003**
  (`p19-rollout-backout-guide.md`, `p19-redacted-pilot-receipt-template.md`)

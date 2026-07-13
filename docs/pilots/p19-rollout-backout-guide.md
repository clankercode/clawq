# P19 live pilot — rollout, backout, and cleanup

Operator runbook for the P19 GitHub App → Clawq → Teams pilot. Covers enabling
and disabling high-risk pilot gates, rolling back routes and delivery work, and
proving no residual delivery or action authority after cleanup.

Canonical contract:
[docs/plans/2026-07-12-github-item-room-routing.md](../plans/2026-07-12-github-item-room-routing.md).

Redacted receipt form:
[docs/pilots/p19-redacted-pilot-receipt-template.md](p19-redacted-pilot-receipt-template.md).

Route setup and readiness:
[docs/github-route-operator-contract.md](../github-route-operator-contract.md).

P21 production attribution handoff:
[docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md](../plans/2026-07-13-github-user-attribution-and-feature-discovery.md).

P19 → P21 migration matrix and staged gates:
[p21-attribution-migration-rollout.md](p21-attribution-migration-rollout.md)
(`Github_attribution_rollout`).

## Scope and non-goals

| In scope | Out of scope |
|----------|--------------|
| One named, isolated Teams Room pilot | Production multi-Room rollout |
| Time-bounded App-attributed high-risk gates | Principal-owned user leases (P21) |
| Route, outbox, dead-letter, managed-bundle cleanup | Secret-bearing Connector onboarding adapters |
| Redacted operational receipt | Advanced Org filters (P20) |

This pilot proves **P19 mechanics** (ingress, routes, journal/projections, cards,
outbox, confirmed actions, restart recovery). It is **not** production user
attribution or authorization.

## Safe default

All high-risk App-attributed pilot gates default to **off**:

| Action family | Default pilot name | Default |
|---------------|--------------------|---------|
| PR review submission | `p19-pr-review-pilot` | `enabled=false` |
| Issue create / close / reopen | `p19-issue-lifecycle-pilot` | `enabled=false` |
| Workflow dispatch | `p19-workflow-dispatch-pilot` | `enabled=false` |
| Code-changing work / constrained PR create | `p19-code-change-pilot` | `enabled=false` |
| Room-triggered background work | `p19-room-background-work-pilot` | `enabled=false` |
| Merge | `p19-merge-pilot` | `enabled=false` (also independently gated per Room/Repo) |

Gate shape (all families): `{ enabled; pilot_name; expires_at }`. When
`enabled=false` or `expires_at` is in the past, planning and apply fail closed
with a non-production message. If P21 user authorization is unavailable, there
is **no App/PAT fallback**.

Ordinary reads, search, status, explicit comments, ordinary metadata, and
reviewer *requests* follow Room/App policy without these gates. Merge and the
high-risk families above remain pilot- or P21-gated.

## Prerequisites before enable

1. **Isolated destination.** One named Teams Room dedicated to the pilot.
2. **Full binary.** Use full `clawq` (not minimal): App webhook, setup
   transactions, and networked route admin live in integrations.
3. **App setup complete.** Manifest → browser install/callback → resume plan →
   confirm/apply. Callback is **not** apply.
4. **Route readiness.** `assess_readiness` Pass (or Fail with deliberate repair
   notes). Org selectors require live Active App installation scope.
5. **Managed access.** Confirm setup-owned bundle linkage and that the Tool
   catalog includes expected GitHub tools on the **next turn** without restart.
6. **Time bound.** Choose an explicit `expires_at` (ISO-8601 UTC). Never enable
   without expiry for an open-ended “prod exception.”
7. **Authority.** Destination Room admin (or global admin for cross-Room). Record
   actor labels used during the window.

## Enable pilot gates (rollout)

High-risk gates are **named, independent, and time-bounded**. Enable only what
the pilot session will exercise; leave others off.

### Enable checklist

1. Confirm gates are currently default-off (inspect plan payloads or runtime
   config: `enabled=false`, no residual `expires_at` from a prior window).
2. For each family under test, set:
   - `enabled = true`
   - `pilot_name` = the canonical name from the table (or a room-local named
     window that still records the gate identity in receipts)
   - `expires_at = <UTC end of window>`
3. Re-check route capabilities (`allow_merge`, review/reply flags, etc.) — gate
   on alone is not enough if the route policy denies the family.
4. Record enablement in the redacted receipt: who, when, which gates, expiry,
   Room id handle, App/installation ids (public metadata only).
5. Exercise the pilot path **inside the window**:
   - New card, card edit, thread reply, Room question
   - Reads / search / status
   - Comments / reviews
   - Each confirmed action family under test
   - Background work (if gated on)
   - Merge (if independently enabled)
   - Restart recovery + transient delivery retry
6. Every plan/receipt must record **actor labels** (`App` attribution for this
   pilot), **pilot_name**, and **gate state** (enabled + expiry). Plans must
   stay secret-free (no PEM, client/webhook secrets, PATs, bearer tokens).

### Enable anti-patterns

- Do not enable gates in non-isolated Rooms.
- Do not treat a successful pilot as production-ready enablement.
- Do not leave `enabled=true` after `expires_at` — treat expiry as mandatory
  disable even if operators forget cleanup.
- Do not “temporarily” re-enable after cleanup without a new receipt and window.
- Do not document App pilot enablement as a substitute for P21 `User_required`.

## Disable pilot gates (immediate backout)

Disable is the first step of any incident or scheduled end-of-window cleanup.

1. Set every pilot gate to `enabled = false`.
2. Clear or leave `expires_at` irrelevant once disabled (disabled wins).
3. Confirm planning for each high-risk family fails with the standard
   unavailable reason (includes “not production-ready” and, when applicable,
   “P21 user authorization disabled/unavailable; no App/PAT fallback”).
4. Confirm no new high-risk applies succeed after disable (in-flight confirmed
   applies that already passed authorize may still complete — drain those
   deliberately; see outbox section).
5. Record disable timestamp and operator in the receipt.

**Merge** remains independently off per Room/Repo in addition to the pilot gate.
Re-check route policy so merge cannot reappear via a residual capability flag
misread as enablement.

## Route and managed-access cleanup

Pilot only **pilot-created** routes and **setup-owned** managed linkage. Do not
strip independent/manual Room grants.

### Order

1. **Inspect** destination routes: ids, selectors, enabled flags, managed
   bundle/feature ids, revision.
2. **Disable** pilot routes (`Github_route_admin.plan_disable` → confirm/apply)
   so match outcomes become intentional **Muted** (fail-closed, no-fallthrough).
3. **Remove** (soft) routes that should free the active slot
   (`plan_remove` → confirm/apply). Soft remove retains history; it does not
   re-enable delivery.
4. **Managed bundle detach:** disable/remove of the last managed feature detaches
   **only** setup-owned linkage. Verify independent grants remain.
5. **Catalog:** next Room turn must no longer expose pilot-only mutation tools
   that depended on detached setup-owned access. Ordinary admin-granted tools
   may remain.
6. **Do not** casually delete webhook delivery-ledger accepts; ACK independence
   from Connector means historical delivery ids stay for dedupe.

### Match expectations after cleanup

| Decision | Expected after pilot cleanup |
|----------|------------------------------|
| **Muted** | Pilot Room still has a disabled most-specific route |
| **No_route** | Routes fully removed for that destination/selector |
| **Matched** | Must not occur for pilot selectors after disable/remove |

Use `Github_route_ops.explain_match` on a sample envelope to prove silence.

## Outbox, dead letters, and delivery drain

Delivery uses a 24-hour retrying outbox with per-event dead letters
(`Github_delivery_outbox`, ops in `Github_delivery_ops`). Webhook HTTP ACK is
**independent** of outbox completion.

### Drain / cancel during backout

1. **Metrics** for the pilot Room:
   `Github_delivery_ops.metrics ~db ~room_id` — pending, in_flight, succeeded,
   dead_letter, superseded.
2. **Diagnose:** `Github_delivery_ops.diagnose ~db ~room_id` — counts, oldest
   pending age, dead-letter samples (errors already redacted at store time).
3. **Supersede open work** for pilot items so retries stop:
   - Per item: `Github_delivery_outbox.supersede_pending_for_item` (or
     reconcile helper) marks Pending/In_flight as **Superseded**.
   - Succeeded and dead-letter rows are left for audit; they do not retry.
4. **Stale in-flight after crash:** `repair_stale_in_flight` requeues stuck
   In_flight → Pending; only use when you intend to finish delivery. Prefer
   supersede during cleanup.
5. **Dead letters:** inspect with `list_dead_letters ~room_id`. Do **not**
   requeue (`requeue_dead_letter`) during cleanup unless repairing a live
   production destination. Record sample ids (not intent bodies with private
   content) in the receipt.
6. **Catch-up:** avoid running catch-up reconciliation against a Room you are
   decommissioning; it can enqueue a current-state intent. If catch-up already
   ran, supersede the new pending rows.
7. **Prove idle:** metrics show `pending=0`, `in_flight=0` for the pilot Room
   (dead_letter/superseded/succeeded may be non-zero for audit).

### Delivery authority after cleanup

- No Pending/In_flight intents for the pilot Room.
- Disabled/removed routes → Muted/No_route (no new journal→outbox path).
- High-risk gates off → no new mutation applies.
- Historical Succeeded/Dead_letter rows and delivery-ledger accepts are not
  residual *authority*; they are audit/dedupe artifacts.

## Background jobs and action receipts

1. Cancel or let complete any Room-triggered background work started under the
   pilot; do not leave pilot-gated runners scheduled.
2. Confirmed action receipts and backlinks remain as durable audit; they must
   stay redacted (handles, public ids, head SHAs, pilot_name — never secrets).
3. Webhook self-loop prevention stays active: Clawq-originated mutations must not
   create duplicate visible Room noise when residual webhooks arrive after
   mute. Muted/No_route handles residual events.

## Cleanup result (success criteria)

Cleanup is complete only when **all** of the following hold:

1. **Gates off.** Every P19 high-risk pilot gate `enabled=false` (and/or past
   `expires_at` with enabled false).
2. **No high-risk applies.** Fresh plan attempts for merge, review submit, issue
   lifecycle, workflow dispatch, and code-change fail closed with the pilot
   unavailable reason (and P21 no-fallback language when user auth is off).
3. **Routes quiet.** Pilot selectors Muted or No_route; no Matched pilot traffic
   into the isolated Room.
4. **Setup-owned only detached.** Managed bundle features created by pilot setup
   detached; non-setup grants untouched.
5. **Outbox idle.** Pilot Room `pending=0` and `in_flight=0`; dead letters
   inspected and not requeued for “keep trying.”
6. **No residual action authority.** No pilot gate, route capability, or
   setup-owned catalog entry alone can authorize high-risk mutation.
7. **Redacted receipt filed.** Completed
   [receipt template](p19-redacted-pilot-receipt-template.md) with timestamps,
   checks, cleanup result, and **no secrets/private content**.
8. **P21 handoff documented.** Production user-required enablement is explicitly
   deferred; installing P21 code with user auth disabled must leave high-risk
   actions **denied**, never re-open the P19 App pilot path, and never fall back
   silently to App/PAT.

## P21 migration handoff (safe path)

Document this block on every pilot receipt:

1. **P19 App pilot is temporary.** After verification it is disabled and cleaned
   up, not inherited as a production exception.
2. **Installing P21 while user authorization is disabled** leaves high-risk
   actions denied.
3. **Production enablement** requires P21 migration of those families to
   `User_required`, admin readiness, and an audited attribution gate with a
   current Principal-owned user lease.
4. **Authority loss** (revoke, stale lease, disabled user auth) returns repair or
   reconfirmation — it never re-enables the P19 App pilot path or falls back
   silently to App/PAT.
5. Rollback of P21 staged enablement restores the **safe disabled** state without
   actor-mode substitution (see P21 plan verification section).

## Secrets and redaction (always)

Never paste into Rooms, tickets, git, or the redacted receipt:

- PEM private keys, `client_secret`, `webhook_secret`, PATs, installation tokens
- Authorization / Bearer headers, cookie material, device codes
- Private issue/PR bodies, customer chat text, raw webhook JSON dumps

Allowed:

- Credential **handles**, App id, installation id, repo full names, route ids
- Redacted digests, readiness booleans, pilot_name, actor mode labels
- Counts, timestamps, check pass/fail, outbox status totals

## Minimal operator verification script

After enable, and again after cleanup (expect inverse outcomes on cleanup):

1. Inspect destination routes (selectors, enabled, managed linkage).
2. Assess App + route readiness.
3. Explain match on a sample envelope (Matched vs Muted vs No_route).
4. Confirm Tool catalog refresh on next turn (no restart).
5. Confirm gate state in a dry plan payload (`pilot_name`, enabled/expiry) without
   secrets.
6. Outbox metrics for Room: pending/in_flight/dead_letter.
7. After disable: high-risk plan fails closed; outbox idle; receipt complete.

## Related modules

| Area | Modules |
|------|---------|
| Pilot gates | `github_pr_review_actions`, `github_issue_actions`, `github_workflow_dispatch`, `github_code_change_action`, `github_room_background_work`, `github_merge_action` |
| Shared workflow | `github_action_workflow`, `github_action_reconcile` |
| Routes | `github_route_admin`, `github_route_apply`, `github_route_ops`, `github_route_match` |
| Delivery | `github_delivery_outbox`, `github_delivery_ops`, `github_delivery_reconcile` |
| Setup | `github_app_setup_tx`, `github_app_setup_callback`, `github_app_setup_resume`, `setup_plan_bundle` |

## Task linkage

- Live pilot exercise: **P19.M4.E3.T002**
- This guide + redacted receipt template + cleanup result: **P19.M4.E3.T003**

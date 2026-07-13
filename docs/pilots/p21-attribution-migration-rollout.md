# P19 → P21 attribution migration and staged rollout

Operator and implementer contract for the versioned action state matrix and the
explicit pilot, production, rollback, and cleanup gates (P21.M3.E2.T006).

Canonical plan:
[docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md](../plans/2026-07-13-github-user-attribution-and-feature-discovery.md).

P19 pilot mechanics (App interim only):
[p19-rollout-backout-guide.md](p19-rollout-backout-guide.md).  
P21 production enable / rollback / cleanup / limitations:
[p21-rollout-backout-guide.md](p21-rollout-backout-guide.md).

Runtime module: `Github_attribution_rollout` (`src/github_attribution_rollout.ml`).
Policy defaults: `Github_attribution_policy`. Visible fallback rules:
`Github_attribution_fallback`.

## Goals

1. **Versioned matrix.** Every P19 read, mutation, and background action maps
   from legacy App/PAT (or named pilot App) behavior to a P21 target of
   `App_installation`, `User_preferred`, or `User_required`, with preview-actor,
   fallback, delayed-work, receipt, and webhook semantics recorded per row.
2. **Safe default on upgrade.** Installing P21 leaves high-risk `User_required`
   **disabled** until Principal, vault, policy, private-delivery, repair, and
   backout readiness pass. No silent actor change and no weakened confirmation.
3. **Explicit gates.** Pilot enable, production enable, rollback, and cleanup
   are audited transitions — never implicit side effects of deploy or config
   drift.
4. **No App/PAT fallback on authority loss.** When the production gate is off or
   user auth is unavailable, required work fails closed; it does **not** reopen
   the P19 App pilot path as a silent substitute.

## Matrix version

| Field | Value |
|-------|-------|
| `matrix_version` / `schema_version` | `1` |
| Owner | `Github_attribution_rollout` |
| Bump when | Row set, stage semantics, or gate shapes change incompatibly |

Export: `Github_attribution_rollout.matrix_to_json (matrix ())`.

Every `Github_attribution_policy.defaults` action appears in the matrix with
matching target attribution, tier, and `pilot_allowed`
(`matrix_covers_policy_defaults`).

## Action state matrix (v1 summary)

| Action | Surface | Legacy P19 | P21 target | Pilot name | Preview | Fallback | Delayed | Receipt | Webhook |
|--------|---------|------------|------------|------------|---------|----------|---------|---------|---------|
| `read` / `search` / `get_status` / `get_item` / `list_room_items` | read | App | App_installation | — | not required | none | none | app actor | ambient |
| `pat_read` | read | PAT | Pat_compat | — | not required | none | none | app actor | ambient |
| `comment` / `label` / `assign` / `review_request` | mutation | App | User_preferred | — | names actor | visible App | pin lineage | resolved mode | match receipt |
| `review_submit` | mutation | pilot App | User_required | `p19-pr-review-pilot` | user only | none | pin lineage | pilot | self-loop guard |
| `issue_create` / `issue_close` / `issue_reopen` | mutation | pilot App | User_required | `p19-issue-lifecycle-pilot` | user only | none | pin lineage | pilot | self-loop guard |
| `workflow_dispatch` | mutation | pilot App | User_required | `p19-workflow-dispatch-pilot` | user only | none | pin lineage | pilot | self-loop guard |
| `code_change` | mutation | pilot App | User_required | `p19-code-change-pilot` | user only | none | pin lineage | pilot | self-loop guard |
| `merge` | mutation | pilot App | User_required | `p19-merge-pilot` | user only | none | pin lineage | pilot | self-loop guard |
| `room_background_work` | background | pilot App | User_required | `p19-room-background-work-pilot` | user only | none | pin lineage | pilot | self-loop guard |

Unknown actions fail closed as `User_required` / Critical with no pilot and
production gate required.

### Semantics glossary

| Tag | Meaning |
|-----|---------|
| **Preview names actor** | Confirmation envelope must name User or App (for visible fallback). |
| **Preview user only** | `User_required` preview must name the Principal-owned user. |
| **Visible App fallback** | `User_preferred` only; policy + preview must both name App; never silent. |
| **No fallback** | Never treat App/PAT as a substitute for a failed user path. |
| **Pin actor lineage** | Durable work pins immutable Actor evidence + logical binding lineage. |
| **Match receipt** | Resulting webhooks correlate to native attribution receipts once. |
| **Self-loop guard** | Clawq-originated mutations do not re-notify the Room as external events. |

## Stages

| Stage | String | Meaning |
|-------|--------|---------|
| Safe default | `safe_default` | Upgrade/install default. Production gate off; pilots off. Reads still work. |
| P19 pilot | `p19_pilot` | One or more named, time-bounded App pilots enabled. Not production user auth. |
| P21 production | `p21_production` | User attribution after readiness + audited production enable. |
| Rollback | `rollback` | Production/pilot paths closed; no actor-mode substitution; drain/reconfirm. |
| Cleanup | `cleanup` | Post-disable residual-authority proof before declaring complete. |

Default stage: **`safe_default`**.

```
safe_default ──pilot_enable──► p19_pilot ──pilot_disable──► safe_default
     │                              │
     │                              │ (after pilot cleanup; not automatic)
     └────production_enable─────────┴──► p21_production
                                              │
                                    production_disable / rollback
                                              │
                                              ▼
                                          rollback ──cleanup──► safe_default
```

Illegal without re-validation: `rollback`/`cleanup` → `p21_production` or
`p19_pilot` (would reopen authority silently).

## Gates

### 1. Pilot enable (`Gate_pilot_enable`)

Shape: `{ enabled; pilot_name; expires_at; audit_ref }`.

| Rule | Requirement |
|------|-------------|
| Default | `enabled=false` for every high-risk family |
| Enable | `enabled=true`, non-empty `pilot_name`, non-empty `expires_at` (ISO-8601 UTC), `audit_ref` |
| Open-ended | **Forbidden** — missing/empty expiry while enabled is treated inactive |
| From stages | `safe_default` or `p19_pilot` only |
| Production | Remains **off**; pilot does not enable user attribution |
| Effective path | Matching active pilot → `path_pilot_app` for `pilot_allowed` rows only |

See [p19-rollout-backout-guide.md](p19-rollout-backout-guide.md) for Room
isolation, route, and outbox procedures.

### 2. Production enable (`Gate_production_enable`)

Shape: `{ enabled; audit_ref; enabled_at }`.

| Rule | Requirement |
|------|-------------|
| Default | `enabled=false` |
| Readiness | All of: `principal_ready`, `vault_ready`, `policy_ready`, `private_delivery_ready`, `repair_ready`, `backout_ready` |
| Audit | Non-empty `audit_ref` |
| From stages | Not from `rollback` or `cleanup` (finish cleanup first) |
| Effective path | Stage `p21_production` + gate on + readiness → `path_user` for User_* rows |
| Fallback | Still governed by `Github_attribution_fallback` (visible App only for User_preferred) |

`User_required` **never** falls back to App/PAT when the gate is on but the user
path fails — repair or reconfirmation only.

### 3. Rollback (`Gate_rollback`)

Shape: `{ active; reason; audit_ref; restores_stage=safe_default }`.

| Rule | Requirement |
|------|-------------|
| Intent | Restore safe disabled state without actor-mode substitution |
| Production | Forced `enabled=false` |
| Pilot | Must not be re-enabled by rollback |
| In-flight | Drain or fail with reconfirmation; do not switch User ⇄ App |
| Next | Proceed to cleanup proof |

### 4. Cleanup (`Gate_cleanup`)

Shape: `{ active; audit_ref; residual_authority_cleared; pilot_credentials_destroyed; bindings_unlinked }`.

Cleanup is complete only when **all** hold:

1. Production gate off.
2. Every pilot gate inactive (disabled and/or expired).
3. `residual_authority_cleared=true` (routes quiet, outbox idle, no high-risk apply).
4. `pilot_credentials_destroyed=true` (pilot secrets/bindings destroyed as applicable).
5. `bindings_unlinked=true` (or asserted true for App-only pilot with no user bindings).
6. Redacted receipt filed with timestamps and checks (no secrets).

Predicate: `Github_attribution_rollout.no_residual_authority`.

## Production readiness checklist

| Flag | Evidence examples |
|------|-------------------|
| `principal_ready` | Verified Principals / identity links for pilot actors; no unresolved legacy rows authorizing user work |
| `vault_ready` | Master-key source, vault CRUD, generation CAS, lease invalidation |
| `policy_ready` | Attribution policy matrix loaded; tool catalog freeze; confirm/apply paths |
| `private_delivery_ready` | Connector can deliver auth URLs/device codes privately |
| `repair_ready` | Admin diagnostics distinguish policy/identity/auth/delivery failures without secrets |
| `backout_ready` | Rollback + cleanup runbook exercised (this doc + [p21-rollout-backout-guide.md](p21-rollout-backout-guide.md); P19 guide for App interim only) |

`readiness_complete` is true only when every flag is true.

## Resolve behavior (summary)

`Github_attribution_rollout.resolve` is pure and injectable:

| Condition | Effective path |
|-----------|----------------|
| Target `App_installation` | `path_app_primary` (always) |
| Target `Pat_compat` | `path_pat_compat` |
| Stage `rollback` / `cleanup` + user-attributed | `path_denied` |
| Production on + readiness + stage `p21_production` + User_* | `path_user` |
| Pilot active + `pilot_allowed` + production not fully open | `path_pilot_app` |
| Else User_* | `path_denied` with repair-oriented message (includes no App/PAT fallback language when user auth unavailable) |

Downstream modules (`Github_attribution_authorize`, fallback, dispatch lease)
still revalidate live authority; this module only stages **which path class** is
open.

## Operator sequence (recommended)

1. **Deploy P21** with production gate off → matrix at `safe_default`.
2. Optionally run a **named P19 App pilot** (`pilot_enable`) in an isolated Room;
   file redacted receipt; `pilot_disable` + cleanup when done.
3. Complete **readiness** checks; record evidence.
4. **`production_enable`** with audit ref → stage `p21_production`.
5. Exercise User_preferred and User_required families with real Principals.
6. On incident or schedule end: **`rollback`** then **`cleanup`** until
   `no_residual_authority` holds; stage returns to `safe_default`.

## Anti-patterns

- Treating a successful P19 App pilot as production user attribution.
- Enabling production without all readiness flags.
- Open-ended pilot windows (`expires_at` missing).
- Reopening pilot App as a silent fallback when user auth fails under production.
- Rollback that lands in `p21_production` or re-enables pilot.
- Actor-mode substitution on retry (User → App or App → User) — forbidden by
  fallback/authorize; rollout must not encourage it.

## Related modules

| Module | Role |
|--------|------|
| `Github_attribution_policy` | Per-action risk tier + attribution + pilot_allowed |
| `Github_attribution_fallback` | Visible App fallback / fail-closed User_required |
| `Github_attribution_authorize` | Full allow/deny after policy + live evidence |
| `Github_attribution_dispatch_lease` | Opaque lease after final revalidation |
| `Github_attribution_rollout` | Matrix + stages + gates (this document) |

## Tests

Suite: `github_attribution_rollout` (`test/test_github_attribution_rollout.ml`).

```bash
make test-run ARGS="test github_attribution_rollout"
```

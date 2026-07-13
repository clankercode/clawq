# P21 Teams dual-attribution pilot checklist

**Task:** `P21.M4.E2.T003`  
**Runbook:** [p21-teams-dual-attribution-pilot-runbook.md](p21-teams-dual-attribution-pilot-runbook.md)  
**Receipt template:** [p21-redacted-pilot-receipt-template.md](p21-redacted-pilot-receipt-template.md)  
**Rollout / backout / cleanup / limitations:** [p21-rollout-backout-guide.md](p21-rollout-backout-guide.md)  
**Filled dry-run receipt:** [receipts/p21-dual-attr-20260713-dryrun.md](receipts/p21-dual-attr-20260713-dryrun.md)  
**Canonical acceptance body:** Under staged attribution rollout
(`safe_default` → readiness → `p21_production`), two Teams Principals in one
Channel link separate GitHub accounts via **web** and **device** flows; GitHub
attributes each action family correctly; an unlinked third user is denied.
Exercise permitted App fallback, delayed work across normal refresh,
lineage-breaking relink/revoke, key rotation, SSO/permission denial, restart,
webhook reconciliation, rollback, cleanup, and return the gate to **safe
default**.

Use this checklist during a live pilot or as a dry-run gate list. Mark each row
`Pass` / `Fail` / `Skip` with evidence (redacted). **Do not mark Pass for live
signals if the live pilot was not executed.**

Pilot name: _______________________  
Room id / session key: _______________________  
Window (UTC): _______________________ → _______________________  
Principal A (web) handle: _______________________  
Principal B (device) handle: _______________________  
Participant C (unlinked): _______________________  
Operator: _______________________  
Date: _______________________  
**Mode:** [ ] Live  [ ] Dry-run / blocked  

### Dry-run / blocked status (fill when not live)

| Field | Value |
|-------|--------|
| Live pilot status | **BLOCKED** (if dry-run) |
| Block reason | e.g. no Teams pilot room + dual GitHub user OAuth credentials + public webhook URL |
| Dry-run software contract | [ ] `make test-run ARGS="test github_p21_pilot_dryrun"` PASS |
| Claim | Software contract proven only; **not** live pilot execution |

---

## A. Preconditions and safety

| # | Acceptance signal | Result | Evidence (redacted) |
|---|-------------------|--------|---------------------|
| A1 | Single **named isolated** Teams Room selected | | |
| A2 | Full `clawq` binary (integrations); daemon for webhooks/outbox | | |
| A3 | Teams connector configured; bot in pilot channel | | |
| A4 | Public HTTPS base URL for Teams + GitHub callbacks/webhooks | | |
| A5 | Stage **`safe_default`**; production gate **off**; P19 pilots **off** | | |
| A6 | Merge capability off by default for Room/Repo | | |
| A7 | No PEM / client secret / webhook secret / PAT / access / refresh / device code in plans, inspect, or Channel | | |
| A8 | Two distinct pilot GitHub users + third unlinked Teams participant identified | | |

---

## B. Readiness and production enable

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| B1 | `principal_ready` evidence recorded | | |
| B2 | `vault_ready` | | |
| B3 | `policy_ready` (matrix / catalog) | | |
| B4 | `private_delivery_ready` | | |
| B5 | `repair_ready` | | |
| B6 | `backout_ready` (this checklist + runbook) | | |
| B7 | `readiness_complete` true before enable | | |
| B8 | Audited `Gate_production_enable` → stage `p21_production` | | |
| B9 | Production enable **rejected** without readiness (spot) | | |
| B10 | P19 App pilot path **not** opened as production substitute | | |

---

## C. Dual Principal linking (web + device)

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| C1 | Principal A links GitHub via **web OAuth + PKCE** | | |
| C2 | Auth URL delivered **privately** to A; Room sees neutral status only | | |
| C3 | Callback once; binding + vault ref + lineage for A | | |
| C4 | Principal B links GitHub via **device** flow | | |
| C5 | Device code delivered **privately** to B | | |
| C6 | B binding + lineage distinct from A (ids, github_user_id, lineage_id) | | |
| C7 | Participant C has **no** authorizing GitHub binding | | |
| C8 | Preference / eligible resolve correct per Room/Repo for A and B | | |
| C9 | No cross-Principal borrow of leases or vault material | | |

---

## D. Attribution matrix — reads and User_preferred

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| D1 | Reads (`read` / `search` / `get_*` / `list_room_items`) use App_installation | | |
| D2 | A: user-attributed comment (preview names user) → GitHub as A | | |
| D3 | B: user-attributed ordinary mutation → GitHub as B | | |
| D4 | Visible App fallback only when policy + preview name App | | |
| D5 | No silent App/PAT fallback when user path fails under user-named preview | | |
| D6 | C denied on User_preferred with redacted repair reason | | |
| D7 | Receipts record requested/resolved mode (secret-free) | | |

---

## E. User_required action families

Enable only under production gate. Defaults remain deny when gate off.

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| E1 | **PR review submit** as user (A or B) with receipt | | |
| E2 | **Issue create** | | |
| E3 | **Issue close or reopen** | | |
| E4 | **Typed workflow_dispatch** | | |
| E5 | **Code-changing work** and/or constrained PR create | | |
| E6 | **Merge** with live policy revalidation | | |
| E7 | **Room background work** with pinned lineage | | |
| E8 | Dual attribution: at least one family exercised as **both** A and B | | |
| E9 | C denied on User_required (no App/PAT fallback language) | | |
| E10 | Production gate off/rollback → User_required **fails closed** | | |
| E11 | Opaque dispatch lease only after final revalidation | | |
| E12 | Tokens never appear in runner/shell/prompt/Git transport | | |

If a family is intentionally out of pilot scope, mark **Skip** with reason
(still must remain fail-closed without user path).

---

## F. Delayed work, refresh, relink, revoke

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| F1 | Delayed/background job pins Actor + binding lineage | | |
| F2 | Normal token refresh within lineage keeps job authorized | | |
| F3 | Relink / split / new lineage → job fails closed; reconfirm required | | |
| F4 | Authorization revoke / unlink disables binding, leases, secrets | | |
| F5 | Retry never switches User ⇄ App | | |

---

## G. Key rotation and SSO / permission denial

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| G1 | Key / vault rotation invalidates or rewraps as designed | | |
| G2 | Post-rotation mutation revalidates; no stale lease reuse | | |
| G3 | SSO/SAML denial fails closed with actionable reason | | |
| G4 | Permission/scope denial classified without secrets | | |

---

## H. Restart, outbox, webhook

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| H1 | Daemon restart recovers pending / stale In_flight | | |
| H2 | Actor snapshot survives restart; revalidation current | | |
| H3 | Webhook delivery id Duplicate on redelivery | | |
| H4 | Native receipt matches webhook once; no self-loop re-card | | |
| H5 | Cross-Principal receipt isolation | | |
| H6 | Dead letters inspectable; metrics redacted | | |

---

## I. Rollback, cleanup, safe default

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| I1 | Rollback / production disable with audit ref | | |
| I2 | User paths denied under rollback; no actor substitution | | |
| I3 | P19 pilot path **not** re-enabled | | |
| I4 | Cleanup flags: residual cleared, pilot credentials destroyed, bindings unlinked | | |
| I5 | `no_residual_authority` true | | |
| I6 | Stage returns to **`safe_default`** | | |
| I7 | Outbox quiet for pilot Room; routes muted/removed as planned | | |
| I8 | Redacted artifacts ready for `P21.M4.E2.T004` | | |

---

## J. Rollup

| Area | Pass | Fail | Skip |
|------|-----:|-----:|-----:|
| A Preconditions | | | |
| B Readiness / enable | | | |
| C Dual link | | | |
| D Preferred / reads | | | |
| E User_required families | | | |
| F Delayed / lineage | | | |
| G Rotation / SSO | | | |
| H Restart / webhook | | | |
| I Rollback / cleanup | | | |
| **Total** | | | |

### Overall pilot result

- [ ] **Live pilot PASS** — all non-Skip rows Pass; cleanup complete; safe default  
- [ ] **Live pilot FAIL** — failures listed below; cleanup still completed  
- [ ] **Blocked / dry-run only** — live Teams room, dual GitHub OAuth, or public webhook unavailable; runbook+checklist+dry-run suite committed **without claiming live execution**

### Failures / skips (redacted)

| ID | Reason | Follow-up |
|----|--------|-----------|
| | | |

### Sign-off

Operator: _______________________  
Date: _______________________  
Next task: `P21.M4.E2.T004` (redacted receipt, backout, cleanup result, limitations)

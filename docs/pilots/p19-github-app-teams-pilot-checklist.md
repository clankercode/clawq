# P19 GitHub App → Clawq → Teams pilot checklist

**Task:** `P19.M4.E3.T002`  
**Runbook:** [p19-github-app-teams-pilot-runbook.md](p19-github-app-teams-pilot-runbook.md)  
**Canonical acceptance body:** In one named isolated Teams Room complete App
setup, confirm/apply, immediate Tool-catalog refresh, routing, cards,
delivery/restart, reads, comments/reviews, and each confirmed action family.
High-risk App-attributed actions run only under an explicit time-bounded pilot
feature gate that is off by default and cannot be generalized as
production-ready; verify actor labels, receipts/webhooks, cleanup, and backout.

Use this checklist during a live pilot or as a dry-run gate list. Mark each row
`Pass` / `Fail` / `Skip` with evidence (redacted). **Do not mark Pass for live
signals if the live pilot was not executed.**

> **Current mutation boundary:** no live GitHub REST mutation dispatcher is
> implemented for P19.M4.E1 ordinary collaboration/reviewer/Issue actions.
> `Github_action_workflow` confirm/apply
> must fail closed before
> `Applied`, receipt,
> or webhook correlation; live mutation rows are blocked, not Pass/Fail pilot
> exercises. Grounding/read checks cover journal/projection data only (the live
> fetch interface is best-effort and no fully live T002 read surface exists).

Pilot name: _______________________  
Room id / session key: _______________________  
Window (UTC): _______________________ → _______________________  
Operator: _______________________  
Date: _______________________  
Mode: [ ] Live  [ ] Dry-run / blocked  

---

## A. Preconditions and safety

| # | Acceptance signal | Result | Evidence (redacted) |
|---|-------------------|--------|---------------------|
| A1 | Single **named isolated** Teams Room selected (not a production general channel) | | |
| A2 | Full `clawq` binary (integrations) in use; daemon reachable for webhooks/outbox | | |
| A3 | Teams connector configured; bot present in pilot channel | | |
| A4 | Public HTTPS base URL can receive Teams + GitHub App webhook/callback | | |
| A5 | All high-risk pilot gates **default off** before enablement | | |
| A6 | Merge capability **off** by default for Room/Repo | | |
| A7 | No PEM / client secret / webhook secret / PAT / bearer in plans, inspect, or Channel | | |

---

## B. Assisted App setup (manifest → callback → plan → apply)

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| B1 | Setup transaction created bound to pilot Room (`Github_app_setup_tx`) | | |
| B2 | Browser manifest install completed for pilot org/repos only | | |
| B3 | Callback exchange verifies state once and stores **credential handles** only | | |
| B4 | Resume surfaces confirmable `Setup_plan` (`Github_app_setup`) in originating Room | | |
| B5 | Readiness reports app identity, scope, permissions, webhook, Connector | | |
| B6 | Confirm/apply with matching plan id + digest succeeds once | | |
| B7 | Stale/expired plan is **regenerated**, not applied | | |
| B8 | Live installation scope Active for pilot repos | | |
| B9 | Channel/audit output contains **no** secrets | | |

---

## C. Route apply and immediate Tool catalog

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| C1 | Route plan_create for pilot destination (Repo or Org/Item as chosen) | | |
| C2 | Apply attaches managed access bundle when configured | | |
| C3 | Route inspect shows expected selector, enabled, comment mode, revision | | |
| C4 | `assess_readiness` overall **Pass** (or documented Warn with owner) | | |
| C5 | `explain_match` → **Matched** for a sample pilot envelope | | |
| C6 | **Next Room turn** sees newly enabled GitHub tools **without** Clawq restart | | |
| C7 | Cross-Room apply without authority **fails closed** | | |
| C8 | Org route refused under PAT (App required) | | |

---

## D. Webhook ingress (ACK independent of Teams)

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| D1 | Ingress path `/github/app/webhook` (or configured equivalent) | | |
| D2 | Valid HMAC accepted; bad signature rejected | | |
| D3 | First `X-GitHub-Delivery` Accepted and ledgered | | |
| D4 | Redelivery of same delivery id → **Duplicate** (no second work item) | | |
| D5 | Suspended/out-of-scope install/repo rejected | | |
| D6 | HTTP 2xx does **not** wait on Connector/card success | | |

---

## E. Journal, projection, Teams cards

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| E1 | Matched event journaled (`Github_room_event_journal`); agent **not** woken | | |
| E2 | Lifecycle open (PR/Issue) creates new Teams Adaptive Card | | |
| E3 | Minor state change **edits** current card (or coherent update path) | | |
| E4 | Projection state matches journal reduce (`Github_item_projection`) | | |
| E5 | Comment mode `summary`: metadata/counts only, **no** comment body leak | | |
| E6 | Card/actions resolve back to correct item context | | |
| E7 | Plain fallback path available for non-Teams destinations (spot or test) | | |

---

## F. Outbox, restart, retry

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| F1 | Delivery intents enqueued in outbox after routing | | |
| F2 | Successful Teams delivery marks outbox **Succeeded** | | |
| F3 | Transient failure schedules backoff retry | | |
| F4 | Daemon restart recovers stale **In_flight** / pending work | | |
| F5 | Catch-up reconciliation collapses to **one** current-state delivery per item | | |
| F6 | Dead letters inspectable; requeue is explicit (`Github_delivery_ops`) | | |
| F7 | Metrics/diagnose redacted and actionable | | |

---

## G. Conversational grounding and reads

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| G1 | Thread reply to card uses correct item context | | |
| G2 | Room @mention / question grounded on journal + live GitHub state | | |
| G3 | `Get_item` / `Search_items` / `Get_status` / `List_room_items` succeed in-room | | |
| G4 | Cross-room / unauthorized repo tool calls **Denied** with redacted reason | | |

---

## H. Ordinary collaboration (currently BLOCKED before live dispatch)

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| H1 | Explicit comment preview → confirm → **Blocked**; no GitHub comment | Skip / Blocked | |
| H2 | Label mutation under `allow_label` → **Blocked** before dispatch | Skip / Blocked | |
| H3 | Assign mutation under `allow_assign` → **Blocked** before dispatch | Skip / Blocked | |
| H4 | Request reviewers under ordinary path → **Blocked** before dispatch | Skip / Blocked | |
| H5 | Capability false → **Denied** before API | | |
| H6 | No apply/native-attribution receipt or correlation is recorded | Pass | |
| H7 | No mutation-derived webhook is expected | Skip / N/A | |

---

## I. High-risk App-attributed families (currently BLOCKED / SKIP)

Enable only for the named pilot window. Defaults remain off outside pilot.

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| I0 | Gate set enabled with explicit `pilot_name` + `expires_at` for this Room only | | |
| I1 | PR review submission → **Blocked / Skip**; no receipt | Skip / Blocked | |
| I2 | Issue create → **Blocked / Skip**; no receipt | Skip / Blocked | |
| I3 | Issue/PR close or reopen → **Blocked / Skip**; no receipt | Skip / Blocked | |
| I4 | Typed workflow_dispatch is separate scope; **Skip** | Skip | |
| I5 | Code-changing work / constrained PR create is separate scope; **Skip** | Skip | |
| I6 | Merge is separately scoped; **Skip** unless its own dispatcher is verified | Skip | |
| I7 | Background Room work is separately scoped; **Skip** | Skip | |
| I8 | No action receipt exists while dispatch is blocked | Pass | |
| I9 | No mutation webhook/correlation exists while dispatch is blocked | Pass | |
| I10 | Gate **disabled** or **expired** → high-risk preview/apply **fails closed** | | |
| I11 | No silent fallback to App/PAT when user auth unavailable | | |
| I12 | High-risk actions **not** presented as production-ready | | |

If a family is intentionally out of pilot scope, mark **Skip** with reason (still
must remain gated off).

---

## J. Mute and routing edge cases

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| J1 | Most-specific disabled route → **Muted** (no fallthrough to Org/Repo) | | |
| J2 | Explain lists winner and shadowed broader routes | | |
| J3 | Issue transfer deduped to ≤1 delivery per Room (if exercised) | | |
| J4 | At most one active route per `(destination, canonical selector)` | | |

---

## K. Cleanup and backout

| # | Acceptance signal | Result | Evidence |
|---|-------------------|--------|----------|
| K1 | All high-risk pilot gates **disabled** after verification | | |
| K2 | High-risk actions deny after cleanup | | |
| K3 | Pilot routes disabled/removed; managed-only linkage detached | | |
| K4 | Independent/manual grants preserved if present | | |
| K5 | App install restricted or removed from pilot-only scope as planned | | |
| K6 | Outbox residual documented or drained | | |
| K7 | Redacted metrics/inspect/readiness exported for T003 receipt | | |
| K8 | No production inheritance of P19 App pilot exception | | |
| K9 | P21 handoff note: user-required path remains denied until audited enablement | | |

---

## L. Rollup

| Area | Pass | Fail | Skip |
|------|-----:|-----:|-----:|
| A Preconditions | | | |
| B App setup | | | |
| C Routes / tools | | | |
| D Ingress | | | |
| E Cards / journal | | | |
| F Outbox / restart | | | |
| G Grounding / reads | | | |
| H Ordinary collab | | | |
| I High-risk families | | | |
| J Mute / edges | | | |
| K Cleanup | | | |
| **Total** | | | |

### Overall pilot result

- [ ] **Live pilot PASS** — all non-Skip rows Pass; cleanup complete  
- [ ] **Live pilot FAIL** — failures listed below; cleanup still completed  
- [ ] **Blocked / dry-run only** — live credentials or Teams room unavailable; runbook+checklist committed without claiming live execution  

### Failures / skips (redacted)

| ID | Reason | Follow-up |
|----|--------|-----------|
| | | |

### Sign-off

Operator: _______________________  
Date: _______________________  
Next task: `P19.M4.E3.T003` (redacted pilot receipt, rollout/backout guide, cleanup result)

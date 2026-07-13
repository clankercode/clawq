# P21 dual-attribution pilot receipt (filled dry-run example)

**Mode:** `dry-run_blocked`  
**Status:** `blocked`  
**Task (live):** P21.M4.E2.T003  
**Task (publish):** P21.M4.E2.T004  

This is a **filled redacted receipt** for the software-contract dry-run of the
P21 Teams dual-attribution pilot. It contains **no secrets**. It does **not**
claim that a live Teams pilot executed.

Companion template:
[../p21-redacted-pilot-receipt-template.md](../p21-redacted-pilot-receipt-template.md).  
Backout / cleanup / limitations:
[../p21-rollout-backout-guide.md](../p21-rollout-backout-guide.md).  
Runbook:
[../p21-teams-dual-attribution-pilot-runbook.md](../p21-teams-dual-attribution-pilot-runbook.md).

---

## 0. Document control

| Field | Value (redacted) |
|-------|------------------|
| Receipt id | `p21-dual-attr-20260713-dryrun` |
| Task (live) | P21.M4.E2.T003 |
| Task (publish) | P21.M4.E2.T004 |
| Operator principal handle | lab-operator (local dry-run) |
| Approver (if any) | n/a (blocked; no production enable) |
| Window start (UTC) | 2026-07-13T00:00:00Z (doc publish window) |
| Window end (UTC) | 2026-07-13T23:59:59Z |
| Clawq build / revision | worktree `bl/P21.M4.E2.T004` (see git HEAD at publish) |
| `matrix_version` | as loaded by `Github_attribution_policy` / rollout defaults in suite |
| Environment label | `lab-dryrun` |
| Mode | `dry-run_blocked` |
| Status | `blocked` |

**Statement of intent:** Prove the **software contract** for P21 staged
rollout, dual-Principal isolation semantics, and attribution matrix / fallback
paths offline. Live Microsoft Teams dual-Principal OAuth pilot remains
**BLOCKED** in this environment.

**Block reason:** no Teams pilot room + dual GitHub user OAuth credentials +
public webhook / callback URL available to the agent execution environment.

**Live pilot claim:** **NOT EXECUTED** — do not treat this receipt as live Pass.

---

## 1. Setup (Room + App + readiness)

| Check | Result | Notes (redacted) |
|-------|--------|------------------|
| Isolated named Teams Room | N/A | blocked — no pilot room |
| Full binary (not minimal) | Pass | dry-run suite links full modules |
| Public webhook/callback host | N/A | blocked — no public URL |
| App install Active (pilot repos) | N/A | blocked |
| User-auth OAuth + device config | N/A | blocked — no dual user OAuth |
| Private delivery ready | N/A | blocked |
| Vault ready | Pass (contract) | vault security + recovery suites separate |
| Policy / matrix ready | Pass (contract) | attribution policy + rollout pure tests |
| Repair / diagnostics ready | Pass (contract) | enablement diagnostics shape covered in unit suites |
| Backout path reviewed | Pass | [p21-rollout-backout-guide.md](../p21-rollout-backout-guide.md) |
| Secrets redacted in plans/inspect | Pass | this document has no secrets |

App id (public): n/a (dry-run)  
Installation id(s) (public): n/a  
Credential handles only (list): none — no live vault pilot rows created  

---

## 2. Staged rollout gates

| Stage / gate | Value | Audit ref (redacted) |
|--------------|-------|----------------------|
| Initial stage | `safe_default` | suite `github_p21_pilot_dryrun` |
| Production gate before pilot | `enabled=false` | default |
| P19 pilots before pilot | all off | default |
| Production enable `enabled_at` | n/a | **not enabled** (blocked) |
| Stage during exercise | `safe_default` (live); suite exercises `p21_production` in-memory only | pure transitions |
| Rollback reason | n/a live; suite covers `Gate_rollback` | `aud-p21-pilot-dryrun` pattern in tests |
| Cleanup complete | n/a live | procedure published in backout guide |
| Final stage | `safe_default` | required end state |

Confirm: high-risk User_required was **disabled by default** before enable.
**Yes** (asserted: `user_required_disabled_by_default`).

---

## 3. Dual Principals (web + device)

| Role | Principal id (redacted) | Link flow | GitHub user id | Lineage id | Binding status end |
|------|-------------------------|-----------|----------------|------------|--------------------|
| A | synthetic-A (suite) | web PKCE (contract) | synthetic | lin_a (contract) | n/a live |
| B | synthetic-B (suite) | device (contract) | synthetic | lin_b (contract) | n/a live |
| C | synthetic-C (suite) | none / unlinked | — | — | denied (contract) |

Distinct Principals: **Yes** (isolation contract tests)  
Distinct GitHub users: **Yes** (isolation contract tests)  
Private delivery of auth materials: **N/A** (blocked)

---

## 4. Action families (attribution matrix)

### 4.1 Reads (App_installation)

| Family | Exercised | Result | Notes |
|--------|-----------|--------|-------|
| read / search / status / get_item / list | Y (contract) | Pass | path_app_primary always open in resolve |

### 4.2 User_preferred

| Family | Principal | Preview actor | Result | Receipt / correlation id |
|--------|-----------|---------------|--------|---------------------------|
| comment / label / assign / review_request | A/B synthetic | user | Pass (contract) | in-memory suite only |
| Visible App fallback (explicit) | — | app | Pass (contract) | only when policy+preview name App |
| Silent fallback denied | — | — | Pass (contract) | deny codes asserted |
| C denied | C | — | Pass (contract) | unlinked |

### 4.3 User_required

| Family | Principal | Apply once | Lease revalidated | Receipt id | Webhook reconcile once | Result |
|--------|-----------|------------|-------------------|------------|------------------------|--------|
| review_submit / issue_* / workflow_dispatch / code_change / merge / room_background_work | synthetic | n/a live | n/a live | n/a live | n/a live | Gate deny under safe_default; path_user under synthetic production+readiness |
| Dual A+B same family | — | — | — | — | — | isolation contract (no cross-borrow) |
| C denied (no App/PAT fallback) | C | — | — | — | — | Pass (contract) |

Actor mode labels observed: suite asserts `User` / `App` string labels and
`used_app_fallback` only on explicit preferred fallback path.

---

## 5. Delayed work, refresh, relink, revoke

| Check | Result | Notes |
|-------|--------|-------|
| Delayed job pin lineage | Pass (contract) | delayed / durable job attribution suites |
| In-lineage refresh OK | Pass (contract) | vault generation CAS suites |
| Relink/split breaks job | Pass (contract) | isolation / invalidate suites |
| Revoke/unlink disables authority | Pass (contract) | revocation + invalidate |
| No User ⇄ App on retry | Pass (contract) | fallback + rollout |

---

## 6. Key rotation and SSO / permission denial

| Check | Result | Notes |
|-------|--------|-------|
| Vault / key rotation | Pass (contract) | rewrap / recovery suites |
| Stale lease rejected | Pass (contract) | lease generation CAS |
| SSO denial fail-closed | Pass (contract) | authorize deny classification |
| Permission denial classified | Pass (contract) | redacted reason classes |

---

## 7. Restart, delivery, webhooks

| Check | Result | Notes |
|-------|--------|-------|
| Restart recovers outbox / in-flight | N/A live | delivery suites cover mechanics offline |
| Actor snapshot preserved | Pass (contract) | actor attribution modules |
| Delivery id Duplicate | N/A live | ledger dedupe unit coverage elsewhere |
| Receipt ↔ webhook once | N/A live | reconcile contract tests |
| Cross-Principal isolation | Pass (contract) | p21 integration / isolation |
| Outbox metrics snapshot | n/a live | pending=0 / in_flight=0 expected after cleanup when live |

---

## 8. Cleanup result

| Step | Done | Evidence (redacted) |
|------|------|---------------------|
| Production gate `enabled=false` | Y | never enabled live; suite restores deny |
| User paths deny after rollback | Y | suite `Gate_rollback` / cleanup path denies |
| P19 pilots remain off | Y | defaults + dry-run docs contract |
| Pilot bindings unlinked / credentials destroyed | Y (n/a live) | no live pilot bindings created |
| Routes disabled/removed as planned | Y (n/a live) | no pilot routes created |
| Outbox pending=0, in_flight=0 for Room | Y (n/a live) | no pilot Room traffic |
| `no_residual_authority` | Y (procedure) | predicate documented; suite asserts cleanup flags |
| Stage `safe_default` | Y | end state required |

**Cleanup status:** `n/a_blocked` (no residual live authority created; backout
guide published for when live runs)

**Residual risks / follow-ups:**

- Live pilot still required for end-to-end Teams private delivery, dual OAuth,
  and webhook correlation under real connectors.
- Operators must re-enter runbook §0 when credentials and public URL exist.

---

## 9. Limitations and explicit statements

1. Production user attribution requires readiness + audited production gate; it
   is **not** inherited from a P19 App pilot.
2. User_required **never** falls back to App/PAT when user auth fails.
3. Visible App fallback is **only** for User_preferred when policy and preview
   both name App.
4. Rollback does **not** reopen pilot App or substitute actor modes.
5. Cleanup must prove `no_residual_authority` before declaring complete.
6. **Whole-store vault rollback:** a whole-store rollback under the same
   available key is **not detectable** without an external monotonic anchor.
   Record AEAD and token-generation CAS do not detect replacement of the entire
   store with an internally consistent older snapshot under the same key.
   Constant:
   `Github_user_token_vault_recovery.whole_store_rollback_detectable_without_external_anchor = false`.
   Tag:
   `whole_store_rollback_not_detectable_without_external_monotonic_anchor`.
   V1 makes **no whole-store anti-rollback claim**; backup selection and restore
   authorization are an explicit operational trust boundary.
7. Master-key compromise or unrecoverable loss requires destructive
   disable-and-relink — not in-place recovery and not App substitution for
   User_required.
8. This receipt is **dry-run_blocked** only; software contract suites PASS do
   **not** equal live pilot Pass.
9. Limitations observed beyond the above: none additional (no live residual).

---

## 10. Forbidden content checklist (sign-off)

Confirm **none** of the following appear in this receipt or linked exports:

- [x] PEM / private keys  
- [x] `client_secret` / `webhook_secret`  
- [x] PAT / access_token / refresh_token / installation token / bearer  
- [x] Device codes / user codes  
- [x] Private PR/issue bodies or review text beyond public titles/numbers  
- [x] Raw webhook payloads  
- [x] Customer chat transcripts  

Operator sign-off: dry-run publisher (P21.M4.E2.T004)  
Date (UTC): 2026-07-13  

---

## 11. Dry-run evidence commands

```bash
make test-run ARGS="test github_p21_pilot_dryrun"
make test-run ARGS="test github_p21_docs"
make test-run ARGS="test github_attribution_rollout"
```

Expected: suites PASS; live pilot remains **BLOCKED** until Teams pilot room +
dual GitHub user OAuth + public webhook URL are available.

```text
Receipt id: p21-dual-attr-20260713-dryrun
Mode: dry-run_blocked
Block: no Teams room + dual GitHub user OAuth + public webhook URL
Dry-run suite: github_p21_pilot_dryrun (software contract)
Live pilot: NOT EXECUTED
Stage: safe_default throughout live surface (no production enable)
Cleanup: n/a_blocked — no residual live authority; guide published
Limitations: whole-store rollback under same key undetectable without external anchor
Secrets: none in this document
```

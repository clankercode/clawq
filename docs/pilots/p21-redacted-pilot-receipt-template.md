# P21 redacted dual-attribution pilot receipt template

Fill one copy per live pilot window. Store only **redacted** values: public
ids, handles, counts, digests, pass/fail, and stage/gate strings. **Never**
include secrets, PEMs, PATs, access/refresh tokens, device codes, bearer
tokens, private issue/PR bodies, raw webhook JSON, or customer chat text.

Companion runbook:
[p21-teams-dual-attribution-pilot-runbook.md](p21-teams-dual-attribution-pilot-runbook.md).  
Companion checklist:
[p21-teams-dual-attribution-pilot-checklist.md](p21-teams-dual-attribution-pilot-checklist.md).  
Migration contract:
[p21-attribution-migration-rollout.md](p21-attribution-migration-rollout.md).

Canonical plan:
[docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md](../plans/2026-07-13-github-user-attribution-and-feature-discovery.md).

Task publish: `P21.M4.E2.T004` (this template is prepared by T003; filled
receipt is T004).

---

## 0. Document control

| Field | Value (redacted) |
|-------|------------------|
| Receipt id | `p21-dual-attr-YYYYMMDD-<short>` |
| Task (live) | P21.M4.E2.T003 |
| Task (publish) | P21.M4.E2.T004 |
| Operator principal handle | |
| Approver (if any) | |
| Window start (UTC) | |
| Window end (UTC) | |
| Clawq build / revision | |
| `matrix_version` | |
| Environment label | e.g. `lab` / `staging-isolated` |
| Mode | `live` \| `dry-run_blocked` |
| Status | `planned` \| `in_progress` \| `cleanup_complete` \| `aborted` \| `blocked` |

**Statement of intent:** This pilot proves **P21 production user attribution**
with two Teams Principals (web + device OAuth) in one Channel under staged
rollout. It is **not** a P19 App high-risk pilot. Production enable is audited;
cleanup returns **`safe_default`** and destroys residual pilot authority.

If Mode is `dry-run_blocked`, state the block reason and that only the software
contract dry-run suite was executed — **do not claim live pilot Pass**.

Block reason (if any):  

---

## 1. Setup (Room + App + readiness)

| Check | Result | Notes (redacted) |
|-------|--------|------------------|
| Isolated named Teams Room | Pass / Fail / N/A | Room handle / id hash only |
| Full binary (not minimal) | Pass / Fail / N/A | |
| Public webhook/callback host | Pass / Fail / N/A | host only |
| App install Active (pilot repos) | Pass / Fail / N/A | app id, installation id public |
| User-auth OAuth + device config | Pass / Fail / N/A | |
| Private delivery ready | Pass / Fail / N/A | |
| Vault ready | Pass / Fail / N/A | |
| Policy / matrix ready | Pass / Fail / N/A | matrix_version |
| Repair / diagnostics ready | Pass / Fail / N/A | |
| Backout path reviewed | Pass / Fail / N/A | |
| Secrets redacted in plans/inspect | Pass / Fail / N/A | |

App id (public):  
Installation id(s) (public):  
Credential handles only (list):  

---

## 2. Staged rollout gates

| Stage / gate | Value | Audit ref (redacted) |
|--------------|-------|----------------------|
| Initial stage | `safe_default` | |
| Production gate before pilot | `enabled=false` | |
| P19 pilots before pilot | all off | |
| Production enable `enabled_at` | | |
| Stage during exercise | `p21_production` | |
| Rollback reason | | |
| Cleanup complete | Y/N | |
| Final stage | `safe_default` | |

Confirm: high-risk User_required was **disabled by default** before enable.
Yes / No  

---

## 3. Dual Principals (web + device)

| Role | Principal id (redacted) | Link flow | GitHub user id | Lineage id | Binding status end |
|------|-------------------------|-----------|----------------|------------|--------------------|
| A | | web PKCE | | | |
| B | | device | | | |
| C | | none / unlinked | — | — | denied |

Distinct Principals: Yes / No  
Distinct GitHub users: Yes / No  
Private delivery of auth materials: Pass / Fail / N/A  

---

## 4. Action families (attribution matrix)

### 4.1 Reads (App_installation)

| Family | Exercised | Result | Notes |
|--------|-----------|--------|-------|
| read / search / status / get_item / list | Y/N | | |

### 4.2 User_preferred

| Family | Principal | Preview actor | Result | Receipt / correlation id |
|--------|-----------|---------------|--------|---------------------------|
| comment | A / B | user / app | | |
| label | | | | |
| assign | | | | |
| review_request | | | | |
| Visible App fallback (explicit) | | app | | |
| Silent fallback denied | | | | |
| C denied | C | | | |

### 4.3 User_required

| Family | Principal | Apply once | Lease revalidated | Receipt id | Webhook reconcile once | Result |
|--------|-----------|------------|-------------------|------------|------------------------|--------|
| review_submit | | | | | | |
| issue_create | | | | | | |
| issue_close / reopen | | | | | | |
| workflow_dispatch | | | | | | |
| code_change / PR create | | | | | | |
| merge | | | | | | |
| room_background_work | | | | | | |
| Dual A+B same family | | | | | | |
| C denied (no App/PAT fallback) | C | | | | | |

Actor mode labels observed:  

---

## 5. Delayed work, refresh, relink, revoke

| Check | Result | Notes |
|-------|--------|-------|
| Delayed job pin lineage | Pass / Fail / N/A | |
| In-lineage refresh OK | Pass / Fail / N/A | generation advance only |
| Relink/split breaks job | Pass / Fail / N/A | |
| Revoke/unlink disables authority | Pass / Fail / N/A | |
| No User ⇄ App on retry | Pass / Fail / N/A | |

---

## 6. Key rotation and SSO / permission denial

| Check | Result | Notes |
|-------|--------|-------|
| Vault / key rotation | Pass / Fail / N/A | |
| Stale lease rejected | Pass / Fail / N/A | |
| SSO denial fail-closed | Pass / Fail / N/A | |
| Permission denial classified | Pass / Fail / N/A | |

---

## 7. Restart, delivery, webhooks

| Check | Result | Notes |
|-------|--------|-------|
| Restart recovers outbox / in-flight | Pass / Fail / N/A | |
| Actor snapshot preserved | Pass / Fail / N/A | |
| Delivery id Duplicate | Pass / Fail / N/A | |
| Receipt ↔ webhook once | Pass / Fail / N/A | |
| Cross-Principal isolation | Pass / Fail / N/A | |
| Outbox metrics snapshot | — | pending / in_flight / succeeded / dead_letter / superseded |

---

## 8. Cleanup result

| Step | Done | Evidence (redacted) |
|------|------|---------------------|
| Production gate `enabled=false` | Y/N | |
| User paths deny after rollback | Y/N | sample error class only |
| P19 pilots remain off | Y/N | |
| Pilot bindings unlinked / credentials destroyed | Y/N | |
| Routes disabled/removed as planned | Y/N | |
| Outbox pending=0, in_flight=0 for Room | Y/N | |
| `no_residual_authority` | Y/N | |
| Stage `safe_default` | Y/N | |

**Cleanup status:** `cleanup_complete` / `partial` / `not_started` / `n/a_blocked`  

**Residual risks / follow-ups:**  

---

## 9. Limitations and explicit statements

Record (edit only if policy changes; do not weaken):

1. Production user attribution requires readiness + audited production gate; it
   is **not** inherited from a P19 App pilot.
2. User_required **never** falls back to App/PAT when user auth fails.
3. Visible App fallback is **only** for User_preferred when policy and preview
   both name App.
4. Rollback does **not** reopen pilot App or substitute actor modes.
5. Cleanup must prove `no_residual_authority` before declaring complete.
6. Limitations observed in this pilot (list):  

---

## 10. Forbidden content checklist (sign-off)

Confirm **none** of the following appear in this receipt or linked exports:

- [ ] PEM / private keys  
- [ ] `client_secret` / `webhook_secret`  
- [ ] PAT / access_token / refresh_token / installation token / bearer  
- [ ] Device codes / user codes  
- [ ] Private PR/issue bodies or review text beyond public titles/numbers  
- [ ] Raw webhook payloads  
- [ ] Customer chat transcripts  

Operator sign-off: _______________  Date (UTC): _______________  

---

## 11. Example skeleton (illustrative only)

```text
Receipt id: p21-dual-attr-20260713-lab1
Mode: dry-run_blocked
Block: no Teams room + dual GitHub user OAuth + public webhook URL
Dry-run suite: github_p21_pilot_dryrun PASS
Live pilot: NOT EXECUTED
Stage: safe_default throughout (no production enable)
Secrets: none in this document
```

Live skeleton (when executed):

```text
Receipt id: p21-dual-attr-20260713-lab1
Window: 2026-07-13T14:00:00Z → 2026-07-13T20:00:00Z
Room: teams:19:…@thread.tacv2 (hash: cd34)
A: web → gh user 1001 lineage lin_a
B: device → gh user 1002 lineage lin_b
C: unlinked denied
Stage: safe_default → p21_production → rollback → cleanup → safe_default
Families: comment (A,B), merge (A), review_submit (B); App fallback comment once
Cleanup: no_residual_authority; bindings unlinked; cleanup_complete
Secrets: none in this document
```

Copy sections into your lab ticket or `docs/pilots/receipts/` (if used). Do not
commit secrets.

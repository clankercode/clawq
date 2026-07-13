# P19 redacted pilot receipt template

Fill one copy per live pilot window. Store only **redacted** values: public
ids, handles, counts, digests, and pass/fail. **Never** include secrets, PEMs,
PATs, bearer tokens, private issue/PR bodies, raw webhook JSON, or customer chat
text.

Companion runbook:
[docs/pilots/p19-rollout-backout-guide.md](p19-rollout-backout-guide.md).

Canonical contract:
[docs/plans/2026-07-12-github-item-room-routing.md](../plans/2026-07-12-github-item-room-routing.md).

---

## 0. Document control

| Field | Value (redacted) |
|-------|------------------|
| Receipt id | `p19-pilot-YYYYMMDD-<short>` |
| Task | P19.M4.E3.T002 (live) / P19.M4.E3.T003 (publish) |
| Operator principal handle | |
| Approver (if any) | |
| Window start (UTC) | |
| Window end / `expires_at` (UTC) | |
| Clawq build / revision | |
| Environment label | e.g. `lab` / `staging-isolated` |
| Status | `planned` \| `in_progress` \| `cleanup_complete` \| `aborted` |

**Statement of intent:** This pilot proves P19 GitHub App → Clawq → Teams
mechanics. It is **not** production user attribution. High-risk App-attributed
actions run only under named time-bounded pilot gates that are **off by
default** and **disabled during cleanup**. Production enablement waits for P21.

---

## 1. Setup (App + Room)

| Check | Result | Notes (redacted) |
|-------|--------|------------------|
| Isolated named Teams Room | Pass / Fail | Room handle / id hash only |
| Full binary (not minimal) | Pass / Fail | |
| App manifest / setup transaction | Pass / Fail | tx id handle |
| Browser install + callback exchange | Pass / Fail | callback ≠ apply |
| Resume plan in originating Room | Pass / Fail | plan id + digest (not secrets) |
| Confirm / apply App setup | Pass / Fail | |
| Installation scope Active | Pass / Fail | installation id, org/repo public names |
| Webhook ingress ready (signature path) | Pass / Fail | public URL host only |
| Connector (Teams) ready | Pass / Fail | independent of GitHub ACK |
| Secrets redacted in plans/inspect | Pass / Fail | no PEM/client/webhook secret/PAT |

App id (public):  
Installation id(s) (public):  
Credential handles only (list):  

---

## 2. Routes and Tool catalog

| Check | Result | Notes |
|-------|--------|-------|
| Route plan create/update for pilot selectors | Pass / Fail | route ids, selectors |
| Confirm/apply routes | Pass / Fail | revision |
| Selector specificity understood (Item > Repo > Org) | Pass / Fail | |
| Fail-closed mute / no-fallthrough verified | Pass / Fail | explain_match |
| Org requires App (if Org selector) | Pass / Fail | |
| Managed setup-owned bundle attach | Pass / Fail | bundle/feature ids |
| Tool catalog refresh on next turn (no restart) | Pass / Fail | sample tool names only |
| assess_readiness overall | Pass / Warn / Fail | |

Matched sample envelope outcome: Matched / Muted / No_route  

---

## 3. Delivery (journal, cards, outbox)

| Check | Result | Notes |
|-------|--------|-------|
| New lifecycle card in Teams | Pass / Fail | |
| Card edit / replacement | Pass / Fail | |
| Thread reply tied to item context | Pass / Fail | |
| Room question grounded in journal + live GitHub | Pass / Fail | |
| Hidden journal / projection update | Pass / Fail | item keys only |
| Webhook ACK independent of Connector | Pass / Fail | delivery id accept once |
| Transient delivery retry | Pass / Fail | |
| Restart recovery (stale in-flight / pending) | Pass / Fail | |
| Dead letter inspect (if any) | Pass / Fail / N/A | sample outbox ids only |
| Outbox metrics snapshot | — | pending / in_flight / succeeded / dead_letter / superseded |

Plain-message fallback exercised (non-Teams or editless)? Yes / No / N/A  

---

## 4. Collaboration actions (ordinary)

| Family | Exercised | Result | Receipt / correlation id |
|--------|-----------|--------|---------------------------|
| Read / search / status | Y/N | Pass / Fail | |
| Explicit comment | Y/N | Pass / Fail | |
| Ordinary metadata mutation | Y/N | Pass / Fail | |
| Reviewer request (not submit) | Y/N | Pass / Fail | |

Actor mode labels observed (e.g. `App`):  

---

## 5. High-risk pilot gates and confirmed actions

### Gate enablement

| Pilot name | enabled | expires_at (UTC) | Families covered |
|------------|---------|------------------|------------------|
| `p19-pr-review-pilot` | | | PR review submission |
| `p19-issue-lifecycle-pilot` | | | Issue create / close / reopen |
| `p19-workflow-dispatch-pilot` | | | Typed workflow dispatch |
| `p19-code-change-pilot` | | | Code work / constrained PR create |
| `p19-room-background-work-pilot` | | | Room background work |
| `p19-merge-pilot` | | | Merge (also Room/Repo independent) |

Confirm: gates were **off by default** before this window. Yes / No  

### Confirmed action families

| Family | Preview+confirm | Apply once | Live revalidate | Receipt id | Webhook reconcile no-loop | Result |
|--------|-----------------|------------|-----------------|------------|---------------------------|--------|
| PR review submit | | | | | | |
| Issue create | | | | | | |
| Issue/PR close-reopen | | | | | | |
| Workflow dispatch | | | | | | |
| Code-changing work | | | | | | |
| Constrained PR create | | | | | | |
| Background work | | | | | | |
| Merge | | | | | | |

Each receipt must record: `pilot_name`, gate state, actor/attribution label,
target item key, head/base as applicable — **secret-free**.

Denial while gate off (sampled)? Pass / Fail / N/A  
Denial when user_auth unavailable (no App/PAT fallback language)? Pass / Fail / N/A  

---

## 6. Journal / outbox / Teams activity summary

| Metric | Value |
|--------|-------|
| Room handle | |
| Journal events accepted (count) | |
| Distinct item keys touched (count) | |
| Teams cards posted/edited (count) | |
| Outbox succeeded | |
| Outbox superseded | |
| Outbox dead_letter | |
| Action receipts durable (count) | |
| Backlinks written (count) | |

Timestamps (UTC): first event ____ ; last event ____ ; restart test at ____  

---

## 7. Checks matrix (roll-up)

| Area | Pass? |
|------|-------|
| Setup + secrets redaction | |
| Routes + catalog refresh | |
| Delivery + restart + retry | |
| Ordinary collab | |
| Each high-risk family under gate | |
| Actor labels + pilot_name in receipts | |
| Self-loop / no duplicate visible noise | |
| Cleanup (section 8) | |
| P21 handoff notes (section 9) | |

Overall pilot verification: **Pass / Fail / Partial**  

---

## 8. Cleanup result

Complete using
[rollout/backout guide](p19-rollout-backout-guide.md#cleanup-result-success-criteria).

| Step | Done | Evidence (redacted) |
|------|------|---------------------|
| All pilot gates `enabled=false` | Y/N | |
| High-risk plan fails closed after disable | Y/N | sample error class only |
| Pilot routes disabled/removed | Y/N | Muted / No_route |
| Setup-owned managed access detached only | Y/N | independent grants preserved |
| Outbox pending=0, in_flight=0 for Room | Y/N | metrics snapshot |
| Dead letters inspected, not requeued for keep-alive | Y/N | |
| Background pilot jobs cancelled/complete | Y/N | |
| No residual delivery or action authority | Y/N | |
| Catalog no longer offers pilot-only mutation via detached setup | Y/N | |

**Cleanup status:** `cleanup_complete` / `partial` / `not_started`  

**Residual risks / follow-ups:**  

---

## 9. P21 production-attribution handoff

Record these explicit statements (edit only if policy changes; do not weaken):

1. P19 high-risk App pilot gates are **disabled and cleaned up** after
   verification; they are **not** a production exception to inherit.
2. **Installing P21 while user authorization is disabled** leaves high-risk
   actions **denied**.
3. Production enablement requires P21 migration to **`User_required`**, admin
   readiness, and an audited attribution gate with a **current Principal-owned
   user lease**.
4. Authority loss returns **repair or reconfirmation** — never re-enables the
   P19 App pilot path and never falls back silently to **App/PAT**.
5. Limitations observed in this pilot (list):  

---

## 10. Forbidden content checklist (sign-off)

Confirm **none** of the following appear in this receipt or linked exports:

- [ ] PEM / private keys  
- [ ] `client_secret` / `webhook_secret`  
- [ ] PAT / installation token / bearer material  
- [ ] Private PR/issue bodies or review text beyond public titles/numbers  
- [ ] Raw webhook payloads  
- [ ] Customer chat transcripts  

Operator sign-off: _______________  Date (UTC): _______________  

---

## 11. Example skeleton (illustrative only)

```text
Receipt id: p19-pilot-20260713-lab1
Window: 2026-07-13T14:00:00Z → 2026-07-13T18:00:00Z
Room: teams:19:…@thread.tacv2 (hash: ab12)
App id: 123456; installation: 789012; repos: example/pilot-repo
Gates enabled: p19-pr-review-pilot, p19-merge-pilot (expires 18:00Z)
Routes: route_01 repo:example/pilot-repo enabled→disabled at cleanup
Outbox end state: pending=0 in_flight=0 succeeded=12 dead_letter=0 superseded=3
High-risk: review submit + merge exercised; receipts rec_… / rec_…
Cleanup: gates off; route muted; setup-owned bundle detached; cleanup_complete
P21: user auth not in scope; high-risk remain denied without User_required
Secrets: none in this document
```

Copy the sections above into your lab ticket or `docs/pilots/receipts/` (if
used) and replace the skeleton with real redacted values. Do not commit secrets.

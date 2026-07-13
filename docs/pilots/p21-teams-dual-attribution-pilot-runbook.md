# P21 Teams dual-attribution pilot runbook

**Task:** `P21.M4.E2.T003`  
**Goal:** Run (or dry-run) a live Microsoft Teams dual-Principal user-attribution
pilot that proves P21 staged rollout, separate web and device OAuth bindings,
per-Principal action attribution, fallback, delayed work, lineage breaks,
rotation, denial, restart, webhook reconciliation, rollback, cleanup, and return
to the safe default.  
**Canonical contract:**
[docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md](../plans/2026-07-13-github-user-attribution-and-feature-discovery.md)  
**Migration / gates:**
[p21-attribution-migration-rollout.md](p21-attribution-migration-rollout.md)  
**P19 precursor (App interim only):**
[p19-github-app-teams-pilot-runbook.md](p19-github-app-teams-pilot-runbook.md)  
**Companion checklist:**
[p21-teams-dual-attribution-pilot-checklist.md](p21-teams-dual-attribution-pilot-checklist.md)  
**Receipt template:**
[p21-redacted-pilot-receipt-template.md](p21-redacted-pilot-receipt-template.md)  
**Rollout / backout / cleanup / limitations (T004):**
[p21-rollout-backout-guide.md](p21-rollout-backout-guide.md)  
**Filled dry-run receipt example:**
[receipts/p21-dual-attr-20260713-dryrun.md](receipts/p21-dual-attr-20260713-dryrun.md)

This pilot proves **P21 production user attribution** under the staged
rollout gate — not the P19 App-attributed high-risk pilot path. High-risk
`User_required` families run only after audited `production_enable` with full
readiness. P19 App pilot gates stay **off** for this pilot unless an explicit
side-by-side App-fallback exercise is documented and time-bounded.

Use the **full** `clawq` binary (integrations). The minimal build disables
GitHub user auth, webhooks, private delivery, and networked route admin.

---

## 0. Preconditions

### 0.1 Named, isolated pilot room

Choose **one** Teams Room used only for this pilot:

| Field | Example / notes |
|-------|-----------------|
| Connector | `teams` |
| Team / channel | Isolated pilot channel (not a production org-wide room) |
| Conversation id | `19:…@thread.tacv2` (team channel) or `19:…@thread.v2` |
| Session key | `teams:{team_id}:{conversation_id}` |
| Room id | Durable Room id bound to that session |
| Pilot window | Explicit start/end timestamps (UTC) |
| Pilot name | e.g. `p21-dual-attr-pilot-YYYYMMDD` |

Record these values in the pilot receipt (T004); never record secrets.

### 0.2 Dual Principals and third (unlinked) participant

| Role | Requirement |
|------|-------------|
| **Principal A (web)** | Verified Teams actor → Principal; GitHub account linked via **web OAuth + PKCE** |
| **Principal B (device)** | Distinct verified Teams actor → Principal; GitHub account linked via **device code** flow |
| **Participant C (unlinked)** | Third Teams actor present in the Room **without** a GitHub binding (or with deliberately revoked/unlinked binding) |

A and B must resolve to **different** Principals and **different** GitHub
numeric user identities / logical binding lineages. Shared Rooms must never
borrow authority across Principals.

### 0.3 Credentials and reachability (live pilot only)

| Requirement | Notes |
|-------------|--------|
| Teams Bot Framework app | `channels.teams.app_id`, `app_secret`, `tenant_id`, public webhook path (default `/teams/webhook`) |
| Public HTTPS base URL | Serves Teams webhooks **and** GitHub App user-auth callbacks |
| GitHub App with user authorization | App id, private key handle, webhook secret handle, OAuth client configured for user access tokens with expiring refresh |
| Web OAuth callback | Exact redirect (S256 PKCE); path default under `/github/app/…` user-auth callback |
| Device authorization | Enabled when testing device link for Principal B |
| Installation | Active install on the pilot org or selected repos with permissions for exercised families |
| Private delivery | Connector can deliver auth URLs / device codes **privately** (not as Room-visible secrets) |
| Full binary + daemon | `clawq` with integrations; daemon for webhooks, outbox drain, token refresh |
| Admin principal | Room admin or global admin for production gate enable, account admin, plan confirm/apply |
| Two GitHub user accounts | Distinct pilot users for A (web) and B (device); SSO-capable org if SSO denial is in scope |

**Agent / dry-run environments** without a real Teams room, dual GitHub user
OAuth credentials, and a public webhook URL **must not** claim a live pilot.
See §14.

### 0.4 Implemented modules exercised by this pilot

| Stage | Modules |
|-------|---------|
| Staged rollout | `Github_attribution_rollout` (matrix, stages, gates, readiness, resolve) |
| Policy / fallback | `Github_attribution_policy`, `Github_attribution_fallback` |
| Authorize / lease | `Github_attribution_authorize`, `Github_attribution_dispatch_lease` |
| Previews / audit | `Github_attribution_audit` (+ action-family attribution modules) |
| Principals / actors | `Principal_identity`, `Principal_identity_store`, `Principal_resolve`, Teams ingress |
| Account binding | `Github_account_binding`, `Github_account_preference`, `Github_eligible_account_resolve` |
| Web OAuth | `Github_user_auth_*` PKCE / callback / activate |
| Device OAuth | `Github_user_auth_device`, device poll, shared activation |
| Vault / refresh | `Github_user_token_vault`, `Github_user_token_store`, `Github_user_token_lease` |
| Delayed work | `Github_delayed_attribution`, `Github_durable_job_actor_attribution` |
| Revocation / unlink | `Github_user_auth_revocation_webhook`, `Github_user_auth_invalidate`, `Principal_unlink_split` |
| Mutations | Collab, PR review, issue, workflow dispatch, code change, merge, room background |
| Delivery / webhooks | `Github_app_webhook_ingress`, journal, projection, outbox, `Github_action_reconcile` |
| Enablement UX | `Github_user_auth_enablement`, admin diagnostics / CLI account surfaces |

Domain APIs are authoritative; CLI/agent wrappers call these modules.

---

## 1. Baseline safety gates (before any mutation)

1. Confirm **stage is `safe_default`**:
   - Production gate `enabled=false`
   - All P19 high-risk pilot gates off (`p19-pr-review-pilot`,
     `p19-issue-lifecycle-pilot`, `p19-workflow-dispatch-pilot`,
     `p19-code-change-pilot`, `p19-merge-pilot`,
     `p19-room-background-work-pilot`)
2. Confirm merge capability **off** by default for Room/Repo.
3. Confirm redaction: PEM, client secret, webhook secret, PATs, access tokens,
   refresh tokens, device codes, and bearer material never appear in plans,
   inspect, Channel posts, or audit details.
4. Confirm personal tokens stay out of runners, shell, prompts, worktrees, and
   Git transport surfaces.

Only after §1, complete readiness and enable production for this pilot window
(§4). Disable production and complete cleanup in §13.

---

## 2. Teams connector readiness

1. Ensure Teams channel config is present and the bot is installed in the pilot
   team/channel.
2. Run connection smoke (when CLI available):

   ```bash
   clawq channel test teams
   ```

3. Post a manual message or Adaptive Card smoke so service URL, JWT validation,
   and allowlists are known-good.
4. Capture Room session key / room id for binding routes and auth delivery.
5. Confirm **three** Teams users can post in the Room (A, B, C).

**Acceptance signal:** inbound Teams activity reaches Clawq; outbound reply or
card appears; no secret leakage.

---

## 3. GitHub App + user-authorization readiness

1. App Active on pilot repos; installation scope reconciled.
2. User-authorization readiness: OAuth client, exact redirect URI, device flow
   settings, expiring token policy, vault master-key source.
3. Webhook ingress path ready (`/github/app/webhook` default) with secret handle.
4. Public base URL reachable for callback and webhook.
5. Production **readiness flags** evidence collected (see matrix doc):

| Flag | Live evidence examples |
|------|------------------------|
| `principal_ready` | A and B resolve; C does not authorize user work |
| `vault_ready` | Vault CRUD, generation CAS, lease invalidation smoke |
| `policy_ready` | Attribution matrix / tool catalog frozen for pilot |
| `private_delivery_ready` | Auth URL/device code delivered privately to A/B |
| `repair_ready` | Diagnostics distinguish policy/identity/auth/delivery |
| `backout_ready` | This runbook + cleanup checklist reviewed |

`readiness_complete` is true only when every flag is true
(`Github_attribution_rollout.readiness_complete`).

**Acceptance signal:** readiness complete; production gate still **off** until
§4 audited enable.

---

## 4. Staged rollout: production enable

Use `Github_attribution_rollout.validate_transition` semantics (operator
config/CLI as implemented):

1. From `safe_default` (or after any residual cleanup).
2. `Gate_production_enable` with non-empty `audit_ref`, `enabled_at`, and
   **complete** readiness.
3. Stage becomes `p21_production`. Production gate `enabled=true`.
4. P19 pilot App path remains **off** — production does not reopen App pilot as
   a silent substitute for failed user auth.
5. Record gate state, audit ref, and `matrix_version` in the receipt.

Illegal: enable from `rollback` / `cleanup` without finishing cleanup;
open-ended pilots; production without readiness.

**Acceptance signal:** User_preferred and User_required resolve to `path_user`
when user auth available; denied with repair-oriented message when gate off.

---

## 5. Dual Principal linking (web + device)

### 5.1 Principal A — web OAuth (S256 PKCE)

1. A invokes account link / authorize from the Room (or private DM path).
2. Room receives **neutral status only**; auth URL is private.
3. Browser completes exact-redirect callback; state/verifier bound once.
4. Shared activation: verified numeric GitHub identity → plan/confirm → binding
   with vault ref + logical lineage.
5. Preference set for pilot Room/Repo as needed
   (`Github_account_preference` / eligible resolve).

### 5.2 Principal B — device authorization

1. B starts device flow; device code and verification URI delivered **privately**.
2. Poll until terminal success or hard failure; no partial active binding on
   failure.
3. Shared activation path (flow-neutral with web) creates B's binding and
   lineage distinct from A.

### 5.3 Participant C — unlinked

1. C remains without GitHub binding (or explicitly unlinked).
2. Any User_preferred / User_required action for C **denies** with redacted
   repair guidance; no borrow of A/B tokens.

**Acceptance signal:** A and B have distinct Principal ids, GitHub user ids, and
lineage ids; C cannot obtain a user lease; private delivery never posts tokens
or codes into the shared Channel body.

---

## 6. Route and Tool catalog (if not already applied)

If the Room lacks GitHub routes/tools from a prior P19 setup:

1. Plan/apply Repo (or Org) route into the pilot Room (App install still
   required for ingress).
2. Confirm readiness Pass and tool catalog refresh **without restart**.
3. Keep high-risk Room capabilities aligned with production policy (merge still
   revalidated live).

**Acceptance signal:** reads and catalog tools resolve; mutations go through
P21 attribution, not silent App.

---

## 7. Attribution matrix — action families

Under `p21_production` + readiness + current user leases, exercise each family
with **both** A and B where practical (at least one family fully dual-attributed;
all families at least once by a linked Principal).

### 7.1 App_installation reads (always App-primary)

| Action | Expect |
|--------|--------|
| `read` / `search` / `get_status` / `get_item` / `list_room_items` | App path; no user lease required |

### 7.2 User_preferred ordinary mutations

| Action | Expect |
|--------|--------|
| `comment` / `label` / `assign` / `review_request` | User path when lease present; **visible App fallback only** if policy permits **and** preview names App |

Exercise for Principal A:

1. User-attributed comment (preview names user) → GitHub shows A's login.
2. Explicit App fallback preview (names App) → App actor; receipt
   `used_app_fallback` / resolved mode records App.
3. No silent fallback when preview names user but user path fails → deny/repair.

### 7.3 User_required high-risk families

| Family | Gate / policy | Expect |
|--------|---------------|--------|
| PR review submit | `review_submit` | User only; no App/PAT fallback |
| Issue create / close / reopen | `issue_*` | User only |
| Typed workflow_dispatch | `workflow_dispatch` | User only |
| Code change / constrained PR | `code_change` | User only |
| Merge | `merge` + live policy revalidation | User only |
| Room background work | `room_background_work` | User only; pin lineage |

For each family:

1. Preview names Principal-owned user; plan id + digest confirm.
2. Dispatch issues opaque lease only after final revalidation.
3. Receipt records immutable actor evidence, requested/resolved mode, lineage
   (secret-free).
4. GitHub attributes the action to the correct user (A vs B).
5. Resulting webhook reconciles once (`Github_action_reconcile`); no re-card
   loop / self-notify as external event.
6. C denied; wrong-Principal revalidation denied.

**Acceptance signal:** dual attribution correct on GitHub; unlinked denied;
receipts and webhooks consistent; no App/PAT language on User_required deny.

---

## 8. Delayed work, refresh, and lineage

1. Enqueue delayed / background work for Principal A with pinned Actor snapshot
   + binding lineage (`Github_delayed_attribution` /
   `Github_durable_job_actor_attribution`).
2. **Normal refresh:** advance token generation **within** the same logical
   lineage; delayed work still authorized.
3. **Lineage break — relink:** re-link A to a different GitHub user (or split)
   → in-flight delayed work fails closed; requires reconfirm under new lineage.
4. **Revoke:** GitHub App authorization revocation webhook (or local unlink)
   disables binding, leases, secrets; delayed work denied.
5. Retry never switches actor mode User ⇄ App.

**Acceptance signal:** in-lineage refresh succeeds; relink/revoke breaks
authority without silent substitution.

---

## 9. Key rotation and SSO / permission denial

1. **Vault / master-key rotation** (or staged rewrap): live leases invalidated
   as designed; re-auth or re-issue lease before next mutation; no plaintext in
   logs.
2. **SSO / SAML denial:** attempt user-required action against an SSO-gated org
   without SSO session → deny with actionable redacted reason (not App
   fallback).
3. **Permission denial:** insufficient App/user scope → deny before or at
   provider boundary; receipt/diagnostic classifies policy vs identity vs
   provider without secrets.

**Acceptance signal:** rotation and SSO/permission denials fail closed; no
secret leakage; no actor-mode substitution.

---

## 10. Restart recovery and delivery retry

1. With pending outbox and/or in-flight user action, restart the daemon.
2. Stale In_flight recovered; catch-up collapses to current item state.
3. Pinned actor snapshots survive restart; revalidation uses current vault
   generation, not stale plaintext.
4. GitHub redelivery of same `X-GitHub-Delivery` remains Duplicate at ingress.
5. Transient Teams failure backs off; dead letters inspectable.

**Acceptance signal:** restart-safe delivery and attribution; ACK independent of
Connector.

---

## 11. Webhook reconciliation

1. Each successful mutation produces at most one coherent Teams projection
   update for the item.
2. Native attribution receipt matches webhook once; Clawq-originated events do
   not re-open as external human events.
3. Cross-Principal isolation: A's receipt/correlation never authorizes B's
   apply or lease.

**Acceptance signal:** reconcile closed exactly once per action; isolation
holds.

---

## 12. Rollback

On schedule end, incident, or verification complete:

1. `Gate_rollback` (or production disable) with audit ref and reason.
2. Production gate forced `enabled=false`; stage `rollback`.
3. In-flight user work drains or fails with **reconfirmation** — no User ⇄ App
   substitution; P19 pilot path **not** re-enabled.
4. User_required / User_preferred resolve to deny under rollback/cleanup.

**Acceptance signal:** high-risk and preferred user paths denied; reads may
still use App_installation.

---

## 13. Cleanup and return to safe default

Perform even if the pilot partially failed. T004 publishes the redacted
receipt; this runbook defines operational steps.

1. Production gate off; stage through cleanup.
2. `Gate_cleanup` only when:
   - `residual_authority_cleared=true` (routes quiet, outbox idle, no high-risk
     apply)
   - `pilot_credentials_destroyed=true` (destroy pilot vault rows / device
     state / unused OAuth clients as applicable)
   - `bindings_unlinked=true` (unlink A/B pilot bindings or destroy pilot-only
     Principals per lab policy)
3. Disable or remove pilot routes if temporary; preserve independent grants.
4. P19 App pilot gates remain off.
5. Export redacted inspect/readiness/metrics for the receipt.
6. Predicate: `Github_attribution_rollout.no_residual_authority`.
7. Stage returns to **`safe_default`**.

**Acceptance signal:** `no_residual_authority`; stage `safe_default`; redacted
artifacts ready for `P21.M4.E2.T004`.

---

## 14. Dry-run / blocked environments

When a real **Teams pilot room**, **dual GitHub user OAuth credentials** (two
distinct users for web + device link), or a **public webhook / callback URL**
are **not** available in the execution environment:

1. Do **not** claim the live pilot executed.
2. Still use this runbook + the checklist as the dry-run procedure deliverable.
3. Record a blocked note with reason, for example:

   > Live pilot **BLOCKED**: no Teams pilot room + dual GitHub user OAuth
   > credentials + public webhook URL in this environment.

4. Automated coverage that proves the **software contract** (not live
   execution):

   ```bash
   make test-run ARGS="test github_p21_pilot_dryrun"
   ```

   Suite: `test/test_github_p21_pilot_dryrun.ml` — staged rollout gates,
   dual-Principal isolation semantics, attribution matrix / fallback paths,
   docs presence — **without** live Teams or GitHub network I/O.

5. Related integration suites (still non-live): `github_p21_integration`,
   `github_attribution_rollout`, `github_attribution_fallback`, family
   attribution tests.

Live pilot resume: re-enter at §0 with credentials, complete §§2–13, then T004.

---

## 15. Related docs

| Doc | Role |
|-----|------|
| [p21-attribution-migration-rollout.md](p21-attribution-migration-rollout.md) | Matrix, stages, gates |
| [p21-teams-dual-attribution-pilot-checklist.md](p21-teams-dual-attribution-pilot-checklist.md) | Gate checklist |
| [p21-redacted-pilot-receipt-template.md](p21-redacted-pilot-receipt-template.md) | Redacted receipt template |
| [p21-rollout-backout-guide.md](p21-rollout-backout-guide.md) | P21 enable/rollback/cleanup + limitations |
| [receipts/p21-dual-attr-20260713-dryrun.md](receipts/p21-dual-attr-20260713-dryrun.md) | Filled dry-run receipt (no secrets) |
| [p19-github-app-teams-pilot-runbook.md](p19-github-app-teams-pilot-runbook.md) | App interim pilot |
| [p19-rollout-backout-guide.md](p19-rollout-backout-guide.md) | P19 gate backout |
| Plan | [2026-07-13-github-user-attribution-and-feature-discovery.md](../plans/2026-07-13-github-user-attribution-and-feature-discovery.md) |

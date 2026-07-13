# P19 GitHub App → Clawq → Teams pilot runbook

**Task:** `P19.M4.E3.T002`  
**Goal:** Run (or dry-run) a live GitHub App → Clawq → Microsoft Teams pilot that
proves P19 routing, delivery, collaboration, and high-risk action mechanics.  
**Canonical contract:**
[docs/plans/2026-07-12-github-item-room-routing.md](../plans/2026-07-12-github-item-room-routing.md)  
**Operator contract:**
[docs/github-route-operator-contract.md](../github-route-operator-contract.md)  
**ADRs:** [0002](../adr/0002-use-unified-live-github-app-routes.md),
[0003](../adr/0003-require-plan-confirm-apply-for-agent-setup.md),
[0004](../adr/0004-room-session-owns-github-event-context.md),
[0008](../adr/0008-github-route-model-and-setup.md)  
**Companion checklist:**
[p19-github-app-teams-pilot-checklist.md](p19-github-app-teams-pilot-checklist.md)

This pilot proves **P19 mechanics**, not production user attribution. Production
enablement of user-required high-risk actions waits for P21.

Use the **full** `clawq` binary (integrations). The minimal build disables
GitHub App webhook, setup transactions, and networked route admin.

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
| Pilot name | e.g. `p19-app-teams-pilot-YYYYMMDD` |

Record these values in the pilot receipt (T003); never record secrets.

### 0.2 Credentials and reachability (live pilot only)

| Requirement | Notes |
|-------------|--------|
| Teams Bot Framework app | `channels.teams.app_id`, `app_secret`, `tenant_id`, public webhook path (default `/teams/webhook`) |
| Public HTTPS base URL | Serves both Teams and GitHub App callbacks/webhooks |
| GitHub App | App id, private key (credential handle / PEM path), webhook secret handle |
| Installation | Active install on the pilot org or selected repos |
| Full binary + daemon | `clawq` with integrations; daemon running for webhooks and outbox drain |
| Admin principal | Room admin or global admin for plan confirm/apply |

**Agent / dry-run environments** without a real Teams room and live GitHub App
installation **must not** claim a live pilot. Use this runbook as a dry-run
procedure and record the block reason (see §12).

### 0.3 Implemented modules exercised by this pilot

| Stage | Modules |
|-------|---------|
| App onboarding | `Github_app_setup_tx`, `Github_app_setup_callback`, `Github_app_setup_resume` |
| Installation scope | `Github_app_installation_scope`, `Github_app_token` |
| Webhook ingress | `Github_app_webhook_ingress` (path default `/github/app/webhook`) |
| Normalization / routes | `Github_event_envelope`, `Github_route_store`, `Github_route_match`, `Github_route_admin`, `Github_route_apply`, `Github_route_ops` |
| Journal / projection | `Github_room_event_journal`, `Github_item_projection`, `Github_item_context_resolve` |
| Delivery / Teams cards | `Github_delivery_intent`, `Github_teams_card_render`, `Github_plain_delivery_render`, `Github_delivery_outbox`, `Github_delivery_reconcile`, `Github_delivery_ops` |
| Room tools | `Github_room_tools` (`Get_item`, `Search_items`, `Get_status`, `List_room_items`) |
| Ordinary collab | `Github_collab_actions`, `Github_collab_grounding` |
| Confirmed actions | `Github_action_workflow`, `Github_pr_review_actions`, `Github_issue_actions`, `Github_workflow_dispatch`, `Github_code_change_action`, `Github_merge_action`, `Github_room_background_work` |
| Receipts / loops | `Github_action_reconcile` |
| Setup framework | `Setup_plan`, `Setup_plan_apply` |

Domain APIs are authoritative; CLI/agent wrappers call these modules.

---

## 1. Baseline safety gates (before any mutation)

1. Confirm **high-risk pilot gates default off**:
   - `p19-pr-review-pilot` — PR review submission
   - `p19-issue-lifecycle-pilot` — Issue create / open / close / reopen
   - `p19-workflow-dispatch-pilot` — typed workflow dispatch
   - `p19-code-change-pilot` — code-changing work / constrained PR create
   - `p19-merge-pilot` — merge
   - `p19-room-background-work-pilot` — Room background work
2. Confirm merge is **independently off** per Room/Repo capability policy.
3. Confirm route setup will **not** implicitly grant high-risk mutation authority.
4. Confirm redaction: PEM, client secret, webhook secret, PATs, and bearer tokens
   never appear in plans, inspect, Channel posts, or audit details.

Only after §1, enable a **single named time-bounded** pilot gate set for this
room (record name + `expires_at`). Disable all gates in cleanup (§11).

---

## 2. Teams connector readiness

1. Ensure Teams channel config is present and the bot is installed in the pilot
   team/channel.
2. Run connection smoke (when CLI available):

   ```bash
   clawq channel test teams
   ```

3. Post a manual message or Adaptive Card smoke to the pilot conversation so
   service URL, JWT validation path, and allowlists are known-good
   (`Teams_auth`, `Teams_api`, `Teams_webhook`).
4. Capture Room session key / room id for binding App setup and routes.
5. Optionally prepare room-agent profile via pilot wizard (Teams-first):

   ```bash
   CLAWQ_ADMIN=1 clawq rooms -- wizard plan \
     --profile-id p19-pilot-agent \
     --connector teams \
     --room "<conversation_id>"
   ```

   Apply only after plan review. See [pilot-setup-wizard.md](../pilot-setup-wizard.md).

**Acceptance signal:** inbound Teams activity reaches Clawq; outbound reply or
card appears in the pilot channel without secret leakage.

---

## 3. Assisted GitHub App onboarding (manifest → callback → plan)

Do **not** skip assisted setup. The pilot must begin with
manifest/callback/plan/confirm/apply in the named Teams Room.

### 3.1 Start setup transaction

Use `Github_app_setup_tx` (resumable, expiring, bound to principal + Room):

- Requested scope: pilot org or selected pilot repos only.
- Permissions: at least Issues/PRs read-write as required for the action families
  under test; Metadata read; Checks/Contents as needed for CI and review paths.
- Events: PR, issue, issue_comment, pull_request_review,
  pull_request_review_comment, check_run, check_suite, workflow_run (align with
  `default_events` / operator needs).
- Hook path: `/github/app/webhook` (default).
- Callback path: `/github/app/setup/callback` (default).
- TTL: default 30 minutes (`default_ttl_seconds`).

Channel-facing render must be **secret-free** (no PEM / client secret / webhook
secret).

### 3.2 Browser install

Open the emitted manifest URL, create/install the App, select only pilot repos
(or a dedicated pilot org). Survive process restart with the open transaction.

### 3.3 Callback exchange

`Github_app_setup_callback` verifies one-time state, exchanges code, materializes
App identity + **credential handles** (not plaintext secrets into the Room), and
**consumes** the transaction. **Callback is not apply.**

### 3.4 Resume in Room

`Github_app_setup_resume.resume_after_exchange` builds a confirmable
`Setup_plan` (`Github_app_setup`) for the active Room with readiness:

- app identity, live installation scope, permissions
- webhook reachability, Connector readiness
- managed-access diff, warnings

### 3.5 Confirm / apply

Apply only with matching **plan id + digest** after authority and readiness
recheck (`Setup_plan_apply` / domain adapter). Stale base revision or expiry →
regenerate (`regenerate_if_stale`), do not apply.

Post-apply: reconcile live installation scope
(`Github_app_installation_scope`).

**Acceptance signal:** App Active; credential handles stored; Room has a
confirmable apply receipt; no secrets in Channel or audit.

---

## 4. Route plan → confirm → apply + managed tools

### 4.1 Plan a route into the pilot Room

Prefer a **Repo** or **Org** route for the pilot repository (Item routes for
specific PRs if needed). Use `Github_route_admin.plan_create`:

- Destination: `Room` of the pilot room id
- Selector: Repo `owner/pilot-repo` or Org (App-only)
- Comment mode: start with `summary` (then exercise `threaded` / `off` if time)
- Capability policy: enable only ordinary collab needed first (`allow_reply`,
  labels/assign as required); keep merge/high-risk off until §8
- Managed feature/bundle ids when attaching setup-owned Room access

Inspect with `Github_route_admin.inspect` /
`list_inspect_for_destination` (channel-safe).

### 4.2 Apply

`Github_route_apply.apply_confirmed` with plan id + digest. Managed bundle
attaches; **tool catalog refreshes on the next turn without daemon restart**.

### 4.3 Readiness and match explain

```
Github_route_ops.assess_readiness  → overall Pass (or Fail with repair strings)
Github_route_ops.explain_match     → Matched for a sample pilot envelope
```

**Acceptance signal:** route enabled at expected revision; readiness Pass;
managed linkage present; next Room turn can resolve a newly enabled GitHub tool
(`Github_room_tools` / catalog) **without restart**.

---

## 5. Webhook ingress (independent of Connector)

Shared ingress: `Github_app_webhook_ingress` on `/github/app/webhook`.

1. Path match (query stripped).
2. HMAC `X-Hub-Signature-256` with webhook secret (credential boundary).
3. Non-empty `X-GitHub-Delivery`; ledger hit → **Duplicate** (safe redelivery).
4. Installation Active + repo authorized.
5. Event allow-list (`ping` always allowed).
6. Optional expected App id check.
7. Record delivery id **before** HTTP Accepted; **no Connector call** in this path.

Operator checks:

1. Send GitHub `ping` or open a low-risk draft PR / Issue on the pilot repo.
2. Confirm first delivery Accepted; redeliver same id → Duplicate.
3. If GitHub shows 2xx but Teams has no card, investigate **routing**,
   **journal/projection**, and **outbox/Connector** — not “missing ACK”.

**Acceptance signal:** verified ingress + durable delivery identity; duplicates
do not double-work.

---

## 6. Journal, projection, Teams cards, outbox

Pipeline after Accepted match:

1. **Match** — `Github_route_match` (`Item > Repo > Org`, no-fallthrough mute).
2. **Journal** — `Github_room_event_journal.append` (hidden event message; never
   wakes agent).
3. **Projection** — `Github_item_projection.reduce_entry` (lifecycle vs update).
4. **Delivery intent** — `Github_delivery_intent` (create/update card, reply,
   plain).
5. **Render** — `Github_teams_card_render` Adaptive Cards (schema 1.4); other
   connectors use plain fallbacks.
6. **Outbox** — `Github_delivery_outbox` enqueue; drain with retries (24h max age
   → dead letter); catch-up via `Github_delivery_reconcile`.
7. **Ops** — `Github_delivery_ops.metrics` / `diagnose` / `repair_stale_in_flight`.

Exercise:

| Scenario | Expected |
|----------|----------|
| PR or Issue opened | New lifecycle Adaptive Card in pilot Teams Room |
| Title/label/CI update | In-place card edit (`card_supports_edit`) or update card |
| Comment in `summary` mode | Counts/metadata only; no comment body leak |
| Transient Connector failure | Outbox Pending → retry backoff → Succeeded |
| Restart mid-flight | Stale In_flight reclaimed; catch-up one current state per item |
| Dead letter | Inspectable via `list_dead_letters`; optional `requeue_dead_letter` |

**Acceptance signal:** cards appear/edit in Teams; journal survives compaction;
outbox recovers across restart; no agent wake on webhook alone.

---

## 7. Conversational grounding and ordinary collaboration

### 7.1 Thread reply / Room mention / card action

Reply to a notification thread, @mention the bot, or use a supported card action.
`Github_item_context_resolve` + `Github_collab_grounding` ground the turn on
journal + live GitHub state for the item.

**Acceptance signal:** Room Session answers with the correct item context; no
cross-room leakage.

### 7.2 Read tools (next turn after route apply)

Invoke catalog tools without restart:

| Tool | Module API |
|------|------------|
| `Get_item` | `Github_room_tools` |
| `Search_items` | |
| `Get_status` | |
| `List_room_items` | |

Deny paths must redacted-fail when repo not in scope.

### 7.3 Ordinary mutations (not high-risk pilot families)

Via `Github_collab_actions` + `Github_action_workflow` preview/confirm/apply:

- Explicit comment (`allow_reply`)
- Label add/remove (`allow_label`)
- Assign add/remove (`allow_assign`)
- Reviewer request (`Github_pr_review_actions.plan_request_reviewers`) — ordinary
  path, not submit_review

Each mutation: redacted revision-bound preview → fresh confirm → durable receipt
→ webhook reconcile without loops (`Github_action_reconcile`).

**Acceptance signal:** GitHub reflects the change; Teams shows at most one
coherent projection update; correlation closes exactly once.

---

## 8. High-risk App-attributed action families (time-bounded gate only)

Enable pilot gates **only for the named pilot room and window**. Defaults remain
off outside this pilot and **must not** be presented as production-ready.

Use `Github_action_workflow.preview` / `apply_confirmed` with the matching
`pilot_gate` (`enabled=true`, `pilot_name`, `expires_at`).

| Family | Gate name (default) | Module |
|--------|---------------------|--------|
| PR review submission | `p19-pr-review-pilot` | `Github_pr_review_actions` |
| Issue create / open / close / reopen | `p19-issue-lifecycle-pilot` | `Github_issue_actions` |
| Typed workflow_dispatch | `p19-workflow-dispatch-pilot` | `Github_workflow_dispatch` |
| Code work + constrained PR create | `p19-code-change-pilot` | `Github_code_change_action` |
| Merge (also Room/Repo capability off by default) | `p19-merge-pilot` | `Github_merge_action` |
| Background Room work | `p19-room-background-work-pilot` | `Github_room_background_work` |

For each family exercised:

1. Preview shows target, actor mode (**App** pilot), effects, gate name/expiry.
2. Confirm once with plan id + digest; revalidation on expiry/stale head/policy.
3. Receipt records actor labels and gate state (secret-free).
4. Resulting webhook reconciles via `Github_action_reconcile` without re-enqueue
   loops or duplicate visible messages (`Closed` once; human events
   `Ignored_human_event`).
5. Denial when gate off/expired or capability false — **no silent App/PAT
   fallback** when user auth is unavailable.

**Acceptance signal:** each intended family succeeds under the gate; same action
fails closed when gate disabled; receipts and webhooks consistent.

---

## 9. Restart recovery and delivery retry

1. Enqueue or wait for open outbox work; stop/restart the daemon mid-flight.
2. Confirm `claim_due` recovers stale In_flight; catch-up collapses to current
   item state (`Github_delivery_reconcile`).
3. Simulate transient Teams/API failure; confirm exponential backoff and eventual
   success or dead letter after 24h policy (do not wait 24h in pilot — inject age
   or force failure path in a controlled fixture if live wait is impractical).
4. Confirm GitHub redelivery of the same `X-GitHub-Delivery` remains Duplicate
   at ingress and does not create a second card.

**Acceptance signal:** restart-safe delivery; independent ACK vs Connector.

---

## 10. Mute, transfer, and failure paths (spot checks)

| Check | Method |
|-------|--------|
| Most-specific mute | Disable Item/Repo route; Org must stay silent (`Muted`, no fallthrough) |
| Match explain | Winner + shadowed routes listed |
| Org requires App | PAT cannot apply Org route |
| Stale plan | Apply rejected; regenerate |
| Cross-room apply | Non-admin denied |
| Secret redaction | `Github_route_ops.redact_json` / inspect free of secret material |

---

## 11. Cleanup and backout (required)

Perform cleanup even if the pilot partially failed. T003 publishes the redacted
receipt; this runbook defines the operational steps.

1. **Disable all high-risk pilot gates** (`enabled=false`, clear or expire
   `expires_at`). Verify high-risk previews deny.
2. **Disable or remove pilot routes** (`Github_route_admin.plan_disable` /
   `plan_remove` → apply). Confirm managed-only bundle detach; preserve
   independent grants if any.
3. **Uninstall or restrict GitHub App** installation from non-pilot repos; rotate
   webhook secret / App credentials if they were exposed in logs (should not be).
4. **Teams:** leave or archive the pilot channel if temporary; revoke bot from
   room if required by policy.
5. **Outbox:** drain or document remaining dead letters; do not casually reopen
   closed delivery ledger rows.
6. **State:** export redacted inspect/readiness/metrics for the receipt; no
   secrets.
7. **Handoff to P21:** high-risk actions remain denied when user authorization is
   disabled; never re-enable the P19 App pilot path as a production exception.

**Acceptance signal:** gates off; routes muted/removed; pilot room quiet; redacted
artifacts ready for `P19.M4.E3.T003`.

---

## 12. Dry-run / blocked environments

When live GitHub App credentials, public webhook reachability, or a real Teams
pilot room are **not** available in the execution environment:

1. Do **not** claim the live pilot executed.
2. Still use this runbook + the checklist as the dry-run deliverable.
3. Record a blocked note with reason (credentials / Teams room / public URL).
4. Automated coverage remains the unit/integration tests for the modules above
   (`make test` focused suites; see test files matching `github_*`).

Live pilot resume: re-enter at §0 with credentials, complete §§2–11, then T003.

---

## 13. Related docs

| Doc | Role |
|-----|------|
| [github-route-operator-contract.md](../github-route-operator-contract.md) | Day-2 route ops |
| [github-integration.md](../github-integration.md) | App/PAT config and grants |
| [glossary-github-routes.md](../glossary-github-routes.md) | Vocabulary |
| [pilot-setup-wizard.md](../pilot-setup-wizard.md) | Room-agent wizard |
| [design/teams-reply-matrix.md](../design/teams-reply-matrix.md) | Teams threading |
| [TEAMS_API.md](../../src/TEAMS_API.md) | Bot Framework notes |
| Checklist | [p19-github-app-teams-pilot-checklist.md](p19-github-app-teams-pilot-checklist.md) |

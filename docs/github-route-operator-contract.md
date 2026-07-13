# GitHub route operator contract

Operator-facing contract for GitHub Item/Repo/Org routing, App onboarding, and
repair. Aligns with
[docs/plans/2026-07-12-github-item-room-routing.md](plans/2026-07-12-github-item-room-routing.md),
[docs/adr/0008-github-route-model-and-setup.md](adr/0008-github-route-model-and-setup.md),
and ADRs [0002](adr/0002-use-unified-live-github-app-routes.md) /
[0003](adr/0003-require-plan-confirm-apply-for-agent-setup.md).

Terms: [docs/glossary-github-routes.md](glossary-github-routes.md).

## Scope

| In scope | Out of scope (elsewhere) |
|----------|---------------------------|
| App manifest/callback setup resume | Secret-bearing Connector onboarding (Teams/Slack adapters) |
| Route plan / inspect / apply / disable / remove / preview, including typed advanced filters | Raw JSON predicates |
| Org vs Repo auth, mute semantics | Production user-attribution for high-risk actions (P21) |
| Webhook ingress readiness, delivery ledger | Full outbox/card pilot runbook (P19.M3/M4) |
| Redacted audit and repair steps | Raw JSON predicates |

Use the **full** `clawq` binary for GitHub App webhook, setup transactions, and
networked route admin. The **minimal** build disables integration-only surfaces;
do not expect partial Org/App behavior there.

## Invariants operators must treat as law

1. **Selector specificity:** for a destination and envelope, `Item > Repo > Org`
   among *configured* selectors, chosen **before** enabled/filter evaluation.
   `Github_route_match.specificity_order` is the runtime authority: both
   `Github_route_match.specificity_rank` and `Github_route_match.resolve`
   consume it. Upgrade validation derives its drift value from that matcher
   value rather than maintaining a validator-local precedence literal.
2. **Fail-closed mute (no-fallthrough):** if the most-specific route is disabled
   or filter-rejected, the event is **Muted** — it does **not** fall through to
   a broader Org or Repo route.
3. **Org requires App:** Org selectors need live Active App installation scope.
   PAT cannot claim Org; apply refuses with App migration guidance.
4. **Plan is not apply:** planning and browser callback resume only store a
   confirmable plan. Mutation requires explicit confirm/apply with plan id +
   digest.
5. **Delivery ACK independent of Connector:** GitHub HTTP ACK and the delivery-id
   ledger do not wait on Connector card render or outbox completion.
6. **Secrets redaction:** PEM, client secret, webhook secret, bearer tokens, and
   similar never appear in plans, inspect summaries, Channel posts, or audit
   details. Use credential handles / public metadata only.
7. **At most one active route** per `(destination, canonical selector)`.
8. **Webhook does not wake the agent** — history and projections only until a
   human/agent-triggering interaction.

## Setup flow (App onboarding)

```
plan/manifest tx  →  browser install/callback  →  verify exchange (one-time)
       →  resume in originating Room (or notification) with readiness + Setup_plan
       →  admin confirms  →  apply (atomic)  →  managed bundle attach + catalog refresh
```

### Steps

1. **Start setup transaction** (resumable, expiring, bound to Room/session
   context). Captures intended permissions, events, and webhook reachability
   expectations.
2. **Browser install** via GitHub App manifest / install URL. One-time state and
   expiry must survive restart; forgery/replay is rejected.
3. **Callback exchange** verifies the code/state, materializes App identity and
   credential handles (not plaintext secrets into the Room), and **consumes** the
   transaction. The **callback is not apply** confirmation.
4. **Resume** (`Github_app_setup_resume`): build a redacted confirmable
   `Setup_plan` (kind `Github_app_setup`) for the active Room when bound and
   active; otherwise a notification target with room/session keys. Surface
   readiness: app identity, live installation scope, permissions, webhook,
   Connector.
5. **Confirm/apply** only with matching plan id + digest after authority and
   readiness recheck. Stale base revision or expiry → regenerate plan, do not
   apply.
6. **Post-apply:** installation scope is reconciled live; newly granted repos on
   all-repo installs join Org routes without config edits; selected-repo removals
   and suspension fail closed.

### PAT path

- Exact-Repo compatibility only (legacy grants / subscriptions).
- Cannot create or apply Org routes.
- Prefer App migration when operators need org-wide forwarding or automatic
  new-repo pickup.

## Route admin surfaces

Full-build CLI command handlers are live. Mutating route/App commands require
an authenticated current actor supplied by a trusted adapter and are rechecked
through `Setup_plan_consent`. `CLAWQ_ADMIN` and `CLAWQ_PRINCIPAL_ID` are not
authority evidence for this surface. **The current production command bridge
does not yet supply that actor, so raw CLI mutations fail closed.** A Room,
connector, or enrolled-CLI adapter must be wired before mutation commands are
an operator-supported production path. Read-only inspection, preview,
diagnostics, export, and upgrade validation are redacted and do not require an
actor.

```sh
# Invoke mutating route/App handlers through an authenticated Room/agent
# surface; raw environment claims are intentionally rejected.
clawq github app deliveries --room ROOM
clawq github diagnostics audit --plan PLAN_ID
clawq github route preview ROOM --envelope-json '{"version":1,"event":"pull_request",...}'
clawq github route diagnostics --room ROOM --json
clawq github route export --room ROOM
clawq github route validate --room ROOM --json
```

Session-bound App setup uses `github app apply … --session SESSION_KEY` and
requires global-admin authority. `clawq-min github …` always reports the
integration boundary instead of applying a partial route/App change.

Implemented domain modules behind those commands:

| Intent | Planning API | Notes |
|--------|--------------|-------|
| Create route | `Github_route_admin.plan_create` | Pending `Setup_plan` only |
| Change filter/mode/policy | `plan_update` | OCC via `expected_revision` |
| Disable | `plan_disable` | `enabled=false`, row retained |
| Remove (soft) | `plan_remove` | Soft disable; frees active slot |
| Inspect | `inspect` / `list_inspect_for_destination` | Channel-safe summary + explain |
| Apply | `Github_route_apply.apply_confirmed` | Digest, authority, Org App gate |
| Readiness | `Github_route_ops.assess_readiness` | Fail/Warn/Pass + repair strings |
| Match explain | `Github_route_ops.explain_match` | Matched / Muted / No_route |
| Audit | `Github_route_ops.record_audit` / `list_audit` | Durable; always `redact_json` on details |
| Diagnostics/export | `Github_route_diagnostics.collect` via `github route diagnostics\|export` | Current local route/App/delivery/catalog evidence; catalog refs are opaque. With `--room ROOM --envelope-json JSON`, the safe normalized envelope adds non-mutating preview-equivalent winning-selector, predicate, final-reason, and enrichment evidence. |
| Upgrade validation | `Github_route_upgrade_validate.validate` via `github route validate` | Durable refresh queue scoped to the requested Room + actual documentation contract; unavailable probes Warn |

Ops payload shapes (secret-free): `create` | `update` | `disable` | `remove`.

Legacy per-PR subscription CLI remains a compatibility alias that plans/applies
**Item** routes.

### Typed advanced filters and preview

`github route plan ROOM SELECTOR --filter-json JSON` and
`github route change ROUTE_ID --filter-json JSON` accept the typed, versioned
`Github_route_filter` JSON shape. The current shape has
`"schema_version": 1`; baseline-only legacy filters may omit it, but a filter
with `pr` or `issue` predicates must declare version 1. Version 0 or a missing
version with advanced predicates is rejected.

Advanced predicates may be supplied either directly as `pr` / `issue` or in an
`advanced` wrapper containing only those two fields. The two representations
are mutually exclusive, and unknown or raw predicate-wrapper contents are
rejected. This is not a generic JSON expression language.

Use `github route preview ROOM --envelope-json JSON` to evaluate a safe,
normalized event envelope without mutating routes or the accept ledger. It
prints the selected route, predicate outcomes, and decision. Demanded
path/team enrichment that is absent, rate-limited, or out of scope fails closed
to **Muted**. PR `head_branch` predicates read persisted `pull_request.head.ref`;
`author` predicates and team-cache identity read the PR/Issue item's
`user.login`, never the webhook sender/actor.

### Authority

- Destination Room admin may confirm plans targeting that Room.
- Global admin may target another Room.
- Cross-Room apply without authority fails closed before mutation.

### Managed bundle lifecycle

- Apply of create/update with managed feature/bundle ids attaches setup-owned
  Room access linkage.
- Disable/remove of the last managed feature detaches **only** setup-owned
  linkage; independent/manual grants are preserved.
- Apply atomically records a Room catalog-refresh request. The next Room turn
  consumes it before freezing its Tool catalog **without daemon restart** — no
  audit-only placeholder.

## Match outcomes operators will see

| Decision | Meaning | Typical repair |
|----------|---------|----------------|
| **Matched** | Most-specific route enabled and filter allows | None; expect journal/card path |
| **Muted** | Most-specific route disabled or filter rejected | Enable route, relax filter, or intentionally leave muted |
| **No_route** | No Item/Repo/Org selector applies for destination | Create route or fix destination binding |

Muted is success of the mute rule, not a delivery bug. Broader Org feeds stay
silent when a narrow Repo/Item mute wins.

## Webhook ingress checklist

Shared App ingress (default path `/github/app/webhook`):

1. Path match (query stripped).
2. HMAC signature with configured webhook secret (credential boundary).
3. Non-empty `X-GitHub-Delivery`; ledger hit → **Duplicate** (safe redelivery).
4. Installation Active + repo authorized when installation-scoped and repo present.
5. Event in allow-list (`ping` always allowed).
6. Optional expected App id check.
7. Record delivery id **before** returning Accepted; **no Connector call** in this path.

If GitHub shows 2xx but Rooms lack cards, investigate **routing match**,
**journal/projection**, and **outbox/Connector** — not “missing ACK”. Delivery
ack is independent of Connector.

## Readiness and repair

`assess_readiness` aggregates checks (overall Fail if any Fail, else Warn, else
Pass). Common failures and actions:

| Check theme | Fail signal | Repair |
|-------------|-------------|--------|
| Installation | Missing or not Active | Re-run App install/setup; reconcile installation scope |
| Org auth | PAT or cannot claim org scope | Migrate to App; install on org; ensure Active scope |
| Revision | Plan base_revision ≠ current | Re-plan; discard stale plan id |
| Webhook | Unreachable / secret mismatch | Fix public URL, secret handle, signature verify |
| Tools/MCP/credentials/egress | Flag false | Restore grants, MCP allowlist, credential lease, egress policy |
| Connector | Not ready | Repair Channel connector independently of GitHub ACK |
| Delivery | Outbox/dead-letter | Repair outbox; do not re-open closed delivery ledger rows casually |

Match explain should list the **winner** route and **shadowed** broader routes so
operators understand mute.

## Secrets and redaction

Never paste into Rooms, tickets, or logs:

- PEM private keys, `client_secret`, `webhook_secret`, PATs, installation tokens
- Authorization / Bearer headers

Allowed in plans and inspect:

- Credential **handles**, App id, installation id, repo full names, route ids
- Redacted digests and readiness booleans

`Github_route_ops.redact_json` and audit paths must strip secret keys and bound
large strings. Export/repair dumps use the same redaction.

## Minimal verification script for operators

After a change:

1. **Inspect** destination routes: expected selectors, enabled flags, comment
   mode, managed linkage.
2. **Assess readiness** for App + route: overall Pass or actionable Fail.
3. **Explain match** on a sample envelope: Matched vs intentional Muted.
4. **Confirm** pending plan only when digest matches; apply once; re-inspect
   revision.
5. Send GitHub `ping` or a low-risk event: delivery ledger accepts once;
   duplicate redelivery stays Duplicate.
6. Confirm Room card/journal path separately from HTTP 2xx if needed.
7. Confirm no secrets in inspect/audit output.

## Failure-mode cheat sheet

| Symptom | Likely cause | Action |
|---------|--------------|--------|
| Org plan rejected | PAT or missing Active install | App migration; install + reconcile |
| Callback done but no routes | Expected — plan not apply | Confirm plan in Room |
| Stale plan apply rejected | Expiry or base revision drift | Regenerate plan |
| Events stop for one repo under Org | Item/Repo mute or exclude filter | Inspect most-specific route |
| New repo missing under Org | Selected-repo install without grant | Grant repo on GitHub or use all-repo install |
| Duplicate Room noise | Missing accept ledger / dual destinations | Check destination uniqueness and try_accept ledger |
| 2xx webhook, no card | Connector/outbox after independent ACK | Delivery diagnostics, not re-ACK |
| Secrets in Channel | Bug — violate redaction contract | Rotate credentials; fix export path |

## Upgrade validation and drift checks

After binary upgrades, schema migrations, or subscription cutover, run
`clawq github route validate [--room ROOM] [--json]` (and
`github route diagnostics` or `github route export`). To include a safe,
non-mutating explain in either diagnostics/export report, the grammar is
`--room ROOM --envelope-json JSON [--json]`; an envelope is rejected without a
Room. The report is redacted and covers local operational evidence; it does not
claim GitHub network reachability.

| Category | What it checks | Fail / Warn meaning |
|----------|----------------|---------------------|
| **Schema** | Each route `filter.schema_version` vs `Github_route_filter.current_schema_version` (currently **1**) | Fail if unsupported/too new; Warn if older than current (migrates on next read/write) |
| **Migration** | Legacy `github_pr_subscriptions` vs Item routes; migrate provenance | Fail if legacy rows exist with zero routes; Warn if legacy still present after partial cutover |
| **Managed** | `managed_bundle_id` and `managed_feature_id` both set or both absent | Fail on partial linkage |
| **Installation** | Org routes require live **Active** App installation + `can_claim_org_scope` | Fail when PAT-only or suspended/deleted; PAT cannot claim Org |
| **Catalog** | Actual Room-effective frozen `Tool_catalog`/access snapshot and opaque revision metadata when daemon-observable | Fail on observed tool/MCP unhealthy. A detached, denied, or unavailable snapshot emits `catalog_state_unavailable` Warn; an unscoped base registry must never create `tools_catalog=Pass` |
| **Session** | Durable next-turn refresh queue scoped to the requested Room; live active-session/no-restart state when observable | Warn on that Room's pending refresh or unavailable daemon-process state; never synthesize a no-restart Pass |
| **Drift** | Runtime behavior vs the operator-contract block below (filter schema, envelope version, default `comment_mode=summary`, modes `off`/`summary`/`threaded`, matcher-authoritative specificity `Item > Repo > Org`) | Fail when runtime and contract diverge; Warn if the contract cannot be read |
| **Alias** | Deprecated compatibility CLI aliases (`Github_route_migrate.compatibility_cli_aliases`) | Warn while aliases remain; Fail if an alias does not map to `github route *` (dual-write forbidden) |

The validator reads this actual checked-out contract block (or the path named
by `CLAWQ_GITHUB_ROUTE_CONTRACT`) and compares specificity with
`Github_route_match.specificity_order`, which the matcher consumes through
`specificity_rank`/`resolve`; it does not compare two copied validator
literals. Operators running an installed binary without the contract file see a
Drift Warn, not a synthetic Pass.

<!-- github-route-runtime-contract
filter_schema_version=1
envelope_version=1
default_comment_mode=summary
comment_modes=off,summary,threaded
specificity_order=Item > Repo > Org
-->

### Repair (upgrade validation)

Use `report.repair_guidance` lines first (derived from Fail/Warn checks). Common
actions:

1. **Schema too old:** re-save/update the route so the filter rewrites at the
   current schema version.
2. **Schema too new:** upgrade the Clawq binary, or re-plan with a supported
   filter.
3. **Unmigrated subscriptions:** run `Github_route_migrate.migrate_database`
   (default `Prefer_existing_route`); then `list_all` / diagnostics. Do **not**
   dual-write legacy rows after cutover.
4. **Partial managed linkage:** re-apply setup so bundle + feature ids are set
   together, or clear both for manual grants only.
5. **Org installation:** install/unsuspend App; reconcile Active installation
   scope; migrate off PAT for Org.
6. **Tools/MCP catalog:** restore grants/allowlist; after `list_changed`, only
   clear quarantine on successful relist. `catalog_state_unavailable` means
   this CLI did not obtain the requested Room's effective frozen
   `Tool_catalog`/access snapshot; inspect the active Room Session instead of
   treating an unscoped base registry or MCP result as healthy.
7. **Session refresh pending:** allow the next Room turn to rebuild the frozen
   Tool catalog — **do not restart** the daemon for catalog pickup alone. A
   separate CLI process reports unobservable live-session state as Warn.
8. **Deprecated aliases:** point automation at `github route item|repo|org *`;
   aliases remain compatibility read-through only.

### Rollback (failed upgrade / cutover)

`report.rollback_guidance` encodes the standard order:

1. Stop new route applies until validation is acceptable.
2. **Disable** newly applied routes (`plan_disable` → confirm/apply) → intentional
   **Muted** (fail-closed, no-fallthrough).
3. **Soft-remove** routes that should free active selector slots (`plan_remove`).
4. Detach **only** setup-owned managed linkage when removing the last managed
   feature; preserve independent/manual grants.
5. Supersede mistaken migrate winners via store disable/update; re-run migrate
   with `Prefer_legacy` only when that is the explicit policy.
6. Confirm the next Room turn drops tools that depended on detached setup-owned
   access (no restart).
7. **Do not** delete webhook delivery-ledger accepts (ACK independence / dedupe).
8. Re-run `clawq github route validate` plus `github route
   export` before re-enabling automation.

See also pilot cleanup order in
[pilots/p19-rollout-backout-guide.md](pilots/p19-rollout-backout-guide.md).

### Deprecated aliases

Legacy per-PR subscription CLI names remain **compatibility aliases** over Item
routes (no dual-write to `github_pr_subscriptions`):

| Deprecated alias | Canonical |
|------------------|-----------|
| `subscriptions add` / `pr-subscribe` | `github route item add` |
| `subscriptions list` / `show` | `github route item list` / `show` |
| `subscriptions remove` / `pr-unsubscribe` | `github route item remove` |
| `subscriptions enable` / `disable` | `github route item enable` / `disable` |

Prefer the canonical `github route *` surfaces in new automation. Alias presence
is a **Warn** in upgrade validation until operators retire legacy scripts.

## Related implementation modules

Shared setup boundary (room-agent + GitHub adapters on one stack):
[setup-framework-boundary.md](setup-framework-boundary.md).

- `src/github_route_store.ml` — durable routes
- `src/github_route_match.ml` — specificity / mute / accept ledger
- `src/github_route_admin.ml` — plan/inspect/ops
- `src/github_route_apply.ml` — confirm apply + managed access
- `src/github_route_ops.ml` — readiness, explain, redact, audit
- `src/github_route_diagnostics.ml` — route/filter setup diagnostics and redacted export
- `src/github_route_upgrade_validate.ml` — upgrade validation, drift checks, repair/rollback guidance
- `src/github_route_migrate.ml` — legacy subscription → Item route cutover + compatibility aliases
- `src/github_app_setup_tx.ml` / `github_app_setup_callback.ml` /
  `github_app_setup_resume.ml` — App onboarding
- `src/github_app_webhook_ingress.ml` — verified shared ingress
- `src/github_app_installation_scope.ml` — live Org/repo scope
- `src/setup_plan.ml` / `setup_plan_apply.ml` — typed plan framework

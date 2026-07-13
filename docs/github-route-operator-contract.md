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
| Route plan / inspect / apply / disable / remove | Advanced branch/path/team filters (P20) |
| Org vs Repo auth, mute semantics | Production user-attribution for high-risk actions (P21) |
| Webhook ingress readiness, delivery ledger | Full outbox/card pilot runbook (P19.M3/M4) |
| Redacted audit and repair steps | Raw JSON predicates |

Use the **full** `clawq` binary for GitHub App webhook, setup transactions, and
networked route admin. The **minimal** build disables integration-only surfaces;
do not expect partial Org/App behavior there.

## Invariants operators must treat as law

1. **Selector specificity:** for a destination and envelope, `Item > Repo > Org`
   among *configured* selectors, chosen **before** enabled/filter evaluation.
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

Implemented domain modules (agent/CLI call these; names may be wrapped by
command bridge as features land):

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
| Audit | `Github_route_ops.audit_event` | Always `redact_json` on details |

Ops payload shapes (secret-free): `create` | `update` | `disable` | `remove`.

Legacy per-PR subscription CLI remains a compatibility alias that plans/applies
**Item** routes.

### Authority

- Destination Room admin may confirm plans targeting that Room.
- Global admin may target another Room.
- Cross-Room apply without authority fails closed before mutation.

### Managed bundle lifecycle

- Apply of create/update with managed feature/bundle ids attaches setup-owned
  Room access linkage.
- Disable/remove of the last managed feature detaches **only** setup-owned
  linkage; independent/manual grants are preserved.
- Tool catalog for the Room refreshes on the **next turn** (no restart).

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
`Github_route_upgrade_validate.validate` (and re-export diagnostics). The report
is redacted and covers:

| Category | What it checks | Fail / Warn meaning |
|----------|----------------|---------------------|
| **Schema** | Each route `filter.schema_version` vs `Github_route_filter.current_schema_version` (currently **1**) | Fail if unsupported/too new; Warn if older than current (migrates on next read/write) |
| **Migration** | Legacy `github_pr_subscriptions` vs Item routes; migrate provenance | Fail if legacy rows exist with zero routes; Warn if legacy still present after partial cutover |
| **Managed** | `managed_bundle_id` and `managed_feature_id` both set or both absent | Fail on partial linkage |
| **Installation** | Org routes require live **Active** App installation + `can_claim_org_scope` | Fail when PAT-only or suspended/deleted; PAT cannot claim Org |
| **Catalog** | Injectable tools/MCP ok flags + revision metadata when managed routes exist | Fail on tools/MCP unhealthy; Warn if managed routes lack catalog revisions |
| **Session** | Active Session catalog refresh **without daemon restart** | Fail if refresh requires restart; Warn on pending next-turn refresh rooms |
| **Drift** | Runtime constants vs documented defaults (filter schema, envelope version, default `comment_mode=summary`, modes `off`/`summary`/`threaded`, specificity `Item > Repo > Org`) | Fail when runtime and docs diverge |
| **Alias** | Deprecated compatibility CLI aliases (`Github_route_migrate.compatibility_cli_aliases`) | Warn while aliases remain; Fail if an alias does not map to `github route *` (dual-write forbidden) |

Documented constants live in the module
(`documented_filter_schema_version`, `documented_default_comment_mode`, …) and
must stay aligned with this contract.

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
   clear quarantine on successful relist.
7. **Session refresh pending:** allow the next Room turn to rebuild the frozen
   Tool catalog — **do not restart** the daemon for catalog pickup alone.
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
8. Re-run `validate` + `Github_route_diagnostics.collect` before re-enabling
   automation.

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

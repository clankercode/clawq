# P20 Setup-Framework and Advanced-Routing Regression Verification

**Task**: `P20.M2.E2.T003` — Run final setup-framework and advanced-routing
regression verification  
**Canonical plan**: [2026-07-12-github-item-room-routing.md](plans/2026-07-12-github-item-room-routing.md)  
**Related**: [setup-framework-boundary.md](setup-framework-boundary.md),
[ADR 0008](adr/0008-github-route-model-and-setup.md),
[github-route-operator-contract.md](github-route-operator-contract.md)

**Date (UTC)**: 2026-07-13T03:13:35Z  
**Worktree**: `/home/xertrov/src/clawq-worktrees/bl/P20.M2.E2.T003`  
**Branch**: `bl/P20.M2.E2.T003`  
**Tree SHA at verification**: `0b4be7b9ac7e081fb4353c6defabc402263db6dd`  
**Opam switch**: `clawq-5.1`  
**Runner prefix**: `opam exec --switch=clawq-5.1 --`

## Verdict

**PASS — no unresolved required behavior; no unowned failure modes.**

All required regression surfaces for P20 M1 (advanced structured Org/Repo
forwarding) and P20 M2 (setup-framework reuse and operational polish) passed
under `make test-run`. No product code fixes were required in this branch.

| Batch | Command outcome | Tests run | Failures |
|-------|-----------------|-----------|----------|
| Combined broad gate | `Test Successful in 5.122s. 383 tests run.` | 383 | 0 |
| Plain Connector delivery (extra) | `Test Successful in 0.608s. 6 tests run.` | 6 | 0 |
| **Grand total** | | **389** | **0** |

## Acceptance mapping

| Acceptance area | Suites | Outcome |
|-----------------|--------|---------|
| Shared adapter contracts | `setup_framework_contract` | 9/9 OK |
| Room-agent setup plan/apply | `room_agent_setup_plan`, `room_agent_setup_apply` | 7+9 OK |
| Advanced filter schema/migration | `github_route_filter` | 11/11 OK |
| Filter eval predicates + rate-limit | `github_route_filter_eval` | 19/19 OK |
| Route explain / filter preview | `github_route_filter_preview` | 7/7 OK |
| Route match (specificity, no fallthrough) | `github_route_match` | 15/15 OK |
| Advanced match | `github_route_match_advanced` | 14/14 OK |
| Fixtures, migration parity, Org-scale bench budgets | `github_route_match_fixtures` | 11/11 OK |
| Diagnostics + redacted export | `github_route_diagnostics` | 6/6 OK |
| Upgrade validation / drift / admin guidance | `github_route_upgrade_validate` | 17/17 OK |
| Operator/docs contract snapshots | `github_route_docs` | 4/4 OK |
| Migration / rollback / dual-active guard | `github_route_migrate` | 16/16 OK |
| Store, transfer, admin, apply, ops | `github_route_store`, `github_route_transfer`, `github_route_admin`, `github_route_apply`, `github_route_ops` | 12+8+9+7+12 OK |
| Config/catalog reload | `minimal_reload` | 16/16 OK |
| Access isolation / explanation | `access_snapshot`, `access_explanation` | 39+34 OK |
| Connector delivery | `github_delivery_*`, `teams_delivery_*`, `room_ambient_delivery`, `connector_capabilities`, `github_plain_delivery` | all OK |

## Commands run (exact)

Environment for every command below:

```bash
cd /home/xertrov/src/clawq-worktrees/bl/P20.M2.E2.T003
# prefix: opam exec --switch=clawq-5.1 --
```

### 1. Setup framework + room-agent adapters

```bash
opam exec --switch=clawq-5.1 -- make test-run ARGS='test "setup_framework_contract|room_agent_setup"'
```

**Outcome**:

```text
Test Successful in 2.347s. 25 tests run.
```

| Suite | OK |
|-------|---:|
| `setup_framework_contract` | 9 |
| `room_agent_setup_plan` | 7 |
| `room_agent_setup_apply` | 9 |
| **Subtotal** | **25** |

Contract cases (shared reject/idempotent semantics, room-agent + GitHub route):

- `room_agent digest mismatch`
- `github_route digest mismatch`
- `room_agent stale base revision`
- `github_route stale base revision`
- `room_agent authority denied`
- `github_route authority denied`
- `room_agent idempotent re-apply`
- `github_route idempotent re-apply`
- `paired reject reason strings match`

### 2. Advanced routing (`github_route*`)

```bash
opam exec --switch=clawq-5.1 -- make test-run ARGS='test "github_route"'
```

**Outcome**:

```text
Test Successful in 2.835s. 168 tests run.
```

| Suite | OK | Notes |
|-------|---:|-------|
| `github_route_filter` | 11 | v0→v1 migrate, advanced fields, reject raw JSON predicates |
| `github_route_filter_eval` | 19 | PR/Issue predicates, rate-limit / missing enrichment fail-closed |
| `github_route_filter_preview` | 7 | explain/preview, muted advanced no fallthrough, enrichment status |
| `github_route_store` | 12 | persistence / uniqueness |
| `github_route_match` | 15 | Item > Repo > Org; disabled/filtered no Org fallthrough |
| `github_route_match_advanced` | 14 | advanced selector + filter composition |
| `github_route_match_fixtures` | 11 | migration fixtures, rate-limit gates, Org-scale budgets/bench |
| `github_route_transfer` | 8 | dual-match at most one accept per destination |
| `github_route_migrate` | 16 | legacy sub → route; idempotent re-run; no dual active |
| `github_route_admin` | 9 | plan/inspect/disable/remove; secret-free plans |
| `github_route_apply` | 7 | digest/authority; PAT Org reject; managed bundle; catalog refresh |
| `github_route_ops` | 12 | operational route helpers |
| `github_route_diagnostics` | 6 | redacted export; no secrets/webhook bodies; repair hints |
| `github_route_upgrade_validate` | 17 | schema/drift/managed/org-install/legacy/admin guidance |
| `github_route_docs` | 4 | ADR 0008, operator contract, glossary, plan cross-links |
| **Subtotal** | **168** | |

`github_route_match_fixtures` coverage of P20.M1.E2.T003-style gates:

- migration parity matched + JSON v0 path
- PR/Issue predicate combination fixtures
- full AND composition
- rate-limit gate fail-closed + never accepts under rate-limit alone
- access denied fail-closed
- org-scale candidate budget
- org-scale enrichment budget cold and warm
- org-scale match cost budget documented (uses `Github_route_match_bench`)

### 3. Delivery, access isolation, reload

```bash
opam exec --switch=clawq-5.1 -- make test-run ARGS='test "github_delivery|teams_delivery|room_ambient_delivery|access_snapshot|access_explanation|minimal_reload|connector_capabilities"'
```

**Outcome**:

```text
Test Successful in 4.080s. 190 tests run.
```

| Suite | OK | Notes |
|-------|---:|-------|
| `github_delivery_intent` | 5 | delivery intent construction |
| `github_delivery_outbox` | 7 | enqueue/claim/retry/dead-letter; no secrets in errors |
| `github_delivery_reconcile` | 5 | pending collapse; one catchup per item |
| `github_delivery_ops` | 6 | metrics, diagnose, repair, restart reorder |
| `teams_delivery_lifecycle` | 15 | Teams card lifecycle |
| `teams_delivery_regression` | 17 | Teams regression matrix |
| `room_ambient_delivery` | 17 | ambient Room delivery policy path |
| `connector_capabilities` | 29 | Connector capability surface |
| `access_snapshot` | 39 | access isolation snapshots |
| `access_explanation` | 34 | human-readable access denial/allow explanation |
| `minimal_reload` | 16 | config/profile/scope bundle reload without restart |
| **Subtotal** | **190** | |

### 4. Combined broad gate (primary evidence line)

```bash
opam exec --switch=clawq-5.1 -- make test-run ARGS='test "setup_framework_contract|room_agent_setup|github_route|github_delivery|teams_delivery|room_ambient_delivery|access_snapshot|access_explanation|minimal_reload|connector_capabilities"'
```

**Outcome**:

```text
Test Successful in 5.122s. 383 tests run.
```

29 suites, 383 OK, 0 FAIL, 0 ERROR. Suite-level counts match the sum of
batches 1–3 (no flaky divergence on re-run).

### 5. Plain Connector delivery render (adjacent delivery surface)

Not matched by the combined regex above (suite name `github_plain_delivery`).

```bash
opam exec --switch=clawq-5.1 -- make test-run ARGS='test github_plain_delivery'
```

**Outcome**:

```text
Test Successful in 0.608s. 6 tests run.
```

| Suite | OK |
|-------|---:|
| `github_plain_delivery` | 6 |

## Per-suite inventory (30 suites)

| # | Suite | OK | Area |
|--:|-------|---:|------|
| 1 | `setup_framework_contract` | 9 | Shared plan-confirm-apply contracts |
| 2 | `room_agent_setup_plan` | 7 | Room-agent plan adapter |
| 3 | `room_agent_setup_apply` | 9 | Room-agent apply/repair adapter |
| 4 | `github_route_filter` | 11 | Advanced filter fields + migration |
| 5 | `github_route_filter_eval` | 19 | Predicate evaluation |
| 6 | `github_route_filter_preview` | 7 | Preview / explain |
| 7 | `github_route_store` | 12 | Route store |
| 8 | `github_route_match` | 15 | Specificity matching |
| 9 | `github_route_match_advanced` | 14 | Advanced matching |
| 10 | `github_route_match_fixtures` | 11 | Fixtures + Org-scale bench budgets |
| 11 | `github_route_transfer` | 8 | Transfer dual-match dedupe |
| 12 | `github_route_migrate` | 16 | Legacy migration / rollback safety |
| 13 | `github_route_admin` | 9 | Plan/inspect/disable/remove |
| 14 | `github_route_apply` | 7 | Confirm/apply gates |
| 15 | `github_route_ops` | 12 | Ops helpers |
| 16 | `github_route_diagnostics` | 6 | Diagnostics + redacted export |
| 17 | `github_route_upgrade_validate` | 17 | Upgrade/drift validation |
| 18 | `github_route_docs` | 4 | Doc contract snapshots |
| 19 | `github_delivery_intent` | 5 | Delivery |
| 20 | `github_delivery_outbox` | 7 | Delivery |
| 21 | `github_delivery_reconcile` | 5 | Delivery |
| 22 | `github_delivery_ops` | 6 | Delivery |
| 23 | `github_plain_delivery` | 6 | Plain Connector render |
| 24 | `teams_delivery_lifecycle` | 15 | Teams delivery |
| 25 | `teams_delivery_regression` | 17 | Teams delivery |
| 26 | `room_ambient_delivery` | 17 | Ambient delivery |
| 27 | `connector_capabilities` | 29 | Connector capabilities |
| 28 | `access_snapshot` | 39 | Access isolation |
| 29 | `access_explanation` | 34 | Access isolation |
| 30 | `minimal_reload` | 16 | Reload / no-restart catalog path |
| | **Total** | **389** | |

## Failure modes checked (owned → suite)

| Failure mode | Owner / contract | Covered by |
|--------------|------------------|------------|
| Digest mismatch on confirm | `Setup_plan_apply` | `setup_framework_contract`, `github_route_apply` |
| Stale base revision | `Setup_plan_apply` | `setup_framework_contract` |
| Authority denied (non-admin / cross-room) | `Setup_plan_consent` | `setup_framework_contract`, `github_route_apply` |
| Idempotent re-apply (same receipt, no double mutate) | `Setup_plan_apply` | `setup_framework_contract`, `github_route_admin` |
| PAT + Org scope rejected | GitHub route adapter | `github_route_apply` |
| Disabled/filtered more-specific route no Org fallthrough | Route match | `github_route_match`, `github_route_filter_preview` |
| Advanced predicate reject / mute | Filter eval | `github_route_filter_eval`, `github_route_match_fixtures` |
| Rate-limited or missing path enrichment fail-closed | Filter eval | `github_route_filter_eval`, `github_route_match_fixtures` |
| Access denied on enrichment fail-closed | Match fixtures | `github_route_match_fixtures` |
| Dual transfer match → one accept | Transfer | `github_route_transfer` |
| Legacy migrate idempotent; no dual active | Migrate | `github_route_migrate` |
| Schema too new / drift / managed partial / legacy unmigrated | Upgrade validate | `github_route_upgrade_validate` |
| Diagnostics redaction (no secrets/webhook bodies) | Diagnostics | `github_route_diagnostics` |
| Outbox dead-letter / restart reorder / reconcile one-per-item | Delivery ops | `github_delivery_outbox`, `github_delivery_ops`, `github_delivery_reconcile` |
| Catalog/profile reload without daemon restart | Reload | `minimal_reload` |
| Access snapshot isolation | Access | `access_snapshot`, `access_explanation` |

No failure mode in the required set was observed as failing in this run.
No suite was skipped for missing binaries among the selected suites (all
selected cases reported `[OK]`).

## Out of scope / not claimed by this gate

- Live GitHub network or authenticated App install browser flows
- Runner-binary integration (`make test-run ARGS="test runner_integration"`)
- Full `make test` / `make test-all` repository gate (this task is the P20
  setup-framework + advanced-routing focused regression)
- P21 user-attribution / User_required high-risk action enablement

## How to re-run

Primary one-liner (383 tests):

```bash
cd /home/xertrov/src/clawq-worktrees/bl/P20.M2.E2.T003
opam exec --switch=clawq-5.1 -- make test-run ARGS='test "setup_framework_contract|room_agent_setup|github_route|github_delivery|teams_delivery|room_ambient_delivery|access_snapshot|access_explanation|minimal_reload|connector_capabilities"'
```

Include plain delivery:

```bash
opam exec --switch=clawq-5.1 -- make test-run ARGS='test "setup_framework_contract|room_agent_setup|github_route|github_delivery|github_plain_delivery|teams_delivery|room_ambient_delivery|access_snapshot|access_explanation|minimal_reload|connector_capabilities"'
```

## Product code changes

None required. Verification-only deliverable for `P20.M2.E2.T003`.

## Sign-off

| Item | Status |
|------|--------|
| Shared adapter contracts | PASS |
| Advanced filter fixtures/benchmarks | PASS |
| Route explain / preview | PASS |
| Migration / rollback safety | PASS |
| Reload | PASS |
| Access isolation | PASS |
| Connector delivery regressions | PASS |
| Unresolved required behavior | None |
| Unowned failure modes | None |

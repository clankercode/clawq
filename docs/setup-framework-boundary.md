# Setup Framework Boundary

**Status**: Authoritative for setup adapters  
**Task**: P20.M2.E1.T003  
**ADRs**: [0003](adr/0003-require-plan-confirm-apply-for-agent-setup.md),
[0008](adr/0008-github-route-model-and-setup.md)

## Single boundary

All agent-assisted and admin setup that mutates durable config, routes, or
setup-owned access uses **one** typed plan-confirm-apply stack:

| Layer | Module(s) | Owns |
|-------|-----------|------|
| Plan type | `Setup_plan` | Principal, contexts, redacted diff, digest, base revision, expiry, apply payload kind |
| Apply engine | `Setup_plan_apply` | Identity recheck, stale/expiry/destination/authority gates, atomic CAS, receipts, audit, retry-idempotency |
| Consent | `Setup_plan_consent` | Global-admin / room-admin / cross-room consent authority checks |
| Managed access | `Setup_plan_bundle` | Setup-owned bundle linkage attach/detach with feature provenance |

Domain adapters **plan** and supply **domain mutation** only. They must not
implement a second confirmation or apply state machine.

## Current adapters

| Adapter | Plan | Confirm/apply | Domain mutation |
|---------|------|---------------|-----------------|
| Room-agent pilot | `Room_agent_setup_plan` | `Room_agent_setup_apply` | Optional compensatable `config_apply` → wizard `apply_plan` + managed bundles |
| GitHub routes | `Github_route_admin` plan_* | `Github_route_apply` | `Github_route_admin.apply_route_ops` + managed bundles |
| GitHub App setup | setup tx / resume | `Setup_plan_apply` via resume path | App/installation materialization (callback ≠ apply) |

Contract coverage for shared reject/idempotent semantics lives in
`test/test_setup_framework_contract.ml` (room-agent + GitHub route).

## Inside the boundary (must go through the stack)

- Create/update room profiles and room bindings via setup apply
- GitHub Item/Repo/Org route create/update/disable/remove
- GitHub App setup confirmation after browser callback resume
- Attaching or detaching **setup-owned** access bundles/features

## Outside the boundary (not parallel setup semantics)

- Secret-bearing Connector onboarding (Teams/Slack credential wizards)
- Interactive TUI field prompts and validation helpers
- Readiness probes, rerun *reports*, and human-readable plan *summaries*
- Direct config CLI (`config set`) and non-setup admin tools
- Runtime GitHub collaboration action previews (separate action framework)

## Wizard CLI compatibility

`clawq rooms wizard` keeps the same subcommands as aliases over the shared stack:

| CLI | Behavior |
|-----|----------|
| `plan` | Read-only. Surfaces shared `Setup_plan` summary (canonical). Legacy `generate_plan` item list is display-only compatibility output, not a second plan type. |
| `apply` | Store typed plan → `Room_agent_setup_apply.apply_confirmed` with CLI principal as global-admin and config mutation via `apply_plan`. |
| `rerun` / `rerun --apply` | Report remains display; `--apply` uses the same plan-confirm-apply path as `apply`. |
| interactive confirm | Same store + `apply_confirmed` path (no direct config write without a stored plan). |

Do not reintroduce a second pending-plan store, digest scheme, or apply
receipt path for room-agent or GitHub route setup.

## Shared failure contract

Adapters must surface the same `Setup_plan_apply` reason strings for core gates:

| Case | Reason |
|------|--------|
| Confirm digest ≠ plan digest | `digest_mismatch` |
| Plan `base_revision` ≠ current | `stale_revision` |
| Principal lacks destination authority | `authority_denied` |
| Retry with same plan id + digest after success | `Applied` with same receipt (`applied_idempotent` audit; domain ops not re-run) |

Adapter-specific gates (e.g. GitHub `org_requires_app`) may add reasons but must
not rename the core ones above.

When a domain mutation touches state outside the shared SQLite transaction, its
adapter must return a compensating rollback. The room-agent wizard snapshots
the config before its mutation and restores it if the shared CAS/receipt/audit
transaction later rejects; it also refreshes the config revision immediately
before confirm/apply.

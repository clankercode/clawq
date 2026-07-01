# Product Goals

## Goal

Build room-agent profiles for Clawq so a chat room can have a persistent agent identity, workspace, memory scope, policy, and scheduled/ambient behavior while staying compatible with the existing OCaml runtime, connector, scheduler, task, and memory architecture.

## Success Criteria

- A room can be bound to exactly one active profile in V1.
- Profiled rooms use a shared room session, with child thread and routine sessions for bounded work.
- Profiled rooms get persistent CWDs under `~/.clawq/workspace/rooms/<slug>/` with containment and lifecycle rules.
- Slack ships first as the complete E2E connector; Teams gets early thread capability audit and later hardening.
- P11 blocks unscoped global memory/search injection for profiled rooms until scoped memory tables land in P12.
- Scoped memory is represented with first-class tables and APIs, not namespaced legacy keys.
- Room routines extend the existing scheduler/cron system; there is no parallel scheduler.
- Room-agent async work depends on existing durable background-task/restart replay work from P7.
- Operators have admin-gated surfaces for profile binding, grants, budgets, routines, and inspection.
- Ambient behavior is opt-in, rate-limited, quiet-hours aware, retention-bounded, and scoped to room grants.

## Non-Goals

- General multi-tenant organization model in V1.
- Full connector parity in P11.
- A new scheduler or cron engine.
- Raw, unscoped transcript/ledger exposure.
- Repo/org scoped memory kinds before there is an owned implementation reason.

## Verification / Acceptance

The backlog implementation should preserve the task-level gates called out in `docs/plans/2026-06-26-room-agent-profiles.md`:

- `bl check --strict` passes.
- `bl why` proves P11 async behavior and P13 routine/ambient behavior wait on P7 background-task reliability gates.
- Cross-room memory negative tests exist before scoped memory is considered complete.
- Shared-room key parsing tests cover multi-segment connector keys and restart/completion routing.
- Scheduler tests prove existing cron behavior remains compatible.

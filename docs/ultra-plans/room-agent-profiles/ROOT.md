# Room Agent Profiles Ultra Plan

Status: audit tree bootstrapped from existing seed plan and ingested backlog.

Seed plan: `docs/plans/2026-06-26-room-agent-profiles.md`

Review: `docs/plans/2026-06-26-room-agent-profiles.review.md`

Backlog phases: P11, P12, P13.

## Tree

| Node | Responsibility | Backlog Coverage |
|---|---|---|
| 01-profile-foundation | Profile schema, binding, workspace, CLI/admin lifecycle | P11.M1 |
| 02-session-routing-threading | Shared room sessions, typed keys, thread/routine child sessions, Slack/Teams thread metadata | P11.M2, P11.M3 |
| 03-task-delivery | Mention-to-task classification, background binding, progress/completion, restart/concurrency | P11.M4 |
| 04-scoped-memory | Scoped memory schema, grants, legacy read-in-place, scoped prompt/search | P12.M1 |
| 05-policy-budget-ledger | Tool/codebase policy, authz, budgets, activity ledger | P12.M2, P12.M3 |
| 06-scheduler-ambient | Cron-integrated routines and ambient watcher | P13.M1, P13.M2 |
| 07-connector-polish-docs | Connector capability matrix, Teams hardening, docs | P13.M3 |

## Review Dashboard

| Date | Review | Result |
|---|---|---|
| 2026-06-26 | Initial `bl` consistency | PASS, `bl check --strict` |
| 2026-06-26 | PIRFL subagent review | PASS, no blockers |
| 2026-06-26 | Ultra granularity audit | MAJOR fixed: split coarse tasks; 59 total tasks; `bl check --strict` PASS |
| 2026-06-26 | Phase-specific subagent reviews | NOT PASS findings fixed: P11 boundary, P12 budget/authz/schema, P13 safety/history/docs; 67 total tasks |

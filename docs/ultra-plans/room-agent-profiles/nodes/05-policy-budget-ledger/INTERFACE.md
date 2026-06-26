# 05 Policy Budget Ledger INTERFACE

## Exposes

- `profile_policy`
- Tool/codebase grant checks.
- `profile_budget_state`
- Budget pre-call guard.
- `room_activity_ledger_entry`

## Depends On

- 01-profile-foundation for profile identity.
- 02-session-routing-threading for session/profile context.
- 03-task-delivery for origin/task status.
- 04-scoped-memory for memory grants.
- Existing global security config and provider-call accounting.

## Consumers

- 02-session-routing-threading consumes effective policy.
- 03-task-delivery consumes budget/policy gates for async work.
- 06-scheduler-ambient consumes policy, budget, and ledger behavior.
- 07-connector-polish-docs documents ledger/admin behavior.

## Contract Checks

- Profile grants never weaken global security.
- Guests cannot mutate policy, grants, budgets, routines, CWD, or ledger settings.
- Budget failures do not include prompt content.

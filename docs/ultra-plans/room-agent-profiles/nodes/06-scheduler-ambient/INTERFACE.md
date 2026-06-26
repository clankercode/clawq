# 06 Scheduler Ambient INTERFACE

## Exposes

- `routine_target`
- Room routine execution path.
- Scoped connector history capture.
- Stale work watcher decision records.
- Ambient safety controls.

## Depends On

- Existing scheduler/cron system.
- 02-session-routing-threading for routine sessions.
- 03-task-delivery for async execution/progress.
- 04-scoped-memory for history/memory scope.
- 05-policy-budget-ledger for policy, budget, and ledger behavior.
- 07-connector-polish-docs for capability matrix.

## Consumers

- 07-connector-polish-docs documents routine/ambient operation.

## Contract Checks

- Existing cron jobs remain unchanged.
- Routine profile resolution happens at tick time.
- Ambient watcher defaults off and respects retention, rate limits, quiet hours, and grants.

# 03 Task Delivery INTERFACE

## Exposes

- Task/background `room_origin` storage.
- Request classification result.
- Room async launch path.
- Sparse progress state model.
- Restart replay verification contract.

## Depends On

- 02-session-routing-threading for origin and child session creation.
- P7 durable background queue/restart work.
- 07-connector-polish-docs for connector capability matrix where delivery behavior differs.

## Consumers

- 05-policy-budget-ledger consumes origin and task status for activity ledger.
- 06-scheduler-ambient consumes async launch and status delivery for routine/watcher work.

## Contract Checks

- Async launch is gated on P7 reliability tasks.
- Progress posts do not leak raw logs.
- Restart replay completes exactly once or records a durable failure.

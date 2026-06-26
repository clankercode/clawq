# 02 Session Routing And Threading INTERFACE

## Exposes

- Shared room session resolution.
- `room_origin`
- `connector_thread_ref`
- Child thread/routine session creation.
- P11 memory injection guard for profiled rooms.

## Depends On

- 01-profile-foundation for bindings, workspace paths, and typed room session keys.
- Existing connector handlers for Slack and Teams.
- Existing agent/session turn execution path.

## Consumers

- 03-task-delivery consumes origin metadata and child session creation.
- 04-scoped-memory consumes session/profile context for scoped prompt injection.
- 05-policy-budget-ledger consumes session/profile attribution.
- 06-scheduler-ambient consumes routine session creation.
- 07-connector-polish-docs consumes Teams audit results and connector thread support.

## Contract Checks

- Unprofiled Slack behavior remains unchanged.
- Shared room sessions do not inject global memory before P12 scoped memory.
- Child sessions inherit policy and workspace constraints, but do not silently grant new privileges.

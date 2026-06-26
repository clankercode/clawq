# 04 Scoped Memory INTERFACE

## Exposes

- `memory_scope_id`
- `memory_grant`
- `store_scoped`
- `recall_scoped`
- `list_scoped`
- `forget_scoped`
- Scoped `Memory.search`

## Depends On

- 01-profile-foundation for profile identity.
- Existing memory schema versioning and repair paths.
- Existing agent prompt injection and compaction paths.

## Consumers

- 02-session-routing-threading consumes scoped prompt/search behavior.
- 05-policy-budget-ledger consumes grant policy.
- 06-scheduler-ambient consumes scoped history/memory rules.

## Contract Checks

- Channel A/B negative tests prove no cross-room leakage.
- Legacy memory is read deliberately and does not double-copy.
- Prompt injection and compaction cannot bypass scoped APIs.

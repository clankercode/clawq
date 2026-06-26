# 01 Profile Foundation INTERFACE

## Exposes

- `room_profile`
- `room_profile_binding`
- `room_workspace_path`
- `room_session_key` constructor/parser API
- Admin lifecycle command surface

## Depends On

- Existing runtime config loading.
- Existing SQLite memory schema/migration mechanics.
- Existing command bridge/minimal-build split.

## Consumers

- 02-session-routing-threading consumes bindings, workspaces, and keys.
- 03-task-delivery consumes workspace/key/origin context.
- 04-scoped-memory consumes profile identity for scope creation.
- 06-scheduler-ambient consumes profile identity and routine workspace paths.

## Contract Checks

- Duplicate room bindings fail closed.
- Workspace path realpaths remain contained.
- Positional parser consumers migrate to typed parsing or are proven safe.

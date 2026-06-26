# 01 Profile Foundation SPEC

## Responsibilities

- Define the one-room-one-profile data model.
- Reconcile config and DB room profile bindings without allowing duplicate active profiles per room.
- Generate safe persistent workspace paths under the room workspace root.
- Provide typed construction and parsing entrypoints for room session keys.
- Add admin-only profile lifecycle commands and safe workspace retention behavior.

## Non-Responsibilities

- Connector-specific event handling.
- Scoped memory APIs.
- Scheduler execution.
- Ambient watcher decisions.

## Backlog Mapping

- P11.M1.E1.T001: Add room profile config types and validation.
- P11.M1.E1.T002: Add room profile binding DB schema and API.
- P11.M1.E1.T003: Reconcile room profile config and DB bindings.
- P11.M1.E2.T001: Implement safe persistent room workspace paths.
- P11.M1.E3.T001: Add typed room session key construction and parsing.
- P11.M1.E4.T001: Add admin rooms list show bind and unbind CLI.
- P11.M1.E4.T002: Add room workspace lifecycle and retention policy.
- P11.M1.E4.T003: Add room profile rebind rename and delete lifecycle.
- P11.M1.E4.T004: Add minimal build stubs and admin authz tests for rooms CLI.

## Granularity Note

Granularity audit split `P11.M1.E1` into config validation, DB schema/API, and reconciliation tests. The node is now reasonably decomposed for implementation.

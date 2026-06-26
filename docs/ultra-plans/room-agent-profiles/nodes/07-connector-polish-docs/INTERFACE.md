# 07 Connector Polish Docs INTERFACE

## Exposes

- `connector_capability_matrix`
- Teams room-agent hardening results.
- Operator documentation.

## Depends On

- 02-session-routing-threading for Teams audit findings.
- Existing connector capability module.
- 03-task-delivery and 06-scheduler-ambient for consumer behavior.

## Consumers

- 03-task-delivery consumes delivery capabilities.
- 06-scheduler-ambient consumes history and delivery capabilities.
- Operators consume docs.

## Contract Checks

- Capability matrix drives behavior; connector-specific delivery paths do not grow ad hoc conditionals without declared capability.
- Docs reflect minimal-build behavior.

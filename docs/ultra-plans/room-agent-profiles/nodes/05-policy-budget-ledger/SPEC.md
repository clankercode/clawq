# 05 Policy Budget Ledger SPEC

## Responsibilities

- Enforce room profile tool grants under existing global security constraints.
- Enforce codebase grants as an intersection with sandbox/workspace policy.
- Add profile policy authz and denial tests.
- Attribute request stats to room profiles.
- Enforce monthly room profile budgets per provider call.
- Add redacted room activity ledger.

## Non-Responsibilities

- Memory table storage.
- Connector thread support.
- Scheduler metadata.

## Backlog Mapping

- P12.M2.E1.T001: Enforce room profile tool grants.
- P12.M2.E2.T001: Enforce codebase grants under global security.
- P12.M2.E3.T001: Add profile policy authorization and denial tests.
- P12.M3.E1.T001: Attribute request stats to room profiles.
- P12.M3.E2.T001: Add room budget state and query API.
- P12.M3.E2.T002: Add provider pre-call budget guards to agent loops.
- P12.M3.E2.T003: Implement budget reservation and concurrency semantics.
- P12.M3.E2.T004: Add room budget soft warnings and admin notification.
- P12.M3.E2.T005: Add budget denial redaction and negative tests.
- P12.M3.E3.T001: Add room activity ledger schema and append API.
- P12.M3.E3.T002: Instrument room task background and provider events into ledger.
- P12.M3.E3.T003: Add room ledger admin filters export and retention.

## Granularity Note

Phase review split `P12.M3.E2` into budget state/query, provider guards, reservation/concurrency, soft warnings, and prompt-redaction negative tests. Ledger admin/export now explicitly depends on the P12 authz matrix.

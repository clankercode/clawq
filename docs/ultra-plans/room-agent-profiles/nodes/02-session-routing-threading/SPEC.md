# 02 Session Routing And Threading SPEC

## Responsibilities

- Resolve Slack profiled rooms to shared room sessions.
- Apply profile CWD/model/template precedence while preserving global security constraints.
- Disable unscoped global memory/search injection in P11 profiled rooms.
- Preserve room/thread origin metadata.
- Add Slack thread metadata and reply support.
- Audit Teams thread-like capability.
- Create child thread/routine work sessions that inherit the P11 policy subset and workspace constraints.

## Non-Responsibilities

- Background task queue reliability.
- Scoped memory table implementation.
- Budget enforcement.

## Backlog Mapping

- P11.M2.E1.T001: Resolve Slack profiled rooms to shared room sessions.
- P11.M2.E2.T001: Apply room CWD precedence and repo policy.
- P11.M2.E2.T002: Apply room profile model template and OAuth precedence.
- P11.M2.E3.T001: Disable unscoped global memory/search in profiled rooms.
- P11.M3.E1.T001: Add room origin metadata type and serialization.
- P11.M3.E2.T001: Add Slack thread metadata and reply support.
- P11.M3.E3.T001: Audit Teams thread-like reply capability.
- P11.M3.E4.T001: Create deterministic child thread sessions.
- P11.M3.E4.T002: Add routine session key and workspace plumbing.
- P11.M3.E4.T003: Add deterministic thread-less child session fallback.

## P11/P12 Boundary

P11 child sessions inherit only the P11 subset: room CWD, model/template precedence, admin/guest async context, and the privacy guard. Full tool/codebase/budget/scoped-memory policy is owned by P12.

## Granularity Note

Granularity audit split `P11.M3.E4` into child thread sessions, routine session/workspace plumbing, and thread-less fallback. The node is now reasonably decomposed.

# Decisions

## ADR-001: One Room, One Active Profile In V1

Status: accepted

Decision: A connector room binding resolves to exactly one active room profile. Multi-profile arbitration and profile stacking are out of V1.

Rationale: The user explicitly selected one-room-one-profile. It keeps shared session, memory, CWD, and budget attribution tractable.

Alternatives: hybrid multi-profile routing, per-user room overlays, or tag-based profile selection. These add policy and attribution ambiguity.

## ADR-002: Shared Room Session With Child Thread/Routine Sessions

Status: accepted

Decision: Profiled rooms use one shared room session for room continuity. Thread-bound work and scheduled routines create child sessions that inherit profile policy and workspace constraints.

Rationale: Shared rooms match the intended mental model; child sessions preserve bounded work state and delivery identity.

Tradeoff: Shared history requires explicit trust/authorization boundaries and a P11 privacy guard.

## ADR-003: Scoped Memory Tables

Status: accepted

Decision: P12 introduces `memory_scopes`, `scoped_memories`, and `memory_grants` rather than encoding scope in string keys.

Rationale: Scoped tables are more principled for grants, migrations, prompt injection, and auditing.

Alternatives: hybrid namespaced keys or separate per-room stores. Namespaced keys underfit grants and migration; per-room stores complicate cross-scope admin flows.

## ADR-004: Slack First, Teams Audit Early

Status: accepted

Decision: Slack is the first E2E MVP connector. Teams gets a thread-like reply capability audit in P11 and hardening in P13.

Rationale: Slack exercises the real transition from user-scoped to shared-room sessions and new `thread_ts` support. Teams can hide the keying change because it already has conversation-like state.

## ADR-005: Extend Existing Scheduler

Status: accepted

Decision: Scheduled room routines extend the existing cron/scheduler paths with room profile metadata.

Rationale: User explicitly noted existing scheduled tasks/cron should be integrated with/extended, not replaced.

## ADR-006: P11 Privacy Guard Before Scoped Memory

Status: accepted

Decision: P11 includes a guard that prevents profiled room turns from reading or injecting unscoped global memory/search context.

Rationale: Shared room sessions would otherwise expose one participant's global memories to other room participants before P12 scoped memory lands.

## ADR-007: Task-Level Backlog Dependencies

Status: accepted

Decision: Dependencies are encoded on leaf tasks, not only on phases or milestones.

Rationale: Review found phase-level dependencies do not reliably gate `bl` child task availability. `bl why`/`bl blockers` should explain the actual execution graph.

# 03 Task Delivery SPEC

## Responsibilities

- Bind room origin metadata to task-tree and background-task rows.
- Classify room mentions into quick replies or durable async tasks.
- Launch async work into room child sessions.
- Deliver sparse progress/completion/failure updates to the originating thread or room.
- Verify concurrency and restart replay behavior.

## Non-Responsibilities

- Low-level connector capability declaration.
- Memory scope table design.
- Budget accounting internals.

## Backlog Mapping

- P11.M4.E1.T001: Add room origin columns to task tree and background tasks.
- P11.M4.E2.T001: Add deterministic room request classifier tests.
- P11.M4.E2.T002: Create tasks from explicit async room commands.
- P11.M4.E2.T003: Launch room background work under profile policy.
- P11.M4.E2.T004: Enforce guest policy for room async requests.
- P11.M4.E3.T001: Persist sparse room progress states.
- P11.M4.E3.T002: Deliver Slack/current room progress updates.
- P11.M4.E3.T003: Deliver final room completion and failure messages.
- P11.M4.E4.T001: Verify concurrent room messages and restart replay.

## Granularity Note

Granularity audit split the two coarse execution tasks:

- `P11.M4.E2` now separates classifier tests, explicit task creation, background launch, and guest policy.
- `P11.M4.E3` now separates persisted status state, connector delivery behavior, and final completion/failure delivery.

P11 delivery uses Slack/current connector behavior. Generic thread/card/history capability matrix work is deferred to P13.

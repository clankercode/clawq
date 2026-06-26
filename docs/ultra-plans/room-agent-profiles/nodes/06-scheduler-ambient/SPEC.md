# 06 Scheduler Ambient SPEC

## Responsibilities

- Extend existing scheduler/cron metadata for room routines.
- Run cron jobs as room profile sessions resolved at tick time.
- Provide admin-only routine create/edit/remove/trigger surface.
- Capture scoped connector history for profiled rooms.
- Add opt-in stale room task/thread watcher.
- Add ambient safety controls and inspection.

## Non-Responsibilities

- A parallel scheduler.
- Unscoped passive transcript capture.
- Connector capability matrix ownership.

## Backlog Mapping

- P13.M1.E1.T001: Extend scheduler cron metadata for room routines.
- P13.M1.E2.T001: Run cron jobs as room profile sessions.
- P13.M1.E3.T001: Add room routine create list and show commands.
- P13.M1.E3.T002: Add room routine edit and remove commands.
- P13.M1.E3.T003: Add manual room routine trigger path.
- P13.M1.E3.T004: Add room routine admin and negative tests.
- P13.M2.E1.T001: Add connector-history policy and retention core.
- P13.M2.E1.T002: Capture Slack scoped room history for ambient watcher.
- P13.M2.E1.T003: Add Teams and Discord scoped-history behavior.
- P13.M2.E2.T001: Add stale room task and thread query engine.
- P13.M2.E2.T002: Persist watcher decisions and material-change checks.
- P13.M2.E2.T003: Deliver ambient follow-ups safely.
- P13.M2.E3.T001: Enforce ambient policy config and rate limits.
- P13.M2.E3.T002: Add ambient admin inspection surface.

## Granularity Note

Granularity audit split ambient watcher work:

- `P13.M2.E1` now separates policy/retention core, Slack history capture, and Teams/Discord scoped-history behavior.
- `P13.M2.E2` now separates stale query engine, watcher decision/material-change persistence, and safe follow-up delivery.
- `P13.M2.E3` now separates pre-delivery policy enforcement from admin inspection.

`P13.M1.E2` remains a single task but now depends on final scoped-memory, policy, budget, child-session, and P7 reliability gates.

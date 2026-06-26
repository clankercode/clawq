# Interview Queue

## P0

None currently blocking. Prior P0/P1 review questions were resolved in the seed plan before backlog ingestion.

## P1

- [ ] Authz surface: Should the room admin policy name concrete roles now, or leave role mapping to the existing channel auth/admin mechanism during implementation?
- [ ] Ledger inspection: What is the preferred default retention window for room activity ledger entries?

## P2

- [ ] Documentation: Should operator docs include connector-by-connector examples or only Slack-first setup in P11?

## Answered

- [x] One room has one profile in V1.
- [x] Room profile uses shared room session plus child thread/routine sessions.
- [x] Scoped memory tables are preferred over hybrid/namespaced memory.
- [x] Scheduled routines extend existing cron/scheduler.
- [x] Slack is first E2E MVP; Teams is audited/hardened as fast-follow.
- [x] Granularity: high-complexity one-task epics were split before implementation starts.

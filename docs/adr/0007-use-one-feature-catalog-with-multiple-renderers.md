# 7. Use one feature catalog with multiple renderers

Date: 2026-07-13
Status: Accepted

## Context

Feature onboarding is spread across slash-command help, agent help, public docs,
and machine-readable references. Copying prose between `/new-features` and
`clawq_help` would drift, while forcing identical output would make one surface
too verbose or the other too shallow.

## Decision

Maintain one versioned feature catalog containing stable batch/feature IDs,
short and expansive material, role-aware getting-started actions, documentation
links, availability, and source backlog tasks. Generate checked-in runtime data
from the docs-owned source.

`/new-features` renders a bounded short summary plus the first setup or docs
action. `clawq_help` renders expanded prerequisites, setup, troubleshooting,
commands/tools, and references. Both use the same ordered IDs and links. Default
discovery exposes only entries whose source work is shipped; unavailable work is
not presented as usable.

## Consequences

- The renderers can differ in prose length without semantic drift.
- Docs/build checks can reject duplicate IDs, missing links, stale generated
  output, or an `available` entry backed by unfinished tasks.
- Historical Claude Tag-parity and current GitHub batches are queryable through
  one interface.
- Role-aware guidance can say “ask an admin” without exposing secret setup data.

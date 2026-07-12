# 4. Let the Room Session own GitHub event context

Date: 2026-07-12
Status: Accepted

## Context

GitHub notifications need item-specific cards and threads, but participants must
also ask Room-wide questions across several repositories and items. A separate
agent Session per card would fragment shared context and make main-Room questions
and policy changes harder to reason about.

## Decision

The existing shared Room Session remains authoritative. Every routed normalized
event is appended as a hidden Session event and recorded in a durable indexed
journal. Per-Room/per-item projections derive visible cards and thread mappings
from that journal. Cards and threads resolve context into the Room Session; they
do not create authoritative item Sessions.

Webhook delivery updates history and projections but does not invoke the agent.
Replies, mentions, and supported actions invoke it explicitly.

## Consequences

- Room-wide questions can span routed items.
- Compaction may summarize Session history while the journal preserves older
  structured facts for retrieval.
- Projection replay, ordering, deduplication, and reconciliation become explicit
  durable responsibilities.
- Direct non-Room Sessions can use the same route path with weaker continuity.


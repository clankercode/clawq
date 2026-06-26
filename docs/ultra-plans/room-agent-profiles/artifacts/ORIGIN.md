# Origin Mapping

This ultra-plan tree was bootstrapped from existing versioned seed artifacts rather than duplicating their full text.

Source seed:

- `docs/plans/2026-06-26-room-agent-profiles.md`
- `docs/plans/2026-06-26-room-agent-profiles.review.md`
- Ingested backlog under `.backlog/11-room-agent-mvp/`, `.backlog/12-room-agent-governance/`, and `.backlog/13-ambient-and-scheduled-room-age/`

Deviation from `ultra-plan-from-seed`: the seed and review are about 9,900 words and already live in this branch, so this file references them and maps sections instead of embedding full verbatim copies.

| Seed Section | Captured In | Mode | Notes |
|---|---|---|---|
| Planning Inputs / Terms / Target Models | PRODUCT_GOALS, DECISIONS, INTERFACES | summarized | Key accepted decisions captured as ADRs. |
| V1 Policy Decisions | DECISIONS, INTERVIEW_QUEUE | summarized | Unresolved items converted to queue rows where still relevant. |
| Connector Support Matrix | 07-connector-polish-docs, DECISIONS | summarized | Slack-first and Teams audit captured. |
| Recommended Phase Structure | ROOT | summarized | P11/P12/P13 mapped to nodes. |
| Existing Backlog To Fold In | node SPEC files, backlog tasks | referenced | Existing bug/idea IDs captured mostly in task bodies. |
| Core Architecture Boundaries | PRODUCT_GOALS, INTERFACES | summarized | Runtime split and existing scheduler/memory boundaries preserved. |
| Phase P11 | nodes 01, 02, 03 | mapped | Implementation tasks live in `bl`. |
| Phase P12 | nodes 04, 05 | mapped | Implementation tasks live in `bl`. |
| Phase P13 | nodes 06, 07 | mapped | Implementation tasks live in `bl`. |
| TDD / Verification Strategy | PRODUCT_GOALS, node PLAN notes | summarized | Verification is represented as acceptance criteria and future leaf-plan notes. |
| Final Review Findings | DECISIONS, INTERVIEW_QUEUE, backlog dependency edits | mapped | Critical/high/medium findings were folded into task bodies and dependencies. |

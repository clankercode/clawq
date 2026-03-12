# clawq Action Proposals from Peer Platform Research

Date: 2026-03-13
Source: `docs/research/peer-platform-research-2026-03.md`

This note distills the merged peer-platform research into clawq-facing action proposals, ordered by leverage and implementation readiness.

## Recommended order of attack

### 1. Completion notifications
Why first:
- Highest-value missing piece in the “always-on personal agent” pattern.
- Research explicitly identifies this as clawq’s clearest gap-closer.
- Closely aligned with already-observed autonomy/runtime seams in this repo.
- Likely improves perceived reliability across Telegram/Discord/Slack without major architectural upheaval.

Suggested acceptance criteria:
- Background task completion is routed back to the originating channel/session when known.
- Success, failure, and blocked/permission-denied outcomes are distinguished clearly.
- Result summaries are concise and channel-appropriate.
- Notification delivery is test-covered for at least Telegram and one non-Telegram connector path.

Likely existing seams:
- background task session/channel linkage already exists from prior B132/B146/B156 work.
- remaining work is mainly better completion delivery UX and reliability.

### 2. Session cost aggregation and budgets
Why second:
- Uses existing primitives (`request_stats`, `cost_tracker.ml`).
- High operator value with comparatively low implementation risk.
- Helps close the research-identified “always-on” gap around runaway cost.

Suggested split:
- **Phase A:** add per-session cost aggregation/readout (`costs` command / inspection surface).
- **Phase B:** add soft/hard budget limits with alerting and optional stop behavior.

Suggested acceptance criteria:
- Cost totals available by session, model, and time window.
- Budget thresholds configurable and surfaced clearly.
- Over-budget behavior is deterministic and test-covered.

### 3. task_tree output budgeting (B383)
Why third:
- Internal leverage improvement for autonomous operation.
- Recently confirmed as a real context-efficiency problem.
- Fresh merged design note already narrows implementation.

Direct inputs:
- `docs/B383-task-tree-output-verbosity-design.md`

Suggested acceptance criteria:
- Large task_tree outputs are mechanically budgeted before LLM summarization is needed.
- Batch operation confirmations collapse sensibly.
- Active/blocked items are preserved preferentially.
- Focused tests cover batch output and budget caps.

### 4. Per-channel config overrides
Why fourth:
- Research shows channel personality/routing is a meaningful differentiator.
- Could improve connector UX without needing full multi-agent routing.
- Plausible precursor to richer agent-profile routing later.

Suggested acceptance criteria:
- Optional per-channel overrides for system prompt and access policy.
- Clear precedence rules between global defaults and channel overrides.
- Channel-specific tests for parsing and effective runtime behavior.

## Medium-term proposals

### Named autonomy profiles
- `suggest` / `auto-edit` / `full-auto`
- Better user-facing abstraction than raw per-tool risk levels.
- Could be layered onto existing tool permission and autonomy controls.

### Channel router abstraction
- Research points to the OpenClaw-style gateway/normalizer pattern.
- clawq already has most pieces, but explicit routing abstraction could simplify channel-specific policy, profiles, and notifications.
- Probably an architectural follow-on, not an immediate fix.

### Model routing by task complexity
- Strong potential cost win.
- More design-sensitive than cost aggregation because it changes behavior, not just observability.
- Worth pursuing after better budgets/visibility exist.

## Lower-priority / longer-horizon ideas
- MCP `.well-known` metadata
- repository indexing into structured memory
- DM access policies / approve-on-first-contact
- optional microVM sandbox backend
- MCP server registry
- A2A protocol endpoints

## Practical recommendation

If choosing one concrete implementation lane next from this research, prefer:
1. **completion notifications**, or
2. **B383 task_tree output budgeting**,
with **session cost aggregation** immediately behind them.

That ordering best matches both:
- the repo’s current autonomy/runtime pain points, and
- the research memo’s strongest competitive conclusions.

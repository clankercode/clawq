# GitHub Item Routing and Room Collaboration Plan

Date: 2026-07-12
Status: Approved for backlog ingest

## Goal

Route GitHub pull-request and issue activity into Clawq Rooms and other Sessions,
then let participants ask questions and perform policy-authorized GitHub work from
the same shared Room Session. Microsoft Teams is the rich-card pilot, while the
routing, projection, and interaction model remains Connector-neutral.

## Product contract

Architecture ADRs, glossary, and operator procedures for this contract:

- [ADR 0002 — unified live GitHub App routes](../adr/0002-use-unified-live-github-app-routes.md)
- [ADR 0003 — plan-confirm-apply for agent setup](../adr/0003-require-plan-confirm-apply-for-agent-setup.md)
- [ADR 0008 — route model, App setup, and operator contract](../adr/0008-github-route-model-and-setup.md)
- [Glossary: GitHub routes and setup](../glossary-github-routes.md)
- [GitHub route operator contract](../github-route-operator-contract.md)

### Route model

A GitHub event route has a destination, selector, versioned filter, comment mode,
managed-access linkage, capability policy, enabled state, revision, and audit
provenance.

Selectors are `Item`, `Repo`, or `Org`. Resolution is destination-local and uses
`Item > Repo > Org` specificity. The most-specific configured selector wins before
enabled/filter evaluation, so a disabled or filtered Item/Repo route never falls
through to an Org route. This lets a narrow route mute a broad feed.

Org routes must explicitly configure forwarding. The initial defaults include:

- PR opened, including opened as draft;
- PR converted to draft, ready for review, reopened, closed unmerged, and merged;
- Issue opened, reopened, closed, and transferred;
- commit, review, CI, label, assignment, milestone, title, and similar state
  changes as updates to the current item card; and
- comments in `summary` mode.

The supported comment modes are `off`, `summary`, and `threaded`. Summary mode
updates counts and latest-comment metadata without forwarding comment bodies.
Issue transfers are matched against both source and destination scope and are
deduplicated to at most one delivery per Room.

For a destination and canonical selector, at most one active route may exist.
Persistence rejects or deterministically resolves legacy/concurrent collisions.
After specificity and filter evaluation, a GitHub delivery plus canonical item
may produce at most one accepted routed event per destination, even when duplicate
webhooks, concurrent route changes, or migrated subscriptions overlap.

### Session and projection model

The shared Room Session is authoritative. Every matched normalized event is
appended chronologically as a hidden `event` message. A durable indexed journal
preserves older event history after Session compaction. Cards are visible
projections over that history, not separate per-item Sessions.

Lifecycle changes create a new card. Minor state changes edit the current card.
Teams receives Adaptive Cards; other Connectors receive equivalent rich or text
fallbacks according to their capabilities. Webhook delivery never wakes the
agent. A reply to a notification thread, an agent mention, or a supported card
action invokes the existing Room Session with the relevant item context.

### Setup and authority

Setup uses a reusable typed `plan_change -> explicit confirm -> apply` protocol.
Plans are read-only, redacted, revision-bound, expiring, persistable, and
auditable. Apply is atomic and idempotent. A global admin may target another
Room; otherwise the destination Room's admin must consent.

GitHub App onboarding uses the manifest/browser/callback flow. One-time state,
expiry, verified callback exchange, live installation identity, repository
selection, permissions, and webhook reachability are checked before the
originating Room receives a confirmable plan. Secrets are stored through the
credential boundary and never posted into a Channel.

Org scope follows the live GitHub App installation. All-repository installations
pick up newly granted repositories automatically; selected-repository removals,
suspension, and deletion fail closed. PAT mode remains an exact-Repo compatibility
path and cannot claim live Org scope.

Confirmed setup attaches a setup-owned Room access bundle and makes the resulting
catalog available to the active Room on its next turn without daemon restart.
Removing the last managed route detaches only setup-owned linkage and preserves
independent/manual grants.

### Tool and MCP scope

Clawq constructs a provider-neutral immutable Tool catalog for each executable
turn. It filters the base registry using the effective Room, Session, user,
template, MCP-server, credential, and Connector policy before provider
serialization or discovery. Execution resolves against the same frozen catalog
and repeats the authorization check.

The portable discovery interface is:

- `search_tools(query, limit=5)` for short authorized results;
- `inspect_tool(identity)` for the selected schema and risk policy; and
- `call_tool(identity, arguments)` for reauthorized dispatch.

OpenAI and Anthropic native discovery are adapters over the already-filtered
catalog. Anthropic deferred loading is treated as a context optimization, not an
authorization boundary. MCP tools use canonical `(server, remote-name)` identity,
paginated discovery, `list_changed` refresh, metadata limits, collision checks,
and fail-closed Room allowlists. MCP credentials and credential-bearing clients
must not cross Room access scopes.

Remote `list_changed` is security-significant: it immediately quarantines the
affected server/revision for new turns before relisting. A failed, malformed, or
timed-out relist leaves that server unavailable until a successful retry or
explicit repair; removals are published atomically. Already-running turns retain
their frozen catalog, but final invocation revalidates the MCP server and tool
revision and refuses a quarantined, removed, or replaced revision. By contrast,
failure while building a replacement from local configuration may retain the
previously validated local state because no remote change has been asserted.

### GitHub collaboration policy

- Read, search, status, and summarization require current repository and tool
  access.
- Explicit comments, ordinary metadata changes, and reviewer requests may execute
  after resolving and displaying the target under current Room/App policy. PR
  review submission also revalidates the displayed head and review policy, but is
  a high-risk App-attributed action subject to the pilot/production gate below.
- One shared action framework resolves the target, actor mode, typed inputs,
  policy, and authority into a redacted revision-bound preview. Fresh confirmation
  applies the preview exactly once, emits a durable receipt, and exposes
  correlation for webhook reconciliation. Expiry, replay, target/actor/policy
  change, stale state, cancellation, retry, and provider failure fail closed.
- Confirmed Issue creation and Issue/PR close/reopen, typed workflow dispatch,
  and code-changing work with constrained PR creation are separate action-family
  implementations over that framework, with independent failure-path tests.
- Confirmed code-changing work may create a PR only from an explicitly supplied
  branch or the branch/result produced by the confirmed code-work operation, and
  revalidates head and base before dispatch.
- Merge is independently enabled per Room/Repo, defaults off, and always requires
  fresh confirmation after live head, draft, mergeability, checks, reviews,
  branch-policy, method, actor mode, and authority validation.

PR review submission, Issue creation and lifecycle actions, workflow dispatch,
code-changing work/PR creation, and merge are high-risk App-attributed actions in
P19. They remain off by default outside one named, isolated, time-bounded P19
pilot and must not be presented as production-ready. Production enablement waits
for P21 to migrate them to `User_required`, complete attribution readiness, and
pass an audited enablement gate. If P21 user authorization is disabled,
unconfigured, stale, revoked, or loses authority, these actions remain denied;
they never silently fall back to App or PAT. The P19 pilot gate is disabled and
cleaned up after verification rather than inherited as a production exception.

Route setup never grants mutation authority implicitly. Actions produce durable
receipts and backlinks, and their resulting webhooks reconcile without loops or
duplicate visible messages. Room-triggered background work reuses the existing
GitHub/Claude-tag-equivalent execution path.

## Public interfaces and durable types

- `Github_item`: repository, `Pull_request | Issue`, number.
- `Github_event`: versioned normalized delivery/install/item/event envelope.
- `Github_route`: destination, selector, filter, comment mode, capabilities,
  managed linkage, revision, provenance, and lifecycle state.
- `Setup_plan`: principal, source/destination context, current/planned state,
  structured diff, readiness, warnings, base revision, expiry, digest, and typed
  apply payload.
- `Tool_catalog`: immutable catalog revision containing canonical identities,
  aliases, origin/server provenance, eager/deferred metadata, schema revisions,
  and the effective access snapshot.
- `Journal_entry`, `Item_projection`, `Delivery_intent`, and `Outbox_job` for
  durable event history, current card state, Connector operations, retries, and
  reconciliation.

Agent and CLI surfaces provide route `plan`, `apply`, `inspect`, `change`,
`disable`, and `remove`. Existing subscription commands remain compatibility
aliases over Item routes.

## Backlog structure

The backlog is authoritative for task status, acceptance bodies, estimates, and
dependencies. This checked-in inventory is a reviewable snapshot of the ingested
structure: P19 has 53 tasks totaling 231 hours, P20 has 13 tasks totaling 53
hours, and the combined plan has 66 tasks totaling 284 hours.

### P19: GitHub Item Room Routing and Collaboration — 53 tasks / 231h

1. Trusted Agent-Assisted Admin and Room Tool Scope
   - Reusable Plan-Confirm-Apply Administration
   - Frozen Room Tool Catalog
   - MCP Catalog and Live Reload Security
2. Live GitHub App Ingress and Unified Routes
   - Assisted GitHub App Onboarding and Installation Scope
   - Normalized GitHub Items and Route Resolution
   - Agent-Assisted GitHub Route Setup
3. Durable Room Delivery and Item Projections
   - Room Event Journal and Deterministic Projections
   - Connector-Neutral Delivery with Teams Cards
   - Durable Outbox, Reconciliation, and Operations
4. Conversational GitHub Collaboration and Live Pilot — 14 tasks / 68h
   - Policy-Aware Item Context and Ordinary Collaboration
   - Confirmed Workflows and Merge
   - Verification and Rollout

### P20: Advanced GitHub Routing and Setup Reuse — 13 tasks / 53h

1. Advanced Structured Org and Repo Forwarding
   - Versioned Advanced Filter Language
   - Explainability and Org-Scale Evaluation
2. Setup Framework Reuse and Operational Polish
   - Port the Existing Room-Agent Pilot
   - Upgrade and Operations

#### Current P20 filter interface

Advanced PR/Issue routing is stored as typed `Github_route_filter` schema
version 1. Operators pass it through `github route plan|change --filter-json
JSON` and can inspect a safe normalized event with `github route preview ROOM
--envelope-json JSON`. Direct `pr` / `issue` fields and an `advanced` wrapper
are mutually exclusive; raw or unknown wrapper fields are rejected. Advanced
keys need schema version 1 (legacy baseline-only filters remain readable).
Missing demanded enrichment, rate limits, and access denial mute delivery.
Normalized PR `head.ref` and PR/Issue `user.login` are persisted for branch,
author, and team-cache evaluation; webhook `sender` remains separate actor
attribution.

## Verification

The implementation must cover manifest replay/forgery/expiry/restart, installation
scope changes, webhook verification/deduplication, specificity/no-fallthrough,
transfer deduplication, lifecycle classification, stale ordering, comment modes,
card edit/replacement/fallback, hidden history and compaction, outbox restart and
24-hour dead letters, catch-up reconciliation, provider-visible Tool isolation,
alias deny-wins, MCP cross-Room and credential isolation, list changes and reload
races, risk-tiered GitHub actions, merge revalidation, and webhook self-loop
prevention.

P19 verification requires a live GitHub App -> Clawq -> Teams pilot exercising
new cards, edits, a thread reply, a Room question, reads, comments/reviews, each
confirmed action family, background work, merge, restart recovery, transient
delivery retry, a redacted receipt, and cleanup. The pilot begins inside one named
isolated Teams Room with assisted manifest/callback/plan/confirm/apply, observes
the managed access bundle, and invokes a newly enabled GitHub tool on the
immediately following turn without restarting Clawq. High-risk App-attributed
actions run only under an explicit time-bounded pilot gate that is off by default,
records actor labels and gate state in receipts, and is disabled during cleanup.
This pilot proves P19 mechanics, not production user attribution or authorization.

The rollout/backout receipt must document the safe handoff to P21. Installing P21
while user authorization is disabled leaves high-risk actions denied; completing
admin readiness and the audited attribution gate enables them only with a current
Principal-owned user lease. Authority loss returns repair or reconfirmation and
never re-enables the P19 App pilot path or falls back silently to App/PAT.

After non-trivial OCaml changes run focused tests, `make test`, and
`make fmt-check`. Runtime/library reshaping also requires `make test-all` and at
least one optimized build.

## Assumptions and exclusions

- Teams is the required rich-card pilot; architecture remains Connector-neutral.
- P19 high-risk App actions are experimental and default-off outside the isolated
  pilot; their production rollout is a P21 user-attribution responsibility.
- Direct Sessions are supported with weaker continuity than durable Rooms.
- Advanced branch/path/team/assignee/milestone predicates land in P20; P19 has
  versioned baseline event and repository forwarding controls.
- Raw JSON predicates are not supported.
- Teams, Slack, and remaining secret-bearing Connector onboarding adapters are
  separate future Ideas, not part of P19/P20 implementation.

## Complete task inventory

Dependencies shown as `-` mean no explicit dependency. Task status and full
acceptance criteria remain authoritative in `.backlog`.

### P19.M1 — Trusted Agent-Assisted Admin and Room Tool Scope

| ID | Title | Estimate | Dependencies |
|---|---|---:|---|
| P19.M1.E1.T001 | Build reusable typed admin setup plans | 4h | - |
| P19.M1.E1.T002 | Enforce confirmation revision idempotency and redacted audit | 4h | P19.M1.E1.T001 |
| P19.M1.E1.T003 | Enforce current-Room and cross-Room admin consent rules | 3h | P19.M1.E1.T002 |
| P19.M1.E1.T004 | Manage setup-owned Room access-bundle attachment and detachment | 3h | P19.M1.E1.T002, P19.M1.E1.T003 |
| P19.M1.E2.T001 | Make canonical and alias authorization deny-wins | 3h | - |
| P19.M1.E2.T002 | Capture an immutable per-turn Room Tool catalog | 5h | P19.M1.E2.T001 |
| P19.M1.E2.T003 | Scope provider serialization discovery and execution to the frozen catalog | 4h | P19.M1.E2.T002 |
| P19.M1.E2.T004 | Implement portable search_tools inspect and call discovery | 4h | P19.M1.E2.T003 |
| P19.M1.E2.T005 | Add OpenAI and Anthropic native deferred-discovery adapters | 5h | P19.M1.E2.T004 |
| P19.M1.E3.T001 | Add canonical MCP identities pagination list_changed and metadata limits | 5h | P19.M1.E2.T002 |
| P19.M1.E3.T002 | Isolate MCP server access credentials and clients by Room scope | 5h | P19.M1.E3.T001, P19.M1.E2.T003 |
| P19.M1.E3.T003 | Publish transactional registry and MCP reloads and refresh active Rooms | 4h | P19.M1.E3.T001, P19.M1.E3.T002, P19.M1.E1.T004 |
| P19.M1.E3.T004 | Replace optimistic tool-scope docs with end-to-end conformance checks | 3h | P19.M1.E3.T003 |

### P19.M2 — Live GitHub App Ingress and Unified Routes

| ID | Title | Estimate | Dependencies |
|---|---|---:|---|
| P19.M2.E1.T001 | Create resumable GitHub App manifest setup transactions | 4h | P19.M1.E1.T002 |
| P19.M2.E1.T002 | Verify and exchange one-time browser callbacks | 5h | P19.M2.E1.T001 |
| P19.M2.E1.T003 | Persist and reconcile live Org and repository installation scope | 5h | P19.M2.E1.T002 |
| P19.M2.E1.T004 | Build verified shared App webhook ingress with durable delivery identity | 5h | P19.M2.E1.T003 |
| P19.M2.E1.T005 | Preserve deterministic PAT and legacy per-Repo compatibility | 3h | P19.M2.E1.T003, P19.M2.E1.T004 |
| P19.M2.E2.T001 | Normalize PR Issue review comment CI and transfer events | 5h | P19.M2.E1.T004 |
| P19.M2.E2.T002 | Persist Item Repo and Org routes filters comment modes and capability policy | 5h | P19.M2.E2.T001 |
| P19.M2.E2.T003 | Implement Item Repo Org no-fallthrough matching | 4h | P19.M2.E2.T002 |
| P19.M2.E2.T004 | Deduplicate Issue transfer across source and destination matches per Room | 3h | P19.M2.E2.T003 |
| P19.M2.E2.T005 | Migrate per-PR subscriptions into Item routes with CLI aliases | 4h | P19.M2.E2.T003 |
| P19.M2.E3.T001 | Resume App setup in the originating Room with readiness and a confirmable plan | 4h | P19.M2.E1.T002, P19.M2.E2.T002, P19.M1.E1.T002 |
| P19.M2.E3.T002 | Add agent and CLI route plan inspect change disable and remove interfaces | 5h | P19.M2.E3.T001 |
| P19.M2.E3.T003 | Apply confirmed routes and refresh managed Room access immediately | 4h | P19.M2.E3.T002, P19.M1.E3.T003 |
| P19.M2.E3.T004 | Add route and App readiness explain audit repair and redaction tests | 4h | P19.M2.E3.T003 |
| P19.M2.E3.T005 | Write route-model setup ADRs glossary and operator contract | 3h | P19.M2.E3.T004 |

### P19.M3 — Durable Room Delivery and Item Projections

| ID | Title | Estimate | Dependencies |
|---|---|---:|---|
| P19.M3.E1.T001 | Journal routed events and append hidden Room Session event messages | 5h | P19.M2.E2.T003 |
| P19.M3.E1.T002 | Reduce events into deterministic per-Room item projections | 5h | P19.M3.E1.T001 |
| P19.M3.E1.T003 | Implement off summary and threaded comment behavior | 3h | P19.M3.E1.T002 |
| P19.M3.E1.T004 | Index Room and item history for current and compacted Session context | 4h | P19.M3.E1.T001, P19.M3.E1.T002 |
| P19.M3.E2.T001 | Define connector-neutral lifecycle-card update and reply intents | 4h | P19.M3.E1.T002 |
| P19.M3.E2.T002 | Render and edit rich Teams Adaptive Cards | 5h | P19.M3.E2.T001 |
| P19.M3.E2.T003 | Add plain-message and editless fallbacks for other Channels and Sessions | 4h | P19.M3.E2.T001 |
| P19.M3.E2.T004 | Resolve card actions thread replies and Room mentions back to item context | 5h | P19.M3.E2.T002, P19.M3.E2.T003, P19.M3.E1.T004 |
| P19.M3.E3.T001 | Add the 24-hour retrying delivery outbox and per-event dead letters | 5h | P19.M3.E1.T001, P19.M3.E2.T001 |
| P19.M3.E3.T002 | Reconcile recovery into one current-state catch-up per item | 4h | P19.M3.E3.T001, P19.M3.E1.T002 |
| P19.M3.E3.T003 | Add delivery diagnostics metrics repair and restart-reordering tests | 4h | P19.M3.E3.T001, P19.M3.E3.T002 |

### P19.M4 — Conversational GitHub Collaboration and Live Pilot (14 tasks / 68h)

| ID | Title | Estimate | Dependencies |
|---|---|---:|---|
| P19.M4.E1.T001 | Ground thread and main-Room questions in journal plus live GitHub state | 4h | P19.M3.E1.T004, P19.M3.E2.T004 |
| P19.M4.E1.T002 | Expose Room-scoped GitHub read search and status tools | 4h | P19.M4.E1.T001, P19.M1.E3.T004 |
| P19.M4.E1.T003 | Add direct explicit comments and metadata mutations | 5h | P19.M4.E1.T002 |
| P19.M4.E1.T004 | Add reviewer requests and PR review submission | 4h | P19.M4.E1.T002, P19.M4.E1.T003 |
| P19.M4.E2.T001 | Require shared revision-bound preview confirm apply for GitHub actions | 4h | P19.M4.E1.T003 |
| P19.M4.E2.T002 | Bring Claude-tag-equivalent background work into Room threads | 5h | P19.M4.E2.T001, P19.M4.E2.T007, P19.M3.E3.T001 |
| P19.M4.E2.T003 | Add independently enabled fresh-confirmed merge with live policy checks | 5h | P19.M4.E1.T004, P19.M4.E2.T001 |
| P19.M4.E2.T004 | Reconcile action receipts backlinks and resulting webhooks without loops | 5h | P19.M4.E1.T003, P19.M4.E1.T004, P19.M4.E2.T002, P19.M4.E2.T003, P19.M4.E2.T005, P19.M4.E2.T006, P19.M4.E2.T007 |
| P19.M4.E2.T005 | Implement confirmed Issue creation and lifecycle actions | 5h | P19.M4.E2.T001 |
| P19.M4.E2.T006 | Implement confirmed typed workflow dispatch | 4h | P19.M4.E2.T001 |
| P19.M4.E2.T007 | Implement confirmed code-changing work and constrained PR creation | 5h | P19.M4.E2.T001 |
| P19.M4.E3.T001 | Add cross-Connector policy migration and failure-path integration coverage | 6h | P19.M4.E2.T004 |
| P19.M4.E3.T002 | Run the live GitHub App to Clawq to Teams pilot | 8h | P19.M4.E3.T001, P19.M2.E3.T003, P19.M2.E3.T004 |
| P19.M4.E3.T003 | Publish the redacted pilot receipt rollout backout guide and cleanup result | 4h | P19.M4.E3.T002 |

### P20.M1 — Advanced Structured Org and Repo Forwarding

| ID | Title | Estimate | Dependencies |
|---|---|---:|---|
| P20.M1.E1.T001 | Add versioned advanced PR and Issue filter fields and migration | 4h | P19.M2.E2.T002 |
| P20.M1.E1.T002 | Add demand-driven changed-path and team-membership enrichment | 5h | P20.M1.E1.T001 |
| P20.M1.E1.T003 | Implement PR branch path label author team and draft predicates | 5h | P20.M1.E1.T001, P20.M1.E1.T002 |
| P20.M1.E1.T004 | Implement Issue label author team assignee and milestone predicates | 4h | P20.M1.E1.T001, P20.M1.E1.T002 |
| P20.M1.E2.T001 | Add filter preview and explain with structured rejection reasons | 4h | P20.M1.E1.T003, P20.M1.E1.T004 |
| P20.M1.E2.T002 | Add indexed and cached matching without raw JSON predicates | 5h | P20.M1.E2.T001 |
| P20.M1.E2.T003 | Add migration fixtures rate-limit behavior and Org-scale benchmarks | 4h | P20.M1.E2.T002 |

### P20.M2 — Setup Framework Reuse and Operational Polish

| ID | Title | Estimate | Dependencies |
|---|---|---:|---|
| P20.M2.E1.T001 | Adapt room-agent pilot planning to the shared typed setup framework | 4h | P19.M1.E1.T002 |
| P20.M2.E1.T002 | Adapt room-agent confirmation apply and repair to the shared framework | 4h | P20.M2.E1.T001, P19.M1.E1.T004 |
| P20.M2.E1.T003 | Add shared contract tests and retire parallel wizard semantics | 3h | P20.M2.E1.T002 |
| P20.M2.E2.T001 | Add route and filter setup diagnostics and redacted export | 3h | P20.M1.E2.T001, P20.M2.E1.T003 |
| P20.M2.E2.T002 | Add upgrade validation drift checks and admin guidance | 4h | P20.M2.E2.T001 |
| P20.M2.E2.T003 | Run final setup-framework and advanced-routing regression verification | 4h | P20.M2.E2.T002 |

# GitHub Item Routing and Room Collaboration Plan

Date: 2026-07-12
Status: Approved for backlog ingest

## Goal

Route GitHub pull-request and issue activity into Clawq Rooms and other Sessions,
then let participants ask questions and perform policy-authorized GitHub work from
the same shared Room Session. Microsoft Teams is the rich-card pilot, while the
routing, projection, and interaction model remains Connector-neutral.

## Product contract

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

### GitHub collaboration policy

- Read, search, status, and summarization require current repository and tool
  access.
- Explicit comments, metadata changes, reviewer requests, and reviews may execute
  after resolving and displaying the target.
- Create, close/reopen, workflow triggers, and code-changing/background work
  require preview and fresh confirmation.
- Merge is independently enabled per Room/Repo, defaults off, and always requires
  fresh confirmation after live head, draft, mergeability, checks, reviews,
  branch-policy, method, and authority validation.

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

### P19: GitHub Item Room Routing and Collaboration

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
4. Conversational GitHub Collaboration and Live Pilot
   - Policy-Aware Item Context and Ordinary Collaboration
   - Confirmed Workflows and Merge
   - Verification and Rollout

### P20: Advanced GitHub Routing and Setup Reuse

1. Advanced Structured Org and Repo Forwarding
   - Versioned Advanced Filter Language
   - Explainability and Org-Scale Evaluation
2. Setup Framework Reuse and Operational Polish
   - Port the Existing Room-Agent Pilot
   - Upgrade and Operations

## Verification

The implementation must cover manifest replay/forgery/expiry/restart, installation
scope changes, webhook verification/deduplication, specificity/no-fallthrough,
transfer deduplication, lifecycle classification, stale ordering, comment modes,
card edit/replacement/fallback, hidden history and compaction, outbox restart and
24-hour dead letters, catch-up reconciliation, provider-visible Tool isolation,
alias deny-wins, MCP cross-Room and credential isolation, list changes and reload
races, risk-tiered GitHub actions, merge revalidation, and webhook self-loop
prevention.

Core completion requires a live GitHub App -> Clawq -> Teams pilot exercising new
cards, edits, a thread reply, a Room question, an authorized mutation/review,
background work, merge, restart recovery, transient delivery retry, a redacted
receipt, and cleanup.

After non-trivial OCaml changes run focused tests, `make test`, and
`make fmt-check`. Runtime/library reshaping also requires `make test-all` and at
least one optimized build.

## Assumptions and exclusions

- Teams is the required rich-card pilot; architecture remains Connector-neutral.
- Direct Sessions are supported with weaker continuity than durable Rooms.
- Advanced branch/path/team/assignee/milestone predicates land in P20; P19 has
  versioned baseline event and repository forwarding controls.
- Raw JSON predicates are not supported.
- Teams, Slack, and remaining secret-bearing Connector onboarding adapters are
  separate future Ideas, not part of P19/P20 implementation.


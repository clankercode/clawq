# Glossary: GitHub routes and setup

Shared vocabulary for GitHub Item/Repo/Org routing, App setup, and Room
delivery. Canonical plan:
[docs/plans/2026-07-12-github-item-room-routing.md](plans/2026-07-12-github-item-room-routing.md).
Decisions: [ADR 0002](adr/0002-use-unified-live-github-app-routes.md),
[ADR 0003](adr/0003-require-plan-confirm-apply-for-agent-setup.md),
[ADR 0008](adr/0008-github-route-model-and-setup.md). Operator procedures:
[docs/github-route-operator-contract.md](github-route-operator-contract.md).

## Core objects

**Github_item**  
Canonical work item identity: repository full name, kind (`Pull_request` or
`Issue`), and number.

**Github_event (envelope)**  
Versioned normalized delivery/install/item/event payload produced after webhook
verification. Matching and projections consume envelopes, not raw GitHub JSON
predicates.

**Github_route**  
Durable routing rule: destination, selector, versioned filter, comment mode,
capability policy, enabled flag, revision, managed-access linkage, provenance.

**Destination**  
Where matched events go: `Room of room_id` (preferred) or `Session of session_key`
(weaker continuity). Resolution is always destination-local.

**Selector**  
What GitHub traffic the route claims:

| Selector | Identity | Specificity rank |
|----------|----------|------------------|
| **Item** | repo + kind + number | Highest |
| **Repo** | `owner/repo` | Middle |
| **Org** | organization / installation org scope | Lowest |

**Canonical selector key**  
Stable string for uniqueness, e.g. `item:owner/repo:pr:42`, `repo:owner/repo`,
`org:acme` (repo/org segments lowercased).

## Matching and mute

**Specificity order**  
`Item > Repo > Org`. Among configured routes whose selectors apply to an
envelope, the most-specific **configured** class is chosen first.

**No-fallthrough**  
Enabled/filter evaluation runs only on the winning specificity class. A
disabled or filter-rejected narrow route does **not** yield to a broader route.

**Fail-closed mute**  
Outcome **Muted** when the most-specific route exists but is disabled or fails
its filter. Intentional silence of a broader Org/Repo feed.

**Matched**  
Most-specific route is enabled and `filter_allows` succeeds.

**No_route**  
No Item/Repo/Org selector applies for that destination and envelope.

**Shadowed route**  
A less-specific route that would have applied if a more-specific configured
route did not exist. Shown in match explain for operators.

**Accept ledger / try_accept**  
Durable idempotency: one GitHub delivery id + canonical item key yields at most
one accepted routed event per destination.

## Filters and comments

**Event filter**  
Baseline include/exclude event names (and families) plus optional
include/exclude repos (especially for Org narrowing). Exclude always wins.
Empty include list means “all non-excluded.”

**Comment mode**

| Mode | Behavior |
|------|----------|
| `off` | Do not forward comment activity as comment bodies |
| `summary` | Counts / latest-comment metadata without body text |
| `threaded` | Threaded comment presentation when the Connector supports it |

**Capability policy**  
Independent flags for reply, label, assign, review, merge, close, and extras.
Route setup never silently grants high-risk mutation authority for production
user-attribution (see P21); pilot gates are explicit.

## Auth and installation

**GitHub App (App)**  
JWT + installation token identity. Supports live Org installation scope, shared
webhook ingress, and automatic pickup of newly granted repos on all-repository
installs.

**PAT**  
Personal access token compatibility path. **exact-Repo only.** Cannot claim Org
scope or live installation semantics.

**can_claim_org_scope**  
Auth snapshot predicate: true only when App auth plus Active installation can
legitimately own Org routes.

**Installation scope**  
Live record of installation id, account, suspended/deleted state, and
all-repos vs selected repository grants. Removals and suspension **fail closed**.

**Credential handle**  
Opaque reference into the credential store. Plans and Channel messages use
handles; plaintext secrets stay behind the credential boundary.

## Setup protocol

**Setup_plan**  
Typed, redacted, expiring, revision-bound structured plan with digest and apply
payload. Kinds include `Github_route` and `Github_app_setup`.

**Plan-confirm-apply**  
Three-phase admin protocol: plan (read-only store), explicit confirm, apply
(atomic mutation). Natural language alone is not consent.

**Plan is not apply**  
Storing or viewing a plan never mutates routes or installation state.

**Callback resume**  
After verified browser/manifest callback exchange, setup resumes in the
originating Room (or a notification) with readiness and a confirmable plan. The
**callback is not apply confirmation**.

**Base revision / OCC**  
Optimistic concurrency token. Stale plans and concurrent updates fail before
mutation; operators re-plan.

**Digest**  
Content hash of the plan; apply requires the matching plan id and digest.

**Managed access bundle**  
Setup-owned Room access linkage attached on apply. Detaches only after the last
managed feature/route is removed; manual grants remain.

**Catalog refresh**  
Post-apply hook so the active Room sees updated tools on the **next turn**
without daemon restart.

## Ingress and delivery

**Shared App webhook ingress**  
One verified HTTP path (default `/github/app/webhook`) for App deliveries:
signature, delivery id, scope, event allow-list, then durable accept.

**Delivery id**  
GitHub `X-GitHub-Delivery` value. Primary key for ingress idempotency.

**Delivery ACK independent of Connector**  
HTTP 2xx and ledger recording do not depend on Connector fan-out, card render,
or outbox success. Connector failure is a separate repair track.

**Duplicate**  
Ingress outcome when a delivery id was already accepted; safe GitHub retry.

**Outbox / dead letter**  
Durable Connector delivery jobs and aged failures (P19.M3). Independent of
ingress ACK.

**Journal / projection**  
Room Session append-only event history and derived item cards (ADR 0004).
Webhook path does not invoke the agent.

## Operator diagnostics

**Inspect**  
Channel-safe route summary and explain lines (no secrets).

**Readiness report**  
Pass/Warn/Fail checks for installation, Org auth, grants, tools, MCP,
credentials, egress, Connector, delivery, and plan revision, each with optional
repair text.

**Match explain**  
Human/structured summary of Matched/Muted/No_route, winner route, and shadowed
routes.

**Redaction**  
Mandatory stripping of private keys, secrets, tokens, PEM/bearer patterns, and
bounded truncation of large strings on audit/export paths.

## Compatibility

**Legacy per-PR subscription**  
Older storage/CLI for single PR watches. Migrated to **Item** routes; CLI remains
compatibility aliases over the unified route model.

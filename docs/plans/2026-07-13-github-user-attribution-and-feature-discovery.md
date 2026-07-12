# GitHub User Attribution and Feature Discovery

Date: 2026-07-13
Status: approved for backlog ingestion
Source idea: I059

## Goal

Let a verified human Principal privately authorize the GitHub App and have
explicit GitHub mutations performed with GitHub's native user-plus-App
attribution. Preserve existing App/PAT routing and automation, prevent shared
Rooms or delayed work from borrowing another participant's credentials, and
make the completed GitHub and Claude Tag-parity feature sets discoverable from
public docs, `/new-features`, and `clawq_help`.

## Locked architecture

- A Connector actor is ingress evidence. A Principal is Clawq's durable human
  identity. Rooms and Sessions never own human credentials.
- Cross-Connector linking is explicit, private, expiring, and proven by both
  actors. Display names, emails, and matching GitHub accounts never auto-merge
  Principals. Audited admin repair uses plan-confirm-apply.
- GitHub numeric user ID plus App ID is the account identity. Login and avatar
  are display metadata. Preferences are selection hints, not authorization.
- GitHub App user access tokens are Principal-owned encrypted records. Web flow
  uses state plus S256 PKCE; device flow is first-class and obeys GitHub's
  server-provided interval, `slow_down`, and expiry behavior. Shared Rooms see
  neutral status only; authorization material is delivered privately.
- Expiring user tokens are required. Refresh is on-demand, single-flight, and
  compare-and-swap generation protected. Missing keys, tampering, expiry,
  revocation, SSO failure, permission loss, and account mismatch fail closed.
- Every GitHub mutation declares `App`, `User_required`, or `User_preferred`.
  Reads and ambient automation remain App-first. Comments and ordinary metadata
  are user-preferred with only explicitly previewed, policy-enabled App
  fallback. Reviews, lifecycle actions, workflows, creation, code work, and
  merge are user-required.
- OAuth authorization is not action confirmation. Delayed work pins the
  initiating Principal, account, mode, confirmation, and token generation, then
  revalidates and leases at dispatch without switching identity.
- User tokens are API-boundary credentials only. They never enter prompts,
  Session history, tool payloads, config, logs, jobs, worktrees, runners, shell,
  Git transport, receipts, or audit exports.
- V1 live support is GitHub.com. Records carry host and App identity for future
  migration, but no GHES claim is made.
- Feature discovery has one canonical material source and two renderers:
  `/new-features` is concise and action-oriented; `clawq_help` is expansive.
  Only shipped features are shown. The discovery phase starts after P19-P21.

## Public interfaces

- Domain types: `Connector_actor`, `Principal`, `Identity_link`,
  `Github_account_binding`, immutable `Actor_snapshot`, authorization
  transaction, token generation, and `Github_attribution_mode`.
- CLI: `clawq github account link|list|status|use|relink|unlink`.
- Agent: a narrow `github_account` tool returns redacted status while private
  continuations deliver URLs, device codes, and account-selection controls.
- Account selection: explicit choice, Room+Repo, Room+Org, Principal+Repo,
  Principal+Org, Principal default, sole eligible account, then private prompt.
- Discovery: `/new-features`, `/new-features list`, and
  `/new-features <batch-id> [page]`; `clawq_help` gains optional `batch`,
  `feature`, and `page` fields for `topic=new-features`.
- Canonical discovery data: `docs/src/data/feature_catalog.json`, with generated
  checked-in OCaml runtime material and a drift-check target.

## P21 inventory: Principal Identity and GitHub User Attribution

P21 has 50 tasks / 214h. It has no phase-wide dependency so identity and vault
work can begin before P19 completes; action tasks carry exact P19 dependencies.

### M1 Stable Principal Identity and Account Binding — 44h

#### E1 Principal and Connector Identity Foundation

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M1.E1.T001 Define Principal Connector actor and Identity Link domain model | 3 | — | Typed versioned identities exclude Room/session/display-name ownership. |
| P21.M1.E1.T002 Persist Principals and collision-safe Connector identity links | 4 | T001 | Additive schema and concurrent first-seen creation enforce one active owner. |
| P21.M1.E1.T003 Resolve verified Connector Actors to stable Principals | 4 | T002 | Stable Teams/Slack/Discord/Telegram/web/CLI actors resolve; forged or ambiguous actors do not. |
| P21.M1.E1.T004 Add private cross-Connector linking unlinking and revocation | 4 | T003, P19.M1.E1.T002 | Two-sided expiring proof and audited repair; revocation is immediately effective. |

#### E2 Principal GitHub Accounts and Preferences

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M1.E2.T001 Persist Principal-owned GitHub account bindings | 4 | M1.E1.T002, P19.M2.E1.T003 | Numeric user/App identity and opaque vault reference only. |
| P21.M1.E2.T002 Enforce verified ownership and duplicate-account policy | 4 | T001, M1.E1.T004 | No cross-Principal collision without explicit audited exception. |
| P21.M1.E2.T003 Implement repository- and Room-aware account preferences | 3 | T002 | Deterministic precedence; ambiguity prompts privately. |
| P21.M1.E2.T004 Expose redacted account inspection preference and unlink operations | 3 | T003, P19.M1.E1.T002 | Self-service surfaces disclose no token material. |

#### E3 Durable Actor Attribution and Migration

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M1.E3.T001 Define immutable Actor snapshots for intents and delayed work | 3 | M1.E1.T003, M1.E2.T003 | Evidence-only snapshot contains no reusable authority. |
| P21.M1.E3.T002 Carry Actor snapshots through P19 actions jobs retries and receipts | 4 | T001, P19.M4.E2.T004 | Execution re-resolves current authority. |
| P21.M1.E3.T003 Migrate legacy requester identities without unsafe coalescing | 4 | M1.E1.T004, M1.E2.T003, T001 | Only unambiguous actors backfill; legacy-unresolved rows cannot authorize users. |
| P21.M1.E3.T004 Prove cross-Principal isolation and delayed-work attribution | 4 | T002, T003 | Shared-Room, tenant, rename, restart, ambiguity, and revocation tests. |

### M2 Private GitHub Authorization and Token Vault — 63h

#### E1 App Readiness and Authorization Transactions

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M2.E1.T001 Extend GitHub App user-authorization readiness | 4 | P19.M2.E1.T003 | Require expiring tokens and validate OAuth client, callback, and device settings. |
| P21.M2.E1.T002 Persist one-time Principal-bound authorization transactions | 4 | M1.E1.T004, P19.M1.E1.T002 | Restart-safe, expiring, one-time transaction with source context. |
| P21.M2.E1.T003 Deliver authorization continuations privately | 4 | T002 | Rooms receive neutral status; unsupported private delivery refuses safely. |

#### E2 Web Authorization with PKCE and State

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M2.E2.T001 Start state-bound S256 PKCE authorization | 4 | M2.E1.T001-T003 | Exact redirect, independent state/verifier, no Room disclosure. |
| P21.M2.E2.T002 Verify callback and exchange the code exactly once | 5 | T001, M2.E4.T002 | Constant-time state/expiry/redirect/replay checks precede exchange. |
| P21.M2.E2.T003 Verify GitHub numeric identity and prepare a confirmable binding plan | 4 | T002, M1.E2.T002, M2.E4.T004 | `/user` verification seals pending credentials; explicit matching confirmation atomically activates the binding. |

#### E3 Device Authorization

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M2.E3.T001 Start device authorization with private code delivery | 4 | M2.E1.T001-T003, M2.E4.T002 | Persist encrypted device state and server timing. |
| P21.M2.E3.T002 Implement durable leased device polling | 5 | T001 | Single worker honors interval, slow_down, cancellation, expiry, and restart. |
| P21.M2.E3.T003 Handle every terminal response and prepare verified binding | 5 | T002, M2.E2.T003 | Success reuses the confirmable binding plan; all failures terminate without active partial state. |

#### E4 Fail-Closed Mutable Token Vault

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M2.E4.T001 Define versioned encrypted GitHub user-token records | 5 | — | Authenticated ciphertext with unique nonce and record-bound associated data. |
| P21.M2.E4.T002 Implement fail-closed mutable vault CRUD and key handling | 5 | T001 | No plaintext fallback for key, envelope, version, or identity failures. |
| P21.M2.E4.T003 Expose callback-scoped opaque leases at the GitHub HTTP boundary | 4 | T002, M1.E2.T001 | Only handle/binding/generation crosses other boundaries. |
| P21.M2.E4.T004 Add generation-based CAS transitions and lease invalidation | 5 | T002, T003 | Stale writes and leases cannot restore older authority. |
| P21.M2.E4.T005 Prove vault tamper restart deletion and rollback behavior | 5 | T004 | Corruption, swapping, copy, concurrency, key loss, and destruction fail closed. |

### M3 Rotation Revocation and Attributed Actions — 69h

#### E1 Refresh and Revocation Lifecycle

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M3.E1.T001 Refresh expiring tokens using server-returned lifetimes | 4 | M2.E2.T003, M2.E3.T003, M2.E4.T004 | On-demand refresh uses returned expiries and validated token type. |
| P21.M3.E1.T002 Add durable single-flight refresh with CAS rotation | 5 | T001 | One remote refresh; late/replayed results cannot roll state back. |
| P21.M3.E1.T003 Process GitHub App authorization revocation webhooks | 4 | P19.M2.E1.T004, M2.E4.T004 | Verified idempotent revocation disables bindings, jobs, leases, and secrets. |
| P21.M3.E1.T004 Implement unlink and Principal/Connector removal invalidation | 4 | T002, T003, M1.E1.T004 | Local denial precedes best-effort remote revoke and unconditional destruction. |

#### E2 Attribution Policy and Authorization Resolution

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M3.E2.T001 Add attribution requirements and risk-tier defaults | 5 | P19.M1.E1.T002, P19.M4.E2.T004 | Every mutation declares App/User_required/User_preferred. |
| P21.M3.E2.T002 Resolve eligible accounts and first-use context preferences | 4 | M1.E2.T003, M2 authorization flows | Explicit Room/Repo/Org precedence; no login/recency guessing. |
| P21.M3.E2.T003 Resolve opaque leases after all current policy checks | 8 | T002, M3.E1.T002 | Intersect Room, confirmation, binding, App, repo, Org/SSO, user, and live state. |
| P21.M3.E2.T004 Enforce visible App fallback and fail-closed user-required behavior | 4 | T001, T003 | Fallback occurs only when policy and confirmed preview named it. |
| P21.M3.E2.T005 Add attribution previews receipts repair states and audit | 5 | T003, T004, P19.M4.E2.T004 | Record requested/resolved actor and reason without credentials. |

#### E3 P19 Action Integration

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M3.E3.T001 Integrate user-preferred comments and metadata writes | 5 | P19.M4.E1.T003, M3.E2.T005 | Native user attribution or explicitly confirmed App fallback. |
| P21.M3.E3.T002 Integrate user-required high-risk GitHub mutations | 8 | P19.M4.E1.T004, P19.M4.E2.T001, P19.M4.E2.T002, P19.M4.E2.T003, M3.E2.T005 | Reviews, lifecycle, workflow, creation, code work, and merge cannot fall back. |
| P21.M3.E3.T003 Preserve pinned attribution through delayed/background work | 6 | P19.M4.E2.T002, M1.E3.T002, M3.E2.T005 | Retry cannot change Principal, account, mode, or confirmation. |
| P21.M3.E3.T004 Keep personal tokens out of automation runners shell and Git transport | 3 | T003 | Scheduled work stays App-owned; transport is a separate Idea. |
| P21.M3.E3.T005 Reconcile resulting webhooks with native attribution receipts | 4 | P19.M4.E2.T004, T001, T002 | Native actor/App association closes receipts without loops. |

### M4 UX Operations Verification and Security Contract — 38h

#### E1 User and Admin Experience

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M4.E1.T001 Add GitHub account CLI and redacted agent surfaces | 4 | M3.E1.T004 | Link/list/status/use/relink/unlink are self-service and private. |
| P21.M4.E1.T002 Add admin enablement readiness and repair | 4 | T001, P19 setup readiness | Admin enables capability; users authorize only themselves. |
| P21.M4.E1.T003 Add user-authorization diagnostics and metrics | 3 | T001, M3.E2.T005 | Distinguish SSO, permission, refresh, rate-limit, revocation, App/repo scope, expiry, ambiguity, and private-delivery failures without secrets. |
| P21.M4.E1.T004 Preserve App/PAT compatibility and minimal-build behavior | 3 | M3.E3.T005 | Existing paths remain explicit; integrations are disabled cleanly in minimal. |

#### E2 Integration and Live Pilot

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M4.E2.T001 Add cross-Connector and shared-Room integration coverage | 5 | M4.E1, M3.E3 | Two linked users and one unlinked user remain isolated. |
| P21.M4.E2.T002 Run token-leak restart concurrency and private-delivery verification | 5 | T001, M2.E4.T005 | Scan every persistence, prompt, Connector, tool, job, runner, and audit surface. |
| P21.M4.E2.T003 Run a live Teams dual-attribution pilot | 6 | T001, T002, P19.M4.E3.T002 | Separate web/device users, fallback, delayed work, rotation, revocation, denial. |
| P21.M4.E2.T004 Publish redacted receipt backout cleanup and limitations | 2 | T003 | Clean all GitHub artifacts and local bindings with evidence. |

#### E3 Architecture and Operator Documentation

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M4.E3.T001 Finalize Principal/token-vault security boundary ADRs and glossary | 3 | M4.E2.T004 | Final docs match shipped schema and data flow. |
| P21.M4.E3.T002 Maintain implementation inventory and user-auth operator contract | 3 | M4.E3.T001 | Task/source/API crosswalk and repair guidance are current. |

## P22 inventory: GitHub Documentation and Feature Discovery

P22 depends on completed P19, P20, and P21. It has 13 tasks / 76h.

### M1 Complete GitHub Documentation — 24h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P22.M1.E1.T001 Build a code- and backlog-backed GitHub docs coverage matrix | 4 | P19, P20, P21 | Inventory every command, config, tool, endpoint, event, policy, and recovery surface. |
| P22.M1.E1.T002 Publish the complete Astro GitHub documentation set | 8 | T001 | Setup, routing, collaboration, attribution, operations, migration, and examples. |
| P22.M1.E1.T003 Publish GitHub machine references and update llms indexes | 6 | T002 | Integration llms file plus spec-compliant llms.txt and complete llms-full.txt. |
| P22.M1.E1.T004 Add automated GitHub documentation coverage validation | 6 | T003 | Fail on missing symbols, stale names, broken links, or malformed llms.txt. |

### M2 Claude Tag Parity Documentation Audit — 19h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P22.M2.E1.T001 Refresh the FT-01–FT-46 and P11–P18 evidence crosswalk | 6 | P22.M1.E1.T003 | Current code/test/backlog/docs evidence and honest status. |
| P22.M2.E1.T002 Close all shipped parity documentation gaps | 8 | T001 | User and machine docs cover implemented/partial capabilities and deliberate differences. |
| P22.M2.E1.T003 Add a repeatable parity documentation audit | 5 | T002 | Stale evidence, contradictory status, missing docs, and unowned partials fail actionably. |

### M3 Shared Feature Catalog and Discovery — 33h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P22.M3.E1.T001 Define canonical feature schema generation and checks | 8 | P22.M1.E1.T004, P22.M2.E1.T002 | One source generates runtime/docs data and rejects unfinished `available` entries. |
| P22.M3.E1.T002 Populate GitHub and historical Claude Tag-parity batches | 6 | T001 | Every entry has short/long material, role-aware start action, docs, and source tasks. |
| P22.M3.E1.T003 Add concise Connector-neutral `/new-features` | 5 | T002 | Latest/list/batch/page output is bounded and setup-oriented. |
| P22.M3.E1.T004 Add expansive `clawq_help` feature rendering | 5 | T002 | Optional batch/feature/page fields render details and troubleshooting. |
| P22.M3.E1.T005 Verify shared material visibility generation and Connector fallbacks | 5 | T003, T004 | Same ordered IDs/links, intentionally different verbosity, shipped-only visibility. |
| P22.M3.E1.T006 Make feature and documentation maintenance a repository contract | 4 | T005 | Update AGENTS.md, docs instructions, agent templates/prompts, generation/check targets, and contribution guidance so future shipped features update the catalog and relevant human/machine docs. |

## Follow-up boundary

Create a separate Idea for brokered authenticated Git pushes and verified
user/LLM commit authorship. It must cover token brokering without runner/shell
exposure, explicit email consent, signing and audit, and whether a display such
as `{Name} + LLMs <email>` is desirable. P21 does not solve Git transport.

## Verification and rollout

- Additive schema migrations and compatibility fixtures preserve existing App
  and PAT behavior. User authorization is disabled until an admin enables it
  and readiness proves master-key, App, callback, expiry, and private-delivery
  requirements.
- Unit tests cover identity collisions, cross-tenant actors, linking, account
  precedence, PKCE, callback replay, device polling, vault tampering, refresh
  races, revocation, SSO/permission loss, and redaction.
- Integration tests cover shared Room isolation, delayed work, minimal builds,
  App fallback policy, native attribution receipts, and webhook reconciliation.
- Live verification uses two Teams Principals with separate web/device flows and
  an unlinked third Principal, then cleans up with a redacted receipt.
- P22 runs docs builds, llms structural/link checks, source-to-doc coverage,
  feature catalog drift checks, and cross-renderer consistency checks.

## Official protocol references

- https://docs.github.com/en/apps/creating-github-apps/authenticating-with-a-github-app/generating-a-user-access-token-for-a-github-app
- https://docs.github.com/en/apps/creating-github-apps/authenticating-with-a-github-app/refreshing-user-access-tokens
- https://docs.github.com/en/apps/creating-github-apps/registering-a-github-app/choosing-permissions-for-a-github-app
- https://docs.github.com/en/webhooks/webhook-events-and-payloads#github_app_authorization
- https://docs.github.com/en/enterprise-cloud@latest/apps/using-github-apps/saml-and-github-apps

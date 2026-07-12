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

- A Connector actor is verified ingress evidence. Teams verifies Bot Connector
  OpenID metadata/JWKS, RS256 signature, issuer, audience, time, tenant,
  `serviceUrl`, endorsements, key rotation, and fail-closed key retrieval before
  deriving tenant plus immutable user ID. Slack, Discord, and Telegram have
  equally explicit signature/replay/namespace adapters. Web, CLI, and direct
  Sessions require authenticated issuer plus immutable subject bootstrap;
  request fields, process/session metadata, display names, and ambiguous or bot
  provenance never establish a human actor. A Principal is Clawq's durable human
  identity. Rooms and Sessions never own human credentials.
- Cross-Connector linking is explicit, private, expiring, and proven by both
  verified actors. Display names, emails, and matching GitHub accounts never
  auto-merge Principals. Audited admin repair uses plan-confirm-apply. When both
  actors already have Principals, the earlier durable creation order survives,
  with stable Principal ID as the tie-breaker unless an admin repair explicitly
  selects the survivor. Apply atomically adopts non-conflicting state and leaves
  an immutable `merged_into` tombstone; external-account conflicts fail closed,
  pending authorization is invalidated, and historical Actor snapshots are not
  rewritten. Unlink is an explicit split to a new empty Principal and transfers
  no account binding, credential, pending transaction, or authority.
- GitHub numeric user ID plus App ID is the account identity. Login and avatar
  are display metadata. Preferences are selection hints, not authorization.
- GitHub App user access tokens are Principal-owned encrypted records. Web flow
  uses state plus S256 PKCE; device flow is first-class and obeys GitHub's
  server-provided interval, `slow_down`, and expiry behavior. Shared Rooms see
  neutral status only; authorization material is delivered privately.
- Expiring user tokens are required. Token generation versions one logical
  binding's GitHub access/refresh lineage and protects refresh, revoke, and lease
  CAS. Vault master-key version is a separate encryption property: external key
  source, active key ID, staged crash-safe rewrap, backup-required key IDs, and
  retirement are explicit. Restore starts with user authorization disabled and
  discards leases until refresh and identity reconciliation. Missing keys,
  tampering, expiry, revocation, SSO failure, permission loss, account mismatch,
  and compromise fail closed; compromise or unrecoverable loss disables and
  destroys affected bindings and requires relinking. Record AEAD and generation
  CAS detect row swap and live stale writes, but cannot detect replacement of the
  whole database with an internally consistent old snapshot under an available
  key. V1 makes no whole-store anti-rollback claim without an external monotonic
  anchor.
- Every GitHub mutation declares `App`, `User_required`, or `User_preferred`.
  Reads and ambient automation remain App-first. Comments and ordinary metadata
  are user-preferred with only explicitly previewed, policy-enabled App
  fallback. Reviewer requests follow that ordinary-metadata rule. PR review
  submissions and reviewer decisions, lifecycle actions, workflows, creation,
  code work, and merge are user-required.
- OAuth authorization is not action confirmation. Delayed work pins immutable
  Actor evidence plus logical Principal/account binding lineage, requested and
  resolved mode, confirmation, and expected GitHub actor, but no credential.
  Ordinary refresh may advance token generation within the same valid lineage;
  unlink, split, conflicting merge, relink, revoke, actor/account change, or
  policy loss breaks lineage and pauses or fails rather than switching identity.
- User tokens are API-boundary credentials only. They never enter prompts,
  Session history, tool payloads, config, logs, jobs, worktrees, runners, shell,
  Git transport, receipts, or audit exports.
- V1 live support is GitHub.com. Records carry host and App identity for future
  migration, but no GHES claim is made.
- P19 high-risk App-attributed behavior is available only under a named,
  time-bounded pilot gate that is off by default. P21 owns a versioned migration
  state matrix for every read, mutation, background action, preview actor,
  fallback, receipt, and webhook. An upgrade never silently changes actor or
  weakens confirmation: `User_required` stays disabled until Principal, vault,
  policy, private-delivery, and repair readiness pass; pilot enable, production
  enable, rollback, and cleanup are explicit audited transitions.
- Feature discovery has one canonical material source and two renderers:
  `/new-features` is concise and action-oriented; `clawq_help` is expansive.
  Catalog audience is `public`, `authenticated-user`, `room-admin`, or
  `operator`; missing,
  ambiguous, guest, or unverified caller-role evidence renders public material
  only. Availability is `planned`, `preview`, `available`, `deprecated`, or
  `removed`; default discovery shows available entries, while deprecated and
  removed entries retain deterministic replacement/tombstone semantics and are
  not presented as normally usable. Both renderers use the same ordered visible
  IDs and links.
  P22 starts only after P19, P20, and P21 complete.
- Documentation evidence is repository-owned and machine-readable. The GitHub
  coverage matrix maps stable surface IDs to code, completed leaf tasks, public
  docs, machine references, and exclusions. The hashed FT source manifest and
  parity crosswalk replace machine-local downloads and prose as audit inputs.
  Deterministic check mode never mutates evidence or embeds current time;
  explicit update mode refreshes evidence and writes a reviewed dated receipt.
  One `docs-check` runs the matrix, parity, catalog, generated-data, llms,
  navigation/orphan/link, and Astro checks in PR CI and before Pages upload.

## Public interfaces

- Domain types: `Connector_actor`, `Principal`, `Identity_link`,
  `Merged_principal_alias`, `Github_account_binding`, immutable `Actor_snapshot`,
  logical binding lineage, authorization transaction, token generation, vault
  master-key ID/version, verified caller audience, and
  `Github_attribution_mode`.
- CLI: `clawq github account link|list|status|use|relink|unlink`.
- Agent: a narrow `github_account` tool returns redacted status while private
  continuations deliver URLs, device codes, and account-selection controls.
- Account selection: explicit choice, Room+Repo, Room+Org, Principal+Repo,
  Principal+Org, Principal default, sole eligible account, then private prompt.
- Discovery: `/new-features`, `/new-features list`, and
  `/new-features <batch-id> [page]`; `clawq_help` gains optional `batch`,
  `feature`, and `page` fields for `topic=new-features`. Slash and tool paths
  consume verified current audience; `clawq_help` defaults to public when Tool
  context cannot carry that evidence.
- Canonical data paths:
  - `docs/src/data/feature_catalog.json`: versioned catalog and generated
    checked-in OCaml runtime material.
  - `docs/src/data/github_coverage_matrix.json`: GitHub surface/evidence matrix.
  - `docs/src/data/claude_tag_ft_source_manifest.json`: normalized FT source,
    provenance, version rules, and content hashes.
  - `docs/src/data/claude_tag_parity_crosswalk.json`: FT/P11-P18 status and
    code/test/task/public/machine-doc evidence.
  - `docs/plans/receipts/documentation-audits/`: explicit reviewed update
    receipts; deterministic check mode does not create or rewrite them.

## P21 inventory: Principal Identity and GitHub User Attribution

P21 has 70 tasks / 328h. It has no phase-wide dependency so identity and vault
work can begin before P19 completes; action tasks carry exact P19 dependencies.

### M1 Stable Principal Identity and Account Binding — 103h

#### E1 Principal and Connector Identity Foundation — 57h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M1.E1.T001 Define Principal Connector actor and Identity Link domain model | 3 | — | Typed versioned identities exclude Room/session/display-name ownership. |
| P21.M1.E1.T002 Persist Principals and collision-safe Connector identity links | 4 | P21.M1.E1.T001 | Additive schema and concurrent first-seen creation enforce one active owner. |
| P21.M1.E1.T003 Resolve adapter-verified Connector actors to stable Principals | 5 | P21.M1.E1.T002, P21.M1.E1.T005, P21.M1.E1.T006, P21.M1.E1.T007, P21.M1.E1.T008, P21.M1.E1.T009 | Resolve only typed adapter-verified assertions; invalid, bot, stale, or ambiguous provenance never resolves as human. |
| P21.M1.E1.T004 Define private cross-Connector linking and admin repair protocol | 4 | P21.M1.E1.T003, P19.M1.E1.T002 | Version the private proof and revision-bound audited repair protocol without auto-linking. |
| P21.M1.E1.T005 Validate Teams ingress and derive canonical user identity | 6 | P21.M1.E1.T002 | Verify OpenID/JWKS, RS256, claims, tenant, `serviceUrl`, endorsements, rotation, and immutable Teams user identity fail closed. |
| P21.M1.E1.T006 Validate Slack ingress and derive canonical user identity | 4 | P21.M1.E1.T002 | Trust only authenticated Socket Mode WSS envelopes and derive workspace-scoped immutable human user identity. |
| P21.M1.E1.T007 Validate Discord ingress and derive canonical user identity | 4 | P21.M1.E1.T002 | Trust only authenticated Gateway sessions/sequences and derive application/guild-scoped immutable human identity. |
| P21.M1.E1.T008 Validate Telegram ingress and derive canonical user identity | 4 | P21.M1.E1.T002 | Trust only bot-authenticated HTTPS long-poll updates with monotonic offsets and immutable sender identity. |
| P21.M1.E1.T009 Fail closed for web CLI and direct-session Principal bootstrap | 5 | P21.M1.E1.T002 | Require authenticated issuer/subject enrolment; local process, Session, or request metadata grants no Principal. |
| P21.M1.E1.T010 Execute private two-sided cross-Connector link proof transaction | 4 | P21.M1.E1.T004 | Execute one-time expiring proof atomically with replay, concurrency, cancellation, and audit coverage. |
| P21.M1.E1.T011 Merge or adopt Principals deterministically after verified linking | 7 | P21.M1.E1.T010 | Select a deterministic survivor, adopt non-conflicting state atomically, and leave a stable tombstone. |
| P21.M1.E1.T012 Implement unlink split and identity revocation lifecycle | 7 | P21.M1.E1.T011 | Split to a new empty Principal, transfer no authority implicitly, and invalidate affected work. |

#### E2 Principal GitHub Accounts and Preferences — 18h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M1.E2.T001 Persist Principal-owned GitHub account bindings | 5 | P21.M1.E1.T002, P21.M1.E1.T011, P19.M2.E1.T003 | Persist numeric user/App identity, lineage, display metadata, and opaque vault reference with adoption support. |
| P21.M1.E2.T002 Enforce verified ownership and duplicate-account policy | 5 | P21.M1.E2.T001, P21.M1.E1.T012 | Require current verified ownership and fail closed on duplicate, merge/split conflict, or race. |
| P21.M1.E2.T003 Implement repository- and Room-aware account preferences | 4 | P21.M1.E2.T002, P21.M1.E1.T011 | Apply deterministic preference precedence across adoption without login/recency guessing. |
| P21.M1.E2.T004 Expose redacted account inspection preference and unlink operations | 4 | P21.M1.E2.T003, P21.M1.E1.T012, P19.M1.E1.T002 | Route private inspection, preference, unlink/split, and revocation through canonical lifecycle operations. |

#### E3 Durable Actor Attribution and Migration — 28h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M1.E3.T001 Define immutable Actor snapshots for intents and delayed work | 4 | P21.M1.E1.T003, P21.M1.E1.T011, P21.M1.E1.T012, P21.M1.E2.T003 | Capture immutable evidence plus logical lineage, never reusable authority. |
| P21.M1.E3.T002 Carry Actor snapshots through P19 action intents and confirmations | 4 | P21.M1.E3.T001, P19.M4.E2.T001 | Preserve initiating evidence through preview/confirmation and re-resolve live authority at dispatch. |
| P21.M1.E3.T003 Migrate legacy requester identities without unsafe coalescing | 5 | P21.M1.E1.T011, P21.M1.E1.T012, P21.M1.E2.T003, P21.M1.E3.T001 | Backfill only unambiguous verified identities; unresolved rows cannot authorize user work. |
| P21.M1.E3.T004 Prove cross-Principal isolation and delayed-work attribution | 6 | P21.M1.E3.T002, P21.M1.E3.T003, P21.M1.E3.T005, P21.M1.E3.T006, P21.M1.E1.T012 | Cover every adapter, merge/split/revoke, shared Rooms, history, retry, and legacy isolation. |
| P21.M1.E3.T005 Propagate Actor snapshots through durable jobs retries and outbox | 5 | P21.M1.E3.T001, P21.M1.E3.T002, P19.M3.E3.T001 | Preserve immutable evidence and lineage through durable work without tokens or borrowing. |
| P21.M1.E3.T006 Propagate Actor snapshots through receipts and webhook reconciliation | 4 | P21.M1.E3.T001, P21.M1.E3.T002, P19.M4.E2.T004 | Preserve historical actor evidence through receipt closure and distinguish unrelated webhooks. |

### M2 Private GitHub Authorization and Token Vault — 84h

#### E1 App Readiness and Authorization Transactions — 12h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M2.E1.T001 Extend GitHub App user-authorization readiness | 4 | P19.M2.E1.T003 | Require expiring tokens and validate OAuth client, callback, and device settings. |
| P21.M2.E1.T002 Persist one-time Principal-bound authorization transactions | 4 | P21.M1.E1.T004, P19.M1.E1.T002 | Restart-safe, expiring, one-time transaction with source context. |
| P21.M2.E1.T003 Deliver authorization continuations privately | 4 | P21.M2.E1.T002 | Rooms receive neutral status; unsupported private delivery refuses safely. |

#### E2 Web Authorization with PKCE and State — 19h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M2.E2.T001 Start state-bound S256 PKCE authorization | 4 | P21.M2.E1.T001, P21.M2.E1.T002, P21.M2.E1.T003 | Exact redirect, independent state/verifier, no Room disclosure. |
| P21.M2.E2.T002 Verify callback and exchange the code exactly once | 5 | P21.M2.E2.T001, P21.M2.E4.T002 | Constant-time state/expiry/redirect/replay checks precede exchange. |
| P21.M2.E2.T003 Route web authorization through shared verified activation | 4 | P21.M2.E2.T002, P21.M2.E2.T004 | Send web success through the flow-neutral verification, plan, confirmation, and activation transaction. |
| P21.M2.E2.T004 Build shared verified pending-credential activation transaction | 6 | P21.M1.E2.T002, P21.M2.E4.T004 | Verify `/user`, seal pending credentials, and atomically activate only after matching private confirmation. |

#### E3 Device Authorization — 13h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M2.E3.T001 Start device authorization with private code delivery | 4 | P21.M2.E1.T001, P21.M2.E1.T002, P21.M2.E1.T003, P21.M2.E4.T002 | Persist encrypted device state and server timing. |
| P21.M2.E3.T002 Implement durable leased device polling | 5 | P21.M2.E3.T001 | Single worker honors interval, `slow_down`, cancellation, expiry, and restart. |
| P21.M2.E3.T003 Handle every terminal response and prepare verified binding | 4 | P21.M2.E3.T002, P21.M2.E2.T004 | Route success through shared activation; every failure terminates without active partial state. |

#### E4 Fail-Closed Mutable Token Vault — 40h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M2.E4.T001 Define versioned encrypted GitHub user-token records | 5 | — | Authenticated ciphertext with unique nonce and record-bound associated data. |
| P21.M2.E4.T002 Implement fail-closed mutable vault CRUD | 5 | P21.M2.E4.T001, P21.M2.E4.T006 | No plaintext fallback; CRUD records key ID and supports staged rewrap without weakening CAS. |
| P21.M2.E4.T003 Expose callback-scoped opaque leases at the GitHub HTTP boundary | 4 | P21.M2.E4.T002, P21.M1.E2.T001 | Only opaque handle, binding, and generation cross other boundaries. |
| P21.M2.E4.T004 Add generation-based CAS transitions and lease invalidation | 5 | P21.M2.E4.T002, P21.M2.E4.T003 | Stale live writes and leases cannot restore older authority. |
| P21.M2.E4.T005 Prove vault tamper restart deletion and rollback limits | 6 | P21.M2.E4.T004, P21.M2.E4.T008 | Test tamper, row replay, rotation, restore, and the explicit whole-store rollback limitation. |
| P21.M2.E4.T006 Define master-key source version and startup boundary | 4 | P21.M2.E4.T001 | Define external key source, key IDs, readiness, redacted diagnostics, and fail-closed startup. |
| P21.M2.E4.T007 Implement staged master-key rotation and resumable rewrap | 6 | P21.M2.E4.T002, P21.M2.E4.T006 | Rotate and rewrap crash-safely under CAS before retiring old key versions. |
| P21.M2.E4.T008 Define vault backup restore and key-compromise recovery | 5 | P21.M2.E4.T007 | Restore only with required keys and disabled authorization; compromise/loss requires destructive disable and relink. |

### M3 Rotation Revocation and Attributed Actions — 93h

#### E1 Refresh and Revocation Lifecycle — 17h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M3.E1.T001 Refresh expiring tokens using server-returned lifetimes | 4 | P21.M2.E2.T003, P21.M2.E3.T003, P21.M2.E4.T004 | Refresh on demand from returned lifetimes while advancing generation inside the same logical lineage. |
| P21.M3.E1.T002 Add durable single-flight refresh with CAS rotation | 5 | P21.M3.E1.T001 | Permit one remote refresh; late or replayed results cannot roll authority back. |
| P21.M3.E1.T003 Process GitHub App authorization revocation webhooks | 4 | P19.M2.E1.T004, P21.M2.E4.T004 | Verified idempotent revocation disables bindings, jobs, leases, and secrets. |
| P21.M3.E1.T004 Implement unlink and Principal or Connector removal invalidation | 4 | P21.M3.E1.T002, P21.M3.E1.T003, P21.M1.E1.T012 | Deny locally before remote revoke and break old logical lineage unconditionally. |

#### E2 Attribution Policy and Authorization Resolution — 32h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M3.E2.T001 Add attribution requirements and risk-tier defaults | 5 | P19.M1.E1.T002, P19.M4.E2.T004 | Every mutation declares App/User_required/User_preferred. |
| P21.M3.E2.T002 Resolve eligible accounts and first-use context preferences | 4 | P21.M1.E2.T003, P21.M2.E2.T003, P21.M2.E3.T003 | Apply explicit Room/Repo/Org precedence and prompt privately on ambiguity. |
| P21.M3.E2.T003 Resolve attribution authorization after all current policy checks | 5 | P21.M3.E2.T002, P21.M3.E1.T002, P19.M1.E2.T003 | Resolve a typed decision from frozen Tool, Principal, confirmation, binding, App/repo, Org/SSO, user, and live state. |
| P21.M3.E2.T004 Enforce visible App fallback and fail-closed user-required behavior | 4 | P21.M3.E2.T001, P21.M3.E2.T003 | Permit fallback only when policy and current preview name it; required work never falls back. |
| P21.M3.E2.T005 Add attribution previews receipts repair states and audit | 5 | P21.M3.E2.T003, P21.M3.E2.T004, P19.M4.E2.T004 | Record immutable actor evidence, requested/resolved mode, lineage, and actionable redacted reason. |
| P21.M3.E2.T006 Define P19 to P21 attribution migration and staged rollout | 5 | P21.M3.E2.T001, P21.M3.E2.T004, P19.M4.E2.T004 | Version the action state matrix and explicit pilot, production, rollback, and cleanup gates. |
| P21.M3.E2.T007 Issue opaque GitHub leases after final authorization revalidation | 4 | P21.M3.E2.T003, P21.M2.E4.T003, P21.M2.E4.T004, P19.M1.E2.T003 | Revalidate all revisions immediately before HTTP dispatch and expose only a callback-scoped lease. |

#### E3 P19 Action Integration — 44h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M3.E3.T001 Integrate user-preferred comments and metadata writes | 5 | P19.M4.E1.T003, P21.M3.E2.T005, P21.M3.E2.T006, P21.M3.E2.T007 | Use native user attribution or only explicitly previewed policy-permitted App fallback. |
| P21.M3.E3.T002 Integrate attributed reviewer requests and user-required PR reviews | 5 | P19.M4.E1.T004, P21.M3.E2.T005, P21.M3.E2.T006, P21.M3.E2.T007 | Keep reviewer requests user-preferred with only explicit policy-permitted App fallback; require a current user lease for PR review submission and decisions. |
| P21.M3.E3.T003 Preserve pinned attribution through delayed and background work | 6 | P19.M4.E2.T002, P21.M1.E3.T002, P21.M1.E3.T005, P21.M3.E2.T005, P21.M3.E2.T006, P21.M3.E2.T007 | Permit refresh-generation advance in one lineage; break on identity, binding, actor, or authority change. |
| P21.M3.E3.T004 Keep personal tokens out of automation runners shell and Git transport | 4 | P21.M3.E3.T003, P21.M3.E3.T008 | Keep user tokens out of runner, process, prompt, worktree, shell, and Git surfaces. |
| P21.M3.E3.T005 Reconcile resulting webhooks with native attribution receipts | 6 | P19.M2.E1.T004, P19.M4.E2.T004, P21.M3.E3.T001, P21.M3.E3.T002, P21.M3.E3.T003, P21.M3.E3.T006, P21.M3.E3.T007, P21.M3.E3.T008, P21.M3.E3.T009 | Match each action family to native actor and receipt exactly once without loops. |
| P21.M3.E3.T006 Integrate user-required lifecycle and creation actions | 5 | P19.M4.E2.T005, P21.M3.E2.T005, P21.M3.E2.T006, P21.M3.E2.T007 | Require the same current Principal's user lease for creation and close/reopen actions. |
| P21.M3.E3.T007 Integrate user-required typed workflow dispatch | 4 | P19.M4.E2.T006, P21.M3.E2.T005, P21.M3.E2.T006, P21.M3.E2.T007 | Bind actor, repository/ref, inputs, confirmation, and current lease at dispatch. |
| P21.M3.E3.T008 Integrate user-required code work and constrained PR creation | 5 | P19.M4.E2.T002, P19.M4.E2.T007, P21.M3.E2.T005, P21.M3.E2.T006, P21.M3.E2.T007 | Pin actor lineage while keeping personal tokens outside code runners and Git transport. |
| P21.M3.E3.T009 Integrate user-required merge attribution | 4 | P19.M4.E2.T003, P21.M3.E2.T005, P21.M3.E2.T006, P21.M3.E2.T007 | Require fresh user lease and live merge-policy validation immediately before merge. |

### M4 UX Operations Verification and Security Contract — 48h

#### E1 User and Admin Experience — 14h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M4.E1.T001 Add GitHub account CLI and redacted agent surfaces | 4 | P21.M3.E1.T004 | Link/list/status/use/relink/unlink are self-service and private. |
| P21.M4.E1.T002 Add admin enablement readiness and repair | 4 | P21.M4.E1.T001, P19.M2.E3.T004 | Admin enables capability; users authorize only themselves. |
| P21.M4.E1.T003 Add user-authorization diagnostics and metrics | 3 | P21.M4.E1.T001, P21.M3.E2.T005 | Distinguish policy, identity, authorization, delivery, and provider failures without secrets. |
| P21.M4.E1.T004 Preserve App PAT and minimal-build compatibility | 3 | P21.M3.E3.T005 | Existing explicit paths remain compatible; integrations disable cleanly in minimal builds. |

#### E2 Integration and Live Pilot — 25h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M4.E2.T001 Add cross-Connector and shared-Room integration coverage | 7 | P21.M4.E1.T001, P21.M4.E1.T002, P21.M4.E1.T003, P21.M4.E1.T004, P21.M1.E3.T004, P21.M3.E3.T001, P21.M3.E3.T002, P21.M3.E3.T003, P21.M3.E3.T004, P21.M3.E3.T005, P21.M3.E3.T006, P21.M3.E3.T007, P21.M3.E3.T008, P21.M3.E3.T009 | Cover every verified actor, merge/split, action family, delayed lineage, exclusion, and receipt isolation path. |
| P21.M4.E2.T002 Run token-leak restart concurrency and private-delivery verification | 7 | P21.M4.E2.T001, P21.M2.E4.T005, P21.M3.E3.T004 | Scan every persistence/execution surface and test key lifecycle, restore, CAS, and rollback limits. |
| P21.M4.E2.T003 Run a live Teams dual-attribution pilot | 8 | P21.M4.E2.T001, P21.M4.E2.T002, P21.M3.E2.T006, P19.M4.E3.T002 | Exercise staged rollout, separate web/device users, action families, lineage, rotation, denial, restart, and cleanup. |
| P21.M4.E2.T004 Publish redacted receipt backout cleanup and limitations | 3 | P21.M4.E2.T003 | Prove gate rollback, credential/binding destruction, no residual authority, and explicit limitations. |

#### E3 Architecture and Operator Documentation — 9h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P21.M4.E3.T001 Finalize Principal token-vault security boundary ADRs and glossary | 5 | P21.M4.E2.T004 | Finalize adapter trust, merge/split, lineage, key lifecycle, rollback, rollout, and confinement vocabulary. |
| P21.M4.E3.T002 Maintain implementation inventory and user-auth operator contract | 4 | P21.M4.E3.T001 | Keep task/source/schema/API/test crosswalk, safe defaults, repair, restore, and cleanup guidance current. |

## P22 inventory: GitHub Documentation and Feature Discovery

P22 depends on completed P19, P20, and P21. It has 18 tasks / 94h.

### M1 Complete GitHub Documentation — 34h

#### E1 GitHub Public and Machine Reference — 34h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P22.M1.E1.T001 Define canonical machine-readable GitHub coverage matrix | 4 | — | Define versioned stable surface IDs, evidence/task/doc fields, extractors, and shipped-state validation. |
| P22.M1.E1.T002 Publish GitHub setup routing and delivery documentation | 5 | P22.M1.E1.T005 | Publish matrix-backed setup, App/PAT, route/filter, delivery, migration, and example pages. |
| P22.M1.E1.T003 Publish GitHub machine references and update llms indexes | 5 | P22.M1.E1.T002, P22.M1.E1.T006 | Generate complete integration llms and llms-full coverage while preserving the llms.txt index contract. |
| P22.M1.E1.T004 Validate GitHub matrix and human-machine documentation coverage | 5 | P22.M1.E1.T001, P22.M1.E1.T003, P22.M1.E1.T005 | Deterministically compare matrix, code/backlog, tasks, evidence, public docs, machine refs, and links. |
| P22.M1.E1.T005 Populate the canonical GitHub coverage matrix from shipped evidence | 5 | P22.M1.E1.T001 | Populate repository-owned surfaces from code and completed leaf tasks; prose alone is not evidence. |
| P22.M1.E1.T006 Publish GitHub collaboration attribution and operations documentation | 6 | P22.M1.E1.T002, P22.M1.E1.T005 | Publish collaboration, linking, attribution, background, receipt, recovery, security, and rollout pages. |
| P22.M1.E1.T007 Validate Astro navigation orphan pages and deployment docs check | 4 | P22.M1.E1.T003, P22.M1.E1.T004, P22.M1.E1.T006 | Run one deterministic nav/orphan/link/generated-drift/Astro gate locally, in PR CI, and before Pages upload. |

### M2 Claude Tag Parity Documentation Audit — 22h

#### E1 Parity Crosswalk and Documentation Equivalence — 22h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P22.M2.E1.T001 Populate the machine-readable FT parity evidence crosswalk | 6 | P22.M1.E1.T003, P22.M2.E1.T004 | Record honest FT/P11-P18 status, current evidence, exclusions, and owned partials against the hashed source. |
| P22.M2.E1.T002 Close all shipped parity documentation gaps | 7 | P22.M2.E1.T001 | Give every implemented/partial entry public and machine docs without marketing missing work as shipped. |
| P22.M2.E1.T003 Add deterministic parity audit and reviewed receipt | 5 | P22.M2.E1.T002, P22.M2.E1.T004 | Check hashes/evidence read-only; source or status changes require an explicit dated reviewed receipt update. |
| P22.M2.E1.T004 Create a repository-owned hashed FT source manifest | 4 | P22.M1.E1.T003 | Check in stable FT IDs, normalized sources, provenance, hashes, version rules, and reviewed source snapshot. |

### M3 Shared Feature Catalog and Discovery — 38h

#### E1 Canonical Feature Material and Dual Renderers — 38h

| Task | Hours | Depends on | Outcome |
|---|---:|---|---|
| P22.M3.E1.T001 Define versioned feature catalog lifecycle schema and generation | 7 | P22.M1.E1.T004, P22.M2.E1.T002 | Define versions, lifecycle, audience, valid completed leaf tasks, checked-in generation, and deterministic removal semantics. |
| P22.M3.E1.T002 Populate GitHub and historical Claude Tag-parity batches | 6 | P22.M3.E1.T001, P22.M2.E1.T003, P22.M1.E1.T007 | Populate only audited completed work with lifecycle, audience, docs, actions, and leaf source tasks. |
| P22.M3.E1.T003 Add concise Connector-neutral new-features slash command | 5 | P22.M3.E1.T002 | Render bounded deterministic summaries for the verified audience without disclosure. |
| P22.M3.E1.T004 Add expansive clawq_help feature rendering | 5 | P22.M3.E1.T002, P22.M3.E1.T007 | Render expanded visible catalog material from verified Tool caller context, defaulting to public. |
| P22.M3.E1.T005 Verify shared material visibility generation and Connector fallbacks | 6 | P22.M3.E1.T003, P22.M3.E1.T004, P22.M3.E1.T007 | Prove identical ordered visible IDs/lifecycle/links across renderers and safe role/minimal fallbacks. |
| P22.M3.E1.T006 Make feature and documentation maintenance a repository contract | 5 | P22.M3.E1.T005, P22.M2.E1.T003, P22.M1.E1.T007 | Enforce one docs-check and complete catalog/matrix/parity/human/machine maintenance guidance. |
| P22.M3.E1.T007 Plumb verified caller audience role into clawq_help | 4 | P22.M3.E1.T001 | Carry trusted `public`, `authenticated-user`, `room-admin`, or `operator` role; missing evidence defaults public. |

## Follow-up boundary

Create a separate Idea for brokered authenticated Git pushes and verified
user/LLM commit authorship. It must cover token brokering without runner/shell
exposure, explicit email consent, signing and audit, and whether a display such
as `{Name} + LLMs <email>` is desirable. P21 does not solve Git transport.

## Verification and rollout

- The migration state matrix preserves existing explicit App/PAT reads and
  compatible automation. P19 high-risk App attribution remains a named,
  time-bounded pilot gate that is off by default; P21 production enablement is a
  separate audited transition after verified actors, Principal/account state,
  private delivery, vault/key lifecycle, attribution policy, repair, and backout
  readiness pass. Rollback restores the safe disabled state without actor-mode
  substitution.
- Unit and adapter tests cover Teams OpenID/JWKS/claim/tenant/`serviceUrl`/
  endorsement/key-rotation checks; Slack, Discord, Telegram, web, CLI, and
  direct-session trust; cross-tenant collisions; deterministic merge/adoption;
  conflict, tombstone, unlink/split/revoke; account precedence; PKCE/callback;
  shared web/device activation; device polling; and redaction.
- Vault tests distinguish token generation from master-key version and cover
  single-flight refresh, row swap, live stale writes, lease invalidation,
  staged crash-resumable rewrap, backup restore, key loss/compromise, destructive
  recovery, and the documented inability to detect an internally consistent
  whole-store rollback without an external monotonic anchor.
- Integration tests cover two linked users and one unlinked user in a shared
  Room, every action family, safe App fallback, `User_required` denial, ordinary
  refresh within pinned logical lineage, lineage-breaking merge/split/relink/
  revoke, personal-token exclusion from runners/shell/Git, native attribution
  receipts, verified webhook reconciliation, restarts, minimal builds, and no
  cross-Principal borrowing.
- The live Teams pilot exercises separate web/device users, the staged rollout
  gate, all attribution modes and action families, delayed refresh lineage, key
  rotation, SSO/permission denial, revoke, restart, webhook correlation, and
  cleanup. Its redacted receipt records limitations, returns the gate to the safe
  default, destroys pilot credentials/bindings, and proves no residual authority.
- P22 check mode deterministically validates the repository-owned GitHub matrix,
  FT source hashes and parity crosswalk, catalog lifecycle/audience/source tasks,
  generated runtime drift, llms structure/links, shared renderer visibility,
  navigation/orphan pages, and the Astro build. The same `docs-check` runs in PR
  CI and before Pages upload. Evidence refresh is a separate explicit update
  mode that writes a reviewed dated receipt rather than changing check output.

## Official protocol references

- https://docs.github.com/en/apps/creating-github-apps/authenticating-with-a-github-app/generating-a-user-access-token-for-a-github-app
- https://docs.github.com/en/apps/creating-github-apps/authenticating-with-a-github-app/refreshing-user-access-tokens
- https://docs.github.com/en/apps/creating-github-apps/registering-a-github-app/choosing-permissions-for-a-github-app
- https://docs.github.com/en/webhooks/webhook-events-and-payloads#github_app_authorization
- https://docs.github.com/en/enterprise-cloud@latest/apps/using-github-apps/saml-and-github-apps

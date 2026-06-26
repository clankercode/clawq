# Room Agent Profiles Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add Claude Tag-style room agents to Clawq: shared profiled room sessions, thread-bound work sessions, persistent room workspaces, scoped memory, governance, routines, and ambient follow-up.

**Architecture:** A profiled room is a shared channel/conversation bound to a named room-agent profile. The room profile owns identity, model/template, tool grants, scoped memory, persistent CWD, budgets, scheduled routines, and audit policy. Long-running or substantial work runs in child thread-bound work sessions so the shared room identity remains stable while each task keeps clean lineage.

**Tech Stack:** OCaml 5.1, Dune, SQLite via existing `Memory`, connector integrations in `clawq_runtime_integrations`, core policy/schema in `clawq_runtime_core`, existing task tree/background task/cron/request stats/audit subsystems.

---

## Planning Inputs

Primary local inputs:
- `/home/xertrov/.clawq/workspace/docs/research/claude-tag-vs-clawq.md`
- `/home/xertrov/.clawq/workspace/docs/research/claude-tag-gap-index.json`
- Current `bl tree` state as of 2026-06-26.
- Repository code around `runtime_config`, `session`, `task_tree`, `background_task`, `memory`, `request_stats`, `audit`, `slack`, `teams`, `discord`, and cron/slash command handling.

External product inputs:
- Anthropic Claude Tag announcement and docs around overview, lifecycle, agent identity, security/data, memory, and routines.

Important local decisions already made:
- Profiled rooms use **shared room sessions** by default.
- Thread-bound work sessions are still valuable, but as child work sessions under a shared room profile.
- Use multiple backlog phases.
- Scheduled tasks must integrate with and extend the existing cron system.
- Scoped memory should use first-class scoped memory tables rather than relying on namespaced global keys.
- Profiled rooms get a persistent dedicated CWD, likely under `~/.clawq/workspace/rooms/<connector>/<slug>-<hash>/`.
- V1 room profiles are **1 room : 1 profile**. Multi-room profiles are deferred because per-room workspace, budget, and memory state would otherwise collide.
- Slack is the first end-to-end MVP connector. Teams gets a capability audit and thread-like reply design early, then hardening as a fast-follow.
- P11 must include a privacy guard that disables unscoped global memory/search injection for profiled rooms until P12 scoped memory is complete.

## Terms

| Term | Meaning |
|---|---|
| Room | A shared connector conversation, for example Slack channel, Teams conversation, Discord channel, Matrix room, Telegram group, or equivalent. |
| Profiled room | A room with an explicit Clawq room-agent profile bound to it. |
| Room-agent profile | A durable config/state object defining identity, model, template, tools, memory grants, budget, CWD, routines, and audit behavior for one room. |
| Shared room session | The persistent session key used by the room agent itself. Everyone allowed in the room steers this same session. |
| Thread-bound work session | A child session created for one thread/request/task beneath the room profile. |
| Room workspace | A persistent filesystem directory for the room agent's local work products and state. |
| Routine | A scheduled job, watch, or recurrence that runs as the room profile through the existing cron machinery. |

## Target Session Model

| Layer | Example Key Shape | Purpose | Context Owner |
|---|---|---|---|
| Shared room session | `slack:C456:room`, `teams:t1:19_xyz_thread_v2:room` | The persistent agent identity for a profiled room. | Room profile |
| Thread work session | `slack:C456:thread:1700000000.000100` | Focused work session for one thread/task. | Room profile plus thread/task origin |
| Routine session | `slack:C456:routine:daily-standup` | Scheduled recurring work. | Room profile plus routine definition |

Implementation requirements:
- Keep exact key construction and parsing centralized in a new `room_session.ml` module. Do not duplicate ad hoc string formatting across connectors.
- `room_session.ml` must parse keys into typed variants such as `Personal`, `Room`, `Thread`, and `Routine`.
- Existing positional parsers such as `Restart_notify.parse_channel_from_key` must route through the new parser. Multi-segment keys must not silently truncate the connector identity.
- Add round-trip parser tests for room/thread/routine keys across Slack, Teams, Discord, Telegram, and a generic connector.

## Target Workspace Model

| Scope | Suggested Path | Notes |
|---|---|---|
| Room | `~/.clawq/workspace/rooms/<connector>/<slug>-<hash>/` | Default effective CWD for the shared room session. |
| Thread | `~/.clawq/workspace/rooms/<connector>/<slug>-<hash>/threads/<thread-slug>-<hash>/` | Optional CWD for a thread-bound work session. |
| Task | `~/.clawq/workspace/rooms/<connector>/<slug>-<hash>/tasks/<task-id>/` | Optional CWD for durable task artifacts. |
| Routine | `~/.clawq/workspace/rooms/<connector>/<slug>-<hash>/routines/<routine-id>/` | Persistent state for scheduled jobs. |

Path requirements:
- Use a readable slug plus a stable short hash of connector identity.
- Use at least 12 hex chars of hash over the full connector/workspace/team/room identity, and fail closed if a stored identity mismatches the path identity.
- Never place raw service URLs or unbounded channel names directly into a path.
- Create directories lazily on profile bind or first use.
- Store declarative workspace policy in config and the resolved materialized path in DB so later renames do not orphan state.
- Validate explicit `workspace_dir` overrides on bind: resolve realpaths, reject symlink/traversal escapes, and require containment under `~/.clawq/workspace/rooms/` unless the path is explicitly allowed by `allowed_cwd_patterns` or `extra_allowed_paths`.
- Any `workspace_dir` override is admin-only.

## V1 Policy Decisions

| Topic | V1 Decision |
|---|---|
| Room/profile cardinality | Strict 1 room : 1 profile. One profile cannot be bound to several rooms until per-binding memory/budget/workspace state exists. |
| Profile source of truth | Config is the declarative source. DB stores materialized bindings, resolved workspace paths, and runtime lifecycle state. Config reload reconciles explicit changes and refuses ambiguous duplicate bindings. |
| CWD precedence | Explicit `/repo` session CWD > resolved room workspace from DB > profile workspace default from config > `agent_template.cwd` > global workspace. A fresh room session starts at the room workspace; explicit `/repo` changes persist until reset or rebind policy clears them. |
| Admin surfaces | Bind/unbind/rebind/delete profiles, memory-grant changes, codebase grants, budget changes, routine creation/triggering, and ledger reads are admin-only unless a task explicitly defines a narrower safe read path. |
| Guest surfaces | Guests may steer ordinary room turns and request async work only if the profile permits it. Guests cannot mutate profile policy, CWD policy, grants, budgets, or codebase access. |
| Memory grant graph | Default deny. Grants are explicit and non-transitive. Thread and routine scopes inherit their parent room scope; all other cross-scope reads require a direct grant. V1 has no deny rules; absence of allow means deny. |
| Budget window | Calendar month in UTC for V1. Thread and routine child sessions roll up into the parent profile budget. |
| Ledger storage | Redact/reference-only by default. Full prompts are not stored in the room ledger unless an explicit admin policy enables it. |
| Ambient mode | Disabled by default, opt-in per profile, rate-limited, quiet-hours aware, and scoped to the room's memory/history grant. |

## Connector Support Matrix

| Connector | P11 Shared Room Session | P11 Thread Work | P13 Ambient History | Notes |
|---|---|---|---|---|
| Slack | Full MVP target | Full MVP target via `thread_ts` | Later | First E2E connector because current sessions are user-scoped and thread support is net-new. |
| Teams | Shared conversation support/audit | Design + reply-chain audit first, hardening later | Later | Existing `conversation_id`, `activity_id`, and `reply_to_id` provide thread-like reply substrate for some rooms. |
| Discord | Parser/key support and later implementation | Degraded or later thread/channel support | Later | Must be scoped after Slack unless explicitly promoted. |
| Telegram | Shared group support later | Degraded fallback unless message-thread-id support is added | Later | Thread-less fallback must be deterministic and must not fork a new child session on replay. |
| Matrix | Shared room support later | Degraded fallback initially | Later | No thread substrate assumed in this plan. |
| Other connectors | Generic typed-key support only | No-op/degraded until connector capability says otherwise | No-op/degraded | Capability matrix decides delivery behavior. |

Thread-less fallback:
- If a connector lacks durable thread identity, async work should use a deterministic task/session key derived from the source message id or durable queue id.
- If no stable source id exists, fall back to the shared room session plus task id, not a synthesized random thread id.
- The user-facing message should say that the connector cannot keep replies in a native thread.

## Recommended Phase Structure

| Phase | Goal |
|---|---|
| P11: Room Agent MVP | Shared room profiles, room CWDs, shared session routing, thread metadata, and mention-to-task MVP. |
| P12: Room Agent Governance | Scoped memory tables, grants, tool/codebase policy, budgets, and activity ledger. |
| P13: Ambient and Scheduled Room Agents | Cron-integrated routines, ambient watcher, stalled-thread follow-up, and multi-connector polish. |

Do not ingest this plan into `bl` until review is complete.

P7 dependency scope:
- P11.M1-P11.M3 can proceed without waiting on all P7 queue replay work.
- P11.M4 mention-to-task async delivery and P13 routines/ambient work should depend on the P7 replay/failure-handling terminal tasks, because they rely on durable delivery and restart behavior.

## Existing Backlog To Fold In

These should be referenced in task bodies when ingested, not blindly closed:
- `I028`: tie agents into background tasks, slash commands, and `@agent`.
- `I030`: cross-channel thread continuity.
- `I037`: channel-specific personas.
- `I049`: per-channel config overrides.
- `I029`: verified policy engine.
- `I050`: tool call proof certificates.
- `I055`: remote runner question relay.
- `B255`: Claude review delegates and nested Claude session prohibition.
- `B430`: moving interrupted shell work to background.
- `B424`, `B457`, `B499`: Teams slash/card/tool visibility issues.

Prerequisite to respect:
- P7 persisted inbound queue startup replay and failure handling should finish before room-agent async behavior is relied on heavily.

## Core Architecture Boundaries

| Concern | Belongs In | Rationale |
|---|---|---|
| Profile types, pure resolution, DB schema, memory policy | `clawq_runtime_core` | Needed by CLI, tests, and minimal/shared runtime code. |
| Slack/Teams/Discord/Telegram thread handling | `clawq_runtime_integrations` | Connector/network behavior is integration-only. |
| Minimal build behavior | `command_bridge_min.ml` stubs or read-only inspection | Minimal build must not acquire network/server dependencies. |
| Tool grant filtering | Existing tool registry/template filtering path plus room policy | Must preserve global security and sandbox checks. |
| Scheduled routines | Existing cron commands/storage/execution extended with room profile fields | Avoid parallel scheduler. |

## Phase P11: Room Agent MVP

### Milestone P11.M1: Room Profile Foundation

Objective: define and persist room profiles without changing existing unprofiled connector behavior.

Epics:

| Epic | Outcome |
|---|---|
| P11.M1.E1 Profile Schema | Config/DB types exist for room profiles and bindings. |
| P11.M1.E2 Room Workspace | Profiles get stable persistent CWD paths. |
| P11.M1.E3 Shared Room Session Keys | Connectors can construct and parse profiled room session keys safely. |
| P11.M1.E4 CLI/Admin Surface | Operators can list/show/bind/unbind profiles safely. |
| P11.M1.E5 Profile Lifecycle | Unbind/rebind/rename/delete semantics do not leak state across profiles. |

Task candidates:

1. Add room profile types.
   - Files likely touched: `src/runtime_config_types.ml`, `src/runtime_config.ml`, `src/config_loader.ml`, `test/test_config_loader.ml`.
   - Acceptance:
     - Config supports profile definitions with `id`, `display_name`, `connector`, `room_id`, `agent_template`, `model`, `workspace_dir_policy`, `memory_scope_id`, `allowed_tools`, `denied_tools`, `budget`, `ambient_enabled`, `audit_policy`.
     - Config rejects duplicate room bindings and duplicate profile ids.
     - V1 validation rejects a single profile bound to more than one room.
     - Config round-trips through existing JSON serialization.
     - Empty profile list preserves current behavior.

2. Add DB schema for profile bindings.
   - Files likely touched: `src/memory_0_schema.ml`, `src/memory.ml`, `test/test_memory.ml`.
   - Acceptance:
     - `room_profiles` and `room_profile_bindings` can be initialized idempotently.
     - Bindings can map connector + team/workspace + room/conversation id to one profile id.
     - The schema enforces or checks V1 one-room-one-profile cardinality.
     - Existing DBs migrate without data loss.

3. Add stable room workspace path generation.
   - Files likely touched: new `src/room_workspace.ml`, `src/dune`, `test/test_room_workspace.ml`.
   - Acceptance:
     - Slug/hash paths are deterministic.
     - Unsafe characters are removed or encoded.
     - Raw service URLs do not appear in paths.
     - Hash length is at least 12 hex chars and includes connector + workspace/team + room identity.
     - Traversal strings, symlink escapes, control characters, overlong names, and slug collisions are covered by adversarial tests.
     - Explicit overrides are rejected unless realpath containment passes under the rooms root or existing allowed CWD policy.
     - Directory creation is lazy and uses mode compatible with existing workspace handling.

4. Add profile-aware shared room session key helpers and parser.
   - Files likely touched: new `src/room_session.ml`, `src/dune`, `test/test_room_session.ml`.
   - Acceptance:
     - Helpers generate stable shared room keys for Slack, Teams, Discord, Telegram, and generic connectors.
     - Helpers generate child thread/routine keys.
     - Parser returns typed variants and preserves all connector identity components.
     - `Restart_notify.parse_channel_from_key` and other existing multi-segment key consumers use the new parser or are explicitly proven safe.
     - Regression tests show `slack:C456:room` parses as channel `slack` and room `C456`, not as a truncated positional fragment.
     - Teams unsanitized and sanitized conversation ids round-trip without ambiguity.
     - Existing personal/session key helpers remain available for unprofiled rooms.

5. Add CLI/admin inspection.
   - Files likely touched: `src/command_bridge.ml`, `src/command_bridge_min.ml`, `src/main.ml`, `test/test_main.ml`, `test/test_command_bridge*.ml`.
   - Acceptance:
     - `clawq rooms list`, `clawq rooms show <profile>`, and `clawq rooms bind ...` or equivalent are available in full build.
     - Mutating commands are admin-gated through the existing admin primitive, not only hidden by UX.
     - Minimal build returns the standard "disabled in minimal build" message for mutation paths.
     - Admin-facing errors identify missing profile, duplicate binding, and invalid connector.

6. Specify and implement profile lifecycle operations.
   - Files likely touched: room profile DB/API module, command bridge, tests.
   - Acceptance:
     - Unbind stops new room routing but preserves history, ledger, memory scope, and workspace by default.
     - Rebind refuses to attach a room to a different profile unless the admin chooses preserve or reset semantics explicitly.
     - Rename keeps stable profile ids and only changes display names.
     - Delete is soft-delete by default and cannot remove a profile with active tasks/routines unless forced by an admin command with a visible consequence summary.
     - Workspace GC is retention-based and never deletes directories with active task, routine, or ledger references.

### Milestone P11.M2: Profile Resolution In Connectors

Objective: apply room profiles at turn setup while leaving unprofiled behavior unchanged.

Epics:

| Epic | Outcome |
|---|---|
| P11.M2.E1 Connector Resolution | Slack/Teams/Discord/Telegram resolve room profiles. |
| P11.M2.E2 Effective CWD | Profiled turns use the room workspace as effective CWD. |
| P11.M2.E3 Template/Model Selection | Room profile model/template overrides apply predictably. |
| P11.M2.E4 Privacy Guard | P11 profiled rooms do not inherit unscoped global memory/search context. |

Task candidates:

1. Resolve profiles for connector turns.
   - Files likely touched: `src/slack.ml`, `src/teams.ml`, `src/discord.ml`, `src/telegram.ml`, `src/session_turn.ml`, `src/session_core.ml`.
   - Acceptance:
     - If a room binding exists, addressed room messages use the shared room session key.
     - If no binding exists, current session key behavior remains unchanged.
     - Slack is the first complete E2E connector; Teams support is initially limited to capability audit and thread-like reply design unless explicitly scheduled later.
     - Tests cover profiled and unprofiled Slack paths and at least one parser/capability test for Teams.

2. Set room CWD on profiled turns.
   - Files likely touched: `src/session_turn.ml`, `src/session_core.ml`, `src/memory.ml`, connector handlers.
   - Acceptance:
     - Shared room session has persistent `effective_cwd`.
     - `task_tree`, `shell_exec`, file tools, and background task defaults see the room CWD through existing context plumbing.
     - CWD precedence follows the V1 policy table.
     - `/repo` remains explicit, persists in `session_state.effective_cwd`, and cannot point outside profile/global CWD policy.
     - Background tasks use the room CWD for choosing the source repo/default prompt context, while the running task still executes in its own worktree when worktrees are enabled.

3. Apply room model/template selection.
   - Files likely touched: `src/session_core.ml`, `src/agent_router.ml`, `src/agent_template.ml`.
   - Acceptance:
     - Room profile model is a new precedence tier in the existing channel path, not an existing template behavior.
     - Precedence is explicit: session override > room profile > channel default > global default, with template model behavior defined separately for subagents/background tasks.
     - Room profile agent template applies system prompt/tool filter.
     - Existing `agent_bindings` behavior is either preserved or explicitly ordered below room profile resolution.
     - Profile-selected Anthropic OAuth/Claude runner usage remains blocked unless `security.allow_anthropic_oauth_inference` permits it.

4. Add P11 privacy guard for profiled rooms.
   - Files likely touched: `src/agent.ml`, `src/agent_0_compact.ml`, memory/search call sites, tests.
   - Acceptance:
     - Profiled room turns do not use unscoped `Memory.search` across all messages.
     - Profiled room turns do not inject raw global/core memories before scoped memory is implemented.
     - Compaction and memory-flush paths do not reintroduce global core-memory reads for profiled rooms.
     - Tests seed Channel A and Channel B history/memory and prove Channel A's profiled room prompt cannot surface Channel B content.

### Milestone P11.M3: Thread-Bound Work Sessions

Objective: support connector thread metadata and child work sessions for substantial tasks.

Epics:

| Epic | Outcome |
|---|---|
| P11.M3.E1 Thread Metadata | Connectors preserve source message/thread identity. |
| P11.M3.E2 Child Work Sessions | Room profiles can spawn thread-bound work sessions. |
| P11.M3.E3 Thread Reply Routing | Progress/completion can reply to the original thread where supported. |

Task candidates:

1. Add generic origin metadata type.
   - Files likely touched: new `src/room_origin.ml`, `src/dune`, `test/test_room_origin.ml`.
   - Acceptance:
     - Origin captures connector, workspace/team id, room id, requester id/name, message id, thread id, service URL where required, and profile id.
     - Origin can be serialized for task/background/ledger rows.

2. Add Slack thread support.
   - Files likely touched: `src/slack.ml`, `test/test_slack.ml`.
   - Acceptance:
     - Slack event parser preserves `thread_ts` when present.
     - Send helpers can include `thread_ts` for thread-bound replies.
     - Ordinary channel sends remain unchanged.

3. Audit and extend Teams thread-like support.
   - Files likely touched: `src/teams.ml`, `test/test_teams.ml`.
   - Current substrate:
     - Teams has `conversation_id`, `activity_id`, `reply_to_id`, `send_reply`, `edit_activity`, and per-conversation throttling.
   - Acceptance:
     - Plan documents which Teams room types support replies/threads through Bot Framework activity replies.
     - Origin metadata stores `activity_id` as source message id and uses `reply_to_id` for reply paths where supported.
     - Tests prove reply payloads include the expected activity target.

4. Add thread-bound session creation.
   - Files likely touched: `src/session_core.ml`, `src/session_turn.ml`, `src/task_tree*.ml`, `src/background_task*.ml`.
   - Acceptance:
     - A profiled room request can create a child thread session key.
     - Child session inherits profile model/template/tool/memory grants.
     - Child session uses thread/task CWD beneath the room workspace when configured.

### Milestone P11.M4: Mention-To-Task MVP

Objective: turn a substantial profiled room mention into durable visible async work.

Epics:

| Epic | Outcome |
|---|---|
| P11.M4.E1 Request Classification | Room mention can stay quick reply or become a task. |
| P11.M4.E2 Task/Background Binding | Task tree and background task rows carry room origin. |
| P11.M4.E3 Completion Delivery | Completion/failure posts back to originating thread/room. |
| P11.M4.E4 Concurrency And Restart | Async routing remains stable under concurrent messages and daemon restart. |

Task candidates:

1. Add room task origin columns.
   - Files likely touched: `src/task_tree_core.ml`, `src/background_task.ml`, `test/test_task_tree.ml`, `test/test_background_task.ml`.
   - Acceptance:
     - Rows can store `profile_id`, `origin_json`, `thread_id`, and requester summary.
     - Existing rows migrate safely.
     - Rendered task output shows concise origin when useful.

2. Add request-classification and mention-to-task command path.
   - Files likely touched: connector handlers, `src/slash_commands*.ml`, `src/task_tree.ml`, `src/background_task_tools.ml`.
   - Acceptance:
     - The classifier is deterministic and testable: explicit async commands always create tasks; short direct questions stay quick replies; configured keywords or slash/command forms can force either path.
     - Classification runs before entering the long shared-room turn where possible, so obvious async work does not sit behind a busy room mutex.
     - A profiled room user can explicitly request async work.
     - Created task links to source room/thread and starts a local/native/background agent according to profile policy.
     - Non-profiled rooms keep existing behavior.
     - Guest async requests are allowed only when the profile policy permits them.

3. Add concise progress states.
   - Files likely touched: `src/status_message*.ml`, connector handlers, `src/daemon_util.ml`.
   - Acceptance:
     - Shared room receives sparse state transitions: accepted, planned, working, blocked, completed, failed, needs input.
     - Full logs/transcripts remain available by command rather than dumped into the room.
     - Connectors with edit support update in place where practical.
     - Progress state is persisted as non-blocking best-effort room activity/task metadata with indexes for profile, thread, status, and updated time.

4. Add concurrency and restart/replay tests.
   - Files likely touched: `test/test_daemon.ml`, `test/test_restart.ml`, `test/test_session_persistence.ml`, `test/test_background_task.ml`.
   - Acceptance:
     - Two concurrent messages to a shared room do not corrupt history, CWD, or origin metadata.
     - A long-running room task can complete after daemon restart and deliver once to the original room/thread when the connector target still exists.
     - Missing/deleted connector targets produce a ledger/task error without infinite retry.

## Phase P12: Room Agent Governance

### Milestone P12.M1: Scoped Memory Tables

Objective: replace global memory access in room-agent paths with first-class scoped memory.

Epics:

| Epic | Outcome |
|---|---|
| P12.M1.E1 Scoped Schema | Dedicated tables define scopes, entries, grants, and provenance. |
| P12.M1.E2 Scoped APIs | Runtime code uses scoped memory APIs with actor context. |
| P12.M1.E3 Migration | Existing `core_memories` migrate into a legacy/default scope. |
| P12.M1.E4 Prompt Injection | Automatic memory injection respects scope grants. |

Recommended schema shape:

| Table | Responsibility |
|---|---|
| `memory_scopes` | Scope id, kind, owner/profile/session ids, visibility, created/updated timestamps. |
| `scoped_memories` | Key/content/category/provenance bound to one scope. |
| `memory_grants` | Explicit read/write/admin grants from profile/session/scope to another scope. |
| `memory_scope_events` | Optional later audit/event table for writes, updates, deletes, and grant changes. |

Migration requirements for every schema task:
- Coordinate one schema version bump per milestone. Current schema version must be checked at implementation time.
- Add new tables to `init_*` and `ensure_all_tables`.
- Add new columns through `migrate_step` and `repair_missing_columns`.
- Prefer read-in-place legacy scope binding for existing `core_memories`; if copying rows, do it in a version-gated transaction with a sentinel/idempotency guard.
- Add double-init tests proving no duplicated legacy rows or duplicate scope bindings.
- State migrations are forward-only and recommend DB snapshot before first run on user data.

Task candidates:

1. Add scoped memory schema.
   - Files likely touched: `src/memory_0_schema.ml`, `src/memory.ml`, `test/test_memory.ml`.
   - Acceptance:
     - Schema initializes idempotently.
     - V1 scope kinds include `personal`, `room`, `thread`, `workspace`, and `legacy`; `repo` arrives with codebase grants and `org` waits until an org entity exists.
     - FTS/search works with scope filtering.

2. Add scoped memory APIs.
   - Files likely touched: `src/memory.ml`, `src/tools_builtin_io.ml`, `test/test_memory.ml`, `test/test_tools.ml`.
   - Acceptance:
     - `store_scoped`, `recall_scoped`, `list_scoped`, and `forget_scoped` require actor/profile/session context.
     - Channel A cannot recall/list Channel B memory without a grant.
     - Private room memory is not visible to workspace scope.
     - Grant resolution is default-deny, non-transitive, direct-allow only, with thread/routine scopes inheriting parent room grants.
     - Grant creation and deletion are admin-only and never exposed as an unrestricted agent tool.

3. Migrate legacy core memories.
   - Files likely touched: `src/memory_0_schema.ml`, `src/memory.ml`, migration tests.
   - Acceptance:
     - Existing `core_memories` are read through or bound into `legacy/global` or `workspace/default` without creating two divergent memory backends.
     - Read-in-place is preferred over copying. If copying is chosen, it is version-gated, transaction-wrapped, and sentinel-guarded.
     - Existing explicit CLI memory commands remain usable through a deliberate legacy/default scope.
     - No runtime room-agent path reads raw global memories directly.

4. Update automatic prompt injection.
   - Files likely touched: `src/agent.ml`, `src/agent_0_compact.ml`, `src/memory.ml`, `test/test_agent*.ml`, `test/test_memory_search.ml`.
   - Acceptance:
     - Room sessions inject room/profile-granted memories only.
     - Thread work sessions inherit room grants plus thread scope.
     - `Memory.search` accepts a session/scope filter for profiled room turns and does not search every session's messages.
     - All known global core-memory read paths are routed through scoped APIs or an explicit admin/legacy path.
     - Tests prove unrelated channel memories are absent from prompts.

### Milestone P12.M2: Profile Policy And Tool Grants

Objective: enforce room profile grants without bypassing global security.

Epics:

| Epic | Outcome |
|---|---|
| P12.M2.E1 Tool Grants | Room profiles filter tool registry. |
| P12.M2.E2 Codebase Grants | Room profiles can grant named repos/codebases. |
| P12.M2.E3 Policy Errors | Denied access is actionable and auditable. |

Task candidates:

1. Apply tool grants.
   - Files likely touched: `src/agent_template.ml`, `src/tools_builtin.ml`, `src/session_core.ml`, tests for tool registry.
   - Acceptance:
     - Room profile allowed/denied tools combine with template filters.
     - Global `security.tools_enabled`, sandbox, `workspace_only`, and Anthropic OAuth opt-in remain stronger than profile grants.
     - Denied tools return a clear message.

2. Add codebase profile grants.
   - Files likely touched: runtime config types/loaders, background task enqueue paths, docs.
   - Acceptance:
     - Profile can grant named repo/codebase entries.
     - Background tasks started from room profile use granted repo policy.
     - Codebase grants intersect with, and never expand beyond, global `workspace_only`, `allowed_cwd_patterns`, sandbox, and extra-path policy.
     - A rejection test proves a profile cannot grant a repo path denied by global security policy.
     - Raw filesystem grants are not required for ordinary codebase tasks.

### Milestone P12.M3: Budgets And Activity Ledger

Objective: make room-agent activity governable and inspectable.

Epics:

| Epic | Outcome |
|---|---|
| P12.M3.E1 Budget Config | Profiles can define soft/hard spend ceilings. |
| P12.M3.E2 Pre-call Enforcement | LLM calls respect profile budgets. |
| P12.M3.E3 Activity Ledger | Admins can inspect room-agent work lineage. |

Task candidates:

1. Add budget fields and usage lookup.
   - Files likely touched: `src/runtime_config_types.ml`, `src/request_stats.ml`, `src/memory_0_schema.ml`, `src/agent.ml`, `src/debate.ml`, config tests.
   - Acceptance:
     - Profile budget can be configured by UTC calendar month.
     - Usage can be queried by profile/session/room, and child thread/routine sessions roll up to the parent profile.
     - Attribution is explicit, either through a `profile_id` request stats column or a documented prefix-aggregation scheme backed by tests.
     - Soft and hard limits are distinguishable.

2. Enforce hard budgets before provider calls.
   - Files likely touched: `src/agent.ml`, `src/session_turn.ml`, provider call wrapper path, tests.
   - Acceptance:
     - Hard limit blocks before each provider invocation inside both `agent.ml` iterative loops; a coarse turn-entry pre-check may be added but is not the only gate.
     - Soft limit emits an admin-targeted warning once per period threshold crossing.
     - Concurrent turns serialize check+reserve or document and test a bounded overshoot.
     - Budget failures do not leak private prompt content.

3. Add room activity ledger.
   - Files likely touched: `src/memory_0_schema.ml`, new `src/room_activity.ml`, command/slash/admin views.
   - Acceptance:
     - Ledger ties requester, profile, room/thread, task id, background task id, model/provider, tool names, token/cost summary, artifact pointers, and final status.
     - Admins can filter by room/profile/date/requester.
     - Sensitive payloads are redacted or stored as references according to profile audit policy, with redact/reference-only as the default.
     - Ledger writes are non-blocking best effort and failures do not fail the user turn.
     - Retention/export either reuses `audit.ml` policy or defines equivalent admin-visible retention behavior.

## Phase P13: Ambient And Scheduled Room Agents

### Milestone P13.M1: Cron-Integrated Room Routines

Objective: extend existing cron rather than creating a second scheduler.

Epics:

| Epic | Outcome |
|---|---|
| P13.M1.E1 Routine Model | Cron jobs can target room profiles and optional threads. |
| P13.M1.E2 Routine CWD | Routines use persistent room/routine workspaces. |
| P13.M1.E3 Routine Delivery | Routine output posts to configured room/thread. |

Task candidates:

1. Extend cron job metadata.
   - Files likely touched: `src/scheduler.ml`, `src/command_bridge.ml`, `src/slash_commands.ml`, `src/slash_commands_fmt.ml`, `test/test_scheduler.ml`, `test/test_setup_cron.ml`.
   - Acceptance:
     - Cron jobs can store `profile_id`, optional `thread_id`, and routine workspace id.
     - Existing cron jobs continue to run unchanged.
     - `cron list/show/history` displays room profile target when present.
     - Additive nullable columns follow the existing scheduler migration pattern.

2. Run cron jobs as room profiles.
   - Files likely touched: `src/scheduler.ml`, daemon scheduler/cron execution path, `src/session_turn.ml`, `src/room_session.ml`.
   - Acceptance:
     - Routine resolves profile to session key at tick time through `room_session.ml` and does not store stale resolved keys.
     - Routine turn uses shared room session or routine child session as configured.
     - Routine inherits profile model/template/tools/memory/budget.
     - Routine uses room/routine CWD.
     - Output and failures are recorded in existing cron history/run records so loop auto-disable behavior still works.

3. Add routine admin surface.
   - Files likely touched: slash command parsing/formatting, command bridge.
   - Acceptance:
     - Operators can create/edit/remove room routines.
     - Routine creation, editing, manual triggering, and deletion are admin-gated.
     - Invalid profile or unbound room fails with actionable error.
     - Triggering a routine manually uses the same path as scheduled execution.

### Milestone P13.M2: Ambient Room Watcher

Objective: add opt-in proactive room behavior with strict quietness and grants.

Epics:

| Epic | Outcome |
|---|---|
| P13.M2.E1 Connector History Coverage | Room watcher has safe recent room context. |
| P13.M2.E2 Stalled Thread Detection | Watcher can identify unresolved threads/tasks. |
| P13.M2.E3 Rate Limits And Quiet Hours | Ambient behavior remains quiet and configurable. |

Task candidates:

1. Extend connector history for profiled rooms.
   - Files likely touched: `src/connector_history.ml`, `src/slack.ml`, `src/teams.ml`, `src/discord.ml`, tests.
   - Acceptance:
     - Recent unaddressed room messages can be recorded for profiled rooms according to profile policy.
     - Slack reaches parity with Teams/Discord where feasible.
     - Private rooms and restricted channels obey grants.
     - Retention is bounded and aligned with the room memory/history policy.

2. Add stale task/thread watcher.
   - Files likely touched: daemon scheduler, task tree queries, room activity.
   - Acceptance:
     - Watcher can find blocked/in-progress room tasks with no recent activity using a configured stale-after duration.
     - Watcher can post a concise follow-up or ask for input.
     - Watcher stays silent when nothing materially changed.

3. Add ambient safety controls.
   - Files likely touched: profile config, daemon scheduler, tests.
   - Acceptance:
     - Ambient mode is disabled by default.
     - Per-profile quiet hours and rate limits are enforced.
     - Admin can inspect last ambient decisions without exposing unauthorized content.

### Milestone P13.M3: Multi-Connector Polish

Objective: make room agents reliable beyond Slack.

Epics:

| Epic | Outcome |
|---|---|
| P13.M3.E1 Connector Capability Matrix | Each connector declares thread/edit/card/file support. |
| P13.M3.E2 Teams Hardening | Existing Teams bugs are folded into room-agent UX. |
| P13.M3.E3 Docs And Operator Guide | Users can configure and debug room agents. |

Task candidates:

1. Add connector capability matrix.
   - Files likely touched: `src/connector_capabilities.ml`, connector handlers, connector tests.
   - Acceptance:
     - Existing connector capability records are extended rather than replaced.
     - Capabilities cover thread replies, edit in place, file upload, card/buttons, typing/status, and history capture.
     - Room-agent progress delivery uses the capability matrix rather than connector-specific guesses.
     - Thread-less connectors degrade deterministically without creating a new child key on every replay.

2. Fold Teams UX bugs into room-agent hardening.
   - Related backlog: `B424`, `B457`, `B499`.
   - Acceptance:
     - Slash commands work consistently in Teams for room profiles.
     - Consent card actions route correctly.
     - Background task completions do not suppress tool/progress delivery.

3. Add documentation.
   - Files likely touched: `docs/public/llms-full.txt`, setup docs, `docs/research` or `docs/design`.
   - Acceptance:
     - `llms-full.txt` documents room profiles, scoped memory, routines, and admin commands.
     - Setup docs include Slack and Teams examples.
     - Troubleshooting covers missing profile, denied tool, budget exceeded, and no thread support.

## TDD And Verification Strategy

Recommended focused test order:

1. Pure helpers:
   - `test/test_room_session.ml`
   - `test/test_room_workspace.ml`
   - `test/test_room_origin.ml`

2. Schema/API:
   - `test/test_config_loader.ml`
   - `test/test_memory.ml`
   - `test/test_task_tree.ml`
   - `test/test_background_task.ml`
   - `test/test_request_stats.ml`
   - `test/test_audit.ml`

3. Connector behavior:
   - `test/test_slack.ml`
   - `test/test_teams.ml`
   - `test/test_discord.ml`
   - `test/test_telegram*.ml` where relevant

4. Integration:
   - `test/test_session_model_override.ml` for profile/model precedence.
   - `test/test_session_persistence.ml` for shared room session state and CWD persistence.
   - `test/test_restart.ml` and `test/test_daemon.ml` for restart/replay and completion delivery.
   - `test/test_scheduler.ml` for cron/routine engine behavior.
   - daemon/session tests for profile resolution, CWD propagation, async completion, cron routine execution, and restart/replay behavior.

Required negative/security tests:
- Workspace path traversal, symlink escape, slug collision, control/unicode stripping, and length-bound tests.
- Channel A cannot see Channel B message search, scoped memory, core memory, connector history, or prompt injection.
- Guest users cannot bind profiles, grant memory, change CWD policy, create routines, read ledgers, or grant codebases.
- A profile-selected Claude/OAuth runner remains blocked when `security.allow_anthropic_oauth_inference = false`.
- A codebase grant denied by global workspace/sandbox policy fails closed.
- Budget errors do not contain raw prompt text.

Minimum verification for non-trivial OCaml edits:
- `make test-run ARGS="test <focused_suite>"`
- `make test`
- `make fmt-check`

Runtime split verification when connector work is touched:
- Build normal binary.
- Build minimal binary if the touched command surface has minimal stubs.
- Confirm integration-only code does not enter `clawq_runtime_core` accidentally.

## Backlog Ingestion Strategy After Review

After Claude review and plan refinement:

1. Snapshot:
   - `bl tree`
   - `bl list --unfinished --json`
   - `bl list --bugs --unfinished`
   - `bl list --ideas --unfinished`

2. Add phases without relying on phase-level dependencies for scheduling.
   - Phase-level dependencies are descriptive only for this backlog shape; they do not reliably gate child tasks.
   - Capture the actual returned phase ids instead of assuming P11/P12/P13 are still available.

3. Add milestones and epics from this document.
   - Annotate every task body with its intended parent epic id.
   - Add an overlap table in the task body for related `B*`/`I*` ids: existing id, target task, relationship (`references`, `supersedes`, `keep`).

4. Add tasks with:
   - Tags: `room-agent,claude-tag,profiled-room`
   - Body fields: acceptance criteria, likely files, related existing backlog IDs, test command.
   - Task-level `--depends-on` only after the target dependency task id exists.
   - P7 terminal task dependencies only on async-heavy tasks such as mention-to-task completion delivery, restart/replay, routines, and ambient watcher work.
   - Cross-phase dependencies on concrete terminal tasks, not broad phase ids.

5. Do not close overlapping ideas/bugs automatically. Mark them in task bodies first, then close/move only after the new tasks are accepted.

6. Validate:
   - `bl check`
   - `bl tree`
   - `bl show <new phase>`
   - `bl why <representative gated task>` to prove task-level dependencies actually block work.
   - `bl blockers` to inspect dependency chains.

7. If a hardcoded phase number shifted because other work landed first, update this planning document or an ingestion note with the actual ids created.

## Resolved Review Questions

| Question | Resolution |
|---|---|
| Shared room + child thread risks | Keep the model, but add P11 privacy guard, typed parser, guest/admin policy, concurrency tests, and restart/replay tests before async-heavy behavior ships. |
| Scoped memory vs core memories | Runtime room paths use scoped memory only. Legacy core memory remains available through an explicit legacy/admin scope, preferably read-in-place rather than copied. |
| Cron extension | Extend existing `scheduler.ml`/cron metadata with nullable profile/thread/routine fields and resolve room session at tick time. Do not create a parallel scheduler. |
| Room CWD storage | Config holds declarative policy; DB holds materialized resolved path and session `effective_cwd`. |
| First connector | Slack first for full E2E because it exercises the real per-user to shared-room change. Teams gets early capability audit and fast-follow hardening. |
| Phase breadth | Phase breadth is acceptable for `bl`; split only individual oversized epics/tasks during ingestion. The real risk is dependency mechanics, handled with task-level deps and `bl why`. |

## Remaining Author Questions Before Ingestion

These are not blockers for keeping the plan reviewable, but they should be answered before final backlog ingestion:

1. Should guest users be allowed to request async work by default in a profiled room, or should async work be admin-only until profile policy explicitly allows guests?
2. Should the default monthly budget warning be admin-only, room-visible, or both with redacted room-visible wording?
3. When a room is rebound to a new profile, should the default be preserve old workspace/memory read-only, or reset to a fresh workspace/memory scope?

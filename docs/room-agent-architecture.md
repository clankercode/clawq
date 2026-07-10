# Room Agent Architecture

**Status**: Authoritative  
**Last updated**: 2026-06-30  
**Owner**: Room Agent System

This document describes how room agents work end-to-end in Clawq — from
incoming message to tool execution to response — with focus on the scope
bundle model, access snapshots, room policy, and session lifecycle.

---

## Table of Contents

1. [Architecture Overview](#1-architecture-overview)
2. [Scope Bundle Model](#2-scope-bundle-model)
3. [Scope Resolution](#3-scope-resolution)
4. [Effective Access Snapshots](#4-effective-access-snapshots)
5. [Room Policy](#5-room-policy)
6. [Session Lifecycle](#6-session-lifecycle)
7. [Background Tasks](#7-background-tasks)
8. [Instruction Layers](#8-instruction-layers)
9. [Data Flow Diagram](#9-data-flow-diagram)

---

## 1. Architecture Overview

Room agents are the core execution unit in Clawq. Every user interaction —
whether from Slack, Teams, Discord, Telegram, or the web UI — flows through
the same pipeline:

```
Connector → Session Manager → Room Policy → Scope Resolution → Access Snapshot → Agent Turn → Tool Execution → Response
```

**Key design principles:**

- **Immutable snapshots**: Access policy is frozen at the moment work begins.
  Config changes during execution do not alter in-flight access.
- **Deny wins**: Conflicting tool grants resolve to deny (deny-list always
  overrides allow-list).
- **Provenance tracking**: Every effective grant records which bundle, scope,
  and layer contributed it.
- **Fail-closed**: Invalid bundle references or missing scopes produce zero
  grants, not open access.

**Core modules:**

| Module | Responsibility |
|--------|---------------|
| `session_turn.ml` | Session lifecycle, message queuing, turn orchestration |
| `session_management.ml` | Session config/model management, reset, compaction, and debug dumps |
| `runtime_config.ml` | Scope resolution, effective access computation |
| `access_snapshot.ml` | Immutable snapshots, tool denial checks, audit persistence |
| `room_policy.ml` | Room classification (dm/group/external/shared), policy evaluation |
| `invocation_restrict.ml` | Role-based invocation restrictions (admin/member/guest) |
| `background_task_context.ml` | Background task context/origin routing |
| `background_task_spawn.ml` | Process spawning, worktrees, scheduling, restart/readoption |
| `background_task_room.ml` | Room and GitHub-triggered background task launching |
| `background_task_workflow.ml` | Workflow-triggered background task launches |
| `agent_turn_setup.ml` / `agent_turn_core.ml` / `agent_2_tools.ml` | Agent creation, shared turn loop, tool execution, snapshot-scoped access |

---

## 2. Scope Bundle Model

The scope bundle model is the foundation of Clawq's access control. It
decouples *what* is granted (bundles) from *where* it applies (scopes).

### Access Bundles

An `access_bundle` is a named collection of grants:

```ocaml
type access_bundle = {
  id : string;
  display_name : string option;
  system_prompt : string option;
  allowed_tools : string list;
  denied_tools : string list;
  codebase_grants : string list;
  mcp_servers : string list;
  skills : string list;
  repositories : string list;      (* deprecated, use repo_grants *)
  repo_grants : repo_grant list;   (* fine-grained capability control *)
  domains : string list;
  egress_rules : egress_rule list;
  credential_handles : string list;
  instructions : instruction_record list;
  memory_grants : string list;
  budget_refs : string list;
  status : string;
}
```

**Bundles are reusable**: a single bundle can be referenced by multiple scopes.
For example, a "standard-tools" bundle might be included in both workspace and
room scopes.

### Access Scopes

An `access_scope` binds bundles to a specific layer in the hierarchy:

```ocaml
type access_scope_level = Default | Workspace | Channel | Room

type access_scope = {
  id : string;
  level : access_scope_level;
  workspace : string option;   (* selector for Workspace layer *)
  channel : string option;     (* selector for Channel layer *)
  room : string option;        (* selector for Room layer *)
  access_bundle_ids : string list;
  status : string;
}
```

Scopes are selectors — they match sessions based on the connector, workspace,
channel, and room identifiers embedded in the session key.

### Room Profiles

Room profiles are a higher-level construct that combines bundles with
agent-specific settings (model, system prompt, tool iteration limits):

```ocaml
type room_profile = {
  id : string;
  display_name : string option;
  model : string;
  system_prompt : string;
  max_tool_iterations : int;
  status : string;
  allowed_tools : string list;
  denied_tools : string list;
  access_bundle_ids : string list;
  ambient_enabled : bool;
  ambient_quiet_start : int;
  ambient_quiet_end : int;
  ambient_rate_limit_rph : int;
}
```

The three `ambient_*` fields configure ambient scheduling: `ambient_quiet_start`/
`ambient_quiet_end` define the quiet-hours window (hours, 0-23), and
`ambient_rate_limit_rph` caps ambient activations per hour.

Room profiles are bound to rooms via `room_profile_binding`:

```ocaml
type room_profile_binding = {
  profile_id : string;
  room : string;
  active : bool;
}
```

A profile's `access_bundle_ids` are treated as the **room layer** during
resolution — they append after all scope bundles.

### Effective Access

The resolved result is an `effective_access` record where each item carries
provenance:

```ocaml
type effective_access_item = {
  value : string;
  provenance : access_provenance list;
}

type access_provenance = {
  layer : string;       (* "default", "workspace", "channel", "room" *)
  source_id : string;   (* bundle or scope id *)
  field : string;       (* "allowed_tools", "codebase_grants", etc. *)
}
```

---

## 3. Scope Resolution

Scope resolution is the process of merging bundles from matching scopes into a
single `effective_access` record. It is deterministic and pure (no IO, no
mutation).

### Precedence Hierarchy

Scope levels have fixed precedence (ascending priority):

```
Default (0) < Workspace (1) < Channel (2) < Room (3)
```

- **Default**: Always applies; provides baseline grants.
- **Workspace**: Matches by workspace selector against the effective workspace
  path (the normalized `workspace` config value / `$CLAWQ_WORKSPACE`, e.g.
  `/home/user/src/clawq`). The selector is treated as a path, not a Teams team ID.
- **Channel**: Matches by channel type (e.g., "slack", "discord").
- **Room**: Matches by specific room/channel ID.
- **Profile bundles**: Treated as room-layer; appended after scope bundles.

### Resolution Algorithm

```
1. Filter active scopes (status != "deleted")
2. For each scope level, find scopes matching the session key:
   - Workspace: match workspace selector
   - Channel: match channel type
   - Room: match room ID (required); optional workspace/channel selectors
     further narrow the match (channel matched via `string_option_matches`)
3. Sort scopes by (level_rank ASC, id lexicographic)
4. Collect bundles from matching scopes (declaration order within scope)
5. Append profile bundles (room layer)
6. Merge all bundle fields:
   - allowed_tools: union of all bundles
   - denied_tools: union of all bundles
   - codebase_grants: union, then apply global security caps
   - repo_grants: union, then apply codebase grant coverage check
   - egress_rules: ordered by scope priority (Room > Channel > Workspace > Default)
7. Apply deny-wins filter: remove any allowed_tool that appears in denied_tools
8. Apply global security caps:
   - workspace_only: block grants outside workspace
   - allowed_cwd_patterns: restrict grants to matching paths
9. Apply codebase grant coverage: repo_grants must be covered by codebase_grants
```

### Conflict Resolution Rules

| Scenario | Resolution |
|----------|-----------|
| Same tool in allowed and denied (same bundle) | **Deny wins** (INV-CONF-2) |
| Tool allowed by workspace, denied by room | **Deny wins** (INV-CONF-3) |
| Duplicate bundle references across scopes | **Provenance merged, no duplication** (INV-CONF-5) |
| Invalid bundle reference | **Fail-closed**: zero grants from that scope (INV-CONF-4) |
| Missing `memory_grants` field | **Defaults to empty** (no access) (INV-MEM-2) |

### Channel Isolation

Room scopes with a `channel` selector only match sessions from that channel
type. A Slack room scope does not grant access to Discord sessions (INV-CHAN-1).
Missing layer selectors are **not wildcards** — a workspace scope without a
`workspace` field matches nothing (INV-CHAN-2).

### Repo Grant Resolution

Repo grants receive additional validation:

1. **Explicit over legacy**: `repo_grants` suppress `repositories` for the same
   repo key (INV-REPO-1).
2. **Global security check**: Grants outside the workspace are blocked when
   `workspace_only = true` (INV-REPO-2).
3. **Codebase coverage**: If `codebase_grants` is non-empty, repo grants must
   be covered by at least one codebase grant pattern (INV-REPO-3).
4. **Path normalization**: Traversal (`../`) is normalized before checks
   (INV-REPO-4).
5. **Wildcard handling**: Wildcard repo grants require exact codebase grant
   match, not glob matching (INV-REPO-5).

---

## 4. Effective Access Snapshots

An access snapshot is an **immutable** record of the resolved access policy at
the moment work begins. It ensures that config changes during execution do not
alter in-flight access.

### Snapshot Structure

```ocaml
type t = {
  id : string;                           (* "snap_<ts>_<rand>" *)
  timestamp : string;
  config_hash : string;                  (* SHA-256 of serialized config *)
  session_key : string option;
  work_type : work_type;
  room_id : string option;
  profile_id : string option;
  bundle_sources : bundle_source list;   (* provenance trail *)
  allowed_tools : string list;
  denied_tools : string list;
  codebase_grants : string list;
  blocked_codebase_grants : string list;
  mcp_servers : string list;
  skills : string list;
  repositories : string list;
  repo_grants : string list;
  blocked_repo_grants : string list;
  domains : string list;
  credential_handles : string list;
  memory_grants : string list;
  budget_refs : string list;
  egress_rules_count : int;
  instruction_digests : string list;
  redacted_summary : string;
  room_classification : room_scope;
  room_policy_decision : string;
}
```

### Work Types

Snapshots are created for every type of executable work:

```ocaml
type work_type =
  | Room_turn         (* Interactive room session *)
  | Background_task   (* Async delegated work *)
  | Ambient_work      (* Scheduled ambient activity *)
  | GitHub_trigger    (* Webhook-triggered work *)
  | Routine           (* Scheduled routine execution *)
```

### Tool Denial

The `tool_denial` function checks a tool against the snapshot's resolved
allowed/denied lists:

```ocaml
let tool_denial (snap : t) ~tool_name : string option =
  if List.mem tool_name snap.denied_tools then
    Some "Error: Tool '<name>' is denied by the access snapshot policy."
  else if snap.allowed_tools <> [] && not (List.mem tool_name snap.allowed_tools) then
    Some "Error: Tool '<name>' is not in the allowed tools list."
  else None
```

**Invariant (INV-SNAP-2)**: Snapshot `tool_denial` matches the resolver's
allowed/denied lists exactly.

### Agent Integration

When a turn begins, the snapshot is stored on the agent:

```ocaml
agent.access_snapshot_id <- Some snap.id;
agent.access_snapshot <- Some snap;
```

During tool execution, the agent uses the snapshot instead of re-resolving
from the live config:

```ocaml
let room_profile_tool_denial agent ~session_key ~tool_name =
  match agent.access_snapshot with
  | Some snap -> Access_snapshot.tool_denial snap ~tool_name
  | None -> (* fallback to live config resolution *)
      Runtime_config.room_profile_tool_denial_for_session agent.config
        ~session_key ~tool_name
```

### Persistence

Snapshots are persisted to SQLite for audit trails. They include:

- The full resolved access fields (as JSON arrays)
- Bundle provenance (which bundle/layer contributed each grant)
- Config hash for change detection
- Room classification and policy decision

### Invariants

| Invariant | Status | Description |
|-----------|--------|-------------|
| INV-SNAP-1 | `[RUNTIME] [TEST]` | Snapshots are immutable once created |
| INV-SNAP-2 | `[RUNTIME] [TEST]` | Tool denial matches resolver exactly |
| INV-SNAP-3 | `[RUNTIME] [TEST]` | Config hash is deterministic (SHA-256) |
| INV-SNAP-4 | `[RUNTIME] [TEST]` | Bundle provenance is preserved |

---

## 5. Room Policy

Room policy evaluates whether work should proceed in rooms classified as
containing external users, guests, or shared channels.

### Room Classification

Rooms are classified into one of five scopes:

```ocaml
type room_scope =
  | Rm_dm       (* Direct message between two internal users *)
  | Rm_group    (* Internal group conversation *)
  | Rm_external (* Room with external participants *)
  | Rm_shared   (* Shared room with another organization *)
  | Rm_unknown  (* Connector does not expose metadata *)
```

Classification is derived from connector-provided metadata:

```ocaml
let classification_from_context ~connector ~room_id ~session_key
    ?(is_group = false) ?(has_external_users = false) ?tenant_id () =
  let scope =
    if has_external_users then Rm_external
    else if is_group then Rm_group
    else derive_scope_from_session_key session_key
  in
  { connector; room_id; scope; has_external_users; tenant_id }
```

When connectors don't expose metadata, scope is inferred from the session key
structure via `derive_scope_from_session_key`:

- A single ID segment after the connector yields `Rm_dm` **only if it starts
  with `@`** (personal/DM key); otherwise it falls back to `Rm_unknown`.
- A multi-segment key (two or more segments after the connector) yields
  `Rm_group` (room or thread).
- Anything else yields `Rm_unknown`.

### Policy Actions

Each scope maps to a policy action:

```ocaml
type external_policy_action =
  | Policy_allow                     (* Proceed without restriction *)
  | Policy_warn of string            (* Proceed but show warning *)
  | Policy_deny of string * bool     (* Deny; bool = allow admin override *)
```

**Default behavior:**

- `Rm_dm` and `Rm_group`: Always `Policy_allow` (internal rooms).
- `Rm_external` and `Rm_shared`: Use per-connector override or default action.
- `Rm_unknown`: Use per-connector override or default action.

### Policy Evaluation

```ocaml
let evaluate policy ~classification ~is_admin () : eval_result =
  match action_for_scope policy ~connector ~scope with
  | Policy_allow -> Proceed
  | Policy_warn msg -> Proceed_with_warning (format_message msg)
  | Policy_deny (reason, allow_admin) ->
      if allow_admin && is_admin then Denied_admin_override (format_message reason)
      else Denied (format_message reason)
```

### Invocation Restrictions

Role-based restrictions are checked **before** room policy:

```ocaml
type work_kind =
  | Room_work | Routine | Memory_mutation | GitHub_trigger | Background_task

type caller_role = Admin | Member | Guest | Unknown
```

**Role requirements:**

| Work Kind | Required Roles |
|-----------|---------------|
| Room_work | All roles (policy handles external restrictions) |
| Routine | Admin, Member |
| Memory_mutation | Admin, Member |
| GitHub_trigger | Admin, Member |
| Background_task | Admin, Member |

The combined check (`check_room_policy_and_role`) runs role check first, then
room policy. Either can deny work.

---

## 6. Session Lifecycle

### Message Flow

```
1. Connector receives message
2. Session manager normalizes message (! prefix for interrupts)
3. Special commands handled (/status, /agent, etc.)
4. Room policy evaluated (external room check + role check)
5. If denied → return denial message
6. Message queued if session is busy
7. Access snapshot created (immutable)
8. Agent turn begins:
   a. Skill references expanded
   b. Skill injections prepended to history
   c. Attachment context injected
   d. History prepared (compaction if needed)
   e. Runtime context built
   f. LLM called with streaming
   g. Tool calls executed (parallel when multiple)
   h. Response streamed to connector
9. History persisted
10. Queued messages drained (recursive)
```

### Session Locking

Sessions use a mutex to prevent concurrent turns:

- `with_session_lock_unless_draining`: Acquires lock or returns draining message.
- `try_session_lock`: Non-blocking lock attempt.
- `with_in_flight`: Tracks active work for graceful shutdown.
- `with_live_activity`: Prevents session cleanup during active work.

### Message Queuing

When a session is busy, incoming messages are queued:

- `enqueue_message_if_busy`: Adds to queue if session is locked.
- `enqueue_followup_if_busy`: For deferred followups.
- `drain_queued_messages`: Processes queue after turn completes.
- `take_all_queued_messages_for_injection`: Injects queued messages mid-turn.

Queue items carry full context:

```ocaml
type queued_message = {
  message : string;
  content_parts : content_part list;
  attachments : attachment list;
  channel_name : string option;
  channel_type : string option;
  sender_id : string option;
  sender_name : string option;
  user_group : string option;
  channel : string option;
  channel_id : string option;
  message_id : string option;
  inbound_queue_id : int option;
  bang : bool;
  deferred_followup : bool;
  snapshot_work_type : Access_snapshot.work_type option;
  has_external_users : bool;
}
```

### Interrupt Handling

Messages prefixed with `!` trigger interrupts:

- `!message`: Interrupt current turn, inject message.
- `!` alone: Interrupt with `[interrupted]` marker.
- `interrupt_check` is polled during tool execution and LLM calls.

### Stuck Detection

An observer monitors sessions for stuck patterns:

- ConsecutiveErrors: Same tool failing repeatedly.
- RepeatedToolCall: Same tool+args called many times.
- SameErrorString: Same error message repeated.
- NearMaxIters: Approaching iteration limit.

When stuck, a correction message is injected and a postmortem agent is
spawned to analyze the failure.

---

## 7. Background Tasks

Background tasks extend room context into async execution.

### Room Background Task Launch

```ocaml
let launch_room_bg_task ~db ~session_key ~connector ~room_id ~requester_id
    ~goal ?preferred_runner ?agent_name ?thread_id ?model_override
    ?notify_cfg ?(use_worktree = false) ?access_snapshot_id ?config () =
  (* 1. Resolve profile_id from room binding *)
  (* 2. Auto-create access snapshot if not provided *)
  (* 3. Check blocked repo grants *)
  (* 4. Build room origin metadata *)
  (* 5. Create room session record *)
  (* 6. Enqueue task with runner *)
```

### Context Flow

Room context flows through background tasks via:

1. **Access Snapshot**: Captures effective access at launch time. Background
   tasks inherit the room's repo grants and other access rights.

2. **Room Origin**: Metadata about where the task was triggered:

   ```ocaml
   (* Module Room_origin, type t *)
   type t = {
     connector : string option;
     workspace_id : string option;
     room_id : string option;
     requester_id : string option;
     requester_name : string option;
     source_message_id : string option;
     thread_id : string option;
     service_url : string option;
     profile_id : int option;
   }
   ```

3. **Profile ID**: From room-profile binding, passed to task metadata.

4. **Session Key**: Carries room context for notification routing.

### Worktree Isolation

Background tasks can run in git worktrees for isolation:

- `use_worktree=true`: Creates a git worktree from the repo.
- `use_worktree=false`: Runs in the room workspace directory.
- Room workspaces are plain directories (no git), so `use_worktree` defaults
  to `false` for room-launched tasks.

### Runner Selection

Tasks can run on external runners (Codex, Claude, Kimi, Gemini, Opencode,
Cursor) or the local runner:

- External runners: Spawned as subprocess with CLI arguments.
- Local runner: Runs in-process with the local turn function.
- ACP mode: Interactive runner communication via ACP protocol.

### Repo Grant Enforcement

Before launching, background tasks check:

1. **Blocked repo grants**: Denied by security policy.
2. **Repo grant coverage**: Must be covered by codebase grants.
3. **Access snapshot**: Records effective policy at launch time.

### Progress Tracking

Room background tasks create progress checklists:

```ocaml
let item = Room_progress_checklist.append ~db ~task_id:id
    ~title:"Task accepted" ?session_record_id ()
```

Checklist items track task state transitions (Current → Done/Failed).

### Local Task Restart Policy (B736)

Local runner tasks run **in-process** within the daemon. Unlike external
runners (which spawn child processes that survive a daemon restart), Local
tasks are lost when the daemon process exits.

To mitigate this, Local tasks support a **restart policy** that controls
what happens on daemon startup:

| Field | Default | Description |
|---|---|---|
| `restart_policy` | `reenqueue` | `reenqueue` re-queues the task on restart; `fail` marks it as failed |
| `max_restarts` | `2` | Maximum number of restart attempts before giving up |
| `restart_count` | `0` | Auto-incremented on each re-enqueue |

**Startup sequence** (in `daemon.ml`):

1. `reap_dead_running_tasks` — marks orphaned external-runner tasks as failed.
   B768: liveness is checked through the session-host seam using the persisted
   `host_kind`/`host_session_id`; a reused PID is detected as a stale session
   and marked failed instead of being mistaken for the original process.
2. `reenqueue_stale_local_tasks` — scans for Local tasks in `Running` state
   that are no longer tracked in memory (i.e. daemon was restarted):
   - `restart_policy=fail` → `Failed` with reason "Interrupted by daemon restart".
   - `restart_count >= max_restarts` → `Failed` with reason "Max restarts exceeded".
   - Room budget exceeded → `Failed` with reason "Budget exceeded on restart".
   - Otherwise → transition to `Queued`, increment `restart_count`.
3. `readopt_running_tasks` — re-adopts hosted sessions whose host confirms
   the recorded identity is still live (B768); dead or stale sessions are left
   for reaping.

Re-enqueued Local tasks resume via the normal scheduler. Agent history is
hydrated from the session's persisted messages (`Memory.load_history`), so
the model sees prior context — not a blank slate.

---

## 8. Instruction Layers

Instructions are layered from default to room-specific, with each layer able
to override or supplement the previous.

### Layer Hierarchy

```
Default instructions (built-in)
  ↓
Workspace instructions (config-level)
  ↓
Channel instructions (connector-level)
  ↓
Room-profile instructions (room-specific)
```

### Instruction Records

Instructions carry structured metadata:

```ocaml
type instruction_edit_policy = Locked | Admin_only | Open

type instruction_record = {
  text : string;
  source_scope : string;          (* always set; "default" for built-ins *)
  author : string option;
  enabled : bool;
  digest : string option;          (* None => computed from text via SHA-256 *)
  locked : bool;                   (* legacy hard-lock flag *)
  edit_policy : instruction_edit_policy;
}
```

`edit_policy` is the typed variant governing who may edit the instruction
(`Locked` = no edits, `Admin_only` = admins, `Open` = anyone). The boolean
`locked` field is a separate legacy hard-lock flag.

### Resolution

During scope resolution, instructions from all matching bundles are collected:

1. Each bundle's `instructions` field contributes instruction records.
2. Provenance tracks which bundle/layer contributed each instruction.
3. Instructions are prepended to the agent's system prompt.
4. Digest-based deduplication prevents the same instruction from appearing
   twice.

### Agent Integration

Instructions are resolved per-session and stored on the agent:

```ocaml
let instruction_items =
  Session_room_profile.resolve_instruction_items_for_session mgr ~key
in
let agent = Agent.create ~config ~instruction_items ()
```

The agent's system prompt includes all resolved instructions.

---

## 8.5. Cross-Room Context Learning

When a room has an admin-created **read grant** over a sibling room's scope,
the agent's turn-time retrieval (in `src/agent_2_tools.ml` `inject_search_context`)
also searches the granted scope for relevant context and injects results with
a provenance label: `[granted:room/<sibling-key>]`.

This implements Claude-Tag-style "cross-channel learning, if granted permission"
(clawq issue B733).

### How It Works

1. At the start of a room agent turn, `inject_search_context` performs scoped
   FTS + vector retrieval for the room's own scope (as before).
2. It then calls `Memory.list_scopes_granted_to_principal` to find all sibling
   scopes where the current room (as principal) holds a `read` capability
   grant.
3. For each granted sibling scope (excluding self), it performs FTS message
   search and, if an embedding provider is configured, a scope-aware vector
   search. Results are merged using the existing `Vector.merge_results`.
4. Merged results are labelled with `[granted:room/<sibling-key>]` provenance
   and appended to the context injection.
5. A `cross_scope_context_injected` event is emitted to the
   `room_activity_ledger` for each sibling scope that returned results.

### Invariants

- **Fail-closed**: No grant => no cross-scope retrieval. A room with no read
  grant over a sibling retrieves nothing from it.
- **Provenance**: Every cross-scope snippet is labelled with its source scope
  for agent attribution and audit.
- **Budget-aware**: Cross-scope retrieval respects `room_budget` hard caps;
  if the current room's budget is exceeded, cross-scope embedding calls are
  skipped (mirrors the B734 scoped vector budget gate).
- **No private-channel reporting**: Grant creation over private/default-deny
  scopes is refused. The Slack connector's private-channel policy (B735) is
  the primary defense; cross-scope retrieval is a secondary safeguard.
- **Self-exclusion**: A room never retrieves from its own scope via the
  granted-sibling path (the own-scope path handles that).

### Grants

Grants are stored in the `memory_grants` table (scope_id, principal_kind,
principal_id, capability). The `list_scopes_granted_to_principal` function
queries for all scopes where a given principal has `read` capability. Grants
are admin-created via `clawq rooms memory grants add`.

---

## 9. Data Flow Diagram

### End-to-End Message Flow

```
┌─────────────────────────────────────────────────────────────────────┐
│                        Incoming Message                             │
│  (Slack / Teams / Discord / Telegram / Web UI / GitHub Webhook)     │
└──────────────────────────────┬──────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────────┐
│                      Session Manager                                │
│  • Normalize message (! prefix → interrupt)                         │
│  • Handle special commands (/status, /agent, etc.)                  │
│  • Queue if session busy                                            │
└──────────────────────────────┬──────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    Room Policy Evaluation                           │
│  Invocation_restrict.check_room_policy_and_role                     │
│  ┌─────────────────┐    ┌─────────────────────┐                    │
│  │ Role Check       │───▶│ Room Policy Check    │                    │
│  │ (admin/member/   │    │ (dm/group/external/  │                    │
│  │  guest)          │    │  shared/unknown)     │                    │
│  └─────────────────┘    └─────────────────────┘                    │
│  Result: Ok(classification, decision) or Error(denial)              │
└──────────────────────────────┬──────────────────────────────────────┘
                               │
                    ┌──────────┴──────────┐
                    │                     │
                    ▼                     ▼
            ┌──────────────┐    ┌──────────────────┐
            │ Denied       │    │ Allowed           │
            │ Return error │    │ Continue           │
            └──────────────┘    └────────┬─────────┘
                                         │
                                         ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    Scope Resolution                                 │
│  Runtime_config.resolve_effective_access                            │
│                                                                     │
│  ┌─────────┐ ┌───────────┐ ┌─────────┐ ┌──────┐ ┌──────────────┐ │
│  │ Default  │ │ Workspace │ │ Channel │ │ Room │ │ Profile      │ │
│  │ Bundles  │ │ Bundles   │ │ Bundles │ │Bndl  │ │ Bundles      │ │
│  └────┬─────┘ └─────┬─────┘ └────┬────┘ └──┬───┘ └──────┬───────┘ │
│       └──────────────┴────────────┴─────────┴────────────┘         │
│                              │                                      │
│                              ▼                                      │
│                    ┌───────────────────┐                            │
│                    │ Merge + Deny-Wins │                            │
│                    │ + Security Caps   │                            │
│                    └─────────┬─────────┘                            │
│                              │                                      │
│                              ▼                                      │
│                    ┌───────────────────┐                            │
│                    │ effective_access  │                            │
│                    │ (with provenance) │                            │
│                    └───────────────────┘                            │
└──────────────────────────────┬──────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    Access Snapshot                                  │
│  Access_snapshot.create_and_persist                                 │
│  • Immutable record of resolved access                              │
│  • Config hash (SHA-256)                                            │
│  • Bundle provenance trail                                          │
│  • Room classification + policy decision                            │
│  • Stored on agent: agent.access_snapshot <- Some snap              │
└──────────────────────────────┬──────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────────┐
│                       Agent Turn                                    │
│  Agent.create + Session_turn.run_locked_turn                        │
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │ Preparation                                                  │   │
│  │  • Expand skill references                                   │   │
│  │  • Inject skill instructions                                 │   │
│  │  • Inject attachment context                                 │   │
│  │  • Prepare history (compaction if needed)                    │   │
│  │  • Build runtime context                                     │   │
│  └──────────────────────────┬──────────────────────────────────┘   │
│                              │                                      │
│                              ▼                                      │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │ LLM Call                                                     │   │
│  │  • Stream response from provider                             │   │
│  │  • Parse tool calls from response                            │   │
│  │  • Record provider ledger events                             │   │
│  └──────────────────────────┬──────────────────────────────────┘   │
│                              │                                      │
│                              ▼                                      │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │ Tool Execution (parallel when multiple)                      │   │
│  │                                                              │   │
│  │  ┌──────────────────┐                                       │   │
│  │  │ Tool Denial Check │ agent.access_snapshot.tool_denial     │   │
│  │  │ (snapshot-scoped) │                                       │   │
│  │  └────────┬──────────┘                                       │   │
│  │           │                                                  │   │
│  │    ┌──────┴──────┐                                          │   │
│  │    │             │                                          │   │
│  │    ▼             ▼                                          │   │
│  │ ┌──────┐  ┌──────────┐                                     │   │
│  │ │Denied│  │ Execute   │ Tool.execute                        │   │
│  │ │Error │  │ Tool      │ (with session_key, room context)    │   │
│  │ └──────┘  └──────────┘                                     │   │
│  └─────────────────────────────────────────────────────────────┘   │
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │ Response                                                     │   │
│  │  • Stream to connector                                       │   │
│  │  • Persist history                                           │   │
│  │  • Update effective_cwd                                      │   │
│  │  • Drain queued messages                                     │   │
│  └─────────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────┘
```

### Background Task Flow

```
┌──────────────────┐
│ Room Command      │
│ /delegate <goal>  │
└────────┬─────────┘
         │
         ▼
┌─────────────────────────────────────────────────────────────────┐
│ launch_room_bg_task                                              │
│  1. Resolve profile_id from room binding                         │
│  2. Auto-create access snapshot (Background_task work_type)      │
│  3. Check blocked repo grants                                    │
│  4. Build room origin (connector, room_id, requester, thread)    │
│  5. Create room session record                                   │
│  6. Enqueue task with runner                                     │
└────────┬────────────────────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────────────────────────┐
│ spawn_task                                                       │
│  1. Prepare worktree (if git repo)                               │
│  2. Build command from runner framework                          │
│  3. Set environment (session_key, access_snapshot_id)            │
│  4. Start process group (fork+setsid+execve)                     │
│  5. Monitor process (waitpid, watchdog)                          │
│  6. Finalize (exit code, output, session ID extraction)          │
└────────┬────────────────────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────────────────────────┐
│ Background Agent (in worktree or room workspace)                 │
│  • Inherits access snapshot (repo grants, tool access)           │
│  • Room origin for notification routing                          │
│  • Progress checklist updates                                    │
│  • Completion notification to room                               │
└─────────────────────────────────────────────────────────────────┘
```

### GitHub Review Trigger Flow

```
┌──────────────────┐
│ GitHub Webhook    │
│ (PR opened/       │
│  synchronize)     │
└────────┬─────────┘
         │
         ▼
┌─────────────────────────────────────────────────────────────────┐
│ launch_triggered_run                                             │
│  1. Build review prompt (PR metadata, changed files)             │
│  2. Resolve effective access for room                            │
│  3. Verify repo is in granted repositories                       │
│  4. Enrich prompt with access snapshot + room origin             │
│  5. Launch as background task                                    │
└────────┬────────────────────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────────────────────────┐
│ Review Agent                                                     │
│  • Access snapshot scoped to room policy                         │
│  • Repo grant enforcement (blocked if not granted)               │
│  • Progress reporting via room checklist                         │
│  • Multi-room subscription support                               │
└─────────────────────────────────────────────────────────────────┘
```

---

## Appendix: Key Invariants Reference

| ID | Category | Description | Status |
|----|----------|-------------|--------|
| INV-DET-1 | Determinism | Resolver is pure given stable HOME | `[RUNTIME] [TEST]` |
| INV-PREC-1 | Precedence | Default < Workspace < Channel < Room | `[RUNTIME] [TEST]` |
| INV-CONF-1 | Conflict | Deny wins over allow | `[RUNTIME] [TEST]` |
| INV-CONF-4 | Conflict | Invalid refs → fail-closed | `[RUNTIME] [TEST]` |
| INV-SEC-1 | Security | workspace_only blocks outside grants | `[RUNTIME] [TEST]` |
| INV-MEM-1 | Memory | Grants are direct, not transitive | `[RUNTIME] [TEST]` |
| INV-CHAN-1 | Channel | Room scopes don't cross channels | `[RUNTIME] [TEST]` |
| INV-SNAP-1 | Snapshot | Immutable once created | `[RUNTIME] [TEST]` |
| INV-SNAP-2 | Snapshot | Tool denial matches resolver | `[RUNTIME] [TEST]` |
| INV-REPO-3 | Repo | Grants require codebase coverage | `[RUNTIME] [TEST]` |

For the full invariant catalogue, see
[scope-resolution-invariants.md](scope-resolution-invariants.md).

---

## Appendix: Module Cross-Reference

| Source File | Key Functions | Description |
|-------------|---------------|-------------|
| `src/access_snapshot.ml` | `create`, `create_and_persist`, `tool_denial`, `record_for_work` | Snapshot creation, persistence, tool denial |
| `src/room_policy.ml` | `classification_from_context`, `evaluate`, `room_status_message` | Room classification and policy evaluation |
| `src/invocation_restrict.ml` | `check_role`, `check_room_policy_and_role` | Role-based invocation restrictions |
| `src/session_turn.ml` | `run_session_turn`, `run_locked_turn`, `drain_queued_messages` | Shared buffered/streaming session lifecycle and turn orchestration |
| `src/session_management.ml` | `update_config`, `reset`, `compact`, `dump_json` | Session management operations |
| `src/background_task_context.ml` | `routing_from_context`, `origin_fields_from_context`, `build_delegate_prompt` | Background task context/origin routing |
| `src/background_task_spawn.ml` | `spawn_task`, `start_queued`, `readopt_running_tasks` | Process spawning, scheduling, and restart/readoption |
| `src/background_task_room.ml` | `launch_room_bg_task`, `launch_triggered_run` | Room and GitHub-triggered task launching |
| `src/background_task_workflow.ml` | `trigger_workflow_from_room_command`, `trigger_security_review_workflow` | Workflow-triggered background task launches |
| `src/agent_turn_setup.ml` | `create` | Agent construction and turn preparation |
| `src/agent_turn_core.ml` | `run_turn`, `buffered_io`, `streaming_io` | Shared buffered/streaming agent loop |
| `src/agent_2_tools.ml` | `execute_tools`, `room_profile_tool_denial` | Tool execution with snapshot-scoped access |
| `src/runtime_config.ml` | `resolve_effective_access`, `sort_scopes` | Scope resolution and effective access |
| `src/runtime_config_types.ml` | `access_bundle`, `access_scope`, `effective_access`, `room_profile` | Type definitions |

---

## Related Design Documents

- **Self-Extension System** — How clawq grows its own feature surface ad-hoc via agent-authored plugins: [design/self-extension-system.md](design/self-extension-system.md).

# Memory & Governance Guide

**Last updated**: 2026-06-30

This guide covers Clawq's room-scoped memory system, visibility controls, grant management, isolation guarantees, prompt integration, and governance tooling.

---

## 1. Room Memory

Memories are stored per-room using a **scope** model. Each room gets a `memory_scope` row (kind=`"room"`, key=`<room_id>`) in the database. All memory operations are filtered to the calling room's scope -- one room cannot see, modify, or delete another room's memories.

### CRUD Operations (Agent Tools)

Agents in a room session have access to five tools:

| Tool | Description |
|------|-------------|
| `room_memory_list` | List memories visible to the current room |
| `room_memory_show` | Show full content of a memory by ID |
| `room_memory_save` | Save or update a memory (upsert by reference) |
| `room_memory_correct` | Correct memory content (preserves old provenance) |
| `room_memory_forget` | Soft-delete (redact) a memory |

### CRUD Operations (CLI)

All CLI commands go through `clawq rooms memory`:

```bash
# List memories in a room
clawq rooms memory list <room_id>

# Show a specific memory
clawq rooms memory show <room_id> <memory_id>

# Save a new memory (or update by reference)
clawq rooms memory save <room_id> <reference> <content> [--visibility V]

# Correct existing memory content
clawq rooms memory correct <room_id> <memory_id> <new_content>

# Soft-delete (redact) a memory
clawq rooms memory forget <room_id> <memory_id> [reason]

# Hard-purge (admin-only, irreversible)
clawq rooms memory forget <room_id> <memory_id> -- --hard [reason]
```

The `--visibility` flag on `save` accepts `public` (default), `private`, or `team`.

### Upsert Semantics

When saving a memory with a `reference` that already exists in the room scope, the content is updated in place rather than creating a duplicate. The `id` stays the same. Visibility is preserved on upsert unless explicitly overridden.

---

## 2. Memory Visibility

Every memory has a **visibility** level that controls who within the room can see it.

| Level | Behavior |
|-------|----------|
| `public` | Visible to all callers in the room (default) |
| `private` | Intended for the scope owner (the profile bound to the room), but **currently not surfaced through the room memory tools** (see note below) |
| `team` | Visible only to principals with an explicit team grant |

### How Visibility Is Enforced

The tool layer calls `can_see_memory` before returning any memory. This function checks:

- **Public**: always visible within the scope
- **Private**: only if the caller's `principal_id` matches the scope's `scope_profile_id`
- **Team**: only if a matching row exists in `memory_team_grants`

> **Note (Private visibility wiring gap):** The room memory tools
> (`room_memory_list`, `room_memory_show`, etc.) currently pass
> `principal_id = <room_id>` rather than the bound profile id, while
> `can_see_memory` compares `principal_id` against `scope_profile_id`
> (the stringified profile id). As a result, `private` memories are **never
> surfaced** through the room agent tools today — they are only reachable via
> admin/raw paths. This is either a tool-layer wiring fix (pass the bound
> profile id as `principal_id` in `tools_builtin_room_memory.ml`) pending a
> product decision on whether room agents should see their own private
> memories. **TODO**: resolve the product decision and wire accordingly. No
> code is changed by this doc update.

Visibility does **not** override scope isolation. A team grant for `room-a` on a memory stored in `room-b`'s scope does not make that memory visible to `room-a`. Scope boundaries are absolute.

### Changing Visibility

```bash
# Save with private visibility
clawq rooms memory save my-room "secret-note" "sensitive data" --visibility private

# Later, explicitly change to public
clawq rooms memory save my-room "secret-note" "declassified data" --visibility public
```

If `save` is called without `--visibility` on an existing reference, the current visibility is preserved.

---

## 3. Memory Grants

Grants control **scope-level** and **memory-level** access. They are managed by admin CLI commands.

### Scope Grants

Scope grants give a principal a **capability** on a room's memory scope. Supported capabilities: `list`, `read`, `write`.

```bash
# Add a scope grant (admin-only)
CLAWQ_ADMIN=1 clawq rooms memory grant add <room_id> <principal_kind> <principal_id> <capability>

# Example: allow a different room to read this room's memories
CLAWQ_ADMIN=1 clawq rooms memory grant add my-room room other-room read

# Example: allow a profile to write
CLAWQ_ADMIN=1 clawq rooms memory grant add my-room profile my-profile write

# List grants
CLAWQ_ADMIN=1 clawq rooms memory grant list <room_id>

# Revoke a grant
CLAWQ_ADMIN=1 clawq rooms memory grant revoke <room_id> <principal_kind> <principal_id> <capability>
```

Profile names are resolved to IDs automatically when `principal_kind` is `profile`.

### Team Grants

Team grants control visibility of `team`-level memories. They operate on individual memory rows, not scopes.

```bash
# Add a team grant (admin-only)
CLAWQ_ADMIN=1 clawq rooms memory team-grant add <room_id> <memory_id> <principal_kind> <principal_id>

# List team grants for a memory
CLAWQ_ADMIN=1 clawq rooms memory team-grant list <room_id> <memory_id>

# Remove a team grant
CLAWQ_ADMIN=1 clawq rooms memory team-grant remove <room_id> <memory_id> <principal_kind> <principal_id>
```

### Grant Resolution

- Grants are **direct**, not transitive. Granting access to a scope does not cascade to child grants.
- Expired grants (where `expires_at` is in the past) are excluded from resolution.
- Revoked grants (where `revoked_at` is set) are excluded if the column exists.
- All grant mutations require admin privileges.

---

## 4. Memory Isolation

### Cross-Channel Isolation

Each room's memories are isolated by `scope_kind = "room"` and `scope_key = <room_id>`. The tool layer resolves the room ID from the session key and applies these filters to every operation.

This means:

- Room A **cannot list, show, correct, or forget** Room B's memories.
- Room A **cannot save** into Room B's scope.
- Search results (both SQL content-search and FTS) are scoped to the calling room.

These invariants are enforced at the **tool layer** (`tools_builtin_room_memory.ml`), not at the raw Memory API. Admin-level operations that bypass the tool layer can perform cross-scope access intentionally.

### Search Isolation

Both content-search (`query_scoped_memories` with `content_search`) and FTS search (`Memory.search`) respect scope boundaries. Queries are filtered by `scope_kind` and `scope_key`, so one room's search never surfaces another room's content.

### Forgotten Content

Redacted (forgotten) memories:

- Are excluded from `query_scoped_memories` (list and content-search)
- Have their content set to NULL in the database
- Cannot be corrected or re-forgotten
- Show redacted status in `show` operations

**Known gap**: The `Memory.search` FTS path joins scoped memories by reference string but does not filter `redacted_at IS NULL`. A redacted memory that references an existing message can expose the underlying message content through FTS. This is a tracked issue (INV-REDACT-3b).

---

## 5. Memory in Prompts

When a user sends a message in a room session, the agent automatically receives relevant memory context as a system message. The injection path depends on whether the room is profiled.

### Profiled Rooms

For rooms with a profile binding, the agent receives scoped memory context:

1. The room ID and profile ID are resolved from the session.
2. `inject_search_context` queries scoped memories with the room's `scope_kind` and `scope_key`.
3. Visibility filtering is applied using the profile ID as the principal.
4. Matching memories are prepended as a system message:

```
Relevant scoped memory context:
[scoped-message:room/my-room-id] memory content here...
```

### Unprofiled Rooms

For rooms without a profile binding, a legacy path is used:

1. FTS keyword search across all messages
2. Vector search (if embedding provider is configured)
3. Core memories (global, not room-scoped) are included for awareness

### What Gets Injected

- Scoped memories matching the user's message content
- Scoped message references (messages stored with a `scope_kind`/`scope_key`)
- Core memories (up to 10, for unprofiled sessions)

Content is clipped to prevent excessively long injections.

---

## 6. Governance Readiness

The `rooms readiness` command produces a comprehensive report checking whether a room-agent is correctly configured.

```bash
# Full readiness report
clawq rooms readiness

# For a specific room
clawq rooms readiness --room-id <room_id>

# For a specific profile
clawq rooms readiness --profile-id <profile_id>

# JSON output
clawq rooms readiness --room-id <room_id> --json
```

### Checks Performed

| Check | What It Verifies |
|-------|-----------------|
| **Connector** | Room is bound to an active profile; profile exists |
| **Scope** | Room scope classification (dm, group, external, shared) |
| **Memory** | Memory scopes exist for the profile |
| **GitHub App** | GitHub app token is configured |
| **Repo Grants** | Repository access grants are set up |
| **Webhook Reachability** | Webhook endpoint is reachable |
| **Room Backlink** | Room is linked to a GitHub access bundle |
| **Budget** | Token/cost limits and current usage |
| **Activity Ledger** | Ledger schema is accessible |
| **Egress Audit** | Egress audit schema is accessible |
| **Routine** | Scheduled routines are configured and enabled |
| **Ambient** | Ambient watcher is enabled, no excessive failures |
| **Proxy Readiness** | Gateway or tunnel is configured for webhooks |

Each check reports `PASS`, `FAIL`, `WARN`, or `SKIP` with an actionable `fix_command` for failures and warnings.

---

## 7. Audit Export

The `rooms audit-export` command produces a governance audit export for a room, suitable for compliance review and debugging.

```bash
# Text output (default)
CLAWQ_ADMIN=1 clawq rooms audit-export <room_id>

# JSON output
CLAWQ_ADMIN=1 clawq rooms audit-export <room_id> --json

# JSONL output (one JSON object per line)
CLAWQ_ADMIN=1 clawq rooms audit-export <room_id> --jsonl
```

### Event Categories

| Category | Events |
|----------|--------|
| `scope_snapshot` | Room scope, binding status, profile info |
| `memory` | `memory_saved`, `memory_corrected`, `memory_forgotten`, `memory_hard_purged`, `scope_granted`, `scope_revoked`, `team_grant_added`, `team_grant_removed` |
| `github` | `github_update_delivered`, `github_update_skipped`, `github_update_denied` |
| `delivery` | `delivery_attempt`, `delivery_success`, `delivery_failure`, plus Teams/Ambient delivery lifecycle events |
| `setup` | `admin_denied`, `room_bound`, `room_unbound`, `profile_created`, `profile_deleted`, `profile_updated` |
| `policy` | `provider_request`, `provider_response`, `background_task_*` events |

### Redaction

The audit export applies automatic redaction to sensitive fields in event metadata:

- **Credential fields** (`token`, `bearer`, `api_key`, `secret`): partially masked
- **Reference/ID fields** (`reference`, `source_message_id`, `service_url`, `delivery_id`, etc.): partially masked (keep first/last 4 characters)

Bearer tokens in content previews are replaced with `[REDACTED]` and truncated to 200 characters.

### Export Format

The JSON export includes:

```json
{
  "room_id": "...",
  "exported_at": "2026-06-30T...",
  "scope_snapshot": { "scope": "group", "binding_active": true, ... },
  "events": [{ "category": "memory", "event_type": "memory_saved", ... }],
  "total_count": 42,
  "category_counts": { "memory": 10, "delivery": 20, ... }
}
```

The JSONL format puts the header (scope snapshot + counts) on the first line, followed by one event per line.

---

## 8. Setup Wizard

The `rooms wizard` command guides room-agent configuration through plan, apply, and rerun modes.

```bash
# Interactive wizard (default, admin-only)
CLAWQ_ADMIN=1 clawq rooms wizard

# Plan mode -- show what would happen, no side effects
clawq rooms wizard plan --profile-id <id> [options]

# Apply mode -- apply configuration changes (admin-only)
CLAWQ_ADMIN=1 clawq rooms wizard apply --profile-id <id> [options]

# Rerun mode -- compare desired vs current state
clawq rooms wizard rerun --profile-id <id> [--apply] [options]

# Validate delivery simulation
clawq rooms wizard validate-delivery --profile-id <id> [--connector C] [--room R]
```

### Plan Mode

`plan` shows what configuration changes would be made without writing anything. It runs readiness checks and reports any issues. No admin privileges required for read-only planning.

### Apply Mode

`apply` writes the configuration to disk and database. Requires `CLAWQ_ADMIN=1`. Readiness checks are run before applying; if any fail, the apply is aborted.

### Rerun Mode

`rerun` compares the desired state (from flags) against the current config and reports changed, blocked, and valid items. Use `--apply` to also write changes (requires admin).

### Common Options

| Flag | Description |
|------|-------------|
| `--profile-id ID` | Room profile ID (required) |
| `--model M` | Model identifier (default: `openai:gpt-5.4`) |
| `--system-prompt P` | Custom system prompt |
| `--max-iters N` | Max tool iterations per turn (1-1000, default: 25) |
| `--allowed-tools T1,T2` | Comma-separated allowlist |
| `--denied-tools T1,T2` | Comma-separated denylist |
| `--access-bundles B1,B2` | Access bundle IDs |
| `--token-limit N` | Token budget limit |
| `--cost-limit F` | Cost limit in USD |
| `--reset-period P` | Budget reset period (default: `monthly`) |
| `--connector C` | Connector type (default: `teams`) |
| `--room R` | Room ID to bind |
| `--inactive` | Create binding as inactive |

---

## Admin Gating

Many memory and governance operations require admin privileges, controlled by the `CLAWQ_ADMIN` environment variable.

```bash
# Set for a single command
CLAWQ_ADMIN=1 clawq rooms memory grant add ...

# Or export for the session
export CLAWQ_ADMIN=1
```

Operations requiring `CLAWQ_ADMIN=1`:

- **Grant management**: `grant add`, `grant revoke`, `grant list`, `team-grant add`, `team-grant remove`, `team-grant list`
- **Hard purge**: `memory forget --hard`
- **Audit export**: `rooms audit-export`
- **Wizard apply**: `rooms wizard apply`
- **Wizard interactive**: `rooms wizard` (default mode)

Operations that work without admin:

- `rooms memory list/show/save/correct/forget` (soft-delete)
- `rooms readiness`
- `rooms wizard plan` and `rooms wizard rerun` (without `--apply`)

When a non-admin user attempts an admin-only operation, the command returns an error and (for grant operations) logs an `admin_denied` event to the room activity ledger.

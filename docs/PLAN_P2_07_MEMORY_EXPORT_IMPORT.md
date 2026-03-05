# P2-07: Memory Export / Import

## Context

clawq's memory system (`memory.ml`) stores session history and supports FTS + vector search, but has no way to export memories to a portable format or import them on a new machine/instance. nullclaw has a `MEMORY_SNAPSHOT.json` export/import system with categories (core, session, transient) and auto-hydration on empty DB.

This plan adds snapshot export/import plus a memory category system to enable durable "core" facts that survive database resets.

## Design

### Memory Categories

Introduce a `category` column to the `messages` table (or a separate `core_memories` table):

```
core      — durable facts user explicitly stores ("remember that X")
session   — per-conversation history (existing messages table)
transient — short-lived context (ephemeral, TTL < 1 hour)
```

**Simplest approach:** Add a separate `core_memories` SQLite table alongside the existing `messages` table. Core memories are key-value facts, not message threads.

```sql
CREATE TABLE IF NOT EXISTS core_memories (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  key TEXT NOT NULL UNIQUE,
  content TEXT NOT NULL,
  category TEXT NOT NULL DEFAULT 'core',
  created_at INTEGER NOT NULL,
  updated_at INTEGER NOT NULL
);
CREATE INDEX IF NOT EXISTS core_memories_key ON core_memories (key);
```

### Snapshot Format

`MEMORY_SNAPSHOT.json` (in workspace root or `~/.clawq/`):
```json
{
  "version": 1,
  "exported_at": 1741302000,
  "entries": [
    {
      "key": "user_name",
      "content": "Alice",
      "category": "core",
      "created_at": 1741000000
    }
  ]
}
```

Only `core` category memories are exported (session history is transient by nature).

### Auto-Hydration

On daemon/agent startup: if `core_memories` table is empty and `MEMORY_SNAPSHOT.json` exists in workspace, auto-import. Log how many entries were loaded.

### Memory Tools (agent-visible)

Add explicit memory tools to `tools_builtin.ml` (currently memory is implicit via search context injection):

- `memory_store` — `{key: string, content: string}` → stores as core memory
- `memory_recall` — `{query: string}` → returns top matching core memories
- `memory_forget` — `{key: string}` → deletes core memory by key
- `memory_list` — `{}` → lists all core memory keys with previews

These tools allow agents to explicitly manage persistent knowledge.

### Export/Import CLI Commands

Extend `cmd_memory` in `command_bridge.ml`:

```
clawq memory export [--path <file>]   # exports core memories to JSON
clawq memory import [--path <file>]   # imports from JSON (merge, not replace)
clawq memory list                     # lists all core memories
clawq memory store <key> <content>    # store a core memory
clawq memory forget <key>             # delete a core memory
clawq memory stats                    # count by category, DB size
```

### Changes to `memory.ml`

New functions to add:
```ocaml
(* Core memory CRUD *)
val store_core : db:Sqlite3.db -> key:string -> content:string -> unit
val recall_core : db:Sqlite3.db -> query:string -> limit:int -> (string * string) list
val forget_core : db:Sqlite3.db -> key:string -> unit
val list_core : db:Sqlite3.db -> (string * string * int) list   (* key, preview, created_at *)

(* Export/Import *)
val export_snapshot : db:Sqlite3.db -> path:string -> int   (* returns count exported *)
val import_snapshot : db:Sqlite3.db -> path:string -> int   (* returns count imported *)
val should_hydrate : db:Sqlite3.db -> workspace:string -> bool
val auto_hydrate : db:Sqlite3.db -> workspace:string -> int
```

### Schema Migration

Add to `migrate.ml`:
- Migration version bump
- `CREATE TABLE IF NOT EXISTS core_memories ...` on upgrade

### Search Context Injection Update

Modify `agent.ml` search context injection to include top core memories alongside FTS session history results. Core memories have higher weight (they're explicitly stored facts).

### Export Format Versioning

`"version": 1` field allows future format changes. Import validates version and logs a warning if version is newer than supported.

## Files to Create/Modify

- **Modify**: `src/memory.ml` — add core_memories table + CRUD + export/import functions
- **Modify**: `src/migrate.ml` — add schema migration for `core_memories` table
- **Modify**: `src/tools_builtin.ml` — add `memory_store`, `memory_recall`, `memory_forget`, `memory_list` tools
- **Modify**: `src/tool_registry.ml` — register new memory tools (if not auto-registered)
- **Modify**: `src/command_bridge.ml` — implement `cmd_memory` subcommands (export, import, list, store, forget, stats)
- **Modify**: `src/agent.ml` — inject core memories into search context
- **Modify**: `src/daemon.ml` — call `Memory.auto_hydrate` on startup

## Test Strategy

1. **Schema**: `core_memories` table created fresh; migration from old schema preserves messages
2. **Store/recall**: store `{key="foo", content="bar"}` → recall with query "foo" returns it
3. **Forget**: store then forget → recall returns empty
4. **Export**: store 3 entries → export → verify JSON has `version=1` and 3 entries with correct fields
5. **Import**: write valid JSON file → import → verify entries in DB; duplicate key → upsert (no error)
6. **Import unknown version**: `version=99` → log warning, still attempt import
7. **Auto-hydration**: empty DB + snapshot file present → auto_hydrate returns count > 0; non-empty DB → skip
8. **Memory tools**: `memory_store` tool invocation → entry in DB; `memory_forget` tool → entry removed
9. **Agent context injection**: core memory present → appears in system message prefix on next turn

Run: `make test`. Add suite `memory_export` to `test/test_main.ml`.

## Dependencies

- No new opam packages. All via existing `sqlite3`, `yojson`.

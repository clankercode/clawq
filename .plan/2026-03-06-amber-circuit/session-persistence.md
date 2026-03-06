# Session Persistence Design

## Existing Schema (memory.ml, schema_version = 1)

```sql
CREATE TABLE IF NOT EXISTS messages (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  session_key TEXT NOT NULL,
  role TEXT NOT NULL,
  content TEXT NOT NULL,
  tool_call_id TEXT,
  tool_name TEXT,
  tool_calls_json TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
CREATE INDEX IF NOT EXISTS idx_messages_session_key ON messages (session_key);
```

This schema already captures full message content. The question is whether it is actively written
during conversations or only used for search (FTS/embeddings). **Verify during T-SR2 before
adding duplicate writes.**

## New Schema (schema_version = 2)

### session_state table

Tracks the live state of each conversation session across restarts.

```sql
CREATE TABLE IF NOT EXISTS session_state (
  session_key   TEXT PRIMARY KEY,
  turn          TEXT NOT NULL DEFAULT 'user',  -- 'user' | 'agent'
  channel       TEXT,                           -- 'telegram' | 'discord' | 'slack' | etc.
  channel_id    TEXT,                           -- chat_id for routing replies
  response_sent_at TEXT,                        -- NULL = agent hasn't sent response yet
  last_active   TEXT NOT NULL DEFAULT (datetime('now'))
);
```

**turn semantics:**
- `'user'` = waiting for a user message (or just sent a response)
- `'agent'` = agent is actively generating a response (LLM call in flight or pending)

**response_sent_at semantics:**
- `NULL` when `turn = 'agent'` means: agent owes a response, hasn't sent it yet
- Set to `datetime('now')` immediately after the channel send succeeds
- On startup resume: only resume sessions where `turn = 'agent' AND response_sent_at IS NULL`

### discord_resume_state table

Singleton row for Discord gateway RESUME across restarts.

```sql
CREATE TABLE IF NOT EXISTS discord_resume_state (
  id                INTEGER PRIMARY KEY CHECK (id = 1),
  session_id        TEXT NOT NULL,
  seq               INTEGER NOT NULL,
  resume_gateway_url TEXT NOT NULL,
  updated_at        TEXT NOT NULL DEFAULT (datetime('now'))
);
```

Always upsert with `id = 1`. Read on daemon startup before connecting to Discord.

## Schema Migration

In `memory.ml`, bump `current_schema_version` from 1 to 2. Add migration branch:

```ocaml
| 1 ->
  (* v1 -> v2: add session_state and discord_resume_state *)
  exec_exn db "CREATE TABLE IF NOT EXISTS session_state (...)";
  exec_exn db "CREATE TABLE IF NOT EXISTS discord_resume_state (...)";
  set_version db 2
```

## New module: session_persistence.ml

```ocaml
(** Save a single message to the messages table.
    No-op if db is None. Idempotent if called twice (duplicate INSERT is harmless
    for history reconstruction since we ORDER BY id). *)
val save_message :
  db:Sqlite3.db option ->
  session_key:string ->
  role:string ->
  content:string ->
  ?tool_call_id:string ->
  ?tool_name:string ->
  ?tool_calls_json:string ->
  unit -> unit

(** Load all messages for a session, ordered by id ASC.
    Returns [] if db is None or no messages found. *)
val load_history :
  db:Sqlite3.db option ->
  session_key:string ->
  Provider.message list

(** Upsert session_state row. *)
val upsert_session_state :
  db:Sqlite3.db option ->
  session_key:string ->
  turn:string ->
  ?channel:string ->
  ?channel_id:string ->
  ?response_sent_at:string ->
  unit -> unit

(** Mark response as sent (sets response_sent_at, turn = 'user'). *)
val mark_response_sent :
  db:Sqlite3.db option ->
  session_key:string ->
  unit -> unit

(** Load all sessions where agent owes a response and was recently active. *)
val load_pending_agent_sessions :
  db:Sqlite3.db option ->
  max_age_seconds:int ->
  (string * string option * string option) list  (* session_key, channel, channel_id *)
```

## History Reconstruction in session.ml

When `get_or_create_session session_key` is called and no in-memory session exists:

1. Call `Session_persistence.load_history ~db ~session_key`
2. If result is non-empty, call `Agent.create_with_history ~config ~history ~system_prompt ~tool_registry`
3. If result is empty, call `Agent.create` as before (new session)

This means `Agent.create_with_history` is a new variant (or `Agent.create` accepts optional
`~initial_history`).

## Turn State Updates in agent.ml / session.ml

Before LLM call in `Agent.run_turn`:
```ocaml
Session_persistence.upsert_session_state ~db ~session_key ~turn:"agent" ()
```

After channel send succeeds (in the per-channel handler, not in agent.ml):
```ocaml
Session_persistence.mark_response_sent ~db ~session_key ()
```

The channel send is the terminal action — only after a successful send do we mark it complete.
This ensures restarts retry unsent responses.

## Message Write in agent.ml

In `Agent.add_message` (or wherever messages are appended to history):
```ocaml
Session_persistence.save_message ~db ~session_key ~role ~content
  ?tool_call_id ?tool_name ?tool_calls_json ()
```

If the messages table is already written elsewhere (verify!), skip this and just ensure the
existing writer is called before drain.

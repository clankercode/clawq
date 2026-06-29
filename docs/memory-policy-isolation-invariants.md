# Memory Policy Isolation Invariants

**Status**: Authoritative  
**Last updated**: 2026-06-29  
**Owner**: P18.M3 (Memory Policy)

This document enumerates the invariants guaranteed by the memory subsystem
(`memory_scoped.ml`, `memory_core.ml`, `tools_builtin_room_memory.ml`), the
room budget system (`room_budget.ml`), the egress evaluator
(`egress_evaluator.ml`), the tool/agent filter layer (`agent_template.ml`,
`skills.ml`), and session lifecycle management. Most invariants link to runtime
tests that enforce them; exceptions are noted explicitly.

**Proof backlog**: See [`proof-backlog.md`](proof-backlog.md) for a catalogue
of invariants that are candidates for formal proof or stronger verification.

---

## 1. Memory Scope Isolation

**INV-MEM-ISO-1**: A room session can only list, show, correct, or forget
memories belonging to its own scope. The tool layer resolves the room id from
the session key and filters all operations to `scope_kind = "room"` and
`scope_key = <resolved_room_id>`.

- **Code**: `resolve_room_id_for_context` in `tools_builtin_room_memory.ml`
  resolves room id from session key. `check_room_access` verifies capability
  before any CRUD operation.
- **Test**: `test_cross_channel_list_isolation`,
  `test_cross_channel_show_isolation`,
  `test_cross_channel_correct_isolation`,
  `test_cross_channel_forget_isolation`,
  `test_cross_channel_save_isolation` — channel-b cannot see, show, correct,
  or forget channel-a memories.

**INV-MEM-ISO-2**: A memory saved by one room is never visible to another room,
regardless of visibility level (public, private, team). Scope boundaries are
absolute.

- **Code**: Tool layer filters `scope_kind` and `scope_key` in all query
  paths. `query_scoped_memories` SQL uses `s.key = ?` binding.
- **Test**: `test_three_room_isolation` — three rooms, each sees only its own
  memories.

**INV-MEM-ISO-3**: Raw memory API calls (`query_scoped_memories`,
`correct_scoped_memory`, `delete_scoped_memory`) do not enforce scope isolation
by themselves. Isolation is enforced at the tool layer
(`tools_builtin_room_memory.ml`). This is intentional: admin-level operations
may need cross-scope access.

- **Code**: `tools_builtin_room_memory.ml` checks `scope_kind`/`scope_key`
  match before calling raw API. Raw API accepts any `scope_id` or `id`.
- **Test**: `test_raw_correct_works_at_raw_level`,
  `test_raw_delete_works_at_raw_level` — raw cross-scope operations succeed,
  confirming isolation is tool-layer enforcement.

**INV-MEM-ISO-4**: Cross-channel search (FTS and content-search) is scoped.
Memory search results are filtered by `scope_kind` and `scope_key`, so one
channel's search never surfaces another channel's content.

- **Code**: `query_scoped_memories` applies `scope_kind` and `scope_key`
  WHERE clauses. `Memory.search` (FTS path) also accepts scope filters.
- **Test**: `test_cross_channel_search_isolation` — search for "zyx987" in
  channel-a returns only channel-a's result; channel-b returns only channel-b's.

---

## 2. Memory Visibility Levels

**INV-VIS-1**: Three visibility levels exist: `Public`, `Private`, `Team`.
Default is `Public`.

- **Code**: `memory_visibility` type in `memory_types.ml`. Default applied in
  `upsert_scoped_memory` when no visibility is specified.
- **Test**: `test_private_memory_not_visible_in_list` — public memory visible,
  private memory hidden.

**INV-VIS-2**: Private memory is visible only to the scope owner (the profile
bound to the scope). Non-owner callers cannot see private memories in list or
show operations.

- **Code**: `can_see_memory` in `memory_scoped.ml` checks
  `scope_profile_id` match for `Private` visibility. Tool layer calls
  `can_see_memory` before returning results.
- **Test**: `test_private_memory_not_visible_in_list`,
  `test_private_memory_not_visible_in_show`.

**INV-VIS-3**: Team memory is visible only to principals with an explicit team
grant (`memory_team_grants` table). Without a grant, team memory is hidden.

- **Code**: `can_see_memory` calls `has_team_grant` for `Team` visibility.
  `has_team_grant` queries `memory_team_grants` table.
- **Test**: `test_team_memory_not_visible_without_grant`,
  `test_team_memory_visible_with_grant`.

**INV-VIS-4**: Team grants do not override scope isolation. A team grant for
room-a on a memory in room-b's scope does not make room-b's memory visible to
room-a, because the tool layer filters by scope before checking visibility.

- **Code**: Tool layer checks `scope_kind`/`scope_key` match before
  visibility filter.
- **Test**: `test_team_memory_grant_for_different_room`.

**INV-VIS-5**: Visibility is preserved on upsert when no explicit visibility is
provided. Changing visibility requires an explicit visibility parameter.

- **Code**: `upsert_scoped_memory` only updates visibility column when
  `visibility = Some _`. Otherwise the existing value is preserved.
- **Test**: `test_private_memory_preserved_after_save`,
  `test_private_memory_visibility_change_to_public`.

---

## 3. Repo Grants (Memory Grant Resolution)

**INV-GRANT-1**: Memory grants are direct, not transitive. Granting access to
a scope does not transitively grant access to that scope's child grants.

- **Code**: `resolve_grants` queries `memory_grants` for the specific
  `(scope_id, principal_kind, principal_id)` tuple. No recursive traversal.
- **Test**: `test_memory_grants_are_direct_not_transitive` in
  `test/test_scope_resolver.ml` — only the directly referenced bundle's grants
  are collected, not child scope grants.

**INV-GRANT-2**: Grant mutations (`grant_access`, `revoke_access`) require
admin privileges. Non-admin callers receive an error and no grant is created or
removed.

- **Code**: `require_memory_grant_admin` checks `is_admin` flag. Both
  `grant_access` and `revoke_access` gate on this check.
- **Test**: `test_grant_denied_no_event` — non-admin grant produces error, no
  ledger event.

**INV-GRANT-3**: Expired grants are excluded from resolution. Grants with
`expires_at` in the past are filtered out by the `resolve_grants` SQL query.

- **Code**: `resolve_grants` SQL includes
  `AND (expires_at IS NULL OR datetime(expires_at) > datetime('now'))`.
- **Test**: No dedicated test for expired grant filtering. The invariant is
  enforced by the SQL clause.

**INV-GRANT-4**: Revoked grants are excluded from resolution when the
`revoked_at` column exists. The resolver dynamically checks for column
existence and adds the `revoked_at IS NULL` clause.

- **Code**: `resolve_grants` calls `sqlite_column_exists` to check for
  `revoked_at` column before adding the filter.
- **Test**: No dedicated test. The invariant is enforced by the dynamic SQL
  construction.

---

## 4. Credential Non-Disclosure

**INV-CRED-1**: Memory content previews in ledger events have Bearer tokens
redacted. The `sanitize_content_preview` function replaces patterns matching
`Bearer [A-Za-z0-9._+/=-]+` with `[REDACTED]`.

- **Code**: `sanitize_content_preview` in `memory_scoped.ml` uses
  `Str.global_replace` with `Bearer [A-Za-z0-9._+/=-]+` regex.
- **Test**: `test_content_preview_sanitizes_bearer_token` — Bearer token
  replaced with `[REDACTED]`, original value absent from preview.

**INV-CRED-2**: Content previews are truncated to 200 characters (plus `...`
suffix) to prevent accidental disclosure of long sensitive content in ledger
events.

- **Code**: `sanitize_content_preview` with `~max_len = 200`.
- **Test**: `test_content_preview_truncates_long_content` — 300-char content
  becomes 203-char preview (200 + "...").

**INV-CRED-3**: Redacted (forgotten) memories have their content set to NULL
and provenance scrubbed of any `prev_content` entries. The original content is
not recoverable through scoped memory APIs (`get_scoped_memory`,
`query_scoped_memories`, `room_memory_show`, `room_memory_list`). The FTS
search path (`Memory.search`) is an exception — see INV-REDACT-3b for the
current gap.

- **Code**: `redact_scoped_memory` SQL sets `content = NULL`, scrubs
  provenance with `CASE WHEN provenance LIKE '%prev_content:%' THEN
  'redacted'`.
- **Test**: `test_forgotten_memory_show_returns_redacted` — original content
  not present in show response.
- **Note**: Ledger events emitted during the redaction operation itself
  (`memory_forgotten`) include a sanitized `content_preview` of the
  pre-redaction content (with Bearer tokens redacted and truncated to 200
  chars). Non-Bearer content remains visible through ledger APIs. This is
  intentional for audit trail purposes.

---

## 5. Egress Default-Deny Monotonicity

**INV-EGR-1**: The egress evaluator defaults to `Deny` when no rule matches.
First-match-wins semantics with deny as the catch-all ensures that new
destinations are blocked by default.

- **Code**: `evaluate` in `egress_evaluator.ml` returns
  `{ action = Deny; log_policy = Log; matched_rule_index = -1 }` when no
  rule matches.
- **Test**: No dedicated egress evaluator test file. The invariant is enforced
  by the `evaluate` function's fallthrough case.

**INV-EGR-2**: Unmatched destinations remain denied regardless of rule changes.
Adding a new egress rule can change a previously default-denied destination to
allowed (if the new rule matches), but destinations that still match no rule
continue to receive the default-deny action.

- **Code**: `evaluate` returns `Deny` only from the no-match fallthrough.
  A new matching rule takes precedence over the fallthrough for its specific
  host/path/method, but does not alter behavior for other unmatched
  destinations.
- **Note**: This is a structural invariant of the evaluator, not tested
  directly.

**INV-EGR-3**: Egress rules are evaluated in declaration order (first match
wins). Rule ordering in the configuration determines which rule applies to a
given request.

- **Code**: `find_first_match` in `evaluate` iterates rules in list order.
- **Note**: No dedicated test for rule ordering. The invariant is enforced by
  the list traversal.

---

## 6. MCP Filter Soundness

**INV-FILTER-1**: Agent templates filter the tool registry by allowed and
denied tool lists. Denied tools are removed after allowed filtering, ensuring
deny wins over allow.

- **Code**: `filter_tool_registry` in `agent_template.ml` first filters to
  allowed tools (if non-empty), then removes denied tools from the result.
- **Test**: `test_layers_merge_deterministically_and_deny_wins` and
  `test_allow_and_deny_same_tool_denies` in `test/test_scope_resolver.ml` —
  deny wins over allow in merged bundles.

**INV-FILTER-2**: Skills can filter visible tools via
`Skills.filter_visible_tools`. This is an additional layer on top of agent
template filtering.

- **Code**: `filter_visible_tools` in `skills.ml`.
- **Test**: No dedicated test for skills filter. Used by Teams connector
  (`teams.ml`).

**INV-FILTER-3**: MCP client tools are discovered and filtered at connection
time. `tool_of_mcp_definition` converts MCP tool definitions into the
internal `Tool.t` representation. Invalid definitions are filtered out
(`filter_map`).

- **Code**: `mcp_client.ml` uses `List.filter_map` on discovered tools.
- **Test**: No dedicated MCP filter test. The invariant is enforced by the
  `filter_map` pattern.

---

## 7. Budget Invariants

**INV-BUDG-1**: Room budgets are scoped to room profiles (`profile_id` is the
primary key). Each profile has at most one budget entry.

- **Code**: `room_budgets` table has `profile_id INTEGER PRIMARY KEY` with
  `FOREIGN KEY` referencing `room_profiles(id) ON DELETE CASCADE`.
- **Test**: No dedicated budget invariant test. Schema constraint enforces
  uniqueness.

**INV-BUDG-2**: Budget limits have non-negative constraints.
`token_limit >= 0` and `cost_limit_usd >= 0.0` are enforced by CHECK
constraints.

- **Code**: `CREATE TABLE room_budgets ... CHECK(token_limit >= 0) ...
  CHECK(cost_limit_usd >= 0.0)`.
- **Test**: Schema-level enforcement.

**INV-BUDG-3**: Budget enforcement tracks cumulative usage (tokens, cost,
turns) per profile per reset period. When limits are exceeded, further requests
are blocked (`limit_exceeded = true`).

- **Code**: `room_budget.ml` tracks `current_usage` and compares against
  limits.
- **Test**: No dedicated invariant test. Budget logic is tested through
  integration paths.

**INV-BUDG-4**: Soft warning threshold defaults to 80%
(`default_soft_warn_threshold_pct = 0.8`). When usage exceeds the soft
threshold but not the hard limit, a warning is surfaced without blocking.

- **Code**: `default_soft_warn_threshold_pct` constant in `room_budget.ml`.
- **Test**: Schema default enforces the threshold.

**INV-BUDG-5**: Budget period reset is explicit. `reset_profile_budget` must
be called to start a new period; it advances `period_started_at` to the
current time. Usage accumulates within a period and only resets when
`reset_profile_budget` is invoked. The `limit_exceeded` flag is derived by
`get_profile_budget` (not persisted), so advancing the period start causes
usage and derived limit status to be recalculated for the new period.

- **Code**: `reset_profile_budget` in `room_budget.ml` updates
  `period_started_at` and `updated_at`. `get_profile_budget` derives
  `limit_exceeded` from current usage vs. limits.
- **Test**: No dedicated invariant test. Budget reset is exercised through
  integration paths.

---

## 8. Session Lifecycle

**INV-SESS-1**: `clear_session` removes all session data: messages, session
state, workspace state, inbound queue, session repos, session log epochs (and
their child messages), and summary store entries. The deletes are sequential
(not wrapped in a transaction), so a mid-operation failure can leave partial
data. This is acceptable because `clear_session` is idempotent — re-invoking
it cleans up remaining rows.

- **Code**: `clear_session` in `memory_core.ml` deletes from tables in
  sequence: `session_log_epoch_messages`, `session_log_epochs`,
  `messages`, `session_state`, `session_workspace_state`, `inbound_queue`,
  `session_repos`, and calls `Summary_store.delete_for_session`.
- **Test**: No dedicated session lifecycle invariant test. The function is
  tested through integration paths.

**INV-SESS-2**: Session cleanup respects age and message-count limits.
`cleanup_session` deletes messages older than `max_age_days` and trims to
`max_messages`, preserving tool-group integrity.

- **Code**: `cleanup_session` in `memory_core.ml` uses
  `Message_history.ensure_tool_group_integrity` and
  `Message_history.expand_keep_for_tool_groups` to maintain coherent
  tool-call/result groups during trimming.
- **Test**: No dedicated cleanup invariant test.

**INV-SESS-3**: Message replacement (`replace_session_messages`) archives the
existing history into session log epochs before deleting and replacing. This
ensures no message data is lost during context window management.

- **Code**: `replace_session_messages` calls `archive_session_epoch` on
  existing messages before `DELETE FROM messages` and re-insert.
- **Test**: No dedicated archival invariant test.

**INV-SESS-4**: The `FOREIGN KEY` constraint on
`room_profile_bindings -> room_profiles` with `ON DELETE CASCADE` ensures that
deleting a room profile cascades to its bindings. The `PRAGMA foreign_keys = ON`
enforcement at connection init ensures this is active.

- **Code**: `memory_core.ml` executes `PRAGMA foreign_keys = ON` during
  `init`. `delete_room_profile` in `memory_core.ml` also manually deletes
  bindings before the profile row.
- **Test**: No dedicated FK cascade test.

---

## 9. Redaction (Forgetting) Invariants

**INV-REDACT-1**: Redaction is idempotent. Redacting an already-redacted
memory returns `false` (no change). The SQL `WHERE redacted_at IS NULL` clause
prevents double-redaction.

- **Code**: `redact_scoped_memory` SQL includes
  `WHERE id = ? AND redacted_at IS NULL`.
- **Test**: `test_forgotten_memory_cannot_be_forgotten_again`.

**INV-REDACT-2**: Redacted memories cannot be corrected. The `correct`
operation checks `redacted_at` and rejects corrections to redacted memories.

- **Code**: Tool layer in `room_memory_correct` checks
  `m.redacted_at <> None` before allowing correction.
- **Test**: `test_forgotten_memory_cannot_be_corrected`.

**INV-REDACT-3**: Redacted memories are excluded from `query_scoped_memories`
(list, search by content). The `m.redacted_at IS NULL` clause is mandatory in
that path.

- **Code**: `query_scoped_memories` always includes
  `"m.redacted_at IS NULL"` in the WHERE clause.
- **Test**: `test_forgotten_memory_not_in_search`,
  `test_forgotten_memory_not_in_list`.

**INV-REDACT-3b**: The `Memory.search` FTS path (`memory_search.ml`) joins
scoped memories via reference but does **not** filter `sm.redacted_at IS NULL`.
This is a known gap: `Memory.search` queries `messages_fts` (which indexes
`messages.content`) and joins `scoped_memories` by reference string. Redacting
`scoped_memories.content` does **not** clear `messages.content` or its FTS
index row, so a redacted scoped memory that references an existing message can
still expose the underlying message content through the FTS search path.

- **Code**: `search` in `memory_search.ml` — no `redacted_at` filter in the
  scoped-memory join. The join condition matches on reference string, not on
  redaction status.
- **Note**: `test_forgotten_memory_not_in_fts_search` is misnamed — it tests
  `query_scoped_memories` (content-search path), not the FTS path. A
  dedicated FTS redaction test is needed to surface this gap. A fix would
  add `AND sm.redacted_at IS NULL` to the scoped-memory join or clear
  `messages.content` during redaction.

**INV-REDACT-4**: Redaction clears the content to NULL and sets the
`redacted_at` timestamp and `redaction_reason`. The reference is preserved
(or set to `redacted:<id>` if previously NULL) for audit trail continuity.
Provenance is scrubbed of any `prev_content` entries (set to `'redacted'`).
Note: `redaction_metadata` is a read-only column (populated by migrations);
`redact_scoped_memory` does not write to it.

- **Code**: `redact_scoped_memory` SQL sets `content = NULL`,
  `redacted_at = datetime('now')`, `redaction_reason = ?`,
  `reference = COALESCE(reference, 'redacted:' || id)`,
  `provenance = CASE WHEN provenance LIKE '%prev_content:%' THEN 'redacted'
  ELSE provenance END`.

---

## 10. Ledger Audit Trail

**INV-LEDGER-1**: All memory mutation operations (save, correct, redact, hard
delete, grant, revoke, team grant add/remove) emit ledger events when a ledger
function is provided. No-ledger calls produce no events.

- **Code**: `emit_memory_event` and `emit_grant_event` in
  `memory_scoped.ml` check for `?ledger = Some _` before emitting.
- **Test**: `test_save_emits_memory_saved`,
  `test_correct_emits_memory_corrected`,
  `test_forget_emits_memory_forgotten`,
  `test_hard_delete_emits_memory_hard_purged`,
  `test_save_no_ledger_no_event`.

**INV-LEDGER-2**: Ledger events record the actor, scope kind, scope key,
memory id, and visibility. Content previews are sanitized (Bearer redaction)
and truncated.

- **Code**: `emit_memory_event` constructs metadata with `memory_id`,
  `scope_kind`, `scope_key`, `principal`, `visibility`, `content_preview`.
  `sanitize_content_preview` handles redaction and truncation.
- **Test**: `test_save_emits_memory_saved` — checks all metadata fields.
  `test_content_preview_sanitizes_bearer_token`,
  `test_content_preview_truncates_long_content`.

**INV-LEDGER-3**: Grant and revoke events record `principal_kind`,
`principal_id`, and `capability` in metadata.

- **Code**: `emit_grant_event` constructs metadata with scope, principal, and
  capability fields.
- **Test**: `test_grant_emits_scope_granted`,
  `test_revoke_emits_scope_revoked`.

---

## 11. Unowned Scope Access

**INV-UNOWNED-1**: Room scopes without a profile binding (unowned) deny
access by default. The `check_room_access` function requires either a profile
binding (ownership) or an explicit grant for the requested capability.

- **Code**: `check_room_access` in `tools_builtin_room_memory.ml` checks
  profile binding first, then falls back to direct grant lookup.
- **Test**: `test_unowned_scope_no_access_without_grant`.

**INV-UNOWNED-2**: Unowned scopes can be accessed via explicit direct grants
(`grant_access` with `principal_kind:"room"`).

- **Code**: `check_room_access` falls back to direct room grant when no
  profile binding exists.
- **Test**: `test_unowned_scope_with_direct_grant`.

---

## 12. Scope Resolver Isolation

**INV-SRES-1**: The room id resolver produces distinct room ids for distinct
session keys. `resolve_room_id_for_context` parses the session key or queries
the session DB to determine the room id.

- **Code**: `resolve_room_id_for_context` in `tools_builtin_room_memory.ml`
  tries `get_session_channel` first, then parses session key format
  `channel:room-id`.
- **Test**: `test_scope_resolver_isolates_different_room_ids`.

---

## Appendix: Test File Locations

| Test file | Coverage |
|-----------|----------|
| `test/test_memory_isolation.ml` | INV-MEM-ISO, INV-VIS, INV-REDACT, INV-UNOWNED, INV-SRES |
| `test/test_memory_ledger.ml` | INV-LEDGER, INV-CRED |
| `test/test_scope_resolver.ml` | INV-GRANT (via scope resolution), INV-FILTER (via deny-wins) |
| `test/test_access_snapshot.ml` | Snapshot immutability (referenced by scope-resolution-invariants) |
| `src/memory_scoped.ml` | Scoped memory CRUD, grant resolution, visibility checks, redaction |
| `src/memory_core.ml` | Session lifecycle, core memory, cleanup |
| `src/tools_builtin_room_memory.ml` | Room memory tools, scope resolution, access control |
| `src/egress_evaluator.ml` | Egress default-deny evaluation |
| `src/agent_template.ml` | Tool registry filtering (allowed/denied) |
| `src/room_budget.ml` | Room budget tracking and enforcement |

## Known Gaps

- **INV-GRANT-3**: No dedicated test for expired grant exclusion from
  resolution. The SQL clause enforces it, but a unit test would strengthen
  the guarantee.
- **INV-GRANT-4**: No dedicated test for revoked grant exclusion when
  `revoked_at` column exists. Dynamic SQL construction handles it, but a
  test with column present/absent would be valuable.
- **INV-EGR-1**: No dedicated egress evaluator unit test. The default-deny
  fallthrough is structural but untested in isolation.
- **INV-FILTER-2**: Skills filter has no dedicated test. Used by Teams
  connector but not independently verified.
- **INV-BUDG-1 through INV-BUDG-5**: Budget invariants are enforced by
  schema constraints and runtime logic but lack dedicated invariant tests.
- **INV-SESS-1 through INV-SESS-4**: Session lifecycle invariants are
  structural but lack dedicated tests. Integration paths exercise them
  indirectly.

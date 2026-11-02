# GitHub session identity and legacy history recovery

GitHub events retain logical keys such as `github:acme/backend:pr:42` and route
messages to the original `acme/backend` repository. Session turns use the
existing canonical key format, such as `github:acme_backend:pr:42`, throughout
locking, activity tracking, queues, tools, access snapshots, and persistence.
Buffered turns, streaming turns, try-turns, and restart resume share this
identity. A subsequent turn restores its history after restart or idle-session
eviction. GitHub notifier registration uses the same canonical key; API owner,
repository, channel routing, and event provenance retain their original values.

Older versions could persist current history under the raw owner/repo key while
restoring only the canonical key. When a session manager starts with a database,
it reconciles those legacy GitHub keys before pending-session enumeration or
lazy restoration. It uses the exact session sanitizer to find aliases.

| Persisted state | Recovery behavior |
| --- | --- |
| One raw identity, no canonical history or conflicting work/reset evidence | Promote the legacy history and associated live state to the canonical key. A canonical settings-only row does not block recovery. |
| Both raw and canonical current history | Use canonical history. Preserve the raw branch separately; never concatenate or deduplicate the branches into live context. |
| Multiple raw aliases map to one canonical key | Preserve the raw identities and record a diagnostic. Do not choose a branch automatically. |
| Canonical archive, reset, compaction, prior reconciliation, or conflicting live routing/metadata | Preserve the legacy branch for an explicit recovery decision. An empty current history is insufficient evidence that importing it is safe. |
| Raw data reappears after a recorded promotion | Hold the new raw branch without reimporting it or replacing canonical history. |

An unambiguous promotion changes ownership in a checked SQLite transaction.
Message IDs, timestamps, tool-call payloads, provider response JSON, thinking
content, epoch IDs, queue IDs, and embedding message references are retained.
The optional full-text index is updated with the message ownership change.
Workspace state, pending questions, repository bindings, summaries, and task
trees move with the recovered session where present. Canonical settings take
precedence; missing model/CWD values can be filled from legacy state. The work
and channel-routing fields transfer as one tuple. Displaced source metadata is
saved in the reconciliation receipt. A failed write rolls the transaction back
and surfaces the failure instead of exposing a partially reconciled manager.
Normal tool-group integrity repair still runs when history is loaded.

Reconciliation decisions persist in `session_key_reconciliations`, with the raw
key, canonical key, decision, reason, and source metadata snapshot. Held raw
branches remain inspectable in their original tables. Automatic pending-session
resume, durable inbound replay/claim, keepalive, and heartbeat selection exclude
those raw identities. Their queue rows and payloads are retained. Startup logs
identify held keys and the diagnostic table. Operators can inspect decisions
with a read-only query on the relevant database:

```sql
SELECT raw_key, canonical_key, decision, reason
FROM session_key_reconciliations
ORDER BY created_at, raw_key;
```

Resetting canonical history does not discard the reconciliation record or revive
a competing raw branch. Recovery of a held branch requires an operator decision;
this change supplies no automatic history merge or dedicated recovery command.

The existing sanitizer is lossy: for example, workflow names `ci/a` and `ci_a`
can map to the same canonical key. This repair preserves the established forward
key format and handles known persisted ambiguities conservatively. It does not
introduce a new collision-free identifier scheme. Historical audit/provenance
records and unrelated connector histories are not rewritten.

Focused regressions use an in-process mock provider, temporary HOME/workspace
directories, and isolated in-memory or temporary file databases. They exercise
real turn, tool, streaming, restart, eviction, and legacy reconciliation paths,
including rollback and full-text indexing, without accessing a live user store.

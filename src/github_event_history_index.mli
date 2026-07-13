(** Indexed Room/item journal history for current and compacted Session context
    (P19.M3.E1.T004).

    Queries the durable [github_room_event_journal] (and matching item
    projections) so a Session can bootstrap after compaction with relevant
    GitHub event history. Webhook delivery never wakes the agent; this module
    only reads.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type context_slice = {
  room_id : string;
  item_key : string option;
  entries : Github_room_event_journal.journal_entry list;
  projections : Github_item_projection.projection list;
  truncated : bool;
}

val ensure_schema : Sqlite3.db -> unit
(** Additional indexes if needed on journal; optional FTS skip — use SQL
    indexes. Idempotent. *)

val history_for_room :
  db:Sqlite3.db ->
  room_id:string ->
  ?before:string ->
  ?limit:int ->
  unit ->
  (Github_room_event_journal.journal_entry list, string) result
(** Room journal history. Optional exclusive [before] [created_at] cursor. When
    [limit] is set, returns the most recent [limit] rows ordered chronological
    ASC. *)

val history_for_item :
  db:Sqlite3.db ->
  room_id:string ->
  item_key:string ->
  ?limit:int ->
  unit ->
  (Github_room_event_journal.journal_entry list, string) result
(** Item-scoped journal history for a room. When [limit] is set, most recent
    [limit] rows, chronological ASC. *)

val context_for_session :
  db:Sqlite3.db ->
  room_id:string ->
  ?item_key:string ->
  ?limit:int ->
  unit ->
  (context_slice, string) result
(** Load recent journal + matching projections for compacted session bootstrap.
    [truncated] is true when more journal rows exist beyond [limit] (default
    50). *)

val format_context_block : context_slice -> string
(** Safe text block for session preamble; no secrets/bodies. *)

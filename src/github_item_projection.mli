(** Deterministic per-Room item projections from the event journal
    (P19.M3.E1.T002).

    Cards are visible projections over journal history, not separate Sessions.
    Lifecycle events create/update a card with [card_kind = Lifecycle]; minor
    updates edit the current card with [card_kind = Update]. Replay is
    deterministic: same journal order yields the same projection state.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type card_kind = Lifecycle | Update

type projection = {
  room_id : string;
  item_key : string;
  title : string option;
  state : string option;
  draft : bool option;
  merged : bool option;
  labels : string list;
  assignees : string list;
  head_sha : string option;
  html_url : string option;
  last_event_at : string option;
  last_family : Github_event_envelope.family option;
  comment_count : int;
  revision : int;  (** monotonic reduce counter *)
  card_kind : card_kind;  (** last applied effect *)
}

val ensure_schema : Sqlite3.db -> unit
(** [github_item_projections] table UNIQUE([room_id], [item_key]). Idempotent.
*)

val reduce_entry :
  db:Sqlite3.db ->
  entry:Github_room_event_journal.journal_entry ->
  unit ->
  (projection, string) result
(** Load envelope from [entry.envelope_json]; apply fold rules:
    - Lifecycle (open/reopen/transfer/close/...) → upsert, [card_kind]
      [Lifecycle]
    - Comment → increment [comment_count], [card_kind] [Update]
    - Ci/Commit/State_update/Review → merge safe [after] state, [card_kind]
      [Update] Deterministic: same journal order → same projection. *)

val reduce_room :
  db:Sqlite3.db -> room_id:string -> (projection list, string) result
(** Clear room projections and replay journal chronologically via
    [reduce_entry]. *)

val get :
  db:Sqlite3.db ->
  room_id:string ->
  item_key:string ->
  (projection option, string) result

val list_for_room :
  db:Sqlite3.db -> room_id:string -> (projection list, string) result

(** Durable journal of GitHub events routed into Room Sessions (P19.M3.E1.T001).

    Matched normalized envelopes are appended chronologically as hidden [event]
    messages on the Room Session. A durable indexed journal preserves history
    after Session compaction. Webhook delivery never wakes the agent.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type journal_entry = {
  id : string;
  room_id : string;
  delivery_id : string option;
  item_key : string;
  envelope_json : Yojson.Safe.t;  (** safe envelope serialization *)
  route_id : string option;
  created_at : string;
  session_message_id : string option;
}

val ensure_schema : Sqlite3.db -> unit
(** Table [github_room_event_journal] + indexes on [room_id], [delivery_id],
    [item_key]. Idempotent. *)

val append :
  db:Sqlite3.db ->
  room_id:string ->
  envelope:Github_event_envelope.t ->
  ?route_id:string ->
  ?session_append:(room_id:string -> content:string -> (string, string) result) ->
  ?now:float ->
  unit ->
  (journal_entry, string) result
(** 1. Serialize envelope to safe JSON (no bodies/secrets) 2. INSERT journal row
    (idempotent on room+delivery+item if delivery present) 3. If
    [session_append] provided, append hidden event message; store message id 4.
    Never invoke agent/wake

    Duplicate delivery returns the existing entry [Ok]. *)

val list_for_room :
  db:Sqlite3.db ->
  room_id:string ->
  ?limit:int ->
  unit ->
  (journal_entry list, string) result
(** Chronological order (created_at ASC, id ASC). *)

val get_by_delivery :
  db:Sqlite3.db ->
  room_id:string ->
  delivery_id:string ->
  item_key:string ->
  (journal_entry option, string) result

val format_hidden_event_message : Github_event_envelope.t -> string
(** Short hidden event body for session transcript (no secrets/bodies). *)

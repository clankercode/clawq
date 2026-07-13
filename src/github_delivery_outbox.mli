(** 24-hour retrying delivery outbox with per-event dead letters
    (P19.M3.E3.T001).

    Durable, independent of webhook ACK: intents are enqueued after routing and
    retried with exponential backoff until success or until the entry ages past
    [default_max_age_seconds] (24h), at which point they become independently
    inspectable dead letters.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type status = Pending | In_flight | Succeeded | Dead_letter | Superseded

type entry = {
  id : string;
  room_id : string;
  item_key : string;
  intent_json : Yojson.Safe.t;
  status : status;
  attempts : int;
  next_attempt_at : string;
  created_at : string;
  last_error : string option;
  dead_lettered_at : string option;
}

val default_max_age_seconds : float
(** 24h = 86400. *)

val ensure_schema : Sqlite3.db -> unit

val enqueue :
  db:Sqlite3.db ->
  room_id:string ->
  item_key:string ->
  intent:Github_delivery_intent.intent ->
  ?now:float ->
  unit ->
  (entry, string) result
(** Insert a Pending outbox row due immediately. Uses [intent.id] as the row
    primary key so re-enqueue of the same intent is unique/idempotent. *)

val claim_due :
  db:Sqlite3.db ->
  ?now:float ->
  ?limit:int ->
  unit ->
  (entry list, string) result
(** Pending/In_flight with [next_attempt_at] <= now; mark In_flight. Supports
    restart recovery of stale in-flight rows whose attempt time is due. *)

val mark_success :
  db:Sqlite3.db -> id:string -> ?now:float -> unit -> (unit, string) result

val mark_failure :
  db:Sqlite3.db ->
  id:string ->
  error:string ->
  ?now:float ->
  ?max_age_seconds:float ->
  unit ->
  (entry, string) result
(** Increment attempts; schedule exponential backoff
    [min(3600, 30 * 2^attempts)]; if age > max age (default 24h) → Dead_letter.
    Error strings are redacted so secrets never land in storage. *)

val list_dead_letters :
  db:Sqlite3.db -> ?limit:int -> unit -> (entry list, string) result

val count_open_for_item :
  db:Sqlite3.db -> room_id:string -> item_key:string -> (int, string) result
(** Count Pending + In_flight rows for [room_id]/[item_key]. Used by catch-up
    reconciliation to measure collapsed backlog. *)

val supersede_pending_for_item :
  db:Sqlite3.db -> room_id:string -> item_key:string -> (int, string) result
(** Mark all Pending/In_flight rows for the item as [Superseded]. Returns the
    number of rows updated. Succeeded and dead-letter rows are left alone. *)

val mark_superseded : db:Sqlite3.db -> id:string -> (unit, string) result
(** Mark a single Pending/In_flight row as [Superseded]. *)

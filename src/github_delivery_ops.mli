(** Delivery outbox diagnostics, metrics, and repair helpers (P19.M3.E3.T003).

    Operator-facing counts and human diagnostics over the 24h delivery outbox
    and dead letters, plus repair for stuck [In_flight] rows and manual requeue
    of dead letters.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type metrics = {
  pending : int;
  in_flight : int;
  succeeded : int;
  dead_letter : int;
  superseded : int;
}

val metrics :
  db:Sqlite3.db -> ?room_id:string -> unit -> (metrics, string) result
(** Status counts across the outbox, optionally restricted to [room_id]. *)

val diagnose : db:Sqlite3.db -> ?room_id:string -> unit -> string list
(** Human lines: counts, oldest pending age, dead letter samples. *)

val default_stale_in_flight_seconds : float
(** Default age threshold for [repair_stale_in_flight] (300s). *)

val repair_stale_in_flight :
  db:Sqlite3.db ->
  ?older_than_seconds:float ->
  ?now:float ->
  unit ->
  (int, string) result
(** Requeue [In_flight] rows whose [next_attempt_at] is older than
    [now - older_than_seconds] back to [Pending]. Returns rows updated. *)

val requeue_dead_letter :
  db:Sqlite3.db -> id:string -> ?now:float -> unit -> (unit, string) result
(** Move a [Dead_letter] row back to [Pending], due immediately. Keeps attempt
    count and last_error for audit; clears [dead_lettered_at]. *)

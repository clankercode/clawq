(** Catch-up reconciliation: one current-state delivery intent per item
    (P19.M3.E3.T002).

    After restart or missed delivery events, collapse pending outbox backlog
    into a single connector-neutral intent that reflects the current item
    projection — not a flood of historical per-event deliveries.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type catchup = {
  room_id : string;
  item_key : string;
  intent : Github_delivery_intent.intent;
  collapsed_from : int;  (** number of journal/outbox events collapsed *)
}

val plan_catchup_for_room :
  db:Sqlite3.db ->
  room_id:string ->
  ?now:float ->
  unit ->
  (catchup list, string) result
(** For each projection in room, emit at most one Update_card/Create intent
    reflecting current projection state (collapse pending outbox for same item).
*)

val reconcile_room :
  db:Sqlite3.db -> room_id:string -> ?now:float -> unit -> (int, string) result
(** [plan_catchup] → cancel/supersede pending outbox rows for those items →
    enqueue one catchup intent each. Returns number enqueued. *)

val supersede_pending_for_item :
  db:Sqlite3.db -> room_id:string -> item_key:string -> (int, string) result
(** Mark Pending/In_flight outbox rows for the item as superseded. Returns
    count. *)

(** Reconcile GitHub action receipts with resulting webhooks without loops
    (P19.M4.E2.T004).

    Every mutation, background action, workflow, code job, and merge records a
    secret-free correlation (Room, item, action, actor mode/evidence, plan /
    receipt / delivery / GitHub refs). Verified webhooks that match an open
    correlation close that receipt exactly once, update the item projection, and
    never re-enqueue delivery work or emit duplicate visible output. Unrelated
    human events remain distinct ([Ignored_human_event]).

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type correlation = {
  room_id : string;
  item_key : string option;
  action : string;
  plan_id : string option;
  receipt_id : string option;
  delivery_id : string option;
  github_ref : string option;
  actor_mode : string;  (** app | pat | user | pilot *)
}

type reconcile_result =
  | Closed of { correlation : correlation; first_time : bool }
  | No_matching_receipt
  | Already_closed
  | Ignored_human_event

val ensure_schema : Sqlite3.db -> unit
(** Table [github_action_correlations]. Idempotent. Secret-free columns only. *)

val record_correlation :
  db:Sqlite3.db ->
  correlation:correlation ->
  ?now:float ->
  unit ->
  (unit, string) result
(** Persist an open correlation for a confirmed GitHub action. Fields are
    redacted before storage (no bearer tokens, PATs, or secret-shaped
    key=value). *)

val reconcile_webhook :
  db:Sqlite3.db ->
  room_id:string ->
  envelope:Github_event_envelope.t ->
  ?now:float ->
  unit ->
  reconcile_result
(** Match [delivery_id] / [item_key] / action fingerprints to open correlations;
    close exactly once; update the projection via the room event journal; do not
    enqueue new outbox work for self-events.

    Human events without a correlation → [Ignored_human_event]. Bot/app events
    without a match → [No_matching_receipt]. A second matching webhook after
    close → [Already_closed]. *)

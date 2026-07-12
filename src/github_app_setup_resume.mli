(** Resume GitHub App setup after a verified callback exchange (P19.M2.E3.T001).

    Builds a confirmable [Setup_plan] (kind [Github_app_setup]) for the
    originating Room or a resumable notification target. Callback exchange does
    not apply; this module only plans and stores. Stale plans (expired or base
    revision mismatch) must regenerate rather than apply.

    Presents App identity, live installation scope, permissions, webhook and
    Connector readiness, managed-access diff, and warnings. Apply payloads and
    channel-facing surfaces are secret-free (credential handles / public
    metadata only — never PEM, client secret, or webhook secret plaintext).

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type readiness = {
  app_identity_ok : bool;
  scope_ok : bool;
  permissions_ok : bool;
  webhook_ready : bool;
  connector_ready : bool;
  warnings : string list;
  items : Setup_plan.readiness_item list;
}

type resume_target =
  | Active_room of string
  | Notification of {
      room_id : string option;
      session_key : string option;
      message : string;
    }

type resume_result = {
  transaction : Github_app_setup_tx.t;  (** Consumed. *)
  app : Github_app_setup_callback.app_credentials;
  target : resume_target;
  plan : Setup_plan.t;  (** Confirmable plan; NOT applied. *)
  readiness : readiness;
  live_scope_summary : string;
  managed_access_diff : Setup_plan.diff_op list;
}

val resume_after_exchange :
  db:Sqlite3.db ->
  exchange:Github_app_setup_callback.exchange_result ->
  ?installation:Github_app_installation_scope.t option ->
  ?webhook_reachable:bool ->
  ?connector_ready:bool ->
  ?room_active:bool ->
  ?current_base_revision:string ->
  ?now:float ->
  unit ->
  (resume_result, string) result
(** After successful exchange:

    - If [tx.bind] is [Room r] and [room_active] (default true) →
      [Active_room r]; otherwise a [Notification] for that room.
    - If [tx.bind] is [Session s] → [Notification] with [session_key].
    - Builds a [Setup_plan] with kind [Github_app_setup], principal from the
      transaction, and destination room/session context.
    - Readiness from args + optional installation status.
    - Plan is pure: no apply; [apply_payload] is secret-free (handles only).
    - Stores the plan via [Setup_plan_apply.store_plan]. *)

val regenerate_if_stale :
  db:Sqlite3.db ->
  plan:Setup_plan.t ->
  current_base_revision:string ->
  ?now:float ->
  unit ->
  ([ `Current of Setup_plan.t | `Regenerated of Setup_plan.t ], string) result
(** Stale plans (expired or [base_revision] mismatch) must regenerate, not
    apply. Regeneration produces a new plan id/digest bound to
    [current_base_revision] and stores it. *)

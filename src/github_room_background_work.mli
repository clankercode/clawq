(** Room-anchored Claude-tag-equivalent background work (P19.M4.E2.T002).

    Room requests reuse existing GitHub-originated [Github_work_item] runner /
    work-item semantics for isolation, runner selection, acknowledgement,
    cancellation, retry, progress, blocked, and completion. Progress and results
    are intended for the anchored thread ([thread_ref]); duplicate [dedup_key]
    values do not create a second work item.

    Code-changing work and constrained PR creation remain on
    [Github_code_change_action] (T007). Background permission is a separate
    route capability and does not grant code_change.

    Capability: route [capability_policy.extra] key ["background_work"] (bool),
    defaults off when absent.

    The P19 pilot-only App-attributed action gate is explicit and off by default
    outside the named pilot; production enablement waits for P21 [User_required]
    attribution (no silent App/PAT production fallback).

    Planning produces confirmable secret-free [Setup_plan] values only — no live
    runner dispatch or GitHub mutation at plan time. Enqueue creates / returns
    the durable [Github_work_item] envelope (idempotent on [dedup_key]).

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type request = {
  room_id : string;
  item_key : string option;
  prompt : string;
  runner_pref : string option;
  thread_ref : string option;  (** anchored thread for progress / results *)
  dedup_key : string;
}

type pilot_gate = {
  enabled : bool;
  pilot_name : string;
  expires_at : string option;
      (** ISO-8601 UTC; [None] = no expiry while enabled *)
}

val capability_key : string
(** Extra capability policy key: ["background_work"]. *)

val default_pilot_gate : pilot_gate
(** Off-by-default pilot gate ([enabled = false], name
    ["p19-room-background-work-pilot"]). *)

val has_background_work_capability :
  Github_route_store.capability_policy -> bool
(** True when [extra] contains [(capability_key, true)]. Absent or false →
    denied. *)

val authorize :
  route:Github_route_store.t option ->
  pilot:pilot_gate ->
  ?now:float ->
  unit ->
  (unit, string) result
(** Separate background permission: pilot enabled and unexpired, plus route
    extra ["background_work"]. Does not imply code_change (T007). *)

val plan_background :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  req:request ->
  base_revision:string ->
  ?route:Github_route_store.t ->
  ?pilot:pilot_gate ->
  ?actor_key:Principal_identity.connector_actor_key ->
  ?actor_snapshot:Actor_snapshot.t ->
  ?account_binding_id:string ->
  ?session_id:string ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Confirmable plan for room-anchored background work. Apply kind
    [Generic "github_room_background_work"]. Payload names room, dedup_key,
    optional item_key / thread_ref / runner_pref, pilot, and webhook
    correlation. Secret-free. No live mutation. Defaults [pilot] to
    [default_pilot_gate] (deny).

    Optional [actor_key] / [actor_snapshot] pins initiating attribution via
    [Github_action_actor_attribution] (immutable evidence; re-resolve at
    apply/exec). *)

val enqueue_work_item :
  db:Sqlite3.db ->
  req:request ->
  ?actor_snapshot:Actor_snapshot.t ->
  ?now:float ->
  unit ->
  (Github_work_item.t, string) result
(** Idempotent on [dedup_key] via [Github_work_item.create_if_new]. Returns the
    existing item on duplicate (no second work item). Parses [item_key] when
    present ([issue:repo:n] / [pr:repo:n] / [item:repo:issue:n] forms);
    room-scoped synthetic identity when absent.

    Optional [actor_snapshot] is stored token-free on the work item and
    preserved across retry / cancel / restart. Conflicting lineage on duplicate
    is rejected. *)

val cancel_work_item :
  db:Sqlite3.db -> id:int -> unit -> (Github_work_item.t, string) result
(** Cancel via [Github_work_item] lifecycle ([Cancelled] terminal). *)

val request_retry :
  db:Sqlite3.db -> id:int -> unit -> (Github_work_item.t, string) result
(** Re-queue a non-running terminal/blocked item for retry (attempt_count bump
    via attach semantics when already task-linked; status → [Queued]). *)

val mark_progress :
  db:Sqlite3.db -> id:int -> unit -> (Github_work_item.t, string) result
(** Mark [Running] (progress in-flight toward the anchored thread). *)

val mark_blocked :
  db:Sqlite3.db ->
  id:int ->
  summary:string ->
  unit ->
  (Github_work_item.t, string) result
(** Mark [Blocked] with a secret-free summary. *)

val mark_completed :
  db:Sqlite3.db ->
  id:int ->
  summary:string ->
  unit ->
  (Github_work_item.t, string) result
(** Mark [Succeeded] with a secret-free result summary for thread delivery. *)

val is_background_plan : Setup_plan.t -> bool
(** True when apply kind is [github_room_background_work]. *)

val receipt_safe_error : string -> string
(** Projection-safe error receipt text: redacts bearer tokens, GitHub PATs, and
    token/secret key=value shapes. *)

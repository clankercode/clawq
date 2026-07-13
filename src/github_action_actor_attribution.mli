(** Carry immutable [Actor_snapshot] evidence through P19 GitHub action intents,
    previews, confirmations, and dispatch envelopes (P21.M1.E3.T002).

    Thin adapter over [Actor_snapshot] + [Setup_plan]:

    - Capture initiating Actor evidence (Principal / link / account lineage) at
      preview / intent time — never from Room history or another participant.
    - Embed the snapshot into the pending plan ([apply_payload.data] +
      [planned_state]) so confirmation and dispatch envelopes retain it.
    - At confirm/apply, re-resolve live authority; actor, link, account, target,
      or policy changes invalidate the confirmation. Snapshots are never
      reusable authority.

    Durable jobs/outbox are T005. Receipts and webhook reconcile attach the same
    snapshot via [Github_action_reconcile] (T006) without elevating it to
    authority.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0005-separate-human-principals-from-room-sessions.md. *)

module A = Actor_snapshot
module P = Principal_identity

val field_actor_snapshot : string
(** JSON field names used in plan [apply_payload.data] / [planned_state]. *)

val field_target_fingerprint : string

(** {1 Target / policy fingerprint (confirmation invalidation)} *)

type target_fingerprint = {
  item_key : string option;
  base_revision : string;
  route_id : string option;
  route_revision : string option;
  capability : string option;
  action_kind : string option;
  head_sha : string option;
  policy_digest : string option;
      (** Opaque policy pin (e.g. capability flags or merge-policy digest).
          Change invalidates confirmation when [current] supplies a different
          value. *)
}
(** Target and policy surface pinned at intent time. Compared at apply when a
    fresh [current] fingerprint is supplied. [base_revision] is also enforced by
    [Setup_plan_apply]. *)

val empty_target_fingerprint : base_revision:string -> target_fingerprint

val target_fingerprint_of_plan : Setup_plan.t -> target_fingerprint
(** Project common fields from plan [apply_payload.data] / [planned_state] /
    [base_revision]. *)

val target_fingerprint_to_json : target_fingerprint -> Yojson.Safe.t

val target_fingerprint_of_json :
  Yojson.Safe.t -> (target_fingerprint, string) result

val target_fingerprints_compatible :
  planned:target_fingerprint ->
  current:target_fingerprint ->
  (unit, string) result
(** [Ok ()] when [current] does not change any pinned field that was set on
    [planned]. Unset fields on [planned] are not enforced. Empty [current]
    fields for a pinned planned value fail. *)

(** {1 Capture initiating identity (never Room history)} *)

val capture_for_intent :
  db:Sqlite3.db ->
  actor_key:P.connector_actor_key ->
  ?account_binding_id:string ->
  ?room_id:string ->
  ?session_id:string ->
  ?message_id:string ->
  ?intent_id:string ->
  ?confirmation_id:string ->
  ?now:float ->
  unit ->
  (A.t, string) result
(** Capture from live store via [Actor_snapshot.create_from_live]. Room /
    Session / message are source context only — never identity. Fails closed
    when the actor is missing or disabled. *)

val reject_identity_from_room_history : room_id:string -> string
(** Stable error: Room history cannot supply initiating identity. *)

val reject_identity_from_other_participant :
  initiating:P.connector_actor_key -> claimed:P.connector_actor_key -> string
(** Stable error when a different actor is offered as the initiator. *)

val assert_not_borrowed_identity :
  initiating:P.connector_actor_key ->
  claimed:P.connector_actor_key ->
  (unit, string) result
(** [Ok ()] only when [claimed] equals [initiating]. *)

(** {1 Embed / extract on Setup_plan} *)

val attach_to_plan :
  plan:Setup_plan.t ->
  snapshot:A.t ->
  ?target:target_fingerprint ->
  unit ->
  Setup_plan.t
(** Embed redacted-by-construction snapshot JSON and target fingerprint into
    [apply_payload.data] and [planned_state], add a readiness note, recompute
    digest via [Setup_plan.redact]. Does not persist. *)

val snapshot_of_plan : Setup_plan.t -> (A.t option, string) result
(** Read snapshot from [apply_payload.data] (preferred) or [planned_state].
    [Ok None] when absent; [Error] on malformed present payload. *)

val target_fingerprint_stored :
  Setup_plan.t -> (target_fingerprint option, string) result
(** Explicit stored fingerprint when present; else [Ok None]. *)

val has_actor_snapshot : Setup_plan.t -> bool

(** {1 Re-resolve at confirm / apply} *)

type invalidation =
  | Snapshot_missing
  | Snapshot_malformed of string
  | Authority_unusable of { breaks : A.authority_break list }
  | Target_changed of string
  | Policy_changed of string
  | Borrowed_identity of string
  | Room_history_identity of string

val string_of_invalidation : invalidation -> string

type dispatch_envelope = {
  plan_id : string;
  digest : string;
  snapshot : A.t;
      (** Immutable initiating evidence; never reusable authority. *)
  live_authority : A.current_authority;
      (** Re-resolved at prepare time; [usable] must be true. *)
  target : target_fingerprint;
  principal_lineage_id : string;
      (** Logical Principal id from the snapshot (pre-merge id preserved). *)
  account_lineage_id : string option;
}
(** Secret-free dispatch envelope for execution. Retains initiating snapshot and
    lineage; live credentials must still be leased separately. *)

val prepare_dispatch :
  db:Sqlite3.db ->
  plan:Setup_plan.t ->
  ?current_target:target_fingerprint ->
  unit ->
  (dispatch_envelope, invalidation) result
(** Extract snapshot + target, re-resolve live authority, compare optional
    current target/policy fingerprint. Fails closed when the snapshot is
    missing, authority is unusable, or target/policy changed. *)

val revalidate_for_apply :
  db:Sqlite3.db ->
  plan:Setup_plan.t ->
  ?current_target:target_fingerprint ->
  ?require_snapshot:bool ->
  unit ->
  (dispatch_envelope option, string) result
(** Apply-path helper. When the plan has no snapshot and [require_snapshot] is
    false (default), returns [Ok None] (legacy / App-only plans). When a
    snapshot is present (or required), runs [prepare_dispatch] and maps
    invalidations to actionable error strings. *)

val attach_and_restamp :
  db:Sqlite3.db ->
  plan:Setup_plan.t ->
  snapshot:A.t ->
  ?target:target_fingerprint ->
  unit ->
  (Setup_plan.t, string) result
(** [attach_to_plan] then replace the pending stored plan (digest-aware). *)

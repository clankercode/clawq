(** Propagate immutable [Actor_snapshot] evidence through durable jobs, retries,
    outbox entries, cancellation, and restart (P21.M1.E3.T005).

    Thin adapter over [Actor_snapshot] for delayed work surfaces:

    - Capture initiating Actor evidence at enqueue / plan time (never from Room
      history or another participant).
    - Persist token-free snapshot JSON on outbox rows and work items; retries,
      cancellation, and restart re-read the same immutable evidence.
    - At execution, re-resolve live Principal / identity link / account lineage;
      stale, split, revoked, or mismatched lineage fails closed.
    - Snapshots are never reusable authority and never borrow another
      participant's identity.

    Action intents/confirmations use [Github_action_actor_attribution] (T002).
    Receipts / webhook reconciliation are a separate slice (T006).

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0005-separate-human-principals-from-room-sessions.md. *)

module A = Actor_snapshot
module P = Principal_identity

val field_actor_snapshot : string
(** JSON field name used when embedding snapshots into durable envelopes. *)

(** {1 Capture initiating identity for delayed work} *)

val capture_for_delayed_job :
  db:Sqlite3.db ->
  actor_key:P.connector_actor_key ->
  delayed_job_id:string ->
  ?account_binding_id:string ->
  ?room_id:string ->
  ?session_id:string ->
  ?message_id:string ->
  ?intent_id:string ->
  ?confirmation_id:string ->
  ?now:float ->
  unit ->
  (A.t, string) result
(** Capture from live store via [Actor_snapshot.create_from_live] with
    [reason="delayed_job"] and [work_refs.delayed_job_id] set. Room / Session /
    message are source context only. Fails closed when the actor is missing or
    disabled. *)

val reject_identity_from_room_history : room_id:string -> string

val reject_identity_from_other_participant :
  initiating:P.connector_actor_key -> claimed:P.connector_actor_key -> string

val assert_not_borrowed_identity :
  initiating:P.connector_actor_key ->
  claimed:P.connector_actor_key ->
  (unit, string) result
(** [Ok ()] only when [claimed] equals [initiating]. *)

(** {1 Token-free storage JSON} *)

val snapshot_to_storage_json : A.t -> (Yojson.Safe.t, string) result
(** Serialize for durable storage. Rejects payloads that would carry token-like
    material (defense in depth; [A.to_json] is already token-free). *)

val snapshot_of_storage_json : Yojson.Safe.t -> (A.t, string) result
(** Parse previously stored snapshot JSON. Rejects token material. *)

val lineage_summary_json : A.t -> Yojson.Safe.t
(** Secret-free Principal / actor / account lineage pin for envelopes. *)

(** {1 Re-resolve at execution} *)

type exec_invalidation =
  | Snapshot_missing
  | Snapshot_malformed of string
  | Authority_unusable of { breaks : A.authority_break list }
  | Borrowed_identity of string
  | Job_cancelled of string
  | Lineage_mismatch of string

val string_of_exec_invalidation : exec_invalidation -> string

type exec_envelope = {
  job_id : string;
  snapshot : A.t;
      (** Immutable initiating evidence; never reusable authority. *)
  live_authority : A.current_authority;
      (** Re-resolved at prepare time; [usable] must be true. *)
  principal_lineage_id : string;
      (** Logical Principal id from the snapshot (pre-merge id preserved). *)
  account_lineage_id : string option;
}
(** Secret-free execution envelope. Live credentials must still be leased
    separately from vault state. *)

val prepare_execution :
  db:Sqlite3.db ->
  job_id:string ->
  snapshot:A.t ->
  ?claimed_actor:P.connector_actor_key ->
  ?cancelled:bool ->
  unit ->
  (exec_envelope, exec_invalidation) result
(** Re-resolve live authority from the immutable snapshot. Fails closed when
    authority is unusable, [cancelled] is true, or [claimed_actor] borrows
    another participant. *)

val prepare_execution_of_json :
  db:Sqlite3.db ->
  job_id:string ->
  snapshot_json:Yojson.Safe.t option ->
  ?require_snapshot:bool ->
  ?claimed_actor:P.connector_actor_key ->
  ?cancelled:bool ->
  unit ->
  (exec_envelope option, string) result
(** When [snapshot_json] is [None] and [require_snapshot] is false (default),
    returns [Ok None] (legacy unattributed jobs). Otherwise runs
    [prepare_execution] and maps invalidations to actionable strings. *)

(** {1 Snapshot identity comparison (write-once / first-wins)} *)

val snapshots_same_initiating_lineage : A.t -> A.t -> bool
(** True when both pin the same actor identity key, Principal id, and account
    lineage id (when set). Used to reject borrow-on-retry without requiring
    byte-identical capture timestamps. *)

val reject_conflicting_snapshot :
  existing:A.t -> offered:A.t -> (unit, string) result
(** [Ok ()] when lineages match; [Error] when an offered snapshot would borrow
    or replace initiating lineage. *)

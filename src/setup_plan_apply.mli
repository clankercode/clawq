(** Confirm/apply engine for [Setup_plan] (P19.M1.E1.T002).

    Apply requires matching plan ID + digest from the authorized principal,
    rechecks expiry, base revision, destination, and authority, and is atomic
    and retry-idempotent. Stale, denied, failed, and successful attempts emit
    redacted audit records.

    Adapter mutation is supplied by the caller via [apply_ops]; this module owns
    rechecks, store CAS, receipts, and audit — not domain apply logic. *)

type reject_reason =
  | Plan_not_found
  | Digest_mismatch
  | Principal_mismatch
  | Expired
  | Stale_revision
  | Destination_mismatch
  | Authority_denied
  | Readiness_failed
  | Concurrent_conflict
  | Apply_error

type outcome =
  | Applied of {
      receipt_id : string;
      first_time : bool;
          (** [false] when a retry replayed an already-applied plan. *)
    }
  | Rejected of { reason : reject_reason; message : string }

type audit_record = {
  id : int;
  timestamp : string;
  plan_id : string;
  digest : string;
  principal_id : string;
  outcome : string;
      (** "applied" | "applied_idempotent" | "rejected" | "failed" *)
  reason : string option;
  details : string;  (** Redacted JSON summary; never contains raw secrets. *)
}

type authority_check =
  principal:Setup_plan.principal ->
  destination:Setup_plan.context ->
  (unit, string) result
(** Return [Ok ()] if the principal may apply to the destination. *)

type apply_ops = plan:Setup_plan.t -> receipt_id:string -> (unit, string) result
(** Domain mutation after rechecks. Invoked only on the first successful CAS
    path (not on idempotent retries). Implementations must tolerate retry with
    the same [receipt_id] if the process crashes between domain success and
    durable commit. *)

val init_schema : Sqlite3.db -> unit

val store_plan : db:Sqlite3.db -> Setup_plan.t -> (unit, string) result
(** Persist a redacted plan as [pending]. Fails if plan id already exists. *)

val replace_pending_plan :
  db:Sqlite3.db -> Setup_plan.t -> (unit, string) result
(** Replace [plan_json] + [digest] for an existing [pending] plan (same id).
    Used to restamp actor snapshots onto a just-created intent without changing
    identity of the plan row. Fails if missing or not pending. *)

val get_plan : db:Sqlite3.db -> plan_id:string -> Setup_plan.t option

val apply :
  db:Sqlite3.db ->
  plan_id:string ->
  digest:string ->
  principal:Setup_plan.principal ->
  current_base_revision:string ->
  destination_room:string ->
  ?destination_session:string ->
  ?now:float ->
  authority:authority_check ->
  apply_ops:apply_ops ->
  unit ->
  outcome
(** Recheck + atomic apply.

    Order: identity (id/digest/principal) → already-applied short-circuit
    (retry-idempotent; ignores advanced revision/expiry) → live rechecks
    (expiry, base revision, Room-or-Session destination, or an empty global
    destination for [Room_profile] only, readiness, authority) → BEGIN IMMEDIATE
    \+ CAS pending→applied + in-tx success audit.

    Concurrent writers are rejected with [Stale_revision] /
    [Concurrent_conflict] rather than overwriting a committed apply. *)

val list_audit :
  db:Sqlite3.db -> ?plan_id:string -> ?limit:int -> unit -> audit_record list

val string_of_reject_reason : reject_reason -> string
val string_of_outcome : outcome -> string

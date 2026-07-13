(** Execute private two-sided cross-Connector link proof transactions
    (P21.M1.E1.T010).

    Persists versioned {!Principal_link_protocol.link_transaction} rows, runs
    one-time expiring two-sided proof with CAS-safe updates, and records
    completion as a minimal link-edge (no Principal merge/adoption — T011).

    Failures (replay against non-completed state, expiry, actor/revision change,
    ambiguity, cancellation, concurrent CAS) leave ownership unchanged and
    always emit exactly one redacted audit event.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module L = Principal_link_protocol
module P = Principal_identity

type audit_sink = L.redacted_audit_event -> unit
(** Optional sink invoked once per mutating operation with a redacted event. *)

type stored_tx = {
  tx : L.link_transaction;
  tx_revision : int;
      (** Monotonic CAS revision for concurrent present/cancel/expire. *)
  initiator_principal_id : P.principal_id option;
      (** Initiating Principal bound at create (usually the initiator endpoint's
          principal). *)
  pair_key : string;
      (** Ordered actor-identity pair key for concurrent-open detection. *)
  updated_at : string;
}
(** Stored row metadata beyond the protocol transaction. *)

type link_edge = {
  id : string;
  link_tx_id : string;
  actor_a_key : string;
  actor_b_key : string;
  principal_a_id : string option;
  principal_b_id : string option;
  completed_at : string;
}
(** Minimal cross-Connector link edge written on [Completed] only. Does not
    merge Principals or reassign identity ownership. *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent tables: [principal_link_tx], [principal_link_edges]. *)

val generate_id : ?now:float -> unit -> string

val generate_opaque_token : unit -> string
(** Cryptographic random hex for replay / challenge ids. *)

val pair_key_of_endpoints : L.verified_endpoint -> L.verified_endpoint -> string
(** Canonical unordered pair key of the two actor identity keys. *)

(** {1 Create open link} *)

val create_open_link :
  db:Sqlite3.db ->
  endpoint_a:L.verified_endpoint ->
  endpoint_b:L.verified_endpoint ->
  ?initiator:[ `A | `B ] ->
  ?initiator_principal_id:P.principal_id ->
  ?id:string ->
  ?replay_protection_id:string ->
  ?proof_challenge_id:string ->
  ?ttl_seconds:float ->
  ?now:float ->
  ?audit_sink:audit_sink ->
  unit ->
  (stored_tx * L.redacted_audit_event, string) result
(** Create and persist an [Open] two-sided private proof transaction.

    Requires two distinct verified endpoints. Binds the initiating Principal
    (explicit only when it equals the initiator endpoint, otherwise derived from
    that endpoint). The initiator endpoint must therefore have a bound
    Principal. Concurrent open/awaiting transactions for the same actor pair
    fail closed without writing ownership changes. Emits [Link_tx_created] (or a
    rejection audit on concurrent fail). *)

(** {1 Proof presentation} *)

type present_status =
  | Endpoint_proved  (** One side proved; still awaiting counterpart. *)
  | Link_completed  (** Both sides proved; edge recorded. *)
  | Idempotent_replay
      (** Matching replay id against already-[Completed]; no re-apply. *)
  | Rejected of string  (** Fail closed; ownership unchanged. *)

type present_result = {
  status : present_status;
  stored : stored_tx option;
  edge : link_edge option;
  audit : L.redacted_audit_event;
  ownership_changed : bool;
      (** Always [false] in T010 (merge/adoption is T011). *)
}

val present_proof :
  db:Sqlite3.db ->
  id:string ->
  side:[ `A | `B ] ->
  presented_replay_id:string ->
  presented_challenge_id:string ->
  ?presented_actor_key:P.connector_actor_key ->
  ?presented_actor_revision:int ->
  ?presented_principal_id:P.principal_id ->
  ?presented_principal_revision:int ->
  ?expected_tx_revision:int ->
  ?now:float ->
  ?audit_sink:audit_sink ->
  unit ->
  present_result
(** Present one-time endpoint proof.

    Validates open status, expiry, replay id, challenge id, side identity, bound
    actor/principal revisions (actor change / ambiguity fail closed), and CAS
    [expected_tx_revision] when provided. Fresh proofs require the
    adapter-verified actor key and revision; a bound endpoint Principal and
    revision must also be presented. Concurrent stale CAS fails without mutating
    the row. On both sides proved, inserts a [link_edge] and sets status
    [Completed]. Always emits exactly one redacted audit event. *)

(** {1 Cancel / expire} *)

val cancel_link :
  db:Sqlite3.db ->
  id:string ->
  ?reason:string ->
  ?expected_tx_revision:int ->
  ?now:float ->
  ?audit_sink:audit_sink ->
  unit ->
  (stored_tx * L.redacted_audit_event, string) result
(** Cancel a non-terminal transaction. CAS fail closed when revision mismatches.
*)

val expire_link :
  db:Sqlite3.db ->
  id:string ->
  ?expected_tx_revision:int ->
  ?now:float ->
  ?audit_sink:audit_sink ->
  unit ->
  (stored_tx * L.redacted_audit_event, string) result
(** Expire when past [expires_at]. No ownership change. *)

(** {1 Reads} *)

val get : db:Sqlite3.db -> id:string -> (stored_tx option, string) result
val get_edge : db:Sqlite3.db -> id:string -> (link_edge option, string) result

val get_edge_by_tx :
  db:Sqlite3.db -> link_tx_id:string -> (link_edge option, string) result

val find_open_for_pair :
  db:Sqlite3.db ->
  endpoint_a:L.verified_endpoint ->
  endpoint_b:L.verified_endpoint ->
  (stored_tx option, string) result
(** Latest non-terminal transaction for the actor pair, if any. *)

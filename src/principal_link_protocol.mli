(** Private cross-Connector linking and admin repair protocol (P21.M1.E1.T004).

    Versioned types and pure validation for:
    - Two-sided private proof link transactions between verified Connector actor
      endpoints
    - Private proof delivery descriptors (abstract channel/handle; no secrets)
    - Expiry, replay-protection ids, and cancellation
    - Revision binding for CAS-safe apply
    - Redacted audit event shapes
    - Admin repair plan-confirm-apply status machine

    Display-name, email, and external-account auto-link bases are forbidden.
    Proof execution, Principal adoption/merge, and unlink/split lifecycle belong
    to later tasks (P21.M1.E1.T010–T012). This module defines the contract only.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0005-separate-human-principals-from-room-sessions.md. *)

val protocol_version : int
(** Protocol schema version; starts at 1. *)

val default_link_ttl_seconds : float
(** Default private-proof transaction TTL: 15 minutes. *)

val default_repair_ttl_seconds : float
(** Default admin-repair plan TTL: 30 minutes. *)

(** {1 Link basis (what may establish a cross-Connector link)} *)

(** How a cross-Connector association is proposed. Only
    [Two_sided_private_proof] and [Admin_repair] are allowed. *)
type link_basis =
  | Two_sided_private_proof
      (** Ordinary path: both verified actors complete private proof. *)
  | Admin_repair
      (** Audited plan-confirm-apply repair by an authorized admin. *)
  | Auto_display_name  (** Forbidden. *)
  | Auto_email  (** Forbidden. *)
  | Auto_external_account  (** Forbidden (e.g. matching GitHub accounts). *)

val string_of_link_basis : link_basis -> string
val link_basis_of_string : string -> (link_basis, string) result

val link_basis_is_allowed : link_basis -> bool
(** [true] only for [Two_sided_private_proof] and [Admin_repair]. *)

val assert_link_basis_allowed : link_basis -> (unit, string) result
(** Reject auto-link bases with an actionable error. *)

(** {1 Verified Connector actor endpoint} *)

type verified_endpoint = {
  actor_key : Principal_identity.connector_actor_key;
      (** Immutable verified Connector identity. *)
  principal_id : Principal_identity.principal_id option;
      (** Current Principal when known; [None] if first-seen has not bound yet
          (execution may still resolve before proof completes). *)
  principal_revision : int option;
      (** Bound Principal revision for CAS when [principal_id] is [Some]. *)
  actor_revision : int;
      (** Bound Connector-actor revision at transaction/plan creation. *)
  verified_at : string;
      (** ISO-8601 UTC when trusted ingress last established this actor. Must be
          non-empty — unverified actors cannot be endpoints. *)
}
(** One side of a link: a verified Connector actor, optionally already owned by
    a Principal, with revision bindings for later CAS apply. *)

val make_verified_endpoint :
  actor_key:Principal_identity.connector_actor_key ->
  ?principal_id:Principal_identity.principal_id ->
  ?principal_revision:int ->
  ?actor_revision:int ->
  verified_at:string ->
  unit ->
  (verified_endpoint, string) result
(** Require non-empty [verified_at]. When [principal_id] is set,
    [principal_revision] must be a positive int (default 1). [actor_revision]
    defaults to 1 and must be positive. *)

val endpoints_distinct : verified_endpoint -> verified_endpoint -> bool
(** [false] when both sides share the same actor identity key. *)

val require_two_verified_endpoints :
  verified_endpoint -> verified_endpoint -> (unit, string) result
(** Reject identical actors or empty verification timestamps. *)

(** {1 Private proof delivery (secret-free descriptors)} *)

(** Abstract private channel. Exports carry only connector kind + handle id. *)
type private_delivery_channel =
  | Connector_dm of {
      connector : Principal_identity.connector;
      handle_id : string;
          (** Opaque delivery handle/alias — never a token, code, or URL secret.
          *)
    }
  | Web_private of { handle_id : string }
  | Cli_private of { handle_id : string }
  | Unsupported of { reason : string }
      (** Private delivery unavailable; link must refuse safely rather than fall
          back to Room-visible channels. *)

type private_proof_delivery = {
  channel : private_delivery_channel;
  delivery_id : string;  (** Opaque correlation id for this delivery attempt. *)
  endpoint_side : [ `A | `B ];
  created_at : string;
}
(** Descriptor that a private proof continuation was (or will be) delivered.
    Never contains proof secrets, device codes, OAuth state, or raw URLs with
    embedded tokens. *)

val make_private_proof_delivery :
  channel:private_delivery_channel ->
  delivery_id:string ->
  endpoint_side:[ `A | `B ] ->
  ?created_at:string ->
  unit ->
  (private_proof_delivery, string) result
(** Reject empty [delivery_id] or empty handle ids on concrete channels. *)

val delivery_is_export_safe : private_proof_delivery -> bool
(** [true] when channel carries only abstract handles (always for well-formed
    values from [make_private_proof_delivery]). *)

(** {1 Link transaction status} *)

(** Lifecycle of a private two-sided link proof transaction. *)
type link_tx_status =
  | Open
      (** Created; neither endpoint has completed proof (or both still pending).
      *)
  | Awaiting_counterpart
      (** Exactly one endpoint has proved; waiting for the other. *)
  | Completed  (** Both endpoints proved; ready for adoption (T011). *)
  | Expired
  | Cancelled
  | Superseded
      (** Replaced by a newer transaction or an admin repair on the same pair.
      *)

val string_of_link_tx_status : link_tx_status -> string
val link_tx_status_of_string : string -> (link_tx_status, string) result

val link_tx_status_is_terminal : link_tx_status -> bool
(** [Completed], [Expired], [Cancelled], [Superseded]. *)

val link_tx_status_accepts_proof : link_tx_status -> bool
(** [Open] or [Awaiting_counterpart] only. *)

(** {1 Versioned private link transaction} *)

type link_transaction = {
  version : int;
  id : string;  (** Opaque transaction id. *)
  basis : link_basis;
      (** Must be [Two_sided_private_proof] for ordinary construction. *)
  endpoint_a : verified_endpoint;
  endpoint_b : verified_endpoint;
  initiator : [ `A | `B ];
  status : link_tx_status;
  replay_protection_id : string;
      (** One-time correlation id for replay detection. Completing with the same
          id after [Completed] is an idempotent replay, not a new merge. *)
  proof_challenge_id : string;
      (** Opaque id of the proof secret held outside this record. The secret
          itself is never stored on the transaction or in audit exports. *)
  a_proved : bool;
  b_proved : bool;
  delivery_a : private_proof_delivery option;
  delivery_b : private_proof_delivery option;
  created_at : string;
  expires_at : string;
  completed_at : string option;
  cancelled_at : string option;
  cancel_reason : string option;
}
(** Versioned two-endpoint private proof transaction. Revision binding lives on
    each [verified_endpoint]. Execution (atomic prove/complete) is T010. *)

val make_link_transaction :
  id:string ->
  endpoint_a:verified_endpoint ->
  endpoint_b:verified_endpoint ->
  ?initiator:[ `A | `B ] ->
  replay_protection_id:string ->
  proof_challenge_id:string ->
  ?ttl_seconds:float ->
  ?now:float ->
  ?created_at:string ->
  ?expires_at:string ->
  unit ->
  (link_transaction, string) result
(** Construct an [Open] [Two_sided_private_proof] transaction.

    Validates: allowed basis, two distinct verified endpoints, non-empty ids,
    positive TTL / [expires_at > created_at]. Does not persist or deliver proof.
*)

val validate_link_transaction : link_transaction -> (unit, string) result
(** Structural validation of an existing record (version, endpoints, basis,
    status/proof consistency, timestamps). *)

val link_transaction_is_expired : ?now:float -> link_transaction -> bool
(** Lexicographic ISO-8601 compare of [now] against [expires_at]. Terminal
    statuses other than open/awaiting are not "live expired" for apply, but this
    still reports clock expiry. *)

val assert_link_open_for_proof :
  ?now:float -> link_transaction -> (unit, string) result
(** Fail if status does not accept proof, or if expired/cancelled. *)

val assert_not_cancelled : link_transaction -> (unit, string) result

type replay_check =
  | Fresh  (** [replay_protection_id] has not completed this transaction. *)
  | Idempotent_completed
      (** Transaction already [Completed]; caller should return the prior result
          without re-merging (T010/T011). *)
  | Rejected of string
      (** Replay against a non-completed or mismatched state. *)

val check_replay :
  link_transaction -> presented_replay_id:string -> replay_check
(** Compare [presented_replay_id] to [tx.replay_protection_id] and status.

    - Matching id + [Completed] → [Idempotent_completed]
    - Matching id + open/awaiting → [Fresh] (continue proof)
    - Matching id + expired/cancelled/superseded → [Rejected]
    - Mismatched id → [Rejected] *)

val mark_endpoint_proved_pure :
  link_transaction ->
  side:[ `A | `B ] ->
  ?now:float ->
  unit ->
  (link_transaction, string) result
(** Pure status transition for one endpoint proof acknowledgment.

    Does not verify cryptographic proof material (T010). Updates [a_proved] /
    [b_proved] and advances [Open] → [Awaiting_counterpart] → [Completed].
    Rejects expired, cancelled, terminal, or double-prove of the same side. *)

val cancel_link_transaction_pure :
  link_transaction ->
  ?reason:string ->
  ?now:float ->
  unit ->
  (link_transaction, string) result
(** Pure cancel. Rejects already-terminal statuses. *)

val expire_link_transaction_pure :
  link_transaction -> ?now:float -> unit -> (link_transaction, string) result
(** Pure expire when past [expires_at]. No-op error if not yet expired. *)

(** {1 Admin repair plan-confirm-apply} *)

(** Status machine for revision-bound admin repair. Natural language and
    external callbacks never confirm. *)
type repair_status =
  | Planned  (** Built; not yet explicitly confirmed. *)
  | Confirmed  (** Authorized principal confirmed digest. *)
  | Applied  (** Atomic CAS apply succeeded (or idempotent replay). *)
  | Rejected  (** Validation, authority, conflict, or stale revision. *)
  | Expired
  | Cancelled
  | Stale_revision  (** Bound principal revisions no longer match live state. *)

val string_of_repair_status : repair_status -> string
val repair_status_of_string : string -> (repair_status, string) result
val repair_status_is_terminal : repair_status -> bool

(** How the survivor Principal is chosen for a merge repair. *)
type survivor_selection =
  | By_creation_order
      (** Survivor = earlier durable Principal creation; tie-break by stable
          Principal id (ordinary merge rule; admin may still plan this). *)
  | Explicit of Principal_identity.principal_id
      (** Admin names the survivor; must be one of the two endpoint Principals.
      *)

type repair_conflict =
  | External_account_collision of { summary : string }
      (** Distinct bindings violating uniqueness; fail closed until a new plan
          keeps one and revokes the other. *)
  | Preference_conflict of { key : string; summary : string }
  | Pending_authorization_invalidated of { count : int }
  | Other of { code : string; summary : string }
      (** Redacted conflict previews — summaries must not include credentials.
      *)

type repair_preview = {
  survivor_principal_id : Principal_identity.principal_id option;
      (** Resolved when both Principals known and selection applies; [None] if
          adoption without merge (only one Principal). *)
  merged_principal_id : Principal_identity.principal_id option;
      (** The Principal that becomes [Merged_into] tombstone, if any. *)
  conflicts : repair_conflict list;
  notes : string list;  (** Human-readable redacted notes for the plan. *)
}
(** Plan preview: survivor, merged id, and enumerated conflicts. *)

type admin_repair_plan = {
  version : int;
  id : string;
  basis : link_basis;  (** Must be [Admin_repair]. *)
  endpoint_a : verified_endpoint;
  endpoint_b : verified_endpoint;
  admin_principal_id : Principal_identity.principal_id;
  survivor : survivor_selection;
  base_principal_a_revision : int option;
      (** Required when endpoint_a has a principal_id. *)
  base_principal_b_revision : int option;
  preview : repair_preview;
  digest : string;
      (** Hash over redacted canonical plan body (excluding digest field). *)
  status : repair_status;
  created_at : string;
  expires_at : string;
  confirmed_at : string option;
  applied_at : string option;
  reject_reason : string option;
}
(** Revision-bound admin repair plan. Apply execution is later; this is the
    protocol skeleton forbidding auto-link bases. *)

val make_admin_repair_plan :
  id:string ->
  endpoint_a:verified_endpoint ->
  endpoint_b:verified_endpoint ->
  admin_principal_id:Principal_identity.principal_id ->
  survivor:survivor_selection ->
  preview:repair_preview ->
  ?ttl_seconds:float ->
  ?now:float ->
  ?created_at:string ->
  ?expires_at:string ->
  unit ->
  (admin_repair_plan, string) result
(** Construct a [Planned] [Admin_repair] plan with computed digest.

    Requires two distinct verified endpoints. When an endpoint has a
    [principal_id], its [principal_revision] is captured as the base revision.
    Explicit survivor must equal one of the two principal ids when both exist.
    Rejects any non-[Admin_repair] basis (constructor fixes basis). *)

val validate_admin_repair_plan : admin_repair_plan -> (unit, string) result
val admin_repair_is_expired : ?now:float -> admin_repair_plan -> bool

val compute_repair_digest : admin_repair_plan -> string
(** Recompute digest from plan fields excluding [digest] itself. *)

val confirm_repair_plan_pure :
  admin_repair_plan ->
  presented_digest:string ->
  confirming_principal:Principal_identity.principal_id ->
  ?now:float ->
  unit ->
  (admin_repair_plan, string) result
(** Pure Planned → Confirmed. Requires matching digest, same admin principal,
    not expired. *)

val mark_repair_applied_pure :
  admin_repair_plan -> ?now:float -> unit -> (admin_repair_plan, string) result
(** Pure Confirmed → Applied. Rejects other statuses (idempotent if already
    Applied). *)

val reject_repair_plan_pure :
  admin_repair_plan ->
  reason:string ->
  unit ->
  (admin_repair_plan, string) result

val cancel_repair_plan_pure :
  admin_repair_plan -> ?now:float -> unit -> (admin_repair_plan, string) result

(** {1 Forbidden auto-link proposals} *)

type auto_link_proposal = {
  basis : link_basis;
  display_name : string option;
  email : string option;
  external_account_hint : string option;
  left_actor : Principal_identity.connector_actor_key option;
  right_actor : Principal_identity.connector_actor_key option;
}
(** Evidence that must never by itself create or merge Principals. *)

val reject_auto_link : auto_link_proposal -> (unit, string) result
(** [Error] for auto bases, same-actor pairs, or display/email/external-only
    evidence. [Ok ()] only when basis is allowed and two distinct actor keys are
    present (caller still constructs a link_transaction or repair plan). *)

(** {1 Redacted audit events} *)

type audit_kind =
  | Link_tx_created
  | Link_proof_delivered
  | Link_endpoint_proved
  | Link_tx_completed
  | Link_tx_expired
  | Link_tx_cancelled
  | Link_tx_replayed
  | Link_tx_superseded
  | Repair_planned
  | Repair_confirmed
  | Repair_applied
  | Repair_rejected
  | Repair_cancelled
  | Repair_expired
  | Repair_stale_revision
  | Auto_link_rejected

val string_of_audit_kind : audit_kind -> string
val audit_kind_of_string : string -> (audit_kind, string) result

type redacted_audit_event = {
  version : int;
  id : string;
  kind : audit_kind;
  subject_id : string;  (** Transaction or repair plan id (never a secret). *)
  endpoint_a_key : string;
      (** [Principal_identity.actor_identity_key]; no display name. *)
  endpoint_b_key : string option;
  principal_ids : string list;  (** Opaque Principal ids only. *)
  status : string;
  reason : string option;
  timestamp : string;
  details : Yojson.Safe.t;
      (** Secret-free structured detail. Must not include proof secrets, tokens,
          emails used as link keys, or raw delivery URLs with secrets. *)
}
(** Audit shape safe for persistence and export. *)

val make_redacted_audit_event :
  id:string ->
  kind:audit_kind ->
  subject_id:string ->
  endpoint_a_key:string ->
  ?endpoint_b_key:string ->
  ?principal_ids:string list ->
  status:string ->
  ?reason:string ->
  ?timestamp:string ->
  ?details:Yojson.Safe.t ->
  ?now:float ->
  unit ->
  (redacted_audit_event, string) result

val audit_from_link_transaction :
  link_transaction ->
  kind:audit_kind ->
  ?id:string ->
  ?reason:string ->
  ?details:Yojson.Safe.t ->
  ?now:float ->
  unit ->
  redacted_audit_event
(** Build a redacted event from a link transaction (no proof secrets). *)

val audit_from_repair_plan :
  admin_repair_plan ->
  kind:audit_kind ->
  ?id:string ->
  ?reason:string ->
  ?details:Yojson.Safe.t ->
  ?now:float ->
  unit ->
  redacted_audit_event

val redact_audit_details : Yojson.Safe.t -> Yojson.Safe.t
(** Drop/replace keys commonly carrying secrets ([proof], [secret], [token],
    [code], [password], [email] as link key, etc.). *)

val audit_event_is_redacted : redacted_audit_event -> bool
(** Heuristic: [details] after [redact_audit_details] equals [details], and
    known secret field names are absent. *)

val redacted_audit_event_to_json : redacted_audit_event -> Yojson.Safe.t
(** Export form — always runs details through [redact_audit_details]. *)

(** {1 JSON (secret-free export forms)} *)

val verified_endpoint_to_json : verified_endpoint -> Yojson.Safe.t
val private_proof_delivery_to_json : private_proof_delivery -> Yojson.Safe.t

val link_transaction_to_json : link_transaction -> Yojson.Safe.t
(** Export never includes proof secrets; [proof_challenge_id] is an opaque id
    only. *)

val admin_repair_plan_to_json : admin_repair_plan -> Yojson.Safe.t

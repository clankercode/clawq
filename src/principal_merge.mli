(** Deterministic Principal merge / adoption after verified linking
    (P21.M1.E1.T011).

    After a two-sided private proof completes (or an admin repair is applied),
    this module chooses a surviving Principal by a documented stable rule,
    atomically adopts non-conflicting identity links, external-account bindings,
    preferences, and current actor authority under compare-and-swap revisions,
    and leaves an immutable [Merged_into] tombstone on the loser.

    {2 Survivor rule (ordinary two-sided flow)}

    - Prefer the Principal with the earlier durable [created_at] (lexicographic
      ISO-8601 UTC).
    - On an exact [created_at] tie, prefer the smaller opaque [principal_id]
      (stable string order via {!Principal_identity.principal_id_compare}).
    - Admin repair may override with an explicit survivor (must be one of the
      two Principals). See {!Principal_link_protocol.survivor_selection}.

    {2 Conflict policy (fail closed)}

    - Distinct exclusive-slot external-account bindings that cannot coexist on
      one Principal refuse apply (no silent overwrite of credentials).
    - Identical external-account identities coalesce without copying credential
      authority.
    - Conflicting preferences keep the survivor's value and are enumerated in
      the preview/receipt; non-conflicting preferences are adopted.
    - Pending authorization on the loser is invalidated, not rebound.
    - Historical {!actor_snapshot} rows retain pre-merge actor evidence and the
      original owning Principal id; live resolution follows the tombstone, but
      snapshots never gain survivor credentials they did not already authorize.

    {2 Concurrency}

    Apply serializes under [BEGIN IMMEDIATE], CAS-checks both Principal
    revisions (and optional expected values), and refuses partial adoption.
    Replaying the same [link_tx_id] returns the stored receipt idempotently.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0005-separate-human-principals-from-room-sessions.md. *)

module P = Principal_identity
module L = Principal_link_protocol

(** {1 Survivor selection} *)

val select_survivor :
  left:P.principal ->
  right:P.principal ->
  ?selection:L.survivor_selection ->
  unit ->
  (P.principal * P.principal, string) result
(** Resolve [(survivor, loser)] using [selection] (default [By_creation_order]).

    Documented ordinary rule: earlier [created_at], then lexicographic
    [principal_id]. [Explicit id] requires [id] to be one of the two Principals.
    Same-id pair is rejected. *)

(** {1 External accounts (adoption / conflict stubs for E2)} *)

type external_account = {
  id : string;
  principal_id : P.principal_id;
  account_kind : string;  (** Logical provider, e.g. ["github"]. *)
  uniqueness_domain : string;
      (** Uniqueness namespace, e.g. ["github.com:app:42"]. *)
  account_identity : string;
      (** Stable external identity inside the domain (e.g. numeric user id). *)
  exclusive_slot : bool;
      (** When [true], a Principal may hold at most one identity per
          [(account_kind, uniqueness_domain)]. Distinct identities on the two
          Principals for the same exclusive slot refuse merge. *)
  revision : int;
  payload_json : string;  (** Opaque non-secret metadata; never credentials. *)
  created_at : string;
  updated_at : string;
}
(** Principal-owned external account binding (lightweight until E2 full GitHub
    bindings). Used for conflict detection and non-conflicting adoption. *)

val put_external_account :
  db:Sqlite3.db ->
  ?now:float ->
  external_account ->
  (external_account, string) result
(** Insert or replace by [id]. Enforces global uniqueness of
    [(account_kind, uniqueness_domain, account_identity)]. *)

val list_external_accounts :
  db:Sqlite3.db ->
  principal_id:P.principal_id ->
  (external_account list, string) result

(** {1 Preferences} *)

type preference = {
  principal_id : P.principal_id;
  key : string;
  value : string;
  revision : int;
  updated_at : string;
}

val put_preference :
  db:Sqlite3.db ->
  ?now:float ->
  principal_id:P.principal_id ->
  key:string ->
  value:string ->
  ?revision:int ->
  unit ->
  (preference, string) result

val list_preferences :
  db:Sqlite3.db ->
  principal_id:P.principal_id ->
  (preference list, string) result

(** {1 Pending authorization invalidation (stub counter)} *)

val set_pending_authorization_count :
  db:Sqlite3.db ->
  principal_id:P.principal_id ->
  count:int ->
  (unit, string) result

val get_pending_authorization_count :
  db:Sqlite3.db -> principal_id:P.principal_id -> (int, string) result

(** {1 Immutable actor snapshots} *)

type actor_snapshot = {
  id : string;
  actor_key : string;
  principal_id_at_snapshot : P.principal_id;
      (** Owning Principal when the snapshot was taken (often the merge loser).
          Never rewritten by later merges. *)
  actor_json : string;
      (** Full connector_actor JSON at snapshot time (evidence retention). *)
  reason : string;  (** e.g. ["pre_merge"]. *)
  merge_id : string option;
  created_at : string;
}
(** Immutable historical Actor evidence. Current authority follows live
    Principal lineage (tombstone redirect); snapshots do not re-attribute
    credentials. *)

val get_actor_snapshot :
  db:Sqlite3.db -> id:string -> (actor_snapshot option, string) result

val list_actor_snapshots_for_actor :
  db:Sqlite3.db -> actor_key:string -> (actor_snapshot list, string) result

(** {1 Preview / receipt / apply} *)

type hard_conflict =
  | External_account_collision of {
      uniqueness_domain : string;
      summary : string;
    }
  | Principal_not_mergeable of { principal_id : string; reason : string }
  | Other of { code : string; summary : string }

type preference_resolution = {
  key : string;
  outcome : [ `Adopted_from_loser | `Kept_survivor | `Identical ];
  survivor_value : string option;
  loser_value : string option;
}

type merge_preview = {
  survivor_id : P.principal_id;
  loser_id : P.principal_id;
  adopted_actor_keys : string list;
  adopted_link_ids : string list;
  hard_conflicts : hard_conflict list;
  preference_resolutions : preference_resolution list;
  pending_auth_invalidated : int;
  notes : string list;
}
(** Pure conflict/adopt preview; does not mutate. *)

type merge_receipt = {
  id : string;
  link_tx_id : string option;
  survivor_id : P.principal_id;
  loser_id : P.principal_id;
  adopted_actor_keys : string list;
  adopted_link_ids : string list;
  preference_resolutions : preference_resolution list;
  pending_auth_invalidated : int;
  actor_snapshot_ids : string list;
  survivor_revision_after : int;
  loser_revision_after : int;
  applied_at : string;
  notes : string list;
}
(** Durable apply receipt for idempotent replay. *)

type apply_status =
  | Applied of merge_receipt  (** Fresh atomic adopt + tombstone. *)
  | Idempotent of merge_receipt
      (** Same [link_tx_id] or already-merged pair; no re-apply. *)
  | Refused of {
      reason : string;
      conflicts : hard_conflict list;
      preview : merge_preview option;
    }  (** Fail closed; no partial adoption. *)
  | Stale_revision of string  (** Optimistic concurrency mismatch. *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent tables: merge receipts, external accounts, preferences, pending
    auth counts, actor snapshots. Also ensures
    {!Principal_identity_store.ensure_schema}. *)

val get_merge_receipt :
  db:Sqlite3.db -> id:string -> (merge_receipt option, string) result

val get_merge_receipt_by_link_tx :
  db:Sqlite3.db -> link_tx_id:string -> (merge_receipt option, string) result

val preview_merge :
  db:Sqlite3.db ->
  left_id:P.principal_id ->
  right_id:P.principal_id ->
  ?selection:L.survivor_selection ->
  unit ->
  (merge_preview, string) result
(** Build a preview without writing. Fails if either Principal is missing or not
    mergeable (disabled / already a tombstone to a third party). *)

val apply_merge :
  db:Sqlite3.db ->
  left_id:P.principal_id ->
  right_id:P.principal_id ->
  ?selection:L.survivor_selection ->
  ?expected_left_revision:int ->
  ?expected_right_revision:int ->
  ?link_tx_id:string ->
  ?merge_id:string ->
  ?now:float ->
  unit ->
  apply_status
(** Atomic merge under [BEGIN IMMEDIATE] with CAS revisions.

    Steps: snapshot loser's actors → reassign active actors/links to survivor →
    adopt non-conflicting accounts/preferences → invalidate pending auth on
    loser → tombstone loser as [Merged_into survivor] → store receipt.

    When [link_tx_id] matches an existing receipt, returns [Idempotent]. Hard
    conflicts return [Refused] with zero writes beyond the transaction rollback.
*)

val adopt_after_verified_link :
  db:Sqlite3.db ->
  ?principal_a:P.principal_id ->
  ?principal_b:P.principal_id ->
  ?expected_a_revision:int ->
  ?expected_b_revision:int ->
  ?selection:L.survivor_selection ->
  ?link_tx_id:string ->
  ?merge_id:string ->
  ?now:float ->
  unit ->
  apply_status
(** Post-verified-link entry point.

    - Both Principals present and distinct → {!apply_merge}
    - Same Principal → [Idempotent] no-op receipt (or existing by [link_tx_id])
    - Only one Principal → no merge (returns [Applied] empty-adopt receipt
      noting single-principal adopt; actors already owned stay put)
    - Neither → [Refused] *)

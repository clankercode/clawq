(** Unlink / split and identity revocation lifecycle (P21.M1.E1.T012).

    Unlinking a Connector actor is an identity split, not a reverse credential
    merge. A new Principal is created only through an explicit revision-bound
    split plan (self-service unlink builds one implicitly; admin repair uses
    plan-confirm-apply). Apply:

    - Moves the named actor to a new empty Principal
    - Transfers no account binding, credential, pending transaction, or
      authority automatically (default ownership retains all state on the source
      Principal)
    - Optionally rebinds only accounts/preferences named in an explicit
      ownership intent, failing closed on conflicts
    - Immediately revokes new authority: pending authorization and account
      leases are invalidated or marked rebind-required
    - Leaves historical {!Principal_merge.actor_snapshot} rows immutable
      (pre-unlink evidence keeps the original owning Principal id)

    Reverse-merge (restoring a [Merged_into] tombstone / undoing adoption) is
    always refused.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0005-separate-human-principals-from-room-sessions.md. *)

module P = Principal_identity
module M = Principal_merge

(** {1 Account leases (authority stubs until full vault E2/M2)} *)

type lease_status =
  | Active
  | Invalidated  (** Broken by unlink/split/revoke; not reusable. *)
  | Rebind_required
      (** Survives only as a marker that explicit rebind is required. *)

type account_lease = {
  id : string;
  principal_id : P.principal_id;
  account_id : string option;
  actor_key : string option;
  status : lease_status;
  revision : int;
  created_at : string;
  updated_at : string;
}

val put_account_lease :
  db:Sqlite3.db -> ?now:float -> account_lease -> (account_lease, string) result

val list_account_leases :
  db:Sqlite3.db ->
  principal_id:P.principal_id ->
  (account_lease list, string) result

val list_account_leases_for_actor :
  db:Sqlite3.db -> actor_key:string -> (account_lease list, string) result

(** {1 Ownership intent (accounts / preferences)} *)

type ownership_intent =
  | Retain_on_source
      (** Default: every external account and preference stays on the source
          Principal. New Principal is empty (default prefs only — none). *)
  | Explicit_rebind of {
      account_ids : string list;
          (** Must currently belong to the source Principal. *)
      preference_keys : string list;
    }
      (** What may move with the unlinked actor. Nothing transfers
          automatically. *)

type ownership_conflict =
  | Account_not_owned of { account_id : string; summary : string }
  | Preference_not_owned of { key : string; summary : string }
  | Reverse_merge_forbidden of { summary : string }
  | Other of { code : string; summary : string }

(** {1 Split plan (revision-bound)} *)

type plan_status =
  | Planned
  | Confirmed
  | Applied
  | Rejected
  | Expired
  | Cancelled
  | Stale_revision

val string_of_plan_status : plan_status -> string
val plan_status_of_string : string -> (plan_status, string) result
val plan_status_is_terminal : plan_status -> bool

type split_preview = {
  source_principal_id : P.principal_id;
  actor_key : string;
  ownership : ownership_intent;
  accounts_retained : string list;
  accounts_to_rebind : string list;
  preferences_retained : string list;
  preferences_to_rebind : string list;
  pending_auth_to_invalidate : int;
  leases_to_invalidate : int;
  hard_conflicts : ownership_conflict list;
  notes : string list;
}

type split_plan = {
  version : int;
  id : string;
  source_principal_id : P.principal_id;
  source_revision : int;  (** CAS-bound source Principal revision. *)
  actor_key : P.connector_actor_key;
  actor_revision : int;  (** CAS-bound actor revision. *)
  ownership : ownership_intent;
  admin_principal_id : P.principal_id option;
      (** Set for admin-repair style plans; [None] for self-service. *)
  preview : split_preview;
  digest : string;
  status : plan_status;
  created_at : string;
  expires_at : string;
  confirmed_at : string option;
  applied_at : string option;
  reject_reason : string option;
  new_principal_id : P.principal_id option;
      (** Filled after apply when a new Principal was created. *)
}
(** Explicit revision-bound plan. A new Principal is created only by applying a
    Confirmed plan (or the one-shot self-service path that builds and applies
    the same plan structure under CAS). *)

val protocol_version : int
val default_plan_ttl_seconds : float

val preview_unlink :
  db:Sqlite3.db ->
  source_principal_id:P.principal_id ->
  actor_key:P.connector_actor_key ->
  ?ownership:ownership_intent ->
  unit ->
  (split_preview, string) result
(** Pure-ish conflict/ownership preview; does not mutate. *)

val make_split_plan :
  db:Sqlite3.db ->
  id:string ->
  source_principal_id:P.principal_id ->
  actor_key:P.connector_actor_key ->
  ?ownership:ownership_intent ->
  ?admin_principal_id:P.principal_id ->
  ?ttl_seconds:float ->
  ?now:float ->
  unit ->
  (split_plan, string) result
(** Build a [Planned] plan bound to current source Principal + actor revisions.
    Persists the plan row. Rejects reverse-merge shapes and ownership conflicts
    at plan time (fail closed). *)

val get_split_plan :
  db:Sqlite3.db -> id:string -> (split_plan option, string) result

val confirm_split_plan :
  db:Sqlite3.db ->
  id:string ->
  presented_digest:string ->
  ?confirming_principal:P.principal_id ->
  ?now:float ->
  unit ->
  (split_plan, string) result
(** Planned → Confirmed. Admin plans require [confirming_principal] to match
    [admin_principal_id]. Natural language / external callbacks never confirm —
    caller must present the digest. *)

val cancel_split_plan :
  db:Sqlite3.db ->
  id:string ->
  ?now:float ->
  unit ->
  (split_plan, string) result

(** {1 Receipt / apply} *)

type unlink_receipt = {
  id : string;
  plan_id : string;
  source_principal_id : P.principal_id;
  new_principal_id : P.principal_id;
  actor_key : string;
  unlinked_link_id : string option;
  new_link_id : string;
  rebound_account_ids : string list;
  rebound_preference_keys : string list;
  pending_auth_invalidated : int;
  leases_invalidated : int;
  actor_snapshot_ids : string list;
  source_revision_after : int;
  new_principal_revision : int;
  actor_revision_after : int;
  applied_at : string;
  notes : string list;
}
(** Durable apply receipt for idempotent replay. *)

type apply_status =
  | Applied of unlink_receipt
  | Idempotent of unlink_receipt
  | Refused of {
      reason : string;
      conflicts : ownership_conflict list;
      preview : split_preview option;
    }
  | Stale_revision of string

val ensure_schema : Sqlite3.db -> unit
(** Idempotent tables: split plans, unlink receipts, account leases. Also
    ensures {!Principal_merge.ensure_schema}. *)

val get_unlink_receipt :
  db:Sqlite3.db -> id:string -> (unlink_receipt option, string) result

val get_unlink_receipt_by_plan :
  db:Sqlite3.db -> plan_id:string -> (unlink_receipt option, string) result

val apply_split_plan :
  db:Sqlite3.db ->
  id:string ->
  ?expected_source_revision:int ->
  ?expected_actor_revision:int ->
  ?now:float ->
  unit ->
  apply_status
(** Apply a [Confirmed] plan under [BEGIN IMMEDIATE] with CAS on source
    Principal and actor revisions.

    Steps: snapshot actor → create empty Principal → mark old identity link
    Unlinked → insert Active link on new Principal → reassign actor → optional
    explicit account/preference rebind → invalidate pending auth and leases →
    store receipt. Historical snapshots are never rewritten. *)

val unlink_actor :
  db:Sqlite3.db ->
  source_principal_id:P.principal_id ->
  actor_key:P.connector_actor_key ->
  ?ownership:ownership_intent ->
  ?expected_source_revision:int ->
  ?expected_actor_revision:int ->
  ?plan_id:string ->
  ?unlink_id:string ->
  ?now:float ->
  unit ->
  apply_status
(** Self-service one-shot: plan (Confirmed) + apply under the same revision
    bindings. Equivalent to an explicit split plan without a separate confirm
    step. Idempotent when [plan_id] matches an existing receipt. *)

val refuse_reverse_merge :
  db:Sqlite3.db ->
  survivor_id:P.principal_id ->
  loser_id:P.principal_id ->
  ?now:float ->
  unit ->
  apply_status
(** Always [Refused]. Unlink/split never restores a [Merged_into] tombstone or
    reverses credential adoption. *)

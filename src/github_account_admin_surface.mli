(** Redacted private self-service and revision-bound admin surfaces for GitHub
    account inspection, preferences, and unlink/split/revocation
    (P21.M1.E2.T004).

    Surfaces never expose vault tokens, sealed ciphertext, OAuth secrets, or raw
    vault row ids. Mutable lifecycle operations route through the canonical
    modules:

    - {!Github_account_binding} — account identity, snapshots, status
    - {!Github_account_preference} — preference list / resolve
    - {!Github_account_ownership_policy} — ownership conflicts
    - {!Principal_unlink_split} — Connector unlink / identity split
    - {!Github_user_token_cas} — vault deactivate + lease invalidation when keys
      and a vault attachment are present

    Conflict disclosure happens before confirmation (plan digest). Apply
    invalidates affected authority immediately (binding status, optional vault
    CAS, principal unlink leases/pending auth). Immutable historical attribution
    is preserved via binding snapshots and actor snapshots; live authority
    follows current rows only.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

module P = Principal_identity
module B = Github_account_binding
module Pref = Github_account_preference
module U = Principal_unlink_split
module V = Github_user_token_vault

val schema_version : int
(** Surface schema / export version; starts at 1. *)

val ensure_schema : Sqlite3.db -> unit
(** Ensures Principal, binding, preference, unlink/split, and vault tables. *)

(** {1 Surface caller} *)

type surface =
  | Self_service of { principal_id : P.principal_id }
      (** Private self-service: may only inspect/mutate own Principal. *)
  | Admin of {
      admin_principal_id : P.principal_id;
      subject_principal_id : P.principal_id;
      reason : string;
          (** Non-empty operator reason recorded on plans and redacted exports.
          *)
    }  (** Revision-bound admin repair surface. *)

val subject_principal : surface -> P.principal_id
val string_of_surface_kind : surface -> string

val make_self_service :
  principal_id:P.principal_id -> unit -> (surface, string) result

val make_admin :
  admin_principal_id:P.principal_id ->
  subject_principal_id:P.principal_id ->
  reason:string ->
  unit ->
  (surface, string) result
(** Require non-empty trimmed [reason]. *)

(** {1 Redacted account / preference views}

    No tokens, vault ciphertext, or vault row ids. [vault_attached] is a
    presence flag only. *)

type redacted_account = {
  binding_id : string;
  lineage_id : string;
  principal_id : string;
  host : string;
  app_id : int;
  github_user_id : int64;
  login : string option;
  avatar_url : string option;
  authorization_status : string;
  revision : int;
  vault_attached : bool;
  created_at : string;
  updated_at : string;
}

type redacted_preference = {
  principal_id : string;
  scope : string;
  scope_key : string;
  binding_id : string option;
  lineage_id : string option;
  revision : int;
  updated_at : string;
}

type redacted_snapshot = {
  snapshot_id : string;
  binding_id : string;
  principal_id_at_snapshot : string;
      (** Owning Principal when the snapshot was taken — never rewritten. *)
  lineage_id : string;
  reason : string;
  related_id : string option;
  created_at : string;
  authorization_status_at_snapshot : string option;
  login_at_snapshot : string option;
}
(** Historical attribution summary. Does not re-export raw [binding_json] (which
    may carry an opaque vault id). *)

type account_inspect = {
  surface_kind : string;
  principal_id : string;
  admin_principal_id : string option;
  admin_reason : string option;
  accounts : redacted_account list;
  preferences : redacted_preference list;
  notes : string list;
}

type preference_view = {
  surface_kind : string;
  principal_id : string;
  preferences : redacted_preference list;
  resolve : Pref.resolve_result option;
      (** Optional resolve against a private context; never auto-picks by login
          or another participant. *)
}

val inspect_accounts :
  db:Sqlite3.db -> surface:surface -> unit -> (account_inspect, string) result
(** List redacted accounts + preferences for the subject Principal. *)

val inspect_account :
  db:Sqlite3.db ->
  surface:surface ->
  binding_id:string ->
  unit ->
  (redacted_account * redacted_snapshot list, string) result
(** Single binding + immutable historical snapshots. Fails when the binding is
    not owned by the subject Principal. *)

val view_preferences :
  db:Sqlite3.db ->
  surface:surface ->
  ?resolve_context:Pref.resolve_context ->
  unit ->
  (preference_view, string) result
(** Preference listing. When [resolve_context] is provided it must target the
    subject Principal; foreign contexts are refused. *)

val set_preference :
  db:Sqlite3.db ->
  surface:surface ->
  ?now:float ->
  scope:Pref.preference_scope ->
  value:Pref.preference_value ->
  unit ->
  (redacted_preference, string) result
(** Self-service / admin preference write for the subject Principal. Does not
    establish ownership; resolve revalidates eligibility. *)

val clear_preference :
  db:Sqlite3.db ->
  surface:surface ->
  scope:Pref.preference_scope ->
  unit ->
  (unit, string) result

(** {1 Account revoke / unlink plans (conflict disclosure before confirm)} *)

type account_action_kind =
  | Revoke
      (** Upstream or local revoke; binding → [Revoked]; vault deactivated when
          keys present. Relink required. *)
  | Unlink_account
      (** Explicit account unlink; binding → [Unlinked], vault ref cleared;
          vault deactivated when keys present. *)
  | Disable
      (** Temporary local hold; binding → [Disabled]; vault deactivated when
          keys present. *)

val string_of_account_action_kind : account_action_kind -> string

type conflict = { code : string; summary : string; related_ids : string list }
(** Hard conflict disclosed before confirmation. Empty list means plan is
    applyable (subject to CAS at apply time). *)

type account_action_plan = private {
  version : int;
  kind : account_action_kind;
  binding_id : string;
  lineage_id : string;
  principal_id : string;
  expected_binding_revision : int;
  vault_attached : bool;
  hard_conflicts : conflict list;
  notes : string list;
  will_snapshot : bool;
  will_invalidate_vault : bool;
      (** [true] when a vault is attached; apply uses CAS when keys are
          supplied. *)
  will_clear_vault_ref : bool;
  digest : string;
      (** SHA-256 of the canonical plan body; must be presented at apply. *)
  surface_kind : string;
  admin_principal_id : string option;
  admin_reason : string option;
  created_at : string;
}
(** Opaque issued plan. Callers may inspect its redacted fields but cannot forge
    or alter its confirmation-bound contents. *)

val plan_account_action :
  db:Sqlite3.db ->
  surface:surface ->
  kind:account_action_kind ->
  binding_id:string ->
  ?now:float ->
  unit ->
  (account_action_plan, string) result
(** Build a plan with conflict disclosure. Does not mutate. Plans with non-empty
    [hard_conflicts] must not be applied. *)

type account_action_receipt = {
  kind : account_action_kind;
  binding_id : string;
  lineage_id : string;
  principal_id : string;
  previous_status : string;
  new_status : string;
  binding_revision_after : int;
  snapshot_id : string option;
  vault_invalidated : bool;
  leases_invalidated : int;
  vault_ref_cleared : bool;
  applied_at : string;
  notes : string list;
}

type account_apply_status =
  | Applied of account_action_receipt
  | Refused of { reason : string; conflicts : conflict list }
  | Stale_revision of string

val apply_account_action :
  db:Sqlite3.db ->
  surface:surface ->
  plan:account_action_plan ->
  presented_digest:string ->
  ?keys:V.key_provider ->
  ?now:float ->
  unit ->
  account_apply_status
(** Confirm + apply. Requires matching [presented_digest], no hard conflicts,
    and CAS on binding revision.

    For [Revoke] / [Unlink_account] with [keys], runs the canonical
    {!Github_user_auth_invalidate} lifecycle: local disable + lineage break
    first, optional remote revoke, always destroy secrets for destructive kinds.
    [Disable] keeps the lighter CAS-deactivate path (secrets retained). Never
    returns or logs token material. *)

(** {1 Connector unlink / split (canonical lifecycle)} *)

type actor_unlink_surface_plan = {
  plan : U.split_plan;
  github_accounts_retained : redacted_account list;
      (** GitHub bindings stay on the source Principal (no silent transfer). *)
  preferences : redacted_preference list;
  hard_conflicts : conflict list;
      (** Normalized from the split plan preview for surface consumers. *)
}

val plan_actor_unlink :
  db:Sqlite3.db ->
  surface:surface ->
  actor_key:P.connector_actor_key ->
  ?ownership:U.ownership_intent ->
  ?plan_id:string ->
  ?ttl_seconds:float ->
  ?now:float ->
  unit ->
  (actor_unlink_surface_plan, string) result
(** Revision-bound split plan through {!Principal_unlink_split}. Self-service
    plans omit admin principal; admin plans bind [admin_principal_id]. Conflicts
    (including GitHub rebind refusals) are disclosed on the plan preview. *)

val confirm_actor_unlink :
  db:Sqlite3.db ->
  surface:surface ->
  plan_id:string ->
  presented_digest:string ->
  ?now:float ->
  unit ->
  (U.split_plan, string) result
(** Planned → Confirmed. The supplied surface must match the durable plan:
    self-service requires its own source Principal and no admin binding; admin
    requires both the plan's admin and subject Principals. *)

val apply_actor_unlink :
  db:Sqlite3.db ->
  surface:surface ->
  plan_id:string ->
  ?expected_source_revision:int ->
  ?expected_actor_revision:int ->
  ?now:float ->
  unit ->
  U.apply_status
(** Apply a Confirmed plan only when the supplied surface matches the durable
    plan's self-service or admin+subject binding. Invalidates pending auth and
    leases immediately; historical actor snapshots remain immutable. *)

val actor_unlink_self_service :
  db:Sqlite3.db ->
  surface:surface ->
  actor_key:P.connector_actor_key ->
  ?ownership:U.ownership_intent ->
  ?expected_source_revision:int ->
  ?expected_actor_revision:int ->
  ?plan_id:string ->
  ?unlink_id:string ->
  ?now:float ->
  unit ->
  U.apply_status
(** Self-service one-shot plan+apply. Refused when [surface] is [Admin] (admins
    must use plan-confirm-apply). *)

(** {1 Redacted JSON exports (no secrets)} *)

val redacted_account_to_json : redacted_account -> Yojson.Safe.t
val redacted_preference_to_json : redacted_preference -> Yojson.Safe.t
val redacted_snapshot_to_json : redacted_snapshot -> Yojson.Safe.t
val account_inspect_to_json : account_inspect -> Yojson.Safe.t
val preference_view_to_json : preference_view -> Yojson.Safe.t
val conflict_to_json : conflict -> Yojson.Safe.t
val account_action_plan_to_json : account_action_plan -> Yojson.Safe.t
val account_action_receipt_to_json : account_action_receipt -> Yojson.Safe.t

val actor_unlink_surface_plan_to_json :
  actor_unlink_surface_plan -> Yojson.Safe.t
(** Includes plan id, digest, status, preview notes/conflicts, and redacted
    GitHub accounts. Never embeds vault tokens. *)

val json_contains_plaintext : json:Yojson.Safe.t -> plaintext:string -> bool
(** Test helper: true if [plaintext] appears anywhere in the JSON tree. *)

val redacted_account_of_binding : B.binding -> redacted_account
(** Pure conversion used by inspect and plans. *)

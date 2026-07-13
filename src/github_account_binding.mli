(** Principal-owned GitHub account bindings (P21.M1.E2.T001).

    A binding is durable authorization state for one verified GitHub numeric
    user under one GitHub App on one host, owned by a surviving Principal.

    Identity is immutable: [host] + [app_id] + [github_user_id]. Login and
    avatar are mutable display metadata — changing them never creates a new
    account. The vault reference is an opaque handle/id only (see
    {!Github_user_token_vault}); no token plaintext appears on the binding.

    Logical [lineage_id] versions one binding's authority lineage across
    ordinary display updates and Principal adoption. Historical
    {!binding_snapshot} rows retain prior Principal ownership and evidence
    without rewriting.

    Principal adoption reassigns ownership under [BEGIN IMMEDIATE] with
    compare-and-swap [revision], writing an immutable snapshot first.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

module P = Principal_identity

val schema_version : int
(** Binding schema version; starts at 1. *)

val default_host : string
(** V1 live support host: [github.com]. *)

(** {1 Authorization status} *)

type authorization_status =
  | Pending  (** Authorization in flight; not yet usable for user actions. *)
  | Authorized  (** Verified and usable (subject to vault readiness). *)
  | Disabled
      (** Locally disabled (e.g. restore, operator, or temporary hold). *)
  | Revoked  (** Upstream or local revoke; requires relink. *)
  | Unlinked  (** Explicit unlink/split; binding retains historical identity. *)

val string_of_authorization_status : authorization_status -> string

val authorization_status_of_string :
  string -> (authorization_status, string) result

(** {1 Opaque vault reference}

    Only the vault row id / handle is stored. Callers must never place token
    plaintext, refresh material, or sealed ciphertext on a binding. *)

type vault_ref = string

val make_vault_ref : string -> (vault_ref, string) result
(** Accept a non-empty trimmed opaque id. Rejects empty/whitespace. *)

val vault_ref_to_string : vault_ref -> string

(** {1 Account identity (immutable)} *)

type account_identity = { host : string; app_id : int; github_user_id : int64 }
(** Canonical GitHub account identity. Equal user IDs under different hosts or
    Apps are distinct. Login is deliberately absent. *)

val make_account_identity :
  ?host:string ->
  app_id:int ->
  github_user_id:int64 ->
  unit ->
  (account_identity, string) result
(** [app_id] and [github_user_id] must be positive; host non-empty. *)

val account_identity_key : account_identity -> string
(** Deterministic key: ["host:<h>:app:<id>:user:<uid>"]. *)

val uniqueness_domain : account_identity -> string
(** Exclusive-slot domain for merge conflict checks: ["<host>:app:<app_id>"]. *)

val account_identity_equal : account_identity -> account_identity -> bool

(** {1 Mutable display metadata} *)

type display = { login : string option; avatar_url : string option }
(** Mutable presentation. Never part of identity or lineage. *)

val empty_display : display

(** {1 Binding} *)

type binding = {
  version : int;
  id : string;  (** Opaque binding row id. *)
  principal_id : P.principal_id;  (** Current surviving owner. *)
  identity : account_identity;  (** Immutable host/App/numeric user. *)
  display : display;  (** Mutable login/avatar. *)
  authorization_status : authorization_status;
  revision : int;  (** Monotonic CAS revision. *)
  lineage_id : string;
      (** Logical binding lineage; stable across login changes and Principal
          adoption. Relink after revoke/destroy starts a new lineage. *)
  vault_ref : vault_ref option;
      (** Opaque {!Github_user_token_vault} id only; never tokens. *)
  created_at : string;
  updated_at : string;
}

val make_binding :
  id:string ->
  principal_id:P.principal_id ->
  identity:account_identity ->
  ?display:display ->
  ?authorization_status:authorization_status ->
  ?revision:int ->
  ?lineage_id:string ->
  ?vault_ref:vault_ref ->
  ?created_at:string ->
  ?updated_at:string ->
  unit ->
  binding
(** Defaults: empty display, [Pending], revision [1], [lineage_id = id] when
    blank, empty timestamps for the persistence layer. *)

(** {1 Immutable binding snapshots} *)

type binding_snapshot = {
  id : string;
  binding_id : string;
  principal_id_at_snapshot : P.principal_id;
      (** Owning Principal when the snapshot was taken. Never rewritten. *)
  lineage_id : string;
  binding_json : string;
      (** Full binding JSON at snapshot time (evidence retention; no tokens). *)
  reason : string;  (** e.g. ["pre_adopt"], ["pre_merge"]. *)
  related_id : string option;  (** Optional merge id or operation id. *)
  created_at : string;
}
(** Historical evidence. Live authority follows the current binding row;
    snapshots do not re-attribute credentials. *)

(** {1 Schema} *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent tables: [github_account_bindings],
    [github_account_binding_snapshots]. Also ensures
    {!Principal_identity_store.ensure_schema}. *)

(** {1 CRUD} *)

val insert : db:Sqlite3.db -> ?now:float -> binding -> (binding, string) result
(** Insert a binding. Fails if [id] already exists or the immutable
    [(host, app_id, github_user_id)] identity is already bound. Fills blank
    timestamps and blank [lineage_id] (defaults to [id]). *)

val get : db:Sqlite3.db -> id:string -> (binding option, string) result

val get_by_identity :
  db:Sqlite3.db -> identity:account_identity -> (binding option, string) result

val list_for_principal :
  db:Sqlite3.db -> principal_id:P.principal_id -> (binding list, string) result

val list_for_app_user :
  db:Sqlite3.db ->
  app_id:int ->
  github_user_id:int64 ->
  ?host:string ->
  unit ->
  (binding list, string) result
(** Every binding matching App + numeric GitHub user. Optional [host] narrows
    further; without it, all hosts for that App/user are returned (revocation
    webhooks are App-scoped and host-agnostic). Ordered by [created_at], [id].
*)

val delete : db:Sqlite3.db -> id:string -> (unit, string) result
(** Hard-delete the live binding row. Snapshots are retained. Missing id is an
    error. Does not touch vault rows. *)

(** {1 Mutations (identity preserved)} *)

val update_display :
  db:Sqlite3.db ->
  ?expected_revision:int ->
  ?login:string option ->
  ?avatar_url:string option ->
  ?now:float ->
  id:string ->
  unit ->
  (binding, string) result
(** Mutate login and/or avatar only. Does not create a new account, change
    [identity], or advance [lineage_id]. Bumps [revision]. *)

val update_authorization_status :
  db:Sqlite3.db ->
  ?expected_revision:int ->
  ?now:float ->
  id:string ->
  status:authorization_status ->
  unit ->
  (binding, string) result
(** Set authorization status. Identity and lineage unchanged. Bumps [revision].
*)

val set_vault_ref :
  db:Sqlite3.db ->
  ?expected_revision:int ->
  ?now:float ->
  id:string ->
  vault_ref:vault_ref option ->
  unit ->
  (binding, string) result
(** Attach or clear the opaque vault handle only. Never accepts or stores token
    material. *)

val update :
  db:Sqlite3.db ->
  ?expected_revision:int ->
  ?display:display ->
  ?authorization_status:authorization_status ->
  ?vault_ref:vault_ref option ->
  ?now:float ->
  id:string ->
  unit ->
  (binding, string) result
(** Combined CAS update for mutable fields. [identity] and [lineage_id] are
    immutable here. *)

(** {1 Lineage snapshots} *)

val snapshot :
  db:Sqlite3.db ->
  ?now:float ->
  ?reason:string ->
  ?related_id:string ->
  ?snapshot_id:string ->
  id:string ->
  unit ->
  (binding_snapshot, string) result
(** Persist an immutable snapshot of the current binding row (prior evidence).
    Does not mutate the live binding. *)

val get_snapshot :
  db:Sqlite3.db -> id:string -> (binding_snapshot option, string) result

val list_snapshots_for_binding :
  db:Sqlite3.db -> binding_id:string -> (binding_snapshot list, string) result

(** {1 Principal adoption (transactional)} *)

val adopt_to_principal :
  db:Sqlite3.db ->
  ?expected_revision:int ->
  ?now:float ->
  ?reason:string ->
  ?related_id:string ->
  id:string ->
  to_principal:P.principal_id ->
  unit ->
  (binding * binding_snapshot, string) result
(** Under one IMMEDIATE transaction: write a historical snapshot retaining the
    prior Principal and evidence, then reassign [principal_id] to
    [to_principal]. [identity], [lineage_id], and [vault_ref] are preserved (no
    credential copy from another binding). Bumps [revision]. *)

val adopt_all_for_principal :
  db:Sqlite3.db ->
  ?now:float ->
  ?reason:string ->
  ?related_id:string ->
  from_principal:P.principal_id ->
  to_principal:P.principal_id ->
  unit ->
  ( (binding * binding_snapshot) list,
    [ `Msg of string | `Conflict of string ] )
  result
(** Transactionally adopt every binding owned by [from_principal] into
    [to_principal].

    - Identical identity already on the survivor: snapshot loser evidence, then
      delete the loser row (coalesce; no credential copy).
    - Distinct identities under the same exclusive slot ([host]+[app_id]):
      refuse with [`Conflict] (fail closed).
    - Otherwise: snapshot + reassign.

    Zero partial writes on conflict. *)

(** {1 Introspection (no secrets)} *)

val binding_to_json : binding -> Yojson.Safe.t
(** Metadata JSON only — vault ref is the opaque id string; never tokens. *)

val binding_of_json : Yojson.Safe.t -> (binding, string) result

val binding_snapshot_to_json : binding_snapshot -> Yojson.Safe.t
(** Snapshot evidence JSON (prior Principal retained). *)

(** Shared verified pending-credential activation transaction (P21.M2.E2.T004).

    Flow-neutral seam used by both web (PKCE) and device authorization after a
    completed OAuth exchange. Accepts ephemeral pending credentials only when
    the source authorization transaction is in a valid post-exchange state,
    validates token response shape and GitHub [/user] numeric identity, seals
    Principal/binding/generation plus a revision-bound redacted plan, and
    activates atomically only after matching private confirmation.

    Fail-closed: collision, identity/Principal mismatch, replay, expiry,
    cancellation, confirmation mismatch, or changed Principal destroys pending
    material and preserves any prior live Authorized binding/vault.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

module Tx = Github_user_auth_tx
module V = Github_user_token_vault
module B = Github_account_binding
module S = Github_user_token_store
module Op = Github_account_ownership_policy

val schema_version : int
(** Activation-row schema version; starts at 1. *)

val default_ttl_seconds : float
(** Default pending-activation TTL (seconds): 15 minutes. *)

(** {1 Pending credentials (ephemeral)} *)

type pending_credential = {
  access_token : string;
  refresh_token : string option;
  scopes : string list;
  expires_in : int;
      (** Seconds until access-token expiry. Required (expiring tokens only). *)
  token_type : string option;
}
(** Ephemeral OAuth token material from a completed web or device exchange. Must
    not be logged, Room-exported, or written to redacted plans. *)

val make_pending_credential :
  access_token:string ->
  ?refresh_token:string ->
  ?scopes:string list ->
  expires_in:int ->
  ?token_type:string ->
  unit ->
  (pending_credential, string) result
(** Validate shape: non-empty access token, positive [expires_in]. Web PKCE
    callbacks project their token response through this constructor before
    [prepare] (no module cycle with the callback). *)
(** Validate shape: non-empty access token, positive [expires_in]. *)

val pending_credential_of_pkce :
  Github_user_auth_pkce_callback.token_response ->
  (pending_credential, string) result
(** Project a web token response into a pending credential. *)
(** Validate shape: non-empty access token, positive [expires_in]. Web PKCE and
    device-poll callers project their token responses through this constructor
    before [prepare] (avoids module cycles with flow-specific poll/callback
    modules). Device grants must supply positive [expires_in] (fail closed when
    GitHub omits lifetime). *)


(** {1 GitHub /user identity (injectable)} *)

type github_user = {
  id : int64;  (** Numeric GitHub user id (account identity). *)
  login : string;
  avatar_url : string option;
}

type fetch_user = access_token:string -> (github_user, string) result
(** Injectable [/user] probe. Tests inject offline fakes; production wires
    authenticated [GET /user]. *)

(** {1 Activation status} *)

type activation_status =
  | Pending_confirmation
      (** Credentials sealed; redacted plan ready; awaiting private
          confirmation. *)
  | Activated  (** Binding Authorized under matching confirmation. *)
  | Destroyed
      (** Fail-closed teardown of pending material; prior state preserved. *)
  | Expired
  | Cancelled
  | Rejected  (** Mismatch / collision / Principal change; terminal. *)

val string_of_activation_status : activation_status -> string
val activation_status_of_string : string -> (activation_status, string) result

val status_is_terminal : activation_status -> bool
(** [true] for every status except [Pending_confirmation]. *)

(** {1 Activation mode} *)

type activation_mode =
  | New_binding  (** No prior live binding for the App+user identity. *)
  | Supersede_pending
      (** Same Principal already had a [Pending] binding; pending vault/material
          was replaced. *)

val string_of_activation_mode : activation_mode -> string

(** {1 Revision-bound redacted plan} *)

type redacted_plan = {
  plan_id : string;
  digest : string;
      (** SHA-256 hex of canonical redacted body (no tokens). Confirmation binds
          to this digest. *)
  principal_id : string;
  principal_revision : int;
  base_revision : string;
      (** Bound from the source authorization transaction. *)
  flow_kind : Tx.flow_kind;
  auth_tx_id : string;
  host : string;
  app_id : int;
  github_user_id : int64;
  login : string;
  avatar_url : string option;
  scopes : string list;
  vault_id : string;  (** Opaque vault handle only. *)
  vault_generation : int;
  binding_id : string;
  binding_revision : int;
  mode : activation_mode;
  created_at : string;
  expires_at : string;
}
(** Secret-free binding plan. Never carries access/refresh tokens, confirmation
    plaintext, client secrets, or device codes. *)

val redacted_plan_to_json : redacted_plan -> Yojson.Safe.t
val compute_plan_digest : redacted_plan -> string

(** {1 Durable activation transaction} *)

type activation = {
  version : int;
  id : string;
  status : activation_status;
  principal_id : string;
  principal_revision : int;
  flow_kind : Tx.flow_kind;
  auth_tx_id : string;
  base_revision : string;
  host : string;
  app_id : int;
  github_user_id : int64;
  login : string;
  avatar_url : string option;
  scopes : string list;
  vault_id : string;
  vault_generation : int;
  binding_id : string;
  binding_revision : int;
  mode : activation_mode;
  plan_id : string;
  plan_digest : string;
  created_at : string;
  expires_at : string;
  activated_at : string option;
  destroyed_at : string option;
  terminal_reason : string option;
  updated_at : string;
}
(** Durable activation row. Confirmation token plaintext is never stored (only a
    hash). Token ciphertext lives only in the vault. *)

(** {1 Outcomes} *)

type prepared = {
  activation : activation;
  plan : redacted_plan;
  confirmation_token : string;
      (** One-time private confirmation secret. Returned once; never re-readable
          from storage. Deliver only on a private channel. *)
  vault : V.vault_record;
  binding : B.binding;  (** [Pending]; not [Authorized]. *)
  github_user : github_user;
}
(** Result of [prepare]. Tokens never appear as plaintext fields. *)

type activated = {
  activation : activation;
  plan : redacted_plan;
  vault : V.vault_record;
  binding : B.binding;  (** [Authorized] after successful confirm. *)
}
(** Result of [confirm]. *)

(** {1 Failures} *)

type failure_kind =
  | Invalid_credential of string
      (** Missing/invalid token response shape (access token, expires_in, …). *)
  | Incomplete_exchange
      (** Source auth transaction is not in a post-exchange state eligible for
          activation (e.g. still open for web, or missing). *)
  | Replay
      (** Auth tx already has a sealed/activated activation, or activation
          already terminal. *)
  | Expired
  | Cancelled
  | Principal_changed of string
      (** Principal missing, tombstoned, disabled, or revision CAS mismatch. *)
  | Identity_mismatch of string
      (** Intended account pin or [/user] shape failed. *)
  | User_probe of string  (** Injectable [/user] probe failed. *)
  | Collision of string
      (** Duplicate ownership / identity already Authorized by same or other
          Principal; prior state preserved. *)
  | Confirmation_mismatch
  | Plan_mismatch
  | Not_found
  | Already_activated
  | Destroyed_status
  | Partial of string
      (** Seal/bind/plan failed after partial work; pending material destroyed.
      *)
  | Storage of string
  | Invalid of string

type failure = {
  kind : failure_kind;
  message : string;  (** Actionable, secret-free operator/user message. *)
  activation : activation option;
      (** Related activation when known (may already be terminal). *)
}
(** Fail-closed error. Never embeds access_token, refresh_token, or confirmation
    plaintext. *)

val string_of_failure_kind : failure_kind -> string
val redacted_summary : activation -> string
val redacted_prepared_summary : prepared -> string

(** {1 Schema} *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent table [github_user_auth_activate]. Also ensures auth-tx, vault,
    binding, and Principal schemas. *)

(** {1 Introspection} *)

val get : db:Sqlite3.db -> id:string -> (activation option, failure) result

val get_by_auth_tx :
  db:Sqlite3.db -> auth_tx_id:string -> (activation option, failure) result

val get_plan :
  db:Sqlite3.db ->
  activation_id:string ->
  (redacted_plan option, failure) result

val is_activated : activation -> bool

val has_active_binding : binding:B.binding -> bool
(** [true] only when [authorization_status = Authorized]. *)

(** {1 Prepare (seal + plan)} *)

val prepare :
  db:Sqlite3.db ->
  keys:V.key_provider ->
  ?fetch_user:fetch_user ->
  auth_tx_id:string ->
  credential:pending_credential ->
  ?now:float ->
  ?ttl_seconds:float ->
  ?activation_id:string ->
  ?vault_id:string ->
  ?binding_id:string ->
  ?plan_id:string ->
  unit ->
  (prepared, failure) result
(** Validate pending credential + completed exchange → probe [/user] → seal
    vault + [Pending] binding → revision-bound redacted plan → pending
    activation with private confirmation token.

    Requires:
    - [credential] shape valid (access token + positive expires_in)
    - source [auth_tx_id] exists and is activation-eligible:
    - [Web_pkce]: status [Completed] (one-shot exchange already claimed)
    - [Device]: status [Open] or [Completed] (device grant delivered tokens;
      cancelled/expired/rejected refuse)
    - unexpired auth tx + Principal active at pinned revision
    - identity not already [Authorized] (collision preserves prior)
    - no prior non-destroyed activation for the same auth tx (replay)

    On any seal/bind/plan failure: destroy partial vault/binding rows created by
    this call and return [Partial] / typed failure with no [Authorized] binding
    introduced. *)

(** {1 Private confirm + atomic activate} *)

val confirm :
  db:Sqlite3.db ->
  keys:V.key_provider ->
  activation_id:string ->
  confirmation_token:string ->
  ?expected_principal_id:string ->
  ?expected_plan_digest:string ->
  ?now:float ->
  unit ->
  (activated, failure) result
(** Match private confirmation (constant-time hash compare) and atomically
    activate:

    1. Load pending activation; refuse terminal/expired/missing 2. Constant-time
    confirmation-token match 3. Revalidate Principal lineage + pinned revision
    (CAS) 4. Revalidate source auth tx not cancelled/rejected and Principal
    still owns the bound context 5. Optional plan-digest and principal-id match
    6. Under [BEGIN IMMEDIATE]: CAS [Pending] → [Authorized] on the binding and
    mark activation [Activated]

    Mismatch / Principal change / expiry / cancellation destroys pending vault
    material when the binding is still non-Authorized and preserves any prior
    Authorized state (none introduced by this activation). *)

(** {1 Fail-closed destroy} *)

val destroy :
  db:Sqlite3.db ->
  keys:V.key_provider ->
  activation_id:string ->
  ?reason:string ->
  ?now:float ->
  unit ->
  (activation, failure) result
(** Destroy pending material for a non-activated activation: delete sealed vault
    when the binding is still [Pending] and vault_ref matches, clear vault_ref /
    delete Pending binding as appropriate, mark activation [Destroyed]. Never
    tears down a live [Authorized] binding from another lineage. Idempotent when
    already [Destroyed]. *)

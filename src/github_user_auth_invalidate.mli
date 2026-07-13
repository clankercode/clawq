(** Unlink and Principal / Connector removal invalidation (P21.M3.E1.T004).

    Canonical lifecycle for breaking GitHub user-authorization authority:

    1. {b Local first}: CAS-deactivate the vault (when present), set binding
    status, break logical [lineage_id], discard process-local leases, and
    invalidate pending-authorization counters so delayed work fails closed. 2.
    {b Optional remote}: revoke the upstream App user token or grant under a
    narrow revocation-scoped open of sealed material (never re-enables local
    access). Remote failure is recorded and never restores authority. 3.
    {b Always destroy secrets}: sealed vault rows are destroyed after the local
    fence regardless of remote outcome.

    Principal removal, Connector unlink/split hooks, and self-service account
    unlink/revoke share this module so authority breaks before any network work
    and old lineage pins never follow a later relink.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

module V = Github_user_token_vault
module B = Github_account_binding
module C = Github_user_token_cas
module L = Github_user_token_lease
module P = Principal_identity

(** {1 Kind / remote mode} *)

type kind =
  | Disable
      (** Temporary local hold. Breaks leases/lineage fence? No — disable keeps
          lineage and secrets; only deactivates vault + [Disabled] status. *)
  | Revoke
      (** Upstream or local revoke; break lineage, remote grant revoke
          preferred, destroy secrets. *)
  | Unlink
      (** Explicit account unlink; break lineage, clear vault_ref, destroy
          secrets, remote grant revoke preferred. *)
  | Principal_removal
      (** Principal disable / removal: revoke every owned binding. *)
  | Connector_split
      (** Connector unlink/split hook: same destructive path as [Unlink] for
          bindings that must lose authority with the split. *)

val string_of_kind : kind -> string

type remote_mode =
  | Skip  (** No network work. *)
  | Revoke_token
      (** [DELETE /applications/\{client_id\}/token] with the access token. *)
  | Revoke_grant
      (** [DELETE /applications/\{client_id\}/grant] — stronger; revokes the
          whole App authorization for the user. Preferred for unlink/revoke. *)

val string_of_remote_mode : remote_mode -> string

val default_remote_mode : kind -> remote_mode
(** [Skip] for [Disable]; [Revoke_grant] for destructive kinds. *)

(** {1 Injectable remote boundary} *)

type http_delete =
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, string) result
(** Injectable HTTP DELETE. Returns [(status_code, body)] or transport error.
    Never log [body] (may embed tokens in request construction only). *)

type resolve_client =
  client_id_handle:string -> (string * string, string) result
(** Resolve opaque client-id handle → [(client_id, client_secret)]. Secret lives
    only for Basic auth construction. *)

val revoke_endpoint :
  ?host:string -> client_id:string -> mode:remote_mode -> unit -> string
(** Absolute GitHub API URL for token/grant revoke. [Skip] yields empty. *)

(** {1 Narrow revocation open}

    After local disable the vault is inactive, so ordinary leases refuse. This
    helper opens sealed material solely for the remote revoke callback without
    re-enabling the vault or registering a reusable lease. *)

val with_revocation_token :
  db:Sqlite3.db ->
  keys:V.key_provider ->
  ?expected:V.account_key ->
  vault_id:string ->
  f:(access_token:string -> refresh_token:string option -> 'a) ->
  unit ->
  ('a, V.denial) result
(** Open vault (active or inactive) and pass tokens only to [f]. Does not
    advance generation, does not set [active], and never returns token material
    as a plain value. *)

(** {1 Redacted effect / receipt} *)

type remote_outcome =
  | Remote_skipped of string
  | Remote_succeeded of { status_code : int; mode : remote_mode }
  | Remote_failed of { summary : string; mode : remote_mode }
      (** Failure is terminal for the remote attempt; local authority stays
          denied. *)

type binding_effect = {
  binding_id : string;
  principal_id : string;
  host : string;
  app_id : int;
  github_user_id : int64;
  vault_id : string option;
  prior_generation : int option;
  new_generation : int option;
  prior_lineage_id : string;
  new_lineage_id : string option;
      (** [None] when lineage was not broken (e.g. [Disable] or already terminal
          without a break). *)
  local_disabled : bool;
  leases_invalidated : int;
  secrets_destroyed : bool;
  vault_ref_cleared : bool;
  already_terminal : bool;
  remote : remote_outcome;
  status_after : string;
}
(** Per-binding redacted effect. No tokens, ciphertext, or client_secret. *)

type receipt = {
  id : string;
  kind : kind;
  principal_id : string option;  (** Set for principal-scoped invalidation. *)
  actor_key : string option;  (** Set for connector-split hooks. *)
  related_id : string option;  (** Optional plan / operation id. *)
  effects : binding_effect list;
  bindings_matched : int;
  pending_auth_invalidated : int;
  secrets_destroyed : int;
  leases_invalidated : int;
  lineages_broken : int;
  remote_attempted : int;
  remote_succeeded : int;
  remote_failed : int;
  created_at : string;
  notes : string list;
}
(** Redacted operator / audit receipt for one invalidation apply. *)

val receipt_to_json : receipt -> Yojson.Safe.t
val binding_effect_to_json : binding_effect -> Yojson.Safe.t
val string_of_receipt : receipt -> string

val receipt_contains_plaintext : receipt:receipt -> plaintext:string -> bool
(** Test helper. *)

(** {1 Denial} *)

type denial =
  | Binding of string
  | Vault of V.denial
  | Cas of C.denial
  | Storage of string
  | Invalid_input of string

val string_of_denial : denial -> string
val denial_exposes_token : denial:denial -> plaintext:string -> bool

(** {1 Schema} *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent redacted receipts table. Also ensures binding, vault, and merge
    schemas used for pending-auth counters. *)

val get_receipt : db:Sqlite3.db -> id:string -> (receipt option, string) result

(** {1 Single-binding invalidation} *)

val invalidate_binding :
  db:Sqlite3.db ->
  keys:V.key_provider ->
  kind:kind ->
  ?remote_mode:remote_mode ->
  ?http_delete:http_delete ->
  ?resolve_client:resolve_client ->
  ?client_id_handle:string ->
  ?related_id:string ->
  ?now:float ->
  ?snapshot:bool ->
  binding_id:string ->
  unit ->
  (receipt, denial) result
(** Apply the canonical lifecycle to one binding.

    Order (hard):
    - Optional immutable binding snapshot
    - Local CAS deactivate + status + lease discard ({b before network})
    - Logical lineage break for destructive kinds ({b before network})
    - Optional remote token/grant revoke under {!with_revocation_token}
    - Always destroy sealed vault secrets (destructive kinds)
    - Clear vault_ref when unlinking/revoking/removing

    Remote failure never re-enables the vault or binding. [Disable] skips remote
    revoke and secret destruction. *)

(** {1 Principal / multi-binding} *)

val invalidate_for_principal :
  db:Sqlite3.db ->
  keys:V.key_provider ->
  kind:kind ->
  ?remote_mode:remote_mode ->
  ?http_delete:http_delete ->
  ?resolve_client:resolve_client ->
  ?client_id_handle:string ->
  ?related_id:string ->
  ?now:float ->
  principal_id:P.principal_id ->
  unit ->
  (receipt, denial) result
(** Invalidate every binding owned by [principal_id]. Defaults [kind] usage to
    [Principal_removal] when called for principal disable/removal. Zeroes
    pending-authorization counters so delayed confirmations fail closed. *)

(** {1 Connector unlink / split integration} *)

val invalidate_for_connector_split :
  db:Sqlite3.db ->
  ?keys:V.key_provider ->
  ?remote_mode:remote_mode ->
  ?http_delete:http_delete ->
  ?resolve_client:resolve_client ->
  ?client_id_handle:string ->
  ?related_id:string ->
  ?now:float ->
  source_principal_id:P.principal_id ->
  actor_key:string ->
  ?binding_ids:string list ->
  unit ->
  (receipt, denial) result
(** Share the canonical lifecycle with Principal/Connector unlink-split.

    - Always invalidates pending-authorization counters on the source Principal
      (lineage-scoped delayed work fails rather than following a relink).
    - When [binding_ids] is supplied (explicit ownership intent that must drop
      authority), [keys] is required and each listed binding is fully
      invalidated as [Connector_split].
    - When [binding_ids] is omitted/empty, no vault destruction runs —
      credentials retained on the source Principal stay until an explicit
      account unlink; [keys] may be omitted. *)

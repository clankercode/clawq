(** Refresh expiring GitHub App user tokens from server-returned lifetimes
    (P21.M3.E1.T001).

    Lease acquisition refreshes only inside a documented skew window: when the
    vault access-token [expires_at] is at or before [now + skew], a remote
    refresh is attempted. Outside that window the existing sealed material is
    reused and no remote call is made.

    Remote refresh:
    - POSTs [grant_type=refresh_token] to the GitHub token endpoint
    - Uses the injectable client-secret boundary (opaque handle → plaintext
      client_id + client_secret only for the HTTP body; never logged or
      returned)
    - Requires server [expires_in] and [refresh_token_expires_in] — never
      assumes 8h / 6mo lifetimes
    - Validates [token_type] is bearer and [scope] is empty (GitHub App user
      tokens are permission-bound, not classic OAuth scopes)
    - Records both access and refresh ISO-8601 expiries durably
    - CAS-replaces vault material via {!Github_user_token_cas}, advancing token
      [generation] inside the same logical binding lineage (Principal, account,
      actor, and [lineage_id] unchanged)

    Eligible pinned jobs revalidate by re-issuing a lease on the new generation
    without switching identity. Single-flight remote refresh coordination is
    P21.M3.E1.T002.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

module V = Github_user_token_vault
module L = Github_user_token_lease
module C = Github_user_token_cas
module B = Github_account_binding

(** {1 Skew window}

    Refresh is triggered only when the access token is inside this window of
    expiry (inclusive of already-expired access tokens still refreshable). *)

val default_refresh_skew_seconds : float
(** Default skew: 300 seconds (5 minutes). Documented safety margin before
    access-token expiry so HTTP dispatch does not race the token lifetime. *)

val needs_refresh :
  ?now:float -> ?skew_seconds:float -> access_expires_at:string -> unit -> bool
(** [true] when [access_expires_at] parses as ISO-8601 UTC and
    [now + skew_seconds >= access_expires_at]. Malformed expiry is treated as
    needing refresh (fail toward revalidation). *)

(** {1 Injectable boundaries} *)

type http_post =
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, string) result
(** Injectable HTTP POST. Returns [(status_code, body)] or transport error. *)

type resolve_client =
  client_id_handle:string -> (string * string, string) result
(** Resolve opaque client-id handle → [(client_id, client_secret)]. The secret
    exists only for the duration of the token POST body construction. Never log
    or Room-export the secret. *)

val token_endpoint : ?host:string -> unit -> string
(** [POST] token URL for [host] (default [github.com]). *)

(** {1 Parsed refresh response (ephemeral)} *)

type refresh_response = {
  access_token : string;
  refresh_token : string;
      (** New refresh token (GitHub rotates on use). Required for expiring
          tokens. *)
  expires_in : int;
      (** Server-returned seconds until access token expiry. Required; never
          assumed. *)
  refresh_token_expires_in : int;
      (** Server-returned seconds until refresh token expiry. Required; never
          assumed. *)
  token_type : string;  (** Must be [bearer] (case-insensitive). *)
  scope : string;  (** Must be empty for GitHub App user tokens. *)
}
(** Ephemeral parsed body. Must not be logged or exported. *)

val parse_refresh_response : body:string -> (refresh_response, string) result
(** Parse JSON (preferred) or form-urlencoded refresh body. Fail closed on:
    - missing/empty [access_token] or [refresh_token]
    - missing/non-positive [expires_in] or [refresh_token_expires_in]
    - [token_type] not bearer
    - non-empty [scope]
    - OAuth error objects *)

(** {1 Recorded lifetimes} *)

type lifetimes = {
  access_expires_at : string;  (** ISO-8601 UTC from server [expires_in]. *)
  refresh_expires_at : string;
      (** ISO-8601 UTC from server [refresh_token_expires_in]. *)
}
(** Server-derived expiries only — never hard-coded 8h / 6mo. *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent table for durable access/refresh expiry records. *)

val get_recorded_lifetimes :
  db:Sqlite3.db -> vault_id:string -> (lifetimes option, string) result
(** Load last recorded server lifetimes for [vault_id], if any. *)

(** {1 Outcomes / denials} *)

type outcome = {
  record : V.vault_record;
      (** Post-refresh vault metadata (generation advanced; active). *)
  leases_invalidated : int;
      (** Process-local leases discarded for the prior generation. *)
  lifetimes : lifetimes;  (** Server-returned access + refresh expiries. *)
  lineage_id : string option;
      (** Logical binding lineage when a binding was validated; unchanged by
          refresh. *)
  binding : B.binding option;
      (** Binding snapshot when [binding_id] was provided (identity unchanged).
      *)
  refreshed : bool;
      (** [true] when a remote refresh was performed; [false] when the access
          token was outside the skew window and material was reused. *)
}
(** Success. Never embeds access/refresh plaintext. *)

type denial =
  | Not_in_skew
      (** Explicit refresh requested but access token is outside the skew window
          (and still valid). *)
  | Refresh_token_missing
  | Refresh_token_expired
      (** Recorded refresh expiry is at or before [now], or server rejected the
          refresh as expired. *)
  | Vault_not_active
  | Account_mismatch of { expected : V.account_key; found : V.account_key }
  | Lineage_mismatch of { expected : string; actual : string }
      (** Binding lineage no longer matches the pin (unlink/relink/split). *)
  | Binding of string  (** Binding load/authorization failure (no secrets). *)
  | Client_resolve of string  (** Client id/secret resolution failed. *)
  | Transport of string
  | Http_denial of int
  | Malformed_response of string
  | Invalid_token_type of string
  | Nonempty_scope of string
  | Vault of V.denial
  | Cas of C.denial
  | Lease of L.denial
  | Invalid_input of string
  | Storage of string

val string_of_denial : denial -> string
(** Redacted denial; never includes tokens, client_secret, or key material. *)

val denial_exposes_token : denial:denial -> plaintext:string -> bool
(** Test helper. *)

val denial_exposes_secret : denial:denial -> secret:string -> bool
(** Test helper for client_secret redaction. *)

(** {1 Refresh (remote) and lease acquisition} *)

val refresh :
  db:Sqlite3.db ->
  keys:V.key_provider ->
  http_post:http_post ->
  resolve_client:resolve_client ->
  client_id_handle:string ->
  ?now:float ->
  ?skew_seconds:float ->
  ?force:bool ->
  ?binding_id:string ->
  ?expected_lineage_id:string ->
  ?expected:V.account_key ->
  vault_id:string ->
  unit ->
  (outcome, denial) result
(** Perform a remote token refresh when the access token is inside the skew
    window (or [force=true]). Steps:

    1. Load vault metadata; optional account / binding / lineage checks 2.
    Refuse outside skew unless [force] 3. Open sealed material; require refresh
    token; check recorded refresh expiry 4. Resolve client_id + client_secret
    via [resolve_client] 5. POST refresh grant; validate bearer + empty scope +
    server lifetimes 6. CAS replace tokens with new access [expires_at]; advance
    generation 7. Record access + refresh expiries; invalidate prior-generation
    leases

    Principal, account identity, and binding [lineage_id] are unchanged. *)

val acquire_lease :
  db:Sqlite3.db ->
  keys:V.key_provider ->
  ?http_post:http_post ->
  ?resolve_client:resolve_client ->
  ?client_id_handle:string ->
  ?now:float ->
  ?skew_seconds:float ->
  ?ttl_seconds:float ->
  ?binding_id:string ->
  ?expected_lineage_id:string ->
  ?expected:V.account_key ->
  vault_id:string ->
  unit ->
  (L.lease * outcome, denial) result
(** Lease acquisition for GitHub HTTP dispatch:

    - When access expiry is outside the skew window, issue a lease from current
      vault metadata without a remote call ([refreshed=false]).
    - When inside the skew window, require [http_post], [resolve_client], and
      [client_id_handle], run {!refresh}, then issue a lease on the new
      generation so pinned jobs revalidate under the same lineage.

    Returns the live lease plus redacted [outcome]. *)

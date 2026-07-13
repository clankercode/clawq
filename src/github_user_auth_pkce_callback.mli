(** Verify GitHub user OAuth callback and exchange the code exactly once
    (P21.M2.E2.T002).

    Before any network exchange the module verifies, fail-closed:
    - constant-time OAuth [state] equality against the Principal transaction
    - open / unused transaction status
    - expiry
    - exact redirect_uri binding against protected PKCE material
    - S256 code_verifier integrity (challenge recompute)

    On success the code is exchanged exactly once via injectable HTTP, tokens
    are sealed in the Principal-owned vault, and a [Pending] account binding
    (with opaque vault ref only) is created. Failures never leave an active
    ([Authorized]) binding; partial vault/binding failures destroy sealed
    material before returning.

    One-shot: the Principal authorization transaction is claimed ([Completed])
    under a SQLite write lock before the remote exchange so replay, duplicate
    callbacks, and concurrent completion cannot mint a second binding. Terminal
    statuses never reopen.

    Mismatch, replay, duplicate callback, OAuth denial, timeout, malformed token
    response, or partial exchange → no active binding.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

module Tx = Github_user_auth_tx
module Pkce = Github_user_auth_pkce
module V = Github_user_token_vault
module B = Github_account_binding
module S = Github_user_token_store

val token_endpoint : ?host:string -> unit -> string
(** [POST] token URL for [host] (default [github.com]). *)

(** {1 Callback request} *)

type callback_request = {
  code : string option;
      (** Authorization code from the callback query. Absent on OAuth denial. *)
  state : string;  (** Presented OAuth state (constant-time compared). *)
  redirect_uri : string;
      (** Exact redirect URI used for the authorize request / callback. Must
          match the protected PKCE material. *)
  error : string option;
      (** OAuth error code when the user denies or GitHub aborts (e.g.
          [access_denied]). *)
  error_description : string option;
}
(** Browser callback payload. Prefer constructing via [make_callback_request].
*)

val make_callback_request :
  ?code:string ->
  state:string ->
  redirect_uri:string ->
  ?error:string ->
  ?error_description:string ->
  unit ->
  (callback_request, string) result
(** Build a callback request. Requires non-empty [state] and [redirect_uri].
    Exactly one of [code] or [error] should be present; empty code with no error
    is refused. *)

(** {1 Injectable boundaries} *)

type http_post =
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, string) result
(** Injectable HTTP POST. Returns [(status_code, body)] or transport error
    (timeout, DNS, reset, etc.). *)

type resolve_client =
  client_id_handle:string -> (string * string, string) result
(** Resolve opaque client-id handle → [(client_id, client_secret)]. Never log or
    Room-export the secret. *)

type github_user = {
  id : int64;  (** Numeric GitHub user id (account identity). *)
  login : string;
  avatar_url : string option;
}
(** Identity returned by the post-exchange user probe (injectable). Full shared
    /user activation lives in later tasks; this only supplies the immutable
    numeric id required to seal vault + binding rows. *)

type fetch_user = access_token:string -> (github_user, string) result
(** Injectable user probe after a successful token exchange. Tests inject
    offline fakes; production wires authenticated [GET /user]. *)

(** {1 Token response (ephemeral)} *)

type token_response = {
  access_token : string;
  refresh_token : string option;
  scopes : string list;
  expires_in : int;  (** Seconds until access token expiry (required). *)
  token_type : string option;
}
(** Parsed GitHub OAuth token response. Must not be logged or Room-exported. *)

val parse_token_response : body:string -> (token_response, string) result
(** Parse JSON (preferred) or form-urlencoded token body. Fail closed on missing
    [access_token], missing/invalid [expires_in], or OAuth error objects. *)

(** {1 Exchange result / typed failure} *)

type exchange_result = {
  tx : Tx.t;  (** Terminal [Completed] authorization transaction. *)
  material : Pkce.protected_material;
  vault : V.vault_record;  (** Sealed Principal-owned token record. *)
  binding : B.binding;
      (** [Pending] binding with opaque [vault_ref]; not [Authorized]. *)
  token_scopes : string list;
  github_user : github_user;
}
(** Full success only. Tokens never appear as plaintext fields. *)

type failure_kind =
  | State_mismatch
      (** Presented state does not match any open Principal transaction
          (constant-time path still applied when a candidate exists). *)
  | Replay  (** Transaction already terminal / completed; second use refused. *)
  | Duplicate_callback  (** Concurrent or second claim lost the open CAS. *)
  | Expired
  | Redirect_mismatch
  | Unused_status
      (** Transaction is not open (cancelled, superseded, rejected, …). *)
  | Verifier_invalid
      (** Missing protected material, unresolvable verifier, or S256 challenge
          mismatch. *)
  | Denial  (** OAuth [error] (e.g. access_denied). *)
  | Timeout  (** HTTP transport timeout / connectivity failure. *)
  | Malformed_response
  | Partial_exchange
      (** Remote exchange or user probe succeeded but local seal/bind failed
          (and was rolled back). *)
  | Http_denial of int  (** Non-2xx token endpoint status. *)
  | Invalid of string
  | Storage of string

type exchange_error = {
  kind : failure_kind;
  message : string;  (** Actionable, secret-free operator/user message. *)
  tx : Tx.t option;
      (** Related transaction when known (may already be terminal). *)
}
(** Fail-closed error. Never embeds code_verifier, access_token, or client
    secret. *)

val string_of_failure_kind : failure_kind -> string

val has_active_binding : binding:B.binding -> bool
(** [true] only when [authorization_status = Authorized]. [Pending] bindings
    created on success are not active for user-attributed work. *)

val redacted_summary : exchange_result -> string
(** Operator summary without tokens, verifier, or client secret. *)

(** {1 Exchange} *)

val exchange :
  db:Sqlite3.db ->
  store:Pkce.secret_backend ->
  keys:V.key_provider ->
  ?http_post:http_post ->
  ?resolve_client:resolve_client ->
  ?fetch_user:fetch_user ->
  ?now:float ->
  ?binding_id:string ->
  ?vault_id:string ->
  callback:callback_request ->
  unit ->
  (exchange_result, exchange_error) result
(** Validate callback → claim one-shot transaction → exchange code → seal vault
    \+ Pending binding.

    Requires [http_post], [resolve_client], and [fetch_user] (inject fakes in
    tests). Default implementations refuse closed when omitted.

    Order of operations: 1. Load tx by presented state; constant-time state
    compare 2. Require [Web_pkce] + [Open] + unexpired 3. Exact redirect match
    against protected material 4. Resolve and verify S256 code_verifier against
    stored challenge 5. Under [BEGIN IMMEDIATE], CAS-claim the open tx as
    [Completed] 6. POST token exchange with code + verifier + exact redirect +
    client 7. Parse token response; probe numeric user; seal vault; insert
    Pending binding with vault_ref 8. On any post-claim failure: destroy any
    partial vault row; leave tx terminal; return error with no active binding

    OAuth denial ([error] set) cancels the open transaction and returns [Denial]
    without contacting GitHub. *)

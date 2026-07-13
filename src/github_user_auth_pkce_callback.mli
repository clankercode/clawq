(** Verify GitHub user OAuth callback, exchange the code exactly once, and route
    success through shared verified activation (P21.M2.E2.T002 + T003).

    Before any network exchange the module verifies, fail-closed:
    - constant-time OAuth [state] equality against the Principal transaction
    - open / unused transaction status
    - expiry
    - exact redirect_uri binding against protected PKCE material
    - S256 code_verifier integrity (challenge recompute)

    On success the code is exchanged exactly once via injectable HTTP; the
    still- pending credential and transaction context are handed to the
    flow-neutral [Github_user_auth_activate.prepare] path (/user verification,
    seal, revision- bound redacted plan, private confirmation token). No
    web-only binding semantics: [Authorized] only after shared private
    confirmation.

    Failures never leave an active ([Authorized]) binding. Activation failures
    destroy pending material and return a private repair state. Partial vault/
    binding failures destroy sealed material before returning.

    One-shot: the Principal authorization transaction is claimed ([Completed])
    under a SQLite write lock before the remote exchange so replay, duplicate
    callbacks, and concurrent completion cannot mint a second binding. Terminal
    statuses never reopen.

    Mismatch, replay, duplicate callback, OAuth denial, timeout, malformed token
    response, or partial/activation failure → no active binding + private
    repair.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

module Tx = Github_user_auth_tx
module Pkce = Github_user_auth_pkce
module Activate = Github_user_auth_activate
module V = Github_user_token_vault
module B = Github_account_binding

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

type github_user = Activate.github_user
(** Numeric GitHub identity from the shared activation [/user] probe. *)

type fetch_user = Activate.fetch_user
(** Injectable user probe after a successful token exchange, forwarded into
    [Activate.prepare]. Tests inject offline fakes; production wires
    authenticated [GET /user]. *)

(** {1 Token response (ephemeral)} *)

type token_response = {
  access_token : string;
  refresh_token : string option;
  scopes : string list;
  expires_in : int;  (** Seconds until access token expiry (required). *)
  token_type : string option;
}
(** Parsed GitHub OAuth token response. Must not be logged or Room-exported.
    Projected into [Activate.pending_credential] before shared prepare. *)

val parse_token_response : body:string -> (token_response, string) result
(** Parse JSON (preferred) or form-urlencoded token body. Fail closed on missing
    [access_token], missing/invalid [expires_in], or OAuth error objects. *)

(** {1 Exchange result / typed failure} *)

type exchange_result = {
  tx : Tx.t;  (** Terminal [Completed] authorization transaction. *)
  material : Pkce.protected_material;
  prepared : Activate.prepared;
      (** Shared activation: pending vault/binding, redacted plan, and one-time
          private confirmation token. Binding is [Pending], not [Authorized]. *)
  token_scopes : string list;
}
(** Full success only. Tokens never appear as plaintext fields. Confirmation
    plaintext is returned once on [prepared.confirmation_token] for private
    delivery only. *)

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
      (** Remote exchange succeeded but local seal/bind/activation failed (and
          was rolled back / pending material destroyed). *)
  | Activation of string
      (** Shared activation refused (collision, identity/Principal mismatch,
          user probe, …). Pending material destroyed; prior Authorized state
          preserved. Payload is [Activate.string_of_failure_kind]. *)
  | Http_denial of int  (** Non-2xx token endpoint status. *)
  | Invalid of string
  | Storage of string

type exchange_error = {
  kind : failure_kind;
  message : string;  (** Actionable, secret-free operator/user message. *)
  repair : string;
      (** Private repair guidance (never Room-export secrets). Empty when no
          further private action is available. *)
  tx : Tx.t option;
      (** Related transaction when known (may already be terminal). *)
  activation : Activate.activation option;
      (** Related activation when prepare partially created one (usually
          terminal Destroyed/Rejected). *)
}
(** Fail-closed error. Never embeds code_verifier, access_token, or client
    secret. *)

val string_of_failure_kind : failure_kind -> string

val has_active_binding : binding:B.binding -> bool
(** [true] only when [authorization_status = Authorized]. [Pending] bindings
    created on success are not active for user-attributed work. *)

val redacted_summary : exchange_result -> string
(** Operator summary without tokens, verifier, client secret, or confirmation
    plaintext. *)

val private_repair_summary : exchange_error -> string
(** Secret-free private repair state for the Principal channel (not Room-
    exportable as progress). *)

(** {1 Exchange} *)

val exchange :
  db:Sqlite3.db ->
  store:Pkce.secret_backend ->
  keys:V.key_provider ->
  ?http_post:http_post ->
  ?resolve_client:resolve_client ->
  ?fetch_user:fetch_user ->
  ?now:float ->
  ?ttl_seconds:float ->
  ?activation_id:string ->
  ?binding_id:string ->
  ?vault_id:string ->
  ?plan_id:string ->
  callback:callback_request ->
  unit ->
  (exchange_result, exchange_error) result
(** Validate callback → claim one-shot transaction → exchange code → project
    pending credential → [Activate.prepare] (shared /user, seal, plan,
    confirmation).

    Requires [http_post], [resolve_client], and [fetch_user] (inject fakes in
    tests). Default implementations refuse closed when omitted.

    Order of operations: 1. Load tx by presented state; constant-time state
    compare 2. Require [Web_pkce] + [Open] + unexpired 3. Exact redirect match
    against protected material 4. Resolve and verify S256 code_verifier against
    stored challenge 5. Under [BEGIN IMMEDIATE], CAS-claim the open tx as
    [Completed] 6. POST token exchange with code + verifier + exact redirect +
    client 7. Parse token response into still-pending credential 8. Call shared
    [Activate.prepare] with auth_tx + credential (probe /user, seal vault,
    Pending binding, redacted plan, confirmation token) 9. On any post-claim
    failure: destroy any partial vault/activation material; leave tx terminal;
    return error with no active binding and a private repair state

    OAuth denial ([error] set) cancels the open transaction and returns [Denial]
    without contacting GitHub. *)

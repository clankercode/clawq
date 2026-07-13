(** Durable leased GitHub App device-code polling and terminal handling
    (P21.M2.E3.T002 + T003).

    A single worker claims an expiring poll lease before contacting GitHub's
    token endpoint. Polls never run before the durable [next_poll_at], and the
    server-derived [expires_at] is never recomputed on restart.

    Intermediate responses:
    - [authorization_pending] keeps [interval_seconds] and advances
      [next_poll_at] by that interval
    - [slow_down] uses a server-returned interval when present, otherwise adds
      five seconds to the current interval, then advances [next_poll_at]

    Cancellation (auth tx) and local/server expiry set a durable stop reason so
    future claims refuse without further HTTP.

    Terminal handling (T003):
    - [Granted] projects still-pending credentials into the flow-neutral
      [Github_user_auth_activate.prepare] path (/user, seal, redacted plan,
      private confirmation). No web-owned module is a device-flow prerequisite.
    - Expiry, denial, disabled flow, incorrect client/code, unsupported grant,
      malformed, and unknown responses terminate explicitly: durable stop
      reason, bound auth tx closed, and no active partial ([Authorized])
      binding.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

val schema_version : int
(** Poll-lease / stop-reason schema version on the device table; starts at 1. *)

val default_lease_seconds : float
(** Default exclusive poll-lease TTL (seconds). *)

val slow_down_extra_seconds : int
(** Seconds added to the current interval when [slow_down] omits a new interval
    (GitHub / RFC 8628 default: 5). *)

val access_token_path : string
(** Path segment for the device token endpoint ([/login/oauth/access_token]). *)

val access_token_url : ?host:string -> unit -> string
(** Full token URL for [host] (default [github.com]). *)

val device_grant_type : string
(** [urn:ietf:params:oauth:grant-type:device_code]. *)

(** {1 Injectable boundaries} *)

type http_post = Github_user_auth_device.http_post
type resolve_client_id = Github_user_auth_device.resolve_client_id

(** {1 Stop / terminal reasons} *)

type stop_reason =
  | Cancelled  (** Bound auth transaction cancelled. *)
  | Expired  (** Local session or auth-tx expiry. *)
  | Access_denied  (** User denied the device request. *)
  | Device_code_expired  (** GitHub [expired_token]. *)
  | Access_granted  (** Tokens received; further polls stopped. *)
  | Unsupported_grant  (** [unsupported_grant_type]. *)
  | Incorrect_device_code
  | Terminal of string  (** Other terminal GitHub/error code. *)

val string_of_stop_reason : stop_reason -> string
val stop_reason_of_string : string -> (stop_reason, string) result

(** {1 Token endpoint parse} *)

type token_success = {
  access_token : string;
  token_type : string option;
  scope : string option;
  expires_in : int option;
  refresh_token : string option;
}
(** Ephemeral success body. Must not be logged, Room-exported, or written to
    redacted diagnostics. *)

type token_error = {
  error : string;
  error_description : string option;
  interval : int option;
      (** Optional server-returned minimum poll interval (esp. [slow_down]). *)
}

type token_response =
  | Token_success of token_success
  | Token_error of token_error

val parse_token_response : body:string -> (token_response, string) result
(** Parse form-urlencoded or JSON token body. Fail closed on empty/malformed. *)

(** {1 Durable poll state (no secrets)} *)

type poll_state = {
  session_id : string;
  interval_seconds : int;
  expires_at : string;
  next_poll_at : string;
  poll_lease_owner : string option;
  poll_lease_token : string option;
  poll_lease_expires_at : string option;
  poll_stop_reason : stop_reason option;
  updated_at : string;
}
(** Durable timing + lease metadata. No device_code or access tokens. *)

type lease = {
  session_id : string;
  worker_id : string;
  token : string;
  lease_expires_at : string;
}
(** Exclusive in-flight poll lease held by one worker. *)

(** {1 Poll outcomes} *)

type pending_timing = {
  session : Github_user_auth_device.session;
  interval_seconds : int;
  next_poll_at : string;
}
(** Post-response timing for continuing polls. *)

type granted = {
  session : Github_user_auth_device.session;
  tokens : token_success;
      (** Still-pending credential for shared activation (T003). Ephemeral. *)
}

type poll_outcome =
  | Authorization_pending of pending_timing
      (** Keep interval; next_poll_at advanced by interval. *)
  | Slow_down of pending_timing
      (** Interval updated (server value or +5s); next_poll_at advanced. *)
  | Granted of granted
      (** Access token received; durable stop set; no further polls. *)
  | Stopped of {
      session : Github_user_auth_device.session option;
      reason : stop_reason;
      message : string;
    }  (** Cancellation, expiry, denial, or other terminal; no further polls. *)
  | Not_due of {
      session : Github_user_auth_device.session;
      next_poll_at : string;
    }  (** [now] is still before durable [next_poll_at]; no HTTP. *)
  | Lease_busy of { session_id : string; owner : string option }
      (** Another worker holds a non-expired poll lease. *)

type refuse_error = Github_user_auth_device.refuse_error

(** {1 Schema} *)

val ensure_schema : Sqlite3.db -> unit
(** Ensures device + auth-tx schemas and additive poll lease / stop columns. *)

(** {1 Introspection} *)

val get_poll_state :
  db:Sqlite3.db -> session_id:string -> (poll_state option, refuse_error) result
(** Load durable timing/lease/stop fields (no ciphertext). *)

val is_stopped : poll_state -> bool
val redacted_poll_state : poll_state -> string

val redacted_outcome : poll_outcome -> string
(** Secret-free summaries for tests and diagnostics. *)

(** {1 Lease claim / release} *)

val try_claim :
  db:Sqlite3.db ->
  session_id:string ->
  worker_id:string ->
  ?lease_seconds:float ->
  ?now:float ->
  unit ->
  (lease, poll_outcome) result
(** Atomically claim the exclusive poll lease when:

    - no durable stop reason
    - [now >= next_poll_at]
    - [now < expires_at] (local server expiry; not recomputed on restart)
    - bound auth tx is [Open] and unexpired
    - no other non-expired lease is held

    On local cancel/expiry, records [poll_stop_reason] and returns
    [Error (Stopped _)]. On not-due / busy returns the corresponding [Error]
    outcome (no HTTP). [Ok lease] means this worker alone may poll. *)

val release_lease :
  db:Sqlite3.db ->
  session_id:string ->
  token:string ->
  ?now:float ->
  unit ->
  (unit, refuse_error) result
(** Clear a held lease by token. No-op (Ok) if token already lost. *)

(** {1 Poll once} *)

val poll_once :
  db:Sqlite3.db ->
  keys:Github_user_token_vault.key_provider ->
  ?http_post:http_post ->
  ?resolve_client_id:resolve_client_id ->
  session_id:string ->
  worker_id:string ->
  ?lease_seconds:float ->
  ?now:float ->
  unit ->
  (poll_outcome, refuse_error) result
(** Claim lease → open sealed [device_code] → POST token endpoint → apply timing
    / stop, release lease.

    Never polls before [next_poll_at]. At most one concurrent worker succeeds
    the claim. Restart reloads durable [expires_at] / [next_poll_at] and does
    not reset them. *)

val apply_token_response :
  db:Sqlite3.db ->
  session_id:string ->
  lease_token:string ->
  response:token_response ->
  ?now:float ->
  unit ->
  (poll_outcome, refuse_error) result
(** Apply a parsed token response under a held lease (test / internal path).
    Updates durable interval / next_poll_at / stop, then releases the lease.
    Terminal GitHub errors also close the bound auth transaction fail-closed. *)

(** {1 Terminal handling + shared activation (T003)} *)

type fetch_user = Github_user_auth_activate.fetch_user
(** Injectable GitHub [/user] probe, forwarded into [Activate.prepare]. *)

type prepared = {
  session : Github_user_auth_device.session;
  prepared : Github_user_auth_activate.prepared;
      (** Shared activation: pending vault/binding, redacted plan, one-time
          private confirmation token. Binding is [Pending], not [Authorized]. *)
  auth_tx_id : string;
}
(** Device success after shared prepare. Tokens never appear as plaintext fields
    outside sealed vault material. *)

type terminated = {
  session : Github_user_auth_device.session option;
  reason : stop_reason;
  message : string;
  repair : string;  (** Private repair guidance (never Room-export secrets). *)
  activation : Github_user_auth_activate.activation option;
      (** Related activation when prepare partially created one (usually
          terminal Destroyed/Rejected). *)
}
(** Explicit terminal failure: durable poll stop + closed auth tx + no active
    partial binding. *)

type handle_result =
  | Continuing of poll_outcome
      (** Intermediate: [Authorization_pending], [Slow_down], [Not_due], or
          [Lease_busy]. *)
  | Prepared of prepared
      (** [Granted] routed through shared [Activate.prepare]. *)
  | Terminated of terminated
      (** Expiry, denial, disabled flow, incorrect client/code, unsupported
          grant, malformed, unknown, or activation refusal. *)

val credential_of_token_success :
  token_success -> (Github_user_auth_activate.pending_credential, string) result
(** Project device-grant success into a pending credential. Requires positive
    [expires_in] (fail closed when GitHub omits lifetime). *)

val redacted_handle_result : handle_result -> string
(** Secret-free summary (no access tokens, confirmation plaintext, or device
    codes). *)

val prepare_granted :
  db:Sqlite3.db ->
  keys:Github_user_token_vault.key_provider ->
  ?fetch_user:fetch_user ->
  granted:granted ->
  ?now:float ->
  ?ttl_seconds:float ->
  ?activation_id:string ->
  ?vault_id:string ->
  ?binding_id:string ->
  ?plan_id:string ->
  unit ->
  (prepared, terminated) result
(** Route a [Granted] poll result into shared [Activate.prepare] with the still-
    pending credential and bound authorization transaction context.

    On activation failure: pending material destroyed by Activate, auth tx
    cancelled, poll remains stopped, returns [Terminated] with private repair
    and no [Authorized] binding introduced. *)

val finalize_terminal :
  db:Sqlite3.db ->
  outcome:poll_outcome ->
  ?now:float ->
  unit ->
  (handle_result, refuse_error) result
(** For intermediate outcomes returns [Continuing]. For [Stopped] / already-
    terminal reasons, ensures the bound auth transaction is closed and returns
    [Terminated] with repair text. [Granted] alone is not enough — call
    [prepare_granted] or [handle_outcome]. *)

val handle_outcome :
  db:Sqlite3.db ->
  keys:Github_user_token_vault.key_provider ->
  ?fetch_user:fetch_user ->
  outcome:poll_outcome ->
  ?now:float ->
  ?ttl_seconds:float ->
  ?activation_id:string ->
  ?vault_id:string ->
  ?binding_id:string ->
  ?plan_id:string ->
  unit ->
  (handle_result, refuse_error) result
(** Full T003 terminal handler:

    - [Granted] → [prepare_granted] (shared /user + seal + plan + confirmation)
    - [Stopped] → auth tx closed, no partial binding, private repair
    - intermediate → [Continuing]

    Never leaves an [Authorized] binding from device terminal handling. *)

val poll_and_prepare :
  db:Sqlite3.db ->
  keys:Github_user_token_vault.key_provider ->
  ?http_post:http_post ->
  ?resolve_client_id:resolve_client_id ->
  ?fetch_user:fetch_user ->
  session_id:string ->
  worker_id:string ->
  ?lease_seconds:float ->
  ?now:float ->
  ?ttl_seconds:float ->
  ?activation_id:string ->
  ?vault_id:string ->
  ?binding_id:string ->
  ?plan_id:string ->
  unit ->
  (handle_result, refuse_error) result
(** [poll_once] then [handle_outcome]. Production device-flow entrypoint for
    terminal routing into shared activation. *)

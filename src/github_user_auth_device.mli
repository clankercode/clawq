(** Start GitHub App device authorization with private code delivery
    (P21.M2.E3.T001).

    When device flow is disabled, start refuses without contacting GitHub and
    without writing secret state. When enabled, the module:

    - requests a device code from GitHub ([POST /login/device/code])
    - creates a Principal-bound [Github_user_auth_tx] of [Device] kind
    - persists the server [device_code] as authenticated ciphertext together
      with server expiry, poll interval, App/client, intended account, and
      [next_poll_at]
    - delivers [user_code] and [verification_uri] only through the private
      continuation ([Github_user_auth_delivery]); shared Rooms see neutral
      progress only

    The pollable [device_code] is never Room-exported. Production HTTP and
    client-id resolution are injectable so unit tests stay offline.

    Durable leased polling lives in [Github_user_auth_device_poll] (T002).
    Terminal exchange and verified binding activation live in T003.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

val schema_version : int
(** Device-session row / sealed-payload schema version; starts at 1. *)

val default_host : string
(** V1 live host: [github.com]. *)

val device_code_path : string
(** Path segment for the device-code endpoint ([/login/device/code]). *)

val device_code_url : ?host:string -> unit -> string
(** Full device-code URL for [host] (default [github.com]). *)

(** {1 Injectable boundaries} *)

type http_post =
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, string) result
(** Injectable HTTP POST. Returns [(status_code, body)] or transport error. *)

type resolve_client_id = handle:string -> (string, string) result
(** Resolve an opaque client-id handle to the plaintext OAuth client id used in
    the device-code request. Never log or Room-export the plaintext. *)

(** {1 Server device-code response (ephemeral)} *)

type device_code_response = {
  device_code : string;
      (** Poll secret (40 chars). Persist only as ciphertext. *)
  user_code : string;
      (** User-facing code (e.g. [WDJB-MJHT]). Private delivery only. *)
  verification_uri : string;
      (** Browser URL for entering [user_code] (e.g. github.com/login/device).
      *)
  verification_uri_complete : string option;
      (** Optional pre-filled verification URL when GitHub returns one. *)
  expires_in : int;  (** Seconds until device/user codes expire (server). *)
  interval : int;
      (** Minimum seconds between token polls (server; default 5). *)
}
(** Parsed GitHub device-code response. Ephemeral; must not be serialized to
    Room-visible surfaces. *)

val parse_device_code_response :
  body:string -> (device_code_response, string) result
(** Parse form-urlencoded or JSON device-code body. Fail closed on missing or
    invalid fields. *)

(** {1 Durable encrypted device session (no plaintext codes)} *)

type session = {
  version : int;
  id : string;  (** Opaque device-session id. *)
  tx_id : string;  (** Bound [Github_user_auth_tx] id. *)
  principal_id : string;
  app : Github_user_auth_tx.app_client;
  intended_account : Github_user_auth_tx.intended_account;
  key_id : Github_user_token_master_key.key_id;
      (** Master-key id under which [device_code] ciphertext is sealed. *)
  key_version : Github_user_token_master_key.key_version;
  interval_seconds : int;  (** Server poll interval. *)
  expires_at : string;  (** ISO-8601 UTC from server [expires_in]. *)
  next_poll_at : string;
      (** ISO-8601 UTC earliest next poll (created_at + interval initially). *)
  created_at : string;
  updated_at : string;
}
(** Durable metadata. Ciphertext and plaintext codes are never fields here. *)

type opened_secrets = {
  device_code : string;
  user_code : string;
  verification_uri : string;
  verification_uri_complete : string option;
}
(** In-process open of sealed material. Must not be logged, Room-exported, or
    written to JSON diagnostics. *)

(** {1 Start result and typed refusals} *)

type refuse_reason =
  | Device_flow_disabled
      (** Feature flag / App setting refuses device authorization. *)
  | Master_key_not_ready of Github_user_token_master_key.not_ready_reason list
  | No_private_channel
  | Invalid_input of string
  | Http of string
  | Storage of string
  | Crypto_failure
  | Delivery of Github_user_auth_delivery.refuse_reason

type refuse_error = {
  reason : refuse_reason;
  message : string;  (** Actionable, secret-free operator/user message. *)
  room_safe_progress : Github_user_auth_delivery.progress_content option;
}
(** Safe refusal: typed reason, no secret leakage. *)

val string_of_refuse_reason : refuse_reason -> string

type start_result = {
  session : session;
  tx : Github_user_auth_tx.t;
  delivery_plan : Github_user_auth_delivery.delivery_plan;
      (** Always a successful [Private] plan on Ok; companion Room progress is
          neutral. *)
}
(** Successful start. [user_code] / [verification_uri] appear only inside
    [delivery_plan]'s private body — never on Room-export helpers. *)

(** {1 Schema} *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent table [github_user_auth_device]. Also ensures
    [Github_user_auth_tx] schema. *)

(** {1 Start} *)

val start :
  db:Sqlite3.db ->
  ?http_post:http_post ->
  ?resolve_client_id:resolve_client_id ->
  keys:Github_user_token_vault.key_provider ->
  device_flow_enabled:bool ->
  principal_id:string ->
  connector_actor:Principal_identity.connector_actor_key ->
  source:Github_user_auth_tx.source ->
  app:Github_user_auth_tx.app_client ->
  ?intended_account:Github_user_auth_tx.intended_account ->
  base_revision:string ->
  continuation_handle:string ->
  channel:Github_user_auth_delivery.delivery_channel ->
  ?shared_room_id:string ->
  ?now:float ->
  ?id:string ->
  ?tx_id:string ->
  unit ->
  (start_result, refuse_error) result
(** Start device authorization.

    Refuses immediately when [device_flow_enabled] is false (no HTTP, no durable
    secret write). Requires a private [channel], Ready master-key material,
    resolvable client id, and a successful GitHub device-code response. Persists
    encrypted [device_code] with server timing fields, creates the bound auth
    transaction, and routes private delivery of user code + verification URI
    only. *)

(** {1 Load / open (for later polling; secret-aware)} *)

val get : db:Sqlite3.db -> id:string -> (session option, refuse_error) result
(** Metadata only (no decrypt). *)

val get_by_tx :
  db:Sqlite3.db -> tx_id:string -> (session option, refuse_error) result

val open_secrets :
  db:Sqlite3.db ->
  keys:Github_user_token_vault.key_provider ->
  id:string ->
  unit ->
  (session * opened_secrets, refuse_error) result
(** Load and decrypt sealed codes. Fail closed on missing key / corrupt
    envelope. Never returns partial secrets. *)

(** {1 Introspection (no secrets)} *)

val session_to_json : session -> Yojson.Safe.t
(** Metadata JSON only — never ciphertext or plaintext codes. *)

val redacted_summary : session -> string
(** Human summary without codes or ciphertext. *)

val start_result_redacted_summary : start_result -> string
(** Export-safe start summary (delegates to delivery plan redaction). *)

val row_contains_plaintext :
  db:Sqlite3.db -> id:string -> plaintext:string -> (bool, refuse_error) result
(** Test helper: true if any stored text column contains [plaintext]. *)

val json_contains_plaintext : json:Yojson.Safe.t -> plaintext:string -> bool
(** Test helper. *)

(** Verify and exchange one-time GitHub App manifest browser callbacks
    (P19.M2.E1.T002).

    Validates state, expiry, origin transaction, trusted callback path,
    bind/principal context, and non-reuse before conversion. Exchanges the
    temporary [code] via [POST https://api.github.com/app-manifests/{code}/
    conversions], then requires authenticated verification of the returned App
    installation before storing credential handles. The verified installation
    scope, exchange receipt, and consumed marker are committed together.
    Failures leave no active partial GitHub App config and keep the setup
    transaction recoverable.

    Production HTTP, Secret_store wiring, and daemon route registration are
    intentionally injectable / out of pure-module scope; unit tests inject fakes
    and never require live network.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type exchange_request = {
  code : string;
      (** Temporary one-time code from the browser callback query. *)
  state : string;  (** CSRF / correlation token matching an open transaction. *)
  callback_path : string option;
      (** Path portion or full callback URL as received; validated against the
          transaction's public_base_url + default_callback_path when present. *)
  expected_bind : Github_app_setup_tx.bind_target option;
      (** When set, must match the transaction bind (forged/swapped Room fails).
      *)
  expected_principal_id : string option;
      (** When set, must match the transaction principal. *)
  installation_id : int option;
      (** Installation id from the callback query. Required for exchange and
          verified against the returned App through [verify_installation]. *)
  setup_action : string option;  (** Optional setup_action (informational). *)
}

type app_credentials = {
  app_id : int;
  slug : string option;
  client_id_handle : string;  (** Credential-store handle (not plaintext). *)
  client_secret_handle : string;
  private_key_handle : string;  (** PEM stored as a handle. *)
  webhook_secret_handle : string;
  html_url : string option;
  owner : string option;
}

type exchange_result = {
  transaction : Github_app_setup_tx.t;  (** Status [Consumed]. *)
  app : app_credentials;
  installation_id : int option;
  verified_installation : Github_app_installation_scope.t;
      (** Current active installation scope returned by authenticated GitHub
          verification and persisted atomically with this receipt. *)
  raw_app_id : int;
  receipt_id : string;
      (** Durable exchange receipt row id (handles + public metadata only). *)
}

type http_post =
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, string) result
(** Injectable HTTP POST. Returns [(status_code, body)] or transport error. *)

type store_secret = name:string -> plaintext:string -> (string, string) result
(** Injectable credential boundary. Returns an opaque handle; never write
    plaintext secrets into channel config or transaction rows. *)

type verify_installation =
  app_id:int ->
  private_key_pem:string ->
  installation_id:int ->
  (Github_app_installation_scope.t, string) result
(** Authenticate as the just-converted App and fetch its live installation
    record. The result must carry the requested installation id, matching App
    id, and [Active] status. It is deliberately injected so the route can use
    the production authenticated HTTP client while unit tests remain offline. *)

val conversion_url : code:string -> string
(** Canonical conversion endpoint for [code]. *)

val expected_callback_url : public_base_url:string -> string
(** [public_base_url] joined with [Github_app_setup_tx.default_callback_path].
*)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent SQLite schema for exchange receipts (handles + metadata only). *)

val exchange :
  db:Sqlite3.db ->
  ?http_post:http_post ->
  ?verify_installation:verify_installation ->
  ?store_secret:store_secret ->
  ?now:float ->
  exchange_request ->
  (exchange_result, string) result
(** Full validate → convert → authenticated installation verification → store
    handles → atomically persist scope/receipt/consumed pipeline.

    Requires trusted [expected_bind], [expected_principal_id], and
    [callback_path] in the request, plus [installation_id] and
    [verify_installation]; omitted context fails closed. Requires [http_post]
    (or fails with a clear message). Default [store_secret] encrypts via
    [Secret_store] when [CLAWQ_MASTER_KEY] is set; tests should inject an
    in-memory store. Does not mutate Runtime_config directly. *)

val get_receipt :
  db:Sqlite3.db -> id:string -> (app_credentials option, string) result
(** Load a stored exchange receipt by id (handles only, no plaintext). *)

val find_receipt_by_tx :
  db:Sqlite3.db ->
  tx_id:string ->
  ((string * app_credentials) option, string) result
(** Lookup receipt by originating transaction id. Returns [(receipt_id, app)].
*)

(** Start state-bound S256 PKCE GitHub user authorization (P21.M2.E2.T001).

    Generates independent high-entropy OAuth [state] and PKCE [code_verifier],
    stores the verifier as a protected secret-store handle bound to the
    Principal authorization transaction (never room-exportable), builds the
    authorize URL with the exact registered [redirect_uri] and S256
    [code_challenge] only, and prepares private delivery of the URL.

    Rejects:
    - plain / none PKCE methods (S256 only)
    - unregistered or non-exact redirect URIs
    - reusable / colliding one-time state
    - secret-bearing Room output (URLs, verifiers, challenges with secrets)

    Context modules: [Github_user_auth_tx] (correlation + one-time state),
    [Github_user_auth_delivery] (private-only for authorization URLs).

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

val schema_version : int
(** Protected PKCE material schema version; starts at 1. *)

(** {1 Challenge method} *)

type challenge_method =
  | S256  (** Only S256 is supported. [plain] and [none] are rejected. *)

val string_of_challenge_method : challenge_method -> string
(** Always ["S256"]. *)

val challenge_method_of_string : string -> (challenge_method, string) result
(** Accepts only ["S256"] (case-insensitive). Rejects ["plain"], ["none"],
    empty, and unknown values. *)

(** {1 Secret backend}

    Same shape as [Github_user_token_store.secret_backend]. Production wires
    [Secret_store] / vault; tests use
    [Github_user_token_store.make_in_memory_secret_store]. *)

type secret_backend = Github_user_token_store.secret_backend

(** {1 Protected transaction material}

    Persisted beside the authorization transaction. The code_verifier never
    appears as plaintext here — only an opaque secret-store handle. *)

type protected_material = {
  version : int;
  tx_id : string;
  one_time_state : string;
      (** OAuth state correlation token (also
          [Github_user_auth_tx.one_time_state]). Not a credential; never a
          code_verifier. *)
  code_verifier_handle : string;
      (** Opaque secret-store handle. Not room-exportable. Never embed in Room
          messages, redacted summaries, or audit exports. *)
  redirect_uri : string;
      (** Exact registered OAuth callback used in the authorize request. *)
  code_challenge : string;  (** S256 BASE64URL(SHA256(verifier)). *)
  code_challenge_method : challenge_method;  (** Always [S256]. *)
  client_id_handle : string;  (** Opaque client-id handle (not plaintext). *)
  created_at : string;
}
(** Protected PKCE material bound to one authorization transaction. *)

(** {1 Cryptographic helpers} *)

val generate_state : unit -> string
(** Independent high-entropy OAuth state (hex of 32 random bytes). *)

val generate_code_verifier : unit -> string
(** Independent high-entropy PKCE code_verifier (BASE64URL of 32 random bytes;
    43 unreserved characters per RFC 7636). *)

val code_challenge_s256 : code_verifier:string -> string
(** [BASE64URL(SHA256(ASCII(code_verifier)))] (RFC 7636 S256). *)

val ensure_rng_initialized : unit -> unit
(** Idempotent RNG init for tests that call generators before [start]. *)

(** {1 Redirect validation} *)

val registered_redirect_valid : string -> bool
(** [true] when the URI is a non-empty absolute [https] URL with a path (same
    rule as readiness callback_uri). *)

val require_exact_redirect :
  registered:string -> requested:string -> (string, string) result
(** Exact match of trimmed strings against the registered callback. No
    scheme/host/path normalization beyond trim — unregistered or mutated
    redirects fail closed. *)

(** {1 Schema and protected store} *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent table for protected PKCE material (verifier handle, exact
    redirect, S256 challenge) keyed by [tx_id] / [one_time_state]. *)

val load_protected :
  db:Sqlite3.db -> tx_id:string -> (protected_material option, string) result

val load_protected_by_state :
  db:Sqlite3.db ->
  one_time_state:string ->
  (protected_material option, string) result

val get_code_verifier :
  store:secret_backend -> material:protected_material -> (string, string) result
(** Resolve the protected handle to plaintext verifier. For exchange (later
    task); never log or room-export the result. *)

val destroy_protected :
  db:Sqlite3.db -> store:secret_backend -> tx_id:string -> (unit, string) result
(** Delete the verifier from [store] and the protected PKCE row bound to
    [tx_id]. This is idempotent when the row was already removed; the DELETE
    additionally binds the persisted handle to avoid removing replacement
    material. *)

(** {1 Start authorization} *)

type start_result = {
  tx : Github_user_auth_tx.t;
  material : protected_material;
  authorization_url : string;
      (** Full authorize URL for private delivery only. *)
  private_material : Github_user_auth_delivery.private_material;
      (** Ready for [Github_user_auth_delivery] private channel. *)
  delivery_context : Github_user_auth_delivery.delivery_context;
}
(** Successful web PKCE start: open tx, protected verifier handle, private URL.
*)

val start :
  db:Sqlite3.db ->
  store:secret_backend ->
  principal_id:string ->
  connector_actor:Principal_identity.connector_actor_key ->
  source:Github_user_auth_tx.source ->
  app:Github_user_auth_tx.app_client ->
  client_id:string ->
  registered_redirect_uri:string ->
  ?requested_redirect_uri:string ->
  ?intended_account:Github_user_auth_tx.intended_account ->
  base_revision:string ->
  continuation_handle:string ->
  ?scopes:string list ->
  ?login:string ->
  ?challenge_method:string ->
  ?ttl_seconds:float ->
  ?now:float ->
  ?id:string ->
  ?one_time_state:string ->
  unit ->
  (start_result, string) result
(** Start a state-bound S256 PKCE authorization.

    - Generates independent high-entropy [state] and [code_verifier] when not
      supplied ([one_time_state] override is for tests only and must still be
      high-entropy and unused).
    - Seals the verifier into [store] and persists only the opaque handle.
    - Creates a [Web_pkce] [Github_user_auth_tx] with that state (supersedes
      prior open web_pkce for the same principal/source bind key).
    - Builds the authorize URL with the exact registered redirect and
      [code_challenge_method=S256] only.
    - Rejects plain/none challenge methods, invalid/unregistered redirects,
      empty client_id, and state reuse (existing one_time_state).

    Does not perform network I/O. Caller privately delivers [private_material]
    via [Github_user_auth_delivery]. *)

val build_authorization_url :
  host:string ->
  client_id:string ->
  redirect_uri:string ->
  state:string ->
  code_challenge:string ->
  code_challenge_method:challenge_method ->
  ?scopes:string list ->
  ?login:string ->
  unit ->
  (string, string) result
(** Pure URL builder. Always emits S256. Fails on empty required fields or
    non-github.com V1 host (after normalize). *)

(** {1 Redaction and delivery helpers} *)

val redacted_summary : start_result -> string
(** Operator/audit summary: tx id, handles, method, redirect host/path presence;
    never code_verifier, full authorization URL query, or secret handles'
    plaintext. *)

val room_summary : start_result -> string
(** Shared-Room safe summary. Never includes authorization URL, verifier,
    challenge, state, or client_id. *)

val plan_private_delivery :
  result:start_result ->
  channel:Github_user_auth_delivery.delivery_channel ->
  ?shared_room_id:string ->
  unit ->
  Github_user_auth_delivery.delivery_plan
(** Route the authorization URL privately; companion Room progress is
    secret-free. *)

val room_output_is_safe : string -> bool
(** [true] when text has no authorization URL query material, code_verifier, or
    other private PKCE secrets. *)

val contains_pkce_secrets : start_result -> string -> bool
(** [true] when [text] embeds the verifier (if resolved), authorization URL, or
    raw challenge/state from this start. *)

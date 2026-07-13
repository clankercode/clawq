(** Versioned encrypted GitHub App user-token records (P21.M2.E4.T001).

    Principal-owned GitHub user access/refresh material is stored as opaque
    secret-store handles (or authenticated ciphertext handles), never as
    plaintext in persisted/export JSON.

    Encryption at rest uses AES-256-GCM with a unique nonce and associated data
    bound to the record identity (principal, github user, app, version) so row
    swap/tamper fails closed at decrypt.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md.

    Full mutable vault CRUD, key IDs, and generation CAS are later tasks
    (P21.M2.E4.T002+). *)

val schema_version : int
(** Record schema version; starts at 1. *)

(** {1 Opaque secret backend}

    Injectable store for access/refresh plaintext. Production wires
    [Secret_store] (or vault); tests use [make_in_memory_secret_store]. *)

type secret_backend = {
  put : name:string -> plaintext:string -> (string, string) result;
      (** Store plaintext; return an opaque handle. Never log [plaintext]. *)
  get : handle:string -> (string, string) result;
      (** Resolve a handle to plaintext. *)
  delete : handle:string -> (unit, string) result;
      (** Best-effort delete; missing handle may succeed. *)
}

val make_in_memory_secret_store :
  unit -> secret_backend * (string, string) Hashtbl.t
(** Fresh mock backend. Second value is the internal handle→plaintext map for
    test assertions (tests must not export it). *)

val secret_backend_of_secret_store_key : key:string -> secret_backend
(** Backend that encrypts with [Secret_store.encrypt_secret] under [key] and
    returns [$ENC:...] handles. No AAD binding (use [seal_encrypted] for
    record-bound AAD). *)

(** {1 Versioned record} *)

type t = {
  version : int;  (** Schema version of this record. *)
  principal_id : string;  (** Owning Principal (never Room/Session). *)
  github_user_id : int64;  (** Numeric GitHub user id (account identity). *)
  access_token_handle : string;
      (** Opaque secret-store handle or authenticated ciphertext handle. *)
  refresh_token_handle : string option;
      (** Opaque handle for refresh material when present. *)
  scopes : string list;  (** Granted OAuth scopes. *)
  expires_at : string;  (** ISO-8601 UTC expiry of the access token. *)
  app_id : int;  (** GitHub App id (with user id forms account identity). *)
}
(** Durable user-token record. Token values are never stored here as plaintext.
*)

type plaintext_tokens = { access_token : string; refresh_token : string option }
(** Ephemeral token material. Must not be serialized, logged, or exported. *)

val make :
  ?version:int ->
  principal_id:string ->
  github_user_id:int64 ->
  access_token_handle:string ->
  ?refresh_token_handle:string ->
  scopes:string list ->
  expires_at:string ->
  app_id:int ->
  unit ->
  (t, string) result
(** Validate and construct a record with existing handles (no token material).
*)

val seal :
  store:secret_backend ->
  principal_id:string ->
  github_user_id:int64 ->
  tokens:plaintext_tokens ->
  scopes:string list ->
  expires_at:string ->
  app_id:int ->
  unit ->
  (t, string) result
(** Store access/refresh plaintext via [store] and return a versioned record
    with opaque handles only. *)

val resolve_tokens :
  store:secret_backend -> t -> (plaintext_tokens, string) result
(** Resolve handles back to plaintext via [store]. Fail closed on missing
    handles or empty access token. *)

val delete_tokens : store:secret_backend -> t -> (unit, string) result
(** Delete handle material for access (and refresh if present). *)

(** {1 Authenticated ciphertext (record-bound AAD)} *)

val aad_of :
  principal_id:string ->
  github_user_id:int64 ->
  app_id:int ->
  version:int ->
  string
(** Deterministic associated data binding ciphertext to record identity. *)

val encrypt_with_aad : key:string -> aad:string -> plaintext:string -> string
(** AES-256-GCM encrypt with a unique 12-byte nonce and [aad]. Returns
    [$ENC_AAD_V1:] + base64(nonce || ciphertext||tag). *)

val decrypt_with_aad :
  key:string -> aad:string -> handle:string -> (string, string) result
(** Decrypt a handle produced by [encrypt_with_aad]. AAD mismatch or wrong key
    fails closed. *)

val is_aad_handle : string -> bool
(** True when [handle] uses the [$ENC_AAD_V1:] prefix. *)

val seal_encrypted :
  key:string ->
  principal_id:string ->
  github_user_id:int64 ->
  tokens:plaintext_tokens ->
  scopes:string list ->
  expires_at:string ->
  app_id:int ->
  unit ->
  (t, string) result
(** Encrypt tokens with record-bound AAD under [key]; handles are ciphertext (no
    separate secret map). *)

val resolve_encrypted : key:string -> t -> (plaintext_tokens, string) result
(** Decrypt AAD-bound handles using record fields as AAD. *)

(** {1 JSON export / import (no plaintext tokens)} *)

val to_json : t -> Yojson.Safe.t
(** Export record metadata and handles only. Never embeds access/refresh
    plaintext. *)

val of_json : Yojson.Safe.t -> (t, string) result
(** Parse a previously exported record. *)

val export_json_string : t -> string
(** Compact JSON string from [to_json]. *)

val json_contains_plaintext : json:Yojson.Safe.t -> plaintext:string -> bool
(** Test helper: true if [plaintext] appears as any JSON string leaf. *)

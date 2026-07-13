(** Fail-closed mutable GitHub user-token vault CRUD (P21.M2.E4.T002).

    SQLite-backed store for Principal-owned GitHub App user access/refresh
    material. Every durable row is authenticated ciphertext under the versioned
    master-key boundary ({!Github_user_token_master_key}); there is no plaintext
    token fallback path in the database, exports, or diagnostics.

    Contract highlights:
    - Create / read / replace / destroy require resolved key material from an
      injectable provider that never logs key bytes.
    - Create and replace require [Ready] master-key readiness (active write key)
      and record [key_id] / [key_version] on every write.
    - Each sealed row carries a token [generation] for later CAS transitions and
      staged rewrap (T004 / T007) without weakening generation compare-and-set.
    - Missing/wrong key, corrupt envelope, unsupported version, swapped record,
      account mismatch, or crypto failure returns a typed [denial] and never a
      partial token.

    Out of scope: opaque HTTP leases (T003), full generation CAS transitions /
    lease invalidation (T004), staged rewrap engine (T007).

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

val schema_version : int
(** Vault row / envelope schema version; starts at 1. *)

val default_host : string
(** V1 live support host: [github.com]. *)

(** {1 Account identity} *)

type account_key = {
  principal_id : string;
  github_user_id : int64;
  app_id : int;
  host : string;
}
(** Logical account identity. Tokens are Principal-owned; Room/Session never
    appear here. *)

val make_account_key :
  principal_id:string ->
  github_user_id:int64 ->
  app_id:int ->
  ?host:string ->
  unit ->
  (account_key, string) result

(** {1 Sealed vault record (no plaintext tokens)} *)

type vault_record = {
  id : string;  (** Opaque vault reference for bindings / leases. *)
  account : account_key;
  record_version : int;  (** Envelope / schema version of this row. *)
  key_id : Github_user_token_master_key.key_id;
      (** Master-key id under which ciphertext is sealed. *)
  key_version : Github_user_token_master_key.key_version;
  generation : int;
      (** Token-lineage generation (CAS). Independent of [key_version]. *)
  scopes : string list;
  expires_at : string;
  created_at : string;
  updated_at : string;
}
(** Durable metadata. Ciphertext is never exposed as a plaintext token. *)

type opened = {
  record : vault_record;
  tokens : Github_user_token_store.plaintext_tokens;
}
(** In-process open result. [tokens] must not be logged, serialized, or
    exported. *)

(** {1 Typed denials (fail closed; no partial tokens)} *)

type denial =
  | Master_key_not_ready of Github_user_token_master_key.not_ready_reason list
      (** Active write key not Ready; create/replace refuse. *)
  | Missing_key of { key_id : Github_user_token_master_key.key_id }
      (** Provider has no material for the record's [key_id]. *)
  | Wrong_key  (** Material present for [key_id] but AEAD open fails. *)
  | Corrupt_envelope
      (** Ciphertext missing, truncated, malformed, or not a vault envelope. *)
  | Unsupported_version of { version : int }
      (** Row / payload version not supported by this binary. *)
  | Swapped_record
      (** Ciphertext / payload identity does not match the durable row binding.
      *)
  | Account_mismatch of { expected : account_key; found : account_key }
      (** Caller-expected account does not match the stored row. *)
  | Not_found
  | Already_exists
  | Generation_conflict of { expected : int; actual : int }
      (** Replace CAS: stored generation ≠ expected. *)
  | Crypto_failure
      (** Key material unsuitable for AEAD or unexpected crypto error. *)
  | Invalid_input of string
  | Storage of string

val string_of_denial : denial -> string
(** Redacted denial string; never includes key material or token plaintext. *)

val denial_exposes_token : denial:denial -> plaintext:string -> bool
(** Test helper: true if [plaintext] appears in the denial rendering. *)

(** {1 Injectable key material provider}

    Production wires master-key sources after {!Github_user_token_master_key}
    evaluates Ready. Tests inject static material. Providers MUST never log
    [aes_key] bytes. *)

type key_material = {
  key_id : Github_user_token_master_key.key_id;
  key_version : Github_user_token_master_key.key_version;
  aes_key : string;
      (** Exactly 32 bytes for AES-256-GCM. Never log or export. *)
}

type key_provider = {
  readiness : unit -> Github_user_token_master_key.readiness;
      (** Current master-key readiness (from T006 evaluate / probe). *)
  resolve :
    key_id:Github_user_token_master_key.key_id -> (key_material, unit) result;
      (** Resolve material for a sealed row's [key_id]. [Error ()] →
          [Missing_key]. Never log the material. *)
  active : unit -> (key_material, denial) result;
      (** Active write key when Ready; otherwise [Master_key_not_ready]. *)
}

val make_static_key_provider :
  readiness:Github_user_token_master_key.readiness ->
  keys:key_material list ->
  unit ->
  key_provider
(** Test / in-process provider. [active] uses readiness active [key_id]. *)

val make_single_key_provider :
  key_id:Github_user_token_master_key.key_id ->
  key_version:Github_user_token_master_key.key_version ->
  aes_key:string ->
  unit ->
  (key_provider, string) result
(** Ready provider with one active key. [aes_key] must be 32 bytes. *)

(** {1 Schema} *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent table [github_user_token_vault]. *)

(** {1 CRUD} *)

val create :
  db:Sqlite3.db ->
  keys:key_provider ->
  ?id:string ->
  ?now:float ->
  account:account_key ->
  tokens:Github_user_token_store.plaintext_tokens ->
  scopes:string list ->
  expires_at:string ->
  unit ->
  (vault_record, denial) result
(** Seal under the active master key and insert. Records [key_id],
    [key_version], and [generation = 1]. Fails closed when readiness is not
    Ready or ciphertext cannot be produced. Never stores plaintext tokens. *)

val read :
  db:Sqlite3.db ->
  keys:key_provider ->
  ?expected:account_key ->
  id:string ->
  unit ->
  (opened, denial) result
(** Load by opaque vault id, resolve material for the row's [key_id], open AEAD,
    and verify payload binding. Optional [expected] account must match the row
    before tokens are returned. Failures yield typed [denial] with no partial
    token. *)

val read_by_account :
  db:Sqlite3.db ->
  keys:key_provider ->
  account:account_key ->
  unit ->
  (opened, denial) result
(** Load by Principal + GitHub user + App (+ host). *)

val get_meta :
  db:Sqlite3.db -> id:string -> (vault_record option, denial) result
(** Metadata only (no decrypt). *)

val get_meta_by_account :
  db:Sqlite3.db -> account:account_key -> (vault_record option, denial) result

val replace :
  db:Sqlite3.db ->
  keys:key_provider ->
  ?now:float ->
  id:string ->
  expected_generation:int ->
  tokens:Github_user_token_store.plaintext_tokens ->
  scopes:string list ->
  expires_at:string ->
  unit ->
  (vault_record, denial) result
(** CAS replace: succeeds only when stored [generation = expected_generation].
    Reseals under the current active key, records new [key_id]/[key_version],
    and advances [generation] by 1. Does not weaken CAS for later rewrap (rewrap
    will compare generation without advancing it). *)

val destroy : db:Sqlite3.db -> id:string -> (unit, denial) result
(** Delete the sealed row. Missing id is [Not_found]. Does not require key
    material (ciphertext is discarded). *)

(** {1 Introspection helpers (no secrets)} *)

val record_to_json : vault_record -> Yojson.Safe.t
(** Metadata JSON only — never ciphertext or token plaintext. *)

val json_contains_plaintext : json:Yojson.Safe.t -> plaintext:string -> bool
(** Test helper. *)

val row_contains_plaintext :
  db:Sqlite3.db -> id:string -> plaintext:string -> (bool, denial) result
(** True if any stored text column contains [plaintext] (should always be false
    for real tokens after create). *)

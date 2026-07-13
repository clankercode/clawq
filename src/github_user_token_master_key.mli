(** Vault master-key source version and startup boundary (P21.M2.E4.T006).

    Declares the supported external master-key sources, versioned key-ID
    metadata, startup readiness, and redacted diagnostics for the Principal-
    owned GitHub user-token vault.

    Contract highlights (canonical plan + ADR 0006):
    - Master keys come from an external key source, never from the credential
      database, config exports, logs, prompts, DB rows, backups, or fallback
      storage.
    - Each sealed record will carry a versioned key ID (vault master-key version
      is independent of token generation).
    - Missing, wrong, duplicated, unsupported, inaccessible, or permission-
      unsafe keys fail closed as [NotReady]; there is no silent fallback.
    - Diagnostics and config validation never embed key material or token
      plaintext.

    This module is a DEFINE boundary: pure types, validation, and readiness.
    Staged rotation/rewrap (T007) and vault CRUD (T002) are out of scope.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

val schema_version : int
(** Master-key source contract version; starts at 1. *)

val default_env_var : string
(** Default environment variable for the active vault master key:
    [CLAWQ_GITHUB_VAULT_MASTER_KEY]. Distinct from the general
    [CLAWQ_MASTER_KEY] config-secret passphrase; operators must configure vault
    sources explicitly (no silent cross-fallback). *)

val default_max_file_mode : int
(** Default maximum permission bits for file sources: [0o600] (owner read/write
    only). Group/other bits fail closed as [Permissions]. *)

(** {1 Source kinds (versioned)} *)

(** Supported external master-key sources for schema version 1. *)
type source_kind =
  | Env of { var_name : string }
      (** Key material from a process environment variable. *)
  | File of { path : string }
      (** Key material from a filesystem path; mode checks apply. *)

type key_role =
  | Active  (** Declared write key for new seals. Exactly one must be Ready. *)
  | Staged
      (** Present for rotation/rewrap (T007); optional at basic startup. *)
  | Backup_required
      (** Required to restore a retained backup set; optional at basic startup.
      *)
  | Retired
      (** Known retired ID (metadata only; must not supply material at startup).
      *)

type key_id = string
(** Opaque vault key identifier. Never the raw key bytes. *)

type key_version = int
(** Monotonic vault master-key version. Independent of token generation. *)

type key_metadata = {
  key_id : key_id;
  key_version : key_version;
  role : key_role;
  source_kind : source_kind;
}
(** Public key identity for diagnostics and sealed-record association. *)

(** {1 Fail-closed reason codes} *)

type not_ready_reason =
  | Missing  (** Configured source has no material (unset env, missing file). *)
  | Wrong  (** Material present but fails length/format checks. *)
  | Duplicated
      (** Conflicting Active sources, duplicate key IDs, or multi-source clash.
      *)
  | Unsupported
      (** Schema version or source kind not supported by this binary. *)
  | Inaccessible  (** Source cannot be read (I/O error, unreadable path). *)
  | Permissions  (** File mode too permissive or not a regular file. *)
  | Empty  (** Source resolved but material is empty after trim. *)
  | Invalid_metadata  (** key_id / key_version / role fields invalid. *)
  | No_active  (** No Active-role source declared or successfully loaded. *)

val string_of_reason : not_ready_reason -> string
val string_of_role : key_role -> string

val string_of_source_kind : source_kind -> string
(** Redacted source description (var name or path only; never material). *)

(** {1 Config (no key material)} *)

type source_config = {
  kind : source_kind;
  key_id : key_id;
  key_version : key_version;
  role : key_role;
  min_length : int option;
      (** When set, material must have at least this many bytes after trim. *)
  expected_length : int option;
      (** When set, material must have exactly this many bytes after trim. *)
  max_file_mode : int option;
      (** File sources only; defaults to [default_max_file_mode]. *)
}
(** Declares one external key source. Never embeds key material. *)

type keyring_config = { schema_version : int; sources : source_config list }
(** Operator-declared external keyring. Validated before any material load. *)

val make_source :
  kind:source_kind ->
  key_id:key_id ->
  key_version:key_version ->
  role:key_role ->
  ?min_length:int ->
  ?expected_length:int ->
  ?max_file_mode:int ->
  unit ->
  (source_config, string) result
(** Construct a source entry with field validation (no I/O). *)

val make_keyring :
  ?schema_version:int ->
  sources:source_config list ->
  unit ->
  (keyring_config, string) result
(** Construct a keyring config. Fails on unsupported schema or empty sources. *)

val validate_keyring_config :
  keyring_config -> (unit, not_ready_reason list) result
(** Pure config validation without reading env/files or logging secrets.

    Fails closed on unsupported schema, invalid metadata, zero Active sources,
    multiple Active sources, duplicate key IDs, or retired sources that claim a
    live material path. *)

(** {1 Observations and injectables} *)

type env_observation = {
  present : bool;
  empty : bool;
  byte_length : int option;  (** Length only; value is never retained. *)
}

type file_observation = {
  exists : bool;
  readable : bool;
  is_regular : bool;
  mode : int option;
  size : int option;
  mode_ok : bool option;
      (** [Some false] when mode exceeds allowed owner-only bits. *)
}

type material_observation = {
  present : bool;
  empty : bool;
  byte_length : int option;
  valid : bool;
  failure : not_ready_reason option;
}
(** Outcome of validating material without retaining it. *)

type source_observation = {
  config : source_config;
  env : env_observation option;
  file : file_observation option;
  material : material_observation;
  access_error : string option;
      (** Short redacted error tag (e.g. ["enoent"], ["eacces"]); never content.
      *)
}

type file_stat = {
  exists : bool;
  readable : bool;
  is_regular : bool;
  mode : int;
  size : int;
}

type env_reader = var_name:string -> string option
(** Injectable env lookup. Production: [Sys.getenv_opt]. *)

type file_stat_fn = path:string -> (file_stat, string) result
(** Injectable path stat. Error string is a short code, not path contents. *)

type file_read_fn = path:string -> (string, string) result
(** Injectable path read. Error string is a short code, not file contents. *)

val default_env_reader : env_reader
val observe_env : env_reader:env_reader -> var_name:string -> env_observation

val observe_source :
  env_reader:env_reader ->
  file_stat:file_stat_fn ->
  file_read:file_read_fn ->
  source_config ->
  source_observation
(** Collect a single-source observation. Validates material then discards it. *)

val observe_keyring :
  env_reader:env_reader ->
  file_stat:file_stat_fn ->
  file_read:file_read_fn ->
  keyring_config ->
  source_observation list
(** Observe every declared source. *)

(** {1 Startup readiness} *)

type readiness =
  | Ready of {
      active : key_metadata;
      available : key_metadata list;
          (** Successfully loaded non-Active keys (staged / backup_required). *)
    }
  | NotReady of {
      reasons : not_ready_reason list;
      observed : key_metadata list;
          (** Declared sources observed, without implying material validity. *)
    }
      (** Startup decision. [Ready] requires exactly one valid Active key and a
          supported schema. [NotReady] is fail-closed: user authorization and
          vault writes must refuse. *)

val evaluate :
  keyring:keyring_config -> observations:source_observation list -> readiness
(** Pure readiness decision from config + observations (no I/O). *)

val probe :
  ?env_reader:env_reader ->
  ?file_stat:file_stat_fn ->
  ?file_read:file_read_fn ->
  keyring_config ->
  readiness
(** Observe configured sources then [evaluate]. Defaults use process env; file
    injectables default to inaccessible ([Inaccessible]) so production callers
    must wire real file probes explicitly when File sources are used. *)

val is_ready : readiness -> bool
val active_metadata : readiness -> key_metadata option
val reasons : readiness -> not_ready_reason list

val allows_user_authorization : readiness -> bool
(** True only when [Ready]. Callers must refuse act-as-user and vault seal while
    false; there is no plaintext fallback store. *)

(** {1 Redacted diagnostics (never key material)} *)

type redacted_diagnostics = {
  schema_version : int;
  ready : bool;
  active_key_id : key_id option;
  active_key_version : key_version option;
  active_role : string option;
  active_source : string option;
  available_key_ids : key_id list;
  source_kinds : string list;
  reasons : string list;
  observed_key_ids : key_id list;
  allows_user_authorization : bool;
  note : string;
}
(** Safe for logs, config exports, admin status, and audit. *)

val diagnostics : schema_version:int -> readiness -> redacted_diagnostics
val diagnostics_to_json : redacted_diagnostics -> Yojson.Safe.t
val format_diagnostics : redacted_diagnostics -> string

val diagnostics_contains_plaintext :
  diagnostics:redacted_diagnostics -> plaintext:string -> bool
(** Test helper: true if [plaintext] appears in any diagnostic string field. *)

val json_contains_plaintext : json:Yojson.Safe.t -> plaintext:string -> bool
(** Test helper: true if [plaintext] appears as any JSON string leaf. *)

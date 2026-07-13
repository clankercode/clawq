(** Callback-scoped opaque leases at the GitHub HTTP boundary (P21.M2.E4.T003).

    Access snapshots, jobs, receipts, tools, and audit/export surfaces may carry
    only:
    - an opaque lease [handle]
    - binding principal / account identity
    - the vault [generation] pinned at issue time

    Raw access (and refresh) token material is opened only inside {!with_token}
    / {!with_authorization_header} callbacks for GitHub HTTP dispatch. It is
    never returned from this module's public values, never embedded in lease
    JSON / string / log helpers, and there is intentionally no API that injects
    user tokens into runner env, shell, or Git transport.

    Fail-closed at use time:
    - lease expired (TTL)
    - lease revoked / discarded
    - vault generation advanced past the pinned generation
    - vault account no longer matches the pinned binding
    - access token [expires_at] reached
    - vault open denials (missing key, tamper, etc.)

    This is a trusted in-process API boundary (same model as
    {!Credential_lease}): it prevents accidental leakage into logs, prompts,
    jobs, and tool arguments. It is not capability security against malicious
    OCaml that bypasses the type system.

    Generation CAS transitions that invalidate live leases on
    replace/disable/revoke/unlink are coordinated by {!Github_user_token_cas}
    (T004). This module refuses wrong-generation and inactive-vault use and
    exposes {!invalidate_generation} / {!discard_for_vault} hooks for that path
    and for vault recovery.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

val default_ttl_seconds : float
(** Default lease TTL when issuing (seconds). Covers a single HTTP dispatch
    window; default is 300s. *)

(** {1 Opaque handle and binding identity}

    Safe to place on access snapshots, jobs, receipts, tools, and logs. *)

type handle = private string
(** Opaque lease id. Never a token. *)

val handle_to_string : handle -> string

val handle_of_string : string -> (handle, string) result
(** Accept non-empty trimmed handle strings (e.g. rehydrated from a receipt). *)

type binding = {
  principal_id : string;
  github_user_id : int64;
  app_id : int;
  host : string;
  vault_id : string;  (** Opaque {!Github_user_token_vault} row id. *)
  generation : int;
      (** Token-lineage generation pinned at issue. Independent of master-key
          version. *)
  binding_id : string option;
      (** Optional {!Github_account_binding} row id when known. *)
}
(** Principal + account binding + generation carried across non-HTTP boundaries.
    Contains no token material. *)

type identity = {
  handle : handle;
  binding : binding;
  scopes : string list;  (** Non-secret OAuth scopes recorded at issue. *)
  token_expires_at : string;
      (** Access-token expiry ISO-8601 from the vault row (metadata only). *)
  issued_at : float;
  lease_expires_at : float;
  revoked : bool;
}
(** Fully redacted lease view — safe to log / serialize / place on jobs. *)

type lease
(** Live process-local lease. Construct only via {!issue} /
    {!issue_from_record}. Does not embed raw token bytes; opening is deferred to
    {!with_token}. *)

val identity_of : lease -> identity
(** Redacted identity view of a live lease (safe for logs / jobs / receipts). *)

val handle : lease -> handle
val binding : lease -> binding
val generation : lease -> int
val vault_id : lease -> string
val is_revoked : lease -> bool
val is_expired : ?now:float -> lease -> bool

(** {1 Typed denials (fail closed; no partial tokens)} *)

type denial =
  | Lease_not_found  (** Unknown handle / missing vault row at issue or use. *)
  | Lease_expired
  | Lease_revoked
  | Generation_mismatch of { expected : int; actual : int }
      (** Vault generation advanced past the lease pin (refresh/replace/revoke).
      *)
  | Vault_not_active
      (** Vault row is inactive (disabled / revoked / unlinked). *)
  | Token_expired  (** Access token [expires_at] is at or before [now]. *)
  | Account_mismatch of {
      expected : Github_user_token_vault.account_key;
      found : Github_user_token_vault.account_key;
    }
  | Vault of Github_user_token_vault.denial
      (** Propagated vault open / storage denial (already redacted). *)
  | Invalid_input of string
  | Forbidden_surface of string
      (** Explicit refuse for runner / shell / Git transport injection. *)

val string_of_denial : denial -> string
(** Redacted denial string; never includes token plaintext or key material. *)

val denial_exposes_token : denial:denial -> plaintext:string -> bool
(** Test helper: true if [plaintext] appears in the denial rendering. *)

(** {1 Issue (no raw token returned)} *)

val issue :
  db:Sqlite3.db ->
  ?now:float ->
  ?ttl_seconds:float ->
  ?binding_id:string ->
  ?expected:Github_user_token_vault.account_key ->
  vault_id:string ->
  unit ->
  (lease, denial) result
(** Issue a callback-scoped lease from vault metadata only (no decrypt). Pins
    principal, account, vault id, and generation. Optional [expected] account
    must match the row. Fails closed when the vault row is missing. Never
    returns access/refresh plaintext. *)

val issue_from_record :
  ?now:float ->
  ?ttl_seconds:float ->
  ?binding_id:string ->
  record:Github_user_token_vault.vault_record ->
  unit ->
  (lease, denial) result
(** Issue from an already-loaded metadata record (still no token material). *)

(** {1 Open raw token only inside the GitHub HTTP callback} *)

val with_token :
  db:Sqlite3.db ->
  keys:Github_user_token_vault.key_provider ->
  ?now:float ->
  lease:lease ->
  f:(access_token:string -> 'a) ->
  unit ->
  ('a, denial) result
(** Re-validate lease (revoked/expired), re-open the vault row, check generation
    \+ account + token expiry, then call [f] with the access token only. Refresh
    material is never exposed. The access token exists only for the duration of
    [f]; this function never returns it as a plain value.

    [f] may return HTTP response data (or other non-secret results). Callers
    must not capture the token into logs, prompts, jobs, runners, shell, or Git
    transport. *)

val with_authorization_header :
  db:Sqlite3.db ->
  keys:Github_user_token_vault.key_provider ->
  ?now:float ->
  ?header_name:string ->
  lease:lease ->
  f:(headers:(string * string) list -> 'a) ->
  unit ->
  ('a, denial) result
(** GitHub HTTP boundary helper: builds
    [[(header_name, "Bearer " ^ access_token)]] (default header
    ["Authorization"]) and invokes [f]. Same fail-closed checks as
    {!with_token}. *)

(** {1 Revoke / discard (process-local)} *)

val revoke : lease -> unit
(** Mark this lease revoked. Subsequent {!with_token} fails closed. *)

val revoke_handle : handle:handle -> bool
(** Revoke by opaque handle if still registered. Returns whether a live lease
    was found. *)

val discard_for_vault : vault_id:string -> int
(** Revoke every live lease for [vault_id]. Returns count discarded. Used by
    vault recovery and T004 generation invalidation. *)

val invalidate_generation : vault_id:string -> generation:int -> int
(** Revoke live leases for [vault_id] whose pinned generation is [<=]
    [generation]. Returns count discarded. Stale writers after a replace cannot
    keep using pre-CAS authority. *)

val discard_all : unit -> int
(** Revoke every registered lease (restore / compromise recovery hook). *)

val live_count : unit -> int
(** Process-local count of non-revoked registered leases (tests/ops). *)

(** {1 Export / introspection (always redacted)} *)

val to_json : lease -> Yojson.Safe.t
(** Identity JSON only — handle, binding, generation, scopes, timestamps. Never
    embeds access/refresh plaintext or ciphertext. *)

val identity_to_json : identity -> Yojson.Safe.t
(** Same shape as [to_json] for a detached identity value. *)

val identity_of_json : Yojson.Safe.t -> (identity, string) result
(** Parse a previously exported identity. Does not restore a live openable lease
    by itself — re-issue after revalidation for HTTP use. *)

val string_of_identity : identity -> string
(** Compact single-line redacted summary for logs. *)

val json_contains_plaintext : json:Yojson.Safe.t -> plaintext:string -> bool
(** Test helper. *)

val identity_contains_plaintext : identity:identity -> plaintext:string -> bool
(** Test helper: true if [plaintext] appears in any identity string field. *)

(** {1 Explicit refuse surfaces}

    User tokens must never enter runner env, shell, or Git transport. These
    always fail closed so call sites can document the contract in types. *)

val refuse_runner_env : lease -> (unit, denial) result
val refuse_shell_injection : lease -> (unit, denial) result
val refuse_git_transport : lease -> (unit, denial) result

(** Resolve currently eligible Principal-owned GitHub accounts for a context
    with first-use preference semantics (P21.M3.E2.T002).

    Wraps {!Github_account_preference} precedence and adds current-validity
    checks required for user-attributed actions after private authorization
    activation (P21.M2.E2.T003 / P21.M2.E3.T003):

    - binding [Authorized]
    - host / optional App filter
    - vault reference attached
    - vault row present, bound to the same account, and [active]
    - Principal is current active lineage (not tombstone / missing)

    Selection never guesses by login, display name, recency, or another Room
    participant's preference.

    {2 Resolution precedence (highest first)}

    1. Explicit choice (binding id or lineage id) 2. Room + Repo 3. Room + Org
    4. Room-only (when present in storage) 5. Principal + Repo 6. Principal +
    Org 7. Principal default 8. Sole currently-valid eligible account 9.
    [Ambiguous] / [None_eligible] private prompt

    Steps 2–7 only win when the stored preference resolves to a currently valid
    binding owned by the same Principal. Stale, foreign, revoked,
    vault-inactive, or unauthorized targets fall through.

    {2 First-use context preferences}

    When a private selection is confirmed for a context that has no stored
    preference at the most specific applicable scope,
    {!record_first_use_preference} writes that scope (Room+Repo → Room+Org →
    Principal+Repo → Principal+Org → Principal default). It never overwrites an
    existing preference and never invents a selection from login/recency.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module P = Principal_identity
module B = Github_account_binding
module Pref = Github_account_preference
module V = Github_user_token_vault

val schema_version : int
(** Resolver schema / export version; starts at 1. *)

val ensure_schema : Sqlite3.db -> unit
(** Ensures Principal, binding, preference, and vault tables. *)

(** {1 Current validity} *)

type validity_failure =
  | Not_authorized
  | Host_or_app_mismatch
  | Missing_vault_ref
  | Vault_missing
  | Vault_inactive
  | Vault_account_mismatch
  | Principal_not_current of string
  | Storage of string

val string_of_validity_failure : validity_failure -> string

type validity = Valid | Invalid of validity_failure

val check_binding_validity :
  db:Sqlite3.db ->
  host:string ->
  ?app_id:int ->
  binding:B.binding ->
  unit ->
  validity
(** Structural + vault-meta current validity for one binding. Does not open
    token material and never consults login/display. *)

val list_currently_valid_bindings :
  db:Sqlite3.db ->
  principal_id:P.principal_id ->
  ?host:string ->
  ?app_id:int ->
  unit ->
  (B.binding list, string) result
(** Principal-owned bindings that pass {!check_binding_validity}. Sorted by
    binding id ascending (stable; deliberately not recency or login). Fails when
    the Principal is not current active lineage. *)

(** {1 Resolution}

    Same result type as {!Pref.resolve} so private-prompt / redacted export
    helpers stay shared. *)

val resolve :
  db:Sqlite3.db ->
  context:Pref.resolve_context ->
  unit ->
  (Pref.resolve_result, string) result
(** Apply preference precedence over currently valid eligible accounts only. *)

(** {1 First-use context preferences} *)

val first_use_scope :
  context:Pref.resolve_context -> (Pref.preference_scope, string) result
(** Most specific preference scope for [context]: Room+Repo when room and repo
    are present; else Room+Org; else Principal+Repo; else Principal+Org; else
    Principal default. Room-only is not chosen for first-use recording (public
    contract lists Room+Repo / Room+Org / Repo / Org / default). *)

type first_use_record =
  | Recorded of Pref.stored_preference
      (** Preference written at the first-use scope. *)
  | Already_set of Pref.stored_preference
      (** Scope already had a preference; left unchanged. *)
  | Not_eligible of string
      (** Binding is not currently valid for this Principal/context. *)

val record_first_use_preference :
  db:Sqlite3.db ->
  ?now:float ->
  context:Pref.resolve_context ->
  binding:B.binding ->
  unit ->
  (first_use_record, string) result
(** After private account selection (or sole-eligible confirmation), record a
    preference at {!first_use_scope} when that scope is unset. Requires
    [binding] to be currently valid under [context]. Does not establish
    ownership. *)

val first_use_record_to_json : first_use_record -> Yojson.Safe.t
(** Redacted diagnostic JSON; no tokens. *)

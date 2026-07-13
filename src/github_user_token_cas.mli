(** Generation-based CAS transitions and lease invalidation (P21.M2.E4.T004).

    Coordinates {!Github_user_token_vault}, {!Github_user_token_lease}, and
    optional {!Github_account_binding} updates so that:

    - Token replacement compares account binding, generation, and active state
      transactionally; successful replace advances generation and immediately
      invalidates leases pinned to the prior generation.
    - Disable / revoke / unlink set [active = false], advance generation, update
      binding authorization status when a binding id is supplied, and discard
      live leases so old authority fails closed immediately.
    - Concurrent stale writers that still hold an older generation cannot
      restore older token material or flip active state back.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

module V = Github_user_token_vault
module L = Github_user_token_lease
module B = Github_account_binding

(** {1 Outcomes} *)

type transition = {
  record : V.vault_record;
      (** Post-CAS vault metadata (generation advanced; active as set). *)
  leases_invalidated : int;
      (** Process-local leases discarded for the prior generation / vault. *)
  binding : B.binding option;
      (** Updated binding when [binding_id] was provided and found. *)
}
(** Result of a successful coordinated CAS transition. *)

type denial =
  | Vault of V.denial  (** Propagated vault denial (already redacted). *)
  | Binding of string  (** Binding load/update failure (no secrets). *)
  | Invalid_input of string

val string_of_denial : denial -> string
(** Redacted denial string; never includes token or key material. *)

val denial_exposes_token : denial:denial -> plaintext:string -> bool
(** Test helper. *)

(** {1 Replace (active token material)} *)

val replace :
  db:Sqlite3.db ->
  keys:V.key_provider ->
  ?now:float ->
  id:string ->
  expected_generation:int ->
  expected:V.account_key ->
  ?binding_id:string ->
  tokens:Github_user_token_store.plaintext_tokens ->
  scopes:string list ->
  expires_at:string ->
  unit ->
  (transition, denial) result
(** Transactional CAS replace under [BEGIN IMMEDIATE]:

    - Requires stored binding = [expected], generation = [expected_generation],
      and [active = true].
    - Reseals new token material, advances generation by 1, leaves active true.
    - Invalidates process-local leases for [id] with generation
      [<= expected_generation].
    - Optional [binding_id] is re-validated to match the vault account and vault
      ref; binding authorization is left unchanged on pure replace.

    Stale concurrent writers with an older generation fail closed and cannot
    restore prior tokens. *)

(** {1 Disable / revoke / unlink} *)

val disable :
  db:Sqlite3.db ->
  keys:V.key_provider ->
  ?now:float ->
  id:string ->
  expected_generation:int ->
  expected:V.account_key ->
  ?binding_id:string ->
  unit ->
  (transition, denial) result
(** CAS deactivate for a temporary local hold. Sets [active = false], advances
    generation, invalidates leases, and sets binding status [Disabled] when
    [binding_id] is provided. *)

val revoke :
  db:Sqlite3.db ->
  keys:V.key_provider ->
  ?now:float ->
  id:string ->
  expected_generation:int ->
  expected:V.account_key ->
  ?binding_id:string ->
  unit ->
  (transition, denial) result
(** CAS deactivate for upstream or local revoke. Sets [active = false], advances
    generation, invalidates leases, and sets binding status [Revoked] when
    [binding_id] is provided. Relink is required for new authority. *)

val unlink :
  db:Sqlite3.db ->
  keys:V.key_provider ->
  ?now:float ->
  id:string ->
  expected_generation:int ->
  expected:V.account_key ->
  ?binding_id:string ->
  unit ->
  (transition, denial) result
(** CAS deactivate for explicit unlink/split. Sets [active = false], advances
    generation, invalidates leases, and when [binding_id] is provided sets
    status [Unlinked] and clears the opaque vault ref (no token material moves).
*)

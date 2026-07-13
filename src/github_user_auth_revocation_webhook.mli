(** Process GitHub App authorization revocation webhooks (P21.M3.E1.T003).

    When a user revokes their authorization of a GitHub App upstream, GitHub
    delivers [github_app_authorization] with action [revoked]. After verifying
    App identity (HMAC + configured App id via an injectable verifier) and the
    sender's numeric GitHub user id, this module:

    - Finds every local {!Github_account_binding} matching that App + user (any
      host / principal)
    - For each matching binding: CAS-revoke via {!Github_user_token_cas}
      ([active = false], generation advanced, binding [Revoked], leases
      discarded) then destroy sealed vault secrets via
      {!Github_user_token_vault.destroy}
    - Destroys orphan vault rows for the same App + user that are not linked
      through a live binding
    - Records exactly one redacted receipt keyed by [delivery_id] (idempotent)

    Refresh, lease issue, API act-as-user, and jobs that require an active vault
    or [Authorized] binding fail closed after a successful revocation. Replay of
    the same [delivery_id] returns the stored receipt without re-applying side
    effects.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

module I = Github_app_webhook_ingress
module V = Github_user_token_vault
module B = Github_account_binding

(** {1 Verified revocation identity} *)

type verified_revocation = {
  delivery_id : string;
  app_id : int;
      (** Verified App identity (payload or configured expected App). *)
  github_user_id : int64;  (** Sender numeric GitHub user id. *)
  action : string;  (** Expected [revoked]. *)
  event : string;  (** Expected [github_app_authorization]. *)
}
(** Secret-free verified identity for one revocation delivery. *)

(** {1 Injectable verifier}

    Production wires HMAC + durable delivery identity
    ({!Github_app_webhook_ingress}) plus payload parse. Tests inject offline
    fakes that return a [verified_revocation] without crypto. *)

type verify_denial =
  | Ingress of I.reject_reason * string
      (** Ingress reject (bad signature, path, event, …). *)
  | Duplicate_delivery of string  (** Delivery already reserved by ingress. *)
  | Wrong_event of string
  | Wrong_action of string
  | Missing_sender
  | Invalid_payload of string

val string_of_verify_denial : verify_denial -> string
(** Redacted denial; never includes signature secrets or tokens. *)

type verifier =
  db:Sqlite3.db ->
  webhook_secret:string ->
  expected_app_id:int ->
  ?now:float ->
  request:I.request ->
  unit ->
  (verified_revocation, verify_denial) result
(** Injectable App-authorization verifier. *)

val default_verifier : verifier
(** Verify via {!I.verify_and_accept}, require event [github_app_authorization]
    \+ action [revoked], extract [sender.id] as the numeric user id, and resolve
    App id from the accepted payload or [expected_app_id]. *)

val parse_verified_from_accepted :
  accepted:I.accepted ->
  expected_app_id:int ->
  (verified_revocation, verify_denial) result
(** Pure parse of a already-Accepted ingress payload (tests / wiring). *)

(** {1 Redacted receipt} *)

type binding_effect = {
  binding_id : string;
  principal_id : string;
  host : string;
  vault_id : string option;
  prior_generation : int option;
  new_generation : int option;
  already_revoked : bool;
  secrets_destroyed : bool;
  leases_invalidated : int;
}
(** Per-binding redacted effect. No tokens, ciphertext, or login. *)

type receipt = {
  id : string;
  delivery_id : string;
  app_id : int;
  github_user_id : int64;
  action : string;
  bindings_matched : int;
  bindings_revoked : int;
      (** Bindings newly transitioned to [Revoked] (or already terminal counted
          as already_revoked). *)
  secrets_destroyed : int;
  leases_invalidated : int;
  orphan_secrets_destroyed : int;
  already_processed : bool;
      (** [true] when this receipt was loaded from a prior delivery (idempotent
          replay). *)
  effects : binding_effect list;
  created_at : string;
}
(** One redacted receipt per delivery. Safe for audit / operator surfaces. *)

val receipt_to_json : receipt -> Yojson.Safe.t
(** Metadata JSON only — never tokens or sealed ciphertext. *)

val receipt_contains_plaintext : receipt:receipt -> plaintext:string -> bool
(** Test helper. *)

val string_of_receipt : receipt -> string
(** Compact redacted operator summary. *)

(** {1 Outcome / denial} *)

type outcome =
  | Applied of receipt
      (** First successful processing for this [delivery_id]. *)
  | Duplicate of receipt
      (** Same [delivery_id] already processed; prior receipt returned. *)
  | Ignored of { reason : string; message : string }
      (** Verified delivery that is not an actionable revocation (e.g. unknown
          action). No binding mutations. *)

type denial =
  | Verify of verify_denial
  | Binding of string
  | Vault of V.denial
  | Cas of Github_user_token_cas.denial
  | Storage of string
  | Invalid_input of string

val string_of_denial : denial -> string
(** Redacted denial; never includes token or key material. *)

val denial_exposes_token : denial:denial -> plaintext:string -> bool
(** Test helper. *)

(** {1 Schema} *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent table [github_user_auth_revocation_receipts] (redacted columns
    only). Also ensures binding, vault, and ingress schemas. *)

val get_receipt_by_delivery :
  db:Sqlite3.db -> delivery_id:string -> (receipt option, string) result
(** Load a stored receipt by GitHub delivery id. *)

(** {1 Process} *)

val process_verified :
  db:Sqlite3.db ->
  keys:V.key_provider ->
  ?now:float ->
  verified:verified_revocation ->
  unit ->
  (outcome, denial) result
(** Apply revocation for a pre-verified identity. Idempotent on [delivery_id]: a
    second call returns [Duplicate] with the stored receipt and performs no
    further generation advances or destroys. Non-[revoked] actions yield
    [Ignored]. *)

val process :
  db:Sqlite3.db ->
  keys:V.key_provider ->
  webhook_secret:string ->
  expected_app_id:int ->
  ?verify:verifier ->
  ?now:float ->
  request:I.request ->
  unit ->
  (outcome, denial) result
(** Verify (injectable) then {!process_verified}. Default [verify] is
    {!default_verifier}. *)

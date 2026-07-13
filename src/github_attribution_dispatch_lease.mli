(** Issue opaque GitHub leases after final authorization revalidation
    (P21.M3.E2.T007).

    Immediately before HTTP dispatch this module:

    + revalidates the frozen prior [Allow] decision against live evidence
    + CAS-checks the prior checked revisions (Tool catalog, access, Principal,
      confirmation, binding lineage, vault generation, installation, live state,
      actor snapshot) via {!Github_attribution_authorize}
    + re-checks mode / action / Principal / binding continuity against the prior
      decision
    + on [User] mode, issues a callback-scoped opaque lease only (no raw token)
    + on [App] mode, returns the revalidated allow with [lease = None]
    + on any change, denies closed without raw token escape

    Callers assemble live injectable evidence the same way as
    {!Github_attribution_authorize}; this module never opens vault ciphertext
    except through {!Github_user_token_lease} (which keeps plaintext inside
    [with_token] callbacks). It never returns access/refresh tokens as plain
    values.

    Policy races and revocation between preview, queue, and dispatch fail
    closed: stale pins, generation advance/rollback, SSO/permission loss,
    binding unlink, and vault disable all deny without lease issuance.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Auth = Github_attribution_authorize
module Lease = Github_user_token_lease
module V = Github_user_token_vault

val schema_version : int
(** Dispatch-lease export schema; starts at 1. *)

(** {1 Pins from a prior Allow}

    Convert frozen checked revisions from a preview/queue Allow into a
    {!Auth.revision_pin} so dispatch revalidation CAS-checks the same surface.
*)

val pin_of_checked_revisions : Auth.checked_revisions -> Auth.revision_pin
val pin_of_allow : Auth.allow -> Auth.revision_pin

val request_with_prior_pin :
  live:Auth.request -> prior:Auth.allow -> Auth.request
(** Replace [live.pin] with pins derived from [prior.revisions]. Live evidence
    fields are left unchanged. *)

(** {1 Issued result (no raw token)} *)

type issued = {
  mode : Auth.resolved_mode;
  decision : Auth.allow;
      (** Fresh Allow from revalidation immediately before dispatch. *)
  lease : Lease.lease option;
      (** [Some] only when [mode = User]. Opaque; open only via
          {!Lease.with_token}. [None] for App-installation paths. *)
  identity : Lease.identity option;
      (** Redacted identity when a lease was issued; safe for jobs/receipts. *)
}
(** Successful dispatch gate. Never embeds access/refresh plaintext. *)

val issued_to_json : issued -> Yojson.Safe.t
(** Redacted JSON: mode, decision revisions, optional lease identity. Never
    embeds tokens. *)

val string_of_issued : issued -> string
(** One-line non-secret summary. *)

(** {1 Denials (fail closed; no partial tokens)} *)

type denial =
  | Authorization of Auth.deny
      (** Live revalidation returned Deny (stale pin, policy, SSO, etc.). *)
  | Prior_mode_mismatch of {
      expected : Auth.resolved_mode;
      actual : Auth.resolved_mode;
    }
  | Prior_action_mismatch of { expected : string; actual : string }
  | Prior_principal_mismatch of {
      expected : string option;
      actual : string option;
    }
  | Prior_binding_mismatch of {
      expected : string option;
      actual : string option;
    }
  | User_lease_requires_vault_id
      (** User mode Allow but caller omitted [vault_id]. *)
  | Generation_race of { expected : int; actual : int }
      (** Vault generation changed between authorize evidence and lease issue;
          issued lease (if any) is revoked before return. *)
  | Lease of Lease.denial
      (** Propagated opaque-lease / vault denial (already redacted). *)
  | Invalid_input of string

val string_of_denial : denial -> string
(** Redacted denial string; never includes token plaintext or key material. *)

val denial_to_json : denial -> Yojson.Safe.t
(** Redacted structured denial for audit / repair. *)

val denial_exposes_token : denial:denial -> plaintext:string -> bool
(** Test helper: true if [plaintext] appears in the denial rendering. *)

(** {1 Pure revalidation (no lease)} *)

val revalidate :
  live:Auth.request -> prior:Auth.allow -> unit -> (Auth.allow, denial) result
(** Pin [live] from [prior], run {!Auth.authorize}, and enforce mode / action /
    Principal / binding continuity. Issues no lease and no token. *)

(** {1 Revalidate then issue opaque lease} *)

val issue_for_dispatch :
  db:Sqlite3.db ->
  ?now:float ->
  ?ttl_seconds:float ->
  live:Auth.request ->
  prior:Auth.allow ->
  ?vault_id:string ->
  ?expected:V.account_key ->
  unit ->
  (issued, denial) result
(** Immediately before HTTP dispatch:

    1. {!revalidate} against live evidence pinned by [prior] 2. On User mode:
    require [vault_id], issue {!Lease.issue} with optional [expected] account
    and [prior] binding_id 3. After issue, refuse if vault generation advanced
    past the prior pin (revoke the just-issued lease on race) 4. On App mode:
    return [lease = None] without touching the vault

    Never returns raw access/refresh tokens. Use {!Lease.with_token} /
    {!Lease.with_authorization_header} on [issued.lease] for HTTP only. *)

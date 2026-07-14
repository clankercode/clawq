(** Authenticated web, CLI, and direct-session Principal bootstrap trust
    adapters (P21.M1.E1.T009).

    Local process, Session, display, or request metadata alone never grants a
    Principal. Only verified durable issuer plus immutable subject (web OIDC),
    or explicit CLI device enrolment that still maps to a Principal, may resolve
    to a Principal. Direct sessions, raw web claims, absent/forged/stale/
    ambiguous provenance, and revoked enrolment remain anonymous and cannot
    start linking or user authorization.

    No configured web OIDC verifier exists in this module yet. [Web_oidc]
    therefore represents unverified decoded claims and is deliberately rejected
    rather than treating issuer/subject/expiry fields as authentication.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0005-separate-human-principals-from-room-sessions.md.

    Note on constructor names: the task sketch uses [Anonymous] for both
    provenance and decision. OCaml keeps constructors in one module namespace,
    so absent provenance is [Absent] and the fail-closed decision is
    [Anonymous of \{ reason \}]. *)

type provenance =
  | Web_oidc of { issuer : string; subject : string; exp : float }
      (** Unverified decoded OIDC claims. Always rejected until a configured
          issuer/JWT/JWKS verifier constructs authenticated evidence. *)
  | Cli_enrolled of { device_id : string; principal_id : string; exp : float }
      (** CLI device claim; requires live enrolment lookup (revocation-aware).
      *)
  | Direct_session of { session_key : string }
      (** Process/session identity — never sufficient alone. *)
  | Absent  (** Explicit absent / anonymous provenance. *)

type decision =
  | Principal of Principal_identity.principal_id
  | Anonymous of { reason : string }

val resolve :
  provenance:provenance ->
  ?now:float ->
  ?enrolled:(device_id:string -> Principal_identity.principal_id option) ->
  unit ->
  decision
(** Fail-closed Principal bootstrap.

    - [Direct_session] is always [Anonymous].
    - [Web_oidc] is always [Anonymous]: a raw issuer/subject/expiry tuple has no
      configured issuer trust anchor or JWT/JWKS signature verification.
    - [Cli_enrolled] requires non-empty device id, [exp > now], an [enrolled]
      lookup that returns a Principal for that device (missing = not enrolled or
      revoked), and claimed [principal_id] matching the enrolment.
    - [Absent] provenance yields [Anonymous]. *)

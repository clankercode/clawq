(** Resolve adapter-verified Connector actors to stable Principals
    (P21.M1.E1.T003).

    This is the single persistence resolution path after trust adapters have
    already verified identity. Callers must only supply [connector_actor_key]
    values derived from typed Human outcomes of Teams, Slack, Discord, or
    Telegram ingress, or from authenticated web/CLI bootstrap. Bot, forged,
    replayed, missing, expired, revoked, or ambiguous provenance must never be
    converted into an actor key and never call [resolve_or_create].

    Namespace plus immutable user ID is collision-safe (store UNIQUE). Display
    renames do not change the resolved Principal. Concurrent first-seen races
    re-read the winning owner.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0005-separate-human-principals-from-room-sessions.md. *)

val resolve_or_create :
  db:Sqlite3.db ->
  actor_key:Principal_identity.connector_actor_key ->
  ?display:Principal_identity.display_metadata ->
  ?now:float ->
  unit ->
  (Principal_identity.principal_id, string) result
(** First-seen creates principal+actor+link via
    {!Principal_identity_store.create_first_seen}; subsequent returns the
    existing Principal. On concurrent first-seen collision, re-reads and returns
    the winning owner. *)

(** {1 Fail-closed wiring for bootstrap / ingress outcomes} *)

type decision =
  | Principal of Principal_identity.principal_id
  | Rejected of { reason : string }
      (** Never a human Principal; does not create store rows. *)

val of_bootstrap : Principal_bootstrap.decision -> decision
(** Map bootstrap trust adapter output. [Anonymous] is always [Rejected];
    [Principal] passes through without inventing store state. *)

val resolve_bootstrap :
  db:Sqlite3.db ->
  provenance:Principal_bootstrap.provenance ->
  ?display:Principal_identity.display_metadata ->
  ?now:float ->
  ?enrolled:(device_id:string -> Principal_identity.principal_id option) ->
  unit ->
  decision
(** Fail-closed bootstrap → Principal resolution with optional store binding.

    - [Direct_session] / [Absent] / raw web claims / expired / forged / revoked
      → [Rejected]
    - [Web_oidc] is rejected before any actor key or store row can be created:
      [Principal_bootstrap] has no configured issuer/JWT/JWKS verifier for raw
      decoded claims.
    - [Cli_enrolled]: returns the enrolled Principal after bootstrap accepts;
      does not invent a Connector actor from device claims alone *)

val display_of_name : string option -> Principal_identity.display_metadata
(** Convenience for ingress Human outcomes that only carry a display name. *)

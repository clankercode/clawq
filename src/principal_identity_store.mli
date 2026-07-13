(** SQLite persistence for Principals, Connector actors, and Identity Links
    (P21.M1.E1.T002).

    Additive schema over {!Principal_identity} domain types. Collision safety is
    enforced by a UNIQUE constraint on the canonical Connector actor identity
    key (and at most one active identity link per actor key). Concurrent
    first-seen creation serializes under IMMEDIATE transactions so only one
    active owner wins.

    Optimistic concurrency: [revision] is a monotonic integer; update helpers
    with [~expected_revision] fail when the stored revision differs (CAS).

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0005-separate-human-principals-from-room-sessions.md. *)

val ensure_schema : Sqlite3.db -> unit
(** Idempotent tables: [principals], [connector_actors], [identity_links]. *)

(** {1 Principals} *)

val insert_principal :
  db:Sqlite3.db ->
  ?now:float ->
  Principal_identity.principal ->
  (Principal_identity.principal, string) result
(** Insert a Principal. Fails if [id] already exists. Fills empty
    [created_at]/[updated_at] from [now] when blank. *)

val get_principal :
  db:Sqlite3.db ->
  id:Principal_identity.principal_id ->
  (Principal_identity.principal option, string) result

val update_principal :
  db:Sqlite3.db ->
  ?expected_revision:int ->
  ?lifecycle:Principal_identity.principal_lifecycle ->
  ?display:Principal_identity.display_metadata ->
  ?now:float ->
  id:Principal_identity.principal_id ->
  unit ->
  (Principal_identity.principal, string) result
(** Bumps [revision]. When [expected_revision] is set and mismatches, returns a
    revision conflict error. *)

(** {1 Connector actors} *)

val insert_connector_actor :
  db:Sqlite3.db ->
  ?now:float ->
  Principal_identity.connector_actor ->
  (Principal_identity.connector_actor, string) result
(** Insert a Connector actor. Collision-safe: rejects when the canonical
    [actor_identity_key] already exists (one active owner). *)

val get_connector_actor :
  db:Sqlite3.db ->
  key:Principal_identity.connector_actor_key ->
  (Principal_identity.connector_actor option, string) result

val get_connector_actor_by_identity_key :
  db:Sqlite3.db ->
  identity_key:string ->
  (Principal_identity.connector_actor option, string) result

val update_connector_actor :
  db:Sqlite3.db ->
  ?expected_revision:int ->
  ?principal_id:Principal_identity.principal_id ->
  ?lifecycle:Principal_identity.actor_lifecycle ->
  ?display:Principal_identity.display_metadata ->
  ?verified_at:string option ->
  ?now:float ->
  key:Principal_identity.connector_actor_key ->
  unit ->
  (Principal_identity.connector_actor, string) result
(** Bumps [revision]. Identity key is immutable and cannot change. *)

(** {1 Identity links} *)

val insert_identity_link :
  db:Sqlite3.db ->
  ?now:float ->
  Principal_identity.identity_link ->
  (Principal_identity.identity_link, string) result
(** Insert an identity link. At most one [Active] link per Connector actor key;
    collision returns an error. Non-active historical links may share an actor
    key. Generates [id] when blank. *)

val get_identity_link :
  db:Sqlite3.db ->
  id:string ->
  (Principal_identity.identity_link option, string) result

val get_active_identity_link :
  db:Sqlite3.db ->
  key:Principal_identity.connector_actor_key ->
  (Principal_identity.identity_link option, string) result

val update_identity_link :
  db:Sqlite3.db ->
  ?expected_revision:int ->
  ?status:Principal_identity.identity_link_status ->
  ?principal_id:Principal_identity.principal_id ->
  ?unlinked_at:string option ->
  ?now:float ->
  id:string ->
  unit ->
  (Principal_identity.identity_link, string) result
(** Bumps [revision]. Activating a link that would collide with another active
    link for the same actor key fails. *)

(** {1 First-seen creation} *)

val create_first_seen :
  db:Sqlite3.db ->
  key:Principal_identity.connector_actor_key ->
  ?principal_id:Principal_identity.principal_id ->
  ?display:Principal_identity.display_metadata ->
  ?verified_at:string ->
  ?now:float ->
  unit ->
  ( Principal_identity.principal
    * Principal_identity.connector_actor
    * Principal_identity.identity_link,
    string )
  result
(** Atomic first-seen path: create Principal + Connector actor + Active identity
    link under one IMMEDIATE transaction. Concurrent inserts for the same actor
    key reject with a collision error (one active owner). *)

(** {1 Listing helpers (merge / admin)} *)

val list_connector_actors_for_principal :
  db:Sqlite3.db ->
  principal_id:Principal_identity.principal_id ->
  (Principal_identity.connector_actor list, string) result
(** All Connector actors currently pointing at [principal_id] (any lifecycle).
*)

val list_active_identity_links_for_principal :
  db:Sqlite3.db ->
  principal_id:Principal_identity.principal_id ->
  (Principal_identity.identity_link list, string) result
(** Active identity links owned by [principal_id]. *)

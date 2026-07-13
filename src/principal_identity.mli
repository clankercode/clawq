(** Versioned Principal, Connector actor, and Identity Link domain model
    (P21.M1.E1.T001).

    A [Principal] is Clawq's durable human identity. A [Connector_actor] is
    verified ingress evidence: Connector + tenant/workspace/account scope +
    immutable Connector user ID. An [Identity_link] binds one actor to one
    Principal.

    Room IDs, Session IDs, and display names are execution/display context only
    — they never establish identity or credential ownership. Equal Connector
    user IDs in different tenants remain distinct actors. Display-name changes
    do not change identity.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0005-separate-human-principals-from-room-sessions.md. *)

val schema_version : int
(** Schema version of Principal identity models; starts at 1. *)

(** {1 Opaque Principal ID} *)

type principal_id
(** Opaque durable Principal identifier. Construct only via
    [principal_id_of_string]. Not a Room ID, Session ID, or display name. *)

val principal_id_of_string : string -> (principal_id, string) result
(** Accept a non-empty trimmed opaque id. Rejects empty/whitespace-only. *)

val principal_id_to_string : principal_id -> string
val principal_id_equal : principal_id -> principal_id -> bool
val principal_id_compare : principal_id -> principal_id -> int

(** {1 Connector + scoped immutable user identity} *)

type connector =
  | Teams
  | Slack
  | Discord
  | Telegram
  | Web  (** Authenticated web issuer/subject bootstrap. *)
  | Cli  (** Authenticated CLI issuer/subject bootstrap. *)
  | Direct  (** Authenticated direct-session issuer/subject bootstrap. *)

val string_of_connector : connector -> string
val connector_of_string : string -> (connector, string) result

type connector_scope = {
  tenant_or_workspace : string;
      (** Tenant, workspace, team, guild, bot-account, or equivalent namespace.
          Equal user IDs under different scopes are distinct actors. *)
  immutable_user_id : string;
      (** Connector-stable human user id established by trusted ingress
          verification. Never a display name, email, Room id, or Session id. *)
}
(** Scoped immutable user identity for one Connector. *)

type connector_actor_key = { connector : connector; scope : connector_scope }
(** Canonical identity key for a Connector actor. Display metadata is
    deliberately absent. *)

val actor_identity_key : connector_actor_key -> string
(** Deterministic identity string:
    ["connector:<name>:tenant:<scope>:user:<id>"]. Lowercases connector name;
    scope and user segments are taken as-is after trim. Does not incorporate
    Room, Session, or display name. *)

val connector_actor_key_equal :
  connector_actor_key -> connector_actor_key -> bool

val make_connector_actor_key :
  connector:connector ->
  tenant_or_workspace:string ->
  immutable_user_id:string ->
  (connector_actor_key, string) result
(** Build a key after rejecting empty tenant/workspace or immutable user id. *)

(** {1 Mutable display metadata (non-identity)} *)

type display_metadata = {
  display_name : string option;
  avatar_url : string option;
  email : string option;
      (** Contact hint only; never used to create or merge Principals. *)
  extra : (string * string) list;  (** Additional non-identity labels. *)
}
(** Mutable presentation fields. Changing any field must not change identity. *)

val empty_display : display_metadata

(** {1 Explicit non-identity execution context} *)

type non_identity_context = {
  room_id : string option;
  session_id : string option;
  display_name : string option;
}
(** Room, Session, and display names carried for routing/UX only. These fields
    are never Principal IDs, never Connector actor keys, and never authorize
    credential ownership. *)

val empty_non_identity_context : non_identity_context

(** {1 Lifecycle and revision} *)

type principal_lifecycle =
  | Active
  | Disabled
  | Merged_into of principal_id
      (** Immutable tombstone after merge; cannot own actors or credentials. *)

type actor_lifecycle =
  | Active
  | Unlinked  (** Split off; live resolution follows the current link. *)
  | Disabled

type identity_link_status =
  | Active
  | Unlinked
  | Superseded  (** Replaced by merge/split or admin repair. *)

val string_of_principal_lifecycle : principal_lifecycle -> string
val string_of_actor_lifecycle : actor_lifecycle -> string
val string_of_identity_link_status : identity_link_status -> string

(** {1 Principal} *)

type principal = {
  version : int;
      (** Must equal [schema_version] for newly constructed values. *)
  id : principal_id;
  lifecycle : principal_lifecycle;
  revision : int;
      (** Monotonic revision for compare-and-swap updates (T002+). *)
  display : display_metadata;  (** Mutable; not identity. *)
  created_at : string;  (** ISO-8601 UTC; durable creation order for merge. *)
  updated_at : string;
}

val make_principal :
  id:principal_id ->
  ?lifecycle:principal_lifecycle ->
  ?revision:int ->
  ?display:display_metadata ->
  ?created_at:string ->
  ?updated_at:string ->
  unit ->
  principal
(** Construct a versioned Principal. Defaults: [Active], revision [1], empty
    display, empty timestamps left for the caller/persistence layer. *)

val principal_is_active : principal -> bool
(** [true] only when lifecycle is [Active]. Tombstones and disabled Principals
    cannot authorize new actor-originated work. *)

val with_principal_display : principal -> display_metadata -> principal
(** Update mutable display metadata only; id and lifecycle are unchanged. *)

(** {1 Connector actor} *)

type connector_actor = {
  version : int;
  key : connector_actor_key;
      (** Immutable identity: connector + tenant/workspace + user id. *)
  principal_id : principal_id;  (** Current owning Principal. *)
  lifecycle : actor_lifecycle;
  revision : int;
  display : display_metadata;  (** Mutable; never part of [key]. *)
  verified_at : string option;
      (** When trusted ingress last established the immutable user id. *)
  created_at : string;
  updated_at : string;
}

val make_connector_actor :
  key:connector_actor_key ->
  principal_id:principal_id ->
  ?lifecycle:actor_lifecycle ->
  ?revision:int ->
  ?display:display_metadata ->
  ?verified_at:string ->
  ?created_at:string ->
  ?updated_at:string ->
  unit ->
  connector_actor

val with_actor_display : connector_actor -> display_metadata -> connector_actor
(** Update display only; [key] (identity) is unchanged. *)

(** {1 Identity link} *)

type identity_link = {
  version : int;
  id : string;  (** Opaque link row id (persistence will assign). *)
  principal_id : principal_id;
  actor_key : connector_actor_key;
  status : identity_link_status;
  revision : int;
  linked_at : string;
  unlinked_at : string option;
}
(** Explicit binding of one Connector actor to one Principal. Cross-Connector
    linking (T004+) creates/updates these after private two-sided proof; this
    module only defines the typed record. *)

val make_identity_link :
  id:string ->
  principal_id:principal_id ->
  actor_key:connector_actor_key ->
  ?status:identity_link_status ->
  ?revision:int ->
  ?linked_at:string ->
  ?unlinked_at:string ->
  unit ->
  identity_link

(** {1 JSON codecs} *)

val display_metadata_to_json : display_metadata -> Yojson.Safe.t

val display_metadata_of_json :
  Yojson.Safe.t -> (display_metadata, string) result

val connector_actor_key_to_json : connector_actor_key -> Yojson.Safe.t

val connector_actor_key_of_json :
  Yojson.Safe.t -> (connector_actor_key, string) result

val principal_to_json : principal -> Yojson.Safe.t
val principal_of_json : Yojson.Safe.t -> (principal, string) result
val connector_actor_to_json : connector_actor -> Yojson.Safe.t
val connector_actor_of_json : Yojson.Safe.t -> (connector_actor, string) result
val identity_link_to_json : identity_link -> Yojson.Safe.t
val identity_link_of_json : Yojson.Safe.t -> (identity_link, string) result
val non_identity_context_to_json : non_identity_context -> Yojson.Safe.t

val non_identity_context_of_json :
  Yojson.Safe.t -> (non_identity_context, string) result
(** Codecs for Room/Session/display context. Presence of these fields on a
    payload never constitutes Principal or Connector identity. *)

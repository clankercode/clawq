(** Immutable per-turn Room Tool catalog (P19.M1.E2.T002).

    Freezes allowed tool references plus canonical identity, aliases, origin,
    MCP server, schema revision, deferred flag, and effective-access revision
    before provider request construction. Distinct catalogs per Room/turn; an
    in-flight turn cannot gain or replace tools after config/MCP reload. *)

type origin = Builtin | Skill | Mcp of string | Other of string

type entry = {
  canonical : string;
  aliases : string list;
  origin : origin;
  mcp_server : string option;
  schema_revision : string;
  deferred : bool;
  description : string;
  parameters_schema : Yojson.Safe.t;
  risk_level : Tool.risk_level;
}

type t = {
  id : string;  (** Catalog instance id (unique per freeze). *)
  revision : string;  (** Content hash of the frozen entries. *)
  access_revision : string;
      (** Effective-access / config revision bound at freeze time. *)
  room_id : string option;
  session_key : string option;
  created_at : string;
  entries : entry list;
}

val schema_revision_of : Yojson.Safe.t -> string
val origin_to_string : origin -> string

val freeze :
  registry:Tool_registry.t ->
  ?allowed_tools:string list ->
  ?denied_tools:string list ->
  ?access_revision:string ->
  ?room_id:string ->
  ?session_key:string ->
  ?now:float ->
  ?id:string ->
  unit ->
  t
(** Capture an immutable catalog filtered by deny-wins [Tool_authz]. When both
    allow and deny lists are empty, all registry tools are frozen. *)

val freeze_from_snapshot :
  registry:Tool_registry.t ->
  snap:Access_snapshot.t ->
  ?room_id:string ->
  ?session_key:string ->
  ?now:float ->
  unit ->
  t

val lookup : t -> string -> entry option
(** Resolve a canonical name or alias within this frozen catalog only. *)

val names : t -> string list
(** Canonical names in freeze order. *)

val contains : t -> string -> bool
val equal_revision : t -> t -> bool
val to_openai_json : t -> Yojson.Safe.t

val to_openai_json_with_search : t -> Yojson.Safe.t
(** Like [Tool_registry.to_openai_json_with_search] but only frozen entries. *)

val entry_count : t -> int

val authorize_invoke : t -> tool_name:string -> (entry, string) result
(** Final execution guard: tool must resolve in this catalog. *)

val search : t -> query:string -> limit:int -> entry list
(** Keyword discovery restricted to frozen entries (deferred preferred). *)

val freeze_for_access :
  registry:Tool_registry.t ->
  ?snap:Access_snapshot.t ->
  ?room_id:string ->
  ?session_key:string ->
  unit ->
  t
(** Convenience: freeze from registry using snapshot allow/deny when present. *)

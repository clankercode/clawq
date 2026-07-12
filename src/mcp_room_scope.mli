(** Room-scoped MCP server access and credential isolation (P19.M1.E3.T002).

    Provider exposure, search, and invocation enforce the frozen mcp_servers
    policy from the in-flight access snapshot. HTTP credentials are leased per
    call; credential-bearing stdio clients are scope-keyed or rejected. Two
    differently granted Rooms cannot see or use each other's servers or
    credentials. *)

type transport = Http | Stdio

type grant = {
  server : string;
  transport : transport;
  credential_handle : string option;
      (** Opaque handle id — never the secret material. *)
}

type room_scope = {
  room_id : string;
  allowed_servers : grant list;
  access_revision : string;
      (** Snapshot config_hash / access revision at freeze. *)
}

type lease = {
  room_id : string;
  server : string;
  credential_handle : string;
  lease_id : string;
  access_revision : string;
}

val make_scope :
  room_id:string ->
  allowed_servers:grant list ->
  access_revision:string ->
  room_scope

val filter_servers : scope:room_scope -> server_names:string list -> string list
(** Exposure/search: only servers granted to this room. *)

val may_invoke : scope:room_scope -> server:string -> (grant, string) result
(** Invocation uses frozen scope (snapshot policy). *)

val lease_http_credential :
  scope:room_scope -> server:string -> (lease, string) result
(** HTTP credentials leased per call; fails if no handle or not granted. *)

val stdio_client_key :
  scope:room_scope -> server:string -> (string, string) result
(** Scope-keyed stdio client key; rejects credential-bearing stdio without a
    handle, or unscoped access. *)

val scopes_isolated : a:room_scope -> b:room_scope -> server:string -> bool
(** True when [server] is not shared between a and b grants (isolation holds for
    that server across the two rooms). *)

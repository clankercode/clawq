(* MCP client with policy-scoped credential resolution *)

type server_config = {
  name : string;
  command : string;
  args : string list;
  env : (string * string) list;
  credential_handle : string option;
      (** Optional credential handle ID. When set, the MCP connection resolves
          credentials through the snapshot-scoped lease API. Missing or
          unauthorized handles deny connection before any network call. *)
}

type io_transport = {
  process : Lwt_process.process_full;
  stderr_drain : unit Lwt.t;
}

type http_transport = {
  url : string;
  headers : (string * string) list;
  post :
    url:string ->
    headers:(string * string) list ->
    body:string ->
    (int * string * string) Lwt.t;
}

type transport = Stdio of io_transport | Http of http_transport

type t = {
  config : server_config;
  transport : transport;
  mutable next_id : int;
  mutable discovered : Tool.t list;
}

val drain_channel : Lwt_io.input_channel -> unit Lwt.t
val server_config_of_json : Yojson.Safe.t -> (server_config, string) result
val load_server_configs : string -> server_config list
val discovered_tools : t -> Tool.t list

val connect :
  ?startup_timeout_s:float ->
  ?http_post:
    (url:string ->
    headers:(string * string) list ->
    body:string ->
    (int * string * string) Lwt.t) ->
  ?config:Runtime_config.t ->
  server_config ->
  t Lwt.t
(** [connect cfg] connects to an MCP server without policy checking. Use
    [connect_with_policy] for snapshot-scoped credential resolution. When
    [config] is provided, MCP tools will resolve room-specific credentials at
    invocation time. *)

val disconnect : t -> unit Lwt.t

val resolve_mcp_server_credentials :
  config:Runtime_config.t ->
  snapshot:Access_snapshot.t ->
  server_config ->
  ((string * string) list, string) result
(** [resolve_mcp_server_credentials ~config ~snapshot cfg] resolves the
    credential handle for an MCP server through the snapshot-scoped lease API.
    Returns [Ok env] with the resolved headers/env vars, or [Error msg] if the
    handle is missing or unauthorized. When [cfg.credential_handle] is [None],
    returns [Ok cfg.env] (legacy path). *)

val connect_with_policy :
  config:Runtime_config.t ->
  snapshot:Access_snapshot.t ->
  ?startup_timeout_s:float ->
  ?http_post:
    (url:string ->
    headers:(string * string) list ->
    body:string ->
    (int * string * string) Lwt.t) ->
  server_config ->
  t Lwt.t
(** [connect_with_policy ~config ~snapshot cfg] connects to an MCP server after
    resolving credentials through policy. If [cfg.credential_handle] is set,
    credentials are resolved through the snapshot-scoped lease API. Missing or
    unauthorized handles deny connection before any network call. *)

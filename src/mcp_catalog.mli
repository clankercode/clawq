(** Canonical MCP tool identities, pagination, list_changed quarantine, and
    metadata limits (P19.M1.E3.T001).

    - Identity is collision-safe [(server, remote_name)] with an explicit
      server/tool revision.
    - [tools/list] pagination is fully drained.
    - [list_changed] immediately quarantines only the affected server/revision
      for new turns before relisting.
    - Failed/malformed/timed-out relists leave the server unavailable until
      successful retry or explicit repair.
    - Removed tools never remain discoverable.
    - Names, descriptions, annotations, schemas have size/depth/control-char
      validation; hostile or colliding definitions fail closed. *)

type identity = { server : string; remote_name : string; revision : string }

type tool_def = {
  identity : identity;
  description : string;
  annotations : Yojson.Safe.t;
  schema : Yojson.Safe.t;
}

type page = { tools : tool_def list; next_cursor : string option }

type server_status =
  | Available of { revision : string; tools : tool_def list }
  | Quarantined of { revision : string; reason : string }
  | Unavailable of { reason : string }

type t
(** Catalog state for one MCP registry view. *)

val max_name_len : int
(** Metadata limits (documented). *)

val max_description_len : int
val max_schema_bytes : int
val max_schema_depth : int
val max_annotations_bytes : int

val identity_key : identity -> string
(** Canonical key: [server ^ "\x1f" ^ remote_name]. *)

val make_identity :
  server:string -> remote_name:string -> revision:string -> identity

val validate_tool_def : tool_def -> (unit, string) result
(** Size/depth/control-character validation. *)

val empty : unit -> t

val apply_pages :
  t -> server:string -> revision:string -> pages:page list -> (t, string) result
(** Drain all pages into an Available server view. Fails closed on collision,
    validation failure, or empty required drain. *)

val list_changed : t -> server:string -> revision:string -> t
(** Immediately quarantine [server]/[revision] for new turns. *)

val mark_relist_failed : t -> server:string -> reason:string -> t
(** Failed/malformed/timeout relist → Unavailable. *)

val repair_server : t -> server:string -> unit -> t
(** Clear quarantine/unavailable for explicit repair (tools empty until relist).
*)

val status : t -> server:string -> server_status option

val discoverable_tools : t -> tool_def list
(** Only tools from Available servers (never quarantined/unavailable/removed).
*)

val is_discoverable : t -> identity:identity -> bool

val can_invoke : t -> identity:identity -> (unit, string) result
(** Concurrent invocation racing quarantine: fail closed if not Available. *)

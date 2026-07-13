(** Room-scoped GitHub read, search, and status tools (P19.M4.E1.T002).

    Pure API over journal/projections. Access is room-local: tools never leak
    items from another Room. Optional auth/installation arguments enforce
    current repository authorization (PAT/App selection). Tool schemas are
    secret-free OpenAI-style definitions for catalog integration (names only).

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type tool_name = Get_item | Search_items | Get_status | List_room_items
type tool_request = { room_id : string; name : tool_name; args : Yojson.Safe.t }

type tool_result =
  | Ok_json of Yojson.Safe.t
  | Denied of string
  | Error of string

val tool_name_to_string : tool_name -> string
(** Canonical catalog tool names. *)

val tool_definitions : unit -> Yojson.Safe.t list
(** OpenAI-style tool schemas for the four tools; secret-free. *)

val dispatch :
  db:Sqlite3.db ->
  request:tool_request ->
  ?auth:Github_auth_selection.auth_snapshot ->
  ?installation:Github_app_installation_scope.t ->
  unit ->
  tool_result
(** [Get_item]: require [item_key] in room projections/journal. [Search_items]:
    query over room projections (title/state/labels). [Get_status]: projection
    status fields. [List_room_items]: list projections. Deny if room has no
    access / empty, or auth rejects the repository. *)

val runtime_tool_names : string list

val runtime_tools : db:Sqlite3.db -> config:Runtime_config.t -> Tool.t list
(** Runtime bindings obtain Room scope and policy only from the immutable
    access snapshot attached to the current turn. *)

val register_runtime_tools :
  db:Sqlite3.db -> config:Runtime_config.t -> Tool_registry.t -> unit

(** Portable tool discovery over a frozen catalog (P19.M1.E2.T004).

    - Providers without native discovery receive authorized eager tools plus
      portable search when deferred tools exist.
    - [search_tools] returns at most five short authorized results.
    - [inspect_tool] reveals one selected schema (+ risk policy metadata).
    - [call_tool] reauthorizes the canonical identity without leaking the broad
      catalog. *)

type short_result = {
  identity : string;  (** Canonical tool identity. *)
  summary : string;  (** Short description (truncated). *)
  deferred : bool;
}

type inspect_result = {
  identity : string;
  description : string;
  parameters_schema : Yojson.Safe.t;
  risk_level : string;
  deferred : bool;
  aliases : string list;
  origin : string;
}

val max_search_results : int
(** Hard cap: 5. *)

val provider_payload :
  catalog:Tool_catalog.t -> prefer_native_search:bool -> Yojson.Safe.t
(** Eager tools always; when deferred tools exist and [prefer_native_search] is
    false, include portable [search_tools]/[inspect_tool]/[call_tool] instead of
    dumping deferred schemas. *)

val search_tools :
  catalog:Tool_catalog.t ->
  query:string ->
  ?limit:int ->
  unit ->
  short_result list
(** At most [min limit max_search_results] authorized short hits. *)

val inspect_tool :
  catalog:Tool_catalog.t -> identity:string -> (inspect_result, string) result
(** One selected schema; fails closed if not in catalog. *)

val call_tool_authorize :
  catalog:Tool_catalog.t ->
  identity:string ->
  (Tool_catalog.entry, string) result
(** Reauthorize canonical identity for dispatch (no catalog dump). *)

val portable_tool_defs : Yojson.Safe.t list
(** OpenAI-shaped defs for search_tools / inspect_tool / call_tool. *)

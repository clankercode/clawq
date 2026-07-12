(** Transactional MCP registry/client publish and reload (P19.M1.E3.T003).

    Build+validate local configuration replacement before atomic publish; on
    failure retain the previously validated state. Remote list_changed uses
    fail-closed quarantine until successful relist/repair; then publish
    additions/changes/removals atomically and mark Rooms for next-turn refresh.
    Final invocation revalidates revision against current published state. *)

type published = {
  catalog : Mcp_catalog.t;
  generation : int;  (** Monotonic publish generation. *)
  rooms_pending_refresh : string list;
}

type t
(** Mutable publisher holding current + building state. *)

val create : unit -> t
val current : t -> published option

val begin_local_replacement : t -> unit
(** Start building a replacement catalog (does not publish yet). *)

val stage_server_pages :
  t ->
  server:string ->
  revision:string ->
  pages:Mcp_catalog.page list ->
  (unit, string) result
(** Stage validated pages into the building catalog. *)

val commit_local_replacement :
  t -> rooms:string list -> (published, string) result
(** Atomic publish of staged state; failure leaves previous current intact. *)

val abort_local_replacement : t -> unit
(** Discard staged state; previous current retained. *)

val on_list_changed : t -> server:string -> revision:string -> unit
(** Quarantine server immediately in the current published catalog. *)

val publish_relist :
  t ->
  server:string ->
  revision:string ->
  pages:Mcp_catalog.page list ->
  rooms:string list ->
  (published, string) result
(** After quarantine: successful relist publishes atomically; on failure mark
    unavailable and retain quarantine/unavailable contract. *)

val revalidate_invoke :
  t -> identity:Mcp_catalog.identity -> (unit, string) result
(** Final invocation guard against current published state. *)

val rooms_needing_refresh : t -> string list
val clear_room_refresh : t -> room_id:string -> unit

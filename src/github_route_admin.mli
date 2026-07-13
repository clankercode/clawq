(** Agent/CLI-facing GitHub route plan, inspect, change, disable, and remove
    (P19.M2.E3.T002).

    Planning produces and stores a [Setup_plan] with
    [apply_payload.kind = Github_route] only — no route mutation. Mutations go
    through [Setup_plan_apply.apply] with [apply_route_ops] as the domain
    adapter.

    Ops JSON (secret-free):
    - [\{"op":"create","destination":"room:R","selector":\{...\},...\}]
    - [\{"op":"update","id":"...","expected_revision":"2",...\}]
    - [\{"op":"disable","id":"...","expected_revision":"2"\}]
    - [\{"op":"remove","id":"...","expected_revision":"2"\}] (soft:
      enabled=false)

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type inspect_view = {
  route : Github_route_store.t;
  summary : string;  (** Channel-safe one-line summary (no secrets). *)
  explain : string list;
      (** No-fallthrough, filter, comment mode, capabilities, managed linkage.
      *)
}

val plan_create :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  destination:Github_route_store.destination ->
  selector:Github_route_store.selector ->
  ?filter:Github_route_store.event_filter ->
  ?comment_mode:Github_route_store.comment_mode ->
  ?capability_policy:Github_route_store.capability_policy ->
  ?enabled:bool ->
  ?route_id:string ->
  ?managed_bundle_id:string ->
  ?managed_feature_id:string ->
  ?on_collision:[ `Reject | `Replace ] ->
  base_revision:string ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Pure plan for creating a route; stores plan as pending; does not mutate
    routes. *)

val plan_update :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  id:string ->
  ?filter:Github_route_store.event_filter ->
  ?comment_mode:Github_route_store.comment_mode ->
  ?capability_policy:Github_route_store.capability_policy ->
  ?enabled:bool ->
  ?expected_revision:string ->
  base_revision:string ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Plan a partial update of an existing route (OCC via [expected_revision]). *)

val plan_disable :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  id:string ->
  ?expected_revision:string ->
  base_revision:string ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Plan [enabled=false] without deleting the row. *)

val plan_remove :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  id:string ->
  ?expected_revision:string ->
  base_revision:string ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Plan soft-remove ([enabled=false]), freeing the active destination+selector
    slot. Hard delete is not used by the store. *)

val inspect : db:Sqlite3.db -> id:string -> (inspect_view, string) result
(** Safe inspect of a stored route (summary + explain; no secrets). *)

val list_inspect_for_destination :
  db:Sqlite3.db ->
  destination:Github_route_store.destination ->
  (inspect_view list, string) result

val list_plans_for_destination :
  db:Sqlite3.db ->
  destination:Github_route_store.destination ->
  ?status:string ->
  unit ->
  Setup_plan.t list
(** Pending (default) [Github_route] plans whose destination context matches.
    Returns [] on query errors. *)

val apply_route_ops :
  db:Sqlite3.db ->
  plan:Setup_plan.t ->
  receipt_id:string ->
  (unit, string) result
(** Domain adapter for [Setup_plan_apply.apply]: interpret [apply_payload.ops]
    JSON and create/update/disable/remove via [Github_route_store]. Idempotent
    on retry with the same plan/receipt. *)

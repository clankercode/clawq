(** Policy-gated GitHub collaboration write intents (P19.M4.E1.T003).

    Authorizes ordinary comment / label / assign mutations against a route's
    [capability_policy]. Planning produces a confirmable [Setup_plan] with
    [apply_payload.kind = Generic "github_collab_action"] only — no live GitHub
    API mutation and no silent write.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type action =
  | Comment of { item_key : string; body : string }
  | Label of { item_key : string; add : string list; remove : string list }
  | Assign of { item_key : string; add : string list; remove : string list }

type decision =
  | Allowed of { action : action; capability : string }
  | Denied of { reason : string }

val authorize : route:Github_route_store.t option -> action:action -> decision
(** Use [capability_policy.allow_reply] for [Comment]; [allow_label] for
    [Label]; [allow_assign] for [Assign]. Deny with reason if route is missing
    or the required capability is false. *)

val plan_action :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  room_id:string ->
  action:action ->
  base_revision:string ->
  ?route:Github_route_store.t ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Create a confirmable [Setup_plan] for a policy-allowed collab action. Stores
    the plan as pending. Does not perform live GitHub mutation. Returns [Error]
    when authorization denies the action or inputs are invalid. *)

val action_item_key : action -> string
(** Extract the target [item_key] from an action. *)

val action_to_json : action -> Yojson.Safe.t
(** Secret-free JSON encoding of an action (ops / planned_state). *)

val capability_for_action : action -> string
(** Capability name required by [action]: ["allow_reply"], ["allow_label"], or
    ["allow_assign"]. *)

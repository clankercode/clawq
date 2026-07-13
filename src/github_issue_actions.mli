(** Confirmed Issue creation and Issue/PR lifecycle actions (P19.M4.E2.T005).

    Implements Issue create / open / close / reopen through the shared
    preview-confirm-apply framework ([Setup_plan] + [Setup_plan_apply]). This
    family is high-risk and App-attributed in P19: it remains off by default
    outside one named, time-bounded pilot gate and must not be presented as
    production-ready. Production enablement waits for P21 [User_required]
    attribution; if P21 user auth is disabled/unavailable there is no App/PAT
    fallback.

    Authorization also requires an explicit route capability:
    - [Create] → ["allow_create"] (route [capability_policy.extra])
    - [Open] / [Close] / [Reopen] → [capability_policy.allow_close]

    Planning produces confirmable [Setup_plan] values with
    [apply_payload.kind = Generic "github_issue_*"] only — no live GitHub API
    mutation. GitHub rejection and projection failures should be surfaced via
    [receipt_safe_error] (projection-safe, secret-free).

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type pilot_gate = {
  enabled : bool;
  pilot_name : string;
  expires_at : string option;
      (** ISO-8601 UTC; [None] = no expiry while enabled *)
}

type action =
  | Create of {
      repo_full_name : string;
      title : string;
      body : string option;
      labels : string list;
    }
  | Open of { item_key : string; comment : string option }
      (** Lifecycle transition to open for an existing Issue/PR. *)
  | Close of {
      item_key : string;
      state_reason : string option;
          (** optional GitHub reason: completed | not_planned | duplicate *)
      comment : string option;
    }
  | Reopen of { item_key : string; comment : string option }

type decision =
  | Allowed of { action : action; capability : string }
  | Denied of { reason : string }

val default_pilot_gate : pilot_gate
(** Off-by-default pilot gate ([enabled = false], name
    ["p19-issue-lifecycle-pilot"]). *)

val action_kind_string : action -> string
(** Stable kind: ["create"] | ["open"] | ["close"] | ["reopen"]. *)

val capability_for_action : action -> string
(** Required capability name: ["allow_create"] for [Create]; ["allow_close"] for
    lifecycle open/close/reopen. *)

val action_target : action -> string
(** Target key shown in plans: repo for create, [item_key] otherwise. *)

val action_to_json : action -> Yojson.Safe.t
(** Secret-free JSON encoding of an action (ops / planned_state). *)

val apply_kind_for_action : action -> string
(** [Setup_plan] generic apply kind: ["github_issue_create"],
    ["github_issue_open"], ["github_issue_close"], or ["github_issue_reopen"].
*)

val is_issue_action_kind : string -> bool
(** True for the four [github_issue_*] apply kinds. *)

val authorize :
  route:Github_route_store.t option ->
  pilot:pilot_gate ->
  user_auth_available:bool ->
  action:action ->
  ?now:float ->
  unit ->
  decision
(** High-risk authorization:

    1. If pilot is not enabled or has expired → deny (not available outside
    pilot; not production-ready). P21 user-auth absence never falls back to
    App/PAT for a production path. 2. When pilot is on: require a route granting
    the action capability ([allow_create] via [extra], or [allow_close]). 3.
    Validate non-empty targets / title. *)

val plan_action :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  room_id:string ->
  pilot:pilot_gate ->
  user_auth_available:bool ->
  action:action ->
  base_revision:string ->
  ?route:Github_route_store.t ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Create a confirmable [Setup_plan] for a policy- and pilot-allowed issue
    action. Stores the plan as pending. Does not perform live GitHub mutation.
    Returns [Error] when authorization denies the action or inputs are invalid.
*)

val plan_create :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  room_id:string ->
  pilot:pilot_gate ->
  user_auth_available:bool ->
  repo_full_name:string ->
  title:string ->
  ?body:string ->
  ?labels:string list ->
  base_revision:string ->
  ?route:Github_route_store.t ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Convenience wrapper for [Create]. *)

val plan_open :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  room_id:string ->
  pilot:pilot_gate ->
  user_auth_available:bool ->
  item_key:string ->
  ?comment:string ->
  base_revision:string ->
  ?route:Github_route_store.t ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result

val plan_close :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  room_id:string ->
  pilot:pilot_gate ->
  user_auth_available:bool ->
  item_key:string ->
  ?state_reason:string ->
  ?comment:string ->
  base_revision:string ->
  ?route:Github_route_store.t ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result

val plan_reopen :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  room_id:string ->
  pilot:pilot_gate ->
  user_auth_available:bool ->
  item_key:string ->
  ?comment:string ->
  base_revision:string ->
  ?route:Github_route_store.t ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result

val receipt_safe_error : string -> string
(** Projection-safe error receipt text: redacts bearer tokens, GitHub PATs, and
    token/secret key=value shapes so GitHub rejection and projection failures
    never embed credentials. *)

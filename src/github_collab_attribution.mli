(** Thin integration of P21 attribution authorize, dispatch lease, and audit
    into P19 collab comment / label / assign metadata write paths
    (P21.M3.E3.T001).

    Comments, labels, assignees, and ordinary metadata are [User_preferred]:
    native user attribution when available, or only the explicitly previewed,
    policy-permitted App fallback. This module:

    + maps collab actions to policy action ids ([comment]/[label]/[assign])
    + runs route capability authorize then {!Github_attribution_authorize}
    + records attribution previews / repair states via
      {!Github_attribution_audit}
    + embeds the frozen prior [Allow] on the collab [Setup_plan]
    + immediately before apply/dispatch, revalidates and issues an opaque lease
      via {!Github_attribution_dispatch_lease}, then records a native receipt

    Issues no raw token material. Live GitHub HTTP remains outside this module
    (callers open [issued.lease] only via
    {!Github_user_token_lease.with_token}).

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Auth = Github_attribution_authorize
module Lease = Github_attribution_dispatch_lease
module Audit = Github_attribution_audit
module Policy = Github_attribution_policy
module Collab = Github_collab_actions
module V = Github_user_token_vault

val schema_version : int
(** Integration helper export schema; starts at 1. *)

val field_attribution_allow : string
(** Plan field holding the frozen redacted prior Allow JSON. *)

val field_requested_mode : string
val field_resolved_mode : string
val field_used_app_fallback : string

(** {1 Action mapping} *)

val policy_action_of_collab : Collab.action -> string
(** Canonical policy id: ["comment"], ["label"], or ["assign"]. *)

val is_user_preferred_metadata : Collab.action -> bool
(** [true] when policy lookup is [User_preferred] (all collab metadata actions).
*)

val request_for_action : action:Collab.action -> Auth.request -> Auth.request
(** Overwrite [evidence.action] with {!policy_action_of_collab}. *)

(** {1 Capability + attribution gate} *)

type gate =
  | Capability_denied of { reason : string }
  | Attribution of {
      capability : string;
      request : Auth.request;
      decision : Auth.decision;
    }

val gate :
  route:Github_route_store.t option ->
  action:Collab.action ->
  evidence:Auth.request ->
  unit ->
  gate
(** Capability authorize (P19 collab) then attribution authorize (P21). Pure;
    issues no audit, lease, or token. *)

(** {1 Stage preview (authorize + audit, no lease)} *)

type staged = {
  action : Collab.action;
  capability : string;
  request : Auth.request;
  allow : Auth.allow;
  preview : Audit.t;
}
(** Successful staged attribution for a collab metadata write. *)

type stage_error =
  | Capability of string
  | Attribution of { deny : Auth.deny; repair : Audit.t option }
  | Audit of string

val string_of_stage_error : stage_error -> string

val stage_preview :
  db:Sqlite3.db ->
  route:Github_route_store.t option ->
  action:Collab.action ->
  evidence:Auth.request ->
  ?item_key:string ->
  ?room_id:string ->
  ?plan_id:string ->
  ?job_id:string ->
  ?actor_snapshot:Actor_snapshot.t ->
  ?github_user_id:int64 ->
  ?now:float ->
  unit ->
  (staged, stage_error) result
(** Run {!gate}. On Allow: persist a [Preview] audit record. On Deny: persist a
    [Repair_state] when possible and return [Error]. Never issues a lease. *)

(** {1 Prior Allow JSON (plan pin)} *)

val allow_to_json : Auth.allow -> Yojson.Safe.t
(** Redacted prior Allow suitable for plan embedding. Never embeds tokens. *)

val allow_of_json : Yojson.Safe.t -> (Auth.allow, string) result
(** Parse a previously embedded prior Allow. *)

val attach_allow_to_plan :
  plan:Setup_plan.t -> allow:Auth.allow -> unit -> Setup_plan.t
(** Embed prior Allow + resolved attribution fields into plan data/planned_state
    and recompute digest. Does not persist. *)

val allow_of_plan : Setup_plan.t -> (Auth.allow option, string) result
(** [Ok None] when absent; [Error] when present but malformed. *)

val has_attribution_allow : Setup_plan.t -> bool

(** {1 Plan + stage together} *)

type planned = { plan : Setup_plan.t; staged : staged }
(** Pending collab plan with frozen attribution Allow and preview audit. *)

val plan_with_attribution :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  room_id:string ->
  action:Collab.action ->
  base_revision:string ->
  evidence:Auth.request ->
  ?route:Github_route_store.t ->
  ?item_key:string ->
  ?job_id:string ->
  ?actor_snapshot:Actor_snapshot.t ->
  ?github_user_id:int64 ->
  ?now:float ->
  unit ->
  (planned, string) result
(** {!stage_preview} then {!Github_collab_actions.plan_action}, attach Allow to
    the plan, and replace the pending plan so digest matches. On attribution
    deny returns the repair message. *)

(** {1 Dispatch (revalidate + lease + receipt)} *)

type dispatched = { issued : Lease.issued; receipt : Audit.t }
(** Dispatch gate passed. [issued.lease] is [Some] only for User mode; open via
    {!Github_user_token_lease.with_token}. App mode has [lease = None]. *)

val prepare_dispatch :
  db:Sqlite3.db ->
  live:Auth.request ->
  prior:Auth.allow ->
  ?vault_id:string ->
  ?expected:V.account_key ->
  ?item_key:string ->
  ?room_id:string ->
  ?plan_id:string ->
  ?receipt_id:string ->
  ?job_id:string ->
  ?actor_snapshot:Actor_snapshot.t ->
  ?github_user_id:int64 ->
  ?now:float ->
  unit ->
  (dispatched, Lease.denial) result
(** Revalidate prior Allow against live evidence, issue opaque lease (User) or
    none (App), record a native [Receipt]. On denial, records a repair/audit
    when possible and returns the lease denial. Never returns raw tokens. *)

val prepare_dispatch_from_plan :
  db:Sqlite3.db ->
  plan:Setup_plan.t ->
  live:Auth.request ->
  ?vault_id:string ->
  ?expected:V.account_key ->
  ?receipt_id:string ->
  ?job_id:string ->
  ?actor_snapshot:Actor_snapshot.t ->
  ?github_user_id:int64 ->
  ?now:float ->
  unit ->
  (dispatched, string) result
(** Load prior Allow from [plan], force live action id from plan collab kind,
    then {!prepare_dispatch}. Maps lease denials to redacted strings. *)

val revoke_issued_lease : Lease.issued -> unit
(** Best-effort revoke of a just-issued user lease (e.g. receipt-only apply with
    no live HTTP). No-op for App path. *)

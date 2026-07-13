(** User-required PR merge attribution (P21.M3.E3.T009).

    Wires {!Github_attribution_authorize}, {!Github_attribution_dispatch_lease},
    and {!Github_attribution_audit} into the independently gated P19 merge
    action family ([merge] — [User_required] critical risk).

    Production path requires a fresh current Principal user lease plus live
    merge-policy validation (head, draft, mergeability, checks, reviews, branch
    policy, method, authority) immediately before dispatch. App/PAT fallback is
    forbidden. The separate P19 pilot App path remains gated by
    {!Github_merge_action} and is not a silent substitute when user auth is
    unavailable.

    Success, denial, and receipt are recorded as secret-free attribution audit
    rows. Changed prerequisites, actor loss, and replay fail closed.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Auth = Github_attribution_authorize
module Dispatch = Github_attribution_dispatch_lease
module Audit = Github_attribution_audit
module Policy = Github_attribution_policy
module Merge = Github_merge_action
module V = Github_user_token_vault

val policy_action_merge : string
(** Canonical policy id ["merge"]. *)

(** {1 Live revalidation (merge policy / target / replay)} *)

type live_revalidation = {
  item_present : bool;  (** Target PR still exists and is addressable. *)
  already_applied : bool;
      (** Duplicate / replay: prior identical merge already applied. *)
  policy : Merge.live_policy;
      (** Fresh live merge-policy snapshot (head/draft/mergeable/checks/...). *)
}

val default_live_revalidation :
  ?policy:Merge.live_policy -> unit -> live_revalidation
(** Defaults: [item_present = true], [already_applied = false]. When [policy] is
    omitted, uses a fail-closed empty head so callers must supply a real
    snapshot. *)

val revalidate_live :
  req:Merge.merge_request -> live:live_revalidation -> (unit, string) result
(** Fail closed on missing item, already-applied, or any live merge-policy
    failure (head/draft/mergeable/checks/reviews/branch/method/authority). Does
    not issue authority. *)

val live_action_evidence :
  req:Merge.merge_request -> live:live_revalidation -> Auth.live_action_evidence
(** Project live revalidation into authorize [live_action] evidence. Revision is
    the live head SHA when present. *)

(** {1 Capability / pilot prechecks} *)

val authorize_capability :
  route:Github_route_store.t option ->
  pilot:Merge.pilot_gate ->
  user_auth_available:bool ->
  req:Merge.merge_request ->
  policy:Merge.live_policy ->
  ?now:float ->
  unit ->
  (unit, string) result
(** Route capability + pilot/P21 user path via {!Merge.authorize_merge}. *)

(** {1 Preview: authorize + audit} *)

type preview_ok = {
  allow : Auth.allow;
  decision : Auth.decision;
  audit : Audit.t;
  policy_action : string;
  used_app_fallback : bool;
  mode : Auth.resolved_mode;
}

type preview_deny = {
  reason : string;
  decision : Auth.decision option;
  audit : Audit.t option;
  policy_action : string;
  failed_check : string option;
  failure_code : string option;
}

val authorize_preview :
  db:Sqlite3.db ->
  route:Github_route_store.t option ->
  pilot:Merge.pilot_gate ->
  user_auth_available:bool ->
  req:Merge.merge_request ->
  auth:Auth.request ->
  live:live_revalidation ->
  ?room_id:string ->
  ?plan_id:string ->
  ?actor_snapshot:Actor_snapshot.t ->
  ?github_user_id:int64 ->
  ?now:float ->
  unit ->
  (preview_ok, preview_deny) result
(** Capability + live merge-policy revalidation + {!Auth.authorize} with action
    forced to ["merge"]. Rejects App-resolved mode (no App fallback on
    production User_required). Persists a [Preview] or [Repair_state] audit row.
    Never issues a lease. *)

(** {1 Dispatch: revalidate + opaque lease + receipt} *)

type dispatch_ok = {
  issued : Dispatch.issued;
  receipt : Audit.t;
  policy_action : string;
  mode : Auth.resolved_mode;
  has_user_lease : bool;
}

type dispatch_deny = {
  reason : string;
  denial : Dispatch.denial option;
  audit : Audit.t option;
  policy_action : string;
}

val dispatch :
  db:Sqlite3.db ->
  req:Merge.merge_request ->
  live_auth:Auth.request ->
  prior:Auth.allow ->
  live:live_revalidation ->
  ?vault_id:string ->
  ?expected:V.account_key ->
  ?room_id:string ->
  ?plan_id:string ->
  ?receipt_id:string ->
  ?actor_snapshot:Actor_snapshot.t ->
  ?github_user_id:int64 ->
  ?now:float ->
  ?ttl_seconds:float ->
  unit ->
  (dispatch_ok, dispatch_deny) result
(** Immediately before HTTP:

    1. Live revalidation (item / replay / merge policy) 2. Final
    {!Dispatch.issue_for_dispatch} against [prior] 3. Mode continuity: requires
    [User] mode + opaque user lease (App/PAT forbidden) 4. Persist a [Receipt]
    (success) or [Repair_state] (deny)

    Never returns raw tokens. *)

val string_of_preview_deny : preview_deny -> string
val string_of_dispatch_deny : dispatch_deny -> string

(** {1 Prior Allow plan pin + plan_with_attribution} *)

val schema_version : int
val field_attribution_allow : string
val allow_to_json : Auth.allow -> Yojson.Safe.t
val allow_of_json : Yojson.Safe.t -> (Auth.allow, string) result

val attach_allow_to_plan :
  plan:Setup_plan.t ->
  allow:Auth.allow ->
  ?live:live_revalidation ->
  unit ->
  Setup_plan.t

val allow_of_plan : Setup_plan.t -> (Auth.allow option, string) result
val has_attribution_allow : Setup_plan.t -> bool

type planned = { plan : Setup_plan.t; preview : preview_ok }

val plan_with_attribution :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  room_id:string ->
  req:Merge.merge_request ->
  policy:Merge.live_policy ->
  base_revision:string ->
  auth:Auth.request ->
  live:live_revalidation ->
  route:Github_route_store.t option ->
  pilot:Merge.pilot_gate ->
  user_auth_available:bool ->
  ?actor_snapshot:Actor_snapshot.t ->
  ?github_user_id:int64 ->
  ?now:float ->
  unit ->
  (planned, string) result
(** Stage attribution preview, build the P19 merge plan, embed frozen prior
    Allow, and re-store pending so digests match. *)

val prepare_dispatch_from_plan :
  db:Sqlite3.db ->
  plan:Setup_plan.t ->
  live_auth:Auth.request ->
  live:live_revalidation ->
  ?vault_id:string ->
  ?expected:V.account_key ->
  ?receipt_id:string ->
  ?actor_snapshot:Actor_snapshot.t ->
  ?github_user_id:int64 ->
  ?now:float ->
  unit ->
  (dispatch_ok, string) result
(** Load prior Allow from plan, revalidate live merge policy, issue lease,
    record receipt. *)

val revoke_issued_lease : Dispatch.issued -> unit
(** Best-effort revoke of a just-issued user lease (receipt-only apply). *)

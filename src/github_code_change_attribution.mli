(** User-required code work and constrained PR creation attribution
    (P21.M3.E3.T008).

    Wires {!Github_attribution_authorize}, {!Github_attribution_dispatch_lease},
    and {!Github_attribution_audit} into the independently gated P19 code-change
    family:

    - [code_work] — confirmed code-changing work
    - [pr_create] — constrained PR creation

    Both normalize to the policy action [code_change] ([User_required] high
    risk). Production path requires a fresh current Principal user lease;
    App/PAT fallback is forbidden. The separate P19 pilot App path remains gated
    by {!Github_code_change_action} and is not a silent substitute when user
    auth is unavailable.

    Live revalidation covers repository/base presence, constrained PR head/base
    refs, cancelled/failed/stale code-work results, and duplicate/replay.
    Success, denial, and receipt are recorded as secret-free attribution audit
    rows. Personal tokens never reach runner, shell, prompt, worktree, Git
    transport, or scheduled ambient work (lease is opaque; receipt-only path
    revokes immediately).

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Auth = Github_attribution_authorize
module Dispatch = Github_attribution_dispatch_lease
module Audit = Github_attribution_audit
module Policy = Github_attribution_policy
module Code = Github_code_change_action
module V = Github_user_token_vault

val policy_action : string
(** Canonical policy id ["code_change"]. *)

(** {1 Action family} *)

type family =
  | Code_work of Code.code_work_request
  | Pr_create of Code.pr_create_request

val policy_action_of_family : family -> string
(** Always ["code_change"] (aliases [code_work] / [pr_create] normalize here).
*)

val item_key_of_family : family -> string option
(** Optional item correlation from the request. *)

val family_label : family -> string
(** ["code_work"] or ["pr_create"] for diagnostics / receipt text. *)

(** {1 Live revalidation (repo / base / refs / result / replay)} *)

type live_revalidation = {
  repo_present : bool;  (** Target repository still addressable. *)
  base_present : bool;  (** Base branch still addressable. *)
  already_applied : bool;
      (** Duplicate / replay: prior identical action already applied. *)
  current_refs : Code.live_refs option;
      (** Live head/base snapshot for [Pr_create] pre-dispatch revalidation. *)
  code_work_result_status : Code.result_status option;
      (** Optional code-work outcome status (cancelled / failed / stale). *)
  code_work_finished_at : string option;
      (** ISO-8601 finish time for stale-result guard. *)
  max_age_seconds : float option;
      (** When set with [code_work_finished_at], enforce freshness. *)
  target_revision : string option;
      (** Live target revision (head SHA / worktree tip). *)
  planned_target_revision : string option;
      (** Planned revision pinned at preview; mismatch is stale target. *)
}

val default_live_revalidation : live_revalidation
(** Presence true, [already_applied = false], optional fields open. *)

val revalidate_live :
  family:family -> live:live_revalidation -> (unit, string) result
(** Fail closed on missing repo/base, already-applied, PR ref mismatch, unusable
    code-work result, or stale target revision. Does not issue authority. *)

val live_action_evidence :
  family:family -> live:live_revalidation -> Auth.live_action_evidence
(** Project live revalidation into authorize [live_action] evidence. *)

(** {1 Capability / pilot prechecks} *)

val authorize_capability :
  family:family ->
  route:Github_route_store.t option ->
  pilot:Code.pilot_gate ->
  user_auth_available:bool ->
  ?now:float ->
  unit ->
  (unit, string) result
(** Route capability + pilot/P21 user path via {!Code.authorize_code_work} /
    {!Code.authorize_pr_create}. *)

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
  family:family ->
  route:Github_route_store.t option ->
  pilot:Code.pilot_gate ->
  user_auth_available:bool ->
  auth:Auth.request ->
  live:live_revalidation ->
  ?item_key:string ->
  ?room_id:string ->
  ?plan_id:string ->
  ?actor_snapshot:Actor_snapshot.t ->
  ?github_user_id:int64 ->
  ?now:float ->
  unit ->
  (preview_ok, preview_deny) result
(** Capability + live revalidation + {!Auth.authorize} with action forced to
    [code_change]. Rejects App-resolved mode (no App fallback on production
    User_required). Persists a [Preview] or [Repair_state] audit row. Never
    issues a lease. *)

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
  family:family ->
  live_auth:Auth.request ->
  prior:Auth.allow ->
  live:live_revalidation ->
  ?vault_id:string ->
  ?expected:V.account_key ->
  ?item_key:string ->
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

    1. Live revalidation (repo / base / refs / result / replay / stale) 2. Final
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
  family:family ->
  base_revision:string ->
  auth:Auth.request ->
  live:live_revalidation ->
  route:Github_route_store.t option ->
  pilot:Code.pilot_gate ->
  user_auth_available:bool ->
  ?actor_snapshot:Actor_snapshot.t ->
  ?github_user_id:int64 ->
  ?now:float ->
  unit ->
  (planned, string) result
(** Stage attribution preview, build the P19 plan, embed frozen prior Allow, and
    re-store pending so digests match. *)

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
(** Load prior Allow from plan, revalidate live state, issue lease, record
    receipt. *)

val revoke_issued_lease : Dispatch.issued -> unit
(** Best-effort revoke of a just-issued user lease (receipt-only apply). *)

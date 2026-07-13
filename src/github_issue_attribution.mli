(** User-required Issue creation and Issue/PR lifecycle attribution
    (P21.M3.E3.T006).

    Wires {!Github_attribution_authorize}, {!Github_attribution_dispatch_lease},
    and {!Github_attribution_audit} into the P19 issue action family:

    - [issue_create] — Issue creation ([Create]) and lifecycle open ([Open],
      matrix alias of create)
    - [issue_close] — close an existing Issue/PR
    - [issue_reopen] — reopen an existing Issue/PR

    All are [User_required] high-risk: current Principal user lease and required
    confirmation only; App/PAT fallback is forbidden on the production path. The
    separate P19 pilot App path remains gated by {!Github_issue_actions} and is
    not a silent substitute when user auth is unavailable.

    Live revalidation covers target presence, state continuity, stale target
    revision, and duplicate/replay. Success, denial, and receipt are recorded as
    secret-free attribution audit rows.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Auth = Github_attribution_authorize
module Dispatch = Github_attribution_dispatch_lease
module Audit = Github_attribution_audit
module Policy = Github_attribution_policy
module Issue = Github_issue_actions
module V = Github_user_token_vault

val policy_action_create : string
(** Canonical policy id ["issue_create"]. *)

val policy_action_close : string
(** Canonical policy id ["issue_close"]. *)

val policy_action_reopen : string
(** Canonical policy id ["issue_reopen"]. *)

(** {1 Action mapping} *)

val policy_action_of_action : Issue.action -> string
(** Map [Create]/[Open] → [issue_create], [Close] → [issue_close], [Reopen] →
    [issue_reopen]. *)

val item_key_of_action : Issue.action -> string option
(** [None] for [Create] (repo-scoped); [Some item_key] for lifecycle actions. *)

(** {1 Live revalidation (target / state / replay)} *)

type live_revalidation = {
  item_present : bool;
      (** Target Issue/PR still exists (lifecycle). Ignored for [Create]. *)
  repo_present : bool;  (** Target repo still exists/addressable ([Create]). *)
  current_state : string option;
      (** Live state: ["open"] | ["closed"] (lowercase). *)
  already_applied : bool;
      (** Duplicate / replay: prior identical action already applied. *)
  target_revision : string option;
      (** Live target revision (updated_at / state_revision). *)
  planned_target_revision : string option;
      (** Planned revision pinned at preview; mismatch is stale target. *)
}

val default_live_revalidation : live_revalidation
(** [item_present = true], [repo_present = true], others open. *)

val revalidate_live :
  action:Issue.action -> live:live_revalidation -> (unit, string) result
(** Fail closed on missing target/repo, already-applied, wrong state for
    close/reopen/open, or stale target revision. Does not issue authority. *)

val live_action_evidence :
  action:Issue.action -> live:live_revalidation -> Auth.live_action_evidence
(** Project live revalidation into authorize [live_action] evidence. *)

(** {1 Capability / pilot prechecks} *)

val authorize_capability :
  action:Issue.action ->
  route:Github_route_store.t option ->
  pilot:Issue.pilot_gate ->
  user_auth_available:bool ->
  ?now:float ->
  unit ->
  (unit, string) result
(** Route capability + pilot/P21 user path via {!Issue.authorize}. *)

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
  action:Issue.action ->
  route:Github_route_store.t option ->
  pilot:Issue.pilot_gate ->
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
(** Capability + live revalidation + {!Auth.authorize} with action forced to the
    policy id. Rejects App-resolved mode (no App fallback on production
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
  action:Issue.action ->
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

    1. Live revalidation (target / state / replay / stale revision) 2. Final
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
  action:Issue.action ->
  base_revision:string ->
  auth:Auth.request ->
  live:live_revalidation ->
  route:Github_route_store.t option ->
  pilot:Issue.pilot_gate ->
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

(** Attributed reviewer requests and user-required PR review submission
    (P21.M3.E3.T002).

    Wires {!Github_attribution_authorize}, {!Github_attribution_dispatch_lease},
    and {!Github_attribution_audit} into the P19 PR review action families:

    - [review_request] (request reviewers) — [User_preferred] ordinary metadata:
      native user lease when available, or only an explicitly previewed
      policy-permitted App fallback after target and selected-reviewer
      revalidation.
    - [review_submit] (PR review submission / decisions) — [User_required]
      high-risk: current Principal user lease and required confirmation only;
      App/PAT fallback is forbidden on the production path. The separate P19
      pilot App path remains gated by {!Github_pr_review_actions} and is not a
      silent substitute when user auth is unavailable.

    Live revalidation covers head SHA, self-review, selected reviewers, missing
    item, and duplicate/replay. Success, denial, and receipt are recorded as
    secret-free attribution audit rows.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Auth = Github_attribution_authorize
module Dispatch = Github_attribution_dispatch_lease
module Audit = Github_attribution_audit
module Policy = Github_attribution_policy
module Review = Github_pr_review_actions
module V = Github_user_token_vault

val policy_action_request_reviewers : string
(** Canonical policy id ["review_request"]. *)

val policy_action_submit_review : string
(** Canonical policy id ["review_submit"]. *)

(** {1 Action family} *)

type family =
  | Request_reviewers of Review.request_reviewers
  | Submit_review of Review.submit_review

val policy_action_of_family : family -> string
val item_key_of_family : family -> string

(** {1 Live revalidation (target / reviewers / head / self / replay)} *)

type live_revalidation = {
  head_sha_live : string option;
      (** Current PR head SHA from GitHub. Required for [Submit_review]. *)
  pr_author_login : string option;
      (** PR author login for self-approve guard. *)
  reviewers_still_valid : bool;
      (** Selected reviewers still valid on the target (request path). *)
  already_applied : bool;
      (** Duplicate / replay: prior identical submission already applied. *)
  item_present : bool;  (** Target PR/item still exists and is addressable. *)
}

val default_live_revalidation : live_revalidation
(** [item_present = true], [reviewers_still_valid = true], others open. *)

val revalidate_live :
  family:family -> live:live_revalidation -> (unit, string) result
(** Fail closed on missing item, invalid reviewers, stale head, self-review
    Approve, or duplicate/replay. Does not issue authority. *)

val live_action_evidence :
  family:family -> live:live_revalidation -> Auth.live_action_evidence
(** Project live revalidation into authorize [live_action] evidence (ok/detail
    revision = planned head when present). *)

(** {1 Capability / pilot prechecks} *)

val authorize_capability :
  family:family ->
  route:Github_route_store.t option ->
  pilot:Review.pilot_gate ->
  user_auth_available:bool ->
  ?now:float ->
  unit ->
  (unit, string) result
(** Route capability ([allow_review]) plus family-specific rules. For
    [Submit_review]: pilot App interim when the pilot is enabled, else require
    [user_auth_available] for the P21 User_required path (no App/PAT fallback
    when user auth is off). *)

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
  pilot:Review.pilot_gate ->
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
    family policy id. On [Submit_review], rejects App-resolved mode (no App
    fallback on production User_required). Persists a [Preview] (allow /
    fallback) or [Repair_state] (deny) audit row. Never issues a lease. *)

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

    1. Live revalidation (head / self / reviewers / replay) 2. Final
    {!Dispatch.issue_for_dispatch} against [prior] 3. Mode continuity:
    [Submit_review] requires [User] mode + opaque user lease (App/PAT
    forbidden); [Request_reviewers] accepts User lease or App without user lease
    only when [prior.used_app_fallback] or resolved App under User_preferred 4.
    Persist a [Receipt] (success) or [Repair_state] (deny)

    Never returns raw tokens. *)

val string_of_preview_deny : preview_deny -> string

val string_of_dispatch_deny : dispatch_deny -> string
(** Redacted one-line summaries. *)

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
  pilot:Review.pilot_gate ->
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

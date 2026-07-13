(** Shared revision-bound plan → confirm → apply for GitHub mutating actions
    (P19.M4.E2.T001 / T003 / T005 / T006; actor attribution P21.M1.E3.T002; P21
    collab/PR/issue attribution P21.M3.E3.T001 / T002 / T006).

    Unifies collab (comment / label / assign), PR review (request_reviewers /
    submit_review), independently gated merge, Issue create/open/close/reopen,
    and typed workflow_dispatch into one preview/confirm/apply path over
    [Setup_plan] + [Setup_plan_apply]. Preview authorizes and stores a pending
    plan that shows target and effects; apply rechecks digest, principal,
    expiry, and [base_revision], then records a receipt only (no live GitHub
    mutation in this task). Merge may also revalidate a fresh [live_policy] when
    supplied.

    When [actor_key] or [actor_snapshot] is supplied at preview, an immutable
    [Actor_snapshot] is pinned onto the plan (intent / confirmation envelope).
    Confirm/apply re-resolves live Principal, identity link, and account
    lineage; actor, link, account, target, or policy changes invalidate
    confirmation. Room history cannot supply identity. On first successful
    apply, the initiating snapshot and requested/resolved attribution are
    attached to a durable receipt correlation for webhook reconcile
    (P21.M1.E3.T006); the snapshot is never reusable authority.

    Optional [attribution_evidence] stages P21 attribution (authorize + audit
    preview + frozen prior Allow) for collab via [Github_collab_attribution] and
    for PR review via [Github_pr_review_attribution] ([request_reviewers]
    User_preferred; [submit_review] User_required with no App/PAT fallback).
    Apply then requires matching [attribution_live] (and [review_live] for PR
    review families) to issue an opaque lease and record a native attribution
    receipt.
    Optional [attribution_evidence] on collab preview stages P21 user-preferred
    attribution (authorize + audit preview + frozen prior Allow on the plan) via
    [Github_collab_attribution]. Apply then requires matching [attribution_live]
    evidence to revalidate, issue an opaque lease, and record a native
    attribution receipt before the Setup_plan apply receipt.

    Optional [attribution_evidence] on PR review preview stages P21 attribution
    (authorize + audit preview + frozen prior Allow) via
    [Github_pr_review_attribution]: [request_reviewers] is User_preferred;
    [submit_review] is User_required with no App/PAT fallback. Apply then
    requires matching [attribution_live] (+ [review_live] revalidation) to issue
    an opaque lease and record a native attribution receipt.
    preview + frozen prior Allow on the plan):

    - collab → [Github_collab_attribution] (User_preferred)
    - PR review → [Github_pr_review_attribution] (request User_preferred; submit
      User_required)
    - issue create/lifecycle → [Github_issue_attribution] (User_required)

    Apply then requires matching [attribution_live] (plus [review_live] /
    [issue_live] where applicable) to revalidate, issue an opaque lease, and
    record a native attribution receipt before the Setup_plan apply receipt.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md,
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md, and
    docs/adr/0003-require-plan-confirm-apply-for-agent-setup.md. *)

type action_kind =
  | Collab of Github_collab_actions.action
  | Request_reviewers of Github_pr_review_actions.request_reviewers
  | Submit_review of Github_pr_review_actions.submit_review
  | Merge of {
      req : Github_merge_action.merge_request;
      policy : Github_merge_action.live_policy;
    }
  | Issue of Github_issue_actions.action
  | Workflow_dispatch of Github_workflow_dispatch.request

val preview :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  room_id:string ->
  action:action_kind ->
  base_revision:string ->
  ?route:Github_route_store.t ->
  ?pilot:Github_pr_review_actions.pilot_gate ->
  ?merge_pilot:Github_merge_action.pilot_gate ->
  ?issue_pilot:Github_issue_actions.pilot_gate ->
  ?workflow_pilot:Github_workflow_dispatch.pilot_gate ->
  ?user_auth_available:bool ->
  ?actor_key:Principal_identity.connector_actor_key ->
  ?actor_snapshot:Actor_snapshot.t ->
  ?account_binding_id:string ->
  ?session_id:string ->
  ?attribution_evidence:Github_attribution_authorize.request ->
  ?review_live:Github_pr_review_attribution.live_revalidation ->
  ?github_user_id:int64 ->
  ?issue_live:Github_issue_attribution.live_revalidation ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Authorize + build plan; store as pending; do not apply.

    Preview shows the target ([item_key] / repo / head SHA / workflow ref where
    relevant) and planned effects in [Setup_plan] diff / planned_state. Confirm
    is required before [apply_confirmed].

    Optional [actor_key] / [actor_snapshot] pins initiating attribution via
    [Github_action_actor_attribution]. [room_id] / [session_id] are source
    context only.

    Optional [attribution_evidence] runs collab
    [Github_collab_attribution.plan_with_attribution] or PR review
    [Github_pr_review_attribution.plan_with_attribution] (with [review_live]
    revalidation for head / self-review / reviewers / replay).
    Optional [attribution_evidence] (collab only) runs
    [Github_collab_attribution.plan_with_attribution]: capability + P21
    authorize, audit preview, and frozen prior Allow on the plan.

    Optional [attribution_evidence] (PR review families) runs
    [Github_pr_review_attribution.plan_with_attribution] with [review_live]
    revalidation (head / self-review / reviewers / replay).
    Optional [attribution_evidence] stages family-specific P21 attribution
    (collab / PR review / issue). [review_live] and [issue_live] supply live
    revalidation inputs for those families.

    [pilot] gates PR submit_review; [merge_pilot] gates merge; [issue_pilot]
    gates Issue create/lifecycle; [workflow_pilot] gates workflow_dispatch. All
    high-risk pilots default off. *)

val apply_confirmed :
  db:Sqlite3.db ->
  plan_id:string ->
  digest:string ->
  principal:Setup_plan.principal ->
  current_base_revision:string ->
  ?current_merge_policy:Github_merge_action.live_policy ->
  ?current_target:Github_action_actor_attribution.target_fingerprint ->
  ?attribution_live:Github_attribution_authorize.request ->
  ?review_live:Github_pr_review_attribution.live_revalidation ->
  ?issue_live:Github_issue_attribution.live_revalidation ->
  ?vault_id:string ->
  ?expected_account:Github_user_token_vault.account_key ->
  ?github_user_id:int64 ->
  ?now:float ->
  unit ->
  (Setup_plan_apply.outcome, string) result
(** Confirm/apply a pending GitHub action plan.

    Uses [Setup_plan_apply.apply] with an adapter that records the apply receipt
    only (no live GitHub API). Rechecks plan id + digest, principal, expiry,
    destination room, and [current_base_revision] against the plan's
    [base_revision]. When an [Actor_snapshot] is pinned on the plan, re-resolves
    live authority and optionally compares [current_target] before apply. Merge
    plans optionally revalidate [current_merge_policy].

    Collab or PR review plans with staged [attribution_allow] require
    [attribution_live] (and [vault_id] for User mode); PR review also uses
    [review_live] revalidation to issue an opaque lease and record a native
    attribution receipt before the Setup_plan apply receipt. Returns [Error]
    only for structural issues (e.g. missing destination room on the stored
    plan); domain rejects are [Ok (Rejected _)]. *)
    Collab plans with staged [attribution_allow] require [attribution_live] (and
    [vault_id] for User mode) to revalidate and issue an opaque lease, record a
    native attribution receipt, then continue with receipt-only apply. Returns
    [Error] only for structural issues (e.g. missing destination room on the
    stored plan); domain rejects are [Ok (Rejected _)]. *)
    plans optionally revalidate [current_merge_policy]. Returns [Error] only for
    structural issues (e.g. missing destination room on the stored plan); domain
    rejects are [Ok (Rejected _)]. *)

    PR review plans with staged [attribution_allow] require [attribution_live]
    (and [vault_id] for User mode) plus [review_live] revalidation to issue an
    opaque lease and record a native attribution receipt before the Setup_plan
    apply receipt. Returns [Error] only for structural issues (e.g. missing
    destination room on the stored plan); domain rejects are [Ok (Rejected _)].
*)
    Plans with staged [attribution_allow] require [attribution_live] (and
    [vault_id] for User mode) plus family live revalidation to issue an opaque
    lease and record a native attribution receipt before the Setup_plan apply
    receipt. Returns [Error] only for structural issues (e.g. missing

val is_github_action_plan : Setup_plan.t -> bool
(** True when [apply_payload.kind] is a GitHub collab / request_reviewers /
    submit_review / merge / github_issue_* / workflow_dispatch generic kind. *)

val action_kind_label : action_kind -> string
(** Short stable label for diagnostics: ["collab"], ["request_reviewers"],
    ["submit_review"], ["merge"], ["issue"], or ["workflow_dispatch"]. *)

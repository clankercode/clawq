(** Shared revision-bound plan → confirm → apply for GitHub mutating actions
    (P19.M4.E2.T001 / T003 / T005 / T006).

    Unifies collab (comment / label / assign), PR review (request_reviewers /
    submit_review), independently gated merge, Issue create/open/close/reopen,
    and typed workflow_dispatch into one preview/confirm/apply path over
    [Setup_plan] + [Setup_plan_apply]. Preview authorizes and stores a pending
    plan that shows target and effects; apply rechecks digest, principal,
    expiry, and [base_revision], then records a receipt only (no live GitHub
    mutation in this task). Merge may also revalidate a fresh [live_policy] when
    supplied.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md and
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
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Authorize + build plan; store as pending; do not apply.

    Preview shows the target ([item_key] / repo / head SHA / workflow ref where
    relevant) and planned effects in [Setup_plan] diff / planned_state. Confirm
    is required before [apply_confirmed].

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
  ?now:float ->
  unit ->
  (Setup_plan_apply.outcome, string) result
(** Confirm/apply a pending GitHub action plan.

    Uses [Setup_plan_apply.apply] with an adapter that records the apply receipt
    only (no live GitHub API). Rechecks plan id + digest, principal, expiry,
    destination room, and [current_base_revision] against the plan's
    [base_revision]. Merge plans optionally revalidate [current_merge_policy].
    Returns [Error] only for structural issues (e.g. missing destination room on
    the stored plan); domain rejects are [Ok (Rejected _)]. *)

val is_github_action_plan : Setup_plan.t -> bool
(** True when [apply_payload.kind] is a GitHub collab / request_reviewers /
    submit_review / merge / github_issue_* / workflow_dispatch generic kind. *)

val action_kind_label : action_kind -> string
(** Short stable label for diagnostics: ["collab"], ["request_reviewers"],
    ["submit_review"], ["merge"], ["issue"], or ["workflow_dispatch"]. *)

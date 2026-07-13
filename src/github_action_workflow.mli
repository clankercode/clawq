(** Shared revision-bound plan → confirm → apply for GitHub mutating actions
    (P19.M4.E2.T001 / P19.M4.E2.T005).

    Unifies collab (comment / label / assign), PR review (request_reviewers /
    submit_review), and Issue create/open/close/reopen into one
    preview/confirm/apply path over [Setup_plan] + [Setup_plan_apply]. Preview
    authorizes and stores a pending plan that shows target and effects; apply
    rechecks digest, principal, expiry, and [base_revision], then records a
    receipt only (no live GitHub mutation in this task).

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md and
    docs/adr/0003-require-plan-confirm-apply-for-agent-setup.md. *)

type action_kind =
  | Collab of Github_collab_actions.action
  | Request_reviewers of Github_pr_review_actions.request_reviewers
  | Submit_review of Github_pr_review_actions.submit_review
  | Issue of Github_issue_actions.action

val preview :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  room_id:string ->
  action:action_kind ->
  base_revision:string ->
  ?route:Github_route_store.t ->
  ?pilot:Github_pr_review_actions.pilot_gate ->
  ?issue_pilot:Github_issue_actions.pilot_gate ->
  ?user_auth_available:bool ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Authorize + build plan; store as pending; do not apply.

    Preview shows the target ([item_key] / repo / head SHA where relevant) and
    planned effects in [Setup_plan] diff / planned_state. Confirm is required
    before [apply_confirmed].

    [pilot] applies to PR review submission; [issue_pilot] applies to Issue
    create/lifecycle. Both default off. *)

val apply_confirmed :
  db:Sqlite3.db ->
  plan_id:string ->
  digest:string ->
  principal:Setup_plan.principal ->
  current_base_revision:string ->
  ?now:float ->
  unit ->
  (Setup_plan_apply.outcome, string) result
(** Confirm/apply a pending GitHub action plan.

    Uses [Setup_plan_apply.apply] with an adapter that records the apply receipt
    only (no live GitHub API). Rechecks plan id + digest, principal, expiry,
    destination room, and [current_base_revision] against the plan's
    [base_revision]. Returns [Error] only for structural issues (e.g. missing
    destination room on the stored plan); domain rejects are [Ok (Rejected _)].
*)

val is_github_action_plan : Setup_plan.t -> bool
(** True when [apply_payload.kind] is a GitHub collab / request_reviewers /
    submit_review / github_issue_* generic kind. *)

val action_kind_label : action_kind -> string
(** Short stable label for diagnostics: ["collab"], ["request_reviewers"],
    ["submit_review"], or ["issue"]. *)

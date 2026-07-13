(** Confirmed code-changing work and constrained PR creation (P19.M4.E2.T007).

    High-risk App-attributed actions available only inside a named, time-bounded
    P19 pilot gate that is off by default. Outside that pilot they are denied
    and must not be presented as production-ready. Production availability waits
    for P21 [User_required] attribution; if P21 user auth is
    disabled/unavailable there is no App/PAT fallback.

    Capability: route [capability_policy.extra] key ["code_change"] (bool),
    independent of write/review/merge and defaults off when absent.

    {2 Code-changing work}

    A fresh confirmed plan must name repository, base branch, scope, runner, and
    output authority. Planning produces confirmable [Setup_plan] values only —
    no live runner dispatch or GitHub mutation.

    {2 Constrained PR creation}

    PR creation accepts only:
    - an explicitly supplied head branch under the constrained branch prefix, or
    - the head branch/result produced by a confirmed code-work operation that
      succeeded and was not cancelled/stale.

    Title is required. Head and base are revalidated before dispatch. Branch
    naming must stay under [branch_prefix] (default ["clawq/"]), must not equal
    base, and rejects [..] / spaces / empty segments.

    Cancellation, stale result, duplicate invocation, runner failure, receipt,
    webhook correlation, and disabled rollout gate are independently expressible
    and testable. No live GitHub mutation at plan time.

    Concepts parallel [Github_work_item] (runner pref, publication branch,
    result status) but this module stays independent.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type pilot_gate = {
  enabled : bool;
  pilot_name : string;
  expires_at : string option;
      (** ISO-8601 UTC; [None] = no expiry while enabled *)
}

(** Confirmed code-work outcome status (work-item-like, independent type). *)
type result_status = Succeeded | Failed | Cancelled | Running

(** Where the PR head branch comes from. *)
type head_source =
  | Explicit_branch of string
      (** Caller-supplied head; must satisfy branch constraints. *)
  | Confirmed_code_work of {
      code_work_plan_id : string;
      head_branch : string;
      head_sha : string;
      status : result_status;
      finished_at : string option;
          (** ISO-8601; used for freshness / stale checks when present *)
    }

type code_work_request = {
  repo_full_name : string;  (** [owner/repo] *)
  base_branch : string;
  scope : string;  (** human-readable change scope for the plan *)
  runner : string;  (** runner preference / identity (no credentials) *)
  output_authority : string;
      (** who may publish results / open a PR from this work *)
  branch_prefix : string;
      (** constrained namespace; default [default_branch_prefix] *)
  head_branch : string option;
      (** optional pre-declared head under [branch_prefix] *)
  item_key : string option;
  related_issue : int option;
}

type pr_create_request = {
  repo_full_name : string;
  base_branch : string;
  title : string;  (** required non-empty *)
  body : string option;
  draft : bool;
  head : head_source;
  branch_prefix : string;
  head_sha : string option;
      (** optional exact head SHA for revalidation; when set must match live *)
  item_key : string option;
}

type live_refs = {
  head_branch : string;
  base_branch : string;
  head_sha : string;
  base_sha : string option;
  head_exists : bool;
  base_exists : bool;
}
(** Live head/base snapshot for pre-dispatch revalidation. Callers obtain this
    from GitHub immediately before use; this module never fabricates refs. *)

val capability_key : string
(** Extra capability policy key: ["code_change"]. *)

val default_branch_prefix : string
(** Restricted publication namespace: ["clawq/"]. *)

val default_pilot_gate : pilot_gate
(** Off-by-default pilot gate ([enabled = false]). *)

val result_status_to_string : result_status -> string
val result_status_of_string : string -> (result_status, string) result

val has_code_change_capability : Github_route_store.capability_policy -> bool
(** True when [extra] contains [(capability_key, true)]. Absent or false →
    denied (defaults off, like [allow_merge]). *)

val validate_branch_name : ?prefix:string -> string -> (string, string) result
(** Reject empty, spaces, [..], leading/trailing [/], and (when [prefix] is set)
    branches that do not start with that prefix. Returns trimmed name. *)

val authorize_code_work :
  route:Github_route_store.t option ->
  pilot:pilot_gate ->
  user_auth_available:bool ->
  req:code_work_request ->
  ?now:float ->
  unit ->
  (unit, string) result
(** Authorize code-changing work:

    1. Pilot enabled and unexpired (else deny; no App/PAT production fallback).
    2. Route grants extra capability [code_change]. 3. Non-empty
    [repo_full_name] (owner/repo), [base_branch], [scope], [runner],
    [output_authority]. 4. Optional [head_branch] must pass
    [validate_branch_name ~prefix]. *)

val authorize_pr_create :
  route:Github_route_store.t option ->
  pilot:pilot_gate ->
  user_auth_available:bool ->
  req:pr_create_request ->
  ?now:float ->
  unit ->
  (unit, string) result
(** Authorize constrained PR creation:

    1. Pilot + capability (same as code work). 2. Non-empty repo, base, and
    required [title]. 3. Head source is either [Explicit_branch] under prefix,
    or [Confirmed_code_work] that [Succeeded], is not cancelled, and whose head
    branch matches constraints and equals the planned head. 4. Head branch ≠
    base branch. *)

val revalidate_pr_refs :
  planned_head:string ->
  planned_base:string ->
  ?planned_head_sha:string ->
  current:live_refs ->
  unit ->
  (unit, string) result
(** Immediate pre-dispatch revalidation of head/base. Fail closed if head or
    base missing, branch names differ, or optional planned head SHA mismatches
    live head SHA. *)

val check_code_work_result_usable :
  result_status:result_status ->
  ?finished_at:string ->
  ?max_age_seconds:float ->
  ?now:float ->
  unit ->
  (unit, string) result
(** Independent failure-path helper: deny [Failed], [Cancelled], [Running];
    optionally treat finished results older than [max_age_seconds] as stale. *)

val check_not_duplicate_invocation :
  already_applied:bool -> (unit, string) result
(** Independent failure-path helper: deny when a prior successful apply already
    recorded for this plan/result. *)

val runner_failure_message : runner:string -> detail:string -> string
(** Secret-free runner-failure receipt text (independent failure path). *)

val plan_code_work :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  room_id:string ->
  pilot:pilot_gate ->
  user_auth_available:bool ->
  req:code_work_request ->
  base_revision:string ->
  ?route:Github_route_store.t ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Confirmable plan for code-changing work. Apply kind
    [Generic "github_code_work"]. Payload names repository, base, scope, runner,
    output authority, and branch constraints. No live mutation. *)

val plan_pr_create :
  db:Sqlite3.db ->
  principal:Setup_plan.principal ->
  room_id:string ->
  pilot:pilot_gate ->
  user_auth_available:bool ->
  req:pr_create_request ->
  base_revision:string ->
  ?route:Github_route_store.t ->
  ?now:float ->
  unit ->
  (Setup_plan.t, string) result
(** Confirmable plan for constrained PR creation. Apply kind
    [Generic "github_pr_create"]. Payload includes title, head/base, head
    source, pilot name, and correlation fields for webhook reconciliation. No
    live mutation. *)

val apply_confirmed :
  db:Sqlite3.db ->
  plan_id:string ->
  digest:string ->
  principal:Setup_plan.principal ->
  current_base_revision:string ->
  ?current_refs:live_refs ->
  ?now:float ->
  unit ->
  (Setup_plan_apply.outcome, string) result
(** Confirm/apply a pending code-work or PR-create plan via [Setup_plan_apply].

    Rechecks plan id + digest, principal, expiry, destination room, and
    [current_base_revision]. When [current_refs] is supplied for a PR-create
    plan, revalidates head/base before the receipt-only adapter. Domain adapter
    records a receipt only (no live GitHub / runner dispatch in this task). *)

val is_code_change_plan : Setup_plan.t -> bool
(** True when apply kind is [github_code_work] or [github_pr_create]. *)

val receipt_safe_error : string -> string
(** Projection-safe error receipt text: redacts bearer tokens, GitHub PATs, and
    token/secret key=value shapes. *)

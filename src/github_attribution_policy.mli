(** Attribution requirements and risk-tier defaults for GitHub mutations
    (P21.M3.E2.T001 / T004).

    Every mutation declares whether App installation, user-required,
    user-preferred, or PAT compatibility attribution applies, together with a
    risk tier and whether P19 App-attributed pilot execution is allowed as an
    interim path.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

type risk_tier = Low | Medium | High | Critical

type attribution =
  | App_installation
      (** Act as the GitHub App installation (reads, ambient automation under
          App-first policy). Not a user→App fallback. *)
  | User_required
      (** Must use a current Principal-owned user access token; no App/PAT
          fallback. High-risk mutations (merge, review submit, issue create /
          close / reopen, code change, workflow dispatch). *)
  | User_preferred
      (** Prefer Principal-owned user attribution. App is allowed only as a
          visible fallback when action policy permits and the current preview
          explicitly names the App actor (P21.M3.E2.T004). *)
  | Pat_compat
      (** Legacy PAT exact-repo compatibility path; not preferred for new
          mutations. Never used as a silent fallback from user paths. *)

type requirement = {
  action : string;  (** Canonical action id (lowercase snake_case). *)
  tier : risk_tier;
  attribution : attribution;
  pilot_allowed : bool;
      (** When true, P19 may enable App-attributed execution under a named,
          time-bounded pilot gate until User_required production readiness.
          Low-risk App / User_preferred paths set this false (production App is
          the normal or explicitly-previewed path, not a pilot exception). *)
}

val risk_tier_to_string : risk_tier -> string
val attribution_to_string : attribution -> string

val permits_app_fallback : attribution -> bool
(** [true] only for [User_preferred]. [User_required], pure [App_installation],
    and [Pat_compat] never treat App as a user-path fallback. *)

val defaults : unit -> requirement list
(** Built-in attribution requirements for known GitHub mutation families. *)

val lookup : action:string -> requirement
(** Lookup by action id (case-insensitive, trimmed). Accepts aliases including
    [submit_review] → [review_submit], [request_reviewers] → [review_request],
    [code_work] → [code_change], [collab_comment] → [comment], [collab_label] →
    [label], [collab_assign]/[assignee] → [assign], [issue_open]/[create_issue]
    → [issue_create], [close_issue] → [issue_close], [reopen_issue] →
    [issue_reopen].

    Unknown actions fail closed as [User_required] / [Critical] with
    [pilot_allowed = false] so undeclared mutations never silently use App. *)

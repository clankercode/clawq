(** Attribution requirements and risk-tier defaults for GitHub mutations
    (P21.M3.E2.T001).

    Every mutation declares whether App installation, user-required, or PAT
    compatibility attribution applies, together with a risk tier and whether P19
    App-attributed pilot execution is allowed as an interim path.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

type risk_tier = Low | Medium | High | Critical

type attribution =
  | App_installation
      (** Act as the GitHub App installation (reads, ambient, low-risk metadata
          / comments under App-first policy). *)
  | User_required
      (** Must use a current Principal-owned user access token; no silent App
          fallback. High-risk mutations (merge, review submit, code change,
          workflow dispatch). *)
  | Pat_compat
      (** Legacy PAT exact-repo compatibility path; not preferred for new
          mutations. *)

type requirement = {
  action : string;  (** Canonical action id (lowercase snake_case). *)
  tier : risk_tier;
  attribution : attribution;
  pilot_allowed : bool;
      (** When true, P19 may enable App-attributed execution under a named,
          time-bounded pilot gate until User_required production readiness.
          Low-risk App paths set this false (production App is the normal path,
          not a pilot exception). *)
}

val risk_tier_to_string : risk_tier -> string
val attribution_to_string : attribution -> string

val defaults : unit -> requirement list
(** Built-in attribution requirements for known GitHub mutation families. *)

val lookup : action:string -> requirement
(** Lookup by action id (case-insensitive, trimmed). Accepts a few aliases
    ([submit_review] → [review_submit], [code_work] → [code_change],
    [collab_comment] → [comment], [collab_label] → [label]).

    Unknown actions fail closed as [User_required] / [Critical] with
    [pilot_allowed = false] so undeclared mutations never silently use App. *)

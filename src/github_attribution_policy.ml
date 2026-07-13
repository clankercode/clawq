(* Attribution requirements and risk-tier defaults (P21.M3.E2.T001 / T004).
   See github_attribution_policy.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

type risk_tier = Low | Medium | High | Critical

type attribution =
  | App_installation
  | User_required
  | User_preferred
  | Pat_compat

type requirement = {
  action : string;
  tier : risk_tier;
  attribution : attribution;
  pilot_allowed : bool;
}

let risk_tier_to_string = function
  | Low -> "low"
  | Medium -> "medium"
  | High -> "high"
  | Critical -> "critical"

let attribution_to_string = function
  | App_installation -> "app_installation"
  | User_required -> "user_required"
  | User_preferred -> "user_preferred"
  | Pat_compat -> "pat_compat"

let permits_app_fallback = function
  | User_preferred -> true
  | App_installation | User_required | Pat_compat -> false

let req ~action ~tier ~attribution ~pilot_allowed =
  { action; tier; attribution; pilot_allowed }

(** Built-in defaults.

    High-risk mutations (merge, review submit, code change, workflow dispatch)
    require user attribution; P19 may still enable App under an explicit pilot
    ([pilot_allowed = true]) — pilot rollout is owned by T006 and never means
    silent fallback from a failed user path (T004).

    Comments and ordinary metadata are [User_preferred]: native user when
    available, App only when the current preview explicitly names the App actor
    (visible fallback). *)
let defaults () =
  [
    (* Low / Medium — User_preferred with visible App fallback only *)
    req ~action:"comment" ~tier:Low ~attribution:User_preferred
      ~pilot_allowed:false;
    req ~action:"label" ~tier:Medium ~attribution:User_preferred
      ~pilot_allowed:false;
    (* Ordinary metadata: request reviewers (not high-risk review submission) *)
    req ~action:"review_request" ~tier:Medium ~attribution:User_preferred
      ~pilot_allowed:false;
    (* High / Critical — User_required; pilot App interim allowed (not fallback) *)
    req ~action:"review_submit" ~tier:High ~attribution:User_required
      ~pilot_allowed:true;
    req ~action:"code_change" ~tier:High ~attribution:User_required
      ~pilot_allowed:true;
    req ~action:"workflow_dispatch" ~tier:Critical ~attribution:User_required
      ~pilot_allowed:true;
    req ~action:"merge" ~tier:Critical ~attribution:User_required
      ~pilot_allowed:true;
  ]

let normalize_action action =
  let a = String.lowercase_ascii (String.trim action) in
  match a with
  | "submit_review" | "review" -> "review_submit"
  | "request_reviewers" | "request_review" | "reviewer_request" ->
      "review_request"
  | "code_work" | "pr_create" -> "code_change"
  | "workflow" -> "workflow_dispatch"
  | "collab_comment" -> "comment"
  | "collab_label" -> "label"
  | other -> other

let by_action =
  lazy
    (List.fold_left
       (fun acc (r : requirement) -> (r.action, r) :: acc)
       [] (defaults ()))

let fail_closed ~action : requirement =
  {
    action;
    tier = Critical;
    attribution = User_required;
    pilot_allowed = false;
  }

let lookup ~action =
  let action = normalize_action action in
  if action = "" then fail_closed ~action:""
  else
    match List.assoc_opt action (Lazy.force by_action) with
    | Some r -> r
    | None -> fail_closed ~action

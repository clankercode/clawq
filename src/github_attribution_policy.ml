(* Attribution requirements and risk-tier defaults (P21.M3.E2.T001).
   See github_attribution_policy.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

type risk_tier = Low | Medium | High | Critical
type attribution = App_installation | User_required | Pat_compat

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
  | Pat_compat -> "pat_compat"

let req ~action ~tier ~attribution ~pilot_allowed =
  { action; tier; attribution; pilot_allowed }

(** Built-in defaults.

    High-risk mutations (merge, review submit, code change, workflow dispatch)
    require user attribution; P19 may still enable App under an explicit pilot
    ([pilot_allowed = true]).

    Low/medium metadata and comments use App installation without a pilot
    exception path ([pilot_allowed = false]). *)
let defaults () =
  [
    (* Low / Medium — App-first ordinary mutations *)
    req ~action:"comment" ~tier:Low ~attribution:App_installation
      ~pilot_allowed:false;
    req ~action:"label" ~tier:Medium ~attribution:App_installation
      ~pilot_allowed:false;
    (* High / Critical — User_required; pilot App interim allowed *)
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

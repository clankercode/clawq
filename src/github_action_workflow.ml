(* Shared revision-bound plan → confirm → apply for GitHub mutating actions.
   See github_action_workflow.mli. *)

type action_kind =
  | Collab of Github_collab_actions.action
  | Request_reviewers of Github_pr_review_actions.request_reviewers
  | Submit_review of Github_pr_review_actions.submit_review

let action_kind_label = function
  | Collab _ -> "collab"
  | Request_reviewers _ -> "request_reviewers"
  | Submit_review _ -> "submit_review"

let is_github_action_kind = function
  | Setup_plan.Generic
      ( "github_collab_action" | "github_request_reviewers"
      | "github_submit_review" ) ->
      true
  | _ -> false

let is_github_action_plan (plan : Setup_plan.t) =
  is_github_action_kind plan.apply_payload.kind

(** Receipt-only adapter: durable apply receipt is owned by Setup_plan_apply; no
    live GitHub mutation until a later task wires real dispatch. *)
let receipt_only_apply_ops ~(plan : Setup_plan.t) ~receipt_id =
  if not (is_github_action_plan plan) then
    Error
      (Printf.sprintf
         "github_action_workflow: unsupported apply kind for plan %s (receipt \
          %s); expected github_collab_action | github_request_reviewers | \
          github_submit_review"
         plan.id receipt_id)
  else Ok ()

let authority_allow ~principal:_ ~destination:_ = Ok ()

let preview ~db ~principal ~room_id ~action ~base_revision ?route
    ?(pilot = Github_pr_review_actions.default_pilot_gate)
    ?(user_auth_available = false) ?(now = Unix.gettimeofday ()) () =
  match action with
  | Collab collab ->
      Github_collab_actions.plan_action ~db ~principal ~room_id ~action:collab
        ~base_revision ?route ~now ()
  | Request_reviewers req ->
      Github_pr_review_actions.plan_request_reviewers ~db ~principal ~room_id
        ~req ~base_revision ?route ~now ()
  | Submit_review req ->
      Github_pr_review_actions.plan_submit_review ~db ~principal ~room_id ~pilot
        ~user_auth_available ~req ~base_revision ?route ~now ()

let apply_confirmed ~db ~plan_id ~digest ~principal ~current_base_revision
    ?(now = Unix.gettimeofday ()) () =
  Setup_plan_apply.init_schema db;
  match Setup_plan_apply.get_plan ~db ~plan_id with
  | None ->
      (* Delegate to apply so audit records Plan_not_found consistently. *)
      Ok
        (Setup_plan_apply.apply ~db ~plan_id ~digest ~principal
           ~current_base_revision ~destination_room:"" ~now
           ~authority:authority_allow ~apply_ops:receipt_only_apply_ops ())
  | Some plan -> (
      if not (is_github_action_plan plan) then
        Error
          (Printf.sprintf
             "plan %s is not a GitHub action plan (apply_payload.kind mismatch)"
             plan_id)
      else
        match plan.destination.room_id with
        | None ->
            Error
              (Printf.sprintf
                 "plan %s has no destination room; cannot apply GitHub action"
                 plan_id)
        | Some destination_room ->
            Ok
              (Setup_plan_apply.apply ~db ~plan_id ~digest ~principal
                 ~current_base_revision ~destination_room ~now
                 ~authority:authority_allow ~apply_ops:receipt_only_apply_ops ())
      )

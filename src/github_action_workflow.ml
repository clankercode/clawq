(* Shared revision-bound plan → confirm → apply for GitHub mutating actions.
   See github_action_workflow.mli. *)

module Attr = Github_action_actor_attribution
module Reconcile = Github_action_reconcile
module Collab_attr = Github_collab_attribution
module Review_attr = Github_pr_review_attribution
module Issue_attr = Github_issue_attribution
module Wd_attr = Github_workflow_dispatch_attribution

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

let action_kind_label = function
  | Collab _ -> "collab"
  | Request_reviewers _ -> "request_reviewers"
  | Submit_review _ -> "submit_review"
  | Merge _ -> "merge"
  | Issue _ -> "issue"
  | Workflow_dispatch _ -> "workflow_dispatch"

let is_github_action_kind = function
  | Setup_plan.Generic
      ( "github_collab_action" | "github_request_reviewers"
      | "github_submit_review" | "github_merge" | "github_workflow_dispatch" )
    ->
      true
  | Setup_plan.Generic kind when Github_issue_actions.is_issue_action_kind kind
    ->
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
          github_submit_review | github_merge | github_issue_* | \
          github_workflow_dispatch"
         plan.id receipt_id)
  else Ok ()

let authority_allow ~principal:_ ~destination:_ = Ok ()

(** Optionally pin an initiating Actor snapshot onto a just-created pending
    plan. Room/session are source context only — identity comes from [actor_key]
    / [actor_snapshot], never Room history. *)
let maybe_attach_actor_snapshot ~db ~plan ~room_id ?session_id ?actor_key
    ?actor_snapshot ?account_binding_id ?now () =
  match (actor_snapshot, actor_key) with
  | None, None -> Ok plan
  | Some (snap : Actor_snapshot.t), Some key -> (
      let initiating =
        let open Actor_snapshot in
        snap.lineage.actor_key
      in
      match Attr.assert_not_borrowed_identity ~initiating ~claimed:key with
      | Error e -> Error e
      | Ok () ->
          let target = Attr.target_fingerprint_of_plan plan in
          Attr.attach_and_restamp ~db ~plan ~snapshot:snap ~target ())
  | Some (snap : Actor_snapshot.t), None ->
      let target = Attr.target_fingerprint_of_plan plan in
      Attr.attach_and_restamp ~db ~plan ~snapshot:snap ~target ()
  | None, Some key -> (
      let now = match now with Some t -> t | None -> Unix.gettimeofday () in
      match
        Attr.capture_for_intent ~db ~actor_key:key ?account_binding_id ~room_id
          ?session_id ~intent_id:plan.id ~now ()
      with
      | Error e -> Error e
      | Ok snap ->
          let target = Attr.target_fingerprint_of_plan plan in
          Attr.attach_and_restamp ~db ~plan ~snapshot:snap ~target ())

let preview ~db ~principal ~room_id ~action ~base_revision ?route
    ?(pilot = Github_pr_review_actions.default_pilot_gate)
    ?(merge_pilot = Github_merge_action.default_pilot_gate)
    ?(issue_pilot = Github_issue_actions.default_pilot_gate)
    ?(workflow_pilot = Github_workflow_dispatch.default_pilot_gate)
    ?(user_auth_available = false) ?actor_key ?actor_snapshot
    ?account_binding_id ?session_id ?attribution_evidence
    ?(review_live = Review_attr.default_live_revalidation)
    ?(issue_live = Issue_attr.default_live_revalidation)
    ?(workflow_live = Wd_attr.default_live_revalidation) ?github_user_id
    ?(now = Unix.gettimeofday ()) () =
  let plan_res =
    match action with
    | Collab collab -> (
        match attribution_evidence with
        | Some evidence -> (
            match
              Collab_attr.plan_with_attribution ~db ~principal ~room_id
                ~action:collab ~base_revision ~evidence ?route ?actor_snapshot
                ?github_user_id ~now ()
            with
            | Ok planned -> Ok planned.plan
            | Error e -> Error e)
        | None ->
            Github_collab_actions.plan_action ~db ~principal ~room_id
              ~action:collab ~base_revision ?route ~now ())
    | Request_reviewers req -> (
        match attribution_evidence with
        | Some auth -> (
            match
              Review_attr.plan_with_attribution ~db ~principal ~room_id
                ~family:(Review_attr.Request_reviewers req) ~base_revision ~auth
                ~live:review_live ~route ~pilot ~user_auth_available
                ?actor_snapshot ?github_user_id ~now ()
            with
            | Ok planned -> Ok planned.plan
            | Error e -> Error e)
        | None ->
            Github_pr_review_actions.plan_request_reviewers ~db ~principal
              ~room_id ~req ~base_revision ?route ~now ())
    | Submit_review req -> (
        match attribution_evidence with
        | Some auth -> (
            match
              Review_attr.plan_with_attribution ~db ~principal ~room_id
                ~family:(Review_attr.Submit_review req) ~base_revision ~auth
                ~live:review_live ~route ~pilot ~user_auth_available
                ?actor_snapshot ?github_user_id ~now ()
            with
            | Ok planned -> Ok planned.plan
            | Error e -> Error e)
        | None ->
            Github_pr_review_actions.plan_submit_review ~db ~principal ~room_id
              ~pilot ~user_auth_available ~req ~base_revision ?route ~now ())
    | Merge { req; policy } ->
        Github_merge_action.plan_merge ~db ~principal ~room_id
          ~pilot:merge_pilot ~user_auth_available ~req ~policy ~base_revision
          ?route ~now ()
    | Issue issue_action -> (
        match attribution_evidence with
        | Some auth -> (
            match
              Issue_attr.plan_with_attribution ~db ~principal ~room_id
                ~action:issue_action ~base_revision ~auth ~live:issue_live
                ~route ~pilot:issue_pilot ~user_auth_available ?actor_snapshot
                ?github_user_id ~now ()
            with
            | Ok planned -> Ok planned.plan
            | Error e -> Error e)
        | None ->
            Github_issue_actions.plan_action ~db ~principal ~room_id
              ~pilot:issue_pilot ~user_auth_available ~action:issue_action
              ~base_revision ?route ~now ())
    | Workflow_dispatch req -> (
        match attribution_evidence with
        | Some auth -> (
            match
              Wd_attr.plan_with_attribution ~db ~principal ~room_id ~req
                ~base_revision ~auth ~live:workflow_live ~route
                ~pilot:workflow_pilot ~user_auth_available ?actor_snapshot
                ?github_user_id ~now ()
            with
            | Ok planned -> Ok planned.plan
            | Error e -> Error e)
        | None ->
            Github_workflow_dispatch.plan_dispatch ~db ~principal ~room_id
              ~pilot:workflow_pilot ~user_auth_available ~req ~base_revision
              ?route ~now ())
  in
  match plan_res with
  | Error e -> Error e
  | Ok plan ->
      maybe_attach_actor_snapshot ~db ~plan ~room_id ?session_id ?actor_key
        ?actor_snapshot ?account_binding_id ~now ()

let reject_actor_attribution msg : Setup_plan_apply.outcome =
  Setup_plan_apply.Rejected
    { reason = Setup_plan_apply.Apply_error; message = msg }

(** After a successful apply, attach the initiating Actor snapshot (when pinned)
    and attribution modes to a durable open correlation so webhook reconcile
    retains historical identity. Best-effort: apply receipt is already durable;
    correlation record failure must not rewrite the applied plan. *)
let maybe_record_receipt_correlation ~db ~plan ~receipt_id ~now =
  match Reconcile.record_from_applied_plan ~db ~plan ~receipt_id ~now () with
  | Ok _ | Error _ -> ()

let apply_outcome_with_correlation ~db ~plan ~now
    (outcome : Setup_plan_apply.outcome) =
  (match outcome with
  | Setup_plan_apply.Applied { receipt_id; first_time = true } ->
      maybe_record_receipt_correlation ~db ~plan ~receipt_id ~now
  | Setup_plan_apply.Applied { first_time = false; _ }
  | Setup_plan_apply.Rejected _ ->
      ());
  outcome

(** When a collab plan carries staged attribution, revalidate live evidence and
    issue an opaque lease before receipt-only apply. Receipt-only path revokes
    the lease after native attribution receipt (no live HTTP in this layer). *)
let maybe_collab_attribution_dispatch ~db ~plan ?attribution_live ?vault_id
    ?expected_account ?github_user_id ~now () =
  if not (Collab_attr.has_attribution_allow plan) then Ok None
  else
    match attribution_live with
    | None ->
        Error
          "collab plan has staged attribution_allow; apply requires \
           attribution_live evidence for revalidation and dispatch lease"
    | Some live -> (
        match
          Collab_attr.prepare_dispatch_from_plan ~db ~plan ~live ?vault_id
            ?expected:expected_account ?github_user_id ~now ()
        with
        | Error e -> Error e
        | Ok dispatched ->
            (* Receipt-only apply: lease proves dispatch gate; no HTTP here. *)
            Collab_attr.revoke_issued_lease dispatched.issued;
            Ok (Some dispatched))

(** When a PR review plan carries staged attribution, revalidate live evidence
    and issue an opaque lease before receipt-only apply. *)
let maybe_pr_review_attribution_dispatch ~db ~plan ?attribution_live
    ?(review_live = Review_attr.default_live_revalidation) ?vault_id
    ?expected_account ?github_user_id ~now () =
  if not (Review_attr.has_attribution_allow plan) then Ok None
  else
    match attribution_live with
    | None ->
        Error
          "PR review plan has staged attribution_allow; apply requires \
           attribution_live evidence for revalidation and dispatch lease"
    | Some live_auth -> (
        match
          Review_attr.prepare_dispatch_from_plan ~db ~plan ~live_auth
            ~live:review_live ?vault_id ?expected:expected_account
            ?github_user_id ~now ()
        with
        | Error e -> Error e
        | Ok dispatched ->
            Review_attr.revoke_issued_lease dispatched.issued;
            Ok (Some dispatched))

(** When an issue create/lifecycle plan carries staged attribution, revalidate
    live evidence and issue an opaque user lease before receipt-only apply. *)
let maybe_issue_attribution_dispatch ~db ~plan ?attribution_live
    ?(issue_live = Issue_attr.default_live_revalidation) ?vault_id
    ?expected_account ?github_user_id ~now () =
  if not (Issue_attr.has_attribution_allow plan) then Ok None
  else
    match attribution_live with
    | None ->
        Error
          "issue plan has staged attribution_allow; apply requires \
           attribution_live evidence for revalidation and dispatch lease"
    | Some live_auth -> (
        match
          Issue_attr.prepare_dispatch_from_plan ~db ~plan ~live_auth
            ~live:issue_live ?vault_id ?expected:expected_account
            ?github_user_id ~now ()
        with
        | Error e -> Error e
        | Ok dispatched ->
            Issue_attr.revoke_issued_lease dispatched.issued;
            Ok (Some dispatched))

(** When a workflow_dispatch plan carries staged attribution, revalidate live
    evidence and issue an opaque user lease before receipt-only apply. *)
let maybe_workflow_dispatch_attribution_dispatch ~db ~plan ?attribution_live
    ?(workflow_live = Wd_attr.default_live_revalidation) ?vault_id
    ?expected_account ?github_user_id ~now () =
  if not (Wd_attr.has_attribution_allow plan) then Ok None
  else
    match attribution_live with
    | None ->
        Error
          "workflow_dispatch plan has staged attribution_allow; apply requires \
           attribution_live evidence for revalidation and dispatch lease"
    | Some live_auth -> (
        match
          Wd_attr.prepare_dispatch_from_plan ~db ~plan ~live_auth
            ~live:workflow_live ?vault_id ?expected:expected_account
            ?github_user_id ~now ()
        with
        | Error e -> Error e
        | Ok dispatched ->
            Wd_attr.revoke_issued_lease dispatched.issued;
            Ok (Some dispatched))

let apply_with_actor_revalidation ~db ~plan ~plan_id ~digest ~principal
    ~current_base_revision ~destination_room ?current_target ?attribution_live
    ?review_live ?issue_live ?workflow_live ?vault_id ?expected_account
    ?github_user_id ?(now = Unix.gettimeofday ()) () =
  match
    Attr.revalidate_for_apply ~db ~plan ?current_target ~require_snapshot:false
      ()
  with
  | Error msg -> Ok (reject_actor_attribution msg)
  | Ok _envelope_opt -> (
      match
        maybe_collab_attribution_dispatch ~db ~plan ?attribution_live ?vault_id
          ?expected_account ?github_user_id ~now ()
      with
      | Error msg -> Ok (reject_actor_attribution msg)
      | Ok _ -> (
          match
            maybe_pr_review_attribution_dispatch ~db ~plan ?attribution_live
              ?review_live ?vault_id ?expected_account ?github_user_id ~now ()
          with
          | Error msg -> Ok (reject_actor_attribution msg)
          | Ok _ -> (
              match
                maybe_issue_attribution_dispatch ~db ~plan ?attribution_live
                  ?issue_live ?vault_id ?expected_account ?github_user_id ~now
                  ()
              with
              | Error msg -> Ok (reject_actor_attribution msg)
              | Ok _ -> (
                  match
                    maybe_workflow_dispatch_attribution_dispatch ~db ~plan
                      ?attribution_live ?workflow_live ?vault_id
                      ?expected_account ?github_user_id ~now ()
                  with
                  | Error msg -> Ok (reject_actor_attribution msg)
                  | Ok _dispatched_opt ->
                      (* Snapshot (when present) re-resolved; staged
                         attribution dispatch revalidated. Proceed with
                         receipt-only apply. *)
                      let outcome =
                        Setup_plan_apply.apply ~db ~plan_id ~digest ~principal
                          ~current_base_revision ~destination_room ~now
                          ~authority:authority_allow
                          ~apply_ops:receipt_only_apply_ops ()
                      in
                      Ok (apply_outcome_with_correlation ~db ~plan ~now outcome)
                  ))))

let apply_confirmed ~db ~plan_id ~digest ~principal ~current_base_revision
    ?current_merge_policy ?current_target ?attribution_live ?review_live
    ?issue_live ?workflow_live ?vault_id ?expected_account ?github_user_id
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
      else if Github_merge_action.is_merge_plan plan then
        (* Actor revalidation first, then merge-specific live policy checks. *)
        match
          Attr.revalidate_for_apply ~db ~plan ?current_target
            ~require_snapshot:false ()
        with
        | Error msg -> Ok (reject_actor_attribution msg)
        | Ok _ -> (
            match
              Github_merge_action.apply_confirmed ~db ~plan_id ~digest
                ~principal ~current_base_revision
                ?current_policy:current_merge_policy ~now ()
            with
            | Error e -> Error e
            | Ok outcome ->
                Ok (apply_outcome_with_correlation ~db ~plan ~now outcome))
      else
        match plan.destination.room_id with
        | None ->
            Error
              (Printf.sprintf
                 "plan %s has no destination room; cannot apply GitHub action"
                 plan_id)
        | Some destination_room ->
            apply_with_actor_revalidation ~db ~plan ~plan_id ~digest ~principal
              ~current_base_revision ~destination_room ?current_target
              ?attribution_live ?review_live ?issue_live ?workflow_live
              ?vault_id ?expected_account ?github_user_id ~now ())

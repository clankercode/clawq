(* Shared revision-bound plan → confirm → apply for GitHub mutating actions.
   See github_action_workflow.mli. *)

module Attr = Github_action_actor_attribution
module Reconcile = Github_action_reconcile
module Collab_attr = Github_collab_attribution
module Review_attr = Github_pr_review_attribution
module Issue_attr = Github_issue_attribution
module Merge_attr = Github_merge_attribution
module Wd_attr = Github_workflow_dispatch_attribution
module Code_attr = Github_code_change_attribution

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
  | Code_work of Github_code_change_action.code_work_request
  | Pr_create of Github_code_change_action.pr_create_request

let action_kind_label = function
  | Collab _ -> "collab"
  | Request_reviewers _ -> "request_reviewers"
  | Submit_review _ -> "submit_review"
  | Merge _ -> "merge"
  | Issue _ -> "issue"
  | Workflow_dispatch _ -> "workflow_dispatch"
  | Code_work _ -> "code_work"
  | Pr_create _ -> "pr_create"

let is_github_action_kind = function
  | Setup_plan.Generic
      ( "github_collab_action" | "github_request_reviewers"
      | "github_submit_review" | "github_merge" | "github_workflow_dispatch"
      | "github_code_work" | "github_pr_create" ) ->
      true
  | Setup_plan.Generic kind when Github_issue_actions.is_issue_action_kind kind
    ->
      true
  | _ -> false

let is_github_action_plan (plan : Setup_plan.t) =
  is_github_action_kind plan.apply_payload.kind

let is_e1_fail_closed_kind = function
  | Setup_plan.Generic
      ( "github_collab_action" | "github_request_reviewers"
      | "github_submit_review" ) ->
      true
  | Setup_plan.Generic kind when Github_issue_actions.is_issue_action_kind kind
    ->
      true
  | _ -> false

(** Ordinary P19 collaboration/review/issue actions have no live dispatcher.
    Workflow and code-work paths retain their existing P21 attribution receipt
    behavior until their separately scoped adapters change. *)
let receipt_only_apply_ops ~(plan : Setup_plan.t) ~receipt_id =
  if not (is_github_action_plan plan) then
    Error
      (Printf.sprintf
         "github_action_workflow: unsupported apply kind for plan %s (receipt \
          %s); expected github_collab_action | github_request_reviewers | \
          github_submit_review | github_merge | github_issue_* | \
          github_workflow_dispatch | github_code_work | github_pr_create"
         plan.id receipt_id)
  else if is_e1_fail_closed_kind plan.apply_payload.kind then
    Error
      (Printf.sprintf
         "GitHub action apply is unavailable for plan %s: this pilot has no \
          live GitHub REST dispatcher. The pending plan was not applied; no \
          GitHub mutation, receipt, or webhook correlation was produced."
         plan.id)
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
    ?(code_change_pilot = Github_code_change_action.default_pilot_gate)
    ?(user_auth_available = false) ?actor_key ?actor_snapshot
    ?account_binding_id ?session_id ?attribution_evidence
    ?(review_live = Review_attr.default_live_revalidation)
    ?(issue_live = Issue_attr.default_live_revalidation) ?merge_live
    ?(workflow_live = Wd_attr.default_live_revalidation)
    ?(code_change_live = Code_attr.default_live_revalidation) ?github_user_id
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
    | Merge { req; policy } -> (
        match attribution_evidence with
        | Some auth -> (
            let live =
              match merge_live with
              | Some l -> l
              | None -> Merge_attr.default_live_revalidation ~policy ()
            in
            match
              Merge_attr.plan_with_attribution ~db ~principal ~room_id ~req
                ~policy ~base_revision ~auth ~live ~route ~pilot:merge_pilot
                ~user_auth_available ?actor_snapshot ?github_user_id ~now ()
            with
            | Ok planned -> Ok planned.plan
            | Error e -> Error e)
        | None ->
            Github_merge_action.plan_merge ~db ~principal ~room_id
              ~pilot:merge_pilot ~user_auth_available ~req ~policy
              ~base_revision ?route ~now ())
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
    | Code_work req -> (
        match attribution_evidence with
        | Some auth -> (
            match
              Code_attr.plan_with_attribution ~db ~principal ~room_id
                ~family:(Code_attr.Code_work req) ~base_revision ~auth
                ~live:code_change_live ~route ~pilot:code_change_pilot
                ~user_auth_available ?actor_snapshot ?github_user_id ~now ()
            with
            | Ok planned -> Ok planned.plan
            | Error e -> Error e)
        | None ->
            Github_code_change_action.plan_code_work ~db ~principal ~room_id
              ~pilot:code_change_pilot ~user_auth_available ~req ~base_revision
              ?route ~now ())
    | Pr_create req -> (
        match attribution_evidence with
        | Some auth -> (
            match
              Code_attr.plan_with_attribution ~db ~principal ~room_id
                ~family:(Code_attr.Pr_create req) ~base_revision ~auth
                ~live:code_change_live ~route ~pilot:code_change_pilot
                ~user_auth_available ?actor_snapshot ?github_user_id ~now ()
            with
            | Ok planned -> Ok planned.plan
            | Error e -> Error e)
        | None ->
            Github_code_change_action.plan_pr_create ~db ~principal ~room_id
              ~pilot:code_change_pilot ~user_auth_available ~req ~base_revision
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

(** Persist reconciliation evidence inside the setup-plan apply transaction. An
    applied GitHub plan without this row cannot be safely reconciled, so its
    receipt must not commit. *)
let receipt_apply_with_reconciliation ~db ~now ?github_user_id
    ?attribution_receipt_id ?job_id ?expected_github_login ?native_actor_kind
    ~(plan : Setup_plan.t) ~receipt_id () =
  match receipt_only_apply_ops ~plan ~receipt_id with
  | Error _ as error -> error
  | Ok () -> (
      match
        Reconcile.record_from_applied_plan ~db ~plan ~receipt_id ?github_user_id
          ?attribution_receipt_id ?job_id ?expected_github_login
          ?native_actor_kind ~now ()
      with
      | Ok _ -> Ok ()
      | Error err ->
          Error
            ("GitHub action apply refused: durable reconciliation evidence \
              could not be recorded: " ^ err))

let apply_outcome_with_correlation ~db ~plan ~now ?github_user_id
    ?attribution_receipt_id ?job_id ?expected_github_login ?native_actor_kind
    (outcome : Setup_plan_apply.outcome) =
  match outcome with
  | Setup_plan_apply.Applied { receipt_id; _ } -> (
      match Reconcile.get_by_receipt_id ~db ~receipt_id with
      | Some _ -> outcome
      | None -> (
          (* Repairs receipts written by the pre-T006 path, but never reports
             success unless the missing durable evidence is restored. *)
          match
            Reconcile.record_from_applied_plan ~db ~plan ~receipt_id
              ?github_user_id ?attribution_receipt_id ?job_id
              ?expected_github_login ?native_actor_kind ~now ()
          with
          | Ok _ -> outcome
          | Error err ->
              reject_actor_attribution
                ("GitHub action outcome withheld: durable reconciliation \
                  evidence is unavailable: " ^ err)))
  | Setup_plan_apply.Rejected _ -> outcome

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

(** When a merge plan carries staged attribution, revalidate live merge policy
    and issue an opaque user lease before receipt-only apply. *)
let maybe_merge_attribution_dispatch ~db ~plan ?attribution_live ?merge_live
    ?current_merge_policy ?vault_id ?expected_account ?github_user_id ~now () =
  if not (Merge_attr.has_attribution_allow plan) then Ok None
  else
    match attribution_live with
    | None ->
        Error
          "merge plan has staged attribution_allow; apply requires \
           attribution_live evidence for revalidation and dispatch lease"
    | Some live_auth -> (
        match
          match merge_live with
          | Some l -> Ok l
          | None -> (
              match current_merge_policy with
              | Some policy ->
                  Ok (Merge_attr.default_live_revalidation ~policy ())
              | None ->
                  Error
                    "merge plan has staged attribution_allow; apply requires \
                     current_merge_policy or merge_live for live revalidation \
                     and dispatch lease")
        with
        | Error e -> Error e
        | Ok live -> (
            match
              Merge_attr.prepare_dispatch_from_plan ~db ~plan ~live_auth ~live
                ?vault_id ?expected:expected_account ?github_user_id ~now ()
            with
            | Error e -> Error e
            | Ok dispatched ->
                Merge_attr.revoke_issued_lease dispatched.issued;
                Ok (Some dispatched)))

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

(** When a code-work / PR-create plan carries staged attribution, revalidate
    live evidence and issue an opaque user lease before receipt-only apply. *)
let maybe_code_change_attribution_dispatch ~db ~plan ?attribution_live
    ?(code_change_live = Code_attr.default_live_revalidation) ?vault_id
    ?expected_account ?github_user_id ~now () =
  if not (Code_attr.has_attribution_allow plan) then Ok None
  else
    match attribution_live with
    | None ->
        Error
          "code-change plan has staged attribution_allow; apply requires \
           attribution_live evidence for revalidation and dispatch lease"
    | Some live_auth -> (
        match
          Code_attr.prepare_dispatch_from_plan ~db ~plan ~live_auth
            ~live:code_change_live ?vault_id ?expected:expected_account
            ?github_user_id ~now ()
        with
        | Error e -> Error e
        | Ok dispatched ->
            Code_attr.revoke_issued_lease dispatched.issued;
            Ok (Some dispatched))

let apply_with_actor_revalidation ~db ~plan ~plan_id ~digest ~principal
    ~current_base_revision ~destination_room ?current_target ?attribution_live
    ?review_live:_ ?issue_live:_ ?workflow_live ?code_change_live ?vault_id
    ?expected_account ?github_user_id ?(now = Unix.gettimeofday ()) () =
  match
    Attr.revalidate_for_apply ~db ~plan ?current_target ~require_snapshot:false
      ()
  with
  | Error msg -> Ok (reject_actor_attribution msg)
  | Ok _envelope_opt when is_e1_fail_closed_kind plan.apply_payload.kind ->
      (* Attribution dispatch issues a native receipt/lease.  Until ordinary
         P19 actions have a live REST dispatcher, that side effect would
         falsely claim an apply that cannot occur.  Keep actor revalidation
         side-effect-free and let the shared adapter reject before any receipt
         or webhook correlation is created. *)
      Ok
        (Setup_plan_apply.apply ~db ~plan_id ~digest ~principal
           ~current_base_revision ~destination_room ~now
           ~authority:authority_allow ~apply_ops:receipt_only_apply_ops ())
  | Ok _envelope_opt -> (
      match
        maybe_workflow_dispatch_attribution_dispatch ~db ~plan ?attribution_live
          ?workflow_live ?vault_id ?expected_account ?github_user_id ~now ()
      with
      | Error msg -> Ok (reject_actor_attribution msg)
      | Ok workflow_d -> (
          let attribution_receipt_id =
            Option.map (fun d -> d.Wd_attr.receipt.id) workflow_d
          in
          match
            maybe_code_change_attribution_dispatch ~db ~plan ?attribution_live
              ?code_change_live ?vault_id ?expected_account ?github_user_id ~now
              ()
          with
          | Error msg -> Ok (reject_actor_attribution msg)
          | Ok code_d ->
              let attribution_receipt_id =
                match code_d with
                | Some d -> Some d.Code_attr.receipt.id
                | None -> attribution_receipt_id
              in
              let outcome =
                let apply_ops ~plan ~receipt_id =
                  receipt_apply_with_reconciliation ~db ~now ?github_user_id
                    ?attribution_receipt_id ~plan ~receipt_id ()
                in
                Setup_plan_apply.apply ~db ~plan_id ~digest ~principal
                  ~current_base_revision ~destination_room ~now
                  ~authority:authority_allow ~apply_ops ()
              in
              Ok
                (apply_outcome_with_correlation ~db ~plan ~now ?github_user_id
                   ?attribution_receipt_id outcome)))

let apply_confirmed ~db ~plan_id ~digest ~principal ~current_base_revision
    ?current_merge_policy ?current_target ?attribution_live ?review_live
    ?issue_live ?merge_live ?workflow_live ?code_change_live ?vault_id
    ?expected_account ?github_user_id ?(now = Unix.gettimeofday ()) () =
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
        (* Actor revalidation, optional staged attribution lease, then merge
           live policy checks. *)
        match
          Attr.revalidate_for_apply ~db ~plan ?current_target
            ~require_snapshot:false ()
        with
        | Error msg -> Ok (reject_actor_attribution msg)
        | Ok _ -> (
            match
              maybe_merge_attribution_dispatch ~db ~plan ?attribution_live
                ?merge_live ?current_merge_policy ?vault_id ?expected_account
                ?github_user_id ~now ()
            with
            | Error msg -> Ok (reject_actor_attribution msg)
            | Ok merge_d -> (
                let attribution_receipt_id =
                  Option.map (fun d -> d.Merge_attr.receipt.id) merge_d
                in
                match
                  Github_merge_action.apply_confirmed ~db ~plan_id ~digest
                    ~principal ~current_base_revision
                    ?current_policy:current_merge_policy ~now ()
                with
                | Error e -> Error e
                | Ok outcome ->
                    Ok
                      (apply_outcome_with_correlation ~db ~plan ~now
                         ?github_user_id ?attribution_receipt_id outcome)))
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
              ?code_change_live ?vault_id ?expected_account ?github_user_id ~now
              ())

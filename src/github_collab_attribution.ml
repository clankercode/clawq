(* Thin integration: attribution authorize + dispatch lease + audit for P19
   collab comment / label / assign metadata writes (P21.M3.E3.T001).
   See github_collab_attribution.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Auth = Github_attribution_authorize
module Lease = Github_attribution_dispatch_lease
module Audit = Github_attribution_audit
module Policy = Github_attribution_policy
module Collab = Github_collab_actions
module V = Github_user_token_vault
module Token_lease = Github_user_token_lease

let schema_version = 1
let field_attribution_allow = "attribution_allow"
let field_requested_mode = "requested_mode"
let field_resolved_mode = "resolved_mode"
let field_used_app_fallback = "used_app_fallback"
let field_attribution = "attribution"

(* -------------------------------------------------------------------------- *)
(* JSON helpers                                                                *)
(* -------------------------------------------------------------------------- *)

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let member_opt key = function
  | `Assoc _ as json -> (
      match Yojson.Safe.Util.member key json with `Null -> None | v -> Some v)
  | _ -> None

let get_string key json =
  match member_opt key json with
  | Some (`String s) ->
      let t = String.trim s in
      if t = "" then None else Some t
  | _ -> None

let get_bool key json =
  match member_opt key json with Some (`Bool b) -> Some b | _ -> None

let get_int key json =
  match member_opt key json with
  | Some (`Int n) -> Some n
  | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
  | _ -> None

let opt_string name = function
  | None -> (name, `Null)
  | Some s -> (name, `String s)

let opt_int name = function None -> (name, `Null) | Some n -> (name, `Int n)

let json_assoc_merge (base : Yojson.Safe.t)
    (extras : (string * Yojson.Safe.t) list) =
  let extras = sort_assoc extras in
  let keys = List.map fst extras in
  match base with
  | `Assoc fields ->
      let filtered =
        List.filter
          (fun (k, _) -> not (List.exists (String.equal k) keys))
          fields
      in
      `Assoc (sort_assoc (filtered @ extras))
  | `Null -> `Assoc extras
  | other -> `Assoc (sort_assoc (("_prior", other) :: extras))

(* -------------------------------------------------------------------------- *)
(* Action mapping                                                              *)
(* -------------------------------------------------------------------------- *)

let policy_action_of_collab = function
  | Collab.Comment _ -> "comment"
  | Collab.Label _ -> "label"
  | Collab.Assign _ -> "assign"

let is_user_preferred_metadata action =
  let req = Policy.lookup ~action:(policy_action_of_collab action) in
  match req.attribution with Policy.User_preferred -> true | _ -> false

let request_for_action ~action (evidence : Auth.request) : Auth.request =
  { evidence with action = policy_action_of_collab action }

(* -------------------------------------------------------------------------- *)
(* Capability + attribution gate                                               *)
(* -------------------------------------------------------------------------- *)

type gate =
  | Capability_denied of { reason : string }
  | Attribution of {
      capability : string;
      request : Auth.request;
      decision : Auth.decision;
    }

let gate ~route ~action ~evidence () =
  match Collab.authorize ~route ~action with
  | Collab.Denied { reason } -> Capability_denied { reason }
  | Collab.Allowed { action; capability } ->
      let request = request_for_action ~action evidence in
      Attribution { capability; request; decision = Auth.authorize request }

(* -------------------------------------------------------------------------- *)
(* Prior Allow JSON                                                            *)
(* -------------------------------------------------------------------------- *)

let checked_revisions_of_json json : (Auth.checked_revisions, string) result =
  match json with
  | `Assoc _ ->
      Ok
        {
          Auth.policy_action =
            Option.value (get_string "policy_action" json) ~default:"";
          requirement_attribution =
            Option.value (get_string "requirement_attribution" json) ~default:"";
          requirement_tier =
            Option.value (get_string "requirement_tier" json) ~default:"";
          tool_catalog_revision = get_string "tool_catalog_revision" json;
          access_revision = get_string "access_revision" json;
          principal_id = get_string "principal_id" json;
          principal_revision = get_int "principal_revision" json;
          actor_revision = get_int "actor_revision" json;
          identity_link_revision = get_int "identity_link_revision" json;
          binding_id = get_string "binding_id" json;
          binding_lineage_id = get_string "binding_lineage_id" json;
          vault_generation = get_int "vault_generation" json;
          installation_id = get_int "installation_id" json;
          installation_revision = get_string "installation_revision" json;
          confirmation_id = get_string "confirmation_id" json;
          actor_snapshot_id = get_string "actor_snapshot_id" json;
          live_state_revision = get_string "live_state_revision" json;
        }
  | _ -> Error "attribution_allow.revisions must be a JSON object"

let risk_tier_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "low" -> Ok Policy.Low
  | "medium" -> Ok Policy.Medium
  | "high" -> Ok Policy.High
  | "critical" -> Ok Policy.Critical
  | other -> Error (Printf.sprintf "unknown risk tier: %s" other)

let attribution_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "app_installation" | "app" -> Ok Policy.App_installation
  | "user_required" -> Ok Policy.User_required
  | "user_preferred" -> Ok Policy.User_preferred
  | "pat_compat" | "pat" -> Ok Policy.Pat_compat
  | other -> Error (Printf.sprintf "unknown attribution: %s" other)

let resolved_mode_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "app" -> Ok Auth.App
  | "user" -> Ok Auth.User
  | other -> Error (Printf.sprintf "unknown resolved mode: %s" other)

let allow_to_json (a : Auth.allow) =
  `Assoc
    (sort_assoc
       [
         ("schema_version", `Int schema_version);
         ("mode", `String (Auth.resolved_mode_to_string a.mode));
         ("used_app_fallback", `Bool a.used_app_fallback);
         ("action", `String a.requirement.action);
         ( "attribution",
           `String (Policy.attribution_to_string a.requirement.attribution) );
         ("tier", `String (Policy.risk_tier_to_string a.requirement.tier));
         ("pilot_allowed", `Bool a.requirement.pilot_allowed);
         opt_string "binding_id" a.binding_id;
         opt_string "principal_id" a.principal_id;
         ("revisions", Auth.checked_revisions_to_json a.revisions);
         ("issues_token", `Bool false);
         ("issues_lease", `Bool false);
       ])

let allow_of_json json : (Auth.allow, string) result =
  match json with
  | `Assoc _ ->
      let ( let* ) = Result.bind in
      let* mode =
        match get_string "mode" json with
        | None -> Error "attribution_allow.mode missing"
        | Some s -> resolved_mode_of_string s
      in
      let* action =
        match get_string "action" json with
        | None | Some "" -> Error "attribution_allow.action missing"
        | Some a -> Ok a
      in
      let* attribution =
        match get_string "attribution" json with
        | None -> Error "attribution_allow.attribution missing"
        | Some s -> attribution_of_string s
      in
      let* tier =
        match get_string "tier" json with
        | None -> Error "attribution_allow.tier missing"
        | Some s -> risk_tier_of_string s
      in
      let pilot_allowed =
        Option.value (get_bool "pilot_allowed" json) ~default:false
      in
      let used_app_fallback =
        Option.value (get_bool "used_app_fallback" json) ~default:false
      in
      let* revisions =
        match member_opt "revisions" json with
        | None -> Error "attribution_allow.revisions missing"
        | Some j -> checked_revisions_of_json j
      in
      Ok
        {
          Auth.mode;
          used_app_fallback;
          requirement = { Policy.action; tier; attribution; pilot_allowed };
          revisions;
          binding_id = get_string "binding_id" json;
          principal_id = get_string "principal_id" json;
        }
  | _ -> Error "attribution_allow must be a JSON object"

(* -------------------------------------------------------------------------- *)
(* Stage preview                                                               *)
(* -------------------------------------------------------------------------- *)

type staged = {
  action : Collab.action;
  capability : string;
  request : Auth.request;
  allow : Auth.allow;
  preview : Audit.t;
}

type stage_error =
  | Capability of string
  | Attribution of { deny : Auth.deny; repair : Audit.t option }
  | Audit of string

let string_of_stage_error = function
  | Capability r -> r
  | Attribution { deny; _ } ->
      Printf.sprintf "attribution denied (%s): %s" deny.failed_check
        deny.repair.message
  | Audit e -> e

let default_item_key action = Collab.action_item_key action

let stage_preview ~db ~route ~action ~evidence ?item_key ?room_id ?plan_id
    ?job_id ?actor_snapshot ?github_user_id ?(now = Unix.gettimeofday ()) () =
  Audit.ensure_schema db;
  match gate ~route ~action ~evidence () with
  | Capability_denied { reason } -> Error (Capability reason)
  | Attribution { capability; request; decision } -> (
      let item_key =
        match item_key with
        | Some k -> Some k
        | None -> Some (default_item_key action)
      in
      match decision with
      | Auth.Deny deny ->
          let repair =
            match
              Audit.record_authorize_decision ~db ~decision
                ~kind:Audit.Repair_state ?item_key ?room_id ?job_id ?plan_id
                ?actor_snapshot ?github_user_id ~now ()
            with
            | Ok r -> Some r
            | Error _ -> None
          in
          Error (Attribution { deny; repair })
      | Auth.Allow allow -> (
          match
            Audit.record_authorize_decision ~db ~decision ~kind:Audit.Preview
              ?item_key ?room_id ?job_id ?plan_id ?actor_snapshot
              ?github_user_id ~now ()
          with
          | Error e -> Error (Audit e)
          | Ok preview -> Ok { action; capability; request; allow; preview }))

(* -------------------------------------------------------------------------- *)
(* Plan attach / extract                                                       *)
(* -------------------------------------------------------------------------- *)

let attach_allow_to_plan ~plan ~(allow : Auth.allow) () =
  let allow_json = allow_to_json allow in
  let requested = Policy.attribution_to_string allow.requirement.attribution in
  let resolved = Auth.resolved_mode_to_string allow.mode in
  let extras =
    [
      (field_attribution_allow, allow_json);
      (field_requested_mode, `String requested);
      (field_resolved_mode, `String resolved);
      (field_used_app_fallback, `Bool allow.used_app_fallback);
      (* github_action_reconcile reads "attribution" as resolved actor mode *)
      (field_attribution, `String resolved);
      ("attribution_policy", `String requested);
      ("attribution_schema_version", `Int schema_version);
    ]
  in
  let data = json_assoc_merge plan.Setup_plan.apply_payload.data extras in
  let planned_state = json_assoc_merge plan.planned_state extras in
  let readiness =
    plan.readiness
    @ [
        {
          Setup_plan.name = "attribution";
          status = Setup_plan.Pass;
          message =
            Printf.sprintf "mode=%s fallback=%b action=%s" resolved
              allow.used_app_fallback allow.requirement.action;
        };
      ]
  in
  let diff =
    plan.diff
    @ [
        Setup_plan.Note
          {
            path = "attribution/" ^ allow.requirement.action;
            message =
              Printf.sprintf
                "Staged %s attribution mode=%s used_app_fallback=%b; \
                 revalidate and issue opaque lease at apply/dispatch. No raw \
                 token on plan."
                allow.requirement.action resolved allow.used_app_fallback;
          };
      ]
  in
  let plan =
    {
      plan with
      planned_state;
      readiness;
      diff;
      apply_payload = { plan.apply_payload with data };
      digest = "";
    }
  in
  Setup_plan.redact plan

let attribution_allow_json_of_plan (plan : Setup_plan.t) =
  match member_opt field_attribution_allow plan.apply_payload.data with
  | Some j -> Some j
  | None -> member_opt field_attribution_allow plan.planned_state

let is_collab_attribution_plan (plan : Setup_plan.t) =
  match plan.apply_payload.kind with
  | Setup_plan.Generic "github_collab_action" -> true
  | _ -> false

let has_attribution_allow (plan : Setup_plan.t) =
  is_collab_attribution_plan plan
  &&
  match attribution_allow_json_of_plan plan with
  | None -> false
  | Some _ -> true

let allow_of_plan (plan : Setup_plan.t) : (Auth.allow option, string) result =
  match attribution_allow_json_of_plan plan with
  | None -> Ok None
  | Some j -> (
      match allow_of_json j with
      | Ok a -> Ok (Some a)
      | Error e ->
          Error (Printf.sprintf "malformed attribution_allow on plan: %s" e))

(* -------------------------------------------------------------------------- *)
(* Plan + stage                                                                *)
(* -------------------------------------------------------------------------- *)

type planned = { plan : Setup_plan.t; staged : staged }

let plan_with_attribution ~db ~principal ~room_id ~action ~base_revision
    ~evidence ?route ?item_key ?job_id ?actor_snapshot ?github_user_id
    ?(now = Unix.gettimeofday ()) () =
  let route_opt = route in
  match
    stage_preview ~db ~route:route_opt ~action ~evidence ?item_key ~room_id
      ?job_id ?actor_snapshot ?github_user_id ~now ()
  with
  | Error e -> Error (string_of_stage_error e)
  | Ok staged -> (
      match
        Collab.plan_action ~db ~principal ~room_id ~action ~base_revision ?route
          ~now ()
      with
      | Error e -> Error e
      | Ok plan -> (
          let plan = attach_allow_to_plan ~plan ~allow:staged.allow () in
          (* Re-stamp preview with plan id now that we have it. *)
          let _ =
            Audit.record_authorize_decision ~db
              ~decision:(Auth.Allow staged.allow) ~kind:Audit.Audit
              ?item_key:(Some (default_item_key action))
              ~room_id ~plan_id:plan.id ?job_id ?actor_snapshot ?github_user_id
              ~now ()
          in
          match Setup_plan_apply.replace_pending_plan ~db plan with
          | Error e -> Error e
          | Ok () ->
              Ok { plan; staged = { staged with preview = staged.preview } }))

(* -------------------------------------------------------------------------- *)
(* Dispatch                                                                    *)
(* -------------------------------------------------------------------------- *)

type dispatched = { issued : Lease.issued; receipt : Audit.t }

let record_dispatch_denial ~db ~(prior : Auth.allow) ~live ?item_key ?room_id
    ?plan_id ?job_id ?actor_snapshot ?github_user_id ~now
    (denial : Lease.denial) =
  let action = prior.requirement.action in
  let reason = Lease.string_of_denial denial in
  let failure_class, failure_code =
    match denial with
    | Lease.Authorization d ->
        ( Audit.classify_failure ~failed_check:d.failed_check
            ~code:d.repair.code (),
          d.repair.code )
    | Lease.Prior_mode_mismatch _ -> (Audit.Identity, "prior_mode_mismatch")
    | Lease.Prior_action_mismatch _ -> (Audit.Identity, "prior_action_mismatch")
    | Lease.Prior_principal_mismatch _ ->
        (Audit.Identity, "prior_principal_mismatch")
    | Lease.Prior_binding_mismatch _ ->
        (Audit.Identity, "prior_binding_mismatch")
    | Lease.User_lease_requires_vault_id ->
        (Audit.Identity, "user_lease_requires_vault_id")
    | Lease.Generation_race _ -> (Audit.Refresh, "generation_race")
    | Lease.Lease _ -> (Audit.Refresh, "lease_denied")
    | Lease.Invalid_input _ -> (Audit.Other "invalid_input", "invalid_input")
  in
  let revisions =
    match denial with
    | Lease.Authorization d -> d.revisions
    | _ -> prior.revisions
  in
  let _ =
    Audit.record_repair ~db ~action ~reason ~failure_class ~failure_code
      ?item_key ?room_id ?job_id ?plan_id
      ~requested_mode:
        (Policy.attribution_to_string prior.requirement.attribution)
      ~resolved_mode:(Auth.resolved_mode_to_string prior.mode)
      ~used_app_fallback:prior.used_app_fallback ?actor_snapshot
      ~revisions_json:
        (Yojson.Safe.to_string (Auth.checked_revisions_to_json revisions))
      ~now ()
  in
  let _ = live in
  let _ = github_user_id in
  ()

let prepare_dispatch ~db ~(live : Auth.request) ~(prior : Auth.allow) ?vault_id
    ?expected ?item_key ?room_id ?plan_id ?receipt_id ?job_id ?actor_snapshot
    ?github_user_id ?(now = Unix.gettimeofday ()) () =
  Audit.ensure_schema db;
  let live : Auth.request = { live with action = prior.requirement.action } in
  match
    Lease.issue_for_dispatch ~db ~now ~live ~prior ?vault_id ?expected ()
  with
  | Error denial ->
      record_dispatch_denial ~db ~prior ~live ?item_key ?room_id ?plan_id
        ?job_id ?actor_snapshot ?github_user_id ~now denial;
      Error denial
  | Ok issued -> (
      let mode = Auth.resolved_mode_to_string issued.mode in
      let requested =
        Policy.attribution_to_string issued.decision.requirement.attribution
      in
      let reason =
        if issued.decision.used_app_fallback then
          Printf.sprintf
            "Collab metadata write completed via visible App fallback \
             (action=%s)"
            issued.decision.requirement.action
        else
          Printf.sprintf
            "Collab metadata write completed with native attribution mode=%s \
             action=%s"
            mode issued.decision.requirement.action
      in
      let result =
        if issued.decision.used_app_fallback then Audit.Fallback_app
        else Audit.Completed
      in
      let github_actor =
        Audit.github_actor_of_revisions issued.decision.revisions
          ?binding_github_user_id:github_user_id ()
      in
      match
        Audit.record_receipt ~db ~action:issued.decision.requirement.action
          ~reason ~result ?item_key ?room_id ?job_id ?plan_id ?receipt_id
          ~requested_mode:requested ~resolved_mode:mode
          ~used_app_fallback:issued.decision.used_app_fallback
          ?fallback_reason:
            (if issued.decision.used_app_fallback then
               Some "policy-permitted visible App fallback"
             else None)
          ~github_actor
          ~lineage:
            (Audit.lineage_of_checked_revisions issued.decision.revisions)
          ?actor_snapshot
          ?actor_snapshot_id:issued.decision.revisions.actor_snapshot_id
          ~revisions_json:
            (Yojson.Safe.to_string
               (Auth.checked_revisions_to_json issued.decision.revisions))
          ~now ()
      with
      | Error e ->
          (* Lease was issued; revoke so a failed audit cannot leave a dangling
             user lease without a receipt. *)
          (match issued.lease with
          | Some l -> Token_lease.revoke l
          | None -> ());
          Error (Lease.Invalid_input ("receipt audit failed: " ^ e))
      | Ok receipt -> Ok { issued; receipt })

let collab_action_hint_of_plan (plan : Setup_plan.t) =
  match plan.apply_payload.kind with
  | Setup_plan.Generic "github_collab_action" -> (
      match member_opt "action" plan.apply_payload.data with
      | Some (`Assoc fields) -> (
          match List.assoc_opt "kind" fields with
          | Some (`String s) -> Some (String.trim s)
          | _ -> None)
      | _ -> (
          match member_opt "action" plan.planned_state with
          | Some (`Assoc fields) -> (
              match List.assoc_opt "kind" fields with
              | Some (`String s) -> Some (String.trim s)
              | _ -> None)
          | _ -> None))
  | _ -> None

let prepare_dispatch_from_plan ~db ~plan ~(live : Auth.request) ?vault_id
    ?expected ?receipt_id ?job_id ?actor_snapshot ?github_user_id
    ?(now = Unix.gettimeofday ()) () =
  match allow_of_plan plan with
  | Error e -> Error e
  | Ok None ->
      Error
        "plan has no staged attribution_allow; cannot prepare collab dispatch"
  | Ok (Some prior) -> (
      let action_id =
        match collab_action_hint_of_plan plan with
        | Some kind -> (
            match String.lowercase_ascii kind with
            | ("comment" | "label" | "assign") as a -> a
            | other -> other)
        | None -> prior.requirement.action
      in
      let live : Auth.request = { live with action = action_id } in
      let item_key =
        match
          match member_opt "item_key" plan.apply_payload.data with
          | Some (`String s) -> Some s
          | _ -> (
              match member_opt "item_key" plan.planned_state with
              | Some (`String s) -> Some s
              | _ -> None)
        with
        | Some s when String.trim s <> "" -> Some (String.trim s)
        | _ -> None
      in
      let room_id = plan.destination.room_id in
      match
        prepare_dispatch ~db ~live ~prior ?vault_id ?expected ?item_key ?room_id
          ~plan_id:plan.id ?receipt_id ?job_id ?actor_snapshot ?github_user_id
          ~now ()
      with
      | Ok d -> Ok d
      | Error denial -> Error (Lease.string_of_denial denial))

let revoke_issued_lease (issued : Lease.issued) =
  match issued.lease with Some l -> Token_lease.revoke l | None -> ()

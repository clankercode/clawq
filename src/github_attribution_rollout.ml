(* P19 → P21 attribution migration matrix and staged rollout (P21.M3.E2.T006).
   See github_attribution_rollout.mli and
   docs/pilots/p21-attribution-migration-rollout.md. *)

module Policy = Github_attribution_policy

let matrix_version = 1
let schema_version = matrix_version

(* -------------------------------------------------------------------------- *)
(* Surface / legacy / semantic tags                                             *)
(* -------------------------------------------------------------------------- *)

type surface = Read | Mutation | Background

let surface_to_string = function
  | Read -> "read"
  | Mutation -> "mutation"
  | Background -> "background"

type legacy_path = Legacy_app | Legacy_pat | Legacy_pilot_app | Legacy_denied

let legacy_path_to_string = function
  | Legacy_app -> "legacy_app"
  | Legacy_pat -> "legacy_pat"
  | Legacy_pilot_app -> "legacy_pilot_app"
  | Legacy_denied -> "legacy_denied"

type preview_rule =
  | Preview_not_required
  | Preview_names_actor
  | Preview_user_only

let preview_rule_to_string = function
  | Preview_not_required -> "preview_not_required"
  | Preview_names_actor -> "preview_names_actor"
  | Preview_user_only -> "preview_user_only"

type fallback_rule = No_fallback | Visible_app_fallback

let fallback_rule_to_string = function
  | No_fallback -> "no_fallback"
  | Visible_app_fallback -> "visible_app_fallback"

type delayed_rule = No_delay_pin | Pin_actor_lineage

let delayed_rule_to_string = function
  | No_delay_pin -> "no_delay_pin"
  | Pin_actor_lineage -> "pin_actor_lineage"

type receipt_rule = Receipt_app_actor | Receipt_resolved_mode | Receipt_pilot

let receipt_rule_to_string = function
  | Receipt_app_actor -> "receipt_app_actor"
  | Receipt_resolved_mode -> "receipt_resolved_mode"
  | Receipt_pilot -> "receipt_pilot"

type webhook_rule =
  | Webhook_ambient
  | Webhook_match_receipt
  | Webhook_self_loop_guard

let webhook_rule_to_string = function
  | Webhook_ambient -> "webhook_ambient"
  | Webhook_match_receipt -> "webhook_match_receipt"
  | Webhook_self_loop_guard -> "webhook_self_loop_guard"

(* -------------------------------------------------------------------------- *)
(* Matrix                                                                       *)
(* -------------------------------------------------------------------------- *)

type matrix_row = {
  action : string;
  surface : surface;
  legacy : legacy_path;
  target : Policy.attribution;
  tier : Policy.risk_tier;
  pilot_allowed : bool;
  pilot_name : string option;
  preview : preview_rule;
  fallback : fallback_rule;
  delayed : delayed_rule;
  receipt : receipt_rule;
  webhook : webhook_rule;
  production_requires_user_gate : bool;
}

let row ~action ~surface ~legacy ~target ~tier ~pilot_allowed ?pilot_name
    ~preview ~fallback ~delayed ~receipt ~webhook ~production_requires_user_gate
    () =
  {
    action;
    surface;
    legacy;
    target;
    tier;
    pilot_allowed;
    pilot_name;
    preview;
    fallback;
    delayed;
    receipt;
    webhook;
    production_requires_user_gate;
  }

(** Built-in migration matrix.

    Reads stay App-first. User_preferred ordinary metadata uses visible App
    fallback only. High-risk User_required families keep P19 pilot_allowed as an
    interim App path (not a silent user→App fallback). *)
let matrix () =
  [
    (* --- Reads (App-first; production gate not required) --- *)
    row ~action:"read" ~surface:Read ~legacy:Legacy_app
      ~target:Policy.App_installation ~tier:Policy.Low ~pilot_allowed:false
      ~preview:Preview_not_required ~fallback:No_fallback ~delayed:No_delay_pin
      ~receipt:Receipt_app_actor ~webhook:Webhook_ambient
      ~production_requires_user_gate:false ();
    row ~action:"search" ~surface:Read ~legacy:Legacy_app
      ~target:Policy.App_installation ~tier:Policy.Low ~pilot_allowed:false
      ~preview:Preview_not_required ~fallback:No_fallback ~delayed:No_delay_pin
      ~receipt:Receipt_app_actor ~webhook:Webhook_ambient
      ~production_requires_user_gate:false ();
    row ~action:"get_status" ~surface:Read ~legacy:Legacy_app
      ~target:Policy.App_installation ~tier:Policy.Low ~pilot_allowed:false
      ~preview:Preview_not_required ~fallback:No_fallback ~delayed:No_delay_pin
      ~receipt:Receipt_app_actor ~webhook:Webhook_ambient
      ~production_requires_user_gate:false ();
    row ~action:"get_item" ~surface:Read ~legacy:Legacy_app
      ~target:Policy.App_installation ~tier:Policy.Low ~pilot_allowed:false
      ~preview:Preview_not_required ~fallback:No_fallback ~delayed:No_delay_pin
      ~receipt:Receipt_app_actor ~webhook:Webhook_ambient
      ~production_requires_user_gate:false ();
    row ~action:"list_room_items" ~surface:Read ~legacy:Legacy_app
      ~target:Policy.App_installation ~tier:Policy.Low ~pilot_allowed:false
      ~preview:Preview_not_required ~fallback:No_fallback ~delayed:No_delay_pin
      ~receipt:Receipt_app_actor ~webhook:Webhook_ambient
      ~production_requires_user_gate:false ();
    (* PAT exact-repo compatibility (not preferred for new mutations) *)
    row ~action:"pat_read" ~surface:Read ~legacy:Legacy_pat
      ~target:Policy.Pat_compat ~tier:Policy.Low ~pilot_allowed:false
      ~preview:Preview_not_required ~fallback:No_fallback ~delayed:No_delay_pin
      ~receipt:Receipt_app_actor ~webhook:Webhook_ambient
      ~production_requires_user_gate:false ();
    (* --- User_preferred ordinary mutations --- *)
    row ~action:"comment" ~surface:Mutation ~legacy:Legacy_app
      ~target:Policy.User_preferred ~tier:Policy.Low ~pilot_allowed:false
      ~preview:Preview_names_actor ~fallback:Visible_app_fallback
      ~delayed:Pin_actor_lineage ~receipt:Receipt_resolved_mode
      ~webhook:Webhook_match_receipt ~production_requires_user_gate:true ();
    row ~action:"label" ~surface:Mutation ~legacy:Legacy_app
      ~target:Policy.User_preferred ~tier:Policy.Medium ~pilot_allowed:false
      ~preview:Preview_names_actor ~fallback:Visible_app_fallback
      ~delayed:Pin_actor_lineage ~receipt:Receipt_resolved_mode
      ~webhook:Webhook_match_receipt ~production_requires_user_gate:true ();
    row ~action:"assign" ~surface:Mutation ~legacy:Legacy_app
      ~target:Policy.User_preferred ~tier:Policy.Medium ~pilot_allowed:false
      ~preview:Preview_names_actor ~fallback:Visible_app_fallback
      ~delayed:Pin_actor_lineage ~receipt:Receipt_resolved_mode
      ~webhook:Webhook_match_receipt ~production_requires_user_gate:true ();
    row ~action:"review_request" ~surface:Mutation ~legacy:Legacy_app
      ~target:Policy.User_preferred ~tier:Policy.Medium ~pilot_allowed:false
      ~preview:Preview_names_actor ~fallback:Visible_app_fallback
      ~delayed:Pin_actor_lineage ~receipt:Receipt_resolved_mode
      ~webhook:Webhook_match_receipt ~production_requires_user_gate:true ();
    (* --- User_required high-risk (P19 pilot interim) --- *)
    row ~action:"review_submit" ~surface:Mutation ~legacy:Legacy_pilot_app
      ~target:Policy.User_required ~tier:Policy.High ~pilot_allowed:true
      ~pilot_name:"p19-pr-review-pilot" ~preview:Preview_user_only
      ~fallback:No_fallback ~delayed:Pin_actor_lineage ~receipt:Receipt_pilot
      ~webhook:Webhook_self_loop_guard ~production_requires_user_gate:true ();
    row ~action:"issue_create" ~surface:Mutation ~legacy:Legacy_pilot_app
      ~target:Policy.User_required ~tier:Policy.High ~pilot_allowed:true
      ~pilot_name:"p19-issue-lifecycle-pilot" ~preview:Preview_user_only
      ~fallback:No_fallback ~delayed:Pin_actor_lineage ~receipt:Receipt_pilot
      ~webhook:Webhook_self_loop_guard ~production_requires_user_gate:true ();
    row ~action:"issue_close" ~surface:Mutation ~legacy:Legacy_pilot_app
      ~target:Policy.User_required ~tier:Policy.High ~pilot_allowed:true
      ~pilot_name:"p19-issue-lifecycle-pilot" ~preview:Preview_user_only
      ~fallback:No_fallback ~delayed:Pin_actor_lineage ~receipt:Receipt_pilot
      ~webhook:Webhook_self_loop_guard ~production_requires_user_gate:true ();
    row ~action:"issue_reopen" ~surface:Mutation ~legacy:Legacy_pilot_app
      ~target:Policy.User_required ~tier:Policy.High ~pilot_allowed:true
      ~pilot_name:"p19-issue-lifecycle-pilot" ~preview:Preview_user_only
      ~fallback:No_fallback ~delayed:Pin_actor_lineage ~receipt:Receipt_pilot
      ~webhook:Webhook_self_loop_guard ~production_requires_user_gate:true ();
    row ~action:"workflow_dispatch" ~surface:Mutation ~legacy:Legacy_pilot_app
      ~target:Policy.User_required ~tier:Policy.Critical ~pilot_allowed:true
      ~pilot_name:"p19-workflow-dispatch-pilot" ~preview:Preview_user_only
      ~fallback:No_fallback ~delayed:Pin_actor_lineage ~receipt:Receipt_pilot
      ~webhook:Webhook_self_loop_guard ~production_requires_user_gate:true ();
    row ~action:"code_change" ~surface:Mutation ~legacy:Legacy_pilot_app
      ~target:Policy.User_required ~tier:Policy.High ~pilot_allowed:true
      ~pilot_name:"p19-code-change-pilot" ~preview:Preview_user_only
      ~fallback:No_fallback ~delayed:Pin_actor_lineage ~receipt:Receipt_pilot
      ~webhook:Webhook_self_loop_guard ~production_requires_user_gate:true ();
    row ~action:"merge" ~surface:Mutation ~legacy:Legacy_pilot_app
      ~target:Policy.User_required ~tier:Policy.Critical ~pilot_allowed:true
      ~pilot_name:"p19-merge-pilot" ~preview:Preview_user_only
      ~fallback:No_fallback ~delayed:Pin_actor_lineage ~receipt:Receipt_pilot
      ~webhook:Webhook_self_loop_guard ~production_requires_user_gate:true ();
    (* --- Background --- *)
    row ~action:"room_background_work" ~surface:Background
      ~legacy:Legacy_pilot_app ~target:Policy.User_required ~tier:Policy.High
      ~pilot_allowed:true ~pilot_name:"p19-room-background-work-pilot"
      ~preview:Preview_user_only ~fallback:No_fallback
      ~delayed:Pin_actor_lineage ~receipt:Receipt_pilot
      ~webhook:Webhook_self_loop_guard ~production_requires_user_gate:true ();
  ]

let normalize_action action =
  let a = String.lowercase_ascii (String.trim action) in
  match a with
  | "submit_review" | "review" -> "review_submit"
  | "code_work" | "pr_create" -> "code_change"
  | "workflow" -> "workflow_dispatch"
  | "collab_comment" -> "comment"
  | "collab_label" -> "label"
  | "collab_assign" | "assignee" | "assignees" -> "assign"
  | "request_review" | "reviewer_request" -> "review_request"
  | "issue_open" | "create_issue" -> "issue_create"
  | "close_issue" -> "issue_close"
  | "reopen_issue" -> "issue_reopen"
  | "background" | "room_background" -> "room_background_work"
  | "status" -> "get_status"
  | "get_item_status" -> "get_status"
  | other -> other

let by_action =
  lazy
    (List.fold_left
       (fun acc (r : matrix_row) -> (r.action, r) :: acc)
       [] (matrix ()))

let fail_closed_row ~action : matrix_row =
  {
    action;
    surface = Mutation;
    legacy = Legacy_denied;
    target = Policy.User_required;
    tier = Policy.Critical;
    pilot_allowed = false;
    pilot_name = None;
    preview = Preview_user_only;
    fallback = No_fallback;
    delayed = Pin_actor_lineage;
    receipt = Receipt_resolved_mode;
    webhook = Webhook_self_loop_guard;
    production_requires_user_gate = true;
  }

let lookup ~action =
  let action = normalize_action action in
  if action = "" then fail_closed_row ~action:""
  else
    match List.assoc_opt action (Lazy.force by_action) with
    | Some r -> r
    | None -> fail_closed_row ~action

let row_to_json (r : matrix_row) =
  `Assoc
    [
      ("action", `String r.action);
      ("surface", `String (surface_to_string r.surface));
      ("legacy", `String (legacy_path_to_string r.legacy));
      ("target", `String (Policy.attribution_to_string r.target));
      ("tier", `String (Policy.risk_tier_to_string r.tier));
      ("pilot_allowed", `Bool r.pilot_allowed);
      ( "pilot_name",
        match r.pilot_name with None -> `Null | Some n -> `String n );
      ("preview", `String (preview_rule_to_string r.preview));
      ("fallback", `String (fallback_rule_to_string r.fallback));
      ("delayed", `String (delayed_rule_to_string r.delayed));
      ("receipt", `String (receipt_rule_to_string r.receipt));
      ("webhook", `String (webhook_rule_to_string r.webhook));
      ("production_requires_user_gate", `Bool r.production_requires_user_gate);
    ]

let matrix_to_json rows =
  `Assoc
    [
      ("matrix_version", `Int matrix_version);
      ("schema_version", `Int schema_version);
      ("rows", `List (List.map row_to_json rows));
    ]

(* -------------------------------------------------------------------------- *)
(* Stages                                                                       *)
(* -------------------------------------------------------------------------- *)

type stage = Safe_default | P19_pilot | P21_production | Rollback | Cleanup

let stage_to_string = function
  | Safe_default -> "safe_default"
  | P19_pilot -> "p19_pilot"
  | P21_production -> "p21_production"
  | Rollback -> "rollback"
  | Cleanup -> "cleanup"

let stage_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "safe_default" | "default" | "safe" -> Ok Safe_default
  | "p19_pilot" | "pilot" -> Ok P19_pilot
  | "p21_production" | "production" -> Ok P21_production
  | "rollback" -> Ok Rollback
  | "cleanup" -> Ok Cleanup
  | other -> Error (Printf.sprintf "unknown rollout stage %S" other)

let default_stage = Safe_default
let stages () = [ Safe_default; P19_pilot; P21_production; Rollback; Cleanup ]

(* -------------------------------------------------------------------------- *)
(* Gates                                                                        *)
(* -------------------------------------------------------------------------- *)

type pilot_gate = {
  enabled : bool;
  pilot_name : string;
  expires_at : string option;
  audit_ref : string option;
}

type production_gate = {
  enabled : bool;
  audit_ref : string option;
  enabled_at : string option;
}

type rollback_gate = {
  active : bool;
  reason : string;
  audit_ref : string option;
  restores_stage : stage;
}

type cleanup_gate = {
  active : bool;
  audit_ref : string option;
  residual_authority_cleared : bool;
  pilot_credentials_destroyed : bool;
  bindings_unlinked : bool;
}

let default_pilot_gate ~pilot_name : pilot_gate =
  { enabled = false; pilot_name; expires_at = None; audit_ref = None }

let default_production_gate : production_gate =
  { enabled = false; audit_ref = None; enabled_at = None }

let default_rollback_gate : rollback_gate =
  {
    active = false;
    reason = "";
    audit_ref = None;
    restores_stage = Safe_default;
  }

let default_cleanup_gate : cleanup_gate =
  {
    active = false;
    audit_ref = None;
    residual_authority_cleared = false;
    pilot_credentials_destroyed = false;
    bindings_unlinked = false;
  }

let default_pilot_gates () =
  matrix ()
  |> List.filter_map (fun (r : matrix_row) ->
      match (r.pilot_allowed, r.pilot_name) with
      | true, Some name -> Some (default_pilot_gate ~pilot_name:name)
      | _ -> None)
  |> List.fold_left
       (fun (acc, seen) (g : pilot_gate) ->
         if List.mem g.pilot_name seen then (acc, seen)
         else (g :: acc, g.pilot_name :: seen))
       ([], [])
  |> fst |> List.rev

let iso_now ~now =
  (* Lightweight ISO-8601 UTC for expiry comparison only (lexicographic works
     for zero-padded UTC forms). Time_util may be unavailable in pure unit
     paths; callers supply comparable ISO strings in expires_at. *)
  let tm = Unix.gmtime now in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let pilot_expired ~now (pilot : pilot_gate) =
  match pilot.expires_at with
  | None -> true (* open-ended while enabled is treated as expired / inactive *)
  | Some exp when String.trim exp = "" -> true
  | Some exp ->
      let now_iso = iso_now ~now in
      String.compare now_iso (String.trim exp) > 0

let pilot_gate_active ~now (pilot : pilot_gate) =
  pilot.enabled && not (pilot_expired ~now pilot)

let pilot_gate_to_json (g : pilot_gate) =
  `Assoc
    [
      ("enabled", `Bool g.enabled);
      ("pilot_name", `String g.pilot_name);
      ( "expires_at",
        match g.expires_at with None -> `Null | Some e -> `String e );
      ("audit_ref", match g.audit_ref with None -> `Null | Some a -> `String a);
    ]

let production_gate_to_json (g : production_gate) =
  `Assoc
    [
      ("enabled", `Bool g.enabled);
      ("audit_ref", match g.audit_ref with None -> `Null | Some a -> `String a);
      ( "enabled_at",
        match g.enabled_at with None -> `Null | Some e -> `String e );
    ]

let rollback_gate_to_json (g : rollback_gate) =
  `Assoc
    [
      ("active", `Bool g.active);
      ("reason", `String g.reason);
      ("audit_ref", match g.audit_ref with None -> `Null | Some a -> `String a);
      ("restores_stage", `String (stage_to_string g.restores_stage));
    ]

let cleanup_gate_to_json (g : cleanup_gate) =
  `Assoc
    [
      ("active", `Bool g.active);
      ("audit_ref", match g.audit_ref with None -> `Null | Some a -> `String a);
      ("residual_authority_cleared", `Bool g.residual_authority_cleared);
      ("pilot_credentials_destroyed", `Bool g.pilot_credentials_destroyed);
      ("bindings_unlinked", `Bool g.bindings_unlinked);
    ]

(* -------------------------------------------------------------------------- *)
(* Readiness                                                                    *)
(* -------------------------------------------------------------------------- *)

type readiness = {
  principal_ready : bool;
  vault_ready : bool;
  policy_ready : bool;
  private_delivery_ready : bool;
  repair_ready : bool;
  backout_ready : bool;
}

let empty_readiness : readiness =
  {
    principal_ready = false;
    vault_ready = false;
    policy_ready = false;
    private_delivery_ready = false;
    repair_ready = false;
    backout_ready = false;
  }

let all_ready : readiness =
  {
    principal_ready = true;
    vault_ready = true;
    policy_ready = true;
    private_delivery_ready = true;
    repair_ready = true;
    backout_ready = true;
  }

let readiness_complete (r : readiness) =
  r.principal_ready && r.vault_ready && r.policy_ready
  && r.private_delivery_ready && r.repair_ready && r.backout_ready

let readiness_missing (r : readiness) =
  let check name ok acc = if ok then acc else name :: acc in
  []
  |> check "principal_ready" r.principal_ready
  |> check "vault_ready" r.vault_ready
  |> check "policy_ready" r.policy_ready
  |> check "private_delivery_ready" r.private_delivery_ready
  |> check "repair_ready" r.repair_ready
  |> check "backout_ready" r.backout_ready
  |> List.rev

let readiness_to_json (r : readiness) =
  `Assoc
    [
      ("principal_ready", `Bool r.principal_ready);
      ("vault_ready", `Bool r.vault_ready);
      ("policy_ready", `Bool r.policy_ready);
      ("private_delivery_ready", `Bool r.private_delivery_ready);
      ("repair_ready", `Bool r.repair_ready);
      ("backout_ready", `Bool r.backout_ready);
      ("complete", `Bool (readiness_complete r));
    ]

(* -------------------------------------------------------------------------- *)
(* Resolve effective path                                                       *)
(* -------------------------------------------------------------------------- *)

type effective_path =
  | Path_app_primary
  | Path_pat_compat
  | Path_user
  | Path_visible_app_fallback
  | Path_pilot_app
  | Path_denied of { code : string; message : string }

let effective_path_to_string = function
  | Path_app_primary -> "path_app_primary"
  | Path_pat_compat -> "path_pat_compat"
  | Path_user -> "path_user"
  | Path_visible_app_fallback -> "path_visible_app_fallback"
  | Path_pilot_app -> "path_pilot_app"
  | Path_denied { code; _ } -> "path_denied:" ^ code

let effective_path_to_json = function
  | Path_app_primary ->
      `Assoc
        [
          ("path", `String "app_primary");
          ("schema_version", `Int schema_version);
        ]
  | Path_pat_compat ->
      `Assoc
        [
          ("path", `String "pat_compat"); ("schema_version", `Int schema_version);
        ]
  | Path_user ->
      `Assoc
        [ ("path", `String "user"); ("schema_version", `Int schema_version) ]
  | Path_visible_app_fallback ->
      `Assoc
        [
          ("path", `String "visible_app_fallback");
          ("schema_version", `Int schema_version);
        ]
  | Path_pilot_app ->
      `Assoc
        [
          ("path", `String "pilot_app"); ("schema_version", `Int schema_version);
        ]
  | Path_denied { code; message } ->
      `Assoc
        [
          ("path", `String "denied");
          ("code", `String code);
          ("message", `String message);
          ("schema_version", `Int schema_version);
        ]

type resolve_input = {
  action : string;
  stage : stage;
  production : production_gate;
  pilot_gates : pilot_gate list;
  readiness : readiness;
  now : float;
  user_auth_available : bool;
}

let default_resolve_input ~action ?(stage = Safe_default)
    ?(production = default_production_gate)
    ?(pilot_gates = default_pilot_gates ()) ?(readiness = empty_readiness)
    ?(now = 0.) ?(user_auth_available = false) () : resolve_input =
  {
    action;
    stage;
    production;
    pilot_gates;
    readiness;
    now;
    user_auth_available;
  }

let find_pilot ~name (gates : pilot_gate list) =
  List.find_opt
    (fun (g : pilot_gate) ->
      String.equal
        (String.lowercase_ascii (String.trim g.pilot_name))
        (String.lowercase_ascii (String.trim name)))
    gates

let deny ~code ~message = Path_denied { code; message }

let pilot_path_for_row ~now (row : matrix_row) (gates : pilot_gate list) =
  match row.pilot_name with
  | None -> None
  | Some name -> (
      match find_pilot ~name gates with
      | Some g when pilot_gate_active ~now g -> Some Path_pilot_app
      | _ -> None)

let resolve_user_attributed (inp : resolve_input) (row : matrix_row) :
    effective_path =
  (* Rollback / cleanup never open production or pilot; drain / reconfirm only. *)
  match inp.stage with
  | Rollback ->
      deny ~code:"rollout_rollback_active"
        ~message:
          "Attribution rollout is rolling back to the safe disabled state. \
           High-risk and user-attributed work is denied without actor-mode \
           substitution. Complete cleanup after disable."
  | Cleanup ->
      deny ~code:"rollout_cleanup_active"
        ~message:
          "Attribution rollout cleanup is active. Gates must remain off until \
           residual authority is proven cleared."
  | Safe_default | P19_pilot | P21_production -> (
      let production_ok =
        inp.production.enabled && readiness_complete inp.readiness
      in
      let denied_without_pilot () =
        if row.pilot_allowed then
          let unavail =
            if not inp.user_auth_available then
              " P21 user authorization disabled/unavailable; no App/PAT \
               fallback."
            else if not inp.production.enabled then
              " Production attribution gate is off."
            else
              let missing =
                String.concat ", " (readiness_missing inp.readiness)
              in
              Printf.sprintf " Production readiness incomplete (%s)." missing
          in
          deny ~code:"user_required_gate_disabled"
            ~message:
              (Printf.sprintf
                 "Action %S requires User_required attribution or an active \
                  named P19 pilot. Pilot is off/expired.%s"
                 row.action unavail)
        else
          deny ~code:"attribution_gate_disabled"
            ~message:
              (Printf.sprintf
                 "Action %S requires the P21 user-attribution gate and \
                  readiness. User_preferred/User_required work cannot fall \
                  back to App or PAT while the gate is off or readiness is \
                  incomplete."
                 row.action)
      in
      if not production_ok then
        match pilot_path_for_row ~now:inp.now row inp.pilot_gates with
        | Some p when row.pilot_allowed -> p
        | _ -> denied_without_pilot ()
      else if inp.stage = P21_production then
        match row.target with
        | Policy.User_required | Policy.User_preferred -> Path_user
        | Policy.App_installation | Policy.Pat_compat -> denied_without_pilot ()
      else
        (* Production gate on but stage not yet production. *)
        match row.target with
        | Policy.User_preferred -> Path_user
        | Policy.User_required -> (
            match pilot_path_for_row ~now:inp.now row inp.pilot_gates with
            | Some p -> p
            | None ->
                deny ~code:"production_stage_required"
                  ~message:
                    "User_required production path requires the p21_production \
                     stage after readiness and audited enablement. Pilot App \
                     path is off or expired.")
        | Policy.App_installation | Policy.Pat_compat -> denied_without_pilot ()
      )

let resolve (inp : resolve_input) : effective_path =
  let row = lookup ~action:inp.action in
  match row.target with
  | Policy.App_installation -> Path_app_primary
  | Policy.Pat_compat -> Path_pat_compat
  | Policy.User_preferred | Policy.User_required ->
      resolve_user_attributed inp row

(* -------------------------------------------------------------------------- *)
(* Transitions                                                                  *)
(* -------------------------------------------------------------------------- *)

type gate_kind =
  | Gate_pilot_enable
  | Gate_pilot_disable
  | Gate_production_enable
  | Gate_production_disable
  | Gate_rollback
  | Gate_cleanup

let gate_kind_to_string = function
  | Gate_pilot_enable -> "pilot_enable"
  | Gate_pilot_disable -> "pilot_disable"
  | Gate_production_enable -> "production_enable"
  | Gate_production_disable -> "production_disable"
  | Gate_rollback -> "rollback"
  | Gate_cleanup -> "cleanup"

type transition_request = {
  kind : gate_kind;
  from_stage : stage;
  pilot : pilot_gate option;
  production : production_gate option;
  rollback : rollback_gate option;
  cleanup : cleanup_gate option;
  readiness : readiness;
  audit_ref : string option;
}

type transition_result = {
  to_stage : stage;
  production : production_gate;
  message : string;
}

let non_empty_opt = function None -> false | Some s -> String.trim s <> ""

let audit_ok req_audit gate_audit =
  non_empty_opt req_audit || non_empty_opt gate_audit

let validate_transition (req : transition_request) :
    (transition_result, string) result =
  let prod_or_default =
    match req.production with Some p -> p | None -> default_production_gate
  in
  match req.kind with
  | Gate_pilot_enable -> (
      match req.pilot with
      | None -> Error "pilot_enable requires pilot gate payload"
      | Some pilot ->
          if not pilot.enabled then
            Error "pilot_enable requires pilot.enabled=true"
          else if String.trim pilot.pilot_name = "" then
            Error "pilot_enable requires non-empty pilot_name"
          else if not (non_empty_opt pilot.expires_at) then
            Error
              "pilot_enable requires non-empty expires_at (never open-ended)"
          else if not (audit_ok req.audit_ref pilot.audit_ref) then
            Error "pilot_enable requires audit_ref"
          else if
            match req.from_stage with
            | Safe_default | P19_pilot -> false
            | P21_production | Rollback | Cleanup -> true
          then
            Error
              "pilot_enable only from safe_default or p19_pilot (not from \
               production/rollback/cleanup)"
          else
            Ok
              {
                to_stage = P19_pilot;
                production = { prod_or_default with enabled = false };
                message =
                  Printf.sprintf
                    "Enabled named P19 pilot %S until %s. Production \
                     attribution remains off; App pilot is interim only."
                    pilot.pilot_name
                    (match pilot.expires_at with Some e -> e | None -> "?");
              })
  | Gate_pilot_disable ->
      if
        match req.from_stage with
        | P19_pilot | Safe_default -> false
        | _ -> true
      then Error "pilot_disable only from p19_pilot or safe_default"
      else
        Ok
          {
            to_stage = Safe_default;
            production = { prod_or_default with enabled = false };
            message =
              "Disabled P19 pilot gate(s). High-risk actions fail closed; no \
               App/PAT fallback.";
          }
  | Gate_production_enable ->
      if not (readiness_complete req.readiness) then
        Error
          (Printf.sprintf
             "production_enable requires complete readiness; missing: %s"
             (String.concat ", " (readiness_missing req.readiness)))
      else if not prod_or_default.enabled then
        Error "production_enable requires production.enabled=true"
      else if not (audit_ok req.audit_ref prod_or_default.audit_ref) then
        Error "production_enable requires audit_ref"
      else if
        match req.from_stage with
        | Safe_default | P19_pilot | P21_production -> false
        | Rollback | Cleanup -> true
      then
        Error
          "production_enable not allowed during rollback/cleanup; finish \
           cleanup first"
      else
        Ok
          {
            to_stage = P21_production;
            production = prod_or_default;
            message =
              "Enabled P21 production user-attribution gate after readiness. \
               User_required never falls back to App/PAT; User_preferred App \
               fallback only when preview names App.";
          }
  | Gate_production_disable ->
      Ok
        {
          to_stage = Safe_default;
          production =
            { enabled = false; audit_ref = req.audit_ref; enabled_at = None };
          message =
            "Disabled P21 production attribution gate. Restored safe default \
             without actor-mode substitution.";
        }
  | Gate_rollback -> (
      match req.rollback with
      | None -> Error "rollback requires rollback gate payload"
      | Some rb ->
          if not rb.active then Error "rollback requires rollback.active=true"
          else if String.trim rb.reason = "" then
            Error "rollback requires non-empty reason"
          else if rb.restores_stage <> Safe_default then
            Error "rollback must restore safe_default (no actor substitution)"
          else if not (audit_ok req.audit_ref rb.audit_ref) then
            Error "rollback requires audit_ref"
          else
            Ok
              {
                to_stage = Rollback;
                production =
                  {
                    enabled = false;
                    audit_ref = req.audit_ref;
                    enabled_at = None;
                  };
                message =
                  Printf.sprintf
                    "Rollback active: %s. Production and pilot paths closed; \
                     no actor-mode substitution. Proceed to cleanup."
                    rb.reason;
              })
  | Gate_cleanup -> (
      match req.cleanup with
      | None -> Error "cleanup requires cleanup gate payload"
      | Some c ->
          if not c.active then Error "cleanup requires cleanup.active=true"
          else if not c.residual_authority_cleared then
            Error "cleanup requires residual_authority_cleared=true"
          else if not c.pilot_credentials_destroyed then
            Error "cleanup requires pilot_credentials_destroyed=true"
          else if not c.bindings_unlinked then
            Error
              "cleanup requires bindings_unlinked=true (or N/A asserted true \
               for App-only pilot)"
          else if not (audit_ok req.audit_ref c.audit_ref) then
            Error "cleanup requires audit_ref"
          else
            Ok
              {
                to_stage = Safe_default;
                production =
                  {
                    enabled = false;
                    audit_ref = req.audit_ref;
                    enabled_at = None;
                  };
                message =
                  "Cleanup complete: residual authority cleared; pilot \
                   credentials destroyed; bindings unlinked. Safe default \
                   restored.";
              })

let cleanup_complete (c : cleanup_gate) =
  c.active && c.residual_authority_cleared && c.pilot_credentials_destroyed
  && c.bindings_unlinked

let no_residual_authority ~production ~pilot_gates ~now ~cleanup =
  (not production.enabled)
  && List.for_all (fun g -> not (pilot_gate_active ~now g)) pilot_gates
  && cleanup.residual_authority_cleared && cleanup.pilot_credentials_destroyed
  && cleanup.bindings_unlinked

(* -------------------------------------------------------------------------- *)
(* Invariants                                                                   *)
(* -------------------------------------------------------------------------- *)

let matrix_covers_policy_defaults () =
  let rows = matrix () in
  let find_row action =
    List.find_opt (fun (r : matrix_row) -> r.action = action) rows
  in
  let rec check = function
    | [] -> Ok ()
    | (req : Policy.requirement) :: rest -> (
        match find_row req.action with
        | None ->
            Error
              (Printf.sprintf "matrix missing policy default action %S"
                 req.action)
        | Some row ->
            if row.target <> req.attribution then
              Error
                (Printf.sprintf
                   "matrix target mismatch for %S: matrix=%s policy=%s"
                   req.action
                   (Policy.attribution_to_string row.target)
                   (Policy.attribution_to_string req.attribution))
            else if row.tier <> req.tier then
              Error
                (Printf.sprintf
                   "matrix tier mismatch for %S: matrix=%s policy=%s" req.action
                   (Policy.risk_tier_to_string row.tier)
                   (Policy.risk_tier_to_string req.tier))
            else if row.pilot_allowed <> req.pilot_allowed then
              Error
                (Printf.sprintf "matrix pilot_allowed mismatch for %S"
                   req.action)
            else check rest)
  in
  check (Policy.defaults ())

let user_required_disabled_by_default () =
  (not default_production_gate.enabled) && default_stage = Safe_default

let transition_weakens_confirmation ~from_stage ~to_stage =
  (* Silent weakenings that must never occur without validate_transition. *)
  match (from_stage, to_stage) with
  | Cleanup, P21_production -> true
  | Rollback, P21_production -> true
  | Rollback, P19_pilot -> true
  | Cleanup, P19_pilot -> true
  | Safe_default, P21_production -> false (* ok only via audited gate *)
  | _ -> false

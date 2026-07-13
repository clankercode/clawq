(* User-required PR merge attribution (P21.M3.E3.T009).
   See github_merge_attribution.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Auth = Github_attribution_authorize
module Dispatch = Github_attribution_dispatch_lease
module Audit = Github_attribution_audit
module Policy = Github_attribution_policy
module Merge = Github_merge_action
module V = Github_user_token_vault
module Token_lease = Github_user_token_lease

let schema_version = 1
let field_attribution_allow = "attribution_allow"
let field_requested_mode = "requested_mode"
let field_resolved_mode = "resolved_mode"
let field_used_app_fallback = "used_app_fallback"
let field_attribution = "attribution"
let field_live_revalidation = "merge_live"
let policy_action_merge = "merge"

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

let empty_policy : Merge.live_policy =
  {
    head_sha = "";
    is_draft = true;
    mergeable = false;
    required_checks_ok = false;
    required_reviews_ok = false;
    branch_policy_ok = false;
    allowed_methods = [];
    actor_mode = Merge.User;
    authority_ok = false;
  }

type live_revalidation = {
  item_present : bool;
  already_applied : bool;
  policy : Merge.live_policy;
}

let default_live_revalidation ?(policy = empty_policy) () : live_revalidation =
  { item_present = true; already_applied = false; policy }

let revalidate_live ~(req : Merge.merge_request) ~(live : live_revalidation) =
  if live.already_applied then
    Error
      "duplicate or replay: this merge was already applied; refuse silent \
       re-dispatch"
  else if not live.item_present then
    Error
      "target PR is missing or no longer addressable; re-resolve the item and \
       re-preview"
  else
    Merge.revalidate_for_apply ~planned_head_sha:req.head_sha
      ~planned_method:req.method_ ~current:live.policy

let live_action_evidence ~req ~live : Auth.live_action_evidence =
  match revalidate_live ~req ~live with
  | Ok () ->
      let rev = String.trim live.policy.head_sha in
      {
        ok = true;
        revision = (if rev = "" then None else Some rev);
        detail = None;
      }
  | Error detail ->
      let rev = String.trim live.policy.head_sha in
      {
        ok = false;
        revision =
          (if rev = "" then Some (String.trim req.head_sha) else Some rev);
        detail = Some detail;
      }

let authorize_capability ~route ~pilot ~user_auth_available ~req ~policy
    ?(now = Unix.gettimeofday ()) () =
  Merge.authorize_merge ~route ~pilot ~user_auth_available ~req ~policy ~now ()

type preview_ok = {
  allow : Auth.allow;
  decision : Auth.decision;
  audit : Audit.t;
  policy_action : string;
  used_app_fallback : bool;
  mode : Auth.resolved_mode;
}

type preview_deny = {
  reason : string;
  decision : Auth.decision option;
  audit : Audit.t option;
  policy_action : string;
  failed_check : string option;
  failure_code : string option;
}

let string_of_preview_deny (d : preview_deny) =
  match (d.failed_check, d.failure_code) with
  | Some c, Some code ->
      Printf.sprintf "preview_deny action=%s check=%s code=%s: %s"
        d.policy_action c code d.reason
  | _ -> Printf.sprintf "preview_deny action=%s: %s" d.policy_action d.reason

let force_action (auth : Auth.request) ~action : Auth.request =
  { auth with action }

let merge_live_into_auth (auth : Auth.request) ~(req : Merge.merge_request)
    ~(live : live_revalidation) : Auth.request =
  let live_action = live_action_evidence ~req ~live in
  { auth with live_action }

let deny_preview ~policy_action ~reason ?decision ?audit ?failed_check
    ?failure_code () : (preview_ok, preview_deny) result =
  Error { reason; decision; audit; policy_action; failed_check; failure_code }

let authorize_preview ~db ~route ~pilot ~user_auth_available
    ~(req : Merge.merge_request) ~(auth : Auth.request)
    ~(live : live_revalidation) ?room_id ?plan_id ?actor_snapshot
    ?github_user_id ?(now = Unix.gettimeofday ()) () =
  let policy_action = policy_action_merge in
  let item_key =
    let t = String.trim req.Merge.item_key in
    if t = "" then None else Some t
  in
  match
    authorize_capability ~route ~pilot ~user_auth_available ~req
      ~policy:live.policy ~now ()
  with
  | Error reason ->
      let audit =
        match
          Audit.record_repair ~db ~action:policy_action ~reason
            ~failure_class:Audit.Policy ~failure_code:"capability_denied"
            ?item_key ?room_id ?plan_id ~now ()
        with
        | Ok a -> Some a
        | Error _ -> None
      in
      deny_preview ~policy_action ~reason ?audit ~failed_check:"capability"
        ~failure_code:"capability_denied" ()
  | Ok () -> (
      match revalidate_live ~req ~live with
      | Error reason ->
          let audit =
            match
              Audit.record_repair ~db ~action:policy_action ~reason
                ~failure_class:Audit.Live_state
                ~failure_code:"live_state_failed" ?item_key ?room_id ?plan_id
                ~now ()
            with
            | Ok a -> Some a
            | Error _ -> None
          in
          deny_preview ~policy_action ~reason ?audit ~failed_check:"live_action"
            ~failure_code:"live_state_failed" ()
      | Ok () -> (
          let auth =
            force_action
              (merge_live_into_auth auth ~req ~live)
              ~action:policy_action
          in
          let decision = Auth.authorize auth in
          match decision with
          | Auth.Deny d ->
              let audit =
                match
                  Audit.record_authorize_decision ~db ~decision
                    ~kind:Audit.Repair_state ?item_key ?room_id ?plan_id
                    ?actor_snapshot ?github_user_id ~now ()
                with
                | Ok a -> Some a
                | Error _ -> None
              in
              deny_preview ~policy_action ~reason:d.repair.message ~decision
                ?audit ~failed_check:d.failed_check ~failure_code:d.repair.code
                ()
          | Auth.Allow allow -> (
              (* Production User_required path: never accept App as resolved
                 mode. P19 pilot App uses Merge.authorize_merge without this
                 path. *)
              match allow.mode with
              | Auth.App ->
                  let reason =
                    Printf.sprintf
                      "%s is User_required: App/PAT attribution is forbidden \
                       on the production path. Use a current Principal user \
                       lease, or the named P19 pilot App path explicitly."
                      policy_action
                  in
                  let audit =
                    match
                      Audit.record_repair ~db ~action:policy_action ~reason
                        ~failure_class:Audit.Fallback
                        ~failure_code:"app_fallback_forbidden" ?item_key
                        ?room_id ?plan_id
                        ~requested_mode:
                          (Policy.attribution_to_string
                             allow.requirement.attribution)
                        ~resolved_mode:"app" ~used_app_fallback:false ~now ()
                    with
                    | Ok a -> Some a
                    | Error _ -> None
                  in
                  deny_preview ~policy_action ~reason ~decision ?audit
                    ~failed_check:"fallback"
                    ~failure_code:"app_fallback_forbidden" ()
              | Auth.User -> (
                  let audit_res =
                    Audit.record_authorize_decision ~db ~decision
                      ~kind:Audit.Preview ?item_key ?room_id ?plan_id
                      ?actor_snapshot ?github_user_id ~now ()
                  in
                  match audit_res with
                  | Error e ->
                      deny_preview ~policy_action
                        ~reason:("attribution audit failed: " ^ e)
                        ~decision ()
                  | Ok audit ->
                      Ok
                        {
                          allow;
                          decision;
                          audit;
                          policy_action;
                          used_app_fallback = allow.used_app_fallback;
                          mode = allow.mode;
                        }))))

type dispatch_ok = {
  issued : Dispatch.issued;
  receipt : Audit.t;
  policy_action : string;
  mode : Auth.resolved_mode;
  has_user_lease : bool;
}

type dispatch_deny = {
  reason : string;
  denial : Dispatch.denial option;
  audit : Audit.t option;
  policy_action : string;
}

let string_of_dispatch_deny (d : dispatch_deny) =
  match d.denial with
  | Some den ->
      Printf.sprintf "dispatch_deny action=%s: %s (%s)" d.policy_action d.reason
        (Dispatch.string_of_denial den)
  | None ->
      Printf.sprintf "dispatch_deny action=%s: %s" d.policy_action d.reason

let deny_dispatch ~policy_action ~reason ?denial ?audit () :
    (dispatch_ok, dispatch_deny) result =
  Error { reason; denial; audit; policy_action }

let enforce_dispatch_mode ~(issued : Dispatch.issued) ~policy_action =
  match (issued.mode, issued.lease) with
  | Auth.User, Some _ -> Ok ()
  | Auth.User, None ->
      Error
        (Printf.sprintf
           "%s requires a current Principal user lease at dispatch; no lease \
            was issued"
           policy_action)
  | Auth.App, _ ->
      Error
        (Printf.sprintf
           "%s is User_required: App/PAT fallback is forbidden at dispatch"
           policy_action)

let dispatch ~db ~(req : Merge.merge_request) ~(live_auth : Auth.request)
    ~(prior : Auth.allow) ~(live : live_revalidation) ?vault_id ?expected
    ?room_id ?plan_id ?receipt_id ?actor_snapshot ?github_user_id
    ?(now = Unix.gettimeofday ()) ?ttl_seconds () =
  let policy_action = policy_action_merge in
  let item_key =
    let t = String.trim req.Merge.item_key in
    if t = "" then None else Some t
  in
  match revalidate_live ~req ~live with
  | Error reason ->
      let audit =
        match
          Audit.record_repair ~db ~action:policy_action ~reason
            ~failure_class:Audit.Live_state ~failure_code:"live_state_failed"
            ?item_key ?room_id ?plan_id ~now ()
        with
        | Ok a -> Some a
        | Error _ -> None
      in
      deny_dispatch ~policy_action ~reason ?audit ()
  | Ok () -> (
      let live_auth =
        force_action
          (merge_live_into_auth live_auth ~req ~live)
          ~action:policy_action
      in
      match
        Dispatch.issue_for_dispatch ~db ~now ?ttl_seconds ~live:live_auth ~prior
          ?vault_id ?expected ()
      with
      | Error denial ->
          let reason = Dispatch.string_of_denial denial in
          let fc =
            match denial with
            | Dispatch.Authorization d ->
                Audit.classify_failure ~failed_check:d.failed_check
                  ~code:d.repair.code ()
            | Dispatch.Prior_mode_mismatch _ | Dispatch.Prior_action_mismatch _
            | Dispatch.Prior_principal_mismatch _
            | Dispatch.Prior_binding_mismatch _ ->
                Audit.Identity
            | Dispatch.Generation_race _ -> Audit.Refresh
            | Dispatch.User_lease_requires_vault_id | Dispatch.Lease _ ->
                Audit.Revocation
            | Dispatch.Invalid_input _ -> Audit.Policy
          in
          let code =
            match denial with
            | Dispatch.Authorization d -> d.repair.code
            | Dispatch.Prior_mode_mismatch _ -> "prior_mode_mismatch"
            | Dispatch.Prior_action_mismatch _ -> "prior_action_mismatch"
            | Dispatch.Prior_principal_mismatch _ -> "prior_principal_mismatch"
            | Dispatch.Prior_binding_mismatch _ -> "prior_binding_mismatch"
            | Dispatch.User_lease_requires_vault_id ->
                "user_lease_requires_vault"
            | Dispatch.Generation_race _ -> "generation_race"
            | Dispatch.Lease _ -> "lease_denied"
            | Dispatch.Invalid_input _ -> "invalid_input"
          in
          let audit =
            match
              Audit.record_repair ~db ~action:policy_action ~reason
                ~failure_class:fc ~failure_code:code ?item_key ?room_id ?plan_id
                ~requested_mode:
                  (Policy.attribution_to_string prior.requirement.attribution)
                ~resolved_mode:(Auth.resolved_mode_to_string prior.mode)
                ~now ()
            with
            | Ok a -> Some a
            | Error _ -> None
          in
          deny_dispatch ~policy_action ~reason ~denial ?audit ()
      | Ok issued -> (
          match enforce_dispatch_mode ~issued ~policy_action with
          | Error reason ->
              let audit =
                match
                  Audit.record_repair ~db ~action:policy_action ~reason
                    ~failure_class:Audit.Fallback
                    ~failure_code:"dispatch_mode_forbidden" ?item_key ?room_id
                    ?plan_id
                    ~resolved_mode:(Auth.resolved_mode_to_string issued.mode)
                    ~used_app_fallback:issued.decision.used_app_fallback ~now ()
                with
                | Ok a -> Some a
                | Error _ -> None
              in
              deny_dispatch ~policy_action ~reason ?audit ()
          | Ok () -> (
              let has_user_lease = Option.is_some issued.lease in
              let mode_s = Auth.resolved_mode_to_string issued.mode in
              let reason =
                Printf.sprintf
                  "dispatch ok action=%s mode=%s user_lease=%b item=%s head=%s"
                  policy_action mode_s has_user_lease
                  (match item_key with Some k -> k | None -> "")
                  (String.trim req.Merge.head_sha)
              in
              let result =
                if issued.decision.used_app_fallback then Audit.Fallback_app
                else Audit.Completed
              in
              let github_actor =
                Audit.github_actor_of_revisions issued.decision.revisions
                  ?binding_github_user_id:github_user_id ()
              in
              let lineage =
                Audit.lineage_of_checked_revisions issued.decision.revisions
              in
              match
                Audit.record_receipt ~db ~action:policy_action ~reason ~result
                  ?item_key ?room_id ?plan_id ?receipt_id
                  ~requested_mode:
                    (Policy.attribution_to_string
                       issued.decision.requirement.attribution)
                  ~resolved_mode:mode_s
                  ~used_app_fallback:issued.decision.used_app_fallback
                  ~github_actor ~lineage ?actor_snapshot
                  ?actor_snapshot_id:issued.decision.revisions.actor_snapshot_id
                  ~revisions_json:
                    (Yojson.Safe.to_string
                       (Auth.checked_revisions_to_json issued.decision.revisions))
                  ~now ()
              with
              | Error e ->
                  deny_dispatch ~policy_action
                    ~reason:("receipt audit failed: " ^ e)
                    ()
              | Ok receipt ->
                  Ok
                    {
                      issued;
                      receipt;
                      policy_action;
                      mode = issued.mode;
                      has_user_lease;
                    })))

(* -------------------------------------------------------------------------- *)
(* Prior Allow JSON (plan pin)                                                 *)
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

let live_to_json (l : live_revalidation) =
  `Assoc
    (sort_assoc
       [
         ("item_present", `Bool l.item_present);
         ("already_applied", `Bool l.already_applied);
         ("head_sha", `String l.policy.head_sha);
         ("is_draft", `Bool l.policy.is_draft);
         ("mergeable", `Bool l.policy.mergeable);
         ("required_checks_ok", `Bool l.policy.required_checks_ok);
         ("required_reviews_ok", `Bool l.policy.required_reviews_ok);
         ("branch_policy_ok", `Bool l.policy.branch_policy_ok);
         ("authority_ok", `Bool l.policy.authority_ok);
         ("actor_mode", `String (Merge.actor_mode_to_string l.policy.actor_mode));
       ])

let attach_allow_to_plan ~plan ~(allow : Auth.allow) ?live () =
  let allow_json = allow_to_json allow in
  let requested = Policy.attribution_to_string allow.requirement.attribution in
  let resolved = Auth.resolved_mode_to_string allow.mode in
  let extras =
    [
      (field_attribution_allow, allow_json);
      (field_requested_mode, `String requested);
      (field_resolved_mode, `String resolved);
      (field_used_app_fallback, `Bool allow.used_app_fallback);
      (field_attribution, `String resolved);
      ("attribution_policy", `String requested);
      ("attribution_schema_version", `Int schema_version);
      ("policy_action", `String allow.requirement.action);
    ]
    @
    match live with
    | None -> []
    | Some l -> [ (field_live_revalidation, live_to_json l) ]
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
                 revalidate live merge policy and issue opaque lease at \
                 apply/dispatch. No raw token on plan."
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

let is_merge_attribution_plan (plan : Setup_plan.t) = Merge.is_merge_plan plan

let has_attribution_allow (plan : Setup_plan.t) =
  is_merge_attribution_plan plan
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

type planned = { plan : Setup_plan.t; preview : preview_ok }

let plan_with_attribution ~db ~principal ~room_id ~(req : Merge.merge_request)
    ~(policy : Merge.live_policy) ~base_revision ~(auth : Auth.request)
    ~(live : live_revalidation) ~route ~pilot ~user_auth_available
    ?actor_snapshot ?github_user_id ?(now = Unix.gettimeofday ()) () =
  match
    authorize_preview ~db ~route ~pilot ~user_auth_available ~req ~auth ~live
      ~room_id ?actor_snapshot ?github_user_id ~now ()
  with
  | Error d -> Error (string_of_preview_deny d)
  | Ok preview -> (
      let plan_res =
        match route with
        | Some r ->
            Merge.plan_merge ~db ~principal ~room_id ~pilot ~user_auth_available
              ~req ~policy ~base_revision ~route:r ~now ()
        | None ->
            Merge.plan_merge ~db ~principal ~room_id ~pilot ~user_auth_available
              ~req ~policy ~base_revision ~now ()
      in
      match plan_res with
      | Error e -> Error e
      | Ok plan -> (
          let plan = attach_allow_to_plan ~plan ~allow:preview.allow ~live () in
          let item_key =
            let t = String.trim req.Merge.item_key in
            if t = "" then None else Some t
          in
          let _ =
            Audit.record_authorize_decision ~db ~decision:preview.decision
              ~kind:Audit.Audit ?item_key ~room_id ~plan_id:plan.id
              ?actor_snapshot ?github_user_id ~now ()
          in
          match Setup_plan_apply.replace_pending_plan ~db plan with
          | Error e -> Error e
          | Ok () -> Ok { plan; preview }))

let req_of_plan (plan : Setup_plan.t) : (Merge.merge_request, string) result =
  let data = plan.apply_payload.data in
  let action_fields =
    match member_opt "action" data with
    | Some (`Assoc fields) -> Some fields
    | _ -> None
  in
  let field name =
    match action_fields with
    | Some fields -> (
        match List.assoc_opt name fields with
        | Some (`String s) -> Some s
        | _ -> None)
    | None -> None
  in
  if not (Merge.is_merge_plan plan) then Error "plan is not a github_merge plan"
  else
    let item_key =
      match field "item_key" with
      | Some s -> s
      | None -> Option.value (get_string "item_key" data) ~default:""
    in
    let head_sha =
      match field "head_sha" with
      | Some s -> s
      | None -> Option.value (get_string "head_sha" data) ~default:""
    in
    let method_s =
      match field "merge_method" with
      | Some s -> s
      | None -> Option.value (get_string "merge_method" data) ~default:""
    in
    match Merge.merge_method_of_string method_s with
    | Error e -> Error e
    | Ok method_ ->
        Ok
          {
            Merge.item_key;
            method_;
            head_sha;
            commit_title = field "commit_title";
            commit_message = field "commit_message";
          }

let prepare_dispatch_from_plan ~db ~plan ~live_auth ~live ?vault_id ?expected
    ?receipt_id ?actor_snapshot ?github_user_id ?(now = Unix.gettimeofday ()) ()
    =
  match allow_of_plan plan with
  | Error e -> Error e
  | Ok None ->
      Error
        "plan has no staged attribution_allow; cannot prepare merge dispatch"
  | Ok (Some prior) -> (
      match req_of_plan plan with
      | Error e -> Error e
      | Ok req -> (
          match
            dispatch ~db ~req ~live_auth ~prior ~live ?vault_id ?expected
              ?room_id:plan.destination.room_id ~plan_id:plan.id ?receipt_id
              ?actor_snapshot ?github_user_id ~now ()
          with
          | Ok d -> Ok d
          | Error d -> Error (string_of_dispatch_deny d)))

let revoke_issued_lease (issued : Dispatch.issued) =
  match issued.lease with Some l -> Token_lease.revoke l | None -> ()

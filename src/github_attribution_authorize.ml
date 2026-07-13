(* Resolve attribution authorization after all current policy checks
   (P21.M3.E2.T003). See github_attribution_authorize.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Policy = Github_attribution_policy

let schema_version = 1

type resolved_mode = App | User

let resolved_mode_to_string = function App -> "app" | User -> "user"

type checked_revisions = {
  policy_action : string;
  requirement_attribution : string;
  requirement_tier : string;
  tool_catalog_revision : string option;
  access_revision : string option;
  principal_id : string option;
  principal_revision : int option;
  actor_revision : int option;
  identity_link_revision : int option;
  binding_id : string option;
  binding_lineage_id : string option;
  vault_generation : int option;
  installation_id : int option;
  installation_revision : string option;
  confirmation_id : string option;
  actor_snapshot_id : string option;
  live_state_revision : string option;
}

let empty_checked_revisions ~policy_action ~requirement_attribution
    ~requirement_tier =
  {
    policy_action;
    requirement_attribution;
    requirement_tier;
    tool_catalog_revision = None;
    access_revision = None;
    principal_id = None;
    principal_revision = None;
    actor_revision = None;
    identity_link_revision = None;
    binding_id = None;
    binding_lineage_id = None;
    vault_generation = None;
    installation_id = None;
    installation_revision = None;
    confirmation_id = None;
    actor_snapshot_id = None;
    live_state_revision = None;
  }

let opt_string name = function
  | None -> (name, `Null)
  | Some s -> (name, `String s)

let opt_int name = function None -> (name, `Null) | Some n -> (name, `Int n)

let checked_revisions_to_json (r : checked_revisions) =
  `Assoc
    [
      ("schema_version", `Int schema_version);
      ("policy_action", `String r.policy_action);
      ("requirement_attribution", `String r.requirement_attribution);
      ("requirement_tier", `String r.requirement_tier);
      opt_string "tool_catalog_revision" r.tool_catalog_revision;
      opt_string "access_revision" r.access_revision;
      opt_string "principal_id" r.principal_id;
      opt_int "principal_revision" r.principal_revision;
      opt_int "actor_revision" r.actor_revision;
      opt_int "identity_link_revision" r.identity_link_revision;
      opt_string "binding_id" r.binding_id;
      opt_string "binding_lineage_id" r.binding_lineage_id;
      opt_int "vault_generation" r.vault_generation;
      opt_int "installation_id" r.installation_id;
      opt_string "installation_revision" r.installation_revision;
      opt_string "confirmation_id" r.confirmation_id;
      opt_string "actor_snapshot_id" r.actor_snapshot_id;
      opt_string "live_state_revision" r.live_state_revision;
    ]

type repair = { code : string; message : string }

let repair_to_json (r : repair) =
  `Assoc [ ("code", `String r.code); ("message", `String r.message) ]

type allow = {
  mode : resolved_mode;
  requirement : Policy.requirement;
  revisions : checked_revisions;
  binding_id : string option;
  principal_id : string option;
}

type deny = {
  failed_check : string;
  repair : repair;
  requirement : Policy.requirement option;
  revisions : checked_revisions;
}

type decision = Allow of allow | Deny of deny

let is_allow = function Allow _ -> true | Deny _ -> false
let is_deny = function Deny _ -> true | Allow _ -> false

let decision_to_json = function
  | Allow a ->
      `Assoc
        [
          ("schema_version", `Int schema_version);
          ("decision", `String "allow");
          ("mode", `String (resolved_mode_to_string a.mode));
          ("action", `String a.requirement.action);
          ( "attribution",
            `String (Policy.attribution_to_string a.requirement.attribution) );
          ("tier", `String (Policy.risk_tier_to_string a.requirement.tier));
          opt_string "binding_id" a.binding_id;
          opt_string "principal_id" a.principal_id;
          ("revisions", checked_revisions_to_json a.revisions);
          ("issues_token", `Bool false);
          ("issues_lease", `Bool false);
        ]
  | Deny d ->
      let req_fields =
        match d.requirement with
        | None ->
            [
              ("action", `String d.revisions.policy_action);
              ("attribution", `Null);
              ("tier", `Null);
            ]
        | Some req ->
            [
              ("action", `String req.action);
              ( "attribution",
                `String (Policy.attribution_to_string req.attribution) );
              ("tier", `String (Policy.risk_tier_to_string req.tier));
            ]
      in
      `Assoc
        ([
           ("schema_version", `Int schema_version);
           ("decision", `String "deny");
           ("failed_check", `String d.failed_check);
           ("repair", repair_to_json d.repair);
           ("revisions", checked_revisions_to_json d.revisions);
           ("issues_token", `Bool false);
           ("issues_lease", `Bool false);
         ]
        @ req_fields)

let string_of_decision = function
  | Allow a ->
      Printf.sprintf "allow mode=%s action=%s attribution=%s"
        (resolved_mode_to_string a.mode)
        a.requirement.action
        (Policy.attribution_to_string a.requirement.attribution)
  | Deny d ->
      Printf.sprintf "deny check=%s code=%s" d.failed_check d.repair.code

(* -------------------------------------------------------------------------- *)
(* Injectable evidence                                                         *)
(* -------------------------------------------------------------------------- *)

type tool_catalog_evidence = {
  revision : string;
  access_revision : string;
  tool_authorized : bool;
  room_id : string option;
  session_key : string option;
}

type repo_grant_evidence = {
  repo_full_name : string;
  granted : bool;
  blocked : bool;
  access_revision : string option;
}

type principal_confirmation_evidence = {
  principal_id : string;
  principal_revision : int;
  principal_current_active : bool;
  actor_revision : int option;
  identity_link_revision : int option;
  confirmation_id : string option;
  confirmation_required : bool;
  confirmation_satisfied : bool;
}

type selected_binding = {
  binding_id : string;
  lineage_id : string;
  authorized : bool;
  vault_active : bool;
  vault_generation : int;
  lineage_matches_pin : bool;
}

type binding_resolution =
  | Not_required
  | None_eligible
  | Ambiguous
  | Selected of selected_binding

type binding_lineage_evidence = { resolution : binding_resolution }

type installation_evidence = {
  installation_id : int option;
  revision : string option;
  active : bool;
  repo_authorized : bool;
  permissions_ok : bool;
}

type user_org_sso_evidence = {
  user_authority_ok : bool;
  org_policy_ok : bool;
  sso_ok : bool;
}

type live_action_evidence = {
  ok : bool;
  revision : string option;
  detail : string option;
}

type revision_pin = {
  tool_catalog_revision : string option;
  access_revision : string option;
  principal_revision : int option;
  binding_lineage_id : string option;
  vault_generation : int option;
  installation_revision : string option;
  confirmation_id : string option;
  actor_snapshot_id : string option;
  live_state_revision : string option;
}

let empty_revision_pin =
  {
    tool_catalog_revision = None;
    access_revision = None;
    principal_revision = None;
    binding_lineage_id = None;
    vault_generation = None;
    installation_revision = None;
    confirmation_id = None;
    actor_snapshot_id = None;
    live_state_revision = None;
  }

type request = {
  action : string;
  tool_catalog : tool_catalog_evidence;
  repo_grant : repo_grant_evidence;
  principal : principal_confirmation_evidence;
  binding : binding_lineage_evidence;
  installation : installation_evidence;
  user_org_sso : user_org_sso_evidence;
  live_action : live_action_evidence;
  pin : revision_pin;
  actor_snapshot_id : string option;
}

let make_selected_binding ~binding_id ~lineage_id ?(authorized = true)
    ?(vault_active = true) ?(vault_generation = 1) ?(lineage_matches_pin = true)
    () =
  let binding_id = String.trim binding_id in
  let lineage_id = String.trim lineage_id in
  if binding_id = "" then Error "binding_id must be non-empty"
  else if lineage_id = "" then Error "lineage_id must be non-empty"
  else if vault_generation < 1 then Error "vault_generation must be >= 1"
  else
    Ok
      {
        binding_id;
        lineage_id;
        authorized;
        vault_active;
        vault_generation;
        lineage_matches_pin;
      }

(* -------------------------------------------------------------------------- *)
(* Authorize                                                                   *)
(* -------------------------------------------------------------------------- *)

let needs_user_binding (attr : Policy.attribution) =
  match attr with
  | Policy.User_required -> true
  | Policy.App_installation | Policy.Pat_compat -> false

let resolved_mode_of_attribution (attr : Policy.attribution) =
  match attr with
  | Policy.User_required -> User
  | Policy.App_installation | Policy.Pat_compat -> App

let deny ~failed_check ~code ~message ?requirement revisions =
  Deny { failed_check; repair = { code; message }; requirement; revisions }

let pin_mismatch_string ~label ~expected ~actual =
  Printf.sprintf
    "%s revision stale: expected %s but live evidence is %s. Re-preview the \
     action so frozen pins match current policy state."
    label expected actual

let pin_mismatch_int ~label ~expected ~actual =
  Printf.sprintf
    "%s revision stale: expected %d but live evidence is %d. Re-preview the \
     action so frozen pins match current policy state."
    label expected actual

let check_string_pin ~failed_check ~code ~label ~expected ~actual ~requirement
    revisions =
  match expected with
  | None -> Ok ()
  | Some exp ->
      if String.equal exp actual then Ok ()
      else
        Error
          (deny ~failed_check ~code
             ~message:(pin_mismatch_string ~label ~expected:exp ~actual)
             ?requirement:(Some requirement) revisions)

let check_string_pin_opt ~failed_check ~code ~label ~expected ~actual
    ~requirement revisions =
  match (expected, actual) with
  | None, _ -> Ok ()
  | Some exp, Some act when String.equal exp act -> Ok ()
  | Some exp, Some act ->
      Error
        (deny ~failed_check ~code
           ~message:(pin_mismatch_string ~label ~expected:exp ~actual:act)
           ?requirement:(Some requirement) revisions)
  | Some exp, None ->
      Error
        (deny ~failed_check ~code
           ~message:
             (Printf.sprintf
                "%s pin %S present but live evidence has no revision. \
                 Re-preview or repair live state capture."
                label exp)
           ?requirement:(Some requirement) revisions)

let check_int_pin ~failed_check ~code ~label ~expected ~actual ~requirement
    revisions =
  match expected with
  | None -> Ok ()
  | Some exp ->
      if exp = actual then Ok ()
      else
        Error
          (deny ~failed_check ~code
             ~message:(pin_mismatch_int ~label ~expected:exp ~actual)
             ?requirement:(Some requirement) revisions)

let base_revisions ~(req : Policy.requirement) ~(r : request) :
    checked_revisions =
  let binding_id, lineage_id, generation =
    match r.binding.resolution with
    | Selected b ->
        (Some b.binding_id, Some b.lineage_id, Some b.vault_generation)
    | Not_required | None_eligible | Ambiguous -> (None, None, None)
  in
  {
    policy_action = req.action;
    requirement_attribution = Policy.attribution_to_string req.attribution;
    requirement_tier = Policy.risk_tier_to_string req.tier;
    tool_catalog_revision = Some r.tool_catalog.revision;
    access_revision =
      (match r.repo_grant.access_revision with
      | Some _ as a -> a
      | None -> Some r.tool_catalog.access_revision);
    principal_id = Some r.principal.principal_id;
    principal_revision = Some r.principal.principal_revision;
    actor_revision = r.principal.actor_revision;
    identity_link_revision = r.principal.identity_link_revision;
    binding_id;
    binding_lineage_id = lineage_id;
    vault_generation = generation;
    installation_id = r.installation.installation_id;
    installation_revision = r.installation.revision;
    confirmation_id = r.principal.confirmation_id;
    actor_snapshot_id = r.actor_snapshot_id;
    live_state_revision = r.live_action.revision;
  }

let ( let* ) = Result.bind

let allow_result ~mode ~requirement ~revisions ~selected_opt ~principal_id =
  Ok
    (Allow
       {
         mode;
         requirement;
         revisions;
         binding_id =
           (match selected_opt with
           | Some b -> Some b.binding_id
           | None -> None);
         principal_id = Some principal_id;
       })

let check_tool_catalog ~(r : request) ~requirement revisions =
  if not r.tool_catalog.tool_authorized then
    Error
      (deny ~failed_check:"tool_catalog" ~code:"tool_not_in_catalog"
         ~message:
           "Requested GitHub tool is not in the frozen current-turn \
            Room/session Tool catalog. Re-run the turn after enabling the tool \
            in the Room access policy, or use a catalog that freezes it."
         ~requirement revisions)
  else
    check_string_pin ~failed_check:"tool_catalog"
      ~code:"stale_tool_catalog_revision" ~label:"Tool catalog"
      ~expected:r.pin.tool_catalog_revision ~actual:r.tool_catalog.revision
      ~requirement revisions

let check_repo_grant ~(r : request) ~requirement revisions =
  let repo = String.trim r.repo_grant.repo_full_name in
  if repo = "" then
    Error
      (deny ~failed_check:"repo_grant" ~code:"empty_repo"
         ~message:
           "repo_full_name must be non-empty (owner/repo). Provide the target \
            repository for grant evaluation."
         ~requirement revisions)
  else if r.repo_grant.blocked then
    Error
      (deny ~failed_check:"repo_grant" ~code:"repo_blocked"
         ~message:
           (Printf.sprintf
              "Repository %s is on the blocked repo-grant list (deny-wins). \
               Remove the block or choose an authorized repository."
              repo)
         ~requirement revisions)
  else if not r.repo_grant.granted then
    Error
      (deny ~failed_check:"repo_grant" ~code:"repo_not_granted"
         ~message:
           (Printf.sprintf
              "Repository %s is not in the Room/session repo grants. Add a \
               repo grant for this Room or select a granted repository."
              repo)
         ~requirement revisions)
  else
    let access_live =
      match r.repo_grant.access_revision with
      | Some a -> a
      | None -> r.tool_catalog.access_revision
    in
    check_string_pin ~failed_check:"repo_grant" ~code:"stale_access_revision"
      ~label:"Access" ~expected:r.pin.access_revision ~actual:access_live
      ~requirement revisions

let check_principal ~(r : request) ~requirement revisions =
  if String.trim r.principal.principal_id = "" then
    Error
      (deny ~failed_check:"principal" ~code:"empty_principal"
         ~message:
           "principal_id must be non-empty. Resolve a verified Principal \
            before authorizing attributed GitHub work."
         ~requirement revisions)
  else if not r.principal.principal_current_active then
    Error
      (deny ~failed_check:"principal" ~code:"principal_not_current"
         ~message:
           "Principal is not the current active lineage \
            (merged/disabled/missing). Repair identity (use the survivor \
            Principal or re-link) before retrying."
         ~requirement revisions)
  else
    check_int_pin ~failed_check:"principal" ~code:"stale_principal_revision"
      ~label:"Principal" ~expected:r.pin.principal_revision
      ~actual:r.principal.principal_revision ~requirement revisions

let check_confirmation ~(r : request) ~requirement revisions =
  if r.principal.confirmation_required && not r.principal.confirmation_satisfied
  then
    Error
      (deny ~failed_check:"confirmation" ~code:"confirmation_required"
         ~message:
           "This action requires explicit action confirmation (OAuth \
            authorization is not sufficient). Confirm the current preview for \
            this Principal, then retry."
         ~requirement revisions)
  else
    match r.pin.confirmation_id with
    | None -> Ok ()
    | Some expected -> (
        match r.principal.confirmation_id with
        | Some actual when String.equal expected actual -> Ok ()
        | Some actual ->
            Error
              (deny ~failed_check:"confirmation" ~code:"stale_confirmation"
                 ~message:
                   (pin_mismatch_string ~label:"Confirmation" ~expected ~actual)
                 ~requirement revisions)
        | None ->
            Error
              (deny ~failed_check:"confirmation" ~code:"stale_confirmation"
                 ~message:
                   (Printf.sprintf
                      "Confirmation pin %S present but no live \
                       confirmation_id. Re-confirm the action preview."
                      expected)
                 ~requirement revisions))

let validate_selected_binding ~(b : selected_binding) ~requirement revisions =
  if not b.authorized then
    Error
      (deny ~failed_check:"binding" ~code:"binding_not_authorized"
         ~message:
           "Selected GitHub account binding is not Authorized. Complete \
            private authorization activation or relink."
         ~requirement revisions)
  else if not b.vault_active then
    Error
      (deny ~failed_check:"binding" ~code:"vault_inactive"
         ~message:
           "Selected account vault is inactive (disabled/revoked). Relink the \
            GitHub account; no lease will be issued while inactive."
         ~requirement revisions)
  else if not b.lineage_matches_pin then
    Error
      (deny ~failed_check:"binding" ~code:"lineage_mismatch"
         ~message:
           "Logical binding lineage no longer matches the pinned Actor/intent \
            lineage (unlink/relink/revoke). Re-resolve the account and \
            re-preview."
         ~requirement revisions)
  else Ok b

let check_binding ~(r : request) ~user_binding_required ~requirement revisions =
  match (user_binding_required, r.binding.resolution) with
  | false, Not_required -> Ok None
  | false, (None_eligible | Ambiguous) -> Ok None
  | false, Selected b ->
      let* b = validate_selected_binding ~b ~requirement revisions in
      Ok (Some b)
  | true, Not_required ->
      Error
        (deny ~failed_check:"binding" ~code:"binding_required"
           ~message:
             "User_required actions need a currently valid Principal-owned \
              GitHub account binding. Link an account or resolve eligibility \
              first."
           ~requirement revisions)
  | true, None_eligible ->
      Error
        (deny ~failed_check:"binding" ~code:"no_eligible_account"
           ~message:
             "No currently eligible GitHub account for this Principal/context. \
              Link a GitHub account privately, ensure the vault is active, and \
              retry."
           ~requirement revisions)
  | true, Ambiguous ->
      Error
        (deny ~failed_check:"binding" ~code:"account_ambiguous"
           ~message:
             "Multiple eligible GitHub accounts and no deterministic \
              preference. Select an account privately (or set a Room/Repo \
              preference); selection never guesses by login or recency."
           ~requirement revisions)
  | true, Selected b ->
      let* b = validate_selected_binding ~b ~requirement revisions in
      Ok (Some b)

let check_binding_pins ~(r : request) ~selected_opt ~requirement revisions =
  match selected_opt with
  | None -> Ok ()
  | Some b ->
      let* () =
        check_string_pin ~failed_check:"binding" ~code:"stale_binding_lineage"
          ~label:"Binding lineage" ~expected:r.pin.binding_lineage_id
          ~actual:b.lineage_id ~requirement revisions
      in
      check_int_pin ~failed_check:"binding" ~code:"stale_vault_generation"
        ~label:"Vault generation" ~expected:r.pin.vault_generation
        ~actual:b.vault_generation ~requirement revisions

let check_installation ~(r : request) ~requirement revisions =
  if not r.installation.active then
    Error
      (deny ~failed_check:"installation" ~code:"installation_inactive"
         ~message:
           "GitHub App installation is not Active (suspended or deleted). \
            Restore the installation or choose an active installation for this \
            repository."
         ~requirement revisions)
  else if not r.installation.repo_authorized then
    Error
      (deny ~failed_check:"installation" ~code:"installation_repo_denied"
         ~message:
           (Printf.sprintf
              "Repository %s is outside the App installation repository \
               selection (or was revoked). Grant the repo to the installation \
               or pick an authorized repository."
              (String.trim r.repo_grant.repo_full_name))
         ~requirement revisions)
  else if not r.installation.permissions_ok then
    Error
      (deny ~failed_check:"installation" ~code:"permissions_insufficient"
         ~message:
           "App installation permissions do not cover this action. Update App \
            permissions and re-approve the installation, then retry."
         ~requirement revisions)
  else
    check_string_pin_opt ~failed_check:"installation"
      ~code:"stale_installation_revision" ~label:"Installation"
      ~expected:r.pin.installation_revision ~actual:r.installation.revision
      ~requirement revisions

let check_user_org_sso ~(r : request) ~user_binding_required ~requirement
    revisions =
  if user_binding_required && not r.user_org_sso.user_authority_ok then
    Error
      (deny ~failed_check:"user_org_sso" ~code:"user_authority_lost"
         ~message:
           "GitHub user authority for this action is no longer valid (removed, \
            blocked, or permission-lost). Repair GitHub access or relink, then \
            retry."
         ~requirement revisions)
  else if not r.user_org_sso.org_policy_ok then
    Error
      (deny ~failed_check:"user_org_sso" ~code:"org_policy_denied"
         ~message:
           "Organization or repository policy denies this action for the \
            current actor. Adjust org policy or use an account with sufficient \
            rights."
         ~requirement revisions)
  else if not r.user_org_sso.sso_ok then
    Error
      (deny ~failed_check:"user_org_sso" ~code:"sso_required"
         ~message:
           "SAML/SSO authorization is missing or expired for this \
            organization. Complete SSO for the GitHub account, reauthorize the \
            App if required, then retry."
         ~requirement revisions)
  else Ok ()

let check_live_action ~(r : request) ~requirement revisions =
  if not r.live_action.ok then
    let detail =
      match r.live_action.detail with
      | Some d when String.trim d <> "" -> String.trim d
      | _ -> "live action prerequisites are not satisfied"
    in
    Error
      (deny ~failed_check:"live_action" ~code:"live_state_failed"
         ~message:
           (Printf.sprintf
              "Live action state denied authorization: %s. Re-preview after \
               prerequisites (head SHA, checks, branch protection) stabilize."
              detail)
         ~requirement revisions)
  else
    check_string_pin_opt ~failed_check:"live_action"
      ~code:"stale_live_state_revision" ~label:"Live action state"
      ~expected:r.pin.live_state_revision ~actual:r.live_action.revision
      ~requirement revisions

let check_actor_snapshot_pin ~(r : request) ~requirement revisions =
  match r.pin.actor_snapshot_id with
  | None -> Ok ()
  | Some expected -> (
      match r.actor_snapshot_id with
      | Some actual when String.equal expected actual -> Ok ()
      | Some actual ->
          Error
            (deny ~failed_check:"actor_snapshot" ~code:"stale_actor_snapshot"
               ~message:
                 (pin_mismatch_string ~label:"Actor snapshot" ~expected ~actual)
               ~requirement revisions)
      | None ->
          Error
            (deny ~failed_check:"actor_snapshot" ~code:"stale_actor_snapshot"
               ~message:
                 (Printf.sprintf
                    "Actor snapshot pin %S present but request has no \
                     actor_snapshot_id. Re-capture attribution evidence."
                    expected)
               ~requirement revisions))

let authorize (r : request) : decision =
  let action = String.trim r.action in
  if action = "" then
    let revisions =
      empty_checked_revisions ~policy_action:""
        ~requirement_attribution:"user_required" ~requirement_tier:"critical"
    in
    deny ~failed_check:"policy" ~code:"empty_action"
      ~message:
        "action id must be non-empty. Pass a canonical GitHub mutation id \
         (e.g. merge, comment, review_submit)."
      revisions
  else
    let requirement = Policy.lookup ~action in
    let revisions = base_revisions ~req:requirement ~r in
    let user_binding_required = needs_user_binding requirement.attribution in
    let mode = resolved_mode_of_attribution requirement.attribution in
    match
      let* () = check_tool_catalog ~r ~requirement revisions in
      let* () = check_repo_grant ~r ~requirement revisions in
      let* () = check_principal ~r ~requirement revisions in
      let* () = check_confirmation ~r ~requirement revisions in
      let* selected_opt =
        check_binding ~r ~user_binding_required ~requirement revisions
      in
      let* () = check_binding_pins ~r ~selected_opt ~requirement revisions in
      let* () = check_installation ~r ~requirement revisions in
      let* () =
        check_user_org_sso ~r ~user_binding_required ~requirement revisions
      in
      let* () = check_live_action ~r ~requirement revisions in
      let* () = check_actor_snapshot_pin ~r ~requirement revisions in
      allow_result ~mode ~requirement ~revisions ~selected_opt
        ~principal_id:r.principal.principal_id
    with
    | Ok decision -> decision
    | Error decision -> decision

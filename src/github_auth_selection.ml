(* Deterministic GitHub PAT vs App auth selection (P19.M2.E1.T005).
   See github_auth_selection.mli and
   docs/plans/2026-07-12-github-item-room-routing.md + ADR 0002. *)

type auth_mode = Pat_only | App_only | Mixed
type scope_kind = Exact_repo of string | Org of string | Installation of int

type selection_reason =
  | Pat_exact_repo
  | App_installation_scope
  | App_preferred_when_mixed
  | Pat_fallback_exact_repo
  | Rejected_org_requires_app
  | Rejected_no_auth

type selection = {
  mode : auth_mode;
  chosen : [ `Pat | `App of int | `None ];
  installation_id : int option;
  repo : string option;
  reason : selection_reason;
  explanation : string;
}

type auth_snapshot = {
  pat_token_present : bool;
  app : Runtime_config.github_app_config option;
}

let auth_mode_to_string = function
  | Pat_only -> "pat_only"
  | App_only -> "app_only"
  | Mixed -> "mixed"

let selection_reason_to_string = function
  | Pat_exact_repo -> "pat_exact_repo"
  | App_installation_scope -> "app_installation_scope"
  | App_preferred_when_mixed -> "app_preferred_when_mixed"
  | Pat_fallback_exact_repo -> "pat_fallback_exact_repo"
  | Rejected_org_requires_app -> "rejected_org_requires_app"
  | Rejected_no_auth -> "rejected_no_auth"

let pat_present_string = function
  | None -> false
  | Some s -> String.trim s <> ""

let snapshot_of_parts ?pat ?app () : auth_snapshot =
  { pat_token_present = pat_present_string pat; app }

let snapshot_of_auth (auth : Runtime_config.github_auth option) : auth_snapshot
    =
  match auth with
  | None -> { pat_token_present = false; app = None }
  | Some (Runtime_config.GithubPat token) ->
      { pat_token_present = pat_present_string (Some token); app = None }
  | Some (Runtime_config.GithubApp app) ->
      { pat_token_present = false; app = Some app }

let classify_snapshot (s : auth_snapshot) : auth_mode =
  match (s.pat_token_present, s.app) with
  | true, Some _ -> Mixed
  | true, None -> Pat_only
  | false, Some _ -> App_only
  | false, None -> Pat_only

let classify_auth (auth : Runtime_config.github_auth option) : auth_mode =
  classify_snapshot (snapshot_of_auth auth)

let app_owns_installation (app : Runtime_config.github_app_config)
    (installation : Github_app_installation_scope.t) =
  match installation.app_id with
  | Some app_id -> app_id = app.app_id
  | None -> false

let app_installation_viable ~(app : Runtime_config.github_app_config)
    (installation : Github_app_installation_scope.t) ~repo_full_name =
  app_owns_installation app installation
  && Github_app_installation_scope.is_repo_authorized installation
       ~repo_full_name

let make_selection ~mode ~chosen ~installation_id ~repo ~reason ~explanation :
    selection =
  { mode; chosen; installation_id; repo; reason; explanation }

let select_for_repo ~(auth : auth_snapshot) ?installation ~repo_full_name () :
    selection =
  let installation = (installation : Github_app_installation_scope.t option) in
  let mode = classify_snapshot auth in
  let repo = Some repo_full_name in
  let app_viable =
    match (auth.app, installation) with
    | Some app, Some inst -> app_installation_viable ~app inst ~repo_full_name
    | _ -> false
  in
  let app_inst_id =
    match installation with
    | Some inst when app_viable ->
        Some inst.Github_app_installation_scope.installation_id
    | _ -> None
  in
  match (app_viable, auth.pat_token_present, app_inst_id) with
  | true, true, Some iid when mode = Mixed ->
      make_selection ~mode ~chosen:(`App iid) ~installation_id:(Some iid) ~repo
        ~reason:App_preferred_when_mixed
        ~explanation:
          (Printf.sprintf
             "Mixed auth: both PAT and App installation %d authorize %s; \
              preferring GitHub App (documented mixed preference)"
             iid repo_full_name)
  | true, _, Some iid ->
      make_selection ~mode ~chosen:(`App iid) ~installation_id:(Some iid) ~repo
        ~reason:App_installation_scope
        ~explanation:
          (Printf.sprintf
             "Selected GitHub App installation %d for exact-repo %s \
              (installation Active and repository authorized)"
             iid repo_full_name)
  | false, true, _ when mode = Mixed && auth.app <> None ->
      let why_app =
        match installation with
        | None -> "no installation scope provided"
        | Some inst -> (
            match auth.app with
            | Some app when not (app_owns_installation app inst) ->
                "installation does not belong to the configured GitHub App"
            | Some _ -> (
                match inst.status with
                | Github_app_installation_scope.Active ->
                    "repository not authorized on the installation"
                | Github_app_installation_scope.Suspended { reason } ->
                    Printf.sprintf "installation suspended%s"
                      (match reason with Some r -> ": " ^ r | None -> "")
                | Github_app_installation_scope.Deleted ->
                    "installation deleted")
            | None -> "GitHub App credentials are not configured")
      in
      make_selection ~mode ~chosen:`Pat ~installation_id:None ~repo
        ~reason:Pat_fallback_exact_repo
        ~explanation:
          (Printf.sprintf
             "Mixed auth: App path not viable (%s); falling back to PAT \
              exact-repo for %s"
             why_app repo_full_name)
  | false, true, _ ->
      make_selection ~mode ~chosen:`Pat ~installation_id:None ~repo
        ~reason:Pat_exact_repo
        ~explanation:
          (Printf.sprintf
             "Selected PAT exact-repo path for %s (PAT present; App not viable \
              or not configured)"
             repo_full_name)
  | false, false, _ ->
      make_selection ~mode ~chosen:`None ~installation_id:None ~repo
        ~reason:Rejected_no_auth
        ~explanation:
          (Printf.sprintf
             "No usable GitHub auth for repo %s: neither a PAT token nor an \
              Active authorized App installation is available"
             repo_full_name)
  | true, _, None ->
      (* Defensive: app_viable implies app_inst_id Some *)
      make_selection ~mode ~chosen:`None ~installation_id:None ~repo
        ~reason:Rejected_no_auth
        ~explanation:
          (Printf.sprintf
             "Internal inconsistency selecting auth for %s (App reported \
              viable without installation id)"
             repo_full_name)

let org_matches_account ~org (account : Github_app_installation_scope.account) =
  String.lowercase_ascii (String.trim org)
  = String.lowercase_ascii (String.trim account.login)

let can_claim_org_scope ~(auth : auth_snapshot)
    ~(installation : Github_app_installation_scope.t option) : bool =
  match (auth.app, installation) with
  | Some app, Some inst when app_owns_installation app inst -> (
      match inst.status with
      | Github_app_installation_scope.Active -> true
      | Github_app_installation_scope.Suspended _
      | Github_app_installation_scope.Deleted ->
          false)
  | _ -> false

let select_for_org_route ~(auth : auth_snapshot) ?installation ~org () :
    selection =
  let installation = (installation : Github_app_installation_scope.t option) in
  let mode = classify_snapshot auth in
  let reject_requires_app ~detail =
    make_selection ~mode ~chosen:`None ~installation_id:None ~repo:None
      ~reason:Rejected_org_requires_app
      ~explanation:
        (Printf.sprintf
           "Org route for %s requires a verified GitHub App installation (live \
            Org scope is App-only; PAT cannot claim Org). %s"
           org detail)
  in
  match auth.app with
  | None ->
      reject_requires_app
        ~detail:
          (if auth.pat_token_present then "Current auth is PAT-only."
           else "No App or PAT credentials are configured.")
  | Some app -> (
      match installation with
      | None ->
          reject_requires_app
            ~detail:"No installation scope was provided for the org account."
      | Some inst -> (
          match app_owns_installation app inst with
          | false ->
              reject_requires_app
                ~detail:
                  (Printf.sprintf
                     "Installation %d is not verified as belonging to the \
                      configured GitHub App."
                     inst.installation_id)
          | true -> (
              match inst.Github_app_installation_scope.status with
              | Github_app_installation_scope.Suspended { reason } ->
                  reject_requires_app
                    ~detail:
                      (Printf.sprintf "Installation %d is suspended%s."
                         inst.installation_id
                         (match reason with
                         | Some r -> " (" ^ r ^ ")"
                         | None -> ""))
              | Github_app_installation_scope.Deleted ->
                  reject_requires_app
                    ~detail:
                      (Printf.sprintf "Installation %d is deleted."
                         inst.installation_id)
              | Github_app_installation_scope.Active ->
                  if not (org_matches_account ~org inst.account) then
                    reject_requires_app
                      ~detail:
                        (Printf.sprintf
                           "Installation %d account %S does not match org %S \
                            (case-insensitive)."
                           inst.installation_id inst.account.login org)
                  else
                    let iid = inst.installation_id in
                    let reason, explanation =
                      if mode = Mixed then
                        ( App_preferred_when_mixed,
                          Printf.sprintf
                            "Org route for %s: verified Active App \
                             installation %d (account %s). Mixed auth present \
                             but Org scope requires App; selected App."
                            org iid inst.account.login )
                      else
                        ( App_installation_scope,
                          Printf.sprintf
                            "Org route for %s: selected verified Active App \
                             installation %d (account %s)"
                            org iid inst.account.login )
                    in
                    make_selection ~mode ~chosen:(`App iid)
                      ~installation_id:(Some iid) ~repo:None ~reason
                      ~explanation)))

let migration_preserves_pat ~(before : auth_snapshot) ~(after : auth_snapshot) :
    bool =
  if before.pat_token_present then after.pat_token_present else true

let migration_safe ~(before : auth_snapshot) ~(after : auth_snapshot)
    ~confirmed_apply : (unit, string) result =
  if before.pat_token_present && not after.pat_token_present then
    if confirmed_apply then Ok ()
    else
      Error
        "Migration would drop PAT credentials before confirmed apply; retain \
         PAT config or set confirmed_apply=true"
  else Ok ()

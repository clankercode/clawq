(* github_wizard_checks.ml -- GitHub App and repo grant validation for the
   room-agent wizard.

   Each [check_*] function returns [(passed, message)] where [message]
   includes an exact repair command when [passed = false].

   When GitHub is not configured, all checks return (true, "skip") so they
   never block non-GitHub room setup. *)

(* ── Helpers ─────────────────────────────────────────────────────── *)

let setup_github_cmd = "clawq setup github"

(** [skip_msg] is the standard pass message when GitHub is not configured. *)
let skip_msg = "No GitHub channel configured (skip)"

(* ── GitHub App installation ─────────────────────────────────────── *)

(** [check_github_app_token cfg] verifies that the GitHub App private key can be
    loaded, app_id is positive, and all configured installations have valid IDs.
    Returns [(true, skip_msg)] when GitHub is not configured. *)
let check_github_app_token (cfg : Runtime_config.t) : bool * string =
  match cfg.channels.github with
  | None -> (true, skip_msg)
  | Some gh -> (
      match gh.auth with
      | GithubPat _ -> (true, "Using PAT auth (no App to validate)")
      | GithubApp app_config -> (
          let cp = Setup_common.config_path () in
          if app_config.app_id <= 0 then
            ( false,
              Printf.sprintf
                "GitHub App app_id is invalid (%d). Fix: edit %s and set \
                 channels.github.auth.app_id to a positive integer, then run: \
                 %s"
                app_config.app_id cp setup_github_cmd )
          else if app_config.private_key_path = "" then
            ( false,
              Printf.sprintf
                "GitHub App private_key_path is empty. Fix: run %s and \
                 configure the App private key path"
                setup_github_cmd )
          else
            match Github_app_token.create ~config:app_config () with
            | Error msg ->
                ( false,
                  Printf.sprintf
                    "GitHub App private key invalid: %s. Fix: verify the PEM \
                     file at '%s' or run %s to reconfigure"
                    msg app_config.private_key_path setup_github_cmd )
            | Ok _token ->
                if app_config.installations = [] then
                  ( false,
                    Printf.sprintf
                      "GitHub App has no installations configured. Fix: edit \
                       %s and add entries to \
                       channels.github.auth.installations, then run: %s"
                      cp setup_github_cmd )
                else
                  let bad_ids =
                    List.filter_map
                      (fun (inst : Runtime_config.github_app_installation) ->
                        if inst.installation_id <= 0 then
                          Some (string_of_int inst.installation_id)
                        else None)
                      app_config.installations
                  in
                  if bad_ids <> [] then
                    ( false,
                      Printf.sprintf
                        "GitHub App has invalid installation IDs: %s. Fix: \
                         edit %s and set \
                         channels.github.auth.installations[].installation_id \
                         to positive integers"
                        (String.concat ", " bad_ids)
                        cp )
                  else
                    let count = List.length app_config.installations in
                    ( true,
                      Printf.sprintf
                        "GitHub App configured: app_id=%d, %d installation%s"
                        app_config.app_id count
                        (if count = 1 then "" else "s") )))

(* ── Repo grants ─────────────────────────────────────────────────── *)

(** [check_repo_grants cfg] validates that access bundles have properly
    formatted repo grants with non-empty capabilities when GitHub is configured.
    Returns [(true, skip_msg)] when GitHub is not configured. *)
let check_repo_grants (cfg : Runtime_config.t) : bool * string =
  match cfg.channels.github with
  | None -> (true, skip_msg)
  | Some _gh ->
      let all_repo_grants =
        List.concat_map
          (fun (b : Runtime_config_types.access_bundle) ->
            if b.status = "deleted" then [] else b.repo_grants)
          cfg.access_bundles
      in
      if all_repo_grants = [] then
        ( false,
          Printf.sprintf
            "No repo grants defined in access bundles. Fix: run %s to add an \
             access bundle with repo_grants, or edit %s and add repo_grants to \
             an existing access_bundles entry"
            setup_github_cmd
            (Setup_common.config_path ()) )
      else
        let invalid_names =
          List.filter_map
            (fun (rg : Runtime_config_types.repo_grant) ->
              match String.split_on_char '/' rg.repo with
              | [ owner; repo ] when owner <> "" && repo <> "" -> None
              | _ -> Some rg.repo)
            all_repo_grants
        in
        if invalid_names <> [] then
          ( false,
            Printf.sprintf
              "Invalid repo grant names: %s. Fix: edit %s and set \
               access_bundles[].repo_grants[].repo to 'owner/repo' or \
               'owner/*' format"
              (String.concat ", " invalid_names)
              (Setup_common.config_path ()) )
        else
          let no_caps =
            List.filter_map
              (fun (rg : Runtime_config_types.repo_grant) ->
                if rg.capabilities = [] then Some rg.repo else None)
              all_repo_grants
          in
          if no_caps <> [] then
            ( false,
              Printf.sprintf
                "Repo grants with empty capabilities: %s. Fix: edit %s and add \
                 capabilities (read, comment, branch, pr) to \
                 access_bundles[].repo_grants[].capabilities"
                (String.concat ", " no_caps)
                (Setup_common.config_path ()) )
          else
            let count = List.length all_repo_grants in
            ( true,
              Printf.sprintf
                "Repo grants validated: %d grant%s with capabilities" count
                (if count = 1 then "" else "s") )

(* ── Webhook reachability ────────────────────────────────────────── *)

(** [check_webhook_reachability cfg] validates that:
    - GitHub webhook secrets and paths are configured for each repo.
    - A gateway port or tunnel URL exists for GitHub to reach the endpoint.

    This is a config-level check; it cannot verify live network reachability.
    Returns [(true, skip_msg)] when GitHub is not configured. *)
let check_webhook_reachability (cfg : Runtime_config.t) : bool * string =
  match cfg.channels.github with
  | None -> (true, skip_msg)
  | Some gh ->
      if gh.repos = [] then
        ( false,
          Printf.sprintf
            "No GitHub repositories configured. Fix: run %s to add repos"
            setup_github_cmd )
      else
        let issues =
          List.filter_map
            (fun (r : Runtime_config.github_repo_config) ->
              let problems = ref [] in
              if r.webhook_secret = "" then
                problems := "missing webhook_secret" :: !problems;
              if r.webhook_path = "" then
                problems := "missing webhook_path" :: !problems;
              if !problems = [] then None
              else
                Some
                  (Printf.sprintf "%s (%s)" r.name
                     (String.concat ", " !problems)))
            gh.repos
        in
        if issues <> [] then
          ( false,
            Printf.sprintf
              "Webhook config issues: %s. Fix: run %s to reconfigure repos"
              (String.concat "; " issues)
              setup_github_cmd )
        else
          let gateway_port, tunnel_url =
            Setup_common.get_gateway_and_tunnel_url ()
          in
          let has_endpoint = tunnel_url <> None || gateway_port > 0 in
          if not has_endpoint then
            ( false,
              "Webhooks configured but no reachable endpoint. Fix: run 'clawq \
               gateway start' to start the gateway, or run 'clawq tunnel \
               start' to configure a tunnel" )
          else
            let endpoint_desc =
              match tunnel_url with
              | Some url -> Printf.sprintf "tunnel=%s" url
              | None -> Printf.sprintf "gateway port=%d" gateway_port
            in
            let count = List.length gh.repos in
            ( true,
              Printf.sprintf "Webhook config valid for %d repo%s (%s)" count
                (if count = 1 then "" else "s")
                endpoint_desc )

(* ── Room backlink readiness ─────────────────────────────────────── *)

(** [check_room_backlink ~cfg ~profile_id ~access_bundle_ids] verifies that:
    - The profile being configured has access bundles.
    - Access bundles have repo grants.

    Returns [(true, skip_msg)] when GitHub is not configured. *)
let check_room_backlink ~(cfg : Runtime_config.t) ~(profile_id : string)
    ~(access_bundle_ids : string list) : bool * string =
  match cfg.channels.github with
  | None -> (true, skip_msg)
  | Some _ ->
      let has_issues = ref false in
      let messages = ref [] in
      (* Check that the profile being configured has access bundles *)
      let profile_has_bundles =
        access_bundle_ids <> []
        ||
        match
          List.find_opt
            (fun (p : Runtime_config_types.room_profile) -> p.id = profile_id)
            cfg.room_profiles
        with
        | Some p -> p.access_bundle_ids <> []
        | None -> false
      in
      if not profile_has_bundles then begin
        has_issues := true;
        messages :=
          Printf.sprintf
            "Profile '%s' has no access bundles. Fix: run clawq rooms wizard \
             apply --profile-id %s --access-bundles <BUNDLE_ID>"
            profile_id profile_id
          :: !messages
      end;
      (* Check that bundles referenced by the profile have repo_grants *)
      let bundle_ids =
        if access_bundle_ids <> [] then access_bundle_ids
        else
          match
            List.find_opt
              (fun (p : Runtime_config_types.room_profile) -> p.id = profile_id)
              cfg.room_profiles
          with
          | Some p -> p.access_bundle_ids
          | None -> []
      in
      let bundles_without_grants =
        List.filter_map
          (fun bundle_id ->
            match
              List.find_opt
                (fun (b : Runtime_config_types.access_bundle) ->
                  b.id = bundle_id && b.status <> "deleted")
                cfg.access_bundles
            with
            | Some b when b.repo_grants = [] -> Some bundle_id
            | Some _ -> None
            | None -> Some (bundle_id ^ " (not found)"))
          bundle_ids
      in
      if bundles_without_grants <> [] then begin
        has_issues := true;
        messages :=
          Printf.sprintf
            "Access bundles without repo_grants: %s. Fix: edit %s and add \
             repo_grants to access_bundles[].repo_grants for these bundles"
            (String.concat ", " bundles_without_grants)
            (Setup_common.config_path ())
          :: !messages
      end;
      if !has_issues then (false, String.concat "\n  " (List.rev !messages))
      else
        let binding_count = List.length cfg.room_profile_bindings in
        ( true,
          Printf.sprintf "Room backlinks valid: %d binding%s" binding_count
            (if binding_count = 1 then "" else "s") )

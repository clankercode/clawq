let parse_model_availability value =
  match Models_catalog.availability_filter_of_string value with
  | Some availability -> Ok availability
  | None ->
      Error
        (Printf.sprintf
           "Error: invalid availability filter '%s'. Use available, \
            unavailable, or all."
           value)

let parse_models_list_args args =
  let rec loop provider_filter json availability = function
    | [] -> Ok (provider_filter, json, availability)
    | "--json" :: rest -> loop provider_filter true availability rest
    | "--provider" :: p :: rest -> loop (Some p) json availability rest
    | "--availability" :: value :: rest -> (
        match parse_model_availability value with
        | Ok availability -> loop provider_filter json availability rest
        | Error _ as err -> err)
    | "--available" :: rest ->
        loop provider_filter json Models_catalog.Available rest
    | "--unavailable" :: rest ->
        loop provider_filter json Models_catalog.Unavailable rest
    | "--all" :: rest -> loop provider_filter json Models_catalog.All rest
    | "--availability" :: [] ->
        Error
          "Error: --availability requires one of: available, unavailable, all."
    | bad :: _ ->
        Error
          (Printf.sprintf
             "Error: unrecognized models list option '%s'. Usage: clawq models \
              list [--provider P] [--json] [--availability \
              available|unavailable|all]"
             bad)
  in
  loop None false Models_catalog.Available args

let db_only_model_infos ?(availability = Models_catalog.Available) ~get_db
    ~provider_filter () =
  try
    Model_discovery.get_db_only_model_infos ~db:(get_db ()) ~provider_filter
      ~availability ()
  with _ -> []

let error_prefix msg =
  if
    String.length msg >= 6
    && String.lowercase_ascii (String.sub msg 0 6) = "error:"
  then msg
  else "Error: " ^ msg

let models_usage =
  "Usage: clawq models <subcommand>\n\n\
   Model names use provider:model (e.g. anthropic:claude-sonnet-4-6).\n\
   Bare names work when unique; ambiguous names list candidates.\n\
   Legacy provider/model is accepted and normalized to provider:model.\n\n\
   Subcommands:\n\
  \  list [--provider P] [--json] [--availability available|unavailable|all]\n\
  \                           List known models + current default\n\
  \  set-default MODEL            Set default model (validates first)\n\
  \                           Options: --skip-validation / --no-test\n\
  \  refresh [--force]            Refresh model list from provider APIs\n\
  \  refresh --provider PNAME     Refresh models for a specific provider"

let cmd_models ~get_db ~get_config args =
  let current_default () =
    let cfg : Runtime_config.t = get_config () in
    cfg.agent_defaults.primary_model
  in
  let plain_list ~provider_filter ~availability ~db_extras =
    let body =
      Models_catalog.to_plain_list ~provider_filter ~availability ~db_extras ()
    in
    Models_catalog.with_list_header ~current_default:(current_default ()) body
  in
  match args with
  | [] ->
      let db_extras =
        db_only_model_infos ~get_db ~provider_filter:None
          ~availability:Models_catalog.Available ()
      in
      plain_list ~provider_filter:None ~availability:Models_catalog.Available
        ~db_extras
  | "list" :: rest -> (
      match parse_models_list_args rest with
      | Error msg -> msg
      | Ok (provider_filter, json, availability) ->
          let db_extras =
            db_only_model_infos ~get_db ~provider_filter ~availability ()
          in
          if json then
            Yojson.Safe.to_string
              (Models_catalog.to_json ~provider_filter ~availability ~db_extras
                 ())
          else plain_list ~provider_filter ~availability ~db_extras)
  | "set-default" :: rest when rest <> [] -> (
      let skip_validation =
        List.exists (fun a -> a = "--skip-validation" || a = "--no-test") rest
      in
      let positional =
        List.filter (fun a -> a <> "--skip-validation" && a <> "--no-test") rest
      in
      match positional with
      | [ raw_model ] -> (
          let cfg : Runtime_config.t = get_config () in
          let configured_providers = List.map fst cfg.providers in
          (* Do not require a configured provider here: users may point at a
             custom/self-hosted provider:model before the provider is added.
             Live validation / preflight still catches unusable targets. *)
          match
            Models_catalog.resolve_model_name_for_set
              ~require_configured_provider:false ~configured_providers raw_model
          with
          | Error err -> error_prefix err
          | Ok resolved -> (
              let canonical_value = resolved.Models_catalog.canonical_value in
              let previous_model = cfg.agent_defaults.primary_model in
              let previous_disp =
                if String.trim previous_model = "" then "(not set)"
                else previous_model
              in
              let rollback_cmd =
                if String.trim previous_model <> "" then
                  Printf.sprintf "clawq models set-default %s" previous_model
                else "clawq models set-default <previous-model>"
              in
              let rollback_banner =
                Printf.sprintf
                  "Current model: %s\nRollback command if needed:\n  %s\n"
                  previous_disp rollback_cmd
              in
              let commit_and_format () =
                let set_result =
                  Config_set.set_value "agent_defaults.primary_model"
                    canonical_value
                in
                let catalog_note =
                  match resolved.Models_catalog.catalog_match with
                  | Some _ -> ""
                  | None ->
                      "\n\
                       Note: model is not in the built-in catalog; ensure the \
                       provider can serve it."
                in
                Printf.sprintf "%sDefault model set to: %s%s%s\n%s"
                  rollback_banner canonical_value resolved.Models_catalog.hint
                  catalog_note set_result
              in
              match
                try
                  Model_discovery.validate_cached_model_allowed ~db:(get_db ())
                    canonical_value
                with _ -> None
              with
              | Some msg -> error_prefix msg
              | None -> (
                  if skip_validation then
                    commit_and_format ()
                    ^ "\nNote: validation skipped (--skip-validation)."
                  else
                    let result =
                      Model_validation.validate_sync ~config:cfg
                        ~model:canonical_value ()
                    in
                    match result with
                    | Model_validation.Ok_validated -> commit_and_format ()
                    | Model_validation.Error_msg msg ->
                        rollback_banner
                        ^ Model_validation.format_failure ~rollback_cmd msg)))
      | _ ->
          "Usage: clawq models set-default MODEL [--skip-validation]\n\
           MODEL uses provider:model (e.g. anthropic:claude-sonnet-4-6).\n\
           Bare names resolve when unique; ambiguous names list candidates.")
  | [ "refresh" ] ->
      let db = get_db () in
      let config : Runtime_config.t = get_config () in
      Lwt_main.run (Model_discovery.maybe_refresh ~db ~config ());
      "Model discovery refresh complete. Run 'clawq models list' to see \
       updated models."
  | [ "refresh"; "--force" ] ->
      let db = get_db () in
      let config : Runtime_config.t = get_config () in
      Lwt_main.run (Model_discovery.maybe_refresh ~db ~force:true ~config ());
      "Model discovery force-refresh complete. Run 'clawq models list' to see \
       updated models."
  | [ "refresh"; "--provider"; pname ]
  | [ "refresh"; "--provider"; pname; "--force" ]
  | [ "refresh"; "--force"; "--provider"; pname ] -> (
      let config : Runtime_config.t = get_config () in
      match List.assoc_opt pname config.providers with
      | None -> Printf.sprintf "Provider '%s' not found in config." pname
      | Some pc -> (
          let db = get_db () in
          let result =
            Lwt_main.run
              (Model_discovery.refresh_provider ~db ~provider_name:pname
                 ~provider_config:pc)
          in
          match result with
          | Ok n ->
              Printf.sprintf "Refreshed %d model(s) for provider '%s'." n pname
          | Error e ->
              Printf.sprintf "Refresh failed for provider '%s': %s" pname e))
  | _ -> models_usage

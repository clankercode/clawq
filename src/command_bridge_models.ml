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

let cmd_models ~get_db ~get_config args =
  match args with
  | [] ->
      let db_extras =
        db_only_model_infos ~get_db ~provider_filter:None
          ~availability:Models_catalog.Available ()
      in
      Models_catalog.to_plain_list ~db_extras ()
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
          else
            Models_catalog.to_plain_list ~provider_filter ~availability
              ~db_extras ())
  | "set-default" :: rest when rest <> [] -> (
      let skip_validation =
        List.exists (fun a -> a = "--skip-validation" || a = "--no-test") rest
      in
      let positional =
        List.filter (fun a -> a <> "--skip-validation" && a <> "--no-test") rest
      in
      match positional with
      | [ raw_model ] -> (
          let model = Models_catalog.resolve_alias_or_name raw_model in
          let provider, model_id, fmt = Models_catalog.split_name model in
          let plain_matches =
            match fmt with
            | Models_catalog.Plain -> Models_catalog.fuzzy_plain_matches model
            | Models_catalog.Canonical | Models_catalog.Legacy -> []
          in
          match (fmt, plain_matches) with
          | Models_catalog.Plain, [] ->
              Printf.sprintf
                "Error: model '%s' not found in catalog.\n\
                 Hint: use provider:model format (e.g. \
                 anthropic:claude-sonnet-4-6) to set an unknown model."
                model
          | Models_catalog.Plain, _ :: _ :: _ ->
              let candidates =
                plain_matches
                |> List.map Models_catalog.full_name
                |> List.sort_uniq String.compare
                |> String.concat "\n  "
              in
              Printf.sprintf
                "Error: ambiguous model '%s'. Use provider:model format.\n\
                 Candidates:\n\
                \  %s"
                model candidates
          | _ -> (
              let canonical_value, hint =
                match fmt with
                | Models_catalog.Legacy ->
                    let canonical_id =
                      Option.value ~default:model_id
                        (Models_catalog.canonical_id ~provider model_id)
                    in
                    let canonical = provider ^ ":" ^ canonical_id in
                    ( canonical,
                      Printf.sprintf
                        "\nNote: normalized \"%s\" to canonical format \"%s\"."
                        model canonical )
                | Models_catalog.Plain -> (
                    match plain_matches with
                    | [ m ] when m.Models_catalog.provider <> "" ->
                        let canonical =
                          m.Models_catalog.provider ^ ":" ^ m.Models_catalog.id
                        in
                        ( canonical,
                          Printf.sprintf
                            "\nNote: resolved bare model name to \"%s\"."
                            canonical )
                    | _ -> (model, ""))
                | Models_catalog.Canonical -> (
                    match Models_catalog.canonical_id ~provider model_id with
                    | Some canonical_id ->
                        let canonical = provider ^ ":" ^ canonical_id in
                        ( canonical,
                          Printf.sprintf
                            "\nNote: corrected model casing \"%s\" -> \"%s\"."
                            model canonical )
                    | None -> (model, ""))
              in
              let cfg : Runtime_config.t = get_config () in
              let previous_model = cfg.agent_defaults.primary_model in
              let rollback_cmd =
                if previous_model <> "" then
                  Printf.sprintf "clawq models set-default %s" previous_model
                else "clawq models set-default <previous-model>"
              in
              let display_provider =
                match fmt with
                | Models_catalog.Canonical | Models_catalog.Legacy -> provider
                | Models_catalog.Plain -> (
                    match plain_matches with
                    | [ m ] when m.Models_catalog.provider <> "" ->
                        m.Models_catalog.provider
                    | _ -> "")
              in
              let display_model =
                match fmt with
                | Models_catalog.Canonical | Models_catalog.Legacy -> model_id
                | Models_catalog.Plain -> model
              in
              let rollback_banner =
                Printf.sprintf
                  "Current model: %s\nRollback command if needed:\n  %s\n"
                  previous_model rollback_cmd
              in
              let commit_and_format () =
                let set_result =
                  Config_set.set_value "agent_defaults.primary_model"
                    canonical_value
                in
                if display_provider <> "" then
                  Printf.sprintf
                    "%sDefault model set to: %s (provider: %s)%s\n%s"
                    rollback_banner display_model display_provider hint
                    set_result
                else
                  Printf.sprintf "%sDefault model set to: %s%s\n%s"
                    rollback_banner display_model hint set_result
              in
              match
                try
                  Model_discovery.validate_cached_model_allowed ~db:(get_db ())
                    canonical_value
                with _ -> None
              with
              | Some msg -> "Error: " ^ msg
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
      | _ -> "Usage: clawq models set-default MODEL [--skip-validation]")
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
  | _ ->
      "Usage: clawq models <subcommand>\n\n\
       Subcommands:\n\
      \  list [--provider P] [--json] [--availability available|unavailable|all]\n\
      \                           List known models (catalog + DB cache)\n\
      \  set-default MODEL            Set default model (e.g. \
       anthropic:claude-sonnet-4-6)\n\
      \  refresh [--force]            Refresh model list from provider APIs\n\
      \  refresh --provider PNAME     Refresh models for a specific provider"

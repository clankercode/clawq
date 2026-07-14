let provider_warning cfg provider =
  if provider = "" || List.mem_assoc provider cfg.Runtime_config.providers then
    ""
  else
    Printf.sprintf
      "\n\
       Warning: provider '%s' not found in config. Add it to your config.json \
       to use this model."
      provider

let model_set_reply cfg ~previous
    (resolved : Models_catalog.resolved_model_name) =
  let warn = provider_warning cfg resolved.canonical_provider in
  let rollback =
    if String.trim previous <> "" then
      Printf.sprintf "\nPrevious model: %s\nRollback: /model set %s" previous
        previous
    else ""
  in
  if resolved.display_provider <> "" then
    Printf.sprintf
      "Model set to: %s%s%s%s\n\
       Persisted for this session across restarts. Use /model set-default to \
       change the global default."
      resolved.canonical_value resolved.hint warn rollback
  else
    Printf.sprintf
      "Warning: '%s' not found in model catalog. Setting anyway.%s\n\
       Persisted for this session across restarts. Use /model set-default to \
       change the global default."
      resolved.canonical_value rollback

let model_set_default_reply ~previous
    (resolved : Models_catalog.resolved_model_name) =
  let rollback =
    if String.trim previous <> "" then
      Printf.sprintf "\nPrevious default: %s\nRollback: /model set-default %s"
        previous previous
    else ""
  in
  Printf.sprintf "Default model set to: %s%s%s\nApplies to new sessions."
    resolved.canonical_value resolved.hint rollback

let handle_model_set_action ?(config_source = "slash_command") ~session_manager
    ~key action =
  let open Lwt.Syntax in
  let open Slash_commands in
  match action with
  | ModelSet name | ModelSetForce name -> (
      let force = match action with ModelSetForce _ -> true | _ -> false in
      let cfg = Session.get_config session_manager in
      let configured_providers = List.map fst cfg.Runtime_config.providers in
      match
        Models_catalog.resolve_model_name_for_set ~force
          ~require_configured_provider:true ~configured_providers name
      with
      | Error err -> Lwt.return err
      | Ok resolved -> (
          match
            Model_discovery.validate_cached_model_allowed_opt
              (Session.get_db session_manager)
              resolved.Models_catalog.canonical_value
          with
          | Some err -> Lwt.return err
          | None ->
              let previous =
                Session.get_session_effective_model session_manager ~key
              in
              let* _compaction =
                Session.set_session_model_with_compact session_manager ~key
                  ~model:resolved.canonical_value
              in
              Lwt.return (model_set_reply cfg ~previous resolved)))
  | ModelSetDefault name -> (
      let cfg = Session.get_config session_manager in
      let configured_providers = List.map fst cfg.Runtime_config.providers in
      match
        Models_catalog.resolve_model_name_for_set ~force:false
          ~require_configured_provider:false ~configured_providers name
      with
      | Error err -> Lwt.return err
      | Ok resolved -> (
          match
            Model_discovery.validate_cached_model_allowed_opt
              (Session.get_db session_manager)
              resolved.Models_catalog.canonical_value
          with
          | Some err -> Lwt.return err
          | None -> (
              let previous = cfg.Runtime_config.agent_defaults.primary_model in
              match
                Config_set.set_json_value "agent_defaults.primary_model"
                  (`String resolved.canonical_value)
              with
              | Error e ->
                  Lwt.return (Printf.sprintf "Error writing config: %s" e)
              | Ok () ->
                  let agent_defaults =
                    {
                      cfg.Runtime_config.agent_defaults with
                      primary_model = resolved.canonical_value;
                    }
                  in
                  Session.update_config ~source:config_source session_manager
                    { cfg with agent_defaults };
                  Lwt.return (model_set_default_reply ~previous resolved))))
  | _ -> invalid_arg "Slash_commands_model.handle_model_set_action"

let provider_warning cfg provider =
  if provider = "" || List.mem_assoc provider cfg.Runtime_config.providers then
    ""
  else
    Printf.sprintf
      "\n\
       Warning: provider '%s' not found in config. Add it to your config.json \
       to use this model."
      provider

let model_set_reply cfg (resolved : Models_catalog.resolved_model_name) =
  let warn = provider_warning cfg resolved.canonical_provider in
  if resolved.display_provider <> "" then
    Printf.sprintf
      "Model set to: %s (provider: %s)%s%s\n\
       Persisted for this session across restarts. Use /model set-default to \
       change the global default."
      resolved.display_model resolved.display_provider resolved.hint warn
  else
    Printf.sprintf
      "Warning: '%s' not found in model catalog. Setting anyway.\n\
       Persisted for this session across restarts. Use /model set-default to \
       change the global default."
      resolved.canonical_value

let model_set_default_reply (resolved : Models_catalog.resolved_model_name) =
  if resolved.display_provider <> "" then
    Printf.sprintf
      "Default model set to: %s (provider: %s)%s\nApplies to new sessions."
      resolved.display_model resolved.display_provider resolved.hint
  else
    Printf.sprintf "Default model set to: %s\nApplies to new sessions."
      resolved.display_model

let handle_model_set_action ?(config_source = "slash_command") ~session_manager
    ~key action =
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
      | Error err -> err
      | Ok resolved -> (
          match
            Model_discovery.validate_cached_model_allowed_opt
              (Session.get_db session_manager)
              resolved.Models_catalog.canonical_value
          with
          | Some err -> err
          | None ->
              Session.set_session_model session_manager ~key
                ~model:resolved.canonical_value;
              model_set_reply cfg resolved))
  | ModelSetDefault name -> (
      let cfg = Session.get_config session_manager in
      let configured_providers = List.map fst cfg.Runtime_config.providers in
      match
        Models_catalog.resolve_model_name_for_set ~force:false
          ~require_configured_provider:false ~configured_providers name
      with
      | Error err -> err
      | Ok resolved -> (
          match
            Model_discovery.validate_cached_model_allowed_opt
              (Session.get_db session_manager)
              resolved.Models_catalog.canonical_value
          with
          | Some err -> err
          | None -> (
              match
                Config_set.set_json_value "agent_defaults.primary_model"
                  (`String resolved.canonical_value)
              with
              | Error e -> Printf.sprintf "Error writing config: %s" e
              | Ok () ->
                  let agent_defaults =
                    {
                      cfg.Runtime_config.agent_defaults with
                      primary_model = resolved.canonical_value;
                    }
                  in
                  Session.update_config ~source:config_source session_manager
                    { cfg with agent_defaults };
                  model_set_default_reply resolved)))
  | _ -> invalid_arg "Slash_commands_model.handle_model_set_action"

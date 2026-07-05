let refresh_active_template_tool_registries session_manager =
  match Session.get_tool_registry session_manager with
  | None -> ()
  | Some base_registry ->
      Hashtbl.iter
        (fun _ (agent, _, _) ->
          match (agent.Agent.agent_template, agent.Agent.tool_registry) with
          | Some tmpl, Some registry ->
              let refreshed =
                Agent_template.filter_tool_registry base_registry tmpl
              in
              Tool_registry.restore registry (Tool_registry.snapshot refreshed)
          | _ -> ())
        session_manager.sessions

let apply_runtime_config_reload
    ?(reconcile_room_profiles =
      fun ~db ~config -> ignore (Memory.reconcile_room_profiles ~db ~config))
    ?send_file_runtime ?(after_publish = fun () -> ()) ~source ~current_config
    ~session_manager ~sandbox ~db ~tool_registry ~new_config () =
  let old_config = !current_config in
  let old_sandbox = !sandbox in
  let old_registry = Option.map Tool_registry.snapshot tool_registry in
  try
    (* Reconcile room profile config into DB BEFORE publishing new_config so
       that on failure the old config and its derived policies remain active. *)
    (match db with
    | Some db -> reconcile_room_profiles ~db ~config:new_config
    | None -> ());
    sandbox := Daemon_util.make_sandbox new_config;
    current_config := new_config;
    (* Re-initialize GitHub App token cache on config reload *)
    (match new_config.channels.github with
    | Some gc -> Github_app_token.init_from_config gc
    | None -> Github_app_token.invalidate_all ());
    Session.set_sandbox session_manager !sandbox;
    Session.update_config ~source session_manager new_config;
    Http_debug.sync_config new_config.log;
    (let old_sc = old_config.summarizer in
     let new_sc = new_config.summarizer in
     if old_sc <> new_sc then
       Logs.info (fun m ->
           m
             "Summarizer config updated [%s]: enabled=%b→%b, model=%s→%s, \
              threshold=%d→%d"
             source old_sc.enabled new_sc.enabled
             (Pmodel.to_string old_sc.model)
             (Pmodel.to_string new_sc.model)
             old_sc.threshold_chars new_sc.threshold_chars));
    (match tool_registry with
    | Some registry -> (
        Daemon_util.refresh_runtime_bound_tools ?send_file_runtime
          ~config:new_config ~session_manager ~sandbox:!sandbox registry;
        match db with
        | Some db ->
            let notify =
              if new_config.agent_defaults.task_tree_notifications then
                Some
                  (Daemon_task_tree_helpers.task_tree_notify_for_session
                     session_manager)
              else None
            in
            Daemon_task_tree_helpers
            .refresh_task_tree_tools_with_current_workspace ~current_config ~db
              ?notify registry
        | None -> ())
    | None -> ());
    refresh_active_template_tool_registries session_manager;
    (* B735: validate private channel policy on config reload *)
    (match new_config.channels.slack with
    | Some s when Runtime_config.slack_has_valid_credentials s ->
        Lwt.async (fun () ->
            let open Lwt.Syntax in
            Lwt.catch
              (fun () ->
                let* warnings =
                  Slack.validate_private_channels_in_allowlist
                    ~bot_token:s.bot_token ~allow_channels:s.allow_channels
                    ~private_channel_policy:s.private_channel_policy
                    ~allow_private_channels:s.allow_private_channels
                in
                List.iter (fun w -> Logs.warn (fun m -> m "%s" w)) warnings;
                Lwt.return_unit)
              (fun _exn -> Lwt.return_unit))
    | _ -> ());
    after_publish ();
    Ok ()
  with exn ->
    (* Rollback to old config and sandbox on failure to preserve
       last valid policy *)
    Logs.warn (fun m ->
        m "Config reload failed [%s], rolling back to previous config: %s"
          source (Printexc.to_string exn));
    (match (tool_registry, old_registry) with
    | Some registry, Some snapshot -> Tool_registry.restore registry snapshot
    | _ -> ());
    sandbox := old_sandbox;
    current_config := old_config;
    (* Restore GitHub App token to match rolled-back config *)
    (match old_config.channels.github with
    | Some gc -> Github_app_token.init_from_config gc
    | None -> Github_app_token.invalidate_all ());
    Session.set_sandbox session_manager old_sandbox;
    Session.update_config ~source session_manager old_config;
    refresh_active_template_tool_registries session_manager;
    Http_debug.sync_config old_config.log;
    Error (Printexc.to_string exn)

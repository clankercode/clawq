(** Task tree tool helpers for daemon setup. *)

let task_tree_tool_with_current_workspace ~current_config ~db ?notify () =
  Task_tree.tool ~db
    ~default_repo_path:(Runtime_config.effective_workspace !current_config)
    ?notify ()

let task_start_agent_tool_with_current_workspace ~current_config ~db () =
  Task_tree.start_agent_tool ~db
    ~default_repo_path:(Runtime_config.effective_workspace !current_config)
    ()

let task_tree_notify_for_session session_manager session_key =
  match Session.find_registered_notifier session_manager ~key:session_key with
  | Some notifier ->
      let connector =
        if String.starts_with ~prefix:"telegram:" session_key then
          Format_adapter.Telegram_markdown
        else if String.starts_with ~prefix:"discord:" session_key then
          Format_adapter.Discord
        else if String.starts_with ~prefix:"slack:" session_key then
          Format_adapter.Slack
        else Format_adapter.Plain
      in
      Some (connector, notifier)
  | None -> None

let refresh_task_tree_tools_with_current_workspace ~current_config ~db ?notify
    registry =
  Tool_registry.replace registry
    (task_tree_tool_with_current_workspace ~current_config ~db ?notify ());
  Tool_registry.replace registry
    (task_start_agent_tool_with_current_workspace ~current_config ~db ())

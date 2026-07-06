include Teams_api

let send_bash_run ~(session_manager : Session.t) ~key ~send_text cmd =
  let open Lwt.Syntax in
  let config = Session.get_config session_manager in
  let* result =
    Slash_commands_bash.run_bash_command ~config ~session_key:key cmd
  in
  let full_text = Slash_commands_bash.format_result cmd result in
  let max_len = 25000 in
  let text =
    if String.length full_text <= max_len then full_text
    else String.sub full_text 0 max_len ^ "\n...[truncated]"
  in
  send_text text

let send_tools ~(session_manager : Session.t) ~is_admin ~send_text =
  let text =
    match Session.get_tool_registry session_manager with
    | Some reg ->
        let tools, _ = Tool_registry.partition_skills reg in
        let tools = Skills.filter_visible_tools ~show_test:is_admin tools in
        let skills =
          Skills.filter_visible_tools ~show_test:is_admin
            (Skills.available_skills_as_tools ())
        in
        Slash_commands.format_tools ~connector:Format_adapter.Teams tools skills
          (Agent_template.available_templates ())
    | None -> "Tools are not enabled."
  in
  send_text text

let send_tasks ~(session_manager : Session.t) ~key ~full ~send_text =
  let raw =
    match Session.get_db session_manager with
    | Some db ->
        Task_tree.init_schema db;
        if full then Task_tree.render_tree_with_legend ~db ~session_key:key
        else Task_tree.render_emoji_tree ~db ~session_key:key ()
    | None -> "Tasks are not available (no database)."
  in
  send_text
    (Slash_commands_fmt.format_tasks ~connector:Format_adapter.Teams raw)

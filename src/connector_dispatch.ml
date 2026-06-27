(* Shared slash-command dispatch for chat connectors.

   discord.ml, slack.ml and telegram.ml each carried a near-identical match over
   [Slash_commands.result] for the "compute a reply string, then send it" family
   of commands. This module factors that common core out so a new informational
   command only has to be wired once.

   Per-connector differences are captured in [dispatch_env]:
   - [connector]      the [Format_adapter.connector] used to render replies;
   - [connector_name] lower-case connector identity for admin OTC and config
     update [~source];
   - [log_name] and log field labels preserve each connector's existing log text;
   - [send_plain]     send an unformatted reply (Telegram uses bare send_message);
   - [send_formatted] send a rich/markdown/HTML reply (Telegram chunks + HTML).
     For Discord/Slack [send_plain] and [send_formatted] are the same closure.

   Commands with genuine per-connector behaviour are NOT handled here and remain
   in each connector's own match: agent-spawning commands (Delegate, AgentInvoke,
   ForkAnd, Debate, Rig), Compact (connector-specific status notifier), BashRun
   and DebugDumpChat (different truncation / upload paths), Model (Telegram uses a
   different send mechanism per sub-command), and the free-form NotACommand turn.
   Calling [dispatch] with one of those is a no-op (they are never routed here). *)

type dispatch_env = {
  connector : Format_adapter.connector;
  connector_name : string;
  log_name : string;
  thinking_channel_field : string;
  thinking_user_field : string;
  show_thinking_channel_field : string;
  show_thinking_user_field : string;
  session_mgr : Session.t;
  key : string;
  channel_id : string;
  user_id : string;
  is_admin : bool;
  send_plain : string -> unit Lwt.t;
  send_formatted : string -> unit Lwt.t;
}

(* Shared implementation of the per-connector [set_thinking_level] helpers.
   The user-facing string is identical across connectors. [connector_name]
   preserves the config [~source] tag, while [log_name] and the field-label
   strings preserve connector-specific log lines. *)
let set_thinking_level (env : dispatch_env) level =
  let cfg = Session.get_config env.session_mgr in
  let previous = cfg.agent_defaults.reasoning_effort in
  match Config_set.set_reasoning_effort level with
  | Ok () ->
      let agent_defaults =
        { cfg.agent_defaults with reasoning_effort = level }
      in
      Session.update_config ~source:env.connector_name env.session_mgr
        { cfg with agent_defaults };
      Logs.info (fun m ->
          m "%s thinking level updated %s=%s %s=%s from=%s to=%s" env.log_name
            env.thinking_channel_field env.channel_id env.thinking_user_field
            env.user_id
            (Slash_commands.thinking_level_to_string previous)
            (Slash_commands.thinking_level_to_string level));
      Printf.sprintf "Thinking level changed from %s to %s."
        (Slash_commands.thinking_level_to_string previous)
        (Slash_commands.thinking_level_to_string level)
  | Error err ->
      Logs.err (fun m ->
          m "%s thinking level update failed %s=%s %s=%s: %s" env.log_name
            env.thinking_channel_field env.channel_id env.thinking_user_field
            env.user_id err);
      "Failed to update thinking level: " ^ err

let dispatch (env : dispatch_env) (result : Slash_commands.result) : unit Lwt.t
    =
  let open Lwt.Syntax in
  let open Slash_commands in
  let connector = env.connector in
  let session_mgr = env.session_mgr in
  let key = env.key in
  match result with
  | RegisterAsAdminOtc None ->
      let _code =
        Admin.generate_otc ~channel:env.connector_name ~sender_id:env.user_id
      in
      env.send_plain
        "Admin registration initiated. Check the daemon console/logs for your \
         one-time code, then run: /register_as_admin_otc CODE"
  | RegisterAsAdminOtc (Some code) -> (
      match Session.get_db session_mgr with
      | Some db -> (
          match
            Admin.verify_otc ~db ~channel:env.connector_name
              ~sender_id:env.user_id ~code
          with
          | Ok () -> env.send_plain "Successfully registered as admin."
          | Error err_msg -> env.send_plain err_msg)
      | None -> env.send_plain "Database not available.")
  | Reply text -> env.send_plain text
  | RuntimeCtx ->
      let* text = Session.runtime_context_block session_mgr ~key in
      env.send_plain text
  | FormattedReply fn ->
      let text = fn connector in
      env.send_formatted text
  | Help | Menu _ ->
      let show_test = env.is_admin in
      let text =
        Slash_commands.format_help ~connector ~show_test ~is_admin:env.is_admin
          ()
      in
      env.send_formatted text
  | Reset ->
      let* active_bg_tasks = Session.reset session_mgr ~key in
      env.send_formatted
        (Slash_commands_fmt.format_reset ~connector ~active_bg_tasks)
  | Uptime ->
      let raw =
        Daemon_status.daemon_uptime_reply
          ~pid:(Daemon_status.read_current_daemon_pid ())
      in
      env.send_formatted (Slash_commands_fmt.format_uptime ~connector raw)
  | Status ->
      let text =
        Slash_commands.format_status ~connector
          ~db:(Session.get_db session_mgr)
          ~session_count:(Session.session_count session_mgr)
          ~active_count:(Session.active_session_count session_mgr)
          ()
      in
      env.send_formatted text
  | Thinking Slash_commands.ShowThinking ->
      let current =
        (Session.get_config session_mgr).agent_defaults.reasoning_effort
      in
      env.send_formatted
        (Slash_commands_fmt.format_thinking_status ~connector current)
  | Thinking (Slash_commands.SetThinking level) ->
      env.send_formatted (set_thinking_level env level)
  | ShowThinking action ->
      let cfg = Session.get_config session_mgr in
      let current = cfg.agent_defaults.show_thinking in
      let text =
        match action with
        | Slash_commands.ShowThinkingStatus ->
            Slash_commands_fmt.format_show_thinking_status ~connector current
        | Slash_commands.ToggleShowThinking -> (
            let new_val = not current in
            match Config_set.set_show_thinking new_val with
            | Ok () ->
                let agent_defaults =
                  { cfg.agent_defaults with show_thinking = new_val }
                in
                Session.update_config ~source:env.connector_name session_mgr
                  { cfg with agent_defaults };
                Logs.info (fun m ->
                    m "%s show_thinking toggled %s=%s %s=%s from=%b to=%b"
                      env.log_name env.show_thinking_channel_field
                      env.channel_id env.show_thinking_user_field env.user_id
                      current new_val);
                Slash_commands_fmt.format_show_thinking_toggle ~connector
                  new_val
            | Error err -> "Failed to update show_thinking: " ^ err)
      in
      env.send_formatted text
  | Heartbeat action ->
      let text =
        match action with
        | Slash_commands.HeartbeatStatus ->
            Slash_commands_fmt.format_heartbeat_status ~connector
              (Session.session_heartbeat_status_text session_mgr ~key)
        | Slash_commands.SetHeartbeat enabled -> (
            match Session.set_session_heartbeat session_mgr ~key ~enabled with
            | Ok () ->
                Slash_commands_fmt.format_heartbeat_set ~connector enabled key
            | Error err -> err)
      in
      env.send_formatted text
  | Debug action ->
      let text =
        match action with
        | Slash_commands.DebugStatus ->
            Slash_commands_fmt.format_debug_status ~connector
              (Session.session_debug_status_text session_mgr ~key)
        | Slash_commands.SetDebug enabled -> (
            match Session.set_session_debug session_mgr ~key ~enabled with
            | Ok () ->
                Slash_commands_fmt.format_debug_set ~connector enabled key
            | Error err -> err)
      in
      env.send_formatted text
  | AgentMenu page ->
      env.send_formatted (Slash_commands_fmt.format_agent_menu ~connector ~page)
  | ModelMenu page ->
      env.send_formatted (Slash_commands_fmt.format_model_menu ~connector ~page)
  | ThinkingMenu ->
      env.send_formatted (Slash_commands_fmt.format_thinking_menu ~connector)
  | ConfigMenu page ->
      env.send_formatted
        (Slash_commands_fmt.format_config_menu ~connector ~page)
  | SkillsMenu page ->
      let show_test = env.is_admin in
      env.send_formatted
        (Slash_commands_fmt.format_skills_menu ~connector ~page ~show_test ())
  | CostsMenu ->
      env.send_formatted (Slash_commands_fmt.format_costs_menu ~connector)
  | BgMenu -> env.send_formatted (Slash_commands_fmt.format_bg_menu ~connector)
  | Tools ->
      let show_test = env.is_admin in
      let text =
        match Session.get_tool_registry session_mgr with
        | Some reg ->
            let tools, _ = Tool_registry.partition_skills reg in
            let tools = Skills.filter_visible_tools ~show_test tools in
            let skills =
              Skills.filter_visible_tools ~show_test
                (Skills.available_skills_as_tools ())
            in
            Slash_commands.format_tools ~connector tools skills
              (Agent_template.available_templates ())
        | None -> "Tools are not enabled."
      in
      env.send_formatted text
  | Tasks ->
      let raw =
        match Session.get_db session_mgr with
        | Some db ->
            Task_tree.init_schema db;
            Task_tree.render_emoji_tree ~db ~session_key:key ()
        | None -> "Tasks are not available (no database)."
      in
      env.send_formatted (Slash_commands_fmt.format_tasks ~connector raw)
  | TasksFull ->
      let raw =
        match Session.get_db session_mgr with
        | Some db ->
            Task_tree.init_schema db;
            Task_tree.render_tree_with_legend ~db ~session_key:key
        | None -> "Tasks are not available (no database)."
      in
      env.send_formatted (Slash_commands_fmt.format_tasks ~connector raw)
  | Costs action ->
      let text =
        match Session.get_db session_mgr with
        | Some db -> Slash_commands.format_costs ~connector ~db action
        | None -> "Costs are not available (no database)."
      in
      env.send_formatted text
  | Session action ->
      let text =
        match Session.get_db session_mgr with
        | Some db ->
            Slash_commands_sessions.format_session ~connector ~db action
        | None -> "Sessions not available (no database)."
      in
      env.send_formatted text
  | Usage action ->
      let text =
        match Session.get_db session_mgr with
        | Some db -> Slash_commands.format_usage ~connector ~db action
        | None -> "Usage is not available (no database)."
      in
      env.send_formatted text
  | Active ->
      let text =
        match Session.get_db session_mgr with
        | Some db ->
            let config = Session.get_config session_mgr in
            Slash_commands.format_active ~connector ~db ~config ()
        | None -> "Active usage is not available (no database)."
      in
      env.send_formatted text
  | Bg action ->
      let* text =
        match Session.get_db session_mgr with
        | Some db -> Slash_commands.format_bg ~connector ~db action
        | None -> Lwt.return "Background tasks are not available (no database)."
      in
      env.send_formatted text
  | Cron action ->
      let text =
        match Session.get_db session_mgr with
        | Some db ->
            Slash_commands.format_cron ~connector ~db ~session_key:key action
        | None -> "Cron is not available (no database)."
      in
      env.send_formatted text
  | Bl action -> env.send_formatted (Slash_commands.format_bl ~connector action)
  | HeldItems action ->
      let text =
        match Session.get_db session_mgr with
        | Some db -> Slash_commands.format_held_items ~connector ~db action
        | None -> "Held items are not available (no database)."
      in
      env.send_formatted text
  | Memories action ->
      let text =
        match Session.get_db session_mgr with
        | Some db -> Slash_commands.format_memories ~connector ~db action
        | None -> "Memories are not available (no database)."
      in
      env.send_formatted text
  | Repo action -> (
      match Session.get_db session_mgr with
      | Some db ->
          Slash_commands_repo.handle_repo_action ~db ~session_key:key ~connector
            ~send_reply:env.send_formatted
            ~set_cwd:(fun cwd ->
              Session.set_effective_cwd session_mgr ~key ~cwd)
            action
      | None ->
          env.send_plain "Repository management is not available (no database)."
      )
  (* Commands with per-connector behaviour are handled by each connector's own
     match and never routed here; treat as a no-op for totality. *)
  | Compact | Delegate _ | ForkAnd _ | AgentInvoke _ | Debate _ | BashRun _
  | DebugDumpChat | Rig _ | Model _ | InjectConnectorHistory _ | SkillInvoke _
  | AdminRequired _ | NotACommand ->
      Lwt.return_unit

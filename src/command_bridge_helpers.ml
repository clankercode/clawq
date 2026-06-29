(* Command-bridge helper hub.
   Foundation + gateway/pairing live in command_bridge_gateway.ml;
   usage/cost handlers in command_bridge_usage.ml;
   session display helpers in command_bridge_session_fmt.ml.
   All are re-exported here via [include] so the public
   Command_bridge_helpers.* surface is unchanged. *)

include Command_bridge_gateway

let redact_key = Tui_input.redact

let make_sandbox (cfg : Runtime_config.t) =
  let ws = Runtime_config.effective_workspace cfg in
  let backend = Sandbox.backend_of_policy cfg.security.sandbox_backend in
  Sandbox.create ~backend ~workspace:ws
    ~extra_allowed_paths:cfg.security.extra_allowed_paths
    ~workspace_only:cfg.security.workspace_only ()

let shell_visible_roots_summary (cfg : Runtime_config.t) =
  let workspace = Runtime_config.effective_workspace cfg in
  let extra_allowed_paths =
    cfg.security.extra_allowed_paths |> List.map Runtime_config.expand_home
  in
  if not cfg.security.workspace_only then
    "unrestricted host filesystem view (tool-level checks relaxed)"
  else
    String.concat ", "
      (List.sort_uniq String.compare (workspace :: extra_allowed_paths))

let shell_policy_summary (cfg : Runtime_config.t) sandbox =
  let allowlist = "shell allowlist + path checks" in
  if not cfg.security.workspace_only then
    ( allowlist
      ^ "; workspace_only disabled; shell can access the host filesystem",
      false )
  else
    match sandbox.Sandbox.backend with
    | Sandbox.None ->
        ( allowlist
          ^ "; OS-level filesystem sandbox disabled; workspace boundaries are \
             enforced by tool validation only",
          false )
    | _ ->
        ( Printf.sprintf
            "%s; OS-level filesystem sandbox enabled via %s with workspace \
             isolation"
            allowlist
            (Sandbox.backend_to_string sandbox.Sandbox.backend),
          true )

let apply_agent_template_restrictions registry (tmpl : Agent_template.t) =
  let tools = Tool_registry.list registry in
  let filtered =
    match tmpl.allowed_tools with
    | [] -> tools
    | allowed -> List.filter (fun (t : Tool.t) -> List.mem t.name allowed) tools
  in
  let filtered =
    match tmpl.disallowed_tools with
    | [] -> filtered
    | denied ->
        List.filter (fun (t : Tool.t) -> not (List.mem t.name denied)) filtered
  in
  let new_reg = Tool_registry.create () in
  List.iter (Tool_registry.register new_reg) filtered;
  new_reg

let build_tool_registry ?db ?agent_template (cfg : Runtime_config.t) =
  if not cfg.security.tools_enabled then None
  else begin
    let registry = Tool_registry.create () in
    let sandbox = make_sandbox cfg in
    Tools_builtin.register_all ~config:cfg ~sandbox ?db registry;
    let skills =
      Skills.load_all ~workspace_only:cfg.security.workspace_only
        ~allowed_commands:Tools_builtin.default_shell_allowlist ()
    in
    List.iter (fun s -> Tool_registry.register_skill registry s) skills;
    Tool_registry.register registry (Skills.skill_create_tool ());
    let workspace = Runtime_config.effective_workspace cfg in
    Tool_registry.register registry
      (Skills.skill_list_tool ~workspace_dir:workspace ());
    let _cache = Skills.init_cache ~workspace_dir:workspace () in
    Tool_registry.register registry
      (Skills.use_skill_tool ~workspace_only:cfg.security.workspace_only ());
    Session_turn.expand_skill_refs_fn := Skills.expand_skill_refs;
    (Agent.find_skill_for_reload_fn :=
       fun name ->
         match Skills.find_skill_md name with
         | Some s -> Some (s.meta.md_description, s.instructions)
         | None -> None);
    match agent_template with
    | Some tmpl -> Some (apply_agent_template_restrictions registry tmpl)
    | None -> Some registry
  end

let format_debug_messages messages =
  let lines = ref [] in
  let add line = lines := line :: !lines in
  List.iter
    (fun (msg : Provider.message) ->
      add (Printf.sprintf "--- %s ---" msg.role);
      add msg.content;
      add "")
    messages;
  String.concat "\n" (List.rev !lines)

let cmd_debug_prompt args =
  let cfg : Runtime_config.t = get_config () in
  let db = get_db () in
  let tool_registry = build_tool_registry ~db:(Some db) cfg in
  let provider_name, _provider, model =
    Provider.select_provider ~config:cfg ()
  in
  let agent = Agent.create ~config:cfg ?tool_registry () in
  let session_key = "__debug_prompt__" in
  let user_message =
    match String.concat " " args with "" -> "Hello!" | msg -> msg
  in
  let messages =
    if user_message = "" then Agent.build_messages agent
    else begin
      let compaction_info =
        Lwt_main.run (Agent.prepare_turn_history agent ~user_message ())
      in
      let compacted = Option.is_some compaction_info in
      let sandbox = make_sandbox cfg in
      let shell_policy_summary, shell_is_sandboxed =
        shell_policy_summary cfg sandbox
      in
      let background_tasks =
        begin
          Background_task.init_schema db;
          Background_task.list_tasks ~db
          |> List.filter (fun (t : Background_task.task) ->
              match t.Background_task.status with
              | Background_task.Queued | Background_task.Running -> true
              | _ -> false)
          |> List.sort
               (fun (a : Background_task.task) (b : Background_task.task) ->
                 compare a.Background_task.id b.Background_task.id)
          |> List.map (fun (t : Background_task.task) ->
              {
                Prompt_builder.id = t.Background_task.id;
                runner = Background_task.string_of_runner t.runner;
                repo_label = Filename.basename t.repo_path;
                branch = (if t.branch = "" then "(auto)" else t.branch);
                status = Background_task.string_of_status t.status;
                health =
                  Background_task.string_of_health
                    (Background_task.diagnose_health t);
                elapsed = Background_task.elapsed_string t;
              })
        end
      in
      let task_tree_summary =
        begin
          Task_tree.init_schema db;
          let effective_key =
            match
              Task_tree.find_active_session_key ~db ~preferred:"__main__"
            with
            | Some k -> k
            | None -> "__main__"
          in
          Some
            (Task_tree.render_tree_with_legend ~db ~session_key:effective_key)
        end
      in
      let heartbeat_routing_applies =
        cfg.heartbeat.enabled
        && Session.heartbeat_supported_session_key session_key
        && Memory.session_heartbeat_enabled ~db ~session_key
      in
      let runtime_context =
        Prompt_builder.build_runtime_context ~config:cfg
          ~details:
            {
              Prompt_builder.session_id = session_key;
              session_name = Some "debug prompt";
              is_main_session = false;
              heartbeat_routing_applies;
              effective_workspace = Runtime_config.effective_workspace cfg;
              workspace_only = cfg.security.workspace_only;
              sandbox_backend_requested = cfg.security.sandbox_backend;
              sandbox_backend_effective =
                Sandbox.backend_to_string sandbox.Sandbox.backend;
              shell_is_sandboxed;
              shell_policy_summary;
              shell_visible_roots_summary = shell_visible_roots_summary cfg;
              daemon_uptime_line =
                Daemon_status.daemon_runtime_context_line
                  ~pid:(Daemon_status.read_current_daemon_pid ());
              background_tasks;
              context_usage =
                Some
                  (Agent.runtime_context_usage agent
                     ~compacted_before_turn:compacted);
              tunnel_status_line =
                Some ("- Tunnel: " ^ !Prompt_builder.tunnel_status_line_fn ());
              task_tree_summary;
            }
          ()
      in
      Agent.build_messages ?runtime_context agent
    end
  in
  Printf.sprintf "provider: %s\nmodel: %s\n\n%s" provider_name model
    (format_debug_messages messages)

let cmd_status () =
  let cfg = get_config () in
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add "clawq status";
  add (Printf.sprintf "  config dir: %s" (Dot_dir.path ()));
  add (Printf.sprintf "  model: %s" cfg.agent_defaults.primary_model);
  (match
     Runtime_config.primary_model_deprecation_warning cfg.agent_defaults
   with
  | Some warn -> add ("  " ^ warn)
  | None -> ());
  (match Runtime_config.default_provider_deprecation_warning cfg with
  | Some warn -> add ("  " ^ warn)
  | None -> ());
  add (Printf.sprintf "  temperature: %.2f" cfg.default_temperature);
  add (Printf.sprintf "  gateway: %s:%d" cfg.gateway.host cfg.gateway.port);
  add
    (Printf.sprintf "  gateway auth: %s"
       (match cfg.gateway.auth_token with
       | Some _ -> "enabled"
       | None -> "disabled"));
  add
    (Printf.sprintf "  cli channel: %s"
       (if cfg.channels.cli then "enabled" else "disabled"));
  add
    (Printf.sprintf "  telegram: %s"
       (match cfg.channels.telegram with
       | None -> "not configured"
       | Some tg ->
           if Runtime_config.telegram_has_valid_credentials tg then
             Printf.sprintf "enabled (%d account(s))" (List.length tg.accounts)
           else "disabled (no auth)"));
  add
    (Printf.sprintf "  discord: %s"
       (match cfg.channels.discord with
       | None -> "not configured"
       | Some d ->
           if Runtime_config.discord_has_valid_credentials d then
             Printf.sprintf "enabled (guilds=%d users=%d)"
               (List.length d.allow_guilds)
               (List.length d.allow_users)
           else "disabled (no auth)"));
  add
    (Printf.sprintf "  slack: %s"
       (match cfg.channels.slack with
       | None -> "not configured"
       | Some s ->
           if Runtime_config.slack_has_valid_credentials s then
             Printf.sprintf "enabled (path=%s socket_mode=%b)" s.events_path
               s.socket_mode
           else "disabled (no auth)"));
  add
    (Printf.sprintf "  teams: %s"
       (match cfg.channels.teams with
       | None -> "not configured"
       | Some t ->
           if Runtime_config.teams_has_valid_credentials t then
             Printf.sprintf "enabled (teams=%d users=%d)"
               (List.length t.allow_teams)
               (List.length t.allow_users)
           else "disabled (no auth)"));
  add
    (Printf.sprintf "  github: %s"
       (match cfg.channels.github with
       | None -> "not configured"
       | Some g ->
           if Runtime_config.github_has_valid_credentials g then
             Printf.sprintf "enabled (repos=%d)" (List.length g.repos)
           else "disabled (no auth)"));
  let other_channel_status =
    let enabled = ref [] in
    let disabled = ref [] in
    let check_channel name opt valid_fn =
      match opt with
      | None -> disabled := name :: !disabled
      | Some c ->
          if valid_fn c then enabled := name :: !enabled
          else disabled := name :: !disabled
    in
    check_channel "mattermost" cfg.channels.mattermost
      Runtime_config.mattermost_has_valid_credentials;
    check_channel "dingtalk" cfg.channels.dingtalk
      Runtime_config.dingtalk_has_valid_credentials;
    check_channel "matrix" cfg.channels.matrix
      Runtime_config.matrix_has_valid_credentials;
    check_channel "email" cfg.channels.email
      Runtime_config.email_has_valid_credentials;
    check_channel "whatsapp" cfg.channels.whatsapp
      Runtime_config.whatsapp_has_valid_credentials;
    check_channel "nostr" cfg.channels.nostr
      Runtime_config.nostr_has_valid_credentials;
    check_channel "lark" cfg.channels.lark
      Runtime_config.lark_has_valid_credentials;
    check_channel "line" cfg.channels.line
      Runtime_config.line_has_valid_credentials;
    check_channel "onebot" cfg.channels.onebot
      Runtime_config.onebot_has_valid_credentials;
    check_channel "irc" cfg.channels.irc
      Runtime_config.irc_has_valid_credentials;
    check_channel "signal" cfg.channels.signal
      Runtime_config.signal_has_valid_credentials;
    check_channel "imessage" cfg.channels.imessage
      Runtime_config.imessage_has_valid_credentials;
    (List.rev !enabled, List.rev !disabled)
  in
  let enabled_others, disabled_others = other_channel_status in
  List.iter
    (fun name -> add (Printf.sprintf "  %s: enabled" name))
    enabled_others;
  if disabled_others <> [] then
    add
      (Printf.sprintf "  Disabled: %d others (%s)"
         (List.length disabled_others)
         (String.concat ", " disabled_others));
  add (Printf.sprintf "  memory backend: %s" cfg.memory.backend);
  add (Printf.sprintf "  providers: %d configured" (List.length cfg.providers));
  (match read_daemon_state () with
  | None -> add "  daemon: not running (start with 'clawq agent')"
  | Some json -> (
      let open Yojson.Safe.Util in
      try
        let pid = json |> member "pid" |> to_int in
        if pid_is_alive pid then
          add (Printf.sprintf "  daemon: running (pid %d)" pid)
        else begin
          add (Printf.sprintf "  daemon: stale state (pid %d not running)" pid);
          remove_daemon_state ()
        end
      with _ -> add "  daemon: state file found"));
  List.rev !lines |> String.concat "\n"

let cmd_doctor () =
  let cfg = get_config () in
  let issues = ref [] in
  let add s = issues := s :: !issues in
  if cfg.providers = [] then add "WARNING: No providers configured";
  List.iter
    (fun (name, (p : Runtime_config.provider_config)) ->
      if not (Runtime_config.provider_has_auth p) then
        add
          (Printf.sprintf "WARNING: Provider '%s' has no configured auth" name);
      List.iter add (Openai_codex_oauth.doctor_warnings ~provider_name:name p))
    cfg.providers;
  (match cfg.default_provider with
  | Some name -> (
      add
        "WARNING: \"default_provider\" is deprecated. Remove it from \
         config.json and set \"agent_defaults.primary_model\" to a \
         \"provider:model\" string (e.g. \"openrouter:gpt-5.4\") instead.";
      if not (List.exists (fun (n, _) -> n = name) cfg.providers) then
        add
          (Printf.sprintf
             "WARNING: default_provider '%s' not found in providers" name)
      else
        match List.assoc_opt name cfg.providers with
        | Some p when not (Runtime_config.provider_has_auth p) ->
            add
              (Printf.sprintf
                 "WARNING: default_provider '%s' has no configured auth" name)
        | _ -> ())
  | None -> ());
  (match cfg.channels.telegram with
  | None -> ()
  | Some tg ->
      List.iter
        (fun (name, (acct : Runtime_config.telegram_account)) ->
          if acct.bot_token = "" then
            add
              (Printf.sprintf
                 "WARNING: Telegram account '%s' has empty bot_token" name);
          if acct.allow_from = [] then
            add
              (Printf.sprintf
                 "WARNING: Telegram account '%s' has no allow_from entries" name))
        tg.accounts);
  if cfg.security.encrypt_secrets then
    List.iter
      (fun (name, (p : Runtime_config.provider_config)) ->
        if
          Runtime_config.is_key_set p.api_key
          && String.length p.api_key > 0
          && p.api_key.[0] <> '$'
        then
          add
            (Printf.sprintf
               "WARNING: Provider '%s' has plaintext API key but \
                encrypt_secrets is enabled. Use \"$ENV_VAR\" syntax to \
                reference environment variables."
               name))
      cfg.providers;
  (if cfg.tunnel.enabled then
     let binary =
       match cfg.tunnel.provider with
       | "cloudflare" | "cf" -> Some "cloudflared"
       | "tailscale" -> Some "tailscale"
       | "ngrok" -> Some "ngrok"
       | _ -> None
     in
     match binary with
     | None -> ()
     | Some bin ->
         if Sys.command (Printf.sprintf "which %s >/dev/null 2>&1" bin) <> 0
         then
           add
             (Printf.sprintf
                "WARNING: tunnel provider '%s' requires '%s' in PATH"
                cfg.tunnel.provider bin));
  (* Teams channel checks *)
  (match cfg.channels.teams with
  | None -> (
      let raw_path = Dot_dir.config_path () in
      if Sys.file_exists raw_path then
        try
          let raw = Yojson.Safe.from_file raw_path in
          let open Yojson.Safe.Util in
          let tm =
            try raw |> member "channels" |> member "teams" with _ -> `Null
          in
          if tm <> `Null then
            add
              "WARNING: channels.teams is present in config but failed to load \
               (check that app_id, app_secret, and tenant_id are all set)"
        with _ -> ())
  | Some t ->
      if t.app_id = "" then add "WARNING: channels.teams.app_id is empty"
      else if t.tenant_id = "" then
        add "WARNING: channels.teams.tenant_id is empty"
      else if t.webhook_path = "" then
        add "WARNING: channels.teams.webhook_path is empty");
  let result =
    match List.rev !issues with
    | [] -> "doctor: all checks passed"
    | issues -> "doctor: issues found\n" ^ String.concat "\n" issues
  in
  result

let cmd_onboard () =
  let config_dir = Dot_dir.path () in
  let config_path = Dot_dir.config_path () in
  if Sys.file_exists config_path then
    "Config already exists at " ^ config_path
    ^ "\nRun 'clawq config wizard' to reconfigure, or edit directly."
  else if Unix.isatty Unix.stdin then begin
    Config_wizard_tui.run_wizard Config_wizard_model.Onboard;
    ""
  end
  else begin
    (try if not (Sys.file_exists config_dir) then Sys.mkdir config_dir 0o755
     with _ -> ());
    let template =
      {|{
  "default_temperature": 0.7,
  "providers": {
    "openai-codex": {
      "api_key": "YOUR_API_KEY_HERE",
      "base_url": "https://api.openai.com/v1"
    }
  },
  "agent_defaults": {
    "primary_model": "openai-codex:gpt-5.4"
  },
  "security": {
    "workspace_only": true,
    "tools_enabled": true
  }
}|}
    in
    (try
       let oc = open_out config_path in
       Fun.protect
         ~finally:(fun () -> close_out oc)
         (fun () -> output_string oc template)
     with exn ->
       failwith
         (Printf.sprintf "Failed to write config: %s" (Printexc.to_string exn)));
    "Created config template at " ^ config_path
    ^ "\nEdit it to add your API keys and bot tokens."
  end

let cmd_config args =
  match args with
  | [ "wizard" ] ->
      Config_wizard_tui.run_wizard Config_wizard_model.FullWizard;
      ""
  | "set" :: key :: value :: _ ->
      let result = Config_set.set_value key value in
      let base =
        if Config_set.is_secret_path key then
          Printf.sprintf "Set %s = %s" key (redact_key value)
        else result
      in
      if key = "agent_defaults.primary_model" then
        let pf = Pmodel.parse_flexible value in
        match Pmodel.deprecation_warning pf with
        | Some warn -> base ^ "\n" ^ warn
        | None -> base
      else base
  | [ "set"; key ] when Config_set.is_secret_path key -> (
      let prompt = Printf.sprintf "Enter value for '%s': " key in
      match Tui_input.read_secret prompt with
      | Error msg -> msg
      | Ok value -> (
          match Config_set.set_json_value key (`String value) with
          | Ok () -> Printf.sprintf "Set %s = %s" key (redact_key value)
          | Error err -> err))
  | [ "get"; key ] -> Config_set.get_value_redacted key
  | "show" :: rest -> Config_show.show (List.nth_opt rest 0)
  | [ "tree"; "keys" ] -> Config_tree.render_current ~show_values:false ()
  | "tree" :: rest ->
      Config_tree.render_current ?section:(List.nth_opt rest 0) ()
  | "search" :: rest -> (
      match rest with
      | [ query ] -> Config_search.search query
      | [] -> Config_search.search ""
      | _ -> Config_search.search (String.concat " " rest))
  | _ ->
      "Usage: clawq config <subcommand>\n\n\
       Subcommands:\n\
      \  wizard           Interactive configuration wizard\n\
      \  set KEY VALUE    Set a config value by dot-path\n\
      \  set KEY          Prompt for value (secret keys only, hidden input)\n\
      \  get KEY          Get a config value by dot-path (secrets redacted)\n\
      \  show [SECTION]   Display current config (secrets redacted)\n\
      \  tree [SECTION]   Render config as a tree (secrets redacted)\n\
      \  tree keys        Render config tree, structure only (no values)\n\
      \  search QUERY     Search config keys matching QUERY"

include Command_bridge_usage

let cmd_channel_test_teams () =
  let cfg = get_config () in
  match cfg.channels.teams with
  | None ->
      "Teams channel is not configured.\n\
       Set channels.teams.app_id, app_secret, and tenant_id in config.\n\
       Run: clawq config set channels.teams.app_id \"your-app-id\""
  | Some tc -> (
      match Lwt_main.run (Teams.test_connection ~config:tc) with
      | Ok msg -> msg
      | Error msg -> "Teams connection FAILED\n" ^ msg)

let channel_model_suffix cfg channel_type =
  match Runtime_config.channel_default_model cfg ~channel_type with
  | Some m -> Printf.sprintf "; default_model: %s" m
  | None -> ""

let cmd_channel_status () =
  let cfg = get_config () in
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add "Configured channels:";
  add
    (Printf.sprintf "  cli: %s"
       (if cfg.channels.cli then "enabled" else "disabled"));
  (match cfg.channels.telegram with
  | None -> add "  telegram: not configured"
  | Some tg ->
      List.iter
        (fun (name, (acct : Runtime_config.telegram_account)) ->
          add
            (Printf.sprintf "  telegram/%s: %s (allow_from: %s%s)" name
               (if Runtime_config.telegram_account_has_valid_credentials acct
                then "configured"
                else "not configured (missing bot_token)")
               (String.concat ", " acct.allow_from)
               (channel_model_suffix cfg "telegram")))
        tg.accounts);
  (match cfg.channels.discord with
  | None -> add "  discord: not configured"
  | Some d ->
      if Runtime_config.discord_has_valid_credentials d then
        add
          (Printf.sprintf
             "  discord: configured (allow_guilds: %s; allow_users: %s; \
              intents: %d%s)"
             (String.concat ", " d.allow_guilds)
             (String.concat ", " d.allow_users)
             d.intents
             (channel_model_suffix cfg "discord"))
      else add "  discord: not configured (missing bot_token)");
  (match cfg.channels.slack with
  | None -> add "  slack: not configured"
  | Some s ->
      if Runtime_config.slack_has_valid_credentials s then
        add
          (Printf.sprintf
             "  slack: configured (events_path: %s; socket_mode: %b; \
              allow_channels: %s; allow_users: %s%s)"
             s.events_path s.socket_mode
             (String.concat ", " s.allow_channels)
             (String.concat ", " s.allow_users)
             (channel_model_suffix cfg "slack"))
      else add "  slack: not configured (missing bot_token or signing_secret)");
  (match cfg.channels.teams with
  | None -> add "  teams: not configured"
  | Some t ->
      if Runtime_config.teams_has_valid_credentials t then
        add
          (Printf.sprintf
             "  teams: configured (app_id: %s...; webhook_path: %s; \
              allow_teams: %s; allow_users: %s%s)"
             (String.sub t.app_id 0 (min 8 (String.length t.app_id)))
             t.webhook_path
             (String.concat ", " t.allow_teams)
             (String.concat ", " t.allow_users)
             (channel_model_suffix cfg "teams"))
      else
        add "  teams: not configured (missing app_id, app_secret, or tenant_id)");
  List.rev !lines |> String.concat "\n"

let cmd_channel_set_model name model =
  let valid_channels = Runtime_config.all_channel_types in
  if not (List.mem name valid_channels) then
    Printf.sprintf "Error: unknown channel '%s'. Valid channels: %s" name
      (String.concat ", " valid_channels)
  else
    let pf = Pmodel.parse_flexible model in
    let key = Printf.sprintf "channels.%s.default_model" name in
    match Config_set.set_json_value key (`String model) with
    | Error e -> e
    | Ok () -> (
        let base = Printf.sprintf "Set %s default_model = %s" name model in
        match Pmodel.deprecation_warning pf with
        | Some warn -> base ^ "\n" ^ warn
        | None -> base)

let cmd_channel_show_model name =
  let valid_channels = Runtime_config.all_channel_types in
  if not (List.mem name valid_channels) then
    Printf.sprintf "Error: unknown channel '%s'. Valid channels: %s" name
      (String.concat ", " valid_channels)
  else
    let cfg = get_config () in
    let channel_model =
      Runtime_config.channel_default_model cfg ~channel_type:name
    in
    let global_model = cfg.agent_defaults.primary_model in
    match channel_model with
    | Some m ->
        Printf.sprintf "%s default_model: %s (global: %s)" name m global_model
    | None ->
        Printf.sprintf "%s default_model: (not set, inherits global: %s)" name
          global_model

let cmd_channel_clear_model name =
  let valid_channels = Runtime_config.all_channel_types in
  if not (List.mem name valid_channels) then
    Printf.sprintf "Error: unknown channel '%s'. Valid channels: %s" name
      (String.concat ", " valid_channels)
  else
    let key = Printf.sprintf "channels.%s.default_model" name in
    match Config_set.set_json_value key `Null with
    | Error e -> e
    | Ok () ->
        Printf.sprintf "Cleared %s default_model (now inherits global)" name

let cmd_channel args =
  match args with
  | [] -> cmd_channel_status ()
  | [ "set-model"; name; model ] -> cmd_channel_set_model name model
  | [ "set-model"; _ ] | [ "set-model" ] ->
      "Usage: channel set-model <channel_name> <model>\n\
       Example: channel set-model discord anthropic:claude-opus-4-6"
  | [ "show-model"; name ] -> cmd_channel_show_model name
  | [ "show-model" ] ->
      "Usage: channel show-model <channel_name>\n\
       Example: channel show-model discord"
  | [ "clear-model"; name ] -> cmd_channel_clear_model name
  | [ "clear-model" ] ->
      "Usage: channel clear-model <channel_name>\n\
       Example: channel clear-model discord"
  | unknown :: _ ->
      Printf.sprintf
        "Unknown channel subcommand: %s\n\
         Usage: channel [set-model|show-model|clear-model] ..."
        unknown

let cmd_memory args =
  let cfg = get_config () in
  let base_status () =
    Printf.sprintf "Memory backend: %s\nSearch enabled: %b" cfg.memory.backend
      cfg.memory.search_enabled
  in
  let grant_usage =
    "Usage: clawq memory grant <create|revoke> <scope_id> <principal_kind> \
     <principal_id> <capability>"
  in
  let admin_grant_error =
    "Error: memory grant mutation requires admin privileges. Set CLAWQ_ADMIN=1 \
     in your environment."
  in
  let is_admin =
    match Sys.getenv_opt "CLAWQ_ADMIN" with
    | Some v -> v = "1" || v = "true"
    | None -> false
  in
  let parse_scope_id value =
    match int_of_string_opt value with
    | Some id -> Ok id
    | None -> Error grant_usage
  in
  match args with
  | [] | [ "status" ] ->
      let db = get_db () in
      let count = Memory.count_core ~db in
      base_status () ^ Printf.sprintf "\nCore memories: %d" count
  | [ "stats" ] ->
      let db = get_db () in
      let count = Memory.count_core ~db in
      base_status () ^ Printf.sprintf "\nCore memories: %d" count
  | [
   "grant"; "create"; scope_id_arg; principal_kind; principal_id; capability;
  ] -> (
      if not is_admin then admin_grant_error
      else
        match parse_scope_id scope_id_arg with
        | Error msg -> msg
        | Ok scope_id -> (
            let db = get_db () in
            let ledger ~room_id ~event_type ~actor ~metadata =
              ignore
                (Room_activity_ledger.append_now ~db ~room_id ~event_type ~actor
                   ~metadata)
            in
            match
              Memory.grant_access ~db ~is_admin ~scope_id ~principal_kind
                ~principal_id ~capability ~ledger ()
            with
            | Ok () -> "Created memory grant"
            | Error msg -> "Error: " ^ msg))
  | [
   "grant"; "revoke"; scope_id_arg; principal_kind; principal_id; capability;
  ] -> (
      if not is_admin then admin_grant_error
      else
        match parse_scope_id scope_id_arg with
        | Error msg -> msg
        | Ok scope_id -> (
            let db = get_db () in
            let ledger ~room_id ~event_type ~actor ~metadata =
              ignore
                (Room_activity_ledger.append_now ~db ~room_id ~event_type ~actor
                   ~metadata)
            in
            match
              Memory.revoke_access ~db ~is_admin ~scope_id ~principal_kind
                ~principal_id ~capability ~ledger ()
            with
            | Ok removed -> Printf.sprintf "Revoked %d memory grant(s)" removed
            | Error msg -> "Error: " ^ msg))
  | "grant" :: _ -> grant_usage
  | "export" :: rest -> (
      let path =
        match rest with [ p ] -> p | _ -> Dot_dir.sub "memory_snapshot.json"
      in
      let db = get_db () in
      try
        let count = Memory.export_snapshot ~db ~path in
        Printf.sprintf "Exported %d core memories to %s" count path
      with exn -> "Error: " ^ Printexc.to_string exn)
  | "import" :: [ path ] -> (
      if not (Sys.file_exists path) then
        Printf.sprintf "File not found: %s" path
      else
        let db = get_db () in
        try
          let count = Memory.import_snapshot ~db ~path in
          Printf.sprintf "Imported %d core memories from %s" count path
        with exn -> "Error: " ^ Printexc.to_string exn)
  | "import" :: _ -> "Usage: clawq memory import <path>"
  | "list" :: rest ->
      let category =
        match rest with
        | [ "--category"; cat ] -> cat
        | [ cat ] when not (String.length cat > 0 && cat.[0] = '-') -> cat
        | _ -> ""
      in
      let db = get_db () in
      let results = Memory.list_core ~db ~category () in
      if results = [] then "No core memories found"
      else
        let lines =
          List.map
            (fun (key, content, cat) ->
              Printf.sprintf "[%s] (%s): %s" key cat content)
            results
        in
        String.concat "\n" lines
  | "store" :: key :: content :: rest -> (
      let category =
        match rest with [ "--category"; cat ] -> cat | _ -> "general"
      in
      let db = get_db () in
      try
        Memory.store_core ~db ~key ~content ~category ();
        Printf.sprintf "Stored memory: %s" key
      with exn -> "Error: " ^ Printexc.to_string exn)
  | "store" :: _ ->
      "Usage: clawq memory store <key> <content> [--category <category>]"
  | "forget" :: [ key ] ->
      let db = get_db () in
      let deleted = Memory.forget_core ~db ~key in
      if deleted then Printf.sprintf "Deleted memory: %s" key
      else Printf.sprintf "No memory found with key: %s" key
  | "forget" :: _ -> "Usage: clawq memory forget <key>"
  | _ ->
      "Usage: clawq memory <subcommand>\n\
      \  memory status                                    - Show memory status\n\
      \  memory stats                                     - Alias for status\n\
      \  memory list [--category <cat>]                   - List core memories\n\
      \  memory store <key> <content> [--category <cat>]  - Store a memory\n\
      \  memory forget <key>                              - Delete a memory\n\
      \  memory grant <create|revoke> <scope_id> <principal_kind> \
       <principal_id> <capability>\n\
      \                                                   - Admin-only grant \
       management\n\
      \  memory export [path]                             - Export to JSON\n\
      \  memory import <path>                             - Import from JSON"

let cmd_workspace args =
  let cfg = get_config () in
  let workspace = Runtime_config.effective_workspace cfg in
  match args with
  | [] -> Printf.sprintf "Workspace: %s" workspace
  | [ "backup" ] -> (
      let name = Workspace_version.auto_backup_name () in
      match Workspace_version.backup ~workspace ~name with
      | Ok files ->
          Printf.sprintf "Backed up %d file(s) as '%s'" (List.length files) name
      | Error e -> Printf.sprintf "Error: %s" e)
  | [ "backup"; name ] -> (
      match Workspace_version.backup ~workspace ~name with
      | Ok files ->
          Printf.sprintf "Backed up %d file(s) as '%s'" (List.length files) name
      | Error e -> Printf.sprintf "Error: %s" e)
  | [ "versions" ] | [ "list" ] ->
      let versions = Workspace_version.list_versions () in
      if versions = [] then "No workspace versions found."
      else
        let lines =
          List.map
            (fun (name, ts) -> Printf.sprintf "  %s  (%s)" name ts)
            versions
        in
        "Workspace versions:\n" ^ String.concat "\n" lines
  | [ "restore"; name ] -> (
      match Workspace_version.restore ~workspace ~name with
      | Ok files ->
          Printf.sprintf "Restored %d file(s) from '%s'" (List.length files)
            name
      | Error e -> Printf.sprintf "Error: %s" e)
  | [ "delete"; name ] -> (
      match Workspace_version.delete ~name with
      | Ok () -> Printf.sprintf "Deleted version '%s'" name
      | Error e -> Printf.sprintf "Error: %s" e)
  | _ ->
      "Usage: clawq workspace [backup [NAME] | versions | restore NAME | \
       delete NAME]"

include Command_bridge_session_fmt

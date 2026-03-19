(* Force-link provider_init.ml so its native-provider registrations run. *)
let _link_provider_init = Provider_init.registered
let get_config () = Config_loader.load ()
let daemon_state_path () = Dot_dir.sub "daemon_state.json"

let remove_daemon_state () =
  let path = daemon_state_path () in
  if Sys.file_exists path then try Sys.remove path with _ -> ()

let pid_is_alive pid =
  try
    Unix.kill pid 0;
    true
  with Unix.Unix_error _ -> false

let read_file path =
  try
    let ic = open_in path in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Some s
  with _ -> None

let proc_start_ticks pid =
  let path = Printf.sprintf "/proc/%d/stat" pid in
  match read_file path with
  | None -> None
  | Some stat -> (
      let idx = try Some (String.rindex stat ')') with _ -> None in
      match idx with
      | None -> None
      | Some i -> (
          let rest = String.sub stat (i + 2) (String.length stat - i - 2) in
          let fields =
            String.split_on_char ' ' rest |> List.filter (fun s -> s <> "")
          in
          try Some (List.nth fields 19) with _ -> None))

let proc_cmdline_contains ~needle pid =
  let path = Printf.sprintf "/proc/%d/cmdline" pid in
  match read_file path with
  | None -> false
  | Some s ->
      let hay = String.lowercase_ascii s in
      let nee = String.lowercase_ascii needle in
      let hlen = String.length hay in
      let nlen = String.length nee in
      let rec loop i =
        if i + nlen > hlen then false
        else if String.sub hay i nlen = nee then true
        else loop (i + 1)
      in
      nlen > 0 && loop 0

let read_daemon_state () =
  let path = daemon_state_path () in
  if Sys.file_exists path then
    try
      let json = Yojson.Safe.from_file path in
      Some json
    with _ -> None
  else None

let gateway_token_path () = Dot_dir.sub "gateway_token"

let read_gateway_token () =
  match read_file (gateway_token_path ()) with
  | Some token when String.trim token <> "" -> Some (String.trim token)
  | _ -> None

let save_gateway_token token =
  let token = String.trim token in
  let clawq_dir = Dot_dir.path () in
  (try if not (Sys.file_exists clawq_dir) then Sys.mkdir clawq_dir 0o700
   with _ -> ());
  let token_path = gateway_token_path () in
  let fd =
    Unix.openfile token_path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  let oc = Unix.out_channel_of_descr fd in
  Fun.protect
    (fun () -> output_string oc token)
    ~finally:(fun () -> close_out oc)

let read_live_daemon_gateway () =
  match read_daemon_state () with
  | None -> None
  | Some json -> (
      let open Yojson.Safe.Util in
      try
        let pid = json |> member "pid" |> to_int in
        if not (pid_is_alive pid) then begin
          remove_daemon_state ();
          None
        end
        else
          Some
            ( json |> member "gateway_host" |> to_string,
              json |> member "gateway_port" |> to_int )
      with _ -> None)

let read_daemon_tunnel_info () =
  match read_daemon_state () with
  | None -> None
  | Some json -> (
      let open Yojson.Safe.Util in
      try
        let pid = json |> member "pid" |> to_int in
        if not (pid_is_alive pid) then begin
          remove_daemon_state ();
          None
        end
        else
          match json |> member "tunnel" with
          | `Null -> None
          | tunnel_json ->
              let state = tunnel_json |> member "state" |> to_string in
              if state = "active" then
                let provider =
                  try tunnel_json |> member "provider" |> to_string
                  with _ -> "unknown"
                in
                let url =
                  match tunnel_json |> member "url" with
                  | `String u -> Some u
                  | _ -> None
                in
                Some (provider, url)
              else None
      with _ -> None)

let get_sync ~uri ~headers =
  Lwt_main.run
    (Lwt.catch
       (fun () ->
         let open Lwt.Syntax in
         let* status, resp_body = Http_client.get ~uri ~headers in
         Lwt.return (Ok (status, resp_body)))
       (fun exn -> Lwt.return (Error (Printexc.to_string exn))))

let try_localhost_gateway () =
  (* Try to detect a gateway running on localhost without requiring daemon_state.json *)
  let host = "127.0.0.1" in
  let port = 13451 in
  let url = Printf.sprintf "http://%s:%d/health" host port in
  match get_sync ~uri:url ~headers:[] with
  | Error _ -> None
  | Ok (200, _) -> Some (host, port)
  | Ok _ -> None

let gateway_auth_headers cfg =
  match (cfg.Runtime_config.gateway.auth_token, read_gateway_token ()) with
  | Some token, _ when String.trim token <> "" ->
      [ ("Authorization", "Bearer " ^ String.trim token) ]
  | None, Some token -> [ ("Authorization", "Bearer " ^ token) ]
  | _ -> []

let parse_json_error_body body =
  try
    let json = Yojson.Safe.from_string body in
    match Yojson.Safe.Util.member "error" json with
    | `String msg -> Some msg
    | _ -> None
  with _ -> None

let is_loopback_host host =
  match String.lowercase_ascii (String.trim host) with
  | "localhost" | "127.0.0.1" | "::1" -> true
  | _ -> false

let post_json_sync ~uri ~headers ~body =
  Lwt_main.run
    (Lwt.catch
       (fun () ->
         let open Lwt.Syntax in
         let* status, resp_body = Http_client.post_json ~uri ~headers ~body in
         Lwt.return (Ok (status, resp_body)))
       (fun exn -> Lwt.return (Error (Printexc.to_string exn))))

let read_live_gateway_pairing_code () =
  match read_daemon_state () with
  | None -> None
  | Some json -> (
      let open Yojson.Safe.Util in
      try
        let pid = json |> member "pid" |> to_int in
        if pid_is_alive pid then
          match json |> member "pairing_code" with
          | `String code when code <> "" -> Some code
          | _ -> None
        else begin
          remove_daemon_state ();
          None
        end
      with _ -> None)

type auto_pair_result = No_attempt | Paired of string | Pair_failed of string

let fetch_gateway_pairing_code ~host ~port =
  (* Fetch the pairing code directly from the running gateway via GET /pair.
     Only safe because this path is guarded by is_loopback_host. *)
  let url = Printf.sprintf "http://%s:%d/pair" host port in
  match get_sync ~uri:url ~headers:[] with
  | Error _ -> None
  | Ok (200, body) -> (
      try
        let json = Yojson.Safe.from_string body in
        let open Yojson.Safe.Util in
        match json |> member "code" with
        | `String code when String.trim code <> "" -> Some (String.trim code)
        | _ -> None
      with _ -> None)
  | Ok _ -> None

let try_auto_pair_live_gateway ~host ~port =
  if not (is_loopback_host host) then No_attempt
  else
    let code =
      match read_live_gateway_pairing_code () with
      | Some _ as c -> c
      | None -> fetch_gateway_pairing_code ~host ~port
    in
    match code with
    | None -> No_attempt
    | Some code -> (
        let url = Printf.sprintf "http://%s:%d/pair" host port in
        let body = `Assoc [ ("code", `String code) ] |> Yojson.Safe.to_string in
        match post_json_sync ~uri:url ~headers:[] ~body with
        | Error msg -> Pair_failed ("pairing request failed: " ^ msg)
        | Ok (status, resp_body) -> (
            match status with
            | 200 -> (
                try
                  let json = Yojson.Safe.from_string resp_body in
                  let open Yojson.Safe.Util in
                  match json |> member "token" with
                  | `String token when String.trim token <> "" ->
                      save_gateway_token token;
                      Paired (String.trim token)
                  | _ ->
                      Pair_failed
                        "pairing response did not contain a usable token"
                with exn ->
                  Pair_failed
                    (Printf.sprintf "failed to parse pairing response: %s"
                       (Printexc.to_string exn)))
            | _ ->
                Pair_failed
                  (match parse_json_error_body resp_body with
                  | Some msg -> msg
                  | None -> resp_body)))

let post_live_gateway_json ~cfg ~host ~port ~path ~body =
  let url = Printf.sprintf "http://%s:%d%s" host port path in
  let headers = gateway_auth_headers cfg in
  match post_json_sync ~uri:url ~headers ~body with
  | Ok ((401 | 403), _) as rejected -> (
      match try_auto_pair_live_gateway ~host ~port with
      | Paired token ->
          let retry_headers = [ ("Authorization", "Bearer " ^ token) ] in
          post_json_sync ~uri:url ~headers:retry_headers ~body
      | No_attempt -> rejected
      | Pair_failed msg -> Error ("Auto-pair failed: " ^ msg))
  | other -> other

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

let get_db () =
  let cfg = get_config () in
  let db_path =
    if cfg.memory.db_path <> "" then cfg.memory.db_path else Dot_dir.db_path ()
  in
  let clawq_dir = Dot_dir.path () in
  (try if not (Sys.file_exists clawq_dir) then Sys.mkdir clawq_dir 0o755
   with _ -> ());
  Memory.init ~db_path ~search_enabled:cfg.memory.search_enabled ()

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
       output_string oc template;
       close_out oc
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
      \  search QUERY     Search config keys matching QUERY"

let cmd_models args =
  match args with
  | [] | [ "list" ] ->
      let db_extras =
        try
          Model_discovery.get_db_only_models ~db:(get_db ())
            ~provider_filter:None
        with _ -> []
      in
      Models_catalog.to_plain_list ~db_extras ()
  | [ "list"; "--json" ] -> Yojson.Safe.to_string (Models_catalog.to_json ())
  | [ "list"; "--provider"; p ] ->
      let db_extras =
        try
          Model_discovery.get_db_only_models ~db:(get_db ())
            ~provider_filter:(Some p)
        with _ -> []
      in
      Models_catalog.to_plain_list ~provider_filter:(Some p) ~db_extras ()
  | [ "list"; "--provider"; p; "--json" ] ->
      Yojson.Safe.to_string
        (Models_catalog.to_json ~provider_filter:(Some p) ())
  | [ "list"; "--json"; "--provider"; p ] ->
      Yojson.Safe.to_string
        (Models_catalog.to_json ~provider_filter:(Some p) ())
  | [ "set-default"; model ] ->
      let provider, model_id, fmt = Models_catalog.split_name model in
      (* Plain name with no provider: reject if not in catalog *)
      if
        fmt = Models_catalog.Plain
        && Models_catalog.find_by_full_name model = None
      then
        Printf.sprintf
          "Error: model '%s' not found in catalog.\n\
           Hint: use provider:model format (e.g. anthropic:claude-sonnet-4-6) \
           to set an unknown model."
          model
      else
        (* Auto-normalize legacy provider/model to canonical provider:model *)
        let canonical_value, hint =
          match fmt with
          | Models_catalog.Legacy ->
              let canonical = provider ^ ":" ^ model_id in
              ( canonical,
                Printf.sprintf
                  "\nNote: normalized \"%s\" to canonical format \"%s\"." model
                  canonical )
          | Models_catalog.Plain -> (
              match Models_catalog.find_by_full_name model with
              | Some m when m.Models_catalog.provider <> "" ->
                  let canonical =
                    m.Models_catalog.provider ^ ":" ^ m.Models_catalog.id
                  in
                  ( canonical,
                    Printf.sprintf "\nNote: resolved bare model name to \"%s\"."
                      canonical )
              | _ -> (model, ""))
          | Models_catalog.Canonical -> (model, "")
        in
        let set_result =
          Config_set.set_value "agent_defaults.primary_model" canonical_value
        in
        let display_provider =
          match fmt with
          | Models_catalog.Canonical | Models_catalog.Legacy -> provider
          | Models_catalog.Plain -> (
              match Models_catalog.find_by_full_name model with
              | Some m when m.Models_catalog.provider <> "" ->
                  m.Models_catalog.provider
              | _ -> "")
        in
        let display_model =
          match fmt with
          | Models_catalog.Canonical | Models_catalog.Legacy -> model_id
          | Models_catalog.Plain -> model
        in
        let confirm =
          if display_provider <> "" then
            Printf.sprintf "Default model set to: %s (provider: %s)%s\n%s"
              display_model display_provider hint set_result
          else
            Printf.sprintf "Default model set to: %s%s\n%s" display_model hint
              set_result
        in
        confirm
  | [ "refresh" ] ->
      let db = get_db () in
      let config = get_config () in
      Lwt_main.run (Model_discovery.maybe_refresh ~db ~config ());
      "Model discovery refresh complete. Run 'clawq models list' to see \
       updated models."
  | [ "refresh"; "--force" ] ->
      let db = get_db () in
      let config = get_config () in
      Lwt_main.run (Model_discovery.maybe_refresh ~db ~force:true ~config ());
      "Model discovery force-refresh complete. Run 'clawq models list' to see \
       updated models."
  | [ "refresh"; "--provider"; pname ]
  | [ "refresh"; "--provider"; pname; "--force" ]
  | [ "refresh"; "--force"; "--provider"; pname ] -> (
      let config = get_config () in
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
      \  list [--provider P] [--json]  List known models (catalog + DB cache)\n\
      \  set-default MODEL            Set default model (e.g. \
       anthropic:claude-sonnet-4-6)\n\
      \  refresh [--force]            Refresh model list from provider APIs\n\
      \  refresh --provider PNAME     Refresh models for a specific provider"

let parse_since_arg args =
  let rec find = function
    | "--since" :: v :: _ -> Some v
    | _ :: rest -> find rest
    | [] -> None
  in
  match find args with
  | None -> None
  | Some period ->
      let now = Unix.gettimeofday () in
      let ts =
        match String.lowercase_ascii period with
        | "today" ->
            let tm = Unix.gmtime now in
            let midnight =
              { tm with Unix.tm_hour = 0; tm_min = 0; tm_sec = 0 }
            in
            let local_ts, _ = Unix.mktime midnight in
            let dummy_gm = Unix.gmtime 0.0 in
            let dummy_local = Unix.localtime 0.0 in
            let tz_offset_s =
              float_of_int
                (((dummy_local.Unix.tm_hour - dummy_gm.Unix.tm_hour) * 3600)
                + ((dummy_local.Unix.tm_min - dummy_gm.Unix.tm_min) * 60))
            in
            Some (local_ts -. tz_offset_s)
        | "7d" -> Some (now -. 604800.0)
        | "30d" -> Some (now -. 2592000.0)
        | "90d" -> Some (now -. 7776000.0)
        | _ -> None
      in
      ts

let parse_string_arg flag args =
  let rec find = function
    | f :: v :: _ when f = flag -> Some v
    | _ :: rest -> find rest
    | [] -> None
  in
  find args

let parse_int_arg flag args =
  match parse_string_arg flag args with
  | Some s -> ( try Some (int_of_string s) with _ -> None)
  | None -> None

let cmd_usage_history args =
  let db = get_db () in
  Provider_quota.set_db db;
  let provider = parse_string_arg "--provider" args in
  let since = parse_since_arg args in
  let limit =
    match parse_int_arg "--limit" args with Some n -> Some n | None -> Some 50
  in
  let json_mode = List.mem "--json" args in
  let entries =
    match provider with
    | Some p ->
        Provider_quota.history_for_provider ~db ~provider:p ?since ?limit ()
    | None -> Provider_quota.history_all ~db ?since ?limit ()
  in
  if entries = [] then "No quota history found."
  else if json_mode then
    let arr = `List (List.map Provider_quota.history_entry_to_json entries) in
    Yojson.Safe.pretty_to_string arr
  else
    let columns =
      Table_format.
        [
          { header = "TIME"; align = Left; min_width = 19; flex = false };
          { header = "PROVIDER"; align = Left; min_width = 10; flex = false };
          { header = "SESSION"; align = Right; min_width = 7; flex = false };
          { header = "WEEKLY"; align = Right; min_width = 7; flex = false };
          { header = "MONTHLY"; align = Right; min_width = 7; flex = false };
          { header = "STATUS"; align = Left; min_width = 6; flex = false };
        ]
    in
    let rows =
      List.map
        (fun (entry : Provider_quota.history_entry) ->
          let sess, week, mon =
            match entry.h_state with
            | Provider_quota.Unknown _ -> ("-", "-", "-")
            | Provider_quota.Known { session; weekly; monthly } ->
                let fmt_pct = function
                  | None -> "-"
                  | Some w -> Printf.sprintf "%.0f%%" w.Provider_quota.used_pct
                in
                (fmt_pct session, fmt_pct weekly, fmt_pct monthly)
          in
          let status =
            Provider_quota.status_label
              {
                provider_name = entry.h_provider;
                state = entry.h_state;
                fetched_at = entry.h_fetched_at;
              }
          in
          [ entry.h_recorded_at; entry.h_provider; sess; week; mon; status ])
        entries
    in
    "Quota History:\n" ^ Table_format.render columns rows

let cmd_usage_purge args =
  let db = get_db () in
  Provider_quota.set_db db;
  let now = Unix.gettimeofday () in
  let before =
    match args with
    | [] -> now -. 7776000.0
    | period :: _ -> (
        match String.lowercase_ascii period with
        | "7d" -> now -. 604800.0
        | "30d" -> now -. 2592000.0
        | "90d" -> now -. 7776000.0
        | "all" -> now +. 1.0
        | _ -> now -. 7776000.0)
  in
  let count = Provider_quota.purge_history ~db ~before () in
  Printf.sprintf "Purged %d quota history entries." count

let cmd_active _args =
  let db = get_db () in
  let cfg = get_config () in
  Provider_quota.set_db db;
  Provider_quota.set_cache_ttl cfg.quota_cache_ttl_s;
  Slash_commands.format_active ~connector:Format_adapter.Plain ~db ~config:cfg
    ()

let cmd_usage args =
  match args with
  | "history" :: rest -> cmd_usage_history rest
  | "purge" :: rest -> cmd_usage_purge rest
  | _ ->
      let refresh = List.mem "--refresh" args || List.mem "-r" args in
      let cfg = get_config () in
      let db = get_db () in
      Provider_quota.set_db db;
      Provider_quota.set_cache_ttl cfg.quota_cache_ttl_s;
      let results =
        if refresh then
          let refreshed =
            Lwt_main.run (Provider_quota.refresh_all ~config:cfg ())
          in
          List.map (fun pq -> (pq.Provider_quota.provider_name, pq)) refreshed
        else Provider_quota.get_all_cached ()
      in
      if results = [] then
        if refresh then "No providers configured."
        else
          "No cached quota data. Run 'clawq usage --refresh' to fetch current \
           data."
      else
        let threshold_for name =
          match List.assoc_opt name cfg.providers with
          | Some pc -> Option.value ~default:0.85 pc.quota_threshold
          | None -> 0.85
        in
        let columns =
          Table_format.
            [
              {
                header = "PROVIDER";
                align = Left;
                min_width = 10;
                flex = false;
              };
              { header = "SESSION"; align = Right; min_width = 7; flex = false };
              { header = "WEEKLY"; align = Right; min_width = 7; flex = false };
              { header = "MONTHLY"; align = Right; min_width = 7; flex = false };
              { header = "STATUS"; align = Left; min_width = 6; flex = false };
            ]
        in
        let rows =
          List.map
            (fun (_name, pq) ->
              let sess, week, mon =
                match pq.Provider_quota.state with
                | Provider_quota.Unknown _ -> ("-", "-", "-")
                | Provider_quota.Known { session; weekly; monthly } ->
                    let fmt_pct = function
                      | None -> "-"
                      | Some w ->
                          Printf.sprintf "%.0f%%" w.Provider_quota.used_pct
                    in
                    (fmt_pct session, fmt_pct weekly, fmt_pct monthly)
              in
              let status =
                Provider_quota.status_label
                  ~threshold:(threshold_for pq.Provider_quota.provider_name)
                  pq
              in
              [ pq.Provider_quota.provider_name; sess; week; mon; status ])
            results
        in
        "Provider Usage:\n" ^ Table_format.render columns rows

let cmd_provider args =
  match args with
  | "quota" :: rest -> (
      let cfg = get_config () in
      let db = get_db () in
      Provider_quota.set_db db;
      let target = match rest with [ name ] -> Some name | _ -> None in
      match target with
      | Some name when not (List.mem_assoc name cfg.providers) ->
          Printf.sprintf "Provider '%s' not configured" name
      | _ ->
          let providers_to_check =
            match target with
            | Some name -> (
                match List.assoc_opt name cfg.providers with
                | Some pc -> [ (name, pc) ]
                | None -> [])
            | None -> cfg.providers
          in
          Provider_quota.set_cache_ttl cfg.quota_cache_ttl_s;
          let results =
            Lwt_main.run
              (Lwt_list.map_s
                 (fun (name, pc) ->
                   Provider_quota.fetch_for_provider ~config:pc ~name ())
                 providers_to_check)
          in
          let threshold_for name =
            match List.assoc_opt name cfg.providers with
            | Some pc -> Option.value ~default:0.85 pc.quota_threshold
            | None -> 0.85
          in
          if results = [] then "No providers configured."
          else
            let lines =
              List.map
                (fun pq ->
                  let summary = Provider_quota.to_summary_string pq in
                  let label =
                    Provider_quota.status_label
                      ~threshold:(threshold_for pq.Provider_quota.provider_name)
                      pq
                  in
                  summary ^ "  " ^ label)
                results
            in
            String.concat "\n" lines)
  | "list" :: _ | [] -> cmd_models []
  | unknown :: _ ->
      Printf.sprintf
        "Unknown provider subcommand: %s\nUsage: provider quota [NAME]" unknown

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
  match args with
  | [] | [ "status" ] ->
      let db = get_db () in
      let count = Memory.count_core ~db in
      base_status () ^ Printf.sprintf "\nCore memories: %d" count
  | [ "stats" ] ->
      let db = get_db () in
      let count = Memory.count_core ~db in
      base_status () ^ Printf.sprintf "\nCore memories: %d" count
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

type session_list_args = {
  channel : string option;
  prefix : string option;
  activity : Memory.session_activity;
  only_main : bool option;
  include_postmortem : bool;
}

let parse_session_list_args args =
  let rec loop state = function
    | [] -> Ok state
    | "--channel" :: value :: rest ->
        loop { state with channel = Some value } rest
    | "--prefix" :: value :: rest ->
        loop { state with prefix = Some value } rest
    | "--active" :: rest -> loop { state with activity = Memory.Active } rest
    | "--inactive" :: rest ->
        loop { state with activity = Memory.Inactive } rest
    | "--main" :: rest -> loop { state with only_main = Some true } rest
    | "--non-main" :: rest -> loop { state with only_main = Some false } rest
    | "--include-postmortem" :: rest ->
        loop { state with include_postmortem = true } rest
    | flag :: _ when String.length flag > 0 && flag.[0] = '-' ->
        Error (Printf.sprintf "Unknown session list flag: %s" flag)
    | _ ->
        Error
          "Usage: clawq session list [--channel NAME] [--prefix PREFIX] \
           [--active|--inactive] [--main|--non-main] [--include-postmortem]"
  in
  loop
    {
      channel = None;
      prefix = None;
      activity = Memory.Any;
      only_main = None;
      include_postmortem = false;
    }
    args

type session_show_args = {
  epoch : Memory.epoch_selector option;
  offset : int;
  limit : int option;
}

let parse_session_show_args args =
  let rec loop epoch offset limit = function
    | [] -> Ok { epoch; offset; limit }
    | "--epoch" :: "current" :: rest ->
        loop (Some Memory.Current) offset limit rest
    | "--epoch" :: value :: rest -> (
        match int_of_string_opt value with
        | Some id when id > 0 ->
            loop (Some (Memory.Archived id)) offset limit rest
        | _ -> Error (Printf.sprintf "Invalid epoch value: %s" value))
    | "--offset" :: value :: rest -> (
        match int_of_string_opt value with
        | Some n when n >= 0 -> loop epoch n limit rest
        | _ -> Error (Printf.sprintf "Invalid offset value: %s" value))
    | "--limit" :: value :: rest -> (
        match int_of_string_opt value with
        | Some n when n > 0 -> loop epoch offset (Some n) rest
        | _ -> Error (Printf.sprintf "Invalid limit value: %s" value))
    | flag :: _ when String.length flag > 0 && flag.[0] = '-' ->
        Error (Printf.sprintf "Unknown session show flag: %s" flag)
    | _ ->
        Error
          "Usage: clawq session show SESSION [--epoch current|ID] [--offset N] \
           [--limit N]"
  in
  loop None 0 None args

(* B235: session events helpers *)
let string_contains haystack needle =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let rec loop i =
      if i + nlen > hlen then false
      else if String.sub haystack i nlen = needle then true
      else loop (i + 1)
    in
    loop 0

type session_events_args = {
  ev_epoch : Memory.epoch_selector option;
  ev_type : string option;
}

let parse_session_events_args args =
  let rec loop epoch filter_type = function
    | [] -> Ok { ev_epoch = epoch; ev_type = filter_type }
    | "--epoch" :: "current" :: rest ->
        loop (Some Memory.Current) filter_type rest
    | "--epoch" :: value :: rest -> (
        match int_of_string_opt value with
        | Some id when id > 0 ->
            loop (Some (Memory.Archived id)) filter_type rest
        | _ -> Error (Printf.sprintf "Invalid epoch value: %s" value))
    | "--type" :: value :: rest -> loop epoch (Some value) rest
    | flag :: _ when String.length flag > 0 && flag.[0] = '-' ->
        Error (Printf.sprintf "Unknown session events flag: %s" flag)
    | _ ->
        Error
          "Usage: clawq session events SESSION [--epoch current|ID] [--type \
           TYPE]"
  in
  loop None None args

let classify_event_message (row : Memory.raw_message) =
  match row.role with
  | "event" ->
      if string_contains row.content "workspace context refreshed" then
        "workspace_refresh"
      else "unknown_event"
  | "system" ->
      if string_contains row.content "Relevant context from memory:" then
        "memory_context"
      else "attachment"
  | "assistant" ->
      if string_contains row.content "[Conversation history compacted]" then
        "compaction"
      else "other"
  | role -> role

let is_session_event_row (row : Memory.raw_message) =
  row.role = "event" || row.role = "system"
  || row.role = "assistant"
     && string_contains row.content "[Conversation history compacted]"

let string_or_null = function Some value -> `String value | None -> `Null

let session_show_system_prompt config =
  Prompt_builder.build ~config ~tool_registry:None ()

let session_show_active_workspace_file config filename =
  List.mem filename config.Runtime_config.prompt.workspace_files

let session_show_active_workspace_path config path =
  let workspace = Runtime_config.effective_workspace config in
  let resolved =
    if Filename.is_relative path then Filename.concat workspace path else path
  in
  let normalized = Path_util.normalize_path resolved in
  List.exists
    (fun filename ->
      Path_util.normalize_path (Filename.concat workspace filename) = normalized)
    config.Runtime_config.prompt.workspace_files

let session_show_shell_command_targets_active_workspace_file config command =
  let workspace = Runtime_config.effective_workspace config in
  List.exists
    (fun filename ->
      let abs_path =
        Path_util.normalize_path (Filename.concat workspace filename)
      in
      List.exists
        (fun needle ->
          let needle_len = String.length needle in
          needle_len > 0
          && String.length command >= needle_len
          &&
          let rec loop i =
            if i + needle_len > String.length command then false
            else if String.sub command i needle_len = needle then true
            else loop (i + 1)
          in
          loop 0)
        [ filename; Filename.basename filename; "./" ^ filename; abs_path ])
    config.Runtime_config.prompt.workspace_files

let redact_tool_call_arguments_for_session_show ~config ~function_name arguments
    =
  let open Yojson.Safe.Util in
  try
    let args = Yojson.Safe.from_string arguments in
    let redacted json = Some (Yojson.Safe.to_string json) in
    match function_name with
    | "doc_write" ->
        let filename = args |> member "filename" |> to_string in
        if session_show_active_workspace_file config filename then
          redacted
            (`Assoc
               [
                 ("filename", `String filename);
                 ("content", `String "[redacted]");
               ])
        else None
    | "file_write" | "file_append" ->
        let path = args |> member "path" |> to_string in
        if session_show_active_workspace_path config path then
          redacted
            (`Assoc
               [ ("path", `String path); ("content", `String "[redacted]") ])
        else None
    | "file_edit" ->
        let path = args |> member "path" |> to_string in
        if session_show_active_workspace_path config path then
          redacted
            (`Assoc
               [
                 ("path", `String path);
                 ("old_text", `String "[redacted]");
                 ("new_text", `String "[redacted]");
                 ( "replace_all",
                   match args |> member "replace_all" with
                   | `Bool b -> `Bool b
                   | _ -> `Bool false );
               ])
        else None
    | "file_edit_lines" ->
        let path = args |> member "path" |> to_string in
        if session_show_active_workspace_path config path then
          redacted
            (`Assoc
               [
                 ("path", `String path);
                 ( "start_line",
                   match args |> member "start_line" with
                   | `Int n -> `Int n
                   | _ -> `Null );
                 ( "end_line",
                   match args |> member "end_line" with
                   | `Int n -> `Int n
                   | _ -> `Null );
                 ("content", `String "[redacted]");
               ])
        else None
    | "shell_exec" ->
        let command = args |> member "command" |> to_string in
        if
          session_show_shell_command_targets_active_workspace_file config
            command
        then
          redacted
            (`Assoc
               [
                 ("command", `String "[redacted]");
                 ( "cwd",
                   match args |> member "cwd" with
                   | `String cwd -> `String cwd
                   | _ -> `Null );
               ])
        else None
    | _ -> None
  with _ -> None

let sanitize_tool_calls_json_for_session_show ~config = function
  | None -> None
  | Some tool_calls_json -> (
      try
        let json = Yojson.Safe.from_string tool_calls_json in
        let sanitized =
          match json with
          | `List calls ->
              `List
                (List.map
                   (function
                     | `Assoc fields as call ->
                         let function_name =
                           match List.assoc_opt "function_name" fields with
                           | Some (`String name) -> Some name
                           | _ -> None
                         in
                         let arguments =
                           match List.assoc_opt "arguments" fields with
                           | Some (`String args) -> Some args
                           | _ -> None
                         in
                         begin match (function_name, arguments) with
                         | Some name, Some args -> (
                             match
                               redact_tool_call_arguments_for_session_show
                                 ~config ~function_name:name args
                             with
                             | Some redacted_arguments ->
                                 `Assoc
                                   (("arguments", `String redacted_arguments)
                                   :: List.remove_assoc "arguments" fields)
                             | None -> call)
                         | _ -> call
                         end
                     | other -> other)
                   calls)
          | other -> other
        in
        Some (Yojson.Safe.to_string sanitized)
      with _ -> Some tool_calls_json)

let sanitize_provider_response_items_json_for_session_show ~config = function
  | None -> None
  | Some provider_response_items_json -> (
      try
        let json = Yojson.Safe.from_string provider_response_items_json in
        let sanitized =
          match json with
          | `List items ->
              `List
                (List.map
                   (function
                     | `Assoc fields as item ->
                         let item_type =
                           match List.assoc_opt "type" fields with
                           | Some (`String value) -> Some value
                           | _ -> None
                         in
                         let function_name =
                           match List.assoc_opt "name" fields with
                           | Some (`String value) -> Some value
                           | _ -> None
                         in
                         let arguments =
                           match List.assoc_opt "arguments" fields with
                           | Some (`String value) -> Some value
                           | _ -> None
                         in
                         begin match (item_type, function_name, arguments) with
                         | ( Some ("function_call" | "tool_call"),
                             Some name,
                             Some args ) -> (
                             match
                               redact_tool_call_arguments_for_session_show
                                 ~config ~function_name:name args
                             with
                             | Some redacted_arguments ->
                                 `Assoc
                                   (("arguments", `String redacted_arguments)
                                   :: List.remove_assoc "arguments" fields)
                             | None -> item)
                         | _ -> item
                         end
                     | other -> other)
                   items)
          | other -> other
        in
        Some (Yojson.Safe.to_string sanitized)
      with _ -> Some provider_response_items_json)

let raw_message_json config index (row : Memory.raw_message) =
  `Assoc
    [
      ("index", `Int index);
      ("id", `Int row.id);
      ("role", `String row.role);
      ("content", `String row.content);
      ("tool_call_id", string_or_null row.tool_call_id);
      ("tool_name", string_or_null row.tool_name);
      ( "tool_calls_json",
        string_or_null
          (sanitize_tool_calls_json_for_session_show ~config row.tool_calls_json)
      );
      ( "provider_response_items_json",
        string_or_null
          (sanitize_provider_response_items_json_for_session_show ~config
             row.provider_response_items_json) );
      ("created_at", `String row.created_at);
    ]

let cost_summary_columns =
  Table_format.
    [
      { header = "PERIOD"; align = Left; min_width = 12; flex = false };
      { header = "COST"; align = Right; min_width = 8; flex = false };
      { header = "TURNS"; align = Right; min_width = 5; flex = false };
      { header = "PROMPT"; align = Right; min_width = 6; flex = false };
      { header = "ADDED"; align = Right; min_width = 6; flex = false };
      { header = "COMPLETION"; align = Right; min_width = 6; flex = false };
    ]

let cost_summary_row label (s : Request_stats.summary) =
  [
    label;
    Printf.sprintf "$%.4f" s.total_cost_usd;
    string_of_int s.total_turns;
    Request_stats.format_tokens s.total_prompt_tokens;
    Request_stats.format_tokens s.total_added_prompt_tokens;
    Request_stats.format_tokens s.total_completion_tokens;
  ]

let summary_to_json label (s : Request_stats.summary) =
  `Assoc
    [
      ("period", `String label);
      ("cost_usd", `Float s.total_cost_usd);
      ("prompt_tokens", `Int s.total_prompt_tokens);
      ("completion_tokens", `Int s.total_completion_tokens);
      ("added_prompt_tokens", `Int s.total_added_prompt_tokens);
      ("turns", `Int s.total_turns);
    ]

let cmd_costs args =
  let db = get_db () in
  let json_mode = List.mem "--json" args in
  let args = List.filter (fun a -> a <> "--json") args in
  match args with
  | [] ->
      let today =
        Request_stats.summary_for_period ~db
          ~since:"datetime('now', 'start of day')"
      in
      let week =
        Request_stats.summary_for_period ~db ~since:"datetime('now', '-7 days')"
      in
      let month =
        Request_stats.summary_for_period ~db
          ~since:"datetime('now', '-30 days')"
      in
      let all = Request_stats.total_summary ~db in
      if json_mode then
        Yojson.Safe.pretty_to_string
          (`List
             [
               summary_to_json "today" today;
               summary_to_json "7_days" week;
               summary_to_json "30_days" month;
               summary_to_json "all_time" all;
             ])
      else if all.total_turns = 0 then "No cost data recorded yet."
      else
        let rows =
          [
            cost_summary_row "Today" today;
            cost_summary_row "Last 7 days" week;
            cost_summary_row "Last 30 days" month;
            cost_summary_row "All time" all;
          ]
        in
        "Cost Summary:\n" ^ Table_format.render cost_summary_columns rows
  | [ "session" ] ->
      let sessions = Request_stats.summary_by_session ~db in
      if json_mode then
        Yojson.Safe.pretty_to_string
          (`List
             (List.map
                (fun (ss : Request_stats.session_summary) ->
                  `Assoc
                    [
                      ("session_key", `String ss.session_key);
                      ("cost_usd", `Float ss.summary.total_cost_usd);
                      ("prompt_tokens", `Int ss.summary.total_prompt_tokens);
                      ( "completion_tokens",
                        `Int ss.summary.total_completion_tokens );
                      ( "added_prompt_tokens",
                        `Int ss.summary.total_added_prompt_tokens );
                      ("turns", `Int ss.summary.total_turns);
                      ("first_request", `String ss.first_request);
                      ("last_request", `String ss.last_request);
                    ])
                sessions))
      else if sessions = [] then "No cost data recorded yet."
      else
        let session_columns =
          Table_format.
            [
              { header = "SESSION"; align = Left; min_width = 10; flex = true };
              { header = "COST"; align = Right; min_width = 8; flex = false };
              { header = "TURNS"; align = Right; min_width = 5; flex = false };
              { header = "PROMPT"; align = Right; min_width = 6; flex = false };
              { header = "ADDED"; align = Right; min_width = 6; flex = false };
              {
                header = "COMPLETION";
                align = Right;
                min_width = 6;
                flex = false;
              };
            ]
        in
        let rows =
          List.map
            (fun (ss : Request_stats.session_summary) ->
              cost_summary_row ss.session_key ss.summary)
            sessions
        in
        "Session Costs:\n" ^ Table_format.render session_columns rows
  | [ "session"; key ] ->
      let s = Request_stats.summary_for_session ~db ~session_key:key in
      if json_mode then Yojson.Safe.pretty_to_string (summary_to_json key s)
      else if s.total_turns = 0 then
        Printf.sprintf "No cost data for session '%s'." key
      else
        let rows = [ cost_summary_row "Total" s ] in
        Printf.sprintf "Costs for %s:\n" key
        ^ Table_format.render cost_summary_columns rows
  | [ "model" ] ->
      let models = Request_stats.summary_by_model ~db in
      if json_mode then
        Yojson.Safe.pretty_to_string
          (`List
             (List.map
                (fun (ms : Request_stats.model_summary) ->
                  `Assoc
                    [
                      ("model", `String ms.model);
                      ("provider", `String ms.provider);
                      ("cost_usd", `Float ms.summary.total_cost_usd);
                      ("prompt_tokens", `Int ms.summary.total_prompt_tokens);
                      ( "completion_tokens",
                        `Int ms.summary.total_completion_tokens );
                      ("turns", `Int ms.summary.total_turns);
                    ])
                models))
      else if models = [] then "No cost data recorded yet."
      else
        let model_columns =
          Table_format.
            [
              { header = "MODEL"; align = Left; min_width = 15; flex = true };
              { header = "COST"; align = Right; min_width = 8; flex = false };
              { header = "TURNS"; align = Right; min_width = 5; flex = false };
              { header = "PROMPT"; align = Right; min_width = 6; flex = false };
              {
                header = "COMPLETION";
                align = Right;
                min_width = 6;
                flex = false;
              };
            ]
        in
        let rows =
          List.map
            (fun (ms : Request_stats.model_summary) ->
              [
                ms.provider ^ ":" ^ ms.model;
                Printf.sprintf "$%.4f" ms.summary.total_cost_usd;
                string_of_int ms.summary.total_turns;
                Request_stats.format_tokens ms.summary.total_prompt_tokens;
                Request_stats.format_tokens ms.summary.total_completion_tokens;
              ])
            models
        in
        "Model Costs:\n" ^ Table_format.render model_columns rows
  | [ "provider" ] ->
      let providers = Request_stats.summary_by_provider ~db in
      if json_mode then
        Yojson.Safe.pretty_to_string
          (`List
             (List.map
                (fun (prov, s) ->
                  `Assoc
                    [
                      ("provider", `String prov);
                      ("cost_usd", `Float s.Request_stats.total_cost_usd);
                      ("prompt_tokens", `Int s.total_prompt_tokens);
                      ("completion_tokens", `Int s.total_completion_tokens);
                      ("turns", `Int s.total_turns);
                    ])
                providers))
      else if providers = [] then "No cost data recorded yet."
      else
        let provider_columns =
          Table_format.
            [
              {
                header = "PROVIDER";
                align = Left;
                min_width = 10;
                flex = false;
              };
              { header = "COST"; align = Right; min_width = 8; flex = false };
              { header = "TURNS"; align = Right; min_width = 5; flex = false };
              { header = "PROMPT"; align = Right; min_width = 6; flex = false };
              {
                header = "COMPLETION";
                align = Right;
                min_width = 6;
                flex = false;
              };
            ]
        in
        let rows =
          List.map
            (fun (prov, (s : Request_stats.summary)) ->
              [
                prov;
                Printf.sprintf "$%.4f" s.total_cost_usd;
                string_of_int s.total_turns;
                Request_stats.format_tokens s.total_prompt_tokens;
                Request_stats.format_tokens s.total_completion_tokens;
              ])
            providers
        in
        "Provider Costs:\n" ^ Table_format.render provider_columns rows
  | _ ->
      "Usage: clawq costs [subcommand] [--json]\n\n\
       Subcommands:\n\
      \  (default)       Cost summary by time period\n\
      \  session [KEY]   Per-session cost breakdown\n\
      \  model           Per-model cost breakdown\n\
      \  provider        Per-provider cost breakdown"

let session_epoch_json (epoch : Memory.session_epoch) =
  `Assoc
    [
      ( "epoch",
        match epoch.epoch_id with
        | Some id -> `Int id
        | None -> `String epoch.label );
      ("label", `String epoch.label);
      ("current", `Bool epoch.current);
      ("message_count", `Int epoch.message_count);
      ("first_message_at", string_or_null epoch.first_message_at);
      ("last_message_at", string_or_null epoch.last_message_at);
      ("recorded_at", string_or_null epoch.recorded_at);
    ]

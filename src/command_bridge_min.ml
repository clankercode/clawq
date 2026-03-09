let get_config () = Config_loader.load ()

let redact_key s =
  let len = String.length s in
  if len <= 8 then String.make len '*'
  else String.sub s 0 4 ^ "..." ^ String.sub s (len - 4) 4

let cmd_status () =
  let cfg = get_config () in
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add "clawq-min status";
  add (Printf.sprintf "  model: %s" cfg.agent_defaults.primary_model);
  add (Printf.sprintf "  temperature: %.2f" cfg.default_temperature);
  add
    (Printf.sprintf "  cli channel: %s"
       (if cfg.channels.cli then "enabled" else "disabled"));
  add (Printf.sprintf "  memory backend: %s" cfg.memory.backend);
  add (Printf.sprintf "  providers: %d configured" (List.length cfg.providers));
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
          (Printf.sprintf "WARNING: Provider '%s' has no configured auth" name))
    cfg.providers;
  (match cfg.default_provider with
  | Some name when not (List.exists (fun (n, _) -> n = name) cfg.providers) ->
      add
        (Printf.sprintf "WARNING: default_provider '%s' not found in providers"
           name)
  | Some name -> (
      match List.assoc_opt name cfg.providers with
      | Some p when not (Runtime_config.provider_has_auth p) ->
          add
            (Printf.sprintf
               "WARNING: default_provider '%s' has no configured auth" name)
      | _ -> ())
  | None -> ());
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
  match List.rev !issues with
  | [] -> "doctor: all checks passed"
  | issues -> "doctor: issues found\n" ^ String.concat "\n" issues

let cmd_onboard () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let config_dir = Filename.concat home ".clawq" in
  let config_path = Filename.concat config_dir "config.json" in
  if Sys.file_exists config_path then
    "Config already exists at " ^ config_path
    ^ "\nRun 'clawq-min config wizard' to reconfigure, or edit directly."
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
  "default_provider": "openrouter",
  "providers": {
    "openrouter": {
      "api_key": "YOUR_API_KEY_HERE",
      "base_url": "https://openrouter.ai/api/v1"
    }
  },
  "agent_defaults": {
    "primary_model": "openai/gpt-4o"
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
    ^ "\nEdit it to add your API keys."
  end

let cmd_config args =
  match args with
  | [ "wizard" ] ->
      Config_wizard_tui.run_wizard Config_wizard_model.Onboard;
      ""
  | "set" :: key :: value :: _ -> Config_set.set_value key value
  | [ "get"; key ] -> Config_set.get_value key
  | "show" :: rest -> Config_show.show (List.nth_opt rest 0)
  | _ ->
      "Usage: clawq-min config <subcommand>\n\n\
       Subcommands:\n\
      \  wizard           Interactive configuration wizard\n\
      \  set KEY VALUE    Set a config value by dot-path\n\
      \  get KEY          Get a config value by dot-path\n\
      \  show [SECTION]   Display current config (secrets redacted)"

let cmd_models () =
  let cfg = get_config () in
  match cfg.providers with
  | [] -> "No providers configured. Run 'clawq-min onboard' to set up."
  | providers ->
      let lines =
        List.map
          (fun (name, (p : Runtime_config.provider_config)) ->
            let url =
              match p.base_url with Some u -> u | None -> "(default)"
            in
            let model_info =
              match p.default_model with
              | Some m -> Printf.sprintf " model: %s" m
              | None -> ""
            in
            Printf.sprintf "  %s: %s (key: %s)%s" name url
              (if Runtime_config.provider_has_auth p then "configured"
               else "not set")
              model_info)
          providers
      in
      "Configured providers:\n" ^ String.concat "\n" lines
      ^ Printf.sprintf "\nDefault model: %s" cfg.agent_defaults.primary_model
      ^ Printf.sprintf "\nDefault provider: %s"
          (match cfg.default_provider with Some p -> p | None -> "(auto)")

let cmd_channel () =
  let cfg = get_config () in
  Printf.sprintf "Configured channels:\n  cli: %s"
    (if cfg.channels.cli then "enabled" else "disabled")

let cmd_memory () =
  let cfg = get_config () in
  Printf.sprintf "Memory backend: %s\nSearch enabled: %b" cfg.memory.backend
    cfg.memory.search_enabled

let cmd_workspace () =
  let cfg = get_config () in
  Printf.sprintf "Workspace: %s" (Runtime_config.effective_workspace cfg)

let cmd_capabilities () =
  let cfg = get_config () in
  let caps = ref [] in
  let add s = caps := s :: !caps in
  let active_providers =
    List.filter (fun (_, p) -> Runtime_config.provider_has_auth p) cfg.providers
  in
  add
    (Printf.sprintf "  - LLM chat: %d provider(s) configured (%s)"
       (List.length active_providers)
       (if active_providers = [] then "none active"
        else String.concat ", " (List.map fst active_providers)));
  if cfg.channels.cli then add "  - CLI channel: enabled";
  add
    (Printf.sprintf "  - Memory: %s (FTS search: %s)" cfg.memory.backend
       (if cfg.memory.search_enabled then "enabled" else "disabled"));
  if cfg.security.tools_enabled then begin
    let registry = Tool_registry.create () in
    let ws = Runtime_config.effective_workspace cfg in
    let backend = Sandbox.backend_of_policy cfg.security.sandbox_backend in
    let sandbox =
      Sandbox.create ~backend ~workspace:ws
        ~extra_allowed_paths:cfg.security.extra_allowed_paths
        ~workspace_only:cfg.security.workspace_only ()
    in
    Tools_builtin.register_all ~config:cfg ~sandbox registry;
    let skills =
      Skills.load_all ~workspace_only:cfg.security.workspace_only
        ~allowed_commands:Tools_builtin.default_shell_allowlist ()
    in
    List.iter (fun s -> Tool_registry.register registry s) skills;
    let tool_names = List.map (fun (t : Tool.t) -> t.name) registry.tools in
    add
      (Printf.sprintf "  - Tools: %d registered (%s)" (List.length tool_names)
         (String.concat ", " tool_names))
  end
  else add "  - Tools: disabled";
  add "  - Service/runtime/tunnel/MCP integrations: disabled in minimal build";
  "Available capabilities:\n" ^ String.concat "\n" (List.rev !caps)

let cmd_auth args =
  match args with
  | [ "codex-login" ]
  | [ "status"; "codex" ]
  | [ "logout"; "codex" ]
  | [ "codex-status" ]
  | [ "codex-logout" ]
  | [ "codex-login"; _ ]
  | [ "codex-status"; _ ]
  | [ "codex-logout"; _ ] ->
      "Codex OAuth auth commands are disabled in minimal build. Use full \
       'clawq' binary."
  | [ "encrypt" ] ->
      if not (get_config ()).security.encrypt_secrets then
        "Secret encryption is disabled. Set security.encrypt_secrets to true \
         in config."
      else begin
        match Secret_store.get_master_key () with
        | Error msg -> Printf.sprintf "Error: %s" msg
        | Ok key -> (
            let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
            let config_path =
              Filename.concat (Filename.concat home ".clawq") "config.json"
            in
            if not (Sys.file_exists config_path) then
              "No config file found at " ^ config_path
            else
              let json =
                try Ok (Yojson.Safe.from_file config_path)
                with exn -> Error exn
              in
              match json with
              | Error exn ->
                  Printf.sprintf "Failed to read config: %s"
                    (Printexc.to_string exn)
              | Ok json -> (
                  match Secret_store.encrypt_config_secrets ~key json with
                  | Error msg -> Printf.sprintf "Error: %s" msg
                  | Ok new_json -> (
                      try
                        let s =
                          Yojson.Safe.pretty_to_string ~std:true new_json
                        in
                        let oc = open_out config_path in
                        output_string oc s;
                        output_char oc '\n';
                        close_out oc;
                        "Secrets encrypted in " ^ config_path
                      with exn ->
                        Printf.sprintf "Failed to write config: %s"
                          (Printexc.to_string exn))))
      end
  | _ -> (
      let cfg = get_config () in
      match cfg.providers with
      | [] -> "No providers configured. No provider auth set."
      | providers ->
          let lines =
            List.map
              (fun (name, (p : Runtime_config.provider_config)) ->
                Printf.sprintf "  %s: %s" name
                  (if Runtime_config.is_key_set p.api_key then
                     redact_key p.api_key
                   else if Runtime_config.provider_has_codex_oauth p then
                     "codex-oauth configured"
                   else "not set"))
              providers
          in
          "Provider auth status:\n" ^ String.concat "\n" lines)

let get_db () =
  let cfg = get_config () in
  let db_path =
    if cfg.memory.db_path <> "" then cfg.memory.db_path
    else
      let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
      Filename.concat (Filename.concat home ".clawq") "memory.db"
  in
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let clawq_dir = Filename.concat home ".clawq" in
  (try if not (Sys.file_exists clawq_dir) then Sys.mkdir clawq_dir 0o755
   with _ -> ());
  Memory.init ~db_path ~search_enabled:cfg.memory.search_enabled ()

let cmd_cron args =
  match args with
  | [ "list" ] | [] ->
      let db = get_db () in
      Scheduler.init_schema db;
      let jobs = Scheduler.list_jobs ~db in
      if jobs = [] then "No cron jobs configured."
      else
        let header =
          Printf.sprintf "  %-20s %-15s %-20s %s" "NAME" "SESSION" "SCHEDULE"
            "ENABLED"
        in
        let rows =
          List.map
            (fun (j : Scheduler.job) ->
              Printf.sprintf "  %-20s %-15s %-20s %s" j.name j.session_key
                j.schedule_str
                (if j.enabled then "yes" else "no"))
            jobs
        in
        "Cron jobs:\n" ^ header ^ "\n" ^ String.concat "\n" rows
  | "add" :: name :: session_key :: schedule :: message -> (
      let db = get_db () in
      Scheduler.init_schema db;
      let msg = String.concat " " message in
      match Scheduler.add_job ~db ~name ~session_key ~message:msg ~schedule with
      | Ok () -> Printf.sprintf "Added cron job '%s'" name
      | Error e -> Printf.sprintf "Error: %s" e)
  | [ "remove"; name ] ->
      let db = get_db () in
      Scheduler.init_schema db;
      if Scheduler.remove_job ~db ~name then
        Printf.sprintf "Removed job '%s'" name
      else Printf.sprintf "No job found with name '%s'" name
  | _ -> "Usage: clawq-min cron <list|add|remove>"

let cmd_background _args =
  "Background task execution is disabled in the minimal build. Use the full \
   clawq binary."

let cmd_delegate _args =
  "Background task delegation is disabled in the minimal build. Use the full \
   clawq binary."

let cmd_audit args =
  let cfg = get_config () in
  if not cfg.security.audit_enabled then
    "Audit trail is disabled. Set security.audit_enabled to true in config."
  else
    let db = get_db () in
    Audit.init_schema db;
    match args with
    | [ "list" ] | [] ->
        let rows = Audit.query ~db ~limit:20 () in
        if rows = [] then "No audit log entries."
        else
          let lines =
            List.map
              (fun (r : Audit.row) ->
                Printf.sprintf "  %-5d %-20s %-18s %-10s %s" r.id r.timestamp
                  r.event_type
                  (match r.tool_name with Some n -> n | None -> "")
                  (match r.details with
                  | Some d when String.length d > 50 ->
                      String.sub d 0 50 ^ "..."
                  | Some d -> d
                  | None -> ""))
              rows
          in
          "Audit log:\n" ^ String.concat "\n" lines
    | _ -> "Usage: clawq-min audit <list>"

let cmd_skills args =
  match args with
  | [ "list" ] | [] ->
      let files = Skills.list_skills () in
      if files = [] then "No skills found in " ^ Skills.skills_dir ()
      else "Skills:\n" ^ String.concat "\n" (List.map (fun f -> "  " ^ f) files)
  | [ "path" ] -> "Skills directory: " ^ Skills.skills_dir ()
  | [ "init" ] -> Skills.create_example ()
  | _ -> "Usage: clawq-min skills <list|path|init>"

let unsupported cmd =
  Printf.sprintf
    "%s is disabled in minimal build. Use full 'clawq' binary for \
     server/network integrations."
    cmd

let handle args =
  match args with
  | "phase2" :: _ -> Phase2.render ()
  | "status" :: _ -> cmd_status ()
  | "config" :: rest -> cmd_config rest
  | "doctor" :: _ -> cmd_doctor ()
  | "onboard" :: _ -> cmd_onboard ()
  | "models" :: _ -> cmd_models ()
  | "channel" :: _ -> cmd_channel ()
  | "memory" :: _ -> cmd_memory ()
  | "workspace" :: _ -> cmd_workspace ()
  | "capabilities" :: _ -> cmd_capabilities ()
  | "auth" :: rest -> cmd_auth rest
  | "cron" :: rest -> cmd_cron rest
  | "background" :: rest -> cmd_background rest
  | "delegate" :: rest -> cmd_delegate rest
  | "skills" :: rest -> cmd_skills rest
  | "audit" :: rest -> cmd_audit rest
  | "session" :: _ -> unsupported "session"
  | "update" :: _ -> unsupported "update"
  | "otp-show" :: _ -> unsupported "otp-show"
  | "agent" :: _ -> unsupported "agent"
  | "transcribe" :: _ -> unsupported "transcribe"
  | "mcp" :: _ -> unsupported "mcp"
  | "runtime" :: _ -> unsupported "runtime"
  | "tunnel" :: _ -> unsupported "tunnel"
  | "service" :: _ -> unsupported "service"
  | "hardware" :: _ -> "hardware: deferred to Phase 2"
  | "benchmark" :: rest -> Benchmark.run rest
  | "migrate" :: rest -> Migrate.cmd_migrate rest
  | _ -> Clawq_core.dispatch args

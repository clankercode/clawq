let get_config () = Config_loader.load ()
let redact_key = Tui_input.redact

let cmd_status () =
  let cfg = get_config () in
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add "clawq-min status";
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
  let config_dir = Dot_dir.path () in
  let config_path = Dot_dir.config_path () in
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
    ^ "\nEdit it to add your API keys."
  end

let cmd_config args =
  match args with
  | [ "wizard" ] ->
      Config_wizard_tui.run_wizard Config_wizard_model.Onboard;
      ""
  | "set" :: key :: value :: _ ->
      let result = Config_set.set_value key value in
      if Config_set.is_secret_path key then
        Printf.sprintf "Set %s = %s" key (redact_key value)
      else result
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
      "Usage: clawq-min config <subcommand>\n\n\
       Subcommands:\n\
      \  wizard           Interactive configuration wizard\n\
      \  set KEY VALUE    Set a config value by dot-path\n\
      \  set KEY          Prompt for value (secret keys only, hidden input)\n\
      \  get KEY          Get a config value by dot-path (secrets redacted)\n\
      \  show [SECTION]   Display current config (secrets redacted)\n\
      \  tree [SECTION]   Tree view of config (secrets redacted)\n\
      \  tree keys        Tree view, structure only (no values)\n\
      \  search QUERY     Search config keys matching QUERY"

let cmd_models args =
  let parse_availability value =
    match Models_catalog.availability_filter_of_string value with
    | Some availability -> Ok availability
    | None ->
        Error
          (Printf.sprintf
             "Error: invalid availability filter '%s'. Use available, \
              unavailable, or all."
             value)
  in
  let parse_list_args args =
    let rec loop provider_filter availability = function
      | [] -> Ok (provider_filter, availability)
      | "--provider" :: p :: rest -> loop (Some p) availability rest
      | "--availability" :: value :: rest -> (
          match parse_availability value with
          | Ok availability -> loop provider_filter availability rest
          | Error _ as err -> err)
      | "--available" :: rest ->
          loop provider_filter Models_catalog.Available rest
      | "--unavailable" :: rest ->
          loop provider_filter Models_catalog.Unavailable rest
      | "--all" :: rest -> loop provider_filter Models_catalog.All rest
      | "--availability" :: [] ->
          Error
            "Error: --availability requires one of: available, unavailable, \
             all."
      | bad :: _ ->
          Error
            (Printf.sprintf
               "Error: unrecognized models list option '%s'. Usage: clawq-min \
                models list [--provider P] [--availability \
                available|unavailable|all]"
               bad)
    in
    loop None Models_catalog.Available args
  in
  match args with
  | [] | [ "list" ] ->
      let provider_filter = None in
      Models_catalog.to_plain_list ~provider_filter ()
  | "list" :: rest -> (
      match parse_list_args rest with
      | Ok (provider_filter, availability) ->
          Models_catalog.to_plain_list ~provider_filter ~availability ()
      | Error msg -> msg)
  | [ "set-default"; model ] -> (
      match Models_catalog.find_by_full_name model with
      | Some _ -> Config_set.set_value "agent_defaults.primary_model" model
      | None ->
          Printf.sprintf "Warning: model '%s' not found in catalog.\n%s" model
            (Config_set.set_value "agent_defaults.primary_model" model))
  | _ ->
      "Usage: clawq-min models <subcommand>\n\n\
       Subcommands:\n\
      \  list [--provider P] [--availability available|unavailable|all]\n\
      \  set-default MODEL       Set default model"

let cmd_usage _ =
  "Usage is not available in the minimal build. Use the full clawq binary for \
   this feature."

let cmd_active _ =
  "Active is not available in the minimal build. Use the full clawq binary for \
   this feature."

let cmd_channel args =
  let cfg = get_config () in
  match args with
  | [ "set-model"; name; model ] -> (
      let valid = Runtime_config.all_channel_types in
      if not (List.mem name valid) then
        Printf.sprintf "Error: unknown channel '%s'. Valid channels: %s" name
          (String.concat ", " valid)
      else
        let pf = Pmodel.parse_flexible model in
        let key = Printf.sprintf "channels.%s.default_model" name in
        match Config_set.set_json_value key (`String model) with
        | Error e -> e
        | Ok () -> (
            let base = Printf.sprintf "Set %s default_model = %s" name model in
            match Pmodel.deprecation_warning pf with
            | Some warn -> base ^ "\n" ^ warn
            | None -> base))
  | [ "show-model"; name ] -> (
      let valid = Runtime_config.all_channel_types in
      if not (List.mem name valid) then
        Printf.sprintf "Error: unknown channel '%s'. Valid channels: %s" name
          (String.concat ", " valid)
      else
        let channel_model =
          Runtime_config.channel_default_model cfg ~channel_type:name
        in
        let global_model = cfg.agent_defaults.primary_model in
        match channel_model with
        | Some m ->
            Printf.sprintf "%s default_model: %s (global: %s)" name m
              global_model
        | None ->
            Printf.sprintf "%s default_model: (not set, inherits global: %s)"
              name global_model)
  | [ "clear-model"; name ] -> (
      let valid = Runtime_config.all_channel_types in
      if not (List.mem name valid) then
        Printf.sprintf "Error: unknown channel '%s'. Valid channels: %s" name
          (String.concat ", " valid)
      else
        let key = Printf.sprintf "channels.%s.default_model" name in
        match Config_set.set_json_value key `Null with
        | Error e -> e
        | Ok () ->
            Printf.sprintf "Cleared %s default_model (now inherits global)" name
      )
  | _ ->
      Printf.sprintf "Configured channels:\n  cli: %s"
        (if cfg.channels.cli then "enabled" else "disabled")

let cmd_memory () =
  let cfg = get_config () in
  Printf.sprintf "Memory backend: %s\nSearch enabled: %b" cfg.memory.backend
    cfg.memory.search_enabled

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
      "Codex OAuth is not available in the minimal build. Use the full clawq \
       binary for this feature."
  | [ "encrypt" ] ->
      if not (get_config ()).security.encrypt_secrets then
        "Secret encryption is disabled. Set security.encrypt_secrets to true \
         in config."
      else begin
        match Secret_store.get_master_key () with
        | Error msg -> Printf.sprintf "Error: %s" msg
        | Ok key -> (
            let config_path = Dot_dir.config_path () in
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
                        Fun.protect
                          ~finally:(fun () -> close_out oc)
                          (fun () ->
                            output_string oc s;
                            output_char oc '\n');
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
    if cfg.memory.db_path <> "" then cfg.memory.db_path else Dot_dir.db_path ()
  in
  let clawq_dir = Dot_dir.path () in
  (try if not (Sys.file_exists clawq_dir) then Sys.mkdir clawq_dir 0o755
   with _ -> ());
  Memory.init ~db_path ~search_enabled:cfg.memory.search_enabled ()

let cmd_cron args =
  match args with
  | "list" :: flags | ([] as flags) ->
      let show_prompt = List.mem "--prompt" flags || List.mem "-p" flags in
      let db = get_db () in
      Scheduler.init_schema db;
      let jobs = Scheduler.list_jobs ~db in
      if jobs = [] then
        "No cron jobs configured. Use 'clawq cron add' to create one."
      else
        let columns =
          let base =
            [
              Table_format.
                { header = "NAME"; align = Left; min_width = 4; flex = false };
              { header = "SESSION"; align = Left; min_width = 7; flex = false };
              { header = "SCHEDULE"; align = Left; min_width = 8; flex = false };
              { header = "ENABLED"; align = Left; min_width = 3; flex = false };
            ]
          in
          if show_prompt then
            base
            @ [
                Table_format.
                  {
                    header = "PROMPT";
                    align = Left;
                    min_width = 10;
                    flex = true;
                  };
              ]
          else base
        in
        let rows =
          List.map
            (fun (j : Scheduler.job) ->
              let base =
                [
                  j.name;
                  j.session_key;
                  j.schedule_str;
                  (if j.enabled then "yes" else "no");
                ]
              in
              if show_prompt then base @ [ j.message ] else base)
            jobs
        in
        "Cron jobs:\n" ^ Table_format.render columns rows
  | "add" :: name :: session_key :: schedule :: message -> (
      let db = get_db () in
      Scheduler.init_schema db;
      let rec extract_ttl acc = function
        | "--ttl" :: v :: rest -> (Some v, List.rev_append acc rest)
        | x :: rest -> extract_ttl (x :: acc) rest
        | [] -> (None, List.rev acc)
      in
      let ttl, message = extract_ttl [] message in
      let msg = String.concat " " message in
      match
        Scheduler.add_job ~db ~name ~session_key ~message:msg ~schedule ?ttl ()
      with
      | Ok () -> Printf.sprintf "Added cron job '%s'" name
      | Error e -> Printf.sprintf "Error: %s" e)
  | [ "remove"; name ] ->
      let db = get_db () in
      Scheduler.init_schema db;
      if Scheduler.remove_job ~db ~name then
        Printf.sprintf "Removed job '%s'" name
      else Printf.sprintf "No job found with name '%s'" name
  | [ "trigger"; name ] | [ "run"; name ] -> (
      let db = get_db () in
      Scheduler.init_schema db;
      Background_task.init_schema db;
      match Scheduler.trigger_job ~db ~name with
      | Ok task_id ->
          Printf.sprintf
            "Triggered cron job '%s' — enqueued as background task %d.\n\
             Use 'clawq background show %d' to check progress."
            name task_id task_id
      | Error e -> Printf.sprintf "Error: %s" e)
  | _ ->
      "Usage: clawq-min cron <list|add|remove|trigger>\n\
      \  cron add <name> <session> <schedule> <msg> [--ttl <duration>]\n\
      \  cron trigger <name>                          - Trigger a job \
       immediately\n\
       Schedule format: \"every 5m\" (interval) or standard 5-field cron (e.g. \
       \"0 9 * * 1-5\" for weekdays at 9am)\n\
       TTL duration: e.g. 24h, 7d, 30m (job auto-disables after this time)"

let cmd_background _args =
  "Background tasks are not available in the minimal build. Use the full clawq \
   binary for this feature."

let cmd_delegate _args =
  "Delegation is not available in the minimal build. Use the full clawq binary \
   for this feature."

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
        if rows = [] then
          "No audit log entries. Entries are created when tools are invoked or \
           security events occur."
        else
          let columns =
            Table_format.
              [
                { header = "ID"; align = Right; min_width = 2; flex = false };
                {
                  header = "TIMESTAMP";
                  align = Left;
                  min_width = 19;
                  flex = false;
                };
                { header = "EVENT"; align = Left; min_width = 5; flex = false };
                { header = "TOOL"; align = Left; min_width = 4; flex = false };
                {
                  header = "DETAILS";
                  align = Left;
                  min_width = 10;
                  flex = true;
                };
              ]
          in
          let tbl_rows =
            List.map
              (fun (r : Audit.row) ->
                [
                  string_of_int r.id;
                  r.timestamp;
                  r.event_type;
                  (match r.tool_name with Some n -> n | None -> "");
                  (match r.details with
                  | Some d when String.length d > 50 ->
                      String.sub d 0 50 ^ "..."
                  | Some d -> d
                  | None -> "");
                ])
              rows
          in
          "Audit log:\n" ^ Table_format.render columns tbl_rows
    | _ -> "Usage: clawq-min audit <list>"

let cmd_skills args =
  Command_bridge_shared.cmd_skills ~prog_name:"clawq-min" args

let cmd_manifest = Command_bridge_shared.cmd_manifest

let unsupported cmd =
  Printf.sprintf
    "%s is not available in the minimal build. Use the full clawq binary for \
     this feature."
    cmd

let handle args =
  match args with
  | "phase2" :: _ -> Phase2.render ()
  | "status" :: _ -> cmd_status ()
  | "config" :: rest -> cmd_config rest
  | "doctor" :: _ -> cmd_doctor ()
  | "onboard" :: _ -> cmd_onboard ()
  | "models" :: rest -> cmd_models rest
  | "usage" :: rest -> cmd_usage rest
  | "active" :: rest -> cmd_active rest
  | "channel" :: rest -> cmd_channel rest
  | "memory" :: _ -> cmd_memory ()
  | "workspace" :: rest -> cmd_workspace rest
  | "capabilities" :: _ -> cmd_capabilities ()
  | "auth" :: rest -> cmd_auth rest
  | "cron" :: rest -> cmd_cron rest
  | "background" :: rest -> cmd_background rest
  | "subagents" :: rest -> cmd_background rest
  | "delegate" :: rest -> cmd_delegate rest
  | "skills" :: rest -> cmd_skills rest
  | "audit" :: rest -> cmd_audit rest
  | "held-items" :: _ -> unsupported "held-items"
  | "rig" :: _ | "rigging" :: _ -> unsupported "rig"
  | "costs" :: _ -> unsupported "costs"
  | "session" :: _ -> unsupported "session"
  | "update" :: _ -> unsupported "update"
  | "otp-show" :: _ -> unsupported "otp-show"
  | "agent" :: _ -> unsupported "agent"
  | "transcribe" :: _ -> unsupported "transcribe"
  | "mcp" :: _ -> unsupported "mcp"
  | "runner" :: _ -> unsupported "runner"
  | "runtime" :: _ -> unsupported "runtime"
  | "tunnel" :: _ -> unsupported "tunnel"
  | "service" :: _ -> unsupported "service"
  | "pair" :: _ -> unsupported "pair"
  | "setup" :: _ -> unsupported "setup"
  | "watcher" :: _ -> unsupported "watcher"
  | "ec-run" :: _ -> unsupported "ec-run"
  | "debate" :: _ -> unsupported "debate"
  | "pipeline" :: _ -> unsupported "pipeline"
  | "manifest" :: rest -> cmd_manifest rest
  | "hardware" :: _ -> "hardware: deferred to Phase 2"
  | "benchmark" :: rest -> Benchmark.run rest
  | "migrate" :: rest -> Migrate.cmd_migrate rest
  | "completions" :: rest -> Completions.cmd_completions rest
  | _ -> Clawq_core.dispatch args

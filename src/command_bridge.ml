let config = lazy (Config_loader.load ())

let get_config () = Lazy.force config

let read_daemon_state () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let path = Filename.concat (Filename.concat home ".clawq") "daemon_state.json" in
  if Sys.file_exists path then
    try
      let json = Yojson.Safe.from_file path in
      Some json
    with _ -> None
  else None

let redact_key s =
  let len = String.length s in
  if len <= 8 then String.make len '*'
  else String.sub s 0 4 ^ "..." ^ String.sub s (len - 4) 4

let doctor_issues (cfg : Runtime_config.t) =
  let issues = ref [] in
  let add s = issues := s :: !issues in
  let workspace = Runtime_config.effective_workspace cfg in
  if not (Sys.file_exists workspace) then
    add (Printf.sprintf "WARNING: workspace path does not exist: %s" workspace);
  let ego_path = Filename.concat workspace "EGO.md" in
  let soul_path = Filename.concat workspace "SOUL.md" in
  if Sys.file_exists soul_path && not (Sys.file_exists ego_path) then
    add "WARNING: SOUL.md exists without EGO.md; migrate persona file to EGO.md";
  if Sys.file_exists soul_path && Sys.file_exists ego_path then
    add "WARNING: both EGO.md and SOUL.md exist; EGO.md is preferred";
  if cfg.providers = [] then add "WARNING: No providers configured";
  List.iter
    (fun (name, (p : Runtime_config.provider_config)) ->
      if not (Runtime_config.is_key_set p.api_key) then
        add (Printf.sprintf "WARNING: Provider '%s' has no API key" name))
    cfg.providers;
  (match cfg.default_provider with
  | Some name when not (List.exists (fun (n, _) -> n = name) cfg.providers) ->
    add (Printf.sprintf "WARNING: default_provider '%s' not found in providers" name)
  | Some name ->
    (match List.assoc_opt name cfg.providers with
     | Some p when not (Runtime_config.is_key_set p.api_key) ->
       add (Printf.sprintf "WARNING: default_provider '%s' has no API key" name)
     | _ -> ())
  | None -> ());
  let primary_target = Runtime_config.effective_primary_target cfg.agent_defaults in
  (match primary_target.provider with
  | None -> ()
  | Some provider_name ->
    (match List.assoc_opt provider_name cfg.providers with
     | None ->
       add
         (Printf.sprintf
            "WARNING: model_priority[0] selects provider '%s' for model '%s' but provider is not configured"
            provider_name primary_target.model)
     | Some p when not (Runtime_config.is_key_set p.api_key) ->
       add
         (Printf.sprintf
            "WARNING: model_priority[0] selects provider '%s' for model '%s' but provider has no API key"
            provider_name primary_target.model)
     | Some _ -> ()));
  if cfg.agent_defaults.primary_model
     <> Runtime_config.effective_primary_model cfg.agent_defaults
  then
    add
      (Printf.sprintf
         "WARNING: primary_model '%s' differs from model_priority[0] '%s'; model_priority is used"
         cfg.agent_defaults.primary_model
         (Runtime_config.effective_primary_model cfg.agent_defaults));
  (match cfg.channels.telegram with
  | None -> ()
  | Some tg ->
    List.iter
      (fun (name, (acct : Runtime_config.telegram_account)) ->
        if acct.bot_token = "" then
          add
            (Printf.sprintf "WARNING: Telegram account '%s' has empty bot_token"
               name);
        if acct.allow_from = [] then
          add
            (Printf.sprintf
               "WARNING: Telegram account '%s' has no allow_from entries" name))
      tg.accounts);
  if cfg.security.encrypt_secrets then
    List.iter (fun (name, (p : Runtime_config.provider_config)) ->
      if Runtime_config.is_key_set p.api_key
         && String.length p.api_key > 0
         && p.api_key.[0] <> '$' then
        add (Printf.sprintf
               "WARNING: Provider '%s' has plaintext API key but encrypt_secrets is enabled. \
                Use \"$ENV_VAR\" syntax to reference environment variables." name))
      cfg.providers;
  List.rev !issues

let cmd_status () =
  let cfg = get_config () in
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add "clawq status";
  add
    (Printf.sprintf "  model: %s"
       (Runtime_config.effective_primary_model cfg.agent_defaults));
  add
    (Printf.sprintf "  temperature: %.2f" cfg.default_temperature);
  add (Printf.sprintf "  gateway: %s:%d" cfg.gateway.host cfg.gateway.port);
  add
    (Printf.sprintf "  cli channel: %s"
       (if cfg.channels.cli then "enabled" else "disabled"));
  add
    (Printf.sprintf "  telegram: %s"
       (match cfg.channels.telegram with
       | None -> "not configured"
       | Some tg ->
         Printf.sprintf "%d account(s)" (List.length tg.accounts)));
  add (Printf.sprintf "  memory backend: %s" cfg.memory.backend);
  add
    (Printf.sprintf "  providers: %d configured"
       (List.length cfg.providers));
  (match read_daemon_state () with
  | None -> add "  daemon: not running"
  | Some json ->
    let open Yojson.Safe.Util in
    (try
       let pid = json |> member "pid" |> to_int in
       add (Printf.sprintf "  daemon: running (pid %d)" pid)
     with _ -> add "  daemon: state file found"));
  List.rev !lines |> String.concat "\n"

let cmd_doctor () =
  let cfg = get_config () in
  let issues = doctor_issues cfg in
  let result =
    match issues with
    | [] -> "doctor: all checks passed"
    | issues -> "doctor: issues found\n" ^ String.concat "\n" issues
  in
  result

let cmd_onboard () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let config_dir = Filename.concat home ".clawq" in
  let config_path = Filename.concat config_dir "config.json" in
  if Sys.file_exists config_path then
    "Config already exists at " ^ config_path
    ^ "\nEdit it directly or delete to re-onboard."
  else begin
    (try
       if not (Sys.file_exists config_dir) then Sys.mkdir config_dir 0o755
     with _ -> ());
    let template =
      {|{
  "default_temperature": 0.7,
  "workspace": "~/.clawq/workspace",
  "default_provider": "openrouter",
  "providers": {
    "openrouter": {
      "api_key": "YOUR_API_KEY_HERE",
      "base_url": "https://openrouter.ai/api/v1"
    },
    "groq": {
      "api_key": "YOUR_GROQ_API_KEY_HERE",
      "base_url": "https://api.groq.com/openai/v1"
    },
    "zai_coding": {
      "api_key": "$ZAI_CODING_API_KEY",
      "default_model": "glm-5"
    }
  },
  "zai_mcp": {
    "web_search_enabled": true,
    "web_reader_enabled": true
  },
  "stt": {
    "provider": "groq",
    "model": "whisper-large-v3",
    "language": "en"
  },
  "agent_defaults": {
    "primary_model": "openai/gpt-4o",
    "model_priority": [
      { "provider": "openrouter", "model": "openai/gpt-4o" },
      { "provider": "groq", "model": "openai/gpt-oss-120b" }
    ],
    "max_tool_iterations": 10
  },
  "prompt": {
    "dynamic_enabled": true,
    "include_tools_section": true,
    "include_safety_section": true,
    "include_workspace_section": true,
    "include_runtime_section": true,
    "include_datetime_section": true,
    "workspace_files": ["AGENTS.md", "EGO.md", "SOUL.md", "TOOLS.md", "USER.md"],
    "max_workspace_file_chars": 3500,
    "max_workspace_total_chars": 12000
  },
  "channels": {
    "cli": true,
    "telegram": {
      "accounts": {
        "main": {
          "bot_token": "YOUR_BOT_TOKEN_HERE",
          "allow_from": ["*"]
        }
      }
    }
  },
  "gateway": {
    "host": "127.0.0.1",
    "port": 3000,
    "require_pairing": false
  },
  "memory": {
    "backend": "sqlite",
    "search_enabled": false
  },
  "security": {
    "workspace_only": true,
    "audit_enabled": false,
    "tools_enabled": true
  },
  "tunnel": {
    "enabled": false,
    "provider": "cloudflare",
    "cloudflare": {
      "api_token": "$CLOUDFLARE_API_TOKEN",
      "account_id": "",
      "tunnel_id": "",
      "tunnel_name": "clawq",
      "hostname": ""
    }
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
    ^ "\nThen run: clawq workspace init"
  end

let cmd_models () =
  let cfg = get_config () in
  match cfg.providers with
  | [] -> "No providers configured. Run 'clawq onboard' to set up."
  | providers ->
    let lines =
      List.map
        (fun (name, (p : Runtime_config.provider_config)) ->
          let url =
            match p.base_url with Some u -> u | None -> "(default)"
          in
          let model_info = match p.default_model with
            | Some m -> Printf.sprintf " model: %s" m
            | None -> ""
          in
          Printf.sprintf "  %s: %s (key: %s)%s" name url
            (if Runtime_config.is_key_set p.api_key then "configured" else "not set")
            model_info)
        providers
    in
    "Configured providers:\n" ^ String.concat "\n" lines
    ^ Printf.sprintf "\nDefault model: %s"
        (Runtime_config.effective_primary_model cfg.agent_defaults)
    ^ Printf.sprintf "\nDefault provider: %s"
        (match cfg.default_provider with Some p -> p | None -> "(auto)")

let cmd_channel () =
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
          (Printf.sprintf "  telegram/%s: %s (allow_from: %s)" name
             (if acct.bot_token = "" then "no token" else "configured")
             (String.concat ", " acct.allow_from)))
      tg.accounts);
  List.rev !lines |> String.concat "\n"

let cmd_memory () =
  let cfg = get_config () in
  Printf.sprintf "Memory backend: %s\nSearch enabled: %b" cfg.memory.backend
    cfg.memory.search_enabled

let cmd_workspace args =
  let cfg = get_config () in
  let workspace = Runtime_config.effective_workspace cfg in
  match args with
  | [ "init" ] ->
    let created = Workspace_scaffold.scaffold ~workspace in
    if created = [] then
      Printf.sprintf "Workspace ready at %s (no new files created)\n" workspace
    else
      Printf.sprintf "Workspace initialized at %s\nCreated:\n%s\n"
        workspace
        (String.concat "\n" (List.map (fun f -> "  - " ^ f) created))
  | _ ->
    let docs = [ "AGENTS.md"; "EGO.md"; "SOUL.md"; "USER.md"; "TOOLS.md" ] in
    let status_lines =
      List.map
        (fun name ->
          let path = Filename.concat workspace name in
          Printf.sprintf "  %s: %s" name
            (if Sys.file_exists path then "present" else "missing"))
        docs
    in
    "Workspace: " ^ workspace ^ "\n"
    ^ "Docs:\n" ^ String.concat "\n" status_lines ^ "\n"

let cmd_capabilities () =
  "Available capabilities:\n\
  \  - LLM chat (OpenAI-compatible providers)\n\
  \  - Telegram channel (long-polling)\n\
  \  - HTTP gateway (/health)\n\
  \  - Config management\n\
  \  - Session management"

let cmd_auth () =
  let cfg = get_config () in
  let providers = Runtime_config.with_zai_coding_provider cfg.providers in
  match providers with
  | [] -> "No providers configured. No API keys set."
  | listed_providers ->
    let lines =
      List.map
        (fun (name, (p : Runtime_config.provider_config)) ->
          Printf.sprintf "  %s: %s" name
            (if Runtime_config.is_key_set p.api_key then redact_key p.api_key
             else "not set"))
        listed_providers
    in
    "API key status:\n" ^ String.concat "\n" lines

let cmd_transcribe args =
  match args with
  | [] -> "Usage: clawq transcribe <audio_file>"
  | file_path :: _ ->
    if not (Sys.file_exists file_path) then
      Printf.sprintf "File not found: %s" file_path
    else
      let cfg = get_config () in
      let ic = open_in_bin file_path in
      let n = in_channel_length ic in
      let buf = Bytes.create n in
      really_input ic buf 0 n;
      close_in ic;
      let audio_data = Bytes.to_string buf in
      let filename = Filename.basename file_path in
      let content_type = Stt.content_type_of_ext filename in
      let result =
        Lwt_main.run
          (Stt.transcribe ~config:cfg ~audio_data ~filename ~content_type ())
      in
      result.text

let cmd_mcp () =
  let cfg = get_config () in
  Lwt_main.run (Mcp_server.run ~config:cfg ());
  ""

let parse_agent_workspace_override args =
  match args with
  | [] -> Ok None
  | [ "--workspace"; path ] when path <> "" -> Ok (Some path)
  | [ "--workspace" ] ->
    Error "Usage: clawq agent [--workspace <path>]"
  | _ ->
    Error "Usage: clawq agent [--workspace <path>]"

let cmd_agent args =
  match parse_agent_workspace_override args with
  | Error usage -> usage
  | Ok workspace_override ->
  let cfg = get_config () in
  let cfg =
    match workspace_override with
    | None -> cfg
    | Some workspace -> { cfg with workspace }
  in
  Lwt_main.run (Daemon.run ~config:cfg);
  "Daemon stopped."

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
      let header = Printf.sprintf "  %-20s %-15s %-20s %s" "NAME" "SESSION" "SCHEDULE" "ENABLED" in
      let rows = List.map (fun (j : Scheduler.job) ->
        Printf.sprintf "  %-20s %-15s %-20s %s" j.name j.session_key j.schedule_str
          (if j.enabled then "yes" else "no")
      ) jobs in
      "Cron jobs:\n" ^ header ^ "\n" ^ String.concat "\n" rows
  | "add" :: name :: session_key :: schedule :: message ->
    let db = get_db () in
    Scheduler.init_schema db;
    let msg = String.concat " " message in
    (match Scheduler.add_job ~db ~name ~session_key ~message:msg ~schedule with
     | Ok () -> Printf.sprintf "Added cron job '%s'" name
     | Error e -> Printf.sprintf "Error: %s" e)
  | [ "remove"; name ] ->
    let db = get_db () in
    Scheduler.init_schema db;
    if Scheduler.remove_job ~db ~name then
      Printf.sprintf "Removed job '%s'" name
    else
      Printf.sprintf "No job found with name '%s'" name
  | "history" :: name :: _ ->
    let db = get_db () in
    Scheduler.init_schema db;
    let runs = Scheduler.get_history ~db ~name ~limit:10 in
    if runs = [] then Printf.sprintf "No run history for '%s'" name
    else
      let header = Printf.sprintf "  %-5s %-20s %-8s %s" "ID" "STARTED" "STATUS" "PREVIEW" in
      let rows = List.map (fun (r : Scheduler.run) ->
        Printf.sprintf "  %-5d %-20s %-8s %s" r.run_id r.started_at r.status
          (match r.result_preview with Some p -> String.sub p 0 (min 40 (String.length p)) | None -> "")
      ) runs in
      Printf.sprintf "Run history for '%s':\n%s\n%s" name header (String.concat "\n" rows)
  | _ ->
    "Usage: clawq cron <list|add|remove|history>\n\
    \  cron list                                    - List all jobs\n\
    \  cron add <name> <session> <schedule> <msg>   - Add a job\n\
    \  cron remove <name>                           - Remove a job\n\
    \  cron history <name>                          - Show run history"

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
        let header = Printf.sprintf "  %-5s %-20s %-18s %-10s %s"
            "ID" "TIMESTAMP" "EVENT" "TOOL" "DETAILS" in
        let lines = List.map (fun (r : Audit.row) ->
          Printf.sprintf "  %-5d %-20s %-18s %-10s %s"
            r.id r.timestamp r.event_type
            (match r.tool_name with Some n -> n | None -> "")
            (match r.details with Some d ->
               if String.length d > 50 then String.sub d 0 50 ^ "..." else d
             | None -> "")
        ) rows in
        "Audit log:\n" ^ header ^ "\n" ^ String.concat "\n" lines
    | [ "list"; "--limit"; n ] ->
      let limit = try int_of_string n with _ -> 20 in
      let rows = Audit.query ~db ~limit () in
      if rows = [] then "No audit log entries."
      else
        let header = Printf.sprintf "  %-5s %-20s %-18s %-10s %s"
            "ID" "TIMESTAMP" "EVENT" "TOOL" "DETAILS" in
        let lines = List.map (fun (r : Audit.row) ->
          Printf.sprintf "  %-5d %-20s %-18s %-10s %s"
            r.id r.timestamp r.event_type
            (match r.tool_name with Some n -> n | None -> "")
            (match r.details with Some d ->
               if String.length d > 50 then String.sub d 0 50 ^ "..." else d
             | None -> "")
        ) rows in
        "Audit log:\n" ^ header ^ "\n" ^ String.concat "\n" lines
    | _ ->
      "Usage: clawq audit <list|list --limit N>"

let cmd_skills args =
  match args with
  | [ "list" ] | [] ->
    let files = Skills.list_skills () in
    if files = [] then
      "No skills found in " ^ Skills.skills_dir ()
    else
      "Skills:\n" ^ String.concat "\n"
        (List.map (fun f -> "  " ^ f) files)
  | [ "path" ] ->
    "Skills directory: " ^ Skills.skills_dir ()
  | [ "init" ] ->
    Skills.create_example ()
  | _ ->
    "Usage: clawq skills <list|path|init>"

let cmd_service args =
  match args with
  | [ "start" ] ->
    let cfg = get_config () in
    Service.cmd_start ~config:cfg
  | [ "stop" ] -> Service.cmd_stop ()
  | [ "status" ] | [] -> Service.cmd_status ()
  | [ "restart" ] ->
    let cfg = get_config () in
    Service.cmd_restart ~config:cfg
  | _ ->
    "Usage: clawq service <start|stop|status|restart>"

let handle args =
  match args with
  | "phase2" :: _ -> Phase2.render ()
  | "agent" :: rest -> cmd_agent rest
  | "status" :: _ -> cmd_status ()
  | "doctor" :: _ -> cmd_doctor ()
  | "onboard" :: _ -> cmd_onboard ()
  | "models" :: _ -> cmd_models ()
  | "channel" :: _ -> cmd_channel ()
  | "memory" :: _ -> cmd_memory ()
  | "workspace" :: rest -> cmd_workspace rest
  | "capabilities" :: _ -> cmd_capabilities ()
  | "auth" :: _ -> cmd_auth ()
  | "transcribe" :: rest -> cmd_transcribe rest
  | "mcp" :: _ -> cmd_mcp ()
  | "cron" :: rest -> cmd_cron rest
  | "skills" :: rest -> cmd_skills rest
  | "audit" :: rest -> cmd_audit rest
  | "hardware" :: _ -> "hardware: deferred to Phase 2"
  | "migrate" :: rest -> Migrate.cmd_migrate rest
  | "service" :: rest -> cmd_service rest
  | _ -> Clawq_core.dispatch args

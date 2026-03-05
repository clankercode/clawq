let config = lazy (Config_loader.load ())
let get_config () = Lazy.force config

let daemon_state_path () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat (Filename.concat home ".clawq") "daemon_state.json"

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

let redact_key s =
  let len = String.length s in
  if len <= 8 then String.make len '*'
  else String.sub s 0 4 ^ "..." ^ String.sub s (len - 4) 4

let cmd_status () =
  let cfg = get_config () in
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add "clawq status";
  add (Printf.sprintf "  model: %s" cfg.agent_defaults.primary_model);
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
       | Some tg -> Printf.sprintf "%d account(s)" (List.length tg.accounts)));
  add (Printf.sprintf "  memory backend: %s" cfg.memory.backend);
  add (Printf.sprintf "  providers: %d configured" (List.length cfg.providers));
  (match read_daemon_state () with
  | None -> add "  daemon: not running"
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
      if not (Runtime_config.is_key_set p.api_key) then
        add (Printf.sprintf "WARNING: Provider '%s' has no API key" name))
    cfg.providers;
  (match cfg.default_provider with
  | Some name when not (List.exists (fun (n, _) -> n = name) cfg.providers) ->
      add
        (Printf.sprintf "WARNING: default_provider '%s' not found in providers"
           name)
  | Some name -> (
      match List.assoc_opt name cfg.providers with
      | Some p when not (Runtime_config.is_key_set p.api_key) ->
          add
            (Printf.sprintf "WARNING: default_provider '%s' has no API key" name)
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
  let result =
    match List.rev !issues with
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
    },
    "groq": {
      "api_key": "YOUR_GROQ_API_KEY_HERE",
      "base_url": "https://api.groq.com/openai/v1"
    }
  },
  "stt": {
    "provider": "groq",
    "model": "whisper-large-v3",
    "language": "en"
  },
  "agent_defaults": {
    "primary_model": "openai/gpt-4o"
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
    "require_pairing": true,
    "auth_token": ""
  },
  "runtime": {
    "docker_image": "clawq:latest",
    "docker_container_name": "clawq",
    "docker_port": 3000
  },
  "tunnel": {
    "provider": "cloudflare",
    "enabled": false
  },
  "memory": {
    "backend": "sqlite",
    "search_enabled": false
  },
  "security": {
    "workspace_only": true,
    "audit_enabled": false,
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
            let model_info =
              match p.default_model with
              | Some m -> Printf.sprintf " model: %s" m
              | None -> ""
            in
            Printf.sprintf "  %s: %s (key: %s)%s" name url
              (if Runtime_config.is_key_set p.api_key then "configured"
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

let cmd_workspace () = Printf.sprintf "Workspace: %s" (Sys.getcwd ())

let cmd_capabilities () =
  let cfg = get_config () in
  let caps = ref [] in
  let add s = caps := s :: !caps in
  (* Providers *)
  let active_providers =
    List.filter
      (fun (_, p) -> Runtime_config.is_key_set p.Runtime_config.api_key)
      cfg.providers
  in
  add
    (Printf.sprintf "  - LLM chat: %d provider(s) configured (%s)"
       (List.length active_providers)
       (if active_providers = [] then "none active"
        else String.concat ", " (List.map fst active_providers)));
  (* Channels *)
  if cfg.channels.cli then add "  - CLI channel: enabled";
  (match cfg.channels.telegram with
  | Some tg ->
      add
        (Printf.sprintf "  - Telegram channel: %d account(s)"
           (List.length tg.accounts))
  | None -> ());
  (* Gateway *)
  add
    (Printf.sprintf "  - HTTP gateway: %s:%d" cfg.gateway.host cfg.gateway.port);
  (* Memory *)
  add
    (Printf.sprintf "  - Memory: %s (FTS search: %s)" cfg.memory.backend
       (if cfg.memory.search_enabled then "enabled" else "disabled"));
  (* Tools *)
  if cfg.security.tools_enabled then begin
    let registry = Tool_registry.create () in
    Tools_builtin.register_all ~config:cfg registry;
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
  (* MCP *)
  if cfg.mcp.enabled then begin
    let exposed =
      match cfg.mcp.exposed_tools with
      | None -> "all tools"
      | Some names -> String.concat ", " names
    in
    add (Printf.sprintf "  - MCP server: enabled (exposing: %s)" exposed)
  end
  else add "  - MCP server: disabled";
  (* Security *)
  add
    (Printf.sprintf
       "  - Security: workspace_only=%b audit=%b encrypt_secrets=%b"
       cfg.security.workspace_only cfg.security.audit_enabled
       cfg.security.encrypt_secrets);
  (* STT *)
  (match cfg.stt with
  | Some s -> add (Printf.sprintf "  - Voice/STT: %s (%s)" s.provider s.model)
  | None -> ());
  (* Cron *)
  add "  - Cron scheduler: available";
  (* Service management *)
  add "  - Service management: start/stop/restart/status";
  "Available capabilities:\n" ^ String.concat "\n" (List.rev !caps)

let cmd_auth args =
  match args with
  | [ "encrypt" ] ->
      if not (get_config ()).security.encrypt_secrets then
        "Secret encryption is disabled. Set security.encrypt_secrets to true \
         in config."
      else begin
        match Secret_store.get_master_key () with
        | Error msg -> Printf.sprintf "Error: %s" msg
        | Ok key ->
            let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
            let config_path =
              Filename.concat (Filename.concat home ".clawq") "config.json"
            in
            if not (Sys.file_exists config_path) then
              "No config file found at " ^ config_path
            else begin
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
                        "API keys encrypted in " ^ config_path
                      with exn ->
                        Printf.sprintf "Failed to write config: %s"
                          (Printexc.to_string exn)))
            end
      end
  | _ -> (
      let cfg = get_config () in
      match cfg.providers with
      | [] -> "No providers configured. No API keys set."
      | providers ->
          let lines =
            List.map
              (fun (name, (p : Runtime_config.provider_config)) ->
                Printf.sprintf "  %s: %s" name
                  (if Runtime_config.is_key_set p.api_key then
                     redact_key p.api_key
                   else "not set"))
              providers
          in
          "API key status:\n" ^ String.concat "\n" lines)

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
  if not cfg.mcp.enabled then
    "MCP server is disabled. Set mcp.enabled to true in config."
  else if not cfg.security.tools_enabled then
    "MCP server requires security.tools_enabled=true to expose tools."
  else begin
    let registry = Tool_registry.create () in
    Tools_builtin.register_all ~config:cfg registry;
    let skills =
      Skills.load_all ~workspace_only:cfg.security.workspace_only
        ~allowed_commands:Tools_builtin.default_shell_allowlist ()
    in
    List.iter (fun s -> Tool_registry.register registry s) skills;
    (* Filter to exposed_tools allowlist if configured *)
    (match cfg.mcp.exposed_tools with
    | Some allowed ->
        registry.tools <-
          List.filter
            (fun (t : Tool.t) -> List.mem t.name allowed)
            registry.tools
    | None -> ());
    Lwt_main.run (Mcp_server.run ~registry ());
    ""
  end

let cmd_agent () =
  let cfg = get_config () in
  (try Lwt_main.run (Daemon.run ~config:cfg)
   with Failure msg ->
     print_endline ("Error: " ^ msg);
     exit 1);
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
  | "history" :: name :: _ ->
      let db = get_db () in
      Scheduler.init_schema db;
      let runs = Scheduler.get_history ~db ~name ~limit:10 in
      if runs = [] then Printf.sprintf "No run history for '%s'" name
      else
        let header =
          Printf.sprintf "  %-5s %-20s %-8s %s" "ID" "STARTED" "STATUS"
            "PREVIEW"
        in
        let rows =
          List.map
            (fun (r : Scheduler.run) ->
              Printf.sprintf "  %-5d %-20s %-8s %s" r.run_id r.started_at
                r.status
                (match r.result_preview with
                | Some p -> String.sub p 0 (min 40 (String.length p))
                | None -> ""))
            runs
        in
        Printf.sprintf "Run history for '%s':\n%s\n%s" name header
          (String.concat "\n" rows)
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
          let header =
            Printf.sprintf "  %-5s %-20s %-18s %-10s %s" "ID" "TIMESTAMP"
              "EVENT" "TOOL" "DETAILS"
          in
          let lines =
            List.map
              (fun (r : Audit.row) ->
                Printf.sprintf "  %-5d %-20s %-18s %-10s %s" r.id r.timestamp
                  r.event_type
                  (match r.tool_name with Some n -> n | None -> "")
                  (match r.details with
                  | Some d ->
                      if String.length d > 50 then String.sub d 0 50 ^ "..."
                      else d
                  | None -> ""))
              rows
          in
          "Audit log:\n" ^ header ^ "\n" ^ String.concat "\n" lines
    | [ "list"; "--limit"; n ] ->
        let limit = try int_of_string n with _ -> 20 in
        let rows = Audit.query ~db ~limit () in
        if rows = [] then "No audit log entries."
        else
          let header =
            Printf.sprintf "  %-5s %-20s %-18s %-10s %s" "ID" "TIMESTAMP"
              "EVENT" "TOOL" "DETAILS"
          in
          let lines =
            List.map
              (fun (r : Audit.row) ->
                Printf.sprintf "  %-5d %-20s %-18s %-10s %s" r.id r.timestamp
                  r.event_type
                  (match r.tool_name with Some n -> n | None -> "")
                  (match r.details with
                  | Some d ->
                      if String.length d > 50 then String.sub d 0 50 ^ "..."
                      else d
                  | None -> ""))
              rows
          in
          "Audit log:\n" ^ header ^ "\n" ^ String.concat "\n" lines
    | [ "verify" ] -> (
        match Audit.get_signing_key () with
        | Error msg -> Printf.sprintf "Error: %s" msg
        | Ok key -> (
            match Audit.verify_chain ~db ~key with
            | Ok () -> "Audit chain verification: OK"
            | Error (id, reason) ->
                Printf.sprintf "Audit chain verification FAILED at entry %d: %s"
                  id reason))
    | [ "export" ] ->
        let path = cfg.security.audit_retention.export_path in
        let export_file = Filename.concat path "audit_export.jsonl" in
        let count = Audit.export_json ~db ~path:export_file in
        Printf.sprintf "Exported %d audit entries to %s" count export_file
    | [ "export"; path ] ->
        let count = Audit.export_json ~db ~path in
        Printf.sprintf "Exported %d audit entries to %s" count path
    | [ "purge" ] ->
        let ret = cfg.security.audit_retention in
        let deleted =
          Audit.purge_old ~db ~max_age_days:ret.max_age_days
            ~max_entries:ret.max_entries
        in
        Printf.sprintf "Purged %d audit entries" deleted
    | _ -> "Usage: clawq audit <list|list --limit N|verify|export [path]|purge>"

let cmd_skills args =
  match args with
  | [ "list" ] | [] ->
      let files = Skills.list_skills () in
      if files = [] then "No skills found in " ^ Skills.skills_dir ()
      else "Skills:\n" ^ String.concat "\n" (List.map (fun f -> "  " ^ f) files)
  | [ "path" ] -> "Skills directory: " ^ Skills.skills_dir ()
  | [ "init" ] -> Skills.create_example ()
  | _ -> "Usage: clawq skills <list|path|init>"

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
  | _ -> "Usage: clawq service <start|stop|status|restart>"

let cmd_runtime args =
  let cfg = get_config () in
  let docker_cfg =
    {
      Runtime_docker.image = cfg.runtime.docker_image;
      container_name = cfg.runtime.docker_container_name;
      port = cfg.runtime.docker_port;
      extra_args = [];
    }
  in
  match args with
  | [ "status" ] | [] ->
      let native_status = Runtime_native.status_string () in
      let docker_status =
        Lwt_main.run (Runtime_docker.status ~docker_config:docker_cfg)
      in
      Printf.sprintf "Runtime status:\n  native: %s\n  docker: %s" native_status
        docker_status
  | [ "native"; "start" ] -> (
      match Runtime_native.start ~config:cfg with
      | Ok () -> "Native runtime started"
      | Error msg -> Printf.sprintf "Error: %s" msg)
  | [ "native"; "stop" ] -> (
      match Runtime_native.stop () with
      | Ok () -> "Native runtime stopped"
      | Error msg -> Printf.sprintf "Error: %s" msg)
  | [ "native"; "health" ] ->
      let healthy = Lwt_main.run (Runtime_native.health ~config:cfg) in
      if healthy then "Native runtime: healthy" else "Native runtime: unhealthy"
  | [ "docker"; "start" ] ->
      Lwt_main.run (Runtime_docker.start ~docker_config:docker_cfg ~config:cfg)
  | [ "docker"; "stop" ] ->
      Lwt_main.run (Runtime_docker.stop ~docker_config:docker_cfg)
  | [ "docker"; "health" ] ->
      let healthy =
        Lwt_main.run (Runtime_docker.health ~docker_config:docker_cfg)
      in
      if healthy then "Docker runtime: healthy" else "Docker runtime: unhealthy"
  | _ ->
      "Usage: clawq runtime <status|native start|native stop|native \
       health|docker start|docker stop|docker health>"

let cmd_tunnel args =
  let tunnel_state_path () =
    let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
    Filename.concat (Filename.concat home ".clawq") "tunnel_state.json"
  in
  let save_tunnel_state ~pid ~port ~url =
    let start_ticks = proc_start_ticks pid in
    let path = tunnel_state_path () in
    let dir = Filename.dirname path in
    (try if not (Sys.file_exists dir) then Sys.mkdir dir 0o755 with _ -> ());
    let json =
      `Assoc
        [
          ("provider", `String Tunnel_cloudflare.name);
          ("pid", `Int pid);
          ("port", `Int port);
          ("url", `String url);
          ( "start_ticks",
            match start_ticks with Some s -> `String s | None -> `Null );
        ]
    in
    try
      let oc = open_out path in
      output_string oc (Yojson.Safe.pretty_to_string ~std:true json);
      output_char oc '\n';
      close_out oc;
      Ok ()
    with exn -> Error (Printexc.to_string exn)
  in
  let read_tunnel_state () =
    let path = tunnel_state_path () in
    if not (Sys.file_exists path) then None
    else
      try
        let json = Yojson.Safe.from_file path in
        let open Yojson.Safe.Util in
        let pid = json |> member "pid" |> to_int in
        let url = json |> member "url" |> to_string in
        let start_ticks =
          try
            let v = json |> member "start_ticks" in
            if v = `Null then None else Some (to_string v)
          with _ -> None
        in
        Some (pid, url, start_ticks)
      with _ -> None
  in
  let remove_tunnel_state () =
    let path = tunnel_state_path () in
    if Sys.file_exists path then try Sys.remove path with _ -> ()
  in
  let cfg = get_config () in
  if cfg.tunnel.provider <> Tunnel_cloudflare.name then
    Printf.sprintf
      "Tunnel provider '%s' is not supported in this build (supported: %s)"
      cfg.tunnel.provider Tunnel_cloudflare.name
  else if not cfg.tunnel.enabled then
    "Tunnel is disabled in config (set tunnel.enabled=true to use)"
  else
    let tunnel_pid_matches ~pid ~start_ticks =
      if not (pid_is_alive pid) then false
      else if not (proc_cmdline_contains ~needle:"cloudflared" pid) then false
      else
        match (start_ticks, proc_start_ticks pid) with
        | Some expected, Some actual -> expected = actual
        | _ -> true
    in
    match args with
    | [ "start" ] -> (
        let tunnel = Tunnel_cloudflare.create ~port:cfg.gateway.port in
        Lwt_main.run (Tunnel_cloudflare.start tunnel);
        match
          (Tunnel_cloudflare.get_pid tunnel, Tunnel_cloudflare.get_url tunnel)
        with
        | Some pid, Some url -> (
            match save_tunnel_state ~pid ~port:cfg.gateway.port ~url with
            | Ok () -> Printf.sprintf "Tunnel started: %s (pid %d)" url pid
            | Error err ->
                Printf.sprintf
                  "Tunnel started: %s (pid %d)\n\
                   Warning: failed to save state: %s"
                  url pid err)
        | _ -> "Tunnel started but URL or PID not available")
    | [ "stop" ] -> (
        match read_tunnel_state () with
        | None -> "No running tunnel state found"
        | Some (pid, _url, start_ticks) ->
            if not (tunnel_pid_matches ~pid ~start_ticks) then begin
              remove_tunnel_state ();
              Printf.sprintf
                "Refusing to stop pid %d: tunnel process identity mismatch; \
                 stale state removed"
                pid
            end
            else begin
              (try Unix.kill pid Sys.sigterm with _ -> ());
              let rec wait_for_exit attempts =
                if attempts <= 0 then false
                else
                  try
                    Unix.kill pid 0;
                    Unix.sleepf 0.2;
                    wait_for_exit (attempts - 1)
                  with Unix.Unix_error _ -> true
              in
              if wait_for_exit 20 then begin
                remove_tunnel_state ();
                Printf.sprintf "Tunnel stopped (pid %d)" pid
              end
              else
                Printf.sprintf
                  "Tunnel stop signal sent but process still running (pid %d)"
                  pid
            end)
    | [ "status" ] | [] -> (
        match read_tunnel_state () with
        | None ->
            Printf.sprintf
              "Tunnel provider: %s\n\
              \  Status: stopped\n\
              \  To start: clawq tunnel start"
              Tunnel_cloudflare.name
        | Some (pid, url, start_ticks) ->
            let running = tunnel_pid_matches ~pid ~start_ticks in
            if running then
              Printf.sprintf
                "Tunnel provider: %s\n  Status: running (pid %d)\n  URL: %s"
                Tunnel_cloudflare.name pid url
            else begin
              remove_tunnel_state ();
              Printf.sprintf
                "Tunnel provider: %s\n  Status: stopped (stale state cleaned)"
                Tunnel_cloudflare.name
            end)
    | _ -> "Usage: clawq tunnel <start|status|stop>"

let cmd_reset_agent () =
  let cfg = get_config () in
  let workspace = Runtime_config.effective_workspace cfg in
  let db_path =
    if cfg.memory.db_path <> "" then cfg.memory.db_path
    else
      let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
      Filename.concat (Filename.concat home ".clawq") "memory.db"
  in
  let red s = "\027[1;31m" ^ s ^ "\027[0m" in
  let bold s = "\027[1m" ^ s ^ "\027[0m" in
  let dim s = "\027[2m" ^ s ^ "\027[0m" in
  print_endline "";
  print_endline (red "  !! RESET AGENT !!");
  print_endline "";
  print_endline "  This will permanently delete:";
  print_endline
    ("    "
    ^ bold "· All conversation history  "
    ^ dim ("(" ^ db_path ^ " — messages, embeddings)"));
  print_endline
    ("    "
    ^ bold "· All cron jobs and run logs  "
    ^ dim "(cron_jobs, cron_runs)");
  print_endline
    ("    "
    ^ bold "· All workspace identity files  "
    ^ dim ("(" ^ workspace ^ "/)"));
  print_endline
    (dim
       "      EGO.md  AGENTS.md  USER.md  IDENTITY.md  TOOLS.md  HEARTBEAT.md  \
        BOOTSTRAP.md");
  print_endline "";
  print_endline "  This will NOT touch:";
  print_endline (dim "    · config.json");
  print_endline (dim "    · daemon.log  daemon.pid");
  print_endline "";
  print_string "  Type ";
  print_string (bold "RESET");
  print_string " to confirm, or anything else to cancel: ";
  flush stdout;
  let answer = try input_line stdin with End_of_file -> "" in
  print_endline "";
  if String.trim answer <> "RESET" then begin
    print_endline "  Cancelled. Nothing changed.";
    print_endline "";
    "Cancelled."
  end
  else begin
    let db =
      Memory.init ~db_path ~search_enabled:cfg.memory.search_enabled ()
    in
    let exec sql =
      let stmt = Sqlite3.prepare db sql in
      ignore (Sqlite3.step stmt);
      ignore (Sqlite3.finalize stmt)
    in
    exec "DELETE FROM messages";
    exec "DELETE FROM embeddings";
    exec "DELETE FROM cron_jobs";
    exec "DELETE FROM cron_runs";
    ignore (Sqlite3.db_close db);
    List.iter
      (fun (name, content) ->
        let path = Filename.concat workspace name in
        try
          let oc = open_out path in
          output_string oc content;
          close_out oc
        with _ -> ())
      Workspace_scaffold.templates;
    print_endline "  Done:";
    print_endline "    · Conversation history cleared";
    print_endline "    · Cron jobs and run logs cleared";
    print_endline "    · Workspace files redeployed from defaults";
    print_endline "";
    "Agent reset complete."
  end

let handle args =
  match args with
  | "phase2" :: _ -> Phase2.render ()
  | "agent" :: _ -> cmd_agent ()
  | "status" :: _ -> cmd_status ()
  | "doctor" :: _ -> cmd_doctor ()
  | "onboard" :: _ -> cmd_onboard ()
  | "models" :: _ -> cmd_models ()
  | "channel" :: _ -> cmd_channel ()
  | "memory" :: _ -> cmd_memory ()
  | "workspace" :: _ -> cmd_workspace ()
  | "capabilities" :: _ -> cmd_capabilities ()
  | "auth" :: rest -> cmd_auth rest
  | "transcribe" :: rest -> cmd_transcribe rest
  | "mcp" :: _ -> cmd_mcp ()
  | "cron" :: rest -> cmd_cron rest
  | "skills" :: rest -> cmd_skills rest
  | "audit" :: rest -> cmd_audit rest
  | "runtime" :: rest -> cmd_runtime rest
  | "tunnel" :: rest -> cmd_tunnel rest
  | "hardware" :: _ -> "hardware: deferred to Phase 2"
  | "migrate" :: rest -> Migrate.cmd_migrate rest
  | "service" :: rest -> cmd_service rest
  | "reset-agent" :: _ -> cmd_reset_agent ()
  | _ -> Clawq_core.dispatch args

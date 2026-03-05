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

let cmd_status () =
  let cfg = get_config () in
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add "clawq status";
  add (Printf.sprintf "  model: %s" cfg.agent_defaults.primary_model);
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
  let issues = ref [] in
  let add s = issues := s :: !issues in
  if cfg.providers = [] then add "WARNING: No providers configured";
  List.iter
    (fun (name, (p : Runtime_config.provider_config)) ->
      if not (Runtime_config.is_key_set p.api_key) then
        add (Printf.sprintf "WARNING: Provider '%s' has no API key" name))
    cfg.providers;
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
    (try
       if not (Sys.file_exists config_dir) then Sys.mkdir config_dir 0o755
     with _ -> ());
    let template =
      {|{
  "default_temperature": 0.7,
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
    "require_pairing": false
  },
  "memory": {
    "backend": "sqlite",
    "search_enabled": false
  },
  "security": {
    "workspace_only": true,
    "audit_enabled": false
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
          Printf.sprintf "  %s: %s (key: %s)" name url
            (if Runtime_config.is_key_set p.api_key then "configured" else "not set"))
        providers
    in
    "Configured providers:\n" ^ String.concat "\n" lines
    ^ Printf.sprintf "\nDefault model: %s" cfg.agent_defaults.primary_model

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

let cmd_workspace () =
  Printf.sprintf "Workspace: %s" (Sys.getcwd ())

let cmd_capabilities () =
  "Available capabilities:\n\
  \  - LLM chat (OpenAI-compatible providers)\n\
  \  - Telegram channel (long-polling)\n\
  \  - HTTP gateway (/health)\n\
  \  - Config management\n\
  \  - Session management"

let cmd_auth () =
  let cfg = get_config () in
  match cfg.providers with
  | [] -> "No providers configured. No API keys set."
  | providers ->
    let lines =
      List.map
        (fun (name, (p : Runtime_config.provider_config)) ->
          Printf.sprintf "  %s: %s" name
            (if Runtime_config.is_key_set p.api_key then redact_key p.api_key
             else "not set"))
        providers
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

let cmd_agent () =
  let cfg = get_config () in
  Lwt_main.run (Daemon.run ~config:cfg);
  "Daemon stopped."

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
  | "auth" :: _ -> cmd_auth ()
  | "transcribe" :: rest -> cmd_transcribe rest
  | "mcp" :: _ -> cmd_mcp ()
  | "cron" :: _ -> "cron: not yet implemented"
  | "skills" :: _ -> "skills: not yet implemented"
  | "hardware" :: _ -> "hardware: deferred to Phase 2"
  | "migrate" :: _ -> "migrate: not yet implemented"
  | "service" :: _ -> "service: not yet implemented"
  | _ -> Clawq_core.dispatch args

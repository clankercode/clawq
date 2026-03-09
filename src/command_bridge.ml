(* Force-link provider_init.ml so its native-provider registrations run. *)
let _link_provider_init = Provider_init.registered
let get_config () = Config_loader.load ()

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

let gateway_token_path () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat (Filename.concat home ".clawq") "gateway_token"

let read_gateway_token () =
  match read_file (gateway_token_path ()) with
  | Some token when String.trim token <> "" -> Some (String.trim token)
  | _ -> None

let save_gateway_token token =
  let token = String.trim token in
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let clawq_dir = Filename.concat home ".clawq" in
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

let try_auto_pair_live_gateway ~host ~port =
  if not (is_loopback_host host) then No_attempt
  else
    match read_live_gateway_pairing_code () with
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
  | Ok ((401 | 403), _) as rejected when headers = [] -> (
      match try_auto_pair_live_gateway ~host ~port with
      | Paired token ->
          let retry_headers = [ ("Authorization", "Bearer " ^ token) ] in
          post_json_sync ~uri:url ~headers:retry_headers ~body
      | No_attempt -> rejected
      | Pair_failed msg -> Error ("Auto-pair failed: " ^ msg))
  | other -> other

let redact_key s =
  let len = String.length s in
  if len <= 8 then String.make len '*'
  else String.sub s 0 4 ^ "..." ^ String.sub s (len - 4) 4

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

let build_tool_registry ?db (cfg : Runtime_config.t) =
  if not cfg.security.tools_enabled then None
  else begin
    let registry = Tool_registry.create () in
    let sandbox = make_sandbox cfg in
    Tools_builtin.register_all ~config:cfg ~sandbox ?db registry;
    let skills =
      Skills.load_all ~workspace_only:cfg.security.workspace_only
        ~allowed_commands:Tools_builtin.default_shell_allowlist ()
    in
    List.iter (fun s -> Tool_registry.register registry s) skills;
    Tool_registry.register registry
      (Skills.skill_create_tool ~workspace_only:cfg.security.workspace_only
         ~allowed_commands:Tools_builtin.default_shell_allowlist registry);
    Tool_registry.register registry (Skills.skill_list_tool ());
    Some registry
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
  let tool_registry = build_tool_registry ~db:(Some (get_db ())) cfg in
  let provider_name, _provider, model = Provider.select_provider ~config:cfg in
  let agent = Agent.create ~config:cfg ?tool_registry () in
  let user_message =
    match String.concat " " args with "" -> "Hello!" | msg -> msg
  in
  let messages =
    if user_message = "" then Agent.build_messages agent
    else begin
      let compacted =
        Lwt_main.run (Agent.prepare_turn_history agent ~user_message ())
      in
      let sandbox = make_sandbox cfg in
      let shell_policy_summary, shell_is_sandboxed =
        shell_policy_summary cfg sandbox
      in
      let runtime_context =
        Prompt_builder.build_runtime_context ~config:cfg
          ~details:
            {
              Prompt_builder.session_id = "__debug_prompt__";
              session_name = Some "debug prompt";
              is_main_session = false;
              heartbeat_routing_applies = false;
              effective_workspace = Runtime_config.effective_workspace cfg;
              workspace_only = cfg.security.workspace_only;
              sandbox_backend_requested = cfg.security.sandbox_backend;
              sandbox_backend_effective =
                Sandbox.backend_to_string sandbox.Sandbox.backend;
              shell_is_sandboxed;
              shell_policy_summary;
              shell_visible_roots_summary = shell_visible_roots_summary cfg;
              background_tasks = [];
              context_usage =
                Some
                  (Agent.runtime_context_usage agent
                     ~compacted_before_turn:compacted);
              task_tree_summary = None;
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
  add
    (Printf.sprintf "  discord: %s"
       (match cfg.channels.discord with
       | None -> "not configured"
       | Some d ->
           Printf.sprintf "configured (guilds=%d users=%d)"
             (List.length d.allow_guilds)
             (List.length d.allow_users)));
  add
    (Printf.sprintf "  slack: %s"
       (match cfg.channels.slack with
       | None -> "not configured"
       | Some s ->
           Printf.sprintf "configured (path=%s socket_mode=%b)" s.events_path
             s.socket_mode));
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
    ^ "\nEdit it to add your API keys and bot tokens."
  end

let cmd_config args =
  match args with
  | [ "wizard" ] ->
      Config_wizard_tui.run_wizard Config_wizard_model.FullWizard;
      ""
  | "set" :: key :: value :: _ -> Config_set.set_value key value
  | [ "get"; key ] -> Config_set.get_value key
  | "show" :: rest -> Config_show.show (List.nth_opt rest 0)
  | _ ->
      "Usage: clawq config <subcommand>\n\n\
       Subcommands:\n\
      \  wizard           Interactive configuration wizard\n\
      \  set KEY VALUE    Set a config value by dot-path\n\
      \  get KEY          Get a config value by dot-path\n\
      \  show [SECTION]   Display current config (secrets redacted)"

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
               else if Runtime_config.provider_has_codex_oauth p then
                 "codex-oauth"
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
  (match cfg.channels.discord with
  | None -> add "  discord: not configured"
  | Some d ->
      add
        (Printf.sprintf
           "  discord: configured (allow_guilds: %s; allow_users: %s; intents: \
            %d)"
           (String.concat ", " d.allow_guilds)
           (String.concat ", " d.allow_users)
           d.intents));
  (match cfg.channels.slack with
  | None -> add "  slack: not configured"
  | Some s ->
      add
        (Printf.sprintf
           "  slack: configured (events_path: %s; socket_mode: %b; \
            allow_channels: %s; allow_users: %s)"
           s.events_path s.socket_mode
           (String.concat ", " s.allow_channels)
           (String.concat ", " s.allow_users)));
  List.rev !lines |> String.concat "\n"

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
      let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
      let path =
        match rest with
        | [ p ] -> p
        | _ ->
            Filename.concat
              (Filename.concat home ".clawq")
              "memory_snapshot.json"
      in
      let db = get_db () in
      try
        Memory.export_snapshot ~db ~path;
        Printf.sprintf "Exported core memories to %s" path
      with exn -> "Error: " ^ Printexc.to_string exn)
  | "import" :: [ path ] -> (
      if not (Sys.file_exists path) then
        Printf.sprintf "File not found: %s" path
      else
        let db = get_db () in
        try
          Memory.import_snapshot ~db ~path;
          Printf.sprintf "Imported core memories from %s" path
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

let cmd_workspace () =
  let cfg = get_config () in
  Printf.sprintf "Workspace: %s" (Runtime_config.effective_workspace cfg)

type session_list_args = {
  channel : string option;
  prefix : string option;
  activity : Memory.session_activity;
  only_main : bool option;
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
    | flag :: _ when String.length flag > 0 && flag.[0] = '-' ->
        Error (Printf.sprintf "Unknown session list flag: %s" flag)
    | _ ->
        Error
          "Usage: clawq session list [--channel NAME] [--prefix PREFIX] \
           [--active|--inactive] [--main|--non-main]"
  in
  loop
    { channel = None; prefix = None; activity = Memory.Any; only_main = None }
    args

let parse_epoch_selector args =
  let rec loop epoch = function
    | [] -> Ok epoch
    | "--epoch" :: "current" :: rest -> loop (Some Memory.Current) rest
    | "--epoch" :: value :: rest -> (
        match int_of_string_opt value with
        | Some id when id > 0 -> loop (Some (Memory.Archived id)) rest
        | _ -> Error (Printf.sprintf "Invalid epoch value: %s" value))
    | flag :: _ when String.length flag > 0 && flag.[0] = '-' ->
        Error (Printf.sprintf "Unknown session show flag: %s" flag)
    | _ -> Error "Usage: clawq session show SESSION [--epoch current|ID]"
  in
  loop None args

let string_or_null = function Some value -> `String value | None -> `Null
let session_show_system_prompt _config = "[redacted in session show]"

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

let cmd_session args =
  let db = get_db () in
  match args with
  | [] | [ "list" ] ->
      let sessions = Memory.list_session_infos ~db () in
      if sessions = [] then "No sessions found"
      else
        String.concat "\n"
          (List.map
             (fun (row : Memory.session_info) ->
               let channel =
                 match row.channel with
                 | Some value -> value
                 | None -> (
                     match
                       Memory.parse_channel_from_session_key row.session_key
                     with
                     | Some value -> value
                     | None -> "-")
               in
               let state =
                 if row.turn = Some "agent" then "active" else "inactive"
               in
               Printf.sprintf
                 "%s  state=%s  channel=%s  messages=%d  archives=%d"
                 row.session_key state channel row.message_count
                 row.archived_epoch_count)
             sessions)
  | "list" :: rest -> (
      match parse_session_list_args rest with
      | Error msg -> msg
      | Ok parsed ->
          let sessions =
            Memory.list_session_infos ~db ?channel:parsed.channel
              ?prefix:parsed.prefix ~activity:parsed.activity
              ?only_main:parsed.only_main ()
          in
          if sessions = [] then "No sessions matched"
          else
            String.concat "\n"
              (List.map
                 (fun (row : Memory.session_info) ->
                   let channel =
                     match row.channel with
                     | Some value -> value
                     | None -> (
                         match
                           Memory.parse_channel_from_session_key row.session_key
                         with
                         | Some value -> value
                         | None -> "-")
                   in
                   let state =
                     if row.turn = Some "agent" then "active" else "inactive"
                   in
                   Printf.sprintf
                     "%s  state=%s  channel=%s  messages=%d  archives=%d"
                     row.session_key state channel row.message_count
                     row.archived_epoch_count)
                 sessions))
  | [ "epochs"; session_key ] ->
      let epochs = Memory.list_session_epochs ~db ~session_key in
      if
        List.for_all
          (fun (e : Memory.session_epoch) -> e.message_count = 0)
          epochs
      then Printf.sprintf "No chat log found for session %s" session_key
      else
        Yojson.Safe.pretty_to_string
          (`Assoc
             [
               ("session_key", `String session_key);
               ("epochs", `List (List.map session_epoch_json epochs));
             ])
  | "show" :: session_key :: rest -> (
      match parse_epoch_selector rest with
      | Error msg -> msg
      | Ok epoch_selector -> (
          let epoch =
            match epoch_selector with
            | Some value -> value
            | None -> Memory.Current
          in
          let epoch_label =
            match epoch with
            | Memory.Current -> `String "current"
            | Memory.Archived id -> `Int id
          in
          match Memory.load_epoch_messages ~db ~session_key ~epoch with
          | None -> Printf.sprintf "No epoch matched for session %s" session_key
          | Some rows ->
              let config = get_config () in
              let epochs = Memory.list_session_epochs ~db ~session_key in
              let archived =
                List.filter
                  (fun (e : Memory.session_epoch) -> not e.current)
                  epochs
              in
              let archived_epoch_count = List.length archived in
              let total_archived_messages =
                List.fold_left
                  (fun acc (e : Memory.session_epoch) -> acc + e.message_count)
                  0 archived
              in
              Yojson.Safe.pretty_to_string
                (`Assoc
                   [
                     ("session_key", `String session_key);
                     ("epoch", epoch_label);
                     ( "system_prompt",
                       `String (session_show_system_prompt config) );
                     ("archived_epoch_count", `Int archived_epoch_count);
                     ("total_archived_messages", `Int total_archived_messages);
                     ( "messages",
                       `List
                         (List.mapi
                            (fun i row -> raw_message_json config i row)
                            rows) );
                   ])))
  | "inject" :: session_key :: message_parts -> (
      let message = String.concat " " message_parts in
      if String.trim message = "" then
        "Usage: clawq session inject SESSION MESSAGE..."
      else
        match read_live_daemon_gateway () with
        | None ->
            Memory.store_message ~db ~session_key
              (Provider.make_message ~role:"user" ~content:message);
            Memory.upsert_session_state ~db ~session_key ~turn:"user" ();
            Printf.sprintf
              "Warning: no live daemon detected, so the injected message could \
               not use live busy/queue/bang semantics. Persisted it to session \
               %s so it will appear in the log when the agent starts again."
              session_key
        | Some (host, port) -> (
            let cfg = get_config () in
            let body =
              Yojson.Safe.to_string
                (`Assoc
                   [
                     ("session_key", `String session_key);
                     ("message", `String message);
                   ])
            in
            let result =
              post_live_gateway_json ~cfg ~host ~port ~path:"/session/inject"
                ~body
            in
            match result with
            | Error msg -> Printf.sprintf "Session inject failed: %s" msg
            | Ok (status, resp_body) -> (
                match status with
                | 200 -> (
                    try
                      let json = Yojson.Safe.from_string resp_body in
                      let open Yojson.Safe.Util in
                      let queued = json |> member "queued" |> to_bool in
                      let response = json |> member "response" |> to_string in
                      if queued then
                        Printf.sprintf
                          "Queued injected message for busy session %s%s"
                          session_key
                          (if String.length message > 0 && message.[0] = '!'
                           then " (bang interrupt requested)"
                           else "")
                      else
                        Printf.sprintf
                          "Processed injected message for session %s\n%s"
                          session_key response
                    with _ ->
                      Printf.sprintf
                        "Session inject succeeded for %s but returned an \
                         unexpected response: %s"
                        session_key resp_body)
                | 401 | 403 ->
                    Printf.sprintf
                      "Session inject was rejected by the live gateway (%d): %s"
                      status
                      (match parse_json_error_body resp_body with
                      | Some msg -> msg
                      | None -> resp_body)
                | _ ->
                    Printf.sprintf "Session inject failed (%d): %s" status
                      (match parse_json_error_body resp_body with
                      | Some msg -> msg
                      | None -> resp_body))))
  | _ ->
      "Usage: clawq session <subcommand>\n\
      \  session list [--channel NAME] [--prefix PREFIX] [--active|--inactive] \
       [--main|--non-main]\n\
      \  session epochs SESSION\n\
      \  session show SESSION [--epoch current|ID]\n\
      \  session inject SESSION MESSAGE..."

type background_add_args = {
  runner : Background_task.runner;
  model : string option;
  repo_path : string;
  branch : string option;
  prompt : string;
}

type background_wait_args = { id : int; timeout_seconds : float }
type background_logs_args = { id : int; lines : int; follow : bool }

type delegate_args = {
  preferred_runner : Background_task.runner option;
  model : string option;
  repo_path : string option;
  branch : string option;
  goal : string;
}

let path_is_git_repo path =
  Sys.command
    (Printf.sprintf "git -C %s rev-parse --is-inside-work-tree >/dev/null 2>&1"
       (Filename.quote path))
  = 0

let default_delegate_repo_path (cfg : Runtime_config.t) =
  let cwd = Sys.getcwd () in
  if path_is_git_repo cwd then cwd else Runtime_config.effective_workspace cfg

let parse_background_add_args args =
  let rec loop model branch positionals = function
    | [] -> (
        let positionals = List.rev positionals in
        match positionals with
        | runner_s :: repo_path :: prompt_parts -> (
            match Background_task.runner_of_string runner_s with
            | None ->
                Error "Runner must be one of: codex, claude (or claude-code)"
            | Some runner ->
                let prompt = String.concat " " prompt_parts |> String.trim in
                if prompt = "" then Error "Prompt is required"
                else Ok { runner; model; repo_path; branch; prompt })
        | _ ->
            Error
              "Usage: clawq background add <codex|claude> [--model <name>] \
               <repo> [--branch <name>] <prompt>")
    | "--model" :: value :: rest -> loop (Some value) branch positionals rest
    | "--branch" :: value :: rest -> loop model (Some value) positionals rest
    | arg :: rest -> loop model branch (arg :: positionals) rest
  in
  loop None None [] args

let parse_background_wait_args args =
  let rec loop timeout id = function
    | [] -> (
        match id with
        | Some id -> Ok { id; timeout_seconds = timeout }
        | None ->
            Error "Usage: clawq background wait <id> [--timeout <seconds>]")
    | "--timeout" :: seconds :: rest -> (
        try loop (float_of_string seconds) id rest
        with _ -> Error "Timeout must be a number")
    | arg :: rest -> (
        match id with
        | Some _ ->
            Error "Usage: clawq background wait <id> [--timeout <seconds>]"
        | None -> (
            try loop timeout (Some (int_of_string arg)) rest
            with _ -> Error "Background task id must be an integer"))
  in
  loop 180.0 None args

let parse_background_logs_args args =
  let rec loop lines follow id = function
    | [] -> (
        match id with
        | Some id -> Ok { id; lines; follow }
        | None ->
            Error
              "Usage: clawq background logs <id> [--lines <count>] [--follow]")
    | "--lines" :: count :: rest -> (
        try loop (max 1 (int_of_string count)) follow id rest
        with _ -> Error "Log line count must be an integer")
    | ("--follow" | "-f") :: rest -> loop lines true id rest
    | arg :: rest -> (
        match id with
        | Some _ ->
            Error
              "Usage: clawq background logs <id> [--lines <count>] [--follow]"
        | None -> (
            try loop lines follow (Some (int_of_string arg)) rest
            with _ -> Error "Background task id must be an integer"))
  in
  loop 40 false None args

let parse_delegate_args args =
  let rec loop preferred_runner model repo_path branch positionals = function
    | [] ->
        let goal = String.concat " " (List.rev positionals) |> String.trim in
        if goal = "" then
          Error
            "Usage: clawq delegate [--runner auto|codex|claude] [--model \
             <name>] [--repo <path>] [--branch <name>] <goal>"
        else Ok { preferred_runner; model; repo_path; branch; goal }
    | "--runner" :: value :: rest ->
        let value = String.lowercase_ascii (String.trim value) in
        let preferred_runner =
          if value = "" || value = "auto" then None
          else Background_task.runner_of_string value
        in
        if value <> "auto" && preferred_runner = None then
          Error "Runner must be one of: auto, codex, claude"
        else loop preferred_runner model repo_path branch positionals rest
    | "--model" :: value :: rest ->
        loop preferred_runner (Some value) repo_path branch positionals rest
    | "--repo" :: value :: rest ->
        loop preferred_runner model (Some value) branch positionals rest
    | "--branch" :: value :: rest ->
        loop preferred_runner model repo_path (Some value) positionals rest
    | arg :: rest ->
        loop preferred_runner model repo_path branch (arg :: positionals) rest
  in
  loop None None None None [] args

let format_background_task_row (task : Background_task.task) =
  let branch = if task.branch = "" then "-" else task.branch in
  Printf.sprintf "  %-4d %-8s %-8s %-18s %s" task.id
    (Background_task.string_of_runner task.runner)
    (Background_task.string_of_status task.status)
    branch task.repo_path

let format_background_task_details (task : Background_task.task) =
  let add line acc = line :: acc in
  let lines = ref [] in
  lines := add (Printf.sprintf "id: %d" task.id) !lines;
  lines :=
    add
      (Printf.sprintf "runner: %s"
         (Background_task.string_of_runner task.runner))
      !lines;
  lines :=
    add
      (Printf.sprintf "status: %s"
         (Background_task.string_of_status task.status))
      !lines;
  lines := add (Printf.sprintf "repo: %s" task.repo_path) !lines;
  lines :=
    add
      (Printf.sprintf "branch: %s"
         (if task.branch = "" then "(auto)" else task.branch))
      !lines;
  lines := add (Printf.sprintf "created_at: %s" task.created_at) !lines;
  (match task.started_at with
  | Some value -> lines := add (Printf.sprintf "started_at: %s" value) !lines
  | None -> ());
  (match task.finished_at with
  | Some value -> lines := add (Printf.sprintf "finished_at: %s" value) !lines
  | None -> ());
  (match task.worktree_path with
  | Some value -> lines := add (Printf.sprintf "worktree: %s" value) !lines
  | None -> ());
  (match task.log_path with
  | Some value -> lines := add (Printf.sprintf "log: %s" value) !lines
  | None -> ());
  (match task.pid with
  | Some value -> lines := add (Printf.sprintf "pid: %d" value) !lines
  | None -> ());
  (match task.result_preview with
  | Some value when String.trim value <> "" ->
      lines := add (Printf.sprintf "result: %s" value) !lines
  | _ -> ());
  lines := add (Printf.sprintf "prompt: %s" task.prompt) !lines;
  String.concat "\n" (List.rev !lines)

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
    let registry =
      match build_tool_registry ~db:(Some (get_db ())) cfg with
      | Some registry -> registry
      | None -> assert false
    in
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
  | [ "codex-login" ] | [ "login"; "codex" ] -> (
      match Openai_codex_oauth.login () with
      | Ok creds ->
          Printf.sprintf "Codex login complete%s"
            (match creds.Runtime_config.email with
            | Some email -> Printf.sprintf " for %s" email
            | None -> "")
      | Error msg -> Printf.sprintf "Codex login failed: %s" msg)
  | [ "codex-login"; provider_name ] -> (
      match Openai_codex_oauth.login ~provider_name () with
      | Ok creds ->
          Printf.sprintf "%s: Codex login complete%s" provider_name
            (match creds.Runtime_config.email with
            | Some email -> Printf.sprintf " for %s" email
            | None -> "")
      | Error msg ->
          Printf.sprintf "%s: Codex login failed: %s" provider_name msg)
  | [ "codex-status" ] | [ "status"; "codex" ] -> Openai_codex_oauth.status ()
  | [ "codex-status"; provider_name ] ->
      Openai_codex_oauth.status ~provider_name ()
  | [ "codex-logout" ] | [ "logout"; "codex" ] -> Openai_codex_oauth.logout ()
  | [ "codex-logout"; provider_name ] ->
      Openai_codex_oauth.logout ~provider_name ()
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
  | "pair" :: rest -> (
      let cfg = get_config () in
      let host = cfg.gateway.host in
      let port = cfg.gateway.port in
      let code =
        match rest with
        | c :: _ -> c
        | [] ->
            print_string "Enter OTP pairing code: ";
            flush stdout;
            input_line stdin
      in
      let url = Printf.sprintf "http://%s:%d/pair" host port in
      let body = `Assoc [ ("code", `String code) ] |> Yojson.Safe.to_string in
      let result =
        Lwt_main.run
          (Lwt.catch
             (fun () ->
               let open Lwt.Syntax in
               let* _status, resp_body =
                 Http_client.post_json ~uri:url ~headers:[] ~body
               in
               Lwt.return (Ok resp_body))
             (fun exn -> Lwt.return (Error (Printexc.to_string exn))))
      in
      match result with
      | Error msg -> Printf.sprintf "Pairing request failed: %s" msg
      | Ok resp_body -> (
          try
            let json = Yojson.Safe.from_string resp_body in
            let open Yojson.Safe.Util in
            match json |> member "token" with
            | `String token ->
                let token_path = gateway_token_path () in
                (try save_gateway_token token
                 with exn ->
                   raise
                     (Failure
                        (Printf.sprintf "Failed to save token: %s"
                           (Printexc.to_string exn))));
                Printf.sprintf
                  "Paired successfully! Token saved to %s\nToken: %s" token_path
                  token
            | _ -> (
                match json |> member "error" with
                | `String err -> Printf.sprintf "Pairing failed: %s" err
                | _ -> Printf.sprintf "Unexpected response: %s" resp_body)
          with exn ->
            Printf.sprintf "Failed to parse response: %s\nBody: %s"
              (Printexc.to_string exn) resp_body))
  | _ -> (
      let cfg = get_config () in
      match cfg.providers with
      | [] -> "No providers configured. No provider auth set."
      | providers ->
          let lines =
            List.map
              (fun (name, (p : Runtime_config.provider_config)) ->
                let auth_status =
                  if Runtime_config.is_key_set p.api_key then
                    redact_key p.api_key
                  else if Runtime_config.provider_has_codex_oauth p then
                    "codex-oauth configured"
                  else "not set"
                in
                Printf.sprintf "  %s: %s" name auth_status)
              providers
          in
          "Provider auth status:\n" ^ String.concat "\n" lines)

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
    let registry =
      match build_tool_registry ~db:(Some (get_db ())) cfg with
      | Some registry -> registry
      | None -> assert false
    in
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

let agent_argv ~executable = [| executable; "agent" |]

let cmd_agent ?(run_daemon = fun ~config -> Lwt_main.run (Daemon.run ~config))
    ?(execv = Unix.execv) ?(acquire_lock = Service.acquire_singleton_lock)
    ?(release_lock = Service.release_singleton_lock) () =
  let cfg = get_config () in
  match acquire_lock () with
  | None ->
      "Another clawq agent instance already holds the daemon lock. Refusing to \
       start a second live agent."
  | Some lock_fd -> (
      let result =
        try run_daemon ~config:cfg
        with Failure msg ->
          print_endline ("Error: " ^ msg);
          release_lock (Some lock_fd);
          exit 1
      in
      match result with
      | Daemon.Shutdown ->
          release_lock (Some lock_fd);
          "Daemon stopped."
      | Daemon.Restart ->
          let executable = Restart_exec.executable () in
          execv executable (agent_argv ~executable);
          "Daemon restart requested.")

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
  | "history" :: name :: _ | "runs" :: name :: _ ->
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
  | [ "runs" ] ->
      let db = get_db () in
      Scheduler.init_schema db;
      let runs = Scheduler.list_runs ~db ~limit:20 () in
      if runs = [] then "No run history."
      else
        let header =
          Printf.sprintf "  %-5s %-15s %-20s %-8s %s" "ID" "JOB" "STARTED"
            "STATUS" "PREVIEW"
        in
        let rows =
          List.map
            (fun (r : Scheduler.run) ->
              Printf.sprintf "  %-5d %-15s %-20s %-8s %s" r.run_id r.job_name
                r.started_at r.status
                (match r.result_preview with
                | Some p -> String.sub p 0 (min 40 (String.length p))
                | None -> ""))
            runs
        in
        "Run history:\n" ^ header ^ "\n" ^ String.concat "\n" rows
  | _ ->
      "Usage: clawq cron <list|add|remove|history|runs>\n\
      \  cron list                                    - List all jobs\n\
      \  cron add <name> <session> <schedule> <msg>   - Add a job\n\
      \  cron remove <name>                           - Remove a job\n\
      \  cron history <name>                          - Show run history\n\
      \  cron runs [name]                             - Show all run history"

let cmd_background args =
  match args with
  | [ "list" ] ->
      let db = get_db () in
      Background_task.init_schema db;
      let tasks, hidden = Background_task.list_tasks_for_display ~db in
      Background_task.format_task_list_with_hidden tasks hidden
  | [] ->
      let db = get_db () in
      Background_task.init_schema db;
      let tasks, hidden = Background_task.list_tasks_for_display ~db in
      let list_output =
        Background_task.format_task_list_with_hidden tasks hidden
      in
      list_output
      ^ "\n\n\
         Commands:\n\
        \  background list                                         - List all \
         tasks\n\
        \  background show <id>                                    - Show task \
         details\n\
        \  background add <codex|claude> <repo> [--branch <name>] <prompt> - \
         Queue a task\n\
        \  background wait <id> [--timeout <seconds>]              - Wait for \
         completion\n\
        \  background logs <id> [--lines <count>] [--follow]       - Show task \
         logs\n\
        \  background cancel <id>                                  - Cancel a \
         task"
  | [ "show"; id_s ] -> (
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: background task id must be an integer"
      else
        match Background_task.get_task ~db ~id with
        | None -> Printf.sprintf "No background task found with id %d" id
        | Some task -> Background_task.format_task_summary task)
  | "add" :: rest -> (
      let cfg = get_config () in
      let db = get_db () in
      Background_task.init_schema db;
      match parse_background_add_args rest with
      | Error msg -> "Error: " ^ msg
      | Ok parsed -> (
          let session_key, channel, channel_id =
            Background_task.routing_from_context ?notify_cfg:cfg.notify ()
          in
          match
            Background_task.enqueue ~db ~runner:parsed.runner
              ?model:parsed.model ~repo_path:parsed.repo_path
              ~prompt:parsed.prompt ?branch:parsed.branch ?session_key ?channel
              ?channel_id ()
          with
          | Ok id ->
              Printf.sprintf
                "Queued background task %d (%s). Use `clawq background wait \
                 %d` or `clawq background show %d` to track it."
                id
                (Background_task.string_of_runner parsed.runner)
                id id
          | Error msg -> "Error: " ^ msg))
  | "wait" :: rest -> (
      let db = get_db () in
      Background_task.init_schema db;
      match parse_background_wait_args rest with
      | Error msg -> "Error: " ^ msg
      | Ok parsed -> (
          let timeout_seconds =
            Float.min parsed.timeout_seconds Background_task.max_wait_seconds
          in
          let result =
            Lwt_main.run
              (Background_task.wait_until_terminal ~timeout_seconds ~db
                 ~id:parsed.id ())
          in
          match result with
          | Background_task.Finished task ->
              Background_task.format_task_summary task
          | Background_task.Timeout task ->
              Printf.sprintf
                "Task %d is still %s after waiting. Run `clawq background wait \
                 %d` to continue waiting, or `clawq background logs %d` to \
                 check progress.\n\n\
                 %s"
                parsed.id
                (Background_task.string_of_status task.status)
                parsed.id parsed.id
                (Background_task.format_task_summary task)
          | Background_task.Not_found ->
              Printf.sprintf "Error: No background task found with id %d"
                parsed.id))
  | "logs" :: rest -> (
      let db = get_db () in
      Background_task.init_schema db;
      match parse_background_logs_args rest with
      | Error msg -> "Error: " ^ msg
      | Ok parsed when parsed.follow -> (
          let result =
            Lwt_main.run
              (Background_task.log_follow ~db ~id:parsed.id
                 ~initial_lines:parsed.lines ())
          in
          match result with Ok () -> "" | Error msg -> "Error: " ^ msg)
      | Ok parsed -> (
          match Background_task.get_task ~db ~id:parsed.id with
          | None ->
              Printf.sprintf "Error: No background task found with id %d"
                parsed.id
          | Some task -> (
              match Background_task.log_excerpt ~lines:parsed.lines task with
              | Ok text -> text
              | Error msg -> "Error: " ^ msg)))
  | [ "cancel"; id_s ] -> (
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: background task id must be an integer"
      else
        match Background_task.cancel ~db ~id with
        | Ok msg -> msg
        | Error msg -> "Error: " ^ msg)
  | _ ->
      "Usage: clawq background <list|show|add|wait|logs|cancel>\n\
      \  background list                                         - List queued \
       and completed tasks\n\
      \  background show <id>                                    - Show task \
       details\n\
      \  background add <codex|claude> <repo> [--branch <name>] <prompt> - \
       Queue a worktree runner\n\
      \  background wait <id> [--timeout <seconds>]              - Wait for a \
       task to finish\n\
      \  background logs <id> [--lines <count>] [--follow]       - Show recent \
       task log lines\n\
      \  background cancel <id>                                  - Cancel a \
       queued/running task"

let cmd_delegate args =
  let cfg = get_config () in
  let db = get_db () in
  Background_task.init_schema db;
  match parse_delegate_args args with
  | Error msg -> "Error: " ^ msg
  | Ok parsed -> (
      match
        Background_task.delegate_enqueue ~db ?notify_cfg:cfg.notify
          ?preferred_runner:parsed.preferred_runner ?model:parsed.model
          ?repo_path:parsed.repo_path ?branch:parsed.branch
          ~default_repo_path:(default_delegate_repo_path cfg)
          ~goal:parsed.goal ()
      with
      | Ok (id, runner, repo_path) ->
          Printf.sprintf
            "Delegated task %d (%s) for %s. Use `clawq background wait %d` or \
             `clawq background show %d` to track it."
            id
            (Background_task.string_of_runner runner)
            repo_path id id
      | Error msg -> "Error: " ^ msg)

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
            let anchor = Audit.get_chain_anchor ~db in
            let signed_entries, unsigned_entries = Audit.signature_counts ~db in
            match Audit.verify_chain ~db ~key with
            | Ok () -> (
                match (anchor, signed_entries, unsigned_entries) with
                | Some _, 0, _ ->
                    "Audit chain verification: OK (no signed entries; stored \
                     retained-chain anchor not exercised)"
                | Some _, _, n when n > 0 ->
                    "Audit chain verification: OK (signed retained suffix \
                     verified against anchor; unsigned entries are \
                     informational only)"
                | Some _, _, _ ->
                    "Audit chain verification: OK (signed retained suffix \
                     verified against anchor)"
                | None, _, _ -> "Audit chain verification: OK")
            | Error (id, reason) ->
                Printf.sprintf "Audit chain verification FAILED at entry %d: %s"
                  id reason))
    | [ "export" ] ->
        let path = cfg.security.audit_retention.export_path in
        let export_file = Filename.concat path "audit_export.jsonl" in
        let count = Audit.export_json ~db ~path:export_file in
        Printf.sprintf "Exported %d audit entries to %s (anchor sidecar: %s)"
          count export_file
          (export_file ^ ".anchor.json")
    | [ "export"; path ] ->
        let count = Audit.export_json ~db ~path in
        Printf.sprintf "Exported %d audit entries to %s (anchor sidecar: %s)"
          count path (path ^ ".anchor.json")
    | [ "import"; path ] -> (
        match Audit.import_json ~db ~path () with
        | Ok (count, Some anchor_path) ->
            Printf.sprintf
              "Imported %d audit entries from %s (anchor sidecar: %s)" count
              path anchor_path
        | Ok (count, None) ->
            Printf.sprintf "Imported %d audit entries from %s" count path
        | Error msg -> Printf.sprintf "Error: %s" msg)
    | [ "import"; path; "--anchor"; anchor_path ] -> (
        match Audit.import_json ~db ~path ~anchor_path () with
        | Ok (count, Some used_anchor) ->
            Printf.sprintf "Imported %d audit entries from %s (anchor: %s)"
              count path used_anchor
        | Ok (count, None) ->
            Printf.sprintf "Imported %d audit entries from %s" count path
        | Error msg -> Printf.sprintf "Error: %s" msg)
    | [ "purge" ] ->
        let ret = cfg.security.audit_retention in
        let deleted =
          Audit.purge_old ~db ~max_age_days:ret.max_age_days
            ~max_entries:ret.max_entries
        in
        Printf.sprintf
          "Purged %d audit entries while preserving a contiguous retained \
           suffix"
          deleted
    | _ ->
        "Usage: clawq audit <list|list --limit N|verify|export [path]|import \
         PATH [--anchor PATH.anchor.json]|purge>"

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
  | [ "signal-restart" ] -> Service.cmd_signal_restart ()
  | [ "restart" ] ->
      let cfg = get_config () in
      Service.cmd_restart ~config:cfg
  | _ -> "Usage: clawq service <start|stop|status|signal-restart|restart>"

let parse_update_args args =
  match args with
  | [] -> Ok Update_tool.Auto
  | [ "--mode"; value ] -> (
      match
        Update_tool.update_mode_of_string
          (String.lowercase_ascii (String.trim value))
      with
      | Some mode -> Ok mode
      | None ->
          Error
            (Printf.sprintf
               "Invalid update mode '%s'. Use: clawq update [--mode \
                auto|git|binary]"
               value))
  | _ -> Error "Usage: clawq update [--mode auto|git|binary]"

let render_update_output ~progress ~result =
  let progress = List.filter (fun line -> String.trim line <> "") progress in
  match List.rev progress with
  | last :: _ when last = result -> String.concat "\n" progress
  | _ -> String.concat "\n" (progress @ [ result ])

let offline_update_stub mode =
  let progress = ref [] in
  let send_progress text =
    progress := text :: !progress;
    Lwt.return_unit
  in
  let result =
    Lwt_main.run (Update_tool.run_offline_update ~mode ~send_progress ())
  in
  let progress_lines =
    List.rev !progress |> List.filter (fun s -> String.trim s <> "")
  in
  render_update_output ~progress:progress_lines ~result

let cmd_update args =
  match parse_update_args args with
  | Error msg -> msg
  | Ok mode -> (
      match read_live_daemon_gateway () with
      | None -> offline_update_stub mode
      | Some (host, port) -> (
          let cfg = get_config () in
          let body =
            Yojson.Safe.to_string
              (`Assoc
                 [ ("mode", `String (Update_tool.string_of_update_mode mode)) ])
          in
          let result =
            post_live_gateway_json ~cfg ~host ~port ~path:"/daemon/update" ~body
          in
          match result with
          | Error msg -> Printf.sprintf "Update request failed: %s" msg
          | Ok (status, resp_body) -> (
              match status with
              | 200 -> (
                  try
                    let json = Yojson.Safe.from_string resp_body in
                    let open Yojson.Safe.Util in
                    let progress =
                      json |> member "progress" |> to_list |> List.map to_string
                    in
                    let result = json |> member "result" |> to_string in
                    render_update_output ~progress ~result
                  with _ ->
                    Printf.sprintf
                      "Update request succeeded but returned an unexpected \
                       response: %s"
                      resp_body)
              | 401 | 403 ->
                  Printf.sprintf
                    "Update request was rejected by the live gateway (%d): %s"
                    status
                    (match parse_json_error_body resp_body with
                    | Some msg -> msg
                    | None -> resp_body)
              | _ ->
                  Printf.sprintf "Update request failed (%d): %s" status
                    (match parse_json_error_body resp_body with
                    | Some msg -> msg
                    | None -> resp_body))))

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
  let cfg = get_config () in
  let provider_name = cfg.tunnel.provider in
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
          ("provider", `String provider_name);
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
  if not cfg.tunnel.enabled then
    "Tunnel is disabled in config (set tunnel.enabled=true to use)"
  else
    let process_needle =
      match provider_name with
      | "cloudflare" | "cf" -> "cloudflared"
      | "tailscale" -> "tailscale"
      | "ngrok" -> "ngrok"
      | _ -> provider_name
    in
    let tunnel_pid_matches ~pid ~start_ticks =
      if not (pid_is_alive pid) then false
      else if not (proc_cmdline_contains ~needle:process_needle pid) then false
      else
        match (start_ticks, proc_start_ticks pid) with
        | Some expected, Some actual -> expected = actual
        | _ -> true
    in
    (* Generic tunnel operations using first-class module-like dispatch *)
    let tunnel_start () =
      match provider_name with
      | p when p = Tunnel_cloudflare.name || p = "cf" ->
          let t =
            Tunnel_cloudflare.create ~port:cfg.gateway.port ~config:cfg.tunnel
          in
          Lwt_main.run (Tunnel_cloudflare.start t);
          (Tunnel_cloudflare.get_pid t, Tunnel_cloudflare.get_url t)
      | p when p = Tunnel_tailscale.name ->
          let t =
            Tunnel_tailscale.create ~port:cfg.gateway.port ~config:cfg.tunnel
          in
          Lwt_main.run (Tunnel_tailscale.start t);
          (Tunnel_tailscale.get_pid t, Tunnel_tailscale.get_url t)
      | p when p = Tunnel_ngrok.name ->
          let t =
            Tunnel_ngrok.create ~port:cfg.gateway.port ~config:cfg.tunnel
          in
          Lwt_main.run (Tunnel_ngrok.start t);
          (Tunnel_ngrok.get_pid t, Tunnel_ngrok.get_url t)
      | p when p = Tunnel_custom.name ->
          let custom_command =
            try Sys.getenv "CLAWQ_TUNNEL_COMMAND" with Not_found -> ""
          in
          if custom_command = "" then begin
            Printf.eprintf
              "Custom tunnel requires CLAWQ_TUNNEL_COMMAND env var\n";
            (None, None)
          end
          else
            let t =
              Tunnel_custom.create ~port:cfg.gateway.port ~config:cfg.tunnel
                ~custom_command
                ~url_regex:
                  (try Sys.getenv "CLAWQ_TUNNEL_URL_REGEX"
                   with Not_found -> "https://[a-zA-Z0-9._/-]+")
            in
            Lwt_main.run (Tunnel_custom.start t);
            (Tunnel_custom.get_pid t, Tunnel_custom.get_url t)
      | _ ->
          Printf.eprintf "Unknown tunnel provider: %s\n" provider_name;
          (None, None)
    in
    match args with
    | [ "start" ] -> (
        let pid_url = tunnel_start () in
        match pid_url with
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
              provider_name
        | Some (pid, url, start_ticks) ->
            let running = tunnel_pid_matches ~pid ~start_ticks in
            if running then
              Printf.sprintf
                "Tunnel provider: %s\n  Status: running (pid %d)\n  URL: %s"
                provider_name pid url
            else begin
              remove_tunnel_state ();
              Printf.sprintf
                "Tunnel provider: %s\n  Status: stopped (stale state cleaned)"
                provider_name
            end)
    | [ "apply" ] -> Lwt_main.run (!Tunnel_manager.daemon_apply_fn ())
    | [ "restart" ] -> Lwt_main.run (!Tunnel_manager.daemon_restart_fn ())
    | [ "daemon-status" ] -> !Tunnel_manager.daemon_status_fn ()
    | _ -> "Usage: clawq tunnel <start|stop|status|apply|restart|daemon-status>"

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

let cmd_reset_workspace () =
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
  print_endline (red "  !! RESET WORKSPACE !!");
  print_endline "";
  print_endline "  This will permanently delete:";
  print_endline
    ("    "
    ^ bold "· All conversation history  "
    ^ dim ("(" ^ db_path ^ " — messages, embeddings)"));
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
  print_endline (dim "    · cron jobs and run logs");
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
    print_endline "    · Workspace files redeployed from defaults";
    print_endline "";
    "Workspace reset complete."
  end

let cmd_otp_show () =
  let cfg = get_config () in
  let lines = ref [] in
  let add line = lines := line :: !lines in
  (match read_live_gateway_pairing_code () with
  | Some code -> add (Printf.sprintf "  gateway: %s" code)
  | None -> ());
  (match cfg.channels.telegram with
  | None -> ()
  | Some tg ->
      List.iter
        (fun (name, (acct : Runtime_config.telegram_account)) ->
          match acct.totp with
          | Some t when t.totp_enabled && t.totp_secret <> "" ->
              let time = Unix.gettimeofday () in
              let code = Totp.generate_totp ~secret:t.totp_secret ~time in
              let remaining = Totp.time_remaining ~time in
              add
                (Printf.sprintf "  telegram/%s: %s (expires in %ds)" name code
                   remaining)
          | _ -> ())
        tg.accounts);
  let results = List.rev !lines in
  if results <> [] then "Current pairing codes:\n" ^ String.concat "\n" results
  else if cfg.gateway.require_pairing then
    "No live gateway pairing code found. Start `clawq agent` and rerun `clawq \
     otp-show`, or configure Telegram TOTP pairing."
  else
    "Pairing is not configured. Enable `gateway.require_pairing` or configure \
     Telegram TOTP pairing."

let debug_html_preview_pages =
  [
    ( "/",
      Html_page.render ~title:"Index" ~extra_css:""
        ~body_html:
          {|<h1>Html_page Preview</h1>
  <span class="label label-ok">index</span>
  <p><a href="/ok">Auth success page</a></p>
  <p><a href="/error">Auth error page</a></p>
  <p><a href="/custom">Custom content</a></p>
  <div class="qed">&#9632;</div>|}
    );
    ("/ok", Openai_codex_oauth.callback_page_ok);
    ( "/error",
      Openai_codex_oauth.callback_page_error "State Mismatch"
        "The OAuth state parameter did not match. Please retry the login flow."
    );
    ( "/custom",
      Html_page.render ~title:"Custom Example" ~extra_css:""
        ~body_html:
          {|<h1>Custom Page</h1>
  <span class="label label-ok">example</span>
  <p>This demonstrates using Html_page.render with arbitrary content.</p>
  <p><a href="/">Back to index</a></p>
  <div class="qed">&#9632;</div>|}
    );
  ]

let cmd_debug args =
  match args with
  | "prompt" :: rest -> cmd_debug_prompt rest
  | [ "html-preview" ] | [ "html-preview"; _ ] ->
      let port =
        match args with
        | [ _; p ] -> ( try int_of_string p with _ -> 8099)
        | _ -> 8099
      in
      Printf.printf "Serving Html_page preview on http://localhost:%d\n%!" port;
      Printf.printf "Pages: /  /ok  /error  /custom\n%!";
      Printf.printf "Press Ctrl-C to stop.\n%!";
      let _ : string =
        Lwt_main.run
          (let open Lwt.Syntax in
           let* _server =
             Lwt_io.establish_server_with_client_address
               (Unix.ADDR_INET (Unix.inet_addr_loopback, port))
               (fun _addr (ic, oc) ->
                 Lwt.catch
                   (fun () ->
                     let* request_line = Lwt_io.read_line ic in
                     let path =
                       match String.split_on_char ' ' request_line with
                       | _ :: target :: _ -> target
                       | _ -> "/"
                     in
                     let body =
                       match List.assoc_opt path debug_html_preview_pages with
                       | Some page -> page
                       | None ->
                           Html_page.render ~title:"Not Found" ~extra_css:""
                             ~body_html:
                               (Printf.sprintf
                                  {|<h1>Not Found</h1>
  <span class="label label-error">404</span>
  <p>No page at <code>%s</code>.</p>
  <p><a href="/">Back to index</a></p>
  <div class="qed">&#9632;</div>|}
                                  path)
                     in
                     let* () =
                       Lwt_io.write oc
                         (Printf.sprintf
                            "HTTP/1.1 200 OK\r\n\
                             Content-Type: text/html; charset=utf-8\r\n\
                             Content-Length: %d\r\n\
                             Connection: close\r\n\
                             \r\n\
                             %s"
                            (String.length body) body)
                     in
                     Lwt_io.flush oc)
                   (fun _exn -> Lwt.return_unit))
           in
           let waiter, _wakener = Lwt.wait () in
           let* () = waiter in
           Lwt.return "debug html-preview: stopped")
      in
      "debug html-preview: stopped"
  | _ ->
      "Usage: clawq debug html-preview [PORT]\n\
       Serves Html_page test pages on localhost (default port 8099).\n\n\
       Usage: clawq debug prompt [MESSAGE]\n\
       Prints the normalized logical messages for a single agent turn."

let handle args =
  match args with
  | "phase2" :: _ -> Phase2.render ()
  | "agent" :: _ -> cmd_agent ()
  | "status" :: _ -> cmd_status ()
  | "config" :: rest -> cmd_config rest
  | "doctor" :: _ -> cmd_doctor ()
  | "onboard" :: _ -> cmd_onboard ()
  | "models" :: _ -> cmd_models ()
  | "channel" :: _ -> cmd_channel ()
  | "memory" :: rest -> cmd_memory rest
  | "session" :: rest -> cmd_session rest
  | "workspace" :: _ -> cmd_workspace ()
  | "capabilities" :: _ -> cmd_capabilities ()
  | "auth" :: rest -> cmd_auth rest
  | "transcribe" :: rest -> cmd_transcribe rest
  | "mcp" :: _ -> cmd_mcp ()
  | "cron" :: rest -> cmd_cron rest
  | "background" :: rest -> cmd_background rest
  | "delegate" :: rest -> cmd_delegate rest
  | "skills" :: rest -> cmd_skills rest
  | "audit" :: rest -> cmd_audit rest
  | "runtime" :: rest -> cmd_runtime rest
  | "tunnel" :: rest -> cmd_tunnel rest
  | "update" :: rest -> cmd_update rest
  | "hardware" :: _ -> "hardware: deferred to Phase 2"
  | "migrate" :: rest -> Migrate.cmd_migrate rest
  | "service" :: rest -> cmd_service rest
  | "reset-agent" :: _ -> cmd_reset_agent ()
  | "reset-workspace" :: _ -> cmd_reset_workspace ()
  | "otp-show" :: _ -> cmd_otp_show ()
  | "debug" :: rest -> cmd_debug rest
  | _ -> Clawq_core.dispatch args

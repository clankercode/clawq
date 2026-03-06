open Cmdliner

let unescape_newlines s =
  let len = String.length s in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    if !i + 1 < len && s.[!i] = '\\' && s.[!i + 1] = 'n' then begin
      Buffer.add_char buf '\n';
      i := !i + 2
    end
    else begin
      Buffer.add_char buf s.[!i];
      i := !i + 1
    end
  done;
  Buffer.contents buf

let run name args =
  print_string (unescape_newlines (Command_bridge.handle (name :: args)));
  `Ok ()

let rest_args docv = Arg.(value & pos_all string [] & info [] ~docv)

(* Commands with no meaningful positional args *)
let simple name doc =
  Cmd.v (Cmd.info name ~doc) Term.(ret (const (run name) $ const []))

(* Commands that forward remaining positional args to command_bridge *)
let with_args name doc man =
  let args = rest_args "ARGS" in
  Cmd.v (Cmd.info name ~doc ~man) Term.(ret (const (run name) $ args))

let agent_cmd =
  simple "agent"
    "Start the clawq daemon (agent loop, gateway, and all configured channels)."

let status_cmd = simple "status" "Show runtime configuration and daemon status."

let doctor_cmd =
  simple "doctor" "Check configuration for common issues and misconfigurations."

let onboard_cmd =
  simple "onboard"
    "Create a starter config file at ~/.clawq/config.json if none exists."

let models_cmd =
  simple "models" "List configured LLM providers and their default models."

let channel_cmd =
  simple "channel" "List configured channels (CLI, Telegram, Discord, Slack)."

let memory_cmd = simple "memory" "Show memory backend configuration."
let workspace_cmd = simple "workspace" "Print the current workspace directory."

let capabilities_cmd =
  simple "capabilities"
    "List all active runtime capabilities (providers, channels, tools, etc.)."

let mcp_cmd =
  simple "mcp"
    "Start the MCP server (exposes configured tools over the Model Context \
     Protocol)."

let config_cmd =
  with_args "config" "View or modify clawq configuration."
    [
      `S "SUBCOMMANDS";
      `I ("wizard", "Interactive configuration wizard (TUI).");
      `I ("set KEY VALUE", "Set a config value by dot-path.");
      `I ("get KEY", "Get a config value by dot-path.");
      `I ("show [SECTION]", "Display current config (secrets redacted).");
    ]

let phase2_cmd = simple "phase2" "Show Phase 2 feature status."

let hardware_cmd =
  simple "hardware" "Hardware integration (deferred to Phase 2)."

let auth_cmd =
  with_args "auth"
    "Manage provider authentication, including Codex subscription login."
    [
      `S "SUBCOMMANDS";
      `I ("(no args)", "Print redacted provider auth status for all providers.");
      `I
        ("encrypt", "Encrypt plaintext API keys in config using the master key.");
      `I ("codex-login [PROVIDER]", "Start ChatGPT/Codex OAuth login flow.");
      `I ("codex-status [PROVIDER]", "Show stored Codex OAuth status.");
      `I ("codex-logout [PROVIDER]", "Clear stored Codex OAuth credentials.");
    ]

let transcribe_cmd =
  let file =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"FILE" ~doc:"Audio file to transcribe.")
  in
  let info =
    Cmd.info "transcribe"
      ~doc:"Transcribe an audio file using the configured STT provider."
  in
  Cmd.v info Term.(ret (const (fun f -> run "transcribe" [ f ]) $ file))

let cron_cmd =
  with_args "cron" "Manage cron jobs for scheduled agent messages."
    [
      `S "SUBCOMMANDS";
      `I ("list", "List all configured cron jobs (default).");
      `I
        ( "add NAME SESSION SCHEDULE MSG",
          "Add a cron job. SCHEDULE is a cron expression (e.g. \"0 9 * * \
           1-5\")." );
      `I ("remove NAME", "Remove a cron job by name.");
      `I ("history NAME", "Show the last 10 run records for a job.");
    ]

let audit_cmd =
  with_args "audit" "View and manage the security audit log."
    [
      `S "SUBCOMMANDS";
      `I ("list", "Show the 20 most recent audit entries (default).");
      `I ("list --limit N", "Show the N most recent entries.");
      `I ("verify", "Verify the cryptographic chain of the audit log.");
      `I
        ( "export [PATH]",
          "Export all entries as JSONL. Defaults to the path in config." );
      `I ("purge", "Delete old entries per the retention policy in config.");
    ]

let skills_cmd =
  with_args "skills" "Manage agent skills (shell-script tool extensions)."
    [
      `S "SUBCOMMANDS";
      `I ("list", "List available skills (default).");
      `I ("path", "Print the skills directory path.");
      `I ("init", "Create an example skill file in the skills directory.");
    ]

let service_cmd =
  with_args "service" "Manage the clawq system service (start/stop/restart)."
    [
      `S "SUBCOMMANDS";
      `I ("start", "Start the clawq service.");
      `I ("stop", "Stop the clawq service.");
      `I ("restart", "Restart the clawq service.");
      `I ("status", "Show service status (default).");
    ]

let runtime_cmd =
  with_args "runtime" "Manage native and Docker runtimes for the clawq daemon."
    [
      `S "SUBCOMMANDS";
      `I ("status", "Show status of native and Docker runtimes (default).");
      `I
        ( "native start|stop|health",
          "Control or health-check the native runtime." );
      `I
        ( "docker start|stop|health",
          "Control or health-check the Docker runtime." );
    ]

let tunnel_cmd =
  with_args "tunnel"
    "Manage a public tunnel to the local gateway (Cloudflare supported)."
    [
      `S "SUBCOMMANDS";
      `I ("start", "Start the tunnel.");
      `I ("stop", "Stop the tunnel.");
      `I ("status", "Show tunnel status (default).");
    ]

let migrate_cmd = with_args "migrate" "Run database migrations." []

let reset_agent_cmd =
  simple "reset-agent"
    "Wipe all session history, cron jobs, and workspace files, then redeploy \
     workspace defaults. Prompts for confirmation before acting. Does NOT \
     touch config.json."

let main_info =
  Cmd.info "clawq" ~version:"0.1.0-dev" ~doc:"Coq-first AI assistant runtime"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "clawq is a modular AI assistant runtime with an agent loop, \
           multi-channel support (CLI, Telegram, Discord, Slack), an HTTP \
           gateway, cron scheduling, audit logging, and MCP server.";
        `P "Run $(b,clawq COMMAND --help) for per-command usage.";
      ]

(* Clear MANPAGER so Cmdliner's pager selection does not pick up user-defined
   pipelines (e.g. col -bx | bat) that strip ANSI escape sequences from groff
   output and render help as raw escape codes. *)
let help_env var = match var with "MANPAGER" -> None | _ -> Sys.getenv_opt var

let () =
  let cmds =
    [
      config_cmd;
      agent_cmd;
      status_cmd;
      doctor_cmd;
      onboard_cmd;
      models_cmd;
      channel_cmd;
      memory_cmd;
      workspace_cmd;
      capabilities_cmd;
      auth_cmd;
      transcribe_cmd;
      mcp_cmd;
      cron_cmd;
      audit_cmd;
      skills_cmd;
      service_cmd;
      runtime_cmd;
      tunnel_cmd;
      migrate_cmd;
      reset_agent_cmd;
      phase2_cmd;
      hardware_cmd;
    ]
  in
  exit (Cmd.eval ~env:help_env (Cmd.group main_info cmds))

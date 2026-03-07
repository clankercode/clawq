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
  print_string (unescape_newlines (Command_bridge_min.handle (name :: args)));
  `Ok ()

let rest_args docv = Arg.(value & pos_all string [] & info [] ~docv)

let simple name doc =
  Cmd.v (Cmd.info name ~doc) Term.(ret (const (run name) $ const []))

let with_args name doc man =
  let args = rest_args "ARGS" in
  Cmd.v (Cmd.info name ~doc ~man) Term.(ret (const (run name) $ args))

let status_cmd = simple "status" "Show runtime configuration summary."
let doctor_cmd = simple "doctor" "Check configuration for common issues."

let onboard_cmd =
  simple "onboard"
    "Create a starter config file at ~/.clawq/config.json if none exists."

let models_cmd =
  simple "models" "List configured LLM providers and their default models."

let channel_cmd = simple "channel" "List configured channels."
let memory_cmd = simple "memory" "Show memory backend configuration."
let workspace_cmd = simple "workspace" "Print the current workspace directory."
let capabilities_cmd = simple "capabilities" "List active runtime capabilities."
let phase2_cmd = simple "phase2" "Show Phase 2 feature status."

let auth_cmd =
  with_args "auth"
    "Show provider auth status or encrypt plaintext secrets in config."
    [
      `S "SUBCOMMANDS";
      `I ("(no args)", "Print redacted auth status for all providers.");
      `I ("encrypt", "Encrypt plaintext secrets in config using the master key.");
      `I
        ( "codex-login [PROVIDER]",
          "Disabled in minimal build; use full clawq binary for Codex OAuth." );
      `I
        ( "codex-status [PROVIDER]",
          "Disabled in minimal build; use full clawq binary for Codex OAuth." );
      `I
        ( "codex-logout [PROVIDER]",
          "Disabled in minimal build; use full clawq binary for Codex OAuth." );
    ]

let cron_cmd =
  with_args "cron" "Manage cron jobs for scheduled agent messages."
    [
      `S "SUBCOMMANDS";
      `I ("list", "List all configured cron jobs (default).");
      `I ("add NAME SESSION SCHEDULE MSG", "Add a cron job.");
      `I ("remove NAME", "Remove a cron job by name.");
    ]

let audit_cmd =
  with_args "audit" "View the security audit log."
    [
      `S "SUBCOMMANDS";
      `I ("list", "Show the 20 most recent audit entries (default).");
    ]

let skills_cmd =
  with_args "skills" "Manage agent skills."
    [
      `S "SUBCOMMANDS";
      `I ("list", "List available skills (default).");
      `I ("path", "Print the skills directory path.");
      `I ("init", "Create an example skill file.");
    ]

let migrate_cmd = with_args "migrate" "Run database migrations." []

(* Disabled-in-minimal stubs — shown in help but print a clear message at runtime *)
let disabled name doc =
  simple name (doc ^ " (disabled in minimal build; use full clawq binary).")

let agent_cmd = disabled "agent" "Start the clawq daemon"
let mcp_cmd = disabled "mcp" "Start the MCP server"
let transcribe_cmd = disabled "transcribe" "Transcribe an audio file"
let runtime_cmd = disabled "runtime" "Manage native and Docker runtimes"
let tunnel_cmd = disabled "tunnel" "Manage public tunnel"
let service_cmd = disabled "service" "Manage the clawq system service"
let hardware_cmd = disabled "hardware" "Hardware integration (deferred)"
let otp_show_cmd = disabled "otp-show" "Show pairing codes"

let main_info =
  Cmd.info "clawq-min" ~version:"0.1.0-dev"
    ~doc:"Minimal clawq CLI (core-only, no network integrations)"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "clawq-min is the core-only build of clawq. Network integrations \
           (daemon, gateway, Telegram, Discord, Slack, MCP) are disabled. Use \
           the full $(b,clawq) binary for those features.";
        `P "Run $(b,clawq-min COMMAND --help) for per-command usage.";
      ]

(* Clear MANPAGER so Cmdliner's pager selection does not pick up user-defined
   pipelines (e.g. col -bx | bat) that strip ANSI escape sequences from groff
   output and render help as raw escape codes. *)
let help_env var = match var with "MANPAGER" -> None | _ -> Sys.getenv_opt var

let () =
  let cmds =
    [
      status_cmd;
      doctor_cmd;
      onboard_cmd;
      models_cmd;
      channel_cmd;
      memory_cmd;
      workspace_cmd;
      capabilities_cmd;
      auth_cmd;
      cron_cmd;
      audit_cmd;
      skills_cmd;
      migrate_cmd;
      phase2_cmd;
      agent_cmd;
      mcp_cmd;
      transcribe_cmd;
      runtime_cmd;
      tunnel_cmd;
      service_cmd;
      otp_show_cmd;
      hardware_cmd;
    ]
  in
  exit (Cmd.eval ~env:help_env (Cmd.group main_info cmds))

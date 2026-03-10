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
  let result = unescape_newlines (Command_bridge.handle (name :: args)) in
  if Cli_exit.should_error ~name ~args ~result then `Error (false, result)
  else begin
    print_string result;
    `Ok ()
  end

let rest_args docv = Arg.(value & pos_all string [] & info [] ~docv)

let required_rest_args docv =
  Arg.(non_empty & pos_all string [] & info [] ~docv)

let required_trailing_args start docv =
  Arg.(non_empty & pos_right start string [] & info [] ~docv)

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
  with_args "models" "List known models and set default model."
    [
      `S "SUBCOMMANDS";
      `I
        ( "list [--provider P]",
          "List known models from the catalog (optionally filter by provider)."
        );
      `I
        ( "set-default MODEL",
          "Set default model (e.g. anthropic/claude-sonnet-4-6)." );
    ]

let usage_cmd =
  with_args "usage" "Show provider quota/usage status."
    [
      `S "OPTIONS";
      `I
        ( "--refresh, -r",
          "Force refresh quota data from all configured providers (uses cache \
           if < TTL)." );
    ]

let provider_cmd =
  with_args "provider"
    "Inspect LLM provider configuration and live quota state."
    [
      `S "SUBCOMMANDS";
      `I
        ( "quota [NAME]",
          "Fetch and display live quota/usage for all providers, or a single \
           named provider." );
      `I ("list", "List configured providers (same as 'models').");
    ]

let channel_cmd =
  simple "channel" "List configured channels (CLI, Telegram, Discord, Slack)."

let memory_cmd = simple "memory" "Show memory backend configuration."

let session_cmd =
  with_args "session" "Inspect persisted sessions and raw chat log epochs."
    [
      `S "SUBCOMMANDS";
      `I
        ( "list [--channel NAME] [--prefix PREFIX] [--active|--inactive] \
           [--main|--non-main]",
          "List persisted sessions with optional filters." );
      `I ("epochs SESSION", "List the current and archived chat-log epochs.");
      `I
        ( "show SESSION [--epoch current|ID] [--offset N] [--limit N]",
          "Print the raw chat log for the current or a specific archived \
           epoch. Use --offset and --limit to page through long message \
           histories." );
      `I
        ( "inject SESSION MESSAGE...",
          "Inject a live inbound message through the daemon session manager." );
      `I
        ( "events SESSION [--epoch current|ID] [--type TYPE]",
          "Show event, system, and compaction messages for a session. --type \
           filters to a specific event type: workspace_refresh, unknown_event, \
           memory_context, attachment, compaction." );
      `I
        ( "compact SESSION",
          "Compact session history by summarizing older messages to free up \
           context space." );
    ]

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

let background_list_cmd =
  Cmd.v
    (Cmd.info "list"
       ~doc:"List queued, running, and completed background tasks.")
    Term.(ret (const (run "background") $ const [ "list" ]))

let background_show_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  Cmd.v
    (Cmd.info "show"
       ~doc:"Show detailed task status, including worktree and log paths.")
    Term.(ret (const (fun id -> run "background" [ "show"; id ]) $ id))

let background_add_cmd =
  let runner =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"RUNNER")
  in
  let repo = Arg.(required & pos 1 (some string) None & info [] ~docv:"REPO") in
  let model =
    Arg.(
      value
      & opt (some string) None
      & info [ "model" ] ~docv:"MODEL"
          ~doc:"Explicit runner model to use when supported.")
  in
  let branch =
    Arg.(
      value
      & opt (some string) None
      & info [ "branch" ] ~docv:"NAME" ~doc:"Branch name for the new worktree.")
  in
  let prompt = required_trailing_args 1 "PROMPT" in
  Cmd.v
    (Cmd.info "add" ~doc:"Queue a background coding task for a repository.")
    Term.(
      ret
        (const (fun runner repo model branch prompt ->
             let args = [ "add"; runner; repo ] in
             let args =
               match model with
               | Some value -> args @ [ "--model"; value ]
               | None -> args
             in
             let args =
               match branch with
               | Some name -> args @ [ "--branch"; name ]
               | None -> args
             in
             run "background" (args @ prompt))
        $ runner $ repo $ model $ branch $ prompt))

let background_wait_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  let timeout =
    Arg.(
      value
      & opt (some string) None
      & info [ "timeout" ] ~docv:"SECONDS"
          ~doc:"Maximum number of seconds to wait.")
  in
  Cmd.v
    (Cmd.info "wait"
       ~doc:"Wait for a task to finish and print its final status.")
    Term.(
      ret
        (const (fun id timeout ->
             let args = [ "wait"; id ] in
             let args =
               match timeout with
               | Some seconds -> args @ [ "--timeout"; seconds ]
               | None -> args
             in
             run "background" args)
        $ id $ timeout))

let background_logs_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  let lines =
    Arg.(
      value
      & opt (some string) None
      & info [ "lines" ] ~docv:"COUNT"
          ~doc:"How many trailing log lines to show.")
  in
  let offset =
    Arg.(
      value
      & opt (some string) None
      & info [ "offset" ] ~docv:"LINE"
          ~doc:"1-indexed line number to start from (paged mode).")
  in
  let follow =
    Arg.(
      value & flag
      & info [ "follow"; "f" ]
          ~doc:"Follow the log output, streaming new lines until the task ends.")
  in
  Cmd.v
    (Cmd.info "logs"
       ~doc:"Show the task log output for a queued, running, or finished task.")
    Term.(
      ret
        (const (fun id lines offset follow ->
             let args = [ "logs"; id ] in
             let args =
               match lines with
               | Some count -> args @ [ "--lines"; count ]
               | None -> args
             in
             let args =
               match offset with
               | Some off -> args @ [ "--offset"; off ]
               | None -> args
             in
             let args = if follow then args @ [ "--follow" ] else args in
             run "background" args)
        $ id $ lines $ offset $ follow))

let background_cancel_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  Cmd.v
    (Cmd.info "cancel" ~doc:"Cancel a queued or running background task.")
    Term.(ret (const (fun id -> run "background" [ "cancel"; id ]) $ id))

let background_cmd =
  Cmd.group
    ~default:Term.(ret (const (run "background") $ const []))
    (Cmd.info "background"
       ~doc:
         "Manage background coding tasks that run a coding agent in git \
          worktrees.")
    [
      background_list_cmd;
      background_show_cmd;
      background_add_cmd;
      background_wait_cmd;
      background_logs_cmd;
      background_cancel_cmd;
    ]

let delegate_cmd =
  let runner =
    Arg.(
      value
      & opt (some string) None
      & info [ "runner" ] ~docv:"RUNNER"
          ~doc:
            "Preferred runner: auto (kimi → cursor → \
             opencode/zai-coding-plan/glm-5 → claude → codex → gemini), or \
             explicit: kimi, opencode, codex, claude, gemini, cursor.")
  in
  let repo =
    Arg.(
      value
      & opt (some string) None
      & info [ "repo" ] ~docv:"PATH" ~doc:"Repository path to queue against.")
  in
  let model =
    Arg.(
      value
      & opt (some string) None
      & info [ "model" ] ~docv:"MODEL"
          ~doc:"Explicit runner model to use when supported.")
  in
  let branch =
    Arg.(
      value
      & opt (some string) None
      & info [ "branch" ] ~docv:"NAME"
          ~doc:"Optional branch name for the new worktree.")
  in
  let goal = required_rest_args "GOAL" in
  Cmd.v
    (Cmd.info "delegate"
       ~doc:
         "High-level workflow for delegating coding tasks to background \
          runners."
       ~man:
         [
           `S "RELATED";
           `I
             ("background list", "Inspect queued and completed delegated tasks.");
           `I
             ( "background wait ID [--timeout SECONDS]",
               "Wait for a delegated task to finish." );
           `I
             ( "background logs ID [--lines COUNT] [--offset LINE] [--follow]",
               "Show the log output for a delegated task." );
           `I
             ( "background cancel ID",
               "Cancel a queued or running delegated task." );
         ])
    Term.(
      ret
        (const (fun runner model repo branch goal ->
             let args = [] in
             let args =
               match runner with
               | Some value -> args @ [ "--runner"; value ]
               | None -> args
             in
             let args =
               match model with
               | Some value -> args @ [ "--model"; value ]
               | None -> args
             in
             let args =
               match repo with
               | Some value -> args @ [ "--repo"; value ]
               | None -> args
             in
             let args =
               match branch with
               | Some value -> args @ [ "--branch"; value ]
               | None -> args
             in
             run "delegate" (args @ goal))
        $ runner $ model $ repo $ branch $ goal))

let plan_cmd =
  with_args "plan"
    "Run multi-stage planning pipelines: planner → plan-review loop → coder → \
     code-review loop."
    [
      `S "SUBCOMMANDS";
      `I
        ( "start <PROMPT> [--repo PATH] [--runner NAME] [--planner-model M] \
           [--reviewer-model M] [--coder-model M] [--max-plan-review-iters N] \
           [--max-code-review-iters N] [--no-plan-review] [--no-code-review]",
          "Start a new planning pipeline (foreground, blocking)." );
      `I ("list", "List all pipelines (default).");
      `I ("show <id>", "Show pipeline status and details.");
      `I ("logs <id> [--lines N]", "Show logs for the current stage.");
      `I ("cancel <id>", "Cancel a running pipeline.");
    ]

let audit_list_cmd =
  let limit =
    Arg.(
      value & opt int 20
      & info [ "limit" ] ~docv:"N" ~doc:"Show at most $(docv) entries.")
  in
  let info = Cmd.info "list" ~doc:"Show recent audit entries." in
  Cmd.v info
    Term.(
      ret
        (const (fun limit ->
             run "audit" [ "list"; "--limit"; string_of_int limit ])
        $ limit))

let audit_verify_cmd =
  let info = Cmd.info "verify" ~doc:"Verify the signed audit chain." in
  Cmd.v info Term.(ret (const (run "audit") $ const [ "verify" ]))

let audit_export_cmd =
  let path = Arg.(value & pos 0 (some string) None & info [] ~docv:"PATH") in
  let info =
    Cmd.info "export"
      ~doc:
        "Export all entries as JSONL. With no PATH, uses the configured audit \
         export location."
  in
  Cmd.v info
    Term.(
      ret
        (const (fun path -> run "audit" ([ "export" ] @ Option.to_list path))
        $ path))

let audit_import_cmd =
  let path = Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH") in
  let anchor =
    Arg.(value & opt (some string) None & info [ "anchor" ] ~docv:"PATH")
  in
  let info =
    Cmd.info "import"
      ~doc:
        "Restore an exported JSONL file into an empty audit log, loading the \
         default or explicit anchor sidecar when present."
  in
  Cmd.v info
    Term.(
      ret
        (const (fun path anchor ->
             let args = [ "import"; path ] in
             let args =
               match anchor with
               | Some anchor_path -> args @ [ "--anchor"; anchor_path ]
               | None -> args
             in
             run "audit" args)
        $ path $ anchor))

let audit_purge_cmd =
  let info =
    Cmd.info "purge"
      ~doc:
        "Retain the newest contiguous suffix allowed by the retention policy."
  in
  Cmd.v info Term.(ret (const (run "audit") $ const [ "purge" ]))

let audit_cmd =
  Cmd.group
    ~default:Term.(ret (const (run "audit") $ const [ "list" ]))
    (Cmd.info "audit" ~doc:"View and manage the security audit log.")
    [
      audit_list_cmd;
      audit_verify_cmd;
      audit_export_cmd;
      audit_import_cmd;
      audit_purge_cmd;
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
  with_args "service"
    "Manage the clawq system service (start/stop/restart/signal-restart)."
    [
      `S "SUBCOMMANDS";
      `I ("start", "Start the clawq service.");
      `I ("stop", "Stop the clawq service.");
      `I ("restart", "Restart the clawq service.");
      `I
        ( "signal-restart",
          "Send SIGUSR1 to the running daemon for a graceful restart." );
      `I ("status", "Show service status (default).");
    ]

let update_cmd =
  with_args "update"
    "Request a live daemon update, with an offline fallback stub when none is \
     running."
    [
      `S "OPTIONS";
      `I
        ( "--mode auto|git|binary",
          "Update mode. 'auto' prefers git rebuild when a repo is present, \
           otherwise binary download if configured." );
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
      `I ("apply", "Trigger live tunnel reconfiguration in running daemon.");
      `I ("restart", "Stop and restart tunnel with current config.");
      `I ("daemon-status", "Show tunnel manager state from running daemon.");
    ]

let migrate_cmd = with_args "migrate" "Run database migrations." []

let debug_cmd =
  with_args "debug" "Internal debugging utilities."
    [
      `S "SUBCOMMANDS";
      `I
        ( "html-preview [PORT]",
          "Serve Html_page test pages on localhost (default port 8099)." );
      `I
        ( "prompt [MESSAGE]",
          "Print the normalized logical messages for a single agent turn." );
    ]

let reset_agent_cmd =
  simple "reset-agent"
    "Wipe all session history, cron jobs, and workspace files, then redeploy \
     workspace defaults. Prompts for confirmation before acting. Does NOT \
     touch config.json."

let reset_workspace_cmd =
  simple "reset-workspace"
    "Wipe conversation history and workspace identity files, then redeploy \
     workspace defaults. Leaves cron jobs and config.json intact. Prompts for \
     confirmation before acting."

let otp_show_cmd =
  simple "otp-show"
    "Show the current browser pairing code and any Telegram TOTP codes."

let benchmark_cmd =
  let iterations =
    Arg.(
      value
      & opt (some string) None
      & info [ "iterations"; "n" ] ~docv:"N"
          ~doc:"Number of iterations per tool (default 10).")
  in
  let tool =
    Arg.(
      value
      & opt (some string) None
      & info [ "tool" ] ~docv:"NAME" ~doc:"Benchmark only the named tool.")
  in
  Cmd.v
    (Cmd.info "benchmark"
       ~doc:"Measure tool invocation latency to diagnose performance.")
    Term.(
      ret
        (const (fun iterations tool ->
             let args = [] in
             let args =
               match iterations with
               | Some n -> args @ [ "--iterations"; n ]
               | None -> args
             in
             let args =
               match tool with
               | Some name -> args @ [ "--tool"; name ]
               | None -> args
             in
             run "benchmark" args)
        $ iterations $ tool))

let version_cmd =
  let info = Cmd.info "version" ~doc:"Print version and build info." in
  Cmd.v info
    Term.(
      ret
        (const (fun () ->
             Printf.printf "clawq %s\ngit %s\nbuilt %s\n" Build_info.version
               Build_info.git_shorthash Build_info.build_date;
             `Ok ())
        $ const ()))

let main_info =
  Cmd.info "clawq" ~version:Build_info.version_string
    ~doc:"Coq-first AI assistant runtime"
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
  let argv =
    let args = Array.to_list Sys.argv in
    match args with [ prog; "-v" ] -> [| prog; "--version" |] | _ -> Sys.argv
  in
  let cmds =
    [
      config_cmd;
      agent_cmd;
      status_cmd;
      doctor_cmd;
      onboard_cmd;
      models_cmd;
      usage_cmd;
      provider_cmd;
      channel_cmd;
      memory_cmd;
      session_cmd;
      workspace_cmd;
      capabilities_cmd;
      auth_cmd;
      transcribe_cmd;
      mcp_cmd;
      cron_cmd;
      background_cmd;
      delegate_cmd;
      plan_cmd;
      audit_cmd;
      skills_cmd;
      service_cmd;
      update_cmd;
      runtime_cmd;
      tunnel_cmd;
      migrate_cmd;
      reset_agent_cmd;
      reset_workspace_cmd;
      otp_show_cmd;
      version_cmd;
      phase2_cmd;
      hardware_cmd;
      debug_cmd;
      benchmark_cmd;
    ]
  in
  exit (Cmd.eval ~argv ~env:help_env (Cmd.group main_info cmds))

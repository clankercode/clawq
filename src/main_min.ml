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

let required_rest_args docv =
  Arg.(non_empty & pos_all string [] & info [] ~docv)

let required_trailing_args start docv =
  Arg.(non_empty & pos_right start string [] & info [] ~docv)

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
  with_args "models" "List known models and set default model."
    [
      `S "SUBCOMMANDS";
      `I
        ( "list [--provider P]",
          "List known models from the catalog (optionally filter by provider)."
        );
      `I ("set-default MODEL", "Set default model.");
    ]

let usage_cmd =
  simple "usage"
    "Show provider quota/usage status (requires full clawq binary)."

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

let background_list_cmd =
  Cmd.v
    (Cmd.info "list" ~doc:"Disabled in minimal build; use full clawq binary.")
    Term.(ret (const (run "background") $ const [ "list" ]))

let background_show_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  Cmd.v
    (Cmd.info "show" ~doc:"Disabled in minimal build; use full clawq binary.")
    Term.(ret (const (fun id -> run "background" [ "show"; id ]) $ id))

let background_add_cmd =
  let runner =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"RUNNER")
  in
  let repo = Arg.(required & pos 1 (some string) None & info [] ~docv:"REPO") in
  let branch =
    Arg.(
      value
      & opt (some string) None
      & info [ "branch" ] ~docv:"NAME" ~doc:"Branch name for the new worktree.")
  in
  let prompt = required_trailing_args 1 "PROMPT" in
  Cmd.v
    (Cmd.info "add" ~doc:"Disabled in minimal build; use full clawq binary.")
    Term.(
      ret
        (const (fun runner repo branch prompt ->
             let args = [ "add"; runner; repo ] in
             let args =
               match branch with
               | Some name -> args @ [ "--branch"; name ]
               | None -> args
             in
             run "background" (args @ prompt))
        $ runner $ repo $ branch $ prompt))

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
    (Cmd.info "wait" ~doc:"Disabled in minimal build; use full clawq binary.")
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
  Cmd.v
    (Cmd.info "logs" ~doc:"Disabled in minimal build; use full clawq binary.")
    Term.(
      ret
        (const (fun id lines ->
             let args = [ "logs"; id ] in
             let args =
               match lines with
               | Some count -> args @ [ "--lines"; count ]
               | None -> args
             in
             run "background" args)
        $ id $ lines))

let background_cancel_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  Cmd.v
    (Cmd.info "cancel" ~doc:"Disabled in minimal build; use full clawq binary.")
    Term.(ret (const (fun id -> run "background" [ "cancel"; id ]) $ id))

let background_cmd =
  Cmd.group
    ~default:Term.(ret (const (run "background") $ const [ "list" ]))
    (Cmd.info "background"
       ~doc:"Manage background coding tasks (disabled in minimal build).")
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
       ~doc:"High-level background-task workflow (disabled in minimal build)."
       ~man:
         [
           `S "RELATED";
           `I
             ( "background list",
               "Disabled in minimal build; use full clawq binary." );
           `I
             ( "background wait ID [--timeout SECONDS]",
               "Disabled in minimal build; use full clawq binary." );
           `I
             ( "background logs ID [--lines COUNT]",
               "Disabled in minimal build; use full clawq binary." );
           `I
             ( "background cancel ID",
               "Disabled in minimal build; use full clawq binary." );
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
let update_cmd = disabled "update" "Trigger a daemon update"
let hardware_cmd = disabled "hardware" "Hardware integration (deferred)"
let otp_show_cmd = disabled "otp-show" "Show pairing codes"

let version_cmd =
  let info = Cmd.info "version" ~doc:"Print version and build info." in
  Cmd.v info
    Term.(
      ret
        (const (fun () ->
             print_endline Build_info.version_string;
             `Ok ())
        $ const ()))

let main_info =
  Cmd.info "clawq-min" ~version:Build_info.version_string
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
  let argv =
    let args = Array.to_list Sys.argv in
    match args with [ prog; "-v" ] -> [| prog; "--version" |] | _ -> Sys.argv
  in
  let cmds =
    [
      status_cmd;
      doctor_cmd;
      onboard_cmd;
      models_cmd;
      usage_cmd;
      channel_cmd;
      memory_cmd;
      workspace_cmd;
      capabilities_cmd;
      auth_cmd;
      cron_cmd;
      background_cmd;
      delegate_cmd;
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
      update_cmd;
      otp_show_cmd;
      version_cmd;
      hardware_cmd;
      benchmark_cmd;
    ]
  in
  exit (Cmd.eval ~argv ~env:help_env (Cmd.group main_info cmds))

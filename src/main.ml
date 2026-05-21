open Cmdliner

let run name args =
  let result =
    String_util.unescape_newlines (Command_bridge.handle (name :: args))
  in
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
    "Create a starter config file at the clawq config directory if none \
     exists. Set CLAWQ_HOME to override the default (~/.clawq)."

let models_list_cmd =
  let provider =
    Arg.(
      value
      & opt (some string) None
      & info [ "provider" ] ~docv:"P" ~doc:"Filter by provider name.")
  in
  let json = Arg.(value & flag & info [ "json" ] ~doc:"Output as JSON.") in
  Cmd.v
    (Cmd.info "list"
       ~doc:
         "List known models from the catalog (optionally filter by provider).")
    Term.(
      ret
        (const (fun provider json ->
             let args = [ "list" ] in
             let args =
               match provider with
               | Some p -> args @ [ "--provider"; p ]
               | None -> args
             in
             let args = if json then args @ [ "--json" ] else args in
             run "models" args)
        $ provider $ json))

let models_set_default_cmd =
  let model =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"MODEL")
  in
  let skip_validation =
    Arg.(
      value & flag
      & info [ "skip-validation"; "no-test" ]
          ~doc:
            "Skip the live test completion that normally runs before \
             committing the switch. Use only when you know the model works.")
  in
  Cmd.v
    (Cmd.info "set-default"
       ~doc:"Set default model (e.g. anthropic:claude-sonnet-4-6).")
    Term.(
      ret
        (const (fun model skip ->
             let args = [ "set-default"; model ] in
             let args = if skip then args @ [ "--skip-validation" ] else args in
             run "models" args)
        $ model $ skip_validation))

let models_refresh_cmd =
  let force =
    Arg.(
      value & flag
      & info [ "force" ] ~doc:"Force refresh, ignoring the cache TTL.")
  in
  let provider =
    Arg.(
      value
      & opt (some string) None
      & info [ "provider" ] ~docv:"PNAME"
          ~doc:"Refresh models for a specific provider only.")
  in
  Cmd.v
    (Cmd.info "refresh" ~doc:"Refresh model list from provider APIs.")
    Term.(
      ret
        (const (fun force provider ->
             let args = [ "refresh" ] in
             let args =
               match provider with
               | Some p -> args @ [ "--provider"; p ]
               | None -> args
             in
             let args = if force then args @ [ "--force" ] else args in
             run "models" args)
        $ force $ provider))

let models_cmd =
  Cmd.group
    ~default:Term.(ret (const (run "models") $ const []))
    (Cmd.info "models" ~doc:"List known models and set default model.")
    [ models_list_cmd; models_set_default_cmd; models_refresh_cmd ]

let costs_cmd =
  with_args "costs" "Show cumulative LLM costs and token usage."
    [
      `S "SUBCOMMANDS";
      `I ("(default)", "Cost summary by time period (today, 7d, 30d, all).");
      `I ("session [KEY]", "Per-session cost breakdown.");
      `I ("model", "Per-model cost breakdown.");
      `I ("provider", "Per-provider cost breakdown.");
      `S "OPTIONS";
      `I ("--json", "Output as JSON.");
    ]

let usage_cmd =
  with_args "usage" "Show provider quota/usage status."
    [
      `S "SUBCOMMANDS";
      `I ("(default)", "Show current quota (use --refresh/-r to force fetch).");
      `I ("history", "Show historical quota snapshots.");
      `I ("purge [PERIOD]", "Delete old history (default: 90d).");
      `S "HISTORY OPTIONS";
      `I ("--provider NAME", "Filter to a specific provider.");
      `I ("--since PERIOD", "Time range: today, 7d, 30d, 90d.");
      `I ("--limit N", "Max rows (default: 50).");
      `I ("--json", "Output as JSON.");
    ]

let active_cmd =
  simple "active" "Show active 5-hour window usage (cost, tokens, quota)."

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
  with_args "channel" "List configured channels or test a channel connection."
    []

let memory_cmd = simple "memory" "Show memory backend configuration."

let session_list_cmd =
  let channel =
    Arg.(
      value
      & opt (some string) None
      & info [ "channel" ] ~docv:"NAME" ~doc:"Filter by channel name.")
  in
  let prefix =
    Arg.(
      value
      & opt (some string) None
      & info [ "prefix" ] ~docv:"PREFIX" ~doc:"Filter by session key prefix.")
  in
  let active =
    Arg.(value & flag & info [ "active" ] ~doc:"Show only active sessions.")
  in
  let inactive =
    Arg.(value & flag & info [ "inactive" ] ~doc:"Show only inactive sessions.")
  in
  let main_only =
    Arg.(value & flag & info [ "main" ] ~doc:"Show only main sessions.")
  in
  let non_main =
    Arg.(value & flag & info [ "non-main" ] ~doc:"Show only non-main sessions.")
  in
  Cmd.v
    (Cmd.info "list" ~doc:"List persisted sessions with optional filters.")
    Term.(
      ret
        (const (fun channel prefix active inactive main_only non_main ->
             let args = [ "list" ] in
             let args =
               match channel with
               | Some v -> args @ [ "--channel"; v ]
               | None -> args
             in
             let args =
               match prefix with
               | Some v -> args @ [ "--prefix"; v ]
               | None -> args
             in
             let args =
               if active then args @ [ "--active" ]
               else if inactive then args @ [ "--inactive" ]
               else args
             in
             let args =
               if main_only then args @ [ "--main" ]
               else if non_main then args @ [ "--non-main" ]
               else args
             in
             run "session" args)
        $ channel $ prefix $ active $ inactive $ main_only $ non_main))

let session_epochs_cmd =
  let session_key =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SESSION")
  in
  Cmd.v
    (Cmd.info "epochs" ~doc:"List the current and archived chat-log epochs.")
    Term.(ret (const (fun sk -> run "session" [ "epochs"; sk ]) $ session_key))

let session_show_cmd =
  let session_key =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SESSION")
  in
  let epoch =
    Arg.(
      value
      & opt (some string) None
      & info [ "epoch" ] ~docv:"current|ID"
          ~doc:"Select epoch: 'current' or a numeric archive ID.")
  in
  let offset =
    Arg.(
      value
      & opt (some int) None
      & info [ "offset" ] ~docv:"N" ~doc:"Skip the first N messages.")
  in
  let limit =
    Arg.(
      value
      & opt (some int) None
      & info [ "limit" ] ~docv:"N" ~doc:"Show at most N messages.")
  in
  Cmd.v
    (Cmd.info "show"
       ~doc:
         "Print the raw chat log for the current or a specific archived epoch.")
    Term.(
      ret
        (const (fun sk epoch offset limit ->
             let args = [ "show"; sk ] in
             let args =
               match epoch with
               | Some v -> args @ [ "--epoch"; v ]
               | None -> args
             in
             let args =
               match offset with
               | Some n -> args @ [ "--offset"; string_of_int n ]
               | None -> args
             in
             let args =
               match limit with
               | Some n -> args @ [ "--limit"; string_of_int n ]
               | None -> args
             in
             run "session" args)
        $ session_key $ epoch $ offset $ limit))

let session_inject_cmd =
  let args = required_trailing_args 0 "MESSAGE" in
  let session_key =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SESSION")
  in
  let cwd =
    Arg.(
      value
      & opt (some string) None
      & info [ "cwd" ]
          ~doc:"Set the agent's working directory for this session."
          ~docv:"PATH")
  in
  Cmd.v
    (Cmd.info "inject"
       ~doc:"Inject a live inbound message through the daemon session manager.")
    Term.(
      ret
        (const (fun cwd sk msg_parts ->
             run "session"
               ([ "inject" ]
               @ (match cwd with Some c -> [ "--cwd"; c ] | None -> [])
               @ [ sk ] @ msg_parts))
        $ cwd $ session_key $ args))

let session_events_cmd =
  let session_key =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SESSION")
  in
  let epoch =
    Arg.(
      value
      & opt (some string) None
      & info [ "epoch" ] ~docv:"current|ID"
          ~doc:"Select epoch: 'current' or a numeric archive ID.")
  in
  let event_type =
    Arg.(
      value
      & opt (some string) None
      & info [ "type" ] ~docv:"TYPE"
          ~doc:
            "Filter to a specific event type: workspace_refresh, \
             unknown_event, memory_context, attachment, compaction.")
  in
  Cmd.v
    (Cmd.info "events"
       ~doc:"Show event, system, and compaction messages for a session.")
    Term.(
      ret
        (const (fun sk epoch event_type ->
             let args = [ "events"; sk ] in
             let args =
               match epoch with
               | Some v -> args @ [ "--epoch"; v ]
               | None -> args
             in
             let args =
               match event_type with
               | Some v -> args @ [ "--type"; v ]
               | None -> args
             in
             run "session" args)
        $ session_key $ epoch $ event_type))

let session_pending_cmd =
  let session_key =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SESSION")
  in
  Cmd.v
    (Cmd.info "pending" ~doc:"Show pending inbound queue rows for a session.")
    Term.(ret (const (fun sk -> run "session" [ "pending"; sk ]) $ session_key))

let session_compact_cmd =
  let session_key =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SESSION")
  in
  Cmd.v
    (Cmd.info "compact"
       ~doc:
         "Compact session history by summarizing older messages to free up \
          context space.")
    Term.(ret (const (fun sk -> run "session" [ "compact"; sk ]) $ session_key))

let session_model_cmd =
  let session_key =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SESSION")
  in
  let args = Arg.(value & pos_right 0 string [] & info [] ~docv:"ARGS") in
  Cmd.v
    (Cmd.info "model"
       ~doc:
         "Get, set, or clear the per-session model override (e.g. model \
          SESSION set anthropic:claude-sonnet-4-6).")
    Term.(
      ret
        (const (fun sk rest -> run "session" ([ "model"; sk ] @ rest))
        $ session_key $ args))

let session_cmd =
  Cmd.group
    ~default:Term.(ret (const (run "session") $ const []))
    (Cmd.info "session"
       ~doc:"Inspect persisted sessions and raw chat log epochs.")
    [
      session_list_cmd;
      session_epochs_cmd;
      session_show_cmd;
      session_inject_cmd;
      session_events_cmd;
      session_pending_cmd;
      session_compact_cmd;
      session_model_cmd;
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
      `I ("search QUERY", "Search config keys matching QUERY.");
      `S "EXAMPLES";
      `P "clawq config set providers.openrouter.api_key \"sk-...\"";
      `P "clawq config set agent_defaults.primary_model \"openrouter:gpt-5.4\"";
      `P "clawq config set security.tools_enabled true";
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
        ( "set-key PROVIDER [API_KEY]",
          "Set the API key for a named provider. Omit API_KEY to enter it \
           interactively with hidden input." );
      `I ("providers", "List known provider names and their configured status.");
      `I
        ("encrypt", "Encrypt plaintext API keys in config using the master key.");
      `I ("codex-login [PROVIDER]", "Start ChatGPT/Codex OAuth login flow.");
      `I ("codex-status [PROVIDER]", "Show stored Codex OAuth status.");
      `I ("codex-logout [PROVIDER]", "Clear stored Codex OAuth credentials.");
      `I
        ( "pair [OTP]",
          "Pair with a running clawq gateway using an OTP code. Omit OTP to \
           enter it interactively." );
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
      `S "EXAMPLES";
      `P "clawq cron list";
      `P
        "clawq cron add morning chat \"0 9 * * 1-5\" \"Good morning, what's \
         the plan?\"";
      `P "clawq cron show morning";
      `P "clawq cron history morning";
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
  let agent =
    Arg.(
      value
      & opt (some string) None
      & info [ "agent" ] ~docv:"NAME"
          ~doc:
            "Agent template name to use (e.g. coder, reviewer). Applies the \
             agent's system prompt, tool restrictions, and model override.")
  in
  let prompt = required_trailing_args 1 "PROMPT" in
  Cmd.v
    (Cmd.info "add" ~doc:"Queue a background coding task for a repository.")
    Term.(
      ret
        (const (fun runner repo model branch agent prompt ->
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
             let args =
               match agent with
               | Some name -> args @ [ "--agent"; name ]
               | None -> args
             in
             run "background" (args @ prompt))
        $ runner $ repo $ model $ branch $ agent $ prompt))

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

let background_resume_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  Cmd.v
    (Cmd.info "resume"
       ~doc:
         "Resume a previously started background task using the runner's \
          native session support.")
    Term.(ret (const (fun id -> run "background" [ "resume"; id ]) $ id))

let background_message_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  let message = required_trailing_args 1 "MESSAGE" in
  Cmd.v
    (Cmd.info "message"
       ~doc:"Send a chat message into a started background task and resume it.")
    Term.(
      ret
        (const (fun id message ->
             run "background" ([ "message"; id ] @ message))
        $ id $ message))

let background_cancel_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  Cmd.v
    (Cmd.info "cancel" ~doc:"Cancel a queued or running background task.")
    Term.(ret (const (fun id -> run "background" [ "cancel"; id ]) $ id))

let background_retry_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  Cmd.v
    (Cmd.info "retry" ~doc:"Re-queue a failed background task.")
    Term.(ret (const (fun id -> run "background" [ "retry"; id ]) $ id))

let background_recover_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  let runner =
    Arg.(
      value
      & opt (some string) None
      & info [ "runner" ] ~docv:"RUNNER"
          ~doc:"Override runner (codex|claude|kimi|gemini|opencode|cursor).")
  in
  let model =
    Arg.(
      value
      & opt (some string) None
      & info [ "model" ] ~docv:"MODEL" ~doc:"Override model.")
  in
  Cmd.v
    (Cmd.info "recover"
       ~doc:"Recover a failed or stuck background task with full context.")
    Term.(
      ret
        (const (fun id runner model ->
             let args = [ "recover"; id ] in
             let args =
               match runner with
               | Some r -> args @ [ "--runner"; r ]
               | None -> args
             in
             let args =
               match model with
               | Some m -> args @ [ "--model"; m ]
               | None -> args
             in
             run "background" args)
        $ id $ runner $ model))

let background_finalize_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  Cmd.v
    (Cmd.info "finalize"
       ~doc:
         "Rebase and fast-forward a completed task worktree into the target \
          branch.")
    Term.(ret (const (fun id -> run "background" [ "finalize"; id ]) $ id))

let background_cmd =
  Cmd.group
    ~default:Term.(ret (const (run "background") $ const []))
    (Cmd.info "background"
       ~doc:
         "Manage background coding tasks that run a coding agent in git \
          worktrees."
       ~man:
         [
           `S "EXAMPLES";
           `P "clawq background list";
           `P "clawq background add codex /path/to/repo \"implement feature X\"";
           `P "clawq background show 3";
           `P "clawq background logs 3 --follow";
           `P "clawq background message 3 \"please also fix the tests\"";
         ])
    [
      background_list_cmd;
      background_show_cmd;
      background_add_cmd;
      background_wait_cmd;
      background_logs_cmd;
      background_resume_cmd;
      background_message_cmd;
      background_cancel_cmd;
      background_retry_cmd;
      background_recover_cmd;
      background_finalize_cmd;
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
           `S "EXAMPLES";
           `P "clawq delegate \"implement the feature from TODO.md\"";
           `P
             "clawq delegate --runner codex --model gpt-5.4 \"fix all failing \
              tests\"";
           `P "clawq delegate --repo /path/to/repo \"refactor the auth module\"";
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

let plan_list_cmd =
  Cmd.v
    (Cmd.info "list" ~doc:"List all pipelines (default).")
    Term.(ret (const (run "plan") $ const [ "list" ]))

let plan_start_cmd =
  let prompt = rest_args "PROMPT" in
  let repo =
    Arg.(
      value
      & opt (some string) None
      & info [ "repo" ] ~docv:"PATH" ~doc:"Repository path to plan against.")
  in
  let runner =
    Arg.(
      value
      & opt (some string) None
      & info [ "runner" ] ~docv:"NAME"
          ~doc:"Runner: auto, kimi, opencode, codex, claude, gemini, cursor.")
  in
  let planner_model =
    Arg.(
      value
      & opt (some string) None
      & info [ "planner-model" ] ~docv:"M" ~doc:"Model for the planner stage.")
  in
  let reviewer_model =
    Arg.(
      value
      & opt (some string) None
      & info [ "reviewer-model" ] ~docv:"M" ~doc:"Model for the reviewer stage.")
  in
  let coder_model =
    Arg.(
      value
      & opt (some string) None
      & info [ "coder-model" ] ~docv:"M" ~doc:"Model for the coder stage.")
  in
  let max_plan_review =
    Arg.(
      value
      & opt (some int) None
      & info
          [ "max-plan-review-iters" ]
          ~docv:"N" ~doc:"Maximum plan-review iterations (default 3).")
  in
  let max_code_review =
    Arg.(
      value
      & opt (some int) None
      & info
          [ "max-code-review-iters" ]
          ~docv:"N" ~doc:"Maximum code-review iterations (default 3).")
  in
  let no_plan_review =
    Arg.(value & flag & info [ "no-plan-review" ] ~doc:"Skip plan review.")
  in
  let no_code_review =
    Arg.(value & flag & info [ "no-code-review" ] ~doc:"Skip code review.")
  in
  Cmd.v
    (Cmd.info "start"
       ~doc:"Start a new planning pipeline (foreground, blocking).")
    Term.(
      ret
        (const
           (fun
             prompt
             repo
             runner
             planner_model
             reviewer_model
             coder_model
             max_plan_review
             max_code_review
             no_plan_review
             no_code_review
           ->
             let args = [ "start" ] @ prompt in
             let args =
               match repo with Some p -> args @ [ "--repo"; p ] | None -> args
             in
             let args =
               match runner with
               | Some r -> args @ [ "--runner"; r ]
               | None -> args
             in
             let args =
               match planner_model with
               | Some m -> args @ [ "--planner-model"; m ]
               | None -> args
             in
             let args =
               match reviewer_model with
               | Some m -> args @ [ "--reviewer-model"; m ]
               | None -> args
             in
             let args =
               match coder_model with
               | Some m -> args @ [ "--coder-model"; m ]
               | None -> args
             in
             let args =
               match max_plan_review with
               | Some n -> args @ [ "--max-plan-review-iters"; string_of_int n ]
               | None -> args
             in
             let args =
               match max_code_review with
               | Some n -> args @ [ "--max-code-review-iters"; string_of_int n ]
               | None -> args
             in
             let args =
               if no_plan_review then args @ [ "--no-plan-review" ] else args
             in
             let args =
               if no_code_review then args @ [ "--no-code-review" ] else args
             in
             run "plan" args)
        $ prompt $ repo $ runner $ planner_model $ reviewer_model $ coder_model
        $ max_plan_review $ max_code_review $ no_plan_review $ no_code_review))

let plan_show_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  Cmd.v
    (Cmd.info "show" ~doc:"Show pipeline status and details.")
    Term.(ret (const (fun id -> run "plan" [ "show"; id ]) $ id))

let plan_logs_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  let lines =
    Arg.(
      value
      & opt (some int) None
      & info [ "lines" ] ~docv:"N"
          ~doc:"Number of log lines to show (default 50).")
  in
  Cmd.v
    (Cmd.info "logs" ~doc:"Show logs for the current stage.")
    Term.(
      ret
        (const (fun id lines ->
             let args = [ "logs"; id ] in
             let args =
               match lines with
               | Some n -> args @ [ "--lines"; string_of_int n ]
               | None -> args
             in
             run "plan" args)
        $ id $ lines))

let plan_cancel_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  Cmd.v
    (Cmd.info "cancel" ~doc:"Cancel a running pipeline.")
    Term.(ret (const (fun id -> run "plan" [ "cancel"; id ]) $ id))

let plan_cmd =
  Cmd.group
    ~default:Term.(ret (const (run "plan") $ const []))
    (Cmd.info "plan"
       ~doc:
         "Run multi-stage planning pipelines: planner → plan-review loop → \
          coder → code-review loop.")
    [
      plan_list_cmd;
      plan_start_cmd;
      plan_show_cmd;
      plan_logs_cmd;
      plan_cancel_cmd;
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
    "Manage the clawq system service \
     (start/stop/restart/signal-restart/systemd-unit)."
    [
      `S "SUBCOMMANDS";
      `I ("start", "Start the clawq service.");
      `I ("stop", "Stop the clawq service.");
      `I ("restart", "Restart the clawq service.");
      `I
        ( "signal-restart",
          "Send SIGUSR1 to the running daemon for a graceful restart." );
      `I ("status", "Show service status (default).");
      `I
        ( "systemd-unit",
          "Print a systemd unit file. Install with: clawq service systemd-unit \
           > ~/.config/systemd/user/clawq.service" );
    ]

let update_cmd =
  let mode =
    Arg.(
      value
      & opt (some string) None
      & info [ "mode" ] ~docv:"auto|git|binary"
          ~doc:
            "Update mode. 'auto' prefers git rebuild when a repo is present, \
             otherwise binary download if configured.")
  in
  Cmd.v
    (Cmd.info "update"
       ~doc:
         "Request a live daemon update, with an offline fallback stub when \
          none is running.")
    Term.(
      ret
        (const (fun mode ->
             let args =
               match mode with Some m -> [ "--mode"; m ] | None -> []
             in
             run "update" args)
        $ mode))

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

let agents_cmd =
  with_args "agents"
    "Manage agent templates (roles, tool restrictions, routing bindings)."
    [
      `S "SUBCOMMANDS";
      `I ("list", "List all agent templates (builtins and user-defined).");
      `I ("show NAME", "Show full details for an agent template.");
      `I ("create NAME", "Create a new template in ~/.clawq/agents/.");
      `I ("edit NAME", "Edit a template (copies builtin to user dir first).");
      `I ("delete NAME", "Delete a user-defined template.");
      `I
        ( "bind PATTERN AGENT [--priority N]",
          "Add/update a routing binding in config.json." );
      `I ("unbind PATTERN", "Remove a routing binding.");
      `I ("bindings", "List current agent bindings from config.");
      `I ("setup", "Launch interactive agent template wizard.");
      `I ("path", "Show template search directories.");
    ]

let rig_cmd =
  with_args "rig" "Manage agent-driven setup rigs (install, adjust, remove)."
    [
      `S "SUBCOMMANDS";
      `I
        ( "install NAME",
          "Install a rig by delegating setup to a background task." );
      `I ("adjust NAME", "Reconfigure an installed rig.");
      `I ("remove NAME", "Remove an installed rig and clean up.");
      `I ("list", "List available rigs and their install status.");
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

let completions_shell_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "shell" ] ~docv:"SHELL"
        ~doc:
          "Target shell: bash, zsh, or fish. Auto-detected from \\$SHELL if \
           omitted.")

let completions_print_cmd =
  let info =
    Cmd.info "print"
      ~doc:"Print the completion script for the current or specified shell."
  in
  Cmd.v info
    Term.(
      ret
        (const (fun shell ->
             let args =
               match shell with
               | Some s -> [ "print"; "--shell"; s ]
               | None -> [ "print" ]
             in
             run "completions" args)
        $ completions_shell_arg))

let completions_install_cmd =
  let info =
    Cmd.info "install"
      ~doc:
        "Install completion script to the default location for the current or \
         specified shell."
  in
  Cmd.v info
    Term.(
      ret
        (const (fun shell ->
             let args =
               match shell with
               | Some s -> [ "install"; "--shell"; s ]
               | None -> [ "install" ]
             in
             run "completions" args)
        $ completions_shell_arg))

let completions_cmd =
  Cmd.group
    ~default:Term.(ret (const (run "completions") $ const []))
    (Cmd.info "completions"
       ~doc:"Generate and install shell tab-completion scripts.")
    [ completions_print_cmd; completions_install_cmd ]

let setup_sub name doc =
  Cmd.v (Cmd.info name ~doc) Term.(ret (const (run "setup") $ const [ name ]))

let setup_cmd =
  let subs =
    List.concat_map
      (fun (cat : Setup_main.category) ->
        List.map
          (fun (e : Setup_main.wizard_entry) -> setup_sub e.name e.label)
          cat.entries)
      Setup_main.all_categories
  in
  Cmd.group
    ~default:Term.(ret (const (run "setup") $ const []))
    (Cmd.info "setup" ~doc:"Interactive setup wizards for clawq features.")
    subs

let watcher_cmd =
  with_args "watcher"
    "Manage the error correction watcher (status, enable/disable, reports)."
    [
      `S "SUBCOMMANDS";
      `I ("status", "Show watcher config and EC process status (default).");
      `I ("enable", "Enable the error correction watcher.");
      `I ("disable", "Disable the error correction watcher.");
      `I ("reports", "List recent EC reports.");
      `I ("report ID", "Show a specific EC report.");
    ]

let ec_run_cmd =
  let daemon_mode =
    Arg.(
      value & flag
      & info [ "daemon-mode" ] ~doc:"Run EC process in daemon mode (internal).")
  in
  Cmd.v
    (Cmd.info "ec-run" ~doc:"Internal: run the error correction process.")
    Term.(
      ret
        (const (fun daemon_mode ->
             let args = if daemon_mode then [ "--daemon-mode" ] else [] in
             run "ec-run" args)
        $ daemon_mode))

let version_cmd =
  let info = Cmd.info "version" ~doc:"Print version and build info." in
  Cmd.v info
    Term.(
      ret
        (const (fun () ->
             Printf.printf "clawq %s\ngit %s\nbuilt %s\n" Build_info.version_dev
               Build_info.git_shorthash Build_info.build_date;
             `Ok ())
        $ const ()))

let held_items_cmd =
  with_args "held-items" "Manage held feature plans awaiting admin review."
    [
      `S "SUBCOMMANDS";
      `I
        ( "save --name NAME --desc DESC --plan-file FILE --layer N",
          "Save a feature plan to the held items queue." );
      `I ("list [--status STATUS]", "List held items (default: pending).");
      `I ("show ID", "Show details of a specific held item.");
      `I ("approve ID [--by ADMIN] [--notes TEXT]", "Approve a pending item.");
      `I ("reject ID [--by ADMIN] [--notes TEXT]", "Reject a pending item.");
    ]

let debate_cmd =
  with_args "debate"
    "Route a prompt to multiple models and synthesize a consensus."
    [
      `S "SUBCOMMANDS";
      `I ("--models m1,m2,m3", "Override default debate models.");
      `I ("--judge model", "Override the judge model.");
      `I ("--no-judge", "Skip synthesis, show raw responses.");
      `I ("--format json|text", "Output format (default: text).");
      `I ("--history", "List past debate rounds.");
      `I ("--show ID", "Show a specific past debate round.");
    ]

let pipeline_cmd =
  with_args "pipeline"
    "Define and run structured output pipelines with validated JSON Schema \
     outputs."
    [
      `S "SUBCOMMANDS";
      `I ("list", "List available pipelines.");
      `I ("show NAME", "Show pipeline definition details.");
      `I ("run NAME --input k=v ...", "Execute a pipeline.");
      `I ("validate NAME", "Validate a pipeline definition.");
      `I ("create NAME", "Scaffold a new pipeline YAML file.");
      `I ("wizard", "Interactive pipeline builder.");
      `I ("history [--pipeline NAME]", "List past pipeline runs.");
      `I ("result RUN-ID", "Show results of a pipeline run.");
    ]

let manifest_cmd =
  with_args "manifest" "Generate connector command manifests (Teams, Telegram)."
    [
      `S "SUBCOMMANDS";
      `I ("teams", "Generate Teams bot manifest commands JSON (top 10).");
      `I ("teams --output FILE", "Write Teams manifest to a file.");
      `I ("teams -n COUNT", "Customize the number of commands.");
      `I ("telegram", "Generate Telegram setMyCommands JSON payload.");
    ]

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
  Printexc.record_backtrace true;
  let argv =
    let args = Array.to_list Sys.argv in
    match args with
    | [ prog; "-v" ] -> [| prog; "--version" |]
    | _ -> Array.map (fun arg -> if arg = "-h" then "--help" else arg) Sys.argv
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
      costs_cmd;
      active_cmd;
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
      agents_cmd;
      rig_cmd;
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
      completions_cmd;
      setup_cmd;
      watcher_cmd;
      ec_run_cmd;
      manifest_cmd;
      held_items_cmd;
      debate_cmd;
      pipeline_cmd;
    ]
  in
  exit (Cmd.eval ~argv ~env:help_env (Cmd.group main_info cmds))

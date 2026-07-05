open Cmdliner
open Main_cmd_common

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

let background_transcript_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  let regex =
    Arg.(
      value
      & opt (some string) None
      & info [ "regex" ] ~docv:"REGEX"
          ~doc:"Filter transcript lines with an OCaml Str regex.")
  in
  let max_lines =
    Arg.(
      value
      & opt (some string) None
      & info [ "max-lines" ] ~docv:"COUNT"
          ~doc:"Maximum inline transcript lines (default 200, hard cap 300).")
  in
  let export =
    Arg.(
      value & flag
      & info [ "export" ] ~doc:"Also write the filtered transcript to JSONL.")
  in
  Cmd.v
    (Cmd.info "transcript"
       ~doc:"Show a bounded task transcript with optional regex filtering.")
    Term.(
      ret
        (const (fun id regex max_lines export ->
             let args = [ "transcript"; id ] in
             let args =
               match regex with
               | Some value -> args @ [ "--regex"; value ]
               | None -> args
             in
             let args =
               match max_lines with
               | Some value -> args @ [ "--max-lines"; value ]
               | None -> args
             in
             let args = if export then args @ [ "--export" ] else args in
             run "background" args)
        $ id $ regex $ max_lines $ export))

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

let background_start_cmd =
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
          ~doc:"Agent template name for local/native tasks.")
  in
  let prompt = required_trailing_args 1 "PROMPT" in
  Cmd.v
    (Cmd.info "start" ~doc:"Alias for background add.")
    Term.(
      ret
        (const (fun runner repo model branch agent prompt ->
             let args = [ "start"; runner; repo ] in
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

let background_send_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  let message = required_trailing_args 1 "MESSAGE" in
  Cmd.v
    (Cmd.info "send" ~doc:"Alias for background message.")
    Term.(
      ret
        (const (fun id message -> run "background" ([ "send"; id ] @ message))
        $ id $ message))

let background_stop_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  Cmd.v
    (Cmd.info "stop" ~doc:"Alias for background cancel.")
    Term.(ret (const (fun id -> run "background" [ "stop"; id ]) $ id))

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
          ~doc:
            "Override runner (codex|claude|kimi|gemini|opencode|cursor|local).")
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
          worktrees or local sessions."
       ~man:
         [
           `S "EXAMPLES";
           `P "clawq background list";
           `P "clawq background add codex /path/to/repo \"implement feature X\"";
           `P "clawq background show 3";
           `P "clawq background logs 3 --follow";
           `P "clawq background transcript 3 --regex failure";
           `P "clawq background message 3 \"please also fix the tests\"";
         ])
    [
      background_list_cmd;
      background_show_cmd;
      background_add_cmd;
      background_wait_cmd;
      background_logs_cmd;
      background_transcript_cmd;
      background_resume_cmd;
      background_message_cmd;
      background_start_cmd;
      background_send_cmd;
      background_stop_cmd;
      background_cancel_cmd;
      background_retry_cmd;
      background_recover_cmd;
      background_finalize_cmd;
    ]

let subagents_list_cmd =
  Cmd.v
    (Cmd.info "list" ~doc:"List native/local subagent background tasks.")
    Term.(ret (const (run "subagents") $ const [ "list" ]))

let subagents_start_cmd =
  let repo = Arg.(required & pos 0 (some string) None & info [] ~docv:"REPO") in
  let model =
    Arg.(
      value
      & opt (some string) None
      & info [ "model" ] ~docv:"MODEL"
          ~doc:"Explicit model for the native subagent.")
  in
  let agent =
    Arg.(
      value
      & opt (some string) None
      & info [ "agent" ] ~docv:"NAME"
          ~doc:"Agent template name to use, e.g. coder or reviewer.")
  in
  let prompt = required_trailing_args 1 "PROMPT" in
  Cmd.v
    (Cmd.info "start" ~doc:"Start a native/local subagent task.")
    Term.(
      ret
        (const (fun repo model agent prompt ->
             let args = [ "start"; repo ] in
             let args =
               match model with
               | Some value -> args @ [ "--model"; value ]
               | None -> args
             in
             let args =
               match agent with
               | Some name -> args @ [ "--agent"; name ]
               | None -> args
             in
             run "subagents" (args @ prompt))
        $ repo $ model $ agent $ prompt))

let subagents_stop_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  Cmd.v
    (Cmd.info "stop" ~doc:"Stop a native subagent task.")
    Term.(ret (const (fun id -> run "subagents" [ "stop"; id ]) $ id))

let subagents_send_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  let message = required_trailing_args 1 "MESSAGE" in
  Cmd.v
    (Cmd.info "send" ~doc:"Send a follow-up message to a native subagent.")
    Term.(
      ret
        (const (fun id message -> run "subagents" ([ "send"; id ] @ message))
        $ id $ message))

let subagents_transcript_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  let regex =
    Arg.(
      value
      & opt (some string) None
      & info [ "regex" ] ~docv:"REGEX" ~doc:"Filter transcript lines.")
  in
  let max_lines =
    Arg.(
      value
      & opt (some string) None
      & info [ "max-lines" ] ~docv:"COUNT"
          ~doc:"Maximum inline transcript lines.")
  in
  let export =
    Arg.(value & flag & info [ "export" ] ~doc:"Export transcript JSONL.")
  in
  Cmd.v
    (Cmd.info "transcript" ~doc:"Show a bounded native subagent transcript.")
    Term.(
      ret
        (const (fun id regex max_lines export ->
             let args = [ "transcript"; id ] in
             let args =
               match regex with
               | Some value -> args @ [ "--regex"; value ]
               | None -> args
             in
             let args =
               match max_lines with
               | Some value -> args @ [ "--max-lines"; value ]
               | None -> args
             in
             let args = if export then args @ [ "--export" ] else args in
             run "subagents" args)
        $ id $ regex $ max_lines $ export))

let subagents_cmd =
  Cmd.group
    ~default:Term.(ret (const (run "subagents") $ const []))
    (Cmd.info "subagents"
       ~doc:"Manage native/local subagents backed by background tasks."
       ~man:
         [
           `S "EXAMPLES";
           `P
             "clawq subagents start --agent coder --model \
              xiaomi-token-plan-sgp:mimo-v2.5-pro /path/to/repo \"investigate \
              this\"";
           `P "clawq subagents send 3 \"please include the failing command\"";
           `P "clawq subagents transcript 3 --regex failure";
         ])
    [
      subagents_list_cmd;
      subagents_start_cmd;
      subagents_stop_cmd;
      subagents_send_cmd;
      subagents_transcript_cmd;
    ]

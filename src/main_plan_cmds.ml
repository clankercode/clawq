open Cmdliner
open Main_cmd_common

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

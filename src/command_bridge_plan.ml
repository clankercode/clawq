open Command_bridge_helpers

type plan_start_args = {
  plan_prompt : string;
  plan_repo : string option;
  plan_runner : Background_task.runner option;
  plan_planner_model : string option;
  plan_reviewer_model : string option;
  plan_coder_model : string option;
  plan_max_plan_review_iters : int;
  plan_max_code_review_iters : int;
}

let parse_plan_start_args args =
  let rec loop prompt_parts repo runner planner_model reviewer_model coder_model
      max_plan_review max_code_review = function
    | [] ->
        let prompt = String.concat " " (List.rev prompt_parts) |> String.trim in
        if prompt = "" then
          Error
            "Usage: clawq plan start <PROMPT> [--repo PATH] [--runner NAME] \
             [--planner-model M] [--reviewer-model M] [--coder-model M] \
             [--max-plan-review-iters N] [--max-code-review-iters N] \
             [--no-plan-review] [--no-code-review]"
        else
          Ok
            {
              plan_prompt = prompt;
              plan_repo = repo;
              plan_runner = runner;
              plan_planner_model = planner_model;
              plan_reviewer_model = reviewer_model;
              plan_coder_model = coder_model;
              plan_max_plan_review_iters = max_plan_review;
              plan_max_code_review_iters = max_code_review;
            }
    | "--repo" :: v :: rest ->
        loop prompt_parts (Some v) runner planner_model reviewer_model
          coder_model max_plan_review max_code_review rest
    | "--runner" :: v :: rest ->
        let r = Background_task.runner_of_string v in
        loop prompt_parts repo r planner_model reviewer_model coder_model
          max_plan_review max_code_review rest
    | "--planner-model" :: v :: rest ->
        loop prompt_parts repo runner (Some v) reviewer_model coder_model
          max_plan_review max_code_review rest
    | "--reviewer-model" :: v :: rest ->
        loop prompt_parts repo runner planner_model (Some v) coder_model
          max_plan_review max_code_review rest
    | "--coder-model" :: v :: rest ->
        loop prompt_parts repo runner planner_model reviewer_model (Some v)
          max_plan_review max_code_review rest
    | "--max-plan-review-iters" :: v :: rest -> (
        try
          loop prompt_parts repo runner planner_model reviewer_model coder_model
            (int_of_string v) max_code_review rest
        with _ -> Error "--max-plan-review-iters requires an integer value")
    | "--max-code-review-iters" :: v :: rest -> (
        try
          loop prompt_parts repo runner planner_model reviewer_model coder_model
            max_plan_review (int_of_string v) rest
        with _ -> Error "--max-code-review-iters requires an integer value")
    | "--no-plan-review" :: rest ->
        loop prompt_parts repo runner planner_model reviewer_model coder_model 0
          max_code_review rest
    | "--no-code-review" :: rest ->
        loop prompt_parts repo runner planner_model reviewer_model coder_model
          max_plan_review 0 rest
    | arg :: rest ->
        loop (arg :: prompt_parts) repo runner planner_model reviewer_model
          coder_model max_plan_review max_code_review rest
  in
  loop [] None None None None None 3 3 args

let cmd_plan args =
  let cfg = get_config () in
  let db = get_db () in
  Plan_pipeline.init_schema db;
  Background_task.init_schema db;
  match args with
  | [] | [ "list" ] ->
      let pipelines = Plan_pipeline.list_pipelines ~db in
      Plan_pipeline.format_pipeline_list pipelines
      ^ "\n\n\
         Commands:\n\
        \  plan start <PROMPT> [--repo PATH] [--runner NAME]   - Start pipeline\n\
        \  plan list                                           - List pipelines\n\
        \  plan show <id>                                      - Show pipeline \
         details\n\
        \  plan logs <id> [--lines N]                          - Show stage logs\n\
        \  plan cancel <id>                                    - Cancel \
         pipeline"
  | "start" :: rest -> (
      match parse_plan_start_args rest with
      | Error msg -> "Error: " ^ msg
      | Ok parsed -> (
          let repo_path =
            match parsed.plan_repo with
            | Some p -> p
            | None -> Command_bridge_bgargs.default_delegate_repo_path cfg
          in
          let model_config : Plan_pipeline.model_config =
            {
              Plan_pipeline.planner_model = parsed.plan_planner_model;
              reviewer_model = parsed.plan_reviewer_model;
              coder_model = parsed.plan_coder_model;
              max_plan_review_iters = parsed.plan_max_plan_review_iters;
              max_code_review_iters = parsed.plan_max_code_review_iters;
            }
          in
          let runner_result =
            Background_task.resolve_runner ?preferred:parsed.plan_runner ()
          in
          match runner_result with
          | Error msg -> "Error: " ^ msg
          | Ok (runner, _) -> (
              let pipeline =
                Plan_pipeline.create ~db ~prompt:parsed.plan_prompt ~repo_path
                  ~model_config
              in
              Printf.printf
                "Started pipeline %d (stage: planning)\n\
                 Plan file: %s\n\
                 Use `clawq plan show %d` to check progress.\n"
                pipeline.Plan_pipeline.id
                (Plan_pipeline.plan_file_path pipeline)
                pipeline.Plan_pipeline.id;
              flush stdout;
              let result =
                Lwt_main.run
                  (Plan_pipeline.run_foreground ~db ~pipeline ~runner
                     ~on_progress:(fun s ->
                       print_endline s;
                       flush stdout)
                     ())
              in
              ignore result;
              match Plan_pipeline.get_pipeline ~db ~id:pipeline.id with
              | None -> "Pipeline complete."
              | Some p -> Plan_pipeline.format_pipeline_summary p)))
  | [ "show"; id_s ] -> (
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: pipeline id must be an integer"
      else
        match Plan_pipeline.get_pipeline ~db ~id with
        | None -> Printf.sprintf "No pipeline found with id %d" id
        | Some p -> Plan_pipeline.format_pipeline_summary p)
  | "logs" :: rest -> (
      let id, lines =
        let rec loop id lines = function
          | [] -> (id, lines)
          | "--lines" :: n :: rest -> (
              try loop id (int_of_string n) rest with _ -> loop id lines rest)
          | v :: rest -> (
              try loop (Some (int_of_string v)) lines rest
              with _ -> loop id lines rest)
        in
        loop None 50 rest
      in
      match id with
      | None -> "Usage: clawq plan logs <id> [--lines N]"
      | Some id -> (
          match Plan_pipeline.get_pipeline ~db ~id with
          | None -> Printf.sprintf "No pipeline found with id %d" id
          | Some p -> (
              match p.Plan_pipeline.current_bg_task_id with
              | None ->
                  Printf.sprintf
                    "Pipeline %d has no background task (stage: %s)." id
                    (Plan_pipeline.string_of_stage p.stage)
              | Some task_id -> (
                  match Background_task.get_task ~db ~id:task_id with
                  | None ->
                      Printf.sprintf "Background task %d not found." task_id
                  | Some task -> (
                      match
                        Background_task.log_excerpt ~offset:0 ~lines task
                      with
                      | Ok text -> text
                      | Error msg -> "Error: " ^ msg)))))
  | [ "cancel"; id_s ] -> (
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: pipeline id must be an integer"
      else
        match Plan_pipeline.cancel_pipeline ~db ~id with
        | Ok msg -> msg
        | Error msg -> "Error: " ^ msg)
  | _ ->
      "Usage: clawq plan <start|list|show|logs|cancel>\n\
      \  plan start <PROMPT> [--repo PATH] [--runner NAME]\n\
      \            [--planner-model M] [--reviewer-model M] [--coder-model M]\n\
      \            [--max-plan-review-iters N] [--max-code-review-iters N]\n\
      \            [--no-plan-review] [--no-code-review]\n\
      \  plan list                              - List all pipelines\n\
      \  plan show <id>                         - Show pipeline details\n\
      \  plan logs <id> [--lines N]             - Show stage logs\n\
      \  plan cancel <id>                       - Cancel pipeline"

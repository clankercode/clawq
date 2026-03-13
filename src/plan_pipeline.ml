type stage =
  | Planning
  | PlanReview of int
  | Coding
  | CodeReview of int
  | Done
  | PipelineFailed of string

type model_config = {
  planner_model : string option;
  reviewer_model : string option;
  coder_model : string option;
  max_plan_review_iters : int;
  max_code_review_iters : int;
}

type pipeline = {
  id : int;
  prompt : string;
  repo_path : string;
  pipeline_dir : string;
  stage : stage;
  status : string;
  planner_model : string option;
  reviewer_model : string option;
  coder_model : string option;
  max_plan_review_iters : int;
  max_code_review_iters : int;
  current_bg_task_id : int option;
  coder_worktree_path : string option;
  error_msg : string option;
  created_at : string;
  updated_at : string;
}

let default_model_config =
  {
    planner_model = None;
    reviewer_model = None;
    coder_model = None;
    max_plan_review_iters = 3;
    max_code_review_iters = 3;
  }

let pipeline_dir_root () = Dot_dir.sub "plans"
let plan_file_path pipeline = Filename.concat pipeline.pipeline_dir "plan.md"

let plan_hash_file pipeline =
  Filename.concat pipeline.pipeline_dir ".plan_hash_before"

let string_of_stage = function
  | Planning -> "planning"
  | PlanReview n -> Printf.sprintf "plan_review_%d" n
  | Coding -> "coding"
  | CodeReview n -> Printf.sprintf "code_review_%d" n
  | Done -> "done"
  | PipelineFailed msg -> "failed:" ^ msg

let stage_of_string s =
  match s with
  | "planning" -> Planning
  | "coding" -> Coding
  | "done" -> Done
  | _ ->
      let pfx_plan = "plan_review_" in
      let pfx_code = "code_review_" in
      let pfx_fail = "failed:" in
      let starts s pfx =
        String.length s >= String.length pfx
        && String.sub s 0 (String.length pfx) = pfx
      in
      if starts s pfx_plan then
        let rest =
          String.sub s (String.length pfx_plan)
            (String.length s - String.length pfx_plan)
        in
        try PlanReview (int_of_string rest)
        with _ -> PipelineFailed ("invalid stage: " ^ s)
      else if starts s pfx_code then
        let rest =
          String.sub s (String.length pfx_code)
            (String.length s - String.length pfx_code)
        in
        try CodeReview (int_of_string rest)
        with _ -> PipelineFailed ("invalid stage: " ^ s)
      else if starts s pfx_fail then
        let rest =
          String.sub s (String.length pfx_fail)
            (String.length s - String.length pfx_fail)
        in
        PipelineFailed rest
      else Planning

let ensure_dir path =
  try if not (Sys.file_exists path) then Unix.mkdir path 0o755
  with Unix.Unix_error _ -> ()

let sql_text = function Sqlite3.Data.TEXT s -> Some s | _ -> None
let sql_int = function Sqlite3.Data.INT n -> Some (Int64.to_int n) | _ -> None

let pipeline_of_stmt stmt =
  let stage_s =
    Sqlite3.column stmt 4 |> sql_text |> Option.value ~default:"planning"
  in
  {
    id = Sqlite3.column stmt 0 |> sql_int |> Option.value ~default:0;
    prompt = Sqlite3.column stmt 1 |> sql_text |> Option.value ~default:"";
    repo_path = Sqlite3.column stmt 2 |> sql_text |> Option.value ~default:"";
    pipeline_dir = Sqlite3.column stmt 3 |> sql_text |> Option.value ~default:"";
    stage = stage_of_string stage_s;
    status =
      Sqlite3.column stmt 5 |> sql_text |> Option.value ~default:"running";
    planner_model = Sqlite3.column stmt 6 |> sql_text;
    reviewer_model = Sqlite3.column stmt 7 |> sql_text;
    coder_model = Sqlite3.column stmt 8 |> sql_text;
    max_plan_review_iters =
      Sqlite3.column stmt 9 |> sql_int |> Option.value ~default:3;
    max_code_review_iters =
      Sqlite3.column stmt 10 |> sql_int |> Option.value ~default:3;
    current_bg_task_id = Sqlite3.column stmt 11 |> sql_int;
    coder_worktree_path = Sqlite3.column stmt 12 |> sql_text;
    error_msg = Sqlite3.column stmt 13 |> sql_text;
    created_at = Sqlite3.column stmt 14 |> sql_text |> Option.value ~default:"";
    updated_at = Sqlite3.column stmt 15 |> sql_text |> Option.value ~default:"";
  }

let init_schema db =
  let exec sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | rc ->
        failwith
          (Printf.sprintf "SQLite error: %s (sql: %s)" (Sqlite3.Rc.to_string rc)
             sql)
  in
  exec
    "CREATE TABLE IF NOT EXISTS plan_pipelines (\n\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \  prompt TEXT NOT NULL,\n\
    \  repo_path TEXT NOT NULL,\n\
    \  pipeline_dir TEXT,\n\
    \  stage TEXT NOT NULL DEFAULT 'planning',\n\
    \  status TEXT NOT NULL DEFAULT 'running',\n\
    \  planner_model TEXT,\n\
    \  reviewer_model TEXT,\n\
    \  coder_model TEXT,\n\
    \  max_plan_review_iters INTEGER NOT NULL DEFAULT 3,\n\
    \  max_code_review_iters INTEGER NOT NULL DEFAULT 3,\n\
    \  current_bg_task_id INTEGER,\n\
    \  coder_worktree_path TEXT,\n\
    \  error_msg TEXT,\n\
    \  created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \  updated_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
     )";
  exec
    "CREATE INDEX IF NOT EXISTS idx_plan_pipelines_status ON plan_pipelines \
     (status)"

let create ~db ~prompt ~repo_path ~(model_config : model_config) =
  let root = pipeline_dir_root () in
  ensure_dir root;
  let sql =
    "INSERT INTO plan_pipelines (prompt, repo_path, planner_model, \
     reviewer_model, coder_model, max_plan_review_iters, \
     max_code_review_iters) VALUES (?, ?, ?, ?, ?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind_opt idx = function
        | Some v -> ignore (Sqlite3.bind stmt idx (Sqlite3.Data.TEXT v))
        | None -> ignore (Sqlite3.bind stmt idx Sqlite3.Data.NULL)
      in
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT prompt));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT repo_path));
      bind_opt 3 model_config.planner_model;
      bind_opt 4 model_config.reviewer_model;
      bind_opt 5 model_config.coder_model;
      ignore
        (Sqlite3.bind stmt 6
           (Sqlite3.Data.INT (Int64.of_int model_config.max_plan_review_iters)));
      ignore
        (Sqlite3.bind stmt 7
           (Sqlite3.Data.INT (Int64.of_int model_config.max_code_review_iters)));
      ignore (Sqlite3.step stmt));
  let id = Int64.to_int (Sqlite3.last_insert_rowid db) in
  let pipeline_dir = Filename.concat root (Printf.sprintf "pipeline-%d" id) in
  let sql2 = "UPDATE plan_pipelines SET pipeline_dir = ? WHERE id = ?" in
  let stmt2 = Sqlite3.prepare db sql2 in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt2))
    (fun () ->
      ignore (Sqlite3.bind stmt2 1 (Sqlite3.Data.TEXT pipeline_dir));
      ignore (Sqlite3.bind stmt2 2 (Sqlite3.Data.INT (Int64.of_int id)));
      ignore (Sqlite3.step stmt2));
  {
    id;
    prompt;
    repo_path;
    pipeline_dir;
    stage = Planning;
    status = "running";
    planner_model = model_config.planner_model;
    reviewer_model = model_config.reviewer_model;
    coder_model = model_config.coder_model;
    max_plan_review_iters = model_config.max_plan_review_iters;
    max_code_review_iters = model_config.max_code_review_iters;
    current_bg_task_id = None;
    coder_worktree_path = None;
    error_msg = None;
    created_at = "";
    updated_at = "";
  }

let get_pipeline ~db ~id =
  let sql =
    "SELECT id, prompt, repo_path, COALESCE(pipeline_dir,''), stage, status, \
     planner_model, reviewer_model, coder_model, max_plan_review_iters, \
     max_code_review_iters, current_bg_task_id, coder_worktree_path, \
     error_msg, created_at, updated_at FROM plan_pipelines WHERE id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (pipeline_of_stmt stmt)
      | _ -> None)

let list_pipelines ~db =
  let sql =
    "SELECT id, prompt, repo_path, COALESCE(pipeline_dir,''), stage, status, \
     planner_model, reviewer_model, coder_model, max_plan_review_iters, \
     max_code_review_iters, current_bg_task_id, coder_worktree_path, \
     error_msg, created_at, updated_at FROM plan_pipelines ORDER BY id DESC"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        rows := pipeline_of_stmt stmt :: !rows
      done;
      List.rev !rows)

let update_pipeline_db ~db pipeline =
  let sql =
    "UPDATE plan_pipelines SET stage = ?, status = ?, current_bg_task_id = ?, \
     coder_worktree_path = ?, error_msg = ?, updated_at = datetime('now') \
     WHERE id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind_opt_text idx = function
        | Some v -> ignore (Sqlite3.bind stmt idx (Sqlite3.Data.TEXT v))
        | None -> ignore (Sqlite3.bind stmt idx Sqlite3.Data.NULL)
      in
      let bind_opt_int idx = function
        | Some v ->
            ignore (Sqlite3.bind stmt idx (Sqlite3.Data.INT (Int64.of_int v)))
        | None -> ignore (Sqlite3.bind stmt idx Sqlite3.Data.NULL)
      in
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.TEXT (string_of_stage pipeline.stage)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT pipeline.status));
      bind_opt_int 3 pipeline.current_bg_task_id;
      bind_opt_text 4 pipeline.coder_worktree_path;
      bind_opt_text 5 pipeline.error_msg;
      ignore (Sqlite3.bind stmt 6 (Sqlite3.Data.INT (Int64.of_int pipeline.id)));
      ignore (Sqlite3.step stmt))

let build_stage_prompt ~stage ~pipeline =
  let plan_file = plan_file_path pipeline in
  let repo = pipeline.repo_path in
  match stage with
  | Planning ->
      String.concat "\n"
        [
          "You are a planning agent. Your task: " ^ pipeline.prompt;
          "";
          "Repository: " ^ repo;
          "Plan file to write: " ^ plan_file;
          "";
          "Explore the repository, then write a comprehensive plan to "
          ^ plan_file ^ ".";
          "The plan MUST include sections: ## Overview, ## Relevant Files \
           (with line numbers),";
          "## Implementation Steps (numbered), ## Testing Approach, ## \
           Potential Footguns.";
          "Make NO code changes. When done, output: PLAN_WRITTEN";
        ]
  | PlanReview _n ->
      String.concat "\n"
        [
          "You are a plan review agent. Improve the implementation plan.";
          "";
          "Plan: " ^ plan_file;
          "Repository: " ^ repo ^ "  (read-only reference)";
          "";
          "Read the plan. Check: logical gaps, incorrect file paths, missing \
           error handling,";
          "steps that contradict the codebase. Edit " ^ plan_file
          ^ " with improvements.";
          "If no improvements are needed, output exactly: PLAN_STABLE";
          "Make NO code changes.";
        ]
  | Coding ->
      let coder_wt = Option.value ~default:repo pipeline.coder_worktree_path in
      String.concat "\n"
        [
          "You are a coding agent. Implement the plan.";
          "";
          "Plan: " ^ plan_file;
          "Repository: " ^ repo;
          "Working directory: " ^ coder_wt;
          "";
          "Read the plan, implement every step. Follow existing code style.";
          "Run tests after implementation to verify correctness.";
        ]
  | CodeReview _n ->
      let coder_wt = Option.value ~default:repo pipeline.coder_worktree_path in
      String.concat "\n"
        [
          "You are a code review agent. Review and fix the implementation.";
          "";
          "Plan: " ^ plan_file;
          "Working directory: " ^ coder_wt;
          "";
          "Read the plan, check the implementation, run tests.";
          "Fix any bugs, missing cases, or test failures.";
          "If everything passes: output exactly: CODE_STABLE";
        ]
  | Done -> "Pipeline is done."
  | PipelineFailed msg -> "Pipeline failed: " ^ msg

let read_log_tail path max_chars =
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let len = in_channel_length ic in
        let start = max 0 (len - max_chars) in
        seek_in ic start;
        String.trim (really_input_string ic (len - start)))
  with _ -> ""

let contains_substring haystack needle =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let rec loop i =
      if i + nlen > hlen then false
      else if String.sub haystack i nlen = needle then true
      else loop (i + 1)
    in
    loop 0

let save_plan_hash pipeline =
  let path = plan_file_path pipeline in
  if Sys.file_exists path then
    try
      let hash = Digest.file path in
      let hf = plan_hash_file pipeline in
      let oc = open_out hf in
      output_string oc hash;
      close_out oc
    with _ -> ()

let check_plan_stable ~pipeline ~bg_task =
  (* Check log/preview for stability marker *)
  let has_marker =
    let log_marker =
      match bg_task.Background_task.log_path with
      | Some path when Sys.file_exists path ->
          let content = read_log_tail path 4096 in
          contains_substring content "PLAN_STABLE"
          || contains_substring content "CODE_STABLE"
      | _ -> false
    in
    let preview_marker =
      match bg_task.Background_task.result_preview with
      | Some p ->
          contains_substring p "PLAN_STABLE"
          || contains_substring p "CODE_STABLE"
      | None -> false
    in
    log_marker || preview_marker
  in
  if has_marker then Lwt.return true
  else
    (* Check if plan.md hash is unchanged from before the review *)
    let plan_path = plan_file_path pipeline in
    let hash_file = plan_hash_file pipeline in
    if Sys.file_exists plan_path && Sys.file_exists hash_file then
      try
        let current_hash = Digest.file plan_path in
        let ic = open_in hash_file in
        let before_hash = really_input_string ic (in_channel_length ic) in
        close_in ic;
        Lwt.return (current_hash = before_hash)
      with _ -> Lwt.return false
    else Lwt.return false

let model_for_stage pipeline = function
  | Planning -> pipeline.planner_model
  | PlanReview _ | CodeReview _ -> pipeline.reviewer_model
  | Coding -> pipeline.coder_model
  | Done | PipelineFailed _ -> None

let advance_stage ~pipeline ~bg_task ~is_stable =
  match pipeline.stage with
  | Planning ->
      if pipeline.max_plan_review_iters > 0 then PlanReview 0 else Coding
  | PlanReview n ->
      if is_stable || n + 1 >= pipeline.max_plan_review_iters then Coding
      else PlanReview (n + 1)
  | Coding -> if pipeline.max_code_review_iters > 0 then CodeReview 0 else Done
  | CodeReview n ->
      if is_stable || n + 1 >= pipeline.max_code_review_iters then Done
      else CodeReview (n + 1)
  | Done -> Done
  | PipelineFailed _ as s ->
      ignore bg_task;
      s

let run_foreground ~db ~pipeline ~runner ~on_progress () =
  let open Lwt.Syntax in
  ensure_dir (pipeline_dir_root ());
  ensure_dir pipeline.pipeline_dir;
  let rec loop pipeline =
    match pipeline.stage with
    | Done ->
        on_progress "Pipeline complete.";
        update_pipeline_db ~db { pipeline with status = "done" };
        Lwt.return ()
    | PipelineFailed msg ->
        on_progress (Printf.sprintf "Pipeline failed: %s" msg);
        let current_status =
          match get_pipeline ~db ~id:pipeline.id with
          | Some p -> p.status
          | None -> pipeline.status
        in
        if current_status <> "cancelled" then
          update_pipeline_db ~db
            { pipeline with status = "failed"; error_msg = Some msg };
        Lwt.return ()
    | stage -> (
        on_progress
          (Printf.sprintf "[plan-pipeline %d] stage: %s" pipeline.id
             (string_of_stage stage));
        (* Save plan hash before review stages *)
        (match stage with
        | PlanReview _ | CodeReview _ -> save_plan_hash pipeline
        | _ -> ());
        let prompt = build_stage_prompt ~stage ~pipeline in
        let model = model_for_stage pipeline stage in
        match
          Background_task.enqueue ~db ~runner ?model
            ~repo_path:pipeline.repo_path ~prompt ()
        with
        | Error msg ->
            let next = PipelineFailed ("enqueue failed: " ^ msg) in
            let p = { pipeline with stage = next } in
            loop p
        | Ok task_id -> (
            let p = { pipeline with current_bg_task_id = Some task_id } in
            update_pipeline_db ~db p;
            match Background_task.get_task ~db ~id:task_id with
            | None ->
                let next = PipelineFailed "task vanished after enqueue" in
                loop { p with stage = next }
            | Some task -> (
                Background_task.spawn_task ~db task;
                let* wait_result =
                  Background_task.wait_until_terminal ~timeout_seconds:1800.0
                    ~db ~id:task_id ()
                in
                let finished_task = Background_task.get_task ~db ~id:task_id in
                let bg_task =
                  Option.value
                    ~default:
                      {
                        task with
                        Background_task.status = Background_task.Failed;
                      }
                    finished_task
                in
                (* For Coding: capture worktree path *)
                let p =
                  match stage with
                  | Coding ->
                      {
                        p with
                        coder_worktree_path =
                          bg_task.Background_task.worktree_path;
                      }
                  | _ -> p
                in
                let* is_stable =
                  match stage with
                  | PlanReview _ | CodeReview _ ->
                      check_plan_stable ~pipeline:p ~bg_task
                  | _ -> Lwt.return false
                in
                match wait_result with
                | Background_task.Not_found ->
                    loop
                      {
                        p with
                        stage =
                          PipelineFailed
                            (Printf.sprintf "task %d not found in DB" task_id);
                      }
                | Background_task.Timeout _ ->
                    loop
                      {
                        p with
                        stage =
                          PipelineFailed
                            (Printf.sprintf "task %d timed out after 30 minutes"
                               task_id);
                      }
                | Background_task.Interrupted _ ->
                    (* Interrupted mid-poll; resume waiting on the next loop *)
                    loop p
                | Background_task.Finished finished -> (
                    match finished.Background_task.status with
                    | Background_task.Cancelled ->
                        loop
                          {
                            p with
                            stage =
                              PipelineFailed
                                (Printf.sprintf "task %d was cancelled" task_id);
                          }
                    | Background_task.DirtyWorktree ->
                        loop
                          {
                            p with
                            stage =
                              PipelineFailed
                                (Printf.sprintf
                                   "task %d completed but left uncommitted \
                                    worktree changes"
                                   task_id);
                          }
                    | Background_task.Failed -> (
                        match stage with
                        | PlanReview _ | CodeReview _ ->
                            (* Review agents may exit non-zero but still have
                               made changes; advance normally *)
                            let next_stage =
                              advance_stage ~pipeline:p ~bg_task ~is_stable
                            in
                            let p = { p with stage = next_stage } in
                            update_pipeline_db ~db p;
                            loop p
                        | _ ->
                            loop
                              {
                                p with
                                stage =
                                  PipelineFailed
                                    (Printf.sprintf
                                       "task %d failed (exit non-zero)" task_id);
                              })
                    | _ ->
                        let next_stage =
                          advance_stage ~pipeline:p ~bg_task ~is_stable
                        in
                        let p = { p with stage = next_stage } in
                        update_pipeline_db ~db p;
                        loop p))))
  in
  loop pipeline

let format_pipeline_summary pipeline =
  let stage_s = string_of_stage pipeline.stage in
  let bg_task_s =
    match pipeline.current_bg_task_id with
    | Some id -> Printf.sprintf " (bg task %d)" id
    | None -> ""
  in
  let coder_wt_s =
    match pipeline.coder_worktree_path with
    | Some p -> Printf.sprintf "\n  coder_worktree: %s" p
    | None -> ""
  in
  let err_s =
    match pipeline.error_msg with
    | Some e -> Printf.sprintf "\n  error: %s" e
    | None -> ""
  in
  Printf.sprintf
    "Pipeline %d  [%s]  status=%s%s\n\
    \  prompt: %s\n\
    \  repo: %s\n\
    \  plan_file: %s%s%s\n\
    \  created: %s"
    pipeline.id stage_s pipeline.status bg_task_s
    (if String.length pipeline.prompt > 60 then
       String.sub pipeline.prompt 0 60 ^ "..."
     else pipeline.prompt)
    pipeline.repo_path (plan_file_path pipeline) coder_wt_s err_s
    pipeline.created_at

let format_pipeline_list pipelines =
  if pipelines = [] then "No plan pipelines."
  else
    let header =
      Printf.sprintf "  %-5s %-12s %-10s %s" "ID" "STAGE" "STATUS" "PROMPT"
    in
    let rows =
      List.map
        (fun p ->
          let stage_s =
            let s = string_of_stage p.stage in
            if String.length s > 12 then String.sub s 0 12 else s
          in
          let prompt_s =
            if String.length p.prompt > 40 then String.sub p.prompt 0 40 ^ "..."
            else p.prompt
          in
          Printf.sprintf "  %-5d %-12s %-10s %s" p.id stage_s p.status prompt_s)
        pipelines
    in
    "Plan pipelines:\n" ^ header ^ "\n" ^ String.concat "\n" rows

let cancel_pipeline ~db ~id =
  match get_pipeline ~db ~id with
  | None -> Error (Printf.sprintf "No pipeline found with id %d" id)
  | Some p when p.status = "done" || p.status = "failed" ->
      Error (Printf.sprintf "Pipeline %d is already %s" id p.status)
  | Some p when p.status = "cancelled" ->
      Error (Printf.sprintf "Pipeline %d is already cancelled" id)
  | Some p ->
      (* Cancel current bg task if any *)
      (match p.current_bg_task_id with
      | Some task_id -> ignore (Background_task.cancel ~db ~id:task_id)
      | None -> ());
      let sql =
        "UPDATE plan_pipelines SET status = 'cancelled', updated_at = \
         datetime('now') WHERE id = ?"
      in
      let stmt = Sqlite3.prepare db sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id)));
          ignore (Sqlite3.step stmt));
      Ok (Printf.sprintf "Pipeline %d cancelled." id)

(* Tools *)

let start_tool ~db ~default_repo_path =
  {
    Tool.name = "plan_pipeline_start";
    description =
      "Start a multi-stage planning pipeline: planner agent writes a plan, \
       plan-review agents improve it, then a coder agent implements it, \
       followed by code-review agents. The pipeline runs in foreground \
       (blocking) and writes the plan to ~/.clawq/plans/pipeline-N/plan.md.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "prompt",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "The goal or feature to plan and implement." );
                    ] );
                ( "repo_path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Absolute path to the git repository. Defaults to \
                           the workspace path." );
                    ] );
                ( "runner",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Which external coding CLI to use: codex, claude, \
                           kimi, gemini, opencode, cursor. Defaults to \
                           auto-selected." );
                    ] );
                ( "planner_model",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Model for planning stages.");
                    ] );
                ( "reviewer_model",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Model for review stages.");
                    ] );
                ( "coder_model",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Model for coding stages.");
                    ] );
                ( "max_plan_review_iters",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "Max plan review iterations (default 3, 0 = skip \
                           review)." );
                    ] );
                ( "max_code_review_iters",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "Max code review iterations (default 3, 0 = skip \
                           review)." );
                    ] );
              ] );
          ("required", `List [ `String "prompt" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let prompt = try args |> member "prompt" |> to_string with _ -> "" in
        if String.trim prompt = "" then
          Lwt.return
            "Error: parameter \"prompt\" is required and must be a non-empty \
             string."
        else
          let repo_path =
            try
              match args |> member "repo_path" with
              | `String s when String.trim s <> "" -> String.trim s
              | _ -> default_repo_path
            with _ -> default_repo_path
          in
          let runner_s =
            try
              match args |> member "runner" with
              | `String s -> String.trim s
              | _ -> ""
            with _ -> ""
          in
          let runner_opt =
            if runner_s = "" then None
            else Background_task.runner_of_string runner_s
          in
          let get_opt_str field =
            try
              match args |> member field with
              | `String s when String.trim s <> "" -> Some (String.trim s)
              | _ -> None
            with _ -> None
          in
          let get_opt_int field default =
            try
              match args |> member field with
              | `Int n -> n
              | `Float f -> int_of_float f
              | _ -> default
            with _ -> default
          in
          let model_config =
            {
              planner_model = get_opt_str "planner_model";
              reviewer_model = get_opt_str "reviewer_model";
              coder_model = get_opt_str "coder_model";
              max_plan_review_iters = get_opt_int "max_plan_review_iters" 3;
              max_code_review_iters = get_opt_int "max_code_review_iters" 3;
            }
          in
          match Background_task.resolve_runner ?preferred:runner_opt () with
          | Error msg ->
              Lwt.return
                (Printf.sprintf
                   "Error: could not resolve runner: %s. Pass runner=\"auto\" \
                    or specify one of: codex, claude, kimi, gemini, opencode, \
                    cursor."
                   msg)
          | Ok (runner, _) ->
              init_schema db;
              Background_task.init_schema db;
              let pipeline = create ~db ~prompt ~repo_path ~model_config in
              let progress_buf = Buffer.create 256 in
              let on_progress s =
                Buffer.add_string progress_buf s;
                Buffer.add_char progress_buf '\n'
              in
              let open Lwt.Syntax in
              let* () = run_foreground ~db ~pipeline ~runner ~on_progress () in
              Lwt.return
                (Printf.sprintf "Pipeline %d complete.\n%s\nPlan file: %s"
                   pipeline.id
                   (Buffer.contents progress_buf)
                   (plan_file_path pipeline)));
    invoke_stream = None;
    risk_level = Tool.Medium;
    deferred = false;
  }

let status_tool ~db =
  {
    Tool.name = "plan_pipeline_status";
    description = "Get the current status and stage of a planning pipeline.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "id",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ("description", `String "Pipeline id to inspect.");
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        if id < 0 then
          Lwt.return
            "Error: parameter \"id\" must be a positive integer identifying \
             the pipeline."
        else (
          init_schema db;
          match get_pipeline ~db ~id with
          | None ->
              Lwt.return
                (Printf.sprintf
                   "No pipeline found with id %d. Use plan_pipeline_list to \
                    see available pipelines."
                   id)
          | Some p -> Lwt.return (format_pipeline_summary p)));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

let list_tool ~db =
  {
    Tool.name = "plan_pipeline_list";
    description = "List recent planning pipelines and their current status.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ("properties", `Assoc []);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ _args ->
        init_schema db;
        let pipelines = list_pipelines ~db in
        Lwt.return (format_pipeline_list pipelines));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

let logs_tool ~db =
  {
    Tool.name = "plan_pipeline_logs";
    description =
      "Read the log output from a planning pipeline stage. Shows the log from \
       the most recent background task for that pipeline.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "id",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ("description", `String "Pipeline id.");
                    ] );
                ( "lines",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String "Number of log lines to return (default 50)." );
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        let lines = try args |> member "lines" |> to_int with _ -> 50 in
        if id < 0 then
          Lwt.return
            "Error: parameter \"id\" must be a positive integer pipeline id."
        else (
          init_schema db;
          Background_task.init_schema db;
          match get_pipeline ~db ~id with
          | None ->
              Lwt.return
                (Printf.sprintf
                   "No pipeline found with id %d. Use plan_pipeline_list to \
                    see available pipelines."
                   id)
          | Some p -> (
              match p.current_bg_task_id with
              | None ->
                  Lwt.return
                    (Printf.sprintf
                       "Pipeline %d has no background task yet (stage: %s)." id
                       (string_of_stage p.stage))
              | Some task_id -> (
                  match Background_task.get_task ~db ~id:task_id with
                  | None ->
                      Lwt.return
                        (Printf.sprintf
                           "Background task %d not found for pipeline %d."
                           task_id id)
                  | Some task -> (
                      match
                        Background_task.log_excerpt ~offset:0 ~lines task
                      with
                      | Ok text -> Lwt.return text
                      | Error msg -> Lwt.return ("Error reading log: " ^ msg))))));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

let cancel_tool ~db =
  {
    Tool.name = "plan_pipeline_cancel";
    description =
      "Cancel a running planning pipeline and its current background task.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "id",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ("description", `String "Pipeline id to cancel.");
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        if id < 0 then
          Lwt.return
            "Error: parameter \"id\" must be a positive integer pipeline id."
        else (
          init_schema db;
          Background_task.init_schema db;
          match cancel_pipeline ~db ~id with
          | Ok msg -> Lwt.return msg
          | Error msg -> Lwt.return ("Error: " ^ msg)));
    invoke_stream = None;
    risk_level = Tool.Medium;
    deferred = false;
  }

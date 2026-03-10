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

val default_model_config : model_config
val pipeline_dir_root : unit -> string
val plan_file_path : pipeline -> string
val string_of_stage : stage -> string
val stage_of_string : string -> stage
val init_schema : Sqlite3.db -> unit

val create :
  db:Sqlite3.db ->
  prompt:string ->
  repo_path:string ->
  model_config:model_config ->
  pipeline

val get_pipeline : db:Sqlite3.db -> id:int -> pipeline option
val list_pipelines : db:Sqlite3.db -> pipeline list
val build_stage_prompt : stage:stage -> pipeline:pipeline -> string

val check_plan_stable :
  pipeline:pipeline -> bg_task:Background_task.task -> bool Lwt.t

val save_plan_hash : pipeline -> unit

val advance_stage :
  pipeline:pipeline -> bg_task:Background_task.task -> is_stable:bool -> stage

val run_foreground :
  db:Sqlite3.db ->
  pipeline:pipeline ->
  runner:Background_task.runner ->
  on_progress:(string -> unit) ->
  unit ->
  unit Lwt.t

val format_pipeline_summary : pipeline -> string
val format_pipeline_list : pipeline list -> string
val cancel_pipeline : db:Sqlite3.db -> id:int -> (string, string) result
val start_tool : db:Sqlite3.db -> default_repo_path:string -> Tool.t
val status_tool : db:Sqlite3.db -> Tool.t
val list_tool : db:Sqlite3.db -> Tool.t
val logs_tool : db:Sqlite3.db -> Tool.t
val cancel_tool : db:Sqlite3.db -> Tool.t

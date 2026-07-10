type background_add_args = {
  runner : Background_task.runner;
  model : string option;
  repo_path : string;
  branch : string option;
  agent_name : string option;
  host_kind : string option;
  prompt : string;
}

type background_wait_args = { id : int; timeout_seconds : float }

type background_logs_args = {
  id : int;
  lines : int;
  offset : int;
  follow : bool;
}

type delegate_args = {
  preferred_runner : Background_task.runner option;
  model : string option;
  repo_path : string option;
  branch : string option;
  goal : string;
  use_worktree : bool;
}

let path_is_git_repo path =
  Sys.command
    (Printf.sprintf "git -C %s rev-parse --is-inside-work-tree >/dev/null 2>&1"
       (Filename.quote path))
  = 0

let default_delegate_repo_path (cfg : Runtime_config.t) =
  let cwd = Sys.getcwd () in
  if path_is_git_repo cwd then cwd else Runtime_config.effective_workspace cfg

let parse_background_add_args args =
  let rec loop model branch agent_name host_kind positionals = function
    | [] -> (
        let positionals = List.rev positionals in
        match positionals with
        | runner_s :: repo_path :: prompt_parts -> (
            match Background_task.runner_of_string runner_s with
            | None ->
                Error
                  "Runner must be one of: codex, claude (or claude-code), \
                   kimi, gemini, opencode, cursor (or cursor-cli), local"
            | Some runner ->
                let prompt = String.concat " " prompt_parts |> String.trim in
                if prompt = "" then Error "Prompt is required"
                else
                  Ok
                    {
                      runner;
                      model;
                      repo_path;
                      branch;
                      agent_name;
                      host_kind;
                      prompt;
                    })
        | _ ->
            Error
              "Usage: clawq background add \
               <codex|claude|kimi|gemini|opencode|cursor|local> [--model \
               <name>] [--agent <name>] [--host <direct|herdr|tmux>] <repo> \
               [--branch <name>] <prompt>")
    | "--model" :: value :: rest ->
        loop (Some value) branch agent_name host_kind positionals rest
    | "--branch" :: value :: rest ->
        loop model (Some value) agent_name host_kind positionals rest
    | "--agent" :: value :: rest ->
        loop model branch (Some value) host_kind positionals rest
    | "--host" :: value :: rest ->
        loop model branch agent_name (Some value) positionals rest
    | arg :: rest ->
        loop model branch agent_name host_kind (arg :: positionals) rest
  in
  loop None None None None [] args

let parse_background_wait_args args =
  let rec loop timeout id = function
    | [] -> (
        match id with
        | Some id -> Ok { id; timeout_seconds = timeout }
        | None ->
            Error "Usage: clawq background wait <id> [--timeout <seconds>]")
    | "--timeout" :: seconds :: rest -> (
        try loop (float_of_string seconds) id rest
        with _ -> Error "Timeout must be a number")
    | arg :: rest -> (
        match id with
        | Some _ ->
            Error "Usage: clawq background wait <id> [--timeout <seconds>]"
        | None -> (
            try loop timeout (Some (int_of_string arg)) rest
            with _ -> Error "Background task id must be an integer"))
  in
  loop 180.0 None args

let parse_background_logs_args args =
  let usage =
    "Usage: clawq background logs <id> [--lines <count>] [--offset <line>] \
     [--follow]"
  in
  let rec loop lines offset follow id = function
    | [] -> (
        match id with
        | Some id -> Ok { id; lines; offset; follow }
        | None -> Error usage)
    | "--lines" :: count :: rest -> (
        try loop (max 1 (int_of_string count)) offset follow id rest
        with _ -> Error "Log line count must be an integer")
    | "--offset" :: off :: rest -> (
        try loop lines (max 1 (int_of_string off)) follow id rest
        with _ -> Error "Offset must be a positive integer")
    | ("--follow" | "-f") :: rest -> loop lines offset true id rest
    | arg :: rest -> (
        match id with
        | Some _ -> Error usage
        | None -> (
            try loop lines offset follow (Some (int_of_string arg)) rest
            with _ -> Error "Background task id must be an integer"))
  in
  loop 40 0 false None args

let parse_delegate_args args =
  let rec loop preferred_runner model repo_path branch use_worktree positionals
      = function
    | [] ->
        let goal = String.concat " " (List.rev positionals) |> String.trim in
        if goal = "" then
          Error
            "Usage: clawq delegate [--runner \
             auto|kimi|opencode|codex|claude|gemini|cursor] [--model <name>] \
             [--repo <path>] [--branch <name>] [--no-worktree] <goal>"
        else
          Ok { preferred_runner; model; repo_path; branch; goal; use_worktree }
    | "--runner" :: value :: rest ->
        let value = String.lowercase_ascii (String.trim value) in
        let preferred_runner =
          if value = "" || value = "auto" then None
          else Background_task.runner_of_string value
        in
        if value <> "auto" && preferred_runner = None then
          Error
            "Runner must be one of: auto, codex, claude, kimi, gemini, \
             opencode, cursor"
        else
          loop preferred_runner model repo_path branch use_worktree positionals
            rest
    | "--model" :: value :: rest ->
        loop preferred_runner (Some value) repo_path branch use_worktree
          positionals rest
    | "--repo" :: value :: rest ->
        loop preferred_runner model (Some value) branch use_worktree positionals
          rest
    | "--branch" :: value :: rest ->
        loop preferred_runner model repo_path (Some value) use_worktree
          positionals rest
    | "--no-worktree" :: rest ->
        (* B649: opt out of git-worktree isolation so delegate accepts non-git
           paths (e.g. plain ~/.clawq/workspace runs). *)
        loop preferred_runner model repo_path branch false positionals rest
    | arg :: rest ->
        loop preferred_runner model repo_path branch use_worktree
          (arg :: positionals) rest
  in
  loop None None None None true [] args

let format_background_task_details (task : Background_task.task) =
  let add line acc = line :: acc in
  let lines = ref [] in
  lines := add (Printf.sprintf "id: %d" task.id) !lines;
  lines :=
    add
      (Printf.sprintf "runner: %s"
         (Background_task.string_of_runner task.runner))
      !lines;
  lines :=
    add
      (Printf.sprintf "status: %s"
         (Background_task.string_of_status task.status))
      !lines;
  let health = Background_task.diagnose_health task in
  (match health with
  | Background_task.Not_applicable -> ()
  | _ ->
      lines :=
        add
          (Printf.sprintf "health: %s"
             (Background_task.string_of_health health))
          !lines);
  lines := add (Printf.sprintf "repo: %s" task.repo_path) !lines;
  lines :=
    add
      (Printf.sprintf "branch: %s"
         (if task.branch = "" then "(auto)" else task.branch))
      !lines;
  lines := add (Printf.sprintf "created_at: %s" task.created_at) !lines;
  (match task.started_at with
  | Some value -> lines := add (Printf.sprintf "started_at: %s" value) !lines
  | None -> ());
  (match task.finished_at with
  | Some value -> lines := add (Printf.sprintf "finished_at: %s" value) !lines
  | None -> ());
  (match task.worktree_path with
  | Some value -> lines := add (Printf.sprintf "worktree: %s" value) !lines
  | None -> ());
  (match task.log_path with
  | Some value -> lines := add (Printf.sprintf "log: %s" value) !lines
  | None -> ());
  (match task.pid with
  | Some value -> lines := add (Printf.sprintf "pid: %d" value) !lines
  | None -> ());
  (match task.result_preview with
  | Some value when String.trim value <> "" ->
      lines := add (Printf.sprintf "result: %s" value) !lines
  | _ -> ());
  lines := add (Printf.sprintf "prompt: %s" task.prompt) !lines;
  String.concat "\n" (List.rev !lines)

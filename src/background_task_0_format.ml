type runner = Codex | Claude | Kimi | Gemini | Opencode | Cursor | Local

type status =
  | Queued
  | Running
  | Succeeded
  | Failed
  | DirtyWorktree
  | Cancelled

type health =
  | Active
  | Quiet
  | Stalled
  | Zombie
  | Process_missing
  | Log_stale
  | Startup_failed
  | Not_applicable

type task = {
  id : int;
  runner : runner;
  model : string option;
  repo_path : string;
  prompt : string;
  branch : string;
  worktree_path : string option;
  log_path : string option;
  status : status;
  runner_session_id : string option;
  session_key : string option;
  channel : string option;
  channel_id : string option;
  pid : int option;
  result_preview : string option;
  created_at : string;
  started_at : string option;
  finished_at : string option;
  automerge : bool;
  use_worktree : bool;
  merge_status : string option;
  retry_count : int;
  parent_task_id : int option;
  replaced_by : int option;
  acp : bool;
  agent_name : string option;
  notification_status : string option;
  notification_error : string option;
  notification_attempts : int;
  follow_up_prompt : string option;
      (** B488: optional prompt sent to the originating session as a new message
          once the task succeeds. Use cases: "make sure tests pass, run
          review-and-fix, commit and rebase against master" — a checklist the
          resumed session should execute. *)
}

type queued_message = {
  id : int;
  task_id : int;
  message : string;
  created_at : string;
}

let string_of_runner = function
  | Codex -> "codex"
  | Claude -> "claude"
  | Kimi -> "kimi"
  | Gemini -> "gemini"
  | Opencode -> "opencode"
  | Cursor -> "cursor"
  | Local -> "local"

let runner_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "codex" -> Some Codex
  | "claude" | "claude-code" | "claude_code" -> Some Claude
  | "kimi" -> Some Kimi
  | "gemini" -> Some Gemini
  | "opencode" -> Some Opencode
  | "cursor" | "cursor-cli" | "cursor_cli" | "cursor-agent" | "cursor_agent" ->
      Some Cursor
  | "local" -> Some Local
  | _ -> None

(* B487: shortname aliases that map to (runner, model). When a caller passes
   one of these as the runner argument, the daemon resolves it into both a
   runner choice AND a model override so prompts like "use opus" or "delegate
   to glm-5" work without the caller having to know the full provider+model
   syntax. Resolution is case-insensitive and trim-tolerant. *)
let runner_alias_table : (string * (runner * string)) list =
  [
    ("opus", (Claude, "claude-opus-4-6"));
    ("sonnet", (Claude, "claude-sonnet-4-6"));
    ("haiku", (Claude, "claude-haiku-4-5"));
    ("gpt-5", (Codex, "gpt-5.4"));
    ("gpt-5.4", (Codex, "gpt-5.4"));
    ("codex-spark", (Codex, "gpt-5.3-codex-spark"));
    ("glm-5", (Opencode, "zai-coding-plan:glm-5"));
    ("glm-5.1", (Opencode, "zai-coding-plan:glm-5.1"));
    ("kimi-coding", (Kimi, "kimi-for-coding"));
    ("k2", (Kimi, "kimi-for-coding"));
    ("k2.6", (Kimi, "kimi-k2.6"));
  ]

let runner_alias_of_string s =
  let key = String.lowercase_ascii (String.trim s) in
  List.assoc_opt key runner_alias_table

(* runner_of_string that ALSO recognises B487 aliases. Returns the
   (runner, optional model override) tuple. Bare runner names map to
   (runner, None). *)
let runner_and_model_of_string s : (runner * string option) option =
  match runner_of_string s with
  | Some r -> Some (r, None)
  | None -> (
      match runner_alias_of_string s with
      | Some (r, model) -> Some (r, Some model)
      | None -> None)

let string_of_status = function
  | Queued -> "queued"
  | Running -> "running"
  | Succeeded -> "succeeded"
  | Failed -> "failed"
  | DirtyWorktree -> "dirty_worktree"
  | Cancelled -> "cancelled"

let status_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "queued" -> Queued
  | "running" -> Running
  | "succeeded" -> Succeeded
  | "failed" -> Failed
  | "dirty_worktree" -> DirtyWorktree
  | "cancelled" -> Cancelled
  | _ -> Failed

let is_terminal_status = function
  | Succeeded | Failed | DirtyWorktree | Cancelled -> true
  | Queued | Running -> false

let max_retry_count = 3

let string_of_health = function
  | Active -> "active"
  | Quiet -> "quiet"
  | Stalled -> "stalled"
  | Zombie -> "zombie"
  | Process_missing -> "process-missing"
  | Log_stale -> "log-stale"
  | Startup_failed -> "startup-failed"
  | Not_applicable -> "-"

let runner_binary = function
  | Codex -> "codex"
  | Claude -> "claude"
  | Kimi -> "kimi"
  | Gemini -> "gemini"
  | Opencode -> "opencode"
  | Cursor -> "cursor-agent"
  | Local -> "clawq"

let command_exists command =
  Sys.command
    (Printf.sprintf "command -v %s >/dev/null 2>&1" (Filename.quote command))
  = 0

let path_is_git_repo path =
  Sys.command
    (Printf.sprintf "git -C %s rev-parse --is-inside-work-tree >/dev/null 2>&1"
       (Filename.quote path))
  = 0

let path_is_git_worktree path =
  let dot_git = Filename.concat path ".git" in
  path_is_git_repo path && Sys.file_exists dot_git
  && not (Sys.is_directory dot_git)

let validate_workspace_path path =
  if String.trim path = "" then Error "Path is required"
  else if not (Sys.file_exists path) then
    Error (Printf.sprintf "Path does not exist: %s" path)
  else if not (Sys.is_directory path) then
    Error (Printf.sprintf "Path is not a directory: %s" path)
  else Ok ()

let validate_repo_path ?(require_git = true) repo_path =
  match validate_workspace_path repo_path with
  | Error _ as err -> err
  | Ok () ->
      if require_git && not (path_is_git_repo repo_path) then
        Error
          (Printf.sprintf
             "Repository path is not a git repository: %s\n\
              Use a git repository path or run `git init` in the directory."
             repo_path)
      else Ok ()

let runner_available runner =
  runner = Local || command_exists (runner_binary runner)

let resolve_runner ?(check_available = true) ?preferred ?(allow_claude = true)
    () =
  let available runner = (not check_available) || runner_available runner in
  (* B606: when allow_claude is false (Anthropic OAuth opt-in not set), skip
     Claude both in auto selection and reject an explicit Claude preference
     with a clear error. *)
  match preferred with
  | Some Claude when not allow_claude ->
      Error
        "Runner 'claude' is disabled by \
         security.allow_anthropic_oauth_inference = false. Set this to true in \
         ~/.clawq/config.json to enable, or pick a different runner."
  | Some runner when available runner -> Ok (runner, None)
  | Some runner ->
      Error
        (Printf.sprintf "Runner '%s' is not available in PATH"
           (string_of_runner runner))
  | None when available Kimi -> Ok (Kimi, None)
  | None when available Cursor -> Ok (Cursor, None)
  | None when available Opencode -> Ok (Opencode, Some "zai-coding-plan/glm-5")
  | None when allow_claude && available Claude -> Ok (Claude, None)
  | None when available Codex -> Ok (Codex, None)
  | None when available Gemini -> Ok (Gemini, None)
  | None ->
      Error
        "No supported background runner is available in PATH (looked for \
         'kimi', 'cursor-agent', 'opencode', 'claude', 'codex', and 'gemini')"

let default_branch_name id = Printf.sprintf "clawq-bg-%d" id
let clawq_dir () = Dot_dir.path ()
let ensure_dir path = if Sys.file_exists path then () else Unix.mkdir path 0o755

let ensure_parent_dir path =
  let parent = Filename.dirname path in
  if parent <> path then ensure_dir parent

let worktree_root () = Filename.concat (clawq_dir ()) "background-worktrees"
let log_root () = Filename.concat (clawq_dir ()) "background-logs"

let task_worktree_path id =
  Filename.concat (worktree_root ()) (Printf.sprintf "task-%d" id)

let task_log_path id =
  Filename.concat (log_root ()) (Printf.sprintf "task-%d.log" id)

let preview_limit = 500
let compact_preview_limit = 200

let preview_text s =
  let trimmed = String.trim s in
  if String.length trimmed <= preview_limit then trimmed
  else String.sub trimmed 0 preview_limit ^ "..."

let preview_text_n limit s =
  let trimmed = String.trim s in
  if String.length trimmed <= limit then trimmed
  else String.sub trimmed 0 limit ^ "..."

let status_summary = function
  | Queued -> "queued"
  | Running -> "running"
  | Succeeded -> "succeeded"
  | Failed -> "failed"
  | DirtyWorktree -> "dirty-worktree"
  | Cancelled -> "cancelled"

let parse_sqlite_datetime s =
  try
    Scanf.sscanf s "%d-%d-%d %d:%d:%d" (fun y mo d h mi s ->
        (* Parse SQLite datetime as UTC (not local time) to get correct elapsed
           time regardless of system timezone. SQLite datetime('now') returns UTC. *)
        let year = y in
        let month = mo in
        let day = d in
        (* Convert to day number using Julian day calculation for UTC *)
        let a = (14 - month) / 12 in
        let yy = year + 4800 - a in
        let mm = month + (12 * a) - 3 in
        let day_num =
          day
          + (((153 * mm) + 2) / 5)
          + (365 * yy) + (yy / 4) - (yy / 100) + (yy / 400) - 32045
        in
        (* Julian day 2451545 is 2000-01-01, Unix epoch 1970-01-01 is JD 2440588 *)
        let unix_day = day_num - 2440588 in
        let seconds_since_epoch =
          (unix_day * 86400) + (h * 3600) + (mi * 60) + s
        in
        float_of_int seconds_since_epoch)
  with _ -> 0.0

let log_mtime path =
  try Some (Unix.stat path).Unix.st_mtime
  with Unix.Unix_error _ | Sys_error _ -> None

let stalled_threshold_seconds = 120.0
let log_stale_threshold_seconds = 90.0
let startup_timeout_seconds = 30.0

let log_has_content path =
  try (Unix.stat path).Unix.st_size > 0
  with Unix.Unix_error _ | Sys_error _ -> false

type local_task_state = { cancelled : bool ref }

let running : (int, local_task_state) Hashtbl.t = Hashtbl.create 16
let is_tracked_locally id = Hashtbl.mem running id

let diagnose_health ?(now = Unix.gettimeofday ())
    ?(pid_alive = fun pid -> Process_group.group_alive pid)
    ?(is_local_tracked = is_tracked_locally) (task : task) =
  match task.status with
  | Queued | Succeeded | Failed | DirtyWorktree | Cancelled -> Not_applicable
  | Running ->
      let is_local = task.runner = Local in
      let pid_alive =
        match task.pid with
        | Some pid when pid > 0 -> pid_alive pid
        | Some _pid when _pid = -1 -> is_local_tracked task.id
        | _ -> false
      in
      if not pid_alive then
        match task.pid with Some _ -> Zombie | None -> Process_missing
      else
        let started =
          match task.started_at with
          | Some s -> parse_sqlite_datetime s
          | None -> 0.0
        in
        let elapsed = if started > 0.0 then now -. started else 0.0 in
        let log_fresh =
          match task.log_path with
          | Some path -> (
              match log_mtime path with
              | Some mtime -> now -. mtime < log_stale_threshold_seconds
              | None -> false)
          | None -> false
        in
        let log_empty =
          match task.log_path with
          | Some path -> not (log_has_content path)
          | None -> true
        in
        if (not is_local) && log_empty && elapsed >= startup_timeout_seconds
        then Startup_failed
        else if log_fresh then Active
        else if elapsed < log_stale_threshold_seconds then Active
        else if elapsed >= stalled_threshold_seconds then Stalled
        else if is_local then Active
        else Log_stale

let format_elapsed_seconds secs =
  let secs = max 0 (int_of_float secs) in
  if secs < 60 then "<1m"
  else
    let mins = secs / 60 in
    let hours = mins / 60 in
    if hours = 0 then Printf.sprintf "%dm" mins
    else if hours >= 2 then "2h+"
    else Printf.sprintf "%dh%dm" hours (mins mod 60)

let format_age_seconds secs =
  let secs = max 0 (int_of_float secs) in
  if secs < 60 then "<1m"
  else
    let mins = secs / 60 in
    let hours = mins / 60 in
    let days = hours / 24 in
    if days > 0 then Printf.sprintf "%dd%dh" days (hours mod 24)
    else if hours > 0 then Printf.sprintf "%dh%dm" hours (mins mod 60)
    else Printf.sprintf "%dm" mins

let age_string (task : task) =
  let now = Unix.gettimeofday () in
  let created = parse_sqlite_datetime task.created_at in
  if created <= 0.0 then "-" else format_age_seconds (now -. created)

let runtime_string (task : task) =
  match task.started_at with
  | None -> "-"
  | Some started ->
      let start_time = parse_sqlite_datetime started in
      if start_time <= 0.0 then "-"
      else
        let end_time =
          match task.finished_at with
          | Some finished ->
              let t = parse_sqlite_datetime finished in
              if t > 0.0 then t else Unix.gettimeofday ()
          | None -> Unix.gettimeofday ()
        in
        format_elapsed_seconds (end_time -. start_time)

let format_task_summary ?(full = false) ?(compact = false) (task : task) =
  let branch = if task.branch = "" then "(auto)" else task.branch in
  let lines = ref [] in
  let add line = lines := line :: !lines in
  add (Printf.sprintf "task: %d" task.id);
  add (Printf.sprintf "runner: %s" (string_of_runner task.runner));
  (match task.model with
  | Some model when String.trim model <> "" ->
      add (Printf.sprintf "model: %s" model)
  | _ -> ());
  (match task.runner_session_id with
  | Some sid -> add (Printf.sprintf "runner_session: %s" sid)
  | None -> ());
  if task.acp then add "mode: acp";
  (match task.agent_name with
  | Some name -> add (Printf.sprintf "agent: %s" name)
  | None -> ());
  add (Printf.sprintf "status: %s" (status_summary task.status));
  if task.retry_count > 0 then
    add (Printf.sprintf "retries: %d/%d" task.retry_count max_retry_count);
  (match task.parent_task_id with
  | Some pid -> add (Printf.sprintf "parent_task: %d" pid)
  | None -> ());
  (match task.replaced_by with
  | Some rid -> add (Printf.sprintf "replaced_by: %d" rid)
  | None -> ());
  let health = diagnose_health task in
  (match health with
  | Not_applicable -> ()
  | _ -> add (Printf.sprintf "health: %s" (string_of_health health)));
  add (Printf.sprintf "runtime: %s" (runtime_string task));
  if not compact then add (Printf.sprintf "repo: %s" task.repo_path);
  if not compact then add (Printf.sprintf "branch: %s" branch);
  if not compact then add (Printf.sprintf "created_at: %s" task.created_at);
  (if not compact then
     match task.started_at with
     | Some value -> add (Printf.sprintf "started_at: %s" value)
     | None -> ());
  (if not compact then
     match task.finished_at with
     | Some value -> add (Printf.sprintf "finished_at: %s" value)
     | None -> ());
  (if not compact then
     match task.worktree_path with
     | Some value -> add (Printf.sprintf "worktree: %s" value)
     | None -> ());
  (match task.log_path with
  | Some value -> add (Printf.sprintf "log: %s" value)
  | None -> ());
  let plimit = if compact then compact_preview_limit else preview_limit in
  (match task.result_preview with
  | Some text when String.trim text <> "" ->
      add (Printf.sprintf "result: %s" (preview_text_n plimit text))
  | _ -> ());
  (match task.notification_status with
  | Some status ->
      let line = Printf.sprintf "notification: %s" status in
      let line =
        match task.notification_error with
        | Some err -> line ^ " (" ^ err ^ ")"
        | None -> line
      in
      add line
  | None -> ());
  add
    (Printf.sprintf "prompt: %s"
       (if full then task.prompt else preview_text_n plimit task.prompt));
  String.concat "\n" (List.rev !lines)

let max_inactive_shown = 3

let rec format_task_list tasks = format_task_list_with_hidden tasks 0

and task_list_table_data tasks =
  let columns =
    Table_format.
      [
        { header = "ID"; align = Right; min_width = 2; flex = false };
        { header = "RUNNER"; align = Left; min_width = 6; flex = false };
        { header = "STATUS"; align = Left; min_width = 6; flex = false };
        { header = "HEALTH"; align = Left; min_width = 6; flex = false };
        { header = "AGE"; align = Left; min_width = 3; flex = false };
        { header = "RUNTIME"; align = Left; min_width = 7; flex = false };
        { header = "MERGE"; align = Left; min_width = 3; flex = false };
        { header = "BRANCH"; align = Left; min_width = 6; flex = false };
        { header = "REPO"; align = Left; min_width = 4; flex = true };
      ]
  in
  let rows =
    List.map
      (fun (task : task) ->
        let branch = if task.branch = "" then "-" else task.branch in
        let runtime = runtime_string task in
        let health = diagnose_health task in
        let age = age_string task in
        let merge =
          if not task.use_worktree then "-"
          else if task.automerge then "auto"
          else "manual"
        in
        [
          string_of_int task.id;
          string_of_runner task.runner;
          string_of_status task.status;
          string_of_health health;
          age;
          runtime;
          merge;
          branch;
          task.repo_path;
        ])
      tasks
  in
  (columns, rows)

and format_task_list_with_hidden tasks hidden_count =
  if tasks = [] && hidden_count = 0 then "No background tasks."
  else
    let columns, rows = task_list_table_data tasks in
    let footer =
      if hidden_count > 0 then
        Printf.sprintf
          "\n\
          \  (%d older task%s hidden. Use `clawq background show <id>` to \
           view.)"
          hidden_count
          (if hidden_count = 1 then "" else "s")
      else ""
    in
    "Background tasks:\n" ^ Table_format.render columns rows ^ footer

let format_injected_message idx message =
  Printf.sprintf
    "A new message arrived while you were working. Treat it as a new user chat \
     message injected into the current background task conversation. \
     Incorporate it and continue the task unless it explicitly changes or \
     stops the task.\n\n\
     Injected message %d:\n\
     %s"
    idx message

let elapsed_string (task : task) =
  let now = Unix.gettimeofday () in
  let ref_time =
    match task.status with
    | Running -> (
        match task.started_at with
        | Some s -> parse_sqlite_datetime s
        | None -> parse_sqlite_datetime task.created_at)
    | _ -> parse_sqlite_datetime task.created_at
  in
  if ref_time <= 0.0 then "<1m" else format_elapsed_seconds (now -. ref_time)

let task_label (task : task) =
  let repo = Filename.basename task.repo_path in
  let branch = if task.branch = "" then "(auto)" else task.branch in
  Printf.sprintf "%s repo=%s branch=%s"
    (string_of_runner task.runner)
    repo branch

let terse_started_message (task : task) =
  Printf.sprintf "[bg #%d started: %s]" task.id (task_label task)

let merge_status_suffix (task : task) =
  match task.merge_status with
  | Some "merged" -> " (automerged)"
  | Some "conflict" ->
      Printf.sprintf " (rebase conflict — use `background finalize %d`)" task.id
  | Some s -> Printf.sprintf " (merge: %s)" s
  | None -> ""

let terse_finished_message (task : task) =
  let elapsed = elapsed_string task in
  let status_word =
    match task.status with
    | Succeeded -> "succeeded"
    | Failed -> "failed"
    | DirtyWorktree -> "dirty-worktree"
    | Cancelled -> "cancelled"
    | Queued -> "queued"
    | Running -> "running"
  in
  let base =
    Printf.sprintf "[bg #%d %s: %s (%s)" task.id status_word (task_label task)
      elapsed
  in
  let merge_suffix = merge_status_suffix task in
  match (task.status, task.result_preview) with
  | (Failed | DirtyWorktree), Some preview ->
      let short =
        if String.length preview > 80 then String.sub preview 0 80 ^ "..."
        else preview
      in
      base ^ merge_suffix ^ " -- " ^ short ^ "]"
  | Succeeded, Some preview when task.runner = Local ->
      let short =
        if String.length preview > 300 then String.sub preview 0 300 ^ "..."
        else preview
      in
      base ^ merge_suffix ^ " -- " ^ short ^ "]"
  | _ -> base ^ merge_suffix ^ "]"

let finalize_hint (task : task) =
  match (task.status, task.branch, task.worktree_path) with
  | DirtyWorktree, _, Some wt ->
      Some
        (Printf.sprintf
           "worktree %s has uncommitted changes; commit manually and run \
            `background finalize %d`"
           wt task.id)
  | Succeeded, branch, Some _ when String.trim branch <> "" ->
      Some
        (Printf.sprintf "next: rebase/review %s into %s when ready" branch
           task.repo_path)
  | _ -> None

let status_message (task : task) =
  let merge_suffix = merge_status_suffix task in
  let headline =
    Printf.sprintf "Background task %d finished: %s (%s)%s" task.id
      (status_summary task.status)
      (string_of_runner task.runner)
      merge_suffix
  in
  let details =
    [
      Some (Printf.sprintf "repo: %s" task.repo_path);
      Some
        (Printf.sprintf "branch: %s"
           (if task.branch = "" then "(auto)" else task.branch));
      Option.map
        (fun path -> Printf.sprintf "worktree: %s" path)
        task.worktree_path;
      Option.map (fun path -> Printf.sprintf "log: %s" path) task.log_path;
      Option.map
        (fun text -> Printf.sprintf "result: %s" (preview_text text))
        task.result_preview;
      finalize_hint task;
    ]
    |> List.filter_map (fun x -> x)
  in
  String.concat "\n" (headline :: details)

type git_status_info = { dirty_count : int; commit_count : int; rebased : bool }

let read_cmd_line cmd =
  try
    let ic = Unix.open_process_in cmd in
    let line = try String.trim (input_line ic) with End_of_file -> "" in
    ignore (Unix.close_process_in ic);
    line
  with _ -> ""

let gather_git_status (task : task) : git_status_info option =
  match task.worktree_path with
  | Some wt when Sys.file_exists wt && path_is_git_repo wt ->
      let q = Filename.quote wt in
      let dirty_count =
        let s =
          read_cmd_line
            (Printf.sprintf "git -C %s status --porcelain 2>/dev/null | wc -l" q)
        in
        try int_of_string s with _ -> 0
      in
      let commit_count =
        let s =
          read_cmd_line
            (Printf.sprintf
               "git -C %s rev-list --count HEAD --not --remotes 2>/dev/null" q)
        in
        try int_of_string s with _ -> 0
      in
      let rebased =
        Sys.command
          (Printf.sprintf
             "git -C %s merge-base --is-ancestor origin/main HEAD 2>/dev/null" q)
        = 0
        || Sys.command
             (Printf.sprintf
                "git -C %s merge-base --is-ancestor origin/master HEAD \
                 2>/dev/null"
                q)
           = 0
      in
      Some { dirty_count; commit_count; rebased }
  | _ -> None

let format_git_status (info : git_status_info) =
  let dirty =
    if info.dirty_count = 0 then "clean"
    else Printf.sprintf "dirty (%d files)" info.dirty_count
  in
  let commits =
    Printf.sprintf "%d commit%s" info.commit_count
      (if info.commit_count = 1 then "" else "s")
  in
  let rebase = if info.rebased then "rebased" else "not rebased" in
  Printf.sprintf "git: %s, %s, %s" dirty commits rebase

let channel_notification_message ?summary ?git_info (task : task) =
  let status_word =
    match task.status with
    | Succeeded -> "SUCCEEDED"
    | Failed -> "FAILED"
    | DirtyWorktree -> "DIRTY WORKTREE"
    | Cancelled -> "CANCELLED"
    | Queued -> "QUEUED"
    | Running -> "RUNNING"
  in
  let elapsed = elapsed_string task in
  let merge_suffix = merge_status_suffix task in
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add
    (Printf.sprintf "Background task #%d finished: %s%s" task.id status_word
       merge_suffix);
  add (Printf.sprintf "%s (%s)" (task_label task) elapsed);
  (match git_info with Some info -> add (format_git_status info) | None -> ());
  (match summary with
  | Some s -> add (Printf.sprintf "Summary: %s" s)
  | None -> (
      match task.result_preview with
      | Some text when String.trim text <> "" ->
          add (Printf.sprintf "Summary: %s" (preview_text_n 120 text))
      | _ -> ()));
  (match task.status with
  | Failed ->
      add
        (Printf.sprintf
           "Hint: `background retry %d` to retry, `background logs %d` for \
            details"
           task.id task.id)
  | DirtyWorktree ->
      add
        (Printf.sprintf "Hint: `background finalize %d` to commit and finalize"
           task.id)
  | _ -> ());
  String.concat "\n" (List.rev !lines)

let exit_code_of_status = function
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n -> 128 + n
  | Unix.WSTOPPED n -> 128 + n

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

let command_to_log_string = function
  | Process_group.Exec argv ->
      String.concat " " (Array.to_list (Array.map Filename.quote argv))
  | Process_group.Shell s -> s

let write_log_preamble ~log_path ~task_id ~command =
  try
    let oc =
      open_out_gen [ Open_wronly; Open_creat; Open_append ] 0o644 log_path
    in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () ->
        Printf.fprintf oc "[clawq] task %d starting: %s\n" task_id
          (command_to_log_string command))
  with _ -> ()

let append_log_error ~log_path msg =
  try
    let oc =
      open_out_gen [ Open_wronly; Open_creat; Open_append ] 0o644 log_path
    in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> Printf.fprintf oc "\n[clawq] ERROR: %s\n" msg)
  with _ -> ()

let append_messages_to_log ~log_path (msgs : (string * string) list) =
  try
    let oc =
      open_out_gen [ Open_wronly; Open_creat; Open_append ] 0o644 log_path
    in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () ->
        List.iter
          (fun (role, content) -> Printf.fprintf oc "\n[%s]\n%s\n" role content)
          msgs)
  with _ -> ()

let append_log_line ~log_path msg =
  try
    let oc =
      open_out_gen [ Open_wronly; Open_creat; Open_append ] 0o644 log_path
    in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> Printf.fprintf oc "\n%s\n" msg)
  with _ -> ()

let start_log_heartbeat ~log_path =
  let stop = ref false in
  Lwt.async (fun () ->
      let open Lwt.Syntax in
      let rec beat () =
        if !stop then Lwt.return_unit
        else
          let* () = Lwt_unix.sleep 30.0 in
          if !stop then Lwt.return_unit
          else begin
            (try
               let now = Unix.gettimeofday () in
               Unix.utimes log_path now now
             with _ -> ());
            beat ()
          end
      in
      beat ());
  stop

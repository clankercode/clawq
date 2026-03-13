type runner = Codex | Claude | Kimi | Gemini | Opencode | Cursor

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

let runner_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "codex" -> Some Codex
  | "claude" | "claude-code" | "claude_code" -> Some Claude
  | "kimi" -> Some Kimi
  | "gemini" -> Some Gemini
  | "opencode" -> Some Opencode
  | "cursor" | "cursor-cli" | "cursor_cli" | "cursor-agent" | "cursor_agent" ->
      Some Cursor
  | _ -> None

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
  | Not_applicable -> "-"

let runner_binary = function
  | Codex -> "codex"
  | Claude -> "claude"
  | Kimi -> "kimi"
  | Gemini -> "gemini"
  | Opencode -> "opencode"
  | Cursor -> "cursor-agent"

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

let runner_available runner = command_exists (runner_binary runner)

let resolve_runner ?(check_available = true) ?preferred () =
  let available runner = (not check_available) || runner_available runner in
  match preferred with
  | Some runner when available runner -> Ok (runner, None)
  | Some runner ->
      Error
        (Printf.sprintf "Runner '%s' is not available in PATH"
           (string_of_runner runner))
  | None when available Kimi -> Ok (Kimi, None)
  | None when available Cursor -> Ok (Cursor, None)
  | None when available Opencode -> Ok (Opencode, Some "zai-coding-plan/glm-5")
  | None when available Claude -> Ok (Claude, None)
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

let stalled_threshold_seconds = 300.0
let log_stale_threshold_seconds = 120.0

let diagnose_health ?(now = Unix.gettimeofday ())
    ?(pid_alive = fun pid -> Process_group.group_alive pid) (task : task) =
  match task.status with
  | Queued | Succeeded | Failed | DirtyWorktree | Cancelled -> Not_applicable
  | Running ->
      let pid_alive =
        match task.pid with
        | Some pid when pid > 0 -> pid_alive pid
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
        if log_fresh then Active
        else if elapsed < log_stale_threshold_seconds then Active
        else if elapsed >= stalled_threshold_seconds then Stalled
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
  add
    (Printf.sprintf "prompt: %s"
       (if full then task.prompt else preview_text_n plimit task.prompt));
  String.concat "\n" (List.rev !lines)

let max_inactive_shown = 3

let rec format_task_list tasks = format_task_list_with_hidden tasks 0

and format_task_list_with_hidden tasks hidden_count =
  if tasks = [] && hidden_count = 0 then "No background tasks."
  else
    let columns =
      Table_format.
        [
          { header = "ID"; align = Right; min_width = 2; flex = false };
          { header = "RUNNER"; align = Left; min_width = 6; flex = false };
          { header = "STATUS"; align = Left; min_width = 6; flex = false };
          { header = "HEALTH"; align = Left; min_width = 6; flex = false };
          { header = "RUNTIME"; align = Left; min_width = 7; flex = false };
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
          [
            string_of_int task.id;
            string_of_runner task.runner;
            string_of_status task.status;
            string_of_health health;
            runtime;
            branch;
            task.repo_path;
          ])
        tasks
    in
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

let sql_text = function Sqlite3.Data.TEXT s -> Some s | _ -> None
let sql_int = function Sqlite3.Data.INT i -> Some (Int64.to_int i) | _ -> None

let sql_bool stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Int64.to_int n <> 0
  | _ -> false

let queued_message_of_stmt stmt =
  {
    id = Option.value (sql_int (Sqlite3.column stmt 0)) ~default:0;
    task_id = Option.value (sql_int (Sqlite3.column stmt 1)) ~default:0;
    message = Sqlite3.column stmt 2 |> sql_text |> Option.value ~default:"";
    created_at = Sqlite3.column stmt 3 |> sql_text |> Option.value ~default:"";
  }

let task_of_stmt stmt : task =
  {
    id = Option.value (sql_int (Sqlite3.column stmt 0)) ~default:0;
    runner =
      Sqlite3.column stmt 1 |> sql_text
      |> Option.value ~default:"codex"
      |> runner_of_string
      |> Option.value ~default:Codex;
    model = Sqlite3.column stmt 2 |> sql_text;
    repo_path = Sqlite3.column stmt 3 |> sql_text |> Option.value ~default:"";
    prompt = Sqlite3.column stmt 4 |> sql_text |> Option.value ~default:"";
    branch = Sqlite3.column stmt 5 |> sql_text |> Option.value ~default:"";
    worktree_path = Sqlite3.column stmt 6 |> sql_text;
    log_path = Sqlite3.column stmt 7 |> sql_text;
    status =
      Sqlite3.column stmt 8 |> sql_text
      |> Option.value ~default:"failed"
      |> status_of_string;
    session_key = Sqlite3.column stmt 9 |> sql_text;
    channel = Sqlite3.column stmt 10 |> sql_text;
    channel_id = Sqlite3.column stmt 11 |> sql_text;
    pid = Sqlite3.column stmt 12 |> sql_int;
    result_preview = Sqlite3.column stmt 13 |> sql_text;
    created_at = Sqlite3.column stmt 14 |> sql_text |> Option.value ~default:"";
    started_at = Sqlite3.column stmt 15 |> sql_text;
    finished_at = Sqlite3.column stmt 16 |> sql_text;
    automerge = sql_bool stmt 17;
    use_worktree = sql_bool stmt 18;
    merge_status = Sqlite3.column stmt 19 |> sql_text;
    retry_count =
      (match Sqlite3.column stmt 20 with
      | Sqlite3.Data.INT i -> Int64.to_int i
      | _ -> 0);
    parent_task_id = Sqlite3.column stmt 21 |> sql_int;
    replaced_by = Sqlite3.column stmt 22 |> sql_int;
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
    "CREATE TABLE IF NOT EXISTS background_tasks (\n\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \  runner TEXT NOT NULL,\n\
    \  model TEXT,\n\
    \  repo_path TEXT NOT NULL,\n\
    \  prompt TEXT NOT NULL,\n\
    \  branch TEXT,\n\
    \  worktree_path TEXT,\n\
    \  log_path TEXT,\n\
    \  status TEXT NOT NULL DEFAULT 'queued',\n\
    \  session_key TEXT,\n\
    \  channel TEXT,\n\
    \  channel_id TEXT,\n\
    \  pid INTEGER,\n\
    \  result_preview TEXT,\n\
    \  created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \  started_at TEXT,\n\
    \  finished_at TEXT\n\
     )";
  exec
    "CREATE INDEX IF NOT EXISTS idx_background_tasks_status ON \
     background_tasks (status)";
  exec
    "CREATE TABLE IF NOT EXISTS background_task_inbound_queue (\n\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \  task_id INTEGER NOT NULL,\n\
    \  message TEXT NOT NULL,\n\
    \  created_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
     )";
  exec
    "CREATE INDEX IF NOT EXISTS idx_background_task_inbound_queue_task_id ON \
     background_task_inbound_queue (task_id, id)";
  let try_alter sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK | Sqlite3.Rc.ERROR -> ()
    | rc ->
        failwith
          (Printf.sprintf "SQLite error: %s (sql: %s)" (Sqlite3.Rc.to_string rc)
             sql)
  in
  try_alter "ALTER TABLE background_tasks ADD COLUMN model TEXT";
  try_alter
    "ALTER TABLE background_tasks ADD COLUMN automerge INTEGER NOT NULL \
     DEFAULT 0";
  try_alter
    "ALTER TABLE background_tasks ADD COLUMN use_worktree INTEGER NOT NULL \
     DEFAULT 1";
  try_alter "ALTER TABLE background_tasks ADD COLUMN merge_status TEXT";
  try_alter
    "ALTER TABLE background_tasks ADD COLUMN retry_count INTEGER NOT NULL \
     DEFAULT 0";
  try_alter "ALTER TABLE background_tasks ADD COLUMN parent_task_id INTEGER";
  try_alter "ALTER TABLE background_tasks ADD COLUMN replaced_by INTEGER"

let list_queued_messages ~db ~task_id =
  let sql =
    "SELECT id, task_id, message, created_at FROM \
     background_task_inbound_queue WHERE task_id = ? ORDER BY id ASC"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int task_id)));
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        rows := queued_message_of_stmt stmt :: !rows
      done;
      List.rev !rows)

let queue_message ~db ~task_id ~message =
  let sql =
    "INSERT INTO background_task_inbound_queue (task_id, message) VALUES (?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int task_id)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT message));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok (Int64.to_int (Sqlite3.last_insert_rowid db))
      | rc ->
          Error
            (Printf.sprintf "Failed to queue task message: %s"
               (Sqlite3.Rc.to_string rc)))

let delete_queued_message ~db ~queue_id =
  let sql = "DELETE FROM background_task_inbound_queue WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int queue_id)));
      ignore (Sqlite3.step stmt))

let take_queued_messages ~db ~task_id =
  let rows = list_queued_messages ~db ~task_id in
  List.iter (fun msg -> delete_queued_message ~db ~queue_id:msg.id) rows;
  rows

let queued_message_count ~db ~task_id =
  let sql =
    "SELECT COUNT(*) FROM background_task_inbound_queue WHERE task_id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int task_id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT i -> Int64.to_int i
          | _ -> 0)
      | _ -> 0)

let resume_supported (task : task) =
  task.use_worktree && task.branch <> ""
  &&
  match task.worktree_path with
  | Some worktree_path -> path_is_git_worktree worktree_path
  | None -> false

let format_injected_message idx message =
  Printf.sprintf
    "A new message arrived while you were working. Treat it as a new user chat \
     message injected into the current background task conversation. \
     Incorporate it and continue the task unless it explicitly changes or \
     stops the task.\n\n\
     Injected message %d:\n\
     %s"
    idx message

let resume_prompt_of_messages messages =
  match messages with
  | [] ->
      "Resume the current background task from where you left off. Continue \
       the existing work, make concrete progress, and finish cleanly if the \
       task is already near completion."
  | _ ->
      messages
      |> List.mapi (fun idx message ->
          format_injected_message (idx + 1) message)
      |> String.concat "\n\n"

type invocation = Fresh | Resume of string

let enqueue ~db ~runner ?model ?(require_git = true) ?(automerge = false)
    ?(use_worktree = true) ~repo_path ~prompt ?branch ?session_key ?channel
    ?channel_id ?parent_task_id () =
  match validate_repo_path ~require_git repo_path with
  | Error _ as err -> err
  | Ok () ->
      let sql =
        "INSERT INTO background_tasks (runner, model, repo_path, prompt, \
         branch, session_key, channel, channel_id, automerge, use_worktree, \
         parent_task_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      in
      let stmt = Sqlite3.prepare db sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore
            (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT (string_of_runner runner)));
          let bind_opt index = function
            | Some value when String.trim value <> "" ->
                ignore (Sqlite3.bind stmt index (Sqlite3.Data.TEXT value))
            | _ -> ignore (Sqlite3.bind stmt index Sqlite3.Data.NULL)
          in
          bind_opt 2 model;
          ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT repo_path));
          ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT prompt));
          bind_opt 5 branch;
          bind_opt 6 session_key;
          bind_opt 7 channel;
          bind_opt 8 channel_id;
          ignore
            (Sqlite3.bind stmt 9
               (Sqlite3.Data.INT (if automerge then 1L else 0L)));
          ignore
            (Sqlite3.bind stmt 10
               (Sqlite3.Data.INT (if use_worktree then 1L else 0L)));
          (match parent_task_id with
          | Some pid ->
              ignore
                (Sqlite3.bind stmt 11 (Sqlite3.Data.INT (Int64.of_int pid)))
          | None -> ignore (Sqlite3.bind stmt 11 Sqlite3.Data.NULL));
          match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE -> Ok (Int64.to_int (Sqlite3.last_insert_rowid db))
          | rc ->
              Error
                (Printf.sprintf "Failed to enqueue background task: %s"
                   (Sqlite3.Rc.to_string rc)))

let select_columns =
  "id, runner, model, repo_path, prompt, COALESCE(branch, ''), worktree_path, \
   log_path, status, session_key, channel, channel_id, pid, result_preview, \
   created_at, started_at, finished_at, COALESCE(automerge, 0), \
   COALESCE(use_worktree, 1), merge_status, COALESCE(retry_count, 0), \
   parent_task_id, replaced_by"

let list_tasks ~db : task list =
  let sql =
    Printf.sprintf "SELECT %s FROM background_tasks ORDER BY id DESC"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        rows := task_of_stmt stmt :: !rows
      done;
      List.rev !rows)

let list_tasks_for_display ~db : task list * int =
  let all = list_tasks ~db in
  let active, inactive =
    List.partition (fun t -> not (is_terminal_status t.status)) all
  in
  let recent_inactive =
    (* all is ordered by id DESC then reversed, so inactive preserves that;
       sort desc and take the most recent N *)
    let sorted =
      List.sort (fun (a : task) (b : task) -> compare b.id a.id) inactive
    in
    let rec take n = function
      | [] -> []
      | _ when n <= 0 -> []
      | x :: xs -> x :: take (n - 1) xs
    in
    take max_inactive_shown sorted
  in
  let hidden_count = List.length inactive - List.length recent_inactive in
  let visible =
    List.sort
      (fun (a : task) (b : task) -> compare b.id a.id)
      (active @ recent_inactive)
  in
  (visible, hidden_count)

let get_task ~db ~id : task option =
  let sql =
    Printf.sprintf "SELECT %s FROM background_tasks WHERE id = ?" select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (task_of_stmt stmt)
      | _ -> None)

type wait_result =
  | Finished of task
  | Timeout of task
  | Interrupted of task
  | Not_found

let max_wait_seconds = 110.0

let rec wait_until_terminal ?(timeout_seconds = 110.0) ?(poll_seconds = 1.0)
    ?interrupt_check ~db ~id () =
  let open Lwt.Syntax in
  match get_task ~db ~id with
  | None -> Lwt.return Not_found
  | Some task when is_terminal_status task.status -> Lwt.return (Finished task)
  | Some task when timeout_seconds <= 0.0 -> Lwt.return (Timeout task)
  | Some _ ->
      let sleep_for = Float.min poll_seconds timeout_seconds in
      let* () = Lwt_unix.sleep sleep_for in
      let interrupted =
        match interrupt_check with
        | Some check -> check () <> None
        | None -> false
      in
      if interrupted then
        match get_task ~db ~id with
        | None -> Lwt.return Not_found
        | Some task -> Lwt.return (Interrupted task)
      else
        wait_until_terminal
          ~timeout_seconds:(timeout_seconds -. sleep_for)
          ~poll_seconds ?interrupt_check ~db ~id ()

let read_last_lines path ~lines =
  if lines <= 0 then Ok []
  else
    try
      let ic = open_in path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let rec loop acc count =
            match input_line ic with
            | line ->
                let acc =
                  if count >= lines then List.tl acc @ [ line ]
                  else acc @ [ line ]
                in
                loop acc (min lines (count + 1))
            | exception End_of_file -> Ok acc
          in
          loop [] 0)
    with Sys_error msg -> Error msg

let permission_rejection_markers =
  [ "permission requested:"; "auto-rejecting"; "The user rejected permission" ]

let looks_like_permission_rejection output =
  List.exists
    (fun needle -> String_util.contains output needle)
    permission_rejection_markers

let classify_task_result ~exit_code ~output =
  if exit_code <> 0 then Failed
  else if looks_like_permission_rejection output then Failed
  else Succeeded

let result_preview_of_output ~exit_code ~output =
  if output = "" then Printf.sprintf "Process exited with code %d" exit_code
  else Printf.sprintf "exit %d: %s" exit_code output

let read_command_first_line command =
  try
    let ic = Unix.open_process_in command in
    let line = try Some (input_line ic) with End_of_file -> None in
    let exit_code =
      match Unix.close_process_in ic with
      | Unix.WEXITED code -> code
      | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> 128
    in
    (exit_code, line)
  with _ -> (128, None)

let worktree_harvest_issue (task : task) =
  match (task.use_worktree, task.worktree_path) with
  | false, _ | _, None -> None
  | true, Some worktree_path when not (path_is_git_repo worktree_path) ->
      Some
        "Task worktree is no longer a git repository; changes cannot be \
         harvested"
  | true, Some worktree_path -> (
      let command =
        Printf.sprintf
          "git -C %s status --porcelain --untracked-files=normal 2>&1"
          (Filename.quote worktree_path)
      in
      let exit_code, first_line = read_command_first_line command in
      if exit_code <> 0 then
        Some
          (Printf.sprintf "Unable to inspect task worktree for harvesting%s"
             (match first_line with
             | Some line when String.trim line <> "" -> ": " ^ String.trim line
             | _ -> ""))
      else
        match first_line with
        | Some line when String.trim line <> "" ->
            Some
              (Printf.sprintf
                 "Task left uncommitted worktree changes that cannot be \
                  harvested: %s"
                 (String.trim line))
        | _ -> None)

let completion_outcome ~db ~id ~exit_code ~output =
  let default_preview = result_preview_of_output ~exit_code ~output in
  match get_task ~db ~id with
  | Some { status = Cancelled; _ } -> (Cancelled, default_preview)
  | Some task -> (
      match classify_task_result ~exit_code ~output with
      | Failed -> (Failed, default_preview)
      | Succeeded -> (
          match worktree_harvest_issue task with
          | Some issue -> (DirtyWorktree, issue)
          | None -> (Succeeded, default_preview))
      | DirtyWorktree | Cancelled | Queued | Running -> (Failed, default_preview)
      )
  | None -> (classify_task_result ~exit_code ~output, default_preview)

let read_lines_window path ~offset ~limit =
  if limit <= 0 then Ok ([], 0)
  else
    try
      let ic = open_in path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let rec loop line_num acc collected =
            match input_line ic with
            | line ->
                if line_num >= offset && collected < limit then
                  loop (line_num + 1) ((line_num, line) :: acc) (collected + 1)
                else if collected >= limit then
                  let rec count n =
                    match input_line ic with
                    | _ -> count (n + 1)
                    | exception End_of_file -> n
                  in
                  let total = count line_num in
                  Ok (List.rev acc, total)
                else loop (line_num + 1) acc collected
            | exception End_of_file -> Ok (List.rev acc, line_num - 1)
          in
          loop 1 [] 0)
    with Sys_error msg -> Error msg

let count_lines path =
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let count = ref 0 in
        (try
           while true do
             ignore (input_line ic);
             incr count
           done
         with End_of_file -> ());
        !count)
  with Sys_error _ -> 0

let background_task_logs_max_chars = 3000
let background_task_logs_max_line_chars = 1200
let background_task_logs_max_lines = 200

let truncate_background_task_log_line line =
  if String.length line <= background_task_logs_max_line_chars then (line, false)
  else
    ( String.sub line 0 background_task_logs_max_line_chars
      ^ Printf.sprintf " ...(truncated %d chars)"
          (String.length line - background_task_logs_max_line_chars),
      true )

let trim_rendered_lines ~max_chars lines =
  let budget = max 0 max_chars in
  let rec take acc used remaining =
    match remaining with
    | [] -> (List.rev acc, false)
    | line :: rest ->
        let line_len = String.length line in
        let sep_len = if acc = [] then 0 else 1 in
        if used + sep_len + line_len <= budget then
          take (line :: acc) (used + sep_len + line_len) rest
        else (List.rev acc, true)
  in
  take [] 0 lines

let render_background_task_log_lines indexed_lines =
  let truncated_any_line = ref false in
  let numbered_lines =
    indexed_lines
    |> List.map (fun (n, line) ->
        let line, truncated = truncate_background_task_log_line line in
        if truncated then truncated_any_line := true;
        Printf.sprintf "%d: %s" n line)
  in
  let rendered_lines, truncated_by_budget =
    trim_rendered_lines ~max_chars:background_task_logs_max_chars numbered_lines
  in
  (rendered_lines, truncated_by_budget, !truncated_any_line)

let log_excerpt ?(offset = 0) ?(lines = 20) task =
  match task.log_path with
  | None -> Error (Printf.sprintf "Task %d has no log file yet" task.id)
  | Some path when not (Sys.file_exists path) ->
      Error (Printf.sprintf "Log file does not exist yet: %s" path)
  | Some path ->
      if offset > 0 then
        read_lines_window path ~offset ~limit:lines
        |> Result.map (fun (indexed_lines, total) ->
            let header =
              Printf.sprintf "Log excerpt for task %d (%s)\npath: %s" task.id
                (string_of_status task.status)
                path
            in
            if indexed_lines = [] then
              header
              ^ Printf.sprintf
                  "\n\n(No lines in requested range. Log has %d lines.)" total
            else
              let rendered_lines, truncated, truncated_any_line =
                render_background_task_log_lines indexed_lines
              in
              let rendered = String.concat "\n" rendered_lines in
              let last_line = fst (List.hd (List.rev indexed_lines)) in
              let suffix =
                if truncated then
                  let next_offset = offset + List.length rendered_lines in
                  Printf.sprintf
                    "\n\n\
                     (Output truncated by size budget. Showing lines %d-%d of \
                     %d. Use offset=%d to continue.)"
                    offset
                    (offset + List.length rendered_lines - 1)
                    total next_offset
                else if last_line < total then
                  Printf.sprintf
                    "\n\n\
                     (Showing lines %d-%d of %d. Use offset=%d to continue.)"
                    offset last_line total (last_line + 1)
                else Printf.sprintf "\n\n(End of log - total %d lines)" total
              in
              let trunc_suffix =
                if truncated_any_line then
                  Printf.sprintf
                    "\n\n(Note: long log lines are truncated to %d chars.)"
                    background_task_logs_max_line_chars
                else ""
              in
              header ^ "\n\n" ^ rendered ^ suffix ^ trunc_suffix)
      else
        read_last_lines path ~lines
        |> Result.map (fun chunks ->
            let header =
              Printf.sprintf "Log excerpt for task %d (%s)\npath: %s" task.id
                (string_of_status task.status)
                path
            in
            if chunks = [] then header ^ "\n\n(log file is empty)"
            else
              let total = count_lines path in
              let n_returned = List.length chunks in
              let start_num = max 1 (total - n_returned + 1) in
              let indexed_lines =
                List.mapi (fun i line -> (start_num + i, line)) chunks
              in
              let rendered_lines, truncated, truncated_any_line =
                render_background_task_log_lines indexed_lines
              in
              let rendered = String.concat "\n" rendered_lines in
              let shown = List.length rendered_lines in
              let shown_start = start_num in
              let shown_end = start_num + shown - 1 in
              let footer =
                if truncated then
                  Printf.sprintf
                    "\n\n\
                     (Output truncated by size budget. Showing lines %d-%d of \
                     %d. Use offset=%d to continue.)"
                    shown_start shown_end total (shown_end + 1)
                else
                  Printf.sprintf
                    "\n\n(Showing last %d lines, lines %d-%d of %d.)" shown
                    shown_start shown_end total
              in
              let trunc_suffix =
                if truncated_any_line then
                  Printf.sprintf
                    "\n\n(Note: long log lines are truncated to %d chars.)"
                    background_task_logs_max_line_chars
                else ""
              in
              header ^ "\n\n" ^ rendered ^ footer ^ trunc_suffix)

let read_lines_range path ~offset ~lines =
  if lines <= 0 then Ok ([], 0)
  else
    try
      let ic = open_in path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let line_num = ref 0 in
          (* Skip to offset *)
          (try
             while !line_num < offset do
               ignore (input_line ic);
               incr line_num
             done
           with End_of_file -> ());
          (* Read up to lines *)
          let acc = ref [] in
          let count = ref 0 in
          (try
             while !count < lines do
               let line = input_line ic in
               acc := line :: !acc;
               incr count;
               incr line_num
             done
           with End_of_file -> ());
          Ok (List.rev !acc, !line_num))
    with Sys_error msg -> Error msg

let log_range ~offset ~lines task =
  match task.log_path with
  | None -> Error (Printf.sprintf "Task %d has no log file yet" task.id)
  | Some path when not (Sys.file_exists path) ->
      Error (Printf.sprintf "Log file does not exist yet: %s" path)
  | Some path ->
      let total_lines = count_lines path in
      read_lines_range path ~offset ~lines
      |> Result.map (fun (chunks, next_line) ->
          let has_more = next_line < total_lines in
          let header =
            Printf.sprintf
              "Log for task %d (%s)\n\
               path: %s\n\
               total_lines: %d\n\
               offset: %d\n\
               showing: %d"
              task.id
              (string_of_status task.status)
              path total_lines offset (List.length chunks)
          in
          let continuation =
            if has_more then
              Printf.sprintf "\nhas_more: true\nnext_offset: %d" next_line
            else "\nhas_more: false"
          in
          if chunks = [] then
            header ^ continuation ^ "\n\n(no lines in requested range)"
          else header ^ continuation ^ "\n\n" ^ String.concat "\n" chunks)

let file_size path =
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> in_channel_length ic)
  with Sys_error _ -> 0

let read_from_offset path ~offset =
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let file_len = in_channel_length ic in
        if file_len > offset then begin
          seek_in ic offset;
          let len = file_len - offset in
          let buf = Bytes.create len in
          let actually_read = input ic buf 0 len in
          Some (Bytes.sub_string buf 0 actually_read, offset + actually_read)
        end
        else None)
  with Sys_error _ -> None

let log_follow ?(poll_seconds = 0.5) ~db ~id ~initial_lines
    ?(emit =
      fun s ->
        print_string s;
        flush stdout) () =
  let open Lwt.Syntax in
  let rec wait_for_task () =
    match get_task ~db ~id with
    | None ->
        Lwt.return_error
          (Printf.sprintf "No background task found with id %d" id)
    | Some task
      when task.log_path = None && not (is_terminal_status task.status) ->
        let* () = Lwt_unix.sleep poll_seconds in
        wait_for_task ()
    | Some task -> Lwt.return_ok task
  in
  let* task_result = wait_for_task () in
  match task_result with
  | Error msg -> Lwt.return_error msg
  | Ok task -> (
      match task.log_path with
      | None ->
          Lwt.return_error
            (Printf.sprintf "Task %d finished with no log file" task.id)
      | Some path ->
          let header =
            Printf.sprintf "Following log for task %d (%s)\npath: %s\n\n"
              task.id
              (string_of_status task.status)
              path
          in
          emit header;
          (* Print initial tail lines, then track offset from end of file *)
          let offset = ref 0 in
          if Sys.file_exists path then begin
            (match read_last_lines path ~lines:initial_lines with
            | Ok chunks when chunks <> [] ->
                emit (String.concat "\n" chunks ^ "\n")
            | _ -> ());
            offset := file_size path
          end;
          let rec follow () =
            (* Read any new content *)
            (match read_from_offset path ~offset:!offset with
            | Some (s, new_offset) when s <> "" ->
                emit s;
                offset := new_offset
            | _ -> ());
            (* Check task status *)
            match get_task ~db ~id with
            | None -> Lwt.return_ok ()
            | Some task when is_terminal_status task.status ->
                (* One final read *)
                (match read_from_offset path ~offset:!offset with
                | Some (s, _) when s <> "" -> emit s
                | _ -> ());
                emit
                  (Printf.sprintf "\n--- Task %d %s ---\n" task.id
                     (string_of_status task.status));
                Lwt.return_ok ()
            | Some _ ->
                let* () = Lwt_unix.sleep poll_seconds in
                follow ()
          in
          follow ())

let set_running ~db ~id ~branch ~worktree_path ~log_path ~pid =
  let sql =
    "UPDATE background_tasks SET status = 'running', branch = ?, worktree_path \
     = ?, log_path = ?, pid = ?, started_at = datetime('now'), finished_at = \
     NULL WHERE id = ? AND status = 'queued'"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT branch));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT worktree_path));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT log_path));
      ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.INT (Int64.of_int pid)));
      ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.INT (Int64.of_int id)));
      ignore (Sqlite3.step stmt);
      Sqlite3.changes db > 0)

let finish ~db ~id ~status ~result_preview =
  let sql =
    "UPDATE background_tasks SET status = ?, result_preview = ?, pid = NULL, \
     finished_at = datetime('now') WHERE id = ? AND status <> 'cancelled'"
  in
  let stmt = Sqlite3.prepare db sql in
  let preview = preview_text result_preview in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT (string_of_status status)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT preview));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int id)));
      ignore (Sqlite3.step stmt))

let finalize_completed_task ~db ~id ~exit_code ~output =
  match get_task ~db ~id with
  | Some { status = Queued; _ } -> Queued
  | _ ->
      let final_status, result_preview =
        completion_outcome ~db ~id ~exit_code ~output
      in
      finish ~db ~id ~status:final_status ~result_preview;
      final_status

let queued_resume_message_count ~db ~id = queued_message_count ~db ~task_id:id

let requeue_for_resume ~db ~id ~result_preview =
  let sql =
    "UPDATE background_tasks SET status = 'queued', pid = NULL, result_preview \
     = ?, started_at = NULL, finished_at = NULL WHERE id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  let preview = preview_text result_preview in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT preview));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int id)));
      ignore (Sqlite3.step stmt))

let request_resume ~message ~db ~id =
  match get_task ~db ~id with
  | None -> Error (Printf.sprintf "No background task found with id %d" id)
  | Some task when not (resume_supported task) ->
      Error
        (Printf.sprintf
           "Task %d cannot be resumed because it does not have an isolated \
            worktree-backed session. The original worktree may have been \
            removed or is no longer a git worktree. Re-run it as a normal \
            background worktree task first."
           id)
  | Some task -> (
      let normalized_message = Option.map String.trim message in
      (match task.status with
      | Running -> (
          match task.pid with
          | Some pid when pid > 0 -> Process_group.terminate_blocking pid
          | _ -> ())
      | _ -> ());
      match normalized_message with
      | Some "" -> Error "Message must not be empty"
      | Some text -> (
          match queue_message ~db ~task_id:id ~message:text with
          | Error msg -> Error msg
          | Ok _queue_id ->
              requeue_for_resume ~db ~id
                ~result_preview:"Queued new message for resumed background task";
              Ok
                (Printf.sprintf
                   "Queued message for background task %d. The task will \
                    resume and receive it as a chat message."
                   id))
      | None ->
          requeue_for_resume ~db ~id
            ~result_preview:"Queued background task for native runner resume";
          Ok (Printf.sprintf "Queued background task %d for resume" id))

let set_merge_status ~db ~id ~merge_status =
  let sql = "UPDATE background_tasks SET merge_status = ? WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT merge_status));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int id)));
      ignore (Sqlite3.step stmt))

let mark_cancelled ~db ~id ~result_preview =
  let sql =
    "UPDATE background_tasks SET status = 'cancelled', result_preview = ?, pid \
     = NULL, finished_at = datetime('now') WHERE id = ? AND status IN \
     ('queued', 'running')"
  in
  let stmt = Sqlite3.prepare db sql in
  let preview = preview_text result_preview in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT preview));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int id)));
      ignore (Sqlite3.step stmt);
      Sqlite3.changes db > 0)

let cancel_with_signal ~send_signal ~db ~id
    ?(terminate_group = Process_group.terminate_blocking) () =
  match get_task ~db ~id with
  | None -> Error (Printf.sprintf "No background task found with id %d" id)
  | Some task -> (
      match task.status with
      | Cancelled -> Error (Printf.sprintf "Task %d is already cancelled" id)
      | Succeeded | Failed | DirtyWorktree ->
          Error (Printf.sprintf "Task %d is already finished" id)
      | Queued ->
          ignore
            (mark_cancelled ~db ~id
               ~result_preview:"Cancelled before execution started");
          Ok "Cancelled queued task"
      | Running -> (
          match task.pid with
          | Some pid when pid > 0 ->
              (try send_signal (-pid) Sys.sigterm with _ -> ());
              terminate_group pid;
              ignore
                (mark_cancelled ~db ~id
                   ~result_preview:"Cancellation requested for running task");
              Ok (Printf.sprintf "Sent SIGTERM to task %d (pid %d)" id pid)
          | Some _ | None ->
              ignore
                (mark_cancelled ~db ~id
                   ~result_preview:
                     "Cancelled running task without tracked process id");
              Ok "Cancelled running task"))

let cancel ~db ~id = cancel_with_signal ~send_signal:Unix.kill ~db ~id ()

let retry ~db ~id =
  match get_task ~db ~id with
  | None -> Error (Printf.sprintf "No background task found with id %d" id)
  | Some task when not (task.status = Failed || task.status = DirtyWorktree) ->
      Error
        (Printf.sprintf
           "Task %d has status '%s' — only failed tasks can be retried" id
           (string_of_status task.status))
  | Some task when task.retry_count >= max_retry_count ->
      Error
        (Printf.sprintf
           "Task %d has already been retried %d/%d times — maximum retries \
            exceeded"
           id task.retry_count max_retry_count)
  | Some task ->
      let new_retry_count = task.retry_count + 1 in
      let sql =
        "UPDATE background_tasks SET status = 'queued', pid = NULL, \
         result_preview = NULL, started_at = NULL, finished_at = NULL, \
         worktree_path = NULL, log_path = NULL, branch = '', merge_status = \
         NULL, retry_count = ? WHERE id = ?"
      in
      let stmt = Sqlite3.prepare db sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore
            (Sqlite3.bind stmt 1
               (Sqlite3.Data.INT (Int64.of_int new_retry_count)));
          ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int id)));
          match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE ->
              Ok
                (Printf.sprintf "Re-queued task %d (retry %d/%d)" id
                   new_retry_count max_retry_count)
          | rc ->
              Error
                (Printf.sprintf "Failed to retry task %d: %s" id
                   (Sqlite3.Rc.to_string rc)))

type evidence = {
  original_prompt : string;
  log_tail : string option;
  worktree_diff_stat : string option;
  result_preview : string option;
  health : health;
  status : status;
}

let gather_evidence (task : task) =
  let log_tail =
    match log_excerpt ~lines:40 task with
    | Ok text -> Some text
    | Error _ -> None
  in
  let worktree_diff_stat =
    match task.worktree_path with
    | Some wt when Sys.file_exists wt && path_is_git_repo wt ->
        let cmd =
          Printf.sprintf "git -C %s diff HEAD --stat 2>&1" (Filename.quote wt)
        in
        let ic = Unix.open_process_in cmd in
        let buf = Buffer.create 256 in
        (try
           while Buffer.length buf < 3000 do
             Buffer.add_char buf (input_char ic)
           done
         with End_of_file -> ());
        ignore (Unix.close_process_in ic);
        let s = String.trim (Buffer.contents buf) in
        if s = "" then None else Some s
    | _ -> None
  in
  {
    original_prompt = task.prompt;
    log_tail;
    worktree_diff_stat;
    result_preview = task.result_preview;
    health = diagnose_health task;
    status = task.status;
  }

let build_recovery_prompt ~original_id evidence =
  let or_none = function None -> "none" | Some s -> s in
  let commit_line =
    "- CRITICAL: You MUST `git add` and `git commit` all changes before \
     reporting completion. Verify with `git status` that the worktree is \
     clean. Tasks with uncommitted changes are marked as dirty-worktree \
     failures regardless of exit code."
  in
  String.concat "\n"
    [
      Printf.sprintf
        "You are a replacement background coding agent. A previous task (id \
         %d) %s and you are continuing the work."
        original_id
        (string_of_status evidence.status);
      "";
      "Original Goal:";
      evidence.original_prompt;
      "";
      "Previous Result:";
      or_none evidence.result_preview;
      "";
      "Previous Worktree Changes (git diff --stat):";
      or_none evidence.worktree_diff_stat;
      "";
      "Previous Log Tail:";
      or_none evidence.log_tail;
      "";
      "Execution contract:";
      commit_line;
      "- Work only inside this directory/worktree.";
      "- Do not inspect or modify the original source repo path directly; use \
       only the files available in the current worktree.";
      "- Make the smallest focused change that completes the task well.";
      "- Run relevant verification when practical and mention what you ran.";
      "- Summarize the changes, results, and any follow-up concerns at the end.";
      "- Do not push or perform destructive git history edits.";
    ]

let set_replaced_by ~db ~id ~replaced_by_id =
  let sql = "UPDATE background_tasks SET replaced_by = ? WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int replaced_by_id)));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int id)));
      ignore (Sqlite3.step stmt))

let recover ~db ~id ?runner ?model () =
  match get_task ~db ~id with
  | None -> Error (Printf.sprintf "No background task found with id %d" id)
  | Some task ->
      let health = diagnose_health task in
      let recoverable =
        match task.status with
        | Failed | DirtyWorktree | Cancelled -> true
        | Running -> (
            match health with
            | Stalled | Zombie | Process_missing -> true
            | _ -> false)
        | Queued | Succeeded -> false
      in
      if not recoverable then
        let reason =
          match task.status with
          | Running ->
              Printf.sprintf
                "Task %d is still actively running — cancel it first or wait \
                 for it to finish"
                id
          | Succeeded ->
              Printf.sprintf "Task %d already succeeded — nothing to recover" id
          | Queued ->
              Printf.sprintf
                "Task %d is queued — cancel it first if you want to replace it"
                id
          | _ -> Printf.sprintf "Task %d cannot be recovered" id
        in
        Error reason
      else begin
        (* If running but stuck, cancel it first *)
        (match task.status with Running -> ignore (cancel ~db ~id) | _ -> ());
        let evidence = gather_evidence task in
        let prompt = build_recovery_prompt ~original_id:id evidence in
        let effective_runner =
          match runner with Some r -> r | None -> task.runner
        in
        let effective_model =
          match model with Some _ -> model | None -> task.model
        in
        match
          enqueue ~db ~runner:effective_runner ?model:effective_model
            ~require_git:false ~automerge:task.automerge
            ~use_worktree:task.use_worktree ~repo_path:task.repo_path ~prompt
            ?session_key:task.session_key ?channel:task.channel
            ?channel_id:task.channel_id ~parent_task_id:id ()
        with
        | Ok new_id ->
            set_replaced_by ~db ~id ~replaced_by_id:new_id;
            Ok (new_id, effective_runner)
        | Error msg -> Error msg
      end

let count_active ~db =
  let sql =
    "SELECT COUNT(*) FROM background_tasks WHERE status IN ('queued', \
     'running')"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT i -> Int64.to_int i
          | _ -> 0)
      | _ -> 0)

let count_active_for_session ~db ~session_key =
  let sql =
    "SELECT COUNT(*) FROM background_tasks WHERE status IN ('queued', \
     'running') AND session_key = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT i -> Int64.to_int i
          | _ -> 0)
      | _ -> 0)

let ensure_roots () =
  ensure_dir (clawq_dir ());
  ensure_dir (worktree_root ());
  ensure_dir (log_root ())

let routing_from_context ?context ?notify_cfg () =
  let session_key =
    match context with
    | Some c -> c.Tool.session_key
    | None ->
        let value =
          try Some (Sys.getenv "CLAWQ_SESSION_ID") with Not_found -> None
        in
        Option.bind value (fun raw ->
            let trimmed = String.trim raw in
            if trimmed = "" then None else Some trimmed)
  in
  match session_key with
  | Some key -> (
      match Restart_notify.parse_channel_from_key key with
      | Some (channel, channel_id) ->
          (session_key, Some channel, Some channel_id)
      | None -> (session_key, None, None))
  | None -> (
      match notify_cfg with
      | Some notify ->
          let notify : Runtime_config.notify_config = notify in
          (None, Some notify.notify_channel, Some notify.notify_target)
      | None -> (None, None, None))

let build_delegate_prompt ~automerge:_ ~goal =
  let commit_line =
    "- CRITICAL: You MUST `git add` and `git commit` all changes before \
     reporting completion. Verify with `git status` that the worktree is \
     clean. Tasks with uncommitted changes are marked as dirty-worktree \
     failures regardless of exit code."
  in
  String.concat "\n"
    [
      "You are a delegated background coding agent running in the target \
       directory.";
      "";
      "Goal:";
      goal;
      "";
      "Execution contract:";
      commit_line;
      "- Work only inside this directory/worktree.";
      "- Do not inspect or modify the original source repo path directly; use \
       only the files available in the current worktree.";
      "- Make the smallest focused change that completes the task well.";
      "- Run relevant verification when practical and mention what you ran.";
      "- Summarize the changes, results, and any follow-up concerns at the end.";
      "- Do not push or perform destructive git history edits.";
    ]

let delegate_enqueue ?context ?notify_cfg ?(check_available = true)
    ?(automerge = false) ?(use_worktree = true) ~db ?preferred_runner ?model
    ?repo_path ?branch ~default_repo_path ~goal () =
  let chosen_repo_path =
    match repo_path with
    | Some path when String.trim path <> "" -> path
    | _ -> default_repo_path
  in
  if String.trim chosen_repo_path = "" then
    Error "Could not determine a repository path for delegation"
  else
    match validate_workspace_path chosen_repo_path with
    | Error _ as err -> err
    | Ok () -> (
        match
          resolve_runner ~check_available ?preferred:preferred_runner ()
        with
        | Error _ as err -> err
        | Ok (runner, auto_model) -> (
            let effective_model =
              match model with Some _ -> model | None -> auto_model
            in
            let prompt = build_delegate_prompt ~automerge ~goal in
            let session_key, channel, channel_id =
              routing_from_context ?context ?notify_cfg ()
            in
            match
              enqueue ~db ~runner ?model:effective_model ~require_git:false
                ~automerge ~use_worktree ~repo_path:chosen_repo_path ~prompt
                ?branch ?session_key ?channel ?channel_id ()
            with
            | Ok id -> Ok (id, runner, chosen_repo_path)
            | Error _ as err -> err))

let command_of_task_with_invocation task invocation =
  let model_args flag =
    match task.model with
    | Some model when String.trim model <> "" -> [| flag; model |]
    | _ -> [||]
  in
  match (task.runner, invocation) with
  | Codex, Fresh ->
      Array.concat
        [
          [| "codex"; "exec" |];
          model_args "--model";
          [| "--dangerously-bypass-approvals-and-sandbox"; task.prompt |];
        ]
  | Codex, Resume prompt ->
      Array.concat
        [
          [| "codex"; "exec"; "resume"; "--last" |];
          model_args "--model";
          [| "--dangerously-bypass-approvals-and-sandbox"; prompt |];
        ]
  | Claude, Fresh ->
      Array.concat
        [
          [| "claude"; "-p" |];
          model_args "--model";
          [| "--dangerously-skip-permissions"; task.prompt |];
        ]
  | Claude, Resume prompt ->
      Array.concat
        [
          [| "claude"; "-c"; "-p" |];
          model_args "--model";
          [| "--dangerously-skip-permissions"; prompt |];
        ]
  | Kimi, Fresh ->
      Array.concat
        [
          [| "kimi"; "--print"; "--yolo" |];
          model_args "--model";
          [| "-p"; task.prompt |];
        ]
  | Kimi, Resume prompt ->
      Array.concat
        [
          [| "kimi"; "--continue"; "--print"; "--yolo" |];
          model_args "--model";
          [| "-p"; prompt |];
        ]
  | Gemini, Fresh ->
      Array.concat
        [
          [| "gemini"; "--yolo" |];
          model_args "--model";
          [| "--prompt"; task.prompt |];
        ]
  | Gemini, Resume prompt ->
      Array.concat
        [
          [| "gemini"; "--resume"; "latest"; "--yolo" |];
          model_args "--model";
          [| "--prompt"; prompt |];
        ]
  | Opencode, Fresh ->
      Array.concat
        [ [| "opencode"; "run" |]; model_args "--model"; [| task.prompt |] ]
  | Opencode, Resume prompt ->
      Array.concat
        [ [| "opencode"; "run"; "-c" |]; model_args "--model"; [| prompt |] ]
  | Cursor, Fresh ->
      Array.concat
        [
          [| "cursor-agent"; "--print"; "--yolo"; "--trust" |];
          model_args "--model";
          [| task.prompt |];
        ]
  | Cursor, Resume prompt ->
      Array.concat
        [
          [| "cursor-agent"; "--continue"; "--print"; "--yolo"; "--trust" |];
          model_args "--model";
          [| prompt |];
        ]

let command_of_task task = command_of_task_with_invocation task Fresh

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
      Printf.sprintf " (merge conflict — use `background finalize %d`)" task.id
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
        (Printf.sprintf "next: merge/review %s into %s when ready" branch
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

let read_into_buffer_and_log ic oc buf =
  let rec loop () =
    let open Lwt.Syntax in
    let* chunk = Lwt_io.read ~count:4096 ic in
    if chunk = "" then Lwt.return_unit
    else begin
      Buffer.add_string buf chunk;
      let* () = Lwt_io.write oc chunk in
      loop ()
    end
  in
  loop ()

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

let run_command_capture ~cwd ~argv ~log_path =
  let proc =
    Process_group.start ~cwd ~env:(Unix.environment ())
      (Process_group.Exec argv)
  in
  let stdout_buf = Buffer.create 1024 in
  let stderr_buf = Buffer.create 256 in
  let open Lwt.Syntax in
  let* result =
    Lwt.finalize
      (fun () ->
        Lwt_io.with_file ~mode:Lwt_io.Output log_path (fun log_oc ->
            let* () =
              Lwt.join
                [
                  read_into_buffer_and_log proc.Process_group.stdout log_oc
                    stdout_buf;
                  read_into_buffer_and_log proc.Process_group.stderr log_oc
                    stderr_buf;
                ]
            in
            let* status = Process_group.wait proc.pid in
            Lwt.return
              ( proc.pid,
                exit_code_of_status status,
                Buffer.contents stdout_buf,
                Buffer.contents stderr_buf )))
      (fun () -> Process_group.close proc)
  in
  Lwt.return result

let run_simple_command ~cwd argv =
  let proc =
    Process_group.start ~cwd ~env:(Unix.environment ())
      (Process_group.Exec argv)
  in
  let open Lwt.Syntax in
  Lwt.finalize
    (fun () ->
      let* stdout = Lwt_io.read proc.Process_group.stdout in
      let* stderr = Lwt_io.read proc.Process_group.stderr in
      let* status = Process_group.wait proc.pid in
      Lwt.return (exit_code_of_status status, stdout, stderr))
    (fun () -> Process_group.close proc)

let prepare_worktree ?(run_simple_command = run_simple_command) task =
  let log_path = Option.value task.log_path ~default:(task_log_path task.id) in
  ensure_roots ();
  ensure_parent_dir log_path;
  let open Lwt.Syntax in
  if (not task.use_worktree) || not (path_is_git_repo task.repo_path) then
    (* Non-git directory or use_worktree=false: run agent directly in the path
       without worktree isolation. *)
    Lwt.return (Ok ("", task.repo_path, log_path))
  else
    let branch =
      if task.branch <> "" then task.branch else default_branch_name task.id
    in
    let worktree_path =
      Option.value task.worktree_path ~default:(task_worktree_path task.id)
    in
    if Sys.file_exists worktree_path then
      Lwt.return (Ok (branch, worktree_path, log_path))
    else if Option.is_some task.worktree_path then
      let* exit_code, stdout, stderr =
        run_simple_command ~cwd:task.repo_path
          [|
            "git";
            "-C";
            task.repo_path;
            "worktree";
            "add";
            worktree_path;
            branch;
          |]
      in
      if exit_code = 0 then Lwt.return (Ok (branch, worktree_path, log_path))
      else
        Lwt.return
          (Error
             (Printf.sprintf "git worktree add failed (exit %d): %s%s" exit_code
                stdout stderr))
    else
      let* exit_code, stdout, stderr =
        run_simple_command ~cwd:task.repo_path
          [|
            "git";
            "-C";
            task.repo_path;
            "worktree";
            "add";
            "-b";
            branch;
            worktree_path;
          |]
      in
      if exit_code = 0 then Lwt.return (Ok (branch, worktree_path, log_path))
      else
        Lwt.return
          (Error
             (Printf.sprintf "git worktree add failed (exit %d): %s%s" exit_code
                stdout stderr))

let running : (int, unit) Hashtbl.t = Hashtbl.create 16

let spawn_task ?(on_task_started = fun _ -> Lwt.return_unit)
    ?(on_task_finished = fun _ -> Lwt.return_unit)
    ?(run_simple_command = run_simple_command) ?command_override ~db
    (task : task) =
  Hashtbl.replace running task.id ();
  Lwt.async (fun () ->
      let open Lwt.Syntax in
      let finalize () =
        Hashtbl.remove running task.id;
        Lwt.return_unit
      in
      Lwt.finalize
        (fun () ->
          let* prepared = prepare_worktree ~run_simple_command task in
          match prepared with
          | Error err ->
              finish ~db ~id:task.id ~status:Failed ~result_preview:err;
              Lwt.return_unit
          | Ok (branch, worktree_path, log_path) ->
              let task_for_command =
                {
                  task with
                  branch;
                  worktree_path = Some worktree_path;
                  log_path = Some log_path;
                }
              in
              let queued_messages = list_queued_messages ~db ~task_id:task.id in
              let invocation =
                if queued_messages <> [] || resume_supported task then
                  Resume
                    (resume_prompt_of_messages
                       (List.map
                          (fun (msg : queued_message) -> msg.message)
                          queued_messages))
                else Fresh
              in
              let command =
                match command_override with
                | Some cmd -> cmd
                | None ->
                    Process_group.Exec
                      (command_of_task_with_invocation task_for_command
                         invocation)
              in
              let proc =
                Process_group.start_to_file ~cwd:worktree_path
                  ~env:(Unix.environment ()) ~log_path command
              in
              let pid = proc.file_pid in
              if
                not
                  (set_running ~db ~id:task.id ~branch ~worktree_path ~log_path
                     ~pid)
              then
                let* () = Process_group.terminate_immediately pid in
                let* _ = Process_group.wait pid in
                Lwt.return_unit
              else
                let* () =
                  match get_task ~db ~id:task.id with
                  | Some t -> on_task_started t
                  | None -> Lwt.return_unit
                in
                let* status = Process_group.wait pid in
                let exit_code = exit_code_of_status status in
                let () =
                  if exit_code = 0 then
                    List.iter
                      (fun (msg : queued_message) ->
                        delete_queued_message ~db ~queue_id:msg.id)
                      queued_messages
                in
                (* B210 watchdog: after child exits, give process group
                   2s then kill remaining members (e.g. grandchildren) *)
                Lwt.async (fun () ->
                    let open Lwt.Syntax in
                    let* () = Lwt_unix.sleep 2.0 in
                    Logs.info (fun m ->
                        m
                          "Background task %d: child exited, killing remaining \
                           process group members"
                          task.id);
                    Process_group.signal_group pid Sys.sigkill;
                    Lwt.return_unit);
                let output = read_log_tail log_path preview_limit in
                let exit_code = exit_code_of_status status in
                ignore
                  (finalize_completed_task ~db ~id:task.id ~exit_code ~output);
                let* () =
                  match get_task ~db ~id:task.id with
                  | Some finished_task -> on_task_finished finished_task
                  | None -> Lwt.return_unit
                in
                Lwt.return_unit)
        finalize)

let default_spawn_task ~on_task_started ~on_task_finished ~db task =
  spawn_task ~on_task_started ~on_task_finished ~db task

let rec take n = function
  | [] -> []
  | _ when n <= 0 -> []
  | x :: xs -> x :: take (n - 1) xs

let available_worker_slots ?max_running_tasks (tasks : task list) =
  match max_running_tasks with
  | None -> None
  | Some max_running_tasks ->
      let running_count =
        List.fold_left
          (fun acc (task : task) ->
            if task.status = Running then acc + 1 else acc)
          0 tasks
      in
      Some (max 0 (max max_running_tasks 0 - running_count))

let queued_tasks_ready_to_start ?max_running_tasks (tasks : task list) :
    task list =
  let queued =
    List.filter
      (fun (task : task) ->
        task.status = Queued && not (Hashtbl.mem running task.id))
      tasks
  in
  match available_worker_slots ?max_running_tasks tasks with
  | None -> queued
  | Some slots -> take slots queued

let start_queued_with_callback_impl ?max_running_tasks ~spawn_task
    ~on_task_started ~on_task_finished ~db () =
  let queued =
    queued_tasks_ready_to_start ?max_running_tasks (list_tasks ~db)
  in
  List.iter (spawn_task ~on_task_started ~on_task_finished ~db) queued

let start_queued_with_callback ?max_running_tasks ~on_task_finished ~db
    ?(on_task_started = fun _ -> Lwt.return_unit) () =
  start_queued_with_callback_impl ?max_running_tasks
    ~spawn_task:default_spawn_task ~on_task_started ~on_task_finished ~db ()

let start_queued ?max_running_tasks ~db () =
  start_queued_with_callback ?max_running_tasks
    ~on_task_finished:(fun _ -> Lwt.return_unit)
    ~db ()

let is_tracked_locally id = Hashtbl.mem running id
let clear_all_tracked () = Hashtbl.clear running

let reap_dead_running_tasks ~db ~on_task_finished =
  let running_in_db =
    List.filter
      (fun (t : task) -> t.status = Running && not (Hashtbl.mem running t.id))
      (list_tasks ~db)
  in
  let count = ref 0 in
  List.iter
    (fun task ->
      let pid_alive =
        match task.pid with
        | Some pid when pid > 0 -> Process_group.group_alive pid
        | _ -> false
      in
      if not pid_alive then begin
        let reason =
          match task.pid with
          | Some pid ->
              Printf.sprintf
                "Process group %d no longer alive (orphaned/crashed) — use \
                 'background retry %d' to re-queue"
                pid task.id
          | None ->
              Printf.sprintf
                "No PID recorded for running task — use 'background retry %d' \
                 to re-queue"
                task.id
        in
        Logs.warn (fun m ->
            m "Reaping stale background task %d: %s" task.id reason);
        finish ~db ~id:task.id ~status:Failed ~result_preview:reason;
        incr count;
        Lwt.async (fun () ->
            match get_task ~db ~id:task.id with
            | Some t -> on_task_finished t
            | None -> Lwt.return_unit)
      end)
    running_in_db;
  !count

let readopt_running_tasks ~db ~on_task_finished =
  let pid_or_group_alive pid =
    Process_group.group_alive pid
    ||
      try
        Unix.kill pid 0;
        true
      with Unix.Unix_error _ -> false
  in
  let orphaned =
    List.filter
      (fun (t : task) ->
        t.status = Running
        && (not (Hashtbl.mem running t.id))
        &&
        match t.pid with
        | Some pid when pid > 0 -> pid_or_group_alive pid
        | _ -> false)
      (list_tasks ~db)
  in
  let count = ref 0 in
  List.iter
    (fun task ->
      match task.pid with
      | Some pid ->
          Hashtbl.replace running task.id ();
          incr count;
          Lwt.async (fun () ->
              Lwt.finalize
                (fun () ->
                  let open Lwt.Syntax in
                  (* The readopted process may not be our child (reparented to
                     init after daemon restart), so waitpid can fail with
                     ECHILD. Fall back to polling group_alive in that case. *)
                  let* exit_code =
                    Lwt.catch
                      (fun () ->
                        let* status = Lwt_unix.waitpid [] pid in
                        Lwt.return (exit_code_of_status (snd status)))
                      (function
                        | Unix.Unix_error (Unix.ECHILD, _, _) ->
                            let rec poll () =
                              let* () = Lwt_unix.sleep 5.0 in
                              if Process_group.group_alive pid then poll ()
                              else Lwt.return 1
                            in
                            poll ()
                        | exn -> Lwt.fail exn)
                  in
                  (* B210 watchdog: kill remaining process group members *)
                  Lwt.async (fun () ->
                      let open Lwt.Syntax in
                      let* () = Lwt_unix.sleep 2.0 in
                      Process_group.signal_group pid Sys.sigkill;
                      Lwt.return_unit);
                  let output =
                    match task.log_path with
                    | Some path -> read_log_tail path preview_limit
                    | None -> ""
                  in
                  ignore
                    (finalize_completed_task ~db ~id:task.id ~exit_code ~output);
                  match get_task ~db ~id:task.id with
                  | Some t -> on_task_finished t
                  | None -> Lwt.return_unit)
                (fun () ->
                  Hashtbl.remove running task.id;
                  Lwt.return_unit))
      | None -> ())
    orphaned;
  !count

let enqueue_tool_with_notify ~notify_cfg ~db =
  {
    Tool.name = "background_task_enqueue";
    description =
      "Queue a background coding task (Codex, Claude, Kimi, Gemini, Opencode, \
       or Cursor) in its own git worktree. Lower-level alternative to delegate \
       — use when you need explicit control over runner, repo, branch, or \
       model. Use delegate for simple 'spawn a subagent' requests.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "runner",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "enum",
                        `List
                          [
                            `String "codex";
                            `String "claude";
                            `String "kimi";
                            `String "gemini";
                            `String "opencode";
                            `String "cursor";
                          ] );
                      ( "description",
                        `String
                          "Which external coding CLI to run in the background \
                           worktree." );
                    ] );
                ( "repo_path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Absolute or relative path to the git repository to \
                           use as the worktree source." );
                    ] );
                ( "prompt",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Implementation prompt to hand to the coding agent."
                      );
                    ] );
                ( "branch",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional branch name for the new worktree. Defaults \
                           to clawq-bg-<task-id>." );
                    ] );
                ( "model",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional explicit model for the external runner, \
                           e.g. gpt-5.4 or claude-sonnet-4-6." );
                    ] );
                ( "automerge",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Auto-rebase and merge task branch on success. \
                           Default: false. Ignored if use_worktree is false." );
                    ] );
                ( "use_worktree",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Run in a git worktree (default: true). Set false to \
                           run directly in the repo directory." );
                    ] );
              ] );
          ( "required",
            `List [ `String "runner"; `String "repo_path"; `String "prompt" ] );
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let runner_s =
          try args |> member "runner" |> to_string with _ -> ""
        in
        let repo_path =
          try args |> member "repo_path" |> to_string with _ -> ""
        in
        let prompt = try args |> member "prompt" |> to_string with _ -> "" in
        let branch =
          try
            match args |> member "branch" with
            | `String s when String.trim s <> "" -> Some s
            | _ -> None
          with _ -> None
        in
        let model =
          try
            match args |> member "model" with
            | `String s when String.trim s <> "" -> Some (String.trim s)
            | _ -> None
          with _ -> None
        in
        let automerge =
          try
            args |> member "automerge" |> to_bool_option
            |> Option.value ~default:false
          with _ -> false
        in
        let use_worktree =
          try
            args |> member "use_worktree" |> to_bool_option
            |> Option.value ~default:true
          with _ -> true
        in
        match runner_of_string runner_s with
        | None ->
            Lwt.return
              "Error: runner must be 'codex', 'claude', 'kimi', 'gemini', \
               'opencode', or 'cursor'"
        | Some runner when String.trim repo_path = "" ->
            Lwt.return "Error: repo_path is required"
        | Some _ when String.trim prompt = "" ->
            Lwt.return "Error: prompt is required"
        | Some runner -> (
            let session_key, channel, channel_id =
              routing_from_context ?context ?notify_cfg ()
            in
            match
              enqueue ~db ~runner ?model ~automerge ~use_worktree ~repo_path
                ~prompt ?branch ?session_key ?channel ?channel_id ()
            with
            | Ok id ->
                Lwt.return
                  (Printf.sprintf
                     "Queued background task %d (%s). Use background_task_list \
                      or `clawq background show %d` to track it."
                     id (string_of_runner runner) id)
            | Error msg -> Lwt.return ("Error: " ^ msg)));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let enqueue_tool ~db = enqueue_tool_with_notify ~notify_cfg:None ~db

let list_tool ~db =
  {
    Tool.name = "background_task_list";
    description =
      "List background coding tasks or inspect one task by id, including \
       current status, repo, branch, log path, and result preview. The prompt \
       is truncated by default; pass full:true to include the complete \
       original prompt when needed.";
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
                      ( "description",
                        `String
                          "Optional task id to inspect. When omitted, returns \
                           the full task list." );
                    ] );
                ( "full",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "When true and an id is provided, include the full \
                           untruncated prompt. Defaults to false." );
                    ] );
              ] );
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let task_id =
          try Some (args |> member "id" |> to_int) with _ -> None
        in
        let full = try args |> member "full" |> to_bool with _ -> false in
        match task_id with
        | Some id -> (
            match get_task ~db ~id with
            | Some task -> Lwt.return (format_task_summary ~full task)
            | None ->
                Lwt.return
                  (Printf.sprintf "No background task found with id %d" id))
        | None ->
            let tasks, hidden = list_tasks_for_display ~db in
            Lwt.return (format_task_list_with_hidden tasks hidden));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let wait_tool ~db =
  {
    Tool.name = "background_task_wait";
    description =
      "Wait for a background coding task to finish (max 110 seconds). If the \
       task is still running when the timeout is reached, call this tool again \
       to continue waiting.";
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
                      ("description", `String "Task id to wait for");
                    ] );
                ( "timeout_seconds",
                  `Assoc
                    [
                      ("type", `String "number");
                      ( "description",
                        `String
                          (Printf.sprintf
                             "Seconds to wait (default and max: %.0f). Values \
                              above the max are clamped."
                             max_wait_seconds) );
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        let raw_timeout =
          try args |> member "timeout_seconds" |> to_number
          with _ -> max_wait_seconds
        in
        let timeout_seconds =
          Float.min (Float.max raw_timeout 0.0) max_wait_seconds
        in
        let was_clamped = raw_timeout > max_wait_seconds in
        let interrupt_check =
          Option.bind context (fun c -> c.Tool.interrupt_check)
        in
        if id < 0 then
          Lwt.return "Error: id is required and must be a non-negative integer."
        else
          let open Lwt.Syntax in
          let* result =
            wait_until_terminal ~timeout_seconds ?interrupt_check ~db ~id ()
          in
          match result with
          | Finished task -> Lwt.return (format_task_summary ~compact:true task)
          | Timeout task ->
              let clamp_note =
                if was_clamped then
                  Printf.sprintf " (requested %.0fs clamped to max %.0fs)"
                    raw_timeout max_wait_seconds
                else ""
              in
              Lwt.return
                (Printf.sprintf
                   "Task %d is still %s after waiting%s. To continue waiting, \
                    call background_task_wait again with {\"id\": %d}. You can \
                    also check progress with background_task_logs.\n\
                    runner: %s | runtime: %s | repo: %s"
                   id
                   (string_of_status task.status)
                   clamp_note id
                   (string_of_runner task.runner)
                   (runtime_string task) task.repo_path)
          | Interrupted task ->
              Lwt.return
                (Printf.sprintf
                   "Task %d is still %s. Waiting was interrupted to process a \
                    new incoming message. Call background_task_wait again with \
                    {\"id\": %d} to resume waiting. You can also check \
                    progress with background_task_logs.\n\
                    runner: %s | runtime: %s | repo: %s"
                   id
                   (string_of_status task.status)
                   id
                   (string_of_runner task.runner)
                   (runtime_string task) task.repo_path)
          | Not_found ->
              Lwt.return
                (Printf.sprintf "Error: No background task found with id %d" id));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let logs_tool ~db =
  {
    Tool.name = "background_task_logs";
    description =
      "Read lines from a background task log file. Supports offset-based \
       paging (like file_read) or tail-style retrieval.";
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
                      ("description", `String "Task id whose log should be read");
                    ] );
                ( "offset",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "1-indexed line number to start reading from. When \
                           set, returns lines starting at this position (paged \
                           mode). When omitted, returns trailing lines (tail \
                           mode)." );
                    ] );
                ( "limit",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "Max lines to return (default 20). In paged mode, \
                           controls window size. In tail mode, controls how \
                           many trailing lines." );
                    ] );
                ( "lines",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "Alias for limit (backward compatibility). If both \
                           limit and lines are set, limit takes precedence." );
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        let offset = try args |> member "offset" |> to_int with _ -> 0 in
        let limit_explicit =
          try Some (args |> member "limit" |> to_int) with _ -> None
        in
        let lines_explicit =
          try Some (args |> member "lines" |> to_int) with _ -> None
        in
        let lines =
          match (limit_explicit, lines_explicit) with
          | Some l, _ -> l
          | None, Some l -> l
          | None, None -> 20
        in
        let lines = min lines background_task_logs_max_lines in
        if id < 0 then Lwt.return "Error: id is required"
        else if offset < 0 then Lwt.return "Error: offset must be >= 1"
        else
          match get_task ~db ~id with
          | None ->
              Lwt.return
                (Printf.sprintf "Error: No background task found with id %d" id)
          | Some task -> (
              match log_excerpt ~offset ~lines task with
              | Ok text -> Lwt.return text
              | Error msg -> Lwt.return ("Error: " ^ msg)));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let delegate_tool_with_notify ?(check_available = true) ~db ~default_repo_path
    ~notify_cfg () =
  {
    Tool.name = "delegate";
    description =
      "Delegate a coding task to a background subagent (Codex, Claude, Kimi, \
       Gemini, Opencode, or Cursor) that runs in its own git worktree. Use \
       when asked to spawn subagents, use workers, or run tasks with a \
       specific model (e.g. 'use haiku to ...', 'delegate to sonnet'). \
       Auto-selects runner and repo by default.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "goal",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Implementation goal for the delegated coding task."
                      );
                    ] );
                ( "runner",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "enum",
                        `List
                          [
                            `String "auto";
                            `String "codex";
                            `String "claude";
                            `String "kimi";
                            `String "gemini";
                            `String "opencode";
                            `String "cursor";
                          ] );
                      ( "description",
                        `String
                          "Optional runner choice. 'auto' prefers Kimi, then \
                           Cursor, then Opencode (with zai-coding-plan/glm-5), \
                           then Claude, then Codex, then Gemini." );
                    ] );
                ( "repo_path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional repository path. Defaults to the runtime \
                           workspace." );
                    ] );
                ( "branch",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional branch name for the worktree. Defaults to \
                           clawq-bg-<task-id>." );
                    ] );
                ( "model",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional explicit model for the external runner, \
                           e.g. gpt-5.4 or claude-sonnet-4-6." );
                    ] );
                ( "automerge",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Auto-rebase and merge task branch on success. \
                           Default: false. Ignored if use_worktree is false." );
                    ] );
                ( "use_worktree",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Run in a git worktree (default: true). Set false to \
                           run directly in the repo directory." );
                    ] );
              ] );
          ("required", `List [ `String "goal" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let goal = try args |> member "goal" |> to_string with _ -> "" in
        let runner_pref, runner_error =
          try
            match args |> member "runner" |> to_string with
            | s when String.trim s = "" || String.lowercase_ascii s = "auto" ->
                (None, None)
            | s -> (
                match runner_of_string s with
                | Some runner -> (Some runner, None)
                | None ->
                    ( None,
                      Some
                        "runner must be 'auto', 'codex', 'claude', 'kimi', \
                         'gemini', 'opencode', or 'cursor'" ))
          with _ -> (None, None)
        in
        let repo_path =
          try
            match args |> member "repo_path" with
            | `String s when String.trim s <> "" -> Some s
            | _ -> None
          with _ -> None
        in
        let branch =
          try
            match args |> member "branch" with
            | `String s when String.trim s <> "" -> Some s
            | _ -> None
          with _ -> None
        in
        let model =
          try
            match args |> member "model" with
            | `String s when String.trim s <> "" -> Some (String.trim s)
            | _ -> None
          with _ -> None
        in
        let automerge =
          try
            args |> member "automerge" |> to_bool_option
            |> Option.value ~default:false
          with _ -> false
        in
        let use_worktree =
          try
            args |> member "use_worktree" |> to_bool_option
            |> Option.value ~default:true
          with _ -> true
        in
        if String.trim goal = "" then Lwt.return "Error: goal is required"
        else if runner_error <> None then
          Lwt.return ("Error: " ^ Option.get runner_error)
        else
          match
            delegate_enqueue ?context ?notify_cfg ~check_available ~db
              ~automerge ~use_worktree ?preferred_runner:runner_pref ?model
              ?repo_path ?branch ~default_repo_path ~goal ()
          with
          | Ok (id, runner, repo) ->
              Lwt.return
                (Printf.sprintf
                   "Delegated task %d (%s) for %s. Use background_task_wait or \
                    `clawq background show %d` to track it."
                   id (string_of_runner runner) repo id)
          | Error msg -> Lwt.return ("Error: " ^ msg));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let delegate_tool ?check_available ~db ~default_repo_path () =
  delegate_tool_with_notify ?check_available ~db ~default_repo_path
    ~notify_cfg:None ()

let resume_tool ~db =
  {
    Tool.name = "background_task_resume";
    description =
      "Resume a previously started background coding task using the runner's \
       built-in session resume support. Requires a worktree-backed task that \
       has already started at least once.";
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
                      ("description", `String "Task id to resume");
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
          Lwt.return "Error: id is required and must be a non-negative integer."
        else
          match request_resume ~db ~id ~message:None with
          | Ok msg -> Lwt.return msg
          | Error msg -> Lwt.return ("Error: " ^ msg));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let message_tool ~db =
  {
    Tool.name = "background_task_send_message";
    description =
      "Send a new chat message into a background coding task. The current run \
       is resumed with the runner's native continue/resume support and the \
       message is injected as a user chat message.";
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
                      ("description", `String "Task id to message");
                    ] );
                ( "message",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Message to inject into the task chat" );
                    ] );
              ] );
          ("required", `List [ `String "id"; `String "message" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        let message =
          try args |> member "message" |> to_string with _ -> ""
        in
        if id < 0 then
          Lwt.return "Error: id is required and must be a non-negative integer."
        else if String.trim message = "" then
          Lwt.return "Error: message is required and must not be empty."
        else
          match request_resume ~db ~id ~message:(Some message) with
          | Ok msg -> Lwt.return msg
          | Error msg -> Lwt.return ("Error: " ^ msg));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let cancel_tool ~db =
  {
    Tool.name = "background_task_cancel";
    description = "Cancel a queued or running background coding task by id.";
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
                      ("description", `String "Task id to cancel");
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        if id < 0 then Lwt.return "Error: id is required"
        else
          match cancel ~db ~id with
          | Ok msg -> Lwt.return msg
          | Error msg -> Lwt.return ("Error: " ^ msg));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let recover_tool ~db =
  {
    Tool.name = "background_task_recover";
    description =
      "Recover a failed or stuck background task by spawning a replacement \
       with full context from the original. Works on failed, dirty_worktree, \
       cancelled, or stuck (stalled/zombie/process-missing) tasks.";
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
                      ("description", `String "Task id to recover");
                    ] );
                ( "runner",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional runner override \
                           (codex|claude|kimi|gemini|opencode|cursor)" );
                    ] );
                ( "model",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Optional model override");
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
            "Error: id is required and must be a positive integer. Provide the \
             numeric task id of the task to recover."
        else
          let runner =
            try
              let s = args |> member "runner" |> to_string in
              if String.trim s = "" then None else runner_of_string s
            with _ -> None
          in
          let model =
            try
              let s = args |> member "model" |> to_string in
              if String.trim s = "" then None else Some s
            with _ -> None
          in
          match recover ~db ~id ?runner ?model () with
          | Ok (new_id, effective_runner) ->
              Lwt.return
                (Printf.sprintf
                   "Recovered task %d → new task %d (%s). Use `background show \
                    %d` to track it."
                   id new_id
                   (string_of_runner effective_runner)
                   new_id)
          | Error msg -> Lwt.return ("Error: " ^ msg));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

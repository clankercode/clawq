include Background_task_0_format

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
    runner_session_id = Sqlite3.column stmt 23 |> sql_text;
    acp = sql_bool stmt 24;
    agent_name = Sqlite3.column stmt 25 |> sql_text;
    notification_status = Sqlite3.column stmt 26 |> sql_text;
    notification_error = Sqlite3.column stmt 27 |> sql_text;
    notification_attempts =
      (match Sqlite3.column stmt 28 with
      | Sqlite3.Data.INT i -> Int64.to_int i
      | _ -> 0);
    follow_up_prompt = (try Sqlite3.column stmt 29 |> sql_text with _ -> None);
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
  try_alter "ALTER TABLE background_tasks ADD COLUMN replaced_by INTEGER";
  try_alter "ALTER TABLE background_tasks ADD COLUMN runner_session_id TEXT";
  try_alter
    "ALTER TABLE background_tasks ADD COLUMN acp INTEGER NOT NULL DEFAULT 0";
  try_alter "ALTER TABLE background_tasks ADD COLUMN agent_name TEXT";
  try_alter "ALTER TABLE background_tasks ADD COLUMN notification_status TEXT";
  try_alter "ALTER TABLE background_tasks ADD COLUMN notification_error TEXT";
  try_alter
    "ALTER TABLE background_tasks ADD COLUMN notification_attempts INTEGER NOT \
     NULL DEFAULT 0";
  try_alter "ALTER TABLE background_tasks ADD COLUMN follow_up_prompt TEXT";
  Acp_history.init_schema db

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

let record_notification_result ~db ~id ~status ?error () =
  let sql =
    "UPDATE background_tasks SET notification_status = ?, notification_error = \
     ?, notification_attempts = COALESCE(notification_attempts, 0) + 1 WHERE \
     id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT status));
      ignore
        (Sqlite3.bind stmt 2
           (match error with
           | Some e -> Sqlite3.Data.TEXT e
           | None -> Sqlite3.Data.NULL));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int id)));
      ignore (Sqlite3.step stmt))

let resume_supported (task : task) =
  task.use_worktree && task.branch <> ""
  &&
  match task.worktree_path with
  | Some worktree_path -> path_is_git_worktree worktree_path
  | None -> false

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

let enqueue ~db ~runner ?model ?(require_git = true) ?(automerge = true)
    ?(use_worktree = true) ?(acp = false) ~repo_path ~prompt ?branch
    ?session_key ?channel ?channel_id ?parent_task_id ?agent_name
    ?follow_up_prompt () =
  if acp && runner = Local then
    Error "ACP mode is not supported with the Local runner"
  else
    match validate_repo_path ~require_git repo_path with
    | Error _ as err -> err
    | Ok () ->
        let sql =
          "INSERT INTO background_tasks (runner, model, repo_path, prompt, \
           branch, session_key, channel, channel_id, automerge, use_worktree, \
           parent_task_id, acp, agent_name, follow_up_prompt) VALUES (?, ?, ?, \
           ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        in
        let stmt = Sqlite3.prepare db sql in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
          (fun () ->
            ignore
              (Sqlite3.bind stmt 1
                 (Sqlite3.Data.TEXT (string_of_runner runner)));
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
            ignore
              (Sqlite3.bind stmt 12 (Sqlite3.Data.INT (if acp then 1L else 0L)));
            bind_opt 13 agent_name;
            bind_opt 14 follow_up_prompt;
            match Sqlite3.step stmt with
            | Sqlite3.Rc.DONE ->
                Ok (Int64.to_int (Sqlite3.last_insert_rowid db))
            | rc ->
                Error
                  (Printf.sprintf "Failed to enqueue background task: %s"
                     (Sqlite3.Rc.to_string rc)))

let select_columns =
  "id, runner, model, repo_path, prompt, COALESCE(branch, ''), worktree_path, \
   log_path, status, session_key, channel, channel_id, pid, result_preview, \
   created_at, started_at, finished_at, COALESCE(automerge, 0), \
   COALESCE(use_worktree, 1), merge_status, COALESCE(retry_count, 0), \
   parent_task_id, replaced_by, runner_session_id, COALESCE(acp, 0), \
   agent_name, notification_status, notification_error, \
   COALESCE(notification_attempts, 0), follow_up_prompt"

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
      (* B473: only abandon the wait for real interrupt reasons (restart,
         user-issued cancel). A pending queued inbound message is delivered
         inline at the end of the agent turn and should not collapse a
         background_task_wait into a 1-second no-op. *)
      let interrupted =
        match interrupt_check with
        | Some check -> (
            match check () with
            | None -> false
            | Some reason when reason = Agent.queued_message_interrupt_token ->
                false
            | Some _ -> true)
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

let log_excerpt ?db ?connector ?(offset = 0) ?(lines = 20) task =
  match (task.acp, db) with
  | true, Some db when Acp_history.has_history ~db ~task_id:task.id ->
      let connector =
        match connector with Some c -> c | None -> Format_adapter.Plain
      in
      Ok
        (Acp_history.format_for_display_rich ~db ~task_id:task.id ~connector
           ~max_lines:lines ())
  | _ -> (
      match task.log_path with
      | None -> Error (Printf.sprintf "Task %d has no log file yet" task.id)
      | Some path when not (Sys.file_exists path) ->
          Error (Printf.sprintf "Log file does not exist yet: %s" path)
      | Some path ->
          if offset > 0 then
            read_lines_window path ~offset ~limit:lines
            |> Result.map (fun (indexed_lines, total) ->
                let header =
                  Printf.sprintf "Log excerpt for task %d (%s)\npath: %s"
                    task.id
                    (string_of_status task.status)
                    path
                in
                if indexed_lines = [] then
                  header
                  ^ Printf.sprintf
                      "\n\n(No lines in requested range. Log has %d lines.)"
                      total
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
                         (Output truncated by size budget. Showing lines %d-%d \
                         of %d. Use offset=%d to continue.)"
                        offset
                        (offset + List.length rendered_lines - 1)
                        total next_offset
                    else if last_line < total then
                      Printf.sprintf
                        "\n\n\
                         (Showing lines %d-%d of %d. Use offset=%d to \
                         continue.)"
                        offset last_line total (last_line + 1)
                    else
                      Printf.sprintf "\n\n(End of log - total %d lines)" total
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
                  Printf.sprintf "Log excerpt for task %d (%s)\npath: %s"
                    task.id
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
                         (Output truncated by size budget. Showing lines %d-%d \
                         of %d. Use offset=%d to continue.)"
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
                  header ^ "\n\n" ^ rendered ^ footer ^ trunc_suffix))

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

let set_runner_session_id ~db ~id ~runner_session_id =
  let sql = "UPDATE background_tasks SET runner_session_id = ? WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT runner_session_id));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int id)));
      ignore (Sqlite3.step stmt))

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
  | Some task when task.runner <> Local && not (resume_supported task) ->
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
          if task.runner = Local then
            match Hashtbl.find_opt running id with
            | Some st -> st.cancelled := true
            | None -> ()
          else
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

let completion_sentinel = "OK_TASK_DONE_CHECKED_REBASED_COMMITED"

let completion_pass_message () =
  String.concat "\n"
    [
      "COMPLETION VERIFICATION — Your background task has finished its primary \
       work.";
      "Before this task can be finalized, complete these steps:";
      "";
      "1. Stage and commit ALL remaining changes (if any):";
      "   git add -A && git commit -m \"<appropriate commit msg here>\"";
      "2. Rebase your branch against the master branch (not origin/master):";
      "   git rebase master";
      "3. REVIEW AND FIX — Thoroughly review all changes on this branch (git \
       diff master..HEAD). Check for:";
      "   - Correctness: logic errors, off-by-one, missing edge cases";
      "   - Quality: code style, naming, unnecessary complexity";
      "   - Completeness: missing tests, incomplete implementations, TODOs";
      "   - Safety: no secrets, no injection vulnerabilities, no regressions";
      "   Fix any issues found, commit the fixes, and re-review until clean.";
      "4. Run any relevant tests/checks to verify your work, fix any issues";
      "5. Verify the worktree is completely clean: git status";
      "6. When ALL steps are done, output exactly as the only thing on the \
       last line:";
      "   " ^ completion_sentinel;
    ]

let request_completion_pass ~db ~id =
  let message = completion_pass_message () in
  ignore (queue_message ~db ~task_id:id ~message);
  requeue_for_resume ~db ~id ~result_preview:"completion pass queued";
  set_merge_status ~db ~id ~merge_status:"completion_pass"

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
              (match Hashtbl.find_opt running id with
              | Some st -> st.cancelled := true
              | None -> ());
              ignore
                (mark_cancelled ~db ~id ~result_preview:"Cancelled local task");
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
      "- Before reporting completion, rebase your branch against the master \
       branch (e.g., `git rebase master`) to ensure your changes are up to \
       date. If the rebase has conflicts, resolve straightforward ones and \
       continue.";
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
            | Stalled | Zombie | Process_missing | Startup_failed -> true
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
          (None, Some notify.channel, Some notify.target)
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
      "- Before reporting completion, rebase your branch against the master \
       branch (e.g., `git rebase master`) to ensure your changes are up to \
       date. If the rebase has conflicts, resolve straightforward ones and \
       continue.";
      "- Work only inside this directory/worktree.";
      "- Do not inspect or modify the original source repo path directly; use \
       only the files available in the current worktree.";
      "- Make the smallest focused change that completes the task well.";
      "- Run relevant verification when practical and mention what you ran.";
      "- Summarize the changes, results, and any follow-up concerns at the end.";
      "- Do not push or perform destructive git history edits.";
    ]

let delegate_enqueue ?context ?notify_cfg ?(check_available = true)
    ?(automerge = true) ?(use_worktree = true) ?(acp = false)
    ?(allow_claude = true) ?follow_up_prompt ~db ?preferred_runner ?model
    ?repo_path ?branch ~default_repo_path ~goal () =
  let chosen_repo_path =
    match repo_path with
    | Some path when String.trim path <> "" -> path
    | _ -> default_repo_path
  in
  if String.trim chosen_repo_path = "" then
    Error "Could not determine a repository path for delegation"
  else
    (* B649/B651: when use_worktree=true the harvest step requires the
       working path to be a git repo. Resolve symlinks so a workspace
       symlinked to a real repo is accepted; reject non-git paths upfront
       with a clear error so the task is not enqueued at all (instead of
       running and reporting status=dirty-worktree at the end). *)
    let chosen_repo_path =
      try Unix.realpath chosen_repo_path with _ -> chosen_repo_path
    in
    let validation =
      if use_worktree then validate_repo_path ~require_git:true chosen_repo_path
      else validate_workspace_path chosen_repo_path
    in
    match validation with
    | Error msg ->
        Error
          (Printf.sprintf
             "%s\n\
              Delegate with use_worktree=true requires a git repository; pass \
              use_worktree=false to run in a plain workspace, or point \
              repo_path at an actual git checkout."
             msg)
    | Ok () -> (
        match
          resolve_runner ~check_available ?preferred:preferred_runner
            ~allow_claude ()
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
                ~automerge ~use_worktree ~acp ?follow_up_prompt
                ~repo_path:chosen_repo_path ~prompt ?branch ?session_key
                ?channel ?channel_id ()
            with
            | Ok id -> Ok (id, runner, chosen_repo_path)
            | Error _ as err -> err))

let runner_to_framework_runner (r : runner) : Runner_framework.runner =
  match r with
  | Codex -> Codex
  | Claude -> Claude
  | Kimi -> Kimi
  | Gemini -> Gemini
  | Opencode -> Opencode
  | Cursor -> Cursor
  | Local -> assert false

let invocation_to_framework (inv : invocation) : Runner_framework.invocation =
  match inv with Fresh -> Fresh | Resume s -> Resume s

let command_of_task_with_invocation task invocation =
  let def =
    Runner_framework.runner_def_of_runner
      (runner_to_framework_runner task.runner)
  in
  Runner_framework.build_command_for ~model:task.model ~prompt:task.prompt
    ~runner_session_id:task.runner_session_id def
    (invocation_to_framework invocation)

let command_argv_of_task_with_invocation task invocation =
  let def =
    Runner_framework.runner_def_of_runner
      (runner_to_framework_runner task.runner)
  in
  match invocation_to_framework invocation with
  | Fresh ->
      def.build_fresh_argv ~model:task.model ~prompt:task.prompt
        ~pre_session_id:None
  | Resume prompt ->
      let mode =
        Runner_framework.resume_mode_of
          ~runner_session_id:task.runner_session_id
      in
      def.build_resume_argv ~model:task.model ~resume_mode:mode ~prompt

let command_of_task task = command_argv_of_task_with_invocation task Fresh

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

let spawn_task ?(on_task_started = fun _ -> Lwt.return_unit)
    ?(on_task_finished = fun _ -> Lwt.return_unit)
    ?(run_simple_command = run_simple_command) ?command_override
    ?(augment_env = fun ~session_key:_ ~task_id:_ env -> env) ~db (task : task)
    =
  Hashtbl.replace running task.id { cancelled = ref false };
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
              if task.acp && task.runner = Local then begin
                Logs.err (fun m ->
                    m "ACP mode is not supported with the Local runner");
                let () =
                  ignore
                    (finalize_completed_task ~db ~id:task.id ~exit_code:1
                       ~output:
                         "Error: ACP mode is not supported with the Local \
                          runner")
                in
                Lwt.return_unit
              end
              else if task.acp && command_override = None then begin
                (* ACP interactive path *)
                let acp_command =
                  Runner_framework.acp_argv_of_runner
                    (runner_to_framework_runner task.runner)
                in
                let effective_prompt =
                  match invocation with
                  | Fresh -> task_for_command.prompt
                  | Resume resume_text -> resume_text
                in
                ignore
                  (set_running ~db ~id:task.id ~branch ~worktree_path ~log_path
                     ~pid:0);
                let* () =
                  match get_task ~db ~id:task.id with
                  | Some t -> on_task_started t
                  | None -> Lwt.return_unit
                in
                let* exit_code, output =
                  Acp_client.run_task ~db ~task_id:task.id ~log_path
                    ~cwd:worktree_path ~prompt_text:effective_prompt
                    ~command:acp_command ()
                in
                let () =
                  if exit_code = 0 then
                    List.iter
                      (fun (msg : queued_message) ->
                        delete_queued_message ~db ~queue_id:msg.id)
                      queued_messages
                in
                ignore
                  (finalize_completed_task ~db ~id:task.id ~exit_code ~output);
                let* () =
                  match get_task ~db ~id:task.id with
                  | Some finished_task -> on_task_finished finished_task
                  | None -> Lwt.return_unit
                in
                Lwt.return_unit
              end
              else begin
                (* Legacy CLI-argument spawn path *)
                let command, pre_session_id =
                  match command_override with
                  | Some cmd -> (cmd, None)
                  | None ->
                      let result =
                        command_of_task_with_invocation task_for_command
                          invocation
                      in
                      ( Process_group.Exec result.Runner_framework.argv,
                        result.Runner_framework.pre_generated_session_id )
                in
                (match pre_session_id with
                | Some sid ->
                    set_runner_session_id ~db ~id:task.id ~runner_session_id:sid
                | None -> ());
                let base_env =
                  Runtime_config.augment_env_path (Unix.environment ())
                in
                let env =
                  match task.session_key with
                  | Some sk ->
                      augment_env ~session_key:sk ~task_id:task.id base_env
                  | None -> base_env
                in
                write_log_preamble ~log_path ~task_id:task.id ~command;
                let spawn_time = Unix.gettimeofday () in
                let proc =
                  Process_group.start_to_file ~cwd:worktree_path ~env ~log_path
                    command
                in
                let pid = proc.file_pid in
                if
                  not
                    (set_running ~db ~id:task.id ~branch ~worktree_path
                       ~log_path ~pid)
                then begin
                  Logs.warn (fun m ->
                      m "Background task %d: set_running failed; killing pid %d"
                        task.id pid);
                  let err_msg =
                    Printf.sprintf
                      "set_running failed: task %d was no longer queued (pid \
                       %d killed)"
                      task.id pid
                  in
                  append_log_error ~log_path err_msg;
                  let* () = Process_group.terminate_immediately pid in
                  let* _ = Process_group.wait pid in
                  finish ~db ~id:task.id ~status:Failed ~result_preview:err_msg;
                  Lwt.return_unit
                end
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
                            "Background task %d: child exited, killing \
                             remaining process group members"
                            task.id);
                      Process_group.signal_group pid Sys.sigkill;
                      Lwt.return_unit);
                  let elapsed = Unix.gettimeofday () -. spawn_time in
                  let output =
                    let raw = read_log_tail log_path preview_limit in
                    if elapsed < 5.0 then
                      Printf.sprintf "%s\n[clawq] process exited in %.1fs" raw
                        elapsed
                    else raw
                  in
                  (* Extract runner session ID from log if not set *)
                  (let current =
                     match get_task ~db ~id:task.id with
                     | Some t -> t.runner_session_id
                     | None -> None
                   in
                   if current = None then
                     let def =
                       Runner_framework.runner_def_of_runner
                         (runner_to_framework_runner task.runner)
                     in
                     let full_output = read_log_tail log_path (64 * 1024) in
                     match
                       Runner_framework.extract_session_id def full_output
                     with
                     | Some sid ->
                         set_runner_session_id ~db ~id:task.id
                           ~runner_session_id:sid
                     | None -> ());
                  let exit_code = exit_code_of_status status in
                  ignore
                    (finalize_completed_task ~db ~id:task.id ~exit_code ~output);
                  let* () =
                    match get_task ~db ~id:task.id with
                    | Some finished_task -> on_task_finished finished_task
                    | None -> Lwt.return_unit
                  in
                  Lwt.return_unit
              end)
        finalize)

let default_spawn_task ?augment_env ~on_task_started ~on_task_finished ~db task
    =
  spawn_task ?augment_env ~on_task_started ~on_task_finished ~db task

let local_task_timeout_seconds = Background_task_local.timeout_seconds_default

let local_task_deps : Background_task_local.deps =
  {
    prepare_worktree = (fun task -> prepare_worktree task);
    finish;
    get_task;
    set_running;
    list_queued_messages;
    delete_queued_message;
    resume_prompt_of_messages;
  }

let spawn_local_task ?timeout_seconds ~run_turn ~on_task_started
    ~on_task_finished ~db task =
  Background_task_local.spawn ?timeout_seconds local_task_deps ~run_turn
    ~on_task_started ~on_task_finished ~db task

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

let start_queued_with_callback ?max_running_tasks ?augment_env ~on_task_finished
    ~db ?(on_task_started = fun _ -> Lwt.return_unit) () =
  start_queued_with_callback_impl ?max_running_tasks
    ~spawn_task:(default_spawn_task ?augment_env)
    ~on_task_started ~on_task_finished ~db ()

let start_queued ?max_running_tasks ?augment_env ~db () =
  start_queued_with_callback ?max_running_tasks ?augment_env
    ~on_task_finished:(fun _ -> Lwt.return_unit)
    ~db ()

let start_queued_with_local_runner ~run_turn ?timeout_seconds ?max_running_tasks
    ?augment_env ~on_task_finished ~on_task_started ~db () =
  let spawn ~on_task_started ~on_task_finished ~db (task : task) =
    if task.runner = Local then
      spawn_local_task ?timeout_seconds ~run_turn ~on_task_started
        ~on_task_finished ~db task
    else
      default_spawn_task ?augment_env ~on_task_started ~on_task_finished ~db
        task
  in
  start_queued_with_callback_impl ?max_running_tasks ~spawn_task:spawn
    ~on_task_started ~on_task_finished ~db ()

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
          | Some _pid when _pid <= 0 ->
              Printf.sprintf
                "Local in-process task %d did not survive daemon restart — use \
                 'background retry %d' to re-queue"
                task.id task.id
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
  let all_running =
    List.filter
      (fun (t : task) -> t.status = Running && not (Hashtbl.mem running t.id))
      (list_tasks ~db)
  in
  List.iter
    (fun (t : task) ->
      match t.pid with
      | Some _pid when _pid <= 0 ->
          Logs.info (fun m ->
              m
                "Skipping readopt for local task %d (in-process tasks cannot \
                 survive daemon restart)"
                t.id)
      | _ -> ())
    all_running;
  let orphaned =
    List.filter
      (fun (t : task) ->
        match t.pid with
        | Some pid when pid > 0 -> pid_or_group_alive pid
        | _ -> false)
      all_running
  in
  let count = ref 0 in
  List.iter
    (fun task ->
      match task.pid with
      | Some pid ->
          Hashtbl.replace running task.id { cancelled = ref false };
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

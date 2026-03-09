type runner = Codex | Claude
type status = Queued | Running | Succeeded | Failed | Cancelled

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
}

let string_of_runner = function Codex -> "codex" | Claude -> "claude"

let runner_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "codex" -> Some Codex
  | "claude" | "claude-code" | "claude_code" -> Some Claude
  | _ -> None

let string_of_status = function
  | Queued -> "queued"
  | Running -> "running"
  | Succeeded -> "succeeded"
  | Failed -> "failed"
  | Cancelled -> "cancelled"

let status_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "queued" -> Queued
  | "running" -> Running
  | "succeeded" -> Succeeded
  | "failed" -> Failed
  | "cancelled" -> Cancelled
  | _ -> Failed

let is_terminal_status = function
  | Succeeded | Failed | Cancelled -> true
  | Queued | Running -> false

let runner_binary = function Codex -> "codex" | Claude -> "claude"

let command_exists command =
  Sys.command
    (Printf.sprintf "command -v %s >/dev/null 2>&1" (Filename.quote command))
  = 0

let path_is_git_repo path =
  Sys.command
    (Printf.sprintf "git -C %s rev-parse --is-inside-work-tree >/dev/null 2>&1"
       (Filename.quote path))
  = 0

let validate_repo_path repo_path =
  if String.trim repo_path = "" then Error "Repository path is required"
  else if not (Sys.file_exists repo_path) then
    Error (Printf.sprintf "Repository path does not exist: %s" repo_path)
  else if not (Sys.is_directory repo_path) then
    Error (Printf.sprintf "Repository path is not a directory: %s" repo_path)
  else if not (path_is_git_repo repo_path) then
    Error
      (Printf.sprintf "Repository path is not a git repository: %s" repo_path)
  else Ok ()

let runner_available runner = command_exists (runner_binary runner)

let resolve_runner ?(check_available = true) ?preferred () =
  let available runner = (not check_available) || runner_available runner in
  match preferred with
  | Some runner when available runner -> Ok runner
  | Some runner ->
      Error
        (Printf.sprintf "Runner '%s' is not available in PATH"
           (string_of_runner runner))
  | None when available Codex -> Ok Codex
  | None when available Claude -> Ok Claude
  | None ->
      Error
        "No supported background runner is available in PATH (looked for \
         'codex' and 'claude')"

let default_branch_name id = Printf.sprintf "clawq-bg-%d" id

let clawq_dir () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat home ".clawq"

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

let preview_text s =
  let trimmed = String.trim s in
  if String.length trimmed <= preview_limit then trimmed
  else String.sub trimmed 0 preview_limit ^ "..."

let status_summary = function
  | Queued -> "queued"
  | Running -> "running"
  | Succeeded -> "succeeded"
  | Failed -> "failed"
  | Cancelled -> "cancelled"

let format_task_summary (task : task) =
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
  add (Printf.sprintf "repo: %s" task.repo_path);
  add (Printf.sprintf "branch: %s" branch);
  add (Printf.sprintf "created_at: %s" task.created_at);
  (match task.started_at with
  | Some value -> add (Printf.sprintf "started_at: %s" value)
  | None -> ());
  (match task.finished_at with
  | Some value -> add (Printf.sprintf "finished_at: %s" value)
  | None -> ());
  (match task.worktree_path with
  | Some value -> add (Printf.sprintf "worktree: %s" value)
  | None -> ());
  (match task.log_path with
  | Some value -> add (Printf.sprintf "log: %s" value)
  | None -> ());
  (match task.result_preview with
  | Some text when String.trim text <> "" ->
      add (Printf.sprintf "result: %s" (preview_text text))
  | _ -> ());
  add (Printf.sprintf "prompt: %s" task.prompt);
  String.concat "\n" (List.rev !lines)

let max_inactive_shown = 3

let rec format_task_list tasks = format_task_list_with_hidden tasks 0

and format_task_list_with_hidden tasks hidden_count =
  if tasks = [] && hidden_count = 0 then "No background tasks."
  else
    let header =
      Printf.sprintf "  %-4s %-8s %-8s %-18s %s" "ID" "RUNNER" "STATUS" "BRANCH"
        "REPO"
    in
    let rows =
      List.map
        (fun (task : task) ->
          let branch = if task.branch = "" then "-" else task.branch in
          Printf.sprintf "  %-4d %-8s %-8s %-18s %s" task.id
            (string_of_runner task.runner)
            (string_of_status task.status)
            branch task.repo_path)
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
    "Background tasks:\n" ^ header ^ "\n" ^ String.concat "\n" rows ^ footer

let sql_text = function Sqlite3.Data.TEXT s -> Some s | _ -> None
let sql_int = function Sqlite3.Data.INT i -> Some (Int64.to_int i) | _ -> None

let task_of_stmt stmt =
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
  match
    Sqlite3.exec db "ALTER TABLE background_tasks ADD COLUMN model TEXT"
  with
  | Sqlite3.Rc.OK -> ()
  | Sqlite3.Rc.ERROR -> ()
  | rc ->
      failwith
        (Printf.sprintf "SQLite error: %s (sql: %s)" (Sqlite3.Rc.to_string rc)
           "ALTER TABLE background_tasks ADD COLUMN model TEXT")

let enqueue ~db ~runner ?model ~repo_path ~prompt ?branch ?session_key ?channel
    ?channel_id () =
  match validate_repo_path repo_path with
  | Error _ as err -> err
  | Ok () ->
      let sql =
        "INSERT INTO background_tasks (runner, model, repo_path, prompt, \
         branch, session_key, channel, channel_id) VALUES (?, ?, ?, ?, ?, ?, \
         ?, ?)"
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
          match Sqlite3.step stmt with
          | Sqlite3.Rc.DONE -> Ok (Int64.to_int (Sqlite3.last_insert_rowid db))
          | rc ->
              Error
                (Printf.sprintf "Failed to enqueue background task: %s"
                   (Sqlite3.Rc.to_string rc)))

let list_tasks ~db =
  let sql =
    "SELECT id, runner, model, repo_path, prompt, COALESCE(branch, ''), \
     worktree_path, log_path, status, session_key, channel, channel_id, pid, \
     result_preview, created_at, started_at, finished_at FROM background_tasks \
     ORDER BY id DESC"
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

let list_tasks_for_display ~db =
  let all = list_tasks ~db in
  let active, inactive =
    List.partition (fun t -> not (is_terminal_status t.status)) all
  in
  let recent_inactive =
    (* all is ordered by id DESC then reversed, so inactive preserves that;
       sort desc and take the most recent N *)
    let sorted = List.sort (fun a b -> compare b.id a.id) inactive in
    let rec take n = function
      | [] -> []
      | _ when n <= 0 -> []
      | x :: xs -> x :: take (n - 1) xs
    in
    take max_inactive_shown sorted
  in
  let hidden_count = List.length inactive - List.length recent_inactive in
  let visible =
    List.sort (fun a b -> compare b.id a.id) (active @ recent_inactive)
  in
  (visible, hidden_count)

let get_task ~db ~id =
  let sql =
    "SELECT id, runner, model, repo_path, prompt, COALESCE(branch, ''), \
     worktree_path, log_path, status, session_key, channel, channel_id, pid, \
     result_preview, created_at, started_at, finished_at FROM background_tasks \
     WHERE id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (task_of_stmt stmt)
      | _ -> None)

let rec wait_until_terminal ?(timeout_seconds = 300.0) ?(poll_seconds = 1.0) ~db
    ~id () =
  let open Lwt.Syntax in
  match get_task ~db ~id with
  | None ->
      Lwt.return
        (Error (Printf.sprintf "No background task found with id %d" id))
  | Some task when is_terminal_status task.status -> Lwt.return (Ok task)
  | Some _ when timeout_seconds <= 0.0 ->
      Lwt.return
        (Error
           (Printf.sprintf "Timed out waiting for background task %d to finish"
              id))
  | Some _ ->
      let sleep_for = Float.min poll_seconds timeout_seconds in
      let* () = Lwt_unix.sleep sleep_for in
      wait_until_terminal
        ~timeout_seconds:(timeout_seconds -. sleep_for)
        ~poll_seconds ~db ~id ()

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
                  if count < lines then acc @ [ line ]
                  else List.tl acc @ [ line ]
                in
                let count = Int.min lines (count + 1) in
                loop acc count
            | exception End_of_file -> Ok acc
          in
          loop [] 0)
    with Sys_error msg -> Error msg

let log_excerpt ?(lines = 40) task =
  match task.log_path with
  | None -> Error (Printf.sprintf "Task %d has no log file yet" task.id)
  | Some path when not (Sys.file_exists path) ->
      Error (Printf.sprintf "Log file does not exist yet: %s" path)
  | Some path ->
      read_last_lines path ~lines
      |> Result.map (fun chunks ->
          let header =
            Printf.sprintf "Log excerpt for task %d (%s)\npath: %s" task.id
              (string_of_status task.status)
              path
          in
          if chunks = [] then header ^ "\n\n(log file is empty)"
          else header ^ "\n\n" ^ String.concat "\n" chunks)

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
      | Succeeded | Failed ->
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

let build_delegate_prompt ~goal =
  String.concat "\n"
    [
      "You are a delegated background coding agent running in an isolated git \
       worktree.";
      "";
      "Goal:";
      goal;
      "";
      "Execution contract:";
      "- Work only inside this worktree for the target repository.";
      "- Make the smallest focused change that completes the task well.";
      "- Run relevant verification when practical and mention what you ran.";
      "- Summarize the changes, results, and any follow-up concerns at the end.";
      "- Do not commit, push, or perform destructive git history edits unless \
       explicitly asked.";
    ]

let delegate_enqueue ?context ?notify_cfg ?(check_available = true) ~db
    ?preferred_runner ?model ?repo_path ?branch ~default_repo_path ~goal () =
  let chosen_repo_path =
    match repo_path with
    | Some path when String.trim path <> "" -> path
    | _ -> default_repo_path
  in
  if String.trim chosen_repo_path = "" then
    Error "Could not determine a repository path for delegation"
  else
    match validate_repo_path chosen_repo_path with
    | Error _ as err -> err
    | Ok () -> (
        match
          resolve_runner ~check_available ?preferred:preferred_runner ()
        with
        | Error _ as err -> err
        | Ok runner -> (
            let prompt = build_delegate_prompt ~goal in
            let session_key, channel, channel_id =
              routing_from_context ?context ?notify_cfg ()
            in
            match
              enqueue ~db ~runner ?model ~repo_path:chosen_repo_path ~prompt
                ?branch ?session_key ?channel ?channel_id ()
            with
            | Ok id -> Ok (id, runner, chosen_repo_path)
            | Error _ as err -> err))

let command_of_task task =
  let model_args flag =
    match task.model with
    | Some model when String.trim model <> "" -> [| flag; model |]
    | _ -> [||]
  in
  match task.runner with
  | Codex ->
      Array.concat
        [
          [| "codex"; "exec" |];
          model_args "--model";
          [| "--dangerously-bypass-approvals-and-sandbox"; task.prompt |];
        ]
  | Claude ->
      Array.concat
        [
          [| "claude"; "-p" |];
          model_args "--model";
          [| "--dangerously-skip-permissions"; task.prompt |];
        ]

let parse_sqlite_datetime s =
  try
    Scanf.sscanf s "%d-%d-%d %d:%d:%d" (fun y mo d h mi s ->
        let tm =
          {
            Unix.tm_sec = s;
            tm_min = mi;
            tm_hour = h;
            tm_mday = d;
            tm_mon = mo - 1;
            tm_year = y - 1900;
            tm_wday = 0;
            tm_yday = 0;
            tm_isdst = false;
          }
        in
        fst (Unix.mktime tm))
  with _ -> 0.0

let format_elapsed_seconds secs =
  let secs = max 0 (int_of_float secs) in
  if secs < 60 then "<1m"
  else
    let mins = secs / 60 in
    let hours = mins / 60 in
    if hours = 0 then Printf.sprintf "%dm" mins
    else if hours >= 2 then "2h+"
    else Printf.sprintf "%dh%dm" hours (mins mod 60)

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

let terse_finished_message (task : task) =
  let elapsed = elapsed_string task in
  let status_word =
    match task.status with
    | Succeeded -> "succeeded"
    | Failed -> "failed"
    | Cancelled -> "cancelled"
    | Queued -> "queued"
    | Running -> "running"
  in
  let base =
    Printf.sprintf "[bg #%d %s: %s (%s)" task.id status_word (task_label task)
      elapsed
  in
  match (task.status, task.result_preview) with
  | Failed, Some preview ->
      let short =
        if String.length preview > 80 then String.sub preview 0 80 ^ "..."
        else preview
      in
      base ^ " -- " ^ short ^ "]"
  | _ -> base ^ "]"

let status_message (task : task) =
  let headline =
    Printf.sprintf "Background task %d finished: %s (%s)" task.id
      (status_summary task.status)
      (string_of_runner task.runner)
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
  let branch =
    if task.branch <> "" then task.branch else default_branch_name task.id
  in
  let worktree_path = task_worktree_path task.id in
  let log_path = task_log_path task.id in
  ensure_roots ();
  ensure_parent_dir log_path;
  let open Lwt.Syntax in
  if Sys.file_exists worktree_path then
    Lwt.return
      (Error (Printf.sprintf "Worktree path already exists: %s" worktree_path))
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
    ?(run_simple_command = run_simple_command) ?command_override ~db task =
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
              let command =
                match command_override with
                | Some cmd -> cmd
                | None ->
                    Process_group.Exec
                      (command_of_task
                         {
                           task with
                           branch;
                           worktree_path = Some worktree_path;
                           log_path = Some log_path;
                         })
              in
              let proc =
                Process_group.start ~cwd:worktree_path
                  ~env:(Unix.environment ()) command
              in
              let pid = proc.pid in
              if
                not
                  (set_running ~db ~id:task.id ~branch ~worktree_path ~log_path
                     ~pid)
              then
                let* () = Process_group.terminate_immediately pid in
                let* _ = Process_group.wait pid in
                let* () = Process_group.close proc in
                Lwt.return_unit
              else
                let* () =
                  match get_task ~db ~id:task.id with
                  | Some t -> on_task_started t
                  | None -> Lwt.return_unit
                in
                let stdout_buf = Buffer.create 1024 in
                let stderr_buf = Buffer.create 256 in
                let wait_promise = Process_group.wait proc.pid in
                (* B210 watchdog: after child exits, give IO 2s to drain,
                   then kill remaining process group members that may be
                   holding pipe fds open (e.g. grandchildren) *)
                Lwt.async (fun () ->
                    let open Lwt.Syntax in
                    let* _status = wait_promise in
                    let* () = Lwt_unix.sleep 2.0 in
                    Logs.info (fun m ->
                        m
                          "Background task %d: child exited, killing remaining \
                           process group members"
                          task.id);
                    Process_group.signal_group proc.pid Sys.sigkill;
                    Lwt.return_unit);
                let* status =
                  Lwt.finalize
                    (fun () ->
                      let* () =
                        Lwt_io.with_file ~mode:Lwt_io.Output log_path
                          (fun log_oc ->
                            Lwt.join
                              [
                                read_into_buffer_and_log
                                  proc.Process_group.stdout log_oc stdout_buf;
                                read_into_buffer_and_log
                                  proc.Process_group.stderr log_oc stderr_buf;
                              ])
                      in
                      wait_promise)
                    (fun () -> Process_group.close proc)
                in
                let output =
                  String.trim
                    (Buffer.contents stdout_buf ^ "\n"
                   ^ Buffer.contents stderr_buf)
                in
                let exit_code = exit_code_of_status status in
                let final_status =
                  match get_task ~db ~id:task.id with
                  | Some { status = Cancelled; _ } -> Cancelled
                  | _ -> if exit_code = 0 then Succeeded else Failed
                in
                let result_preview =
                  if output = "" then
                    Printf.sprintf "Process exited with code %d" exit_code
                  else Printf.sprintf "exit %d: %s" exit_code output
                in
                finish ~db ~id:task.id ~status:final_status ~result_preview;
                let* () =
                  match get_task ~db ~id:task.id with
                  | Some finished_task -> on_task_finished finished_task
                  | None -> Lwt.return_unit
                in
                Lwt.return_unit)
        finalize)

let default_spawn_task ~on_task_started ~on_task_finished ~db task =
  spawn_task ~on_task_started ~on_task_finished ~db task

let start_queued_with_callback_impl ~spawn_task ~on_task_started
    ~on_task_finished ~db =
  let queued =
    List.filter
      (fun t -> t.status = Queued && not (Hashtbl.mem running t.id))
      (list_tasks ~db)
  in
  List.iter (spawn_task ~on_task_started ~on_task_finished ~db) queued

let start_queued_with_callback ~on_task_finished ~db
    ?(on_task_started = fun _ -> Lwt.return_unit) () =
  start_queued_with_callback_impl ~spawn_task:default_spawn_task
    ~on_task_started ~on_task_finished ~db

let start_queued ~db =
  start_queued_with_callback ~on_task_finished:(fun _ -> Lwt.return_unit) ~db ()

let is_tracked_locally id = Hashtbl.mem running id

let reap_dead_running_tasks ~db ~on_task_finished =
  let running_in_db =
    List.filter
      (fun t -> t.status = Running && not (Hashtbl.mem running t.id))
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
                "Process group %d no longer alive (orphaned/crashed)" pid
          | None -> "No PID recorded for running task"
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

let enqueue_tool_with_notify ~notify_cfg ~db =
  {
    Tool.name = "background_task_enqueue";
    description =
      "Queue a background Codex or Claude coding task in its own git worktree. \
       Lower-level alternative to delegate — use when you need explicit \
       control over runner, repo, branch, or model. Use delegate for simple \
       'spawn a subagent' requests.";
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
                      ("enum", `List [ `String "codex"; `String "claude" ]);
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
                          "Implementation prompt to hand to Codex or Claude." );
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
        match runner_of_string runner_s with
        | None -> Lwt.return "Error: runner must be 'codex' or 'claude'"
        | Some runner when String.trim repo_path = "" ->
            Lwt.return "Error: repo_path is required"
        | Some _ when String.trim prompt = "" ->
            Lwt.return "Error: prompt is required"
        | Some runner -> (
            let session_key, channel, channel_id =
              routing_from_context ?context ?notify_cfg ()
            in
            match
              enqueue ~db ~runner ?model ~repo_path ~prompt ?branch ?session_key
                ?channel ?channel_id ()
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
       current status, repo, branch, log path, and result preview.";
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
              ] );
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let task_id =
          try Some (args |> member "id" |> to_int) with _ -> None
        in
        match task_id with
        | Some id -> (
            match get_task ~db ~id with
            | Some task -> Lwt.return (format_task_summary task)
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
      "Wait for a background coding task to finish, then return its final \
       status summary.";
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
                          "Maximum time to wait before returning an error." );
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        let timeout_seconds =
          try args |> member "timeout_seconds" |> to_number with _ -> 300.0
        in
        if id < 0 then Lwt.return "Error: id is required"
        else
          let open Lwt.Syntax in
          let* result = wait_until_terminal ~timeout_seconds ~db ~id () in
          match result with
          | Ok task -> Lwt.return (format_task_summary task)
          | Error msg -> Lwt.return ("Error: " ^ msg));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let logs_tool ~db =
  {
    Tool.name = "background_task_logs";
    description = "Read the latest lines from a background task log file.";
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
                ( "lines",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "How many trailing log lines to return (default 40)."
                      );
                    ] );
              ] );
          ("required", `List [ `String "id" ]);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let id = try args |> member "id" |> to_int with _ -> -1 in
        let lines = try args |> member "lines" |> to_int with _ -> 40 in
        if id < 0 then Lwt.return "Error: id is required"
        else
          match get_task ~db ~id with
          | None ->
              Lwt.return
                (Printf.sprintf "Error: No background task found with id %d" id)
          | Some task -> (
              match log_excerpt ~lines task with
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
      "Delegate a coding task to a background subagent (Codex or Claude) that \
       runs in its own git worktree. Use when asked to spawn subagents, use \
       workers, or run tasks with a specific model (e.g. 'use haiku to ...', \
       'delegate to sonnet'). Auto-selects runner and repo by default.";
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
                          [ `String "auto"; `String "codex"; `String "claude" ]
                      );
                      ( "description",
                        `String
                          "Optional runner choice. 'auto' prefers Codex when \
                           available, then Claude." );
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
                    (None, Some "runner must be 'auto', 'codex', or 'claude'"))
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
        if String.trim goal = "" then Lwt.return "Error: goal is required"
        else if runner_error <> None then
          Lwt.return ("Error: " ^ Option.get runner_error)
        else
          match
            delegate_enqueue ?context ?notify_cfg ~check_available ~db
              ?preferred_runner:runner_pref ?model ?repo_path ?branch
              ~default_repo_path ~goal ()
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

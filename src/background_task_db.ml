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
  (* Wrap SELECT + DELETE in a single transaction so another reader cannot
     grab the same messages between the read and the delete. *)
  let exec_sql sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | rc ->
        failwith
          (Printf.sprintf "take_queued_messages txn error: %s (sql: %s)"
             (Sqlite3.Rc.to_string rc) sql)
  in
  exec_sql "BEGIN IMMEDIATE";
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.exec db "COMMIT"))
    (fun () ->
      let rows = list_queued_messages ~db ~task_id in
      List.iter (fun msg -> delete_queued_message ~db ~queue_id:msg.id) rows;
      rows)

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

(* Reactive enqueue signal: daemon waits on this instead of pure sleep *)
let enqueue_condition = Lwt_condition.create ()
let signal_enqueue () = Lwt_condition.signal enqueue_condition ()

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
                signal_enqueue ();
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

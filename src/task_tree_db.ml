open Task_tree_types

let init_schema db =
  Memory.exec_exn db
    "CREATE TABLE IF NOT EXISTS task_tree (\n\
    \  id TEXT NOT NULL,\n\
    \  session_key TEXT NOT NULL,\n\
    \  parent_id TEXT,\n\
    \  title TEXT NOT NULL,\n\
    \  status TEXT NOT NULL DEFAULT 'pending',\n\
    \  note TEXT,\n\
    \  depends_on TEXT NOT NULL DEFAULT '[]',\n\
    \  agent_model TEXT,\n\
    \  agent_type TEXT,\n\
    \  agent_prompt TEXT,\n\
    \  agent_details TEXT,\n\
    \  autostart INTEGER NOT NULL DEFAULT 0,\n\
    \  agent_task_id INTEGER,\n\
    \  sort_order INTEGER NOT NULL DEFAULT 0,\n\
    \  deleted_at TEXT,\n\
    \  profile_id INTEGER,\n\
    \  origin_json TEXT,\n\
    \  thread_id TEXT,\n\
    \  requester TEXT,\n\
    \  created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \  updated_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \  PRIMARY KEY (session_key, id)\n\
     )";
  Memory.exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_task_tree_session ON task_tree \
     (session_key)";
  Memory.exec_exn db
    "CREATE TABLE IF NOT EXISTS task_tree_archive (\n\
    \  id TEXT NOT NULL,\n\
    \  session_key TEXT NOT NULL,\n\
    \  parent_id TEXT,\n\
    \  title TEXT NOT NULL,\n\
    \  status TEXT NOT NULL,\n\
    \  note TEXT,\n\
    \  sort_order INTEGER NOT NULL DEFAULT 0,\n\
    \  created_at TEXT NOT NULL,\n\
    \  completed_at TEXT NOT NULL,\n\
    \  archived_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \  archive_group INTEGER NOT NULL\n\
     )";
  Memory.exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_task_tree_archive_session ON \
     task_tree_archive (session_key)";
  let try_alter sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | Sqlite3.Rc.ERROR
      when String.starts_with ~prefix:"duplicate column name"
             (Sqlite3.errmsg db) ->
        ()
    | rc ->
        failwith
          (Printf.sprintf "SQLite error: %s (sql: %s)" (Sqlite3.Rc.to_string rc)
             sql)
  in
  try_alter
    "ALTER TABLE task_tree ADD COLUMN depends_on TEXT NOT NULL DEFAULT '[]'";
  try_alter "ALTER TABLE task_tree ADD COLUMN agent_model TEXT";
  try_alter "ALTER TABLE task_tree ADD COLUMN agent_type TEXT";
  try_alter "ALTER TABLE task_tree ADD COLUMN agent_prompt TEXT";
  try_alter "ALTER TABLE task_tree ADD COLUMN agent_details TEXT";
  try_alter
    "ALTER TABLE task_tree ADD COLUMN autostart INTEGER NOT NULL DEFAULT 0";
  try_alter "ALTER TABLE task_tree ADD COLUMN agent_task_id INTEGER";
  try_alter "ALTER TABLE task_tree ADD COLUMN profile_id INTEGER";
  try_alter "ALTER TABLE task_tree ADD COLUMN origin_json TEXT";
  try_alter "ALTER TABLE task_tree ADD COLUMN thread_id TEXT";
  try_alter "ALTER TABLE task_tree ADD COLUMN requester TEXT"

let load_tasks ?(include_deleted = false) ~db ~session_key () =
  let deleted_filter =
    if include_deleted then "" else " AND deleted_at IS NULL"
  in
  let sql =
    Printf.sprintf
      "SELECT id, session_key, parent_id, title, status, note, sort_order, \
       deleted_at, depends_on, agent_model, agent_type, agent_prompt, \
       agent_details, autostart, agent_task_id, profile_id, origin_json, \
       thread_id, requester FROM task_tree WHERE session_key = ?%s ORDER BY \
       sort_order ASC, id ASC"
      deleted_filter
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key) : Sqlite3.Rc.t);
      let results = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let id =
          Sqlite3.column stmt 0 |> sql_text |> Option.value ~default:""
        in
        let sk =
          Sqlite3.column stmt 1 |> sql_text |> Option.value ~default:""
        in
        let parent_id =
          match Sqlite3.column stmt 2 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let title =
          Sqlite3.column stmt 3 |> sql_text |> Option.value ~default:""
        in
        let status =
          match Sqlite3.column stmt 4 with
          | Sqlite3.Data.TEXT s -> (
              match status_of_string s with Some st -> st | None -> Pending)
          | _ -> Pending
        in
        let note = Sqlite3.column stmt 5 |> sql_text in
        let sort_order =
          Sqlite3.column stmt 6 |> sql_int |> Option.value ~default:0
        in
        let deleted_at = Sqlite3.column stmt 7 |> sql_text in
        let depends_on =
          Sqlite3.column stmt 8 |> sql_text |> Option.value ~default:"[]"
          |> json_string_list_of_text
        in
        let agent_model = Sqlite3.column stmt 9 |> sql_text in
        let agent_type = Sqlite3.column stmt 10 |> sql_text in
        let agent_prompt = Sqlite3.column stmt 11 |> sql_text in
        let agent_details = Sqlite3.column stmt 12 |> sql_text in
        let autostart = Sqlite3.column stmt 13 |> sql_bool in
        let agent_task_id = Sqlite3.column stmt 14 |> sql_int in
        let profile_id = Sqlite3.column stmt 15 |> sql_int in
        let origin_json = Sqlite3.column stmt 16 |> sql_text in
        let thread_id = Sqlite3.column stmt 17 |> sql_text in
        let requester = Sqlite3.column stmt 18 |> sql_text in
        results :=
          {
            id;
            session_key = sk;
            parent_id;
            title;
            status;
            note;
            depends_on;
            agent_model;
            agent_type;
            agent_prompt;
            agent_details;
            autostart;
            agent_task_id;
            sort_order;
            deleted_at;
            profile_id;
            origin_json;
            thread_id;
            requester;
          }
          :: !results
      done;
      List.rev !results)

let count_tasks ~db ~session_key =
  Memory.query_single_int_with_params db
    "SELECT COUNT(*) FROM task_tree WHERE session_key = ? AND deleted_at IS \
     NULL"
    [ Sqlite3.Data.TEXT session_key ]

let dependencies_terminal ~tasks (task : task) =
  List.for_all
    (fun dep_id ->
      let dep_id = resolve_existing_id ~tasks ~id:dep_id in
      match List.find_opt (fun t -> t.id = dep_id) tasks with
      | Some dep -> is_terminal dep.status
      | None -> false)
    task.depends_on

let ready_autostart_tasks ~db ~session_key =
  let tasks = load_tasks ~db ~session_key () in
  tasks
  |> List.filter (fun (task : task) ->
      task.status = Pending && task.autostart && task.agent_task_id = None
      && Option.value
           (Option.map
              (fun prompt -> String.trim prompt <> "")
              task.agent_prompt)
           ~default:false
      && dependencies_terminal ~tasks task)
  |> List.sort (fun a b ->
      let c = compare a.sort_order b.sort_order in
      if c <> 0 then c else compare a.id b.id)

let find_active_session_key ~db ~preferred =
  if count_tasks ~db ~session_key:preferred > 0 then Some preferred
  else
    let sql =
      "SELECT session_key FROM task_tree WHERE status IN ('pending', \
       'in_progress') AND deleted_at IS NULL GROUP BY session_key ORDER BY \
       COUNT(*) DESC LIMIT 1"
    in
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match Sqlite3.column stmt 0 with
            | Sqlite3.Data.TEXT s -> Some s
            | _ -> None)
        | _ -> None)

let next_auto_id ~db ~session_key =
  let sql =
    "SELECT MAX(CAST(id AS INTEGER)) FROM task_tree WHERE session_key = ? AND \
     id GLOB '[0-9]*'"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key) : Sqlite3.Rc.t);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          let tasks = load_tasks ~include_deleted:true ~db ~session_key () in
          let rec next_available n =
            let candidate = string_of_int n in
            match display_id_collision ~tasks ~id:candidate with
            | Some _ -> next_available (n + 1)
            | None -> candidate
          in
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> next_available (Int64.to_int n + 1)
          | _ -> next_available 1)
      | _ -> "1")

let id_exists ~db ~session_key ~id =
  let sql = "SELECT COUNT(*) FROM task_tree WHERE session_key = ? AND id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key) : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT id) : Sqlite3.Rc.t);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n > 0
          | _ -> false)
      | _ -> false)

let task_depth ~tasks ~id =
  let rec go current_id depth =
    if depth > max_depth then depth
    else
      match List.find_opt (fun t -> t.id = current_id) tasks with
      | Some t -> (
          match t.parent_id with
          | Some pid -> go pid (depth + 1)
          | None -> depth)
      | None -> depth
  in
  go id 0

let get_children ~tasks ~id = List.filter (fun t -> t.parent_id = Some id) tasks

(* B641: when an ID lookup misses, return up to 3 close-match suggestions so
   the agent can recover instead of hallucinating another bad ID. Match by
   prefix first (cheap, dominant case), then by case-insensitive equality. *)
let suggest_similar_ids ~tasks ~id =
  let lower s = String.lowercase_ascii s in
  let target_lc = lower id in
  let prefix_matches =
    List.filter_map
      (fun t ->
        let tl = lower t.id in
        if tl = target_lc then Some t.id
        else if
          String.length tl >= String.length target_lc
          && String.sub tl 0 (String.length target_lc) = target_lc
        then Some t.id
        else None)
      tasks
  in
  let take n l =
    let rec loop acc i = function
      | [] -> List.rev acc
      | _ when i = 0 -> List.rev acc
      | x :: rest -> loop (x :: acc) (i - 1) rest
    in
    loop [] n l
  in
  take 3 prefix_matches

let not_found_error ~tasks ~id =
  let suggestions = suggest_similar_ids ~tasks ~id in
  let hint =
    match suggestions with
    | [] ->
        "Run shell_exec `bl tree` (or `bl list --bugs` / `bl list --ideas`) to \
         enumerate current IDs. Bug IDs are B-prefixed, ideas are I-prefixed; \
         phase/milestone/epic/task IDs use P/M/E/T prefixes joined by dots \
         (e.g. P1.M2.E3.T004). IDs are case-sensitive."
    | xs ->
        Printf.sprintf
          "Did you mean: %s? Run `bl tree` to enumerate all current IDs. IDs \
           are case-sensitive."
          (String.concat ", " xs)
  in
  Printf.sprintf "Task '%s' not found. %s" id hint

let rec get_subtree_ids ~tasks ~id =
  let children = get_children ~tasks ~id in
  id :: List.concat_map (fun c -> get_subtree_ids ~tasks ~id:c.id) children

let count_in_progress ~db ~session_key =
  Memory.query_single_int_with_params db
    "SELECT COUNT(*) FROM task_tree WHERE session_key = ? AND status = \
     'in_progress' AND deleted_at IS NULL"
    [ Sqlite3.Data.TEXT session_key ]

let get_ancestors ~tasks ~id =
  let rec go current_id acc =
    match List.find_opt (fun t -> t.id = current_id) tasks with
    | Some t -> (
        match t.parent_id with
        | Some pid -> go pid (t :: acc)
        | None -> t :: acc)
    | None -> acc
  in
  go id []

let next_sort_order ~db ~session_key =
  Memory.query_single_int_with_params db
    "SELECT COALESCE(MAX(sort_order), 0) + 1 FROM task_tree WHERE session_key \
     = ?"
    [ Sqlite3.Data.TEXT session_key ]

let insert_task ~db ~session_key ~id ~parent_id ~title ~status ~note ~depends_on
    ~agent_model ~agent_type ~agent_prompt ~agent_details ~autostart ?profile_id
    ?origin_json ?thread_id ?requester () =
  let sort_order = next_sort_order ~db ~session_key in
  let sql =
    "INSERT INTO task_tree (id, session_key, parent_id, title, status, note, \
     depends_on, agent_model, agent_type, agent_prompt, agent_details, \
     autostart, sort_order, profile_id, origin_json, thread_id, requester) \
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id) : Sqlite3.Rc.t);
      ignore
        (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT session_key) : Sqlite3.Rc.t);
      ignore
        (Sqlite3.bind stmt 3
           (match parent_id with
           | Some p -> Sqlite3.Data.TEXT p
           | None -> Sqlite3.Data.NULL)
          : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT title) : Sqlite3.Rc.t);
      ignore
        (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT (string_of_status status))
          : Sqlite3.Rc.t);
      ignore
        (Sqlite3.bind stmt 6
           (match note with
           | Some n -> Sqlite3.Data.TEXT n
           | None -> Sqlite3.Data.NULL)
          : Sqlite3.Rc.t);
      ignore
        (Sqlite3.bind stmt 7
           (Sqlite3.Data.TEXT (text_of_json_string_list depends_on))
          : Sqlite3.Rc.t);
      let bind_opt idx = function
        | Some value when String.trim value <> "" ->
            ignore
              (Sqlite3.bind stmt idx (Sqlite3.Data.TEXT value) : Sqlite3.Rc.t)
        | _ -> ignore (Sqlite3.bind stmt idx Sqlite3.Data.NULL : Sqlite3.Rc.t)
      in
      bind_opt 8 agent_model;
      bind_opt 9 agent_type;
      bind_opt 10 agent_prompt;
      bind_opt 11 agent_details;
      ignore
        (Sqlite3.bind stmt 12 (Sqlite3.Data.INT (if autostart then 1L else 0L))
          : Sqlite3.Rc.t);
      ignore
        (Sqlite3.bind stmt 13 (Sqlite3.Data.INT (Int64.of_int sort_order))
          : Sqlite3.Rc.t);
      (* profile_id *)
      (match profile_id with
      | Some pid ->
          ignore
            (Sqlite3.bind stmt 14 (Sqlite3.Data.INT (Int64.of_int pid))
              : Sqlite3.Rc.t)
      | None -> ignore (Sqlite3.bind stmt 14 Sqlite3.Data.NULL : Sqlite3.Rc.t));
      bind_opt 15 origin_json;
      bind_opt 16 thread_id;
      bind_opt 17 requester;
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Printf.sprintf "SQLite error inserting task: %s"
               (Sqlite3.Rc.to_string rc)))

let update_task_status ~db ~session_key ~id ~status =
  let sql =
    "UPDATE task_tree SET status = ?, updated_at = datetime('now') WHERE \
     session_key = ? AND id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT (string_of_status status))
          : Sqlite3.Rc.t);
      ignore
        (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT session_key) : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT id) : Sqlite3.Rc.t);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Printf.sprintf "SQLite error updating task: %s"
               (Sqlite3.Rc.to_string rc)))

let update_task_agent_metadata ~db ~session_key ~id ~depends_on ~agent_model
    ~agent_type ~agent_prompt ~agent_details ~autostart =
  let sql =
    "UPDATE task_tree SET depends_on = ?, agent_model = ?, agent_type = ?, \
     agent_prompt = ?, agent_details = ?, autostart = ?, updated_at = \
     datetime('now') WHERE session_key = ? AND id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.TEXT (text_of_json_string_list depends_on))
          : Sqlite3.Rc.t);
      let bind_opt idx = function
        | Some value when String.trim value <> "" ->
            ignore
              (Sqlite3.bind stmt idx (Sqlite3.Data.TEXT value) : Sqlite3.Rc.t)
        | _ -> ignore (Sqlite3.bind stmt idx Sqlite3.Data.NULL : Sqlite3.Rc.t)
      in
      bind_opt 2 agent_model;
      bind_opt 3 agent_type;
      bind_opt 4 agent_prompt;
      bind_opt 5 agent_details;
      ignore
        (Sqlite3.bind stmt 6 (Sqlite3.Data.INT (if autostart then 1L else 0L))
          : Sqlite3.Rc.t);
      ignore
        (Sqlite3.bind stmt 7 (Sqlite3.Data.TEXT session_key) : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 8 (Sqlite3.Data.TEXT id) : Sqlite3.Rc.t);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Printf.sprintf "SQLite error updating task agent metadata: %s"
               (Sqlite3.Rc.to_string rc)))

let mark_agent_started ~db ~session_key ~id ~agent_task_id =
  let sql =
    "UPDATE task_tree SET agent_task_id = ?, updated_at = datetime('now') \
     WHERE session_key = ? AND id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int agent_task_id))
          : Sqlite3.Rc.t);
      ignore
        (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT session_key) : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT id) : Sqlite3.Rc.t);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Printf.sprintf "SQLite error marking task agent started: %s"
               (Sqlite3.Rc.to_string rc)))

let update_task_note ~db ~session_key ~id ~note =
  let sql =
    "UPDATE task_tree SET note = ?, updated_at = datetime('now') WHERE \
     session_key = ? AND id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (match note with
           | Some n -> Sqlite3.Data.TEXT n
           | None -> Sqlite3.Data.NULL)
          : Sqlite3.Rc.t);
      ignore
        (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT session_key) : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT id) : Sqlite3.Rc.t);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Printf.sprintf "SQLite error updating note: %s"
               (Sqlite3.Rc.to_string rc)))

let delete_task ~db ~session_key ~id =
  let sql = "DELETE FROM task_tree WHERE session_key = ? AND id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key) : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT id) : Sqlite3.Rc.t);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Printf.sprintf "SQLite error deleting task: %s"
               (Sqlite3.Rc.to_string rc)))

let soft_delete_task ~db ~session_key ~id =
  let sql =
    "UPDATE task_tree SET deleted_at = datetime('now'), updated_at = \
     datetime('now') WHERE session_key = ? AND id = ? AND deleted_at IS NULL"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key) : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT id) : Sqlite3.Rc.t);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Printf.sprintf "SQLite error soft-deleting task: %s"
               (Sqlite3.Rc.to_string rc)))

let update_sort_order ~db ~session_key ~id ~sort_order =
  let sql =
    "UPDATE task_tree SET sort_order = ?, updated_at = datetime('now') WHERE \
     session_key = ? AND id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int sort_order))
          : Sqlite3.Rc.t);
      ignore
        (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT session_key) : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT id) : Sqlite3.Rc.t);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Printf.sprintf "SQLite error updating sort_order: %s"
               (Sqlite3.Rc.to_string rc)))

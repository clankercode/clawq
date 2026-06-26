type status = Pending | In_progress | Done | Task_error | Cancelled

let string_of_status = function
  | Pending -> "pending"
  | In_progress -> "in_progress"
  | Done -> "done"
  | Task_error -> "error"
  | Cancelled -> "cancelled"

let status_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "pending" -> Some Pending
  | "in_progress" -> Some In_progress
  | "done" -> Some Done
  | "error" -> Some Task_error
  | "cancelled" -> Some Cancelled
  | _ -> None

let is_terminal = function Done | Cancelled -> true | _ -> false

type task = {
  id : string;
  session_key : string;
  parent_id : string option;
  title : string;
  status : status;
  note : string option;
  depends_on : string list;
  agent_model : string option;
  agent_type : string option;
  agent_prompt : string option;
  agent_details : string option;
  autostart : bool;
  agent_task_id : int option;
  sort_order : int;
  deleted_at : string option;
}

let max_depth = 5
let warn_concurrent_in_progress = 5
let max_batch_size = 50
let max_title_length = 200
let tree_wrap_columns = 80

let is_digit_string s =
  String.length s > 0
  && String.for_all (function '0' .. '9' -> true | _ -> false) s

let display_id id = if is_digit_string id then "T" ^ id else id
let display_ids ids = String.concat ", " (List.map display_id ids)

let display_id_collision ~tasks ~id =
  let candidate_display_id = display_id id in
  List.find_opt
    (fun task -> task.id <> id && display_id task.id = candidate_display_id)
    tasks

let json_string_list_of_text text =
  try
    match Yojson.Safe.from_string text with
    | `List values ->
        List.filter_map
          (function
            | `String s when String.trim s <> "" -> Some (String.trim s)
            | _ -> None)
          values
    | _ -> []
  with _ -> []

let text_of_json_string_list values =
  values |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> List.map (fun s -> `String s)
  |> fun values -> Yojson.Safe.to_string (`List values)

let sql_text = function Sqlite3.Data.TEXT s -> Some s | _ -> None
let sql_int = function Sqlite3.Data.INT n -> Some (Int64.to_int n) | _ -> None
let sql_bool = function Sqlite3.Data.INT n -> Int64.to_int n <> 0 | _ -> false

let strip_legacy_id_prefix id =
  let id = String.trim id in
  if String.length id > 0 && id.[0] = '#' then
    String.sub id 1 (String.length id - 1)
  else id

let is_hash_prefixed_id id =
  let id = String.trim id in
  String.length id > 0 && id.[0] = '#'

let legacy_numeric_id id =
  let id = strip_legacy_id_prefix id in
  if String.length id > 1 && id.[0] = 'T' then
    let rest = String.sub id 1 (String.length id - 1) in
    if is_digit_string rest then Some rest else None
  else None

let resolve_existing_id ~tasks ~id =
  let id = strip_legacy_id_prefix id in
  if List.exists (fun t -> t.id = id) tasks then id
  else
    match legacy_numeric_id id with
    | Some legacy_id when List.exists (fun t -> t.id = legacy_id) tasks ->
        legacy_id
    | _ -> id

let utf8_step s i =
  let b = Char.code s.[i] in
  if b land 0x80 = 0 then 1
  else if b land 0xE0 = 0xC0 then 2
  else if b land 0xF0 = 0xE0 then 3
  else if b land 0xF8 = 0xF0 then 4
  else 1

let utf8_columns s =
  let len = String.length s in
  let rec loop i columns =
    if i >= len then columns else loop (i + utf8_step s i) (columns + 1)
  in
  loop 0 0

let split_at_columns s columns =
  let len = String.length s in
  let rec loop i used =
    if i >= len || used >= columns then i
    else loop (i + utf8_step s i) (used + 1)
  in
  let cut = loop 0 0 in
  (String.sub s 0 cut, String.sub s cut (len - cut))

let add_wrapped_line buf ~initial_prefix ~continuation_prefix text =
  let words =
    String.split_on_char ' ' text |> List.filter (fun s -> String.length s > 0)
  in
  let width = tree_wrap_columns in
  let rec emit prefix words =
    let available = max 1 (width - utf8_columns prefix) in
    let rec fill acc used = function
      | [] -> (String.concat " " (List.rev acc), [])
      | word :: rest ->
          let sep = if acc = [] then 0 else 1 in
          let word_cols = utf8_columns word in
          if used + sep + word_cols <= available then
            fill (word :: acc) (used + sep + word_cols) rest
          else if acc = [] then
            let chunk, remaining = split_at_columns word available in
            let rest = if remaining = "" then rest else remaining :: rest in
            (chunk, rest)
          else (String.concat " " (List.rev acc), word :: rest)
    in
    let line, rest = fill [] 0 words in
    Buffer.add_string buf prefix;
    Buffer.add_string buf line;
    Buffer.add_char buf '\n';
    if rest <> [] then emit continuation_prefix rest
  in
  emit initial_prefix words

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
  try_alter "ALTER TABLE task_tree ADD COLUMN agent_task_id INTEGER"

let load_tasks ?(include_deleted = false) ~db ~session_key () =
  let deleted_filter =
    if include_deleted then "" else " AND deleted_at IS NULL"
  in
  let sql =
    Printf.sprintf
      "SELECT id, session_key, parent_id, title, status, note, sort_order, \
       deleted_at, depends_on, agent_model, agent_type, agent_prompt, \
       agent_details, autostart, agent_task_id FROM task_tree WHERE \
       session_key = ?%s ORDER BY sort_order ASC, id ASC"
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
    ~agent_model ~agent_type ~agent_prompt ~agent_details ~autostart =
  let sort_order = next_sort_order ~db ~session_key in
  let sql =
    "INSERT INTO task_tree (id, session_key, parent_id, title, status, note, \
     depends_on, agent_model, agent_type, agent_prompt, agent_details, \
     autostart, sort_order) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
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

let status_icon = function
  | Pending -> "[ ]"
  | In_progress -> "[>]"
  | Done -> "[x]"
  | Task_error -> "[!]"
  | Cancelled -> "[-]"

let status_emoji = function
  | Pending -> "\xe2\xac\x9c"
  | In_progress -> "\xf0\x9f\x94\xb6"
  | Done -> "\xe2\x9c\x85"
  | Task_error -> "\xe2\x9d\x8c"
  | Cancelled -> "\xe2\x9e\x96"

let render_task_tree tasks =
  let buf = Buffer.create 512 in
  let rec render_children ~parent_id ~prefix =
    let children =
      List.filter (fun t -> t.parent_id = parent_id) tasks
      |> List.sort (fun a b -> compare a.sort_order b.sort_order)
    in
    let total = List.length children in
    List.iteri
      (fun i t ->
        let is_last = i = total - 1 in
        let connector = if is_last then "└── " else "├── " in
        let child_prefix =
          if is_last then prefix ^ "    " else prefix ^ "│   "
        in
        let note_str =
          match t.note with Some n -> " (" ^ n ^ ")" | None -> ""
        in
        let metadata =
          List.filter_map
            (fun item -> item)
            [
              (match t.agent_type with
              | Some agent -> Some ("agent=" ^ agent)
              | None -> None);
              (if t.autostart then Some "autostart" else None);
              (match t.depends_on with
              | [] -> None
              | deps -> Some ("depends_on=" ^ display_ids deps));
              (match t.agent_task_id with
              | Some id -> Some (Printf.sprintf "bg=%d" id)
              | None -> None);
            ]
        in
        let metadata_str =
          match metadata with
          | [] -> ""
          | items -> " {" ^ String.concat ", " items ^ "}"
        in
        let deleted_str = if t.deleted_at <> None then " [deleted]" else "" in
        let text =
          Printf.sprintf "%s %s %s%s%s%s" (status_icon t.status)
            (display_id t.id) t.title note_str metadata_str deleted_str
        in
        add_wrapped_line buf ~initial_prefix:(prefix ^ connector)
          ~continuation_prefix:child_prefix text;
        render_children ~parent_id:(Some t.id) ~prefix:child_prefix)
      children
  in
  render_children ~parent_id:None ~prefix:"";
  let result = Buffer.contents buf in
  if String.length result > 0 && result.[String.length result - 1] = '\n' then
    String.sub result 0 (String.length result - 1)
  else result

let render_tree ~db ~session_key =
  let tasks = load_tasks ~db ~session_key () in
  if tasks = [] then
    "No tasks tracked. Use the task_tree tool to plan and track your work.\n\
     Breaking complex goals into subtasks helps maintain focus across long \
     sessions."
  else render_task_tree tasks

let render_tree_with_legend ~db ~session_key =
  let tree = render_tree ~db ~session_key in
  if count_tasks ~db ~session_key = 0 then tree
  else
    let ip_count = count_in_progress ~db ~session_key in
    let warning =
      if ip_count >= warn_concurrent_in_progress then (
        let buf = Buffer.create 128 in
        Buffer.add_string buf "\n\n";
        add_wrapped_line buf ~initial_prefix:"" ~continuation_prefix:""
          (Printf.sprintf
             "\xe2\x9a\xa0\xef\xb8\x8f WARNING: %d tasks are in_progress. \
              Consider completing or updating some before starting more work."
             ip_count);
        Buffer.contents buf)
      else ""
    in
    tree
    ^ "\n\n\
       Legend: [ ] pending  [>] in_progress  [x] done  [!] error  [-] cancelled"
    ^ warning

let render_emoji_tree ?(max_title_chars = 50) ~db ~session_key () =
  ignore max_title_chars;
  let tasks = load_tasks ~db ~session_key () in
  if tasks = [] then
    "No tasks tracked. Use the task_tree tool to plan and track your work.\n\
     Breaking complex goals into subtasks helps maintain focus across long \
     sessions."
  else
    let buf = Buffer.create 512 in
    let rec render_children ~parent_id ~prefix =
      let children =
        List.filter (fun t -> t.parent_id = parent_id) tasks
        |> List.sort (fun a b -> compare a.sort_order b.sort_order)
      in
      let total = List.length children in
      List.iteri
        (fun i t ->
          let is_last = i = total - 1 in
          let connector =
            if is_last then "\xe2\x94\x94\xe2\x94\x80\xe2\x94\x80 "
            else "\xe2\x94\x9c\xe2\x94\x80\xe2\x94\x80 "
          in
          let child_prefix =
            if is_last then prefix ^ "    " else prefix ^ "\xe2\x94\x82   "
          in
          let text =
            Printf.sprintf "%s %s %s" (status_emoji t.status) (display_id t.id)
              t.title
          in
          add_wrapped_line buf ~initial_prefix:(prefix ^ connector)
            ~continuation_prefix:child_prefix text;
          render_children ~parent_id:(Some t.id) ~prefix:child_prefix)
        children
    in
    render_children ~parent_id:None ~prefix:"";
    (* Summary line *)
    let n_pending = ref 0 in
    let n_active = ref 0 in
    let n_done = ref 0 in
    let n_error = ref 0 in
    let n_cancelled = ref 0 in
    List.iter
      (fun t ->
        match t.status with
        | Pending -> incr n_pending
        | In_progress -> incr n_active
        | Done -> incr n_done
        | Task_error -> incr n_error
        | Cancelled -> incr n_cancelled)
      tasks;
    let total = List.length tasks in
    let counts =
      List.filter_map
        (fun (n, label) ->
          if n > 0 then Some (Printf.sprintf "%d %s" n label) else None)
        [
          (!n_pending, "pending");
          (!n_active, "active");
          (!n_done, "done");
          (!n_error, "error");
          (!n_cancelled, "cancelled");
        ]
    in
    Buffer.add_string buf
      (Printf.sprintf "\n%d tasks \xc2\xb7 %s" total
         (String.concat " \xc2\xb7 " counts));
    let ip_count = !n_active in
    if ip_count >= warn_concurrent_in_progress then begin
      let warning =
        Printf.sprintf
          "\xe2\x9a\xa0\xef\xb8\x8f WARNING: %d tasks are in_progress. \
           Consider completing or updating some before starting more work."
          ip_count
      in
      Buffer.add_string buf "\n\n";
      add_wrapped_line buf ~initial_prefix:"" ~continuation_prefix:"" warning
    end;
    Buffer.contents buf

let render_compact ~db ~session_key =
  let tasks = load_tasks ~db ~session_key () in
  if tasks = [] then
    "No tasks tracked. Use the task_tree tool to plan and track your work.\n\
     Breaking complex goals into subtasks helps maintain focus across long \
     sessions."
  else
    let buf = Buffer.create 256 in
    (* Count by status *)
    let n_pending = ref 0 in
    let n_active = ref 0 in
    let n_done = ref 0 in
    let n_error = ref 0 in
    let n_cancelled = ref 0 in
    List.iter
      (fun t ->
        match t.status with
        | Pending -> incr n_pending
        | In_progress -> incr n_active
        | Done -> incr n_done
        | Task_error -> incr n_error
        | Cancelled -> incr n_cancelled)
      tasks;
    let total = List.length tasks in
    (* Summary line with non-zero counts *)
    let counts =
      List.filter_map
        (fun (n, label) ->
          if n > 0 then Some (Printf.sprintf "%d %s" n label) else None)
        [
          (!n_pending, "pending");
          (!n_active, "active");
          (!n_done, "done");
          (!n_error, "error");
          (!n_cancelled, "cancelled");
        ]
    in
    Buffer.add_string buf
      (Printf.sprintf "Tasks: %d total (%s)" total (String.concat ", " counts));
    (* Active section *)
    let active_tasks =
      List.filter (fun t -> t.status = In_progress) tasks
      |> List.sort (fun a b -> compare a.sort_order b.sort_order)
    in
    if active_tasks <> [] then begin
      Buffer.add_string buf "\nActive:";
      List.iter
        (fun t ->
          let note_str =
            match t.note with Some n -> " (" ^ n ^ ")" | None -> ""
          in
          Buffer.add_string buf
            (Printf.sprintf "\n  [>] %s — %s%s" (display_id t.id) t.title
               note_str))
        active_tasks
    end;
    (* Blocked section *)
    let error_tasks =
      List.filter (fun t -> t.status = Task_error) tasks
      |> List.sort (fun a b -> compare a.sort_order b.sort_order)
    in
    if error_tasks <> [] then begin
      Buffer.add_string buf "\nBlocked:";
      List.iter
        (fun t ->
          let note_str =
            match t.note with Some n -> " (" ^ n ^ ")" | None -> ""
          in
          Buffer.add_string buf
            (Printf.sprintf "\n  [!] %s — %s%s" (display_id t.id) t.title
               note_str))
        error_tasks
    end;
    (* Next: root-actionable pending tasks (no pending ancestor) *)
    let pending_tasks =
      List.filter (fun t -> t.status = Pending) tasks
      |> List.sort (fun a b -> compare a.sort_order b.sort_order)
    in
    let actionable =
      List.filter
        (fun t ->
          let ancestors = get_ancestors ~tasks ~id:t.id in
          not
            (List.exists
               (fun a -> a.id <> t.id && a.status = Pending)
               ancestors))
        pending_tasks
    in
    if actionable <> [] then begin
      let show = List.filteri (fun i _ -> i < 3) actionable in
      let overflow = List.length actionable - 3 in
      Buffer.add_string buf "\nNext:";
      List.iter
        (fun t ->
          Buffer.add_string buf
            (Printf.sprintf "\n  [ ] %s — %s" (display_id t.id) t.title))
        show;
      if overflow > 0 then
        Buffer.add_string buf (Printf.sprintf "\n  (+%d more)" overflow)
    end;
    (* Archive nudge *)
    let n_archivable = !n_done + !n_cancelled in
    if n_archivable > 0 then
      Buffer.add_string buf
        (Printf.sprintf "\n(%d done — archive to save tokens)" n_archivable);
    Buffer.contents buf

let render_focus ~db ~session_key =
  let tasks = load_tasks ~db ~session_key () in
  if tasks = [] then
    "No tasks tracked. Use the task_tree tool to plan and track your work.\n\
     Breaking complex goals into subtasks helps maintain focus across long \
     sessions."
  else
    let buf = Buffer.create 256 in
    (* --- counts --- *)
    let n_pending = ref 0 in
    let n_active = ref 0 in
    let n_done = ref 0 in
    let n_error = ref 0 in
    let n_cancelled = ref 0 in
    List.iter
      (fun t ->
        match t.status with
        | Pending -> incr n_pending
        | In_progress -> incr n_active
        | Done -> incr n_done
        | Task_error -> incr n_error
        | Cancelled -> incr n_cancelled)
      tasks;
    let total = List.length tasks in
    let counts =
      List.filter_map
        (fun (n, label) ->
          if n > 0 then Some (Printf.sprintf "%d %s" n label) else None)
        [
          (!n_pending, "pending");
          (!n_active, "active");
          (!n_done, "done");
          (!n_error, "error");
          (!n_cancelled, "cancelled");
        ]
    in
    Buffer.add_string buf
      (Printf.sprintf "Tasks: %d total (%s)" total (String.concat ", " counts));
    (* --- active with path --- *)
    let active_tasks =
      List.filter (fun t -> t.status = In_progress) tasks
      |> List.sort (fun a b -> compare a.sort_order b.sort_order)
    in
    if active_tasks <> [] then begin
      Buffer.add_string buf "\nActive:";
      List.iter
        (fun t ->
          let note_str =
            match t.note with Some n -> " (" ^ n ^ ")" | None -> ""
          in
          let ancs = get_ancestors ~tasks ~id:t.id in
          let path_ancs =
            match List.rev ancs with _ :: rest -> List.rev rest | [] -> []
          in
          let path_str =
            if path_ancs = [] then ""
            else
              "\n    path: "
              ^ String.concat " > " (List.map (fun a -> a.title) path_ancs)
          in
          Buffer.add_string buf
            (Printf.sprintf "\n  [>] %s — %s%s%s" (display_id t.id) t.title
               note_str path_str))
        active_tasks
    end;
    (* --- blocked --- *)
    let error_tasks =
      List.filter (fun t -> t.status = Task_error) tasks
      |> List.sort (fun a b -> compare a.sort_order b.sort_order)
    in
    if error_tasks <> [] then begin
      Buffer.add_string buf "\nBlocked:";
      List.iter
        (fun t ->
          let note_str = match t.note with Some n -> " — " ^ n | None -> "" in
          Buffer.add_string buf
            (Printf.sprintf "\n  [!] %s — %s%s" (display_id t.id) t.title
               note_str))
        error_tasks
    end;
    (* --- next: children of active first, then actionable pending --- *)
    let pending_tasks =
      List.filter (fun t -> t.status = Pending) tasks
      |> List.sort (fun a b -> compare a.sort_order b.sort_order)
    in
    let active_ids = List.map (fun t -> t.id) active_tasks in
    let children_of_active =
      List.filter
        (fun t ->
          match t.parent_id with
          | Some pid -> List.mem pid active_ids
          | None -> false)
        pending_tasks
    in
    let actionable =
      List.filter
        (fun t ->
          let ancestors = get_ancestors ~tasks ~id:t.id in
          not
            (List.exists
               (fun a -> a.id <> t.id && a.status = Pending)
               ancestors))
        pending_tasks
    in
    let next_tasks =
      if children_of_active <> [] then children_of_active else actionable
    in
    if next_tasks <> [] then begin
      let show = List.filteri (fun i _ -> i < 3) next_tasks in
      let overflow = List.length next_tasks - 3 in
      Buffer.add_string buf "\nNext:";
      List.iter
        (fun t ->
          Buffer.add_string buf
            (Printf.sprintf "\n  [ ] %s — %s" (display_id t.id) t.title))
        show;
      if overflow > 0 then
        Buffer.add_string buf (Printf.sprintf "\n  (+%d more)" overflow)
    end;
    (* --- archive nudge --- *)
    let n_archivable = !n_done + !n_cancelled in
    if n_archivable > 0 then
      Buffer.add_string buf
        (Printf.sprintf "\n(%d done — archive to save tokens)" n_archivable);
    Buffer.contents buf

let format_notification ~connector ~db ~session_key (ops : Yojson.Safe.t list) =
  let open Yojson.Safe.Util in
  let meaningful_ops =
    List.filter
      (fun op_json ->
        match try op_json |> member "op" |> to_string with _ -> "" with
        | "add" | "update" | "remove" | "clear" | "archive" | "restore" -> true
        | _ -> false)
      ops
  in
  if meaningful_ops = [] then None
  else begin
    let count_op name =
      List.fold_left
        (fun acc op_json ->
          let op = try op_json |> member "op" |> to_string with _ -> "" in
          if op = name then acc + 1 else acc)
        0 meaningful_ops
    in
    let plural n singular plural =
      if n = 1 then Printf.sprintf "%d %s" n singular
      else Printf.sprintf "%d %s" n plural
    in
    let tasks = load_tasks ~db ~session_key () in
    let display_input_id id =
      let ids =
        String.split_on_char ',' id
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
      in
      let ids = if ids = [] then [ id ] else ids in
      ids
      |> List.map (fun id -> resolve_existing_id ~tasks ~id |> display_id)
      |> String.concat ", "
    in
    let update_details =
      List.filter_map
        (fun op_json ->
          let op = try op_json |> member "op" |> to_string with _ -> "" in
          if op <> "update" then None
          else
            let id = try op_json |> member "id" |> to_string with _ -> "?" in
            let status =
              try Some (op_json |> member "status" |> to_string)
              with _ -> None
            in
            let id = Format_adapter.code connector (display_input_id id) in
            Some
              (match status with
              | Some s ->
                  Printf.sprintf "Updated %s -> %s" id
                    (Format_adapter.code connector s)
              | None -> Printf.sprintf "Updated %s" id))
        meaningful_ops
    in
    let lines = Buffer.create 128 in
    let add_count = count_op "add" in
    if add_count > 0 then
      Buffer.add_string lines
        (Printf.sprintf "Added %s\n" (plural add_count "task" "tasks"));
    List.iter
      (fun line ->
        Buffer.add_string lines line;
        Buffer.add_char lines '\n')
      update_details;
    let remove_count = count_op "remove" in
    if remove_count > 0 then
      Buffer.add_string lines
        (Printf.sprintf "Soft-deleted %s\n"
           (plural remove_count "task" "tasks"));
    if count_op "clear" > 0 then
      Buffer.add_string lines "Soft-deleted completed tasks\n";
    let archive_count = count_op "archive" in
    if archive_count > 0 then
      Buffer.add_string lines
        (Printf.sprintf "Archived %s\n" (plural archive_count "tree" "trees"));
    let restore_count = count_op "restore" in
    if restore_count > 0 then
      Buffer.add_string lines
        (Printf.sprintf "Restored %s\n" (plural restore_count "task" "tasks"));
    let in_progress =
      List.filter (fun t -> t.status = In_progress) tasks
      |> List.sort (fun a b -> compare a.sort_order b.sort_order)
    in
    let error_tasks =
      List.filter (fun t -> t.status = Task_error) tasks
      |> List.sort (fun a b -> compare a.sort_order b.sort_order)
    in
    let pending_tasks =
      List.filter (fun t -> t.status = Pending) tasks
      |> List.sort (fun a b -> compare a.sort_order b.sort_order)
    in
    let active_ids = List.map (fun t -> t.id) in_progress in
    let children_of_active =
      List.filter
        (fun t ->
          match t.parent_id with
          | Some pid -> List.mem pid active_ids
          | None -> false)
        pending_tasks
    in
    let actionable =
      List.filter
        (fun t ->
          let ancestors = get_ancestors ~tasks ~id:t.id in
          not
            (List.exists
               (fun a -> a.id <> t.id && a.status = Pending)
               ancestors))
        pending_tasks
    in
    let next_task =
      match (children_of_active, actionable) with
      | h :: _, _ -> Some h
      | [], h :: _ -> Some h
      | [], [] -> None
    in
    let add_hint label t =
      Buffer.add_string lines
        (Printf.sprintf "%s: %s %s\n" label
           (Format_adapter.code connector (display_id t.id))
           t.title)
    in
    (match (in_progress, error_tasks, next_task) with
    | t :: _, _, _ -> add_hint "Focus" t
    | [], t :: _, _ -> add_hint "Blocked" t
    | [], [], Some t -> add_hint "Next" t
    | [], [], None -> ());
    let content = String.trim (Buffer.contents lines) in
    if content = "" then None
    else
      let header = Format_adapter.bold connector "Task tree updated" ^ "\n" in
      Some (header ^ content)
  end

(* Validate and execute add operation *)
let do_add ~db ~session_key ~id ~parent_id ~title ~status ~note ~depends_on
    ~agent_model ~agent_type ~agent_prompt ~agent_details ~autostart =
  if String.length title > max_title_length then
    Error
      (Printf.sprintf "Title too long (%d chars, max %d)" (String.length title)
         max_title_length)
  else if String.length title = 0 then
    Error "Title is required for add. Provide a 'title' field."
  else
    match id with
    | Some custom_id when is_hash_prefixed_id custom_id ->
        Error
          (Printf.sprintf
             "Task ID '%s' is invalid: explicit add IDs must not start with \
              '#'. To fix: omit 'id' for auto-assignment, or use a non-# \
              custom ID. Use display references such as T1, or legacy #1 \
              references, only when referring to existing tasks."
             custom_id)
    | _ -> (
        let actual_id =
          match id with Some i -> i | None -> next_auto_id ~db ~session_key
        in
        if id_exists ~db ~session_key ~id:actual_id then
          Error
            (Printf.sprintf
               "Task ID '%s' already exists. Choose a different 'id' or omit \
                it for auto-assignment."
               actual_id)
        else
          let all_tasks =
            load_tasks ~include_deleted:true ~db ~session_key ()
          in
          match display_id_collision ~tasks:all_tasks ~id:actual_id with
          | Some existing ->
              Error
                (Printf.sprintf
                   "Task ID '%s' collides with existing task ID '%s': both \
                    display as display ID '%s'. Choose a different 'id' or \
                    omit it for auto-assignment."
                   actual_id existing.id (display_id actual_id))
          | None ->
              let tasks = load_tasks ~db ~session_key () in
              let parent_id =
                Option.map
                  (fun pid -> resolve_existing_id ~tasks ~id:pid)
                  parent_id
              in
              let parent_depth =
                match parent_id with
                | None -> 0
                | Some pid -> task_depth ~tasks ~id:pid + 1
              in
              if parent_depth >= max_depth then
                Error
                  (Printf.sprintf
                     "Max nesting depth exceeded (max %d levels). Flatten the \
                      hierarchy or archive completed subtrees first."
                     max_depth)
              else
                let parent_valid =
                  match parent_id with
                  | Some pid -> id_exists ~db ~session_key ~id:pid
                  | None -> true
                in
                if not parent_valid then
                  Error
                    (Printf.sprintf
                       "Parent task '%s' not found. Use 'depth' for batch tree \
                        building (depth 0 = root), or set 'parent' to an \
                        existing task ID. Omit both for a root task."
                       (Option.get parent_id))
                else begin
                  let actual_status =
                    match status with Some s -> s | None -> Pending
                  in
                  match
                    insert_task ~db ~session_key ~id:actual_id ~parent_id ~title
                      ~status:actual_status ~note ~depends_on ~agent_model
                      ~agent_type ~agent_prompt ~agent_details ~autostart
                  with
                  | Ok () -> Ok actual_id
                  | Error e -> Error e
                end)

(* Validate and execute update operation *)
let do_update ~db ~session_key ~id ~status ~note ~depends_on ~agent_model
    ~agent_type ~agent_prompt ~agent_details ~autostart =
  let tasks = load_tasks ~db ~session_key () in
  let id = resolve_existing_id ~tasks ~id in
  match List.find_opt (fun t -> t.id = id) tasks with
  | None -> Error (not_found_error ~tasks ~id)
  | Some task -> (
      match
        ( status,
          note,
          depends_on,
          agent_model,
          agent_type,
          agent_prompt,
          agent_details,
          autostart )
      with
      | None, None, None, None, None, None, None, None ->
          Error "Update requires at least status, note, or agent metadata"
      | _ ->
          let result = ref (Ok ()) in
          (match status with
          | Some new_status -> (
              (* Lifecycle validation *)
              (match new_status with
              | Done ->
                  let children = get_children ~tasks ~id in
                  let incomplete =
                    List.filter (fun c -> not (is_terminal c.status)) children
                  in
                  if incomplete <> [] then begin
                    let child_ids =
                      display_ids (List.map (fun c -> c.id) incomplete)
                    in
                    result :=
                      Error
                        (Printf.sprintf
                           "Cannot mark %s done — children still incomplete: %s"
                           (display_id id) child_ids)
                  end
              | In_progress -> ()
              | _ -> ());
              match !result with
              | Error _ -> ()
              | Ok () -> (
                  match
                    update_task_status ~db ~session_key ~id ~status:new_status
                  with
                  | Error e -> result := Error e
                  | Ok () ->
                      (* in_progress propagation: promote pending ancestors *)
                      if new_status = In_progress then begin
                        let ancestors = get_ancestors ~tasks ~id in
                        List.iter
                          (fun anc ->
                            if anc.status = Pending && anc.id <> id then
                              ignore
                                (update_task_status ~db ~session_key ~id:anc.id
                                   ~status:In_progress))
                          ancestors
                      end))
          | None -> ());
          (match (!result, note) with
          | Ok (), Some n -> (
              match update_task_note ~db ~session_key ~id ~note:(Some n) with
              | Error e -> result := Error e
              | Ok () -> ())
          | Ok (), None -> ()
          | Error _, _ -> ());
          (match !result with
          | Error _ -> ()
          | Ok () -> (
              match
                ( depends_on,
                  agent_model,
                  agent_type,
                  agent_prompt,
                  agent_details,
                  autostart )
              with
              | None, None, None, None, None, None -> ()
              | _ -> (
                  let depends_on =
                    Option.value depends_on ~default:task.depends_on
                  in
                  let agent_model =
                    match agent_model with
                    | Some _ -> agent_model
                    | None -> task.agent_model
                  in
                  let agent_type =
                    match agent_type with
                    | Some _ -> agent_type
                    | None -> task.agent_type
                  in
                  let agent_prompt =
                    match agent_prompt with
                    | Some _ -> agent_prompt
                    | None -> task.agent_prompt
                  in
                  let agent_details =
                    match agent_details with
                    | Some _ -> agent_details
                    | None -> task.agent_details
                  in
                  let autostart =
                    Option.value autostart ~default:task.autostart
                  in
                  match
                    update_task_agent_metadata ~db ~session_key ~id ~depends_on
                      ~agent_model ~agent_type ~agent_prompt ~agent_details
                      ~autostart
                  with
                  | Ok () -> ()
                  | Error e -> result := Error e)));
          !result)

(* Validate and execute remove operation — soft-deletes instead of hard-deletes *)
let do_remove ~db ~session_key ~id ?(recursive = false) () =
  let tasks = load_tasks ~db ~session_key () in
  let id = resolve_existing_id ~tasks ~id in
  match List.find_opt (fun t -> t.id = id) tasks with
  | None -> Error (not_found_error ~tasks ~id)
  | Some _ ->
      let subtree_ids = get_subtree_ids ~tasks ~id in
      if not recursive then begin
        let has_in_progress =
          List.exists
            (fun sid ->
              match List.find_opt (fun t -> t.id = sid) tasks with
              | Some t -> t.status = In_progress
              | None -> false)
            subtree_ids
        in
        if has_in_progress then
          Error
            (Printf.sprintf
               "Cannot remove %s — subtree contains in_progress tasks. Use \
                recursive=true to force-remove the entire subtree."
               (display_id id))
        else begin
          let ids_reversed = List.rev subtree_ids in
          List.iter
            (fun sid -> ignore (soft_delete_task ~db ~session_key ~id:sid))
            ids_reversed;
          Ok (List.length subtree_ids)
        end
      end
      else begin
        (* recursive=true: soft-delete all, no in_progress guard *)
        let ids_reversed = List.rev subtree_ids in
        List.iter
          (fun sid -> ignore (soft_delete_task ~db ~session_key ~id:sid))
          ids_reversed;
        Ok (List.length subtree_ids)
      end

(* Soft-delete all done/cancelled tasks; returns the count affected *)
let do_clear ~db ~session_key =
  Memory.exec_with_params db
    "UPDATE task_tree SET deleted_at = datetime('now'), updated_at = \
     datetime('now') WHERE session_key = ? AND status IN ('done', 'cancelled') \
     AND deleted_at IS NULL"
    [ Sqlite3.Data.TEXT session_key ];
  Ok (Sqlite3.changes db)

(* Archive completed subtrees *)
let do_archive ~db ~session_key ~id =
  let tasks = load_tasks ~db ~session_key () in
  let next_archive_group () =
    Memory.query_single_int_with_params db
      "SELECT COALESCE(MAX(archive_group), 0) + 1 FROM task_tree_archive WHERE \
       session_key = ?"
      [ Sqlite3.Data.TEXT session_key ]
  in
  let archive_subtree root_id =
    let subtree_ids = get_subtree_ids ~tasks ~id:root_id in
    let all_terminal =
      List.for_all
        (fun sid ->
          match List.find_opt (fun t -> t.id = sid) tasks with
          | Some t -> is_terminal t.status
          | None -> true)
        subtree_ids
    in
    if not all_terminal then
      Error
        (Printf.sprintf
           "Cannot archive %s — subtree contains non-terminal tasks"
           (display_id root_id))
    else begin
      let group = next_archive_group () in
      List.iter
        (fun sid ->
          match List.find_opt (fun t -> t.id = sid) tasks with
          | Some t ->
              let sql =
                "INSERT INTO task_tree_archive (id, session_key, parent_id, \
                 title, status, note, sort_order, created_at, completed_at, \
                 archived_at, archive_group) VALUES (?, ?, ?, ?, ?, ?, ?, \
                 datetime('now'), datetime('now'), datetime('now'), ?)"
              in
              let stmt = Sqlite3.prepare db sql in
              Fun.protect
                ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
                (fun () ->
                  ignore
                    (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT t.id)
                      : Sqlite3.Rc.t);
                  ignore
                    (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT t.session_key)
                      : Sqlite3.Rc.t);
                  ignore
                    (Sqlite3.bind stmt 3
                       (match t.parent_id with
                       | Some p -> Sqlite3.Data.TEXT p
                       | None -> Sqlite3.Data.NULL)
                      : Sqlite3.Rc.t);
                  ignore
                    (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT t.title)
                      : Sqlite3.Rc.t);
                  ignore
                    (Sqlite3.bind stmt 5
                       (Sqlite3.Data.TEXT (string_of_status t.status))
                      : Sqlite3.Rc.t);
                  ignore
                    (Sqlite3.bind stmt 6
                       (match t.note with
                       | Some n -> Sqlite3.Data.TEXT n
                       | None -> Sqlite3.Data.NULL)
                      : Sqlite3.Rc.t);
                  ignore
                    (Sqlite3.bind stmt 7
                       (Sqlite3.Data.INT (Int64.of_int t.sort_order))
                      : Sqlite3.Rc.t);
                  ignore
                    (Sqlite3.bind stmt 8 (Sqlite3.Data.INT (Int64.of_int group))
                      : Sqlite3.Rc.t);
                  ignore (Sqlite3.step stmt : Sqlite3.Rc.t));
              ignore (soft_delete_task ~db ~session_key ~id:sid)
          | None -> ())
        subtree_ids;
      Ok (List.length subtree_ids)
    end
  in
  match id with
  | Some root_id -> (
      let root_id = resolve_existing_id ~tasks ~id:root_id in
      match List.find_opt (fun t -> t.id = root_id) tasks with
      | None -> Error (not_found_error ~tasks ~id:root_id)
      | Some _ -> archive_subtree root_id)
  | None ->
      (* Archive all fully-completed root trees *)
      let roots = List.filter (fun t -> t.parent_id = None) tasks in
      let completed_roots =
        List.filter
          (fun root ->
            let subtree_ids = get_subtree_ids ~tasks ~id:root.id in
            List.for_all
              (fun sid ->
                match List.find_opt (fun t -> t.id = sid) tasks with
                | Some t -> is_terminal t.status
                | None -> true)
              subtree_ids)
          roots
      in
      if completed_roots = [] then
        Error "No fully completed root trees to archive"
      else begin
        let total = ref 0 in
        List.iter
          (fun root ->
            match archive_subtree root.id with
            | Ok n -> total := !total + n
            | Error _ -> ())
          completed_roots;
        Ok !total
      end

(* Restore a soft-deleted task and its soft-deleted descendants *)
let do_restore ~db ~session_key ~id =
  let all_tasks = load_tasks ~include_deleted:true ~db ~session_key () in
  let id = resolve_existing_id ~tasks:all_tasks ~id in
  match List.find_opt (fun t -> t.id = id) all_tasks with
  | None ->
      Error
        (Printf.sprintf
           "Task '%s' not found (including deleted). Check the ID or use \
            op=list include_deleted=true to see deleted tasks."
           id)
  | Some task ->
      if task.deleted_at = None then
        Error
          (Printf.sprintf
             "Task '%s' is not deleted — nothing to restore. Use op=update to \
              change its status."
             id)
      else begin
        let subtree_ids = get_subtree_ids ~tasks:all_tasks ~id in
        let deleted_ids =
          List.filter
            (fun sid ->
              match List.find_opt (fun t -> t.id = sid) all_tasks with
              | Some t -> t.deleted_at <> None
              | None -> false)
            subtree_ids
        in
        let sql =
          "UPDATE task_tree SET deleted_at = NULL, updated_at = \
           datetime('now') WHERE session_key = ? AND id = ?"
        in
        List.iter
          (fun sid ->
            let stmt = Sqlite3.prepare db sql in
            Fun.protect
              ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
              (fun () ->
                ignore
                  (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key)
                    : Sqlite3.Rc.t);
                ignore
                  (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT sid) : Sqlite3.Rc.t);
                ignore (Sqlite3.step stmt : Sqlite3.Rc.t)))
          deleted_ids;
        Ok (List.length deleted_ids)
      end

(* Hard-purge soft-deleted rows older than configured threshold *)

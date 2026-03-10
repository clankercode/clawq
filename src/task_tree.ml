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
  sort_order : int;
  deleted_at : string option;
}

let max_active_tasks = 50
let max_depth = 5
let warn_concurrent_in_progress = 5
let max_batch_size = 20
let max_title_length = 200

let init_schema db =
  Memory.exec_exn db
    "CREATE TABLE IF NOT EXISTS task_tree (\n\
    \  id TEXT NOT NULL,\n\
    \  session_key TEXT NOT NULL,\n\
    \  parent_id TEXT,\n\
    \  title TEXT NOT NULL,\n\
    \  status TEXT NOT NULL DEFAULT 'pending',\n\
    \  note TEXT,\n\
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
     task_tree_archive (session_key)"

let load_tasks ?(include_deleted = false) ~db ~session_key () =
  let deleted_filter =
    if include_deleted then "" else " AND deleted_at IS NULL"
  in
  let sql =
    Printf.sprintf
      "SELECT id, session_key, parent_id, title, status, note, sort_order, \
       deleted_at FROM task_tree WHERE session_key = ?%s ORDER BY sort_order \
       ASC, id ASC"
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
          match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let sk =
          match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let parent_id =
          match Sqlite3.column stmt 2 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let title =
          match Sqlite3.column stmt 3 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let status =
          match Sqlite3.column stmt 4 with
          | Sqlite3.Data.TEXT s -> (
              match status_of_string s with Some st -> st | None -> Pending)
          | _ -> Pending
        in
        let note =
          match Sqlite3.column stmt 5 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let sort_order =
          match Sqlite3.column stmt 6 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0
        in
        let deleted_at =
          match Sqlite3.column stmt 7 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        results :=
          {
            id;
            session_key = sk;
            parent_id;
            title;
            status;
            note;
            sort_order;
            deleted_at;
          }
          :: !results
      done;
      List.rev !results)

let count_tasks ~db ~session_key =
  Memory.query_single_int db
    (Printf.sprintf
       "SELECT COUNT(*) FROM task_tree WHERE session_key = '%s' AND deleted_at \
        IS NULL"
       (String.concat "''" (String.split_on_char '\'' session_key)))

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
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> string_of_int (Int64.to_int n + 1)
          | _ -> "1")
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

let rec get_subtree_ids ~tasks ~id =
  let children = get_children ~tasks ~id in
  id :: List.concat_map (fun c -> get_subtree_ids ~tasks ~id:c.id) children

let count_in_progress ~db ~session_key =
  Memory.query_single_int db
    (Printf.sprintf
       "SELECT COUNT(*) FROM task_tree WHERE session_key = '%s' AND status = \
        'in_progress' AND deleted_at IS NULL"
       (String.concat "''" (String.split_on_char '\'' session_key)))

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
  Memory.query_single_int db
    (Printf.sprintf
       "SELECT COALESCE(MAX(sort_order), 0) + 1 FROM task_tree WHERE \
        session_key = '%s'"
       (String.concat "''" (String.split_on_char '\'' session_key)))

let insert_task ~db ~session_key ~id ~parent_id ~title ~status ~note =
  let sort_order = next_sort_order ~db ~session_key in
  let sql =
    "INSERT INTO task_tree (id, session_key, parent_id, title, status, note, \
     sort_order) VALUES (?, ?, ?, ?, ?, ?, ?)"
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
        (Sqlite3.bind stmt 7 (Sqlite3.Data.INT (Int64.of_int sort_order))
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

let render_tree ~db ~session_key =
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
      let n = ref 0 in
      List.iter
        (fun t ->
          incr n;
          let number =
            match prefix with
            | "" -> string_of_int !n
            | p -> p ^ "." ^ string_of_int !n
          in
          let note_str =
            match t.note with Some n -> " (" ^ n ^ ")" | None -> ""
          in
          Buffer.add_string buf
            (Printf.sprintf "%s. %s %s%s\n" number (status_icon t.status)
               t.title note_str);
          render_children ~parent_id:(Some t.id) ~prefix:number)
        children
    in
    render_children ~parent_id:None ~prefix:"";
    let result = Buffer.contents buf in
    if String.length result > 0 && result.[String.length result - 1] = '\n' then
      String.sub result 0 (String.length result - 1)
    else result

let render_tree_with_legend ~db ~session_key =
  let tree = render_tree ~db ~session_key in
  if count_tasks ~db ~session_key = 0 then tree
  else
    let ip_count = count_in_progress ~db ~session_key in
    let warning =
      if ip_count >= warn_concurrent_in_progress then
        Printf.sprintf
          "\n\n\
           \xe2\x9a\xa0\xef\xb8\x8f WARNING: %d tasks are in_progress. \
           Consider completing or updating some before starting more work."
          ip_count
      else ""
    in
    tree
    ^ "\n\n\
       Legend: [ ] pending  [>] in_progress  [x] done  [!] error  [-] cancelled"
    ^ warning

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
            (Printf.sprintf "\n  [>] #%s — %s%s" t.id t.title note_str))
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
            (Printf.sprintf "\n  [!] #%s — %s%s" t.id t.title note_str))
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
          Buffer.add_string buf (Printf.sprintf "\n  [ ] #%s — %s" t.id t.title))
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

let format_notification ~connector ~db ~session_key (ops : Yojson.Safe.t list) =
  let open Yojson.Safe.Util in
  let lines = Buffer.create 128 in
  let meaningful = ref false in
  List.iter
    (fun op_json ->
      let op = try op_json |> member "op" |> to_string with _ -> "" in
      match op with
      | "add" ->
          meaningful := true;
          let title =
            try op_json |> member "title" |> to_string with _ -> "?"
          in
          let status =
            try op_json |> member "status" |> to_string with _ -> "pending"
          in
          Buffer.add_string lines
            (Printf.sprintf "+ %s [%s]\n"
               (Format_adapter.bold connector title)
               status)
      | "update" ->
          meaningful := true;
          let id = try op_json |> member "id" |> to_string with _ -> "?" in
          let status =
            try Some (op_json |> member "status" |> to_string) with _ -> None
          in
          let note =
            try Some (op_json |> member "note" |> to_string) with _ -> None
          in
          (* TODO: show recursive count in notification when recursive=true *)
          let detail =
            match (status, note) with
            | Some s, Some n ->
                Printf.sprintf " -> %s (%s)" (Format_adapter.code connector s) n
            | Some s, None ->
                Printf.sprintf " -> %s" (Format_adapter.code connector s)
            | None, Some n -> Printf.sprintf " note: %s" n
            | None, None -> ""
          in
          Buffer.add_string lines
            (Printf.sprintf "~ %s%s\n"
               (Format_adapter.code connector ("#" ^ id))
               detail)
      | "remove" ->
          meaningful := true;
          let id = try op_json |> member "id" |> to_string with _ -> "?" in
          Buffer.add_string lines
            (Printf.sprintf "- Soft-deleted %s\n"
               (Format_adapter.code connector ("#" ^ id)))
      | "clear" ->
          meaningful := true;
          Buffer.add_string lines "Soft-deleted completed tasks\n"
      | "archive" -> (
          meaningful := true;
          let id =
            try Some (op_json |> member "id" |> to_string) with _ -> None
          in
          match id with
          | Some id ->
              Buffer.add_string lines
                (Printf.sprintf "Archived %s\n"
                   (Format_adapter.code connector ("#" ^ id)))
          | None -> Buffer.add_string lines "Archived completed trees\n")
      | "restore" ->
          meaningful := true;
          let id = try op_json |> member "id" |> to_string with _ -> "?" in
          Buffer.add_string lines
            (Printf.sprintf "Restored %s\n"
               (Format_adapter.code connector ("#" ^ id)))
      | "reorder" | _ -> ())
    ops;
  if not !meaningful then None
  else begin
    let tasks = load_tasks ~db ~session_key () in
    let in_progress = List.filter (fun t -> t.status = In_progress) tasks in
    (match in_progress with
    | [ t ] ->
        Buffer.add_string lines
          (Printf.sprintf "Focus: %s %s\n"
             (Format_adapter.code connector ("#" ^ t.id))
             t.title)
    | _ :: _ ->
        let ids =
          String.concat ", "
            (List.map
               (fun t -> Format_adapter.code connector ("#" ^ t.id))
               in_progress)
        in
        Buffer.add_string lines (Printf.sprintf "Active: %s\n" ids)
    | [] -> ());
    let content = Buffer.contents lines in
    let header = Format_adapter.bold connector "Task tree updated" ^ "\n" in
    Some (header ^ content)
  end

(* Validate and execute add operation *)
let do_add ~db ~session_key ~id ~parent_id ~title ~status ~note =
  if String.length title > max_title_length then
    Error
      (Printf.sprintf "Title too long (%d chars, max %d)" (String.length title)
         max_title_length)
  else if String.length title = 0 then
    Error "Title is required for add. Provide a 'title' field."
  else if count_tasks ~db ~session_key >= max_active_tasks then
    Error
      (Printf.sprintf "Too many active tasks (max %d). Archive or clear first."
         max_active_tasks)
  else begin
    let actual_id =
      match id with Some i -> i | None -> next_auto_id ~db ~session_key
    in
    if id_exists ~db ~session_key ~id:actual_id then
      Error
        (Printf.sprintf
           "Task ID '%s' already exists. Choose a different 'id' or omit it \
            for auto-assignment."
           actual_id)
    else
      let tasks = load_tasks ~db ~session_key () in
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
                building (depth 0 = root), or set 'parent' to an existing task \
                ID. Omit both for a root task."
               (Option.get parent_id))
        else begin
          let actual_status =
            match status with Some s -> s | None -> Pending
          in
          match
            insert_task ~db ~session_key ~id:actual_id ~parent_id ~title
              ~status:actual_status ~note
          with
          | Ok () -> Ok actual_id
          | Error e -> Error e
        end
  end

(* Validate and execute update operation *)
let do_update ~db ~session_key ~id ~status ~note =
  let tasks = load_tasks ~db ~session_key () in
  match List.find_opt (fun t -> t.id = id) tasks with
  | None ->
      Error
        (Printf.sprintf
           "Task '%s' not found. Check the ID against the current task tree — \
            IDs are case-sensitive."
           id)
  | Some task -> (
      match (status, note) with
      | None, None -> Error "Update requires at least status or note"
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
                      String.concat ", "
                        (List.map (fun c -> "#" ^ c.id) incomplete)
                    in
                    result :=
                      Error
                        (Printf.sprintf
                           "Cannot mark #%s done — children still incomplete: \
                            %s"
                           id child_ids)
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
          !result)

(* Validate and execute remove operation — soft-deletes instead of hard-deletes *)
let do_remove ~db ~session_key ~id ?(recursive = false) () =
  let tasks = load_tasks ~db ~session_key () in
  match List.find_opt (fun t -> t.id = id) tasks with
  | None ->
      Error
        (Printf.sprintf
           "Task '%s' not found. Check the ID against the current task tree — \
            IDs are case-sensitive."
           id)
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
               "Cannot remove #%s — subtree contains in_progress tasks. Use \
                recursive=true to force-remove the entire subtree."
               id)
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
  let escaped_key =
    String.concat "''" (String.split_on_char '\'' session_key)
  in
  Memory.exec_exn db
    (Printf.sprintf
       "UPDATE task_tree SET deleted_at = datetime('now'), updated_at = \
        datetime('now') WHERE session_key = '%s' AND status IN ('done', \
        'cancelled') AND deleted_at IS NULL"
       escaped_key);
  Ok (Sqlite3.changes db)

(* Archive completed subtrees *)
let do_archive ~db ~session_key ~id =
  let tasks = load_tasks ~db ~session_key () in
  let next_archive_group () =
    Memory.query_single_int db
      (Printf.sprintf
         "SELECT COALESCE(MAX(archive_group), 0) + 1 FROM task_tree_archive \
          WHERE session_key = '%s'"
         (String.concat "''" (String.split_on_char '\'' session_key)))
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
           "Cannot archive #%s — subtree contains non-terminal tasks" root_id)
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
      match List.find_opt (fun t -> t.id = root_id) tasks with
      | None ->
          Error
            (Printf.sprintf
               "Task '%s' not found. Check the ID against the current task \
                tree — IDs are case-sensitive."
               root_id)
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
let maybe_purge_deleted_tasks ~db ~config =
  let days = config.Runtime_config.memory.task_tree_purge_after_days in
  if days > 0 then
    Memory.exec_exn db
      (Printf.sprintf
         "DELETE FROM task_tree WHERE deleted_at IS NOT NULL AND \
          datetime(deleted_at, '+%d days') < datetime('now')"
         days)

(* Reorder a task among its siblings *)
let do_reorder ~db ~session_key ~id ~position =
  let tasks = load_tasks ~db ~session_key () in
  match List.find_opt (fun t -> t.id = id) tasks with
  | None ->
      Error
        (Printf.sprintf
           "Task '%s' not found. Check the ID against the current task tree — \
            IDs are case-sensitive."
           id)
  | Some task -> (
      let siblings =
        List.filter (fun t -> t.parent_id = task.parent_id) tasks
        |> List.sort (fun a b ->
            let c = compare a.sort_order b.sort_order in
            if c <> 0 then c else compare a.id b.id)
      in
      if List.length siblings <= 1 then Error "No siblings to reorder among"
      else
        let parse_position pos =
          if pos = "first" then Ok `First
          else if pos = "last" then Ok `Last
          else if String.length pos > 7 && String.sub pos 0 7 = "before:" then
            Ok (`Before (String.sub pos 7 (String.length pos - 7)))
          else if String.length pos > 6 && String.sub pos 0 6 = "after:" then
            Ok (`After (String.sub pos 6 (String.length pos - 6)))
          else
            Error
              (Printf.sprintf
                 "Invalid position '%s'. Use 'first', 'last', 'before:<id>', \
                  or 'after:<id>'"
                 pos)
        in
        match parse_position position with
        | Error e -> Error e
        | Ok parsed -> (
            let validate_ref ref_id =
              match List.find_opt (fun t -> t.id = ref_id) siblings with
              | None ->
                  Error
                    (Printf.sprintf
                       "Reference task '%s' not found among siblings" ref_id)
              | Some _ ->
                  if ref_id = id then
                    Error "Cannot reorder a task relative to itself"
                  else Ok ()
            in
            let ref_valid =
              match parsed with
              | `Before ref_id | `After ref_id -> validate_ref ref_id
              | `First | `Last -> Ok ()
            in
            match ref_valid with
            | Error e -> Error e
            | Ok () -> (
                let others = List.filter (fun t -> t.id <> id) siblings in
                let new_order =
                  match parsed with
                  | `First -> task :: others
                  | `Last -> others @ [ task ]
                  | `Before ref_id ->
                      List.concat_map
                        (fun t -> if t.id = ref_id then [ task; t ] else [ t ])
                        others
                  | `After ref_id ->
                      List.concat_map
                        (fun t -> if t.id = ref_id then [ t; task ] else [ t ])
                        others
                in
                let err = ref None in
                List.iteri
                  (fun i t ->
                    if !err = None then
                      match
                        update_sort_order ~db ~session_key ~id:t.id
                          ~sort_order:(i + 1)
                      with
                      | Ok () -> ()
                      | Error e -> err := Some e)
                  new_order;
                match !err with None -> Ok () | Some e -> Error e)))

(* Template infrastructure *)
let _templates_dir_override = ref None
let set_templates_dir d = _templates_dir_override := Some d

let templates_dir () =
  let dir =
    match !_templates_dir_override with
    | Some d -> d
    | None ->
        let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
        Filename.concat (Filename.concat home ".clawq") "task_templates"
  in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error _ -> ());
  dir

let is_valid_template_name name =
  name <> ""
  && String.length name <= 64
  &&
  let ok = ref true in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> ()
      | _ -> ok := false)
    name;
  !ok

let substitute_vars (vars : Yojson.Safe.t) text =
  let open Yojson.Safe.Util in
  let pairs = try to_assoc vars with _ -> [] in
  List.fold_left
    (fun acc (key, value) ->
      let v = try to_string value with _ -> Yojson.Safe.to_string value in
      let pattern = "{{" ^ key ^ "}}" in
      let plen = String.length pattern in
      let alen = String.length acc in
      if plen = 0 || alen < plen then acc
      else
        let buf = Buffer.create alen in
        let i = ref 0 in
        while !i <= alen - plen do
          if String.sub acc !i plen = pattern then begin
            Buffer.add_string buf v;
            i := !i + plen
          end
          else begin
            Buffer.add_char buf acc.[!i];
            incr i
          end
        done;
        while !i < alen do
          Buffer.add_char buf acc.[!i];
          incr i
        done;
        Buffer.contents buf)
    text pairs

let load_template name =
  let path = Filename.concat (templates_dir ()) (name ^ ".json") in
  if not (Sys.file_exists path) then
    Error
      (Printf.sprintf
         "Template '%s' not found. Use 'list_templates' to see available \
          templates, or use inline 'tasks' instead."
         name)
  else
    try
      let ic = open_in path in
      let content =
        Fun.protect
          ~finally:(fun () -> close_in ic)
          (fun () ->
            let n = in_channel_length ic in
            really_input_string ic n)
      in
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      let description =
        try Some (json |> member "description" |> to_string) with _ -> None
      in
      let tasks = try json |> member "tasks" |> to_list with _ -> [] in
      if tasks = [] then
        Error (Printf.sprintf "Template '%s' has no tasks defined." name)
      else Ok (description, tasks)
    with
    | Yojson.Json_error msg ->
        Error (Printf.sprintf "Template '%s' has invalid JSON: %s" name msg)
    | exn ->
        Error
          (Printf.sprintf "Failed to load template '%s': %s" name
             (Printexc.to_string exn))

let save_template_to_disk ~name ~description ~tasks =
  if not (is_valid_template_name name) then
    Error
      "Template name must contain only alphanumeric characters, underscores, \
       and hyphens (max 64 chars)."
  else
    let open Yojson.Safe.Util in
    let err = ref None in
    let count = ref 0 in
    List.iter
      (fun task_json ->
        if !err = None then begin
          let title =
            try Some (task_json |> member "title" |> to_string) with _ -> None
          in
          let depth =
            try Some (task_json |> member "depth" |> to_int) with _ -> None
          in
          (match title with
          | None | Some "" ->
              err := Some "Each task must have a non-empty 'title' field."
          | _ -> ());
          (match !err with
          | Some _ -> ()
          | None -> (
              match depth with
              | None ->
                  err := Some "Each task must have a 'depth' field (integer)."
              | _ -> ()));
          (match !err with
          | Some _ -> ()
          | None -> (
              let status_str =
                try Some (task_json |> member "status" |> to_string)
                with _ -> None
              in
              match status_str with
              | Some s when status_of_string s = None ->
                  err :=
                    Some
                      (Printf.sprintf
                         "Invalid status '%s'. Valid statuses: pending, \
                          in_progress, done, error, cancelled."
                         s)
              | _ -> ()));
          incr count
        end)
      tasks;
    match !err with
    | Some e -> Error e
    | None ->
        if !count = 0 then Error "Template must contain at least one task."
        else begin
          let dir = templates_dir () in
          let path = Filename.concat dir (name ^ ".json") in
          let json_obj =
            `Assoc
              ([ ("name", `String name) ]
              @ (match description with
                | Some d -> [ ("description", `String d) ]
                | None -> [])
              @ [ ("tasks", `List tasks) ])
          in
          try
            let oc = open_out path in
            Fun.protect
              ~finally:(fun () -> close_out oc)
              (fun () ->
                output_string oc (Yojson.Safe.pretty_to_string json_obj));
            Ok !count
          with exn ->
            Error
              (Printf.sprintf "Failed to save template '%s': %s" name
                 (Printexc.to_string exn))
        end

let list_saved_templates () =
  let dir = templates_dir () in
  let files = try Sys.readdir dir |> Array.to_list with _ -> [] in
  let templates =
    List.filter_map
      (fun f ->
        if Filename.check_suffix f ".json" then
          let name = Filename.chop_suffix f ".json" in
          let desc =
            try
              let path = Filename.concat dir f in
              let ic = open_in path in
              let content =
                Fun.protect
                  ~finally:(fun () -> close_in ic)
                  (fun () ->
                    let n = in_channel_length ic in
                    really_input_string ic n)
              in
              let json = Yojson.Safe.from_string content in
              let open Yojson.Safe.Util in
              try Some (json |> member "description" |> to_string)
              with _ -> None
            with _ -> None
          in
          Some (name, desc)
        else None)
      files
  in
  List.sort (fun (a, _) (b, _) -> String.compare a b) templates

let delete_template_from_disk name =
  if not (is_valid_template_name name) then
    Error
      "Template name must contain only alphanumeric characters, underscores, \
       and hyphens (max 64 chars)."
  else
    let path = Filename.concat (templates_dir ()) (name ^ ".json") in
    if not (Sys.file_exists path) then
      Error
        (Printf.sprintf
           "Template '%s' not found. Use 'list_templates' to see available \
            templates."
           name)
    else begin
      try
        Sys.remove path;
        Ok ()
      with exn ->
        Error
          (Printf.sprintf "Failed to delete template '%s': %s" name
             (Printexc.to_string exn))
    end

let expand_seeds (ops : Yojson.Safe.t list) =
  let open Yojson.Safe.Util in
  let task_to_add_op vars task_json =
    let title = try task_json |> member "title" |> to_string with _ -> "" in
    let title = substitute_vars vars title in
    let note =
      try Some (task_json |> member "note" |> to_string) with _ -> None
    in
    let depth =
      try Some (task_json |> member "depth" |> to_int) with _ -> None
    in
    let status =
      try Some (task_json |> member "status" |> to_string) with _ -> None
    in
    let id =
      try Some (task_json |> member "id" |> to_string) with _ -> None
    in
    `Assoc
      ([ ("op", `String "add"); ("title", `String title) ]
      @ (match depth with Some d -> [ ("depth", `Int d) ] | None -> [])
      @ (match status with Some s -> [ ("status", `String s) ] | None -> [])
      @ (match note with
        | Some n -> [ ("note", `String (substitute_vars vars n)) ]
        | None -> [])
      @ match id with Some i -> [ ("id", `String i) ] | None -> [])
  in
  let result = ref [] in
  let err = ref None in
  List.iter
    (fun op_json ->
      if !err = None then begin
        let op_name = try op_json |> member "op" |> to_string with _ -> "" in
        if op_name = "seed" then begin
          let template_name =
            try Some (op_json |> member "template" |> to_string)
            with _ -> None
          in
          let inline_tasks =
            try Some (op_json |> member "tasks" |> to_list) with _ -> None
          in
          let vars =
            try
              let v = op_json |> member "vars" in
              if v = `Null then `Assoc [] else v
            with _ -> `Assoc []
          in
          match (template_name, inline_tasks) with
          | None, None ->
              err :=
                Some
                  "Seed requires either 'template' (name of a saved template) \
                   or 'tasks' (inline array of task definitions). Provide \
                   exactly one."
          | Some _, Some _ ->
              err :=
                Some
                  "Seed must have either 'template' or 'tasks', not both. Use \
                   'template' for saved templates, 'tasks' for inline \
                   definitions."
          | Some name, None -> (
              match load_template name with
              | Error e -> err := Some e
              | Ok (_desc, task_defs) ->
                  result :=
                    List.rev_append
                      (List.map (task_to_add_op vars) task_defs)
                      !result)
          | None, Some tasks ->
              if tasks = [] then
                err :=
                  Some
                    "Seed 'tasks' array must contain at least one task \
                     definition."
              else
                result :=
                  List.rev_append (List.map (task_to_add_op vars) tasks) !result
        end
        else result := op_json :: !result
      end)
    ops;
  match !err with Some e -> Error e | None -> Ok (List.rev !result)

(* Process a batch of operations *)
let process_operations ~db ~session_key (ops : Yojson.Safe.t list) =
  let open Yojson.Safe.Util in
  match expand_seeds ops with
  | Error e -> Error e
  | Ok ops ->
      let n = List.length ops in
      if n = 0 then Error "No operations provided"
      else if n > max_batch_size then
        Error
          (Printf.sprintf "Too many operations (%d, max %d)" n max_batch_size)
      else begin
        (* Wrap in transaction *)
        Memory.exec_exn db "BEGIN IMMEDIATE";
        let depth_stack : (int * string) list ref = ref [] in
        let results = Buffer.create 256 in
        let error = ref None in
        let op_idx = ref 0 in
        List.iter
          (fun op_json ->
            if !error = None then begin
              incr op_idx;
              let op_name =
                try op_json |> member "op" |> to_string with _ -> ""
              in
              let result =
                try
                  match op_name with
                  | "add" -> (
                      let title =
                        try Some (op_json |> member "title" |> to_string)
                        with _ -> None
                      in
                      let custom_id =
                        try Some (op_json |> member "id" |> to_string)
                        with _ -> None
                      in
                      let explicit_parent =
                        try
                          let p = op_json |> member "parent" |> to_string in
                          let trimmed = String.trim p in
                          if String.length trimmed = 0 then None
                          else Some trimmed
                        with _ -> None
                      in
                      let depth =
                        try Some (op_json |> member "depth" |> to_int)
                        with _ -> None
                      in
                      let status =
                        try
                          Some
                            ( op_json |> member "status" |> to_string |> fun s ->
                              match status_of_string s with
                              | Some st -> st
                              | None -> Pending )
                        with _ -> None
                      in
                      let note =
                        try Some (op_json |> member "note" |> to_string)
                        with _ -> None
                      in
                      match title with
                      | None ->
                          Error
                            "Title is required for add. Provide a 'title' \
                             field."
                      | Some title -> (
                          let parent_id =
                            match depth with
                            | Some 0 -> None
                            | Some d when d > 0 ->
                                let max_depth_in_stack =
                                  match !depth_stack with
                                  | [] -> -1
                                  | _ ->
                                      List.fold_left
                                        (fun acc (lvl, _) -> max acc lvl)
                                        (-1) !depth_stack
                                in
                                if d > max_depth_in_stack + 1 + 1 then
                                  failwith
                                    (Printf.sprintf
                                       "depth %d skips levels — use depth %d \
                                        first or set parent explicitly"
                                       d
                                       (max_depth_in_stack + 1 + 1))
                                else
                                  let parent =
                                    List.find_opt
                                      (fun (lvl, _) -> lvl = d - 1)
                                      !depth_stack
                                  in
                                  Option.map snd parent
                            | _ -> (
                                match explicit_parent with
                                | Some p -> Some p
                                | None -> None)
                          in
                          let add_result =
                            do_add ~db ~session_key ~id:custom_id ~parent_id
                              ~title ~status ~note
                          in
                          match add_result with
                          | Ok actual_id ->
                              let d =
                                match depth with
                                | Some d -> d
                                | None ->
                                    if explicit_parent = None then 0
                                    else
                                      let tasks =
                                        load_tasks ~db ~session_key ()
                                      in
                                      task_depth ~tasks ~id:actual_id
                              in
                              (* Update depth stack: truncate to depth d, push new *)
                              depth_stack :=
                                (d, actual_id)
                                :: List.filter
                                     (fun (lvl, _) -> lvl < d)
                                     !depth_stack;
                              let parent_note =
                                match parent_id with
                                | Some pid ->
                                    Printf.sprintf " (child of %s)" pid
                                | None -> ""
                              in
                              Buffer.add_string results
                                (Printf.sprintf "Added %s: %s [%s]%s\n"
                                   actual_id title
                                   (string_of_status
                                      (match status with
                                      | Some s -> s
                                      | None -> Pending))
                                   parent_note);
                              Ok ()
                          | Error e -> Error e))
                  | "update" -> (
                      let id_str =
                        try Some (op_json |> member "id" |> to_string)
                        with _ -> None
                      in
                      let status =
                        try
                          Some
                            ( op_json |> member "status" |> to_string |> fun s ->
                              match status_of_string s with
                              | Some st -> st
                              | None ->
                                  failwith
                                    (Printf.sprintf "Invalid status '%s'" s) )
                        with
                        | Failure msg -> failwith msg
                        | _ -> None
                      in
                      let note =
                        try Some (op_json |> member "note" |> to_string)
                        with _ -> None
                      in
                      let recursive =
                        try op_json |> member "recursive" |> to_bool
                        with _ -> false
                      in
                      match id_str with
                      | None ->
                          Error
                            "ID is required for update. Provide an 'id' field \
                             with an existing task ID."
                      | Some id_str ->
                          let ids =
                            String.split_on_char ',' id_str
                            |> List.map String.trim
                            |> List.filter (fun s -> s <> "")
                          in
                          if recursive then begin
                            match status with
                            | None ->
                                Error
                                  "recursive=true requires a 'status' field. \
                                   Supported statuses: done, cancelled."
                            | Some new_status
                              when new_status <> Done && new_status <> Cancelled
                              ->
                                Error
                                  (Printf.sprintf
                                     "recursive=true is only supported for \
                                      status=done or status=cancelled, not \
                                      '%s'. To set other statuses, use \
                                      recursive=false or omit it."
                                     (string_of_status new_status))
                            | Some new_status -> (
                                let tasks = load_tasks ~db ~session_key () in
                                let all_ids =
                                  List.concat_map
                                    (fun single_id ->
                                      get_subtree_ids ~tasks ~id:single_id)
                                    ids
                                in
                                let err = ref None in
                                List.iter
                                  (fun sid ->
                                    if !err = None then
                                      match
                                        update_task_status ~db ~session_key
                                          ~id:sid ~status:new_status
                                      with
                                      | Ok () -> ()
                                      | Error e -> err := Some e)
                                  all_ids;
                                match !err with
                                | Some e -> Error e
                                | None ->
                                    let total = List.length all_ids in
                                    Buffer.add_string results
                                      (Printf.sprintf
                                         "Updated %d task(s) recursively \
                                          (status=%s)\n"
                                         total
                                         (string_of_status new_status));
                                    Ok ())
                          end
                          else begin
                            let err = ref None in
                            List.iter
                              (fun single_id ->
                                if !err = None then
                                  match
                                    do_update ~db ~session_key ~id:single_id
                                      ~status ~note
                                  with
                                  | Ok () ->
                                      let parts = ref [] in
                                      (match status with
                                      | Some s ->
                                          parts :=
                                            Printf.sprintf "status=%s"
                                              (string_of_status s)
                                            :: !parts
                                      | None -> ());
                                      (match note with
                                      | Some n ->
                                          parts :=
                                            Printf.sprintf "note=%s" n :: !parts
                                      | None -> ());
                                      Buffer.add_string results
                                        (Printf.sprintf "Updated #%s: %s\n"
                                           single_id
                                           (String.concat ", " (List.rev !parts)))
                                  | Error e -> err := Some e)
                              ids;
                            match !err with Some e -> Error e | None -> Ok ()
                          end)
                  | "remove" -> (
                      let id =
                        try Some (op_json |> member "id" |> to_string)
                        with _ -> None
                      in
                      let recursive =
                        try op_json |> member "recursive" |> to_bool
                        with _ -> false
                      in
                      match id with
                      | None ->
                          Error
                            "ID is required for remove. Provide an 'id' field \
                             with an existing task ID."
                      | Some id -> (
                          match
                            do_remove ~db ~session_key ~id ~recursive ()
                          with
                          | Ok count ->
                              Buffer.add_string results
                                (Printf.sprintf
                                   "Soft-deleted #%s (%d task(s)). Restore \
                                    with: op=restore id=%s\n"
                                   id count id);
                              Ok ()
                          | Error e -> Error e))
                  | "clear" -> (
                      match do_clear ~db ~session_key with
                      | Ok count ->
                          Buffer.add_string results
                            (Printf.sprintf
                               "Soft-deleted %d done/cancelled task(s). \
                                Restore individual tasks with: op=restore \
                                id=<id>. View deleted with: op=list \
                                include_deleted=true\n"
                               count);
                          Ok ()
                      | Error e -> Error e)
                  | "archive" -> (
                      let id =
                        try Some (op_json |> member "id" |> to_string)
                        with _ -> None
                      in
                      match do_archive ~db ~session_key ~id with
                      | Ok count ->
                          Buffer.add_string results
                            (match id with
                            | Some id ->
                                Printf.sprintf
                                  "Archived subtree #%s (%d task(s)). Restore \
                                   with: op=restore id=%s\n"
                                  id count id
                            | None ->
                                Printf.sprintf
                                  "Archived all completed root trees (%d \
                                   task(s)). View with: op=list \
                                   include_deleted=true\n"
                                  count);
                          Ok ()
                      | Error e -> Error e)
                  | "reorder" -> (
                      let id =
                        try Some (op_json |> member "id" |> to_string)
                        with _ -> None
                      in
                      let position =
                        try Some (op_json |> member "position" |> to_string)
                        with _ -> None
                      in
                      match (id, position) with
                      | None, _ ->
                          Error
                            "ID is required for reorder. Provide an 'id' field."
                      | _, None ->
                          Error
                            "position is required for reorder. Use 'first', \
                             'last', 'before:<id>', or 'after:<id>'."
                      | Some id, Some position -> (
                          match do_reorder ~db ~session_key ~id ~position with
                          | Ok () ->
                              Buffer.add_string results
                                (Printf.sprintf "Reordered #%s to %s\n" id
                                   position);
                              Ok ()
                          | Error e -> Error e))
                  | "save_template" -> (
                      let name =
                        try Some (op_json |> member "name" |> to_string)
                        with _ -> None
                      in
                      let description =
                        try Some (op_json |> member "description" |> to_string)
                        with _ -> None
                      in
                      let tasks =
                        try Some (op_json |> member "tasks" |> to_list)
                        with _ -> None
                      in
                      match (name, tasks) with
                      | None, _ ->
                          Error
                            "save_template requires a 'name' field with a \
                             valid template name (alphanumeric, underscores, \
                             hyphens, max 64 chars)."
                      | _, None ->
                          Error
                            "save_template requires a 'tasks' array with task \
                             definitions. Each task needs 'title' (string) and \
                             'depth' (integer)."
                      | Some name, Some tasks -> (
                          match
                            save_template_to_disk ~name ~description ~tasks
                          with
                          | Ok count ->
                              Buffer.add_string results
                                (Printf.sprintf
                                   "Saved template '%s' (%d tasks)\n" name count);
                              Ok ()
                          | Error e -> Error e))
                  | "list_templates" ->
                      let templates = list_saved_templates () in
                      if templates = [] then
                        Buffer.add_string results "No saved templates found.\n"
                      else begin
                        Buffer.add_string results "Available templates:\n";
                        List.iter
                          (fun (name, desc) ->
                            let desc_str =
                              match desc with
                              | Some d -> " \xe2\x80\x94 " ^ d
                              | None -> ""
                            in
                            Buffer.add_string results
                              (Printf.sprintf "  %s%s\n" name desc_str))
                          templates
                      end;
                      Ok ()
                  | "delete_template" -> (
                      let name =
                        try Some (op_json |> member "name" |> to_string)
                        with _ -> None
                      in
                      match name with
                      | None ->
                          Error
                            "delete_template requires a 'name' field with the \
                             template name to delete. Use 'list_templates' to \
                             see available templates."
                      | Some name -> (
                          match delete_template_from_disk name with
                          | Ok () ->
                              Buffer.add_string results
                                (Printf.sprintf "Deleted template '%s'\n" name);
                              Ok ()
                          | Error e -> Error e))
                  | "restore" -> (
                      let id =
                        try Some (op_json |> member "id" |> to_string)
                        with _ -> None
                      in
                      match id with
                      | None ->
                          Error
                            "ID is required for restore. Provide the 'id' of \
                             the soft-deleted task to recover. Use op=list \
                             include_deleted=true to find deleted task IDs."
                      | Some id -> (
                          match do_restore ~db ~session_key ~id with
                          | Ok count ->
                              Buffer.add_string results
                                (Printf.sprintf "Restored #%s (%d task(s))\n" id
                                   count);
                              Ok ()
                          | Error e -> Error e))
                  | "list" ->
                      let include_deleted =
                        try op_json |> member "include_deleted" |> to_bool
                        with _ -> false
                      in
                      let tasks =
                        load_tasks ~include_deleted ~db ~session_key ()
                      in
                      if tasks = [] then
                        Buffer.add_string results
                          (if include_deleted then
                             "No tasks found (including deleted).\n"
                           else
                             "No active tasks. Use op=list \
                              include_deleted=true to show deleted tasks.\n")
                      else begin
                        let buf = Buffer.create 256 in
                        let rec render_children ~parent_id ~prefix =
                          let children =
                            List.filter (fun t -> t.parent_id = parent_id) tasks
                            |> List.sort (fun a b ->
                                compare a.sort_order b.sort_order)
                          in
                          let n = ref 0 in
                          List.iter
                            (fun t ->
                              incr n;
                              let number =
                                match prefix with
                                | "" -> string_of_int !n
                                | p -> p ^ "." ^ string_of_int !n
                              in
                              let note_str =
                                match t.note with
                                | Some n -> " (" ^ n ^ ")"
                                | None -> ""
                              in
                              let del_str =
                                if t.deleted_at <> None then " [deleted]"
                                else ""
                              in
                              Buffer.add_string buf
                                (Printf.sprintf "%s. %s %s%s%s\n" number
                                   (status_icon t.status) t.title note_str
                                   del_str);
                              render_children ~parent_id:(Some t.id)
                                ~prefix:number)
                            children
                        in
                        render_children ~parent_id:None ~prefix:"";
                        Buffer.add_string results (Buffer.contents buf)
                      end;
                      Ok ()
                  | "" -> Error "Operation 'op' field is required"
                  | other ->
                      Error
                        (Printf.sprintf
                           "Unknown operation '%s'. Valid operations: add, \
                            update, remove, clear, archive, restore, list, \
                            reorder, seed, save_template, list_templates, \
                            delete_template."
                           other)
                with Failure msg -> Error msg
              in
              match result with
              | Ok () -> ()
              | Error msg ->
                  error :=
                    Some
                      (Printf.sprintf "Batch failed at operation %d/%d: %s"
                         !op_idx n msg)
            end)
          ops;
        match !error with
        | Some msg ->
            Memory.exec_exn db "ROLLBACK";
            Error (msg ^ ". No operations were applied.")
        | None ->
            Memory.exec_exn db "COMMIT";
            let summary = render_compact ~db ~session_key in
            let output = Buffer.contents results in
            let ip_count = count_in_progress ~db ~session_key in
            let warning =
              if ip_count >= warn_concurrent_in_progress then
                Printf.sprintf
                  "\n\n\
                   \xe2\x9a\xa0\xef\xb8\x8f WARNING: %d tasks are in_progress. \
                   Consider completing or updating some before starting more \
                   work."
                  ip_count
              else ""
            in
            Ok (output ^ "\n" ^ summary ^ warning)
      end

let tool ~db ?notify () : Tool.t =
  {
    name = "task_tree";
    description =
      "Persistent hierarchical task tree. Survives context compaction, visible \
       every turn.\n\n\
       Ops: add, update, remove, clear, archive, restore, list, reorder, seed, \
       save_template, list_templates, delete_template.\n\
       Statuses: pending, in_progress, done, error, cancelled.\n\n\
       Rules:\n\
       - Parent cannot be done until all children are done/cancelled.\n\
       - In-progress tasks cannot be removed without recursive=true.\n\
       - Setting in_progress promotes pending ancestors.\n\
       - remove/clear/archive soft-delete (recoverable via restore).\n\n\
       Bulk ops:\n\
       - update recursive=true status=done|cancelled: marks full subtree.\n\
       - remove recursive=true: force-removes entire subtree incl. in_progress.\n\
       - restore id=X: recovers soft-deleted task and all its deleted children.\n\
       - list include_deleted=true: shows all tasks including deleted ones.\n\n\
       Tips:\n\
       - Let IDs auto-assign (omit 'id' on add). Use 'depth' for tree building.\n\
       - Keep titles <60 chars; put details in 'note'.\n\
       - Batch ops in one call. Archive done work to keep tree small.\n\
       - 3-7 subtasks per parent. Mark in_progress/done as you go.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "operations",
                  `Assoc
                    [
                      ("type", `String "array");
                      ( "items",
                        `Assoc
                          [
                            ("type", `String "object");
                            ( "properties",
                              `Assoc
                                [
                                  ( "op",
                                    `Assoc
                                      [
                                        ("type", `String "string");
                                        ( "enum",
                                          `List
                                            [
                                              `String "add";
                                              `String "update";
                                              `String "remove";
                                              `String "clear";
                                              `String "archive";
                                              `String "restore";
                                              `String "list";
                                              `String "reorder";
                                              `String "seed";
                                              `String "save_template";
                                              `String "list_templates";
                                              `String "delete_template";
                                            ] );
                                      ] );
                                  ("id", `Assoc [ ("type", `String "string") ]);
                                  ( "parent",
                                    `Assoc [ ("type", `String "string") ] );
                                  ( "depth",
                                    `Assoc [ ("type", `String "integer") ] );
                                  ( "title",
                                    `Assoc [ ("type", `String "string") ] );
                                  ( "status",
                                    `Assoc
                                      [
                                        ("type", `String "string");
                                        ( "enum",
                                          `List
                                            [
                                              `String "pending";
                                              `String "in_progress";
                                              `String "done";
                                              `String "error";
                                              `String "cancelled";
                                            ] );
                                      ] );
                                  ("note", `Assoc [ ("type", `String "string") ]);
                                  ( "recursive",
                                    `Assoc [ ("type", `String "boolean") ] );
                                  ( "include_deleted",
                                    `Assoc [ ("type", `String "boolean") ] );
                                  ( "position",
                                    `Assoc [ ("type", `String "string") ] );
                                  ( "template",
                                    `Assoc [ ("type", `String "string") ] );
                                  ( "tasks",
                                    `Assoc
                                      [
                                        ("type", `String "array");
                                        ( "items",
                                          `Assoc
                                            [
                                              ("type", `String "object");
                                              ( "properties",
                                                `Assoc
                                                  [
                                                    ( "title",
                                                      `Assoc
                                                        [
                                                          ( "type",
                                                            `String "string" );
                                                        ] );
                                                    ( "depth",
                                                      `Assoc
                                                        [
                                                          ( "type",
                                                            `String "integer" );
                                                        ] );
                                                    ( "note",
                                                      `Assoc
                                                        [
                                                          ( "type",
                                                            `String "string" );
                                                        ] );
                                                  ] );
                                            ] );
                                      ] );
                                  ("vars", `Assoc [ ("type", `String "object") ]);
                                  ( "description",
                                    `Assoc [ ("type", `String "string") ] );
                                  ("name", `Assoc [ ("type", `String "string") ]);
                                ] );
                            ("required", `List [ `String "op" ]);
                          ] );
                    ] );
              ] );
          ("required", `List [ `String "operations" ]);
        ];
    invoke =
      (fun ?context args ->
        let session_key =
          match context with
          | Some ctx -> (
              match ctx.Tool.session_key with Some k -> k | None -> "default")
          | None -> "default"
        in
        let open Yojson.Safe.Util in
        let ops = try args |> member "operations" |> to_list with _ -> [] in
        match process_operations ~db ~session_key ops with
        | Ok result ->
            let expanded_ops =
              match expand_seeds ops with Ok o -> o | Error _ -> ops
            in
            (match notify with
            | Some lookup -> (
                match lookup session_key with
                | Some (connector, send) -> (
                    match
                      format_notification ~connector ~db ~session_key
                        expanded_ops
                    with
                    | Some text ->
                        Lwt.async (fun () ->
                            Lwt.catch
                              (fun () -> send text)
                              (fun exn ->
                                Logs.warn (fun m ->
                                    m
                                      "Failed to send task tree notification: \
                                       %s"
                                      (Printexc.to_string exn));
                                Lwt.return_unit))
                    | None -> ())
                | None -> ())
            | None -> ());
            Lwt.return result
        | Error msg -> Lwt.return ("Error: " ^ msg));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

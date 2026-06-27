open Task_tree_types
include Task_tree_db

(** Create a task-tree record for an [AsyncCommand] dispatched from a room. The
    caller provides [~title] directly (derived from
    {!Room_request_classifier.title_of_async_cmd} or elsewhere). Populates
    origin metadata ([origin_json], [thread_id], [requester], [profile_id]) from
    the room context.

    Returns [Ok task_id] if the record was created, [Error msg] on failure, or
    [Ok ""] if [~title] is empty. *)
let create_async_cmd_task ~db ~session_key ~title ~(origin : Room_origin.t)
    ?thread_id ?requester ?profile_id () =
  if title = "" then Ok ""
  else
    let origin_json =
      if Room_origin.is_empty origin then None
      else Some (Room_origin.to_compact_json_string origin)
    in
    let id = next_auto_id ~db ~session_key in
    match
      insert_task ~db ~session_key ~id ~parent_id:None ~title ~status:Pending
        ~note:None ~depends_on:[] ~agent_model:None ~agent_type:None
        ~agent_prompt:None ~agent_details:None ~autostart:false ?profile_id
        ?origin_json ?thread_id ?requester ()
    with
    | Ok () -> Ok id
    | Error e -> Error e

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
    ~agent_model ~agent_type ~agent_prompt ~agent_details ~autostart ?profile_id
    ?origin_json ?thread_id ?requester () =
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
                      ?profile_id ?origin_json ?thread_id ?requester ()
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
              | Pending when task.status = Task_error ->
                  (* Allow retry: Task_error -> Pending *)
                  ()
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

open Task_tree_types

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
              (match t.requester with
              | Some r when String.trim r <> "" ->
                  Some (Printf.sprintf "from=%s" r)
              | _ -> None);
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
  let tasks = Task_tree_db.load_tasks ~db ~session_key () in
  if tasks = [] then
    "No tasks tracked. Use the task_tree tool to plan and track your work.\n\
     Breaking complex goals into subtasks helps maintain focus across long \
     sessions."
  else render_task_tree tasks

let render_tree_with_legend ~db ~session_key =
  let tree = render_tree ~db ~session_key in
  if Task_tree_db.count_tasks ~db ~session_key = 0 then tree
  else
    let ip_count = Task_tree_db.count_in_progress ~db ~session_key in
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

let count_by_status tasks =
  let pending = ref 0
  and active = ref 0
  and done_ = ref 0
  and error = ref 0
  and cancelled = ref 0 in
  List.iter
    (fun t ->
      match t.status with
      | Pending -> incr pending
      | In_progress -> incr active
      | Done -> incr done_
      | Task_error -> incr error
      | Cancelled -> incr cancelled)
    tasks;
  (!pending, !active, !done_, !error, !cancelled)

let status_count_summary ?(active_label = "active") ?(done_label = "done")
    ?(error_label = "error") ?(cancelled_label = "cancelled") tasks =
  let pending, active, done_, error, cancelled = count_by_status tasks in
  List.filter_map
    (fun (n, label) ->
      if n > 0 then Some (Printf.sprintf "%d %s" n label) else None)
    [
      (pending, "pending");
      (active, active_label);
      (done_, done_label);
      (error, error_label);
      (cancelled, cancelled_label);
    ]

let render_emoji_tree ?(max_title_chars = 50) ~db ~session_key () =
  ignore max_title_chars;
  let tasks = Task_tree_db.load_tasks ~db ~session_key () in
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
    let total = List.length tasks in
    let counts = status_count_summary tasks in
    Buffer.add_string buf
      (Printf.sprintf "\n%d tasks \xc2\xb7 %s" total
         (String.concat " \xc2\xb7 " counts));
    let _, ip_count, _, _, _ = count_by_status tasks in
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
  let tasks = Task_tree_db.load_tasks ~db ~session_key () in
  if tasks = [] then
    "No tasks tracked. Use the task_tree tool to plan and track your work.\n\
     Breaking complex goals into subtasks helps maintain focus across long \
     sessions."
  else
    let buf = Buffer.create 256 in
    (* Count by status *)
    let n_pending, n_active, n_done, n_error, n_cancelled =
      count_by_status tasks
    in
    let total = List.length tasks in
    (* Summary line with non-zero counts *)
    let counts = status_count_summary tasks in
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
          let requester_str =
            match t.requester with
            | Some r when String.trim r <> "" -> " [" ^ r ^ "]"
            | _ -> ""
          in
          Buffer.add_string buf
            (Printf.sprintf "\n  [>] %s — %s%s%s" (display_id t.id) t.title
               note_str requester_str))
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
          let requester_str =
            match t.requester with
            | Some r when String.trim r <> "" -> " [" ^ r ^ "]"
            | _ -> ""
          in
          Buffer.add_string buf
            (Printf.sprintf "\n  [!] %s — %s%s%s" (display_id t.id) t.title
               note_str requester_str))
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
          let ancestors = Task_tree_db.get_ancestors ~tasks ~id:t.id in
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
    let n_archivable = n_done + n_cancelled in
    if n_archivable > 0 then
      Buffer.add_string buf
        (Printf.sprintf "\n(%d done — archive to save tokens)" n_archivable);
    Buffer.contents buf

let render_focus ~db ~session_key =
  let tasks = Task_tree_db.load_tasks ~db ~session_key () in
  if tasks = [] then
    "No tasks tracked. Use the task_tree tool to plan and track your work.\n\
     Breaking complex goals into subtasks helps maintain focus across long \
     sessions."
  else
    let buf = Buffer.create 256 in
    (* --- counts --- *)
    let n_pending, n_active, n_done, n_error, n_cancelled =
      count_by_status tasks
    in
    let total = List.length tasks in
    let counts = status_count_summary tasks in
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
          let requester_str =
            match t.requester with
            | Some r when String.trim r <> "" -> " [" ^ r ^ "]"
            | _ -> ""
          in
          let ancs = Task_tree_db.get_ancestors ~tasks ~id:t.id in
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
            (Printf.sprintf "\n  [>] %s — %s%s%s%s" (display_id t.id) t.title
               note_str requester_str path_str))
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
          let requester_str =
            match t.requester with
            | Some r when String.trim r <> "" -> " [" ^ r ^ "]"
            | _ -> ""
          in
          Buffer.add_string buf
            (Printf.sprintf "\n  [!] %s — %s%s%s" (display_id t.id) t.title
               note_str requester_str))
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
          let ancestors = Task_tree_db.get_ancestors ~tasks ~id:t.id in
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
    let n_archivable = n_done + n_cancelled in
    if n_archivable > 0 then
      Buffer.add_string buf
        (Printf.sprintf "\n(%d done — archive to save tokens)" n_archivable);
    Buffer.contents buf

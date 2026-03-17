include Task_tree_core

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
    | None -> Dot_dir.sub "task_templates"
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
      let ops =
        if ops = [] then [ `Assoc [ ("op", `String "list") ] ] else ops
      in
      let n = List.length ops in
      if n > max_batch_size then
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
       save_template, list_templates, delete_template. Omit 'operations' (or \
       pass an empty array) to default to 'list'.\n\
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
                                        ( "description",
                                          `String
                                            "Operation to perform (required)" );
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
          ("required", `List []);
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

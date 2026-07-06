include Task_tree_core

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

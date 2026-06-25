(* config_tree.ml — Render config as a tree(1)-style hierarchy.

   Shared by `clawq config tree` (CLI) and `/config tree` (slash command).
   Secrets are redacted by reusing Config_show.redact_json, so the same
   substring rules ("token", "secret", "api_key", ...) apply here. *)

let truncate_value ?(max = 60) s =
  if String.length s <= max then s else String.sub s 0 (max - 1) ^ "…"

let scalar_to_string = function
  | `String s -> s
  | `Int i -> string_of_int i
  | `Float f -> Printf.sprintf "%g" f
  | `Bool b -> string_of_bool b
  | `Null -> "null"
  | other -> Yojson.Safe.to_string other

(* Render [json] as a tree. [root_label] is the single line printed for the
   root; children hang below it with ├─ / └─ connectors. When [show_values] is
   false, leaf scalars print their key only (structure-only, like `tree`). *)
let render_json ?(show_values = true) ?(root_label = "config") json =
  let buf = Buffer.create 512 in
  Buffer.add_string buf root_label;
  Buffer.add_char buf '\n';
  let rec walk prefix node =
    let children =
      match node with
      | `Assoc fields -> fields
      | `List items -> List.mapi (fun i v -> (Printf.sprintf "[%d]" i, v)) items
      | _ -> []
    in
    let n = List.length children in
    List.iteri
      (fun i (k, v) ->
        let last = i = n - 1 in
        let connector = if last then "└─ " else "├─ " in
        let child_prefix = prefix ^ if last then "   " else "│  " in
        match v with
        | `Assoc [] | `List [] ->
            Buffer.add_string buf (prefix ^ connector ^ k ^ " (empty)\n")
        | `Assoc _ | `List _ ->
            Buffer.add_string buf (prefix ^ connector ^ k ^ "\n");
            walk child_prefix v
        | scalar ->
            if show_values then
              Buffer.add_string buf
                (prefix ^ connector ^ k ^ " = "
                ^ truncate_value (scalar_to_string scalar)
                ^ "\n")
            else Buffer.add_string buf (prefix ^ connector ^ k ^ "\n"))
      children
  in
  walk "" json;
  let s = Buffer.contents buf in
  if String.length s > 0 && s.[String.length s - 1] = '\n' then
    String.sub s 0 (String.length s - 1)
  else s

(* Load the current config file, redact secrets, optionally narrow to [section]
   (a dot-path), and render it as a tree. *)
let render_current ?section ?(show_values = true) () =
  let path = Dot_dir.config_path () in
  if not (Sys.file_exists path) then
    "No config file found at " ^ path ^ "\nRun 'clawq onboard' to create one."
  else
    match try Some (Yojson.Safe.from_file path) with _ -> None with
    | None -> "Error: failed to parse " ^ path
    | Some json -> (
        let redacted = Config_show.redact_json json in
        match section with
        | None -> render_json ~show_values ~root_label:"config" redacted
        | Some key -> (
            match Config_show.resolve_dot_path redacted key with
            | Some v -> render_json ~show_values ~root_label:key v
            | None -> Printf.sprintf "Section '%s' not found" key))

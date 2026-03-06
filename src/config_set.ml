(* config_set.ml — Read/write individual config values by dot-path *)

let config_path () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat (Filename.concat home ".clawq") "config.json"

let load_json path =
  if Sys.file_exists path then
    try Ok (Yojson.Safe.from_file path)
    with exn -> Error (Printexc.to_string exn)
  else Ok (`Assoc [])

let split_path key = String.split_on_char '.' key

let infer_value s =
  match String.lowercase_ascii s with
  | "true" -> `Bool true
  | "false" -> `Bool false
  | "null" -> `Null
  | _ -> (
      match int_of_string_opt s with
      | Some i -> `Int i
      | None -> (
          match float_of_string_opt s with
          | Some f -> `Float f
          | None ->
              if String.length s >= 2 && s.[0] = '[' then
                try Yojson.Safe.from_string s with _ -> `String s
              else `String s))

let rec json_get path json =
  match (path, json) with
  | [], v -> Some v
  | k :: rest, `Assoc fields -> (
      match List.assoc_opt k fields with
      | Some child -> json_get rest child
      | None -> None)
  | _ -> None

let rec json_set path value json =
  match (path, json) with
  | [ k ], `Assoc fields ->
      let updated =
        if List.mem_assoc k fields then
          List.map (fun (n, v) -> if n = k then (n, value) else (n, v)) fields
        else fields @ [ (k, value) ]
      in
      `Assoc updated
  | k :: rest, `Assoc fields ->
      let child =
        match List.assoc_opt k fields with Some c -> c | None -> `Assoc []
      in
      let updated_child = json_set rest value child in
      let updated =
        if List.mem_assoc k fields then
          List.map
            (fun (n, v) -> if n = k then (n, updated_child) else (n, v))
            fields
        else fields @ [ (k, updated_child) ]
      in
      `Assoc updated
  | [ k ], _ -> `Assoc [ (k, value) ]
  | k :: rest, _ -> `Assoc [ (k, json_set rest value (`Assoc [])) ]
  | [], _ -> value

let write_json path json =
  try
    let dir = Filename.dirname path in
    (try
       if not (Sys.file_exists dir) then (
         Unix.mkdir dir 0o755;
         ())
     with _ -> ());
    let s = Yojson.Safe.pretty_to_string ~std:true json in
    let oc = open_out path in
    output_string oc s;
    output_char oc '\n';
    close_out oc;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

let set_value key value =
  let path = config_path () in
  match load_json path with
  | Error e -> Printf.sprintf "Error loading config: %s" e
  | Ok json -> (
      let segments = split_path key in
      if segments = [ "" ] then "Error: empty key"
      else
        let json_val = infer_value value in
        let updated = json_set segments json_val json in
        match write_json path updated with
        | Ok () -> Printf.sprintf "Set %s = %s" key value
        | Error e -> Printf.sprintf "Error writing config: %s" e)

let get_value key =
  let path = config_path () in
  match load_json path with
  | Error e -> Printf.sprintf "Error loading config: %s" e
  | Ok json -> (
      let segments = split_path key in
      match json_get segments json with
      | Some (`String s) -> s
      | Some v -> Yojson.Safe.to_string v
      | None -> Printf.sprintf "Key '%s' not found" key)

(* config_search.ml — Search config keys by query string *)

let contains ~haystack ~needle =
  let h = String.length haystack and n = String.length needle in
  if n = 0 then true
  else if n > h then false
  else
    let found = ref false in
    let i = ref 0 in
    while !i <= h - n && not !found do
      if String.sub haystack !i n = needle then found := true;
      incr i
    done;
    !found

(* Returns a sort key: lower = higher priority. None = no match.
   Priority:
     0 = exact full path match
     1 = exact last segment match
     2 = exact any segment match
     3 = query is a prefix of the full path (query + "." or query = path)
     4 = query is a substring of any segment *)
let score_path ~query path =
  let q = String.lowercase_ascii query in
  let p = String.lowercase_ascii path in
  let segments = String.split_on_char '.' p in
  let last = List.nth segments (List.length segments - 1) in
  if p = q then Some 0
  else if last = q then Some 1
  else if List.exists (fun s -> s = q) segments then Some 2
  else
    let pref = q ^ "." in
    if
      String.length p >= String.length pref
      && String.sub p 0 (String.length pref) = pref
    then Some 3
    else if List.exists (fun s -> contains ~haystack:s ~needle:q) segments then
      Some 4
    else None

let load_config_json () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let path = Filename.concat (Filename.concat home ".clawq") "config.json" in
  if Sys.file_exists path then
    try Some (Yojson.Safe.from_file path) with _ -> None
  else None

let lookup_value json path =
  let segments = String.split_on_char '.' path in
  (* Can't look up dynamic placeholder paths in actual JSON *)
  if List.mem "<NAME>" segments then None
  else
    let rec walk j = function
      | [] -> Some j
      | k :: rest -> (
          match j with
          | `Assoc fields -> (
              match List.assoc_opt k fields with
              | Some v -> walk v rest
              | None -> None)
          | _ -> None)
    in
    walk json segments

let format_value path value =
  let is_secret =
    let segs = String.split_on_char '.' path in
    let last = List.nth segs (List.length segs - 1) in
    Config_show.is_secret_key last
  in
  match value with
  | None -> if is_secret then "[secret - not set]" else "(not set)"
  | Some `Null -> "null"
  | Some (`String s) when is_secret ->
      if String.length s = 0 then "[secret - empty]" else "***"
  | Some (`String s) -> s
  | Some v -> Yojson.Safe.to_string v

let search query =
  if String.length (String.trim query) = 0 then
    "Usage: clawq config search QUERY\n\
     Search config keys matching QUERY (case-insensitive).\n\n\
     Matches by: exact path, exact segment, path prefix, substring in segment."
  else
    let q = String.trim query in
    let all_paths = Config_set.all_schema_paths () in
    let matches =
      List.filter_map
        (fun (path, kind) ->
          match score_path ~query:q path with
          | None -> None
          | Some score -> Some (score, path, kind))
        all_paths
    in
    if matches = [] then
      Printf.sprintf
        "No config keys matching %S.\n\
         Tip: run 'clawq config show' to browse all config sections."
        q
    else begin
      let sorted =
        List.sort
          (fun (s1, p1, _) (s2, p2, _) ->
            let c = compare s1 s2 in
            if c <> 0 then c else compare p1 p2)
          matches
      in
      let json_opt = load_config_json () in
      let n = List.length sorted in
      let buf = Buffer.create 256 in
      Buffer.add_string buf
        (Printf.sprintf "Config keys matching %S (%d result%s):\n\n" q n
           (if n = 1 then "" else "s"));
      let max_len =
        List.fold_left (fun m (_, p, _) -> max m (String.length p)) 0 sorted
      in
      let col_width = min (max_len + 2) 58 in
      List.iter
        (fun (_, path, kind) ->
          let pad s =
            let len = String.length s in
            if len >= col_width then s
            else s ^ String.make (col_width - len) ' '
          in
          let value_str =
            match kind with
            | `Section ->
                "(section \xe2\x80\x94 use 'config show " ^ path ^ "')"
            | `Leaf -> (
                match json_opt with
                | None -> "(no config file)"
                | Some json -> format_value path (lookup_value json path))
          in
          Buffer.add_string buf
            (Printf.sprintf "  %s  %s\n" (pad path) value_str))
        sorted;
      Buffer.contents buf
    end

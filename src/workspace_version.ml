let versions_dir () = Dot_dir.sub "workspace-versions"
let valid_name_re = Str.regexp "^[a-zA-Z0-9][a-zA-Z0-9._-]*$"

let validate_name name =
  if String.length name = 0 then Error "Version name cannot be empty"
  else if String.length name > 128 then
    Error "Version name too long (max 128 characters)"
  else if not (Str.string_match valid_name_re name 0) then
    Error
      "Version name must start with alphanumeric and contain only \
       alphanumeric, hyphens, underscores, and dots"
  else Ok ()

let auto_backup_name () =
  let t = Unix.localtime (Unix.gettimeofday ()) in
  Printf.sprintf "pre-reset-%04d-%02d-%02d-%02d%02d%02d" (t.Unix.tm_year + 1900)
    (t.Unix.tm_mon + 1) t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min
    t.Unix.tm_sec

let version_dir name = Filename.concat (versions_dir ()) name

let copy_file ~src ~dst =
  let ic = open_in_bin src in
  Fun.protect
    (fun () ->
      let content = really_input_string ic (in_channel_length ic) in
      let oc = open_out_bin dst in
      Fun.protect
        (fun () -> output_string oc content)
        ~finally:(fun () -> close_out oc))
    ~finally:(fun () -> close_in ic)

let list_dir path =
  if Sys.file_exists path && Sys.is_directory path then
    Array.to_list (Sys.readdir path)
  else []

let rec list_files_recursive ~base ~prefix =
  let dir = Filename.concat base prefix in
  let entries = list_dir dir in
  List.concat_map
    (fun entry ->
      let rel = if prefix = "" then entry else Filename.concat prefix entry in
      let full = Filename.concat base rel in
      if Sys.is_directory full then list_files_recursive ~base ~prefix:rel
      else [ rel ])
    entries

let write_meta ~version_path ~workspace ~files =
  let t = Unix.gettimeofday () in
  let tm = Unix.localtime t in
  let timestamp =
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d" (tm.Unix.tm_year + 1900)
      (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
      tm.Unix.tm_sec
  in
  let json =
    `Assoc
      [
        ("timestamp", `String timestamp);
        ("workspace", `String workspace);
        ("files", `List (List.map (fun f -> `String f) files));
      ]
  in
  let path = Filename.concat version_path "_meta.json" in
  let oc = open_out path in
  Fun.protect
    (fun () -> output_string oc (Yojson.Safe.pretty_to_string json ^ "\n"))
    ~finally:(fun () -> close_out oc)

let read_meta ~version_path =
  let path = Filename.concat version_path "_meta.json" in
  if Sys.file_exists path then
    try
      let json = Yojson.Safe.from_file path in
      let timestamp =
        match Yojson.Safe.Util.member "timestamp" json with
        | `String s -> s
        | _ -> "unknown"
      in
      Some timestamp
    with _ -> None
  else None

let backup ~workspace ~name =
  match validate_name name with
  | Error e -> Error e
  | Ok () ->
      let vdir = version_dir name in
      if Sys.file_exists vdir then
        Error (Printf.sprintf "Version '%s' already exists" name)
      else if not (Sys.file_exists workspace && Sys.is_directory workspace) then
        Error (Printf.sprintf "Workspace '%s' does not exist" workspace)
      else begin
        let root = versions_dir () in
        Workspace_scaffold.ensure_dir root;
        Workspace_scaffold.ensure_dir vdir;
        let files =
          list_files_recursive ~base:workspace ~prefix:""
          |> List.filter (fun f -> f <> "_meta.json")
        in
        List.iter
          (fun rel ->
            let src = Filename.concat workspace rel in
            let dst = Filename.concat vdir rel in
            Workspace_scaffold.ensure_dir (Filename.dirname dst);
            copy_file ~src ~dst)
          files;
        write_meta ~version_path:vdir ~workspace ~files;
        Ok files
      end

let restore ~workspace ~name =
  match validate_name name with
  | Error e -> Error e
  | Ok () ->
      let vdir = version_dir name in
      if not (Sys.file_exists vdir && Sys.is_directory vdir) then
        Error (Printf.sprintf "Version '%s' does not exist" name)
      else begin
        Workspace_scaffold.ensure_dir workspace;
        let files =
          list_files_recursive ~base:vdir ~prefix:""
          |> List.filter (fun f -> f <> "_meta.json")
        in
        List.iter
          (fun rel ->
            let src = Filename.concat vdir rel in
            let dst = Filename.concat workspace rel in
            Workspace_scaffold.ensure_dir (Filename.dirname dst);
            copy_file ~src ~dst)
          files;
        Ok files
      end

let list_versions () =
  let root = versions_dir () in
  let entries = list_dir root in
  let versions =
    List.filter_map
      (fun name ->
        let vdir = Filename.concat root name in
        if Sys.is_directory vdir then
          let timestamp =
            match read_meta ~version_path:vdir with
            | Some ts -> ts
            | None -> "unknown"
          in
          Some (name, timestamp)
        else None)
      entries
  in
  List.sort (fun (_, a) (_, b) -> String.compare b a) versions

let rec remove_dir path =
  if Sys.is_directory path then begin
    let entries = Sys.readdir path in
    Array.iter
      (fun entry ->
        let full = Filename.concat path entry in
        remove_dir full)
      entries;
    Unix.rmdir path
  end
  else Sys.remove path

let delete ~name =
  match validate_name name with
  | Error e -> Error e
  | Ok () ->
      let vdir = version_dir name in
      if not (Sys.file_exists vdir && Sys.is_directory vdir) then
        Error (Printf.sprintf "Version '%s' does not exist" name)
      else begin
        (try remove_dir vdir with _ -> ());
        if Sys.file_exists vdir then
          Error (Printf.sprintf "Failed to fully remove version '%s'" name)
        else Ok ()
      end

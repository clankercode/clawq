type source = Session_history of string | Acp_history | Log_file of string
type entry = { role : string; content : string }

let default_inline_lines = 200
let hard_inline_lines = 300
let max_inline_chars = 12_000
let stable_session_key id = Printf.sprintf "__bg_task:%d" id

let source_name = function
  | Session_history key -> Printf.sprintf "session %s" key
  | Acp_history -> "ACP history"
  | Log_file path -> Printf.sprintf "log file %s" path

let sanitize_export_component s =
  let b = Bytes.of_string s in
  for i = 0 to Bytes.length b - 1 do
    match Bytes.get b i with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> ()
    | _ -> Bytes.set b i '_'
  done;
  Bytes.to_string b

let ensure_dir path =
  if Sys.file_exists path then ()
  else
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let export_path ~id =
  let root = Dot_dir.ensure () in
  let dir = Filename.concat root "background-transcripts" in
  ensure_dir dir;
  Filename.concat dir
    (Printf.sprintf "task-%d-%d.jsonl" id (int_of_float (Unix.gettimeofday ())))

let json_of_entry ~source idx entry =
  `Assoc
    [
      ("index", `Int idx);
      ("source", `String (source_name source));
      ("role", `String entry.role);
      ("content", `String entry.content);
    ]

let write_jsonl ~id ~source entries =
  let path = export_path ~id in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      entries
      |> List.iteri (fun idx entry ->
          output_string oc
            (Yojson.Safe.to_string (json_of_entry ~source (idx + 1) entry));
          output_char oc '\n'));
  path

let session_entries ~db ~session_key =
  Memory.load_history ~db ~session_key
  |> List.map (fun (msg : Provider.message) ->
      { role = msg.role; content = msg.content })

let acp_entries ~db ~id =
  Acp_history.get_history ~db ~task_id:id
  |> List.filter_map (fun (e : Acp_history.history_entry) ->
      match e.content_text with
      | Some content when String.trim content <> "" ->
          let role =
            match e.role with
            | Some role when String.trim role <> "" -> role
            | _ -> e.msg_type
          in
          Some { role; content }
      | _ -> None)

let log_entries path =
  if not (Sys.file_exists path) then []
  else
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let rec loop acc =
          match input_line ic with
          | line -> loop ({ role = "log"; content = line } :: acc)
          | exception End_of_file -> List.rev acc
        in
        loop [])

let load_entries ~db ~id =
  let session_key = stable_session_key id in
  match session_entries ~db ~session_key with
  | _ :: _ as entries -> Ok (Session_history session_key, entries)
  | [] -> (
      if Acp_history.has_history ~db ~task_id:id then
        Ok (Acp_history, acp_entries ~db ~id)
      else
        match Background_task.get_task ~db ~id with
        | None ->
            Error (Printf.sprintf "No background task found with id %d" id)
        | Some task -> (
            match task.log_path with
            | Some path -> Ok (Log_file path, log_entries path)
            | None ->
                Error
                  (Printf.sprintf
                     "Background task %d has no session history, ACP history, \
                      or log file yet"
                     id)))

let compile_regex = function
  | None -> Ok None
  | Some pattern -> (
      try Ok (Some (Str.regexp pattern))
      with Failure msg ->
        Error
          (Printf.sprintf
             "Invalid regex %S: %s. Use a valid OCaml Str regular expression."
             pattern msg))

let entry_text entry = Printf.sprintf "%s: %s" entry.role entry.content

let filter_entries ?regex entries =
  match compile_regex regex with
  | Error _ as err -> err
  | Ok None -> Ok entries
  | Ok (Some re) ->
      Ok
        (List.filter
           (fun entry ->
             try
               ignore (Str.search_forward re (entry_text entry) 0);
               true
             with Not_found -> false)
           entries)

let clamp_lines = function
  | None -> default_inline_lines
  | Some n -> max 1 (min hard_inline_lines n)

let take n xs =
  let rec loop acc n = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | x :: rest -> loop (x :: acc) (n - 1) rest
  in
  loop [] n xs

let render_inline ~source ~shown ~total entries =
  let b = Buffer.create 256 in
  Buffer.add_string b
    (Printf.sprintf "Transcript source: %s\nShowing %d/%d line(s)\n\n"
       (source_name source) shown total);
  entries
  |> List.iteri (fun idx entry ->
      Buffer.add_string b
        (Printf.sprintf "%04d %s\n" (idx + 1) (entry_text entry)));
  Buffer.contents b

let render ?regex ?max_lines ?(export = false) ~db ~id () =
  match load_entries ~db ~id with
  | Error msg -> "Error: " ^ msg
  | Ok (source, entries) -> (
      match filter_entries ?regex entries with
      | Error msg -> "Error: " ^ msg
      | Ok filtered -> (
          let line_cap = clamp_lines max_lines in
          let total = List.length filtered in
          let shown_entries = take line_cap filtered in
          let inline =
            render_inline ~source
              ~shown:(List.length shown_entries)
              ~total shown_entries
          in
          let too_many_lines = total > line_cap in
          let too_many_chars = String.length inline > max_inline_chars in
          let export_path =
            if export || too_many_lines || too_many_chars then
              Some (write_jsonl ~id ~source filtered)
            else None
          in
          match (too_many_lines || too_many_chars, export_path) with
          | true, Some path ->
              Printf.sprintf
                "refusing inline transcript for task %d: %d filtered line(s), \
                 cap %d line(s), %d rendered chars (budget %d).\n\
                 JSONL export: %s"
                id total line_cap (String.length inline) max_inline_chars path
          | _ -> (
              match export_path with
              | Some path -> inline ^ Printf.sprintf "\nJSONL export: %s" path
              | None -> inline)))

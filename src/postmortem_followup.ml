(* B610: after a postmortem turn finishes, scan the agent's response and the
   on-disk postmortem doc for a structured "FILE_BUG" block. When found, lodge
   the bug via the local `bl` CLI. The marker is intentionally explicit so the
   postmortem agent has to opt in — we don't want every postmortem to mass-file
   speculative bugs.

   Marker shape (case-sensitive prefix at start of line):

       FILE_BUG: <one-line title>
       BODY:
       <multi-line body until end of message or a "ENDBUG" line>

   The body may span multiple lines and can include backticks / code. The
   helper extracts up to one bug per postmortem doc. *)

let trim_trailing_ws s =
  let n = String.length s in
  let i = ref (n - 1) in
  while !i >= 0 && (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\n') do
    decr i
  done;
  if !i = n - 1 then s else String.sub s 0 (!i + 1)

(* Parse a "FILE_BUG: <title>\nBODY:\n<body...>\nENDBUG" block (ENDBUG line
   optional — body runs to end of input otherwise). Returns (title, body) or
   None when no marker is present. *)
let extract_file_bug (text : string) : (string * string) option =
  let lines = String.split_on_char '\n' text in
  let rec find_title = function
    | [] -> None
    | line :: rest ->
        let prefix = "FILE_BUG:" in
        let plen = String.length prefix in
        let len = String.length line in
        if len > plen && String.sub line 0 plen = prefix then
          let title = String.trim (String.sub line plen (len - plen)) in
          if title = "" then None else Some (title, rest)
        else find_title rest
  in
  match find_title lines with
  | None -> None
  | Some (title, after_title) ->
      let rec skip_to_body = function
        | [] -> None
        | line :: rest when String.trim line = "BODY:" -> Some rest
        | _ :: rest -> skip_to_body rest
      in
      let body_lines_opt = skip_to_body after_title in
      let body_lines =
        match body_lines_opt with None -> after_title | Some ls -> ls
      in
      let rec collect acc = function
        | [] -> List.rev acc
        | line :: _ when String.trim line = "ENDBUG" -> List.rev acc
        | line :: rest -> collect (line :: acc) rest
      in
      let body = String.concat "\n" (collect [] body_lines) in
      let body = trim_trailing_ws body in
      if String.trim body = "" then None else Some (title, body)

let bl_executable () =
  try Sys.getenv "BL_BIN" with Not_found -> "/home/xertrov/.local/bin/bl"

(* Normalize a title for dedup: lowercase, collapse non-alnum to single space,
   trim. Catches near-duplicates like "Cron jobs need ... — disable" vs
   "Cron consecutive-identical-output detection — disable and alert". *)
let normalize_title s =
  let buf = Buffer.create (String.length s) in
  let last_was_space = ref true in
  String.iter
    (fun c ->
      let c = Char.lowercase_ascii c in
      if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then begin
        Buffer.add_char buf c;
        last_was_space := false
      end
      else if not !last_was_space then begin
        Buffer.add_char buf ' ';
        last_was_space := true
      end)
    s;
  String.trim (Buffer.contents buf)

(* Compute shingled token sets and Jaccard similarity to flag near-duplicate
   titles. Returns true when overlap >= 0.6. *)
let titles_similar a b =
  let toks s =
    String.split_on_char ' ' (normalize_title s)
    |> List.filter (fun t -> String.length t >= 3)
  in
  let ta = toks a and tb = toks b in
  if ta = [] || tb = [] then false
  else
    let set_a = List.sort_uniq compare ta in
    let set_b = List.sort_uniq compare tb in
    let inter = List.filter (fun t -> List.mem t set_b) set_a |> List.length in
    let union = List.length (List.sort_uniq compare (set_a @ set_b)) in
    if union = 0 then false else float_of_int inter /. float_of_int union >= 0.6

(* Read the YAML frontmatter `title:` field from a .todo file. Frontmatter is
   between two `---` lines; we scan for a leading `title:` key inside it. *)
let read_todo_title path =
  try
    In_channel.with_open_text path (fun ic ->
        let rec read_lines acc =
          match In_channel.input_line ic with
          | None -> List.rev acc
          | Some l -> read_lines (l :: acc)
        in
        let lines = read_lines [] in
        let rec find_title in_fm = function
          | [] -> None
          | l :: rest ->
              let trimmed = String.trim l in
              if trimmed = "---" then
                if in_fm then None (* end of frontmatter without title *)
                else find_title true rest
              else if in_fm then begin
                let key = "title:" in
                let klen = String.length key in
                let llen = String.length trimmed in
                if llen > klen && String.sub trimmed 0 klen = key then
                  Some (String.trim (String.sub trimmed klen (llen - klen)))
                else find_title true rest
              end
              else find_title false rest
        in
        find_title false lines)
  with _ -> None

(* Look at recent backlog bug frontmatter titles for a similar title. Reading
   the actual title (rather than the truncated filename slug) gives a far
   better signal: filenames are clipped to ~30 chars by `bl`, so a long
   title like "Cron jobs need consecutive-identical-output detection (9th
   recurrence...)" becomes "cron-jobs-need-consecutive-ide" — too few tokens
   for Jaccard to score against. *)
let recent_similar_bug_exists ~title =
  let dir = "/home/xertrov/src/clawq/.backlog/bugs" in
  if not (Sys.file_exists dir && Sys.is_directory dir) then false
  else
    let files =
      Sys.readdir dir |> Array.to_list
      |> List.filter (fun f -> Filename.check_suffix f ".todo")
    in
    List.exists
      (fun f ->
        match read_todo_title (Filename.concat dir f) with
        | Some t -> titles_similar title t
        | None -> false)
      files

(* Run `bl bug --title ... --body ... --simple`. Returns Lwt.unit; logs the
   outcome. *)
let lodge_bug ~title ~body ~session_key ~reason ~doc_path =
  let open Lwt.Syntax in
  let tagged_body =
    Printf.sprintf
      "%s\n\n\
       --\n\
       Auto-filed by clawq postmortem for session %s (pattern: %s).\n\
       Postmortem doc: %s"
      body session_key reason doc_path
  in
  let bl = bl_executable () in
  Lwt.catch
    (fun () ->
      let cmd =
        ( bl,
          [| bl; "bug"; "--title"; title; "--body"; tagged_body; "--simple" |]
        )
      in
      let* status = Lwt_process.exec ~stdout:`Dev_null ~stderr:`Dev_null cmd in
      (match status with
      | Unix.WEXITED 0 ->
          Logs.info (fun m ->
              m "postmortem: auto-lodged backlog bug — title=%S session=%s"
                title session_key)
      | Unix.WEXITED code ->
          Logs.warn (fun m ->
              m "postmortem: bl bug exited with code %d (title=%S)" code title)
      | _ ->
          Logs.warn (fun m ->
              m "postmortem: bl bug terminated abnormally (title=%S)" title));
      Lwt.return_unit)
    (fun exn ->
      Logs.warn (fun m ->
          m "postmortem: bl bug invocation failed: %s" (Printexc.to_string exn));
      Lwt.return_unit)

let try_lodge_bug ~doc_path ~response ~session_key ~reason =
  let open Lwt.Syntax in
  (* Try the postmortem doc first (the agent is told to append its analysis
     there); fall back to the immediate agent response. *)
  let doc_content =
    try Some (In_channel.with_open_text doc_path In_channel.input_all)
    with _ -> None
  in
  let candidate =
    match doc_content with
    | Some c when extract_file_bug c <> None -> Some c
    | _ -> if extract_file_bug response <> None then Some response else None
  in
  match candidate with
  | None -> Lwt.return_unit
  | Some text -> (
      match extract_file_bug text with
      | None -> Lwt.return_unit
      | Some (title, body) ->
          if recent_similar_bug_exists ~title then begin
            Logs.info (fun m ->
                m
                  "postmortem: skipping bug lodge — similar backlog entry \
                   already exists (title=%S session=%s)"
                  title session_key);
            Lwt.return_unit
          end
          else
            let* () = lodge_bug ~title ~body ~session_key ~reason ~doc_path in
            Lwt.return_unit)

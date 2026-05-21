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
          let* () = lodge_bug ~title ~body ~session_key ~reason ~doc_path in
          Lwt.return_unit)

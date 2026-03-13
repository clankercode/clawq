let postmortems_dir () =
  let dir = Dot_dir.sub "postmortems" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  dir

let format_timestamp () =
  let t = Unix.gettimeofday () in
  let tm = Unix.localtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d%02d%02d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let format_iso_timestamp () =
  let t = Unix.gettimeofday () in
  let tm = Unix.localtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let sanitize_for_filename s =
  String.map (function '/' | '\\' -> '_' | c -> c) s

let write_doc ~session_key ~pattern ~evidence_summary ~correction =
  let open Lwt.Syntax in
  let dir = postmortems_dir () in
  let ts = format_timestamp () in
  let filename =
    Printf.sprintf "%s-%s.md" ts (sanitize_for_filename session_key)
  in
  let path = Filename.concat dir filename in
  let iso_ts = format_iso_timestamp () in
  let content =
    Printf.sprintf
      "# Postmortem: %s\n\n\
       **Timestamp**: %s\n\
       **Pattern**: %s\n\n\
       ## Evidence\n\n\
       %s\n\n\
       ## Correction Applied\n\n\
       %s\n\n\
       ## Analysis\n\n\
       *(To be filled by postmortem agent)*\n\n\
       ## Takeaways\n\n\
       *(To be filled by postmortem agent)*\n"
      session_key iso_ts pattern evidence_summary correction
  in
  Out_channel.with_open_text path (fun oc -> output_string oc content);
  let* () = Lwt.return_unit in
  Lwt.return path

let format_history_text messages =
  let limit = 15 in
  let max_chars = 3000 in
  let rec take n = function
    | [] -> []
    | _ when n = 0 -> []
    | x :: rest -> x :: take (n - 1) rest
  in
  let msgs = take limit messages in
  let lines =
    List.map
      (fun (msg : Provider.message) ->
        Printf.sprintf "[%s]: %s" msg.role msg.content)
      msgs
  in
  let full = String.concat "\n" lines in
  if String.length full > max_chars then String.sub full 0 max_chars ^ "..."
  else full

let make_postmortem_prompt ~session_key ~reason ~doc_path ~history_text () =
  Printf.sprintf
    "You are a postmortem analyst for an AI agent system.\n\n\
     An agent session was detected as stuck with this pattern: %s\n\n\
     The stuck session history (recent messages) is:\n\
     %s\n\n\
     Your tasks:\n\
     1. Identify the root cause (what specifically failed, not surface symptoms)\n\
     2. Search for a solution using your available tools (shell_exec, \
     file_read, memory_recall)\n\
     3. Update the postmortem document at: %s\n\
    \   Append to the \"## Analysis\" section: your root cause analysis\n\
    \   Append to the \"## Takeaways\" section: 1-3 bullet points to prevent \
     recurrence\n\
     4. Update the MEMORY.md file in the workspace (use memory_store tool with \
     key \"lesson:%s:%s\")\n\
     5. Reply with a one-sentence summary of what you found.\n\n\
     Focus on the ROOT CAUSE, not surface errors. What single thing, if fixed, \
     would have prevented this?"
    reason history_text doc_path session_key (format_timestamp ())

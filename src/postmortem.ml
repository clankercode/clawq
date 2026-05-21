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

(* B611: render the postmortem evidence with the structured tool-call data
   the analyst needs to diagnose loops. Assistant turns with tool_calls now
   include each tool's name and args (truncated). Tool result turns include
   the tool name and a content preview. Plain content is preserved
   verbatim. The newest 20 messages are kept (was 15) up to ~6000 chars
   (was 3000) since structured rendering needs more room. *)
let format_history_text messages =
  let limit = 20 in
  let max_chars = 6000 in
  let truncate_field s n =
    if String.length s <= n then s else String.sub s 0 n ^ "...[truncated]"
  in
  let rec take n = function
    | [] -> []
    | _ when n = 0 -> []
    | x :: rest -> x :: take (n - 1) rest
  in
  let msgs = take limit messages in
  let format_tool_call (tc : Provider.tool_call) =
    Printf.sprintf "  - tool=%s id=%s args=%s" tc.function_name tc.id
      (truncate_field tc.arguments 400)
  in
  let format_msg (msg : Provider.message) =
    match msg.role with
    | "assistant" when msg.tool_calls <> [] ->
        let calls_block =
          String.concat "\n" (List.map format_tool_call msg.tool_calls)
        in
        let text =
          if msg.content = "" then ""
          else "\n  text=" ^ truncate_field msg.content 400
        in
        Printf.sprintf "[assistant] tool_calls=%d:\n%s%s"
          (List.length msg.tool_calls)
          calls_block text
    | "tool" ->
        let name = Option.value msg.name ~default:"unknown" in
        let preview = truncate_field msg.content 600 in
        let id = Option.value msg.tool_call_id ~default:"(no tool_call_id)" in
        Printf.sprintf "[tool name=%s id=%s] %s" name id preview
    | role -> Printf.sprintf "[%s] %s" role (truncate_field msg.content 600)
  in
  let lines = List.map format_msg msgs in
  let full = String.concat "\n" lines in
  if String.length full > max_chars then
    String.sub full 0 max_chars ^ "\n...[history truncated for length]"
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
     6. B624: if the root cause is a structural defect (broken config, missing \
     required field, broken prompt template, looping cron) that the user \
     should fix, emit a FILE_BUG block at the END of your response in this \
     exact format (no leading whitespace on the marker lines):\n\n\
    \   FILE_BUG: <concise one-line title>\n\
    \   BODY:\n\
    \   <one or more lines describing the root cause and suggested fix,\n\
    \    including any file paths, config keys, or commands to run>\n\
    \   ENDBUG\n\n\
     The system will automatically lodge this as a backlog item via `bl bug` \
     so it gets fixed. Do NOT emit FILE_BUG for transient failures (rate \
     limits, network glitches, model jitter) — only for structural defects \
     worth a maintainer's attention.\n\n\
     Focus on the ROOT CAUSE, not surface errors. What single thing, if fixed, \
     would have prevented this?"
    reason history_text doc_path session_key (format_timestamp ())

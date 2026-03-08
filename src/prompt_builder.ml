let contains_sub s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec loop i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else loop (i + 1)
  in
  if len_sub = 0 then true else loop 0

let now_utc_iso8601 () =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let now_local_iso8601 () =
  let tm = Unix.localtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let local_timezone_label () =
  match Sys.getenv_opt "TZ" with
  | Some tz when String.trim tz <> "" -> tz
  | _ -> "system-local"

let read_file_trimmed path =
  try
    let ic = open_in path in
    Fun.protect
      (fun () -> input_line ic |> String.trim)
      ~finally:(fun () -> close_in_noerr ic)
  with _ -> ""

let parse_gitdir_file path =
  let line = read_file_trimmed path in
  let prefix = "gitdir: " in
  let prefix_len = String.length prefix in
  if String.length line > prefix_len && String.sub line 0 prefix_len = prefix
  then
    let raw = String.sub line prefix_len (String.length line - prefix_len) in
    let dir = Filename.dirname path in
    Some (if Filename.is_relative raw then Filename.concat dir raw else raw)
  else None

let git_dir_at dir =
  let dot_git = Filename.concat dir ".git" in
  if Sys.file_exists dot_git then
    if Sys.is_directory dot_git then Some dot_git else parse_gitdir_file dot_git
  else None

let rec find_git_root_and_dir dir =
  match git_dir_at dir with
  | Some git_dir -> Some (dir, git_dir)
  | None ->
      let parent = Filename.dirname dir in
      if parent = dir then None else find_git_root_and_dir parent

let current_git_branch ~git_dir =
  let head = read_file_trimmed (Filename.concat git_dir "HEAD") in
  let prefix = "ref: refs/heads/" in
  let prefix_len = String.length prefix in
  if String.length head > prefix_len && String.sub head 0 prefix_len = prefix
  then Some (String.sub head prefix_len (String.length head - prefix_len))
  else if head = "" then None
  else
    let short_len = min 12 (String.length head) in
    Some ("detached@" ^ String.sub head 0 short_len)

let read_command_trimmed command =
  try
    let ic = Unix.open_process_in command in
    Fun.protect
      (fun () -> input_line ic |> String.trim)
      ~finally:(fun () -> ignore (Unix.close_process_in ic))
  with _ -> ""

let detected_os_label =
  lazy
    (match read_command_trimmed "uname -s -r -m" with
    | s when String.trim s <> "" -> s
    | _ -> Sys.os_type)

type context_usage = {
  history_messages : int;
  estimated_history_tokens : int;
  context_window_tokens : int;
  compaction_threshold_tokens : int;
  max_messages_per_session : int;
  compacted_before_turn : bool;
}

type runtime_context_details = {
  session_id : string;
  session_name : string option;
  is_main_session : bool;
  heartbeat_routing_applies : bool;
  effective_workspace : string;
  workspace_only : bool;
  sandbox_backend_requested : string;
  sandbox_backend_effective : string;
  shell_is_sandboxed : bool;
  shell_policy_summary : string;
  shell_visible_roots_summary : string;
  context_usage : context_usage option;
}

let yes_no b = if b then "yes" else "no"

let add_runtime_details lines (details : runtime_context_details) =
  let add line = lines := line :: !lines in
  add ("- Session id: " ^ details.session_id);
  (match details.session_name with
  | Some name when String.trim name <> "" -> add ("- Session name: " ^ name)
  | _ -> ());
  add ("- Main session: " ^ yes_no details.is_main_session);
  add
    ("- Heartbeat routing applies: " ^ yes_no details.heartbeat_routing_applies);
  add ("- Effective workspace: " ^ details.effective_workspace);
  add ("- Workspace only: " ^ yes_no details.workspace_only);
  add
    (Printf.sprintf "- Shell sandboxed: %s (requested=%s effective=%s)"
       (yes_no details.shell_is_sandboxed)
       details.sandbox_backend_requested details.sandbox_backend_effective);
  add ("- Shell policy: " ^ details.shell_policy_summary);
  add ("- Shell visible roots: " ^ details.shell_visible_roots_summary);
  match details.context_usage with
  | None -> ()
  | Some usage ->
      add
        (Printf.sprintf "- Context usage: %d messages, ~%d/%d tokens"
           usage.history_messages usage.estimated_history_tokens
           usage.context_window_tokens);
      add
        (Printf.sprintf
           "- Compaction: before a turn when history > %d messages or est \
            tokens > %d; compacted before this turn: %s"
           usage.max_messages_per_session usage.compaction_threshold_tokens
           (yes_no usage.compacted_before_turn))

let build_runtime_context ~(config : Runtime_config.t) ?details () =
  if not config.prompt.dynamic_enabled then None
  else
    let lines = ref [] in
    let add line = lines := line :: !lines in
    if config.prompt.include_datetime_section then begin
      add ("- Current UTC: " ^ now_utc_iso8601 ());
      add ("- Local time: " ^ now_local_iso8601 ());
      add ("- Local timezone: " ^ local_timezone_label ())
    end;
    if config.prompt.include_runtime_section then begin
      add ("- Current working directory: " ^ Sys.getcwd ());
      add ("- Workspace root: " ^ Runtime_config.effective_workspace config);
      (match find_git_root_and_dir (Sys.getcwd ()) with
      | Some (repo_root, git_dir) -> (
          add ("- Git repo root: " ^ repo_root);
          match current_git_branch ~git_dir with
          | Some branch -> add ("- Git branch: " ^ branch)
          | None -> ())
      | None -> ());
      add ("- OS: " ^ Lazy.force detected_os_label)
    end;
    (match details with
    | Some details -> add_runtime_details lines details
    | None -> ());
    match List.rev !lines with
    | [] -> None
    | items ->
        Some
          (String.concat "\n"
             ([ "[Runtime context for this turn only]" ] @ items))

let safe_prompt_filename name =
  name <> ""
  && (not (contains_sub name ".."))
  && (not (contains_sub name "/"))
  && not (contains_sub name "\\")

let read_file_limited path limit =
  try
    let ic = open_in_bin path in
    let n = in_channel_length ic in
    let take = min n limit in
    let buf = Bytes.create take in
    really_input ic buf 0 take;
    close_in ic;
    let s = Bytes.to_string buf in
    if n > limit then s ^ "\n[...truncated...]" else s
  with _ -> ""

let private_only_files = [ "MEMORY.md"; "memory.md" ]

let workspace_doc_blocks ~(config : Runtime_config.t)
    ?(session_type = "private") () =
  let workspace = Runtime_config.effective_workspace config in
  let ego_path = Filename.concat workspace "EGO.md" in
  let ego_exists = Sys.file_exists ego_path in
  let is_group = session_type = "group" in
  let budget = ref config.prompt.max_workspace_total_chars in
  let blocks = ref [] in
  let seen = Hashtbl.create 16 in
  let add_file file =
    if
      safe_prompt_filename file && !budget > 0
      && (not (Hashtbl.mem seen file))
      && (not (file = "SOUL.md" && ego_exists))
      && not (is_group && List.mem file private_only_files)
    then begin
      Hashtbl.replace seen file true;
      let path = Filename.concat workspace file in
      if Sys.file_exists path then
        let cap = min !budget config.prompt.max_workspace_file_chars in
        let content = read_file_limited path cap in
        if content <> "" then begin
          budget := !budget - min !budget (String.length content);
          blocks := (file, content) :: !blocks
        end
    end
  in
  List.iter add_file config.prompt.workspace_files;
  if Sys.file_exists (Filename.concat workspace "BOOTSTRAP.md") then
    add_file "BOOTSTRAP.md";
  List.rev !blocks

let tools_block tool_registry =
  match tool_registry with
  | None -> []
  | Some registry ->
      Tool_registry.list registry
      |> List.map (fun (t : Tool.t) ->
          let risk =
            match t.risk_level with
            | Tool.Low -> "low"
            | Tool.Medium -> "medium"
            | Tool.High -> "high"
          in
          Printf.sprintf "- %s (risk=%s): %s" t.name risk t.description)

let base_prompt =
  "You are an autonomous AI assistant. Your identity, principles, and \
   operating protocol are defined by the workspace files below. Embody them."

let group_chat_section ~channel_type =
  match channel_type with
  | "group" ->
      Some
        "## Group Chat Conduct\n\
         In group channels, only respond when directly addressed or when your \
         input is clearly relevant.\n\
         You may use [NO_REPLY] as your entire response to indicate you \
         intentionally decline to reply."
  | _ -> None

let skills_section ~workspace_dir =
  let skills_dir = Filename.concat workspace_dir ".claude-p/skills" in
  if not (Sys.file_exists skills_dir) then None
  else
    let skills = try Sys.readdir skills_dir |> Array.to_list with _ -> [] in
    let skills = List.filter (fun s -> s.[0] <> '.') skills in
    if skills = [] then None
    else
      Some
        (Printf.sprintf "## Available Skills\n%s"
           (String.concat "\n" (List.map (fun s -> "- " ^ s) skills)))

type scheduled_job_info = {
  sj_name : string;
  sj_schedule : string;
  sj_description : string option;
}

let scheduled_tasks_section ~jobs =
  match jobs with
  | [] -> None
  | _ ->
      let lines =
        List.map
          (fun j ->
            Printf.sprintf "- %s: `%s` — %s" j.sj_name j.sj_schedule
              (match j.sj_description with
              | Some d -> d
              | None -> "(no description)"))
          jobs
      in
      Some ("## Scheduled Tasks\n" ^ String.concat "\n" lines)

let attachment_syntax_block attachments =
  if attachments = [] then None
  else
    let types_present =
      List.sort_uniq String.compare
        (List.map (fun (atype, _path) -> atype) attachments)
    in
    let syntax_lines =
      List.map
        (fun atype ->
          Printf.sprintf "- [%s:/path] — reference a %s attachment"
            (String.uppercase_ascii atype)
            atype)
        types_present
    in
    let refs =
      List.map
        (fun (atype, path) ->
          Printf.sprintf "  [%s:%s]" (String.uppercase_ascii atype) path)
        attachments
    in
    Some
      (String.concat "\n"
         ([
            "## Attachments";
            "This message includes attachments. Reference syntax:";
          ]
         @ syntax_lines @ [ ""; "Attached:" ] @ refs))

let build ~(config : Runtime_config.t) ~tool_registry ?(attachments = [])
    ?(channel_type = "dm") ?(workspace = None) ?(scheduled_jobs = []) () =
  if not config.prompt.dynamic_enabled then
    if config.agent_defaults.system_prompt <> "" then
      config.agent_defaults.system_prompt
    else base_prompt
  else
    let lines = ref [] in
    let add s = lines := s :: !lines in
    let ws = Runtime_config.effective_workspace config in
    let effective_ws = match workspace with Some w -> w | None -> ws in
    if config.agent_defaults.system_prompt <> "" then begin
      add config.agent_defaults.system_prompt;
      add ""
    end
    else begin
      add base_prompt;
      add ""
    end;
    if config.prompt.include_workspace_section then begin
      add "## Workspace Context";
      add ("Root: " ^ ws);
      let docs = workspace_doc_blocks ~config ~session_type:channel_type () in
      if docs = [] then
        add "No workspace identity files found. Operating with defaults only."
      else begin
        add "";
        add
          "The following files define your identity, behavioral protocol, and \
           local context. EGO.md governs who you are. AGENTS.md governs how \
           you operate. All other files provide situational context. When \
           instructions conflict, EGO.md takes precedence, then AGENTS.md, \
           then the rest in order of appearance.";
        add "";
        List.iter
          (fun (name, content) ->
            add ("### " ^ name);
            add content;
            add "")
          docs
      end
    end;
    if config.prompt.include_safety_section then begin
      add "";
      add "## Safety Invariants";
      add
        "- Secrets, tokens, credentials, and private data are never revealed, \
         logged, or echoed.";
      add
        "- Destructive or irreversible actions require explicit prior \
         authorization.";
      add
        "- Workspace boundaries and configured security policies are hard \
         constraints, not suggestions."
    end;
    add "";
    add "## Operating Stance";
    add "- Act, then report. Prefer execution to speculation.";
    add
      "- State what you know, what you verified, and what remains uncertain — \
       never conflate the three.";
    add "- Scope every intervention to what was actually requested.";
    if config.prompt.include_tools_section then begin
      add "";
      add "## Tools";
      let tool_lines = tools_block tool_registry in
      if tool_lines = [] then add "- No tools registered."
      else List.iter add tool_lines
    end;
    add "";
    add "## Clawq Runtime";
    add
      (Printf.sprintf "- Version: %s (git %s, built %s)" Build_info.version
         Build_info.git_shorthash Build_info.build_date);
    add "";
    add "## Self-Reference";
    add "- Full self-knowledge document: https://clawq.org/llms-full.txt";
    add
      "- Contains: all CLI commands, config fields with defaults, built-in \
       tools, channels, gateway endpoints, setup guides.";
    add
      "- Fetch this when you need to understand your own capabilities or \
       modify your own configuration/behavior.";
    (match group_chat_section ~channel_type with
    | Some s ->
        add "";
        add s
    | None -> ());
    (match skills_section ~workspace_dir:effective_ws with
    | Some s ->
        add "";
        add s
    | None -> ());
    (match scheduled_tasks_section ~jobs:scheduled_jobs with
    | Some s ->
        add "";
        add s
    | None -> ());
    (match attachment_syntax_block attachments with
    | Some s ->
        add "";
        add s
    | None -> ());
    String.concat "\n" (List.rev !lines)

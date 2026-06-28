(* Global hook for tunnel status — set by daemon.ml via Tunnel_manager *)
let tunnel_status_line_fn : (unit -> string) ref =
  ref (fun () -> "not configured")

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

let day_of_week_abbrev wday =
  [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |].(wday)

let now_local_iso8601 () =
  let tm = Unix.localtime (Unix.gettimeofday ()) in
  Printf.sprintf "%s %04d-%02d-%02dT%02d:%02d:%02d"
    (day_of_week_abbrev tm.Unix.tm_wday)
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

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

let list_cwd_entries ?effective_cwd () =
  let cwd = match effective_cwd with Some c -> c | None -> Sys.getcwd () in
  try
    let entries = Sys.readdir cwd |> Array.to_list in
    let entries = List.sort String.compare entries in
    let classify name =
      let path = Filename.concat cwd name in
      if try Sys.is_directory path with _ -> false then name ^ "/" else name
    in
    let classified = List.map classify entries in
    let max_entries = 200 in
    let len = List.length classified in
    if len = 0 then "(empty directory)"
    else if len <= max_entries then String.concat "  " classified
    else
      let taken = List.filteri (fun i _ -> i < max_entries) classified in
      String.concat "  " taken
      ^ Printf.sprintf "  ...(%d more)" (len - max_entries)
  with _ -> "(unable to list)"

type context_usage = {
  history_messages : int;
  estimated_history_tokens : int;
  context_window_tokens : int;
  compaction_threshold_tokens : int;
  max_messages_per_session : int;
  compacted_before_turn : bool;
}

type background_task_summary = {
  id : int;
  runner : string;
  repo_label : string;
  branch : string;
  status : string;
  health : string;
  elapsed : string;
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
  daemon_uptime_line : string option;
  background_tasks : background_task_summary list;
  context_usage : context_usage option;
  tunnel_status_line : string option;
  task_tree_summary : string option;
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
    ("- Heartbeat routing enabled for this session: "
    ^ yes_no details.heartbeat_routing_applies);
  add ("- Effective workspace: " ^ details.effective_workspace);
  add ("- Workspace only: " ^ yes_no details.workspace_only);
  add
    (Printf.sprintf "- Shell sandboxed: %s (requested=%s effective=%s)"
       (yes_no details.shell_is_sandboxed)
       details.sandbox_backend_requested details.sandbox_backend_effective);
  add ("- Shell policy: " ^ details.shell_policy_summary);
  add ("- Shell visible roots: " ^ details.shell_visible_roots_summary);
  (match details.daemon_uptime_line with Some line -> add line | None -> ());
  (match details.tunnel_status_line with Some line -> add line | None -> ());
  (match details.context_usage with
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
           (yes_no usage.compacted_before_turn)));
  (match details.background_tasks with
  | [] -> add "- Background tasks: none running"
  | tasks ->
      add
        ("- Background tasks:"
        ^ String.concat ""
            (List.map
               (fun task ->
                 let health_suffix =
                   if task.health = "-" || task.health = "" then ""
                   else Printf.sprintf " health=%s" task.health
                 in
                 Printf.sprintf "\n  - #%d %s %s %s%s repo=%s branch=%s" task.id
                   task.runner task.status task.elapsed health_suffix
                   task.repo_label task.branch)
               tasks)));
  match details.task_tree_summary with
  | None -> ()
  | Some summary ->
      let compacted =
        match details.context_usage with
        | Some u -> u.compacted_before_turn
        | None -> false
      in
      if compacted then begin
        add "";
        add "## Post-Compaction Orientation";
        add
          "Your conversation history was compacted. Your task tree (below) \
           reflects your current work plan."
      end;
      add "";
      add "## Current Tasks";
      add summary

let build_runtime_context ~(config : Runtime_config.t) ?(force_full = false)
    ?(md_skills : (string * string) list = []) ?details ?effective_cwd () =
  if (not force_full) && not config.prompt.dynamic_enabled then None
  else
    let lines = ref [] in
    let add line = lines := line :: !lines in
    if force_full || config.prompt.include_datetime_section then begin
      add ("- Current UTC: " ^ now_utc_iso8601 ());
      add ("- Local time: " ^ now_local_iso8601 ());
      add ("- Local timezone: " ^ local_timezone_label ())
    end;
    if force_full || config.prompt.include_runtime_section then begin
      let cwd =
        match effective_cwd with Some c -> c | None -> Sys.getcwd ()
      in
      add ("- Current working directory: " ^ cwd);
      add ("- Directory contents: " ^ list_cwd_entries ?effective_cwd ());
      add ("- Workspace root: " ^ Runtime_config.effective_workspace config);
      (match find_git_root_and_dir cwd with
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
    if md_skills <> [] then begin
      add "";
      add "## Available Skills";
      add
        "Use the `use_skill` tool or `/skill-name` to activate a skill. \
         Reference @skill-name in messages to auto-attach.";
      List.iter
        (fun (name, description) ->
          add (Printf.sprintf "- %s: %s" name description))
        md_skills
    end;
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

let workspace_doc_content_digests ~(config : Runtime_config.t)
    ?(session_type = "private") () =
  let blocks = workspace_doc_blocks ~config ~session_type () in
  List.filter_map
    (fun (_name, content) ->
      if content = "" then None
      else Some (Digest.to_hex (Digest.string content)))
    blocks

let project_doc_filenames = [ "CLAUDE.md"; "AGENTS.md" ]

type project_docs_result = {
  content : string option;
  digests : string list;
  git_root : string option;
}

(* Resolve the directory project docs (CLAUDE.md/AGENTS.md) are loaded from.
   For room/thread sessions [effective_cwd] is the per-room workspace subfolder
   (B706): prefer its enclosing git root, but a plain (non-git) room folder
   still autoloads its own docs. For the main session (no [effective_cwd]) the
   prior process-cwd, git-root-only behavior is preserved unchanged. *)
let resolve_project_doc_dir ?effective_cwd () =
  match effective_cwd with
  | Some dir when dir <> "" -> (
      match find_git_root_and_dir dir with
      | Some (root, _git_dir) -> Some (root, true)
      | None ->
          if try Sys.is_directory dir with _ -> false then Some (dir, false)
          else None)
  | _ -> (
      match find_git_root_and_dir (Sys.getcwd ()) with
      | Some (root, _git_dir) -> Some (root, true)
      | None -> None)

let build_project_docs_message ~(config : Runtime_config.t) ?effective_cwd
    ~ws_doc_digests () =
  if not config.prompt.include_project_docs then
    { content = None; digests = []; git_root = None }
  else
    match resolve_project_doc_dir ?effective_cwd () with
    | None -> { content = None; digests = []; git_root = None }
    | Some (git_root, from_git_root) -> (
        let budget = ref config.prompt.max_project_doc_chars in
        let seen_digests = Hashtbl.create 4 in
        List.iter (fun d -> Hashtbl.replace seen_digests d true) ws_doc_digests;
        let collected_digests = ref [] in
        let blocks = ref [] in
        List.iter
          (fun filename ->
            let path = Filename.concat git_root filename in
            if Sys.file_exists path && !budget > 0 then begin
              let raw = read_file_limited path !budget in
              if raw <> "" then begin
                let digest = Digest.to_hex (Digest.string raw) in
                if not (Hashtbl.mem seen_digests digest) then begin
                  Hashtbl.replace seen_digests digest true;
                  collected_digests := digest :: !collected_digests;
                  budget := !budget - min !budget (String.length raw);
                  blocks := (filename, raw) :: !blocks
                end
              end
            end)
          project_doc_filenames;
        let total_chars =
          List.fold_left (fun acc (_, c) -> acc + String.length c) 0 !blocks
        in
        if total_chars > config.prompt.project_doc_warn_chars then
          Logs.warn (fun m ->
              m
                "Project docs total %d chars exceeds warning threshold %d; \
                 consider trimming CLAUDE.md/AGENTS.md"
                total_chars config.prompt.project_doc_warn_chars);
        let digests = List.rev !collected_digests in
        match !blocks with
        | [] -> { content = None; digests; git_root = Some git_root }
        | blocks_rev ->
            let lines = ref [] in
            let add s = lines := s :: !lines in
            if from_git_root then begin
              add "## Project Instructions (auto-loaded from git root)";
              add (Printf.sprintf "Repository root: %s" git_root)
            end
            else begin
              add
                "## Project Instructions (auto-loaded from workspace directory)";
              add (Printf.sprintf "Directory: %s" git_root)
            end;
            add "";
            List.iter
              (fun (name, raw) ->
                add ("### " ^ name);
                add raw;
                add "")
              (List.rev blocks_rev);
            add
              "**Note:** These project instructions are refreshed each turn. \
               Subdirectory-specific CLAUDE.md/AGENTS.md files will be loaded \
               when you first access files in those directories.";
            let content_str = String.concat "\n" (List.rev !lines) in
            { content = Some content_str; digests; git_root = Some git_root })

let tools_block tool_registry =
  match tool_registry with
  | None -> []
  | Some registry ->
      Tool_registry.list registry
      |> List.sort (fun (a : Tool.t) (b : Tool.t) ->
          String.compare a.name b.name)
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
    ?(channel_type = "dm") ?(workspace = None) ?(scheduled_jobs = [])
    ?agent_template ?room_profile_system_prompt
    ?(instruction_items : Runtime_config.effective_instruction_item list = [])
    () =
  (* Determine the effective system prompt source. Room profile overrides
     agent_template per the acceptance chain:
     session override > room profile > channel default > global. *)
  let effective_system_prompt () =
    match room_profile_system_prompt with
    | Some s when s <> "" -> Some s
    | _ -> (
        match agent_template with
        | Some (tmpl : Agent_template.t) when tmpl.system_prompt <> "" ->
            Some tmpl.system_prompt
        | _ ->
            if config.agent_defaults.system_prompt <> "" then
              Some config.agent_defaults.system_prompt
            else None)
  in
  if not config.prompt.dynamic_enabled then
    match effective_system_prompt () with Some s -> s | None -> base_prompt
  else
    let lines = ref [] in
    let add s = lines := s :: !lines in
    let ws = Runtime_config.effective_workspace config in
    (* Room profile system prompt > agent template system prompt >
       agent_defaults system prompt > base_prompt *)
    (match effective_system_prompt () with
    | Some prompt ->
        add prompt;
        add ""
    | None ->
        add base_prompt;
        add "");
    (* Add agent template goal/backstory when the template is present and
       the effective prompt came from the template or base_prompt (not room
       profile override — room profile provides its own complete prompt). *)
    (match (room_profile_system_prompt, agent_template) with
    | None, Some (tmpl : Agent_template.t) -> begin
        if tmpl.goal <> "" then begin
          add "## Agent Goal";
          add tmpl.goal;
          add ""
        end;
        if tmpl.backstory <> "" then begin
          add "## Agent Backstory";
          add tmpl.backstory;
          add ""
        end
      end
    | _ -> ());
    if config.prompt.include_autonomy_section then begin
      add "## Autonomous Operation";
      add
        "- You are designed for autonomous, long-running operation. Prioritize \
         continuous execution and completing tasks fully rather than stopping \
         early or waiting for confirmation.";
      add
        "- Messages injected during your workflow are steering information or \
         side-questions. Incorporate them into your ongoing work without \
         interrupting your current task, unless they explicitly ask you to \
         stop, pause, or wait for a response.";
      add
        "- Do not treat an incoming message as a signal to abandon or \
         deprioritize your current work. Acknowledge it, address it if brief, \
         or note it for follow-up, then continue.";
      add
        "- When approaching context limits, save progress and state rather \
         than wrapping up prematurely. Your context may be compacted \
         automatically, allowing you to continue working.";
      add
        "- Use `send_message` to communicate progress, results, blockers, and \
         milestones asynchronously. This allows you to notify the user without \
         stopping work. Prefer sending a message and continuing over stopping \
         to wait for acknowledgment.";
      add
        "- When you have the tools and context to make progress, do so. \
         Default to action over asking permission for internal, reversible \
         work. Reserve questions for genuinely ambiguous goals or irreversible \
         external actions.";
      add ""
    end;
    if config.prompt.include_workspace_section then begin
      match agent_template with
      | None ->
          add "## Workspace Context";
          add ("Root: " ^ ws);
          let docs =
            workspace_doc_blocks ~config ~session_type:channel_type ()
          in
          if docs = [] then
            add
              "No workspace identity files found. Operating with defaults only."
          else begin
            add "";
            add
              "The following files define your identity, behavioral protocol, \
               and local context. EGO.md governs who you are. AGENTS.md \
               governs how you operate. All other files provide situational \
               context. When instructions conflict, EGO.md takes precedence, \
               then AGENTS.md, then the rest in order of appearance.";
            add "";
            List.iter
              (fun (name, content) ->
                add ("### " ^ name);
                add content;
                add "")
              docs;
            add
              "**Note:** The workspace files above are already injected into \
               this prompt and refreshed every turn. Do not re-read them with \
               file_read unless you have a concrete reason to check for \
               mid-session changes or need content beyond the truncation \
               limit."
          end
      | Some _ ->
          add "## Workspace Context";
          add ("Root: " ^ ws);
          add
            "(Workspace identity files suppressed for named agents. Project \
             instructions from CLAUDE.md/AGENTS.md are provided separately.)"
    end;
    (* Layered instructions from access bundles. Ordered by scope level:
       default → workspace → channel → room/profile. Each instruction
       carries provenance labels showing which scope/bundle contributed it.
       Profile system prompts are excluded to avoid duplication with the
       room_profile_system_prompt already injected above. *)
    if instruction_items <> [] then begin
      add "";
      add "## Layered Instructions";
      add
        "The following instructions are resolved from access scopes in \
         precedence order. Higher-precedence layers override lower ones for \
         conflicting directives.";
      add "";
      let layer_order = [ "default"; "workspace"; "channel"; "room" ] in
      let by_layer :
          (string, Runtime_config.effective_instruction_item list) Hashtbl.t =
        Hashtbl.create 8
      in
      List.iter
        (fun (item : Runtime_config.effective_instruction_item) ->
          let layer =
            match item.provenance with p :: _ -> p.layer | [] -> "unknown"
          in
          let existing =
            Option.value ~default:[] (Hashtbl.find_opt by_layer layer)
          in
          Hashtbl.replace by_layer layer (item :: existing))
        instruction_items;
      (* Reverse each layer's list to preserve original order within layer *)
      Hashtbl.filter_map_inplace (fun _ items -> Some (List.rev items)) by_layer;
      List.iter
        (fun layer ->
          match Hashtbl.find_opt by_layer layer with
          | None | Some [] -> ()
          | Some items -> begin
              add
                (Printf.sprintf "### Layer: %s" (String.uppercase_ascii layer));
              add "";
              List.iter
                (fun (item : Runtime_config.effective_instruction_item) ->
                  let provenance_label =
                    match item.provenance with
                    | p :: _ -> Printf.sprintf "[%s/%s]" p.source_id p.field
                    | [] -> "[unknown]"
                  in
                  add provenance_label;
                  add item.instruction.text;
                  add "")
                items
            end)
        layer_order;
      (* Render any layers not in the predefined order *)
      let rendered = Hashtbl.create 8 in
      List.iter (fun layer -> Hashtbl.replace rendered layer true) layer_order;
      Hashtbl.iter
        (fun layer items ->
          if (not (Hashtbl.mem rendered layer)) && items <> [] then begin
            add (Printf.sprintf "### Layer: %s" (String.uppercase_ascii layer));
            add "";
            List.iter
              (fun (item : Runtime_config.effective_instruction_item) ->
                let provenance_label =
                  match item.provenance with
                  | p :: _ -> Printf.sprintf "[%s/%s]" p.source_id p.field
                  | [] -> "[unknown]"
                in
                add provenance_label;
                add item.instruction.text;
                add "")
              items
          end)
        by_layer
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
    add
      "- Scope interventions to what was requested, but do not hesitate to fix \
       obvious adjacent issues or continue naturally related work.";
    if config.prompt.include_tools_section then begin
      add "";
      add "## Tools";
      let tool_lines = tools_block tool_registry in
      if tool_lines = [] then add "- No tools registered."
      else List.iter add tool_lines;
      add "";
      add "Example tool call:";
      add "  shell_exec(command=\"ls -la\", head=100, tail=100)"
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

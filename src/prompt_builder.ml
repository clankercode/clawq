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

let workspace_doc_blocks ~(config : Runtime_config.t) =
  let workspace = Runtime_config.effective_workspace config in
  let ego_path = Filename.concat workspace "EGO.md" in
  let ego_exists = Sys.file_exists ego_path in
  let budget = ref config.prompt.max_workspace_total_chars in
  let blocks = ref [] in
  List.iter
    (fun file ->
      if
        safe_prompt_filename file && !budget > 0
        && not (file = "SOUL.md" && ego_exists)
      then
        let path = Filename.concat workspace file in
        if Sys.file_exists path then
          let cap = min !budget config.prompt.max_workspace_file_chars in
          let content = read_file_limited path cap in
          if content <> "" then begin
            budget := !budget - min !budget (String.length content);
            blocks := (file, content) :: !blocks
          end)
    config.prompt.workspace_files;
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
      let docs = workspace_doc_blocks ~config in
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
    if config.prompt.include_runtime_section then begin
      add "";
      add "## Runtime";
      add
        (Printf.sprintf "- Provider preference: %s"
           (match
              Runtime_config.effective_primary_provider config.agent_defaults
            with
           | Some p -> p
           | None -> "(automatic)"));
      add
        (Printf.sprintf "- Model preference: %s"
           (Runtime_config.effective_primary_model config.agent_defaults));
      add (Printf.sprintf "- Temperature: %.2f" config.default_temperature);
      add (Printf.sprintf "- Tools enabled: %b" config.security.tools_enabled);
      add (Printf.sprintf "- Workspace only: %b" config.security.workspace_only)
    end;
    if config.prompt.include_datetime_section then begin
      add "";
      add "## DateTime";
      add ("- Current UTC: " ^ now_utc_iso8601 ())
    end;
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

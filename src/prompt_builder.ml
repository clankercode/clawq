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

let build ~(config : Runtime_config.t) ~tool_registry =
  if not config.prompt.dynamic_enabled then config.agent_defaults.system_prompt
  else
    let lines = ref [] in
    let add s = lines := s :: !lines in
    let workspace = Runtime_config.effective_workspace config in
    add config.agent_defaults.system_prompt;
    add "";
    add "## Execution Contract";
    add "- Prefer direct execution over speculative discussion.";
    add "- Be precise, truthful, and explicit about verification status.";
    add "- Minimize diff size while preserving readability and maintainability.";
    if config.prompt.include_safety_section then begin
      add "";
      add "## Safety";
      add "- Never reveal secrets, tokens, or private data.";
      add "- Ask before destructive or irreversible actions.";
      add "- Respect workspace boundaries and configured security policies."
    end;
    if config.prompt.include_workspace_section then begin
      add "";
      add "## Workspace";
      add ("- Root: " ^ workspace);
      add "- Treat workspace files as authoritative local context.";
      let docs = workspace_doc_blocks ~config in
      if docs = [] then add "- No workspace identity docs found."
      else begin
        add "";
        add "### Workspace Docs";
        List.iter
          (fun (name, content) ->
            add ("#### " ^ name);
            add content;
            add "")
          docs
      end
    end;
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
    String.concat "\n" (List.rev !lines)

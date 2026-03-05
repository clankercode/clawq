let templates : (string * string) list =
  [
    ( "EGO.md",
      "# EGO\n\n"
      ^ "## Mission\n"
      ^ "Ship precise, minimal, verified engineering changes aligned with this workspace.\n\n"
      ^ "## Defaults\n"
      ^ "- Read context before editing.\n"
      ^ "- Prefer concrete execution over speculation.\n"
      ^ "- Keep diffs tight and maintainable.\n"
      ^ "- Verify with tests/checks that match changed scope.\n\n"
      ^ "## Safety\n"
      ^ "- Do not leak secrets or private data.\n"
      ^ "- Ask before destructive, irreversible, or externally visible actions.\n"
      ^ "- Respect workspace boundaries and existing project conventions.\n" );
    ( "AGENTS.md",
      "# AGENTS\n\n"
      ^ "This workspace is your operating context.\n\n"
      ^ "## Session Start\n"
      ^ "1. Read EGO.md\n"
      ^ "2. Read USER.md and IDENTITY.md if present\n"
      ^ "3. Review TOOLS.md for local specifics\n"
      ^ "4. Execute the requested task directly unless blocked\n\n"
      ^ "## Working Rules\n"
      ^ "- Be concise by default.\n"
      ^ "- Prefer deterministic, testable changes.\n"
      ^ "- Preserve unrelated local modifications.\n" );
    ( "USER.md",
      "# USER\n\n"
      ^ "Describe user preferences that affect implementation choices, communication style, and risk tolerance.\n" );
    ( "IDENTITY.md",
      "# IDENTITY\n\n"
      ^ "Assistant identity for this workspace (optional).\n" );
    ( "TOOLS.md",
      "# TOOLS\n\n"
      ^ "Workspace-local notes: hostnames, service names, scripts, and operational caveats.\n" );
    ( "HEARTBEAT.md",
      "# HEARTBEAT\n\n"
      ^ "Keep empty unless periodic checks are required.\n" );
    ( "BOOTSTRAP.md",
      "# BOOTSTRAP\n\n"
      ^ "Use this file for first-run setup guidance, then delete once stable.\n" );
  ]

let ensure_dir path =
  let rec loop p =
    if p = "" || p = "/" then ()
    else if Sys.file_exists p then ()
    else
      let parent = Filename.dirname p in
      if parent <> p then loop parent;
      (try Unix.mkdir p 0o755 with _ -> ())
  in
  loop path

let write_if_missing ~workspace (name, content) =
  let path = Filename.concat workspace name in
  if Sys.file_exists path then false
  else
    let oc = open_out path in
    output_string oc content;
    output_char oc '\n';
    close_out oc;
    true

let scaffold ~workspace =
  ensure_dir workspace;
  let created =
    List.fold_left
      (fun acc t -> if write_if_missing ~workspace t then fst t :: acc else acc)
      [] templates
  in
  List.rev created

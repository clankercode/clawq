(* setup_prompt.ml — Interactive setup wizard for prompt configuration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_positive_int s =
  match int_of_string_opt s with
  | Some v when v > 0 -> Ok s
  | Some _ -> Error "Value must be a positive integer."
  | None -> Error "Value must be a valid integer."

let build_prompt_json ~dynamic_enabled ~include_tools_section
    ~include_safety_section ~include_workspace_section ~include_runtime_section
    ~include_datetime_section ~include_autonomy_section ~workspace_files
    ~max_workspace_file_chars ~max_workspace_total_chars =
  `Assoc
    [
      ( "prompt",
        `Assoc
          [
            ("dynamic_enabled", `Bool dynamic_enabled);
            ("include_tools_section", `Bool include_tools_section);
            ("include_safety_section", `Bool include_safety_section);
            ("include_workspace_section", `Bool include_workspace_section);
            ("include_runtime_section", `Bool include_runtime_section);
            ("include_datetime_section", `Bool include_datetime_section);
            ("include_autonomy_section", `Bool include_autonomy_section);
            ( "workspace_files",
              `List (List.map (fun s -> `String s) workspace_files) );
            ("max_workspace_file_chars", `Int max_workspace_file_chars);
            ("max_workspace_total_chars", `Int max_workspace_total_chars);
          ] );
    ]

let post_setup_instructions =
  {|
  Prompt configuration setup:

    1. dynamic_enabled: When true, clawq builds the system prompt dynamically
       from configured sections. When false, only the static system_prompt is used.
    2. include_tools_section: Include tool descriptions in the system prompt.
    3. include_safety_section: Include safety/policy instructions.
    4. include_workspace_section: Include workspace file contents (CLAUDE.md etc).
    5. include_runtime_section: Include runtime info (model, version, etc).
    6. include_datetime_section: Include current date/time.
    7. include_autonomy_section: Include autonomy/continuation instructions.
    8. workspace_files: Files loaded into the workspace context section.
       Comma-separated. e.g. CLAUDE.md,README.md
    9. max_workspace_file_chars: Max characters read from each workspace file.
   10. max_workspace_total_chars: Max total characters across all workspace files.

  After saving:

    - Restart the daemon: clawq daemon restart
    - Verify: clawq status

  Full documentation: https://clawq.org/prompt/
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    Some cfg.prompt
  with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let default = Runtime_config.default_prompt in
  let get_p f = match existing with Some c -> f c | None -> f default in
  let dynamic_enabled =
    Setup_tui.make_bool_field ~key:"d" ~label:"Dynamic prompt enabled"
      ~menu_label:"Toggle dynamic prompt"
      ~description:"Build system prompt dynamically from sections."
      ~default:(get_p (fun c -> c.Runtime_config.dynamic_enabled))
      ()
  in
  let include_tools_section =
    Setup_tui.make_bool_field ~key:"t" ~label:"Include tools section"
      ~menu_label:"Toggle tools section"
      ~description:"Include tool descriptions in system prompt."
      ~default:(get_p (fun c -> c.Runtime_config.include_tools_section))
      ()
  in
  let include_safety_section =
    Setup_tui.make_bool_field ~key:"sf" ~label:"Include safety section"
      ~menu_label:"Toggle safety section"
      ~description:"Include safety/policy instructions in system prompt."
      ~default:(get_p (fun c -> c.Runtime_config.include_safety_section))
      ()
  in
  let include_workspace_section =
    Setup_tui.make_bool_field ~key:"w" ~label:"Include workspace section"
      ~menu_label:"Toggle workspace section"
      ~description:
        "Include workspace file contents (CLAUDE.md, README.md, etc)."
      ~default:(get_p (fun c -> c.Runtime_config.include_workspace_section))
      ()
  in
  let include_runtime_section =
    Setup_tui.make_bool_field ~key:"r" ~label:"Include runtime section"
      ~menu_label:"Toggle runtime section"
      ~description:
        "Include runtime info (model, version, config) in system prompt."
      ~default:(get_p (fun c -> c.Runtime_config.include_runtime_section))
      ()
  in
  let include_datetime_section =
    Setup_tui.make_bool_field ~key:"dt" ~label:"Include datetime section"
      ~menu_label:"Toggle datetime section"
      ~description:"Include current date and time in system prompt."
      ~default:(get_p (fun c -> c.Runtime_config.include_datetime_section))
      ()
  in
  let include_autonomy_section =
    Setup_tui.make_bool_field ~key:"au" ~label:"Include autonomy section"
      ~menu_label:"Toggle autonomy section"
      ~description:
        "Include autonomy/continuation instructions in system prompt."
      ~default:(get_p (fun c -> c.Runtime_config.include_autonomy_section))
      ()
  in
  let workspace_files =
    Setup_tui.make_list_field ~key:"wf" ~label:"Workspace files"
      ~menu_label:"Set workspace files (comma-separated)"
      ~description:
        "Files loaded into workspace context section of system prompt."
      ~default:(get_p (fun c -> c.Runtime_config.workspace_files))
      ()
  in
  let max_workspace_file_chars =
    Setup_tui.make_int_field ~key:"mf" ~label:"Max chars/file"
      ~menu_label:"Set max chars per workspace file"
      ~description:"Maximum characters read from each workspace file."
      ~validate:validate_positive_int
      ~default:(get_p (fun c -> c.Runtime_config.max_workspace_file_chars))
      ()
  in
  let max_workspace_total_chars =
    Setup_tui.make_int_field ~key:"mt" ~label:"Max total workspace chars"
      ~menu_label:"Set max total workspace chars"
      ~description:"Maximum total characters across all workspace files."
      ~validate:validate_positive_int
      ~default:(get_p (fun c -> c.Runtime_config.max_workspace_total_chars))
      ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " Prompt Configuration ";
      docs_url = "https://clawq.org/prompt/";
      fields =
        [
          dynamic_enabled;
          include_tools_section;
          include_safety_section;
          include_workspace_section;
          include_runtime_section;
          include_datetime_section;
          include_autonomy_section;
          workspace_files;
          max_workspace_file_chars;
          max_workspace_total_chars;
        ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_prompt_json
            ~dynamic_enabled:(Setup_tui.get_bool dynamic_enabled)
            ~include_tools_section:(Setup_tui.get_bool include_tools_section)
            ~include_safety_section:(Setup_tui.get_bool include_safety_section)
            ~include_workspace_section:
              (Setup_tui.get_bool include_workspace_section)
            ~include_runtime_section:
              (Setup_tui.get_bool include_runtime_section)
            ~include_datetime_section:
              (Setup_tui.get_bool include_datetime_section)
            ~include_autonomy_section:
              (Setup_tui.get_bool include_autonomy_section)
            ~workspace_files:(Setup_tui.get_str_list workspace_files)
            ~max_workspace_file_chars:
              (Setup_tui.get_int max_workspace_file_chars)
            ~max_workspace_total_chars:
              (Setup_tui.get_int max_workspace_total_chars));
      pre_save_check = (fun () -> Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec

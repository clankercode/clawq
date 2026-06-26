(* setup_agents.ml — Interactive setup wizard for agent templates *)

let validate_name s =
  if Agent_template.is_valid_name s then Ok s
  else
    Error "Name must be lowercase alphanumeric, hyphens, underscores (max 64)"

let write_template_file path (t : Agent_template.t) =
  let content = Agent_template.to_frontmatter_string t in
  let oc = open_out path in
  Fun.protect
    (fun () -> output_string oc content)
    ~finally:(fun () -> close_out oc)

let offer_editor path =
  let open_it =
    Setup_common.prompt_yn ~prompt:"Open system prompt in editor?" ~default:true
      ()
  in
  if open_it then
    let editor = Setup_common.find_editor () in
    ignore (Sys.command (Printf.sprintf "%s %s" editor (Filename.quote path)))

let run () =
  let name_field =
    Setup_tui.make_field ~key:"1" ~label:"Name" ~menu_label:"Agent name"
      ~description:"Lowercase identifier (e.g., my-agent)"
      ~validate:validate_name ()
  in
  let description_field =
    Setup_tui.make_field ~key:"2" ~label:"Description" ~menu_label:"Description"
      ~description:"Short description of what this agent does" ()
  in
  let role_field =
    Setup_tui.make_choice_field ~key:"3" ~label:"Role" ~menu_label:"Role"
      ~choices:(Agent_template.all_builtin_roles @ [ "custom" ])
      ~description:"Agent archetype role" ~default:"coder" ()
  in
  let model_field =
    Setup_tui.make_field ~key:"4" ~label:"Model" ~menu_label:"Model override"
      ~description:"Leave blank for default model" ()
  in
  let max_iters_field =
    Setup_tui.make_int_field ~key:"5" ~label:"Max tool iterations"
      ~menu_label:"Max tool iterations"
      ~description:"Maximum tool call iterations per turn" ~default:20 ()
  in
  let allowed_tools_field =
    Setup_tui.make_list_field ~key:"6" ~label:"Allowed tools"
      ~menu_label:"Allowed tools (comma-sep)"
      ~description:"Tool allowlist (empty = all tools allowed)" ()
  in
  let disallowed_tools_field =
    Setup_tui.make_list_field ~key:"7" ~label:"Disallowed tools"
      ~menu_label:"Disallowed tools (comma-sep)"
      ~description:"Tool denylist (applied after allowlist)" ()
  in
  let tool_search_field =
    Setup_tui.make_bool_field ~key:"8" ~label:"Tool search enabled"
      ~menu_label:"Tool search enabled"
      ~description:"Allow agent to discover deferred tools" ~default:true ()
  in
  let reasoning_field =
    Setup_tui.make_choice_field ~key:"9" ~label:"Reasoning effort"
      ~menu_label:"Reasoning effort"
      ~choices:[ "low"; "medium"; "high"; "" ]
      ~description:"Reasoning effort level (blank for default)" ~default:"" ()
  in
  let all_fields =
    [
      name_field;
      description_field;
      role_field;
      model_field;
      max_iters_field;
      allowed_tools_field;
      disallowed_tools_field;
      tool_search_field;
      reasoning_field;
    ]
  in
  let build_template_from_fields () =
    let name = Setup_tui.get_str name_field in
    let description = Setup_tui.get_str description_field in
    let role = Agent_template.role_of_string (Setup_tui.get_str role_field) in
    let model =
      match Setup_tui.get_str model_field with "" -> None | s -> Some s
    in
    let max_tool_iterations =
      let v = Setup_tui.get_int max_iters_field in
      if v > 0 then Some v else None
    in
    let allowed_tools = Setup_tui.get_str_list allowed_tools_field in
    let disallowed_tools = Setup_tui.get_str_list disallowed_tools_field in
    let tool_search_enabled = Some (Setup_tui.get_bool tool_search_field) in
    let reasoning_effort =
      match Setup_tui.get_str reasoning_field with "" -> None | s -> Some s
    in
    {
      Agent_template.name;
      description;
      role;
      goal = "";
      backstory = "";
      system_prompt =
        "# " ^ name
        ^ "\n\nDescribe this agent's behavior and instructions here.";
      model;
      max_tool_iterations;
      allowed_tools;
      disallowed_tools;
      tool_search_enabled;
      reasoning_effort;
      cwd = None;
      source = User_file "";
      metadata = [];
    }
  in
  let copy_from_builtin () =
    let builtins = Agent_template_builtins.all in
    Printf.printf "\n  Available built-in templates:\n";
    List.iteri
      (fun i (t : Agent_template.t) ->
        Printf.printf "    %d. %s — %s\n" (i + 1) t.name t.description)
      builtins;
    Printf.printf "\n";
    let input =
      Setup_common.prompt_string ~prompt:"Select template number" ~default:"" ()
    in
    match int_of_string_opt input with
    | Some n when n >= 1 && n <= List.length builtins ->
        let tmpl = List.nth builtins (n - 1) in
        name_field.value := tmpl.name ^ "-custom";
        description_field.value := tmpl.description;
        role_field.value := Agent_template.role_to_string tmpl.role;
        (match tmpl.model with Some m -> model_field.value := m | None -> ());
        (match tmpl.max_tool_iterations with
        | Some n -> max_iters_field.value := string_of_int n
        | None -> ());
        if tmpl.allowed_tools <> [] then
          Setup_tui.set_str_list allowed_tools_field tmpl.allowed_tools;
        if tmpl.disallowed_tools <> [] then
          Setup_tui.set_str_list disallowed_tools_field tmpl.disallowed_tools;
        Setup_common.print_success
          (Printf.sprintf "Copied settings from '%s'" tmpl.name)
    | _ -> Setup_common.print_warning "Invalid selection."
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = "Agent Template";
      docs_url = "https://clawq.org/docs/agents";
      fields = all_fields;
      extra_actions =
        [ ("c", "Copy from built-in template", copy_from_builtin) ];
      build_json =
        (fun () ->
          (* We write .md files, not config.json — return empty *)
          `Assoc []);
      pre_save_check =
        (fun () ->
          let name = Setup_tui.get_str name_field in
          if name = "" then Error "Name is required"
          else if not (Agent_template.is_valid_name name) then
            Error "Invalid name format"
          else if Setup_tui.get_str description_field = "" then
            Error "Description is required"
          else Ok ());
      post_instructions =
        (fun () ->
          "\n\
           Agent Template Setup\n\
           ====================\n\n\
           1. Configure the agent metadata fields above\n\
           2. Save to create the template .md file in ~/.clawq/agents/\n\
           3. Edit the system prompt in your text editor\n\
           4. Use 'clawq agents list' to verify the template is discovered\n\
           5. Use 'clawq agents bind <pattern> <name>' to route messages to it\n");
    }
  in
  (* Override save behavior: write .md file instead of config.json *)
  match Setup_common.check_tty () with
  | Error e -> e
  | Ok () ->
      let dirty = ref false in
      let quit = ref false in
      while not !quit do
        Setup_tui.draw_wizard_dashboard spec;
        let options =
          List.map (fun f -> (f.Setup_tui.key, f.menu_label)) spec.fields
          @ List.map (fun (k, l, _) -> (k, l)) spec.extra_actions
          @ [ ("h", "Show setup instructions") ]
          @
          if !dirty then [ ("s", Setup_common.bold "Save agent template") ]
          else []
        in
        let choice =
          Setup_common.prompt_menu ~title:"Actions" ~options
            ~shortcut_exit:"q/Enter" ()
        in
        let key = String.lowercase_ascii choice in
        let field_match =
          List.find_opt (fun f -> f.Setup_tui.key = key) spec.fields
        in
        let extra_match =
          List.find_opt (fun (k, _, _) -> k = key) spec.extra_actions
        in
        match (field_match, extra_match) with
        | Some f, _ ->
            if Setup_tui.prompt_for_field f then dirty := true;
            Setup_common.press_enter_to_continue ()
        | _, Some (_, _, handler) ->
            handler ();
            dirty := true;
            Setup_common.press_enter_to_continue ()
        | None, None -> (
            match key with
            | "q" | "" ->
                if !dirty then begin
                  let save =
                    Setup_common.prompt_yn
                      ~prompt:"You have unsaved changes. Save before exiting?"
                      ~default:true ()
                  in
                  if save then begin
                    match spec.pre_save_check () with
                    | Error e ->
                        Setup_common.print_warning e;
                        Setup_common.press_enter_to_continue ()
                    | Ok () ->
                        let tmpl = build_template_from_fields () in
                        let dir = Agent_template.init_dir () in
                        let path = Filename.concat dir (tmpl.name ^ ".md") in
                        (try write_template_file path tmpl
                         with exn ->
                           Setup_common.print_error
                             (Printf.sprintf "Failed to write: %s"
                                (Printexc.to_string exn)));
                        Setup_common.print_success
                          (Printf.sprintf "Saved to %s" path);
                        offer_editor path;
                        quit := true
                  end
                  else quit := true
                end
                else quit := true
            | "h" ->
                Printf.printf "%s" (spec.post_instructions ());
                Setup_common.press_enter_to_continue ()
            | "s" ->
                if not !dirty then (
                  Setup_common.print_warning "No changes to save.";
                  Setup_common.press_enter_to_continue ())
                else begin
                  match spec.pre_save_check () with
                  | Error e ->
                      Setup_common.print_warning e;
                      Setup_common.press_enter_to_continue ()
                  | Ok () ->
                      let tmpl = build_template_from_fields () in
                      let dir = Agent_template.init_dir () in
                      let path = Filename.concat dir (tmpl.name ^ ".md") in
                      (try write_template_file path tmpl
                       with exn ->
                         Setup_common.print_error
                           (Printf.sprintf "Failed to write: %s"
                              (Printexc.to_string exn)));
                      Setup_common.print_success
                        (Printf.sprintf "Saved to %s" path);
                      offer_editor path;
                      dirty := false;
                      Setup_common.press_enter_to_continue ()
                end
            | _ ->
                Setup_common.print_warning
                  (Printf.sprintf "Unknown option: %s" key);
                Setup_common.press_enter_to_continue ())
      done;
      if !dirty then "Exited with unsaved changes."
      else "Agent template setup complete."

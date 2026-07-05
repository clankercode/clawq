open Command_bridge_helpers

let cmd_agents args =
  let cfg = get_config () in
  let workspace = Runtime_config.effective_workspace cfg in
  if not (Agent_template.is_cache_initialized ()) then
    ignore (Agent_template.init_cache ~workspace_dir:workspace ());
  match args with
  | [] | [ "list" ] ->
      let all = Agent_template.available_templates () in
      if all = [] then "No agent templates found."
      else
        let lines =
          List.map
            (fun (t : Agent_template.t) ->
              let tag =
                match t.source with
                | Builtin -> "[builtin]"
                | User_file _ -> "[user]"
              in
              Printf.sprintf "  %-14s %-10s %-10s %s" t.name
                (Agent_template.role_to_string t.role)
                tag t.description)
            all
        in
        Printf.sprintf "%-16s %-10s %-10s %s\n" "NAME" "ROLE" "SOURCE"
          "DESCRIPTION"
        ^ String.concat "\n" lines
  | [ "show"; name ] -> (
      match Agent_template.resolve name with
      | None -> Printf.sprintf "Agent template not found: %s" name
      | Some t ->
          let lines = ref [] in
          let add s = lines := s :: !lines in
          add (Printf.sprintf "Name:        %s" t.name);
          add (Printf.sprintf "Description: %s" t.description);
          add
            (Printf.sprintf "Role:        %s"
               (Agent_template.role_to_string t.role));
          add
            (Printf.sprintf "Source:      %s"
               (match t.source with Builtin -> "builtin" | User_file p -> p));
          if t.goal <> "" then add (Printf.sprintf "Goal:        %s" t.goal);
          if t.backstory <> "" then
            add (Printf.sprintf "Backstory:   %s" t.backstory);
          (match t.model with
          | Some m -> add (Printf.sprintf "Model:       %s" m)
          | None -> add "Model:       (default)");
          (match t.max_tool_iterations with
          | Some n -> add (Printf.sprintf "Max iters:   %d" n)
          | None -> add "Max iters:   (default)");
          if t.allowed_tools <> [] then
            add
              (Printf.sprintf "Allowed:     %s"
                 (String.concat ", " t.allowed_tools));
          if t.disallowed_tools <> [] then
            add
              (Printf.sprintf "Disallowed:  %s"
                 (String.concat ", " t.disallowed_tools));
          (match t.tool_search_enabled with
          | Some b -> add (Printf.sprintf "Tool search: %b" b)
          | None -> ());
          (match t.reasoning_effort with
          | Some e -> add (Printf.sprintf "Reasoning:   %s" e)
          | None -> ());
          if t.metadata <> [] then begin
            add "Metadata:";
            List.iter
              (fun (k, v) -> add (Printf.sprintf "  %s: %s" k v))
              t.metadata
          end;
          add "";
          add "--- System Prompt ---";
          let preview =
            if String.length t.system_prompt > 500 then
              String.sub t.system_prompt 0 500 ^ "\n[...truncated]"
            else t.system_prompt
          in
          add preview;
          String.concat "\n" (List.rev !lines))
  | [ "create"; name ] ->
      if not (Agent_template.is_valid_name name) then
        "Invalid name. Use lowercase letters, digits, hyphens, underscores \
         (max 64 chars)."
      else begin
        let dir = Agent_template.init_dir () in
        let path = Filename.concat dir (name ^ ".md") in
        if Sys.file_exists path then
          Printf.sprintf "Template already exists: %s" path
        else begin
          let content =
            Printf.sprintf
              "---\n\
               name: %s\n\
               description: A custom agent template\n\
               role: coder\n\
               goal: Implement tasks effectively\n\
               backstory: You are a focused specialist agent.\n\
               ---\n\n\
               You are the %s agent.\n\n\
               ## Operating Protocol\n\
               1. Read relevant context\n\
               2. Plan the approach\n\
               3. Execute and verify\n\n\
               ## Constraints\n\
               - Follow project conventions\n\
               - Do not add unrequested features\n"
              name name
          in
          let oc = open_out path in
          Fun.protect
            (fun () -> output_string oc content)
            ~finally:(fun () -> close_out oc);
          Printf.sprintf "Created agent template: %s\nEdit it to customize."
            path
        end
      end
  | [ "edit"; name ] -> (
      match Agent_template.resolve name with
      | None -> Printf.sprintf "Agent template not found: %s" name
      | Some t -> (
          match t.source with
          | User_file path ->
              let editor = Setup_common.find_editor () in
              ignore
                (Sys.command
                   (Printf.sprintf "%s %s" editor (Filename.quote path)));
              Printf.sprintf "Opened %s in editor." path
          | Builtin ->
              let dir = Agent_template.init_dir () in
              let path = Filename.concat dir (t.name ^ ".md") in
              let content = Agent_template.to_frontmatter_string t in
              let oc = open_out path in
              Fun.protect
                (fun () -> output_string oc content)
                ~finally:(fun () -> close_out oc);
              let editor = Setup_common.find_editor () in
              ignore
                (Sys.command
                   (Printf.sprintf "%s %s" editor (Filename.quote path)));
              Printf.sprintf "Copied builtin '%s' to %s and opened in editor."
                t.name path))
  | [ "delete"; name ] -> (
      match Agent_template.resolve name with
      | None -> Printf.sprintf "Agent template not found: %s" name
      | Some t -> (
          match t.source with
          | Builtin ->
              Printf.sprintf
                "Cannot delete builtin template '%s'. Use 'agents edit %s' to \
                 create a user override instead."
                name name
          | User_file path ->
              (try Sys.remove path
               with exn ->
                 Printf.printf "Warning: %s\n" (Printexc.to_string exn));
              Printf.sprintf "Deleted agent template: %s" path))
  | "bind" :: pattern :: agent_name :: rest -> (
      let priority =
        match rest with
        | [ "--priority"; n ] -> (
            match int_of_string_opt n with Some p -> p | None -> 0)
        | _ -> 0
      in
      let warning =
        match Agent_template.resolve agent_name with
        | None ->
            Some
              (Printf.sprintf
                 "Warning: agent template '%s' not found. Binding created \
                  anyway."
                 agent_name)
        | Some _ -> None
      in
      let new_binding : Agent_router.binding =
        { pattern; agent_name; priority }
      in
      let existing =
        List.filter
          (fun (b : Agent_router.binding) -> b.pattern <> pattern)
          cfg.agent_bindings
      in
      let bindings = new_binding :: existing in
      let bindings_json =
        `Assoc
          [
            ( "agent_bindings",
              `List
                (List.map
                   (fun (b : Agent_router.binding) ->
                     `Assoc
                       [
                         ("pattern", `String b.pattern);
                         ("agent_name", `String b.agent_name);
                         ("priority", `Int b.priority);
                       ])
                   bindings) );
          ]
      in
      match Setup_common.merge_and_write_config bindings_json with
      | Ok path -> (
          let msg =
            Printf.sprintf "Bound pattern '%s' to agent '%s' (priority %d).\n%s"
              pattern agent_name priority path
          in
          match warning with Some w -> w ^ "\n" ^ msg | None -> msg)
      | Error e -> Printf.sprintf "Failed to write config: %s" e)
  | [ "unbind"; pattern ] -> (
      let remaining =
        List.filter
          (fun (b : Agent_router.binding) -> b.pattern <> pattern)
          cfg.agent_bindings
      in
      if List.length remaining = List.length cfg.agent_bindings then
        Printf.sprintf "No binding found for pattern: %s" pattern
      else
        let bindings_json =
          `Assoc
            [
              ( "agent_bindings",
                `List
                  (List.map
                     (fun (b : Agent_router.binding) ->
                       `Assoc
                         [
                           ("pattern", `String b.pattern);
                           ("agent_name", `String b.agent_name);
                           ("priority", `Int b.priority);
                         ])
                     remaining) );
            ]
        in
        match Setup_common.merge_and_write_config bindings_json with
        | Ok path ->
            Printf.sprintf "Removed binding for pattern '%s'.\n%s" pattern path
        | Error e -> Printf.sprintf "Failed to write config: %s" e)
  | [ "bindings" ] ->
      let bindings = cfg.agent_bindings in
      if bindings = [] then "No agent bindings configured."
      else
        let header =
          Printf.sprintf "%-20s %-20s %s" "PATTERN" "AGENT" "PRIORITY"
        in
        let rows =
          List.map
            (fun (b : Agent_router.binding) ->
              Printf.sprintf "%-20s %-20s %d" b.pattern b.agent_name b.priority)
            bindings
        in
        String.concat "\n" (header :: rows)
  | [ "setup" ] -> Setup_agents.run ()
  | [ "path" ] ->
      let dirs = Agent_template.search_dirs ~workspace_dir:workspace () in
      "Agent template search directories:\n"
      ^ String.concat "\n"
          (List.map
             (fun d ->
               let exists =
                 if Sys.file_exists d then " (exists)" else " (not found)"
               in
               "  " ^ d ^ exists)
             dirs)
  | _ ->
      "Usage: clawq agents \
       <list|show|create|edit|delete|bind|unbind|bindings|setup|path>\n\n\
       Subcommands:\n\
      \  list                     List all agent templates\n\
      \  show <name>              Show full template details\n\
      \  create <name>            Create a new template in ~/.clawq/agents/\n\
      \  edit <name>              Edit template (copies builtin to user dir)\n\
      \  delete <name>            Delete a user template\n\
      \  bind <pattern> <agent>   Bind a routing pattern to an agent\n\
      \  unbind <pattern>         Remove a routing pattern binding\n\
      \  bindings                 List current agent bindings\n\
      \  setup                    Launch interactive setup wizard\n\
      \  path                     Show template search directories"

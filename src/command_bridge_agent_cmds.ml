open Command_bridge_helpers

let cmd_skills args = Command_bridge_shared.cmd_skills ~prog_name:"clawq" args

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
      (* Verify agent exists *)
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

let admin_env_var = "CLAWQ_ADMIN"

let is_admin_cli () =
  match Sys.getenv_opt admin_env_var with
  | Some v -> v = "1" || v = "true"
  | None -> false

let require_admin () =
  if is_admin_cli () then None
  else
    Some
      "Error: this command requires admin privileges. Set CLAWQ_ADMIN=1 in \
       your environment."

let room_profile_deleted (p : Runtime_config.room_profile) =
  String.lowercase_ascii p.status = "deleted"

let room_profile_to_json (p : Runtime_config.room_profile) =
  `Assoc
    ([
       ("id", `String p.id);
       ("model", `String p.model);
       ("system_prompt", `String p.system_prompt);
       ("max_tool_iterations", `Int p.max_tool_iterations);
       ("status", `String p.status);
     ]
    @ (if p.allowed_tools = [] then []
       else
         [
           ( "allowed_tools",
             `List (List.map (fun t -> `String t) p.allowed_tools) );
         ])
    @ (if p.denied_tools = [] then []
       else
         [
           ("denied_tools", `List (List.map (fun t -> `String t) p.denied_tools));
         ])
    @
    match p.display_name with
    | Some name -> [ ("display_name", `String name) ]
    | None -> [])

let room_profile_binding_to_json (b : Runtime_config.room_profile_binding) =
  `Assoc
    [
      ("profile_id", `String b.profile_id);
      ("room", `String b.room);
      ("active", `Bool b.active);
    ]

let write_room_config ~profiles ~bindings =
  Setup_common.merge_and_write_config
    (`Assoc
       [
         ("room_profiles", `List (List.map room_profile_to_json profiles));
         ( "room_profile_bindings",
           `List (List.map room_profile_binding_to_json bindings) );
       ])

let room_matches rooms = function
  | Some room -> List.mem room rooms
  | None -> false

let active_background_count_for_rooms ~db rooms =
  Background_task.init_schema db;
  Background_task.list_tasks ~db
  |> List.filter (fun (t : Background_task.task) ->
      (match t.status with
        | Background_task.Queued | Background_task.Running -> true
        | _ -> false)
      && (room_matches rooms t.session_key || room_matches rooms t.channel_id))
  |> List.length

let active_cron_count_for_rooms ~db rooms =
  Scheduler.init_schema db;
  Scheduler.list_jobs ~db
  |> List.filter (fun (j : Scheduler.job) ->
      j.enabled && List.mem j.session_key rooms)
  |> List.length

let add_room_ids_from_reference acc ?channel_id session_key =
  Room_workspace.room_ids_for_reference ?channel_id session_key @ acc

let add_room_ids_from_origin acc = function
  | None -> acc
  | Some origin_json -> (
      match Room_origin.of_json_string_opt origin_json with
      | None -> acc
      | Some origin -> (
          let acc =
            match (origin.connector, origin.room_id) with
            | Some connector, Some room_id -> (connector ^ ":" ^ room_id) :: acc
            | _ -> acc
          in
          match origin.room_id with
          | Some room_id -> room_id :: acc
          | None -> acc))

let active_task_tree_room_ids ~db =
  Task_tree.init_schema db;
  let stmt =
    Sqlite3.prepare db
      "SELECT session_key, origin_json FROM task_tree WHERE deleted_at IS NULL \
       AND status NOT IN ('done', 'cancelled')"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let refs = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let acc =
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.TEXT key -> add_room_ids_from_reference !refs key
          | _ -> !refs
        in
        refs :=
          match Sqlite3.column stmt 1 with
          | Sqlite3.Data.TEXT origin_json ->
              add_room_ids_from_origin acc (Some origin_json)
          | _ -> acc
      done;
      !refs)

let active_room_workspace_ids ~cfg ~db =
  let configured =
    cfg.Runtime_config.room_profile_bindings
    |> List.filter (fun (b : Runtime_config.room_profile_binding) -> b.active)
    |> List.map (fun (b : Runtime_config.room_profile_binding) -> b.room)
  in
  Background_task.init_schema db;
  let task_refs =
    Background_task.list_tasks ~db
    |> List.fold_left
         (fun acc (t : Background_task.task) ->
           match t.status with
           | Background_task.Queued | Background_task.Running -> (
               let acc = add_room_ids_from_origin acc t.origin_json in
               let acc =
                 match t.session_key with
                 | Some key ->
                     add_room_ids_from_reference acc ?channel_id:t.channel_id
                       key
                 | None -> acc
               in
               match t.channel_id with Some id -> id :: acc | None -> acc)
           | _ -> acc)
         []
  in
  Scheduler.init_schema db;
  let routine_refs =
    Scheduler.list_jobs ~db
    |> List.fold_left
         (fun acc (j : Scheduler.job) ->
           if not j.enabled then acc
           else
             let acc = add_room_ids_from_reference acc j.session_key in
             match j.routine_workspace_id with
             | Some id -> id :: acc
             | None -> acc)
         []
  in
  configured @ task_refs @ routine_refs @ active_task_tree_room_ids ~db
  |> List.sort_uniq String.compare

let room_workspace_paths_for_refs room_ids =
  room_ids
  |> List.map (Room_workspace.workspace_path ~create:false)
  |> List.sort_uniq String.compare

let parse_retention_seconds flags =
  let rec loop retention_days = function
    | [] -> Ok (retention_days *. Room_workspace.seconds_per_day)
    | "--retention-days" :: value :: rest -> (
        match float_of_string_opt value with
        | Some days when days >= 0.0 -> loop days rest
        | _ -> Error "--retention-days must be a non-negative number")
    | flag :: _ -> Error (Printf.sprintf "unknown rooms gc flag: %s" flag)
  in
  loop Room_workspace.default_retention_days flags

let format_gc_section title entries =
  let lines =
    match entries with
    | [] -> [ Printf.sprintf "%s: (none)" title ]
    | _ ->
        Printf.sprintf "%s:" title
        :: List.map
             (fun (entry : Room_workspace.gc_entry) ->
               Printf.sprintf "- %s (%s)" entry.path
                 (Room_workspace.gc_reason_to_string entry.reason))
             entries
  in
  String.concat "\n" lines

let format_gc_result retention_seconds (result : Room_workspace.gc_result) =
  Printf.sprintf "Room workspace GC complete (retention %.1f day(s)).\n%s\n%s"
    (retention_seconds /. Room_workspace.seconds_per_day)
    (format_gc_section "Preserved paths" result.preserved)
    (format_gc_section "Purged paths" result.purged)

let cmd_rooms args =
  let cfg = get_config () in
  match args with
  | [] | [ "list" ] ->
      let profiles = cfg.room_profiles in
      let bindings = cfg.room_profile_bindings in
      if profiles = [] && bindings = [] then
        "No room profiles or bindings configured."
      else
        let columns =
          Table_format.
            [
              { header = "PROFILE"; align = Left; min_width = 8; flex = false };
              { header = "MODEL"; align = Left; min_width = 10; flex = false };
              { header = "ROOM"; align = Left; min_width = 8; flex = false };
              { header = "ACTIVE"; align = Left; min_width = 6; flex = false };
            ]
        in
        let rows =
          List.map
            (fun (p : Runtime_config.room_profile) ->
              let bound =
                List.filter
                  (fun (b : Runtime_config.room_profile_binding) ->
                    b.profile_id = p.id)
                  bindings
              in
              if bound = [] then [ p.id; p.model; "(none)"; "-" ]
              else
                List.map
                  (fun (b : Runtime_config.room_profile_binding) ->
                    [
                      p.id; p.model; b.room; (if b.active then "yes" else "no");
                    ])
                  bound
                |> List.concat)
            profiles
        in
        let unbound_bindings =
          List.filter_map
            (fun (b : Runtime_config.room_profile_binding) ->
              if
                List.exists
                  (fun (p : Runtime_config.room_profile) -> p.id = b.profile_id)
                  profiles
              then None
              else
                Some
                  [
                    b.profile_id;
                    "(missing)";
                    b.room;
                    (if b.active then "yes" else "no");
                  ])
            bindings
        in
        let all_rows = rows @ unbound_bindings in
        Format_adapter.bold Format_adapter.Plain "Room Profiles"
        ^ "\n\n"
        ^ Format_adapter.render_table Format_adapter.Plain ~max_width:80 columns
            all_rows
  | [ "show"; room_id ] ->
      let binding =
        List.find_opt
          (fun (b : Runtime_config.room_profile_binding) -> b.room = room_id)
          cfg.room_profile_bindings
      in
      let profile =
        match binding with
        | Some b ->
            List.find_opt
              (fun (p : Runtime_config.room_profile) -> p.id = b.profile_id)
              cfg.room_profiles
        | None -> None
      in
      let lines = ref [] in
      let add s = lines := s :: !lines in
      add (Printf.sprintf "Room:      %s" room_id);
      (match binding with
      | Some b ->
          add (Printf.sprintf "Profile:   %s" b.profile_id);
          add
            (Printf.sprintf "Active:    %s" (if b.active then "yes" else "no"))
      | None -> add "Profile:   (not bound)");
      (match profile with
      | Some p ->
          add (Printf.sprintf "Model:     %s" p.model);
          add (Printf.sprintf "Max iters: %d" p.max_tool_iterations);
          if p.system_prompt <> "" then begin
            add "";
            add "--- System Prompt ---";
            let preview =
              if String.length p.system_prompt > 500 then
                String.sub p.system_prompt 0 500 ^ "\n[...truncated]"
              else p.system_prompt
            in
            add preview
          end
      | None -> (
          match binding with
          | Some b ->
              add
                (Printf.sprintf "Warning: profile '%s' not found in config."
                   b.profile_id)
          | None -> ()));
      String.concat "\n" (List.rev !lines)
  | [ "workspace"; room_id ] ->
      let path = Room_workspace.workspace_path room_id in
      Printf.sprintf "Workspace for room '%s':\nPreserved path: %s" room_id path
  | "gc" :: flags -> (
      match require_admin () with
      | Some err -> err
      | None -> (
          match parse_retention_seconds flags with
          | Error msg -> "Error: " ^ msg
          | Ok retention_seconds ->
              let db = get_db () in
              let protected_paths =
                active_room_workspace_ids ~cfg ~db
                |> room_workspace_paths_for_refs
              in
              let result =
                Room_workspace.gc ~retention_seconds ~protected_paths ()
              in
              format_gc_result retention_seconds result))
  | "bind" :: room_id :: profile_id :: rest -> (
      match require_admin () with
      | Some err -> err
      | None -> (
          let preserve = List.mem "--preserve" rest in
          let reset = List.mem "--reset" rest in
          if preserve && reset then
            "Error: choose only one of --preserve or --reset when rebinding."
          else
            let active_profiles =
              List.filter
                (fun (p : Runtime_config.room_profile) ->
                  not (room_profile_deleted p))
                cfg.room_profiles
            in
            let profile_exists =
              List.exists
                (fun (p : Runtime_config.room_profile) -> p.id = profile_id)
                active_profiles
            in
            if not profile_exists then
              let available =
                List.map
                  (fun (p : Runtime_config.room_profile) -> p.id)
                  active_profiles
              in
              if available = [] then
                Printf.sprintf
                  "Error: no room profiles configured. Add a room_profiles \
                   entry to config.json first."
              else
                Printf.sprintf
                  "Error: profile '%s' not found. Available profiles: %s"
                  profile_id
                  (String.concat ", " available)
            else
              let existing =
                List.find_opt
                  (fun (b : Runtime_config.room_profile_binding) ->
                    b.room = room_id)
                  cfg.room_profile_bindings
              in
              match existing with
              | Some b when b.profile_id = profile_id ->
                  Printf.sprintf "Room '%s' is already bound to profile '%s'."
                    room_id profile_id
              | Some b when not (preserve || reset) ->
                  Printf.sprintf
                    "Error: room '%s' is already bound to profile '%s'. Rebind \
                     requires an explicit choice: pass --preserve to keep room \
                     history/state or --reset to start fresh."
                    room_id b.profile_id
              | _ -> (
                  let remaining =
                    List.filter
                      (fun (b : Runtime_config.room_profile_binding) ->
                        b.room <> room_id)
                      cfg.room_profile_bindings
                  in
                  let new_binding : Runtime_config.room_profile_binding =
                    { profile_id; room = room_id; active = true }
                  in
                  let bindings = new_binding :: remaining in
                  let bindings_json =
                    `Assoc
                      [
                        ( "room_profile_bindings",
                          `List (List.map room_profile_binding_to_json bindings)
                        );
                      ]
                  in
                  match Setup_common.merge_and_write_config bindings_json with
                  | Ok path ->
                      let choice = if reset then "reset" else "preserve" in
                      Printf.sprintf "Bound room '%s' to profile '%s' (%s).\n%s"
                        room_id profile_id choice path
                  | Error e -> Printf.sprintf "Failed to write config: %s" e)))
  | "rename" :: profile_id :: name_parts -> (
      match require_admin () with
      | Some err -> err
      | None -> (
          let display_name = String.trim (String.concat " " name_parts) in
          if display_name = "" then
            "Error: rooms rename requires a non-empty display name."
          else
            let found =
              List.exists
                (fun (p : Runtime_config.room_profile) ->
                  p.id = profile_id && not (room_profile_deleted p))
                cfg.room_profiles
            in
            if not found then
              Printf.sprintf "Error: active profile '%s' not found." profile_id
            else
              let profiles =
                List.map
                  (fun (p : Runtime_config.room_profile) ->
                    if p.id = profile_id then
                      { p with display_name = Some display_name }
                    else p)
                  cfg.room_profiles
              in
              match
                write_room_config ~profiles ~bindings:cfg.room_profile_bindings
              with
              | Ok path ->
                  Printf.sprintf
                    "Profile '%s' renamed to '%s'. Stable identity is unchanged.\n\
                     %s"
                    profile_id display_name path
              | Error e -> Printf.sprintf "Failed to write config: %s" e))
  | "delete" :: profile_id :: flags -> (
      match require_admin () with
      | Some err -> err
      | None -> (
          let force = List.mem "--force" flags in
          match
            List.find_opt
              (fun (p : Runtime_config.room_profile) -> p.id = profile_id)
              cfg.room_profiles
          with
          | None -> Printf.sprintf "Error: profile '%s' not found." profile_id
          | Some profile when room_profile_deleted profile ->
              Printf.sprintf "Profile '%s' is already deleted." profile_id
          | Some _ -> (
              let bound_rooms =
                cfg.room_profile_bindings
                |> List.filter (fun (b : Runtime_config.room_profile_binding) ->
                    b.profile_id = profile_id)
                |> List.map (fun (b : Runtime_config.room_profile_binding) ->
                    b.room)
              in
              let db = get_db () in
              let active_tasks =
                active_background_count_for_rooms ~db bound_rooms
              in
              let active_cron = active_cron_count_for_rooms ~db bound_rooms in
              if (not force) && (active_tasks > 0 || active_cron > 0) then
                Printf.sprintf
                  "Error: profile '%s' has active work (%d background task(s), \
                   %d active routine/cron job(s)). Stop or move that work, or \
                   pass --force to delete anyway."
                  profile_id active_tasks active_cron
              else
                let profiles =
                  List.map
                    (fun (p : Runtime_config.room_profile) ->
                      if p.id = profile_id then { p with status = "deleted" }
                      else p)
                    cfg.room_profiles
                in
                let bindings =
                  List.filter
                    (fun (b : Runtime_config.room_profile_binding) ->
                      b.profile_id <> profile_id)
                    cfg.room_profile_bindings
                in
                match write_room_config ~profiles ~bindings with
                | Ok path ->
                    Printf.sprintf
                      "Profile '%s' soft-deleted; removed %d binding(s).\n%s"
                      profile_id (List.length bound_rooms) path
                | Error e -> Printf.sprintf "Failed to write config: %s" e)))
  | [ "unbind"; room_id ] -> (
      match require_admin () with
      | Some err -> err
      | None -> (
          let existing =
            List.find_opt
              (fun (b : Runtime_config.room_profile_binding) ->
                b.room = room_id)
              cfg.room_profile_bindings
          in
          match existing with
          | None -> Printf.sprintf "No binding found for room '%s'." room_id
          | Some _ -> (
              let remaining =
                List.filter
                  (fun (b : Runtime_config.room_profile_binding) ->
                    b.room <> room_id)
                  cfg.room_profile_bindings
              in
              let bindings_json =
                `Assoc
                  [
                    ( "room_profile_bindings",
                      `List
                        (List.map
                           (fun (b : Runtime_config.room_profile_binding) ->
                             `Assoc
                               [
                                 ("profile_id", `String b.profile_id);
                                 ("room", `String b.room);
                                 ("active", `Bool b.active);
                               ])
                           remaining) );
                  ]
              in
              match Setup_common.merge_and_write_config bindings_json with
              | Ok path ->
                  Printf.sprintf
                    "Unbound room '%s'. The profile is preserved; rebind with: \
                     clawq rooms bind ROOM_ID PROFILE_ID\n\n\
                     Note: changes take effect after daemon restart or config \
                     reload.\n\
                     %s"
                    room_id path
              | Error e -> Printf.sprintf "Failed to write config: %s" e)))
  | _ ->
      "Usage: clawq rooms <list|show|workspace|gc|bind|rename|delete|unbind>\n\n\
       Subcommands:\n\
      \  list                        List all room profiles and bindings\n\
      \  show <room_id>              Show room binding and profile details\n\
      \  workspace <room_id>         Show/create the room workspace path\n\
      \  gc [--retention-days N]     Purge expired room workspaces (admin-only)\n\
      \  bind <room_id> <profile_id> [--preserve|--reset]\n\
      \                              Bind or explicitly rebind a room \
       (admin-only)\n\
      \  rename <profile_id> <name>  Update profile display name (admin-only)\n\
      \  delete <profile_id> [--force]\n\
      \                              Soft-delete profile and remove bindings \
       (admin-only)\n\
      \  unbind <room_id>            Remove room binding (preserves profile)"

let cmd_rig args =
  match args with
  | [ "install"; name ] | [ "add"; name ] -> (
      match Rig.find_rig name with
      | None ->
          Printf.sprintf
            "Unknown rig '%s'. Run 'clawq rig list' to see available rigs." name
      | Some rig -> (
          let cfg = get_config () in
          let db = get_db () in
          Background_task.init_schema db;
          match Rig.prompt_for ~name ~action:`Install with
          | Error msg -> "Error: " ^ msg
          | Ok prompt -> (
              match
                Background_task.delegate_enqueue ~db ?notify_cfg:cfg.notify
                  ~default_repo_path:(Dot_dir.path ()) ~goal:prompt ()
              with
              | Ok (id, _, _) ->
                  Rig.mark_installed ~name ~version:rig.version;
                  Printf.sprintf
                    "Rig '%s' install delegated as task %d. Track with: clawq \
                     background show %d"
                    name id id
              | Error msg -> "Error: " ^ msg)))
  | [ "adjust"; name ] | [ "modify"; name ] -> (
      match Rig.find_rig name with
      | None ->
          Printf.sprintf
            "Unknown rig '%s'. Run 'clawq rig list' to see available rigs." name
      | Some _rig -> (
          let cfg = get_config () in
          let db = get_db () in
          Background_task.init_schema db;
          match Rig.prompt_for ~name ~action:`Adjust with
          | Error msg -> "Error: " ^ msg
          | Ok prompt -> (
              match
                Background_task.delegate_enqueue ~db ?notify_cfg:cfg.notify
                  ~default_repo_path:(Dot_dir.path ()) ~goal:prompt ()
              with
              | Ok (id, _, _) ->
                  Printf.sprintf
                    "Rig '%s' adjust delegated as task %d. Track with: clawq \
                     background show %d"
                    name id id
              | Error msg -> "Error: " ^ msg)))
  | [ "remove"; name ] | [ "uninstall"; name ] -> (
      match Rig.find_rig name with
      | None ->
          Printf.sprintf
            "Unknown rig '%s'. Run 'clawq rig list' to see available rigs." name
      | Some _rig -> (
          let cfg = get_config () in
          let db = get_db () in
          Background_task.init_schema db;
          match Rig.prompt_for ~name ~action:`Remove with
          | Error msg -> "Error: " ^ msg
          | Ok prompt -> (
              match
                Background_task.delegate_enqueue ~db ?notify_cfg:cfg.notify
                  ~default_repo_path:(Dot_dir.path ()) ~goal:prompt ()
              with
              | Ok (id, _, _) ->
                  Rig.mark_removed ~name;
                  Printf.sprintf
                    "Rig '%s' remove delegated as task %d. Track with: clawq \
                     background show %d"
                    name id id
              | Error msg -> "Error: " ^ msg)))
  | [] | [ "list" ] -> Rig.list_text ()
  | _ ->
      "Usage: clawq rig install|adjust|remove|list [name]\n\n\
       Subcommands:\n\
      \  install <name>   Install a rig (setup via background task)\n\
      \  adjust <name>    Reconfigure an installed rig\n\
      \  remove <name>    Remove an installed rig\n\
      \  list             List available rigs"

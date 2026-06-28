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

(** Like [require_admin] but also records a denial event in the room activity
    ledger when the check fails. [room_id] identifies the affected room or
    profile scope; [action] describes the attempted operation. *)
let require_admin_audited ~room_id ~action =
  match require_admin () with
  | Some _ as err ->
      (try
         let db = get_db () in
         Room_activity_ledger.init_schema db;
         ignore
           (Room_activity_ledger.append_now ~db ~room_id
              ~event_type:"admin_denied" ~actor:"cli"
              ~metadata:
                (`Assoc
                   [
                     ("action", `String action);
                     ("error", `String "requires CLAWQ_ADMIN");
                   ]))
       with _ -> ());
      err
  | None -> None

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
    @ (if p.access_bundle_ids = [] then []
       else
         [
           ( "access_bundle_ids",
             `List (List.map (fun id -> `String id) p.access_bundle_ids) );
         ])
    @ (if not p.ambient_enabled then [] else [ ("ambient_enabled", `Bool true) ])
    @ (if p.ambient_rate_limit_rph = 0 then []
       else [ ("ambient_rate_limit_rph", `Int p.ambient_rate_limit_rph) ])
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

type ledger_filters = {
  room_id : string option;
  event_type : string option;
  from_timestamp : string option;
  to_timestamp : string option;
  actor : string option;
  metadata_filters : (string * string) list;
}

let empty_ledger_filters =
  {
    room_id = None;
    event_type = None;
    from_timestamp = None;
    to_timestamp = None;
    actor = None;
    metadata_filters = [];
  }

let metadata_filter_key = function
  | "--profile-id" | "--profile" -> Some "profile_id"
  | "--thread-id" | "--thread" -> Some "thread_id"
  | "--task-id" | "--task" -> Some "task_id"
  | "--background-id" | "--background" -> Some "background_id"
  | "--requester" -> Some "requester"
  | "--status" -> Some "status"
  | _ -> None

let parse_ledger_filters flags =
  let rec loop filters jsonl = function
    | [] -> Ok (filters, jsonl)
    | ("--room-id" | "--room") :: value :: rest ->
        loop { filters with room_id = Some value } jsonl rest
    | ("--event-type" | "--type") :: value :: rest ->
        loop { filters with event_type = Some value } jsonl rest
    | ("--from" | "--since") :: value :: rest ->
        loop { filters with from_timestamp = Some value } jsonl rest
    | ("--to" | "--until") :: value :: rest ->
        loop { filters with to_timestamp = Some value } jsonl rest
    | "--actor" :: value :: rest ->
        loop { filters with actor = Some value } jsonl rest
    | "--format" :: "json" :: rest -> loop filters false rest
    | "--format" :: "jsonl" :: rest -> loop filters true rest
    | "--jsonl" :: rest -> loop filters true rest
    | flag :: value :: rest -> (
        match metadata_filter_key flag with
        | Some key ->
            loop
              {
                filters with
                metadata_filters = (key, value) :: filters.metadata_filters;
              }
              jsonl rest
        | None -> Error (Printf.sprintf "unknown rooms ledger flag: %s" flag))
    | [ flag ] ->
        Error (Printf.sprintf "missing value for rooms ledger flag: %s" flag)
  in
  loop empty_ledger_filters false flags

let metadata_value_matches expected = function
  | `String value -> String.equal value expected
  | `Int value -> String.equal (string_of_int value) expected
  | `Intlit value -> String.equal value expected
  | `Bool value -> String.equal (string_of_bool value) expected
  | _ -> false

let event_matches_metadata filters (event : Room_activity_ledger.event) =
  match event.metadata with
  | `Assoc fields ->
      List.for_all
        (fun (key, expected) ->
          match List.assoc_opt key fields with
          | Some value -> metadata_value_matches expected value
          | None -> false)
        filters
  | _ -> filters = []

let apply_ledger_filters filters events =
  events
  |> List.filter (fun (event : Room_activity_ledger.event) ->
      match filters.actor with
      | Some actor -> String.equal event.actor actor
      | None -> true)
  |> List.filter (event_matches_metadata filters.metadata_filters)

let query_room_ledger ~db filters =
  Room_activity_ledger.query ?room_id:filters.room_id
    ?event_type:filters.event_type ?from_timestamp:filters.from_timestamp
    ?to_timestamp:filters.to_timestamp ~db ()
  |> apply_ledger_filters filters

let format_ledger_events events =
  match events with
  | [] -> "No room activity ledger entries matched."
  | _ ->
      let columns =
        Table_format.
          [
            { header = "ID"; align = Right; min_width = 3; flex = false };
            { header = "TIMESTAMP"; align = Left; min_width = 20; flex = false };
            { header = "ROOM"; align = Left; min_width = 8; flex = false };
            { header = "EVENT"; align = Left; min_width = 10; flex = false };
            { header = "ACTOR"; align = Left; min_width = 8; flex = false };
          ]
      in
      let rows =
        List.map
          (fun (event : Room_activity_ledger.event) ->
            [
              string_of_int event.id;
              event.timestamp;
              event.room_id;
              event.event_type;
              event.actor;
            ])
          events
      in
      Format_adapter.render_table Format_adapter.Plain ~max_width:120 columns
        rows

let floor_div a b =
  let q = a / b in
  let r = a mod b in
  if r <> 0 && r < 0 <> (b < 0) then q - 1 else q

let days_from_civil year month day =
  let year = if month <= 2 then year - 1 else year in
  let era = floor_div (if year >= 0 then year else year - 399) 400 in
  let yoe = year - (era * 400) in
  let mp = month + if month > 2 then -3 else 9 in
  let doy = (((153 * mp) + 2) / 5) + day - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let civil_from_days days =
  let days = days + 719468 in
  let era = floor_div (if days >= 0 then days else days - 146096) 146097 in
  let doe = days - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let year = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let day = doy - (((153 * mp) + 2) / 5) + 1 in
  let month = mp + if mp < 10 then 3 else -9 in
  let year = year + if month <= 2 then 1 else 0 in
  (year, month, day)

let strip_fractional_seconds timestamp =
  match (String.index_opt timestamp '.', String.rindex_opt timestamp 'Z') with
  | Some dot, Some z when dot < z -> String.sub timestamp 0 dot ^ "Z"
  | _ -> timestamp

let epoch_seconds_of_timestamp timestamp =
  try
    Scanf.sscanf (strip_fractional_seconds timestamp) "%d-%d-%dT%d:%d:%dZ"
      (fun year month day hour minute second ->
        Ok
          ((days_from_civil year month day * 86400)
          + (hour * 3600) + (minute * 60) + second))
  with _ ->
    Error
      (Printf.sprintf
         "timestamp must use UTC ISO-8601 format like 2026-06-27T10:00:00Z: %s"
         timestamp)

let timestamp_of_epoch_seconds seconds =
  let days = floor_div seconds 86400 in
  let seconds_of_day = seconds - (days * 86400) in
  let year, month, day = civil_from_days days in
  let hour = seconds_of_day / 3600 in
  let minute = seconds_of_day mod 3600 / 60 in
  let second = seconds_of_day mod 60 in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" year month day hour minute
    second

let parse_ledger_retention flags =
  let rec loop retention_days now = function
    | [] -> Ok (retention_days, now)
    | "--retention-days" :: value :: rest -> (
        match float_of_string_opt value with
        | Some days when days >= 0.0 -> loop days now rest
        | _ -> Error "--retention-days must be a non-negative number")
    | "--now" :: value :: rest -> loop retention_days value rest
    | flag :: _ ->
        Error (Printf.sprintf "unknown rooms ledger retention flag: %s" flag)
  in
  loop Room_workspace.default_retention_days
    (Room_activity_ledger.timestamp_now ())
    flags

let retention_cutoff ~now ~retention_days =
  match epoch_seconds_of_timestamp now with
  | Error _ as err -> err
  | Ok now_seconds ->
      let retention_seconds = int_of_float (retention_days *. 86400.0) in
      Ok (timestamp_of_epoch_seconds (now_seconds - retention_seconds))

let cmd_rooms_ledger args =
  match require_admin () with
  | Some err -> err
  | None -> (
      match args with
      | "list" :: flags -> (
          match parse_ledger_filters flags with
          | Error msg -> "Error: " ^ msg
          | Ok (filters, _) ->
              let events = query_room_ledger ~db:(get_db ()) filters in
              format_ledger_events events)
      | "export" :: flags -> (
          match parse_ledger_filters flags with
          | Error msg -> "Error: " ^ msg
          | Ok (filters, jsonl) ->
              let events = query_room_ledger ~db:(get_db ()) filters in
              if jsonl then Room_activity_ledger.events_to_jsonl events
              else Room_activity_ledger.events_to_json_string events)
      | "retention-cleanup" :: flags | "retention" :: flags -> (
          match parse_ledger_retention flags with
          | Error msg -> "Error: " ^ msg
          | Ok (retention_days, now) -> (
              match retention_cutoff ~now ~retention_days with
              | Error msg -> "Error: " ^ msg
              | Ok before_timestamp ->
                  let deleted =
                    Room_activity_ledger.delete_before ~db:(get_db ())
                      ~before_timestamp
                  in
                  Printf.sprintf
                    "Room activity ledger retention cleanup complete: deleted \
                     %d entr%s older than %s (retention %.1f day(s))."
                    deleted
                    (if deleted = 1 then "y" else "ies")
                    before_timestamp retention_days))
      | _ ->
          "Usage: clawq rooms ledger <list|export|retention-cleanup> [filters]\n\n\
           Filters: --room-id ID --event-type TYPE --from TS --to TS --actor \
           ACTOR\n\
          \         --profile-id ID --thread-id ID --task-id ID \
           --background-id ID\n\
          \         --requester ID --status STATUS\n\
           Export:  add --format jsonl or --jsonl for JSON Lines")

(** Resolve a config-level room profile to its DB integer id. Creates the DB row
    if it does not yet exist. Returns [Error msg] when the profile is not
    configured or is soft-deleted. *)
let resolve_room_profile_db_id ~(db : Sqlite3.db) ~(cfg : Runtime_config.t)
    profile_id_str =
  let active_profiles =
    List.filter
      (fun (p : Runtime_config.room_profile) -> not (room_profile_deleted p))
      cfg.room_profiles
  in
  if
    not
      (List.exists
         (fun (p : Runtime_config.room_profile) -> p.id = profile_id_str)
         active_profiles)
  then
    let available =
      List.map (fun (p : Runtime_config.room_profile) -> p.id) active_profiles
    in
    if available = [] then
      Error
        "Error: no room profiles configured. Add a room_profiles entry to \
         config.json first."
    else
      Error
        (Printf.sprintf "Error: profile '%s' not found. Available profiles: %s"
           profile_id_str
           (String.concat ", " available))
  else
    match Memory_core.get_room_profile_by_name ~db ~name:profile_id_str with
    | Some rp -> Ok rp.id
    | None ->
        let id = Memory_core.insert_room_profile ~db ~name:profile_id_str in
        Ok id

(** Extract [--thread-id VALUE] from a flag list, returning (value, remaining).
*)
let extract_thread_id_flag args =
  let rec loop acc = function
    | "--thread-id" :: v :: rest -> (Some v, List.rev_append acc rest)
    | x :: rest -> loop (x :: acc) rest
    | [] -> (None, List.rev acc)
  in
  loop [] args

let show_room_routine name =
  let db = get_db () in
  Scheduler.init_schema db;
  match Scheduler.get_job ~db ~name with
  | None -> Printf.sprintf "No room routine found with name '%s'." name
  | Some job when job.profile_id = None ->
      Printf.sprintf
        "Job '%s' exists but is not a room routine (no profile_id set). Use \
         'clawq cron show %s' instead."
        name name
  | Some job ->
      let runs = Scheduler.get_history ~db ~name ~limit:5 in
      let lines = ref [] in
      let add s = lines := s :: !lines in
      add (Printf.sprintf "Name:      %s" job.name);
      add (Printf.sprintf "Session:   %s" job.session_key);
      (match Scheduler.job_routine_target job with
      | Some target -> add (Printf.sprintf "Target:    %s" target)
      | None -> ());
      add (Printf.sprintf "Schedule:  %s" job.schedule_str);
      add (Printf.sprintf "Enabled:   %s" (if job.enabled then "yes" else "no"));
      (match job.thread_id with
      | Some tid -> add (Printf.sprintf "Thread:    %s" tid)
      | None -> ());
      (match job.expires_at with
      | Some ea -> add (Printf.sprintf "Expires:   %s" ea)
      | None -> ());
      add "";
      add "--- Message ---";
      add job.message;
      if runs <> [] then begin
        add "";
        add "--- Recent Runs ---";
        List.iter
          (fun (r : Scheduler.run) ->
            let preview =
              match r.result_preview with
              | Some p when String.length p > 60 -> String.sub p 0 57 ^ "..."
              | Some p -> p
              | None -> ""
            in
            add
              (Printf.sprintf "  %d  %s  %s  %s" r.run_id r.started_at r.status
                 preview))
          runs
      end;
      String.concat "\n" (List.rev !lines)

let cmd_rooms_routine cfg args =
  match args with
  | "create" :: profile_id_str :: schedule :: message_parts -> (
      match
        require_admin_audited ~room_id:profile_id_str ~action:"routine_create"
      with
      | Some err -> err
      | None -> (
          let thread_id, message_parts = extract_thread_id_flag message_parts in
          let message = String.trim (String.concat " " message_parts) in
          if message = "" then
            "Error: room routine message cannot be empty. Provide a message \
             after the schedule (e.g. 'clawq rooms routine create coding \
             \"every 1h\" \"Generate daily summary\"')."
          else
            let db = get_db () in
            Scheduler.init_schema db;
            match resolve_room_profile_db_id ~db ~cfg profile_id_str with
            | Error e -> e
            | Ok db_profile_id -> (
                let routine_name = Printf.sprintf "routine-%s" profile_id_str in
                let session_key =
                  Room_session.make_routine_key ~profile_id:profile_id_str
                    ~routine_id:routine_name ()
                in
                match
                  Scheduler.add_job ~db ~name:routine_name ~session_key ~message
                    ~schedule ~profile_id:db_profile_id ?thread_id ()
                with
                | Ok () ->
                    Printf.sprintf
                      "Created room routine '%s' (profile=%s, schedule=%s).\n\
                       Session key: %s"
                      routine_name profile_id_str schedule session_key
                | Error e -> Printf.sprintf "Error: %s" e)))
  | [ "list" ] | "list" :: _ ->
      let db = get_db () in
      Scheduler.init_schema db;
      let jobs = Scheduler.list_jobs ~db in
      let routine_jobs =
        List.filter (fun (j : Scheduler.job) -> j.profile_id <> None) jobs
      in
      if routine_jobs = [] then
        "No room routines configured. Use 'clawq rooms routine create' to \
         create one."
      else
        let columns =
          Table_format.
            [
              { header = "NAME"; align = Left; min_width = 4; flex = false };
              { header = "PROFILE"; align = Left; min_width = 7; flex = false };
              { header = "SCHEDULE"; align = Left; min_width = 8; flex = false };
              { header = "ENABLED"; align = Left; min_width = 3; flex = false };
              { header = "THREAD"; align = Left; min_width = 3; flex = false };
              { header = "MESSAGE"; align = Left; min_width = 10; flex = true };
            ]
        in
        let rows =
          List.map
            (fun (j : Scheduler.job) ->
              let profile_str =
                match Scheduler.job_routine_target j with
                | Some target -> target
                | None -> "-"
              in
              let preview =
                if String.length j.message > 50 then
                  String.sub j.message 0 47 ^ "..."
                else j.message
              in
              [
                j.name;
                profile_str;
                j.schedule_str;
                (if j.enabled then "yes" else "no");
                (match j.thread_id with Some t -> t | None -> "-");
                preview;
              ])
            routine_jobs
        in
        Format_adapter.bold Format_adapter.Plain "Room Routines"
        ^ "\n\n"
        ^ Format_adapter.render_table Format_adapter.Plain ~max_width:80 columns
            rows
  | [ "show"; name ] -> show_room_routine name
  | [ "show" ] ->
      "Error: rooms routine show requires a routine name. Usage: clawq rooms \
       routine show <name>"
  | "edit" :: name :: edit_parts -> (
      match require_admin_audited ~room_id:name ~action:"routine_edit" with
      | Some err -> err
      | None -> (
          let db = get_db () in
          Scheduler.init_schema db;
          match Scheduler.get_job ~db ~name with
          | None -> Printf.sprintf "No room routine found with name '%s'." name
          | Some job when job.profile_id = None ->
              Printf.sprintf
                "Job '%s' exists but is not a room routine (no profile_id \
                 set). Use 'clawq cron show %s' instead."
                name name
          | Some _ -> (
              let schedule = ref None in
              let message = ref None in
              let rec parse_flags = function
                | "--schedule" :: s :: rest ->
                    schedule := Some s;
                    parse_flags rest
                | "--message" :: m :: rest ->
                    message := Some m;
                    parse_flags rest
                | [] -> ()
                | unknown :: _ ->
                    ignore
                      (Printf.printf "Warning: unknown flag '%s'\n" unknown)
              in
              parse_flags edit_parts;
              match
                Scheduler.update_job ~db ~name ?schedule:!schedule
                  ?message:!message ()
              with
              | Ok () ->
                  let changes =
                    List.filter_map
                      (fun (label, v) ->
                        match v with Some x -> Some (label, x) | None -> None)
                      [ ("schedule", !schedule); ("message", !message) ]
                  in
                  let change_str =
                    String.concat ", "
                      (List.map
                         (fun (l, v) -> Printf.sprintf "%s=%s" l v)
                         changes)
                  in
                  Printf.sprintf "Updated room routine '%s' (%s)." name
                    change_str
              | Error e -> Printf.sprintf "Error: %s" e)))
  | [ "edit" ] ->
      "Error: rooms routine edit requires a routine name. Usage: clawq rooms \
       routine edit <name> [--schedule S] [--message M]"
  | [ "remove"; name ] -> (
      match require_admin_audited ~room_id:name ~action:"routine_remove" with
      | Some err -> err
      | None -> (
          let db = get_db () in
          Scheduler.init_schema db;
          match Scheduler.get_job ~db ~name with
          | None -> Printf.sprintf "No room routine found with name '%s'." name
          | Some job when job.profile_id = None ->
              Printf.sprintf
                "Job '%s' exists but is not a room routine (no profile_id \
                 set). Use 'clawq cron remove %s' instead."
                name name
          | Some _ ->
              if Scheduler.remove_job ~db ~name then
                Printf.sprintf "Removed room routine '%s'." name
              else Printf.sprintf "Failed to remove room routine '%s'." name))
  | [ "remove" ] ->
      "Error: rooms routine remove requires a routine name. Usage: clawq rooms \
       routine remove <name>"
  | [ "enable"; name ] -> (
      match require_admin_audited ~room_id:name ~action:"routine_enable" with
      | Some err -> err
      | None -> (
          let db = get_db () in
          Scheduler.init_schema db;
          match Scheduler.get_job ~db ~name with
          | None -> Printf.sprintf "No room routine found with name '%s'." name
          | Some job when job.profile_id = None ->
              Printf.sprintf
                "Job '%s' exists but is not a room routine (no profile_id set)."
                name
          | Some j when j.enabled ->
              Printf.sprintf "Room routine '%s' is already enabled." name
          | Some _ -> (
              match Scheduler.toggle_job ~db ~name with
              | Ok () -> Printf.sprintf "Enabled room routine '%s'." name
              | Error e -> Printf.sprintf "Error: %s" e)))
  | [ "enable" ] -> "Error: rooms routine enable requires a routine name."
  | [ "disable"; name ] -> (
      match require_admin_audited ~room_id:name ~action:"routine_disable" with
      | Some err -> err
      | None -> (
          let db = get_db () in
          Scheduler.init_schema db;
          match Scheduler.get_job ~db ~name with
          | None -> Printf.sprintf "No room routine found with name '%s'." name
          | Some job when job.profile_id = None ->
              Printf.sprintf
                "Job '%s' exists but is not a room routine (no profile_id set)."
                name
          | Some j when not j.enabled ->
              Printf.sprintf "Room routine '%s' is already disabled." name
          | Some _ -> (
              match Scheduler.toggle_job ~db ~name with
              | Ok () -> Printf.sprintf "Disabled room routine '%s'." name
              | Error e -> Printf.sprintf "Error: %s" e)))
  | [ "disable" ] -> "Error: rooms routine disable requires a routine name."
  | [ "trigger"; name ] -> (
      match require_admin_audited ~room_id:name ~action:"routine_trigger" with
      | Some err -> err
      | None -> (
          let db = get_db () in
          Scheduler.init_schema db;
          Background_task.init_schema db;
          match Scheduler.get_job ~db ~name with
          | None -> Printf.sprintf "No room routine found with name '%s'." name
          | Some job when job.profile_id = None ->
              Printf.sprintf
                "Job '%s' exists but is not a room routine (no profile_id \
                 set). Use 'clawq cron trigger %s' instead."
                name name
          | Some _ -> (
              match Scheduler.trigger_job ~db ~name () with
              | Ok task_id ->
                  Printf.sprintf
                    "Triggered room routine '%s' — enqueued as background task \
                     %d.\n\
                     Use 'clawq background show %d' to check progress."
                    name task_id task_id
              | Error e -> Printf.sprintf "Error: %s" e)))
  | [ "trigger" ] ->
      "Error: rooms routine trigger requires a routine name. Usage: clawq \
       rooms routine trigger <name>"
  | _ ->
      "Usage: clawq rooms routine \
       <create|list|show|edit|remove|enable|disable|trigger>\n\n\
       Subcommands:\n\
      \  create <profile> <schedule> <message> [--thread-id ID]\n\
      \                              Create a room routine (admin-only)\n\
      \  list                        List all room routines\n\
      \  show <name>                 Show room routine details\n\
      \  edit <name> [--schedule S] [--message M]\n\
      \                              Edit a room routine (admin-only)\n\
      \  remove <name>               Remove a room routine (admin-only)\n\
      \  enable <name>               Enable a room routine (admin-only)\n\
      \  disable <name>              Disable a room routine (admin-only)\n\
      \  trigger <name>              Trigger a room routine now (admin-only)"

let cmd_rooms_memory (cfg : Runtime_config.t) args =
  let db = get_db () in
  let is_admin =
    match Sys.getenv_opt "CLAWQ_ADMIN" with
    | Some v -> v = "1" || v = "true"
    | None -> false
  in
  let reconcile_error =
    try
      ignore (Memory.reconcile_room_profiles ~db ~config:cfg);
      None
    with exn ->
      Some
        (Printf.sprintf "Error reconciling room profile bindings: %s"
           (Printexc.to_string exn))
  in
  let room_display_name room_id =
    match Memory.get_room_profile_for_room ~db ~room_id with
    | Some profile -> Printf.sprintf "%s (%s)" room_id profile.name
    | None -> room_id
  in
  let get_room_binding ~room_id =
    match reconcile_error with
    | Some msg -> Error msg
    | None -> Ok (Memory.get_room_profile_binding ~db ~room_id)
  in
  let attach_scope_profile_if_missing ~(scope : Memory.memory_scope) ~profile_id
      =
    match scope.profile_id with
    | Some _ -> scope
    | None ->
        let stmt =
          Sqlite3.prepare db
            "UPDATE memory_scopes SET profile_id = ?, updated_at = \
             datetime('now') WHERE id = ? AND profile_id IS NULL"
        in
        Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
          (fun () ->
            ignore
              (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int profile_id)));
            ignore
              (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int scope.id)));
            match Sqlite3.step stmt with
            | Sqlite3.Rc.DONE -> ()
            | rc ->
                failwith
                  (Printf.sprintf "update room memory scope owner failed: %s"
                     (Sqlite3.Rc.to_string rc)));
        Option.value ~default:scope (Memory.get_scope ~db ~id:scope.id)
  in
  let ensure_room_scope ~room_id =
    match get_room_binding ~room_id with
    | Error msg -> Error msg
    | Ok binding -> (
        match Memory.get_scope_by_kind_key ~db ~kind:"room" ~key:room_id with
        | Some scope ->
            let scope =
              match binding with
              | Some binding ->
                  attach_scope_profile_if_missing ~scope
                    ~profile_id:binding.profile_id
              | None -> scope
            in
            Ok scope
        | None -> (
            match binding with
            | Some binding ->
                Ok
                  (Memory.create_scope ~db ~kind:"room" ~key:room_id
                     ~profile_id:binding.profile_id ~provenance:"cli" ())
            | None when is_admin ->
                Ok
                  (Memory.create_scope ~db ~kind:"room" ~key:room_id
                     ~provenance:"cli" ())
            | None ->
                Error
                  (Printf.sprintf "No memory scope found for room '%s'." room_id)
            ))
  in
  let check_grant ~room_id ~capability =
    match get_room_binding ~room_id with
    | Error msg -> Error msg
    | Ok None -> (
        (* No binding: check if scope exists and if grants allow access *)
        match Memory.get_scope_by_kind_key ~db ~kind:"room" ~key:room_id with
        | None ->
            Error
              (Printf.sprintf "No memory scope found for room '%s'." room_id)
        | Some scope ->
            if is_admin then Ok scope
            else
              let grants =
                Memory.resolve_grants ~db ~scope_id:scope.id
                  ~principal_kind:"room" ~principal_id:room_id
              in
              if List.mem capability grants then Ok scope
              else
                Error
                  (Printf.sprintf
                     "Access denied: room '%s' does not have '%s' capability."
                     room_id capability))
    | Ok (Some binding) -> (
        (* Has binding: owner has implicit access, or check grants *)
        match Memory.get_scope_by_kind_key ~db ~kind:"room" ~key:room_id with
        | None ->
            Error
              (Printf.sprintf "No memory scope found for room '%s'." room_id)
        | Some scope ->
            let scope =
              attach_scope_profile_if_missing ~scope
                ~profile_id:binding.profile_id
            in
            if is_admin then Ok scope
            else
              let is_owner =
                match scope.profile_id with
                | Some profile_id -> profile_id = binding.profile_id
                | None -> false
              in
              if is_owner then Ok scope
              else
                let grants =
                  Memory.resolve_grants ~db ~scope_id:scope.id
                    ~principal_kind:"profile"
                    ~principal_id:(string_of_int binding.profile_id)
                in
                if List.mem capability grants then Ok scope
                else
                  Error
                    (Printf.sprintf
                       "Access denied: room '%s' does not have '%s' capability."
                       room_id capability))
  in
  match args with
  | [ "list"; room_id ] -> (
      match check_grant ~room_id ~capability:"list" with
      | Error msg -> msg
      | Ok scope ->
          let memories =
            Memory.query_scoped_memories ~db ~scope_kind:"room"
              ~scope_key:room_id ~limit:100 ()
          in
          if memories = [] then
            Printf.sprintf "No memories found for room '%s'."
              (room_display_name room_id)
          else
            let columns =
              Table_format.
                [
                  { header = "ID"; align = Right; min_width = 4; flex = false };
                  {
                    header = "REFERENCE";
                    align = Left;
                    min_width = 12;
                    flex = false;
                  };
                  {
                    header = "CONTENT";
                    align = Left;
                    min_width = 20;
                    flex = true;
                  };
                  {
                    header = "PROVENANCE";
                    align = Left;
                    min_width = 8;
                    flex = false;
                  };
                  {
                    header = "UPDATED";
                    align = Left;
                    min_width = 16;
                    flex = false;
                  };
                ]
            in
            let rows =
              List.map
                (fun (m : Memory.scoped_memory) ->
                  let content_preview =
                    match m.content with
                    | Some c when String.length c > 80 ->
                        String.sub c 0 80 ^ "..."
                    | Some c -> c
                    | None -> "(empty)"
                  in
                  [
                    string_of_int m.id;
                    m.reference;
                    content_preview;
                    m.provenance;
                    m.updated_at;
                  ])
                memories
            in
            Format_adapter.bold Format_adapter.Plain
              (Printf.sprintf "Room Memories: %s" (room_display_name room_id))
            ^ "\n\n"
            ^ Format_adapter.render_table Format_adapter.Plain ~max_width:100
                columns rows)
  | [ "show"; room_id; memory_id_str ] -> (
      match check_grant ~room_id ~capability:"read" with
      | Error msg -> msg
      | Ok _scope -> (
          match int_of_string_opt memory_id_str with
          | None ->
              Printf.sprintf "Error: '%s' is not a valid memory ID."
                memory_id_str
          | Some memory_id -> (
              match Memory.get_scoped_memory ~db ~id:memory_id with
              | None -> Printf.sprintf "Memory #%d not found." memory_id
              | Some m when m.scope_kind <> "room" || m.scope_key <> room_id ->
                  Printf.sprintf "Memory #%d does not belong to room '%s'."
                    memory_id room_id
              | Some m ->
                  let lines = ref [] in
                  let add s = lines := s :: !lines in
                  add
                    (Printf.sprintf "Room:       %s"
                       (room_display_name room_id));
                  add (Printf.sprintf "ID:         %d" m.id);
                  add (Printf.sprintf "Reference:  %s" m.reference);
                  add (Printf.sprintf "Provenance: %s" m.provenance);
                  add (Printf.sprintf "Created:    %s" m.created_at);
                  add (Printf.sprintf "Updated:    %s" m.updated_at);
                  (match m.content with
                  | Some c ->
                      add "";
                      add "--- Content ---";
                      add c
                  | None -> add "Content:    (empty)");
                  (match m.redacted_at with
                  | Some ts ->
                      add "";
                      add (Printf.sprintf "Redacted:   %s" ts);
                      Option.iter
                        (fun r -> add (Printf.sprintf "Reason:     %s" r))
                        m.redaction_reason
                  | None -> ());
                  String.concat "\n" (List.rev !lines))))
  | "save" :: room_id :: reference :: content_parts -> (
      if reference = "" then "Error: memory reference is required."
      else if content_parts = [] then "Error: memory content is required."
      else
        match ensure_room_scope ~room_id with
        | Error msg -> msg
        | Ok _scope -> (
            match check_grant ~room_id ~capability:"write" with
            | Error msg -> msg
            | Ok scope -> (
                let content = String.concat " " content_parts in
                let provenance = if is_admin then "admin-cli" else "cli" in
                try
                  let m =
                    Memory.upsert_scoped_memory ~db ~scope_id:scope.id
                      ~reference ~content ~provenance ()
                  in
                  Printf.sprintf "Saved memory '%s' (ID: %d) for room '%s'."
                    m.reference m.id
                    (room_display_name room_id)
                with exn ->
                  Printf.sprintf "Error saving memory: %s"
                    (Printexc.to_string exn))))
  | "save" :: _ ->
      "Usage: clawq rooms memory save <room_id> <reference> <content>"
  | "correct" :: room_id :: memory_id_str :: content_parts -> (
      match check_grant ~room_id ~capability:"write" with
      | Error msg -> msg
      | Ok _scope -> (
          match int_of_string_opt memory_id_str with
          | None ->
              Printf.sprintf "Error: '%s' is not a valid memory ID."
                memory_id_str
          | Some memory_id -> (
              match Memory.get_scoped_memory ~db ~id:memory_id with
              | None -> Printf.sprintf "Memory #%d not found." memory_id
              | Some m when m.scope_kind <> "room" || m.scope_key <> room_id ->
                  Printf.sprintf "Memory #%d does not belong to room '%s'."
                    memory_id room_id
              | Some m when m.redacted_at <> None ->
                  Printf.sprintf
                    "Memory #%d is redacted and cannot be corrected." memory_id
              | Some _ -> (
                  if content_parts = [] then
                    "Error: new memory content is required."
                  else
                    let content = String.concat " " content_parts in
                    let provenance =
                      if is_admin then "corrected:admin-cli"
                      else "corrected:cli"
                    in
                    match
                      Memory.correct_scoped_memory ~db ~id:memory_id ~content
                        ~provenance ()
                    with
                    | None ->
                        Printf.sprintf "Error: failed to correct memory #%d."
                          memory_id
                    | Some updated ->
                        Printf.sprintf
                          "Corrected memory #%d '%s' for room '%s'.\n\
                           Old provenance preserved in correction trail."
                          updated.id updated.reference
                          (room_display_name room_id)))))
  | "correct" :: _ ->
      "Usage: clawq rooms memory correct <room_id> <id> <new_content>"
  | "forget" :: room_id :: memory_id_str :: flags -> (
      match check_grant ~room_id ~capability:"write" with
      | Error msg -> msg
      | Ok _scope -> (
          match int_of_string_opt memory_id_str with
          | None ->
              Printf.sprintf "Error: '%s' is not a valid memory ID."
                memory_id_str
          | Some memory_id -> (
              match Memory.get_scoped_memory ~db ~id:memory_id with
              | None -> Printf.sprintf "Memory #%d not found." memory_id
              | Some m when m.scope_kind <> "room" || m.scope_key <> room_id ->
                  Printf.sprintf "Memory #%d does not belong to room '%s'."
                    memory_id room_id
              | Some m ->
                  let hard_purge = List.mem "--hard" flags in
                  let reason =
                    let non_flags =
                      List.filter
                        (fun s -> not (String.starts_with ~prefix:"--" s))
                        flags
                    in
                    match non_flags with
                    | [] -> "user request"
                    | parts -> String.concat " " parts
                  in
                  if hard_purge then begin
                    match
                      require_admin_audited ~room_id ~action:"memory_hard_purge"
                    with
                    | Some err -> err
                    | None ->
                        if Memory.delete_scoped_memory ~db ~id:memory_id then
                          Printf.sprintf
                            "Hard-purged memory #%d '%s' for room '%s'."
                            memory_id m.reference
                            (room_display_name room_id)
                        else
                          Printf.sprintf
                            "Error: failed to hard-purge memory #%d." memory_id
                  end
                  else if m.redacted_at <> None then
                    Printf.sprintf
                      "Memory #%d is already redacted. Use --hard to purge."
                      memory_id
                  else if
                    Memory.redact_scoped_memory ~db ~id:memory_id ~reason ()
                  then
                    Printf.sprintf
                      "Forgot (redacted) memory #%d '%s' for room '%s'. \
                       Content is now hidden."
                      memory_id m.reference
                      (room_display_name room_id)
                  else
                    Printf.sprintf "Error: failed to redact memory #%d."
                      memory_id)))
  | "forget" :: _ ->
      "Usage: clawq rooms memory forget <room_id> <id> [-- --hard] [reason]\n\
       Note: use -- separator before flags when invoking from CLI."
  | _ ->
      "Usage: clawq rooms memory <list|show|save|correct|forget> <room_id> \
       [args...]\n\n\
       Subcommands:\n\
      \  list <room_id>              List memories in a room scope\n\
      \  show <room_id> <id>         Show details of a specific memory\n\
      \  save <room_id> <ref> <content>\n\
      \                              Save or update a room-scoped memory\n\
      \  correct <room_id> <id> <content>\n\
      \                              Correct a memory (preserves old provenance)\n\
      \  forget <room_id> <id> [-- --hard] [reason]\n\
      \                              Forget (redact) a memory (admin: --hard \
       for purge)"

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
  | "ledger" :: rest -> cmd_rooms_ledger rest
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
  | [ "inspect"; room_id ] -> (
      match require_admin () with
      | Some err -> err
      | None ->
          let db = get_db () in
          let result = Ambient_inspection.inspect ~db ~cfg ~room_id () in
          Ambient_inspection.format_inspection result)
  | "routine" :: rest -> cmd_rooms_routine cfg rest
  | "memory" :: rest -> cmd_rooms_memory cfg rest
  | _ ->
      "Usage: clawq rooms \
       <list|show|workspace|inspect|ledger|gc|bind|rename|delete|unbind|routine|memory>\n\n\
       Subcommands:\n\
      \  list                        List all room profiles and bindings\n\
      \  show <room_id>              Show room binding and profile details\n\
      \  workspace <room_id>         Show/create the room workspace path\n\
      \  inspect <room_id>           Inspect ambient watcher state (admin-only)\n\
      \  ledger <list|export|retention-cleanup>\n\
      \                              Query/export/prune room activity ledger \
       entries (admin-only)\n\
      \  gc [--retention-days N]     Purge expired room workspaces (admin-only)\n\
      \  bind <room_id> <profile_id> [--preserve|--reset]\n\
      \                              Bind or explicitly rebind a room \
       (admin-only)\n\
      \  rename <profile_id> <name>  Update profile display name (admin-only)\n\
      \  delete <profile_id> [--force]\n\
      \                              Soft-delete profile and remove bindings \
       (admin-only)\n\
      \  unbind <room_id>            Remove room binding (preserves profile)\n\
      \  routine <create|list|show|edit|remove|enable|disable|trigger>   \
       Manage room routines (admin-only)\n\
      \  memory <list|show|save|correct|forget> <room_id> [args...]\n\
      \                              Manage room-scoped memories"

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

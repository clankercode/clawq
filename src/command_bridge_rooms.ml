open Command_bridge_helpers
open Command_bridge_room_common

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

let resolve_room_profile_db_id =
  Command_bridge_room_routine.resolve_room_profile_db_id

let extract_thread_id_flag = Command_bridge_room_routine.extract_thread_id_flag
let show_room_routine = Command_bridge_room_routine.show_room_routine
let cmd_rooms_routine = Command_bridge_room_routine.cmd_rooms_routine
let cmd_rooms_memory = Command_bridge_room_memory.cmd_rooms_memory

let list_inbound_memory_grants ~db ~room_id =
  match Memory.get_scope_by_kind_key ~db ~kind:"room" ~key:room_id with
  | None -> []
  | Some scope -> Memory.list_grants ~db ~scope_id:scope.id

let cmd_rooms_explain_access cfg args =
  match require_admin () with
  | Some err -> err
  | None -> (
      match args with
      | [] ->
          "Error: rooms explain-access requires a room_id.\n\n\
           Usage: clawq rooms explain-access <room_id> [--json]"
      | room_id :: flags ->
          let as_json = List.mem "--json" flags in
          let explanation =
            Access_explanation.create ~config:cfg ~session_key:room_id ()
          in
          (* Check if the room has an active profile binding *)
          let binding =
            (* Match using the same channel-id fallback as
               Runtime_config.resolve_room_profile *)
            let channel_id =
              match String.index_opt room_id ':' with
              | Some i ->
                  String.sub room_id (i + 1) (String.length room_id - i - 1)
              | None -> ""
            in
            List.find_opt
              (fun (b : Runtime_config.room_profile_binding) ->
                b.active
                && (b.room = room_id
                   || (channel_id <> "" && b.room = channel_id)))
              cfg.room_profile_bindings
          in
          (* Lookup profile, filtering out deleted ones *)
          let profile =
            match binding with
            | Some b ->
                List.find_opt
                  (fun (p : Runtime_config.room_profile) ->
                    p.id = b.profile_id && not (room_profile_deleted p))
                  cfg.room_profiles
            | None -> None
          in
          (* Also check if the profile exists but is deleted *)
          let profile_deleted =
            match binding with
            | Some b ->
                List.exists
                  (fun (p : Runtime_config.room_profile) ->
                    p.id = b.profile_id && room_profile_deleted p)
                  cfg.room_profiles
            | None -> false
          in
          if as_json then
            let explanation_json = Access_explanation.to_json explanation in
            let binding_status =
              match (binding, profile) with
              | Some _, Some _ -> "active"
              | Some _, None when profile_deleted -> "deleted"
              | Some _, None -> "missing"
              | None, _ -> "unbound"
            in
            let extra =
              let base =
                [
                  ( "room_binding",
                    `Assoc
                      [
                        ( "bound",
                          `Bool
                            (binding <> None && profile <> None
                           && not profile_deleted) );
                        ("status", `String binding_status);
                        ( "profile_id",
                          match binding with
                          | Some b -> `String b.profile_id
                          | None -> `Null );
                        ("room_id", `String room_id);
                      ] );
                ]
              in
              (* Inbound memory grants: who can learn from this room *)
              let inbound_grants =
                try
                  let db = get_db () in
                  let grants = list_inbound_memory_grants ~db ~room_id in
                  if grants = [] then []
                  else
                    [
                      ( "inbound_memory_grants",
                        `List
                          (List.map
                             (fun (g : Memory.scope_grant) ->
                               `Assoc
                                 [
                                   ("principal_kind", `String g.principal_kind);
                                   ("principal_id", `String g.principal_id);
                                   ("capability", `String g.capability);
                                 ])
                             grants) );
                    ]
                with _ -> []
              in
              base @ inbound_grants
            in
            let merged =
              match explanation_json with
              | `Assoc fields -> `Assoc (fields @ extra)
              | other ->
                  `Assoc
                    [ ("explanation", other); ("room_binding", `Assoc extra) ]
            in
            Yojson.Safe.pretty_to_string merged
          else
            let buf = Buffer.create 2048 in
            let add line =
              Buffer.add_string buf line;
              Buffer.add_char buf '\n'
            in
            add (Access_explanation.to_text explanation);
            (match (binding, profile) with
            | None, _ -> (
                add "";
                add "--- Room Binding Status ---";
                add
                  (Printf.sprintf
                     "Room '%s' is not bound to any profile.\n\
                      To bind it, run: clawq rooms bind %s <profile_id>"
                     room_id room_id);
                let active_profiles =
                  List.filter
                    (fun (p : Runtime_config.room_profile) ->
                      not (room_profile_deleted p))
                    cfg.room_profiles
                in
                match active_profiles with
                | [] ->
                    add
                      "\nNo active room profiles configured. Create one first."
                | profiles ->
                    add "\nAvailable profiles:";
                    List.iter
                      (fun (p : Runtime_config.room_profile) ->
                        add (Printf.sprintf "  - %s (model: %s)" p.id p.model))
                      profiles)
            | Some _b, None when profile_deleted -> (
                add "";
                add "--- Room Binding Status ---";
                add
                  (Printf.sprintf
                     "Room '%s' is bound to profile '%s', but that profile has \
                      been deleted.\n\
                      Rebind to an active profile: clawq rooms bind %s \
                      <profile_id>"
                     room_id
                     (match binding with Some b -> b.profile_id | None -> "")
                     room_id);
                let active_profiles =
                  List.filter
                    (fun (p : Runtime_config.room_profile) ->
                      not (room_profile_deleted p))
                    cfg.room_profiles
                in
                match active_profiles with
                | [] -> add "No active room profiles available."
                | profiles ->
                    add "Active profiles:";
                    List.iter
                      (fun (p : Runtime_config.room_profile) ->
                        add (Printf.sprintf "  - %s (model: %s)" p.id p.model))
                      profiles)
            | Some _b, None ->
                add "";
                add "--- Room Binding Status ---";
                add
                  (Printf.sprintf
                     "Room '%s' is bound to profile '%s', but that profile is \
                      missing from config.\n\
                      Rebind to an active profile: clawq rooms bind %s \
                      <profile_id>"
                     room_id
                     (match binding with Some b -> b.profile_id | None -> "")
                     room_id)
            | Some _b, Some _p -> ());
            (* Inbound memory grants: who can learn from this room *)
            (try
               let db = get_db () in
               let grants = list_inbound_memory_grants ~db ~room_id in
               if grants <> [] then begin
                 add "";
                 add
                   "--- Inbound Memory Grants (who can learn from this room) \
                    ---";
                 List.iter
                   (fun (g : Memory.scope_grant) ->
                     add
                       (Printf.sprintf "  - %s/%s (%s)" g.principal_kind
                          g.principal_id g.capability))
                   grants
               end
             with _ -> ());
            Buffer.contents buf)

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
      (* Show GitHub grants for this room *)
      (try
         let explanation =
           Access_explanation.create ~config:cfg ~session_key:room_id ()
         in
         let has_grants =
           explanation.repo_grants <> []
           || explanation.blocked_repo_grants <> []
           || explanation.codebase_grants <> []
           || explanation.blocked_codebase_grants <> []
         in
         if has_grants then begin
           add "";
           add "--- GitHub Grants ---";
           if explanation.repo_grants <> [] then begin
             add "Granted repos:";
             List.iter
               (fun (ie : Access_explanation.item_explanation) ->
                 match Runtime_config.repo_grant_of_json_string ie.value with
                 | Some rg ->
                     let caps =
                       String.concat ", "
                         (List.map Runtime_config.repo_capability_to_string
                            rg.capabilities)
                     in
                     add (Printf.sprintf "  - %s [%s]" rg.repo caps)
                 | None -> add (Printf.sprintf "  - %s" ie.value))
               explanation.repo_grants
           end;
           if explanation.blocked_repo_grants <> [] then begin
             add "Blocked repos:";
             List.iter
               (fun (ie : Access_explanation.item_explanation) ->
                 match Runtime_config.repo_grant_of_json_string ie.value with
                 | Some rg ->
                     let caps =
                       String.concat ", "
                         (List.map Runtime_config.repo_capability_to_string
                            rg.capabilities)
                     in
                     add (Printf.sprintf "  - %s [%s] (blocked)" rg.repo caps)
                 | None -> add (Printf.sprintf "  - %s (blocked)" ie.value))
               explanation.blocked_repo_grants
           end;
           if explanation.codebase_grants <> [] then begin
             add "Codebase grants:";
             List.iter
               (fun (ie : Access_explanation.item_explanation) ->
                 add (Printf.sprintf "  - %s" ie.value))
               explanation.codebase_grants
           end;
           if explanation.blocked_codebase_grants <> [] then begin
             add "Blocked codebase grants:";
             List.iter
               (fun (ie : Access_explanation.item_explanation) ->
                 add (Printf.sprintf "  - %s (blocked)" ie.value))
               explanation.blocked_codebase_grants
           end
         end
       with _ -> ());
      (* Add delivery failure summary if there are recent failures *)
      (try
         let db = get_db () in
         match
           Room_deliveries_cli.delivery_failure_summary_line ~db ~room_id ()
         with
         | Some line ->
             add "";
             add line
         | None -> ()
       with _ -> ());
      String.concat "\n" (List.rev !lines)
  | [ "workspace"; room_id ] ->
      let path = Room_workspace.workspace_path room_id in
      Printf.sprintf "Workspace for room '%s':\nPreserved path: %s" room_id path
  | "deliveries" :: rest -> Room_deliveries_cli.cmd_rooms_deliveries rest
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
  | "explain-access" :: rest -> cmd_rooms_explain_access cfg rest
  | "session" :: rest -> Room_session_cli.cmd_rooms_session rest
  | "readiness" :: flags ->
      let get_flag name default =
        let rec find = function
          | k :: v :: _ when k = name -> v
          | _ :: rest -> find rest
          | [] -> default
        in
        find flags
      in
      let room_id_str = get_flag "--room-id" "" in
      let profile_id_str = get_flag "--profile-id" "" in
      let as_json = List.mem "--json" flags in
      let room_id = if room_id_str = "" then None else Some room_id_str in
      let profile_id =
        if profile_id_str = "" then None else Some profile_id_str
      in
      let db = try Some (get_db ()) with _ -> None in
      let report =
        Room_readiness_report.generate ~cfg ~db ?room_id ?profile_id ()
      in
      if as_json then Room_readiness_report.format_json report
      else Room_readiness_report.format_text report
  | "audit-export" :: room_id :: flags -> (
      match require_admin () with
      | Some err -> err
      | None ->
          let db = get_db () in
          let as_jsonl = List.mem "--jsonl" flags in
          let as_json = List.mem "--json" flags || as_jsonl in
          Room_activity_ledger.init_schema db;
          let exp = Room_audit_export.generate ~cfg ~db ~room_id () in
          if as_jsonl then Room_audit_export.export_to_jsonl exp
          else if as_json then Room_audit_export.export_to_json_string exp
          else Room_audit_export.format_text exp)
  | "audit-export" :: _ ->
      "Error: audit-export requires a room_id.\n\n\
       Usage: clawq rooms audit-export <room_id> [--json|--jsonl]"
  | "wizard" :: rest -> Setup_room_wizard.run rest
  | _ ->
      "Usage: clawq rooms \
       <list|show|workspace|inspect|ledger|deliveries|gc|bind|rename|delete|unbind|routine|memory|explain-access|session|readiness|audit-export|wizard>\n\n\
       Subcommands:\n\
      \  list                        List all room profiles and bindings\n\
      \  show <room_id>              Show room binding and profile details\n\
      \  workspace <room_id>         Show/create the room workspace path\n\
      \  inspect <room_id>           Inspect ambient watcher state (admin-only)\n\
      \  ledger <list|export|retention-cleanup>\n\
      \                              Query/export/prune room activity ledger \
       entries (admin-only)\n\
      \  deliveries [--room-id ID] [--connector C] [--from TS] [--limit N] \
       [--json]\n\
      \                              Show recent delivery failures (admin-only)\n\
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
      \  memory <list|show|save|correct|forget|team-grant|grant> <room_id> \
       [args...]\n\
      \                              Manage room-scoped memories\n\
      \  explain-access <room_id> [--json]\n\
      \                              Explain effective access for a room \
       (admin-only)\n\
      \  session <list|show|get-latest> [args]\n\
      \                              Query room session records (admin-only)\n\
      \  readiness [--room-id R] [--profile-id P] [--json]\n\
      \                              Show room-agent readiness report\n\
      \  audit-export <room_id> [--json|--jsonl]\n\
      \                              Export room governance audit (admin-only)\n\
      \  wizard [interactive|plan|apply] [options]\n\
      \                              Room-agent pilot wizard with plan/apply \
       flow"

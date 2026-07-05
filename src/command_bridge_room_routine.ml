open Command_bridge_helpers
open Command_bridge_room_common

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

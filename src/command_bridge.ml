open Command_bridge_helpers
open Command_bridge_session

let cmd_transcribe args =
  match args with
  | [] -> "Usage: clawq transcribe <audio_file>"
  | file_path :: _ ->
      if not (Sys.file_exists file_path) then
        Printf.sprintf "File not found: %s" file_path
      else
        let cfg = get_config () in
        let ic = open_in_bin file_path in
        let n = in_channel_length ic in
        let buf = Bytes.create n in
        really_input ic buf 0 n;
        close_in ic;
        let audio_data = Bytes.to_string buf in
        let filename = Filename.basename file_path in
        let content_type = Stt.content_type_of_ext filename in
        let result =
          Lwt_main.run
            (Stt.transcribe ~config:cfg ~audio_data ~filename ~content_type ())
        in
        result.text

let cmd_mcp () =
  let cfg = get_config () in
  if not cfg.mcp.enabled then
    "MCP server is disabled. Set mcp.enabled to true in config."
  else if not cfg.security.tools_enabled then
    "MCP server requires security.tools_enabled=true to expose tools."
  else begin
    let registry =
      match build_tool_registry ~db:(Some (get_db ())) cfg with
      | Some registry -> registry
      | None -> assert false
    in
    (* Filter to exposed_tools allowlist if configured *)
    (match cfg.mcp.exposed_tools with
    | Some allowed ->
        registry.tools <-
          List.filter
            (fun (t : Tool.t) -> List.mem t.name allowed)
            registry.tools
    | None -> ());
    Lwt_main.run (Mcp_server.run ~registry ());
    ""
  end

let agent_argv ~executable = [| executable; "agent" |]

let cmd_agent ?(run_daemon = fun ~config -> Lwt_main.run (Daemon.run ~config))
    ?(execv = Unix.execv) ?(acquire_lock = Service.acquire_singleton_lock)
    ?(release_lock = Service.release_singleton_lock) () =
  let cfg = get_config () in
  match acquire_lock () with
  | None ->
      "Another clawq agent instance already holds the daemon lock. Refusing to \
       start a second live agent."
  | Some lock_fd -> (
      let result =
        try run_daemon ~config:cfg with
        | Failure msg ->
            Logs.err (fun m -> m "%s" msg);
            release_lock (Some lock_fd);
            exit 1
        | exn ->
            let bt = Printexc.get_backtrace () in
            let bt_msg = if bt = "" then "" else "\n" ^ bt in
            Logs.err (fun m ->
                m "Daemon crashed with exception: %s%s" (Printexc.to_string exn)
                  bt_msg);
            release_lock (Some lock_fd);
            exit 1
      in
      match result with
      | Daemon.Shutdown ->
          release_lock (Some lock_fd);
          "Daemon stopped."
      | Daemon.Restart ->
          let executable = Restart_exec.executable () in
          execv executable (agent_argv ~executable);
          "Daemon restart requested.")

let cmd_cron args =
  match args with
  | "list" :: flags | ([] as flags) ->
      let show_prompt = List.mem "--prompt" flags || List.mem "-p" flags in
      let db = get_db () in
      Scheduler.init_schema db;
      let jobs = Scheduler.list_jobs ~db in
      if jobs = [] then "No cron jobs configured."
      else
        let columns =
          let base =
            [
              Table_format.
                { header = "NAME"; align = Left; min_width = 4; flex = false };
              { header = "SESSION"; align = Left; min_width = 7; flex = false };
              { header = "SCHEDULE"; align = Left; min_width = 8; flex = false };
              { header = "ENABLED"; align = Left; min_width = 3; flex = false };
            ]
          in
          if show_prompt then
            base
            @ [
                Table_format.
                  {
                    header = "PROMPT";
                    align = Left;
                    min_width = 10;
                    flex = true;
                  };
              ]
          else base
        in
        let rows =
          List.map
            (fun (j : Scheduler.job) ->
              let base =
                [
                  j.name;
                  j.session_key;
                  j.schedule_str;
                  (if j.enabled then "yes" else "no");
                ]
              in
              if show_prompt then base @ [ j.message ] else base)
            jobs
        in
        "Cron jobs:\n" ^ Table_format.render columns rows
  | "add" :: name :: session_key :: schedule :: message -> (
      let db = get_db () in
      Scheduler.init_schema db;
      let msg = String.concat " " message in
      match Scheduler.add_job ~db ~name ~session_key ~message:msg ~schedule with
      | Ok () -> Printf.sprintf "Added cron job '%s'" name
      | Error e -> Printf.sprintf "Error: %s" e)
  | [ "remove"; name ] ->
      let db = get_db () in
      Scheduler.init_schema db;
      if Scheduler.remove_job ~db ~name then
        Printf.sprintf "Removed job '%s'" name
      else Printf.sprintf "No job found with name '%s'" name
  | "history" :: name :: _ | "runs" :: name :: _ ->
      let db = get_db () in
      Scheduler.init_schema db;
      let runs = Scheduler.get_history ~db ~name ~limit:10 in
      if runs = [] then Printf.sprintf "No run history for '%s'" name
      else
        let columns =
          Table_format.
            [
              { header = "ID"; align = Right; min_width = 2; flex = false };
              { header = "STARTED"; align = Left; min_width = 19; flex = false };
              { header = "STATUS"; align = Left; min_width = 6; flex = false };
              { header = "PREVIEW"; align = Left; min_width = 10; flex = true };
            ]
        in
        let rows =
          List.map
            (fun (r : Scheduler.run) ->
              [
                string_of_int r.run_id;
                r.started_at;
                r.status;
                (match r.result_preview with Some p -> p | None -> "");
              ])
            runs
        in
        Printf.sprintf "Run history for '%s':\n%s" name
          (Table_format.render columns rows)
  | [ "runs" ] ->
      let db = get_db () in
      Scheduler.init_schema db;
      let runs = Scheduler.list_runs ~db ~limit:20 () in
      if runs = [] then "No run history."
      else
        let columns =
          Table_format.
            [
              { header = "ID"; align = Right; min_width = 2; flex = false };
              { header = "JOB"; align = Left; min_width = 3; flex = false };
              { header = "STARTED"; align = Left; min_width = 19; flex = false };
              { header = "STATUS"; align = Left; min_width = 6; flex = false };
              { header = "PREVIEW"; align = Left; min_width = 10; flex = true };
            ]
        in
        let rows =
          List.map
            (fun (r : Scheduler.run) ->
              [
                string_of_int r.run_id;
                r.job_name;
                r.started_at;
                r.status;
                (match r.result_preview with Some p -> p | None -> "");
              ])
            runs
        in
        "Run history:\n" ^ Table_format.render columns rows
  | _ ->
      "Usage: clawq cron <list|add|remove|history|runs>\n\
      \  cron list [--prompt|-p]                      - List all jobs \
       (--prompt shows prompt text)\n\
      \  cron add <name> <session> <schedule> <msg>   - Add a job\n\
      \  cron remove <name>                           - Remove a job\n\
      \  cron history <name>                          - Show run history\n\
      \  cron runs [name]                             - Show all run history"

let cmd_background args =
  match args with
  | [ "list" ] ->
      let db = get_db () in
      Background_task.init_schema db;
      let tasks, hidden = Background_task.list_tasks_for_display ~db in
      Background_task.format_task_list_with_hidden tasks hidden
  | [] ->
      let db = get_db () in
      Background_task.init_schema db;
      let tasks, hidden = Background_task.list_tasks_for_display ~db in
      let list_output =
        Background_task.format_task_list_with_hidden tasks hidden
      in
      list_output
      ^ "\n\n\
         Commands:\n\
        \  background list                                         - List all \
         tasks\n\
        \  background show <id>                                    - Show task \
         details\n\
        \  background add <codex|claude|kimi|gemini|opencode|cursor> [--model \
         <model>] <repo> [--branch <name>] <prompt> - Queue a task\n\
        \  background wait <id> [--timeout <seconds>]              - Wait for \
         completion\n\
        \  background logs <id> [--lines N] [--offset N] [--follow] - Show \
         task logs\n\
        \  background resume <id>                                - Resume a task\n\
        \  background message <id> <message>...                  - Send a \
         message to a task\n\
        \  background cancel <id>                                  - Cancel a \
         task\n\
        \  background retry <id>                                   - Re-queue \
         a failed task\n\
        \  background finalize <id>                                - Rebase, \
         merge and clean up worktree"
  | [ "show"; id_s ] -> (
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: background task id must be an integer"
      else
        match Background_task.get_task ~db ~id with
        | None -> Printf.sprintf "No background task found with id %d" id
        | Some task -> Background_task.format_task_summary task)
  | "add" :: rest -> (
      let cfg = get_config () in
      let db = get_db () in
      Background_task.init_schema db;
      match parse_background_add_args rest with
      | Error msg -> "Error: " ^ msg
      | Ok parsed -> (
          let session_key, channel, channel_id =
            Background_task.routing_from_context ?notify_cfg:cfg.notify ()
          in
          match
            Background_task.enqueue ~db ~runner:parsed.runner
              ?model:parsed.model ~repo_path:parsed.repo_path
              ~prompt:parsed.prompt ?branch:parsed.branch ?session_key ?channel
              ?channel_id ()
          with
          | Ok id ->
              Printf.sprintf
                "Queued background task %d (%s). Use `clawq background wait \
                 %d` or `clawq background show %d` to track it."
                id
                (Background_task.string_of_runner parsed.runner)
                id id
          | Error msg -> "Error: " ^ msg))
  | "wait" :: rest -> (
      let db = get_db () in
      Background_task.init_schema db;
      match parse_background_wait_args rest with
      | Error msg -> "Error: " ^ msg
      | Ok parsed -> (
          let timeout_seconds =
            Float.min parsed.timeout_seconds Background_task.max_wait_seconds
          in
          let result =
            Lwt_main.run
              (Background_task.wait_until_terminal ~timeout_seconds ~db
                 ~id:parsed.id ())
          in
          match result with
          | Background_task.Finished task ->
              Background_task.format_task_summary task
          | Background_task.Timeout task ->
              Printf.sprintf
                "Task %d is still %s after waiting. Run `clawq background wait \
                 %d` to continue waiting, or `clawq background logs %d` to \
                 check progress.\n\n\
                 %s"
                parsed.id
                (Background_task.string_of_status task.status)
                parsed.id parsed.id
                (Background_task.format_task_summary task)
          | Background_task.Interrupted task ->
              Printf.sprintf
                "Task %d is still %s. Run `clawq background wait %d` to \
                 continue waiting, or `clawq background logs %d` to check \
                 progress.\n\n\
                 %s"
                parsed.id
                (Background_task.string_of_status task.status)
                parsed.id parsed.id
                (Background_task.format_task_summary task)
          | Background_task.Not_found ->
              Printf.sprintf "Error: No background task found with id %d"
                parsed.id))
  | "logs" :: rest -> (
      let db = get_db () in
      Background_task.init_schema db;
      match parse_background_logs_args rest with
      | Error msg -> "Error: " ^ msg
      | Ok parsed when parsed.follow && parsed.offset > 0 ->
          "Error: --follow and --offset cannot be used together"
      | Ok parsed when parsed.follow -> (
          let result =
            Lwt_main.run
              (Background_task.log_follow ~db ~id:parsed.id
                 ~initial_lines:parsed.lines ())
          in
          match result with Ok () -> "" | Error msg -> "Error: " ^ msg)
      | Ok parsed -> (
          match Background_task.get_task ~db ~id:parsed.id with
          | None ->
              Printf.sprintf "Error: No background task found with id %d"
                parsed.id
          | Some task -> (
              match
                Background_task.log_excerpt ~offset:parsed.offset
                  ~lines:parsed.lines task
              with
              | Ok text -> text
              | Error msg -> "Error: " ^ msg)))
  | [ "resume"; id_s ] -> (
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: background task id must be an integer"
      else
        match Background_task.request_resume ~db ~id ~message:None with
        | Ok msg -> msg
        | Error msg -> "Error: " ^ msg)
  | "message" :: id_s :: message_parts -> (
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      let message = String.concat " " message_parts |> String.trim in
      if id < 0 then "Error: background task id must be an integer"
      else if message = "" then
        "Usage: clawq background message <id> <message...>"
      else
        match
          Background_task.request_resume ~db ~id ~message:(Some message)
        with
        | Ok msg -> msg
        | Error msg -> "Error: " ^ msg)
  | [ "cancel"; id_s ] -> (
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: background task id must be an integer"
      else
        match Background_task.cancel ~db ~id with
        | Ok msg -> msg
        | Error msg -> "Error: " ^ msg)
  | [ "retry"; id_s ] -> (
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: background task id must be an integer"
      else
        match Background_task.retry ~db ~id with
        | Ok msg -> msg
        | Error msg -> "Error: " ^ msg)
  | [ "finalize"; id_s ] | "finalize" :: id_s :: _ -> (
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: background task id must be an integer"
      else
        match Background_task.get_task ~db ~id with
        | None -> Printf.sprintf "Error: no background task found with id %d" id
        | Some task when task.worktree_path = None ->
            Printf.sprintf
              "Error: task %d has no worktree — nothing to finalize" id
        | Some task ->
            let result = Lwt_main.run (Worktree_merge.finalize_task ~db task) in
            Worktree_merge.format_result result)
  | _ ->
      "Usage: clawq background \
       <list|show|add|wait|logs|resume|message|cancel|retry|finalize>\n\
      \  background list                                         - List queued \
       and completed tasks\n\
      \  background show <id>                                    - Show task \
       details\n\
      \  background add <codex|claude|kimi|gemini|opencode|cursor> [--model \
       <model>] <repo> [--branch <name>] <prompt> - Queue a worktree runner\n\
      \  background wait <id> [--timeout <seconds>]              - Wait for a \
       task to finish\n\
      \  background logs <id> [--lines N] [--offset N] [--follow] - Show task \
       log lines\n\
      \  background resume <id>                                  - Resume a \
       started task with the runner's native session support\n\
      \  background message <id> <message...>                    - Inject a \
       chat message into a started task and resume it\n\
      \  background cancel <id>                                  - Cancel a \
       queued/running task\n\
      \  background retry <id>                                   - Re-queue a \
       failed task (max 3 retries)\n\
      \  background finalize <id>                                - Rebase, \
       merge and clean up a task's worktree"

let cmd_delegate args =
  let cfg = get_config () in
  let db = get_db () in
  Background_task.init_schema db;
  match parse_delegate_args args with
  | Error msg -> "Error: " ^ msg
  | Ok parsed -> (
      match
        Background_task.delegate_enqueue ~db ?notify_cfg:cfg.notify
          ?preferred_runner:parsed.preferred_runner ?model:parsed.model
          ?repo_path:parsed.repo_path ?branch:parsed.branch
          ~default_repo_path:(default_delegate_repo_path cfg)
          ~goal:parsed.goal ()
      with
      | Ok (id, runner, repo_path) ->
          Printf.sprintf
            "Delegated task %d (%s) for %s. Use `clawq background wait %d` or \
             `clawq background show %d` to track it."
            id
            (Background_task.string_of_runner runner)
            repo_path id id
      | Error msg -> "Error: " ^ msg)

let format_audit_table rows =
  if rows = [] then "No audit log entries."
  else
    let columns =
      Table_format.
        [
          { header = "ID"; align = Right; min_width = 2; flex = false };
          { header = "TIMESTAMP"; align = Left; min_width = 19; flex = false };
          { header = "EVENT"; align = Left; min_width = 5; flex = false };
          { header = "TOOL"; align = Left; min_width = 4; flex = false };
          { header = "DETAILS"; align = Left; min_width = 10; flex = true };
        ]
    in
    let tbl_rows =
      List.map
        (fun (r : Audit.row) ->
          [
            string_of_int r.id;
            r.timestamp;
            r.event_type;
            (match r.tool_name with Some n -> n | None -> "");
            (match r.details with
            | Some d ->
                if String.length d > 50 then String.sub d 0 50 ^ "..." else d
            | None -> "");
          ])
        rows
    in
    "Audit log:\n" ^ Table_format.render columns tbl_rows

let cmd_audit args =
  let cfg = get_config () in
  if not cfg.security.audit_enabled then
    "Audit trail is disabled. Set security.audit_enabled to true in config."
  else
    let db = get_db () in
    Audit.init_schema db;
    match args with
    | [ "list" ] | [] ->
        let rows = Audit.query ~db ~limit:20 () in
        format_audit_table rows
    | [ "list"; "--limit"; n ] ->
        let limit = try int_of_string n with _ -> 20 in
        let rows = Audit.query ~db ~limit () in
        format_audit_table rows
    | [ "verify" ] -> (
        match Audit.get_signing_key () with
        | Error msg -> Printf.sprintf "Error: %s" msg
        | Ok key -> (
            let anchor = Audit.get_chain_anchor ~db in
            let signed_entries, unsigned_entries = Audit.signature_counts ~db in
            match Audit.verify_chain ~db ~key with
            | Ok () -> (
                match (anchor, signed_entries, unsigned_entries) with
                | Some _, 0, _ ->
                    "Audit chain verification: OK (no signed entries; stored \
                     retained-chain anchor not exercised)"
                | Some _, _, n when n > 0 ->
                    "Audit chain verification: OK (signed retained suffix \
                     verified against anchor; unsigned entries are \
                     informational only)"
                | Some _, _, _ ->
                    "Audit chain verification: OK (signed retained suffix \
                     verified against anchor)"
                | None, _, _ -> "Audit chain verification: OK")
            | Error (id, reason) ->
                Printf.sprintf "Audit chain verification FAILED at entry %d: %s"
                  id reason))
    | [ "export" ] ->
        let path = cfg.security.audit_retention.export_path in
        let export_file = Filename.concat path "audit_export.jsonl" in
        let count = Audit.export_json ~db ~path:export_file in
        Printf.sprintf "Exported %d audit entries to %s (anchor sidecar: %s)"
          count export_file
          (export_file ^ ".anchor.json")
    | [ "export"; path ] ->
        let count = Audit.export_json ~db ~path in
        Printf.sprintf "Exported %d audit entries to %s (anchor sidecar: %s)"
          count path (path ^ ".anchor.json")
    | [ "import"; path ] -> (
        match Audit.import_json ~db ~path () with
        | Ok (count, Some anchor_path) ->
            Printf.sprintf
              "Imported %d audit entries from %s (anchor sidecar: %s)" count
              path anchor_path
        | Ok (count, None) ->
            Printf.sprintf "Imported %d audit entries from %s" count path
        | Error msg -> Printf.sprintf "Error: %s" msg)
    | [ "import"; path; "--anchor"; anchor_path ] -> (
        match Audit.import_json ~db ~path ~anchor_path () with
        | Ok (count, Some used_anchor) ->
            Printf.sprintf "Imported %d audit entries from %s (anchor: %s)"
              count path used_anchor
        | Ok (count, None) ->
            Printf.sprintf "Imported %d audit entries from %s" count path
        | Error msg -> Printf.sprintf "Error: %s" msg)
    | [ "purge" ] ->
        let ret = cfg.security.audit_retention in
        let deleted =
          Audit.purge_old ~db ~max_age_days:ret.max_age_days
            ~max_entries:ret.max_entries
        in
        Printf.sprintf
          "Purged %d audit entries while preserving a contiguous retained \
           suffix"
          deleted
    | _ ->
        "Usage: clawq audit <list|list --limit N|verify|export [path]|import \
         PATH [--anchor PATH.anchor.json]|purge>"

let cmd_skills args =
  match args with
  | [ "list" ] | [] ->
      let files = Skills.list_skills () in
      if files = [] then "No skills found in " ^ Skills.skills_dir ()
      else "Skills:\n" ^ String.concat "\n" (List.map (fun f -> "  " ^ f) files)
  | [ "path" ] -> "Skills directory: " ^ Skills.skills_dir ()
  | [ "init" ] -> Skills.create_example ()
  | _ -> "Usage: clawq skills <list|path|init>"

let cmd_service args =
  match args with
  | [ "start" ] ->
      let cfg = get_config () in
      Service.cmd_start ~config:cfg
  | [ "stop" ] -> Service.cmd_stop ()
  | [ "status" ] | [] -> Service.cmd_status ()
  | [ "signal-restart" ] -> Service.cmd_signal_restart ()
  | [ "restart" ] ->
      let cfg = get_config () in
      Service.cmd_restart ~config:cfg
  | _ -> "Usage: clawq service <start|stop|status|signal-restart|restart>"

let parse_update_args args =
  match args with
  | [] -> Ok Update_tool.Auto
  | [ "--mode"; value ] -> (
      match
        Update_tool.update_mode_of_string
          (String.lowercase_ascii (String.trim value))
      with
      | Some mode -> Ok mode
      | None ->
          Error
            (Printf.sprintf
               "Invalid update mode '%s'. Use: clawq update [--mode \
                auto|git|binary]"
               value))
  | _ -> Error "Usage: clawq update [--mode auto|git|binary]"

let render_update_output ~progress ~result =
  let progress = List.filter (fun line -> String.trim line <> "") progress in
  match List.rev progress with
  | last :: _ when last = result -> String.concat "\n" progress
  | _ -> String.concat "\n" (progress @ [ result ])

let offline_update_stub mode =
  let progress = ref [] in
  let send_progress text =
    progress := text :: !progress;
    Lwt.return_unit
  in
  let result =
    Lwt_main.run (Update_tool.run_offline_update ~mode ~send_progress ())
  in
  let progress_lines =
    List.rev !progress |> List.filter (fun s -> String.trim s <> "")
  in
  render_update_output ~progress:progress_lines ~result

let cmd_update args =
  match parse_update_args args with
  | Error msg -> msg
  | Ok mode -> (
      let gateway =
        match read_live_daemon_gateway () with
        | Some _ as result -> result
        | None -> try_localhost_gateway ()
      in
      match gateway with
      | None -> offline_update_stub mode
      | Some (host, port) -> (
          let cfg = get_config () in
          let body =
            Yojson.Safe.to_string
              (`Assoc
                 [ ("mode", `String (Update_tool.string_of_update_mode mode)) ])
          in
          let result =
            post_live_gateway_json ~cfg ~host ~port ~path:"/daemon/update" ~body
          in
          match result with
          | Error msg -> Printf.sprintf "Update request failed: %s" msg
          | Ok (status, resp_body) -> (
              match status with
              | 200 -> (
                  try
                    let json = Yojson.Safe.from_string resp_body in
                    let open Yojson.Safe.Util in
                    let progress =
                      json |> member "progress" |> to_list |> List.map to_string
                    in
                    let result = json |> member "result" |> to_string in
                    render_update_output ~progress ~result
                  with _ ->
                    Printf.sprintf
                      "Update request succeeded but returned an unexpected \
                       response: %s"
                      resp_body)
              | 401 | 403 ->
                  Printf.sprintf
                    "Update request was rejected by the live gateway (%d): %s"
                    status
                    (match parse_json_error_body resp_body with
                    | Some msg -> msg
                    | None -> resp_body)
              | _ ->
                  Printf.sprintf "Update request failed (%d): %s" status
                    (match parse_json_error_body resp_body with
                    | Some msg -> msg
                    | None -> resp_body))))

let cmd_runtime args =
  let cfg = get_config () in
  let docker_cfg =
    {
      Runtime_docker.image = cfg.runtime.docker_image;
      container_name = cfg.runtime.docker_container_name;
      port = cfg.runtime.docker_port;
      extra_args = [];
    }
  in
  match args with
  | [ "status" ] | [] ->
      let native_status = Runtime_native.status_string () in
      let docker_status =
        Lwt_main.run (Runtime_docker.status ~docker_config:docker_cfg)
      in
      Printf.sprintf "Runtime status:\n  native: %s\n  docker: %s" native_status
        docker_status
  | [ "native"; "start" ] -> (
      match Runtime_native.start ~config:cfg with
      | Ok () -> "Native runtime started"
      | Error msg -> Printf.sprintf "Error: %s" msg)
  | [ "native"; "stop" ] -> (
      match Runtime_native.stop () with
      | Ok () -> "Native runtime stopped"
      | Error msg -> Printf.sprintf "Error: %s" msg)
  | [ "native"; "health" ] ->
      let healthy = Lwt_main.run (Runtime_native.health ~config:cfg) in
      if healthy then "Native runtime: healthy" else "Native runtime: unhealthy"
  | [ "docker"; "start" ] ->
      Lwt_main.run (Runtime_docker.start ~docker_config:docker_cfg ~config:cfg)
  | [ "docker"; "stop" ] ->
      Lwt_main.run (Runtime_docker.stop ~docker_config:docker_cfg)
  | [ "docker"; "health" ] ->
      let healthy =
        Lwt_main.run (Runtime_docker.health ~docker_config:docker_cfg)
      in
      if healthy then "Docker runtime: healthy" else "Docker runtime: unhealthy"
  | _ ->
      "Usage: clawq runtime <status|native start|native stop|native \
       health|docker start|docker stop|docker health>"

let cmd_tunnel args =
  let cfg = get_config () in
  let provider_name = cfg.tunnel.provider in
  let tunnel_state_path () = Dot_dir.sub "tunnel_state.json" in
  let save_tunnel_state ~pid ~port ~url =
    let start_ticks = proc_start_ticks pid in
    let path = tunnel_state_path () in
    let dir = Filename.dirname path in
    (try if not (Sys.file_exists dir) then Sys.mkdir dir 0o755 with _ -> ());
    let json =
      `Assoc
        [
          ("provider", `String provider_name);
          ("pid", `Int pid);
          ("port", `Int port);
          ("url", `String url);
          ( "start_ticks",
            match start_ticks with Some s -> `String s | None -> `Null );
        ]
    in
    try
      let oc = open_out path in
      output_string oc (Yojson.Safe.pretty_to_string ~std:true json);
      output_char oc '\n';
      close_out oc;
      Ok ()
    with exn -> Error (Printexc.to_string exn)
  in
  let read_tunnel_state () =
    let path = tunnel_state_path () in
    if not (Sys.file_exists path) then None
    else
      try
        let json = Yojson.Safe.from_file path in
        let open Yojson.Safe.Util in
        let pid = json |> member "pid" |> to_int in
        let url = json |> member "url" |> to_string in
        let start_ticks =
          try
            let v = json |> member "start_ticks" in
            if v = `Null then None else Some (to_string v)
          with _ -> None
        in
        Some (pid, url, start_ticks)
      with _ -> None
  in
  let remove_tunnel_state () =
    let path = tunnel_state_path () in
    if Sys.file_exists path then try Sys.remove path with _ -> ()
  in
  if not cfg.tunnel.enabled then
    "Tunnel is disabled in config (set tunnel.enabled=true to use)"
  else
    let process_needle =
      match provider_name with
      | "cloudflare" | "cf" -> "cloudflared"
      | "tailscale" -> "tailscale"
      | "ngrok" -> "ngrok"
      | _ -> provider_name
    in
    let tunnel_pid_matches ~pid ~start_ticks =
      if not (pid_is_alive pid) then false
      else if not (proc_cmdline_contains ~needle:process_needle pid) then false
      else
        match (start_ticks, proc_start_ticks pid) with
        | Some expected, Some actual -> expected = actual
        | _ -> true
    in
    (* Generic tunnel operations using first-class module-like dispatch *)
    let tunnel_start () =
      match provider_name with
      | p when p = Tunnel_cloudflare.name || p = "cf" ->
          let t =
            Tunnel_cloudflare.create ~port:cfg.gateway.port ~config:cfg.tunnel
          in
          Lwt_main.run (Tunnel_cloudflare.start t);
          (Tunnel_cloudflare.get_pid t, Tunnel_cloudflare.get_url t)
      | p when p = Tunnel_tailscale.name ->
          let t =
            Tunnel_tailscale.create ~port:cfg.gateway.port ~config:cfg.tunnel
          in
          Lwt_main.run (Tunnel_tailscale.start t);
          (Tunnel_tailscale.get_pid t, Tunnel_tailscale.get_url t)
      | p when p = Tunnel_ngrok.name ->
          let t =
            Tunnel_ngrok.create ~port:cfg.gateway.port ~config:cfg.tunnel
          in
          Lwt_main.run (Tunnel_ngrok.start t);
          (Tunnel_ngrok.get_pid t, Tunnel_ngrok.get_url t)
      | p when p = Tunnel_custom.name ->
          let custom_command =
            try Sys.getenv "CLAWQ_TUNNEL_COMMAND" with Not_found -> ""
          in
          if custom_command = "" then begin
            Printf.eprintf
              "Custom tunnel requires CLAWQ_TUNNEL_COMMAND env var\n";
            (None, None)
          end
          else
            let t =
              Tunnel_custom.create ~port:cfg.gateway.port ~config:cfg.tunnel
                ~custom_command
                ~url_regex:
                  (try Sys.getenv "CLAWQ_TUNNEL_URL_REGEX"
                   with Not_found -> "https://[a-zA-Z0-9._/-]+")
            in
            Lwt_main.run (Tunnel_custom.start t);
            (Tunnel_custom.get_pid t, Tunnel_custom.get_url t)
      | _ ->
          Printf.eprintf "Unknown tunnel provider: %s\n" provider_name;
          (None, None)
    in
    match args with
    | [ "start" ] -> (
        let pid_url = tunnel_start () in
        match pid_url with
        | Some pid, Some url -> (
            match save_tunnel_state ~pid ~port:cfg.gateway.port ~url with
            | Ok () -> Printf.sprintf "Tunnel started: %s (pid %d)" url pid
            | Error err ->
                Printf.sprintf
                  "Tunnel started: %s (pid %d)\n\
                   Warning: failed to save state: %s"
                  url pid err)
        | _ -> "Tunnel started but URL or PID not available")
    | [ "stop" ] -> (
        match read_tunnel_state () with
        | None -> "No running tunnel state found"
        | Some (pid, _url, start_ticks) ->
            if not (tunnel_pid_matches ~pid ~start_ticks) then begin
              remove_tunnel_state ();
              Printf.sprintf
                "Refusing to stop pid %d: tunnel process identity mismatch; \
                 stale state removed"
                pid
            end
            else begin
              (try Unix.kill pid Sys.sigterm with _ -> ());
              let rec wait_for_exit attempts =
                if attempts <= 0 then false
                else
                  try
                    Unix.kill pid 0;
                    Unix.sleepf 0.2;
                    wait_for_exit (attempts - 1)
                  with Unix.Unix_error _ -> true
              in
              if wait_for_exit 20 then begin
                remove_tunnel_state ();
                Printf.sprintf "Tunnel stopped (pid %d)" pid
              end
              else
                Printf.sprintf
                  "Tunnel stop signal sent but process still running (pid %d)"
                  pid
            end)
    | [ "status" ] | [] -> (
        match read_tunnel_state () with
        | None ->
            Printf.sprintf
              "Tunnel provider: %s\n\
              \  Status: stopped\n\
              \  To start: clawq tunnel start"
              provider_name
        | Some (pid, url, start_ticks) ->
            let running = tunnel_pid_matches ~pid ~start_ticks in
            if running then
              Printf.sprintf
                "Tunnel provider: %s\n  Status: running (pid %d)\n  URL: %s"
                provider_name pid url
            else begin
              remove_tunnel_state ();
              Printf.sprintf
                "Tunnel provider: %s\n  Status: stopped (stale state cleaned)"
                provider_name
            end)
    | [ "apply" ] -> Lwt_main.run (!Tunnel_manager.daemon_apply_fn ())
    | [ "restart" ] -> Lwt_main.run (!Tunnel_manager.daemon_restart_fn ())
    | [ "daemon-status" ] -> !Tunnel_manager.daemon_status_fn ()
    | _ -> "Usage: clawq tunnel <start|stop|status|apply|restart|daemon-status>"

let cmd_reset_agent () =
  let cfg = get_config () in
  let workspace = Runtime_config.effective_workspace cfg in
  let db_path =
    if cfg.memory.db_path <> "" then cfg.memory.db_path else Dot_dir.db_path ()
  in
  let red s = "\027[1;31m" ^ s ^ "\027[0m" in
  let bold s = "\027[1m" ^ s ^ "\027[0m" in
  let dim s = "\027[2m" ^ s ^ "\027[0m" in
  print_endline "";
  print_endline (red "  !! RESET AGENT !!");
  print_endline "";
  print_endline "  This will permanently delete:";
  print_endline
    ("    "
    ^ bold "· All conversation history  "
    ^ dim ("(" ^ db_path ^ " — messages, embeddings)"));
  print_endline
    ("    "
    ^ bold "· All cron jobs and run logs  "
    ^ dim "(cron_jobs, cron_runs)");
  print_endline
    ("    "
    ^ bold "· All workspace identity files  "
    ^ dim ("(" ^ workspace ^ "/)"));
  print_endline
    (dim
       "      EGO.md  AGENTS.md  USER.md  IDENTITY.md  TOOLS.md  HEARTBEAT.md  \
        BOOTSTRAP.md");
  print_endline "";
  print_endline "  This will NOT touch:";
  print_endline (dim "    · config.json");
  print_endline (dim "    · daemon.log  daemon.pid");
  print_endline (dim "    · background tasks (queued/running/finished)");
  print_endline "";
  print_string "  Type ";
  print_string (bold "RESET");
  print_string " to confirm, or anything else to cancel: ";
  flush stdout;
  let answer = try input_line stdin with End_of_file -> "" in
  print_endline "";
  if String.trim answer <> "RESET" then begin
    print_endline "  Cancelled. Nothing changed.";
    print_endline "";
    "Cancelled."
  end
  else begin
    let db =
      Memory.init ~db_path ~search_enabled:cfg.memory.search_enabled ()
    in
    let exec sql =
      let stmt = Sqlite3.prepare db sql in
      ignore (Sqlite3.step stmt);
      ignore (Sqlite3.finalize stmt)
    in
    exec "DELETE FROM messages";
    exec "DELETE FROM embeddings";
    exec "DELETE FROM cron_jobs";
    exec "DELETE FROM cron_runs";
    Background_task.init_schema db;
    let active_bg = Background_task.count_active ~db in
    ignore (Sqlite3.db_close db);
    List.iter
      (fun (name, content) ->
        let path = Filename.concat workspace name in
        try
          let oc = open_out path in
          output_string oc content;
          close_out oc
        with _ -> ())
      Workspace_scaffold.templates;
    print_endline "  Done:";
    print_endline "    · Conversation history cleared";
    print_endline "    · Cron jobs and run logs cleared";
    print_endline "    · Workspace files redeployed from defaults";
    if active_bg > 0 then
      print_endline
        (Printf.sprintf
           "    · Note: %d active background task(s) continue running" active_bg);
    print_endline "";
    "Agent reset complete."
  end

let cmd_reset_workspace () =
  let cfg = get_config () in
  let workspace = Runtime_config.effective_workspace cfg in
  let db_path =
    if cfg.memory.db_path <> "" then cfg.memory.db_path else Dot_dir.db_path ()
  in
  let red s = "\027[1;31m" ^ s ^ "\027[0m" in
  let bold s = "\027[1m" ^ s ^ "\027[0m" in
  let dim s = "\027[2m" ^ s ^ "\027[0m" in
  print_endline "";
  print_endline (red "  !! RESET WORKSPACE !!");
  print_endline "";
  print_endline "  This will permanently delete:";
  print_endline
    ("    "
    ^ bold "· All conversation history  "
    ^ dim ("(" ^ db_path ^ " — messages, embeddings)"));
  print_endline
    ("    "
    ^ bold "· All workspace identity files  "
    ^ dim ("(" ^ workspace ^ "/)"));
  print_endline
    (dim
       "      EGO.md  AGENTS.md  USER.md  IDENTITY.md  TOOLS.md  HEARTBEAT.md  \
        BOOTSTRAP.md");
  print_endline "";
  print_endline "  This will NOT touch:";
  print_endline (dim "    · config.json");
  print_endline (dim "    · daemon.log  daemon.pid");
  print_endline (dim "    · cron jobs and run logs");
  print_endline (dim "    · background tasks (queued/running/finished)");
  print_endline "";
  print_string "  Type ";
  print_string (bold "RESET");
  print_string " to confirm, or anything else to cancel: ";
  flush stdout;
  let answer = try input_line stdin with End_of_file -> "" in
  print_endline "";
  if String.trim answer <> "RESET" then begin
    print_endline "  Cancelled. Nothing changed.";
    print_endline "";
    "Cancelled."
  end
  else begin
    let db =
      Memory.init ~db_path ~search_enabled:cfg.memory.search_enabled ()
    in
    let exec sql =
      let stmt = Sqlite3.prepare db sql in
      ignore (Sqlite3.step stmt);
      ignore (Sqlite3.finalize stmt)
    in
    exec "DELETE FROM messages";
    exec "DELETE FROM embeddings";
    Background_task.init_schema db;
    let active_bg = Background_task.count_active ~db in
    ignore (Sqlite3.db_close db);
    List.iter
      (fun (name, content) ->
        let path = Filename.concat workspace name in
        try
          let oc = open_out path in
          output_string oc content;
          close_out oc
        with _ -> ())
      Workspace_scaffold.templates;
    print_endline "  Done:";
    print_endline "    · Conversation history cleared";
    print_endline "    · Workspace files redeployed from defaults";
    if active_bg > 0 then
      print_endline
        (Printf.sprintf
           "    · Note: %d active background task(s) continue running" active_bg);
    print_endline "";
    "Workspace reset complete."
  end

let cmd_otp_show () =
  let cfg = get_config () in
  let lines = ref [] in
  let add line = lines := line :: !lines in
  (match read_live_gateway_pairing_code () with
  | Some code -> add (Printf.sprintf "  gateway: %s" code)
  | None -> ());
  (match cfg.channels.telegram with
  | None -> ()
  | Some tg ->
      List.iter
        (fun (name, (acct : Runtime_config.telegram_account)) ->
          match acct.totp with
          | Some t when t.totp_enabled && t.totp_secret <> "" ->
              let time = Unix.gettimeofday () in
              let code = Totp.generate_totp ~secret:t.totp_secret ~time in
              let remaining = Totp.time_remaining ~time in
              add
                (Printf.sprintf "  telegram/%s: %s (expires in %ds)" name code
                   remaining)
          | _ -> ())
        tg.accounts);
  let results = List.rev !lines in
  if results <> [] then "Current pairing codes:\n" ^ String.concat "\n" results
  else if cfg.gateway.require_pairing then
    "No live gateway pairing code found. Start `clawq agent` and rerun `clawq \
     otp-show`, or configure Telegram TOTP pairing."
  else
    "Pairing is not configured. Enable `gateway.require_pairing` or configure \
     Telegram TOTP pairing."

let debug_html_preview_pages =
  [
    ( "/",
      Html_page.render ~title:"Index" ~extra_css:""
        ~body_html:
          {|<h1>Html_page Preview</h1>
  <span class="label label-ok">index</span>
  <p><a href="/ok">Auth success page</a></p>
  <p><a href="/error">Auth error page</a></p>
  <p><a href="/custom">Custom content</a></p>
  <div class="qed">&#9632;</div>|}
    );
    ("/ok", Openai_codex_oauth.callback_page_ok);
    ( "/error",
      Openai_codex_oauth.callback_page_error "State Mismatch"
        "The OAuth state parameter did not match. Please retry the login flow."
    );
    ( "/custom",
      Html_page.render ~title:"Custom Example" ~extra_css:""
        ~body_html:
          {|<h1>Custom Page</h1>
  <span class="label label-ok">example</span>
  <p>This demonstrates using Html_page.render with arbitrary content.</p>
  <p><a href="/">Back to index</a></p>
  <div class="qed">&#9632;</div>|}
    );
  ]

let cmd_debug_context args =
  let cfg : Runtime_config.t = get_config () in
  let db = get_db () in
  let session_key = match args with [] -> "__main__" | key :: _ -> key in
  let sandbox = make_sandbox cfg in
  let shell_policy, shell_is_sandboxed = shell_policy_summary cfg sandbox in
  Background_task.init_schema db;
  let background_tasks =
    Background_task.list_tasks ~db
    |> List.filter (fun (t : Background_task.task) ->
        match t.Background_task.status with
        | Background_task.Queued | Background_task.Running -> true
        | _ -> false)
    |> List.sort (fun (a : Background_task.task) (b : Background_task.task) ->
        compare a.Background_task.id b.Background_task.id)
    |> List.map (fun (t : Background_task.task) ->
        {
          Prompt_builder.id = t.Background_task.id;
          runner = Background_task.string_of_runner t.runner;
          repo_label = Filename.basename t.repo_path;
          branch = (if t.branch = "" then "(auto)" else t.branch);
          status = Background_task.string_of_status t.status;
          health =
            Background_task.string_of_health (Background_task.diagnose_health t);
          elapsed = Background_task.elapsed_string t;
        })
  in
  Task_tree.init_schema db;
  let task_tree_summary =
    Some (Task_tree.render_tree_with_legend ~db ~session_key)
  in
  let is_main = session_key = "__main__" in
  let heartbeat_routing_applies =
    cfg.heartbeat.heartbeat_enabled
    && Session.heartbeat_supported_session_key session_key
    && Memory.session_heartbeat_enabled ~db ~session_key
  in
  let details =
    {
      Prompt_builder.session_id = session_key;
      session_name = (if is_main then Some "main" else None);
      is_main_session = is_main;
      heartbeat_routing_applies;
      effective_workspace = Runtime_config.effective_workspace cfg;
      workspace_only = cfg.security.workspace_only;
      sandbox_backend_requested = cfg.security.sandbox_backend;
      sandbox_backend_effective =
        Sandbox.backend_to_string sandbox.Sandbox.backend;
      shell_is_sandboxed;
      shell_policy_summary = shell_policy;
      shell_visible_roots_summary = shell_visible_roots_summary cfg;
      daemon_uptime_line =
        Daemon_status.daemon_runtime_context_line
          ~pid:(Daemon_status.read_current_daemon_pid ());
      background_tasks;
      context_usage = None;
      tunnel_status_line =
        Some ("- Tunnel: " ^ !Prompt_builder.tunnel_status_line_fn ());
      task_tree_summary;
    }
  in
  match Prompt_builder.build_runtime_context ~config:cfg ~details () with
  | Some ctx -> ctx
  | None -> "(dynamic prompt disabled — no runtime context generated)"

let cmd_debug args =
  match args with
  | "prompt" :: rest -> cmd_debug_prompt rest
  | "context" :: rest -> cmd_debug_context rest
  | [ "html-preview" ] | [ "html-preview"; _ ] ->
      let port =
        match args with
        | [ _; p ] -> ( try int_of_string p with _ -> 8099)
        | _ -> 8099
      in
      Printf.printf "Serving Html_page preview on http://localhost:%d\n%!" port;
      Printf.printf "Pages: /  /ok  /error  /custom\n%!";
      Printf.printf "Press Ctrl-C to stop.\n%!";
      let _ : string =
        Lwt_main.run
          (let open Lwt.Syntax in
           let* _server =
             Lwt_io.establish_server_with_client_address
               (Unix.ADDR_INET (Unix.inet_addr_loopback, port))
               (fun _addr (ic, oc) ->
                 Lwt.catch
                   (fun () ->
                     let* request_line = Lwt_io.read_line ic in
                     let path =
                       match String.split_on_char ' ' request_line with
                       | _ :: target :: _ -> target
                       | _ -> "/"
                     in
                     let body =
                       match List.assoc_opt path debug_html_preview_pages with
                       | Some page -> page
                       | None ->
                           Html_page.render ~title:"Not Found" ~extra_css:""
                             ~body_html:
                               (Printf.sprintf
                                  {|<h1>Not Found</h1>
  <span class="label label-error">404</span>
  <p>No page at <code>%s</code>.</p>
  <p><a href="/">Back to index</a></p>
  <div class="qed">&#9632;</div>|}
                                  path)
                     in
                     let* () =
                       Lwt_io.write oc
                         (Printf.sprintf
                            "HTTP/1.1 200 OK\r\n\
                             Content-Type: text/html; charset=utf-8\r\n\
                             Content-Length: %d\r\n\
                             Connection: close\r\n\
                             \r\n\
                             %s"
                            (String.length body) body)
                     in
                     Lwt_io.flush oc)
                   (fun _exn -> Lwt.return_unit))
           in
           let waiter, _wakener = Lwt.wait () in
           let* () = waiter in
           Lwt.return "debug html-preview: stopped")
      in
      "debug html-preview: stopped"
  | _ ->
      "Usage: clawq debug context [SESSION_KEY]\n\
       Prints the runtime context block for a session (default: __main__).\n\n\
       Usage: clawq debug html-preview [PORT]\n\
       Serves Html_page test pages on localhost (default port 8099).\n\n\
       Usage: clawq debug prompt [MESSAGE]\n\
       Prints the normalized logical messages for a single agent turn."

let cmd_setup args =
  match args with
  | [ "discord" ] -> Setup_discord.run ()
  | [ "github" ] -> Setup_github.run ()
  | [ "slack" ] -> Setup_slack.run ()
  | [ "teams" ] -> Setup_teams.run ()
  | [ "telegram" ] -> Setup_telegram.run ()
  | [ "tunnel" ] -> Setup_tunnel.run ()
  | [ "summarizer" ] -> Setup_summarizer.run ()
  | _ ->
      "Usage: clawq setup <channel>\n\n\
       Available channels:\n\
      \  discord     Configure Discord bot integration\n\
      \  github      Configure GitHub webhook integration\n\
      \  slack       Configure Slack integration\n\
      \  summarizer  Configure autosummarizer settings\n\
      \  teams       Configure MS Teams bot integration\n\
      \  telegram    Configure Telegram bot integration\n\
      \  tunnel      Configure Cloudflare tunnel\n\n\
       Documentation: https://clawq.org/channels/\n"

let handle args =
  match args with
  | "phase2" :: _ -> Phase2.render ()
  | "agent" :: _ -> cmd_agent ()
  | "status" :: _ -> cmd_status ()
  | "config" :: rest -> cmd_config rest
  | "doctor" :: _ -> cmd_doctor ()
  | "onboard" :: _ -> cmd_onboard ()
  | "models" :: rest -> cmd_models rest
  | "costs" :: rest -> cmd_costs rest
  | "usage" :: rest ->
      let refresh = List.mem "--refresh" rest || List.mem "-r" rest in
      cmd_usage refresh
  | "provider" :: rest -> cmd_provider rest
  | "channel" :: "test" :: "teams" :: _ -> cmd_channel_test_teams ()
  | "channel" :: _ -> cmd_channel ()
  | "memory" :: rest -> cmd_memory rest
  | "session" :: rest -> cmd_session rest
  | "workspace" :: _ -> cmd_workspace ()
  | "capabilities" :: _ -> cmd_capabilities ()
  | "auth" :: rest -> cmd_auth rest
  | "transcribe" :: rest -> cmd_transcribe rest
  | "mcp" :: _ -> cmd_mcp ()
  | "cron" :: rest -> cmd_cron rest
  | "background" :: rest -> cmd_background rest
  | "delegate" :: rest -> cmd_delegate rest
  | "skills" :: rest -> cmd_skills rest
  | "audit" :: rest -> cmd_audit rest
  | "runtime" :: rest -> cmd_runtime rest
  | "tunnel" :: rest -> cmd_tunnel rest
  | "update" :: rest -> cmd_update rest
  | "hardware" :: _ -> "hardware: deferred to Phase 2"
  | "migrate" :: rest -> Migrate.cmd_migrate rest
  | "service" :: rest -> cmd_service rest
  | "reset-agent" :: _ -> cmd_reset_agent ()
  | "reset-workspace" :: _ -> cmd_reset_workspace ()
  | "otp-show" :: _ -> cmd_otp_show ()
  | "debug" :: rest -> cmd_debug rest
  | "setup" :: rest -> cmd_setup rest
  | "plan" :: rest -> cmd_plan rest
  | "benchmark" :: rest -> Benchmark.run rest
  | "completions" :: rest -> Completions.cmd_completions rest
  | _ -> Clawq_core.dispatch args

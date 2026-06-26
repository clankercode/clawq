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
        let audio_data =
          Fun.protect
            ~finally:(fun () -> close_in_noerr ic)
            (fun () ->
              let n = in_channel_length ic in
              let buf = Bytes.create n in
              really_input ic buf 0 n;
              Bytes.to_string buf)
        in
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

let cmd_runner args =
  let cfg = get_config () in
  match args with
  | "token" :: rest -> (
      let session_key =
        match rest with
        | "--session" :: sk :: _ -> sk
        | sk :: _ when not (String.length sk > 0 && sk.[0] = '-') -> sk
        | _ -> ""
      in
      if session_key = "" then
        "Usage: clawq runner token --session <session_key> [--ttl-hours N]"
      else
        let ttl_hours =
          let rec find = function
            | "--ttl-hours" :: n :: _ -> ( try int_of_string n with _ -> 24)
            | _ :: tl -> find tl
            | [] -> cfg.mcp.runner_token_ttl_hours
          in
          find rest
        in
        let port = cfg.gateway.port in
        let url = Printf.sprintf "http://127.0.0.1:%d/runner/token" port in
        let body =
          `Assoc
            [
              ("session_key", `String session_key); ("ttl_hours", `Int ttl_hours);
            ]
          |> Yojson.Safe.to_string
        in
        let headers =
          match cfg.gateway.auth_token with
          | Some tok -> [ ("Authorization", "Bearer " ^ tok) ]
          | None -> []
        in
        let result =
          Lwt_main.run
            (Http_client.post_json_with_headers ~uri:url ~body ~headers)
        in
        let _status, _resp_headers, resp_body = result in
        try
          let json = Yojson.Safe.from_string resp_body in
          let token = Yojson.Safe.Util.(json |> member "token" |> to_string) in
          Printf.sprintf
            "Runner token: %s\n\
             MCP URL: http://127.0.0.1:%d/mcp\n\
             REST URL: http://127.0.0.1:%d/runner/ask"
            token port port
        with _ -> Printf.sprintf "Error: unexpected response: %s" resp_body)
  | _ ->
      "Usage: clawq runner <command>\n\
      \  runner token --session <session_key> [--ttl-hours N]  - Generate a \
       runner auth token"

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
      if jobs = [] then
        "No cron jobs configured. Use 'clawq cron add' to create one."
      else
        let columns =
          let base =
            [
              Table_format.
                { header = "NAME"; align = Left; min_width = 4; flex = false };
              { header = "SESSION"; align = Left; min_width = 7; flex = false };
              { header = "SCHEDULE"; align = Left; min_width = 8; flex = false };
              { header = "ENABLED"; align = Left; min_width = 3; flex = false };
              { header = "EPH"; align = Left; min_width = 3; flex = false };
              { header = "EXPIRES"; align = Left; min_width = 3; flex = false };
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
                  (if j.ephemeral then "yes" else "no");
                  (match j.expires_at with Some ea -> ea | None -> "-");
                ]
              in
              if show_prompt then base @ [ j.message ] else base)
            jobs
        in
        Format_adapter.bold Format_adapter.Plain "Cron Jobs"
        ^ "\n\n"
        ^ Format_adapter.render_table Format_adapter.Plain ~max_width:80 columns
            rows
  | [ "show"; name ] -> (
      let db = get_db () in
      Scheduler.init_schema db;
      match Scheduler.get_job ~db ~name with
      | None -> Printf.sprintf "No cron job found with name '%s'." name
      | Some (job : Scheduler.job) ->
          let connector = Format_adapter.Plain in
          let runs = Scheduler.get_history ~db ~name ~limit:5 in
          let doc =
            [
              Content_dsl.Paragraph
                [ Bold "Cron Job"; Text " — "; Code job.name ];
              Paragraph [ Text "Session: "; Code job.session_key ];
              Paragraph [ Text "Schedule: "; Code job.schedule_str ];
              Paragraph
                [ Text "Enabled: "; Text (if job.enabled then "yes" else "no") ];
              Paragraph
                [
                  Text "Ephemeral: ";
                  Text (if job.ephemeral then "yes" else "no");
                ];
              Paragraph
                [
                  Text "Expires: ";
                  Text
                    (match job.expires_at with
                    | Some ea -> ea
                    | None -> "never");
                ];
            ]
            @ (match job.agent_name with
              | Some agent ->
                  [ Content_dsl.Paragraph [ Text "Agent: "; Code agent ] ]
              | None -> [])
            @ [
                Content_dsl.Separator;
                Paragraph [ Bold "Message" ];
                CodeBlock { language = None; content = job.message };
              ]
            @
            if runs = [] then
              [ Content_dsl.Paragraph [ Italic "No run history." ] ]
            else
              let history_columns =
                Table_format.
                  [
                    {
                      header = "ID";
                      align = Right;
                      min_width = 2;
                      flex = false;
                    };
                    {
                      header = "STARTED";
                      align = Left;
                      min_width = 19;
                      flex = false;
                    };
                    {
                      header = "STATUS";
                      align = Left;
                      min_width = 6;
                      flex = false;
                    };
                    {
                      header = "PREVIEW";
                      align = Left;
                      min_width = 10;
                      flex = true;
                    };
                  ]
              in
              let history_rows =
                List.map
                  (fun (r : Scheduler.run) ->
                    let preview =
                      match r.result_preview with
                      | Some p when String.length p > 40 ->
                          String.sub p 0 37 ^ "..."
                      | Some p -> p
                      | None -> ""
                    in
                    [ string_of_int r.run_id; r.started_at; r.status; preview ])
                  runs
              in
              [
                Content_dsl.Separator;
                Paragraph [ Bold "Recent Runs" ];
                Paragraph
                  [
                    Text
                      (Format_adapter.render_table connector ~max_width:70
                         history_columns history_rows);
                  ];
              ]
          in
          Content_dsl.render_document connector doc)
  | "add" :: name :: session_key :: schedule :: message -> (
      let db = get_db () in
      Scheduler.init_schema db;
      let ephemeral = List.mem "--ephemeral" message in
      let message = List.filter (fun s -> s <> "--ephemeral") message in
      let rec extract_ttl acc = function
        | "--ttl" :: v :: rest -> (Some v, List.rev_append acc rest)
        | x :: rest -> extract_ttl (x :: acc) rest
        | [] -> (None, List.rev acc)
      in
      let ttl, message = extract_ttl [] message in
      let msg = String.concat " " message in
      match
        Scheduler.add_job ~db ~name ~session_key ~message:msg ~schedule
          ~ephemeral ?ttl ()
      with
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
        Format_adapter.bold Format_adapter.Plain
          (Printf.sprintf "Run History — %s" name)
        ^ "\n\n"
        ^ Format_adapter.render_table Format_adapter.Plain ~max_width:80 columns
            rows
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
        Format_adapter.bold Format_adapter.Plain "Run History"
        ^ "\n\n"
        ^ Format_adapter.render_table Format_adapter.Plain ~max_width:80 columns
            rows
  | [ "trigger"; name ] | [ "run"; name ] -> (
      let db = get_db () in
      Scheduler.init_schema db;
      Background_task.init_schema db;
      match Scheduler.trigger_job ~db ~name with
      | Ok task_id ->
          Printf.sprintf
            "Triggered cron job '%s' — enqueued as background task %d.\n\
             Use 'clawq background show %d' to check progress."
            name task_id task_id
      | Error e -> Printf.sprintf "Error: %s" e)
  (* B587: explicit enable/disable so operators can pause a misbehaving cron
     without removing it (and losing its schedule + prompt). *)
  | [ "disable"; name ] -> (
      let db = get_db () in
      Scheduler.init_schema db;
      match Scheduler.get_job ~db ~name with
      | None -> Printf.sprintf "No cron job found with name '%s'." name
      | Some j when not j.enabled ->
          Printf.sprintf "Cron job '%s' is already disabled." name
      | Some _ -> (
          match Scheduler.toggle_job ~db ~name with
          | Ok () -> Printf.sprintf "Disabled cron job '%s'." name
          | Error e -> Printf.sprintf "Error: %s" e))
  | [ "enable"; name ] -> (
      let db = get_db () in
      Scheduler.init_schema db;
      match Scheduler.get_job ~db ~name with
      | None -> Printf.sprintf "No cron job found with name '%s'." name
      | Some j when j.enabled ->
          Printf.sprintf "Cron job '%s' is already enabled." name
      | Some _ -> (
          match Scheduler.toggle_job ~db ~name with
          | Ok () -> Printf.sprintf "Enabled cron job '%s'." name
          | Error e -> Printf.sprintf "Error: %s" e))
  | _ ->
      "Usage: clawq cron \
       <list|show|add|remove|enable|disable|trigger|history|runs>\n\
      \  cron list [--prompt|-p]                      - List all jobs \
       (--prompt shows prompt text)\n\
      \  cron show <name>                             - Show job details\n\
      \  cron add <name> <session> <schedule> <msg> [--ephemeral] [--ttl \
       <duration>] - Add a job\n\
      \  cron remove <name>                           - Remove a job\n\
      \  cron enable <name>                           - Enable a paused job\n\
      \  cron disable <name>                          - Pause job (keeps \
       schedule + prompt)\n\
      \  cron trigger <name>                          - Trigger a job \
       immediately\n\
      \  cron history <name>                          - Show run history\n\
      \  cron runs [name]                             - Show all run history\n\
       Schedule format: \"every 5m\" (interval) or standard 5-field cron (e.g. \
       \"0 9 * * 1-5\" for weekdays at 9am)\n\
       TTL duration: e.g. 24h, 7d, 30m (job auto-disables after this time)"

type transcript_args = {
  id : int;
  regex : string option;
  max_lines : int option;
  export : bool;
}

let parse_transcript_args args =
  let rec loop id regex max_lines export = function
    | [] -> (
        match id with
        | Some id -> Ok { id; regex; max_lines; export }
        | None ->
            Error
              "Usage: transcript <id> [--regex PATTERN] [--max-lines N] \
               [--export]")
    | "--regex" :: value :: rest -> loop id (Some value) max_lines export rest
    | "--max-lines" :: value :: rest -> (
        try loop id regex (Some (int_of_string value)) export rest
        with _ -> Error "--max-lines must be an integer")
    | "--export" :: rest -> loop id regex max_lines true rest
    | arg :: rest -> (
        match id with
        | Some _ ->
            Error
              "Usage: transcript <id> [--regex PATTERN] [--max-lines N] \
               [--export]"
        | None -> (
            try loop (Some (int_of_string arg)) regex max_lines export rest
            with _ -> Error "background task id must be an integer"))
  in
  loop None None None false args

let cmd_background_transcript rest =
  let db = get_db () in
  Background_task.init_schema db;
  match parse_transcript_args rest with
  | Error msg -> "Error: " ^ msg
  | Ok parsed ->
      Background_task_transcript.render ~db ~id:parsed.id ?regex:parsed.regex
        ?max_lines:parsed.max_lines ~export:parsed.export ()

let require_local_subagent_task ~db id_s =
  let id = try int_of_string id_s with _ -> -1 in
  if id < 0 then Error "background task id must be an integer"
  else
    match Background_task.get_task ~db ~id with
    | None -> Error (Printf.sprintf "No background task found with id %d" id)
    | Some task when task.runner <> Background_task.Local ->
        Error
          (Printf.sprintf
             "Task %d is not a native/local subagent (runner is %s). Use \
              `clawq background ...` for non-local background tasks."
             id
             (Background_task.string_of_runner task.runner))
    | Some _ -> Ok id

let rec cmd_background args =
  match args with
  | "start" :: rest -> cmd_background ("add" :: rest)
  | [ "stop"; id_s ] -> cmd_background [ "cancel"; id_s ]
  | "send" :: id_s :: message_parts ->
      cmd_background ("message" :: id_s :: message_parts)
  | "transcript" :: rest -> cmd_background_transcript rest
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
        \  background add <codex|claude|kimi|gemini|opencode|cursor|local> \
         [--model <model>] [--agent <name>] <repo> [--branch <name>] <prompt> \
         - Queue a task\n\
        \  background start <runner> ...                           - Alias for \
         background add\n\
        \  background wait <id> [--timeout <seconds>]              - Wait for \
         completion\n\
        \  background logs <id> [--lines N] [--offset N] [--follow] - Show \
         task logs\n\
        \  background transcript <id> [--regex R] [--max-lines N] [--export] - \
         Show bounded task transcript\n\
        \  background resume <id>                                - Resume a task\n\
        \  background message <id> <message>...                  - Send a \
         message to a task\n\
        \  background send <id> <message>...                     - Alias for \
         background message\n\
        \  background cancel <id>                                  - Cancel a \
         task\n\
        \  background stop <id>                                    - Alias for \
         background cancel\n\
        \  background retry <id>                                   - Re-queue \
         a failed task\n\
        \  background recover <id> [--runner R] [--model M]        - Recover a \
         failed/stuck task with full context\n\
        \  background finalize <id>                                - Rebase \
         and fast-forward into target branch\n\
        \  background export-acp <id>                              - Export \
         ACP session history as JSONL"
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
              ?model:parsed.model ?agent_name:parsed.agent_name
              ~repo_path:parsed.repo_path ~prompt:parsed.prompt
              ?branch:parsed.branch ?session_key ?channel ?channel_id ()
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
              let db = get_db () in
              match
                Background_task.log_excerpt ~db ~offset:parsed.offset
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
  | "recover" :: id_s :: rest -> (
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: background task id must be an integer"
      else
        let runner =
          match rest with
          | "--runner" :: r :: _ -> Background_task.runner_of_string r
          | _ -> None
        in
        let model =
          match rest with
          | "--model" :: m :: _ -> Some m
          | _ :: "--model" :: m :: _ -> Some m
          | _ :: _ :: "--model" :: m :: _ -> Some m
          | _ -> None
        in
        match Background_task.recover ~db ~id ?runner ?model () with
        | Ok (new_id, effective_runner) ->
            Printf.sprintf
              "Recovered task %d → new task %d (%s). Use `clawq background \
               show %d` to track it."
              id new_id
              (Background_task.string_of_runner effective_runner)
              new_id
        | Error msg -> "Error: " ^ msg)
  | [ "export-acp"; id_s ] ->
      let db = get_db () in
      Background_task.init_schema db;
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: background task id must be an integer"
      else if not (Acp_history.has_history ~db ~task_id:id) then
        Printf.sprintf "No ACP history found for task %d" id
      else Acp_history.export_jsonl ~db ~task_id:id
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
       <list|show|add|start|wait|logs|transcript|resume|message|send|cancel|stop|retry|recover|finalize|export-acp>\n\
      \  background list                                         - List queued \
       and completed tasks\n\
      \  background show <id>                                    - Show task \
       details\n\
      \  background add <codex|claude|kimi|gemini|opencode|cursor|local> \
       [--model <model>] [--agent <name>] <repo> [--branch <name>] <prompt> - \
       Queue a background runner\n\
      \  background start <runner> ...                           - Alias for \
       background add\n\
      \  background wait <id> [--timeout <seconds>]              - Wait for a \
       task to finish\n\
      \  background logs <id> [--lines N] [--offset N] [--follow] - Show task \
       log lines\n\
      \  background transcript <id> [--regex R] [--max-lines N] [--export] - \
       Show bounded task transcript\n\
      \  background resume <id>                                  - Resume a \
       started task with the runner's native session support\n\
      \  background message <id> <message...>                    - Inject a \
       chat message into a started task and resume it\n\
      \  background send <id> <message...>                       - Alias for \
       background message\n\
      \  background cancel <id>                                  - Cancel a \
       queued/running task\n\
      \  background stop <id>                                    - Alias for \
       background cancel\n\
      \  background retry <id>                                   - Re-queue a \
       failed task (max 3 retries)\n\
      \  background recover <id> [--runner R] [--model M]        - Recover a \
       failed/stuck task with full context\n\
      \  background finalize <id>                                - Rebase and \
       fast-forward into target branch\n\
      \  background export-acp <id>                              - Export ACP \
       session history as JSONL"

let cmd_subagents args =
  match args with
  | [] ->
      "Usage: clawq subagents <list|start|stop|send|transcript>\n\
      \  subagents list\n\
      \  subagents start [--model M] [--agent NAME] <repo> <prompt...>\n\
      \  subagents stop <id>\n\
      \  subagents send <id> <message...>\n\
      \  subagents transcript <id> [--regex R] [--max-lines N] [--export]"
  | [ "list" ] ->
      let db = get_db () in
      Background_task.init_schema db;
      Background_task.list_tasks ~db
      |> List.filter (fun (task : Background_task.task) ->
          task.runner = Background_task.Local)
      |> fun tasks -> Background_task.format_task_list_with_hidden tasks 0
  | "start" :: rest -> (
      let cfg = get_config () in
      let db = get_db () in
      let workspace = Runtime_config.effective_workspace cfg in
      ignore (Agent_template.init_cache ~workspace_dir:workspace ());
      Background_task.init_schema db;
      match parse_background_add_args ("local" :: rest) with
      | Error msg -> "Error: " ^ msg
      | Ok parsed -> (
          match parsed.agent_name with
          | Some name when Agent_template.resolve name = None ->
              Printf.sprintf "Error: agent template '%s' not found" name
          | _ -> (
              let session_key, channel, channel_id =
                Background_task.routing_from_context ?notify_cfg:cfg.notify ()
              in
              match
                Background_task.enqueue ~db ~runner:Background_task.Local
                  ?model:parsed.model ?agent_name:parsed.agent_name
                  ~repo_path:parsed.repo_path ~prompt:parsed.prompt
                  ?branch:parsed.branch ?session_key ?channel ?channel_id ()
              with
              | Ok id ->
                  Printf.sprintf
                    "Queued subagent task %d (local). Use `clawq subagents \
                     transcript %d` to inspect its bounded transcript."
                    id id
              | Error msg -> "Error: " ^ msg)))
  | [ "stop"; id_s ] -> (
      let db = get_db () in
      Background_task.init_schema db;
      match require_local_subagent_task ~db id_s with
      | Error msg -> "Error: " ^ msg
      | Ok _ -> cmd_background [ "cancel"; id_s ])
  | "send" :: id_s :: message_parts -> (
      let db = get_db () in
      Background_task.init_schema db;
      match require_local_subagent_task ~db id_s with
      | Error msg -> "Error: " ^ msg
      | Ok _ -> cmd_background ("message" :: id_s :: message_parts))
  | "transcript" :: rest -> (
      let db = get_db () in
      Background_task.init_schema db;
      match parse_transcript_args rest with
      | Error msg -> "Error: " ^ msg
      | Ok parsed -> (
          match require_local_subagent_task ~db (string_of_int parsed.id) with
          | Error msg -> "Error: " ^ msg
          | Ok _ ->
              Background_task_transcript.render ~db ~id:parsed.id
                ?regex:parsed.regex ?max_lines:parsed.max_lines
                ~export:parsed.export ()))
  | _ ->
      "Usage: clawq subagents <list|start|stop|send|transcript>\n\
      \  subagents list\n\
      \  subagents start [--model M] [--agent NAME] <repo> <prompt...>\n\
      \  subagents stop <id>\n\
      \  subagents send <id> <message...>\n\
      \  subagents transcript <id> [--regex R] [--max-lines N] [--export]"

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
          ~use_worktree:parsed.use_worktree
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
  if rows = [] then
    "No audit log entries. Entries are created when tools are invoked or \
     security events occur."
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
  | [ "systemd-unit" ] -> Service.cmd_systemd_unit ()
  | [ "launchd-plist" ] -> Service.cmd_launchd_plist ()
  | [ "install" ] -> Service.cmd_install ()
  | [ "uninstall" ] -> Service.cmd_uninstall ()
  | _ ->
      "Usage: clawq service \
       <start|stop|status|signal-restart|restart|install|uninstall|systemd-unit|launchd-plist>"

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
                auto|git|binary|pkg]"
               value))
  | _ -> Error "Usage: clawq update [--mode auto|git|binary|pkg]"

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
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () ->
          output_string oc (Yojson.Safe.pretty_to_string ~std:true json);
          output_char oc '\n');
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
            failwith
              "Custom tunnel requires the CLAWQ_TUNNEL_COMMAND environment \
               variable to be set."
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
      | _ -> (None, None)
    in
    let is_known_provider =
      match provider_name with
      | p
        when p = Tunnel_cloudflare.name || p = "cf" || p = Tunnel_tailscale.name
             || p = Tunnel_ngrok.name || p = Tunnel_custom.name ->
          true
      | _ -> false
    in
    match args with
    | [ "start" ] -> (
        if not is_known_provider then
          Printf.sprintf
            "Unknown tunnel provider: %s. Supported: cloudflare, tailscale, \
             ngrok, custom."
            provider_name
        else
          match try Ok (tunnel_start ()) with Failure msg -> Error msg with
          | Error msg -> "Error: " ^ msg
          | Ok pid_url -> (
              match pid_url with
              | Some pid, Some url -> (
                  match save_tunnel_state ~pid ~port:cfg.gateway.port ~url with
                  | Ok () ->
                      Printf.sprintf "Tunnel started: %s (pid %d)" url pid
                  | Error err ->
                      Printf.sprintf
                        "Tunnel started: %s (pid %d)\n\
                         Warning: failed to save state: %s"
                        url pid err)
              | _ -> "Tunnel started but URL or PID not available"))
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
        let file_status =
          match read_tunnel_state () with
          | Some (pid, url, start_ticks) ->
              let running = tunnel_pid_matches ~pid ~start_ticks in
              if running then
                Some
                  (Printf.sprintf
                     "Tunnel provider: %s\n\
                     \  Status: running (pid %d)\n\
                     \  URL: %s"
                     provider_name pid url)
              else begin
                remove_tunnel_state ();
                None
              end
          | None -> None
        in
        match file_status with
        | Some s -> s
        | None -> (
            match read_daemon_tunnel_info () with
            | Some (provider, Some url) ->
                Printf.sprintf
                  "Tunnel provider: %s\n\
                  \  Status: running (daemon-managed)\n\
                  \  URL: %s"
                  provider url
            | Some (provider, None) ->
                Printf.sprintf
                  "Tunnel provider: %s\n\
                  \  Status: running (daemon-managed, URL pending)"
                  provider
            | None ->
                Printf.sprintf
                  "Tunnel provider: %s\n\
                  \  Status: stopped\n\
                  \  To start: clawq tunnel start"
                  provider_name))
    | [ "apply" ] -> Lwt_main.run (!Tunnel_manager.daemon_apply_fn ())
    | [ "restart" ] -> Lwt_main.run (!Tunnel_manager.daemon_restart_fn ())
    | [ "daemon-status" ] -> !Tunnel_manager.daemon_status_fn ()
    | _ -> "Usage: clawq tunnel <start|stop|status|apply|restart|daemon-status>"

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

let cmd_held_items args =
  let db = get_db () in
  Held_items.init_db db;
  match args with
  | "save" :: rest -> (
      let rec parse name desc plan_file layer requestor channel acc =
        match acc with
        | "--name" :: v :: tl ->
            parse (Some v) desc plan_file layer requestor channel tl
        | "--desc" :: v :: tl ->
            parse name (Some v) plan_file layer requestor channel tl
        | "--plan-file" :: v :: tl ->
            parse name desc (Some v) layer requestor channel tl
        | "--layer" :: v :: tl ->
            parse name desc plan_file (int_of_string_opt v) requestor channel tl
        | "--requestor" :: v :: tl ->
            parse name desc plan_file layer (Some v) channel tl
        | "--channel" :: v :: tl ->
            parse name desc plan_file layer requestor (Some v) tl
        | _ :: tl -> parse name desc plan_file layer requestor channel tl
        | [] -> (name, desc, plan_file, layer, requestor, channel)
      in
      let name, desc, plan_file, layer, requestor, channel =
        parse None None None None None None rest
      in
      match (name, desc, plan_file, layer) with
      | Some n, Some d, Some pf, Some l ->
          let plan_json =
            try
              let ic = open_in pf in
              Fun.protect
                ~finally:(fun () -> close_in ic)
                (fun () ->
                  let len = in_channel_length ic in
                  really_input_string ic len)
            with exn ->
              Printf.sprintf "{\"error\": \"Failed to read plan file: %s\"}"
                (Printexc.to_string exn)
          in
          let id =
            Held_items.save ~db ~feature_name:n ~description:d ~plan_json
              ~layer:l ?requestor_id:requestor ?channel ()
          in
          Printf.sprintf "Saved held item #%d: %s (layer %d)" id n l
      | _ ->
          "Usage: clawq held-items save --name NAME --desc DESC --plan-file \
           FILE --layer N [--requestor ID] [--channel CH]")
  | [ "list" ] | [] ->
      let items = Held_items.list_items ~db ~status:"pending" () in
      if items = [] then "No pending held items."
      else
        let columns =
          Table_format.
            [
              { header = "ID"; align = Right; min_width = 2; flex = false };
              { header = "NAME"; align = Left; min_width = 8; flex = false };
              { header = "LAYER"; align = Right; min_width = 3; flex = false };
              { header = "STATUS"; align = Left; min_width = 6; flex = false };
              { header = "CREATED"; align = Left; min_width = 10; flex = false };
              {
                header = "DESCRIPTION";
                align = Left;
                min_width = 10;
                flex = true;
              };
            ]
        in
        let rows =
          List.map
            (fun (item : Held_items.held_item) ->
              let desc_short =
                if String.length item.description > 50 then
                  String.sub item.description 0 50 ^ "..."
                else item.description
              in
              [
                string_of_int item.id;
                item.feature_name;
                string_of_int item.layer;
                item.status;
                item.created_at;
                desc_short;
              ])
            items
        in
        "Held items (pending):\n" ^ Table_format.render columns rows
  | [ "list"; "--status"; status ] ->
      let items = Held_items.list_items ~db ~status () in
      if items = [] then Printf.sprintf "No held items with status '%s'." status
      else
        let columns =
          Table_format.
            [
              { header = "ID"; align = Right; min_width = 2; flex = false };
              { header = "NAME"; align = Left; min_width = 8; flex = false };
              { header = "LAYER"; align = Right; min_width = 3; flex = false };
              { header = "STATUS"; align = Left; min_width = 6; flex = false };
              { header = "CREATED"; align = Left; min_width = 10; flex = false };
              {
                header = "DESCRIPTION";
                align = Left;
                min_width = 10;
                flex = true;
              };
            ]
        in
        let rows =
          List.map
            (fun (item : Held_items.held_item) ->
              let desc_short =
                if String.length item.description > 50 then
                  String.sub item.description 0 50 ^ "..."
                else item.description
              in
              [
                string_of_int item.id;
                item.feature_name;
                string_of_int item.layer;
                item.status;
                item.created_at;
                desc_short;
              ])
            items
        in
        Printf.sprintf "Held items (%s):\n" status
        ^ Table_format.render columns rows
  | [ "show"; id_str ] | [ "view"; id_str ] -> (
      match int_of_string_opt id_str with
      | None ->
          Printf.sprintf
            "Error: '%s' is not a valid ID. Provide a numeric held item ID."
            id_str
      | Some id -> (
          match Held_items.get ~db ~id with
          | None -> Printf.sprintf "No held item found with ID %d." id
          | Some item ->
              Printf.sprintf
                "Held Item #%d\n\
                 Name: %s\n\
                 Layer: %d\n\
                 Status: %s\n\
                 Description: %s\n\
                 Requestor: %s\n\
                 Channel: %s\n\
                 Created: %s\n\
                 Reviewed by: %s\n\
                 Reviewed at: %s\n\
                 Notes: %s\n\n\
                 Plan:\n\
                 %s"
                item.id item.feature_name item.layer item.status
                item.description
                (Option.value ~default:"-" item.requestor_id)
                (Option.value ~default:"-" item.channel)
                item.created_at
                (Option.value ~default:"-" item.reviewed_by)
                (Option.value ~default:"-" item.reviewed_at)
                (Option.value ~default:"-" item.review_notes)
                item.plan_json))
  | "approve" :: id_str :: rest -> (
      match int_of_string_opt id_str with
      | None ->
          Printf.sprintf
            "Error: '%s' is not a valid ID. Provide a numeric held item ID."
            id_str
      | Some id ->
          let rec parse_opts by notes = function
            | "--by" :: v :: tl -> parse_opts (Some v) notes tl
            | "--notes" :: v :: tl -> parse_opts by (Some v) tl
            | _ :: tl -> parse_opts by notes tl
            | [] -> (by, notes)
          in
          let by, notes = parse_opts None None rest in
          if
            Held_items.review ~db ~id ~action:"approved" ?reviewed_by:by ?notes
              ()
          then Printf.sprintf "Approved held item #%d." id
          else
            Printf.sprintf
              "Failed to approve item #%d. It may not exist or may not be \
               pending."
              id)
  | "reject" :: id_str :: rest -> (
      match int_of_string_opt id_str with
      | None ->
          Printf.sprintf
            "Error: '%s' is not a valid ID. Provide a numeric held item ID."
            id_str
      | Some id ->
          let rec parse_opts by notes = function
            | "--by" :: v :: tl -> parse_opts (Some v) notes tl
            | "--notes" :: v :: tl -> parse_opts by (Some v) tl
            | _ :: tl -> parse_opts by notes tl
            | [] -> (by, notes)
          in
          let by, notes = parse_opts None None rest in
          if
            Held_items.review ~db ~id ~action:"rejected" ?reviewed_by:by ?notes
              ()
          then Printf.sprintf "Rejected held item #%d." id
          else
            Printf.sprintf
              "Failed to reject item #%d. It may not exist or may not be \
               pending."
              id)
  | _ ->
      "Usage: clawq held-items <subcommand>\n\n\
       Subcommands:\n\
      \  save --name NAME --desc DESC --plan-file FILE --layer N [--requestor \
       ID] [--channel CH]\n\
      \  list [--status pending|approved|rejected|all]\n\
      \  show ID\n\
      \  approve ID [--by ADMIN] [--notes TEXT]\n\
      \  reject ID [--by ADMIN] [--notes TEXT]"

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
  | "usage" :: rest -> cmd_usage rest
  | "active" :: rest -> cmd_active rest
  | "provider" :: rest -> cmd_provider rest
  | "channel" :: "test" :: "teams" :: _ -> cmd_channel_test_teams ()
  | "channel" :: rest -> cmd_channel rest
  | "memory" :: rest -> cmd_memory rest
  | "session" :: rest -> cmd_session rest
  | "workspace" :: rest -> cmd_workspace rest
  | "capabilities" :: _ -> cmd_capabilities ()
  | "auth" :: rest -> cmd_auth rest
  | "transcribe" :: rest -> cmd_transcribe rest
  | "mcp" :: _ -> cmd_mcp ()
  | "runner" :: rest -> cmd_runner rest
  | "cron" :: rest -> cmd_cron rest
  | "background" :: rest -> cmd_background rest
  | "subagents" :: rest -> cmd_subagents rest
  | "delegate" :: rest -> cmd_delegate rest
  | "skills" :: rest -> Command_bridge_agent_cmds.cmd_skills rest
  | "pair" :: rest -> Command_bridge_pair.cmd_pair rest
  | "agents" :: rest -> Command_bridge_agent_cmds.cmd_agents rest
  | "rig" :: rest -> Command_bridge_agent_cmds.cmd_rig rest
  | "rigging" :: _ -> Command_bridge_agent_cmds.cmd_rig [ "list" ]
  | "audit" :: rest -> cmd_audit rest
  | "runtime" :: rest -> Command_bridge_debug.cmd_runtime rest
  | "tunnel" :: rest -> cmd_tunnel rest
  | "update" :: rest -> cmd_update rest
  | "hardware" :: _ -> "hardware: deferred to Phase 2"
  | "migrate" :: rest -> Migrate.cmd_migrate rest
  | "service" :: rest -> cmd_service rest
  | "reset-agent" :: _ -> Command_bridge_debug.cmd_reset_agent ()
  | "reset-workspace" :: _ -> Command_bridge_debug.cmd_reset_workspace ()
  | "otp-show" :: _ -> cmd_otp_show ()
  | "debug" :: rest -> Command_bridge_debug.cmd_debug rest
  | "setup" :: rest -> Command_bridge_debug.cmd_setup rest
  | "plan" :: rest -> cmd_plan rest
  | "benchmark" :: rest -> Benchmark.run rest
  | "completions" :: rest -> Completions.cmd_completions rest
  | "watcher" :: rest -> Command_bridge_debug.cmd_watcher rest
  | "ec-run" :: rest -> Command_bridge_debug.cmd_ec_run rest
  | "manifest" :: rest -> Command_bridge_debug.cmd_manifest rest
  | "held-items" :: rest -> cmd_held_items rest
  | "debate" :: rest -> Debate.cmd_debate ~get_config ~get_db rest
  | "pipeline" :: rest -> Command_bridge_session.cmd_pipeline rest
  | _ -> Clawq_core.dispatch args

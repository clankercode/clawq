open Command_bridge_helpers

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
    cfg.heartbeat.enabled
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

let cmd_debug_http args =
  match args with
  | [ "on" ] ->
      let _ = Config_set.set_value "log.debug_http" "true" in
      "HTTP debug logging enabled in config."
  | [ "off" ] ->
      let _ = Config_set.set_value "log.debug_http" "false" in
      "HTTP debug logging disabled in config."
  | [ "status" ] -> Http_debug.status_info ()
  | [ "clear" ] -> Http_debug.clear_logs ()
  | [ "tail" ] -> Http_debug.tail_logs 10
  | [ "tail"; n ] -> (
      try Http_debug.tail_logs (int_of_string n)
      with _ ->
        "Usage: clawq debug http tail [N] (N must be a positive integer)")
  | _ ->
      "Usage: clawq debug http {on|off|status|clear|tail [N]}\n\n\
      \  on      Enable HTTP debug logging in config\n\
      \  off     Disable HTTP debug logging in config\n\
      \  status  Show HTTP debug status and log dir info\n\
      \  clear   Delete all HTTP debug log files\n\
      \  tail    Show last N HAR files (default 10)"

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
           let forever, wakener = Lwt.wait () in
           let handler_int =
             Lwt_unix.on_signal Sys.sigint (fun _ ->
                 Lwt.wakeup_later wakener "")
           in
           let handler_term =
             Lwt_unix.on_signal Sys.sigterm (fun _ ->
                 Lwt.wakeup_later wakener "")
           in
           Lwt.finalize
             (fun () -> forever)
             (fun () ->
               Lwt_unix.disable_signal_handler handler_int;
               Lwt_unix.disable_signal_handler handler_term;
               Lwt.return_unit))
      in
      "debug html-preview: stopped"
  | "http" :: rest -> cmd_debug_http rest
  | _ ->
      "Usage: clawq debug context [SESSION_KEY]\n\
       Prints the runtime context block for a session (default: __main__).\n\n\
       Usage: clawq debug html-preview [PORT]\n\
       Serves Html_page test pages on localhost (default port 8099).\n\n\
       Usage: clawq debug http {on|off|status|clear|tail [N]}\n\
       Manage HTTP debug logging (HAR files).\n\n\
       Usage: clawq debug prompt [MESSAGE]\n\
       Prints the normalized logical messages for a single agent turn."

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
          Fun.protect
            ~finally:(fun () -> close_out oc)
            (fun () -> output_string oc content)
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
    let backup_name = Workspace_version.auto_backup_name () in
    (match Workspace_version.backup ~workspace ~name:backup_name with
    | Ok files ->
        Printf.printf "  Auto-backup: %d file(s) saved as '%s'\n"
          (List.length files) backup_name;
        Printf.printf "    Restore with: clawq workspace restore %s\n\n"
          backup_name
    | Error e -> Printf.printf "  Warning: auto-backup failed: %s\n\n" e);
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
          Fun.protect
            ~finally:(fun () -> close_out oc)
            (fun () -> output_string oc content)
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

let cmd_setup args =
  match args with
  | [] -> Setup_main.run ()
  | [ "discord" ] -> Setup_discord.run ()
  | [ "github" ] -> Setup_github.run ()
  | [ "slack" ] -> Setup_slack.run ()
  | [ "teams" ] -> Setup_teams.run ()
  | [ "telegram" ] -> Setup_telegram.run ()
  | [ "tunnel" ] -> Setup_tunnel.run ()
  | [ "summarizer" ] -> Setup_summarizer.run ()
  | [ "matrix" ] -> Setup_matrix.run ()
  | [ "irc" ] -> Setup_irc.run ()
  | [ "email" ] -> Setup_email.run ()
  | [ "signal" ] -> Setup_signal_channel.run ()
  | [ "whatsapp" ] -> Setup_whatsapp.run ()
  | [ "nostr" ] -> Setup_nostr.run ()
  | [ "lark" ] -> Setup_lark.run ()
  | [ "line" ] -> Setup_line.run ()
  | [ "onebot" ] -> Setup_onebot.run ()
  | [ "mattermost" ] -> Setup_mattermost.run ()
  | [ "dingtalk" ] -> Setup_dingtalk.run ()
  | [ "imessage" ] -> Setup_imessage.run ()
  | [ "provider" ] -> Setup_provider.run ()
  | [ "web-search" ] | [ "websearch" ] -> Setup_web_search.run ()
  | [ "voice" ] | [ "tts" ] -> Setup_voice.run ()
  | [ "cron" ] -> Setup_cron.run ()
  | [ "security" ] -> Setup_security.run ()
  | [ "gateway" ] -> Setup_gateway.run ()
  | [ "totp" ] | [ "2fa" ] -> Setup_totp.run ()
  | [ "memory" ] -> Setup_memory.run ()
  | [ "prompt" ] -> Setup_prompt.run ()
  | [ "resilience" ] -> Setup_resilience.run ()
  | [ "heartbeat" ] -> Setup_heartbeat.run ()
  | [ "notify" ] | [ "notifications" ] -> Setup_notify.run ()
  | [ "error-watcher" ] | [ "ec" ] -> Setup_error_watcher.run ()
  | [ "observer" ] -> Setup_observer.run ()
  | [ "zai-mcp" ] | [ "zai" ] -> Setup_zai_mcp.run ()
  | _ ->
      let buf = Buffer.create 2048 in
      Buffer.add_string buf
        "Usage: clawq setup [<wizard>]\n\n\
         Run without arguments for the interactive wizard hub.\n";
      List.iter
        (fun (cat : Setup_main.category) ->
          Buffer.add_string buf (Printf.sprintf "\n  %s:\n" cat.title);
          List.iter
            (fun (e : Setup_main.wizard_entry) ->
              let padded =
                let n = String.length e.name in
                if n >= 16 then e.name ^ " "
                else e.name ^ String.make (16 - n) ' '
              in
              Buffer.add_string buf (Printf.sprintf "    %s%s\n" padded e.label))
            cat.entries)
        Setup_main.all_categories;
      Buffer.add_string buf
        "\n\
        \  Tip: run `clawq setup` with no arguments for an interactive menu.\n\
        \  Documentation: https://clawq.org/setup/\n";
      Buffer.contents buf

let cmd_watcher args =
  let cfg = get_config () in
  let ew = cfg.error_watcher in
  match args with
  | [ "status" ] | [] ->
      let pid_status =
        match Error_watcher.read_pid_file () with
        | Some pid ->
            if Error_watcher.process_alive pid then
              Printf.sprintf "running (pid %d)" pid
            else "not running (stale PID file)"
        | None -> "not running"
      in
      Printf.sprintf
        "Error Correction Watcher\n\
         ========================\n\
         Enabled:            %b\n\
         EC process:         %s\n\
         Scan interval:      %.0fs\n\
         Cooldown:           %.0fs\n\
         Max errors/batch:   %d\n\
         Auto-fix enabled:   %b\n\
         Commit tag:         %s\n\
         Primary models:     %s\n\
         Fallback models:    %s\n\
         Ignore patterns:    %s\n"
        ew.enabled pid_status ew.scan_interval_s ew.cooldown_s
        ew.max_errors_per_batch ew.auto_fix_enabled ew.commit_tag
        (String.concat ", " ew.primary_models)
        (String.concat ", " ew.fallback_models)
        (if ew.ignore_patterns = [] then "(none)"
         else String.concat ", " ew.ignore_patterns)
  | [ "enable" ] -> Config_set.set_value "error_watcher.enabled" "true"
  | [ "disable" ] -> Config_set.set_value "error_watcher.enabled" "false"
  | [ "reports" ] ->
      let db = get_db () in
      Ec_diagnosis.init_ec_reports_schema db;
      let reports = Ec_diagnosis.list_ec_reports ~db () in
      if reports = [] then "No EC reports found."
      else
        let header =
          Printf.sprintf "%-6s %-20s %-16s %s\n" "ID" "Timestamp" "Error Hash"
            "Status"
        in
        let rows =
          List.map
            (fun (id, ts, hash, status) ->
              Printf.sprintf "%-6d %-20s %-16s %s" id ts hash status)
            reports
        in
        header ^ String.concat "\n" rows ^ "\n"
  | [ "report"; id_str ] -> (
      let db = get_db () in
      Ec_diagnosis.init_ec_reports_schema db;
      match int_of_string_opt id_str with
      | None -> "Error: report ID must be an integer."
      | Some id ->
          let sql =
            "SELECT id, timestamp, error_hash, error_context, diagnoses_json, \
             voting_json, winning_plan, fix_task_id, status FROM ec_reports \
             WHERE id = ?"
          in
          let stmt = Sqlite3.prepare db sql in
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
            (fun () ->
              ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id)));
              if Sqlite3.step stmt <> Sqlite3.Rc.ROW then
                Printf.sprintf "No report found with ID %d." id
              else
                let col_text i =
                  match Sqlite3.column stmt i with
                  | Sqlite3.Data.TEXT s -> s
                  | _ -> ""
                in
                let fix_task =
                  match Sqlite3.column stmt 7 with
                  | Sqlite3.Data.INT n -> Some (Int64.to_int n)
                  | _ -> None
                in
                Printf.sprintf
                  "EC Report #%d\n\
                   =============\n\
                   Timestamp:    %s\n\
                   Error Hash:   %s\n\
                   Status:       %s\n\
                   Fix Task ID:  %s\n\n\
                   Error Context:\n\
                   %s\n\n\
                   Diagnoses:\n\
                   %s\n\n\
                   Voting:\n\
                   %s\n\n\
                   Winning Plan:\n\
                   %s\n"
                  id (col_text 1) (col_text 2) (col_text 8)
                  (match fix_task with
                  | Some tid -> string_of_int tid
                  | None -> "(none)")
                  (col_text 3) (col_text 4) (col_text 5) (col_text 6)))
  | _ ->
      "Usage: clawq watcher <status|enable|disable|reports|report ID>\n\n\
      \  status    Show watcher config and EC process status (default)\n\
      \  enable    Enable the error correction watcher\n\
      \  disable   Disable the error correction watcher\n\
      \  reports   List recent EC reports\n\
      \  report ID Show a specific EC report\n"

let cmd_ec_run args =
  if List.mem "--daemon-mode" args then begin
    Ec_process.run_daemon_mode ();
    ""
  end
  else "Usage: clawq ec-run --daemon-mode\n(internal command)\n"

let cmd_manifest = function
  | [ "teams" ] ->
      print_string (Slash_commands_manifest.teams_json ());
      ""
  | [ "teams"; "--output"; path ] ->
      let oc = open_out path in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () -> output_string oc (Slash_commands_manifest.teams_json ()));
      Printf.sprintf "Wrote Teams manifest to %s" path
  | [ "teams"; "-n"; n ] -> (
      match int_of_string_opt n with
      | Some n when n > 0 ->
          print_string (Slash_commands_manifest.teams_json ~n ());
          ""
      | _ -> "Error: -n requires a positive integer")
  | [ "telegram" ] ->
      print_string (Slash_commands_manifest.telegram_json ());
      ""
  | _ ->
      "Usage: clawq manifest <platform>\n\n\
       Platforms:\n\
      \  teams    [--output FILE] [-n COUNT]  Generate Teams bot manifest \
       commands\n\
      \  telegram                             Generate Telegram setMyCommands \
       payload"

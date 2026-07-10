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

include Command_bridge_cron
include Command_bridge_bgcmd

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

include Command_bridge_tunnel

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
  | "worker" :: rest -> cmd_worker rest
  | "subagents" :: rest -> cmd_subagents rest
  | "delegate" :: rest -> cmd_delegate rest
  | "skills" :: rest -> Command_bridge_agent_cmds.cmd_skills rest
  | "pair" :: rest -> Command_bridge_pair.cmd_pair rest
  | "agents" :: rest -> Command_bridge_agent_cmds.cmd_agents rest
  | "rooms" :: rest -> Command_bridge_agent_cmds.cmd_rooms rest
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
  | "subscriptions" :: rest ->
      Github_pr_subscriptions_cli.cmd_subscriptions rest
  | "debate" :: rest -> Debate.cmd_debate ~get_config ~get_db rest
  | "pipeline" :: rest -> Command_bridge_session.cmd_pipeline rest
  | _ -> Clawq_core.dispatch args

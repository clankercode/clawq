open Slash_commands_fmt

(* ── Model show/list formatting ────────────────────────────────────────── *)

let format_model_show_telegram ~current ~favorites ~usage_ranked =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "<b>Current Model</b>\n";
  Buffer.add_string buf (Printf.sprintf "<code>%s</code>\n\n" current);
  if favorites <> [] then begin
    Buffer.add_string buf "<b>Favorites</b>\n";
    List.iter
      (fun m ->
        Buffer.add_string buf
          (Printf.sprintf "\xe2\xad\x90 <code>%s</code>\n" m))
      favorites;
    Buffer.add_string buf "\n"
  end;
  if usage_ranked <> [] then begin
    Buffer.add_string buf "<b>Recent Usage</b>\n";
    Buffer.add_string buf "<blockquote expandable>\n";
    List.iter
      (fun (m, count) ->
        Buffer.add_string buf (Printf.sprintf "<code>%s</code> (%d)\n" m count))
      usage_ranked;
    Buffer.add_string buf "</blockquote>\n"
  end;
  Buffer.contents buf

let format_model_list_telegram ~models ~provider =
  let buf = Buffer.create 2048 in
  let title =
    match provider with
    | Some p -> Printf.sprintf "<b>Models: %s</b>\n" p
    | None -> "<b>Available Models</b>\n"
  in
  Buffer.add_string buf title;
  Buffer.add_string buf "<blockquote expandable>\n";
  List.iter
    (fun m -> Buffer.add_string buf (Printf.sprintf "<code>%s</code>\n" m))
    models;
  Buffer.add_string buf "</blockquote>";
  Buffer.contents buf

let format_model_list_plain ~models ~provider =
  let title =
    match provider with
    | Some p -> Printf.sprintf "Models: %s\n" p
    | None -> "Available Models\n"
  in
  title ^ String.concat "\n" models

let format_model_show_plain ~current ~favorites ~usage_ranked =
  let buf = Buffer.create 512 in
  Buffer.add_string buf (Printf.sprintf "Current: %s\n" current);
  if favorites <> [] then begin
    Buffer.add_string buf "Favorites:\n";
    List.iter
      (fun m -> Buffer.add_string buf (Printf.sprintf "  * %s\n" m))
      favorites
  end;
  if usage_ranked <> [] then begin
    Buffer.add_string buf "Recent:\n";
    List.iter
      (fun (m, count) ->
        Buffer.add_string buf (Printf.sprintf "  %s (%d)\n" m count))
      usage_ranked
  end;
  Buffer.contents buf

let format_model_show ~connector ~current ~favorites ~usage_ranked =
  Format_adapter.dispatch connector ~telegram_html:format_model_show_telegram
    ~default:format_model_show_plain ~current ~favorites ~usage_ranked

let format_model_list ~connector ~models ~provider =
  Format_adapter.dispatch connector ~telegram_html:format_model_list_telegram
    ~default:format_model_list_plain ~models ~provider

(* ── Costs formatting ─────────────────────────────────────────────────── *)

(* ── Existing format: costs ────────────────────────────────────────────── *)

let cost_table_row label (s : Request_stats.summary) =
  [
    label;
    Printf.sprintf "$%.4f" s.total_cost_usd;
    string_of_int s.total_turns;
    Request_stats.format_tokens s.total_prompt_tokens;
    Request_stats.format_tokens s.total_added_prompt_tokens;
    Request_stats.format_tokens s.total_completion_tokens;
  ]

let cost_summary_columns =
  Table_format.
    [
      { header = "PERIOD"; align = Left; min_width = 12; flex = false };
      { header = "COST"; align = Right; min_width = 8; flex = false };
      { header = "TURNS"; align = Right; min_width = 5; flex = false };
      { header = "PROMPT"; align = Right; min_width = 6; flex = false };
      { header = "ADDED"; align = Right; min_width = 6; flex = false };
      { header = "COMPLETION"; align = Right; min_width = 6; flex = false };
    ]

let format_costs ~connector ~db action =
  match action with
  | CostsSummary ->
      let today =
        Request_stats.summary_for_period ~db
          ~since:"datetime('now', 'start of day')"
      in
      let week =
        Request_stats.summary_for_period ~db ~since:"datetime('now', '-7 days')"
      in
      let month =
        Request_stats.summary_for_period ~db
          ~since:"datetime('now', '-30 days')"
      in
      let all = Request_stats.total_summary ~db in
      if all.total_turns = 0 then "No cost data recorded yet."
      else
        let rows =
          [
            cost_table_row "Today" today;
            cost_table_row "Last 7 days" week;
            cost_table_row "Last 30 days" month;
            cost_table_row "All time" all;
          ]
        in
        Format_adapter.bold connector "Cost Summary"
        ^ "\n\n"
        ^ Format_adapter.render_table connector ~max_width:60
            cost_summary_columns rows
  | CostsSessions ->
      let sessions = Request_stats.summary_by_session ~db in
      if sessions = [] then "No cost data recorded yet."
      else
        let session_columns =
          Table_format.
            [
              { header = "SESSION"; align = Left; min_width = 10; flex = true };
              { header = "COST"; align = Right; min_width = 8; flex = false };
              { header = "TURNS"; align = Right; min_width = 5; flex = false };
              { header = "PROMPT"; align = Right; min_width = 6; flex = false };
              { header = "ADDED"; align = Right; min_width = 6; flex = false };
              {
                header = "COMPLETION";
                align = Right;
                min_width = 6;
                flex = false;
              };
            ]
        in
        let rows =
          List.map
            (fun (ss : Request_stats.session_summary) ->
              cost_table_row ss.session_key ss.summary)
            sessions
        in
        Format_adapter.bold connector "Session Costs"
        ^ "\n\n"
        ^ Format_adapter.render_table connector ~max_width:60 session_columns
            rows
  | CostsSession key ->
      let s = Request_stats.summary_for_session ~db ~session_key:key in
      if s.total_turns = 0 then
        Printf.sprintf "No cost data for session '%s'." key
      else
        let rows = [ cost_table_row "Total" s ] in
        Format_adapter.bold connector (Printf.sprintf "Costs for %s" key)
        ^ "\n\n"
        ^ Format_adapter.render_table connector ~max_width:60
            cost_summary_columns rows
  | CostsModel ->
      let models = Request_stats.summary_by_model ~db in
      if models = [] then "No cost data recorded yet."
      else
        let model_columns =
          Table_format.
            [
              { header = "MODEL"; align = Left; min_width = 15; flex = true };
              { header = "COST"; align = Right; min_width = 8; flex = false };
              { header = "TURNS"; align = Right; min_width = 5; flex = false };
              { header = "PROMPT"; align = Right; min_width = 6; flex = false };
              {
                header = "COMPLETION";
                align = Right;
                min_width = 6;
                flex = false;
              };
            ]
        in
        let rows =
          List.map
            (fun (ms : Request_stats.model_summary) ->
              [
                ms.provider ^ ":" ^ ms.model;
                Printf.sprintf "$%.4f" ms.summary.total_cost_usd;
                string_of_int ms.summary.total_turns;
                Request_stats.format_tokens ms.summary.total_prompt_tokens;
                Request_stats.format_tokens ms.summary.total_completion_tokens;
              ])
            models
        in
        Format_adapter.bold connector "Model Costs"
        ^ "\n\n"
        ^ Format_adapter.render_table connector ~max_width:60 model_columns rows
  | CostsProvider ->
      let providers = Request_stats.summary_by_provider ~db in
      if providers = [] then "No cost data recorded yet."
      else
        let provider_columns =
          Table_format.
            [
              {
                header = "PROVIDER";
                align = Left;
                min_width = 10;
                flex = false;
              };
              { header = "COST"; align = Right; min_width = 8; flex = false };
              { header = "TURNS"; align = Right; min_width = 5; flex = false };
              { header = "PROMPT"; align = Right; min_width = 6; flex = false };
              {
                header = "COMPLETION";
                align = Right;
                min_width = 6;
                flex = false;
              };
            ]
        in
        let rows =
          List.map
            (fun (provider, (s : Request_stats.summary)) ->
              [
                provider;
                Printf.sprintf "$%.4f" s.total_cost_usd;
                string_of_int s.total_turns;
                Request_stats.format_tokens s.total_prompt_tokens;
                Request_stats.format_tokens s.total_completion_tokens;
              ])
            providers
        in
        Format_adapter.bold connector "Provider Costs"
        ^ "\n\n"
        ^ Format_adapter.render_table connector ~max_width:60 provider_columns
            rows

(* ── Existing format: usage ────────────────────────────────────────────── *)

(* ── Usage formatting ─────────────────────────────────────────────────── *)

let usage_table_row label (s : Request_stats.summary) =
  [
    label;
    string_of_int s.total_turns;
    Request_stats.format_tokens s.total_prompt_tokens;
    Request_stats.format_tokens s.total_added_prompt_tokens;
    Request_stats.format_tokens s.total_completion_tokens;
  ]

let usage_summary_columns =
  Table_format.
    [
      { header = "PERIOD"; align = Left; min_width = 12; flex = false };
      { header = "TURNS"; align = Right; min_width = 5; flex = false };
      { header = "PROMPT"; align = Right; min_width = 6; flex = false };
      { header = "ADDED"; align = Right; min_width = 6; flex = false };
      { header = "COMPLETION"; align = Right; min_width = 6; flex = false };
    ]

let format_usage ~connector ~db action =
  match action with
  | UsageSummary ->
      let today =
        Request_stats.summary_for_period ~db
          ~since:"datetime('now', 'start of day')"
      in
      let week =
        Request_stats.summary_for_period ~db ~since:"datetime('now', '-7 days')"
      in
      let month =
        Request_stats.summary_for_period ~db
          ~since:"datetime('now', '-30 days')"
      in
      let all = Request_stats.total_summary ~db in
      if all.total_turns = 0 then "No usage data recorded yet."
      else
        let rows =
          [
            usage_table_row "Today" today;
            usage_table_row "Last 7 days" week;
            usage_table_row "Last 30 days" month;
            usage_table_row "All time" all;
          ]
        in
        Format_adapter.bold connector "Usage Summary"
        ^ "\n\n"
        ^ Format_adapter.render_table connector ~max_width:60
            usage_summary_columns rows
  | UsageSessions ->
      let sessions = Request_stats.summary_by_session ~db in
      if sessions = [] then "No usage data recorded yet."
      else
        let session_columns =
          Table_format.
            [
              { header = "SESSION"; align = Left; min_width = 10; flex = true };
              { header = "TURNS"; align = Right; min_width = 5; flex = false };
              { header = "PROMPT"; align = Right; min_width = 6; flex = false };
              { header = "ADDED"; align = Right; min_width = 6; flex = false };
              {
                header = "COMPLETION";
                align = Right;
                min_width = 6;
                flex = false;
              };
            ]
        in
        let rows =
          List.map
            (fun (ss : Request_stats.session_summary) ->
              usage_table_row ss.session_key ss.summary)
            sessions
        in
        Format_adapter.bold connector "Session Usage"
        ^ "\n\n"
        ^ Format_adapter.render_table connector ~max_width:60 session_columns
            rows
  | UsageSession key ->
      let s = Request_stats.summary_for_session ~db ~session_key:key in
      if s.total_turns = 0 then
        Printf.sprintf "No usage data for session '%s'." key
      else
        let rows = [ usage_table_row "Total" s ] in
        Format_adapter.bold connector (Printf.sprintf "Usage for %s" key)
        ^ "\n\n"
        ^ Format_adapter.render_table connector ~max_width:60
            usage_summary_columns rows
  | UsageModel ->
      let models = Request_stats.summary_by_model ~db in
      if models = [] then "No usage data recorded yet."
      else
        let model_columns =
          Table_format.
            [
              { header = "MODEL"; align = Left; min_width = 15; flex = true };
              { header = "TURNS"; align = Right; min_width = 5; flex = false };
              { header = "PROMPT"; align = Right; min_width = 6; flex = false };
              {
                header = "COMPLETION";
                align = Right;
                min_width = 6;
                flex = false;
              };
            ]
        in
        let rows =
          List.map
            (fun (ms : Request_stats.model_summary) ->
              [
                ms.provider ^ ":" ^ ms.model;
                string_of_int ms.summary.total_turns;
                Request_stats.format_tokens ms.summary.total_prompt_tokens;
                Request_stats.format_tokens ms.summary.total_completion_tokens;
              ])
            models
        in
        Format_adapter.bold connector "Model Usage"
        ^ "\n\n"
        ^ Format_adapter.render_table connector ~max_width:60 model_columns rows
  | UsageProvider ->
      let providers = Request_stats.summary_by_provider ~db in
      if providers = [] then "No usage data recorded yet."
      else
        let provider_columns =
          Table_format.
            [
              {
                header = "PROVIDER";
                align = Left;
                min_width = 10;
                flex = false;
              };
              { header = "TURNS"; align = Right; min_width = 5; flex = false };
              { header = "PROMPT"; align = Right; min_width = 6; flex = false };
              {
                header = "COMPLETION";
                align = Right;
                min_width = 6;
                flex = false;
              };
            ]
        in
        let rows =
          List.map
            (fun (provider, (s : Request_stats.summary)) ->
              [
                provider;
                string_of_int s.total_turns;
                Request_stats.format_tokens s.total_prompt_tokens;
                Request_stats.format_tokens s.total_completion_tokens;
              ])
            providers
        in
        Format_adapter.bold connector "Provider Usage"
        ^ "\n\n"
        ^ Format_adapter.render_table connector ~max_width:60 provider_columns
            rows

(* ── Existing format: active ───────────────────────────────────────────── *)

(* ── Active usage formatting ──────────────────────────────────────────── *)

let format_active ~connector ~db ~(config : Runtime_config.t) () =
  let five_hr =
    Request_stats.summary_for_period ~db ~since:"datetime('now', '-5 hours')"
  in
  let five_hr_by_model =
    Request_stats.summary_by_model_for_period ~db
      ~since:"datetime('now', '-5 hours')"
  in
  Provider_quota.set_cache_ttl config.quota_cache_ttl_s;
  let quota_results =
    Provider_quota.get_all_cached () |> List.map (fun (_name, pq) -> pq)
  in
  let buf = Buffer.create 512 in
  Buffer.add_string buf
    (Format_adapter.bold connector "Active Usage (5h window)");
  Buffer.add_string buf "\n\n";
  if five_hr.total_turns = 0 && quota_results = [] then
    Buffer.add_string buf "No usage data in the last 5 hours."
  else begin
    if five_hr.total_turns > 0 then begin
      let summary_columns =
        Table_format.
          [
            { header = "PERIOD"; align = Left; min_width = 12; flex = false };
            { header = "COST"; align = Right; min_width = 8; flex = false };
            { header = "TURNS"; align = Right; min_width = 5; flex = false };
            { header = "PROMPT"; align = Right; min_width = 6; flex = false };
            { header = "ADDED"; align = Right; min_width = 6; flex = false };
            {
              header = "COMPLETION";
              align = Right;
              min_width = 6;
              flex = false;
            };
          ]
      in
      let rows =
        [
          [
            "Last 5 hours";
            Printf.sprintf "$%.4f" five_hr.total_cost_usd;
            string_of_int five_hr.total_turns;
            Request_stats.format_tokens five_hr.total_prompt_tokens;
            Request_stats.format_tokens five_hr.total_added_prompt_tokens;
            Request_stats.format_tokens five_hr.total_completion_tokens;
          ];
        ]
      in
      Buffer.add_string buf
        (Format_adapter.render_table connector ~max_width:60 summary_columns
           rows);
      if five_hr_by_model <> [] then begin
        Buffer.add_string buf "\n";
        Buffer.add_string buf (Format_adapter.bold connector "By Model (5h)");
        Buffer.add_string buf "\n\n";
        let model_columns =
          Table_format.
            [
              { header = "MODEL"; align = Left; min_width = 15; flex = true };
              { header = "COST"; align = Right; min_width = 8; flex = false };
              { header = "TURNS"; align = Right; min_width = 5; flex = false };
              { header = "PROMPT"; align = Right; min_width = 6; flex = false };
              {
                header = "COMPLETION";
                align = Right;
                min_width = 6;
                flex = false;
              };
            ]
        in
        let model_rows =
          List.map
            (fun (ms : Request_stats.model_summary) ->
              [
                ms.provider ^ ":" ^ ms.model;
                Printf.sprintf "$%.4f" ms.summary.total_cost_usd;
                string_of_int ms.summary.total_turns;
                Request_stats.format_tokens ms.summary.total_prompt_tokens;
                Request_stats.format_tokens ms.summary.total_completion_tokens;
              ])
            five_hr_by_model
        in
        Buffer.add_string buf
          (Format_adapter.render_table connector ~max_width:60 model_columns
             model_rows)
      end
    end;
    if quota_results <> [] then begin
      if five_hr.total_turns > 0 then Buffer.add_string buf "\n";
      Buffer.add_string buf (Format_adapter.bold connector "Provider Quota");
      Buffer.add_string buf "\n\n";
      let quota_columns =
        Table_format.
          [
            { header = "PROVIDER"; align = Left; min_width = 10; flex = false };
            { header = "SESSION"; align = Right; min_width = 7; flex = false };
            { header = "WEEKLY"; align = Right; min_width = 7; flex = false };
            { header = "MONTHLY"; align = Right; min_width = 7; flex = false };
            { header = "STATUS"; align = Left; min_width = 6; flex = false };
          ]
      in
      let quota_rows =
        List.map
          (fun (pq : Provider_quota.provider_quota) ->
            let sess, week, mon =
              match pq.state with
              | Provider_quota.Unknown _ -> ("-", "-", "-")
              | Provider_quota.Known { session; weekly; monthly } ->
                  let fmt_pct = function
                    | None -> "-"
                    | Some w ->
                        Printf.sprintf "%.0f%%" w.Provider_quota.used_pct
                  in
                  (fmt_pct session, fmt_pct weekly, fmt_pct monthly)
            in
            let threshold =
              match List.assoc_opt pq.provider_name config.providers with
              | Some pc -> Option.value ~default:0.85 pc.quota_threshold
              | None -> 0.85
            in
            let status = Provider_quota.status_label ~threshold pq in
            [ pq.provider_name; sess; week; mon; status ])
          quota_results
      in
      Buffer.add_string buf
        (Format_adapter.render_table connector ~max_width:60 quota_columns
           quota_rows)
    end
  end;
  Buffer.contents buf

(* ── Background task formatting ───────────────────────────────────────── *)

(* ── Existing format: bg ───────────────────────────────────────────────── *)

let format_bg ~connector ~db action =
  match action with
  | BgList ->
      let tasks, hidden = Background_task.list_tasks_for_display ~db in
      if tasks = [] && hidden = 0 then "No background tasks."
      else
        let columns, rows = Background_task.task_list_table_data tasks in
        let footer =
          if hidden > 0 then
            Printf.sprintf
              "\n\
              \  (%d older task%s hidden. Use `clawq background show <id>` to \
               view.)"
              hidden
              (if hidden = 1 then "" else "s")
          else ""
        in
        Format_adapter.bold connector "Background tasks:"
        ^ "\n"
        ^ Format_adapter.render_table connector ~max_width:80 columns rows
        ^ footer
  | BgShow id -> (
      match Background_task.get_task ~db ~id with
      | Some task ->
          Format_adapter.code_block connector
            (Background_task.format_task_summary ~full:true task)
      | None -> Printf.sprintf "No background task found with id %d." id)
  | BgLogs id -> (
      match Background_task.get_task ~db ~id with
      | None -> Printf.sprintf "No background task found with id %d." id
      | Some task -> (
          match task.log_path with
          | None | Some "" -> Printf.sprintf "Task %d has no log file." id
          | Some path -> (
              if not (Sys.file_exists path) then
                Printf.sprintf "Log file not found: %s" path
              else
                try
                  let ic = open_in path in
                  Fun.protect
                    ~finally:(fun () -> close_in_noerr ic)
                    (fun () ->
                      let len = in_channel_length ic in
                      let max_bytes = 4000 in
                      if len <= max_bytes then (
                        let buf = Buffer.create len in
                        (try
                           while true do
                             Buffer.add_char buf (input_char ic)
                           done
                         with End_of_file -> ());
                        Printf.sprintf "Task %d logs (%s):\n%s" id path
                          (Format_adapter.code_block connector
                             (Buffer.contents buf)))
                      else (
                        seek_in ic (len - max_bytes);
                        let buf = Buffer.create max_bytes in
                        (try
                           while true do
                             Buffer.add_char buf (input_char ic)
                           done
                         with End_of_file -> ());
                        Printf.sprintf
                          "Task %d logs (last ~%d bytes of %s):\n...%s" id
                          max_bytes path
                          (Format_adapter.code_block connector
                             (Buffer.contents buf))))
                with exn ->
                  Printf.sprintf "Error reading log for task %d: %s" id
                    (Printexc.to_string exn))))
  | BgCancel id -> (
      match Background_task.cancel ~db ~id with
      | Ok msg -> msg
      | Error msg -> msg)
  | BgRetry id -> (
      match Background_task.retry ~db ~id with
      | Ok msg -> msg
      | Error msg -> msg)
  | BgCreate (agent_name, prompt) -> (
      match Background_task.resolve_runner () with
      | Error msg -> Printf.sprintf "Cannot create task: %s" msg
      | Ok (runner, default_model) -> (
          let repo_path = Sys.getcwd () in
          let result =
            Background_task.enqueue ~db ~runner ?model:default_model ~repo_path
              ?agent_name ~prompt ()
          in
          match result with
          | Ok id ->
              let agent_suffix =
                match agent_name with
                | Some name -> Printf.sprintf " [agent: %s]" name
                | None -> ""
              in
              Printf.sprintf "Background task #%d created (runner: %s)%s." id
                (Background_task.string_of_runner runner)
                agent_suffix
          | Error msg -> Printf.sprintf "Failed to create task: %s" msg))

(* ── Status formatting ────────────────────────────────────────────────── *)

(* ── Existing format: status ───────────────────────────────────────────── *)

let format_status ~connector ~(db : Sqlite3.db option) ~session_count
    ~active_count () =
  let open Yojson.Safe.Util in
  let daemon_json = read_daemon_state_json () in
  let pid = Daemon_status.read_current_daemon_pid () in
  let status_str = match pid with Some _ -> "Running" | None -> "Unknown" in
  let uptime_str =
    match pid with
    | Some p -> (
        match Daemon_status.daemon_uptime_suffix p with
        | Some s -> s
        | None -> "unavailable")
    | None -> "not running"
  in
  let pid_str =
    match pid with Some p -> string_of_int p | None -> "not running"
  in
  let version_str = Build_info.version_string in
  let build_date_str = Build_info.build_date in
  let sessions_str =
    Printf.sprintf "%d total, %d active" session_count active_count
  in
  let db_sessions_str =
    match db with
    | Some db -> string_of_int (List.length (Memory.list_sessions ~db))
    | None -> "n/a"
  in
  let gateway_str =
    match daemon_json with
    | Some json -> (
        try
          let host = json |> member "gateway_host" |> to_string in
          let port = json |> member "gateway_port" |> to_int in
          Printf.sprintf "%s:%d" host port
        with _ -> "unknown")
    | None -> "unknown"
  in
  let connector_status name field =
    match daemon_json with
    | Some json -> (
        try
          let enabled = json |> member field |> to_bool in
          let running =
            try
              let components = json |> member "components" |> to_assoc in
              match List.assoc_opt name components with
              | Some (`String "running") -> true
              | _ -> false
            with _ -> false
          in
          if running then "+ running"
          else if enabled then "~ enabled"
          else "- disabled"
        with _ -> "- disabled")
    | None -> "? unknown"
  in
  let telegram_str = connector_status "telegram" "telegram_enabled" in
  let discord_str = connector_status "discord" "discord_enabled" in
  let slack_str = connector_status "slack" "slack_enabled" in
  let teams_str = connector_status "teams" "teams_enabled" in
  let github_str = connector_status "github" "github_enabled" in
  let tunnel_str, tunnel_url =
    match daemon_json with
    | Some json -> (
        try
          let tunnel = json |> member "tunnel" in
          if tunnel = `Null then ("inactive", None)
          else
            let url =
              try Some (tunnel |> member "url" |> to_string) with _ -> None
            in
            ("active", url)
        with _ -> ("inactive", None))
    | None -> ("unknown", None)
  in
  let status_columns =
    Table_format.
      [
        { header = "FIELD"; align = Left; min_width = 12; flex = false };
        { header = "VALUE"; align = Left; min_width = 20; flex = true };
      ]
  in
  let rows =
    [
      [ "Status"; status_str ];
      [ "Uptime"; uptime_str ];
      [ "PID"; pid_str ];
      [ "Version"; version_str ];
      [ "Build Date"; build_date_str ];
      [ "Sessions"; sessions_str ];
      [ "DB Sessions"; db_sessions_str ];
      [ "Gateway"; gateway_str ];
      [ "Telegram"; telegram_str ];
      [ "Discord"; discord_str ];
      [ "Slack"; slack_str ];
      [ "Teams"; teams_str ];
      [ "GitHub"; github_str ];
      [ "Tunnel"; tunnel_str ];
    ]
  in
  let rows =
    match tunnel_url with
    | Some url -> rows @ [ [ "Tunnel URL"; url ] ]
    | None -> rows
  in
  Format_adapter.bold connector "Bot Status"
  ^ "\n\n"
  ^ Format_adapter.render_table connector ~max_width:60 status_columns rows

(* ── Model usage/quota formatting ─────────────────────────────────────── *)

(* ── Existing format: model usage (quota) ──────────────────────────────── *)

let format_model_usage ~connector ~(config : Runtime_config.t)
    (results : Provider_quota.provider_quota list) =
  if results = [] then "No providers configured."
  else
    let columns =
      Table_format.
        [
          { header = "PROVIDER"; align = Left; min_width = 10; flex = false };
          { header = "SESSION"; align = Right; min_width = 7; flex = false };
          { header = "WEEKLY"; align = Right; min_width = 7; flex = false };
          { header = "MONTHLY"; align = Right; min_width = 7; flex = false };
          { header = "STATUS"; align = Left; min_width = 6; flex = false };
        ]
    in
    let rows =
      List.map
        (fun (pq : Provider_quota.provider_quota) ->
          let sess, week, mon =
            match pq.state with
            | Provider_quota.Unknown _ -> ("-", "-", "-")
            | Provider_quota.Known { session; weekly; monthly } ->
                let fmt_pct = function
                  | None -> "-"
                  | Some w -> Printf.sprintf "%.0f%%" w.Provider_quota.used_pct
                in
                (fmt_pct session, fmt_pct weekly, fmt_pct monthly)
          in
          let threshold =
            match List.assoc_opt pq.provider_name config.providers with
            | Some pc -> Option.value ~default:0.85 pc.quota_threshold
            | None -> 0.85
          in
          let status = Provider_quota.status_label ~threshold pq in
          [ pq.provider_name; sess; week; mon; status ])
        results
    in
    Format_adapter.bold connector "Provider Quota/Usage"
    ^ "\n\n"
    ^ Format_adapter.render_table connector ~max_width:60 columns rows

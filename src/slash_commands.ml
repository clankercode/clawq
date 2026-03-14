type command = { name : string; description : string }
type thinking_action = ShowThinking | SetThinking of string option
type show_thinking_action = ShowThinkingStatus | ToggleShowThinking
type heartbeat_action = HeartbeatStatus | SetHeartbeat of bool

type model_action =
  | ModelShow
  | ModelSet of string
  | ModelSetDefault of string
  | ModelFav of string
  | ModelUnfav of string
  | ModelList of string option
  | ModelUsage

type costs_action =
  | CostsSummary
  | CostsSessions
  | CostsSession of string
  | CostsModel
  | CostsProvider

type usage_action =
  | UsageSummary
  | UsageSessions
  | UsageSession of string
  | UsageModel
  | UsageProvider

type result =
  | Reply of string
  | Help
  | Reset
  | Compact
  | RuntimeCtx
  | Uptime
  | Status
  | Thinking of thinking_action
  | ShowThinking of show_thinking_action
  | Heartbeat of heartbeat_action
  | Delegate of string
  | ForkAnd of string
  | Tools
  | Tasks
  | Costs of costs_action
  | Usage of usage_action
  | Model of model_action
  | DebugDumpChat
  | NotACommand

let allowed_thinking_levels = [ "low"; "medium"; "high"; "off"; "xhigh"; "max" ]
let thinking_level_to_string = function Some level -> level | None -> "off"

let parse_thinking_level value =
  match String.lowercase_ascii value with
  | "low" -> Some (Some "low")
  | "medium" -> Some (Some "medium")
  | "high" -> Some (Some "high")
  | "off" -> Some None
  | "xhigh" -> Some (Some "xhigh")
  | "max" -> Some (Some "max")
  | _ -> None

let thinking_usage () =
  Printf.sprintf "Usage: /thinking [%s]"
    (String.concat "/" allowed_thinking_levels)

let invalid_thinking_level_message value =
  Printf.sprintf "Invalid thinking level '%s'. Use one of: %s" value
    (String.concat ", " allowed_thinking_levels)

let commands =
  [
    { name = "start"; description = "Start the bot" };
    { name = "help"; description = "Show available commands" };
    { name = "new"; description = "Start a new conversation" };
    { name = "status"; description = "Show bot status" };
    { name = "runtime_ctx"; description = "Show current runtime context" };
    { name = "uptime"; description = "Show current daemon uptime" };
    {
      name = "thinking";
      description = "Show or set thinking level: /thinking [level]";
    };
    {
      name = "compact";
      description = "Compact session history (summarize older messages)";
    };
    { name = "pair"; description = "Pair with TOTP code: /pair <6-digit-code>" };
    {
      name = "update";
      description = "Pull, rebuild, and gracefully restart clawq";
    };
    {
      name = "delegate";
      description =
        "Delegate a prompt to a temporary subagent: /delegate <prompt>";
    };
    {
      name = "show_thinking";
      description = "Toggle display of model thinking in responses";
    };
    {
      name = "config";
      description = "View or modify config: /config [show/get/set/keys]";
    };
    {
      name = "heartbeat";
      description = "Show or set heartbeat routing for this session";
    };
    {
      name = "fork_and";
      description =
        "Fork the current session and run a prompt: /fork_and <prompt>";
    };
    { name = "tools"; description = "List all available tools" };
    { name = "tasks"; description = "Show the agent's current task tree" };
    {
      name = "model";
      description = "Manage model: /model [set/fav/unfav/list/usage] [args]";
    };
    {
      name = "costs";
      description = "Show cost breakdowns: /costs [session/model/provider]";
    };
    {
      name = "usage";
      description = "Show token usage: /usage [session/model/provider]";
    };
    {
      name = "debug_dump_chat";
      description = "Dump session to file and send as attachment";
    };
  ]

let pad_right text width =
  let len = String.length text in
  if len >= width then text else text ^ String.make (width - len) ' '

let help_text =
  let command_labels = List.map (fun c -> "/" ^ c.name) commands in
  let command_width =
    List.fold_left
      (fun acc label -> max acc (String.length label))
      0 command_labels
  in
  let rows =
    List.map
      (fun c ->
        let label = pad_right ("/" ^ c.name) command_width in
        Printf.sprintf "  %s  %s" label c.description)
      commands
  in
  String.concat "\n"
    ([ "Available commands:"; "" ]
    @ rows
    @ [
        "";
        "Prefix a message with ! to interrupt the current turn in this session \
         and send the rest as a normal message.";
      ])

let help_text_telegram =
  let rows =
    List.map
      (fun c ->
        Printf.sprintf "%s  %s"
          (Format_adapter.code Format_adapter.Telegram_html ("/" ^ c.name))
          (Format_adapter.escape Format_adapter.Telegram_html c.description))
      commands
  in
  String.concat "\n"
    ([
       Format_adapter.bold Format_adapter.Telegram_html "Available commands:";
       "";
     ]
    @ rows
    @ [
        "";
        Format_adapter.escape Format_adapter.Telegram_html
          "Prefix a message with ! to interrupt the current turn in this \
           session and send the rest as a normal message.";
      ])

let format_help ~connector =
  Format_adapter.dispatch connector ~telegram_html:help_text_telegram
    ~default:help_text

let costs_usage =
  "Usage: /costs [session [KEY]/model/provider]\n\
  \  /costs                 - Cost summary by time period\n\
  \  /costs session         - Cost breakdown across sessions\n\
  \  /costs session <key>   - Cost breakdown for one session\n\
  \  /costs model           - Cost breakdown by model\n\
  \  /costs provider        - Cost breakdown by provider"

let usage_usage =
  "Usage: /usage [session [KEY]/model/provider]\n\
  \  /usage                 - Usage summary by time period\n\
  \  /usage session         - Usage breakdown across sessions\n\
  \  /usage session <key>   - Usage breakdown for one session\n\
  \  /usage model           - Usage breakdown by model\n\
  \  /usage provider        - Usage breakdown by provider"

let handle text =
  let trimmed = String.trim text in
  if String.length trimmed = 0 || trimmed.[0] <> '/' then NotACommand
  else
    let parts =
      String.split_on_char ' ' trimmed |> List.filter (fun part -> part <> "")
    in
    match parts with
    | [] -> NotACommand
    | first :: args -> (
        let cmd =
          if String.length first <= 1 then ""
          else String.sub first 1 (String.length first - 1)
        in
        let cmd =
          match String.index_opt cmd '@' with
          | Some idx -> String.sub cmd 0 idx
          | None -> cmd
        in
        let cmd_lower = String.lowercase_ascii cmd in
        match cmd_lower with
        | "start" ->
            Reply
              "clawq bot ready. Send me a message and I'll respond using AI.\n\
               Use /help to see available commands. Prefix a message with ! to \
               interrupt the current turn."
        | "help" -> Help
        | "new" -> Reset
        | "compact" -> Compact
        | "runtime-ctx" | "runtime_ctx" -> RuntimeCtx
        | "uptime" -> Uptime
        | "status" -> Status
        | "thinking" -> (
            match args with
            | [] -> Thinking ShowThinking
            | [ value ] -> (
                match parse_thinking_level value with
                | Some level -> Thinking (SetThinking level)
                | None -> Reply (invalid_thinking_level_message value))
            | _ -> Reply (thinking_usage ()))
        | "show-thinking" | "show_thinking" | "toggle-show-thinking" -> (
            match args with
            | [] -> ShowThinking ToggleShowThinking
            | [ s ] when String.lowercase_ascii s = "status" ->
                ShowThinking ShowThinkingStatus
            | _ -> Reply "Usage: /show_thinking [status]")
        | "heartbeat" -> (
            match args with
            | [] | [ "status" ] -> Heartbeat HeartbeatStatus
            | [ "on" ] -> Heartbeat (SetHeartbeat true)
            | [ "off" ] -> Heartbeat (SetHeartbeat false)
            | _ -> Reply "Usage: /heartbeat [on/off/status]")
        | "delegate" -> (
            match args with
            | [] -> Reply "Usage: /delegate <prompt>"
            | _ -> Delegate (String.concat " " args))
        | "config" -> (
            match args with
            | [] | [ "help" ] ->
                Reply
                  "Usage: /config <subcommand>\n\n\
                   Subcommands:\n\
                  \  show [SECTION]  — Show config (or a specific section)\n\
                  \  get KEY         — Get a config value by dot-path\n\
                  \  set KEY VALUE   — Set a config value\n\
                  \  keys [PREFIX]   — List valid config key paths\n\
                  \  wizard          — Run the interactive setup wizard"
            | [ "show" ] ->
                let output = Config_show.show None in
                if String.length output > 1500 then
                  let sections =
                    match
                      try Some (Yojson.Safe.from_string output) with _ -> None
                    with
                    | Some (`Assoc fields) ->
                        List.map fst fields |> String.concat ", "
                    | _ -> "(unable to list sections)"
                  in
                  Reply
                    (Printf.sprintf
                       "Config is too large to display in chat. Available \
                        sections:\n\
                        %s\n\n\
                        Use: /config show <section>\n\
                        Example: /config show gateway"
                       sections)
                else Reply output
            | [ "show"; section ] -> Reply (Config_show.show (Some section))
            | [ "get" ] -> Reply "Usage: /config get KEY"
            | [ "get"; key ] -> Reply (Config_set.get_value_redacted key)
            | "set" :: key :: value_parts when value_parts <> [] ->
                let segments = Config_set.split_path key in
                if
                  segments <> [ "" ]
                  && not
                       (Config_set.validate_path segments
                          Config_set.config_schema)
                then Reply (Config_set.suggest_key key segments)
                else if
                  segments <> [ "" ]
                  && not
                       (Config_set.validate_set_path segments
                          Config_set.config_schema)
                then
                  Reply
                    (Config_set.section_not_settable_error
                       ~show_cmd:"/config show" key)
                else if Config_set.is_secret_path key then
                  Reply
                    (Printf.sprintf
                       "Cannot set secret key '%s' via chat. Use the terminal: \
                        clawq config set %s <value>"
                       key key)
                else
                  let value = String.concat " " value_parts in
                  Reply
                    (Config_set.set_value key value
                    ^ "\nNote: restart the daemon for changes to take effect.")
            | [ "set" ] | [ "set"; _ ] -> Reply "Usage: /config set KEY VALUE"
            | [ "keys" ] ->
                let paths = Config_set.config_leaf_paths () in
                Reply (String.concat "\n" paths)
            | [ "keys"; prefix ] ->
                let paths = Config_set.config_leaf_paths () in
                let prefix_lower = String.lowercase_ascii prefix in
                let matches =
                  List.filter
                    (fun p ->
                      let p_lower = String.lowercase_ascii p in
                      String.length p_lower >= String.length prefix_lower
                      && String.sub p_lower 0 (String.length prefix_lower)
                         = prefix_lower)
                    paths
                in
                if matches = [] then
                  Reply
                    (Printf.sprintf "No config keys matching prefix '%s'."
                       prefix)
                else Reply (String.concat "\n" matches)
            | [ "wizard" ] ->
                Reply
                  "The config wizard requires an interactive terminal.\n\
                   Run: clawq config wizard"
            | sub :: _ ->
                Reply
                  (Printf.sprintf
                     "Unknown config subcommand '%s'.\n\
                      Use /config for usage help."
                     sub))
        | "fork-and" | "fork_and" -> (
            match args with
            | [] -> Reply "Usage: /fork_and <prompt>"
            | _ -> ForkAnd (String.concat " " args))
        | "tools" -> Tools
        | "tasks" -> Tasks
        | "costs" -> (
            match args with
            | [] -> Costs CostsSummary
            | [ "session" ] -> Costs CostsSessions
            | [ "session"; key ] -> Costs (CostsSession key)
            | [ "model" ] -> Costs CostsModel
            | [ "provider" ] -> Costs CostsProvider
            | _ -> Reply costs_usage)
        | "usage" -> (
            match args with
            | [] -> Usage UsageSummary
            | [ "session" ] -> Usage UsageSessions
            | [ "session"; key ] -> Usage (UsageSession key)
            | [ "model" ] -> Usage UsageModel
            | [ "provider" ] -> Usage UsageProvider
            | _ -> Reply usage_usage)
        | "model" -> (
            let known_subcommands =
              [ "set"; "fav"; "unfav"; "list"; "usage" ]
            in
            match args with
            | [] -> Model ModelShow
            | [ "set"; name ] -> Model (ModelSet name)
            | [ "set-default"; name ] -> Model (ModelSetDefault name)
            | [ "fav"; name ] -> Model (ModelFav name)
            | [ "unfav"; name ] -> Model (ModelUnfav name)
            | "list" :: rest ->
                let provider = match rest with [ p ] -> Some p | _ -> None in
                Model (ModelList provider)
            | [ "usage" ] -> Model ModelUsage
            | first :: _
              when not
                     (List.mem (String.lowercase_ascii first) known_subcommands)
              ->
                Model (ModelSet (String.concat " " args))
            | _ ->
                Reply
                  "Usage: /model [set/set-default/fav/unfav/list/usage] [args]\n\
                  \  /model                    — Show current model and \
                   favorites\n\
                  \  /model set <name>         — Set model for this session\n\
                  \  /model set-default <name> — Set default model in config \
                   (persistent)\n\
                  \  /model fav <name>         — Toggle favorite status\n\
                  \  /model unfav <name>       — Remove from favorites\n\
                  \  /model list [provider]    — List available models\n\
                  \  /model usage              — Show provider quota/usage")
        | "debug-dump-chat" | "debug_dump_chat" -> DebugDumpChat
        | "" -> NotACommand
        | _ -> NotACommand)

let reset_message ?(active_bg_tasks = 0) () =
  if active_bg_tasks > 0 then
    Printf.sprintf
      "Session reset. Send a new message to start fresh.\n\
       Note: %d active background task(s) are still running."
      active_bg_tasks
  else "Session reset. Send a new message to start fresh."

let risk_level_string (r : Tool.risk_level) =
  match r with Low -> "Low" | Medium -> "Medium" | High -> "High"

let extract_params (schema : Yojson.Safe.t) : (string * string * bool) list =
  let open Yojson.Safe.Util in
  let props = try schema |> member "properties" |> to_assoc with _ -> [] in
  let required =
    try schema |> member "required" |> to_list |> List.map to_string
    with _ -> []
  in
  List.map
    (fun (name, v) ->
      let typ = try v |> member "type" |> to_string with _ -> "string" in
      let is_required = List.mem name required in
      (name, typ, is_required))
    props

let format_tool_plain buf (t : Tool.t) =
  Buffer.add_char buf '\n';
  Buffer.add_string buf
    (Printf.sprintf "%s [%s]\n" t.name (risk_level_string t.risk_level));
  Buffer.add_string buf (Printf.sprintf "  %s\n" t.description);
  let params = extract_params t.parameters_schema in
  if params <> [] then
    let param_strs =
      List.map
        (fun (name, typ, req) ->
          if req then Printf.sprintf "%s* (%s)" name typ
          else Printf.sprintf "%s (%s)" name typ)
        params
    in
    Buffer.add_string buf
      (Printf.sprintf "  Args: %s\n" (String.concat ", " param_strs))

let format_tools_plain (tools : Tool.t list) (skills : Tool.t list) : string =
  let sort_tools ts =
    List.sort (fun (a : Tool.t) b -> String.compare a.name b.name) ts
  in
  let sorted = sort_tools tools in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (Printf.sprintf "Tools (%d):\n" (List.length sorted));
  List.iter (format_tool_plain buf) sorted;
  if skills <> [] then begin
    let sorted_skills = sort_tools skills in
    Buffer.add_string buf
      (Printf.sprintf "\n\nSkills (%d):\n" (List.length sorted_skills));
    List.iter (format_tool_plain buf) sorted_skills
  end;
  Buffer.contents buf

let truncate_description desc max_len =
  if String.length desc <= max_len then desc
  else String.sub desc 0 (max_len - 3) ^ "..."

let format_tool_telegram buf (t : Tool.t) =
  let params = extract_params t.parameters_schema in
  let param_str =
    if params = [] then ""
    else
      let names =
        List.map (fun (name, _, req) -> if req then name ^ "*" else name) params
      in
      " <code>" ^ String.concat " " names ^ "</code>"
  in
  Buffer.add_string buf (Printf.sprintf "<b>%s</b>%s\n" t.name param_str);
  Buffer.add_string buf (truncate_description t.description 60 ^ "\n\n")

let format_tools_telegram (tools : Tool.t list) (skills : Tool.t list) : string
    =
  let sort_tools ts =
    List.sort (fun (a : Tool.t) b -> String.compare a.name b.name) ts
  in
  let sorted = sort_tools tools in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf
    (Printf.sprintf "<b>Tools (%d)</b>\n\n" (List.length sorted));
  if sorted <> [] then begin
    Buffer.add_string buf "<blockquote expandable>\n";
    List.iter (format_tool_telegram buf) sorted;
    Buffer.add_string buf "</blockquote>"
  end;
  if skills <> [] then begin
    let sorted_skills = sort_tools skills in
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Printf.sprintf "<b>Skills (%d)</b>\n\n" (List.length sorted_skills));
    Buffer.add_string buf "<blockquote expandable>\n";
    List.iter (format_tool_telegram buf) sorted_skills;
    Buffer.add_string buf "</blockquote>"
  end;
  Buffer.contents buf

let format_tools ~connector tools skills =
  Format_adapter.dispatch connector ~telegram_html:format_tools_telegram
    ~default:format_tools_plain tools skills

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
        ^ "\n"
        ^ Format_adapter.code_block connector
            (Table_format.render ~max_width:60 cost_summary_columns rows)
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
        ^ "\n"
        ^ Format_adapter.code_block connector
            (Table_format.render ~max_width:60 session_columns rows)
  | CostsSession key ->
      let s = Request_stats.summary_for_session ~db ~session_key:key in
      if s.total_turns = 0 then
        Printf.sprintf "No cost data for session '%s'." key
      else
        let rows = [ cost_table_row "Total" s ] in
        Format_adapter.bold connector (Printf.sprintf "Costs for %s" key)
        ^ "\n"
        ^ Format_adapter.code_block connector
            (Table_format.render ~max_width:60 cost_summary_columns rows)
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
        ^ "\n"
        ^ Format_adapter.code_block connector
            (Table_format.render ~max_width:60 model_columns rows)
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
        ^ "\n"
        ^ Format_adapter.code_block connector
            (Table_format.render ~max_width:60 provider_columns rows)

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
        ^ "\n"
        ^ Format_adapter.code_block connector
            (Table_format.render ~max_width:60 usage_summary_columns rows)
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
        ^ "\n"
        ^ Format_adapter.code_block connector
            (Table_format.render ~max_width:60 session_columns rows)
  | UsageSession key ->
      let s = Request_stats.summary_for_session ~db ~session_key:key in
      if s.total_turns = 0 then
        Printf.sprintf "No usage data for session '%s'." key
      else
        let rows = [ usage_table_row "Total" s ] in
        Format_adapter.bold connector (Printf.sprintf "Usage for %s" key)
        ^ "\n"
        ^ Format_adapter.code_block connector
            (Table_format.render ~max_width:60 usage_summary_columns rows)
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
        ^ "\n"
        ^ Format_adapter.code_block connector
            (Table_format.render ~max_width:60 model_columns rows)
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
        ^ "\n"
        ^ Format_adapter.code_block connector
            (Table_format.render ~max_width:60 provider_columns rows)

let read_daemon_state_json () =
  try
    let path = Filename.concat (Dot_dir.path ()) "daemon_state.json" in
    if Sys.file_exists path then Some (Yojson.Safe.from_file path) else None
  with _ -> None

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
  ^ "\n"
  ^ Format_adapter.code_block connector
      (Table_format.render ~max_width:60 status_columns rows)

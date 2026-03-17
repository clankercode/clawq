(** Format functions and types for slash commands.

    This module is [include]-d by {!Slash_commands} so that all types and format
    helpers remain accessible under the [Slash_commands] namespace. *)

(* ── Types ─────────────────────────────────────────────────────────────── *)

type command = { name : string; description : string; priority : int }
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

type bg_action =
  | BgList
  | BgShow of int
  | BgLogs of int
  | BgCancel of int
  | BgRetry of int
  | BgCreate of string option * string

type cron_action =
  | CronList
  | CronAdd of {
      name : string;
      schedule : string;
      message : string;
      ttl : string option;
    }
  | CronEdit of {
      name : string;
      schedule : string option;
      message : string option;
      ttl : string option;
    }
  | CronRemove of string
  | CronShow of string
  | CronHistory of string option
  | CronHelp

type bl_action = BlList | BlShow of string | BlBugs | BlIdeas

type rig_action =
  | RigInstall of string
  | RigAdjust of string
  | RigRemove of string
  | RigList

type result =
  | Reply of string
  | FormattedReply of (Format_adapter.connector -> string)
  | Help
  | Reset
  | Compact
  | RuntimeCtx
  | Uptime
  | Status
  | Thinking of thinking_action
  | ShowThinking of show_thinking_action
  | Heartbeat of heartbeat_action
  | Delegate of string option * string
  | ForkAnd of string option * string
  | Tools
  | Tasks
  | TasksFull
  | Costs of costs_action
  | Usage of usage_action
  | Model of model_action
  | Menu of int
  | Active
  | Bg of bg_action
  | Cron of cron_action
  | Bl of bl_action
  | Rig of rig_action
  | DebugDumpChat
  | AgentInvoke of string * string
  | AgentMenu of int
  | ModelMenu of int
  | ThinkingMenu
  | ConfigMenu of int
  | SkillsMenu of int
  | CostsMenu
  | BgMenu
  | InjectConnectorHistory of int
  | SkillInvoke of string * string
  | AdminRequired of result
  | RegisterAsAdminOtc of string option
  | NotACommand

(* ── Thinking helpers ──────────────────────────────────────────────────── *)

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

(* ── Admin gating ─────────────────────────────────────────────────────── *)

let gate_admin ~is_admin result =
  match result with
  | AdminRequired inner ->
      if is_admin then inner
      else
        Reply
          "This command requires admin privileges. Use /register_as_admin_otc \
           to register as an admin."
  | other -> other

(* ── Commands list ─────────────────────────────────────────────────────── *)

let commands =
  [
    { name = "help"; description = "Show available commands"; priority = 100 };
    { name = "new"; description = "Start a new conversation"; priority = 95 };
    { name = "status"; description = "Show bot status"; priority = 90 };
    {
      name = "model";
      description =
        "Manage model: /model [set/fav/unfav/list/usage/menu] [args]";
      priority = 7;
    };
    {
      name = "thinking";
      description = "Show or set thinking level: /thinking [level]";
      priority = 6;
    };
    { name = "tools"; description = "List all available tools"; priority = 48 };
    {
      name = "tasks";
      description = "Show task tree (compact): /tasks [full]";
      priority = 4;
    };
    {
      name = "costs";
      description = "Show cost breakdowns: /costs [session/model/provider]";
      priority = 65;
    };
    {
      name = "usage";
      description = "Show token usage: /usage [session/model/provider]";
      priority = 60;
    };
    {
      name = "active";
      description = "Show active 5-hour window usage (cost, tokens, quota)";
      priority = 63;
    };
    {
      name = "bg";
      description =
        "Background tasks: /bg [list/show/logs/cancel/retry/create] \
         [id/@agent/prompt]";
      priority = 57;
    };
    {
      name = "cron";
      description =
        "Manage cron jobs: /cron [list/show/add/edit/remove/history] [args]";
      priority = 58;
    };
    {
      name = "bl";
      description = "Backlog overview: /bl [list/bugs/ideas/show] [id]";
      priority = 56;
    };
    {
      name = "rig";
      description =
        "Manage setup rigs: /rig [install/adjust/remove/list] [name]";
      priority = 50;
    };
    {
      name = "delegate";
      description = "Delegate to a subagent: /delegate [@agent] <prompt>";
      priority = 55;
    };
    {
      name = "agent";
      description =
        "Invoke an agent template: /agent <name> <prompt> or /agent list";
      priority = 54;
    };
    {
      name = "fork_and";
      description = "Fork session and run a prompt: /fork_and [@agent] <prompt>";
      priority = 75;
    };
    {
      name = "compact";
      description = "Compact session history (summarize older messages)";
      priority = 45;
    };
    { name = "start"; description = "Start the bot"; priority = 40 };
    {
      name = "show_thinking";
      description = "Toggle display of model thinking in responses";
      priority = 35;
    };
    {
      name = "heartbeat";
      description = "Show or set heartbeat routing for this session";
      priority = 30;
    };
    {
      name = "config";
      description = "View or modify config: /config [show/get/set/keys]";
      priority = 25;
    };
    {
      name = "uptime";
      description = "Show current daemon uptime";
      priority = 20;
    };
    {
      name = "runtime_ctx";
      description = "Show current runtime context";
      priority = 15;
    };
    {
      name = "pair";
      description = "Pair with TOTP code: /pair <6-digit-code>";
      priority = 10;
    };
    {
      name = "update";
      description = "Pull, rebuild, and gracefully restart clawq";
      priority = 8;
    };
    { name = "skills"; description = "List available skills"; priority = 47 };
    {
      name = "menu";
      description = "Show interactive command menu";
      priority = 93;
    };
    {
      name = "debug_dump_chat";
      description = "Dump session to file and send as attachment";
      priority = 3;
    };
    {
      name = "register_as_admin_otc";
      description = "Register as admin via one-time code";
      priority = 2;
    };
  ]

let sorted_by_priority () =
  List.sort (fun a b -> compare b.priority a.priority) commands

(* ── Small helpers ─────────────────────────────────────────────────────── *)

let pad_right text width =
  let len = String.length text in
  if len >= width then text else text ^ String.make (width - len) ' '

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

let truncate_description desc max_len =
  if String.length desc <= max_len then desc
  else String.sub desc 0 (max_len - 3) ^ "..."

let read_daemon_state_json () =
  try
    let path = Filename.concat (Dot_dir.path ()) "daemon_state.json" in
    if Sys.file_exists path then Some (Yojson.Safe.from_file path) else None
  with _ -> None

(* ── New format functions for FormattedReply closures ─────────────────── *)

let format_start ~connector =
  Format_adapter.bold connector "clawq"
  ^ " bot ready. Send me a message and I'll respond using AI.\nUse "
  ^ Format_adapter.code connector "/help"
  ^ " to see available commands. Prefix a message with "
  ^ Format_adapter.code connector "!"
  ^ " to interrupt the current turn."

let format_thinking_usage ~connector =
  "Usage: "
  ^ Format_adapter.code connector "/thinking"
  ^ " ["
  ^ String.concat "/" allowed_thinking_levels
  ^ "]"

let format_invalid_thinking_level ~connector value =
  "Invalid thinking level "
  ^ Format_adapter.code connector (Printf.sprintf "'%s'" value)
  ^ ". Use one of: "
  ^ String.concat ", "
      (List.map (Format_adapter.code connector) allowed_thinking_levels)

let format_show_thinking_usage ~connector =
  "Usage: " ^ Format_adapter.code connector "/show_thinking" ^ " [status]"

let format_heartbeat_usage ~connector =
  "Usage: " ^ Format_adapter.code connector "/heartbeat" ^ " [on/off/status]"

let format_delegate_usage ~connector =
  "Usage: "
  ^ Format_adapter.code connector "/delegate [@agent] <prompt>"
  ^ "\n\nOptionally prefix with "
  ^ Format_adapter.code connector "@agent_name"
  ^ " to use that agent's system prompt and tool restrictions."

let format_agent_usage ~connector =
  "Usage: "
  ^ Format_adapter.code connector "/agent <name> <prompt>"
  ^ "\n\nSubcommands:\n"
  ^ Format_adapter.code connector "/agent list"
  ^ " — list available agent templates\n"
  ^ Format_adapter.code connector "/agent menu"
  ^ " — interactive agent menu\n"
  ^ Format_adapter.code connector "/agent <name> <prompt>"
  ^ " — invoke a named agent template"

let format_agent_list ~connector =
  let all = Agent_template.available_templates () in
  if all = [] then "No agent templates available."
  else
    let lines =
      List.map
        (fun (t : Agent_template.t) ->
          Format_adapter.bold connector t.name ^ " — " ^ t.description)
        all
    in
    "Available agent templates:\n" ^ String.concat "\n" lines

let agents_per_page = 8

let format_agent_menu ~connector ~page =
  let all = Agent_template.available_templates () in
  if all = [] then "No agent templates available."
  else
    let total = List.length all in
    let total_pages = max 1 ((total + agents_per_page - 1) / agents_per_page) in
    let page = max 1 (min page total_pages) in
    let start_idx = (page - 1) * agents_per_page in
    let page_items =
      List.filteri
        (fun i _ -> i >= start_idx && i < start_idx + agents_per_page)
        all
    in
    let lines =
      List.map
        (fun (t : Agent_template.t) ->
          Format_adapter.code connector (Printf.sprintf "/agent %s" t.name)
          ^ " — " ^ t.description)
        page_items
    in
    let header =
      if total_pages > 1 then
        Format_adapter.bold connector
          (Printf.sprintf "Agent Templates (%d/%d)" page total_pages)
      else Format_adapter.bold connector "Agent Templates"
    in
    let nav =
      if total_pages <= 1 then ""
      else
        let prev =
          if page > 1 then
            Format_adapter.code connector
              (Printf.sprintf "/agent menu %d" (page - 1))
            ^ " prev"
          else ""
        in
        let next =
          if page < total_pages then
            Format_adapter.code connector
              (Printf.sprintf "/agent menu %d" (page + 1))
            ^ " next"
          else ""
        in
        let parts = List.filter (fun s -> s <> "") [ prev; next ] in
        "\n\n" ^ String.concat "  |  " parts
    in
    header ^ "\n\n" ^ String.concat "\n" lines ^ "\n\nUsage: "
    ^ Format_adapter.code connector "/agent <name> <prompt>"
    ^ nav

let items_per_menu_page = 9

let paginate_items items page =
  let total = List.length items in
  let total_pages =
    max 1 ((total + items_per_menu_page - 1) / items_per_menu_page)
  in
  let page = max 1 (min page total_pages) in
  let start_idx = (page - 1) * items_per_menu_page in
  let page_items =
    List.filteri
      (fun i _ -> i >= start_idx && i < start_idx + items_per_menu_page)
      items
  in
  (page_items, page, total_pages)

let pagination_footer ~connector ~cmd page total_pages =
  if total_pages <= 1 then ""
  else
    let prev =
      if page > 1 then
        Format_adapter.code connector (Printf.sprintf "%s %d" cmd (page - 1))
        ^ " << "
      else ""
    in
    let next =
      if page < total_pages then
        " >> "
        ^ Format_adapter.code connector (Printf.sprintf "%s %d" cmd (page + 1))
      else ""
    in
    Printf.sprintf "\n\nPage %d/%d  %s%s" page total_pages prev next

let format_model_menu ~connector ~page =
  let prefs = Model_preferences.load () in
  let favs = prefs.favorites in
  if favs = [] then
    "No favorite models. Use "
    ^ Format_adapter.code connector "/model fav <name>"
    ^ " to add favorites, then "
    ^ Format_adapter.code connector "/model menu"
    ^ " to select from them."
  else
    let page_favs, page, total_pages = paginate_items favs page in
    let lines =
      List.map
        (fun m ->
          Format_adapter.code connector (Printf.sprintf "/model set %s" m))
        page_favs
    in
    Format_adapter.bold connector "Model Selection"
    ^ "\n\n" ^ String.concat "\n" lines
    ^ pagination_footer ~connector ~cmd:"/model menu" page total_pages

let format_thinking_menu ~connector =
  let levels = allowed_thinking_levels in
  let lines =
    List.map
      (fun level ->
        Format_adapter.code connector (Printf.sprintf "/thinking %s" level))
      levels
  in
  Format_adapter.bold connector "Thinking Level"
  ^ "\n\n" ^ String.concat "\n" lines

let format_config_menu ~connector ~page =
  let sections = Config_set.top_level_section_names () in
  let page_sections, page, total_pages = paginate_items sections page in
  let lines =
    List.map
      (fun s ->
        Format_adapter.code connector (Printf.sprintf "/config show %s" s))
      page_sections
  in
  Format_adapter.bold connector "Config Sections"
  ^ "\n\n" ^ String.concat "\n" lines
  ^ pagination_footer ~connector ~cmd:"/config menu" page total_pages

let format_skills_menu ~connector ~page ?(show_test = false) () =
  let skills =
    Skills.filter_visible_skills ~show_test (Skills.available_skills ())
  in
  if skills = [] then "No skills available."
  else
    let page_skills, page, total_pages = paginate_items skills page in
    let lines =
      List.map
        (fun (s : Skills.skill_md_meta) ->
          Format_adapter.code connector (Printf.sprintf "/%s" s.md_name)
          ^ " — " ^ s.md_description)
        page_skills
    in
    Format_adapter.bold connector "Skills"
    ^ "\n\n" ^ String.concat "\n" lines
    ^ pagination_footer ~connector ~cmd:"/skills" page total_pages

let format_costs_menu ~connector =
  let items =
    [
      ("/costs", "Cost summary by time period");
      ("/costs session", "Cost breakdown across sessions");
      ("/costs model", "Cost breakdown by model");
      ("/costs provider", "Cost breakdown by provider");
    ]
  in
  let lines =
    List.map
      (fun (cmd, desc) -> Format_adapter.code connector cmd ^ " — " ^ desc)
      items
  in
  Format_adapter.bold connector "Cost Views" ^ "\n\n" ^ String.concat "\n" lines

let format_bg_menu ~connector =
  let items =
    [
      ("/bg list", "List all background tasks");
      ("/bg create <prompt>", "Create a new background task");
    ]
  in
  let lines =
    List.map
      (fun (cmd, desc) -> Format_adapter.code connector cmd ^ " — " ^ desc)
      items
  in
  Format_adapter.bold connector "Background Tasks"
  ^ "\n\n" ^ String.concat "\n" lines

let format_agent_not_found ~connector name =
  let all = Agent_template.available_templates () in
  let names =
    List.map (fun (t : Agent_template.t) -> t.name) all
    |> List.sort String.compare
  in
  "Agent template "
  ^ Format_adapter.code connector (Printf.sprintf "'%s'" name)
  ^ " not found."
  ^
  if names = [] then ""
  else
    "\nAvailable: "
    ^ String.concat ", " (List.map (Format_adapter.code connector) names)

let format_fork_and_usage ~connector =
  "Usage: "
  ^ Format_adapter.code connector "/fork_and [@agent] <prompt>"
  ^ "\n\nOptionally prefix with "
  ^ Format_adapter.code connector "@agent_name"
  ^ " to apply that agent's tool restrictions."

let format_tasks_usage ~connector =
  "Usage: " ^ Format_adapter.code connector "/tasks" ^ " [full]"

let format_menu_usage ~connector =
  "Usage: " ^ Format_adapter.code connector "/menu" ^ " [page]"

let format_config_help ~connector =
  Format_adapter.bold connector "Config"
  ^ "\nUsage: "
  ^ Format_adapter.code connector "/config"
  ^ " <subcommand>\n\nSubcommands:\n  "
  ^ Format_adapter.code connector "show [SECTION]"
  ^ "  \xe2\x80\x94 Show config (or a specific section)\n  "
  ^ Format_adapter.code connector "get KEY"
  ^ "         \xe2\x80\x94 Get a config value by dot-path\n  "
  ^ Format_adapter.code connector "set KEY VALUE"
  ^ "   \xe2\x80\x94 Set a config value\n  "
  ^ Format_adapter.code connector "keys [PREFIX]"
  ^ "   \xe2\x80\x94 List valid config key paths\n  "
  ^ Format_adapter.code connector "wizard"
  ^ "          \xe2\x80\x94 Run the interactive setup wizard"

let format_config_show ~connector output =
  Format_adapter.code_block connector output

let format_config_keys ~connector paths =
  Format_adapter.code_block connector (String.concat "\n" paths)

let format_config_set_confirm ~connector key value =
  "Set "
  ^ Format_adapter.bold connector key
  ^ " = "
  ^ Format_adapter.code connector value
  ^ "\nNote: restart the daemon for changes to take effect."

let format_config_error ~connector msg =
  Format_adapter.bold connector "Error"
  ^ ": "
  ^ Format_adapter.escape connector msg

let format_config_unknown_subcommand ~connector sub =
  "Unknown config subcommand "
  ^ Format_adapter.code connector (Printf.sprintf "'%s'" sub)
  ^ ".\nUse "
  ^ Format_adapter.code connector "/config"
  ^ " for usage help."

let format_bg_usage ~connector =
  "Usage: "
  ^ Format_adapter.code connector "/bg"
  ^ " [subcommand] [id]\n  "
  ^ Format_adapter.code connector "/bg"
  ^ "                    - List all background tasks\n  "
  ^ Format_adapter.code connector "/bg list"
  ^ "               - List all background tasks\n  "
  ^ Format_adapter.code connector "/bg show <id>"
  ^ "          - Show details for a task\n  "
  ^ Format_adapter.code connector "/bg logs <id>"
  ^ "          - Show recent log output for a task\n  "
  ^ Format_adapter.code connector "/bg cancel <id>"
  ^ "        - Cancel a running or queued task\n  "
  ^ Format_adapter.code connector "/bg retry <id>"
  ^ "         - Retry a failed task\n  "
  ^ Format_adapter.code connector "/bg create [@agent] <prompt>"
  ^ " - Create a new background task"

let format_bg_invalid_id ~connector id_str =
  "Invalid task id "
  ^ Format_adapter.code connector (Printf.sprintf "'%s'" id_str)
  ^ ". Expected an integer.\n" ^ format_bg_usage ~connector

let format_model_usage_text ~connector =
  "Usage: "
  ^ Format_adapter.code connector "/model"
  ^ " [set/set-default/fav/unfav/list/usage] [args]\n  "
  ^ Format_adapter.code connector "/model"
  ^ "                    \xe2\x80\x94 Show current model and favorites\n  "
  ^ Format_adapter.code connector "/model set <name>"
  ^ "         \xe2\x80\x94 Set model for this session\n  "
  ^ Format_adapter.code connector "/model set-default <name>"
  ^ " \xe2\x80\x94 Set default model in config (persistent)\n  "
  ^ Format_adapter.code connector "/model fav <name>"
  ^ "         \xe2\x80\x94 Toggle favorite status\n  "
  ^ Format_adapter.code connector "/model unfav <name>"
  ^ "       \xe2\x80\x94 Remove from favorites\n  "
  ^ Format_adapter.code connector "/model list [provider]"
  ^ "    \xe2\x80\x94 List available models\n  "
  ^ Format_adapter.code connector "/model usage"
  ^ "              \xe2\x80\x94 Show provider quota/usage"

let format_costs_usage ~connector =
  "Usage: "
  ^ Format_adapter.code connector "/costs"
  ^ " [session [KEY]/model/provider]\n  "
  ^ Format_adapter.code connector "/costs"
  ^ "                 - Cost summary by time period\n  "
  ^ Format_adapter.code connector "/costs session"
  ^ "         - Cost breakdown across sessions\n  "
  ^ Format_adapter.code connector "/costs session <key>"
  ^ "   - Cost breakdown for one session\n  "
  ^ Format_adapter.code connector "/costs model"
  ^ "           - Cost breakdown by model\n  "
  ^ Format_adapter.code connector "/costs provider"
  ^ "        - Cost breakdown by provider"

let format_usage_usage ~connector =
  "Usage: "
  ^ Format_adapter.code connector "/usage"
  ^ " [session [KEY]/model/provider]\n  "
  ^ Format_adapter.code connector "/usage"
  ^ "                 - Usage summary by time period\n  "
  ^ Format_adapter.code connector "/usage session"
  ^ "         - Usage breakdown across sessions\n  "
  ^ Format_adapter.code connector "/usage session <key>"
  ^ "   - Usage breakdown for one session\n  "
  ^ Format_adapter.code connector "/usage model"
  ^ "           - Usage breakdown by model\n  "
  ^ Format_adapter.code connector "/usage provider"
  ^ "        - Usage breakdown by provider"

let format_cron_usage ~connector =
  "Usage: "
  ^ Format_adapter.code connector "/cron"
  ^ " [list/show/add/edit/remove/history]\n  "
  ^ Format_adapter.code connector "/cron"
  ^ "                                    \xe2\x80\x94 List all cron jobs\n  "
  ^ Format_adapter.code connector "/cron list"
  ^ "                               \xe2\x80\x94 List all cron jobs\n  "
  ^ Format_adapter.code connector "/cron show <name>"
  ^ "                        \xe2\x80\x94 Show job details\n  "
  ^ Format_adapter.code connector
      "/cron add <name> <schedule> <message> [--ttl <duration>]"
  ^ " \xe2\x80\x94 Create a cron job\n  "
  ^ Format_adapter.code connector
      "/cron edit <name> --schedule <expr> [--ttl <duration>]"
  ^ " \xe2\x80\x94 Edit schedule\n  "
  ^ Format_adapter.code connector
      "/cron edit <name> --message <text> [--ttl <duration>]"
  ^ "  \xe2\x80\x94 Edit message\n  "
  ^ Format_adapter.code connector "/cron remove <name>"
  ^ "                      \xe2\x80\x94 Remove a cron job\n  "
  ^ Format_adapter.code connector "/cron history [name]"
  ^ "                     \xe2\x80\x94 Show recent run history\n\n\
     Schedule formats: cron expression (e.g. "
  ^ Format_adapter.code connector "\"*/5 * * * *\""
  ^ ") or interval (e.g. "
  ^ Format_adapter.code connector "\"every 30m\""
  ^ ")"

let format_bl_usage ~connector =
  "Usage: "
  ^ Format_adapter.code connector "/bl"
  ^ " [subcommand]\n  "
  ^ Format_adapter.code connector "/bl"
  ^ "             - List backlog overview\n  "
  ^ Format_adapter.code connector "/bl list"
  ^ "        - List backlog overview\n  "
  ^ Format_adapter.code connector "/bl bugs"
  ^ "        - List bugs only\n  "
  ^ Format_adapter.code connector "/bl ideas"
  ^ "       - List ideas only\n  "
  ^ Format_adapter.code connector "/bl show <id>"
  ^ "   - Show task details\n  "
  ^ Format_adapter.code connector "/bl <id>"
  ^ "        - Show task details"

let format_reset ~connector ~active_bg_tasks =
  Format_adapter.bold connector "Session reset."
  ^ " Send a new message to start fresh."
  ^
  if active_bg_tasks > 0 then
    Printf.sprintf "\nNote: %d active background task(s) are still running."
      active_bg_tasks
  else ""

let format_compact_result ~connector ~removed ~kept =
  Format_adapter.bold connector "Compacted"
  ^ Printf.sprintf ": removed %d messages, kept %d." removed kept

let format_uptime ~connector text =
  Format_adapter.bold connector "Uptime" ^ ": " ^ text

let format_tasks ~connector text =
  match connector with
  | Format_adapter.Plain -> text
  | _ -> Format_adapter.code_block connector text

let format_thinking_status ~connector current =
  "Current thinking level: "
  ^ Format_adapter.bold connector (thinking_level_to_string current)

let format_thinking_set ~connector ~previous level =
  "Thinking level changed from "
  ^ Format_adapter.code connector (thinking_level_to_string previous)
  ^ " to "
  ^ Format_adapter.bold connector (thinking_level_to_string level)
  ^ "."

let format_show_thinking_status ~connector enabled =
  "Show thinking: "
  ^ Format_adapter.bold connector (if enabled then "on" else "off")

let format_show_thinking_toggle ~connector new_val =
  "Show thinking "
  ^ Format_adapter.bold connector (if new_val then "enabled" else "disabled")
  ^ "."

let format_heartbeat_status ~connector text =
  Format_adapter.bold connector "Heartbeat" ^ ": " ^ text

let format_heartbeat_set ~connector enabled key =
  "Heartbeat "
  ^ Format_adapter.bold connector (if enabled then "enabled" else "disabled")
  ^ " for session "
  ^ Format_adapter.code connector key
  ^ "."

let format_cron_confirm ~connector action name =
  Format_adapter.bold connector (String.capitalize_ascii action)
  ^ " cron job "
  ^ Format_adapter.code connector (Printf.sprintf "'%s'" name)
  ^ "."

let format_model_set_confirm ~connector name =
  "Model set to " ^ Format_adapter.code connector name ^ " for this session."

let format_model_set_default_confirm ~connector name =
  "Default model set to " ^ Format_adapter.code connector name ^ "."

let format_model_fav_confirm ~connector name action =
  (match action with `Added -> "Added " | `Removed -> "Removed ")
  ^ Format_adapter.code connector name
  ^
  match action with
  | `Added -> " to favorites."
  | `Removed -> " from favorites."

(* ── Existing format: help ─────────────────────────────────────────────── *)

let format_help_skills_section ~connector (skills : Skills.skill_md_meta list) =
  if skills = [] then ""
  else
    let lines =
      List.map
        (fun (s : Skills.skill_md_meta) ->
          Format_adapter.code connector ("/" ^ s.md_name)
          ^ " \xe2\x80\x94 " ^ s.md_description)
        skills
    in
    "\n\n"
    ^ Format_adapter.bold connector
        (Printf.sprintf "Skills (%d):" (List.length skills))
    ^ "\n" ^ String.concat "\n" lines

let format_help_agents_section ~connector (agents : Agent_template.t list) =
  if agents = [] then ""
  else
    let lines =
      List.map
        (fun (t : Agent_template.t) ->
          Format_adapter.code connector ("@" ^ t.name)
          ^ " \xe2\x80\x94 " ^ t.description)
        agents
    in
    "\n\n"
    ^ Format_adapter.bold connector
        (Printf.sprintf "Agents (%d):" (List.length agents))
    ^ "\n" ^ String.concat "\n" lines

let format_help_with ~connector ~skills ~agents =
  let skills_section = format_help_skills_section ~connector skills in
  let agents_section = format_help_agents_section ~connector agents in
  match connector with
  | Format_adapter.Telegram_html ->
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
           Format_adapter.bold Format_adapter.Telegram_html
             "Available commands:";
           "";
         ]
        @ rows
        @ [
            "";
            Format_adapter.escape Format_adapter.Telegram_html
              "Prefix a message with ! to interrupt the current turn in this \
               session and send the rest as a normal message.";
          ])
      ^ skills_section ^ agents_section
  | Format_adapter.Plain ->
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
            "Prefix a message with ! to interrupt the current turn in this \
             session and send the rest as a normal message.";
          ])
      ^ skills_section ^ agents_section
  | Format_adapter.Teams ->
      let table_columns =
        Table_format.
          [
            { header = "Command"; align = Left; min_width = 0; flex = false };
            { header = "Description"; align = Left; min_width = 0; flex = true };
          ]
      in
      let table_rows =
        List.map (fun c -> [ "/" ^ c.name; c.description ]) commands
      in
      Format_adapter.bold Format_adapter.Teams "Available commands:"
      ^ "\n\n"
      ^ Table_format.render_markdown
          ~escape_cell:(Format_adapter.escape_table_cell Format_adapter.Teams)
          table_columns table_rows
      ^ "\n\n"
      ^ "Prefix a message with ! to interrupt the current turn in this session \
         and send the rest as a normal message." ^ skills_section
      ^ agents_section
  | _ ->
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
      let plain_text =
        String.concat "\n"
          ([ "Available commands:"; "" ]
          @ rows
          @ [
              "";
              "Prefix a message with ! to interrupt the current turn in this \
               session and send the rest as a normal message.";
            ])
      in
      Format_adapter.code_block connector
        (plain_text ^ skills_section ^ agents_section)

let format_help ~connector ?(show_test = false) () =
  let skills =
    Skills.filter_visible_skills ~show_test (Skills.available_skills ())
  in
  let agents = Agent_template.available_templates () in
  format_help_with ~connector ~skills ~agents

(* ── Existing format: tools ────────────────────────────────────────────── *)

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

let format_tools_plain (tools : Tool.t list) (skills : Tool.t list)
    (agents : Agent_template.t list) : string =
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
  if agents <> [] then begin
    Buffer.add_string buf
      (Printf.sprintf "\n\nAgents (%d):\n" (List.length agents));
    List.iter
      (fun (t : Agent_template.t) ->
        Buffer.add_string buf
          (Printf.sprintf "\n@%s\n  %s\n" t.name t.description))
      agents
  end;
  Buffer.contents buf

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

let format_tools_telegram (tools : Tool.t list) (skills : Tool.t list)
    (agents : Agent_template.t list) : string =
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
  if agents <> [] then begin
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Printf.sprintf "<b>Agents (%d)</b>\n\n" (List.length agents));
    Buffer.add_string buf "<blockquote expandable>\n";
    List.iter
      (fun (t : Agent_template.t) ->
        Buffer.add_string buf
          (Printf.sprintf "<b>@%s</b>\n%s\n\n" t.name t.description))
      agents;
    Buffer.add_string buf "</blockquote>"
  end;
  Buffer.contents buf

let format_tools_table ~connector (tools : Tool.t list) (skills : Tool.t list)
    (agents : Agent_template.t list) =
  let sort_tools ts =
    List.sort (fun (a : Tool.t) b -> String.compare a.name b.name) ts
  in
  let columns =
    Table_format.
      [
        { header = "Tool"; align = Left; min_width = 0; flex = false };
        { header = "Risk"; align = Left; min_width = 0; flex = false };
        { header = "Description"; align = Left; min_width = 0; flex = true };
      ]
  in
  let tool_rows =
    List.map
      (fun (t : Tool.t) ->
        [
          t.name;
          risk_level_string t.risk_level;
          truncate_description t.description 60;
        ])
      (sort_tools tools)
  in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf
    (Format_adapter.bold connector
       (Printf.sprintf "Tools (%d)" (List.length tools)));
  Buffer.add_string buf "\n\n";
  Buffer.add_string buf
    (Format_adapter.render_table connector ~max_width:80 columns tool_rows);
  if skills <> [] then begin
    let skill_rows =
      List.map
        (fun (t : Tool.t) ->
          [
            t.name;
            risk_level_string t.risk_level;
            truncate_description t.description 60;
          ])
        (sort_tools skills)
    in
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Format_adapter.bold connector
         (Printf.sprintf "Skills (%d)" (List.length skills)));
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Format_adapter.render_table connector ~max_width:80 columns skill_rows)
  end;
  if agents <> [] then begin
    let agent_columns =
      Table_format.
        [
          { header = "Name"; align = Left; min_width = 0; flex = false };
          { header = "Description"; align = Left; min_width = 0; flex = true };
        ]
    in
    let agent_rows =
      List.map
        (fun (t : Agent_template.t) ->
          [ "@" ^ t.name; truncate_description t.description 60 ])
        agents
    in
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Format_adapter.bold connector
         (Printf.sprintf "Agents (%d)" (List.length agents)));
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Format_adapter.render_table connector ~max_width:80 agent_columns
         agent_rows)
  end;
  Buffer.contents buf

let format_tools ~connector tools skills agents =
  match connector with
  | Format_adapter.Telegram_html -> format_tools_telegram tools skills agents
  | Format_adapter.Plain -> format_tools_plain tools skills agents
  | _ -> format_tools_table ~connector tools skills agents

(* ── Existing format: model show/list ──────────────────────────────────── *)

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

(* ── Existing format: bl ───────────────────────────────────────────────── *)

let run_bl_command args =
  try
    let cmd =
      "bl " ^ String.concat " " args ^ " --json --no-color 2>/dev/null"
    in
    let ic = Unix.open_process_in cmd in
    let buf = Buffer.create 4096 in
    (try
       while true do
         Buffer.add_char buf (input_char ic)
       done
     with End_of_file -> ());
    let _status = Unix.close_process_in ic in
    let output = Buffer.contents buf in
    if String.trim output = "" then Error "No backlog data found."
    else Ok (Yojson.Safe.from_string output)
  with exn ->
    Error (Printf.sprintf "Failed to run bl: %s" (Printexc.to_string exn))

let format_bl_list ~connector json =
  let open Yojson.Safe.Util in
  let buf = Buffer.create 2048 in
  let phases = json |> member "phases" |> to_list in
  let critical_path =
    try json |> member "critical_path" |> to_list |> List.map to_string
    with _ -> []
  in
  if critical_path <> [] then begin
    Buffer.add_string buf
      (Format_adapter.bold connector "Critical Path"
      ^ " "
      ^ String.concat " \xE2\x86\x92 " critical_path
      ^ "\n\n")
  end;
  let phase_columns =
    Table_format.
      [
        { header = "ID"; align = Left; min_width = 4; flex = false };
        { header = "PHASE"; align = Left; min_width = 10; flex = true };
        { header = "DONE"; align = Right; min_width = 4; flex = false };
        { header = "TOTAL"; align = Right; min_width = 5; flex = false };
      ]
  in
  let phase_rows =
    List.map
      (fun phase ->
        let id = phase |> member "id" |> to_string in
        let name = phase |> member "name" |> to_string in
        let stats = phase |> member "stats" in
        let done_count = try stats |> member "done" |> to_int with _ -> 0 in
        let total = try stats |> member "total" |> to_int with _ -> 0 in
        [ id; name; string_of_int done_count; string_of_int total ])
      phases
  in
  if phase_rows <> [] then begin
    Buffer.add_string buf (Format_adapter.bold connector "Phases");
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Format_adapter.render_table connector ~max_width:70 phase_columns
         phase_rows)
  end;
  let bugs = try json |> member "bugs" |> to_list with _ -> [] in
  let open_bugs =
    List.filter
      (fun b ->
        let s = try b |> member "status" |> to_string with _ -> "" in
        s <> "done")
      bugs
  in
  if open_bugs <> [] then begin
    if phase_rows <> [] then Buffer.add_string buf "\n\n";
    let bug_columns =
      Table_format.
        [
          { header = "ID"; align = Left; min_width = 4; flex = false };
          { header = "STATUS"; align = Left; min_width = 6; flex = false };
          { header = "PRI"; align = Left; min_width = 3; flex = false };
          { header = "TITLE"; align = Left; min_width = 10; flex = true };
        ]
    in
    let bug_rows =
      List.map
        (fun b ->
          let id = try b |> member "id" |> to_string with _ -> "?" in
          let status = try b |> member "status" |> to_string with _ -> "?" in
          let priority =
            try b |> member "priority" |> to_string with _ -> ""
          in
          let title = try b |> member "title" |> to_string with _ -> "" in
          let title =
            if String.length title > 60 then String.sub title 0 57 ^ "..."
            else title
          in
          [ id; status; priority; title ])
        open_bugs
    in
    Buffer.add_string buf
      (Format_adapter.bold connector
         (Printf.sprintf "Open Bugs (%d)" (List.length open_bugs)));
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Format_adapter.render_table connector ~max_width:80 bug_columns bug_rows)
  end;
  let ideas = try json |> member "ideas" |> to_list with _ -> [] in
  let open_ideas =
    List.filter
      (fun i ->
        let s = try i |> member "status" |> to_string with _ -> "" in
        s <> "done")
      ideas
  in
  if open_ideas <> [] then begin
    Buffer.add_string buf "\n\n";
    Buffer.add_string buf
      (Format_adapter.bold connector
         (Printf.sprintf "Open Ideas (%d)" (List.length open_ideas)));
    Buffer.add_string buf "\n\n";
    let idea_rows =
      List.map
        (fun i ->
          let id = try i |> member "id" |> to_string with _ -> "?" in
          let status = try i |> member "status" |> to_string with _ -> "?" in
          let title = try i |> member "title" |> to_string with _ -> "" in
          let title =
            if String.length title > 60 then String.sub title 0 57 ^ "..."
            else title
          in
          [ id; status; title ])
        open_ideas
    in
    let idea_columns =
      Table_format.
        [
          { header = "ID"; align = Left; min_width = 4; flex = false };
          { header = "STATUS"; align = Left; min_width = 6; flex = false };
          { header = "TITLE"; align = Left; min_width = 10; flex = true };
        ]
    in
    Buffer.add_string buf
      (Format_adapter.render_table connector ~max_width:80 idea_columns
         idea_rows)
  end;
  Buffer.contents buf

let format_bl_show ~connector id json =
  let open Yojson.Safe.Util in
  let find_task () =
    let search_in items =
      List.find_opt
        (fun item ->
          try item |> member "id" |> to_string = id with _ -> false)
        items
    in
    let bugs = try json |> member "bugs" |> to_list with _ -> [] in
    match search_in bugs with
    | Some t -> Some t
    | None ->
        let ideas = try json |> member "ideas" |> to_list with _ -> [] in
        search_in ideas
  in
  match find_task () with
  | None -> Printf.sprintf "Task '%s' not found in backlog." id
  | Some task ->
      let id = try task |> member "id" |> to_string with _ -> "?" in
      let title = try task |> member "title" |> to_string with _ -> "" in
      let status = try task |> member "status" |> to_string with _ -> "?" in
      let priority =
        try task |> member "priority" |> to_string with _ -> ""
      in
      let complexity =
        try task |> member "complexity" |> to_string with _ -> ""
      in
      let estimate =
        try
          let h = task |> member "estimate_hours" |> to_int in
          Printf.sprintf "%dh" h
        with _ -> ""
      in
      let rows =
        [ [ "ID"; id ]; [ "Title"; title ]; [ "Status"; status ] ]
        @ (if priority <> "" then [ [ "Priority"; priority ] ] else [])
        @ (if complexity <> "" then [ [ "Complexity"; complexity ] ] else [])
        @ if estimate <> "" then [ [ "Estimate"; estimate ] ] else []
      in
      let columns =
        Table_format.
          [
            { header = "FIELD"; align = Left; min_width = 10; flex = false };
            { header = "VALUE"; align = Left; min_width = 20; flex = true };
          ]
      in
      Format_adapter.bold connector (Printf.sprintf "Task %s" id)
      ^ "\n\n"
      ^ Format_adapter.render_table connector ~max_width:60 columns rows

let format_bl_filtered ~connector ~filter_type json =
  let open Yojson.Safe.Util in
  let items = try json |> member filter_type |> to_list with _ -> [] in
  if items = [] then Printf.sprintf "No %s found in backlog." filter_type
  else
    let columns =
      Table_format.
        [
          { header = "ID"; align = Left; min_width = 4; flex = false };
          { header = "STATUS"; align = Left; min_width = 6; flex = false };
          { header = "PRI"; align = Left; min_width = 3; flex = false };
          { header = "TITLE"; align = Left; min_width = 10; flex = true };
        ]
    in
    let rows =
      List.map
        (fun item ->
          let id = try item |> member "id" |> to_string with _ -> "?" in
          let status =
            try item |> member "status" |> to_string with _ -> "?"
          in
          let priority =
            try item |> member "priority" |> to_string with _ -> ""
          in
          let title = try item |> member "title" |> to_string with _ -> "" in
          let title =
            if String.length title > 60 then String.sub title 0 57 ^ "..."
            else title
          in
          [ id; status; priority; title ])
        items
    in
    let label = String.capitalize_ascii filter_type in
    Format_adapter.bold connector
      (Printf.sprintf "%s (%d)" label (List.length items))
    ^ "\n\n"
    ^ Format_adapter.render_table connector ~max_width:80 columns rows

let format_bl ~connector action =
  let json_args =
    match action with
    | BlList -> [ "list" ]
    | BlBugs -> [ "list"; "--bugs" ]
    | BlIdeas -> [ "list"; "--ideas" ]
    | BlShow _ -> [ "list" ]
  in
  match run_bl_command json_args with
  | Error msg -> msg
  | Ok json -> (
      match action with
      | BlList -> format_bl_list ~connector json
      | BlBugs -> format_bl_filtered ~connector ~filter_type:"bugs" json
      | BlIdeas -> format_bl_filtered ~connector ~filter_type:"ideas" json
      | BlShow id -> format_bl_show ~connector id json)

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

(* ── Existing format: cron ─────────────────────────────────────────────── *)

let format_cron ~connector ~db ~session_key action =
  Scheduler.init_schema db;
  match action with
  | CronHelp -> format_cron_usage ~connector
  | CronShow name -> (
      match Scheduler.get_job ~db ~name with
      | None -> Printf.sprintf "No cron job found with name '%s'." name
      | Some (job : Scheduler.job) ->
          ignore session_key;
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
              [ Content_dsl.Separator; Paragraph [ Bold "Recent Runs" ] ]
              @ [
                  Content_dsl.Paragraph
                    [
                      Text
                        (Format_adapter.render_table connector ~max_width:70
                           history_columns history_rows);
                    ];
                ]
          in
          Content_dsl.render_document connector doc)
  | CronList ->
      let jobs = Scheduler.list_jobs ~db in
      if jobs = [] then
        "No cron jobs configured. Use 'clawq cron add' to create one."
      else
        let columns =
          Table_format.
            [
              { header = "NAME"; align = Left; min_width = 4; flex = false };
              { header = "SESSION"; align = Left; min_width = 7; flex = false };
              { header = "SCHEDULE"; align = Left; min_width = 8; flex = false };
              { header = "ENABLED"; align = Left; min_width = 3; flex = false };
              { header = "EXPIRES"; align = Left; min_width = 3; flex = false };
              { header = "MESSAGE"; align = Left; min_width = 10; flex = true };
            ]
        in
        let rows =
          List.map
            (fun (j : Scheduler.job) ->
              let msg_preview =
                if String.length j.message > 40 then
                  String.sub j.message 0 37 ^ "..."
                else j.message
              in
              [
                j.name;
                j.session_key;
                j.schedule_str;
                (if j.enabled then "yes" else "no");
                (match j.expires_at with Some ea -> ea | None -> "-");
                msg_preview;
              ])
            jobs
        in
        Format_adapter.bold connector "Cron Jobs"
        ^ "\n\n"
        ^ Format_adapter.render_table connector ~max_width:80 columns rows
  | CronAdd { name; schedule; message; ttl } -> (
      match
        Scheduler.add_job ~db ~name ~session_key ~message ~schedule ?ttl ()
      with
      | Ok () -> format_cron_confirm ~connector "added" name
      | Error e ->
          Format_adapter.bold connector "Error"
          ^ ": "
          ^ Format_adapter.escape connector e)
  | CronEdit { name; schedule; message; ttl } -> (
      match Scheduler.update_job ~db ~name ?schedule ?message ?ttl () with
      | Ok () -> format_cron_confirm ~connector "updated" name
      | Error e ->
          Format_adapter.bold connector "Error"
          ^ ": "
          ^ Format_adapter.escape connector e
      | exception Invalid_argument e ->
          Format_adapter.bold connector "Error"
          ^ ": "
          ^ Format_adapter.escape connector e)
  | CronRemove name ->
      if Scheduler.remove_job ~db ~name then
        format_cron_confirm ~connector "removed" name
      else
        "No job found with name "
        ^ Format_adapter.code connector (Printf.sprintf "'%s'" name)
        ^ "."
  | CronHistory job_name ->
      let runs =
        match job_name with
        | Some name -> Scheduler.get_history ~db ~name ~limit:10
        | None -> Scheduler.list_runs ~db ~limit:20 ()
      in
      if runs = [] then
        match job_name with
        | Some name -> Printf.sprintf "No run history for '%s'." name
        | None -> "No run history."
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
              let preview =
                match r.result_preview with
                | Some p when String.length p > 40 -> String.sub p 0 37 ^ "..."
                | Some p -> p
                | None -> ""
              in
              [
                string_of_int r.run_id;
                r.job_name;
                r.started_at;
                r.status;
                preview;
              ])
            runs
        in
        let title =
          match job_name with
          | Some name -> Printf.sprintf "Run History \xe2\x80\x94 %s" name
          | None -> "Run History"
        in
        Format_adapter.bold connector title
        ^ "\n\n"
        ^ Format_adapter.render_table connector ~max_width:80 columns rows

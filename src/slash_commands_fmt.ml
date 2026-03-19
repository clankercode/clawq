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
  | ModelSetForce of string
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
  | BgFinalize of int

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
  | CronTrigger of string
  | CronHelp

include Slash_commands_bl_fmt

type session_action =
  | SessionList
  | SessionShow of string
  | SessionArchives of string option
  | SessionArchiveShow of int

type rig_action =
  | RigInstall of string
  | RigAdjust of string
  | RigRemove of string
  | RigList

type repo_action =
  | RepoStatus
  | RepoAssociate of string
  | RepoForget
  | RepoUpdate

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
  | Session of session_action
  | Rig of rig_action
  | Repo of repo_action
  | HeldItems of held_items_action
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
  | Debate of string
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
        "Background tasks: /bg [list/show/logs/cancel/retry/create/finalize] \
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
      name = "repo";
      description = "Manage repo: /repo [url/path/forget/update]";
      priority = 52;
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
      name = "session";
      description = "List/inspect sessions (admin): /session [list/show] [key]";
      priority = 23;
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
    {
      name = "debate";
      description = "Multi-model debate: /debate <prompt>";
      priority = 5;
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
      ( "/bg finalize <id>",
        "Finalize a worktree task (rebase + merge + cleanup)" );
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
  ^ "\nChanges applied."

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
  ^ " - Create a new background task\n  "
  ^ Format_adapter.code connector "/bg finalize <id>"
  ^ "       - Finalize a worktree task (rebase + merge + cleanup)"

let format_bg_invalid_id ~connector id_str =
  "Invalid task id "
  ^ Format_adapter.code connector (Printf.sprintf "'%s'" id_str)
  ^ ". Expected an integer.\n" ^ format_bg_usage ~connector

let format_model_usage_text ~connector =
  "Usage: "
  ^ Format_adapter.code connector "/model"
  ^ " [set/set-force/set-default/fav/unfav/list/usage] [args]\n  "
  ^ Format_adapter.code connector "/model"
  ^ "                    \xe2\x80\x94 Show current model and favorites\n  "
  ^ Format_adapter.code connector "/model set <name>"
  ^ "         \xe2\x80\x94 Set model for this session\n  "
  ^ Format_adapter.code connector "/model set-force <name>"
  ^ "   \xe2\x80\x94 Set model (bypass validation)\n  "
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
  ^ Format_adapter.code connector "/cron trigger <name>"
  ^ "                     \xe2\x80\x94 Trigger a job immediately\n  "
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

let format_session_usage ~connector =
  "Usage: "
  ^ Format_adapter.code connector "/session"
  ^ " [subcommand]\n  "
  ^ Format_adapter.code connector "/session"
  ^ "            - List all sessions\n  "
  ^ Format_adapter.code connector "/session list"
  ^ "       - List all sessions\n  "
  ^ Format_adapter.code connector "/session show <key>"
  ^ " - Show session details\n  "
  ^ Format_adapter.code connector "/session archives [key]"
  ^ " - List archives\n  "
  ^ Format_adapter.code connector "/session archive show <id>"
  ^ " - Show archive messages"

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
          Format_adapter.list_item connector
            (Format_adapter.code connector ("/" ^ s.md_name)
            ^ " \xe2\x80\x94 " ^ s.md_description))
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
          Format_adapter.list_item connector
            (Format_adapter.code connector ("@" ^ t.name)
            ^ " \xe2\x80\x94 " ^ t.description))
        agents
    in
    "\n\n"
    ^ Format_adapter.bold connector
        (Printf.sprintf "Agents (%d):" (List.length agents))
    ^ "\n" ^ String.concat "\n" lines

let format_help_with ~connector ~commands ~skills ~agents =
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
  format_help_with ~connector ~commands ~skills ~agents

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
  | CronTrigger name -> (
      Background_task.init_schema db;
      match Scheduler.trigger_job ~db ~name with
      | Ok task_id ->
          format_cron_confirm ~connector "triggered" name
          ^ " Enqueued as background task "
          ^ Format_adapter.code connector (string_of_int task_id)
          ^ "."
      | Error e ->
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

let topics =
  [
    ("tools", "Built-in tools and when to inspect them.");
    ("channels", "Telegram, Discord, Slack, Teams, web, and gateway use.");
    ("config", "Config files, live reload, and the config wizard.");
    ("skills", "SKILL.md discovery, loading, and slash/@ invocation.");
    ("agents", "Agent templates, bindings, and room-agent profiles.");
    ("memory", "Persistent memory, scoped room memory, and history search.");
    ("background-tasks", "Delegated work, logs, transcripts, and finalize.");
    ("subagents", "Native/local subagents backed by background tasks.");
    ("security", "Workspace policy, sandboxing, egress, audit, and secrets.");
    ("models", "Provider:model selection, catalog, validation, and overrides.");
    ("web-ui", "Browser chat UI and local gateway usage.");
    ("crons", "Scheduled session messages and routine jobs.");
    ("git-worktrees", "Isolated worktrees for background/delegated work.");
    ("onboarding", "First-time setup, quickstart, doctor, and setup wizards.");
  ]

let topic_names = "general" :: List.map fst topics
let topics_line () = String.concat ", " topic_names

let normalize_topic raw =
  raw |> String.trim |> String.lowercase_ascii
  |> String.map (function '_' | ' ' -> '-' | c -> c)

let general_help () =
  Printf.sprintf
    "Clawq Help: general\n\n\
     Clawq is a local AI assistant runtime with built-in tools, memory, \
     channels, configurable model providers, background delegation, native \
     subagents, and a web UI.\n\n\
     Start here:\n\
     - Quickstart: https://clawq.org/quickstart\n\
     - Full self-knowledge reference: https://clawq.org/llms-full.txt\n\n\
     Available topics: %s\n\n\
     Call clawq_help with {\"topic\":\"tools\"} or another topic for focused \
     help."
    (topics_line ())

let help_for_topic = function
  | "tools" ->
      Some
        "Clawq Help: tools\n\n\
         Built-in tools cover shell/file I/O, HTTP/browser fetches, git, \
         messaging, docs, models, memory, background work, subagents, and \
         more. Availability depends on config and runtime wiring. Use `/tools` \
         in chat to inspect the live registry, risk levels, schemas, skills, \
         and agents. Reference: https://clawq.org/tools"
  | "channels" ->
      Some
        "Clawq Help: channels\n\n\
         Channels connect Clawq to Telegram, Discord, Slack, Teams, web chat, \
         email, Matrix, IRC, and other integrations. Use `clawq channel` for \
         configured channels and `clawq setup <channel>` for guided setup. \
         Channel-specific models can override the global default. Reference: \
         https://clawq.org/channels"
  | "config" ->
      Some
        "Clawq Help: config\n\n\
         Config lives in `~/.clawq/config.json` unless `CLAWQ_HOME` is set. \
         Use `clawq config wizard` for the full TUI, `config \
         show/get/set/tree` for direct edits, and `clawq doctor` to check \
         common issues. Live daemon reload handles most config changes. \
         Reference: https://clawq.org/configuration"
  | "skills" ->
      Some
        "Clawq Help: skills\n\n\
         Skills are SKILL.md folders discovered from workspace, personal, and \
         Clawq skill paths. Use `clawq skills list`, `/help skills`, \
         `/skills`, @mentions, or the `use_skill` tool to load them. \
         No-argument skills are de-duplicated across retained context. \
         Reference: https://clawq.org/skills"
  | "agents" ->
      Some
        "Clawq Help: agents\n\n\
         Agent templates define role, goal, model, tool allow/deny lists, and \
         routing metadata. Use `clawq agents list/show/create/edit/bind` or \
         `/help agents` to inspect and route work. Room-agent profiles add \
         connector-aware policy and budget controls. Reference: \
         https://clawq.org/cli-reference"
  | "memory" ->
      Some
        "Clawq Help: memory\n\n\
         Memory includes persistent key-value memories, scoped room memories, \
         history search, summaries, and archive-on-forget safety. Use memory \
         tools for durable facts and `history_search` for past conversation \
         context. Reference: https://clawq.org/configuration"
  | "background-tasks" ->
      Some
        "Clawq Help: background-tasks\n\n\
         Background tasks delegate repo work to Codex, Claude, Kimi, Gemini, \
         Opencode, Cursor, or local runners. Use `clawq background \
         add/list/show/wait/logs/transcript/message/cancel/retry/recover/finalize`. \
         Logs live under `~/.clawq/background-logs/`. Reference: \
         https://clawq.org/background-tasks"
  | "subagents" ->
      Some
        "Clawq Help: subagents\n\n\
         Native subagents are local background tasks optimized for \
         parent-agent delegation. Use the `subagent` and `subagent_result` \
         tools from chat, or `clawq subagents start/list/send/transcript/stop` \
         from the CLI. They share background task storage and transcripts. \
         Reference: https://clawq.org/background-tasks"
  | "security" ->
      Some
        "Clawq Help: security\n\n\
         Security centers on workspace-only path checks, shell allowlists, \
         optional sandbox backends, egress policy, audit logging, secret \
         encryption, room policy, and tool risk levels. Start with `clawq \
         config show security` and `clawq audit`. Reference: \
         https://clawq.org/security"
  | "models" ->
      Some
        "Clawq Help: models\n\n\
         Use canonical `provider:model` names such as `openai:gpt-5.4`. \
         Inspect with `clawq models list`, `/model list`, or the `models` \
         tool. Session and channel overrides can differ from the global \
         default; set actions validate before switching unless explicitly \
         forced. Reference: https://clawq.org/llms-full.txt"
  | "web-ui" ->
      Some
        "Clawq Help: web-ui\n\n\
         The web UI is the browser chat surface served by the local Clawq \
         gateway. Start the daemon with `clawq agent`, check URLs with `clawq \
         status`, and use the web channel for slash commands, tools, and live \
         sessions. Reference: https://clawq.org/quickstart"
  | "crons" ->
      Some
        "Clawq Help: crons\n\n\
         Cron jobs schedule messages into sessions. Use `clawq cron \
         list/show/add/remove/trigger/history`; schedules accept intervals \
         like `every 5m` or standard 5-field cron syntax. Room routines expose \
         similar scheduled behavior for room-agent profiles. Reference: \
         https://clawq.org/cli-reference"
  | "git-worktrees" ->
      Some
        "Clawq Help: git-worktrees\n\n\
         Worktree-backed background tasks isolate branch edits under \
         `~/.clawq/background-worktrees/`. Successful tasks can rebase, \
         fast-forward merge, and clean up automatically; conflicts or dirty \
         worktrees require `background finalize` after repair. Reference: \
         https://clawq.org/background-tasks"
  | "onboarding" ->
      Some
        "Clawq Help: onboarding\n\n\
         For a new install, follow https://clawq.org/quickstart, then run \
         `clawq onboard` or `clawq config wizard`. Use `clawq setup <feature>` \
         for providers/channels, `clawq doctor` for checks, and \
         https://clawq.org/llms-full.txt for the complete self-reference."
  | _ -> None

let read_topic_arg args =
  match args with
  | `Assoc fields -> (
      match List.assoc_opt "topic" fields with
      | None | Some `Null -> Ok "general"
      | Some (`String raw) ->
          let topic = normalize_topic raw in
          if topic = "" then Ok "general" else Ok topic
      | Some _ ->
          Error
            "Error: parameter \"topic\" must be a string. Omit it for general \
             help, or use a topic like {\"topic\":\"tools\"}.")
  | _ ->
      Error
        "Error: clawq_help expects a JSON object. Omit arguments for general \
         help, or use {\"topic\":\"tools\"}."

let unknown_topic topic =
  Printf.sprintf
    "Error: unknown clawq_help topic \"%s\". Available topics: %s. Use \
     {\"topic\":\"general\"} to list topics."
    topic (topics_line ())

let tool =
  {
    Tool.name = "clawq_help";
    description =
      "Get concise built-in help for Clawq. Omit topic or use topic=general to \
       list available topics. Topics include tools, channels, config, skills, \
       agents, memory, background-tasks, subagents, security, models, web-ui, \
       crons, git-worktrees, and onboarding.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "topic",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional help topic. Use general, tools, channels, \
                           config, skills, agents, memory, background-tasks, \
                           subagents, security, models, web-ui, crons, \
                           git-worktrees, or onboarding." );
                      ("enum", `List (List.map (fun t -> `String t) topic_names));
                    ] );
              ] );
          ("required", `List []);
        ];
    invoke =
      (fun ?context:_ args ->
        match read_topic_arg args with
        | Error msg -> Lwt.return msg
        | Ok "general" -> Lwt.return (general_help ())
        | Ok topic -> (
            match help_for_topic topic with
            | Some help -> Lwt.return help
            | None -> Lwt.return (unknown_topic topic)));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

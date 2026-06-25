include Slash_commands_fmt
include Slash_commands_stats_fmt

let handle ?(skill_names = []) text =
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
        | "start" -> FormattedReply (fun connector -> format_start ~connector)
        | "help" -> Help
        | "new" -> Reset
        | "compact" -> Compact
        | "runtime-ctx" | "runtime_ctx" -> RuntimeCtx
        | "uptime" -> Uptime
        | "status" -> Status
        | "thinking" -> (
            match args with
            | [] -> Thinking ShowThinking
            | [ "menu" ] -> ThinkingMenu
            | [ value ] -> (
                match parse_thinking_level value with
                | Some level -> Thinking (SetThinking level)
                | None ->
                    FormattedReply
                      (fun connector ->
                        format_invalid_thinking_level ~connector value))
            | _ ->
                FormattedReply
                  (fun connector -> format_thinking_usage ~connector))
        | "show-thinking" | "show_thinking" | "toggle-show-thinking" -> (
            match args with
            | [] -> ShowThinking ToggleShowThinking
            | [ s ] when String.lowercase_ascii s = "status" ->
                ShowThinking ShowThinkingStatus
            | _ ->
                FormattedReply
                  (fun connector -> format_show_thinking_usage ~connector))
        | "heartbeat" -> (
            match args with
            | [] | [ "status" ] -> Heartbeat HeartbeatStatus
            | [ "on" ] -> Heartbeat (SetHeartbeat true)
            | [ "off" ] -> Heartbeat (SetHeartbeat false)
            | _ ->
                FormattedReply
                  (fun connector -> format_heartbeat_usage ~connector))
        | "debug" -> (
            match args with
            | [] | [ "status" ] -> Debug DebugStatus
            | [ "on" ] -> Debug (SetDebug true)
            | [ "off" ] -> Debug (SetDebug false)
            | _ ->
                FormattedReply (fun connector -> format_debug_usage ~connector))
        | "agent" -> (
            match args with
            | [] ->
                FormattedReply (fun connector -> format_agent_usage ~connector)
            | [ "list" ] ->
                FormattedReply (fun connector -> format_agent_list ~connector)
            | [ "menu" ] -> AgentMenu 1
            | [ "menu"; n ] -> (
                match int_of_string_opt n with
                | Some page when page >= 1 -> AgentMenu page
                | _ ->
                    FormattedReply
                      (fun connector ->
                        "Usage: "
                        ^ Format_adapter.code connector "/agent menu [page]"))
            | name :: rest ->
                let prompt = String.concat " " rest in
                if prompt = "" then
                  FormattedReply
                    (fun connector -> format_agent_usage ~connector)
                else AgentInvoke (name, prompt))
        | "delegate" -> (
            match args with
            | [] ->
                FormattedReply
                  (fun connector -> format_delegate_usage ~connector)
            | first :: rest when String.length first > 1 && first.[0] = '@' ->
                let name = String.sub first 1 (String.length first - 1) in
                let prompt = String.concat " " rest in
                if prompt = "" then
                  FormattedReply
                    (fun connector -> format_delegate_usage ~connector)
                else Delegate (Some name, prompt)
            | _ -> Delegate (None, String.concat " " args))
        | "config" ->
            AdminRequired
              (match args with
              | [] | [ "help" ] ->
                  FormattedReply
                    (fun connector -> format_config_help ~connector)
              | [ "menu" ] -> ConfigMenu 1
              | [ "menu"; n ] -> (
                  match int_of_string_opt n with
                  | Some page when page >= 1 -> ConfigMenu page
                  | _ -> ConfigMenu 1)
              | [ "show" ] ->
                  let output = Config_show.show None in
                  if String.length output > 1500 then
                    let sections =
                      match
                        try Some (Yojson.Safe.from_string output)
                        with _ -> None
                      with
                      | Some (`Assoc fields) ->
                          List.map fst fields |> String.concat ", "
                      | _ -> "(unable to list sections)"
                    in
                    FormattedReply
                      (fun connector ->
                        "Config is too large to display in chat. Available \
                         sections:\n"
                        ^ Format_adapter.code connector sections
                        ^ "\n\nUse: "
                        ^ Format_adapter.code connector "/config show <section>"
                        ^ "\nExample: "
                        ^ Format_adapter.code connector "/config show gateway")
                  else
                    FormattedReply
                      (fun connector -> format_config_show ~connector output)
              | [ "show"; section ] ->
                  FormattedReply
                    (fun connector ->
                      format_config_show ~connector
                        (Config_show.show (Some section)))
              | [ "tree" ] ->
                  let output = Config_tree.render_current () in
                  if String.length output > 1500 then
                    FormattedReply
                      (fun connector ->
                        "Config tree is too large to display in chat.\nUse: "
                        ^ Format_adapter.code connector "/config tree <section>"
                        ^ "\nExample: "
                        ^ Format_adapter.code connector "/config tree gateway")
                  else
                    FormattedReply
                      (fun connector -> format_config_tree ~connector output)
              | [ "tree"; "keys" ] ->
                  FormattedReply
                    (fun connector ->
                      format_config_tree ~connector
                        (Config_tree.render_current ~show_values:false ()))
              | [ "tree"; section ] ->
                  FormattedReply
                    (fun connector ->
                      format_config_tree ~connector
                        (Config_tree.render_current ~section ()))
              | [ "get" ] ->
                  FormattedReply
                    (fun connector ->
                      "Usage: "
                      ^ Format_adapter.code connector "/config get KEY")
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
                    FormattedReply
                      (fun connector ->
                        "Cannot set secret key "
                        ^ Format_adapter.code connector
                            (Printf.sprintf "'%s'" key)
                        ^ " via chat. Use the terminal: "
                        ^ Format_adapter.code connector
                            (Printf.sprintf "clawq config set %s <value>" key))
                  else
                    let value = String.concat " " value_parts in
                    let result = Config_set.set_value key value in
                    FormattedReply
                      (fun connector -> Format_adapter.escape connector result)
              | [ "set" ] | [ "set"; _ ] ->
                  FormattedReply
                    (fun connector ->
                      "Usage: "
                      ^ Format_adapter.code connector "/config set KEY VALUE")
              | [ "keys" ] ->
                  let paths = Config_set.config_leaf_paths () in
                  FormattedReply
                    (fun connector -> format_config_keys ~connector paths)
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
                    FormattedReply
                      (fun connector ->
                        "No config keys matching prefix "
                        ^ Format_adapter.code connector
                            (Printf.sprintf "'%s'" prefix)
                        ^ ".")
                  else
                    FormattedReply
                      (fun connector -> format_config_keys ~connector matches)
              | [ "wizard" ] ->
                  FormattedReply
                    (fun connector ->
                      "The config wizard requires an interactive terminal.\n\
                       Run: "
                      ^ Format_adapter.code connector "clawq config wizard")
              | sub :: _ ->
                  FormattedReply
                    (fun connector ->
                      format_config_unknown_subcommand ~connector sub))
        | "fork-and" | "fork_and" -> (
            match args with
            | [] ->
                FormattedReply
                  (fun connector -> format_fork_and_usage ~connector)
            | first :: rest when String.length first > 1 && first.[0] = '@' ->
                let name = String.sub first 1 (String.length first - 1) in
                let prompt = String.concat " " rest in
                if prompt = "" then
                  FormattedReply
                    (fun connector -> format_fork_and_usage ~connector)
                else ForkAnd (Some name, prompt)
            | _ -> ForkAnd (None, String.concat " " args))
        | "tools" -> Tools
        | "tasks" -> (
            match args with
            | [ "full" ] -> TasksFull
            | [] -> Tasks
            | _ ->
                FormattedReply (fun connector -> format_tasks_usage ~connector))
        | "costs" -> (
            match args with
            | [] -> Costs CostsSummary
            | [ "menu" ] -> CostsMenu
            | [ "session" ] -> Costs CostsSessions
            | [ "session"; key ] -> Costs (CostsSession key)
            | [ "model" ] -> Costs CostsModel
            | [ "provider" ] -> Costs CostsProvider
            | _ ->
                FormattedReply (fun connector -> format_costs_usage ~connector))
        | "active" -> Active
        | "bg" | "background" -> (
            match args with
            | [] | [ "list" ] -> Bg BgList
            | [ "menu" ] -> BgMenu
            | [ "cancel" ] | [ "stop" ] ->
                FormattedReply
                  (fun connector ->
                    "Missing task id. Usage: "
                    ^ Format_adapter.code connector "/bg cancel <id>"
                    ^ "\nUse "
                    ^ Format_adapter.code connector "/bg list"
                    ^ " to see task ids.")
            | [ "show"; id_str ] | [ id_str ] -> (
                match int_of_string_opt id_str with
                | Some id -> Bg (BgShow id)
                | None ->
                    FormattedReply
                      (fun connector -> format_bg_invalid_id ~connector id_str))
            | [ "logs"; id_str ] | [ "log"; id_str ] -> (
                match int_of_string_opt id_str with
                | Some id -> Bg (BgLogs id)
                | None ->
                    FormattedReply
                      (fun connector -> format_bg_invalid_id ~connector id_str))
            | [ "cancel"; id_str ] | [ "stop"; id_str ] -> (
                match int_of_string_opt id_str with
                | Some id -> Bg (BgCancel id)
                | None ->
                    FormattedReply
                      (fun connector -> format_bg_invalid_id ~connector id_str))
            | [ "retry"; id_str ] -> (
                match int_of_string_opt id_str with
                | Some id -> Bg (BgRetry id)
                | None ->
                    FormattedReply
                      (fun connector -> format_bg_invalid_id ~connector id_str))
            | [ "finalize"; id_str ] -> (
                match int_of_string_opt id_str with
                | Some id -> Bg (BgFinalize id)
                | None ->
                    FormattedReply
                      (fun connector -> format_bg_invalid_id ~connector id_str))
            | "create" :: rest | "start" :: rest | "new" :: rest -> (
                match rest with
                | first :: remaining
                  when String.length first > 1 && first.[0] = '@' ->
                    let name = String.sub first 1 (String.length first - 1) in
                    let prompt = String.concat " " remaining in
                    if String.trim prompt = "" then
                      FormattedReply
                        (fun connector ->
                          "Usage: "
                          ^ Format_adapter.code connector
                              "/bg create [@agent] <prompt>"
                          ^ "\nProvide a prompt describing the task to run.")
                    else Bg (BgCreate (Some name, prompt))
                | _ -> (
                    let prompt = String.concat " " rest in
                    match String.trim prompt with
                    | "" ->
                        FormattedReply
                          (fun connector ->
                            "Usage: "
                            ^ Format_adapter.code connector
                                "/bg create [@agent] <prompt>"
                            ^ "\nProvide a prompt describing the task to run.")
                    | prompt -> Bg (BgCreate (None, prompt))))
            | _ -> FormattedReply (fun connector -> format_bg_usage ~connector))
        | "cron" -> (
            let extract_ttl tokens =
              let rec aux acc = function
                | "--ttl" :: v :: rest -> (Some v, List.rev_append acc rest)
                | x :: rest -> aux (x :: acc) rest
                | [] -> (None, List.rev acc)
              in
              aux [] tokens
            in
            let parse_cron_add name rest =
              let ttl, rest = extract_ttl rest in
              match rest with
              | [] ->
                  FormattedReply
                    (fun connector ->
                      "Usage: "
                      ^ Format_adapter.code connector
                          "/cron add <name> <schedule> <message>")
              | w1 :: w2 :: remainder when String.lowercase_ascii w1 = "every"
                ->
                  let schedule = w1 ^ " " ^ w2 in
                  if remainder = [] then
                    FormattedReply
                      (fun connector ->
                        "Usage: "
                        ^ Format_adapter.code connector
                            "/cron add <name> <schedule> <message>")
                  else
                    Cron
                      (CronAdd
                         {
                           name;
                           schedule;
                           message = String.concat " " remainder;
                           ttl;
                         })
              | f1 :: f2 :: f3 :: f4 :: f5 :: remainder when remainder <> [] ->
                  let schedule = String.concat " " [ f1; f2; f3; f4; f5 ] in
                  Cron
                    (CronAdd
                       {
                         name;
                         schedule;
                         message = String.concat " " remainder;
                         ttl;
                       })
              | _ ->
                  FormattedReply
                    (fun connector ->
                      "Usage: "
                      ^ Format_adapter.code connector
                          "/cron add <name> <schedule> <message>")
            in
            match args with
            | [] | [ "list" ] -> Cron CronList
            | "add" :: name :: rest -> parse_cron_add name rest
            | [ "remove"; name ] | [ "rm"; name ] | [ "delete"; name ] ->
                Cron (CronRemove name)
            | "edit" :: name :: rest -> (
                let parse_edit_flags tokens =
                  let ttl, tokens = extract_ttl tokens in
                  let rec aux schedule message = function
                    | "--schedule" :: w1 :: w2 :: rest
                      when String.lowercase_ascii w1 = "every" ->
                        aux (Some (w1 ^ " " ^ w2)) message rest
                    | "--schedule" :: f1 :: f2 :: f3 :: f4 :: f5 :: rest ->
                        aux
                          (Some (String.concat " " [ f1; f2; f3; f4; f5 ]))
                          message rest
                    | "--message" :: m_parts ->
                        let m = String.concat " " m_parts in
                        let msg = if m = "" then None else Some m in
                        (schedule, msg, ttl)
                    | _ :: rest -> aux schedule message rest
                    | [] -> (schedule, message, ttl)
                  in
                  aux None None tokens
                in
                match parse_edit_flags rest with
                | None, None, None ->
                    FormattedReply
                      (fun connector ->
                        "Usage: "
                        ^ Format_adapter.code connector
                            "/cron edit <name> --schedule <expr>"
                        ^ " and/or "
                        ^ Format_adapter.code connector
                            "--message <text> [--ttl <duration>]")
                | schedule, message, ttl ->
                    Cron (CronEdit { name; schedule; message; ttl }))
            | [ "show"; name ] -> Cron (CronShow name)
            | [ "trigger"; name ] | [ "run"; name ] -> Cron (CronTrigger name)
            | [ "history" ] | [ "runs" ] -> Cron (CronHistory None)
            | [ "history"; name ] | [ "runs"; name ] ->
                Cron (CronHistory (Some name))
            | [ "help" ] -> Cron CronHelp
            | _ -> Cron CronHelp)
        | "usage" -> (
            match args with
            | [] -> Usage UsageSummary
            | [ "session" ] -> Usage UsageSessions
            | [ "session"; key ] -> Usage (UsageSession key)
            | [ "model" ] -> Usage UsageModel
            | [ "provider" ] -> Usage UsageProvider
            | _ ->
                FormattedReply (fun connector -> format_usage_usage ~connector))
        | "model" -> (
            let known_subcommands =
              [
                "set";
                "set-force";
                "fav";
                "unfav";
                "list";
                "usage";
                "menu";
                "help";
              ]
            in
            match args with
            | [] -> Model ModelShow
            | [ "help" ] ->
                FormattedReply
                  (fun connector -> format_model_usage_text ~connector)
            | [ "menu" ] -> ModelMenu 1
            | [ "menu"; n ] -> (
                match int_of_string_opt n with
                | Some page when page >= 1 -> ModelMenu page
                | _ -> ModelMenu 1)
            (* Resolve bare aliases like "kimi" -> "kimi_coding:kimi-for-coding"
               at parse time so all downstream handlers get the canonical name
               without each needing to call resolve_alias themselves. *)
            | [ "set"; name ] ->
                Model (ModelSet (Models_catalog.resolve_alias_or_name name))
            | [ "set-force"; name ] ->
                Model
                  (ModelSetForce (Models_catalog.resolve_alias_or_name name))
            | [ "set-default"; name ] ->
                Model
                  (ModelSetDefault (Models_catalog.resolve_alias_or_name name))
            | [ "fav"; name ] ->
                Model (ModelFav (Models_catalog.resolve_alias_or_name name))
            | [ "unfav"; name ] ->
                Model (ModelUnfav (Models_catalog.resolve_alias_or_name name))
            | "list" :: rest ->
                let parse_availability value =
                  Models_catalog.availability_filter_of_string value
                in
                let is_flag value =
                  String.length value > 0 && value.[0] = '-'
                in
                let rec parse provider availability = function
                  | [] -> Ok (provider, availability)
                  | "--provider" :: [] -> Error "--provider"
                  | "--provider" :: p :: _ when is_flag p -> Error p
                  | "--provider" :: p :: tail ->
                      parse (Some p) availability tail
                  | "--availability" :: [] -> Error "--availability"
                  | "--availability" :: value :: tail -> (
                      match parse_availability value with
                      | Some availability -> parse provider availability tail
                      | None -> Error value)
                  | ("--available" | "available") :: tail ->
                      parse provider Models_catalog.Available tail
                  | ("--unavailable" | "unavailable") :: tail ->
                      parse provider Models_catalog.Unavailable tail
                  | ("--all" | "all") :: tail ->
                      parse provider Models_catalog.All tail
                  | value :: _ when is_flag value -> Error value
                  | value :: tail -> parse (Some value) availability tail
                in
                begin match parse None Models_catalog.Available rest with
                | Ok (provider, availability) ->
                    Model (ModelList (provider, availability))
                | Error _ ->
                    FormattedReply
                      (fun connector -> format_model_usage_text ~connector)
                end
            | [ "usage" ] -> Model ModelUsage
            | first :: _
              when not
                     (List.mem (String.lowercase_ascii first) known_subcommands)
              ->
                Model
                  (ModelSet
                     (Models_catalog.resolve_alias_or_name
                        (String.concat " " args)))
            | _ ->
                FormattedReply
                  (fun connector -> format_model_usage_text ~connector))
        | "bl" | "backlog" -> (
            match args with
            | [] | [ "list" ] -> Bl BlList
            | [ "bugs" ] -> Bl BlBugs
            | [ "ideas" ] -> Bl BlIdeas
            | [ "show"; id ] -> Bl (BlShow id)
            | [ id ] -> Bl (BlShow id)
            | _ -> FormattedReply (fun connector -> format_bl_usage ~connector))
        | "session" | "sessions" ->
            AdminRequired
              (match args with
              | [] | [ "list" ] -> Session SessionList
              | [ "show"; key ] -> Session (SessionShow key)
              | [ "archives" ] -> Session (SessionArchives None)
              | [ "archives"; key ] -> Session (SessionArchives (Some key))
              | [ "archive"; "show"; id ] | [ "archives"; "show"; id ] -> (
                  match int_of_string_opt id with
                  | Some n -> Session (SessionArchiveShow n)
                  | None ->
                      Reply
                        "Error: archive ID must be an integer. Use /session \
                         archives to list archive IDs.")
              | _ ->
                  FormattedReply
                    (fun connector -> format_session_usage ~connector))
        | "rig" | "rigging" -> (
            match args with
            | [ "install"; name ] | [ "add"; name ] -> Rig (RigInstall name)
            | [ "adjust"; name ] | [ "modify"; name ] -> Rig (RigAdjust name)
            | [ "remove"; name ] | [ "uninstall"; name ] -> Rig (RigRemove name)
            | [] | [ "list" ] -> Rig RigList
            | _ ->
                Reply "Usage: /rig install|adjust|remove <name>, or /rig list")
        | "repo" -> (
            match args with
            | [] -> Repo RepoStatus
            | [ "forget" ] -> Repo RepoForget
            | [ "update" ] | [ "pull" ] | [ "fetch" ] -> Repo RepoUpdate
            | _ -> Repo (RepoAssociate (String.concat " " args)))
        | "held-items" | "held_items" | "helditems" -> (
            match args with
            | [] | [ "list" ] -> HeldItems (HeldItemsList false)
            | [ "list"; "--all" ] -> HeldItems (HeldItemsList true)
            | [ "view"; id ] | [ "show"; id ] -> (
                match int_of_string_opt id with
                | Some n -> HeldItems (HeldItemsShow n)
                | None ->
                    Reply "Usage: /held-items view <id> — id must be numeric.")
            | [ "approve"; id ] -> (
                match int_of_string_opt id with
                | Some n -> AdminRequired (HeldItems (HeldItemsApprove n))
                | None ->
                    Reply
                      "Usage: /held-items approve <id> — id must be numeric.")
            | "reject" :: id :: reason -> (
                match int_of_string_opt id with
                | Some n ->
                    let notes =
                      match reason with
                      | [] -> None
                      | _ -> Some (String.concat " " reason)
                    in
                    AdminRequired (HeldItems (HeldItemsReject (n, notes)))
                | None ->
                    Reply
                      "Usage: /held-items reject <id> [reason] — id must be \
                       numeric.")
            | _ ->
                FormattedReply
                  (fun connector -> format_held_items_usage ~connector))
        | "menu" -> (
            match args with
            | [] -> Menu 1
            | [ n ] -> (
                match int_of_string_opt n with
                | Some page when page >= 1 -> Menu page
                | _ ->
                    FormattedReply
                      (fun connector -> format_menu_usage ~connector))
            | _ ->
                FormattedReply (fun connector -> format_menu_usage ~connector))
        | "skills" -> (
            match args with
            | [] -> SkillsMenu 1
            | [ n ] -> (
                match int_of_string_opt n with
                | Some page when page >= 1 -> SkillsMenu page
                | _ -> SkillsMenu 1)
            | _ -> SkillsMenu 1)
        | "inject_connector_history" | "inject-connector-history" -> (
            match args with
            | [] -> InjectConnectorHistory 20
            | [ n ] ->
                let count =
                  match int_of_string_opt n with
                  | Some c -> max 1 (min 128 c)
                  | None -> 20
                in
                InjectConnectorHistory count
            | _ -> InjectConnectorHistory 20)
        | "debate" ->
            let prompt = String.concat " " args in
            if prompt = "" then
              FormattedReply
                (fun connector ->
                  "Usage: " ^ Format_adapter.code connector "/debate <prompt>")
            else Debate prompt
        | "debug-dump-chat" | "debug_dump_chat" -> AdminRequired DebugDumpChat
        | "bash" ->
            AdminRequired
              (match args with
              | [] ->
                  FormattedReply
                    (fun connector ->
                      "Usage: "
                      ^ Format_adapter.code connector "/bash <command>")
              | _ ->
                  let prefix_len =
                    match String.index_opt trimmed ' ' with
                    | Some i -> i + 1
                    | None -> String.length trimmed
                  in
                  let cmd_text =
                    if prefix_len < String.length trimmed then
                      String.sub trimmed prefix_len
                        (String.length trimmed - prefix_len)
                    else String.concat " " args
                  in
                  BashRun cmd_text)
        | "register_as_admin_otc" | "register-as-admin-otc" -> (
            match args with
            | [] -> RegisterAsAdminOtc None
            | [ code ] -> RegisterAsAdminOtc (Some code)
            | _ ->
                FormattedReply
                  (fun connector ->
                    "Usage: "
                    ^ Format_adapter.code connector
                        "/register_as_admin_otc [CODE]"))
        | "" -> NotACommand
        | _ -> (
            match
              List.find_opt
                (fun sn -> String.lowercase_ascii sn = cmd_lower)
                skill_names
            with
            | Some original_name ->
                SkillInvoke (original_name, String.concat " " args)
            | None -> NotACommand))

let is_admin_command name =
  match handle ("/" ^ name) with AdminRequired _ -> true | _ -> false

let visible_commands ~is_admin =
  if is_admin then commands
  else List.filter (fun c -> not (is_admin_command c.name)) commands

let sorted_by_priority ?(is_admin = true) () =
  List.sort
    (fun a b -> compare b.priority a.priority)
    (visible_commands ~is_admin)

let format_help ~connector ?(show_test = false) ~is_admin () =
  let cmds = visible_commands ~is_admin in
  let skills =
    Skills.filter_visible_skills ~show_test (Skills.available_skills ())
  in
  let agents = Agent_template.available_templates () in
  format_help_with ~connector ~commands:cmds ~skills ~agents

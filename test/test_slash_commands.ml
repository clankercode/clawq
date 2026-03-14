let result_to_string = function
  | Slash_commands.Reply s -> "Reply(" ^ s ^ ")"
  | Slash_commands.Help -> "Help"
  | Slash_commands.Reset -> "Reset"
  | Slash_commands.Thinking Slash_commands.ShowThinking -> "Thinking(Show)"
  | Slash_commands.Thinking (Slash_commands.SetThinking None) ->
      "Thinking(Set off)"
  | Slash_commands.Thinking (Slash_commands.SetThinking (Some level)) ->
      "Thinking(Set " ^ level ^ ")"
  | Slash_commands.Compact -> "Compact"
  | Slash_commands.RuntimeCtx -> "RuntimeCtx"
  | Slash_commands.Uptime -> "Uptime"
  | Slash_commands.ShowThinking Slash_commands.ShowThinkingStatus ->
      "ShowThinking(Status)"
  | Slash_commands.ShowThinking Slash_commands.ToggleShowThinking ->
      "ShowThinking(Toggle)"
  | Slash_commands.Heartbeat Slash_commands.HeartbeatStatus ->
      "Heartbeat(Status)"
  | Slash_commands.Heartbeat (Slash_commands.SetHeartbeat true) ->
      "Heartbeat(On)"
  | Slash_commands.Heartbeat (Slash_commands.SetHeartbeat false) ->
      "Heartbeat(Off)"
  | Slash_commands.Delegate s -> "Delegate(" ^ s ^ ")"
  | Slash_commands.ForkAnd s -> "ForkAnd(" ^ s ^ ")"
  | Slash_commands.Tools -> "Tools"
  | Slash_commands.Tasks -> "Tasks"
  | Slash_commands.Costs Slash_commands.CostsSummary -> "Costs(Summary)"
  | Slash_commands.Costs Slash_commands.CostsSessions -> "Costs(Sessions)"
  | Slash_commands.Costs (Slash_commands.CostsSession key) ->
      "Costs(Session " ^ key ^ ")"
  | Slash_commands.Costs Slash_commands.CostsModel -> "Costs(Model)"
  | Slash_commands.Costs Slash_commands.CostsProvider -> "Costs(Provider)"
  | Slash_commands.Usage Slash_commands.UsageSummary -> "Usage(Summary)"
  | Slash_commands.Usage Slash_commands.UsageSessions -> "Usage(Sessions)"
  | Slash_commands.Usage (Slash_commands.UsageSession key) ->
      "Usage(Session " ^ key ^ ")"
  | Slash_commands.Usage Slash_commands.UsageModel -> "Usage(Model)"
  | Slash_commands.Usage Slash_commands.UsageProvider -> "Usage(Provider)"
  | Slash_commands.Model Slash_commands.ModelShow -> "Model(Show)"
  | Slash_commands.Model (Slash_commands.ModelSet name) ->
      "Model(Set " ^ name ^ ")"
  | Slash_commands.Model (Slash_commands.ModelFav name) ->
      "Model(Fav " ^ name ^ ")"
  | Slash_commands.Model (Slash_commands.ModelUnfav name) ->
      "Model(Unfav " ^ name ^ ")"
  | Slash_commands.Model (Slash_commands.ModelList None) -> "Model(List)"
  | Slash_commands.Model (Slash_commands.ModelList (Some p)) ->
      "Model(List " ^ p ^ ")"
  | Slash_commands.Model Slash_commands.ModelUsage -> "Model(Usage)"
  | Slash_commands.Model (Slash_commands.ModelSetDefault name) ->
      "Model(SetDefault " ^ name ^ ")"
  | Slash_commands.Status -> "Status"
  | Slash_commands.Menu page -> "Menu(" ^ string_of_int page ^ ")"
  | Slash_commands.DebugDumpChat -> "DebugDumpChat"
  | Slash_commands.NotACommand -> "NotACommand"

let result_eq a b =
  match (a, b) with
  | Slash_commands.Reply a, Slash_commands.Reply b -> a = b
  | Slash_commands.Help, Slash_commands.Help -> true
  | Slash_commands.Reset, Slash_commands.Reset -> true
  | ( Slash_commands.Thinking Slash_commands.ShowThinking,
      Slash_commands.Thinking Slash_commands.ShowThinking ) ->
      true
  | ( Slash_commands.Thinking (Slash_commands.SetThinking a),
      Slash_commands.Thinking (Slash_commands.SetThinking b) ) ->
      a = b
  | Slash_commands.Compact, Slash_commands.Compact -> true
  | Slash_commands.RuntimeCtx, Slash_commands.RuntimeCtx -> true
  | Slash_commands.Uptime, Slash_commands.Uptime -> true
  | Slash_commands.Status, Slash_commands.Status -> true
  | Slash_commands.ShowThinking a, Slash_commands.ShowThinking b -> a = b
  | Slash_commands.Heartbeat a, Slash_commands.Heartbeat b -> a = b
  | Slash_commands.Delegate a, Slash_commands.Delegate b -> a = b
  | Slash_commands.ForkAnd a, Slash_commands.ForkAnd b -> a = b
  | Slash_commands.Tools, Slash_commands.Tools -> true
  | Slash_commands.Tasks, Slash_commands.Tasks -> true
  | Slash_commands.Costs a, Slash_commands.Costs b -> a = b
  | Slash_commands.Usage a, Slash_commands.Usage b -> a = b
  | Slash_commands.Model _, Slash_commands.Model _ -> true
  | Slash_commands.Menu a, Slash_commands.Menu b -> a = b
  | Slash_commands.DebugDumpChat, Slash_commands.DebugDumpChat -> true
  | Slash_commands.NotACommand, Slash_commands.NotACommand -> true
  | _ -> false

let result_testable =
  Alcotest.testable
    (fun fmt r -> Format.fprintf fmt "%s" (result_to_string r))
    result_eq

let test_start () =
  match Slash_commands.handle "/start" with
  | Slash_commands.Reply s ->
      Alcotest.(check bool) "contains ready" true (String.length s > 0)
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_help () =
  match Slash_commands.handle "/help" with
  | Slash_commands.Help ->
      let s = Slash_commands.format_help ~connector:Format_adapter.Plain in
      let contains =
        try
          ignore (Str.search_forward (Str.regexp_string "/help") s 0);
          true
        with Not_found -> false
      in
      Alcotest.(check bool) "contains /help" true contains;
      let contains_markdown_table =
        try
          ignore
            (Str.search_forward
               (Str.regexp_string "| Command | Description |")
               s 0);
          true
        with Not_found -> false
      in
      let contains_plain_start =
        try
          ignore
            (Str.search_forward
               (Str.regexp_string "Available commands:\n\n  /help")
               s 0);
          true
        with Not_found -> false
      in
      Alcotest.(check bool)
        "does not use markdown table" false contains_markdown_table;
      Alcotest.(check bool) "contains plain help line" true contains_plain_start
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Help, got %s" (result_to_string other))

let test_new () =
  Alcotest.check result_testable "reset" Slash_commands.Reset
    (Slash_commands.handle "/new")

let test_status () =
  Alcotest.check result_testable "status" Slash_commands.Status
    (Slash_commands.handle "/status")

let test_compact () =
  Alcotest.check result_testable "compact" Slash_commands.Compact
    (Slash_commands.handle "/compact")

let test_runtime_ctx () =
  Alcotest.check result_testable "runtime-ctx" Slash_commands.RuntimeCtx
    (Slash_commands.handle "/runtime-ctx");
  Alcotest.check result_testable "runtime_ctx" Slash_commands.RuntimeCtx
    (Slash_commands.handle "/runtime_ctx")

let test_uptime () =
  Alcotest.check result_testable "uptime" Slash_commands.Uptime
    (Slash_commands.handle "/uptime")

let test_unknown_command () =
  Alcotest.check result_testable "unknown cmd" Slash_commands.NotACommand
    (Slash_commands.handle "/foo")

let test_thinking_show () =
  Alcotest.check result_testable "thinking show"
    (Slash_commands.Thinking Slash_commands.ShowThinking)
    (Slash_commands.handle "/thinking")

let test_thinking_set_levels () =
  let cases =
    [
      ("low", Some "low");
      ("medium", Some "medium");
      ("high", Some "high");
      ("off", None);
      ("xhigh", Some "xhigh");
      ("max", Some "max");
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.check result_testable ("thinking " ^ input)
        (Slash_commands.Thinking (Slash_commands.SetThinking expected))
        (Slash_commands.handle ("/thinking " ^ input)))
    cases

let test_thinking_case_insensitive () =
  Alcotest.check result_testable "thinking HIGH"
    (Slash_commands.Thinking (Slash_commands.SetThinking (Some "high")))
    (Slash_commands.handle "/thinking HIGH")

let test_thinking_invalid_level () =
  match Slash_commands.handle "/thinking turbo" with
  | Slash_commands.Reply text ->
      let contains =
        try
          ignore
            (Str.search_forward
               (Str.regexp_string "Invalid thinking level")
               text 0);
          true
        with Not_found -> false
      in
      Alcotest.(check bool) "mentions invalid thinking level" true contains
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_thinking_too_many_args () =
  match Slash_commands.handle "/thinking low extra" with
  | Slash_commands.Reply text ->
      Alcotest.(check string)
        "usage" "Usage: /thinking [low/medium/high/off/xhigh/max]" text
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_heartbeat_status () =
  Alcotest.check result_testable "heartbeat status"
    (Slash_commands.Heartbeat Slash_commands.HeartbeatStatus)
    (Slash_commands.handle "/heartbeat")

let test_heartbeat_toggle () =
  Alcotest.check result_testable "heartbeat on"
    (Slash_commands.Heartbeat (Slash_commands.SetHeartbeat true))
    (Slash_commands.handle "/heartbeat on");
  Alcotest.check result_testable "heartbeat off"
    (Slash_commands.Heartbeat (Slash_commands.SetHeartbeat false))
    (Slash_commands.handle "/heartbeat off")

let test_heartbeat_invalid_args () =
  Alcotest.check result_testable "heartbeat invalid args"
    (Slash_commands.Reply "Usage: /heartbeat [on/off/status]")
    (Slash_commands.handle "/heartbeat maybe")

let test_regular_message () =
  Alcotest.check result_testable "regular msg" Slash_commands.NotACommand
    (Slash_commands.handle "hello world")

let test_empty_message () =
  Alcotest.check result_testable "empty msg" Slash_commands.NotACommand
    (Slash_commands.handle "")

let test_commands_list () =
  let names =
    List.map
      (fun (c : Slash_commands.command) -> c.name)
      Slash_commands.commands
  in
  Alcotest.(check bool) "has start" true (List.mem "start" names);
  Alcotest.(check bool) "has help" true (List.mem "help" names);
  Alcotest.(check bool) "has new" true (List.mem "new" names);
  Alcotest.(check bool) "has status" true (List.mem "status" names);
  Alcotest.(check bool) "has thinking" true (List.mem "thinking" names);
  Alcotest.(check bool) "has compact" true (List.mem "compact" names);
  Alcotest.(check bool) "has runtime_ctx" true (List.mem "runtime_ctx" names);
  Alcotest.(check bool) "has uptime" true (List.mem "uptime" names);
  Alcotest.(check bool) "has update" true (List.mem "update" names);
  Alcotest.(check bool) "has delegate" true (List.mem "delegate" names);
  Alcotest.(check bool)
    "has show_thinking" true
    (List.mem "show_thinking" names);
  Alcotest.(check bool) "has config" true (List.mem "config" names);
  Alcotest.(check bool) "has heartbeat" true (List.mem "heartbeat" names);
  Alcotest.(check bool) "has fork_and" true (List.mem "fork_and" names);
  Alcotest.(check bool) "has tools" true (List.mem "tools" names);
  Alcotest.(check bool) "has tasks" true (List.mem "tasks" names);
  Alcotest.(check bool) "has costs" true (List.mem "costs" names);
  Alcotest.(check bool) "has usage" true (List.mem "usage" names)

let test_case_insensitive () =
  (match Slash_commands.handle "/HELP" with
  | Slash_commands.Help -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Help for /HELP, got %s"
           (result_to_string other)));
  Alcotest.check result_testable "reset from /NEW" Slash_commands.Reset
    (Slash_commands.handle "/NEW")

let test_bare_slash () =
  Alcotest.check result_testable "bare slash" Slash_commands.NotACommand
    (Slash_commands.handle "/")

let test_command_with_args () =
  match Slash_commands.handle "/help extra args here" with
  | Slash_commands.Help -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Help for /help with args, got %s"
           (result_to_string other))

let test_format_help_telegram () =
  let output =
    Slash_commands.format_help ~connector:Format_adapter.Telegram_html
  in
  let contains needle =
    try
      ignore (Str.search_forward (Str.regexp_string needle) output 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "telegram uses bold heading" true
    (contains "<b>Available commands:</b>");
  Alcotest.(check bool)
    "telegram uses code formatting" true
    (contains "<code>/start</code>")

let test_whitespace_only () =
  Alcotest.check result_testable "whitespace only" Slash_commands.NotACommand
    (Slash_commands.handle "   ")

let test_delegate_with_prompt () =
  Alcotest.check result_testable "delegate with prompt"
    (Slash_commands.Delegate "do something")
    (Slash_commands.handle "/delegate do something")

let test_delegate_no_args () =
  Alcotest.check result_testable "delegate no args"
    (Slash_commands.Reply "Usage: /delegate <prompt>")
    (Slash_commands.handle "/delegate")

let test_delegate_multi_word () =
  Alcotest.check result_testable "delegate multi-word"
    (Slash_commands.Delegate "a b c d")
    (Slash_commands.handle "/delegate a b c d")

let test_fork_and_with_prompt () =
  Alcotest.check result_testable "fork-and with prompt"
    (Slash_commands.ForkAnd "summarize this")
    (Slash_commands.handle "/fork-and summarize this")

let test_fork_and_no_args () =
  Alcotest.check result_testable "fork-and no args"
    (Slash_commands.Reply "Usage: /fork_and <prompt>")
    (Slash_commands.handle "/fork-and")

let test_fork_and_multi_word () =
  Alcotest.check result_testable "fork-and multi-word"
    (Slash_commands.ForkAnd "a b c d")
    (Slash_commands.handle "/fork-and a b c d")

let test_fork_and_underscore_alias () =
  Alcotest.check result_testable "fork_and underscore alias"
    (Slash_commands.ForkAnd "do something")
    (Slash_commands.handle "/fork_and do something")

let test_costs_default () =
  Alcotest.check result_testable "/costs summary"
    (Slash_commands.Costs Slash_commands.CostsSummary)
    (Slash_commands.handle "/costs")

let test_costs_session () =
  Alcotest.check result_testable "/costs session"
    (Slash_commands.Costs Slash_commands.CostsSessions)
    (Slash_commands.handle "/costs session");
  Alcotest.check result_testable "/costs session key"
    (Slash_commands.Costs (Slash_commands.CostsSession "telegram:1:user"))
    (Slash_commands.handle "/costs session telegram:1:user")

let test_costs_model_and_provider () =
  Alcotest.check result_testable "/costs model"
    (Slash_commands.Costs Slash_commands.CostsModel)
    (Slash_commands.handle "/costs model");
  Alcotest.check result_testable "/costs provider"
    (Slash_commands.Costs Slash_commands.CostsProvider)
    (Slash_commands.handle "/costs provider")

let test_costs_usage_on_invalid_args () =
  match Slash_commands.handle "/costs nope" with
  | Slash_commands.Reply text ->
      let contains =
        try
          ignore (Str.search_forward (Str.regexp_string "Usage:") text 0);
          true
        with Not_found -> false
      in
      Alcotest.(check bool) "mentions usage" true contains
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply(Usage), got %s" (result_to_string other))

let test_usage_default () =
  Alcotest.check result_testable "/usage summary"
    (Slash_commands.Usage Slash_commands.UsageSummary)
    (Slash_commands.handle "/usage")

let test_usage_session () =
  Alcotest.check result_testable "/usage session"
    (Slash_commands.Usage Slash_commands.UsageSessions)
    (Slash_commands.handle "/usage session");
  Alcotest.check result_testable "/usage session key"
    (Slash_commands.Usage (Slash_commands.UsageSession "telegram:1:user"))
    (Slash_commands.handle "/usage session telegram:1:user")

let test_usage_model_and_provider () =
  Alcotest.check result_testable "/usage model"
    (Slash_commands.Usage Slash_commands.UsageModel)
    (Slash_commands.handle "/usage model");
  Alcotest.check result_testable "/usage provider"
    (Slash_commands.Usage Slash_commands.UsageProvider)
    (Slash_commands.handle "/usage provider")

let test_usage_usage_on_invalid_args () =
  match Slash_commands.handle "/usage nope" with
  | Slash_commands.Reply text ->
      let contains =
        try
          ignore (Str.search_forward (Str.regexp_string "Usage:") text 0);
          true
        with Not_found -> false
      in
      Alcotest.(check bool) "mentions usage" true contains
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply(Usage), got %s" (result_to_string other))

let test_leading_whitespace () =
  Alcotest.check result_testable "padded status" Slash_commands.Status
    (Slash_commands.handle "  /status  ")

let contains_str haystack needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with Not_found -> false

let test_show_thinking_toggle () =
  Alcotest.check result_testable "show-thinking toggles"
    (Slash_commands.ShowThinking Slash_commands.ToggleShowThinking)
    (Slash_commands.handle "/show-thinking")

let test_show_thinking_status () =
  Alcotest.check result_testable "show-thinking status"
    (Slash_commands.ShowThinking Slash_commands.ShowThinkingStatus)
    (Slash_commands.handle "/show-thinking status")

let test_show_thinking_aliases () =
  Alcotest.check result_testable "show_thinking alias"
    (Slash_commands.ShowThinking Slash_commands.ToggleShowThinking)
    (Slash_commands.handle "/show_thinking");
  Alcotest.check result_testable "toggle-show-thinking alias"
    (Slash_commands.ShowThinking Slash_commands.ToggleShowThinking)
    (Slash_commands.handle "/toggle-show-thinking")

let test_show_thinking_bad_args () =
  match Slash_commands.handle "/show-thinking foo" with
  | Slash_commands.Reply s ->
      Alcotest.(check bool) "mentions Usage" true (contains_str s "Usage")
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_config_usage () =
  match Slash_commands.handle "/config" with
  | Slash_commands.Reply s ->
      Alcotest.(check bool) "mentions show" true (contains_str s "show")
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_config_show () =
  match Slash_commands.handle "/config show" with
  | Slash_commands.Reply s ->
      Alcotest.(check bool) "non-empty" true (String.length s > 0)
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_config_show_provider_section () =
  Test_helpers.with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      Unix.mkdir clawq_dir 0o755;
      let config_path = Filename.concat clawq_dir "config.json" in
      let config_json =
        `Assoc
          [
            ( "providers",
              `Assoc
                [
                  ( "openai",
                    `Assoc
                      [
                        ("api_key", `String "sk-test");
                        ("base_url", `String "https://api.example.com");
                      ] );
                ] );
          ]
      in
      let oc = open_out config_path in
      output_string oc (Yojson.Safe.pretty_to_string ~std:true config_json);
      close_out oc;
      match Slash_commands.handle "/config show providers.openai" with
      | Slash_commands.Reply s ->
          Alcotest.(check bool)
            "mentions api_key" true (contains_str s "api_key");
          Alcotest.(check bool) "redacts api_key" true (contains_str s "***");
          Alcotest.(check bool)
            "mentions base_url" true
            (contains_str s "base_url")
      | other ->
          Alcotest.fail
            (Printf.sprintf "expected Reply, got %s" (result_to_string other)))

let test_config_get_missing () =
  match Slash_commands.handle "/config get nonexistent.key" with
  | Slash_commands.Reply s ->
      Alcotest.(check bool)
        "contains not found" true
        (contains_str s "not found" || contains_str s "unknown")
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_config_keys () =
  match Slash_commands.handle "/config keys" with
  | Slash_commands.Reply s ->
      Alcotest.(check bool)
        "contains workspace" true
        (contains_str s "workspace")
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_config_keys_prefix () =
  match Slash_commands.handle "/config keys gateway" with
  | Slash_commands.Reply s ->
      Alcotest.(check bool)
        "contains gateway.host" true
        (contains_str s "gateway.host")
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_config_set_secret_blocked () =
  match Slash_commands.handle "/config set channels.discord.bot_token foo" with
  | Slash_commands.Reply s ->
      Alcotest.(check bool)
        "contains cannot" true
        (contains_str s "Cannot" || contains_str s "cannot")
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_config_wizard () =
  match Slash_commands.handle "/config wizard" with
  | Slash_commands.Reply s ->
      Alcotest.(check bool) "mentions terminal" true (contains_str s "terminal")
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_config_unknown_sub () =
  match Slash_commands.handle "/config unknown" with
  | Slash_commands.Reply s ->
      Alcotest.(check bool) "mentions Unknown" true (contains_str s "Unknown")
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_config_leaf_paths () =
  let paths = Config_set.config_leaf_paths () in
  Alcotest.(check bool) "non-empty" true (List.length paths > 0);
  Alcotest.(check bool) "has workspace" true (List.mem "workspace" paths);
  Alcotest.(check bool) "has gateway.host" true (List.mem "gateway.host" paths);
  Alcotest.(check bool)
    "has dynamic placeholder" true
    (List.exists (fun p -> contains_str p "<NAME>") paths)

let test_config_get_no_key () =
  match Slash_commands.handle "/config get" with
  | Slash_commands.Reply s ->
      Alcotest.(check bool)
        "contains usage" true
        (contains_str s "Usage" || contains_str s "KEY")
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_config_set_invalid_path () =
  match Slash_commands.handle "/config set totally.bogus.path value" with
  | Slash_commands.Reply s ->
      Alcotest.(check bool)
        "mentions unknown key" true
        (contains_str s "unknown" || contains_str s "Error")
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_config_set_section_path_rejected () =
  match Slash_commands.handle "/config set providers.openai value" with
  | Slash_commands.Reply s ->
      Alcotest.(check string)
        "section path rejected"
        (Config_set.section_not_settable_error ~show_cmd:"/config show"
           "providers.openai")
        s
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_model_set_colon_format () =
  match Slash_commands.handle "/model set zai_coding:glm-5" with
  | Slash_commands.Model (Slash_commands.ModelSet name) ->
      Alcotest.(check string) "colon name preserved" "zai_coding:glm-5" name
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Model(ModelSet), got %s"
           (result_to_string other))

let test_model_set_slash_format () =
  match Slash_commands.handle "/model set zai_coding/glm-5" with
  | Slash_commands.Model (Slash_commands.ModelSet name) ->
      Alcotest.(check string) "slash name preserved" "zai_coding/glm-5" name
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Model(ModelSet), got %s"
           (result_to_string other))

let test_model_set_default () =
  match
    Slash_commands.handle "/model set-default anthropic:claude-3-5-sonnet"
  with
  | Slash_commands.Model (Slash_commands.ModelSetDefault name) ->
      Alcotest.(check string)
        "set-default name preserved" "anthropic:claude-3-5-sonnet" name
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Model(ModelSetDefault), got %s"
           (result_to_string other))

let test_debug_dump_chat_command () =
  Alcotest.check result_testable "/debug_dump_chat returns DebugDumpChat"
    Slash_commands.DebugDumpChat
    (Slash_commands.handle "/debug_dump_chat");
  Alcotest.check result_testable "/debug-dump-chat alias"
    Slash_commands.DebugDumpChat
    (Slash_commands.handle "/debug-dump-chat")

let test_tools_command () =
  Alcotest.check result_testable "/tools returns Tools" Slash_commands.Tools
    (Slash_commands.handle "/tools")

let test_tasks_command () =
  Alcotest.check result_testable "/tasks returns Tasks" Slash_commands.Tasks
    (Slash_commands.handle "/tasks")

let test_tasks_command_with_telegram_bot_suffix () =
  Alcotest.check result_testable "/tasks@bot returns Tasks" Slash_commands.Tasks
    (Slash_commands.handle "/tasks@clawq_bot")

let test_tasks_render_empty_tree () =
  let db = Sqlite3.db_open ":memory:" in
  Task_tree.init_schema db;
  let session_key = "telegram:123456:789012" in
  let output = Task_tree.render_tree_with_legend ~db ~session_key in
  Alcotest.(check bool)
    "empty tree returns non-empty string" true
    (String.length output > 0);
  Alcotest.(check bool)
    "contains 'No tasks tracked'" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string "No tasks tracked") output 0);
       true
     with Not_found -> false)

let test_tasks_render_nonempty_tree () =
  let db = Sqlite3.db_open ":memory:" in
  Task_tree.init_schema db;
  let session_key = "telegram:123456:789012" in
  let _ =
    Task_tree.process_operations ~db ~session_key
      [ `Assoc [ ("op", `String "add"); ("title", `String "Test task") ] ]
  in
  let output = Task_tree.render_tree_with_legend ~db ~session_key in
  Alcotest.(check bool)
    "non-empty tree returns non-empty string" true
    (String.length output > 0);
  Alcotest.(check bool)
    "contains 'Test task'" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Test task") output 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains Legend" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Legend") output 0);
       true
     with Not_found -> false)

let test_tasks_session_key_isolation () =
  let db = Sqlite3.db_open ":memory:" in
  Task_tree.init_schema db;
  let session1 = "telegram:111:222" in
  let session2 = "telegram:333:444" in
  let _ =
    Task_tree.process_operations ~db ~session_key:session1
      [ `Assoc [ ("op", `String "add"); ("title", `String "Session1 task") ] ]
  in
  let output1 = Task_tree.render_tree_with_legend ~db ~session_key:session1 in
  let output2 = Task_tree.render_tree_with_legend ~db ~session_key:session2 in
  Alcotest.(check bool)
    "session1 has task" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Session1 task") output1 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "session2 has no task" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Session1 task") output2 0);
       false
     with Not_found -> true)

let test_format_tools_plain () =
  let tools =
    [
      {
        Tool.name = "file_read";
        description = "Read file contents with optional offset/limit";
        parameters_schema =
          `Assoc
            [
              ( "properties",
                `Assoc
                  [
                    ("path", `Assoc [ ("type", `String "string") ]);
                    ("offset", `Assoc [ ("type", `String "integer") ]);
                  ] );
              ("required", `List [ `String "path" ]);
            ];
        invoke = (fun ?context:_ _args -> Lwt.return "");
        invoke_stream = None;
        risk_level = Tool.Low;
        deferred = false;
      };
      {
        Tool.name = "shell_exec";
        description = "Execute shell commands in a sandboxed environment";
        parameters_schema =
          `Assoc
            [
              ( "properties",
                `Assoc
                  [
                    ("command", `Assoc [ ("type", `String "string") ]);
                    ("timeout", `Assoc [ ("type", `String "integer") ]);
                  ] );
              ("required", `List [ `String "command" ]);
            ];
        invoke = (fun ?context:_ _args -> Lwt.return "");
        invoke_stream = None;
        risk_level = Tool.High;
        deferred = false;
      };
    ]
  in
  let output =
    Slash_commands.format_tools ~connector:Format_adapter.Plain tools []
  in
  Alcotest.(check bool) "contains count" true (contains_str output "(2)");
  Alcotest.(check bool)
    "contains file_read" true
    (contains_str output "file_read");
  Alcotest.(check bool)
    "contains shell_exec" true
    (contains_str output "shell_exec");
  Alcotest.(check bool) "contains [High]" true (contains_str output "[High]");
  Alcotest.(check bool) "contains [Low]" true (contains_str output "[Low]");
  Alcotest.(check bool)
    "contains required marker" true
    (contains_str output "path*");
  Alcotest.(check bool)
    "file_read before shell_exec (sorted)" true
    (let pos_file =
       try Str.search_forward (Str.regexp_string "file_read") output 0
       with Not_found -> max_int
     in
     let pos_shell =
       try Str.search_forward (Str.regexp_string "shell_exec") output 0
       with Not_found -> max_int
     in
     pos_file < pos_shell);
  Alcotest.(check bool)
    "no Skills section when skills empty" true
    (not (contains_str output "Skills"))

let test_format_tools_telegram () =
  let tools =
    [
      {
        Tool.name = "memory_store";
        description = "Store a key-value pair in session memory";
        parameters_schema =
          `Assoc
            [
              ( "properties",
                `Assoc
                  [
                    ("key", `Assoc [ ("type", `String "string") ]);
                    ("value", `Assoc [ ("type", `String "string") ]);
                  ] );
              ("required", `List [ `String "key"; `String "value" ]);
            ];
        invoke = (fun ?context:_ _args -> Lwt.return "");
        invoke_stream = None;
        risk_level = Tool.Low;
        deferred = false;
      };
    ]
  in
  let output =
    Slash_commands.format_tools ~connector:Format_adapter.Telegram_html tools []
  in
  Alcotest.(check bool) "contains <b>" true (contains_str output "<b>");
  Alcotest.(check bool)
    "contains blockquote" true
    (contains_str output "<blockquote expandable>");
  Alcotest.(check bool)
    "contains tool name" true
    (contains_str output "memory_store");
  Alcotest.(check bool) "contains <code>" true (contains_str output "<code>");
  Alcotest.(check bool)
    "contains required markers" true
    (contains_str output "key* value*");
  Alcotest.(check bool)
    "no Skills section when skills empty" true
    (not (contains_str output "Skills"))

let make_dummy_tool name description =
  {
    Tool.name;
    description;
    parameters_schema = `Assoc [ ("type", `String "object") ];
    invoke = (fun ?context:_ _args -> Lwt.return "");
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

let test_format_tools_telegram_empty () =
  let output =
    Slash_commands.format_tools ~connector:Format_adapter.Telegram_html [] []
  in
  Alcotest.(check bool)
    "contains Tools (0)" true
    (contains_str output "Tools (0)");
  Alcotest.(check bool)
    "no blockquote when empty" true
    (not (contains_str output "<blockquote"))

let test_format_tools_telegram_with_skills () =
  let tools = [ make_dummy_tool "file_read" "Read a file" ] in
  let skills = [ make_dummy_tool "my_script" "Run my script" ] in
  let output =
    Slash_commands.format_tools ~connector:Format_adapter.Telegram_html tools
      skills
  in
  Alcotest.(check bool)
    "has Tools section" true
    (contains_str output "Tools (1)");
  Alcotest.(check bool)
    "has Skills section" true
    (contains_str output "Skills (1)");
  Alcotest.(check bool)
    "tools before skills" true
    (let pos_tools =
       try Str.search_forward (Str.regexp_string "Tools (1)") output 0
       with Not_found -> max_int
     in
     let pos_skills =
       try Str.search_forward (Str.regexp_string "Skills (1)") output 0
       with Not_found -> max_int
     in
     pos_tools < pos_skills);
  Alcotest.(check bool)
    "two blockquote sections" true
    (let count = ref 0 in
     let start = ref 0 in
     (try
        while true do
          let pos =
            Str.search_forward
              (Str.regexp_string "<blockquote expandable>")
              output !start
          in
          incr count;
          start := pos + 1
        done
      with Not_found -> ());
     !count = 2)

let test_format_tools_plain_with_skills () =
  let tools = [ make_dummy_tool "file_read" "Read a file" ] in
  let skills = [ make_dummy_tool "my_script" "Run my script" ] in
  let output =
    Slash_commands.format_tools ~connector:Format_adapter.Plain tools skills
  in
  Alcotest.(check bool)
    "has Tools section" true
    (contains_str output "Tools (1)");
  Alcotest.(check bool)
    "has Skills section" true
    (contains_str output "Skills (1)");
  Alcotest.(check bool) "has file_read" true (contains_str output "file_read");
  Alcotest.(check bool) "has my_script" true (contains_str output "my_script");
  Alcotest.(check bool)
    "tools before skills" true
    (let pos_tools =
       try Str.search_forward (Str.regexp_string "Tools (1)") output 0
       with Not_found -> max_int
     in
     let pos_skills =
       try Str.search_forward (Str.regexp_string "Skills (1)") output 0
       with Not_found -> max_int
     in
     pos_tools < pos_skills)

let with_request_stats_db f =
  Test_helpers.with_memory_db (fun db ->
      Memory.init_request_stats_schema db;
      f db)

let insert_request_stat ~db ~session_key ~provider ~model ~prompt_tokens
    ~completion_tokens ~cost_usd ?(added_prompt_tokens = prompt_tokens) () =
  Request_stats.record ~db ~session_key ~provider ~model ~prompt_tokens
    ~completion_tokens ~cost_usd ~added_prompt_tokens ()

let test_format_costs_plain_and_telegram () =
  with_request_stats_db (fun db ->
      insert_request_stat ~db ~session_key:"telegram:1:user" ~provider:"openai"
        ~model:"gpt-5.4" ~prompt_tokens:1200 ~completion_tokens:300
        ~cost_usd:0.12 ();
      insert_request_stat ~db ~session_key:"discord:chan:user"
        ~provider:"anthropic" ~model:"claude-sonnet-4-6" ~prompt_tokens:800
        ~completion_tokens:200 ~cost_usd:0.25 ();
      let summary =
        Slash_commands.format_costs ~connector:Format_adapter.Plain ~db
          Slash_commands.CostsSummary
      in
      Alcotest.(check bool)
        "plain summary heading" true
        (contains_str summary "Cost Summary");
      Alcotest.(check bool)
        "plain summary includes all time" true
        (contains_str summary "All time");
      Alcotest.(check bool)
        "plain summary has PERIOD header" true
        (contains_str summary "PERIOD");
      Alcotest.(check bool)
        "plain summary has COST header" true
        (contains_str summary "COST");
      let sessions =
        Slash_commands.format_costs ~connector:Format_adapter.Plain ~db
          Slash_commands.CostsSessions
      in
      Alcotest.(check bool)
        "plain sessions heading" true
        (contains_str sessions "Session Costs");
      Alcotest.(check bool)
        "plain sessions include telegram key" true
        (contains_str sessions "telegram:1:user");
      let telegram =
        Slash_commands.format_costs ~connector:Format_adapter.Telegram_html ~db
          Slash_commands.CostsSessions
      in
      Alcotest.(check bool)
        "telegram heading" true
        (contains_str telegram "<b>Session Costs</b>");
      Alcotest.(check bool)
        "telegram uses pre code block" true
        (contains_str telegram "<pre>");
      Alcotest.(check bool)
        "telegram contains session key" true
        (contains_str telegram "telegram:1:user"))

let test_format_usage_plain_and_telegram () =
  with_request_stats_db (fun db ->
      insert_request_stat ~db ~session_key:"telegram:1:user" ~provider:"openai"
        ~model:"gpt-5.4" ~prompt_tokens:1200 ~completion_tokens:300
        ~cost_usd:0.12 ~added_prompt_tokens:900 ();
      insert_request_stat ~db ~session_key:"discord:chan:user"
        ~provider:"anthropic" ~model:"claude-sonnet-4-6" ~prompt_tokens:800
        ~completion_tokens:200 ~cost_usd:0.25 ~added_prompt_tokens:500 ();
      let summary =
        Slash_commands.format_usage ~connector:Format_adapter.Plain ~db
          Slash_commands.UsageSummary
      in
      Alcotest.(check bool)
        "plain usage heading" true
        (contains_str summary "Usage Summary");
      Alcotest.(check bool)
        "plain usage includes all time" true
        (contains_str summary "All time");
      Alcotest.(check bool)
        "plain usage has TURNS header" true
        (contains_str summary "TURNS");
      let sessions =
        Slash_commands.format_usage ~connector:Format_adapter.Plain ~db
          Slash_commands.UsageSessions
      in
      Alcotest.(check bool)
        "plain usage sessions heading" true
        (contains_str sessions "Session Usage");
      Alcotest.(check bool)
        "plain usage sessions include telegram key" true
        (contains_str sessions "telegram:1:user");
      Alcotest.(check bool)
        "plain usage shows added prompt tokens" true
        (contains_str sessions "900");
      let telegram =
        Slash_commands.format_usage ~connector:Format_adapter.Telegram_html ~db
          Slash_commands.UsageSessions
      in
      Alcotest.(check bool)
        "telegram usage heading" true
        (contains_str telegram "<b>Session Usage</b>");
      Alcotest.(check bool)
        "telegram usage uses pre code block" true
        (contains_str telegram "<pre>");
      Alcotest.(check bool)
        "telegram usage contains session key" true
        (contains_str telegram "telegram:1:user"))

let test_format_help_discord_code_block () =
  let output = Slash_commands.format_help ~connector:Format_adapter.Discord in
  Alcotest.(check bool)
    "discord help wrapped in code block" true
    (String.length output > 6
    && String.sub output 0 3 = "```"
    && contains_str output "```\n");
  Alcotest.(check bool)
    "discord help contains /help" true
    (contains_str output "/help")

let test_format_help_slack_code_block () =
  let output = Slash_commands.format_help ~connector:Format_adapter.Slack in
  Alcotest.(check bool)
    "slack help wrapped in code block" true
    (String.length output > 6 && String.sub output 0 3 = "```");
  Alcotest.(check bool)
    "slack help contains /help" true
    (contains_str output "/help")

let test_format_model_usage_empty () =
  let config = Runtime_config.default in
  let result =
    Slash_commands.format_model_usage ~connector:Format_adapter.Discord ~config
      []
  in
  Alcotest.(check string) "empty providers" "No providers configured." result

let test_format_model_usage_table () =
  let config = Runtime_config.default in
  let pq : Provider_quota.provider_quota =
    {
      provider_name = "openai";
      state =
        Provider_quota.Known
          {
            session =
              Some
                { used_pct = 42.0; resets_at = None; window_duration_s = None };
            weekly =
              Some
                { used_pct = 75.0; resets_at = None; window_duration_s = None };
            monthly = None;
          };
      fetched_at = Unix.gettimeofday ();
    }
  in
  let result =
    Slash_commands.format_model_usage ~connector:Format_adapter.Discord ~config
      [ pq ]
  in
  Alcotest.(check bool) "contains code block" true (contains_str result "```");
  Alcotest.(check bool)
    "contains PROVIDER header" true
    (contains_str result "PROVIDER");
  Alcotest.(check bool) "contains openai" true (contains_str result "openai");
  Alcotest.(check bool)
    "contains bold heading" true
    (contains_str result "**Provider Quota/Usage**")

let test_format_model_usage_plain () =
  let config = Runtime_config.default in
  let pq : Provider_quota.provider_quota =
    {
      provider_name = "test-prov";
      state = Provider_quota.Unknown "not_configured";
      fetched_at = Unix.gettimeofday ();
    }
  in
  let result =
    Slash_commands.format_model_usage ~connector:Format_adapter.Plain ~config
      [ pq ]
  in
  Alcotest.(check bool)
    "plain has no code block" false
    (contains_str result "```");
  Alcotest.(check bool)
    "plain contains provider name" true
    (contains_str result "test-prov")

let test_model_bare_name () =
  match Slash_commands.handle "/model glm-5" with
  | Slash_commands.Model (Slash_commands.ModelSet "glm-5") -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Model(Set glm-5), got %s"
           (result_to_string other))

let test_model_bare_name_provider_prefix () =
  match Slash_commands.handle "/model google/gemini-1.5-pro" with
  | Slash_commands.Model (Slash_commands.ModelSet "google/gemini-1.5-pro") -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Model(Set google/gemini-1.5-pro), got %s"
           (result_to_string other))

let test_model_set_explicit_unchanged () =
  match Slash_commands.handle "/model set glm-5" with
  | Slash_commands.Model (Slash_commands.ModelSet "glm-5") -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Model(Set glm-5), got %s"
           (result_to_string other))

let test_model_bare_set_keyword_still_error () =
  (* "/model set" with no name: first token is "set" (known), so falls to usage *)
  match Slash_commands.handle "/model set" with
  | Slash_commands.Reply s ->
      Alcotest.(check bool) "mentions Usage" true (contains_str s "Usage")
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply(Usage), got %s" (result_to_string other))

let test_is_secret_path () =
  Alcotest.(check bool)
    "api_key is secret" true
    (Config_set.is_secret_path "providers.openai.api_key");
  Alcotest.(check bool)
    "bot_token is secret" true
    (Config_set.is_secret_path "channels.discord.bot_token");
  Alcotest.(check bool)
    "host is not secret" false
    (Config_set.is_secret_path "gateway.host");
  Alcotest.(check bool)
    "workspace is not secret" false
    (Config_set.is_secret_path "workspace")

let test_format_status_plain () =
  let text =
    Slash_commands.format_status ~connector:Format_adapter.Plain ~db:None
      ~session_count:5 ~active_count:2 ()
  in
  Alcotest.(check bool)
    "contains Status field" true
    (String_util.contains text "Status");
  Alcotest.(check bool)
    "contains Uptime field" true
    (String_util.contains text "Uptime");
  Alcotest.(check bool)
    "contains Version field" true
    (String_util.contains text "Version");
  Alcotest.(check bool)
    "contains Sessions field" true
    (String_util.contains text "5 total, 2 active");
  Alcotest.(check bool)
    "contains DB Sessions n/a" true
    (String_util.contains text "n/a");
  Alcotest.(check bool)
    "contains Gateway field" true
    (String_util.contains text "Gateway");
  Alcotest.(check bool)
    "contains Telegram field" true
    (String_util.contains text "Telegram");
  Alcotest.(check bool)
    "contains Discord field" true
    (String_util.contains text "Discord")

let test_format_status_telegram_html () =
  let text =
    Slash_commands.format_status ~connector:Format_adapter.Telegram_html
      ~db:None ~session_count:3 ~active_count:1 ()
  in
  Alcotest.(check bool)
    "contains bold tag" true
    (String_util.contains text "<b>Bot Status</b>");
  Alcotest.(check bool)
    "contains pre tag" true
    (String_util.contains text "<pre>");
  Alcotest.(check bool)
    "contains session counts" true
    (String_util.contains text "3 total, 1 active")

let test_render_markdown_basic () =
  let columns =
    Table_format.
      [
        { header = "Name"; align = Left; min_width = 0; flex = false };
        { header = "Value"; align = Right; min_width = 0; flex = false };
      ]
  in
  let rows = [ [ "foo"; "42" ]; [ "bar"; "99" ] ] in
  let output = Table_format.render_markdown columns rows in
  Alcotest.(check bool)
    "header row" true
    (contains_str output "| Name | Value |");
  Alcotest.(check bool)
    "separator row" true
    (contains_str output "| :--- | ---: |");
  Alcotest.(check bool) "data row 1" true (contains_str output "| foo | 42 |");
  Alcotest.(check bool) "data row 2" true (contains_str output "| bar | 99 |")

let test_render_markdown_escape () =
  let columns =
    Table_format.
      [ { header = "Col"; align = Left; min_width = 0; flex = false } ]
  in
  let rows = [ [ "a|b" ] ] in
  let output =
    Table_format.render_markdown
      ~escape_cell:(Format_adapter.escape_table_cell Format_adapter.Teams)
      columns rows
  in
  Alcotest.(check bool) "pipe escaped" true (contains_str output "a\\|b")

let test_format_help_teams_markdown_table () =
  let output = Slash_commands.format_help ~connector:Format_adapter.Teams in
  Alcotest.(check bool)
    "teams help has markdown table header" true
    (contains_str output "| Command | Description |");
  Alcotest.(check bool)
    "teams help has separator" true
    (contains_str output "| :---");
  Alcotest.(check bool)
    "teams help contains /help" true
    (contains_str output "| /help |");
  Alcotest.(check bool)
    "teams help not wrapped in code block" false
    (String.length output >= 3 && String.sub output 0 3 = "```")

let test_format_costs_teams_markdown_table () =
  with_request_stats_db (fun db ->
      insert_request_stat ~db ~session_key:"teams:t1:conv" ~provider:"openai"
        ~model:"gpt-5.4" ~prompt_tokens:1000 ~completion_tokens:200
        ~cost_usd:0.10 ();
      let output =
        Slash_commands.format_costs ~connector:Format_adapter.Teams ~db
          Slash_commands.CostsSummary
      in
      Alcotest.(check bool)
        "teams costs has markdown table" true
        (contains_str output "| PERIOD |");
      Alcotest.(check bool)
        "teams costs not wrapped in code block" false
        (contains_str output "```"))

let test_format_tools_teams_table () =
  let tools =
    [
      {
        Tool.name = "file_read";
        description = "Read file contents";
        parameters_schema = `Assoc [];
        invoke = (fun ?context:_ _args -> Lwt.return "");
        invoke_stream = None;
        risk_level = Tool.Low;
        deferred = false;
      };
      {
        Tool.name = "shell_exec";
        description = "Execute shell commands";
        parameters_schema = `Assoc [];
        invoke = (fun ?context:_ _args -> Lwt.return "");
        invoke_stream = None;
        risk_level = Tool.High;
        deferred = false;
      };
    ]
  in
  let output =
    Slash_commands.format_tools ~connector:Format_adapter.Teams tools []
  in
  Alcotest.(check bool)
    "teams tools has markdown table header" true
    (contains_str output "| Tool |");
  Alcotest.(check bool)
    "teams tools has separator" true
    (contains_str output "| :---");
  Alcotest.(check bool)
    "teams tools contains file_read" true
    (contains_str output "| file_read |");
  Alcotest.(check bool)
    "teams tools not wrapped in code block" false
    (contains_str output "```")

let test_format_tools_discord_code_block () =
  let tools =
    [
      {
        Tool.name = "file_read";
        description = "Read file contents";
        parameters_schema = `Assoc [];
        invoke = (fun ?context:_ _args -> Lwt.return "");
        invoke_stream = None;
        risk_level = Tool.Low;
        deferred = false;
      };
    ]
  in
  let output =
    Slash_commands.format_tools ~connector:Format_adapter.Discord tools []
  in
  Alcotest.(check bool)
    "discord tools wrapped in code block" true
    (contains_str output "```");
  Alcotest.(check bool)
    "discord tools no markdown table pipes" false
    (contains_str output "| Tool |")

let test_format_status_teams_markdown_table () =
  let text =
    Slash_commands.format_status ~connector:Format_adapter.Teams ~db:None
      ~session_count:5 ~active_count:2 ()
  in
  Alcotest.(check bool)
    "teams status has markdown table" true
    (contains_str text "| FIELD |");
  Alcotest.(check bool)
    "teams status has separator" true
    (contains_str text "| :---");
  Alcotest.(check bool)
    "teams status not wrapped in code block" false (contains_str text "```");
  Alcotest.(check bool)
    "teams status has blank line before table" true
    (contains_str text "\n\n|")

let test_format_costs_teams_has_blank_line () =
  with_request_stats_db (fun db ->
      insert_request_stat ~db ~session_key:"teams:t1:conv" ~provider:"openai"
        ~model:"gpt-5.4" ~prompt_tokens:1000 ~completion_tokens:200
        ~cost_usd:0.10 ();
      let output =
        Slash_commands.format_costs ~connector:Format_adapter.Teams ~db
          Slash_commands.CostsSummary
      in
      Alcotest.(check bool)
        "teams costs has blank line before table" true
        (contains_str output "\n\n|"))

let test_format_costs_discord_code_block () =
  with_request_stats_db (fun db ->
      insert_request_stat ~db ~session_key:"discord:chan:user"
        ~provider:"openai" ~model:"gpt-5.4" ~prompt_tokens:1000
        ~completion_tokens:200 ~cost_usd:0.10 ();
      let output =
        Slash_commands.format_costs ~connector:Format_adapter.Discord ~db
          Slash_commands.CostsSummary
      in
      Alcotest.(check bool)
        "discord costs wrapped in code block" true
        (contains_str output "```");
      Alcotest.(check bool)
        "discord costs no markdown table pipes" false
        (contains_str output "| PERIOD |"))

let test_menu_default () =
  Alcotest.check result_testable "/menu returns Menu 1" (Slash_commands.Menu 1)
    (Slash_commands.handle "/menu")

let test_menu_page () =
  Alcotest.check result_testable "/menu 2 returns Menu 2"
    (Slash_commands.Menu 2)
    (Slash_commands.handle "/menu 2");
  Alcotest.check result_testable "/menu 3 returns Menu 3"
    (Slash_commands.Menu 3)
    (Slash_commands.handle "/menu 3")

let test_menu_invalid () =
  match Slash_commands.handle "/menu abc" with
  | Slash_commands.Reply s ->
      Alcotest.(check bool) "mentions Usage" true (contains_str s "Usage")
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_menu_zero_page () =
  match Slash_commands.handle "/menu 0" with
  | Slash_commands.Reply s ->
      Alcotest.(check bool) "mentions Usage" true (contains_str s "Usage")
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply for /menu 0, got %s"
           (result_to_string other))

let test_priority_positive () =
  List.iter
    (fun (c : Slash_commands.command) ->
      Alcotest.(check bool)
        (Printf.sprintf "%s has positive priority" c.name)
        true (c.priority > 0))
    Slash_commands.commands

let test_sorted_by_priority () =
  let sorted = Slash_commands.sorted_by_priority () in
  let rec is_descending = function
    | [] | [ _ ] -> true
    | a :: (b :: _ as rest) ->
        a.Slash_commands.priority >= b.priority && is_descending rest
  in
  Alcotest.(check bool) "sorted descending" true (is_descending sorted);
  Alcotest.(check int)
    "same count"
    (List.length Slash_commands.commands)
    (List.length sorted)

let test_commands_has_menu () =
  let names =
    List.map
      (fun (c : Slash_commands.command) -> c.name)
      Slash_commands.commands
  in
  Alcotest.(check bool) "has menu" true (List.mem "menu" names)

let test_manifest_teams_json () =
  let output = Slash_commands_manifest.teams_json () in
  let json = Yojson.Safe.from_string output in
  let open Yojson.Safe.Util in
  let cmd_lists = json |> member "commandLists" |> to_list in
  Alcotest.(check int) "one command list" 1 (List.length cmd_lists);
  let cmds = List.hd cmd_lists |> member "commands" |> to_list in
  Alcotest.(check int) "default 10 commands" 10 (List.length cmds);
  let first = List.hd cmds in
  let _ = first |> member "title" |> to_string in
  let _ = first |> member "description" |> to_string in
  ()

let test_manifest_teams_json_custom_n () =
  let output = Slash_commands_manifest.teams_json ~n:3 () in
  let json = Yojson.Safe.from_string output in
  let open Yojson.Safe.Util in
  let cmds =
    json |> member "commandLists" |> to_list |> List.hd |> member "commands"
    |> to_list
  in
  Alcotest.(check int) "3 commands" 3 (List.length cmds)

let test_manifest_telegram_json () =
  let output = Slash_commands_manifest.telegram_json () in
  let json = Yojson.Safe.from_string output in
  let open Yojson.Safe.Util in
  let cmds = json |> member "commands" |> to_list in
  Alcotest.(check int)
    "all commands"
    (List.length Slash_commands.commands)
    (List.length cmds);
  let first = List.hd cmds in
  let _ = first |> member "command" |> to_string in
  let _ = first |> member "description" |> to_string in
  ()

let test_manifest_teams_uses_title_key () =
  let output = Slash_commands_manifest.teams_json ~n:1 () in
  Alcotest.(check bool) "uses title key" true (contains_str output "\"title\"");
  Alcotest.(check bool)
    "uses description key" true
    (contains_str output "\"description\"")

let test_manifest_telegram_uses_command_key () =
  let output = Slash_commands_manifest.telegram_json () in
  Alcotest.(check bool)
    "uses command key" true
    (contains_str output "\"command\"");
  Alcotest.(check bool)
    "uses description key" true
    (contains_str output "\"description\"")

let test_menu_adaptive_card_json () =
  let card = Slash_commands_manifest.menu_adaptive_card_json () in
  let open Yojson.Safe.Util in
  let attachments = card |> member "attachments" |> to_list in
  Alcotest.(check int) "one attachment" 1 (List.length attachments);
  let att = List.hd attachments in
  Alcotest.(check string)
    "adaptive card content type" "application/vnd.microsoft.card.adaptive"
    (att |> member "contentType" |> to_string);
  let content = att |> member "content" in
  Alcotest.(check string)
    "card type" "AdaptiveCard"
    (content |> member "type" |> to_string);
  Alcotest.(check string)
    "card version" "1.4"
    (content |> member "version" |> to_string)

let test_menu_adaptive_card_pagination () =
  let card1 = Slash_commands_manifest.menu_adaptive_card_json ~page:1 () in
  let open Yojson.Safe.Util in
  let actions1 =
    card1 |> member "attachments" |> to_list |> List.hd |> member "content"
    |> member "actions" |> to_list
  in
  let total = List.length Slash_commands.commands in
  if total > 9 then begin
    Alcotest.(check bool)
      "page 1 has next button" true
      (List.length actions1 > 0);
    let next_title = List.hd actions1 |> member "title" |> to_string in
    Alcotest.(check bool)
      "next button has Page 2" true
      (contains_str next_title "Page 2")
  end

let suite =
  [
    Alcotest.test_case "handle /start" `Quick test_start;
    Alcotest.test_case "handle /help" `Quick test_help;
    Alcotest.test_case "handle /new" `Quick test_new;
    Alcotest.test_case "handle /compact" `Quick test_compact;
    Alcotest.test_case "handle /runtime-ctx" `Quick test_runtime_ctx;
    Alcotest.test_case "handle /uptime" `Quick test_uptime;
    Alcotest.test_case "handle /status" `Quick test_status;
    Alcotest.test_case "handle /thinking" `Quick test_thinking_show;
    Alcotest.test_case "handle /thinking levels" `Quick test_thinking_set_levels;
    Alcotest.test_case "thinking is case insensitive" `Quick
      test_thinking_case_insensitive;
    Alcotest.test_case "thinking invalid level" `Quick
      test_thinking_invalid_level;
    Alcotest.test_case "thinking too many args" `Quick
      test_thinking_too_many_args;
    Alcotest.test_case "heartbeat status" `Quick test_heartbeat_status;
    Alcotest.test_case "heartbeat toggle" `Quick test_heartbeat_toggle;
    Alcotest.test_case "heartbeat invalid args" `Quick
      test_heartbeat_invalid_args;
    Alcotest.test_case "unknown command" `Quick test_unknown_command;
    Alcotest.test_case "regular message" `Quick test_regular_message;
    Alcotest.test_case "empty message" `Quick test_empty_message;
    Alcotest.test_case "commands list" `Quick test_commands_list;
    Alcotest.test_case "case insensitive" `Quick test_case_insensitive;
    Alcotest.test_case "bare slash" `Quick test_bare_slash;
    Alcotest.test_case "command with args" `Quick test_command_with_args;
    Alcotest.test_case "whitespace only" `Quick test_whitespace_only;
    Alcotest.test_case "leading whitespace" `Quick test_leading_whitespace;
    Alcotest.test_case "delegate with prompt" `Quick test_delegate_with_prompt;
    Alcotest.test_case "delegate no args" `Quick test_delegate_no_args;
    Alcotest.test_case "delegate multi-word prompt" `Quick
      test_delegate_multi_word;
    Alcotest.test_case "fork-and with prompt" `Quick test_fork_and_with_prompt;
    Alcotest.test_case "fork-and no args" `Quick test_fork_and_no_args;
    Alcotest.test_case "fork-and multi-word prompt" `Quick
      test_fork_and_multi_word;
    Alcotest.test_case "/fork_and underscore alias" `Quick
      test_fork_and_underscore_alias;
    Alcotest.test_case "/costs summary" `Quick test_costs_default;
    Alcotest.test_case "/costs session" `Quick test_costs_session;
    Alcotest.test_case "/costs model/provider" `Quick
      test_costs_model_and_provider;
    Alcotest.test_case "/costs invalid args" `Quick
      test_costs_usage_on_invalid_args;
    Alcotest.test_case "/usage summary" `Quick test_usage_default;
    Alcotest.test_case "/usage session" `Quick test_usage_session;
    Alcotest.test_case "/usage model/provider" `Quick
      test_usage_model_and_provider;
    Alcotest.test_case "/usage invalid args" `Quick
      test_usage_usage_on_invalid_args;
    Alcotest.test_case "/show-thinking toggle" `Quick test_show_thinking_toggle;
    Alcotest.test_case "/show-thinking status" `Quick test_show_thinking_status;
    Alcotest.test_case "/show-thinking aliases" `Quick
      test_show_thinking_aliases;
    Alcotest.test_case "/show-thinking bad args" `Quick
      test_show_thinking_bad_args;
    Alcotest.test_case "/config usage" `Quick test_config_usage;
    Alcotest.test_case "/config show" `Quick test_config_show;
    Alcotest.test_case "/config show provider section" `Quick
      test_config_show_provider_section;
    Alcotest.test_case "/config get missing key" `Quick test_config_get_missing;
    Alcotest.test_case "/config keys" `Quick test_config_keys;
    Alcotest.test_case "/config keys prefix" `Quick test_config_keys_prefix;
    Alcotest.test_case "/config set secret blocked" `Quick
      test_config_set_secret_blocked;
    Alcotest.test_case "/config wizard" `Quick test_config_wizard;
    Alcotest.test_case "/config unknown sub" `Quick test_config_unknown_sub;
    Alcotest.test_case "config_leaf_paths" `Quick test_config_leaf_paths;
    Alcotest.test_case "is_secret_path" `Quick test_is_secret_path;
    Alcotest.test_case "/config get no key" `Quick test_config_get_no_key;
    Alcotest.test_case "/config set invalid path" `Quick
      test_config_set_invalid_path;
    Alcotest.test_case "/config set section path rejected" `Quick
      test_config_set_section_path_rejected;
    Alcotest.test_case "/model set colon format" `Quick
      test_model_set_colon_format;
    Alcotest.test_case "/model set slash format" `Quick
      test_model_set_slash_format;
    Alcotest.test_case "/model set-default" `Quick test_model_set_default;
    Alcotest.test_case "/debug_dump_chat returns DebugDumpChat" `Quick
      test_debug_dump_chat_command;
    Alcotest.test_case "/tools returns Tools" `Quick test_tools_command;
    Alcotest.test_case "/tasks returns Tasks" `Quick test_tasks_command;
    Alcotest.test_case "/tasks@bot returns Tasks" `Quick
      test_tasks_command_with_telegram_bot_suffix;
    Alcotest.test_case "/tasks renders empty tree" `Quick
      test_tasks_render_empty_tree;
    Alcotest.test_case "/tasks renders nonempty tree" `Quick
      test_tasks_render_nonempty_tree;
    Alcotest.test_case "/tasks session key isolation" `Quick
      test_tasks_session_key_isolation;
    Alcotest.test_case "format_tools_plain" `Quick test_format_tools_plain;
    Alcotest.test_case "format_tools_telegram" `Quick test_format_tools_telegram;
    Alcotest.test_case "format_tools_telegram empty" `Quick
      test_format_tools_telegram_empty;
    Alcotest.test_case "format_tools_telegram with skills" `Quick
      test_format_tools_telegram_with_skills;
    Alcotest.test_case "format_tools_plain with skills" `Quick
      test_format_tools_plain_with_skills;
    Alcotest.test_case "format costs plain and telegram" `Quick
      test_format_costs_plain_and_telegram;
    Alcotest.test_case "format usage plain and telegram" `Quick
      test_format_usage_plain_and_telegram;
    Alcotest.test_case "format help telegram" `Quick test_format_help_telegram;
    Alcotest.test_case "format help discord code block" `Quick
      test_format_help_discord_code_block;
    Alcotest.test_case "format help slack code block" `Quick
      test_format_help_slack_code_block;
    Alcotest.test_case "format model usage empty" `Quick
      test_format_model_usage_empty;
    Alcotest.test_case "format model usage table" `Quick
      test_format_model_usage_table;
    Alcotest.test_case "format model usage plain" `Quick
      test_format_model_usage_plain;
    Alcotest.test_case "/model bare name sets model" `Quick test_model_bare_name;
    Alcotest.test_case "/model provider/name sets model" `Quick
      test_model_bare_name_provider_prefix;
    Alcotest.test_case "/model set name still works" `Quick
      test_model_set_explicit_unchanged;
    Alcotest.test_case "/model set with no name shows usage" `Quick
      test_model_bare_set_keyword_still_error;
    Alcotest.test_case "format_status plain" `Quick test_format_status_plain;
    Alcotest.test_case "format_status telegram html" `Quick
      test_format_status_telegram_html;
    Alcotest.test_case "render_markdown basic" `Quick test_render_markdown_basic;
    Alcotest.test_case "render_markdown escape" `Quick
      test_render_markdown_escape;
    Alcotest.test_case "format help teams markdown table" `Quick
      test_format_help_teams_markdown_table;
    Alcotest.test_case "format costs teams markdown table" `Quick
      test_format_costs_teams_markdown_table;
    Alcotest.test_case "format costs discord code block" `Quick
      test_format_costs_discord_code_block;
    Alcotest.test_case "format tools teams table" `Quick
      test_format_tools_teams_table;
    Alcotest.test_case "format tools discord code block" `Quick
      test_format_tools_discord_code_block;
    Alcotest.test_case "format status teams markdown table" `Quick
      test_format_status_teams_markdown_table;
    Alcotest.test_case "format costs teams blank line" `Quick
      test_format_costs_teams_has_blank_line;
    Alcotest.test_case "/menu default" `Quick test_menu_default;
    Alcotest.test_case "/menu page" `Quick test_menu_page;
    Alcotest.test_case "/menu invalid" `Quick test_menu_invalid;
    Alcotest.test_case "/menu 0 rejected" `Quick test_menu_zero_page;
    Alcotest.test_case "priority positive" `Quick test_priority_positive;
    Alcotest.test_case "sorted by priority" `Quick test_sorted_by_priority;
    Alcotest.test_case "commands has menu" `Quick test_commands_has_menu;
    Alcotest.test_case "manifest teams json" `Quick test_manifest_teams_json;
    Alcotest.test_case "manifest teams custom n" `Quick
      test_manifest_teams_json_custom_n;
    Alcotest.test_case "manifest telegram json" `Quick
      test_manifest_telegram_json;
    Alcotest.test_case "manifest teams uses title key" `Quick
      test_manifest_teams_uses_title_key;
    Alcotest.test_case "manifest telegram uses command key" `Quick
      test_manifest_telegram_uses_command_key;
    Alcotest.test_case "menu adaptive card json" `Quick
      test_menu_adaptive_card_json;
    Alcotest.test_case "menu adaptive card pagination" `Quick
      test_menu_adaptive_card_pagination;
  ]

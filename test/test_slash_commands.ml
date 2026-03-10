let result_to_string = function
  | Slash_commands.Reply s -> "Reply(" ^ s ^ ")"
  | Slash_commands.Reset -> "Reset"
  | Slash_commands.Thinking Slash_commands.ShowThinking -> "Thinking(Show)"
  | Slash_commands.Thinking (Slash_commands.SetThinking None) ->
      "Thinking(Set off)"
  | Slash_commands.Thinking (Slash_commands.SetThinking (Some level)) ->
      "Thinking(Set " ^ level ^ ")"
  | Slash_commands.Compact -> "Compact"
  | Slash_commands.RuntimeCtx -> "RuntimeCtx"
  | Slash_commands.ShowThinking Slash_commands.ShowThinkingStatus ->
      "ShowThinking(Status)"
  | Slash_commands.ShowThinking Slash_commands.ToggleShowThinking ->
      "ShowThinking(Toggle)"
  | Slash_commands.Delegate s -> "Delegate(" ^ s ^ ")"
  | Slash_commands.ForkAnd s -> "ForkAnd(" ^ s ^ ")"
  | Slash_commands.Tools -> "Tools"
  | Slash_commands.Tasks -> "Tasks"
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
  | Slash_commands.NotACommand -> "NotACommand"

let result_eq a b =
  match (a, b) with
  | Slash_commands.Reply a, Slash_commands.Reply b -> a = b
  | Slash_commands.Reset, Slash_commands.Reset -> true
  | ( Slash_commands.Thinking Slash_commands.ShowThinking,
      Slash_commands.Thinking Slash_commands.ShowThinking ) ->
      true
  | ( Slash_commands.Thinking (Slash_commands.SetThinking a),
      Slash_commands.Thinking (Slash_commands.SetThinking b) ) ->
      a = b
  | Slash_commands.Compact, Slash_commands.Compact -> true
  | Slash_commands.RuntimeCtx, Slash_commands.RuntimeCtx -> true
  | Slash_commands.ShowThinking a, Slash_commands.ShowThinking b -> a = b
  | Slash_commands.Delegate a, Slash_commands.Delegate b -> a = b
  | Slash_commands.ForkAnd a, Slash_commands.ForkAnd b -> a = b
  | Slash_commands.Tools, Slash_commands.Tools -> true
  | Slash_commands.Tasks, Slash_commands.Tasks -> true
  | Slash_commands.Model _, Slash_commands.Model _ -> true
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
  | Slash_commands.Reply s ->
      let contains =
        try
          ignore (Str.search_forward (Str.regexp_string "/help") s 0);
          true
        with Not_found -> false
      in
      Alcotest.(check bool) "contains /help" true contains
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_new () =
  Alcotest.check result_testable "reset" Slash_commands.Reset
    (Slash_commands.handle "/new")

let test_status () =
  match Slash_commands.handle "/status" with
  | Slash_commands.Reply _ -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_compact () =
  Alcotest.check result_testable "compact" Slash_commands.Compact
    (Slash_commands.handle "/compact")

let test_runtime_ctx () =
  Alcotest.check result_testable "runtime-ctx" Slash_commands.RuntimeCtx
    (Slash_commands.handle "/runtime-ctx");
  Alcotest.check result_testable "runtime_ctx" Slash_commands.RuntimeCtx
    (Slash_commands.handle "/runtime_ctx")

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
        "usage" "Usage: /thinking [low|medium|high|off|xhigh|max]" text
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

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
  Alcotest.(check bool) "has update" true (List.mem "update" names);
  Alcotest.(check bool) "has delegate" true (List.mem "delegate" names);
  Alcotest.(check bool)
    "has show_thinking" true
    (List.mem "show_thinking" names);
  Alcotest.(check bool) "has config" true (List.mem "config" names);
  Alcotest.(check bool) "has fork_and" true (List.mem "fork_and" names);
  Alcotest.(check bool) "has tools" true (List.mem "tools" names);
  Alcotest.(check bool) "has tasks" true (List.mem "tasks" names)

let test_case_insensitive () =
  (match Slash_commands.handle "/HELP" with
  | Slash_commands.Reply _ -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply for /HELP, got %s"
           (result_to_string other)));
  Alcotest.check result_testable "reset from /NEW" Slash_commands.Reset
    (Slash_commands.handle "/NEW")

let test_bare_slash () =
  Alcotest.check result_testable "bare slash" Slash_commands.NotACommand
    (Slash_commands.handle "/")

let test_command_with_args () =
  match Slash_commands.handle "/help extra args here" with
  | Slash_commands.Reply _ -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply for /help with args, got %s"
           (result_to_string other))

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

let test_leading_whitespace () =
  match Slash_commands.handle "  /status  " with
  | Slash_commands.Reply _ -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply for padded /status, got %s"
           (result_to_string other))

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
  let output = Slash_commands.format_tools_plain tools in
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
     pos_file < pos_shell)

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
  let output = Slash_commands.format_tools_telegram tools in
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
    (contains_str output "key* value*")

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

let suite =
  [
    Alcotest.test_case "handle /start" `Quick test_start;
    Alcotest.test_case "handle /help" `Quick test_help;
    Alcotest.test_case "handle /new" `Quick test_new;
    Alcotest.test_case "handle /compact" `Quick test_compact;
    Alcotest.test_case "handle /runtime-ctx" `Quick test_runtime_ctx;
    Alcotest.test_case "handle /status" `Quick test_status;
    Alcotest.test_case "handle /thinking" `Quick test_thinking_show;
    Alcotest.test_case "handle /thinking levels" `Quick test_thinking_set_levels;
    Alcotest.test_case "thinking is case insensitive" `Quick
      test_thinking_case_insensitive;
    Alcotest.test_case "thinking invalid level" `Quick
      test_thinking_invalid_level;
    Alcotest.test_case "thinking too many args" `Quick
      test_thinking_too_many_args;
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
    Alcotest.test_case "/model bare name sets model" `Quick test_model_bare_name;
    Alcotest.test_case "/model provider/name sets model" `Quick
      test_model_bare_name_provider_prefix;
    Alcotest.test_case "/model set name still works" `Quick
      test_model_set_explicit_unchanged;
    Alcotest.test_case "/model set with no name shows usage" `Quick
      test_model_bare_set_keyword_still_error;
  ]

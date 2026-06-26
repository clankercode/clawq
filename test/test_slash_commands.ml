let unwrap_admin = function
  | Slash_commands.AdminRequired inner -> inner
  | other -> other

let rec extract_text = function
  | Slash_commands.Reply s -> Some s
  | Slash_commands.FormattedReply fn -> Some (fn Format_adapter.Plain)
  | Slash_commands.AdminRequired inner -> extract_text inner
  | _ -> None

let rec result_to_string = function
  | Slash_commands.Reply s -> "Reply(" ^ s ^ ")"
  | Slash_commands.FormattedReply fn ->
      "FormattedReply(" ^ fn Format_adapter.Plain ^ ")"
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
  | Slash_commands.Debug Slash_commands.DebugStatus -> "Debug(Status)"
  | Slash_commands.Debug (Slash_commands.SetDebug true) -> "Debug(On)"
  | Slash_commands.Debug (Slash_commands.SetDebug false) -> "Debug(Off)"
  | Slash_commands.Delegate (None, s) -> "Delegate(" ^ s ^ ")"
  | Slash_commands.Delegate (Some agent, s) ->
      "Delegate(@" ^ agent ^ ", " ^ s ^ ")"
  | Slash_commands.ForkAnd (None, s) -> "ForkAnd(" ^ s ^ ")"
  | Slash_commands.ForkAnd (Some agent, s) ->
      "ForkAnd(@" ^ agent ^ ", " ^ s ^ ")"
  | Slash_commands.Tools -> "Tools"
  | Slash_commands.Tasks -> "Tasks"
  | Slash_commands.TasksFull -> "TasksFull"
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
  | Slash_commands.Model (Slash_commands.ModelSetForce name) ->
      "Model(SetForce " ^ name ^ ")"
  | Slash_commands.Model (Slash_commands.ModelFav name) ->
      "Model(Fav " ^ name ^ ")"
  | Slash_commands.Model (Slash_commands.ModelUnfav name) ->
      "Model(Unfav " ^ name ^ ")"
  | Slash_commands.Model
      (Slash_commands.ModelList (None, Models_catalog.Available)) ->
      "Model(List)"
  | Slash_commands.Model
      (Slash_commands.ModelList (Some p, Models_catalog.Available)) ->
      "Model(List " ^ p ^ ")"
  | Slash_commands.Model
      (Slash_commands.ModelList (p, Models_catalog.Unavailable)) ->
      "Model(List " ^ Option.value ~default:"-" p ^ " unavailable)"
  | Slash_commands.Model (Slash_commands.ModelList (p, Models_catalog.All)) ->
      "Model(List " ^ Option.value ~default:"-" p ^ " all)"
  | Slash_commands.Model Slash_commands.ModelUsage -> "Model(Usage)"
  | Slash_commands.Model (Slash_commands.ModelSetDefault name) ->
      "Model(SetDefault " ^ name ^ ")"
  | Slash_commands.Status -> "Status"
  | Slash_commands.Menu page -> "Menu(" ^ string_of_int page ^ ")"
  | Slash_commands.Active -> "Active"
  | Slash_commands.Bg Slash_commands.BgList -> "Bg(List)"
  | Slash_commands.Bg (Slash_commands.BgShow id) ->
      "Bg(Show " ^ string_of_int id ^ ")"
  | Slash_commands.Bg (Slash_commands.BgLogs id) ->
      "Bg(Logs " ^ string_of_int id ^ ")"
  | Slash_commands.Bg (Slash_commands.BgCancel id) ->
      "Bg(Cancel " ^ string_of_int id ^ ")"
  | Slash_commands.Bg (Slash_commands.BgRetry id) ->
      "Bg(Retry " ^ string_of_int id ^ ")"
  | Slash_commands.Bg (Slash_commands.BgFinalize id) ->
      "Bg(Finalize " ^ string_of_int id ^ ")"
  | Slash_commands.Bg (Slash_commands.BgCreate (agent_name, prompt)) ->
      let agent_str =
        match agent_name with Some n -> "@" ^ n ^ " " | None -> ""
      in
      "Bg(Create " ^ agent_str ^ prompt ^ ")"
  | Slash_commands.Cron Slash_commands.CronList -> "Cron(List)"
  | Slash_commands.Cron Slash_commands.CronHelp -> "Cron(Help)"
  | Slash_commands.Cron
      (Slash_commands.CronAdd { name; schedule; message; ttl }) ->
      Printf.sprintf "Cron(Add %s %s %s ttl=%s)" name schedule message
        (Option.value ~default:"-" ttl)
  | Slash_commands.Cron
      (Slash_commands.CronEdit { name; schedule; message; ttl }) ->
      Printf.sprintf "Cron(Edit %s sched=%s msg=%s ttl=%s)" name
        (Option.value ~default:"-" schedule)
        (Option.value ~default:"-" message)
        (Option.value ~default:"-" ttl)
  | Slash_commands.Cron (Slash_commands.CronRemove name) ->
      "Cron(Remove " ^ name ^ ")"
  | Slash_commands.Cron (Slash_commands.CronShow name) ->
      "Cron(Show " ^ name ^ ")"
  | Slash_commands.Cron (Slash_commands.CronHistory None) -> "Cron(History)"
  | Slash_commands.Cron (Slash_commands.CronHistory (Some name)) ->
      "Cron(History " ^ name ^ ")"
  | Slash_commands.Cron (Slash_commands.CronTrigger name) ->
      "Cron(Trigger " ^ name ^ ")"
  | Slash_commands.Bl Slash_commands.BlList -> "Bl(List)"
  | Slash_commands.Bl Slash_commands.BlBugs -> "Bl(Bugs)"
  | Slash_commands.Bl Slash_commands.BlIdeas -> "Bl(Ideas)"
  | Slash_commands.Bl (Slash_commands.BlShow id) -> "Bl(Show " ^ id ^ ")"
  | Slash_commands.Session Slash_commands.SessionList -> "Session(List)"
  | Slash_commands.Session (Slash_commands.SessionShow key) ->
      "Session(Show " ^ key ^ ")"
  | Slash_commands.Session (Slash_commands.SessionArchives None) ->
      "Session(Archives)"
  | Slash_commands.Session (Slash_commands.SessionArchives (Some key)) ->
      "Session(Archives " ^ key ^ ")"
  | Slash_commands.Session (Slash_commands.SessionArchiveShow id) ->
      "Session(ArchiveShow " ^ string_of_int id ^ ")"
  | Slash_commands.DebugDumpChat -> "DebugDumpChat"
  | Slash_commands.BashRun cmd -> "BashRun(" ^ cmd ^ ")"
  | Slash_commands.AgentInvoke (name, prompt) ->
      "AgentInvoke(" ^ name ^ ", " ^ prompt ^ ")"
  | Slash_commands.AgentMenu page -> "AgentMenu(" ^ string_of_int page ^ ")"
  | Slash_commands.ModelMenu page -> "ModelMenu(" ^ string_of_int page ^ ")"
  | Slash_commands.ThinkingMenu -> "ThinkingMenu"
  | Slash_commands.ConfigMenu page -> "ConfigMenu(" ^ string_of_int page ^ ")"
  | Slash_commands.SkillsMenu page -> "SkillsMenu(" ^ string_of_int page ^ ")"
  | Slash_commands.CostsMenu -> "CostsMenu"
  | Slash_commands.BgMenu -> "BgMenu"
  | Slash_commands.InjectConnectorHistory n ->
      "InjectConnectorHistory(" ^ string_of_int n ^ ")"
  | Slash_commands.SkillInvoke (name, args) ->
      "SkillInvoke(" ^ name ^ ", " ^ args ^ ")"
  | Slash_commands.AdminRequired inner ->
      "AdminRequired(" ^ result_to_string inner ^ ")"
  | Slash_commands.RegisterAsAdminOtc None -> "RegisterAsAdminOtc(None)"
  | Slash_commands.RegisterAsAdminOtc (Some code) ->
      "RegisterAsAdminOtc(Some " ^ code ^ ")"
  | Slash_commands.Rig action -> (
      match action with
      | Slash_commands.RigInstall name -> "Rig(Install " ^ name ^ ")"
      | Slash_commands.RigAdjust name -> "Rig(Adjust " ^ name ^ ")"
      | Slash_commands.RigRemove name -> "Rig(Remove " ^ name ^ ")"
      | Slash_commands.RigList -> "Rig(List)")
  | Slash_commands.HeldItems action -> (
      match action with
      | Slash_commands.HeldItemsList all ->
          "HeldItems(List " ^ string_of_bool all ^ ")"
      | Slash_commands.HeldItemsShow id ->
          "HeldItems(Show " ^ string_of_int id ^ ")"
      | Slash_commands.HeldItemsApprove id ->
          "HeldItems(Approve " ^ string_of_int id ^ ")"
      | Slash_commands.HeldItemsReject (id, reason) ->
          "HeldItems(Reject " ^ string_of_int id ^ " "
          ^ Option.value ~default:"" reason
          ^ ")")
  | Slash_commands.Repo action -> (
      match action with
      | Slash_commands.RepoStatus -> "Repo(Status)"
      | Slash_commands.RepoAssociate s -> "Repo(Associate " ^ s ^ ")"
      | Slash_commands.RepoForget -> "Repo(Forget)"
      | Slash_commands.RepoUpdate -> "Repo(Update)")
  | Slash_commands.Debate prompt -> "Debate(" ^ prompt ^ ")"
  | Slash_commands.Memories { oldest; page } ->
      Printf.sprintf "Memories(oldest=%b page=%d)" oldest page
  | Slash_commands.NotACommand -> "NotACommand"

let rec result_eq a b =
  match (a, b) with
  | Slash_commands.Reply a, Slash_commands.Reply b -> a = b
  | Slash_commands.FormattedReply a, Slash_commands.FormattedReply b ->
      a Format_adapter.Plain = b Format_adapter.Plain
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
  | Slash_commands.Debug a, Slash_commands.Debug b -> a = b
  | Slash_commands.Delegate (a1, a2), Slash_commands.Delegate (b1, b2) ->
      a1 = b1 && a2 = b2
  | Slash_commands.ForkAnd (a1, a2), Slash_commands.ForkAnd (b1, b2) ->
      a1 = b1 && a2 = b2
  | Slash_commands.Tools, Slash_commands.Tools -> true
  | Slash_commands.Tasks, Slash_commands.Tasks -> true
  | Slash_commands.TasksFull, Slash_commands.TasksFull -> true
  | Slash_commands.Costs a, Slash_commands.Costs b -> a = b
  | Slash_commands.Usage a, Slash_commands.Usage b -> a = b
  | Slash_commands.Model _, Slash_commands.Model _ -> true
  | Slash_commands.Menu a, Slash_commands.Menu b -> a = b
  | Slash_commands.Active, Slash_commands.Active -> true
  | Slash_commands.Bg a, Slash_commands.Bg b -> a = b
  | Slash_commands.Cron a, Slash_commands.Cron b -> a = b
  | Slash_commands.Bl a, Slash_commands.Bl b -> a = b
  | Slash_commands.Session a, Slash_commands.Session b -> a = b
  | Slash_commands.DebugDumpChat, Slash_commands.DebugDumpChat -> true
  | Slash_commands.BashRun a, Slash_commands.BashRun b -> a = b
  | Slash_commands.SkillInvoke (a1, a2), Slash_commands.SkillInvoke (b1, b2) ->
      a1 = b1 && a2 = b2
  | Slash_commands.AgentInvoke (a1, a2), Slash_commands.AgentInvoke (b1, b2) ->
      a1 = b1 && a2 = b2
  | Slash_commands.AgentMenu a, Slash_commands.AgentMenu b -> a = b
  | Slash_commands.ModelMenu a, Slash_commands.ModelMenu b -> a = b
  | Slash_commands.ThinkingMenu, Slash_commands.ThinkingMenu -> true
  | Slash_commands.ConfigMenu a, Slash_commands.ConfigMenu b -> a = b
  | Slash_commands.SkillsMenu a, Slash_commands.SkillsMenu b -> a = b
  | Slash_commands.CostsMenu, Slash_commands.CostsMenu -> true
  | Slash_commands.BgMenu, Slash_commands.BgMenu -> true
  | ( Slash_commands.InjectConnectorHistory a,
      Slash_commands.InjectConnectorHistory b ) ->
      a = b
  | Slash_commands.AdminRequired a, Slash_commands.AdminRequired b ->
      result_eq a b
  | Slash_commands.RegisterAsAdminOtc a, Slash_commands.RegisterAsAdminOtc b ->
      a = b
  | Slash_commands.Rig a, Slash_commands.Rig b -> a = b
  | Slash_commands.Repo a, Slash_commands.Repo b -> a = b
  | Slash_commands.HeldItems a, Slash_commands.HeldItems b -> a = b
  | Slash_commands.Debate a, Slash_commands.Debate b -> a = b
  | Slash_commands.Memories a, Slash_commands.Memories b -> a = b
  | Slash_commands.NotACommand, Slash_commands.NotACommand -> true
  | _ -> false

let result_testable =
  Alcotest.testable
    (fun fmt r -> Format.fprintf fmt "%s" (result_to_string r))
    result_eq

let test_start () =
  match Slash_commands.handle "/start" with
  | Slash_commands.FormattedReply fn ->
      let s = fn Format_adapter.Plain in
      Alcotest.(check bool) "contains ready" true (String.length s > 0)
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected FormattedReply, got %s"
           (result_to_string other))

let test_help () =
  match Slash_commands.handle "/help" with
  | Slash_commands.Help ->
      let s =
        Slash_commands.format_help ~connector:Format_adapter.Plain
          ~is_admin:true ()
      in
      Alcotest.(check bool)
        "contains /help" true
        (Test_helpers.string_contains s "/help");
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
  match extract_text (Slash_commands.handle "/thinking turbo") with
  | Some text ->
      Alcotest.(check bool)
        "mentions invalid thinking level" true
        (Test_helpers.string_contains text "Invalid thinking level")
  | None -> Alcotest.fail "expected text reply for invalid thinking level"

let test_thinking_too_many_args () =
  match extract_text (Slash_commands.handle "/thinking low extra") with
  | Some text ->
      Alcotest.(check bool)
        "mentions /thinking" true
        (Test_helpers.string_contains text "/thinking")
  | None -> Alcotest.fail "expected text reply for thinking usage"

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
  match extract_text (Slash_commands.handle "/heartbeat maybe") with
  | Some text ->
      Alcotest.(check bool)
        "mentions /heartbeat" true
        (Test_helpers.string_contains text "/heartbeat")
  | None -> Alcotest.fail "expected text reply for heartbeat usage"

let test_debug_status () =
  Alcotest.check result_testable "debug status"
    (Slash_commands.Debug Slash_commands.DebugStatus)
    (Slash_commands.handle "/debug");
  Alcotest.check result_testable "debug explicit status"
    (Slash_commands.Debug Slash_commands.DebugStatus)
    (Slash_commands.handle "/debug status")

let test_debug_toggle () =
  Alcotest.check result_testable "debug on"
    (Slash_commands.Debug (Slash_commands.SetDebug true))
    (Slash_commands.handle "/debug on");
  Alcotest.check result_testable "debug off"
    (Slash_commands.Debug (Slash_commands.SetDebug false))
    (Slash_commands.handle "/debug off")

let test_debug_invalid_args () =
  match extract_text (Slash_commands.handle "/debug maybe") with
  | Some text ->
      Alcotest.(check bool)
        "mentions /debug" true
        (Test_helpers.string_contains text "/debug")
  | None -> Alcotest.fail "expected text reply for debug usage"

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
  Alcotest.(check bool) "has debug" true (List.mem "debug" names);
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
      ~is_admin:true ()
  in
  Alcotest.(check bool)
    "telegram uses bold heading" true
    (Test_helpers.string_contains output "<b>Available commands:</b>");
  Alcotest.(check bool)
    "telegram uses code formatting" true
    (Test_helpers.string_contains output "<code>/start</code>")

let test_whitespace_only () =
  Alcotest.check result_testable "whitespace only" Slash_commands.NotACommand
    (Slash_commands.handle "   ")

let test_delegate_with_prompt () =
  Alcotest.check result_testable "delegate with prompt"
    (Slash_commands.Delegate (None, "do something"))
    (Slash_commands.handle "/delegate do something")

let test_delegate_no_args () =
  match extract_text (Slash_commands.handle "/delegate") with
  | Some text ->
      Alcotest.(check bool)
        "mentions /delegate" true
        (Test_helpers.string_contains text "/delegate")
  | None -> Alcotest.fail "expected text reply for delegate usage"

let test_delegate_multi_word () =
  Alcotest.check result_testable "delegate multi-word"
    (Slash_commands.Delegate (None, "a b c d"))
    (Slash_commands.handle "/delegate a b c d")

let test_fork_and_with_prompt () =
  Alcotest.check result_testable "fork-and with prompt"
    (Slash_commands.ForkAnd (None, "summarize this"))
    (Slash_commands.handle "/fork-and summarize this")

let test_fork_and_no_args () =
  match extract_text (Slash_commands.handle "/fork-and") with
  | Some text ->
      Alcotest.(check bool)
        "mentions /fork_and" true
        (Test_helpers.string_contains text "/fork_and")
  | None -> Alcotest.fail "expected text reply for fork_and usage"

let test_fork_and_multi_word () =
  Alcotest.check result_testable "fork-and multi-word"
    (Slash_commands.ForkAnd (None, "a b c d"))
    (Slash_commands.handle "/fork-and a b c d")

let test_fork_and_underscore_alias () =
  Alcotest.check result_testable "fork_and underscore alias"
    (Slash_commands.ForkAnd (None, "do something"))
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
  match extract_text (Slash_commands.handle "/costs nope") with
  | Some text ->
      Alcotest.(check bool)
        "mentions /costs" true
        (Test_helpers.string_contains text "/costs")
  | None -> Alcotest.fail "expected text reply for costs usage"

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
  match extract_text (Slash_commands.handle "/usage nope") with
  | Some text ->
      Alcotest.(check bool)
        "mentions /usage" true
        (Test_helpers.string_contains text "/usage")
  | None -> Alcotest.fail "expected text reply for usage usage"

let test_active () =
  Alcotest.check result_testable "/active" Slash_commands.Active
    (Slash_commands.handle "/active")

let test_bg_list () =
  Alcotest.check result_testable "/bg" (Slash_commands.Bg Slash_commands.BgList)
    (Slash_commands.handle "/bg")

let test_bg_list_explicit () =
  Alcotest.check result_testable "/bg list"
    (Slash_commands.Bg Slash_commands.BgList)
    (Slash_commands.handle "/bg list")

let test_bg_show () =
  Alcotest.check result_testable "/bg show 42"
    (Slash_commands.Bg (Slash_commands.BgShow 42))
    (Slash_commands.handle "/bg show 42")

let test_bg_show_bare_id () =
  Alcotest.check result_testable "/bg 7"
    (Slash_commands.Bg (Slash_commands.BgShow 7))
    (Slash_commands.handle "/bg 7")

let test_bg_logs () =
  Alcotest.check result_testable "/bg logs 5"
    (Slash_commands.Bg (Slash_commands.BgLogs 5))
    (Slash_commands.handle "/bg logs 5")

let test_bg_cancel () =
  Alcotest.check result_testable "/bg cancel 3"
    (Slash_commands.Bg (Slash_commands.BgCancel 3))
    (Slash_commands.handle "/bg cancel 3")

let test_bg_stop_alias () =
  Alcotest.check result_testable "/bg stop 3"
    (Slash_commands.Bg (Slash_commands.BgCancel 3))
    (Slash_commands.handle "/bg stop 3")

let test_bg_cancel_bare () =
  match extract_text (Slash_commands.handle "/bg cancel") with
  | Some s ->
      Alcotest.(check bool)
        "mentions missing id" true
        (Test_helpers.string_contains s "Missing task id");
      Alcotest.(check bool)
        "mentions /bg cancel <id>" true
        (Test_helpers.string_contains s "/bg cancel <id>");
      Alcotest.(check bool)
        "mentions /bg list" true
        (Test_helpers.string_contains s "/bg list")
  | None -> Alcotest.fail "expected text reply for bare /bg cancel"

let test_bg_stop_bare () =
  match extract_text (Slash_commands.handle "/bg stop") with
  | Some s ->
      Alcotest.(check bool)
        "mentions missing id" true
        (Test_helpers.string_contains s "Missing task id")
  | None -> Alcotest.fail "expected text reply for bare /bg stop"

let test_bg_retry () =
  Alcotest.check result_testable "/bg retry 1"
    (Slash_commands.Bg (Slash_commands.BgRetry 1))
    (Slash_commands.handle "/bg retry 1")

let test_bg_finalize () =
  Alcotest.check result_testable "/bg finalize 42"
    (Slash_commands.Bg (Slash_commands.BgFinalize 42))
    (Slash_commands.handle "/bg finalize 42")

let test_bg_finalize_invalid_id () =
  match extract_text (Slash_commands.handle "/bg finalize abc") with
  | Some s ->
      Alcotest.(check bool)
        "mentions invalid" true
        (String_util.contains s "Invalid task id")
  | None -> Alcotest.fail "expected text reply for bg finalize invalid id"

let test_bg_invalid_id () =
  match extract_text (Slash_commands.handle "/bg show abc") with
  | Some s ->
      Alcotest.(check bool)
        "mentions invalid" true
        (String_util.contains s "Invalid task id")
  | None -> Alcotest.fail "expected text reply for bg invalid id"

let test_bg_unknown_subcommand () =
  match extract_text (Slash_commands.handle "/bg foobar") with
  | Some s ->
      Alcotest.(check bool) "mentions /bg" true (String_util.contains s "/bg")
  | None -> Alcotest.fail "expected text reply for bg usage"

let test_bg_background_alias () =
  Alcotest.check result_testable "/background"
    (Slash_commands.Bg Slash_commands.BgList)
    (Slash_commands.handle "/background")

let test_bg_log_alias () =
  Alcotest.check result_testable "/bg log 5"
    (Slash_commands.Bg (Slash_commands.BgLogs 5))
    (Slash_commands.handle "/bg log 5")

let test_bg_create () =
  Alcotest.check result_testable "/bg create fix the bug"
    (Slash_commands.Bg (Slash_commands.BgCreate (None, "fix the bug")))
    (Slash_commands.handle "/bg create fix the bug")

let test_bg_start_alias () =
  Alcotest.check result_testable "/bg start deploy it"
    (Slash_commands.Bg (Slash_commands.BgCreate (None, "deploy it")))
    (Slash_commands.handle "/bg start deploy it")

let test_bg_create_empty_prompt () =
  match extract_text (Slash_commands.handle "/bg create") with
  | Some s ->
      Alcotest.(check bool)
        "mentions /bg create" true
        (String_util.contains s "/bg create")
  | None -> Alcotest.fail "expected text reply for bg create usage"

let test_leading_whitespace () =
  Alcotest.check result_testable "padded status" Slash_commands.Status
    (Slash_commands.handle "  /status  ")

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
  match extract_text (Slash_commands.handle "/show-thinking foo") with
  | Some s ->
      Alcotest.(check bool)
        "mentions /show_thinking" true
        (Test_helpers.string_contains s "/show_thinking")
  | None -> Alcotest.fail "expected text reply for show_thinking usage"

let test_config_usage () =
  match extract_text (Slash_commands.handle "/config") with
  | Some s ->
      Alcotest.(check bool)
        "mentions show" true
        (Test_helpers.string_contains s "show")
  | None -> Alcotest.fail "expected text reply for config usage"

let test_config_show () =
  match extract_text (Slash_commands.handle "/config show") with
  | Some s -> Alcotest.(check bool) "non-empty" true (String.length s > 0)
  | None -> Alcotest.fail "expected text reply for config show"

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
      match
        extract_text (Slash_commands.handle "/config show providers.openai")
      with
      | Some s ->
          Alcotest.(check bool)
            "mentions api_key" true
            (Test_helpers.string_contains s "api_key");
          Alcotest.(check bool)
            "redacts api_key" true
            (Test_helpers.string_contains s "***");
          Alcotest.(check bool)
            "mentions base_url" true
            (Test_helpers.string_contains s "base_url")
      | None -> Alcotest.fail "expected text reply for config show section")

let test_config_get_missing () =
  match unwrap_admin (Slash_commands.handle "/config get nonexistent.key") with
  | Slash_commands.Reply s ->
      Alcotest.(check bool)
        "contains not found" true
        (Test_helpers.string_contains s "not found"
        || Test_helpers.string_contains s "unknown")
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_config_keys () =
  match extract_text (Slash_commands.handle "/config keys") with
  | Some s ->
      Alcotest.(check bool)
        "contains workspace" true
        (Test_helpers.string_contains s "workspace")
  | None -> Alcotest.fail "expected text reply for config keys"

let test_config_keys_prefix () =
  match extract_text (Slash_commands.handle "/config keys gateway") with
  | Some s ->
      Alcotest.(check bool)
        "contains gateway.host" true
        (Test_helpers.string_contains s "gateway.host")
  | None -> Alcotest.fail "expected text reply for config keys prefix"

let test_config_set_secret_blocked () =
  match
    extract_text
      (Slash_commands.handle "/config set channels.discord.bot_token foo")
  with
  | Some s ->
      Alcotest.(check bool)
        "contains cannot" true
        (Test_helpers.string_contains s "Cannot"
        || Test_helpers.string_contains s "cannot")
  | None -> Alcotest.fail "expected text reply for secret blocked"

let test_config_wizard () =
  match extract_text (Slash_commands.handle "/config wizard") with
  | Some s ->
      Alcotest.(check bool)
        "mentions terminal" true
        (Test_helpers.string_contains s "terminal")
  | None -> Alcotest.fail "expected text reply for config wizard"

let test_config_unknown_sub () =
  match extract_text (Slash_commands.handle "/config unknown") with
  | Some s ->
      Alcotest.(check bool)
        "mentions unknown" true
        (Test_helpers.string_contains s "Unknown"
        || Test_helpers.string_contains s "unknown")
  | None -> Alcotest.fail "expected text reply for config unknown sub"

let test_config_leaf_paths () =
  let paths = Config_set.config_leaf_paths () in
  Alcotest.(check bool) "non-empty" true (List.length paths > 0);
  Alcotest.(check bool) "has workspace" true (List.mem "workspace" paths);
  Alcotest.(check bool) "has gateway.host" true (List.mem "gateway.host" paths);
  Alcotest.(check bool)
    "has dynamic placeholder" true
    (List.exists (fun p -> Test_helpers.string_contains p "<NAME>") paths)

let test_config_get_no_key () =
  match extract_text (Slash_commands.handle "/config get") with
  | Some s ->
      Alcotest.(check bool)
        "contains usage" true
        (Test_helpers.string_contains s "Usage"
        || Test_helpers.string_contains s "KEY"
        || Test_helpers.string_contains s "/config get")
  | None -> Alcotest.fail "expected text reply for config get no key"

let test_config_set_invalid_path () =
  match
    unwrap_admin (Slash_commands.handle "/config set totally.bogus.path value")
  with
  | Slash_commands.Reply s ->
      Alcotest.(check bool)
        "mentions unknown key" true
        (Test_helpers.string_contains s "unknown"
        || Test_helpers.string_contains s "Error")
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_config_set_section_path_rejected () =
  match
    unwrap_admin (Slash_commands.handle "/config set providers.openai value")
  with
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

let test_model_list_provider_and_availability_flags () =
  match Slash_commands.handle "/model list --provider openai --all" with
  | Slash_commands.Model
      (Slash_commands.ModelList (Some "openai", Models_catalog.All)) ->
      ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Model(List openai all), got %s"
           (result_to_string other))

let test_model_list_malformed_provider_flags_show_usage () =
  let cases =
    [
      "/model list --provider";
      "/model list --provider --all";
      "/model list --foo";
    ]
  in
  List.iter
    (fun command ->
      match extract_text (Slash_commands.handle command) with
      | Some text ->
          Alcotest.(check bool)
            (command ^ " mentions /model")
            true
            (Test_helpers.string_contains text "/model");
          Alcotest.(check bool)
            (command ^ " mentions list")
            true
            (Test_helpers.string_contains text "list")
      | None ->
          Alcotest.failf "expected usage text for malformed command %s" command)
    cases

let test_debug_dump_chat_command () =
  Alcotest.check result_testable "/debug_dump_chat returns AdminRequired"
    (Slash_commands.AdminRequired Slash_commands.DebugDumpChat)
    (Slash_commands.handle "/debug_dump_chat");
  Alcotest.check result_testable "/debug-dump-chat alias"
    (Slash_commands.AdminRequired Slash_commands.DebugDumpChat)
    (Slash_commands.handle "/debug-dump-chat")

let test_bash_command () =
  Alcotest.check result_testable "/bash ls -la"
    (Slash_commands.AdminRequired (Slash_commands.BashRun "ls -la"))
    (Slash_commands.handle "/bash ls -la")

let test_bash_no_args () =
  match Slash_commands.handle "/bash" with
  | Slash_commands.AdminRequired (Slash_commands.FormattedReply _) -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected AdminRequired(FormattedReply _), got %s"
           (result_to_string other))

let test_bash_multiword_command () =
  Alcotest.check result_testable "/bash echo hello world"
    (Slash_commands.AdminRequired (Slash_commands.BashRun "echo hello world"))
    (Slash_commands.handle "/bash echo hello world")

let test_bash_is_admin_required () =
  match Slash_commands.handle "/bash ls" with
  | Slash_commands.AdminRequired (Slash_commands.BashRun "ls") -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected AdminRequired(BashRun \"ls\"), got %s"
           (result_to_string other))

let test_bash_pipes_and_chains () =
  Alcotest.check result_testable "/bash with pipes and chains"
    (Slash_commands.AdminRequired
       (Slash_commands.BashRun "ls -alh && cd ../ && ls -alh | grep home"))
    (Slash_commands.handle "/bash ls -alh && cd ../ && ls -alh | grep home")

let test_bash_in_commands_list () =
  let names =
    List.map
      (fun (c : Slash_commands.command) -> c.name)
      Slash_commands.commands
  in
  Alcotest.(check bool) "bash in commands" true (List.mem "bash" names)

let test_tools_command () =
  Alcotest.check result_testable "/tools returns Tools" Slash_commands.Tools
    (Slash_commands.handle "/tools")

let test_memories_default () =
  Alcotest.check result_testable "/memories defaults to newest, page 1"
    (Slash_commands.Memories { oldest = false; page = 1 })
    (Slash_commands.handle "/memories")

let test_memories_oldest () =
  Alcotest.check result_testable "/memories oldest"
    (Slash_commands.Memories { oldest = true; page = 1 })
    (Slash_commands.handle "/memories oldest")

let test_memories_page () =
  Alcotest.check result_testable "/memories 3 selects page 3"
    (Slash_commands.Memories { oldest = false; page = 3 })
    (Slash_commands.handle "/memories 3")

let test_memories_oldest_page () =
  Alcotest.check result_testable "/memories oldest 2 selects oldest page 2"
    (Slash_commands.Memories { oldest = true; page = 2 })
    (Slash_commands.handle "/memories oldest 2");
  Alcotest.check result_testable "/memories 2 oldest order-independent"
    (Slash_commands.Memories { oldest = true; page = 2 })
    (Slash_commands.handle "/memories 2 oldest")

let test_memories_invalid_args () =
  match extract_text (Slash_commands.handle "/memories bogus") with
  | Some s ->
      Alcotest.(check bool)
        "mentions /memories" true
        (Test_helpers.string_contains s "/memories")
  | None -> Alcotest.fail "expected usage text for invalid /memories args"

let test_tasks_command () =
  Alcotest.check result_testable "/tasks returns Tasks" Slash_commands.Tasks
    (Slash_commands.handle "/tasks")

let test_tasks_command_with_telegram_bot_suffix () =
  Alcotest.check result_testable "/tasks@bot returns Tasks" Slash_commands.Tasks
    (Slash_commands.handle "/tasks@clawq_bot")

let test_tasks_full_command () =
  Alcotest.check result_testable "/tasks full returns TasksFull"
    Slash_commands.TasksFull
    (Slash_commands.handle "/tasks full")

let test_tasks_invalid_args () =
  match extract_text (Slash_commands.handle "/tasks bogus") with
  | Some s ->
      Alcotest.(check bool)
        "mentions /tasks" true
        (Test_helpers.string_contains s "/tasks")
  | None -> Alcotest.fail "expected text reply for tasks usage"

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

let insert_core_memory ~db ~key ~content ~updated =
  let sql =
    "INSERT INTO core_memories (key, content, category, created_at, \
     updated_at) VALUES (?, ?, 'general', ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT content));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int updated)));
  ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.INT (Int64.of_int updated)));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt)

let test_memories_format_empty () =
  let db = Memory.init ~db_path:":memory:" () in
  let out =
    Slash_commands.format_memories ~connector:Format_adapter.Plain ~db
      { Slash_commands.oldest = false; page = 1 }
  in
  Alcotest.(check bool)
    "empty mentions no memories" true
    (Test_helpers.string_contains out "No memories")

let test_memories_format_ordering () =
  let db = Memory.init ~db_path:":memory:" () in
  insert_core_memory ~db ~key:"oldkey" ~content:"old content" ~updated:1000;
  insert_core_memory ~db ~key:"newkey" ~content:"new content" ~updated:2000;
  let newest =
    Slash_commands.format_memories ~connector:Format_adapter.Plain ~db
      { Slash_commands.oldest = false; page = 1 }
  in
  let pos s hay = Str.search_forward (Str.regexp_string s) hay 0 in
  Alcotest.(check bool)
    "newest-first lists newkey before oldkey" true
    (pos "newkey" newest < pos "oldkey" newest);
  let oldest =
    Slash_commands.format_memories ~connector:Format_adapter.Plain ~db
      { Slash_commands.oldest = true; page = 1 }
  in
  Alcotest.(check bool)
    "oldest-first lists oldkey before newkey" true
    (pos "oldkey" oldest < pos "newkey" oldest)

let test_memories_format_pagination () =
  let db = Memory.init ~db_path:":memory:" () in
  for i = 1 to 11 do
    insert_core_memory ~db
      ~key:(Printf.sprintf "key%02d" i)
      ~content:"content" ~updated:(1000 + i)
  done;
  let out =
    Slash_commands.format_memories ~connector:Format_adapter.Plain ~db
      { Slash_commands.oldest = false; page = 1 }
  in
  Alcotest.(check bool)
    "shows total count" true
    (Test_helpers.string_contains out "11 total");
  Alcotest.(check bool)
    "shows page footer" true
    (Test_helpers.string_contains out "Page 1/2");
  Alcotest.(check bool)
    "footer links to next page" true
    (Test_helpers.string_contains out "/memories 2")

let test_memories_format_escapes_telegram_html () =
  let db = Memory.init ~db_path:":memory:" () in
  insert_core_memory ~db ~key:"k" ~content:"if x < y && y > z then <b>hi</b>"
    ~updated:1000;
  let out =
    Slash_commands.format_memories ~connector:Format_adapter.Telegram_html ~db
      { Slash_commands.oldest = false; page = 1 }
  in
  Alcotest.(check bool)
    "escapes < as &lt;" true
    (Test_helpers.string_contains out "&lt;");
  Alcotest.(check bool)
    "escapes & as &amp;" true
    (Test_helpers.string_contains out "&amp;");
  Alcotest.(check bool)
    "raw content tag is escaped, not literal" false
    (Test_helpers.string_contains out "<b>hi</b>")

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
    Slash_commands.format_tools ~connector:Format_adapter.Plain tools [] []
  in
  Alcotest.(check bool)
    "contains count" true
    (Test_helpers.string_contains output "(2)");
  Alcotest.(check bool)
    "contains file_read" true
    (Test_helpers.string_contains output "file_read");
  Alcotest.(check bool)
    "contains shell_exec" true
    (Test_helpers.string_contains output "shell_exec");
  Alcotest.(check bool)
    "contains [High]" true
    (Test_helpers.string_contains output "[High]");
  Alcotest.(check bool)
    "contains [Low]" true
    (Test_helpers.string_contains output "[Low]");
  Alcotest.(check bool)
    "contains required marker" true
    (Test_helpers.string_contains output "path*");
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
    (not (Test_helpers.string_contains output "Skills"))

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
      []
  in
  Alcotest.(check bool)
    "contains <b>" true
    (Test_helpers.string_contains output "<b>");
  Alcotest.(check bool)
    "contains blockquote" true
    (Test_helpers.string_contains output "<blockquote expandable>");
  Alcotest.(check bool)
    "contains tool name" true
    (Test_helpers.string_contains output "memory_store");
  Alcotest.(check bool)
    "contains <code>" true
    (Test_helpers.string_contains output "<code>");
  Alcotest.(check bool)
    "contains required markers" true
    (Test_helpers.string_contains output "key* value*");
  Alcotest.(check bool)
    "no Skills section when skills empty" true
    (not (Test_helpers.string_contains output "Skills"))

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
    Slash_commands.format_tools ~connector:Format_adapter.Telegram_html [] [] []
  in
  Alcotest.(check bool)
    "contains Tools (0)" true
    (Test_helpers.string_contains output "Tools (0)");
  Alcotest.(check bool)
    "no blockquote when empty" true
    (not (Test_helpers.string_contains output "<blockquote"))

let test_format_tools_telegram_with_skills () =
  let tools = [ make_dummy_tool "file_read" "Read a file" ] in
  let skills = [ make_dummy_tool "my_script" "Run my script" ] in
  let output =
    Slash_commands.format_tools ~connector:Format_adapter.Telegram_html tools
      skills []
  in
  Alcotest.(check bool)
    "has Tools section" true
    (Test_helpers.string_contains output "Tools (1)");
  Alcotest.(check bool)
    "has Skills section" true
    (Test_helpers.string_contains output "Skills (1)");
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
    Slash_commands.format_tools ~connector:Format_adapter.Plain tools skills []
  in
  Alcotest.(check bool)
    "has Tools section" true
    (Test_helpers.string_contains output "Tools (1)");
  Alcotest.(check bool)
    "has Skills section" true
    (Test_helpers.string_contains output "Skills (1)");
  Alcotest.(check bool)
    "has file_read" true
    (Test_helpers.string_contains output "file_read");
  Alcotest.(check bool)
    "has my_script" true
    (Test_helpers.string_contains output "my_script");
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
        (Test_helpers.string_contains summary "Cost Summary");
      Alcotest.(check bool)
        "plain summary includes all time" true
        (Test_helpers.string_contains summary "All time");
      Alcotest.(check bool)
        "plain summary has PERIOD header" true
        (Test_helpers.string_contains summary "PERIOD");
      Alcotest.(check bool)
        "plain summary has COST header" true
        (Test_helpers.string_contains summary "COST");
      let sessions =
        Slash_commands.format_costs ~connector:Format_adapter.Plain ~db
          Slash_commands.CostsSessions
      in
      Alcotest.(check bool)
        "plain sessions heading" true
        (Test_helpers.string_contains sessions "Session Costs");
      Alcotest.(check bool)
        "plain sessions include telegram key" true
        (Test_helpers.string_contains sessions "telegram:1:user");
      let telegram =
        Slash_commands.format_costs ~connector:Format_adapter.Telegram_html ~db
          Slash_commands.CostsSessions
      in
      Alcotest.(check bool)
        "telegram heading" true
        (Test_helpers.string_contains telegram "<b>Session Costs</b>");
      Alcotest.(check bool)
        "telegram uses pre code block" true
        (Test_helpers.string_contains telegram "<pre>");
      Alcotest.(check bool)
        "telegram contains session key" true
        (Test_helpers.string_contains telegram "telegram:1:user"))

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
        (Test_helpers.string_contains summary "Usage Summary");
      Alcotest.(check bool)
        "plain usage includes all time" true
        (Test_helpers.string_contains summary "All time");
      Alcotest.(check bool)
        "plain usage has TURNS header" true
        (Test_helpers.string_contains summary "TURNS");
      let sessions =
        Slash_commands.format_usage ~connector:Format_adapter.Plain ~db
          Slash_commands.UsageSessions
      in
      Alcotest.(check bool)
        "plain usage sessions heading" true
        (Test_helpers.string_contains sessions "Session Usage");
      Alcotest.(check bool)
        "plain usage sessions include telegram key" true
        (Test_helpers.string_contains sessions "telegram:1:user");
      Alcotest.(check bool)
        "plain usage shows added prompt tokens" true
        (Test_helpers.string_contains sessions "900");
      let telegram =
        Slash_commands.format_usage ~connector:Format_adapter.Telegram_html ~db
          Slash_commands.UsageSessions
      in
      Alcotest.(check bool)
        "telegram usage heading" true
        (Test_helpers.string_contains telegram "<b>Session Usage</b>");
      Alcotest.(check bool)
        "telegram usage uses pre code block" true
        (Test_helpers.string_contains telegram "<pre>");
      Alcotest.(check bool)
        "telegram usage contains session key" true
        (Test_helpers.string_contains telegram "telegram:1:user"))

let test_format_help_discord_code_block () =
  let output =
    Slash_commands.format_help ~connector:Format_adapter.Discord ~is_admin:true
      ()
  in
  Alcotest.(check bool)
    "discord help wrapped in code block" true
    (String.length output > 6
    && String.sub output 0 3 = "```"
    && Test_helpers.string_contains output "```\n");
  Alcotest.(check bool)
    "discord help contains /help" true
    (Test_helpers.string_contains output "/help")

let test_format_help_slack_code_block () =
  let output =
    Slash_commands.format_help ~connector:Format_adapter.Slack ~is_admin:true ()
  in
  Alcotest.(check bool)
    "slack help wrapped in code block" true
    (String.length output > 6 && String.sub output 0 3 = "```");
  Alcotest.(check bool)
    "slack help contains /help" true
    (Test_helpers.string_contains output "/help")

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
  Alcotest.(check bool)
    "contains code block" true
    (Test_helpers.string_contains result "```");
  Alcotest.(check bool)
    "contains PROVIDER header" true
    (Test_helpers.string_contains result "PROVIDER");
  Alcotest.(check bool)
    "contains openai" true
    (Test_helpers.string_contains result "openai");
  Alcotest.(check bool)
    "contains bold heading" true
    (Test_helpers.string_contains result "**Provider Quota/Usage**")

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
    (Test_helpers.string_contains result "```");
  Alcotest.(check bool)
    "plain contains provider name" true
    (Test_helpers.string_contains result "test-prov")

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
  match extract_text (Slash_commands.handle "/model set") with
  | Some s ->
      Alcotest.(check bool)
        "mentions /model" true
        (Test_helpers.string_contains s "/model")
  | None -> Alcotest.fail "expected text reply for /model set usage"

let test_model_help () =
  match extract_text (Slash_commands.handle "/model help") with
  | Some s ->
      Alcotest.(check bool)
        "mentions /model" true
        (Test_helpers.string_contains s "/model");
      Alcotest.(check bool)
        "mentions set-force" true
        (Test_helpers.string_contains s "set-force")
  | None -> Alcotest.fail "expected text reply for /model help"

let test_model_set_force () =
  match Slash_commands.handle "/model set-force openai:gpt-5" with
  | Slash_commands.Model (Slash_commands.ModelSetForce "openai:gpt-5") -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Model(SetForce openai:gpt-5), got %s"
           (result_to_string other))

let test_model_set_force_bare () =
  match Slash_commands.handle "/model set-force custom-model" with
  | Slash_commands.Model (Slash_commands.ModelSetForce "custom-model") -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Model(SetForce custom-model), got %s"
           (result_to_string other))

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
    (Test_helpers.string_contains output "| Name | Value |");
  Alcotest.(check bool)
    "separator row" true
    (Test_helpers.string_contains output "| :--- | ---: |");
  Alcotest.(check bool)
    "data row 1" true
    (Test_helpers.string_contains output "| foo | 42 |");
  Alcotest.(check bool)
    "data row 2" true
    (Test_helpers.string_contains output "| bar | 99 |")

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
  Alcotest.(check bool)
    "pipe escaped" true
    (Test_helpers.string_contains output "a\\|b")

let test_format_help_teams_markdown_table () =
  let output =
    Slash_commands.format_help ~connector:Format_adapter.Teams ~is_admin:true ()
  in
  Alcotest.(check bool)
    "teams help has markdown table header" true
    (Test_helpers.string_contains output "| Command | Description |");
  Alcotest.(check bool)
    "teams help has separator" true
    (Test_helpers.string_contains output "| :---");
  Alcotest.(check bool)
    "teams help contains /help" true
    (Test_helpers.string_contains output "| /help |");
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
        (Test_helpers.string_contains output "| PERIOD |");
      Alcotest.(check bool)
        "teams costs not wrapped in code block" false
        (Test_helpers.string_contains output "```"))

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
    Slash_commands.format_tools ~connector:Format_adapter.Teams tools [] []
  in
  Alcotest.(check bool)
    "teams tools has markdown table header" true
    (Test_helpers.string_contains output "| Tool |");
  Alcotest.(check bool)
    "teams tools has separator" true
    (Test_helpers.string_contains output "| :---");
  Alcotest.(check bool)
    "teams tools contains file_read" true
    (Test_helpers.string_contains output "| file_read |");
  Alcotest.(check bool)
    "teams tools not wrapped in code block" false
    (Test_helpers.string_contains output "```")

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
    Slash_commands.format_tools ~connector:Format_adapter.Discord tools [] []
  in
  Alcotest.(check bool)
    "discord tools wrapped in code block" true
    (Test_helpers.string_contains output "```");
  Alcotest.(check bool)
    "discord tools no markdown table pipes" false
    (Test_helpers.string_contains output "| Tool |")

let make_dummy_agent name description : Agent_template.t =
  {
    name;
    description;
    role = Agent_template.Coder;
    goal = "";
    backstory = "";
    system_prompt = "";
    model = None;
    max_tool_iterations = None;
    allowed_tools = [];
    disallowed_tools = [];
    tool_search_enabled = None;
    reasoning_effort = None;
    cwd = None;
    source = Agent_template.Builtin;
    metadata = [];
  }

let test_format_help_with_skills () =
  let skills : Skills.skill_md_meta list =
    [
      {
        md_name = "deploy";
        md_description = "Deploy to production";
        md_allowed_tools = [];
        md_model = None;
        md_disable_model_invocation = false;
        md_source_path = "/tmp/test";
      };
    ]
  in
  let output =
    Slash_commands.format_help_with ~connector:Format_adapter.Plain
      ~commands:Slash_commands.commands ~skills ~agents:[]
  in
  Alcotest.(check bool)
    "has Skills section" true
    (Test_helpers.string_contains output "Skills (1):");
  Alcotest.(check bool)
    "has skill name" true
    (Test_helpers.string_contains output "/deploy");
  Alcotest.(check bool)
    "has skill description" true
    (Test_helpers.string_contains output "Deploy to production")

let test_format_help_with_agents () =
  let agents = [ make_dummy_agent "reviewer" "Code review specialist" ] in
  let output =
    Slash_commands.format_help_with ~connector:Format_adapter.Plain
      ~commands:Slash_commands.commands ~skills:[] ~agents
  in
  Alcotest.(check bool)
    "has Agents section" true
    (Test_helpers.string_contains output "Agents (1):");
  Alcotest.(check bool)
    "has agent name" true
    (Test_helpers.string_contains output "@reviewer");
  Alcotest.(check bool)
    "has agent description" true
    (Test_helpers.string_contains output "Code review specialist");
  Alcotest.(check bool)
    "no Skills section when empty" true
    (not (Test_helpers.string_contains output "Skills"))

let test_format_help_teams_skills_bulleted () =
  let skills : Skills.skill_md_meta list =
    [
      {
        md_name = "deploy";
        md_description = "Deploy to production";
        md_allowed_tools = [];
        md_model = None;
        md_disable_model_invocation = false;
        md_source_path = "/tmp/test";
      };
      {
        md_name = "lint";
        md_description = "Lint the codebase";
        md_allowed_tools = [];
        md_model = None;
        md_disable_model_invocation = false;
        md_source_path = "/tmp/test";
      };
    ]
  in
  let output =
    Slash_commands.format_help_with ~connector:Format_adapter.Teams
      ~commands:Slash_commands.commands ~skills ~agents:[]
  in
  Alcotest.(check bool)
    "deploy is bulleted" true
    (Test_helpers.string_contains output "- `/deploy`");
  Alcotest.(check bool)
    "lint is bulleted" true
    (Test_helpers.string_contains output "- `/lint`")

let test_format_help_teams_agents_bulleted () =
  let agents =
    [
      make_dummy_agent "reviewer" "Code review specialist";
      make_dummy_agent "planner" "Plan implementation tasks";
    ]
  in
  let output =
    Slash_commands.format_help_with ~connector:Format_adapter.Teams
      ~commands:Slash_commands.commands ~skills:[] ~agents
  in
  Alcotest.(check bool)
    "reviewer is bulleted" true
    (Test_helpers.string_contains output "- `@reviewer`");
  Alcotest.(check bool)
    "planner is bulleted" true
    (Test_helpers.string_contains output "- `@planner`")

let test_format_help_plain_skills_no_bullets () =
  let skills : Skills.skill_md_meta list =
    [
      {
        md_name = "deploy";
        md_description = "Deploy to production";
        md_allowed_tools = [];
        md_model = None;
        md_disable_model_invocation = false;
        md_source_path = "/tmp/test";
      };
    ]
  in
  let output =
    Slash_commands.format_help_with ~connector:Format_adapter.Plain
      ~commands:Slash_commands.commands ~skills ~agents:[]
  in
  Alcotest.(check bool)
    "no bullet prefix" true
    (not (Test_helpers.string_contains output "- /deploy"))

let test_format_tools_with_agents () =
  let tools = [ make_dummy_tool "file_read" "Read a file" ] in
  let agents = [ make_dummy_agent "planner" "Plan implementation tasks" ] in
  let output =
    Slash_commands.format_tools ~connector:Format_adapter.Plain tools [] agents
  in
  Alcotest.(check bool)
    "has Tools section" true
    (Test_helpers.string_contains output "Tools (1)");
  Alcotest.(check bool)
    "has Agents section" true
    (Test_helpers.string_contains output "Agents (1)");
  Alcotest.(check bool)
    "has agent name" true
    (Test_helpers.string_contains output "@planner");
  Alcotest.(check bool)
    "has agent description" true
    (Test_helpers.string_contains output "Plan implementation tasks");
  Alcotest.(check bool)
    "tools before agents" true
    (let pos_tools =
       try Str.search_forward (Str.regexp_string "Tools (1)") output 0
       with Not_found -> max_int
     in
     let pos_agents =
       try Str.search_forward (Str.regexp_string "Agents (1)") output 0
       with Not_found -> max_int
     in
     pos_tools < pos_agents)

let test_format_status_teams_markdown_table () =
  let text =
    Slash_commands.format_status ~connector:Format_adapter.Teams ~db:None
      ~session_count:5 ~active_count:2 ()
  in
  Alcotest.(check bool)
    "teams status has markdown table" true
    (Test_helpers.string_contains text "| FIELD |");
  Alcotest.(check bool)
    "teams status has separator" true
    (Test_helpers.string_contains text "| :---");
  Alcotest.(check bool)
    "teams status not wrapped in code block" false
    (Test_helpers.string_contains text "```");
  Alcotest.(check bool)
    "teams status has blank line before table" true
    (Test_helpers.string_contains text "\n\n|")

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
        (Test_helpers.string_contains output "\n\n|"))

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
        (Test_helpers.string_contains output "```");
      Alcotest.(check bool)
        "discord costs no markdown table pipes" false
        (Test_helpers.string_contains output "| PERIOD |"))

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
  match extract_text (Slash_commands.handle "/menu abc") with
  | Some s ->
      Alcotest.(check bool)
        "mentions /menu" true
        (Test_helpers.string_contains s "/menu")
  | None -> Alcotest.fail "expected text reply for menu invalid"

let test_menu_zero_page () =
  match extract_text (Slash_commands.handle "/menu 0") with
  | Some s ->
      Alcotest.(check bool)
        "mentions /menu" true
        (Test_helpers.string_contains s "/menu")
  | None -> Alcotest.fail "expected text reply for menu 0"

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

let test_is_admin_command_auto_detects () =
  Alcotest.(check bool)
    "config is admin" true
    (Slash_commands.is_admin_command "config");
  Alcotest.(check bool)
    "debug_dump_chat is admin" true
    (Slash_commands.is_admin_command "debug_dump_chat");
  Alcotest.(check bool)
    "help is not admin" false
    (Slash_commands.is_admin_command "help");
  Alcotest.(check bool)
    "new is not admin" false
    (Slash_commands.is_admin_command "new")

let test_sorted_by_priority_filters_admin () =
  let guest = Slash_commands.sorted_by_priority ~is_admin:false () in
  let guest_names =
    List.map (fun (c : Slash_commands.command) -> c.name) guest
  in
  Alcotest.(check bool)
    "config hidden for guest" false
    (List.mem "config" guest_names);
  Alcotest.(check bool)
    "debug_dump_chat hidden for guest" false
    (List.mem "debug_dump_chat" guest_names);
  Alcotest.(check bool)
    "help visible for guest" true
    (List.mem "help" guest_names);
  let admin = Slash_commands.sorted_by_priority ~is_admin:true () in
  let admin_names =
    List.map (fun (c : Slash_commands.command) -> c.name) admin
  in
  Alcotest.(check bool)
    "config visible for admin" true
    (List.mem "config" admin_names)

let test_format_help_hides_admin_commands_for_guest () =
  let output =
    Slash_commands.format_help ~connector:Format_adapter.Plain ~is_admin:false
      ()
  in
  Alcotest.(check bool)
    "config hidden in guest help" false
    (Test_helpers.string_contains output "/config");
  Alcotest.(check bool)
    "debug_dump_chat hidden in guest help" false
    (Test_helpers.string_contains output "/debug_dump_chat");
  Alcotest.(check bool)
    "help visible in guest help" true
    (Test_helpers.string_contains output "/help")

let test_format_help_shows_admin_commands_for_admin () =
  let output =
    Slash_commands.format_help ~connector:Format_adapter.Plain ~is_admin:true ()
  in
  Alcotest.(check bool)
    "config in admin help" true
    (Test_helpers.string_contains output "/config");
  Alcotest.(check bool)
    "debug_dump_chat in admin help" true
    (Test_helpers.string_contains output "/debug_dump_chat");
  Alcotest.(check bool)
    "help in admin help" true
    (Test_helpers.string_contains output "/help")

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
  let expected =
    List.length Slash_commands.commands
    + List.length
        (Skills.filter_visible_skills ~show_test:false
           (Skills.available_skills ()))
  in
  Alcotest.(check int) "all commands" expected (List.length cmds);
  let first = List.hd cmds in
  let _ = first |> member "command" |> to_string in
  let _ = first |> member "description" |> to_string in
  ()

let test_manifest_teams_uses_title_key () =
  let output = Slash_commands_manifest.teams_json ~n:1 () in
  Alcotest.(check bool)
    "uses title key" true
    (Test_helpers.string_contains output "\"title\"");
  Alcotest.(check bool)
    "uses description key" true
    (Test_helpers.string_contains output "\"description\"")

let test_manifest_teams_top10_composition () =
  let output = Slash_commands_manifest.teams_json () in
  let json = Yojson.Safe.from_string output in
  let open Yojson.Safe.Util in
  let cmds =
    json |> member "commandLists" |> to_list |> List.hd |> member "commands"
    |> to_list
  in
  let names = List.map (fun c -> c |> member "title" |> to_string) cmds in
  Alcotest.(check bool) "menu in top 10" true (List.mem "menu" names);
  Alcotest.(check bool) "model not in top 10" false (List.mem "model" names);
  Alcotest.(check bool)
    "thinking not in top 10" false
    (List.mem "thinking" names);
  Alcotest.(check bool) "tasks not in top 10" false (List.mem "tasks" names)

let test_manifest_telegram_uses_command_key () =
  let output = Slash_commands_manifest.telegram_json () in
  Alcotest.(check bool)
    "uses command key" true
    (Test_helpers.string_contains output "\"command\"");
  Alcotest.(check bool)
    "uses description key" true
    (Test_helpers.string_contains output "\"description\"")

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
      (Test_helpers.string_contains next_title "Page 2")
  end

let test_cron_list () =
  Alcotest.check result_testable "/cron list"
    (Slash_commands.Cron Slash_commands.CronList)
    (Slash_commands.handle "/cron list")

let test_cron_bare () =
  Alcotest.check result_testable "/cron bare"
    (Slash_commands.Cron Slash_commands.CronList)
    (Slash_commands.handle "/cron")

let test_cron_add () =
  Alcotest.check result_testable "/cron add interval"
    (Slash_commands.Cron
       (Slash_commands.CronAdd
          {
            name = "test-job";
            schedule = "every 5m";
            message = "hello";
            ttl = None;
          }))
    (Slash_commands.handle "/cron add test-job every 5m hello")

let test_cron_add_multi_word () =
  Alcotest.check result_testable "/cron add cron-expr"
    (Slash_commands.Cron
       (Slash_commands.CronAdd
          {
            name = "daily";
            schedule = "0 9 * * *";
            message = "check the dashboard";
            ttl = None;
          }))
    (Slash_commands.handle "/cron add daily 0 9 * * * check the dashboard")

let test_cron_add_with_ttl () =
  Alcotest.check result_testable "/cron add with --ttl"
    (Slash_commands.Cron
       (Slash_commands.CronAdd
          {
            name = "jobname";
            schedule = "every 5m";
            message = "message";
            ttl = Some "24h";
          }))
    (Slash_commands.handle "/cron add jobname every 5m message --ttl 24h")

let test_cron_edit_ttl () =
  Alcotest.check result_testable "/cron edit with --ttl"
    (Slash_commands.Cron
       (Slash_commands.CronEdit
          {
            name = "myjob";
            schedule = Some "every 10m";
            message = None;
            ttl = Some "2h";
          }))
    (Slash_commands.handle "/cron edit myjob --schedule every 10m --ttl 2h")

let test_cron_remove () =
  Alcotest.check result_testable "/cron remove"
    (Slash_commands.Cron (Slash_commands.CronRemove "myjob"))
    (Slash_commands.handle "/cron remove myjob")

let test_cron_rm_alias () =
  Alcotest.check result_testable "/cron rm"
    (Slash_commands.Cron (Slash_commands.CronRemove "myjob"))
    (Slash_commands.handle "/cron rm myjob")

let test_cron_edit_schedule () =
  Alcotest.check result_testable "/cron edit schedule (interval)"
    (Slash_commands.Cron
       (Slash_commands.CronEdit
          {
            name = "myjob";
            schedule = Some "every 10m";
            message = None;
            ttl = None;
          }))
    (Slash_commands.handle "/cron edit myjob --schedule every 10m")

let test_cron_edit_message () =
  Alcotest.check result_testable "/cron edit message"
    (Slash_commands.Cron
       (Slash_commands.CronEdit
          {
            name = "myjob";
            schedule = None;
            message = Some "new prompt";
            ttl = None;
          }))
    (Slash_commands.handle "/cron edit myjob --message new prompt")

let test_cron_edit_nothing () =
  match extract_text (Slash_commands.handle "/cron edit myjob") with
  | Some s ->
      Alcotest.(check bool)
        "edit no flags returns usage" true
        (String.length s > 0)
  | None -> Alcotest.fail "expected text reply for cron edit nothing"

let test_cron_history () =
  Alcotest.check result_testable "/cron history"
    (Slash_commands.Cron (Slash_commands.CronHistory None))
    (Slash_commands.handle "/cron history")

let test_cron_history_name () =
  Alcotest.check result_testable "/cron history name"
    (Slash_commands.Cron (Slash_commands.CronHistory (Some "myjob")))
    (Slash_commands.handle "/cron history myjob")

let test_cron_show () =
  Alcotest.check result_testable "/cron show"
    (Slash_commands.Cron (Slash_commands.CronShow "myjob"))
    (Slash_commands.handle "/cron show myjob")

let test_cron_trigger () =
  Alcotest.check result_testable "/cron trigger"
    (Slash_commands.Cron (Slash_commands.CronTrigger "myjob"))
    (Slash_commands.handle "/cron trigger myjob")

let test_cron_run_alias () =
  Alcotest.check result_testable "/cron run"
    (Slash_commands.Cron (Slash_commands.CronTrigger "myjob"))
    (Slash_commands.handle "/cron run myjob")

let test_cron_help () =
  Alcotest.check result_testable "/cron help"
    (Slash_commands.Cron Slash_commands.CronHelp)
    (Slash_commands.handle "/cron help")

let test_cron_unknown_subcommand () =
  Alcotest.check result_testable "/cron unknown"
    (Slash_commands.Cron Slash_commands.CronHelp)
    (Slash_commands.handle "/cron blah blah blah")

let test_cron_in_commands_list () =
  let has_cron =
    List.exists
      (fun (c : Slash_commands.command) -> c.name = "cron")
      Slash_commands.commands
  in
  Alcotest.(check bool) "commands list includes /cron" true has_cron

let test_bl_list () =
  Alcotest.check result_testable "/bl" (Slash_commands.Bl Slash_commands.BlList)
    (Slash_commands.handle "/bl")

let test_bl_list_explicit () =
  Alcotest.check result_testable "/bl list"
    (Slash_commands.Bl Slash_commands.BlList)
    (Slash_commands.handle "/bl list")

let test_bl_backlog_alias () =
  Alcotest.check result_testable "/backlog"
    (Slash_commands.Bl Slash_commands.BlList)
    (Slash_commands.handle "/backlog")

let test_bl_bugs () =
  Alcotest.check result_testable "/bl bugs"
    (Slash_commands.Bl Slash_commands.BlBugs)
    (Slash_commands.handle "/bl bugs")

let test_bl_ideas () =
  Alcotest.check result_testable "/bl ideas"
    (Slash_commands.Bl Slash_commands.BlIdeas)
    (Slash_commands.handle "/bl ideas")

let test_bl_show () =
  Alcotest.check result_testable "/bl show B123"
    (Slash_commands.Bl (Slash_commands.BlShow "B123"))
    (Slash_commands.handle "/bl show B123")

let test_bl_show_bare_id () =
  Alcotest.check result_testable "/bl B456"
    (Slash_commands.Bl (Slash_commands.BlShow "B456"))
    (Slash_commands.handle "/bl B456")

let test_bl_in_commands_list () =
  let has_bl =
    List.exists
      (fun (c : Slash_commands.command) -> c.name = "bl")
      Slash_commands.commands
  in
  Alcotest.(check bool) "commands list includes /bl" true has_bl

(* ── Agent dispatch tests ───────────────────────────────────────────── *)

let test_agent_invoke () =
  Alcotest.check result_testable "/agent reviewer check code"
    (Slash_commands.AgentInvoke ("reviewer", "check code"))
    (Slash_commands.handle "/agent reviewer check code")

let test_agent_invoke_multi_word () =
  Alcotest.check result_testable "/agent coder implement the thing"
    (Slash_commands.AgentInvoke ("coder", "implement the thing"))
    (Slash_commands.handle "/agent coder implement the thing")

let test_agent_list () =
  match extract_text (Slash_commands.handle "/agent list") with
  | Some _s -> () (* just ensure it returns a FormattedReply *)
  | None -> Alcotest.fail "expected text reply for /agent list"

let test_agent_usage () =
  match extract_text (Slash_commands.handle "/agent") with
  | Some s ->
      Alcotest.(check bool)
        "mentions /agent" true
        (Test_helpers.string_contains s "/agent")
  | None -> Alcotest.fail "expected text reply for /agent usage"

let test_agent_name_no_prompt () =
  match extract_text (Slash_commands.handle "/agent reviewer") with
  | Some s ->
      Alcotest.(check bool)
        "mentions /agent" true
        (Test_helpers.string_contains s "/agent")
  | None -> Alcotest.fail "expected text reply for /agent name-only"

let test_delegate_with_agent () =
  Alcotest.check result_testable "/delegate @reviewer check it"
    (Slash_commands.Delegate (Some "reviewer", "check it"))
    (Slash_commands.handle "/delegate @reviewer check it")

let test_delegate_without_agent () =
  Alcotest.check result_testable "/delegate do the thing"
    (Slash_commands.Delegate (None, "do the thing"))
    (Slash_commands.handle "/delegate do the thing")

let test_fork_and_with_agent () =
  Alcotest.check result_testable "/fork_and @coder build it"
    (Slash_commands.ForkAnd (Some "coder", "build it"))
    (Slash_commands.handle "/fork_and @coder build it")

let test_fork_and_without_agent () =
  Alcotest.check result_testable "/fork-and review code"
    (Slash_commands.ForkAnd (None, "review code"))
    (Slash_commands.handle "/fork-and review code")

let test_bg_create_with_agent () =
  Alcotest.check result_testable "/bg create @coder implement X"
    (Slash_commands.Bg (Slash_commands.BgCreate (Some "coder", "implement X")))
    (Slash_commands.handle "/bg create @coder implement X")

let test_bg_create_without_agent () =
  Alcotest.check result_testable "/bg create fix the bug"
    (Slash_commands.Bg (Slash_commands.BgCreate (None, "fix the bug")))
    (Slash_commands.handle "/bg create fix the bug")

let test_bg_new_with_agent () =
  Alcotest.check result_testable "/bg new @reviewer review"
    (Slash_commands.Bg (Slash_commands.BgCreate (Some "reviewer", "review")))
    (Slash_commands.handle "/bg new @reviewer review")

let test_agent_mention_parsing () =
  let available = [ "reviewer"; "coder"; "team-lead" ] in
  let result =
    Group_chat_filter.parse_agent_mention ~available_agents:available
      "@reviewer check this code"
  in
  Alcotest.(check (option (pair string string)))
    "@reviewer parsed"
    (Some ("reviewer", "check this code"))
    result

let test_agent_mention_no_match () =
  let available = [ "reviewer"; "coder" ] in
  let result =
    Group_chat_filter.parse_agent_mention ~available_agents:available
      "@unknown do something"
  in
  Alcotest.(check (option (pair string string)))
    "@unknown not matched" None result

let test_agent_mention_case_insensitive () =
  let available = [ "Reviewer"; "Coder" ] in
  let result =
    Group_chat_filter.parse_agent_mention ~available_agents:available
      "@reviewer check this"
  in
  Alcotest.(check (option (pair string string)))
    "case insensitive match"
    (Some ("Reviewer", "check this"))
    result

let test_agent_mention_no_prompt () =
  let available = [ "reviewer" ] in
  let result =
    Group_chat_filter.parse_agent_mention ~available_agents:available
      "@reviewer"
  in
  Alcotest.(check (option (pair string string)))
    "@reviewer with no prompt"
    (Some ("reviewer", ""))
    result

let test_agent_mention_not_at_prefix () =
  let available = [ "reviewer" ] in
  let result =
    Group_chat_filter.parse_agent_mention ~available_agents:available
      "hello @reviewer"
  in
  Alcotest.(check (option (pair string string))) "not at start" None result

let test_agent_menu () =
  Alcotest.check result_testable "/agent menu" (Slash_commands.AgentMenu 1)
    (Slash_commands.handle "/agent menu")

let test_agent_menu_page () =
  Alcotest.check result_testable "/agent menu 2" (Slash_commands.AgentMenu 2)
    (Slash_commands.handle "/agent menu 2")

let test_agent_menu_page_invalid () =
  match Slash_commands.handle "/agent menu abc" with
  | Slash_commands.FormattedReply fn ->
      let text = fn Format_adapter.Plain in
      Alcotest.(check bool)
        "invalid page shows usage" true
        (String.length text > 0)
  | _ -> Alcotest.fail "expected FormattedReply for invalid page"

let test_agent_menu_pagination_format () =
  (* With 11 builtins and agents_per_page=8, we should get 2 pages *)
  let text =
    Slash_commands_fmt.format_agent_menu ~connector:Format_adapter.Plain ~page:1
  in
  (* Page 1 should show page indicator and next nav *)
  let has_page_indicator =
    try
      let _ = Str.search_forward (Str.regexp_string "(1/") text 0 in
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "page 1 has page indicator" true has_page_indicator;
  let has_next =
    try
      let _ = Str.search_forward (Str.regexp_string "/agent menu 2") text 0 in
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "page 1 has next nav" true has_next;
  (* Page 2 should have prev nav *)
  let text2 =
    Slash_commands_fmt.format_agent_menu ~connector:Format_adapter.Plain ~page:2
  in
  let has_prev =
    try
      let _ = Str.search_forward (Str.regexp_string "/agent menu 1") text2 0 in
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "page 2 has prev nav" true has_prev

let test_model_menu () =
  Alcotest.check result_testable "/model menu" (Slash_commands.ModelMenu 1)
    (Slash_commands.handle "/model menu")

let test_model_menu_page () =
  Alcotest.check result_testable "/model menu 2" (Slash_commands.ModelMenu 2)
    (Slash_commands.handle "/model menu 2")

let test_thinking_menu () =
  Alcotest.check result_testable "/thinking menu" Slash_commands.ThinkingMenu
    (Slash_commands.handle "/thinking menu")

let test_config_menu () =
  Alcotest.check result_testable "/config menu"
    (Slash_commands.AdminRequired (Slash_commands.ConfigMenu 1))
    (Slash_commands.handle "/config menu")

let test_config_menu_page () =
  Alcotest.check result_testable "/config menu 2"
    (Slash_commands.AdminRequired (Slash_commands.ConfigMenu 2))
    (Slash_commands.handle "/config menu 2")

let test_skills_menu () =
  Alcotest.check result_testable "/skills" (Slash_commands.SkillsMenu 1)
    (Slash_commands.handle "/skills")

let test_skills_menu_page () =
  Alcotest.check result_testable "/skills 2" (Slash_commands.SkillsMenu 2)
    (Slash_commands.handle "/skills 2")

let test_costs_menu () =
  Alcotest.check result_testable "/costs menu" Slash_commands.CostsMenu
    (Slash_commands.handle "/costs menu")

let test_bg_menu () =
  Alcotest.check result_testable "/bg menu" Slash_commands.BgMenu
    (Slash_commands.handle "/bg menu")

let test_skills_in_commands_list () =
  let has_skills =
    List.exists
      (fun (c : Slash_commands.command) -> c.name = "skills")
      Slash_commands.commands
  in
  Alcotest.(check bool) "commands list includes /skills" true has_skills

let test_format_thinking_menu () =
  let text =
    Slash_commands_fmt.format_thinking_menu ~connector:Format_adapter.Plain
  in
  Alcotest.(check bool)
    "contains /thinking off" true
    (Test_helpers.string_contains text "/thinking off");
  Alcotest.(check bool)
    "contains /thinking high" true
    (Test_helpers.string_contains text "/thinking high")

let test_format_costs_menu () =
  let text =
    Slash_commands_fmt.format_costs_menu ~connector:Format_adapter.Plain
  in
  Alcotest.(check bool)
    "contains /costs session" true
    (Test_helpers.string_contains text "/costs session");
  Alcotest.(check bool)
    "contains /costs model" true
    (Test_helpers.string_contains text "/costs model")

let test_format_bg_menu () =
  let text =
    Slash_commands_fmt.format_bg_menu ~connector:Format_adapter.Plain
  in
  Alcotest.(check bool)
    "contains /bg list" true
    (Test_helpers.string_contains text "/bg list");
  Alcotest.(check bool)
    "contains /bg create" true
    (Test_helpers.string_contains text "/bg create")

let test_bg_menu_card_with_cancellable () =
  let cancellable = [ (1, "codex"); (3, "claude") ] in
  let card_json =
    Slash_commands_manifest.bg_menu_adaptive_card_json ~cancellable ()
  in
  let s = Yojson.Safe.to_string card_json in
  Alcotest.(check bool)
    "contains cancel #1" true
    (Test_helpers.string_contains s "Cancel #1 (codex)");
  Alcotest.(check bool)
    "contains /bg cancel 1" true
    (Test_helpers.string_contains s "/bg cancel 1");
  Alcotest.(check bool)
    "contains cancel #3" true
    (Test_helpers.string_contains s "Cancel #3 (claude)");
  Alcotest.(check bool)
    "contains /bg cancel 3" true
    (Test_helpers.string_contains s "/bg cancel 3");
  Alcotest.(check bool)
    "still has list button" true
    (Test_helpers.string_contains s "List Tasks")

let test_bg_menu_card_no_cancellable () =
  let card_json = Slash_commands_manifest.bg_menu_adaptive_card_json () in
  let s = Yojson.Safe.to_string card_json in
  Alcotest.(check bool)
    "has list button" true
    (Test_helpers.string_contains s "List Tasks");
  Alcotest.(check bool)
    "has create button" true
    (Test_helpers.string_contains s "Create Task");
  Alcotest.(check bool)
    "no cancel button" false
    (Test_helpers.string_contains s "Cancel #")

let test_format_config_menu () =
  let text =
    Slash_commands_fmt.format_config_menu ~connector:Format_adapter.Plain
      ~page:1
  in
  Alcotest.(check bool)
    "contains /config show" true
    (Test_helpers.string_contains text "/config show")

let test_format_model_menu () =
  let text =
    Slash_commands_fmt.format_model_menu ~connector:Format_adapter.Plain ~page:1
  in
  let has_fav_hint = Test_helpers.string_contains text "/model fav" in
  let has_model_set = Test_helpers.string_contains text "/model set" in
  Alcotest.(check bool)
    "contains /model fav (no favorites) or /model set (has favorites)" true
    (has_fav_hint || has_model_set)

let test_format_skills_menu () =
  let text =
    Slash_commands_fmt.format_skills_menu ~connector:Format_adapter.Plain
      ~page:1 ()
  in
  let has_no_skills = text = "No skills available." in
  let has_skills_heading = Test_helpers.string_contains text "Skills" in
  Alcotest.(check bool)
    "returns no-skills message or skills listing" true
    (has_no_skills || has_skills_heading)

let test_agent_in_commands_list () =
  let has_agent =
    List.exists
      (fun (c : Slash_commands.command) -> c.name = "agent")
      Slash_commands.commands
  in
  Alcotest.(check bool) "commands list includes /agent" true has_agent

let test_inject_connector_history_default () =
  Alcotest.(check result_testable)
    "default count 20" (Slash_commands.InjectConnectorHistory 20)
    (Slash_commands.handle "/inject_connector_history")

let test_inject_connector_history_explicit () =
  Alcotest.(check result_testable)
    "explicit count 30" (Slash_commands.InjectConnectorHistory 30)
    (Slash_commands.handle "/inject_connector_history 30")

let test_inject_connector_history_hyphen () =
  Alcotest.(check result_testable)
    "hyphenated form" (Slash_commands.InjectConnectorHistory 10)
    (Slash_commands.handle "/inject-connector-history 10")

let test_inject_connector_history_clamp_low () =
  Alcotest.(check result_testable)
    "clamp 0 to 1" (Slash_commands.InjectConnectorHistory 1)
    (Slash_commands.handle "/inject_connector_history 0")

let test_inject_connector_history_clamp_high () =
  Alcotest.(check result_testable)
    "clamp 200 to 128" (Slash_commands.InjectConnectorHistory 128)
    (Slash_commands.handle "/inject_connector_history 200")

let test_register_as_admin_otc_no_args () =
  Alcotest.(check result_testable)
    "no args" (Slash_commands.RegisterAsAdminOtc None)
    (Slash_commands.handle "/register_as_admin_otc")

let test_register_as_admin_otc_with_code () =
  Alcotest.(check result_testable)
    "with code" (Slash_commands.RegisterAsAdminOtc (Some "ABC123"))
    (Slash_commands.handle "/register_as_admin_otc ABC123")

let test_register_as_admin_otc_hyphen () =
  Alcotest.(check result_testable)
    "hyphen alias" (Slash_commands.RegisterAsAdminOtc None)
    (Slash_commands.handle "/register-as-admin-otc")

let test_register_as_admin_otc_too_many_args () =
  match Slash_commands.handle "/register_as_admin_otc A B" with
  | Slash_commands.FormattedReply _ -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected FormattedReply, got %s"
           (result_to_string other))

let test_config_is_admin_required () =
  match Slash_commands.handle "/config show" with
  | Slash_commands.AdminRequired _ -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected AdminRequired, got %s"
           (result_to_string other))

let test_debug_dump_chat_is_admin_required () =
  match Slash_commands.handle "/debug_dump_chat" with
  | Slash_commands.AdminRequired Slash_commands.DebugDumpChat -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected AdminRequired(DebugDumpChat), got %s"
           (result_to_string other))

let test_session_list () =
  Alcotest.check result_testable "/session"
    (Slash_commands.AdminRequired
       (Slash_commands.Session Slash_commands.SessionList))
    (Slash_commands.handle "/session")

let test_session_list_explicit () =
  Alcotest.check result_testable "/session list"
    (Slash_commands.AdminRequired
       (Slash_commands.Session Slash_commands.SessionList))
    (Slash_commands.handle "/session list")

let test_sessions_alias () =
  Alcotest.check result_testable "/sessions"
    (Slash_commands.AdminRequired
       (Slash_commands.Session Slash_commands.SessionList))
    (Slash_commands.handle "/sessions")

let test_session_show () =
  Alcotest.check result_testable "/session show mykey"
    (Slash_commands.AdminRequired
       (Slash_commands.Session (Slash_commands.SessionShow "mykey")))
    (Slash_commands.handle "/session show mykey")

let test_session_bad_args () =
  match Slash_commands.handle "/session bad args" with
  | Slash_commands.AdminRequired (Slash_commands.FormattedReply _) -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected AdminRequired(FormattedReply), got %s"
           (result_to_string other))

let test_session_in_commands_list () =
  let names =
    List.map
      (fun (cmd : Slash_commands.command) -> cmd.name)
      Slash_commands.commands
  in
  Alcotest.(check bool) "session in commands" true (List.mem "session" names)

(* ── /repo tests ─────────────────────────────────────────────────────── *)

let test_repo_status () =
  Alcotest.check result_testable "/repo status"
    (Slash_commands.Repo Slash_commands.RepoStatus)
    (Slash_commands.handle "/repo")

let test_repo_forget () =
  Alcotest.check result_testable "/repo forget"
    (Slash_commands.Repo Slash_commands.RepoForget)
    (Slash_commands.handle "/repo forget")

let test_repo_update () =
  Alcotest.check result_testable "/repo update"
    (Slash_commands.Repo Slash_commands.RepoUpdate)
    (Slash_commands.handle "/repo update")

let test_repo_pull_alias () =
  Alcotest.check result_testable "/repo pull"
    (Slash_commands.Repo Slash_commands.RepoUpdate)
    (Slash_commands.handle "/repo pull")

let test_repo_fetch_alias () =
  Alcotest.check result_testable "/repo fetch"
    (Slash_commands.Repo Slash_commands.RepoUpdate)
    (Slash_commands.handle "/repo fetch")

let test_repo_associate_url () =
  Alcotest.check result_testable "/repo https://github.com/user/repo.git"
    (Slash_commands.Repo
       (Slash_commands.RepoAssociate "https://github.com/user/repo.git"))
    (Slash_commands.handle "/repo https://github.com/user/repo.git")

let test_repo_associate_path () =
  Alcotest.check result_testable "/repo /home/user/project"
    (Slash_commands.Repo (Slash_commands.RepoAssociate "/home/user/project"))
    (Slash_commands.handle "/repo /home/user/project")

let test_repo_in_commands_list () =
  let names =
    List.map
      (fun (cmd : Slash_commands.command) -> cmd.name)
      Slash_commands.commands
  in
  Alcotest.(check bool) "repo in commands" true (List.mem "repo" names)

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
    Alcotest.test_case "debug status" `Quick test_debug_status;
    Alcotest.test_case "debug toggle" `Quick test_debug_toggle;
    Alcotest.test_case "debug invalid args" `Quick test_debug_invalid_args;
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
    Alcotest.test_case "/active" `Quick test_active;
    Alcotest.test_case "/bg list" `Quick test_bg_list;
    Alcotest.test_case "/bg list explicit" `Quick test_bg_list_explicit;
    Alcotest.test_case "/bg show <id>" `Quick test_bg_show;
    Alcotest.test_case "/bg <id> bare" `Quick test_bg_show_bare_id;
    Alcotest.test_case "/bg logs <id>" `Quick test_bg_logs;
    Alcotest.test_case "/bg cancel <id>" `Quick test_bg_cancel;
    Alcotest.test_case "/bg stop alias" `Quick test_bg_stop_alias;
    Alcotest.test_case "/bg cancel bare" `Quick test_bg_cancel_bare;
    Alcotest.test_case "/bg stop bare" `Quick test_bg_stop_bare;
    Alcotest.test_case "/bg retry <id>" `Quick test_bg_retry;
    Alcotest.test_case "/bg finalize <id>" `Quick test_bg_finalize;
    Alcotest.test_case "/bg finalize invalid id" `Quick
      test_bg_finalize_invalid_id;
    Alcotest.test_case "/bg invalid id" `Quick test_bg_invalid_id;
    Alcotest.test_case "/bg unknown subcommand" `Quick
      test_bg_unknown_subcommand;
    Alcotest.test_case "/background alias" `Quick test_bg_background_alias;
    Alcotest.test_case "/bg log alias" `Quick test_bg_log_alias;
    Alcotest.test_case "/bg create" `Quick test_bg_create;
    Alcotest.test_case "/bg start alias" `Quick test_bg_start_alias;
    Alcotest.test_case "/bg create empty prompt" `Quick
      test_bg_create_empty_prompt;
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
    Alcotest.test_case "/model list provider and availability flags" `Quick
      test_model_list_provider_and_availability_flags;
    Alcotest.test_case "/model list malformed provider flags show usage" `Quick
      test_model_list_malformed_provider_flags_show_usage;
    Alcotest.test_case "/debug_dump_chat returns DebugDumpChat" `Quick
      test_debug_dump_chat_command;
    Alcotest.test_case "/tools returns Tools" `Quick test_tools_command;
    Alcotest.test_case "/memories default" `Quick test_memories_default;
    Alcotest.test_case "/memories oldest" `Quick test_memories_oldest;
    Alcotest.test_case "/memories page" `Quick test_memories_page;
    Alcotest.test_case "/memories oldest page" `Quick test_memories_oldest_page;
    Alcotest.test_case "/memories invalid args" `Quick
      test_memories_invalid_args;
    Alcotest.test_case "/memories format empty" `Quick
      test_memories_format_empty;
    Alcotest.test_case "/memories format ordering" `Quick
      test_memories_format_ordering;
    Alcotest.test_case "/memories format pagination" `Quick
      test_memories_format_pagination;
    Alcotest.test_case "/memories format escapes telegram html" `Quick
      test_memories_format_escapes_telegram_html;
    Alcotest.test_case "/tasks returns Tasks" `Quick test_tasks_command;
    Alcotest.test_case "/tasks@bot returns Tasks" `Quick
      test_tasks_command_with_telegram_bot_suffix;
    Alcotest.test_case "/tasks full returns TasksFull" `Quick
      test_tasks_full_command;
    Alcotest.test_case "/tasks invalid args" `Quick test_tasks_invalid_args;
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
    Alcotest.test_case "/model help shows usage" `Quick test_model_help;
    Alcotest.test_case "/model set-force with provider" `Quick
      test_model_set_force;
    Alcotest.test_case "/model set-force bare name" `Quick
      test_model_set_force_bare;
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
    Alcotest.test_case "format help with skills" `Quick
      test_format_help_with_skills;
    Alcotest.test_case "format help with agents" `Quick
      test_format_help_with_agents;
    Alcotest.test_case "format help teams skills bulleted" `Quick
      test_format_help_teams_skills_bulleted;
    Alcotest.test_case "format help teams agents bulleted" `Quick
      test_format_help_teams_agents_bulleted;
    Alcotest.test_case "format help plain skills no bullets" `Quick
      test_format_help_plain_skills_no_bullets;
    Alcotest.test_case "format tools with agents" `Quick
      test_format_tools_with_agents;
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
    Alcotest.test_case "is_admin_command auto detects" `Quick
      test_is_admin_command_auto_detects;
    Alcotest.test_case "sorted by priority filters admin" `Quick
      test_sorted_by_priority_filters_admin;
    Alcotest.test_case "format help hides admin for guest" `Quick
      test_format_help_hides_admin_commands_for_guest;
    Alcotest.test_case "format help shows admin for admin" `Quick
      test_format_help_shows_admin_commands_for_admin;
    Alcotest.test_case "commands has menu" `Quick test_commands_has_menu;
    Alcotest.test_case "manifest teams json" `Quick test_manifest_teams_json;
    Alcotest.test_case "manifest teams custom n" `Quick
      test_manifest_teams_json_custom_n;
    Alcotest.test_case "manifest telegram json" `Quick
      test_manifest_telegram_json;
    Alcotest.test_case "manifest teams uses title key" `Quick
      test_manifest_teams_uses_title_key;
    Alcotest.test_case "manifest teams top 10 composition" `Quick
      test_manifest_teams_top10_composition;
    Alcotest.test_case "manifest telegram uses command key" `Quick
      test_manifest_telegram_uses_command_key;
    Alcotest.test_case "menu adaptive card json" `Quick
      test_menu_adaptive_card_json;
    Alcotest.test_case "menu adaptive card pagination" `Quick
      test_menu_adaptive_card_pagination;
    Alcotest.test_case "/cron list" `Quick test_cron_list;
    Alcotest.test_case "/cron bare" `Quick test_cron_bare;
    Alcotest.test_case "/cron add" `Quick test_cron_add;
    Alcotest.test_case "/cron add multi-word message" `Quick
      test_cron_add_multi_word;
    Alcotest.test_case "/cron add with --ttl" `Quick test_cron_add_with_ttl;
    Alcotest.test_case "/cron edit with --ttl" `Quick test_cron_edit_ttl;
    Alcotest.test_case "/cron remove" `Quick test_cron_remove;
    Alcotest.test_case "/cron rm alias" `Quick test_cron_rm_alias;
    Alcotest.test_case "/cron edit schedule" `Quick test_cron_edit_schedule;
    Alcotest.test_case "/cron edit message" `Quick test_cron_edit_message;
    Alcotest.test_case "/cron edit nothing" `Quick test_cron_edit_nothing;
    Alcotest.test_case "/cron history" `Quick test_cron_history;
    Alcotest.test_case "/cron history name" `Quick test_cron_history_name;
    Alcotest.test_case "/cron show" `Quick test_cron_show;
    Alcotest.test_case "/cron trigger" `Quick test_cron_trigger;
    Alcotest.test_case "/cron run alias" `Quick test_cron_run_alias;
    Alcotest.test_case "/cron help" `Quick test_cron_help;
    Alcotest.test_case "/cron unknown subcommand" `Quick
      test_cron_unknown_subcommand;
    Alcotest.test_case "/cron in commands list" `Quick
      test_cron_in_commands_list;
    Alcotest.test_case "/bl list" `Quick test_bl_list;
    Alcotest.test_case "/bl list explicit" `Quick test_bl_list_explicit;
    Alcotest.test_case "/backlog alias" `Quick test_bl_backlog_alias;
    Alcotest.test_case "/bl bugs" `Quick test_bl_bugs;
    Alcotest.test_case "/bl ideas" `Quick test_bl_ideas;
    Alcotest.test_case "/bl show" `Quick test_bl_show;
    Alcotest.test_case "/bl show bare id" `Quick test_bl_show_bare_id;
    Alcotest.test_case "/bl in commands list" `Quick test_bl_in_commands_list;
    Alcotest.test_case "/agent invoke" `Quick test_agent_invoke;
    Alcotest.test_case "/agent invoke multi-word" `Quick
      test_agent_invoke_multi_word;
    Alcotest.test_case "/agent list" `Quick test_agent_list;
    Alcotest.test_case "/agent usage" `Quick test_agent_usage;
    Alcotest.test_case "/agent name no prompt" `Quick test_agent_name_no_prompt;
    Alcotest.test_case "/delegate @agent" `Quick test_delegate_with_agent;
    Alcotest.test_case "/delegate without agent" `Quick
      test_delegate_without_agent;
    Alcotest.test_case "/fork_and @agent" `Quick test_fork_and_with_agent;
    Alcotest.test_case "/fork-and without agent" `Quick
      test_fork_and_without_agent;
    Alcotest.test_case "/bg create @agent" `Quick test_bg_create_with_agent;
    Alcotest.test_case "/bg create without agent" `Quick
      test_bg_create_without_agent;
    Alcotest.test_case "/bg new @agent" `Quick test_bg_new_with_agent;
    Alcotest.test_case "@agent mention parsed" `Quick test_agent_mention_parsing;
    Alcotest.test_case "@agent mention no match" `Quick
      test_agent_mention_no_match;
    Alcotest.test_case "@agent mention case insensitive" `Quick
      test_agent_mention_case_insensitive;
    Alcotest.test_case "@agent mention no prompt" `Quick
      test_agent_mention_no_prompt;
    Alcotest.test_case "@agent mention not at prefix" `Quick
      test_agent_mention_not_at_prefix;
    Alcotest.test_case "/agent menu" `Quick test_agent_menu;
    Alcotest.test_case "/agent menu page" `Quick test_agent_menu_page;
    Alcotest.test_case "/agent menu invalid page" `Quick
      test_agent_menu_page_invalid;
    Alcotest.test_case "/agent menu pagination format" `Quick
      test_agent_menu_pagination_format;
    Alcotest.test_case "/agent in commands list" `Quick
      test_agent_in_commands_list;
    Alcotest.test_case "/model menu" `Quick test_model_menu;
    Alcotest.test_case "/model menu page" `Quick test_model_menu_page;
    Alcotest.test_case "/thinking menu" `Quick test_thinking_menu;
    Alcotest.test_case "/config menu" `Quick test_config_menu;
    Alcotest.test_case "/config menu page" `Quick test_config_menu_page;
    Alcotest.test_case "/skills" `Quick test_skills_menu;
    Alcotest.test_case "/skills page" `Quick test_skills_menu_page;
    Alcotest.test_case "/costs menu" `Quick test_costs_menu;
    Alcotest.test_case "/bg menu" `Quick test_bg_menu;
    Alcotest.test_case "/skills in commands list" `Quick
      test_skills_in_commands_list;
    Alcotest.test_case "format thinking menu" `Quick test_format_thinking_menu;
    Alcotest.test_case "format costs menu" `Quick test_format_costs_menu;
    Alcotest.test_case "format bg menu" `Quick test_format_bg_menu;
    Alcotest.test_case "bg menu card with cancellable" `Quick
      test_bg_menu_card_with_cancellable;
    Alcotest.test_case "bg menu card no cancellable" `Quick
      test_bg_menu_card_no_cancellable;
    Alcotest.test_case "format config menu" `Quick test_format_config_menu;
    Alcotest.test_case "format model menu" `Quick test_format_model_menu;
    Alcotest.test_case "format skills menu" `Quick test_format_skills_menu;
    Alcotest.test_case "/inject_connector_history default" `Quick
      test_inject_connector_history_default;
    Alcotest.test_case "/inject_connector_history 30" `Quick
      test_inject_connector_history_explicit;
    Alcotest.test_case "/inject-connector-history hyphen" `Quick
      test_inject_connector_history_hyphen;
    Alcotest.test_case "/inject_connector_history clamp low" `Quick
      test_inject_connector_history_clamp_low;
    Alcotest.test_case "/inject_connector_history clamp high" `Quick
      test_inject_connector_history_clamp_high;
    Alcotest.test_case "/register_as_admin_otc no args" `Quick
      test_register_as_admin_otc_no_args;
    Alcotest.test_case "/register_as_admin_otc with code" `Quick
      test_register_as_admin_otc_with_code;
    Alcotest.test_case "/register-as-admin-otc hyphen" `Quick
      test_register_as_admin_otc_hyphen;
    Alcotest.test_case "/register_as_admin_otc too many args" `Quick
      test_register_as_admin_otc_too_many_args;
    Alcotest.test_case "/config is AdminRequired" `Quick
      test_config_is_admin_required;
    Alcotest.test_case "/debug_dump_chat is AdminRequired" `Quick
      test_debug_dump_chat_is_admin_required;
    Alcotest.test_case "/bash ls -la" `Quick test_bash_command;
    Alcotest.test_case "/bash no args" `Quick test_bash_no_args;
    Alcotest.test_case "/bash multiword command" `Quick
      test_bash_multiword_command;
    Alcotest.test_case "/bash is AdminRequired" `Quick
      test_bash_is_admin_required;
    Alcotest.test_case "/bash pipes and chains" `Quick
      test_bash_pipes_and_chains;
    Alcotest.test_case "/bash in commands list" `Quick
      test_bash_in_commands_list;
    Alcotest.test_case "/session list" `Quick test_session_list;
    Alcotest.test_case "/session list explicit" `Quick
      test_session_list_explicit;
    Alcotest.test_case "/sessions alias" `Quick test_sessions_alias;
    Alcotest.test_case "/session show" `Quick test_session_show;
    Alcotest.test_case "/session bad args" `Quick test_session_bad_args;
    Alcotest.test_case "/session in commands list" `Quick
      test_session_in_commands_list;
    Alcotest.test_case "/repo status" `Quick test_repo_status;
    Alcotest.test_case "/repo forget" `Quick test_repo_forget;
    Alcotest.test_case "/repo update" `Quick test_repo_update;
    Alcotest.test_case "/repo pull alias" `Quick test_repo_pull_alias;
    Alcotest.test_case "/repo fetch alias" `Quick test_repo_fetch_alias;
    Alcotest.test_case "/repo associate url" `Quick test_repo_associate_url;
    Alcotest.test_case "/repo associate path" `Quick test_repo_associate_path;
    Alcotest.test_case "/repo in commands list" `Quick
      test_repo_in_commands_list;
  ]

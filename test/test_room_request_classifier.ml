let check_classification =
  Alcotest.testable
    (fun fmt c ->
      Format.fprintf fmt "%s"
        (match c with
        | Room_request_classifier.QuickReply -> "QuickReply"
        | AsyncCommand -> "AsyncCommand"
        | MentionToTask -> "MentionToTask"))
    ( = )

let classify result = Room_request_classifier.classify result

(* Quick-reply slash commands *)

let test_status_is_quick_reply () =
  Alcotest.check check_classification "status" QuickReply (classify Status)

let test_help_is_quick_reply () =
  Alcotest.check check_classification "help" QuickReply (classify Help)

let test_uptime_is_quick_reply () =
  Alcotest.check check_classification "uptime" QuickReply (classify Uptime)

let test_reset_is_quick_reply () =
  Alcotest.check check_classification "reset" QuickReply (classify Reset)

let test_reply_is_quick_reply () =
  Alcotest.check check_classification "reply" QuickReply
    (classify (Reply "hello"))

let test_formatted_reply_is_quick_reply () =
  Alcotest.check check_classification "formatted_reply" QuickReply
    (classify (FormattedReply (fun _ -> "hi")))

let test_thinking_show_is_quick_reply () =
  Alcotest.check check_classification "thinking_show" QuickReply
    (classify (Thinking ShowThinking))

let test_thinking_set_is_quick_reply () =
  Alcotest.check check_classification "thinking_set" QuickReply
    (classify (Thinking (SetThinking (Some "high"))))

let test_tools_is_quick_reply () =
  Alcotest.check check_classification "tools" QuickReply (classify Tools)

let test_tasks_is_quick_reply () =
  Alcotest.check check_classification "tasks" QuickReply (classify Tasks)

let test_runtime_ctx_is_quick_reply () =
  Alcotest.check check_classification "runtime_ctx" QuickReply
    (classify RuntimeCtx)

let test_menu_is_quick_reply () =
  Alcotest.check check_classification "menu" QuickReply (classify (Menu 1))

let test_agent_menu_is_quick_reply () =
  Alcotest.check check_classification "agent_menu" QuickReply
    (classify (AgentMenu 1))

let test_model_menu_is_quick_reply () =
  Alcotest.check check_classification "model_menu" QuickReply
    (classify (ModelMenu 1))

let test_thinking_menu_is_quick_reply () =
  Alcotest.check check_classification "thinking_menu" QuickReply
    (classify ThinkingMenu)

let test_config_menu_is_quick_reply () =
  Alcotest.check check_classification "config_menu" QuickReply
    (classify (ConfigMenu 1))

let test_skills_menu_is_quick_reply () =
  Alcotest.check check_classification "skills_menu" QuickReply
    (classify (SkillsMenu 1))

let test_costs_menu_is_quick_reply () =
  Alcotest.check check_classification "costs_menu" QuickReply
    (classify CostsMenu)

let test_bg_menu_is_quick_reply () =
  Alcotest.check check_classification "bg_menu" QuickReply (classify BgMenu)

let test_heartbeat_is_quick_reply () =
  Alcotest.check check_classification "heartbeat" QuickReply
    (classify (Heartbeat HeartbeatStatus))

let test_debug_is_quick_reply () =
  Alcotest.check check_classification "debug" QuickReply
    (classify (Debug DebugStatus))

let test_costs_is_quick_reply () =
  Alcotest.check check_classification "costs" QuickReply
    (classify (Costs CostsSummary))

let test_usage_is_quick_reply () =
  Alcotest.check check_classification "usage" QuickReply
    (classify (Usage UsageSummary))

let test_active_is_quick_reply () =
  Alcotest.check check_classification "active" QuickReply (classify Active)

let test_bg_is_quick_reply () =
  Alcotest.check check_classification "bg" QuickReply (classify (Bg BgList))

let test_cron_is_quick_reply () =
  Alcotest.check check_classification "cron" QuickReply
    (classify (Cron CronList))

let test_bl_is_quick_reply () =
  Alcotest.check check_classification "bl" QuickReply (classify (Bl BlList))

let test_session_is_quick_reply () =
  Alcotest.check check_classification "session" QuickReply
    (classify (Session SessionList))

let test_repo_is_quick_reply () =
  Alcotest.check check_classification "repo" QuickReply
    (classify (Repo RepoStatus))

let test_held_items_is_quick_reply () =
  Alcotest.check check_classification "held_items" QuickReply
    (classify (HeldItems (HeldItemsList false)))

let test_memories_is_quick_reply () =
  Alcotest.check check_classification "memories" QuickReply
    (classify (Memories { oldest = false; page = 1 }))

let test_inject_connector_history_is_quick_reply () =
  Alcotest.check check_classification "inject_connector_history" QuickReply
    (classify (InjectConnectorHistory 20))

let test_skill_invoke_is_quick_reply () =
  Alcotest.check check_classification "skill_invoke" QuickReply
    (classify (SkillInvoke ("test", "args")))

let test_register_as_admin_otc_is_quick_reply () =
  Alcotest.check check_classification "register_as_admin_otc" QuickReply
    (classify (RegisterAsAdminOtc None))

(* Async commands *)

let test_compact_is_async () =
  Alcotest.check check_classification "compact" AsyncCommand (classify Compact)

let test_delegate_is_async () =
  Alcotest.check check_classification "delegate" AsyncCommand
    (classify (Delegate (Some "agent", "do stuff")))

let test_delegate_no_agent_is_async () =
  Alcotest.check check_classification "delegate_no_agent" AsyncCommand
    (classify (Delegate (None, "do stuff")))

let test_fork_and_is_async () =
  Alcotest.check check_classification "fork_and" AsyncCommand
    (classify (ForkAnd (Some "agent", "prompt")))

let test_agent_invoke_is_async () =
  Alcotest.check check_classification "agent_invoke" AsyncCommand
    (classify (AgentInvoke ("agent", "prompt")))

let test_debate_is_async () =
  Alcotest.check check_classification "debate" AsyncCommand
    (classify (Debate "question"))

let test_bash_run_is_async () =
  Alcotest.check check_classification "bash_run" AsyncCommand
    (classify (BashRun "ls"))

let test_rig_list_is_async () =
  Alcotest.check check_classification "rig_list" AsyncCommand
    (classify (Rig RigList))

let test_rig_install_is_async () =
  Alcotest.check check_classification "rig_install" AsyncCommand
    (classify (Rig (RigInstall "name")))

let test_model_show_is_async () =
  Alcotest.check check_classification "model_show" AsyncCommand
    (classify (Model ModelShow))

let test_model_set_is_async () =
  Alcotest.check check_classification "model_set" AsyncCommand
    (classify (Model (ModelSet "gpt-4")))

(* Mention-to-task *)

let test_not_a_command_is_mention_to_task () =
  Alcotest.check check_classification "not_a_command" MentionToTask
    (classify NotACommand)

let test_debug_dump_chat_is_mention_to_task () =
  Alcotest.check check_classification "debug_dump_chat" MentionToTask
    (classify DebugDumpChat)

(* AdminRequired unwrapping *)

let test_admin_required_delegates_to_inner () =
  Alcotest.check check_classification "admin_required_status" QuickReply
    (classify (AdminRequired Status));
  Alcotest.check check_classification "admin_required_compact" AsyncCommand
    (classify (AdminRequired Compact));
  Alcotest.check check_classification "admin_required_not_a_command"
    MentionToTask
    (classify (AdminRequired NotACommand))

let test_show_thinking_is_quick_reply () =
  Alcotest.check check_classification "show_thinking" QuickReply
    (classify (ShowThinking ToggleShowThinking))

(* Guest async policy *)

let check_guest_policy =
  Alcotest.testable
    (fun fmt r ->
      Format.fprintf fmt "%s"
        (match r with Ok () -> "Ok" | Error msg -> "Error: " ^ msg))
    ( = )

let test_guest_delegate_allowed () =
  Alcotest.check check_guest_policy "delegate" (Ok ())
    (Room_request_classifier.guest_async_policy
       (Delegate (None, "build feature")))

let test_guest_agent_invoke_allowed () =
  Alcotest.check check_guest_policy "agent_invoke" (Ok ())
    (Room_request_classifier.guest_async_policy
       (AgentInvoke ("coder", "implement X")))

let test_guest_debate_allowed () =
  Alcotest.check check_guest_policy "debate" (Ok ())
    (Room_request_classifier.guest_async_policy (Debate "should we use X or Y?"))

let test_guest_fork_denied () =
  match Room_request_classifier.guest_async_policy (ForkAnd (None, "fork")) with
  | Ok () -> Alcotest.fail "expected denial for ForkAnd"
  | Error msg ->
      Alcotest.(check bool)
        "mentions admin" true
        (try
           ignore (Str.search_forward (Str.regexp_string "admin") msg 0);
           true
         with Not_found -> false)

let test_guest_bash_denied () =
  match Room_request_classifier.guest_async_policy (BashRun "ls") with
  | Ok () -> Alcotest.fail "expected denial for BashRun"
  | Error msg ->
      Alcotest.(check bool)
        "mentions admin" true
        (try
           ignore (Str.search_forward (Str.regexp_string "admin") msg 0);
           true
         with Not_found -> false)

let test_guest_rig_install_denied () =
  match
    Room_request_classifier.guest_async_policy (Rig (RigInstall "my-rig"))
  with
  | Ok () -> Alcotest.fail "expected denial for RigInstall"
  | Error _ -> ()

let test_guest_rig_adjust_denied () =
  match
    Room_request_classifier.guest_async_policy (Rig (RigAdjust "my-rig"))
  with
  | Ok () -> Alcotest.fail "expected denial for RigAdjust"
  | Error _ -> ()

let test_guest_rig_remove_denied () =
  match
    Room_request_classifier.guest_async_policy (Rig (RigRemove "my-rig"))
  with
  | Ok () -> Alcotest.fail "expected denial for RigRemove"
  | Error _ -> ()

let test_guest_compact_allowed () =
  Alcotest.check check_guest_policy "compact" (Ok ())
    (Room_request_classifier.guest_async_policy Compact)

let test_guest_not_a_command_allowed () =
  Alcotest.check check_guest_policy "not_a_command" (Ok ())
    (Room_request_classifier.guest_async_policy NotACommand)

let test_guest_admin_required_unwrapped () =
  Alcotest.check check_guest_policy "admin_required_delegate" (Ok ())
    (Room_request_classifier.guest_async_policy
       (AdminRequired (Delegate (None, "prompt"))))

let test_guest_admin_required_bash_denied () =
  match
    Room_request_classifier.guest_async_policy (AdminRequired (BashRun "ls"))
  with
  | Ok () -> Alcotest.fail "expected denial for AdminRequired BashRun"
  | Error _ -> ()

let suite =
  [
    (* Quick-reply commands *)
    Alcotest.test_case "status is quick reply" `Quick test_status_is_quick_reply;
    Alcotest.test_case "help is quick reply" `Quick test_help_is_quick_reply;
    Alcotest.test_case "uptime is quick reply" `Quick test_uptime_is_quick_reply;
    Alcotest.test_case "reset is quick reply" `Quick test_reset_is_quick_reply;
    Alcotest.test_case "reply is quick reply" `Quick test_reply_is_quick_reply;
    Alcotest.test_case "formatted_reply is quick reply" `Quick
      test_formatted_reply_is_quick_reply;
    Alcotest.test_case "thinking show is quick reply" `Quick
      test_thinking_show_is_quick_reply;
    Alcotest.test_case "thinking set is quick reply" `Quick
      test_thinking_set_is_quick_reply;
    Alcotest.test_case "tools is quick reply" `Quick test_tools_is_quick_reply;
    Alcotest.test_case "tasks is quick reply" `Quick test_tasks_is_quick_reply;
    Alcotest.test_case "runtime_ctx is quick reply" `Quick
      test_runtime_ctx_is_quick_reply;
    Alcotest.test_case "menu is quick reply" `Quick test_menu_is_quick_reply;
    Alcotest.test_case "agent_menu is quick reply" `Quick
      test_agent_menu_is_quick_reply;
    Alcotest.test_case "model_menu is quick reply" `Quick
      test_model_menu_is_quick_reply;
    Alcotest.test_case "thinking_menu is quick reply" `Quick
      test_thinking_menu_is_quick_reply;
    Alcotest.test_case "config_menu is quick reply" `Quick
      test_config_menu_is_quick_reply;
    Alcotest.test_case "skills_menu is quick reply" `Quick
      test_skills_menu_is_quick_reply;
    Alcotest.test_case "costs_menu is quick reply" `Quick
      test_costs_menu_is_quick_reply;
    Alcotest.test_case "bg_menu is quick reply" `Quick
      test_bg_menu_is_quick_reply;
    Alcotest.test_case "heartbeat is quick reply" `Quick
      test_heartbeat_is_quick_reply;
    Alcotest.test_case "debug is quick reply" `Quick test_debug_is_quick_reply;
    Alcotest.test_case "costs is quick reply" `Quick test_costs_is_quick_reply;
    Alcotest.test_case "usage is quick reply" `Quick test_usage_is_quick_reply;
    Alcotest.test_case "active is quick reply" `Quick test_active_is_quick_reply;
    Alcotest.test_case "bg is quick reply" `Quick test_bg_is_quick_reply;
    Alcotest.test_case "cron is quick reply" `Quick test_cron_is_quick_reply;
    Alcotest.test_case "bl is quick reply" `Quick test_bl_is_quick_reply;
    Alcotest.test_case "session is quick reply" `Quick
      test_session_is_quick_reply;
    Alcotest.test_case "repo is quick reply" `Quick test_repo_is_quick_reply;
    Alcotest.test_case "held_items is quick reply" `Quick
      test_held_items_is_quick_reply;
    Alcotest.test_case "memories is quick reply" `Quick
      test_memories_is_quick_reply;
    Alcotest.test_case "inject_connector_history is quick reply" `Quick
      test_inject_connector_history_is_quick_reply;
    Alcotest.test_case "skill_invoke is quick reply" `Quick
      test_skill_invoke_is_quick_reply;
    Alcotest.test_case "register_as_admin_otc is quick reply" `Quick
      test_register_as_admin_otc_is_quick_reply;
    Alcotest.test_case "show_thinking is quick reply" `Quick
      test_show_thinking_is_quick_reply;
    (* Async commands *)
    Alcotest.test_case "compact is async" `Quick test_compact_is_async;
    Alcotest.test_case "delegate is async" `Quick test_delegate_is_async;
    Alcotest.test_case "delegate no agent is async" `Quick
      test_delegate_no_agent_is_async;
    Alcotest.test_case "fork_and is async" `Quick test_fork_and_is_async;
    Alcotest.test_case "agent_invoke is async" `Quick test_agent_invoke_is_async;
    Alcotest.test_case "debate is async" `Quick test_debate_is_async;
    Alcotest.test_case "bash_run is async" `Quick test_bash_run_is_async;
    Alcotest.test_case "rig_list is async" `Quick test_rig_list_is_async;
    Alcotest.test_case "rig_install is async" `Quick test_rig_install_is_async;
    Alcotest.test_case "model_show is async" `Quick test_model_show_is_async;
    Alcotest.test_case "model_set is async" `Quick test_model_set_is_async;
    (* Mention-to-task *)
    Alcotest.test_case "not_a_command is mention_to_task" `Quick
      test_not_a_command_is_mention_to_task;
    Alcotest.test_case "debug_dump_chat is mention_to_task" `Quick
      test_debug_dump_chat_is_mention_to_task;
    (* AdminRequired unwrapping *)
    Alcotest.test_case "admin_required delegates to inner" `Quick
      test_admin_required_delegates_to_inner;
    (* Guest async policy *)
    Alcotest.test_case "guest delegate allowed" `Quick
      test_guest_delegate_allowed;
    Alcotest.test_case "guest agent_invoke allowed" `Quick
      test_guest_agent_invoke_allowed;
    Alcotest.test_case "guest debate allowed" `Quick test_guest_debate_allowed;
    Alcotest.test_case "guest fork_and denied" `Quick test_guest_fork_denied;
    Alcotest.test_case "guest bash_run denied" `Quick test_guest_bash_denied;
    Alcotest.test_case "guest rig_install denied" `Quick
      test_guest_rig_install_denied;
    Alcotest.test_case "guest rig_adjust denied" `Quick
      test_guest_rig_adjust_denied;
    Alcotest.test_case "guest rig_remove denied" `Quick
      test_guest_rig_remove_denied;
    Alcotest.test_case "guest compact allowed" `Quick test_guest_compact_allowed;
    Alcotest.test_case "guest not_a_command allowed" `Quick
      test_guest_not_a_command_allowed;
    Alcotest.test_case "guest admin_required delegate allowed" `Quick
      test_guest_admin_required_unwrapped;
    Alcotest.test_case "guest admin_required bash_run denied" `Quick
      test_guest_admin_required_bash_denied;
  ]

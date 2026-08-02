(* Cross-compatibility coverage for Claude Code and Codex lifecycle hooks. *)

let find_event hooks event =
  List.find_opt (fun (config : Hooks.hook_config) -> config.event = event) hooks

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let expect_string_field label expected json =
  match assoc_field label json with
  | Some (`String actual) -> Alcotest.(check string) label expected actual
  | _ -> Alcotest.failf "expected JSON string field %S" label

let expect_absent_field label json =
  Alcotest.(check bool)
    (label ^ " is absent") true
    (Option.is_none (assoc_field label json))

let expect_session_and_cwd ~session_id ~cwd json =
  expect_string_field "session_id" session_id json;
  expect_string_field "cwd" cwd json

let test_parse_claude_config_hooks_section () =
  let json =
    Yojson.Safe.from_string
      {|{
           "hooks": {
             "PreToolUse": [
               {
                 "matcher": "Bash|Write",
                 "hooks": [
                   {
                     "type": "command",
                     "command": "./scripts/guard.sh",
                     "timeout": 12,
                     "async": true
                   }
                 ]
               }
             ],
             "Stop": [
               {"hooks": [{"command": "./scripts/stop.sh"}]}
             ]
           }
         }|}
  in
  let hooks = Hooks.parse_hooks_config_section json in
  Alcotest.(check int) "two configured events" 2 (List.length hooks);
  match find_event hooks Hooks.PreToolUse with
  | Some { entries = [ { matcher = Some matcher; hooks = [ handler ] } ]; _ } ->
      Alcotest.(check string) "Claude matcher" "Bash|Write" matcher;
      Alcotest.(check string)
        "Claude command" "./scripts/guard.sh" handler.command;
      Alcotest.(check bool) "Claude async hook" true handler.async_flag;
      Alcotest.(check bool)
        "Claude command handler" true
        (handler.handler_type = Hooks.Command_handler);
      Alcotest.(check bool)
        "Claude timeout retained" true (handler.timeout = 12.0)
  | _ -> Alcotest.fail "expected Claude PreToolUse hook entry"

let test_parse_codex_root_hooks_file () =
  let json =
    Yojson.Safe.from_string
      {|{
           "PreToolUse": [
             {
               "matcher": "shell_exec",
               "hooks": [
                 {
                   "type": "command",
                   "command": ".clawq/hooks/guard.sh"
                 }
               ]
             }
           ],
           "SessionEnd": [
             {
               "hooks": [
                 {"type": "http", "url": "https://hooks.example/end"}
               ]
             }
           ]
         }|}
  in
  let hooks = Hooks.parse_hooks_file json in
  Alcotest.(check int) "two configured events" 2 (List.length hooks);
  match find_event hooks Hooks.SessionEnd with
  | Some { entries = [ { matcher = None; hooks = [ handler ] } ]; _ } ->
      Alcotest.(check bool)
        "Codex HTTP handler" true
        (handler.handler_type = Hooks.Http_handler);
      Alcotest.(check string)
        "Codex URL" "https://hooks.example/end" handler.command
  | _ -> Alcotest.fail "expected Codex SessionEnd hook entry"

let test_matchers_filter_entries_for_the_event () =
  let handler : Hooks.hook_handler =
    {
      handler_type = Hooks.Command_handler;
      command = "true";
      timeout = 1.0;
      async_flag = false;
      headers = [];
      allowed_env_vars = [];
    }
  in
  let hooks : Hooks.hook_config list =
    [
      {
        event = Hooks.PreToolUse;
        entries =
          [
            { matcher = Some "Bash|Write|Edit"; hooks = [ handler ] };
            { matcher = None; hooks = [ handler ] };
          ];
      };
      {
        event = Hooks.Stop;
        entries = [ { matcher = Some "Bash"; hooks = [ handler ] } ];
      };
    ]
  in
  Alcotest.(check bool)
    "pipe alternatives match" true
    (Hooks.matches_tool_name "Bash|Write|Edit" "Write");
  Alcotest.(check bool)
    "wildcard matches" true
    (Hooks.matches_tool_name "*" "shell_exec");
  Alcotest.(check bool)
    "unmatched tool is rejected" false
    (Hooks.matches_tool_name "Bash|Write|Edit" "Read");
  let matched =
    Hooks.hooks_for_event hooks Hooks.PreToolUse ~tool_name:"Write" ()
  in
  (match matched with
  | [ { entries; _ } ] ->
      Alcotest.(check int)
        "specific and catch-all entries run" 2 (List.length entries)
  | _ -> Alcotest.fail "expected only the requested event");
  let unmatched =
    Hooks.hooks_for_event hooks Hooks.PreToolUse ~tool_name:"Read" ()
  in
  match unmatched with
  | [ { entries; _ } ] ->
      Alcotest.(check int) "only catch-all entry runs" 1 (List.length entries)
  | _ -> Alcotest.fail "expected PreToolUse config for unmatched tool"

let test_build_event_payloads () =
  let tool_input = `Assoc [ ("command", `String "git status") ] in
  let tool_response = `Assoc [ ("exit_code", `Int 0) ] in
  let tool_payload =
    Hooks.build_tool_payload ~tool_name:"shell_exec" ~tool_input ~tool_response
      ~session_id:"session-42" ~cwd:"/repo" ~workspace:"/workspace" ()
  in
  expect_string_field "tool_name" "shell_exec" tool_payload;
  expect_session_and_cwd ~session_id:"session-42" ~cwd:"/repo" tool_payload;
  expect_string_field "workspace" "/workspace" tool_payload;
  Alcotest.(check bool)
    "tool input preserved" true
    (assoc_field "tool_input" tool_payload = Some tool_input);
  Alcotest.(check bool)
    "tool response preserved" true
    (assoc_field "tool_response" tool_payload = Some tool_response);
  let minimal_tool_payload =
    Hooks.build_tool_payload ~tool_name:"shell_exec" ~tool_input
      ~session_id:"session-42" ~cwd:"/repo" ()
  in
  expect_absent_field "tool_response" minimal_tool_payload;
  expect_absent_field "workspace" minimal_tool_payload;
  let prompt_payload =
    Hooks.build_prompt_payload ~prompt:"Summarize this" ~session_id:"session-42"
      ~cwd:"/repo" ()
  in
  expect_string_field "prompt" "Summarize this" prompt_payload;
  expect_session_and_cwd ~session_id:"session-42" ~cwd:"/repo" prompt_payload;
  expect_absent_field "workspace" prompt_payload;
  let prompt_with_workspace =
    Hooks.build_prompt_payload ~prompt:"Summarize this" ~session_id:"session-42"
      ~cwd:"/repo" ~workspace:"/workspace" ()
  in
  expect_string_field "workspace" "/workspace" prompt_with_workspace;
  let session_payload =
    Hooks.build_session_payload ~session_id:"session-42" ~cwd:"/repo"
      ~workspace:"/workspace" ~reason:"completed" ()
  in
  expect_session_and_cwd ~session_id:"session-42" ~cwd:"/repo" session_payload;
  expect_string_field "workspace" "/workspace" session_payload;
  expect_string_field "reason" "completed" session_payload;
  let minimal_session_payload =
    Hooks.build_session_payload ~session_id:"session-42" ~cwd:"/repo" ()
  in
  expect_absent_field "workspace" minimal_session_payload;
  expect_absent_field "reason" minimal_session_payload;
  let error_payload =
    Hooks.build_error_payload ~error_type:"provider_error"
      ~error_message:"rate limited" ~session_id:"session-42" ~cwd:"/repo"
      ~workspace:"/workspace" ()
  in
  expect_session_and_cwd ~session_id:"session-42" ~cwd:"/repo" error_payload;
  expect_string_field "workspace" "/workspace" error_payload;
  expect_string_field "error_type" "provider_error" error_payload;
  expect_string_field "error_message" "rate limited" error_payload;
  let minimal_error_payload =
    Hooks.build_error_payload ~error_type:"provider_error"
      ~error_message:"rate limited" ~session_id:"session-42" ~cwd:"/repo" ()
  in
  expect_absent_field "workspace" minimal_error_payload

let test_all_lifecycle_events_select_configuration () =
  let handler : Hooks.hook_handler =
    {
      handler_type = Hooks.Command_handler;
      command = "true";
      timeout = 1.0;
      async_flag = false;
      headers = [];
      allowed_env_vars = [];
    }
  in
  let events =
    [
      (Hooks.PreToolUse, "PreToolUse");
      (Hooks.PostToolUse, "PostToolUse");
      (Hooks.PostToolUseFailure, "PostToolUseFailure");
      (Hooks.UserPromptSubmit, "UserPromptSubmit");
      (Hooks.Stop, "Stop");
      (Hooks.SessionStart, "SessionStart");
      (Hooks.SessionEnd, "SessionEnd");
      (Hooks.PreCompact, "PreCompact");
      (Hooks.PostCompact, "PostCompact");
      (Hooks.OnError, "OnError");
    ]
  in
  List.iter
    (fun (event, name) ->
      Alcotest.(check string)
        "event name" name
        (Hooks.hook_event_to_string event);
      Alcotest.(check (option bool))
        "event name parses" (Some true)
        (Option.map
           (fun parsed -> parsed = event)
           (Hooks.hook_event_of_string name));
      let hooks : Hooks.hook_config list =
        [ { event; entries = [ { matcher = None; hooks = [ handler ] } ] } ]
      in
      match Hooks.hooks_for_event hooks event () with
      | [ { entries = [ _ ]; _ } ] -> ()
      | _ -> Alcotest.failf "expected selected %s hook" name)
    events

let test_parse_claude_and_codex_json_output () =
  let claude_output =
    {|{
         "hookSpecificOutput": {
           "hookEventName": "PreToolUse",
           "permissionDecision": "deny",
           "permissionDecisionReason": "destructive command",
           "additionalContext": "Use a read-only command instead.",
           "updatedInput": {"command": "git status"}
         }
       }|}
  in
  (match Hooks.parse_hook_json_output claude_output with
  | Some
      {
        decision = Some (Hooks.Hook_block reason);
        decision_reason = Some decision_reason;
        additional_context = Some context;
        updated_input = Some updated_input;
      } ->
      Alcotest.(check string)
        "Claude denial reason" "destructive command" reason;
      Alcotest.(check string)
        "Claude decision reason" "destructive command" decision_reason;
      Alcotest.(check string)
        "Claude additional context" "Use a read-only command instead." context;
      expect_string_field "command" "git status" updated_input
  | _ -> Alcotest.fail "expected parsed Claude hook-specific output");
  let codex_output =
    {|{"decision":"allow","additionalContext":"Codex hook completed"}|}
  in
  match Hooks.parse_hook_json_output codex_output with
  | Some
      { decision = Some Hooks.Hook_allow; additional_context = Some context; _ }
    ->
      Alcotest.(check string)
        "Codex additional context" "Codex hook completed" context
  | _ -> Alcotest.fail "expected parsed Codex hook output"

let test_exit_code_aggregation_semantics () =
  let base_result ~exit_code ~stderr_text ?json_output () : Hooks.hook_result =
    { exit_code; stdout_text = ""; stderr_text; json_output; timed_out = false }
  in
  let blocked =
    Hooks.aggregate_results
      [ base_result ~exit_code:2 ~stderr_text:"blocked by policy" () ]
  in
  Alcotest.(check bool) "exit code 2 blocks" true blocked.blocked;
  Alcotest.(check (option string))
    "exit code 2 keeps stderr as reason" (Some "blocked by policy")
    blocked.block_reason;
  let failure =
    Hooks.aggregate_results
      [ base_result ~exit_code:1 ~stderr_text:"hook failed" () ]
  in
  Alcotest.(check bool) "ordinary failures continue" false failure.blocked;
  Alcotest.(check (list string))
    "ordinary failure is reported" [ "hook failed" ] failure.errors;
  let json_block =
    {
      Hooks.decision = Some (Hooks.Hook_block "JSON policy denied this");
      decision_reason = None;
      additional_context = Some "explain the denial";
      updated_input = None;
    }
  in
  let successful_json_block =
    Hooks.aggregate_results
      [ base_result ~exit_code:0 ~stderr_text:"" ~json_output:json_block () ]
  in
  Alcotest.(check bool)
    "JSON denial blocks even with successful exit" true
    successful_json_block.blocked;
  Alcotest.(check (list string))
    "JSON context is retained" [ "explain the denial" ]
    successful_json_block.additional_contexts

let test_claude_style_command_hook_receives_payload () =
  let handler : Hooks.hook_handler =
    {
      handler_type = Hooks.Command_handler;
      command =
        {|if [ "$CLAWQ_SESSION_ID" != "session-claude" ] || [ "$CLAWQ_TOOL_NAME" != "Bash" ]; then
            printf '%s' 'missing lifecycle env' >&2
            exit 2
          fi
          payload=$(cat)
          case "$payload" in
            *'"tool_name":"Bash"'*)
              printf '%s' '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"Bash is disabled","additionalContext":"Use a safe tool."}}'
              ;;
            *) printf '%s' 'unexpected payload' >&2; exit 2 ;;
          esac|};
      timeout = 1.0;
      async_flag = false;
      headers = [];
      allowed_env_vars = [];
    }
  in
  let payload =
    Hooks.build_tool_payload ~tool_name:"Bash" ~tool_input:(`Assoc [])
      ~session_id:"session-claude" ~cwd:"/repo" ()
    |> Yojson.Safe.to_string
  in
  let result =
    Lwt_main.run
      (Hooks_exec.run_command_hook handler payload
         ~env_vars:
           (Hooks_exec.clawq_env_vars ~cwd:"/repo" ~workspace:"/repo"
              ~session_id:"session-claude" ~tool_name:"Bash" ()))
  in
  Alcotest.(check int) "Claude hook exits successfully" 0 result.exit_code;
  match result.json_output with
  | Some
      {
        decision = Some (Hooks.Hook_block reason);
        additional_context = Some context;
        _;
      } ->
      Alcotest.(check string) "Claude script denial" "Bash is disabled" reason;
      Alcotest.(check string) "Claude script context" "Use a safe tool." context
  | _ -> Alcotest.fail "expected Claude script JSON output"

let test_codex_root_config_dispatches_script () =
  let config =
    Yojson.Safe.from_string
      {|{
           "PreToolUse": [
             {
               "matcher": "shell_exec",
               "hooks": [
                 {
                   "command": "if [ \"$CLAWQ_SESSION_ID\" != \"session-codex\" ] || [ \"$CLAWQ_TOOL_NAME\" != \"shell_exec\" ]; then exit 2; fi; payload=$(cat); case \"$payload\" in *'\"tool_name\":\"shell_exec\"'*) printf '%s' '{\"decision\":\"allow\",\"additionalContext\":\"Codex hook ran\"}' ;; *) exit 2 ;; esac"
                 }
               ]
             }
           ]
         }|}
  in
  let hooks = Hooks.parse_hooks_file config in
  let payload =
    Hooks.build_tool_payload ~tool_name:"shell_exec" ~tool_input:(`Assoc [])
      ~session_id:"session-codex" ~cwd:"/repo" ()
  in
  let result =
    Lwt_main.run
      (Hooks_exec.dispatch ~all_hooks:hooks ~event:Hooks.PreToolUse
         ~session_id:"session-codex" ~tool_name:"shell_exec" ~payload
         ~cwd:"/repo" ~workspace:"/repo" ())
  in
  Alcotest.(check bool) "Codex hook allows execution" false result.blocked;
  Alcotest.(check (list string))
    "Codex context reaches dispatch" [ "Codex hook ran" ]
    result.additional_contexts

let suite =
  [
    Alcotest.test_case "Claude config hooks section" `Quick
      test_parse_claude_config_hooks_section;
    Alcotest.test_case "Codex root hooks file" `Quick
      test_parse_codex_root_hooks_file;
    Alcotest.test_case "event and tool-name matching" `Quick
      test_matchers_filter_entries_for_the_event;
    Alcotest.test_case "event payload shapes" `Quick test_build_event_payloads;
    Alcotest.test_case "all lifecycle events select configuration" `Quick
      test_all_lifecycle_events_select_configuration;
    Alcotest.test_case "Claude and Codex JSON output" `Quick
      test_parse_claude_and_codex_json_output;
    Alcotest.test_case "exit code aggregation semantics" `Quick
      test_exit_code_aggregation_semantics;
    Alcotest.test_case "Claude command hook payload" `Quick
      test_claude_style_command_hook_receives_payload;
    Alcotest.test_case "Codex root config dispatch" `Quick
      test_codex_root_config_dispatches_script;
  ]

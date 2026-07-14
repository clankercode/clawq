let mock_notifier () =
  let sent = ref [] in
  let edited = ref [] in
  let deleted = ref [] in
  let notifier : Status_message.notifier =
    {
      send =
        (fun ?parse_mode:_ text ->
          sent := text :: !sent;
          Lwt.return "msg-1");
      edit =
        (fun id ?parse_mode:_ text ->
          edited := (id, text) :: !edited;
          Lwt.return None);
      delete =
        (fun id ->
          deleted := id :: !deleted;
          Lwt.return_unit);
    }
  in
  (notifier, sent, edited, deleted)

let default_agent_defaults =
  {
    Runtime_config.default.agent_defaults with
    show_thinking = true;
    show_tool_calls = true;
    tool_status_mode = "consolidated";
  }

let test_select_strategy_consolidated () =
  let strategy =
    Status_update.select_strategy ~agent_defaults:default_agent_defaults
      ~capabilities:(Some Connector_capabilities.discord) ()
  in
  Alcotest.(check bool)
    "consolidated" true
    (strategy = Status_update.Consolidated)

let test_select_strategy_individual () =
  let ad = { default_agent_defaults with tool_status_mode = "individual" } in
  let strategy =
    Status_update.select_strategy ~agent_defaults:ad
      ~capabilities:(Some Connector_capabilities.discord) ()
  in
  Alcotest.(check bool) "individual" true (strategy = Status_update.Individual)

let test_select_strategy_no_tool_calls () =
  let ad = { default_agent_defaults with show_tool_calls = false } in
  let strategy =
    Status_update.select_strategy ~agent_defaults:ad
      ~capabilities:(Some Connector_capabilities.discord) ()
  in
  Alcotest.(check bool) "individual" true (strategy = Status_update.Individual)

let test_select_strategy_buffered () =
  let strategy =
    Status_update.select_strategy ~agent_defaults:default_agent_defaults
      ~capabilities:(Some Connector_capabilities.plain) ()
  in
  Alcotest.(check bool) "buffered" true (strategy = Status_update.Buffered)

let test_consolidated_handler_tool_events () =
  let notifier, _sent, _edited, _deleted = mock_notifier () in
  let sm =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  let handler =
    Status_update.make_handler ~strategy:Consolidated
      ~notifier_factory:(Some (fun () -> sm))
      ~notify:(fun _ -> Lwt.return_unit)
      ~agent_defaults:default_agent_defaults ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       handler.on_chunk
         (Provider.ToolStart
            { id = "t1"; name = "file_read"; arguments = "{\"path\":\"a.ml\"}" })
     in
     let* () =
       handler.on_chunk
         (Provider.ToolResult
            {
              id = "t1";
              name = "file_read";
              result = "contents";
              is_error = false;
            })
     in
     handler.finalize ());
  let thinking = handler.get_thinking () in
  Alcotest.(check string) "no thinking" "" thinking

let test_consolidated_handler_thinking () =
  let notifier, _, _, _ = mock_notifier () in
  let sm =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  let handler =
    Status_update.make_handler ~strategy:Consolidated
      ~notifier_factory:(Some (fun () -> sm))
      ~notify:(fun _ -> Lwt.return_unit)
      ~agent_defaults:default_agent_defaults ~parse_mode:"Markdown" ()
  in
  Lwt_main.run (handler.on_chunk (Provider.ThinkingDelta "let me think"));
  Alcotest.(check string)
    "thinking captured" "let me think" (handler.get_thinking ())

let test_consolidated_handler_bash_output_delta_preview () =
  let notifier, _sent, edited, _deleted = mock_notifier () in
  let sm =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  let handler =
    Status_update.make_handler ~strategy:Consolidated
      ~notifier_factory:(Some (fun () -> sm))
      ~notify:(fun _ -> Lwt.return_unit)
      ~agent_defaults:default_agent_defaults ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       handler.on_chunk
         (Provider.ToolStart
            {
              id = "t1";
              name = "bash";
              arguments = "{\"command\":\"make test\"}";
            })
     in
     Alcotest.(check int) "no edits after start" 0 (List.length !edited);
     let* () =
       handler.on_chunk
         (Provider.ToolOutputDelta
            { id = "t1"; chunk = "line1\nline2\nline3\nline4\nline5\n" })
     in
     Alcotest.(check int)
       "output delta does not edit immediately" 0 (List.length !edited);
     let streaming_edit = Status_message.render sm in
     Alcotest.(check bool)
       "streaming preview shows recent output" true
       (Test_helpers.string_contains streaming_edit "line5");
     Alcotest.(check bool)
       "streaming preview keeps last few lines" true
       (Test_helpers.string_contains streaming_edit "line2");
     Alcotest.(check bool)
       "streaming preview drops older output" false
       (Test_helpers.string_contains streaming_edit "line1");
     let* () =
       handler.on_chunk
         (Provider.ToolResult
            {
              id = "t1";
              name = "bash";
              result = "exit_code: 0\nstdout:\ndone\nstderr:\n";
              is_error = false;
            })
     in
     let final_edit =
       match !edited with
       | (_, text) :: _ -> text
       | [] -> Alcotest.fail "expected edit after tool result"
     in
     Alcotest.(check bool)
       "final result clears streaming-only output" false
       (Test_helpers.string_contains final_edit "line5");
     handler.finalize ())

let test_consolidated_handler_ignores_irrelevant_output_deltas () =
  let notifier, _sent, edited, _deleted = mock_notifier () in
  let sm =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  let handler =
    Status_update.make_handler ~strategy:Consolidated
      ~notifier_factory:(Some (fun () -> sm))
      ~notify:(fun _ -> Lwt.return_unit)
      ~agent_defaults:default_agent_defaults ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       handler.on_chunk
         (Provider.ToolStart
            { id = "t1"; name = "file_read"; arguments = "{\"path\":\"a.ml\"}" })
     in
     let* () =
       handler.on_chunk
         (Provider.ToolOutputDelta { id = "t1"; chunk = "non-shell-output" })
     in
     Alcotest.(check int)
       "non-shell delta does not edit" 0 (List.length !edited);
     Alcotest.(check bool)
       "non-shell delta is not rendered" false
       (Test_helpers.string_contains (Status_message.render sm)
          "non-shell-output");
     let* () =
       handler.on_chunk
         (Provider.ToolResult
            { id = "t1"; name = "file_read"; result = "ok"; is_error = false })
     in
     let edits_after_result = List.length !edited in
     let* () =
       handler.on_chunk
         (Provider.ToolOutputDelta { id = "t1"; chunk = "late-output" })
     in
     Alcotest.(check int)
       "completed delta does not edit" edits_after_result (List.length !edited);
     Alcotest.(check bool)
       "completed delta is not rendered" false
       (Test_helpers.string_contains (Status_message.render sm) "late-output");
     handler.finalize ())

let test_individual_handler () =
  let messages = ref [] in
  let notify text =
    messages := text :: !messages;
    Lwt.return_unit
  in
  let handler =
    Status_update.make_handler ~strategy:Individual ~notifier_factory:None
      ~notify ~agent_defaults:default_agent_defaults ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       handler.on_chunk
         (Provider.ToolStart { id = "t1"; name = "file_read"; arguments = "{}" })
     in
     let* () =
       handler.on_chunk
         (Provider.ToolResult
            { id = "t1"; name = "file_read"; result = "ok"; is_error = false })
     in
     handler.finalize ());
  Alcotest.(check bool) "tool result sent" true (List.length !messages > 0)

let test_buffered_handler () =
  let messages = ref [] in
  let notify text =
    messages := text :: !messages;
    Lwt.return_unit
  in
  let handler =
    Status_update.make_handler ~strategy:Buffered ~notifier_factory:None ~notify
      ~agent_defaults:default_agent_defaults ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       handler.on_chunk
         (Provider.ToolStart { id = "t1"; name = "file_read"; arguments = "{}" })
     in
     (* No messages sent during streaming *)
     Alcotest.(check int)
       "no messages during streaming" 0 (List.length !messages);
     let* () =
       handler.on_chunk
         (Provider.ToolResult
            { id = "t1"; name = "file_read"; result = "ok"; is_error = false })
     in
     Alcotest.(check int) "still no messages" 0 (List.length !messages);
     handler.finalize ());
  Alcotest.(check int) "summary sent on finalize" 1 (List.length !messages);
  let msg = List.hd !messages in
  Alcotest.(check bool)
    "contains checkmark" true
    (try
       ignore (Str.search_forward (Str.regexp_string "\xe2\x9c\x93") msg 0);
       true
     with Not_found -> false)

let test_consolidated_handler_reset_creates_new_group () =
  let send_count = ref 0 in
  let all_sent = ref [] in
  let factory () =
    let notifier : Status_message.notifier =
      {
        send =
          (fun ?parse_mode:_ text ->
            incr send_count;
            let id = Printf.sprintf "msg-%d" !send_count in
            all_sent := (id, text) :: !all_sent;
            Lwt.return id);
        edit = (fun _id ?parse_mode:_ _text -> Lwt.return None);
        delete = (fun _id -> Lwt.return_unit);
      }
    in
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  let handler =
    Status_update.make_handler ~strategy:Consolidated
      ~notifier_factory:(Some factory)
      ~notify:(fun _ -> Lwt.return_unit)
      ~agent_defaults:default_agent_defaults ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       handler.on_chunk
         (Provider.ToolStart { id = "t1"; name = "file_read"; arguments = "{}" })
     in
     let* () =
       handler.on_chunk
         (Provider.ToolResult
            { id = "t1"; name = "file_read"; result = "ok"; is_error = false })
     in
     let* () = handler.reset () in
     let* () =
       handler.on_chunk
         (Provider.ToolStart
            { id = "t2"; name = "shell_exec"; arguments = "{}" })
     in
     let* () =
       handler.on_chunk
         (Provider.ToolResult
            {
              id = "t2";
              name = "shell_exec";
              result = "done";
              is_error = false;
            })
     in
     handler.finalize ());
  Alcotest.(check int) "two separate status messages sent" 2 !send_count;
  Alcotest.(check bool)
    "first status group mentions file_read" true
    (List.exists
       (fun (_, text) ->
         try
           ignore (Str.search_forward (Str.regexp_string "file_read") text 0);
           true
         with Not_found -> false)
       !all_sent);
  Alcotest.(check bool)
    "second status group mentions shell_exec" true
    (List.exists
       (fun (_, text) ->
         try
           ignore (Str.search_forward (Str.regexp_string "shell_exec") text 0);
           true
         with Not_found -> false)
       !all_sent)

let test_consolidated_fallback_without_factory () =
  let messages = ref [] in
  let notify text =
    messages := text :: !messages;
    Lwt.return_unit
  in
  let handler =
    Status_update.make_handler ~strategy:Consolidated ~notifier_factory:None
      ~notify ~agent_defaults:default_agent_defaults ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       handler.on_chunk
         (Provider.ToolStart { id = "t1"; name = "file_read"; arguments = "{}" })
     in
     let* () =
       handler.on_chunk
         (Provider.ToolResult
            { id = "t1"; name = "file_read"; result = "ok"; is_error = false })
     in
     handler.finalize ());
  Alcotest.(check bool)
    "falls back to individual" true
    (List.length !messages > 0)

let test_select_strategy_low_volume_forces_individual () =
  let strategy =
    Status_update.select_strategy ~agent_defaults:default_agent_defaults
      ~capabilities:(Some Connector_capabilities.discord) ~low_volume:true ()
  in
  Alcotest.(check bool)
    "low_volume forces Individual" true
    (strategy = Status_update.Individual)

let test_low_volume_visibility_settings () =
  let settings =
    Status_update.visibility_settings ~agent_defaults:default_agent_defaults
      ~low_volume:true ()
  in
  Alcotest.(check bool) "no thinking" false settings.show_thinking;
  Alcotest.(check bool)
    "tool path enabled for errors" true settings.show_tool_calls;
  Alcotest.(check bool) "no starts" false settings.notify_tool_starts;
  Alcotest.(check bool) "no successes" false settings.notify_tool_successes;
  Alcotest.(check bool)
    "shows_tool_status false" false
    (Status_update.shows_tool_status ~agent_defaults:default_agent_defaults
       ~low_volume:true ())

let test_low_volume_individual_suppresses_success_keeps_error () =
  let messages = ref [] in
  let notify text =
    messages := text :: !messages;
    Lwt.return_unit
  in
  let handler =
    Status_update.make_handler ~strategy:Individual ~notifier_factory:None
      ~notify ~agent_defaults:default_agent_defaults ~low_volume:true
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       handler.on_chunk
         (Provider.ToolStart
            { id = "t1"; name = "file_read"; arguments = "{\"path\":\"a\"}" })
     in
     let* () =
       handler.on_chunk
         (Provider.ToolResult
            {
              id = "t1";
              name = "file_read";
              result = "ok contents";
              is_error = false;
            })
     in
     let* () =
       handler.on_chunk
         (Provider.ToolStart
            { id = "t2"; name = "shell_exec"; arguments = "{}" })
     in
     let* () =
       handler.on_chunk
         (Provider.ToolResult
            { id = "t2"; name = "shell_exec"; result = "boom"; is_error = true })
     in
     let* () = handler.on_chunk (Provider.ThinkingDelta "should be muted") in
     handler.finalize ());
  Alcotest.(check bool) "thinking muted" true (handler.get_thinking () = "");
  Alcotest.(check bool)
    "success not notified" false
    (List.exists
       (fun text ->
         try
           ignore (Str.search_forward (Str.regexp_string "file_read") text 0);
           true
         with Not_found -> false)
       !messages);
  Alcotest.(check bool)
    "error still notified" true
    (List.exists
       (fun text ->
         try
           ignore (Str.search_forward (Str.regexp_string "shell_exec") text 0);
           true
         with Not_found -> false)
       !messages)

let tests =
  [
    Alcotest.test_case "select strategy consolidated" `Quick
      test_select_strategy_consolidated;
    Alcotest.test_case "select strategy individual" `Quick
      test_select_strategy_individual;
    Alcotest.test_case "select strategy no tool calls" `Quick
      test_select_strategy_no_tool_calls;
    Alcotest.test_case "select strategy buffered" `Quick
      test_select_strategy_buffered;
    Alcotest.test_case "select strategy low_volume forces individual" `Quick
      test_select_strategy_low_volume_forces_individual;
    Alcotest.test_case "low_volume visibility settings" `Quick
      test_low_volume_visibility_settings;
    Alcotest.test_case "low_volume suppresses success keeps error" `Quick
      test_low_volume_individual_suppresses_success_keeps_error;
    Alcotest.test_case "consolidated handler tool events" `Quick
      test_consolidated_handler_tool_events;
    Alcotest.test_case "consolidated handler thinking" `Quick
      test_consolidated_handler_thinking;
    Alcotest.test_case "consolidated handler bash output delta preview" `Quick
      test_consolidated_handler_bash_output_delta_preview;
    Alcotest.test_case "consolidated handler ignores irrelevant output deltas"
      `Quick test_consolidated_handler_ignores_irrelevant_output_deltas;
    Alcotest.test_case "individual handler" `Quick test_individual_handler;
    Alcotest.test_case "buffered handler" `Quick test_buffered_handler;
    Alcotest.test_case "consolidated handler resets on inject" `Quick
      test_consolidated_handler_reset_creates_new_group;
    Alcotest.test_case "consolidated fallback without factory" `Quick
      test_consolidated_fallback_without_factory;
  ]

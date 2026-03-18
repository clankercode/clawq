let string_contains s sub =
  try
    ignore (Str.search_forward (Str.regexp_string sub) s 0);
    true
  with Not_found -> false

let test_render_paragraph () =
  let doc = [ Content_dsl.Paragraph [ Text "hello "; Bold "world" ] ] in
  let result = Content_dsl.render_document Format_adapter.Discord doc in
  Alcotest.(check bool) "contains hello" true (string_contains result "hello");
  Alcotest.(check bool)
    "contains bold world" true
    (string_contains result "**world**")

let test_render_code_block () =
  let doc =
    [ Content_dsl.CodeBlock { language = Some "ocaml"; content = "let x = 1" } ]
  in
  let result = Content_dsl.render_document Format_adapter.Discord doc in
  Alcotest.(check bool)
    "has code fence" true
    (string_contains result "```ocaml");
  Alcotest.(check bool) "has content" true (string_contains result "let x = 1")

let test_render_tool_entry_done () =
  let doc =
    [
      Content_dsl.ToolEntry
        {
          emoji = "\xF0\x9F\x93\x96";
          name = "file_read";
          summary = Some "src/main.ml";
          state = Content_dsl.Done;
          timing = Some "2.5s";
          preview = Some "42 lines";
          error_detail = None;
          connector_char = None;
        };
    ]
  in
  let result = Content_dsl.render_document Format_adapter.Discord doc in
  Alcotest.(check bool)
    "has checkmark" true
    (string_contains result "\xe2\x9c\x93");
  Alcotest.(check bool)
    "has tool name" true
    (string_contains result "file_read");
  Alcotest.(check bool)
    "has summary" true
    (string_contains result "src/main.ml");
  Alcotest.(check bool) "has timing" true (string_contains result "2.5s");
  Alcotest.(check bool) "has preview" true (string_contains result "42 lines")

let test_render_tool_entry_failed () =
  let doc =
    [
      Content_dsl.ToolEntry
        {
          emoji = "\xF0\x9F\x94\xA7";
          name = "shell_exec";
          summary = Some "make build";
          state = Content_dsl.Failed;
          timing = None;
          preview = None;
          error_detail = Some "exit code 1";
          connector_char = None;
        };
    ]
  in
  let result = Content_dsl.render_document Format_adapter.Discord doc in
  Alcotest.(check bool)
    "has x mark" true
    (string_contains result "\xe2\x9c\x97");
  Alcotest.(check bool) "has error" true (string_contains result "exit code 1")

let test_render_tool_entry_running () =
  let doc =
    [
      Content_dsl.ToolEntry
        {
          emoji = "\xF0\x9F\x92\xBB";
          name = "shell_exec";
          summary = Some "make test";
          state = Content_dsl.Running;
          timing = Some "8s...";
          preview = None;
          error_detail = None;
          connector_char = Some "\xE2\x94\x97 ";
        };
    ]
  in
  let result = Content_dsl.render_document Format_adapter.Discord doc in
  Alcotest.(check bool)
    "has running dot" true
    (string_contains result "\xe2\x97\x89");
  Alcotest.(check bool)
    "has connector" true
    (string_contains result "\xE2\x94\x97")

let test_render_collapsed_tools () =
  let doc = [ Content_dsl.CollapsedTools { count = 5 } ] in
  let result = Content_dsl.render_document Format_adapter.Discord doc in
  Alcotest.(check bool)
    "has count" true
    (string_contains result "5 tools completed")

let test_render_progress_bar () =
  let doc =
    [ Content_dsl.ProgressBar { filled = 0; total = 8; done_count = 4 } ]
  in
  let result = Content_dsl.render_document Format_adapter.Discord doc in
  Alcotest.(check bool) "has progress" true (string_contains result "4/8")

let test_render_thinking_preview () =
  let doc = [ Content_dsl.ThinkingPreview "analyzing code structure" ] in
  let result = Content_dsl.render_document Format_adapter.Discord doc in
  Alcotest.(check bool)
    "has thinking emoji" true
    (string_contains result "\xF0\x9F\x92\xAD");
  Alcotest.(check bool)
    "has text" true
    (string_contains result "analyzing code structure")

let test_render_html_connector () =
  let doc =
    [ Content_dsl.Paragraph [ Bold "hello"; Text " "; Code "world" ] ]
  in
  let result = Content_dsl.render_document Format_adapter.Telegram_html doc in
  Alcotest.(check bool)
    "has HTML bold" true
    (string_contains result "<b>hello</b>");
  Alcotest.(check bool)
    "has HTML code" true
    (string_contains result "<code>world</code>")

let test_render_document_multi_block () =
  let doc =
    [
      Content_dsl.CollapsedTools { count = 3 };
      Content_dsl.ToolEntry
        {
          emoji = "\xF0\x9F\x93\x96";
          name = "file_read";
          summary = None;
          state = Content_dsl.Done;
          timing = None;
          preview = None;
          error_detail = None;
          connector_char = None;
        };
    ]
  in
  let result = Content_dsl.render_document Format_adapter.Discord doc in
  Alcotest.(check bool)
    "has collapsed" true
    (string_contains result "3 tools completed");
  Alcotest.(check bool) "has tool" true (string_contains result "file_read")

let test_render_separator () =
  let doc = [ Content_dsl.Separator ] in
  let result = Content_dsl.render_document Format_adapter.Discord doc in
  Alcotest.(check bool)
    "has separator chars" true
    (string_contains result "\xE2\x94\x81")

let test_teams_multi_block_line_breaks () =
  let doc =
    [
      Content_dsl.ToolEntry
        {
          emoji = "\xF0\x9F\x93\x96";
          name = "file_read";
          summary = Some "a.ml";
          state = Content_dsl.Done;
          timing = None;
          preview = None;
          error_detail = None;
          connector_char = None;
        };
      Content_dsl.ToolEntry
        {
          emoji = "\xF0\x9F\x93\x96";
          name = "file_read";
          summary = Some "b.ml";
          state = Content_dsl.Done;
          timing = None;
          preview = None;
          error_detail = None;
          connector_char = None;
        };
    ]
  in
  let result = Content_dsl.render_document Format_adapter.Teams doc in
  (* Teams markdown needs "  \n" (two trailing spaces) for line breaks *)
  Alcotest.(check bool)
    "has trailing-space line break" true
    (string_contains result "  \n");
  (* Plain "\n" without leading spaces should NOT appear between entries *)
  let lines = String.split_on_char '\n' result in
  let has_bare_newline =
    List.exists
      (fun line ->
        let len = String.length line in
        len >= 2
        && line.[len - 1] <> ' '
        && line.[len - 2] <> ' '
        && line <> List.nth lines (List.length lines - 1))
      (match lines with _ :: rest -> List.rev rest |> List.rev | [] -> [])
  in
  Alcotest.(check bool)
    "no bare newlines between entries" false has_bare_newline

let test_teams_failed_error_line_break () =
  let doc =
    [
      Content_dsl.ToolEntry
        {
          emoji = "\xF0\x9F\x94\xA7";
          name = "shell_exec";
          summary = Some "make build";
          state = Content_dsl.Failed;
          timing = None;
          preview = None;
          error_detail = Some "exit code 1";
          connector_char = None;
        };
    ]
  in
  let result = Content_dsl.render_document Format_adapter.Teams doc in
  Alcotest.(check bool)
    "error detail has trailing-space line break" true
    (string_contains result "  \n")

let test_render_question_block_telegram () =
  let doc =
    [
      Content_dsl.QuestionBlock
        {
          question_text = "Pick one";
          hint = None;
          options = [ (1, "Alpha"); (2, "Beta") ];
          instruction = Some "Reply with number or text";
        };
    ]
  in
  let result = Content_dsl.render_document Format_adapter.Telegram_html doc in
  Alcotest.(check bool)
    "has HTML bold" true
    (string_contains result "<b>Pick one</b>");
  Alcotest.(check bool)
    "has option 1 with code" true
    (string_contains result "<code>1</code>. Alpha");
  Alcotest.(check bool)
    "has option 2 with code" true
    (string_contains result "<code>2</code>. Beta");
  Alcotest.(check bool)
    "has italic instruction" true
    (string_contains result "<i>Reply with number or text</i>")

let test_render_question_block_discord () =
  let doc =
    [
      Content_dsl.QuestionBlock
        {
          question_text = "Confirm?";
          hint = Some "This is important";
          options = [];
          instruction = Some "Reply yes/no";
        };
    ]
  in
  let result = Content_dsl.render_document Format_adapter.Discord doc in
  Alcotest.(check bool)
    "has markdown bold" true
    (string_contains result "**Confirm?**");
  Alcotest.(check bool)
    "has hint" true
    (string_contains result "This is important");
  Alcotest.(check bool)
    "has instruction" true
    (string_contains result "Reply yes/no")

let test_render_question_block_plain () =
  let doc =
    [
      Content_dsl.QuestionBlock
        {
          question_text = "How many?";
          hint = None;
          options = [];
          instruction = Some "Reply with a number";
        };
    ]
  in
  let result = Content_dsl.render_document Format_adapter.Plain doc in
  Alcotest.(check bool)
    "has question (plain, no formatting)" true
    (string_contains result "How many?");
  Alcotest.(check bool)
    "has instruction (plain, no formatting)" true
    (string_contains result "Reply with a number")

let test_render_question_block_no_options () =
  let doc =
    [
      Content_dsl.QuestionBlock
        {
          question_text = "Enter text";
          hint = Some "hint here";
          options = [];
          instruction = None;
        };
    ]
  in
  let result = Content_dsl.render_document Format_adapter.Discord doc in
  Alcotest.(check bool)
    "has question" true
    (string_contains result "Enter text");
  Alcotest.(check bool) "has hint" true (string_contains result "hint here")

let tests =
  [
    Alcotest.test_case "paragraph" `Quick test_render_paragraph;
    Alcotest.test_case "code block" `Quick test_render_code_block;
    Alcotest.test_case "tool entry done" `Quick test_render_tool_entry_done;
    Alcotest.test_case "tool entry failed" `Quick test_render_tool_entry_failed;
    Alcotest.test_case "tool entry running" `Quick
      test_render_tool_entry_running;
    Alcotest.test_case "collapsed tools" `Quick test_render_collapsed_tools;
    Alcotest.test_case "progress bar" `Quick test_render_progress_bar;
    Alcotest.test_case "thinking preview" `Quick test_render_thinking_preview;
    Alcotest.test_case "HTML connector" `Quick test_render_html_connector;
    Alcotest.test_case "multi-block document" `Quick
      test_render_document_multi_block;
    Alcotest.test_case "separator" `Quick test_render_separator;
    Alcotest.test_case "Teams multi-block line breaks" `Quick
      test_teams_multi_block_line_breaks;
    Alcotest.test_case "Teams failed error line break" `Quick
      test_teams_failed_error_line_break;
    Alcotest.test_case "QuestionBlock Telegram" `Quick
      test_render_question_block_telegram;
    Alcotest.test_case "QuestionBlock Discord" `Quick
      test_render_question_block_discord;
    Alcotest.test_case "QuestionBlock plain" `Quick
      test_render_question_block_plain;
    Alcotest.test_case "QuestionBlock no options" `Quick
      test_render_question_block_no_options;
  ]

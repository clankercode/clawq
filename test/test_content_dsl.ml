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
  ]

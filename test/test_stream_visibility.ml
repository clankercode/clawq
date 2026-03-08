let test_tool_start_message_uses_summary () =
  let message =
    Stream_visibility.tool_start_message ~name:"file_read"
      ~summary:(Some "src/main.ml")
  in
  Alcotest.(check string)
    "tool start summary message" "\xF0\x9F\x94\xA7 file_read src/main.ml"
    message

let test_tool_call_message_error_includes_summary_and_truncates () =
  let result = String.make 300 'x' in
  let message =
    Stream_visibility.tool_call_message ~name:"shell_exec"
      ~summary:(Some "ls -la") ~result ~is_error:true
  in
  Alcotest.(check bool)
    "tool call error has prefix" true
    (String.starts_with
       ~prefix:"\xF0\x9F\x94\xA7 shell_exec \xE2\x9C\x97 ls -la - " message);
  Alcotest.(check bool)
    "tool call error truncated" true
    (String.ends_with ~suffix:"..." message)

let test_summarize_tool_arguments_uses_key_context () =
  Alcotest.(check (option string))
    "file_read shows path" (Some "src/main.ml")
    (Stream_visibility.summarize_tool_arguments ~name:"file_read"
       {|{"path":"src/main.ml","offset":10}|});
  Alcotest.(check (option string))
    "shell_exec shows command" (Some "git status")
    (Stream_visibility.summarize_tool_arguments ~name:"shell_exec"
       {|{"command":"git status"}|});
  Alcotest.(check (option string))
    "shell_exec shows command and cwd" (Some "git status in /tmp/wt")
    (Stream_visibility.summarize_tool_arguments ~name:"shell_exec"
       {|{"command":"git status","cwd":"/tmp/wt"}|});
  Alcotest.(check (option string))
    "generic tool shows labeled summary" (Some "query=status")
    (Stream_visibility.summarize_tool_arguments ~name:"search"
       {|{"query":"status","limit":5}|})

let test_summarize_tool_arguments_shows_file_edit_details () =
  Alcotest.(check (option string))
    "file_edit shows line delta" (Some "src/main.ml -1L/+2L all")
    (Stream_visibility.summarize_tool_arguments ~name:"file_edit"
       {|{"path":"src/main.ml","old_text":"a","new_text":"b
c","replace_all":true}|});
  Alcotest.(check (option string))
    "file_edit_lines shows range and delta" (Some "src/main.ml L10-12 -3L/+2L")
    (Stream_visibility.summarize_tool_arguments ~name:"file_edit_lines"
       {|{"path":"src/main.ml","start_line":10,"end_line":12,"content":"x
y"}|});
  Alcotest.(check (option string))
    "file_write shows added lines" (Some "src/main.ml write +2L")
    (Stream_visibility.summarize_tool_arguments ~name:"file_write"
       {|{"path":"src/main.ml","content":"x
y"}|});
  Alcotest.(check (option string))
    "file_append shows added lines" (Some "src/main.ml append +2L")
    (Stream_visibility.summarize_tool_arguments ~name:"file_append"
       {|{"path":"src/main.ml","content":"x
y"}|})

let test_summarize_tool_arguments_shows_other_tool_details () =
  Alcotest.(check (option string))
    "glob shows pattern and root" (Some "src/**/*.ml in src")
    (Stream_visibility.summarize_tool_arguments ~name:"glob"
       {|{"pattern":"src/**/*.ml","root":"src"}|});
  Alcotest.(check (option string))
    "grep shows pattern path and filter" (Some "tool call in src match *.ml")
    (Stream_visibility.summarize_tool_arguments ~name:"grep"
       {|{"pattern":"tool call","path":"src","file_glob":"*.ml"}|});
  Alcotest.(check (option string))
    "http request shows method and host" (Some "POST example.com/api")
    (Stream_visibility.summarize_tool_arguments ~name:"http_request"
       {|{"method":"post","url":"https://example.com/api"}|});
  Alcotest.(check (option string))
    "web search shows query and count" (Some "ocaml lwt x3")
    (Stream_visibility.summarize_tool_arguments ~name:"web_search"
       {|{"query":"ocaml lwt","limit":3}|});
  Alcotest.(check (option string))
    "memory store shows key and category" (Some "prefs [user]")
    (Stream_visibility.summarize_tool_arguments ~name:"memory_store"
       {|{"key":"prefs","content":"x","category":"user"}|});
  Alcotest.(check (option string))
    "git operations shows commit message" (Some "commit refine summaries")
    (Stream_visibility.summarize_tool_arguments ~name:"git_operations"
       {|{"operation":"commit","message":"refine summaries"}|});
  Alcotest.(check (option string))
    "doc write shows filename mode and lines" (Some "TOOLS.md append +2L")
    (Stream_visibility.summarize_tool_arguments ~name:"doc_write"
       {|{"filename":"TOOLS.md","append":true,"content":"a
b"}|})

let test_thinking_message_prefixes_content () =
  Alcotest.(check string)
    "thinking message" "Thinking:\nplan first"
    (Stream_visibility.thinking_message "plan first")

let suite =
  [
    Alcotest.test_case "tool call success message" `Quick
      test_tool_start_message_uses_summary;
    Alcotest.test_case "tool call error truncates" `Quick
      test_tool_call_message_error_includes_summary_and_truncates;
    Alcotest.test_case "tool argument summaries" `Quick
      test_summarize_tool_arguments_uses_key_context;
    Alcotest.test_case "file edit summaries" `Quick
      test_summarize_tool_arguments_shows_file_edit_details;
    Alcotest.test_case "other tool summaries" `Quick
      test_summarize_tool_arguments_shows_other_tool_details;
    Alcotest.test_case "thinking message prefixes content" `Quick
      test_thinking_message_prefixes_content;
  ]

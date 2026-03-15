let test_tool_start_message_uses_summary () =
  let message =
    Stream_visibility.tool_start_message ~name:"file_read"
      ~summary:(Some "src/main.ml")
  in
  Alcotest.(check string)
    "tool start summary message"
    "\xF0\x9F\x93\x96 *file_read* \xE2\x80\x94 `src/main.ml`" message

let test_tool_call_message_error_includes_summary_and_truncates () =
  let result = String.make 300 'x' in
  let message =
    Stream_visibility.tool_call_message ~name:"shell_exec"
      ~summary:(Some "ls -la") ~result ~is_error:true
  in
  Alcotest.(check bool)
    "tool call error has prefix" true
    (String.starts_with
       ~prefix:"\xE2\x9D\x8C *shell_exec* \xE2\x80\x94 `ls -la`" message);
  Alcotest.(check bool)
    "tool call error truncated" true
    (String.ends_with ~suffix:"..._" message)

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
    "shell_exec shows command head and tail" (Some "git status head 5 tail 10")
    (Stream_visibility.summarize_tool_arguments ~name:"shell_exec"
       {|{"command":"git status","head":5,"tail":10}|});
  Alcotest.(check (option string))
    "generic tool shows labeled summary" (Some "query=status")
    (Stream_visibility.summarize_tool_arguments ~name:"search"
       {|{"query":"status","limit":5}|})

let test_summarize_tool_arguments_shows_file_edit_details () =
  Alcotest.(check (option string))
    "file_edit shows line delta"
    (Some "src/main.ml \xF0\x9F\x94\xB4-1L/\xF0\x9F\x9F\xA2+2L all")
    (Stream_visibility.summarize_tool_arguments ~name:"file_edit"
       {|{"path":"src/main.ml","old_text":"a","new_text":"b
c","replace_all":true}|});
  Alcotest.(check (option string))
    "file_edit_lines shows range and delta"
    (Some "src/main.ml L10-12 \xF0\x9F\x94\xB4-3L/\xF0\x9F\x9F\xA2+2L")
    (Stream_visibility.summarize_tool_arguments ~name:"file_edit_lines"
       {|{"path":"src/main.ml","start_line":10,"end_line":12,"content":"x
y"}|});
  Alcotest.(check (option string))
    "file_write shows added lines"
    (Some "src/main.ml write \xF0\x9F\x9F\xA2+2L")
    (Stream_visibility.summarize_tool_arguments ~name:"file_write"
       {|{"path":"src/main.ml","content":"x
y"}|});
  Alcotest.(check (option string))
    "file_append shows added lines"
    (Some "src/main.ml append \xF0\x9F\x9F\xA2+2L")
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

let test_summarize_tool_result_previews () =
  Alcotest.(check (option string))
    "file_read shows line count" (Some "3 lines")
    (Stream_visibility.summarize_tool_result ~name:"file_read" "a\nb\nc");
  Alcotest.(check (option string))
    "shell_exec empty" (Some "empty output")
    (Stream_visibility.summarize_tool_result ~name:"shell_exec" "  ");
  Alcotest.(check (option string))
    "shell_exec first line" (Some "hello world")
    (Stream_visibility.summarize_tool_result ~name:"shell_exec"
       "hello world\nmore stuff");
  Alcotest.(check (option string))
    "grep matches" (Some "2 matches")
    (Stream_visibility.summarize_tool_result ~name:"grep" "line1\nline2");
  Alcotest.(check (option string))
    "grep no matches" (Some "no matches")
    (Stream_visibility.summarize_tool_result ~name:"grep" "");
  Alcotest.(check (option string))
    "glob few files" (Some "a.ml, b.ml")
    (Stream_visibility.summarize_tool_result ~name:"glob" "a.ml\nb.ml");
  Alcotest.(check (option string))
    "glob many files" (Some "5 files")
    (Stream_visibility.summarize_tool_result ~name:"glob"
       "a.ml\nb.ml\nc.ml\nd.ml\ne.ml");
  Alcotest.(check (option string))
    "glob no files" (Some "no files")
    (Stream_visibility.summarize_tool_result ~name:"glob" "");
  Alcotest.(check (option string))
    "memory_store" (Some "stored")
    (Stream_visibility.summarize_tool_result ~name:"memory_store" "OK");
  Alcotest.(check (option string))
    "memory_recall found" (Some "found, ~2 tokens")
    (Stream_visibility.summarize_tool_result ~name:"memory_recall" "hello world");
  Alcotest.(check (option string))
    "memory_recall no match" (Some "no match")
    (Stream_visibility.summarize_tool_result ~name:"memory_recall"
       "No matching memories found.");
  Alcotest.(check (option string))
    "web_fetch small" (Some "500 B")
    (Stream_visibility.summarize_tool_result ~name:"web_fetch"
       (String.make 500 'x'));
  Alcotest.(check (option string))
    "generic short" (Some "done")
    (Stream_visibility.summarize_tool_result ~name:"custom_tool" "done");
  Alcotest.(check (option string))
    "generic long" (Some "~17 tokens")
    (Stream_visibility.summarize_tool_result ~name:"custom_tool"
       (String.make 100 'x'));
  Alcotest.(check (option string))
    "generic empty" None
    (Stream_visibility.summarize_tool_result ~name:"custom_tool" "")

let test_tool_call_message_includes_result_preview () =
  let message =
    Stream_visibility.tool_call_message ~name:"file_read"
      ~summary:(Some "src/main.ml") ~result:"line1\nline2\nline3"
      ~is_error:false
  in
  Alcotest.(check bool)
    "success has arrow" true
    (String.length message > 0
    &&
      try
        ignore (Str.search_forward (Str.regexp_string "\xE2\x86\x92") message 0);
        true
      with Not_found -> false);
  Alcotest.(check bool)
    "success preview has line count" true
    (try
       ignore (Str.search_forward (Str.regexp_string "3 lines") message 0);
       true
     with Not_found -> false);
  let message2 =
    Stream_visibility.tool_call_message ~name:"memory_store" ~summary:None
      ~result:"OK" ~is_error:false
  in
  Alcotest.(check bool)
    "no-summary success has preview" true
    (try
       ignore (Str.search_forward (Str.regexp_string "stored") message2 0);
       true
     with Not_found -> false)

let test_thinking_message_prefixes_content () =
  Alcotest.(check string)
    "thinking message" "\xF0\x9F\x92\xAD *Thinking:*\nplan first"
    (Stream_visibility.thinking_message "plan first")

let test_estimate_tokens () =
  let check label expected input =
    Alcotest.(check int)
      label expected
      (Stream_visibility.estimate_tokens input)
  in
  check "empty" 0 "";
  check "single word" 1 "hello";
  check "two words" 2 "hello world";
  check "number" 1 "12345";
  check "decimal" 5 "3,141.59";
  check "punctuation pair" 1 "()";
  check "punctuation seq" 3 "-----";
  check "short token" 1 "hi";
  check "whitespace only" 0 "   ";
  check "CJK" 3 "\xe4\xbd\xa0\xe5\xa5\xbd\xe5\x90\x97";
  check "long word ceil(10/6)=2" 2 "everything";
  check "accented ceil(5/3)=2" 2 "caf\xc3\xa9s";
  check "mixed sentence" 8 "Hello, world! How are you?"

let test_redact_home_path () =
  let home = try Sys.getenv "HOME" with Not_found -> "" in
  let check label expected input =
    Alcotest.(check string)
      label expected
      (Stream_visibility.redact_home_path input)
  in
  if String.length home > 0 then begin
    check "path under home" ("~" ^ "/src/project") (home ^ "/src/project");
    check "exact home" "~" home;
    check "path not under home" "/tmp/work" "/tmp/work";
    check "path with home as prefix but not dir" (home ^ "extra")
      (home ^ "extra")
  end
  else begin
    check "no home env returns unchanged" "/tmp/work" "/tmp/work"
  end

let test_summarize_tool_arguments_redacts_home () =
  let home = try Sys.getenv "HOME" with Not_found -> "" in
  if String.length home > 0 then begin
    Alcotest.(check (option string))
      "shell_exec redacts home in cwd"
      (Some ("git status in ~" ^ "/src/proj"))
      (Stream_visibility.summarize_tool_arguments ~name:"shell_exec"
         (Printf.sprintf {|{"command":"git status","cwd":"%s/src/proj"}|} home));
    Alcotest.(check (option string))
      "glob redacts home in root"
      (Some ("*.ml in ~" ^ "/src/proj"))
      (Stream_visibility.summarize_tool_arguments ~name:"glob"
         (Printf.sprintf {|{"pattern":"*.ml","root":"%s/src/proj"}|} home));
    Alcotest.(check (option string))
      "grep redacts home in path"
      (Some ("pattern in ~" ^ "/src/proj"))
      (Stream_visibility.summarize_tool_arguments ~name:"grep"
         (Printf.sprintf {|{"pattern":"pattern","path":"%s/src/proj"}|} home))
  end

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
    Alcotest.test_case "result preview extraction" `Quick
      test_summarize_tool_result_previews;
    Alcotest.test_case "tool call message includes result preview" `Quick
      test_tool_call_message_includes_result_preview;
    Alcotest.test_case "thinking message prefixes content" `Quick
      test_thinking_message_prefixes_content;
    Alcotest.test_case "estimate tokens heuristic" `Quick test_estimate_tokens;
    Alcotest.test_case "redact home path" `Quick test_redact_home_path;
    Alcotest.test_case "summarize tool arguments redacts home" `Quick
      test_summarize_tool_arguments_redacts_home;
  ]

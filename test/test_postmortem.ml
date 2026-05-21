(* B611: postmortem evidence now includes structured tool-call data so the
   analyst can diagnose loops. Previously format_history_text rendered each
   message as just [role]: content, losing assistant tool_call names/args and
   tool result tool names. *)

let contains hay needle =
  let hlen = String.length hay in
  let nlen = String.length needle in
  let rec loop i =
    if i + nlen > hlen then false
    else if String.sub hay i nlen = needle then true
    else loop (i + 1)
  in
  nlen = 0 || loop 0

let assistant_with_tool_calls ~calls =
  {
    Provider.role = "assistant";
    content = "";
    content_parts = [];
    tool_calls = calls;
    tool_call_id = None;
    name = None;
    provider_response_items_json = None;
    thinking = None;
    is_error = false;
  }

let test_format_history_text_includes_tool_call_names_and_args () =
  let assistant =
    assistant_with_tool_calls
      ~calls:
        [
          {
            Provider.id = "tc1";
            function_name = "shell_exec";
            arguments = {|{"command":"ls /tmp"}|};
          };
        ]
  in
  let tool_result =
    Provider.make_tool_result ~tool_call_id:"tc1" ~name:"shell_exec"
      ~content:"Error: permission denied"
  in
  let rendered = Postmortem.format_history_text [ assistant; tool_result ] in
  Alcotest.(check bool)
    "rendered text mentions tool name shell_exec" true
    (contains rendered "shell_exec");
  Alcotest.(check bool)
    "rendered text includes the tool call arguments" true
    (contains rendered "ls /tmp");
  Alcotest.(check bool)
    "rendered text includes the tool call id (so dups are visible)" true
    (contains rendered "tc1");
  Alcotest.(check bool)
    "rendered text includes the error content from tool result" true
    (contains rendered "permission denied")

let test_format_history_text_repeated_tool_calls_show_each_invocation () =
  let mk_call id args =
    { Provider.id; function_name = "file_read"; arguments = args }
  in
  let assistant_a = assistant_with_tool_calls ~calls:[ mk_call "a" {|{}|} ] in
  let result_a =
    Provider.make_tool_result ~tool_call_id:"a" ~name:"file_read"
      ~content:"Error: missing required parameter 'path'"
  in
  let assistant_b = assistant_with_tool_calls ~calls:[ mk_call "b" {|{}|} ] in
  let result_b =
    Provider.make_tool_result ~tool_call_id:"b" ~name:"file_read"
      ~content:"Error: missing required parameter 'path'"
  in
  let assistant_c = assistant_with_tool_calls ~calls:[ mk_call "c" {|{}|} ] in
  let result_c =
    Provider.make_tool_result ~tool_call_id:"c" ~name:"file_read"
      ~content:"Error: missing required parameter 'path'"
  in
  let rendered =
    Postmortem.format_history_text
      [ assistant_a; result_a; assistant_b; result_b; assistant_c; result_c ]
  in
  Alcotest.(check bool)
    "rendered shows tool call id 'a'" true
    (contains rendered "tc=" || contains rendered "id=a");
  Alcotest.(check bool)
    "rendered shows tool call id 'b'" true (contains rendered "id=b");
  Alcotest.(check bool)
    "rendered shows tool call id 'c'" true (contains rendered "id=c");
  Alcotest.(check bool)
    "rendered mentions missing required parameter" true
    (contains rendered "missing required parameter")

let test_format_history_text_truncates_long_content () =
  let long_args = String.make 2000 'x' in
  let assistant =
    assistant_with_tool_calls
      ~calls:
        [
          {
            Provider.id = "tc-long";
            function_name = "shell_exec";
            arguments = long_args;
          };
        ]
  in
  let rendered = Postmortem.format_history_text [ assistant ] in
  Alcotest.(check bool)
    "individual tool call args are truncated to bounded length" true
    (String.length rendered < 1200);
  Alcotest.(check bool)
    "truncation marker present" true
    (contains rendered "truncated")

(* B610: Postmortem_followup.extract_file_bug parses FILE_BUG / BODY / ENDBUG
   markers out of the agent's response or postmortem doc so a backlog bug
   can be auto-lodged. Marker is explicit so the agent has to opt in. *)
let test_extract_file_bug_basic () =
  let text =
    "Analysis complete.\n\
     FILE_BUG: Sample failure title\n\
     BODY:\n\
     The agent looped on shell_exec without supplying command. Root cause:\n\
     model omits required param.\n\
     ENDBUG\n\
     Trailing notes can follow."
  in
  match Postmortem_followup.extract_file_bug text with
  | None -> Alcotest.fail "expected a parsed FILE_BUG block"
  | Some (title, body) ->
      Alcotest.(check string) "title" "Sample failure title" title;
      Alcotest.(check bool)
        "body contains first explanation line" true
        (try
           let _ = Str.search_forward (Str.regexp_string "Root cause") body 0 in
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "body terminates before ENDBUG marker" true
        (try
           let _ =
             Str.search_forward (Str.regexp_string "Trailing notes") body 0
           in
           false
         with Not_found -> true)

let test_extract_file_bug_runs_to_eof_without_endbug () =
  let text =
    "FILE_BUG: Open-ended report\n\
     BODY:\n\
     This bug body has no ENDBUG terminator.\n\
     It should still be captured."
  in
  match Postmortem_followup.extract_file_bug text with
  | None -> Alcotest.fail "expected a parsed FILE_BUG block"
  | Some (title, body) ->
      Alcotest.(check string) "title" "Open-ended report" title;
      Alcotest.(check bool)
        "body includes both content lines" true
        ((try
            let _ =
              Str.search_forward
                (Str.regexp_string "no ENDBUG terminator")
                body 0
            in
            true
          with Not_found -> false)
        &&
          try
            let _ =
              Str.search_forward (Str.regexp_string "still be captured") body 0
            in
            true
          with Not_found -> false)

let test_extract_file_bug_no_marker_returns_none () =
  let text =
    "Analysis: nothing notable. The model self-corrected after one retry."
  in
  Alcotest.(check (option (pair string string)))
    "no marker -> None" None
    (Postmortem_followup.extract_file_bug text)

let test_extract_file_bug_empty_title_returns_none () =
  let text = "FILE_BUG:   \nBODY:\nsome content\nENDBUG" in
  Alcotest.(check (option (pair string string)))
    "empty title -> None" None
    (Postmortem_followup.extract_file_bug text)

let test_extract_file_bug_empty_body_returns_none () =
  let text = "FILE_BUG: title here\nBODY:\nENDBUG" in
  Alcotest.(check (option (pair string string)))
    "empty body -> None" None
    (Postmortem_followup.extract_file_bug text)

let suite =
  [
    Alcotest.test_case
      "B611: format_history_text includes tool-call names and args" `Quick
      test_format_history_text_includes_tool_call_names_and_args;
    Alcotest.test_case "B611: repeated tool calls render with distinct ids"
      `Quick test_format_history_text_repeated_tool_calls_show_each_invocation;
    Alcotest.test_case "B611: long content is truncated" `Quick
      test_format_history_text_truncates_long_content;
    Alcotest.test_case "B610: extract_file_bug basic marker" `Quick
      test_extract_file_bug_basic;
    Alcotest.test_case "B610: extract_file_bug runs to EOF without ENDBUG"
      `Quick test_extract_file_bug_runs_to_eof_without_endbug;
    Alcotest.test_case "B610: extract_file_bug no marker -> None" `Quick
      test_extract_file_bug_no_marker_returns_none;
    Alcotest.test_case "B610: extract_file_bug empty title -> None" `Quick
      test_extract_file_bug_empty_title_returns_none;
    Alcotest.test_case "B610: extract_file_bug empty body -> None" `Quick
      test_extract_file_bug_empty_body_returns_none;
  ]

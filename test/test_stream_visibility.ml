let test_tool_start_message_includes_name_and_arguments () =
  let message =
    Stream_visibility.tool_start_message ~name:"bash"
      ~arguments:{|{"command":"pwd"}|}
  in
  Alcotest.(check string)
    "tool start message" "Tool call: bash\n{\"command\":\"pwd\"}" message

let test_tool_result_message_truncates_long_output () =
  let result = String.make 900 'x' in
  let message =
    Stream_visibility.tool_result_message ~name:"bash" ~result ~is_error:false
  in
  Alcotest.(check bool)
    "tool result has prefix" true
    (String.starts_with ~prefix:"Tool result: bash\n" message);
  Alcotest.(check bool)
    "tool result truncated" true
    (String.ends_with ~suffix:"..." message)

let test_thinking_message_prefixes_content () =
  Alcotest.(check string)
    "thinking message" "Thinking:\nplan first"
    (Stream_visibility.thinking_message "plan first")

let suite =
  [
    Alcotest.test_case "tool start message includes args" `Quick
      test_tool_start_message_includes_name_and_arguments;
    Alcotest.test_case "tool result message truncates" `Quick
      test_tool_result_message_truncates_long_output;
    Alcotest.test_case "thinking message prefixes content" `Quick
      test_thinking_message_prefixes_content;
  ]

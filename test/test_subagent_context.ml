(* B720: Tests for context forking — verify forked history is prepended
   verbatim with no extra system messages, preserving prompt cache alignment. *)

let msg ~role content =
  {
    Provider.role;
    content;
    content_parts = [];
    tool_calls = [];
    tool_call_id = None;
    name = None;
    provider_response_items_json = None;
    thinking = None;
    is_error = false;
  }

let test_messages_roundtrip () =
  (* Serialize parent messages, deserialize, verify exact match *)
  let parent_msgs =
    [
      msg ~role:"system" "You are a helpful assistant.";
      msg ~role:"user" "What is 2+2?";
      msg ~role:"assistant" "4";
    ]
  in
  let json = Provider.messages_to_json parent_msgs in
  let json_str = Yojson.Safe.to_string json in
  let restored = Daemon_util_localturn.messages_of_json_string json_str in
  Alcotest.(check int)
    "same message count"
    (List.length parent_msgs)
    (List.length restored);
  List.iter2
    (fun orig got ->
      Alcotest.(check string)
        "role matches"
        orig.Provider.role
        got.Provider.role;
      Alcotest.(check string)
        "content matches"
        orig.Provider.content
        got.Provider.content)
    parent_msgs restored

let test_forked_prefix_no_extra_system () =
  (* The critical test: forked history must NOT have an extra system message
     prepended. After deserialization, the first message must be the parent's
     original first message. *)
  let parent_msgs =
    [
      msg ~role:"system" "You are a coder agent.";
      msg ~role:"user" "Write a hello world program";
      msg ~role:"assistant" "print('hello world')";
    ]
  in
  let json = Provider.messages_to_json parent_msgs in
  let json_str = Yojson.Safe.to_string json in
  let forked = Daemon_util_localturn.messages_of_json_string json_str in
  (* First message must be the parent's system message, not a wrapper *)
  (match forked with
  | first :: _ ->
      Alcotest.(check string)
        "first message is parent's system prompt"
        "You are a coder agent."
        first.Provider.content;
      Alcotest.(check string)
        "first message role is system"
        "system"
        first.Provider.role
  | [] -> Alcotest.fail "expected at least one message in forked history");
  (* No message should contain the old wrapper text *)
  List.iter
    (fun m ->
      let has_wrapper =
        String.sub m.Provider.content 0
          (min 30 (String.length m.Provider.content))
        = "The following messages are fr"
      in
      Alcotest.(check bool)
        "no wrapper system message"
        false
        has_wrapper)
    forked

let test_forked_then_child_history () =
  (* Simulate the history construction: forked @ db_history *)
  let parent_msgs =
    [
      msg ~role:"system" "System prompt";
      msg ~role:"user" "Parent question";
      msg ~role:"assistant" "Parent answer";
    ]
  in
  let child_db_msgs =
    [ msg ~role:"user" "Child follow-up"; msg ~role:"assistant" "Child reply" ]
  in
  let json = Provider.messages_to_json parent_msgs in
  let json_str = Yojson.Safe.to_string json in
  let forked = Daemon_util_localturn.messages_of_json_string json_str in
  let combined = forked @ child_db_msgs in
  Alcotest.(check int) "combined length" 5 (List.length combined);
  (* First 3 messages are the parent's, verbatim *)
  let first_three = List.filteri (fun i _ -> i < 3) combined in
  List.iter2
    (fun orig got ->
      Alcotest.(check string)
        "parent msg preserved"
        orig.Provider.content
        got.Provider.content)
    parent_msgs first_three;
  (* Last 2 messages are the child's *)
  let last_two = List.filteri (fun i _ -> i >= 3) combined in
  List.iter2
    (fun orig got ->
      Alcotest.(check string)
        "child msg preserved"
        orig.Provider.content
        got.Provider.content)
    child_db_msgs last_two

let test_empty_snapshot_uses_db_history () =
  (* When context_snapshot is empty or None, only db_history is used *)
  let child_msgs = [ msg ~role:"user" "hello"; msg ~role:"assistant" "hi" ] in
  let empty_forked = Daemon_util_localturn.messages_of_json_string "" in
  Alcotest.(check int) "empty snapshot yields empty list" 0 (List.length empty_forked);
  let combined = empty_forked @ child_msgs in
  Alcotest.(check int)
    "combined is just child msgs"
    (List.length child_msgs)
    (List.length combined)

let test_malformed_json_yields_empty () =
  let result = Daemon_util_localturn.messages_of_json_string "not json" in
  Alcotest.(check int) "malformed json yields empty list" 0 (List.length result)

let test_tool_calls_preserved () =
  (* Messages with tool_calls survive the roundtrip *)
  let parent_msgs =
    [
      msg ~role:"user" "list files";
      {
        (msg ~role:"assistant" "") with
        Provider.tool_calls =
          [
            {
              Provider.id = "call_123";
              function_name = "shell_exec";
              arguments = "{\"command\": \"ls\"}";
            };
          ];
      };
      {
        (msg ~role:"tool" "file1.txt\nfile2.txt") with
        Provider.tool_call_id = Some "call_123";
      };
    ]
  in
  let json = Provider.messages_to_json parent_msgs in
  let json_str = Yojson.Safe.to_string json in
  let restored = Daemon_util_localturn.messages_of_json_string json_str in
  Alcotest.(check int) "3 messages restored" 3 (List.length restored);
  let assistant_msg = List.nth restored 1 in
  Alcotest.(check int) "tool_calls preserved" 1 (List.length assistant_msg.Provider.tool_calls);
  let tc = List.hd assistant_msg.Provider.tool_calls in
  Alcotest.(check string) "tool call id" "call_123" tc.Provider.id;
  Alcotest.(check string) "function name" "shell_exec" tc.Provider.function_name;
  let tool_msg = List.nth restored 2 in
  Alcotest.(check (option string)) "tool_call_id preserved" (Some "call_123") tool_msg.Provider.tool_call_id

let suite =
  [
    Alcotest.test_case "messages roundtrip" `Quick test_messages_roundtrip;
    Alcotest.test_case "forked prefix has no extra system message" `Quick
      test_forked_prefix_no_extra_system;
    Alcotest.test_case "forked then child history ordering" `Quick
      test_forked_then_child_history;
    Alcotest.test_case "empty snapshot uses db_history only" `Quick
      test_empty_snapshot_uses_db_history;
    Alcotest.test_case "malformed json yields empty list" `Quick
      test_malformed_json_yields_empty;
    Alcotest.test_case "tool_calls preserved through roundtrip" `Quick
      test_tool_calls_preserved;
  ]

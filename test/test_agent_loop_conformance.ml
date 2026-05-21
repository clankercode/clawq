(* Test conformance between Coq-extracted AgentLoop and native OCaml implementations *)

let make_user content = Provider.make_message ~role:"user" ~content
let make_assistant content = Provider.make_message ~role:"assistant" ~content

let make_assistant_with_calls calls =
  {
    Provider.role = "assistant";
    Provider.content = "";
    Provider.content_parts = [];
    Provider.tool_calls = calls;
    Provider.tool_call_id = None;
    Provider.name = None;
    Provider.provider_response_items_json = None;
    Provider.thinking = None;
    Provider.is_error = false;
  }

let make_tool_result id content =
  {
    Provider.role = "tool";
    Provider.content;
    Provider.content_parts = [];
    Provider.tool_calls = [];
    Provider.tool_call_id = Some id;
    Provider.name = None;
    Provider.provider_response_items_json = None;
    Provider.thinking = None;
    Provider.is_error = false;
  }

let make_tool_result_with_name id name content =
  {
    Provider.role = "tool";
    Provider.content;
    Provider.content_parts = [];
    Provider.tool_calls = [];
    Provider.tool_call_id = Some id;
    Provider.name = Some name;
    Provider.provider_response_items_json = None;
    Provider.thinking = None;
    Provider.is_error = false;
  }

let make_tool_call id name =
  { Provider.id; Provider.function_name = name; Provider.arguments = "{}" }

(* Test: collect_tool_call_ids matches *)

let test_collect_tool_call_ids_basic () =
  let messages =
    [
      make_user "hello";
      make_assistant_with_calls [ make_tool_call "call_1" "test_tool" ];
      make_tool_result "call_1" "result";
      make_assistant_with_calls
        [ make_tool_call "call_2" "tool2"; make_tool_call "call_3" "tool3" ];
      make_tool_result "call_2" "r2";
      make_tool_result "call_3" "r3";
    ]
  in
  let _coq, _native, equal =
    Agent_loop_conformance.conformance_collect_tool_call_ids messages
  in
  Alcotest.(check bool) "collect_tool_call_ids matches" true equal

let test_collect_tool_call_ids_empty () =
  let messages = [ make_user "hi"; make_assistant "there" ] in
  let _coq, _native, equal =
    Agent_loop_conformance.conformance_collect_tool_call_ids messages
  in
  Alcotest.(check bool) "collect_tool_call_ids empty matches" true equal

(* Test: collect_tool_result_ids matches *)

let test_collect_tool_result_ids_basic () =
  let messages =
    [
      make_assistant_with_calls
        [ make_tool_call "c1" "t"; make_tool_call "c2" "t" ];
      make_tool_result "c1" "r1";
      make_tool_result "c2" "r2";
      make_tool_result "orphan" "orphan_result";
    ]
  in
  let _coq, _native, equal =
    Agent_loop_conformance.conformance_collect_tool_result_ids messages
  in
  Alcotest.(check bool) "collect_tool_result_ids matches" true equal

(* Test: ensure_tool_group_integrity removes orphan tool results *)

let test_ensure_tool_group_integrity_orphan_result () =
  let messages =
    [
      make_user "q";
      make_assistant_with_calls [ make_tool_call "c1" "t" ];
      make_tool_result "c1" "r1";
      make_tool_result "orphan" "orphan_result";
    ]
  in
  let coq, native, equal =
    Agent_loop_conformance.conformance_ensure_tool_group_integrity messages
  in
  Alcotest.(check bool) "ensure_tool_group_integrity matches" true equal;
  Alcotest.(check int) "coq removes orphan" 3 (List.length coq);
  Alcotest.(check int) "native removes orphan" 3 (List.length native)

(* Test: ensure_tool_group_integrity strips dangling tool calls *)

let test_ensure_tool_group_integrity_dangling_call () =
  let messages =
    [
      make_user "q";
      make_assistant_with_calls
        [ make_tool_call "c1" "t"; make_tool_call "dangling" "t" ];
      make_tool_result "c1" "r1";
    ]
  in
  let coq, native, equal =
    Agent_loop_conformance.conformance_ensure_tool_group_integrity messages
  in
  Alcotest.(check bool)
    "ensure_tool_group_integrity matches (dangling)" true equal;
  (* Both should keep the message but with only the non-dangling call *)
  let coq_assistant =
    List.find
      (fun m -> m.Provider.role = "assistant" && m.Provider.tool_calls <> [])
      coq
  in
  Alcotest.(check int)
    "coq strips dangling call" 1
    (List.length coq_assistant.Provider.tool_calls)

(* Test: trim_history preserves integrity *)

let test_trim_history_basic () =
  let messages =
    [
      make_user "1";
      make_assistant_with_calls [ make_tool_call "c1" "t" ];
      make_tool_result "c1" "r1";
      make_user "2";
      make_assistant_with_calls [ make_tool_call "c2" "t" ];
      make_tool_result "c2" "r2";
      make_user "3";
    ]
  in
  let coq, native, equal =
    Agent_loop_conformance.conformance_trim_history 4 messages
  in
  Alcotest.(check bool) "trim_history matches" true equal;
  Alcotest.(check int) "coq trims to 4" 4 (List.length coq);
  Alcotest.(check int) "native trims to 4" 4 (List.length native)

(* Test: force_compress_history preserves integrity *)

let test_force_compress_history_basic () =
  let messages =
    [
      make_user "1";
      make_assistant_with_calls [ make_tool_call "c1" "t" ];
      make_tool_result "c1" "r1";
      make_user "2";
      make_assistant_with_calls [ make_tool_call "c2" "t" ];
      make_tool_result "c2" "r2";
      make_user "3";
    ]
  in
  let coq, native, equal =
    Agent_loop_conformance.conformance_force_compress_history 3 messages
  in
  Alcotest.(check bool) "force_compress_history matches" true equal;
  Alcotest.(check int) "coq compresses to 3" 3 (List.length coq);
  Alcotest.(check int) "native compresses to 3" 3 (List.length native)

(* Test: complex scenario with multiple orphans *)

let test_complex_orphan_scenario () =
  let messages =
    [
      make_user "start";
      make_assistant_with_calls
        [ make_tool_call "c1" "t"; make_tool_call "orphan_call" "t" ];
      make_tool_result "c1" "r1";
      make_tool_result "orphan_result" "r";
      make_user "middle";
      make_assistant_with_calls [ make_tool_call "c2" "t" ];
      make_tool_result "c2" "r2";
    ]
  in
  let coq, native, equal =
    Agent_loop_conformance.conformance_ensure_tool_group_integrity messages
  in
  Alcotest.(check bool) "complex scenario matches" true equal;
  (* Should remove orphan_result and strip orphan_call *)
  Alcotest.(check int) "coq result length" 6 (List.length coq);
  Alcotest.(check int) "native result length" 6 (List.length native)

(* Test: tool names are preserved through Coq roundtrip (B363 fix) *)

let test_tool_name_preservation () =
  let messages =
    [
      make_user "hello";
      make_assistant_with_calls [ make_tool_call "call_1" "test_function" ];
      make_tool_result "call_1" "result";
      make_assistant_with_calls
        [
          make_tool_call "call_2" "another_tool";
          make_tool_call "call_3" "third";
        ];
      make_tool_result "call_2" "r2";
      make_tool_result "call_3" "r3";
    ]
  in
  let coq_input = Agent_loop_conformance.provider_to_coq_history messages in
  let coq_output = Clawq_core.AgentLoop.ensure_tool_group_integrity coq_input in
  let result =
    Agent_loop_conformance.coq_to_provider_history_with_names
      ~original_messages:messages coq_output
  in
  let tool_results = List.filter (fun m -> m.Provider.role = "tool") result in
  let get_name m = m.Provider.name in
  let names = List.filter_map get_name tool_results in
  Alcotest.(check int) "3 tool results" 3 (List.length tool_results);
  Alcotest.(check int) "3 tool names preserved" 3 (List.length names);
  Alcotest.(check bool)
    "name 'test_function' preserved" true
    (List.mem "test_function" names);
  Alcotest.(check bool)
    "name 'another_tool' preserved" true
    (List.mem "another_tool" names);
  Alcotest.(check bool) "name 'third' preserved" true (List.mem "third" names)

(* Test: tool names preserved through force_compress_history *)

let test_tool_name_preservation_compress () =
  let messages =
    [
      make_user "1";
      make_assistant_with_calls [ make_tool_call "c1" "tool_one" ];
      make_tool_result "c1" "r1";
      make_user "2";
      make_assistant_with_calls [ make_tool_call "c2" "tool_two" ];
      make_tool_result "c2" "r2";
      make_user "3";
    ]
  in
  let coq, _native, _equal =
    Agent_loop_conformance.conformance_force_compress_history 4 messages
  in
  let tool_results = List.filter (fun m -> m.Provider.role = "tool") coq in
  let names = List.filter_map (fun m -> m.Provider.name) tool_results in
  Alcotest.(check int) "1 tool result preserved" 1 (List.length tool_results);
  Alcotest.(check int) "1 tool name preserved" 1 (List.length names);
  Alcotest.(check bool)
    "name 'tool_one' preserved" true
    (List.mem "tool_one" names)

(* Test suite *)

let suite =
  [
    Alcotest.test_case "collect_tool_call_ids basic" `Quick
      test_collect_tool_call_ids_basic;
    Alcotest.test_case "collect_tool_call_ids empty" `Quick
      test_collect_tool_call_ids_empty;
    Alcotest.test_case "collect_tool_result_ids basic" `Quick
      test_collect_tool_result_ids_basic;
    Alcotest.test_case "orphan tool result" `Quick
      test_ensure_tool_group_integrity_orphan_result;
    Alcotest.test_case "dangling tool call" `Quick
      test_ensure_tool_group_integrity_dangling_call;
    Alcotest.test_case "trim_history basic" `Quick test_trim_history_basic;
    Alcotest.test_case "force_compress_history basic" `Quick
      test_force_compress_history_basic;
    Alcotest.test_case "complex orphan scenario" `Quick
      test_complex_orphan_scenario;
    Alcotest.test_case "tool name preservation" `Quick
      test_tool_name_preservation;
    Alcotest.test_case "tool name preservation compress" `Quick
      test_tool_name_preservation_compress;
  ]

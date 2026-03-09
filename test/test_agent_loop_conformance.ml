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
  ]

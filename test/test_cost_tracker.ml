let test_calculate_cost_known () =
  let cost =
    Cost_tracker.calculate_cost ~model:"claude-opus-4-6" ~prompt_tokens:1000
      ~completion_tokens:500
  in
  let expected =
    (1000.0 *. 15.0 /. 1_000_000.0) +. (500.0 *. 75.0 /. 1_000_000.0)
  in
  Alcotest.(check (float 0.0001)) "claude cost" expected cost

let test_calculate_cost_with_prefix () =
  let cost =
    Cost_tracker.calculate_cost ~model:"anthropic/claude-sonnet-4-6"
      ~prompt_tokens:10000 ~completion_tokens:2000
  in
  Alcotest.(check bool) "cost > 0" true (cost > 0.0)

let test_calculate_cost_unknown () =
  let cost =
    Cost_tracker.calculate_cost ~model:"custom-model" ~prompt_tokens:1000
      ~completion_tokens:500
  in
  Alcotest.(check (float 0.0001)) "unknown = 0" 0.0 cost

let test_record_and_get_session () =
  Cost_tracker.record_turn ~model:"gpt-4o-mini" ~prompt_tokens:100
    ~completion_tokens:50 ~session_id:"test-session-1";
  Cost_tracker.record_turn ~model:"gpt-4o-mini" ~prompt_tokens:200
    ~completion_tokens:100 ~session_id:"test-session-1";
  let total = Cost_tracker.get_session_cost ~session_id:"test-session-1" in
  Alcotest.(check bool) "accumulated cost > 0" true (total > 0.0);
  let stats = Cost_tracker.get_session_stats ~session_id:"test-session-1" in
  match stats with
  | Some s ->
      Alcotest.(check int) "turn count" 2 s.turn_count;
      Alcotest.(check int) "total prompt" 300 s.total_prompt_tokens;
      Alcotest.(check int) "total completion" 150 s.total_completion_tokens
  | None -> Alcotest.fail "expected session stats"

let test_get_session_cost_empty () =
  let cost = Cost_tracker.get_session_cost ~session_id:"nonexistent" in
  Alcotest.(check (float 0.0001)) "empty session = 0" 0.0 cost

let suite =
  [
    Alcotest.test_case "calculate known model" `Quick test_calculate_cost_known;
    Alcotest.test_case "calculate with prefix" `Quick
      test_calculate_cost_with_prefix;
    Alcotest.test_case "calculate unknown" `Quick test_calculate_cost_unknown;
    Alcotest.test_case "record and get session" `Quick
      test_record_and_get_session;
    Alcotest.test_case "empty session cost" `Quick test_get_session_cost_empty;
  ]

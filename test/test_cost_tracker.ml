let test_calculate_cost_known () =
  let cost =
    Cost_tracker.calculate_cost ~model:"claude-opus-4-6" ~prompt_tokens:1000
      ~completion_tokens:500
  in
  let expected =
    (1000.0 *. 5.0 /. 1_000_000.0) +. (500.0 *. 25.0 /. 1_000_000.0)
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

let test_lookup_pricing_record () =
  let p = Cost_tracker.lookup_pricing "claude-opus-4-6" in
  match p with
  | Some pricing ->
      Alcotest.(check (float 0.01)) "input" 5.0 pricing.input_per_m;
      Alcotest.(check (float 0.01)) "output" 25.0 pricing.output_per_m;
      Alcotest.(check bool) "has cache" true (pricing.cache_read_per_m <> None)
  | None -> Alcotest.fail "expected pricing"

let test_cache_cost_with_hit () =
  let cost =
    Cost_tracker.calculate_cost_with_cache ~model:"gpt-5.4" ~prompt_tokens:5000
      ~completion_tokens:1000 ~added_prompt_tokens:1000 ~cache_hit:true ()
  in
  (* fresh: 1000 * 2.50/1M = 0.0025, cached: 4000 * 1.25/1M = 0.005,
     output: 1000 * 15.0/1M = 0.015 => total = 0.0225 *)
  Alcotest.(check (float 0.0001)) "cache hit cost" 0.0225 cost;
  let cost_no_cache =
    Cost_tracker.calculate_cost ~model:"gpt-5.4" ~prompt_tokens:5000
      ~completion_tokens:1000
  in
  Alcotest.(check bool) "cache cheaper" true (cost < cost_no_cache)

let test_cache_cost_no_cache_rate () =
  let cost_cache =
    Cost_tracker.calculate_cost_with_cache ~model:"gpt-4-turbo"
      ~prompt_tokens:5000 ~completion_tokens:1000 ~added_prompt_tokens:1000
      ~cache_hit:true ()
  in
  let cost_standard =
    Cost_tracker.calculate_cost ~model:"gpt-4-turbo" ~prompt_tokens:5000
      ~completion_tokens:1000
  in
  Alcotest.(check (float 0.0001))
    "no cache rate = standard" cost_standard cost_cache

let test_cache_cost_with_api_cached_tokens () =
  let cost_api =
    Cost_tracker.calculate_cost_with_cache ~model:"gpt-5.4" ~prompt_tokens:5000
      ~completion_tokens:1000 ~added_prompt_tokens:1000 ~cache_hit:true
      ~api_cached_tokens:3500 ()
  in
  (* fresh: (5000-3500)=1500 * 2.50/1M = 0.00375, cached: 3500 * 1.25/1M = 0.004375,
     output: 1000 * 15.0/1M = 0.015 => total = 0.023125 *)
  Alcotest.(check (float 0.0001)) "api cached cost" 0.023125 cost_api;
  let cost_heuristic =
    Cost_tracker.calculate_cost_with_cache ~model:"gpt-5.4" ~prompt_tokens:5000
      ~completion_tokens:1000 ~added_prompt_tokens:1000 ~cache_hit:true ()
  in
  (* Heuristic uses added_prompt_tokens: fresh=1000, cached=4000 *)
  Alcotest.(check bool)
    "api vs heuristic differ" true
    (Float.abs (cost_api -. cost_heuristic) > 0.0001)

let suite =
  [
    Alcotest.test_case "calculate known model" `Quick test_calculate_cost_known;
    Alcotest.test_case "calculate with prefix" `Quick
      test_calculate_cost_with_prefix;
    Alcotest.test_case "calculate unknown" `Quick test_calculate_cost_unknown;
    Alcotest.test_case "record and get session" `Quick
      test_record_and_get_session;
    Alcotest.test_case "empty session cost" `Quick test_get_session_cost_empty;
    Alcotest.test_case "lookup returns record" `Quick test_lookup_pricing_record;
    Alcotest.test_case "cache cost with hit" `Quick test_cache_cost_with_hit;
    Alcotest.test_case "cache cost no cache rate" `Quick
      test_cache_cost_no_cache_rate;
    Alcotest.test_case "cache cost with api cached tokens" `Quick
      test_cache_cost_with_api_cached_tokens;
  ]

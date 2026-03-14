let with_db f =
  Test_helpers.with_memory_db (fun db ->
      Memory.init_request_stats_schema db;
      f db)

let insert ~db ~session_key ~provider ~model ~prompt_tokens ~completion_tokens
    ?cost_usd ?added_prompt_tokens () =
  Request_stats.record ~db ~session_key ~provider ~model ~prompt_tokens
    ~completion_tokens ?cost_usd ?added_prompt_tokens ()

let test_total_summary_empty () =
  with_db (fun db ->
      let s = Request_stats.total_summary ~db in
      Alcotest.(check (float 0.0001)) "cost" 0.0 s.total_cost_usd;
      Alcotest.(check int) "prompt" 0 s.total_prompt_tokens;
      Alcotest.(check int) "completion" 0 s.total_completion_tokens;
      Alcotest.(check int) "added" 0 s.total_added_prompt_tokens;
      Alcotest.(check int) "turns" 0 s.total_turns)

let test_total_summary_with_data () =
  with_db (fun db ->
      insert ~db ~session_key:"s1" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:1000 ~completion_tokens:500 ~cost_usd:0.01
        ~added_prompt_tokens:1000 ();
      insert ~db ~session_key:"s1" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:2000 ~completion_tokens:300 ~cost_usd:0.02
        ~added_prompt_tokens:800 ();
      insert ~db ~session_key:"s2" ~provider:"anthropic"
        ~model:"claude-opus-4-6" ~prompt_tokens:500 ~completion_tokens:200
        ~cost_usd:0.05 ~added_prompt_tokens:500 ();
      let s = Request_stats.total_summary ~db in
      Alcotest.(check (float 0.0001)) "cost" 0.08 s.total_cost_usd;
      Alcotest.(check int) "prompt" 3500 s.total_prompt_tokens;
      Alcotest.(check int) "completion" 1000 s.total_completion_tokens;
      Alcotest.(check int) "added" 2300 s.total_added_prompt_tokens;
      Alcotest.(check int) "turns" 3 s.total_turns)

let test_summary_by_session () =
  with_db (fun db ->
      insert ~db ~session_key:"s1" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:1000 ~completion_tokens:500 ~cost_usd:0.10 ();
      insert ~db ~session_key:"s2" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:500 ~completion_tokens:200 ~cost_usd:0.50 ();
      insert ~db ~session_key:"s1" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:300 ~completion_tokens:100 ~cost_usd:0.05 ();
      let sessions = Request_stats.summary_by_session ~db in
      Alcotest.(check int) "count" 2 (List.length sessions);
      let first = List.hd sessions in
      Alcotest.(check string) "top session" "s2" first.session_key;
      Alcotest.(check (float 0.0001))
        "top cost" 0.50 first.summary.total_cost_usd;
      let second = List.nth sessions 1 in
      Alcotest.(check string) "second session" "s1" second.session_key;
      Alcotest.(check int) "second turns" 2 second.summary.total_turns)

let test_summary_for_session () =
  with_db (fun db ->
      insert ~db ~session_key:"s1" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:1000 ~completion_tokens:500 ~cost_usd:0.10 ();
      insert ~db ~session_key:"s2" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:500 ~completion_tokens:200 ~cost_usd:0.50 ();
      let s = Request_stats.summary_for_session ~db ~session_key:"s1" in
      Alcotest.(check (float 0.0001)) "cost" 0.10 s.total_cost_usd;
      Alcotest.(check int) "turns" 1 s.total_turns)

let test_summary_for_session_nonexistent () =
  with_db (fun db ->
      let s =
        Request_stats.summary_for_session ~db ~session_key:"nonexistent"
      in
      Alcotest.(check (float 0.0001)) "cost" 0.0 s.total_cost_usd;
      Alcotest.(check int) "turns" 0 s.total_turns)

let test_summary_by_model () =
  with_db (fun db ->
      insert ~db ~session_key:"s1" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:1000 ~completion_tokens:500 ~cost_usd:0.10 ();
      insert ~db ~session_key:"s1" ~provider:"openai" ~model:"gpt-4o"
        ~prompt_tokens:500 ~completion_tokens:200 ~cost_usd:0.50 ();
      insert ~db ~session_key:"s2" ~provider:"anthropic"
        ~model:"claude-opus-4-6" ~prompt_tokens:800 ~completion_tokens:300
        ~cost_usd:0.30 ();
      let models = Request_stats.summary_by_model ~db in
      Alcotest.(check int) "count" 3 (List.length models);
      let first = List.hd models in
      Alcotest.(check string) "top model" "gpt-4o" first.model;
      Alcotest.(check (float 0.0001))
        "top cost" 0.50 first.summary.total_cost_usd)

let test_summary_by_provider () =
  with_db (fun db ->
      insert ~db ~session_key:"s1" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:1000 ~completion_tokens:500 ~cost_usd:0.10 ();
      insert ~db ~session_key:"s1" ~provider:"openai" ~model:"gpt-4o"
        ~prompt_tokens:500 ~completion_tokens:200 ~cost_usd:0.20 ();
      insert ~db ~session_key:"s2" ~provider:"anthropic"
        ~model:"claude-opus-4-6" ~prompt_tokens:800 ~completion_tokens:300
        ~cost_usd:0.50 ();
      let providers = Request_stats.summary_by_provider ~db in
      Alcotest.(check int) "count" 2 (List.length providers);
      let first_prov, first_s = List.hd providers in
      Alcotest.(check string) "top provider" "anthropic" first_prov;
      Alcotest.(check (float 0.0001)) "top cost" 0.50 first_s.total_cost_usd)

let test_summary_for_period () =
  with_db (fun db ->
      insert ~db ~session_key:"s1" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:1000 ~completion_tokens:500 ~cost_usd:0.10 ();
      insert ~db ~session_key:"s1" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:2000 ~completion_tokens:300 ~cost_usd:0.20 ();
      let s =
        Request_stats.summary_for_period ~db ~since:"datetime('now', '-1 day')"
      in
      Alcotest.(check (float 0.0001)) "cost" 0.30 s.total_cost_usd;
      Alcotest.(check int) "turns" 2 s.total_turns;
      let s_future =
        Request_stats.summary_for_period ~db ~since:"datetime('now', '+1 day')"
      in
      Alcotest.(check int) "future turns" 0 s_future.total_turns;
      let s_start =
        Request_stats.summary_for_period ~db
          ~since:"datetime('now', 'start of day')"
      in
      Alcotest.(check (float 0.0001)) "today cost" 0.30 s_start.total_cost_usd)

let test_null_cost () =
  with_db (fun db ->
      insert ~db ~session_key:"s1" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:1000 ~completion_tokens:500 ();
      let s = Request_stats.total_summary ~db in
      Alcotest.(check (float 0.0001)) "null cost coalesced" 0.0 s.total_cost_usd;
      Alcotest.(check int) "turns" 1 s.total_turns)

let test_format_tokens () =
  Alcotest.(check string)
    "millions" "1.4M"
    (Request_stats.format_tokens 1_400_000);
  Alcotest.(check string)
    "thousands" "15.2K"
    (Request_stats.format_tokens 15_200);
  Alcotest.(check string) "small" "42" (Request_stats.format_tokens 42);
  Alcotest.(check string) "zero" "0" (Request_stats.format_tokens 0);
  Alcotest.(check string) "exact 1K" "1.0K" (Request_stats.format_tokens 1_000);
  Alcotest.(check string)
    "exact 1M" "1.0M"
    (Request_stats.format_tokens 1_000_000)

let test_get_prev_totals () =
  with_db (fun db ->
      insert ~db ~session_key:"s1" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:1000 ~completion_tokens:500 ~cost_usd:0.10 ();
      let prev = Request_stats.get_prev_totals ~db ~session_key:"s1" in
      match prev with
      | Some (pt, ct, ts) ->
          Alcotest.(check int) "prev prompt" 1000 pt;
          Alcotest.(check int) "prev completion" 500 ct;
          Alcotest.(check bool) "has timestamp" true (String.length ts > 0)
      | None -> Alcotest.fail "expected prev totals")

let test_get_prev_totals_empty () =
  with_db (fun db ->
      let prev = Request_stats.get_prev_totals ~db ~session_key:"nonexistent" in
      Alcotest.(check bool) "none" true (prev = None))

let test_added_prompt_tokens_stored () =
  with_db (fun db ->
      insert ~db ~session_key:"s1" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:1000 ~completion_tokens:500 ~added_prompt_tokens:800 ();
      let s = Request_stats.total_summary ~db in
      Alcotest.(check int) "added stored" 800 s.total_added_prompt_tokens)

let test_cache_pricing_calculation () =
  let cost =
    Cost_tracker.calculate_cost_with_cache ~model:"claude-opus-4-6"
      ~prompt_tokens:10000 ~completion_tokens:1000 ~added_prompt_tokens:2000
      ~cache_hit:true ()
  in
  (* added: 2000 * 5.0/1M = 0.01, cached: 8000 * 0.50/1M = 0.004,
     output: 1000 * 25.0/1M = 0.025 => total = 0.039 *)
  Alcotest.(check (float 0.0001)) "cache cost" 0.039 cost

let test_cache_pricing_without_cache () =
  let cost_no_cache =
    Cost_tracker.calculate_cost_with_cache ~model:"claude-opus-4-6"
      ~prompt_tokens:10000 ~completion_tokens:1000 ~added_prompt_tokens:2000
      ~cache_hit:false ()
  in
  let cost_standard =
    Cost_tracker.calculate_cost ~model:"claude-opus-4-6" ~prompt_tokens:10000
      ~completion_tokens:1000
  in
  Alcotest.(check (float 0.0001)) "same as standard" cost_standard cost_no_cache

let suite =
  [
    Alcotest.test_case "total_summary empty" `Quick test_total_summary_empty;
    Alcotest.test_case "total_summary with data" `Quick
      test_total_summary_with_data;
    Alcotest.test_case "summary_by_session" `Quick test_summary_by_session;
    Alcotest.test_case "summary_for_session" `Quick test_summary_for_session;
    Alcotest.test_case "summary_for_session nonexistent" `Quick
      test_summary_for_session_nonexistent;
    Alcotest.test_case "summary_by_model" `Quick test_summary_by_model;
    Alcotest.test_case "summary_by_provider" `Quick test_summary_by_provider;
    Alcotest.test_case "summary_for_period" `Quick test_summary_for_period;
    Alcotest.test_case "null cost_usd" `Quick test_null_cost;
    Alcotest.test_case "format_tokens" `Quick test_format_tokens;
    Alcotest.test_case "get_prev_totals" `Quick test_get_prev_totals;
    Alcotest.test_case "get_prev_totals empty" `Quick test_get_prev_totals_empty;
    Alcotest.test_case "added_prompt_tokens stored" `Quick
      test_added_prompt_tokens_stored;
    Alcotest.test_case "cache pricing calculation" `Quick
      test_cache_pricing_calculation;
    Alcotest.test_case "cache pricing without cache" `Quick
      test_cache_pricing_without_cache;
  ]

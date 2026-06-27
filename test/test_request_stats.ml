let with_db f =
  Test_helpers.with_memory_db (fun db ->
      Memory.init_request_stats_schema db;
      f db)

let insert ~db ~session_key ~provider ~model ~prompt_tokens ~completion_tokens
    ?cost_usd ?added_prompt_tokens ?profile_id ?latency_ms () =
  Request_stats.record ~db ~session_key ~provider ~model ~prompt_tokens
    ~completion_tokens ?cost_usd ?added_prompt_tokens ?profile_id ?latency_ms ()

let exec_exn db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "sqlite exec failed: %s" (Sqlite3.Rc.to_string rc))

let request_stat_profile_latency ~db ~session_key =
  let stmt =
    Sqlite3.prepare db
      "SELECT profile_id, latency_ms, requested_at FROM request_stats WHERE \
       session_key = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let profile_id =
            match Sqlite3.column stmt 0 with
            | Sqlite3.Data.INT n -> Some (Int64.to_int n)
            | _ -> None
          in
          let latency_ms =
            match Sqlite3.column stmt 1 with
            | Sqlite3.Data.INT n -> Some (Int64.to_int n)
            | _ -> None
          in
          let requested_at =
            match Sqlite3.column stmt 2 with
            | Sqlite3.Data.TEXT s -> s
            | _ -> ""
          in
          (profile_id, latency_ms, requested_at)
      | _ -> Alcotest.fail "request_stats row not found")

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

let test_profiled_stats_attribution_stored () =
  with_db (fun db ->
      insert ~db ~session_key:"room-session" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:100 ~completion_tokens:25 ~cost_usd:0.01 ~profile_id:42
        ~latency_ms:123 ();
      let profile_id, latency_ms, requested_at =
        request_stat_profile_latency ~db ~session_key:"room-session"
      in
      Alcotest.(check (option int)) "profile_id stored" (Some 42) profile_id;
      Alcotest.(check (option int)) "latency stored" (Some 123) latency_ms;
      Alcotest.(check bool)
        "timestamp stored" true
        (String.length requested_at > 0))

let test_summary_query_filters_profile_and_time_range () =
  with_db (fun db ->
      insert ~db ~session_key:"old-p1" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:100 ~completion_tokens:10 ~cost_usd:0.10 ~profile_id:1 ();
      insert ~db ~session_key:"new-p1" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:200 ~completion_tokens:20 ~cost_usd:0.20 ~profile_id:1 ();
      insert ~db ~session_key:"new-p2" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:400 ~completion_tokens:40 ~cost_usd:0.40 ~profile_id:2 ();
      exec_exn db
        "UPDATE request_stats SET requested_at = '2026-01-01 00:00:00' WHERE \
         session_key = 'old-p1'";
      exec_exn db
        "UPDATE request_stats SET requested_at = '2026-01-02 00:00:00' WHERE \
         session_key = 'new-p1'";
      exec_exn db
        "UPDATE request_stats SET requested_at = '2026-01-02 00:00:00' WHERE \
         session_key = 'new-p2'";
      let s =
        Request_stats.summary_query ~db ~profile_id:1
          ~since:"2026-01-01 12:00:00" ~until:"2026-01-03 00:00:00" ()
      in
      Alcotest.(check (float 0.0001)) "profile/time cost" 0.20 s.total_cost_usd;
      Alcotest.(check int) "profile/time turns" 1 s.total_turns;
      Alcotest.(check int) "profile/time prompt" 200 s.total_prompt_tokens)

let test_profile_resolution_from_room_sessions () =
  with_db (fun db ->
      Memory.init_room_profiles_schema db;
      Memory.init_room_profile_bindings_schema db;
      let profile_id = Memory.insert_room_profile ~db ~name:"coding" in
      Memory.upsert_room_profile_binding ~db ~room_id:"slack:C1" ~profile_id;
      insert ~db ~session_key:"slack:C1" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:100 ~completion_tokens:10 ();
      let child_key =
        Room_session.child_thread_key ~connector:"slack" ~profile_id:"coding"
          ~room_id:"C1" ()
      in
      insert ~db ~session_key:child_key ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:200 ~completion_tokens:20 ();
      let routine_key =
        Room_session.routine_key ~profile_id:"coding" ~routine_id:"daily" ()
      in
      insert ~db ~session_key:routine_key ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:300 ~completion_tokens:30 ();
      let direct_profile, _, _ =
        request_stat_profile_latency ~db ~session_key:"slack:C1"
      in
      let child_profile, _, _ =
        request_stat_profile_latency ~db ~session_key:child_key
      in
      let routine_profile, _, _ =
        request_stat_profile_latency ~db ~session_key:routine_key
      in
      Alcotest.(check (option int))
        "direct room profile" (Some profile_id) direct_profile;
      Alcotest.(check (option int))
        "child thread profile" (Some profile_id) child_profile;
      Alcotest.(check (option int))
        "routine profile" (Some profile_id) routine_profile)

let test_unprofiled_stats_fallback () =
  with_db (fun db ->
      insert ~db ~session_key:"plain" ~provider:"openai" ~model:"gpt-5.4"
        ~prompt_tokens:100 ~completion_tokens:10 ~cost_usd:0.10 ();
      let profile_id, latency_ms, _ =
        request_stat_profile_latency ~db ~session_key:"plain"
      in
      Alcotest.(check (option int)) "profile remains null" None profile_id;
      Alcotest.(check (option int)) "latency remains null" None latency_ms;
      let s = Request_stats.summary_query ~db () in
      Alcotest.(check (float 0.0001))
        "unprofiled cost counted" 0.10 s.total_cost_usd;
      Alcotest.(check int) "unprofiled turns counted" 1 s.total_turns)

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
    Alcotest.test_case "profiled stats attribution stored" `Quick
      test_profiled_stats_attribution_stored;
    Alcotest.test_case "summary_query filters profile and time range" `Quick
      test_summary_query_filters_profile_and_time_range;
    Alcotest.test_case "profile resolution from room sessions" `Quick
      test_profile_resolution_from_room_sessions;
    Alcotest.test_case "unprofiled stats fallback" `Quick
      test_unprofiled_stats_fallback;
    Alcotest.test_case "cache pricing calculation" `Quick
      test_cache_pricing_calculation;
    Alcotest.test_case "cache pricing without cache" `Quick
      test_cache_pricing_without_cache;
  ]

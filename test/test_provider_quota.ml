(* Tests for Provider_quota: is_constrained, JSON parsing, cache, Unknown handling *)

let make_window ?(resets_at = None) ?(window_duration_s = None) used_pct :
    Provider_quota.window_state =
  { used_pct; resets_at; window_duration_s }

let known_session pct =
  Provider_quota.Known
    { session = Some (make_window pct); weekly = None; monthly = None }

let known_weekly pct =
  Provider_quota.Known
    { session = None; weekly = Some (make_window pct); monthly = None }

let known_monthly pct =
  Provider_quota.Known
    { session = None; weekly = None; monthly = Some (make_window pct) }

let known_none =
  Provider_quota.Known { session = None; weekly = None; monthly = None }

(* ── is_constrained ──────────────────────────────────────────────────────── *)

let test_unknown_never_constrained () =
  Alcotest.(check bool)
    "no_api unknown not constrained" false
    (Provider_quota.is_constrained (Provider_quota.Unknown "no_api"));
  Alcotest.(check bool)
    "fetch_failed unknown not constrained" false
    (Provider_quota.is_constrained
       (Provider_quota.Unknown "fetch_failed:timeout"));
  Alcotest.(check bool)
    "not_configured unknown not constrained" false
    (Provider_quota.is_constrained (Provider_quota.Unknown "not_configured"))

let test_threshold_exceeded () =
  Alcotest.(check bool)
    "90% session constrained" true
    (Provider_quota.is_constrained (known_session 90.0));
  Alcotest.(check bool)
    "86% weekly constrained" true
    (Provider_quota.is_constrained (known_weekly 86.0))

let test_below_threshold_not_constrained () =
  Alcotest.(check bool)
    "80% session not constrained (default threshold 85%)" false
    (Provider_quota.is_constrained (known_session 80.0))

let test_custom_threshold () =
  Alcotest.(check bool)
    "60% at threshold 0.5 constrained" true
    (Provider_quota.is_constrained ~threshold:0.5 (known_session 60.0));
  Alcotest.(check bool)
    "60% at threshold 0.7 not constrained" false
    (Provider_quota.is_constrained ~threshold:0.7 (known_session 60.0))

let test_pace_aware_early_window () =
  (* 10% used in first 5% of a 5h window — pace_ratio = 2.0 but used < 50% *)
  let now = Unix.gettimeofday () in
  let window_dur = 18000.0 in
  let elapsed = window_dur *. 0.05 in
  let resets_at = Some (now -. elapsed +. window_dur) in
  let state =
    Provider_quota.Known
      {
        session =
          Some
            (make_window ~resets_at ~window_duration_s:(Some window_dur) 10.0);
        weekly = None;
        monthly = None;
      }
  in
  Alcotest.(check bool)
    "10% pace-constrained but used < 50% => not constrained" false
    (Provider_quota.is_constrained ~threshold:0.85 state)

let test_pace_aware_late_constrained () =
  (* 80% used in 40% of window => pace_ratio = 2.0 AND used >= 50% *)
  let now = Unix.gettimeofday () in
  let window_dur = 18000.0 in
  let elapsed = window_dur *. 0.40 in
  let resets_at = Some (now -. elapsed +. window_dur) in
  let state =
    Provider_quota.Known
      {
        session =
          Some
            (make_window ~resets_at ~window_duration_s:(Some window_dur) 80.0);
        weekly = None;
        monthly = None;
      }
  in
  Alcotest.(check bool)
    "80% in 40% of window pace constrained" true
    (Provider_quota.is_constrained ~threshold:0.85 state)

let test_monthly_constrained () =
  Alcotest.(check bool)
    "monthly 92% constrained" true
    (Provider_quota.is_constrained (known_monthly 92.0))

let test_all_none_not_constrained () =
  Alcotest.(check bool)
    "all windows None not constrained" false
    (Provider_quota.is_constrained known_none)

(* ── quota_notice ────────────────────────────────────────────────────────── *)

let test_quota_notice_below_threshold () =
  let pq =
    {
      Provider_quota.provider_name = "anthropic";
      state = known_session 60.0;
      fetched_at = Unix.gettimeofday ();
    }
  in
  Alcotest.(check bool)
    "60% does not trigger notice at 70% threshold" false
    (Provider_quota.quota_notice pq <> None)

let test_quota_notice_above_threshold () =
  let pq =
    {
      Provider_quota.provider_name = "anthropic";
      state = known_session 89.0;
      fetched_at = Unix.gettimeofday ();
    }
  in
  match Provider_quota.quota_notice pq with
  | None -> Alcotest.fail "expected notice at 89%"
  | Some s ->
      Alcotest.(check bool)
        "notice contains provider name" true
        (let re = Str.regexp_string "anthropic" in
         try
           ignore (Str.search_forward re s 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "notice contains quota keyword" true
        (let re = Str.regexp_string "[quota]" in
         try
           ignore (Str.search_forward re s 0);
           true
         with Not_found -> false)

let test_quota_notice_unknown () =
  let pq =
    {
      Provider_quota.provider_name = "cursor";
      state = Provider_quota.Unknown "no_api";
      fetched_at = Unix.gettimeofday ();
    }
  in
  Alcotest.(check bool)
    "Unknown provider has no notice" true
    (Provider_quota.quota_notice pq = None)

(* ── to_summary_string ───────────────────────────────────────────────────── *)

let test_to_summary_unknown () =
  let pq =
    {
      Provider_quota.provider_name = "cursor";
      state = Provider_quota.Unknown "no_api";
      fetched_at = Unix.gettimeofday ();
    }
  in
  let s = Provider_quota.to_summary_string pq in
  Alcotest.(check bool)
    "summary contains provider name" true
    (let re = Str.regexp_string "cursor" in
     try
       ignore (Str.search_forward re s 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "summary contains Unknown" true
    (let re = Str.regexp_string "Unknown" in
     try
       ignore (Str.search_forward re s 0);
       true
     with Not_found -> false)

let test_to_summary_known () =
  let pq =
    {
      Provider_quota.provider_name = "anthropic";
      state = known_session 45.0;
      fetched_at = Unix.gettimeofday ();
    }
  in
  let s = Provider_quota.to_summary_string pq in
  Alcotest.(check bool)
    "summary contains 45%" true
    (let re = Str.regexp_string "45%" in
     try
       ignore (Str.search_forward re s 0);
       true
     with Not_found -> false)

(* ── cache ───────────────────────────────────────────────────────────────── *)

let test_cache_roundtrip () =
  Provider_quota.reset_for_test ();
  let name = Printf.sprintf "test_provider_%d" (Random.int 1000000) in
  Alcotest.(check bool)
    "not in cache before insert" true
    (Provider_quota.get_cached name = None);
  let all = Provider_quota.get_all_cached () in
  Alcotest.(check bool)
    "get_all_cached does not contain test name" true
    (not (List.exists (fun (n, _) -> n = name) all))

let test_db_persistence () =
  let db = Sqlite3.db_open ":memory:" in
  Provider_quota.set_db db;
  Provider_quota.reset_for_test ();
  Provider_quota.set_db db;
  let name = "test_db_provider_persist" in
  let pq =
    {
      Provider_quota.provider_name = name;
      state =
        Provider_quota.Known
          { session = Some (make_window 55.0); weekly = None; monthly = None };
      fetched_at = Unix.gettimeofday ();
    }
  in
  Provider_quota.store_result pq;
  (* Simulate new process: clear in-memory cache but keep DB *)
  Provider_quota.reset_for_test ();
  Provider_quota.set_db db;
  Alcotest.(check bool)
    "entry present after DB roundtrip" true
    (Provider_quota.get_cached name <> None);
  Provider_quota.reset_for_test ()

let test_db_ttl_expired () =
  let db = Sqlite3.db_open ":memory:" in
  Provider_quota.set_db db;
  Provider_quota.reset_for_test ();
  Provider_quota.set_db db;
  Provider_quota.set_cache_ttl 0;
  let name = "test_db_ttl_expired" in
  let pq =
    {
      Provider_quota.provider_name = name;
      state =
        Provider_quota.Known
          { session = Some (make_window 40.0); weekly = None; monthly = None };
      fetched_at = Unix.gettimeofday () -. 10.0;
    }
  in
  Provider_quota.store_result pq;
  Provider_quota.reset_for_test ();
  Provider_quota.set_db db;
  Alcotest.(check bool)
    "expired entry not returned by get_all_cached" true
    (Provider_quota.get_all_cached () = []);
  Provider_quota.reset_for_test ();
  Provider_quota.set_cache_ttl 300

(* ── history ─────────────────────────────────────────────────────────────── *)

let setup_history_db () =
  let db = Sqlite3.db_open ":memory:" in
  Provider_quota.reset_for_test ();
  Provider_quota.set_db db;
  ignore
    (Sqlite3.exec db
       "CREATE TABLE IF NOT EXISTS quota_history (\n\
       \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
       \     provider TEXT NOT NULL,\n\
       \     state_json TEXT NOT NULL,\n\
       \     fetched_at REAL NOT NULL,\n\
       \     recorded_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
       \   )");
  db

let test_history_record_and_query () =
  let db = setup_history_db () in
  let pq1 =
    {
      Provider_quota.provider_name = "anthropic";
      state = known_session 40.0;
      fetched_at = Unix.gettimeofday () -. 100.0;
    }
  in
  let pq2 =
    {
      Provider_quota.provider_name = "anthropic";
      state = known_session 60.0;
      fetched_at = Unix.gettimeofday ();
    }
  in
  Provider_quota.store_result pq1;
  Provider_quota.store_result pq2;
  let entries =
    Provider_quota.history_for_provider ~db ~provider:"anthropic" ()
  in
  Alcotest.(check int) "two history entries" 2 (List.length entries);
  let first = List.hd entries in
  Alcotest.(check string) "newest first" "anthropic" first.h_provider;
  Provider_quota.reset_for_test ()

let test_history_all_providers () =
  let db = setup_history_db () in
  let pq1 =
    {
      Provider_quota.provider_name = "anthropic";
      state = known_session 40.0;
      fetched_at = Unix.gettimeofday ();
    }
  in
  let pq2 =
    {
      Provider_quota.provider_name = "codex";
      state = known_weekly 55.0;
      fetched_at = Unix.gettimeofday ();
    }
  in
  Provider_quota.store_result pq1;
  Provider_quota.store_result pq2;
  let entries = Provider_quota.history_all ~db () in
  Alcotest.(check int) "two entries from two providers" 2 (List.length entries);
  let providers =
    List.map (fun (e : Provider_quota.history_entry) -> e.h_provider) entries
  in
  Alcotest.(check bool)
    "both providers present" true
    (List.mem "anthropic" providers && List.mem "codex" providers);
  Provider_quota.reset_for_test ()

let test_history_fetch_failed_not_recorded () =
  let db = setup_history_db () in
  let pq =
    {
      Provider_quota.provider_name = "anthropic";
      state = Provider_quota.Unknown "fetch_failed:HTTP 500";
      fetched_at = Unix.gettimeofday ();
    }
  in
  Provider_quota.store_result pq;
  let entries =
    Provider_quota.history_for_provider ~db ~provider:"anthropic" ()
  in
  Alcotest.(check int) "fetch_failed not in history" 0 (List.length entries);
  Provider_quota.reset_for_test ()

let test_history_json_serialization () =
  let entry =
    {
      Provider_quota.h_id = 1;
      h_provider = "anthropic";
      h_state = known_session 75.0;
      h_fetched_at = 1710000000.0;
      h_recorded_at = "2025-03-10 00:00:00";
    }
  in
  let json = Provider_quota.history_entry_to_json entry in
  let j_str = Yojson.Safe.to_string json in
  let parsed = Yojson.Safe.from_string j_str in
  let provider = Yojson.Safe.Util.(parsed |> member "provider" |> to_string) in
  Alcotest.(check string) "provider roundtrips" "anthropic" provider;
  let id = Yojson.Safe.Util.(parsed |> member "id" |> to_int) in
  Alcotest.(check int) "id roundtrips" 1 id

let test_history_purge () =
  let db = setup_history_db () in
  let pq_old =
    {
      Provider_quota.provider_name = "anthropic";
      state = known_session 30.0;
      fetched_at = Unix.gettimeofday () -. 100000.0;
    }
  in
  let pq_new =
    {
      Provider_quota.provider_name = "anthropic";
      state = known_session 50.0;
      fetched_at = Unix.gettimeofday ();
    }
  in
  Provider_quota.store_result pq_old;
  Provider_quota.store_result pq_new;
  let before = Unix.gettimeofday () -. 50000.0 in
  let count = Provider_quota.purge_history ~db ~before () in
  Alcotest.(check int) "purged 1 old entry" 1 count;
  let remaining = Provider_quota.history_all ~db () in
  Alcotest.(check int) "1 entry remaining" 1 (List.length remaining);
  Provider_quota.reset_for_test ()

let test_history_limit () =
  let db = setup_history_db () in
  for i = 0 to 4 do
    let pq =
      {
        Provider_quota.provider_name = "anthropic";
        state = known_session (float_of_int (i * 10));
        fetched_at = Unix.gettimeofday () +. float_of_int i;
      }
    in
    Provider_quota.store_result pq
  done;
  let entries =
    Provider_quota.history_for_provider ~db ~provider:"anthropic" ~limit:3 ()
  in
  Alcotest.(check int) "limit caps to 3" 3 (List.length entries);
  Provider_quota.reset_for_test ()

let suite =
  [
    Alcotest.test_case "Unknown never constrained" `Quick
      test_unknown_never_constrained;
    Alcotest.test_case "threshold exceeded" `Quick test_threshold_exceeded;
    Alcotest.test_case "below threshold not constrained" `Quick
      test_below_threshold_not_constrained;
    Alcotest.test_case "custom threshold" `Quick test_custom_threshold;
    Alcotest.test_case "pace-aware early window not constrained" `Quick
      test_pace_aware_early_window;
    Alcotest.test_case "pace-aware late window constrained" `Quick
      test_pace_aware_late_constrained;
    Alcotest.test_case "monthly constrained" `Quick test_monthly_constrained;
    Alcotest.test_case "all None not constrained" `Quick
      test_all_none_not_constrained;
    Alcotest.test_case "quota notice below threshold" `Quick
      test_quota_notice_below_threshold;
    Alcotest.test_case "quota notice above threshold" `Quick
      test_quota_notice_above_threshold;
    Alcotest.test_case "quota notice Unknown" `Quick test_quota_notice_unknown;
    Alcotest.test_case "to_summary Unknown" `Quick test_to_summary_unknown;
    Alcotest.test_case "to_summary Known" `Quick test_to_summary_known;
    Alcotest.test_case "cache roundtrip" `Quick test_cache_roundtrip;
    Alcotest.test_case "DB persistence roundtrip" `Quick test_db_persistence;
    Alcotest.test_case "DB TTL expiry" `Quick test_db_ttl_expired;
    Alcotest.test_case "history record and query" `Quick
      test_history_record_and_query;
    Alcotest.test_case "history all providers" `Quick test_history_all_providers;
    Alcotest.test_case "history fetch_failed not recorded" `Quick
      test_history_fetch_failed_not_recorded;
    Alcotest.test_case "history JSON serialization" `Quick
      test_history_json_serialization;
    Alcotest.test_case "history purge" `Quick test_history_purge;
    Alcotest.test_case "history limit" `Quick test_history_limit;
  ]

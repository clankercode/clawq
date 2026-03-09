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
  let name = Printf.sprintf "test_provider_%d" (Random.int 1000000) in
  Alcotest.(check bool)
    "not in cache before insert" true
    (Provider_quota.get_cached name = None);
  let all = Provider_quota.get_all_cached () in
  Alcotest.(check bool)
    "get_all_cached does not contain test name" true
    (not (List.exists (fun (n, _) -> n = name) all))

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
  ]

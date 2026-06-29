let with_db f =
  let db = Memory.init ~db_path:":memory:" () in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

(* --- Quiet hours --- *)

let test_quiet_hours_default_range () =
  (* Default: quiet_start=23, quiet_end=8 *)
  (match
     Github_pr_policy.check_quiet_hours ~hour:12 ~quiet_start:23 ~quiet_end:8
   with
  | Github_pr_policy.Allowed -> ()
  | _ -> Alcotest.fail "hour 12 should be allowed with default quiet hours");
  (match
     Github_pr_policy.check_quiet_hours ~hour:23 ~quiet_start:23 ~quiet_end:8
   with
  | Github_pr_policy.Denied Github_pr_policy.Quiet_hours -> ()
  | _ -> Alcotest.fail "hour 23 should be denied (quiet start)");
  (match
     Github_pr_policy.check_quiet_hours ~hour:0 ~quiet_start:23 ~quiet_end:8
   with
  | Github_pr_policy.Denied Github_pr_policy.Quiet_hours -> ()
  | _ -> Alcotest.fail "hour 0 should be denied (within quiet hours)");
  (match
     Github_pr_policy.check_quiet_hours ~hour:7 ~quiet_start:23 ~quiet_end:8
   with
  | Github_pr_policy.Denied Github_pr_policy.Quiet_hours -> ()
  | _ -> Alcotest.fail "hour 7 should be denied (within quiet hours)");
  match
    Github_pr_policy.check_quiet_hours ~hour:8 ~quiet_start:23 ~quiet_end:8
  with
  | Github_pr_policy.Allowed -> ()
  | _ -> Alcotest.fail "hour 8 should be allowed (quiet end)"

let test_quiet_hours_same_start_end_disables () =
  match
    Github_pr_policy.check_quiet_hours ~hour:10 ~quiet_start:10 ~quiet_end:10
  with
  | Github_pr_policy.Allowed -> ()
  | _ -> Alcotest.fail "same start/end should disable quiet hours"

let test_quiet_hours_non_wrapping () =
  (match
     Github_pr_policy.check_quiet_hours ~hour:13 ~quiet_start:14 ~quiet_end:18
   with
  | Github_pr_policy.Allowed -> ()
  | _ -> Alcotest.fail "hour 13 should be allowed");
  (match
     Github_pr_policy.check_quiet_hours ~hour:14 ~quiet_start:14 ~quiet_end:18
   with
  | Github_pr_policy.Denied Github_pr_policy.Quiet_hours -> ()
  | _ -> Alcotest.fail "hour 14 should be denied (quiet start)");
  (match
     Github_pr_policy.check_quiet_hours ~hour:17 ~quiet_start:14 ~quiet_end:18
   with
  | Github_pr_policy.Denied Github_pr_policy.Quiet_hours -> ()
  | _ -> Alcotest.fail "hour 17 should be denied (within quiet)");
  match
    Github_pr_policy.check_quiet_hours ~hour:18 ~quiet_start:14 ~quiet_end:18
  with
  | Github_pr_policy.Allowed -> ()
  | _ -> Alcotest.fail "hour 18 should be allowed (quiet end)"

(* --- Dedup key construction --- *)

let test_make_dedup_key_ci () =
  let key =
    Github_pr_policy.make_dedup_key ~repo:"owner/repo" ~pr_number:42
      ~ci_name:"test" ~ci_conclusion:"failure" ~is_ci:true ~delivery_id:"abc"
  in
  Alcotest.(check bool)
    "CI key contains repo" true
    (try
       ignore (Str.search_forward (Str.regexp_string "owner/repo") key 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "CI key contains check name" true
    (try
       ignore (Str.search_forward (Str.regexp_string "test") key 0);
       true
     with Not_found -> false);
  (* Same CI info should produce same key *)
  let key2 =
    Github_pr_policy.make_dedup_key ~repo:"owner/repo" ~pr_number:42
      ~ci_name:"test" ~ci_conclusion:"failure" ~is_ci:true ~delivery_id:"xyz"
  in
  Alcotest.(check string) "same CI info produces same key" key key2

let test_make_dedup_key_non_ci () =
  let key =
    Github_pr_policy.make_dedup_key ~repo:"owner/repo" ~pr_number:42 ~ci_name:""
      ~ci_conclusion:"" ~is_ci:false ~delivery_id:"delivery-123"
  in
  Alcotest.(check bool)
    "non-CI key uses delivery_id" true
    (try
       ignore (Str.search_forward (Str.regexp_string "delivery-123") key 0);
       true
     with Not_found -> false)

(* --- Persistent dedup --- *)

let test_persistent_dedup () =
  with_db (fun db ->
      Github_pr_policy.init_schema db;
      let dedup_key = "ci:owner/repo:42:test:failure" in
      (* First check should not be duplicate *)
      let dup1 =
        Github_pr_policy.is_duplicate ~db ~dedup_key ~room_id:"room-1"
          ~cooldown_seconds:60
      in
      Alcotest.(check bool) "first check not duplicate" false dup1;
      (* Record delivery *)
      Github_pr_policy.record_delivery ~db ~dedup_key ~room_id:"room-1"
        ~repo:"owner/repo" ~pr_number:42 ~event_type:"check_run";
      (* Second check should be duplicate *)
      let dup2 =
        Github_pr_policy.is_duplicate ~db ~dedup_key ~room_id:"room-1"
          ~cooldown_seconds:60
      in
      Alcotest.(check bool) "second check is duplicate" true dup2;
      (* Different room should not be duplicate *)
      let dup3 =
        Github_pr_policy.is_duplicate ~db ~dedup_key ~room_id:"room-2"
          ~cooldown_seconds:60
      in
      Alcotest.(check bool) "different room not duplicate" false dup3)

let test_purge_old_entries () =
  with_db (fun db ->
      Github_pr_policy.init_schema db;
      (* Insert a backdated row to test purge *)
      let stmt =
        Sqlite3.prepare db
          "INSERT INTO github_notification_dedupe (dedup_key, room_id, repo, \
           pr_number, event_type, sent_at) VALUES (?, ?, ?, ?, ?, \
           datetime('now', '-2 hours'))"
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore
            (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT "old-key") : Sqlite3.Rc.t);
          ignore
            (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT "room-1") : Sqlite3.Rc.t);
          ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT "r") : Sqlite3.Rc.t);
          ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.INT 1L) : Sqlite3.Rc.t);
          ignore
            (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT "check_run") : Sqlite3.Rc.t);
          ignore (Sqlite3.step stmt : Sqlite3.Rc.t));
      (* Purge with 60s retention should delete the 2-hour-old row *)
      let deleted =
        Github_pr_policy.purge_old_entries ~db ~retention_seconds:60
      in
      Alcotest.(check bool) "purged old entries" true (deleted >= 1))

(* --- Rate limiting --- *)

let test_rate_limit_count () =
  with_db (fun db ->
      Github_pr_policy.init_schema db;
      (* No deliveries yet *)
      let count0 =
        Github_pr_policy.count_deliveries_this_hour ~db ~room_id:"room-1"
      in
      Alcotest.(check int) "initial count" 0 count0;
      (* Record a delivery *)
      Github_pr_policy.record_delivery ~db ~dedup_key:"key1" ~room_id:"room-1"
        ~repo:"r" ~pr_number:1 ~event_type:"check_run";
      let count1 =
        Github_pr_policy.count_deliveries_this_hour ~db ~room_id:"room-1"
      in
      Alcotest.(check int) "count after delivery" 1 count1;
      (* Repeated successful deliveries of the same dedup key still count
         separately for per-room rate limiting. *)
      Github_pr_policy.record_delivery ~db ~dedup_key:"key1" ~room_id:"room-1"
        ~repo:"r" ~pr_number:1 ~event_type:"check_run";
      let count_same_key =
        Github_pr_policy.count_deliveries_this_hour ~db ~room_id:"room-1"
      in
      Alcotest.(check int) "same key delivery count" 2 count_same_key;
      (* Different room *)
      let count2 =
        Github_pr_policy.count_deliveries_this_hour ~db ~room_id:"room-2"
      in
      Alcotest.(check int) "different room count" 0 count2)

(* --- Combined policy check --- *)

let test_decide_allowed_does_not_record () =
  with_db (fun db ->
      Github_pr_policy.init_schema db;
      let result =
        Github_pr_policy.decide ~db ~dedup_key:"key1" ~room_id:"room-1" ~hour:12
          ~quiet_start:23 ~quiet_end:8 ~max_per_hour:0 ~dedupe_seconds:60 ()
      in
      (match result with
      | Github_pr_policy.Allowed -> ()
      | Github_pr_policy.Denied r ->
          Alcotest.failf "expected Allowed, got Denied: %s"
            (Github_pr_policy.reason_to_string r));
      let count =
        Github_pr_policy.count_deliveries_this_hour ~db ~room_id:"room-1"
      in
      Alcotest.(check int) "decide does not record delivery" 0 count)

let test_check_allowed () =
  with_db (fun db ->
      Github_pr_policy.init_schema db;
      let result =
        Github_pr_policy.check ~db ~dedup_key:"key1" ~event_type:"opened"
          ~room_id:"room-1" ~repo:"owner/repo" ~pr_number:42 ~hour:12
          ~quiet_start:23 ~quiet_end:8 ~max_per_hour:0 ~dedupe_seconds:60 ()
      in
      match result with
      | Github_pr_policy.Allowed -> ()
      | Github_pr_policy.Denied r ->
          Alcotest.failf "expected Allowed, got Denied: %s"
            (Github_pr_policy.reason_to_string r))

let test_check_dedup () =
  with_db (fun db ->
      Github_pr_policy.init_schema db;
      (* First delivery allowed *)
      let r1 =
        Github_pr_policy.check ~db ~dedup_key:"key1" ~event_type:"check_run"
          ~room_id:"room-1" ~repo:"owner/repo" ~pr_number:42 ~hour:12
          ~quiet_start:23 ~quiet_end:8 ~max_per_hour:0 ~dedupe_seconds:60 ()
      in
      (match r1 with
      | Github_pr_policy.Allowed -> ()
      | _ -> Alcotest.fail "first delivery should be allowed");
      (* Same dedup key should be denied *)
      let r2 =
        Github_pr_policy.check ~db ~dedup_key:"key1" ~event_type:"check_run"
          ~room_id:"room-1" ~repo:"owner/repo" ~pr_number:42 ~hour:12
          ~quiet_start:23 ~quiet_end:8 ~max_per_hour:0 ~dedupe_seconds:60 ()
      in
      match r2 with
      | Github_pr_policy.Denied Github_pr_policy.Duplicate -> ()
      | _ -> Alcotest.fail "duplicate should be denied")

let test_check_quiet_hours_denied () =
  with_db (fun db ->
      Github_pr_policy.init_schema db;
      let result =
        Github_pr_policy.check ~db ~dedup_key:"key1" ~event_type:"opened"
          ~room_id:"room-1" ~repo:"owner/repo" ~pr_number:42 ~hour:2
          ~quiet_start:23 ~quiet_end:8 ~max_per_hour:0 ~dedupe_seconds:60 ()
      in
      match result with
      | Github_pr_policy.Denied Github_pr_policy.Quiet_hours -> ()
      | _ -> Alcotest.fail "should be denied during quiet hours")

let test_check_rate_limited () =
  with_db (fun db ->
      Github_pr_policy.init_schema db;
      (* With max_per_hour=1, first delivery allowed *)
      let r1 =
        Github_pr_policy.check ~db ~dedup_key:"key1" ~event_type:"check_run"
          ~room_id:"room-1" ~repo:"owner/repo" ~pr_number:42 ~hour:12
          ~quiet_start:23 ~quiet_end:8 ~max_per_hour:1 ~dedupe_seconds:0 ()
      in
      (match r1 with
      | Github_pr_policy.Allowed -> ()
      | _ -> Alcotest.fail "first delivery should be allowed");
      (* Second delivery with different key but same room should be rate limited *)
      let r2 =
        Github_pr_policy.check ~db ~dedup_key:"key2" ~event_type:"check_run"
          ~room_id:"room-1" ~repo:"owner/repo" ~pr_number:42 ~hour:12
          ~quiet_start:23 ~quiet_end:8 ~max_per_hour:1 ~dedupe_seconds:0 ()
      in
      match r2 with
      | Github_pr_policy.Denied Github_pr_policy.Rate_limited -> ()
      | _ -> Alcotest.fail "should be rate limited")

let test_check_priority_order () =
  with_db (fun db ->
      Github_pr_policy.init_schema db;
      (* Record a delivery for dedup test *)
      Github_pr_policy.record_delivery ~db ~dedup_key:"dup-key" ~room_id:"r1"
        ~repo:"o/r" ~pr_number:1 ~event_type:"check_run";
      (* Dedup takes priority over quiet hours *)
      let r1 =
        Github_pr_policy.check ~db ~dedup_key:"dup-key" ~event_type:"check_run"
          ~room_id:"r1" ~repo:"o/r" ~pr_number:1 ~hour:2 ~quiet_start:23
          ~quiet_end:8 ~max_per_hour:0 ~dedupe_seconds:60 ()
      in
      (match r1 with
      | Github_pr_policy.Denied Github_pr_policy.Duplicate -> ()
      | _ -> Alcotest.fail "dedup should take priority over quiet hours");
      (* Quiet hours takes priority over rate limit *)
      let r2 =
        Github_pr_policy.check ~db ~dedup_key:"key2" ~event_type:"opened"
          ~room_id:"r1" ~repo:"o/r" ~pr_number:2 ~hour:2 ~quiet_start:23
          ~quiet_end:8 ~max_per_hour:1 ~dedupe_seconds:60 ()
      in
      match r2 with
      | Github_pr_policy.Denied Github_pr_policy.Quiet_hours -> ()
      | _ -> Alcotest.fail "quiet hours should take priority over rate limit")

(* --- reason_to_string --- *)

let test_reason_to_string () =
  let reasons =
    [
      Github_pr_policy.Duplicate;
      Github_pr_policy.Quiet_hours;
      Github_pr_policy.Rate_limited;
    ]
  in
  List.iter
    (fun r ->
      let s = Github_pr_policy.reason_to_string r in
      if String.length s = 0 then
        Alcotest.failf "reason_to_string returned empty for %s"
          (match r with
          | Github_pr_policy.Duplicate -> "Duplicate"
          | Github_pr_policy.Quiet_hours -> "Quiet_hours"
          | Github_pr_policy.Rate_limited -> "Rate_limited"))
    reasons

let suite =
  [
    Alcotest.test_case "quiet hours default range" `Quick
      test_quiet_hours_default_range;
    Alcotest.test_case "quiet hours same start=end disables" `Quick
      test_quiet_hours_same_start_end_disables;
    Alcotest.test_case "quiet hours non-wrapping" `Quick
      test_quiet_hours_non_wrapping;
    Alcotest.test_case "make dedup key CI" `Quick test_make_dedup_key_ci;
    Alcotest.test_case "make dedup key non-CI" `Quick test_make_dedup_key_non_ci;
    Alcotest.test_case "persistent dedup" `Quick test_persistent_dedup;
    Alcotest.test_case "purge old entries" `Quick test_purge_old_entries;
    Alcotest.test_case "rate limit count" `Quick test_rate_limit_count;
    Alcotest.test_case "decide allowed does not record" `Quick
      test_decide_allowed_does_not_record;
    Alcotest.test_case "check allowed" `Quick test_check_allowed;
    Alcotest.test_case "check dedup" `Quick test_check_dedup;
    Alcotest.test_case "check quiet hours denied" `Quick
      test_check_quiet_hours_denied;
    Alcotest.test_case "check rate limited" `Quick test_check_rate_limited;
    Alcotest.test_case "check priority order" `Quick test_check_priority_order;
    Alcotest.test_case "reason_to_string non-empty" `Quick test_reason_to_string;
  ]

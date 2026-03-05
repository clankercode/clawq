let test_parse_interval_minutes () =
  match Scheduler.parse_schedule "every 5m" with
  | Ok (Scheduler.Interval f) ->
    Alcotest.(check (float 0.01)) "5 minutes" 300.0 f
  | _ -> Alcotest.fail "expected Interval"

let test_parse_interval_hours () =
  match Scheduler.parse_schedule "every 2h" with
  | Ok (Scheduler.Interval f) ->
    Alcotest.(check (float 0.01)) "2 hours" 7200.0 f
  | _ -> Alcotest.fail "expected Interval"

let test_parse_interval_seconds () =
  match Scheduler.parse_schedule "every 30s" with
  | Ok (Scheduler.Interval f) ->
    Alcotest.(check (float 0.01)) "30 seconds" 30.0 f
  | _ -> Alcotest.fail "expected Interval"

let test_parse_cron () =
  match Scheduler.parse_schedule "*/5 * * * *" with
  | Ok (Scheduler.CronExpr { minute; _ }) ->
    Alcotest.(check (list int)) "step 5 on minute" [ -5 ] minute
  | Ok _ -> Alcotest.fail "expected CronExpr"
  | Error e -> Alcotest.fail ("parse error: " ^ e)

let test_parse_cron_specific () =
  match Scheduler.parse_schedule "0 9 * * *" with
  | Ok (Scheduler.CronExpr { minute; hour; _ }) ->
    Alcotest.(check (list int)) "minute 0" [ 0 ] minute;
    Alcotest.(check (list int)) "hour 9" [ 9 ] hour
  | _ -> Alcotest.fail "expected CronExpr"

let test_parse_invalid () =
  match Scheduler.parse_schedule "garbage" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error"

let test_parse_invalid_cron_step_zero () =
  match Scheduler.parse_schedule "*/0 * * * *" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error"

let test_parse_invalid_cron_out_of_range () =
  match Scheduler.parse_schedule "61 24 0 13 7" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error"

let test_should_run_interval () =
  let sched = Scheduler.Interval 300.0 in
  let now = 1000.0 in
  Alcotest.(check bool) "should run when no last_run"
    true (Scheduler.should_run sched ~last_run:None ~now);
  Alcotest.(check bool) "should run after interval"
    true (Scheduler.should_run sched ~last_run:(Some 600.0) ~now);
  Alcotest.(check bool) "should not run before interval"
    false (Scheduler.should_run sched ~last_run:(Some 800.0) ~now)

let test_crud_jobs () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  let jobs = Scheduler.list_jobs ~db in
  Alcotest.(check int) "no jobs initially" 0 (List.length jobs);
  (match Scheduler.add_job ~db ~name:"test" ~session_key:"default"
           ~message:"hello" ~schedule:"every 5m" with
   | Ok () -> ()
   | Error e -> Alcotest.fail ("add_job failed: " ^ e));
  let jobs = Scheduler.list_jobs ~db in
  Alcotest.(check int) "one job" 1 (List.length jobs);
  let j = List.hd jobs in
  Alcotest.(check string) "name" "test" j.name;
  Alcotest.(check string) "session_key" "default" j.session_key;
  Alcotest.(check string) "message" "hello" j.message;
  Alcotest.(check bool) "enabled" true j.enabled;
  Alcotest.(check bool) "remove" true (Scheduler.remove_job ~db ~name:"test");
  let jobs = Scheduler.list_jobs ~db in
  Alcotest.(check int) "no jobs after remove" 0 (List.length jobs);
  Alcotest.(check bool) "remove nonexistent" false
    (Scheduler.remove_job ~db ~name:"nope")

let test_add_invalid_schedule () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  match Scheduler.add_job ~db ~name:"bad" ~session_key:"x"
          ~message:"msg" ~schedule:"garbage" with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected Error for invalid schedule"

let test_run_history () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore (Scheduler.add_job ~db ~name:"hist" ~session_key:"s"
            ~message:"m" ~schedule:"every 1m");
  let run_id = Scheduler.record_run_start ~db ~job_name:"hist" in
  Scheduler.record_run_finish ~db ~run_id ~status:"ok" ~result_preview:"done";
  let runs = Scheduler.get_history ~db ~name:"hist" ~limit:10 in
  Alcotest.(check int) "one run" 1 (List.length runs);
  let r = List.hd runs in
  Alcotest.(check string) "status" "ok" r.status;
  Alcotest.(check string) "preview" "done"
    (match r.result_preview with Some s -> s | None -> "")

let test_get_last_run_time_parses_timestamp () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore (Scheduler.add_job ~db ~name:"last" ~session_key:"s"
            ~message:"m" ~schedule:"every 1m");
  ignore (Scheduler.record_run_start ~db ~job_name:"last");
  match Scheduler.get_last_run_time ~db ~job_name:"last" with
  | None -> Alcotest.fail "expected last_run timestamp"
  | Some ts ->
    let now = Unix.gettimeofday () in
    Alcotest.(check bool) "parsed reasonable timestamp"
      true (ts <= now && ts > now -. 60.0)

let suite =
  [
    Alcotest.test_case "parse interval minutes" `Quick test_parse_interval_minutes;
    Alcotest.test_case "parse interval hours" `Quick test_parse_interval_hours;
    Alcotest.test_case "parse interval seconds" `Quick test_parse_interval_seconds;
    Alcotest.test_case "parse cron step" `Quick test_parse_cron;
    Alcotest.test_case "parse cron specific" `Quick test_parse_cron_specific;
    Alcotest.test_case "parse invalid" `Quick test_parse_invalid;
    Alcotest.test_case "parse invalid cron step zero" `Quick
      test_parse_invalid_cron_step_zero;
    Alcotest.test_case "parse invalid cron out of range" `Quick
      test_parse_invalid_cron_out_of_range;
    Alcotest.test_case "should_run interval" `Quick test_should_run_interval;
    Alcotest.test_case "CRUD jobs" `Quick test_crud_jobs;
    Alcotest.test_case "add invalid schedule" `Quick test_add_invalid_schedule;
    Alcotest.test_case "run history" `Quick test_run_history;
    Alcotest.test_case "get_last_run_time parses timestamp" `Quick
      test_get_last_run_time_parses_timestamp;
  ]

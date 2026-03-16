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
  Alcotest.(check bool)
    "should run when no last_run" true
    (Scheduler.should_run sched ~last_run:None ~now);
  Alcotest.(check bool)
    "should run after interval" true
    (Scheduler.should_run sched ~last_run:(Some 600.0) ~now);
  Alcotest.(check bool)
    "should not run before interval" false
    (Scheduler.should_run sched ~last_run:(Some 800.0) ~now)

let test_should_run_cron_uses_localtime () =
  let now = Unix.gettimeofday () in
  let tm = Unix.localtime now in
  let sched =
    Scheduler.CronExpr
      {
        minute = [ tm.tm_min ];
        hour = [ tm.tm_hour ];
        dom = [];
        month = [];
        dow = [];
      }
  in
  Alcotest.(check bool)
    "cron matches current local time" true
    (Scheduler.should_run sched ~last_run:None ~now);
  let non_matching_hour = (tm.tm_hour + 1) mod 24 in
  let sched_mismatch =
    Scheduler.CronExpr
      {
        minute = [ tm.tm_min ];
        hour = [ non_matching_hour ];
        dom = [];
        month = [];
        dow = [];
      }
  in
  Alcotest.(check bool)
    "cron does not match wrong hour" false
    (Scheduler.should_run sched_mismatch ~last_run:None ~now)

let test_crud_jobs () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  let jobs = Scheduler.list_jobs ~db in
  Alcotest.(check int) "no jobs initially" 0 (List.length jobs);
  (match
     Scheduler.add_job ~db ~name:"test" ~session_key:"default" ~message:"hello"
       ~schedule:"every 5m" ()
   with
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
  Alcotest.(check bool)
    "remove nonexistent" false
    (Scheduler.remove_job ~db ~name:"nope")

let test_add_invalid_schedule () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  match
    Scheduler.add_job ~db ~name:"bad" ~session_key:"x" ~message:"msg"
      ~schedule:"garbage" ()
  with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected Error for invalid schedule"

let test_run_history () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"hist" ~session_key:"s" ~message:"m"
       ~schedule:"every 1m" ());
  let run_id = Scheduler.record_run_start ~db ~job_name:"hist" in
  Scheduler.record_run_finish ~db ~run_id ~status:"ok" ~result_preview:"done";
  let runs = Scheduler.get_history ~db ~name:"hist" ~limit:10 in
  Alcotest.(check int) "one run" 1 (List.length runs);
  let r = List.hd runs in
  Alcotest.(check string) "status" "ok" r.status;
  Alcotest.(check string)
    "preview" "done"
    (match r.result_preview with Some s -> s | None -> "")

let test_get_last_run_time_parses_timestamp () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"last" ~session_key:"s" ~message:"m"
       ~schedule:"every 1m" ());
  ignore (Scheduler.record_run_start ~db ~job_name:"last");
  match Scheduler.get_last_run_time ~db ~job_name:"last" with
  | None -> Alcotest.fail "expected last_run timestamp"
  | Some ts ->
      let now = Unix.gettimeofday () in
      Alcotest.(check bool)
        "parsed reasonable timestamp" true
        (ts <= now && ts > now -. 60.0)

let test_get_session_channel_with_channel () =
  let db = Memory.init ~db_path:":memory:" () in
  let sql =
    "INSERT INTO session_state (session_key, channel, channel_id, turn) VALUES \
     ('teams:conv1:u', 'teams', 'https://smba.trafficmanager.net/conv1', \
     'idle')"
  in
  ignore (Sqlite3.exec db sql);
  match Memory.get_session_channel ~db ~session_key:"teams:conv1:u" with
  | Some (channel, channel_id) ->
      Alcotest.(check string) "channel" "teams" channel;
      Alcotest.(check string)
        "channel_id" "https://smba.trafficmanager.net/conv1" channel_id
  | None -> Alcotest.fail "expected Some (channel, channel_id)"

let test_get_session_channel_without_channel () =
  let db = Memory.init ~db_path:":memory:" () in
  let sql =
    "INSERT INTO session_state (session_key, turn) VALUES ('cli:default', \
     'idle')"
  in
  ignore (Sqlite3.exec db sql);
  Alcotest.(check bool)
    "no channel info" true
    (Memory.get_session_channel ~db ~session_key:"cli:default" = None)

let test_get_session_channel_nonexistent () =
  let db = Memory.init ~db_path:":memory:" () in
  Alcotest.(check bool)
    "nonexistent session" true
    (Memory.get_session_channel ~db ~session_key:"nope" = None)

let test_record_run_delivery_failed () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"dfail" ~session_key:"teams:c:u" ~message:"msg"
       ~schedule:"every 1m" ());
  let run_id = Scheduler.record_run_start ~db ~job_name:"dfail" in
  Scheduler.record_run_finish ~db ~run_id ~status:"delivery_failed"
    ~result_preview:"LLM ok, delivery failed: timeout";
  let runs = Scheduler.get_history ~db ~name:"dfail" ~limit:1 in
  Alcotest.(check int) "one run" 1 (List.length runs);
  let r = List.hd runs in
  Alcotest.(check string) "status" "delivery_failed" r.status;
  Alcotest.(check bool) "has preview" true (r.result_preview <> None)

let test_tick_posts_prompt_via_deliver () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let db = Memory.init ~db_path:":memory:" () in
     Scheduler.init_schema db;
     ignore
       (Scheduler.add_job ~db ~name:"prompt_test" ~session_key:"telegram:42:u"
          ~message:"what is the weather?" ~schedule:"every 1s" ());
     (* Set channel info so deliver callback is used *)
     let sql =
       "INSERT INTO session_state (session_key, channel, channel_id, turn) \
        VALUES ('telegram:42:u', 'telegram', '42', 'idle')"
     in
     ignore (Sqlite3.exec db sql);
     let delivered = ref [] in
     let deliver ~channel ~channel_id ~text =
       delivered := (channel, channel_id, text) :: !delivered;
       Lwt.return (Ok ())
     in
     let session_mgr = Session.create ~config:Runtime_config.default ~db () in
     let* () = Scheduler.tick ~db ~session_mgr ~deliver () in
     (* tick uses Lwt.async; give it a chance to run *)
     let* () = Lwt_unix.sleep 0.05 in
     (* The prompt should have been delivered first *)
     let prompt_delivered =
       List.exists
         (fun (_, _, text) ->
           String.length text > 0
           &&
           let prefix = "[cron:prompt_test]" in
           String.length text >= String.length prefix
           && String.sub text 0 (String.length prefix) = prefix)
         !delivered
     in
     Alcotest.(check bool) "prompt was delivered" true prompt_delivered;
     Lwt.return_unit)

let test_tick_posts_prompt_via_notifier () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let db = Memory.init ~db_path:":memory:" () in
     Scheduler.init_schema db;
     ignore
       (Scheduler.add_job ~db ~name:"notify_test" ~session_key:"discord:ch:u"
          ~message:"daily standup" ~schedule:"every 1s" ());
     let notified = ref [] in
     let session_mgr = Session.create ~config:Runtime_config.default ~db () in
     Session.register_channel_notifier session_mgr ~key:"discord:ch:u"
       (fun text ->
         notified := text :: !notified;
         Lwt.return_unit);
     let* () = Scheduler.tick ~db ~session_mgr () in
     let* () = Lwt_unix.sleep 0.05 in
     let prompt_notified =
       List.exists
         (fun text ->
           let prefix = "[cron:notify_test]" in
           String.length text >= String.length prefix
           && String.sub text 0 (String.length prefix) = prefix)
         !notified
     in
     Alcotest.(check bool) "prompt was notified" true prompt_notified;
     Lwt.return_unit)

let test_tick_prompt_before_response () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let db = Memory.init ~db_path:":memory:" () in
     Scheduler.init_schema db;
     ignore
       (Scheduler.add_job ~db ~name:"order_test" ~session_key:"telegram:99:u"
          ~message:"check status" ~schedule:"every 1s" ());
     let sql =
       "INSERT INTO session_state (session_key, channel, channel_id, turn) \
        VALUES ('telegram:99:u', 'telegram', '99', 'idle')"
     in
     ignore (Sqlite3.exec db sql);
     let delivery_order = ref [] in
     let deliver ~channel:_ ~channel_id:_ ~text =
       delivery_order := text :: !delivery_order;
       Lwt.return (Ok ())
     in
     let session_mgr = Session.create ~config:Runtime_config.default ~db () in
     let* () = Scheduler.tick ~db ~session_mgr ~deliver () in
     (* Give async a chance to run (turn will fail without provider) *)
     let* () = Lwt_unix.sleep 0.1 in
     (* At minimum the prompt should be the first delivered message *)
     let rev_order = List.rev !delivery_order in
     (match rev_order with
     | first :: _ ->
         let prefix = "[cron:order_test]" in
         Alcotest.(check bool)
           "first delivery is the prompt" true
           (String.length first >= String.length prefix
           && String.sub first 0 (String.length prefix) = prefix)
     | [] ->
         (* Turn may fail before any delivery if no provider,
            but prompt should still be delivered *)
         Alcotest.fail "expected at least prompt delivery");
     Lwt.return_unit)

let test_add_ephemeral_job () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  (match
     Scheduler.add_job ~db ~name:"eph" ~session_key:"default" ~message:"hello"
       ~schedule:"every 5m" ~ephemeral:true ()
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail ("add_job failed: " ^ e));
  match Scheduler.get_job ~db ~name:"eph" with
  | None -> Alcotest.fail "expected job"
  | Some j -> Alcotest.(check bool) "ephemeral" true j.ephemeral

let test_add_non_ephemeral_default () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  (match
     Scheduler.add_job ~db ~name:"regular" ~session_key:"default"
       ~message:"hello" ~schedule:"every 5m" ()
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail ("add_job failed: " ^ e));
  match Scheduler.get_job ~db ~name:"regular" with
  | None -> Alcotest.fail "expected job"
  | Some j -> Alcotest.(check bool) "not ephemeral by default" false j.ephemeral

let test_list_jobs_ephemeral () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"e1" ~session_key:"s" ~message:"m"
       ~schedule:"every 1m" ~ephemeral:true ());
  ignore
    (Scheduler.add_job ~db ~name:"e2" ~session_key:"s" ~message:"m"
       ~schedule:"every 1m" ());
  let jobs = Scheduler.list_jobs ~db in
  Alcotest.(check int) "two jobs" 2 (List.length jobs);
  let j1 = List.find (fun (j : Scheduler.job) -> j.name = "e1") jobs in
  let j2 = List.find (fun (j : Scheduler.job) -> j.name = "e2") jobs in
  Alcotest.(check bool) "e1 ephemeral" true j1.ephemeral;
  Alcotest.(check bool) "e2 not ephemeral" false j2.ephemeral

let test_tick_ephemeral_enqueues_bg_task () =
  Lwt_main.run
    (let db = Memory.init ~db_path:":memory:" () in
     Scheduler.init_schema db;
     Background_task.init_schema db;
     ignore
       (Scheduler.add_job ~db ~name:"eph_tick" ~session_key:"telegram:42:u"
          ~message:"check status" ~schedule:"every 1s" ~ephemeral:true ());
     let session_mgr = Session.create ~config:Runtime_config.default ~db () in
     let open Lwt.Syntax in
     let* () = Scheduler.tick ~db ~session_mgr () in
     let runs = Scheduler.get_history ~db ~name:"eph_tick" ~limit:1 in
     Alcotest.(check int) "one run" 1 (List.length runs);
     let r = List.hd runs in
     Alcotest.(check string) "delegated status" "delegated" r.status;
     Alcotest.(check bool)
       "preview mentions bg task" true
       (match r.result_preview with
       | Some p -> String.length p >= 7 && String.sub p 0 7 = "bg task"
       | None -> false);
     let tasks = Background_task.list_tasks ~db in
     let local_tasks =
       List.filter
         (fun (t : Background_task.task) -> t.runner = Background_task.Local)
         tasks
     in
     Alcotest.(check int) "one local bg task" 1 (List.length local_tasks);
     Lwt.return_unit)

let suite =
  [
    Alcotest.test_case "parse interval minutes" `Quick
      test_parse_interval_minutes;
    Alcotest.test_case "parse interval hours" `Quick test_parse_interval_hours;
    Alcotest.test_case "parse interval seconds" `Quick
      test_parse_interval_seconds;
    Alcotest.test_case "parse cron step" `Quick test_parse_cron;
    Alcotest.test_case "parse cron specific" `Quick test_parse_cron_specific;
    Alcotest.test_case "parse invalid" `Quick test_parse_invalid;
    Alcotest.test_case "parse invalid cron step zero" `Quick
      test_parse_invalid_cron_step_zero;
    Alcotest.test_case "parse invalid cron out of range" `Quick
      test_parse_invalid_cron_out_of_range;
    Alcotest.test_case "should_run interval" `Quick test_should_run_interval;
    Alcotest.test_case "should_run cron uses localtime" `Quick
      test_should_run_cron_uses_localtime;
    Alcotest.test_case "CRUD jobs" `Quick test_crud_jobs;
    Alcotest.test_case "add invalid schedule" `Quick test_add_invalid_schedule;
    Alcotest.test_case "run history" `Quick test_run_history;
    Alcotest.test_case "get_last_run_time parses timestamp" `Quick
      test_get_last_run_time_parses_timestamp;
    Alcotest.test_case "get_session_channel with channel" `Quick
      test_get_session_channel_with_channel;
    Alcotest.test_case "get_session_channel without channel" `Quick
      test_get_session_channel_without_channel;
    Alcotest.test_case "get_session_channel nonexistent" `Quick
      test_get_session_channel_nonexistent;
    Alcotest.test_case "record_run delivery_failed status" `Quick
      test_record_run_delivery_failed;
    Alcotest.test_case "tick posts prompt via deliver callback" `Quick
      test_tick_posts_prompt_via_deliver;
    Alcotest.test_case "tick posts prompt via notifier" `Quick
      test_tick_posts_prompt_via_notifier;
    Alcotest.test_case "tick prompt delivered before response" `Quick
      test_tick_prompt_before_response;
    Alcotest.test_case "add ephemeral job" `Quick test_add_ephemeral_job;
    Alcotest.test_case "add non-ephemeral default" `Quick
      test_add_non_ephemeral_default;
    Alcotest.test_case "list jobs ephemeral" `Quick test_list_jobs_ephemeral;
    Alcotest.test_case "tick ephemeral enqueues bg task" `Quick
      test_tick_ephemeral_enqueues_bg_task;
  ]

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

let test_job_routine_metadata_defaults_to_none () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"plain" ~session_key:"s" ~message:"m"
       ~schedule:"every 1m" ());
  match Scheduler.get_job ~db ~name:"plain" with
  | None -> Alcotest.fail "expected job"
  | Some j ->
      Alcotest.(check (option int)) "profile_id" None j.profile_id;
      Alcotest.(check (option string)) "thread_id" None j.thread_id;
      Alcotest.(check (option string))
        "routine_workspace_id" None j.routine_workspace_id;
      Alcotest.(check (option string))
        "routine target" None
        (Scheduler.job_routine_target j)

let test_job_routine_metadata_round_trips () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"routine" ~session_key:"room:abc" ~message:"m"
       ~schedule:"every 1m" ~profile_id:42 ~thread_id:"thread-1"
       ~routine_workspace_id:"workspace-1" ());
  let jobs = Scheduler.list_jobs ~db in
  let j = List.find (fun (j : Scheduler.job) -> j.name = "routine") jobs in
  Alcotest.(check (option int)) "profile_id" (Some 42) j.profile_id;
  Alcotest.(check (option string)) "thread_id" (Some "thread-1") j.thread_id;
  Alcotest.(check (option string))
    "routine_workspace_id" (Some "workspace-1") j.routine_workspace_id;
  Alcotest.(check (option string))
    "routine target" (Some "profile=42 thread=thread-1 workspace=workspace-1")
    (Scheduler.job_routine_target j)

let test_run_history_includes_routine_target () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"routine_hist" ~session_key:"room:abc"
       ~message:"m" ~schedule:"every 1m" ~profile_id:42 ~thread_id:"thread-1"
       ~routine_workspace_id:"workspace-1" ());
  let run_id = Scheduler.record_run_start ~db ~job_name:"routine_hist" in
  Scheduler.record_run_finish ~db ~run_id ~status:"ok" ~result_preview:"done";
  let runs = Scheduler.get_history ~db ~name:"routine_hist" ~limit:10 in
  let r = List.hd runs in
  Alcotest.(check (option int)) "profile_id" (Some 42) r.profile_id;
  Alcotest.(check (option string))
    "routine target" (Some "profile=42 thread=thread-1 workspace=workspace-1")
    (Scheduler.run_routine_target r)

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

(* B587: toggle_job flips enabled state with idempotent semantics. *)
let test_toggle_job_flips_enabled () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"togglable" ~session_key:"s" ~message:"m"
       ~schedule:"every 1m" ());
  Alcotest.(check bool)
    "initially enabled" true
    (match Scheduler.get_job ~db ~name:"togglable" with
    | Some j -> j.enabled
    | None -> false);
  (match Scheduler.toggle_job ~db ~name:"togglable" with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  Alcotest.(check bool)
    "disabled after first toggle" false
    (match Scheduler.get_job ~db ~name:"togglable" with
    | Some j -> j.enabled
    | None -> false);
  (match Scheduler.toggle_job ~db ~name:"togglable" with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  Alcotest.(check bool)
    "re-enabled after second toggle" true
    (match Scheduler.get_job ~db ~name:"togglable" with
    | Some j -> j.enabled
    | None -> false)

let test_toggle_job_returns_error_for_missing () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  match Scheduler.toggle_job ~db ~name:"nonexistent" with
  | Error msg ->
      Alcotest.(check bool)
        "error message mentions missing job" true
        (try
           let _ = Str.search_forward (Str.regexp_string "not found") msg 0 in
           true
         with Not_found -> false)
  | Ok () -> Alcotest.fail "expected Error for missing job name"

(* B463/B467/B472: explicit "ok_notifier_unconfirmed" status exists in the
   cron run schema; cron history can render it distinctly from "ok"
   (deliver_fn confirmed) and "delivery_failed". *)
let test_record_run_ok_notifier_unconfirmed () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"notif" ~session_key:"teams:c:u" ~message:"m"
       ~schedule:"every 1m" ());
  let run_id = Scheduler.record_run_start ~db ~job_name:"notif" in
  Scheduler.record_run_finish ~db ~run_id ~status:"ok_notifier_unconfirmed"
    ~result_preview:"LLM ok, delivery via registered notifier (unconfirmed)";
  let runs = Scheduler.get_history ~db ~name:"notif" ~limit:1 in
  Alcotest.(check int) "one run" 1 (List.length runs);
  let r = List.hd runs in
  Alcotest.(check string)
    "status notifier-unconfirmed" "ok_notifier_unconfirmed" r.status

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

let test_parse_duration_seconds () =
  (match Scheduler.parse_duration_seconds "5m" with
  | Ok f -> Alcotest.(check (float 0.01)) "5m = 300s" 300.0 f
  | Error e -> Alcotest.fail ("expected Ok: " ^ e));
  (match Scheduler.parse_duration_seconds "2h" with
  | Ok f -> Alcotest.(check (float 0.01)) "2h = 7200s" 7200.0 f
  | Error e -> Alcotest.fail ("expected Ok: " ^ e));
  (match Scheduler.parse_duration_seconds "30s" with
  | Ok f -> Alcotest.(check (float 0.01)) "30s" 30.0 f
  | Error e -> Alcotest.fail ("expected Ok: " ^ e));
  match Scheduler.parse_duration_seconds "1d" with
  | Ok f -> Alcotest.(check (float 0.01)) "1d = 86400s" 86400.0 f
  | Error e -> Alcotest.fail ("expected Ok: " ^ e)

let test_parse_duration_seconds_invalid () =
  (match Scheduler.parse_duration_seconds "0m" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for 0m");
  (match Scheduler.parse_duration_seconds "-1h" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for -1h");
  (match Scheduler.parse_duration_seconds "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for abc");
  match Scheduler.parse_duration_seconds "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for empty"

let test_add_job_with_ttl () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  (match
     Scheduler.add_job ~db ~name:"ttl_job" ~session_key:"default"
       ~message:"hello" ~schedule:"every 5m" ~ttl:"24h" ()
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail ("add_job failed: " ^ e));
  match Scheduler.get_job ~db ~name:"ttl_job" with
  | None -> Alcotest.fail "expected job"
  | Some j ->
      Alcotest.(check bool) "expires_at is Some" true (j.expires_at <> None)

let test_add_job_without_ttl () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  (match
     Scheduler.add_job ~db ~name:"no_ttl" ~session_key:"default"
       ~message:"hello" ~schedule:"every 5m" ()
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail ("add_job failed: " ^ e));
  match Scheduler.get_job ~db ~name:"no_ttl" with
  | None -> Alcotest.fail "expected job"
  | Some j ->
      Alcotest.(check bool) "expires_at is None" true (j.expires_at = None)

let test_tick_skips_expired_job () =
  Lwt_main.run
    (let db = Memory.init ~db_path:":memory:" () in
     Scheduler.init_schema db;
     ignore
       (Scheduler.add_job ~db ~name:"exp_job" ~session_key:"default"
          ~message:"hello" ~schedule:"every 1s" ());
     (* Set expires_at in the past *)
     let sql =
       "UPDATE cron_jobs SET expires_at = datetime('now', '-1 hour') WHERE \
        name = 'exp_job'"
     in
     ignore (Sqlite3.exec db sql);
     let session_mgr = Session.create ~config:Runtime_config.default ~db () in
     let open Lwt.Syntax in
     let* () = Scheduler.tick ~db ~session_mgr () in
     (* Job should be disabled *)
     (match Scheduler.get_job ~db ~name:"exp_job" with
     | None -> Alcotest.fail "expected job"
     | Some j -> Alcotest.(check bool) "job disabled" false j.enabled);
     (* Should have an "expired" run *)
     let runs = Scheduler.get_history ~db ~name:"exp_job" ~limit:1 in
     Alcotest.(check int) "one run" 1 (List.length runs);
     let r = List.hd runs in
     Alcotest.(check string) "status is expired" "expired" r.status;
     Lwt.return_unit)

let test_tick_runs_non_expired_job () =
  Lwt_main.run
    (let db = Memory.init ~db_path:":memory:" () in
     Scheduler.init_schema db;
     Background_task.init_schema db;
     ignore
       (Scheduler.add_job ~db ~name:"future_job" ~session_key:"default"
          ~message:"hello" ~schedule:"every 1s" ~ephemeral:true ());
     (* Set expires_at in the future *)
     let sql =
       "UPDATE cron_jobs SET expires_at = datetime('now', '+1 hour') WHERE \
        name = 'future_job'"
     in
     ignore (Sqlite3.exec db sql);
     let session_mgr = Session.create ~config:Runtime_config.default ~db () in
     let open Lwt.Syntax in
     let* () = Scheduler.tick ~db ~session_mgr () in
     (* Job should still be enabled *)
     (match Scheduler.get_job ~db ~name:"future_job" with
     | None -> Alcotest.fail "expected job"
     | Some j -> Alcotest.(check bool) "job still enabled" true j.enabled);
     (* Should have a run with delegated status (ephemeral bg task) *)
     let runs = Scheduler.get_history ~db ~name:"future_job" ~limit:1 in
     Alcotest.(check int) "one run" 1 (List.length runs);
     let r = List.hd runs in
     Alcotest.(check string) "status is delegated" "delegated" r.status;
     Lwt.return_unit)

let test_list_jobs_includes_expires_at () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"with_ttl" ~session_key:"s" ~message:"m"
       ~schedule:"every 1m" ~ttl:"1h" ());
  ignore
    (Scheduler.add_job ~db ~name:"no_ttl_list" ~session_key:"s" ~message:"m"
       ~schedule:"every 1m" ());
  let jobs = Scheduler.list_jobs ~db in
  let j1 = List.find (fun (j : Scheduler.job) -> j.name = "with_ttl") jobs in
  let j2 = List.find (fun (j : Scheduler.job) -> j.name = "no_ttl_list") jobs in
  Alcotest.(check bool) "with_ttl has expires_at" true (j1.expires_at <> None);
  Alcotest.(check bool)
    "no_ttl_list has no expires_at" true (j2.expires_at = None)

let test_update_job_ttl () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"upd_ttl" ~session_key:"s" ~message:"m"
       ~schedule:"every 1m" ());
  (* Initially no TTL *)
  (match Scheduler.get_job ~db ~name:"upd_ttl" with
  | None -> Alcotest.fail "expected job"
  | Some j -> Alcotest.(check bool) "no ttl initially" true (j.expires_at = None));
  (* Add TTL *)
  (match Scheduler.update_job ~db ~name:"upd_ttl" ~ttl:"2h" () with
  | Ok () -> ()
  | Error e -> Alcotest.fail ("update failed: " ^ e));
  (match Scheduler.get_job ~db ~name:"upd_ttl" with
  | None -> Alcotest.fail "expected job"
  | Some j ->
      Alcotest.(check bool) "has ttl after update" true (j.expires_at <> None));
  (* Clear TTL *)
  (match Scheduler.update_job ~db ~name:"upd_ttl" ~ttl:"none" () with
  | Ok () -> ()
  | Error e -> Alcotest.fail ("update failed: " ^ e));
  match Scheduler.get_job ~db ~name:"upd_ttl" with
  | None -> Alcotest.fail "expected job"
  | Some j ->
      Alcotest.(check bool) "no ttl after clear" true (j.expires_at = None)

(* ── B630/B632: consecutive-identical-output detection ──────────────────── *)

let job_enabled ~db ~name =
  match Scheduler.get_job ~db ~name with Some j -> j.enabled | None -> false

(* Stub a triggered cron run: create the job, insert a cron_runs row with the
   given bg_task_id, and return the bg_task_id so we can drive
   mark_run_output. *)
let stub_cron_run ~db ~job_name ~bg_task_id =
  let run_id = Scheduler.record_run_start ~db ~job_name in
  Scheduler.record_run_bg_task ~db ~run_id ~bg_task_id

let test_identical_output_disables_cron () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"loopy" ~session_key:"s" ~message:"m"
       ~schedule:"every 1h" ());
  let output = "Nothing notable." in
  (* Feed 5 identical outputs through 5 distinct bg task ids. *)
  for i = 1 to 5 do
    stub_cron_run ~db ~job_name:"loopy" ~bg_task_id:i;
    let _ = Scheduler.mark_run_output ~db ~bg_task_id:i ~output in
    ()
  done;
  Alcotest.(check bool)
    "5 identical outputs disable the cron" false
    (job_enabled ~db ~name:"loopy")

let test_varying_outputs_keep_cron_enabled () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"varied" ~session_key:"s" ~message:"m"
       ~schedule:"every 1h" ());
  for i = 1 to 5 do
    stub_cron_run ~db ~job_name:"varied" ~bg_task_id:i;
    let _ =
      Scheduler.mark_run_output ~db ~bg_task_id:i
        ~output:(Printf.sprintf "different content %d" i)
    in
    ()
  done;
  Alcotest.(check bool)
    "varying outputs keep the cron enabled" true
    (job_enabled ~db ~name:"varied")

let test_empty_output_does_not_trigger () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"empty" ~session_key:"s" ~message:"m"
       ~schedule:"every 1h" ());
  for i = 1 to 5 do
    stub_cron_run ~db ~job_name:"empty" ~bg_task_id:i;
    let _ = Scheduler.mark_run_output ~db ~bg_task_id:i ~output:"   \n  " in
    ()
  done;
  Alcotest.(check bool)
    "empty outputs don't disable the cron" true
    (job_enabled ~db ~name:"empty")

let test_whitespace_normalized () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"ws" ~session_key:"s" ~message:"m"
       ~schedule:"every 1h" ());
  let variations =
    [|
      "Nothing notable.";
      "Nothing  notable.";
      "Nothing\tnotable.";
      "Nothing notable.  ";
      "  Nothing notable.\n";
    |]
  in
  Array.iteri
    (fun i out ->
      let bg_id = i + 1 in
      stub_cron_run ~db ~job_name:"ws" ~bg_task_id:bg_id;
      let _ = Scheduler.mark_run_output ~db ~bg_task_id:bg_id ~output:out in
      ())
    variations;
  Alcotest.(check bool)
    "whitespace-variant outputs hash identically and disable cron" false
    (job_enabled ~db ~name:"ws")

let test_mark_run_output_non_cron_noop () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  (* bg_task_id 9999 was never registered as a cron run *)
  Alcotest.(check (option string))
    "no-op when bg task is not cron-linked" None
    (Scheduler.mark_run_output ~db ~bg_task_id:9999 ~output:"foo")

(* B665: the inline cron-tick path uses mark_run_output_by_run_id (no
   Background_task hop). Verify identical-output detection fires from that
   path too — same threshold, same disable behavior, just keyed by run_id
   directly. *)
let test_inline_run_output_disables_cron () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"inline" ~session_key:"s" ~message:"m"
       ~schedule:"every 1h" ());
  for _ = 1 to 5 do
    let run_id = Scheduler.record_run_start ~db ~job_name:"inline" in
    Scheduler.mark_run_output_by_run_id ~db ~run_id ~job_name:"inline"
      ~output:"Nothing notable."
  done;
  Alcotest.(check bool)
    "5 identical inline outputs disable the cron" false
    (job_enabled ~db ~name:"inline")

let test_inline_run_output_varying_keeps_enabled () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"inline_varied" ~session_key:"s" ~message:"m"
       ~schedule:"every 1h" ());
  for i = 1 to 5 do
    let run_id = Scheduler.record_run_start ~db ~job_name:"inline_varied" in
    Scheduler.mark_run_output_by_run_id ~db ~run_id ~job_name:"inline_varied"
      ~output:(Printf.sprintf "varied %d" i)
  done;
  Alcotest.(check bool)
    "varying inline outputs keep cron enabled" true
    (job_enabled ~db ~name:"inline_varied")

let test_tick_marks_response_sent_after_turn () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let db = Memory.init ~db_path:":memory:" () in
     Scheduler.init_schema db;
     ignore
       (Scheduler.add_job ~db ~name:"briefing" ~session_key:"cron:briefing"
          ~message:"daily briefing" ~schedule:"every 1s" ());
     (* Create a channelless session row with turn='idle' *)
     let sql =
       "INSERT INTO session_state (session_key, turn) VALUES ('cron:briefing', \
        'idle')"
     in
     ignore (Sqlite3.exec db sql);
     (* Use a config with a provider that fails immediately (connection refused)
        and minimal resilience settings so the turn errors out fast. *)
     let config =
       {
         Runtime_config.default with
         providers =
           [
             ( "openai-codex",
               {
                 Runtime_config.default_provider_config with
                 api_key = "test-key";
                 base_url = Some "http://127.0.0.1:1";
               } );
           ];
         resilience =
           {
             Runtime_config.default.resilience with
             timeout_s = 1.0;
             max_retries = 0;
           };
       }
     in
     let session_mgr = Session.create ~config ~db () in
     let* () = Scheduler.tick ~db ~session_mgr () in
     (* tick uses Lwt.async; give it a chance to run (turn will fail — provider
        connection refused — hitting the error handler which calls
        mark_response_sent) *)
     let* () = Lwt_unix.sleep 2.0 in
     (* Verify turn is reset to 'user', not stuck at 'agent' *)
     Alcotest.(check (option string))
       "turn reset to user" (Some "user")
       (Test_helpers.query_single_text_option db
          "SELECT turn FROM session_state WHERE session_key = 'cron:briefing'");
     (* Verify response_sent_at is set *)
     Alcotest.(check bool)
       "response_sent_at set" true
       (Test_helpers.query_single_text_option db
          "SELECT response_sent_at FROM session_state WHERE session_key = \
           'cron:briefing'"
       <> None);
     Lwt.return_unit)

(* P13.M1.E2.T001: effective_session_key tests *)

let test_effective_session_key_no_profile () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"plain" ~session_key:"default" ~message:"m"
       ~schedule:"every 1m" ());
  match Scheduler.get_job ~db ~name:"plain" with
  | None -> Alcotest.fail "expected job"
  | Some j ->
      let turn_key, delivery_key = Scheduler.effective_session_key ~db j in
      Alcotest.(check string) "turn key" "default" turn_key;
      Alcotest.(check string) "delivery key" "default" delivery_key

let test_effective_session_key_with_profile () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  let profile_db_id = Memory_core.insert_room_profile ~db ~name:"my-room" in
  ignore
    (Scheduler.add_job ~db ~name:"prof" ~session_key:"telegram:42:u"
       ~message:"m" ~schedule:"every 1m" ~profile_id:profile_db_id ());
  match Scheduler.get_job ~db ~name:"prof" with
  | None -> Alcotest.fail "expected job"
  | Some j ->
      let turn_key, delivery_key = Scheduler.effective_session_key ~db j in
      let expected_routine =
        Room_session.make_routine_key ~profile_id:"my-room" ~routine_id:"prof"
          ()
      in
      Alcotest.(check string)
        "turn key is routine key" expected_routine turn_key;
      Alcotest.(check string)
        "delivery key is original" "telegram:42:u" delivery_key

let test_effective_session_key_missing_profile () =
  let db = Memory.init ~db_path:":memory:" () in
  Scheduler.init_schema db;
  ignore
    (Scheduler.add_job ~db ~name:"missing" ~session_key:"slack:C:U" ~message:"m"
       ~schedule:"every 1m" ~profile_id:9999 ());
  match Scheduler.get_job ~db ~name:"missing" with
  | None -> Alcotest.fail "expected job"
  | Some j ->
      let turn_key, delivery_key = Scheduler.effective_session_key ~db j in
      Alcotest.(check string) "turn key falls back" "slack:C:U" turn_key;
      Alcotest.(check string) "delivery key falls back" "slack:C:U" delivery_key

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
    Alcotest.test_case "job routine metadata defaults to none" `Quick
      test_job_routine_metadata_defaults_to_none;
    Alcotest.test_case "job routine metadata round trips" `Quick
      test_job_routine_metadata_round_trips;
    Alcotest.test_case "run history includes routine target" `Quick
      test_run_history_includes_routine_target;
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
    Alcotest.test_case
      "B463/B467/B472: record_run ok_notifier_unconfirmed status" `Quick
      test_record_run_ok_notifier_unconfirmed;
    Alcotest.test_case "B587: toggle_job flips enabled" `Quick
      test_toggle_job_flips_enabled;
    Alcotest.test_case "B587: toggle_job errors for missing job" `Quick
      test_toggle_job_returns_error_for_missing;
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
    Alcotest.test_case "parse_duration_seconds valid" `Quick
      test_parse_duration_seconds;
    Alcotest.test_case "parse_duration_seconds invalid" `Quick
      test_parse_duration_seconds_invalid;
    Alcotest.test_case "add job with TTL" `Quick test_add_job_with_ttl;
    Alcotest.test_case "add job without TTL" `Quick test_add_job_without_ttl;
    Alcotest.test_case "tick skips expired job" `Quick
      test_tick_skips_expired_job;
    Alcotest.test_case "tick runs non-expired job" `Quick
      test_tick_runs_non_expired_job;
    Alcotest.test_case "list jobs includes expires_at" `Quick
      test_list_jobs_includes_expires_at;
    Alcotest.test_case "update job TTL" `Quick test_update_job_ttl;
    Alcotest.test_case "tick marks response_sent after turn" `Quick
      test_tick_marks_response_sent_after_turn;
    Alcotest.test_case "identical-output loop disables cron job" `Quick
      test_identical_output_disables_cron;
    Alcotest.test_case "non-identical outputs keep cron enabled" `Quick
      test_varying_outputs_keep_cron_enabled;
    Alcotest.test_case "empty output is not considered identical" `Quick
      test_empty_output_does_not_trigger;
    Alcotest.test_case "whitespace normalization in identical detection" `Quick
      test_whitespace_normalized;
    Alcotest.test_case "mark_run_output on non-cron task is a no-op" `Quick
      test_mark_run_output_non_cron_noop;
    Alcotest.test_case
      "B665: inline mark_run_output_by_run_id disables cron after threshold"
      `Quick test_inline_run_output_disables_cron;
    Alcotest.test_case
      "B665: inline mark_run_output_by_run_id with varying outputs keeps cron \
       enabled"
      `Quick test_inline_run_output_varying_keeps_enabled;
    Alcotest.test_case "P13.M1.E2.T001: effective_session_key no profile" `Quick
      test_effective_session_key_no_profile;
    Alcotest.test_case "P13.M1.E2.T001: effective_session_key with profile"
      `Quick test_effective_session_key_with_profile;
    Alcotest.test_case "P13.M1.E2.T001: effective_session_key missing profile"
      `Quick test_effective_session_key_missing_profile;
  ]

(* B823: daemon liveness heartbeat + stale-holder takeover tests. *)

let sleep_bin = "/bin/sleep"
let sleep_available () = Sys.file_exists sleep_bin

let read_file path =
  try
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let buf = Buffer.create 256 in
        let chunk = Bytes.create 256 in
        let rec loop () =
          let n = input ic chunk 0 256 in
          if n > 0 then (
            Buffer.add_subbytes buf chunk 0 n;
            loop ())
        in
        (try loop () with End_of_file -> ());
        Some (Buffer.contents buf))
  with _ -> None

let with_env key value f =
  let previous = Sys.getenv_opt key in
  (match value with Some v -> Unix.putenv key v | None -> Unix.putenv key "");
  Fun.protect f ~finally:(fun () ->
      match previous with
      | Some v -> Unix.putenv key v
      | None -> Unix.putenv key "")

(* Spawn a short-lived fake holder process and return its pid. The caller is
   responsible for reaping it (waitpid) after the test. *)
let spawn_fake_holder seconds =
  match Unix.fork () with
  | 0 -> (
      try Unix.execv sleep_bin [| "sleep"; string_of_int seconds |]
      with _ -> exit 127)
  | pid -> pid

let reap pid =
  try
    let rec loop () =
      match Unix.waitpid [ Unix.WNOHANG ] pid with
      | 0, _ ->
          Unix.sleepf 0.02;
          loop ()
      | _, _ -> ()
      | exception Unix.Unix_error (Unix.ECHILD, _, _) -> ()
    in
    loop ()
  with _ -> ()

let wait_until_dead ?(timeout = 5.0) pid =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    if not (Daemon_liveness.process_alive pid) then true
    else if Unix.gettimeofday () >= deadline then false
    else (
      Unix.sleepf 0.05;
      loop ())
  in
  loop ()

(* --- heartbeat file format --- *)

let test_heartbeat_format_roundtrip () =
  let content = Daemon_liveness.format_heartbeat 1750000000.5 in
  Alcotest.(check bool)
    "starts with unix ts" true
    (Test_helpers.string_contains content "1750000000.500");
  Alcotest.(check bool)
    "contains iso time with Z suffix" true
    (Test_helpers.string_contains content "Z");
  Alcotest.(check (option (float 1e-9)))
    "parse recovers ts" (Some 1750000000.5)
    (Daemon_liveness.parse_heartbeat_ts content);
  Alcotest.(check (option (float 1e-9)))
    "parse rejects junk" None
    (Daemon_liveness.parse_heartbeat_ts "not a timestamp");
  Alcotest.(check (option (float 1e-9)))
    "parse rejects empty" None
    (Daemon_liveness.parse_heartbeat_ts "")

let test_heartbeat_write_read_age () =
  Test_helpers.with_temp_home (fun _home ->
      Daemon_liveness.write_heartbeat ~now:1000.0 ();
      Alcotest.(check (option (float 1e-9)))
        "read returns written ts" (Some 1000.0)
        (Daemon_liveness.read_heartbeat_ts ());
      Alcotest.(check (option (float 1e-9)))
        "age is now - ts" (Some 250.5)
        (Daemon_liveness.heartbeat_age ~now:1250.5 ());
      Daemon_liveness.write_heartbeat ~now:2000.0 ();
      Alcotest.(check (option (float 1e-9)))
        "rewrite replaces ts" (Some 2000.0)
        (Daemon_liveness.read_heartbeat_ts ());
      Alcotest.(check bool)
        "no temp file left behind" false
        (Sys.file_exists (Daemon_liveness.heartbeat_path () ^ ".tmp")))

let test_heartbeat_age_clamps_future_ts () =
  Test_helpers.with_temp_home (fun _home ->
      Daemon_liveness.write_heartbeat ~now:3000.0 ();
      Alcotest.(check (option (float 1e-9)))
        "clock-skewed future ts clamps to 0" (Some 0.0)
        (Daemon_liveness.heartbeat_age ~now:1000.0 ()))

let test_stale_threshold_default_and_env () =
  Alcotest.(check (float 1e-9))
    "default threshold is 300s" 300.0
    (Daemon_liveness.stale_threshold_s ());
  with_env Daemon_liveness.stale_env (Some "60") (fun () ->
      Alcotest.(check (float 1e-9))
        "env overrides threshold" 60.0
        (Daemon_liveness.stale_threshold_s ()));
  with_env Daemon_liveness.stale_env (Some "junk") (fun () ->
      Alcotest.(check (float 1e-9))
        "invalid env falls back to default" 300.0
        (Daemon_liveness.stale_threshold_s ()))

(* --- verdict classification (service status) --- *)

let test_classify_holder_verdicts () =
  let threshold = 300.0 in
  (match
     Daemon_liveness.classify_holder ~threshold_s:threshold ~holder_alive:true
       ~heartbeat_age_s:(Some 5.0)
   with
  | Daemon_liveness.Healthy 5.0 -> ()
  | v ->
      Alcotest.failf "expected Healthy 5.0, got %s"
        (Daemon_liveness.verdict_string v));
  (match
     Daemon_liveness.classify_holder ~threshold_s:threshold ~holder_alive:true
       ~heartbeat_age_s:(Some 400.0)
   with
  | Daemon_liveness.Stale 400.0 -> ()
  | v ->
      Alcotest.failf "expected Stale 400.0, got %s"
        (Daemon_liveness.verdict_string v));
  (match
     Daemon_liveness.classify_holder ~threshold_s:threshold ~holder_alive:true
       ~heartbeat_age_s:None
   with
  | Daemon_liveness.NoHeartbeat -> ()
  | v ->
      Alcotest.failf "expected NoHeartbeat, got %s"
        (Daemon_liveness.verdict_string v));
  (match
     Daemon_liveness.classify_holder ~threshold_s:threshold ~holder_alive:false
       ~heartbeat_age_s:(Some 5.0)
   with
  | Daemon_liveness.NoDaemon -> ()
  | v ->
      Alcotest.failf "expected NoDaemon, got %s"
        (Daemon_liveness.verdict_string v));
  Alcotest.(check bool)
    "threshold boundary is stale" true
    (Test_helpers.string_contains
       (Daemon_liveness.verdict_string
          (Daemon_liveness.classify_holder ~threshold_s:300.0 ~holder_alive:true
             ~heartbeat_age_s:(Some 300.0)))
       "stale")

(* --- takeover decision (pure) --- *)

let expect_takeover d =
  match d with
  | Daemon_liveness.Takeover r ->
      Alcotest.(check bool)
        "takeover reason is non-empty" true
        (String.length r > 0)
  | Daemon_liveness.Refuse r ->
      Alcotest.failf "expected Takeover, got Refuse: %s" r

let expect_refuse d =
  match d with
  | Daemon_liveness.Refuse r ->
      Alcotest.(check bool) "refusal is non-empty" true (String.length r > 0)
  | Daemon_liveness.Takeover r ->
      Alcotest.failf "expected Refuse, got Takeover: %s" r

let test_decide_takeover_dead_holder () =
  expect_takeover
    (Daemon_liveness.decide_takeover ~now:1000.0 ~threshold_s:300.0
       ~holder_pid:(Some 4242) ~holder_alive:false ~heartbeat_age_s:(Some 5.0)
       ~orphaned:false)

let test_decide_takeover_no_holder_pid () =
  expect_takeover
    (Daemon_liveness.decide_takeover ~now:1000.0 ~threshold_s:300.0
       ~holder_pid:None ~holder_alive:false ~heartbeat_age_s:None
       ~orphaned:false)

let test_decide_takeover_healthy_refused () =
  expect_refuse
    (Daemon_liveness.decide_takeover ~now:1000.0 ~threshold_s:300.0
       ~holder_pid:(Some 4242) ~holder_alive:true ~heartbeat_age_s:(Some 12.0)
       ~orphaned:false)

let test_decide_takeover_stale_holder () =
  expect_takeover
    (Daemon_liveness.decide_takeover ~now:1000.0 ~threshold_s:300.0
       ~holder_pid:(Some 4242) ~holder_alive:true ~heartbeat_age_s:(Some 500.0)
       ~orphaned:false)

let test_decide_takeover_missing_heartbeat () =
  expect_takeover
    (Daemon_liveness.decide_takeover ~now:1000.0 ~threshold_s:300.0
       ~holder_pid:(Some 4242) ~holder_alive:true ~heartbeat_age_s:None
       ~orphaned:false)

let test_decide_takeover_orphaned_holder_despite_fresh_heartbeat () =
  expect_takeover
    (Daemon_liveness.decide_takeover ~now:1000.0 ~threshold_s:300.0
       ~holder_pid:(Some 4242) ~holder_alive:true ~heartbeat_age_s:(Some 5.0)
       ~orphaned:true)

let test_decide_takeover_refusal_mentions_healthy_daemon () =
  match
    Daemon_liveness.decide_takeover ~now:1000.0 ~threshold_s:300.0
      ~holder_pid:(Some 4242) ~holder_alive:true ~heartbeat_age_s:(Some 12.0)
      ~orphaned:false
  with
  | Daemon_liveness.Refuse r ->
      Alcotest.(check bool)
        "refusal explains a healthy daemon is running" true
        (Test_helpers.string_contains r "healthy")
  | Daemon_liveness.Takeover r ->
      Alcotest.failf "expected Refuse, got Takeover: %s" r

(* --- terminate_holder escalation (injected kill/alive/sleep) --- *)

let test_terminate_holder_sigterm_suffices () =
  let signals = ref [] in
  let alive = ref true in
  let kill pid signal =
    signals := (pid, signal) :: !signals;
    if signal = Sys.sigkill then alive := false
  in
  let sleep_s _ = alive := false in
  let result =
    Daemon_liveness.terminate_holder ~term_wait_s:2.0 ~kill
      ~alive:(fun _ -> !alive)
      ~sleep_s 4242
  in
  (match result with
  | Ok msg ->
      Alcotest.(check bool)
        "outcome mentions SIGTERM" true
        (Test_helpers.string_contains msg "SIGTERM")
  | Error e -> Alcotest.failf "expected Ok, got Error: %s" e);
  Alcotest.(check (list (pair int int)))
    "only SIGTERM sent"
    [ (4242, Sys.sigterm) ]
    !signals

let test_terminate_holder_escalates_to_sigkill () =
  let alive = ref true in
  let kill _pid signal = if signal = Sys.sigkill then alive := false in
  let result =
    Daemon_liveness.terminate_holder ~term_wait_s:0.1 ~kill_wait_s:0.05 ~kill
      ~alive:(fun _ -> !alive)
      ~sleep_s:(fun _ -> ())
      4242
  in
  match result with
  | Ok msg ->
      Alcotest.(check bool)
        "outcome mentions SIGKILL" true
        (Test_helpers.string_contains msg "SIGKILL")
  | Error e -> Alcotest.failf "expected Ok, got Error: %s" e

let test_terminate_holder_reports_unkillable () =
  let result =
    Daemon_liveness.terminate_holder ~term_wait_s:0.1 ~kill_wait_s:0.05
      ~kill:(fun _ _ -> ())
      ~alive:(fun _ -> true)
      ~sleep_s:(fun _ -> ())
      4242
  in
  match result with
  | Error e ->
      Alcotest.(check bool)
        "error mentions the pid" true
        (Test_helpers.string_contains e "4242")
  | Ok msg -> Alcotest.failf "expected Error, got Ok: %s" msg

(* --- lock holder pid recording --- *)

let test_singleton_lock_records_holder_pid () =
  Test_helpers.with_temp_home (fun _home ->
      match Service.acquire_singleton_lock () with
      | None -> Alcotest.fail "expected to acquire lock"
      | Some fd ->
          Alcotest.(check (option int))
            "lock file records holder pid"
            (Some (Unix.getpid ()))
            (Service.read_lock_holder_pid ());
          Service.release_singleton_lock (Some fd);
          Alcotest.(check (option int))
            "lock content survives release"
            (Some (Unix.getpid ()))
            (Service.read_lock_holder_pid ()))

let test_read_holder_pid_from_file () =
  Test_helpers.with_temp_home (fun home ->
      ignore (Dot_dir.ensure ());
      let path = Filename.concat home ".clawq/fake.lock" in
      let oc = open_out path in
      output_string oc "12345\n";
      close_out oc;
      Alcotest.(check (option int))
        "reads pid from lock file" (Some 12345)
        (Daemon_liveness.read_holder_pid path);
      let oc = open_out path in
      output_string oc "garbage\n";
      close_out oc;
      Alcotest.(check (option int))
        "garbage content reads as None" None
        (Daemon_liveness.read_holder_pid path);
      Alcotest.(check (option int))
        "missing file reads as None" None
        (Daemon_liveness.read_holder_pid (path ^ ".missing")))

(* --- takeover execution with a real fake holder --- *)

let test_service_takeover_kills_hung_holder () =
  if not (sleep_available ()) then Alcotest.skip ();
  Test_helpers.with_temp_home (fun _home ->
      let holder = spawn_fake_holder 30 in
      Fun.protect
        ~finally:(fun () -> reap holder)
        (fun () ->
          (* fake holder identity: daemon.pid + meta so read_pid sees it *)
          Service.write_pid holder;
          let old = Unix.gettimeofday () -. 400.0 in
          Daemon_liveness.write_heartbeat ~now:old ();
          (match Service.takeover_unresponsive_daemon ~pid:holder () with
          | Ok note ->
              Alcotest.(check bool)
                "takeover note mentions holder pid" true
                (Test_helpers.string_contains note (string_of_int holder));
              Alcotest.(check bool)
                "takeover note mentions stale heartbeat" true
                (Test_helpers.string_contains note "stale")
          | Error e -> Alcotest.failf "expected takeover, got refusal: %s" e);
          Alcotest.(check bool)
            "holder process is dead after takeover" true
            (wait_until_dead holder);
          Alcotest.(check bool)
            "daemon.pid removed after takeover" false
            (Sys.file_exists (Service.pid_path ()));
          let log =
            match read_file (Service.log_path ()) with
            | Some s -> s
            | None -> ""
          in
          Alcotest.(check bool)
            "takeover logged to daemon.log" true
            (Test_helpers.string_contains log "takeover")))

let test_service_takeover_refuses_healthy_holder () =
  if not (sleep_available ()) then Alcotest.skip ();
  Test_helpers.with_temp_home (fun _home ->
      let holder = spawn_fake_holder 30 in
      Fun.protect
        ~finally:(fun () ->
          (try Unix.kill holder Sys.sigkill with _ -> ());
          reap holder)
        (fun () ->
          Service.write_pid holder;
          Daemon_liveness.write_heartbeat ();
          (match Service.takeover_unresponsive_daemon ~pid:holder () with
          | Error refusal ->
              Alcotest.(check bool)
                "refusal says a healthy daemon is running" true
                (Test_helpers.string_contains refusal "healthy")
          | Ok note -> Alcotest.failf "expected refusal, got takeover: %s" note);
          Alcotest.(check bool)
            "healthy holder still alive" true
            (Daemon_liveness.process_alive holder)))

(* B828: EC children never write the shared daemon heartbeat, so a stale
   daemon heartbeat must not make a live EC holder replaceable. Holder health
   comes from the holder pid itself. *)
let test_ec_takeover_refuses_live_holder_despite_stale_heartbeat () =
  if not (sleep_available ()) then Alcotest.skip ();
  Test_helpers.with_temp_home (fun _home ->
      ignore (Dot_dir.ensure ());
      let holder = spawn_fake_holder 30 in
      Fun.protect
        ~finally:(fun () ->
          (try Unix.kill holder Sys.sigkill with _ -> ());
          reap holder)
        (fun () ->
          let lock_path = Error_watcher.lock_file_path () in
          let oc = open_out lock_path in
          Printf.fprintf oc "%d\n" holder;
          close_out oc;
          let old = Unix.gettimeofday () -. 500.0 in
          Daemon_liveness.write_heartbeat ~now:old ();
          (match Ec_process.resolve_lock_takeover ~orphaned:false () with
          | Error refusal ->
              Alcotest.(check bool)
                "refusal rests on holder pid health" true
                (Test_helpers.string_contains refusal "healthy")
          | Ok note -> Alcotest.failf "expected refusal, got takeover: %s" note);
          Alcotest.(check bool)
            "live holder untouched" true
            (Daemon_liveness.process_alive holder)))

(* B828: a crashed, unreaped EC holder is a zombie; the next ec-run must read
   it as dead and take over the lock instead of refusing. *)
let test_ec_takeover_zombie_holder () =
  Test_helpers.with_temp_home (fun _home ->
      ignore (Dot_dir.ensure ());
      let zombie = match Unix.fork () with 0 -> Unix._exit 0 | pid -> pid in
      Fun.protect
        ~finally:(fun () -> reap zombie)
        (fun () ->
          let deadline = Unix.gettimeofday () +. 5.0 in
          let rec wait_zombie () =
            match Daemon_liveness.proc_state zombie with
            | Some "Z" -> true
            | _ ->
                if Unix.gettimeofday () >= deadline then false
                else (
                  Unix.sleepf 0.02;
                  wait_zombie ())
          in
          Alcotest.(check bool)
            "holder reached zombie state" true (wait_zombie ());
          let lock_path = Error_watcher.lock_file_path () in
          let oc = open_out lock_path in
          Printf.fprintf oc "%d\n" zombie;
          close_out oc;
          Error_watcher.write_pid_file zombie;
          match Ec_process.resolve_lock_takeover () with
          | Ok note ->
              Alcotest.(check bool)
                "zombie holder yields takeover" true
                (Test_helpers.string_contains note (string_of_int zombie))
          | Error e ->
              Alcotest.failf
                "expected takeover of zombie holder, got refusal: %s" e))

let test_ec_takeover_refuses_healthy_holder () =
  if not (sleep_available ()) then Alcotest.skip ();
  Test_helpers.with_temp_home (fun _home ->
      ignore (Dot_dir.ensure ());
      let holder = spawn_fake_holder 30 in
      Fun.protect
        ~finally:(fun () ->
          (try Unix.kill holder Sys.sigkill with _ -> ());
          reap holder)
        (fun () ->
          let lock_path = Error_watcher.lock_file_path () in
          let oc = open_out lock_path in
          Printf.fprintf oc "%d\n" holder;
          close_out oc;
          Daemon_liveness.write_heartbeat ();
          (match Ec_process.resolve_lock_takeover ~orphaned:false () with
          | Error refusal ->
              Alcotest.(check bool)
                "ec refusal mentions healthy daemon" true
                (Test_helpers.string_contains refusal "healthy")
          | Ok note ->
              Alcotest.failf "expected ec refusal, got takeover: %s" note);
          Alcotest.(check bool)
            "ec holder still alive" true
            (Daemon_liveness.process_alive holder)))

let test_ec_takeover_dead_holder_cleans_up () =
  if not (sleep_available ()) then Alcotest.skip ();
  Test_helpers.with_temp_home (fun _home ->
      ignore (Dot_dir.ensure ());
      let short = spawn_fake_holder 0 in
      reap short;
      Alcotest.(check bool)
        "short holder is dead" false
        (Daemon_liveness.process_alive short);
      let lock_path = Error_watcher.lock_file_path () in
      let oc = open_out lock_path in
      Printf.fprintf oc "%d\n" short;
      close_out oc;
      Error_watcher.write_pid_file short;
      match Ec_process.resolve_lock_takeover () with
      | Ok note ->
          Alcotest.(check bool)
            "dead-holder takeover reports cleanup" true
            (Test_helpers.string_contains note (string_of_int short))
      | Error e ->
          Alcotest.failf "expected dead-holder cleanup, got refusal: %s" e)

(* --- service status verdict --- *)

let test_status_verdict_no_daemon () =
  Test_helpers.with_temp_home (fun _home ->
      let status = Service.cmd_status () in
      Alcotest.(check bool)
        "status reports no daemon heartbeat" true
        (Test_helpers.string_contains status "heartbeat: no daemon"))

let test_status_verdict_healthy_and_stale () =
  if not (sleep_available ()) then Alcotest.skip ();
  Test_helpers.with_temp_home (fun _home ->
      let holder = spawn_fake_holder 30 in
      Fun.protect
        ~finally:(fun () ->
          (try Unix.kill holder Sys.sigkill with _ -> ());
          reap holder)
        (fun () ->
          Service.write_pid holder;
          Daemon_liveness.write_heartbeat ();
          let healthy = Service.cmd_status () in
          Alcotest.(check bool)
            "status verdict healthy" true
            (Test_helpers.string_contains healthy "verdict: healthy");
          Alcotest.(check bool)
            "status shows lock holder line" true
            (Test_helpers.string_contains healthy "lock holder");
          let old = Unix.gettimeofday () -. 400.0 in
          Daemon_liveness.write_heartbeat ~now:old ();
          let stale = Service.cmd_status () in
          Alcotest.(check bool)
            "status verdict stale" true
            (Test_helpers.string_contains stale "verdict: stale")))

let test_status_verdict_no_heartbeat_file () =
  if not (sleep_available ()) then Alcotest.skip ();
  Test_helpers.with_temp_home (fun _home ->
      let holder = spawn_fake_holder 30 in
      Fun.protect
        ~finally:(fun () ->
          (try Unix.kill holder Sys.sigkill with _ -> ());
          reap holder)
        (fun () ->
          Service.write_pid holder;
          (* no heartbeat file written: pre-B823 daemon *)
          let status = Service.cmd_status () in
          Alcotest.(check bool)
            "status flags missing heartbeat file" true
            (Test_helpers.string_contains status "heartbeat: file missing")))

(* --- heartbeat loop --- *)

let test_heartbeat_loop_writes_promptly () =
  Test_helpers.with_temp_home (fun _home ->
      Lwt_main.run
        (let open Lwt.Syntax in
         let before = Sys.file_exists (Daemon_liveness.heartbeat_path ()) in
         let stop = Daemon_liveness.start_heartbeat_loop ~interval_s:0.05 () in
         let deadline = Unix.gettimeofday () +. 2.0 in
         let rec wait_for_write () =
           if Sys.file_exists (Daemon_liveness.heartbeat_path ()) then
             Lwt.return_unit
           else if Unix.gettimeofday () >= deadline then
             Lwt.fail_with "heartbeat loop never wrote the file"
           else
             let* () = Lwt_unix.sleep 0.02 in
             wait_for_write ()
         in
         let* () = wait_for_write () in
         let* () = Lwt_unix.sleep 0.15 in
         Alcotest.(check bool) "heartbeat file existed before" false before;
         Alcotest.(check bool)
           "heartbeat file still parses after refreshes" true
           (Daemon_liveness.read_heartbeat_ts () <> None);
         (* stop the loop inside this scheduler run so no background writer
            leaks past the test and into the deleted temp home *)
         stop ();
         let* () = Lwt_unix.sleep 0.12 in
         Lwt.return_unit))

let suite =
  [
    Alcotest.test_case "heartbeat format roundtrip" `Quick
      test_heartbeat_format_roundtrip;
    Alcotest.test_case "heartbeat write read age" `Quick
      test_heartbeat_write_read_age;
    Alcotest.test_case "heartbeat age clamps future ts" `Quick
      test_heartbeat_age_clamps_future_ts;
    Alcotest.test_case "stale threshold default and env" `Quick
      test_stale_threshold_default_and_env;
    Alcotest.test_case "classify holder verdicts" `Quick
      test_classify_holder_verdicts;
    Alcotest.test_case "decide takeover dead holder" `Quick
      test_decide_takeover_dead_holder;
    Alcotest.test_case "decide takeover no holder pid" `Quick
      test_decide_takeover_no_holder_pid;
    Alcotest.test_case "decide takeover healthy refused" `Quick
      test_decide_takeover_healthy_refused;
    Alcotest.test_case "decide takeover stale holder" `Quick
      test_decide_takeover_stale_holder;
    Alcotest.test_case "decide takeover missing heartbeat" `Quick
      test_decide_takeover_missing_heartbeat;
    Alcotest.test_case "decide takeover orphaned holder despite fresh heartbeat"
      `Quick test_decide_takeover_orphaned_holder_despite_fresh_heartbeat;
    Alcotest.test_case "decide takeover refusal mentions healthy daemon" `Quick
      test_decide_takeover_refusal_mentions_healthy_daemon;
    Alcotest.test_case "terminate holder sigterm suffices" `Quick
      test_terminate_holder_sigterm_suffices;
    Alcotest.test_case "terminate holder escalates to sigkill" `Quick
      test_terminate_holder_escalates_to_sigkill;
    Alcotest.test_case "terminate holder reports unkillable" `Quick
      test_terminate_holder_reports_unkillable;
    Alcotest.test_case "singleton lock records holder pid" `Quick
      test_singleton_lock_records_holder_pid;
    Alcotest.test_case "read holder pid from file" `Quick
      test_read_holder_pid_from_file;
    Alcotest.test_case "service takeover kills hung holder" `Quick
      test_service_takeover_kills_hung_holder;
    Alcotest.test_case "service takeover refuses healthy holder" `Quick
      test_service_takeover_refuses_healthy_holder;
    Alcotest.test_case "ec takeover refuses live holder despite stale heartbeat"
      `Quick test_ec_takeover_refuses_live_holder_despite_stale_heartbeat;
    Alcotest.test_case "ec takeover zombie holder" `Quick
      test_ec_takeover_zombie_holder;
    Alcotest.test_case "ec takeover refuses healthy holder" `Quick
      test_ec_takeover_refuses_healthy_holder;
    Alcotest.test_case "ec takeover dead holder cleans up" `Quick
      test_ec_takeover_dead_holder_cleans_up;
    Alcotest.test_case "status verdict no daemon" `Quick
      test_status_verdict_no_daemon;
    Alcotest.test_case "status verdict healthy and stale" `Quick
      test_status_verdict_healthy_and_stale;
    Alcotest.test_case "status verdict no heartbeat file" `Quick
      test_status_verdict_no_heartbeat_file;
    Alcotest.test_case "heartbeat loop writes promptly" `Quick
      test_heartbeat_loop_writes_promptly;
  ]

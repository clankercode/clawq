(* B825: getUpdates long-poll stalls surface as ERROR spam (~65/day).
   Covers the single bounded retry on timeout, the dedup of consecutive
   identical timeout errors to one WARN line per 5-minute window with an
   occurrence count, and poll-loop survival across an unbounded timeout
   stream. The getUpdates transport, clock, and sleep are injected so no test
   performs real I/O or real waits. *)

let timeout_exn = Failure "HTTP timeout in get_with_timeout after 45.0s"

let with_captured_logs f =
  let messages = ref [] in
  let reporter =
    {
      Logs.report =
        (fun _src level ~over k msgf ->
          let k _ =
            over ();
            k ()
          in
          msgf @@ fun ?header:_ ?tags:_ fmt ->
          Format.kasprintf
            (fun msg ->
              messages := (level, msg) :: !messages;
              k ())
            fmt);
    }
  in
  let prev = Logs.reporter () in
  let prev_level = Logs.level () in
  (* The default level (Warning) drops INFO lines; the B636 cancel path logs
     at INFO, so capture at DEBUG and restore afterwards. *)
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter reporter;
  Fun.protect
    ~finally:(fun () ->
      Logs.set_reporter prev;
      Logs.set_level prev_level)
    (fun () ->
      let result = f () in
      (result, List.rev !messages))

(* Only count WARN/ERROR poll-error lines; suppressed repeats log at DEBUG. *)
let poll_error_lines logs =
  List.filter
    (fun (level, msg) ->
      (level = Logs.Warning || level = Logs.Error)
      && String_util.contains msg "Telegram poll error")
    logs

let lines_at_level level logs = List.filter (fun (lvl, _) -> lvl = level) logs

type fake_outcome = [ `Timeout | `Failure of exn | `Ok | `Stop ]

(* Runs [poll_account] against a scripted fake transport. The fake clock
   advances [clock_step] seconds per getUpdates call; `Stop resolves the stop
   promise so the loop terminates on the next iteration. *)
let run_fake_poll ~(fake : int -> fake_outcome) ~clock_step () =
  let time = ref 0.0 in
  let attempts = ref 0 in
  let stop, stopper = Lwt.wait () in
  let session_mgr = Session.create ~config:Runtime_config.default () in
  let account : Runtime_config.telegram_account =
    { bot_token = "test-token"; allow_from = [ "*" ]; totp = None }
  in
  let get_updates ~offset:_ =
    incr attempts;
    time := !time +. clock_step;
    match fake !attempts with
    | `Timeout -> Lwt.fail timeout_exn
    | `Failure exn -> Lwt.fail exn
    | `Ok -> Lwt.return (Telegram.Updates (0, []))
    | `Stop ->
        Lwt.wakeup_later stopper ();
        Lwt.return (Telegram.Updates (0, []))
  in
  let result, logs =
    with_captured_logs (fun () ->
        Lwt_main.run
          (Telegram_poll.poll_account ~get_updates
             ~set_my_commands:(fun () -> Lwt.return_unit)
             ~clock:(fun () -> !time)
             ~sleep:(fun _ -> Lwt.return_unit)
             ~bot_token:"test-token" ~account ~name:"default" ~session_mgr ~stop
             ()))
  in
  (result, !attempts, logs)

let timeout_for attempt = if attempt >= 6 then `Stop else `Timeout

(* Three poll iterations must fully time out before the stop, so the middle
   one lands inside the 5-minute dedup window and the last one after it:
   poll1 logs [seen 1x], poll2 is suppressed, poll3 logs [seen 2x]. *)
let timeout_window_script attempt = if attempt >= 7 then `Stop else `Timeout

let test_consecutive_timeouts_log_one_warn_per_window () =
  let (), attempts, logs =
    run_fake_poll ~fake:timeout_for ~clock_step:60.0 ()
  in
  (* 3 poll iterations: 2 fully time out (2 calls each incl. retry), the third
     is rescued by the retry hitting `Stop. *)
  Alcotest.(check int) "getUpdates attempts" 6 attempts;
  let errors = poll_error_lines logs in
  Alcotest.(check int) "one deduped poll error line" 1 (List.length errors);
  match errors with
  | [] -> Alcotest.fail "expected a deduped poll error line"
  | (level, msg) :: _ ->
      Alcotest.(check bool)
        "timeout spam logs at WARN" true (level = Logs.Warning);
      Alcotest.(check bool)
        "line carries the original timeout" true
        (String_util.contains msg "HTTP timeout in get_with_timeout after 45.0s")

let test_timeout_warn_repeats_after_window_with_count () =
  let (), attempts, logs =
    run_fake_poll ~fake:timeout_window_script ~clock_step:100.0 ()
  in
  Alcotest.(check int) "three timed-out poll iterations" 7 attempts;
  let warns = lines_at_level Logs.Warning (poll_error_lines logs) in
  Alcotest.(check int) "one WARN per 5m window" 2 (List.length warns);
  match List.rev warns with
  | [] -> Alcotest.fail "expected deduped WARN lines"
  | (_, latest) :: _ ->
      Alcotest.(check bool)
        "second window line reports the occurrence count" true
        (String_util.contains latest "[seen 2x]")

let test_timeout_is_retried_once_without_logging () =
  let fake attempt =
    match attempt with 1 -> `Timeout | 2 -> `Ok | _ -> `Stop
  in
  let (), attempts, logs = run_fake_poll ~fake ~clock_step:60.0 () in
  Alcotest.(check int) "timeout retried once then recovered" 3 attempts;
  Alcotest.(check int)
    "recovered timeout logs nothing" 0
    (List.length (poll_error_lines logs))

let test_non_timeout_errors_are_not_retried_and_stay_error () =
  let fake attempt =
    match attempt with
    | 1 -> `Failure (Failure "connect: connection refused")
    | _ -> `Stop
  in
  let (), attempts, logs = run_fake_poll ~fake ~clock_step:60.0 () in
  Alcotest.(check int) "non-timeout failure is not retried" 2 attempts;
  let errors = lines_at_level Logs.Error (poll_error_lines logs) in
  Alcotest.(check int)
    "non-timeout failure still logs ERROR" 1 (List.length errors);
  match errors with
  | [] -> Alcotest.fail "expected an ERROR poll line"
  | (_, msg) :: _ ->
      Alcotest.(check bool)
        "error carries the failure detail" true
        (String_util.contains msg "connect: connection refused")

let test_poll_loop_survives_indefinite_timeout_stream () =
  let fake attempt = if attempt >= 100 then `Stop else `Timeout in
  let (), attempts, logs = run_fake_poll ~fake ~clock_step:1.0 () in
  Alcotest.(check int) "loop kept polling through the stall" 100 attempts;
  Alcotest.(check int)
    "timeout stream produces a single WARN" 1
    (List.length (lines_at_level Logs.Warning (poll_error_lines logs)));
  Alcotest.(check int)
    "timeout stream produces no ERROR" 0
    (List.length (lines_at_level Logs.Error (poll_error_lines logs)))

(* A clean poll breaks the chain: the next timeout must be logged, not
   suppressed as a repeat of the earlier one. *)
let test_clean_poll_resets_the_dedup_chain () =
  let fake attempt =
    match attempt with
    | 1 | 2 -> `Timeout (* poll 1: logs [seen 1x] *)
    | 3 -> `Ok (* poll 2: clean poll resets the chain *)
    | 4 | 5 -> `Timeout (* poll 3: must log again, not suppress *)
    | _ -> `Stop
  in
  let (), attempts, logs = run_fake_poll ~fake ~clock_step:10.0 () in
  Alcotest.(check int) "scripted attempts" 6 attempts;
  let warns = lines_at_level Logs.Warning (poll_error_lines logs) in
  Alcotest.(check int)
    "timeout after a clean poll is logged" 2 (List.length warns);
  Alcotest.(check bool)
    "second timeout starts a fresh count" true
    (match List.rev warns with
    | (_, latest) :: _ -> String_util.contains latest "[seen 1x]"
    | [] -> false)

(* A cancellation arriving during the retry must take the B636 INFO path,
   not log a poll error. *)
let test_cancelled_retry_logs_info_not_poll_error () =
  let fake attempt =
    match attempt with 1 -> `Timeout | 2 -> `Failure Lwt.Canceled | _ -> `Stop
  in
  let (), attempts, logs = run_fake_poll ~fake ~clock_step:60.0 () in
  Alcotest.(check int) "timeout then cancelled retry then stop" 3 attempts;
  Alcotest.(check int)
    "no poll error logged" 0
    (List.length (poll_error_lines logs));
  Alcotest.(check int)
    "cancel logged at INFO" 1
    (List.length
       (List.filter
          (fun (level, msg) ->
            level = Logs.Info
            && String_util.contains msg "Telegram poll cancelled")
          logs))

let test_poll_request_keeps_timeout_margin_and_uri_params () =
  (* B825 decision 3: the HTTP budget must stay timeout+15s and the timeout
     handed to Telegram must match what the poll loop asks for. *)
  let timeout = Telegram_poll.poll_timeout_s in
  Alcotest.(check (float 0.001))
    "request timeout keeps the +15s margin" 45.0
    (Telegram.poll_request_timeout_s ~timeout);
  let uri = Telegram.get_updates_uri ~bot_token:"tok" ~offset:7 ~timeout in
  Alcotest.(check bool)
    "uri asks telegram for the poll long-poll timeout" true
    (String_util.contains uri (Printf.sprintf "timeout=%d" timeout));
  Alcotest.(check bool)
    "uri resumes from the poll offset" true
    (String_util.contains uri "offset=7")

let suite =
  [
    Alcotest.test_case "consecutive timeouts log one WARN per window" `Quick
      test_consecutive_timeouts_log_one_warn_per_window;
    Alcotest.test_case "timeout WARN repeats after window with count" `Quick
      test_timeout_warn_repeats_after_window_with_count;
    Alcotest.test_case "timeout is retried once without logging" `Quick
      test_timeout_is_retried_once_without_logging;
    Alcotest.test_case "non-timeout errors are not retried and stay ERROR"
      `Quick test_non_timeout_errors_are_not_retried_and_stay_error;
    Alcotest.test_case "poll loop survives an indefinite timeout stream" `Quick
      test_poll_loop_survives_indefinite_timeout_stream;
    Alcotest.test_case "clean poll resets the dedup chain" `Quick
      test_clean_poll_resets_the_dedup_chain;
    Alcotest.test_case "cancelled retry logs INFO not a poll error" `Quick
      test_cancelled_retry_logs_info_not_poll_error;
    Alcotest.test_case "poll request keeps timeout margin and uri params" `Quick
      test_poll_request_keeps_timeout_margin_and_uri_params;
  ]

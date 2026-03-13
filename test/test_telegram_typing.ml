let test_typing_loop_calls_send_action () =
  let call_count = ref 0 in
  let send_action () =
    incr call_count;
    Lwt.return_unit
  in
  let p, resolve = Lwt.wait () in
  let result_p = Telegram.typing_loop ~send_action ~interval:0.01 p in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_unix.sleep 0.05 in
     Lwt.wakeup resolve "done";
     let* result = result_p in
     Alcotest.(check string) "promise value forwarded" "done" result;
     Alcotest.(check bool)
       "send_action called at least twice" true (!call_count >= 2);
     Lwt.return_unit)

let test_typing_loop_stops_when_promise_resolves () =
  let call_count = ref 0 in
  let send_action () =
    incr call_count;
    Lwt.return_unit
  in
  let p, resolve = Lwt.wait () in
  let result_p = Telegram.typing_loop ~send_action ~interval:0.01 p in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_unix.sleep 0.03 in
     Lwt.wakeup resolve "ok";
     let* _result = result_p in
     let count_at_stop = !call_count in
     let* () = Lwt_unix.sleep 0.05 in
     Alcotest.(check int)
       "no further calls after stop" count_at_stop !call_count;
     Lwt.return_unit)

let test_typing_loop_survives_send_failure () =
  let call_count = ref 0 in
  let send_action () =
    incr call_count;
    if !call_count <= 2 then Lwt.fail_with "network error" else Lwt.return_unit
  in
  let p, resolve = Lwt.wait () in
  let result_p = Telegram.typing_loop ~send_action ~interval:0.01 p in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_unix.sleep 0.08 in
     Lwt.wakeup resolve "survived";
     let* result = result_p in
     Alcotest.(check string) "result preserved despite errors" "survived" result;
     Alcotest.(check bool) "loop continued past failures" true (!call_count > 2);
     Lwt.return_unit)

let test_typing_loop_propagates_promise_rejection () =
  let send_action () = Lwt.return_unit in
  let p, resolve = Lwt.wait () in
  let result_p = Telegram.typing_loop ~send_action ~interval:0.01 p in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_unix.sleep 0.02 in
     Lwt.wakeup_exn resolve (Failure "test error");
     let* raised =
       Lwt.catch
         (fun () ->
           let* _result = result_p in
           Lwt.return false)
         (fun _exn -> Lwt.return true)
     in
     Alcotest.(check bool) "exception propagated" true raised;
     Lwt.return_unit)

let test_chat_action_for_tool () =
  Alcotest.(check string)
    "file_write -> upload_document" "upload_document"
    (Telegram.chat_action_for_tool "file_write");
  Alcotest.(check string)
    "file_edit -> upload_document" "upload_document"
    (Telegram.chat_action_for_tool "file_edit");
  Alcotest.(check string)
    "web_fetch -> find_location" "find_location"
    (Telegram.chat_action_for_tool "web_fetch");
  Alcotest.(check string)
    "web_search -> find_location" "find_location"
    (Telegram.chat_action_for_tool "web_search");
  Alcotest.(check string)
    "http_get -> find_location" "find_location"
    (Telegram.chat_action_for_tool "http_get");
  Alcotest.(check string)
    "http_request -> find_location" "find_location"
    (Telegram.chat_action_for_tool "http_request");
  Alcotest.(check string)
    "transcribe -> record_voice" "record_voice"
    (Telegram.chat_action_for_tool "transcribe");
  Alcotest.(check string)
    "shell_exec -> typing" "typing"
    (Telegram.chat_action_for_tool "shell_exec");
  Alcotest.(check string)
    "unknown -> typing" "typing"
    (Telegram.chat_action_for_tool "anything_else")

let test_send_chat_action_is_not_serialized_by_outbound_lock () =
  let mutex = Telegram.get_outbound_mutex "chat-lock" in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock mutex in
     let finished = ref false in
     let action_p =
       Lwt.catch
         (fun () ->
           let* () =
             Telegram.send_chat_action ~bot_token:"token" ~chat_id:"chat-lock"
               ~action:"typing"
           in
           finished := true;
           Lwt.return_unit)
         (fun _exn ->
           finished := true;
           Lwt.return_unit)
     in
     let* () = Lwt_unix.sleep 0.02 in
     Alcotest.(check bool)
       "send_chat_action completes even while outbound mutex is held" true
       !finished;
     Lwt_mutex.unlock mutex;
     let* () = action_p in
     Lwt.return_unit)

let test_with_outbound_lock_blocks_until_mutex_released () =
  let mutex = Telegram.get_outbound_mutex "chat-lock-blocked" in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_mutex.lock mutex in
     let finished = ref false in
     let blocked_p =
       Telegram.with_outbound_lock ~chat_id:"chat-lock-blocked" (fun () ->
           finished := true;
           Lwt.return_unit)
     in
     let* () = Lwt_unix.sleep 0.02 in
     Alcotest.(check bool)
       "with_outbound_lock remains blocked while mutex held" false !finished;
     Lwt_mutex.unlock mutex;
     let* () = blocked_p in
     Alcotest.(check bool)
       "with_outbound_lock runs after release" true !finished;
     Lwt.return_unit)

let test_refreshable_typing_can_overlap_outbound_lock () =
  let mutex = Telegram.get_outbound_mutex "chat-refresh" in
  Lwt_main.run
    (let open Lwt.Syntax in
     let done_p, done_u = Lwt.wait () in
     let call_count = ref 0 in
     let loop_p, refresh =
       Telegram.typing_loop_refreshable
         ~send_action:(fun () ->
           incr call_count;
           Lwt.return_unit)
         ~interval:10.0 done_p
     in
     let* () = Lwt_mutex.lock mutex in
     refresh ();
     let* () = Lwt_unix.sleep 0.02 in
     Alcotest.(check bool)
       "refresh-driven typing send is not blocked by outbound mutex" true
       (!call_count > 0);
     Lwt_mutex.unlock mutex;
     Lwt.wakeup done_u ();
     let* () = loop_p in
     Lwt.return_unit)

let test_refreshable_loop_calls_send_action () =
  let call_count = ref 0 in
  let send_action () =
    incr call_count;
    Lwt.return_unit
  in
  let p, resolve = Lwt.wait () in
  let result_p, _refresh =
    Telegram.typing_loop_refreshable ~send_action ~interval:0.01 p
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_unix.sleep 0.05 in
     Lwt.wakeup resolve "done";
     let* result = result_p in
     Alcotest.(check string) "promise value forwarded" "done" result;
     Alcotest.(check bool)
       "send_action called at least twice" true (!call_count >= 2);
     Lwt.return_unit)

let test_refreshable_loop_refresh_triggers_immediate_send () =
  let call_times = ref [] in
  let send_action () =
    call_times := Unix.gettimeofday () :: !call_times;
    Lwt.return_unit
  in
  let p, resolve = Lwt.wait () in
  let result_p, refresh =
    Telegram.typing_loop_refreshable ~send_action ~interval:10.0 p
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_unix.sleep 0.02 in
     let count_before = List.length !call_times in
     refresh ();
     let* () = Lwt_unix.sleep 0.02 in
     let count_after = List.length !call_times in
     Alcotest.(check bool)
       "refresh caused additional send" true
       (count_after > count_before);
     Lwt.wakeup resolve "ok";
     let* _result = result_p in
     Lwt.return_unit)

let test_refreshable_loop_stops_on_resolve () =
  let call_count = ref 0 in
  let send_action () =
    incr call_count;
    Lwt.return_unit
  in
  let p, resolve = Lwt.wait () in
  let result_p, refresh =
    Telegram.typing_loop_refreshable ~send_action ~interval:0.01 p
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_unix.sleep 0.03 in
     Lwt.wakeup resolve "ok";
     let* _result = result_p in
     let count_at_stop = !call_count in
     refresh ();
     let* () = Lwt_unix.sleep 0.05 in
     Alcotest.(check int)
       "no further calls after stop" count_at_stop !call_count;
     Lwt.return_unit)

let test_deferred_typing_skips_for_fast_promise () =
  let call_count = ref 0 in
  let send_action () =
    incr call_count;
    Lwt.return_unit
  in
  let p = Lwt.return "fast" in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* result =
       Telegram.typing_loop_deferred ~send_action ~interval:0.01 ~grace:0.1 p
     in
     Alcotest.(check string) "fast result" "fast" result;
     Alcotest.(check int) "no typing sent for fast resolve" 0 !call_count;
     Lwt.return_unit)

let test_deferred_typing_starts_for_slow_promise () =
  let call_count = ref 0 in
  let send_action () =
    incr call_count;
    Lwt.return_unit
  in
  let p, resolve = Lwt.wait () in
  Lwt_main.run
    (let open Lwt.Syntax in
     let result_p =
       Telegram.typing_loop_deferred ~send_action ~interval:0.01 ~grace:0.02 p
     in
     let* () = Lwt_unix.sleep 0.08 in
     Lwt.wakeup resolve "slow";
     let* result = result_p in
     Alcotest.(check string) "slow result" "slow" result;
     Alcotest.(check bool)
       "typing was sent for slow promise" true (!call_count >= 2);
     Lwt.return_unit)

let test_live_activity_typing_follows_session_state () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "telegram:1:u" in
  let call_count = ref 0 in
  let refresh_trigger = Lwt_condition.create () in
  Lwt_main.run
    (let open Lwt.Syntax in
     let loop_p =
       Telegram.typing_loop_live_activity
         ~current_activity:(fun () -> Session.current_live_activity mgr ~key)
         ~wait_for_change:(fun ~after_generation ->
           Session.wait_for_live_activity_change mgr ~key ~after_generation)
         ~wait_for_refresh:(fun () -> Lwt_condition.wait refresh_trigger)
         ~send_action:(fun () ->
           incr call_count;
           Lwt.return_unit)
         ~interval:0.01 ~idle_timeout:0.05 ()
     in
     let* () = Lwt_unix.sleep 0.02 in
     Alcotest.(check int) "inactive session sends nothing" 0 !call_count;
     let* () =
       Session.with_live_activity mgr ~key (fun () ->
           let* () = Lwt_unix.sleep 0.04 in
           Alcotest.(check bool)
             "active session sends typing" true (!call_count >= 2);
           let count_before_refresh = !call_count in
           Lwt_condition.broadcast refresh_trigger ();
           let* () = Lwt_unix.sleep 0.02 in
           Alcotest.(check bool)
             "refresh triggers immediate resend" true
             (!call_count > count_before_refresh);
           Lwt.return_unit)
     in
     let count_at_stop = !call_count in
     let* () = Lwt_unix.sleep 0.02 in
     Alcotest.(check int)
       "typing stops after activity ends" count_at_stop !call_count;
     let* () = loop_p in
     Lwt.return_unit)

let suite =
  [
    Alcotest.test_case "typing loop calls send_action repeatedly" `Quick
      test_typing_loop_calls_send_action;
    Alcotest.test_case "typing loop stops when promise resolves" `Quick
      test_typing_loop_stops_when_promise_resolves;
    Alcotest.test_case "typing loop survives send failures" `Quick
      test_typing_loop_survives_send_failure;
    Alcotest.test_case "typing loop propagates promise rejection" `Quick
      test_typing_loop_propagates_promise_rejection;
    Alcotest.test_case "chat_action_for_tool maps tool names" `Quick
      test_chat_action_for_tool;
    Alcotest.test_case "send_chat_action bypasses outbound lock" `Quick
      test_send_chat_action_is_not_serialized_by_outbound_lock;
    Alcotest.test_case "with_outbound_lock blocks until release" `Quick
      test_with_outbound_lock_blocks_until_mutex_released;
    Alcotest.test_case "refreshable typing overlaps outbound lock" `Quick
      test_refreshable_typing_can_overlap_outbound_lock;
    Alcotest.test_case "refreshable loop sends repeatedly" `Quick
      test_refreshable_loop_calls_send_action;
    Alcotest.test_case "refreshable loop refresh triggers immediate send" `Quick
      test_refreshable_loop_refresh_triggers_immediate_send;
    Alcotest.test_case "refreshable loop stops on resolve" `Quick
      test_refreshable_loop_stops_on_resolve;
    Alcotest.test_case "deferred typing skips for fast promise" `Quick
      test_deferred_typing_skips_for_fast_promise;
    Alcotest.test_case "deferred typing starts for slow promise" `Quick
      test_deferred_typing_starts_for_slow_promise;
    Alcotest.test_case "live activity typing follows session state" `Quick
      test_live_activity_typing_follows_session_state;
  ]

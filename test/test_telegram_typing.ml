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
     (* Let the loop fire a few times *)
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
     (* Wait and verify count hasn't increased (loop was cancelled) *)
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
     (* Let the loop fire several times including failures *)
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
    "transcribe -> record_voice" "record_voice"
    (Telegram.chat_action_for_tool "transcribe");
  Alcotest.(check string)
    "shell_exec -> typing" "typing"
    (Telegram.chat_action_for_tool "shell_exec");
  Alcotest.(check string)
    "unknown -> typing" "typing"
    (Telegram.chat_action_for_tool "anything_else")

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
     (* Initial send fires immediately *)
     let* () = Lwt_unix.sleep 0.02 in
     let count_before = List.length !call_times in
     (* Trigger a refresh — should cause an immediate re-send *)
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
     (* Refresh after stop should be harmless *)
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
  (* Promise that resolves almost instantly *)
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
     (* Start the deferred loop with a short grace period *)
     let result_p =
       Telegram.typing_loop_deferred ~send_action ~interval:0.01 ~grace:0.02 p
     in
     (* Let the grace period elapse and typing loop fire a few times *)
     let* () = Lwt_unix.sleep 0.08 in
     Lwt.wakeup resolve "slow";
     let* result = result_p in
     Alcotest.(check string) "slow result" "slow" result;
     Alcotest.(check bool)
       "typing was sent for slow promise" true (!call_count >= 2);
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
  ]

(* Tests for the shared Typing_indicator module *)

let test_live_activity_typing_follows_session_state () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "typing_ind:test:1" in
  let call_count = ref 0 in
  let refresh_trigger = Lwt_condition.create () in
  Lwt_main.run
    (let open Lwt.Syntax in
     let loop_p =
       Typing_indicator.typing_loop_live_activity
         ~current_activity:(fun () -> Session.current_live_activity mgr ~key)
         ~wait_for_change:(fun ~after_generation ->
           Session.wait_for_live_activity_change mgr ~key ~after_generation)
         ~wait_for_refresh:(fun () -> Lwt_condition.wait refresh_trigger)
         ~send_action:(fun () ->
           incr call_count;
           Lwt.return_unit)
         ~interval:0.01 ~idle_timeout:0.05 ()
     in
     (* Inactive: no calls *)
     let* () = Lwt_unix.sleep 0.02 in
     Alcotest.(check int) "inactive session sends nothing" 0 !call_count;
     (* Active: sends typing *)
     let* () =
       Session.with_live_activity mgr ~key (fun () ->
           let* () = Lwt_unix.sleep 0.04 in
           Alcotest.(check bool) "active sends typing" true (!call_count >= 2);
           (* Refresh triggers immediate resend *)
           let count_before = !call_count in
           Lwt_condition.broadcast refresh_trigger ();
           let* () = Lwt_unix.sleep 0.02 in
           Alcotest.(check bool)
             "refresh triggers resend" true
             (!call_count > count_before);
           Lwt.return_unit)
     in
     (* After activity stops: no more sends *)
     let count_at_stop = !call_count in
     let* () = Lwt_unix.sleep 0.02 in
     Alcotest.(check int)
       "typing stops after activity ends" count_at_stop !call_count;
     let* () = loop_p in
     Lwt.return_unit)

let test_ensure_watcher_deduplicates () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "typing_ind:dedup:1" in
  let call_count = ref 0 in
  let send_action () =
    incr call_count;
    Lwt.return_unit
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let w1 =
       Typing_indicator.ensure_session_typing_watcher ~session_mgr:mgr ~key
         ~send_action ~interval:0.01 ~idle_timeout:0.05
     in
     let w2 =
       Typing_indicator.ensure_session_typing_watcher ~session_mgr:mgr ~key
         ~send_action ~interval:0.01 ~idle_timeout:0.05
     in
     (* Both calls return watchers but only one loop runs *)
     ignore w1;
     ignore w2;
     let* () =
       Session.with_live_activity mgr ~key (fun () -> Lwt_unix.sleep 0.04)
     in
     let count = !call_count in
     Alcotest.(check bool) "watcher ran" true (count >= 2);
     (* Wait for idle timeout so watcher cleans up *)
     let* () = Lwt_unix.sleep 0.08 in
     Lwt.return_unit)

let test_watcher_refresh_after_send () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "typing_ind:refresh:1" in
  let call_count = ref 0 in
  let watcher =
    Typing_indicator.ensure_session_typing_watcher ~session_mgr:mgr ~key
      ~send_action:(fun () ->
        incr call_count;
        Lwt.return_unit)
      ~interval:0.01 ~idle_timeout:0.1
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Session.with_live_activity mgr ~key (fun () ->
           let* () = Lwt_unix.sleep 0.03 in
           let count_before = !call_count in
           watcher.refresh ();
           let* () = Lwt_unix.sleep 0.02 in
           Alcotest.(check bool)
             "refresh causes additional send" true
             (!call_count > count_before);
           Lwt.return_unit)
     in
     let* () = Lwt_unix.sleep 0.15 in
     Lwt.return_unit)

let test_send_action_failure_does_not_crash () =
  let config = Runtime_config.default in
  let mgr = Session.create ~config () in
  let key = "typing_ind:fail:1" in
  let call_count = ref 0 in
  Lwt_main.run
    (let open Lwt.Syntax in
     let loop_p =
       Typing_indicator.typing_loop_live_activity
         ~current_activity:(fun () -> Session.current_live_activity mgr ~key)
         ~wait_for_change:(fun ~after_generation ->
           Session.wait_for_live_activity_change mgr ~key ~after_generation)
         ~wait_for_refresh:(fun () ->
           let p, _u = Lwt.wait () in
           p)
         ~send_action:(fun () ->
           incr call_count;
           Lwt.fail_with "network error")
         ~interval:0.01 ~idle_timeout:0.05 ()
     in
     let* () =
       Session.with_live_activity mgr ~key (fun () -> Lwt_unix.sleep 0.04)
     in
     Alcotest.(check bool)
       "send_action was called despite failures" true (!call_count >= 2);
     let* () = loop_p in
     Lwt.return_unit)

let suite =
  [
    Alcotest.test_case "live activity typing follows session state" `Quick
      test_live_activity_typing_follows_session_state;
    Alcotest.test_case "ensure_watcher deduplicates" `Quick
      test_ensure_watcher_deduplicates;
    Alcotest.test_case "watcher refresh after send" `Quick
      test_watcher_refresh_after_send;
    Alcotest.test_case "send_action failure does not crash" `Quick
      test_send_action_failure_does_not_crash;
  ]

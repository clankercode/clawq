(* B830k regression: the background-task poll loop's wakeup wait must cancel
   its losing branch. The pre-fix shape used [Lwt.choose
   [Lwt_condition.wait cond; Lwt_unix.sleep 5.0]]: when the tick won, the
   losing condition waiter was never cancelled, so it stayed on the
   condition's waiter list forever. One ~49-word waiter per 5s tick retained
   ≈ 35K words/hour — the B830 hourly live-words leak. [Daemon_runtime_loops.
   wait_for_enqueue_or_tick] must keep [Lwt.pick] so the loser is cancelled
   and Lwt_condition drops it from its waiter list. *)

let live_words_after_settle () =
  (* an engine iteration lets transient (cancelled-sleep) debris clear, as in
     the always-iterating daemon *)
  ignore (Lwt_main.run (Lwt_unix.sleep 0.01));
  Gc.full_major ();
  (Gc.quick_stat ()).Gc.live_words

let run_waits cond n =
  Lwt_main.run
    (Lwt_list.iter_s
       (fun _ ->
         Daemon_runtime_loops.wait_for_enqueue_or_tick ~tick_s:0.0001 cond ())
       (List.init n (fun i -> i)))

let tick_resolves_without_signal () =
  let cond : unit Lwt_condition.t = Lwt_condition.create () in
  let resolved = ref false in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Lwt.map
         (fun () -> resolved := true)
         (Daemon_runtime_loops.wait_for_enqueue_or_tick ~tick_s:0.01 cond ())
     in
     Lwt.return_unit);
  Alcotest.(check bool) "tick path resolves the wait" true !resolved

let signal_resolves_immediately () =
  let cond : unit Lwt_condition.t = Lwt_condition.create () in
  let resolved = ref false in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_unix.sleep 0.01 in
     Lwt.async (fun () ->
         let* () =
           Daemon_runtime_loops.wait_for_enqueue_or_tick ~tick_s:30.0 cond ()
         in
         resolved := true;
         Lwt.return_unit);
     let* () = Lwt_unix.sleep 0.02 in
     Lwt_condition.signal cond ();
     let* () = Lwt_unix.sleep 0.05 in
     Lwt.return_unit);
  Alcotest.(check bool) "signal path resolves the wait" true !resolved

let tick_resolved_waits_retain_nothing () =
  let cond : unit Lwt_condition.t = Lwt_condition.create () in
  run_waits cond 50;
  ignore (live_words_after_settle ());
  let before = live_words_after_settle () in
  run_waits cond 300;
  let after = live_words_after_settle () in
  (* The pre-fix [Lwt.choose] shape retains ~49 words per wait (~14,700 words
     for this batch). 1,024 tolerates noise from unrelated pending asyncs in
     the shared test process while still failing loudly (14x) on a [choose]
     regression. *)
  Alcotest.(check bool)
    (Printf.sprintf "live words grew by %d across 300 tick-resolved waits"
       (after - before))
    true
    (after - before < 1024)

let suite =
  [
    Alcotest.test_case "tick resolves wait without signal" `Quick
      tick_resolves_without_signal;
    Alcotest.test_case "signal resolves wait immediately" `Quick
      signal_resolves_immediately;
    Alcotest.test_case "tick-resolved waits retain no condition waiters" `Quick
      tick_resolved_waits_retain_nothing;
  ]

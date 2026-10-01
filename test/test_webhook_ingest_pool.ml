(* B830: bounded webhook ingest pool — concurrency bound, drop-oldest
   overflow, and live worker resize.

   Ordering is driven by Lwt.wait gates and Lwt.pause yields only (no real
   sleeps, per test/CLAUDE.md fast-test discipline). *)

let pool_bounds_concurrency_to_worker_count () =
  let pool = Webhook_ingest_pool.create ~workers:4 ~max_queue:1000 () in
  let running = ref 0 and peak = ref 0 and completed = ref 0 in
  let job () =
    let open Lwt.Syntax in
    incr running;
    peak := max !peak !running;
    let* () = Lwt.pause () in
    decr running;
    incr completed;
    Lwt.return_unit
  in
  for _ = 1 to 20 do
    let (_ : unit Lwt.t) = Webhook_ingest_pool.enqueue pool job in
    ()
  done;
  Lwt_main.run (Webhook_ingest_pool.wait_idle pool);
  Alcotest.(check int) "all 20 jobs ran" 20 !completed;
  Alcotest.(check bool)
    "max observed concurrency within bound"
    (!peak <= 4 && !peak >= 2)
    true;
  let queued, running_now, dropped = Webhook_ingest_pool.stats pool in
  Alcotest.(check int) "queue drained" 0 queued;
  Alcotest.(check int) "nothing running" 0 running_now;
  Alcotest.(check int) "nothing dropped under bound" 0 dropped

let pool_drops_oldest_when_queue_overflows () =
  let pool = Webhook_ingest_pool.create ~workers:1 ~max_queue:2 () in
  let ran = ref [] in
  let gate, release = Lwt.wait () in
  (* job 0 occupies the only worker; started0 resolves once it is running *)
  let started0, started0_resolver = Lwt.wait () in
  let job i () =
    let open Lwt.Syntax in
    if i = 0 then Lwt.wakeup started0_resolver ();
    ran := i :: !ran;
    let* () = gate in
    Lwt.return_unit
  in
  let (_ : unit Lwt.t) = Webhook_ingest_pool.enqueue pool (job 0) in
  Lwt_main.run started0;
  (* queue cap 2: jobs 1..5 overflow; the newest two (4, 5) survive *)
  for i = 1 to 5 do
    let (_ : unit Lwt.t) = Webhook_ingest_pool.enqueue pool (job i) in
    ()
  done;
  Lwt.wakeup release ();
  Lwt_main.run (Webhook_ingest_pool.wait_idle pool);
  let executed = List.sort compare !ran in
  Alcotest.(check (list int))
    "newest jobs kept, oldest dropped" [ 0; 4; 5 ] executed;
  let _, _, dropped = Webhook_ingest_pool.stats pool in
  Alcotest.(check int) "three drops recorded" 3 dropped

let pool_configure_resizes_workers () =
  let pool = Webhook_ingest_pool.create ~workers:1 ~max_queue:100 () in
  let gate, release = Lwt.wait () in
  (* each started promise resolves when its job begins running *)
  let mk_job () =
    let started, resolver = Lwt.wait () in
    let job () =
      let open Lwt.Syntax in
      Lwt.wakeup resolver ();
      let* () = gate in
      Lwt.return_unit
    in
    (job, started)
  in
  let blocker, blocker_started = mk_job () in
  let (_ : unit Lwt.t) = Webhook_ingest_pool.enqueue pool blocker in
  Lwt_main.run blocker_started;
  Lwt_main.run (Webhook_ingest_pool.configure pool ~workers:4 ~max_queue:100);
  (* job 0 still blocks the single original worker; the three new jobs can
     only start if the resized pool really has more workers — the join below
     resolving before release proves at least 4 concurrent jobs *)
  let extra = List.init 3 (fun _ -> mk_job ()) in
  List.iter
    (fun (job, _) ->
      let (_ : unit Lwt.t) = Webhook_ingest_pool.enqueue pool job in
      ())
    extra;
  let (_ : unit) =
    Lwt_main.run (Lwt.join (List.map (fun (_, started) -> started) extra))
  in
  Lwt.wakeup release ();
  Lwt_main.run (Webhook_ingest_pool.wait_idle pool);
  let _, running_after, _ = Webhook_ingest_pool.stats pool in
  Alcotest.(check int) "pool idle after resize test" 0 running_after

let suite =
  [
    Alcotest.test_case "pool bounds concurrency to worker count" `Quick
      pool_bounds_concurrency_to_worker_count;
    Alcotest.test_case "pool drops oldest when queue overflows" `Quick
      pool_drops_oldest_when_queue_overflows;
    Alcotest.test_case "pool configure resizes workers" `Quick
      pool_configure_resizes_workers;
  ]

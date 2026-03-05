let run_lwt f = Lwt_main.run (f ())

let test_within_limit () =
  run_lwt (fun () ->
      let open Lwt.Syntax in
      let lim = Rate_limiter.create ~rate_per_minute:60 ~burst_multiplier:1.0 in
      let rec check n =
        if n = 0 then Lwt.return_unit
        else
          let* ok = Rate_limiter.check_and_consume lim ~key:"ip1" in
          Alcotest.(check bool) "should be allowed" true ok;
          check (n - 1)
      in
      check 5)

let test_over_limit () =
  run_lwt (fun () ->
      let open Lwt.Syntax in
      let lim = Rate_limiter.create ~rate_per_minute:3 ~burst_multiplier:1.0 in
      let rec exhaust n =
        if n = 0 then Lwt.return_unit
        else
          let* _ok = Rate_limiter.check_and_consume lim ~key:"ip1" in
          exhaust (n - 1)
      in
      let* () = exhaust 3 in
      let* ok = Rate_limiter.check_and_consume lim ~key:"ip1" in
      Alcotest.(check bool) "should be rejected" false ok;
      Lwt.return_unit)

let test_refill () =
  run_lwt (fun () ->
      let open Lwt.Syntax in
      (* Use a high rate so even a small sleep refills at least 1 token *)
      let lim =
        Rate_limiter.create ~rate_per_minute:6000 ~burst_multiplier:1.0
      in
      let rec exhaust n =
        if n = 0 then Lwt.return_unit
        else
          let* _ok = Rate_limiter.check_and_consume lim ~key:"ip1" in
          exhaust (n - 1)
      in
      (* 6000 rpm = 100/s, so max_tokens = 6000. Exhaust them all. *)
      let* () = exhaust 6000 in
      let* rejected = Rate_limiter.check_and_consume lim ~key:"ip1" in
      Alcotest.(check bool) "exhausted" false rejected;
      (* Wait 0.1s => ~10 tokens refilled *)
      let* () = Lwt_unix.sleep 0.1 in
      let* ok = Rate_limiter.check_and_consume lim ~key:"ip1" in
      Alcotest.(check bool) "should pass after refill" true ok;
      Lwt.return_unit)

let test_burst () =
  run_lwt (fun () ->
      let open Lwt.Syntax in
      let lim = Rate_limiter.create ~rate_per_minute:10 ~burst_multiplier:2.0 in
      (* burst allows 20 tokens initially *)
      let rec exhaust n allowed =
        if n = 0 then Lwt.return allowed
        else
          let* ok = Rate_limiter.check_and_consume lim ~key:"ip1" in
          exhaust (n - 1) (if ok then allowed + 1 else allowed)
      in
      let* allowed = exhaust 25 0 in
      Alcotest.(check bool) "burst allows 20" true (allowed = 20);
      Lwt.return_unit)

let test_cleanup () =
  run_lwt (fun () ->
      let open Lwt.Syntax in
      let lim = Rate_limiter.create ~rate_per_minute:60 ~burst_multiplier:1.0 in
      let* _ok = Rate_limiter.check_and_consume lim ~key:"a" in
      let* _ok = Rate_limiter.check_and_consume lim ~key:"b" in
      Alcotest.(check int) "2 buckets" 2 (Rate_limiter.bucket_count lim);
      (* Sleep briefly so entries become "old" relative to 0s idle *)
      let* () = Lwt_unix.sleep 0.01 in
      let* () = Rate_limiter.cleanup_expired lim ~max_idle_seconds:0.0 in
      Alcotest.(check int)
        "0 buckets after cleanup" 0
        (Rate_limiter.bucket_count lim);
      Lwt.return_unit)

let test_independent_keys () =
  run_lwt (fun () ->
      let open Lwt.Syntax in
      let lim = Rate_limiter.create ~rate_per_minute:2 ~burst_multiplier:1.0 in
      let* _ = Rate_limiter.check_and_consume lim ~key:"a" in
      let* _ = Rate_limiter.check_and_consume lim ~key:"a" in
      let* rejected = Rate_limiter.check_and_consume lim ~key:"a" in
      Alcotest.(check bool) "a exhausted" false rejected;
      let* ok = Rate_limiter.check_and_consume lim ~key:"b" in
      Alcotest.(check bool) "b still allowed" true ok;
      Lwt.return_unit)

let suite =
  [
    Alcotest.test_case "within limit" `Quick test_within_limit;
    Alcotest.test_case "over limit" `Quick test_over_limit;
    Alcotest.test_case "refill" `Quick test_refill;
    Alcotest.test_case "burst" `Quick test_burst;
    Alcotest.test_case "cleanup" `Quick test_cleanup;
    Alcotest.test_case "independent keys" `Quick test_independent_keys;
  ]

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

let test_refill_tokens_bounded () =
  let lim = Rate_limiter.create ~rate_per_minute:60 ~burst_multiplier:1.0 in
  let entry =
    Rate_limiter.
      {
        tokens = lim.max_tokens -. 0.25;
        last_refill = Rate_limiter.now () -. 120.0;
      }
  in
  Rate_limiter.refill lim entry;
  Alcotest.(check bool)
    "refill keeps tokens bounded" true
    (entry.tokens <= lim.max_tokens +. 1e-9)

let test_consume_decreases_tokens_by_one () =
  run_lwt (fun () ->
      let open Lwt.Syntax in
      let lim =
        Rate_limiter.
          {
            buckets = Hashtbl.create 1;
            mutex = Lwt_mutex.create ();
            rate_per_minute = 0.0;
            max_tokens = 2.0;
          }
      in
      let entry =
        Rate_limiter.{ tokens = 2.0; last_refill = Rate_limiter.now () }
      in
      Hashtbl.replace lim.buckets "theorem" entry;
      let before = entry.tokens in
      let* ok = Rate_limiter.check_and_consume lim ~key:"theorem" in
      Alcotest.(check bool) "consume allowed" true ok;
      Alcotest.(check (float 0.0))
        "consume decreases by exactly one" 1.0 (before -. entry.tokens);
      Lwt.return_unit)

let test_refill_matches_extracted_oracle () =
  let entry = Rate_limiter.{ tokens = 1.5; last_refill = 10.000 } in
  let _coq, _native, equal =
    Rate_limiter.conformance_refill ~rate_per_minute:120 ~max_tokens:10.0 entry
      ~now:10.250
  in
  Alcotest.(check bool) "refill matches extracted oracle" true equal

let test_refill_cap_matches_extracted_oracle () =
  let entry = Rate_limiter.{ tokens = 1.75; last_refill = 5.000 } in
  let _coq, _native, equal =
    Rate_limiter.conformance_refill ~rate_per_minute:120 ~max_tokens:2.0 entry
      ~now:5.250
  in
  Alcotest.(check bool) "capped refill matches extracted oracle" true equal

let test_try_consume_allowed_matches_extracted_oracle () =
  let entry = Rate_limiter.{ tokens = 1.25; last_refill = 1.000 } in
  let _coq, _native, equal =
    Rate_limiter.conformance_try_consume ~rate_per_minute:60 ~max_tokens:5.0
      entry ~now:1.500
  in
  Alcotest.(check bool) "allowed consume matches extracted oracle" true equal

let test_try_consume_denied_matches_extracted_oracle () =
  let entry = Rate_limiter.{ tokens = 0.25; last_refill = 7.000 } in
  let _coq, _native, equal =
    Rate_limiter.conformance_try_consume ~rate_per_minute:0 ~max_tokens:1.0
      entry ~now:7.500
  in
  Alcotest.(check bool) "denied consume matches extracted oracle" true equal

let suite =
  [
    Alcotest.test_case "within limit" `Quick test_within_limit;
    Alcotest.test_case "over limit" `Quick test_over_limit;
    Alcotest.test_case "refill" `Quick test_refill;
    Alcotest.test_case "refill tokens bounded" `Quick test_refill_tokens_bounded;
    Alcotest.test_case "burst" `Quick test_burst;
    Alcotest.test_case "cleanup" `Quick test_cleanup;
    Alcotest.test_case "independent keys" `Quick test_independent_keys;
    Alcotest.test_case "consume decreases tokens by one" `Quick
      test_consume_decreases_tokens_by_one;
    Alcotest.test_case "refill matches extracted oracle" `Quick
      test_refill_matches_extracted_oracle;
    Alcotest.test_case "capped refill matches extracted oracle" `Quick
      test_refill_cap_matches_extracted_oracle;
    Alcotest.test_case "allowed consume matches extracted oracle" `Quick
      test_try_consume_allowed_matches_extracted_oracle;
    Alcotest.test_case "denied consume matches extracted oracle" `Quick
      test_try_consume_denied_matches_extracted_oracle;
  ]

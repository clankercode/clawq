let test_with_timeout_success () =
  let out =
    Lwt_main.run
      (Resilience.with_timeout ~timeout_s:0.1 (fun () -> Lwt.return "ok"))
  in
  match out with
  | Ok s -> Alcotest.(check string) "value" "ok" s
  | Error e -> Alcotest.fail e

let test_with_timeout_expired () =
  let out =
    Lwt_main.run
      (Resilience.with_timeout ~timeout_s:0.01 (fun () ->
           let open Lwt.Syntax in
           let* () = Lwt_unix.sleep 0.05 in
           Lwt.return "late"))
  in
  match out with
  | Ok _ -> Alcotest.fail "expected timeout"
  | Error msg ->
      Alcotest.(check bool)
        "timeout msg" true
        (Test_helpers.string_contains msg "timed out")

let test_with_retry_eventual_success () =
  let attempts = ref 0 in
  let result =
    Lwt_main.run
      (Resilience.with_retry ~max_retries:3 ~base_delay_s:0.0 (fun () ->
           incr attempts;
           if !attempts < 3 then Lwt.fail (Failure "boom") else Lwt.return "ok"))
  in
  Alcotest.(check string) "result" "ok" result;
  Alcotest.(check int) "attempt count" 3 !attempts

let test_with_fallback_used () =
  let result =
    Lwt_main.run
      (Resilience.with_fallback
         ~primary:(fun () -> Lwt.fail (Failure "primary failed"))
         ~fallback:(fun () -> Lwt.return 7))
  in
  Alcotest.(check int) "fallback result" 7 result

let test_circuit_breaker_closed () =
  let cb = Resilience.create_circuit_breaker ~failure_threshold:3 () in
  let now = Unix.gettimeofday () in
  Alcotest.(check bool)
    "initially closed" false
    (Resilience.is_circuit_open cb ~now)

let test_circuit_breaker_opens () =
  let cb = Resilience.create_circuit_breaker ~failure_threshold:2 () in
  let now = Unix.gettimeofday () in
  Resilience.record_failure cb ~now;
  Alcotest.(check bool)
    "one failure not open" false
    (Resilience.is_circuit_open cb ~now);
  Resilience.record_failure cb ~now;
  Alcotest.(check bool)
    "two failures opens" true
    (Resilience.is_circuit_open cb ~now)

let test_circuit_breaker_half_open () =
  let cb =
    Resilience.create_circuit_breaker ~failure_threshold:1 ~cooldown_s:0.1 ()
  in
  let now = Unix.gettimeofday () in
  Resilience.record_failure cb ~now;
  Alcotest.(check bool) "open" true (Resilience.is_circuit_open cb ~now);
  let later = now +. 0.2 in
  Alcotest.(check bool)
    "half-open after cooldown" false
    (Resilience.is_circuit_open cb ~now:later)

let test_circuit_breaker_reset_on_success () =
  let cb = Resilience.create_circuit_breaker ~failure_threshold:2 () in
  let now = Unix.gettimeofday () in
  Resilience.record_failure cb ~now;
  Resilience.record_success cb;
  Resilience.record_failure cb ~now;
  Alcotest.(check bool)
    "reset after success" false
    (Resilience.is_circuit_open cb ~now)

let test_provider_chain () =
  let pc = Resilience.create_provider_circuits ~failure_threshold:1 () in
  let result =
    Lwt_main.run
      (Resilience.with_provider_chain ~pc
         ~providers:[ ("a", 1); ("b", 2) ]
         (fun id _p ->
           if id = "a" then Lwt.fail_with "a failed" else Lwt.return "from b"))
  in
  Alcotest.(check string) "falls to b" "from b" result

let suite =
  [
    Alcotest.test_case "with_timeout success" `Quick test_with_timeout_success;
    Alcotest.test_case "with_timeout expired" `Quick test_with_timeout_expired;
    Alcotest.test_case "with_retry eventual success" `Quick
      test_with_retry_eventual_success;
    Alcotest.test_case "with_fallback" `Quick test_with_fallback_used;
    Alcotest.test_case "circuit breaker closed" `Quick
      test_circuit_breaker_closed;
    Alcotest.test_case "circuit breaker opens" `Quick test_circuit_breaker_opens;
    Alcotest.test_case "circuit breaker half-open" `Quick
      test_circuit_breaker_half_open;
    Alcotest.test_case "circuit breaker reset" `Quick
      test_circuit_breaker_reset_on_success;
    Alcotest.test_case "provider chain fallback" `Quick test_provider_chain;
  ]

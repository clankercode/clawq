let contains hay needle =
  let hlen = String.length hay in
  let nlen = String.length needle in
  let rec loop i =
    if i + nlen > hlen then false
    else if String.sub hay i nlen = needle then true
    else loop (i + 1)
  in
  nlen = 0 || loop 0

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
      Alcotest.(check bool) "timeout msg" true (contains msg "timed out")

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

let suite =
  [
    Alcotest.test_case "with_timeout success" `Quick test_with_timeout_success;
    Alcotest.test_case "with_timeout expired" `Quick test_with_timeout_expired;
    Alcotest.test_case "with_retry eventual success" `Quick
      test_with_retry_eventual_success;
    Alcotest.test_case "with_fallback" `Quick test_with_fallback_used;
  ]

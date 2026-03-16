(* test_setup_cron.ml — Unit tests for Setup_cron pure validation functions *)

let validate_job_name_valid () =
  Alcotest.(check (result string string))
    "valid" (Ok "daily-report")
    (Setup_cron.validate_job_name "daily-report")

let validate_job_name_underscores () =
  Alcotest.(check (result string string))
    "underscores ok" (Ok "my_job_1")
    (Setup_cron.validate_job_name "my_job_1")

let validate_job_name_empty () =
  match Setup_cron.validate_job_name "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty name"

let validate_job_name_whitespace_only () =
  match Setup_cron.validate_job_name "   " with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for whitespace-only"

let validate_job_name_with_space () =
  match Setup_cron.validate_job_name "my job" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for name with space"

let validate_job_name_with_tab () =
  match Setup_cron.validate_job_name "my\tjob" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for name with tab"

let validate_schedule_interval_seconds () =
  match Setup_cron.validate_schedule "every 30s" with
  | Ok s -> Alcotest.(check string) "kept" "every 30s" s
  | Error e -> Alcotest.failf "expected ok, got error: %s" e

let validate_schedule_interval_minutes () =
  match Setup_cron.validate_schedule "every 5m" with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "expected ok, got error: %s" e

let validate_schedule_interval_hours () =
  match Setup_cron.validate_schedule "every 2h" with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "expected ok, got error: %s" e

let validate_schedule_cron_daily () =
  match Setup_cron.validate_schedule "0 9 * * *" with
  | Ok s -> Alcotest.(check string) "kept" "0 9 * * *" s
  | Error e -> Alcotest.failf "expected ok, got error: %s" e

let validate_schedule_cron_weekly () =
  match Setup_cron.validate_schedule "0 9 * * 1" with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "expected ok, got error: %s" e

let validate_schedule_cron_wildcard_step () =
  match Setup_cron.validate_schedule "0 */4 * * *" with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "expected ok, got error: %s" e

let validate_schedule_invalid () =
  match Setup_cron.validate_schedule "not a schedule" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for invalid schedule"

let validate_schedule_empty () =
  match Setup_cron.validate_schedule "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty schedule"

let validate_schedule_trimmed () =
  (* Leading/trailing whitespace should be trimmed *)
  match Setup_cron.validate_schedule "  every 1h  " with
  | Ok s -> Alcotest.(check string) "trimmed" "every 1h" s
  | Error e -> Alcotest.failf "expected ok after trim, got error: %s" e

let validate_message_valid () =
  Alcotest.(check (result string string))
    "valid" (Ok "Run daily report")
    (Setup_cron.validate_message "Run daily report")

let validate_message_empty () =
  match Setup_cron.validate_message "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty message"

let validate_message_whitespace_only () =
  match Setup_cron.validate_message "   " with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for whitespace-only message"

let validate_message_trimmed () =
  match Setup_cron.validate_message "  hello world  " with
  | Ok "hello world" -> ()
  | Ok s -> Alcotest.failf "expected 'hello world' but got '%s'" s
  | Error _ -> Alcotest.fail "expected success after trim"

let suite =
  [
    Alcotest.test_case "validate_job_name valid" `Quick validate_job_name_valid;
    Alcotest.test_case "validate_job_name underscores" `Quick
      validate_job_name_underscores;
    Alcotest.test_case "validate_job_name empty" `Quick validate_job_name_empty;
    Alcotest.test_case "validate_job_name whitespace" `Quick
      validate_job_name_whitespace_only;
    Alcotest.test_case "validate_job_name with space" `Quick
      validate_job_name_with_space;
    Alcotest.test_case "validate_job_name with tab" `Quick
      validate_job_name_with_tab;
    Alcotest.test_case "validate_schedule interval seconds" `Quick
      validate_schedule_interval_seconds;
    Alcotest.test_case "validate_schedule interval minutes" `Quick
      validate_schedule_interval_minutes;
    Alcotest.test_case "validate_schedule interval hours" `Quick
      validate_schedule_interval_hours;
    Alcotest.test_case "validate_schedule cron daily" `Quick
      validate_schedule_cron_daily;
    Alcotest.test_case "validate_schedule cron weekly" `Quick
      validate_schedule_cron_weekly;
    Alcotest.test_case "validate_schedule cron wildcard step" `Quick
      validate_schedule_cron_wildcard_step;
    Alcotest.test_case "validate_schedule invalid" `Quick
      validate_schedule_invalid;
    Alcotest.test_case "validate_schedule empty" `Quick validate_schedule_empty;
    Alcotest.test_case "validate_schedule trimmed" `Quick
      validate_schedule_trimmed;
    Alcotest.test_case "validate_message valid" `Quick validate_message_valid;
    Alcotest.test_case "validate_message empty" `Quick validate_message_empty;
    Alcotest.test_case "validate_message whitespace" `Quick
      validate_message_whitespace_only;
    Alcotest.test_case "validate_message trimmed" `Quick
      validate_message_trimmed;
  ]

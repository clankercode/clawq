let exec_exn db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "sqlite exec failed: %s" (Sqlite3.Rc.to_string rc))

let with_db f =
  let db = Memory.init ~db_path:":memory:" () in
  let profile_id = Memory.insert_room_profile ~db ~name:"budget-profile" in
  f db profile_id

let record_usage ~db ~profile_id ~session_key ~prompt_tokens ~completion_tokens
    ~cost_usd ~requested_at =
  Request_stats.record ~db ~session_key ~profile_id ~provider:"openai"
    ~model:"gpt-5.4" ~prompt_tokens ~completion_tokens ~cost_usd ();
  exec_exn db
    (Printf.sprintf
       "UPDATE request_stats SET requested_at = '%s' WHERE session_key = '%s'"
       requested_at session_key)

let expect_budget ~db ~profile_id =
  match Room_budget.get_profile_budget ~db ~profile_id with
  | Some state -> state
  | None -> Alcotest.fail "expected room budget state"

let test_budget_creation_and_idempotent_query () =
  with_db (fun db profile_id ->
      Room_budget.init_schema db;
      Room_budget.init_schema db;
      Room_budget.init_profile_budget ~db ~profile_id ~token_limit:1_000
        ~cost_limit_usd:2.50 ~reset_period:"daily"
        ~period_started_at:"2026-01-01 00:00:00" ();
      Room_budget.init_profile_budget ~db ~profile_id ~token_limit:1_000
        ~cost_limit_usd:2.50 ~reset_period:"daily"
        ~period_started_at:"2026-01-02 00:00:00" ();
      let state = expect_budget ~db ~profile_id in
      Alcotest.(check int) "profile_id" profile_id state.profile_id;
      Alcotest.(check int) "token limit" 1_000 state.token_limit;
      Alcotest.(check (float 0.0001)) "cost limit" 2.50 state.cost_limit_usd;
      Alcotest.(check string) "reset period" "daily" state.reset_period;
      Alcotest.(check string)
        "idempotent init preserves period" "2026-01-01 00:00:00"
        state.period_started_at;
      Alcotest.(check int) "current tokens" 0 state.current_usage.total_tokens;
      Alcotest.(check (float 0.0001))
        "current cost" 0.0 state.current_usage.cost_usd;
      Alcotest.(check bool) "not exceeded" false state.limit_exceeded)

let test_usage_tracking_and_limit_exceeded () =
  with_db (fun db profile_id ->
      Room_budget.init_profile_budget ~db ~profile_id ~token_limit:100
        ~cost_limit_usd:1.00 ~reset_period:"daily"
        ~period_started_at:"2026-01-01 00:00:00" ();
      record_usage ~db ~profile_id ~session_key:"within" ~prompt_tokens:60
        ~completion_tokens:20 ~cost_usd:0.40 ~requested_at:"2026-01-01 01:00:00";
      let within = expect_budget ~db ~profile_id in
      Alcotest.(check int) "within tokens" 80 within.current_usage.total_tokens;
      Alcotest.(check (float 0.0001))
        "within cost" 0.40 within.current_usage.cost_usd;
      Alcotest.(check bool) "within limit" false within.limit_exceeded;
      record_usage ~db ~profile_id ~session_key:"exceeded" ~prompt_tokens:30
        ~completion_tokens:5 ~cost_usd:0.70 ~requested_at:"2026-01-01 02:00:00";
      let exceeded = expect_budget ~db ~profile_id in
      Alcotest.(check int)
        "exceeded tokens" 115 exceeded.current_usage.total_tokens;
      Alcotest.(check (float 0.0001))
        "exceeded cost" 1.10 exceeded.current_usage.cost_usd;
      Alcotest.(check bool) "token exceeded" true exceeded.token_limit_exceeded;
      Alcotest.(check bool) "cost exceeded" true exceeded.cost_limit_exceeded;
      Alcotest.(check bool) "limit exceeded" true exceeded.limit_exceeded)

let test_budget_reset_starts_new_period () =
  with_db (fun db profile_id ->
      Room_budget.init_profile_budget ~db ~profile_id ~token_limit:100
        ~cost_limit_usd:1.00 ~reset_period:"daily"
        ~period_started_at:"2026-01-01 00:00:00" ();
      record_usage ~db ~profile_id ~session_key:"old" ~prompt_tokens:70
        ~completion_tokens:30 ~cost_usd:0.90 ~requested_at:"2026-01-01 01:00:00";
      let before = expect_budget ~db ~profile_id in
      Alcotest.(check int)
        "before reset tokens" 100 before.current_usage.total_tokens;
      Alcotest.(check bool)
        "reset updated row" true
        (Room_budget.reset_profile_budget ~db ~profile_id
           ~period_started_at:"2026-01-02 00:00:00" ());
      let after = expect_budget ~db ~profile_id in
      Alcotest.(check string)
        "reset period start" "2026-01-02 00:00:00" after.period_started_at;
      Alcotest.(check int)
        "old usage excluded" 0 after.current_usage.total_tokens;
      record_usage ~db ~profile_id ~session_key:"new" ~prompt_tokens:10
        ~completion_tokens:5 ~cost_usd:0.20 ~requested_at:"2026-01-02 01:00:00";
      let current = expect_budget ~db ~profile_id in
      Alcotest.(check int)
        "new usage counted" 15 current.current_usage.total_tokens;
      Alcotest.(check (float 0.0001))
        "new cost counted" 0.20 current.current_usage.cost_usd)

let expect_reservation = function
  | Ok release -> release
  | Error state ->
      Alcotest.failf "expected budget reservation, got exceeded for profile %d"
        state.Room_budget.profile_id

let test_concurrent_reservations_do_not_overcommit () =
  with_db (fun db profile_id ->
      Room_budget.init_profile_budget ~db ~profile_id ~token_limit:100
        ~cost_limit_usd:1.00 ~reset_period:"daily"
        ~period_started_at:"2026-01-01 00:00:00" ();
      Lwt_main.run
        (let open Lwt.Syntax in
         let* first =
           Room_budget.reserve_profile_budget ~db ~profile_id
             ~estimated_tokens:80 ~estimated_cost_usd:0.0
         in
         let release_first = expect_reservation first in
         let second =
           Room_budget.reserve_profile_budget ~db ~profile_id
             ~estimated_tokens:30 ~estimated_cost_usd:0.0
         in
         let* () = Lwt.pause () in
         Alcotest.(check bool)
           "second reservation queued" true
           (match Lwt.state second with Lwt.Sleep -> true | _ -> false);
         release_first ();
         let* second_result = second in
         let release_second = expect_reservation second_result in
         release_second ();
         Lwt.return_unit))

let test_reservation_released_on_failure () =
  with_db (fun db profile_id ->
      Room_budget.init_profile_budget ~db ~profile_id ~token_limit:100
        ~cost_limit_usd:1.00 ~reset_period:"daily"
        ~period_started_at:"2026-01-01 00:00:00" ();
      Lwt_main.run
        (let open Lwt.Syntax in
         let* failed =
           Lwt.catch
             (fun () ->
               Room_budget.with_profile_budget_reservation ~db ~profile_id
                 ~estimated_tokens:100 ~estimated_cost_usd:0.0 (fun () ->
                   Lwt.fail_with "provider failed"))
             (fun exn -> Lwt.return (Printexc.to_string exn))
         in
         Alcotest.(check bool)
           "provider failure observed" true
           (Test_helpers.string_contains failed "provider failed");
         let* next =
           Room_budget.reserve_profile_budget ~db ~profile_id
             ~estimated_tokens:100 ~estimated_cost_usd:0.0
         in
         let release_next = expect_reservation next in
         release_next ();
         Lwt.return_unit))

let test_reservation_fails_when_budget_unavailable () =
  with_db (fun db profile_id ->
      Room_budget.init_profile_budget ~db ~profile_id ~token_limit:100
        ~cost_limit_usd:1.00 ~reset_period:"daily"
        ~period_started_at:"2026-01-01 00:00:00" ();
      record_usage ~db ~profile_id ~session_key:"used" ~prompt_tokens:95
        ~completion_tokens:0 ~cost_usd:0.50 ~requested_at:"2026-01-01 01:00:00";
      Lwt_main.run
        (let open Lwt.Syntax in
         let* reservation =
           Room_budget.reserve_profile_budget ~db ~profile_id
             ~estimated_tokens:10 ~estimated_cost_usd:0.0
         in
         match reservation with
         | Ok release ->
             release ();
             Alcotest.fail "reservation should fail when budget is unavailable"
         | Error state ->
             Alcotest.(check int)
               "reported current usage" 95 state.current_usage.total_tokens;
             Lwt.return_unit))

let suite =
  [
    Alcotest.test_case "budget creation and idempotent query" `Quick
      test_budget_creation_and_idempotent_query;
    Alcotest.test_case "usage tracking and limit exceeded" `Quick
      test_usage_tracking_and_limit_exceeded;
    Alcotest.test_case "budget reset starts new period" `Quick
      test_budget_reset_starts_new_period;
    Alcotest.test_case "concurrent reservations do not overcommit" `Quick
      test_concurrent_reservations_do_not_overcommit;
    Alcotest.test_case "reservation released on failure" `Quick
      test_reservation_released_on_failure;
    Alcotest.test_case "reservation fails when budget unavailable" `Quick
      test_reservation_fails_when_budget_unavailable;
  ]

(** Tests for Setup_room_wizard_validate: audit ledger and connector delivery
    validation in the room wizard. *)

open Setup_room_wizard_validate

let contains_substring = Test_helpers.string_contains

(** {1 Simulation tests} *)

let test_simulate_teams_delivery () =
  let summary =
    simulate_delivery ~connector:"teams" ~room_id:"room-test"
      ~profile_id:"test-profile" ~task_id:42
  in
  (* Ledger events should include attempt and success *)
  Alcotest.(check int) "delivery attempts" 1 summary.delivery_attempt_count;
  Alcotest.(check int) "delivery successes" 1 summary.delivery_success_count;
  Alcotest.(check int) "delivery failures" 1 summary.delivery_failure_count;
  (* Egress events should include allowed and denied *)
  Alcotest.(check int) "egress allowed" 1 summary.egress_allowed_count;
  Alcotest.(check int) "egress denied" 1 summary.egress_denied_count;
  (* Teams lifecycle should have 5 states *)
  Alcotest.(check int) "lifecycle states" 5 summary.lifecycle_state_count;
  Alcotest.(check bool)
    "lifecycle events non-empty" true
    (summary.lifecycle_events <> [])

let test_simulate_slack_delivery () =
  let summary =
    simulate_delivery ~connector:"slack" ~room_id:"C12345"
      ~profile_id:"slack-profile" ~task_id:100
  in
  (* Ledger events should include attempt and success *)
  Alcotest.(check int) "delivery attempts" 1 summary.delivery_attempt_count;
  Alcotest.(check int) "delivery successes" 1 summary.delivery_success_count;
  Alcotest.(check int) "delivery failures" 1 summary.delivery_failure_count;
  (* Egress events should include allowed and denied *)
  Alcotest.(check int) "egress allowed" 1 summary.egress_allowed_count;
  Alcotest.(check int) "egress denied" 1 summary.egress_denied_count;
  (* Slack should NOT have lifecycle events *)
  Alcotest.(check int) "no lifecycle for slack" 0 summary.lifecycle_state_count;
  Alcotest.(check bool)
    "lifecycle events empty" true
    (summary.lifecycle_events = [])

let test_simulate_discord_delivery () =
  let summary =
    simulate_delivery ~connector:"discord" ~room_id:"discord-channel"
      ~profile_id:"discord-profile" ~task_id:200
  in
  Alcotest.(check int) "delivery attempts" 1 summary.delivery_attempt_count;
  Alcotest.(check int) "delivery successes" 1 summary.delivery_success_count;
  Alcotest.(check int)
    "no lifecycle for discord" 0 summary.lifecycle_state_count

(** {1 Ledger event content tests} *)

let test_ledger_events_have_correct_metadata () =
  let summary =
    simulate_delivery ~connector:"teams" ~room_id:"room-meta"
      ~profile_id:"meta-profile" ~task_id:55
  in
  (* Find the delivery attempt event *)
  let attempt =
    List.find_opt
      (fun (e : Room_activity_ledger.event) ->
        e.event_type = "delivery_attempt")
      summary.ledger_events
  in
  match attempt with
  | None -> Alcotest.fail "expected delivery_attempt event"
  | Some evt ->
      Alcotest.(check string) "room_id" "room-meta" evt.room_id;
      Alcotest.(check string) "actor" "teams" evt.actor;
      let task_id =
        match evt.metadata with
        | `Assoc fields -> (
            match List.assoc_opt "task_id" fields with
            | Some (`Int n) -> n
            | _ -> -1)
        | _ -> -1
      in
      Alcotest.(check int) "task_id in metadata" 55 task_id

let test_success_event_has_message_id () =
  let summary =
    simulate_delivery ~connector:"slack" ~room_id:"C99" ~profile_id:"p"
      ~task_id:1
  in
  let success =
    List.find_opt
      (fun (e : Room_activity_ledger.event) ->
        e.event_type = "delivery_success")
      summary.ledger_events
  in
  match success with
  | None -> Alcotest.fail "expected delivery_success event"
  | Some evt ->
      let message_id =
        match evt.metadata with
        | `Assoc fields -> (
            match List.assoc_opt "message_id" fields with
            | Some (`String s) -> s
            | _ -> "")
        | _ -> ""
      in
      Alcotest.(check bool) "message_id present" true (message_id <> "")

(** {1 Egress audit content tests} *)

let test_egress_events_redacted () =
  let summary =
    simulate_delivery ~connector:"teams" ~room_id:"room-egress"
      ~profile_id:"egress-profile" ~task_id:10
  in
  (* The allowed egress event should have a redacted host *)
  let allowed =
    List.find_opt
      (fun (e : Egress_audit.event) -> e.decision = Egress_audit.Allowed)
      summary.egress_events
  in
  match allowed with
  | None -> Alcotest.fail "expected allowed egress event"
  | Some evt ->
      (* Host should be redacted -- not the raw hostname *)
      Alcotest.(check bool)
        "host is redacted" true
        (not (contains_substring evt.host_redacted "api.com"));
      Alcotest.(check bool) "host not empty" true (evt.host_redacted <> "")

let test_egress_events_have_session_key () =
  let summary =
    simulate_delivery ~connector:"teams" ~room_id:"room-sk"
      ~profile_id:"sk-profile" ~task_id:20
  in
  List.iter
    (fun (evt : Egress_audit.event) ->
      match evt.session_key with
      | Some sk ->
          Alcotest.(check bool)
            "session_key contains room" true
            (contains_substring sk "room-sk")
      | None -> Alcotest.fail "expected session_key in egress event")
    summary.egress_events

(** {1 Teams lifecycle content tests} *)

let test_lifecycle_events_ordered () =
  let summary =
    simulate_delivery ~connector:"teams" ~room_id:"room-lc"
      ~profile_id:"lc-profile" ~task_id:30
  in
  Alcotest.(check int) "five lifecycle events" 5 summary.lifecycle_state_count;
  let states =
    List.map
      (fun (e : Room_activity_ledger.event) ->
        match e.metadata with
        | `Assoc fields -> (
            match List.assoc_opt "lifecycle_state" fields with
            | Some (`String s) -> s
            | _ -> "?")
        | _ -> "?")
      summary.lifecycle_events
  in
  Alcotest.(check (list string))
    "lifecycle order"
    [
      "scheduled";
      "generated";
      "attempted";
      "transport_accepted";
      "message_id_recorded";
    ]
    states

let test_lifecycle_events_have_tracking_id () =
  let summary =
    simulate_delivery ~connector:"teams" ~room_id:"room-tid"
      ~profile_id:"tid-profile" ~task_id:40
  in
  let tracking_ids =
    List.map
      (fun (e : Room_activity_ledger.event) ->
        match e.metadata with
        | `Assoc fields -> (
            match List.assoc_opt "tracking_id" fields with
            | Some (`String s) -> s
            | _ -> "")
        | _ -> "")
      summary.lifecycle_events
  in
  let unique_ids = List.sort_uniq String.compare tracking_ids in
  Alcotest.(check int)
    "single tracking id across lifecycle" 1 (List.length unique_ids);
  Alcotest.(check bool)
    "tracking id starts with dlv_" true
    (match unique_ids with
    | [ id ] -> String.length id > 4 && String.sub id 0 4 = "dlv_"
    | _ -> false)

(** {1 Display tests} *)

let test_display_traces_passes_for_teams () =
  let summary =
    simulate_delivery ~connector:"teams" ~room_id:"room-d"
      ~profile_id:"d-profile" ~task_id:50
  in
  let passed =
    display_traces ~connector:"teams" ~room_id:"room-d" ~profile_id:"d-profile"
      summary
  in
  Alcotest.(check bool) "teams validation passes" true passed

let test_display_traces_passes_for_slack () =
  let summary =
    simulate_delivery ~connector:"slack" ~room_id:"C55" ~profile_id:"slack-p"
      ~task_id:60
  in
  let passed =
    display_traces ~connector:"slack" ~room_id:"C55" ~profile_id:"slack-p"
      summary
  in
  Alcotest.(check bool) "slack validation passes" true passed

(** {1 Run entry point tests} *)

let test_run_missing_profile_id () =
  let result = run ~profile_id:"" ~connector:"teams" ~room_id:"room-1" () in
  Alcotest.(check bool)
    "error for missing profile-id" true
    (contains_substring result "Error")

let test_run_missing_room () =
  let result = run ~profile_id:"test" ~connector:"teams" ~room_id:"" () in
  Alcotest.(check bool)
    "error for missing room" true
    (contains_substring result "Error")

let test_run_success () =
  let result =
    run ~profile_id:"test" ~connector:"teams" ~room_id:"19:abc@thread.tacv2" ()
  in
  Alcotest.(check bool)
    "success message" true
    (contains_substring result "Validation passed")

(** {1 Readiness check tests} *)

let test_readiness_includes_audit_checks () =
  let cfg = Config_loader.parse_config (`Assoc []) in
  let db = Memory.init ~db_path:":memory:" () in
  let state =
    { Setup_room_wizard.default_state with profile_id = "test-profile" }
  in
  let checks =
    Setup_room_wizard.run_readiness_checks ~cfg ~db:(Some db) ~state
  in
  let ledger_check =
    List.find_opt (fun c -> c.Setup_room_wizard.name = "Activity Ledger") checks
  in
  let egress_check =
    List.find_opt (fun c -> c.Setup_room_wizard.name = "Egress Audit") checks
  in
  (match ledger_check with
  | None -> Alcotest.fail "expected Activity Ledger readiness check"
  | Some check ->
      Alcotest.(check bool) "ledger check passes" true check.passed;
      Alcotest.(check bool)
        "ledger check says accessible" true
        (contains_substring check.message "accessible"));
  match egress_check with
  | None -> Alcotest.fail "expected Egress Audit readiness check"
  | Some check ->
      Alcotest.(check bool) "egress check passes" true check.passed;
      Alcotest.(check bool)
        "egress check says accessible" true
        (contains_substring check.message "accessible")

let test_readiness_audit_checks_skip_without_db () =
  let cfg = Config_loader.parse_config (`Assoc []) in
  let state =
    { Setup_room_wizard.default_state with profile_id = "test-profile" }
  in
  let checks = Setup_room_wizard.run_readiness_checks ~cfg ~db:None ~state in
  let ledger_check =
    List.find_opt (fun c -> c.Setup_room_wizard.name = "Activity Ledger") checks
  in
  let egress_check =
    List.find_opt (fun c -> c.Setup_room_wizard.name = "Egress Audit") checks
  in
  (match ledger_check with
  | None -> Alcotest.fail "expected Activity Ledger readiness check"
  | Some check ->
      Alcotest.(check bool) "ledger check passes (skip)" true check.passed;
      Alcotest.(check bool)
        "ledger check says skip" true
        (contains_substring check.message "skip"));
  match egress_check with
  | None -> Alcotest.fail "expected Egress Audit readiness check"
  | Some check ->
      Alcotest.(check bool) "egress check passes (skip)" true check.passed;
      Alcotest.(check bool)
        "egress check says skip" true
        (contains_substring check.message "skip")

(** {1 Suite} *)

let suite =
  [
    Alcotest.test_case "simulate teams delivery" `Quick
      test_simulate_teams_delivery;
    Alcotest.test_case "simulate slack delivery" `Quick
      test_simulate_slack_delivery;
    Alcotest.test_case "simulate discord delivery" `Quick
      test_simulate_discord_delivery;
    Alcotest.test_case "ledger events have correct metadata" `Quick
      test_ledger_events_have_correct_metadata;
    Alcotest.test_case "success event has message_id" `Quick
      test_success_event_has_message_id;
    Alcotest.test_case "egress events redacted" `Quick
      test_egress_events_redacted;
    Alcotest.test_case "egress events have session_key" `Quick
      test_egress_events_have_session_key;
    Alcotest.test_case "lifecycle events ordered" `Quick
      test_lifecycle_events_ordered;
    Alcotest.test_case "lifecycle events have tracking_id" `Quick
      test_lifecycle_events_have_tracking_id;
    Alcotest.test_case "display traces passes for teams" `Quick
      test_display_traces_passes_for_teams;
    Alcotest.test_case "display traces passes for slack" `Quick
      test_display_traces_passes_for_slack;
    Alcotest.test_case "run missing profile_id" `Quick
      test_run_missing_profile_id;
    Alcotest.test_case "run missing room" `Quick test_run_missing_room;
    Alcotest.test_case "run success" `Quick test_run_success;
    Alcotest.test_case "readiness includes audit checks" `Quick
      test_readiness_includes_audit_checks;
    Alcotest.test_case "readiness audit checks skip without db" `Quick
      test_readiness_audit_checks_skip_without_db;
  ]

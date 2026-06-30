(** Room UX smoke tests for Slack and Teams connectors.

    These smoke tests exercise the complete rendering pipeline for room-origin
    background tasks, verifying that progress updates, explanations, and final
    artifacts produce correct output for each connector's format.

    Coverage:
    - Teams production path: Adaptive Card rendering with actions
    - Slack baseline path: mrkdwn rendering with links *)

open Room_progress_checklist

(** Helper to create a checklist item with defaults. *)
let make_item ?(id = 1) ?(task_id = 1) ?(transcript_url = None)
    ?(session_url = None) ?(session_record_id = None)
    ?(delivery_state = Delivery_confirmed) ~title ~state () =
  {
    id;
    task_id;
    title;
    state;
    transcript_url;
    session_url;
    session_record_id;
    last_update = "2026-06-30T00:00:00Z";
    delivery_state;
  }

(** Helper to create a complete task scenario with multiple checklist items. *)
let make_task_scenario ~task_id () =
  [
    make_item ~id:1 ~task_id ~title:"Analyze codebase structure" ~state:Done
      ~transcript_url:(Some "https://runner.example.com/tr/1")
      ~session_url:(Some "https://runner.example.com/s/1") ();
    make_item ~id:2 ~task_id ~title:"Implement auth module" ~state:Done
      ~transcript_url:(Some "https://runner.example.com/tr/2") ();
    make_item ~id:3 ~task_id ~title:"Write unit tests" ~state:Current
      ~session_url:(Some "https://runner.example.com/s/3") ();
    make_item ~id:4 ~task_id ~title:"Run integration tests" ~state:Planned ();
    make_item ~id:5 ~task_id ~title:"Deploy to staging" ~state:Planned ();
  ]

(** {1 Teams Production Path Smoke Tests} *)

(** Teams smoke test: Progress update renders as valid Adaptive Card with
    checklist items and status indicators. *)
let test_teams_progress_update_renders_valid_card () =
  let task_id = 42 in
  let items = make_task_scenario ~task_id () in
  let card =
    Teams_progress_card.build_card ~task_id ~task_label:"Fix authentication bug"
      ~items ~elapsed:"5m 30s" ()
  in
  (* Verify envelope structure *)
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "envelope type" "message"
    (card |> member "type" |> to_string);
  let attachments = card |> member "attachments" |> to_list in
  Alcotest.(check int) "one attachment" 1 (List.length attachments);
  let att = List.hd attachments in
  Alcotest.(check string)
    "content type" "application/vnd.microsoft.card.adaptive"
    (att |> member "contentType" |> to_string);
  let content = att |> member "content" in
  Alcotest.(check string)
    "card type" "AdaptiveCard"
    (content |> member "type" |> to_string);
  Alcotest.(check string)
    "schema version" "1.4"
    (content |> member "version" |> to_string);
  (* Verify header contains task info *)
  let body = content |> member "body" |> to_list in
  let header = List.hd body in
  let header_text = header |> member "text" |> to_string in
  Alcotest.(check bool)
    "header has task id" true
    (Test_helpers.string_contains header_text "Task #42");
  Alcotest.(check bool)
    "header has task label" true
    (Test_helpers.string_contains header_text "Fix authentication bug");
  (* Verify elapsed time appears in body *)
  let json_str = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "has elapsed in body" true
    (Test_helpers.string_contains json_str "5m 30s")

(** Teams smoke test: Checklist items appear in card body with correct state
    indicators. *)
let test_teams_checklist_items_appear_with_state_indicators () =
  let task_id = 42 in
  let items = make_task_scenario ~task_id () in
  let card =
    Teams_progress_card.build_card ~task_id ~task_label:"Fix auth" ~items ()
  in
  let json_str = Yojson.Safe.to_string card in
  (* Verify all checklist items appear *)
  Alcotest.(check bool)
    "has analyze step" true
    (Test_helpers.string_contains json_str "Analyze codebase structure");
  Alcotest.(check bool)
    "has implement step" true
    (Test_helpers.string_contains json_str "Implement auth module");
  Alcotest.(check bool)
    "has tests step" true
    (Test_helpers.string_contains json_str "Write unit tests");
  Alcotest.(check bool)
    "has integration step" true
    (Test_helpers.string_contains json_str "Run integration tests");
  Alcotest.(check bool)
    "has deploy step" true
    (Test_helpers.string_contains json_str "Deploy to staging");
  (* Verify state indicators appear *)
  Alcotest.(check bool)
    "has working indicator for current" true
    (Test_helpers.string_contains json_str "(working)");
  Alcotest.(check bool)
    "has done icon" true
    (Test_helpers.string_contains json_str "\xE2\x9C\x85")

(** Teams smoke test: Action buttons render with correct commands for task
    lifecycle. *)
let test_teams_actions_render_for_task_lifecycle () =
  let task_id = 42 in
  let items = make_task_scenario ~task_id () in
  let actions : Teams_progress_card.task_actions =
    {
      task_id;
      show_retry = true;
      show_logs = true;
      show_finalize = true;
      show_inspect = true;
      show_continue = true;
      show_cancel = true;
      log_path = Some "/tmp/task-42.log";
    }
  in
  let card =
    Teams_progress_card.build_card ~task_id ~task_label:"Fix auth" ~items
      ~actions:(Some actions) ()
  in
  let json_str = Yojson.Safe.to_string card in
  (* Verify action buttons appear *)
  Alcotest.(check bool)
    "has retry button" true
    (Test_helpers.string_contains json_str "Retry Task");
  Alcotest.(check bool)
    "has logs button" true
    (Test_helpers.string_contains json_str "View Logs");
  Alcotest.(check bool)
    "has finalize button" true
    (Test_helpers.string_contains json_str "Finalize");
  Alcotest.(check bool)
    "has inspect button" true
    (Test_helpers.string_contains json_str "Inspect");
  Alcotest.(check bool)
    "has continue button" true
    (Test_helpers.string_contains json_str "Continue");
  Alcotest.(check bool)
    "has cancel button" true
    (Test_helpers.string_contains json_str "Cancel");
  (* Verify Teams imBack format *)
  Alcotest.(check bool)
    "has msteams action type" true
    (Test_helpers.string_contains json_str "msteams");
  Alcotest.(check bool)
    "has imBack action" true
    (Test_helpers.string_contains json_str "\"imBack\"");
  (* Verify commands contain task id *)
  Alcotest.(check bool)
    "retry command has task id" true
    (Test_helpers.string_contains json_str "/background retry 42")

(** Teams smoke test: Final artifact renders with succeeded outcome styling. *)
let test_teams_final_artifact_succeeded () =
  let task_id = 42 in
  let items =
    [
      make_item ~id:1 ~task_id ~title:"Analyze codebase" ~state:Final ();
      make_item ~id:2 ~task_id ~title:"Implement auth" ~state:Final ();
      make_item ~id:3 ~task_id ~title:"Write tests" ~state:Final ();
    ]
  in
  let card =
    Teams_progress_card.build_card ~task_id ~task_label:"Fix auth bug" ~items
      ~task_outcome:Teams_progress_card.Succeeded ~elapsed:"15m"
      ~summary:"All tests passing, auth module complete" ()
  in
  let json_str = Yojson.Safe.to_string card in
  (* Verify succeeded styling *)
  Alcotest.(check bool)
    "has checkmark icon" true
    (Test_helpers.string_contains json_str "\xE2\x9C\x85");
  (* Verify summary appears *)
  Alcotest.(check bool)
    "has custom summary" true
    (Test_helpers.string_contains json_str "All tests passing");
  (* Verify elapsed time *)
  Alcotest.(check bool)
    "has elapsed" true
    (Test_helpers.string_contains json_str "15m")

(** Teams smoke test: Final artifact renders with failed outcome styling. *)
let test_teams_final_artifact_failed () =
  let task_id = 42 in
  let items =
    [
      make_item ~id:1 ~task_id ~title:"Analyze codebase" ~state:Done ();
      make_item ~id:2 ~task_id ~title:"Implement auth" ~state:Blocked ();
    ]
  in
  let card =
    Teams_progress_card.build_card ~task_id ~task_label:"Fix auth bug" ~items
      ~task_outcome:Teams_progress_card.Failed ()
  in
  let json_str = Yojson.Safe.to_string card in
  (* Verify failed styling *)
  Alcotest.(check bool)
    "has cross mark icon" true
    (Test_helpers.string_contains json_str "\xE2\x9D\x8C");
  (* Verify blocked item shows generic indicator *)
  Alcotest.(check bool)
    "has blocked indicator" true
    (Test_helpers.string_contains json_str "(blocked)")

(** Teams smoke test: Transcript and session links appear in checklist items. *)
let test_teams_links_appear_in_checklist () =
  let task_id = 42 in
  let items = make_task_scenario ~task_id () in
  let card =
    Teams_progress_card.build_card ~task_id ~task_label:"Fix auth" ~items ()
  in
  let json_str = Yojson.Safe.to_string card in
  (* Verify links appear *)
  Alcotest.(check bool)
    "has transcript link" true
    (Test_helpers.string_contains json_str "transcript");
  Alcotest.(check bool)
    "has session link" true
    (Test_helpers.string_contains json_str "session")

(** Teams smoke test: Update card produces same structure as initial card. *)
let test_teams_update_card_matches_initial () =
  let task_id = 42 in
  let items = make_task_scenario ~task_id () in
  let initial_card =
    Teams_progress_card.build_card ~task_id ~task_label:"Fix auth" ~items ()
  in
  let update_card =
    Teams_progress_card.build_update_card ~task_id ~task_label:"Fix auth" ~items
      ()
  in
  (* Both should have same structure *)
  let initial_content =
    Yojson.Safe.Util.member "attachments" initial_card
    |> Yojson.Safe.Util.to_list |> List.hd
    |> Yojson.Safe.Util.member "content"
  in
  let update_content =
    Yojson.Safe.Util.member "attachments" update_card
    |> Yojson.Safe.Util.to_list |> List.hd
    |> Yojson.Safe.Util.member "content"
  in
  let initial_body_len =
    Yojson.Safe.Util.member "body" initial_content
    |> Yojson.Safe.Util.to_list |> List.length
  in
  let update_body_len =
    Yojson.Safe.Util.member "body" update_content
    |> Yojson.Safe.Util.to_list |> List.length
  in
  Alcotest.(check int) "same body length" initial_body_len update_body_len

(** {1 Slack Baseline Path Smoke Tests} *)

(** Slack smoke test: Progress update renders as mrkdwn with checklist items and
    state indicators. *)
let test_slack_progress_update_renders_mrkdwn () =
  let task_id = 42 in
  let items = make_task_scenario ~task_id () in
  let msg =
    Slack_progress_checklist.render_checklist
      ~task_label:"Fix authentication bug" ~elapsed:"5m 30s" items
  in
  (* Verify task label appears in bold *)
  Alcotest.(check bool)
    "has bold task label" true
    (Test_helpers.string_contains msg "*Fix authentication bug*");
  (* Verify checklist items appear *)
  Alcotest.(check bool)
    "has analyze step" true
    (Test_helpers.string_contains msg "Analyze codebase structure");
  Alcotest.(check bool)
    "has implement step" true
    (Test_helpers.string_contains msg "Implement auth module");
  Alcotest.(check bool)
    "has tests step" true
    (Test_helpers.string_contains msg "Write unit tests");
  (* Verify summary appears *)
  Alcotest.(check bool)
    "has summary" true
    (Test_helpers.string_contains msg "done")

(** Slack smoke test: Checklist items show correct state indicators for each
    lifecycle state. *)
let test_slack_state_indicators_correct () =
  let task_id = 42 in
  let items = make_task_scenario ~task_id () in
  let msg =
    Slack_progress_checklist.render_checklist ~task_label:"Fix auth" items
  in
  (* Verify state indicators *)
  Alcotest.(check bool)
    "has done checkmark" true
    (Test_helpers.string_contains msg "\xE2\x9C\x85");
  Alcotest.(check bool)
    "has current arrows" true
    (Test_helpers.string_contains msg "\xF0\x9F\x94\x84");
  Alcotest.(check bool)
    "has working label" true
    (Test_helpers.string_contains msg "(working)")

(** Slack smoke test: Transcript and session links appear in Slack mrkdwn
    format. *)
let test_slack_links_use_mrkdwn_format () =
  let task_id = 42 in
  let items = make_task_scenario ~task_id () in
  let msg =
    Slack_progress_checklist.render_checklist ~task_label:"Fix auth" items
  in
  (* Verify Slack-format links: <url|label> *)
  Alcotest.(check bool)
    "has transcript link" true
    (Test_helpers.string_contains msg "transcript");
  Alcotest.(check bool)
    "has session link" true
    (Test_helpers.string_contains msg "session");
  (* Verify Slack link format, not markdown *)
  Alcotest.(check bool)
    "no markdown links" false
    (Test_helpers.string_contains msg "[transcript](")

(** Slack smoke test: Final artifact renders with succeeded outcome. *)
let test_slack_final_artifact_succeeded () =
  let task_id = 42 in
  let items =
    [
      make_item ~id:1 ~task_id ~title:"Analyze codebase" ~state:Final ();
      make_item ~id:2 ~task_id ~title:"Implement auth" ~state:Final ();
      make_item ~id:3 ~task_id ~title:"Write tests" ~state:Final ();
    ]
  in
  let msg =
    Slack_progress_checklist.render_final ~task_label:"Fix auth bug"
      ~task_status:"succeeded" ~elapsed:"15m"
      ~summary:"All tests passing, auth module complete" items
  in
  (* Verify succeeded styling *)
  Alcotest.(check bool)
    "has checkmark icon" true
    (Test_helpers.string_contains msg "\xE2\x9C\x85");
  (* Verify summary appears *)
  Alcotest.(check bool)
    "has custom summary" true
    (Test_helpers.string_contains msg "All tests passing");
  (* Verify elapsed time *)
  Alcotest.(check bool)
    "has elapsed" true
    (Test_helpers.string_contains msg "15m")

(** Slack smoke test: Final artifact renders with failed outcome. *)
let test_slack_final_artifact_failed () =
  let task_id = 42 in
  let items =
    [
      make_item ~id:1 ~task_id ~title:"Analyze codebase" ~state:Done ();
      make_item ~id:2 ~task_id ~title:"Implement auth" ~state:Blocked ();
    ]
  in
  let msg =
    Slack_progress_checklist.render_final ~task_label:"Fix auth bug"
      ~task_status:"failed" items
  in
  (* Verify failed styling *)
  Alcotest.(check bool)
    "has cross mark icon" true
    (Test_helpers.string_contains msg "\xE2\x9D\x8C");
  (* Verify blocked item shows generic indicator *)
  Alcotest.(check bool)
    "has blocked indicator" true
    (Test_helpers.string_contains msg "(blocked)")

(** Slack smoke test: Summary line shows completion ratio and state counts. *)
let test_slack_summary_shows_completion_ratio () =
  let task_id = 42 in
  let items = make_task_scenario ~task_id () in
  let summary = Slack_progress_checklist.render_summary items in
  (* Verify completion ratio *)
  Alcotest.(check bool)
    "has done count" true
    (Test_helpers.string_contains summary "2/5 done");
  Alcotest.(check bool)
    "has current count" true
    (Test_helpers.string_contains summary "1 current");
  Alcotest.(check bool)
    "has planned count" true
    (Test_helpers.string_contains summary "2 planned")

(** Slack smoke test: Empty checklist produces valid message with task label. *)
let test_slack_empty_checklist_produces_valid_message () =
  let msg =
    Slack_progress_checklist.render_checklist ~task_label:"Empty task" []
  in
  Alcotest.(check bool)
    "has task label" true
    (Test_helpers.string_contains msg "*Empty task*");
  Alcotest.(check bool)
    "has no items indicator" true
    (Test_helpers.string_contains msg "(no items)")

(** {1 Cross-Connector Consistency Tests} *)

(** Both connectors show the same checklist items for the same task. *)
let test_both_connectors_show_same_checklist_items () =
  let task_id = 42 in
  let items = make_task_scenario ~task_id () in
  let teams_card =
    Teams_progress_card.build_card ~task_id ~task_label:"Fix auth" ~items ()
  in
  let slack_msg =
    Slack_progress_checklist.render_checklist ~task_label:"Fix auth" items
  in
  let teams_json = Yojson.Safe.to_string teams_card in
  (* Both should contain all checklist titles *)
  let titles =
    [
      "Analyze codebase structure";
      "Implement auth module";
      "Write unit tests";
      "Run integration tests";
      "Deploy to staging";
    ]
  in
  List.iter
    (fun title ->
      Alcotest.(check bool)
        ("teams has " ^ title) true
        (Test_helpers.string_contains teams_json title);
      Alcotest.(check bool)
        ("slack has " ^ title) true
        (Test_helpers.string_contains slack_msg title))
    titles

(** Both connectors show the same summary information. *)
let test_both_connectors_show_same_summary () =
  let task_id = 42 in
  let items = make_task_scenario ~task_id () in
  let teams_summary = Teams_progress_card.render_summary_line items in
  let slack_summary = Slack_progress_checklist.render_summary items in
  (* Both should report same counts *)
  Alcotest.(check bool)
    "teams has done count" true
    (Test_helpers.string_contains teams_summary "2 done");
  Alcotest.(check bool)
    "slack has done count" true
    (Test_helpers.string_contains slack_summary "2/5 done")

(** Both connectors use the same overall status priority. *)
let test_both_connectors_use_same_status_priority () =
  (* Blocked takes priority *)
  let items_blocked =
    [
      make_item ~id:1 ~task_id:1 ~title:"A" ~state:Done ();
      make_item ~id:2 ~task_id:1 ~title:"B" ~state:Current ();
      make_item ~id:3 ~task_id:1 ~title:"C" ~state:Blocked ();
    ]
  in
  let teams_style = Teams_progress_card.overall_style items_blocked in
  let slack_icon = Slack_progress_checklist.overall_icon items_blocked in
  Alcotest.(check string) "teams blocked priority" "Blocked" teams_style.label;
  Alcotest.(check bool)
    "slack blocked icon" true
    (Test_helpers.string_contains slack_icon "\xF0\x9F\x9A\xAB")

(** Both connectors produce valid fallback/plain text for accessibility. *)
let test_both_connectors_produce_valid_fallback_text () =
  let task_id = 42 in
  let items = make_task_scenario ~task_id () in
  let teams_fallback =
    Teams_progress_card.build_fallback_text ~task_label:"Fix auth" ~items ()
  in
  (* Fallback should use plain text icons *)
  Alcotest.(check bool)
    "has done icon" true
    (Test_helpers.string_contains teams_fallback "[x]");
  Alcotest.(check bool)
    "has current icon" true
    (Test_helpers.string_contains teams_fallback "[~]");
  Alcotest.(check bool)
    "has planned icon" true
    (Test_helpers.string_contains teams_fallback "[ ]")

(** {1 Security Tests} *)

(** Blocked items never leak secrets in either connector. *)
let test_blocked_items_never_leak_secrets () =
  let task_id = 42 in
  let items =
    [
      make_item ~id:1 ~task_id ~title:"Analyze codebase" ~state:Done ();
      make_item ~id:2 ~task_id ~title:"Connect to API" ~state:Blocked
        ~delivery_state:
          (Delivery_failed "password=abc123 token=Bearer secret-key") ();
    ]
  in
  let teams_card =
    Teams_progress_card.build_card ~task_id ~task_label:"Fix auth" ~items ()
  in
  let teams_json = Yojson.Safe.to_string teams_card in
  let slack_msg =
    Slack_progress_checklist.render_checklist ~task_label:"Fix auth" items
  in
  (* No secrets in either output *)
  Alcotest.(check bool)
    "no password in teams" false
    (Test_helpers.string_contains teams_json "password=abc123");
  Alcotest.(check bool)
    "no token in teams" false
    (Test_helpers.string_contains teams_json "token=Bearer");
  Alcotest.(check bool)
    "no secret in teams" false
    (Test_helpers.string_contains teams_json "secret-key");
  Alcotest.(check bool)
    "no password in slack" false
    (Test_helpers.string_contains slack_msg "password=abc123");
  Alcotest.(check bool)
    "no token in slack" false
    (Test_helpers.string_contains slack_msg "token=Bearer");
  Alcotest.(check bool)
    "no secret in slack" false
    (Test_helpers.string_contains slack_msg "secret-key")

(** URL sanitization prevents credential leakage in links. *)
let test_url_sanitization_prevents_credential_leakage () =
  let task_id = 42 in
  let items =
    [
      make_item ~id:1 ~task_id ~title:"Task" ~state:Done
        ~transcript_url:(Some "https://example.com/tr?token=secret789")
        ~session_url:(Some "https://example.com/s?key=apikey123") ();
    ]
  in
  let teams_card =
    Teams_progress_card.build_card ~task_id ~task_label:"T" ~items ()
  in
  let teams_json = Yojson.Safe.to_string teams_card in
  let slack_msg =
    Slack_progress_checklist.render_checklist ~task_label:"T" items
  in
  Alcotest.(check bool)
    "no secret789 in teams" false
    (Test_helpers.string_contains teams_json "secret789");
  Alcotest.(check bool)
    "no apikey123 in teams" false
    (Test_helpers.string_contains teams_json "apikey123");
  Alcotest.(check bool)
    "no secret789 in slack" false
    (Test_helpers.string_contains slack_msg "secret789");
  Alcotest.(check bool)
    "no apikey123 in slack" false
    (Test_helpers.string_contains slack_msg "apikey123")

(** {1 Suite registration} *)

let suite =
  [
    (* Teams production path *)
    Alcotest.test_case "teams progress update renders valid card" `Quick
      test_teams_progress_update_renders_valid_card;
    Alcotest.test_case "teams checklist items with state indicators" `Quick
      test_teams_checklist_items_appear_with_state_indicators;
    Alcotest.test_case "teams actions render for task lifecycle" `Quick
      test_teams_actions_render_for_task_lifecycle;
    Alcotest.test_case "teams final artifact succeeded" `Quick
      test_teams_final_artifact_succeeded;
    Alcotest.test_case "teams final artifact failed" `Quick
      test_teams_final_artifact_failed;
    Alcotest.test_case "teams links appear in checklist" `Quick
      test_teams_links_appear_in_checklist;
    Alcotest.test_case "teams update card matches initial" `Quick
      test_teams_update_card_matches_initial;
    (* Slack baseline path *)
    Alcotest.test_case "slack progress update renders mrkdwn" `Quick
      test_slack_progress_update_renders_mrkdwn;
    Alcotest.test_case "slack state indicators correct" `Quick
      test_slack_state_indicators_correct;
    Alcotest.test_case "slack links use mrkdwn format" `Quick
      test_slack_links_use_mrkdwn_format;
    Alcotest.test_case "slack final artifact succeeded" `Quick
      test_slack_final_artifact_succeeded;
    Alcotest.test_case "slack final artifact failed" `Quick
      test_slack_final_artifact_failed;
    Alcotest.test_case "slack summary shows completion ratio" `Quick
      test_slack_summary_shows_completion_ratio;
    Alcotest.test_case "slack empty checklist valid message" `Quick
      test_slack_empty_checklist_produces_valid_message;
    (* Cross-connector consistency *)
    Alcotest.test_case "both show same checklist items" `Quick
      test_both_connectors_show_same_checklist_items;
    Alcotest.test_case "both show same summary" `Quick
      test_both_connectors_show_same_summary;
    Alcotest.test_case "both use same status priority" `Quick
      test_both_connectors_use_same_status_priority;
    Alcotest.test_case "both produce valid fallback text" `Quick
      test_both_connectors_produce_valid_fallback_text;
    (* Security *)
    Alcotest.test_case "blocked items never leak secrets" `Quick
      test_blocked_items_never_leak_secrets;
    Alcotest.test_case "url sanitization prevents credential leakage" `Quick
      test_url_sanitization_prevents_credential_leakage;
  ]

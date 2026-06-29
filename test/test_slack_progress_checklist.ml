open Room_progress_checklist

(** Helper to create a checklist item with defaults. *)
let make_item ?(id = 1) ?(task_id = 1) ?(transcript_url = None)
    ?(session_url = None) ?(delivery_state = Delivery_confirmed) ~title ~state
    () =
  {
    id;
    task_id;
    title;
    state;
    transcript_url;
    session_url;
    last_update = "2026-06-29T10:00:00Z";
    delivery_state;
  }

(** {1 render_item tests} *)

let test_render_item_done () =
  let item = make_item ~title:"Implement auth" ~state:Done () in
  let rendered = Slack_progress_checklist.render_item item in
  Alcotest.(check bool)
    "has check mark" true
    (Test_helpers.string_contains rendered "\xE2\x9C\x85");
  Alcotest.(check bool)
    "has bold title" true
    (Test_helpers.string_contains rendered "*Implement auth*");
  Alcotest.(check bool)
    "no state label" false
    (Test_helpers.string_contains rendered "(working)")

let test_render_item_current () =
  let item = make_item ~title:"Run tests" ~state:Current () in
  let rendered = Slack_progress_checklist.render_item item in
  Alcotest.(check bool)
    "has arrows" true
    (Test_helpers.string_contains rendered "\xF0\x9F\x94\x84");
  Alcotest.(check bool)
    "has working label" true
    (Test_helpers.string_contains rendered "(working)")

let test_render_item_blocked () =
  let item = make_item ~title:"Deploy" ~state:Blocked () in
  let rendered = Slack_progress_checklist.render_item item in
  Alcotest.(check bool)
    "has prohibited" true
    (Test_helpers.string_contains rendered "\xF0\x9F\x9A\xAB");
  Alcotest.(check bool)
    "has blocked label" true
    (Test_helpers.string_contains rendered "(blocked)")

let test_render_item_planned () =
  let item = make_item ~title:"Review PR" ~state:Planned () in
  let rendered = Slack_progress_checklist.render_item item in
  Alcotest.(check bool)
    "has white square" true
    (Test_helpers.string_contains rendered "\xE2\xAC\x9C");
  Alcotest.(check bool)
    "has title" true
    (Test_helpers.string_contains rendered "*Review PR*");
  Alcotest.(check bool)
    "no state label" false
    (Test_helpers.string_contains rendered "(working)")

let test_render_item_final () =
  let item = make_item ~title:"Ship it" ~state:Final () in
  let rendered = Slack_progress_checklist.render_item item in
  Alcotest.(check bool)
    "has flag" true
    (Test_helpers.string_contains rendered "\xF0\x9F\x8F\x81")

(** {1 Link rendering tests} *)

let test_render_item_with_transcript () =
  let item =
    make_item ~title:"Auth" ~state:Done
      ~transcript_url:(Some "https://example.com/tr") ()
  in
  let rendered = Slack_progress_checklist.render_item item in
  Alcotest.(check bool)
    "has transcript link" true
    (Test_helpers.string_contains rendered "<https://example.com/tr|transcript>");
  Alcotest.(check bool)
    "no session link" false
    (Test_helpers.string_contains rendered "|session>")

let test_render_item_with_session () =
  let item =
    make_item ~title:"Code" ~state:Current
      ~session_url:(Some "https://example.com/s") ()
  in
  let rendered = Slack_progress_checklist.render_item item in
  Alcotest.(check bool)
    "has session link" true
    (Test_helpers.string_contains rendered "<https://example.com/s|session>")

let test_render_item_with_both_links () =
  let item =
    make_item ~title:"Full" ~state:Done
      ~transcript_url:(Some "https://t.example.com")
      ~session_url:(Some "https://s.example.com") ()
  in
  let rendered = Slack_progress_checklist.render_item item in
  Alcotest.(check bool)
    "has transcript" true
    (Test_helpers.string_contains rendered "transcript");
  Alcotest.(check bool)
    "has session" true
    (Test_helpers.string_contains rendered "session");
  Alcotest.(check bool)
    "has separator" true
    (Test_helpers.string_contains rendered " | ")

let test_render_item_empty_links_omitted () =
  let item =
    make_item ~title:"No links" ~state:Done ~transcript_url:(Some "")
      ~session_url:(Some "  ") ()
  in
  let rendered = Slack_progress_checklist.render_item item in
  Alcotest.(check bool)
    "no link separator" false
    (Test_helpers.string_contains rendered " — ")

(** {1 Blocked item secrecy tests} *)

let test_blocked_item_no_secrets () =
  let item = make_item ~title:"Waiting on API key" ~state:Blocked () in
  let rendered = Slack_progress_checklist.render_item item in
  (* Should show generic "blocked" indicator, not internal details *)
  Alcotest.(check bool)
    "has blocked label" true
    (Test_helpers.string_contains rendered "(blocked)");
  Alcotest.(check bool)
    "no internal detail" false
    (Test_helpers.string_contains rendered "secret");
  Alcotest.(check bool)
    "no credential" false
    (Test_helpers.string_contains rendered "credential")

(** {1 render_summary tests} *)

let test_render_summary_all_done () =
  let items =
    [
      make_item ~id:1 ~title:"A" ~state:Done ();
      make_item ~id:2 ~title:"B" ~state:Done ();
      make_item ~id:3 ~title:"C" ~state:Final ();
    ]
  in
  let summary = Slack_progress_checklist.render_summary items in
  Alcotest.(check bool)
    "has ratio" true
    (Test_helpers.string_contains summary "3/3 done");
  Alcotest.(check bool)
    "no current" false
    (Test_helpers.string_contains summary "current")

let test_render_summary_mixed () =
  let items =
    [
      make_item ~id:1 ~title:"A" ~state:Done ();
      make_item ~id:2 ~title:"B" ~state:Current ();
      make_item ~id:3 ~title:"C" ~state:Blocked ();
      make_item ~id:4 ~title:"D" ~state:Planned ();
    ]
  in
  let summary = Slack_progress_checklist.render_summary items in
  Alcotest.(check bool)
    "has ratio" true
    (Test_helpers.string_contains summary "1/4 done");
  Alcotest.(check bool)
    "has current" true
    (Test_helpers.string_contains summary "1 current");
  Alcotest.(check bool)
    "has blocked" true
    (Test_helpers.string_contains summary "1 blocked");
  Alcotest.(check bool)
    "has planned" true
    (Test_helpers.string_contains summary "1 planned")

let test_render_summary_empty () =
  let summary = Slack_progress_checklist.render_summary [] in
  Alcotest.(check string) "empty" "(no items)" summary

let test_render_summary_none_done () =
  let items =
    [
      make_item ~id:1 ~title:"A" ~state:Current ();
      make_item ~id:2 ~title:"B" ~state:Planned ();
    ]
  in
  let summary = Slack_progress_checklist.render_summary items in
  Alcotest.(check bool)
    "has 0/2" true
    (Test_helpers.string_contains summary "0/2 done")

(** {1 render_checklist tests} *)

let test_render_checklist_basic () =
  let items =
    [
      make_item ~id:1 ~title:"Step one" ~state:Done ();
      make_item ~id:2 ~title:"Step two" ~state:Current ();
    ]
  in
  let msg =
    Slack_progress_checklist.render_checklist ~task_label:"Build feature" items
  in
  Alcotest.(check bool)
    "has task label" true
    (Test_helpers.string_contains msg "*Build feature*");
  Alcotest.(check bool)
    "has step one" true
    (Test_helpers.string_contains msg "Step one");
  Alcotest.(check bool)
    "has step two" true
    (Test_helpers.string_contains msg "Step two");
  Alcotest.(check bool)
    "has summary" true
    (Test_helpers.string_contains msg "1/2 done")

let test_render_checklist_with_elapsed () =
  let items = [ make_item ~id:1 ~title:"Work" ~state:Current () ] in
  let msg =
    Slack_progress_checklist.render_checklist ~task_label:"Task"
      ~elapsed:"5m 30s" items
  in
  Alcotest.(check bool)
    "has elapsed" true
    (Test_helpers.string_contains msg "5m 30s")

let test_render_checklist_empty () =
  let msg =
    Slack_progress_checklist.render_checklist ~task_label:"Empty task" []
  in
  Alcotest.(check bool)
    "has task label" true
    (Test_helpers.string_contains msg "*Empty task*");
  Alcotest.(check bool)
    "has no items" true
    (Test_helpers.string_contains msg "(no items)")

(** {1 render_final tests} *)

let test_render_final_succeeded () =
  let items = [ make_item ~id:1 ~title:"Done step" ~state:Final () ] in
  let msg =
    Slack_progress_checklist.render_final ~task_label:"Completed task"
      ~task_status:"succeeded" items
  in
  Alcotest.(check bool)
    "has check mark" true
    (Test_helpers.string_contains msg "\xE2\x9C\x85");
  Alcotest.(check bool)
    "has task label" true
    (Test_helpers.string_contains msg "*Completed task*")

let test_render_final_failed () =
  let items = [ make_item ~id:1 ~title:"Failed step" ~state:Blocked () ] in
  let msg =
    Slack_progress_checklist.render_final ~task_label:"Broken task"
      ~task_status:"failed" items
  in
  Alcotest.(check bool)
    "has cross mark" true
    (Test_helpers.string_contains msg "\xE2\x9D\x8C")

let test_render_final_with_summary () =
  let items = [ make_item ~id:1 ~title:"X" ~state:Done () ] in
  let msg =
    Slack_progress_checklist.render_final ~task_label:"T"
      ~summary:"All checks passed" items
  in
  Alcotest.(check bool)
    "has custom summary" true
    (Test_helpers.string_contains msg "All checks passed")

let test_render_final_with_elapsed () =
  let items = [ make_item ~id:1 ~title:"X" ~state:Done () ] in
  let msg =
    Slack_progress_checklist.render_final ~task_label:"T" ~elapsed:"2h" items
  in
  Alcotest.(check bool)
    "has elapsed" true
    (Test_helpers.string_contains msg "2h")

(** {1 Blocked reason masking tests} *)

let test_blocked_item_generic_indicator () =
  let items =
    [
      make_item ~id:1 ~title:"API integration" ~state:Blocked ();
      make_item ~id:2 ~title:"Database migration" ~state:Done ();
    ]
  in
  let msg =
    Slack_progress_checklist.render_checklist ~task_label:"Backend work" items
  in
  (* Blocked items show generic label, not reasons *)
  Alcotest.(check bool)
    "has blocked indicator" true
    (Test_helpers.string_contains msg "(blocked)");
  Alcotest.(check bool)
    "no leaked info" false
    (Test_helpers.string_contains msg "password");
  Alcotest.(check bool)
    "no api key" false
    (Test_helpers.string_contains msg "api_key");
  Alcotest.(check bool)
    "no token" false
    (Test_helpers.string_contains msg "Bearer")

(** {1 Overall icon selection tests} *)

let test_overall_icon_blocked_priority () =
  let items =
    [
      make_item ~id:1 ~title:"A" ~state:Done ();
      make_item ~id:2 ~title:"B" ~state:Blocked ();
      make_item ~id:3 ~title:"C" ~state:Current ();
    ]
  in
  let summary = Slack_progress_checklist.render_summary items in
  (* Blocked takes priority over current *)
  Alcotest.(check bool)
    "blocked icon" true
    (Test_helpers.string_contains summary "\xF0\x9F\x9A\xAB")

let test_overall_icon_current_priority () =
  let items =
    [
      make_item ~id:1 ~title:"A" ~state:Done ();
      make_item ~id:2 ~title:"B" ~state:Current ();
    ]
  in
  let summary = Slack_progress_checklist.render_summary items in
  Alcotest.(check bool)
    "current icon" true
    (Test_helpers.string_contains summary "\xF0\x9F\x94\x84")

let test_overall_icon_all_done () =
  let items =
    [
      make_item ~id:1 ~title:"A" ~state:Done ();
      make_item ~id:2 ~title:"B" ~state:Final ();
    ]
  in
  let summary = Slack_progress_checklist.render_summary items in
  Alcotest.(check bool)
    "done icon" true
    (Test_helpers.string_contains summary "\xE2\x9C\x85")

(** {1 Integration function tests} *)

let test_format_for_room_progress () =
  let items =
    [
      make_item ~id:1 ~title:"Step one" ~state:Done ();
      make_item ~id:2 ~title:"Step two" ~state:Current ();
    ]
  in
  let msg =
    Slack_progress_checklist.format_for_room_progress
      ~task_label:"Build feature" ~elapsed:"5m" items
  in
  Alcotest.(check bool)
    "has task label" true
    (Test_helpers.string_contains msg "*Build feature*");
  Alcotest.(check bool)
    "has completion ratio" true
    (Test_helpers.string_contains msg "1/2 done")

let test_format_final_for_room_progress () =
  let items = [ make_item ~id:1 ~title:"Done" ~state:Final () ] in
  let msg =
    Slack_progress_checklist.format_final_for_room_progress ~task_label:"Task"
      ~summary:"All good" ~task_status:"succeeded" items
  in
  Alcotest.(check bool)
    "has check mark" true
    (Test_helpers.string_contains msg "\xE2\x9C\x85");
  Alcotest.(check bool)
    "has summary" true
    (Test_helpers.string_contains msg "All good")

let suite =
  [
    Alcotest.test_case "render item done" `Quick test_render_item_done;
    Alcotest.test_case "render item current" `Quick test_render_item_current;
    Alcotest.test_case "render item blocked" `Quick test_render_item_blocked;
    Alcotest.test_case "render item planned" `Quick test_render_item_planned;
    Alcotest.test_case "render item final" `Quick test_render_item_final;
    Alcotest.test_case "render item with transcript" `Quick
      test_render_item_with_transcript;
    Alcotest.test_case "render item with session" `Quick
      test_render_item_with_session;
    Alcotest.test_case "render item with both links" `Quick
      test_render_item_with_both_links;
    Alcotest.test_case "render item empty links omitted" `Quick
      test_render_item_empty_links_omitted;
    Alcotest.test_case "blocked item no secrets" `Quick
      test_blocked_item_no_secrets;
    Alcotest.test_case "render summary all done" `Quick
      test_render_summary_all_done;
    Alcotest.test_case "render summary mixed" `Quick test_render_summary_mixed;
    Alcotest.test_case "render summary empty" `Quick test_render_summary_empty;
    Alcotest.test_case "render summary none done" `Quick
      test_render_summary_none_done;
    Alcotest.test_case "render checklist basic" `Quick
      test_render_checklist_basic;
    Alcotest.test_case "render checklist with elapsed" `Quick
      test_render_checklist_with_elapsed;
    Alcotest.test_case "render checklist empty" `Quick
      test_render_checklist_empty;
    Alcotest.test_case "render final succeeded" `Quick
      test_render_final_succeeded;
    Alcotest.test_case "render final failed" `Quick test_render_final_failed;
    Alcotest.test_case "render final with summary" `Quick
      test_render_final_with_summary;
    Alcotest.test_case "render final with elapsed" `Quick
      test_render_final_with_elapsed;
    Alcotest.test_case "blocked item generic indicator" `Quick
      test_blocked_item_generic_indicator;
    Alcotest.test_case "overall icon blocked priority" `Quick
      test_overall_icon_blocked_priority;
    Alcotest.test_case "overall icon current priority" `Quick
      test_overall_icon_current_priority;
    Alcotest.test_case "overall icon all done" `Quick test_overall_icon_all_done;
    Alcotest.test_case "format for room progress" `Quick
      test_format_for_room_progress;
    Alcotest.test_case "format final for room progress" `Quick
      test_format_final_for_room_progress;
  ]

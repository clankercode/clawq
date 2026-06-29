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

(** Extract a JSON value's string field, failing the test if absent. *)
let json_string key json =
  Yojson.Safe.Util.member key json |> Yojson.Safe.Util.to_string

(** Check that a JSON value is an Assoc containing a given string field. *)
let has_string_field ~key ~expected json =
  let actual = json_string key json in
  Alcotest.(check string) key expected actual

(** {1 style_of_state tests} *)

let test_style_of_state_planned () =
  let style = Teams_progress_card.style_of_state Planned in
  Alcotest.(check string) "label" "Planned" style.label;
  Alcotest.(check string) "color" "#6B7280" style.color

let test_style_of_state_current () =
  let style = Teams_progress_card.style_of_state Current in
  Alcotest.(check string) "label" "In Progress" style.label;
  Alcotest.(check string) "color" "#3B82F6" style.color

let test_style_of_state_blocked () =
  let style = Teams_progress_card.style_of_state Blocked in
  Alcotest.(check string) "label" "Blocked" style.label;
  Alcotest.(check string) "color" "#EF4444" style.color

let test_style_of_state_done () =
  let style = Teams_progress_card.style_of_state Done in
  Alcotest.(check string) "label" "Done" style.label;
  Alcotest.(check string) "color" "#10B981" style.color

let test_style_of_state_final () =
  let style = Teams_progress_card.style_of_state Final in
  Alcotest.(check string) "label" "Complete" style.label;
  Alcotest.(check string) "color" "#8B5CF6" style.color

(** {1 overall_style tests} *)

let test_overall_style_blocked_priority () =
  let items =
    [
      make_item ~id:1 ~title:"A" ~state:Done ();
      make_item ~id:2 ~title:"B" ~state:Current ();
      make_item ~id:3 ~title:"C" ~state:Blocked ();
    ]
  in
  let style = Teams_progress_card.overall_style items in
  Alcotest.(check string) "blocked takes priority" "Blocked" style.label

let test_overall_style_current_priority () =
  let items =
    [
      make_item ~id:1 ~title:"A" ~state:Done ();
      make_item ~id:2 ~title:"B" ~state:Current ();
    ]
  in
  let style = Teams_progress_card.overall_style items in
  Alcotest.(check string) "current takes priority" "In Progress" style.label

let test_overall_style_planned_priority () =
  let items =
    [
      make_item ~id:1 ~title:"A" ~state:Done ();
      make_item ~id:2 ~title:"B" ~state:Planned ();
    ]
  in
  let style = Teams_progress_card.overall_style items in
  Alcotest.(check string) "planned takes priority" "Planned" style.label

let test_overall_style_all_done () =
  let items =
    [
      make_item ~id:1 ~title:"A" ~state:Done ();
      make_item ~id:2 ~title:"B" ~state:Final ();
    ]
  in
  let style = Teams_progress_card.overall_style items in
  Alcotest.(check string) "all done" "Done" style.label

let test_overall_style_empty_defaults_planned () =
  let style = Teams_progress_card.overall_style [] in
  Alcotest.(check string) "empty defaults to planned" "Planned" style.label

(** {1 style_of_outcome tests} *)

let test_style_of_outcome_succeeded () =
  let style =
    Teams_progress_card.style_of_outcome Teams_progress_card.Succeeded
  in
  Alcotest.(check string) "label" "Succeeded" style.label;
  Alcotest.(check string) "color" "#10B981" style.color

let test_style_of_outcome_failed () =
  let style = Teams_progress_card.style_of_outcome Teams_progress_card.Failed in
  Alcotest.(check string) "label" "Failed" style.label;
  Alcotest.(check string) "color" "#EF4444" style.color

let test_style_of_outcome_dirty_worktree () =
  let style =
    Teams_progress_card.style_of_outcome Teams_progress_card.DirtyWorktree
  in
  Alcotest.(check string) "label" "Dirty Worktree" style.label;
  Alcotest.(check string) "color" "#F59E0B" style.color

let test_style_of_outcome_cancelled () =
  let style =
    Teams_progress_card.style_of_outcome Teams_progress_card.Cancelled
  in
  Alcotest.(check string) "label" "Cancelled" style.label;
  Alcotest.(check string) "color" "#6B7280" style.color

(** {1 build_card tests} *)

let test_build_card_basic_structure () =
  let items =
    [
      make_item ~id:1 ~title:"Step one" ~state:Done ();
      make_item ~id:2 ~title:"Step two" ~state:Current ();
    ]
  in
  let card =
    Teams_progress_card.build_card ~task_id:42 ~task_label:"Build feature"
      ~items ()
  in
  (* Top-level envelope: type=message, attachments=[...] *)
  has_string_field ~key:"type" ~expected:"message" card;
  let attachments =
    Yojson.Safe.Util.member "attachments" card |> Yojson.Safe.Util.to_list
  in
  Alcotest.(check int) "one attachment" 1 (List.length attachments);
  let att = List.hd attachments in
  has_string_field ~key:"contentType"
    ~expected:"application/vnd.microsoft.card.adaptive" att;
  let content = Yojson.Safe.Util.member "content" att in
  has_string_field ~key:"type" ~expected:"AdaptiveCard" content;
  has_string_field ~key:"version" ~expected:"1.3" content;
  has_string_field ~key:"$schema"
    ~expected:"http://adaptivecards.io/schemas/adaptive-card.json" content

let test_build_card_header_contains_task_info () =
  let items = [ make_item ~id:1 ~title:"A" ~state:Current () ] in
  let card =
    Teams_progress_card.build_card ~task_id:7 ~task_label:"Deploy" ~items ()
  in
  let attachments =
    Yojson.Safe.Util.member "attachments" card |> Yojson.Safe.Util.to_list
  in
  let content = Yojson.Safe.Util.member "content" (List.hd attachments) in
  let body =
    Yojson.Safe.Util.member "body" content |> Yojson.Safe.Util.to_list
  in
  let header = List.hd body in
  let header_text = json_string "text" header in
  Alcotest.(check bool)
    "header has task id" true
    (Test_helpers.string_contains header_text "Task #7");
  Alcotest.(check bool)
    "header has task label" true
    (Test_helpers.string_contains header_text "Deploy")

let test_build_card_contains_checklist_items () =
  let items =
    [
      make_item ~id:1 ~title:"Auth" ~state:Done ();
      make_item ~id:2 ~title:"Tests" ~state:Current ();
    ]
  in
  let card =
    Teams_progress_card.build_card ~task_id:1 ~task_label:"T" ~items ()
  in
  let json_str = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "has auth item" true
    (Test_helpers.string_contains json_str "Auth");
  Alcotest.(check bool)
    "has tests item" true
    (Test_helpers.string_contains json_str "Tests")

let test_build_card_with_elapsed () =
  let items = [ make_item ~id:1 ~title:"X" ~state:Current () ] in
  let card =
    Teams_progress_card.build_card ~task_id:1 ~task_label:"T" ~items
      ~elapsed:"5m 30s" ()
  in
  let json_str = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "has elapsed" true
    (Test_helpers.string_contains json_str "5m 30s")

let test_build_card_without_elapsed () =
  let items = [ make_item ~id:1 ~title:"X" ~state:Current () ] in
  let card =
    Teams_progress_card.build_card ~task_id:1 ~task_label:"T" ~items ()
  in
  let json_str = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "no elapsed text" false
    (Test_helpers.string_contains json_str "Elapsed:")

let test_build_card_with_summary () =
  let items = [ make_item ~id:1 ~title:"X" ~state:Done () ] in
  let card =
    Teams_progress_card.build_card ~task_id:1 ~task_label:"T" ~items
      ~summary:"All good" ()
  in
  let json_str = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "has custom summary" true
    (Test_helpers.string_contains json_str "All good")

let test_build_card_with_actions () =
  let items = [ make_item ~id:1 ~title:"X" ~state:Current () ] in
  let actions : Teams_progress_card.task_actions =
    {
      task_id = 5;
      show_retry = true;
      show_logs = true;
      show_finalize = false;
      log_path = Some "/tmp/log.txt";
    }
  in
  let card =
    Teams_progress_card.build_card ~task_id:5 ~task_label:"T" ~items
      ~actions:(Some actions) ()
  in
  let json_str = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "has retry button" true
    (Test_helpers.string_contains json_str "Retry Task");
  Alcotest.(check bool)
    "has logs button" true
    (Test_helpers.string_contains json_str "View Logs");
  Alcotest.(check bool)
    "no finalize" false
    (Test_helpers.string_contains json_str "Finalize")

let test_build_card_with_finalize_action () =
  let items = [ make_item ~id:1 ~title:"X" ~state:Current () ] in
  let actions : Teams_progress_card.task_actions =
    {
      task_id = 3;
      show_retry = false;
      show_logs = false;
      show_finalize = true;
      log_path = None;
    }
  in
  let card =
    Teams_progress_card.build_card ~task_id:3 ~task_label:"T" ~items
      ~actions:(Some actions) ()
  in
  let json_str = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "has finalize button" true
    (Test_helpers.string_contains json_str "Finalize");
  Alcotest.(check bool)
    "no retry" false
    (Test_helpers.string_contains json_str "Retry Task")

let test_build_card_no_actions () =
  let items = [ make_item ~id:1 ~title:"X" ~state:Current () ] in
  let card =
    Teams_progress_card.build_card ~task_id:1 ~task_label:"T" ~items ()
  in
  let json_str = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "no action buttons" false
    (Test_helpers.string_contains json_str "ActionSet")

let test_build_card_with_task_outcome () =
  let items = [ make_item ~id:1 ~title:"X" ~state:Final () ] in
  let card =
    Teams_progress_card.build_card ~task_id:1 ~task_label:"T" ~items
      ~task_outcome:Teams_progress_card.Succeeded ()
  in
  let json_str = Yojson.Safe.to_string card in
  (* Succeeded outcome uses green (#10B981) which maps to "Good" color *)
  Alcotest.(check bool)
    "has Good color" true
    (Test_helpers.string_contains json_str "Good");
  (* Header shows the checkmark icon *)
  Alcotest.(check bool)
    "has checkmark icon" true
    (Test_helpers.string_contains json_str "\xE2\x9C\x85")

let test_build_card_with_failed_outcome () =
  let items = [ make_item ~id:1 ~title:"X" ~state:Blocked () ] in
  let card =
    Teams_progress_card.build_card ~task_id:1 ~task_label:"T" ~items
      ~task_outcome:Teams_progress_card.Failed ()
  in
  let json_str = Yojson.Safe.to_string card in
  (* Failed outcome uses red (#EF4444) which maps to "Attention" color *)
  Alcotest.(check bool)
    "has Attention color" true
    (Test_helpers.string_contains json_str "Attention");
  (* Header shows the cross mark icon *)
  Alcotest.(check bool)
    "has cross mark icon" true
    (Test_helpers.string_contains json_str "\xE2\x9D\x8C")

(** {1 build_update_card tests} *)

let test_build_update_card_no_envelope () =
  let items = [ make_item ~id:1 ~title:"X" ~state:Current () ] in
  let card =
    Teams_progress_card.build_update_card ~task_id:1 ~task_label:"T" ~items ()
  in
  (* build_update_card returns the same message envelope as build_card *)
  has_string_field ~key:"type" ~expected:"message" card;
  let attachments =
    Yojson.Safe.Util.member "attachments" card |> Yojson.Safe.Util.to_list
  in
  Alcotest.(check int) "one attachment" 1 (List.length attachments);
  let content = Yojson.Safe.Util.member "content" (List.hd attachments) in
  has_string_field ~key:"type" ~expected:"AdaptiveCard" content;
  has_string_field ~key:"version" ~expected:"1.3" content

let test_build_update_card_same_structure_as_build () =
  let items =
    [
      make_item ~id:1 ~title:"A" ~state:Done ();
      make_item ~id:2 ~title:"B" ~state:Current ();
    ]
  in
  let update_card =
    Teams_progress_card.build_update_card ~task_id:42 ~task_label:"F" ~items ()
  in
  let build_card =
    Teams_progress_card.build_card ~task_id:42 ~task_label:"F" ~items ()
  in
  (* Both should have the same envelope structure *)
  let update_content =
    Yojson.Safe.Util.member "attachments" update_card
    |> Yojson.Safe.Util.to_list |> List.hd
    |> Yojson.Safe.Util.member "content"
  in
  let build_content =
    Yojson.Safe.Util.member "attachments" build_card
    |> Yojson.Safe.Util.to_list |> List.hd
    |> Yojson.Safe.Util.member "content"
  in
  let update_body =
    Yojson.Safe.Util.member "body" update_content |> Yojson.Safe.Util.to_list
  in
  let build_body =
    Yojson.Safe.Util.member "body" build_content |> Yojson.Safe.Util.to_list
  in
  Alcotest.(check int)
    "same body length" (List.length build_body) (List.length update_body)

(** {1 build_fallback_text tests} *)

let test_build_fallback_text_basic () =
  let items =
    [
      make_item ~id:1 ~title:"Step one" ~state:Done ();
      make_item ~id:2 ~title:"Step two" ~state:Current ();
    ]
  in
  let text =
    Teams_progress_card.build_fallback_text ~task_label:"Build feature" ~items
      ()
  in
  Alcotest.(check bool)
    "has task label" true
    (Test_helpers.string_contains text "Build feature");
  Alcotest.(check bool)
    "has step one" true
    (Test_helpers.string_contains text "Step one");
  Alcotest.(check bool)
    "has step two" true
    (Test_helpers.string_contains text "Step two")

let test_build_fallback_text_empty () =
  let text =
    Teams_progress_card.build_fallback_text ~task_label:"Empty task" ~items:[]
      ()
  in
  Alcotest.(check bool)
    "has task label" true
    (Test_helpers.string_contains text "Empty task")

let test_build_fallback_text_with_summary () =
  let items = [ make_item ~id:1 ~title:"X" ~state:Done () ] in
  let text =
    Teams_progress_card.build_fallback_text ~task_label:"T" ~items
      ~summary:"Custom summary" ()
  in
  Alcotest.(check bool)
    "has custom summary" true
    (Test_helpers.string_contains text "Custom summary")

let test_build_fallback_text_uses_plain_text_icons () =
  let items =
    [
      make_item ~id:1 ~title:"Done step" ~state:Done ();
      make_item ~id:2 ~title:"Blocked step" ~state:Blocked ();
    ]
  in
  let text =
    Teams_progress_card.build_fallback_text ~task_label:"T" ~items ()
  in
  (* Fallback uses plain text state_icon from room_progress_checklist *)
  Alcotest.(check bool)
    "has done icon" true
    (Test_helpers.string_contains text "[x]");
  Alcotest.(check bool)
    "has blocked icon" true
    (Test_helpers.string_contains text "[!]")

(** {1 Blocked item reason tests} *)

let test_blocked_item_no_secret_leakage () =
  let items =
    [
      make_item ~id:1 ~title:"Waiting on API key" ~state:Blocked ();
      make_item ~id:2 ~title:"Database migration" ~state:Done ();
    ]
  in
  let card =
    Teams_progress_card.build_card ~task_id:1 ~task_label:"Backend" ~items ()
  in
  let json_str = Yojson.Safe.to_string card in
  (* Should show blocked indicator, not leaked secrets *)
  Alcotest.(check bool)
    "has blocked indicator" true
    (Test_helpers.string_contains json_str "blocked");
  Alcotest.(check bool)
    "no password" false
    (Test_helpers.string_contains json_str "password");
  Alcotest.(check bool)
    "no api_key" false
    (Test_helpers.string_contains json_str "api_key");
  Alcotest.(check bool)
    "no Bearer" false
    (Test_helpers.string_contains json_str "Bearer")

let test_blocked_item_generic_indicator_only () =
  let item = make_item ~title:"Deploy blocked" ~state:Blocked () in
  let card =
    Teams_progress_card.build_card ~task_id:1 ~task_label:"T" ~items:[ item ] ()
  in
  let json_str = Yojson.Safe.to_string card in
  (* The blocked item should show "(blocked)" as a generic label *)
  Alcotest.(check bool)
    "has generic blocked label" true
    (Test_helpers.string_contains json_str "(blocked)")

let test_blocked_item_fallback_no_secrets () =
  let items =
    [ make_item ~id:1 ~title:"Waiting for credentials" ~state:Blocked () ]
  in
  let text =
    Teams_progress_card.build_fallback_text ~task_label:"T" ~items ()
  in
  Alcotest.(check bool)
    "has blocked icon" true
    (Test_helpers.string_contains text "[!]");
  Alcotest.(check bool)
    "no credential leak" false
    (Test_helpers.string_contains text "secret");
  Alcotest.(check bool)
    "no token" false
    (Test_helpers.string_contains text "Bearer")

let test_delivery_failed_secret_not_in_teams_card () =
  let items =
    [
      make_item ~id:1 ~title:"Auth step" ~state:Blocked
        ~delivery_state:
          (Delivery_failed "password=abc123 token=Bearer secret-key") ();
    ]
  in
  let card =
    Teams_progress_card.build_card ~task_id:1 ~task_label:"T" ~items ()
  in
  let json_str = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "no password in card" false
    (Test_helpers.string_contains json_str "password=abc123");
  Alcotest.(check bool)
    "no token in card" false
    (Test_helpers.string_contains json_str "token=Bearer");
  Alcotest.(check bool)
    "no secret-key in card" false
    (Test_helpers.string_contains json_str "secret-key")

let test_delivery_failed_secret_not_in_slack_mrkdwn () =
  let items =
    [
      make_item ~id:1 ~title:"Deploy step" ~state:Blocked
        ~delivery_state:(Delivery_failed "api_key=xyz789 credential=leaked") ();
    ]
  in
  let msg = Slack_progress_checklist.render_checklist ~task_label:"T" items in
  Alcotest.(check bool)
    "no api_key in slack" false
    (Test_helpers.string_contains msg "api_key=xyz789");
  Alcotest.(check bool)
    "no credential in slack" false
    (Test_helpers.string_contains msg "credential=leaked")

let test_delivery_failed_secret_not_in_fallback_text () =
  let items =
    [
      make_item ~id:1 ~title:"Review step" ~state:Blocked
        ~delivery_state:(Delivery_failed "Bearer eyJhbGciOiJIUzI1NiJ9 secret")
        ();
    ]
  in
  let text =
    Teams_progress_card.build_fallback_text ~task_label:"T" ~items ()
  in
  Alcotest.(check bool)
    "no Bearer in fallback" false
    (Test_helpers.string_contains text "eyJhbGciOiJIUzI1NiJ9");
  Alcotest.(check bool)
    "no secret in fallback" false
    (Test_helpers.string_contains text "secret")

(** {1 Empty checklist tests} *)

let test_build_card_empty_checklist () =
  let card =
    Teams_progress_card.build_card ~task_id:1 ~task_label:"Empty" ~items:[] ()
  in
  let json_str = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "has task label" true
    (Test_helpers.string_contains json_str "Empty");
  (* Should still produce valid Adaptive Card structure *)
  has_string_field ~key:"type" ~expected:"message" card

let test_build_update_card_empty_checklist () =
  let card =
    Teams_progress_card.build_update_card ~task_id:1 ~task_label:"Empty"
      ~items:[] ()
  in
  (* build_update_card returns message envelope, not raw card *)
  has_string_field ~key:"type" ~expected:"message" card

let test_build_fallback_text_empty_checklist () =
  let text =
    Teams_progress_card.build_fallback_text ~task_label:"Empty" ~items:[] ()
  in
  Alcotest.(check bool)
    "has task label" true
    (Test_helpers.string_contains text "Empty")

(** {1 Item element rendering tests} *)

let test_item_element_has_text_block_type () =
  let item = make_item ~title:"Test" ~state:Done () in
  let element = Teams_progress_card.render_item_element item in
  has_string_field ~key:"type" ~expected:"TextBlock" element;
  has_string_field ~key:"spacing" ~expected:"Small" element;
  let wrap =
    Yojson.Safe.Util.member "wrap" element |> Yojson.Safe.Util.to_bool
  in
  Alcotest.(check bool) "wrap is true" true wrap

let test_item_element_contains_title () =
  let item = make_item ~title:"Implement auth" ~state:Done () in
  let element = Teams_progress_card.render_item_element item in
  let text = json_string "text" element in
  Alcotest.(check bool)
    "has title" true
    (Test_helpers.string_contains text "Implement auth")

let test_item_element_current_has_working_label () =
  let item = make_item ~title:"T" ~state:Current () in
  let element = Teams_progress_card.render_item_element item in
  let text = json_string "text" element in
  Alcotest.(check bool)
    "has working" true
    (Test_helpers.string_contains text "(working)")

let test_item_element_blocked_has_blocked_label () =
  let item = make_item ~title:"T" ~state:Blocked () in
  let element = Teams_progress_card.render_item_element item in
  let text = json_string "text" element in
  Alcotest.(check bool)
    "has blocked" true
    (Test_helpers.string_contains text "(blocked)")

let test_item_element_with_transcript_link () =
  let item =
    make_item ~title:"T" ~state:Done
      ~transcript_url:(Some "https://example.com/tr") ()
  in
  let element = Teams_progress_card.render_item_element item in
  let text = json_string "text" element in
  Alcotest.(check bool)
    "has transcript link" true
    (Test_helpers.string_contains text "[transcript](https://example.com/tr)")

let test_item_element_with_session_link () =
  let item =
    make_item ~title:"T" ~state:Done ~session_url:(Some "https://example.com/s")
      ()
  in
  let element = Teams_progress_card.render_item_element item in
  let text = json_string "text" element in
  Alcotest.(check bool)
    "has session link" true
    (Test_helpers.string_contains text "[session](https://example.com/s)")

let test_item_element_with_both_links () =
  let item =
    make_item ~title:"T" ~state:Done
      ~transcript_url:(Some "https://t.example.com")
      ~session_url:(Some "https://s.example.com") ()
  in
  let element = Teams_progress_card.render_item_element item in
  let text = json_string "text" element in
  Alcotest.(check bool)
    "has transcript" true
    (Test_helpers.string_contains text "transcript");
  Alcotest.(check bool)
    "has session" true
    (Test_helpers.string_contains text "session");
  Alcotest.(check bool)
    "has separator" true
    (Test_helpers.string_contains text " | ")

let test_item_element_empty_links_omitted () =
  let item =
    make_item ~title:"T" ~state:Done ~transcript_url:(Some "")
      ~session_url:(Some "  ") ()
  in
  let element = Teams_progress_card.render_item_element item in
  let text = json_string "text" element in
  Alcotest.(check bool)
    "no link separator" false
    (Test_helpers.string_contains text " — ")

(** {1 Summary line tests} *)

let test_render_summary_line_all_done () =
  let items =
    [
      make_item ~id:1 ~title:"A" ~state:Done ();
      make_item ~id:2 ~title:"B" ~state:Done ();
      make_item ~id:3 ~title:"C" ~state:Final ();
    ]
  in
  let summary = Teams_progress_card.render_summary_line items in
  Alcotest.(check bool)
    "has done count" true
    (Test_helpers.string_contains summary "2 done");
  Alcotest.(check bool)
    "has final count" true
    (Test_helpers.string_contains summary "1 final")

let test_render_summary_line_mixed () =
  let items =
    [
      make_item ~id:1 ~title:"A" ~state:Done ();
      make_item ~id:2 ~title:"B" ~state:Current ();
      make_item ~id:3 ~title:"C" ~state:Blocked ();
      make_item ~id:4 ~title:"D" ~state:Planned ();
    ]
  in
  let summary = Teams_progress_card.render_summary_line items in
  Alcotest.(check bool)
    "has done count" true
    (Test_helpers.string_contains summary "1 done");
  Alcotest.(check bool)
    "has current count" true
    (Test_helpers.string_contains summary "1 current");
  Alcotest.(check bool)
    "has blocked count" true
    (Test_helpers.string_contains summary "1 blocked");
  Alcotest.(check bool)
    "has planned count" true
    (Test_helpers.string_contains summary "1 planned")

let test_render_summary_line_empty () =
  let summary = Teams_progress_card.render_summary_line [] in
  Alcotest.(check string) "empty" "No items yet" summary

(** {1 Action controls tests} *)

let test_render_actions_retry_and_logs () =
  let actions : Teams_progress_card.task_actions =
    {
      task_id = 10;
      show_retry = true;
      show_logs = true;
      show_finalize = false;
      log_path = Some "/tmp/log.txt";
    }
  in
  let elements = Teams_progress_card.render_actions actions in
  Alcotest.(check int) "one ActionSet" 1 (List.length elements);
  let json_str = Yojson.Safe.to_string (`List elements) in
  Alcotest.(check bool)
    "has retry" true
    (Test_helpers.string_contains json_str "Retry Task");
  Alcotest.(check bool)
    "has logs" true
    (Test_helpers.string_contains json_str "View Logs");
  Alcotest.(check bool)
    "has imBack" true
    (Test_helpers.string_contains json_str "imBack")

let test_render_actions_empty () =
  let actions : Teams_progress_card.task_actions =
    {
      task_id = 1;
      show_retry = false;
      show_logs = false;
      show_finalize = false;
      log_path = None;
    }
  in
  let elements = Teams_progress_card.render_actions actions in
  Alcotest.(check int) "no elements" 0 (List.length elements)

let test_render_actions_retry_command () =
  let actions : Teams_progress_card.task_actions =
    {
      task_id = 42;
      show_retry = true;
      show_logs = false;
      show_finalize = false;
      log_path = None;
    }
  in
  let elements = Teams_progress_card.render_actions actions in
  let json_str = Yojson.Safe.to_string (`List elements) in
  Alcotest.(check bool)
    "retry command has task id" true
    (Test_helpers.string_contains json_str "/background retry 42")

let test_render_actions_finalize_command () =
  let actions : Teams_progress_card.task_actions =
    {
      task_id = 7;
      show_retry = false;
      show_logs = false;
      show_finalize = true;
      log_path = None;
    }
  in
  let elements = Teams_progress_card.render_actions actions in
  let json_str = Yojson.Safe.to_string (`List elements) in
  Alcotest.(check bool)
    "finalize command has task id" true
    (Test_helpers.string_contains json_str "/background finalize 7")

let test_render_actions_logs_command () =
  let actions : Teams_progress_card.task_actions =
    {
      task_id = 3;
      show_retry = false;
      show_logs = true;
      show_finalize = false;
      log_path = Some "/tmp/out.log";
    }
  in
  let elements = Teams_progress_card.render_actions actions in
  let json_str = Yojson.Safe.to_string (`List elements) in
  Alcotest.(check bool)
    "logs command has task id" true
    (Test_helpers.string_contains json_str "/background logs 3")

(** {1 Connector-specific rendering tests} *)

let test_teams_card_has_teams_content_type () =
  let items = [ make_item ~id:1 ~title:"X" ~state:Current () ] in
  let card =
    Teams_progress_card.build_card ~task_id:1 ~task_label:"T" ~items ()
  in
  let json_str = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "has Teams content type" true
    (Test_helpers.string_contains json_str
       "application/vnd.microsoft.card.adaptive")

let test_teams_card_has_adaptive_card_schema () =
  let items = [ make_item ~id:1 ~title:"X" ~state:Current () ] in
  let card =
    Teams_progress_card.build_card ~task_id:1 ~task_label:"T" ~items ()
  in
  let json_str = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "has adaptive card schema" true
    (Test_helpers.string_contains json_str "adaptivecards.io")

let test_teams_card_imback_action_format () =
  let items = [ make_item ~id:1 ~title:"X" ~state:Current () ] in
  let actions : Teams_progress_card.task_actions =
    {
      task_id = 5;
      show_retry = true;
      show_logs = false;
      show_finalize = false;
      log_path = None;
    }
  in
  let card =
    Teams_progress_card.build_card ~task_id:5 ~task_label:"T" ~items
      ~actions:(Some actions) ()
  in
  let json_str = Yojson.Safe.to_string card in
  (* Teams uses msteams imBack type for action buttons *)
  Alcotest.(check bool)
    "has msteams" true
    (Test_helpers.string_contains json_str "msteams");
  Alcotest.(check bool)
    "has imBack type" true
    (Test_helpers.string_contains json_str "\"imBack\"")

let test_slack_renderer_uses_mrkdwn_format () =
  let items =
    [
      make_item ~id:1 ~title:"Deploy" ~state:Current ();
      make_item ~id:2 ~title:"Test" ~state:Done ();
    ]
  in
  let msg =
    Slack_progress_checklist.render_checklist ~task_label:"Build" items
  in
  (* Slack uses mrkdwn: bold with asterisks *)
  Alcotest.(check bool)
    "has bold title" true
    (Test_helpers.string_contains msg "*Build*");
  (* Slack uses slack-format links: <url|label> *)
  Alcotest.(check bool)
    "no markdown links" false
    (Test_helpers.string_contains msg "[transcript](")

let test_fallback_text_plain_format () =
  let items =
    [
      make_item ~id:1 ~title:"Step" ~state:Done
        ~transcript_url:(Some "https://t.example.com") ();
    ]
  in
  let text =
    Teams_progress_card.build_fallback_text ~task_label:"T" ~items ()
  in
  (* Plain text fallback uses room_progress_checklist's plain text format *)
  Alcotest.(check bool)
    "has plain text icon" true
    (Test_helpers.string_contains text "[x]");
  Alcotest.(check bool)
    "has plain text link format" true
    (Test_helpers.string_contains text "(transcript:");
  Alcotest.(check bool)
    "no mrkdwn bold" false
    (Test_helpers.string_contains text "*Step*")

let suite =
  [
    Alcotest.test_case "style of state planned" `Quick
      test_style_of_state_planned;
    Alcotest.test_case "style of state current" `Quick
      test_style_of_state_current;
    Alcotest.test_case "style of state blocked" `Quick
      test_style_of_state_blocked;
    Alcotest.test_case "style of state done" `Quick test_style_of_state_done;
    Alcotest.test_case "style of state final" `Quick test_style_of_state_final;
    Alcotest.test_case "overall style blocked priority" `Quick
      test_overall_style_blocked_priority;
    Alcotest.test_case "overall style current priority" `Quick
      test_overall_style_current_priority;
    Alcotest.test_case "overall style planned priority" `Quick
      test_overall_style_planned_priority;
    Alcotest.test_case "overall style all done" `Quick
      test_overall_style_all_done;
    Alcotest.test_case "overall style empty defaults planned" `Quick
      test_overall_style_empty_defaults_planned;
    Alcotest.test_case "style of outcome succeeded" `Quick
      test_style_of_outcome_succeeded;
    Alcotest.test_case "style of outcome failed" `Quick
      test_style_of_outcome_failed;
    Alcotest.test_case "style of outcome dirty worktree" `Quick
      test_style_of_outcome_dirty_worktree;
    Alcotest.test_case "style of outcome cancelled" `Quick
      test_style_of_outcome_cancelled;
    Alcotest.test_case "build card basic structure" `Quick
      test_build_card_basic_structure;
    Alcotest.test_case "build card header contains task info" `Quick
      test_build_card_header_contains_task_info;
    Alcotest.test_case "build card contains checklist items" `Quick
      test_build_card_contains_checklist_items;
    Alcotest.test_case "build card with elapsed" `Quick
      test_build_card_with_elapsed;
    Alcotest.test_case "build card without elapsed" `Quick
      test_build_card_without_elapsed;
    Alcotest.test_case "build card with summary" `Quick
      test_build_card_with_summary;
    Alcotest.test_case "build card with actions" `Quick
      test_build_card_with_actions;
    Alcotest.test_case "build card with finalize action" `Quick
      test_build_card_with_finalize_action;
    Alcotest.test_case "build card no actions" `Quick test_build_card_no_actions;
    Alcotest.test_case "build card with task outcome" `Quick
      test_build_card_with_task_outcome;
    Alcotest.test_case "build card with failed outcome" `Quick
      test_build_card_with_failed_outcome;
    Alcotest.test_case "build update card no envelope" `Quick
      test_build_update_card_no_envelope;
    Alcotest.test_case "build update card same structure" `Quick
      test_build_update_card_same_structure_as_build;
    Alcotest.test_case "build fallback text basic" `Quick
      test_build_fallback_text_basic;
    Alcotest.test_case "build fallback text empty" `Quick
      test_build_fallback_text_empty;
    Alcotest.test_case "build fallback text with summary" `Quick
      test_build_fallback_text_with_summary;
    Alcotest.test_case "build fallback text uses plain text icons" `Quick
      test_build_fallback_text_uses_plain_text_icons;
    Alcotest.test_case "blocked item no secret leakage" `Quick
      test_blocked_item_no_secret_leakage;
    Alcotest.test_case "blocked item generic indicator only" `Quick
      test_blocked_item_generic_indicator_only;
    Alcotest.test_case "blocked item fallback no secrets" `Quick
      test_blocked_item_fallback_no_secrets;
    Alcotest.test_case "delivery failed secret not in teams card" `Quick
      test_delivery_failed_secret_not_in_teams_card;
    Alcotest.test_case "delivery failed secret not in slack mrkdwn" `Quick
      test_delivery_failed_secret_not_in_slack_mrkdwn;
    Alcotest.test_case "delivery failed secret not in fallback text" `Quick
      test_delivery_failed_secret_not_in_fallback_text;
    Alcotest.test_case "build card empty checklist" `Quick
      test_build_card_empty_checklist;
    Alcotest.test_case "build update card empty checklist" `Quick
      test_build_update_card_empty_checklist;
    Alcotest.test_case "build fallback text empty checklist" `Quick
      test_build_fallback_text_empty_checklist;
    Alcotest.test_case "item element has text block type" `Quick
      test_item_element_has_text_block_type;
    Alcotest.test_case "item element contains title" `Quick
      test_item_element_contains_title;
    Alcotest.test_case "item element current has working label" `Quick
      test_item_element_current_has_working_label;
    Alcotest.test_case "item element blocked has blocked label" `Quick
      test_item_element_blocked_has_blocked_label;
    Alcotest.test_case "item element with transcript link" `Quick
      test_item_element_with_transcript_link;
    Alcotest.test_case "item element with session link" `Quick
      test_item_element_with_session_link;
    Alcotest.test_case "item element with both links" `Quick
      test_item_element_with_both_links;
    Alcotest.test_case "item element empty links omitted" `Quick
      test_item_element_empty_links_omitted;
    Alcotest.test_case "render summary line all done" `Quick
      test_render_summary_line_all_done;
    Alcotest.test_case "render summary line mixed" `Quick
      test_render_summary_line_mixed;
    Alcotest.test_case "render summary line empty" `Quick
      test_render_summary_line_empty;
    Alcotest.test_case "render actions retry and logs" `Quick
      test_render_actions_retry_and_logs;
    Alcotest.test_case "render actions empty" `Quick test_render_actions_empty;
    Alcotest.test_case "render actions retry command" `Quick
      test_render_actions_retry_command;
    Alcotest.test_case "render actions finalize command" `Quick
      test_render_actions_finalize_command;
    Alcotest.test_case "render actions logs command" `Quick
      test_render_actions_logs_command;
    Alcotest.test_case "teams card has teams content type" `Quick
      test_teams_card_has_teams_content_type;
    Alcotest.test_case "teams card has adaptive card schema" `Quick
      test_teams_card_has_adaptive_card_schema;
    Alcotest.test_case "teams card imback action format" `Quick
      test_teams_card_imback_action_format;
    Alcotest.test_case "slack renderer uses mrkdwn format" `Quick
      test_slack_renderer_uses_mrkdwn_format;
    Alcotest.test_case "fallback text plain format" `Quick
      test_fallback_text_plain_format;
  ]

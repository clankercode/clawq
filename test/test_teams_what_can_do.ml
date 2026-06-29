(** Tests for Teams_what_can_do capability card. *)

(** Helper to build a capability_status with all defaults. *)
let make_status ?(edit = true) ?(delete = true) ?(react = false)
    ?(typing_indicator = true) ?(status_messages = true) ?(file_sending = true)
    ?(adaptive_cards = true) ?(buttons = true) ?(history_capture = false)
    ?(profile_bound = false) ?(memory_available = true)
    ?(github_configured = false) ?(connector_history_enabled = false)
    ?(connector_history_persist = false) ?(delivery_mode = "edit in place")
    ?(max_message_length = 28672) () : Teams_what_can_do.capability_status =
  {
    edit;
    delete;
    react;
    typing_indicator;
    status_messages;
    file_sending;
    adaptive_cards;
    buttons;
    history_capture;
    profile_bound;
    memory_available;
    github_configured;
    connector_history_enabled;
    connector_history_persist;
    delivery_mode;
    max_message_length;
  }

(** {1 Degraded behaviors tests} *)

let test_degraded_no_memory () =
  let snap = make_status ~memory_available:false () in
  let degraded = Teams_what_can_do.degraded_behaviors snap in
  let features =
    List.map (fun (d : Teams_what_can_do.degraded_item) -> d.feature) degraded
  in
  Alcotest.(check bool) "Memory in degraded" true (List.mem "Memory" features);
  Alcotest.(check bool)
    "Room memory in degraded" true
    (List.mem "Room memory" features);
  Alcotest.(check bool)
    "History (DB) in degraded" true
    (List.mem "History (DB)" features)

let test_degraded_no_profile_binding () =
  let snap = make_status ~profile_bound:false () in
  let degraded = Teams_what_can_do.degraded_behaviors snap in
  let features =
    List.map (fun (d : Teams_what_can_do.degraded_item) -> d.feature) degraded
  in
  Alcotest.(check bool)
    "Scoped access in degraded" true
    (List.mem "Scoped access" features);
  Alcotest.(check bool)
    "Room-scoped memory in degraded" true
    (List.mem "Room-scoped memory" features)

let test_degraded_no_history_disabled () =
  let snap = make_status ~connector_history_enabled:false () in
  let degraded = Teams_what_can_do.degraded_behaviors snap in
  let features =
    List.map (fun (d : Teams_what_can_do.degraded_item) -> d.feature) degraded
  in
  Alcotest.(check bool)
    "History capture in degraded" true
    (List.mem "History capture" features);
  (* Should not include persistence warning when history is disabled *)
  Alcotest.(check bool)
    "No History persistence" false
    (List.mem "History persistence" features)

let test_degraded_no_history_no_binding () =
  let snap =
    make_status ~connector_history_enabled:true ~profile_bound:false ()
  in
  let degraded = Teams_what_can_do.degraded_behaviors snap in
  let features =
    List.map (fun (d : Teams_what_can_do.degraded_item) -> d.feature) degraded
  in
  Alcotest.(check bool)
    "History capture in degraded" true
    (List.mem "History capture" features)

let test_degraded_no_persist () =
  let snap =
    make_status ~connector_history_enabled:true ~connector_history_persist:false
      ()
  in
  let degraded = Teams_what_can_do.degraded_behaviors snap in
  let features =
    List.map (fun (d : Teams_what_can_do.degraded_item) -> d.feature) degraded
  in
  Alcotest.(check bool)
    "History persistence in degraded" true
    (List.mem "History persistence" features)

let test_degraded_no_github () =
  let snap = make_status ~github_configured:false () in
  let degraded = Teams_what_can_do.degraded_behaviors snap in
  let features =
    List.map (fun (d : Teams_what_can_do.degraded_item) -> d.feature) degraded
  in
  Alcotest.(check bool) "GitHub in degraded" true (List.mem "GitHub" features)

let test_degraded_none_when_all_good () =
  let snap =
    make_status ~memory_available:true ~profile_bound:true ~history_capture:true
      ~connector_history_enabled:true ~connector_history_persist:true
      ~github_configured:true ()
  in
  let degraded = Teams_what_can_do.degraded_behaviors snap in
  Alcotest.(check int) "No degraded items" 0 (List.length degraded)

(** {1 Adaptive Card tests} *)

let json_string key json =
  Yojson.Safe.Util.member key json |> Yojson.Safe.Util.to_string

let test_card_is_bot_framework_envelope () =
  let snap = make_status () in
  let card = Teams_what_can_do.build_card ~snap () in
  let msg_type = json_string "type" card in
  Alcotest.(check string) "type is message" "message" msg_type;
  let attachments =
    Yojson.Safe.Util.member "attachments" card |> Yojson.Safe.Util.to_list
  in
  Alcotest.(check int) "one attachment" 1 (List.length attachments);
  let att = List.hd attachments in
  let content_type = json_string "contentType" att in
  Alcotest.(check string)
    "content type" "application/vnd.microsoft.card.adaptive" content_type;
  let content = Yojson.Safe.Util.member "content" att in
  let card_type = json_string "type" content in
  Alcotest.(check string) "card type" "AdaptiveCard" card_type

let test_card_has_body_elements () =
  let snap = make_status () in
  let card = Teams_what_can_do.build_card ~snap () in
  let content =
    Yojson.Safe.Util.member "attachments" card
    |> Yojson.Safe.Util.to_list |> List.hd
    |> Yojson.Safe.Util.member "content"
  in
  let body =
    Yojson.Safe.Util.member "body" content |> Yojson.Safe.Util.to_list
  in
  Alcotest.(check bool) "body has elements" true (List.length body > 0)

(** {1 Text fallback tests} *)

let test_text_contains_capabilities () =
  let snap = make_status () in
  let text = Teams_what_can_do.build_text ~snap () in
  Alcotest.(check bool)
    "text contains Edit messages" true
    (Option.is_some (Astring.String.find_sub ~sub:"Edit messages" text)
    || Astring.String.is_infix ~affix:"Edit messages" text)

let test_text_contains_degraded () =
  let snap = make_status ~memory_available:false () in
  let text = Teams_what_can_do.build_text ~snap () in
  Alcotest.(check bool)
    "text contains Degraded" true
    (Astring.String.is_infix ~affix:"Degraded Behaviors" text)

(** {1 Delivery mode tests} *)

let test_delivery_mode_edit () =
  let caps = Connector_capabilities.teams in
  let mode = Teams_what_can_do.delivery_mode_of_caps caps in
  Alcotest.(check string) "teams delivery" "edit in place" mode

(** {1 Suite} *)

let suite =
  [
    Alcotest.test_case "degraded no memory" `Quick test_degraded_no_memory;
    Alcotest.test_case "degraded no profile binding" `Quick
      test_degraded_no_profile_binding;
    Alcotest.test_case "degraded history disabled" `Quick
      test_degraded_no_history_disabled;
    Alcotest.test_case "degraded history no binding" `Quick
      test_degraded_no_history_no_binding;
    Alcotest.test_case "degraded no persist" `Quick test_degraded_no_persist;
    Alcotest.test_case "degraded no github" `Quick test_degraded_no_github;
    Alcotest.test_case "degraded none when all good" `Quick
      test_degraded_none_when_all_good;
    Alcotest.test_case "card envelope" `Quick
      test_card_is_bot_framework_envelope;
    Alcotest.test_case "card has body" `Quick test_card_has_body_elements;
    Alcotest.test_case "text capabilities" `Quick
      test_text_contains_capabilities;
    Alcotest.test_case "text degraded" `Quick test_text_contains_degraded;
    Alcotest.test_case "delivery mode" `Quick test_delivery_mode_edit;
  ]

let string_contains s sub =
  try
    ignore (Str.search_forward (Str.regexp_string sub) s 0);
    true
  with Not_found -> false

(* Strategy selection tests *)

let test_strategy_telegram_single_select () =
  let caps = Some Connector_capabilities.telegram in
  let strategy =
    Question_presenter.select_strategy ~capabilities:caps
      ~has_rich_notifier:true
      (Tools_builtin.Single_select { options = [ "a"; "b"; "c" ] })
  in
  Alcotest.(check bool)
    "single_select with rich -> Rich_buttons" true
    (strategy = Question_presenter.Rich_buttons)

let test_strategy_telegram_single_select_many_options () =
  let caps = Some Connector_capabilities.telegram in
  let opts = List.init 10 (fun i -> Printf.sprintf "opt%d" i) in
  let strategy =
    Question_presenter.select_strategy ~capabilities:caps
      ~has_rich_notifier:true
      (Tools_builtin.Single_select { options = opts })
  in
  Alcotest.(check bool)
    "single_select >8 options -> Formatted_text" true
    (strategy = Question_presenter.Formatted_text)

let test_strategy_telegram_confirm () =
  let caps = Some Connector_capabilities.telegram in
  let strategy =
    Question_presenter.select_strategy ~capabilities:caps
      ~has_rich_notifier:true Tools_builtin.Confirm
  in
  Alcotest.(check bool)
    "confirm with rich -> Rich_buttons" true
    (strategy = Question_presenter.Rich_buttons)

let test_strategy_telegram_rating_small () =
  let caps = Some Connector_capabilities.telegram in
  let strategy =
    Question_presenter.select_strategy ~capabilities:caps
      ~has_rich_notifier:true
      (Tools_builtin.Rating { min = 1; max = 5 })
  in
  Alcotest.(check bool)
    "rating 1-5 with rich -> Rich_buttons" true
    (strategy = Question_presenter.Rich_buttons)

let test_strategy_telegram_rating_large () =
  let caps = Some Connector_capabilities.telegram in
  let strategy =
    Question_presenter.select_strategy ~capabilities:caps
      ~has_rich_notifier:true
      (Tools_builtin.Rating { min = 1; max = 10 })
  in
  Alcotest.(check bool)
    "rating 1-10 with rich -> Formatted_text" true
    (strategy = Question_presenter.Formatted_text)

let test_strategy_telegram_multi_select () =
  let caps = Some Connector_capabilities.telegram in
  let strategy =
    Question_presenter.select_strategy ~capabilities:caps
      ~has_rich_notifier:true
      (Tools_builtin.Multi_select { options = [ "a"; "b" ] })
  in
  Alcotest.(check bool)
    "multi_select with rich -> Rich_poll" true
    (strategy = Question_presenter.Rich_poll)

let test_strategy_telegram_text () =
  let caps = Some Connector_capabilities.telegram in
  let strategy =
    Question_presenter.select_strategy ~capabilities:caps
      ~has_rich_notifier:true
      (Tools_builtin.Text { placeholder = None })
  in
  Alcotest.(check bool)
    "text always -> Formatted_text" true
    (strategy = Question_presenter.Formatted_text)

let test_strategy_no_rich_notifier () =
  let caps = Some Connector_capabilities.telegram in
  let strategy =
    Question_presenter.select_strategy ~capabilities:caps
      ~has_rich_notifier:false
      (Tools_builtin.Single_select { options = [ "a"; "b" ] })
  in
  Alcotest.(check bool)
    "no rich_notifier -> Formatted_text" true
    (strategy = Question_presenter.Formatted_text)

let test_strategy_plain_connector () =
  let caps = Some Connector_capabilities.plain in
  let strategy =
    Question_presenter.select_strategy ~capabilities:caps
      ~has_rich_notifier:true Tools_builtin.Confirm
  in
  Alcotest.(check bool)
    "plain connector -> Formatted_text" true
    (strategy = Question_presenter.Formatted_text)

let test_strategy_no_capabilities () =
  let strategy =
    Question_presenter.select_strategy ~capabilities:None
      ~has_rich_notifier:true Tools_builtin.Confirm
  in
  Alcotest.(check bool)
    "no capabilities -> Formatted_text" true
    (strategy = Question_presenter.Formatted_text)

let test_strategy_teams_single_select () =
  let caps = Some Connector_capabilities.teams in
  let strategy =
    Question_presenter.select_strategy ~capabilities:caps
      ~has_rich_notifier:true
      (Tools_builtin.Single_select { options = [ "a"; "b" ] })
  in
  Alcotest.(check bool)
    "teams single_select with rich -> Rich_buttons" true
    (strategy = Question_presenter.Rich_buttons)

(* Rendering tests *)

let test_render_rich_buttons_single_select () =
  let qi : Tools_builtin.question_item =
    {
      question = "Pick one";
      qtype = Single_select { options = [ "Alpha"; "Beta"; "Gamma" ] };
    }
  in
  let rendered =
    Question_presenter.render_question ~strategy:Rich_buttons
      ~connector:Format_adapter.Telegram_html ~session_key:"telegram:123"
      ~index:0 ~total:1 qi
  in
  match rendered with
  | Question_presenter.RichMessage
      (Rich_message.TextWithButtons { text; button_rows }) ->
      Alcotest.(check bool)
        "text contains question" true
        (string_contains text "Pick one");
      let all_buttons = List.concat button_rows in
      Alcotest.(check int) "3 buttons" 3 (List.length all_buttons);
      let labels =
        List.map (fun (b : Rich_message.button) -> b.label) all_buttons
      in
      Alcotest.(check (list string))
        "button labels"
        [ "Alpha"; "Beta"; "Gamma" ]
        labels
  | _ -> Alcotest.fail "expected RichMessage TextWithButtons"

let test_render_rich_buttons_confirm () =
  let qi : Tools_builtin.question_item =
    { question = "Are you sure?"; qtype = Confirm }
  in
  let rendered =
    Question_presenter.render_question ~strategy:Rich_buttons
      ~connector:Format_adapter.Telegram_html ~session_key:"telegram:123"
      ~index:0 ~total:1 qi
  in
  match rendered with
  | Question_presenter.RichMessage
      (Rich_message.TextWithButtons { button_rows; _ }) ->
      let all_buttons = List.concat button_rows in
      Alcotest.(check int) "2 buttons" 2 (List.length all_buttons);
      let labels =
        List.map (fun (b : Rich_message.button) -> b.label) all_buttons
      in
      Alcotest.(check (list string)) "yes/no buttons" [ "Yes"; "No" ] labels
  | _ -> Alcotest.fail "expected RichMessage TextWithButtons"

let test_render_rich_buttons_rating () =
  let qi : Tools_builtin.question_item =
    { question = "Rate this"; qtype = Rating { min = 1; max = 3 } }
  in
  let rendered =
    Question_presenter.render_question ~strategy:Rich_buttons
      ~connector:Format_adapter.Telegram_html ~session_key:"telegram:123"
      ~index:0 ~total:1 qi
  in
  match rendered with
  | Question_presenter.RichMessage
      (Rich_message.TextWithButtons { button_rows; _ }) ->
      let all_buttons = List.concat button_rows in
      Alcotest.(check int) "3 buttons" 3 (List.length all_buttons);
      let labels =
        List.map (fun (b : Rich_message.button) -> b.label) all_buttons
      in
      Alcotest.(check (list string)) "1 2 3 buttons" [ "1"; "2"; "3" ] labels
  | _ -> Alcotest.fail "expected RichMessage TextWithButtons"

let test_render_rich_poll_multi_select () =
  let qi : Tools_builtin.question_item =
    {
      question = "Pick many";
      qtype = Multi_select { options = [ "X"; "Y"; "Z" ] };
    }
  in
  let rendered =
    Question_presenter.render_question ~strategy:Rich_poll
      ~connector:Format_adapter.Telegram_html ~session_key:"telegram:123"
      ~index:0 ~total:1 qi
  in
  match rendered with
  | Question_presenter.RichMessage
      (Rich_message.Poll { question; options; allows_multiple }) ->
      Alcotest.(check bool)
        "question contains text" true
        (string_contains question "Pick many");
      Alcotest.(check (list string)) "options" [ "X"; "Y"; "Z" ] options;
      Alcotest.(check bool) "allows multiple" true allows_multiple
  | _ -> Alcotest.fail "expected RichMessage Poll"

let test_render_formatted_text_telegram () =
  let qi : Tools_builtin.question_item =
    { question = "Pick one"; qtype = Single_select { options = [ "A"; "B" ] } }
  in
  let rendered =
    Question_presenter.render_question ~strategy:Formatted_text
      ~connector:Format_adapter.Telegram_html ~session_key:"telegram:123"
      ~index:0 ~total:1 qi
  in
  match rendered with
  | Question_presenter.TextMessage text ->
      Alcotest.(check bool)
        "has bold question" true
        (string_contains text "<b>");
      Alcotest.(check bool) "has option A" true (string_contains text "A");
      Alcotest.(check bool) "has option B" true (string_contains text "B");
      Alcotest.(check bool)
        "has instruction" true
        (string_contains text "Reply with number or text")
  | _ -> Alcotest.fail "expected TextMessage"

let test_render_formatted_text_discord () =
  let qi : Tools_builtin.question_item =
    { question = "Sure?"; qtype = Confirm }
  in
  let rendered =
    Question_presenter.render_question ~strategy:Formatted_text
      ~connector:Format_adapter.Discord ~session_key:"discord:123" ~index:0
      ~total:1 qi
  in
  match rendered with
  | Question_presenter.TextMessage text ->
      Alcotest.(check bool) "has markdown bold" true (string_contains text "**");
      Alcotest.(check bool)
        "has reply yes/no" true
        (string_contains text "Reply yes/no")
  | _ -> Alcotest.fail "expected TextMessage"

let test_render_formatted_text_multi_question () =
  let qi : Tools_builtin.question_item =
    { question = "First?"; qtype = Confirm }
  in
  let rendered =
    Question_presenter.render_question ~strategy:Formatted_text
      ~connector:Format_adapter.Plain ~session_key:"web:123" ~index:0 ~total:3
      qi
  in
  match rendered with
  | Question_presenter.TextMessage text ->
      Alcotest.(check bool)
        "has question numbering" true
        (string_contains text "[Question 1/3]")
  | _ -> Alcotest.fail "expected TextMessage"

let test_render_plain_text () =
  let qi : Tools_builtin.question_item =
    { question = "Pick one"; qtype = Single_select { options = [ "A"; "B" ] } }
  in
  let rendered =
    Question_presenter.render_question ~strategy:Plain_text
      ~connector:Format_adapter.Plain ~session_key:"web:123" ~index:0 ~total:1
      qi
  in
  match rendered with
  | Question_presenter.TextMessage text ->
      Alcotest.(check bool)
        "has question" true
        (string_contains text "Pick one");
      Alcotest.(check bool) "has option 1" true (string_contains text "1. A");
      Alcotest.(check bool) "has option 2" true (string_contains text "2. B");
      Alcotest.(check bool)
        "has instruction" true
        (string_contains text "(Reply with number or text)")
  | _ -> Alcotest.fail "expected TextMessage"

(* Callback ID tests *)

let test_callback_id_roundtrip () =
  let cb_id =
    Question_presenter.make_callback_id ~session_key:"telegram:123"
      ~question_index:2 ~option_index:5
  in
  Alcotest.(check bool)
    "starts with auq:" true
    (String.length cb_id > 4 && String.sub cb_id 0 4 = "auq:");
  match Question_presenter.parse_callback_id cb_id with
  | Some (qi, oi) ->
      Alcotest.(check int) "question_index" 2 qi;
      Alcotest.(check int) "option_index" 5 oi
  | None -> Alcotest.fail "parse_callback_id returned None"

let test_callback_id_invalid () =
  Alcotest.(check bool)
    "empty string" true
    (Question_presenter.parse_callback_id "" = None);
  Alcotest.(check bool)
    "wrong prefix" true
    (Question_presenter.parse_callback_id "foo:1:2:3" = None);
  Alcotest.(check bool)
    "not enough parts" true
    (Question_presenter.parse_callback_id "auq:abc:1" = None)

(* Extract callback answers tests *)

let test_extract_callback_answers_buttons () =
  let msg =
    Rich_message.TextWithButtons
      {
        text = "Pick one";
        button_rows =
          [
            [
              { label = "Alpha"; callback_id = "auq:abc:0:0" };
              { label = "Beta"; callback_id = "auq:abc:0:1" };
            ];
          ];
      }
  in
  let answers = Question_presenter.extract_callback_answers msg in
  Alcotest.(check int) "2 answers" 2 (List.length answers);
  let first_id, first_label = List.nth answers 0 in
  Alcotest.(check string) "first id" "auq:abc:0:0" first_id;
  Alcotest.(check string) "first label" "Alpha" first_label

let test_extract_callback_answers_text () =
  let msg = Rich_message.Text "hello" in
  let answers = Question_presenter.extract_callback_answers msg in
  Alcotest.(check int) "no answers for text" 0 (List.length answers)

(* Teams adaptive card tests *)

let test_teams_adaptive_card () =
  let card =
    Question_presenter.build_teams_card_from_buttons ~text:"Pick one"
      ~button_rows:
        [
          [
            { Rich_message.label = "Alpha"; callback_id = "cb_a" };
            { Rich_message.label = "Beta"; callback_id = "cb_b" };
          ];
        ]
  in
  let json_str = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "has AdaptiveCard" true
    (string_contains json_str "AdaptiveCard");
  Alcotest.(check bool)
    "has Action.Submit" true
    (string_contains json_str "Action.Submit");
  Alcotest.(check bool)
    "has clawq_question_answer" true
    (string_contains json_str "clawq_question_answer");
  Alcotest.(check bool) "has Alpha" true (string_contains json_str "Alpha");
  Alcotest.(check bool) "has Beta" true (string_contains json_str "Beta")

let test_teams_poll_card () =
  let card =
    Question_presenter.build_teams_poll_card ~question:"Pick many"
      ~options:[ "X"; "Y"; "Z" ]
  in
  let json_str = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "has AdaptiveCard" true
    (string_contains json_str "AdaptiveCard");
  Alcotest.(check bool)
    "has Input.ChoiceSet" true
    (string_contains json_str "Input.ChoiceSet");
  Alcotest.(check bool)
    "has isMultiSelect" true
    (string_contains json_str "isMultiSelect");
  Alcotest.(check bool) "has Submit" true (string_contains json_str "Submit")

let test_teams_card_from_buttons () =
  let card =
    Question_presenter.build_teams_card_from_buttons ~text:"Confirm?"
      ~button_rows:
        [
          [
            { Rich_message.label = "Yes"; callback_id = "cb1" };
            { Rich_message.label = "No"; callback_id = "cb2" };
          ];
        ]
  in
  let json_str = Yojson.Safe.to_string card in
  Alcotest.(check bool)
    "has AdaptiveCard" true
    (string_contains json_str "AdaptiveCard");
  Alcotest.(check bool) "has Yes" true (string_contains json_str "Yes");
  Alcotest.(check bool) "has No" true (string_contains json_str "No")

(* Button row chunking test *)

let test_button_rows_chunking () =
  let qi : Tools_builtin.question_item =
    {
      question = "Pick one";
      qtype =
        Single_select { options = [ "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H" ] };
    }
  in
  let rendered =
    Question_presenter.render_question ~strategy:Rich_buttons
      ~connector:Format_adapter.Telegram_html ~session_key:"telegram:123"
      ~index:0 ~total:1 qi
  in
  match rendered with
  | Question_presenter.RichMessage
      (Rich_message.TextWithButtons { button_rows; _ }) ->
      (* 8 buttons, max 3 per row = 3 rows (3+3+2) *)
      Alcotest.(check int) "3 rows" 3 (List.length button_rows);
      Alcotest.(check int)
        "first row has 3" 3
        (List.length (List.nth button_rows 0));
      Alcotest.(check int)
        "last row has 2" 2
        (List.length (List.nth button_rows 2))
  | _ -> Alcotest.fail "expected RichMessage TextWithButtons"

(* Question callback resolution tests *)

let test_question_callback_resolution () =
  let mgr = Session.create ~config:Runtime_config.default () in
  let key = "telegram:123" in
  let promise, _resolver = Session.register_pending_question mgr ~key in
  Session.register_question_callbacks mgr ~key
    ~callbacks:[ ("auq:abc:0:0", "Alpha"); ("auq:abc:0:1", "Beta") ];
  let resolved =
    Session.resolve_question_callback mgr ~key ~callback_id:"auq:abc:0:0"
  in
  Alcotest.(check bool) "resolved" true resolved;
  let result = Lwt_main.run promise in
  Alcotest.(check string) "answer is Alpha" "Alpha" result;
  Alcotest.(check bool)
    "no longer pending" false
    (Session.has_pending_question mgr ~key)

let test_question_callback_unknown () =
  let mgr = Session.create ~config:Runtime_config.default () in
  let key = "telegram:123" in
  let _promise, _resolver = Session.register_pending_question mgr ~key in
  let resolved =
    Session.resolve_question_callback mgr ~key ~callback_id:"unknown_id"
  in
  Alcotest.(check bool) "not resolved" false resolved;
  Alcotest.(check bool)
    "still pending" true
    (Session.has_pending_question mgr ~key);
  Session.cancel_pending_question mgr ~key

let test_question_callback_cleanup () =
  let mgr = Session.create ~config:Runtime_config.default () in
  let key = "telegram:123" in
  Session.register_question_callbacks mgr ~key
    ~callbacks:[ ("cb1", "A"); ("cb2", "B"); ("cb3", "C") ];
  Session.clear_question_callbacks mgr ~key
    ~callback_ids:[ "cb1"; "cb2"; "cb3" ];
  (* After cleanup, callbacks should not resolve *)
  let _promise, _resolver = Session.register_pending_question mgr ~key in
  let resolved =
    Session.resolve_question_callback mgr ~key ~callback_id:"cb1"
  in
  Alcotest.(check bool) "not resolved after cleanup" false resolved;
  Session.cancel_pending_question mgr ~key

let test_text_fallback_still_works () =
  let mgr = Session.create ~config:Runtime_config.default () in
  let key = "telegram:123" in
  let promise, _resolver = Session.register_pending_question mgr ~key in
  Session.register_question_callbacks mgr ~key ~callbacks:[ ("cb1", "Alpha") ];
  (* Simulate text reply via pending_questions resolver directly *)
  (match Hashtbl.find_opt mgr.pending_questions key with
  | Some r ->
      Hashtbl.remove mgr.pending_questions key;
      Lwt.wakeup_later r "typed answer"
  | None -> Alcotest.fail "resolver not found");
  let result = Lwt_main.run promise in
  Alcotest.(check string) "typed answer works" "typed answer" result;
  Session.clear_question_callbacks mgr ~key ~callback_ids:[ "cb1" ]

let suite =
  [
    Alcotest.test_case "strategy: telegram single_select" `Quick
      test_strategy_telegram_single_select;
    Alcotest.test_case "strategy: telegram single_select many options" `Quick
      test_strategy_telegram_single_select_many_options;
    Alcotest.test_case "strategy: telegram confirm" `Quick
      test_strategy_telegram_confirm;
    Alcotest.test_case "strategy: telegram rating small" `Quick
      test_strategy_telegram_rating_small;
    Alcotest.test_case "strategy: telegram rating large" `Quick
      test_strategy_telegram_rating_large;
    Alcotest.test_case "strategy: telegram multi_select" `Quick
      test_strategy_telegram_multi_select;
    Alcotest.test_case "strategy: telegram text" `Quick
      test_strategy_telegram_text;
    Alcotest.test_case "strategy: no rich_notifier" `Quick
      test_strategy_no_rich_notifier;
    Alcotest.test_case "strategy: plain connector" `Quick
      test_strategy_plain_connector;
    Alcotest.test_case "strategy: no capabilities" `Quick
      test_strategy_no_capabilities;
    Alcotest.test_case "strategy: teams single_select" `Quick
      test_strategy_teams_single_select;
    Alcotest.test_case "render: rich buttons single_select" `Quick
      test_render_rich_buttons_single_select;
    Alcotest.test_case "render: rich buttons confirm" `Quick
      test_render_rich_buttons_confirm;
    Alcotest.test_case "render: rich buttons rating" `Quick
      test_render_rich_buttons_rating;
    Alcotest.test_case "render: rich poll multi_select" `Quick
      test_render_rich_poll_multi_select;
    Alcotest.test_case "render: formatted text telegram" `Quick
      test_render_formatted_text_telegram;
    Alcotest.test_case "render: formatted text discord" `Quick
      test_render_formatted_text_discord;
    Alcotest.test_case "render: formatted text multi-question" `Quick
      test_render_formatted_text_multi_question;
    Alcotest.test_case "render: plain text" `Quick test_render_plain_text;
    Alcotest.test_case "callback ID roundtrip" `Quick test_callback_id_roundtrip;
    Alcotest.test_case "callback ID invalid" `Quick test_callback_id_invalid;
    Alcotest.test_case "extract callback answers buttons" `Quick
      test_extract_callback_answers_buttons;
    Alcotest.test_case "extract callback answers text" `Quick
      test_extract_callback_answers_text;
    Alcotest.test_case "Teams adaptive card" `Quick test_teams_adaptive_card;
    Alcotest.test_case "Teams poll card" `Quick test_teams_poll_card;
    Alcotest.test_case "Teams card from buttons" `Quick
      test_teams_card_from_buttons;
    Alcotest.test_case "button rows chunking" `Quick test_button_rows_chunking;
    Alcotest.test_case "question callback resolution" `Quick
      test_question_callback_resolution;
    Alcotest.test_case "question callback unknown" `Quick
      test_question_callback_unknown;
    Alcotest.test_case "question callback cleanup" `Quick
      test_question_callback_cleanup;
    Alcotest.test_case "text fallback still works" `Quick
      test_text_fallback_still_works;
  ]

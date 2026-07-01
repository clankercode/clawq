let test_telegram_profile () =
  let caps = Connector_capabilities.telegram in
  Alcotest.(check bool)
    "can edit" true
    (caps.can_edit = Connector_capabilities.Edit_in_place);
  Alcotest.(check bool) "can delete" true caps.can_delete;
  Alcotest.(check bool) "can type" true caps.can_type;
  Alcotest.(check string) "parse_mode" "MarkdownV2" caps.parse_mode;
  Alcotest.(check int) "max_message_length" 4096 caps.max_message_length

let test_discord_profile () =
  let caps = Connector_capabilities.discord in
  Alcotest.(check bool)
    "can edit" true
    (caps.can_edit = Connector_capabilities.Edit_in_place);
  Alcotest.(check bool) "can react" true caps.can_react;
  Alcotest.(check string) "parse_mode" "Markdown" caps.parse_mode;
  Alcotest.(check int) "max_message_length" 2000 caps.max_message_length

let test_slack_profile () =
  let caps = Connector_capabilities.slack in
  Alcotest.(check bool)
    "can edit" true
    (caps.can_edit = Connector_capabilities.Edit_in_place);
  Alcotest.(check bool) "can react" true caps.can_react;
  Alcotest.(check string) "parse_mode" "mrkdwn" caps.parse_mode

let test_teams_profile () =
  let caps = Connector_capabilities.teams in
  Alcotest.(check bool)
    "can edit" true
    (caps.can_edit = Connector_capabilities.Edit_in_place);
  Alcotest.(check bool) "can type" true caps.can_type;
  Alcotest.(check string) "parse_mode" "Markdown" caps.parse_mode;
  Alcotest.(check int) "max_message_length" 28672 caps.max_message_length

let test_matrix_profile () =
  let caps = Connector_capabilities.matrix in
  Alcotest.(check bool)
    "can edit" true
    (caps.can_edit = Connector_capabilities.Edit_in_place);
  Alcotest.(check bool) "can delete" true caps.can_delete;
  Alcotest.(check string) "parse_mode" "Markdown" caps.parse_mode

let test_plain_profile () =
  let caps = Connector_capabilities.plain in
  Alcotest.(check bool)
    "no edit" true
    (caps.can_edit = Connector_capabilities.No_edit);
  Alcotest.(check bool) "no delete" false caps.can_delete;
  Alcotest.(check bool) "no react" false caps.can_react

let test_irc_profile () =
  let caps = Connector_capabilities.irc in
  Alcotest.(check bool)
    "no edit" true
    (caps.can_edit = Connector_capabilities.No_edit);
  Alcotest.(check bool) "no delete" false caps.can_delete;
  Alcotest.(check int) "max_message_length" 512 caps.max_message_length

let test_mattermost_profile () =
  let caps = Connector_capabilities.mattermost in
  Alcotest.(check bool)
    "can edit" true
    (caps.can_edit = Connector_capabilities.Edit_in_place);
  Alcotest.(check bool) "can delete" true caps.can_delete;
  Alcotest.(check bool) "can react" true caps.can_react;
  Alcotest.(check int) "max_message_length" 16383 caps.max_message_length;
  Alcotest.(check string) "parse_mode" "Markdown" caps.parse_mode

let test_lark_profile () =
  let caps = Connector_capabilities.lark in
  Alcotest.(check bool)
    "no edit" true
    (caps.can_edit = Connector_capabilities.No_edit);
  Alcotest.(check int) "max_message_length" 4096 caps.max_message_length

let test_line_profile () =
  let caps = Connector_capabilities.line in
  Alcotest.(check bool)
    "no edit" true
    (caps.can_edit = Connector_capabilities.No_edit);
  Alcotest.(check int) "max_message_length" 5000 caps.max_message_length

let test_dingtalk_profile () =
  let caps = Connector_capabilities.dingtalk in
  Alcotest.(check bool)
    "no edit" true
    (caps.can_edit = Connector_capabilities.No_edit);
  Alcotest.(check int) "max_message_length" 20000 caps.max_message_length

let test_onebot_profile () =
  let caps = Connector_capabilities.onebot in
  Alcotest.(check bool)
    "no edit" true
    (caps.can_edit = Connector_capabilities.No_edit);
  Alcotest.(check bool) "can delete" true caps.can_delete;
  Alcotest.(check int) "max_message_length" 4500 caps.max_message_length

let test_nostr_profile () =
  let caps = Connector_capabilities.nostr in
  Alcotest.(check bool)
    "no edit" true
    (caps.can_edit = Connector_capabilities.No_edit);
  Alcotest.(check int) "max_message_length" 8000 caps.max_message_length

let test_imessage_profile () =
  let caps = Connector_capabilities.imessage in
  Alcotest.(check bool)
    "no edit" true
    (caps.can_edit = Connector_capabilities.No_edit);
  Alcotest.(check int) "max_message_length" 4096 caps.max_message_length

let test_email_profile () =
  let caps = Connector_capabilities.email in
  Alcotest.(check bool)
    "no edit" true
    (caps.can_edit = Connector_capabilities.No_edit);
  Alcotest.(check int) "max_message_length" 65536 caps.max_message_length

let test_github_profile () =
  let caps = Connector_capabilities.github in
  Alcotest.(check bool)
    "can edit" true
    (caps.can_edit = Connector_capabilities.Edit_in_place);
  Alcotest.(check bool) "can delete" true caps.can_delete;
  Alcotest.(check bool) "can react" true caps.can_react;
  Alcotest.(check int) "max_message_length" 65536 caps.max_message_length;
  Alcotest.(check string) "parse_mode" "Markdown" caps.parse_mode

let test_signal_profile () =
  let caps = Connector_capabilities.signal in
  Alcotest.(check bool)
    "no edit" true
    (caps.can_edit = Connector_capabilities.No_edit);
  Alcotest.(check bool) "can delete" true caps.can_delete;
  Alcotest.(check bool) "can react" true caps.can_react;
  Alcotest.(check int) "max_message_length" 6000 caps.max_message_length

let test_whatsapp_profile () =
  let caps = Connector_capabilities.whatsapp in
  Alcotest.(check bool)
    "no edit" true
    (caps.can_edit = Connector_capabilities.No_edit);
  Alcotest.(check bool) "can react" true caps.can_react;
  Alcotest.(check int) "max_message_length" 4096 caps.max_message_length

let test_web_channel_profile () =
  let caps = Connector_capabilities.web_channel in
  Alcotest.(check bool)
    "no edit" true
    (caps.can_edit = Connector_capabilities.No_edit);
  Alcotest.(check int) "max_message_length" 65536 caps.max_message_length

let test_supports_rich_questions () =
  Alcotest.(check bool)
    "telegram supports rich" true
    (Connector_capabilities.supports_rich_questions
       Connector_capabilities.telegram);
  Alcotest.(check bool)
    "teams supports rich" true
    (Connector_capabilities.supports_rich_questions Connector_capabilities.teams);
  Alcotest.(check bool)
    "discord not rich" false
    (Connector_capabilities.supports_rich_questions
       Connector_capabilities.discord);
  Alcotest.(check bool)
    "slack not rich" false
    (Connector_capabilities.supports_rich_questions Connector_capabilities.slack);
  Alcotest.(check bool)
    "plain not rich" false
    (Connector_capabilities.supports_rich_questions Connector_capabilities.plain);
  Alcotest.(check bool)
    "irc not rich" false
    (Connector_capabilities.supports_rich_questions Connector_capabilities.irc)

let test_thread_reply_strategies () =
  Alcotest.(check bool)
    "slack uses native threads" true
    (Connector_capabilities.thread_reply_strategy Connector_capabilities.slack
    = Connector_capabilities.Use_native_thread);
  Alcotest.(check bool)
    "teams uses thread-like replies" true
    (Connector_capabilities.thread_reply_strategy Connector_capabilities.teams
    = Connector_capabilities.Use_thread_like_reply);
  Alcotest.(check bool)
    "thread-less plain falls back" true
    (Connector_capabilities.thread_reply_strategy Connector_capabilities.plain
    = Connector_capabilities.Use_room_fallback);
  Alcotest.(check bool)
    "thread-less irc falls back" true
    (Connector_capabilities.thread_reply_strategy Connector_capabilities.irc
    = Connector_capabilities.Use_room_fallback)

let test_progress_delivery_strategies () =
  let resend_caps =
    {
      Connector_capabilities.plain with
      can_edit = Connector_capabilities.Delete_and_resend;
    }
  in
  Alcotest.(check bool)
    "editable connector edits progress" true
    (Connector_capabilities.progress_delivery Connector_capabilities.slack
    = Connector_capabilities.Edit_progress_in_place);
  Alcotest.(check bool)
    "delete/resend connector resends progress" true
    (Connector_capabilities.progress_delivery resend_caps
    = Connector_capabilities.Delete_and_resend_progress);
  Alcotest.(check bool)
    "non-editable connector buffers progress" true
    (Connector_capabilities.progress_delivery Connector_capabilities.plain
    = Connector_capabilities.Buffered_progress)

let test_card_button_strategies () =
  Alcotest.(check bool)
    "teams uses cards" true
    (Connector_capabilities.card_strategy Connector_capabilities.teams
    = Connector_capabilities.Use_cards);
  Alcotest.(check bool)
    "telegram uses buttons" true
    (Connector_capabilities.card_strategy Connector_capabilities.telegram
    = Connector_capabilities.Use_buttons);
  Alcotest.(check bool)
    "discord cards fall back to text" true
    (Connector_capabilities.card_strategy Connector_capabilities.discord
    = Connector_capabilities.Use_text_fallback);
  Alcotest.(check bool)
    "plain cards fall back to text" true
    (Connector_capabilities.card_strategy Connector_capabilities.plain
    = Connector_capabilities.Use_text_fallback)

let test_history_capture_strategies () =
  Alcotest.(check bool)
    "slack captures scoped room history" true
    (Connector_capabilities.history_capture_strategy
       Connector_capabilities.slack
    = Connector_capabilities.Capture_ambient_history);
  Alcotest.(check bool)
    "teams captures ambient history" true
    (Connector_capabilities.history_capture_strategy
       Connector_capabilities.teams
    = Connector_capabilities.Capture_ambient_history);
  Alcotest.(check bool)
    "discord captures ambient history" true
    (Connector_capabilities.history_capture_strategy
       Connector_capabilities.discord
    = Connector_capabilities.Capture_ambient_history);
  Alcotest.(check bool)
    "plain history capture is skipped" true
    (Connector_capabilities.history_capture_strategy
       Connector_capabilities.plain
    = Connector_capabilities.Skip_history_capture)

(** {1 of_name lookup tests} *)

let test_of_name_known () =
  let check name expected =
    match Connector_capabilities.of_name name with
    | Some caps ->
        Alcotest.(check string)
          (name ^ " parse_mode") expected.Connector_capabilities.parse_mode
          caps.parse_mode
    | None -> Alcotest.fail (name ^ " should be recognised")
  in
  check "telegram" Connector_capabilities.telegram;
  check "discord" Connector_capabilities.discord;
  check "slack" Connector_capabilities.slack;
  check "teams" Connector_capabilities.teams;
  check "matrix" Connector_capabilities.matrix;
  check "irc" Connector_capabilities.irc;
  check "mattermost" Connector_capabilities.mattermost;
  check "github" Connector_capabilities.github;
  check "signal" Connector_capabilities.signal;
  check "web" Connector_capabilities.web_channel

let test_of_name_unknown () =
  Alcotest.(check bool)
    "unknown returns None" true
    (Connector_capabilities.of_name "nonexistent" = None);
  Alcotest.(check bool)
    "empty returns None" true
    (Connector_capabilities.of_name "" = None);
  Alcotest.(check bool)
    "case sensitive" true
    (Connector_capabilities.of_name "Telegram" = None)

(** {1 Capability fallback tests for unsupported paths} *)

let test_card_strategy_fallback_all_non_card () =
  (* Every connector that cannot send cards or buttons must fall back to
     text. This is the core invariant for non-card connector fallback. *)
  let non_card_names =
    [
      "discord";
      "slack";
      "matrix";
      "irc";
      "mattermost";
      "lark";
      "line";
      "dingtalk";
      "onebot";
      "nostr";
      "imessage";
      "email";
      "github";
      "signal";
      "whatsapp";
      "web";
    ]
  in
  List.iter
    (fun name ->
      match Connector_capabilities.of_name name with
      | Some caps ->
          let strategy = Connector_capabilities.card_strategy caps in
          Alcotest.(check bool)
            (name ^ " uses text fallback when no cards/buttons")
            true
            (strategy = Connector_capabilities.Use_text_fallback)
      | None -> Alcotest.fail (name ^ " should be recognised"))
    non_card_names

let test_card_strategy_cards_for_card_capable () =
  (* Connectors with can_send_cards must use cards, not text fallback. *)
  Alcotest.(check bool)
    "teams uses cards" true
    (Connector_capabilities.card_strategy Connector_capabilities.teams
    = Connector_capabilities.Use_cards)

let test_card_strategy_buttons_for_button_only () =
  (* Connectors with can_send_buttons but not can_send_cards use buttons. *)
  Alcotest.(check bool)
    "telegram uses buttons" true
    (Connector_capabilities.card_strategy Connector_capabilities.telegram
    = Connector_capabilities.Use_buttons)

let tests =
  [
    Alcotest.test_case "telegram profile" `Quick test_telegram_profile;
    Alcotest.test_case "discord profile" `Quick test_discord_profile;
    Alcotest.test_case "slack profile" `Quick test_slack_profile;
    Alcotest.test_case "teams profile" `Quick test_teams_profile;
    Alcotest.test_case "matrix profile" `Quick test_matrix_profile;
    Alcotest.test_case "plain profile" `Quick test_plain_profile;
    Alcotest.test_case "irc profile" `Quick test_irc_profile;
    Alcotest.test_case "mattermost profile" `Quick test_mattermost_profile;
    Alcotest.test_case "lark profile" `Quick test_lark_profile;
    Alcotest.test_case "line profile" `Quick test_line_profile;
    Alcotest.test_case "dingtalk profile" `Quick test_dingtalk_profile;
    Alcotest.test_case "onebot profile" `Quick test_onebot_profile;
    Alcotest.test_case "nostr profile" `Quick test_nostr_profile;
    Alcotest.test_case "imessage profile" `Quick test_imessage_profile;
    Alcotest.test_case "email profile" `Quick test_email_profile;
    Alcotest.test_case "github profile" `Quick test_github_profile;
    Alcotest.test_case "signal profile" `Quick test_signal_profile;
    Alcotest.test_case "whatsapp profile" `Quick test_whatsapp_profile;
    Alcotest.test_case "web_channel profile" `Quick test_web_channel_profile;
    Alcotest.test_case "supports_rich_questions" `Quick
      test_supports_rich_questions;
    Alcotest.test_case "thread reply strategies" `Quick
      test_thread_reply_strategies;
    Alcotest.test_case "progress delivery strategies" `Quick
      test_progress_delivery_strategies;
    Alcotest.test_case "card button strategies" `Quick
      test_card_button_strategies;
    Alcotest.test_case "history capture strategies" `Quick
      test_history_capture_strategies;
    Alcotest.test_case "of_name known connectors" `Quick test_of_name_known;
    Alcotest.test_case "of_name unknown connectors" `Quick test_of_name_unknown;
    Alcotest.test_case "card strategy fallback all non-card" `Quick
      test_card_strategy_fallback_all_non_card;
    Alcotest.test_case "card strategy cards for card-capable" `Quick
      test_card_strategy_cards_for_card_capable;
    Alcotest.test_case "card strategy buttons for button-only" `Quick
      test_card_strategy_buttons_for_button_only;
  ]

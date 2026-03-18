let test_telegram_profile () =
  let caps = Connector_capabilities.telegram in
  Alcotest.(check bool)
    "can edit" true
    (caps.can_edit = Connector_capabilities.Edit_in_place);
  Alcotest.(check bool) "can delete" true caps.can_delete;
  Alcotest.(check bool) "can type" true caps.can_type;
  Alcotest.(check string) "parse_mode" "HTML" caps.parse_mode;
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
  ]

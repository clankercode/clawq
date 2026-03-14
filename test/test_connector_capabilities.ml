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

let tests =
  [
    Alcotest.test_case "telegram profile" `Quick test_telegram_profile;
    Alcotest.test_case "discord profile" `Quick test_discord_profile;
    Alcotest.test_case "slack profile" `Quick test_slack_profile;
    Alcotest.test_case "teams profile" `Quick test_teams_profile;
    Alcotest.test_case "matrix profile" `Quick test_matrix_profile;
    Alcotest.test_case "plain profile" `Quick test_plain_profile;
  ]

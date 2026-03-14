let test_bold () =
  Alcotest.(check string)
    "telegram" "*hello*"
    (Format_adapter.bold Telegram_markdown "hello");
  Alcotest.(check string)
    "discord" "**hello**"
    (Format_adapter.bold Discord "hello");
  Alcotest.(check string) "slack" "*hello*" (Format_adapter.bold Slack "hello");
  Alcotest.(check string) "plain" "hello" (Format_adapter.bold Plain "hello")

let test_italic () =
  Alcotest.(check string)
    "telegram" "_hello_"
    (Format_adapter.italic Telegram_markdown "hello");
  Alcotest.(check string)
    "discord" "*hello*"
    (Format_adapter.italic Discord "hello")

let test_code () =
  Alcotest.(check string)
    "all use backticks" "`code`"
    (Format_adapter.code Discord "code")

let test_link () =
  Alcotest.(check string)
    "slack link" "<https://example.com|click>"
    (Format_adapter.link Slack ~text:"click" ~url:"https://example.com");
  Alcotest.(check string)
    "discord link" "[click](https://example.com)"
    (Format_adapter.link Discord ~text:"click" ~url:"https://example.com")

let test_telegram_html_bold () =
  Alcotest.(check string)
    "html bold" "<b>hello</b>"
    (Format_adapter.bold Telegram_html "hello")

let test_telegram_html_italic () =
  Alcotest.(check string)
    "html italic" "<i>hello</i>"
    (Format_adapter.italic Telegram_html "hello")

let test_telegram_html_code () =
  Alcotest.(check string)
    "html code" "<code>hello</code>"
    (Format_adapter.code Telegram_html "hello")

let test_escape_telegram_html () =
  Alcotest.(check string)
    "escapes html entities" "&lt;script&gt;"
    (Format_adapter.escape Telegram_html "<script>");
  Alcotest.(check string)
    "escapes ampersand" "&amp;"
    (Format_adapter.escape Telegram_html "&")

let test_escape_identity () =
  Alcotest.(check string)
    "discord no-op" "<script>"
    (Format_adapter.escape Discord "<script>");
  Alcotest.(check string)
    "plain no-op" "<script>"
    (Format_adapter.escape Plain "<script>")

let test_of_parse_mode () =
  Alcotest.(check bool)
    "HTML -> Telegram_html" true
    (Format_adapter.of_parse_mode "HTML" = Telegram_html);
  Alcotest.(check bool)
    "Markdown -> Discord" true
    (Format_adapter.of_parse_mode "Markdown" = Discord);
  Alcotest.(check bool)
    "mrkdwn -> Slack" true
    (Format_adapter.of_parse_mode "mrkdwn" = Slack);
  Alcotest.(check bool)
    "unknown -> Plain" true
    (Format_adapter.of_parse_mode "unknown" = Plain)

let test_parse_mode_string_round_trip () =
  Alcotest.(check string)
    "Telegram_html" "HTML"
    (Format_adapter.parse_mode_string Telegram_html);
  Alcotest.(check string)
    "Discord" "Markdown"
    (Format_adapter.parse_mode_string Discord);
  Alcotest.(check string)
    "Slack" "mrkdwn"
    (Format_adapter.parse_mode_string Slack)

let test_escape_table_cell_teams () =
  Alcotest.(check string)
    "Teams escapes pipe characters" "a\\|b"
    (Format_adapter.escape_table_cell Teams "a|b")

let suite =
  [
    Alcotest.test_case "bold formatting" `Quick test_bold;
    Alcotest.test_case "italic formatting" `Quick test_italic;
    Alcotest.test_case "code formatting" `Quick test_code;
    Alcotest.test_case "link formatting" `Quick test_link;
    Alcotest.test_case "telegram html bold" `Quick test_telegram_html_bold;
    Alcotest.test_case "telegram html italic" `Quick test_telegram_html_italic;
    Alcotest.test_case "telegram html code" `Quick test_telegram_html_code;
    Alcotest.test_case "escape telegram html" `Quick test_escape_telegram_html;
    Alcotest.test_case "escape identity for non-html" `Quick
      test_escape_identity;
    Alcotest.test_case "of_parse_mode mappings" `Quick test_of_parse_mode;
    Alcotest.test_case "parse_mode_string round trip" `Quick
      test_parse_mode_string_round_trip;
    Alcotest.test_case "teams table cell escaping" `Quick
      test_escape_table_cell_teams;
  ]

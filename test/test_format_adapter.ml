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

let suite =
  [
    Alcotest.test_case "bold formatting" `Quick test_bold;
    Alcotest.test_case "italic formatting" `Quick test_italic;
    Alcotest.test_case "code formatting" `Quick test_code;
    Alcotest.test_case "link formatting" `Quick test_link;
  ]

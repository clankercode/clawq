let test_sanitize_url_empty () =
  Alcotest.(check string) "empty" "" (Url_sanitize.sanitize_url "")

let test_sanitize_url_no_params () =
  let url = "https://example.com/page" in
  Alcotest.(check string) "no params" url (Url_sanitize.sanitize_url url)

let test_sanitize_url_safe_params () =
  let url = "https://example.com/page?name=foo&count=42" in
  Alcotest.(check string) "safe params" url (Url_sanitize.sanitize_url url)

let test_sanitize_url_token_param () =
  let url = "https://example.com/page?token=abc123secret&name=foo" in
  let sanitized = Url_sanitize.sanitize_url url in
  Alcotest.(check bool)
    "token masked" true
    (Test_helpers.string_contains sanitized "abc1***");
  Alcotest.(check bool)
    "name preserved" true
    (Test_helpers.string_contains sanitized "name=foo");
  Alcotest.(check bool)
    "secret not exposed" false
    (Test_helpers.string_contains sanitized "abc123secret")

let test_sanitize_url_api_key_param () =
  let url = "https://example.com/api?key=sk_live_abc123def456" in
  let sanitized = Url_sanitize.sanitize_url url in
  Alcotest.(check bool)
    "key masked" true
    (Test_helpers.string_contains sanitized "sk_l***");
  Alcotest.(check bool)
    "full key not exposed" false
    (Test_helpers.string_contains sanitized "sk_live_abc123def456")

let test_sanitize_url_password_param () =
  let url = "https://example.com/login?password=supersecret123&user=bob" in
  let sanitized = Url_sanitize.sanitize_url url in
  Alcotest.(check bool)
    "password masked" true
    (Test_helpers.string_contains sanitized "supe***");
  Alcotest.(check bool)
    "user preserved" true
    (Test_helpers.string_contains sanitized "user=bob")

let test_sanitize_url_multiple_sensitive () =
  let url =
    "https://example.com/api?token=abc&secret=xyz&safe=keep"
  in
  let sanitized = Url_sanitize.sanitize_url url in
  Alcotest.(check bool)
    "safe preserved" true
    (Test_helpers.string_contains sanitized "safe=keep");
  Alcotest.(check bool)
    "full token not exposed" false
    (Test_helpers.string_contains sanitized "token=abc")

let test_sanitize_url_suffix_patterns () =
  let url = "https://example.com/api?my_token=val1&api_secret=val2" in
  let sanitized = Url_sanitize.sanitize_url url in
  Alcotest.(check bool)
    "suffix token masked" true
    (Test_helpers.string_contains sanitized "my_token=");
  Alcotest.(check bool)
    "suffix secret masked" true
    (Test_helpers.string_contains sanitized "api_secret=")

let test_sanitize_url_userinfo () =
  let url = "https://user:password123@host.com/path" in
  let sanitized = Url_sanitize.sanitize_url url in
  Alcotest.(check bool)
    "password masked" true
    (Test_helpers.string_contains sanitized "***");
  Alcotest.(check bool)
    "full password not exposed" false
    (Test_helpers.string_contains sanitized "password123")

let test_safe_teams_link () =
  let link = Url_sanitize.safe_teams_link "https://example.com/tr" "transcript"
  in
  Alcotest.(check bool)
    "has label" true
    (Test_helpers.string_contains link "transcript");
  Alcotest.(check bool)
    "has url" true
    (Test_helpers.string_contains link "https://example.com/tr")

let test_safe_slack_link () =
  let link = Url_sanitize.safe_slack_link "https://example.com/tr" "transcript"
  in
  Alcotest.(check bool)
    "has label" true
    (Test_helpers.string_contains link "transcript");
  Alcotest.(check bool)
    "has url" true
    (Test_helpers.string_contains link "https://example.com/tr");
  Alcotest.(check bool)
    "slack format" true
    (Test_helpers.string_contains link "<")

let test_safe_teams_link_sanitizes () =
  let link =
    Url_sanitize.safe_teams_link "https://example.com/page?token=secret123"
      "link"
  in
  Alcotest.(check bool)
    "token masked in link" false
    (Test_helpers.string_contains link "secret123")

let test_safe_slack_link_sanitizes () =
  let link =
    Url_sanitize.safe_slack_link "https://example.com/page?api_key=key123"
      "link"
  in
  Alcotest.(check bool)
    "key masked in link" false
    (Test_helpers.string_contains link "key123")

let suite =
  [
    Alcotest.test_case "sanitize url empty" `Quick test_sanitize_url_empty;
    Alcotest.test_case "sanitize url no params" `Quick test_sanitize_url_no_params;
    Alcotest.test_case "sanitize url safe params" `Quick
      test_sanitize_url_safe_params;
    Alcotest.test_case "sanitize url token param" `Quick
      test_sanitize_url_token_param;
    Alcotest.test_case "sanitize url api key param" `Quick
      test_sanitize_url_api_key_param;
    Alcotest.test_case "sanitize url password param" `Quick
      test_sanitize_url_password_param;
    Alcotest.test_case "sanitize url multiple sensitive" `Quick
      test_sanitize_url_multiple_sensitive;
    Alcotest.test_case "sanitize url suffix patterns" `Quick
      test_sanitize_url_suffix_patterns;
    Alcotest.test_case "sanitize url userinfo" `Quick test_sanitize_url_userinfo;
    Alcotest.test_case "safe teams link" `Quick test_safe_teams_link;
    Alcotest.test_case "safe slack link" `Quick test_safe_slack_link;
    Alcotest.test_case "safe teams link sanitizes" `Quick
      test_safe_teams_link_sanitizes;
    Alcotest.test_case "safe slack link sanitizes" `Quick
      test_safe_slack_link_sanitizes;
  ]

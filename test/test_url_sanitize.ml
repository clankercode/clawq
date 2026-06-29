let test_sanitize_url_empty () =
  Alcotest.(check string) "empty" "" (Url_sanitize.sanitize_url "")

let test_sanitize_url_no_params () =
  let url = "https://example.com/page" in
  Alcotest.(check string) "no params" url (Url_sanitize.sanitize_url url)

let test_sanitize_url_safe_params () =
  let url = "https://example.com/page?name=foo&count=42" in
  let sanitized = Url_sanitize.sanitize_url url in
  Alcotest.(check bool)
    "name preserved" true
    (Test_helpers.string_contains sanitized "name=foo");
  Alcotest.(check bool)
    "count preserved" true
    (Test_helpers.string_contains sanitized "count=42")

let test_sanitize_url_token_param () =
  let url = "https://example.com/page?token=abc123secret&name=foo" in
  let sanitized = Url_sanitize.sanitize_url url in
  Alcotest.(check bool)
    "name preserved" true
    (Test_helpers.string_contains sanitized "name=foo");
  Alcotest.(check bool)
    "secret not exposed" false
    (Test_helpers.string_contains sanitized "abc123secret");
  Alcotest.(check bool)
    "token value replaced" true
    (Test_helpers.string_contains sanitized "REDACTED")

let test_sanitize_url_api_key_param () =
  let url = "https://example.com/api?key=sk_live_abc123def456" in
  let sanitized = Url_sanitize.sanitize_url url in
  Alcotest.(check bool)
    "full key not exposed" false
    (Test_helpers.string_contains sanitized "sk_live_abc123def456");
  Alcotest.(check bool)
    "key value replaced" true
    (Test_helpers.string_contains sanitized "REDACTED")

let test_sanitize_url_password_param () =
  let url = "https://example.com/login?password=supersecret123&user=bob" in
  let sanitized = Url_sanitize.sanitize_url url in
  Alcotest.(check bool)
    "password not exposed" false
    (Test_helpers.string_contains sanitized "supersecret123");
  Alcotest.(check bool)
    "user preserved" true
    (Test_helpers.string_contains sanitized "user=bob");
  Alcotest.(check bool)
    "password value replaced" true
    (Test_helpers.string_contains sanitized "REDACTED")

let test_sanitize_url_multiple_sensitive () =
  let url = "https://example.com/api?token=abc&secret=xyz&safe=keep" in
  let sanitized = Url_sanitize.sanitize_url url in
  Alcotest.(check bool)
    "safe preserved" true
    (Test_helpers.string_contains sanitized "safe=keep");
  Alcotest.(check bool)
    "full token not exposed" false
    (Test_helpers.string_contains sanitized "token=abc&")

let test_sanitize_url_suffix_patterns () =
  let url = "https://example.com/api?my_token=val1&api_secret=val2" in
  let sanitized = Url_sanitize.sanitize_url url in
  Alcotest.(check bool)
    "val1 not exposed" false
    (Test_helpers.string_contains sanitized "val1");
  Alcotest.(check bool)
    "val2 not exposed" false
    (Test_helpers.string_contains sanitized "val2")

let test_sanitize_url_userinfo () =
  let url = "https://user:password123@host.com/path" in
  let sanitized = Url_sanitize.sanitize_url url in
  Alcotest.(check bool)
    "password masked" true
    (Test_helpers.string_contains sanitized "REDACTED");
  Alcotest.(check bool)
    "full password not exposed" false
    (Test_helpers.string_contains sanitized "password123");
  Alcotest.(check bool)
    "user preserved" true
    (Test_helpers.string_contains sanitized "user")

let test_sanitize_url_no_partial_exposure () =
  (* Verify that no part of the secret is exposed *)
  let url = "https://example.com/page?token=abcdefghij" in
  let sanitized = Url_sanitize.sanitize_url url in
  Alcotest.(check bool)
    "no partial secret" false
    (Test_helpers.string_contains sanitized "abcdefgh")

let test_safe_teams_link () =
  let link =
    Url_sanitize.safe_teams_link "https://example.com/tr" "transcript"
  in
  Alcotest.(check bool)
    "has label" true
    (Test_helpers.string_contains link "transcript");
  Alcotest.(check bool)
    "has url" true
    (Test_helpers.string_contains link "https://example.com/tr")

let test_safe_slack_link () =
  let link =
    Url_sanitize.safe_slack_link "https://example.com/tr" "transcript"
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
    Alcotest.test_case "sanitize url no params" `Quick
      test_sanitize_url_no_params;
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
    Alcotest.test_case "no partial secret exposure" `Quick
      test_sanitize_url_no_partial_exposure;
    Alcotest.test_case "safe teams link" `Quick test_safe_teams_link;
    Alcotest.test_case "safe slack link" `Quick test_safe_slack_link;
    Alcotest.test_case "safe teams link sanitizes" `Quick
      test_safe_teams_link_sanitizes;
    Alcotest.test_case "safe slack link sanitizes" `Quick
      test_safe_slack_link_sanitizes;
  ]

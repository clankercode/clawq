let test_loopback_host_detection () =
  List.iter
    (fun host ->
      Alcotest.(check bool)
        ("loopback " ^ host) true
        (Daemon_logging.is_loopback_host host))
    [ "127.0.0.1"; " localhost "; "::1" ];
  List.iter
    (fun host ->
      Alcotest.(check bool)
        ("non-loopback " ^ host) false
        (Daemon_logging.is_loopback_host host))
    [ "0.0.0.0"; "192.168.1.10"; "" ]

let test_detects_cohttp_request_lines () =
  Alcotest.(check bool)
    "GET request line" true
    (Daemon_logging.starts_with_http_method "GET /botTOKEN/sendMessage");
  Alcotest.(check bool)
    "PATCH request line" true
    (Daemon_logging.starts_with_http_method "PATCH /v1/messages");
  Alcotest.(check bool)
    "normal log line" false
    (Daemon_logging.starts_with_http_method "POSTED /not-an-http-method")

let test_scrubs_telegram_tokens () =
  Alcotest.(check string)
    "single token" "POST /bot<REDACTED>/sendChatAction HTTP/1.1"
    (Daemon_logging.scrub_telegram_tokens
       "POST /bot123456:ABC-DEF/sendChatAction HTTP/1.1");
  Alcotest.(check string)
    "multiple tokens" "/bot<REDACTED>/a /bot<REDACTED>/b"
    (Daemon_logging.scrub_telegram_tokens "/botone/a /bottwo/b");
  Alcotest.(check string)
    "unterminated path is unchanged" "GET /bot12345"
    (Daemon_logging.scrub_telegram_tokens "GET /bot12345")

let suite =
  [
    Alcotest.test_case "detects loopback hosts" `Quick
      test_loopback_host_detection;
    Alcotest.test_case "detects cohttp request lines" `Quick
      test_detects_cohttp_request_lines;
    Alcotest.test_case "scrubs telegram tokens" `Quick
      test_scrubs_telegram_tokens;
  ]

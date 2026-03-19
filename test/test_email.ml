(* Tests for Email channel module *)

(* --- decode_rfc2047_word tests --- *)

let test_decode_plain_text () =
  Alcotest.(check string)
    "plain text" "hello"
    (Email_channel.decode_rfc2047_word "hello")

let test_decode_base64 () =
  let encoded = "=?UTF-8?B?SGVsbG8gV29ybGQ=?=" in
  let result = Email_channel.decode_rfc2047_word encoded in
  Alcotest.(check string) "base64 decoded" "Hello World" result

let test_decode_qp () =
  let encoded = "=?UTF-8?Q?Hello_World?=" in
  let result = Email_channel.decode_rfc2047_word encoded in
  Alcotest.(check string)
    "QP decoded with underscore to space" "Hello World" result

let test_decode_qp_hex () =
  let encoded = "=?UTF-8?Q?Hello=20World?=" in
  let result = Email_channel.decode_rfc2047_word encoded in
  Alcotest.(check string) "QP hex space" "Hello World" result

let test_decode_short_word () =
  Alcotest.(check string)
    "short word" "abc"
    (Email_channel.decode_rfc2047_word "abc")

let test_decode_not_encoded () =
  Alcotest.(check string)
    "not encoded" "normal text"
    (Email_channel.decode_rfc2047_word "normal text")

(* --- decode_header_value tests --- *)

let test_decode_header_plain () =
  Alcotest.(check string)
    "plain header" "Subject line"
    (Email_channel.decode_header_value "Subject line")

let test_decode_header_mixed () =
  let v = "Re: =?UTF-8?B?SGVsbG8=?= world" in
  let result = Email_channel.decode_header_value v in
  Alcotest.(check string) "mixed decoded" "Re: Hello world" result

(* --- strip_html tests --- *)

let test_strip_html_no_tags () =
  Alcotest.(check string) "no tags" "hello" (Email_channel.strip_html "hello")

let test_strip_html_simple () =
  Alcotest.(check string)
    "simple tags" "hello"
    (Email_channel.strip_html "<b>hello</b>")

let test_strip_html_nested () =
  Alcotest.(check string)
    "nested" "content"
    (Email_channel.strip_html "<div><p>content</p></div>")

let test_strip_html_empty () =
  Alcotest.(check string) "empty" "" (Email_channel.strip_html "")

let test_strip_html_br () =
  Alcotest.(check string)
    "br" "line1line2"
    (Email_channel.strip_html "line1<br>line2")

(* --- is_allowed tests --- *)

let mk_email_cfg ?(allow_from = []) () : Runtime_config.email_config =
  {
    imap_host = "imap.test.com";
    imap_port = 993;
    smtp_host = "smtp.test.com";
    smtp_port = 587;
    username = "user";
    password = "pass";
    from_address = "bot@test.com";
    allow_from;
    poll_interval_s = 60.0;
    default_model = None;
  }

let test_is_allowed_empty () =
  let cfg = mk_email_cfg () in
  Alcotest.(check bool)
    "empty allows all" true
    (Email_channel.is_allowed ~cfg ~from:"anyone@test.com")

let test_is_allowed_exact_match () =
  let cfg = mk_email_cfg ~allow_from:[ "alice@test.com" ] () in
  Alcotest.(check bool)
    "exact match" true
    (Email_channel.is_allowed ~cfg ~from:"alice@test.com")

let test_is_allowed_domain_match () =
  let cfg = mk_email_cfg ~allow_from:[ "@test.com" ] () in
  Alcotest.(check bool)
    "domain match" true
    (Email_channel.is_allowed ~cfg ~from:"anyone@test.com")

let test_is_allowed_bare_domain () =
  let cfg = mk_email_cfg ~allow_from:[ "test.com" ] () in
  Alcotest.(check bool)
    "bare domain" true
    (Email_channel.is_allowed ~cfg ~from:"alice@test.com")

let test_is_allowed_no_match () =
  let cfg = mk_email_cfg ~allow_from:[ "alice@test.com" ] () in
  Alcotest.(check bool)
    "no match" false
    (Email_channel.is_allowed ~cfg ~from:"bob@other.com")

let test_is_allowed_case_insensitive () =
  let cfg = mk_email_cfg ~allow_from:[ "Alice@Test.COM" ] () in
  Alcotest.(check bool)
    "case insensitive" true
    (Email_channel.is_allowed ~cfg ~from:"alice@test.com")

(* --- extract_email_addr tests --- *)

let test_extract_bare () =
  Alcotest.(check string)
    "bare email" "user@test.com"
    (Email_channel.extract_email_addr "user@test.com")

let test_extract_with_name () =
  Alcotest.(check string)
    "with name" "user@test.com"
    (Email_channel.extract_email_addr "User Name <user@test.com>")

let test_extract_no_angle () =
  Alcotest.(check string)
    "no angle" "user@test.com"
    (Email_channel.extract_email_addr "user@test.com")

let test_extract_whitespace () =
  Alcotest.(check string)
    "trimmed" "user@test.com"
    (Email_channel.extract_email_addr "  user@test.com  ")

(* --- mark_seen / is_seen tests --- *)

let test_mark_seen_and_check () =
  (* Reset state - these use global state, but test basic behavior *)
  let id = "test-msg-" ^ string_of_float (Unix.gettimeofday ()) in
  Alcotest.(check bool) "not seen initially" false (Email_channel.is_seen id);
  Email_channel.mark_seen id;
  Alcotest.(check bool) "seen after mark" true (Email_channel.is_seen id)

let test_mark_seen_idempotent () =
  let id = "idem-" ^ string_of_float (Unix.gettimeofday ()) in
  Email_channel.mark_seen id;
  Email_channel.mark_seen id;
  Alcotest.(check bool) "still seen" true (Email_channel.is_seen id)

(* --- parse_fetch_headers tests --- *)

let test_parse_fetch_headers_basic () =
  let text =
    "From: alice@test.com\nSubject: Hello\nMessage-ID: <abc123>\n\nbody"
  in
  let from_, subject, msg_id = Email_channel.parse_fetch_headers text in
  Alcotest.(check string) "from" "alice@test.com" from_;
  Alcotest.(check string) "subject" "Hello" subject;
  Alcotest.(check string) "message-id" "<abc123>" msg_id

let test_parse_fetch_headers_missing () =
  let text = "X-Custom: value\n\nbody" in
  let from_, subject, msg_id = Email_channel.parse_fetch_headers text in
  Alcotest.(check string) "from empty" "" from_;
  Alcotest.(check string) "subject empty" "" subject;
  Alcotest.(check string) "message-id empty" "" msg_id

let suite =
  [
    Alcotest.test_case "decode plain text" `Quick test_decode_plain_text;
    Alcotest.test_case "decode base64" `Quick test_decode_base64;
    Alcotest.test_case "decode qp" `Quick test_decode_qp;
    Alcotest.test_case "decode qp hex" `Quick test_decode_qp_hex;
    Alcotest.test_case "decode short word" `Quick test_decode_short_word;
    Alcotest.test_case "decode not encoded" `Quick test_decode_not_encoded;
    Alcotest.test_case "decode header plain" `Quick test_decode_header_plain;
    Alcotest.test_case "decode header mixed" `Quick test_decode_header_mixed;
    Alcotest.test_case "strip html no tags" `Quick test_strip_html_no_tags;
    Alcotest.test_case "strip html simple" `Quick test_strip_html_simple;
    Alcotest.test_case "strip html nested" `Quick test_strip_html_nested;
    Alcotest.test_case "strip html empty" `Quick test_strip_html_empty;
    Alcotest.test_case "strip html br" `Quick test_strip_html_br;
    Alcotest.test_case "is_allowed empty" `Quick test_is_allowed_empty;
    Alcotest.test_case "is_allowed exact" `Quick test_is_allowed_exact_match;
    Alcotest.test_case "is_allowed domain" `Quick test_is_allowed_domain_match;
    Alcotest.test_case "is_allowed bare domain" `Quick
      test_is_allowed_bare_domain;
    Alcotest.test_case "is_allowed no match" `Quick test_is_allowed_no_match;
    Alcotest.test_case "is_allowed case insensitive" `Quick
      test_is_allowed_case_insensitive;
    Alcotest.test_case "extract bare email" `Quick test_extract_bare;
    Alcotest.test_case "extract with name" `Quick test_extract_with_name;
    Alcotest.test_case "extract no angle" `Quick test_extract_no_angle;
    Alcotest.test_case "extract whitespace" `Quick test_extract_whitespace;
    Alcotest.test_case "mark seen and check" `Quick test_mark_seen_and_check;
    Alcotest.test_case "mark seen idempotent" `Quick test_mark_seen_idempotent;
    Alcotest.test_case "parse headers basic" `Quick
      test_parse_fetch_headers_basic;
    Alcotest.test_case "parse headers missing" `Quick
      test_parse_fetch_headers_missing;
  ]

let test_escape_mdv2_special_chars () =
  let result = Telegram_format.escape_mdv2 "hello_world*bold[link](url)" in
  Alcotest.(check string)
    "special chars escaped" "hello\\_world\\*bold\\[link\\]\\(url\\)" result

let test_escape_mdv2_plain () =
  let result = Telegram_format.escape_mdv2 "plain text 123" in
  Alcotest.(check string) "plain text unchanged" "plain text 123" result

let test_escape_mdv2_all_specials () =
  let result = Telegram_format.escape_mdv2 "_*[]()~`>#+-=|{}.!\\" in
  Alcotest.(check string)
    "all specials escaped"
    "\\_\\*\\[\\]\\(\\)\\~\\`\\>\\#\\+\\-\\=\\|\\{\\}\\.\\!\\\\" result

let test_parse_inline_bold () =
  let segs = Telegram_format.parse_inline_markdown "hello *world*" in
  Alcotest.(check int) "two segments" 2 (List.length segs);
  match segs with
  | [ Telegram_format.Plain "hello "; Telegram_format.Bold "world" ] -> ()
  | _ -> Alcotest.fail "unexpected segments"

let test_parse_inline_italic () =
  let segs = Telegram_format.parse_inline_markdown "_italic_ text" in
  match segs with
  | [ Telegram_format.Italic "italic"; Telegram_format.Plain " text" ] -> ()
  | _ -> Alcotest.fail "unexpected segments"

let test_parse_inline_code () =
  let segs = Telegram_format.parse_inline_markdown "run `ls -la` now" in
  match segs with
  | [
   Telegram_format.Plain "run ";
   Telegram_format.Code "ls -la";
   Telegram_format.Plain " now";
  ] ->
      ()
  | _ -> Alcotest.fail "unexpected segments"

let test_markdown_to_mdv2_basic () =
  let result = Telegram_format.markdown_to_mdv2 "hello *bold* _italic_" in
  Alcotest.(check string) "converted" "hello *bold* _italic_" result

let test_markdown_to_mdv2_escapes_plain () =
  let result = Telegram_format.markdown_to_mdv2 "price is 10.5 (USD)" in
  Alcotest.(check string)
    "dots and parens escaped" "price is 10\\.5 \\(USD\\)" result

let test_markdown_to_mdv2_mixed () =
  let result = Telegram_format.markdown_to_mdv2 "*bold* has a dot." in
  Alcotest.(check string)
    "bold preserved, dot escaped" "*bold* has a dot\\." result

let test_expandable_blockquote_short () =
  let result = Telegram_format.expandable_blockquote "line1\nline2" in
  Alcotest.(check string) "short text just escaped" "line1\nline2" result

let test_expandable_blockquote_long () =
  let text = "a\nb\nc\nd\ne" in
  let result = Telegram_format.expandable_blockquote ~visible_lines:2 text in
  assert (String.length result > String.length text);
  assert (String.sub result (String.length result - 2) 2 = "||")

let test_format_verbose_result_short () =
  let result = Telegram_format.format_verbose_result ~name:"shell_exec" "ok" in
  Alcotest.(check bool) "short result is None" true (result = None)

let test_format_verbose_result_long () =
  let long = String.concat "\n" (List.init 10 (fun i -> string_of_int i)) in
  let result = Telegram_format.format_verbose_result ~name:"shell_exec" long in
  Alcotest.(check bool) "long result is Some" true (result <> None)

let test_spoiler_wraps_text () =
  let result = Telegram_format.spoiler "secret data" in
  Alcotest.(check string) "spoiler wrapped" "||secret data||" result

let test_spoiler_escapes_special () =
  let result = Telegram_format.spoiler "key=val.x" in
  Alcotest.(check string) "spoiler escapes" "||key\\=val\\.x||" result

let test_is_sensitive_memory_recall () =
  let result =
    Telegram_format.is_sensitive_content ~name:"memory_recall" "anything"
  in
  Alcotest.(check bool) "memory_recall is sensitive" true result

let test_is_sensitive_shell_with_password () =
  let result =
    Telegram_format.is_sensitive_content ~name:"shell_exec"
      "DATABASE_PASSWORD=hunter2"
  in
  Alcotest.(check bool) "shell with password is sensitive" true result

let test_is_sensitive_shell_normal () =
  let result =
    Telegram_format.is_sensitive_content ~name:"shell_exec"
      "total 42\ndrwxr-xr-x 2 user user 4096"
  in
  Alcotest.(check bool) "normal shell not sensitive" false result

let test_is_sensitive_other_tool () =
  let result =
    Telegram_format.is_sensitive_content ~name:"file_read" "password=foo"
  in
  Alcotest.(check bool) "file_read not sensitive" false result

let test_format_sensitive_result_some () =
  let result =
    Telegram_format.format_sensitive_result ~name:"memory_recall" "my secret"
  in
  Alcotest.(check bool) "sensitive returns Some" true (result <> None)

let test_format_sensitive_result_none () =
  let result =
    Telegram_format.format_sensitive_result ~name:"shell_exec" "ls output"
  in
  Alcotest.(check bool) "normal returns None" true (result = None)

let contains s sub =
  try
    ignore (Str.search_forward (Str.regexp_string sub) s 0);
    true
  with Not_found -> false

let test_format_error_standalone () =
  let result =
    Telegram_format.format_error_standalone ~emoji:"X" ~name:"shell_exec"
      ~summary:(Some "bad cmd") ~duration_secs:(Some 2.9)
      ~result:"Error: command is required"
  in
  (* MarkdownV2 escapes _ in tool name *)
  Alcotest.(check bool)
    "contains escaped name" true
    (contains result "shell\\_exec");
  (* MarkdownV2 escapes . in duration *)
  Alcotest.(check bool)
    "contains escaped duration" true (contains result "2\\.9s");
  (* └ = E2 94 94 *)
  Alcotest.(check bool)
    "contains tree corner" true
    (contains result "\xe2\x94\x94");
  Alcotest.(check bool)
    "contains error text" true
    (contains result "Error: command is required")

let suite =
  [
    Alcotest.test_case "escape_mdv2 special chars" `Quick
      test_escape_mdv2_special_chars;
    Alcotest.test_case "escape_mdv2 plain text" `Quick test_escape_mdv2_plain;
    Alcotest.test_case "escape_mdv2 all specials" `Quick
      test_escape_mdv2_all_specials;
    Alcotest.test_case "parse_inline bold" `Quick test_parse_inline_bold;
    Alcotest.test_case "parse_inline italic" `Quick test_parse_inline_italic;
    Alcotest.test_case "parse_inline code" `Quick test_parse_inline_code;
    Alcotest.test_case "markdown_to_mdv2 basic" `Quick
      test_markdown_to_mdv2_basic;
    Alcotest.test_case "markdown_to_mdv2 escapes plain" `Quick
      test_markdown_to_mdv2_escapes_plain;
    Alcotest.test_case "markdown_to_mdv2 mixed" `Quick
      test_markdown_to_mdv2_mixed;
    Alcotest.test_case "expandable_blockquote short" `Quick
      test_expandable_blockquote_short;
    Alcotest.test_case "expandable_blockquote long" `Quick
      test_expandable_blockquote_long;
    Alcotest.test_case "format_verbose_result short" `Quick
      test_format_verbose_result_short;
    Alcotest.test_case "format_verbose_result long" `Quick
      test_format_verbose_result_long;
    Alcotest.test_case "spoiler wraps text" `Quick test_spoiler_wraps_text;
    Alcotest.test_case "spoiler escapes special chars" `Quick
      test_spoiler_escapes_special;
    Alcotest.test_case "is_sensitive memory_recall" `Quick
      test_is_sensitive_memory_recall;
    Alcotest.test_case "is_sensitive shell with password" `Quick
      test_is_sensitive_shell_with_password;
    Alcotest.test_case "is_sensitive shell normal" `Quick
      test_is_sensitive_shell_normal;
    Alcotest.test_case "is_sensitive other tool" `Quick
      test_is_sensitive_other_tool;
    Alcotest.test_case "format_sensitive_result some" `Quick
      test_format_sensitive_result_some;
    Alcotest.test_case "format_sensitive_result none" `Quick
      test_format_sensitive_result_none;
    Alcotest.test_case "format_error_standalone" `Quick
      test_format_error_standalone;
  ]

let test_no_tables_unchanged () =
  let input = "Hello world\nThis is plain text.\nNo pipes here." in
  let result = Markdown_util.normalize_tables input in
  Alcotest.(check string) "plain text unchanged" input result

let test_table_gets_blank_lines () =
  let input = "Some text\n| A | B |\n| --- | --- |\n| 1 | 2 |\nMore text" in
  let result = Markdown_util.normalize_tables input in
  let expected =
    "Some text\n\n| A | B |\n| --- | --- |\n| 1 | 2 |\n\nMore text"
  in
  Alcotest.(check string) "blank lines around table" expected result

let test_missing_separator_inserted () =
  let input = "Text\n\n| Name | Value |\n| Alice | 42 |\n\nEnd" in
  let result = Markdown_util.normalize_tables input in
  let expected =
    "Text\n\n| Name | Value |\n| --- | --- |\n| Alice | 42 |\n\nEnd"
  in
  Alcotest.(check string) "separator inserted" expected result

let test_trailing_pipe_added () =
  let input = "Before\n\n| A | B\n| --- | ---\n| 1 | 2\n\nAfter" in
  let result = Markdown_util.normalize_tables input in
  let expected = "Before\n\n| A | B |\n| --- | --- |\n| 1 | 2 |\n\nAfter" in
  Alcotest.(check string) "trailing pipes added" expected result

let test_code_block_skipped () =
  let input = "Before\n```\n| A | B |\n| 1 | 2 |\n```\nAfter" in
  let result = Markdown_util.normalize_tables input in
  Alcotest.(check string) "code block untouched" input result

let test_already_well_formed_unchanged () =
  let input =
    "Text\n\n| H1 | H2 |\n| --- | --- |\n| a | b |\n| c | d |\n\nEnd"
  in
  let result = Markdown_util.normalize_tables input in
  Alcotest.(check string) "well-formed unchanged" input result

let test_multiple_tables () =
  let input =
    "Intro\n\
     | A | B |\n\
     | --- | --- |\n\
     | 1 | 2 |\n\
     Middle\n\
     | X | Y |\n\
     | --- | --- |\n\
     | 3 | 4 |\n\
     End"
  in
  let result = Markdown_util.normalize_tables input in
  let expected =
    "Intro\n\n\
     | A | B |\n\
     | --- | --- |\n\
     | 1 | 2 |\n\n\
     Middle\n\n\
     | X | Y |\n\
     | --- | --- |\n\
     | 3 | 4 |\n\n\
     End"
  in
  Alcotest.(check string) "multiple tables normalized" expected result

let test_table_at_start () =
  let input = "| A | B |\n| --- | --- |\n| 1 | 2 |\nEnd" in
  let result = Markdown_util.normalize_tables input in
  let expected = "| A | B |\n| --- | --- |\n| 1 | 2 |\n\nEnd" in
  Alcotest.(check string) "table at start" expected result

let test_idempotent () =
  let input = "Text\n\n| H1 | H2 |\n| --- | --- |\n| a | b |\n\nEnd" in
  let once = Markdown_util.normalize_tables input in
  let twice = Markdown_util.normalize_tables once in
  Alcotest.(check string) "idempotent" once twice

let test_no_pipe_fast_path () =
  let input = "No pipes at all in this text" in
  let result = Markdown_util.normalize_tables input in
  Alcotest.(check string) "no pipe fast path" input result

let test_tilde_fence_skipped () =
  let input = "Before\n~~~\n| A | B |\n| 1 | 2 |\n~~~\nAfter" in
  let result = Markdown_util.normalize_tables input in
  Alcotest.(check string) "tilde fence untouched" input result

let suite =
  [
    Alcotest.test_case "no tables unchanged" `Quick test_no_tables_unchanged;
    Alcotest.test_case "table gets blank lines" `Quick
      test_table_gets_blank_lines;
    Alcotest.test_case "missing separator inserted" `Quick
      test_missing_separator_inserted;
    Alcotest.test_case "trailing pipe added" `Quick test_trailing_pipe_added;
    Alcotest.test_case "code block skipped" `Quick test_code_block_skipped;
    Alcotest.test_case "already well-formed unchanged" `Quick
      test_already_well_formed_unchanged;
    Alcotest.test_case "multiple tables" `Quick test_multiple_tables;
    Alcotest.test_case "table at start" `Quick test_table_at_start;
    Alcotest.test_case "idempotent" `Quick test_idempotent;
    Alcotest.test_case "no pipe fast path" `Quick test_no_pipe_fast_path;
    Alcotest.test_case "tilde fence skipped" `Quick test_tilde_fence_skipped;
  ]

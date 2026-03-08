(* Tests for Chat UI assets module *)

let test_index_html_non_empty () =
  Alcotest.(check bool)
    "index_html non-empty" true
    (String.length Chat_ui_assets.index_html > 0)

let test_index_html_has_doctype () =
  let contains s sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has DOCTYPE" true
    (contains Chat_ui_assets.index_html "<!DOCTYPE")

let test_index_html_has_chat_js () =
  let contains s sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has chat.js" true
    (contains Chat_ui_assets.index_html "chat.js")

let test_chat_css_non_empty () =
  Alcotest.(check bool)
    "chat_css non-empty" true
    (String.length Chat_ui_assets.chat_css > 0)

let test_chat_css_has_body () =
  let contains s sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has body rule" true
    (contains Chat_ui_assets.chat_css "body")

let test_chat_js_non_empty () =
  Alcotest.(check bool)
    "chat_js non-empty" true
    (String.length Chat_ui_assets.chat_js > 0)

let test_chat_js_has_fetch () =
  let contains s sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has fetch" true
    (contains Chat_ui_assets.chat_js "fetch")

let test_chat_js_has_pair_modal () =
  let contains s sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has pair modal" true
    (contains Chat_ui_assets.chat_js "pair")

let test_index_html_has_pair_modal () =
  let contains s sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has pair-modal div" true
    (contains Chat_ui_assets.index_html "pair-modal")

let test_chat_css_has_tool_panel () =
  let contains s sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has tool-panel" true
    (contains Chat_ui_assets.chat_css "tool-panel")

let test_chat_css_pairing_modal_hidden () =
  let contains s sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "pairing-modal[hidden] display:none present" true
    (contains Chat_ui_assets.chat_css "pairing-modal[hidden]")

let test_index_html_no_cjs_hljs () =
  let contains s sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "highlight.js CDN uses browser bundle not lib/common.min.js" false
    (contains Chat_ui_assets.index_html "highlight.js/lib/common.min.js")

let suite =
  [
    Alcotest.test_case "index_html non-empty" `Quick test_index_html_non_empty;
    Alcotest.test_case "index_html has DOCTYPE" `Quick
      test_index_html_has_doctype;
    Alcotest.test_case "index_html has chat.js" `Quick
      test_index_html_has_chat_js;
    Alcotest.test_case "chat_css non-empty" `Quick test_chat_css_non_empty;
    Alcotest.test_case "chat_css has body" `Quick test_chat_css_has_body;
    Alcotest.test_case "chat_js non-empty" `Quick test_chat_js_non_empty;
    Alcotest.test_case "chat_js has fetch" `Quick test_chat_js_has_fetch;
    Alcotest.test_case "chat_js has pair modal" `Quick
      test_chat_js_has_pair_modal;
    Alcotest.test_case "index_html has pair-modal" `Quick
      test_index_html_has_pair_modal;
    Alcotest.test_case "chat_css has tool-panel" `Quick
      test_chat_css_has_tool_panel;
    Alcotest.test_case "pairing modal hidden overrides display" `Quick
      test_chat_css_pairing_modal_hidden;
    Alcotest.test_case "index_html no CJS highlight.js" `Quick
      test_index_html_no_cjs_hljs;
  ]

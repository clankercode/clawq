(* test_setup_zai_mcp.ml — Unit tests for Setup_zai_mcp pure functions *)

let validate_key_valid () =
  Alcotest.(check (result string string))
    "valid key" (Ok "sk-abc123")
    (Setup_zai_mcp.validate_key "sk-abc123")

let validate_key_empty () =
  match Setup_zai_mcp.validate_key "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty key"

let validate_key_whitespace () =
  match Setup_zai_mcp.validate_key "   " with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for whitespace-only key"

let build_json_roundtrip () =
  let json =
    Setup_zai_mcp.build_zai_mcp_json ~key:"sk-abc123" ~websearch_enabled:true
      ~webfetch_enabled:true
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.zai_mcp with
  | Some z ->
      Alcotest.(check string) "key" "sk-abc123" z.key;
      Alcotest.(check bool) "websearch_enabled" true z.websearch_enabled;
      Alcotest.(check bool) "webfetch_enabled" true z.webfetch_enabled
  | None -> Alcotest.fail "expected zai_mcp config"

let build_json_partial_disabled () =
  let json =
    Setup_zai_mcp.build_zai_mcp_json ~key:"my-token" ~websearch_enabled:false
      ~webfetch_enabled:true
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.zai_mcp with
  | Some z ->
      Alcotest.(check bool) "websearch disabled" false z.websearch_enabled;
      Alcotest.(check bool) "webfetch enabled" true z.webfetch_enabled
  | None -> Alcotest.fail "expected zai_mcp config"

let build_json_both_disabled () =
  let json =
    Setup_zai_mcp.build_zai_mcp_json ~key:"tok" ~websearch_enabled:false
      ~webfetch_enabled:false
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.zai_mcp with
  | Some z ->
      Alcotest.(check bool) "websearch disabled" false z.websearch_enabled;
      Alcotest.(check bool) "webfetch disabled" false z.webfetch_enabled
  | None -> Alcotest.fail "expected zai_mcp config"

let post_instructions_content () =
  let s = Setup_zai_mcp.post_setup_instructions in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has docs url" true
    (contains "https://clawq.org/zai-mcp/");
  Alcotest.(check bool) "mentions web search" true (contains "web search");
  Alcotest.(check bool) "mentions web fetch" true (contains "web fetch")

let suite =
  [
    Alcotest.test_case "validate_key valid" `Quick validate_key_valid;
    Alcotest.test_case "validate_key empty" `Quick validate_key_empty;
    Alcotest.test_case "validate_key whitespace" `Quick validate_key_whitespace;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json partial disabled" `Quick
      build_json_partial_disabled;
    Alcotest.test_case "build_json both disabled" `Quick
      build_json_both_disabled;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]

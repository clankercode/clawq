(* Tests for Signal channel module *)

(* --- chunk_text tests --- *)

let test_chunk_short () =
  let chunks = Signal.chunk_text "hello" in
  Alcotest.(check int) "1 chunk" 1 (List.length chunks);
  Alcotest.(check string) "content" "hello" (List.hd chunks)

let test_chunk_empty () =
  let chunks = Signal.chunk_text "" in
  Alcotest.(check int) "1 chunk for empty" 1 (List.length chunks);
  Alcotest.(check string) "empty" "" (List.hd chunks)

let test_chunk_exact_limit () =
  let text = String.make 1600 'a' in
  let chunks = Signal.chunk_text ~max_bytes:1600 text in
  Alcotest.(check int) "1 chunk at limit" 1 (List.length chunks)

let test_chunk_over_limit () =
  let text = String.make 3201 'x' in
  let chunks = Signal.chunk_text ~max_bytes:1600 text in
  Alcotest.(check bool) "multiple chunks" true (List.length chunks >= 2)

let test_chunk_preserves_content () =
  let text = String.make 5000 'z' in
  let chunks = Signal.chunk_text ~max_bytes:1600 text in
  let reconstructed = String.concat "" chunks in
  Alcotest.(check string) "content preserved" text reconstructed

let test_chunk_custom_max_bytes () =
  let text = String.make 100 'a' in
  let chunks = Signal.chunk_text ~max_bytes:30 text in
  Alcotest.(check bool) "multiple chunks at 30" true (List.length chunks >= 3)

let test_chunk_newline_break () =
  let text = String.make 800 'a' ^ "\n" ^ String.make 800 'b' in
  let chunks = Signal.chunk_text ~max_bytes:1600 text in
  Alcotest.(check bool) "splits near newline" true (List.length chunks >= 1)

(* --- parse_jsonrpc_event tests --- *)

let test_parse_jsonrpc_receive () =
  let line =
    {|{"method":"receive","params":{"envelope":{"source":"+1234","dataMessage":{"message":"hello"}}}}|}
  in
  match Signal.parse_jsonrpc_event line with
  | Some (from, _group_id_opt, msg) ->
      Alcotest.(check string) "from" "+1234" from;
      Alcotest.(check string) "msg" "hello" msg
  | None -> Alcotest.fail "expected Some"

let test_parse_jsonrpc_non_receive () =
  let line = {|{"method":"send","params":{}}|} in
  match Signal.parse_jsonrpc_event line with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for non-receive"

let test_parse_jsonrpc_invalid () =
  match Signal.parse_jsonrpc_event "not json" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for invalid"

let test_parse_jsonrpc_empty () =
  match Signal.parse_jsonrpc_event "" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for empty"

(* --- parse_rest_messages tests --- *)

let test_parse_rest_messages_valid () =
  let body =
    {|[{"envelope":{"source":"+5678","dataMessage":{"body":"world"}}}]|}
  in
  let msgs = Signal.parse_rest_messages body in
  Alcotest.(check int) "1 message" 1 (List.length msgs);
  let from, _group_id_opt, text = List.hd msgs in
  Alcotest.(check string) "from" "+5678" from;
  Alcotest.(check string) "text" "world" text

let test_parse_rest_messages_empty_list () =
  let msgs = Signal.parse_rest_messages "[]" in
  Alcotest.(check int) "no messages" 0 (List.length msgs)

let test_parse_rest_messages_invalid () =
  let msgs = Signal.parse_rest_messages "bad" in
  Alcotest.(check int) "no messages for invalid" 0 (List.length msgs)

let test_parse_rest_messages_empty_text () =
  let body =
    {|[{"envelope":{"source":"+1111","dataMessage":{"message":""}}}]|}
  in
  let msgs = Signal.parse_rest_messages body in
  Alcotest.(check int) "skip empty text" 0 (List.length msgs)

(* --- is_allowed tests --- *)

let mk_signal_cfg ?(allow_from = []) () : Runtime_config.signal_config =
  {
    base_url = "http://localhost:8080";
    account = "+1";
    allow_from;
    max_chunk_bytes = 1600;
    api_mode = "jsonrpc";
  }

let test_is_allowed_empty_list () =
  let cfg = mk_signal_cfg () in
  Alcotest.(check bool)
    "empty list allows all" true
    (Signal.is_allowed ~cfg ~from:"+999")

let test_is_allowed_match () =
  let cfg = mk_signal_cfg ~allow_from:[ "+123"; "+456" ] () in
  Alcotest.(check bool)
    "match allowed" true
    (Signal.is_allowed ~cfg ~from:"+123")

let test_is_allowed_no_match () =
  let cfg = mk_signal_cfg ~allow_from:[ "+123" ] () in
  Alcotest.(check bool)
    "no match denied" false
    (Signal.is_allowed ~cfg ~from:"+999")

let suite =
  [
    Alcotest.test_case "chunk short" `Quick test_chunk_short;
    Alcotest.test_case "chunk empty" `Quick test_chunk_empty;
    Alcotest.test_case "chunk exact limit" `Quick test_chunk_exact_limit;
    Alcotest.test_case "chunk over limit" `Quick test_chunk_over_limit;
    Alcotest.test_case "chunk preserves content" `Quick
      test_chunk_preserves_content;
    Alcotest.test_case "chunk custom max_bytes" `Quick
      test_chunk_custom_max_bytes;
    Alcotest.test_case "chunk newline break" `Quick test_chunk_newline_break;
    Alcotest.test_case "parse jsonrpc receive" `Quick test_parse_jsonrpc_receive;
    Alcotest.test_case "parse jsonrpc non-receive" `Quick
      test_parse_jsonrpc_non_receive;
    Alcotest.test_case "parse jsonrpc invalid" `Quick test_parse_jsonrpc_invalid;
    Alcotest.test_case "parse jsonrpc empty" `Quick test_parse_jsonrpc_empty;
    Alcotest.test_case "parse rest valid" `Quick test_parse_rest_messages_valid;
    Alcotest.test_case "parse rest empty list" `Quick
      test_parse_rest_messages_empty_list;
    Alcotest.test_case "parse rest invalid" `Quick
      test_parse_rest_messages_invalid;
    Alcotest.test_case "parse rest empty text" `Quick
      test_parse_rest_messages_empty_text;
    Alcotest.test_case "is_allowed empty list" `Quick test_is_allowed_empty_list;
    Alcotest.test_case "is_allowed match" `Quick test_is_allowed_match;
    Alcotest.test_case "is_allowed no match" `Quick test_is_allowed_no_match;
  ]

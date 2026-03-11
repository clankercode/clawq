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

let test_parse_sse_data_line_valid () =
  match Signal.parse_sse_data_line {|data: {"method":"receive"}|} with
  | Some payload ->
      Alcotest.(check string) "payload" {|{"method":"receive"}|} payload
  | None -> Alcotest.fail "expected SSE payload"

let test_parse_sse_data_line_ignores_non_data () =
  match Signal.parse_sse_data_line "event: message" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for non-data line"

let test_process_sse_chunk_handles_split_lines () =
  let seen = ref [] in
  let on_event line =
    seen := line :: !seen;
    Lwt.return_unit
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let buffer = Buffer.create 32 in
     let* () = Signal.process_sse_chunk ~on_event buffer "data: {\"a\":" in
     let* () = Signal.process_sse_chunk ~on_event buffer "1}\n\n" in
     Signal.flush_sse_buffer ~on_event buffer);
  Alcotest.(check (list string))
    "assembled lines" [ "data: {\"a\":1}"; "" ] (List.rev !seen)

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


let free_port () =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close sock)
    (fun () ->
      Unix.setsockopt sock Unix.SO_REUSEADDR true;
      Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname sock with
      | Unix.ADDR_INET (_, port) -> port
      | Unix.ADDR_UNIX _ -> Alcotest.fail "expected inet socket")

let with_signal_sse_server ~delay_s ~payload f =
  let port = free_port () in
  let callback _conn _req _body =
    let body_stream, push = Lwt_stream.create () in
    Lwt.async (fun () ->
        let open Lwt.Syntax in
        let* () = Lwt_unix.sleep delay_s in
        push (Some ("data: " ^ payload ^ "

"));
        push None;
        Lwt.return_unit);
    let headers =
      Cohttp.Header.of_list [ ("Content-Type", "text/event-stream") ]
    in
    Cohttp_lwt_unix.Server.respond ~status:`OK ~headers
      ~body:(Cohttp_lwt.Body.of_stream body_stream) ()
  in
  let stop, stopper = Lwt.wait () in
  let server =
    Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  Lwt.async (fun () -> Lwt.pick [ server; stop ]);
  Fun.protect
    ~finally:(fun () -> Lwt.wakeup_later stopper ())
    (fun () -> f port)

let test_receive_loop_jsonrpc_allows_delayed_sse_body () =
  let payload =
    {|{"jsonrpc":"2.0","method":"receive","params":{"envelope":{"source":"+1234","dataMessage":{"message":"hello from stream"}}}}|}
  in
  with_signal_sse_server ~delay_s:0.15 ~payload (fun port ->
      let cfg : Runtime_config.signal_config =
        {
          base_url = Printf.sprintf "http://127.0.0.1:%d" port;
          account = "+1000";
          allow_from = [];
          max_chunk_bytes = 1600;
          api_mode = "jsonrpc";
        }
      in
      let old_timeout = !(Http_client.default_timeout_s) in
      Fun.protect
        ~finally:(fun () -> Http_client.set_default_timeout_s old_timeout)
        (fun () ->
          Http_client.set_default_timeout_s 0.05;
          let seen = ref None in
          let finished = Lwt.wait () in
          let stop, stopper = finished in
          Lwt_main.run
            (Lwt.pick
               [
                 (Signal.receive_loop_jsonrpc ~cfg ~on_message:(fun ~from ~group_id_opt:_ ~text ->
                      seen := Some (from, text);
                      if Lwt.is_sleeping stop then Lwt.wakeup_later stopper ();
                      Lwt.return_unit)
                    ());
                 stop;
                 (let open Lwt.Syntax in
                  let* () = Lwt_unix.sleep 1.0 in
                  Alcotest.fail "timed out waiting for SSE message");
               ]);
          match !seen with
          | Some (from, text) ->
              Alcotest.(check string) "from" "+1234" from;
              Alcotest.(check string) "text" "hello from stream" text
          | None -> Alcotest.fail "expected SSE-delivered message"))

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
    Alcotest.test_case "parse sse data line" `Quick
      test_parse_sse_data_line_valid;
    Alcotest.test_case "parse sse ignores non-data" `Quick
      test_parse_sse_data_line_ignores_non_data;
    Alcotest.test_case "process sse split lines" `Quick
      test_process_sse_chunk_handles_split_lines;
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
    Alcotest.test_case "jsonrpc loop allows delayed sse body" `Quick
      test_receive_loop_jsonrpc_allows_delayed_sse_body;
  ]

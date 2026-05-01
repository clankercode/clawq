(* Tests for Provider_minimax module *)

let test_user_message () =
  let msgs = [ Provider.make_message ~role:"user" ~content:"hello" ] in
  let result = Provider_minimax.messages_to_anthropic_json msgs in
  Alcotest.(check int) "1 message" 1 (List.length result);
  let json = List.hd result in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "role" "user" (json |> member "role" |> to_string);
  Alcotest.(check string)
    "content" "hello"
    (json |> member "content" |> to_string)

let test_assistant_message () =
  let msgs = [ Provider.make_message ~role:"assistant" ~content:"hi" ] in
  let result = Provider_minimax.messages_to_anthropic_json msgs in
  let json = List.hd result in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "role" "assistant" (json |> member "role" |> to_string)

let test_system_message_filtered () =
  let msgs =
    [ Provider.make_message ~role:"system" ~content:"you are a bot" ]
  in
  let result = Provider_minimax.messages_to_anthropic_json msgs in
  Alcotest.(check int) "system filtered out" 0 (List.length result)

let test_tool_result_becomes_user () =
  let msg =
    Provider.make_tool_result ~tool_call_id:"tc1" ~name:"bash" ~content:"done"
  in
  let result = Provider_minimax.messages_to_anthropic_json [ msg ] in
  let json = List.hd result in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "tool result role=user" "user"
    (json |> member "role" |> to_string)

let test_tool_result_has_tool_use_id () =
  let msg =
    Provider.make_tool_result ~tool_call_id:"tc1" ~name:"bash" ~content:"done"
  in
  let result = Provider_minimax.messages_to_anthropic_json [ msg ] in
  let json = List.hd result in
  let open Yojson.Safe.Util in
  let content = json |> member "content" |> to_list in
  Alcotest.(check int) "1 content block" 1 (List.length content);
  let block = List.hd content in
  Alcotest.(check string)
    "type" "tool_result"
    (block |> member "type" |> to_string);
  Alcotest.(check string)
    "tool_use_id" "tc1"
    (block |> member "tool_use_id" |> to_string)

let test_assistant_with_tool_calls () =
  let tc =
    {
      Provider.id = "call-1";
      function_name = "file_read";
      arguments = {|{"path":"/tmp"}|};
    }
  in
  let msg =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls = [ tc ];
      tool_call_id = None;
      name = None;
      provider_response_items_json = None;
      thinking = None;
    }
  in
  let result = Provider_minimax.messages_to_anthropic_json [ msg ] in
  let json = List.hd result in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "role" "assistant" (json |> member "role" |> to_string);
  let content = json |> member "content" |> to_list in
  Alcotest.(check int) "1 tool use block" 1 (List.length content);
  let block = List.hd content in
  Alcotest.(check string) "type" "tool_use" (block |> member "type" |> to_string);
  Alcotest.(check string)
    "name" "file_read"
    (block |> member "name" |> to_string)

let test_mixed_messages () =
  let msgs =
    [
      Provider.make_message ~role:"system" ~content:"system prompt";
      Provider.make_message ~role:"user" ~content:"q1";
      Provider.make_message ~role:"assistant" ~content:"a1";
    ]
  in
  let result = Provider_minimax.messages_to_anthropic_json msgs in
  Alcotest.(check int) "system filtered, 2 remain" 2 (List.length result)

let test_extract_system_present () =
  let msgs =
    [
      Provider.make_message ~role:"system" ~content:"be helpful";
      Provider.make_message ~role:"user" ~content:"hello";
    ]
  in
  Alcotest.(check string)
    "extracted" "be helpful"
    (Provider_minimax.extract_system_prompt msgs)

let test_extract_system_absent () =
  let msgs = [ Provider.make_message ~role:"user" ~content:"hello" ] in
  Alcotest.(check string)
    "empty" ""
    (Provider_minimax.extract_system_prompt msgs)

let test_extract_system_multiple () =
  let msgs =
    [
      Provider.make_message ~role:"system" ~content:"part1";
      Provider.make_message ~role:"system" ~content:"part2";
      Provider.make_message ~role:"user" ~content:"hello";
    ]
  in
  let result = Provider_minimax.extract_system_prompt msgs in
  Alcotest.(check bool) "contains part1" true (String.length result > 0);
  let contains s sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "has part1" true (contains result "part1");
  Alcotest.(check bool) "has part2" true (contains result "part2")

let test_tools_none () =
  Alcotest.(check bool)
    "None -> None" true
    (Provider_minimax.tools_to_anthropic_json None = None)

let test_tools_empty_list () =
  match Provider_minimax.tools_to_anthropic_json (Some (`List [])) with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for empty list"

let test_tools_valid () =
  let tool =
    `Assoc
      [
        ("type", `String "function");
        ( "function",
          `Assoc
            [
              ("name", `String "test_tool");
              ("description", `String "A test tool");
              ( "parameters",
                `Assoc [ ("type", `String "object"); ("properties", `Assoc []) ]
              );
            ] );
      ]
  in
  match Provider_minimax.tools_to_anthropic_json (Some (`List [ tool ])) with
  | Some (`List [ converted ]) ->
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "name" "test_tool"
        (converted |> member "name" |> to_string);
      Alcotest.(check string)
        "desc" "A test tool"
        (converted |> member "description" |> to_string)
  | _ -> Alcotest.fail "expected Some with 1 tool"

let test_parse_text_response () =
  let body =
    {|{"content":[{"type":"text","text":"hello"}],"model":"MiniMax-M2.7","stop_reason":"end_turn","usage":{"input_tokens":10,"output_tokens":5}}|}
  in
  match Provider_minimax.parse_response body "MiniMax-M2.7" with
  | Ok (Provider.Text { content; model; usage }) ->
      Alcotest.(check string) "content" "hello" content;
      Alcotest.(check string) "model" "MiniMax-M2.7" model;
      Alcotest.(check bool) "has usage" true (usage <> None)
  | Ok (Provider.ToolCalls _) -> Alcotest.fail "expected Text"
  | Error e -> Alcotest.fail ("error: " ^ e)

let test_parse_thinking_response () =
  let body =
    {|{"content":[{"type":"thinking","thinking":"inner monologue"},{"type":"text","text":"hello"}],"model":"MiniMax-M2.7","stop_reason":"end_turn","usage":{"input_tokens":10,"output_tokens":5}}|}
  in
  match Provider_minimax.parse_response body "MiniMax-M2.7" with
  | Ok (Provider.Text { content; thinking; _ }) -> (
      Alcotest.(check string) "visible content" "hello" content;
      Alcotest.(check bool) "has thinking" true (thinking <> None);
      match thinking with
      | Some t ->
          Alcotest.(check bool)
            "thinking contains" true
            (try
               ignore
                 (Str.search_forward (Str.regexp_string "inner monologue") t 0);
               true
             with Not_found -> false)
      | None -> ())
  | Ok (Provider.ToolCalls _) -> Alcotest.fail "expected Text"
  | Error e -> Alcotest.fail ("error: " ^ e)

let test_parse_tool_use_response () =
  let body =
    {|{"content":[{"type":"tool_use","id":"call-1","name":"file_read","input":{"path":"/tmp"}}],"model":"MiniMax-M2.7","stop_reason":"tool_use","usage":{"input_tokens":10,"output_tokens":5}}|}
  in
  match Provider_minimax.parse_response body "MiniMax-M2.7" with
  | Ok (Provider.ToolCalls { calls; _ }) ->
      Alcotest.(check int) "1 call" 1 (List.length calls);
      let tc = List.hd calls in
      Alcotest.(check string) "id" "call-1" tc.id;
      Alcotest.(check string) "name" "file_read" tc.function_name
  | Ok (Provider.Text _) -> Alcotest.fail "expected ToolCalls"
  | Error e -> Alcotest.fail ("error: " ^ e)

let test_parse_invalid_json () =
  match Provider_minimax.parse_response "not json" "model" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error"

let test_parse_empty_content () =
  let body =
    {|{"content":[],"model":"MiniMax-M2.7","stop_reason":"end_turn"}|}
  in
  match Provider_minimax.parse_response body "MiniMax-M2.7" with
  | Ok (Provider.Text { content; _ }) ->
      Alcotest.(check string) "empty content" "" content
  | _ -> Alcotest.fail "expected Text with empty content"

let test_parse_ignores_extra_blocks () =
  let body =
    {|{"content":[{"type":"thinking","thinking":"private"},{"type":"text","text":"hello"}],"model":"MiniMax-M2.7","stop_reason":"end_turn"}|}
  in
  match Provider_minimax.parse_response body "MiniMax-M2.7" with
  | Ok (Provider.Text { content; _ }) ->
      Alcotest.(check string) "visible text only" "hello" content
  | _ -> Alcotest.fail "expected Text response"

(* --- Integration tests (call actual MiniMax API) --- *)

let minimax_api_key =
  try Some (Sys.getenv "MINIMAX_API_KEY") with Not_found -> None

let make_test_config () : Runtime_config.t =
  let default = Runtime_config.default in
  match minimax_api_key with
  | Some api_key ->
      let provider_config : Runtime_config.provider_config =
        {
          Runtime_config.default_provider_config with
          api_key;
          base_url = Some "https://api.minimax.io";
          default_model = Some "MiniMax-M2.7-highspeed";
          thinking_budget_tokens = Some 1024;
        }
      in
      {
        default with
        default_provider = Some "minimax";
        providers = [ ("minimax", provider_config) ];
        agent_defaults =
          {
            default.agent_defaults with
            primary_model = "minimax:minimax-m2.7-highspeed";
          };
      }
  | None -> Alcotest.fail "MINIMAX_API_KEY not set"

let test_live_simple_completion () =
  if minimax_api_key = None then Alcotest.skip ();
  let config = make_test_config () in
  let msgs =
    [
      Provider.make_message ~role:"user"
        ~content:"What is 1+1? Answer in 3 words or less.";
    ]
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* result =
       Provider_minimax.complete ~config
         ~provider:(List.assoc "minimax" config.providers)
         ~model:"MiniMax-M2.7" ~messages:msgs ()
     in
     match result with
     | Provider.Text { content; _ } ->
         Alcotest.(check bool) "has content" true (String.length content > 0);
         Alcotest.(check bool)
           "content not empty" true
           (String.trim content <> "");
         Lwt.return_unit
     | Provider.ToolCalls _ -> Alcotest.fail "expected Text, got ToolCalls"
     | exception Lwt_unix.Timeout -> Alcotest.fail "request timed out"
     | exception exn ->
         Alcotest.fail ("request failed: " ^ Printexc.to_string exn))

let test_live_with_system_prompt () =
  if minimax_api_key = None then Alcotest.skip ();
  let config = make_test_config () in
  let msgs =
    [
      Provider.make_message ~role:"system"
        ~content:"You are a helpful assistant that answers briefly.";
      Provider.make_message ~role:"user"
        ~content:"What is the capital of France?";
    ]
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* result =
       Provider_minimax.complete ~config
         ~provider:(List.assoc "minimax" config.providers)
         ~model:"MiniMax-M2.7" ~messages:msgs ()
     in
     match result with
     | Provider.Text { content; _ } ->
         Alcotest.(check bool) "has content" true (String.length content > 0);
         Alcotest.(check bool)
           "content contains Paris" true
           (try
              ignore (Str.search_forward (Str.regexp_string "Paris") content 0);
              true
            with Not_found -> false);
         Lwt.return_unit
     | Provider.ToolCalls _ -> Alcotest.fail "expected Text, got ToolCalls"
     | exception Lwt_unix.Timeout -> Alcotest.fail "request timed out"
     | exception exn ->
         Alcotest.fail ("request failed: " ^ Printexc.to_string exn))

let test_live_thinking_response () =
  if minimax_api_key = None then Alcotest.skip ();
  let config = make_test_config () in
  let msgs =
    [
      Provider.make_message ~role:"user"
        ~content:"Explain why the sky is blue in one sentence.";
    ]
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* result =
       Provider_minimax.complete ~config
         ~provider:(List.assoc "minimax" config.providers)
         ~model:"MiniMax-M2.7" ~messages:msgs ()
     in
     match result with
     | Provider.Text { thinking; _ } ->
         Alcotest.(check bool) "has thinking" true (thinking <> None);
         Lwt.return_unit
     | Provider.ToolCalls _ -> Alcotest.fail "expected Text with thinking"
     | exception Lwt_unix.Timeout -> Alcotest.fail "request timed out"
     | exception exn ->
         Alcotest.fail ("request failed: " ^ Printexc.to_string exn))

let test_live_streaming () =
  if minimax_api_key = None then Alcotest.skip ();
  let config = make_test_config () in
  let msgs =
    [ Provider.make_message ~role:"user" ~content:"Count from 1 to 3." ]
  in
  let chunks = ref [] in
  Lwt_main.run
    (let open Lwt.Syntax in
     let on_chunk chunk =
       chunks := chunk :: !chunks;
       Lwt.return_unit
     in
     let* result =
       Provider_minimax.complete_streaming ~config
         ~provider:(List.assoc "minimax" config.providers)
         ~model:"MiniMax-M2.7" ~messages:msgs ~on_chunk ()
     in
     Alcotest.(check bool) "received chunks" true (List.length !chunks > 0);
     match result with
     | Provider.Text { content; _ } ->
         Alcotest.(check bool) "has content" true (String.length content > 0);
         Lwt.return_unit
     | Provider.ToolCalls _ -> Alcotest.fail "expected Text"
     | exception Lwt_unix.Timeout -> Alcotest.fail "streaming timed out"
     | exception exn ->
         Alcotest.fail ("streaming failed: " ^ Printexc.to_string exn))

let suite =
  [
    Alcotest.test_case "user message" `Quick test_user_message;
    Alcotest.test_case "assistant message" `Quick test_assistant_message;
    Alcotest.test_case "system filtered" `Quick test_system_message_filtered;
    Alcotest.test_case "tool result -> user" `Quick
      test_tool_result_becomes_user;
    Alcotest.test_case "tool result has tool_use_id" `Quick
      test_tool_result_has_tool_use_id;
    Alcotest.test_case "assistant with tool calls" `Quick
      test_assistant_with_tool_calls;
    Alcotest.test_case "mixed messages" `Quick test_mixed_messages;
    Alcotest.test_case "extract system present" `Quick
      test_extract_system_present;
    Alcotest.test_case "extract system absent" `Quick test_extract_system_absent;
    Alcotest.test_case "extract system multiple" `Quick
      test_extract_system_multiple;
    Alcotest.test_case "tools none" `Quick test_tools_none;
    Alcotest.test_case "tools empty list" `Quick test_tools_empty_list;
    Alcotest.test_case "tools valid" `Quick test_tools_valid;
    Alcotest.test_case "parse text response" `Quick test_parse_text_response;
    Alcotest.test_case "parse thinking response" `Quick
      test_parse_thinking_response;
    Alcotest.test_case "parse tool use response" `Quick
      test_parse_tool_use_response;
    Alcotest.test_case "parse invalid json" `Quick test_parse_invalid_json;
    Alcotest.test_case "parse empty content" `Quick test_parse_empty_content;
    Alcotest.test_case "parse ignores extra blocks" `Quick
      test_parse_ignores_extra_blocks;
    Alcotest.test_case "live simple completion" `Slow
      test_live_simple_completion;
    Alcotest.test_case "live with system prompt" `Slow
      test_live_with_system_prompt;
    Alcotest.test_case "live thinking response" `Slow
      test_live_thinking_response;
    Alcotest.test_case "live streaming" `Slow test_live_streaming;
  ]

let test_parse_sse_line_delta () =
  let line = {|data: {"choices":[{"delta":{"content":"Hello"}}]}|} in
  match Provider.parse_sse_line line with
  | Some (`Json _) -> ()
  | _ -> Alcotest.fail "Expected Json from SSE data line"

let test_parse_sse_line_done () =
  let line = "data: [DONE]" in
  match Provider.parse_sse_line line with
  | Some `Done -> ()
  | _ -> Alcotest.fail "Expected Done from [DONE] line"

let test_parse_sse_line_empty () =
  match Provider.parse_sse_line "" with
  | None -> ()
  | _ -> Alcotest.fail "Expected None from empty line"

let test_parse_sse_line_comment () =
  match Provider.parse_sse_line ": keep-alive" with
  | None -> ()
  | _ -> Alcotest.fail "Expected None from comment line"

let test_parse_sse_line_no_prefix () =
  match Provider.parse_sse_line "event: message" with
  | None -> ()
  | _ -> Alcotest.fail "Expected None from non-data line"

let test_process_sse_stream_text () =
  let chunks =
    [
      {|data: {"choices":[{"delta":{"content":"Hello"}}],"model":"test-model"}|}
      ^ "\n\n";
      {|data: {"choices":[{"delta":{"content":" world"}}]}|} ^ "\n\n";
      "data: [DONE]\n\n";
    ]
  in
  let stream = Lwt_stream.of_list chunks in
  let collected = ref [] in
  let on_chunk evt =
    collected := evt :: !collected;
    Lwt.return_unit
  in
  let result = Lwt_main.run (Provider.process_sse_stream stream ~on_chunk) in
  match result with
  | Provider.Text { content; model; _ } ->
      Alcotest.(check string) "accumulated content" "Hello world" content;
      Alcotest.(check string) "model" "test-model" model;
      (* Check that we got Delta events and Done *)
      let events = List.rev !collected in
      let deltas =
        List.filter (function Provider.Delta _ -> true | _ -> false) events
      in
      Alcotest.(check int) "two delta events" 2 (List.length deltas);
      let has_done =
        List.exists (function Provider.Done -> true | _ -> false) events
      in
      Alcotest.(check bool) "has done event" true has_done
  | _ -> Alcotest.fail "Expected Text response"

let test_process_sse_stream_tool_calls () =
  let chunks =
    [
      {|data: {"choices":[{"delta":{"tool_calls":[{"index":0,"id":"call_1","function":{"name":"search","arguments":""}}]}}]}|}
      ^ "\n\n";
      {|data: {"choices":[{"delta":{"tool_calls":[{"index":0,"function":{"arguments":"{\"q\":"}}]}}]}|}
      ^ "\n\n";
      {|data: {"choices":[{"delta":{"tool_calls":[{"index":0,"function":{"arguments":"\"test\"}"}}]}}]}|}
      ^ "\n\n";
      "data: [DONE]\n\n";
    ]
  in
  let stream = Lwt_stream.of_list chunks in
  let result =
    Lwt_main.run
      (Provider.process_sse_stream stream ~on_chunk:(fun _ -> Lwt.return_unit))
  in
  match result with
  | Provider.ToolCalls { calls; _ } ->
      Alcotest.(check int) "one tool call" 1 (List.length calls);
      let tc = List.hd calls in
      Alcotest.(check string) "tool call id" "call_1" tc.id;
      Alcotest.(check string) "function name" "search" tc.function_name;
      Alcotest.(check string) "arguments" {|{"q":"test"}|} tc.arguments
  | _ -> Alcotest.fail "Expected ToolCalls response"

let test_process_sse_stream_tool_calls_backfill_metadata () =
  let sse json = "data: " ^ Yojson.Safe.to_string json ^ "\n\n" in
  let chunks =
    [
      sse
        (`Assoc
           [
             ( "choices",
               `List
                 [
                   `Assoc
                     [
                       ( "delta",
                         `Assoc
                           [
                             ( "tool_calls",
                               `List
                                 [
                                   `Assoc
                                     [
                                       ("index", `Int 0);
                                       ( "function",
                                         `Assoc
                                           [ ("arguments", `String {|{"q":|}) ]
                                       );
                                     ];
                                 ] );
                           ] );
                     ];
                 ] );
           ]);
      sse
        (`Assoc
           [
             ( "choices",
               `List
                 [
                   `Assoc
                     [
                       ( "delta",
                         `Assoc
                           [
                             ( "tool_calls",
                               `List
                                 [
                                   `Assoc
                                     [
                                       ("index", `Int 0);
                                       ("id", `String "call_2");
                                       ( "function",
                                         `Assoc
                                           [
                                             ("name", `String "search");
                                             ("arguments", `String {|"late"}|});
                                           ] );
                                     ];
                                 ] );
                           ] );
                     ];
                 ] );
           ]);
      "data: [DONE]\n\n";
    ]
  in
  let stream = Lwt_stream.of_list chunks in
  let result =
    Lwt_main.run
      (Provider.process_sse_stream stream ~on_chunk:(fun _ -> Lwt.return_unit))
  in
  match result with
  | Provider.ToolCalls { calls; _ } ->
      Alcotest.(check int) "one tool call" 1 (List.length calls);
      let tc = List.hd calls in
      Alcotest.(check string) "tool call id" "call_2" tc.id;
      Alcotest.(check string) "function name" "search" tc.function_name;
      Alcotest.(check string) "arguments" {|{"q":"late"}|} tc.arguments
  | _ -> Alcotest.fail "Expected ToolCalls response"

let test_process_sse_stream_partial_chunks () =
  (* Simulate data split across chunk boundaries *)
  let chunks =
    [
      "data: {\"choices\":[{\"del";
      "ta\":{\"content\":\"Hi\"}}]}\n\ndata: [DONE]\n\n";
    ]
  in
  let stream = Lwt_stream.of_list chunks in
  let result =
    Lwt_main.run
      (Provider.process_sse_stream stream ~on_chunk:(fun _ -> Lwt.return_unit))
  in
  match result with
  | Provider.Text { content; _ } ->
      Alcotest.(check string) "content from partial chunks" "Hi" content
  | _ -> Alcotest.fail "Expected Text response"

let test_process_sse_stream_reasoning_content () =
  let chunks =
    [
      {|data: {"choices":[{"delta":{"reasoning_content":"plan first"}}]}|}
      ^ "\n\n";
      {|data: {"choices":[{"delta":{"content":"final answer"}}]}|} ^ "\n\n";
      "data: [DONE]\n\n";
    ]
  in
  let stream = Lwt_stream.of_list chunks in
  let thinking = Buffer.create 32 in
  let visible = Buffer.create 32 in
  let on_chunk = function
    | Provider.ThinkingDelta text ->
        Buffer.add_string thinking text;
        Lwt.return_unit
    | Provider.Delta text ->
        Buffer.add_string visible text;
        Lwt.return_unit
    | _ -> Lwt.return_unit
  in
  let result =
    Lwt_main.run
      (Provider.process_sse_stream ~thinking_style:Provider.ReasoningContent
         stream ~on_chunk)
  in
  Alcotest.(check string)
    "thinking delta" "plan first" (Buffer.contents thinking);
  Alcotest.(check string)
    "visible delta" "final answer" (Buffer.contents visible);
  match result with
  | Provider.Text { content; _ } ->
      Alcotest.(check string) "final content" "final answer" content
  | _ -> Alcotest.fail "Expected Text response"

let test_process_sse_stream_tagged_thinking () =
  let chunks =
    [
      {|data: {"choices":[{"delta":{"content":"<thi"}}]}|} ^ "\n\n";
      {|data: {"choices":[{"delta":{"content":"nk>plan"}}]}|} ^ "\n\n";
      {|data: {"choices":[{"delta":{"content":"</think>visible"}}]}|} ^ "\n\n";
      "data: [DONE]\n\n";
    ]
  in
  let stream = Lwt_stream.of_list chunks in
  let thinking = Buffer.create 32 in
  let visible = Buffer.create 32 in
  let on_chunk = function
    | Provider.ThinkingDelta text ->
        Buffer.add_string thinking text;
        Lwt.return_unit
    | Provider.Delta text ->
        Buffer.add_string visible text;
        Lwt.return_unit
    | _ -> Lwt.return_unit
  in
  let result =
    Lwt_main.run
      (Provider.process_sse_stream ~thinking_style:Provider.TaggedThinking
         stream ~on_chunk)
  in
  Alcotest.(check string) "tagged thinking" "plan" (Buffer.contents thinking);
  Alcotest.(check string) "tagged visible" "visible" (Buffer.contents visible);
  match result with
  | Provider.Text { content; _ } ->
      Alcotest.(check string) "tagged final content" "visible" content
  | _ -> Alcotest.fail "Expected Text response"

let test_provider_config_default_model () =
  let config : Runtime_config.t =
    {
      Runtime_config.default with
      providers =
        [
          ( "test",
            {
              Runtime_config.default_provider_config with
              api_key = "sk-test";
              base_url = Some "http://localhost";
              default_model = Some "custom-model";
            } );
        ];
      default_provider = Some "test";
    }
  in
  let json = Runtime_config.to_json config in
  let s = Yojson.Safe.to_string json in
  Alcotest.(check bool)
    "contains default_model" true
    (try
       ignore (Str.search_forward (Str.regexp_string "custom-model") s 0);
       true
     with Not_found -> false)

let test_select_provider_prefers_colon_model_provider () =
  let config : Runtime_config.t =
    {
      Runtime_config.default with
      providers =
        [
          ( "groq",
            {
              Runtime_config.default_provider_config with
              api_key = "sk-groq";
              base_url = Some "https://api.groq.com/openai/v1";
              default_model = Some "llama-3.3-70b-versatile";
            } );
          ( "zai_coding",
            {
              Runtime_config.default_provider_config with
              api_key = "sk-zai";
              base_url = Some "https://api.z.ai/api/coding/paas/v4";
              default_model = Some "glm-5";
            } );
        ];
      default_provider = Some "groq";
      agent_defaults =
        {
          Runtime_config.default.agent_defaults with
          primary_model = "zai_coding:glm-5";
        };
    }
  in
  let provider_name, _, model = Provider.select_provider ~config () in
  Alcotest.(check string)
    "provider chosen from model target" "zai_coding" provider_name;
  Alcotest.(check string) "model parsed from colon target" "glm-5" model

let test_select_provider_keeps_raw_model_when_target_provider_missing () =
  let config : Runtime_config.t =
    {
      Runtime_config.default with
      providers =
        [
          ( "groq",
            {
              Runtime_config.default_provider_config with
              api_key = "sk-groq";
              base_url = Some "https://api.groq.com/openai/v1";
            } );
        ];
      default_provider = Some "groq";
      agent_defaults =
        {
          Runtime_config.default.agent_defaults with
          primary_model = "zai_coding:glm-5";
        };
    }
  in
  let provider_name, _, model = Provider.select_provider ~config () in
  Alcotest.(check string) "fallback provider selected" "groq" provider_name;
  Alcotest.(check string) "raw model preserved" "zai_coding:glm-5" model

let test_codex_stream_no_duplicate_tool_calls () =
  (* Simulate OpenAI Codex Responses API streaming with 3 tool calls.
     Event sequence: 3x output_item.added, 3x argument deltas,
     3x output_item.done, then response.completed.
     Bug B071: output_index was read from item (always missing) instead of json,
     causing done events to create duplicate entries with empty args. *)
  let sse json = "data: " ^ Yojson.Safe.to_string json ^ "\n\n" in
  let mk_item idx call_id name =
    `Assoc
      [
        ("type", `String "function_call");
        ("call_id", `String call_id);
        ("name", `String name);
      ]
    |> fun item ->
    ( idx,
      item,
      `Assoc
        [
          ("type", `String "response.output_item.added");
          ("output_index", `Int idx);
          ("item", item);
        ] )
  in
  let mk_delta idx delta =
    `Assoc
      [
        ("type", `String "response.function_call_arguments.delta");
        ("output_index", `Int idx);
        ("delta", `String delta);
      ]
  in
  let mk_done idx call_id name args =
    `Assoc
      [
        ("type", `String "response.output_item.done");
        ("output_index", `Int idx);
        ( "item",
          `Assoc
            [
              ("type", `String "function_call");
              ("call_id", `String call_id);
              ("name", `String name);
              ("arguments", `String args);
            ] );
      ]
  in
  let _, _, added0 = mk_item 0 "call_a" "shell_exec" in
  let _, _, added1 = mk_item 1 "call_b" "file_read" in
  let _, _, added2 = mk_item 2 "call_c" "memory_store" in
  let chunks =
    [
      sse added0;
      sse added1;
      sse added2;
      sse (mk_delta 0 {|{"command":"|});
      sse (mk_delta 0 {|ls"}|});
      sse (mk_delta 1 {|{"path":"foo.ml"}|});
      sse (mk_delta 2 {|{"key":"k","value":"v"}|});
      sse (mk_done 0 "call_a" "shell_exec" {|{"command":"ls"}|});
      sse (mk_done 1 "call_b" "file_read" {|{"path":"foo.ml"}|});
      sse (mk_done 2 "call_c" "memory_store" {|{"key":"k","value":"v"}|});
      sse
        (`Assoc
           [
             ("type", `String "response.completed");
             ( "response",
               `Assoc
                 [
                   ("model", `String "codex-mini");
                   ( "usage",
                     `Assoc
                       [
                         ("input_tokens", `Int 100); ("output_tokens", `Int 50);
                       ] );
                   ( "output",
                     `List
                       [
                         `Assoc
                           [
                             ("type", `String "function_call");
                             ("call_id", `String "call_a");
                             ("name", `String "shell_exec");
                             ("arguments", `String {|{"command":"ls"}|});
                           ];
                         `Assoc
                           [
                             ("type", `String "function_call");
                             ("call_id", `String "call_b");
                             ("name", `String "file_read");
                             ("arguments", `String {|{"path":"foo.ml"}|});
                           ];
                         `Assoc
                           [
                             ("type", `String "function_call");
                             ("call_id", `String "call_c");
                             ("name", `String "memory_store");
                             ("arguments", `String {|{"key":"k","value":"v"}|});
                           ];
                       ] );
                 ] );
           ]);
    ]
  in
  let stream = Lwt_stream.of_list chunks in
  let result =
    Lwt_main.run
      (Provider_openai_codex.process_stream stream ~on_chunk:(fun _ ->
           Lwt.return_unit))
  in
  match result with
  | Provider.ToolCalls { calls; model; _ } ->
      Alcotest.(check int)
        "exactly 3 tool calls (no duplicates)" 3 (List.length calls);
      Alcotest.(check string) "model" "codex-mini" model;
      let tc0 = List.nth calls 0 in
      Alcotest.(check string) "tc0 id" "call_a" tc0.id;
      Alcotest.(check string) "tc0 name" "shell_exec" tc0.function_name;
      Alcotest.(check string) "tc0 args" {|{"command":"ls"}|} tc0.arguments;
      let tc1 = List.nth calls 1 in
      Alcotest.(check string) "tc1 id" "call_b" tc1.id;
      Alcotest.(check string) "tc1 name" "file_read" tc1.function_name;
      Alcotest.(check string) "tc1 args" {|{"path":"foo.ml"}|} tc1.arguments;
      let tc2 = List.nth calls 2 in
      Alcotest.(check string) "tc2 id" "call_c" tc2.id;
      Alcotest.(check string) "tc2 name" "memory_store" tc2.function_name;
      Alcotest.(check string)
        "tc2 args" {|{"key":"k","value":"v"}|} tc2.arguments
  | Provider.Text { content; _ } ->
      Alcotest.fail
        (Printf.sprintf "Expected ToolCalls but got Text: %s" content)

let test_codex_stream_backfill_only_missing () =
  (* Test that response.completed only backfills tool calls that were NOT
     already populated by streaming deltas. *)
  let sse json = "data: " ^ Yojson.Safe.to_string json ^ "\n\n" in
  let chunks =
    [
      (* Tool at index 0 gets streamed args *)
      sse
        (`Assoc
           [
             ("type", `String "response.output_item.added");
             ("output_index", `Int 0);
             ( "item",
               `Assoc
                 [
                   ("type", `String "function_call");
                   ("call_id", `String "call_x");
                   ("name", `String "file_read");
                 ] );
           ]);
      sse
        (`Assoc
           [
             ("type", `String "response.function_call_arguments.delta");
             ("output_index", `Int 0);
             ("delta", `String {|{"path":"streamed.ml"}|});
           ]);
      (* response.completed includes both tools — index 0 already has args,
         index 1 was never streamed so should be backfilled *)
      sse
        (`Assoc
           [
             ("type", `String "response.completed");
             ( "response",
               `Assoc
                 [
                   ("model", `String "codex-mini");
                   ( "usage",
                     `Assoc
                       [ ("input_tokens", `Int 10); ("output_tokens", `Int 5) ]
                   );
                   ( "output",
                     `List
                       [
                         `Assoc
                           [
                             ("type", `String "function_call");
                             ("call_id", `String "call_x");
                             ("name", `String "file_read");
                             ("arguments", `String {|{"path":"fallback.ml"}|});
                           ];
                         `Assoc
                           [
                             ("type", `String "function_call");
                             ("call_id", `String "call_y");
                             ("name", `String "shell_exec");
                             ("arguments", `String {|{"command":"pwd"}|});
                           ];
                       ] );
                 ] );
           ]);
    ]
  in
  let stream = Lwt_stream.of_list chunks in
  let result =
    Lwt_main.run
      (Provider_openai_codex.process_stream stream ~on_chunk:(fun _ ->
           Lwt.return_unit))
  in
  match result with
  | Provider.ToolCalls { calls; _ } ->
      Alcotest.(check int) "2 tool calls" 2 (List.length calls);
      let tc0 = List.nth calls 0 in
      (* Index 0 should keep streamed args, NOT fallback *)
      Alcotest.(check string)
        "tc0 keeps streamed args" {|{"path":"streamed.ml"}|} tc0.arguments;
      let tc1 = List.nth calls 1 in
      (* Index 1 should be backfilled from response.completed *)
      Alcotest.(check string) "tc1 backfilled id" "call_y" tc1.id;
      Alcotest.(check string)
        "tc1 backfilled name" "shell_exec" tc1.function_name;
      Alcotest.(check string)
        "tc1 backfilled args" {|{"command":"pwd"}|} tc1.arguments
  | _ -> Alcotest.fail "Expected ToolCalls response"

let test_codex_message_to_input_replays_raw_output_items () =
  let raw_items =
    {|[{"type":"reasoning","id":"rs_1"},{"type":"function_call","call_id":"call_1","name":"bash","arguments":"{}"}]|}
  in
  let msg =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls = [];
      tool_call_id = None;
      name = None;
      provider_response_items_json = Some raw_items;
    }
  in
  match Provider_openai_codex.message_to_input msg with
  | Some (`List items) ->
      Alcotest.(check int) "replays all raw items" 2 (List.length items);
      Alcotest.(check string)
        "first item is reasoning" "reasoning"
        Yojson.Safe.Util.(items |> List.hd |> member "type" |> to_string)
  | _ -> Alcotest.fail "Expected raw output items to be replayed"

let test_codex_stream_preserves_response_output_items () =
  let sse json = "data: " ^ Yojson.Safe.to_string json ^ "\n\n" in
  let chunks =
    [
      sse
        (`Assoc
           [
             ("type", `String "response.completed");
             ( "response",
               `Assoc
                 [
                   ("model", `String "codex-mini");
                   ( "usage",
                     `Assoc
                       [ ("input_tokens", `Int 10); ("output_tokens", `Int 5) ]
                   );
                   ( "output",
                     `List
                       [
                         `Assoc
                           [
                             ("type", `String "reasoning");
                             ("id", `String "rs_1");
                           ];
                         `Assoc
                           [
                             ("type", `String "function_call");
                             ("call_id", `String "call_x");
                             ("name", `String "file_read");
                             ("arguments", `String {|{"path":"foo.ml"}|});
                           ];
                       ] );
                 ] );
           ]);
    ]
  in
  let stream = Lwt_stream.of_list chunks in
  let result =
    Lwt_main.run
      (Provider_openai_codex.process_stream stream ~on_chunk:(fun _ ->
           Lwt.return_unit))
  in
  match result with
  | Provider.ToolCalls { provider_response_items_json = Some raw; _ } ->
      let items = Yojson.Safe.from_string raw |> Yojson.Safe.Util.to_list in
      Alcotest.(check int) "raw output item count" 2 (List.length items)
  | _ -> Alcotest.fail "Expected preserved provider response items"

let test_codex_build_body_drops_orphan_tool_outputs () =
  let body =
    Provider_openai_codex.build_body ~model:"gpt-5"
      ~messages:
        [
          Provider.make_message ~role:"system" ~content:"sys";
          Provider.make_tool_result ~tool_call_id:"tc_missing" ~name:"bash"
            ~content:"orphan";
          Provider.make_message ~role:"user" ~content:"hello";
        ]
      None
  in
  let json = Yojson.Safe.from_string body in
  let input = Yojson.Safe.Util.(json |> member "input" |> to_list) in
  Alcotest.(check int) "orphan tool output omitted" 1 (List.length input)

let suite =
  [
    Alcotest.test_case "SSE parse delta line" `Quick test_parse_sse_line_delta;
    Alcotest.test_case "SSE parse done line" `Quick test_parse_sse_line_done;
    Alcotest.test_case "SSE parse empty line" `Quick test_parse_sse_line_empty;
    Alcotest.test_case "SSE parse comment line" `Quick
      test_parse_sse_line_comment;
    Alcotest.test_case "SSE parse non-data line" `Quick
      test_parse_sse_line_no_prefix;
    Alcotest.test_case "SSE stream text" `Quick test_process_sse_stream_text;
    Alcotest.test_case "SSE stream tool calls" `Quick
      test_process_sse_stream_tool_calls;
    Alcotest.test_case "SSE stream tool calls backfill metadata" `Quick
      test_process_sse_stream_tool_calls_backfill_metadata;
    Alcotest.test_case "SSE stream partial chunks" `Quick
      test_process_sse_stream_partial_chunks;
    Alcotest.test_case "SSE reasoning_content thinking" `Quick
      test_process_sse_stream_reasoning_content;
    Alcotest.test_case "SSE tagged thinking" `Quick
      test_process_sse_stream_tagged_thinking;
    Alcotest.test_case "provider config default_model" `Quick
      test_provider_config_default_model;
    Alcotest.test_case "select provider with colon target" `Quick
      test_select_provider_prefers_colon_model_provider;
    Alcotest.test_case "preserve raw model when provider missing" `Quick
      test_select_provider_keeps_raw_model_when_target_provider_missing;
    Alcotest.test_case "codex stream no duplicate tool calls" `Quick
      test_codex_stream_no_duplicate_tool_calls;
    Alcotest.test_case "codex stream backfill only missing" `Quick
      test_codex_stream_backfill_only_missing;
    Alcotest.test_case "codex message replays raw output items" `Quick
      test_codex_message_to_input_replays_raw_output_items;
    Alcotest.test_case "codex stream preserves response output items" `Quick
      test_codex_stream_preserves_response_output_items;
    Alcotest.test_case "codex build body drops orphan tool outputs" `Quick
      test_codex_build_body_drops_orphan_tool_outputs;
  ]

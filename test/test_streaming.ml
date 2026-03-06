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

let test_provider_config_default_model () =
  let config : Runtime_config.t =
    {
      Runtime_config.default with
      providers =
        [
          ( "test",
            {
              api_key = "sk-test";
              kind = None;
              base_url = Some "http://localhost";
              default_model = Some "custom-model";
              project_id = None;
              location = None;
              service_account_json = None;
              codex_oauth = None;
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
              api_key = "sk-groq";
              kind = None;
              base_url = Some "https://api.groq.com/openai/v1";
              default_model = Some "llama-3.3-70b-versatile";
              project_id = None;
              location = None;
              service_account_json = None;
              codex_oauth = None;
            } );
          ( "zai_coding",
            {
              api_key = "sk-zai";
              kind = None;
              base_url = Some "https://api.z.ai/api/coding/paas/v4";
              default_model = Some "glm-5";
              project_id = None;
              location = None;
              service_account_json = None;
              codex_oauth = None;
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
  let provider_name, _, model = Provider.select_provider ~config in
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
              api_key = "sk-groq";
              kind = None;
              base_url = Some "https://api.groq.com/openai/v1";
              default_model = None;
              project_id = None;
              location = None;
              service_account_json = None;
              codex_oauth = None;
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
  let provider_name, _, model = Provider.select_provider ~config in
  Alcotest.(check string) "fallback provider selected" "groq" provider_name;
  Alcotest.(check string) "raw model preserved" "zai_coding:glm-5" model

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
    Alcotest.test_case "SSE stream partial chunks" `Quick
      test_process_sse_stream_partial_chunks;
    Alcotest.test_case "provider config default_model" `Quick
      test_provider_config_default_model;
    Alcotest.test_case "select provider with colon target" `Quick
      test_select_provider_prefers_colon_model_provider;
    Alcotest.test_case "preserve raw model when provider missing" `Quick
      test_select_provider_keeps_raw_model_when_target_provider_missing;
  ]

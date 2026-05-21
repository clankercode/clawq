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

let test_consecutive_tool_results_coalesce () =
  (* Regression for MiniMax error 2013: when an assistant turn issues N
     tool_uses, the N tool_result blocks must arrive in a single user turn,
     not split across N separate user messages. *)
  let assistant =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls =
        [
          {
            Provider.id = "tc-a";
            function_name = "file_read";
            arguments = {|{"path":"/a"}|};
          };
          {
            Provider.id = "tc-b";
            function_name = "shell_exec";
            arguments = {|{"command":"ls"}|};
          };
        ];
      tool_call_id = None;
      name = None;
      provider_response_items_json = None;
      thinking = None;
    }
  in
  let r1 =
    Provider.make_tool_result ~tool_call_id:"tc-a" ~name:"file_read"
      ~content:"contents-a"
  in
  let r2 =
    Provider.make_tool_result ~tool_call_id:"tc-b" ~name:"shell_exec"
      ~content:"contents-b"
  in
  let result =
    Provider_minimax.messages_to_anthropic_json [ assistant; r1; r2 ]
  in
  Alcotest.(check int) "2 messages (assistant + 1 user)" 2 (List.length result);
  let open Yojson.Safe.Util in
  let user = List.nth result 1 in
  Alcotest.(check string) "role" "user" (user |> member "role" |> to_string);
  let blocks = user |> member "content" |> to_list in
  Alcotest.(check int) "2 tool_result blocks" 2 (List.length blocks);
  let ids = List.map (fun b -> b |> member "tool_use_id" |> to_string) blocks in
  Alcotest.(check (list string)) "ids in order" [ "tc-a"; "tc-b" ] ids

let test_user_text_between_tool_use_and_result_is_reordered () =
  (* Regression: when a user-correction or queued-message injection lands
     between an assistant tool_use and the pending tool_result, the converter
     must move the tool_result adjacent to its tool_use so MiniMax's strict
     adjacency requirement is satisfied. *)
  let assistant =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls =
        [
          {
            Provider.id = "tc-x";
            function_name = "shell_exec";
            arguments = {|{"command":"ls"}|};
          };
        ];
      tool_call_id = None;
      name = None;
      provider_response_items_json = None;
      thinking = None;
    }
  in
  let injected =
    Provider.make_message ~role:"user" ~content:"a new message arrived"
  in
  let tr =
    Provider.make_tool_result ~tool_call_id:"tc-x" ~name:"shell_exec"
      ~content:"done"
  in
  let result =
    Provider_minimax.messages_to_anthropic_json [ assistant; injected; tr ]
  in
  let open Yojson.Safe.Util in
  Alcotest.(check int) "3 messages" 3 (List.length result);
  let roles = List.map (fun m -> m |> member "role" |> to_string) result in
  Alcotest.(check (list string))
    "assistant/user-tool/user-text"
    [ "assistant"; "user"; "user" ]
    roles;
  let second = List.nth result 1 in
  let block = List.hd (second |> member "content" |> to_list) in
  Alcotest.(check string)
    "tool_result block first" "tool_result"
    (block |> member "type" |> to_string);
  let third = List.nth result 2 in
  Alcotest.(check string)
    "text content last" "a new message arrived"
    (third |> member "content" |> to_string)

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

(* B608 regression: Anthropic-compatible APIs report input_tokens as NEW
   (uncached) and cache_read_input_tokens separately. The provider must
   normalize to OpenAI-style total-prompt semantics (pt = new + cached) so
   downstream cost calculation and the cache-hit log are correct. *)
let test_parse_response_normalizes_cached_to_total () =
  let body =
    {|{"content":[{"type":"text","text":"hi"}],"model":"MiniMax-M2.7","stop_reason":"end_turn","usage":{"input_tokens":120,"output_tokens":50,"cache_read_input_tokens":35835}}|}
  in
  match Provider_minimax.parse_response body "MiniMax-M2.7" with
  | Ok (Provider.Text { usage = Some (pt, ct, cached); _ }) ->
      Alcotest.(check int)
        "pt is total (new + cached), not just new" (120 + 35835) pt;
      Alcotest.(check int) "ct unchanged" 50 ct;
      Alcotest.(check int) "cached unchanged" 35835 cached;
      Alcotest.(check bool) "cached <= pt invariant holds" true (cached <= pt)
  | Ok (Provider.Text { usage = None; _ }) -> Alcotest.fail "expected usage"
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

let test_api_model_name_canonicalizes_catalog_aliases () =
  Alcotest.(check string)
    "M2.7 alias" "MiniMax-M2.7"
    (Provider_minimax.api_model_name "minimax-m2.7");
  Alcotest.(check string)
    "M2.7 highspeed alias" "MiniMax-M2.7-highspeed"
    (Provider_minimax.api_model_name "minimax-m2.7-highspeed");
  Alcotest.(check string)
    "official casing preserved" "MiniMax-M2.5"
    (Provider_minimax.api_model_name "MiniMax-M2.5");
  Alcotest.(check string)
    "custom model preserved" "custom-minimax-model"
    (Provider_minimax.api_model_name "custom-minimax-model")

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

(* B620 regression: when session resume produces an assistant turn with
   tool_use blocks whose results were dropped, the converter must filter the
   orphan tool_use so MiniMax doesn't reject the request with HTTP 400 (error
   2013: "tool call result does not follow tool call"). The fix lives in
   Provider_minimax.complete (via Message_history.ensure_tool_group_integrity)
   so the test exercises that pipeline end-to-end. *)
let test_orphan_tool_use_filtered_before_send () =
  let assistant_with_three_tools =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls =
        [
          {
            Provider.id = "tc-A";
            function_name = "shell_exec";
            arguments = {|{"command":"ls"}|};
          };
          {
            Provider.id = "tc-B";
            function_name = "shell_exec";
            arguments = {|{"command":"pwd"}|};
          };
          {
            Provider.id = "tc-C";
            function_name = "shell_exec";
            arguments = {|{"command":"echo hi"}|};
          };
        ];
      tool_call_id = None;
      name = None;
      provider_response_items_json = None;
      thinking = None;
    }
  in
  let result_a =
    Provider.make_tool_result ~tool_call_id:"tc-A" ~name:"shell_exec"
      ~content:"a.txt b.txt"
  in
  let result_b =
    Provider.make_tool_result ~tool_call_id:"tc-B" ~name:"shell_exec"
      ~content:"/home"
  in
  (* tc-C was interrupted; no matching tool_result exists. *)
  let messages = [ assistant_with_three_tools; result_a; result_b ] in
  let cleaned = Message_history.ensure_tool_group_integrity messages in
  let anthropic_messages =
    Provider_minimax.messages_to_anthropic_json cleaned
  in
  let open Yojson.Safe.Util in
  Alcotest.(check int)
    "2 messages: assistant + user-with-tool-results" 2
    (List.length anthropic_messages);
  let assistant = List.nth anthropic_messages 0 in
  Alcotest.(check string)
    "first is assistant" "assistant"
    (assistant |> member "role" |> to_string);
  let assistant_blocks = assistant |> member "content" |> to_list in
  let tool_use_ids =
    List.filter_map
      (fun b ->
        try
          if b |> member "type" |> to_string = "tool_use" then
            Some (b |> member "id" |> to_string)
          else None
        with _ -> None)
      assistant_blocks
  in
  Alcotest.(check (list string))
    "orphan tool_use tc-C filtered; tc-A and tc-B remain" [ "tc-A"; "tc-B" ]
    tool_use_ids;
  let user = List.nth anthropic_messages 1 in
  Alcotest.(check string)
    "second is user" "user"
    (user |> member "role" |> to_string);
  let user_blocks = user |> member "content" |> to_list in
  let tool_result_ids =
    List.filter_map
      (fun b ->
        try
          if b |> member "type" |> to_string = "tool_result" then
            Some (b |> member "tool_use_id" |> to_string)
          else None
        with _ -> None)
      user_blocks
  in
  Alcotest.(check (list string))
    "tool_results in order match remaining tool_use ids" [ "tc-A"; "tc-B" ]
    tool_result_ids

(* B614 integration: send a real request to MiniMax with a tool that has
   required:["query"] and verify the model honors the required field by either
   (a) emitting a tool_call that includes the required argument, or (b)
   returning text that asks for the missing argument. The model must NOT
   silently omit the required argument from a tool_call. *)
let test_live_required_field_honored () =
  if minimax_api_key = None then Alcotest.skip ();
  let config = make_test_config () in
  let search_tool =
    `Assoc
      [
        ("type", `String "function");
        ( "function",
          `Assoc
            [
              ("name", `String "lookup_capital");
              ( "description",
                `String
                  "Look up the capital city of a country. The 'country' \
                   parameter is required and must be a non-empty string." );
              ( "parameters",
                `Assoc
                  [
                    ("type", `String "object");
                    ( "properties",
                      `Assoc
                        [
                          ( "country",
                            `Assoc
                              [
                                ("type", `String "string");
                                ( "description",
                                  `String
                                    "ISO 3166-1 country name to look up the \
                                     capital for. Required." );
                              ] );
                        ] );
                    ("required", `List [ `String "country" ]);
                    ("additionalProperties", `Bool false);
                  ] );
            ] );
      ]
  in
  let tools = `List [ search_tool ] in
  let msgs =
    [
      Provider.make_message ~role:"system"
        ~content:
          "Use the lookup_capital tool to answer questions about capital \
           cities.";
      Provider.make_message ~role:"user"
        ~content:"What is the capital of France?";
    ]
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* result =
       Provider_minimax.complete ~config
         ~provider:(List.assoc "minimax" config.providers)
         ~model:"MiniMax-M2.7" ~messages:msgs ~tools ()
     in
     match result with
     | Provider.ToolCalls { calls; _ } ->
         Alcotest.(check bool)
           "at least one tool call" true
           (List.length calls > 0);
         let call = List.hd calls in
         Alcotest.(check string)
           "tool called is lookup_capital" "lookup_capital" call.function_name;
         let args =
           try Yojson.Safe.from_string call.arguments with _ -> `Assoc []
         in
         let open Yojson.Safe.Util in
         let country =
           try args |> member "country" |> to_string with _ -> ""
         in
         Alcotest.(check bool)
           "required 'country' argument present and non-empty" true
           (String.trim country <> "");
         Lwt.return_unit
     | Provider.Text _ ->
         (* Direct text response without a tool call is a failure: we
            instructed the model to use the tool. If it refuses, that
            indicates the tool definition (or its required[]) was not
            communicated correctly to the model. *)
         Alcotest.fail
           "expected ToolCalls with required 'country' arg; got Text. The \
            model bypassed the tool, which suggests the tool schema (required \
            fields, description) was not visible."
     | exception Lwt_unix.Timeout -> Alcotest.fail "request timed out"
     | exception exn ->
         Alcotest.fail ("request failed: " ^ Printexc.to_string exn))

(* Capture the exact request body MiniMax receives. We don't actually send the
   request; we exercise the same converter pipeline and assert on the JSON the
   converter would produce. This is the contract test cx-reviewer flagged. *)
let test_request_body_has_required_field_for_anthropic_tools () =
  let tool =
    `Assoc
      [
        ("type", `String "function");
        ( "function",
          `Assoc
            [
              ("name", `String "memory_forget");
              ("description", `String "Remove a memory by key");
              ( "parameters",
                `Assoc
                  [
                    ("type", `String "object");
                    ( "properties",
                      `Assoc
                        [
                          ( "key",
                            `Assoc
                              [
                                ("type", `String "string");
                                ( "description",
                                  `String "Memory key to remove (required)" );
                              ] );
                        ] );
                    ("required", `List [ `String "key" ]);
                  ] );
            ] );
      ]
  in
  let converted =
    Provider_minimax.tools_to_anthropic_json (Some (`List [ tool ]))
  in
  match converted with
  | Some (`List [ entry ]) ->
      let open Yojson.Safe.Util in
      let schema = entry |> member "input_schema" in
      let required =
        try schema |> member "required" |> to_list |> List.map to_string
        with _ -> []
      in
      Alcotest.(check (list string))
        "input_schema.required preserves keys from openai schema" [ "key" ]
        required;
      let props =
        try schema |> member "properties" |> to_assoc with _ -> []
      in
      Alcotest.(check bool)
        "input_schema.properties has 'key'" true
        (List.mem_assoc "key" props)
  | _ -> Alcotest.fail "expected exactly one converted entry"

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
    Alcotest.test_case "consecutive tool results coalesce" `Quick
      test_consecutive_tool_results_coalesce;
    Alcotest.test_case "user text between tool_use and result is reordered"
      `Quick test_user_text_between_tool_use_and_result_is_reordered;
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
    Alcotest.test_case
      "B608: parse_response normalizes cached to total prompt tokens" `Quick
      test_parse_response_normalizes_cached_to_total;
    Alcotest.test_case "parse thinking response" `Quick
      test_parse_thinking_response;
    Alcotest.test_case "parse tool use response" `Quick
      test_parse_tool_use_response;
    Alcotest.test_case "parse invalid json" `Quick test_parse_invalid_json;
    Alcotest.test_case "parse empty content" `Quick test_parse_empty_content;
    Alcotest.test_case "parse ignores extra blocks" `Quick
      test_parse_ignores_extra_blocks;
    Alcotest.test_case "api model name canonicalizes catalog aliases" `Quick
      test_api_model_name_canonicalizes_catalog_aliases;
    Alcotest.test_case "live simple completion" `Slow
      test_live_simple_completion;
    Alcotest.test_case "live with system prompt" `Slow
      test_live_with_system_prompt;
    Alcotest.test_case "live thinking response" `Slow
      test_live_thinking_response;
    Alcotest.test_case "live streaming" `Slow test_live_streaming;
    Alcotest.test_case "B620: orphan tool_use filtered before send" `Quick
      test_orphan_tool_use_filtered_before_send;
    Alcotest.test_case "B614: required-field anthropic input_schema preserved"
      `Quick test_request_body_has_required_field_for_anthropic_tools;
    Alcotest.test_case "B614: live required-field honored by model" `Slow
      test_live_required_field_honored;
  ]

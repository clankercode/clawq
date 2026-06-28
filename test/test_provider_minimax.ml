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

(* Helper: construct a paired assistant(tool_use) + tool_result sequence so
   the strict-pairing converter (B644) accepts it. *)
let mk_paired ~tc_id ~tool_name ~tool_args ~result_content =
  let tc =
    { Provider.id = tc_id; function_name = tool_name; arguments = tool_args }
  in
  let asst =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls = [ tc ];
      tool_call_id = None;
      name = None;
      provider_response_items_json = None;
      thinking = None;
      is_error = false;
    }
  in
  let res =
    Provider.make_tool_result ~tool_call_id:tc_id ~name:tool_name
      ~content:result_content
  in
  [ asst; res ]

let test_tool_result_becomes_user () =
  let msgs =
    mk_paired ~tc_id:"tc1" ~tool_name:"bash" ~tool_args:{|{"x":1}|}
      ~result_content:"done"
  in
  let result = Provider_minimax.messages_to_anthropic_json msgs in
  (* Result is [assistant_tool_use; user_with_tool_result]. *)
  let user = List.nth result 1 in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "tool result role=user" "user"
    (user |> member "role" |> to_string)

let test_tool_result_has_tool_use_id () =
  let msgs =
    mk_paired ~tc_id:"tc1" ~tool_name:"bash" ~tool_args:{|{"x":1}|}
      ~result_content:"done"
  in
  let result = Provider_minimax.messages_to_anthropic_json msgs in
  let user = List.nth result 1 in
  let open Yojson.Safe.Util in
  let content = user |> member "content" |> to_list in
  Alcotest.(check int) "1 content block" 1 (List.length content);
  let block = List.hd content in
  Alcotest.(check string)
    "type" "tool_result"
    (block |> member "type" |> to_string);
  Alcotest.(check string)
    "tool_use_id" "tc1"
    (block |> member "tool_use_id" |> to_string)

let test_assistant_with_tool_calls () =
  let msgs =
    mk_paired ~tc_id:"call-1" ~tool_name:"file_read"
      ~tool_args:{|{"path":"/tmp"}|} ~result_content:"contents"
  in
  let result = Provider_minimax.messages_to_anthropic_json msgs in
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

(* B644 regression: a standalone tool_result with no matching assistant
   tool_use must be dropped before reaching MiniMax (avoid error 2013). *)
let test_b644_drops_orphan_tool_result () =
  let orphan =
    Provider.make_tool_result ~tool_call_id:"missing" ~name:"bash"
      ~content:"stale"
  in
  let result = Provider_minimax.messages_to_anthropic_json [ orphan ] in
  Alcotest.(check int) "orphan dropped" 0 (List.length result)

(* B644 regression: an assistant tool_use with no matching tool_result must
   also be dropped before reaching MiniMax. *)
let test_b644_drops_unfollowed_tool_use () =
  let tc =
    {
      Provider.id = "lonely";
      function_name = "shell_exec";
      arguments = {|{"command":"ls"}|};
    }
  in
  let asst =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls = [ tc ];
      tool_call_id = None;
      name = None;
      provider_response_items_json = None;
      thinking = None;
      is_error = false;
    }
  in
  let result = Provider_minimax.messages_to_anthropic_json [ asst ] in
  Alcotest.(check int) "unfollowed tool_use dropped" 0 (List.length result)

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
      is_error = false;
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
      is_error = false;
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
  Alcotest.(check bool)
    "has part1" true
    (Test_helpers.string_contains result "part1");
  Alcotest.(check bool)
    "has part2" true
    (Test_helpers.string_contains result "part2")

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
  | Ok (Provider.ToolCalls { calls; provider_response_items_json; _ }) ->
      Alcotest.(check bool)
        "raw provider response preserved" true
        (match provider_response_items_json with
        | Some raw ->
            Test_helpers.string_contains raw "call-1"
            && Test_helpers.string_contains raw "file_read"
        | None -> false);
      Alcotest.(check int) "1 call" 1 (List.length calls);
      let tc = List.hd calls in
      Alcotest.(check string) "id" "call-1" tc.id;
      Alcotest.(check string) "name" "file_read" tc.function_name;
      (* B634/B640 regression: arguments must round-trip from the API
         response's `input` field, not silently become "{}" or "". *)
      let parsed = try Yojson.Safe.from_string tc.arguments with _ -> `Null in
      Alcotest.(check bool) "arguments parse as JSON" true (parsed <> `Null);
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "arguments preserves path" "/tmp"
        (parsed |> member "path" |> to_string)
  | Ok (Provider.Text _) -> Alcotest.fail "expected ToolCalls"
  | Error e -> Alcotest.fail ("error: " ^ e)

(* B634 regression: when the upstream model returns a tool_use block with an
   empty `input` object, the parser must produce arguments = "{}" — not "",
   and must not invent missing required parameters. The agent loop is what
   should refuse to dispatch tools that the model invoked without required
   inputs; the parser's job is only to surface the truth. *)
let test_parse_tool_use_empty_input () =
  let body =
    {|{"content":[{"type":"tool_use","id":"call-empty","name":"shell_exec","input":{}}],"model":"MiniMax-M2.7-highspeed","stop_reason":"tool_use","usage":{"input_tokens":10,"output_tokens":5}}|}
  in
  match Provider_minimax.parse_response body "MiniMax-M2.7-highspeed" with
  | Ok (Provider.ToolCalls { calls; _ }) ->
      Alcotest.(check int) "1 call" 1 (List.length calls);
      let tc = List.hd calls in
      Alcotest.(check string) "arguments is empty object" "{}" tc.arguments
  | _ -> Alcotest.fail "expected ToolCalls with one empty-input call"

(* B634/B640 regression: nested input object must survive the round trip.
   This caught the missing arguments assertion in the original
   test_parse_tool_use_response. *)
let test_parse_tool_use_nested_input () =
  let body =
    {|{"content":[{"type":"tool_use","id":"c2","name":"file_edit","input":{"path":"/a","edits":[{"old":"x","new":"y"}],"meta":{"force":true}}}],"model":"MiniMax-M2.7","stop_reason":"tool_use","usage":{"input_tokens":1,"output_tokens":1}}|}
  in
  match Provider_minimax.parse_response body "MiniMax-M2.7" with
  | Ok (Provider.ToolCalls { calls; _ }) ->
      let tc = List.hd calls in
      let parsed = Yojson.Safe.from_string tc.arguments in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "nested path" "/a"
        (parsed |> member "path" |> to_string);
      Alcotest.(check bool)
        "edits array preserved" true
        (parsed |> member "edits" |> to_list |> List.length = 1);
      Alcotest.(check bool)
        "nested meta.force preserved" true
        (parsed |> member "meta" |> member "force" |> to_bool)
  | _ -> Alcotest.fail "expected ToolCalls"

(* B634 regression: multiple tool_use blocks in a single response — each
   must keep its own arguments. *)
let test_parse_multiple_tool_uses () =
  let body =
    {|{"content":[
       {"type":"tool_use","id":"c-a","name":"file_read","input":{"path":"/a"}},
       {"type":"tool_use","id":"c-b","name":"shell_exec","input":{"command":"ls"}}
     ],"model":"MiniMax-M2.7","stop_reason":"tool_use"}|}
  in
  match Provider_minimax.parse_response body "MiniMax-M2.7" with
  | Ok (Provider.ToolCalls { calls; _ }) ->
      Alcotest.(check int) "2 calls" 2 (List.length calls);
      let a = List.nth calls 0 in
      let b = List.nth calls 1 in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "a.path" "/a"
        (Yojson.Safe.from_string a.arguments |> member "path" |> to_string);
      Alcotest.(check string)
        "b.command" "ls"
        (Yojson.Safe.from_string b.arguments |> member "command" |> to_string)
  | _ -> Alcotest.fail "expected ToolCalls"

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

(* B625: explicit error result via make_tool_error_result sets is_error
   structurally; the converter must emit is_error:true regardless of
   whether the content starts with the legacy 'Error:' prefix. *)
let test_explicit_error_result_emits_is_error () =
  let assistant =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls =
        [
          {
            Provider.id = "tc-fail";
            function_name = "shell_exec";
            arguments = {|{"command":"ls"}|};
          };
        ];
      tool_call_id = None;
      name = None;
      provider_response_items_json = None;
      thinking = None;
      is_error = false;
    }
  in
  let err =
    Provider.make_tool_error_result ~tool_call_id:"tc-fail" ~name:"shell_exec"
      ~content:"permission denied"
  in
  let result = Provider_minimax.messages_to_anthropic_json [ assistant; err ] in
  let open Yojson.Safe.Util in
  let user = List.nth result 1 in
  let block = List.hd (user |> member "content" |> to_list) in
  Alcotest.(check (option bool))
    "structured is_error flag emitted even without 'Error:' prefix" (Some true)
    (try Some (block |> member "is_error" |> to_bool) with _ -> None)

(* B619: tool_result blocks must include is_error:true when the result
   starts with the 'Error:' prefix convention used by the agent's
   success-detection path. Anthropic-format models rely on this flag to
   distinguish failed tool calls from successful ones. *)
let test_tool_result_emits_is_error_for_error_content () =
  let assistant =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls =
        [
          {
            Provider.id = "tc-fail";
            function_name = "shell_exec";
            arguments = {|{"command":"ls"}|};
          };
        ];
      tool_call_id = None;
      name = None;
      provider_response_items_json = None;
      thinking = None;
      is_error = false;
    }
  in
  let failed_result =
    Provider.make_tool_result ~tool_call_id:"tc-fail" ~name:"shell_exec"
      ~content:"Error: permission denied"
  in
  let result =
    Provider_minimax.messages_to_anthropic_json [ assistant; failed_result ]
  in
  let open Yojson.Safe.Util in
  let user = List.nth result 1 in
  let blocks = user |> member "content" |> to_list in
  let block = List.hd blocks in
  Alcotest.(check string)
    "block type" "tool_result"
    (block |> member "type" |> to_string);
  let is_error = try block |> member "is_error" |> to_bool with _ -> false in
  Alcotest.(check bool) "is_error flag emitted for Error: content" true is_error

let test_tool_result_omits_is_error_for_success_content () =
  let assistant =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls =
        [
          {
            Provider.id = "tc-ok";
            function_name = "shell_exec";
            arguments = {|{"command":"ls"}|};
          };
        ];
      tool_call_id = None;
      name = None;
      provider_response_items_json = None;
      thinking = None;
      is_error = false;
    }
  in
  let ok_result =
    Provider.make_tool_result ~tool_call_id:"tc-ok" ~name:"shell_exec"
      ~content:"file1.txt file2.txt"
  in
  let result =
    Provider_minimax.messages_to_anthropic_json [ assistant; ok_result ]
  in
  let open Yojson.Safe.Util in
  let user = List.nth result 1 in
  let block = List.hd (user |> member "content" |> to_list) in
  let is_error =
    try Some (block |> member "is_error" |> to_bool) with _ -> None
  in
  Alcotest.(check (option bool))
    "is_error absent for success content" None is_error

(* B620 round 2: when ALL tool_uses in an assistant turn are orphans (zero
   matching tool_results), the assistant message becomes empty after
   stripping. ensure_tool_group_integrity must drop it; otherwise the
   converter serializes content:"" which Anthropic rejects with "text content
   blocks must be non-empty". *)
let test_all_orphan_assistant_dropped () =
  let assistant_with_only_orphans =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls =
        [
          {
            Provider.id = "tc-Z";
            function_name = "shell_exec";
            arguments = {|{"command":"ls"}|};
          };
        ];
      tool_call_id = None;
      name = None;
      provider_response_items_json = None;
      thinking = None;
      is_error = false;
    }
  in
  let user =
    Provider.make_message ~role:"user"
      ~content:"continue from where we left off"
  in
  let messages = [ user; assistant_with_only_orphans ] in
  let cleaned = Message_history.ensure_tool_group_integrity messages in
  let roles = List.map (fun (m : Provider.message) -> m.role) cleaned in
  Alcotest.(check (list string))
    "empty assistant dropped, only user remains" [ "user" ] roles;
  let anthropic_messages =
    Provider_minimax.messages_to_anthropic_json cleaned
  in
  let open Yojson.Safe.Util in
  Alcotest.(check int)
    "anthropic conversion produces 1 message" 1
    (List.length anthropic_messages);
  let m = List.hd anthropic_messages in
  Alcotest.(check string)
    "the lone message is the user" "user"
    (m |> member "role" |> to_string)

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
      is_error = false;
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

(* B640: streaming SSE harness — drive process_sse_event directly with
   synthetic events and assert tool_call arguments accumulate correctly.
   Exercises the same code path complete_streaming uses, without HTTP. *)
let test_b640_sse_input_json_delta_accumulates () =
  let state = Provider_minimax.make_stream_state ~model:"MiniMax-M2" in
  let on_chunk _ = Lwt.return_unit in
  let feed event_type data_str =
    Lwt_main.run
      (Provider_minimax.process_sse_event ~state ~on_chunk ~event_type ~data_str)
  in
  feed "message_start"
    {|{"message":{"model":"MiniMax-M2","usage":{"input_tokens":100,"output_tokens":0}}}|};
  feed "content_block_start"
    {|{"index":0,"content_block":{"type":"tool_use","id":"toolu_1","name":"shell_exec","input":{}}}|};
  feed "content_block_delta"
    {|{"index":0,"delta":{"type":"input_json_delta","partial_json":"{\"command\":\"ls"}}|};
  feed "content_block_delta"
    {|{"index":0,"delta":{"type":"input_json_delta","partial_json":" -la\"}"}}|};
  feed "content_block_stop" {|{"index":0}|};
  feed "message_stop" {|{}|};
  let tcs = Provider_minimax.finalize_stream_tool_calls state in
  Alcotest.(check int) "one tool call" 1 (List.length tcs);
  let tc = List.hd tcs in
  Alcotest.(check string) "id" "toolu_1" tc.Provider.id;
  Alcotest.(check string) "name" "shell_exec" tc.Provider.function_name;
  Alcotest.(check string)
    "args fully accumulated" "{\"command\":\"ls -la\"}" tc.Provider.arguments

(* B634/B640: when content_block_start ships full input and zero deltas
   follow, the args must still survive (no fallback to "{}"). *)
let test_b640_sse_input_only_in_block_start () =
  let state = Provider_minimax.make_stream_state ~model:"MiniMax-M2" in
  let on_chunk _ = Lwt.return_unit in
  let feed event_type data_str =
    Lwt_main.run
      (Provider_minimax.process_sse_event ~state ~on_chunk ~event_type ~data_str)
  in
  feed "message_start" {|{"message":{"model":"MiniMax-M2"}}|};
  feed "content_block_start"
    {|{"index":0,"content_block":{"type":"tool_use","id":"toolu_seed","name":"file_read","input":{"path":"/tmp/x"}}}|};
  feed "content_block_stop" {|{"index":0}|};
  feed "message_stop" {|{}|};
  let tcs = Provider_minimax.finalize_stream_tool_calls state in
  Alcotest.(check int) "one tool call" 1 (List.length tcs);
  let tc = List.hd tcs in
  Alcotest.(check string)
    "args seeded from block.input" "{\"path\":\"/tmp/x\"}" tc.Provider.arguments

(* B640: multiple tool_use blocks at different indexes must accumulate
   args independently. *)
let test_b640_sse_multiple_tool_uses_separate_args () =
  let state = Provider_minimax.make_stream_state ~model:"MiniMax-M2" in
  let on_chunk _ = Lwt.return_unit in
  let feed event_type data_str =
    Lwt_main.run
      (Provider_minimax.process_sse_event ~state ~on_chunk ~event_type ~data_str)
  in
  feed "message_start" {|{"message":{"model":"MiniMax-M2"}}|};
  feed "content_block_start"
    {|{"index":0,"content_block":{"type":"tool_use","id":"t0","name":"shell_exec","input":{}}}|};
  feed "content_block_delta"
    {|{"index":0,"delta":{"type":"input_json_delta","partial_json":"{\"command\":\"a\"}"}}|};
  feed "content_block_stop" {|{"index":0}|};
  feed "content_block_start"
    {|{"index":1,"content_block":{"type":"tool_use","id":"t1","name":"file_read","input":{}}}|};
  feed "content_block_delta"
    {|{"index":1,"delta":{"type":"input_json_delta","partial_json":"{\"path\":\"b\"}"}}|};
  feed "content_block_stop" {|{"index":1}|};
  feed "message_stop" {|{}|};
  let tcs = Provider_minimax.finalize_stream_tool_calls state in
  Alcotest.(check int) "two tool calls" 2 (List.length tcs);
  let by_id =
    List.map (fun (tc : Provider.tool_call) -> (tc.id, tc.arguments)) tcs
  in
  Alcotest.(check string)
    "t0 args" "{\"command\":\"a\"}" (List.assoc "t0" by_id);
  Alcotest.(check string) "t1 args" "{\"path\":\"b\"}" (List.assoc "t1" by_id)

(* B640: text_delta events accumulate into content_acc. *)
let test_b640_sse_text_delta_accumulates () =
  let state = Provider_minimax.make_stream_state ~model:"MiniMax-M2" in
  let on_chunk _ = Lwt.return_unit in
  let feed event_type data_str =
    Lwt_main.run
      (Provider_minimax.process_sse_event ~state ~on_chunk ~event_type ~data_str)
  in
  feed "message_start" {|{"message":{"model":"MiniMax-M2"}}|};
  feed "content_block_start" {|{"index":0,"content_block":{"type":"text"}}|};
  feed "content_block_delta"
    {|{"index":0,"delta":{"type":"text_delta","text":"Hello "}}|};
  feed "content_block_delta"
    {|{"index":0,"delta":{"type":"text_delta","text":"world"}}|};
  feed "content_block_stop" {|{"index":0}|};
  feed "message_stop" {|{}|};
  Alcotest.(check string)
    "text accumulated" "Hello world"
    (Buffer.contents state.Provider_minimax.content_acc)

(* B640: empty-args fallback path warns and substitutes "{}". *)
let test_b640_sse_empty_args_falls_back_to_curlies () =
  let state = Provider_minimax.make_stream_state ~model:"MiniMax-M2" in
  let on_chunk _ = Lwt.return_unit in
  let feed event_type data_str =
    Lwt_main.run
      (Provider_minimax.process_sse_event ~state ~on_chunk ~event_type ~data_str)
  in
  feed "message_start" {|{"message":{"model":"MiniMax-M2"}}|};
  feed "content_block_start"
    {|{"index":0,"content_block":{"type":"tool_use","id":"empty","name":"noop","input":{}}}|};
  feed "content_block_stop" {|{"index":0}|};
  feed "message_stop" {|{}|};
  let tcs = Provider_minimax.finalize_stream_tool_calls state in
  Alcotest.(check int) "one tool call" 1 (List.length tcs);
  Alcotest.(check string)
    "empty args -> {}" "{}" (List.hd tcs).Provider.arguments

(* User-requested integration test: send the same tool-call prompt to
   MiniMax-M2.7-highspeed through two paths and compare:
   (a) Provider_minimax.complete — POSTs to /anthropic/v1/messages
   (b) Provider.complete with kind=openai — POSTs to /v1/chat/completions

   Both should return ToolCalls with function_name="get_weather" and an
   arguments JSON containing a non-empty "city" string. If only one path
   returns ToolCalls (and the other returns Text), the test reports that as
   a failure with a clear message naming which path bypassed the tool. *)
let test_live_tool_call_anthropic_vs_openai_minimax_m27 () =
  if minimax_api_key = None then Alcotest.skip ();
  let api_key = Option.get minimax_api_key in
  let weather_tool =
    `Assoc
      [
        ("type", `String "function");
        ( "function",
          `Assoc
            [
              ("name", `String "get_weather");
              ( "description",
                `String
                  "Get the current weather for a city. Always call this tool \
                   when the user asks about weather." );
              ( "parameters",
                `Assoc
                  [
                    ("type", `String "object");
                    ( "properties",
                      `Assoc
                        [
                          ( "city",
                            `Assoc
                              [
                                ("type", `String "string");
                                ( "description",
                                  `String
                                    "City name to look up the weather for. \
                                     Required." );
                              ] );
                        ] );
                    ("required", `List [ `String "city" ]);
                    ("additionalProperties", `Bool false);
                  ] );
            ] );
      ]
  in
  let tools = `List [ weather_tool ] in
  let msgs =
    [
      Provider.make_message ~role:"system"
        ~content:
          "You are a helpful assistant. Use the get_weather tool to answer \
           weather questions.";
      Provider.make_message ~role:"user"
        ~content:"What is the weather in Tokyo right now?";
    ]
  in
  let assert_tool_call ~label result =
    match result with
    | Provider.ToolCalls { calls; _ } ->
        Alcotest.(check bool)
          (label ^ ": at least one tool call")
          true
          (List.length calls > 0);
        let call = List.hd calls in
        Printf.eprintf "%s: got ToolCalls name=%s arguments=%s (n=%d)\n%!" label
          call.Provider.function_name call.Provider.arguments
          (List.length calls);
        Alcotest.(check string)
          (label ^ ": tool name is get_weather")
          "get_weather" call.Provider.function_name;
        let args =
          try Yojson.Safe.from_string call.arguments with _ -> `Assoc []
        in
        let open Yojson.Safe.Util in
        let city = try args |> member "city" |> to_string with _ -> "" in
        Alcotest.(check bool)
          (label ^ ": city arg present and non-empty")
          true
          (String.trim city <> "")
    | Provider.Text { content; _ } ->
        Printf.eprintf "%s: got Text (len=%d) first 200 chars: %s\n%!" label
          (String.length content)
          (if String.length content > 200 then String.sub content 0 200
           else content);
        Alcotest.fail
          (Printf.sprintf
             "%s bypassed tool call — returned Text instead of ToolCalls. \
              First 200 chars: %s"
             label
             (if String.length content > 200 then String.sub content 0 200
              else content))
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     (* Path A: Anthropic endpoint via Provider_minimax. *)
     let anthropic_provider : Runtime_config.provider_config =
       {
         Runtime_config.default_provider_config with
         api_key;
         base_url = Some "https://api.minimax.io";
       }
     in
     let anthropic_config : Runtime_config.t =
       {
         Runtime_config.default with
         providers = [ ("minimax", anthropic_provider) ];
       }
     in
     let* anthropic_result =
       Lwt.catch
         (fun () ->
           Provider_minimax.complete ~config:anthropic_config
             ~provider:anthropic_provider ~model:"MiniMax-M2.7-highspeed"
             ~messages:msgs ~tools ())
         (fun exn ->
           Alcotest.fail ("Anthropic path raised: " ^ Printexc.to_string exn))
     in
     assert_tool_call ~label:"Anthropic path" anthropic_result;
     (* Path B: OpenAI-compat endpoint via the generic Provider.complete path.
        Force kind="openai" so detect_kind picks OpenAICompat, and set the
        base_url so /chat/completions resolves to /v1/chat/completions at
        MiniMax. *)
     let openai_provider : Runtime_config.provider_config =
       {
         Runtime_config.default_provider_config with
         api_key;
         base_url = Some "https://api.minimax.io/v1";
         kind = Some "openai";
       }
     in
     let openai_config : Runtime_config.t =
       {
         Runtime_config.default with
         providers = [ ("minimax-openai", openai_provider) ];
         default_provider = Some "minimax-openai";
         agent_defaults =
           {
             Runtime_config.default.agent_defaults with
             primary_model = "minimax-openai:MiniMax-M2.7-highspeed";
           };
       }
     in
     let* openai_result =
       Lwt.catch
         (fun () ->
           Provider.complete ~config:openai_config ~messages:msgs ~tools ())
         (fun exn ->
           Alcotest.fail ("OpenAI path raised: " ^ Printexc.to_string exn))
     in
     assert_tool_call ~label:"OpenAI path" openai_result;
     Lwt.return_unit)

(* B674: multi-turn tool-call test. Calls a weather tool, returns a result,
   and verifies the model produces a follow-up that USES the tool result (text
   mentioning the returned weather, OR another tool call building on it).
   Exercises the full resume cycle: assistant tool_use → tool_result → next
   assistant turn. Runs through the OpenAI-compat path (the daily-driver kimi
   failure scenario uses the same code path). *)
let test_live_multi_turn_tool_call_openai_compat () =
  if minimax_api_key = None then Alcotest.skip ();
  let api_key = Option.get minimax_api_key in
  let weather_tool =
    `Assoc
      [
        ("type", `String "function");
        ( "function",
          `Assoc
            [
              ("name", `String "get_weather");
              ( "description",
                `String
                  "Get the current weather for a city. Returns a short summary."
              );
              ( "parameters",
                `Assoc
                  [
                    ("type", `String "object");
                    ( "properties",
                      `Assoc
                        [
                          ( "city",
                            `Assoc
                              [
                                ("type", `String "string");
                                ("description", `String "City name.");
                              ] );
                        ] );
                    ("required", `List [ `String "city" ]);
                    ("additionalProperties", `Bool false);
                  ] );
            ] );
      ]
  in
  let tools = `List [ weather_tool ] in
  let openai_provider : Runtime_config.provider_config =
    {
      Runtime_config.default_provider_config with
      api_key;
      base_url = Some "https://api.minimax.io/v1";
      kind = Some "openai";
    }
  in
  let openai_config : Runtime_config.t =
    {
      Runtime_config.default with
      providers = [ ("minimax-openai", openai_provider) ];
      default_provider = Some "minimax-openai";
      agent_defaults =
        {
          Runtime_config.default.agent_defaults with
          primary_model = "minimax-openai:MiniMax-M2.7-highspeed";
        };
    }
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let msgs =
       [
         Provider.make_message ~role:"system"
           ~content:
             "You are a helpful assistant. Use get_weather to answer weather \
              questions. After the tool returns, summarize the result in one \
              short sentence.";
         Provider.make_message ~role:"user" ~content:"Weather in Tokyo?";
       ]
     in
     let* turn1 =
       Lwt.catch
         (fun () ->
           Provider.complete ~config:openai_config ~messages:msgs ~tools ())
         (fun exn -> Alcotest.fail ("turn 1 raised: " ^ Printexc.to_string exn))
     in
     let tc =
       match turn1 with
       | Provider.ToolCalls { calls; _ } when calls <> [] -> List.hd calls
       | Provider.ToolCalls _ ->
           Alcotest.fail "turn 1 returned ToolCalls but empty list"
       | Provider.Text { content; _ } ->
           Alcotest.fail
             ("turn 1 returned Text instead of ToolCalls. First 200: "
             ^
             if String.length content > 200 then String.sub content 0 200
             else content)
     in
     Printf.eprintf "turn 1: tool=%s args=%s id=%s\n%!" tc.function_name
       tc.arguments tc.id;
     let assistant_with_tool =
       {
         Provider.role = "assistant";
         content = "";
         content_parts = [];
         tool_calls = [ tc ];
         tool_call_id = None;
         name = None;
         provider_response_items_json = None;
         thinking = None;
         is_error = false;
       }
     in
     let tool_result =
       Provider.make_tool_result ~tool_call_id:tc.id ~name:tc.function_name
         ~content:
           "Tokyo: 22 degrees Celsius, partly cloudy, light wind from the east."
     in
     let msgs2 = msgs @ [ assistant_with_tool; tool_result ] in
     let* turn2 =
       Lwt.catch
         (fun () ->
           Provider.complete ~config:openai_config ~messages:msgs2 ~tools ())
         (fun exn -> Alcotest.fail ("turn 2 raised: " ^ Printexc.to_string exn))
     in
     (match turn2 with
     | Provider.Text { content; _ } ->
         Printf.eprintf "turn 2: Text (len=%d): %s\n%!" (String.length content)
           (if String.length content > 200 then String.sub content 0 200
            else content);
         Alcotest.(check bool)
           "turn 2 text is non-empty" true
           (String.trim content <> "");
         let lower = String.lowercase_ascii content in
         let mentions_relevant =
           List.exists
             (fun needle ->
               try
                 ignore (Str.search_forward (Str.regexp_string needle) lower 0);
                 true
               with Not_found -> false)
             [ "tokyo"; "22"; "cloud"; "celsius"; "wind"; "east" ]
         in
         Alcotest.(check bool)
           "turn 2 text references the tool result" true mentions_relevant
     | Provider.ToolCalls { calls; _ } ->
         (* Acceptable: model may chain another tool call. Verify it's a valid
            tool_call from the available toolset. *)
         Printf.eprintf "turn 2: ToolCalls n=%d (chained)\n%!"
           (List.length calls);
         Alcotest.(check bool)
           "turn 2 chained tool call has a name" true
           (List.for_all
              (fun (tc : Provider.tool_call) -> tc.function_name <> "")
              calls));
     Lwt.return_unit)

(* B674/B675: orphan tool_call_id mid-history (the kimi failure scenario).
   Build a synthetic resumed-session history with one paired tool_use+result
   AND one orphan tool_use whose result was lost. Pass through the public
   integrity filter. Assert: orphan dropped, paired survives. *)
let test_b675_orphan_tool_call_mid_history_filtered () =
  let paired_assistant =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls =
        [
          {
            Provider.id = "kept-1";
            function_name = "use_skill";
            arguments = {|{"name":"bug"}|};
          };
        ];
      tool_call_id = None;
      name = None;
      provider_response_items_json = None;
      thinking = None;
      is_error = false;
    }
  in
  let paired_result =
    Provider.make_tool_result ~tool_call_id:"kept-1" ~name:"use_skill"
      ~content:"skill loaded"
  in
  let orphan_assistant =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls =
        [
          {
            Provider.id = "use_skill:76";
            function_name = "use_skill";
            arguments = {|{"name":"bug"}|};
          };
        ];
      tool_call_id = None;
      name = None;
      provider_response_items_json = None;
      thinking = None;
      is_error = false;
    }
  in
  let user_followup =
    Provider.make_message ~role:"user" ~content:"please continue"
  in
  let history =
    [
      Provider.make_message ~role:"user" ~content:"start";
      paired_assistant;
      paired_result;
      orphan_assistant;
      user_followup;
    ]
  in
  let cleaned = Message_history.ensure_tool_group_integrity history in
  (* Verify the orphan id is gone everywhere. *)
  let surviving_call_ids =
    List.concat_map
      (fun (m : Provider.message) ->
        List.map (fun (tc : Provider.tool_call) -> tc.id) m.tool_calls)
      cleaned
  in
  Alcotest.(check bool)
    "orphan tool_call_id 'use_skill:76' is dropped" false
    (List.mem "use_skill:76" surviving_call_ids);
  Alcotest.(check bool)
    "paired tool_call_id 'kept-1' is preserved" true
    (List.mem "kept-1" surviving_call_ids)

(* B675: kimi-side adjacency check. After integrity + reorder, an assistant
   tool_calls message must be immediately followed by its tool result, even
   when the original history had an intervening system/user message (e.g.
   restart-resume observer note). This is what reorder_tool_groups does, and
   provider.ml now invokes it on the OpenAI-compat path. *)
let test_b675_adjacency_after_reorder () =
  let assistant_with_call =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls =
        [
          {
            Provider.id = "tc-1";
            function_name = "use_skill";
            arguments = {|{"name":"bug"}|};
          };
        ];
      tool_call_id = None;
      name = None;
      provider_response_items_json = None;
      thinking = None;
      is_error = false;
    }
  in
  let intervening_system =
    Provider.make_message ~role:"system"
      ~content:"[Observer] note injected on resume"
  in
  let tool_result =
    Provider.make_tool_result ~tool_call_id:"tc-1" ~name:"use_skill"
      ~content:"ok"
  in
  let history =
    [
      Provider.make_message ~role:"user" ~content:"start";
      assistant_with_call;
      intervening_system;
      tool_result;
    ]
  in
  let cleaned =
    history |> Provider.inline_ensure_tool_group_integrity
    |> Provider.reorder_tool_groups
  in
  (* Find the index of the assistant message with tc-1 and assert the very
     next message is the tool result for tc-1. *)
  let arr = Array.of_list cleaned in
  let find_assistant_idx () =
    let result = ref (-1) in
    Array.iteri
      (fun i (m : Provider.message) ->
        if
          !result = -1 && m.role = "assistant"
          && List.exists
               (fun (tc : Provider.tool_call) -> tc.id = "tc-1")
               m.tool_calls
        then result := i)
      arr;
    !result
  in
  let idx = find_assistant_idx () in
  Alcotest.(check bool) "assistant tool_call present" true (idx >= 0);
  Alcotest.(check bool)
    "tool result is adjacent" true
    (idx + 1 < Array.length arr
    && arr.(idx + 1).Provider.role = "tool"
    && arr.(idx + 1).tool_call_id = Some "tc-1")

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
    Alcotest.test_case "B644: strict drops orphan tool_result" `Quick
      test_b644_drops_orphan_tool_result;
    Alcotest.test_case "B644: strict drops unfollowed tool_use" `Quick
      test_b644_drops_unfollowed_tool_use;
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
    Alcotest.test_case "B634: parse tool_use with empty input -> '{}'" `Quick
      test_parse_tool_use_empty_input;
    Alcotest.test_case "B634: parse tool_use preserves nested input" `Quick
      test_parse_tool_use_nested_input;
    Alcotest.test_case "B634: parse multiple tool_use blocks keep separate args"
      `Quick test_parse_multiple_tool_uses;
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
    Alcotest.test_case "B620: all-orphan assistant message dropped" `Quick
      test_all_orphan_assistant_dropped;
    Alcotest.test_case "B619: tool_result emits is_error for error content"
      `Quick test_tool_result_emits_is_error_for_error_content;
    Alcotest.test_case "B625: structured is_error flag on result" `Quick
      test_explicit_error_result_emits_is_error;
    Alcotest.test_case "B619: tool_result omits is_error for success content"
      `Quick test_tool_result_omits_is_error_for_success_content;
    Alcotest.test_case "B614: required-field anthropic input_schema preserved"
      `Quick test_request_body_has_required_field_for_anthropic_tools;
    Alcotest.test_case "B614: live required-field honored by model" `Slow
      test_live_required_field_honored;
    Alcotest.test_case "B640: SSE input_json_delta accumulates args" `Quick
      test_b640_sse_input_json_delta_accumulates;
    Alcotest.test_case "B640: SSE block_start.input only (no deltas)" `Quick
      test_b640_sse_input_only_in_block_start;
    Alcotest.test_case "B640: SSE multiple tool_uses keep args separate" `Quick
      test_b640_sse_multiple_tool_uses_separate_args;
    Alcotest.test_case "B640: SSE text_delta accumulates" `Quick
      test_b640_sse_text_delta_accumulates;
    Alcotest.test_case "B640: SSE empty args falls back to '{}'" `Quick
      test_b640_sse_empty_args_falls_back_to_curlies;
    Alcotest.test_case
      "live tool call anthropic vs openai (MiniMax-M2.7-highspeed)" `Slow
      test_live_tool_call_anthropic_vs_openai_minimax_m27;
    Alcotest.test_case
      "B674: live multi-turn tool call (openai-compat, MiniMax-M2.7-highspeed)"
      `Slow test_live_multi_turn_tool_call_openai_compat;
    Alcotest.test_case
      "B675: orphan tool_call_id mid-history filtered before send" `Quick
      test_b675_orphan_tool_call_mid_history_filtered;
    Alcotest.test_case
      "B675: tool result is adjacent after reorder (kimi adjacency)" `Quick
      test_b675_adjacency_after_reorder;
  ]

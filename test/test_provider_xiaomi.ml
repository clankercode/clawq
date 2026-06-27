(* E2E tests for Xiaomi MiMo via Anthropic-compatible endpoint.
   Tagged Slow — skipped by `make test`, included in `make test-all`.
   Requires XIAOMI_API_KEY env var or key discoverable via Xiaomi.resolve_api_key
   (env vars per region, or ~/.mimo for sgp). *)

(* --- key discovery --- *)

let public_api_key =
  try Some (Sys.getenv "XIAOMI_API_KEY") with Not_found -> None

let sgp_api_key = Xiaomi.resolve_api_key "xiaomi-token-plan-sgp"

let any_key_available =
  public_api_key <> None || sgp_api_key <> None

(* Resolve the best available key and provider config. Prefers public xiaomi,
   falls back to sgp token plan. Returns (provider_name, provider_config). *)
let resolve_test_provider () =
  match public_api_key with
  | Some key ->
      ( "xiaomi",
        {
          Runtime_config.default_provider_config with
          api_key = key;
          base_url = Some "https://api.xiaomimimo.com/anthropic";
          kind = Some "xiaomi";
        } )
  | None -> (
      match sgp_api_key with
      | Some key ->
          ( "xiaomi-token-plan-sgp",
            {
              Runtime_config.default_provider_config with
              api_key = key;
              base_url = Some "https://token-plan-sgp.xiaomimimo.com/anthropic";
              kind = Some "xiaomi";
            } )
      | None -> Alcotest.fail "no Xiaomi API key available" )

(* mimo-v2.5-pro is the flagship — use it for all live tests. It has 1M context,
   131K output, and is the model most likely to be used as primary. *)
let test_model = "mimo-v2.5-pro"

let make_test_config () =
  let provider_name, provider_config = resolve_test_provider () in
  let default = Runtime_config.default in
  {
    default with
    default_provider = Some provider_name;
    providers = [ (provider_name, provider_config) ];
    agent_defaults =
      {
        default.agent_defaults with
        primary_model = provider_name ^ ":" ^ test_model;
      };
  }

(* --- diagnostics --- *)

let test_routing_is_anthropic () =
  let pc =
    {
      Runtime_config.default_provider_config with
      api_key = "fake";
      kind = Some "xiaomi";
    }
  in
  Alcotest.(check bool)
    "xiaomi kind routes to Anthropic" true
    (Provider.detect_kind pc = Provider.Anthropic)

(* --- live tests (Slow) --- *)

let test_live_simple_completion () =
  if not any_key_available then Alcotest.skip ();
  let config = make_test_config () in
  let provider_name, provider = resolve_test_provider () in
  let msgs =
    [
      Provider.make_message ~role:"user"
        ~content:"What is 2+2? Answer in one word.";
    ]
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* result =
       Lwt.catch
         (fun () ->
           Provider_anthropic.complete ~config ~provider ~model:test_model
             ~messages:msgs ())
         (fun exn ->
           Alcotest.fail
             ("request failed: " ^ Printexc.to_string exn))
     in
     match result with
     | Provider.Text { content; _ } ->
         Alcotest.(check bool)
           "has content" true (String.length content > 0);
         let lower = String.lowercase_ascii (String.trim content) in
         let mentions_four =
           List.exists
             (fun needle ->
               try
                 ignore (Str.search_forward (Str.regexp_string needle) lower 0);
                 true
               with Not_found -> false)
             [ "four"; "4" ]
         in
         Alcotest.(check bool)
           (Printf.sprintf "answer mentions 4 (got: %s)" (String.trim content))
           true mentions_four;
         Lwt.return_unit
     | Provider.ToolCalls _ ->
         Alcotest.fail "expected Text, got ToolCalls")

let test_live_single_tool_call () =
  if not any_key_available then Alcotest.skip ();
  let config = make_test_config () in
  let provider_name, provider = resolve_test_provider () in
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
                                    "City name to look up the weather for." );
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
  Lwt_main.run
    (let open Lwt.Syntax in
     let* result =
       Lwt.catch
         (fun () ->
           Provider_anthropic.complete ~config ~provider ~model:test_model
             ~messages:msgs ~tools ())
         (fun exn ->
           Alcotest.fail ("request failed: " ^ Printexc.to_string exn))
     in
     match result with
     | Provider.ToolCalls { calls; _ } ->
         Alcotest.(check bool)
           "at least one tool call" true (List.length calls > 0);
         let call = List.hd calls in
         Printf.eprintf "tool call: name=%s args=%s id=%s\n%!"
           call.Provider.function_name call.Provider.arguments
           call.Provider.id;
         Alcotest.(check string)
           "tool name is get_weather" "get_weather"
           call.Provider.function_name;
         let args =
           try Yojson.Safe.from_string call.arguments with _ -> `Assoc []
         in
         let open Yojson.Safe.Util in
         let city = try args |> member "city" |> to_string with _ -> "" in
         Alcotest.(check bool)
           "city arg present and non-empty" true
           (String.trim city <> "");
         Lwt.return_unit
     | Provider.Text { content; _ } ->
         Alcotest.fail
           (Printf.sprintf
              "expected ToolCalls, got Text (len=%d): %s"
              (String.length content)
              (if String.length content > 200 then
                 String.sub content 0 200
               else content)))

(* Multi-turn tool call: tool_use → tool_result → follow-up.
   This is the critical test for the Anthropic endpoint fix — the OpenAI-compat
   endpoint failed at this step (GitHub issue #44). *)
let test_live_multi_turn_tool_call () =
  if not any_key_available then Alcotest.skip ();
  let config = make_test_config () in
  let provider_name, provider = resolve_test_provider () in
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
  Lwt_main.run
    (let open Lwt.Syntax in
     (* Turn 1: expect tool call *)
     let* turn1 =
       Lwt.catch
         (fun () ->
           Provider_anthropic.complete ~config ~provider ~model:test_model
             ~messages:msgs ~tools ())
         (fun exn ->
           Alcotest.fail ("turn 1 failed: " ^ Printexc.to_string exn))
     in
     let tc =
       match turn1 with
       | Provider.ToolCalls { calls; _ } when calls <> [] -> List.hd calls
       | Provider.ToolCalls _ ->
           Alcotest.fail "turn 1 returned ToolCalls but empty list"
       | Provider.Text { content; _ } ->
           Alcotest.fail
             (Printf.sprintf
                "turn 1 expected ToolCalls, got Text (len=%d): %s"
                (String.length content)
                (if String.length content > 200 then
                   String.sub content 0 200
                 else content))
     in
     Printf.eprintf "turn 1: tool=%s args=%s id=%s\n%!" tc.function_name
       tc.arguments tc.id;
     (* Build the full history with tool result *)
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
     (* Turn 2: the critical turn — OpenAI-compat 400'd here *)
     let* turn2 =
       Lwt.catch
         (fun () ->
           Provider_anthropic.complete ~config ~provider ~model:test_model
             ~messages:msgs2 ~tools ())
         (fun exn ->
           Alcotest.fail ("turn 2 failed: " ^ Printexc.to_string exn))
     in
     (match turn2 with
     | Provider.Text { content; _ } ->
         Printf.eprintf "turn 2: Text (len=%d): %s\n%!"
           (String.length content)
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
             [ "tokyo"; "22"; "cloud"; "celsius"; "wind"; "east"; "weather" ]
         in
         Alcotest.(check bool)
           "turn 2 references the tool result" true mentions_relevant
     | Provider.ToolCalls { calls; _ } ->
         (* Acceptable: model may chain another tool call *)
         Printf.eprintf "turn 2: ToolCalls n=%d (chained)\n%!"
           (List.length calls);
         Alcotest.(check bool)
           "turn 2 chained tool call has a name" true
           (List.for_all
              (fun (tc : Provider.tool_call) -> tc.function_name <> "")
              calls));
     Lwt.return_unit)

(* --- suite --- *)

let suite =
  [
    Alcotest.test_case "routing xiaomi is Anthropic" `Quick
      test_routing_is_anthropic;
    Alcotest.test_case "live simple completion" `Slow
      test_live_simple_completion;
    Alcotest.test_case "live single tool call" `Slow
      test_live_single_tool_call;
    Alcotest.test_case "live multi-turn tool call (Anthropic endpoint)" `Slow
      test_live_multi_turn_tool_call;
  ]

let default_config = Runtime_config.default
let make_fake_provider_config = Test_helpers.make_fake_provider_config

let with_fake_tool_loop_provider f =
  let port = Test_helpers.free_port () in
  let requests = ref 0 in
  let callback _conn req _body =
    incr requests;
    match (Cohttp.Request.meth req, Uri.path (Cohttp.Request.uri req)) with
    | `POST, "/chat/completions" ->
        let response_body =
          Yojson.Safe.to_string
            (`Assoc
               [
                 ("id", `String "cmpl_fake_tool_loop");
                 ("object", `String "chat.completion");
                 ("model", `String "fake-model");
                 ( "choices",
                   `List
                     [
                       `Assoc
                         [
                           ("index", `Int 0);
                           ( "message",
                             `Assoc
                               [
                                 ("role", `String "assistant");
                                 ("content", `String "");
                                 ( "tool_calls",
                                   `List
                                     [
                                       `Assoc
                                         [
                                           ("id", `String "call_1");
                                           ("type", `String "function");
                                           ( "function",
                                             `Assoc
                                               [
                                                 ("name", `String "loop_tool");
                                                 ("arguments", `String "{}");
                                               ] );
                                         ];
                                     ] );
                               ] );
                           ("finish_reason", `String "tool_calls");
                         ];
                     ] );
                 ( "usage",
                   `Assoc
                     [
                       ("prompt_tokens", `Int 1); ("completion_tokens", `Int 1);
                     ] );
               ])
        in
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:response_body ()
    | _ -> Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"" ()
  in
  let stop, stopper = Lwt.wait () in
  let server =
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  Lwt.async (fun () -> Lwt.pick [ server; stop ]);
  Fun.protect
    ~finally:(fun () -> Lwt.wakeup_later stopper ())
    (fun () ->
      let config =
        {
          Runtime_config.default with
          default_provider = Some "fake";
          providers =
            [
              ( "fake",
                make_fake_provider_config
                  (Printf.sprintf "http://127.0.0.1:%d" port) );
            ];
          prompt =
            { Runtime_config.default.prompt with dynamic_enabled = false };
          security =
            { Runtime_config.default.security with tools_enabled = true };
          agent_defaults =
            {
              Runtime_config.default.agent_defaults with
              primary_model = "fake-model";
              max_tool_iterations = 1;
              show_thinking = false;
              show_tool_calls = false;
            };
        }
      in
      f config requests)

(* Test: system prompt from config *)
let test_system_prompt_from_config () =
  let config =
    {
      default_config with
      prompt = { default_config.prompt with dynamic_enabled = false };
      agent_defaults =
        {
          default_config.agent_defaults with
          system_prompt = "You are a test bot.";
        };
    }
  in
  let agent = Agent.create ~config () in
  Alcotest.(check string)
    "system prompt from config" "You are a test bot." agent.system_prompt

(* Test: default system prompt *)
let test_default_system_prompt () =
  let agent = Agent.create ~config:default_config () in
  Alcotest.(check bool)
    "default system prompt non-empty" true
    (String.length agent.system_prompt > 0)

(* Test: tool registry creation and serialization *)
let test_tool_registry_serialization () =
  let registry = Tool_registry.create () in
  let test_tool =
    {
      Tool.name = "test_tool";
      description = "A test tool";
      parameters_schema =
        `Assoc [ ("type", `String "object"); ("properties", `Assoc []) ];
      invoke = (fun ?context:_ _ -> Lwt.return "test result");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  Tool_registry.register registry test_tool;
  let json = Tool_registry.to_openai_json registry in
  let json_str = Yojson.Safe.to_string json in
  Alcotest.(check bool)
    "serialized JSON contains tool name" true
    (let re = Str.regexp_string "test_tool" in
     try
       ignore (Str.search_forward re json_str 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "serialized JSON contains function type" true
    (let re = Str.regexp_string "function" in
     try
       ignore (Str.search_forward re json_str 0);
       true
     with Not_found -> false)

(* Test: tool registry find *)
let test_tool_registry_find () =
  let registry = Tool_registry.create () in
  let test_tool =
    {
      Tool.name = "my_tool";
      description = "desc";
      parameters_schema = `Assoc [];
      invoke = (fun ?context:_ _ -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  Tool_registry.register registry test_tool;
  Alcotest.(check bool)
    "find existing tool" true
    (Tool_registry.find registry "my_tool" <> None);
  Alcotest.(check bool)
    "find missing tool" true
    (Tool_registry.find registry "nonexistent" = None)

(* Test: tool invocation *)
let test_tool_invocation () =
  let invoked = ref false in
  let test_tool =
    {
      Tool.name = "invoke_test";
      description = "test";
      parameters_schema = `Assoc [];
      invoke =
        (fun ?context:_ args ->
          invoked := true;
          let open Yojson.Safe.Util in
          let v =
            try args |> member "value" |> to_string with _ -> "default"
          in
          Lwt.return ("got: " ^ v));
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  let result =
    Lwt_main.run (test_tool.invoke (`Assoc [ ("value", `String "hello") ]))
  in
  Alcotest.(check bool) "tool was invoked" true !invoked;
  Alcotest.(check string) "tool result" "got: hello" result

let test_execute_tool_calls_stream_classifies_raw_error_result () =
  let registry = Tool_registry.create () in
  let raw_error = "Error:" ^ String.make 2000 'x' in
  let tool =
    {
      Tool.name = "stream_error_tool";
      description = "Streams progress before returning an error";
      parameters_schema = `Assoc [];
      invoke = (fun ?context:_ _ -> Lwt.return raw_error);
      invoke_stream =
        Some
          (fun ?context:_ ~on_output_chunk _ ->
            let open Lwt.Syntax in
            let* () = on_output_chunk "progress" in
            Lwt.return raw_error);
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  Tool_registry.register registry tool;
  let config =
    {
      default_config with
      summarizer =
        {
          default_config.summarizer with
          enabled = true;
          threshold_chars = 1;
          p2_max_chars = 4;
        };
    }
  in
  let agent = Agent.create ~config ~tool_registry:registry () in
  let call =
    {
      Provider.id = "stream_error";
      function_name = "stream_error_tool";
      arguments = "{}";
    }
  in
  Lwt_main.run
    (Agent.execute_tool_calls_stream agent ~db:None ~audit_enabled:false
       ~session_key:None [ call ] ~on_chunk:(fun _ -> Lwt.return_unit));
  match agent.history with
  | result :: _ ->
      Alcotest.(check bool) "raw error marked is_error" true result.is_error
  | [] -> Alcotest.fail "expected streaming tool result in history"

let test_execute_tool_calls_stream_bounds_final_result () =
  let registry = Tool_registry.create () in
  let streamed = String.make 13050 'x' in
  let tool =
    {
      Tool.name = "stream_tool";
      description = "Streams output before returning a result";
      parameters_schema = `Assoc [];
      invoke = (fun ?context:_ _ -> Lwt.return streamed);
      invoke_stream =
        Some
          (fun ?context:_ ~on_output_chunk _ ->
            let open Lwt.Syntax in
            let* () = on_output_chunk streamed in
            Lwt.return streamed);
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  Tool_registry.register registry tool;
  let agent = Agent.create ~config:default_config ~tool_registry:registry () in
  let events = ref [] in
  let call =
    { Provider.id = "call_1"; function_name = "stream_tool"; arguments = "{}" }
  in
  Lwt_main.run
    (Agent.execute_tool_calls_stream agent ~db:None ~audit_enabled:false
       ~session_key:None [ call ] ~on_chunk:(fun event ->
         events := event :: !events;
         Lwt.return_unit));
  let events = List.rev !events in
  let output_chunks =
    List.filter
      (function Provider.ToolOutputDelta _ -> true | _ -> false)
      events
  in
  Alcotest.(check int) "streamed one output chunk" 1 (List.length output_chunks);
  let final_result =
    List.find_map
      (function Provider.ToolResult { result; _ } -> Some result | _ -> None)
      events
  in
  match final_result with
  | None -> Alcotest.fail "expected final tool_result event"
  | Some result ->
      Alcotest.(check bool)
        "final result bounded" true
        (String.length result < String.length streamed);
      Alcotest.(check bool)
        "final result notes truncation" true
        (let re = Str.regexp_string "truncated" in
         try
           ignore (Str.search_forward re result 0);
           true
         with Not_found -> false)

(* B607: tool calls in a single batch must execute in parallel (Lwt_list.map_p)
   AND emit all ToolStart events up-front before any tool completes. This test
   uses a shared in-flight counter to detect concurrent execution and asserts
   that all 3 ToolStart events arrive before any ToolResult. *)
let test_parallel_tool_calls_and_upfront_tool_starts () =
  let registry = Tool_registry.create () in
  let in_flight = ref 0 in
  let max_in_flight = ref 0 in
  let counting_tool name : Tool.t =
    {
      name;
      description = "Concurrency probe";
      parameters_schema = `Assoc [];
      invoke =
        (fun ?context:_ _args ->
          let open Lwt.Syntax in
          incr in_flight;
          if !in_flight > !max_in_flight then max_in_flight := !in_flight;
          (* Yield repeatedly so other parallel tools have time to start. *)
          let* () = Lwt.pause () in
          let* () = Lwt.pause () in
          let* () = Lwt.pause () in
          decr in_flight;
          Lwt.return (name ^ ":done"));
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  Tool_registry.register registry (counting_tool "probe_a");
  Tool_registry.register registry (counting_tool "probe_b");
  Tool_registry.register registry (counting_tool "probe_c");
  let agent = Agent.create ~config:default_config ~tool_registry:registry () in
  let events = ref [] in
  let calls =
    [
      { Provider.id = "ca"; function_name = "probe_a"; arguments = "{}" };
      { Provider.id = "cb"; function_name = "probe_b"; arguments = "{}" };
      { Provider.id = "cc"; function_name = "probe_c"; arguments = "{}" };
    ]
  in
  Lwt_main.run
    (Agent.execute_tool_calls_stream agent ~db:None ~audit_enabled:false
       ~session_key:None calls ~on_chunk:(fun event ->
         events := event :: !events;
         Lwt.return_unit));
  let events = List.rev !events in
  Alcotest.(check bool)
    "concurrent execution observed (max_in_flight > 1)" true (!max_in_flight > 1);
  (* Up-front ToolStart contract: every ToolStart appears in the event
     stream BEFORE the first ToolResult. *)
  let rec first_result_index i = function
    | [] -> -1
    | Provider.ToolResult _ :: _ -> i
    | _ :: rest -> first_result_index (i + 1) rest
  in
  let first_result = first_result_index 0 events in
  let tool_start_indices =
    List.mapi (fun i e -> (i, e)) events
    |> List.filter_map (function
      | i, Provider.ToolStart _ -> Some i
      | _ -> None)
  in
  Alcotest.(check int) "3 ToolStart events" 3 (List.length tool_start_indices);
  Alcotest.(check bool)
    "all ToolStart before first ToolResult" true
    (List.for_all (fun i -> i < first_result) tool_start_indices);
  let tool_result_count =
    List.length
      (List.filter
         (function Provider.ToolResult _ -> true | _ -> false)
         events)
  in
  Alcotest.(check int) "3 ToolResult events" 3 tool_result_count;
  let all_success =
    List.for_all
      (function
        | Provider.ToolResult { is_error; _ } -> not is_error | _ -> true)
      events
  in
  Alcotest.(check bool) "all 3 results successful" true all_success;
  Alcotest.(check int) "counter drains to zero after batch" 0 !in_flight

(* B598: when a tool call sets forward_to_user:true in its args, the agent
   emits the full result via ToolOutputDelta before the ToolResult event, so
   the channel session can display it as its own message. *)
let test_forward_to_user_emits_tool_output_delta () =
  let registry = Tool_registry.create () in
  let tool =
    {
      Tool.name = "echo_back";
      description = "Returns its echo argument";
      parameters_schema =
        `Assoc
          [
            ("type", `String "object");
            ( "properties",
              `Assoc [ ("echo", `Assoc [ ("type", `String "string") ]) ] );
            ("required", `List [ `String "echo" ]);
          ];
      invoke =
        (fun ?context:_ args ->
          let open Yojson.Safe.Util in
          let s = try args |> member "echo" |> to_string with _ -> "(none)" in
          Lwt.return ("echoed: " ^ s));
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  Tool_registry.register registry tool;
  let agent = Agent.create ~config:default_config ~tool_registry:registry () in
  let with_flag =
    {
      Provider.id = "tc-fwd";
      function_name = "echo_back";
      arguments = {|{"echo":"hello","forward_to_user":true}|};
    }
  in
  let without_flag =
    {
      Provider.id = "tc-nofwd";
      function_name = "echo_back";
      arguments = {|{"echo":"hello"}|};
    }
  in
  let collect_for call =
    let events = ref [] in
    Lwt_main.run
      (Agent.execute_tool_calls_stream agent ~db:None ~audit_enabled:false
         ~session_key:None [ call ] ~on_chunk:(fun event ->
           events := event :: !events;
           Lwt.return_unit));
    List.rev !events
  in
  let events_fwd = collect_for with_flag in
  let events_no = collect_for without_flag in
  let count_output_deltas =
    List.fold_left
      (fun acc e ->
        match e with Provider.ToolOutputDelta _ -> acc + 1 | _ -> acc)
      0
  in
  Alcotest.(check int)
    "forward_to_user:true emits one ToolOutputDelta" 1
    (count_output_deltas events_fwd);
  Alcotest.(check int)
    "no flag emits zero ToolOutputDelta" 0
    (count_output_deltas events_no)

let test_loop_terminates () =
  with_fake_tool_loop_provider (fun config requests ->
      let tool_invocations = ref 0 in
      let registry = Tool_registry.create () in
      let tool =
        {
          Tool.name = "loop_tool";
          description = "Always succeeds";
          parameters_schema = `Assoc [];
          invoke =
            (fun ?context:_ _ ->
              incr tool_invocations;
              Lwt.return "ok");
          invoke_stream = None;
          risk_level = Tool.Low;
          deferred = false;
        }
      in
      Tool_registry.register registry tool;
      let agent = Agent.create ~config ~tool_registry:registry () in
      let response = Lwt_main.run (Agent.turn agent ~user_message:"hello" ()) in
      Alcotest.(check string)
        "turn stops at max tool iterations"
        "I've reached the maximum number of tool iterations. Here's what I was \
         trying to do: loop_tool"
        response;
      Alcotest.(check int) "provider called twice" 2 !requests;
      Alcotest.(check int) "tool executed once" 1 !tool_invocations)

(* Test: memory store and load roundtrip *)
let test_memory_roundtrip () =
  let db = Memory.init ~db_path:":memory:" () in
  let msg1 = Provider.make_message ~role:"user" ~content:"hello" in
  let msg2 = Provider.make_message ~role:"assistant" ~content:"hi there" in
  Memory.store_message ~db ~session_key:"test_session" msg1;
  Memory.store_message ~db ~session_key:"test_session" msg2;
  let loaded = Memory.load_history ~db ~session_key:"test_session" in
  Alcotest.(check int) "loaded 2 messages" 2 (List.length loaded);
  let first = List.nth loaded 0 in
  let second = List.nth loaded 1 in
  Alcotest.(check string) "first role" "user" first.role;
  Alcotest.(check string) "first content" "hello" first.content;
  Alcotest.(check string) "second role" "assistant" second.role;
  Alcotest.(check string) "second content" "hi there" second.content

(* Test: memory clear session *)
let test_memory_clear () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"s1"
    (Provider.make_message ~role:"user" ~content:"a");
  Memory.store_message ~db ~session_key:"s2"
    (Provider.make_message ~role:"user" ~content:"b");
  Memory.clear_session ~db ~session_key:"s1";
  let s1 = Memory.load_history ~db ~session_key:"s1" in
  let s2 = Memory.load_history ~db ~session_key:"s2" in
  Alcotest.(check int) "s1 cleared" 0 (List.length s1);
  Alcotest.(check int) "s2 untouched" 1 (List.length s2)

(* Test: memory list sessions *)
let test_memory_list_sessions () =
  let db = Memory.init ~db_path:":memory:" () in
  Memory.store_message ~db ~session_key:"alpha"
    (Provider.make_message ~role:"user" ~content:"a");
  Memory.store_message ~db ~session_key:"beta"
    (Provider.make_message ~role:"user" ~content:"b");
  Memory.store_message ~db ~session_key:"alpha"
    (Provider.make_message ~role:"assistant" ~content:"c");
  let sessions = Memory.list_sessions ~db in
  Alcotest.(check int) "2 unique sessions" 2 (List.length sessions);
  Alcotest.(check bool) "contains alpha" true (List.mem "alpha" sessions);
  Alcotest.(check bool) "contains beta" true (List.mem "beta" sessions)

(* Test: memory with tool calls *)
let test_memory_tool_calls () =
  let db = Memory.init ~db_path:":memory:" () in
  let tool_msg =
    {
      Provider.role = "assistant";
      content = "";
      content_parts = [];
      tool_calls =
        [
          {
            Provider.id = "call_123";
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
  Memory.store_message ~db ~session_key:"tool_test" tool_msg;
  let tool_result =
    Provider.make_tool_result ~tool_call_id:"call_123" ~name:"shell_exec"
      ~content:"file1.txt\nfile2.txt"
  in
  Memory.store_message ~db ~session_key:"tool_test" tool_result;
  let loaded = Memory.load_history ~db ~session_key:"tool_test" in
  Alcotest.(check int) "loaded 2 messages" 2 (List.length loaded);
  let first = List.nth loaded 0 in
  Alcotest.(check int) "first has 1 tool call" 1 (List.length first.tool_calls);
  let tc = List.nth first.tool_calls 0 in
  Alcotest.(check string) "tool call id" "call_123" tc.id;
  Alcotest.(check string) "tool call name" "shell_exec" tc.function_name;
  let second = List.nth loaded 1 in
  Alcotest.(check string) "tool result role" "tool" second.role;
  Alcotest.(check string)
    "tool result tool_call_id" "call_123"
    (match second.tool_call_id with Some s -> s | None -> "")

(* Test: config loader parses new fields *)
let test_config_new_fields () =
  let json_str =
    {|{
      "agent_defaults": {
        "primary_model": "test-model",
        "system_prompt": "Custom prompt",
        "max_tool_iterations": 5
      },
      "memory": {
        "backend": "sqlite",
        "db_path": "/tmp/test.db"
      },
      "runtime": {
        "docker_image": "clawq:test",
        "docker_container_name": "clawq-test",
        "docker_port": 4000
      },
      "tunnel": {
        "provider": "cloudflare",
        "enabled": true
      },
      "security": {
        "tools_enabled": true
      }
    }|}
  in
  let json = Yojson.Safe.from_string json_str in
  let config = Config_loader.parse_config json in
  Alcotest.(check string)
    "system_prompt" "Custom prompt" config.agent_defaults.system_prompt;
  Alcotest.(check int)
    "max_tool_iterations" 5 config.agent_defaults.max_tool_iterations;
  Alcotest.(check string) "db_path" "/tmp/test.db" config.memory.db_path;
  Alcotest.(check bool) "tools_enabled" true config.security.tools_enabled;
  Alcotest.(check string)
    "docker_image" "clawq:test" config.runtime.docker_image;
  Alcotest.(check int) "docker_port" 4000 config.runtime.docker_port;
  Alcotest.(check bool) "tunnel_enabled" true config.tunnel.enabled

(* Test: provider message JSON serialization *)
let test_provider_message_json () =
  let msg = Provider.make_message ~role:"user" ~content:"hello" in
  let json = Provider.message_to_json msg in
  let json_str = Yojson.Safe.to_string json in
  Alcotest.(check bool)
    "contains role" true
    (String.length json_str > 0
    &&
    let re = Str.regexp_string "user" in
    try
      ignore (Str.search_forward re json_str 0);
      true
    with Not_found -> false)

let test_provider_tool_result_json () =
  let msg =
    Provider.make_tool_result ~tool_call_id:"tc_1" ~name:"test"
      ~content:"result"
  in
  let json = Provider.message_to_json msg in
  let json_str = Yojson.Safe.to_string json in
  Alcotest.(check bool)
    "contains tool_call_id" true
    (let re = Str.regexp_string "tc_1" in
     try
       ignore (Str.search_forward re json_str 0);
       true
     with Not_found -> false)

(* Test: status command shows system prompt info *)
let test_status_shows_prompt () =
  let result = Command_bridge.handle [ "status" ] in
  Alcotest.(check bool)
    "status contains clawq status" true
    (String.length result > 0 && String.sub result 0 12 = "clawq status")

let test_config_nullclaw_compat_paths () =
  let json_str =
    {|{
      "models": {
        "providers": {
          "openrouter": {
            "api_key": "sk-test",
            "base_url": "https://openrouter.ai/api/v1",
            "default_model": "openai/gpt-4o"
          }
        }
      },
      "agents": {
        "defaults": {
          "model": {
            "primary": "openai/gpt-4.1"
          }
        }
      }
    }|}
  in
  let json = Yojson.Safe.from_string json_str in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  Alcotest.(check int)
    "providers from models.providers" 1
    (List.length config.providers);
  Alcotest.(check string)
    "primary model from agents.defaults.model.primary" "openai/gpt-4.1"
    config.agent_defaults.primary_model

let test_tool_search_basic () =
  let registry = Tool_registry.create () in
  let tool1 =
    {
      Tool.name = "file_read";
      description = "Read a file from disk";
      parameters_schema = `Assoc [ ("type", `String "object") ];
      invoke = (fun ?context:_ _ -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = true;
    }
  in
  let tool2 =
    {
      Tool.name = "web_search";
      description = "Search the web for information";
      parameters_schema = `Assoc [ ("type", `String "object") ];
      invoke = (fun ?context:_ _ -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = true;
    }
  in
  Tool_registry.register registry tool1;
  Tool_registry.register registry tool2;
  let results = Tool_registry.search registry ~query:"file read" in
  Alcotest.(check int) "search found 1 match" 1 (List.length results);
  Alcotest.(check string)
    "matched file_read" "file_read" (List.hd results).Tool.name

let test_tool_search_all_match () =
  let registry = Tool_registry.create () in
  let tool1 =
    {
      Tool.name = "file_read";
      description = "Read a file";
      parameters_schema = `Assoc [];
      invoke = (fun ?context:_ _ -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = true;
    }
  in
  let tool2 =
    {
      Tool.name = "file_write";
      description = "Write a file";
      parameters_schema = `Assoc [];
      invoke = (fun ?context:_ _ -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = true;
    }
  in
  Tool_registry.register registry tool1;
  Tool_registry.register registry tool2;
  let results = Tool_registry.search registry ~query:"file" in
  Alcotest.(check int) "search found 2 matches" 2 (List.length results)

let test_tool_deferred_json () =
  let registry = Tool_registry.create () in
  let tool1 =
    {
      Tool.name = "fast_tool";
      description = "Always loaded";
      parameters_schema =
        `Assoc [ ("type", `String "object"); ("properties", `Assoc []) ];
      invoke = (fun ?context:_ _ -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  let tool2 =
    {
      Tool.name = "slow_tool";
      description = "Deferred tool";
      parameters_schema =
        `Assoc [ ("type", `String "object"); ("properties", `Assoc []) ];
      invoke = (fun ?context:_ _ -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = true;
    }
  in
  Tool_registry.register registry tool1;
  Tool_registry.register registry tool2;
  let json = Tool_registry.to_openai_json_with_search registry in
  let json_str = Yojson.Safe.to_string json in
  (* tool_search entry is a proper function tool *)
  Alcotest.(check bool)
    "contains tool_search entry" true
    (let re = Str.regexp_string "tool_search" in
     try
       ignore (Str.search_forward re json_str 0);
       true
     with Not_found -> false);
  (* deferred tool has nested function structure, no defer_loading *)
  let deferred_json = Tool_registry.tool_to_deferred_json tool2 in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "deferred type is function" "function"
    (deferred_json |> member "type" |> to_string);
  let fn = deferred_json |> member "function" in
  Alcotest.(check string)
    "deferred function.name" "slow_tool"
    (fn |> member "name" |> to_string);
  Alcotest.(check string)
    "deferred function.description" "Deferred tool"
    (fn |> member "description" |> to_string);
  (* B614: deferred entries DO include parameters so that Anthropic-format
     providers can preserve required[]. The deferred attribute is meaningful
     only for tool discovery / search workflows. *)
  Alcotest.(check bool)
    "deferred entry includes parameters" true
    (fn |> member "parameters" <> `Null);
  (* no defer_loading field *)
  (match deferred_json |> member "defer_loading" with
  | `Null -> ()
  | _ -> Alcotest.fail "should not have defer_loading field");
  Alcotest.(check bool)
    "contains fast_tool with parameters" true
    (let re = Str.regexp_string "fast_tool" in
     try
       ignore (Str.search_forward re json_str 0);
       true
     with Not_found -> false)

let test_tool_search_empty_query () =
  let registry = Tool_registry.create () in
  let tool1 =
    {
      Tool.name = "file_read";
      description = "Read a file";
      parameters_schema = `Assoc [];
      invoke = (fun ?context:_ _ -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = true;
    }
  in
  Tool_registry.register registry tool1;
  let results = Tool_registry.search registry ~query:"" in
  Alcotest.(check int) "empty query returns empty" 0 (List.length results)

let test_tool_search_no_deferred_omits_search_entry () =
  let registry = Tool_registry.create () in
  let tool1 =
    {
      Tool.name = "fast_tool";
      description = "Always loaded";
      parameters_schema =
        `Assoc [ ("type", `String "object"); ("properties", `Assoc []) ];
      invoke = (fun ?context:_ _ -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  Tool_registry.register registry tool1;
  let json = Tool_registry.to_openai_json_with_search registry in
  let json_str = Yojson.Safe.to_string json in
  Alcotest.(check bool)
    "no tool_search when no deferred tools" false
    (let re = Str.regexp_string "tool_search" in
     try
       ignore (Str.search_forward re json_str 0);
       true
     with Not_found -> false)

let test_tool_search_deferred_only () =
  let registry = Tool_registry.create () in
  let tool1 =
    {
      Tool.name = "file_read";
      description = "Read a file from disk";
      parameters_schema = `Assoc [];
      invoke = (fun ?context:_ _ -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  let tool2 =
    {
      Tool.name = "file_write";
      description = "Write a file to disk";
      parameters_schema = `Assoc [];
      invoke = (fun ?context:_ _ -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = true;
    }
  in
  Tool_registry.register registry tool1;
  Tool_registry.register registry tool2;
  let results = Tool_registry.search registry ~query:"file" in
  Alcotest.(check int) "only deferred tool matched" 1 (List.length results);
  Alcotest.(check string)
    "matched deferred tool" "file_write" (List.hd results).Tool.name

let test_tool_search_config () =
  let json_str =
    {|{
      "agent_defaults": {
        "tool_search_enabled": true
      }
    }|}
  in
  let json = Yojson.Safe.from_string json_str in
  let config = Config_loader.parse_config json in
  Alcotest.(check bool)
    "tool_search_enabled" true config.agent_defaults.tool_search_enabled

let test_execute_tool_search_special_case () =
  let registry = Tool_registry.create () in
  let deferred_tool =
    {
      Tool.name = "hidden_tool";
      description = "A deferred searchable tool";
      parameters_schema =
        `Assoc [ ("type", `String "object"); ("properties", `Assoc []) ];
      invoke = (fun ?context:_ _ -> Lwt.return "hidden");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = true;
    }
  in
  Tool_registry.register registry deferred_tool;
  let config =
    {
      default_config with
      agent_defaults =
        { default_config.agent_defaults with tool_search_enabled = true };
    }
  in
  let agent = Agent.create ~config ~tool_registry:registry () in
  let call =
    {
      Provider.id = "search_call";
      function_name = "tool_search";
      arguments = {|{"query":"hidden"}|};
    }
  in
  Lwt_main.run
    (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
       ~session_key:None [ call ]);
  match agent.history with
  | result :: _ ->
      Alcotest.(check string) "tool result role" "tool" result.role;
      Alcotest.(check (option string))
        "tool result name" (Some "tool_search") result.name;
      Alcotest.(check bool)
        "tool_search output" true
        (Test_helpers.string_contains result.content "tool_search_output");
      Alcotest.(check bool)
        "found hidden tool" true
        (Test_helpers.string_contains result.content "hidden_tool")
  | [] -> Alcotest.fail "expected tool_search result in history"

let test_stuck_detector_uses_structured_tool_error () =
  let tool_error id =
    {
      (Provider.make_tool_result ~tool_call_id:id ~name:"shell_exec"
         ~content:"summarized failure without legacy prefix")
      with
      Provider.is_error = true;
    }
  in
  let history = [ tool_error "c3"; tool_error "c2"; tool_error "c1" ] in
  match Stuck_detector.check ~history ~iteration:3 ~max_iters:10 with
  | Stuck_detector.Definite signals ->
      let rendered = Stuck_detector.signals_to_string signals in
      Alcotest.(check bool)
        "consecutive structured errors detected" true
        (Test_helpers.string_contains rendered "ConsecutiveErrors")
  | Stuck_detector.Clear | Stuck_detector.Suspicious _ ->
      Alcotest.fail "expected definite stuck signal for structured tool errors"

let test_malformed_tool_arguments_do_not_invoke () =
  let registry = Tool_registry.create () in
  let invoked = ref false in
  let tool =
    {
      Tool.name = "needs_json";
      description = "Requires JSON args";
      parameters_schema =
        `Assoc
          [
            ("type", `String "object");
            ( "properties",
              `Assoc [ ("path", `Assoc [ ("type", `String "string") ]) ] );
            ("required", `List [ `String "path" ]);
          ];
      invoke =
        (fun ?context:_ _ ->
          invoked := true;
          Lwt.return "should not run");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  Tool_registry.register registry tool;
  let agent = Agent.create ~config:default_config ~tool_registry:registry () in
  let call =
    { Provider.id = "bad_args"; function_name = "needs_json"; arguments = "" }
  in
  Lwt_main.run
    (Agent.execute_tool_calls agent ~db:None ~audit_enabled:false
       ~session_key:None [ call ]);
  Alcotest.(check bool) "tool not invoked" false !invoked;
  match agent.history with
  | result :: _ ->
      Alcotest.(check bool) "marked error" true result.is_error;
      Alcotest.(check bool)
        "parse error surfaced" true
        (Test_helpers.string_contains result.content
           "failed to parse arguments as JSON")
  | [] -> Alcotest.fail "expected error result in history"

let suite =
  [
    Alcotest.test_case "system prompt from config" `Quick
      test_system_prompt_from_config;
    Alcotest.test_case "default system prompt" `Quick test_default_system_prompt;
    Alcotest.test_case "tool registry serialization" `Quick
      test_tool_registry_serialization;
    Alcotest.test_case "tool registry find" `Quick test_tool_registry_find;
    Alcotest.test_case "tool invocation" `Quick test_tool_invocation;
    Alcotest.test_case "streamed raw error is classified" `Quick
      test_execute_tool_calls_stream_classifies_raw_error_result;
    Alcotest.test_case "streamed tool result is bounded" `Quick
      test_execute_tool_calls_stream_bounds_final_result;
    Alcotest.test_case
      "B607: tool calls execute in parallel with up-front ToolStart" `Quick
      test_parallel_tool_calls_and_upfront_tool_starts;
    Alcotest.test_case "B598: forward_to_user emits ToolOutputDelta" `Quick
      test_forward_to_user_emits_tool_output_delta;
    Alcotest.test_case "loop terminates" `Quick test_loop_terminates;
    Alcotest.test_case "memory roundtrip" `Quick test_memory_roundtrip;
    Alcotest.test_case "memory clear" `Quick test_memory_clear;
    Alcotest.test_case "memory list sessions" `Quick test_memory_list_sessions;
    Alcotest.test_case "memory tool calls" `Quick test_memory_tool_calls;
    Alcotest.test_case "config new fields" `Quick test_config_new_fields;
    Alcotest.test_case "provider message json" `Quick test_provider_message_json;
    Alcotest.test_case "provider tool result json" `Quick
      test_provider_tool_result_json;
    Alcotest.test_case "status shows prompt" `Quick test_status_shows_prompt;
    Alcotest.test_case "config nullclaw compat paths" `Quick
      test_config_nullclaw_compat_paths;
    Alcotest.test_case "tool search basic" `Quick test_tool_search_basic;
    Alcotest.test_case "tool search all match" `Quick test_tool_search_all_match;
    Alcotest.test_case "tool deferred json" `Quick test_tool_deferred_json;
    Alcotest.test_case "tool search config" `Quick test_tool_search_config;
    Alcotest.test_case "tool search empty query" `Quick
      test_tool_search_empty_query;
    Alcotest.test_case "no deferred omits search entry" `Quick
      test_tool_search_no_deferred_omits_search_entry;
    Alcotest.test_case "tool search deferred only" `Quick
      test_tool_search_deferred_only;
    Alcotest.test_case "tool_search executes special-case" `Quick
      test_execute_tool_search_special_case;
    Alcotest.test_case "stuck detector uses structured tool errors" `Quick
      test_stuck_detector_uses_structured_tool_error;
    Alcotest.test_case "malformed tool args do not invoke" `Quick
      test_malformed_tool_arguments_do_not_invoke;
  ]

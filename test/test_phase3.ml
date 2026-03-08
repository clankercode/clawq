let default_config = Runtime_config.default

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
  let config = Config_loader.parse_config json in
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
  (* no parameters key in deferred *)
  (match fn |> member "parameters" with
  | `Null -> ()
  | _ -> Alcotest.fail "deferred should not have parameters");
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

let suite =
  [
    Alcotest.test_case "system prompt from config" `Quick
      test_system_prompt_from_config;
    Alcotest.test_case "default system prompt" `Quick test_default_system_prompt;
    Alcotest.test_case "tool registry serialization" `Quick
      test_tool_registry_serialization;
    Alcotest.test_case "tool registry find" `Quick test_tool_registry_find;
    Alcotest.test_case "tool invocation" `Quick test_tool_invocation;
    Alcotest.test_case "streamed tool result is bounded" `Quick
      test_execute_tool_calls_stream_bounds_final_result;
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
  ]

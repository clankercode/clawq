let default_config = Runtime_config.default

let contains ~needle haystack =
  let re = Str.regexp_string needle in
  try
    ignore (Str.search_forward re haystack 0);
    true
  with Not_found -> false

(* Test: system prompt from config *)
let test_system_prompt_from_config () =
  let config =
    {
      default_config with
      agent_defaults =
        {
          default_config.agent_defaults with
          system_prompt = "You are a test bot.";
        };
    }
  in
  let agent = Agent.create ~config () in
  Alcotest.(check bool)
    "system prompt from config" true
    (contains ~needle:"You are a test bot." agent.system_prompt)

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
        `Assoc
          [
            ("type", `String "object");
            ("properties", `Assoc []);
          ];
      invoke = (fun _ -> Lwt.return "test result");
      risk_level = Tool.Low;
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
      invoke = (fun _ -> Lwt.return "ok");
      risk_level = Tool.Low;
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
        (fun args ->
          invoked := true;
          let open Yojson.Safe.Util in
          let v =
            try args |> member "value" |> to_string with _ -> "default"
          in
          Lwt.return ("got: " ^ v));
      risk_level = Tool.Low;
    }
  in
  let result =
    Lwt_main.run
      (test_tool.invoke (`Assoc [ ("value", `String "hello") ]))
  in
  Alcotest.(check bool) "tool was invoked" true !invoked;
  Alcotest.(check string) "tool result" "got: hello" result

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
  Alcotest.(check bool)
    "contains alpha" true (List.mem "alpha" sessions);
  Alcotest.(check bool)
    "contains beta" true (List.mem "beta" sessions)

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
        "model_priority": [
          {"provider": "groq", "model": "priority-model"},
          "fallback-model"
        ],
        "system_prompt": "Custom prompt",
        "max_tool_interactions": 5
      },
      "memory": {
        "backend": "sqlite",
        "db_path": "/tmp/test.db"
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
  Alcotest.(check string)
    "effective model from priority list" "priority-model"
    (Runtime_config.effective_primary_model config.agent_defaults);
  Alcotest.(check (option string))
    "effective provider from priority list" (Some "groq")
    (Runtime_config.effective_primary_provider config.agent_defaults);
  Alcotest.(check int)
    "max_tool_iterations" 5 config.agent_defaults.max_tool_iterations;
  Alcotest.(check string) "db_path" "/tmp/test.db" config.memory.db_path;
  Alcotest.(check bool) "tools_enabled" true config.security.tools_enabled

(* Test: provider message JSON serialization *)
let test_provider_message_json () =
  let msg = Provider.make_message ~role:"user" ~content:"hello" in
  let json = Provider.message_to_json msg in
  let json_str = Yojson.Safe.to_string json in
  Alcotest.(check bool)
    "contains role" true
    (String.length json_str > 0
     && let re = Str.regexp_string "user" in
        try ignore (Str.search_forward re json_str 0); true
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
     try ignore (Str.search_forward re json_str 0); true
     with Not_found -> false)

(* Test: status command shows system prompt info *)
let test_status_shows_prompt () =
  let result = Command_bridge.handle [ "status" ] in
  Alcotest.(check bool)
    "status contains clawq status" true
    (String.length result > 0
     && String.sub result 0 12 = "clawq status")

let test_doctor_warns_model_priority_provider_key_missing () =
  let cfg =
    {
      default_config with
      providers =
        [
          ( "groq",
            {
              Runtime_config.api_key = "";
              base_url = Some "https://api.groq.com/openai/v1";
              default_model = None;
            } );
        ];
      agent_defaults =
        {
          default_config.agent_defaults with
          model_priority =
            [ { Runtime_config.provider = Some "groq"; model = "openai/gpt-oss-120b" } ];
          primary_model = "openai/gpt-oss-120b";
        };
    }
  in
  let issues = Command_bridge.doctor_issues cfg in
  let joined = String.concat "\n" issues in
  Alcotest.(check bool)
    "doctor warns when priority provider key missing" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string
               "model_priority[0] selects provider 'groq' for model 'openai/gpt-oss-120b' but provider has no API key")
            joined 0);
       true
     with Not_found -> false)

let test_prompt_prefers_ego_over_soul () =
  let tmp = Filename.temp_file "clawq_ws" ".tmp" in
  Sys.remove tmp;
  Unix.mkdir tmp 0o755;
  let ego_path = Filename.concat tmp "EGO.md" in
  let soul_path = Filename.concat tmp "SOUL.md" in
  let oc = open_out ego_path in
  output_string oc "EGO says hello";
  close_out oc;
  let oc2 = open_out soul_path in
  output_string oc2 "SOUL says hello";
  close_out oc2;
  let cfg =
    {
      default_config with
      workspace = tmp;
      agent_defaults =
        { default_config.agent_defaults with system_prompt = "Prelude" };
    }
  in
  let prompt = Prompt_builder.build ~config:cfg ~tool_registry:None in
  Alcotest.(check bool) "prompt contains EGO" true
    (contains ~needle:"EGO says hello" prompt);
  Alcotest.(check bool) "prompt omits SOUL when EGO exists" false
    (contains ~needle:"SOUL says hello" prompt)

let suite =
  [
    Alcotest.test_case "system prompt from config" `Quick
      test_system_prompt_from_config;
    Alcotest.test_case "default system prompt" `Quick
      test_default_system_prompt;
    Alcotest.test_case "tool registry serialization" `Quick
      test_tool_registry_serialization;
    Alcotest.test_case "tool registry find" `Quick test_tool_registry_find;
    Alcotest.test_case "tool invocation" `Quick test_tool_invocation;
    Alcotest.test_case "memory roundtrip" `Quick test_memory_roundtrip;
    Alcotest.test_case "memory clear" `Quick test_memory_clear;
    Alcotest.test_case "memory list sessions" `Quick
      test_memory_list_sessions;
    Alcotest.test_case "memory tool calls" `Quick test_memory_tool_calls;
    Alcotest.test_case "config new fields" `Quick test_config_new_fields;
    Alcotest.test_case "provider message json" `Quick
      test_provider_message_json;
    Alcotest.test_case "provider tool result json" `Quick
      test_provider_tool_result_json;
    Alcotest.test_case "status shows prompt" `Quick test_status_shows_prompt;
    Alcotest.test_case "doctor warns model priority provider missing key" `Quick
      test_doctor_warns_model_priority_provider_key_missing;
    Alcotest.test_case "prompt prefers EGO over SOUL" `Quick
      test_prompt_prefers_ego_over_soul;
  ]

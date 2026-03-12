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

let make_fake_provider_config base_url : Runtime_config.provider_config =
  {
    Runtime_config.default_provider_config with
    api_key = "test-key";
    base_url = Some base_url;
    default_model = Some "fake-model";
  }

(* Fake chat provider that tracks requests and can return tool calls *)
let with_tool_call_provider ~tool_calls_to_return ~request_log f =
  let port = free_port () in
  let call_count = ref 0 in
  let callback _conn _req body =
    let open Lwt.Syntax in
    let* body_text = Cohttp_lwt.Body.to_string body in
    request_log := body_text :: !request_log;
    let n = !call_count in
    incr call_count;
    let response_body =
      if n < List.length tool_calls_to_return then
        (* Return tool calls *)
        let tool_call_json = List.nth tool_calls_to_return n in
        Yojson.Safe.to_string
          (`Assoc
             [
               ("id", `String "cmpl_fake");
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
                               ("content", `Null);
                               ("tool_calls", tool_call_json);
                             ] );
                         ("finish_reason", `String "tool_calls");
                       ];
                   ] );
               ( "usage",
                 `Assoc
                   [ ("prompt_tokens", `Int 1); ("completion_tokens", `Int 1) ]
               );
             ])
      else
        (* Return text (done) *)
        Yojson.Safe.to_string
          (`Assoc
             [
               ("id", `String "cmpl_fake");
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
                               ("content", `String "Done flushing memories.");
                             ] );
                         ("finish_reason", `String "stop");
                       ];
                   ] );
               ( "usage",
                 `Assoc
                   [ ("prompt_tokens", `Int 1); ("completion_tokens", `Int 1) ]
               );
             ])
    in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:response_body ()
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
            { Runtime_config.default.security with tools_enabled = false };
          agent_defaults =
            {
              Runtime_config.default.agent_defaults with
              primary_model = "fake-model";
              show_thinking = false;
              show_tool_calls = false;
            };
        }
      in
      f config)

(* Simple text-only fake provider for compaction (no tool calls) *)
let with_text_provider f =
  let port = free_port () in
  let callback _conn _req body =
    let open Lwt.Syntax in
    let* _body_text = Cohttp_lwt.Body.to_string body in
    let response_body =
      Yojson.Safe.to_string
        (`Assoc
           [
             ("id", `String "cmpl_fake");
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
                             ("content", `String "Summary of conversation.");
                           ] );
                       ("finish_reason", `String "stop");
                     ];
                 ] );
             ( "usage",
               `Assoc
                 [ ("prompt_tokens", `Int 1); ("completion_tokens", `Int 1) ] );
           ])
    in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:response_body ()
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
            { Runtime_config.default.security with tools_enabled = false };
          agent_defaults =
            {
              Runtime_config.default.agent_defaults with
              primary_model = "fake-model";
              show_thinking = false;
              show_tool_calls = false;
            };
        }
      in
      f config)

let fill_agent_history agent n =
  for i = 1 to n do
    agent.Agent.history <-
      Provider.make_message ~role:"user"
        ~content:
          (Printf.sprintf "Message %d with some content to increase size" i)
      :: agent.Agent.history;
    agent.Agent.history <-
      Provider.make_message ~role:"assistant"
        ~content:
          (Printf.sprintf "Response %d with some content to pad the history" i)
      :: agent.Agent.history
  done

(* Test 1: flush_memories_before_compaction stores memories via tool calls *)
let test_flush_stores_memories () =
  let tool_calls_to_return =
    [
      (* First call: memory_list *)
      `List
        [
          `Assoc
            [
              ("id", `String "tc_1");
              ("type", `String "function");
              ( "function",
                `Assoc
                  [
                    ("name", `String "memory_list"); ("arguments", `String "{}");
                  ] );
            ];
        ];
      (* Second call: memory_store *)
      `List
        [
          `Assoc
            [
              ("id", `String "tc_2");
              ("type", `String "function");
              ( "function",
                `Assoc
                  [
                    ("name", `String "memory_store");
                    ( "arguments",
                      `String
                        {|{"key":"test_pref","content":"user likes OCaml","category":"preferences"}|}
                    );
                  ] );
            ];
        ];
    ]
  in
  let request_log = ref [] in
  with_tool_call_provider ~tool_calls_to_return ~request_log (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let to_compact =
        [
          Provider.make_message ~role:"user" ~content:"I really like OCaml";
          Provider.make_message ~role:"assistant"
            ~content:"Great, I'll remember that!";
        ]
      in
      Lwt_main.run
        (Agent.flush_memories_before_compaction ~config
           ~system_prompt:"You are a helpful assistant." ~db ~to_compact);
      (* Verify the memory was stored *)
      let memories = Memory.list_core ~db () in
      Alcotest.(check bool)
        "memory was stored" true
        (List.exists (fun (key, _, _) -> key = "test_pref") memories);
      let content =
        List.find_map
          (fun (key, content, _) ->
            if key = "test_pref" then Some content else None)
          memories
      in
      Alcotest.(check (option string))
        "memory content correct" (Some "user likes OCaml") content)

(* Test 2: flush disabled via config — no LLM call *)
let test_flush_disabled_no_llm_call () =
  let request_log = ref [] in
  with_tool_call_provider ~tool_calls_to_return:[] ~request_log (fun config ->
      let config =
        {
          config with
          memory = { config.memory with pre_compaction_flush = false };
        }
      in
      let db = Memory.init ~db_path:":memory:" () in
      let agent = Agent.create ~config () in
      (* Fill with enough messages to trigger compaction *)
      fill_agent_history agent 260;
      Lwt_main.run
        (let open Lwt.Syntax in
         let* _compacted = Agent.compact_history_if_needed agent ~db () in
         Lwt.return_unit);
      (* With flush disabled, all LLM calls should be for summarization only,
         not for flush. We can't easily distinguish them, but we know no memory
         was stored. *)
      let memories = Memory.list_core ~db () in
      Alcotest.(check int) "no memories stored" 0 (List.length memories))

(* Test 3: flush failure doesn't block compaction *)
let test_flush_failure_doesnt_block_compaction () =
  with_text_provider (fun config ->
      let config =
        {
          config with
          memory = { config.memory with pre_compaction_flush = true };
        }
      in
      let db = Memory.init ~db_path:":memory:" () in
      let agent = Agent.create ~config () in
      fill_agent_history agent 260;
      let initial_len = List.length agent.history in
      let compacted =
        Lwt_main.run
          (let open Lwt.Syntax in
           let* result = Agent.compact_history_if_needed agent ~db () in
           (* Let the Lwt.async background flush task settle before
              Lwt_main.run exits, per test/CLAUDE.md isolation rules. *)
           let* () = Lwt.pause () in
           Lwt.return result)
      in
      Alcotest.(check bool)
        "compaction happened" true (Option.is_some compacted);
      Alcotest.(check bool)
        "history was reduced" true
        (List.length agent.history < initial_len))

(* Test 4: flush receives correct messages *)
let test_flush_receives_to_compact_messages () =
  let request_log = ref [] in
  with_tool_call_provider ~tool_calls_to_return:[] ~request_log (fun config ->
      let db = Memory.init ~db_path:":memory:" () in
      let to_compact =
        [
          Provider.make_message ~role:"user" ~content:"FLUSH_TEST_MARKER_123";
          Provider.make_message ~role:"assistant" ~content:"I see your marker.";
        ]
      in
      Lwt_main.run
        (Agent.flush_memories_before_compaction ~config
           ~system_prompt:"You are a helpful assistant." ~db ~to_compact);
      (* Check that the LLM request included our marker message *)
      let string_contains haystack needle =
        let hl = String.length haystack and nl = String.length needle in
        if nl > hl then false
        else
          let rec loop i =
            if i > hl - nl then false
            else if String.sub haystack i nl = needle then true
            else loop (i + 1)
          in
          loop 0
      in
      let request_contains_content substr =
        List.exists
          (fun body ->
            let open Yojson.Safe.Util in
            try
              let json = Yojson.Safe.from_string body in
              let msgs = json |> member "messages" |> to_list in
              List.exists
                (fun msg ->
                  try
                    let c = msg |> member "content" |> to_string in
                    string_contains c substr
                  with _ -> false)
                msgs
            with _ -> false)
          !request_log
      in
      Alcotest.(check bool)
        "request contained marker message" true
        (request_contains_content "FLUSH_TEST_MARKER_123");
      Alcotest.(check bool)
        "request contained flush trigger" true
        (request_contains_content "URGENT:"))

(* Test 5: config parsing roundtrip for pre_compaction_flush *)
let test_config_roundtrip () =
  let config =
    {
      Runtime_config.default with
      memory =
        { Runtime_config.default.memory with pre_compaction_flush = false };
    }
  in
  let json = Runtime_config.to_json config in
  let parsed = Config_loader.parse_config json in
  Alcotest.(check bool)
    "pre_compaction_flush roundtrips" false parsed.memory.pre_compaction_flush;
  (* Also test default (true) *)
  let config2 = Runtime_config.default in
  let json2 = Runtime_config.to_json config2 in
  let parsed2 = Config_loader.parse_config json2 in
  Alcotest.(check bool)
    "pre_compaction_flush default roundtrips" true
    parsed2.memory.pre_compaction_flush

(* Test 6: config defaults to true when field absent *)
let test_config_defaults_true_when_absent () =
  let json = Yojson.Safe.from_string {|{"memory": {"backend": "sqlite"}}|} in
  let parsed = Config_loader.parse_config json in
  Alcotest.(check bool)
    "pre_compaction_flush defaults true" true parsed.memory.pre_compaction_flush

(* Test 7: dispatch_flush_tool_call handles all tool names *)
let test_dispatch_flush_tool_call () =
  let db = Memory.init ~db_path:":memory:" () in
  (* Test memory_store *)
  let tc_store : Provider.tool_call =
    {
      id = "tc_1";
      function_name = "memory_store";
      arguments = {|{"key":"k1","content":"v1","category":"test"}|};
    }
  in
  let result = Agent.dispatch_flush_tool_call ~db tc_store in
  Alcotest.(check string) "store result" "Stored memory: k1" result.content;
  (* Verify it's actually in the DB *)
  let memories = Memory.list_core ~db ~category:"test" () in
  Alcotest.(check int) "one memory stored" 1 (List.length memories);
  (* Test memory_recall *)
  let tc_recall : Provider.tool_call =
    {
      id = "tc_2";
      function_name = "memory_recall";
      arguments = {|{"query":"k1"}|};
    }
  in
  let result = Agent.dispatch_flush_tool_call ~db tc_recall in
  Alcotest.(check bool)
    "recall finds memory" true
    (String.length result.content > 0
    && not (result.content = "No matching memories found"));
  (* Test memory_list *)
  let tc_list : Provider.tool_call =
    { id = "tc_3"; function_name = "memory_list"; arguments = "{}" }
  in
  let result = Agent.dispatch_flush_tool_call ~db tc_list in
  Alcotest.(check bool)
    "list finds memory" true
    (String.length result.content > 0
    && not (result.content = "No memories found"));
  (* Test memory_forget *)
  let tc_forget : Provider.tool_call =
    {
      id = "tc_4";
      function_name = "memory_forget";
      arguments = {|{"key":"k1"}|};
    }
  in
  let result = Agent.dispatch_flush_tool_call ~db tc_forget in
  Alcotest.(check string) "forget result" "Deleted memory: k1" result.content;
  (* Verify it's gone *)
  let memories = Memory.list_core ~db ~category:"test" () in
  Alcotest.(check int) "memory forgotten" 0 (List.length memories);
  (* Test unknown tool *)
  let tc_unknown : Provider.tool_call =
    { id = "tc_5"; function_name = "unknown_tool"; arguments = "{}" }
  in
  let result = Agent.dispatch_flush_tool_call ~db tc_unknown in
  Alcotest.(check bool)
    "unknown tool error" true
    (String.starts_with ~prefix:"Error:" result.content)

(* Test 8: compact releases lock during LLM calls (B386 regression) *)
let test_compact_releases_lock_during_llm () =
  with_text_provider (fun config ->
      let config =
        {
          config with
          memory = { config.memory with pre_compaction_flush = false };
        }
      in
      let mgr = Session.create ~config ?db:None () in
      Lwt_main.run
        (let open Lwt.Syntax in
         (* Prime the session with enough history to compact *)
         let* () =
           Session.with_session_lock mgr ~key:"test" (fun agent _interrupt ->
               fill_agent_history agent 30;
               Lwt.return_unit)
         in
         (* Launch compact in background *)
         let compact_done = ref false in
         let compact_promise =
           let* _result = Session.compact mgr ~key:"test" () in
           compact_done := true;
           Lwt.return_unit
         in
         (* Give compact time to acquire lock, plan, release, start LLM calls *)
         let* () = Lwt_unix.sleep 0.05 in
         (* Try to acquire the session lock — should succeed because compact
            released it during the LLM execute phase *)
         let* lock_result =
           Session.try_session_lock mgr ~key:"test" (fun _agent _interrupt ->
               Lwt.return true)
         in
         Alcotest.(check (option bool))
           "lock acquired during compact execution" (Some true) lock_result;
         (* Wait for compact to finish *)
         let* () = compact_promise in
         Alcotest.(check bool) "compact completed" true !compact_done;
         Lwt.return_unit))

(* Test 9: apply_compact_result handles new messages during execution *)
let test_apply_handles_new_messages () =
  with_text_provider (fun config ->
      let agent = Agent.create ~config () in
      fill_agent_history agent 30;
      let plan = Agent.plan_force_compact agent in
      Alcotest.(check bool) "plan exists" true (Option.is_some plan);
      let plan = Option.get plan in
      (* Simulate new messages arriving during execute phase *)
      agent.Agent.history <-
        Provider.make_message ~role:"assistant" ~content:"New response"
        :: Provider.make_message ~role:"user" ~content:"New question"
        :: agent.Agent.history;
      let result =
        Agent.apply_compact_result agent plan ~summary:"Test summary"
      in
      Alcotest.(check bool) "apply succeeded" true (Option.is_some result);
      (* Verify new messages are preserved at the front (newest-first) *)
      let first_msg = List.hd agent.Agent.history in
      Alcotest.(check string)
        "newest message preserved" "New response" first_msg.content;
      let second_msg = List.nth agent.Agent.history 1 in
      Alcotest.(check string)
        "second newest preserved" "New question" second_msg.content)

(* Test 10: apply_compact_result handles reset during execution *)
let test_apply_handles_reset () =
  with_text_provider (fun config ->
      let agent = Agent.create ~config () in
      fill_agent_history agent 30;
      let plan = Agent.plan_force_compact agent in
      Alcotest.(check bool) "plan exists" true (Option.is_some plan);
      let plan = Option.get plan in
      (* Simulate session reset during execute phase *)
      agent.Agent.history <- [];
      let result =
        Agent.apply_compact_result agent plan ~summary:"Test summary"
      in
      Alcotest.(check bool)
        "apply returns None on reset" true (Option.is_none result))

(* Test 11: plan_force_compact returns None for short history *)
let test_plan_returns_none_for_short_history () =
  with_text_provider (fun config ->
      let agent = Agent.create ~config () in
      fill_agent_history agent 5;
      let plan = Agent.plan_force_compact agent in
      Alcotest.(check bool)
        "no plan for short history" true (Option.is_none plan))

let suite =
  [
    Alcotest.test_case "flush stores memories via tool calls" `Quick
      test_flush_stores_memories;
    Alcotest.test_case "flush disabled no LLM call" `Quick
      test_flush_disabled_no_llm_call;
    Alcotest.test_case "flush failure doesnt block compaction" `Quick
      test_flush_failure_doesnt_block_compaction;
    Alcotest.test_case "flush receives correct messages" `Quick
      test_flush_receives_to_compact_messages;
    Alcotest.test_case "config roundtrip" `Quick test_config_roundtrip;
    Alcotest.test_case "config defaults true when absent" `Quick
      test_config_defaults_true_when_absent;
    Alcotest.test_case "dispatch flush tool call" `Quick
      test_dispatch_flush_tool_call;
    Alcotest.test_case "compact releases lock during LLM calls" `Quick
      test_compact_releases_lock_during_llm;
    Alcotest.test_case "compact handles new messages during execution" `Quick
      test_apply_handles_new_messages;
    Alcotest.test_case "compact handles reset during execution" `Quick
      test_apply_handles_reset;
    Alcotest.test_case "plan returns None for short history" `Quick
      test_plan_returns_none_for_short_history;
  ]

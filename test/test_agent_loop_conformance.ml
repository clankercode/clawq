(* Test conformance between Coq-extracted AgentLoop and native OCaml implementations *)

let make_user content = Provider.make_message ~role:"user" ~content
let make_assistant content = Provider.make_message ~role:"assistant" ~content

let make_assistant_with_calls calls =
  {
    Provider.role = "assistant";
    Provider.content = "";
    Provider.content_parts = [];
    Provider.tool_calls = calls;
    Provider.tool_call_id = None;
    Provider.name = None;
    Provider.provider_response_items_json = None;
    Provider.thinking = None;
    Provider.is_error = false;
  }

let make_tool_result id content =
  {
    Provider.role = "tool";
    Provider.content;
    Provider.content_parts = [];
    Provider.tool_calls = [];
    Provider.tool_call_id = Some id;
    Provider.name = None;
    Provider.provider_response_items_json = None;
    Provider.thinking = None;
    Provider.is_error = false;
  }

let make_tool_result_with_name id name content =
  {
    Provider.role = "tool";
    Provider.content;
    Provider.content_parts = [];
    Provider.tool_calls = [];
    Provider.tool_call_id = Some id;
    Provider.name = Some name;
    Provider.provider_response_items_json = None;
    Provider.thinking = None;
    Provider.is_error = false;
  }

let make_tool_call id name =
  { Provider.id; Provider.function_name = name; Provider.arguments = "{}" }

(* Test: collect_tool_call_ids matches *)

let test_collect_tool_call_ids_basic () =
  let messages =
    [
      make_user "hello";
      make_assistant_with_calls [ make_tool_call "call_1" "test_tool" ];
      make_tool_result "call_1" "result";
      make_assistant_with_calls
        [ make_tool_call "call_2" "tool2"; make_tool_call "call_3" "tool3" ];
      make_tool_result "call_2" "r2";
      make_tool_result "call_3" "r3";
    ]
  in
  let _coq, _native, equal =
    Agent_loop_conformance.conformance_collect_tool_call_ids messages
  in
  Alcotest.(check bool) "collect_tool_call_ids matches" true equal

let test_collect_tool_call_ids_empty () =
  let messages = [ make_user "hi"; make_assistant "there" ] in
  let _coq, _native, equal =
    Agent_loop_conformance.conformance_collect_tool_call_ids messages
  in
  Alcotest.(check bool) "collect_tool_call_ids empty matches" true equal

(* Test: collect_tool_result_ids matches *)

let test_collect_tool_result_ids_basic () =
  let messages =
    [
      make_assistant_with_calls
        [ make_tool_call "c1" "t"; make_tool_call "c2" "t" ];
      make_tool_result "c1" "r1";
      make_tool_result "c2" "r2";
      make_tool_result "orphan" "orphan_result";
    ]
  in
  let _coq, _native, equal =
    Agent_loop_conformance.conformance_collect_tool_result_ids messages
  in
  Alcotest.(check bool) "collect_tool_result_ids matches" true equal

(* Test: ensure_tool_group_integrity removes orphan tool results *)

let test_ensure_tool_group_integrity_orphan_result () =
  let messages =
    [
      make_user "q";
      make_assistant_with_calls [ make_tool_call "c1" "t" ];
      make_tool_result "c1" "r1";
      make_tool_result "orphan" "orphan_result";
    ]
  in
  let coq, native, equal =
    Agent_loop_conformance.conformance_ensure_tool_group_integrity messages
  in
  Alcotest.(check bool) "ensure_tool_group_integrity matches" true equal;
  Alcotest.(check int) "coq removes orphan" 3 (List.length coq);
  Alcotest.(check int) "native removes orphan" 3 (List.length native)

(* Test: ensure_tool_group_integrity strips dangling tool calls *)

let test_ensure_tool_group_integrity_dangling_call () =
  let messages =
    [
      make_user "q";
      make_assistant_with_calls
        [ make_tool_call "c1" "t"; make_tool_call "dangling" "t" ];
      make_tool_result "c1" "r1";
    ]
  in
  let coq, native, equal =
    Agent_loop_conformance.conformance_ensure_tool_group_integrity messages
  in
  Alcotest.(check bool)
    "ensure_tool_group_integrity matches (dangling)" true equal;
  (* Both should keep the message but with only the non-dangling call *)
  let coq_assistant =
    List.find
      (fun m -> m.Provider.role = "assistant" && m.Provider.tool_calls <> [])
      coq
  in
  Alcotest.(check int)
    "coq strips dangling call" 1
    (List.length coq_assistant.Provider.tool_calls)

(* Test: trim_history preserves integrity *)

let test_trim_history_basic () =
  let messages =
    [
      make_user "1";
      make_assistant_with_calls [ make_tool_call "c1" "t" ];
      make_tool_result "c1" "r1";
      make_user "2";
      make_assistant_with_calls [ make_tool_call "c2" "t" ];
      make_tool_result "c2" "r2";
      make_user "3";
    ]
  in
  let coq, native, equal =
    Agent_loop_conformance.conformance_trim_history 4 messages
  in
  Alcotest.(check bool) "trim_history matches" true equal;
  Alcotest.(check int) "coq trims to 4" 4 (List.length coq);
  Alcotest.(check int) "native trims to 4" 4 (List.length native)

(* Test: force_compress_history preserves integrity *)

let test_force_compress_history_basic () =
  let messages =
    [
      make_user "1";
      make_assistant_with_calls [ make_tool_call "c1" "t" ];
      make_tool_result "c1" "r1";
      make_user "2";
      make_assistant_with_calls [ make_tool_call "c2" "t" ];
      make_tool_result "c2" "r2";
      make_user "3";
    ]
  in
  let coq, native, equal =
    Agent_loop_conformance.conformance_force_compress_history 3 messages
  in
  Alcotest.(check bool) "force_compress_history matches" true equal;
  Alcotest.(check int) "coq compresses to 3" 3 (List.length coq);
  Alcotest.(check int) "native compresses to 3" 3 (List.length native)

(* Test: complex scenario with multiple orphans *)

let test_complex_orphan_scenario () =
  let messages =
    [
      make_user "start";
      make_assistant_with_calls
        [ make_tool_call "c1" "t"; make_tool_call "orphan_call" "t" ];
      make_tool_result "c1" "r1";
      make_tool_result "orphan_result" "r";
      make_user "middle";
      make_assistant_with_calls [ make_tool_call "c2" "t" ];
      make_tool_result "c2" "r2";
    ]
  in
  let coq, native, equal =
    Agent_loop_conformance.conformance_ensure_tool_group_integrity messages
  in
  Alcotest.(check bool) "complex scenario matches" true equal;
  (* Should remove orphan_result and strip orphan_call *)
  Alcotest.(check int) "coq result length" 6 (List.length coq);
  Alcotest.(check int) "native result length" 6 (List.length native)

(* Test: tool names are preserved through Coq roundtrip (B363 fix) *)

let test_tool_name_preservation () =
  let messages =
    [
      make_user "hello";
      make_assistant_with_calls [ make_tool_call "call_1" "test_function" ];
      make_tool_result "call_1" "result";
      make_assistant_with_calls
        [
          make_tool_call "call_2" "another_tool";
          make_tool_call "call_3" "third";
        ];
      make_tool_result "call_2" "r2";
      make_tool_result "call_3" "r3";
    ]
  in
  let coq_input = Agent_loop_conformance.provider_to_coq_history messages in
  let coq_output = Clawq_core.AgentLoop.ensure_tool_group_integrity coq_input in
  let result =
    Agent_loop_conformance.coq_to_provider_history_with_names
      ~original_messages:messages coq_output
  in
  let tool_results = List.filter (fun m -> m.Provider.role = "tool") result in
  let get_name m = m.Provider.name in
  let names = List.filter_map get_name tool_results in
  Alcotest.(check int) "3 tool results" 3 (List.length tool_results);
  Alcotest.(check int) "3 tool names preserved" 3 (List.length names);
  Alcotest.(check bool)
    "name 'test_function' preserved" true
    (List.mem "test_function" names);
  Alcotest.(check bool)
    "name 'another_tool' preserved" true
    (List.mem "another_tool" names);
  Alcotest.(check bool) "name 'third' preserved" true (List.mem "third" names)

(* Test: tool names preserved through force_compress_history *)

let test_tool_name_preservation_compress () =
  let messages =
    [
      make_user "1";
      make_assistant_with_calls [ make_tool_call "c1" "tool_one" ];
      make_tool_result "c1" "r1";
      make_user "2";
      make_assistant_with_calls [ make_tool_call "c2" "tool_two" ];
      make_tool_result "c2" "r2";
      make_user "3";
    ]
  in
  let coq, _native, _equal =
    Agent_loop_conformance.conformance_force_compress_history 4 messages
  in
  let tool_results = List.filter (fun m -> m.Provider.role = "tool") coq in
  let names = List.filter_map (fun m -> m.Provider.name) tool_results in
  Alcotest.(check int) "1 tool result preserved" 1 (List.length tool_results);
  Alcotest.(check int) "1 tool name preserved" 1 (List.length names);
  Alcotest.(check bool)
    "name 'tool_one' preserved" true
    (List.mem "tool_one" names)

let make_openai_test_config () =
  let provider =
    {
      Runtime_config.default_provider_config with
      api_key = "test-key";
      kind = Some "openai";
      default_model = Some "gpt-test";
    }
  in
  {
    Runtime_config.default with
    providers = [ ("openai", provider) ];
    agent_defaults =
      {
        Runtime_config.default.agent_defaults with
        primary_model = "openai:gpt-test";
      };
    memory = { Runtime_config.default.memory with search_enabled = true };
  }

let with_captured_provider f =
  let captured = ref [] in
  let prev_complete = !Provider.native_complete in
  Fun.protect
    (fun () ->
      Provider.register_native_complete Provider.OpenAICompat
        (fun
          ~config:_ ~provider:_ ~model ~messages ?tools:_ ?session_key:_ () ->
          captured := messages;
          Lwt.return
            (Provider.Text
               {
                 content = "ok";
                 model;
                 usage = None;
                 provider_response_items_json = None;
                 thinking = None;
               }));
      f captured)
    ~finally:(fun () -> Provider.native_complete := prev_complete)

let with_counted_provider f =
  let call_count = ref 0 in
  let prev_complete = !Provider.native_complete in
  Fun.protect
    (fun () ->
      Provider.register_native_complete Provider.OpenAICompat
        (fun
          ~config:_ ~provider:_ ~model ~messages:_ ?tools:_ ?session_key:_ () ->
          incr call_count;
          Lwt.return
            (Provider.Text
               {
                 content = "ok";
                 model;
                 usage = Some (10, 5, 0);
                 provider_response_items_json = None;
                 thinking = None;
               }));
      f call_count)
    ~finally:(fun () -> Provider.native_complete := prev_complete)

let record_profile_usage ~db ~profile_id ~session_key ~prompt_tokens
    ~completion_tokens ~cost_usd ~requested_at =
  Request_stats.record ~db ~session_key ~profile_id ~provider:"openai"
    ~model:"gpt-test" ~prompt_tokens ~completion_tokens ~cost_usd ();
  let stmt =
    Sqlite3.prepare db
      "UPDATE request_stats SET requested_at = ? WHERE session_key = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT requested_at));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT session_key));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          Alcotest.failf "failed to update request_stats timestamp: %s"
            (Sqlite3.Rc.to_string rc))

let run_turn_result agent ~db ~session_key =
  Lwt_main.run
    (Lwt.catch
       (fun () ->
         let open Lwt.Syntax in
         let* content =
           Agent.turn agent ~user_message:"hello" ~db ~session_key ()
         in
         Lwt.return (`Returned content))
       (fun exn -> Lwt.return (`Failed (Printexc.to_string exn))))

let all_message_content messages =
  messages
  |> List.map (fun (m : Provider.message) -> m.content)
  |> String.concat "\n"

let test_profiled_room_turn_skips_unscoped_memory_context () =
  with_captured_provider (fun captured ->
      let db = Memory.init ~db_path:":memory:" ~search_enabled:true () in
      let profile_id =
        Memory.insert_room_profile ~db ~name:"channel-a-profile"
      in
      Memory.upsert_room_profile_binding ~db ~room_id:"C_A" ~profile_id;
      Memory.store_message ~db ~session_key:"slack:C_A"
        (Provider.make_message ~role:"user" ~content:"channel A local context");
      Memory.store_message ~db ~session_key:"slack:C_B"
        (Provider.make_message ~role:"user"
           ~content:"CHANNEL-B-SECRET leak-token only belongs to channel B");
      Memory.store_core ~db ~key:"global-core-secret"
        ~content:"GLOBAL-CORE-SECRET must not be injected" ();
      let agent = Agent.create ~config:(make_openai_test_config ()) () in
      ignore
        (Lwt_main.run
           (Agent.turn agent ~user_message:"leak-token" ~db
              ~session_key:"slack:C_A" ()));
      let prompt = all_message_content !captured in
      Alcotest.(check bool)
        "cross-channel search result absent" false
        (Test_helpers.string_contains prompt "CHANNEL-B-SECRET");
      Alcotest.(check bool)
        "global core memory absent" false
        (Test_helpers.string_contains prompt "GLOBAL-CORE-SECRET"))

let test_profiled_room_budget_blocks_provider_call () =
  with_counted_provider (fun call_count ->
      let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
      let profile_id = Memory.insert_room_profile ~db ~name:"budget-profile" in
      Memory.upsert_room_profile_binding ~db ~room_id:"C_BUDGET" ~profile_id;
      Room_budget.init_profile_budget ~db ~profile_id ~token_limit:100
        ~cost_limit_usd:10.0 ~reset_period:"daily"
        ~period_started_at:"2026-01-01 00:00:00" ();
      record_profile_usage ~db ~profile_id ~session_key:"previous"
        ~prompt_tokens:90 ~completion_tokens:30 ~cost_usd:1.0
        ~requested_at:"2026-01-01 01:00:00";
      let agent = Agent.create ~config:(make_openai_test_config ()) () in
      match run_turn_result agent ~db ~session_key:"slack:C_BUDGET" with
      | `Returned content ->
          Alcotest.failf "expected budget block, got %S" content
      | `Failed msg ->
          Alcotest.(check int) "provider not called" 0 !call_count;
          Alcotest.(check bool)
            "mentions budget exceeded" true
            (Test_helpers.string_contains msg "budget exceeded");
          Alcotest.(check bool)
            "includes current usage" true
            (Test_helpers.string_contains msg "current usage");
          Alcotest.(check bool)
            "includes limits" true
            (Test_helpers.string_contains msg "limits");
          Alcotest.(check bool)
            "includes usage value" true
            (Test_helpers.string_contains msg "120");
          Alcotest.(check bool)
            "includes limit value" true
            (Test_helpers.string_contains msg "100"))

let test_unprofiled_room_budget_guard_is_skipped () =
  with_counted_provider (fun call_count ->
      let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
      let profile_id = Memory.insert_room_profile ~db ~name:"budget-profile" in
      Room_budget.init_profile_budget ~db ~profile_id ~token_limit:100
        ~cost_limit_usd:10.0 ~reset_period:"daily"
        ~period_started_at:"2026-01-01 00:00:00" ();
      record_profile_usage ~db ~profile_id ~session_key:"previous"
        ~prompt_tokens:90 ~completion_tokens:30 ~cost_usd:1.0
        ~requested_at:"2026-01-01 01:00:00";
      let agent = Agent.create ~config:(make_openai_test_config ()) () in
      match run_turn_result agent ~db ~session_key:"slack:C_UNPROFILED" with
      | `Returned content ->
          Alcotest.(check string) "provider response" "ok" content;
          Alcotest.(check int) "provider called" 1 !call_count
      | `Failed msg -> Alcotest.failf "unprofiled turn should not fail: %s" msg)

let test_profiled_room_budget_allows_after_reset () =
  with_counted_provider (fun call_count ->
      let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
      let profile_id = Memory.insert_room_profile ~db ~name:"budget-profile" in
      Memory.upsert_room_profile_binding ~db ~room_id:"C_BUDGET" ~profile_id;
      Room_budget.init_profile_budget ~db ~profile_id ~token_limit:100
        ~cost_limit_usd:10.0 ~reset_period:"daily"
        ~period_started_at:"2026-01-01 00:00:00" ();
      record_profile_usage ~db ~profile_id ~session_key:"previous"
        ~prompt_tokens:90 ~completion_tokens:30 ~cost_usd:1.0
        ~requested_at:"2026-01-01 01:00:00";
      let agent = Agent.create ~config:(make_openai_test_config ()) () in
      (match run_turn_result agent ~db ~session_key:"slack:C_BUDGET" with
      | `Returned content ->
          Alcotest.failf "expected budget block, got %S" content
      | `Failed msg ->
          Alcotest.(check bool)
            "first call budget blocked" true
            (Test_helpers.string_contains msg "budget exceeded"));
      Alcotest.(check int) "provider not called before reset" 0 !call_count;
      Alcotest.(check bool)
        "reset succeeds" true
        (Room_budget.reset_profile_budget ~db ~profile_id
           ~period_started_at:"2026-01-02 00:00:00" ());
      match run_turn_result agent ~db ~session_key:"slack:C_BUDGET" with
      | `Returned content ->
          Alcotest.(check string) "provider response after reset" "ok" content;
          Alcotest.(check int) "provider called after reset" 1 !call_count
      | `Failed msg -> Alcotest.failf "turn after reset should not fail: %s" msg)

(* Test suite *)

let suite =
  [
    Alcotest.test_case "profiled room skips unscoped memory context" `Quick
      test_profiled_room_turn_skips_unscoped_memory_context;
    Alcotest.test_case "profiled room budget blocks provider call" `Quick
      test_profiled_room_budget_blocks_provider_call;
    Alcotest.test_case "unprofiled room skips budget guard" `Quick
      test_unprofiled_room_budget_guard_is_skipped;
    Alcotest.test_case "profiled room budget allows after reset" `Quick
      test_profiled_room_budget_allows_after_reset;
    Alcotest.test_case "collect_tool_call_ids basic" `Quick
      test_collect_tool_call_ids_basic;
    Alcotest.test_case "collect_tool_call_ids empty" `Quick
      test_collect_tool_call_ids_empty;
    Alcotest.test_case "collect_tool_result_ids basic" `Quick
      test_collect_tool_result_ids_basic;
    Alcotest.test_case "orphan tool result" `Quick
      test_ensure_tool_group_integrity_orphan_result;
    Alcotest.test_case "dangling tool call" `Quick
      test_ensure_tool_group_integrity_dangling_call;
    Alcotest.test_case "trim_history basic" `Quick test_trim_history_basic;
    Alcotest.test_case "force_compress_history basic" `Quick
      test_force_compress_history_basic;
    Alcotest.test_case "complex orphan scenario" `Quick
      test_complex_orphan_scenario;
    Alcotest.test_case "tool name preservation" `Quick
      test_tool_name_preservation;
    Alcotest.test_case "tool name preservation compress" `Quick
      test_tool_name_preservation_compress;
  ]

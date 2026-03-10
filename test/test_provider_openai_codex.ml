let test_strip_provider_prefix_slash () =
  Alcotest.(check string)
    "slash separator" "glm-5"
    (Provider_openai_codex.strip_provider_prefix "zai_coding/glm-5")

let test_strip_provider_prefix_colon () =
  Alcotest.(check string)
    "colon separator" "glm-5"
    (Provider_openai_codex.strip_provider_prefix "zai_coding:glm-5")

let test_strip_provider_prefix_no_sep () =
  Alcotest.(check string)
    "no separator" "codex-mini-latest"
    (Provider_openai_codex.strip_provider_prefix "codex-mini-latest")

let test_validate_codex_input_items_orphaned_output () =
  (* function_call_output with no matching function_call is dropped *)
  let items =
    [
      `Assoc
        [
          ("type", `String "function_call_output");
          ("call_id", `String "orphan-id");
          ("output", `String "result");
        ];
      `Assoc
        [
          ("role", `String "user");
          ( "content",
            `List
              [
                `Assoc
                  [ ("type", `String "input_text"); ("text", `String "hello") ];
              ] );
        ];
    ]
  in
  let result = Provider_openai_codex.validate_codex_input_items items in
  Alcotest.(check int) "orphaned output dropped" 1 (List.length result);
  (* the user message should survive *)
  match result with
  | [ `Assoc fields ] ->
      Alcotest.(check bool)
        "survivor is user msg" true
        (match List.assoc_opt "role" fields with
        | Some (`String "user") -> true
        | _ -> false)
  | _ -> Alcotest.fail "unexpected result shape"

let test_validate_codex_input_items_orphaned_call () =
  (* function_call with no matching function_call_output is dropped *)
  let items =
    [
      `Assoc
        [
          ("type", `String "function_call");
          ("call_id", `String "orphan-call");
          ("name", `String "shell_exec");
          ("arguments", `String "{}");
        ];
    ]
  in
  let result = Provider_openai_codex.validate_codex_input_items items in
  Alcotest.(check int) "orphaned call dropped" 0 (List.length result)

let test_validate_codex_input_items_valid_pair () =
  (* matched function_call + function_call_output pass through unchanged *)
  let items =
    [
      `Assoc
        [
          ("type", `String "function_call");
          ("call_id", `String "call-1");
          ("name", `String "shell_exec");
          ("arguments", `String "{}");
        ];
      `Assoc
        [
          ("type", `String "function_call_output");
          ("call_id", `String "call-1");
          ("output", `String "ok");
        ];
      `Assoc
        [
          ("role", `String "user");
          ( "content",
            `List
              [
                `Assoc
                  [
                    ("type", `String "input_text"); ("text", `String "continue");
                  ];
              ] );
        ];
    ]
  in
  let result = Provider_openai_codex.validate_codex_input_items items in
  Alcotest.(check int) "all three items kept" 3 (List.length result)

let test_force_compress_history_fallback_nonempty () =
  (* When force_compress_history is called and the entire history consists of
     orphaned tool results (no matching assistant tool_calls message anywhere),
     ensure_tool_group_integrity strips them all.  The fallback must keep the
     raw compressed slice so history stays non-empty and the next API call does
     not receive an empty input array.
     expand_keep_for_tool_groups cannot help here because even after expanding
     the kept set to include everything, there is still no matching call. *)
  let config = Runtime_config.default in
  let agent = Agent.create ~config () in
  let tool_result i =
    Provider.make_tool_result ~tool_call_id:(Printf.sprintf "tc%d" i)
      ~name:"shell_exec"
      ~content:(Printf.sprintf "result-%d" i)
  in
  (* 7 orphaned tool results — no assistant message anywhere.
     Must exceed context_recovery_min_history (6) to trigger compression. *)
  agent.Agent.history <-
    [
      tool_result 7;
      tool_result 6;
      tool_result 5;
      tool_result 4;
      tool_result 3;
      tool_result 2;
      tool_result 1;
    ];
  let did_compress = Agent.force_compress_history agent in
  Alcotest.(check bool) "compression ran" true did_compress;
  Alcotest.(check bool)
    "history non-empty after fallback" true
    (agent.Agent.history <> [])

let suite =
  [
    Alcotest.test_case "strip_provider_prefix slash" `Quick
      test_strip_provider_prefix_slash;
    Alcotest.test_case "strip_provider_prefix colon" `Quick
      test_strip_provider_prefix_colon;
    Alcotest.test_case "strip_provider_prefix no separator" `Quick
      test_strip_provider_prefix_no_sep;
    Alcotest.test_case "validate_codex_input_items orphaned output" `Quick
      test_validate_codex_input_items_orphaned_output;
    Alcotest.test_case "validate_codex_input_items orphaned call" `Quick
      test_validate_codex_input_items_orphaned_call;
    Alcotest.test_case "validate_codex_input_items valid pair" `Quick
      test_validate_codex_input_items_valid_pair;
    Alcotest.test_case "force_compress_history fallback nonempty" `Quick
      test_force_compress_history_fallback_nonempty;
  ]

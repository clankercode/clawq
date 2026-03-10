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

let test_sanitize_function_call_strips_metadata () =
  let item =
    `Assoc
      [
        ("id", `String "fc_abc123");
        ("type", `String "function_call");
        ("status", `String "completed");
        ("arguments", `String "{\"cmd\":\"test\"}");
        ("call_id", `String "call_xyz");
        ("name", `String "shell_exec");
      ]
  in
  let result =
    match Provider_openai_codex.sanitize_input_item item with
    | Some r -> r
    | None -> Alcotest.fail "expected Some but got None"
  in
  let keys =
    match result with
    | `Assoc fields -> List.map fst fields |> List.sort String.compare
    | _ -> Alcotest.fail "expected Assoc"
  in
  Alcotest.(check (list string))
    "only input-valid fields"
    [ "arguments"; "call_id"; "name"; "type" ]
    keys

let test_sanitize_message_strips_metadata () =
  let item =
    `Assoc
      [
        ("id", `String "msg_abc123");
        ("type", `String "message");
        ("status", `String "completed");
        ( "content",
          `List
            [
              `Assoc
                [
                  ("type", `String "output_text");
                  ("annotations", `List []);
                  ("logprobs", `List []);
                  ("text", `String "hello");
                ];
            ] );
        ("phase", `String "final_answer");
        ("role", `String "assistant");
      ]
  in
  let result =
    match Provider_openai_codex.sanitize_input_item item with
    | Some r -> r
    | None -> Alcotest.fail "expected Some but got None"
  in
  match result with
  | `Assoc fields -> (
      let keys = List.map fst fields |> List.sort String.compare in
      Alcotest.(check (list string))
        "message becomes role+content" [ "content"; "role" ] keys;
      (* Check content parts are also sanitized *)
      let content =
        match List.assoc "content" fields with `List l -> l | _ -> []
      in
      match content with
      | [ `Assoc part_fields ] ->
          let part_keys =
            List.map fst part_fields |> List.sort String.compare
          in
          Alcotest.(check (list string))
            "content part only type+text" [ "text"; "type" ] part_keys
      | _ -> Alcotest.fail "expected single content part")
  | _ -> Alcotest.fail "expected Assoc"

let test_sanitize_reasoning_item_dropped () =
  (* reasoning items are output-only and require store=true to reference by ID;
     they must be dropped (None) rather than forwarded as input *)
  let item =
    `Assoc
      [
        ("id", `String "rs_0b6b337facf77de50169af8164c700819185245d5361c014c3");
        ("type", `String "reasoning");
        ("summary", `List []);
      ]
  in
  Alcotest.(check bool)
    "reasoning item dropped (None)" true
    (Provider_openai_codex.sanitize_input_item item = None)

let test_sanitize_unknown_type_strips_id () =
  (* unknown output item types should have their server-assigned id stripped
     (to avoid HTTP 404 when store=false) but otherwise be kept *)
  let item =
    `Assoc
      [
        ("id", `String "unknown_server_id");
        ("type", `String "some_future_type");
        ("data", `String "payload");
      ]
  in
  let result =
    match Provider_openai_codex.sanitize_input_item item with
    | Some r -> r
    | None -> Alcotest.fail "expected Some but got None"
  in
  let fields =
    match result with `Assoc fs -> fs | _ -> Alcotest.fail "expected Assoc"
  in
  Alcotest.(check bool)
    "id field stripped" true
    (not (List.mem_assoc "id" fields));
  Alcotest.(check bool) "type field kept" true (List.mem_assoc "type" fields)

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
    Alcotest.test_case "sanitize function_call strips metadata" `Quick
      test_sanitize_function_call_strips_metadata;
    Alcotest.test_case "sanitize message strips metadata" `Quick
      test_sanitize_message_strips_metadata;
    Alcotest.test_case "sanitize reasoning item dropped" `Quick
      test_sanitize_reasoning_item_dropped;
    Alcotest.test_case "sanitize unknown type strips id" `Quick
      test_sanitize_unknown_type_strips_id;
    Alcotest.test_case "force_compress_history fallback nonempty" `Quick
      test_force_compress_history_fallback_nonempty;
  ]

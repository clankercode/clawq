(* test_setup_memory.ml — Unit tests for Setup_memory pure functions *)

let validate_weight_valid () =
  Alcotest.(check (result string string))
    "valid weight 50" (Ok "50")
    (Setup_memory.validate_weight "50")

let validate_weight_zero () =
  Alcotest.(check (result string string))
    "zero is valid" (Ok "0")
    (Setup_memory.validate_weight "0")

let validate_weight_100 () =
  Alcotest.(check (result string string))
    "100 is valid" (Ok "100")
    (Setup_memory.validate_weight "100")

let validate_weight_negative () =
  match Setup_memory.validate_weight "-1" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative weight"

let validate_weight_over_100 () =
  match Setup_memory.validate_weight "101" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for weight > 100"

let validate_weight_non_int () =
  match Setup_memory.validate_weight "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-integer"

let validate_compaction_threshold_valid () =
  Alcotest.(check (result string string))
    "valid threshold 75" (Ok "75")
    (Setup_memory.validate_compaction_threshold "75")

let validate_compaction_threshold_zero () =
  match Setup_memory.validate_compaction_threshold "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero threshold"

let validate_compaction_threshold_over_100 () =
  match Setup_memory.validate_compaction_threshold "101" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for threshold > 100"

let validate_positive_int_valid () =
  Alcotest.(check (result string string))
    "valid positive int 500" (Ok "500")
    (Setup_memory.validate_positive_int "500")

let validate_positive_int_zero () =
  match Setup_memory.validate_positive_int "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero"

let validate_positive_int_negative () =
  match Setup_memory.validate_positive_int "-5" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative"

let build_json_roundtrip () =
  let json =
    Setup_memory.build_memory_json ~backend:"sqlite" ~search_enabled:false
      ~vector_weight:50 ~keyword_weight:50 ~embedding_model:""
      ~embedding_provider:"" ~compaction_threshold_percent:75
      ~max_messages_per_session:500 ~max_message_age_days:30
      ~pre_compaction_flush:true ~task_tree_purge_after_days:(-1)
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let m = config.memory in
  Alcotest.(check string) "backend" "sqlite" m.backend;
  Alcotest.(check bool) "search_enabled" false m.search_enabled;
  Alcotest.(check int) "vector_weight" 50 m.vector_weight;
  Alcotest.(check int) "keyword_weight" 50 m.keyword_weight;
  Alcotest.(check int)
    "compaction_threshold_percent" 75 m.compaction_threshold_percent;
  Alcotest.(check int) "max_messages_per_session" 500 m.max_messages_per_session;
  Alcotest.(check int) "max_message_age_days" 30 m.max_message_age_days;
  Alcotest.(check bool) "pre_compaction_flush" true m.pre_compaction_flush;
  Alcotest.(check int)
    "task_tree_purge_after_days" (-1) m.task_tree_purge_after_days

let build_json_with_embedding () =
  let json =
    Setup_memory.build_memory_json ~backend:"sqlite" ~search_enabled:true
      ~vector_weight:60 ~keyword_weight:40
      ~embedding_model:"text-embedding-3-small" ~embedding_provider:"openai"
      ~compaction_threshold_percent:80 ~max_messages_per_session:1000
      ~max_message_age_days:60 ~pre_compaction_flush:false
      ~task_tree_purge_after_days:7
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let m = config.memory in
  Alcotest.(check bool) "search_enabled" true m.search_enabled;
  Alcotest.(check int) "vector_weight" 60 m.vector_weight;
  Alcotest.(check int) "keyword_weight" 40 m.keyword_weight;
  Alcotest.(check int)
    "task_tree_purge_after_days" 7 m.task_tree_purge_after_days

let build_json_null_optional_fields () =
  (* Empty strings for optional fields should produce null in JSON, which
     config_loader resolves to None *)
  let json =
    Setup_memory.build_memory_json ~backend:"sqlite" ~search_enabled:false
      ~vector_weight:50 ~keyword_weight:50 ~embedding_model:""
      ~embedding_provider:"" ~compaction_threshold_percent:75
      ~max_messages_per_session:500 ~max_message_age_days:30
      ~pre_compaction_flush:true ~task_tree_purge_after_days:(-1)
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let m = config.memory in
  Alcotest.(check (option string)) "embedding_model none" None m.embedding_model;
  Alcotest.(check (option string))
    "embedding_provider none" None m.embedding_provider

let post_instructions_content () =
  let s = Setup_memory.post_setup_instructions in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has docs url" true
    (contains "https://clawq.org/memory/");
  Alcotest.(check bool) "mentions backend" true (contains "backend");
  Alcotest.(check bool) "mentions compaction" true (contains "compaction")

let suite =
  [
    Alcotest.test_case "validate_weight valid" `Quick validate_weight_valid;
    Alcotest.test_case "validate_weight zero" `Quick validate_weight_zero;
    Alcotest.test_case "validate_weight 100" `Quick validate_weight_100;
    Alcotest.test_case "validate_weight negative" `Quick
      validate_weight_negative;
    Alcotest.test_case "validate_weight over 100" `Quick
      validate_weight_over_100;
    Alcotest.test_case "validate_weight non-int" `Quick validate_weight_non_int;
    Alcotest.test_case "validate_compaction_threshold valid" `Quick
      validate_compaction_threshold_valid;
    Alcotest.test_case "validate_compaction_threshold zero" `Quick
      validate_compaction_threshold_zero;
    Alcotest.test_case "validate_compaction_threshold over 100" `Quick
      validate_compaction_threshold_over_100;
    Alcotest.test_case "validate_positive_int valid" `Quick
      validate_positive_int_valid;
    Alcotest.test_case "validate_positive_int zero" `Quick
      validate_positive_int_zero;
    Alcotest.test_case "validate_positive_int negative" `Quick
      validate_positive_int_negative;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json with embedding" `Quick
      build_json_with_embedding;
    Alcotest.test_case "build_json null optional fields" `Quick
      build_json_null_optional_fields;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]

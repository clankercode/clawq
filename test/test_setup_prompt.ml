(* test_setup_prompt.ml — Unit tests for Setup_prompt pure functions *)

let validate_positive_int_valid () =
  Alcotest.(check (result string string))
    "valid int 8000" (Ok "8000")
    (Setup_common.validate_positive_int "8000")

let validate_positive_int_zero () =
  match Setup_common.validate_positive_int "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for zero"

let validate_positive_int_negative () =
  match Setup_common.validate_positive_int "-1" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative"

let validate_positive_int_non_int () =
  match Setup_common.validate_positive_int "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-integer"

let build_json_roundtrip () =
  let json =
    Setup_prompt.build_prompt_json ~dynamic_enabled:true
      ~include_tools_section:true ~include_safety_section:true
      ~include_workspace_section:true ~include_runtime_section:true
      ~include_datetime_section:true ~include_autonomy_section:true
      ~include_project_docs:true
      ~workspace_files:[ "CLAUDE.md"; "README.md"; ".clawq/config.json" ]
      ~max_workspace_file_chars:8000 ~max_workspace_total_chars:20000
      ~max_project_doc_chars:51200 ~project_doc_warn_chars:15360
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let p = config.prompt in
  Alcotest.(check bool) "dynamic_enabled" true p.dynamic_enabled;
  Alcotest.(check bool) "include_tools_section" true p.include_tools_section;
  Alcotest.(check bool) "include_safety_section" true p.include_safety_section;
  Alcotest.(check bool)
    "include_workspace_section" true p.include_workspace_section;
  Alcotest.(check bool) "include_runtime_section" true p.include_runtime_section;
  Alcotest.(check bool)
    "include_datetime_section" true p.include_datetime_section;
  Alcotest.(check bool)
    "include_autonomy_section" true p.include_autonomy_section;
  Alcotest.(check (list string))
    "workspace_files"
    [ "CLAUDE.md"; "README.md"; ".clawq/config.json" ]
    p.workspace_files;
  Alcotest.(check int)
    "max_workspace_file_chars" 8000 p.max_workspace_file_chars;
  Alcotest.(check int)
    "max_workspace_total_chars" 20000 p.max_workspace_total_chars

let build_json_disabled_sections () =
  let json =
    Setup_prompt.build_prompt_json ~dynamic_enabled:false
      ~include_tools_section:false ~include_safety_section:false
      ~include_workspace_section:false ~include_runtime_section:false
      ~include_datetime_section:false ~include_autonomy_section:false
      ~include_project_docs:false ~workspace_files:[]
      ~max_workspace_file_chars:4000 ~max_workspace_total_chars:10000
      ~max_project_doc_chars:51200 ~project_doc_warn_chars:15360
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  let p = config.prompt in
  Alcotest.(check bool) "dynamic_enabled false" false p.dynamic_enabled;
  Alcotest.(check bool) "tools false" false p.include_tools_section;
  Alcotest.(check bool) "safety false" false p.include_safety_section;
  Alcotest.(check int)
    "max_workspace_file_chars" 4000 p.max_workspace_file_chars;
  Alcotest.(check int)
    "max_workspace_total_chars" 10000 p.max_workspace_total_chars

let post_instructions_content () =
  let s = Setup_prompt.post_setup_instructions in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has docs url" true
    (contains "https://clawq.org/prompt/");
  Alcotest.(check bool)
    "mentions dynamic_enabled" true
    (contains "dynamic_enabled");
  Alcotest.(check bool)
    "mentions workspace_files" true
    (contains "workspace_files")

let suite =
  [
    Alcotest.test_case "validate_positive_int valid" `Quick
      validate_positive_int_valid;
    Alcotest.test_case "validate_positive_int zero" `Quick
      validate_positive_int_zero;
    Alcotest.test_case "validate_positive_int negative" `Quick
      validate_positive_int_negative;
    Alcotest.test_case "validate_positive_int non-int" `Quick
      validate_positive_int_non_int;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json disabled sections" `Quick
      build_json_disabled_sections;
    Alcotest.test_case "post_instructions content" `Quick
      post_instructions_content;
  ]

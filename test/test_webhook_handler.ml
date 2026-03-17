(* Tests for the generic webhook handler infrastructure. *)

(* ---- JSON path utilities ---- *)

let lookup_basic () =
  let json =
    Yojson.Safe.from_string
      {|{"repository":{"full_name":"acme/backend","owner":{"login":"acme"}}}|}
  in
  Alcotest.(check (option string))
    "nested string" (Some "acme/backend")
    (Option.map Webhook_handler.string_of_json
       (Webhook_handler.lookup_json_path json "repository.full_name"));
  Alcotest.(check (option string))
    "deep nested" (Some "acme")
    (Option.map Webhook_handler.string_of_json
       (Webhook_handler.lookup_json_path json "repository.owner.login"))

let lookup_missing () =
  let json = Yojson.Safe.from_string {|{"a":{"b":1}}|} in
  Alcotest.(check bool)
    "missing path" true
    (Webhook_handler.lookup_json_path json "a.c" = None)

let lookup_array_index () =
  let json = Yojson.Safe.from_string {|{"items":[10,20,30]}|} in
  Alcotest.(check (option string))
    "array index 1" (Some "20")
    (Option.map Webhook_handler.string_of_json
       (Webhook_handler.lookup_json_path json "items.1"))

let string_of_json_types () =
  Alcotest.(check string) "null" "" (Webhook_handler.string_of_json `Null);
  Alcotest.(check string)
    "string" "hello"
    (Webhook_handler.string_of_json (`String "hello"));
  Alcotest.(check string)
    "bool" "true"
    (Webhook_handler.string_of_json (`Bool true));
  Alcotest.(check string) "int" "42" (Webhook_handler.string_of_json (`Int 42))

let first_string_picks_first () =
  let json =
    Yojson.Safe.from_string {|{"a":{"x":""},"b":{"y":"found"},"c":"also"}|}
  in
  Alcotest.(check (option string))
    "first non-empty" (Some "found")
    (Webhook_handler.first_string json [ "a.x"; "b.y"; "c" ])

let first_int_coerces () =
  let json = Yojson.Safe.from_string {|{"a":"42","b":7}|} in
  Alcotest.(check (option int))
    "string coerced" (Some 42)
    (Webhook_handler.first_int json [ "a" ]);
  Alcotest.(check (option int))
    "int direct" (Some 7)
    (Webhook_handler.first_int json [ "b" ])

(* ---- Match rule evaluation ---- *)

let value_matches_basic () =
  Alcotest.(check bool)
    "exact match" true
    (Webhook_handler.value_matches (`String "failure") "failure");
  Alcotest.(check bool)
    "case insensitive" true
    (Webhook_handler.value_matches (`String "Failure") "failure");
  Alcotest.(check bool)
    "no match" false
    (Webhook_handler.value_matches (`String "success") "failure")

let value_matches_exists () =
  Alcotest.(check bool)
    "exists on string" true
    (Webhook_handler.value_matches (`String "x") "exists");
  Alcotest.(check bool)
    "exists on null" false
    (Webhook_handler.value_matches `Null "exists");
  Alcotest.(check bool)
    "exists on int" true
    (Webhook_handler.value_matches (`Int 1) "exists")

let value_matches_list () =
  let json = `List [ `String "a"; `String "b"; `String "c" ] in
  Alcotest.(check bool)
    "matches item in list" true
    (Webhook_handler.value_matches json "b");
  Alcotest.(check bool)
    "no match in list" false
    (Webhook_handler.value_matches json "d")

let rules_match_all () =
  let rules =
    Webhook_handler.
      [
        { path = "status"; expected = "completed" };
        { path = "conclusion"; expected = "failure" };
      ]
  in
  let context =
    Yojson.Safe.from_string {|{"status":"completed","conclusion":"failure"}|}
  in
  Alcotest.(check bool)
    "all rules match" true
    (Webhook_handler.rules_match rules context)

let rules_match_partial_fail () =
  let rules =
    Webhook_handler.
      [
        { path = "status"; expected = "completed" };
        { path = "conclusion"; expected = "failure" };
      ]
  in
  let context =
    Yojson.Safe.from_string {|{"status":"completed","conclusion":"success"}|}
  in
  Alcotest.(check bool)
    "partial mismatch" false
    (Webhook_handler.rules_match rules context)

let rules_match_empty () =
  let context = Yojson.Safe.from_string {|{"x":1}|} in
  Alcotest.(check bool)
    "empty rules always match" true
    (Webhook_handler.rules_match [] context)

(* ---- Template rendering ---- *)

let render_simple_substitution () =
  let context =
    Yojson.Safe.from_string {|{"repo":"acme/backend","branch":"main"}|}
  in
  let result =
    Webhook_handler.render_template ~template:"Deploy {{repo}} on {{branch}}."
      ~context_json:context
  in
  Alcotest.(check string) "simple render" "Deploy acme/backend on main." result

let render_json_directive () =
  let context = Yojson.Safe.from_string {|{"data":{"key":"value"}}|} in
  let result =
    Webhook_handler.render_template ~template:"Data: {{json data}}"
      ~context_json:context
  in
  Alcotest.(check bool)
    "json directive contains key" true
    (try
       ignore (Str.search_forward (Str.regexp_string "\"key\"") result 0);
       true
     with Not_found -> false)

let render_missing_var () =
  let context = Yojson.Safe.from_string {|{"a":"1"}|} in
  let result =
    Webhook_handler.render_template ~template:"x={{missing}}"
      ~context_json:context
  in
  Alcotest.(check string) "missing becomes empty" "x=" result

let render_include_placeholder () =
  let context = Yojson.Safe.from_string {|{}|} in
  let result =
    Webhook_handler.render_template ~template:"{{include foo.md}}"
      ~context_json:context
  in
  Alcotest.(check string)
    "include not implemented" "[include not implemented yet]" result

(* ---- Truncate payload ---- *)

let truncate_short () =
  let s = "short" in
  Alcotest.(check string) "no truncation" s (Webhook_handler.truncate_payload s)

let truncate_long () =
  let s = String.make 20 'x' in
  let result = Webhook_handler.truncate_payload ~max_chars:10 s in
  Alcotest.(check bool)
    "truncated" true
    (String.length result < String.length s + 50);
  Alcotest.(check bool)
    "starts with original prefix" true
    (String.sub result 0 10 = String.make 10 'x');
  Alcotest.(check bool)
    "contains truncation marker" true
    (try
       ignore (Str.search_forward (Str.regexp_string "truncated") result 0);
       true
     with Not_found -> false)

(* ---- Frontmatter parsing ---- *)

let parse_full_frontmatter () =
  let lines =
    [
      "---";
      "name: my-hook";
      "event: push";
      "enabled: true";
      "repo: acme/backend";
      "custom_key: custom_value";
      "match:";
      "  status: completed";
      "  conclusion: failure";
      "---";
      "This is the body.";
    ]
  in
  let fm = Webhook_handler.parse_frontmatter lines in
  Alcotest.(check string) "name" "my-hook" fm.name;
  Alcotest.(check string) "event" "push" fm.event;
  Alcotest.(check bool) "enabled" true fm.enabled;
  Alcotest.(check int) "match rules" 2 (List.length fm.match_rules);
  let rule0 = List.nth fm.match_rules 0 in
  Alcotest.(check string) "rule0 path" "status" rule0.path;
  Alcotest.(check string) "rule0 expected" "completed" rule0.expected;
  Alcotest.(check bool)
    "repo in fields" true
    (List.assoc_opt "repo" fm.fields = Some "acme/backend");
  Alcotest.(check bool)
    "custom in fields" true
    (List.assoc_opt "custom_key" fm.fields = Some "custom_value");
  Alcotest.(check int) "body lines" 1 (List.length fm.body_lines);
  Alcotest.(check string)
    "body content" "This is the body." (List.hd fm.body_lines)

let parse_no_frontmatter () =
  let lines = [ "Just a body"; "with multiple lines" ] in
  let fm = Webhook_handler.parse_frontmatter lines in
  Alcotest.(check string) "name empty" "" fm.name;
  Alcotest.(check int) "body lines" 2 (List.length fm.body_lines)

let parse_disabled () =
  let lines = [ "---"; "name: off-hook"; "enabled: false"; "---"; "Body" ] in
  let fm = Webhook_handler.parse_frontmatter lines in
  Alcotest.(check bool) "disabled" false fm.enabled

(* ---- Sanitize filename ---- *)

let sanitize_basic () =
  Alcotest.(check string)
    "normal" "abc-123"
    (Webhook_handler.sanitize_filename_component "abc-123");
  Alcotest.(check string)
    "special chars" "a_b_c"
    (Webhook_handler.sanitize_filename_component "a/b c");
  Alcotest.(check string)
    "empty" "delivery"
    (Webhook_handler.sanitize_filename_component "")

(* ---- Parse bool ---- *)

let parse_bool_values () =
  Alcotest.(check (option bool))
    "true" (Some true)
    (Webhook_handler.parse_bool "true");
  Alcotest.(check (option bool))
    "yes" (Some true)
    (Webhook_handler.parse_bool "yes");
  Alcotest.(check (option bool))
    "false" (Some false)
    (Webhook_handler.parse_bool "false");
  Alcotest.(check (option bool))
    "no" (Some false)
    (Webhook_handler.parse_bool "no");
  Alcotest.(check (option bool))
    "garbage" None
    (Webhook_handler.parse_bool "maybe")

(* ---- Hook file loading ---- *)

let load_hook_files_empty () =
  let dir = Filename.temp_dir "wh_test" "" in
  let files = Webhook_handler.load_hook_files ~dir ~suffix:".md" in
  Alcotest.(check int) "empty dir" 0 (List.length files);
  try Sys.rmdir dir with _ -> ()

let load_hook_files_filters () =
  let dir = Filename.temp_dir "wh_test" "" in
  let write name content =
    let path = Filename.concat dir name in
    let oc = open_out path in
    output_string oc content;
    close_out oc
  in
  write "hook1.md" "content";
  write "hook2.md" "content";
  write "readme.txt" "content";
  let files = Webhook_handler.load_hook_files ~dir ~suffix:".md" in
  Alcotest.(check int) "only .md files" 2 (List.length files);
  Array.iter
    (fun name -> Sys.remove (Filename.concat dir name))
    (Sys.readdir dir);
  try Sys.rmdir dir with _ -> ()

(* ---- Suite ---- *)

let suite =
  [
    Alcotest.test_case "lookup basic" `Quick lookup_basic;
    Alcotest.test_case "lookup missing" `Quick lookup_missing;
    Alcotest.test_case "lookup array index" `Quick lookup_array_index;
    Alcotest.test_case "string_of_json types" `Quick string_of_json_types;
    Alcotest.test_case "first_string picks first" `Quick
      first_string_picks_first;
    Alcotest.test_case "first_int coerces" `Quick first_int_coerces;
    Alcotest.test_case "value_matches basic" `Quick value_matches_basic;
    Alcotest.test_case "value_matches exists" `Quick value_matches_exists;
    Alcotest.test_case "value_matches list" `Quick value_matches_list;
    Alcotest.test_case "rules_match all" `Quick rules_match_all;
    Alcotest.test_case "rules_match partial fail" `Quick
      rules_match_partial_fail;
    Alcotest.test_case "rules_match empty" `Quick rules_match_empty;
    Alcotest.test_case "render simple" `Quick render_simple_substitution;
    Alcotest.test_case "render json directive" `Quick render_json_directive;
    Alcotest.test_case "render missing var" `Quick render_missing_var;
    Alcotest.test_case "render include placeholder" `Quick
      render_include_placeholder;
    Alcotest.test_case "truncate short" `Quick truncate_short;
    Alcotest.test_case "truncate long" `Quick truncate_long;
    Alcotest.test_case "parse full frontmatter" `Quick parse_full_frontmatter;
    Alcotest.test_case "parse no frontmatter" `Quick parse_no_frontmatter;
    Alcotest.test_case "parse disabled" `Quick parse_disabled;
    Alcotest.test_case "sanitize filename" `Quick sanitize_basic;
    Alcotest.test_case "parse_bool values" `Quick parse_bool_values;
    Alcotest.test_case "load_hook_files empty dir" `Quick load_hook_files_empty;
    Alcotest.test_case "load_hook_files filters by suffix" `Quick
      load_hook_files_filters;
  ]

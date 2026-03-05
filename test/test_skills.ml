let test_substitute_template () =
  let result = Skills.substitute_template "cd {{path}} && git status"
      (`Assoc [ ("path", `String "/tmp/repo") ]) in
  Alcotest.(check string) "substituted" "cd /tmp/repo && git status" result

let test_substitute_multiple () =
  let result = Skills.substitute_template "{{a}} and {{b}}"
      (`Assoc [ ("a", `String "X"); ("b", `String "Y") ]) in
  Alcotest.(check string) "multiple subs" "X and Y" result

let test_substitute_no_match () =
  let result = Skills.substitute_template "no params here"
      (`Assoc []) in
  Alcotest.(check string) "no substitution" "no params here" result

let test_load_skill_from_file () =
  let tmp = Filename.temp_file "skill_test" ".json" in
  let json = {|{
    "name": "test_skill",
    "description": "A test skill",
    "parameters": {
      "type": "object",
      "properties": {
        "msg": {"type": "string", "description": "message"}
      }
    },
    "command": "echo {{msg}}",
    "risk_level": "low"
  }|} in
  let oc = open_out tmp in
  output_string oc json;
  close_out oc;
  (match Skills.load_skill tmp with
   | Some tool ->
     Alcotest.(check string) "tool name" "test_skill" tool.name;
     Alcotest.(check string) "tool description" "A test skill" tool.description;
     Alcotest.(check bool) "risk level low" true (tool.risk_level = Tool.Low)
   | None -> Alcotest.fail "failed to load skill");
  Sys.remove tmp

let test_load_skill_invalid () =
  let tmp = Filename.temp_file "skill_bad" ".json" in
  let oc = open_out tmp in
  output_string oc "not valid json {{{";
  close_out oc;
  let result = Skills.load_skill tmp in
  Alcotest.(check bool) "invalid skill returns None" true (result = None);
  Sys.remove tmp

let test_load_all_empty_dir () =
  let tmp_dir = Filename.get_temp_dir_name () ^ "/clawq_skills_test_" ^
                string_of_int (Random.int 100000) in
  (try Sys.mkdir tmp_dir 0o755 with _ -> ());
  let skills = Skills.load_all ~dir:tmp_dir () in
  Alcotest.(check int) "no skills in empty dir" 0 (List.length skills);
  (try Sys.rmdir tmp_dir with _ -> ())

let test_load_all_with_skill () =
  let tmp_dir = Filename.get_temp_dir_name () ^ "/clawq_skills_test_" ^
                string_of_int (Random.int 100000) in
  (try Sys.mkdir tmp_dir 0o755 with _ -> ());
  let skill_path = Filename.concat tmp_dir "echo.json" in
  let json = {|{
    "name": "echo_test",
    "description": "Echo a message",
    "parameters": {"type": "object", "properties": {}},
    "command": "echo hello",
    "risk_level": "low"
  }|} in
  let oc = open_out skill_path in
  output_string oc json; close_out oc;
  let skills = Skills.load_all ~dir:tmp_dir () in
  Alcotest.(check int) "one skill loaded" 1 (List.length skills);
  let s = List.hd skills in
  Alcotest.(check string) "skill name" "echo_test" s.name;
  Sys.remove skill_path;
  (try Sys.rmdir tmp_dir with _ -> ())

let test_list_skills_empty () =
  let tmp_dir = Filename.get_temp_dir_name () ^ "/clawq_skills_list_" ^
                string_of_int (Random.int 100000) in
  (try Sys.mkdir tmp_dir 0o755 with _ -> ());
  let files = Skills.list_skills ~dir:tmp_dir () in
  Alcotest.(check int) "no files" 0 (List.length files);
  (try Sys.rmdir tmp_dir with _ -> ())

let test_risk_level_parsing () =
  Alcotest.(check bool) "high" true (Skills.risk_level_of_string "high" = Tool.High);
  Alcotest.(check bool) "medium" true (Skills.risk_level_of_string "medium" = Tool.Medium);
  Alcotest.(check bool) "low" true (Skills.risk_level_of_string "low" = Tool.Low);
  Alcotest.(check bool) "unknown" true (Skills.risk_level_of_string "xyz" = Tool.Low)

let test_skill_invoke_workspace_policy_blocked () =
  let tmp = Filename.temp_file "skill_policy" ".json" in
  let json = {|{
    "name": "policy_test",
    "description": "Policy test",
    "parameters": {"type": "object", "properties": {}},
    "command": "echo ok && whoami",
    "risk_level": "low"
  }|} in
  let oc = open_out tmp in
  output_string oc json;
  close_out oc;
  (match Skills.load_skill ~workspace_only:true tmp with
   | None -> Alcotest.fail "failed to load skill"
   | Some tool ->
     let out = Lwt_main.run (tool.invoke (`Assoc [])) in
     Alcotest.(check bool) "blocked" true
       (try
          let re = Str.regexp_string "unsafe shell syntax" in
          ignore (Str.search_forward re out 0);
          true
        with Not_found -> false));
  Sys.remove tmp

let test_skill_invoke_workspace_path_blocked () =
  let tmp = Filename.temp_file "skill_path_policy" ".json" in
  let json = {|{
    "name": "path_policy_test",
    "description": "Path policy test",
    "parameters": {"type": "object", "properties": {}},
    "command": "cat /etc/passwd",
    "risk_level": "low"
  }|} in
  let oc = open_out tmp in
  output_string oc json;
  close_out oc;
  (match Skills.load_skill ~workspace_only:true tmp with
   | None -> Alcotest.fail "failed to load skill"
   | Some tool ->
     let out = Lwt_main.run (tool.invoke (`Assoc [])) in
     Alcotest.(check bool) "blocked" true
       (try
          let re = Str.regexp_string "disallowed in workspace_only mode" in
          ignore (Str.search_forward re out 0);
          true
        with Not_found -> false));
  Sys.remove tmp

let test_skill_invoke_workspace_binary_path_blocked () =
  let tmp = Filename.temp_file "skill_bin_policy" ".json" in
  let json = {|{
    "name": "bin_policy_test",
    "description": "Binary policy test",
    "parameters": {"type": "object", "properties": {}},
    "command": "./echo hi",
    "risk_level": "low"
  }|} in
  let oc = open_out tmp in
  output_string oc json;
  close_out oc;
  (match Skills.load_skill ~workspace_only:true tmp with
   | None -> Alcotest.fail "failed to load skill"
   | Some tool ->
     let out = Lwt_main.run (tool.invoke (`Assoc [])) in
     Alcotest.(check bool) "blocked" true
       (try
          let re = Str.regexp_string "binary path is disallowed" in
          ignore (Str.search_forward re out 0);
          true
        with Not_found -> false));
  Sys.remove tmp

let suite =
  [
    Alcotest.test_case "template substitution" `Quick test_substitute_template;
    Alcotest.test_case "template multiple" `Quick test_substitute_multiple;
    Alcotest.test_case "template no match" `Quick test_substitute_no_match;
    Alcotest.test_case "load skill from file" `Quick test_load_skill_from_file;
    Alcotest.test_case "load invalid skill" `Quick test_load_skill_invalid;
    Alcotest.test_case "load_all empty dir" `Quick test_load_all_empty_dir;
    Alcotest.test_case "load_all with skill" `Quick test_load_all_with_skill;
    Alcotest.test_case "list skills empty" `Quick test_list_skills_empty;
    Alcotest.test_case "risk level parsing" `Quick test_risk_level_parsing;
    Alcotest.test_case "skill invoke workspace policy blocked" `Quick
      test_skill_invoke_workspace_policy_blocked;
    Alcotest.test_case "skill invoke workspace path blocked" `Quick
      test_skill_invoke_workspace_path_blocked;
    Alcotest.test_case "skill invoke workspace binary path blocked" `Quick
      test_skill_invoke_workspace_binary_path_blocked;
  ]

let test_substitute_template () =
  let result =
    Skills.substitute_template "cd {{path}} && git status"
      (`Assoc [ ("path", `String "/tmp/repo") ])
  in
  Alcotest.(check string) "substituted" "cd /tmp/repo && git status" result

let test_substitute_multiple () =
  let result =
    Skills.substitute_template "{{a}} and {{b}}"
      (`Assoc [ ("a", `String "X"); ("b", `String "Y") ])
  in
  Alcotest.(check string) "multiple subs" "X and Y" result

let test_substitute_no_match () =
  let result = Skills.substitute_template "no params here" (`Assoc []) in
  Alcotest.(check string) "no substitution" "no params here" result

let process_exists pid =
  try
    Unix.kill pid 0;
    true
  with Unix.Unix_error _ -> false

let with_temp_skill_file json f =
  let tmp = Filename.temp_file "skill_test" ".json" in
  let oc = open_out tmp in
  output_string oc json;
  close_out oc;
  Fun.protect (fun () -> f tmp) ~finally:(fun () -> Sys.remove tmp)

let test_load_skill_from_file () =
  let json =
    {|{
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
  }|}
  in
  with_temp_skill_file json (fun tmp ->
      match Skills.load_skill tmp with
      | Some tool ->
          Alcotest.(check string) "tool name" "test_skill" tool.name;
          Alcotest.(check string)
            "tool description" "A test skill" tool.description;
          Alcotest.(check bool)
            "risk level low" true
            (tool.risk_level = Tool.Low)
      | None -> Alcotest.fail "failed to load skill")

let test_load_skill_invalid () =
  let tmp = Filename.temp_file "skill_bad" ".json" in
  let oc = open_out tmp in
  output_string oc "not valid json {{{";
  close_out oc;
  let result = Skills.load_skill tmp in
  Alcotest.(check bool) "invalid skill returns None" true (result = None);
  Sys.remove tmp

let test_load_all_empty_dir () =
  let tmp_dir =
    Filename.get_temp_dir_name ()
    ^ "/clawq_skills_test_"
    ^ string_of_int (Random.int 100000)
  in
  (try Sys.mkdir tmp_dir 0o755 with _ -> ());
  let skills = Skills.load_all ~dir:tmp_dir () in
  Alcotest.(check int) "no skills in empty dir" 0 (List.length skills);
  try Sys.rmdir tmp_dir with _ -> ()

let test_load_all_with_skill () =
  let tmp_dir =
    Filename.get_temp_dir_name ()
    ^ "/clawq_skills_test_"
    ^ string_of_int (Random.int 100000)
  in
  (try Sys.mkdir tmp_dir 0o755 with _ -> ());
  let skill_path = Filename.concat tmp_dir "echo.json" in
  let json =
    {|{
    "name": "echo_test",
    "description": "Echo a message",
    "parameters": {"type": "object", "properties": {}},
    "command": "echo hello",
    "risk_level": "low"
  }|}
  in
  let oc = open_out skill_path in
  output_string oc json;
  close_out oc;
  let skills = Skills.load_all ~dir:tmp_dir () in
  Alcotest.(check int) "one skill loaded" 1 (List.length skills);
  let s = List.hd skills in
  Alcotest.(check string) "skill name" "echo_test" s.name;
  Sys.remove skill_path;
  try Sys.rmdir tmp_dir with _ -> ()

let test_list_skills_empty () =
  let tmp_dir =
    Filename.get_temp_dir_name ()
    ^ "/clawq_skills_list_"
    ^ string_of_int (Random.int 100000)
  in
  (try Sys.mkdir tmp_dir 0o755 with _ -> ());
  let files = Skills.list_skills ~dir:tmp_dir () in
  Alcotest.(check int) "no files" 0 (List.length files);
  try Sys.rmdir tmp_dir with _ -> ()

let test_risk_level_parsing () =
  Alcotest.(check bool)
    "high" true
    (Skills.risk_level_of_string "high" = Tool.High);
  Alcotest.(check bool)
    "medium" true
    (Skills.risk_level_of_string "medium" = Tool.Medium);
  Alcotest.(check bool) "low" true (Skills.risk_level_of_string "low" = Tool.Low);
  Alcotest.(check bool)
    "unknown" true
    (Skills.risk_level_of_string "xyz" = Tool.Low)

let test_skill_invoke_workspace_policy_blocked () =
  let tmp = Filename.temp_file "skill_policy" ".json" in
  let json =
    {|{
    "name": "policy_test",
    "description": "Policy test",
    "parameters": {"type": "object", "properties": {}},
    "command": "echo ok && whoami",
    "risk_level": "low"
  }|}
  in
  let oc = open_out tmp in
  output_string oc json;
  close_out oc;
  (match Skills.load_skill ~workspace_only:true tmp with
  | None -> Alcotest.fail "failed to load skill"
  | Some tool ->
      let out = Lwt_main.run (tool.invoke (`Assoc [])) in
      Alcotest.(check bool)
        "blocked" true
        (try
           let re = Str.regexp_string "unsafe shell syntax" in
           ignore (Str.search_forward re out 0);
           true
         with Not_found -> false));
  Sys.remove tmp

let test_skill_invoke_workspace_path_blocked () =
  let tmp = Filename.temp_file "skill_path_policy" ".json" in
  let json =
    {|{
    "name": "path_policy_test",
    "description": "Path policy test",
    "parameters": {"type": "object", "properties": {}},
    "command": "cat /etc/passwd",
    "risk_level": "low"
  }|}
  in
  let oc = open_out tmp in
  output_string oc json;
  close_out oc;
  (match Skills.load_skill ~workspace_only:true tmp with
  | None -> Alcotest.fail "failed to load skill"
  | Some tool ->
      let out = Lwt_main.run (tool.invoke (`Assoc [])) in
      Alcotest.(check bool)
        "blocked" true
        (try
           let re = Str.regexp_string "disallowed in workspace_only mode" in
           ignore (Str.search_forward re out 0);
           true
         with Not_found -> false));
  Sys.remove tmp

let test_skill_invoke_workspace_binary_path_blocked () =
  let tmp = Filename.temp_file "skill_bin_policy" ".json" in
  let json =
    {|{
    "name": "bin_policy_test",
    "description": "Binary policy test",
    "parameters": {"type": "object", "properties": {}},
    "command": "./echo hi",
    "risk_level": "low"
  }|}
  in
  let oc = open_out tmp in
  output_string oc json;
  close_out oc;
  (match Skills.load_skill ~workspace_only:true tmp with
  | None -> Alcotest.fail "failed to load skill"
  | Some tool ->
      let out = Lwt_main.run (tool.invoke (`Assoc [])) in
      Alcotest.(check bool)
        "blocked" true
        (try
           let re = Str.regexp_string "binary path is disallowed" in
           ignore (Str.search_forward re out 0);
           true
         with Not_found -> false));
  Sys.remove tmp

let test_skill_interrupt_kills_descendants () =
  let workspace = Filename.get_temp_dir_name () in
  let pid_file = Filename.concat workspace "skill-child.pid" in
  let json =
    Printf.sprintf
      {|{
    "name": "interrupt_test",
    "description": "Interrupt test",
    "parameters": {"type": "object", "properties": {}},
    "command": "sh -c 'sleep 10 & child=$!; printf \"%%s\" \"$child\" > %s; wait $child'",
    "risk_level": "low"
  }|}
      (Filename.quote pid_file)
  in
  with_temp_skill_file json (fun tmp ->
      match Skills.load_skill ~workspace_only:false tmp with
      | None -> Alcotest.fail "failed to load skill"
      | Some tool ->
          let interrupted = ref None in
          let result =
            Lwt_main.run
              (let open Lwt.Syntax in
               let trigger =
                 let rec wait_for_pid_file attempts =
                   if Sys.file_exists pid_file || attempts <= 0 then
                     Lwt.return_unit
                   else
                     let* () = Lwt_unix.sleep 0.02 in
                     wait_for_pid_file (attempts - 1)
                 in
                 let* () = wait_for_pid_file 50 in
                 interrupted := Some "stop now";
                 Lwt.return_unit
               in
               let invoke =
                 tool.invoke
                   ~context:
                     {
                       Tool.session_key = Some "web:test";
                       send_progress = None;
                       interrupt_check = Some (fun () -> !interrupted);
                       inject_system_messages = None;
                       effective_cwd = None;
                       request_cwd_change = None;
                     }
                   (`Assoc [])
               in
               let* result, () = Lwt.both invoke trigger in
               Lwt.return result)
          in
          let child_pid =
            let ic = open_in pid_file in
            Fun.protect
              (fun () -> int_of_string (input_line ic))
              ~finally:(fun () -> close_in ic)
          in
          let rec wait_until_gone attempts =
            if attempts <= 0 || not (process_exists child_pid) then ()
            else begin
              Unix.sleepf 0.05;
              wait_until_gone (attempts - 1)
            end
          in
          wait_until_gone 20;
          Alcotest.(check string)
            "interrupt result" "Skill command interrupted by user." result;
          Alcotest.(check bool)
            "child process terminated" false (process_exists child_pid);
          Sys.remove pid_file)

let test_skill_timeout_kills_descendants () =
  let workspace = Filename.get_temp_dir_name () in
  let pid_file = Filename.concat workspace "skill-timeout-child.pid" in
  let json =
    Printf.sprintf
      {|{
    "name": "timeout_test",
    "description": "Timeout test",
    "parameters": {"type": "object", "properties": {}},
    "command": "sh -c 'sleep 10 & child=$!; printf \"%%s\" \"$child\" > %s; wait $child'",
    "risk_level": "low"
  }|}
      (Filename.quote pid_file)
  in
  with_temp_skill_file json (fun tmp ->
      match Skills.load_skill ~workspace_only:false ~timeout_secs:0.2 tmp with
      | None -> Alcotest.fail "failed to load skill"
      | Some tool ->
          let result = Lwt_main.run (tool.invoke (`Assoc [])) in
          let child_pid =
            let ic = open_in pid_file in
            Fun.protect
              (fun () -> int_of_string (input_line ic))
              ~finally:(fun () -> close_in ic)
          in
          let rec wait_until_gone attempts =
            if attempts <= 0 || not (process_exists child_pid) then ()
            else begin
              Unix.sleepf 0.05;
              wait_until_gone (attempts - 1)
            end
          in
          wait_until_gone 20;
          Alcotest.(check string)
            "timeout result" "Error: skill command timed out after 0 seconds"
            result;
          Alcotest.(check bool)
            "child process terminated" false (process_exists child_pid);
          Sys.remove pid_file)

let test_is_valid_skill_name () =
  Alcotest.(check bool) "simple" true (Skills.is_valid_skill_name "hello");
  Alcotest.(check bool)
    "with hyphens" true
    (Skills.is_valid_skill_name "my-skill");
  Alcotest.(check bool)
    "with underscores" true
    (Skills.is_valid_skill_name "my_skill");
  Alcotest.(check bool)
    "with digits" true
    (Skills.is_valid_skill_name "skill123");
  Alcotest.(check bool) "empty" false (Skills.is_valid_skill_name "");
  Alcotest.(check bool)
    "with spaces" false
    (Skills.is_valid_skill_name "my skill");
  Alcotest.(check bool)
    "with dots" false
    (Skills.is_valid_skill_name "my.skill");
  Alcotest.(check bool)
    "with slash" false
    (Skills.is_valid_skill_name "my/skill");
  Alcotest.(check bool)
    "too long" false
    (Skills.is_valid_skill_name (String.make 65 'a'))

let make_tmp_skills_dir () =
  let dir =
    Filename.get_temp_dir_name ()
    ^ "/clawq_skill_create_"
    ^ string_of_int (Random.int 100000)
  in
  (try Sys.mkdir dir 0o755 with _ -> ());
  dir

let test_skill_create_valid () =
  let tool = Skills.skill_create_tool () in
  let orig_home = try Some (Sys.getenv "HOME") with Not_found -> None in
  Unix.putenv "HOME" (Filename.get_temp_dir_name ());
  let parent = Filename.concat (Filename.get_temp_dir_name ()) ".clawq" in
  (try Sys.mkdir parent 0o755 with _ -> ());
  let sdir = Filename.concat parent "skills" in
  (try Sys.mkdir sdir 0o755 with _ -> ());
  let result =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [
              ("name", `String "test_create");
              ("description", `String "A test skill");
              ("command", `String "echo hello");
            ]))
  in
  let has_created =
    try
      ignore
        (Str.search_forward (Str.regexp_string "Created SKILL.md") result 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "skill created" true has_created;
  let skill_dir = Filename.concat sdir "test_create" in
  let skill_path = Filename.concat skill_dir "SKILL.md" in
  Alcotest.(check bool) "SKILL.md exists" true (Sys.file_exists skill_path);
  (try Sys.remove skill_path with _ -> ());
  (try Sys.rmdir skill_dir with _ -> ());
  (try Sys.rmdir sdir with _ -> ());
  (try Sys.rmdir parent with _ -> ());
  match orig_home with Some h -> Unix.putenv "HOME" h | None -> ()

let test_skill_create_invalid_name () =
  let tool = Skills.skill_create_tool () in
  let result =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [
              ("name", `String "bad/name");
              ("description", `String "desc");
              ("command", `String "echo");
            ]))
  in
  Alcotest.(check bool)
    "rejected" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Error:") result 0);
       true
     with Not_found -> false)

let test_skill_create_missing_fields () =
  let tool = Skills.skill_create_tool () in
  let r1 = Lwt_main.run (tool.invoke (`Assoc [ ("name", `String "x") ])) in
  Alcotest.(check bool)
    "missing desc" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string "description is required") r1 0);
       true
     with Not_found -> false);
  let r2 =
    Lwt_main.run
      (tool.invoke
         (`Assoc [ ("name", `String "x"); ("description", `String "d") ]))
  in
  Alcotest.(check bool)
    "missing command" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string "command is required") r2 0);
       true
     with Not_found -> false)

let test_skill_list_empty () =
  let tool = Skills.skill_list_tool () in
  let result = Lwt_main.run (tool.invoke (`Assoc [])) in
  Alcotest.(check bool) "returns something" true (String.length result > 0)

(* ── SKILL.md tests ── *)

let make_temp_dir prefix =
  let rec try_create n =
    let dir =
      Filename.get_temp_dir_name ()
      ^ "/" ^ prefix ^ "_"
      ^ string_of_int (Random.int 1000000)
    in
    if Sys.file_exists dir then
      if n > 0 then try_create (n - 1)
      else failwith ("make_temp_dir: cannot create unique dir for " ^ prefix)
    else begin
      Sys.mkdir dir 0o755;
      dir
    end
  in
  try_create 10

let rec rm_rf path =
  if Sys.is_directory path then begin
    Array.iter
      (fun entry -> rm_rf (Filename.concat path entry))
      (Sys.readdir path);
    Sys.rmdir path
  end
  else Sys.remove path

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let test_parse_frontmatter () =
  let pairs, body =
    Skills.parse_frontmatter
      "---\n\
       name: test\n\
       description: a test\n\
       allowed-tools: a, b\n\
       model: gpt-5\n\
       ---\n\
       Body here"
  in
  Alcotest.(check int) "4 pairs" 4 (List.length pairs);
  Alcotest.(check string) "name" "test" (List.assoc "name" pairs);
  Alcotest.(check string)
    "description" "a test"
    (List.assoc "description" pairs);
  Alcotest.(check string) "body" "Body here" body

let test_parse_frontmatter_no_delimiters () =
  let pairs, body = Skills.parse_frontmatter "No frontmatter here" in
  Alcotest.(check int) "empty pairs" 0 (List.length pairs);
  Alcotest.(check string) "full content as body" "No frontmatter here" body

let test_skill_md_meta_of_frontmatter () =
  let pairs =
    [
      ("name", "foo");
      ("description", "bar");
      ("allowed-tools", "read, write, exec");
      ("model", "gpt-5");
    ]
  in
  let meta = Skills.skill_md_meta_of_frontmatter ~source_path:"/test" pairs in
  Alcotest.(check bool) "Some" true (Option.is_some meta);
  let m = Option.get meta in
  Alcotest.(check string) "name" "foo" m.md_name;
  Alcotest.(check string) "desc" "bar" m.md_description;
  Alcotest.(check int) "3 tools" 3 (List.length m.md_allowed_tools);
  Alcotest.(check (option string)) "model" (Some "gpt-5") m.md_model;
  Alcotest.(check bool)
    "disable_model_invocation default false" false m.md_disable_model_invocation;
  let missing =
    Skills.skill_md_meta_of_frontmatter ~source_path:"/x" [ ("name", "foo") ]
  in
  Alcotest.(check bool) "None without desc" true (Option.is_none missing)

let test_load_skill_md () =
  let dir = make_temp_dir "skill_load" in
  let skill_dir = Filename.concat dir "test-skill" in
  Sys.mkdir skill_dir 0o755;
  let path = Filename.concat skill_dir "SKILL.md" in
  write_file path
    "---\nname: test-skill\ndescription: A test\n---\nDo the thing.";
  (match Skills.load_skill_md path with
  | Some skill ->
      Alcotest.(check string) "name" "test-skill" skill.meta.md_name;
      Alcotest.(check string) "instructions" "Do the thing." skill.instructions
  | None -> Alcotest.fail "expected Some");
  rm_rf dir

let test_load_skill_md_missing_name () =
  let dir = make_temp_dir "skill_noname" in
  let path = Filename.concat dir "bad.md" in
  write_file path "---\ndescription: no name\n---\nBody";
  let result = Skills.load_skill_md path in
  Alcotest.(check bool) "None" true (Option.is_none result);
  rm_rf dir

let test_load_skill_md_inferred_name () =
  let dir = make_temp_dir "skill_infer" in
  let path = Filename.concat dir "SKILL.md" in
  write_file path "---\ndescription: inferred\n---\nBody";
  (match Skills.load_skill_md ~default_name:"my-skill" path with
  | Some skill ->
      Alcotest.(check string) "name inferred" "my-skill" skill.meta.md_name;
      Alcotest.(check string) "desc" "inferred" skill.meta.md_description;
      Alcotest.(check string) "body" "Body" skill.instructions
  | None -> Alcotest.fail "expected Some with inferred name");
  rm_rf dir

let test_load_skill_md_no_description_with_default () =
  let dir = make_temp_dir "skill_nodesc" in
  let path = Filename.concat dir "SKILL.md" in
  write_file path "No frontmatter at all";
  let result = Skills.load_skill_md ~default_name:"fallback" path in
  Alcotest.(check bool) "None without desc" true (Option.is_none result);
  rm_rf dir

let test_scan_skill_dirs_infers_name_from_directory () =
  let base = make_temp_dir "skill_dir_infer" in
  let skills_dir = Filename.concat base "skills" in
  Sys.mkdir skills_dir 0o755;
  let sd = Filename.concat skills_dir "auto-named" in
  Sys.mkdir sd 0o755;
  write_file
    (Filename.concat sd "SKILL.md")
    "---\ndescription: auto discovered\n---\nauto body";
  let results = Skills.scan_skill_dirs [ skills_dir ] in
  Alcotest.(check int) "1 skill" 1 (List.length results);
  let s = List.hd results in
  Alcotest.(check string) "inferred name" "auto-named" s.md_name;
  Alcotest.(check string) "desc" "auto discovered" s.md_description;
  rm_rf base

let test_scan_skill_dirs_infers_name_flat_md () =
  let base = make_temp_dir "skill_flat_infer" in
  let skills_dir = Filename.concat base "skills" in
  Sys.mkdir skills_dir 0o755;
  write_file
    (Filename.concat skills_dir "flat-skill.md")
    "---\ndescription: flat inferred\n---\nflat body";
  let results = Skills.scan_skill_dirs [ skills_dir ] in
  Alcotest.(check int) "1 skill" 1 (List.length results);
  let s = List.hd results in
  Alcotest.(check string) "inferred name" "flat-skill" s.md_name;
  Alcotest.(check string) "desc" "flat inferred" s.md_description;
  rm_rf base

let test_scan_skill_dirs_name_mismatch () =
  let base = make_temp_dir "skill_mismatch" in
  let skills_dir = Filename.concat base "skills" in
  Sys.mkdir skills_dir 0o755;
  let sd = Filename.concat skills_dir "dir-name" in
  Sys.mkdir sd 0o755;
  write_file
    (Filename.concat sd "SKILL.md")
    "---\nname: frontmatter-name\ndescription: mismatch test\n---\nbody";
  let results = Skills.scan_skill_dirs [ skills_dir ] in
  Alcotest.(check int) "1 skill" 1 (List.length results);
  let s = List.hd results in
  Alcotest.(check string) "uses frontmatter name" "frontmatter-name" s.md_name;
  rm_rf base

let test_disable_model_invocation_true () =
  let pairs =
    [
      ("name", "hidden");
      ("description", "secret skill");
      ("disable-model-invocation", "true");
    ]
  in
  let meta = Skills.skill_md_meta_of_frontmatter ~source_path:"/t" pairs in
  let m = Option.get meta in
  Alcotest.(check bool) "disabled" true m.md_disable_model_invocation

let test_disable_model_invocation_false () =
  let pairs =
    [
      ("name", "visible");
      ("description", "normal skill");
      ("disable-model-invocation", "false");
    ]
  in
  let meta = Skills.skill_md_meta_of_frontmatter ~source_path:"/t" pairs in
  let m = Option.get meta in
  Alcotest.(check bool) "not disabled" false m.md_disable_model_invocation

let test_disable_model_invocation_case_insensitive () =
  let pairs =
    [
      ("name", "ci");
      ("description", "case test");
      ("disable-model-invocation", "True");
    ]
  in
  let meta = Skills.skill_md_meta_of_frontmatter ~source_path:"/t" pairs in
  let m = Option.get meta in
  Alcotest.(check bool) "True parses as true" true m.md_disable_model_invocation

let test_load_skill_md_disable_model_invocation () =
  let dir = make_temp_dir "skill_dmi" in
  let skill_dir = Filename.concat dir "hidden-skill" in
  Sys.mkdir skill_dir 0o755;
  let path = Filename.concat skill_dir "SKILL.md" in
  write_file path
    "---\n\
     name: hidden-skill\n\
     description: A hidden skill\n\
     disable-model-invocation: true\n\
     ---\n\
     Secret instructions.";
  (match Skills.load_skill_md path with
  | Some skill ->
      Alcotest.(check string) "name" "hidden-skill" skill.meta.md_name;
      Alcotest.(check bool)
        "disabled" true skill.meta.md_disable_model_invocation;
      Alcotest.(check string)
        "instructions" "Secret instructions." skill.instructions
  | None -> Alcotest.fail "expected Some");
  rm_rf dir

let test_substitute_arguments () =
  let r1 = Skills.substitute_arguments "Hello $ARGUMENTS world" "foo" in
  Alcotest.(check string) "single" "Hello foo world" r1;
  let r2 = Skills.substitute_arguments "$ARGUMENTS and $ARGUMENTS" "bar" in
  Alcotest.(check string) "multiple" "bar and bar" r2;
  let r3 = Skills.substitute_arguments "no args" "ignored" in
  Alcotest.(check string) "unchanged" "no args" r3

let test_discover_md_skills () =
  let base = make_temp_dir "skill_discover" in
  let cp = Filename.concat base ".claude-p/skills" in
  let cl = Filename.concat base ".claude/skills" in
  Sys.mkdir (Filename.concat base ".claude-p") 0o755;
  Sys.mkdir cp 0o755;
  Sys.mkdir (Filename.concat base ".claude") 0o755;
  Sys.mkdir cl 0o755;
  let s1 = Filename.concat cp "alpha" in
  Sys.mkdir s1 0o755;
  write_file
    (Filename.concat s1 "SKILL.md")
    "---\nname: alpha\ndescription: first\n---\nalpha body";
  let s2 = Filename.concat cl "beta" in
  Sys.mkdir s2 0o755;
  write_file
    (Filename.concat s2 "SKILL.md")
    "---\nname: beta\ndescription: second\n---\nbeta body";
  let s3 = Filename.concat cl "alpha" in
  Sys.mkdir s3 0o755;
  write_file
    (Filename.concat s3 "SKILL.md")
    "---\nname: alpha\ndescription: shadow\n---\nshadow body";
  let dirs = [ cp; cl ] in
  let results = Skills.scan_skill_dirs dirs in
  Alcotest.(check int) "2 skills (deduped)" 2 (List.length results);
  let alpha =
    List.find (fun (s : Skills.skill_md_meta) -> s.md_name = "alpha") results
  in
  Alcotest.(check string) "first-found wins" "first" alpha.md_description;
  rm_rf base

let test_use_skill_tool_found () =
  let dir = make_temp_dir "skill_use" in
  let skills_dir = Filename.concat dir ".claude/skills" in
  Sys.mkdir (Filename.concat dir ".claude") 0o755;
  Sys.mkdir skills_dir 0o755;
  let sd = Filename.concat skills_dir "test-use" in
  Sys.mkdir sd 0o755;
  write_file
    (Filename.concat sd "SKILL.md")
    "---\nname: test-use\ndescription: use test\n---\nFollow: $ARGUMENTS";
  let _cache = Skills.init_cache ~workspace_dir:dir () in
  ignore _cache;
  let tool = Skills.use_skill_tool () in
  (* use_skill now returns brief ack and injects system message *)
  let injected = ref [] in
  let context =
    {
      Tool.session_key = None;
      send_progress = None;
      interrupt_check = None;
      inject_system_messages = Some (fun msgs -> injected := !injected @ msgs);
      effective_cwd = None;
      request_cwd_change = None;
    }
  in
  let result =
    Lwt_main.run
      (tool.invoke ~context
         (`Assoc
            [ ("name", `String "test-use"); ("arguments", `String "my args") ]))
  in
  Alcotest.(check bool)
    "brief ack" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Loaded skill") result 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "has_args: true" true
    (try
       ignore (Str.search_forward (Str.regexp_string "has_args: true") result 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "system message injected with expanded content" true
    (match !injected with
    | [ msg ] -> (
        try
          ignore
            (Str.search_forward (Str.regexp_string "Follow: my args") msg 0);
          true
        with Not_found -> false)
    | _ -> false);
  Skills.global_cache := None;
  rm_rf dir

let test_use_skill_tool_not_found () =
  let dir = make_temp_dir "skill_nf" in
  Sys.mkdir (Filename.concat dir ".claude") 0o755;
  Sys.mkdir (Filename.concat dir ".claude/skills") 0o755;
  let _cache = Skills.init_cache ~workspace_dir:dir () in
  ignore _cache;
  let tool = Skills.use_skill_tool () in
  let result =
    Lwt_main.run (tool.invoke (`Assoc [ ("name", `String "nonexistent") ]))
  in
  Alcotest.(check bool)
    "error" true
    (try
       ignore (Str.search_forward (Str.regexp_string "not found") result 0);
       true
     with Not_found -> false);
  Skills.global_cache := None;
  rm_rf dir

let test_skill_md_priority () =
  let base = make_temp_dir "skill_prio" in
  let ws = Filename.concat base ".claude-p/skills" in
  let personal = Filename.concat base "personal" in
  Sys.mkdir (Filename.concat base ".claude-p") 0o755;
  Sys.mkdir ws 0o755;
  Sys.mkdir personal 0o755;
  let s1 = Filename.concat ws "dup" in
  Sys.mkdir s1 0o755;
  write_file
    (Filename.concat s1 "SKILL.md")
    "---\nname: dup\ndescription: workspace\n---\nws body";
  let s2 = Filename.concat personal "dup" in
  Sys.mkdir s2 0o755;
  write_file
    (Filename.concat s2 "SKILL.md")
    "---\nname: dup\ndescription: personal\n---\npersonal body";
  let results = Skills.scan_skill_dirs [ ws; personal ] in
  Alcotest.(check int) "1 skill (deduped)" 1 (List.length results);
  Alcotest.(check string)
    "workspace wins" "workspace" (List.hd results).md_description;
  rm_rf base

let test_parse_frontmatter_value_with_colons () =
  let pairs, body =
    Skills.parse_frontmatter
      "---\n\
       name: my-skill\n\
       description: URL is https://example.com:8080/path\n\
       ---\n\
       Body text"
  in
  Alcotest.(check int) "2 pairs" 2 (List.length pairs);
  Alcotest.(check string) "name" "my-skill" (List.assoc "name" pairs);
  Alcotest.(check string)
    "description preserves colons" "URL is https://example.com:8080/path"
    (List.assoc "description" pairs);
  Alcotest.(check string) "body" "Body text" body

let test_parse_frontmatter_unclosed () =
  let pairs, body =
    Skills.parse_frontmatter "---\nname: test\ndescription: unclosed"
  in
  Alcotest.(check int) "empty pairs (unclosed)" 0 (List.length pairs);
  Alcotest.(check string)
    "full content as body" "---\nname: test\ndescription: unclosed" body

let test_discover_flat_md_skills () =
  let base = make_temp_dir "skill_flat" in
  let skills_dir = Filename.concat base "skills" in
  Sys.mkdir skills_dir 0o755;
  write_file
    (Filename.concat skills_dir "gamma.md")
    "---\nname: gamma\ndescription: flat skill\n---\ngamma body";
  let sub = Filename.concat skills_dir "delta" in
  Sys.mkdir sub 0o755;
  write_file
    (Filename.concat sub "SKILL.md")
    "---\nname: delta\ndescription: dir skill\n---\ndelta body";
  let results = Skills.scan_skill_dirs [ skills_dir ] in
  Alcotest.(check int) "2 skills (flat + dir)" 2 (List.length results);
  let has name =
    List.exists (fun (s : Skills.skill_md_meta) -> s.md_name = name) results
  in
  Alcotest.(check bool) "gamma found" true (has "gamma");
  Alcotest.(check bool) "delta found" true (has "delta");
  rm_rf base

let test_extract_skill_refs () =
  let skills : Skills.skill_md_meta list =
    [
      {
        md_name = "review-and-fix";
        md_description = "review";
        md_allowed_tools = [];
        md_model = None;
        md_disable_model_invocation = false;
        md_source_path = "/test";
      };
      {
        md_name = "foo";
        md_description = "foo skill";
        md_allowed_tools = [];
        md_model = None;
        md_disable_model_invocation = false;
        md_source_path = "/test2";
      };
    ]
  in
  let r1 = Skills.extract_skill_refs skills "please check @review-and-fix" in
  Alcotest.(check int) "1 ref" 1 (List.length r1);
  Alcotest.(check string) "matched text" "@review-and-fix" (fst (List.hd r1));
  let r2 = Skills.extract_skill_refs skills "@unknown-thing" in
  Alcotest.(check int) "0 refs for unknown" 0 (List.length r2);
  let r3 = Skills.extract_skill_refs skills "user@foo.com" in
  Alcotest.(check int) "no email false-positive" 0 (List.length r3);
  let r4 = Skills.extract_skill_refs skills "@review-and-fix and @foo" in
  Alcotest.(check int) "2 refs" 2 (List.length r4);
  let r5 =
    Skills.extract_skill_refs skills "@review-and-fix text @review-and-fix"
  in
  Alcotest.(check int) "dedup" 1 (List.length r5)

let test_slash_command_skill () =
  let r1 = Slash_commands.handle ~skill_names:[ "foo" ] "/foo bar baz" in
  (match r1 with
  | Slash_commands.SkillInvoke (name, args) ->
      Alcotest.(check string) "skill name" "foo" name;
      Alcotest.(check string) "args" "bar baz" args
  | _ -> Alcotest.fail "expected SkillInvoke");
  let r2 = Slash_commands.handle ~skill_names:[ "foo" ] "/unknown" in
  match r2 with
  | Slash_commands.NotACommand -> ()
  | _ -> Alcotest.fail "expected NotACommand"

let test_skill_list_tool_both_formats () =
  let base = make_temp_dir "skill_list_both" in
  let skills_dir = Filename.concat base ".claude/skills" in
  Sys.mkdir (Filename.concat base ".claude") 0o755;
  Sys.mkdir skills_dir 0o755;
  let sd = Filename.concat skills_dir "md-skill" in
  Sys.mkdir sd 0o755;
  write_file
    (Filename.concat sd "SKILL.md")
    "---\nname: md-test\ndescription: md skill\n---\nmd body";
  let tool = Skills.skill_list_tool ~workspace_dir:base () in
  let result = Lwt_main.run (tool.invoke (`Assoc [])) in
  Alcotest.(check bool)
    "contains md skill" true
    (try
       ignore (Str.search_forward (Str.regexp_string "md-test") result 0);
       true
     with Not_found -> false);
  rm_rf base

let test_skill_list_shows_deprecated () =
  let base = make_temp_dir "skill_deprec" in
  let clawq_dir = Filename.concat base ".clawq" in
  Sys.mkdir clawq_dir 0o755;
  let json_dir = Filename.concat clawq_dir "skills" in
  Sys.mkdir json_dir 0o755;
  write_file
    (Filename.concat json_dir "old.json")
    {|{"name": "old-skill", "description": "legacy", "parameters": {"type": "object", "properties": {}}, "command": "echo old", "risk_level": "low"}|};
  let orig_home = try Some (Sys.getenv "HOME") with Not_found -> None in
  Unix.putenv "HOME" base;
  let tool = Skills.skill_list_tool () in
  let result = Lwt_main.run (tool.invoke (`Assoc [])) in
  (match orig_home with Some h -> Unix.putenv "HOME" h | None -> ());
  Alcotest.(check bool)
    "shows DEPRECATED" true
    (try
       ignore (Str.search_forward (Str.regexp_string "DEPRECATED") result 0);
       true
     with Not_found -> false);
  rm_rf base

let test_skill_create_md_format () =
  let tool = Skills.skill_create_tool () in
  let orig_home = try Some (Sys.getenv "HOME") with Not_found -> None in
  Unix.putenv "HOME" (Filename.get_temp_dir_name ());
  let parent = Filename.concat (Filename.get_temp_dir_name ()) ".clawq" in
  (try Sys.mkdir parent 0o755 with _ -> ());
  let sdir = Filename.concat parent "skills" in
  (try Sys.mkdir sdir 0o755 with _ -> ());
  let result =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [
              ("name", `String "md_test");
              ("description", `String "md test skill");
              ("command", `String "echo md-works");
            ]))
  in
  Alcotest.(check bool)
    "created" true
    (try
       ignore (Str.search_forward (Str.regexp_string "SKILL.md") result 0);
       true
     with Not_found -> false);
  let skill_dir = Filename.concat sdir "md_test" in
  let skill_path = Filename.concat skill_dir "SKILL.md" in
  Alcotest.(check bool) "SKILL.md file exists" true (Sys.file_exists skill_path);
  let ic = open_in skill_path in
  let content =
    Fun.protect
      (fun () ->
        let len = in_channel_length ic in
        let buf = Bytes.create len in
        really_input ic buf 0 len;
        Bytes.to_string buf)
      ~finally:(fun () -> close_in ic)
  in
  Alcotest.(check bool)
    "contains injection" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string "!`echo md-works`") content 0);
       true
     with Not_found -> false);
  (try Sys.remove skill_path with _ -> ());
  (try Sys.rmdir skill_dir with _ -> ());
  (try Sys.rmdir sdir with _ -> ());
  (try Sys.rmdir parent with _ -> ());
  match orig_home with Some h -> Unix.putenv "HOME" h | None -> ()

let test_use_skill_with_injection () =
  let dir = make_temp_dir "skill_inject" in
  let skills_dir = Filename.concat dir ".claude/skills" in
  Sys.mkdir (Filename.concat dir ".claude") 0o755;
  Sys.mkdir skills_dir 0o755;
  let sd = Filename.concat skills_dir "inject-test" in
  Sys.mkdir sd 0o755;
  write_file
    (Filename.concat sd "SKILL.md")
    "---\n\
     name: inject-test\n\
     description: injection test\n\
     ---\n\
     Result: !`echo injected-value`";
  let _cache = Skills.init_cache ~workspace_dir:dir () in
  ignore _cache;
  let tool = Skills.use_skill_tool () in
  let injected = ref [] in
  let context =
    {
      Tool.session_key = None;
      send_progress = None;
      interrupt_check = None;
      inject_system_messages = Some (fun msgs -> injected := !injected @ msgs);
      effective_cwd = None;
      request_cwd_change = None;
    }
  in
  let result =
    Lwt_main.run
      (tool.invoke ~context (`Assoc [ ("name", `String "inject-test") ]))
  in
  Alcotest.(check bool)
    "brief ack returned" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Loaded skill") result 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "system message contains expanded value" true
    (match !injected with
    | [ msg ] -> (
        try
          ignore
            (Str.search_forward
               (Str.regexp_string "Result: injected-value")
               msg 0);
          true
        with Not_found -> false)
    | _ -> false);
  Skills.global_cache := None;
  rm_rf dir

let test_expand_slash_skill_found () =
  let dir = make_temp_dir "slash_expand" in
  let skills_dir = Filename.concat dir ".claude/skills" in
  Sys.mkdir (Filename.concat dir ".claude") 0o755;
  Sys.mkdir skills_dir 0o755;
  let sd = Filename.concat skills_dir "my-skill" in
  Sys.mkdir sd 0o755;
  write_file
    (Filename.concat sd "SKILL.md")
    "---\nname: my-skill\ndescription: test skill\n---\nDo this: $ARGUMENTS";
  let _cache = Skills.init_cache ~workspace_dir:dir () in
  (* No args: injection has raw body *)
  (match
     Lwt_main.run (Skills.expand_slash_skill ~name:"my-skill" ~args:"" ())
   with
  | Ok r ->
      Alcotest.(check bool) "has_args false" false r.has_args;
      Alcotest.(check bool)
        "injection contains skill header" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "[Skill: my-skill]")
                r.skill_injection 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "injection contains raw body" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "Do this: $ARGUMENTS")
                r.skill_injection 0);
           true
         with Not_found -> false)
  | Error _ -> Alcotest.fail "expected Ok");
  (* With args: $ARGUMENTS substituted *)
  (match
     Lwt_main.run
       (Skills.expand_slash_skill ~name:"my-skill" ~args:"hello world" ())
   with
  | Ok r ->
      Alcotest.(check bool) "has_args true" true r.has_args;
      Alcotest.(check bool)
        "injection contains expanded args" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "Do this: hello world")
                r.skill_injection 0);
           true
         with Not_found -> false)
  | Error _ -> Alcotest.fail "expected Ok");
  Skills.global_cache := None;
  rm_rf dir

let test_expand_slash_skill_not_found () =
  let dir = make_temp_dir "slash_nf" in
  Sys.mkdir (Filename.concat dir ".claude") 0o755;
  Sys.mkdir (Filename.concat dir ".claude/skills") 0o755;
  let _cache = Skills.init_cache ~workspace_dir:dir () in
  (match
     Lwt_main.run (Skills.expand_slash_skill ~name:"nonexistent" ~args:"" ())
   with
  | Error msg ->
      Alcotest.(check bool)
        "error mentions skill name" true
        (try
           ignore (Str.search_forward (Str.regexp_string "nonexistent") msg 0);
           true
         with Not_found -> false)
  | Ok _ -> Alcotest.fail "expected Error");
  Skills.global_cache := None;
  rm_rf dir

let test_use_skill_no_context () =
  let dir = make_temp_dir "skill_noctx" in
  let skills_dir = Filename.concat dir ".claude/skills" in
  Sys.mkdir (Filename.concat dir ".claude") 0o755;
  Sys.mkdir skills_dir 0o755;
  let sd = Filename.concat skills_dir "ctx-test" in
  Sys.mkdir sd 0o755;
  write_file
    (Filename.concat sd "SKILL.md")
    "---\nname: ctx-test\ndescription: test\n---\nInstructions here";
  let _cache = Skills.init_cache ~workspace_dir:dir () in
  let tool = Skills.use_skill_tool () in
  (* Without context, should still return brief ack without crashing *)
  let result =
    Lwt_main.run (tool.invoke (`Assoc [ ("name", `String "ctx-test") ]))
  in
  Alcotest.(check bool)
    "brief ack" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Loaded skill") result 0);
       true
     with Not_found -> false);
  Skills.global_cache := None;
  rm_rf dir

let test_dedup_no_args_skill () =
  let history =
    [
      Provider.make_message ~role:"system"
        ~content:"[Skill: my-skill]\nDo something";
      Provider.make_message ~role:"user" ~content:"hello";
    ]
  in
  let injections = [ "[Skill: my-skill]\nDo something" ] in
  let result = Skill_dedup.dedup_skill_injections ~history injections in
  Alcotest.(check int) "one result" 1 (List.length result);
  let r = List.hd result in
  Alcotest.(check bool)
    "deduped to already loaded" true
    (try
       ignore (Str.search_forward (Str.regexp_string "already loaded") r 0);
       true
     with Not_found -> false)

let test_dedup_with_args_not_deduped () =
  let history =
    [
      Provider.make_message ~role:"system"
        ~content:"[Skill: my-skill (args)]\nDo this: hello";
      Provider.make_message ~role:"user" ~content:"hi";
    ]
  in
  let injections = [ "[Skill: my-skill (args)]\nDo this: world" ] in
  let result = Skill_dedup.dedup_skill_injections ~history injections in
  Alcotest.(check int) "one result" 1 (List.length result);
  let r = List.hd result in
  Alcotest.(check bool)
    "not deduped (has args)" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Do this: world") r 0);
       true
     with Not_found -> false)

let test_dedup_new_skill_not_deduped () =
  let history =
    [
      Provider.make_message ~role:"system"
        ~content:"[Skill: other-skill]\nOther instructions";
    ]
  in
  let injections = [ "[Skill: my-skill]\nDo something" ] in
  let result = Skill_dedup.dedup_skill_injections ~history injections in
  Alcotest.(check int) "one result" 1 (List.length result);
  let r = List.hd result in
  Alcotest.(check bool)
    "not deduped (different skill)" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Do something") r 0);
       true
     with Not_found -> false)

let test_compaction_skill_reload () =
  let dir = make_temp_dir "compact_reload" in
  let skills_dir = Filename.concat dir ".claude/skills" in
  Sys.mkdir (Filename.concat dir ".claude") 0o755;
  Sys.mkdir skills_dir 0o755;
  let sd = Filename.concat skills_dir "reload-me" in
  Sys.mkdir sd 0o755;
  write_file
    (Filename.concat sd "SKILL.md")
    "---\nname: reload-me\ndescription: reloadable\n---\nReload instructions";
  let _cache = Skills.init_cache ~workspace_dir:dir () in
  (Agent.find_skill_for_reload_fn :=
     fun name ->
       match Skills.find_skill_md name with
       | Some s -> Some (s.meta.md_description, s.instructions)
       | None -> None);
  let to_compact =
    [
      Provider.make_message ~role:"system"
        ~content:"[Skill: reload-me]\nReload instructions";
      Provider.make_message ~role:"user" ~content:"do work";
      Provider.make_message ~role:"assistant" ~content:"done";
    ]
  in
  let to_keep = [ Provider.make_message ~role:"user" ~content:"more work" ] in
  let result = Agent.reload_skills_after_compaction ~to_compact ~to_keep in
  Alcotest.(check int) "one skill reloaded" 1 (List.length result);
  let msg = List.hd result in
  Alcotest.(check string) "role is system" "system" msg.role;
  Alcotest.(check bool)
    "marked autoloaded" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "autoloaded after compaction")
            msg.content 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains instructions" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "Reload instructions")
            msg.content 0);
       true
     with Not_found -> false);
  (Agent.find_skill_for_reload_fn := fun _ -> None);
  Skills.global_cache := None;
  rm_rf dir

let test_compaction_no_reload_if_kept () =
  (Agent.find_skill_for_reload_fn := fun _ -> Some ("desc", "body"));
  let to_compact =
    [
      Provider.make_message ~role:"system"
        ~content:"[Skill: kept-skill]\nInstructions";
    ]
  in
  let to_keep =
    [
      Provider.make_message ~role:"system"
        ~content:"[Skill: kept-skill]\nInstructions";
    ]
  in
  let result = Agent.reload_skills_after_compaction ~to_compact ~to_keep in
  Alcotest.(check int) "no reload (already kept)" 0 (List.length result);
  Agent.find_skill_for_reload_fn := fun _ -> None

let has_substring ~sub s =
  try
    ignore (Str.search_forward (Str.regexp_string sub) s 0);
    true
  with Not_found -> false

let test_compaction_reload_at_cap () =
  (* 4 skills = exactly at the cap; all should auto-load, no overflow *)
  (Agent.find_skill_for_reload_fn :=
     fun name -> Some ("desc", "instructions for " ^ name));
  let to_compact =
    List.map
      (fun i ->
        Provider.make_message ~role:"system"
          ~content:(Printf.sprintf "[Skill: skill-%d]\nbody" i))
      [ 1; 2; 3; 4 ]
  in
  let to_keep = [ Provider.make_message ~role:"user" ~content:"hi" ] in
  let result = Agent.reload_skills_after_compaction ~to_compact ~to_keep in
  Alcotest.(check int) "all 4 auto-loaded" 4 (List.length result);
  List.iter
    (fun (msg : Provider.message) ->
      Alcotest.(check bool)
        "marked autoloaded" true
        (has_substring ~sub:"autoloaded after compaction" msg.content))
    result;
  Agent.find_skill_for_reload_fn := fun _ -> None

let test_compaction_reload_overflow () =
  (* 6 skills: first 4 auto-loaded, remaining 2 in overflow message *)
  (Agent.find_skill_for_reload_fn :=
     fun name -> Some ("desc", "instructions for " ^ name));
  let to_compact =
    List.map
      (fun i ->
        Provider.make_message ~role:"system"
          ~content:(Printf.sprintf "[Skill: skill-%d]\nbody" i))
      [ 1; 2; 3; 4; 5; 6 ]
  in
  let to_keep = [ Provider.make_message ~role:"user" ~content:"hi" ] in
  let result = Agent.reload_skills_after_compaction ~to_compact ~to_keep in
  (* 4 auto-loaded + 1 overflow message = 5 *)
  Alcotest.(check int) "4 auto + 1 overflow" 5 (List.length result);
  let overflow_msg = List.nth result 4 in
  Alcotest.(check bool)
    "overflow lists skill-5" true
    (has_substring ~sub:"skill-5" overflow_msg.content);
  Alcotest.(check bool)
    "overflow lists skill-6" true
    (has_substring ~sub:"skill-6" overflow_msg.content);
  Alcotest.(check bool)
    "overflow mentions use_skill" true
    (has_substring ~sub:"use_skill(name='skill-name')" overflow_msg.content);
  Alcotest.(check bool)
    "overflow does not list skill-4" false
    (has_substring ~sub:"skill-4" overflow_msg.content);
  Agent.find_skill_for_reload_fn := fun _ -> None

let test_compaction_reload_empty () =
  (* No skills in compacted messages -> empty result *)
  (Agent.find_skill_for_reload_fn := fun _ -> Some ("desc", "body"));
  let to_compact =
    [
      Provider.make_message ~role:"user" ~content:"hello";
      Provider.make_message ~role:"assistant" ~content:"hi";
    ]
  in
  let to_keep = [ Provider.make_message ~role:"user" ~content:"more" ] in
  let result = Agent.reload_skills_after_compaction ~to_compact ~to_keep in
  Alcotest.(check int) "empty result" 0 (List.length result);
  Agent.find_skill_for_reload_fn := fun _ -> None

let test_builtin_idea_skill () =
  let found = Builtin_skills.find_builtin "idea" in
  Alcotest.(check bool) "idea skill found" true (Option.is_some found);
  let name, desc, instructions = Option.get found in
  Alcotest.(check string) "name" "idea" name;
  Alcotest.(check bool) "description non-empty" true (String.length desc > 0);
  Alcotest.(check bool)
    "instructions contain $ARGUMENTS" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string "$ARGUMENTS") instructions 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "instructions contain bl idea" true
    (try
       ignore (Str.search_forward (Str.regexp_string "bl idea") instructions 0);
       true
     with Not_found -> false);
  let skill_md = Skills.find_skill_md "idea" in
  Alcotest.(check bool)
    "find_skill_md finds idea" true (Option.is_some skill_md)

(* B678: briefing-hourly and briefing-daily are deterministic built-in skills
   that the briefing cron jobs invoke. They must be discoverable and contain
   the pre-flight validation directives.
   B680: the skills must also require delivery_session in pre-flight and
   call send_to_session at the end so the cron worker session never
   silently absorbs the briefing output. *)
let test_builtin_briefing_skills_present () =
  let contains haystack needle =
    try
      ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
      true
    with Not_found -> false
  in
  let check_skill name =
    let found = Builtin_skills.find_builtin name in
    Alcotest.(check bool) (name ^ " skill found") true (Option.is_some found);
    let _, _, instructions = Option.get found in
    Alcotest.(check bool)
      (name ^ " mentions memory_recall")
      true
      (contains instructions "memory_recall");
    Alcotest.(check bool)
      (name ^ " enforces non-empty query")
      true
      (contains instructions "non-empty");
    Alcotest.(check bool)
      (name ^ " mentions pre-flight")
      true
      (try
         ignore
           (Str.search_forward
              (Str.regexp_case_fold "pre-flight")
              instructions 0);
         true
       with Not_found -> false);
    Alcotest.(check bool)
      (name ^ " requires delivery_session")
      true
      (contains instructions "delivery_session");
    Alcotest.(check bool)
      (name ^ " calls send_to_session")
      true
      (contains instructions "send_to_session");
    Alcotest.(check bool)
      (name ^ " references wake_agent")
      true
      (contains instructions "wake_agent")
  in
  check_skill "briefing-hourly";
  check_skill "briefing-daily"

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
    Alcotest.test_case "skill interrupt kills descendants" `Quick
      test_skill_interrupt_kills_descendants;
    Alcotest.test_case "skill timeout kills descendants" `Quick
      test_skill_timeout_kills_descendants;
    Alcotest.test_case "valid skill name" `Quick test_is_valid_skill_name;
    Alcotest.test_case "skill_create valid" `Quick test_skill_create_valid;
    Alcotest.test_case "skill_create invalid name" `Quick
      test_skill_create_invalid_name;
    Alcotest.test_case "skill_create missing fields" `Quick
      test_skill_create_missing_fields;
    Alcotest.test_case "skill_list tool" `Quick test_skill_list_empty;
    Alcotest.test_case "parse frontmatter" `Quick test_parse_frontmatter;
    Alcotest.test_case "parse frontmatter no delimiters" `Quick
      test_parse_frontmatter_no_delimiters;
    Alcotest.test_case "skill_md_meta_of_frontmatter" `Quick
      test_skill_md_meta_of_frontmatter;
    Alcotest.test_case "load skill md" `Quick test_load_skill_md;
    Alcotest.test_case "load skill md missing name" `Quick
      test_load_skill_md_missing_name;
    Alcotest.test_case "load skill md inferred name" `Quick
      test_load_skill_md_inferred_name;
    Alcotest.test_case "load skill md no desc with default" `Quick
      test_load_skill_md_no_description_with_default;
    Alcotest.test_case "scan infers name from directory" `Quick
      test_scan_skill_dirs_infers_name_from_directory;
    Alcotest.test_case "scan infers name from flat md" `Quick
      test_scan_skill_dirs_infers_name_flat_md;
    Alcotest.test_case "scan warns name mismatch" `Quick
      test_scan_skill_dirs_name_mismatch;
    Alcotest.test_case "disable-model-invocation true" `Quick
      test_disable_model_invocation_true;
    Alcotest.test_case "disable-model-invocation false" `Quick
      test_disable_model_invocation_false;
    Alcotest.test_case "disable-model-invocation case insensitive" `Quick
      test_disable_model_invocation_case_insensitive;
    Alcotest.test_case "load skill md disable-model-invocation" `Quick
      test_load_skill_md_disable_model_invocation;
    Alcotest.test_case "substitute arguments" `Quick test_substitute_arguments;
    Alcotest.test_case "discover md skills" `Quick test_discover_md_skills;
    Alcotest.test_case "use_skill tool found" `Quick test_use_skill_tool_found;
    Alcotest.test_case "use_skill tool not found" `Quick
      test_use_skill_tool_not_found;
    Alcotest.test_case "skill md priority" `Quick test_skill_md_priority;
    Alcotest.test_case "parse frontmatter value with colons" `Quick
      test_parse_frontmatter_value_with_colons;
    Alcotest.test_case "parse frontmatter unclosed" `Quick
      test_parse_frontmatter_unclosed;
    Alcotest.test_case "discover flat md skills" `Quick
      test_discover_flat_md_skills;
    Alcotest.test_case "extract skill refs" `Quick test_extract_skill_refs;
    Alcotest.test_case "slash command skill" `Quick test_slash_command_skill;
    Alcotest.test_case "skill list tool both formats" `Quick
      test_skill_list_tool_both_formats;
    Alcotest.test_case "skill list shows deprecated" `Quick
      test_skill_list_shows_deprecated;
    Alcotest.test_case "skill create md format" `Quick
      test_skill_create_md_format;
    Alcotest.test_case "use skill with injection" `Quick
      test_use_skill_with_injection;
    Alcotest.test_case "expand slash skill found" `Quick
      test_expand_slash_skill_found;
    Alcotest.test_case "expand slash skill not found" `Quick
      test_expand_slash_skill_not_found;
    Alcotest.test_case "use skill no context" `Quick test_use_skill_no_context;
    Alcotest.test_case "dedup no-args skill" `Quick test_dedup_no_args_skill;
    Alcotest.test_case "dedup with-args not deduped" `Quick
      test_dedup_with_args_not_deduped;
    Alcotest.test_case "dedup new skill not deduped" `Quick
      test_dedup_new_skill_not_deduped;
    Alcotest.test_case "compaction skill reload" `Quick
      test_compaction_skill_reload;
    Alcotest.test_case "compaction no reload if kept" `Quick
      test_compaction_no_reload_if_kept;
    Alcotest.test_case "compaction reload at cap" `Quick
      test_compaction_reload_at_cap;
    Alcotest.test_case "compaction reload overflow" `Quick
      test_compaction_reload_overflow;
    Alcotest.test_case "compaction reload empty" `Quick
      test_compaction_reload_empty;
    Alcotest.test_case "builtin idea skill discoverable" `Quick
      test_builtin_idea_skill;
    Alcotest.test_case
      "B678: briefing-hourly and briefing-daily built-in skills present" `Quick
      test_builtin_briefing_skills_present;
  ]

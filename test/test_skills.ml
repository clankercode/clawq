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
  let registry = Tool_registry.create () in
  let tool =
    Skills.skill_create_tool ~workspace_only:false ~allowed_commands:[] registry
  in
  let tmp_dir = make_tmp_skills_dir () in
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
      ignore (Str.search_forward (Str.regexp_string "Created skill") result 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "skill created" true has_created;
  let found = Tool_registry.find registry "test_create" in
  Alcotest.(check bool) "hot-reloaded" true (found <> None);
  (try Sys.remove (Filename.concat sdir "test_create.json") with _ -> ());
  (try Sys.rmdir sdir with _ -> ());
  (try Sys.rmdir parent with _ -> ());
  (try Sys.rmdir tmp_dir with _ -> ());
  match orig_home with Some h -> Unix.putenv "HOME" h | None -> ()

let test_skill_create_invalid_name () =
  let registry = Tool_registry.create () in
  let tool =
    Skills.skill_create_tool ~workspace_only:false ~allowed_commands:[] registry
  in
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
  let registry = Tool_registry.create () in
  let tool =
    Skills.skill_create_tool ~workspace_only:false ~allowed_commands:[] registry
  in
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
  let result =
    Lwt_main.run
      (tool.invoke
         (`Assoc
            [ ("name", `String "test-use"); ("arguments", `String "my args") ]))
  in
  Alcotest.(check bool)
    "contains Follow" true
    (try
       ignore
         (Str.search_forward (Str.regexp_string "Follow: my args") result 0);
       true
     with Not_found -> false);
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
        md_source_path = "/test";
      };
      {
        md_name = "foo";
        md_description = "foo skill";
        md_allowed_tools = [];
        md_model = None;
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
  ]

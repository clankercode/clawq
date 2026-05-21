(* test_agent_template.ml — Tests for agent template system *)

let valid_template_md =
  "---\n\
   name: my-agent\n\
   description: A test agent\n\
   role: coder\n\
   goal: Write good code\n\
   backstory: You are a skilled developer\n\
   model: openai:gpt-5.4\n\
   max-tool-iterations: 15\n\
   allowed-tools: file_read, shell_exec\n\
   disallowed-tools: file_write\n\
   tool-search-enabled: true\n\
   reasoning-effort: high\n\
   custom-key: custom-value\n\
   ---\n\n\
   You are the test agent.\n"

let test_parse_valid_template () =
  match
    Agent_template.parse_template ~source_path:"/tmp/test.md" valid_template_md
  with
  | Error e -> Alcotest.fail e
  | Ok t -> (
      Alcotest.(check string) "name" "my-agent" t.name;
      Alcotest.(check string) "description" "A test agent" t.description;
      Alcotest.(check string)
        "role" "coder"
        (Agent_template.role_to_string t.role);
      Alcotest.(check string) "goal" "Write good code" t.goal;
      Alcotest.(check string)
        "backstory" "You are a skilled developer" t.backstory;
      Alcotest.(check (option string)) "model" (Some "openai:gpt-5.4") t.model;
      Alcotest.(check (option int))
        "max_tool_iterations" (Some 15) t.max_tool_iterations;
      Alcotest.(check (list string))
        "allowed_tools"
        [ "file_read"; "shell_exec" ]
        t.allowed_tools;
      Alcotest.(check (list string))
        "disallowed_tools" [ "file_write" ] t.disallowed_tools;
      Alcotest.(check (option bool))
        "tool_search_enabled" (Some true) t.tool_search_enabled;
      Alcotest.(check (option string))
        "reasoning_effort" (Some "high") t.reasoning_effort;
      Alcotest.(check (option string)) "cwd absent" None t.cwd;
      Alcotest.(check string)
        "system_prompt" "You are the test agent." t.system_prompt;
      let custom = List.assoc_opt "custom-key" t.metadata in
      Alcotest.(check (option string))
        "custom metadata" (Some "custom-value") custom;
      match t.source with
      | Agent_template.User_file p ->
          Alcotest.(check string) "source path" "/tmp/test.md" p
      | _ -> Alcotest.fail "expected User_file source")

let test_parse_missing_name () =
  let md = "---\ndescription: test\n---\nBody\n" in
  match Agent_template.parse_template ~source_path:"" md with
  | Ok _ -> Alcotest.fail "should have failed"
  | Error e ->
      Alcotest.(check bool) "error mentions name" true (String.length e > 0)

let test_parse_missing_description () =
  let md = "---\nname: test\n---\nBody\n" in
  match Agent_template.parse_template ~source_path:"" md with
  | Ok _ -> Alcotest.fail "should have failed"
  | Error e ->
      Alcotest.(check bool)
        "error mentions description" true
        (String.length e > 0)

let test_parse_empty_body () =
  let md = "---\nname: test\ndescription: test agent\n---\n" in
  match Agent_template.parse_template ~source_path:"" md with
  | Error e -> Alcotest.fail e
  | Ok t -> Alcotest.(check string) "empty body" "" t.system_prompt

let test_parse_no_frontmatter () =
  let md = "Just a body with no frontmatter" in
  match Agent_template.parse_template ~source_path:"" md with
  | Ok _ -> Alcotest.fail "should fail without frontmatter"
  | Error _ -> ()

let test_role_parsing () =
  let check input expected =
    let role = Agent_template.role_of_string input in
    let result = Agent_template.role_to_string role in
    Alcotest.(check string) (Printf.sprintf "role %s" input) expected result
  in
  check "ceo" "ceo";
  check "team-lead" "team-lead";
  check "team_lead" "team-lead";
  check "coder" "coder";
  check "planner" "planner";
  check "reviewer" "reviewer";
  check "researcher" "researcher";
  check "tester" "tester";
  check "debugger" "debugger";
  check "refactorer" "refactorer";
  check "documenter" "documenter";
  check "ops" "ops";
  check "custom-role" "custom-role"

let test_builtin_resolution () =
  let expected_names =
    [
      "ceo";
      "team-lead";
      "reviewer";
      "researcher";
      "tester";
      "coder";
      "planner";
      "debugger";
      "refactorer";
      "documenter";
      "ops";
    ]
  in
  List.iter
    (fun name ->
      match Agent_template_builtins.find name with
      | None -> Alcotest.fail (Printf.sprintf "builtin %s not found" name)
      | Some t -> (
          Alcotest.(check string)
            (Printf.sprintf "builtin %s name" name)
            name t.name;
          match t.source with
          | Agent_template.Builtin -> ()
          | _ ->
              Alcotest.fail (Printf.sprintf "builtin %s has wrong source" name)))
    expected_names

let test_builtin_count () =
  Alcotest.(check int)
    "11 builtins" 11
    (List.length Agent_template_builtins.all)

let test_name_validation () =
  Alcotest.(check bool)
    "valid name" true
    (Agent_template.is_valid_name "my-agent");
  Alcotest.(check bool)
    "valid with underscore" true
    (Agent_template.is_valid_name "my_agent");
  Alcotest.(check bool)
    "valid with digits" true
    (Agent_template.is_valid_name "agent123");
  Alcotest.(check bool)
    "empty is invalid" false
    (Agent_template.is_valid_name "");
  Alcotest.(check bool)
    "spaces invalid" false
    (Agent_template.is_valid_name "my agent");
  Alcotest.(check bool)
    "uppercase invalid" false
    (Agent_template.is_valid_name "MyAgent");
  Alcotest.(check bool)
    "dots invalid" false
    (Agent_template.is_valid_name "my.agent")

let make_template ~name ~model : Agent_template.t =
  {
    name;
    description = "test";
    role = Agent_template.Coder;
    goal = "g";
    backstory = "b";
    system_prompt = "sp";
    model;
    max_tool_iterations = None;
    allowed_tools = [];
    disallowed_tools = [];
    tool_search_enabled = None;
    reasoning_effort = None;
    cwd = None;
    source = Agent_template.Builtin;
    metadata = [];
  }

let make_config ~primary ~subagent_default : Runtime_config.t =
  let d = Runtime_config.default in
  {
    d with
    agent_defaults =
      {
        d.agent_defaults with
        primary_model = primary;
        subagent_default_model = subagent_default;
      };
  }

let test_subagent_default_model_applies () =
  let config =
    make_config ~primary:"primary:m1"
      ~subagent_default:(Some "kimi_coding:kimi-for-code")
  in
  let tmpl = make_template ~name:"sub" ~model:None in
  let cfg2 =
    Agent.apply_subagent_default_model ~config ~agent_template:(Some tmpl)
  in
  Alcotest.(check string)
    "subagent model wins over primary" "kimi_coding:kimi-for-code"
    cfg2.agent_defaults.primary_model

let test_template_model_wins_over_default () =
  let config =
    make_config ~primary:"primary:m1"
      ~subagent_default:(Some "kimi_coding:kimi-for-code")
  in
  let tmpl = make_template ~name:"sub" ~model:(Some "openai-codex:gpt-5.4") in
  let cfg2 =
    Agent.apply_subagent_default_model ~config ~agent_template:(Some tmpl)
  in
  (* Template.model is honored at provider-call time; the function leaves
     config.primary_model alone when the template explicitly chose. *)
  Alcotest.(check string)
    "primary preserved when template.model set" "primary:m1"
    cfg2.agent_defaults.primary_model

let test_no_template_no_override () =
  let config =
    make_config ~primary:"primary:m1"
      ~subagent_default:(Some "kimi_coding:kimi-for-code")
  in
  let cfg2 = Agent.apply_subagent_default_model ~config ~agent_template:None in
  Alcotest.(check string)
    "no override when no template" "primary:m1"
    cfg2.agent_defaults.primary_model

let test_to_frontmatter_roundtrip () =
  match
    Agent_template.parse_template ~source_path:"/tmp/test.md" valid_template_md
  with
  | Error e -> Alcotest.fail e
  | Ok t -> (
      let serialized = Agent_template.to_frontmatter_string t in
      (* Re-parse the serialized output *)
      match
        Agent_template.parse_template ~source_path:"/tmp/test2.md" serialized
      with
      | Error e -> Alcotest.fail (Printf.sprintf "roundtrip failed: %s" e)
      | Ok t2 ->
          Alcotest.(check string) "name roundtrip" t.name t2.name;
          Alcotest.(check string)
            "description roundtrip" t.description t2.description;
          Alcotest.(check string)
            "role roundtrip"
            (Agent_template.role_to_string t.role)
            (Agent_template.role_to_string t2.role);
          Alcotest.(check string) "goal roundtrip" t.goal t2.goal;
          Alcotest.(check string) "backstory roundtrip" t.backstory t2.backstory;
          Alcotest.(check (option string)) "model roundtrip" t.model t2.model;
          Alcotest.(check (list string))
            "allowed_tools roundtrip" t.allowed_tools t2.allowed_tools;
          Alcotest.(check (list string))
            "disallowed_tools roundtrip" t.disallowed_tools t2.disallowed_tools)

let test_tool_restriction_allowed () =
  let registry = Tool_registry.create () in
  let mk name =
    {
      Tool.name;
      description = "test";
      parameters_schema = `Null;
      invoke = (fun ?context:_ _ -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  List.iter
    (Tool_registry.register registry)
    [ mk "file_read"; mk "file_write"; mk "shell_exec" ];
  let tmpl =
    {
      Agent_template.name = "test";
      description = "test";
      role = Coder;
      goal = "";
      backstory = "";
      system_prompt = "";
      model = None;
      max_tool_iterations = None;
      allowed_tools = [ "file_read" ];
      disallowed_tools = [];
      tool_search_enabled = None;
      reasoning_effort = None;
      cwd = None;
      source = Builtin;
      metadata = [];
    }
  in
  let filtered =
    Command_bridge_helpers.apply_agent_template_restrictions registry tmpl
  in
  let names =
    List.map (fun (t : Tool.t) -> t.name) (Tool_registry.list filtered)
  in
  Alcotest.(check (list string)) "only file_read" [ "file_read" ] names

let test_tool_restriction_disallowed () =
  let registry = Tool_registry.create () in
  let mk name =
    {
      Tool.name;
      description = "test";
      parameters_schema = `Null;
      invoke = (fun ?context:_ _ -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  List.iter
    (Tool_registry.register registry)
    [ mk "file_read"; mk "file_write"; mk "shell_exec" ];
  let tmpl =
    {
      Agent_template.name = "test";
      description = "test";
      role = Coder;
      goal = "";
      backstory = "";
      system_prompt = "";
      model = None;
      max_tool_iterations = None;
      allowed_tools = [];
      disallowed_tools = [ "file_write" ];
      tool_search_enabled = None;
      reasoning_effort = None;
      cwd = None;
      source = Builtin;
      metadata = [];
    }
  in
  let filtered =
    Command_bridge_helpers.apply_agent_template_restrictions registry tmpl
  in
  let names =
    List.map (fun (t : Tool.t) -> t.name) (Tool_registry.list filtered)
    |> List.sort String.compare
  in
  Alcotest.(check (list string))
    "no file_write"
    [ "file_read"; "shell_exec" ]
    names

let test_tool_restriction_empty_allows_all () =
  let registry = Tool_registry.create () in
  let mk name =
    {
      Tool.name;
      description = "test";
      parameters_schema = `Null;
      invoke = (fun ?context:_ _ -> Lwt.return "ok");
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  List.iter
    (Tool_registry.register registry)
    [ mk "file_read"; mk "file_write"; mk "shell_exec" ];
  let tmpl =
    {
      Agent_template.name = "test";
      description = "test";
      role = Coder;
      goal = "";
      backstory = "";
      system_prompt = "";
      model = None;
      max_tool_iterations = None;
      allowed_tools = [];
      disallowed_tools = [];
      tool_search_enabled = None;
      reasoning_effort = None;
      cwd = None;
      source = Builtin;
      metadata = [];
    }
  in
  let filtered =
    Command_bridge_helpers.apply_agent_template_restrictions registry tmpl
  in
  let count = List.length (Tool_registry.list filtered) in
  Alcotest.(check int) "all 3 tools" 3 count

let test_resolve_unknown () =
  Alcotest.(check bool)
    "unknown returns None" true
    (Option.is_none (Agent_template.resolve "nonexistent-agent-xyz"))

let test_parse_cwd_field () =
  let md_with_cwd =
    "---\n\
     name: cwd-agent\n\
     description: Agent with cwd\n\
     cwd: /tmp/project\n\
     ---\n\
     System prompt"
  in
  (match Agent_template.parse_template ~source_path:"" md_with_cwd with
  | Error e -> Alcotest.fail e
  | Ok t ->
      Alcotest.(check (option string)) "cwd present" (Some "/tmp/project") t.cwd);
  let md_without_cwd = "---\nname: no-cwd\ndescription: No cwd\n---\nBody" in
  match Agent_template.parse_template ~source_path:"" md_without_cwd with
  | Error e -> Alcotest.fail e
  | Ok t -> Alcotest.(check (option string)) "cwd absent" None t.cwd

let test_cwd_frontmatter_roundtrip () =
  let md =
    "---\n\
     name: cwd-rt\n\
     description: roundtrip cwd\n\
     cwd: /home/user/project\n\
     ---\n\
     Prompt"
  in
  match Agent_template.parse_template ~source_path:"" md with
  | Error e -> Alcotest.fail e
  | Ok t -> (
      let serialized = Agent_template.to_frontmatter_string t in
      match Agent_template.parse_template ~source_path:"" serialized with
      | Error e -> Alcotest.fail (Printf.sprintf "roundtrip failed: %s" e)
      | Ok t2 ->
          Alcotest.(check (option string))
            "cwd roundtrip" (Some "/home/user/project") t2.cwd)

let test_available_templates_includes_builtins () =
  (* Builtins are registered via module init in agent_template_builtins.ml *)
  let all = Agent_template.available_templates () in
  let has_ceo =
    List.exists (fun (t : Agent_template.t) -> t.name = "ceo") all
  in
  let has_coder =
    List.exists (fun (t : Agent_template.t) -> t.name = "coder") all
  in
  Alcotest.(check bool) "has ceo" true has_ceo;
  Alcotest.(check bool) "has coder" true has_coder

let suite =
  [
    Alcotest.test_case "parse valid template" `Quick test_parse_valid_template;
    Alcotest.test_case "parse missing name" `Quick test_parse_missing_name;
    Alcotest.test_case "parse missing description" `Quick
      test_parse_missing_description;
    Alcotest.test_case "parse empty body" `Quick test_parse_empty_body;
    Alcotest.test_case "parse no frontmatter" `Quick test_parse_no_frontmatter;
    Alcotest.test_case "role parsing" `Quick test_role_parsing;
    Alcotest.test_case "builtin resolution" `Quick test_builtin_resolution;
    Alcotest.test_case "builtin count" `Quick test_builtin_count;
    Alcotest.test_case "name validation" `Quick test_name_validation;
    Alcotest.test_case "frontmatter roundtrip" `Quick
      test_to_frontmatter_roundtrip;
    Alcotest.test_case "tool restriction allowed" `Quick
      test_tool_restriction_allowed;
    Alcotest.test_case "tool restriction disallowed" `Quick
      test_tool_restriction_disallowed;
    Alcotest.test_case "empty allows all tools" `Quick
      test_tool_restriction_empty_allows_all;
    Alcotest.test_case "resolve unknown" `Quick test_resolve_unknown;
    Alcotest.test_case "available includes builtins" `Quick
      test_available_templates_includes_builtins;
    Alcotest.test_case "parse cwd field" `Quick test_parse_cwd_field;
    Alcotest.test_case "cwd frontmatter roundtrip" `Quick
      test_cwd_frontmatter_roundtrip;
    Alcotest.test_case
      "subagent_default_model overrides primary when template.model=None" `Quick
      test_subagent_default_model_applies;
    Alcotest.test_case
      "subagent_default_model does not override explicit template.model" `Quick
      test_template_model_wins_over_default;
    Alcotest.test_case "subagent_default_model not applied without a template"
      `Quick test_no_template_no_override;
  ]

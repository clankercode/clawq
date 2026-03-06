(* Contract tests: verify module interfaces and JSON schema validity *)

(* ===== Channel.S contract ===== *)
(* Verify that channel modules implement the required interface.
   Channel.S requires: val name : string and val start : ... *)

(* Discord is in clawq_runtime_integrations - test its name field *)
let test_discord_has_name () =
  Alcotest.(check bool)
    "Discord.name is non-empty" true
    (String.length Discord.name > 0)

(* ===== Provider config contract ===== *)

let test_provider_config_has_required_fields () =
  (* Every provider_config must have api_key and optional base_url *)
  let cfg : Runtime_config.provider_config =
    {
      api_key = "sk-test";
      base_url = None;
      default_model = None;
      project_id = None;
      location = None;
      service_account_json = None;
    }
  in
  Alcotest.(check bool)
    "api_key field exists" true
    (String.length cfg.api_key >= 0);
  Alcotest.(check bool) "base_url is option" true (cfg.base_url = None)

let test_provider_config_base_url_some () =
  let cfg : Runtime_config.provider_config =
    {
      api_key = "sk-test";
      base_url = Some "https://api.example.com/v1";
      default_model = None;
      project_id = None;
      location = None;
      service_account_json = None;
    }
  in
  Alcotest.(check (option string))
    "base_url some" (Some "https://api.example.com/v1") cfg.base_url

(* ===== Tool JSON schema validity ===== *)

(* Helper to validate a tool schema *)
let validate_tool_schema (tool : Tool.t) =
  let open Yojson.Safe.Util in
  (* Has name *)
  Alcotest.(check bool)
    (Printf.sprintf "tool '%s' has name" tool.name)
    true
    (String.length tool.name > 0);
  (* Has description *)
  Alcotest.(check bool)
    (Printf.sprintf "tool '%s' has description" tool.name)
    true
    (String.length tool.description > 0);
  (* parameters_schema is an object *)
  match tool.parameters_schema with
  | `Assoc fields ->
      (* Has type=object *)
      let type_val =
        try List.assoc "type" fields |> to_string with Not_found -> ""
      in
      Alcotest.(check string)
        (Printf.sprintf "tool '%s' parameters type=object" tool.name)
        "object" type_val
  | _ ->
      Alcotest.fail
        (Printf.sprintf "tool '%s' parameters_schema is not an object" tool.name)

let test_shell_exec_schema () =
  let tool =
    Tools_builtin.shell_exec ~workspace:"/tmp" ~workspace_only:true
      ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = "/tmp" }
  in
  validate_tool_schema tool

let test_file_read_schema () =
  let tool =
    Tools_builtin.file_read ~workspace:"/tmp" ~workspace_only:true
      ~extra_allowed_paths:[]
  in
  validate_tool_schema tool

let test_file_write_schema () =
  let tool =
    Tools_builtin.file_write ~workspace:"/tmp" ~workspace_only:true
      ~extra_allowed_paths:[]
  in
  validate_tool_schema tool

let test_shell_exec_has_command_param () =
  let tool =
    Tools_builtin.shell_exec ~workspace:"/tmp" ~workspace_only:true
      ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = "/tmp" }
  in
  let open Yojson.Safe.Util in
  match tool.parameters_schema with
  | `Assoc fields ->
      let props =
        try List.assoc "properties" fields with Not_found -> `Null
      in
      let has_command =
        match props with `Assoc ps -> List.mem_assoc "command" ps | _ -> false
      in
      Alcotest.(check bool) "shell_exec has command param" true has_command
  | _ -> Alcotest.fail "schema not an object"

let test_file_read_has_path_param () =
  let tool =
    Tools_builtin.file_read ~workspace:"/tmp" ~workspace_only:true
      ~extra_allowed_paths:[]
  in
  let open Yojson.Safe.Util in
  match tool.parameters_schema with
  | `Assoc fields ->
      let props =
        try List.assoc "properties" fields with Not_found -> `Null
      in
      let has_path =
        match props with `Assoc ps -> List.mem_assoc "path" ps | _ -> false
      in
      Alcotest.(check bool) "file_read has path param" true has_path
  | _ -> Alcotest.fail "schema not an object"

let test_shell_exec_required_fields () =
  let tool =
    Tools_builtin.shell_exec ~workspace:"/tmp" ~workspace_only:true
      ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = "/tmp" }
  in
  let open Yojson.Safe.Util in
  match tool.parameters_schema with
  | `Assoc fields ->
      let required =
        try List.assoc "required" fields |> to_list |> List.map to_string
        with Not_found -> []
      in
      Alcotest.(check bool)
        "command is required" true
        (List.mem "command" required)
  | _ -> Alcotest.fail "schema not an object"

let test_tool_registry_register_and_find () =
  let registry = Tool_registry.create () in
  let tool =
    Tools_builtin.shell_exec ~workspace:"/tmp" ~workspace_only:true
      ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = "/tmp" }
  in
  Tool_registry.register registry tool;
  match Tool_registry.find registry "shell_exec" with
  | Some found ->
      Alcotest.(check string) "found by name" "shell_exec" found.name
  | None -> Alcotest.fail "tool not found in registry"

let test_tool_registry_list () =
  let registry = Tool_registry.create () in
  let t1 =
    Tools_builtin.shell_exec ~workspace:"/tmp" ~workspace_only:true
      ~allowed_commands:[] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = "/tmp" }
  in
  let t2 =
    Tools_builtin.file_read ~workspace:"/tmp" ~workspace_only:true
      ~extra_allowed_paths:[]
  in
  Tool_registry.register registry t1;
  Tool_registry.register registry t2;
  let tools = Tool_registry.list registry in
  Alcotest.(check int) "2 tools in registry" 2 (List.length tools)

let test_tool_registry_find_missing () =
  let registry = Tool_registry.create () in
  let result = Tool_registry.find registry "nonexistent" in
  Alcotest.(check bool) "missing tool returns None" true (result = None)

let test_tool_openai_json_format () =
  let registry = Tool_registry.create () in
  let tool =
    Tools_builtin.shell_exec ~workspace:"/tmp" ~workspace_only:true
      ~allowed_commands:[ "ls" ] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = "/tmp" }
  in
  Tool_registry.register registry tool;
  let json = Tool_registry.to_openai_json registry in
  let open Yojson.Safe.Util in
  let items = json |> to_list in
  Alcotest.(check int) "1 item" 1 (List.length items);
  let item = List.hd items in
  Alcotest.(check string)
    "type=function" "function"
    (item |> member "type" |> to_string);
  Alcotest.(check string)
    "name in function" "shell_exec"
    (item |> member "function" |> member "name" |> to_string)

let test_tool_risk_levels () =
  let high_tool =
    Tools_builtin.shell_exec ~workspace:"/tmp" ~workspace_only:true
      ~allowed_commands:[] ~extra_allowed_paths:[]
      ~sandbox:{ Sandbox.backend = Sandbox.None; workspace = "/tmp" }
  in
  Alcotest.(check bool)
    "shell_exec is High risk" true
    (high_tool.risk_level = Tool.High)

let test_message_to_json_user_role () =
  let msg = Provider.make_message ~role:"user" ~content:"hello" in
  let json = Provider.message_to_json msg in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "role=user" "user" (json |> member "role" |> to_string);
  Alcotest.(check string)
    "content" "hello"
    (json |> member "content" |> to_string)

let test_message_to_json_assistant_role () =
  let msg = Provider.make_message ~role:"assistant" ~content:"hi there" in
  let json = Provider.message_to_json msg in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "role=assistant" "assistant"
    (json |> member "role" |> to_string)

let test_message_to_json_tool_role () =
  let msg =
    Provider.make_tool_result ~tool_call_id:"tc-1" ~name:"bash" ~content:"ok"
  in
  let json = Provider.message_to_json msg in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "role=tool" "tool" (json |> member "role" |> to_string);
  Alcotest.(check string)
    "tool_call_id" "tc-1"
    (json |> member "tool_call_id" |> to_string)

let test_message_to_json_assistant_with_tool_calls () =
  let tc =
    { Provider.id = "call-abc"; function_name = "file_read"; arguments = "{}" }
  in
  let msg =
    {
      Provider.role = "assistant";
      content = "";
      tool_calls = [ tc ];
      tool_call_id = None;
      name = None;
    }
  in
  let json = Provider.message_to_json msg in
  let open Yojson.Safe.Util in
  let tcs = json |> member "tool_calls" |> to_list in
  Alcotest.(check int) "1 tool call in json" 1 (List.length tcs);
  let first_tc = List.hd tcs in
  Alcotest.(check string)
    "tool call id" "call-abc"
    (first_tc |> member "id" |> to_string);
  Alcotest.(check string)
    "function name" "file_read"
    (first_tc |> member "function" |> member "name" |> to_string)

let test_messages_to_json_list () =
  let msgs =
    [
      Provider.make_message ~role:"user" ~content:"hello";
      Provider.make_message ~role:"assistant" ~content:"hi";
    ]
  in
  let json = Provider.messages_to_json msgs in
  let open Yojson.Safe.Util in
  Alcotest.(check int) "2 messages" 2 (json |> to_list |> List.length)

let suite =
  [
    Alcotest.test_case "discord has name" `Quick test_discord_has_name;
    Alcotest.test_case "provider config required fields" `Quick
      test_provider_config_has_required_fields;
    Alcotest.test_case "provider config base_url some" `Quick
      test_provider_config_base_url_some;
    Alcotest.test_case "shell_exec schema valid" `Quick test_shell_exec_schema;
    Alcotest.test_case "file_read schema valid" `Quick test_file_read_schema;
    Alcotest.test_case "file_write schema valid" `Quick test_file_write_schema;
    Alcotest.test_case "shell_exec has command param" `Quick
      test_shell_exec_has_command_param;
    Alcotest.test_case "file_read has path param" `Quick
      test_file_read_has_path_param;
    Alcotest.test_case "shell_exec required fields" `Quick
      test_shell_exec_required_fields;
    Alcotest.test_case "tool registry register and find" `Quick
      test_tool_registry_register_and_find;
    Alcotest.test_case "tool registry list" `Quick test_tool_registry_list;
    Alcotest.test_case "tool registry find missing" `Quick
      test_tool_registry_find_missing;
    Alcotest.test_case "tool openai json format" `Quick
      test_tool_openai_json_format;
    Alcotest.test_case "tool risk levels" `Quick test_tool_risk_levels;
    Alcotest.test_case "message_to_json user role" `Quick
      test_message_to_json_user_role;
    Alcotest.test_case "message_to_json assistant role" `Quick
      test_message_to_json_assistant_role;
    Alcotest.test_case "message_to_json tool role" `Quick
      test_message_to_json_tool_role;
    Alcotest.test_case "message_to_json assistant with tool_calls" `Quick
      test_message_to_json_assistant_with_tool_calls;
    Alcotest.test_case "messages_to_json list" `Quick test_messages_to_json_list;
  ]

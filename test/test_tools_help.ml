let string_contains = Test_helpers.string_contains
let invoke args = Lwt_main.run (Tools_builtin_help.tool.Tool.invoke args)

let assert_contains label needle haystack =
  Alcotest.(check bool) label true (string_contains haystack needle)

let test_general_lists_topics_and_docs () =
  let result = invoke (`Assoc []) in
  assert_contains "general heading" "Clawq Help: general" result;
  assert_contains "quickstart url" "https://clawq.org/quickstart" result;
  assert_contains "llms full url" "https://clawq.org/llms-full.txt" result;
  assert_contains "tools topic" "tools" result;
  assert_contains "background topic" "background-tasks" result;
  assert_contains "onboarding topic" "onboarding" result

let test_general_topic_alias () =
  let result = invoke (`Assoc [ ("topic", `String "general") ]) in
  assert_contains "general alias heading" "Clawq Help: general" result

let test_known_topic_is_focused () =
  let result = invoke (`Assoc [ ("topic", `String "git_worktrees") ]) in
  assert_contains "normalized topic heading" "Clawq Help: git-worktrees" result;
  assert_contains "mentions worktree storage" "~/.clawq/background-worktrees/"
    result;
  assert_contains "background tasks doc" "https://clawq.org/background-tasks"
    result

let test_unknown_topic_is_actionable () =
  let result = invoke (`Assoc [ ("topic", `String "bananas") ]) in
  assert_contains "unknown error" "Error: unknown clawq_help topic" result;
  assert_contains "available topics" "Available topics:" result;
  assert_contains "example topic" "{\"topic\":\"general\"}" result

let test_non_string_topic_is_actionable () =
  let result = invoke (`Assoc [ ("topic", `Int 42) ]) in
  assert_contains "type error" "parameter \"topic\" must be a string" result;
  assert_contains "string example" "{\"topic\":\"tools\"}" result

let trim_url_token token =
  let rec trim_end i =
    if i < 0 then ""
    else
      match token.[i] with
      | '.' | ',' | ';' | ')' -> trim_end (i - 1)
      | _ -> String.sub token 0 (i + 1)
  in
  trim_end (String.length token - 1)

let urls_in text =
  text
  |> String.map (function '\n' | '\t' -> ' ' | c -> c)
  |> String.split_on_char ' ' |> List.map trim_url_token
  |> List.filter (fun token ->
      let prefix = "https://clawq.org/" in
      let token_len = String.length token in
      let prefix_len = String.length prefix in
      token_len >= prefix_len && String.sub token 0 prefix_len = prefix)

let test_help_urls_are_published () =
  let published_urls =
    [
      "https://clawq.org/background-tasks";
      "https://clawq.org/channels";
      "https://clawq.org/cli-reference";
      "https://clawq.org/configuration";
      "https://clawq.org/llms-full.txt";
      "https://clawq.org/quickstart";
      "https://clawq.org/security";
      "https://clawq.org/skills";
      "https://clawq.org/tools";
    ]
  in
  let topics = Tools_builtin_help.topic_names in
  List.iter
    (fun topic ->
      let result = invoke (`Assoc [ ("topic", `String topic) ]) in
      List.iter
        (fun url ->
          Alcotest.(check bool)
            (Printf.sprintf "%s emits published URL %s" topic url)
            true
            (List.mem url published_urls))
        (urls_in result))
    topics

let test_registered_with_optional_topic_schema () =
  let registry = Tool_registry.create () in
  let config = Runtime_config.default in
  let sandbox =
    Sandbox.create ~backend:Sandbox.None
      ~workspace:(Runtime_config.effective_workspace config)
      ~extra_allowed_paths:[] ~workspace_only:false ()
  in
  Tools_builtin.register_all ~config ~sandbox registry;
  match Tool_registry.find registry "clawq_help" with
  | None -> Alcotest.fail "clawq_help was not registered"
  | Some tool ->
      let open Yojson.Safe.Util in
      Alcotest.(check string) "name" "clawq_help" tool.Tool.name;
      Alcotest.(check (list string))
        "required" []
        (tool.parameters_schema |> member "required" |> to_list
       |> List.map to_string);
      let topic =
        tool.parameters_schema |> member "properties" |> member "topic"
      in
      Alcotest.(check string)
        "topic type" "string"
        (topic |> member "type" |> to_string);
      let enum_values =
        topic |> member "enum" |> to_list |> List.map to_string
      in
      Alcotest.(check bool)
        "enum contains general" true
        (List.mem "general" enum_values);
      Alcotest.(check bool)
        "enum contains onboarding" true
        (List.mem "onboarding" enum_values)

let suite =
  [
    Alcotest.test_case "general lists topics and docs" `Quick
      test_general_lists_topics_and_docs;
    Alcotest.test_case "general topic alias" `Quick test_general_topic_alias;
    Alcotest.test_case "known topic is focused" `Quick
      test_known_topic_is_focused;
    Alcotest.test_case "unknown topic is actionable" `Quick
      test_unknown_topic_is_actionable;
    Alcotest.test_case "non-string topic is actionable" `Quick
      test_non_string_topic_is_actionable;
    Alcotest.test_case "help URLs are published docs paths" `Quick
      test_help_urls_are_published;
    Alcotest.test_case "registered with optional topic schema" `Quick
      test_registered_with_optional_topic_schema;
  ]

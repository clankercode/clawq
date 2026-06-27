let check_contains label haystack needle =
  Alcotest.(check bool)
    label true
    (Test_helpers.string_contains haystack needle)

let assert_denial_contains label denial ~grant_type ~required_permission =
  check_contains (label ^ " grant type") denial ("grant type: " ^ grant_type);
  check_contains
    (label ^ " required permission")
    denial
    ("required permission: " ^ required_permission)

let expect_some label = function
  | Some value -> value
  | None -> Alcotest.fail ("expected denial for " ^ label)

let test_tool_denial_message_includes_grant_metadata () =
  let denial =
    Profile_policy.tool_denial ~profile_id:"profile-a" ~tool_name:"tool_b"
      ~allowed_tools:[ "tool_a" ] ~denied_tools:[]
    |> expect_some "tool grant"
  in
  assert_denial_contains "tool denial" denial ~grant_type:"tool"
    ~required_permission:"invoke:tool_b";
  check_contains "tool denial explains allowlist" denial "allowed_tools"

let test_codebase_denial_message_includes_grant_metadata () =
  let denial =
    Profile_policy.codebase_denial ~profile_id:"profile-a" ~path:"/tmp/outside"
      ~configured_grants:[ "/repo/src/**" ] ~granted:false
    |> expect_some "codebase grant"
  in
  assert_denial_contains "codebase denial" denial ~grant_type:"codebase"
    ~required_permission:"access:/tmp/outside";
  check_contains "codebase denial explains grants" denial "codebase_grants"

let test_memory_scope_denial_message_includes_grant_metadata () =
  let denial =
    Profile_policy.memory_scope_denial ~profile_id:"profile-a" ~scope_id:42
      ~capability:"write" ~granted_capabilities:[ "read" ]
    |> expect_some "memory scope grant"
  in
  assert_denial_contains "memory scope denial" denial ~grant_type:"memory_scope"
    ~required_permission:"memory_scope:42:write";
  check_contains "memory denial explains capability" denial "write"

let test_combined_multi_grant_denials_are_collected () =
  let denials =
    Profile_policy.denials ~profile_id:"profile-a"
      [
        Profile_policy.requirement ~grant_type:"tool"
          ~required_permission:"invoke:shell_exec" ~granted:false
          ~reason:"shell_exec is listed in denied_tools";
        Profile_policy.requirement ~grant_type:"codebase"
          ~required_permission:"access:/tmp/outside" ~granted:false
          ~reason:"path is outside room_profile_codebase_grants";
        Profile_policy.requirement ~grant_type:"memory_scope"
          ~required_permission:"memory_scope:42:write" ~granted:false
          ~reason:"capability write is not granted";
      ]
  in
  Alcotest.(check int) "three denials" 3 (List.length denials);
  List.iter2
    (fun denial (grant_type, required_permission) ->
      assert_denial_contains "combined denial" denial ~grant_type
        ~required_permission)
    denials
    [
      ("tool", "invoke:shell_exec");
      ("codebase", "access:/tmp/outside");
      ("memory_scope", "memory_scope:42:write");
    ]

let suite =
  [
    Alcotest.test_case "tool denial includes grant metadata" `Quick
      test_tool_denial_message_includes_grant_metadata;
    Alcotest.test_case "codebase denial includes grant metadata" `Quick
      test_codebase_denial_message_includes_grant_metadata;
    Alcotest.test_case "memory scope denial includes grant metadata" `Quick
      test_memory_scope_denial_message_includes_grant_metadata;
    Alcotest.test_case "combined multi-grant denials" `Quick
      test_combined_multi_grant_denials_are_collected;
  ]

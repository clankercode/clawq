let test_default_no_bindings () =
  let result =
    Agent_router.resolve ~bindings:[] ~channel_id:"general" ~sender_id:"u1"
      ~guild_id:None
  in
  Alcotest.(check string) "no bindings -> default" "default" result

let test_user_exact_match () =
  let bindings =
    [
      {
        Agent_router.pattern = "user:u42";
        agent_name = "personal";
        priority = 20;
      };
      { pattern = "channel:general"; agent_name = "team"; priority = 10 };
    ]
  in
  let result =
    Agent_router.resolve ~bindings ~channel_id:"general" ~sender_id:"u42"
      ~guild_id:None
  in
  Alcotest.(check string) "user match" "personal" result

let test_channel_match () =
  let bindings =
    [
      {
        Agent_router.pattern = "channel:support";
        agent_name = "helper";
        priority = 10;
      };
      { pattern = "default"; agent_name = "fallback"; priority = 0 };
    ]
  in
  let result =
    Agent_router.resolve ~bindings ~channel_id:"support" ~sender_id:"u99"
      ~guild_id:None
  in
  Alcotest.(check string) "channel match" "helper" result

let test_priority_user_over_channel () =
  let bindings =
    [
      {
        Agent_router.pattern = "channel:general";
        agent_name = "team";
        priority = 10;
      };
      { pattern = "user:u42"; agent_name = "personal"; priority = 20 };
    ]
  in
  let result =
    Agent_router.resolve ~bindings ~channel_id:"general" ~sender_id:"u42"
      ~guild_id:None
  in
  Alcotest.(check string) "user wins over channel" "personal" result

let test_guild_match () =
  let bindings =
    [
      {
        Agent_router.pattern = "guild:g100";
        agent_name = "guild_agent";
        priority = 5;
      };
      { pattern = "default"; agent_name = "fallback"; priority = 0 };
    ]
  in
  let result =
    Agent_router.resolve ~bindings ~channel_id:"ch1" ~sender_id:"u1"
      ~guild_id:(Some "g100")
  in
  Alcotest.(check string) "guild match" "guild_agent" result

let test_default_binding () =
  let bindings =
    [
      {
        Agent_router.pattern = "user:other";
        agent_name = "other_agent";
        priority = 10;
      };
      { pattern = "default"; agent_name = "fallback"; priority = 0 };
    ]
  in
  let result =
    Agent_router.resolve ~bindings ~channel_id:"ch1" ~sender_id:"u1"
      ~guild_id:None
  in
  Alcotest.(check string) "default binding" "fallback" result

let suite =
  [
    Alcotest.test_case "default no bindings" `Quick test_default_no_bindings;
    Alcotest.test_case "user exact match" `Quick test_user_exact_match;
    Alcotest.test_case "channel match" `Quick test_channel_match;
    Alcotest.test_case "priority user over channel" `Quick
      test_priority_user_over_channel;
    Alcotest.test_case "guild match" `Quick test_guild_match;
    Alcotest.test_case "default binding" `Quick test_default_binding;
  ]

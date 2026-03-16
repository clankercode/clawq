let test_should_respond_non_group () =
  Alcotest.(check bool)
    "non-group always responds" true
    (Group_chat_filter.should_respond ~is_group:false ~bot_mentioned:false
       ~is_reply_to_bot:false ~bot_name:"clawq" "random message")

let test_should_respond_bot_mentioned () =
  Alcotest.(check bool)
    "group with mention responds" true
    (Group_chat_filter.should_respond ~is_group:true ~bot_mentioned:true
       ~is_reply_to_bot:false ~bot_name:"clawq" "random message")

let test_should_respond_slash_command () =
  Alcotest.(check bool)
    "group with slash command responds" true
    (Group_chat_filter.should_respond ~is_group:true ~bot_mentioned:false
       ~is_reply_to_bot:false ~bot_name:"clawq" "/help")

let test_should_respond_addressed_by_name () =
  Alcotest.(check bool)
    "group addressed by name responds" true
    (Group_chat_filter.should_respond ~is_group:true ~bot_mentioned:false
       ~is_reply_to_bot:false ~bot_name:"clawq" "clawq, do something")

let test_should_respond_addressed_case_insensitive () =
  Alcotest.(check bool)
    "group addressed case-insensitive responds" true
    (Group_chat_filter.should_respond ~is_group:true ~bot_mentioned:false
       ~is_reply_to_bot:false ~bot_name:"clawq" "CLAWQ do something")

let test_should_respond_reply_to_bot () =
  Alcotest.(check bool)
    "group reply to bot responds" true
    (Group_chat_filter.should_respond ~is_group:true ~bot_mentioned:false
       ~is_reply_to_bot:true ~bot_name:"clawq" "follow up question")

let test_should_not_respond_unaddressed_group () =
  Alcotest.(check bool)
    "unaddressed group message ignored" false
    (Group_chat_filter.should_respond ~is_group:true ~bot_mentioned:false
       ~is_reply_to_bot:false ~bot_name:"clawq" "hey everyone whats up")

let test_strip_bot_name_prefix () =
  Alcotest.(check string)
    "strip clawq, prefix" "do something"
    (Group_chat_filter.strip_bot_name_prefix ~bot_name:"clawq"
       "clawq, do something")

let test_strip_bot_name_prefix_colon () =
  Alcotest.(check string)
    "strip clawq: prefix" "do something"
    (Group_chat_filter.strip_bot_name_prefix ~bot_name:"clawq"
       "clawq: do something")

let test_strip_bot_name_prefix_space () =
  Alcotest.(check string)
    "strip clawq space prefix" "do something"
    (Group_chat_filter.strip_bot_name_prefix ~bot_name:"clawq"
       "clawq do something")

let test_strip_bot_name_prefix_no_match () =
  Alcotest.(check string)
    "no match unchanged" "hello world"
    (Group_chat_filter.strip_bot_name_prefix ~bot_name:"clawq" "hello world")

let test_is_no_reply () =
  Alcotest.(check bool)
    "exact NO_REPLY" true
    (Group_chat_filter.is_no_reply "[NO_REPLY]");
  Alcotest.(check bool)
    "NO_REPLY with whitespace" true
    (Group_chat_filter.is_no_reply "  [NO_REPLY]  ");
  Alcotest.(check bool)
    "not NO_REPLY" false
    (Group_chat_filter.is_no_reply "hello");
  Alcotest.(check bool)
    "partial NO_REPLY" false
    (Group_chat_filter.is_no_reply "[NO_REPLY] extra text")

let test_is_slash_command () =
  Alcotest.(check bool)
    "/help is command" true
    (Group_chat_filter.is_slash_command "/help");
  Alcotest.(check bool)
    "no slash" false
    (Group_chat_filter.is_slash_command "hello");
  Alcotest.(check bool)
    "whitespace then slash" true
    (Group_chat_filter.is_slash_command "  /status")

let test_is_addressed_by_name () =
  Alcotest.(check bool)
    "clawq, hello" true
    (Group_chat_filter.is_addressed_by_name ~bot_name:"clawq" "clawq, hello");
  Alcotest.(check bool)
    "CLAWQ: hello" true
    (Group_chat_filter.is_addressed_by_name ~bot_name:"clawq" "CLAWQ: hello");
  Alcotest.(check bool)
    "clawq hello" true
    (Group_chat_filter.is_addressed_by_name ~bot_name:"clawq" "clawq hello");
  Alcotest.(check bool)
    "clawqx no" false
    (Group_chat_filter.is_addressed_by_name ~bot_name:"clawq" "clawqx hello");
  Alcotest.(check bool)
    "other bot no" false
    (Group_chat_filter.is_addressed_by_name ~bot_name:"clawq" "otherbot, hi")

let suite =
  [
    Alcotest.test_case "non-group always responds" `Quick
      test_should_respond_non_group;
    Alcotest.test_case "group with bot mention responds" `Quick
      test_should_respond_bot_mentioned;
    Alcotest.test_case "group with slash command responds" `Quick
      test_should_respond_slash_command;
    Alcotest.test_case "group addressed by name responds" `Quick
      test_should_respond_addressed_by_name;
    Alcotest.test_case "group addressed case-insensitive responds" `Quick
      test_should_respond_addressed_case_insensitive;
    Alcotest.test_case "group reply to bot responds" `Quick
      test_should_respond_reply_to_bot;
    Alcotest.test_case "unaddressed group message ignored" `Quick
      test_should_not_respond_unaddressed_group;
    Alcotest.test_case "strip bot name prefix" `Quick test_strip_bot_name_prefix;
    Alcotest.test_case "strip bot name prefix colon" `Quick
      test_strip_bot_name_prefix_colon;
    Alcotest.test_case "strip bot name prefix space" `Quick
      test_strip_bot_name_prefix_space;
    Alcotest.test_case "strip bot name prefix no match" `Quick
      test_strip_bot_name_prefix_no_match;
    Alcotest.test_case "is_no_reply detection" `Quick test_is_no_reply;
    Alcotest.test_case "is_slash_command detection" `Quick test_is_slash_command;
    Alcotest.test_case "is_addressed_by_name detection" `Quick
      test_is_addressed_by_name;
  ]

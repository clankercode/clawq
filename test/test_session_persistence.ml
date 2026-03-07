let make_db () = Memory.init ~db_path:":memory:" ()

let test_discord_resume_state_round_trip () =
  let db = make_db () in
  Discord.save_resume_state ~db:(Some db) ~session_id:"sess-1" ~seq:42
    ~resume_gateway_url:"wss://resume.example.com";
  match Discord.load_resume_state ~db:(Some db) with
  | None -> Alcotest.fail "expected persisted resume state"
  | Some state ->
      Alcotest.(check string) "session id" "sess-1" state.session_id;
      Alcotest.(check int) "seq" 42 state.seq;
      Alcotest.(check string)
        "resume url" "wss://resume.example.com" state.resume_gateway_url

let test_discord_clear_resume_state_removes_persisted_row () =
  let db = make_db () in
  Discord.save_resume_state ~db:(Some db) ~session_id:"sess-1" ~seq:42
    ~resume_gateway_url:"wss://resume.example.com";
  Discord.clear_resume_state ~db:(Some db);
  Alcotest.(check bool)
    "resume state cleared" true
    (Discord.load_resume_state ~db:(Some db) = None)

let test_discord_startup_restore_builds_resume_refs () =
  let db = make_db () in
  Discord.save_resume_state ~db:(Some db) ~session_id:"sess-1" ~seq:42
    ~resume_gateway_url:"wss://resume.example.com";
  let resume_session_id, resume_seq, resume_url =
    Discord.make_resume_refs ~db:(Some db)
  in
  Alcotest.(check (option string))
    "session restored" (Some "sess-1") !resume_session_id;
  Alcotest.(check (option int)) "seq restored" (Some 42) !resume_seq;
  Alcotest.(check (option string))
    "resume url restored" (Some "wss://resume.example.com") !resume_url

let test_discord_startup_without_state_keeps_identify_path () =
  let db = make_db () in
  let resume_session_id, resume_seq, resume_url =
    Discord.make_resume_refs ~db:(Some db)
  in
  Alcotest.(check (option string)) "no session restored" None !resume_session_id;
  Alcotest.(check (option int)) "no seq restored" None !resume_seq;
  Alcotest.(check (option string)) "no resume url restored" None !resume_url

let test_discord_fatal_close_codes_clear_resume_state () =
  Alcotest.(check bool)
    "4004 clears resume state" true
    (Discord.should_clear_resume_state 4004);
  Alcotest.(check bool)
    "4010 clears resume state" true
    (Discord.should_clear_resume_state 4010);
  Alcotest.(check bool)
    "4014 clears resume state" true
    (Discord.should_clear_resume_state 4014)

let test_discord_resume_failures_clear_state_for_identify () =
  Alcotest.(check bool)
    "4007 clears invalid seq state" true
    (Discord.should_clear_resume_state 4007);
  Alcotest.(check bool)
    "4009 clears timed out session state" true
    (Discord.should_clear_resume_state 4009);
  Alcotest.(check bool)
    "4007 is not globally fatal" false
    (Discord.is_fatal_close_code 4007);
  Alcotest.(check bool)
    "4009 is not globally fatal" false
    (Discord.is_fatal_close_code 4009)

let suite : unit Alcotest.test_case list =
  [
    Alcotest.test_case "discord resume state round trip" `Quick
      test_discord_resume_state_round_trip;
    Alcotest.test_case "discord clear resume state removes persisted row" `Quick
      test_discord_clear_resume_state_removes_persisted_row;
    Alcotest.test_case "discord startup restore builds resume refs" `Quick
      test_discord_startup_restore_builds_resume_refs;
    Alcotest.test_case "discord startup without state keeps identify path"
      `Quick test_discord_startup_without_state_keeps_identify_path;
    Alcotest.test_case "discord fatal close codes clear resume state" `Quick
      test_discord_fatal_close_codes_clear_resume_state;
    Alcotest.test_case "discord resume failures clear state for identify" `Quick
      test_discord_resume_failures_clear_state_for_identify;
  ]

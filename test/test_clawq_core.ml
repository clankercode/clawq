let test_parse_agent () =
  Alcotest.(check string)
    "parse agent" "CmdAgent"
    (match Clawq_core.parse_command "agent" with
    | Clawq_core.CmdAgent -> "CmdAgent"
    | _ -> "other")

let test_parse_unknown () =
  Alcotest.(check string)
    "parse garbage" "CmdUnknown"
    (match Clawq_core.parse_command "garbage" with
    | Clawq_core.CmdUnknown -> "CmdUnknown"
    | _ -> "other")

let test_dispatch_empty () =
  let result = Clawq_core.dispatch [] in
  Alcotest.(check bool)
    "dispatch [] contains Usage" true
    (String.length result > 0 && String.sub result 0 5 = "Usage")

let test_dispatch_version () =
  let result = Clawq_core.dispatch [ "version" ] in
  Alcotest.(check bool)
    "version starts with clawq" true
    (String.length result >= 5 && String.sub result 0 5 = "clawq")

let test_dispatch_help () =
  let result = Clawq_core.dispatch [ "help" ] in
  Alcotest.(check bool)
    "dispatch help contains Usage" true
    (String.length result > 0 && String.sub result 0 5 = "Usage")

let test_dispatch_cron_mentions_runtime_bridge () =
  let result = Clawq_core.dispatch [ "cron" ] in
  let contains s sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "dispatch cron mentions scheduler-backed command" true
    (contains result "scheduler-backed command"
    && contains result "list/add/remove/history/runs")

let test_audit_make_entry_with_metadata () =
  let entry =
    Clawq_core.make_entry "secret" None "2030-01-01 00:00:00" "tool_result"
      (Some "sess-1") "success" (Some "shell_exec") (Some "high")
  in
  Alcotest.(check string) "prev hash" "genesis" entry.ae_prev_hash;
  Alcotest.(check (option string))
    "session key" (Some "sess-1") entry.ae_session_key;
  Alcotest.(check (option string))
    "tool name" (Some "shell_exec") entry.ae_tool_name;
  Alcotest.(check (option string))
    "risk level" (Some "high") entry.ae_risk_level

let test_audit_verify_chain_detects_metadata_tamper () =
  let entry1 =
    Clawq_core.make_entry "secret" None "2030-01-01 00:00:00" "tool_result"
      (Some "sess-1") "success" (Some "shell_exec") (Some "high")
  in
  let entry2 =
    Clawq_core.make_entry "secret" (Some entry1.ae_signature)
      "2030-01-01 00:00:01" "daemon_event" None "rotation" None None
  in
  let tampered = { entry1 with ae_tool_name = Some "file_read" } in
  Alcotest.(check bool)
    "original chain verifies" true
    (Clawq_core.verify_chain "secret" None [ entry1; entry2 ]);
  Alcotest.(check bool)
    "tampered metadata fails" false
    (Clawq_core.verify_chain "secret" None [ tampered; entry2 ])

let suite =
  [
    Alcotest.test_case "parse_command agent" `Quick test_parse_agent;
    Alcotest.test_case "parse_command unknown" `Quick test_parse_unknown;
    Alcotest.test_case "dispatch empty" `Quick test_dispatch_empty;
    Alcotest.test_case "dispatch version" `Quick test_dispatch_version;
    Alcotest.test_case "dispatch help" `Quick test_dispatch_help;
    Alcotest.test_case "dispatch cron mentions runtime bridge" `Quick
      test_dispatch_cron_mentions_runtime_bridge;
    Alcotest.test_case "audit make_entry with metadata" `Quick
      test_audit_make_entry_with_metadata;
    Alcotest.test_case "audit verify_chain detects metadata tamper" `Quick
      test_audit_verify_chain_detects_metadata_tamper;
  ]

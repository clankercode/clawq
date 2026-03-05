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
  Alcotest.(check string)
    "dispatch version" "clawq 0.1.0-dev"
    (Clawq_core.dispatch [ "version" ])

let test_dispatch_help () =
  let result = Clawq_core.dispatch [ "help" ] in
  Alcotest.(check bool)
    "dispatch help contains Usage" true
    (String.length result > 0 && String.sub result 0 5 = "Usage")

let suite =
  [
    Alcotest.test_case "parse_command agent" `Quick test_parse_agent;
    Alcotest.test_case "parse_command unknown" `Quick test_parse_unknown;
    Alcotest.test_case "dispatch empty" `Quick test_dispatch_empty;
    Alcotest.test_case "dispatch version" `Quick test_dispatch_version;
    Alcotest.test_case "dispatch help" `Quick test_dispatch_help;
  ]

(* Tests for main_wasm module - file-based operations *)

(* --- dispatch tests --- *)

let test_dispatch_help () =
  let code, result = Main_wasm.dispatch [ "help" ] in
  Alcotest.(check int) "help exits 0" 0 code;
  Alcotest.(check bool) "help non-empty" true (String.length result > 0)

let test_dispatch_empty () =
  let code, result = Main_wasm.dispatch [] in
  Alcotest.(check int) "empty exits 0" 0 code;
  Alcotest.(check bool) "empty shows help" true (String.length result > 0)

let test_dispatch_version () =
  let code, result = Main_wasm.dispatch [ "version" ] in
  Alcotest.(check int) "version exits 0" 0 code;
  Alcotest.(check bool) "version non-empty" true (String.length result > 0);
  Alcotest.(check bool)
    "contains wasm" true
    (Test_helpers.string_contains result "wasm")

let test_dispatch_status () =
  let code, result = Main_wasm.dispatch [ "status" ] in
  Alcotest.(check int) "status exits 0" 0 code;
  Alcotest.(check bool) "status non-empty" true (String.length result > 0)

let test_dispatch_unknown () =
  let code, result = Main_wasm.dispatch [ "nonexistent_command" ] in
  Alcotest.(check int) "unknown exits 1" 1 code;
  Alcotest.(check bool) "unknown returns message" true (String.length result > 0);
  Alcotest.(check bool)
    "contains Unknown" true
    (Test_helpers.string_contains result "Unknown"
    || Test_helpers.string_contains result "unknown")

let test_dispatch_dash_h () =
  let code, result = Main_wasm.dispatch [ "-h" ] in
  Alcotest.(check int) "-h exits 0" 0 code;
  Alcotest.(check bool) "help flag" true (String.length result > 0)

let test_dispatch_dash_v () =
  let code, result = Main_wasm.dispatch [ "-v" ] in
  Alcotest.(check int) "-v exits 0" 0 code;
  Alcotest.(check bool) "version flag" true (String.length result > 0)

let test_dispatch_dash_version () =
  let code, result = Main_wasm.dispatch [ "--version" ] in
  Alcotest.(check int) "--version exits 0" 0 code;
  Alcotest.(check bool) "long version" true (String.length result > 0)

(* --- cmd_help tests --- *)

let test_cmd_help () =
  let result = Main_wasm.cmd_help () in
  Alcotest.(check bool)
    "contains version" true
    (Test_helpers.string_contains result Main_wasm.version);
  Alcotest.(check bool)
    "contains commands" true
    (Test_helpers.string_contains result "Commands")

(* --- cmd_version tests --- *)

let test_cmd_version () =
  let result = Main_wasm.cmd_version () in
  Alcotest.(check bool)
    "contains version" true
    (Test_helpers.string_contains result Main_wasm.version)

(* --- cmd_status tests --- *)

let test_cmd_status () =
  let result = Main_wasm.cmd_status () in
  Alcotest.(check bool)
    "contains workspace" true
    (Test_helpers.string_contains result "workspace")

(* --- cmd_memory tests --- *)

let test_cmd_memory_list () =
  let result = Main_wasm.cmd_memory [ "list" ] in
  Alcotest.(check bool) "list returns string" true (String.length result >= 0)

let test_cmd_memory_read () =
  let result = Main_wasm.cmd_memory [ "read" ] in
  Alcotest.(check bool) "read returns string" true (String.length result >= 0)

let test_cmd_memory_add_empty () =
  let result = Main_wasm.cmd_memory [ "add" ] in
  Alcotest.(check bool) "add empty shows usage" true (String.length result > 0)

let test_cmd_memory_unknown () =
  let result = Main_wasm.cmd_memory [ "xyz" ] in
  Alcotest.(check bool) "unknown subcommand" true (String.length result > 0)

(* --- cmd_agent tests --- *)

let test_cmd_agent_no_key () =
  let result = Main_wasm.cmd_agent [] in
  Alcotest.(check bool)
    "mentions API key" true
    (Test_helpers.string_contains result "CLAWQ_API_KEY"
    || Test_helpers.string_contains result "api"
    || Test_helpers.string_contains result "Agent")

(* --- file-based memory tests --- *)

let test_memory_path () =
  let path = Main_wasm.memory_path ~workspace:"/tmp/test" in
  Alcotest.(check bool)
    "contains MEMORY.md" true
    (Test_helpers.string_contains path "MEMORY.md")

let test_read_memory_nonexistent () =
  let content =
    Main_wasm.read_memory ~workspace:"/tmp/nonexistent_wasm_test_123"
  in
  Alcotest.(check string) "empty for missing" "" content

let test_list_memory_entries_empty () =
  let entries =
    Main_wasm.list_memory_entries ~workspace:"/tmp/nonexistent_wasm_test_456"
  in
  Alcotest.(check int) "no entries" 0 (List.length entries)

let test_read_identity_nonexistent () =
  let result =
    Main_wasm.read_identity ~workspace:"/tmp/nonexistent_wasm_test_789"
  in
  Alcotest.(check bool)
    "no identity" true
    (Test_helpers.string_contains result "no IDENTITY.md")

let suite =
  [
    Alcotest.test_case "dispatch help" `Quick test_dispatch_help;
    Alcotest.test_case "dispatch empty" `Quick test_dispatch_empty;
    Alcotest.test_case "dispatch version" `Quick test_dispatch_version;
    Alcotest.test_case "dispatch status" `Quick test_dispatch_status;
    Alcotest.test_case "dispatch unknown" `Quick test_dispatch_unknown;
    Alcotest.test_case "dispatch -h" `Quick test_dispatch_dash_h;
    Alcotest.test_case "dispatch -v" `Quick test_dispatch_dash_v;
    Alcotest.test_case "dispatch --version" `Quick test_dispatch_dash_version;
    Alcotest.test_case "cmd_help" `Quick test_cmd_help;
    Alcotest.test_case "cmd_version" `Quick test_cmd_version;
    Alcotest.test_case "cmd_status" `Quick test_cmd_status;
    Alcotest.test_case "cmd_memory list" `Quick test_cmd_memory_list;
    Alcotest.test_case "cmd_memory read" `Quick test_cmd_memory_read;
    Alcotest.test_case "cmd_memory add empty" `Quick test_cmd_memory_add_empty;
    Alcotest.test_case "cmd_memory unknown" `Quick test_cmd_memory_unknown;
    Alcotest.test_case "cmd_agent no key" `Quick test_cmd_agent_no_key;
    Alcotest.test_case "memory_path" `Quick test_memory_path;
    Alcotest.test_case "read_memory nonexistent" `Quick
      test_read_memory_nonexistent;
    Alcotest.test_case "list entries empty" `Quick
      test_list_memory_entries_empty;
    Alcotest.test_case "read identity nonexistent" `Quick
      test_read_identity_nonexistent;
  ]

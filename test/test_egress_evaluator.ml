(* Tests for egress policy evaluator *)

open Runtime_config

let check_action msg expected (result : Egress_evaluator.result) =
  Alcotest.check
    (Alcotest.of_pp (fun fmt -> function
      | Allow -> Format.fprintf fmt "Allow"
      | Deny -> Format.fprintf fmt "Deny"))
    msg expected result.action

let check_log_policy msg expected (result : Egress_evaluator.result) =
  Alcotest.check
    (Alcotest.of_pp (fun fmt -> function
      | Log -> Format.fprintf fmt "Log"
      | No_log -> Format.fprintf fmt "No_log"))
    msg expected result.log_policy

let check_index msg expected (result : Egress_evaluator.result) =
  Alcotest.check Alcotest.int msg expected result.matched_rule_index

(** Test: empty rule set defaults to deny *)
let test_empty_rules_default_deny () =
  let result = Egress_evaluator.evaluate ~rules:[] ~host:"example.com" () in
  check_action "empty rules should deny" Deny result;
  check_log_policy "default deny logs" Log result;
  check_index "default index is -1" (-1) result

(** Test: single allow rule matches *)
let test_single_allow_rule () =
  let rules =
    [
      {
        host = "api.example.com";
        path = None;
        method_ = None;
        action = Allow;
        log_policy = Log;
      };
    ]
  in
  let result = Egress_evaluator.evaluate ~rules ~host:"api.example.com" () in
  check_action "exact host match allows" Allow result;
  check_index "matched at index 0" 0 result

(** Test: single deny rule matches *)
let test_single_deny_rule () =
  let rules =
    [
      {
        host = "blocked.example.com";
        path = None;
        method_ = None;
        action = Deny;
        log_policy = No_log;
      };
    ]
  in
  let result =
    Egress_evaluator.evaluate ~rules ~host:"blocked.example.com" ()
  in
  check_action "deny rule matches" Deny result;
  check_log_policy "no_log policy" No_log result;
  check_index "matched at index 0" 0 result

(** Test: first match wins *)
let test_first_match_wins () =
  let rules =
    [
      {
        host = "*.example.com";
        path = None;
        method_ = None;
        action = Allow;
        log_policy = Log;
      };
      {
        host = "api.example.com";
        path = None;
        method_ = None;
        action = Deny;
        log_policy = No_log;
      };
    ]
  in
  let result = Egress_evaluator.evaluate ~rules ~host:"api.example.com" () in
  check_action "first match wins (allow)" Allow result;
  check_index "matched at index 0" 0 result

(** Test: wildcard host pattern *)
let test_wildcard_host () =
  let rules =
    [
      {
        host = "*.example.com";
        path = None;
        method_ = None;
        action = Allow;
        log_policy = Log;
      };
    ]
  in
  let r1 = Egress_evaluator.evaluate ~rules ~host:"api.example.com" () in
  check_action "subdomain matches wildcard" Allow r1;
  let r2 = Egress_evaluator.evaluate ~rules ~host:"deep.api.example.com" () in
  check_action "deep subdomain matches wildcard" Allow r2;
  let r3 = Egress_evaluator.evaluate ~rules ~host:"example.com" () in
  check_action "bare domain does not match wildcard" Deny r3

(** Test: catch-all wildcard *)
let test_catchall_wildcard () =
  let rules =
    [
      {
        host = "*";
        path = None;
        method_ = None;
        action = Deny;
        log_policy = Log;
      };
    ]
  in
  let result =
    Egress_evaluator.evaluate ~rules ~host:"anything.example.com" ()
  in
  check_action "catch-all denies" Deny result

(** Test: path matching *)
let test_path_matching () =
  let rules =
    [
      {
        host = "api.example.com";
        path = Some "/v1/*";
        method_ = None;
        action = Allow;
        log_policy = Log;
      };
    ]
  in
  let r1 =
    Egress_evaluator.evaluate ~rules ~host:"api.example.com" ~path:"/v1/users"
      ()
  in
  check_action "path matches wildcard" Allow r1;
  let r2 =
    Egress_evaluator.evaluate ~rules ~host:"api.example.com" ~path:"/v2/users"
      ()
  in
  check_action "path does not match" Deny r2;
  let r3 =
    Egress_evaluator.evaluate ~rules ~host:"api.example.com"
      ~path:"/v1/users/123" ()
  in
  check_action "nested path matches wildcard" Allow r3

(** Test: method matching *)
let test_method_matching () =
  let rules =
    [
      {
        host = "api.example.com";
        path = None;
        method_ = Some "GET";
        action = Allow;
        log_policy = Log;
      };
    ]
  in
  let r1 =
    Egress_evaluator.evaluate ~rules ~host:"api.example.com" ~method_:"GET" ()
  in
  check_action "GET matches" Allow r1;
  let r2 =
    Egress_evaluator.evaluate ~rules ~host:"api.example.com" ~method_:"POST" ()
  in
  check_action "POST does not match" Deny r2

(** Test: case-insensitive method matching *)
let test_case_insensitive_method () =
  let rules =
    [
      {
        host = "api.example.com";
        path = None;
        method_ = Some "GET";
        action = Allow;
        log_policy = Log;
      };
    ]
  in
  let r1 =
    Egress_evaluator.evaluate ~rules ~host:"api.example.com" ~method_:"get" ()
  in
  check_action "lowercase get matches" Allow r1;
  let r2 =
    Egress_evaluator.evaluate ~rules ~host:"api.example.com" ~method_:"Get" ()
  in
  check_action "mixed case Get matches" Allow r2

(** Test: combined host, path, and method *)
let test_combined_matching () =
  let rules =
    [
      {
        host = "api.example.com";
        path = Some "/v1/users";
        method_ = Some "GET";
        action = Allow;
        log_policy = No_log;
      };
    ]
  in
  let r1 =
    Egress_evaluator.evaluate ~rules ~host:"api.example.com" ~path:"/v1/users"
      ~method_:"GET" ()
  in
  check_action "all fields match" Allow r1;
  let r2 =
    Egress_evaluator.evaluate ~rules ~host:"api.example.com" ~path:"/v1/users"
      ~method_:"POST" ()
  in
  check_action "method mismatch denies" Deny r2;
  let r3 =
    Egress_evaluator.evaluate ~rules ~host:"api.example.com" ~path:"/v2/users"
      ~method_:"GET" ()
  in
  check_action "path mismatch denies" Deny r3

(** Test: rule with path specified but no path in request *)
let test_rule_requires_path_no_path () =
  let rules =
    [
      {
        host = "api.example.com";
        path = Some "/v1/*";
        method_ = None;
        action = Allow;
        log_policy = Log;
      };
    ]
  in
  let result = Egress_evaluator.evaluate ~rules ~host:"api.example.com" () in
  check_action "no path when required denies" Deny result

(** Test: rule with method specified but no method in request *)
let test_rule_requires_method_no_method () =
  let rules =
    [
      {
        host = "api.example.com";
        path = None;
        method_ = Some "GET";
        action = Allow;
        log_policy = Log;
      };
    ]
  in
  let result = Egress_evaluator.evaluate ~rules ~host:"api.example.com" () in
  check_action "no method when required denies" Deny result

(** Test: multiple rules with different priorities *)
let test_multiple_rules_priority () =
  let rules =
    [
      {
        host = "api.example.com";
        path = Some "/admin/*";
        method_ = None;
        action = Deny;
        log_policy = Log;
      };
      {
        host = "api.example.com";
        path = None;
        method_ = None;
        action = Allow;
        log_policy = Log;
      };
      {
        host = "*";
        path = None;
        method_ = None;
        action = Deny;
        log_policy = Log;
      };
    ]
  in
  let r1 =
    Egress_evaluator.evaluate ~rules ~host:"api.example.com"
      ~path:"/admin/users" ()
  in
  check_action "admin path denied first" Deny r1;
  check_index "admin path matched at index 0" 0 r1;
  let r2 =
    Egress_evaluator.evaluate ~rules ~host:"api.example.com"
      ~path:"/public/data" ()
  in
  check_action "non-admin path allowed" Allow r2;
  check_index "non-admin path matched at index 1" 1 r2;
  let r3 = Egress_evaluator.evaluate ~rules ~host:"other.example.com" () in
  check_action "other host uses catch-all" Deny r3;
  check_index "other host matched at index 2" 2 r3

(** Test: matches_host function *)
let test_matches_host () =
  Alcotest.check Alcotest.bool "exact match" true
    (Egress_evaluator.matches_host ~pattern:"api.example.com"
       ~host:"api.example.com");
  Alcotest.check Alcotest.bool "exact mismatch" false
    (Egress_evaluator.matches_host ~pattern:"api.example.com"
       ~host:"other.example.com");
  Alcotest.check Alcotest.bool "wildcard subdomain" true
    (Egress_evaluator.matches_host ~pattern:"*.example.com"
       ~host:"api.example.com");
  Alcotest.check Alcotest.bool "wildcard deep subdomain" true
    (Egress_evaluator.matches_host ~pattern:"*.example.com"
       ~host:"deep.api.example.com");
  Alcotest.check Alcotest.bool "wildcard bare domain" false
    (Egress_evaluator.matches_host ~pattern:"*.example.com" ~host:"example.com");
  Alcotest.check Alcotest.bool "catch-all" true
    (Egress_evaluator.matches_host ~pattern:"*" ~host:"anything.com")

(** Test: matches_path function *)
let test_matches_path () =
  Alcotest.check Alcotest.bool "exact path" true
    (Egress_evaluator.matches_path ~pattern:"/v1/users" ~path:"/v1/users");
  Alcotest.check Alcotest.bool "path wildcard" true
    (Egress_evaluator.matches_path ~pattern:"/v1/*" ~path:"/v1/users");
  Alcotest.check Alcotest.bool "path wildcard nested" true
    (Egress_evaluator.matches_path ~pattern:"/v1/*" ~path:"/v1/users/123");
  Alcotest.check Alcotest.bool "path mismatch" false
    (Egress_evaluator.matches_path ~pattern:"/v1/*" ~path:"/v2/users");
  Alcotest.check Alcotest.bool "catch-all path" true
    (Egress_evaluator.matches_path ~pattern:"*" ~path:"/anything")

(** Test: default policy with no matching rules *)
let test_default_policy_no_match () =
  let rules =
    [
      {
        host = "other.example.com";
        path = None;
        method_ = None;
        action = Allow;
        log_policy = Log;
      };
    ]
  in
  let result = Egress_evaluator.evaluate ~rules ~host:"api.example.com" () in
  check_action "no match defaults to deny" Deny result;
  check_log_policy "default deny logs" Log result;
  check_index "no match index is -1" (-1) result

(** Test: realistic egress policy *)
let test_realistic_policy () =
  let rules =
    [
      (* Allow specific API endpoints *)
      {
        host = "api.example.com";
        path = Some "/v1/users";
        method_ = Some "GET";
        action = Allow;
        log_policy = No_log;
      };
      {
        host = "api.example.com";
        path = Some "/v1/users";
        method_ = Some "POST";
        action = Allow;
        log_policy = Log;
      };
      (* Block admin endpoints *)
      {
        host = "api.example.com";
        path = Some "/admin/*";
        method_ = None;
        action = Deny;
        log_policy = Log;
      };
      (* Allow all other API calls *)
      {
        host = "api.example.com";
        path = None;
        method_ = None;
        action = Allow;
        log_policy = No_log;
      };
      (* Allow internal services *)
      {
        host = "*.internal.corp";
        path = None;
        method_ = None;
        action = Allow;
        log_policy = No_log;
      };
      (* Deny everything else *)
      {
        host = "*";
        path = None;
        method_ = None;
        action = Deny;
        log_policy = Log;
      };
    ]
  in
  let r1 =
    Egress_evaluator.evaluate ~rules ~host:"api.example.com" ~path:"/v1/users"
      ~method_:"GET" ()
  in
  check_action "GET users allowed" Allow r1;
  check_log_policy "GET users no_log" No_log r1;
  let r2 =
    Egress_evaluator.evaluate ~rules ~host:"api.example.com" ~path:"/v1/users"
      ~method_:"POST" ()
  in
  check_action "POST users allowed" Allow r2;
  check_log_policy "POST users logged" Log r2;
  let r3 =
    Egress_evaluator.evaluate ~rules ~host:"api.example.com"
      ~path:"/admin/config" ()
  in
  check_action "admin blocked" Deny r3;
  let r4 =
    Egress_evaluator.evaluate ~rules ~host:"api.example.com"
      ~path:"/v1/products" ()
  in
  check_action "other API allowed" Allow r4;
  let r5 = Egress_evaluator.evaluate ~rules ~host:"service.internal.corp" () in
  check_action "internal service allowed" Allow r5;
  let r6 = Egress_evaluator.evaluate ~rules ~host:"malicious.com" () in
  check_action "external site denied" Deny r6

let suite =
  [
    Alcotest.test_case "empty rules default to deny" `Quick
      test_empty_rules_default_deny;
    Alcotest.test_case "single allow rule" `Quick test_single_allow_rule;
    Alcotest.test_case "single deny rule" `Quick test_single_deny_rule;
    Alcotest.test_case "first match wins" `Quick test_first_match_wins;
    Alcotest.test_case "wildcard host pattern" `Quick test_wildcard_host;
    Alcotest.test_case "catch-all wildcard" `Quick test_catchall_wildcard;
    Alcotest.test_case "path matching" `Quick test_path_matching;
    Alcotest.test_case "method matching" `Quick test_method_matching;
    Alcotest.test_case "case-insensitive method" `Quick
      test_case_insensitive_method;
    Alcotest.test_case "combined matching" `Quick test_combined_matching;
    Alcotest.test_case "rule requires path, no path" `Quick
      test_rule_requires_path_no_path;
    Alcotest.test_case "rule requires method, no method" `Quick
      test_rule_requires_method_no_method;
    Alcotest.test_case "multiple rules priority" `Quick
      test_multiple_rules_priority;
    Alcotest.test_case "matches_host function" `Quick test_matches_host;
    Alcotest.test_case "matches_path function" `Quick test_matches_path;
    Alcotest.test_case "default policy no match" `Quick
      test_default_policy_no_match;
    Alcotest.test_case "realistic policy" `Quick test_realistic_policy;
  ]

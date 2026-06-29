(* Tests for egress rules model *)

let test_parse_and_roundtrip () =
  let json =
    Yojson.Safe.from_string
      {|{
        "access_bundles": [
          {
            "id": "egress-test",
            "egress_rules": [
              {"host": "api.example.com", "path": "/v1/*", "method": "GET", "action": "allow", "log_policy": "log"},
              {"host": "*.internal.corp", "action": "deny", "log_policy": "no_log"},
              {"host": "*", "action": "deny", "log_policy": "log"}
            ]
          }
        ]
      }|}
  in
  let cfg = Config_loader.parse_config json in
  Alcotest.(check int) "bundle count" 1 (List.length cfg.access_bundles);
  let bundle = List.nth cfg.access_bundles 0 in
  Alcotest.(check int) "egress rules count" 3 (List.length bundle.egress_rules);
  let r0 = List.nth bundle.egress_rules 0 in
  Alcotest.(check string) "host" "api.example.com" r0.host;
  Alcotest.(check (option string)) "path" (Some "/v1/*") r0.path;
  Alcotest.(check (option string)) "method" (Some "GET") r0.method_;
  (match r0.action with
  | Runtime_config.Allow -> ()
  | _ -> Alcotest.fail "expected Allow");
  (match r0.log_policy with
  | Runtime_config.Log -> ()
  | _ -> Alcotest.fail "expected Log");
  let r1 = List.nth bundle.egress_rules 1 in
  Alcotest.(check string) "host wildcard" "*.internal.corp" r1.host;
  Alcotest.(check (option string)) "path none" None r1.path;
  Alcotest.(check (option string)) "method none" None r1.method_;
  (match r1.action with
  | Runtime_config.Deny -> ()
  | _ -> Alcotest.fail "expected Deny");
  (match r1.log_policy with
  | Runtime_config.No_log -> ()
  | _ -> Alcotest.fail "expected No_log");
  (* Test roundtrip *)
  let reparsed = Config_loader.parse_config (Runtime_config.to_json cfg) in
  let bundle' = List.nth reparsed.access_bundles 0 in
  Alcotest.(check int)
    "roundtrip rules count" 3
    (List.length bundle'.egress_rules);
  let r0' = List.nth bundle'.egress_rules 0 in
  Alcotest.(check string) "roundtrip host" "api.example.com" r0'.host

let test_default_policy () =
  let json =
    Yojson.Safe.from_string
      {|{
        "access_bundles": [
          {
            "id": "default-policy-test",
            "egress_rules": []
          }
        ]
      }|}
  in
  let cfg = Config_loader.parse_config json in
  let bundle = List.nth cfg.access_bundles 0 in
  Alcotest.(check int) "empty egress rules" 0 (List.length bundle.egress_rules);
  (* Verify effective access has no rules = deny all *)
  let access =
    Runtime_config.resolve_effective_access cfg ~session_key:"test:room" ()
  in
  Alcotest.(check int)
    "no egress rules means deny all" 0
    (List.length access.egress_rules)

let test_validation () =
  (* Valid rules *)
  let good =
    Yojson.Safe.from_string
      {|{"access_bundles": [{"id": "b", "egress_rules": [{"host": "example.com", "action": "allow"}]}]}|}
  in
  Alcotest.(check (list string))
    "valid rules pass" []
    (Config_loader_support.validate_access_bundle_json_shapes good);
  (* Invalid host *)
  let bad_host =
    Yojson.Safe.from_string
      {|{"access_bundles": [{"id": "b", "egress_rules": [{"host": 123}]}]}|}
  in
  let issues =
    Config_loader_support.validate_access_bundle_json_shapes bad_host
  in
  Alcotest.(check bool)
    "invalid host rejected" true
    (List.exists
       (fun s -> Test_helpers.string_contains s "host must be a string")
       issues);
  (* Invalid action *)
  let bad_action =
    Yojson.Safe.from_string
      {|{"access_bundles": [{"id": "b", "egress_rules": [{"host": "x", "action": "block"}]}]}|}
  in
  let issues2 =
    Config_loader_support.validate_access_bundle_json_shapes bad_action
  in
  Alcotest.(check bool)
    "invalid action rejected" true
    (List.exists
       (fun s ->
         Test_helpers.string_contains s "action must be 'allow' or 'deny'")
       issues2);
  (* Invalid log_policy *)
  let bad_log =
    Yojson.Safe.from_string
      {|{"access_bundles": [{"id": "b", "egress_rules": [{"host": "x", "log_policy": "silent"}]}]}|}
  in
  let issues3 =
    Config_loader_support.validate_access_bundle_json_shapes bad_log
  in
  Alcotest.(check bool)
    "invalid log_policy rejected" true
    (List.exists
       (fun s ->
         Test_helpers.string_contains s "log_policy must be 'log' or 'no_log'")
       issues3)

let test_rules_in_effective_access () =
  let json =
    Yojson.Safe.from_string
      {|{
        "access_bundles": [
          {
            "id": "bundle1",
            "egress_rules": [
              {"host": "api.example.com", "action": "allow", "log_policy": "log"}
            ]
          },
          {
            "id": "bundle2",
            "egress_rules": [
              {"host": "*.example.com", "action": "deny", "log_policy": "no_log"}
            ]
          }
        ],
        "access_scopes": [
          {
            "id": "scope1",
            "level": "default",
            "access_bundle_ids": ["bundle1", "bundle2"]
          }
        ]
      }|}
  in
  let cfg = Config_loader.parse_config json in
  let access =
    Runtime_config.resolve_effective_access cfg ~session_key:"test:room" ()
  in
  Alcotest.(check int)
    "effective egress rules count" 2
    (List.length access.egress_rules);
  (* Both bundles are in the same default scope, so order is preserved *)
  let hosts =
    List.map
      (fun (r : Runtime_config.egress_rule) -> r.host)
      access.egress_rules
  in
  Alcotest.(check (list string))
    "rules from both bundles present"
    [ "api.example.com"; "*.example.com" ]
    hosts

let test_scope_priority_ordering () =
  (* Room scope should override default scope *)
  let json =
    Yojson.Safe.from_string
      {|{
        "access_bundles": [
          {
            "id": "default-bundle",
            "egress_rules": [
              {"host": "*", "action": "deny", "log_policy": "log"}
            ]
          },
          {
            "id": "room-bundle",
            "egress_rules": [
              {"host": "api.example.com", "action": "allow", "log_policy": "log"}
            ]
          }
        ],
        "access_scopes": [
          {
            "id": "default-scope",
            "level": "default",
            "access_bundle_ids": ["default-bundle"]
          },
          {
            "id": "room-scope",
            "level": "room",
            "room": "test:room",
            "access_bundle_ids": ["room-bundle"]
          }
        ]
      }|}
  in
  let cfg = Config_loader.parse_config json in
  let access =
    Runtime_config.resolve_effective_access cfg ~session_key:"test:room" ()
  in
  Alcotest.(check int)
    "effective egress rules count" 2
    (List.length access.egress_rules);
  (* Room scope rules should come first (higher priority) *)
  let r0 = List.nth access.egress_rules 0 in
  Alcotest.(check string) "room rule comes first" "api.example.com" r0.host;
  (match r0.action with
  | Runtime_config.Allow -> ()
  | _ -> Alcotest.fail "room rule should be Allow");
  let r1 = List.nth access.egress_rules 1 in
  Alcotest.(check string) "default rule comes second" "*" r1.host;
  match r1.action with
  | Runtime_config.Deny -> ()
  | _ -> Alcotest.fail "default rule should be Deny"

let suite =
  [
    Alcotest.test_case "egress rules parse and roundtrip" `Quick
      test_parse_and_roundtrip;
    Alcotest.test_case "egress rules default policy deny" `Quick
      test_default_policy;
    Alcotest.test_case "egress rules validation" `Quick test_validation;
    Alcotest.test_case "egress rules in effective access" `Quick
      test_rules_in_effective_access;
    Alcotest.test_case "egress rules scope priority ordering" `Quick
      test_scope_priority_ordering;
    Alcotest.test_case "egress rules profile overrides default" `Quick
      test_profile_rules_override_default;
  ]

open Runtime_config_types

let check_eq = Alcotest.(check string)

let check_bool label expected actual =
  Alcotest.(check bool) label expected actual

let default_policy : external_room_policy =
  {
    default_action = Policy_warn "External participants detected.";
    per_connector = [];
  }

let deny_policy : external_room_policy =
  {
    default_action = Policy_deny ("External rooms not allowed.", true);
    per_connector = [];
  }

let per_connector_policy : external_room_policy =
  {
    default_action = Policy_warn "Default warning.";
    per_connector =
      [
        ("teams", Policy_deny ("Teams external rooms blocked.", false));
        ("slack", Policy_allow);
      ];
  }

let test_room_scope_to_string () =
  check_eq "dm" "dm" (Room_policy.room_scope_to_string Rm_dm);
  check_eq "group" "group" (Room_policy.room_scope_to_string Rm_group);
  check_eq "external" "external" (Room_policy.room_scope_to_string Rm_external);
  check_eq "shared" "shared" (Room_policy.room_scope_to_string Rm_shared);
  check_eq "unknown" "unknown" (Room_policy.room_scope_to_string Rm_unknown)

let test_room_scope_of_string () =
  Alcotest.(check bool) "dm" true (Room_policy.room_scope_of_string "dm" = Rm_dm);
  Alcotest.(check bool)
    "group" true
    (Room_policy.room_scope_of_string "group" = Rm_group);
  Alcotest.(check bool)
    "external" true
    (Room_policy.room_scope_of_string "external" = Rm_external);
  Alcotest.(check bool)
    "shared" true
    (Room_policy.room_scope_of_string "shared" = Rm_shared);
  Alcotest.(check bool)
    "unknown fallback" true
    (Room_policy.room_scope_of_string "bogus" = Rm_unknown)

let test_dm_and_group_always_allowed () =
  let classification =
    {
      connector = "teams";
      room_id = "conv123";
      scope = Rm_dm;
      has_external_users = false;
      tenant_id = None;
    }
  in
  let result =
    Room_policy.evaluate deny_policy ~classification ~is_admin:false ()
  in
  Alcotest.(check bool) "dm allowed" true (result = Room_policy.Proceed);
  let classification = { classification with scope = Rm_group } in
  let result =
    Room_policy.evaluate deny_policy ~classification ~is_admin:false ()
  in
  Alcotest.(check bool) "group allowed" true (result = Room_policy.Proceed)

let test_external_room_warn () =
  let classification =
    {
      connector = "teams";
      room_id = "conv123";
      scope = Rm_external;
      has_external_users = true;
      tenant_id = Some "other-tenant";
    }
  in
  let result =
    Room_policy.evaluate default_policy ~classification ~is_admin:false ()
  in
  match result with
  | Room_policy.Proceed_with_warning msg ->
      check_bool "warn contains connector" true
        (Test_helpers.string_contains msg "teams");
      check_bool "warn contains scope" true
        (Test_helpers.string_contains msg "external")
  | _ -> Alcotest.fail "expected Proceed_with_warning"

let test_external_room_deny_no_admin () =
  let classification =
    {
      connector = "teams";
      room_id = "conv123";
      scope = Rm_external;
      has_external_users = true;
      tenant_id = None;
    }
  in
  let result =
    Room_policy.evaluate deny_policy ~classification ~is_admin:false ()
  in
  match result with
  | Room_policy.Denied msg ->
      check_bool "deny contains reason" true
        (Test_helpers.string_contains msg "not allowed");
      check_bool "deny mentions admin" true
        (Test_helpers.string_contains msg "admin")
  | _ -> Alcotest.fail "expected Denied"

let test_external_room_deny_admin_override () =
  let classification =
    {
      connector = "teams";
      room_id = "conv123";
      scope = Rm_external;
      has_external_users = true;
      tenant_id = None;
    }
  in
  let result =
    Room_policy.evaluate deny_policy ~classification ~is_admin:true ()
  in
  match result with
  | Room_policy.Denied_admin_override msg ->
      check_bool "admin override mentions risk" true
        (Test_helpers.string_contains msg "admin")
  | _ -> Alcotest.fail "expected Denied_admin_override"

let test_unknown_scope_uses_default () =
  let classification =
    {
      connector = "irc";
      room_id = "#channel";
      scope = Rm_unknown;
      has_external_users = false;
      tenant_id = None;
    }
  in
  let result =
    Room_policy.evaluate default_policy ~classification ~is_admin:false ()
  in
  match result with
  | Room_policy.Proceed_with_warning msg ->
      check_bool "unknown uses default" true
        (Test_helpers.string_contains msg "irc")
  | _ -> Alcotest.fail "expected Proceed_with_warning for unknown scope"

let test_per_connector_override () =
  (* Teams external should be denied (per-connector override) *)
  let classification =
    {
      connector = "teams";
      room_id = "conv123";
      scope = Rm_external;
      has_external_users = true;
      tenant_id = None;
    }
  in
  let result =
    Room_policy.evaluate per_connector_policy ~classification ~is_admin:false ()
  in
  (match result with
  | Room_policy.Denied msg ->
      check_bool "teams override deny" true
        (Test_helpers.string_contains msg "blocked")
  | _ -> Alcotest.fail "expected Denied for teams external");
  (* Slack external should be allowed (per-connector override) *)
  let classification =
    { classification with connector = "slack"; scope = Rm_external }
  in
  let result =
    Room_policy.evaluate per_connector_policy ~classification ~is_admin:false ()
  in
  Alcotest.(check bool)
    "slack override allow" true
    (result = Room_policy.Proceed)

let test_room_status_message () =
  let classification =
    {
      connector = "teams";
      room_id = "conv123";
      scope = Rm_unknown;
      has_external_users = false;
      tenant_id = None;
    }
  in
  let msg = Room_policy.room_status_message ~classification () in
  check_bool "unknown msg mentions connector" true
    (Test_helpers.string_contains msg "teams");
  check_bool "unknown msg explains missing" true
    (Test_helpers.string_contains msg "does not expose")

let test_unsupported_connector_message () =
  let msg = Room_policy.unsupported_connector_message ~connector:"irc" () in
  check_bool "unsupported mentions connector" true
    (Test_helpers.string_contains msg "irc");
  check_bool "unsupported explains unknown" true
    (Test_helpers.string_contains msg "does not report")

let test_classification_from_context () =
  let cls =
    Room_policy.classification_from_context ~connector:"teams" ~room_id:"c1"
      ~session_key:"teams:t1:c1" ~is_group:true ~has_external_users:false ()
  in
  Alcotest.(check bool) "group scope" true (cls.scope = Rm_group);
  let cls_ext =
    Room_policy.classification_from_context ~connector:"teams" ~room_id:"c2"
      ~session_key:"teams:t1:c2" ~is_group:true ~has_external_users:true ()
  in
  Alcotest.(check bool)
    "external overrides group" true
    (cls_ext.scope = Rm_external)

let suite =
  [
    Alcotest.test_case "scope to_string" `Quick test_room_scope_to_string;
    Alcotest.test_case "scope of_string" `Quick test_room_scope_of_string;
    Alcotest.test_case "dm/group always allowed" `Quick
      test_dm_and_group_always_allowed;
    Alcotest.test_case "external room warn" `Quick test_external_room_warn;
    Alcotest.test_case "external room deny non-admin" `Quick
      test_external_room_deny_no_admin;
    Alcotest.test_case "external room deny admin override" `Quick
      test_external_room_deny_admin_override;
    Alcotest.test_case "unknown scope uses default" `Quick
      test_unknown_scope_uses_default;
    Alcotest.test_case "per-connector override" `Quick
      test_per_connector_override;
    Alcotest.test_case "room status message" `Quick test_room_status_message;
    Alcotest.test_case "unsupported connector" `Quick
      test_unsupported_connector_message;
    Alcotest.test_case "classification from context" `Quick
      test_classification_from_context;
  ]

(* Tests for invocation restriction enforcement by scope. *)

open Runtime_config_types
open Invocation_restrict

let parse json = Config_loader.parse_config (Yojson.Safe.from_string json)

let test_check_role_admin_allowed () =
  let result =
    Invocation_restrict.check_role ~user_group:(Some "admin")
      ~work_kind:Room_work ()
  in
  Alcotest.(check bool)
    "admin allowed for room_work" true
    (result = Invocation_restrict.Allowed)

let test_check_role_member_allowed () =
  let result =
    Invocation_restrict.check_role ~user_group:(Some "member")
      ~work_kind:Routine ()
  in
  Alcotest.(check bool)
    "member allowed for routine" true
    (result = Invocation_restrict.Allowed)

let test_check_role_guest_denied_for_routine () =
  let result =
    Invocation_restrict.check_role ~user_group:(Some "guest") ~work_kind:Routine
      ()
  in
  match result with
  | Invocation_restrict.Denied msg ->
      Alcotest.(check bool)
        "denied message contains guest" true
        (Test_helpers.string_contains msg "guest");
      Alcotest.(check bool)
        "denied message contains routine" true
        (Test_helpers.string_contains msg "routine")
  | _ -> Alcotest.fail "expected Denied for guest routine"

let test_check_role_guest_denied_for_memory_mutation () =
  let result =
    Invocation_restrict.check_role ~user_group:(Some "guest")
      ~work_kind:Memory_mutation ()
  in
  match result with
  | Invocation_restrict.Denied msg ->
      Alcotest.(check bool)
        "denied message contains guest" true
        (Test_helpers.string_contains msg "guest");
      Alcotest.(check bool)
        "denied message contains memory_mutation" true
        (Test_helpers.string_contains msg "memory_mutation")
  | _ -> Alcotest.fail "expected Denied for guest memory_mutation"

let test_check_role_guest_denied_for_github_trigger () =
  let result =
    Invocation_restrict.check_role ~user_group:(Some "guest")
      ~work_kind:GitHub_trigger ()
  in
  match result with
  | Invocation_restrict.Denied msg ->
      Alcotest.(check bool)
        "denied message contains guest" true
        (Test_helpers.string_contains msg "guest");
      Alcotest.(check bool)
        "denied message contains github_trigger" true
        (Test_helpers.string_contains msg "github_trigger")
  | _ -> Alcotest.fail "expected Denied for guest github_trigger"

let test_check_role_guest_denied_for_background_task () =
  let result =
    Invocation_restrict.check_role ~user_group:(Some "guest")
      ~work_kind:Background_task ()
  in
  match result with
  | Invocation_restrict.Denied msg ->
      Alcotest.(check bool)
        "denied message contains guest" true
        (Test_helpers.string_contains msg "guest");
      Alcotest.(check bool)
        "denied message contains background_task" true
        (Test_helpers.string_contains msg "background_task")
  | _ -> Alcotest.fail "expected Denied for guest background_task"

let test_check_role_guest_allowed_for_room_work () =
  let result =
    Invocation_restrict.check_role ~user_group:(Some "guest")
      ~work_kind:Room_work ()
  in
  Alcotest.(check bool)
    "guest allowed for room_work" true
    (result = Invocation_restrict.Allowed)

let test_check_role_unknown_defaults_to_guest () =
  let result =
    Invocation_restrict.check_role ~user_group:None ~work_kind:Routine ()
  in
  match result with
  | Invocation_restrict.Denied msg ->
      Alcotest.(check bool)
        "denied message contains guest" true
        (Test_helpers.string_contains msg "guest")
  | _ -> Alcotest.fail "expected Denied for unknown role routine"

let test_check_role_unknown_allowed_for_room_work () =
  let result =
    Invocation_restrict.check_role ~user_group:None ~work_kind:Room_work ()
  in
  Alcotest.(check bool)
    "unknown allowed for room_work" true
    (result = Invocation_restrict.Allowed)

let test_work_kind_to_string () =
  Alcotest.(check string)
    "room_work" "room_work"
    (Invocation_restrict.work_kind_to_string Room_work);
  Alcotest.(check string)
    "routine" "routine"
    (Invocation_restrict.work_kind_to_string Routine);
  Alcotest.(check string)
    "memory_mutation" "memory_mutation"
    (Invocation_restrict.work_kind_to_string Memory_mutation);
  Alcotest.(check string)
    "github_trigger" "github_trigger"
    (Invocation_restrict.work_kind_to_string GitHub_trigger);
  Alcotest.(check string)
    "background_task" "background_task"
    (Invocation_restrict.work_kind_to_string Background_task)

let test_caller_role_to_string () =
  Alcotest.(check string)
    "admin" "admin"
    (Invocation_restrict.caller_role_to_string Admin);
  Alcotest.(check string)
    "member" "member"
    (Invocation_restrict.caller_role_to_string Member);
  Alcotest.(check string)
    "guest" "guest"
    (Invocation_restrict.caller_role_to_string Guest);
  Alcotest.(check string)
    "unknown" "unknown"
    (Invocation_restrict.caller_role_to_string Unknown)

let test_caller_role_of_string () =
  Alcotest.(check bool)
    "admin" true
    (Invocation_restrict.caller_role_of_string "admin" = Admin);
  Alcotest.(check bool)
    "member" true
    (Invocation_restrict.caller_role_of_string "member" = Member);
  Alcotest.(check bool)
    "guest" true
    (Invocation_restrict.caller_role_of_string "guest" = Guest);
  Alcotest.(check bool)
    "unknown" true
    (Invocation_restrict.caller_role_of_string "bogus" = Unknown)

let test_required_roles_for_work_kind () =
  (* Room work: all roles allowed (empty list) *)
  Alcotest.(check int)
    "room_work requires no specific roles" 0
    (List.length (Invocation_restrict.required_roles_for_work_kind Room_work));
  (* Routine: admin and member *)
  let routine_roles =
    Invocation_restrict.required_roles_for_work_kind Routine
  in
  Alcotest.(check bool)
    "routine requires admin" true
    (List.mem Admin routine_roles);
  Alcotest.(check bool)
    "routine requires member" true
    (List.mem Member routine_roles);
  Alcotest.(check bool)
    "routine does not require guest" false
    (List.mem Guest routine_roles);
  (* Memory mutation: admin and member *)
  let memory_roles =
    Invocation_restrict.required_roles_for_work_kind Memory_mutation
  in
  Alcotest.(check bool)
    "memory_mutation requires admin" true
    (List.mem Admin memory_roles);
  Alcotest.(check bool)
    "memory_mutation requires member" true
    (List.mem Member memory_roles);
  (* GitHub trigger: admin and member *)
  let github_roles =
    Invocation_restrict.required_roles_for_work_kind GitHub_trigger
  in
  Alcotest.(check bool)
    "github_trigger requires admin" true
    (List.mem Admin github_roles);
  Alcotest.(check bool)
    "github_trigger requires member" true
    (List.mem Member github_roles);
  (* Background task: admin and member *)
  let bg_roles =
    Invocation_restrict.required_roles_for_work_kind Background_task
  in
  Alcotest.(check bool)
    "background_task requires admin" true (List.mem Admin bg_roles);
  Alcotest.(check bool)
    "background_task requires member" true (List.mem Member bg_roles)

let test_denial_message_redacted () =
  let result =
    Invocation_restrict.check_role ~user_group:(Some "guest") ~work_kind:Routine
      ()
  in
  match result with
  | Invocation_restrict.Denied msg ->
      let redacted = Invocation_restrict.redacted_denial_message msg in
      Alcotest.(check string) "redacted message matches original" msg redacted
  | _ -> Alcotest.fail "expected Denied"

let test_check_room_policy_and_role_admin_allowed () =
  let json = {|{
      "workspace": "/tmp/test"
    }|} in
  let cfg = parse json in
  let result =
    Invocation_restrict.check_room_policy_and_role ~config:cfg ~key:"slack:C123"
      ~channel:(Some "slack") ~channel_id:(Some "C123")
      ~user_group:(Some "admin") ~work_kind:Room_work ()
  in
  match result with
  | Ok (cls, _decision) ->
      Alcotest.(check bool)
        "classification has slack connector" true (cls.connector = "slack")
  | Error msg -> Alcotest.fail ("expected Ok, got Error: " ^ msg)

let test_check_room_policy_and_role_guest_denied_for_routine () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "external_room_policy": {
        "default_action": "allow"
      }
    }|}
  in
  let cfg = parse json in
  let result =
    Invocation_restrict.check_room_policy_and_role ~config:cfg ~key:"slack:C123"
      ~channel:(Some "slack") ~channel_id:(Some "C123")
      ~user_group:(Some "guest") ~work_kind:Routine ()
  in
  match result with
  | Ok _ -> Alcotest.fail "expected Error for guest routine"
  | Error msg ->
      Alcotest.(check bool)
        "error message contains guest" true
        (Test_helpers.string_contains msg "guest");
      Alcotest.(check bool)
        "error message contains routine" true
        (Test_helpers.string_contains msg "routine")

let test_check_room_policy_and_role_external_room_denied () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "external_room_policy": {
        "default": {
          "action": "deny",
          "reason": "External rooms not allowed.",
          "allow_admin_override": false
        }
      }
    }|}
  in
  let cfg = parse json in
  (* Use has_external_users:true with a known connector to trigger external scope *)
  let result =
    Invocation_restrict.check_room_policy_and_role ~config:cfg ~key:"teams:C123"
      ~channel:(Some "teams") ~channel_id:(Some "C123")
      ~user_group:(Some "member") ~has_external_users:true ~work_kind:Room_work
      ()
  in
  match result with
  | Ok _ -> Alcotest.fail "expected Error for external room"
  | Error msg ->
      Alcotest.(check bool)
        "error message contains not allowed" true
        (Test_helpers.string_contains msg "not allowed")

let test_check_room_policy_and_role_admin_override_external () =
  let json =
    {|{
      "workspace": "/tmp/test",
      "external_room_policy": {
        "default": {
          "action": "deny",
          "reason": "External rooms not allowed.",
          "allow_admin_override": true
        }
      }
    }|}
  in
  let cfg = parse json in
  let result =
    Invocation_restrict.check_room_policy_and_role ~config:cfg ~key:"teams:C123"
      ~channel:(Some "teams") ~channel_id:(Some "C123")
      ~user_group:(Some "admin") ~has_external_users:true ~work_kind:Room_work
      ()
  in
  match result with
  | Ok (_cls, decision) ->
      Alcotest.(check bool)
        "decision contains admin_override" true
        (Test_helpers.string_contains decision "admin_override")
  | Error msg -> Alcotest.fail ("expected Ok, got Error: " ^ msg)

let suite =
  [
    Alcotest.test_case "admin allowed for room_work" `Quick
      test_check_role_admin_allowed;
    Alcotest.test_case "member allowed for routine" `Quick
      test_check_role_member_allowed;
    Alcotest.test_case "guest denied for routine" `Quick
      test_check_role_guest_denied_for_routine;
    Alcotest.test_case "guest denied for memory_mutation" `Quick
      test_check_role_guest_denied_for_memory_mutation;
    Alcotest.test_case "guest denied for github_trigger" `Quick
      test_check_role_guest_denied_for_github_trigger;
    Alcotest.test_case "guest denied for background_task" `Quick
      test_check_role_guest_denied_for_background_task;
    Alcotest.test_case "guest allowed for room_work" `Quick
      test_check_role_guest_allowed_for_room_work;
    Alcotest.test_case "unknown defaults to guest" `Quick
      test_check_role_unknown_defaults_to_guest;
    Alcotest.test_case "unknown allowed for room_work" `Quick
      test_check_role_unknown_allowed_for_room_work;
    Alcotest.test_case "work_kind to_string" `Quick test_work_kind_to_string;
    Alcotest.test_case "caller_role to_string" `Quick test_caller_role_to_string;
    Alcotest.test_case "caller_role of_string" `Quick test_caller_role_of_string;
    Alcotest.test_case "required roles for work_kind" `Quick
      test_required_roles_for_work_kind;
    Alcotest.test_case "denial message redacted" `Quick
      test_denial_message_redacted;
    Alcotest.test_case "admin allowed for room policy" `Quick
      test_check_room_policy_and_role_admin_allowed;
    Alcotest.test_case "guest denied for routine with room policy" `Quick
      test_check_room_policy_and_role_guest_denied_for_routine;
    Alcotest.test_case "external room denied" `Quick
      test_check_room_policy_and_role_external_room_denied;
    Alcotest.test_case "admin override external room" `Quick
      test_check_room_policy_and_role_admin_override_external;
  ]

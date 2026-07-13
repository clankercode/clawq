(** Tests for room-agent pilot planning via shared Setup_plan (P20.M2.E1.T001).
*)

open Setup_room_wizard_types

let fixed_now = 1_700_000_000.0

let sample_principal =
  Setup_plan.
    {
      id = "principal:pilot-admin";
      kind = Principal;
      label = Some "Pilot Admin";
    }

let make_empty_cfg () : Runtime_config.t =
  Config_loader.parse_config (`Assoc [])

let make_teams_cfg () : Runtime_config.t =
  let json =
    Yojson.Safe.from_string
      {|{"channels": {"teams": {"app_id": "test-app", "app_secret": "secret-value-xyz", "tenant_id": "tenant", "webhook_path": "/webhook", "service_url": "https://smba.trafficmanager.net"}}}|}
  in
  Config_loader.parse_config json

let make_cfg_with_profile () : Runtime_config.t =
  let json =
    Yojson.Safe.from_string
      {|{
        "channels": {
          "teams": {
            "app_id": "test-app",
            "app_secret": "secret-value-xyz",
            "tenant_id": "tenant",
            "webhook_path": "/webhook",
            "service_url": "https://smba.trafficmanager.net"
          }
        },
        "room_profiles": [
          {
            "id": "pilot-agent",
            "model": "openai:gpt-4o",
            "system_prompt": "existing",
            "max_tool_iterations": 10,
            "status": "active"
          }
        ],
        "room_profile_bindings": [
          {
            "profile_id": "other-agent",
            "room": "19:old@thread.tacv2",
            "active": true
          }
        ]
      }|}
  in
  Config_loader.parse_config json

let sample_state ?(profile_id = "pilot-agent") ?(model = "openai:gpt-5.4")
    ?(connector_type = "teams") ?(connector_room = "19:abc@thread.tacv2")
    ?(token_limit = 0) ?(cost_limit_usd = 0.0) () : wizard_state =
  {
    default_state with
    profile_id;
    model;
    max_tool_iterations = 25;
    connector_type;
    connector_room;
    connector_active = true;
    memory_scope_kind = "room";
    memory_scope_key = connector_room;
    token_limit;
    cost_limit_usd;
    budget_reset_period = "monthly";
  }

let json_string j = Yojson.Safe.to_string j
let contains hay needle = Test_helpers.string_contains hay needle

(** Planning is pure: same inputs → same digest; config hash unchanged; no
    durable side effects. *)
let test_planning_is_pure () =
  let cfg = make_teams_cfg () in
  let rev_before = Setup_plan.base_revision_of_config cfg in
  let state = sample_state () in
  let p1 =
    Room_agent_setup_plan.plan ~cfg ~state ~principal:sample_principal
      ~base_revision:rev_before ~now:fixed_now ~id:"plan_pure_1" ()
  in
  let p2 =
    Room_agent_setup_plan.plan ~cfg ~state ~principal:sample_principal
      ~base_revision:rev_before ~now:fixed_now ~id:"plan_pure_1" ()
  in
  Alcotest.(check string) "digest stable" p1.digest p2.digest;
  Alcotest.(check string) "id fixed" "plan_pure_1" p1.id;
  Alcotest.(check string) "base_revision" rev_before p1.base_revision;
  let rev_after = Setup_plan.base_revision_of_config cfg in
  Alcotest.(check string)
    "config hash unchanged after plan" rev_before rev_after;
  Alcotest.(check int)
    "profiles count unchanged" 0
    (List.length cfg.room_profiles);
  Alcotest.(check int)
    "bindings count unchanged" 0
    (List.length cfg.room_profile_bindings);
  Alcotest.(check bool)
    "apply kind Room_profile" true
    (match p1.apply_payload.kind with
    | Setup_plan.Room_profile -> true
    | _ -> false);
  Alcotest.(check bool) "has diff ops" true (List.length p1.diff > 0)

let test_redacted_secret_free () =
  let cfg = make_teams_cfg () in
  let state = sample_state () in
  let plan =
    Room_agent_setup_plan.plan ~cfg ~state ~principal:sample_principal
      ~now:fixed_now ~id:"plan_redact" ()
  in
  let surfaces =
    [
      Setup_plan.format_summary plan;
      json_string (Setup_plan.to_render_json plan);
      json_string (Setup_plan.to_persist_json plan);
      json_string plan.current_state;
      json_string plan.planned_state;
      json_string plan.apply_payload.ops;
      json_string plan.apply_payload.data;
    ]
    |> String.concat "\n"
  in
  Alcotest.(check bool)
    "no app_secret value" false
    (contains surfaces "secret-value-xyz");
  Alcotest.(check bool)
    "no bot-token-like material" false
    (contains surfaces "xoxb-");
  (* Redact is identity on clean plans (digest preserved). *)
  let again = Setup_plan.redact plan in
  Alcotest.(check string) "digest stable under redact" plan.digest again.digest

let test_readiness_items_present () =
  let cfg = make_teams_cfg () in
  let state = sample_state () in
  let plan =
    Room_agent_setup_plan.plan ~cfg ~state ~principal:sample_principal
      ~now:fixed_now ~id:"plan_ready" ()
  in
  Alcotest.(check bool)
    "readiness non-empty" true
    (List.length plan.readiness > 0);
  let names =
    List.map (fun (r : Setup_plan.readiness_item) -> r.name) plan.readiness
  in
  let require name =
    Alcotest.(check bool)
      (Printf.sprintf "has readiness %s" name)
      true (List.mem name names)
  in
  require "Profile ID";
  require "Model";
  require "Access Bundles";
  require "Connector Available";
  require "Connector Room";
  require "Budget";
  require "Max Tool Iterations";
  (* Profile ID + model + connector room should pass for valid pilot input. *)
  let status_of name =
    match
      List.find_opt
        (fun (r : Setup_plan.readiness_item) -> r.name = name)
        plan.readiness
    with
    | Some r -> r.status
    | None -> Setup_plan.Fail
  in
  Alcotest.(check bool)
    "Profile ID pass" true
    (status_of "Profile ID" = Setup_plan.Pass);
  Alcotest.(check bool) "Model pass" true (status_of "Model" = Setup_plan.Pass);
  Alcotest.(check bool)
    "Connector Room pass" true
    (status_of "Connector Room" = Setup_plan.Pass)

let test_diff_create_and_bind () =
  let cfg = make_teams_cfg () in
  let state = sample_state () in
  let plan =
    Room_agent_setup_plan.plan ~cfg ~state ~principal:sample_principal
      ~now:fixed_now ~id:"plan_diff" ()
  in
  let has_create_profile =
    List.exists
      (function
        | Setup_plan.Create { path; _ } ->
            String.starts_with ~prefix:"room_profiles/" path
        | _ -> false)
      plan.diff
  in
  let has_bind =
    List.exists (function Setup_plan.Bind _ -> true | _ -> false) plan.diff
  in
  Alcotest.(check bool) "create profile op" true has_create_profile;
  Alcotest.(check bool) "bind op" true has_bind;
  Alcotest.(check (option string))
    "destination room" (Some "19:abc@thread.tacv2") plan.destination.room_id;
  Alcotest.(check (option string))
    "destination profile" (Some "pilot-agent") plan.destination.profile_id;
  Alcotest.(check (option string))
    "destination connector" (Some "teams") plan.destination.connector

let test_diff_update_existing_profile () =
  let cfg = make_cfg_with_profile () in
  (* Non-default model forces an update (merge preserves default model). *)
  let state = sample_state ~model:"openai:gpt-5.3" () in
  let plan =
    Room_agent_setup_plan.plan ~cfg ~state ~principal:sample_principal
      ~now:fixed_now ~id:"plan_update" ()
  in
  let has_update =
    List.exists
      (function
        | Setup_plan.Update { path; _ } ->
            String.starts_with ~prefix:"room_profiles/" path
        | _ -> false)
      plan.diff
  in
  Alcotest.(check bool) "update existing profile" true has_update;
  match plan.current_state with
  | `Assoc fields -> (
      match List.assoc_opt "profile" fields with
      | Some (`Assoc pfields) -> (
          match List.assoc_opt "model" pfields with
          | Some (`String m) ->
              Alcotest.(check string) "current model" "openai:gpt-4o" m
          | _ -> Alcotest.fail "current profile model missing")
      | _ -> Alcotest.fail "current profile missing")
  | _ -> Alcotest.fail "current_state not object"

let test_cli_and_agent_share_adapter () =
  let cfg = make_teams_cfg () in
  let state = sample_state () in
  let rev = "rev-shared" in
  let agent_plan =
    Room_agent_setup_plan.plan ~cfg ~state ~principal:sample_principal
      ~base_revision:rev ~now:fixed_now ~id:"plan_shared" ()
  in
  let cli_principal = Room_agent_setup_plan.default_cli_principal in
  let cli_plan =
    Room_agent_setup_plan.plan ~cfg ~state ~principal:cli_principal
      ~base_revision:rev ~now:fixed_now ~id:"plan_shared" ()
  in
  (* Same adapter, same planned state / diff / readiness shape. *)
  Alcotest.(check string)
    "planned_state equal"
    (json_string agent_plan.planned_state)
    (json_string cli_plan.planned_state);
  Alcotest.(check int)
    "diff length equal"
    (List.length agent_plan.diff)
    (List.length cli_plan.diff);
  Alcotest.(check int)
    "readiness length equal"
    (List.length agent_plan.readiness)
    (List.length cli_plan.readiness);
  Alcotest.(check bool)
    "cli principal kind" true
    (match cli_plan.principal.kind with Setup_plan.Cli -> true | _ -> false)

let test_invalid_profile_fails_readiness () =
  let cfg = make_empty_cfg () in
  let state = sample_state ~profile_id:"" ~connector_room:"" () in
  let plan =
    Room_agent_setup_plan.plan ~cfg ~state ~principal:sample_principal
      ~now:fixed_now ~id:"plan_bad" ()
  in
  Alcotest.(check bool)
    "readiness_ok false" false
    (Setup_plan.readiness_ok plan);
  let profile_check =
    List.find
      (fun (r : Setup_plan.readiness_item) -> r.name = "Profile ID")
      plan.readiness
  in
  Alcotest.(check bool)
    "Profile ID fail" true
    (profile_check.status = Setup_plan.Fail)

let suite =
  [
    ("planning is pure", `Quick, test_planning_is_pure);
    ("redacted secret-free surfaces", `Quick, test_redacted_secret_free);
    ("readiness items present", `Quick, test_readiness_items_present);
    ("diff create and bind", `Quick, test_diff_create_and_bind);
    ("diff update existing profile", `Quick, test_diff_update_existing_profile);
    ("cli and agent share adapter", `Quick, test_cli_and_agent_share_adapter);
    ( "invalid profile fails readiness",
      `Quick,
      test_invalid_profile_fails_readiness );
  ]

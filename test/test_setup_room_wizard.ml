(** Tests for Setup_room_wizard Teams-first connector path and Slack baseline.
*)

open Setup_room_wizard

let make_empty_cfg () : Runtime_config.t =
  Config_loader.parse_config (`Assoc [])

let make_teams_cfg () : Runtime_config.t =
  let json =
    Yojson.Safe.from_string
      {|{"channels": {"teams": {"app_id": "test-app", "app_secret": "secret", "tenant_id": "tenant", "webhook_path": "/webhook", "service_url": "https://smba.trafficmanager.net"}}}|}
  in
  Config_loader.parse_config json

let make_slack_cfg () : Runtime_config.t =
  let json =
    Yojson.Safe.from_string
      {|{"channels": {"slack": {"bot_token": "xoxb-test", "signing_secret": "secret", "events_path": "/slack/events", "app_token": "xapp-test", "socket_mode": true}}}|}
  in
  Config_loader.parse_config json

let make_both_cfg () : Runtime_config.t =
  let json =
    Yojson.Safe.from_string
      {|{"channels": {"teams": {"app_id": "test-app", "app_secret": "secret", "tenant_id": "tenant", "webhook_path": "/webhook", "service_url": "https://smba.trafficmanager.net"}, "slack": {"bot_token": "xoxb-test", "signing_secret": "secret", "events_path": "/slack/events", "app_token": "xapp-test", "socket_mode": true}}}|}
  in
  Config_loader.parse_config json

(** {1 Connector detection tests} *)

let test_connector_is_configured () =
  let base_cfg = make_empty_cfg () in
  (* When teams is not configured *)
  Alcotest.(check bool)
    "teams not configured" false
    (connector_is_configured base_cfg "teams");
  (* When teams IS configured *)
  let cfg_with_teams = make_teams_cfg () in
  Alcotest.(check bool)
    "teams configured" true
    (connector_is_configured cfg_with_teams "teams");
  (* Unknown connector *)
  Alcotest.(check bool)
    "unknown connector" false
    (connector_is_configured base_cfg "matrix")

let test_configured_connectors () =
  let base_cfg = make_empty_cfg () in
  Alcotest.(check (list string))
    "no connectors" []
    (configured_connectors base_cfg);
  let cfg_with_teams = make_teams_cfg () in
  Alcotest.(check bool)
    "teams in configured list" true
    (List.mem "teams" (configured_connectors cfg_with_teams))

let test_default_connector () =
  let base_cfg = make_empty_cfg () in
  (* When nothing is configured, defaults to "teams" *)
  Alcotest.(check string)
    "default with no config" "teams"
    (default_connector base_cfg);
  (* When teams is configured *)
  let cfg_with_teams = make_teams_cfg () in
  Alcotest.(check string)
    "default with teams" "teams"
    (default_connector cfg_with_teams);
  (* When only slack is configured *)
  let cfg_with_slack = make_slack_cfg () in
  Alcotest.(check string)
    "default with slack only" "slack"
    (default_connector cfg_with_slack)

(** {1 Room validation tests} *)

let test_validate_teams_room_id () =
  Alcotest.(check bool)
    "empty teams room is error" true
    (Result.is_error (validate_teams_room_id ""));
  Alcotest.(check bool)
    "short teams room is error" true
    (Result.is_error (validate_teams_room_id "ab"));
  Alcotest.(check bool)
    "non-teams format is error" true
    (Result.is_error (validate_teams_room_id "invalid"));
  Alcotest.(check bool)
    "random colon format is error" true
    (Result.is_error (validate_teams_room_id "abc:def"));
  Alcotest.(check bool)
    "valid teams room with thread" true
    (Result.is_ok (validate_teams_room_id "19:abc123@thread.tacv2"));
  Alcotest.(check bool)
    "valid teams room 19: prefix" true
    (Result.is_ok (validate_teams_room_id "19:abc123"))

let test_validate_slack_room_id () =
  Alcotest.(check bool)
    "empty slack room is error" true
    (Result.is_error (validate_slack_room_id ""));
  Alcotest.(check bool)
    "short slack room is error" true
    (Result.is_error (validate_slack_room_id "X"));
  Alcotest.(check bool)
    "valid slack public channel" true
    (Result.is_ok (validate_slack_room_id "C12345"));
  Alcotest.(check bool)
    "valid slack private channel" true
    (Result.is_ok (validate_slack_room_id "G67890"));
  Alcotest.(check bool)
    "valid slack dm" true
    (Result.is_ok (validate_slack_room_id "D12345"));
  Alcotest.(check bool)
    "valid slack channel name" true
    (Result.is_ok (validate_slack_room_id "#general"));
  Alcotest.(check bool)
    "invalid slack room" true
    (Result.is_error (validate_slack_room_id "invalid-id"))

let test_validate_room_id_for_connector () =
  Alcotest.(check bool)
    "teams dispatcher ok" true
    (Result.is_ok
       (validate_room_id_for_connector "teams" "19:abc@thread.tacv2"));
  Alcotest.(check bool)
    "slack dispatcher ok" true
    (Result.is_ok (validate_room_id_for_connector "slack" "C12345"));
  Alcotest.(check bool)
    "other connector empty is error" true
    (Result.is_error (validate_room_id_for_connector "discord" ""));
  Alcotest.(check bool)
    "other connector valid" true
    (Result.is_ok (validate_room_id_for_connector "discord" "room-123"))

(** {1 Capability comparison tests} *)

let test_compare_teams_vs_slack () =
  let rows = compare_teams_vs_slack () in
  Alcotest.(check int) "comparison row count" 11 (List.length rows);
  (* Check that Teams has cards *)
  let cards_row = List.find (fun r -> r.feature = "Adaptive Cards") rows in
  Alcotest.(check string) "teams has cards" "Yes" cards_row.teams_value;
  Alcotest.(check string) "slack no cards" "No" cards_row.slack_value;
  (* Check that Slack has reactions *)
  let react_row = List.find (fun r -> r.feature = "Reactions") rows in
  Alcotest.(check string) "slack has reactions" "Yes" react_row.slack_value;
  Alcotest.(check string) "teams no reactions" "No" react_row.teams_value;
  (* Check max message length *)
  let length_row = List.find (fun r -> r.feature = "Max message length") rows in
  Alcotest.(check string) "teams max length" "28672" length_row.teams_value;
  Alcotest.(check string) "slack max length" "4000" length_row.slack_value

(** {1 Plan generation with connector_type tests} *)

let test_plan_includes_connector () =
  let cfg = make_empty_cfg () in
  let state =
    {
      default_state with
      profile_id = "test-profile";
      connector_type = "teams";
      connector_room = "19:abc@thread.tacv2";
    }
  in
  let plan = generate_plan ~cfg ~state in
  let connector_items =
    List.filter (fun (p : plan_item) -> p.category = "Connector") plan
  in
  Alcotest.(check int) "has connector item" 1 (List.length connector_items);
  let item = List.hd connector_items in
  Alcotest.(check string) "teams primary" "primary" item.action

let test_plan_slack_connector () =
  let cfg = make_empty_cfg () in
  let state =
    {
      default_state with
      profile_id = "test-profile";
      connector_type = "slack";
      connector_room = "C12345";
    }
  in
  let plan = generate_plan ~cfg ~state in
  let connector_items =
    List.filter (fun (p : plan_item) -> p.category = "Connector") plan
  in
  Alcotest.(check int) "has connector item" 1 (List.length connector_items);
  let item = List.hd connector_items in
  Alcotest.(check string) "slack bind" "bind" item.action

(** {1 Readiness check tests} *)

let test_readiness_connector_available () =
  let base_cfg = make_empty_cfg () in
  let state =
    { default_state with connector_type = "teams"; connector_room = "19:abc" }
  in
  let checks = run_readiness_checks ~cfg:base_cfg ~db:None ~state in
  let connector_check =
    List.find (fun c -> c.name = "Connector Available") checks
  in
  Alcotest.(check bool) "teams not available" false connector_check.passed;
  (* Now with teams configured *)
  let cfg_with_teams = make_teams_cfg () in
  let checks2 = run_readiness_checks ~cfg:cfg_with_teams ~db:None ~state in
  let connector_check2 =
    List.find (fun c -> c.name = "Connector Available") checks2
  in
  Alcotest.(check bool) "teams available" true connector_check2.passed

let test_readiness_room_validation () =
  let cfg = make_empty_cfg () in
  (* Valid teams room *)
  let state_valid =
    {
      default_state with
      connector_type = "teams";
      connector_room = "19:abc@thread.tacv2";
    }
  in
  let checks = run_readiness_checks ~cfg ~db:None ~state:state_valid in
  let room_check = List.find (fun c -> c.name = "Connector Room") checks in
  Alcotest.(check bool) "valid teams room passes" true room_check.passed;
  (* Invalid slack room (doesn't start with C/G/D/#) *)
  let state_invalid =
    { default_state with connector_type = "slack"; connector_room = "invalid" }
  in
  let checks2 = run_readiness_checks ~cfg ~db:None ~state:state_invalid in
  let room_check2 = List.find (fun c -> c.name = "Connector Room") checks2 in
  Alcotest.(check bool) "invalid slack room fails" false room_check2.passed

(** {1 Default state tests} *)

let test_default_state_is_teams () =
  Alcotest.(check string)
    "default connector type" "teams" default_state.connector_type

(** {1 Rerun/Repair tests} *)

(** Helper to make a config with an existing room profile. *)
let make_cfg_with_profile () : Runtime_config.t =
  let json =
    Yojson.Safe.from_string
      {|{"room_profiles": [{"id": "my-profile", "model": "openai:gpt-5.4", "system_prompt": "", "max_tool_iterations": 25, "status": "active"}], "room_profile_bindings": [{"profile_id": "my-profile", "room": "19:abc@thread.tacv2", "active": true}]}|}
  in
  Config_loader.parse_config json

(** Helper to make a config with teams connector and profile. *)
let make_full_cfg () : Runtime_config.t =
  let json =
    Yojson.Safe.from_string
      {|{"channels": {"teams": {"app_id": "test-app", "app_secret": "secret", "tenant_id": "tenant", "webhook_path": "/webhook", "service_url": "https://smba.trafficmanager.net"}}, "room_profiles": [{"id": "my-profile", "model": "openai:gpt-5.4", "system_prompt": "", "max_tool_iterations": 25, "status": "active"}], "room_profile_bindings": [{"profile_id": "my-profile", "room": "19:abc@thread.tacv2", "active": true}]}|}
  in
  Config_loader.parse_config json

(** Helper to make a config with non-default profile values. *)
let make_full_cfg_custom_model () : Runtime_config.t =
  let json =
    Yojson.Safe.from_string
      {|{"channels": {"teams": {"app_id": "test-app", "app_secret": "secret", "tenant_id": "tenant", "webhook_path": "/webhook", "service_url": "https://smba.trafficmanager.net"}}, "room_profiles": [{"id": "my-profile", "model": "anthropic:claude-opus-4-6", "system_prompt": "Be helpful", "max_tool_iterations": 10, "status": "active"}], "room_profile_bindings": [{"profile_id": "my-profile", "room": "19:abc@thread.tacv2", "active": true}]}|}
  in
  Config_loader.parse_config json

(** Helper to make a config with access bundles. *)
let make_cfg_with_bundles () : Runtime_config.t =
  let json =
    Yojson.Safe.from_string
      {|{"access_bundles": [{"id": "bundle-1", "status": "active"}]}|}
  in
  Config_loader.parse_config json

let test_rerun_all_already_valid () =
  (* When the desired state matches existing config, everything should be Already_valid *)
  let cfg = make_full_cfg () in
  let state =
    {
      default_state with
      profile_id = "my-profile";
      model = "openai:gpt-5.4";
      max_tool_iterations = 25;
      connector_type = "teams";
      connector_room = "19:abc@thread.tacv2";
      connector_active = true;
    }
  in
  let report = generate_rerun_report ~cfg ~state in
  (* All items should be Already_valid *)
  let all_valid =
    List.for_all (fun (item : rerun_item) -> item.status = Already_valid) report
  in
  Alcotest.(check bool) "all items already valid" true all_valid;
  Alcotest.(check bool) "report is non-empty" true (report <> [])

let test_rerun_profile_changed () =
  (* When model differs, should be Changed *)
  let cfg = make_full_cfg () in
  let state =
    {
      default_state with
      profile_id = "my-profile";
      model = "anthropic:claude-opus-4-6";
      (* Different model *)
      max_tool_iterations = 25;
      connector_type = "teams";
      connector_room = "19:abc@thread.tacv2";
      connector_active = true;
    }
  in
  let report = generate_rerun_report ~cfg ~state in
  let model_item =
    List.find_opt (fun (item : rerun_item) -> item.field = "model") report
  in
  match model_item with
  | None -> Alcotest.fail "expected model item in report"
  | Some item ->
      Alcotest.(check bool) "model is changed" true (item.status = Changed)

let test_rerun_profile_not_exists () =
  (* When profile doesn't exist yet, should report it will be created *)
  let cfg = make_empty_cfg () in
  let state =
    {
      default_state with
      profile_id = "new-profile";
      connector_type = "teams";
      connector_room = "";
    }
  in
  let report = generate_rerun_report ~cfg ~state in
  let existence_item =
    List.find_opt (fun (item : rerun_item) -> item.field = "existence") report
  in
  match existence_item with
  | None -> Alcotest.fail "expected existence item in report"
  | Some item ->
      Alcotest.(check bool) "existence is changed" true (item.status = Changed);
      Alcotest.(check bool)
        "details mentions created" true
        (String.length item.details > 0)

let test_rerun_missing_access_bundle () =
  (* When access bundles don't exist, should be Blocked *)
  let cfg = make_empty_cfg () in
  let state =
    {
      default_state with
      profile_id = "test-profile";
      access_bundle_ids = [ "nonexistent-bundle" ];
      connector_type = "teams";
      connector_room = "";
    }
  in
  let report = generate_rerun_report ~cfg ~state in
  let bundle_item =
    List.find_opt
      (fun (item : rerun_item) -> item.category = "Access Bundle")
      report
  in
  match bundle_item with
  | None -> Alcotest.fail "expected access bundle item in report"
  | Some item ->
      Alcotest.(check bool) "bundle is blocked" true (item.status = Blocked)

let test_rerun_valid_access_bundle () =
  (* When access bundles exist, should be Changed or Already_valid *)
  let cfg = make_cfg_with_bundles () in
  let state =
    {
      default_state with
      profile_id = "test-profile";
      access_bundle_ids = [ "bundle-1" ];
      connector_type = "teams";
      connector_room = "";
    }
  in
  let report = generate_rerun_report ~cfg ~state in
  let bundle_item =
    List.find_opt
      (fun (item : rerun_item) -> item.category = "Access Bundle")
      report
  in
  match bundle_item with
  | None -> Alcotest.fail "expected access bundle item in report"
  | Some item ->
      Alcotest.(check bool) "bundle is not blocked" true (item.status <> Blocked)

let test_rerun_connector_not_configured () =
  (* When connector is not configured, should be Blocked *)
  let cfg = make_empty_cfg () in
  let state =
    {
      default_state with
      profile_id = "test-profile";
      connector_type = "teams";
      connector_room = "19:abc@thread.tacv2";
      connector_active = true;
    }
  in
  let report = generate_rerun_report ~cfg ~state in
  let connector_item =
    List.find_opt
      (fun (item : rerun_item) -> item.category = "Connector")
      report
  in
  match connector_item with
  | None -> Alcotest.fail "expected connector item in report"
  | Some item ->
      Alcotest.(check bool) "connector is blocked" true (item.status = Blocked)

let test_rerun_invalid_room_format () =
  (* When room format is invalid, should be Manual_repair *)
  let cfg = make_teams_cfg () in
  let state =
    {
      default_state with
      profile_id = "test-profile";
      connector_type = "teams";
      connector_room = "invalid-room";
      connector_active = true;
    }
  in
  let report = generate_rerun_report ~cfg ~state in
  let connector_item =
    List.find_opt
      (fun (item : rerun_item) -> item.category = "Connector")
      report
  in
  match connector_item with
  | None -> Alcotest.fail "expected connector item in report"
  | Some item ->
      Alcotest.(check bool)
        "room is manual_repair" true
        (item.status = Manual_repair)

let test_rerun_display_report_counts () =
  (* Test that display_rerun_report returns correct counts *)
  let items : rerun_item list =
    [
      { category = "A"; field = "x"; status = Changed; details = "d1" };
      { category = "A"; field = "y"; status = Already_valid; details = "d2" };
      { category = "B"; field = "z"; status = Blocked; details = "d3" };
      { category = "B"; field = "w"; status = Manual_repair; details = "d4" };
      { category = "C"; field = "v"; status = Changed; details = "d5" };
    ]
  in
  let changed, blocked, manual = display_rerun_report items in
  Alcotest.(check int) "changed count" 2 changed;
  Alcotest.(check int) "blocked count" 1 blocked;
  Alcotest.(check int) "manual count" 1 manual

let test_rerun_empty_report () =
  let changed, blocked, manual = display_rerun_report [] in
  Alcotest.(check int) "changed count" 0 changed;
  Alcotest.(check int) "blocked count" 0 blocked;
  Alcotest.(check int) "manual count" 0 manual

let test_rerun_connector_binding_changed () =
  (* When connector binding exists but with different profile, should be Changed *)
  let cfg = make_full_cfg () in
  let state =
    {
      default_state with
      profile_id = "different-profile";
      model = "openai:gpt-5.4";
      max_tool_iterations = 25;
      connector_type = "teams";
      connector_room = "19:abc@thread.tacv2";
      connector_active = true;
    }
  in
  let report = generate_rerun_report ~cfg ~state in
  let connector_item =
    List.find_opt
      (fun (item : rerun_item) -> item.category = "Connector")
      report
  in
  match connector_item with
  | None -> Alcotest.fail "expected connector item in report"
  | Some item ->
      Alcotest.(check bool) "binding is changed" true (item.status = Changed)

let test_rerun_default_values_preserve_existing () =
  (* When CLI defaults are used (model=default, max_iters=25), they should
     preserve existing non-default values and not report as Changed.
     This is the key test: existing config has custom model "anthropic:claude-opus-4-6"
     and max_iters=10, but CLI passes defaults. The report should show
     Already_valid because apply_plan would preserve the existing values. *)
  let cfg = make_full_cfg_custom_model () in
  let state =
    {
      default_state with
      profile_id = "my-profile";
      (* model and max_iters are at defaults - should preserve existing *)
      model = "openai:gpt-5.4";
      max_tool_iterations = 25;
      connector_type = "teams";
      connector_room = "19:abc@thread.tacv2";
      connector_active = true;
    }
  in
  let report = generate_rerun_report ~cfg ~state in
  let model_item =
    List.find_opt (fun (item : rerun_item) -> item.field = "model") report
  in
  let iters_item =
    List.find_opt (fun (item : rerun_item) -> item.field = "max_iters") report
  in
  (* Both should be Already_valid since defaults preserve existing values *)
  (match model_item with
  | None -> Alcotest.fail "expected model item in report"
  | Some item ->
      Alcotest.(check bool)
        "model is already_valid (preserves anthropic:claude-opus-4-6)" true
        (item.status = Already_valid));
  match iters_item with
  | None -> Alcotest.fail "expected max_iters item in report"
  | Some item ->
      Alcotest.(check bool)
        "max_iters is already_valid (preserves 10)" true
        (item.status = Already_valid)

(** {1 Budget state readiness check tests} *)

let test_readiness_budget_state_no_db () =
  let cfg = make_empty_cfg () in
  let state =
    {
      default_state with
      profile_id = "test-profile";
      token_limit = 1000;
      cost_limit_usd = 10.0;
    }
  in
  let checks = run_readiness_checks ~cfg ~db:None ~state in
  (* Without DB, budget state check should not appear *)
  let budget_state_check =
    List.find_opt (fun c -> c.name = "Budget State") checks
  in
  Alcotest.(check bool)
    "no budget state check without db" true
    (Option.is_none budget_state_check)

let test_readiness_budget_denial_message () =
  let cfg = make_empty_cfg () in
  let state =
    {
      default_state with
      profile_id = "test-profile";
      token_limit = 1000;
      cost_limit_usd = 10.0;
    }
  in
  let checks = run_readiness_checks ~cfg ~db:None ~state in
  let denial_check =
    List.find_opt (fun c -> c.name = "Budget Denial Msg") checks
  in
  match denial_check with
  | None -> Alcotest.fail "expected budget denial msg check"
  | Some check ->
      Alcotest.(check bool) "denial msg check passes" true check.passed;
      Alcotest.(check bool)
        "denial msg indicates safe" true
        (String_util.string_contains check.message "Redacted msg safe")

let test_readiness_budget_denial_no_leak () =
  let cfg = make_empty_cfg () in
  let state =
    {
      default_state with
      profile_id = "test-profile";
      token_limit = 12345;
      cost_limit_usd = 99.99;
    }
  in
  let checks = run_readiness_checks ~cfg ~db:None ~state in
  let denial_check =
    List.find_opt (fun c -> c.name = "Budget Denial Msg") checks
  in
  match denial_check with
  | None -> Alcotest.fail "expected budget denial msg check"
  | Some check ->
      Alcotest.(check bool) "denial msg check passes" true check.passed;
      (* Redacted message should not leak token limit or cost *)
      Alcotest.(check bool)
        "no token limit in redacted" true
        (not (String_util.string_contains check.message "12345"));
      Alcotest.(check bool)
        "no cost limit in redacted" true
        (not (String_util.string_contains check.message "99.99"))

let test_readiness_budget_denial_no_budget () =
  let cfg = make_empty_cfg () in
  let state =
    {
      default_state with
      profile_id = "test-profile";
      token_limit = 0;
      cost_limit_usd = 0.0;
    }
  in
  let checks = run_readiness_checks ~cfg ~db:None ~state in
  let denial_check =
    List.find_opt (fun c -> c.name = "Budget Denial Msg") checks
  in
  Alcotest.(check bool)
    "no denial check when no budget" true
    (Option.is_none denial_check)

let test_readiness_budget_denial_cost_only () =
  let cfg = make_empty_cfg () in
  let state =
    {
      default_state with
      profile_id = "test-profile";
      token_limit = 0;
      cost_limit_usd = 25.0;
    }
  in
  let checks = run_readiness_checks ~cfg ~db:None ~state in
  let denial_check =
    List.find_opt (fun c -> c.name = "Budget Denial Msg") checks
  in
  match denial_check with
  | None -> Alcotest.fail "expected budget denial msg check for cost-only"
  | Some check ->
      Alcotest.(check bool) "cost-only denial msg passes" true check.passed;
      Alcotest.(check bool)
        "cost-only msg indicates safe" true
        (String_util.string_contains check.message "Redacted msg safe")

let test_readiness_budget_state_with_db () =
  let cfg = make_empty_cfg () in
  let db = Memory.init ~db_path:":memory:" () in
  let profile_id = Memory.insert_room_profile ~db ~name:"budget-test-profile" in
  Room_budget.init_schema db;
  Room_budget.init_profile_budget ~db ~profile_id ~token_limit:1000
    ~cost_limit_usd:10.0 ~reset_period:"monthly"
    ~period_started_at:"2026-01-01 00:00:00" ();
  (* Record some usage *)
  Request_stats.record ~db ~session_key:"test-session" ~profile_id
    ~provider:"openai" ~model:"gpt-5.4" ~prompt_tokens:300
    ~completion_tokens:200 ~cost_usd:2.50 ();
  let state =
    {
      default_state with
      profile_id = "budget-test-profile";
      token_limit = 1000;
      cost_limit_usd = 10.0;
    }
  in
  let checks = run_readiness_checks ~cfg ~db:(Some db) ~state in
  let budget_state_check =
    List.find_opt (fun c -> c.name = "Budget State") checks
  in
  match budget_state_check with
  | None -> Alcotest.fail "expected budget state check with db"
  | Some check ->
      Alcotest.(check bool) "budget state passes" true check.passed;
      Alcotest.(check bool)
        "shows token usage" true
        (String_util.string_contains check.message "500");
      Alcotest.(check bool)
        "shows token limit" true
        (String_util.string_contains check.message "1000");
      Alcotest.(check bool)
        "shows cost usage" true
        (String_util.string_contains check.message "2.50");
      Alcotest.(check bool)
        "shows period" true
        (String_util.string_contains check.message "monthly");
      Alcotest.(check bool)
        "shows soft threshold" true
        (String_util.string_contains check.message "80%")

let test_readiness_budget_state_exceeded () =
  let cfg = make_empty_cfg () in
  let db = Memory.init ~db_path:":memory:" () in
  let profile_id =
    Memory.insert_room_profile ~db ~name:"budget-exceeded-profile"
  in
  Room_budget.init_schema db;
  Room_budget.init_profile_budget ~db ~profile_id ~token_limit:100
    ~cost_limit_usd:1.0 ~reset_period:"daily"
    ~period_started_at:"2026-01-01 00:00:00" ();
  (* Exceed the budget *)
  Request_stats.record ~db ~session_key:"test-session" ~profile_id
    ~provider:"openai" ~model:"gpt-5.4" ~prompt_tokens:80 ~completion_tokens:30
    ~cost_usd:1.20 ();
  let state =
    {
      default_state with
      profile_id = "budget-exceeded-profile";
      token_limit = 100;
      cost_limit_usd = 1.0;
    }
  in
  let checks = run_readiness_checks ~cfg ~db:(Some db) ~state in
  let budget_state_check =
    List.find_opt (fun c -> c.name = "Budget State") checks
  in
  match budget_state_check with
  | None -> Alcotest.fail "expected budget state check"
  | Some check ->
      Alcotest.(check bool)
        "budget state fails when exceeded" false check.passed;
      Alcotest.(check bool)
        "shows hard limit exceeded" true
        (String_util.string_contains check.message "HARD LIMIT EXCEEDED")

let test_readiness_budget_state_soft_warning () =
  let cfg = make_empty_cfg () in
  let db = Memory.init ~db_path:":memory:" () in
  let profile_id = Memory.insert_room_profile ~db ~name:"budget-soft-profile" in
  Room_budget.init_schema db;
  Room_budget.clear_all_soft_warn_debounce ();
  Room_budget.init_profile_budget ~db ~profile_id ~token_limit:100
    ~cost_limit_usd:10.0 ~reset_period:"daily"
    ~period_started_at:"2026-01-01 00:00:00" ();
  (* 85% usage - above 80% soft threshold but below 100% hard limit *)
  Request_stats.record ~db ~session_key:"test-session" ~profile_id
    ~provider:"openai" ~model:"gpt-5.4" ~prompt_tokens:50 ~completion_tokens:35
    ~cost_usd:0.40 ();
  let state =
    {
      default_state with
      profile_id = "budget-soft-profile";
      token_limit = 100;
      cost_limit_usd = 10.0;
    }
  in
  let checks = run_readiness_checks ~cfg ~db:(Some db) ~state in
  let budget_state_check =
    List.find_opt (fun c -> c.name = "Budget State") checks
  in
  match budget_state_check with
  | None -> Alcotest.fail "expected budget state check"
  | Some check ->
      Alcotest.(check bool) "budget state passes (soft only)" true check.passed;
      Alcotest.(check bool)
        "shows soft limit warning" true
        (String_util.string_contains check.message "SOFT LIMIT WARNING")

(** {1 Suite} *)

let suite =
  [
    Alcotest.test_case "connector_is_configured" `Quick
      test_connector_is_configured;
    Alcotest.test_case "configured_connectors" `Quick test_configured_connectors;
    Alcotest.test_case "default_connector" `Quick test_default_connector;
    Alcotest.test_case "validate_teams_room_id" `Quick
      test_validate_teams_room_id;
    Alcotest.test_case "validate_slack_room_id" `Quick
      test_validate_slack_room_id;
    Alcotest.test_case "validate_room_id_for_connector" `Quick
      test_validate_room_id_for_connector;
    Alcotest.test_case "compare_teams_vs_slack" `Quick
      test_compare_teams_vs_slack;
    Alcotest.test_case "plan_includes_connector" `Quick
      test_plan_includes_connector;
    Alcotest.test_case "plan_slack_connector" `Quick test_plan_slack_connector;
    Alcotest.test_case "connector_available" `Quick
      test_readiness_connector_available;
    Alcotest.test_case "room_validation" `Quick test_readiness_room_validation;
    Alcotest.test_case "default_state_is_teams" `Quick
      test_default_state_is_teams;
    Alcotest.test_case "rerun_all_already_valid" `Quick
      test_rerun_all_already_valid;
    Alcotest.test_case "rerun_profile_changed" `Quick test_rerun_profile_changed;
    Alcotest.test_case "rerun_profile_not_exists" `Quick
      test_rerun_profile_not_exists;
    Alcotest.test_case "rerun_missing_access_bundle" `Quick
      test_rerun_missing_access_bundle;
    Alcotest.test_case "rerun_valid_access_bundle" `Quick
      test_rerun_valid_access_bundle;
    Alcotest.test_case "rerun_connector_not_configured" `Quick
      test_rerun_connector_not_configured;
    Alcotest.test_case "rerun_invalid_room_format" `Quick
      test_rerun_invalid_room_format;
    Alcotest.test_case "rerun_display_report_counts" `Quick
      test_rerun_display_report_counts;
    Alcotest.test_case "rerun_empty_report" `Quick test_rerun_empty_report;
    Alcotest.test_case "rerun_connector_binding_changed" `Quick
      test_rerun_connector_binding_changed;
    Alcotest.test_case "rerun_default_values_preserve_existing" `Quick
      test_rerun_default_values_preserve_existing;
    Alcotest.test_case "readiness_budget_state_no_db" `Quick
      test_readiness_budget_state_no_db;
    Alcotest.test_case "readiness_budget_denial_message" `Quick
      test_readiness_budget_denial_message;
    Alcotest.test_case "readiness_budget_denial_no_leak" `Quick
      test_readiness_budget_denial_no_leak;
    Alcotest.test_case "readiness_budget_denial_no_budget" `Quick
      test_readiness_budget_denial_no_budget;
    Alcotest.test_case "readiness_budget_denial_cost_only" `Quick
      test_readiness_budget_denial_cost_only;
    Alcotest.test_case "readiness_budget_state_with_db" `Quick
      test_readiness_budget_state_with_db;
    Alcotest.test_case "readiness_budget_state_exceeded" `Quick
      test_readiness_budget_state_exceeded;
    Alcotest.test_case "readiness_budget_state_soft_warning" `Quick
      test_readiness_budget_state_soft_warning;
  ]

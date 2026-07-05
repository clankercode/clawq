(* setup_room_wizard.ml — Room-agent pilot wizard with plan/apply flow

   Supports configuring:
   - Room profile (id, model, system prompt, tool restrictions)
   - Access bundle binding
   - Memory scope
   - Budget limits
   - Connector binding
   - Readiness checks

   Plan mode: shows what would happen without side effects.
   Apply mode: makes the actual changes. *)

open Runtime_config_types
include Setup_room_wizard_types
include Setup_room_wizard_connectors

(* ── Validation ─────────────────────────────────────────────────── *)

let validate_profile_id s =
  if s = "" then Error "Profile ID cannot be empty."
  else if String.length s > 64 then Error "Profile ID must be 64 chars or less."
  else if
    not
      (String.for_all
         (fun c ->
           (c >= 'a' && c <= 'z')
           || (c >= '0' && c <= '9')
           || c = '-' || c = '_')
         s)
  then
    Error "Profile ID must be lowercase alphanumeric with hyphens/underscores."
  else Ok s

let validate_model s = if s = "" then Error "Model cannot be empty." else Ok s

let validate_max_iters s =
  match int_of_string_opt s with
  | Some n when n > 0 && n <= 1000 -> Ok s
  | Some _ -> Error "Max tool iterations must be between 1 and 1000."
  | None -> Error "Must be a valid integer."

let validate_token_limit s =
  match int_of_string_opt s with
  | Some n when n >= 0 -> Ok s
  | Some _ -> Error "Token limit must be non-negative."
  | None -> Error "Must be a valid integer."

let validate_cost_limit s =
  match float_of_string_opt s with
  | Some f when f >= 0.0 -> Ok s
  | Some _ -> Error "Cost limit must be non-negative."
  | None -> Error "Must be a valid number."

let validate_budget_period s =
  match s with
  | "daily" | "weekly" | "monthly" | "yearly" -> Ok s
  | _ -> Error "Period must be: daily, weekly, monthly, or yearly."

(* -- Teams vs Slack capability comparison ------------------------------- *)

type comparison_row = {
  feature : string;
  teams_value : string;
  slack_value : string;
}

let compare_teams_vs_slack () : comparison_row list =
  let tc = Connector_capabilities.teams in
  let sc = Connector_capabilities.slack in
  let yes_no b = if b then "Yes" else "No" in
  let edit_str = function
    | Connector_capabilities.Edit_in_place -> "In-place"
    | Delete_and_resend -> "Delete+resend"
    | No_edit -> "None"
  in
  let thread_str = function
    | Connector_capabilities.Native_thread_replies -> "Native"
    | Thread_like_replies -> "Thread-like"
    | No_thread_replies -> "None"
  in
  [
    {
      feature = "Edit messages";
      teams_value = edit_str tc.can_edit;
      slack_value = edit_str sc.can_edit;
    };
    {
      feature = "Delete messages";
      teams_value = yes_no tc.can_delete;
      slack_value = yes_no sc.can_delete;
    };
    {
      feature = "Reactions";
      teams_value = yes_no tc.can_react;
      slack_value = yes_no sc.can_react;
    };
    {
      feature = "Typing indicator";
      teams_value = yes_no tc.can_type;
      slack_value = yes_no sc.can_type;
    };
    {
      feature = "Status updates";
      teams_value = yes_no tc.can_show_status;
      slack_value = yes_no sc.can_show_status;
    };
    {
      feature = "File sending";
      teams_value = yes_no tc.can_send_files;
      slack_value = yes_no sc.can_send_files;
    };
    {
      feature = "Adaptive Cards";
      teams_value = yes_no tc.can_send_cards;
      slack_value = yes_no sc.can_send_cards;
    };
    {
      feature = "Buttons";
      teams_value = yes_no tc.can_send_buttons;
      slack_value = yes_no sc.can_send_buttons;
    };
    {
      feature = "Thread replies";
      teams_value = thread_str tc.thread_replies;
      slack_value = thread_str sc.thread_replies;
    };
    {
      feature = "Rich questions";
      teams_value = yes_no (Connector_capabilities.supports_rich_questions tc);
      slack_value = yes_no (Connector_capabilities.supports_rich_questions sc);
    };
    {
      feature = "Max message length";
      teams_value = string_of_int tc.max_message_length;
      slack_value = string_of_int sc.max_message_length;
    };
  ]

let display_capability_comparison () =
  let open Setup_common in
  let rows = compare_teams_vs_slack () in
  Printf.printf "\n%s\n" (bold "=== Teams vs Slack Capability Comparison ===");
  Printf.printf "\n";
  Printf.printf "  %-22s  %-16s  %-16s\n" "Feature" "Teams" "Slack";
  Printf.printf "  %-22s  %-16s  %-16s\n" (String.make 22 '-')
    (String.make 16 '-') (String.make 16 '-');
  List.iter
    (fun (r : comparison_row) ->
      Printf.printf "  %-22s  %-16s  %-16s\n" r.feature r.teams_value
        r.slack_value)
    rows;
  Printf.printf "\n";
  Printf.printf "  %s\n"
    (dim "Teams: Adaptive Cards, rich questions, file consent, typing.");
  Printf.printf "  %s\n"
    (dim "Slack: reactions, native threads, ambient history capture.");
  Printf.printf "\n"

(* ── Plan generation ────────────────────────────────────────────── *)

let generate_plan ~(cfg : Runtime_config.t) ~(state : wizard_state) :
    plan_item list =
  let items = ref [] in
  let add cat action details =
    items := { category = cat; action; details } :: !items
  in

  (* Room profile *)
  let existing_profile =
    List.find_opt
      (fun (p : room_profile) -> p.id = state.profile_id)
      cfg.room_profiles
  in
  (match existing_profile with
  | Some _ ->
      add "Room Profile" "update"
        (Printf.sprintf "Update profile '%s': model=%s, max_iters=%d"
           state.profile_id state.model state.max_tool_iterations)
  | None ->
      add "Room Profile" "create"
        (Printf.sprintf "Create profile '%s': model=%s, max_iters=%d"
           state.profile_id state.model state.max_tool_iterations));

  if state.display_name <> "" then
    add "Room Profile" "set-display-name" state.display_name;

  if state.system_prompt <> "" then
    add "Room Profile" "set-system-prompt"
      (Printf.sprintf "(%d chars)" (String.length state.system_prompt));

  if state.allowed_tools <> [] then
    add "Room Profile" "set-allowed-tools"
      (String.concat ", " state.allowed_tools);

  if state.denied_tools <> [] then
    add "Room Profile" "set-denied-tools"
      (String.concat ", " state.denied_tools);

  (* Access bundles *)
  let missing_bundles =
    List.filter
      (fun id ->
        not
          (List.exists
             (fun (b : access_bundle) -> b.id = id && b.status <> "deleted")
             cfg.access_bundles))
      state.access_bundle_ids
  in
  if missing_bundles <> [] then
    add "Access Bundle" "warning"
      (Printf.sprintf "Bundles not found: %s"
         (String.concat ", " missing_bundles))
  else if state.access_bundle_ids <> [] then
    add "Access Bundle" "bind"
      (Printf.sprintf "Bind bundles: %s"
         (String.concat ", " state.access_bundle_ids));

  (* Memory scope *)
  if state.memory_scope_key <> "" then
    add "Memory Scope" "configure"
      (Printf.sprintf "kind=%s, key=%s" state.memory_scope_kind
         state.memory_scope_key);

  (* Budget *)
  if state.token_limit > 0 || state.cost_limit_usd > 0.0 then
    add "Budget" "configure"
      (Printf.sprintf "tokens=%d, cost=$%.2f, period=%s" state.token_limit
         state.cost_limit_usd state.budget_reset_period);

  (* Connector binding *)
  if state.connector_room <> "" then begin
    let connector_label =
      match state.connector_type with
      | "teams" -> "Teams"
      | "slack" -> "Slack"
      | "discord" -> "Discord"
      | "telegram" -> "Telegram"
      | c -> c
    in
    add "Connector"
      (if state.connector_type = "teams" then "primary" else "bind")
      connector_label;
    add "Connector Binding"
      (if state.connector_active then "bind" else "bind-inactive")
      (Printf.sprintf "room=%s, active=%b" state.connector_room
         state.connector_active)
  end;

  List.rev !items

(* ── Readiness checks ───────────────────────────────────────────── *)

let run_readiness_checks ~(cfg : Runtime_config.t) ~(db : Sqlite3.db option)
    ~(state : wizard_state) : readiness_check list =
  let checks = ref [] in
  let add name passed message =
    checks := { name; passed; message } :: !checks
  in

  (* Check profile ID is valid *)
  (match validate_profile_id state.profile_id with
  | Ok _ -> add "Profile ID" true "Valid"
  | Error e -> add "Profile ID" false e);

  (* Check model is set *)
  add "Model" (state.model <> "")
    (if state.model = "" then "Model is required" else state.model);

  (* Check access bundles exist *)
  let missing_bundles =
    List.filter
      (fun id ->
        not
          (List.exists
             (fun (b : access_bundle) -> b.id = id && b.status <> "deleted")
             cfg.access_bundles))
      state.access_bundle_ids
  in
  add "Access Bundles" (missing_bundles = [])
    (if missing_bundles = [] then "All bundles found"
     else Printf.sprintf "Missing: %s" (String.concat ", " missing_bundles));

  (* Check connector room format *)
  if state.connector_room <> "" then begin
    let cfg_connectors = configured_connectors cfg in
    let connector_available = List.mem state.connector_type cfg_connectors in
    add "Connector Available"
      (state.connector_type = "" || connector_available)
      (if state.connector_type = "" then "No connector specified"
       else if connector_available then
         Printf.sprintf "%s configured" state.connector_type
       else
         Printf.sprintf "%s not configured (available: %s)" state.connector_type
           (if cfg_connectors = [] then "none"
            else String.concat ", " cfg_connectors))
  end;

  let room_valid =
    if state.connector_room = "" then true
    else
      match
        validate_room_id_for_connector state.connector_type state.connector_room
      with
      | Ok _ -> true
      | Error _ -> false
  in
  let room_msg =
    if state.connector_room = "" then "No connector configured"
    else
      match
        validate_room_id_for_connector state.connector_type state.connector_room
      with
      | Ok _ ->
          Printf.sprintf "%s room: %s" state.connector_type state.connector_room
      | Error e -> e
  in
  add "Connector Room" room_valid room_msg;

  (* Check budget consistency *)
  add "Budget"
    (state.token_limit >= 0 && state.cost_limit_usd >= 0.0)
    (if state.token_limit >= 0 && state.cost_limit_usd >= 0.0 then "Valid"
     else "Limits must be non-negative");

  (* Check existing budget state when DB is available and profile exists *)
  (match db with
  | Some db when state.profile_id <> "" -> (
      match Memory_core.get_room_profile_by_name ~db ~name:state.profile_id with
      | Some rp -> (
          match Room_budget.get_profile_budget ~db ~profile_id:rp.id with
          | Some budget_state ->
              let usage_pct tokens limit =
                if limit > 0 then
                  Float.of_int tokens /. Float.of_int limit *. 100.0
                else 0.0
              in
              let token_pct =
                usage_pct budget_state.current_usage.total_tokens
                  budget_state.token_limit
              in
              let cost_pct =
                usage_pct
                  (int_of_float (budget_state.current_usage.cost_usd *. 100.0))
                  (int_of_float (budget_state.cost_limit_usd *. 100.0))
              in
              let status_msg =
                Printf.sprintf
                  "tokens: %d/%d (%.1f%%), cost: $%.4f/$%.2f (%.1f%%), period: \
                   %s, soft threshold: %.0f%%"
                  budget_state.current_usage.total_tokens
                  budget_state.token_limit token_pct
                  budget_state.current_usage.cost_usd
                  budget_state.cost_limit_usd cost_pct budget_state.reset_period
                  (budget_state.soft_warn_threshold_pct *. 100.0)
              in
              let limit_msg =
                if budget_state.limit_exceeded then " [HARD LIMIT EXCEEDED]"
                else if budget_state.soft_limit_exceeded then
                  " [SOFT LIMIT WARNING]"
                else ""
              in
              add "Budget State"
                (not budget_state.limit_exceeded)
                (status_msg ^ limit_msg)
          | None ->
              add "Budget State" true "No budget configured for this profile")
      | None ->
          add "Budget State" true
            "Profile not yet in DB (will be created on apply)")
  | _ -> ());

  (* Validate budget denial message redaction when budget is configured *)
  if state.token_limit > 0 || state.cost_limit_usd > 0.0 then begin
    let test_state : Room_budget.state =
      {
        profile_id = 0;
        token_limit = state.token_limit;
        cost_limit_usd = state.cost_limit_usd;
        current_usage =
          {
            prompt_tokens = 0;
            completion_tokens = 0;
            total_tokens = 0;
            cost_usd = 0.0;
            turns = 0;
          };
        reset_period = state.budget_reset_period;
        period_started_at = "";
        token_limit_exceeded = true;
        cost_limit_exceeded = true;
        limit_exceeded = true;
        soft_warn_threshold_pct = 0.8;
        soft_limit_exceeded = false;
        created_at = "";
        updated_at = "";
      }
    in
    let redacted_msg =
      Room_budget.budget_exceeded_message_redacted test_state
    in
    let token_leak =
      state.token_limit > 0
      && String_util.string_contains redacted_msg
           (string_of_int state.token_limit)
    in
    let cost_leak =
      state.cost_limit_usd > 0.0
      && String_util.string_contains redacted_msg
           (string_of_float state.cost_limit_usd)
    in
    let no_leak =
      not
        (token_leak || cost_leak
        || String_util.string_contains redacted_msg "USD"
        || String_util.string_contains redacted_msg "limits")
    in
    let has_profile_id =
      String_util.string_contains redacted_msg "budget exceeded"
    in
    add "Budget Denial Msg"
      (no_leak && has_profile_id)
      (if not no_leak then
         Printf.sprintf "Redacted message leaks sensitive budget details: %s"
           redacted_msg
       else if not has_profile_id then
         Printf.sprintf "Redacted message missing budget exceeded indicator: %s"
           redacted_msg
       else Printf.sprintf "Redacted msg safe (no details leaked)")
  end;

  (* Check max tool iterations *)
  add "Max Tool Iterations"
    (state.max_tool_iterations > 0 && state.max_tool_iterations <= 1000)
    (if state.max_tool_iterations <= 0 then "Must be positive"
     else if state.max_tool_iterations > 1000 then "Must be <= 1000"
     else string_of_int state.max_tool_iterations);

  (* Check budget reset period *)
  let valid_periods = [ "daily"; "weekly"; "monthly"; "yearly" ] in
  add "Budget Reset Period"
    (List.mem state.budget_reset_period valid_periods)
    (if List.mem state.budget_reset_period valid_periods then
       state.budget_reset_period
     else "Must be: daily, weekly, monthly, or yearly");

  (* GitHub App and repo grant checks *)
  let gh_token_ok, gh_token_msg =
    Github_wizard_checks.check_github_app_token cfg
  in
  add "GitHub App" gh_token_ok gh_token_msg;

  let rg_ok, rg_msg = Github_wizard_checks.check_repo_grants cfg in
  add "Repo Grants" rg_ok rg_msg;

  let wh_ok, wh_msg = Github_wizard_checks.check_webhook_reachability cfg in
  add "Webhook Config" wh_ok wh_msg;

  let rb_ok, rb_msg =
    Github_wizard_checks.check_room_backlink ~cfg ~profile_id:state.profile_id
      ~access_bundle_ids:state.access_bundle_ids
  in
  add "Room Backlink" rb_ok rb_msg;

  (* Audit/ledger/delivery visibility checks *)
  (match db with
  | Some db -> (
      (* Verify room activity ledger is queryable *)
      (try
         let _events =
           Room_activity_ledger.query ~db ~room_id:"__wizard_probe" ()
         in
         add "Activity Ledger" true "Schema accessible"
       with exn ->
         add "Activity Ledger" false
           (Printf.sprintf "Ledger query failed: %s" (Printexc.to_string exn)));
      (* Verify egress audit is queryable *)
      try
        let _events = Egress_audit.query ~db ~limit:1 () in
        add "Egress Audit" true "Schema accessible"
      with exn ->
        add "Egress Audit" false
          (Printf.sprintf "Egress audit query failed: %s"
             (Printexc.to_string exn)))
  | None ->
      add "Activity Ledger" true "skip (no DB)";
      add "Egress Audit" true "skip (no DB)");

  List.rev !checks

(* ── Plan display ───────────────────────────────────────────────── *)

let display_plan (items : plan_item list) =
  let open Setup_common in
  Printf.printf "\n%s\n" (bold "=== Execution Plan ===");
  Printf.printf "\n";
  if items = [] then Printf.printf "  %s\n" (dim "(no changes)")
  else begin
    let last_cat = ref "" in
    List.iter
      (fun (item : plan_item) ->
        if item.category <> !last_cat then begin
          Printf.printf "\n  %s\n" (bold item.category);
          last_cat := item.category
        end;
        let action_str =
          match item.action with
          | "create" -> green "+ create"
          | "update" -> cyan "~ update"
          | "warning" -> yellow "! warning"
          | "bind" | "bind-inactive" -> cyan "~ bind"
          | _ -> item.action
        in
        Printf.printf "    %s  %s\n" action_str item.details)
      items
  end;
  Printf.printf "\n"

let display_readiness (checks : readiness_check list) =
  let open Setup_common in
  Printf.printf "\n%s\n" (bold "=== Readiness Checks ===");
  Printf.printf "\n";
  let all_passed = List.for_all (fun c -> c.passed) checks in
  List.iter
    (fun check ->
      let icon = if check.passed then green "PASS" else red "FAIL" in
      Printf.printf "  [%s] %s: %s\n" icon check.name check.message)
    checks;
  Printf.printf "\n";
  all_passed

include Setup_room_wizard_rerun

(* ── Apply logic ────────────────────────────────────────────────── *)

let apply_plan ~(db : Sqlite3.db) ~(cfg : Runtime_config.t)
    ~(state : wizard_state) : (string, string) result =
  (* Build the new/updated profile, merging with existing data *)
  let existing_profile =
    List.find_opt
      (fun (p : room_profile) -> p.id = state.profile_id)
      cfg.room_profiles
  in
  let new_profile : room_profile =
    match existing_profile with
    | Some existing ->
        (* Merge: only override fields that were explicitly set *)
        {
          id = state.profile_id;
          display_name =
            (if state.display_name = "" then existing.display_name
             else Some state.display_name);
          model =
            (if state.model = "openai:gpt-5.4" && existing.model <> "" then
               existing.model
             else state.model);
          system_prompt =
            (if state.system_prompt = "" then existing.system_prompt
             else state.system_prompt);
          max_tool_iterations =
            (if
               state.max_tool_iterations = 25
               && existing.max_tool_iterations > 0
             then existing.max_tool_iterations
             else state.max_tool_iterations);
          status = "active";
          allowed_tools =
            (if state.allowed_tools = [] then existing.allowed_tools
             else state.allowed_tools);
          denied_tools =
            (if state.denied_tools = [] then existing.denied_tools
             else state.denied_tools);
          access_bundle_ids =
            (if state.access_bundle_ids = [] then existing.access_bundle_ids
             else state.access_bundle_ids);
          ambient_enabled = existing.ambient_enabled;
          ambient_quiet_start = existing.ambient_quiet_start;
          ambient_quiet_end = existing.ambient_quiet_end;
          ambient_rate_limit_rph = existing.ambient_rate_limit_rph;
        }
    | None ->
        (* New profile *)
        {
          id = state.profile_id;
          display_name =
            (if state.display_name = "" then None else Some state.display_name);
          model = state.model;
          system_prompt = state.system_prompt;
          max_tool_iterations = state.max_tool_iterations;
          status = "active";
          allowed_tools = state.allowed_tools;
          denied_tools = state.denied_tools;
          access_bundle_ids = state.access_bundle_ids;
          ambient_enabled = false;
          ambient_quiet_start = 0;
          ambient_quiet_end = 0;
          ambient_rate_limit_rph = 0;
        }
  in
  (* Merge profiles *)
  let profiles =
    new_profile
    :: List.filter
         (fun (p : room_profile) -> p.id <> state.profile_id)
         cfg.room_profiles
  in
  (* Handle connector binding *)
  let bindings =
    if state.connector_room = "" then cfg.room_profile_bindings
    else
      let new_binding : room_profile_binding =
        {
          profile_id = state.profile_id;
          room = state.connector_room;
          active = state.connector_active;
        }
      in
      new_binding
      :: List.filter
           (fun (b : room_profile_binding) -> b.room <> state.connector_room)
           cfg.room_profile_bindings
  in
  (* Write config *)
  let profiles_json =
    `List
      (List.map
         (fun (p : room_profile) ->
           `Assoc
             ([
                ("id", `String p.id);
                ("model", `String p.model);
                ("system_prompt", `String p.system_prompt);
                ("max_tool_iterations", `Int p.max_tool_iterations);
                ("status", `String p.status);
              ]
             @ (if p.allowed_tools = [] then []
                else
                  [
                    ( "allowed_tools",
                      `List (List.map (fun t -> `String t) p.allowed_tools) );
                  ])
             @ (if p.denied_tools = [] then []
                else
                  [
                    ( "denied_tools",
                      `List (List.map (fun t -> `String t) p.denied_tools) );
                  ])
             @ (if p.access_bundle_ids = [] then []
                else
                  [
                    ( "access_bundle_ids",
                      `List
                        (List.map (fun id -> `String id) p.access_bundle_ids) );
                  ])
             @ (if p.ambient_enabled then
                  [
                    ("ambient_enabled", `Bool true);
                    ("ambient_quiet_start", `Int p.ambient_quiet_start);
                    ("ambient_quiet_end", `Int p.ambient_quiet_end);
                    ("ambient_rate_limit_rph", `Int p.ambient_rate_limit_rph);
                  ]
                else [])
             @
             match p.display_name with
             | Some name -> [ ("display_name", `String name) ]
             | None -> []))
         profiles)
  in
  let bindings_json =
    `List
      (List.map
         (fun (b : room_profile_binding) ->
           `Assoc
             [
               ("profile_id", `String b.profile_id);
               ("room", `String b.room);
               ("active", `Bool b.active);
             ])
         bindings)
  in
  let json =
    `Assoc
      [
        ("room_profiles", profiles_json);
        ("room_profile_bindings", bindings_json);
      ]
  in
  match Setup_common.merge_and_write_config json with
  | Error e -> Error (Printf.sprintf "Failed to write config: %s" e)
  | Ok path ->
      (* Reconcile config to DB to sync bindings using updated config *)
      let updated_cfg =
        { cfg with room_profiles = profiles; room_profile_bindings = bindings }
      in
      let reconcile_result =
        try
          let _issues =
            Room_profile_reconcile.reconcile_room_profiles ~db
              ~config:updated_cfg
          in
          Ok ()
        with exn ->
          Error
            (Printf.sprintf "DB reconciliation failed: %s"
               (Printexc.to_string exn))
      in
      (* Initialize budget if configured *)
      let budget_result =
        if state.token_limit > 0 || state.cost_limit_usd > 0.0 then
          try
            let db_profile_id =
              match
                Memory_core.get_room_profile_by_name ~db ~name:state.profile_id
              with
              | Some rp -> rp.id
              | None ->
                  Memory_core.insert_room_profile ~db ~name:state.profile_id
            in
            Room_budget.init_profile_budget ~db ~profile_id:db_profile_id
              ~token_limit:state.token_limit
              ~cost_limit_usd:state.cost_limit_usd
              ~reset_period:state.budget_reset_period ();
            Ok ()
          with exn ->
            Error
              (Printf.sprintf "Budget setup failed: %s" (Printexc.to_string exn))
        else Ok ()
      in
      (* Create memory scope if configured, with profile linkage *)
      let scope_result =
        if state.memory_scope_key <> "" then
          try
            let db_profile_id =
              match
                Memory_core.get_room_profile_by_name ~db ~name:state.profile_id
              with
              | Some rp -> Some rp.id
              | None ->
                  Some
                    (Memory_core.insert_room_profile ~db ~name:state.profile_id)
            in
            let _scope =
              Memory.create_scope ~db ~kind:state.memory_scope_kind
                ~key:state.memory_scope_key ?profile_id:db_profile_id
                ~provenance:"wizard" ()
            in
            Ok ()
          with exn ->
            Error
              (Printf.sprintf "Memory scope creation failed: %s"
                 (Printexc.to_string exn))
        else Ok ()
      in
      (* Collect any errors *)
      let errors =
        List.filter_map
          (function Ok () -> None | Error e -> Some e)
          [ reconcile_result; budget_result; scope_result ]
      in
      let base_msg =
        Printf.sprintf "Configuration saved to %s\nProfile '%s' %s." path
          state.profile_id
          (if existing_profile <> None then "updated" else "created")
      in
      if errors <> [] then
        Error (base_msg ^ "\nWarnings: " ^ String.concat "; " errors)
      else Ok base_msg

(* ── Interactive wizard ─────────────────────────────────────────── *)

let run_wizard () =
  match Setup_common.check_tty () with
  | Error e -> e
  | Ok () -> (
      let cfg = Command_bridge_helpers.get_config () in
      let db = Command_bridge_helpers.get_db () in
      let state = ref default_state in

      (* Profile ID *)
      Printf.printf "\n%s\n"
        (Setup_common.bold "=== Room-Agent Pilot Wizard ===");
      Printf.printf "\n";
      let profile_id =
        Setup_common.prompt_string ~prompt:"Profile ID" ~default:"" ()
      in
      match validate_profile_id profile_id with
      | Error e -> Printf.sprintf "Error: %s" e
      | Ok pid ->
          state := { !state with profile_id = pid };

          (* Load existing profile if it exists *)
          (match
             List.find_opt
               (fun (p : room_profile) -> p.id = pid)
               cfg.room_profiles
           with
          | Some existing ->
              state :=
                {
                  !state with
                  display_name =
                    (match existing.display_name with
                    | Some n -> n
                    | None -> "");
                  model = existing.model;
                  system_prompt = existing.system_prompt;
                  max_tool_iterations = existing.max_tool_iterations;
                  allowed_tools = existing.allowed_tools;
                  denied_tools = existing.denied_tools;
                  access_bundle_ids = existing.access_bundle_ids;
                };
              Printf.printf "  %s\n"
                (Setup_common.dim
                   (Printf.sprintf "Loaded existing profile '%s'" pid))
          | None ->
              Printf.printf "  %s\n" (Setup_common.dim "Creating new profile"));

          (* Model *)
          let model =
            Setup_common.prompt_string ~prompt:"Model" ~default:!state.model ()
          in
          (match validate_model model with
          | Error e -> Printf.printf "  Warning: %s\n" e
          | Ok m -> state := { !state with model = m });

          (* Display name *)
          let display_name =
            Setup_common.prompt_string ~prompt:"Display name (optional)"
              ~default:!state.display_name ()
          in
          state := { !state with display_name };

          (* System prompt *)
          Printf.printf "\nSystem prompt (empty to skip, or enter text):\n";
          let system_prompt =
            Setup_common.prompt_string ~prompt:"System prompt"
              ~default:!state.system_prompt ()
          in
          state := { !state with system_prompt };

          (* Max tool iterations *)
          let max_iters_str =
            Setup_common.prompt_string ~prompt:"Max tool iterations"
              ~default:(string_of_int !state.max_tool_iterations)
              ()
          in
          (match validate_max_iters max_iters_str with
          | Ok n ->
              state := { !state with max_tool_iterations = int_of_string n }
          | Error e -> Printf.printf "  Warning: %s\n" e);

          (* Access bundles *)
          Printf.printf "\nAccess bundle IDs (comma-separated, or empty):\n";
          let bundles_str =
            Setup_common.prompt_string ~prompt:"Access bundles"
              ~default:(String.concat "," !state.access_bundle_ids)
              ()
          in
          let bundles =
            if bundles_str = "" then []
            else
              String.split_on_char ',' bundles_str
              |> List.map String.trim
              |> List.filter (fun s -> s <> "")
          in
          state := { !state with access_bundle_ids = bundles };

          (* Memory scope *)
          Printf.printf "\nMemory scope configuration:\n";
          let scope_kind =
            Setup_common.prompt_string
              ~prompt:"Scope kind (room/channel/workspace)"
              ~default:!state.memory_scope_kind ()
          in
          let scope_key =
            Setup_common.prompt_string ~prompt:"Scope key (e.g., room_id)"
              ~default:!state.memory_scope_key ()
          in
          state :=
            {
              !state with
              memory_scope_kind = scope_kind;
              memory_scope_key = scope_key;
            };

          (* Budget *)
          Printf.printf "\nBudget configuration (0 to disable):\n";
          let token_limit_str =
            Setup_common.prompt_string ~prompt:"Token limit"
              ~default:(string_of_int !state.token_limit)
              ()
          in
          (match validate_token_limit token_limit_str with
          | Ok n -> state := { !state with token_limit = int_of_string n }
          | Error e -> Printf.printf "  Warning: %s\n" e);

          let cost_limit_str =
            Setup_common.prompt_string ~prompt:"Cost limit (USD)"
              ~default:(string_of_float !state.cost_limit_usd)
              ()
          in
          (match validate_cost_limit cost_limit_str with
          | Ok f -> state := { !state with cost_limit_usd = float_of_string f }
          | Error e -> Printf.printf "  Warning: %s\n" e);

          let reset_period =
            Setup_common.prompt_string
              ~prompt:"Budget reset period (daily/weekly/monthly/yearly)"
              ~default:!state.budget_reset_period ()
          in
          (match validate_budget_period reset_period with
          | Ok p -> state := { !state with budget_reset_period = p }
          | Error e -> Printf.printf "  Warning: %s\n" e);

          (* Connector binding *)
          Printf.printf "\nConnector binding:\n";
          let available = configured_connectors cfg in
          let has_teams = List.mem "teams" available in
          let has_slack = List.mem "slack" available in
          let def_conn = default_connector cfg in
          Printf.printf "  %s\n"
            (Setup_common.dim
               (Printf.sprintf "Available: %s (default: %s)"
                  (if available = [] then "none"
                   else String.concat ", " available)
                  def_conn));
          let connector_type =
            Setup_common.prompt_string
              ~prompt:"Connector type (teams/slack/discord/telegram)"
              ~default:def_conn ()
          in
          state := { !state with connector_type };
          if connector_type <> "teams" && has_teams then
            Printf.printf "  %s\n"
              (Setup_common.yellow
                 (Printf.sprintf
                    "Note: Teams is configured but you chose '%s'. Teams \
                     supports Adaptive Cards, rich questions, and typing \
                     indicators."
                    connector_type));
          if connector_type = "teams" && has_slack then begin
            Printf.printf "\n  %s\n"
              (Setup_common.cyan
                 "Slack is also configured. Showing capability comparison:");
            display_capability_comparison ();
            Printf.printf "  %s\n"
              (Setup_common.dim
                 "Teams is recommended for rich interactions (cards, buttons, \
                  file consent).")
          end;
          let room_prompt =
            match connector_type with
            | "teams" -> "Teams conversation ID (e.g., 19:xxx@thread.tacv2)"
            | "slack" -> "Slack channel ID (e.g., C12345 or #general)"
            | _ -> "Room ID (e.g., C12345, conv-abc)"
          in
          let connector_room =
            Setup_common.prompt_string ~prompt:room_prompt
              ~default:!state.connector_room ()
          in
          (match
             validate_room_id_for_connector connector_type connector_room
           with
          | Error e ->
              Printf.printf "  %s\n" (Setup_common.yellow ("Warning: " ^ e))
          | Ok _ -> ());
          state := { !state with connector_room };
          if connector_room <> "" then begin
            let active =
              Setup_common.prompt_yn ~prompt:"Active?" ~default:true ()
            in
            state := { !state with connector_active = active }
          end;

          (* Generate and display plan *)
          let plan = generate_plan ~cfg ~state:!state in
          display_plan plan;

          (* Run readiness checks *)
          let checks = run_readiness_checks ~cfg ~db:(Some db) ~state:!state in
          let all_ready = display_readiness checks in

          if not all_ready then begin
            Printf.printf "%s\n"
              (Setup_common.yellow
                 "Some readiness checks failed. Please fix issues before \
                  applying.");
            "Wizard completed with warnings. No changes applied."
          end
          else begin
            (* Ask for confirmation *)
            Printf.printf "\n";
            let apply =
              Setup_common.prompt_yn ~prompt:"Apply this plan?" ~default:true ()
            in
            if not apply then "Plan cancelled. No changes applied."
            else
              match apply_plan ~db ~cfg ~state:!state with
              | Error e -> Printf.sprintf "Error: %s" e
              | Ok msg ->
                  Printf.printf "\n%s\n" (Setup_common.green msg);
                  Printf.printf "\nNext steps:\n";
                  Printf.printf "  1. Restart daemon: clawq daemon restart\n";
                  Printf.printf "  2. Verify: clawq rooms show %s\n"
                    !state.connector_room;
                  Printf.printf "  3. Test: send a message to the room\n";
                  msg
          end)

(* ── Non-interactive plan mode ──────────────────────────────────── *)

let run_plan ~(profile_id : string) ~(model : string) ~(system_prompt : string)
    ~(max_tool_iterations : int) ~(allowed_tools : string list)
    ~(denied_tools : string list) ~(access_bundle_ids : string list)
    ~(token_limit : int) ~(cost_limit_usd : float) ~(reset_period : string)
    ~(connector_type : string) ~(connector_room : string)
    ~(connector_active : bool) () =
  let cfg = Command_bridge_helpers.get_config () in
  let state : wizard_state =
    {
      profile_id;
      display_name = "";
      model;
      system_prompt;
      max_tool_iterations;
      allowed_tools;
      denied_tools;
      access_bundle_ids;
      memory_scope_kind = "room";
      memory_scope_key = connector_room;
      token_limit;
      cost_limit_usd;
      budget_reset_period = reset_period;
      connector_type;
      connector_room;
      connector_active;
    }
  in
  let plan = generate_plan ~cfg ~state in
  display_plan plan;
  if connector_type = "teams" then begin
    let has_slack = connector_is_configured cfg "slack" in
    if has_slack then display_capability_comparison ()
  end;
  let checks = run_readiness_checks ~cfg ~db:None ~state in
  let all_ready = display_readiness checks in
  if all_ready then "Plan is ready. All readiness checks passed."
  else "Plan has warnings. Review readiness checks above."

(* ── Non-interactive rerun mode ─────────────────────────────────── *)

(** [run_rerun] compares the desired state against the current config and
    generates a rerun report. If [--apply] is passed and there are no blocked or
    manual-repair items, it also applies the changes. *)
let run_rerun ~(profile_id : string) ~(model : string) ~(system_prompt : string)
    ~(max_tool_iterations : int) ~(allowed_tools : string list)
    ~(denied_tools : string list) ~(access_bundle_ids : string list)
    ~(token_limit : int) ~(cost_limit_usd : float) ~(reset_period : string)
    ~(connector_type : string) ~(connector_room : string)
    ~(connector_active : bool) ~(apply : bool) () =
  let cfg = Command_bridge_helpers.get_config () in
  let state : wizard_state =
    {
      profile_id;
      display_name = "";
      model;
      system_prompt;
      max_tool_iterations;
      allowed_tools;
      denied_tools;
      access_bundle_ids;
      memory_scope_kind = "room";
      memory_scope_key = connector_room;
      token_limit;
      cost_limit_usd;
      budget_reset_period = reset_period;
      connector_type;
      connector_room;
      connector_active;
    }
  in
  let report = generate_rerun_report ~cfg ~state in
  let changed, blocked, manual = display_rerun_report report in
  let total = List.length report in
  let already_valid = total - changed - blocked - manual in
  if blocked > 0 || manual > 0 then begin
    let msg =
      Printf.sprintf
        "Rerun report has %d blocked and %d manual-repair items. Fix these \
         before applying."
        blocked manual
    in
    if apply then Printf.sprintf "Error: %s" msg else msg
  end
  else if changed = 0 then "All items are already valid. Nothing to do."
  else if not apply then
    Printf.sprintf
      "Rerun report: %d changed, %d already valid. Run with --apply to apply \
       changes."
      changed already_valid
  else begin
    (* Run readiness checks before applying *)
    let checks = run_readiness_checks ~cfg ~db:None ~state in
    let all_ready = List.for_all (fun c -> c.passed) checks in
    if not all_ready then begin
      ignore (display_readiness checks);
      "Error: readiness checks failed. Fix issues before applying."
    end
    else begin
      (* Apply the changes *)
      let db = Command_bridge_helpers.get_db () in
      match apply_plan ~db ~cfg ~state with
      | Error e -> Printf.sprintf "Error applying changes: %s" e
      | Ok msg ->
          Printf.sprintf "Applied %d changes successfully.\n%s" changed msg
    end
  end

(* ── CLI entry point ────────────────────────────────────────────── *)

let admin_env_var = "CLAWQ_ADMIN"

let is_admin_cli () =
  match Sys.getenv_opt admin_env_var with
  | Some v -> v = "1" || v = "true"
  | None -> false

let require_admin () =
  if is_admin_cli () then None
  else
    Some
      "Error: this command requires admin privileges. Set CLAWQ_ADMIN=1 in \
       your environment."

let run args =
  (* Admin check for mutating operations *)
  let check_admin () =
    match require_admin () with Some err -> Error err | None -> Ok ()
  in
  match args with
  | [] | "interactive" :: _ -> (
      match check_admin () with Error err -> err | Ok () -> run_wizard ())
  | "plan" :: flags -> (
      let get_flag name default =
        let rec find = function
          | k :: v :: _ when k = name -> v
          | _ :: rest -> find rest
          | [] -> default
        in
        find flags
      in
      let profile_id = get_flag "--profile-id" "" in
      if profile_id = "" then
        "Error: --profile-id is required for plan mode.\n\n\
         Usage: clawq rooms wizard plan --profile-id ID [--model M] \
         [--system-prompt P]\n\
        \       [--max-iters N] [--allowed-tools T1,T2] [--denied-tools T1,T2]\n\
        \       [--access-bundles B1,B2] [--token-limit N] [--cost-limit F]\n\
        \       [--reset-period P] [--connector C] [--room R] [--inactive]"
      else
        let max_iters_str = get_flag "--max-iters" "25" in
        match int_of_string_opt max_iters_str with
        | Some n when n > 0 && n <= 1000 ->
            let model = get_flag "--model" "openai:gpt-5.4" in
            let system_prompt = get_flag "--system-prompt" "" in
            let max_tool_iterations = n in
            let split_list s =
              if s = "" then []
              else
                String.split_on_char ',' s |> List.map String.trim
                |> List.filter (fun s -> s <> "")
            in
            let allowed_tools = split_list (get_flag "--allowed-tools" "") in
            let denied_tools = split_list (get_flag "--denied-tools" "") in
            let access_bundle_ids =
              split_list (get_flag "--access-bundles" "")
            in
            let token_limit =
              match int_of_string_opt (get_flag "--token-limit" "0") with
              | Some n -> n
              | None -> 0
            in
            let cost_limit_usd =
              match float_of_string_opt (get_flag "--cost-limit" "0") with
              | Some f -> f
              | None -> 0.0
            in
            let reset_period = get_flag "--reset-period" "monthly" in
            let connector_room = get_flag "--room" "" in
            let connector_active = not (List.mem "--inactive" flags) in
            let connector_type = get_flag "--connector" "teams" in
            run_plan ~profile_id ~model ~system_prompt ~max_tool_iterations
              ~allowed_tools ~denied_tools ~access_bundle_ids ~token_limit
              ~cost_limit_usd ~reset_period ~connector_type ~connector_room
              ~connector_active ()
        | Some _ ->
            Printf.sprintf
              "Error: --max-iters must be between 1 and 1000, got '%s'."
              max_iters_str
        | None ->
            Printf.sprintf
              "Error: --max-iters must be a valid integer, got '%s'."
              max_iters_str)
  | "apply" :: flags -> (
      match check_admin () with
      | Error err -> err
      | Ok () -> (
          let get_flag name default =
            let rec find = function
              | k :: v :: _ when k = name -> v
              | _ :: rest -> find rest
              | [] -> default
            in
            find flags
          in
          let profile_id = get_flag "--profile-id" "" in
          if profile_id = "" then
            "Error: --profile-id is required for apply mode.\n\n\
             Usage: clawq rooms wizard apply --profile-id ID [--model M] \
             [--system-prompt P]\n\
            \       [--max-iters N] [--allowed-tools T1,T2] [--denied-tools \
             T1,T2]\n\
            \       [--access-bundles B1,B2] [--token-limit N] [--cost-limit F]\n\
            \       [--reset-period P] [--connector C] [--room R] [--inactive]"
          else
            let cfg = Command_bridge_helpers.get_config () in
            let db = Command_bridge_helpers.get_db () in
            let max_iters_str = get_flag "--max-iters" "25" in
            match int_of_string_opt max_iters_str with
            | Some n when n > 0 && n <= 1000 -> (
                let model = get_flag "--model" "openai:gpt-5.4" in
                let system_prompt = get_flag "--system-prompt" "" in
                let max_tool_iterations = n in
                let split_list s =
                  if s = "" then []
                  else
                    String.split_on_char ',' s |> List.map String.trim
                    |> List.filter (fun s -> s <> "")
                in
                let allowed_tools =
                  split_list (get_flag "--allowed-tools" "")
                in
                let denied_tools = split_list (get_flag "--denied-tools" "") in
                let access_bundle_ids =
                  split_list (get_flag "--access-bundles" "")
                in
                let token_limit =
                  match int_of_string_opt (get_flag "--token-limit" "0") with
                  | Some n -> n
                  | None -> 0
                in
                let cost_limit_usd =
                  match float_of_string_opt (get_flag "--cost-limit" "0") with
                  | Some f -> f
                  | None -> 0.0
                in
                let reset_period = get_flag "--reset-period" "monthly" in
                let connector_room = get_flag "--room" "" in
                let connector_active = not (List.mem "--inactive" flags) in
                let connector_type = get_flag "--connector" "teams" in
                let state : wizard_state =
                  {
                    profile_id;
                    display_name = "";
                    model;
                    system_prompt;
                    max_tool_iterations;
                    allowed_tools;
                    denied_tools;
                    access_bundle_ids;
                    memory_scope_kind = "room";
                    memory_scope_key = connector_room;
                    token_limit;
                    cost_limit_usd;
                    budget_reset_period = reset_period;
                    connector_type;
                    connector_room;
                    connector_active;
                  }
                in
                let checks = run_readiness_checks ~cfg ~db:(Some db) ~state in
                let all_ready = List.for_all (fun c -> c.passed) checks in
                if not all_ready then begin
                  ignore (display_readiness checks);
                  "Error: readiness checks failed. Fix issues before applying."
                end
                else
                  match apply_plan ~db ~cfg ~state with
                  | Error e -> Printf.sprintf "Error: %s" e
                  | Ok msg -> msg)
            | Some _ ->
                Printf.sprintf
                  "Error: --max-iters must be between 1 and 1000, got '%s'."
                  max_iters_str
            | None ->
                Printf.sprintf
                  "Error: --max-iters must be a valid integer, got '%s'."
                  max_iters_str))
  | "rerun" :: flags -> (
      let get_flag name default =
        let rec find = function
          | k :: v :: _ when k = name -> v
          | _ :: rest -> find rest
          | [] -> default
        in
        find flags
      in
      let profile_id = get_flag "--profile-id" "" in
      if profile_id = "" then
        "Error: --profile-id is required for rerun mode.\n\n\
         Usage: clawq rooms wizard rerun --profile-id ID [--model M] \
         [--system-prompt P]\n\
        \       [--max-iters N] [--allowed-tools T1,T2] [--denied-tools T1,T2]\n\
        \       [--access-bundles B1,B2] [--token-limit N] [--cost-limit F]\n\
        \       [--reset-period P] [--connector C] [--room R] [--inactive] \
         [--apply]"
      else
        let apply = List.mem "--apply" flags in
        (* Admin check required for mutating operations *)
        match (apply, check_admin ()) with
        | true, Error err -> err
        | _ -> (
            let max_iters_str = get_flag "--max-iters" "25" in
            match int_of_string_opt max_iters_str with
            | Some n when n > 0 && n <= 1000 ->
                let model = get_flag "--model" "openai:gpt-5.4" in
                let system_prompt = get_flag "--system-prompt" "" in
                let max_tool_iterations = n in
                let split_list s =
                  if s = "" then []
                  else
                    String.split_on_char ',' s |> List.map String.trim
                    |> List.filter (fun s -> s <> "")
                in
                let allowed_tools =
                  split_list (get_flag "--allowed-tools" "")
                in
                let denied_tools = split_list (get_flag "--denied-tools" "") in
                let access_bundle_ids =
                  split_list (get_flag "--access-bundles" "")
                in
                let token_limit =
                  match int_of_string_opt (get_flag "--token-limit" "0") with
                  | Some n -> n
                  | None -> 0
                in
                let cost_limit_usd =
                  match float_of_string_opt (get_flag "--cost-limit" "0") with
                  | Some f -> f
                  | None -> 0.0
                in
                let reset_period = get_flag "--reset-period" "monthly" in
                let connector_room = get_flag "--room" "" in
                let connector_active = not (List.mem "--inactive" flags) in
                let connector_type = get_flag "--connector" "teams" in
                run_rerun ~profile_id ~model ~system_prompt ~max_tool_iterations
                  ~allowed_tools ~denied_tools ~access_bundle_ids ~token_limit
                  ~cost_limit_usd ~reset_period ~connector_type ~connector_room
                  ~connector_active ~apply ()
            | Some _ ->
                Printf.sprintf
                  "Error: --max-iters must be between 1 and 1000, got '%s'."
                  max_iters_str
            | None ->
                Printf.sprintf
                  "Error: --max-iters must be a valid integer, got '%s'."
                  max_iters_str))
  | "validate-delivery" :: flags ->
      let cfg = Command_bridge_helpers.get_config () in
      let get_flag name default =
        let rec find = function
          | k :: v :: _ when k = name -> v
          | _ :: rest -> find rest
          | [] -> default
        in
        find flags
      in
      let profile_id = get_flag "--profile-id" "" in
      let connector = get_flag "--connector" (default_connector cfg) in
      let room_id = get_flag "--room" "" in
      Setup_room_wizard_validate.run ~profile_id ~connector ~room_id ()
  | _ ->
      "Usage: clawq rooms wizard \
       [interactive|plan|apply|rerun|validate-delivery] [options]\n\n\
       Modes:\n\
      \  interactive              Interactive wizard (default)\n\
      \  plan [options]           Show what would happen (no side effects)\n\
      \  apply [options]          Apply configuration changes\n\
      \  rerun [options]          Compare desired state vs current config; \
       report changed/blocked/valid items\n\
      \  validate-delivery [opts] Simulate delivery and show \
       audit/ledger/delivery traces\n\n\
       Options (for plan/apply/rerun/validate-delivery):\n\
      \  --profile-id ID          Room profile ID (required)\n\
      \  --model M                Model identifier\n\
      \  --system-prompt P        System prompt text\n\
      \  --max-iters N            Max tool iterations\n\
      \  --allowed-tools T1,T2    Allowed tools (comma-separated)\n\
      \  --denied-tools T1,T2     Denied tools (comma-separated)\n\
      \  --access-bundles B1,B2   Access bundle IDs (comma-separated)\n\
      \  --token-limit N          Token budget limit\n\
      \  --cost-limit F           Cost budget limit (USD)\n\
      \  --reset-period P         Budget reset period \
       (daily/weekly/monthly/yearly)\n\
      \  --connector C            Connector type (teams/slack/discord/telegram)\n\
      \  --room R                 Room/channel ID (e.g., C12345, conv-abc)\n\
      \  --inactive               Create binding as inactive\n\
      \  --apply                  (rerun only) Apply changes if no blocked \
       items\n\n\
       Note: When using plan/apply modes from the command line, you may need\n\
       to use '--' before the options to avoid Cmdliner parsing conflicts:\n\
      \  clawq rooms -- wizard plan --profile-id ID --model M"

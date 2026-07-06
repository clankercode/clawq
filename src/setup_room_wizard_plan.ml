(* Plan/readiness helpers for the room-agent setup wizard. *)

open Runtime_config_types
open Setup_room_wizard_types
open Setup_room_wizard_connectors

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

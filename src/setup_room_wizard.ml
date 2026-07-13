(* setup_room_wizard.ml — Room-agent pilot wizard with plan/apply flow

   Supports configuring:
   - Room profile (id, model, system prompt, tool restrictions)
   - Access bundle binding
   - Memory scope
   - Budget limits
   - Connector binding
   - Readiness checks

   Plan mode: shows what would happen without side effects.
   Apply mode: store typed Setup_plan then confirm/apply via
   Room_agent_setup_apply (shared framework). Domain config mutation remains
   [apply_plan] as the config_apply hook only — not a parallel confirm path.
   Boundary: docs/setup-framework-boundary.md (P20.M2.E1.T003). *)

open Runtime_config_types
include Setup_room_wizard_types
include Setup_room_wizard_connectors
include Setup_room_wizard_plan
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

type config_file_snapshot = Missing_config | Config_contents of string

let snapshot_config_file () =
  let path = Setup_common.config_path () in
  if Sys.file_exists path then
    try
      Ok (Config_contents (In_channel.with_open_bin path In_channel.input_all))
    with Sys_error e -> Error e
  else Ok Missing_config

let restore_config_file snapshot =
  let path = Setup_common.config_path () in
  try
    match snapshot with
    | Missing_config ->
        if Sys.file_exists path then Unix.unlink path;
        Ok ()
    | Config_contents contents ->
        Out_channel.with_open_bin path (fun oc ->
            Out_channel.output_string oc contents);
        Ok ()
  with Sys_error e -> Error e

(** Run the legacy config mutation with compensation for the shared apply
    transaction. The config file is external to SQLite, so a later receipt or
    audit failure must restore the exact pre-apply contents. *)
let apply_plan_with_rollback ~db ~cfg ~state =
  match snapshot_config_file () with
  | Error error -> Error ("failed to snapshot config before apply: " ^ error)
  | Ok snapshot -> (
      match apply_plan ~db ~cfg ~state with
      | Ok message -> Ok (message, fun () -> restore_config_file snapshot)
      | Error error -> (
          match restore_config_file snapshot with
          | Ok () -> Error error
          | Error rollback_error ->
              Error
                (Printf.sprintf "%s; config rollback failed: %s" error
                   rollback_error)))

let current_live_base_revision () =
  Setup_plan.base_revision_of_config (Command_bridge_helpers.get_config ())

let fresh_apply_request ~plan ~principal ~actor =
  {
    Room_agent_setup_apply.plan_id = plan.Setup_plan.id;
    digest = plan.digest;
    principal;
    current_base_revision = current_live_base_revision ();
    destination_room = plan.destination.room_id;
    now = Unix.gettimeofday ();
    actor;
  }

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

          (* Canonical Setup_plan (shared framework) + legacy display summary. *)
          let principal = Room_agent_setup_plan.default_cli_principal in
          let base_revision = Setup_plan.base_revision_of_config cfg in
          let typed =
            Room_agent_setup_plan.plan ~cfg ~state:!state ~principal
              ~base_revision ()
          in
          Printf.printf "\n%s\n"
            (Setup_common.bold "=== Setup_plan (shared) ===");
          Printf.printf "%s\n" (Setup_plan.format_summary typed);
          let legacy_items = generate_plan ~cfg ~state:!state in
          display_plan legacy_items;

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
            (* Confirm then apply via shared plan-confirm-apply (P20.M2.E1.T003).
               No direct config mutation without a stored plan + digest. *)
            Printf.printf "\n";
            let apply =
              Setup_common.prompt_yn ~prompt:"Apply this plan?" ~default:true ()
            in
            if not apply then "Plan cancelled. No changes applied."
            else
              match
                Room_agent_setup_apply.plan_and_store ~db ~cfg ~state:!state
                  ~principal ~base_revision ()
              with
              | Error e ->
                  Printf.sprintf "Error: failed to store setup plan: %s" e
              | Ok plan -> (
                  let actor : Setup_plan_consent.actor =
                    {
                      principal_id = principal.id;
                      role = Global_admin;
                      source_room_id = plan.destination.room_id;
                    }
                  in
                  let config_apply ~plan:_ ~receipt_id:_ =
                    match apply_plan_with_rollback ~db ~cfg ~state:!state with
                    | Ok (_, rollback) -> Ok rollback
                    | Error e -> Error e
                  in
                  let req = fresh_apply_request ~plan ~principal ~actor in
                  match
                    Room_agent_setup_apply.apply_confirmed ~db ~config_apply req
                  with
                  | Room_agent_setup_apply.Rejected { reason; message } ->
                      Printf.sprintf "Error: setup apply rejected (%s): %s"
                        reason message
                  | Room_agent_setup_apply.Applied
                      { receipt_id; first_time; config_mutated; _ } ->
                      let msg =
                        Printf.sprintf
                          "Applied setup plan %s (receipt %s, first=%b, \
                           config_mutated=%b)."
                          plan.id receipt_id first_time config_mutated
                      in
                      Printf.printf "\n%s\n" (Setup_common.green msg);
                      Printf.printf "\nNext steps:\n";
                      Printf.printf
                        "  1. Restart daemon: clawq daemon restart\n";
                      Printf.printf "  2. Verify: clawq rooms show %s\n"
                        !state.connector_room;
                      Printf.printf "  3. Test: send a message to the room\n";
                      msg)
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
  (* Canonical shared Setup_plan first; legacy item list is display-only. *)
  let typed =
    Room_agent_setup_plan.plan ~cfg ~state
      ~principal:Room_agent_setup_plan.default_cli_principal ()
  in
  Printf.printf "%s\n" (Setup_common.bold "=== Setup_plan (shared) ===");
  Printf.printf "%s\n" (Setup_plan.format_summary typed);
  let legacy_items = generate_plan ~cfg ~state in
  display_plan legacy_items;
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
      (* Shared plan-confirm-apply repair path (P20.M2.E1.T002). *)
      let db = Command_bridge_helpers.get_db () in
      let principal = Room_agent_setup_plan.default_cli_principal in
      let base_revision = Setup_plan.base_revision_of_config cfg in
      match
        Room_agent_setup_apply.plan_and_store ~db ~cfg ~state ~principal
          ~base_revision ()
      with
      | Error e -> Printf.sprintf "Error: failed to store setup plan: %s" e
      | Ok plan -> (
          let actor : Setup_plan_consent.actor =
            {
              principal_id = principal.id;
              role = Global_admin;
              source_room_id = plan.destination.room_id;
            }
          in
          let config_apply ~plan:_ ~receipt_id:_ =
            match apply_plan_with_rollback ~db ~cfg ~state with
            | Ok (_, rollback) -> Ok rollback
            | Error e -> Error e
          in
          let req = fresh_apply_request ~plan ~principal ~actor in
          match
            Room_agent_setup_apply.apply_confirmed ~db ~config_apply req
          with
          | Room_agent_setup_apply.Rejected { reason; message } ->
              Printf.sprintf "Error: setup apply rejected (%s): %s" reason
                message
          | Room_agent_setup_apply.Applied { receipt_id; _ } ->
              Printf.sprintf
                "Applied %d changes successfully (plan %s, receipt %s)." changed
                plan.id receipt_id)
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
                  (* Shared plan-confirm-apply (P20.M2.E1.T002): store a typed
                     plan, then apply with CLI principal + config mutation via
                     existing apply_plan. *)
                  let principal = Room_agent_setup_plan.default_cli_principal in
                  let base_revision = Setup_plan.base_revision_of_config cfg in
                  match
                    Room_agent_setup_apply.plan_and_store ~db ~cfg ~state
                      ~principal ~base_revision ()
                  with
                  | Error e ->
                      Printf.sprintf "Error: failed to store setup plan: %s" e
                  | Ok plan -> (
                      let actor : Setup_plan_consent.actor =
                        {
                          principal_id = principal.id;
                          role = Global_admin;
                          source_room_id = plan.destination.room_id;
                        }
                      in
                      let config_apply ~plan:_ ~receipt_id:_ =
                        match apply_plan_with_rollback ~db ~cfg ~state with
                        | Ok (_, rollback) -> Ok rollback
                        | Error e -> Error e
                      in
                      let req = fresh_apply_request ~plan ~principal ~actor in
                      match
                        Room_agent_setup_apply.apply_confirmed ~db ~config_apply
                          req
                      with
                      | Room_agent_setup_apply.Rejected { reason; message } ->
                          Printf.sprintf "Error: setup apply rejected (%s): %s"
                            reason message
                      | Room_agent_setup_apply.Applied
                          { receipt_id; first_time; config_mutated; _ } ->
                          Printf.sprintf
                            "Applied setup plan %s (receipt %s, first=%b, \
                             config_mutated=%b)."
                            plan.id receipt_id first_time config_mutated))
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

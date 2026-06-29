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

(* ── Types ──────────────────────────────────────────────────────── *)

type wizard_state = {
  profile_id : string;
  display_name : string;
  model : string;
  system_prompt : string;
  max_tool_iterations : int;
  allowed_tools : string list;
  denied_tools : string list;
  access_bundle_ids : string list;
  memory_scope_kind : string;
  memory_scope_key : string;
  token_limit : int;
  cost_limit_usd : float;
  budget_reset_period : string;
  connector_room : string;
  connector_active : bool;
}

type plan_item = { category : string; action : string; details : string }
type readiness_check = { name : string; passed : bool; message : string }

(* ── Defaults ───────────────────────────────────────────────────── *)

let default_state : wizard_state =
  {
    profile_id = "";
    display_name = "";
    model = "openai:gpt-5.4";
    system_prompt = "";
    max_tool_iterations = 25;
    allowed_tools = [];
    denied_tools = [];
    access_bundle_ids = [];
    memory_scope_kind = "room";
    memory_scope_key = "";
    token_limit = 0;
    cost_limit_usd = 0.0;
    budget_reset_period = "monthly";
    connector_room = "";
    connector_active = true;
  }

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
  if state.connector_room <> "" then
    add "Connector Binding"
      (if state.connector_active then "bind" else "bind-inactive")
      (Printf.sprintf "room=%s, active=%b" state.connector_room
         state.connector_active);

  List.rev !items

(* ── Readiness checks ───────────────────────────────────────────── *)

let run_readiness_checks ~(cfg : Runtime_config.t) ~(state : wizard_state) :
    readiness_check list =
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
  add "Connector Room"
    (state.connector_room = "" || String.length state.connector_room > 0)
    (if state.connector_room = "" then "No connector configured"
     else Printf.sprintf "Room: %s" state.connector_room);

  (* Check budget consistency *)
  add "Budget"
    (state.token_limit >= 0 && state.cost_limit_usd >= 0.0)
    (if state.token_limit >= 0 && state.cost_limit_usd >= 0.0 then "Valid"
     else "Limits must be non-negative");

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
      (fun item ->
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
          Printf.printf "\nConnector binding (empty to skip):\n";
          let connector_room =
            Setup_common.prompt_string
              ~prompt:"Room ID (e.g., C12345, conv-abc)"
              ~default:!state.connector_room ()
          in
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
          let checks = run_readiness_checks ~cfg ~state:!state in
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
    ~(connector_room : string) ~(connector_active : bool) () =
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
      connector_room;
      connector_active;
    }
  in
  let plan = generate_plan ~cfg ~state in
  display_plan plan;
  let checks = run_readiness_checks ~cfg ~state in
  let all_ready = display_readiness checks in
  if all_ready then "Plan is ready. All readiness checks passed."
  else "Plan has warnings. Review readiness checks above."

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
        \       [--reset-period P] [--room R] [--inactive]"
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
            run_plan ~profile_id ~model ~system_prompt ~max_tool_iterations
              ~allowed_tools ~denied_tools ~access_bundle_ids ~token_limit
              ~cost_limit_usd ~reset_period ~connector_room ~connector_active ()
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
            \       [--reset-period P] [--room R] [--inactive]"
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
                    connector_room;
                    connector_active;
                  }
                in
                let checks = run_readiness_checks ~cfg ~state in
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
  | _ ->
      "Usage: clawq rooms wizard [interactive|plan|apply] [options]\n\n\
       Modes:\n\
      \  interactive              Interactive wizard (default)\n\
      \  plan [options]           Show what would happen (no side effects)\n\
      \  apply [options]          Apply configuration changes\n\n\
       Options (for plan/apply):\n\
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
      \  --room R                 Room/channel ID (e.g., C12345, conv-abc)\n\
      \  --inactive               Create binding as inactive\n\n\
       Note: When using plan/apply modes from the command line, you may need\n\
       to use '--' before the options to avoid Cmdliner parsing conflicts:\n\
      \  clawq rooms -- wizard plan --profile-id ID --model M"

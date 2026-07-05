open Runtime_config_types
open Setup_room_wizard_types
open Setup_room_wizard_connectors

(** [generate_rerun_report ~cfg ~state] compares the desired [state] against the
    current [cfg] and returns a list of [{category, field, status, details}]
    entries describing what would happen if the wizard were applied.

    Status semantics:
    - [Changed]: value differs from current config; will be updated.
    - [Already_valid]: value matches current config; no action needed.
    - [Blocked]: cannot be applied due to missing dependencies (e.g. missing
      access bundles, unconfigured connector).
    - [Manual_repair]: needs human intervention (e.g. invalid room format,
      inconsistent budget). *)
let generate_rerun_report ~(cfg : Runtime_config.t) ~(state : wizard_state) :
    rerun_item list =
  let items = ref [] in
  let add cat field status details =
    items := { category = cat; field; status; details } :: !items
  in

  (* ── Profile existence ────────────────────────────────────────── *)
  let existing_profile =
    List.find_opt
      (fun (p : room_profile) -> p.id = state.profile_id)
      cfg.room_profiles
  in
  (match existing_profile with
  | Some _ ->
      add "Room Profile" "existence" Already_valid
        (Printf.sprintf "Profile '%s' exists" state.profile_id)
  | None ->
      add "Room Profile" "existence" Changed
        (Printf.sprintf "Profile '%s' will be created" state.profile_id));

  (* ── Profile model ──────────────────────────────────────────────
     Merge semantics: if state.model = default "openai:gpt-5.4" and existing
     model is non-empty, apply_plan preserves the existing model. So we only
     report Changed when the effective model differs. *)
  (match existing_profile with
  | Some p ->
      let effective_model =
        if state.model = "openai:gpt-5.4" && p.model <> "" then p.model
        else state.model
      in
      if effective_model = p.model then
        add "Room Profile" "model" Already_valid
          (Printf.sprintf "model=%s" effective_model)
      else
        add "Room Profile" "model" Changed
          (Printf.sprintf "model: %s -> %s" p.model effective_model)
  | None ->
      add "Room Profile" "model" Changed
        (Printf.sprintf "model=%s (new)" state.model));

  (* ── Profile max_tool_iterations ────────────────────────────────
     Merge semantics: if state.max_tool_iterations = default 25 and existing
     value > 0, apply_plan preserves the existing value. *)
  (match existing_profile with
  | Some p ->
      let effective_iters =
        if state.max_tool_iterations = 25 && p.max_tool_iterations > 0 then
          p.max_tool_iterations
        else state.max_tool_iterations
      in
      if effective_iters = p.max_tool_iterations then
        add "Room Profile" "max_iters" Already_valid
          (Printf.sprintf "max_iters=%d" effective_iters)
      else
        add "Room Profile" "max_iters" Changed
          (Printf.sprintf "max_iters: %d -> %d" p.max_tool_iterations
             effective_iters)
  | None ->
      add "Room Profile" "max_iters" Changed
        (Printf.sprintf "max_iters=%d (new)" state.max_tool_iterations));

  (* ── Display name ─────────────────────────────────────────────── *)
  (if state.display_name <> "" then
     match existing_profile with
     | Some p when p.display_name = Some state.display_name ->
         add "Room Profile" "display_name" Already_valid
           (Printf.sprintf "display_name=%s" state.display_name)
     | Some p ->
         let old = match p.display_name with Some n -> n | None -> "(none)" in
         add "Room Profile" "display_name" Changed
           (Printf.sprintf "display_name: %s -> %s" old state.display_name)
     | None ->
         add "Room Profile" "display_name" Changed
           (Printf.sprintf "display_name=%s (new)" state.display_name));

  (* ── System prompt ────────────────────────────────────────────── *)
  (if state.system_prompt <> "" then
     match existing_profile with
     | Some p when p.system_prompt = state.system_prompt ->
         add "Room Profile" "system_prompt" Already_valid
           (Printf.sprintf "system_prompt (%d chars)"
              (String.length state.system_prompt))
     | Some _ ->
         add "Room Profile" "system_prompt" Changed
           (Printf.sprintf "system_prompt will be updated (%d chars)"
              (String.length state.system_prompt))
     | None ->
         add "Room Profile" "system_prompt" Changed
           (Printf.sprintf "system_prompt=%s (new)"
              (Printf.sprintf "(%d chars)" (String.length state.system_prompt))));

  (* ── Allowed tools ────────────────────────────────────────────── *)
  (if state.allowed_tools <> [] then
     match existing_profile with
     | Some p when p.allowed_tools = state.allowed_tools ->
         add "Room Profile" "allowed_tools" Already_valid
           (Printf.sprintf "allowed_tools=%s"
              (String.concat ", " state.allowed_tools))
     | Some _ ->
         add "Room Profile" "allowed_tools" Changed
           (Printf.sprintf "allowed_tools will be updated: %s"
              (String.concat ", " state.allowed_tools))
     | None ->
         add "Room Profile" "allowed_tools" Changed
           (Printf.sprintf "allowed_tools=%s (new)"
              (String.concat ", " state.allowed_tools)));

  (* ── Denied tools ─────────────────────────────────────────────── *)
  (if state.denied_tools <> [] then
     match existing_profile with
     | Some p when p.denied_tools = state.denied_tools ->
         add "Room Profile" "denied_tools" Already_valid
           (Printf.sprintf "denied_tools=%s"
              (String.concat ", " state.denied_tools))
     | Some _ ->
         add "Room Profile" "denied_tools" Changed
           (Printf.sprintf "denied_tools will be updated: %s"
              (String.concat ", " state.denied_tools))
     | None ->
         add "Room Profile" "denied_tools" Changed
           (Printf.sprintf "denied_tools=%s (new)"
              (String.concat ", " state.denied_tools)));

  (* ── Access bundles ───────────────────────────────────────────── *)
  if state.access_bundle_ids <> [] then begin
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
      add "Access Bundle" "bind" Blocked
        (Printf.sprintf "Missing bundles: %s"
           (String.concat ", " missing_bundles))
    else
      match existing_profile with
      | Some p when p.access_bundle_ids = state.access_bundle_ids ->
          add "Access Bundle" "bind" Already_valid
            (Printf.sprintf "Bundles bound: %s"
               (String.concat ", " state.access_bundle_ids))
      | Some _ ->
          add "Access Bundle" "bind" Changed
            (Printf.sprintf "Bundles will be updated: %s"
               (String.concat ", " state.access_bundle_ids))
      | None ->
          add "Access Bundle" "bind" Changed
            (Printf.sprintf "Bundles will be bound: %s"
               (String.concat ", " state.access_bundle_ids))
  end;

  (* ── Memory scope ─────────────────────────────────────────────── *)
  if state.memory_scope_key <> "" then
    add "Memory Scope" "configure" Changed
      (Printf.sprintf "kind=%s, key=%s" state.memory_scope_kind
         state.memory_scope_key);

  (* ── Budget ───────────────────────────────────────────────────── *)
  if state.token_limit > 0 || state.cost_limit_usd > 0.0 then begin
    let valid = state.token_limit >= 0 && state.cost_limit_usd >= 0.0 in
    let period_ok =
      List.mem state.budget_reset_period
        [ "daily"; "weekly"; "monthly"; "yearly" ]
    in
    if not valid then
      add "Budget" "configure" Manual_repair "Limits must be non-negative"
    else if not period_ok then
      add "Budget" "configure" Manual_repair
        (Printf.sprintf "Invalid reset period: %s" state.budget_reset_period)
    else
      add "Budget" "configure" Changed
        (Printf.sprintf "tokens=%d, cost=$%.2f, period=%s" state.token_limit
           state.cost_limit_usd state.budget_reset_period)
  end;

  (* ── Connector binding ────────────────────────────────────────── *)
  if state.connector_room <> "" then begin
    let connector_label =
      match state.connector_type with
      | "teams" -> "Teams"
      | "slack" -> "Slack"
      | "discord" -> "Discord"
      | "telegram" -> "Telegram"
      | c -> c
    in
    let cfg_connectors = configured_connectors cfg in
    let connector_available = List.mem state.connector_type cfg_connectors in
    if not connector_available then
      add "Connector" "bind" Blocked
        (Printf.sprintf "%s not configured (available: %s)" connector_label
           (if cfg_connectors = [] then "none"
            else String.concat ", " cfg_connectors))
    else
      match
        validate_room_id_for_connector state.connector_type state.connector_room
      with
      | Error e ->
          add "Connector" "room_validation" Manual_repair
            (Printf.sprintf "%s room invalid: %s" connector_label e)
      | Ok _ -> (
          let existing_binding =
            List.find_opt
              (fun (b : room_profile_binding) -> b.room = state.connector_room)
              cfg.room_profile_bindings
          in
          match existing_binding with
          | Some b
            when b.profile_id = state.profile_id
                 && b.active = state.connector_active ->
              add "Connector" "bind" Already_valid
                (Printf.sprintf "%s room=%s already bound to profile '%s'"
                   connector_label state.connector_room state.profile_id)
          | Some b ->
              add "Connector" "bind" Changed
                (Printf.sprintf
                   "%s room=%s bound to '%s' (will update to '%s', active=%b)"
                   connector_label state.connector_room b.profile_id
                   state.profile_id state.connector_active)
          | None ->
              add "Connector" "bind" Changed
                (Printf.sprintf "%s room=%s will be bound to profile '%s'"
                   connector_label state.connector_room state.profile_id))
  end;

  List.rev !items

(** [display_rerun_report items] prints a formatted rerun report showing
    changed, already-valid, blocked, and manual-repair items. Returns
    [(changed_count, blocked_count, manual_count)]. *)
let display_rerun_report (items : rerun_item list) : int * int * int =
  let open Setup_common in
  Printf.printf "\n%s\n" (bold "=== Rerun Report ===");
  Printf.printf "\n";
  if items = [] then begin
    Printf.printf "  %s\n" (dim "(no items to report)");
    (0, 0, 0)
  end
  else begin
    let changed_count = ref 0 in
    let blocked_count = ref 0 in
    let manual_count = ref 0 in
    let last_cat = ref "" in
    List.iter
      (fun item ->
        if item.category <> !last_cat then begin
          Printf.printf "\n  %s\n" (bold item.category);
          last_cat := item.category
        end;
        let status_str, status_icon =
          match item.status with
          | Changed ->
              incr changed_count;
              (cyan "changed", cyan "~")
          | Already_valid -> (green "ok", green "✓")
          | Blocked ->
              incr blocked_count;
              (red "blocked", red "!")
          | Manual_repair ->
              incr manual_count;
              (yellow "manual", yellow "?")
        in
        Printf.printf "    %s %-14s %s: %s\n" status_icon status_str item.field
          item.details)
      items;
    Printf.printf "\n";
    Printf.printf
      "  Summary: %s changed, %s already valid, %s blocked, %s manual repair\n"
      (string_of_int !changed_count |> cyan)
      (string_of_int
         (List.length items - !changed_count - !blocked_count - !manual_count)
      |> green)
      (string_of_int !blocked_count |> red)
      (string_of_int !manual_count |> yellow);
    Printf.printf "\n";
    (!changed_count, !blocked_count, !manual_count)
  end

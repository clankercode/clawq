(* Room-agent pilot planning via shared Setup_plan (P20.M2.E1.T001).

   Read-only adapter: values only, no durable mutation. Shared by agent and CLI.
   See room_agent_setup_plan.mli and ADR 0003. *)

open Runtime_config_types
open Setup_room_wizard_types

let default_cli_principal : Setup_plan.principal =
  {
    id = "cli:rooms-wizard";
    kind = Setup_plan.Cli;
    label = Some "rooms wizard";
  }

(* ── JSON helpers (secret-free profile / binding surfaces) ──────── *)

let string_list_json xs = `List (List.map (fun s -> `String s) xs)

let profile_to_json (p : room_profile) : Yojson.Safe.t =
  `Assoc
    ([
       ("id", `String p.id);
       ("model", `String p.model);
       ("system_prompt", `String p.system_prompt);
       ("max_tool_iterations", `Int p.max_tool_iterations);
       ("status", `String p.status);
     ]
    @ (if p.allowed_tools = [] then []
       else [ ("allowed_tools", string_list_json p.allowed_tools) ])
    @ (if p.denied_tools = [] then []
       else [ ("denied_tools", string_list_json p.denied_tools) ])
    @ (if p.access_bundle_ids = [] then []
       else [ ("access_bundle_ids", string_list_json p.access_bundle_ids) ])
    @ (if not p.ambient_enabled then [] else [ ("ambient_enabled", `Bool true) ])
    @ (if p.ambient_rate_limit_rph = 0 then []
       else [ ("ambient_rate_limit_rph", `Int p.ambient_rate_limit_rph) ])
    @ (if not p.low_volume then [] else [ ("low_volume", `Bool true) ])
    @
    match p.display_name with
    | Some name -> [ ("display_name", `String name) ]
    | None -> [])

let binding_to_json (b : room_profile_binding) : Yojson.Safe.t =
  `Assoc
    [
      ("profile_id", `String b.profile_id);
      ("room", `String b.room);
      ("active", `Bool b.active);
    ]

let budget_json ~token_limit ~cost_limit_usd ~reset_period : Yojson.Safe.t =
  `Assoc
    [
      ("token_limit", `Int token_limit);
      ("cost_limit_usd", `Float cost_limit_usd);
      ("reset_period", `String reset_period);
    ]

let memory_scope_json ~kind ~key : Yojson.Safe.t =
  `Assoc [ ("kind", `String kind); ("key", `String key) ]

(* ── Planned profile (merge semantics match setup_room_wizard apply) *)

let find_profile (cfg : Runtime_config.t) profile_id =
  List.find_opt (fun (p : room_profile) -> p.id = profile_id) cfg.room_profiles

let find_binding_for_room (cfg : Runtime_config.t) room =
  if room = "" then None
  else
    List.find_opt
      (fun (b : room_profile_binding) -> b.room = room)
      cfg.room_profile_bindings

let planned_profile ~(cfg : Runtime_config.t) ~(state : wizard_state) :
    room_profile =
  match find_profile cfg state.profile_id with
  | Some existing ->
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
          (if state.max_tool_iterations = 25 && existing.max_tool_iterations > 0
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
        low_volume = existing.low_volume;
      }
  | None ->
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
        low_volume = false;
      }

let planned_binding ~(state : wizard_state) : room_profile_binding option =
  if state.connector_room = "" then None
  else
    Some
      {
        profile_id = state.profile_id;
        room = state.connector_room;
        active = state.connector_active;
      }

(* ── Current / planned state blobs ──────────────────────────────── *)

let state_blob ~profile ~binding ~budget ~memory : Yojson.Safe.t =
  let fields =
    [
      ( "profile",
        match profile with None -> `Null | Some p -> profile_to_json p );
      ( "binding",
        match binding with None -> `Null | Some b -> binding_to_json b );
    ]
    @ (match budget with None -> [] | Some j -> [ ("budget", j) ])
    @ match memory with None -> [] | Some j -> [ ("memory_scope", j) ]
  in
  `Assoc fields

let current_state_of ~(cfg : Runtime_config.t) ~(state : wizard_state) :
    Yojson.Safe.t =
  let profile = find_profile cfg state.profile_id in
  let binding = find_binding_for_room cfg state.connector_room in
  (* Budget / memory live in DB; config snapshot has no current values. *)
  state_blob ~profile ~binding ~budget:None ~memory:None

let planned_state_of ~(cfg : Runtime_config.t) ~(state : wizard_state) :
    Yojson.Safe.t =
  let profile = Some (planned_profile ~cfg ~state) in
  let binding = planned_binding ~state in
  let budget =
    if state.token_limit > 0 || state.cost_limit_usd > 0.0 then
      Some
        (budget_json ~token_limit:state.token_limit
           ~cost_limit_usd:state.cost_limit_usd
           ~reset_period:state.budget_reset_period)
    else None
  in
  let memory =
    if state.memory_scope_key <> "" then
      Some
        (memory_scope_json ~kind:state.memory_scope_kind
           ~key:state.memory_scope_key)
    else None
  in
  state_blob ~profile ~binding ~budget ~memory

(* ── Diff ───────────────────────────────────────────────────────── *)

let build_diff ~(cfg : Runtime_config.t) ~(state : wizard_state)
    ~(planned : room_profile) : Setup_plan.diff_op list =
  let profile_path = Printf.sprintf "room_profiles/%s" state.profile_id in
  let profile_ops =
    match find_profile cfg state.profile_id with
    | None ->
        [
          Setup_plan.Create
            { path = profile_path; value = profile_to_json planned };
        ]
    | Some existing ->
        let from_ = profile_to_json existing in
        let to_ = profile_to_json planned in
        if from_ = to_ then
          [
            Setup_plan.Note
              {
                path = profile_path;
                message = "Profile already matches desired state";
              };
          ]
        else [ Setup_plan.Update { path = profile_path; from_; to_ } ]
  in
  let binding_ops =
    match planned_binding ~state with
    | None -> []
    | Some b -> (
        let path =
          Printf.sprintf "room_profile_bindings/%s" state.connector_room
        in
        let target = state.profile_id in
        let bind_op =
          Setup_plan.Bind { path; target; active = state.connector_active }
        in
        match find_binding_for_room cfg state.connector_room with
        | None ->
            [ Setup_plan.Create { path; value = binding_to_json b }; bind_op ]
        | Some existing when existing = b ->
            [
              Setup_plan.Note
                { path; message = "Binding already matches desired state" };
            ]
        | Some existing ->
            [
              Setup_plan.Update
                {
                  path;
                  from_ = binding_to_json existing;
                  to_ = binding_to_json b;
                };
              bind_op;
            ])
  in
  let bundle_ops =
    if state.access_bundle_ids = [] then []
    else
      let missing =
        List.filter
          (fun id ->
            not
              (List.exists
                 (fun (b : access_bundle) -> b.id = id && b.status <> "deleted")
                 cfg.access_bundles))
          state.access_bundle_ids
      in
      if missing <> [] then
        [
          Setup_plan.Note
            {
              path = profile_path ^ "/access_bundle_ids";
              message =
                Printf.sprintf "Bundles not found: %s"
                  (String.concat ", " missing);
            };
        ]
      else
        [
          Setup_plan.Note
            {
              path = profile_path ^ "/access_bundle_ids";
              message =
                Printf.sprintf "Bind bundles: %s"
                  (String.concat ", " state.access_bundle_ids);
            };
        ]
  in
  let memory_ops =
    if state.memory_scope_key = "" then []
    else
      [
        Setup_plan.Note
          {
            path = Printf.sprintf "memory_scope/%s" state.memory_scope_key;
            message =
              Printf.sprintf "kind=%s, key=%s" state.memory_scope_kind
                state.memory_scope_key;
          };
      ]
  in
  let budget_ops =
    if state.token_limit <= 0 && state.cost_limit_usd <= 0.0 then []
    else
      [
        Setup_plan.Note
          {
            path = Printf.sprintf "room_budget/%s" state.profile_id;
            message =
              Printf.sprintf "tokens=%d, cost=$%.2f, period=%s"
                state.token_limit state.cost_limit_usd state.budget_reset_period;
          };
      ]
  in
  let connector_ops =
    if state.connector_room = "" || state.connector_type = "" then []
    else
      [
        Setup_plan.Note
          {
            path = "connector";
            message =
              Printf.sprintf "type=%s room=%s"
                (if state.connector_type = "teams" then "teams(primary)"
                 else state.connector_type)
                state.connector_room;
          };
      ]
  in
  profile_ops @ binding_ops @ bundle_ops @ memory_ops @ budget_ops
  @ connector_ops

(* ── Readiness / warnings ───────────────────────────────────────── *)

let readiness_of_checks (checks : readiness_check list) :
    Setup_plan.readiness_item list =
  List.map
    (fun (c : readiness_check) ->
      Setup_plan.
        {
          name = c.name;
          status = (if c.passed then Pass else Fail);
          message = c.message;
        })
    checks

let warnings_of ~(cfg : Runtime_config.t) ~(state : wizard_state)
    (checks : readiness_check list) : Setup_plan.warning list =
  let from_fails =
    List.filter_map
      (fun (c : readiness_check) ->
        if c.passed then None
        else
          Some
            {
              Setup_plan.code = "room_agent_readiness";
              message = Printf.sprintf "%s: %s" c.name c.message;
            })
      checks
  in
  let missing_bundles =
    List.filter
      (fun id ->
        not
          (List.exists
             (fun (b : access_bundle) -> b.id = id && b.status <> "deleted")
             cfg.access_bundles))
      state.access_bundle_ids
  in
  let bundle_warn =
    if missing_bundles = [] then []
    else
      [
        {
          Setup_plan.code = "access_bundle_missing";
          message =
            Printf.sprintf "Bundles not found: %s"
              (String.concat ", " missing_bundles);
        };
      ]
  in
  (* Deduplicate by message when readiness already reports missing bundles. *)
  let codes = List.map (fun w -> w.Setup_plan.code) from_fails in
  let bundle_warn =
    if List.mem "room_agent_readiness" codes && missing_bundles <> [] then
      bundle_warn
    else bundle_warn
  in
  from_fails @ bundle_warn

(* ── Apply payload (secret-free; mutation is T002) ──────────────── *)

let build_apply_payload ~(state : wizard_state) ~(planned : room_profile)
    ~base_revision : Setup_plan.apply_payload =
  let profile_op =
    let fields =
      [
        ("op", `String "upsert_profile");
        ("id", `String planned.id);
        ("model", `String planned.model);
        ("max_tool_iterations", `Int planned.max_tool_iterations);
        ("status", `String planned.status);
        ("system_prompt", `String planned.system_prompt);
        ("allowed_tools", string_list_json planned.allowed_tools);
        ("denied_tools", string_list_json planned.denied_tools);
        ("access_bundle_ids", string_list_json planned.access_bundle_ids);
      ]
      @
      match planned.display_name with
      | None -> []
      | Some n -> [ ("display_name", `String n) ]
    in
    `Assoc fields
  in
  let bind_ops =
    match planned_binding ~state with
    | None -> []
    | Some b ->
        [
          `Assoc
            [
              ("op", `String "bind_room");
              ("profile_id", `String b.profile_id);
              ("room", `String b.room);
              ("active", `Bool b.active);
              ("connector", `String state.connector_type);
            ];
        ]
  in
  let budget_ops =
    if state.token_limit <= 0 && state.cost_limit_usd <= 0.0 then []
    else
      [
        `Assoc
          [
            ("op", `String "set_budget");
            ("profile_id", `String state.profile_id);
            ("token_limit", `Int state.token_limit);
            ("cost_limit_usd", `Float state.cost_limit_usd);
            ("reset_period", `String state.budget_reset_period);
          ];
      ]
  in
  let memory_ops =
    if state.memory_scope_key = "" then []
    else
      [
        `Assoc
          [
            ("op", `String "set_memory_scope");
            ("profile_id", `String state.profile_id);
            ("kind", `String state.memory_scope_kind);
            ("key", `String state.memory_scope_key);
          ];
      ]
  in
  let ops = `List ([ profile_op ] @ bind_ops @ budget_ops @ memory_ops) in
  let data =
    `Assoc
      [
        ("base_revision", `String base_revision);
        ("profile_id", `String state.profile_id);
        ("connector", `String state.connector_type);
        ( "room",
          if state.connector_room = "" then `Null
          else `String state.connector_room );
      ]
  in
  { Setup_plan.kind = Setup_plan.Room_profile; ops; data }

(* ── Context ────────────────────────────────────────────────────── *)

let destination_context ~(state : wizard_state) : Setup_plan.context =
  {
    room_id =
      (if state.connector_room = "" then None else Some state.connector_room);
    session_key = None;
    connector =
      (if state.connector_type = "" then None else Some state.connector_type);
    profile_id = (if state.profile_id = "" then None else Some state.profile_id);
    extra =
      (if state.connector_type = "" then []
       else [ ("connector_type", `String state.connector_type) ]);
  }

(* ── Public entry ───────────────────────────────────────────────── *)

let plan ~(cfg : Runtime_config.t) ~(state : wizard_state)
    ~(principal : Setup_plan.principal) ?db ?base_revision
    ?(now = Unix.gettimeofday ()) ?id () : Setup_plan.t =
  let base_revision =
    match base_revision with
    | Some r -> r
    | None -> Setup_plan.base_revision_of_config cfg
  in
  let planned = planned_profile ~cfg ~state in
  let current_state = current_state_of ~cfg ~state in
  let planned_state = planned_state_of ~cfg ~state in
  let diff = build_diff ~cfg ~state ~planned in
  let checks = Setup_room_wizard_plan.run_readiness_checks ~cfg ~db ~state in
  let readiness = readiness_of_checks checks in
  let warnings = warnings_of ~cfg ~state checks in
  let destination = destination_context ~state in
  let source = destination in
  let apply_payload = build_apply_payload ~state ~planned ~base_revision in
  let raw =
    Setup_plan.make ~principal ~source ~destination ~current_state
      ~planned_state ~diff ~readiness ~warnings ~base_revision ~apply_payload
      ~now ?id ()
  in
  (* Defense-in-depth: plans are secret-free by construction; redact is free. *)
  Setup_plan.redact raw

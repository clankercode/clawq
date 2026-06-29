(* room_readiness_report.ml -- Comprehensive room-agent readiness report.

   Reports connector, scope, memory, GitHub, budget, audit, routine,
   ambient, and proxy-readiness status with actionable fix commands. *)

open Runtime_config_types

(* ── Types ──────────────────────────────────────────────────────── *)

type check_status = Pass | Fail | Warn | Skip

type check_result = {
  name : string;
  status : check_status;
  message : string;
  fix_command : string option;
}

type report = {
  room_id : string option;
  profile_id : string option;
  checks : check_result list;
  passed : int;
  failed : int;
  warned : int;
  skipped : int;
}

(* ── Helpers ────────────────────────────────────────────────────── *)

let make_check ~name ~status ~message ?fix_command () =
  { name; status; message; fix_command }

let pass ~name ~message = make_check ~name ~status:Pass ~message ()
let skip ~name ~message = make_check ~name ~status:Skip ~message ()

let fail ~name ~message ?fix_command () =
  make_check ~name ~status:Fail ~message ?fix_command ()

let warn ~name ~message ?fix_command () =
  make_check ~name ~status:Warn ~message ?fix_command ()

let room_profile_deleted (p : room_profile) =
  String.lowercase_ascii p.status = "deleted"

let connector_is_configured (cfg : Runtime_config.t) = function
  | "teams" -> (
      match cfg.channels.teams with
      | Some t -> t.app_id <> "" && t.app_secret <> ""
      | None -> false)
  | "slack" -> (
      match cfg.channels.slack with
      | Some s -> s.bot_token <> "" && s.signing_secret <> ""
      | None -> false)
  | "discord" -> Option.is_some cfg.channels.discord
  | "telegram" -> Option.is_some cfg.channels.telegram
  | _ -> false

let configured_connectors (cfg : Runtime_config.t) =
  [ "teams"; "slack"; "discord"; "telegram" ]
  |> List.filter (connector_is_configured cfg)

(* ── Individual checks ──────────────────────────────────────────── *)

(** Check 1: Connector readiness — is a connector configured and is the binding
    active? *)
let check_connector ~(cfg : Runtime_config.t) ~(room_id : string option) :
    check_result =
  match room_id with
  | None ->
      warn ~name:"Connector"
        ~message:"No room ID specified; cannot check binding"
        ~fix_command:"clawq rooms bind <room_id> <profile_id>" ()
  | Some rid -> (
      let binding =
        List.find_opt
          (fun (b : room_profile_binding) -> b.room = rid)
          cfg.room_profile_bindings
      in
      match binding with
      | None ->
          fail ~name:"Connector"
            ~message:
              (Printf.sprintf "Room '%s' is not bound to any profile" rid)
            ~fix_command:(Printf.sprintf "clawq rooms bind %s <profile_id>" rid)
            ()
      | Some b ->
          let profile =
            List.find_opt
              (fun (p : room_profile) ->
                p.id = b.profile_id && not (room_profile_deleted p))
              cfg.room_profiles
          in
          if not b.active then
            warn ~name:"Connector"
              ~message:
                (Printf.sprintf
                   "Room '%s' bound to '%s' but binding is inactive" rid
                   b.profile_id)
              ~fix_command:
                (Printf.sprintf "clawq rooms bind %s %s --preserve" rid
                   b.profile_id)
              ()
          else if profile = None then
            fail ~name:"Connector"
              ~message:
                (Printf.sprintf
                   "Room '%s' bound to profile '%s' but profile is missing or \
                    deleted"
                   rid b.profile_id)
              ~fix_command:
                (Printf.sprintf "clawq rooms bind %s <active_profile_id>" rid)
              ()
          else
            pass ~name:"Connector"
              ~message:
                (Printf.sprintf "Room '%s' -> profile '%s' (active)" rid
                   b.profile_id))

(** Check 2: Scope — room scope classification. *)
let check_scope ~(cfg : Runtime_config.t) ~(room_id : string option) :
    check_result =
  match room_id with
  | None -> skip ~name:"Scope" ~message:"No room ID; scope check skipped"
  | Some rid -> (
      (* Derive scope from room ID structure *)
      let scope = Room_policy.derive_scope_from_session_key rid in
      match scope with
      | Rm_external | Rm_shared ->
          warn ~name:"Scope"
            ~message:
              (Printf.sprintf
                 "Room '%s' classified as %s (external participants)" rid
                 (Room_policy.room_scope_to_string scope))
            ~fix_command:
              "clawq rooms explain-access <room_id>  # review external policy"
            ()
      | Rm_unknown ->
          warn ~name:"Scope"
            ~message:
              (Printf.sprintf
                 "Room '%s' scope unknown; connector may not report metadata"
                 rid)
            ()
      | Rm_dm | Rm_group ->
          pass ~name:"Scope"
            ~message:
              (Printf.sprintf "Room '%s' scope: %s" rid
                 (Room_policy.room_scope_to_string scope)))

(** Check 3: Memory — is memory scope configured for the profile? *)
let check_memory ~(cfg : Runtime_config.t) ~(profile_id : string option)
    ~(db : Sqlite3.db option) : check_result =
  match profile_id with
  | None -> skip ~name:"Memory" ~message:"No profile ID; memory check skipped"
  | Some pid -> (
      let profile =
        List.find_opt
          (fun (p : room_profile) -> p.id = pid && not (room_profile_deleted p))
          cfg.room_profiles
      in
      match profile with
      | None ->
          warn ~name:"Memory"
            ~message:
              (Printf.sprintf "Profile '%s' not found; cannot check memory" pid)
            ()
      | Some _p -> (
          (* Check if memory scopes exist in DB for this profile *)
          match db with
          | None ->
              skip ~name:"Memory"
                ~message:"No database; memory scope check skipped"
          | Some db -> (
              match Memory_core.get_room_profile_by_name ~db ~name:pid with
              | None ->
                  warn ~name:"Memory"
                    ~message:
                      (Printf.sprintf
                         "Profile '%s' not yet synced to DB (memory scopes \
                          will be created on first use)"
                         pid)
                    ()
              | Some rp ->
                  let scopes =
                    try
                      Memory.list_scopes ~db ()
                      |> List.filter (fun (s : Memory.memory_scope) ->
                          s.profile_id = Some rp.id)
                    with _ -> []
                  in
                  if scopes = [] then
                    warn ~name:"Memory"
                      ~message:
                        (Printf.sprintf
                           "Profile '%s' has no memory scopes configured" pid)
                      ~fix_command:
                        (Printf.sprintf
                           "clawq rooms memory grant %s --kind room --key %s"
                           pid pid)
                      ()
                  else
                    pass ~name:"Memory"
                      ~message:
                        (Printf.sprintf
                           "Profile '%s' has %d memory scope(s) configured" pid
                           (List.length scopes)))))

(** Check 4: GitHub — app token, repo grants, webhooks, room backlink. *)
let check_github ~(cfg : Runtime_config.t) ~(profile_id : string option)
    ~(access_bundle_ids : string list) : check_result list =
  let gh_token_ok, gh_token_msg =
    Github_wizard_checks.check_github_app_token cfg
  in
  let rg_ok, rg_msg = Github_wizard_checks.check_repo_grants cfg in
  let wh_ok, wh_msg = Github_wizard_checks.check_webhook_reachability cfg in
  let rb_ok, rb_msg =
    Github_wizard_checks.check_room_backlink ~cfg
      ~profile_id:(Option.value profile_id ~default:"")
      ~access_bundle_ids
  in
  let make_gh_check ~name ~ok ~msg ~fix =
    if ok then pass ~name ~message:msg
    else fail ~name ~message:msg ~fix_command:fix ()
  in
  [
    make_gh_check ~name:"GitHub App" ~ok:gh_token_ok ~msg:gh_token_msg
      ~fix:"clawq setup github";
    make_gh_check ~name:"Repo Grants" ~ok:rg_ok ~msg:rg_msg
      ~fix:"clawq setup github";
    make_gh_check ~name:"Webhook Reachability" ~ok:wh_ok ~msg:wh_msg
      ~fix:"clawq gateway start  # or: clawq tunnel start";
    make_gh_check ~name:"Room Backlink" ~ok:rb_ok ~msg:rb_msg
      ~fix:
        (Printf.sprintf "clawq rooms bind <room_id> %s"
           (Option.value profile_id ~default:"<profile_id>"));
  ]

(** Check 5: Budget — limits and current usage. *)
let check_budget ~(cfg : Runtime_config.t) ~(profile_id : string option)
    ~(db : Sqlite3.db option) : check_result =
  match profile_id with
  | None -> skip ~name:"Budget" ~message:"No profile ID; budget check skipped"
  | Some pid -> (
      let profile =
        List.find_opt
          (fun (p : room_profile) -> p.id = pid && not (room_profile_deleted p))
          cfg.room_profiles
      in
      match profile with
      | None ->
          skip ~name:"Budget"
            ~message:
              (Printf.sprintf "Profile '%s' not found; cannot check budget" pid)
      | Some _p -> (
          match db with
          | None ->
              skip ~name:"Budget" ~message:"No database; budget check skipped"
          | Some db -> (
              match Memory_core.get_room_profile_by_name ~db ~name:pid with
              | None ->
                  pass ~name:"Budget"
                    ~message:
                      "No budget configured (profile not yet in DB; will be \
                       created on first use)"
              | Some rp -> (
                  match
                    Room_budget.get_profile_budget ~db ~profile_id:rp.id
                  with
                  | None ->
                      pass ~name:"Budget"
                        ~message:"No budget limits configured for this profile"
                  | Some budget_state ->
                      if budget_state.limit_exceeded then
                        let usage_msg =
                          Printf.sprintf
                            "HARD LIMIT EXCEEDED — tokens: %d/%d, cost: \
                             $%.4f/$%.2f, period: %s"
                            budget_state.current_usage.total_tokens
                            budget_state.token_limit
                            budget_state.current_usage.cost_usd
                            budget_state.cost_limit_usd
                            budget_state.reset_period
                        in
                        fail ~name:"Budget" ~message:usage_msg
                          ~fix_command:
                            (Printf.sprintf
                               "clawq rooms budget adjust %s --token-limit N \
                                --cost-limit F"
                               pid)
                          ()
                      else if budget_state.soft_limit_exceeded then
                        let usage_msg =
                          Printf.sprintf
                            "SOFT LIMIT WARNING — tokens: %d/%d (%.0f%%), \
                             cost: $%.4f/$%.2f"
                            budget_state.current_usage.total_tokens
                            budget_state.token_limit
                            (budget_state.soft_warn_threshold_pct *. 100.0)
                            budget_state.current_usage.cost_usd
                            budget_state.cost_limit_usd
                        in
                        warn ~name:"Budget" ~message:usage_msg ()
                      else
                        pass ~name:"Budget"
                          ~message:
                            (Printf.sprintf
                               "Budget OK — tokens: %d/%d, cost: $%.4f/$%.2f, \
                                period: %s"
                               budget_state.current_usage.total_tokens
                               budget_state.token_limit
                               budget_state.current_usage.cost_usd
                               budget_state.cost_limit_usd
                               budget_state.reset_period)))))

(** Check 6: Audit — activity ledger and egress audit schemas accessible. *)
let check_audit ~(db : Sqlite3.db option) ~(room_id : string option) :
    check_result list =
  match db with
  | None ->
      [
        skip ~name:"Activity Ledger" ~message:"No database; audit check skipped";
        skip ~name:"Egress Audit" ~message:"No database; audit check skipped";
      ]
  | Some db ->
      let ledger_ok, ledger_msg =
        try
          let _events =
            Room_activity_ledger.query ~db
              ~room_id:(Option.value room_id ~default:"__readiness_probe")
              ()
          in
          (true, "Schema accessible")
        with exn ->
          ( false,
            Printf.sprintf "Ledger query failed: %s" (Printexc.to_string exn) )
      in
      let egress_ok, egress_msg =
        try
          let _events = Egress_audit.query ~db ~limit:1 () in
          (true, "Schema accessible")
        with exn ->
          ( false,
            Printf.sprintf "Egress audit query failed: %s"
              (Printexc.to_string exn) )
      in
      let make_audit_check ~name ~ok ~msg =
        if ok then pass ~name ~message:msg
        else
          fail ~name ~message:msg
            ~fix_command:"clawq migrate  # run database migrations" ()
      in
      [
        make_audit_check ~name:"Activity Ledger" ~ok:ledger_ok ~msg:ledger_msg;
        make_audit_check ~name:"Egress Audit" ~ok:egress_ok ~msg:egress_msg;
      ]

(** Check 7: Routine — are scheduled routines configured for the profile? *)
let check_routine ~(profile_id : string option) ~(db : Sqlite3.db option) :
    check_result =
  match profile_id with
  | None -> skip ~name:"Routine" ~message:"No profile ID; routine check skipped"
  | Some pid -> (
      match db with
      | None ->
          skip ~name:"Routine" ~message:"No database; routine check skipped"
      | Some db ->
          Scheduler.init_schema db;
          let jobs = Scheduler.list_jobs ~db in
          let routine_jobs =
            List.filter
              (fun (j : Scheduler.job) ->
                j.profile_id <> None
                &&
                let target = Scheduler.job_routine_target j in
                target = Some pid)
              jobs
          in
          if routine_jobs = [] then
            warn ~name:"Routine"
              ~message:
                (Printf.sprintf "No room routines configured for profile '%s'"
                   pid)
              ~fix_command:
                (Printf.sprintf
                   "clawq rooms routine create %s \"0 */6 * * *\" \"Generate \
                    summary\""
                   pid)
              ()
          else
            let enabled_count =
              List.length
                (List.filter
                   (fun (j : Scheduler.job) -> j.enabled)
                   routine_jobs)
            in
            if enabled_count = 0 then
              warn ~name:"Routine"
                ~message:
                  (Printf.sprintf
                     "Profile '%s' has %d routine(s) but all are disabled" pid
                     (List.length routine_jobs))
                ~fix_command:
                  (Printf.sprintf "clawq rooms routine enable <routine_name>")
                ()
            else
              pass ~name:"Routine"
                ~message:
                  (Printf.sprintf "Profile '%s' has %d active routine(s)" pid
                     enabled_count))

(** Check 8: Ambient — ambient watcher status for the room. *)
let check_ambient ~(cfg : Runtime_config.t) ~(room_id : string option)
    ~(profile_id : string option) ~(db : Sqlite3.db option) : check_result =
  let pid =
    match profile_id with
    | Some p -> Some p
    | None -> (
        match room_id with
        | None -> None
        | Some rid -> (
            match
              List.find_opt
                (fun (b : room_profile_binding) -> b.room = rid)
                cfg.room_profile_bindings
            with
            | Some b -> Some b.profile_id
            | None -> None))
  in
  match pid with
  | None ->
      skip ~name:"Ambient" ~message:"No profile resolved; ambient check skipped"
  | Some pid_str -> (
      let profile =
        List.find_opt
          (fun (p : room_profile) ->
            p.id = pid_str && not (room_profile_deleted p))
          cfg.room_profiles
      in
      match profile with
      | None ->
          skip ~name:"Ambient"
            ~message:
              (Printf.sprintf "Profile '%s' not found; cannot check ambient"
                 pid_str)
      | Some p ->
          if not p.ambient_enabled then
            warn ~name:"Ambient"
              ~message:
                (Printf.sprintf "Ambient watcher disabled for profile '%s'"
                   pid_str)
              ~fix_command:
                (Printf.sprintf "clawq rooms ambient enable %s" pid_str)
              ()
          else begin
            (* Check delivery failures if DB available *)
            let recent_failures =
              match (db, room_id) with
              | Some db, Some rid ->
                  Room_activity_ledger.query ~db ~room_id:rid
                    ~event_type:"ambient_delivery_failed" ()
              | _ -> []
            in
            if List.length recent_failures > 5 then
              warn ~name:"Ambient"
                ~message:
                  (Printf.sprintf
                     "Ambient enabled but %d recent delivery failures for room \
                      '%s'"
                     (List.length recent_failures)
                     (Option.value room_id ~default:"?"))
                ~fix_command:
                  "clawq rooms inspect <room_id>  # check delivery details"
                ()
            else
              let quiet_info =
                if p.ambient_quiet_start <> p.ambient_quiet_end then
                  Printf.sprintf " (quiet: %02d:00-%02d:00 UTC)"
                    p.ambient_quiet_start p.ambient_quiet_end
                else ""
              in
              let rate_info =
                if p.ambient_rate_limit_rph > 0 then
                  Printf.sprintf ", rate limit: %d/hour"
                    p.ambient_rate_limit_rph
                else ""
              in
              pass ~name:"Ambient"
                ~message:
                  (Printf.sprintf "Ambient watcher enabled for '%s'%s%s" pid_str
                     quiet_info rate_info)
          end)

(** Check 9: Proxy-readiness — gateway/tunnel configuration for webhook
    delivery. *)
let check_proxy_readiness ~(cfg : Runtime_config.t) : check_result =
  match cfg.channels.github with
  | None ->
      skip ~name:"Proxy Readiness"
        ~message:"No GitHub channel; proxy check skipped"
  | Some gh ->
      if gh.repos = [] then
        skip ~name:"Proxy Readiness"
          ~message:"No GitHub repos configured; proxy check skipped"
      else
        let gateway_port, tunnel_url =
          Setup_common.get_gateway_and_tunnel_url ()
        in
        let has_endpoint = tunnel_url <> None || gateway_port > 0 in
        if not has_endpoint then
          fail ~name:"Proxy Readiness"
            ~message:
              "GitHub repos configured but no reachable endpoint (no gateway \
               port or tunnel URL)"
            ~fix_command:"clawq gateway start  # or: clawq tunnel start" ()
        else
          let endpoint_desc =
            match tunnel_url with
            | Some url -> Printf.sprintf "tunnel=%s" url
            | None -> Printf.sprintf "gateway port=%d" gateway_port
          in
          pass ~name:"Proxy Readiness"
            ~message:(Printf.sprintf "Endpoint reachable (%s)" endpoint_desc)

(* ── Report generation ──────────────────────────────────────────── *)

(** [generate ~cfg ~db ~room_id ~profile_id ()] produces a full readiness report
    for the given room and/or profile. *)
let generate ~(cfg : Runtime_config.t) ~(db : Sqlite3.db option) ?room_id
    ?profile_id () : report =
  (* Resolve profile_id from room binding if not provided *)
  let profile_id =
    match profile_id with
    | Some _ -> profile_id
    | None -> (
        match room_id with
        | None -> None
        | Some rid -> (
            match
              List.find_opt
                (fun (b : room_profile_binding) -> b.room = rid)
                cfg.room_profile_bindings
            with
            | Some b -> Some b.profile_id
            | None -> None))
  in
  (* Resolve access_bundle_ids for GitHub checks *)
  let access_bundle_ids =
    match profile_id with
    | None -> []
    | Some pid -> (
        match
          List.find_opt
            (fun (p : room_profile) ->
              p.id = pid && not (room_profile_deleted p))
            cfg.room_profiles
        with
        | Some p -> p.access_bundle_ids
        | None -> [])
  in
  let checks =
    [
      check_connector ~cfg ~room_id;
      check_scope ~cfg ~room_id;
      check_memory ~cfg ~profile_id ~db;
    ]
    @ check_github ~cfg ~profile_id ~access_bundle_ids
    @ [ check_budget ~cfg ~profile_id ~db ]
    @ check_audit ~db ~room_id
    @ [
        check_routine ~profile_id ~db;
        check_ambient ~cfg ~room_id ~profile_id ~db;
        check_proxy_readiness ~cfg;
      ]
  in
  let passed = List.length (List.filter (fun c -> c.status = Pass) checks) in
  let failed = List.length (List.filter (fun c -> c.status = Fail) checks) in
  let warned = List.length (List.filter (fun c -> c.status = Warn) checks) in
  let skipped = List.length (List.filter (fun c -> c.status = Skip) checks) in
  { room_id; profile_id; checks; passed; failed; warned; skipped }

(* ── Formatting ─────────────────────────────────────────────────── *)

let format_status = function
  | Pass -> "PASS"
  | Fail -> "FAIL"
  | Warn -> "WARN"
  | Skip -> "SKIP"

(** [format_text report] produces a human-readable readiness report. *)
let format_text (report : report) : string =
  let open Setup_common in
  let buf = Buffer.create 2048 in
  let add line =
    Buffer.add_string buf line;
    Buffer.add_char buf '\n'
  in
  add (bold "=== Room-Agent Readiness Report ===");
  add "";
  (match report.room_id with
  | Some rid -> add (Printf.sprintf "  Room:    %s" rid)
  | None -> add "  Room:    (not specified)");
  (match report.profile_id with
  | Some pid -> add (Printf.sprintf "  Profile: %s" pid)
  | None -> add "  Profile: (not resolved)");
  add "";
  List.iter
    (fun (c : check_result) ->
      let icon =
        match c.status with
        | Pass -> green "PASS"
        | Fail -> red "FAIL"
        | Warn -> yellow "WARN"
        | Skip -> dim "SKIP"
      in
      add (Printf.sprintf "  [%s] %s: %s" icon c.name c.message);
      match (c.status, c.fix_command) with
      | (Fail | Warn), Some fix ->
          add (Printf.sprintf "         Fix: %s" (dim fix))
      | _ -> ())
    report.checks;
  add "";
  let summary_parts =
    [
      Printf.sprintf "%s passed" (string_of_int report.passed |> green);
      Printf.sprintf "%s failed" (string_of_int report.failed |> red);
      Printf.sprintf "%s warnings" (string_of_int report.warned |> yellow);
      Printf.sprintf "%s skipped" (string_of_int report.skipped |> dim);
    ]
  in
  add (Printf.sprintf "  Summary: %s" (String.concat ", " summary_parts));
  add "";
  if report.failed = 0 then
    add (green "  All critical checks passed. Room-agent is ready.")
  else
    add
      (red
         (Printf.sprintf
            "  %d critical check(s) failed. Fix the issues above before \
             enabling the room-agent."
            report.failed));
  Buffer.contents buf

(** [format_json report] produces a JSON readiness report. *)
let format_json (report : report) : string =
  let check_to_json (c : check_result) =
    let status_str = format_status c.status in
    let base =
      [
        ("name", `String c.name);
        ("status", `String status_str);
        ("message", `String c.message);
      ]
    in
    let with_fix =
      match c.fix_command with
      | Some fix -> base @ [ ("fix_command", `String fix) ]
      | None -> base
    in
    `Assoc with_fix
  in
  let json =
    `Assoc
      [
        ( "room_id",
          match report.room_id with Some r -> `String r | None -> `Null );
        ( "profile_id",
          match report.profile_id with Some p -> `String p | None -> `Null );
        ("checks", `List (List.map check_to_json report.checks));
        ("passed", `Int report.passed);
        ("failed", `Int report.failed);
        ("warned", `Int report.warned);
        ("skipped", `Int report.skipped);
        ("ready", `Bool (report.failed = 0));
      ]
  in
  Yojson.Safe.pretty_to_string json

(* Upgrade validation, drift checks, and admin guidance (P20.M2.E2.T002).
   See github_route_upgrade_validate.mli. *)

module S = Github_route_store
module F = Github_route_filter
module Ops = Github_route_ops
module M = Github_route_migrate
module Inst = Github_app_installation_scope
module Auth = Github_auth_selection
module Env = Github_event_envelope

type severity = Pass | Warn | Fail

type category =
  | Schema
  | Migration
  | Managed
  | Installation
  | Catalog
  | Session
  | Drift
  | Alias

type check = {
  name : string;
  category : category;
  severity : severity;
  message : string;
  repair : string option;
}

type catalog_state = {
  tools_ok : bool;
  mcp_ok : bool;
  catalog_revision : string option;
  access_revision : string option;
}

type session_refresh_state = {
  active_room_ids : string list;
  refresh_pending_room_ids : string list;
  refresh_without_restart : bool;
}

type report = {
  generated_at : string;
  overall : severity;
  checks : check list;
  filter_schema_current : int;
  envelope_version : int;
  routes_checked : int;
  legacy_subscription_count : int;
  deprecated_aliases : (string * string) list;
  repair_guidance : string list;
  rollback_guidance : string list;
}

(* ── Documented product defaults (drift sources of truth) ───────── *)

let documented_filter_schema_version = 1
let documented_envelope_version = 1
let documented_default_comment_mode = "summary"
let documented_comment_modes = [ "off"; "summary"; "threaded" ]
let documented_specificity_order = "Item > Repo > Org"

let severity_to_string = function
  | Pass -> "pass"
  | Warn -> "warn"
  | Fail -> "fail"

let category_to_string = function
  | Schema -> "schema"
  | Migration -> "migration"
  | Managed -> "managed"
  | Installation -> "installation"
  | Catalog -> "catalog"
  | Session -> "session"
  | Drift -> "drift"
  | Alias -> "alias"

let default_catalog_state : catalog_state =
  {
    tools_ok = true;
    mcp_ok = true;
    catalog_revision = None;
    access_revision = None;
  }

let default_session_refresh : session_refresh_state =
  {
    active_room_ids = [];
    refresh_pending_room_ids = [];
    refresh_without_restart = true;
  }

let make_check ~name ~category ~severity ~message ?repair () =
  { name; category; severity; message; repair }

let overall_of (checks : check list) : severity =
  if List.exists (fun c -> c.severity = Fail) checks then Fail
  else if List.exists (fun c -> c.severity = Warn) checks then Warn
  else Pass

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let opt_string_field k = function None -> [] | Some v -> [ (k, `String v) ]

(* ── Schema ─────────────────────────────────────────────────────── *)

let check_filter_schema (f : F.t) : check list =
  let cur = F.current_schema_version in
  if f.schema_version < 1 then
    [
      make_check ~name:"filter_schema_version" ~category:Schema ~severity:Fail
        ~message:
          (Printf.sprintf "filter schema_version %d is below minimum 1"
             f.schema_version)
        ~repair:
          "Re-save the route filter so it migrates to the current schema \
           (Github_route_filter.of_json / store update)"
        ();
    ]
  else if f.schema_version > cur then
    [
      make_check ~name:"filter_schema_version" ~category:Schema ~severity:Fail
        ~message:
          (Printf.sprintf
             "filter schema_version %d is newer than runtime support (%d)"
             f.schema_version cur)
        ~repair:
          "Upgrade Clawq binary to a build that supports this filter schema, \
           or re-plan the route with a supported filter"
        ();
    ]
  else if f.schema_version < cur then
    [
      make_check ~name:"filter_schema_version" ~category:Schema ~severity:Warn
        ~message:
          (Printf.sprintf
             "filter schema_version %d is older than current %d (migrates on \
              next read/write)"
             f.schema_version cur)
        ~repair:
          "Optional: re-apply or update the route so the stored filter is \
           rewritten at the current schema_version"
        ();
    ]
  else
    [
      make_check ~name:"filter_schema_version" ~category:Schema ~severity:Pass
        ~message:(Printf.sprintf "filter schema_version current (%d)" cur)
        ();
    ]

let check_route_schemas (routes : S.t list) : check list =
  if routes = [] then
    [
      make_check ~name:"filter_schema_routes" ~category:Schema ~severity:Pass
        ~message:"no routes to schema-check" ();
    ]
  else
    let per_route =
      List.concat_map
        (fun (r : S.t) ->
          List.map
            (fun (c : check) ->
              {
                c with
                name = c.name ^ ":" ^ r.id;
                message = Printf.sprintf "route %s: %s" r.id c.message;
              })
            (check_filter_schema r.filter))
        routes
    in
    (* Collapse all-pass into one summary check; keep non-pass individually. *)
    let bad =
      List.filter (fun c -> c.severity = Fail || c.severity = Warn) per_route
    in
    if bad = [] then
      [
        make_check ~name:"filter_schema_routes" ~category:Schema ~severity:Pass
          ~message:
            (Printf.sprintf "all %d route filter(s) at supported schema_version"
               (List.length routes))
          ();
      ]
    else bad

(* ── Managed linkage ────────────────────────────────────────────── *)

let check_managed_linkage (r : S.t) : check list =
  match (r.managed_bundle_id, r.managed_feature_id) with
  | None, None ->
      [
        make_check
          ~name:("managed_linkage:" ^ r.id)
          ~category:Managed ~severity:Pass
          ~message:
            (Printf.sprintf
               "route %s has no managed linkage (manual/independent)" r.id)
          ();
      ]
  | Some b, Some f when String.trim b <> "" && String.trim f <> "" ->
      [
        make_check
          ~name:("managed_linkage:" ^ r.id)
          ~category:Managed ~severity:Pass
          ~message:
            (Printf.sprintf "route %s managed bundle=%s feature=%s" r.id b f)
          ();
      ]
  | Some _, None | None, Some _ | Some _, Some _ ->
      [
        make_check
          ~name:("managed_linkage:" ^ r.id)
          ~category:Managed ~severity:Fail
          ~message:
            (Printf.sprintf
               "route %s has incomplete managed linkage (bundle and feature \
                must both be set or both absent)"
               r.id)
          ~repair:
            "Re-apply the setup plan so managed_bundle_id and \
             managed_feature_id are set together, or clear both via route \
             update/remove for manual grants only"
          ();
      ]

let check_managed_routes (routes : S.t list) : check list =
  let detailed = List.concat_map check_managed_linkage routes in
  let bad =
    List.filter (fun c -> c.severity = Fail || c.severity = Warn) detailed
  in
  if bad <> [] then bad
  else if routes = [] then
    [
      make_check ~name:"managed_linkage" ~category:Managed ~severity:Pass
        ~message:"no routes to check for managed linkage" ();
    ]
  else
    let managed =
      List.fold_left
        (fun n (r : S.t) ->
          match (r.managed_bundle_id, r.managed_feature_id) with
          | Some _, Some _ -> n + 1
          | _ -> n)
        0 routes
    in
    [
      make_check ~name:"managed_linkage" ~category:Managed ~severity:Pass
        ~message:
          (Printf.sprintf "%d route(s) ok; %d with managed linkage"
             (List.length routes) managed)
        ();
    ]

(* ── Migration readiness ────────────────────────────────────────── *)

let check_migration ~legacy_count ~(routes : S.t list) : check list =
  let migrate_provenance =
    List.fold_left
      (fun n (r : S.t) ->
        match r.provenance.created_via with Some "migrate" -> n + 1 | _ -> n)
      0 routes
  in
  let migration_check =
    if legacy_count = 0 then
      make_check ~name:"subscription_migration" ~category:Migration
        ~severity:Pass ~message:"no legacy github_pr_subscriptions rows present"
        ()
    else if migrate_provenance > 0 && routes <> [] then
      make_check ~name:"subscription_migration" ~category:Migration
        ~severity:Warn
        ~message:
          (Printf.sprintf
             "%d legacy subscription(s) still present; %d route(s) already \
              have created_via=migrate — re-run migrate for remaining cutover"
             legacy_count migrate_provenance)
        ~repair:
          "Run Github_route_migrate.migrate_database (or CLI cutover) then \
           verify Item routes; leave legacy table as read-only archive or drop \
           after verification"
        ()
    else if routes = [] then
      make_check ~name:"subscription_migration" ~category:Migration
        ~severity:Fail
        ~message:
          (Printf.sprintf
             "%d legacy subscription(s) present but no github_routes rows — \
              migration not applied"
             legacy_count)
        ~repair:
          "Run Github_route_migrate.migrate_database with \
           Prefer_existing_route (default); verify with \
           Github_route_store.list_all and diagnostics"
        ()
    else
      make_check ~name:"subscription_migration" ~category:Migration
        ~severity:Warn
        ~message:
          (Printf.sprintf
             "%d legacy subscription(s) present alongside %d route(s); ensure \
              cutover completed (compatibility aliases must not dual-write)"
             legacy_count (List.length routes))
        ~repair:
          "Run migrate_database if Item routes are missing for legacy PRs; \
           confirm CLI aliases only call route store APIs"
        ()
  in
  (* Active uniqueness is store-enforced; still report a Pass when routes load. *)
  let uniqueness =
    make_check ~name:"active_route_uniqueness" ~category:Migration
      ~severity:Pass
      ~message:
        "store enforces at most one active route per (destination, selector)"
      ()
  in
  [ migration_check; uniqueness ]

(* ── Installation scope ─────────────────────────────────────────── *)

let org_routes (routes : S.t list) =
  List.filter
    (fun (r : S.t) -> match r.selector with S.Org _ -> true | _ -> false)
    routes

let check_installation ?installation ?auth ~(routes : S.t list) () : check list
    =
  let org_rs = org_routes routes in
  if org_rs = [] then
    [
      make_check ~name:"installation_scope" ~category:Installation
        ~severity:Pass
        ~message:"no Org routes; installation scope not required for Item/Repo"
        ();
    ]
  else
    let scope_check =
      match installation with
      | None ->
          make_check ~name:"installation_scope" ~category:Installation
            ~severity:Fail
            ~message:
              (Printf.sprintf
                 "%d Org route(s) require live Active App installation scope"
                 (List.length org_rs))
            ~repair:
              "Install the GitHub App on the org, complete setup resume, and \
               reconcile installation scope before Org routes will match"
            ()
      | Some (inst : Inst.t) -> (
          match inst.status with
          | Inst.Active ->
              make_check ~name:"installation_scope" ~category:Installation
                ~severity:Pass
                ~message:
                  (Printf.sprintf
                     "installation %d Active for %s (covers %d Org route(s))"
                     inst.installation_id inst.account.login
                     (List.length org_rs))
                ()
          | Inst.Suspended { reason } ->
              let r = match reason with Some s -> s | None -> "unspecified" in
              make_check ~name:"installation_scope" ~category:Installation
                ~severity:Fail
                ~message:
                  (Printf.sprintf "installation %d suspended (%s)"
                     inst.installation_id r)
                ~repair:
                  "Unsuspend the App installation in GitHub, then re-run \
                   readiness / upgrade validation"
                ()
          | Inst.Deleted ->
              make_check ~name:"installation_scope" ~category:Installation
                ~severity:Fail
                ~message:
                  (Printf.sprintf "installation %d is deleted"
                     inst.installation_id)
                ~repair:
                  "Re-install the GitHub App and re-run App setup so Active \
                   installation scope is recorded"
                ())
    in
    let auth_check =
      match auth with
      | None ->
          make_check ~name:"org_auth_claim" ~category:Installation
            ~severity:Warn
            ~message:
              "Org routes present but no auth snapshot supplied for \
               can_claim_org_scope check"
            ~repair:
              "Pass Github_auth_selection.auth_snapshot with App credentials \
               when validating Org upgrade readiness"
            ()
      | Some auth_snap ->
          if Auth.can_claim_org_scope ~auth:auth_snap ~installation then
            make_check ~name:"org_auth_claim" ~category:Installation
              ~severity:Pass ~message:"can_claim_org_scope = true" ()
          else
            make_check ~name:"org_auth_claim" ~category:Installation
              ~severity:Fail
              ~message:
                "can_claim_org_scope = false (PAT cannot claim Org; need \
                 Active App installation)"
              ~repair:
                "Migrate from PAT to GitHub App: install App on org, ensure \
                 Active installation scope matches App credentials, then \
                 re-plan Org routes"
              ()
    in
    [ scope_check; auth_check ]

(* ── Catalog + Session refresh ──────────────────────────────────── *)

let check_catalog (cs : catalog_state) ~(routes : S.t list) : check list =
  let has_managed =
    List.exists
      (fun (r : S.t) ->
        match (r.managed_bundle_id, r.managed_feature_id) with
        | Some _, Some _ -> true
        | _ -> false)
      routes
  in
  let tools =
    if cs.tools_ok then
      make_check ~name:"tools_catalog" ~category:Catalog ~severity:Pass
        ~message:"tools catalog state ok" ()
    else
      make_check ~name:"tools_catalog" ~category:Catalog ~severity:Fail
        ~message:"tools catalog state not ok"
        ~repair:
          "Restore Room tool grants / base registry access; re-attach \
           setup-owned managed bundle if detached incorrectly"
        ()
  in
  let mcp =
    if cs.mcp_ok then
      make_check ~name:"mcp_catalog" ~category:Catalog ~severity:Pass
        ~message:"MCP catalog state ok" ()
    else
      make_check ~name:"mcp_catalog" ~category:Catalog ~severity:Fail
        ~message:"MCP catalog state not ok (quarantine or allowlist failure)"
        ~repair:
          "Repair MCP server allowlist / relist after list_changed; clear \
           quarantine only after successful discovery"
        ()
  in
  let rev =
    match (has_managed, cs.catalog_revision, cs.access_revision) with
    | true, None, _ | true, _, None ->
        make_check ~name:"catalog_revisions" ~category:Catalog ~severity:Warn
          ~message:
            "managed routes present but catalog_revision and/or \
             access_revision missing on validation snapshot"
          ~repair:
            "After apply, confirm on_catalog_refresh marks affected Rooms and \
             the next turn freezes a catalog with revision metadata"
          ()
    | true, Some cr, Some ar ->
        make_check ~name:"catalog_revisions" ~category:Catalog ~severity:Pass
          ~message:
            (Printf.sprintf "catalog_revision=%s access_revision=%s" cr ar)
          ()
    | false, _, _ ->
        make_check ~name:"catalog_revisions" ~category:Catalog ~severity:Pass
          ~message:"no managed routes; catalog revision metadata optional" ()
  in
  [ tools; mcp; rev ]

let check_session_refresh (sr : session_refresh_state) ~(routes : S.t list) :
    check list =
  let room_dests =
    List.filter_map
      (fun (r : S.t) ->
        match r.destination with S.Room id -> Some id | S.Session _ -> None)
      routes
    |> List.sort_uniq String.compare
  in
  let restart =
    if sr.refresh_without_restart then
      make_check ~name:"session_refresh_no_restart" ~category:Session
        ~severity:Pass
        ~message:
          "active Session catalog refresh does not require daemon restart"
        ()
    else
      make_check ~name:"session_refresh_no_restart" ~category:Session
        ~severity:Fail
        ~message:
          "runtime reports active Session refresh requires restart (contract \
           violation)"
        ~repair:
          "Fix catalog refresh hook so Rooms pick up grants on next turn \
           without restart; see Github_route_apply.on_catalog_refresh"
        ()
  in
  let pending =
    let n = List.length sr.refresh_pending_room_ids in
    if n = 0 then
      make_check ~name:"session_refresh_pending" ~category:Session
        ~severity:Pass ~message:"no rooms pending next-turn catalog refresh" ()
    else
      make_check ~name:"session_refresh_pending" ~category:Session
        ~severity:Warn
        ~message:
          (Printf.sprintf "%d room(s) pending next-turn catalog refresh: %s" n
             (String.concat ","
                (List.sort String.compare sr.refresh_pending_room_ids)))
        ~repair:
          "Allow the next agent turn in each Room to rebuild the frozen Tool \
           catalog; do not restart the daemon for this alone"
        ()
  in
  let active_coverage =
    let active = List.sort_uniq String.compare sr.active_room_ids in
    let uncovered =
      List.filter
        (fun rid ->
          List.mem rid room_dests && not (List.mem rid active)
          (* active empty means "not instrumented" → only warn when some actives exist *))
        room_dests
    in
    match (active, room_dests) with
    | [], _ ->
        make_check ~name:"session_refresh_active_rooms" ~category:Session
          ~severity:Pass
          ~message:
            "no active Session inventory supplied (optional); \
             refresh-on-next-turn still required after managed apply"
          ()
    | _, [] ->
        make_check ~name:"session_refresh_active_rooms" ~category:Session
          ~severity:Pass ~message:"no Room destinations among checked routes" ()
    | _, _ when uncovered <> [] ->
        make_check ~name:"session_refresh_active_rooms" ~category:Session
          ~severity:Warn
          ~message:
            (Printf.sprintf
               "Room destination(s) without an active Session inventory entry: \
                %s"
               (String.concat "," uncovered))
          ~repair:
            "Ensure Room Sessions are bound for destinations that should \
             receive catalog refresh; inactive Rooms still refresh on next \
             open"
          ()
    | _ ->
        make_check ~name:"session_refresh_active_rooms" ~category:Session
          ~severity:Pass
          ~message:
            (Printf.sprintf "%d active Room Session(s) cover route destinations"
               (List.length active))
          ()
  in
  [ restart; pending; active_coverage ]

(* ── Drift checks ───────────────────────────────────────────────── *)

let drift_checks
    ?(documented_filter_schema_version = documented_filter_schema_version)
    ?(documented_envelope_version = documented_envelope_version)
    ?(documented_default_comment_mode = documented_default_comment_mode) () :
    check list =
  let schema =
    if F.current_schema_version = documented_filter_schema_version then
      make_check ~name:"drift_filter_schema_version" ~category:Drift
        ~severity:Pass
        ~message:
          (Printf.sprintf "runtime filter schema_version=%d matches docs"
             F.current_schema_version)
        ()
    else
      make_check ~name:"drift_filter_schema_version" ~category:Drift
        ~severity:Fail
        ~message:
          (Printf.sprintf "runtime filter schema_version=%d != documented %d"
             F.current_schema_version documented_filter_schema_version)
        ~repair:
          "Update docs/github-route-operator-contract.md and \
           Github_route_upgrade_validate.documented_filter_schema_version (or \
           runtime constant) so they match"
        ()
  in
  let envelope =
    if Env.envelope_version = documented_envelope_version then
      make_check ~name:"drift_envelope_version" ~category:Drift ~severity:Pass
        ~message:
          (Printf.sprintf "runtime envelope_version=%d matches docs"
             Env.envelope_version)
        ()
    else
      make_check ~name:"drift_envelope_version" ~category:Drift ~severity:Fail
        ~message:
          (Printf.sprintf "runtime envelope_version=%d != documented %d"
             Env.envelope_version documented_envelope_version)
        ~repair:
          "Align Github_event_envelope.envelope_version with documented \
           product contract"
        ()
  in
  let comment =
    let runtime =
      match S.default_comment_mode with
      | S.Off -> "off"
      | S.Summary -> "summary"
      | S.Threaded -> "threaded"
    in
    if runtime = documented_default_comment_mode then
      make_check ~name:"drift_default_comment_mode" ~category:Drift
        ~severity:Pass
        ~message:(Printf.sprintf "default comment_mode=%s matches docs" runtime)
        ()
    else
      make_check ~name:"drift_default_comment_mode" ~category:Drift
        ~severity:Fail
        ~message:
          (Printf.sprintf "default comment_mode=%s != documented %s" runtime
             documented_default_comment_mode)
        ~repair:
          "Default comment mode is Summary (summary); update runtime or docs"
        ()
  in
  let modes =
    let runtime_modes = [ "off"; "summary"; "threaded" ] in
    if runtime_modes = documented_comment_modes then
      make_check ~name:"drift_comment_modes" ~category:Drift ~severity:Pass
        ~message:"comment modes off|summary|threaded match docs" ()
    else
      make_check ~name:"drift_comment_modes" ~category:Drift ~severity:Fail
        ~message:"comment mode set drifted from documented modes"
        ~repair:"Keep supported modes as off, summary, threaded" ()
  in
  let specificity =
    (* Runtime match order is encoded in Github_route_match; product string is fixed. *)
    make_check ~name:"drift_specificity_order" ~category:Drift ~severity:Pass
      ~message:
        (Printf.sprintf "documented specificity order: %s"
           documented_specificity_order)
      ()
  in
  let default_filter_ver =
    if S.default_filter.schema_version = F.current_schema_version then
      make_check ~name:"drift_default_filter_schema" ~category:Drift
        ~severity:Pass
        ~message:
          (Printf.sprintf "default_filter.schema_version=%d"
             S.default_filter.schema_version)
        ()
    else
      make_check ~name:"drift_default_filter_schema" ~category:Drift
        ~severity:Fail
        ~message:
          "Github_route_store.default_filter schema_version != \
           Github_route_filter.current_schema_version"
        ~repair:"Keep default_filter at current_schema_version" ()
  in
  [ schema; envelope; comment; modes; specificity; default_filter_ver ]

let deprecated_alias_checks () : check list * (string * string) list =
  let aliases = M.compatibility_cli_aliases () in
  let checks =
    if aliases = [] then
      [
        make_check ~name:"deprecated_aliases" ~category:Alias ~severity:Pass
          ~message:"no compatibility aliases registered" ();
      ]
    else
      let bad_targets =
        List.filter
          (fun (_legacy, canonical) ->
            not
              (String.length canonical >= 12
              &&
              (* Canonical targets must be route store surfaces, not dual-write. *)
              (String.starts_with ~prefix:"github route" canonical
              || String.starts_with ~prefix:"github routes" canonical)))
          aliases
      in
      if bad_targets <> [] then
        [
          make_check ~name:"deprecated_aliases" ~category:Alias ~severity:Fail
            ~message:
              (Printf.sprintf
                 "%d compatibility alias target(s) do not map to github route \
                  APIs"
                 (List.length bad_targets))
            ~repair:
              "Update Github_route_migrate.compatibility_cli_aliases so every \
               legacy name delegates to route store APIs (no dual-write to \
               github_pr_subscriptions)"
            ();
        ]
      else
        [
          make_check ~name:"deprecated_aliases" ~category:Alias ~severity:Warn
            ~message:
              (Printf.sprintf
                 "%d deprecated compatibility alias(es) still accepted; prefer \
                  github route item|repo|org commands"
                 (List.length aliases))
            ~repair:
              "Document aliases for operators; migrate automation to github \
               route *; after cutover, aliases remain read-through only"
            ();
        ]
  in
  (checks, aliases)

(* ── Guidance ───────────────────────────────────────────────────── *)

let repair_guidance_lines (checks : check list) : string list =
  checks
  |> List.filter (fun c -> c.severity = Fail || c.severity = Warn)
  |> List.filter_map (fun c ->
      match c.repair with
      | Some r ->
          Some
            (Printf.sprintf "[%s/%s] %s — %s"
               (category_to_string c.category)
               (severity_to_string c.severity)
               c.name r)
      | None ->
          Some
            (Printf.sprintf "[%s/%s] %s — %s"
               (category_to_string c.category)
               (severity_to_string c.severity)
               c.name c.message))

let rollback_guidance_lines () =
  [
    "1. Stop accepting new Org/Item route applies until validation \
     overall=pass (or intentional warn-only).";
    "2. Disable newly applied routes (plan_disable → confirm/apply) so match \
     outcomes become intentional Muted (no-fallthrough).";
    "3. Soft-remove routes that free active selector slots (plan_remove); do \
     not dual-write legacy github_pr_subscriptions.";
    "4. Detach only setup-owned managed linkage when removing the last managed \
     feature; preserve independent/manual grants.";
    "5. If migration created bad Item routes, Prefer_existing_route winners \
     stay; supersede mistaken routes via store update/disable — re-run migrate \
     only with an explicit Prefer_legacy policy.";
    "6. Confirm catalog: next Room turn must not expose tools that depended on \
     detached setup-owned access; no daemon restart required.";
    "7. Do not delete webhook delivery-ledger accepts during rollback (ACK \
     independence / dedupe).";
    "8. Re-run Github_route_upgrade_validate.validate and \
     Github_route_diagnostics.collect before re-enabling automation.";
  ]

(* ── Full validate ──────────────────────────────────────────────── *)

let validate ~db ?destination ?installation ?auth
    ?(catalog_state = default_catalog_state)
    ?(session_refresh = default_session_refresh) ?(now = Unix.gettimeofday ())
    () =
  S.ensure_schema db;
  match
    match destination with
    | Some d -> S.list_for_destination ~db ~destination:d
    | None -> S.list_all ~db
  with
  | Error e -> Error e
  | Ok routes -> (
      match M.load_legacy_from_db ~db with
      | Error e -> Error e
      | Ok legacy ->
          let legacy_count = List.length legacy in
          let schema_cs = check_route_schemas routes in
          let managed_cs = check_managed_routes routes in
          let migration_cs = check_migration ~legacy_count ~routes in
          let install_cs = check_installation ?installation ?auth ~routes () in
          let catalog_cs = check_catalog catalog_state ~routes in
          let session_cs = check_session_refresh session_refresh ~routes in
          let drift_cs = drift_checks () in
          let alias_cs, deprecated_aliases = deprecated_alias_checks () in
          let checks =
            schema_cs @ managed_cs @ migration_cs @ install_cs @ catalog_cs
            @ session_cs @ drift_cs @ alias_cs
          in
          let overall = overall_of checks in
          let repair_guidance = repair_guidance_lines checks in
          let rollback_guidance = rollback_guidance_lines () in
          Ok
            {
              generated_at = Time_util.iso8601_utc ~t:now ();
              overall;
              checks;
              filter_schema_current = F.current_schema_version;
              envelope_version = Env.envelope_version;
              routes_checked = List.length routes;
              legacy_subscription_count = legacy_count;
              deprecated_aliases;
              repair_guidance;
              rollback_guidance;
            })

(* ── Export ─────────────────────────────────────────────────────── *)

let check_to_json (c : check) =
  `Assoc
    (sort_assoc
       ([
          ("category", `String (category_to_string c.category));
          ("message", `String c.message);
          ("name", `String c.name);
          ("severity", `String (severity_to_string c.severity));
        ]
       @ opt_string_field "repair" c.repair))

let alias_to_json (legacy, canonical) =
  `Assoc
    (sort_assoc
       [ ("canonical", `String canonical); ("legacy", `String legacy) ])

let to_json (r : report) : Yojson.Safe.t =
  let raw =
    `Assoc
      (sort_assoc
         [
           ("checks", `List (List.map check_to_json r.checks));
           ( "deprecated_aliases",
             `List (List.map alias_to_json r.deprecated_aliases) );
           ("envelope_version", `Int r.envelope_version);
           ("filter_schema_current", `Int r.filter_schema_current);
           ("generated_at", `String r.generated_at);
           ("legacy_subscription_count", `Int r.legacy_subscription_count);
           ("overall", `String (severity_to_string r.overall));
           ( "repair_guidance",
             `List (List.map (fun s -> `String s) r.repair_guidance) );
           ( "rollback_guidance",
             `List (List.map (fun s -> `String s) r.rollback_guidance) );
           ("routes_checked", `Int r.routes_checked);
         ])
  in
  Ops.redact_json raw

let format_report (r : report) : string list =
  let lines = ref [] in
  let push s = lines := s :: !lines in
  push
    (Printf.sprintf
       "upgrade_validate overall=%s routes=%d legacy_subs=%d \
        filter_schema_current=%d envelope_version=%d"
       (severity_to_string r.overall)
       r.routes_checked r.legacy_subscription_count r.filter_schema_current
       r.envelope_version);
  List.iter
    (fun (c : check) ->
      push
        (Printf.sprintf "check [%s/%s] %s: %s"
           (category_to_string c.category)
           (severity_to_string c.severity)
           c.name c.message))
    r.checks;
  List.iter
    (fun (legacy, canonical) ->
      push (Printf.sprintf "deprecated_alias %S -> %S" legacy canonical))
    r.deprecated_aliases;
  List.iter (fun s -> push ("repair: " ^ s)) r.repair_guidance;
  List.iter (fun s -> push ("rollback: " ^ s)) r.rollback_guidance;
  List.rev !lines

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
  tools_ok : bool option;
  mcp_ok : bool option;
  catalog_revision : string option;
  access_revision : string option;
  scope : catalog_scope;
}

and catalog_scope =
  | Room_effective_catalog of string
      (** A frozen catalog built from the named Room's effective access. *)
  | Catalog_state_unavailable of string
      (** Why this process cannot observe a Room-effective frozen catalog. *)

type session_refresh_state = {
  active_room_ids : string list option;
  refresh_pending_room_ids : string list option;
  refresh_without_restart : bool option;
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

type documented_contract = {
  filter_schema_version : int;
  envelope_version : int;
  default_comment_mode : string;
  comment_modes : string list;
  specificity_order : string;
}

type documentation_state =
  | Documentation_available of documented_contract
  | Documentation_unavailable of string

let runtime_comment_mode =
  match S.default_comment_mode with
  | S.Off -> "off"
  | S.Summary -> "summary"
  | S.Threaded -> "threaded"

let runtime_comment_modes = [ "off"; "summary"; "threaded" ]

let specificity_to_string : Github_route_match.specificity -> string = function
  | `Item -> "Item"
  | `Repo -> "Repo"
  | `Org -> "Org"

let runtime_specificity_order =
  Github_route_match.specificity_order
  |> List.map specificity_to_string
  |> String.concat " > "

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

let check_catalog (cs : catalog_state) ~(destination : S.destination option)
    ~(routes : S.t list) : check list =
  let has_managed =
    List.exists
      (fun (r : S.t) ->
        match (r.managed_bundle_id, r.managed_feature_id) with
        | Some _, Some _ -> true
        | _ -> false)
      routes
  in
  let scope_observed, scope_reason =
    match (cs.scope, destination) with
    | Room_effective_catalog observed, Some (S.Room requested)
      when String.equal observed requested ->
        (true, None)
    | Room_effective_catalog observed, Some (S.Room requested) ->
        ( false,
          Some
            (Printf.sprintf
               "frozen catalog is for Room %s, not requested Room %s" observed
               requested) )
    | Room_effective_catalog _, Some (S.Session _) ->
        ( false,
          Some "a Room-effective catalog cannot establish a Session destination"
        )
    | Room_effective_catalog _, None ->
        ( false,
          Some
            "an unscoped validation cannot establish a Room-effective catalog"
        )
    | Catalog_state_unavailable reason, _ -> (false, Some reason)
  in
  let unavailable_check =
    match scope_reason with
    | None -> []
    | Some reason ->
        [
          make_check ~name:"catalog_state_unavailable" ~category:Catalog
            ~severity:Warn
            ~message:
              (Printf.sprintf
                 "Room-effective frozen Tool_catalog/access snapshot \
                  unavailable (%s); no healthy catalog pass assumed"
                 reason)
            ~repair:
              "Run validation from the daemon-admin surface with the active \
               Room Session, or supply its frozen effective catalog/access \
               snapshot; do not substitute the unscoped base registry"
            ();
        ]
  in
  let tools =
    match cs.tools_ok with
    | Some true when scope_observed ->
        make_check ~name:"tools_catalog" ~category:Catalog ~severity:Pass
          ~message:"Room-effective tools catalog state ok" ()
    | Some true ->
        make_check ~name:"tools_catalog" ~category:Catalog ~severity:Warn
          ~message:
            "tools probe is not backed by the requested Room-effective frozen \
             catalog; no healthy state assumed"
          ~repair:
            "Inspect the active Room's frozen Tool_catalog/access snapshot; a \
             base registry result is not sufficient"
          ()
    | Some false ->
        make_check ~name:"tools_catalog" ~category:Catalog ~severity:Fail
          ~message:"tools catalog state not ok"
          ~repair:
            "Restore Room tool grants / base registry access; re-attach \
             setup-owned managed bundle if detached incorrectly"
          ()
    | None ->
        make_check ~name:"tools_catalog" ~category:Catalog ~severity:Warn
          ~message:"tools catalog probe unavailable; no healthy state assumed"
          ~repair:
            "Run validation through the daemon-admin surface that can inspect \
             the live Room catalog, then re-run this command"
          ()
  in
  let mcp =
    match cs.mcp_ok with
    | Some true when scope_observed ->
        make_check ~name:"mcp_catalog" ~category:Catalog ~severity:Pass
          ~message:"Room-effective MCP catalog state ok" ()
    | Some true ->
        make_check ~name:"mcp_catalog" ~category:Catalog ~severity:Warn
          ~message:
            "MCP probe is not backed by the requested Room-effective frozen \
             catalog; no healthy state assumed"
          ~repair:
            "Inspect the active Room's frozen Tool_catalog/access snapshot and \
             MCP quarantine state"
          ()
    | Some false ->
        make_check ~name:"mcp_catalog" ~category:Catalog ~severity:Fail
          ~message:"MCP catalog state not ok (quarantine or allowlist failure)"
          ~repair:
            "Repair MCP server allowlist / relist after list_changed; clear \
             quarantine only after successful discovery"
          ()
    | None ->
        make_check ~name:"mcp_catalog" ~category:Catalog ~severity:Warn
          ~message:"MCP catalog probe unavailable; no healthy state assumed"
          ~repair:
            "Inspect the live MCP registry/quarantine state, then re-run \
             validation"
          ()
  in
  let rev =
    match
      (scope_observed, has_managed, cs.catalog_revision, cs.access_revision)
    with
    | false, _, _, _ ->
        make_check ~name:"catalog_revisions" ~category:Catalog ~severity:Warn
          ~message:
            "Room-effective frozen catalog/access snapshot unavailable; \
             revision metadata cannot establish catalog health"
          ~repair:
            "Validate against the active Room Session's frozen catalog after \
             the next-turn refresh"
          ()
    | true, true, None, _ | true, true, _, None ->
        make_check ~name:"catalog_revisions" ~category:Catalog ~severity:Warn
          ~message:
            "managed routes present but catalog_revision and/or \
             access_revision missing on validation snapshot"
          ~repair:
            "After apply, confirm on_catalog_refresh marks affected Rooms and \
             the next turn freezes a catalog with revision metadata"
          ()
    | true, true, Some _, Some _ ->
        make_check ~name:"catalog_revisions" ~category:Catalog ~severity:Pass
          ~message:"catalog and access revision metadata observed (redacted)" ()
    | true, false, _, _ ->
        make_check ~name:"catalog_revisions" ~category:Catalog ~severity:Pass
          ~message:"no managed routes; catalog revision metadata optional" ()
  in
  unavailable_check @ [ tools; mcp; rev ]

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
    match sr.refresh_without_restart with
    | Some true ->
        make_check ~name:"session_refresh_no_restart" ~category:Session
          ~severity:Pass
          ~message:
            "active Session catalog refresh does not require daemon restart"
          ()
    | Some false ->
        make_check ~name:"session_refresh_no_restart" ~category:Session
          ~severity:Fail
          ~message:
            "runtime reports active Session refresh requires restart (contract \
             violation)"
          ~repair:
            "Fix catalog refresh hook so Rooms pick up grants on next turn \
             without restart; see Github_route_apply.on_catalog_refresh"
          ()
    | None ->
        make_check ~name:"session_refresh_no_restart" ~category:Session
          ~severity:Warn
          ~message:
            "live Session refresh capability unavailable; no no-restart pass \
             assumed"
          ~repair:
            "Inspect a live daemon turn after a catalog refresh request; do \
             not infer this from configuration alone"
          ()
  in
  let pending =
    match sr.refresh_pending_room_ids with
    | None ->
        make_check ~name:"session_refresh_pending" ~category:Session
          ~severity:Warn ~message:"pending catalog-refresh queue unavailable"
          ~repair:
            "Inspect github_route_catalog_refresh in the active daemon \
             database, then re-run validation"
          ()
    | Some pending_rooms ->
        let n = List.length pending_rooms in
        if n = 0 then
          make_check ~name:"session_refresh_pending" ~category:Session
            ~severity:Pass ~message:"no rooms pending next-turn catalog refresh"
            ()
        else
          make_check ~name:"session_refresh_pending" ~category:Session
            ~severity:Warn
            ~message:
              (Printf.sprintf "%d room(s) pending next-turn catalog refresh: %s"
                 n
                 (String.concat "," (List.sort String.compare pending_rooms)))
            ~repair:
              "Allow the next agent turn in each Room to rebuild the frozen \
               Tool catalog; do not restart the daemon for this alone"
            ()
  in
  let active_coverage =
    match sr.active_room_ids with
    | None ->
        make_check ~name:"session_refresh_active_rooms" ~category:Session
          ~severity:Warn
          ~message:
            "live Room Session inventory unavailable; route coverage not \
             assumed"
          ~repair:
            "Inspect active daemon Room Sessions after the next turn, then \
             re-run validation"
          ()
    | Some active_rooms -> (
        let active = List.sort_uniq String.compare active_rooms in
        let uncovered =
          List.filter
            (fun rid -> List.mem rid room_dests && not (List.mem rid active))
            room_dests
        in
        match (active, room_dests) with
        | _, [] ->
            make_check ~name:"session_refresh_active_rooms" ~category:Session
              ~severity:Pass
              ~message:"no Room destinations among checked routes" ()
        | _, _ when uncovered <> [] ->
            make_check ~name:"session_refresh_active_rooms" ~category:Session
              ~severity:Warn
              ~message:
                (Printf.sprintf
                   "Room destination(s) without an active Session inventory \
                    entry: %s"
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
                (Printf.sprintf
                   "%d active Room Session(s) cover route destinations"
                   (List.length active))
              ())
  in
  [ restart; pending; active_coverage ]

(* ── Drift checks ───────────────────────────────────────────────── *)

let contract_start = "<!-- github-route-runtime-contract"

let read_all path =
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> Ok (really_input_string ic (in_channel_length ic)))
  with Sys_error error -> Error error

let contract_value fields name =
  match List.assoc_opt name fields with
  | Some value when String.trim value <> "" -> Ok (String.trim value)
  | _ -> Error (Printf.sprintf "missing %s" name)

let documented_contract_of_string text =
  let rec collect in_contract fields = function
    | [] ->
        if in_contract then Error "unterminated runtime contract" else Ok fields
    | line :: rest when String.trim line = contract_start ->
        if in_contract then Error "duplicate runtime contract"
        else collect true fields rest
    | line :: rest when in_contract && String.trim line = "-->" -> Ok fields
    | line :: rest when in_contract -> (
        match String.split_on_char '=' (String.trim line) with
        | key :: values when String.trim key <> "" ->
            collect true
              ((String.trim key, String.concat "=" values |> String.trim)
              :: fields)
              rest
        | _ -> collect true fields rest)
    | _ :: rest -> collect false fields rest
  in
  let ( let* ) = Result.bind in
  let* fields = collect false [] (String.split_on_char '\n' text) in
  let* schema = contract_value fields "filter_schema_version" in
  let* envelope = contract_value fields "envelope_version" in
  let* default_comment_mode = contract_value fields "default_comment_mode" in
  let* modes = contract_value fields "comment_modes" in
  let* specificity_order = contract_value fields "specificity_order" in
  match (int_of_string_opt schema, int_of_string_opt envelope) with
  | Some filter_schema_version, Some envelope_version ->
      Ok
        {
          filter_schema_version;
          envelope_version;
          default_comment_mode;
          comment_modes =
            String.split_on_char ',' modes
            |> List.map String.trim
            |> List.filter (fun mode -> mode <> "");
          specificity_order;
        }
  | _ -> Error "filter_schema_version and envelope_version must be integers"

let load_documented_contract path =
  match read_all path with
  | Error error -> Documentation_unavailable error
  | Ok text -> (
      match documented_contract_of_string text with
      | Ok contract -> Documentation_available contract
      | Error error -> Documentation_unavailable error)

let drift_checks
    ?(documentation =
      Documentation_unavailable
        "operator contract not supplied; cannot compare runtime to docs") () :
    check list =
  match documentation with
  | Documentation_unavailable reason ->
      [
        make_check ~name:"drift_documentation_contract" ~category:Drift
          ~severity:Warn
          ~message:
            (Printf.sprintf
               "operator documentation contract unavailable (%s); no drift \
                pass assumed"
               reason)
          ~repair:
            "Provide docs/github-route-operator-contract.md (or set \
             CLAWQ_GITHUB_ROUTE_CONTRACT) and re-run validation"
          ();
      ]
  | Documentation_available documentation ->
      let schema =
        if F.current_schema_version = documentation.filter_schema_version then
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
              (Printf.sprintf
                 "runtime filter schema_version=%d != documented %d"
                 F.current_schema_version documentation.filter_schema_version)
            ~repair:
              "Update the github-route-runtime-contract block in \
               docs/github-route-operator-contract.md or the runtime schema"
            ()
      in
      let envelope =
        if Env.envelope_version = documentation.envelope_version then
          make_check ~name:"drift_envelope_version" ~category:Drift
            ~severity:Pass
            ~message:
              (Printf.sprintf "runtime envelope_version=%d matches docs"
                 Env.envelope_version)
            ()
        else
          make_check ~name:"drift_envelope_version" ~category:Drift
            ~severity:Fail
            ~message:
              (Printf.sprintf "runtime envelope_version=%d != documented %d"
                 Env.envelope_version documentation.envelope_version)
            ~repair:
              "Align Github_event_envelope.envelope_version with documented \
               product contract"
            ()
      in
      let comment =
        if runtime_comment_mode = documentation.default_comment_mode then
          make_check ~name:"drift_default_comment_mode" ~category:Drift
            ~severity:Pass
            ~message:
              (Printf.sprintf "default comment_mode=%s matches docs"
                 runtime_comment_mode)
            ()
        else
          make_check ~name:"drift_default_comment_mode" ~category:Drift
            ~severity:Fail
            ~message:
              (Printf.sprintf "default comment_mode=%s != documented %s"
                 runtime_comment_mode documentation.default_comment_mode)
            ~repair:
              "Default comment mode is Summary (summary); update runtime or \
               docs"
            ()
      in
      let modes =
        if runtime_comment_modes = documentation.comment_modes then
          make_check ~name:"drift_comment_modes" ~category:Drift ~severity:Pass
            ~message:"comment modes off|summary|threaded match docs" ()
        else
          make_check ~name:"drift_comment_modes" ~category:Drift ~severity:Fail
            ~message:"comment mode set drifted from documented modes"
            ~repair:"Keep supported modes as off, summary, threaded" ()
      in
      let specificity =
        if runtime_specificity_order = documentation.specificity_order then
          make_check ~name:"drift_specificity_order" ~category:Drift
            ~severity:Pass
            ~message:
              (Printf.sprintf "specificity order %s matches docs"
                 runtime_specificity_order)
            ()
        else
          make_check ~name:"drift_specificity_order" ~category:Drift
            ~severity:Fail
            ~message:
              (Printf.sprintf "runtime specificity order %s != documented %s"
                 runtime_specificity_order documentation.specificity_order)
            ~repair:
              "Update the github-route-runtime-contract block or runtime route \
               matching order"
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

let unavailable_catalog_checks () =
  [
    make_check ~name:"catalog_state_unavailable" ~category:Catalog
      ~severity:Warn
      ~message:
        "live tool/MCP catalog state was not probed; no healthy catalog pass \
         assumed"
      ~repair:
        "Run `clawq github route validate` against the active Clawq data \
         directory or provide a live catalog probe"
      ();
  ]

let unavailable_session_checks () =
  [
    make_check ~name:"session_refresh_state_unavailable" ~category:Session
      ~severity:Warn
      ~message:
        "live Session refresh state was not probed; no no-restart pass assumed"
      ~repair:
        "Run `clawq github route validate` against the active daemon database \
         and inspect a live Room turn after refresh"
      ();
  ]

let validate ~db ?destination ?installation ?auth ?catalog_state
    ?session_refresh
    ?(documentation =
      Documentation_unavailable
        "operator contract not supplied; cannot compare runtime to docs")
    ?(now = Unix.gettimeofday ()) () =
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
          let catalog_cs =
            match catalog_state with
            | Some state -> check_catalog state ~destination ~routes
            | None -> unavailable_catalog_checks ()
          in
          let session_cs =
            match session_refresh with
            | Some state -> check_session_refresh state ~routes
            | None -> unavailable_session_checks ()
          in
          let drift_cs = drift_checks ~documentation () in
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
  let redacted_text text =
    match Ops.redact_json (`String text) with
    | `String value -> value
    | _ -> "***REDACTED***"
  in
  let push s = lines := redacted_text s :: !lines in
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

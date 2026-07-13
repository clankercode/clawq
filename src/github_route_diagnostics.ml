(* Route and filter setup diagnostics with redacted export (P20.M2.E2.T001).
   See github_route_diagnostics.mli. *)

module S = Github_route_store
module F = Github_route_filter
module Ops = Github_route_ops
module Inst = Github_app_installation_scope
module Preview = Github_route_filter_preview

type predicate_counts = {
  include_events : int;
  exclude_events : int;
  include_repos : int;
  exclude_repos : int;
  pr_predicates : int;
  issue_predicates : int;
  advanced_total : int;
  baseline_total : int;
}

type route_export = {
  id : string;
  destination_key : string;
  selector_key : string;
  specificity : string;
  enabled : bool;
  revision : string;
  comment_mode : string;
  filter_schema_version : int;
  predicate_counts : predicate_counts;
  has_advanced : bool;
  requires_changed_paths : bool;
  requires_team_membership : bool;
  managed_bundle_id : string option;
  managed_feature_id : string option;
  setup_plan_id : string option;
  created_via : string option;
}

type app_scope_export = {
  installation_id : int option;
  status : string;
  account_login : string option;
  selection : string option;
  scope_revision : string option;
}

type delivery_health = {
  pending : int;
  in_flight : int;
  succeeded : int;
  dead_letter : int;
  superseded : int;
  overall : string;
}

type export = {
  exported_at : string;
  destination : string option;
  current_filter_schema_version : int;
  routes : route_export list;
  route_count : int;
  enabled_count : int;
  plan_id : string option;
  plan_base_revision : string option;
  plan_digest : string option;
  catalog_revision : string option;
  catalog_access_revision : string option;
  app_scope : app_scope_export;
  delivery : delivery_health option;
  readiness_overall : string option;
  repair_hints : string list;
  winning_selector : string option;
  decision : string option;
  final_reason : string option;
  predicate_reasons : string list;
  enrichment_status : string list;
  diagnostics : string list;
}

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let comment_mode_to_string = function
  | S.Off -> "off"
  | S.Summary -> "summary"
  | S.Threaded -> "threaded"

let specificity_of = function
  | S.Item _ -> "item"
  | S.Repo _ -> "repo"
  | S.Org _ -> "org"

let count_set = function None -> 0 | Some _ -> 1
let count_bool_opt = function None -> 0 | Some _ -> 1

let count_pr (p : F.pr_advanced) =
  count_set p.base_branch + count_set p.head_branch + count_set p.changed_path
  + count_set p.labels + count_set p.author + count_set p.team
  + count_bool_opt p.draft

let count_issue (i : F.issue_advanced) =
  count_set i.labels + count_set i.author + count_set i.team
  + count_set i.assignee + count_set i.milestone

let count_predicates (f : F.t) : predicate_counts =
  let include_events = List.length f.include_events in
  let exclude_events = List.length f.exclude_events in
  let include_repos = List.length f.include_repos in
  let exclude_repos = List.length f.exclude_repos in
  let pr_predicates = count_pr f.pr in
  let issue_predicates = count_issue f.issue in
  let advanced_total = pr_predicates + issue_predicates in
  let baseline_total =
    include_events + exclude_events + include_repos + exclude_repos
  in
  {
    include_events;
    exclude_events;
    include_repos;
    exclude_repos;
    pr_predicates;
    issue_predicates;
    advanced_total;
    baseline_total;
  }

let of_route (r : S.t) : route_export =
  let counts = count_predicates r.filter in
  {
    id = r.id;
    destination_key = S.destination_key r.destination;
    selector_key = S.canonical_selector_key r.selector;
    specificity = specificity_of r.selector;
    enabled = r.enabled;
    revision = r.revision;
    comment_mode = comment_mode_to_string r.comment_mode;
    filter_schema_version = r.filter.schema_version;
    predicate_counts = counts;
    has_advanced = F.has_advanced r.filter;
    requires_changed_paths = F.requires_changed_paths r.filter;
    requires_team_membership = F.requires_team_membership r.filter;
    managed_bundle_id = r.managed_bundle_id;
    managed_feature_id = r.managed_feature_id;
    setup_plan_id = r.provenance.setup_plan_id;
    created_via = r.provenance.created_via;
  }

let app_scope_of_installation = function
  | None ->
      {
        installation_id = None;
        status = "missing";
        account_login = None;
        selection = None;
        scope_revision = None;
      }
  | Some (i : Inst.t) ->
      {
        installation_id = Some i.installation_id;
        status = Inst.status_to_string i.status;
        account_login = Some i.account.login;
        selection = Some (Inst.selection_mode_to_string i.selection);
        scope_revision = Some i.revision;
      }

let delivery_overall (m : Github_delivery_ops.metrics) =
  if m.dead_letter > 0 then "unhealthy"
  else if m.pending > 0 || m.in_flight > 0 then "degraded"
  else "healthy"

let delivery_health_of_metrics (m : Github_delivery_ops.metrics) :
    delivery_health =
  {
    pending = m.pending;
    in_flight = m.in_flight;
    succeeded = m.succeeded;
    dead_letter = m.dead_letter;
    superseded = m.superseded;
    overall = delivery_overall m;
  }

let room_id_of_destination = function
  | None -> None
  | Some (S.Room id) -> Some id
  | Some (S.Session _) -> None

let opt_string_field k = function None -> [] | Some v -> [ (k, `String v) ]
let opt_int_field k = function None -> [] | Some v -> [ (k, `Int v) ]

let opaque_reference value =
  let digest = Digestif.SHA256.(digest_string value |> to_hex) in
  "opaque:" ^ String.sub digest 0 16

let predicate_counts_to_json (c : predicate_counts) =
  `Assoc
    (sort_assoc
       [
         ("advanced_total", `Int c.advanced_total);
         ("baseline_total", `Int c.baseline_total);
         ("exclude_events", `Int c.exclude_events);
         ("exclude_repos", `Int c.exclude_repos);
         ("include_events", `Int c.include_events);
         ("include_repos", `Int c.include_repos);
         ("issue_predicates", `Int c.issue_predicates);
         ("pr_predicates", `Int c.pr_predicates);
       ])

let route_export_to_json (r : route_export) =
  `Assoc
    (sort_assoc
       ([
          ("comment_mode", `String r.comment_mode);
          ("destination_key", `String r.destination_key);
          ("enabled", `Bool r.enabled);
          ("filter_schema_version", `Int r.filter_schema_version);
          ("has_advanced", `Bool r.has_advanced);
          ("id", `String r.id);
          ("predicate_counts", predicate_counts_to_json r.predicate_counts);
          ("requires_changed_paths", `Bool r.requires_changed_paths);
          ("requires_team_membership", `Bool r.requires_team_membership);
          ("revision", `String r.revision);
          ("selector_key", `String r.selector_key);
          ("specificity", `String r.specificity);
        ]
       @ opt_string_field "created_via" r.created_via
       @ opt_string_field "managed_bundle_id" r.managed_bundle_id
       @ opt_string_field "managed_feature_id" r.managed_feature_id
       @ opt_string_field "setup_plan_id" r.setup_plan_id))

let app_scope_to_json (a : app_scope_export) =
  `Assoc
    (sort_assoc
       ([ ("status", `String a.status) ]
       @ opt_int_field "installation_id" a.installation_id
       @ opt_string_field "account_login" a.account_login
       @ opt_string_field "selection" a.selection
       @ opt_string_field "scope_revision" a.scope_revision))

let delivery_to_json (d : delivery_health) =
  `Assoc
    (sort_assoc
       [
         ("dead_letter", `Int d.dead_letter);
         ("in_flight", `Int d.in_flight);
         ("overall", `String d.overall);
         ("pending", `Int d.pending);
         ("succeeded", `Int d.succeeded);
         ("superseded", `Int d.superseded);
       ])

let string_list_to_json xs = `List (List.map (fun s -> `String s) xs)

let repair_hints_of_readiness (report : Ops.readiness_report) =
  List.filter_map
    (fun (c : Ops.check) ->
      match (c.status, c.repair) with
      | (Ops.Fail | Ops.Warn), Some r -> Some (Printf.sprintf "%s: %s" c.name r)
      | (Ops.Fail | Ops.Warn), None ->
          Some (Printf.sprintf "%s: %s" c.name c.message)
      | Ops.Pass, _ -> None)
    report.checks

(** Redact a free-form string leaf (PEM/bearer/bound) for diagnostics lines. *)
let safe_str s =
  match Ops.redact_json (`String s) with
  | `String s' -> s'
  | _ -> "***REDACTED***"

let build_diagnostics_lines ~(export_core : export) : string list =
  let lines = ref [] in
  let push s = lines := s :: !lines in
  push
    (Printf.sprintf "filter_schema_current=%d"
       export_core.current_filter_schema_version);
  (match export_core.destination with
  | None -> push "destination=all"
  | Some d -> push (Printf.sprintf "destination=%s" (safe_str d)));
  push
    (Printf.sprintf "routes=%d enabled=%d" export_core.route_count
       export_core.enabled_count);
  (match export_core.plan_id with
  | None -> ()
  | Some id -> push (Printf.sprintf "plan_id=%s" (safe_str id)));
  (match export_core.plan_base_revision with
  | None -> ()
  | Some rev -> push (Printf.sprintf "plan_base_revision=%s" (safe_str rev)));
  (match export_core.plan_digest with
  | None -> ()
  | Some d ->
      let d = safe_str d in
      let d = if String.length d <= 64 then d else String.sub d 0 64 ^ "..." in
      push (Printf.sprintf "plan_digest=%s" d));
  (match export_core.catalog_revision with
  | None -> ()
  | Some rev -> push (Printf.sprintf "catalog_revision=%s" (safe_str rev)));
  (match export_core.catalog_access_revision with
  | None -> ()
  | Some rev ->
      push (Printf.sprintf "catalog_access_revision=%s" (safe_str rev)));
  let app = export_core.app_scope in
  push
    (Printf.sprintf "app_scope status=%s installation=%s account=%s" app.status
       (match app.installation_id with
       | None -> "-"
       | Some i -> string_of_int i)
       (match app.account_login with None -> "-" | Some l -> safe_str l));
  (match export_core.delivery with
  | None -> push "delivery=n/a"
  | Some d ->
      push
        (Printf.sprintf
           "delivery overall=%s pending=%d in_flight=%d dead_letter=%d \
            succeeded=%d"
           d.overall d.pending d.in_flight d.dead_letter d.succeeded));
  (match export_core.readiness_overall with
  | None -> ()
  | Some o -> push (Printf.sprintf "readiness_overall=%s" o));
  List.iter
    (fun (r : route_export) ->
      let managed =
        match (r.managed_bundle_id, r.managed_feature_id) with
        | None, None -> "none"
        | b, f ->
            Printf.sprintf "bundle=%s feature=%s"
              (match b with None -> "-" | Some s -> safe_str s)
              (match f with None -> "-" | Some s -> safe_str s)
      in
      push
        (Printf.sprintf
           "route id=%s enabled=%b rev=%s selector=%s filter_schema=%d \
            advanced=%d baseline=%d managed=%s"
           (safe_str r.id) r.enabled (safe_str r.revision)
           (safe_str r.selector_key) r.filter_schema_version
           r.predicate_counts.advanced_total r.predicate_counts.baseline_total
           managed))
    export_core.routes;
  (match export_core.winning_selector with
  | None -> ()
  | Some s -> push (Printf.sprintf "winning_selector=%s" (safe_str s)));
  (match export_core.decision with
  | None -> ()
  | Some d -> push (Printf.sprintf "decision=%s" (safe_str d)));
  (match export_core.final_reason with
  | None -> ()
  | Some r -> push (Printf.sprintf "final_reason=%s" (safe_str r)));
  List.iter
    (fun s -> push (Printf.sprintf "enrichment:%s" (safe_str s)))
    export_core.enrichment_status;
  List.iter
    (fun s -> push (Printf.sprintf "predicate:%s" (safe_str s)))
    export_core.predicate_reasons;
  List.iter
    (fun s -> push (Printf.sprintf "repair:%s" (safe_str s)))
    export_core.repair_hints;
  List.rev !lines

let collect ~db ?destination ?installation ?auth ?plan ?catalog_revision
    ?catalog_access_revision ?envelope ?enrichment ?(tools_granted = true)
    ?(mcp_ok = true) ?(credentials_ok = true) ?(egress_ok = true)
    ?(connector_ok = true) ?delivery_ok ?(now = Unix.gettimeofday ()) () =
  S.ensure_schema db;
  match
    match destination with
    | Some d -> S.list_for_destination ~db ~destination:d
    | None -> S.list_all ~db
  with
  | Error e -> Error e
  | Ok routes ->
      let routes =
        List.sort
          (fun (a : S.t) (b : S.t) ->
            let c =
              String.compare
                (S.destination_key a.destination)
                (S.destination_key b.destination)
            in
            if c <> 0 then c else String.compare a.id b.id)
          routes
      in
      let route_exports = List.map of_route routes in
      let enabled_count =
        List.fold_left
          (fun n (r : route_export) -> if r.enabled then n + 1 else n)
          0 route_exports
      in
      let plan_id = Option.map (fun (p : Setup_plan.t) -> p.id) plan in
      let plan_base_revision =
        Option.map (fun (p : Setup_plan.t) -> p.base_revision) plan
      in
      let plan_digest = Option.map (fun (p : Setup_plan.t) -> p.digest) plan in
      let app_scope = app_scope_of_installation installation in
      (* Delivery health for Room destinations (or all-rooms when unscoped). *)
      let room_id = room_id_of_destination destination in
      let delivery =
        match Github_delivery_ops.metrics ~db ?room_id () with
        | Ok m -> Some (delivery_health_of_metrics m)
        | Error _ ->
            Some
              {
                pending = 0;
                in_flight = 0;
                succeeded = 0;
                dead_letter = 0;
                superseded = 0;
                overall = "error";
              }
      in
      (* Derive delivery_ok from metrics when caller did not override. *)
      let delivery_ok =
        match delivery_ok with
        | Some b -> b
        | None -> (
            match delivery with Some d -> d.dead_letter = 0 | None -> true)
      in
      let sample_route = match routes with r :: _ -> Some r | [] -> None in
      let readiness =
        Ops.assess_readiness ?route:sample_route ?installation ?auth
          ~tools_granted ~mcp_ok ~credentials_ok ~egress_ok ~connector_ok
          ~delivery_ok ?base_revision:plan_base_revision
          ?current_revision:
            (match sample_route with
            | Some r -> Some r.revision
            | None -> plan_base_revision)
          ()
      in
      let repair_hints = repair_hints_of_readiness readiness in
      let readiness_overall =
        Some (Ops.check_status_to_string readiness.overall)
      in
      (* Optional filter-preview explain (no raw webhook / comments). *)
      let ( winning_selector,
            decision,
            final_reason,
            predicate_reasons,
            enrichment_status ) =
        match (destination, envelope) with
        | Some dest, Some env ->
            let p =
              Preview.preview ~db ~destination:dest ~envelope:env ?enrichment ()
            in
            let reasons =
              List.map
                (fun (pr : Preview.predicate_result) ->
                  Printf.sprintf "%s=%s (%s)" pr.name
                    (if pr.passed then "pass" else "fail")
                    pr.detail)
                p.predicates
            in
            ( p.winning_selector,
              Some p.decision,
              Some p.final_reason,
              reasons,
              p.enrichment_status )
        | _ -> (None, None, None, [], [])
      in
      let dest_key = Option.map S.destination_key destination in
      let catalog_revision = Option.map opaque_reference catalog_revision in
      let catalog_access_revision =
        Option.map opaque_reference catalog_access_revision
      in
      let base : export =
        {
          exported_at = Time_util.iso8601_utc ~t:now ();
          destination = dest_key;
          current_filter_schema_version = F.current_schema_version;
          routes = route_exports;
          route_count = List.length route_exports;
          enabled_count;
          plan_id;
          plan_base_revision;
          plan_digest;
          catalog_revision;
          catalog_access_revision;
          app_scope;
          delivery;
          readiness_overall;
          repair_hints;
          winning_selector;
          decision;
          final_reason;
          predicate_reasons;
          enrichment_status;
          diagnostics = [];
        }
      in
      let diagnostics = build_diagnostics_lines ~export_core:base in
      Ok { base with diagnostics }

let to_json (e : export) : Yojson.Safe.t =
  let raw =
    `Assoc
      (sort_assoc
         ([
            ("app_scope", app_scope_to_json e.app_scope);
            ( "current_filter_schema_version",
              `Int e.current_filter_schema_version );
            ("diagnostics", string_list_to_json e.diagnostics);
            ("enabled_count", `Int e.enabled_count);
            ("enrichment_status", string_list_to_json e.enrichment_status);
            ("exported_at", `String e.exported_at);
            ("predicate_reasons", string_list_to_json e.predicate_reasons);
            ("repair_hints", string_list_to_json e.repair_hints);
            ("route_count", `Int e.route_count);
            ("routes", `List (List.map route_export_to_json e.routes));
          ]
         @ opt_string_field "catalog_access_revision" e.catalog_access_revision
         @ opt_string_field "catalog_revision" e.catalog_revision
         @ (match e.delivery with
           | None -> []
           | Some d -> [ ("delivery", delivery_to_json d) ])
         @ opt_string_field "decision" e.decision
         @ opt_string_field "destination" e.destination
         @ opt_string_field "final_reason" e.final_reason
         @ opt_string_field "plan_base_revision" e.plan_base_revision
         @ opt_string_field "plan_digest" e.plan_digest
         @ opt_string_field "plan_id" e.plan_id
         @ opt_string_field "readiness_overall" e.readiness_overall
         @ opt_string_field "winning_selector" e.winning_selector))
  in
  Ops.redact_json raw

let format_diagnostics (e : export) = e.diagnostics

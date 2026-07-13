(* Org-scale matching budgets and pure measurement helpers (P20.M1.E2.T003).
   See github_route_match_bench.mli. *)

module S = Github_route_store
module E = Github_event_envelope
module F = Github_route_filter
module En = Github_filter_enrichment
module A = Github_route_match_advanced
module M = Github_route_match

(* ---- Documented budgets ---- *)

let org_scale_sibling_repo_routes = 200
let org_scale_max_candidates = 3
let max_enrichment_fetches_per_cold_resolve = 2
let max_enrichment_fetches_warm_cache = 0

(* candidates (≤3) + baseline step (1) + all advanced PR+Issue fields (12) +
   small headroom for future typed fields without loosening index guarantees. *)
let max_match_eval_cost_units = 32

type costs = {
  candidates : int;
  path_fetches : int;
  team_fetches : int;
  match_cost_units : int;
  decision : M.decision;
}

let count_advanced_field_steps ~(filter : F.t) : int =
  let pr = filter.pr in
  let issue = filter.issue in
  let step o = match o with None -> 0 | Some _ -> 1 in
  step pr.base_branch + step pr.head_branch + step pr.changed_path
  + step pr.labels + step pr.author + step pr.team
  + (match pr.draft with None -> 0 | Some _ -> 1)
  + step issue.labels + step issue.author + step issue.team
  + step issue.assignee + step issue.milestone

let estimate_match_cost_units ~candidates ~(filter : F.t) =
  (* Always charge one baseline evaluation step when any candidate exists. *)
  let baseline = if candidates > 0 then 1 else 0 in
  candidates + baseline + count_advanced_field_steps ~filter

let measure_indexed_resolve ~db ~destination ~envelope ?index ?fetch_paths
    ?fetch_teams ?cache ?rate_limited ?access_allowed ?now () : costs =
  let idx =
    match index with
    | Some i -> i
    | None -> (
        match A.build_index_from_db ~db ~destination with
        | Ok i -> i
        | Error _ -> A.build_index ~destination ~routes:[])
  in
  let candidates = List.length (A.index_candidates idx ~envelope) in
  let path_fetches = ref 0 in
  let team_fetches = ref 0 in
  let wrap_paths = function
    | None -> None
    | Some fetch ->
        Some
          (fun ~envelope ->
            incr path_fetches;
            fetch ~envelope)
  in
  let wrap_teams = function
    | None -> None
    | Some fetch ->
        Some
          (fun ~envelope ~team_slugs ->
            incr team_fetches;
            fetch ~envelope ~team_slugs)
  in
  let decision =
    A.resolve ~db ~destination ~envelope ~index:idx
      ?fetch_paths:(wrap_paths fetch_paths)
      ?fetch_teams:(wrap_teams fetch_teams) ?cache ?rate_limited ?access_allowed
      ?now ()
  in
  (* Cost uses the winning route filter when known; else empty advanced. *)
  let filter =
    match decision with
    | M.Matched { route; _ } | M.Muted { route; _ } -> route.filter
    | M.No_route -> F.default
  in
  {
    candidates;
    path_fetches = !path_fetches;
    team_fetches = !team_fetches;
    match_cost_units = estimate_match_cost_units ~candidates ~filter;
    decision;
  }

type org_scale_setup = {
  destination : S.destination;
  index : A.route_index;
  envelope : E.t;
  target_repo_route_id : string;
  org_route_id : string;
  item_route_id : string option;
  sibling_repo_count : int;
}

let make_pr_envelope ~repo ~org ~number : E.t =
  {
    version = E.envelope_version;
    delivery_id = Some "bench-deliv";
    installation_id = Some 1;
    event = "pull_request";
    action = Some "opened";
    repo_full_name = repo;
    org = Some org;
    item_kind = Some E.Pull_request;
    item_number = Some number;
    item_node_id = None;
    item_url = None;
    html_url = None;
    family = E.Lifecycle;
    actor = { E.empty_actor with login = Some "alice" };
    item_author = Some "alice";
    before = None;
    after =
      Some
        {
          E.empty_safe_state with
          labels = [ "bug" ];
          draft = Some false;
          base_ref = Some "main";
          state = Some "open";
        };
    transfer = None;
    received_at = Some "2024-01-01T00:00:00Z";
    event_at = None;
    head_sha = Some "deadbeef";
    unsupported = false;
    skip_reason = None;
  }

let install_org_scale_routes ~db ~destination ?(org = "acme")
    ?(target_repo = "acme/widget")
    ?(sibling_repos = org_scale_sibling_repo_routes) ?(include_item = true)
    ?(target_filter = F.default) ?(org_filter = F.default)
    ?(now = 1_700_000_000.0) () : (org_scale_setup, string) result =
  let create ~id ~selector ~filter =
    S.create ~db ~id ~destination ~selector ~filter ~enabled:true ~now ()
  in
  let org_route_id = "bench_org" in
  let target_repo_route_id = "bench_repo" in
  match create ~id:org_route_id ~selector:(S.Org org) ~filter:org_filter with
  | Error e -> Error e
  | Ok _ -> (
      match
        create ~id:target_repo_route_id ~selector:(S.Repo target_repo)
          ~filter:target_filter
      with
      | Error e -> Error e
      | Ok _ -> (
          let rec add_siblings i =
            if i >= sibling_repos then Ok ()
            else
              let id = Printf.sprintf "bench_sib_%04d" i in
              let repo = Printf.sprintf "%s/other-%04d" org i in
              match create ~id ~selector:(S.Repo repo) ~filter:F.default with
              | Error e -> Error e
              | Ok _ -> add_siblings (i + 1)
          in
          match add_siblings 0 with
          | Error e -> Error e
          | Ok () -> (
              let item_route_id =
                if not include_item then None
                else
                  let id = "bench_item" in
                  match
                    create ~id
                      ~selector:
                        (S.Item
                           {
                             repo_full_name = target_repo;
                             kind = `Pull_request;
                             number = 42;
                           })
                      ~filter:F.default
                  with
                  | Error _ -> None
                  | Ok _ -> Some id
              in
              match A.build_index_from_db ~db ~destination with
              | Error e -> Error e
              | Ok index ->
                  Ok
                    {
                      destination;
                      index;
                      envelope =
                        make_pr_envelope ~repo:target_repo ~org ~number:42;
                      target_repo_route_id;
                      org_route_id;
                      item_route_id;
                      sibling_repo_count = sibling_repos;
                    })))

let assert_costs_within_budget (c : costs) : (unit, string) result =
  if c.candidates > org_scale_max_candidates then
    Error
      (Printf.sprintf "candidates %d exceeds budget %d" c.candidates
         org_scale_max_candidates)
  else if
    c.path_fetches + c.team_fetches > max_enrichment_fetches_per_cold_resolve
  then
    Error
      (Printf.sprintf "enrichment fetches %d+%d exceed cold budget %d"
         c.path_fetches c.team_fetches max_enrichment_fetches_per_cold_resolve)
  else if c.match_cost_units > max_match_eval_cost_units then
    Error
      (Printf.sprintf "match_cost_units %d exceeds budget %d" c.match_cost_units
         max_match_eval_cost_units)
  else Ok ()

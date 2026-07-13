(* Indexed and cached advanced route matching (P20.M1.E2.T002).
   See github_route_match_advanced.mli. *)

module S = Github_route_store
module M = Github_route_match
module F = Github_route_filter
module Ev = Github_route_filter_eval
module En = Github_filter_enrichment
module E = Github_event_envelope

type decision = M.decision
type accept_result = M.accept_result
type specificity = M.specificity

let normalize s = String.lowercase_ascii (String.trim s)

(* ---- Destination-local route index ---- *)

type route_index = {
  destination : S.destination;
  destination_key : string;
  routes : S.t list;
  by_item : (string, S.t list) Hashtbl.t;
  by_repo : (string, S.t list) Hashtbl.t;
  by_org : (string, S.t list) Hashtbl.t;
}

let index_destination (idx : route_index) = idx.destination
let index_size (idx : route_index) = List.length idx.routes

let item_key_of_ref (r : S.item_ref) =
  let kind = match r.kind with `Pull_request -> "pr" | `Issue -> "issue" in
  Printf.sprintf "%s:%s:%d" kind (normalize r.repo_full_name) r.number

let item_key_of_envelope (env : E.t) =
  match (env.item_kind, env.item_number) with
  | Some E.Pull_request, Some n when n > 0 ->
      Some (Printf.sprintf "pr:%s:%d" (normalize env.repo_full_name) n)
  | Some E.Issue, Some n when n > 0 ->
      Some (Printf.sprintf "issue:%s:%d" (normalize env.repo_full_name) n)
  | _ -> None

let envelope_org (env : E.t) =
  match env.org with
  | Some o when String.trim o <> "" -> Some (normalize o)
  | _ -> (
      match String.split_on_char '/' env.repo_full_name with
      | owner :: _ :: _ when String.trim owner <> "" -> Some (normalize owner)
      | _ -> None)

let push_tbl (tbl : (string, S.t list) Hashtbl.t) key (route : S.t) =
  let key = normalize key in
  if key = "" then ()
  else
    let prev =
      match Hashtbl.find_opt tbl key with Some xs -> xs | None -> []
    in
    Hashtbl.replace tbl key (route :: prev)

let build_index ~destination ~(routes : S.t list) : route_index =
  let destination_key = S.destination_key destination in
  let dest_routes =
    List.filter
      (fun (r : S.t) -> S.destination_key r.destination = destination_key)
      routes
  in
  let by_item = Hashtbl.create 32 in
  let by_repo = Hashtbl.create 16 in
  let by_org = Hashtbl.create 8 in
  List.iter
    (fun (r : S.t) ->
      match r.selector with
      | S.Item ref_ -> push_tbl by_item (item_key_of_ref ref_) r
      | S.Repo repo -> push_tbl by_repo repo r
      | S.Org org -> push_tbl by_org org r)
    dest_routes;
  {
    destination;
    destination_key;
    routes = dest_routes;
    by_item;
    by_repo;
    by_org;
  }

let build_index_from_db ~db ~destination =
  match S.list_for_destination ~db ~destination with
  | Error e -> Error e
  | Ok routes -> Ok (build_index ~destination ~routes)

let tbl_get tbl key =
  match Hashtbl.find_opt tbl (normalize key) with None -> [] | Some xs -> xs

let index_candidates (idx : route_index) ~(envelope : E.t) : S.t list =
  let repo = normalize envelope.repo_full_name in
  let item_hits =
    match item_key_of_envelope envelope with
    | None -> []
    | Some k -> tbl_get idx.by_item k
  in
  let repo_hits = if repo = "" then [] else tbl_get idx.by_repo repo in
  let org_hits =
    match envelope_org envelope with
    | None -> []
    | Some o -> tbl_get idx.by_org o
  in
  (* Dedup by route id while preserving first-seen order (item, repo, org). *)
  let seen = Hashtbl.create 16 in
  let acc = ref [] in
  List.iter
    (fun (r : S.t) ->
      if Hashtbl.mem seen r.id then ()
      else if not (M.selector_applies r.selector envelope) then ()
      else begin
        Hashtbl.add seen r.id true;
        acc := r :: !acc
      end)
    (item_hits @ repo_hits @ org_hits);
  List.rev !acc

(* ---- Index cache ---- *)

type index_cache = {
  ttl_s : float;
  entries : (string, route_index * float) Hashtbl.t;
}

let default_index_ttl_s = 30.0

let create_index_cache ?(ttl_s = default_index_ttl_s) () =
  { ttl_s = max 0.0 ttl_s; entries = Hashtbl.create 16 }

let invalidate_index ~(cache : index_cache) ~destination =
  Hashtbl.remove cache.entries (S.destination_key destination)

let get_or_build_index ~(cache : index_cache) ~db ~destination ?now () =
  let now = match now with Some t -> t | None -> Unix.gettimeofday () in
  let key = S.destination_key destination in
  match Hashtbl.find_opt cache.entries key with
  | Some (idx, expires_at) when now < expires_at -> Ok idx
  | Some _ | None -> (
      match build_index_from_db ~db ~destination with
      | Error e -> Error e
      | Ok idx ->
          Hashtbl.replace cache.entries key (idx, now +. cache.ttl_s);
          Ok idx)

(* ---- Advanced evaluation ---- *)

let obtain_enrichment ~(filter : F.t) ~(envelope : E.t) ?enrichment ?fetch_paths
    ?fetch_teams ?cache ?now () : En.enrichment =
  match enrichment with
  | Some e -> e
  | None ->
      let demand = En.demand_of_filter filter in
      if (not demand.need_paths) && not demand.need_teams then
        En.empty_enrichment
      else En.enrich ~filter ~envelope ?fetch_paths ?fetch_teams ?cache ?now ()

let enrichment_fail_reason (e : En.enrichment) =
  match e.reasons with
  | [] -> "filter rejected: enrichment incomplete"
  | r :: _ ->
      Printf.sprintf "filter rejected: enrichment incomplete (%s)"
        (String.trim r)

let advanced_reject_detail ~(filter : F.t) ~(envelope : E.t)
    ~(enrichment : En.enrichment) =
  (* Prefer a stable field-level reason when a configured predicate fails. *)
  let pr_ctx = Ev.pr_context_of_envelope ~envelope ~enrichment () in
  let issue_ctx = Ev.issue_context_of_envelope ~envelope ~enrichment () in
  let check name ok = if ok then None else Some name in
  let pr = filter.pr in
  let issue = filter.issue in
  let first =
    List.find_map
      (fun x -> x)
      [
        (match pr.base_branch with
        | None -> None
        | Some m ->
            check "pr.base_branch"
              (Ev.eval_glob_match ~subject:pr_ctx.base_branch m));
        (match pr.head_branch with
        | None -> None
        | Some m ->
            check "pr.head_branch"
              (Ev.eval_glob_match ~subject:pr_ctx.head_branch m));
        (match pr.changed_path with
        | None -> None
        | Some m ->
            check "pr.changed_path"
              (Ev.eval_paths_match ~paths:pr_ctx.changed_paths m));
        (match pr.labels with
        | None -> None
        | Some m ->
            check "pr.labels"
              (Ev.eval_set_match ~subject:pr_ctx.labels ~case_sensitive:false m));
        (match pr.author with
        | None -> None
        | Some m ->
            check "pr.author"
              (Ev.eval_scalar_set_match ~subject:pr_ctx.author
                 ~case_sensitive:false m));
        (match pr.team with
        | None -> None
        | Some m ->
            let ok =
              match pr_ctx.teams with
              | None -> false
              | Some membership ->
                  Ev.eval_set_match ~subject:membership ~case_sensitive:false m
            in
            check "pr.team" ok);
        (match pr.draft with
        | None -> None
        | Some want ->
            let ok =
              match pr_ctx.draft with Some got -> got = want | None -> false
            in
            check "pr.draft" ok);
        (match issue.labels with
        | None -> None
        | Some m ->
            check "issue.labels"
              (Ev.eval_set_match ~subject:issue_ctx.labels ~case_sensitive:false
                 m));
        (match issue.author with
        | None -> None
        | Some m ->
            check "issue.author"
              (Ev.eval_scalar_set_match ~subject:issue_ctx.author
                 ~case_sensitive:false m));
        (match issue.team with
        | None -> None
        | Some m ->
            let ok =
              match issue_ctx.teams with
              | None -> false
              | Some membership ->
                  Ev.eval_set_match ~subject:membership ~case_sensitive:false m
            in
            check "issue.team" ok);
        (match issue.assignee with
        | None -> None
        | Some m ->
            check "issue.assignee"
              (Ev.eval_set_match ~subject:issue_ctx.assignees
                 ~case_sensitive:false m));
        (match issue.milestone with
        | None -> None
        | Some m ->
            check "issue.milestone"
              (Ev.eval_milestone_match ~subject:issue_ctx.milestone
                 ~case_sensitive:false m));
      ]
  in
  match first with
  | Some name -> Printf.sprintf "filter rejected: %s" name
  | None -> "filter rejected: advanced predicate"

let advanced_allows ~(filter : F.t) ~(envelope : E.t)
    ~(enrichment : En.enrichment) () : (unit, string) result =
  if not (F.has_advanced filter) then Ok ()
  else
    let demand = En.demand_of_filter filter in
    if
      (demand.need_paths || demand.need_teams)
      && not (En.demanded_ok enrichment)
    then Error (enrichment_fail_reason enrichment)
    else
      let pr_ctx = Ev.pr_context_of_envelope ~envelope ~enrichment () in
      let issue_ctx = Ev.issue_context_of_envelope ~envelope ~enrichment () in
      let ok =
        Ev.eval_pr ~filter ~ctx:pr_ctx ()
        && Ev.eval_issue ~filter ~ctx:issue_ctx ()
      in
      if ok then Ok ()
      else Error (advanced_reject_detail ~filter ~envelope ~enrichment)

let apply_advanced_to_matched ~(route : S.t) ~specificity ~(envelope : E.t)
    ?enrichment ?fetch_paths ?fetch_teams ?cache ?now () : decision =
  let filter = route.filter in
  if not (F.has_advanced filter) then M.Matched { route; specificity }
  else
    let enrichment =
      obtain_enrichment ~filter ~envelope ?enrichment ?fetch_paths ?fetch_teams
        ?cache ?now ()
    in
    match advanced_allows ~filter ~envelope ~enrichment () with
    | Ok () -> M.Matched { route; specificity }
    | Error reason -> M.Muted { route; specificity; reason }

(* ---- Indexed resolve (full baseline + advanced, no fallthrough) ---- *)

let specificity_rank = function `Item -> 3 | `Repo -> 2 | `Org -> 1

let revision_int (r : S.t) =
  match int_of_string_opt r.revision with Some n -> n | None -> 0

let prefer_route (a : S.t) (b : S.t) =
  match (a.enabled, b.enabled) with
  | true, false -> a
  | false, true -> b
  | _ ->
      let ra = revision_int a and rb = revision_int b in
      if ra <> rb then if ra > rb then a else b
      else if String.compare a.id b.id >= 0 then a
      else b

let pick_preferred routes =
  match routes with
  | [] -> None
  | hd :: tl -> Some (List.fold_left prefer_route hd tl)

let baseline_reject_reason ~(filter : F.t) ~(envelope : E.t) =
  (* Mirror Github_route_match.filter_reject_reason without relying on private
     helpers; baseline semantics are shared via filter_allows. *)
  let event = envelope.event in
  let family = E.string_of_family envelope.family in
  let repo = envelope.repo_full_name in
  if not (Ev.eval_baseline ~filter ~event ~family ~repo ()) then
    if
      filter.exclude_events <> []
      && (List.exists
            (fun t -> normalize t = normalize event)
            filter.exclude_events
         || List.exists
              (fun t -> normalize t = normalize family)
              filter.exclude_events)
    then "filter rejected: event excluded"
    else if
      filter.include_events <> []
      && not
           (List.exists
              (fun t -> normalize t = normalize event)
              filter.include_events
           || List.exists
                (fun t -> normalize t = normalize family)
                filter.include_events)
    then "filter rejected: event not in include_events"
    else if
      filter.exclude_repos <> []
      && List.exists
           (fun t -> normalize t = normalize repo)
           filter.exclude_repos
    then "filter rejected: repo excluded"
    else if
      filter.include_repos <> []
      && not
           (List.exists
              (fun t -> normalize t = normalize repo)
              filter.include_repos)
    then "filter rejected: repo not in include_repos"
    else "filter rejected"
  else "filter rejected"

let resolve_from_candidates ~candidates ~(envelope : E.t) ?enrichment
    ?fetch_paths ?fetch_teams ?cache ?now () : decision =
  match candidates with
  | [] -> M.No_route
  | _ -> (
      let best_rank =
        List.fold_left
          (fun acc (r : S.t) ->
            max acc (specificity_rank (M.specificity_of_selector r.selector)))
          0 candidates
      in
      let tier =
        List.filter
          (fun (r : S.t) ->
            specificity_rank (M.specificity_of_selector r.selector) = best_rank)
          candidates
      in
      match pick_preferred tier with
      | None -> M.No_route
      | Some route ->
          let specificity = M.specificity_of_selector route.selector in
          if not route.enabled then
            M.Muted { route; specificity; reason = "disabled" }
          else if not (M.filter_allows route.filter envelope) then
            M.Muted
              {
                route;
                specificity;
                reason = baseline_reject_reason ~filter:route.filter ~envelope;
              }
          else
            apply_advanced_to_matched ~route ~specificity ~envelope ?enrichment
              ?fetch_paths ?fetch_teams ?cache ?now ())

let resolve_with_index ~(index : route_index) ~(envelope : E.t) ?enrichment
    ?fetch_paths ?fetch_teams ?cache ?now () =
  let candidates = index_candidates index ~envelope in
  resolve_from_candidates ~candidates ~envelope ?enrichment ?fetch_paths
    ?fetch_teams ?cache ?now ()

(* ---- Public resolve / try_accept ---- *)

let resolve ~db ~destination ~envelope ?enrichment ?fetch_paths ?fetch_teams
    ?cache ?index ?index_cache ?now () : decision =
  match (index, index_cache) with
  | Some idx, _ ->
      resolve_with_index ~index:idx ~envelope ?enrichment ?fetch_paths
        ?fetch_teams ?cache ?now ()
  | None, Some icache -> (
      match get_or_build_index ~cache:icache ~db ~destination ?now () with
      | Error _ ->
          (* Fail soft to non-indexed wrap path rather than No_route on list
             errors only when cache rebuild fails; list failure → No_route. *)
          M.No_route
      | Ok idx ->
          resolve_with_index ~index:idx ~envelope ?enrichment ?fetch_paths
            ?fetch_teams ?cache ?now ())
  | None, None -> (
      (* Thin wrap of Github_route_match: advanced layer only on Matched. *)
      match M.resolve ~db ~destination ~envelope () with
      | (M.No_route | M.Muted _) as d -> d
      | M.Matched { route; specificity } ->
          apply_advanced_to_matched ~route ~specificity ~envelope ?enrichment
            ?fetch_paths ?fetch_teams ?cache ?now ())

let try_accept ~db ~destination ~envelope ?enrichment ?fetch_paths ?fetch_teams
    ?cache ?index ?index_cache ?now ?item_key () : accept_result =
  let decision =
    resolve ~db ~destination ~envelope ?enrichment ?fetch_paths ?fetch_teams
      ?cache ?index ?index_cache ?now ()
  in
  match decision with
  | M.Muted _ | M.No_route -> M.Not_accepted decision
  | M.Matched _ as matched -> (
      (* Advanced is a strict subset of baseline match, so the base ledger path
         will also see Matched for the same destination/envelope. *)
      match M.try_accept ~db ~destination ~envelope ?now ?item_key () with
      | M.Accepted _ -> M.Accepted matched
      | M.Duplicate d -> M.Duplicate d
      | M.Not_accepted d -> M.Not_accepted d)

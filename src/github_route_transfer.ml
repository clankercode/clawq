(* Issue transfer dual-scope matching + per-Room accept dedupe.
   See github_route_transfer.mli and docs/plans/2026-07-12-github-item-room-routing.md. *)

type room_id = string

type transfer_plan = {
  destinations : Github_route_store.destination list;
  per_destination :
    (Github_route_store.destination * Github_route_match.decision) list;
}

module A = Github_route_match_advanced

let normalize s = String.lowercase_ascii (String.trim s)

let org_of_repo full_name =
  match String.split_on_char '/' (String.trim full_name) with
  | owner :: _ :: _ when String.trim owner <> "" -> Some (String.trim owner)
  | _ -> None

let with_repo (env : Github_event_envelope.t) repo_full_name =
  let repo_full_name = String.trim repo_full_name in
  if repo_full_name = "" then env
  else
    let org =
      match org_of_repo repo_full_name with Some o -> Some o | None -> env.org
    in
    { env with repo_full_name; org }

let source_repo (env : Github_event_envelope.t) =
  match env.transfer with
  | Some { from_repo = Some r; _ } when String.trim r <> "" -> String.trim r
  | _ -> env.repo_full_name

let dest_repo (env : Github_event_envelope.t) =
  match env.transfer with
  | Some { to_repo = Some r; _ } when String.trim r <> "" -> String.trim r
  | _ -> env.repo_full_name

let source_view env = with_repo env (source_repo env)
let dest_view env = with_repo env (dest_repo env)

let transfer_stable_item_key (env : Github_event_envelope.t) =
  (* Prefer destination (to) repo so source/dest dual evaluation shares one key.
     Fall back to from_repo, then envelope repo / node id. *)
  let repo = normalize (dest_repo env) in
  match (env.item_kind, env.item_number, env.item_node_id) with
  | Some Github_event_envelope.Issue, Some n, _ ->
      Printf.sprintf "issue:%s:%d" repo n
  | Some Github_event_envelope.Issue, None, Some node
    when String.trim node <> "" ->
      Printf.sprintf "issue:node:%s" (normalize node)
  | Some Github_event_envelope.Pull_request, Some n, _ ->
      Printf.sprintf "pr:%s:%d" repo n
  | _ -> Github_route_match.canonical_item_key (dest_view env)

let dedupe_destinations (dests : Github_route_store.destination list) =
  let seen = Hashtbl.create (List.length dests) in
  List.filter
    (fun d ->
      let k = Github_route_store.destination_key d in
      if Hashtbl.mem seen k then false
      else (
        Hashtbl.add seen k true;
        true))
    dests

let specificity_rank = function `Item -> 3 | `Repo -> 2 | `Org -> 1 | _ -> 0

let decision_specificity = function
  | Github_route_match.Matched { specificity; _ }
  | Github_route_match.Muted { specificity; _ } ->
      specificity_rank specificity
  | Github_route_match.No_route -> 0

(** Prefer Matched over Muted over No_route; among equals prefer higher
    specificity. When both Matched/Muted at same rank, prefer [prefer] (dest).
*)
let merge_decisions ~prefer_second first second =
  match (first, second) with
  | Github_route_match.Matched _, Github_route_match.Matched _ ->
      if decision_specificity second > decision_specificity first then second
      else if decision_specificity first > decision_specificity second then
        first
      else if prefer_second then second
      else first
  | (Github_route_match.Matched _ as m), _ -> m
  | _, (Github_route_match.Matched _ as m) -> m
  | Github_route_match.Muted _, Github_route_match.Muted _ ->
      if decision_specificity second > decision_specificity first then second
      else if decision_specificity first > decision_specificity second then
        first
      else if prefer_second then second
      else first
  | (Github_route_match.Muted _ as m), Github_route_match.No_route -> m
  | Github_route_match.No_route, (Github_route_match.Muted _ as m) -> m
  | Github_route_match.No_route, Github_route_match.No_route ->
      Github_route_match.No_route

let plan_transfer ~db ~destinations ~envelope ?enrichment ?fetch_paths
    ?fetch_teams ?cache ?rate_limited ?access_allowed ?index ?index_cache ?now
    () =
  let source_env = source_view envelope in
  let dest_env = dest_view envelope in
  let candidates = dedupe_destinations destinations in
  let per_destination =
    List.filter_map
      (fun dest ->
        let d_src =
          A.resolve ~db ~destination:dest ~envelope:source_env ?enrichment
            ?fetch_paths ?fetch_teams ?cache ?rate_limited ?access_allowed
            ?index ?index_cache ?now ()
        in
        let d_dst =
          A.resolve ~db ~destination:dest ~envelope:dest_env ?enrichment
            ?fetch_paths ?fetch_teams ?cache ?rate_limited ?access_allowed
            ?index ?index_cache ?now ()
        in
        let merged = merge_decisions ~prefer_second:true d_src d_dst in
        match merged with
        | Github_route_match.No_route -> None
        | decision -> Some (dest, decision))
      candidates
  in
  let destinations =
    List.filter_map
      (fun (dest, decision) ->
        match decision with
        | Github_route_match.Matched _ -> Some dest
        | Github_route_match.Muted _ | Github_route_match.No_route -> None)
      per_destination
  in
  { destinations; per_destination }

let envelope_for_accept ~db ~destination ~source_env ~dest_env ?enrichment
    ?fetch_paths ?fetch_teams ?cache ?rate_limited ?access_allowed ?index
    ?index_cache ?now () =
  (* Resolve with the view that produces Matched so try_accept re-resolve works.
     Prefer dest view when both match. *)
  match
    A.resolve ~db ~destination ~envelope:dest_env ?enrichment ?fetch_paths
      ?fetch_teams ?cache ?rate_limited ?access_allowed ?index ?index_cache ?now
      ()
  with
  | Github_route_match.Matched _ -> dest_env
  | _ -> (
      match
        A.resolve ~db ~destination ~envelope:source_env ?enrichment ?fetch_paths
          ?fetch_teams ?cache ?rate_limited ?access_allowed ?index ?index_cache
          ?now ()
      with
      | Github_route_match.Matched _ -> source_env
      | _ -> dest_env)

let accept_transfer ~db ~destinations ~envelope ?enrichment ?fetch_paths
    ?fetch_teams ?cache ?rate_limited ?access_allowed ?index ?index_cache
    ?(now = Unix.gettimeofday ()) () =
  let plan =
    plan_transfer ~db ~destinations ~envelope ?enrichment ?fetch_paths
      ?fetch_teams ?cache ?rate_limited ?access_allowed ?index ?index_cache ~now
      ()
  in
  let source_env = source_view envelope in
  let dest_env = dest_view envelope in
  let item_key = transfer_stable_item_key envelope in
  List.map
    (fun dest ->
      let env_for =
        envelope_for_accept ~db ~destination:dest ~source_env ~dest_env
          ?enrichment ?fetch_paths ?fetch_teams ?cache ?rate_limited
          ?access_allowed ?index ?index_cache ~now ()
      in
      let result =
        A.try_accept ~db ~destination:dest ~envelope:env_for ?enrichment
          ?fetch_paths ?fetch_teams ?cache ?rate_limited ?access_allowed ?index
          ?index_cache ~now ~item_key ()
      in
      (dest, result))
    plan.destinations

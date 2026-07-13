(** Demand-driven path and team enrichment for advanced route filters
    (P20.M1.E1.T002).

    Pure demand detection + injectable fetchers. Failure never becomes allow. *)

module F = Github_route_filter
module E = Github_event_envelope

type demand = { need_paths : bool; need_teams : bool }

let demand_of_filter (filter : F.t) : demand =
  {
    need_paths = F.requires_changed_paths filter;
    need_teams = F.requires_team_membership filter;
  }

let team_slugs_of_filter (filter : F.t) : string list =
  let from_match = function None -> [] | Some (m : F.set_match) -> m.values in
  let raw = from_match filter.pr.team @ from_match filter.issue.team in
  (* Dedup preserving first-seen order; trim empties. *)
  let seen = Hashtbl.create 8 in
  List.fold_left
    (fun acc s ->
      let t = String.trim s in
      if t = "" then acc
      else
        let k = String.lowercase_ascii t in
        if Hashtbl.mem seen k then acc
        else begin
          Hashtbl.add seen k true;
          acc @ [ t ]
        end)
    [] raw

type paths_fetch = envelope:E.t -> (string list, string) result

type teams_fetch =
  envelope:E.t -> team_slugs:string list -> (string list, string) result

type enrichment = {
  paths : (string list, string) result option;
  teams : (string list, string) result option;
  reasons : string list;
  complete : bool;
}

let empty_enrichment : enrichment =
  { paths = None; teams = None; reasons = []; complete = true }

let demanded_ok (e : enrichment) = e.complete

let item_revision (env : E.t) =
  match env.head_sha with
  | Some s when String.trim s <> "" -> String.trim s
  | _ -> (
      match env.item_number with Some n -> string_of_int n | None -> "unknown")

let install_part (env : E.t) =
  match env.installation_id with Some i -> string_of_int i | None -> "-"

let number_part (env : E.t) =
  match env.item_number with Some n -> string_of_int n | None -> "-"

let item_author_part (env : E.t) =
  match env.item_author with
  | Some l when String.trim l <> "" -> String.trim l
  | _ -> "-"

let cache_key_paths (env : E.t) =
  Printf.sprintf "paths:%s:%s:%s:%s" (install_part env)
    (String.lowercase_ascii (String.trim env.repo_full_name))
    (number_part env) (item_revision env)

let cache_key_teams (env : E.t) ~team_slugs =
  let slugs =
    team_slugs
    |> List.map (fun s -> String.lowercase_ascii (String.trim s))
    |> List.filter (fun s -> s <> "")
    |> List.sort String.compare |> String.concat ","
  in
  Printf.sprintf "teams:%s:%s:%s:%s:%s" (install_part env)
    (String.lowercase_ascii (String.trim env.repo_full_name))
    (String.lowercase_ascii (item_author_part env))
    slugs (item_revision env)

(* ---- Cache ---- *)

type cache_entry =
  | Paths of (string list, string) result
  | Teams of (string list, string) result

type cache_slot = { value : cache_entry; expires_at : float }

type cache = {
  ttl_s : float;
  max_entries : int;
  entries : (string, cache_slot) Hashtbl.t;
  (* Insertion order for simple eviction (oldest first). *)
  mutable order : string list;
}

let default_ttl_s = 60.0
let default_max_entries = 256

let create_cache ?(ttl_s = default_ttl_s) ?(max_entries = default_max_entries)
    () =
  {
    ttl_s = max 0.0 ttl_s;
    max_entries = max 1 max_entries;
    entries = Hashtbl.create 32;
    order = [];
  }

let cache_get (c : cache) ~key ~now =
  match Hashtbl.find_opt c.entries key with
  | None -> None
  | Some slot ->
      if now >= slot.expires_at then begin
        Hashtbl.remove c.entries key;
        c.order <- List.filter (( <> ) key) c.order;
        None
      end
      else Some slot.value

let cache_put (c : cache) ~key ~value ~now =
  let expires_at = now +. c.ttl_s in
  let already = Hashtbl.mem c.entries key in
  Hashtbl.replace c.entries key { value; expires_at };
  if not already then c.order <- c.order @ [ key ];
  (* Evict oldest while over capacity. *)
  let rec evict () =
    if Hashtbl.length c.entries <= c.max_entries then ()
    else
      match c.order with
      | [] -> ()
      | oldest :: rest ->
          c.order <- rest;
          Hashtbl.remove c.entries oldest;
          evict ()
  in
  evict ()

(* ---- Enrich ---- *)

let reasons_of_result = function
  | Ok _ -> []
  | Error r ->
      let t = String.trim r in
      if t = "" then [ "unavailable" ] else [ t ]

let is_pr_with_number (env : E.t) =
  match (env.item_kind, env.item_number) with
  | Some E.Pull_request, Some n when n > 0 -> true
  | _ -> false

let has_item_author (env : E.t) =
  match env.item_author with
  | Some l when String.trim l <> "" -> true
  | _ -> false

let resolve_paths ~envelope ~fetch_paths ~cache ~now ~rate_limited
    ~access_allowed : (string list, string) result =
  if rate_limited () then Error "rate_limited"
  else if not (access_allowed ()) then Error "access_denied"
  else if not (is_pr_with_number envelope) then Error "not_a_pr"
  else
    let key = cache_key_paths envelope in
    let from_cache =
      match cache with
      | None -> None
      | Some c -> (
          match cache_get c ~key ~now with
          | Some (Paths r) -> Some r
          | Some (Teams _) | None -> None)
    in
    match from_cache with
    | Some r -> r
    | None ->
        let result =
          match fetch_paths with
          | None -> Error "fetcher_unavailable"
          | Some fetch -> (
              try fetch ~envelope
              with exn -> Error ("unavailable:" ^ Printexc.to_string exn))
        in
        (match cache with
        | None -> ()
        | Some c -> cache_put c ~key ~value:(Paths result) ~now);
        result

let resolve_teams ~envelope ~team_slugs ~fetch_teams ~cache ~now ~rate_limited
    ~access_allowed : (string list, string) result =
  if rate_limited () then Error "rate_limited"
  else if not (access_allowed ()) then Error "access_denied"
  else if not (has_item_author envelope) then Error "missing_item_author"
  else if team_slugs = [] then Error "no_team_slugs"
  else
    let key = cache_key_teams envelope ~team_slugs in
    let from_cache =
      match cache with
      | None -> None
      | Some c -> (
          match cache_get c ~key ~now with
          | Some (Teams r) -> Some r
          | Some (Paths _) | None -> None)
    in
    match from_cache with
    | Some r -> r
    | None ->
        let result =
          match fetch_teams with
          | None -> Error "fetcher_unavailable"
          | Some fetch -> (
              try fetch ~envelope ~team_slugs
              with exn -> Error ("unavailable:" ^ Printexc.to_string exn))
        in
        (match cache with
        | None -> ()
        | Some c -> cache_put c ~key ~value:(Teams result) ~now);
        result

let enrich ~(filter : F.t) ~(envelope : E.t) ?fetch_paths ?fetch_teams ?cache
    ?now ?(rate_limited = fun () -> false) ?(access_allowed = fun () -> true) ()
    : enrichment =
  let now = match now with Some t -> t | None -> Unix.gettimeofday () in
  let demand = demand_of_filter filter in
  if (not demand.need_paths) && not demand.need_teams then empty_enrichment
  else
    let paths =
      if not demand.need_paths then None
      else
        Some
          (resolve_paths ~envelope ~fetch_paths ~cache ~now ~rate_limited
             ~access_allowed)
    in
    let teams =
      if not demand.need_teams then None
      else
        let team_slugs = team_slugs_of_filter filter in
        Some
          (resolve_teams ~envelope ~team_slugs ~fetch_teams ~cache ~now
             ~rate_limited ~access_allowed)
    in
    let reasons =
      (match paths with None -> [] | Some r -> reasons_of_result r)
      @ match teams with None -> [] | Some r -> reasons_of_result r
    in
    let complete =
      (match paths with
        | None -> true
        | Some (Ok _) -> true
        | Some (Error _) -> false)
      &&
      match teams with
      | None -> true
      | Some (Ok _) -> true
      | Some (Error _) -> false
    in
    { paths; teams; reasons; complete }

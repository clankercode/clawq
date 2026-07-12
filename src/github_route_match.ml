(* Destination-local Item > Repo > Org no-fallthrough matching.
   See github_route_match.mli and docs/plans/2026-07-12-github-item-room-routing.md. *)

type match_input = {
  destination : Github_route_store.destination;
  envelope : Github_event_envelope.t;
}

type specificity = [ `Item | `Repo | `Org ]

type decision =
  | Matched of { route : Github_route_store.t; specificity : specificity }
  | Muted of {
      route : Github_route_store.t;
      specificity : specificity;
      reason : string;
    }
  | No_route

let normalize s = String.lowercase_ascii (String.trim s)

let specificity_of_selector = function
  | Github_route_store.Item _ -> `Item
  | Repo _ -> `Repo
  | Org _ -> `Org

let specificity_rank = function `Item -> 3 | `Repo -> 2 | `Org -> 1

let envelope_org (env : Github_event_envelope.t) =
  match env.org with
  | Some o when String.trim o <> "" -> Some (normalize o)
  | _ -> (
      match String.split_on_char '/' env.repo_full_name with
      | owner :: _ :: _ when String.trim owner <> "" -> Some (normalize owner)
      | _ -> None)

let item_kind_matches env_kind sel_kind =
  match (env_kind, sel_kind) with
  | Some Github_event_envelope.Pull_request, `Pull_request -> true
  | Some Github_event_envelope.Issue, `Issue -> true
  | _ -> false

let selector_applies (sel : Github_route_store.selector)
    (env : Github_event_envelope.t) =
  match sel with
  | Item { repo_full_name; kind; number } -> (
      normalize repo_full_name = normalize env.repo_full_name
      && item_kind_matches env.item_kind kind
      && match env.item_number with Some n -> n = number | None -> false)
  | Repo repo -> normalize repo = normalize env.repo_full_name
  | Org org -> (
      match envelope_org env with
      | Some env_org -> normalize org = env_org
      | None -> false)

let list_mem_ci list token =
  let t = normalize token in
  List.exists (fun s -> normalize s = t) list

let event_tokens (env : Github_event_envelope.t) =
  let family = Github_event_envelope.string_of_family env.family in
  (* Match against X-GitHub-Event name and the envelope family string. *)
  [ env.event; family ]

let events_match list (env : Github_event_envelope.t) =
  List.exists (fun tok -> list_mem_ci list tok) (event_tokens env)

let filter_allows (filter : Github_route_store.event_filter)
    (env : Github_event_envelope.t) =
  (* exclude_events always wins *)
  if filter.exclude_events <> [] && events_match filter.exclude_events env then
    false
  else if
    filter.include_events <> [] && not (events_match filter.include_events env)
  then false
  else
    let repo = normalize env.repo_full_name in
    if repo = "" then true
    else if filter.exclude_repos <> [] && list_mem_ci filter.exclude_repos repo
    then false
    else if
      filter.include_repos <> [] && not (list_mem_ci filter.include_repos repo)
    then false
    else true

let filter_reject_reason (filter : Github_route_store.event_filter)
    (env : Github_event_envelope.t) =
  if filter.exclude_events <> [] && events_match filter.exclude_events env then
    "filter rejected: event excluded"
  else if
    filter.include_events <> [] && not (events_match filter.include_events env)
  then "filter rejected: event not in include_events"
  else
    let repo = normalize env.repo_full_name in
    if filter.exclude_repos <> [] && list_mem_ci filter.exclude_repos repo then
      "filter rejected: repo excluded"
    else if
      filter.include_repos <> [] && not (list_mem_ci filter.include_repos repo)
    then "filter rejected: repo not in include_repos"
    else "filter rejected"

let revision_int (r : Github_route_store.t) =
  match int_of_string_opt r.revision with Some n -> n | None -> 0

(** Prefer enabled; then higher revision; then stable id order. *)
let prefer_route (a : Github_route_store.t) (b : Github_route_store.t) =
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

let resolve ~db ~destination ~envelope () =
  match Github_route_store.list_for_destination ~db ~destination with
  | Error _ -> No_route
  | Ok all -> (
      let candidates =
        List.filter
          (fun (r : Github_route_store.t) ->
            selector_applies r.selector envelope)
          all
      in
      match candidates with
      | [] -> No_route
      | _ -> (
          (* Most-specific selector class among candidates (enabled or not). *)
          let best_rank =
            List.fold_left
              (fun acc (r : Github_route_store.t) ->
                max acc (specificity_rank (specificity_of_selector r.selector)))
              0 candidates
          in
          let tier =
            List.filter
              (fun (r : Github_route_store.t) ->
                specificity_rank (specificity_of_selector r.selector)
                = best_rank)
              candidates
          in
          match pick_preferred tier with
          | None -> No_route
          | Some route ->
              let specificity = specificity_of_selector route.selector in
              if not route.enabled then
                Muted { route; specificity; reason = "disabled" }
              else if not (filter_allows route.filter envelope) then
                Muted
                  {
                    route;
                    specificity;
                    reason = filter_reject_reason route.filter envelope;
                  }
              else Matched { route; specificity }))

let ensure_schema db =
  Sqlite3.busy_timeout db 5_000;
  let sqls =
    [
      {|CREATE TABLE IF NOT EXISTS github_route_accepts (
          destination_key TEXT NOT NULL,
          delivery_id TEXT NOT NULL,
          item_key TEXT NOT NULL,
          route_id TEXT,
          revision TEXT,
          accepted_at TEXT NOT NULL,
          PRIMARY KEY (destination_key, delivery_id, item_key)
        )|};
      {|CREATE INDEX IF NOT EXISTS idx_github_route_accepts_delivery
          ON github_route_accepts(delivery_id)|};
    ]
  in
  List.iter
    (fun sql ->
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | rc ->
          failwith
            (Printf.sprintf "github_route_match schema: %s"
               (Sqlite3.Rc.to_string rc)))
    sqls

let canonical_item_key (env : Github_event_envelope.t) =
  let repo = normalize env.repo_full_name in
  match (env.item_kind, env.item_number) with
  | Some Github_event_envelope.Pull_request, Some n ->
      Printf.sprintf "pr:%s:%d" repo n
  | Some Github_event_envelope.Issue, Some n ->
      Printf.sprintf "issue:%s:%d" repo n
  | _ ->
      let action = match env.action with Some a -> a | None -> "" in
      Printf.sprintf "event:%s:%s:%s" repo env.event action

let delivery_key (env : Github_event_envelope.t) =
  match env.delivery_id with
  | Some d when String.trim d <> "" -> String.trim d
  | _ ->
      (* Synthetic but stable for a given envelope shape when delivery missing. *)
      Digest.to_hex
        (Digest.string
           (String.concat "|"
              [
                env.event;
                Option.value env.action ~default:"";
                env.repo_full_name;
                canonical_item_key env;
                Option.value env.event_at ~default:"";
              ]))

type accept_result =
  | Accepted of decision
  | Duplicate of {
      delivery_id : string;
      item_key : string;
      route_id : string option;
    }
  | Not_accepted of decision

let try_accept ~db ~destination ~envelope ?(now = Unix.gettimeofday ())
    ?item_key () =
  ensure_schema db;
  let decision = resolve ~db ~destination ~envelope () in
  match decision with
  | Muted _ | No_route -> Not_accepted decision
  | Matched { route; _ } as matched -> (
      let dest_key = Github_route_store.destination_key destination in
      let delivery_id = delivery_key envelope in
      let item_key =
        match item_key with
        | Some k when String.trim k <> "" -> String.trim k
        | _ -> canonical_item_key envelope
      in
      let accepted_at = Time_util.iso8601_utc ~t:now () in
      let sql =
        {|INSERT INTO github_route_accepts
            (destination_key, delivery_id, item_key, route_id, revision, accepted_at)
          VALUES (?, ?, ?, ?, ?, ?)|}
      in
      let stmt = Sqlite3.prepare db sql in
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT dest_key));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT delivery_id));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT item_key));
      ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT route.id));
      ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT route.revision));
      ignore (Sqlite3.bind stmt 6 (Sqlite3.Data.TEXT accepted_at));
      let rc = Sqlite3.step stmt in
      ignore (Sqlite3.finalize stmt);
      match rc with
      | Sqlite3.Rc.DONE -> Accepted matched
      | Sqlite3.Rc.CONSTRAINT ->
          Duplicate { delivery_id; item_key; route_id = Some route.id }
      | rc ->
          (* Treat unexpected errors as non-accept to fail closed. *)
          Not_accepted
            (Muted
               {
                 route;
                 specificity = specificity_of_selector route.selector;
                 reason =
                   Printf.sprintf "accept ledger error: %s"
                     (Sqlite3.Rc.to_string rc);
               }))

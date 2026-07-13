(* Migrate legacy per-PR subscriptions into Item routes (P19.M2.E2.T005).
   See github_route_migrate.mli and docs/plans/2026-07-12-github-item-room-routing.md.

   Compatibility commands after cutover should call Github_route_store APIs only
   (no dual-write to github_pr_subscriptions). See [compatibility_cli_aliases]. *)

type legacy_subscription = {
  id : string;
  room_id : string;
  repo_full_name : string;
  pr_number : int;
  enabled : bool;
  events : string list;
  profile_id : string option;
  backlink_ref : string option;
  audit_ref : string option;
  created_at : string option;
}

type collision_policy = Prefer_existing_route | Prefer_legacy | Prefer_newest

type resolution =
  | Created of Github_route_store.t
  | Updated of Github_route_store.t
  | Skipped of { reason : string; winner_route_id : string option }
  | Collided of { winner : Github_route_store.t; losers : string list }

type migrate_report = {
  resolutions : (legacy_subscription * resolution) list;
  active_routes : int;
}

let destination_of_legacy (leg : legacy_subscription) =
  Github_route_store.Room leg.room_id

let selector_of_legacy (leg : legacy_subscription) =
  Github_route_store.Item
    {
      repo_full_name = leg.repo_full_name;
      kind = `Pull_request;
      number = leg.pr_number;
    }

let group_key (leg : legacy_subscription) =
  let dest = destination_of_legacy leg in
  let sel = selector_of_legacy leg in
  Github_route_store.destination_key dest
  ^ "|"
  ^ Github_route_store.canonical_selector_key sel

(** Sanitize legacy id for use inside a route id. *)
let sanitize_id_fragment s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> Buffer.add_char b c
      | _ -> Buffer.add_char b '_')
    s;
  let out = Buffer.contents b in
  if out = "" then "unknown" else out

let route_id_for_legacy (leg : legacy_subscription) =
  "ghroute_migrate_" ^ sanitize_id_fragment leg.id

let events_of_notification_preferences
    (p : Github_pr_subscriptions.notification_preferences) =
  let d = Github_pr_subscriptions.default_notification_preferences in
  (* All-default → empty include list (baseline allow-all). *)
  if
    p.on_open = d.on_open && p.on_close = d.on_close
    && p.on_comment = d.on_comment
    && p.on_review = d.on_review && p.on_status = d.on_status
    && p.on_merge = d.on_merge
  then []
  else
    let add flag names acc = if flag then names @ acc else acc in
    []
    (* Prefer GitHub X-GitHub-Event names so route filters match envelopes. *)
    |> add p.on_status [ "status"; "check_run"; "check_suite" ]
    |> add p.on_review [ "pull_request_review" ]
    |> add p.on_comment [ "issue_comment"; "pull_request_review_comment" ]
    |> add (p.on_open || p.on_close || p.on_merge) [ "pull_request" ]
    |> List.rev

let legacy_of_subscription (sub : Github_pr_subscriptions.subscription) :
    legacy_subscription =
  {
    id = string_of_int sub.id;
    room_id = sub.room_id;
    repo_full_name = sub.repo;
    pr_number = sub.pr_number;
    enabled = sub.enabled;
    events = events_of_notification_preferences sub.notification_preferences;
    profile_id = Some (string_of_int sub.profile_id);
    backlink_ref = None;
    audit_ref = None;
    created_at = Some sub.created_at;
  }

let table_exists db name =
  let sql = "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
      match Sqlite3.step stmt with Sqlite3.Rc.ROW -> true | _ -> false)

let load_legacy_from_db ~db =
  try
    if not (table_exists db "github_pr_subscriptions") then Ok []
    else
      (* High limit: one-shot migration of the whole table. *)
      let subs = Github_pr_subscriptions.find_all ~db ~limit:1_000_000 () in
      Ok (List.map legacy_of_subscription subs)
  with exn ->
    Error
      (Printf.sprintf "load_legacy_from_db failed: %s" (Printexc.to_string exn))

let compatibility_cli_aliases () =
  (* After cutover these names mean "delegate to route store APIs" — no dual-write. *)
  [
    ("subscriptions add", "github route item add");
    ("subscriptions list", "github route item list");
    ("subscriptions show", "github route item show");
    ("subscriptions remove", "github route item remove");
    ("subscriptions enable", "github route item enable");
    ("subscriptions disable", "github route item disable");
    ("pr-subscribe", "github route item add");
    ("pr-unsubscribe", "github route item remove");
  ]

let created_at_key (leg : legacy_subscription) =
  match leg.created_at with Some s -> s | None -> ""

(** Prefer larger created_at (ISO/SQLite datetime sorts lexicographically), then
    larger id for stability. *)
let newer_legacy (a : legacy_subscription) (b : legacy_subscription) =
  let ca = created_at_key a and cb = created_at_key b in
  if ca > cb then a else if cb > ca then b else if a.id >= b.id then a else b

let pick_newest_legacy legs =
  match legs with
  | [] -> None
  | hd :: tl -> Some (List.fold_left newer_legacy hd tl)

let provenance_of_legacy (leg : legacy_subscription) :
    Github_route_store.provenance =
  let notes_parts =
    [ "legacy_sub_id=" ^ leg.id ]
    @ (match leg.profile_id with
      | Some p when String.trim p <> "" -> [ "profile_id=" ^ p ]
      | _ -> [])
    @ (match leg.backlink_ref with
      | Some b when String.trim b <> "" -> [ "backlink_ref=" ^ b ]
      | _ -> [])
    @
    match leg.audit_ref with
    | Some a when String.trim a <> "" -> [ "audit_ref=" ^ a ]
    | _ -> []
  in
  {
    Github_route_store.created_by = leg.profile_id;
    created_via = Some "migrate";
    setup_plan_id = leg.audit_ref;
    notes = Some (String.concat ";" notes_parts);
  }

let filter_of_legacy (leg : legacy_subscription) :
    Github_route_store.event_filter =
  { Github_route_store.default_filter with include_events = leg.events }

let count_active_routes ~db =
  let sql = "SELECT COUNT(*) FROM github_routes WHERE enabled = 1" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Ok (Int64.to_int n)
          | _ -> Ok 0)
      | rc ->
          Error
            (Printf.sprintf "count active routes failed: %s"
               (Sqlite3.Rc.to_string rc)))

let group_legacy legs =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun leg ->
      let k = group_key leg in
      let prev =
        match Hashtbl.find_opt tbl k with Some xs -> xs | None -> []
      in
      Hashtbl.replace tbl k (leg :: prev))
    legs;
  Hashtbl.fold (fun _k xs acc -> List.rev xs :: acc) tbl []

let create_from_legacy ~db ~leg ~now ~on_collision =
  let destination = destination_of_legacy leg in
  let selector = selector_of_legacy leg in
  let filter = filter_of_legacy leg in
  let provenance = provenance_of_legacy leg in
  let id = route_id_for_legacy leg in
  (* If a previous migrate already inserted this deterministic id, update in place. *)
  match Github_route_store.get ~db ~id with
  | Error e -> Error e
  | Ok (Some existing) ->
      Github_route_store.update ~db ~id:existing.id
        ~expected_revision:existing.revision ~filter ~enabled:leg.enabled ~now
        ()
      |> Result.map (fun r -> (`Updated r : [ `Created of _ | `Updated of _ ]))
  | Ok None -> (
      match
        Github_route_store.create ~db ~id ~destination ~selector ~filter
          ~enabled:leg.enabled ~provenance ~now ~on_collision ()
      with
      | Error e -> Error e
      | Ok r -> Ok (`Created r))

let resolve_group ~db ~policy ~now (legs : legacy_subscription list) :
    ((legacy_subscription * resolution) list, string) result =
  match legs with
  | [] -> Ok []
  | _ -> (
      let destination = destination_of_legacy (List.hd legs) in
      let selector = selector_of_legacy (List.hd legs) in
      match pick_newest_legacy legs with
      | None -> Ok []
      | Some newest -> (
          let losers =
            List.filter
              (fun (l : legacy_subscription) -> l.id <> newest.id)
              legs
          in
          let loser_ids = List.map (fun l -> l.id) losers in
          match Github_route_store.find_active ~db ~destination ~selector with
          | Error e -> Error e
          | Ok existing_opt -> (
              let apply_create ~on_collision winner_leg =
                match
                  create_from_legacy ~db ~leg:winner_leg ~now ~on_collision
                with
                | Error e -> Error e
                | Ok (`Created r) ->
                    let winner_res =
                      if loser_ids = [] then Created r
                      else Collided { winner = r; losers = loser_ids }
                    in
                    let loser_res =
                      List.map
                        (fun l ->
                          (l, Collided { winner = r; losers = loser_ids }))
                        losers
                    in
                    Ok ((winner_leg, winner_res) :: loser_res)
                | Ok (`Updated r) ->
                    let winner_res =
                      if loser_ids = [] then Updated r
                      else Collided { winner = r; losers = loser_ids }
                    in
                    let loser_res =
                      List.map
                        (fun l ->
                          (l, Collided { winner = r; losers = loser_ids }))
                        losers
                    in
                    Ok ((winner_leg, winner_res) :: loser_res)
              in
              let skip_all ~reason ~winner_route_id =
                Ok
                  (List.map
                     (fun l -> (l, Skipped { reason; winner_route_id }))
                     legs)
              in
              match (policy, existing_opt) with
              | Prefer_existing_route, Some existing ->
                  (* Documented default: keep existing active route. *)
                  skip_all
                    ~reason:
                      (Printf.sprintf
                         "prefer_existing_route: active route %s holds \
                          destination+selector"
                         existing.id)
                    ~winner_route_id:(Some existing.id)
              | Prefer_existing_route, None ->
                  (* Among legacy only: Prefer_newest. *)
                  apply_create ~on_collision:`Reject newest
              | Prefer_legacy, _ -> apply_create ~on_collision:`Replace newest
              | Prefer_newest, None -> apply_create ~on_collision:`Reject newest
              | Prefer_newest, Some existing ->
                  let existing_at = existing.created_at in
                  let legacy_at = created_at_key newest in
                  if legacy_at > existing_at then
                    apply_create ~on_collision:`Replace newest
                  else
                    skip_all
                      ~reason:
                        (Printf.sprintf
                           "prefer_newest: existing route %s is newer or equal"
                           existing.id)
                      ~winner_route_id:(Some existing.id))))

let migrate_subscriptions ~db ~legacy ?(policy = Prefer_existing_route)
    ?(now = Unix.gettimeofday ()) () =
  Github_route_store.ensure_schema db;
  (* Process groups sequentially. Each create/update is transactional via the
     route store (BEGIN IMMEDIATE). Re-running is idempotent: Prefer_existing
     skips active slots; deterministic ids update in place. A mid-run failure
     leaves partial progress that retry safely converges. *)
  let groups = group_legacy legacy in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | g :: rest -> (
        match resolve_group ~db ~policy ~now g with
        | Error e -> Error e
        | Ok res -> loop (List.rev_append res acc) rest)
  in
  match loop [] groups with
  | Error e -> Error e
  | Ok resolutions -> (
      match count_active_routes ~db with
      | Error e -> Error e
      | Ok active_routes -> Ok { resolutions; active_routes })

let migrate_database ~db ?policy ?now () =
  match load_legacy_from_db ~db with
  | Error _ as error -> error
  | Ok legacy -> migrate_subscriptions ~db ~legacy ?policy ?now ()

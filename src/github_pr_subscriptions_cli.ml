(** Admin CLI for GitHub PR subscription lifecycle. Requires CLAWQ_ADMIN=1
    environment variable. *)

open Command_bridge_helpers
open Command_bridge_session

let admin_env_var = "CLAWQ_ADMIN"

let is_admin () =
  match Sys.getenv_opt admin_env_var with
  | Some v -> v = "1" || v = "true"
  | None -> false

let require_admin () =
  if is_admin () then None
  else
    Some
      "Error: this command requires admin privileges. Set CLAWQ_ADMIN=1 in \
       your environment."

module Route = Github_route_store
module Migrate = Github_route_migrate

let pr_item_of_route (route : Route.t) =
  match route.selector with
  | Route.Item { repo_full_name; kind = `Pull_request; number } ->
      Some (repo_full_name, number)
  | Route.Item { kind = `Issue; _ } | Route.Repo _ | Route.Org _ -> None

let route_room_id (route : Route.t) =
  match route.destination with
  | Route.Room room_id -> Some room_id
  | Route.Session _ -> None

let subscription_routes routes =
  List.filter_map
    (fun route ->
      match (route_room_id route, pr_item_of_route route) with
      | Some room_id, Some (repo, pr_number) ->
          Some (route, room_id, repo, pr_number)
      | None, _ | _, None -> None)
    routes

let format_subscription_detail (route : Route.t) ~room_id ~repo ~pr_number =
  Printf.sprintf
    "Subscription route %s\n\
     Room:       %s\n\
     Repository: %s\n\
     PR:         #%d\n\
     Profile ID: %s\n\
     Enabled:    %s\n\
     Created:    %s\n\
     Updated:    %s\n\n\
     Forwarded event families: %s"
    route.id room_id repo pr_number
    (Option.value route.provenance.created_by ~default:"none")
    (if route.enabled then "yes" else "no")
    route.created_at route.updated_at
    (match route.filter.include_events with
    | [] -> "default baseline"
    | events -> String.concat ", " events)

let format_subscription_row ((route : Route.t), room_id, repo, (pr_number : int))
    =
  [
    route.id;
    room_id;
    repo;
    Printf.sprintf "#%d" pr_number;
    (if route.enabled then "yes" else "no");
    route.created_at;
  ]

let subscription_columns =
  Table_format.
    [
      { header = "ID"; align = Right; min_width = 2; flex = false };
      { header = "ROOM"; align = Left; min_width = 8; flex = false };
      { header = "REPO"; align = Left; min_width = 12; flex = false };
      { header = "PR"; align = Left; min_width = 4; flex = false };
      { header = "ENABLED"; align = Left; min_width = 3; flex = false };
      { header = "CREATED"; align = Left; min_width = 10; flex = true };
    ]

let parse_notification_prefs args =
  let prefs = Github_pr_subscriptions.default_notification_preferences in
  let rec loop prefs = function
    | "--on-open" :: v :: rest ->
        loop { prefs with Github_pr_subscriptions.on_open = v = "true" } rest
    | "--on-close" :: v :: rest ->
        loop { prefs with on_close = v = "true" } rest
    | "--on-comment" :: v :: rest ->
        loop { prefs with on_comment = v = "true" } rest
    | "--on-review" :: v :: rest ->
        loop { prefs with on_review = v = "true" } rest
    | "--on-status" :: v :: rest ->
        loop { prefs with on_status = v = "true" } rest
    | "--on-merge" :: v :: rest ->
        loop { prefs with on_merge = v = "true" } rest
    | _ :: rest -> loop prefs rest
    | [] -> prefs
  in
  loop prefs args

let migration_error = function
  | Ok _ -> None
  | Error error ->
      Some
        (Printf.sprintf
           "Error: could not migrate legacy PR subscriptions into GitHub \
            routes: %s"
           error)

let route_id_of_legacy_id id = "ghroute_migrate_" ^ id

let find_route_by_command_id ~db id =
  match Route.get ~db ~id with
  | Error _ as error -> error
  | Ok (Some _ as route) -> Ok route
  | Ok None -> (
      match int_of_string_opt id with
      | None -> Ok None
      | Some _ -> Route.get ~db ~id:(route_id_of_legacy_id id))

let list_subscription_routes ~db =
  Route.list_all ~db |> Result.map subscription_routes

let filter_routes ?room_id ?repo routes =
  List.filter
    (fun (_route, route_room_id, route_repo, _pr_number) ->
      Option.fold ~none:true ~some:(String.equal route_room_id) room_id
      && Option.fold ~none:true ~some:(String.equal route_repo) repo)
    routes

let route_filter_of_prefs prefs =
  {
    Route.default_filter with
    include_events = Migrate.events_of_notification_preferences prefs;
  }

let profile_id_for_name ~db profile_name =
  match Memory_core.get_room_profile_by_name ~db ~name:profile_name with
  | Some profile -> profile.id
  | None -> Memory_core.insert_room_profile ~db ~name:profile_name

let cli_provenance profile_id =
  {
    Route.created_by = Some (string_of_int profile_id);
    created_via = Some "cli";
    setup_plan_id = None;
    notes = Some "legacy-subscriptions-cli-alias";
  }

let create_compat_route ~db ~room_id ~repo ~pr_number ~profile_id ~prefs =
  let destination = Route.Room room_id in
  let selector =
    Route.Item
      { repo_full_name = repo; kind = `Pull_request; number = pr_number }
  in
  Route.create ~db ~destination ~selector
    ~filter:(route_filter_of_prefs prefs)
    ~enabled:true
    ~provenance:(cli_provenance profile_id)
    ~on_collision:`Replace ()

let disable_route ~db (route : Route.t) =
  Route.update ~db ~id:route.id ~expected_revision:route.revision ~enabled:false
    ()

let find_active_item_route ~db ~room_id ~repo ~pr_number =
  let destination = Route.Room room_id in
  let selector =
    Route.Item
      { repo_full_name = repo; kind = `Pull_request; number = pr_number }
  in
  Route.find_active ~db ~destination ~selector

let cmd_subscriptions_with_db ~db args =
  match migration_error (Migrate.migrate_database ~db ()) with
  | Some error -> error
  | None -> (
      match args with
      | [ "list"; "--room"; room_id ] -> (
          match list_subscription_routes ~db with
          | Error error -> "Error: " ^ error
          | Ok routes ->
              let routes = filter_routes ~room_id routes in
              if routes = [] then
                Printf.sprintf "No PR subscriptions found for room '%s'."
                  room_id
              else
                Printf.sprintf "PR Subscriptions for room '%s':\n" room_id
                ^ Table_format.render subscription_columns
                    (List.map format_subscription_row routes))
      | [ "list"; "--repo"; repo ] -> (
          match list_subscription_routes ~db with
          | Error error -> "Error: " ^ error
          | Ok routes ->
              let routes = filter_routes ~repo routes in
              if routes = [] then
                Printf.sprintf "No PR subscriptions found for repo '%s'." repo
              else
                Printf.sprintf "PR Subscriptions for repo '%s':\n" repo
                ^ Table_format.render subscription_columns
                    (List.map format_subscription_row routes))
      | [ "list" ] | "list" :: _ -> (
          match list_subscription_routes ~db with
          | Error error -> "Error: " ^ error
          | Ok [] ->
              "No PR subscriptions configured. Use 'clawq subscriptions add' \
               to create one."
          | Ok routes ->
              "PR Subscriptions:\n"
              ^ Table_format.render subscription_columns
                  (List.map format_subscription_row routes))
      | [ "show"; id ] -> (
          match find_route_by_command_id ~db id with
          | Error error -> "Error: " ^ error
          | Ok None ->
              Printf.sprintf "No subscription route found with ID %s." id
          | Ok (Some route) -> (
              match (route_room_id route, pr_item_of_route route) with
              | Some room_id, Some (repo, pr_number) ->
                  format_subscription_detail route ~room_id ~repo ~pr_number
              | None, _ | _, None ->
                  Printf.sprintf "Route %s is not a PR subscription route." id))
      | "add" :: room_id :: repo :: pr_number_str :: rest -> (
          match int_of_string_opt pr_number_str with
          | None | Some 0 -> "Error: PR number must be a positive integer."
          | Some pr_number when pr_number < 0 ->
              "Error: PR number must be a positive integer."
          | Some pr_number -> (
              let profile_name =
                match rest with
                | "--profile" :: name :: _ -> name
                | _ -> "default"
              in
              let profile_id = profile_id_for_name ~db profile_name in
              let prefs = parse_notification_prefs rest in
              match
                create_compat_route ~db ~room_id ~repo ~pr_number ~profile_id
                  ~prefs
              with
              | Error error -> "Error: " ^ error
              | Ok route ->
                  Printf.sprintf
                    "Created subscription route %s for %s PR #%d in room '%s' \
                     (profile=%s)."
                    route.id repo pr_number room_id profile_name))
      | ([ "disable"; id ] | [ "enable"; id ]) as command -> (
          match find_route_by_command_id ~db id with
          | Error error -> "Error: " ^ error
          | Ok None ->
              Printf.sprintf "No subscription route found with ID %s." id
          | Ok (Some route) -> (
              let enabled =
                match command with [ "enable"; _ ] -> true | _ -> false
              in
              match
                Route.update ~db ~id:route.id ~expected_revision:route.revision
                  ~enabled ()
              with
              | Error error -> "Error: " ^ error
              | Ok _ ->
                  Printf.sprintf "%s subscription route %s."
                    (if enabled then "Enabled" else "Disabled")
                    route.id))
      | [ "remove"; id ] -> (
          match find_route_by_command_id ~db id with
          | Error error -> "Error: " ^ error
          | Ok None ->
              Printf.sprintf "No subscription route found with ID %s." id
          | Ok (Some route) -> (
              match disable_route ~db route with
              | Error error -> "Error: " ^ error
              | Ok _ -> Printf.sprintf "Removed subscription route %s." route.id
              ))
      | [ "remove"; room_id; repo; pr_number_str ] -> (
          match int_of_string_opt pr_number_str with
          | None | Some 0 -> "Error: PR number must be a positive integer."
          | Some pr_number when pr_number < 0 ->
              "Error: PR number must be a positive integer."
          | Some pr_number -> (
              match find_active_item_route ~db ~room_id ~repo ~pr_number with
              | Error error -> "Error: " ^ error
              | Ok None ->
                  Printf.sprintf
                    "No subscription found for %s PR #%d in room '%s'." repo
                    pr_number room_id
              | Ok (Some route) -> (
                  match disable_route ~db route with
                  | Error error -> "Error: " ^ error
                  | Ok _ ->
                      Printf.sprintf
                        "Removed subscription for %s PR #%d in room '%s'." repo
                        pr_number room_id)))
      | _ ->
          "Usage: clawq subscriptions <subcommand>\n\n\
           Compatibility aliases over GitHub Item routes (no legacy-table \
           writes):\n\
          \  list [--room ROOM | --repo REPO]   List subscriptions\n\
          \  show ID                             Show subscription route details\n\
          \  add ROOM REPO PR# [--profile P]     Add an Item route\n\
          \      [--on-open true|false] [--on-close true|false]\n\
          \      [--on-comment true|false] [--on-review true|false]\n\
          \      [--on-status true|false] [--on-merge true|false]\n\
          \  disable ID                          Disable a subscription route\n\
          \  enable ID                           Enable a subscription route\n\
          \  remove ID | ROOM REPO PR#           Remove a subscription route")

let cmd_subscriptions args =
  match require_admin () with
  | Some err -> err
  | None -> cmd_subscriptions_with_db ~db:(get_db ()) args

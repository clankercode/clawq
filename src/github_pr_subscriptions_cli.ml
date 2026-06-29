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

let format_subscription_detail (sub : Github_pr_subscriptions.subscription) =
  Printf.sprintf
    "Subscription #%d\n\
     Room:       %s\n\
     Repository: %s\n\
     PR:         #%d\n\
     Profile ID: %d\n\
     Enabled:    %s\n\
     Created:    %s\n\
     Updated:    %s\n\n\
     Notification Preferences:\n\
    \  on_open:   %s\n\
    \  on_close:  %s\n\
    \  on_comment: %s\n\
    \  on_review: %s\n\
    \  on_status: %s\n\
    \  on_merge:  %s"
    sub.id sub.room_id sub.repo sub.pr_number sub.profile_id
    (if sub.enabled then "yes" else "no")
    sub.created_at sub.updated_at
    (if sub.notification_preferences.on_open then "yes" else "no")
    (if sub.notification_preferences.on_close then "yes" else "no")
    (if sub.notification_preferences.on_comment then "yes" else "no")
    (if sub.notification_preferences.on_review then "yes" else "no")
    (if sub.notification_preferences.on_status then "yes" else "no")
    (if sub.notification_preferences.on_merge then "yes" else "no")

let format_subscription_row (sub : Github_pr_subscriptions.subscription) =
  [
    string_of_int sub.id;
    sub.room_id;
    sub.repo;
    Printf.sprintf "#%d" sub.pr_number;
    (if sub.enabled then "yes" else "no");
    sub.created_at;
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

let cmd_subscriptions args =
  match require_admin () with
  | Some err -> err
  | None -> (
      let db = get_db () in
      Github_pr_subscriptions.init_schema db;
      match args with
      | [ "list"; "--room"; room_id ] ->
          let subs = Github_pr_subscriptions.find_by_room ~db ~room_id in
          if subs = [] then
            Printf.sprintf "No PR subscriptions found for room '%s'." room_id
          else
            let rows = List.map format_subscription_row subs in
            Printf.sprintf "PR Subscriptions for room '%s':\n" room_id
            ^ Table_format.render subscription_columns rows
      | [ "list"; "--repo"; repo ] ->
          let subs = Github_pr_subscriptions.find_by_repo ~db ~repo in
          if subs = [] then
            Printf.sprintf "No PR subscriptions found for repo '%s'." repo
          else
            let rows = List.map format_subscription_row subs in
            Printf.sprintf "PR Subscriptions for repo '%s':\n" repo
            ^ Table_format.render subscription_columns rows
      | [ "list" ] | "list" :: _ ->
          let subs = Github_pr_subscriptions.find_all ~db () in
          if subs = [] then
            "No PR subscriptions configured. Use 'clawq subscriptions add' to \
             create one."
          else
            let rows = List.map format_subscription_row subs in
            "PR Subscriptions:\n"
            ^ Table_format.render subscription_columns rows
      | [ "show"; id_str ] -> (
          match int_of_string_opt id_str with
          | None ->
              Printf.sprintf
                "Error: '%s' is not a valid ID. Provide a numeric subscription \
                 ID."
                id_str
          | Some id -> (
              match Github_pr_subscriptions.find_by_id ~db ~id with
              | None -> Printf.sprintf "No subscription found with ID %d." id
              | Some sub -> format_subscription_detail sub))
      | "add" :: room_id :: repo :: pr_number_str :: rest -> (
          match int_of_string_opt pr_number_str with
          | None -> "Error: PR number must be a positive integer."
          | Some pr_number when pr_number <= 0 ->
              "Error: PR number must be a positive integer."
          | Some pr_number ->
              let profile_id_str =
                match rest with "--profile" :: id :: _ -> id | _ -> "default"
              in
              let db_profile_id =
                match
                  Memory_core.get_room_profile_by_name ~db ~name:profile_id_str
                with
                | Some rp -> rp.id
                | None ->
                    Memory_core.insert_room_profile ~db ~name:profile_id_str
              in
              let notification_prefs = parse_notification_prefs rest in
              let sub =
                Github_pr_subscriptions.add ~db ~room_id ~repo ~pr_number
                  ~profile_id:db_profile_id
                  ~notification_preferences:notification_prefs ()
              in
              Printf.sprintf
                "Created subscription #%d for %s PR #%d in room '%s' \
                 (profile=%s)."
                sub.id repo pr_number room_id profile_id_str)
      | [ "disable"; id_str ] -> (
          match int_of_string_opt id_str with
          | None ->
              Printf.sprintf
                "Error: '%s' is not a valid ID. Provide a numeric subscription \
                 ID."
                id_str
          | Some id ->
              if Github_pr_subscriptions.set_enabled ~db ~id ~enabled:false then
                Printf.sprintf "Disabled subscription #%d." id
              else Printf.sprintf "No subscription found with ID %d." id)
      | [ "enable"; id_str ] -> (
          match int_of_string_opt id_str with
          | None ->
              Printf.sprintf
                "Error: '%s' is not a valid ID. Provide a numeric subscription \
                 ID."
                id_str
          | Some id ->
              if Github_pr_subscriptions.set_enabled ~db ~id ~enabled:true then
                Printf.sprintf "Enabled subscription #%d." id
              else Printf.sprintf "No subscription found with ID %d." id)
      | [ "remove"; id_str ] -> (
          match int_of_string_opt id_str with
          | None ->
              Printf.sprintf
                "Error: '%s' is not a valid ID. Provide a numeric subscription \
                 ID."
                id_str
          | Some id -> (
              match Github_pr_subscriptions.find_by_id ~db ~id with
              | None -> Printf.sprintf "No subscription found with ID %d." id
              | Some sub ->
                  if
                    Github_pr_subscriptions.remove ~db ~room_id:sub.room_id
                      ~repo:sub.repo ~pr_number:sub.pr_number
                  then Printf.sprintf "Removed subscription #%d." id
                  else Printf.sprintf "Failed to remove subscription #%d." id))
      | [ "remove"; room_id; repo; pr_number_str ] -> (
          match int_of_string_opt pr_number_str with
          | None -> "Error: PR number must be a positive integer."
          | Some pr_number when pr_number <= 0 ->
              "Error: PR number must be a positive integer."
          | Some pr_number ->
              if Github_pr_subscriptions.remove ~db ~room_id ~repo ~pr_number
              then
                Printf.sprintf
                  "Removed subscription for %s PR #%d in room '%s'." repo
                  pr_number room_id
              else
                Printf.sprintf
                  "No subscription found for %s PR #%d in room '%s'." repo
                  pr_number room_id)
      | _ ->
          "Usage: clawq subscriptions <subcommand>\n\n\
           Subcommands:\n\
          \  list [--room ROOM | --repo REPO]   List subscriptions\n\
          \  show ID                             Show subscription details\n\
          \  add ROOM REPO PR# [--profile P]     Add a subscription\n\
          \      [--on-open true|false] [--on-close true|false]\n\
          \      [--on-comment true|false] [--on-review true|false]\n\
          \      [--on-status true|false] [--on-merge true|false]\n\
          \  disable ID                          Disable a subscription\n\
          \  enable ID                           Enable a subscription\n\
          \  remove ID | ROOM REPO PR#           Remove a subscription")

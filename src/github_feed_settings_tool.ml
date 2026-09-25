(** Room-scoped GitHub feed settings tool (B821).

    Event-type filtering for one Room's GitHub feed subscriptions. The operative
    mutable surface is [Github_pr_subscriptions.notification_preferences]:
    [Github_pr_dispatch] loads preferences fresh for every delivered event, so
    updates here take effect immediately. Route [filter_json] values are shown
    read-only because the subscription dispatch path does not consult them.

    Scope is the calling Room only; the Room identity is supplied by
    [Github_room_tools] from the turn's access snapshot, never from tool
    arguments. *)

module Subs = Github_pr_subscriptions
module Routes = Github_route_store

let name = "github_feed_settings"

(** {1 Args} *)

let arg_string args key =
  match args with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key args with
      | `String s ->
          let t = String.trim s in
          if t = "" then None else Some t
      | _ -> None)
  | _ -> None

let arg_int_opt args key =
  match args with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key args with
      | `Int n -> Some n
      | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
      | _ -> None)
  | _ -> None

let arg_bool args key =
  match args with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key args with
      | `Bool b -> Some b
      | _ -> None)
  | _ -> None

let arg_string_list args key =
  match args with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key args with
      | `List items ->
          let rec loop acc = function
            | [] -> Some (List.rev acc)
            | `String s :: rest -> loop (s :: acc) rest
            | _ -> None
          in
          loop [] items
      | _ -> None)
  | _ -> None

(** {1 Event families} *)

type family = Open | Close | Merge | Comment | Review | Status

let canonical_families =
  [ "open"; "close"; "merge"; "comment"; "review"; "status" ]

let family_of_token = function
  | "open" -> Some Open
  | "close" -> Some Close
  | "merge" -> Some Merge
  | "comment" -> Some Comment
  | "review" -> Some Review
  (* CI event types all gate on on_status for subscription notifications. *)
  | "status" | "check_run" | "check_suite" | "workflow_run" | "ci" ->
      Some Status
  | _ -> None

let prefs_with_family (prefs : Subs.notification_preferences) family value =
  match family with
  | Open -> { prefs with Subs.on_open = value }
  | Close -> { prefs with Subs.on_close = value }
  | Merge -> { prefs with Subs.on_merge = value }
  | Comment -> { prefs with Subs.on_comment = value }
  | Review -> { prefs with Subs.on_review = value }
  | Status -> { prefs with Subs.on_status = value }

(* Precedence: enable list, then disable list, then explicit on_* booleans. *)
let changes_of_args args =
  let family_list key value =
    match arg_string_list args key with
    | None -> Result.Ok []
    | Some tokens ->
        let rec loop acc = function
          | [] -> Result.Ok (List.rev acc)
          | tok :: rest -> (
              match family_of_token tok with
              | Some f -> loop ((f, value) :: acc) rest
              | None ->
                  Result.Error
                    (Printf.sprintf
                       "invalid event family %S in %S. Valid families: %s. CI \
                        aliases check_run, check_suite, workflow_run and ci \
                        all map to the status family."
                       tok key
                       (String.concat ", " canonical_families)))
        in
        loop [] tokens
  in
  let bool_flag key family =
    match arg_bool args key with None -> [] | Some b -> [ (family, b) ]
  in
  match family_list "enable" true with
  | Result.Error e -> Result.Error e
  | Result.Ok enables -> (
      match family_list "disable" false with
      | Result.Error e -> Result.Error e
      | Result.Ok disables ->
          Result.Ok
            (enables @ disables @ bool_flag "on_open" Open
           @ bool_flag "on_close" Close @ bool_flag "on_merge" Merge
            @ bool_flag "on_comment" Comment
            @ bool_flag "on_review" Review
            @ bool_flag "on_status" Status))

let normalize_repo s = String.lowercase_ascii (String.trim s)
let same_repo a b = normalize_repo a = normalize_repo b

(** {1 JSON shape} *)

let feed_sub_json (sub : Subs.subscription) =
  `Assoc
    [
      ("repo", `String sub.Subs.repo);
      ("pr_number", `Int sub.Subs.pr_number);
      ("enabled", `Bool sub.Subs.enabled);
      ( "notification_preferences",
        Subs.notification_preferences_to_json sub.Subs.notification_preferences
      );
      ("updated_at", `String sub.Subs.updated_at);
    ]

let selector_json (selector : Routes.selector) =
  match selector with
  | Routes.Item { repo_full_name; kind; number } ->
      `Assoc
        [
          ("type", `String "item");
          ("repo_full_name", `String repo_full_name);
          ( "kind",
            `String
              (match kind with `Pull_request -> "pr" | `Issue -> "issue") );
          ("number", `Int number);
        ]
  | Routes.Repo repo ->
      `Assoc [ ("type", `String "repo"); ("repo", `String repo) ]
  | Routes.Org org -> `Assoc [ ("type", `String "org"); ("org", `String org) ]

let route_json (route : Routes.t) =
  `Assoc
    [
      ("id", `String route.Routes.id);
      ("selector", selector_json route.Routes.selector);
      ("filter", Github_route_filter.to_json route.Routes.filter);
      ("enabled", `Bool route.Routes.enabled);
    ]

(** {1 Messages} *)

let empty_room_message room_id =
  Printf.sprintf
    "Room %S has no GitHub subscriptions or routes yet. Add one with `clawq \
     subscriptions add %s owner/repo PR#` (PR# 0 = repo-wide feed; requires \
     CLAWQ_ADMIN=1), then use this tool to tune event types."
    room_id room_id

let no_match_error ~room_id ~repo ~pr known_repos =
  let scope =
    match (repo, pr) with
    | Some r, Some p -> Printf.sprintf "repo %S PR #%d" r p
    | Some r, None -> Printf.sprintf "repo %S" r
    | None, _ -> "any repo"
  in
  Printf.sprintf
    "no GitHub subscriptions in Room %S match %s. This Room subscribes to: %s. \
     Use action \"list\" to inspect rows, or add one with `clawq subscriptions \
     add %s %s PR#`."
    room_id scope
    (if known_repos = [] then "(none)" else String.concat ", " known_repos)
    room_id
    (Option.value repo ~default:"owner/repo")

(** {1 Dispatch} *)

let list ~db ~room_id : (Yojson.Safe.t, string) result =
  let room_subs = Subs.find_by_room ~db ~room_id in
  match Routes.list_for_destination ~db ~destination:(Routes.Room room_id) with
  | Result.Error e -> Result.Error e
  | Result.Ok routes ->
      let fields =
        [
          ("room_id", `String room_id);
          ("subscriptions", `List (List.map feed_sub_json room_subs));
          ("routes", `List (List.map route_json routes));
        ]
      in
      let fields =
        if room_subs = [] && routes = [] then
          fields @ [ ("message", `String (empty_room_message room_id)) ]
        else fields
      in
      Result.Ok (`Assoc fields)

let set ~db ~room_id ~args : (Yojson.Safe.t, string) result =
  let repo = arg_string args "repo" in
  let pr = arg_int_opt args "pr" in
  let pr_checked =
    match pr with
    | Some n when n < 0 ->
        Result.Error
          "argument \"pr\" must be 0 (repo-wide row) or a positive PR number"
    | _ -> Result.Ok pr
  in
  let changes =
    match pr_checked with
    | Result.Error e -> Result.Error e
    | Result.Ok pr -> (
        match (pr, repo) with
        | Some _, None ->
            Result.Error
              "argument \"pr\" requires \"repo\": pass the owner/repo whose \
               subscription row should be updated"
        | _ -> changes_of_args args)
  in
  match changes with
  | Result.Error e -> Result.Error e
  | Result.Ok [] ->
      Result.Error
        "no preference changes requested: pass at least one boolean (on_open, \
         on_close, on_merge, on_comment, on_review, on_status) or an \
         enable/disable event-family list"
  | Result.Ok changes -> (
      let repo_matches sub =
        match repo with None -> true | Some r -> same_repo sub.Subs.repo r
      in
      let pr_matches sub =
        match pr with None -> true | Some p -> sub.Subs.pr_number = p
      in
      let room_subs = Subs.find_by_room ~db ~room_id in
      let rows =
        List.filter (fun sub -> repo_matches sub && pr_matches sub) room_subs
      in
      if rows = [] then
        Result.Error
          (no_match_error ~room_id ~repo ~pr
             (List.sort_uniq String.compare
                (List.map (fun s -> s.Subs.repo) room_subs)))
      else
        let rec apply updated = function
          | [] -> Result.Ok (List.rev updated)
          | sub :: rest -> (
              let prefs =
                List.fold_left
                  (fun acc (family, value) ->
                    prefs_with_family acc family value)
                  sub.Subs.notification_preferences changes
              in
              match
                Subs.update_preferences ~db ~room_id ~repo:sub.Subs.repo
                  ~pr_number:sub.Subs.pr_number ~preferences:prefs
              with
              | exception Not_found ->
                  Result.Error
                    (Printf.sprintf
                       "subscription %s#%d in Room %S changed while updating; \
                        re-run action \"list\" and retry"
                       sub.Subs.repo sub.Subs.pr_number room_id)
              | exception Failure msg -> Result.Error msg
              | updated_sub -> apply (updated_sub :: updated) rest)
        in
        match apply [] rows with
        | Result.Error e -> Result.Error e
        | Result.Ok updated ->
            let repo_wide_applies =
              match (repo, pr) with
              | Some _, Some p when p > 0 ->
                  List.exists
                    (fun sub -> repo_matches sub && sub.Subs.pr_number = 0)
                    room_subs
              | _ -> false
            in
            let fields =
              [
                ("room_id", `String room_id);
                ("repo", match repo with None -> `Null | Some r -> `String r);
                ("pr", match pr with None -> `Null | Some p -> `Int p);
                ("updated_count", `Int (List.length updated));
                ("updated", `List (List.map feed_sub_json updated));
              ]
            in
            let fields =
              if repo_wide_applies then
                fields
                @ [
                    ( "note",
                      `String
                        "a repo-wide subscription row (PR# 0) for this repo \
                         keeps its own preferences and still applies; repeat \
                         this call with pr=0 to change it too" );
                  ]
              else fields
            in
            Result.Ok (`Assoc fields))

let dispatch ~db ~room_id ~args : (Yojson.Safe.t, string) result =
  (* Idempotent: the subscription table ships with the main schema, the route
     table is created lazily by route tooling. *)
  Subs.init_schema db;
  Routes.ensure_schema db;
  match arg_string args "action" with
  | None ->
      Result.Error "missing required argument action (\"list\" or \"set\")"
  | Some "list" -> list ~db ~room_id
  | Some "set" -> set ~db ~room_id ~args
  | Some other ->
      Result.Error
        (Printf.sprintf "invalid action %S: expected \"list\" or \"set\"" other)

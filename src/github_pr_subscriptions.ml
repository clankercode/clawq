type notification_preferences = {
  on_open : bool;
  on_close : bool;
  on_comment : bool;
  on_review : bool;
  on_status : bool;
  on_merge : bool;
}

type subscription = {
  id : int;
  room_id : string;
  repo : string;
  pr_number : int;
  profile_id : int;
  notification_preferences : notification_preferences;
  created_at : string;
  updated_at : string;
}

let default_notification_preferences =
  {
    on_open = true;
    on_close = true;
    on_comment = true;
    on_review = true;
    on_status = true;
    on_merge = true;
  }

let notification_preferences_to_json prefs =
  `Assoc
    [
      ("on_open", `Bool prefs.on_open);
      ("on_close", `Bool prefs.on_close);
      ("on_comment", `Bool prefs.on_comment);
      ("on_review", `Bool prefs.on_review);
      ("on_status", `Bool prefs.on_status);
      ("on_merge", `Bool prefs.on_merge);
    ]

let notification_preferences_of_json json =
  match json with
  | `Assoc fields ->
      let get_bool key default =
        match List.assoc_opt key fields with
        | Some (`Bool b) -> b
        | _ -> default
      in
      {
        on_open = get_bool "on_open" true;
        on_close = get_bool "on_close" true;
        on_comment = get_bool "on_comment" true;
        on_review = get_bool "on_review" true;
        on_status = get_bool "on_status" true;
        on_merge = get_bool "on_merge" true;
      }
  | _ -> default_notification_preferences

let notification_preferences_of_string raw =
  try
    let json = Yojson.Safe.from_string raw in
    notification_preferences_of_json json
  with Yojson.Json_error _ -> default_notification_preferences

let notification_preferences_to_string prefs =
  Yojson.Safe.to_string (notification_preferences_to_json prefs)

let exec_exn db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "github_pr_subscriptions schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

let init_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS github_pr_subscriptions (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     room_id TEXT NOT NULL,\n\
    \     repo TEXT NOT NULL,\n\
    \     pr_number INTEGER NOT NULL,\n\
    \     profile_id INTEGER NOT NULL,\n\
    \     notification_preferences TEXT NOT NULL DEFAULT '{}',\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     updated_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     UNIQUE(room_id, repo, pr_number)\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_github_pr_subs_room ON \
     github_pr_subscriptions(room_id)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_github_pr_subs_repo ON \
     github_pr_subscriptions(repo, pr_number)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_github_pr_subs_profile ON \
     github_pr_subscriptions(profile_id)"

let text_column stmt idx =
  match Sqlite3.column stmt idx with Sqlite3.Data.TEXT s -> s | _ -> ""

let int_column stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | _ -> 0

let subscription_of_stmt stmt =
  {
    id = int_column stmt 0;
    room_id = text_column stmt 1;
    repo = text_column stmt 2;
    pr_number = int_column stmt 3;
    profile_id = int_column stmt 4;
    notification_preferences =
      notification_preferences_of_string (text_column stmt 5);
    created_at = text_column stmt 6;
    updated_at = text_column stmt 7;
  }

let subscription_to_json sub =
  `Assoc
    [
      ("id", `Int sub.id);
      ("room_id", `String sub.room_id);
      ("repo", `String sub.repo);
      ("pr_number", `Int sub.pr_number);
      ("profile_id", `Int sub.profile_id);
      ( "notification_preferences",
        notification_preferences_to_json sub.notification_preferences );
      ("created_at", `String sub.created_at);
      ("updated_at", `String sub.updated_at);
    ]

let subscription_to_string sub =
  Yojson.Safe.to_string (subscription_to_json sub)

let subscriptions_to_json subs = `List (List.map subscription_to_json subs)

let subscriptions_to_string subs =
  Yojson.Safe.to_string (subscriptions_to_json subs)

let bind_params stmt params =
  List.iteri
    (fun i value -> ignore (Sqlite3.bind stmt (i + 1) value : Sqlite3.Rc.t))
    params

let select_one ~db ~room_id ~repo ~pr_number =
  let sql =
    "SELECT id, room_id, repo, pr_number, profile_id, \
     notification_preferences, created_at, updated_at FROM \
     github_pr_subscriptions WHERE room_id = ? AND repo = ? AND pr_number = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          Sqlite3.Data.TEXT room_id;
          Sqlite3.Data.TEXT repo;
          Sqlite3.Data.INT (Int64.of_int pr_number);
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> subscription_of_stmt stmt
      | rc ->
          failwith
            (Printf.sprintf "github_pr_subscriptions select failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** [add ~db ~room_id ~repo ~pr_number ~profile_id ?notification_preferences ()]
    adds a subscription. If a subscription already exists for the same
    room/repo/PR, it is updated with the new profile_id and notification
    preferences. Returns the subscription. *)
let add ~db ~room_id ~repo ~pr_number ~profile_id
    ?(notification_preferences = default_notification_preferences) () =
  let prefs_json =
    notification_preferences_to_string notification_preferences
  in
  let sql =
    "INSERT INTO github_pr_subscriptions (room_id, repo, pr_number, \
     profile_id, notification_preferences) VALUES (?, ?, ?, ?, ?) ON \
     CONFLICT(room_id, repo, pr_number) DO UPDATE SET profile_id = \
     excluded.profile_id, notification_preferences = \
     excluded.notification_preferences, updated_at = datetime('now')"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          Sqlite3.Data.TEXT room_id;
          Sqlite3.Data.TEXT repo;
          Sqlite3.Data.INT (Int64.of_int pr_number);
          Sqlite3.Data.INT (Int64.of_int profile_id);
          Sqlite3.Data.TEXT prefs_json;
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "github_pr_subscriptions add failed: %s"
               (Sqlite3.Rc.to_string rc)));
  select_one ~db ~room_id ~repo ~pr_number

(** [remove ~db ~room_id ~repo ~pr_number] removes a subscription. Returns true
    if a subscription was removed. *)
let remove ~db ~room_id ~repo ~pr_number =
  let stmt =
    Sqlite3.prepare db
      "DELETE FROM github_pr_subscriptions WHERE room_id = ? AND repo = ? AND \
       pr_number = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          Sqlite3.Data.TEXT room_id;
          Sqlite3.Data.TEXT repo;
          Sqlite3.Data.INT (Int64.of_int pr_number);
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db > 0
      | rc ->
          failwith
            (Printf.sprintf "github_pr_subscriptions remove failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** [update_preferences ~db ~room_id ~repo ~pr_number ~preferences] updates the
    notification preferences for a subscription. Returns the updated
    subscription. Raises Not_found if the subscription does not exist. *)
let update_preferences ~db ~room_id ~repo ~pr_number ~preferences =
  let prefs_json = notification_preferences_to_string preferences in
  let stmt =
    Sqlite3.prepare db
      "UPDATE github_pr_subscriptions SET notification_preferences = ?, \
       updated_at = datetime('now') WHERE room_id = ? AND repo = ? AND \
       pr_number = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          Sqlite3.Data.TEXT prefs_json;
          Sqlite3.Data.TEXT room_id;
          Sqlite3.Data.TEXT repo;
          Sqlite3.Data.INT (Int64.of_int pr_number);
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE ->
          if Sqlite3.changes db > 0 then
            select_one ~db ~room_id ~repo ~pr_number
          else raise Not_found
      | rc ->
          failwith
            (Printf.sprintf
               "github_pr_subscriptions update_preferences failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** [find ~db ~room_id ~repo ~pr_number] returns the subscription if it exists.
*)
let find ~db ~room_id ~repo ~pr_number =
  let sql =
    "SELECT id, room_id, repo, pr_number, profile_id, \
     notification_preferences, created_at, updated_at FROM \
     github_pr_subscriptions WHERE room_id = ? AND repo = ? AND pr_number = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          Sqlite3.Data.TEXT room_id;
          Sqlite3.Data.TEXT repo;
          Sqlite3.Data.INT (Int64.of_int pr_number);
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (subscription_of_stmt stmt)
      | _ -> None)

(** [find_by_room ~db ~room_id] returns all subscriptions for a room. *)
let find_by_room ~db ~room_id =
  let sql =
    "SELECT id, room_id, repo, pr_number, profile_id, \
     notification_preferences, created_at, updated_at FROM \
     github_pr_subscriptions WHERE room_id = ? ORDER BY repo, pr_number"
  in
  let stmt = Sqlite3.prepare db sql in
  let subs = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.TEXT room_id ];
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            subs := subscription_of_stmt stmt :: !subs;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf "github_pr_subscriptions find_by_room failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ());
  List.rev !subs

(** [find_by_repo ~db ~repo] returns all subscriptions for a repository. *)
let find_by_repo ~db ~repo =
  let sql =
    "SELECT id, room_id, repo, pr_number, profile_id, \
     notification_preferences, created_at, updated_at FROM \
     github_pr_subscriptions WHERE repo = ? ORDER BY pr_number, room_id"
  in
  let stmt = Sqlite3.prepare db sql in
  let subs = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.TEXT repo ];
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            subs := subscription_of_stmt stmt :: !subs;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf "github_pr_subscriptions find_by_repo failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ());
  List.rev !subs

(** [find_by_repo_pr ~db ~repo ~pr_number] returns all subscriptions for a
    specific PR across all rooms. *)
let find_by_repo_pr ~db ~repo ~pr_number =
  let sql =
    "SELECT id, room_id, repo, pr_number, profile_id, \
     notification_preferences, created_at, updated_at FROM \
     github_pr_subscriptions WHERE repo = ? AND pr_number = ? ORDER BY room_id"
  in
  let stmt = Sqlite3.prepare db sql in
  let subs = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [ Sqlite3.Data.TEXT repo; Sqlite3.Data.INT (Int64.of_int pr_number) ];
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            subs := subscription_of_stmt stmt :: !subs;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf
                 "github_pr_subscriptions find_by_repo_pr failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ());
  List.rev !subs

(** [delete_by_room ~db ~room_id] removes all subscriptions for a room. Returns
    the number of subscriptions removed. *)
let delete_by_room ~db ~room_id =
  let stmt =
    Sqlite3.prepare db "DELETE FROM github_pr_subscriptions WHERE room_id = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.TEXT room_id ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db
      | rc ->
          failwith
            (Printf.sprintf "github_pr_subscriptions delete_by_room failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** [delete_by_repo ~db ~repo] removes all subscriptions for a repository.
    Returns the number of subscriptions removed. *)
let delete_by_repo ~db ~repo =
  let stmt =
    Sqlite3.prepare db "DELETE FROM github_pr_subscriptions WHERE repo = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.TEXT repo ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db
      | rc ->
          failwith
            (Printf.sprintf "github_pr_subscriptions delete_by_repo failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** [count ~db ()] returns the total number of subscriptions. *)
let count ~db () =
  let stmt =
    Sqlite3.prepare db "SELECT COUNT(*) FROM github_pr_subscriptions"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0)
      | _ -> 0)

(** [should_notify ~subscription ~event_type] checks if the subscription should
    be notified for the given event type. *)
let should_notify ~(subscription : subscription) ~event_type =
  match event_type with
  | "opened" | "reopened" -> subscription.notification_preferences.on_open
  | "closed" -> subscription.notification_preferences.on_close
  | "comment" | "issue_comment" | "review_comment" ->
      subscription.notification_preferences.on_comment
  | "review" | "review_requested" ->
      subscription.notification_preferences.on_review
  | "status" | "check_run" | "check_suite" ->
      subscription.notification_preferences.on_status
  | "merged" -> subscription.notification_preferences.on_merge
  | _ -> true

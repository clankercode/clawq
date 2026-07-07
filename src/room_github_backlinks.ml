(** Bidirectional backlink records between GitHub items and room items.

    Durable cross-references for audit trails, retries, and provenance tracking.
    Each record links one GitHub-side identifier (PR/comment/commit/workflow
    run) to one room-side identifier (message/thread/background task/review
    run/artifact).

    Idempotent via UNIQUE constraint on the composite key. Optional ID fields
    use empty string defaults (not NULL) so the UNIQUE constraint deduplicates
    correctly in SQLite. *)

(** {1 Types} *)

(** The type of item on the GitHub side. *)
type github_item_type =
  | Pr_comment
  | Pr_review_comment
  | Pr_review
  | Branch
  | Commit
  | Workflow_run
  | Check_run
  | Check_suite

(** The type of item on the room side. *)
type room_item_type =
  | Message
  | Thread
  | Background_task
  | Review_run
  | Workflow_run_room
  | Artifact

(** Which direction the relationship was established from. *)
type direction =
  | Github_to_room  (** GitHub event triggered a room action *)
  | Room_to_github  (** Room action triggered a GitHub action *)

(** The semantic relationship between the items. *)
type relationship =
  | Subscription_delivery  (** PR event delivered to subscribed room *)
  | Triggered_run  (** Room command triggered a review/workflow run *)
  | Provenance_comment  (** Background task posted result back to PR *)
  | Ci_notification  (** CI status delivered to room *)

type backlink = {
  id : int;
  repo : string;
  pr_number : int option;
  commit_sha : string option;
  github_item_type : github_item_type;
  github_item_id : string option;
  github_url : string option;
  room_id : string;
  thread_id : string option;
  room_item_type : room_item_type;
  room_item_id : string option;
  direction : direction;
  relationship : relationship;
  snapshot_id : string option;
  created_at : string;
}

(** {1 Enum serialization} *)

let github_item_type_to_string = function
  | Pr_comment -> "pr_comment"
  | Pr_review_comment -> "pr_review_comment"
  | Pr_review -> "pr_review"
  | Branch -> "branch"
  | Commit -> "commit"
  | Workflow_run -> "workflow_run"
  | Check_run -> "check_run"
  | Check_suite -> "check_suite"

let github_item_type_of_string = function
  | "pr_comment" -> Pr_comment
  | "pr_review_comment" -> Pr_review_comment
  | "pr_review" -> Pr_review
  | "branch" -> Branch
  | "commit" -> Commit
  | "workflow_run" -> Workflow_run
  | "check_run" -> Check_run
  | "check_suite" -> Check_suite
  | _ -> Commit

let room_item_type_to_string = function
  | Message -> "message"
  | Thread -> "thread"
  | Background_task -> "background_task"
  | Review_run -> "review_run"
  | Workflow_run_room -> "workflow_run"
  | Artifact -> "artifact"

let room_item_type_of_string = function
  | "message" -> Message
  | "thread" -> Thread
  | "background_task" -> Background_task
  | "review_run" -> Review_run
  | "workflow_run" -> Workflow_run_room
  | "artifact" -> Artifact
  | _ -> Message

let direction_to_string = function
  | Github_to_room -> "github_to_room"
  | Room_to_github -> "room_to_github"

let direction_of_string = function
  | "github_to_room" -> Github_to_room
  | "room_to_github" -> Room_to_github
  | _ -> Github_to_room

let relationship_to_string = function
  | Subscription_delivery -> "subscription_delivery"
  | Triggered_run -> "triggered_run"
  | Provenance_comment -> "provenance_comment"
  | Ci_notification -> "ci_notification"

let relationship_of_string = function
  | "subscription_delivery" -> Subscription_delivery
  | "triggered_run" -> Triggered_run
  | "provenance_comment" -> Provenance_comment
  | "ci_notification" -> Ci_notification
  | _ -> Subscription_delivery

(** {1 JSON serialization} *)

let backlink_to_json bl =
  `Assoc
    [
      ("id", `Int bl.id);
      ("repo", `String bl.repo);
      ("pr_number", match bl.pr_number with Some n -> `Int n | None -> `Null);
      ( "commit_sha",
        match bl.commit_sha with Some s -> `String s | None -> `Null );
      ( "github_item_type",
        `String (github_item_type_to_string bl.github_item_type) );
      ( "github_item_id",
        match bl.github_item_id with
        | Some s when s <> "" -> `String s
        | _ -> `Null );
      ( "github_url",
        match bl.github_url with Some s -> `String s | None -> `Null );
      ("room_id", `String bl.room_id);
      ( "thread_id",
        match bl.thread_id with Some s -> `String s | None -> `Null );
      ("room_item_type", `String (room_item_type_to_string bl.room_item_type));
      ( "room_item_id",
        match bl.room_item_id with
        | Some s when s <> "" -> `String s
        | _ -> `Null );
      ("direction", `String (direction_to_string bl.direction));
      ("relationship", `String (relationship_to_string bl.relationship));
      ( "snapshot_id",
        match bl.snapshot_id with Some s -> `String s | None -> `Null );
      ("created_at", `String bl.created_at);
    ]

let backlinks_to_json bls = `List (List.map backlink_to_json bls)

let backlinks_to_json_string bls =
  Yojson.Safe.pretty_to_string (backlinks_to_json bls)

(** {1 Database schema} *)

let exec_exn db sql =
  Sql_util.exec_exn ~label:"room_github_backlinks schema error" db sql

(** Ensure optional ID columns use NOT NULL DEFAULT '' so the UNIQUE constraint
    deduplicates correctly. SQLite treats NULL != NULL, so nullable columns
    would break idempotency. *)
let init_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS room_github_backlinks (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     repo TEXT NOT NULL,\n\
    \     pr_number INTEGER,\n\
    \     commit_sha TEXT,\n\
    \     github_item_type TEXT NOT NULL,\n\
    \     github_item_id TEXT NOT NULL DEFAULT '',\n\
    \     github_url TEXT,\n\
    \     room_id TEXT NOT NULL,\n\
    \     thread_id TEXT,\n\
    \     room_item_type TEXT NOT NULL,\n\
    \     room_item_id TEXT NOT NULL DEFAULT '',\n\
    \     direction TEXT NOT NULL,\n\
    \     relationship TEXT NOT NULL,\n\
    \     snapshot_id TEXT,\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     UNIQUE(repo, github_item_type, github_item_id, room_id, \
     room_item_type, room_item_id)\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_backlinks_repo_pr ON \
     room_github_backlinks(repo, pr_number)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_backlinks_room ON \
     room_github_backlinks(room_id)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_backlinks_room_item ON \
     room_github_backlinks(room_id, room_item_type)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_backlinks_github_item ON \
     room_github_backlinks(repo, github_item_type)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_backlinks_created ON \
     room_github_backlinks(created_at)"

(** {1 Database helpers} *)

let text_column = Sql_util.text_column
let int_column = Sql_util.int_column
let opt_int_column = Sql_util.opt_int_column

(* Local variant: treats empty TEXT as absent to keep UNIQUE dedup correct. *)
let opt_text_column stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.TEXT s when s <> "" -> Some s
  | Sqlite3.Data.NULL -> None
  | _ -> None

let select_columns =
  "id, repo, pr_number, commit_sha, github_item_type, github_item_id, \
   github_url, room_id, thread_id, room_item_type, room_item_id, direction, \
   relationship, snapshot_id, created_at"

let backlink_of_stmt stmt =
  {
    id = int_column stmt 0;
    repo = text_column stmt 1;
    pr_number = opt_int_column stmt 2;
    commit_sha = opt_text_column stmt 3;
    github_item_type = github_item_type_of_string (text_column stmt 4);
    github_item_id = opt_text_column stmt 5;
    github_url = opt_text_column stmt 6;
    room_id = text_column stmt 7;
    thread_id = opt_text_column stmt 8;
    room_item_type = room_item_type_of_string (text_column stmt 9);
    room_item_id = opt_text_column stmt 10;
    direction = direction_of_string (text_column stmt 11);
    relationship = relationship_of_string (text_column stmt 12);
    snapshot_id = opt_text_column stmt 13;
    created_at = text_column stmt 14;
  }

let bind_params = Sql_util.bind_params

(** Convert an optional string to a TEXT value, using empty string for None to
    ensure the UNIQUE constraint deduplicates correctly. *)
let opt_to_text = function
  | Some s -> Sqlite3.Data.TEXT s
  | None -> Sqlite3.Data.TEXT ""

(** {1 CRUD operations} *)

(** [insert ~db ~repo ?pr_number ?commit_sha ~github_item_type ?github_item_id
     ?github_url ~room_id ?thread_id ~room_item_type ?room_item_id ~direction
     ~relationship ?snapshot_id ()] inserts a backlink record. Uses INSERT OR
    IGNORE for idempotency -- duplicate composite keys are silently skipped.
    Optional ID fields use empty string defaults so the UNIQUE constraint works
    correctly (SQLite NULL != NULL would break idempotency). Returns [true] if a
    new row was inserted, [false] if it was a duplicate. *)
let insert ~db ~repo ?pr_number ?commit_sha ~github_item_type ?github_item_id
    ?github_url ~room_id ?thread_id ~room_item_type ?room_item_id ~direction
    ~relationship ?snapshot_id () =
  let sql =
    "INSERT OR IGNORE INTO room_github_backlinks (repo, pr_number, commit_sha, \
     github_item_type, github_item_id, github_url, room_id, thread_id, \
     room_item_type, room_item_id, direction, relationship, snapshot_id) \
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          Sqlite3.Data.TEXT repo;
          (match pr_number with
          | Some n -> Sqlite3.Data.INT (Int64.of_int n)
          | None -> Sqlite3.Data.NULL);
          (match commit_sha with
          | Some s -> Sqlite3.Data.TEXT s
          | None -> Sqlite3.Data.NULL);
          Sqlite3.Data.TEXT (github_item_type_to_string github_item_type);
          opt_to_text github_item_id;
          (match github_url with
          | Some s -> Sqlite3.Data.TEXT s
          | None -> Sqlite3.Data.NULL);
          Sqlite3.Data.TEXT room_id;
          (match thread_id with
          | Some s -> Sqlite3.Data.TEXT s
          | None -> Sqlite3.Data.NULL);
          Sqlite3.Data.TEXT (room_item_type_to_string room_item_type);
          opt_to_text room_item_id;
          Sqlite3.Data.TEXT (direction_to_string direction);
          Sqlite3.Data.TEXT (relationship_to_string relationship);
          (match snapshot_id with
          | Some s -> Sqlite3.Data.TEXT s
          | None -> Sqlite3.Data.NULL);
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db > 0
      | rc ->
          failwith
            (Printf.sprintf "room_github_backlinks insert failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** [find_by_github ~db ~repo ~github_item_type ?github_item_id ()] finds all
    backlinks from a specific GitHub item. *)
let find_by_github ~db ~repo ~github_item_type ?github_item_id () =
  let sql_base =
    Printf.sprintf
      "SELECT %s FROM room_github_backlinks WHERE repo = ? AND \
       github_item_type = ?"
      select_columns
  in
  let sql, params =
    match github_item_id with
    | Some id ->
        ( sql_base ^ " AND github_item_id = ? ORDER BY created_at DESC",
          [
            Sqlite3.Data.TEXT repo;
            Sqlite3.Data.TEXT (github_item_type_to_string github_item_type);
            Sqlite3.Data.TEXT id;
          ] )
    | None ->
        ( sql_base ^ " ORDER BY created_at DESC",
          [
            Sqlite3.Data.TEXT repo;
            Sqlite3.Data.TEXT (github_item_type_to_string github_item_type);
          ] )
  in
  let stmt = Sqlite3.prepare db sql in
  let results = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt params;
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            results := backlink_of_stmt stmt :: !results;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf "room_github_backlinks find_by_github failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ());
  List.rev !results

(** [find_by_room ~db ~room_id ?room_item_type ?room_item_id ()] finds all
    backlinks from a specific room item. *)
let find_by_room ~db ~room_id ?room_item_type ?room_item_id () =
  let filters = ref [ "room_id = ?" ] in
  let params = ref [ Sqlite3.Data.TEXT room_id ] in
  Option.iter
    (fun t ->
      filters := "room_item_type = ?" :: !filters;
      params := Sqlite3.Data.TEXT (room_item_type_to_string t) :: !params)
    room_item_type;
  Option.iter
    (fun id ->
      filters := "room_item_id = ?" :: !filters;
      params := Sqlite3.Data.TEXT id :: !params)
    room_item_id;
  let where_clause = String.concat " AND " (List.rev !filters) in
  let sql =
    Printf.sprintf
      "SELECT %s FROM room_github_backlinks WHERE %s ORDER BY created_at DESC"
      select_columns where_clause
  in
  let stmt = Sqlite3.prepare db sql in
  let results = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt (List.rev !params);
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            results := backlink_of_stmt stmt :: !results;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf "room_github_backlinks find_by_room failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ());
  List.rev !results

(** [find_by_repo_pr ~db ~repo ~pr_number ()] finds all backlinks for a PR. *)
let find_by_repo_pr ~db ~repo ~pr_number () =
  let sql =
    Printf.sprintf
      "SELECT %s FROM room_github_backlinks WHERE repo = ? AND pr_number = ? \
       ORDER BY created_at DESC"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  let results = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [ Sqlite3.Data.TEXT repo; Sqlite3.Data.INT (Int64.of_int pr_number) ];
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            results := backlink_of_stmt stmt :: !results;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf "room_github_backlinks find_by_repo_pr failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ());
  List.rev !results

(** [count_by_room ~db ~room_id ()] counts backlinks for a room. *)
let count_by_room ~db ~room_id () =
  let stmt =
    Sqlite3.prepare db
      "SELECT COUNT(*) FROM room_github_backlinks WHERE room_id = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.TEXT room_id ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> int_column stmt 0
      | _ -> 0)

(** [delete_before ~db ~before_timestamp ()] deletes backlinks older than the
    given timestamp. Returns the number of rows deleted. *)
let delete_before ~db ~before_timestamp () =
  let stmt =
    Sqlite3.prepare db "DELETE FROM room_github_backlinks WHERE created_at < ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.TEXT before_timestamp ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db
      | rc ->
          failwith
            (Printf.sprintf "room_github_backlinks delete_before failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** {1 Convenience insert helpers} *)

(** [record_subscription_delivery ~db ~repo ~pr_number ?github_item_id
     ?github_url ~room_id ~event_type ?snapshot_id ?thread_id ()] records that a
    GitHub PR event was delivered to a room via subscription. The
    [github_item_id] should be the actual GitHub identifier (comment_id,
    review_id, etc.) for durable lookup. *)
let record_subscription_delivery ~db ~repo ~pr_number ?github_item_id
    ?github_url ~room_id ~event_type ?snapshot_id ?thread_id () =
  let github_item_type =
    match event_type with
    | s when s = "pull_request" -> Pr_comment
    | s when s = "issue_comment" -> Pr_comment
    | s when s = "pr_review_comment" -> Pr_review_comment
    | s when s = "pull_request_review" -> Pr_review
    | s when s = "check_run" -> Check_run
    | s when s = "check_suite" -> Check_suite
    | s when s = "workflow_run" -> Workflow_run
    | _ -> Pr_comment
  in
  ignore
    (insert ~db ~repo ~pr_number ~github_item_type ?github_item_id ?github_url
       ~room_id ?thread_id ~room_item_type:Message ~direction:Github_to_room
       ~relationship:Subscription_delivery ?snapshot_id ())

(** [record_ci_notification ~db ~repo ~pr_number ~github_item_type ?github_url
     ~room_id ?snapshot_id ?thread_id ?room_item_id ()] records that a CI event
    was delivered to a room. *)
let record_ci_notification ~db ~repo ~pr_number ~github_item_type ?github_url
    ~room_id ?snapshot_id ?thread_id ?room_item_id () =
  ignore
    (insert ~db ~repo ~pr_number ~github_item_type ?github_url ~room_id
       ?thread_id ~room_item_type:Message ?room_item_id
       ~direction:Github_to_room ~relationship:Ci_notification ?snapshot_id ())

(** [record_triggered_run ~db ~repo ~pr_number ~github_item_type ?github_url
     ~room_id ?thread_id ~room_item_type ~room_item_id ?snapshot_id ()] records
    that a room command triggered a GitHub-side action (review run). *)
let record_triggered_run ~db ~repo ~pr_number ~github_item_type ?github_url
    ~room_id ?thread_id ~room_item_type ?room_item_id ?snapshot_id () =
  ignore
    (insert ~db ~repo ~pr_number ~github_item_type ?github_url ~room_id
       ?thread_id ~room_item_type ?room_item_id ~direction:Room_to_github
       ~relationship:Triggered_run ?snapshot_id ())

(** [record_provenance_comment ~db ~repo ~pr_number ?github_item_id ?github_url
     ~room_id ?thread_id ?room_item_id ?snapshot_id ()] records that a
    background task / room action posted a result comment back to a GitHub PR.
    This is the Room -> GitHub direction with [Provenance_comment] relationship.
*)
let record_provenance_comment ~db ~repo ~pr_number ?github_item_id ?github_url
    ~room_id ?thread_id ?room_item_id ?snapshot_id () =
  ignore
    (insert ~db ~repo ~pr_number ~github_item_type:Pr_comment ?github_item_id
       ?github_url ~room_id ?thread_id ~room_item_type:Message ?room_item_id
       ~direction:Room_to_github ~relationship:Provenance_comment ?snapshot_id
       ())

(** [record_room_to_room ~db ~room_id ?thread_id ~room_item_type ~room_item_id
     ~linked_room_item_type ~linked_room_item_id ?snapshot_id ()] records a
    backlink between two room-side items (e.g., background task -> review run).
    Uses empty strings for repo/github fields since this is not a GitHub link.
*)
let record_room_to_room ~db ~room_id ?thread_id ~room_item_type ~room_item_id
    ~linked_room_item_type ~linked_room_item_id ?snapshot_id () =
  ignore
    (insert ~db ~repo:"" ~github_item_type:Commit ~room_id ?thread_id
       ~room_item_type:linked_room_item_type ~room_item_id:linked_room_item_id
       ~direction:Room_to_github ~relationship:Triggered_run ?snapshot_id ())

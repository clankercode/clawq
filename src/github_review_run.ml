(** Explicit GitHub review run trigger model.

    Provides bounded review/security/code task requests against granted
    repos/PRs with idempotency by repo/PR/head SHA/run kind.

    Trigger sources: labels, subscription rules, or room commands. *)

(** {1 Types} *)

(** The kind of review run to execute. *)
type run_kind =
  | Code_review  (** Standard code review analysis *)
  | Security_scan  (** Security vulnerability scan *)
  | Custom of string  (** Custom review kind with user-defined name *)

(** What triggered this review run. *)
type trigger_source =
  | Label of string  (** Triggered by a GitHub label *)
  | Subscription_rule  (** Triggered by a subscription rule match *)
  | Room_command of { room_id : string; requester_id : string }
      (** Triggered by a room command *)
  | Manual  (** Manually triggered via CLI or API *)

(** Status of a review run. *)
type run_status =
  | Pending  (** Queued but not yet started *)
  | Running  (** Currently executing *)
  | Completed  (** Finished successfully *)
  | Failed  (** Finished with error *)

type review_run = {
  id : int;
  repo : string;  (** Full repo name: owner/repo *)
  pr_number : int;
  head_sha : string;  (** Commit SHA at time of trigger *)
  run_kind : run_kind;
  trigger_source : trigger_source;
  status : run_status;
  task_id : int option;  (** Associated background task ID *)
  result_preview : string option;  (** Summary of results *)
  error_message : string option;  (** Error details if failed *)
  created_at : string;
  started_at : string option;
  finished_at : string option;
}
(** A review run record with full context. *)

(** {1 JSON serialization} *)

let run_kind_to_json kind =
  `String
    (match kind with
    | Code_review -> "code_review"
    | Security_scan -> "security_scan"
    | Custom name -> "custom:" ^ name)

let run_kind_of_json json =
  match json with
  | `String "code_review" -> Code_review
  | `String "security_scan" -> Security_scan
  | `String s when String.length s > 7 && String.sub s 0 7 = "custom:" ->
      Custom (String.sub s 7 (String.length s - 7))
  | _ -> Code_review

let trigger_source_to_json = function
  | Label label ->
      `Assoc [ ("type", `String "label"); ("label", `String label) ]
  | Subscription_rule -> `Assoc [ ("type", `String "subscription_rule") ]
  | Room_command { room_id; requester_id } ->
      `Assoc
        [
          ("type", `String "room_command");
          ("room_id", `String room_id);
          ("requester_id", `String requester_id);
        ]
  | Manual -> `Assoc [ ("type", `String "manual") ]

let trigger_source_of_json json =
  let open Yojson.Safe.Util in
  let typ =
    json |> member "type" |> to_string_option |> Option.value ~default:""
  in
  match typ with
  | "label" ->
      let label =
        json |> member "label" |> to_string_option |> Option.value ~default:""
      in
      Label label
  | "subscription_rule" -> Subscription_rule
  | "room_command" ->
      let room_id =
        json |> member "room_id" |> to_string_option |> Option.value ~default:""
      in
      let requester_id =
        json |> member "requester_id" |> to_string_option
        |> Option.value ~default:""
      in
      Room_command { room_id; requester_id }
  | "manual" -> Manual
  | _ -> Manual

let review_run_to_json run =
  `Assoc
    [
      ("id", `Int run.id);
      ("repo", `String run.repo);
      ("pr_number", `Int run.pr_number);
      ("head_sha", `String run.head_sha);
      ("run_kind", run_kind_to_json run.run_kind);
      ("trigger_source", trigger_source_to_json run.trigger_source);
      ( "status",
        `String
          (match run.status with
          | Pending -> "pending"
          | Running -> "running"
          | Completed -> "completed"
          | Failed -> "failed") );
      ("task_id", match run.task_id with Some id -> `Int id | None -> `Null);
      ( "result_preview",
        match run.result_preview with Some s -> `String s | None -> `Null );
      ( "error_message",
        match run.error_message with Some s -> `String s | None -> `Null );
      ("created_at", `String run.created_at);
      ( "started_at",
        match run.started_at with Some s -> `String s | None -> `Null );
      ( "finished_at",
        match run.finished_at with Some s -> `String s | None -> `Null );
    ]

let review_run_to_string run = Yojson.Safe.to_string (review_run_to_json run)

(** {1 String serialization (legacy format)} *)

let run_kind_to_string = function
  | Code_review -> "code_review"
  | Security_scan -> "security_scan"
  | Custom name -> "custom:" ^ name

let run_kind_of_string = function
  | "code_review" -> Code_review
  | "security_scan" -> Security_scan
  | s when String.length s > 7 && String.sub s 0 7 = "custom:" ->
      Custom (String.sub s 7 (String.length s - 7))
  | _ -> Code_review

let trigger_source_to_string = function
  | Label label -> "label:" ^ label
  | Subscription_rule -> "subscription_rule"
  | Room_command { room_id; requester_id } ->
      Printf.sprintf "room_command:%s:%s" room_id requester_id
  | Manual -> "manual"

let trigger_source_of_string s =
  (* Try JSON parsing first (new format used in DB) *)
  (try Some (trigger_source_of_json (Yojson.Safe.from_string s))
   with _ -> None)
  |> function
  | Some source -> source
  | None ->
      if
        (* Fall back to legacy string format *)
        s = "subscription_rule"
      then Subscription_rule
      else if s = "manual" then Manual
      else if String.length s > 6 && String.sub s 0 6 = "label:" then
        Label (String.sub s 6 (String.length s - 6))
      else if String.length s > 13 && String.sub s 0 13 = "room_command:" then
        match String.split_on_char ':' s with
        | [ _; room_id; requester_id ] -> Room_command { room_id; requester_id }
        | _ -> Manual
      else Manual

let run_status_to_string = function
  | Pending -> "pending"
  | Running -> "running"
  | Completed -> "completed"
  | Failed -> "failed"

let run_status_of_string = function
  | "pending" -> Pending
  | "running" -> Running
  | "completed" -> Completed
  | "failed" -> Failed
  | _ -> Pending

(** {1 Database schema} *)

let exec_exn db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "github_review_run schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

(** Create the review_runs table. Idempotent via IF NOT EXISTS. *)
let init_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS github_review_runs (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     repo TEXT NOT NULL,\n\
    \     pr_number INTEGER NOT NULL,\n\
    \     head_sha TEXT NOT NULL,\n\
    \     run_kind TEXT NOT NULL,\n\
    \     trigger_source TEXT NOT NULL,\n\
    \     status TEXT NOT NULL DEFAULT 'pending',\n\
    \     task_id INTEGER,\n\
    \     result_preview TEXT,\n\
    \     error_message TEXT,\n\
    \     created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \     started_at TEXT,\n\
    \     finished_at TEXT,\n\
    \     UNIQUE(repo, pr_number, head_sha, run_kind)\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_review_runs_repo_pr ON \
     github_review_runs(repo, pr_number)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_review_runs_status ON \
     github_review_runs(status)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_review_runs_task ON \
     github_review_runs(task_id)"

(** {1 Database helpers} *)

let text_column stmt idx =
  match Sqlite3.column stmt idx with Sqlite3.Data.TEXT s -> s | _ -> ""

let int_column stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | _ -> 0

let opt_int_column stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.INT n -> Some (Int64.to_int n)
  | Sqlite3.Data.NULL -> None
  | _ -> None

let opt_text_column stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.TEXT s -> Some s
  | Sqlite3.Data.NULL -> None
  | _ -> None

let review_run_of_stmt stmt =
  {
    id = int_column stmt 0;
    repo = text_column stmt 1;
    pr_number = int_column stmt 2;
    head_sha = text_column stmt 3;
    run_kind = run_kind_of_string (text_column stmt 4);
    trigger_source = trigger_source_of_string (text_column stmt 5);
    status = run_status_of_string (text_column stmt 6);
    task_id = opt_int_column stmt 7;
    result_preview = opt_text_column stmt 8;
    error_message = opt_text_column stmt 9;
    created_at = text_column stmt 10;
    started_at = opt_text_column stmt 11;
    finished_at = opt_text_column stmt 12;
  }

let bind_params stmt params =
  List.iteri
    (fun i value -> ignore (Sqlite3.bind stmt (i + 1) value : Sqlite3.Rc.t))
    params

let select_columns =
  "id, repo, pr_number, head_sha, run_kind, trigger_source, status, task_id, \
   result_preview, error_message, created_at, started_at, finished_at"

(** {1 CRUD operations} *)

(** [find_by_identity ~db ~repo ~pr_number ~head_sha ~run_kind] finds an
    existing review run by its idempotency key. *)
let find_by_identity ~db ~repo ~pr_number ~head_sha ~run_kind =
  let sql =
    Printf.sprintf
      "SELECT %s FROM github_review_runs WHERE repo = ? AND pr_number = ? AND \
       head_sha = ? AND run_kind = ?"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          Sqlite3.Data.TEXT repo;
          Sqlite3.Data.INT (Int64.of_int pr_number);
          Sqlite3.Data.TEXT head_sha;
          Sqlite3.Data.TEXT (run_kind_to_string run_kind);
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (review_run_of_stmt stmt)
      | _ -> None)

(** [find_pending_by_identity ~db ~repo ~pr_number ~head_sha ~run_kind] finds a
    pending or running review run. Returns None if the run is completed or
    failed. *)
let find_pending_by_identity ~db ~repo ~pr_number ~head_sha ~run_kind =
  match find_by_identity ~db ~repo ~pr_number ~head_sha ~run_kind with
  | Some run when run.status = Pending || run.status = Running -> Some run
  | _ -> None

(** [create ~db ~repo ~pr_number ~head_sha ~run_kind ~trigger_source ()] creates
    a new review run. If one already exists with the same identity
    (repo/PR/head_sha/run_kind), returns the existing one instead (idempotency).
    Uses INSERT OR IGNORE for race safety. *)
let create ~db ~repo ~pr_number ~head_sha ~run_kind ~trigger_source () =
  let trigger_json =
    Yojson.Safe.to_string (trigger_source_to_json trigger_source)
  in
  (* Use INSERT OR IGNORE for race-safe idempotency *)
  let sql =
    "INSERT OR IGNORE INTO github_review_runs (repo, pr_number, head_sha, \
     run_kind, trigger_source, status) VALUES (?, ?, ?, ?, ?, 'pending')"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          Sqlite3.Data.TEXT repo;
          Sqlite3.Data.INT (Int64.of_int pr_number);
          Sqlite3.Data.TEXT head_sha;
          Sqlite3.Data.TEXT (run_kind_to_string run_kind);
          Sqlite3.Data.TEXT trigger_json;
        ];
      ignore (Sqlite3.step stmt : Sqlite3.Rc.t));
  (* Always retrieve the record (either newly inserted or existing) *)
  match find_by_identity ~db ~repo ~pr_number ~head_sha ~run_kind with
  | Some run -> run
  | None -> failwith "github_review_run create: record not found after insert"

(** [set_running ~db ~id ~task_id] marks a review run as running and associates
    it with a background task. Returns true if updated. *)
let set_running ~db ~id ~task_id =
  let sql =
    "UPDATE github_review_runs SET status = 'running', task_id = ?, started_at \
     = datetime('now') WHERE id = ? AND status = 'pending'"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          Sqlite3.Data.INT (Int64.of_int task_id);
          Sqlite3.Data.INT (Int64.of_int id);
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db > 0
      | rc ->
          failwith
            (Printf.sprintf "github_review_run set_running failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** [set_completed ~db ~id ~result_preview] marks a review run as completed. *)
let set_completed ~db ~id ?result_preview () =
  let sql =
    "UPDATE github_review_runs SET status = 'completed', result_preview = ?, \
     finished_at = datetime('now') WHERE id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [
          (match result_preview with
          | Some s -> Sqlite3.Data.TEXT s
          | None -> Sqlite3.Data.NULL);
          Sqlite3.Data.INT (Int64.of_int id);
        ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db > 0
      | rc ->
          failwith
            (Printf.sprintf "github_review_run set_completed failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** [set_failed ~db ~id ~error_message] marks a review run as failed. *)
let set_failed ~db ~id ~error_message =
  let sql =
    "UPDATE github_review_runs SET status = 'failed', error_message = ?, \
     finished_at = datetime('now') WHERE id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [ Sqlite3.Data.TEXT error_message; Sqlite3.Data.INT (Int64.of_int id) ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Sqlite3.changes db > 0
      | rc ->
          failwith
            (Printf.sprintf "github_review_run set_failed failed: %s"
               (Sqlite3.Rc.to_string rc)))

(** [find_by_id ~db ~id] retrieves a review run by its ID. *)
let find_by_id ~db ~id =
  let sql =
    Printf.sprintf "SELECT %s FROM github_review_runs WHERE id = ?"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.INT (Int64.of_int id) ];
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (review_run_of_stmt stmt)
      | _ -> None)

(** [find_by_repo_pr ~db ~repo ~pr_number] returns all review runs for a PR. *)
let find_by_repo_pr ~db ~repo ~pr_number =
  let sql =
    Printf.sprintf
      "SELECT %s FROM github_review_runs WHERE repo = ? AND pr_number = ? \
       ORDER BY created_at DESC"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  let runs = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt
        [ Sqlite3.Data.TEXT repo; Sqlite3.Data.INT (Int64.of_int pr_number) ];
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            runs := review_run_of_stmt stmt :: !runs;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf "github_review_run find_by_repo_pr failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ());
  List.rev !runs

(** [find_pending ~db ?limit ()] returns pending review runs. *)
let find_pending ~db ?(limit = 50) () =
  let sql =
    Printf.sprintf
      "SELECT %s FROM github_review_runs WHERE status = 'pending' ORDER BY \
       created_at ASC LIMIT ?"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  let runs = ref [] in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind_params stmt [ Sqlite3.Data.INT (Int64.of_int limit) ];
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            runs := review_run_of_stmt stmt :: !runs;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf "github_review_run find_pending failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ());
  List.rev !runs

(** [count_by_status ~db ()] returns counts of review runs by status. *)
let count_by_status ~db () =
  let sql = "SELECT status, COUNT(*) FROM github_review_runs GROUP BY status" in
  let stmt = Sqlite3.prepare db sql in
  let counts = Hashtbl.create 4 in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            let status = text_column stmt 0 in
            let count = int_column stmt 1 in
            Hashtbl.replace counts status count;
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc ->
            failwith
              (Printf.sprintf "github_review_run count_by_status failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop ());
  counts

(** {1 Label detection} *)

(** [label_to_run_kind label] maps a GitHub label to a review run kind. Returns
    [None] if the label is not a review trigger. *)
let label_to_run_kind label =
  let lower = String.lowercase_ascii label in
  if lower = "review" || lower = "code-review" || lower = "needs-review" then
    Some Code_review
  else if
    lower = "security" || lower = "security-review" || lower = "security-scan"
  then Some Security_scan
  else None

(** {1 Trigger logic} *)

(** [trigger_from_label ~db ~repo ~pr_number ~head_sha ~label] triggers a review
    run if the label maps to a run kind. Returns [Some run] if triggered or
    [None] if the label is not a trigger. *)
let trigger_from_label ~db ~repo ~pr_number ~head_sha ~label =
  match label_to_run_kind label with
  | Some run_kind ->
      let trigger = Label label in
      Some
        (create ~db ~repo ~pr_number ~head_sha ~run_kind ~trigger_source:trigger
           ())
  | None -> None

(** [trigger_from_room_command ~db ~repo ~pr_number ~head_sha ~run_kind ~room_id
     ~requester_id] triggers a review run from a room command. Returns the
    review run (existing or new). *)
let trigger_from_room_command ~db ~repo ~pr_number ~head_sha ~run_kind ~room_id
    ~requester_id =
  let trigger = Room_command { room_id; requester_id } in
  let run =
    create ~db ~repo ~pr_number ~head_sha ~run_kind ~trigger_source:trigger ()
  in
  (* Record backlink from room to GitHub review run *)
  Room_github_backlinks.record_triggered_run ~db ~repo ~pr_number
    ~github_item_type:Pr_comment ~room_id ~room_item_type:Review_run
    ~room_item_id:(string_of_int run.id) ();
  run

(** {1 Prompt assembly} *)

(** Human-readable description of a run kind for the prompt. *)
let run_kind_description = function
  | Code_review -> "code review"
  | Security_scan -> "security vulnerability scan"
  | Custom name -> name

(** [build_review_prompt ~repo ~pr_number ~pr_title ~pr_author ~pr_body
     ~base_branch ~head_branch ~head_sha ~pr_files ~run_kind ~trigger_source ()]
    assembles the enriched prompt for a review run background task. Includes PR
    metadata, changed files, and the review task description. *)
let build_review_prompt ~repo ~pr_number ~pr_title ~pr_author ~pr_body
    ~base_branch ~head_branch ~head_sha ~pr_files ~run_kind ~trigger_source () =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf
    (Printf.sprintf "You are performing a %s on a GitHub pull request.\n\n"
       (run_kind_description run_kind));
  Buffer.add_string buf "## PR Metadata\n";
  Buffer.add_string buf (Printf.sprintf "Repository: %s\n" repo);
  Buffer.add_string buf (Printf.sprintf "PR Number: #%d\n" pr_number);
  Buffer.add_string buf (Printf.sprintf "Title: %s\n" pr_title);
  Buffer.add_string buf (Printf.sprintf "Author: @%s\n" pr_author);
  Buffer.add_string buf
    (Printf.sprintf "Branch: `%s` -> `%s`\n" head_branch base_branch);
  Buffer.add_string buf (Printf.sprintf "Head SHA: %s\n" head_sha);
  if pr_body <> "" then begin
    let truncated =
      if String.length pr_body > 2000 then String.sub pr_body 0 1997 ^ "..."
      else pr_body
    in
    Buffer.add_string buf (Printf.sprintf "\nPR Description:\n%s\n" truncated)
  end;
  if pr_files <> [] then begin
    let count = List.length pr_files in
    Buffer.add_string buf (Printf.sprintf "\nChanged files (%d):\n" count);
    let show = min 30 count in
    List.iteri
      (fun i (filename, status, additions, deletions) ->
        if i < show then
          Buffer.add_string buf
            (Printf.sprintf "  - %s %s (+%d -%d)\n" filename status additions
               deletions))
      pr_files;
    if count > 30 then
      Buffer.add_string buf
        (Printf.sprintf "  ... and %d more files\n" (count - 30))
  end;
  Buffer.add_string buf
    (Printf.sprintf "\n## Trigger\nTriggered by: %s\n"
       (trigger_source_to_string trigger_source));
  (match run_kind with
  | Code_review ->
      Buffer.add_string buf
        "\n\
         ## Task\n\
         Provide a thorough code review. Focus on:\n\
         - Correctness and potential bugs\n\
         - Code quality and maintainability\n\
         - Performance implications\n\
         - Security considerations\n\
         Summarize findings with severity levels (critical, warning, info)."
  | Security_scan ->
      Buffer.add_string buf
        "\n\
         ## Task\n\
         Perform a security vulnerability scan. Focus on:\n\
         - Input validation and injection risks\n\
         - Authentication and authorization issues\n\
         - Data exposure and leakage risks\n\
         - Dependency vulnerabilities\n\
         - Cryptographic issues\n\
         Report findings with CVSS-like severity ratings."
  | Custom name ->
      Buffer.add_string buf
        (Printf.sprintf
           "\n## Task\nPerform a %s review on the changes in this PR." name));
  Buffer.contents buf

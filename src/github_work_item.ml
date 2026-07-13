(** B771: durable, provider-neutral GitHub work items.

    Normalizes a verified GitHub trigger (initially the /clawq issue-comment
    command) into a persistent envelope that can be executed by any runner
    (Codex/Claude/...) on any session host (direct/Herdr/...) and published back
    to the originating thread exactly once.

    The envelope is transport-agnostic on purpose: later queue transports (B774
    remote subscriber workers) reuse the same table and lifecycle. Delivery is
    at-least-once upstream, so creation is idempotent on [dedup_key] and
    publication is idempotent on [published_comment_id]. *)

(** {1 Types} *)

type status =
  | Queued
  | Running
  | Blocked  (** accepted but cannot proceed (e.g. host/runner unavailable) *)
  | Succeeded
  | Failed
  | Cancelled

let string_of_status = function
  | Queued -> "queued"
  | Running -> "running"
  | Blocked -> "blocked"
  | Succeeded -> "succeeded"
  | Failed -> "failed"
  | Cancelled -> "cancelled"

let status_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "queued" -> Some Queued
  | "running" -> Some Running
  | "blocked" -> Some Blocked
  | "succeeded" -> Some Succeeded
  | "failed" -> Some Failed
  | "cancelled" -> Some Cancelled
  | _ -> None

let is_terminal_status = function
  | Succeeded | Failed | Cancelled -> true
  | Queued | Running | Blocked -> false

type result_kind = Reply | Change | Result_blocked | Result_failed

let string_of_result_kind = function
  | Reply -> "reply"
  | Change -> "change"
  | Result_blocked -> "blocked"
  | Result_failed -> "failed"

let result_kind_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "reply" -> Some Reply
  | "change" -> Some Change
  | "blocked" -> Some Result_blocked
  | "failed" -> Some Result_failed
  | _ -> None

type t = {
  id : int;
  dedup_key : string;  (** stable idempotency key; UNIQUE in the table *)
  delivery_id : string option;  (** X-GitHub-Delivery when available *)
  repo_full_name : string;  (** "owner/repo" *)
  is_pr : bool;
  issue_number : int;
  requester : string;  (** GitHub login that triggered the item *)
  trigger : string;  (** e.g. "slash_command"; later "mention", "assignment" *)
  runner_pref : string option;  (** "auto" | runner name; no credentials *)
  host_pref : string option;  (** session-host kind preference *)
  prompt : string;  (** the user's request text (untrusted data) *)
  preamble : string;  (** generated context block (trusted, derived) *)
  policy_ref : string option;  (** repository policy reference (B772) *)
  status : status;
  background_task_id : int option;
  result_kind : result_kind option;
  result_summary : string option;
  published_comment_id : int option;
      (** GitHub comment id of the published result; presence short-circuits
          re-publication on retries *)
  publication_status : string option;  (** "published" | "failed: ..." *)
  publication_branch : string option;
      (** deterministic restricted branch pushed by the publisher *)
  published_pr_number : int option;
      (** draft PR number once published; presence short-circuits retries *)
  ack_comment_id : int option;
      (** placeholder/ack comment posted at intake; edited into the final reply
          at publication *)
  attempt_count : int;
  created_at : string;
  started_at : string option;
  finished_at : string option;
  actor_snapshot_json : Yojson.Safe.t option;
      (** Immutable initiating Actor_snapshot JSON (token-free). Preserved
          across retry / cancel / restart. [None] for legacy unattributed work
          items. *)
}

(** {1 Schema} *)

let init_schema db =
  let exec sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | rc ->
        failwith
          (Printf.sprintf "SQLite error: %s (sql: %s)" (Sqlite3.Rc.to_string rc)
             sql)
  in
  exec
    "CREATE TABLE IF NOT EXISTS github_work_items (\n\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \  dedup_key TEXT NOT NULL UNIQUE,\n\
    \  delivery_id TEXT,\n\
    \  repo_full_name TEXT NOT NULL,\n\
    \  is_pr INTEGER NOT NULL DEFAULT 0,\n\
    \  issue_number INTEGER NOT NULL,\n\
    \  requester TEXT NOT NULL,\n\
    \  trigger TEXT NOT NULL DEFAULT 'slash_command',\n\
    \  runner_pref TEXT,\n\
    \  host_pref TEXT,\n\
    \  prompt TEXT NOT NULL,\n\
    \  preamble TEXT NOT NULL DEFAULT '',\n\
    \  policy_ref TEXT,\n\
    \  status TEXT NOT NULL DEFAULT 'queued',\n\
    \  background_task_id INTEGER,\n\
    \  result_kind TEXT,\n\
    \  result_summary TEXT,\n\
    \  published_comment_id INTEGER,\n\
    \  publication_status TEXT,\n\
    \  publication_branch TEXT,\n\
    \  published_pr_number INTEGER,\n\
    \  ack_comment_id INTEGER,\n\
    \  attempt_count INTEGER NOT NULL DEFAULT 0,\n\
    \  created_at TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \  started_at TEXT,\n\
    \  finished_at TEXT,\n\
    \  actor_snapshot_json TEXT\n\
     )";
  exec
    "CREATE INDEX IF NOT EXISTS idx_github_work_items_status ON \
     github_work_items (status)";
  exec
    "CREATE INDEX IF NOT EXISTS idx_github_work_items_task ON \
     github_work_items (background_task_id)";
  (* Migrate pre-P21 work items that predate attribution. *)
  match
    Sqlite3.exec db
      "ALTER TABLE github_work_items ADD COLUMN actor_snapshot_json TEXT"
  with
  | Sqlite3.Rc.OK -> ()
  | _ -> ()

(** {1 Row mapping} *)

let sql_text = Sql_util.sql_text
let sql_int = Sql_util.sql_int

let parse_actor_snapshot_col = function
  | None -> None
  | Some s when String.trim s = "" -> None
  | Some s -> (
      try
        let j = Yojson.Safe.from_string s in
        if Actor_snapshot.contains_token_material j then None else Some j
      with _ -> None)

let of_stmt stmt : t =
  let text i = Sqlite3.column stmt i |> sql_text in
  let int_opt i = Sqlite3.column stmt i |> sql_int in
  {
    id = Option.value (int_opt 0) ~default:0;
    dedup_key = Option.value (text 1) ~default:"";
    delivery_id = text 2;
    repo_full_name = Option.value (text 3) ~default:"";
    is_pr = Option.value (int_opt 4) ~default:0 <> 0;
    issue_number = Option.value (int_opt 5) ~default:0;
    requester = Option.value (text 6) ~default:"";
    trigger = Option.value (text 7) ~default:"slash_command";
    runner_pref = text 8;
    host_pref = text 9;
    prompt = Option.value (text 10) ~default:"";
    preamble = Option.value (text 11) ~default:"";
    policy_ref = text 12;
    status =
      Option.value (Option.bind (text 13) status_of_string) ~default:Failed;
    background_task_id = int_opt 14;
    result_kind = Option.bind (text 15) result_kind_of_string;
    result_summary = text 16;
    published_comment_id = int_opt 17;
    publication_status = text 18;
    attempt_count = Option.value (int_opt 19) ~default:0;
    created_at = Option.value (text 20) ~default:"";
    started_at = text 21;
    finished_at = text 22;
    ack_comment_id = int_opt 23;
    publication_branch = text 24;
    published_pr_number = int_opt 25;
    actor_snapshot_json = parse_actor_snapshot_col (text 26);
  }

let select_columns =
  "id, dedup_key, delivery_id, repo_full_name, is_pr, issue_number, requester, \
   trigger, runner_pref, host_pref, prompt, preamble, policy_ref, status, \
   background_task_id, result_kind, result_summary, published_comment_id, \
   publication_status, attempt_count, created_at, started_at, finished_at, \
   ack_comment_id, publication_branch, published_pr_number, \
   actor_snapshot_json"

(** {1 Creation (idempotent)} *)

(* Stable idempotency key. The webhook delivery id is unique per delivery
   but a *redelivered* event gets a new delivery id, so the content key
   (repo + thread + comment) is the primary dedup identity; the delivery id
   is kept for audit. *)
let dedup_key_for ~repo_full_name ~issue_number ~comment_id ~delivery_id =
  match comment_id with
  | Some cid ->
      Printf.sprintf "%s#%d:comment:%d" repo_full_name issue_number cid
  | None -> (
      match delivery_id with
      | Some d when String.trim d <> "" ->
          Printf.sprintf "%s#%d:delivery:%s" repo_full_name issue_number d
      | _ -> Printf.sprintf "%s#%d:unkeyed" repo_full_name issue_number)

type create_outcome = Created of t | Duplicate of t

(** Insert a new work item, or return the existing one when the dedup key is
    already present (at-least-once webhook delivery). *)
let create_if_new ~db ~dedup_key ?delivery_id ~repo_full_name ?(is_pr = false)
    ~issue_number ~requester ?(trigger = "slash_command") ?runner_pref
    ?host_pref ~prompt ?(preamble = "") ?policy_ref ?actor_snapshot () :
    (create_outcome, string) result =
  if String.trim requester = "" then
    Error
      "Work item requires a requester (GitHub login). Refusing an anonymous \
       trigger."
  else if String.trim repo_full_name = "" || issue_number <= 0 then
    Error
      (Printf.sprintf
         "Work item requires a repository and positive issue number (got %S \
          #%d)."
         repo_full_name issue_number)
  else
    let actor_snap_encoded =
      match actor_snapshot with
      | None -> Ok None
      | Some snap -> (
          match
            Github_durable_job_actor_attribution.snapshot_to_storage_json snap
          with
          | Error e -> Error e
          | Ok j -> Ok (Some (j, Yojson.Safe.to_string j)))
    in
    match actor_snap_encoded with
    | Error e -> Error e
    | Ok actor_snap_pair -> begin
        let sql =
          "INSERT OR IGNORE INTO github_work_items (dedup_key, delivery_id, \
           repo_full_name, is_pr, issue_number, requester, trigger, \
           runner_pref, host_pref, prompt, preamble, policy_ref, \
           actor_snapshot_json) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        in
        let stmt = Sqlite3.prepare db sql in
        let inserted =
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
            (fun () ->
              let bind_text i v =
                ignore (Sqlite3.bind stmt i (Sqlite3.Data.TEXT v))
              in
              let bind_opt i = function
                | Some v when String.trim v <> "" -> bind_text i v
                | _ -> ignore (Sqlite3.bind stmt i Sqlite3.Data.NULL)
              in
              bind_text 1 dedup_key;
              bind_opt 2 delivery_id;
              bind_text 3 repo_full_name;
              ignore
                (Sqlite3.bind stmt 4
                   (Sqlite3.Data.INT (if is_pr then 1L else 0L)));
              ignore
                (Sqlite3.bind stmt 5
                   (Sqlite3.Data.INT (Int64.of_int issue_number)));
              bind_text 6 requester;
              bind_text 7 trigger;
              bind_opt 8 runner_pref;
              bind_opt 9 host_pref;
              bind_text 10 prompt;
              bind_text 11 preamble;
              bind_opt 12 policy_ref;
              (match actor_snap_pair with
              | None -> ignore (Sqlite3.bind stmt 13 Sqlite3.Data.NULL)
              | Some (_j, s) -> bind_text 13 s);
              ignore (Sqlite3.step stmt);
              Sqlite3.changes db > 0)
        in
        let fetch () =
          let sql =
            Printf.sprintf
              "SELECT %s FROM github_work_items WHERE dedup_key = ?"
              select_columns
          in
          let stmt = Sqlite3.prepare db sql in
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
            (fun () ->
              ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT dedup_key));
              match Sqlite3.step stmt with
              | Sqlite3.Rc.ROW -> Some (of_stmt stmt)
              | _ -> None)
        in
        match fetch () with
        | Some item when inserted -> Ok (Created item)
        | Some item -> (
            (* Duplicate: preserve initiating snapshot; reject borrow. *)
            match (item.actor_snapshot_json, actor_snap_pair) with
            | _, None -> Ok (Duplicate item)
            | None, Some _ ->
                (* First-wins for already-queued unattributed items. *)
                Ok (Duplicate item)
            | Some existing_j, Some (offered_j, _) -> (
                match
                  ( Github_durable_job_actor_attribution.snapshot_of_storage_json
                      existing_j,
                    Github_durable_job_actor_attribution
                    .snapshot_of_storage_json offered_j )
                with
                | Error e, _ | _, Error e -> Error e
                | Ok existing_s, Ok offered_s -> (
                    match
                      Github_durable_job_actor_attribution
                      .reject_conflicting_snapshot ~existing:existing_s
                        ~offered:offered_s
                    with
                    | Ok () -> Ok (Duplicate item)
                    | Error e -> Error e)))
        | None ->
            Error
              (Printf.sprintf
                 "Failed to record work item %s: row not found after insert. \
                  Check the database is writable and retry."
                 dedup_key)
      end

let snapshot_of_item (item : t) : (Actor_snapshot.t option, string) result =
  match item.actor_snapshot_json with
  | None -> Ok None
  | Some j -> (
      match Github_durable_job_actor_attribution.snapshot_of_storage_json j with
      | Ok s -> Ok (Some s)
      | Error e ->
          Error
            (Printf.sprintf "malformed actor_snapshot on work item %d: %s"
               item.id e))

(** {1 Lookup} *)

let get ~db ~id : t option =
  let sql =
    Printf.sprintf "SELECT %s FROM github_work_items WHERE id = ?"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (of_stmt stmt)
      | _ -> None)

(** Write-once pin of initiating Actor_snapshot. Succeeds when the column is
    empty or already holds the same initiating lineage. Never overwrites a
    different initiator. *)
let pin_actor_snapshot ~db ~id ~(snapshot : Actor_snapshot.t) :
    (t, string) result =
  init_schema db;
  match get ~db ~id with
  | None -> Error (Printf.sprintf "work item %d not found" id)
  | Some item -> (
      match
        Github_durable_job_actor_attribution.snapshot_to_storage_json snapshot
      with
      | Error e -> Error e
      | Ok offered_j -> (
          match item.actor_snapshot_json with
          | Some existing_j -> (
              match
                ( Github_durable_job_actor_attribution.snapshot_of_storage_json
                    existing_j,
                  Github_durable_job_actor_attribution.snapshot_of_storage_json
                    offered_j )
              with
              | Error e, _ | _, Error e -> Error e
              | Ok existing_s, Ok offered_s -> (
                  match
                    Github_durable_job_actor_attribution
                    .reject_conflicting_snapshot ~existing:existing_s
                      ~offered:offered_s
                  with
                  | Ok () -> Ok item
                  | Error e -> Error e))
          | None -> (
              let sql =
                "UPDATE github_work_items SET actor_snapshot_json = ? WHERE id \
                 = ? AND actor_snapshot_json IS NULL"
              in
              let stmt = Sqlite3.prepare db sql in
              ignore
                (Sqlite3.bind stmt 1
                   (Sqlite3.Data.TEXT (Yojson.Safe.to_string offered_j)));
              ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int id)));
              let rc = Sqlite3.step stmt in
              ignore (Sqlite3.finalize stmt);
              match rc with
              | Sqlite3.Rc.DONE -> (
                  match get ~db ~id with
                  | Some item -> Ok item
                  | None ->
                      Error
                        (Printf.sprintf
                           "work item %d missing after actor_snapshot pin" id))
              | rc ->
                  Error
                    (Printf.sprintf "pin_actor_snapshot failed: %s (%s)"
                       (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))))

let find_by_task ~db ~background_task_id : t option =
  let sql =
    Printf.sprintf
      "SELECT %s FROM github_work_items WHERE background_task_id = ? ORDER BY \
       id DESC LIMIT 1"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.INT (Int64.of_int background_task_id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (of_stmt stmt)
      | _ -> None)

let list ~db ?status () : t list =
  let where, bind =
    match status with
    | Some s -> (" WHERE status = ?", Some (string_of_status s))
    | None -> ("", None)
  in
  let sql =
    Printf.sprintf "SELECT %s FROM github_work_items%s ORDER BY id DESC"
      select_columns where
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      (match bind with
      | Some v -> ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT v))
      | None -> ());
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        rows := of_stmt stmt :: !rows
      done;
      List.rev !rows)

(** {1 Lifecycle updates} *)

let exec_update ~db sql binds =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      List.iteri (fun i data -> ignore (Sqlite3.bind stmt (i + 1) data)) binds;
      ignore (Sqlite3.step stmt);
      Sqlite3.changes db > 0)

(* PR publication is idempotent like comment publication: first PR wins. *)
let record_pr_publication ~db ~id ~branch ~pr_number ~publication_status =
  exec_update ~db
    "UPDATE github_work_items SET publication_branch = ?, published_pr_number \
     = COALESCE(?, published_pr_number), publication_status = ? WHERE id = ? \
     AND published_pr_number IS NULL"
    [
      Sqlite3.Data.TEXT branch;
      (match pr_number with
      | Some n -> Sqlite3.Data.INT (Int64.of_int n)
      | None -> Sqlite3.Data.NULL);
      Sqlite3.Data.TEXT publication_status;
      Sqlite3.Data.INT (Int64.of_int id);
    ]

let already_published_pr (item : t) = Option.is_some item.published_pr_number

let set_ack_comment ~db ~id ~comment_id =
  ignore
    (exec_update ~db
       "UPDATE github_work_items SET ack_comment_id = ? WHERE id = ?"
       [
         Sqlite3.Data.INT (Int64.of_int comment_id);
         Sqlite3.Data.INT (Int64.of_int id);
       ])

let attach_task ~db ~id ~background_task_id =
  ignore
    (exec_update ~db
       "UPDATE github_work_items SET background_task_id = ?, attempt_count = \
        attempt_count + 1 WHERE id = ?"
       [
         Sqlite3.Data.INT (Int64.of_int background_task_id);
         Sqlite3.Data.INT (Int64.of_int id);
       ])

let set_status ~db ~id ~(status : status) =
  let timestamps =
    match status with
    | Running -> ", started_at = COALESCE(started_at, datetime('now'))"
    | Succeeded | Failed | Cancelled -> ", finished_at = datetime('now')"
    | Queued | Blocked -> ""
  in
  ignore
    (exec_update ~db
       (Printf.sprintf "UPDATE github_work_items SET status = ?%s WHERE id = ?"
          timestamps)
       [
         Sqlite3.Data.TEXT (string_of_status status);
         Sqlite3.Data.INT (Int64.of_int id);
       ])

let record_result ~db ~id ~(status : status) ~(result_kind : result_kind)
    ~result_summary =
  ignore
    (exec_update ~db
       "UPDATE github_work_items SET status = ?, result_kind = ?, \
        result_summary = ?, finished_at = datetime('now') WHERE id = ?"
       [
         Sqlite3.Data.TEXT (string_of_status status);
         Sqlite3.Data.TEXT (string_of_result_kind result_kind);
         Sqlite3.Data.TEXT result_summary;
         Sqlite3.Data.INT (Int64.of_int id);
       ])

(* Publication is idempotent: the first writer wins; a retry that finds a
   comment id already recorded must skip the GitHub mutation entirely. *)
let record_publication ~db ~id ~comment_id ~publication_status =
  exec_update ~db
    "UPDATE github_work_items SET published_comment_id = COALESCE(?, \
     published_comment_id), publication_status = ? WHERE id = ? AND \
     published_comment_id IS NULL"
    [
      (match comment_id with
      | Some cid -> Sqlite3.Data.INT (Int64.of_int cid)
      | None -> Sqlite3.Data.NULL);
      Sqlite3.Data.TEXT publication_status;
      Sqlite3.Data.INT (Int64.of_int id);
    ]

let already_published (item : t) = Option.is_some item.published_comment_id

(** {1 Restart recovery}

    Re-align work-item status with the owning background task after a daemon
    restart. Returns items whose terminal result still needs publication. *)
let sync_from_task ~db (item : t)
    ~(task_status : Background_task_0_format.status)
    ~(task_result : string option) : t option =
  let map_terminal status result_kind =
    let summary = Option.value task_result ~default:"" in
    record_result ~db ~id:item.id ~status ~result_kind ~result_summary:summary;
    get ~db ~id:item.id
  in
  match task_status with
  | Background_task_0_format.Succeeded when not (is_terminal_status item.status)
    ->
      map_terminal Succeeded Reply
  | (Background_task_0_format.Failed | Background_task_0_format.DirtyWorktree)
    when not (is_terminal_status item.status) ->
      map_terminal Failed Result_failed
  | Background_task_0_format.Cancelled when not (is_terminal_status item.status)
    ->
      map_terminal Cancelled Result_failed
  | Background_task_0_format.Running ->
      if item.status <> Running then set_status ~db ~id:item.id ~status:Running;
      None
  | Background_task_0_format.Queued ->
      if item.status <> Queued then set_status ~db ~id:item.id ~status:Queued;
      None
  | _ -> None

(** {1 /clawq option parsing}

    Leading [key=value] tokens on the /clawq command select work-item execution:
    "/clawq runner=codex host=herdr summarize this issue". Unknown keys are left
    in the request text untouched. Bare /clawq keeps the legacy inline-session
    path. *)

type command_options = {
  runner_opt : string option;
  host_opt : string option;
  request : string;
}

let known_option_key key = key = "runner" || key = "host"

let parse_command_options text : command_options =
  let rec consume runner_opt host_opt = function
    | [] -> (runner_opt, host_opt, [])
    | token :: rest as all -> (
        match String.index_opt token '=' with
        | Some i when i > 0 ->
            let key =
              String.lowercase_ascii (String.trim (String.sub token 0 i))
            in
            let value =
              String.trim
                (String.sub token (i + 1) (String.length token - i - 1))
            in
            if not (known_option_key key) then (runner_opt, host_opt, all)
            else if key = "runner" then
              consume
                (if value = "" then runner_opt else Some value)
                host_opt rest
            else
              consume runner_opt
                (if value = "" then host_opt else Some value)
                rest
        | _ -> (runner_opt, host_opt, all))
  in
  match String.split_on_char '\n' text with
  | [] -> { runner_opt = None; host_opt = None; request = text }
  | first :: more ->
      let tokens =
        String.split_on_char ' ' first |> List.filter (fun t -> t <> "")
      in
      let runner_opt, host_opt, remaining = consume None None tokens in
      let first_rest = String.concat " " remaining in
      let request = String.concat "\n" (first_rest :: more) |> String.trim in
      { runner_opt; host_opt; request }

let wants_work_item options =
  Option.is_some options.runner_opt || Option.is_some options.host_opt

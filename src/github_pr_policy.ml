(** Policy gates for GitHub PR notification dispatch.

    Controls delivery of PR subscription notifications through deduplication
    with cooldown, quiet-hour blocking, and per-room rate limiting. All denials
    are logged with reasons for inspection.

    This module lives in [clawq_runtime_core] and operates on plain string
    parameters rather than [Github_webhook] types, avoiding a cross-library
    dependency. *)

(** {1 Types} *)

type deny_reason =
  | Duplicate  (** Same event already delivered within cooldown window *)
  | Quiet_hours  (** Current hour falls within quiet-hour range *)
  | Rate_limited  (** Per-room hourly delivery limit exceeded *)

type decision = Allowed | Denied of deny_reason

let reason_to_string = function
  | Duplicate ->
      "notification suppressed: duplicate event within cooldown window."
  | Quiet_hours ->
      "notification suppressed: current hour falls within quiet hours."
  | Rate_limited ->
      "notification suppressed: per-room hourly delivery limit exceeded."

(** {1 Quiet-hour helpers}

    These mirror [Ambient_policy.is_in_quiet_hours] but live here to avoid a
    cross-library dependency. *)

let default_quiet_start = 23
let default_quiet_end = 8

let is_in_quiet_hours ~hour ~quiet_start ~quiet_end =
  if quiet_start > quiet_end then hour >= quiet_start || hour < quiet_end
  else hour >= quiet_start && hour < quiet_end

let check_quiet_hours ~hour ~quiet_start ~quiet_end =
  if quiet_start = quiet_end then Allowed
  else if is_in_quiet_hours ~hour ~quiet_start ~quiet_end then
    Denied Quiet_hours
  else Allowed

(** {1 Schema} *)

let exec_exn db sql =
  Sql_util.exec_exn ~label:"github_pr_policy schema error" db sql

(** Create the persistent dedup table. Idempotent via IF NOT EXISTS. *)
let init_schema db =
  exec_exn db
    "CREATE TABLE IF NOT EXISTS github_notification_dedupe (\n\
    \     id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \     dedup_key TEXT NOT NULL,\n\
    \     room_id TEXT NOT NULL,\n\
    \     repo TEXT NOT NULL,\n\
    \     pr_number INTEGER NOT NULL,\n\
    \     event_type TEXT NOT NULL,\n\
    \     sent_at TEXT NOT NULL DEFAULT (datetime('now'))\n\
    \   )";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_gh_notif_dedupe_key_room_time ON \
     github_notification_dedupe (dedup_key, room_id, sent_at)";
  exec_exn db
    "CREATE INDEX IF NOT EXISTS idx_gh_notif_dedupe_room_time ON \
     github_notification_dedupe (room_id, sent_at)"

(** {1 Dedup} *)

(** Build a dedup key from event context. CI events for the same PR, check name,
    and conclusion are coalesced; non-CI events use [delivery_id] for exact
    dedup.

    @param ci_name check/workflow name (empty for non-CI events)
    @param ci_conclusion conclusion or status string
    @param is_ci whether this is a CI event (check_run/check_suite/workflow_run)
*)
let make_dedup_key ~repo ~pr_number ~ci_name ~ci_conclusion ~is_ci ~delivery_id
    =
  if is_ci then
    Printf.sprintf "ci:%s:%d:%s:%s" repo pr_number ci_name ci_conclusion
  else Printf.sprintf "evt:%s" delivery_id

(** Check if a dedup key was already seen within the cooldown window. Returns
    [true] if this is a duplicate (should suppress). *)
let is_duplicate ~db ~dedup_key ~room_id ~cooldown_seconds =
  let sql =
    "SELECT 1 FROM github_notification_dedupe WHERE dedup_key = ? AND room_id \
     = ? AND sent_at > datetime('now', ?) LIMIT 1"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let offset = Printf.sprintf "-%d seconds" cooldown_seconds in
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT dedup_key) : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT room_id) : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT offset) : Sqlite3.Rc.t);
      match Sqlite3.step stmt with Sqlite3.Rc.ROW -> true | _ -> false)

(** Record a successful delivery for dedup and rate-limit tracking. *)
let record_delivery ~db ~dedup_key ~room_id ~repo ~pr_number ~event_type =
  let sql =
    "INSERT INTO github_notification_dedupe (dedup_key, room_id, repo, \
     pr_number, event_type) VALUES (?, ?, ?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT dedup_key) : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT room_id) : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT repo) : Sqlite3.Rc.t);
      ignore
        (Sqlite3.bind stmt 4 (Sqlite3.Data.INT (Int64.of_int pr_number))
          : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT event_type) : Sqlite3.Rc.t);
      ignore (Sqlite3.step stmt : Sqlite3.Rc.t))

(** Purge old dedup entries beyond retention period. Returns number of rows
    deleted. *)
let purge_old_entries ~db ~retention_seconds =
  let sql =
    "DELETE FROM github_notification_dedupe WHERE sent_at < datetime('now', ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let offset = Printf.sprintf "-%d seconds" retention_seconds in
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT offset) : Sqlite3.Rc.t);
      ignore (Sqlite3.step stmt : Sqlite3.Rc.t);
      Sqlite3.changes db)

(** {1 Rate limiting} *)

(** Count deliveries to a room within the last hour. *)
let count_deliveries_this_hour ~db ~room_id =
  let sql =
    "SELECT COUNT(*) FROM github_notification_dedupe WHERE room_id = ? AND \
     sent_at > datetime('now', '-1 hour')"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id) : Sqlite3.Rc.t);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0)
      | _ -> 0)

(** {1 Policy check} *)

(** [decide ~db ~dedup_key ~room_id ~hour ?quiet_start ?quiet_end ?max_per_hour
     ?dedupe_seconds ()]

    Runs all policy gates in order: dedup, quiet hours, rate limit. Returns
    [Allowed] or [Denied reason] without recording a delivery. Call
    {!record_delivery} only after the notification send succeeds. *)
let decide ~(db : Sqlite3.db) ~(dedup_key : string) ~(room_id : string)
    ~(hour : int) ?(quiet_start = default_quiet_start)
    ?(quiet_end = default_quiet_end) ?(max_per_hour = 0) ?(dedupe_seconds = 60)
    () =
  (* Gate 1: Dedup with cooldown *)
  if is_duplicate ~db ~dedup_key ~room_id ~cooldown_seconds:dedupe_seconds then
    Denied Duplicate
  else
    (* Gate 2: Quiet hours *)
    match check_quiet_hours ~hour ~quiet_start ~quiet_end with
    | Denied _ as d -> d
    | Allowed ->
        if
          (* Gate 3: Rate limit *)
          max_per_hour > 0
        then
          let count = count_deliveries_this_hour ~db ~room_id in
          if count >= max_per_hour then Denied Rate_limited else Allowed
        else Allowed

(** [check ~db ~dedup_key ~event_type ~room_id ~repo ~pr_number ~hour
     ?quiet_start ?quiet_end ?max_per_hour ?dedupe_seconds ()]

    Runs policy gates and records a successful delivery when allowed. Dispatch
    paths that can fail after policy approval should use {!decide} and record
    only after the send succeeds.

    Callers should build [dedup_key] via {!make_dedup_key} from event fields. *)
let check ~(db : Sqlite3.db) ~(dedup_key : string) ~(event_type : string)
    ~(room_id : string) ~(repo : string) ~(pr_number : int) ~(hour : int)
    ?(quiet_start = default_quiet_start) ?(quiet_end = default_quiet_end)
    ?(max_per_hour = 0) ?(dedupe_seconds = 60) () =
  match
    decide ~db ~dedup_key ~room_id ~hour ~quiet_start ~quiet_end ~max_per_hour
      ~dedupe_seconds ()
  with
  | Denied _ as d -> d
  | Allowed ->
      record_delivery ~db ~dedup_key ~room_id ~repo ~pr_number ~event_type;
      Allowed

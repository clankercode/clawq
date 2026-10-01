(** Shared-database retention sweeps and nightly VACUUM (B830).

    The daemon's high-write tables were append-only: disk grew ~33% per two
    weeks with no purge and no VACUUM. This module centralizes the hourly
    retention sweep and the size-gated nightly VACUUM; per-table windows come
    from the [db_retention] config block (0 disables that table's purge). *)

let src = Logs.Src.create "clawq.db_maintenance" ~doc:"DB retention + vacuum"

module Log = (val Logs.src_log src : Logs.LOG)

(* ── Hourly retention sweep ────────────────────────────────────────────── *)

let log_purged label days count =
  (* Only log ticks that actually deleted rows, so an idle daemon stays
     quiet instead of emitting seven zero-row INFO lines every hour. *)
  if count > 0 then
    Log.info (fun m ->
        m "db retention: %s purged %d row(s) older than %d day(s)" label count
          days)

let sweep_result label days (thunk : int -> (int, string) result) =
  if days > 0 then
    match thunk days with
    | Ok count -> log_purged label days count
    | Error err ->
        Log.warn (fun m -> m "db retention: %s purge failed: %s" label err)

let sweep_exn label days (thunk : int -> int) =
  if days > 0 then
    try log_purged label days (thunk days)
    with exn ->
      Log.warn (fun m ->
          m "db retention: %s purge failed: %s" label (Printexc.to_string exn))

let retention_tick ~db ~(config : Runtime_config.t)
    ?(now = Unix.gettimeofday ()) () =
  let r = config.db_retention in
  sweep_result "github_room_event_journal" r.github_room_event_journal_days
    (fun days -> Github_room_event_journal.purge_older_than ~db ~days ~now ());
  sweep_result "github_app_webhook_deliveries"
    r.github_app_webhook_deliveries_days (fun days ->
      Github_app_webhook_ingress.purge_older_than ~db ~days ~now ());
  sweep_result "request_stats" r.request_stats_days (fun days ->
      Request_stats.purge_older_than ~db ~days ~now ());
  (* egress_audit keeps its existing purge (delete_before); only the cutoff
     computation is new. *)
  sweep_exn "egress_audit" r.egress_audit_days (fun days ->
      Egress_audit.purge_older_than ~db ~days ~now ());
  sweep_result "github_action_correlations" r.github_action_correlations_days
    (fun days ->
      Github_action_reconcile.purge_closed_older_than ~db ~days ~now ());
  sweep_result "github_delivery_outbox" r.github_delivery_outbox_days
    (fun days ->
      Github_delivery_outbox.purge_terminal_older_than ~db ~days ~now ());
  sweep_exn "room_activity_ledger" r.room_activity_ledger_days (fun days ->
      Room_activity_ledger.purge_older_than ~db ~days ~now ())

(* ── Nightly VACUUM ────────────────────────────────────────────────────── *)

(* The cron scheduler has no internal-task seam (cron_jobs only carry user
   agent turns), so the vacuum rides the hourly maintenance tick with three
   gates: at most one execution per 24h, only inside the local low-traffic
   window (default 02:00-05:00), and only when the db file exceeds
   vacuum_min_mb. *)

let vacuum_interval_s = 86_400.0
let default_vacuum_window_hours = (2, 5)

(* VACUUM rewrites the whole database; give its dedicated connection a far
   more generous busy timeout than the shared 5s default. *)
let vacuum_busy_timeout_ms = 60_000

type vacuum_state = { mutable last_vacuum_at : float }

let create_vacuum_state ?(last_vacuum_at = 0.0) () = { last_vacuum_at }

type vacuum_outcome =
  | Vacuum_disabled
  | Vacuum_skipped_no_file
  | Vacuum_skipped_small of int
  | Vacuum_skipped_window of int
  | Vacuum_skipped_recent
  | Vacuum_failed of string
  | Vacuumed of { before_bytes : int; after_bytes : int; duration_s : float }

let in_window ~window hour =
  let lo, hi = window in
  if lo <= hi then hour >= lo && hour < hi else hour >= lo || hour < hi

let file_size_bytes path =
  try Some (Unix.stat path).Unix.st_size with _ -> None

let min_file_bytes min_mb = max 0 min_mb * 1024 * 1024
let format_mb bytes = Printf.sprintf "%.1fMB" (float_of_int bytes /. 1_048_576.)

(* sqlite3-ocaml has no db_filename accessor; PRAGMA database_list exposes the
   main database's file path. In-memory databases report an empty path. *)
let main_db_path db =
  let stmt = Sqlite3.prepare db "PRAGMA database_list" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            if Sql_util.text_column stmt 1 = "main" then
              Some (Sql_util.text_column stmt 2)
            else loop ()
        | _ -> None
      in
      loop ())

(* Runs VACUUM on a dedicated connection and returns (before, after,
   duration). WAL serializes this with other writers, so no extra gating on
   concurrent checkpoint activity is needed. *)
let run_vacuum ~db_path () : (int * int * float, string) result =
  let before = file_size_bytes db_path in
  let started = Unix.gettimeofday () in
  let vac = Sql_util.open_db ~ms:vacuum_busy_timeout_ms db_path in
  let outcome =
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.db_close vac))
      (fun () ->
        match Sqlite3.exec vac "VACUUM" with
        | Sqlite3.Rc.OK ->
            (* In WAL mode the rewritten pages land in the -wal file; truncate
               it so the main file's size (and disk usage) actually drops. *)
            ignore (Sqlite3.exec vac "PRAGMA wal_checkpoint(TRUNCATE)");
            let duration = Unix.gettimeofday () -. started in
            Ok
              ( Option.value before ~default:0,
                Option.value (file_size_bytes db_path) ~default:0,
                duration )
        | rc ->
            Error
              (Printf.sprintf "VACUUM failed: %s (%s)" (Sqlite3.Rc.to_string rc)
                 (Sqlite3.errmsg vac)))
  in
  outcome

let maybe_vacuum ~db ~(config : Runtime_config.t) ~state
    ?(now = Unix.gettimeofday ()) ?(window = default_vacuum_window_hours)
    ?(localtime = Unix.localtime) () =
  let cfg = config.db_retention in
  if not cfg.vacuum_enabled then Vacuum_disabled
  else
    match main_db_path db with
    | None | Some "" -> Vacuum_skipped_no_file
    | Some db_path -> (
        match file_size_bytes db_path with
        | None -> Vacuum_skipped_no_file
        | Some bytes when bytes < min_file_bytes cfg.vacuum_min_mb ->
            Vacuum_skipped_small bytes
        | Some _ -> (
            let local_hour = (localtime now).Unix.tm_hour in
            if not (in_window ~window local_hour) then
              Vacuum_skipped_window local_hour
            else if now -. state.last_vacuum_at < vacuum_interval_s then
              Vacuum_skipped_recent
            else
              match run_vacuum ~db_path () with
              | Ok (before, after, duration) ->
                  state.last_vacuum_at <- now;
                  Log.info (fun m ->
                      m "db vacuum: %s -> %s in %.1fs (%s)" (format_mb before)
                        (format_mb after) duration db_path);
                  Vacuumed
                    {
                      before_bytes = before;
                      after_bytes = after;
                      duration_s = duration;
                    }
              | Error err ->
                  (* Only successful runs set last_vacuum_at, so a BUSY failure
                     retries on the next hourly tick. *)
                  Log.warn (fun m ->
                      m "db vacuum failed for %s: %s" db_path err);
                  Vacuum_failed err))

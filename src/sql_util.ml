(* Shared SQLite plumbing: connection setup, statement execution, parameter
   binding, column accessors, and a row-query combinator. Depends only on
   [Sqlite3] (plus [Unix] for retry sleeps).

   Two column-accessor conventions coexist in the codebase and are both
   preserved here:
   - option-returning [sql_text]/[sql_int] (operate on a [Sqlite3.Data.t]) plus
     [sql_bool] (operates on stmt+index, defaults to [false]);
   - defaulted [text_column]/[int_column] and optional
     [opt_text_column]/[opt_int_column] (operate on stmt+index). *)

(* ── Connection setup and BUSY resilience ──────────────────────────────── *)

(** Default busy timeout for clawq-owned SQLite connections. The daemon, CLI
    commands, and the EC process all open the shared database, so every
    connection must tolerate concurrent writers instead of failing at the first
    [BUSY]. *)
let busy_timeout_ms = 5_000

let set_busy_timeout ?(ms = busy_timeout_ms) db =
  ignore (Sqlite3.busy_timeout db ms)

(** [open_db db_path] opens a connection with the shared busy timeout. All clawq
    database opens go through here so no connection ships without it. *)
let open_db ?(ms = busy_timeout_ms) db_path =
  let db = Sqlite3.db_open db_path in
  set_busy_timeout ~ms db;
  db

(** [pragma_text db sql] returns the first TEXT column of a single-row pragma
    query, or [None] when the query yields no row or fails (e.g. it returned
    [BUSY], or the statement could not be prepared while the schema was locked).
    Never raises, so pragma probes stay best-effort. *)
let pragma_text db sql =
  let probe () =
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match Sqlite3.column stmt 0 with
            | Sqlite3.Data.TEXT s -> Some s
            | _ -> None)
        | _ -> None)
  in
  match probe () with
  | Some text -> Some text
  | None -> None
  | exception Sqlite3.SqliteError _ -> None

let journal_mode db =
  match pragma_text db "PRAGMA journal_mode" with
  | Some mode -> mode
  | None -> ""

(** [enable_wal db] switches the database to WAL journal mode and returns the
    resulting mode. WAL is a persistent property of the file, so one successful
    call converts the database for every later connection. Any answer sqlite
    returns is final — in-memory databases stay in ["memory"] and an already
    converted file answers ["wal"] — so only a failed probe (the file was
    locked) is retried. Best effort: if the switch never lands, the current mode
    is returned and the caller keeps whatever it got. *)
let enable_wal ?(attempts = 3) ?(sleep = Unix.sleepf) db =
  let rec go n =
    match pragma_text db "PRAGMA journal_mode = WAL" with
    | Some mode -> mode
    | None when n <= 1 -> journal_mode db
    | None ->
        sleep 0.05;
        go (n - 1)
  in
  go attempts

let contains_substring hay needle =
  let hay_len = String.length hay and needle_len = String.length needle in
  needle_len = 0
  ||
  let rec loop i =
    i + needle_len <= hay_len
    && (String.sub hay i needle_len = needle || loop (i + 1))
  in
  loop 0

(** [Rc.BUSY] / [LOCKED] reach OCaml as [Failure] strings built from
    [Rc.to_string], and as [Sqlite3.Error] from the raising helpers. Both forms
    carry "BUSY"/"LOCKED"; sqlite3's own message is "database is locked". *)
let is_busy_message msg =
  contains_substring msg "BUSY"
  || contains_substring msg "LOCKED"
  || contains_substring msg "database is locked"

let is_busy_error = function
  | Failure msg -> is_busy_message msg
  | Sqlite3.Error msg -> is_busy_message msg
  | _ -> false

let default_busy_attempts = 4
let default_busy_base_delay_s = 0.05
let default_busy_max_delay_s = 0.5

(** [retry_busy f] runs [f] again with exponential backoff while it fails with a
    transient lock error, and re-raises the last exception once [~attempts]
    executions are exhausted. Non-BUSY errors are never retried. [~sleep] and
    [~on_retry] exist so tests can observe the backoff without waiting. *)
let retry_busy ?attempts ?(base_delay_s = default_busy_base_delay_s)
    ?(max_delay_s = default_busy_max_delay_s) ?(sleep = Unix.sleepf) ?on_retry f
    =
  let attempts = Option.value attempts ~default:default_busy_attempts in
  let rec go attempt =
    match f () with
    | result -> result
    | exception exn ->
        if is_busy_error exn && attempt + 1 < attempts then begin
          let delay =
            Float.min max_delay_s (base_delay_s *. (2.0 ** Float.of_int attempt))
          in
          (match on_retry with
          | Some notify -> notify (attempt + 1) delay
          | None -> ());
          sleep delay;
          go (attempt + 1)
        end
        else raise exn
  in
  go 0

(* ── Statement execution ────────────────────────────────────────────────── *)

(** [exec_exn ?label db sql] runs [sql] via [Sqlite3.exec], raising [Failure] on
    any non-OK result. [label] parameterizes the error prefix so migrated call
    sites keep their exact original message (default ["SQLite error"]). *)
let exec_exn ?(label = "SQLite error") db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "%s: %s (sql: %s)" label (Sqlite3.Rc.to_string rc) sql)

(** [exec_schema ?label db sql] is [exec_exn] with retry-on-BUSY. Meant for
    idempotent schema statements ([CREATE ... IF NOT EXISTS]): a concurrent
    writer holding the database briefly must not fail schema init. *)
let exec_schema ?label db sql = retry_busy (fun () -> exec_exn ?label db sql)

(** [exec_with_params ?label db sql params] prepares [sql], binds [params]
    positionally, steps once expecting [DONE], and finalizes. *)
let exec_with_params ?(label = "SQLite error") db sql params =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      List.iteri
        (fun i p -> ignore (Sqlite3.bind stmt (i + 1) p : Sqlite3.Rc.t))
        params;
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> ()
      | rc ->
          failwith
            (Printf.sprintf "%s: %s (sql: %s)" label (Sqlite3.Rc.to_string rc)
               sql))

(** Bind a list of values positionally (1-indexed), ignoring bind return codes.
*)
let bind_params stmt params =
  List.iteri
    (fun i value -> ignore (Sqlite3.bind stmt (i + 1) value : Sqlite3.Rc.t))
    params

(* ── Option-returning accessors (operate on a [Sqlite3.Data.t] value) ────── *)

let sql_text = function Sqlite3.Data.TEXT s -> Some s | _ -> None
let sql_int = function Sqlite3.Data.INT i -> Some (Int64.to_int i) | _ -> None

let sql_bool stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Int64.to_int n <> 0
  | _ -> false

(* ── Defaulted accessors (operate on stmt + column index) ────────────────── *)

let text_column stmt idx =
  match Sqlite3.column stmt idx with Sqlite3.Data.TEXT s -> s | _ -> ""

let int_column stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | _ -> 0

let opt_text_column stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.TEXT s -> Some s
  | Sqlite3.Data.NULL -> None
  | _ -> None

let opt_int_column stmt idx =
  match Sqlite3.column stmt idx with
  | Sqlite3.Data.INT n -> Some (Int64.to_int n)
  | Sqlite3.Data.NULL -> None
  | _ -> None

(** [query_rows db sql ~bind ~of_stmt] prepares [sql], runs [bind stmt], then
    accumulates [of_stmt stmt] for each returned row in order, finalizing the
    statement even on exception. Matches the prevalent prepare/bind/step/rev
    loop. Pass [~bind:(fun _ -> ())] for parameterless queries. *)
let query_rows db sql ~bind ~of_stmt =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      bind stmt;
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        rows := of_stmt stmt :: !rows
      done;
      List.rev !rows)

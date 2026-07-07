(* Shared SQLite plumbing: statement execution, parameter binding, column
   accessors, and a row-query combinator. Depends only on [Sqlite3].

   Two column-accessor conventions coexist in the codebase and are both
   preserved here:
   - option-returning [sql_text]/[sql_int] (operate on a [Sqlite3.Data.t]) plus
     [sql_bool] (operates on stmt+index, defaults to [false]);
   - defaulted [text_column]/[int_column] and optional
     [opt_text_column]/[opt_int_column] (operate on stmt+index). *)

(** [exec_exn ?label db sql] runs [sql] via [Sqlite3.exec], raising [Failure] on
    any non-OK result. [label] parameterizes the error prefix so migrated call
    sites keep their exact original message (default ["SQLite error"]). *)
let exec_exn ?(label = "SQLite error") db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "%s: %s (sql: %s)" label (Sqlite3.Rc.to_string rc) sql)

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

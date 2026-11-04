(** SQL plumbing for [Slack_standing_order_store], which includes this module:
    parameter builders, strict reads and transactions that turn database
    failures into [Error]. *)

let label = "slack_standing_orders"
let text s = Sqlite3.Data.TEXT s
let int n = Sqlite3.Data.INT (Int64.of_int n)
let bool b = int (if b then 1 else 0)
let stamp () = Time_util.sql_datetime_utc ()
let exec db sql params = Sql_util.exec_with_params ~label db sql params

let with_stmt db sql params f =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      Sql_util.bind_params stmt params;
      f stmt)

(* Like [exec] but hands back the step result, so callers can tell a
   constraint violation from other failures. *)
let step db sql params = with_stmt db sql params Sqlite3.step

(* Every store read. Unlike [Sql_util.query_rows], which stops quietly at the
   first step that is not ROW, a read that does not end in DONE (BUSY, ERROR,
   IOERR, ...) raises, so a failed read never looks like "no rows". *)
let query db sql params of_stmt =
  with_stmt db sql params (fun stmt ->
      let rec rows acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> rows (of_stmt stmt :: acc)
        | Sqlite3.Rc.DONE -> List.rev acc
        | rc ->
            failwith
              (Printf.sprintf "%s: read failed: %s (%s)" label
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
      in
      rows [])

let first = function x :: _ -> Some x | [] -> None

(* A space-separated column list; decoders index columns in this order. *)
let columns names = String.split_on_char ' ' names

(* For aggregates, which answer exactly one row: any other answer is a failed
   read, never a count of 0. *)
let int_query db sql params =
  match query db sql params (fun stmt -> Sql_util.int_column stmt 0) with
  | [ n ] -> n
  | rows ->
      failwith
        (Printf.sprintf "%s: an aggregate read returned %d rows" label
           (List.length rows))

let opt_float_column stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.FLOAT f -> Some f
  | Sqlite3.Data.INT n -> Some (Int64.to_float n)
  | _ -> None

let is_db_error = function
  | Failure _ | Sqlite3.Error _ | Sqlite3.SqliteError _
  | Sqlite3.InternalError _ | Sqlite3.RangeError _ | Sqlite3.DataTypeError _ ->
      true
  | _ -> false

(* The [_in_tx] functions run inside a caller's transaction, whose handler
   (e.g. [Setup_plan_apply.apply]) rolls back on [Error] but not on an
   exception, so database failures become [Error] here. *)
let guard f =
  try f () with exn when is_db_error exn -> Error (Printexc.to_string exn)

let rollback db = try Sql_util.exec_exn ~label db "ROLLBACK" with _ -> ()

(* BEGIN runs outside the handler: a BEGIN that fails (e.g. inside a caller's
   transaction) must not roll back a transaction someone else holds. *)
let in_tx db ~on_error f =
  match
    Sql_util.retry_busy (fun () ->
        Sql_util.exec_exn ~label db "BEGIN IMMEDIATE")
  with
  | exception exn when is_db_error exn ->
      Error (on_error (Printexc.to_string exn))
  | () -> (
      match f () with
      | Error _ as error ->
          rollback db;
          error
      | Ok _ as ok -> (
          match Sql_util.exec_exn ~label db "COMMIT" with
          | () -> ok
          | exception exn when is_db_error exn ->
              rollback db;
              Error (on_error (Printexc.to_string exn)))
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          rollback db;
          if is_db_error exn then Error (on_error (Printexc.to_string exn))
          else Printexc.raise_with_backtrace exn bt)

type queue_state = Pending | Claimed | Failed

type queue_row = {
  queue_id : int;
  session_key : string;
  source : string;
  state : queue_state;
  payload_json : string;
  attempt_count : int;
  last_error : string option;
  claimed_at : string option;
  created_at : string;
}

type queue_claim_result = Claim_ok of queue_row | Claim_empty

let queue_state_to_string = function
  | Pending -> "pending"
  | Claimed -> "claimed"
  | Failed -> "failed"

let queue_state_of_string = function
  | "pending" -> Pending
  | "claimed" -> Claimed
  | "failed" -> Failed
  | s -> failwith (Printf.sprintf "Unknown queue state: %s" s)

(* ---- Inbound Queue API ---- *)

let queue_row_of_stmt stmt =
  let int_col i =
    match Sqlite3.column stmt i with
    | Sqlite3.Data.INT n -> Int64.to_int n
    | _ -> 0
  in
  let text_col i =
    match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> s | _ -> ""
  in
  let text_opt_col i =
    match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> Some s | _ -> None
  in
  {
    queue_id = int_col 0;
    session_key = text_col 1;
    source = text_col 2;
    state = queue_state_of_string (text_col 3);
    payload_json = text_col 4;
    attempt_count = int_col 5;
    last_error = text_opt_col 6;
    claimed_at = text_opt_col 7;
    created_at = text_col 8;
  }

let queue_enqueue ~db ~session_key ~source ~payload_json =
  let sql =
    "INSERT INTO inbound_queue (session_key, source, payload_json) VALUES (?, \
     ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT source));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT payload_json));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Int64.to_int (Sqlite3.last_insert_rowid db)
      | rc ->
          failwith
            (Printf.sprintf "queue_enqueue failed: %s" (Sqlite3.Rc.to_string rc)))

let queue_claim ~db ~session_key =
  let sql =
    "SELECT id, session_key, source, state, payload_json, attempt_count, \
     last_error, claimed_at, created_at FROM inbound_queue WHERE session_key = \
     ? AND state = 'pending' ORDER BY id ASC LIMIT 1"
  in
  let stmt = Sqlite3.prepare db sql in
  let row =
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> Some (queue_row_of_stmt stmt)
        | _ -> None)
  in
  match row with
  | None -> Claim_empty
  | Some row ->
      let update_sql =
        "UPDATE inbound_queue SET state = 'claimed', claimed_at = \
         datetime('now') WHERE id = ? AND state = 'pending'"
      in
      let update_stmt = Sqlite3.prepare db update_sql in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize update_stmt))
        (fun () ->
          ignore
            (Sqlite3.bind update_stmt 1
               (Sqlite3.Data.INT (Int64.of_int row.queue_id)));
          match Sqlite3.step update_stmt with
          | Sqlite3.Rc.DONE when Sqlite3.changes db > 0 ->
              Claim_ok { row with state = Claimed }
          | _ -> Claim_empty)

let queue_release ~db ~queue_id =
  let sql =
    "UPDATE inbound_queue SET state = 'pending', claimed_at = NULL WHERE id = \
     ? AND state = 'claimed'"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int queue_id)));
      ignore (Sqlite3.step stmt);
      Sqlite3.changes db > 0)

let queue_delete ~db ~queue_id =
  let sql = "DELETE FROM inbound_queue WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int queue_id)));
      ignore (Sqlite3.step stmt);
      Sqlite3.changes db > 0)

let queue_record_failure ~db ~queue_id ~error =
  let sql =
    "UPDATE inbound_queue SET state = 'failed', attempt_count = attempt_count \
     + 1, last_error = ? WHERE id = ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT error));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int queue_id)));
      ignore (Sqlite3.step stmt))

let queue_reclaim_stale ~db ~older_than_seconds =
  let sql =
    "UPDATE inbound_queue SET state = 'pending', claimed_at = NULL WHERE state \
     = 'claimed' AND claimed_at < datetime('now', '-' || ? || ' seconds')"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore
        (Sqlite3.bind stmt 1
           (Sqlite3.Data.INT (Int64.of_int older_than_seconds)));
      ignore (Sqlite3.step stmt);
      Sqlite3.changes db)

let queue_reclaim_failed ~db =
  let sql =
    "UPDATE inbound_queue SET state = 'pending', claimed_at = NULL WHERE state \
     = 'failed'"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.step stmt);
      Sqlite3.changes db)

let queue_count ~db ~session_key =
  let sql =
    "SELECT COUNT(*) FROM inbound_queue WHERE session_key = ? AND state IN \
     ('pending', 'failed')"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0)
      | _ -> 0)

let queue_count_all ~db =
  let sql =
    "SELECT COUNT(*) FROM inbound_queue WHERE state IN ('pending', 'failed')"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0)
      | _ -> 0)

let queue_list ~db ~session_key =
  let sql =
    "SELECT id, session_key, source, state, payload_json, attempt_count, \
     last_error, claimed_at, created_at FROM inbound_queue WHERE session_key = \
     ? ORDER BY id ASC"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        rows := queue_row_of_stmt stmt :: !rows
      done;
      List.rev !rows)

let queue_list_pending_sessions ~db =
  let sql =
    "SELECT session_key FROM inbound_queue WHERE state IN ('pending', \
     'failed') GROUP BY session_key ORDER BY MIN(id)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let keys = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.TEXT s -> keys := s :: !keys
        | _ -> ()
      done;
      List.rev !keys)

let queue_clear ~db ~session_key =
  let sql = "DELETE FROM inbound_queue WHERE session_key = ?" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_key));
      ignore (Sqlite3.step stmt);
      Sqlite3.changes db)

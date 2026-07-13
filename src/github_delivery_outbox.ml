(** 24-hour retrying delivery outbox with per-event dead letters
    (P19.M3.E3.T001). Independent of webhook ACK. *)

type status = Pending | In_flight | Succeeded | Dead_letter | Superseded

type entry = {
  id : string;
  room_id : string;
  item_key : string;
  intent_json : Yojson.Safe.t;
  status : status;
  attempts : int;
  next_attempt_at : string;
  created_at : string;
  last_error : string option;
  dead_lettered_at : string option;
}

let default_max_age_seconds = 86_400.0

let string_of_status = function
  | Pending -> "pending"
  | In_flight -> "in_flight"
  | Succeeded -> "succeeded"
  | Dead_letter -> "dead_letter"
  | Superseded -> "superseded"

let status_of_string = function
  | "pending" -> Ok Pending
  | "in_flight" -> Ok In_flight
  | "succeeded" -> Ok Succeeded
  | "dead_letter" -> Ok Dead_letter
  | "superseded" -> Ok Superseded
  | s -> Error (Printf.sprintf "unknown outbox status: %s" s)

(** Backoff seconds after [attempts] failures: min(3600, 30 * 2^attempts). *)
let backoff_seconds ~attempts =
  let capped_attempts = if attempts < 0 then 0 else min attempts 30 in
  let raw = 30. *. (2. ** float_of_int capped_attempts) in
  min 3600. raw

let redacted_placeholder = "***REDACTED***"
let max_error_len = 512

let looks_like_secret s =
  let sl = String.lowercase_ascii s in
  String_util.contains sl "bearer "
  || String_util.contains sl "ghp_"
  || String_util.contains sl "ghs_"
  || String_util.contains sl "github_pat_"
  || String_util.contains sl "gho_"
  || String_util.contains sl "ghu_"
  || String_util.contains s "BEGIN"
     && (String_util.contains s "PRIVATE KEY"
        || String_util.contains s "-----BEGIN")
  || String_util.contains_ci s "client_secret"
  || String_util.contains_ci s "webhook_secret"
  || String_util.contains_ci s "api_key="
  || String_util.contains_ci s "authorization:"

(** Never persist raw secrets in [last_error]. Token/PEM-shaped text is replaced
    wholesale; other errors are length-bounded only. *)
let redact_error error =
  let s = String.trim error in
  let s = if looks_like_secret s then redacted_placeholder else s in
  let len = String.length s in
  if len <= max_error_len then s
  else
    String.sub s 0 max_error_len
    ^ Printf.sprintf "...<%d more bytes>" (len - max_error_len)

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "github_delivery_outbox schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

let ensure_schema db =
  Sqlite3.busy_timeout db 5_000;
  let table_sql =
    {|CREATE TABLE IF NOT EXISTS github_delivery_outbox (
      id TEXT PRIMARY KEY NOT NULL,
      room_id TEXT NOT NULL,
      item_key TEXT NOT NULL,
      intent_json TEXT NOT NULL,
      status TEXT NOT NULL,
      attempts INTEGER NOT NULL DEFAULT 0,
      next_attempt_at TEXT NOT NULL,
      created_at TEXT NOT NULL,
      last_error TEXT,
      dead_lettered_at TEXT
    )|}
  in
  let idx_due =
    {|CREATE INDEX IF NOT EXISTS idx_github_delivery_outbox_due
      ON github_delivery_outbox(status, next_attempt_at)|}
  in
  let idx_room =
    {|CREATE INDEX IF NOT EXISTS idx_github_delivery_outbox_room
      ON github_delivery_outbox(room_id, item_key)|}
  in
  let idx_dead =
    {|CREATE INDEX IF NOT EXISTS idx_github_delivery_outbox_dead
      ON github_delivery_outbox(status, dead_lettered_at)
      WHERE status = 'dead_letter'|}
  in
  List.iter (exec_schema db) [ table_sql; idx_due; idx_room; idx_dead ]

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | Sqlite3.Data.INT n -> Int64.to_string n
  | _ -> ""

let opt_text_col stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> Some s | _ -> None

let int_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | Sqlite3.Data.TEXT s -> ( try int_of_string s with _ -> 0)
  | _ -> 0

let select_columns =
  {|id, room_id, item_key, intent_json, status, attempts,
    next_attempt_at, created_at, last_error, dead_lettered_at|}

let entry_of_stmt stmt : (entry, string) result =
  let id = text_col stmt 0 in
  let room_id = text_col stmt 1 in
  let item_key = text_col stmt 2 in
  let intent_json_s = text_col stmt 3 in
  let status_s = text_col stmt 4 in
  let attempts = int_col stmt 5 in
  let next_attempt_at = text_col stmt 6 in
  let created_at = text_col stmt 7 in
  let last_error = opt_text_col stmt 8 in
  let dead_lettered_at = opt_text_col stmt 9 in
  match status_of_string status_s with
  | Error e -> Error e
  | Ok status -> (
      try
        let intent_json = Yojson.Safe.from_string intent_json_s in
        Ok
          {
            id;
            room_id;
            item_key;
            intent_json;
            status;
            attempts;
            next_attempt_at;
            created_at;
            last_error;
            dead_lettered_at;
          }
      with exn ->
        Error
          (Printf.sprintf "invalid intent_json for outbox id %s: %s" id
             (Printexc.to_string exn)))

let get_by_id ~db ~id : (entry option, string) result =
  let sql =
    Printf.sprintf
      {|SELECT %s FROM github_delivery_outbox WHERE id = ? LIMIT 1|}
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match entry_of_stmt stmt with Ok e -> Ok (Some e) | Error e -> Error e)
    | Sqlite3.Rc.DONE -> Ok None
    | rc ->
        Error
          (Printf.sprintf "get_by_id failed: %s (%s)" (Sqlite3.Rc.to_string rc)
             (Sqlite3.errmsg db))
  in
  ignore (Sqlite3.finalize stmt);
  result

let enqueue ~db ~room_id ~item_key ~intent ?(now = Unix.gettimeofday ()) () =
  ensure_schema db;
  let id = intent.Github_delivery_intent.id in
  let created_at = Time_util.iso8601_utc ~t:now () in
  let next_attempt_at = created_at in
  let intent_json = Github_delivery_intent.to_json intent in
  let intent_json_s = Yojson.Safe.to_string intent_json in
  (* Idempotent on intent id: return existing row if already enqueued. *)
  match get_by_id ~db ~id with
  | Error e -> Error e
  | Ok (Some existing) -> Ok existing
  | Ok None -> (
      let sql =
        {|INSERT INTO github_delivery_outbox
            (id, room_id, item_key, intent_json, status, attempts,
             next_attempt_at, created_at, last_error, dead_lettered_at)
          VALUES (?, ?, ?, ?, ?, 0, ?, ?, NULL, NULL)|}
      in
      let stmt = Sqlite3.prepare db sql in
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT room_id));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT item_key));
      ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT intent_json_s));
      ignore
        (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT (string_of_status Pending)));
      ignore (Sqlite3.bind stmt 6 (Sqlite3.Data.TEXT next_attempt_at));
      ignore (Sqlite3.bind stmt 7 (Sqlite3.Data.TEXT created_at));
      let rc = Sqlite3.step stmt in
      ignore (Sqlite3.finalize stmt);
      match rc with
      | Sqlite3.Rc.DONE ->
          Ok
            {
              id;
              room_id;
              item_key;
              intent_json;
              status = Pending;
              attempts = 0;
              next_attempt_at;
              created_at;
              last_error = None;
              dead_lettered_at = None;
            }
      | rc ->
          (* Race: another writer may have inserted the same id. *)
          if Sqlite3.errcode db = Sqlite3.Rc.CONSTRAINT then
            match get_by_id ~db ~id with
            | Ok (Some e) -> Ok e
            | Ok None ->
                Error
                  (Printf.sprintf "enqueue constraint without row: %s"
                     (Sqlite3.errmsg db))
            | Error e -> Error e
          else
            Error
              (Printf.sprintf "enqueue failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let claim_due ~db ?(now = Unix.gettimeofday ()) ?(limit = 32) () =
  ensure_schema db;
  let now_s = Time_util.iso8601_utc ~t:now () in
  let limit = max 1 limit in
  match Sqlite3.exec db "BEGIN IMMEDIATE" with
  | rc when rc <> Sqlite3.Rc.OK ->
      Error
        (Printf.sprintf "claim_due begin failed: %s (%s)"
           (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
  | _ -> (
      let select_sql =
        Printf.sprintf
          {|SELECT %s FROM github_delivery_outbox
            WHERE status IN ('pending', 'in_flight')
              AND next_attempt_at <= ?
            ORDER BY next_attempt_at ASC, id ASC
            LIMIT ?|}
          select_columns
      in
      let select_stmt = Sqlite3.prepare db select_sql in
      ignore (Sqlite3.bind select_stmt 1 (Sqlite3.Data.TEXT now_s));
      ignore
        (Sqlite3.bind select_stmt 2 (Sqlite3.Data.INT (Int64.of_int limit)));
      let rec collect acc =
        match Sqlite3.step select_stmt with
        | Sqlite3.Rc.ROW -> (
            match entry_of_stmt select_stmt with
            | Ok e -> collect (e :: acc)
            | Error e -> Error e)
        | Sqlite3.Rc.DONE -> Ok (List.rev acc)
        | rc ->
            Error
              (Printf.sprintf "claim_due select failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
      in
      let selected = collect [] in
      ignore (Sqlite3.finalize select_stmt);
      match selected with
      | Error e ->
          ignore (Sqlite3.exec db "ROLLBACK");
          Error e
      | Ok [] ->
          ignore (Sqlite3.exec db "COMMIT");
          Ok []
      | Ok rows -> (
          let update_sql =
            {|UPDATE github_delivery_outbox SET status = 'in_flight'
              WHERE id = ? AND status IN ('pending', 'in_flight')|}
          in
          let update_one (e : entry) =
            let stmt = Sqlite3.prepare db update_sql in
            ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT e.id));
            let rc = Sqlite3.step stmt in
            ignore (Sqlite3.finalize stmt);
            match rc with
            | Sqlite3.Rc.DONE -> Ok ()
            | rc ->
                Error
                  (Printf.sprintf "claim_due update failed for %s: %s (%s)" e.id
                     (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
          in
          let rec mark_all = function
            | [] -> Ok ()
            | e :: rest -> (
                match update_one e with
                | Error e -> Error e
                | Ok () -> mark_all rest)
          in
          match mark_all rows with
          | Error e ->
              ignore (Sqlite3.exec db "ROLLBACK");
              Error e
          | Ok () -> (
              match Sqlite3.exec db "COMMIT" with
              | Sqlite3.Rc.OK ->
                  Ok (List.map (fun e -> { e with status = In_flight }) rows)
              | rc ->
                  ignore (Sqlite3.exec db "ROLLBACK");
                  Error
                    (Printf.sprintf "claim_due commit failed: %s (%s)"
                       (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))))

let mark_success ~db ~id ?now:(_now = Unix.gettimeofday ()) () =
  ensure_schema db;
  let sql =
    {|UPDATE github_delivery_outbox
        SET status = 'succeeded', last_error = NULL
      WHERE id = ? AND status IN ('pending', 'in_flight')|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE ->
      if Sqlite3.changes db > 0 then Ok ()
      else
        Error
          (Printf.sprintf
             "mark_success: no pending/in_flight outbox row for id %s" id)
  | rc ->
      Error
        (Printf.sprintf "mark_success failed: %s (%s)" (Sqlite3.Rc.to_string rc)
           (Sqlite3.errmsg db))

let mark_failure ~db ~id ~error ?(now = Unix.gettimeofday ())
    ?(max_age_seconds = default_max_age_seconds) () =
  ensure_schema db;
  match get_by_id ~db ~id with
  | Error e -> Error e
  | Ok None -> Error (Printf.sprintf "mark_failure: unknown outbox id %s" id)
  | Ok (Some entry) -> (
      match entry.status with
      | Succeeded | Dead_letter | Superseded ->
          Error
            (Printf.sprintf "mark_failure: entry %s is already %s" id
               (string_of_status entry.status))
      | Pending | In_flight -> (
          let attempts = entry.attempts + 1 in
          let safe_error = redact_error error in
          (* Lexicographic ISO-8601 UTC compares equal to chronological order. *)
          let cutoff = Time_util.iso8601_utc ~t:(now -. max_age_seconds) () in
          let expired = entry.created_at <= cutoff in
          if expired then (
            let dead_at = Time_util.iso8601_utc ~t:now () in
            let sql =
              {|UPDATE github_delivery_outbox
                  SET status = 'dead_letter',
                      attempts = ?,
                      last_error = ?,
                      dead_lettered_at = ?,
                      next_attempt_at = ?
                WHERE id = ?|}
            in
            let stmt = Sqlite3.prepare db sql in
            ignore
              (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int attempts)));
            ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT safe_error));
            ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT dead_at));
            ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT dead_at));
            ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT id));
            let rc = Sqlite3.step stmt in
            ignore (Sqlite3.finalize stmt);
            match rc with
            | Sqlite3.Rc.DONE ->
                Ok
                  {
                    entry with
                    status = Dead_letter;
                    attempts;
                    last_error = Some safe_error;
                    dead_lettered_at = Some dead_at;
                    next_attempt_at = dead_at;
                  }
            | rc ->
                Error
                  (Printf.sprintf "mark_failure dead-letter failed: %s (%s)"
                     (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))
          else
            let delay = backoff_seconds ~attempts in
            let next_at = Time_util.iso8601_utc ~t:(now +. delay) () in
            let sql =
              {|UPDATE github_delivery_outbox
                  SET status = 'pending',
                      attempts = ?,
                      last_error = ?,
                      next_attempt_at = ?,
                      dead_lettered_at = NULL
                WHERE id = ?|}
            in
            let stmt = Sqlite3.prepare db sql in
            ignore
              (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int attempts)));
            ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT safe_error));
            ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT next_at));
            ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT id));
            let rc = Sqlite3.step stmt in
            ignore (Sqlite3.finalize stmt);
            match rc with
            | Sqlite3.Rc.DONE ->
                Ok
                  {
                    entry with
                    status = Pending;
                    attempts;
                    last_error = Some safe_error;
                    next_attempt_at = next_at;
                    dead_lettered_at = None;
                  }
            | rc ->
                Error
                  (Printf.sprintf "mark_failure retry failed: %s (%s)"
                     (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))

let list_dead_letters ~db ?room_id ?(limit = 100) () =
  ensure_schema db;
  let limit = max 1 limit in
  match room_id with
  | None ->
      let sql =
        Printf.sprintf
          {|SELECT %s FROM github_delivery_outbox
            WHERE status = 'dead_letter'
            ORDER BY dead_lettered_at DESC, id DESC
            LIMIT ?|}
          select_columns
      in
      let stmt = Sqlite3.prepare db sql in
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int limit)));
      let rec collect acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match entry_of_stmt stmt with
            | Ok e -> collect (e :: acc)
            | Error e -> Error e)
        | Sqlite3.Rc.DONE -> Ok (List.rev acc)
        | rc ->
            Error
              (Printf.sprintf "list_dead_letters failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
      in
      let result = collect [] in
      ignore (Sqlite3.finalize stmt);
      result
  | Some room_id ->
      if String.trim room_id = "" then Error "room_id must be non-empty"
      else
        let sql =
          Printf.sprintf
            {|SELECT %s FROM github_delivery_outbox
              WHERE status = 'dead_letter' AND room_id = ?
              ORDER BY dead_lettered_at DESC, id DESC
              LIMIT ?|}
            select_columns
        in
        let stmt = Sqlite3.prepare db sql in
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
        ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int limit)));
        let rec collect acc =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW -> (
              match entry_of_stmt stmt with
              | Ok e -> collect (e :: acc)
              | Error e -> Error e)
          | Sqlite3.Rc.DONE -> Ok (List.rev acc)
          | rc ->
              Error
                (Printf.sprintf "list_dead_letters failed: %s (%s)"
                   (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
        in
        let result = collect [] in
        ignore (Sqlite3.finalize stmt);
        result

let count_open_for_item ~db ~room_id ~item_key =
  ensure_schema db;
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else if String.trim item_key = "" then Error "item_key must be non-empty"
  else
    let sql =
      {|SELECT COUNT(*) FROM github_delivery_outbox
        WHERE room_id = ? AND item_key = ?
          AND status IN ('pending', 'in_flight')|}
    in
    let stmt = Sqlite3.prepare db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
    ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT item_key));
    let result =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Ok (int_col stmt 0)
      | rc ->
          Error
            (Printf.sprintf "count_open_for_item failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
    in
    ignore (Sqlite3.finalize stmt);
    result

let mark_superseded ~db ~id =
  ensure_schema db;
  let sql =
    {|UPDATE github_delivery_outbox
        SET status = 'superseded', last_error = NULL
      WHERE id = ? AND status IN ('pending', 'in_flight')|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE ->
      if Sqlite3.changes db > 0 then Ok ()
      else
        Error
          (Printf.sprintf
             "mark_superseded: no pending/in_flight outbox row for id %s" id)
  | rc ->
      Error
        (Printf.sprintf "mark_superseded failed: %s (%s)"
           (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))

let supersede_pending_for_item ~db ~room_id ~item_key =
  ensure_schema db;
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else if String.trim item_key = "" then Error "item_key must be non-empty"
  else
    let sql =
      {|UPDATE github_delivery_outbox
          SET status = 'superseded', last_error = NULL
        WHERE room_id = ? AND item_key = ?
          AND status IN ('pending', 'in_flight')|}
    in
    let stmt = Sqlite3.prepare db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
    ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT item_key));
    let rc = Sqlite3.step stmt in
    ignore (Sqlite3.finalize stmt);
    match rc with
    | Sqlite3.Rc.DONE -> Ok (Sqlite3.changes db)
    | rc ->
        Error
          (Printf.sprintf "supersede_pending_for_item failed: %s (%s)"
             (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))

let count_status ~db ~status ?room_id () =
  ensure_schema db;
  let status_s = string_of_status status in
  match room_id with
  | None ->
      let sql =
        {|SELECT COUNT(*) FROM github_delivery_outbox WHERE status = ?|}
      in
      let stmt = Sqlite3.prepare db sql in
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT status_s));
      let result =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> Ok (int_col stmt 0)
        | rc ->
            Error
              (Printf.sprintf "count_status failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
      in
      ignore (Sqlite3.finalize stmt);
      result
  | Some room_id ->
      if String.trim room_id = "" then Error "room_id must be non-empty"
      else
        let sql =
          {|SELECT COUNT(*) FROM github_delivery_outbox
            WHERE status = ? AND room_id = ?|}
        in
        let stmt = Sqlite3.prepare db sql in
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT status_s));
        ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT room_id));
        let result =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW -> Ok (int_col stmt 0)
          | rc ->
              Error
                (Printf.sprintf "count_status failed: %s (%s)"
                   (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
        in
        ignore (Sqlite3.finalize stmt);
        result

let oldest_pending_created_at ~db ?room_id () =
  ensure_schema db;
  match room_id with
  | None ->
      let sql =
        {|SELECT created_at FROM github_delivery_outbox
          WHERE status = 'pending'
          ORDER BY created_at ASC, id ASC
          LIMIT 1|}
      in
      let stmt = Sqlite3.prepare db sql in
      let result =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> Ok (Some (text_col stmt 0))
        | Sqlite3.Rc.DONE -> Ok None
        | rc ->
            Error
              (Printf.sprintf "oldest_pending_created_at failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
      in
      ignore (Sqlite3.finalize stmt);
      result
  | Some room_id ->
      if String.trim room_id = "" then Error "room_id must be non-empty"
      else
        let sql =
          {|SELECT created_at FROM github_delivery_outbox
            WHERE status = 'pending' AND room_id = ?
            ORDER BY created_at ASC, id ASC
            LIMIT 1|}
        in
        let stmt = Sqlite3.prepare db sql in
        ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
        let result =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW -> Ok (Some (text_col stmt 0))
          | Sqlite3.Rc.DONE -> Ok None
          | rc ->
              Error
                (Printf.sprintf "oldest_pending_created_at failed: %s (%s)"
                   (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
        in
        ignore (Sqlite3.finalize stmt);
        result

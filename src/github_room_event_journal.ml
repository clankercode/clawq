(** Durable Room event journal for routed GitHub envelopes (P19.M3.E1.T001). *)

type journal_entry = {
  id : string;
  room_id : string;
  delivery_id : string option;
  item_key : string;
  envelope_json : Yojson.Safe.t;
  route_id : string option;
  created_at : string;
  session_message_id : string option;
}

let generate_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "ghrej_%d_%06d" ts rand

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "github_room_event_journal schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

let ensure_schema db =
  Sqlite3.busy_timeout db 5_000;
  let table_sql =
    {|CREATE TABLE IF NOT EXISTS github_room_event_journal (
      id TEXT PRIMARY KEY NOT NULL,
      room_id TEXT NOT NULL,
      delivery_id TEXT,
      item_key TEXT NOT NULL,
      envelope_json TEXT NOT NULL,
      route_id TEXT,
      created_at TEXT NOT NULL,
      session_message_id TEXT
    )|}
  in
  (* Idempotency when a GitHub delivery id is present: at most one journal row
     per room + delivery + item. Missing delivery_id rows are always inserted. *)
  let uniq_delivery =
    {|CREATE UNIQUE INDEX IF NOT EXISTS idx_github_room_event_journal_dedup
      ON github_room_event_journal(room_id, delivery_id, item_key)
      WHERE delivery_id IS NOT NULL|}
  in
  let idx_room =
    {|CREATE INDEX IF NOT EXISTS idx_github_room_event_journal_room
      ON github_room_event_journal(room_id)|}
  in
  let idx_delivery =
    {|CREATE INDEX IF NOT EXISTS idx_github_room_event_journal_delivery
      ON github_room_event_journal(delivery_id)|}
  in
  let idx_item =
    {|CREATE INDEX IF NOT EXISTS idx_github_room_event_journal_item
      ON github_room_event_journal(item_key)|}
  in
  List.iter (exec_schema db)
    [ table_sql; uniq_delivery; idx_room; idx_delivery; idx_item ]

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | Sqlite3.Data.INT n -> Int64.to_string n
  | _ -> ""

let opt_text_col stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> Some s | _ -> None

let select_columns =
  {|id, room_id, delivery_id, item_key, envelope_json, route_id,
    created_at, session_message_id|}

let entry_of_stmt stmt : (journal_entry, string) result =
  let id = text_col stmt 0 in
  let room_id = text_col stmt 1 in
  let delivery_id = opt_text_col stmt 2 in
  let item_key = text_col stmt 3 in
  let envelope_json_s = text_col stmt 4 in
  let route_id = opt_text_col stmt 5 in
  let created_at = text_col stmt 6 in
  let session_message_id = opt_text_col stmt 7 in
  try
    let envelope_json = Yojson.Safe.from_string envelope_json_s in
    Ok
      {
        id;
        room_id;
        delivery_id;
        item_key;
        envelope_json;
        route_id;
        created_at;
        session_message_id;
      }
  with exn ->
    Error
      (Printf.sprintf "invalid envelope_json for journal id %s: %s" id
         (Printexc.to_string exn))

let get_by_delivery ~db ~room_id ~delivery_id ~item_key =
  ensure_schema db;
  let sql =
    Printf.sprintf
      {|SELECT %s FROM github_room_event_journal
        WHERE room_id = ? AND delivery_id = ? AND item_key = ?
        LIMIT 1|}
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT delivery_id));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT item_key));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match entry_of_stmt stmt with Ok e -> Ok (Some e) | Error e -> Error e)
    | Sqlite3.Rc.DONE -> Ok None
    | rc ->
        Error
          (Printf.sprintf "get_by_delivery failed: %s (%s)"
             (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
  in
  ignore (Sqlite3.finalize stmt);
  result

let format_hidden_event_message (env : Github_event_envelope.t) =
  (* Short, secret-free transcript line for a hidden role=event session message.
     Never includes bodies, tokens, or review/comment text. *)
  let item =
    match (env.item_kind, env.item_number) with
    | Some Github_event_envelope.Pull_request, Some n ->
        Printf.sprintf "PR #%d" n
    | Some Github_event_envelope.Issue, Some n -> Printf.sprintf "issue #%d" n
    | _ -> "item"
  in
  let action = match env.action with Some a -> a | None -> "-" in
  let actor =
    match env.actor.login with Some l when String.trim l <> "" -> l | _ -> "-"
  in
  let family = Github_event_envelope.string_of_family env.family in
  Printf.sprintf "[github_event] %s %s %s action=%s family=%s actor=%s"
    env.repo_full_name item env.event action family actor

let set_session_message_id ~db ~id ~session_message_id =
  let sql =
    {|UPDATE github_room_event_journal SET session_message_id = ? WHERE id = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT session_message_id));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT id));
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "set session_message_id failed: %s (%s)"
           (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))

let insert_row ~db ~id ~room_id ~delivery_id ~item_key ~envelope_json_s
    ~route_id ~created_at =
  let sql =
    {|INSERT INTO github_room_event_journal
        (id, room_id, delivery_id, item_key, envelope_json, route_id,
         created_at, session_message_id)
      VALUES (?, ?, ?, ?, ?, ?, ?, NULL)|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT room_id));
  (match delivery_id with
  | Some d -> ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT d))
  | None -> ignore (Sqlite3.bind stmt 3 Sqlite3.Data.NULL));
  ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT item_key));
  ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT envelope_json_s));
  (match route_id with
  | Some r -> ignore (Sqlite3.bind stmt 6 (Sqlite3.Data.TEXT r))
  | None -> ignore (Sqlite3.bind stmt 6 Sqlite3.Data.NULL));
  ignore (Sqlite3.bind stmt 7 (Sqlite3.Data.TEXT created_at));
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | Sqlite3.Rc.CONSTRAINT -> Error `Duplicate
  | rc ->
      Error
        (`Db
           (Printf.sprintf "insert journal failed: %s (%s)"
              (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let append ~db ~room_id ~envelope ?route_id ?session_append
    ?(now = Unix.gettimeofday ()) () =
  ensure_schema db;
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else
    let item_key = Github_route_match.canonical_item_key envelope in
    let delivery_id =
      match envelope.delivery_id with
      | Some d when String.trim d <> "" -> Some (String.trim d)
      | _ -> None
    in
    (* Fast path: return existing when delivery is present. *)
    let existing =
      match delivery_id with
      | None -> Ok None
      | Some d -> get_by_delivery ~db ~room_id ~delivery_id:d ~item_key
    in
    match existing with
    | Error e -> Error e
    | Ok (Some entry) -> Ok entry
    | Ok None -> (
        let envelope_json = Github_event_envelope.to_safe_json envelope in
        let envelope_json_s = Yojson.Safe.to_string envelope_json in
        let id = generate_id ~now () in
        let created_at = Time_util.iso8601_utc ~t:now () in
        match
          insert_row ~db ~id ~room_id ~delivery_id ~item_key ~envelope_json_s
            ~route_id ~created_at
        with
        | Error (`Db e) -> Error e
        | Error `Duplicate -> (
            (* Concurrent insert won the race — return the winner. *)
            match delivery_id with
            | None ->
                Error
                  "duplicate journal insert without delivery_id (unexpected)"
            | Some d -> (
                match get_by_delivery ~db ~room_id ~delivery_id:d ~item_key with
                | Ok (Some e) -> Ok e
                | Ok None ->
                    Error "duplicate constraint but row not found after race"
                | Error e -> Error e))
        | Ok () -> (
            (* Optional hidden session event — never wakes the agent. The
               callback is expected only to append a role=event message. *)
            let session_message_id =
              match session_append with
              | None -> Ok None
              | Some append_fn -> (
                  let content = format_hidden_event_message envelope in
                  match append_fn ~room_id ~content with
                  | Error e -> Error e
                  | Ok msg_id -> (
                      match
                        set_session_message_id ~db ~id
                          ~session_message_id:msg_id
                      with
                      | Ok () -> Ok (Some msg_id)
                      | Error e -> Error e))
            in
            match session_message_id with
            | Error e -> Error e
            | Ok session_message_id ->
                Ok
                  {
                    id;
                    room_id;
                    delivery_id;
                    item_key;
                    envelope_json;
                    route_id;
                    created_at;
                    session_message_id;
                  }))

let list_for_room ~db ~room_id ?limit () =
  ensure_schema db;
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else
    let sql, bind_limit =
      match limit with
      | Some n when n > 0 ->
          ( Printf.sprintf
              {|SELECT %s FROM github_room_event_journal
                WHERE room_id = ?
                ORDER BY created_at ASC, id ASC
                LIMIT ?|}
              select_columns,
            Some n )
      | _ ->
          ( Printf.sprintf
              {|SELECT %s FROM github_room_event_journal
                WHERE room_id = ?
                ORDER BY created_at ASC, id ASC|}
              select_columns,
            None )
    in
    let stmt = Sqlite3.prepare db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
    (match bind_limit with
    | Some n -> ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int n)))
    | None -> ());
    let rec loop acc =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match entry_of_stmt stmt with
          | Ok e -> loop (e :: acc)
          | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok (List.rev acc)
      | rc ->
          Error
            (Printf.sprintf "list_for_room failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
    in
    let result = loop [] in
    ignore (Sqlite3.finalize stmt);
    result

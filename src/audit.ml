type event =
  | ChatMessage of { session_key : string; role : string; content_preview : string }
  | ToolInvocation of { session_key : string; tool_name : string;
                        risk_level : string; args_preview : string }
  | ToolResult of { session_key : string; tool_name : string; success : bool }
  | ConfigChange of { field : string; old_value : string; new_value : string }
  | DaemonEvent of { action : string; details : string }

type row = {
  id : int;
  timestamp : string;
  event_type : string;
  session_key : string option;
  details : string option;
  tool_name : string option;
  risk_level : string option;
}

let init_schema db =
  let sql = "CREATE TABLE IF NOT EXISTS audit_log (\n\
             \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
             \  timestamp TEXT NOT NULL DEFAULT (datetime('now')),\n\
             \  event_type TEXT NOT NULL,\n\
             \  session_key TEXT,\n\
             \  details TEXT,\n\
             \  tool_name TEXT,\n\
             \  risk_level TEXT\n\
             )" in
  (match Sqlite3.exec db sql with
   | Sqlite3.Rc.OK -> ()
   | rc -> failwith (Printf.sprintf "Audit schema error: %s" (Sqlite3.Rc.to_string rc)));
  (* Migrate: add signature and prev_hash columns if missing *)
  let add_col col =
    let sql = Printf.sprintf "ALTER TABLE audit_log ADD COLUMN %s TEXT" col in
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | _ -> () (* column already exists *)
  in
  add_col "signature";
  add_col "prev_hash"

let event_fields event =
  match event with
  | ChatMessage { session_key; role; content_preview } ->
    ("chat_message", Some session_key,
     Some (Printf.sprintf "%s: %s" role
             (if String.length content_preview > 200
              then String.sub content_preview 0 200 ^ "..."
              else content_preview)),
     None, None)
  | ToolInvocation { session_key; tool_name; risk_level; args_preview } ->
    ("tool_invocation", Some session_key,
     Some (if String.length args_preview > 200
           then String.sub args_preview 0 200 ^ "..."
           else args_preview),
     Some tool_name, Some risk_level)
  | ToolResult { session_key; tool_name; success } ->
    ("tool_result", Some session_key,
     Some (if success then "success" else "failure"),
     Some tool_name, None)
  | ConfigChange { field; old_value; new_value } ->
    ("config_change", None,
     Some (Printf.sprintf "%s: %s -> %s" field old_value new_value),
     None, None)
  | DaemonEvent { action; details } ->
    ("daemon_event", None, Some (Printf.sprintf "%s: %s" action details),
     None, None)

let bind_opt stmt idx = function
  | Some s -> ignore (Sqlite3.bind stmt idx (Sqlite3.Data.TEXT s))
  | None -> ignore (Sqlite3.bind stmt idx Sqlite3.Data.NULL)

let log_unsigned ~db event =
  let event_type, session_key, details, tool_name, risk_level = event_fields event in
  let sql = "INSERT INTO audit_log (event_type, session_key, details, tool_name, risk_level) \
             VALUES (?, ?, ?, ?, ?)" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT event_type));
  bind_opt stmt 2 session_key;
  bind_opt stmt 3 details;
  bind_opt stmt 4 tool_name;
  bind_opt stmt 5 risk_level;
  (match Sqlite3.step stmt with
   | Sqlite3.Rc.DONE -> ()
   | rc -> Logs.warn (fun m -> m "Audit log failed: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

(* Signing key derivation *)
let derive_signing_key passphrase =
  Pbkdf.pbkdf2 ~prf:`SHA256 ~password:passphrase ~salt:"clawq-audit-sign-v1"
    ~count:100_000 ~dk_len:32l

let get_signing_key () =
  match Sys.getenv_opt "CLAWQ_MASTER_KEY" with
  | None -> Error "CLAWQ_MASTER_KEY environment variable is not set"
  | Some "" -> Error "CLAWQ_MASTER_KEY environment variable is empty"
  | Some passphrase -> Ok (derive_signing_key passphrase)

let get_last_signature ~db =
  let sql = "SELECT signature FROM audit_log WHERE signature IS NOT NULL ORDER BY id DESC LIMIT 1" in
  let stmt = Sqlite3.prepare db sql in
  let result =
    if Sqlite3.step stmt = Sqlite3.Rc.ROW then
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    else None
  in
  ignore (Sqlite3.finalize stmt);
  result

let compute_prev_hash last_sig =
  match last_sig with
  | None -> "genesis"
  | Some sig_str ->
    Digestif.SHA256.(digest_string sig_str |> to_hex)

let compute_signature ~key ~prev_hash ~timestamp ~event_type ~details_str =
  let payload = prev_hash ^ timestamp ^ event_type ^ details_str in
  Digestif.SHA256.(hmac_string ~key payload |> to_hex)

let log_signed ~db ~key event =
  let event_type, session_key, details, tool_name, risk_level = event_fields event in
  let details_str = match details with Some d -> d | None -> "" in
  let last_sig = get_last_signature ~db in
  let prev_hash = compute_prev_hash last_sig in
  (* Get current timestamp from SQLite for consistency *)
  let timestamp =
    let stmt = Sqlite3.prepare db "SELECT datetime('now')" in
    let ts = if Sqlite3.step stmt = Sqlite3.Rc.ROW then
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.TEXT s -> s | _ -> ""
    else "" in
    ignore (Sqlite3.finalize stmt);
    ts
  in
  let signature = compute_signature ~key ~prev_hash ~timestamp ~event_type ~details_str in
  let sql = "INSERT INTO audit_log (timestamp, event_type, session_key, details, tool_name, \
             risk_level, signature, prev_hash) \
             VALUES (?, ?, ?, ?, ?, ?, ?, ?)" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT timestamp));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT event_type));
  bind_opt stmt 3 session_key;
  bind_opt stmt 4 details;
  bind_opt stmt 5 tool_name;
  bind_opt stmt 6 risk_level;
  ignore (Sqlite3.bind stmt 7 (Sqlite3.Data.TEXT signature));
  ignore (Sqlite3.bind stmt 8 (Sqlite3.Data.TEXT prev_hash));
  (match Sqlite3.step stmt with
   | Sqlite3.Rc.DONE -> ()
   | rc -> Logs.warn (fun m -> m "Audit log (signed) failed: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let log ~db ?signing_key event =
  match signing_key with
  | Some key -> log_signed ~db ~key event
  | None -> log_unsigned ~db event

let verify_chain ~db ~key =
  let sql = "SELECT id, timestamp, event_type, details, signature, prev_hash \
             FROM audit_log ORDER BY id ASC" in
  let stmt = Sqlite3.prepare db sql in
  let last_sig = ref None in
  let result = ref (Ok ()) in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW && !result = Ok () do
    let id = match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    let timestamp = match Sqlite3.column stmt 1 with
      | Sqlite3.Data.TEXT s -> s | _ -> "" in
    let event_type = match Sqlite3.column stmt 2 with
      | Sqlite3.Data.TEXT s -> s | _ -> "" in
    let details_str = match Sqlite3.column stmt 3 with
      | Sqlite3.Data.TEXT s -> s | _ -> "" in
    let signature = match Sqlite3.column stmt 4 with
      | Sqlite3.Data.TEXT s -> Some s | _ -> None in
    let prev_hash = match Sqlite3.column stmt 5 with
      | Sqlite3.Data.TEXT s -> Some s | _ -> None in
    match signature, prev_hash with
    | None, _ ->
      (* Unsigned entry, skip *)
      ()
    | Some sig_str, Some ph ->
      let expected_prev = compute_prev_hash !last_sig in
      if ph <> expected_prev then
        result := Error (id, Printf.sprintf "prev_hash mismatch: expected %s, got %s" expected_prev ph)
      else begin
        let expected_sig = compute_signature ~key ~prev_hash:ph ~timestamp ~event_type ~details_str in
        if sig_str <> expected_sig then
          result := Error (id, Printf.sprintf "signature mismatch")
        else
          last_sig := Some sig_str
      end
    | Some _, None ->
      result := Error (id, "signed entry missing prev_hash")
  done;
  ignore (Sqlite3.finalize stmt);
  !result

(* Retention: purge old entries *)
let purge_old ~db ~max_age_days ~max_entries =
  let deleted = ref 0 in
  (* Delete by age *)
  let sql_age = Printf.sprintf
    "DELETE FROM audit_log WHERE timestamp < datetime('now', '-%d days')" max_age_days in
  (match Sqlite3.exec db sql_age with
   | Sqlite3.Rc.OK -> deleted := !deleted + Sqlite3.changes db
   | _ -> ());
  (* Delete by count: keep only newest max_entries *)
  let sql_count = Printf.sprintf
    "DELETE FROM audit_log WHERE id NOT IN \
     (SELECT id FROM audit_log ORDER BY id DESC LIMIT %d)" max_entries in
  (match Sqlite3.exec db sql_count with
   | Sqlite3.Rc.OK -> deleted := !deleted + Sqlite3.changes db
   | _ -> ());
  !deleted

(* Export all rows as JSONL *)
let export_json ~db ~path =
  let dir = Filename.dirname path in
  let rec ensure_dir d =
    if d <> "/" && d <> "." && not (Sys.file_exists d) then begin
      ensure_dir (Filename.dirname d);
      (try Sys.mkdir d 0o755 with _ -> ())
    end
  in
  ensure_dir dir;
  let oc = open_out path in
  let sql = "SELECT id, timestamp, event_type, session_key, details, tool_name, \
             risk_level, signature, prev_hash FROM audit_log ORDER BY id ASC" in
  let stmt = Sqlite3.prepare db sql in
  let count = ref 0 in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let text_or_null i = match Sqlite3.column stmt i with
      | Sqlite3.Data.TEXT s -> `String s | _ -> `Null in
    let id = match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT i -> `Int (Int64.to_int i) | _ -> `Int 0 in
    let json = `Assoc [
      ("id", id);
      ("timestamp", text_or_null 1);
      ("event_type", text_or_null 2);
      ("session_key", text_or_null 3);
      ("details", text_or_null 4);
      ("tool_name", text_or_null 5);
      ("risk_level", text_or_null 6);
      ("signature", text_or_null 7);
      ("prev_hash", text_or_null 8);
    ] in
    output_string oc (Yojson.Safe.to_string json);
    output_char oc '\n';
    incr count
  done;
  ignore (Sqlite3.finalize stmt);
  close_out oc;
  !count

(* Retention tick: export if configured, then purge *)
let retention_tick ~db ~(config : Runtime_config.t) =
  let ret = config.security.audit_retention in
  if ret.export_before_purge then begin
    let timestamp = string_of_float (Unix.gettimeofday ()) in
    let path = Filename.concat ret.export_path
      (Printf.sprintf "audit_export_%s.jsonl" timestamp) in
    let count = export_json ~db ~path in
    Logs.info (fun m -> m "Audit export: %d entries to %s" count path)
  end;
  let deleted = purge_old ~db ~max_age_days:ret.max_age_days ~max_entries:ret.max_entries in
  if deleted > 0 then
    Logs.info (fun m -> m "Audit retention: purged %d entries" deleted);
  deleted

let query ~db ?event_type ?session_key ~limit () =
  let conditions = ref [] in
  let params = ref [] in
  (match event_type with
   | Some et -> conditions := "event_type = ?" :: !conditions;
     params := et :: !params
   | None -> ());
  (match session_key with
   | Some sk -> conditions := "session_key = ?" :: !conditions;
     params := sk :: !params
   | None -> ());
  let where = match !conditions with
    | [] -> ""
    | conds -> " WHERE " ^ String.concat " AND " (List.rev conds)
  in
  let sql = Printf.sprintf
      "SELECT id, timestamp, event_type, session_key, details, tool_name, risk_level \
       FROM audit_log%s ORDER BY id DESC LIMIT ?" where in
  let stmt = Sqlite3.prepare db sql in
  let idx = ref 1 in
  List.iter (fun p ->
    ignore (Sqlite3.bind stmt !idx (Sqlite3.Data.TEXT p));
    incr idx
  ) (List.rev !params);
  ignore (Sqlite3.bind stmt !idx (Sqlite3.Data.INT (Int64.of_int limit)));
  let rows = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let text_opt i = match Sqlite3.column stmt i with
      | Sqlite3.Data.TEXT s -> Some s | _ -> None in
    let id = match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT i -> Int64.to_int i | _ -> 0 in
    let timestamp = match Sqlite3.column stmt 1 with
      | Sqlite3.Data.TEXT s -> s | _ -> "" in
    let event_type = match Sqlite3.column stmt 2 with
      | Sqlite3.Data.TEXT s -> s | _ -> "" in
    rows := { id; timestamp; event_type;
              session_key = text_opt 3; details = text_opt 4;
              tool_name = text_opt 5; risk_level = text_opt 6 } :: !rows
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !rows

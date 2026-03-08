type event =
  | ChatMessage of {
      session_key : string;
      role : string;
      content_preview : string;
    }
  | ToolInvocation of {
      session_key : string;
      tool_name : string;
      risk_level : string;
      args_preview : string;
    }
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
  let sql =
    "CREATE TABLE IF NOT EXISTS audit_log (\n\
    \  id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \  timestamp TEXT NOT NULL DEFAULT (datetime('now')),\n\
    \  event_type TEXT NOT NULL,\n\
    \  session_key TEXT,\n\
    \  details TEXT,\n\
    \  tool_name TEXT,\n\
    \  risk_level TEXT\n\
     )"
  in
  (match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "Audit schema error: %s" (Sqlite3.Rc.to_string rc)));
  (* Migrate: add signature and prev_hash columns if missing *)
  let add_col col =
    let sql = Printf.sprintf "ALTER TABLE audit_log ADD COLUMN %s TEXT" col in
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | _ -> () (* column already exists *)
  in
  add_col "signature";
  add_col "prev_hash";
  let meta_sql =
    "CREATE TABLE IF NOT EXISTS audit_meta (\n\
    \  key TEXT PRIMARY KEY,\n\
    \  value TEXT\n\
     )"
  in
  match Sqlite3.exec db meta_sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "Audit meta schema error: %s" (Sqlite3.Rc.to_string rc))

let event_fields event =
  match event with
  | ChatMessage { session_key; role; content_preview } ->
      ( "chat_message",
        Some session_key,
        Some
          (Printf.sprintf "%s: %s" role
             (if String.length content_preview > 200 then
                String.sub content_preview 0 200 ^ "..."
              else content_preview)),
        None,
        None )
  | ToolInvocation { session_key; tool_name; risk_level; args_preview } ->
      ( "tool_invocation",
        Some session_key,
        Some
          (if String.length args_preview > 200 then
             String.sub args_preview 0 200 ^ "..."
           else args_preview),
        Some tool_name,
        Some risk_level )
  | ToolResult { session_key; tool_name; success } ->
      ( "tool_result",
        Some session_key,
        Some (if success then "success" else "failure"),
        Some tool_name,
        None )
  | ConfigChange { field; old_value; new_value } ->
      ( "config_change",
        None,
        Some (Printf.sprintf "%s: %s -> %s" field old_value new_value),
        None,
        None )
  | DaemonEvent { action; details } ->
      ( "daemon_event",
        None,
        Some (Printf.sprintf "%s: %s" action details),
        None,
        None )

let bind_opt stmt idx = function
  | Some s -> ignore (Sqlite3.bind stmt idx (Sqlite3.Data.TEXT s))
  | None -> ignore (Sqlite3.bind stmt idx Sqlite3.Data.NULL)

let log_unsigned ~db event =
  let event_type, session_key, details, tool_name, risk_level =
    event_fields event
  in
  let sql =
    "INSERT INTO audit_log (event_type, session_key, details, tool_name, \
     risk_level) VALUES (?, ?, ?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT event_type));
  bind_opt stmt 2 session_key;
  bind_opt stmt 3 details;
  bind_opt stmt 4 tool_name;
  bind_opt stmt 5 risk_level;
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m -> m "Audit log failed: %s" (Sqlite3.Rc.to_string rc)));
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

let chain_anchor_key = "chain_anchor_signature"

let get_chain_anchor ~db =
  let stmt =
    Sqlite3.prepare db "SELECT value FROM audit_meta WHERE key = ? LIMIT 1"
  in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT chain_anchor_key));
  let result =
    if Sqlite3.step stmt = Sqlite3.Rc.ROW then
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.TEXT s when s <> "" -> Some s
      | _ -> None
    else None
  in
  ignore (Sqlite3.finalize stmt);
  result

let set_chain_anchor ~db anchor =
  let stmt =
    Sqlite3.prepare db
      "INSERT INTO audit_meta (key, value) VALUES (?, ?) ON CONFLICT(key) DO \
       UPDATE SET value = excluded.value"
  in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT chain_anchor_key));
  (match anchor with
  | Some sig_str -> ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT sig_str))
  | None -> ignore (Sqlite3.bind stmt 2 Sqlite3.Data.NULL));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt)

let retention_cutoff ~db ~max_age_days =
  let sql = Printf.sprintf "SELECT datetime('now', '-%d days')" max_age_days in
  let stmt = Sqlite3.prepare db sql in
  let result =
    if Sqlite3.step stmt = Sqlite3.Rc.ROW then
      match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
    else ""
  in
  ignore (Sqlite3.finalize stmt);
  result

let compute_retention_boundary ~db ~max_age_days ~max_entries =
  let cutoff = retention_cutoff ~db ~max_age_days in
  let stmt =
    Sqlite3.prepare db "SELECT id, timestamp FROM audit_log ORDER BY id DESC"
  in
  (* Retain only a contiguous newest suffix so signed-chain verification stays
     valid even if imported timestamps are not monotone with ids. *)
  let rec loop kept boundary =
    if kept >= max_entries then boundary
    else
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let id =
            match Sqlite3.column stmt 0 with
            | Sqlite3.Data.INT i -> Int64.to_int i
            | _ -> 0
          in
          let timestamp =
            match Sqlite3.column stmt 1 with
            | Sqlite3.Data.TEXT s -> s
            | _ -> ""
          in
          if timestamp >= cutoff then loop (kept + 1) (Some id) else boundary
      | _ -> boundary
  in
  let boundary = if max_entries <= 0 then None else loop 0 None in
  ignore (Sqlite3.finalize stmt);
  boundary

let latest_deleted_signature ~db ~boundary_id =
  let sql, bind =
    match boundary_id with
    | Some id ->
        ( "SELECT signature FROM audit_log WHERE signature IS NOT NULL AND id \
           < ? ORDER BY id DESC LIMIT 1",
          Some id )
    | None ->
        ( "SELECT signature FROM audit_log WHERE signature IS NOT NULL ORDER \
           BY id DESC LIMIT 1",
          None )
  in
  let stmt = Sqlite3.prepare db sql in
  (match bind with
  | Some id -> ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id)))
  | None -> ());
  let result =
    if Sqlite3.step stmt = Sqlite3.Rc.ROW then
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    else None
  in
  ignore (Sqlite3.finalize stmt);
  result

let get_last_signature ~db =
  let sql =
    "SELECT signature FROM audit_log WHERE signature IS NOT NULL ORDER BY id \
     DESC LIMIT 1"
  in
  let stmt = Sqlite3.prepare db sql in
  let result =
    if Sqlite3.step stmt = Sqlite3.Rc.ROW then
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    else get_chain_anchor ~db
  in
  ignore (Sqlite3.finalize stmt);
  result

let compute_prev_hash last_sig =
  match last_sig with
  | None -> "genesis"
  | Some sig_str -> Digestif.SHA256.(digest_string sig_str |> to_hex)

let encode_signed_field value =
  Printf.sprintf "%d:%s" (String.length value) value

let compute_signature_legacy ~key ~prev_hash ~timestamp ~event_type ~details_str
    =
  let payload = prev_hash ^ timestamp ^ event_type ^ details_str in
  Digestif.SHA256.(hmac_string ~key payload |> to_hex)

let compute_signature ~key ~prev_hash ~timestamp ~event_type ~session_key
    ~details_str ~tool_name ~risk_level =
  let text = function Some s -> s | None -> "" in
  let payload_fields =
    [
      prev_hash;
      timestamp;
      event_type;
      text session_key;
      details_str;
      text tool_name;
      text risk_level;
    ]
  in
  let payload =
    String.concat "|" (List.map encode_signed_field payload_fields)
  in
  Digestif.SHA256.(hmac_string ~key payload |> to_hex)

let legacy_signature_eligible ~session_key ~tool_name ~risk_level =
  session_key = None && tool_name = None && risk_level = None

let project_current_entry ~timestamp ~event_type ~session_key ~details_str
    ~tool_name ~risk_level ~signature ~prev_hash =
  {
    Clawq_core.ae_timestamp = timestamp;
    ae_event_type = event_type;
    ae_session_key = session_key;
    ae_details = details_str;
    ae_tool_name = tool_name;
    ae_risk_level = risk_level;
    ae_signature = signature;
    ae_prev_hash = prev_hash;
  }

let locate_current_segment_failure ~key ~seed segment =
  let rec loop prev_sig = function
    | [] ->
        Error (0, "extracted verify_chain failed without pinpointing an entry")
    | (id, entry) :: rest ->
        if Clawq_core.verify_link key prev_sig entry then
          loop (Some entry.ae_signature) rest
        else
          let expected_prev = Clawq_core.compute_prev_hash prev_sig in
          if entry.ae_prev_hash <> expected_prev then
            Error
              ( id,
                Printf.sprintf "prev_hash mismatch: expected %s, got %s"
                  expected_prev entry.ae_prev_hash )
          else Error (id, "signature mismatch")
  in
  loop seed segment

let verify_current_segment ~key ~seed segment =
  if segment = [] then Ok ()
  else
    let entries = List.map snd segment in
    if Clawq_core.verify_chain key seed entries then Ok ()
    else locate_current_segment_failure ~key ~seed segment

let log_signed ~db ~key event =
  let event_type, session_key, details, tool_name, risk_level =
    event_fields event
  in
  let details_str = match details with Some d -> d | None -> "" in
  let last_sig = get_last_signature ~db in
  (* Get current timestamp from SQLite for consistency *)
  let timestamp =
    let stmt = Sqlite3.prepare db "SELECT datetime('now')" in
    let ts =
      if Sqlite3.step stmt = Sqlite3.Rc.ROW then
        match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
      else ""
    in
    ignore (Sqlite3.finalize stmt);
    ts
  in
  let entry =
    Clawq_core.make_entry key last_sig timestamp event_type session_key
      details_str tool_name risk_level
  in
  let sql =
    "INSERT INTO audit_log (timestamp, event_type, session_key, details, \
     tool_name, risk_level, signature, prev_hash) VALUES (?, ?, ?, ?, ?, ?, ?, \
     ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT timestamp));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT event_type));
  bind_opt stmt 3 session_key;
  bind_opt stmt 4 details;
  bind_opt stmt 5 tool_name;
  bind_opt stmt 6 risk_level;
  ignore (Sqlite3.bind stmt 7 (Sqlite3.Data.TEXT entry.ae_signature));
  ignore (Sqlite3.bind stmt 8 (Sqlite3.Data.TEXT entry.ae_prev_hash));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ()
  | rc ->
      Logs.warn (fun m ->
          m "Audit log (signed) failed: %s" (Sqlite3.Rc.to_string rc)));
  ignore (Sqlite3.finalize stmt)

let log ~db ?signing_key event =
  match signing_key with
  | Some key -> log_signed ~db ~key event
  | None -> log_unsigned ~db event

let verify_chain ~db ~key =
  let sql =
    "SELECT id, timestamp, event_type, session_key, details, tool_name, \
     risk_level, signature, prev_hash FROM audit_log ORDER BY id ASC"
  in
  let stmt = Sqlite3.prepare db sql in
  (* Unsigned rows are preserved as audit records but are not part of the
     cryptographic chain. Verification only advances across signed rows, so a
     retained-chain anchor applies to the first retained signed row; any leading
     unsigned rows remain informational only. *)
  let last_sig = ref (get_chain_anchor ~db) in
  let result = ref (Ok ()) in
  let current_seed = ref None in
  let current_segment = ref [] in
  let flush_current_segment () =
    match !result with
    | Error _ -> ()
    | Ok () -> (
        let seed =
          match !current_seed with Some seed -> seed | None -> !last_sig
        in
        let segment = List.rev !current_segment in
        match verify_current_segment ~key ~seed segment with
        | Ok () ->
            current_seed := None;
            current_segment := []
        | Error _ as err -> result := err)
  in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW && !result = Ok () do
    let id =
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT i -> Int64.to_int i
      | _ -> 0
    in
    let timestamp =
      match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let event_type =
      match Sqlite3.column stmt 2 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let details_str =
      match Sqlite3.column stmt 4 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let session_key =
      match Sqlite3.column stmt 3 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let tool_name =
      match Sqlite3.column stmt 5 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let risk_level =
      match Sqlite3.column stmt 6 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let signature =
      match Sqlite3.column stmt 7 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let prev_hash =
      match Sqlite3.column stmt 8 with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    match (signature, prev_hash) with
    | None, None ->
        (* Unsigned entry, skip *)
        ()
    | None, Some _ ->
        result := Error (id, "unsigned entry unexpectedly carries prev_hash")
    | Some sig_str, Some ph ->
        let legacy_match =
          legacy_signature_eligible ~session_key ~tool_name ~risk_level
          &&
          let expected_legacy =
            compute_signature_legacy ~key ~prev_hash:ph ~timestamp ~event_type
              ~details_str
          in
          sig_str = expected_legacy
        in
        if legacy_match then begin
          flush_current_segment ();
          if !result = Ok () then begin
            let expected_prev = compute_prev_hash !last_sig in
            if ph <> expected_prev then
              result :=
                Error
                  ( id,
                    Printf.sprintf "prev_hash mismatch: expected %s, got %s"
                      expected_prev ph )
            else last_sig := Some sig_str
          end
        end
        else begin
          (match !current_seed with
          | None -> current_seed := Some !last_sig
          | Some _ -> ());
          current_segment :=
            ( id,
              project_current_entry ~timestamp ~event_type ~session_key
                ~details_str ~tool_name ~risk_level ~signature:sig_str
                ~prev_hash:ph )
            :: !current_segment;
          last_sig := Some sig_str
        end
    | Some _, None -> result := Error (id, "signed entry missing prev_hash")
  done;
  ignore (Sqlite3.finalize stmt);
  flush_current_segment ();
  !result

let signature_counts ~db =
  let stmt =
    Sqlite3.prepare db
      "SELECT SUM(CASE WHEN signature IS NOT NULL THEN 1 ELSE 0 END), SUM(CASE \
       WHEN signature IS NULL THEN 1 ELSE 0 END) FROM audit_log"
  in
  let counts =
    if Sqlite3.step stmt = Sqlite3.Rc.ROW then
      let count i =
        match Sqlite3.column stmt i with
        | Sqlite3.Data.INT n -> Int64.to_int n
        | _ -> 0
      in
      (count 0, count 1)
    else (0, 0)
  in
  ignore (Sqlite3.finalize stmt);
  counts

(* Retention: purge old entries *)
let purge_old ~db ~max_age_days ~max_entries =
  let existing_anchor = get_chain_anchor ~db in
  let boundary_id = compute_retention_boundary ~db ~max_age_days ~max_entries in
  let retained_anchor = latest_deleted_signature ~db ~boundary_id in
  let deleted = ref 0 in
  let delete_sql =
    match boundary_id with
    | Some id -> Printf.sprintf "DELETE FROM audit_log WHERE id < %d" id
    | None -> "DELETE FROM audit_log"
  in
  (match Sqlite3.exec db delete_sql with
  | Sqlite3.Rc.OK -> deleted := Sqlite3.changes db
  | _ -> ());
  (if !deleted > 0 then
     let next_anchor =
       match retained_anchor with
       | Some _ -> retained_anchor
       | None -> existing_anchor
     in
     set_chain_anchor ~db next_anchor);
  !deleted

let export_anchor ~db ~path =
  let json =
    `Assoc
      [
        ("format", `String "clawq-audit-anchor-v1");
        ( "chain_anchor_signature",
          match get_chain_anchor ~db with Some s -> `String s | None -> `Null );
      ]
  in
  Yojson.Safe.to_file (path ^ ".anchor.json") json

(* Export all rows as JSONL *)
let export_json ~db ~path =
  let dir = Filename.dirname path in
  let rec ensure_dir d =
    if d <> "/" && d <> "." && not (Sys.file_exists d) then begin
      ensure_dir (Filename.dirname d);
      try Sys.mkdir d 0o755 with _ -> ()
    end
  in
  ensure_dir dir;
  let oc = open_out path in
  let sql =
    "SELECT id, timestamp, event_type, session_key, details, tool_name, \
     risk_level, signature, prev_hash FROM audit_log ORDER BY id ASC"
  in
  let stmt = Sqlite3.prepare db sql in
  let count = ref 0 in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let text_or_null i =
      match Sqlite3.column stmt i with
      | Sqlite3.Data.TEXT s -> `String s
      | _ -> `Null
    in
    let id =
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT i -> `Int (Int64.to_int i)
      | _ -> `Int 0
    in
    let json =
      `Assoc
        [
          ("id", id);
          ("timestamp", text_or_null 1);
          ("event_type", text_or_null 2);
          ("session_key", text_or_null 3);
          ("details", text_or_null 4);
          ("tool_name", text_or_null 5);
          ("risk_level", text_or_null 6);
          ("signature", text_or_null 7);
          ("prev_hash", text_or_null 8);
        ]
    in
    output_string oc (Yojson.Safe.to_string json);
    output_char oc '\n';
    incr count
  done;
  ignore (Sqlite3.finalize stmt);
  close_out oc;
  export_anchor ~db ~path;
  !count

let import_anchor ~anchor_path =
  let open Yojson.Safe.Util in
  try
    let json = Yojson.Safe.from_file anchor_path in
    let format = json |> member "format" |> to_string in
    if format <> "clawq-audit-anchor-v1" then
      Error
        (Printf.sprintf "Unsupported audit anchor format in %s: %s" anchor_path
           format)
    else
      let anchor =
        match json |> member "chain_anchor_signature" with
        | `Null -> None
        | `String s when s <> "" -> Some s
        | `String _ -> None
        | _ ->
            raise
              (Failure
                 (Printf.sprintf
                    "Invalid chain_anchor_signature value in anchor file %s"
                    anchor_path))
      in
      Ok anchor
  with
  | Yojson.Json_error msg ->
      Error
        (Printf.sprintf "Invalid audit anchor JSON in %s: %s" anchor_path msg)
  | Yojson.Safe.Util.Type_error (msg, _) ->
      Error (Printf.sprintf "Invalid audit anchor in %s: %s" anchor_path msg)
  | Sys_error msg -> Error msg
  | Failure msg -> Error msg

let audit_row_count ~db =
  let stmt = Sqlite3.prepare db "SELECT COUNT(*) FROM audit_log" in
  let count =
    if Sqlite3.step stmt = Sqlite3.Rc.ROW then
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT n -> Int64.to_int n
      | _ -> 0
    else 0
  in
  ignore (Sqlite3.finalize stmt);
  count

let import_json ~db ~path:file_path ?anchor_path () =
  let open Yojson.Safe.Util in
  if audit_row_count ~db > 0 then
    Error "Audit import requires an empty audit log"
  else
    try
      let resolved_anchor_path =
        match anchor_path with
        | Some p -> Some p
        | None ->
            let default_path = file_path ^ ".anchor.json" in
            if Sys.file_exists default_path then Some default_path else None
      in
      let anchor =
        match resolved_anchor_path with
        | Some p -> (
            match import_anchor ~anchor_path:p with
            | Ok a -> a
            | Error msg -> raise (Failure msg))
        | None -> None
      in
      let insert_sql =
        "INSERT INTO audit_log (timestamp, event_type, session_key, details, \
         tool_name, risk_level, signature, prev_hash) VALUES (?, ?, ?, ?, ?, \
         ?, ?, ?)"
      in
      let stmt = Sqlite3.prepare db insert_sql in
      let count = ref 0 in
      let signed_rows = ref 0 in
      let success = ref false in
      let ic = open_in file_path in
      let finalize_import success =
        ignore (Sqlite3.finalize stmt);
        close_in_noerr ic;
        ignore (Sqlite3.exec db (if success then "COMMIT" else "ROLLBACK"))
      in
      (match Sqlite3.exec db "BEGIN IMMEDIATE" with
      | Sqlite3.Rc.OK -> ()
      | rc ->
          raise
            (Failure
               (Printf.sprintf "Failed to start audit import transaction: %s"
                  (Sqlite3.Rc.to_string rc))));
      Fun.protect
        (fun () ->
          try
            while true do
              let line = input_line ic in
              if String.trim line <> "" then begin
                let json = Yojson.Safe.from_string line in
                let text name = json |> member name |> to_string in
                let text_opt name =
                  match json |> member name with
                  | `Null -> None
                  | `String s -> Some s
                  | _ ->
                      raise
                        (Failure
                           (Printf.sprintf "Invalid %s value in %s" name
                              file_path))
                in
                let bind_text idx value =
                  ignore (Sqlite3.bind stmt idx (Sqlite3.Data.TEXT value))
                in
                bind_text 1 (text "timestamp");
                bind_text 2 (text "event_type");
                bind_opt stmt 3 (text_opt "session_key");
                bind_opt stmt 4 (text_opt "details");
                bind_opt stmt 5 (text_opt "tool_name");
                bind_opt stmt 6 (text_opt "risk_level");
                let signature = text_opt "signature" in
                (match signature with Some _ -> incr signed_rows | None -> ());
                bind_opt stmt 7 signature;
                bind_opt stmt 8 (text_opt "prev_hash");
                (match Sqlite3.step stmt with
                | Sqlite3.Rc.DONE -> incr count
                | rc ->
                    raise
                      (Failure
                         (Printf.sprintf "Audit import failed: %s"
                            (Sqlite3.Rc.to_string rc))));
                ignore (Sqlite3.reset stmt);
                ignore (Sqlite3.clear_bindings stmt)
              end
            done;
            assert false
          with End_of_file ->
            (match (anchor, !signed_rows) with
            | Some _, 0 ->
                raise
                  (Failure
                     "Audit import anchor requires at least one signed row")
            | _ -> ());
            set_chain_anchor ~db anchor;
            (if !signed_rows > 0 then
               match get_signing_key () with
               | Ok key -> (
                   match verify_chain ~db ~key with
                   | Ok () -> ()
                   | Error (id, reason) ->
                       raise
                         (Failure
                            (Printf.sprintf
                               "Imported audit chain failed verification at \
                                id=%d: %s"
                               id reason)))
               | Error msg ->
                   raise
                     (Failure
                        (Printf.sprintf
                           "Signed audit import requires verification key: %s"
                           msg)));
            success := true)
        ~finally:(fun () -> finalize_import !success);
      Ok (!count, resolved_anchor_path)
    with
    | Failure msg -> Error msg
    | Yojson.Json_error msg ->
        Error (Printf.sprintf "Invalid audit JSON: %s" msg)
    | Yojson.Safe.Util.Type_error (msg, _) -> Error msg
    | Sys_error msg -> Error msg

(* Retention tick: export if configured, then purge *)
let retention_tick ~db ~(config : Runtime_config.t) =
  let ret = config.security.audit_retention in
  if ret.export_before_purge then begin
    let timestamp = string_of_float (Unix.gettimeofday ()) in
    let path =
      Filename.concat ret.export_path
        (Printf.sprintf "audit_export_%s.jsonl" timestamp)
    in
    let count = export_json ~db ~path in
    Logs.info (fun m -> m "Audit export: %d entries to %s" count path)
  end;
  let deleted =
    purge_old ~db ~max_age_days:ret.max_age_days ~max_entries:ret.max_entries
  in
  if deleted > 0 then
    Logs.info (fun m -> m "Audit retention: purged %d entries" deleted);
  deleted

let query ~db ?event_type ?session_key ~limit () =
  let conditions = ref [] in
  let params = ref [] in
  (match event_type with
  | Some et ->
      conditions := "event_type = ?" :: !conditions;
      params := et :: !params
  | None -> ());
  (match session_key with
  | Some sk ->
      conditions := "session_key = ?" :: !conditions;
      params := sk :: !params
  | None -> ());
  let where =
    match !conditions with
    | [] -> ""
    | conds -> " WHERE " ^ String.concat " AND " (List.rev conds)
  in
  let sql =
    Printf.sprintf
      "SELECT id, timestamp, event_type, session_key, details, tool_name, \
       risk_level FROM audit_log%s ORDER BY id DESC LIMIT ?"
      where
  in
  let stmt = Sqlite3.prepare db sql in
  let idx = ref 1 in
  List.iter
    (fun p ->
      ignore (Sqlite3.bind stmt !idx (Sqlite3.Data.TEXT p));
      incr idx)
    (List.rev !params);
  ignore (Sqlite3.bind stmt !idx (Sqlite3.Data.INT (Int64.of_int limit)));
  let rows = ref [] in
  while Sqlite3.step stmt = Sqlite3.Rc.ROW do
    let text_opt i =
      match Sqlite3.column stmt i with
      | Sqlite3.Data.TEXT s -> Some s
      | _ -> None
    in
    let id =
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT i -> Int64.to_int i
      | _ -> 0
    in
    let timestamp =
      match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    let event_type =
      match Sqlite3.column stmt 2 with Sqlite3.Data.TEXT s -> s | _ -> ""
    in
    rows :=
      {
        id;
        timestamp;
        event_type;
        session_key = text_opt 3;
        details = text_opt 4;
        tool_name = text_opt 5;
        risk_level = text_opt 6;
      }
      :: !rows
  done;
  ignore (Sqlite3.finalize stmt);
  List.rev !rows

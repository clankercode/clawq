(* Global override: use minimal iteration count for faster tests. This affects
   the entire audit test suite and is intentional. *)
let () = Audit.test_iterations_override := Some 1

let test_audit_schema_init () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  (* Should not fail on double init *)
  Audit.init_schema db

let test_audit_log_and_query () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  Audit.log ~db
    (ChatMessage
       { session_key = "s1"; role = "user"; content_preview = "hello world" });
  Audit.log ~db
    (ToolInvocation
       {
         session_key = "s1";
         tool_name = "shell_exec";
         risk_level = "high";
         args_preview = "{\"command\":\"ls\"}";
       });
  Audit.log ~db
    (ToolResult { session_key = "s1"; tool_name = "shell_exec"; success = true });
  Audit.log ~db (DaemonEvent { action = "start"; details = "pid=1234" });
  let rows = Audit.query ~db ~limit:10 () in
  Alcotest.(check int) "4 audit entries" 4 (List.length rows)

let test_audit_query_filter () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  Audit.log ~db
    (ChatMessage { session_key = "s1"; role = "user"; content_preview = "msg1" });
  Audit.log ~db
    (ChatMessage { session_key = "s2"; role = "user"; content_preview = "msg2" });
  Audit.log ~db
    (ToolInvocation
       {
         session_key = "s1";
         tool_name = "file_read";
         risk_level = "low";
         args_preview = "{}";
       });
  let chat_rows = Audit.query ~db ~event_type:"chat_message" ~limit:10 () in
  Alcotest.(check int) "2 chat entries" 2 (List.length chat_rows);
  let s1_rows = Audit.query ~db ~session_key:"s1" ~limit:10 () in
  Alcotest.(check int) "2 s1 entries" 2 (List.length s1_rows)

let test_audit_config_change () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  Audit.log ~db
    (ConfigChange
       { field = "model"; old_value = "gpt-3.5"; new_value = "gpt-4" });
  let rows = Audit.query ~db ~event_type:"config_change" ~limit:10 () in
  Alcotest.(check int) "1 config change" 1 (List.length rows);
  let row : Audit.row = List.hd rows in
  Alcotest.(check bool)
    "details contains field" true
    (match row.details with
    | Some d -> (
        String.length d > 0
        &&
        let re = Str.regexp_string "model" in
        try
          ignore (Str.search_forward re d 0);
          true
        with Not_found -> false)
    | None -> false)

(* --- Retention tests --- *)

let test_purge_by_age () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  (* Insert old entries directly *)
  let sql =
    "INSERT INTO audit_log (timestamp, event_type, details) VALUES \
     (datetime('now', '-100 days'), 'test', 'old entry 1')"
  in
  ignore (Sqlite3.exec db sql);
  let sql2 =
    "INSERT INTO audit_log (timestamp, event_type, details) VALUES \
     (datetime('now', '-100 days'), 'test', 'old entry 2')"
  in
  ignore (Sqlite3.exec db sql2);
  (* Insert a recent entry *)
  Audit.log ~db (DaemonEvent { action = "test"; details = "recent" });
  let before = Audit.query ~db ~limit:100 () in
  Alcotest.(check int) "3 entries before purge" 3 (List.length before);
  let deleted = Audit.purge_old ~db ~max_age_days:30 ~max_entries:1000000 in
  Alcotest.(check int) "2 old entries deleted" 2 deleted;
  let after = Audit.query ~db ~limit:100 () in
  Alcotest.(check int) "1 entry after purge" 1 (List.length after)

let test_purge_by_count () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  for i = 1 to 10 do
    Audit.log ~db
      (DaemonEvent { action = "test"; details = Printf.sprintf "entry %d" i })
  done;
  let before = Audit.query ~db ~limit:100 () in
  Alcotest.(check int) "10 entries before purge" 10 (List.length before);
  let deleted = Audit.purge_old ~db ~max_age_days:9999 ~max_entries:3 in
  Alcotest.(check int) "7 entries deleted" 7 deleted;
  let after = Audit.query ~db ~limit:100 () in
  Alcotest.(check int) "3 entries after purge" 3 (List.length after);
  (* Verify newest entries remain - query returns newest first *)
  let newest = List.hd after in
  Alcotest.(check bool)
    "newest entry is entry 10" true
    (match newest.details with Some d -> d = "test: entry 10" | None -> false)

let test_export_jsonl () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  Audit.log ~db (DaemonEvent { action = "test"; details = "export1" });
  Audit.log ~db (DaemonEvent { action = "test"; details = "export2" });
  let tmpdir = Filename.temp_dir "clawq_test" "" in
  let export_path = Filename.concat tmpdir "export.jsonl" in
  let count = Audit.export_json ~db ~path:export_path in
  Alcotest.(check int) "exported 2 entries" 2 count;
  let ic = open_in export_path in
  let lines = ref [] in
  (try
     while true do
       lines := input_line ic :: !lines
     done
   with End_of_file -> ());
  close_in ic;
  let lines = List.rev !lines in
  Alcotest.(check int) "2 lines in file" 2 (List.length lines);
  (* Verify each line is valid JSON *)
  List.iter
    (fun line ->
      let _json = Yojson.Safe.from_string line in
      ())
    lines;
  let anchor_json = Yojson.Safe.from_file (export_path ^ ".anchor.json") in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "anchor sidecar format" "clawq-audit-anchor-v1"
    (anchor_json |> member "format" |> to_string);
  Alcotest.(check bool)
    "unsigned export has null anchor" true
    (match anchor_json |> member "chain_anchor_signature" with
    | `Null -> true
    | _ -> false);
  (* Cleanup *)
  Sys.remove (export_path ^ ".anchor.json");
  Sys.remove export_path;
  try Sys.rmdir tmpdir with _ -> ()

let test_key = "test-signing-key-for-audit-tests"

let insert_signed_row ?session_key ?tool_name ?risk_level ~db ~key ~timestamp
    ~event_type ~details ~last_sig () =
  let prev_hash = Audit.compute_prev_hash !last_sig in
  let signature =
    Audit.compute_signature ~key ~prev_hash ~timestamp ~event_type ~session_key
      ~details_str:details ~tool_name ~risk_level
  in
  let stmt =
    Sqlite3.prepare db
      "INSERT INTO audit_log (timestamp, event_type, session_key, details, \
       tool_name, risk_level, signature, prev_hash) VALUES (?, ?, ?, ?, ?, ?, \
       ?, ?)"
  in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT timestamp));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT event_type));
  Audit.bind_opt stmt 3 session_key;
  ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT details));
  Audit.bind_opt stmt 5 tool_name;
  Audit.bind_opt stmt 6 risk_level;
  ignore (Sqlite3.bind stmt 7 (Sqlite3.Data.TEXT signature));
  ignore (Sqlite3.bind stmt 8 (Sqlite3.Data.TEXT prev_hash));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  last_sig := Some signature

let insert_legacy_signed_row ?session_key ?tool_name ?risk_level ~db ~key
    ~timestamp ~event_type ~details ~last_sig () =
  let prev_hash = Audit.compute_prev_hash !last_sig in
  let payload = prev_hash ^ timestamp ^ event_type ^ details in
  let signature = Digestif.SHA256.(hmac_string ~key payload |> to_hex) in
  let stmt =
    Sqlite3.prepare db
      "INSERT INTO audit_log (timestamp, event_type, session_key, details, \
       tool_name, risk_level, signature, prev_hash) VALUES (?, ?, ?, ?, ?, ?, \
       ?, ?)"
  in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT timestamp));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT event_type));
  Audit.bind_opt stmt 3 session_key;
  ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT details));
  Audit.bind_opt stmt 5 tool_name;
  Audit.bind_opt stmt 6 risk_level;
  ignore (Sqlite3.bind stmt 7 (Sqlite3.Data.TEXT signature));
  ignore (Sqlite3.bind stmt 8 (Sqlite3.Data.TEXT prev_hash));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  last_sig := Some signature

let check_verified ~db ~key label =
  match Audit.verify_chain ~db ~key with
  | Ok () -> ()
  | Error (id, reason) ->
      Alcotest.fail (Printf.sprintf "%s, failed at id=%d: %s" label id reason)

let test_export_jsonl_preserves_anchor_after_purge () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let key = Audit.derive_signing_key test_key in
  Audit.log_signed ~db ~key (DaemonEvent { action = "e1"; details = "first" });
  Audit.log_signed ~db ~key (DaemonEvent { action = "e2"; details = "second" });
  Audit.log_signed ~db ~key (DaemonEvent { action = "e3"; details = "third" });
  ignore (Audit.purge_old ~db ~max_age_days:9999 ~max_entries:2);
  let tmpdir = Filename.temp_dir "clawq_test" "" in
  let export_path = Filename.concat tmpdir "export_after_purge.jsonl" in
  let count = Audit.export_json ~db ~path:export_path in
  Alcotest.(check int) "2 retained entries exported" 2 count;
  let anchor_json = Yojson.Safe.from_file (export_path ^ ".anchor.json") in
  let open Yojson.Safe.Util in
  let anchor_sig =
    anchor_json |> member "chain_anchor_signature" |> to_string
  in
  Alcotest.(check bool)
    "purged export keeps anchor signature" true
    (String.length anchor_sig > 0);
  Sys.remove (export_path ^ ".anchor.json");
  Sys.remove export_path;
  try Sys.rmdir tmpdir with _ -> ()

let test_purge_by_age_keeps_contiguous_suffix_for_signed_chain () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let key = Audit.derive_signing_key test_key in
  let last_sig = ref None in
  insert_signed_row ~db ~key ~timestamp:"2030-01-01 00:00:00"
    ~event_type:"daemon_event" ~details:"e1: recent but older id" ~last_sig ();
  insert_signed_row ~db ~key ~timestamp:"2000-01-01 00:00:00"
    ~event_type:"daemon_event" ~details:"e2: old boundary" ~last_sig ();
  insert_signed_row ~db ~key ~timestamp:"2030-01-03 00:00:00"
    ~event_type:"daemon_event" ~details:"e3: newest retained" ~last_sig ();
  let deleted = Audit.purge_old ~db ~max_age_days:30 ~max_entries:10 in
  Alcotest.(check int) "two entries purged" 2 deleted;
  let rows = Audit.query ~db ~limit:10 () in
  Alcotest.(check int) "one entry remains" 1 (List.length rows);
  Alcotest.(check string)
    "newest contiguous suffix retained" "e3: newest retained"
    (match (List.hd rows).details with Some d -> d | None -> "");
  match Audit.verify_chain ~db ~key with
  | Ok () -> ()
  | Error (id, reason) ->
      Alcotest.fail
        (Printf.sprintf
           "Expected verification after non-monotone age purge, failed at \
            id=%d: %s"
           id reason)

let test_retention_tick () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  (* Insert old entries *)
  let sql =
    "INSERT INTO audit_log (timestamp, event_type, details) VALUES \
     (datetime('now', '-200 days'), 'test', 'very old')"
  in
  ignore (Sqlite3.exec db sql);
  Audit.log ~db (DaemonEvent { action = "test"; details = "recent" });
  let config =
    {
      Runtime_config.default with
      security =
        {
          Runtime_config.default.security with
          audit_enabled = true;
          audit_retention =
            {
              max_age_days = 30;
              max_entries = 1000000;
              export_before_purge = false;
              export_path = "/tmp";
            };
        };
    }
  in
  let deleted = Audit.retention_tick ~db ~config in
  Alcotest.(check int) "1 old entry purged" 1 deleted;
  let after = Audit.query ~db ~limit:100 () in
  Alcotest.(check int) "1 entry remains" 1 (List.length after)

(* --- Signing tests --- *)

let test_signed_valid_chain () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let key = Audit.derive_signing_key test_key in
  Audit.log_signed ~db ~key
    (DaemonEvent { action = "start"; details = "pid=1" });
  Audit.log_signed ~db ~key
    (ChatMessage
       { session_key = "s1"; role = "user"; content_preview = "hello" });
  Audit.log_signed ~db ~key (DaemonEvent { action = "stop"; details = "clean" });
  match Audit.verify_chain ~db ~key with
  | Ok () -> ()
  | Error (id, reason) ->
      Alcotest.fail
        (Printf.sprintf "Chain verify failed at id=%d: %s" id reason)

let test_verify_chain_append_preserves_validity () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let key = Audit.derive_signing_key test_key in
  let last_sig = ref None in
  insert_signed_row ~db ~key ~timestamp:"2030-01-01 00:00:00"
    ~event_type:"daemon_event" ~details:"append-one" ~last_sig ();
  insert_signed_row ~db ~key ~timestamp:"2030-01-01 00:00:01"
    ~event_type:"daemon_event" ~details:"append-two" ~last_sig ();
  check_verified ~db ~key
    "Expected verify_chain_append baseline chain to verify";
  insert_signed_row ~db ~key ~timestamp:"2030-01-01 00:00:02"
    ~event_type:"daemon_event" ~details:"append-three" ~last_sig ();
  check_verified ~db ~key
    "Expected verify_chain_append appended chain to verify"

let test_build_chain_valid_from_sequential_signed_events () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let key = Audit.derive_signing_key test_key in
  let last_sig = ref None in
  insert_signed_row ~db ~key ~timestamp:"2030-02-01 00:00:00"
    ~event_type:"daemon_event" ~details:"build-one" ~last_sig ();
  insert_signed_row ~db ~key ~timestamp:"2030-02-01 00:00:01"
    ~event_type:"daemon_event" ~details:"build-two" ~last_sig ();
  insert_signed_row ~db ~key ~timestamp:"2030-02-01 00:00:02"
    ~event_type:"daemon_event" ~details:"build-three" ~last_sig ();
  Alcotest.(check int)
    "three sequential signed events inserted" 3
    (List.length (Audit.query ~db ~limit:10 ()));
  check_verified ~db ~key
    "Expected build_chain_valid / verify_chain_build sequential chain to verify"

let test_suffix_monotonicity_retained_suffix_verifies_with_anchor () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let key = Audit.derive_signing_key test_key in
  Audit.log_signed ~db ~key (DaemonEvent { action = "e1"; details = "first" });
  Audit.log_signed ~db ~key (DaemonEvent { action = "e2"; details = "second" });
  Audit.log_signed ~db ~key (DaemonEvent { action = "e3"; details = "third" });
  Audit.log_signed ~db ~key (DaemonEvent { action = "e4"; details = "fourth" });
  let deleted = Audit.purge_old ~db ~max_age_days:9999 ~max_entries:2 in
  Alcotest.(check int) "two entries purged" 2 deleted;
  let rows = Audit.query ~db ~limit:10 () in
  Alcotest.(check (list string))
    "retained suffix stays newest two entries"
    [ "e4: fourth"; "e3: third" ]
    (List.filter_map (fun (row : Audit.row) -> row.details) rows);
  check_verified ~db ~key
    "Expected suffix_monotonicity retained suffix to verify with anchor"

let test_signed_tampered_entry () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let key = Audit.derive_signing_key test_key in
  Audit.log_signed ~db ~key
    (DaemonEvent { action = "start"; details = "pid=1" });
  Audit.log_signed ~db ~key
    (DaemonEvent { action = "test"; details = "original" });
  Audit.log_signed ~db ~key (DaemonEvent { action = "stop"; details = "clean" });
  (* Tamper with entry 2 *)
  ignore
    (Sqlite3.exec db "UPDATE audit_log SET details = 'tampered' WHERE id = 2");
  match Audit.verify_chain ~db ~key with
  | Ok () -> Alcotest.fail "Expected verification to fail after tamper"
  | Error (id, _reason) -> Alcotest.(check int) "tampered entry id" 2 id

let test_signed_tampered_metadata () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let key = Audit.derive_signing_key test_key in
  Audit.log_signed ~db ~key
    (ToolInvocation
       {
         session_key = "s1";
         tool_name = "shell_exec";
         risk_level = "high";
         args_preview = "{\"command\":\"ls\"}";
       });
  ignore
    (Sqlite3.exec db "UPDATE audit_log SET tool_name = 'file_read' WHERE id = 1");
  match Audit.verify_chain ~db ~key with
  | Ok () -> Alcotest.fail "Expected verification to fail after metadata tamper"
  | Error (id, _reason) -> Alcotest.(check int) "tampered entry id" 1 id

let test_signed_deleted_entry () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let key = Audit.derive_signing_key test_key in
  Audit.log_signed ~db ~key (DaemonEvent { action = "e1"; details = "first" });
  Audit.log_signed ~db ~key (DaemonEvent { action = "e2"; details = "second" });
  Audit.log_signed ~db ~key (DaemonEvent { action = "e3"; details = "third" });
  (* Delete middle entry *)
  ignore (Sqlite3.exec db "DELETE FROM audit_log WHERE id = 2");
  match Audit.verify_chain ~db ~key with
  | Ok () -> Alcotest.fail "Expected verification to fail after delete"
  | Error (id, reason) ->
      Alcotest.(check int) "broken at entry 3" 3 id;
      Alcotest.(check bool)
        "prev_hash mismatch" true
        (let re = Str.regexp_string "prev_hash mismatch" in
         try
           ignore (Str.search_forward re reason 0);
           true
         with Not_found -> false)

let test_signed_purge_preserves_anchored_verification () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let key = Audit.derive_signing_key test_key in
  Audit.log_signed ~db ~key (DaemonEvent { action = "e1"; details = "first" });
  Audit.log_signed ~db ~key (DaemonEvent { action = "e2"; details = "second" });
  Audit.log_signed ~db ~key (DaemonEvent { action = "e3"; details = "third" });
  let deleted = Audit.purge_old ~db ~max_age_days:9999 ~max_entries:2 in
  Alcotest.(check int) "one entry purged" 1 deleted;
  match Audit.verify_chain ~db ~key with
  | Ok () -> ()
  | Error (id, reason) ->
      Alcotest.fail
        (Printf.sprintf
           "Expected anchored verification after purge, failed at id=%d: %s" id
           reason)

let test_signed_purge_preserves_mixed_boundary_verification () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let key = Audit.derive_signing_key test_key in
  Audit.log_signed ~db ~key (DaemonEvent { action = "e1"; details = "first" });
  Audit.log ~db (DaemonEvent { action = "u1"; details = "unsigned" });
  Audit.log_signed ~db ~key (DaemonEvent { action = "e2"; details = "second" });
  Audit.log_signed ~db ~key (DaemonEvent { action = "e3"; details = "third" });
  let deleted = Audit.purge_old ~db ~max_age_days:9999 ~max_entries:3 in
  Alcotest.(check int) "one entry purged" 1 deleted;
  let rows = Audit.query ~db ~limit:10 () in
  Alcotest.(check int) "three entries remain" 3 (List.length rows);
  match Audit.verify_chain ~db ~key with
  | Ok () -> ()
  | Error (id, reason) ->
      Alcotest.fail
        (Printf.sprintf
           "Expected anchored verification with unsigned boundary, failed at \
            id=%d: %s"
           id reason)

let test_import_json_restores_retained_anchor () =
  let source_db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema source_db;
  let old_key = Sys.getenv_opt "CLAWQ_MASTER_KEY" in
  let restore_key () =
    match old_key with
    | Some v -> Unix.putenv "CLAWQ_MASTER_KEY" v
    | None -> Unix.putenv "CLAWQ_MASTER_KEY" ""
  in
  let key = Audit.derive_signing_key test_key in
  Fun.protect
    (fun () ->
      Unix.putenv "CLAWQ_MASTER_KEY" test_key;
      Audit.log_signed ~db:source_db ~key
        (DaemonEvent { action = "e1"; details = "first" });
      Audit.log ~db:source_db
        (DaemonEvent { action = "u1"; details = "unsigned" });
      Audit.log_signed ~db:source_db ~key
        (DaemonEvent { action = "e2"; details = "second" });
      Audit.log_signed ~db:source_db ~key
        (DaemonEvent { action = "e3"; details = "third" });
      ignore (Audit.purge_old ~db:source_db ~max_age_days:9999 ~max_entries:3);
      let tmpdir = Filename.temp_dir "clawq_test" "" in
      let export_path = Filename.concat tmpdir "retained.jsonl" in
      let export_count = Audit.export_json ~db:source_db ~path:export_path in
      Alcotest.(check int) "three retained entries exported" 3 export_count;
      let restored_db = Memory.init ~db_path:":memory:" () in
      Audit.init_schema restored_db;
      (match Audit.import_json ~db:restored_db ~path:export_path () with
      | Ok (count, Some anchor_path) ->
          Alcotest.(check int) "three entries imported" 3 count;
          Alcotest.(check string)
            "default anchor path used"
            (export_path ^ ".anchor.json")
            anchor_path
      | Ok (_, None) -> Alcotest.fail "Expected retained-chain anchor sidecar"
      | Error msg -> Alcotest.failf "Import failed: %s" msg);
      match Audit.verify_chain ~db:restored_db ~key with
      | Ok () -> (
          Sys.remove (export_path ^ ".anchor.json");
          Sys.remove export_path;
          try Sys.rmdir tmpdir with _ -> ())
      | Error (id, reason) ->
          Sys.remove (export_path ^ ".anchor.json");
          Sys.remove export_path;
          (try Sys.rmdir tmpdir with _ -> ());
          Alcotest.fail
            (Printf.sprintf
               "Expected restored retained chain to verify, failed at id=%d: %s"
               id reason))
    ~finally:restore_key

let test_import_json_requires_key_for_signed_rows () =
  let source_db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema source_db;
  let old_key = Sys.getenv_opt "CLAWQ_MASTER_KEY" in
  let restore_key () =
    match old_key with
    | Some v -> Unix.putenv "CLAWQ_MASTER_KEY" v
    | None -> Unix.putenv "CLAWQ_MASTER_KEY" ""
  in
  let key = Audit.derive_signing_key test_key in
  Audit.log_signed ~db:source_db ~key
    (DaemonEvent { action = "e1"; details = "first" });
  let tmpdir = Filename.temp_dir "clawq_test" "" in
  let export_path = Filename.concat tmpdir "signed.jsonl" in
  let _ = Audit.export_json ~db:source_db ~path:export_path in
  let restored_db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema restored_db;
  Fun.protect
    (fun () ->
      Unix.putenv "CLAWQ_MASTER_KEY" "";
      match Audit.import_json ~db:restored_db ~path:export_path () with
      | Ok _ -> Alcotest.fail "Expected signed import without key to fail"
      | Error msg ->
          Alcotest.(check bool)
            "reports missing verification key" true
            (String.length msg > 0))
    ~finally:(fun () ->
      restore_key ();
      Sys.remove (export_path ^ ".anchor.json");
      Sys.remove export_path;
      try Sys.rmdir tmpdir with _ -> ())

let test_import_json_rejects_malformed_anchor () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let tmpdir = Filename.temp_dir "clawq_test" "" in
  let export_path = Filename.concat tmpdir "bad.jsonl" in
  let anchor_path = export_path ^ ".anchor.json" in
  let oc = open_out export_path in
  close_out oc;
  let oc = open_out anchor_path in
  output_string oc {|{"format":123}|};
  close_out oc;
  (match Audit.import_json ~db ~path:export_path () with
  | Ok _ -> Alcotest.fail "Expected malformed anchor import to fail"
  | Error msg ->
      Alcotest.(check bool)
        "reports malformed anchor" true
        (String.length msg > 0));
  Sys.remove anchor_path;
  Sys.remove export_path;
  try Sys.rmdir tmpdir with _ -> ()

let test_import_json_rejects_malformed_row () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let tmpdir = Filename.temp_dir "clawq_test" "" in
  let path = Filename.concat tmpdir "bad_row.jsonl" in
  let oc = open_out path in
  output_string oc {|{"timestamp":123}|};
  output_char oc '\n';
  close_out oc;
  (match Audit.import_json ~db ~path () with
  | Ok _ -> Alcotest.fail "Expected malformed row import to fail"
  | Error msg ->
      Alcotest.(check bool) "reports malformed row" true (String.length msg > 0));
  Sys.remove path;
  try Sys.rmdir tmpdir with _ -> ()

let test_import_json_rejects_tampered_signed_chain_when_key_available () =
  let source_db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema source_db;
  let old_key = Sys.getenv_opt "CLAWQ_MASTER_KEY" in
  let restore_key () =
    match old_key with
    | Some v -> Unix.putenv "CLAWQ_MASTER_KEY" v
    | None -> Unix.putenv "CLAWQ_MASTER_KEY" ""
  in
  Fun.protect
    (fun () ->
      Unix.putenv "CLAWQ_MASTER_KEY" test_key;
      let key = Audit.derive_signing_key test_key in
      Audit.log_signed ~db:source_db ~key
        (DaemonEvent { action = "e1"; details = "first" });
      Audit.log_signed ~db:source_db ~key
        (DaemonEvent { action = "e2"; details = "second" });
      let tmpdir = Filename.temp_dir "clawq_test" "" in
      let export_path = Filename.concat tmpdir "tampered.jsonl" in
      let _ = Audit.export_json ~db:source_db ~path:export_path in
      let lines = In_channel.with_open_bin export_path In_channel.input_lines in
      let tampered_lines =
        match lines with
        | first :: second :: rest ->
            let second_json = Yojson.Safe.from_string second in
            let open Yojson.Safe.Util in
            let tampered =
              `Assoc
                [
                  ("timestamp", second_json |> member "timestamp");
                  ("event_type", second_json |> member "event_type");
                  ("session_key", second_json |> member "session_key");
                  ("details", `String "tampered");
                  ("tool_name", second_json |> member "tool_name");
                  ("risk_level", second_json |> member "risk_level");
                  ("signature", second_json |> member "signature");
                  ("prev_hash", second_json |> member "prev_hash");
                ]
            in
            first :: Yojson.Safe.to_string tampered :: rest
        | _ -> Alcotest.fail "Expected two exported rows"
      in
      let oc = open_out export_path in
      List.iter
        (fun line ->
          output_string oc line;
          output_char oc '\n')
        tampered_lines;
      close_out oc;
      let restored_db = Memory.init ~db_path:":memory:" () in
      Audit.init_schema restored_db;
      (match Audit.import_json ~db:restored_db ~path:export_path () with
      | Ok _ -> Alcotest.fail "Expected tampered signed import to fail"
      | Error msg ->
          Alcotest.(check bool)
            "reports verification failure" true
            (String.length msg > 0));
      Alcotest.(check int)
        "failed import rolls back rows" 0
        (List.length (Audit.query ~db:restored_db ~limit:10 ()));
      Sys.remove (export_path ^ ".anchor.json");
      Sys.remove export_path;
      try Sys.rmdir tmpdir with _ -> ())
    ~finally:restore_key

let test_verify_chain_rejects_missing_signature_with_prev_hash () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let key = Audit.derive_signing_key test_key in
  Audit.log_signed ~db ~key (DaemonEvent { action = "e1"; details = "first" });
  Audit.log_signed ~db ~key (DaemonEvent { action = "e2"; details = "second" });
  ignore
    (Sqlite3.exec db
       "UPDATE audit_log SET signature = NULL WHERE id = 2 AND prev_hash IS \
        NOT NULL");
  match Audit.verify_chain ~db ~key with
  | Ok () -> Alcotest.fail "Expected missing signature with prev_hash to fail"
  | Error (id, reason) ->
      Alcotest.(check int) "broken at downgraded entry" 2 id;
      Alcotest.(check bool)
        "reports unexpected prev_hash on unsigned row" true
        (let re = Str.regexp_string "unexpectedly carries prev_hash" in
         try
           ignore (Str.search_forward re reason 0);
           true
         with Not_found -> false)

let test_import_json_rejects_missing_signature_with_prev_hash () =
  let source_db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema source_db;
  let old_key = Sys.getenv_opt "CLAWQ_MASTER_KEY" in
  let restore_key () =
    match old_key with
    | Some v -> Unix.putenv "CLAWQ_MASTER_KEY" v
    | None -> Unix.putenv "CLAWQ_MASTER_KEY" ""
  in
  Fun.protect
    (fun () ->
      Unix.putenv "CLAWQ_MASTER_KEY" test_key;
      let key = Audit.derive_signing_key test_key in
      Audit.log_signed ~db:source_db ~key
        (DaemonEvent { action = "e1"; details = "first" });
      Audit.log_signed ~db:source_db ~key
        (DaemonEvent { action = "e2"; details = "second" });
      let tmpdir = Filename.temp_dir "clawq_test" "" in
      let export_path = Filename.concat tmpdir "downgraded.jsonl" in
      let _ = Audit.export_json ~db:source_db ~path:export_path in
      let lines = In_channel.with_open_bin export_path In_channel.input_lines in
      let tampered_lines =
        match lines with
        | first :: second :: rest ->
            let second_json = Yojson.Safe.from_string second in
            let open Yojson.Safe.Util in
            let downgraded =
              `Assoc
                [
                  ("timestamp", second_json |> member "timestamp");
                  ("event_type", second_json |> member "event_type");
                  ("session_key", second_json |> member "session_key");
                  ("details", second_json |> member "details");
                  ("tool_name", second_json |> member "tool_name");
                  ("risk_level", second_json |> member "risk_level");
                  ("signature", `Null);
                  ("prev_hash", second_json |> member "prev_hash");
                ]
            in
            first :: Yojson.Safe.to_string downgraded :: rest
        | _ -> Alcotest.fail "Expected two exported rows"
      in
      let oc = open_out export_path in
      List.iter
        (fun line ->
          output_string oc line;
          output_char oc '\n')
        tampered_lines;
      close_out oc;
      let restored_db = Memory.init ~db_path:":memory:" () in
      Audit.init_schema restored_db;
      (match Audit.import_json ~db:restored_db ~path:export_path () with
      | Ok _ ->
          Alcotest.fail "Expected downgraded signed import to fail verification"
      | Error msg ->
          Alcotest.(check bool)
            "reports missing signature verification failure" true
            (let re = Str.regexp_string "unexpectedly carries prev_hash" in
             try
               ignore (Str.search_forward re msg 0);
               true
             with Not_found -> false));
      Alcotest.(check int)
        "downgraded import rolls back rows" 0
        (List.length (Audit.query ~db:restored_db ~limit:10 ()));
      Sys.remove (export_path ^ ".anchor.json");
      Sys.remove export_path;
      try Sys.rmdir tmpdir with _ -> ())
    ~finally:restore_key

let test_import_json_rejects_anchor_without_signed_rows () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let tmpdir = Filename.temp_dir "clawq_test" "" in
  let path = Filename.concat tmpdir "unsigned.jsonl" in
  let anchor_path = path ^ ".anchor.json" in
  let oc = open_out path in
  output_string oc
    {|{"timestamp":"2030-01-01 00:00:00","event_type":"daemon_event","session_key":null,"details":"note","tool_name":null,"risk_level":null,"signature":null,"prev_hash":null}|};
  output_char oc '\n';
  close_out oc;
  let oc = open_out anchor_path in
  output_string oc
    {|{"format":"clawq-audit-anchor-v1","chain_anchor_signature":"forged-anchor"}|};
  close_out oc;
  (match Audit.import_json ~db ~path () with
  | Ok _ -> Alcotest.fail "Expected unsigned import with anchor to fail"
  | Error msg ->
      Alcotest.(check bool)
        "reports anchor requires signed row" true
        (let re = Str.regexp_string "anchor requires at least one signed row" in
         try
           ignore (Str.search_forward re msg 0);
           true
         with Not_found -> false));
  Alcotest.(check int)
    "unsigned anchored import rolls back rows" 0
    (List.length (Audit.query ~db ~limit:10 ()));
  Sys.remove anchor_path;
  Sys.remove path;
  try Sys.rmdir tmpdir with _ -> ()

let test_signed_genesis () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let key = Audit.derive_signing_key test_key in
  Audit.log_signed ~db ~key
    (DaemonEvent { action = "first"; details = "genesis test" });
  let stmt =
    Sqlite3.prepare db "SELECT prev_hash FROM audit_log WHERE id = 1"
  in
  let prev_hash =
    if Sqlite3.step stmt = Sqlite3.Rc.ROW then
      match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
    else ""
  in
  ignore (Sqlite3.finalize stmt);
  Alcotest.(check string) "first entry prev_hash is genesis" "genesis" prev_hash

let test_verify_chain_accepts_legacy_signatures () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let key = Audit.derive_signing_key test_key in
  let last_sig = ref None in
  insert_legacy_signed_row ~db ~key ~timestamp:"2030-01-01 00:00:00"
    ~event_type:"daemon_event" ~details:"legacy-one" ~last_sig ();
  insert_legacy_signed_row ~db ~key ~timestamp:"2030-01-01 00:00:01"
    ~event_type:"daemon_event" ~details:"legacy-two" ~last_sig ();
  match Audit.verify_chain ~db ~key with
  | Ok () -> ()
  | Error (id, reason) ->
      Alcotest.fail
        (Printf.sprintf
           "Expected legacy signed rows to verify, failed at id=%d: %s" id
           reason)

let test_verify_chain_rejects_legacy_signature_with_metadata () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let key = Audit.derive_signing_key test_key in
  let prev_hash = Audit.compute_prev_hash None in
  let signature =
    Audit.compute_signature_legacy ~key ~prev_hash
      ~timestamp:"2030-01-01 00:00:00" ~event_type:"tool_result"
      ~details_str:"success"
  in
  let stmt =
    Sqlite3.prepare db
      "INSERT INTO audit_log (timestamp, event_type, session_key, details, \
       tool_name, risk_level, signature, prev_hash) VALUES (?, ?, ?, ?, ?, ?, \
       ?, ?)"
  in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT "2030-01-01 00:00:00"));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT "tool_result"));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT "s1"));
  ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT "success"));
  ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT "shell_exec"));
  ignore (Sqlite3.bind stmt 6 Sqlite3.Data.NULL);
  ignore (Sqlite3.bind stmt 7 (Sqlite3.Data.TEXT signature));
  ignore (Sqlite3.bind stmt 8 (Sqlite3.Data.TEXT prev_hash));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  match Audit.verify_chain ~db ~key with
  | Ok () ->
      Alcotest.fail "Expected legacy signature with metadata to be rejected"
  | Error (id, _reason) ->
      Alcotest.(check int) "rejects metadata-bearing row" 1 id

let test_unsigned_entries () =
  let db = Memory.init ~db_path:":memory:" () in
  Audit.init_schema db;
  let key = Audit.derive_signing_key test_key in
  (* Mix unsigned and signed entries *)
  Audit.log ~db (DaemonEvent { action = "unsigned1"; details = "no sig" });
  Audit.log_signed ~db ~key
    (DaemonEvent { action = "signed1"; details = "with sig" });
  Audit.log ~db (DaemonEvent { action = "unsigned2"; details = "no sig" });
  Audit.log_signed ~db ~key
    (DaemonEvent { action = "signed2"; details = "with sig" });
  match Audit.verify_chain ~db ~key with
  | Ok () -> ()
  | Error (id, reason) ->
      Alcotest.fail
        (Printf.sprintf "Chain verify failed at id=%d: %s" id reason)

let suite =
  [
    Alcotest.test_case "audit schema init" `Quick test_audit_schema_init;
    Alcotest.test_case "audit log and query" `Quick test_audit_log_and_query;
    Alcotest.test_case "audit query filter" `Quick test_audit_query_filter;
    Alcotest.test_case "audit config change" `Quick test_audit_config_change;
    Alcotest.test_case "purge by age" `Quick test_purge_by_age;
    Alcotest.test_case "purge by count" `Quick test_purge_by_count;
    Alcotest.test_case "export jsonl" `Quick test_export_jsonl;
    Alcotest.test_case "export jsonl preserves anchor after purge" `Quick
      test_export_jsonl_preserves_anchor_after_purge;
    Alcotest.test_case "purge by age keeps contiguous suffix for signed chain"
      `Quick test_purge_by_age_keeps_contiguous_suffix_for_signed_chain;
    Alcotest.test_case "retention tick" `Quick test_retention_tick;
    Alcotest.test_case "signed valid chain" `Quick test_signed_valid_chain;
    Alcotest.test_case "verify_chain_append preserves validity" `Quick
      test_verify_chain_append_preserves_validity;
    Alcotest.test_case "build_chain_valid sequential signed events" `Quick
      test_build_chain_valid_from_sequential_signed_events;
    Alcotest.test_case "suffix_monotonicity retained suffix verifies" `Quick
      test_suffix_monotonicity_retained_suffix_verifies_with_anchor;
    Alcotest.test_case "signed tampered entry" `Quick test_signed_tampered_entry;
    Alcotest.test_case "signed tampered metadata" `Quick
      test_signed_tampered_metadata;
    Alcotest.test_case "signed deleted entry" `Quick test_signed_deleted_entry;
    Alcotest.test_case "signed purge preserves anchored verification" `Quick
      test_signed_purge_preserves_anchored_verification;
    Alcotest.test_case "signed purge preserves mixed boundary verification"
      `Quick test_signed_purge_preserves_mixed_boundary_verification;
    Alcotest.test_case "import json restores retained anchor" `Quick
      test_import_json_restores_retained_anchor;
    Alcotest.test_case "import json requires key for signed rows" `Quick
      test_import_json_requires_key_for_signed_rows;
    Alcotest.test_case "import json rejects malformed anchor" `Quick
      test_import_json_rejects_malformed_anchor;
    Alcotest.test_case "import json rejects malformed row" `Quick
      test_import_json_rejects_malformed_row;
    Alcotest.test_case
      "import json rejects tampered signed chain when key available" `Quick
      test_import_json_rejects_tampered_signed_chain_when_key_available;
    Alcotest.test_case "verify chain rejects missing signature with prev_hash"
      `Quick test_verify_chain_rejects_missing_signature_with_prev_hash;
    Alcotest.test_case "import json rejects missing signature with prev_hash"
      `Quick test_import_json_rejects_missing_signature_with_prev_hash;
    Alcotest.test_case "import json rejects anchor without signed rows" `Quick
      test_import_json_rejects_anchor_without_signed_rows;
    Alcotest.test_case "signed genesis" `Quick test_signed_genesis;
    Alcotest.test_case "verify chain accepts legacy signatures" `Quick
      test_verify_chain_accepts_legacy_signatures;
    Alcotest.test_case "verify chain rejects legacy signature with metadata"
      `Quick test_verify_chain_rejects_legacy_signature_with_metadata;
    Alcotest.test_case "unsigned entries" `Quick test_unsigned_entries;
  ]

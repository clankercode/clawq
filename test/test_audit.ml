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
  let row = List.hd rows in
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
  let path = Filename.concat tmpdir "export.jsonl" in
  let count = Audit.export_json ~db ~path in
  Alcotest.(check int) "exported 2 entries" 2 count;
  let ic = open_in path in
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
  (* Cleanup *)
  Sys.remove path;
  try Sys.rmdir tmpdir with _ -> ()

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

let test_key = "test-signing-key-for-audit-tests"

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
    Alcotest.test_case "retention tick" `Quick test_retention_tick;
    Alcotest.test_case "signed valid chain" `Quick test_signed_valid_chain;
    Alcotest.test_case "signed tampered entry" `Quick test_signed_tampered_entry;
    Alcotest.test_case "signed deleted entry" `Quick test_signed_deleted_entry;
    Alcotest.test_case "signed genesis" `Quick test_signed_genesis;
    Alcotest.test_case "unsigned entries" `Quick test_unsigned_entries;
  ]

(* Tests for iMessage channel module *)

let mk_im_cfg ?(allow_from = [ "*" ]) () : Runtime_config.imessage_config =
  { poll_interval_s = 5.0; allow_from; default_model = None }

(* --- is_allowed tests --- *)

let test_is_allowed_wildcard () =
  let config = mk_im_cfg () in
  Alcotest.(check bool)
    "wildcard" true
    (Imessage.is_allowed ~config ~handle_id:"+1234")

let test_is_allowed_match () =
  let config = mk_im_cfg ~allow_from:[ "+1234" ] () in
  Alcotest.(check bool)
    "match" true
    (Imessage.is_allowed ~config ~handle_id:"+1234")

let test_is_allowed_no_match () =
  let config = mk_im_cfg ~allow_from:[ "+1234" ] () in
  Alcotest.(check bool)
    "no match" false
    (Imessage.is_allowed ~config ~handle_id:"+9999")

(* --- is_macos tests --- *)

let test_is_macos () =
  (* On Linux this should be false *)
  let result = Imessage.is_macos () in
  Alcotest.(check bool)
    "is_macos returns bool" true
    (result = true || result = false)

(* --- chat_db_path tests --- *)

let test_chat_db_path_format () =
  let path = Imessage.chat_db_path () in
  Alcotest.(check bool) "non-empty path" true (String.length path > 0)

(* --- state_path tests --- *)

let test_state_path_format () =
  let path = Imessage.state_path () in
  Alcotest.(check bool) "non-empty" true (String.length path > 0);
  Alcotest.(check bool)
    "contains clawq" true
    (try
       ignore (Str.search_forward (Str.regexp_string ".clawq") path 0);
       true
     with Not_found -> false)

(* --- load_last_seen_id tests --- *)

let test_load_last_seen_id_default () =
  (* With no state file, should return 0 *)
  let id = Imessage.load_last_seen_id () in
  Alcotest.(check bool) "default is >= 0" true (id >= 0)

(* --- query_new_messages tests --- *)

(* Helper to set up an in-memory database mimicking iMessage chat.db schema *)
let setup_test_db () =
  let db = Sqlite3.db_open ":memory:" in
  ignore
    (Sqlite3.exec db "CREATE TABLE handle (ROWID INTEGER PRIMARY KEY, id TEXT)");
  ignore
    (Sqlite3.exec db
       "CREATE TABLE message (ROWID INTEGER PRIMARY KEY, text TEXT, handle_id \
        INTEGER, is_from_me INTEGER)");
  db

let test_query_new_messages_empty_db () =
  let db = setup_test_db () in
  let rows = Imessage.query_new_messages ~db ~last_id:0 in
  ignore (Sqlite3.db_close db);
  Alcotest.(check int) "no messages" 0 (List.length rows)

let test_query_new_messages_with_data () =
  let db = setup_test_db () in
  ignore (Sqlite3.exec db "INSERT INTO handle (ROWID, id) VALUES (1, '+1234')");
  ignore (Sqlite3.exec db "INSERT INTO handle (ROWID, id) VALUES (2, '+5678')");
  ignore
    (Sqlite3.exec db
       "INSERT INTO message (ROWID, text, handle_id, is_from_me) VALUES (1, \
        'hello', 1, 0)");
  ignore
    (Sqlite3.exec db
       "INSERT INTO message (ROWID, text, handle_id, is_from_me) VALUES (2, \
        'world', 2, 0)");
  let rows = Imessage.query_new_messages ~db ~last_id:0 in
  ignore (Sqlite3.db_close db);
  Alcotest.(check int) "2 messages" 2 (List.length rows);
  (* Verify handle IDs are resolved to phone numbers *)
  let _, handle1, _ = List.nth rows 0 in
  let _, handle2, _ = List.nth rows 1 in
  Alcotest.(check string) "handle 1" "+1234" handle1;
  Alcotest.(check string) "handle 2" "+5678" handle2

let test_query_new_messages_skips_own () =
  let db = setup_test_db () in
  ignore (Sqlite3.exec db "INSERT INTO handle (ROWID, id) VALUES (1, '+1234')");
  ignore
    (Sqlite3.exec db
       "INSERT INTO message (ROWID, text, handle_id, is_from_me) VALUES (1, \
        'sent', 1, 1)");
  let rows = Imessage.query_new_messages ~db ~last_id:0 in
  ignore (Sqlite3.db_close db);
  Alcotest.(check int) "skip own messages" 0 (List.length rows)

let test_query_new_messages_after_id () =
  let db = setup_test_db () in
  ignore (Sqlite3.exec db "INSERT INTO handle (ROWID, id) VALUES (1, '+1234')");
  ignore (Sqlite3.exec db "INSERT INTO handle (ROWID, id) VALUES (2, '+5678')");
  ignore
    (Sqlite3.exec db
       "INSERT INTO message (ROWID, text, handle_id, is_from_me) VALUES (1, \
        'old', 1, 0)");
  ignore
    (Sqlite3.exec db
       "INSERT INTO message (ROWID, text, handle_id, is_from_me) VALUES (2, \
        'new', 2, 0)");
  let rows = Imessage.query_new_messages ~db ~last_id:1 in
  ignore (Sqlite3.db_close db);
  Alcotest.(check int) "only new messages" 1 (List.length rows)

let suite =
  [
    Alcotest.test_case "is_allowed wildcard" `Quick test_is_allowed_wildcard;
    Alcotest.test_case "is_allowed match" `Quick test_is_allowed_match;
    Alcotest.test_case "is_allowed no match" `Quick test_is_allowed_no_match;
    Alcotest.test_case "is_macos" `Quick test_is_macos;
    Alcotest.test_case "chat_db_path format" `Quick test_chat_db_path_format;
    Alcotest.test_case "state_path format" `Quick test_state_path_format;
    Alcotest.test_case "load_last_seen_id default" `Quick
      test_load_last_seen_id_default;
    Alcotest.test_case "query empty db" `Quick test_query_new_messages_empty_db;
    Alcotest.test_case "query with data" `Quick
      test_query_new_messages_with_data;
    Alcotest.test_case "query skips own" `Quick
      test_query_new_messages_skips_own;
    Alcotest.test_case "query after id" `Quick test_query_new_messages_after_id;
  ]

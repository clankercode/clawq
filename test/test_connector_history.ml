(* test_connector_history.ml — Tests for connector history module *)

let fresh_db () =
  let db = Sqlite3.db_open ":memory:" in
  Memory.init_connector_history_schema db;
  db

let reset_buffers () =
  (* Clear all in-memory buffers between tests *)
  Hashtbl.reset Connector_history.buffers

let test_record_and_get () =
  reset_buffers ();
  Connector_history.record ~persist:false ~key:"test:1" ~channel_type:"teams"
    ~max:50 ~sender_name:"Alice" ~sender_id:"u1" ~text:"Hello" ();
  Connector_history.record ~persist:false ~key:"test:1" ~channel_type:"teams"
    ~max:50 ~sender_name:"Bob" ~sender_id:"u2" ~text:"World" ();
  let entries = Connector_history.get ~key:"test:1" ~count:10 () in
  Alcotest.(check int) "two entries" 2 (List.length entries);
  (* get returns chronological order: oldest first *)
  let first = List.hd entries in
  Alcotest.(check string) "first sender" "Alice" first.sender_name;
  Alcotest.(check string) "first text" "Hello" first.text;
  let second = List.nth entries 1 in
  Alcotest.(check string) "second sender" "Bob" second.sender_name;
  Alcotest.(check string) "second text" "World" second.text

let test_buffer_max () =
  reset_buffers ();
  for i = 1 to 5 do
    Connector_history.record ~persist:false ~key:"test:max"
      ~channel_type:"discord" ~max:3
      ~sender_name:(Printf.sprintf "user%d" i)
      ~sender_id:(string_of_int i) ~text:(Printf.sprintf "msg%d" i) ()
  done;
  let entries = Connector_history.get ~key:"test:max" ~count:10 () in
  Alcotest.(check int) "capped at 3" 3 (List.length entries);
  (* Should have messages 3, 4, 5 (oldest dropped) *)
  let first = List.hd entries in
  Alcotest.(check string) "oldest kept is msg3" "msg3" first.text

let test_get_partial () =
  reset_buffers ();
  for i = 1 to 5 do
    Connector_history.record ~persist:false ~key:"test:partial"
      ~channel_type:"teams" ~max:50 ~sender_name:"user" ~sender_id:"u1"
      ~text:(Printf.sprintf "msg%d" i) ()
  done;
  let entries = Connector_history.get ~key:"test:partial" ~count:2 () in
  Alcotest.(check int) "only 2 returned" 2 (List.length entries);
  (* Should be the 2 most recent, in chronological order *)
  let first = List.hd entries in
  Alcotest.(check string) "msg4" "msg4" first.text;
  let second = List.nth entries 1 in
  Alcotest.(check string) "msg5" "msg5" second.text

let test_format_for_context () =
  reset_buffers ();
  Connector_history.record ~persist:false ~key:"test:fmt" ~channel_type:"teams"
    ~max:50 ~sender_name:"Alice" ~sender_id:"u1" ~text:"Hello there" ();
  let entries = Connector_history.get ~key:"test:fmt" ~count:10 () in
  let formatted = Connector_history.format_for_context entries in
  Alcotest.(check bool)
    "contains sender" true
    (String.length formatted > 0
    &&
      try
        ignore (Str.search_forward (Str.regexp_string "Alice") formatted 0);
        true
      with Not_found -> false);
  Alcotest.(check bool)
    "contains text" true
    (try
       ignore (Str.search_forward (Str.regexp_string "Hello there") formatted 0);
       true
     with Not_found -> false)

let test_format_with_metadata () =
  reset_buffers ();
  Connector_history.record ~persist:false ~key:"test:meta" ~channel_type:"teams"
    ~max:50 ~sender_name:"Alice" ~sender_id:"u1" ~text:"Hello"
    ~metadata_json:"{\"thread_id\":\"t1\"}" ();
  let entries = Connector_history.get ~key:"test:meta" ~count:10 () in
  let formatted = Connector_history.format_for_context entries in
  Alcotest.(check bool)
    "contains metadata" true
    (try
       ignore (Str.search_forward (Str.regexp_string "metadata") formatted 0);
       true
     with Not_found -> false)

let test_clear () =
  reset_buffers ();
  Connector_history.record ~persist:false ~key:"test:clear"
    ~channel_type:"teams" ~max:50 ~sender_name:"Alice" ~sender_id:"u1"
    ~text:"Hello" ();
  Connector_history.clear ~key:"test:clear" ();
  let entries = Connector_history.get ~key:"test:clear" ~count:10 () in
  Alcotest.(check int) "empty after clear" 0 (List.length entries)

let test_db_persistence () =
  reset_buffers ();
  let db = fresh_db () in
  Connector_history.record ~db ~persist:true ~key:"test:db"
    ~channel_type:"teams" ~max:50 ~sender_name:"Alice" ~sender_id:"u1"
    ~text:"Persisted" ();
  (* Verify row exists in DB *)
  let stmt =
    Sqlite3.prepare db
      "SELECT sender_name, text FROM connector_history WHERE session_key = \
       'test:db'"
  in
  let found = ref false in
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
      let name =
        match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
      in
      let text =
        match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
      in
      Alcotest.(check string) "db sender" "Alice" name;
      Alcotest.(check string) "db text" "Persisted" text;
      found := true
  | _ -> ());
  ignore (Sqlite3.finalize stmt);
  Alcotest.(check bool) "row found in DB" true !found;
  ignore (Sqlite3.db_close db)

let test_db_backfill () =
  reset_buffers ();
  let db = fresh_db () in
  (* Insert directly into DB *)
  let stmt =
    Sqlite3.prepare db
      "INSERT INTO connector_history (session_key, channel_type, sender_name, \
       sender_id, text) VALUES (?, 'teams', 'DBUser', 'u99', 'From DB')"
  in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT "test:backfill"));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  (* get should backfill from DB since in-memory buffer is empty *)
  let entries = Connector_history.get ~db ~key:"test:backfill" ~count:10 () in
  Alcotest.(check int) "one entry from DB" 1 (List.length entries);
  let e = List.hd entries in
  Alcotest.(check string) "from DB" "From DB" e.text;
  Alcotest.(check string) "from DB sender" "DBUser" e.sender_name;
  ignore (Sqlite3.db_close db)

let test_nullable_text () =
  reset_buffers ();
  let db = fresh_db () in
  Connector_history.record ~db ~persist:true ~key:"test:null"
    ~channel_type:"teams" ~max:50 ~sender_name:"Alice" ~sender_id:"u1" ~text:""
    ();
  (* Verify NULL stored in DB for empty text *)
  let stmt =
    Sqlite3.prepare db
      "SELECT text FROM connector_history WHERE session_key = 'test:null'"
  in
  let is_null = ref false in
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW -> (
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.NULL -> is_null := true
      | _ -> ())
  | _ -> ());
  ignore (Sqlite3.finalize stmt);
  Alcotest.(check bool) "empty text stored as NULL" true !is_null;
  ignore (Sqlite3.db_close db)

let test_clear_with_db () =
  reset_buffers ();
  let db = fresh_db () in
  Connector_history.record ~db ~persist:true ~key:"test:cleardb"
    ~channel_type:"teams" ~max:50 ~sender_name:"Alice" ~sender_id:"u1"
    ~text:"Hello" ();
  Connector_history.clear ~db ~key:"test:cleardb" ();
  let entries = Connector_history.get ~db ~key:"test:cleardb" ~count:10 () in
  Alcotest.(check int) "empty after clear" 0 (List.length entries);
  ignore (Sqlite3.db_close db)

let test_parse_utc_datetime_roundtrip () =
  (* Format a known epoch as the UTC string sqlite's datetime('now') would emit,
     parse it back, and require an exact round-trip.  This is independent of the
     host timezone/DST because we compare against the originating epoch. Exercise
     both a northern-summer and a northern-winter instant so a host on DST sees
     a differing offset across the two cases. *)
  let check_roundtrip epoch =
    let tm = Unix.gmtime epoch in
    let s =
      Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (tm.Unix.tm_year + 1900)
        (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
    in
    let parsed = Connector_history.parse_utc_datetime s in
    Alcotest.(check (float 0.5)) (Printf.sprintf "roundtrip %s" s) epoch parsed
  in
  check_roundtrip 1751000000.0 (* 2025-06-27 (northern summer) *);
  check_roundtrip 1735000000.0 (* 2024-12-23 (northern winter) *)

let tests =
  [
    Alcotest.test_case "record and get round-trip" `Quick test_record_and_get;
    Alcotest.test_case "parse_utc_datetime UTC round-trip" `Quick
      test_parse_utc_datetime_roundtrip;
    Alcotest.test_case "buffer respects max" `Quick test_buffer_max;
    Alcotest.test_case "get with count < buffer" `Quick test_get_partial;
    Alcotest.test_case "format_for_context output" `Quick
      test_format_for_context;
    Alcotest.test_case "format with metadata" `Quick test_format_with_metadata;
    Alcotest.test_case "clear empties buffer" `Quick test_clear;
    Alcotest.test_case "DB persistence" `Quick test_db_persistence;
    Alcotest.test_case "DB backfill" `Quick test_db_backfill;
    Alcotest.test_case "nullable text" `Quick test_nullable_text;
    Alcotest.test_case "clear with DB" `Quick test_clear_with_db;
  ]

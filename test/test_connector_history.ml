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
  let timestamp = 1_751_000_000.0 in
  Connector_history.record ~db ~persist:true ~key:"test:db" ~room_id:"room-a"
    ~connector_type:"teams" ~channel_type:"teams" ~max:50 ~sender_name:"Alice"
    ~sender_id:"u1" ~text:"Persisted" ~timestamp ();
  (* Verify row exists in DB with room scope, connector type, and timestamp. *)
  let stmt =
    Sqlite3.prepare db
      "SELECT sender_name, text, room_id, connector_type, created_at FROM \
       connector_history WHERE session_key = 'test:db'"
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
      let room_id =
        match Sqlite3.column stmt 2 with Sqlite3.Data.TEXT s -> s | _ -> ""
      in
      let connector_type =
        match Sqlite3.column stmt 3 with Sqlite3.Data.TEXT s -> s | _ -> ""
      in
      let created_at =
        match Sqlite3.column stmt 4 with Sqlite3.Data.TEXT s -> s | _ -> ""
      in
      Alcotest.(check string) "db sender" "Alice" name;
      Alcotest.(check string) "db text" "Persisted" text;
      Alcotest.(check string) "room scope" "room-a" room_id;
      Alcotest.(check string) "connector type" "teams" connector_type;
      Alcotest.(check string)
        "created_at timestamp"
        (Connector_history.utc_datetime_of_epoch timestamp)
        created_at;
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

let test_query_filters_room_connector_time_range () =
  reset_buffers ();
  let db = fresh_db () in
  let add ?(room_id = "room-a") ?(connector_type = "teams") text timestamp =
    Connector_history.record ~db ~persist:true ~key:(room_id ^ ":session")
      ~room_id ~connector_type ~channel_type:connector_type ~max:50
      ~sender_name:"user" ~sender_id:"u1" ~text ~timestamp ()
  in
  add "too-old" 100.0;
  add "match-1" 200.0;
  add "match-2" 300.0;
  add ~room_id:"room-b" "wrong-room" 250.0;
  add ~connector_type:"discord" "wrong-connector" 250.0;
  let entries =
    Connector_history.query ~db ~room_id:"room-a" ~connector_type:"teams"
      ~since_ts:150.0 ~until_ts:350.0 ()
  in
  Alcotest.(check int) "two matching entries" 2 (List.length entries);
  Alcotest.(check (list string))
    "filtered in chronological order" [ "match-1"; "match-2" ]
    (List.map (fun (e : Connector_history.entry) -> e.text) entries);
  ignore (Sqlite3.db_close db)

let test_retention_cleanup_deletes_old_entries () =
  reset_buffers ();
  let db = fresh_db () in
  let now = Unix.gettimeofday () in
  Connector_history.record ~db ~persist:true ~key:"retention" ~room_id:"room-a"
    ~connector_type:"teams" ~channel_type:"teams" ~max:50 ~sender_name:"Old"
    ~sender_id:"u1" ~text:"old"
    ~timestamp:(now -. (3.0 *. 86_400.0))
    ();
  Connector_history.record ~db ~persist:true ~key:"retention" ~room_id:"room-a"
    ~connector_type:"teams" ~channel_type:"teams" ~max:50 ~sender_name:"New"
    ~sender_id:"u2" ~text:"new"
    ~timestamp:(now -. (0.5 *. 86_400.0))
    ();
  Memory.cleanup_connector_history ~db ~max_age_days:1 ~max_messages:50;
  let entries = Connector_history.query ~db ~room_id:"room-a" () in
  Alcotest.(check (list string))
    "only retained entry" [ "new" ]
    (List.map (fun (e : Connector_history.entry) -> e.text) entries);
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

let test_inject_tool_respects_capability_matrix () =
  reset_buffers ();
  let db = fresh_db () in
  let config : Runtime_config.t =
    {
      Runtime_config.default with
      connector_history =
        { Runtime_config.default.connector_history with enabled = true };
    }
  in
  let mgr = Session.create ~config ~db () in
  let key = "telegram:chat1:user1" in
  Session.register_connector_capabilities mgr ~key
    Connector_capabilities.telegram;
  Connector_history.record ~persist:false ~key ~channel_type:"telegram" ~max:50
    ~sender_name:"Alice" ~sender_id:"u1" ~text:"not injectable" ();
  let tool =
    Tools_builtin.inject_connector_history ~config ~db ~session_mgr:mgr ()
  in
  let context = { Tool.default_context with session_key = Some key } in
  let out = Lwt_main.run (tool.invoke ~context (`Assoc [])) in
  Alcotest.(check bool)
    "telegram capability blocks injection" true
    (Test_helpers.string_contains out "does not support connector history");
  ignore (Sqlite3.db_close db)

let test_inject_tool_allows_capture_capable_connector () =
  reset_buffers ();
  let db = fresh_db () in
  let config : Runtime_config.t =
    {
      Runtime_config.default with
      connector_history =
        { Runtime_config.default.connector_history with enabled = true };
    }
  in
  let mgr = Session.create ~config ~db () in
  let key = "teams:T1:C1" in
  Session.register_connector_capabilities mgr ~key Connector_capabilities.teams;
  Connector_history.record ~persist:false ~key ~channel_type:"teams" ~max:50
    ~sender_name:"Alice" ~sender_id:"u1" ~text:"injectable" ();
  let tool =
    Tools_builtin.inject_connector_history ~config ~db ~session_mgr:mgr ()
  in
  let context = { Tool.default_context with session_key = Some key } in
  let out = Lwt_main.run (tool.invoke ~context (`Assoc [])) in
  Alcotest.(check bool)
    "teams capability allows injection" true
    (Test_helpers.string_contains out "injectable");
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
    Alcotest.test_case "query filters room connector time" `Quick
      test_query_filters_room_connector_time_range;
    Alcotest.test_case "retention cleanup deletes old entries" `Quick
      test_retention_cleanup_deletes_old_entries;
    Alcotest.test_case "DB backfill" `Quick test_db_backfill;
    Alcotest.test_case "nullable text" `Quick test_nullable_text;
    Alcotest.test_case "clear with DB" `Quick test_clear_with_db;
    Alcotest.test_case "inject tool respects capability matrix" `Quick
      test_inject_tool_respects_capability_matrix;
    Alcotest.test_case "inject tool allows capture-capable connector" `Quick
      test_inject_tool_allows_capture_capable_connector;
  ]

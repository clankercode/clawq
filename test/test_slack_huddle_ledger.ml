open Test_slack_huddle_fixtures
module L = Slack_huddle_ledger
module C = Slack_huddle_classify

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db : bool))
    (fun () ->
      L.ensure_schema db;
      f db)

let room ?(id = "R0ROOM1") ?(has_ended = false) ?(date_start = Some 1700000000)
    ?(date_end = None) ?(thread = "1700000000.000100") ?(attached = [])
    ?(transcript = Some "F0TRANSCRIPT") () : C.room =
  {
    C.room_id = id;
    has_ended;
    date_start;
    date_end;
    thread_root_ts = Some thread;
    canvas_thread_ts = Some thread;
    attached_file_ids = attached;
    transcript_file_id = transcript;
  }

let seen ?(team = "T123") ?(channel = "C0TEST") ?(now = 1000.)
    ?(root_ts = "1700000000.000100") ?(files = [ "F0CANVAS" ]) db r =
  L.record_seen ~db ~team ~channel ~source:"event" ~now ~root_ts ~file_ids:files
    r

let notes ?(now = 2000.) ?(thread_ts = "1700000000.000100")
    ?(ts = "1700003900.000400") ?(variant = C.Notes_and_transcript) db =
  L.record_notes_ready ~db ~team:"T123" ~channel:"C0TEST" ~source:"event" ~now
    ~thread_ts ~ts ~file_ids:[ "F0CANVAS" ] ~canvas_file_id:"F0CANVAS" ~variant

let rows ?(team = "T123") ?(channel = "C0TEST") db =
  L.list_rows ~db ~team ~channel

let one db =
  match rows db with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected exactly one row, got %d" (List.length rs)

let attachment =
  Alcotest.testable
    (fun ppf -> function
      | L.Attached room_id -> Format.fprintf ppf "Attached %s" room_id
      | L.Stub -> Format.pp_print_string ppf "Stub")
    ( = )

let check_attachment = Alcotest.check attachment
let check_str = Alcotest.(check string)
let check_ids = Alcotest.(check (list string))
let check_str_opt = Alcotest.(check (option string))
let check_int_opt = Alcotest.(check (option int))
let check_float = Alcotest.(check (float 0.0))
let check_int = Alcotest.(check int)

let test_room_lifecycle_merge () =
  with_db (fun db ->
      seen db (room ());
      let r = one db in
      check_bool "kind is Room" true (r.L.kind = L.Room);
      check_str_opt "room_id" (Some "R0ROOM1") r.L.room_id;
      check_bool "open" false r.L.has_ended;
      check_str_opt "root_ts" (Some "1700000000.000100") r.L.root_ts;
      check_ids "root files" [ "F0CANVAS" ] r.L.root_file_ids;
      check_str_opt "transcript hint" (Some "F0TRANSCRIPT")
        r.L.room_transcript_file_id;
      check_ids "attached" [] r.L.attached_file_ids;
      check_str "source" "event" r.L.source;
      seen ~now:1100. ~files:[ "F0CANVAS"; "F0Y" ] db
        (room ~has_ended:true ~date_end:(Some 1700003600) ~attached:[ "F0X" ] ());
      (* A stale "not ended" edit of the same use arrives late. *)
      seen ~now:1200. ~files:[] db (room ~transcript:None ());
      let r = one db in
      check_bool "stale edit cannot reopen" true r.L.has_ended;
      check_int_opt "date_end kept" (Some 1700003600) r.L.date_end;
      check_ids "attached kept" [ "F0X" ] r.L.attached_file_ids;
      check_ids "root files appended" [ "F0CANVAS"; "F0Y" ] r.L.root_file_ids;
      check_str_opt "None never clears the transcript hint"
        (Some "F0TRANSCRIPT") r.L.room_transcript_file_id;
      check_float "last_seen_at" 1200. r.L.last_seen_at)

(* Two ended edits of one use (same date_start) arrive out of order. *)
let test_date_end_never_regresses () =
  List.iter
    (fun (label, first, second) ->
      with_db (fun db ->
          seen db (room ~has_ended:true ~date_end:(Some first) ());
          seen ~now:1100. db (room ~has_ended:true ~date_end:(Some second) ());
          check_int_opt label (Some 1700003600) (one db).L.date_end))
    [
      ("later end first keeps it", 1700003600, 1700003000);
      ("earlier end first is replaced", 1700003000, 1700003600);
    ]

let test_notes_newer_wins () =
  with_db (fun db ->
      seen db (room ());
      check_attachment "attaches before the room ends" (L.Attached "R0ROOM1")
        (notes db);
      let r = one db in
      check_str_opt "notes_ready_ts" (Some "1700003900.000400")
        r.L.notes_ready_ts;
      check_str_opt "canvas" (Some "F0CANVAS") r.L.canvas_file_id;
      check_str_opt "variant hint" (Some "notes_and_transcript")
        r.L.notes_variant_hint;
      check_str_opt "observer never verifies a transcript" None
        r.L.transcript_file_id;
      seen ~now:2100. db (room ~has_ended:true ~date_end:(Some 1700003600) ());
      let r = one db in
      check_bool "ended" true r.L.has_ended;
      check_str_opt "notes survive the ended edit" (Some "1700003900.000400")
        r.L.notes_ready_ts;
      ignore (notes ~ts:"1700003800.000100" ~variant:C.Notes_only db);
      check_str_opt "older ready reply ignored" (Some "notes_and_transcript")
        (one db).L.notes_variant_hint;
      ignore (notes ~ts:"1700004500.000100" ~variant:C.Notes_only db);
      check_str_opt "newer ready reply wins" (Some "notes_only")
        (one db).L.notes_variant_hint)

let test_thread_started_attach () =
  with_db (fun db ->
      seen ~root_ts:"1700000500.000300" db
        (room ~id:"R0ROOM2" ~thread:"1699990000.000100" ());
      check_attachment "canvas thread match" (L.Attached "R0ROOM2")
        (notes ~thread_ts:"1699990000.000100" db))

let test_reused_room_id () =
  with_db (fun db ->
      seen db (room ~has_ended:true ());
      ignore (notes db);
      seen ~now:5000. db
        (room ~date_start:(Some 1700090000) ~transcript:(Some "F0T2") ());
      let r = one db in
      check_int_opt "new start" (Some 1700090000) r.L.date_start;
      check_bool "reset to open" false r.L.has_ended;
      check_str_opt "notes cleared" None r.L.notes_ready_ts;
      check_str_opt "canvas cleared" None r.L.canvas_file_id;
      check_str_opt "new transcript hint" (Some "F0T2")
        r.L.room_transcript_file_id;
      (* A late edit from the first use of the room id. *)
      seen ~now:6000. ~files:[ "F0OLD" ] db
        (room ~has_ended:true ~date_end:(Some 1700003600) ~attached:[ "F0OLD" ]
           ());
      let r = one db in
      check_int_opt "start kept" (Some 1700090000) r.L.date_start;
      check_bool "stale edit cannot end the new use" false r.L.has_ended;
      check_int_opt "date_end untouched" None r.L.date_end;
      check_ids "attached untouched" [] r.L.attached_file_ids;
      check_ids "root files untouched" [ "F0CANVAS" ] r.L.root_file_ids;
      check_str_opt "transcript hint untouched" (Some "F0T2")
        r.L.room_transcript_file_id;
      check_str_opt "notes untouched" None r.L.notes_ready_ts;
      check_float "last_seen_at" 6000. r.L.last_seen_at;
      check_attachment "late ready reply of the first use" L.Stub
        (notes ~ts:"1700003900.000400" db);
      check_bool "the room keeps no notes" true
        (List.exists
           (fun (r : L.row) -> r.L.kind = L.Room && r.L.notes_ready_ts = None)
           (rows db)))

let test_unknown_thread_stub () =
  with_db (fun db ->
      check_attachment "first" L.Stub (notes ~thread_ts:"1690000000.000100" db);
      check_attachment "second" L.Stub (notes ~thread_ts:"1690000000.000100" db);
      let r = one db in
      check_bool "kind is Notes_stub" true (r.L.kind = L.Notes_stub);
      check_str_opt "no room" None r.L.room_id;
      check_str_opt "stub thread" (Some "1690000000.000100") r.L.notes_thread_ts;
      check_str_opt "stub canvas" (Some "F0CANVAS") r.L.canvas_file_id)

let test_isolation_and_ttl () =
  with_db (fun db ->
      seen db (room ());
      seen ~channel:"C0OTHER" db (room ());
      seen ~team:"T999" db (room ());
      check_int "T123/C0TEST" 1 (List.length (rows db));
      check_int "T123/C0OTHER" 1 (List.length (rows ~channel:"C0OTHER" db));
      check_int "T999/C0TEST" 1 (List.length (rows ~team:"T999" db));
      check_int "expired rows pruned" 3
        (L.prune ~db ~now:(1000. +. L.ttl_seconds +. 1.));
      L.ensure_schema db;
      check_int "nothing left" 0 (List.length (rows db)));
  let bare = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close bare : bool))
    (fun () ->
      check_int "prune of a DB that never observed" 0
        (L.prune ~db:bare ~now:1e9);
      check_ids "prune creates no table" []
        (Sql_util.query_rows bare
           "SELECT name FROM sqlite_master WHERE type='table' AND \
            name='slack_huddles'"
           ~bind:ignore ~of_stmt:(fun stmt -> Sql_util.text_column stmt 0)))

(* The DB is the daemon's shared handle: a write that cannot begin its own
   transaction must leave the caller's transaction alone, and a write that
   fails after its own BEGIN must roll back only itself. *)
let test_write_transaction_boundary () =
  with_db (fun db ->
      Sql_util.exec_exn db "BEGIN";
      Sql_util.exec_exn db "CREATE TABLE sentinel (x INTEGER)";
      (match seen db (room ()) with
      | () -> Alcotest.fail "a write inside a caller's transaction must raise"
      | exception Failure _ -> ());
      check_bool "caller's transaction still open" true
        (Sqlite3.exec db "COMMIT" = Sqlite3.Rc.OK);
      check_int "caller's work kept" 1
        (List.length
           (Sql_util.query_rows db
              "SELECT name FROM sqlite_master WHERE name='sentinel'"
              ~bind:ignore ~of_stmt:ignore));
      check_int "nothing written" 0 (List.length (rows db));
      (match
         L.record_seen ~db ~team:"T123" ~channel:"C0TEST" ~source:"bogus"
           ~now:1000. ~root_ts:"1700000000.000100" ~file_ids:[] (room ())
       with
      | () -> Alcotest.fail "an invalid source must violate the CHECK"
      | exception Failure _ -> ());
      check_bool "own failed transaction rolled back" true
        (Sqlite3.exec db "BEGIN" = Sqlite3.Rc.OK);
      Sql_util.exec_exn db "ROLLBACK";
      check_int "failed write left no row" 0 (List.length (rows db)))

let test_verified_use () =
  with_db (fun db ->
      seen db (room ());
      let verify date_start transcript_file_id =
        L.set_verified ~db ~team:"T123" ~channel:"C0TEST" ~room_id:"R0ROOM1"
          ~date_start ~transcript_file_id ~finalised:true
      in
      verify 1700000000 (Some "FVERIFIED");
      check_bool "matching use finalised" true (one db).finalised;
      check_str_opt "verified transcript" (Some "FVERIFIED")
        (one db).transcript_file_id;
      seen db (room ~date_start:(Some 1700003600) ());
      verify 1700000000 (Some "FOLD");
      check_bool "old use cannot finalise new room" false (one db).finalised;
      check_str_opt "old transcript cannot overwrite" None
        (one db).transcript_file_id)

let test_strict_read_failure () =
  let path = Filename.temp_file "clawq-ledger-lock" ".db" in
  let db = Sqlite3.db_open path in
  let other = Sqlite3.db_open path in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sqlite3.db_close other);
      ignore (Sqlite3.db_close db);
      Sys.remove path)
    (fun () ->
      L.ensure_schema db;
      Sql_util.exec_exn other "BEGIN EXCLUSIVE";
      Fun.protect
        ~finally:(fun () -> Sql_util.exec_exn other "ROLLBACK")
        (fun () ->
          let raised =
            try
              ignore (rows db);
              false
            with _ -> true
          in
          check_bool "failed read is not an empty hint list" true raised))

let suite =
  [
    ("room lifecycle merge is monotonic", `Quick, test_room_lifecycle_merge);
    ("date_end never regresses", `Quick, test_date_end_never_regresses);
    ("notes attach, newer wins", `Quick, test_notes_newer_wins);
    ( "thread-started huddle attaches by canvas thread",
      `Quick,
      test_thread_started_attach );
    ( "reused room id: newer start resets, older start is stale",
      `Quick,
      test_reused_room_id );
    ("unknown thread stores one stub", `Quick, test_unknown_thread_stub);
    ("team/channel isolation and 30-day TTL", `Quick, test_isolation_and_ttl);
    ( "write never rolls back a caller's transaction",
      `Quick,
      test_write_transaction_boundary );
    ("verification requires the current room use", `Quick, test_verified_use);
    ("failed ledger read raises", `Quick, test_strict_read_failure);
  ]

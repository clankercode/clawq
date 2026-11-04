open Lwt.Syntax
open Test_slack_huddle_fixtures
open Test_slack_threads
module F = Test_slack_event_fixtures
module L = Slack_huddle_ledger
module W = Slack_huddle_watch
module TL = Test_slack_huddle_ledger

let with_db = TL.with_db
let check_calls = Alcotest.(check (list (triple string string string)))

(* Observes every channel of team T123 and records each question it is asked. *)
let watch_all ?(log = ref []) () =
  {
    W.observed =
      (fun ~team ~app ~channel ->
        log := !log @ [ (team, app, channel) ];
        team = "T123");
    context =
      (fun ~team:_ ~app:_ ~channel:_ -> Slack_huddle_classify.empty_context);
  }

let observe ?(watch = watch_all ()) ?team db e =
  Slack_huddle_observer.observe_db ~db ~watch ~now:1000. (F.envelope ?team e)

let rows db = L.list_rows ~db ~team:"T123" ~channel:"C0TEST"

let with_source source f =
  let saved = !W.source in
  W.source := source;
  Lwt.finalize f (fun () ->
      W.source := saved;
      Lwt.return_unit)

let with_watch w f = with_source (fun _ -> w) f

let test_observed_channel () =
  with_db (fun db ->
      observe db (F.huddle_root ());
      observe db (F.notes_ready ());
      let r = TL.one db in
      TL.check_str_opt "room" (Some "R0ROOM1") r.L.room_id;
      TL.check_ids "root files" [ "F0CANVAS" ] r.L.root_file_ids;
      check_bool "notes attached" true (r.L.notes_ready_ts <> None))

let test_gates () =
  with_db (fun db ->
      observe ~watch:W.none db (F.huddle_root ());
      check_bool "unobserved channel writes nothing" true (rows db = []);
      let log = ref [] in
      observe ~watch:(watch_all ~log ()) ~team:"T999" db (F.huddle_root ());
      check_calls "watch asked with the envelope ids"
        [ ("T999", "A123", "C0TEST") ]
        !log;
      check_bool "no T999 rows" true
        (L.list_rows ~db ~team:"T999" ~channel:"C0TEST" = []);
      let log = ref [] in
      observe ~watch:(watch_all ~log ()) db (F.huddle_root ~channel:"D0DM" ());
      check_calls "D channel never reaches the watch" [] !log;
      check_bool "no D channel rows" true
        (L.list_rows ~db ~team:"T123" ~channel:"D0DM" = []);
      observe ~watch:(watch_all ~log ()) db
        (F.reaction
           ~item:(obj [ ("type", str "file"); ("file", str "F0MD") ])
           ());
      check_calls "channel-less file reaction dropped before the watch" [] !log;
      check_bool "phase 1 of_db observes nothing" false
        ((W.of_db db).observed ~team:"T123" ~app:"A123" ~channel:"C0TEST"))

let test_failures () =
  let closed = Sqlite3.db_open ":memory:" in
  L.ensure_schema closed;
  ignore (Sqlite3.db_close closed : bool);
  check_bool "the store itself raises on a closed DB" true
    (match
       L.record_seen ~db:closed ~team:"T123" ~channel:"C0TEST" ~source:"event"
         ~now:1000. ~root_ts:"1700000000.000100" ~file_ids:[] (TL.room ())
     with
    | () -> false
    | exception _ -> true);
  let log = ref [] in
  observe ~watch:(watch_all ~log ()) closed (F.huddle_root ());
  check_calls "the event passed every gate" [ ("T123", "A123", "C0TEST") ] !log;
  with_db (fun db ->
      List.iter
        (fun e ->
          Slack_huddle_observer.observe_db ~db ~watch:(watch_all ()) ~now:1000.
            e)
        [
          `Null;
          F.block_actions;
          obj [ ("type", str "event_callback"); ("event", arr []) ];
          F.envelope (update (F.huddle_root ()) [ ("room", arr []) ]);
        ];
      check_bool "malformed payloads write nothing" true (rows db = []))

let room_row room_id rows =
  List.find_opt (fun (r : L.row) -> r.L.room_id = Some room_id) rows

let test_tap () =
  with_ingress (fun base calls _ seen _ run ->
      L.ensure_schema base.db;
      let ledger () = L.list_rows ~db:base.db ~team:"T123" ~channel:"C0TEST" in
      let* _ = run (F.huddle_root ()) in
      check_bool "phase-1 default watch writes nothing" true (ledger () = []);
      let* () =
        with_watch (watch_all ()) (fun () ->
            let* _ =
              run ~id:"Ev2"
                (F.huddle_root ~ts:"1700000001.000100"
                   ~room:(F.room ~id:"R0ROOM9" ()) ())
            in
            let* _ = run ~id:"Ev3" (F.notes_ready ()) in
            Lwt.return_unit)
      in
      (match room_row "R0ROOM9" (ledger ()) with
      | Some r ->
          check_bool "notes attached through the tap" true
            (r.L.notes_ready_ts <> None)
      | None -> Alcotest.fail "the tap wrote no R0ROOM9 row");
      check_bool "no Slack call" true (!calls = []);
      check_bool "no model turn yet" true (!seen = []);
      let exploded = ref 0 in
      let exploding =
        {
          (watch_all ()) with
          W.observed =
            (fun ~team:_ ~app:_ ~channel:_ ->
              incr exploded;
              failwith "watch exploded");
        }
      in
      let* () =
        with_watch exploding (fun () ->
            let* _ = run ~id:"Ev4" (F.huddle_root ~ts:"1700000002.000100" ()) in
            check_bool "a failed watch writes nothing" true
              (room_row "R0ROOM1" (ledger ()) = None);
            let* _ = run ~id:"Ev5" (message "please help") in
            Lwt.return_unit)
      in
      Alcotest.(check int) "the watch raised on both events" 2 !exploded;
      check_bool "routing continues after a watch failure" true (!seen <> []);
      Lwt.return_unit)

let test_lookup_failure () =
  with_ingress (fun _ _ _ seen _ run ->
      let asked = ref false in
      with_source
        (fun _ ->
          asked := true;
          failwith "phase-2 query failed")
        (fun () ->
          let* _ = run (message "please help") in
          check_bool "the tap looked the watch up" true !asked;
          check_bool "routing continues after a lookup failure" true
            (!seen <> []);
          Lwt.return_unit))

let suite =
  [
    ("observed channel records huddle and notes", `Quick, test_observed_channel);
    ("envelope, watch and dm gates", `Quick, test_gates);
    ("store failure never escapes; malformed ignored", `Quick, test_failures);
    ( "tap writes the ledger, keeps routing and makes no Slack call",
      `Quick,
      test_tap );
    ("watch lookup failure never escapes the tap", `Quick, test_lookup_failure);
  ]

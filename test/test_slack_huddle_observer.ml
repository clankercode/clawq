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
      (* Reactions read item.channel, a different path. *)
      observe ~watch:(watch_all ~log ()) db
        (F.reaction
           ~item:
             (obj
                [
                  ("type", str "message");
                  ("channel", str "D0DM");
                  ("ts", str "1700004000.000500");
                ])
           ());
      check_calls "D channel reaction never reaches the watch" [] !log;
      let reaction_log = ref [] in
      observe ~watch:(watch_all ~log:reaction_log ()) db (F.reaction ());
      check_calls "control: C channel reaction reaches the watch"
        [ ("T123", "A123", "C0TEST") ]
        !reaction_log;
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

(* ── Phase 2: [W.of_db] over the standing-order store ─────────────────── *)

module S = Test_slack_standing_order_support
module Store = Slack_standing_order_store

let target_t =
  Alcotest.testable
    (fun ppf (t : Slack_huddle_classify.target) ->
      Format.fprintf ppf "{user %s; bot %s; app %s}" t.target_user_id
        t.target_bot_id t.target_app_id)
    ( = )

let claude_target : Slack_huddle_classify.target =
  {
    target_user_id = "UCLAUDE";
    target_bot_id = "BCLAUDE";
    target_app_id = "ACLAUDE";
  }

let other : Slack_standing_order_types.target =
  {
    user_id = "UOTHER";
    app_id = "AOTHER";
    bot_id = "BOTHER";
    label = "Other bot";
  }

let observed ?(team = "T123") ?(app = "A123") db channel =
  (W.of_db db).W.observed ~team ~app ~channel

let check_context db channel ~self ~targets =
  let ctx = (W.of_db db).W.context ~team:"T123" ~app:"A123" ~channel in
  Alcotest.(check (option string))
    (channel ^ " self user") self ctx.Slack_huddle_classify.self_user_id;
  Alcotest.(check (list target_t)) (channel ^ " targets") targets ctx.targets;
  check_bool (channel ^ " no matchers") true
    (ctx.matchers = Slack_huddle_classify.no_matchers)

let deactivate db (o : Slack_standing_order_types.order) op =
  match Store.deactivate db ~order_id:o.id op ~now:S.now with
  | Ok { Store.changed = true; _ } -> ()
  | Ok _ -> Alcotest.fail "the order did not change state"
  | Error e -> Alcotest.fail e

let test_of_db_observes () =
  with_db (fun db ->
      let o = S.seed db ~channel:"C0TEST" () in
      check_bool "active order observed" true (observed db "C0TEST");
      check_context db "C0TEST" ~self:(Some "UBOT") ~targets:[ claude_target ];
      check_bool "another team" false (observed ~team:"T999" db "C0TEST");
      check_bool "another app" false (observed ~app:"A999" db "C0TEST");
      check_bool "another channel" false (observed db "C999");
      deactivate db o (Store.Pause "U123");
      check_bool "paused order still observed" true (observed db "C0TEST");
      check_context db "C0TEST" ~self:(Some "UBOT") ~targets:[ claude_target ])

let test_of_db_ignores_inactive () =
  with_db (fun db ->
      List.iter
        (fun (channel, state) ->
          ignore
            (S.seed db ~channel ~state () : Slack_standing_order_types.order);
          check_bool (channel ^ " not observed") false (observed db channel);
          check_context db channel ~self:None ~targets:[])
        Slack_standing_order_types.
          [
            ("C0PENDING", Pending_confirmation);
            ("C0SUSPENDED", Suspended);
            ("C0CANCELLED", Cancelled);
          ])

let set_bot_user db (o : Slack_standing_order_types.order) bot_user_id =
  Sql_util.exec_with_params ~label:"test" db
    "UPDATE slack_standing_orders SET bot_user_id = ? WHERE id = ?"
    [ bot_user_id; Sqlite3.Data.TEXT o.id ]

let test_of_db_context () =
  with_db (fun db ->
      let first = S.seed db ~channel:"C0TEST" () in
      let second =
        S.seed db ~channel:"C0TEST" ~target:other
          ~state:Slack_standing_order_types.Paused ()
      in
      let other_target : Slack_huddle_classify.target =
        {
          target_user_id = "UOTHER";
          target_bot_id = "BOTHER";
          target_app_id = "AOTHER";
        }
      in
      let targets = [ claude_target; other_target ] in
      check_context db "C0TEST" ~self:(Some "UBOT") ~targets;
      set_bot_user db second Sqlite3.Data.NULL;
      check_context db "C0TEST" ~self:(Some "UBOT") ~targets;
      set_bot_user db second (Sqlite3.Data.TEXT "UBOT2");
      let (), logs =
        Lwt_main.run
          (S.with_log_capture (fun () ->
               check_context db "C0TEST" ~self:None ~targets;
               Lwt.return_unit))
      in
      check_bool "disagreeing bot users are logged" true
        (List.exists (fun (level, _) -> level = Logs.Warning) logs);
      check_bool "both orders still observed" true (observed db "C0TEST");
      set_bot_user db first Sqlite3.Data.NULL;
      set_bot_user db second Sqlite3.Data.NULL;
      check_context db "C0TEST" ~self:None ~targets)

let test_of_db_unreadable () =
  let closed = Sqlite3.db_open ":memory:" in
  ignore (S.seed closed ~channel:"C0TEST" () : Slack_standing_order_types.order);
  ignore (Sqlite3.db_close closed : bool);
  let (), logs =
    Lwt_main.run
      (S.with_log_capture (fun () ->
           check_bool "an unreadable store observes nothing" false
             (observed closed "C0TEST");
           check_context closed "C0TEST" ~self:None ~targets:[];
           Lwt.return_unit))
  in
  Alcotest.(check int)
    "each failed read is logged" 2
    (List.length (List.filter (fun (level, _) -> level = Logs.Warning) logs))

let table_exists db name =
  Sql_util.query_rows db
    "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?"
    ~bind:(fun stmt -> Sql_util.bind_params stmt [ Sqlite3.Data.TEXT name ])
    ~of_stmt:(fun _ -> ())
  <> []

(* Every Store read runs [ensure_schema] first, so the store's table appears
   exactly when the watch first queries it (ruling F13). *)
let test_of_db_lazy () =
  with_db (fun db ->
      let store_table () = table_exists db "slack_standing_orders" in
      let watch = W.of_db db in
      check_bool "building of_db runs no query" false (store_table ());
      Slack_huddle_observer.observe_db ~db ~watch:(W.of_db db) ~now:1000.
        F.block_actions;
      check_bool "block_actions runs no query" false (store_table ());
      observe ~watch db (F.human ~channel:"D0DM" "hello");
      check_bool "a DM runs no query" false (store_table ());
      observe ~watch db (F.human "hello");
      check_bool "a channel message asks the store" true (store_table ()))

let test_real_store_ledger () =
  with_db (fun db ->
      let o = S.seed db ~channel:"C0TEST" () in
      observe ~watch:(W.of_db db) db (F.huddle_root ());
      check_bool "the active order's channel writes a row" true
        (room_row "R0ROOM1" (rows db) <> None);
      deactivate db o (Store.Cancel "U123");
      observe ~watch:(W.of_db db) db
        (F.huddle_root ~ts:"1700000001.000100" ~room:(F.room ~id:"R0ROOM9" ())
           ());
      check_bool "a cancelled order's channel writes nothing" true
        (room_row "R0ROOM9" (rows db) = None);
      Alcotest.(check int) "still one row" 1 (List.length (rows db)))

let suite =
  [
    ("observed channel records huddle and notes", `Quick, test_observed_channel);
    ("envelope, watch and dm gates", `Quick, test_gates);
    ("store failure never escapes; malformed ignored", `Quick, test_failures);
    ( "tap writes the ledger, keeps routing and makes no Slack call",
      `Quick,
      test_tap );
    ("watch lookup failure never escapes the tap", `Quick, test_lookup_failure);
    S.case "of_db observes channels with active or paused orders"
      test_of_db_observes;
    S.case "of_db ignores pending, suspended and cancelled orders"
      test_of_db_ignores_inactive;
    S.case "of_db context needs every order to agree on the bot user"
      test_of_db_context;
    S.case "of_db fails closed on an unreadable store" test_of_db_unreadable;
    S.case "of_db is cheap per event" test_of_db_lazy;
    S.case "real store drives the ledger" test_real_store_ledger;
  ]

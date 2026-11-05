(* Fire-bound authority, persisted audit identity and read-only tracking. *)
open Test_slack_standing_order_grant_support

let test_unannounced () =
  expect_fail ~http:false "unannounced activation" Transient
    Activation_unannounced
    (eval
       ~mutate:(fun e _ ->
         Sql_util.exec_exn e.db "DELETE FROM slack_standing_order_cards")
       ());
  check_string "fixed activation refusal"
    "the channel activation notice is not confirmed yet"
    (Slack_standing_order_render.reason_text Activation_unannounced);
  let e = env () in
  let o = S.seed e.db () in
  let stage = fire_stage e o in
  S.override e.m "users.list" (fun params ->
      Sql_util.exec_exn e.db "DELETE FROM slack_standing_order_cards";
      (200, S.default_response e.m.world "users.list" params));
  expect_fail "announcement read last" Transient Activation_unannounced
    (evaluate e ~stage (G.subject_of_order o));
  expect_fail ~http:false "announcement read error" Transient
    (Indeterminate "internal")
    (eval
       ~mutate:(fun e _ ->
         Sql_util.exec_exn e.db
           "ALTER TABLE slack_standing_order_cards DROP COLUMN message_ts")
       ())

let test_binding () =
  let e = env () in
  let o = S.seed e.db () in
  announce e o;
  let foreign = S.seed e.db ~channel:"C999" () in
  let f = S.fire e.db foreign ~room:"R1" () in
  let subject = G.subject_of_order o in
  List.iter
    (fun stage ->
      expect_fail ~http:false "foreign fire" Hard Identity_changed
        (evaluate e ~stage subject))
    [
      G.Fire { fire_epoch = o.epoch; fire_id = f.id };
      G.Track { fire_id = f.id };
    ];
  List.iter
    (fun stage ->
      expect_fail ~http:false "missing fire" Transient (Indeterminate "fire")
        (evaluate e ~stage subject))
    [
      G.Fire { fire_epoch = o.epoch; fire_id = "sof_missing" };
      G.Track { fire_id = "sof_missing" };
    ];
  let f = S.fire e.db o ~room:"R2" () in
  expect_fail ~http:false "undispatched tracking" Transient
    (Indeterminate "fire")
    (evaluate e ~stage:(G.Track { fire_id = f.id }) subject);
  Sql_util.exec_with_params e.db
    "UPDATE slack_standing_order_fires SET epoch = epoch - 1 WHERE id = ?"
    [ Sqlite3.Data.TEXT f.id ];
  expect_fail ~http:false "stored fire epoch" Inactive Epoch_mismatch
    (evaluate e
       ~stage:(G.Fire { fire_epoch = o.epoch; fire_id = f.id })
       subject);
  let stage = fire_stage e o in
  S.override e.m "users.list" (fun params ->
      Sql_util.exec_exn e.db "DELETE FROM slack_standing_order_fires";
      (200, S.default_response e.m.world "users.list" params));
  expect_fail "fire binding read last" Transient (Indeterminate "fire")
    (evaluate e ~stage subject)

let test_tracking () =
  List.iter
    (fun state ->
      let e = env () in
      let o = S.seed e.db () in
      let f =
        S.fire e.db o ~state:T.Posted ~dispatched_at:S.now ~room:"R1" ()
      in
      let op =
        match state with
        | T.Paused -> Store.Pause "U456"
        | _ -> Store.Cancel "U456"
      in
      (match Store.deactivate e.db ~order_id:o.id op ~now:S.now with
      | Ok _ -> ()
      | Error err -> Alcotest.fail err);
      Sql_util.exec_exn e.db
        "UPDATE slack_standing_orders SET max_fires_per_day = 1, expires_at = \
         '2020-01-01 00:00:00'";
      let v =
        expect_ok "tracking inactive dispatched fire"
          (evaluate e
             ~stage:(G.Track { fire_id = f.id })
             (G.subject_of_order o))
      in
      check_bool "fresh inactive state" true
        (Option.map (fun (o : T.order) -> o.state) v.order = Some state);
      (match Access_snapshot.get_by_id ~db:e.db v.snapshot.id with
      | Some stored ->
          check_bool "persisted tracking work type" true
            (stored.work_type = Access_snapshot.Standing_order);
          check_bool "persisted tracking key" true
            (stored.session_key = Some "slack:C123");
          check_bool "persisted tracking fire identity" true
            (stored.standing_order
            = Some { Access_snapshot.order_id = o.id; fire_id = Some f.id })
      | None -> Alcotest.fail "missing tracking snapshot");
      e.m.calls := [];
      List.iter
        (fun method_ ->
          match
            Lwt_main.run (v.call method_ [ ("channel", `String "C123") ])
          with
          | Error (SO.Method_not_permitted m) ->
              check_string "blocked method" method_ m
          | _ -> Alcotest.fail ("tracking allowed write: " ^ method_))
        [
          "chat.postMessage";
          "chat.update";
          "files.getUploadURLExternal";
          "files.completeUploadExternal";
        ];
      check_int "tracking write denial costs zero HTTP" 0
        (List.length !(e.m.calls));
      (match
         Lwt_main.run (v.call "users.info" [ ("user", `String "U123") ])
       with
      | Ok _ -> ()
      | Error _ -> Alcotest.fail "tracking cannot read");
      S.edit_user e.m.world "U456" (set "is_restricted" (`Bool true));
      expect_fail "tracking retains guest policy" Hard Channel_has_guests
        (evaluate e ~stage:(G.Track { fire_id = f.id }) (G.subject_of_order o));
      e.cfg :=
        S.profile !(e.cfg) ~allowed:[] ~denied:[ "slack_huddle_content" ] ();
      expect_fail ~http:false "tracking retains capabilities" Hard
        (Tool_denied "slack_huddle_content")
        (evaluate e ~stage:(G.Track { fire_id = f.id }) (G.subject_of_order o)))
    T.[ Paused; Cancelled ]

let test_missing_identity () =
  let change method_ path edit e _ =
    S.override e.m method_ (fun params ->
        let body = S.default_response e.m.world method_ params in
        let body =
          match path with
          | None -> edit body
          | Some key -> set key (edit (Slack_huddles.get key body)) body
        in
        (200, body))
  in
  List.iter
    (fun (method_, path, key) ->
      List.iter
        (fun edit ->
          expect_fail
            (method_ ^ ": malformed " ^ key)
            Transient (Indeterminate method_)
            (eval ~mutate:(change method_ path edit) ()))
        [ drop key; set key `Null; set key (`Int 1); set key (`String "") ])
    [
      ("auth.test", None, "team_id");
      ("bots.info", Some "bot", "app_id");
      ("conversations.info", Some "channel", "id");
      ("conversations.info", Some "channel", "is_member");
    ];
  List.iter
    (fun key ->
      expect_fail ("target missing " ^ key) Transient
        (Indeterminate "users.list")
        (eval
           ~mutate:(fun e _ ->
             S.edit_user e.m.world "UCLAUDE" (fun u ->
                 set "profile" (drop key (Slack_huddles.get "profile" u)) u))
           ()))
    [ "api_app_id"; "bot_id" ]

let test_typed_membership () =
  List.iter
    (fun cursor ->
      expect_fail "malformed membership cursor" Transient
        (Indeterminate "conversations.members")
        (eval
           ~mutate:
             (answer "conversations.members" 200
                (S.ok
                   [
                     ("members", `List [ S.str "U123"; S.str "UCLAUDE" ]);
                     ("response_metadata", S.obj [ ("next_cursor", cursor) ]);
                   ]))
           ()))
    [ `Null; `Int 0; `Bool false ];
  List.iter
    (fun key ->
      List.iter
        (fun value ->
          expect_fail ("typed user flag " ^ key) Transient
            (Indeterminate "users.list")
            (eval
               ~mutate:(fun e _ -> S.edit_user e.m.world "U456" (set key value))
               ()))
        [ `Null; `String "false"; `Int 0 ])
    [ "deleted"; "is_bot"; "is_restricted"; "is_ultra_restricted" ]

let test_final_scope () =
  List.iter
    (fun column ->
      let e = env () in
      let o = S.seed e.db () in
      let stage = fire_stage e o in
      S.override e.m "users.list" (fun params ->
          Sql_util.exec_with_params e.db
            ("UPDATE slack_standing_orders SET " ^ column ^ " = ? WHERE id = ?")
            [
              Sqlite3.Data.TEXT
                (if column = "enterprise_id" then "E999" else "C999");
              Sqlite3.Data.TEXT o.id;
            ];
          (200, S.default_response e.m.world "users.list" params));
      expect_fail "scope read last" Hard Identity_changed
        (evaluate e ~stage (G.subject_of_order o)))
    [ "channel_id"; "enterprise_id" ]

let test_share_flags () =
  let flags =
    [ "is_ext_shared"; "is_shared"; "is_org_shared"; "is_pending_ext_shared" ]
  in
  List.iter
    (fun key ->
      expect_fail key Hard Channel_shared
        (eval ~mutate:(channel_with [ (key, `Bool true) ]) ());
      List.iter
        (fun value ->
          expect_fail key Transient (Indeterminate "conversations.info")
            (eval ~mutate:(channel_with [ (key, value) ]) ()))
        [ `Null; `String "false" ])
    flags

let suite =
  [
    S.case "unannounced order cannot fire" test_unannounced;
    S.case "fire binding refuses missing and foreign fires" test_binding;
    S.case "tracking paused dispatch is read only" test_tracking;
    S.case "missing identity metadata is transient" test_missing_identity;
    S.case "malformed membership cursor and typed user flags"
      test_typed_membership;
    S.case "scope identity is read last" test_final_scope;
    S.case "shared hard; missing share flag transient" test_share_flags;
  ]

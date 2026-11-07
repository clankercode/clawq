open Test_slack_standing_order_retention_support

let test_inactive_production () =
  let db = S.fresh_db () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let order, fire = fixture db in
      let m = mock () in
      let manager = Session.create ~config:(S.config ()) ~db () in
      (* Control: this is dispatched durable work and audited transport can read it. *)
      let v =
        Result.get_ok
          (Lwt_main.run
             (Grant.evaluate ~db
                ~current_config:(fun () -> Session.get_config manager)
                ~http:m.http
                ~stage:(Grant.Track { fire_id = fire.id })
                ~unit_label:"control"
                (Grant.subject_of_order order)))
      in
      ignore (Lwt_main.run (v.call "files.info" [ ("file", str "FSOURCE") ]));
      eq "control reaches source HTTP" 1 (count m "files.info");
      m.calls := [];
      Lwt_main.run
        (Reconcile.pass_with ~ctx:(context ()) ~http:m.http ~db ~manager order);
      check "inactive production pass polls verified sources" true
        (count m "files.info" > 0);
      eq "inactive production pass deletes exact own file" 1
        (count m "files.delete");
      eq "inactive production pass posts one fixed note" 1
        (count m "chat.postMessage"))

let suite =
  [
    S.case "inactive production retention survives pause epoch"
      test_inactive_production;
  ]

let test_states state () =
  with_fixture ~state:T.Active (fun db o f m mgr ->
      add_attempt db f;
      let o =
        match state with
        | T.Active -> o
        | _ ->
            let op =
              match state with
              | T.Paused -> Store.Pause "U123"
              | T.Suspended -> Store.Suspend "test"
              | T.Cancelled -> Store.Cancel "U123"
              | _ -> assert false
            in
            (Result.get_ok (Store.deactivate db ~order_id:o.id op ~now:S.now))
              .after
      in
      let before = Store.runtime db ~fire_id:f.id in
      run db o m mgr;
      Alcotest.(check (list string))
        "both attempts exact own IDs"
        [ "FOWN0"; "FOWN1"; "FOWN2" ]
        (deleted m);
      eq "note per retained relay thread" 2 (count m "chat.postMessage");
      check "closed cleanup" true (complete db f.id);
      check "attempt, pickup and relay identity unchanged" true
        (Store.runtime db ~fire_id:f.id = before);
      List.iter
        (fun (_, method_, p) ->
          if method_ = "chat.postMessage" then (
            Alcotest.(check string)
              "fixed copy" Scope.note_text (S.param p "text");
            check "only retained relay thread" true
              (List.mem (S.param p "thread_ts")
                 [ "1699999940.000001"; "1699999970.000001" ])))
        !(m.calls);
      eq "no discovery membership" 0 (count m "conversations.members");
      eq "no relay uploads" 0 (count m "files.completeUploadExternal");
      let audits =
        Egress_audit.query ~db
          ~tool_name:("standing_order:" ^ o.id ^ ":cleanup:" ^ f.id)
          ()
      in
      check "cleanup audit under own order/fire" true (audits <> []);
      run db o m mgr;
      eq "finished cleanup is never reposted" 2 (count m "chat.postMessage"))

let test_no_grant () =
  with_fixture
    ~config:
      (S.with_allow_users
         (S.config ~targets:[] ~creator:So_creator_admin ())
         [ "USOMEONEELSE" ])
    (fun db o f m mgr ->
      S.remove_member m.world "U123";
      S.remove_member m.world "UCLAUDE";
      run db o m mgr;
      check "own cleanup needs no target/grantor grant" true (complete db f.id);
      eq "only installation validation" 0 (count m "users.list"))

let test_source_matrix status body () =
  with_fixture (fun db o _ m mgr ->
      S.override m "files.info" (fun _ -> (status, body));
      run db o m mgr;
      no_writes m;
      eq "each source inspected once" 2 (count m "files.info");
      run db o m mgr;
      eq "no repeat inside 300s" 2 (count m "files.info");
      S.at (S.now +. 299.);
      run db o m mgr;
      eq "299s still held" 2 (count m "files.info");
      S.at (S.now +. 300.);
      run db o m mgr;
      eq "300s permits next poll" 4 (count m "files.info"))

let test_typed_sources () =
  let state fields =
    Retention.source_state ~id:"FSOURCE"
      (Ok (S.ok [ ("file", obj (("id", str "FSOURCE") :: fields)) ]))
  in
  List.iter
    (fun fields ->
      check "typed deletion" true (state fields = Retention.Missing))
    [
      [ ("deleted", `Bool true) ];
      [ ("is_deleted", `Bool true) ];
      [ ("is_tombstoned", `Bool true) ];
      [ ("mode", str "tombstone") ];
    ];
  List.iter
    (fun fields ->
      check "malformed never deletion" true (state fields = Retention.Unknown))
    [
      [ ("deleted", str "true") ];
      [ ("is_tombstoned", `Int 1) ];
      [ ("deleted", `Bool true); ("mode", `Int 1) ];
      [ ("is_deleted", `Null); ("deleted", `String "true") ];
    ];
  check "present" true (state [ ("deleted", `Bool false) ] = Retention.Present);
  check "wrong file never deletion" true
    (Retention.source_state ~id:"FOTHER"
       (Ok
          (S.ok
             [
               ("file", obj [ ("id", str "FSOURCE"); ("deleted", `Bool true) ]);
             ]))
    = Retention.Unknown)

let test_local_denial cfg () =
  with_fixture ~config:cfg (fun db o _ m mgr ->
      run db o m mgr;
      eq "local refusal has zero HTTP" 0 (List.length !(m.calls)))

let test_identity method_ body () =
  with_fixture (fun db o _ m mgr ->
      S.override m method_ (fun _ -> (200, body));
      run db o m mgr;
      no_writes m;
      eq "no sources before verified current identity" 0 (count m "files.info"))

let test_rotation () =
  with_fixture ~config:(S.config ~bot_token:"xoxb-rotated-fixture" ())
    (fun db o f m mgr ->
      let tokens = ref [] in
      let http ~verb ~token ~method_ ~params =
        tokens := token :: !tokens;
        m.http ~verb ~token ~method_ ~params
      in
      Lwt_main.run (Retention.pass ~ctx:(context ()) ~db ~manager:mgr ~http o);
      check "same installation rotated credentials succeed" true
        (complete db f.id);
      check "all calls use current token" true
        (List.for_all (( = ) "xoxb-rotated-fixture") !tokens))

let test_age days eligible () =
  with_fixture (fun db o f m mgr ->
      let at = S.now -. float_of_int (days * 86400) in
      let a =
        {
          (Store.empty_attempt 0) with
          uploaded_file_ids = [ "FOWN0" ];
          dispatched_at = Some at;
          relay_ts = Some (Printf.sprintf "%.6f" at);
        }
      in
      Store.save_attempt db f.id a;
      Sql.exec db
        "UPDATE slack_standing_order_fires SET created_at='2000-01-01 \
         00:00:00' WHERE id=?"
        [ Sql.text f.id ];
      run db o m mgr;
      eq "actual relay age controls selection"
        (if eligible then 1 else 0)
        (count m "files.delete"))

let test_future_unknown () =
  with_fixture (fun db o f m mgr ->
      List.iter
        (fun (dispatch, relay) ->
          let a =
            {
              (Store.empty_attempt 0) with
              uploaded_file_ids = [ "FOWN0" ];
              dispatched_at = dispatch;
              relay_ts = relay;
            }
          in
          Store.save_attempt db f.id a;
          run db o m mgr;
          no_writes m)
        [
          (Some (S.now +. 1.), Some "1700000001.000001");
          (None, Some "1699999940.000001");
          (Some (S.now -. 60.), Some "bad");
        ])

let test_expiry () =
  with_fixture (fun db o f m mgr ->
      S.override m "files.info" (fun p ->
          ( 200,
            S.ok
              [
                ("file", obj [ ("id", Slack_standing_order_pages.get "file" p) ]);
              ] ));
      S.at (S.now +. (29. *. 86400.));
      run db o m mgr;
      eq "day29 polled" 2 (count m "files.info");
      S.at (S.now +. (31. *. 86400.));
      run db o m mgr;
      eq "day31 watching expires" 2 (count m "files.info");
      check "expired metadata remains inspectable" true
        ((Option.get (Cleanup.find db f.id)).status = "expired"))

let test_narrow_call () =
  with_fixture (fun db o f m mgr ->
      Cleanup.refresh db o ~now:S.now;
      let v =
        Result.get_ok
          (Lwt_main.run
             (Grant.evaluate ~db
                ~current_config:(fun () -> Session.get_config mgr)
                ~http:m.http
                ~stage:(Grant.Cleanup { fire_id = f.id })
                ~unit_label:"cleanup" (Grant.subject_of_order o)))
      in
      let before = List.length !(m.calls) in
      List.iter
        (fun (method_, params) ->
          check
            ("narrow refusal " ^ method_)
            true
            (Result.is_error (Lwt_main.run (v.call method_ params))))
        [
          ("files.info", [ ("file", str "FOWN0") ]);
          ("files.delete", [ ("file", str "FSOURCE") ]);
          ("files.delete", [ ("file", str "FOTHERBOT") ]);
          ("files.getUploadURLExternal", []);
          ("files.completeUploadExternal", []);
          ("auth.test", []);
          ( "chat.postMessage",
            [ ("channel", str "C123"); ("text", str "arbitrary") ] );
        ];
      eq "refused calls zero HTTP" before (List.length !(m.calls));
      ignore (Lwt_main.run (v.call "files.info" [ ("file", str "FSOURCE") ]));
      eq "source positive control" 1 (count m "files.info"))

let suite =
  suite
  @ [
      S.case "cleanup independent of grantor target and admin allowlists"
        test_no_grant;
      S.case "typed explicit deletion parser" test_typed_sources;
      S.case "rotated token matching installation" test_rotation;
      S.case "relay day29 selected despite old creation" (test_age 29 true);
      S.case "relay day30 included" (test_age 30 true);
      S.case "relay day31 excluded" (test_age 31 false);
      S.case "future unknown malformed dispatch excluded" test_future_unknown;
      S.case "watch expires after day30" test_expiry;
      S.case "cleanup call cannot upload relay or delete sources"
        test_narrow_call;
    ]
  @ List.map
      (fun state ->
        S.case ("retention in " ^ T.state_to_string state) (test_states state))
      T.[ Active; Paused; Suspended; Cancelled ]
  @ List.map
      (fun code ->
        S.case ("unknown source " ^ code)
          (test_source_matrix 200
             (obj [ ("ok", `Bool false); ("error", str code) ])))
      [ "missing_scope"; "no_permission"; "inaccessible"; "not_authed" ]
  @ List.map
      (fun status ->
        S.case
          ("unknown HTTP " ^ string_of_int status)
          (test_source_matrix status (obj [])))
      [ 429; 500; 503 ]
  @ [
      S.case "malformed successful source unknown"
        (test_source_matrix 200
           (S.ok
              [
                ("file", obj [ ("id", str "FSOURCE"); ("deleted", str "true") ]);
              ]));
    ]
  @ List.map
      (fun name ->
        S.case
          ("zero HTTP capability " ^ name)
          (test_local_denial (S.profile (S.config ()) ~denied:[ name ] ())))
      SO.capability_names
  @ [
      S.case "zero HTTP strict egress"
        (test_local_denial (S.config ~strict_rules:[] ()));
      S.case "zero HTTP disabled feature"
        (test_local_denial (S.config ~enabled:false ()));
    ]
  @ List.map
      (fun (name, method_, body) -> S.case name (test_identity method_ body))
      [
        ( "wrong current team",
          "auth.test",
          S.ok
            [
              ("team_id", str "TOTHER");
              ("user_id", str "UBOT");
              ("bot_id", str "BOWN");
            ] );
        ( "wrong current enterprise",
          "auth.test",
          S.ok
            [
              ("team_id", str "T123");
              ("enterprise_id", str "EOTHER");
              ("user_id", str "UBOT");
              ("bot_id", str "BOWN");
            ] );
        ( "wrong own user",
          "auth.test",
          S.ok
            [
              ("team_id", str "T123");
              ("user_id", str "UOTHER");
              ("bot_id", str "BOWN");
            ] );
        ( "wrong own bot",
          "auth.test",
          S.ok
            [
              ("team_id", str "T123");
              ("user_id", str "UBOT");
              ("bot_id", str "BOTHER");
            ] );
        ( "wrong current app",
          "bots.info",
          S.ok
            [
              ( "bot",
                obj
                  [
                    ("id", str "BOWN");
                    ("user_id", str "UBOT");
                    ("app_id", str "AOTHER");
                  ] );
            ] );
        ( "bots response own identity mismatch",
          "bots.info",
          S.ok
            [
              ( "bot",
                obj
                  [
                    ("id", str "BOTHER");
                    ("user_id", str "UBOT");
                    ("app_id", str "A123");
                  ] );
            ] );
        ( "failed auth lookup",
          "auth.test",
          obj [ ("ok", `Bool false); ("error", str "invalid_auth") ] );
        ( "failed bots lookup",
          "bots.info",
          obj [ ("ok", `Bool false); ("error", str "bot_not_found") ] );
      ]

(* Slack_standing_order_slack: the closed method list, the fail-closed
   standing-order snapshot, and the egress-checked, audited, typed call. *)
module S = Test_slack_standing_order_support
module SO = Slack_standing_order_slack

let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)

let error_t =
  Alcotest.testable
    (fun ppf e -> Format.pp_print_string ppf (SO.error_code e))
    ( = )

let order_ref =
  Some { Access_snapshot.order_id = "so_1"; fire_id = Some "fire_1" }

let snapshot db cfg =
  match SO.snapshot ~db ~config:cfg ~channel_id:"C123" ~order_ref with
  | Ok snap -> snap
  | Error e -> Alcotest.fail ("snapshot: " ^ e)

(* A call under a fresh snapshot, reading [cfg] on every invocation. *)
let make ?http db (m : S.mock) cfg =
  let snap = snapshot db !cfg in
  ( snap,
    SO.make ~db
      ~current_config:(fun () -> !cfg)
      ~snapshot:snap ~channel_id:"C123" ~audit:"standing_order:so_1:plan"
      ~http:(Option.value http ~default:m.S.http)
      () )

let run (call : SO.call) method_ params = Lwt_main.run (call method_ params)

let expect_error what expected = function
  | Ok _ -> Alcotest.failf "%s: expected Error %s" what (SO.error_code expected)
  | Error e -> Alcotest.check error_t what expected e

let expect_ok what = function
  | Ok json -> json
  | Error e -> Alcotest.failf "%s: unexpected Error %s" what (SO.error_code e)

let user_params = [ ("user", `String "U123") ]
let no_http what (m : S.mock) = check_bool what true (!(m.calls) = [])

let test_unlisted_method () =
  let db = S.fresh_db () and m = S.mock () in
  let _, call = make db m (ref (S.config ())) in
  expect_error "unlisted method" (Method_not_permitted "files.unknown")
    (run call "files.unknown" [ ("file", `String "F1") ]);
  no_http "no HTTP for an unlisted method" m

let test_verbs () =
  check_bool "auth.test is POST" true
    (List.assoc_opt "auth.test" SO.methods = Some "POST");
  check_bool "users.info is GET" true
    (List.assoc_opt "users.info" SO.methods = Some "GET");
  check_bool "chat.postMessage is POST" true
    (List.assoc_opt "chat.postMessage" SO.methods = Some "POST");
  List.iter
    (fun (method_, verb) ->
      check_bool
        (method_ ^ " verb is GET or POST")
        true
        (verb = "GET" || verb = "POST"))
    SO.methods;
  check_bool "sixteen methods" true (List.length SO.methods = 16)

let test_egress_denied_before_transport () =
  let db = S.fresh_db () and m = S.mock () in
  let cfg =
    S.config ~strict_rules:[ S.slack_rule ~method_:"GET" "/api/users.info" ] ()
  in
  let _, call = make db m (ref cfg) in
  expect_error "members are not allowed" (Egress_denied "conversations.members")
    (run call "conversations.members" [ ("channel", `String "C123") ]);
  no_http "no HTTP after an egress denial" m;
  expect_error "a POST is not allowed" (Egress_denied "auth.test")
    (run call "auth.test" []);
  no_http "no HTTP for a denied POST" m;
  let json =
    expect_ok "users.info is allowed" (run call "users.info" user_params)
  in
  check_string "the user is returned" "U123"
    Yojson.Safe.Util.(json |> member "user" |> member "id" |> to_string);
  check_bool "only users.info reached HTTP" true
    (!(m.calls) = [ ("GET", "users.info", `Assoc user_params) ])

let test_audit_tag () =
  let db = S.fresh_db () and m = S.mock () in
  let snap, call = make db m (ref (S.config ())) in
  ignore (expect_ok "users.info" (run call "users.info" user_params));
  match Egress_audit.query ~db ~tool_name:"standing_order:so_1:plan" () with
  | [] -> Alcotest.fail "no egress audit row for the standing-order call"
  | events ->
      List.iter
        (fun (e : Egress_audit.event) ->
          check_bool "audit row names the snapshot" true
            (e.snapshot_id = Some snap.id);
          check_bool "audit row names the channel key" true
            (e.session_key = Some "slack:C123");
          check_bool "allowed" true (e.decision = Egress_audit.Allowed))
        events

let test_unauditable_egress () =
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      Access_snapshot.init_schema db;
      let m = S.mock () in
      let _, call = make db m (ref (S.config ())) in
      expect_error "no audit table" Audit_unavailable
        (run call "users.info" user_params);
      no_http "no HTTP without an audit row" m)

let test_channel_and_credentials () =
  let db = S.fresh_db () and m = S.mock () in
  let cfg =
    ref
      (S.with_slack (S.config ()) (fun sc ->
           { sc with allow_channels = [ "C999" ] }))
  in
  let _, call = make db m cfg in
  expect_error "channel outside allow_channels" Channel_denied
    (run call "users.info" user_params);
  cfg := S.config ~bot_token:"" ();
  expect_error "empty bot token" Not_configured
    (run call "users.info" user_params);
  cfg := S.config ~bot_token:"   " ();
  expect_error "blank bot token" Not_configured
    (run call "users.info" user_params);
  let base = S.config () in
  cfg := { base with channels = { base.channels with slack = None } };
  expect_error "no Slack section" Not_configured
    (run call "users.info" user_params);
  no_http "no HTTP without a channel or credentials" m

let peer_call db cfg (m : S.mock) =
  let snap =
    match SO.peer_lookup_snapshot ~db ~config:!cfg ~channel_id:"D123" with
    | Ok snap -> snap
    | Error e -> Alcotest.fail e
  in
  ( snap,
    SO.make_peer_lookup ~db
      ~current_config:(fun () -> !cfg)
      ~snapshot:snap ~channel_id:"D123" ~audit:"slack_peer_thread" ~http:m.http
      () )

let dm_config cfg =
  S.with_slack cfg (fun sc -> { sc with allow_channels = [ "D123" ] })

let test_peer_lookup_scope () =
  let db = S.fresh_db () and m = S.mock () in
  let cfg = ref (dm_config (S.config ())) in
  let snap, call = peer_call db cfg m in
  check_bool "peer lookup uses a room-turn snapshot" true
    (snap.work_type = Access_snapshot.Room_turn && snap.standing_order = None);
  check_bool "peer snapshot is persisted" true
    (Access_snapshot.get_by_id ~db snap.id = Some snap);
  let standing_call =
    SO.make ~db
      ~current_config:(fun () -> !cfg)
      ~snapshot:snap ~channel_id:"D123" ~audit:"standing_order:none:plan"
      ~http:m.http ()
  in
  expect_error "standing orders still reject explicitly allowed DMs"
    Channel_denied
    (run standing_call "users.info" user_params);
  List.iter
    (fun method_ ->
      expect_error "peer scope permits only users.info"
        (Method_not_permitted method_) (run call method_ []))
    [ "chat.postMessage"; "conversations.info"; "users.list" ];
  no_http "rejected scopes never reach HTTP" m;
  ignore (expect_ok "DM peer lookup" (run call "users.info" user_params));
  check_bool "only peer GET reached HTTP" true
    (!(m.calls) = [ ("GET", "users.info", `Assoc user_params) ]);
  match Egress_audit.query ~db ~tool_name:"slack_peer_thread" () with
  | [ e ] ->
      check_bool "DM peer call is audited under its persisted snapshot" true
        (e.snapshot_id = Some snap.id
        && e.session_key = Some "slack:D123"
        && e.decision = Egress_audit.Allowed)
  | _ -> Alcotest.fail "expected one peer lookup audit row"

let test_peer_lookup_policy () =
  let db = S.fresh_db () and m = S.mock () in
  let cfg = ref (dm_config (S.config ~strict_rules:[] ())) in
  let _, call = peer_call db cfg m in
  expect_error "DM peer lookup obeys egress policy" (Egress_denied "users.info")
    (run call "users.info" user_params);
  no_http "denied peer lookup never reaches HTTP" m;
  cfg := dm_config (S.config ());
  ignore
    (expect_ok "updated policy allows lookup"
       (run call "users.info" user_params));
  m.calls := [];
  cfg := S.with_slack !cfg (fun sc -> { sc with allow_channels = [ "C123" ] });
  expect_error "current channel allowlist is checked" Channel_denied
    (run call "users.info" user_params);
  cfg := dm_config (S.config ~bot_token:"" ());
  expect_error "current credentials are checked" Not_configured
    (run call "users.info" user_params);
  no_http "revoked channel or credentials stop peer HTTP" m

let test_peer_lookup_unauditable () =
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      Access_snapshot.init_schema db;
      let m = S.mock () in
      let _, call = peer_call db (ref (dm_config (S.config ()))) m in
      expect_error "peer lookup requires its audit row" Audit_unavailable
        (run call "users.info" user_params);
      no_http "unauditable peer lookup never reaches HTTP" m)

let test_typed_errors () =
  let db = S.fresh_db () and m = S.mock () in
  let _, call = make db m (ref (S.config ())) in
  let answer status body =
    S.override m "users.info" (fun _ -> (status, body))
  in
  answer 429 (S.ok []);
  expect_error "HTTP 429" Rate_limited (run call "users.info" user_params);
  answer 503 (S.ok []);
  expect_error "HTTP 503" (Http_status 503) (run call "users.info" user_params);
  answer 200 (S.obj [ ("ok", `Bool false); ("error", `String "ratelimited") ]);
  expect_error "ratelimited body" Rate_limited
    (run call "users.info" user_params);
  answer 200 (S.obj [ ("ok", `Bool false); ("error", `String "rate_limited") ]);
  expect_error "rate_limited body" Rate_limited
    (run call "users.info" user_params);
  answer 200 (S.obj [ ("ok", `Bool false); ("error", `String "missing_scope") ]);
  expect_error "missing_scope" (Slack_error "missing_scope")
    (run call "users.info" user_params);
  answer 200 (`String "nope");
  expect_error "a non-object body" Transport (run call "users.info" user_params);
  let raw ~verb:_ ~token:_ ~method_:_ ~params:_ = Lwt.return (200, "nope") in
  let _, raw_call = make ~http:raw db m (ref (S.config ())) in
  expect_error "an unparseable body" Transport
    (run raw_call "users.info" user_params);
  let raising ~verb:_ ~token:_ ~method_:_ ~params:_ =
    failwith "connection reset"
  in
  let _, raising_call = make ~http:raising db m (ref (S.config ())) in
  expect_error "a raising transport" Transport
    (run raising_call "users.info" user_params);
  let cancelled ~verb:_ ~token:_ ~method_:_ ~params:_ = Lwt.fail Lwt.Canceled in
  let _, cancelled_call = make ~http:cancelled db m (ref (S.config ())) in
  check_bool "cancellation propagates" true
    (match run cancelled_call "users.info" user_params with
    | _ -> false
    | exception Lwt.Canceled -> true)

let test_error_codes () =
  List.iter
    (fun (e, code) -> check_string code code (SO.error_code e))
    [
      (SO.Rate_limited, "rate_limited");
      (Http_status 503, "http_503");
      (Transport, "transport");
      (Audit_unavailable, "audit_unavailable");
      (Snapshot_unavailable, "snapshot_unavailable");
      (Slack_error "missing_scope", "slack:missing_scope");
      (Egress_denied "chat.postMessage", "egress_denied:chat.postMessage");
      (Method_not_permitted "files.delete", "method_not_permitted:files.delete");
      (Channel_denied, "channel_denied");
      (Not_configured, "not_configured");
    ];
  let db = S.fresh_db () and m = S.mock () in
  let _, call = make db m (ref (S.config ())) in
  S.override m "users.info" (fun _ ->
      (200, S.obj [ ("ok", `Bool false); ("error", `String "<!channel> hi") ]));
  expect_error "a malformed Slack code is never quoted" (Slack_error "unknown")
    (run call "users.info" user_params)

let test_reads_config_per_call () =
  let db = S.fresh_db () and m = S.mock () in
  let tokens = ref [] in
  let http ~verb ~token ~method_ ~params =
    tokens := !tokens @ [ token ];
    m.http ~verb ~token ~method_ ~params
  in
  let cfg = ref (S.config ()) in
  let _, call = make ~http db m cfg in
  ignore (expect_ok "first call" (run call "users.info" user_params));
  cfg := S.config ~bot_token:" xoxb-second-token " ();
  ignore (expect_ok "second call" (run call "users.info" user_params));
  check_bool "each call reads the current, trimmed token" true
    (!tokens = [ "xoxb-test"; "xoxb-second-token" ])

let test_snapshot_shape () =
  let db = S.fresh_db () in
  let snap = snapshot db (S.profile (S.config ()) ~denied:[ "x" ] ()) in
  check_bool "work type" true (snap.work_type = Access_snapshot.Standing_order);
  check_bool "session key" true (snap.session_key = Some "slack:C123");
  check_bool "room id" true (snap.room_id = Some "C123");
  check_bool "order ref" true (snap.standing_order = order_ref);
  check_bool "channel profile" true (snap.profile_id = Some "so");
  match Access_snapshot.get_by_id ~db snap.id with
  | Some found -> check_bool "persisted unchanged" true (found = snap)
  | None -> Alcotest.fail "the snapshot was not persisted"

let test_snapshot_fails_closed () =
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      match
        SO.snapshot ~db ~config:(S.config ()) ~channel_id:"C123" ~order_ref
      with
      | Error e -> check_string "error" "snapshot_not_persisted" e
      | Ok _ -> Alcotest.fail "an unpersisted snapshot must not be returned")

let test_default_http () =
  let seen = ref [] in
  let transport : Slack_transport.t =
    {
      get =
        (fun ~uri ~headers ->
          seen := !seen @ [ ("GET", uri, headers, "") ];
          Lwt.return (200, "{}"));
      post =
        (fun ~uri ~headers ~body ->
          seen := !seen @ [ ("POST", uri, headers, body) ];
          Lwt.return (200, "{}"));
    }
  in
  Lwt_main.run
    (Slack_transport.with_http transport (fun () ->
         let open Lwt.Syntax in
         let* _ =
           SO.default_http ~verb:"GET" ~token:"xoxb-t" ~method_:"users.info"
             ~params:[ ("user", `String "U123"); ("limit", `Int 200) ]
         in
         let* _ =
           SO.default_http ~verb:"POST" ~token:"xoxb-t"
             ~method_:"chat.postMessage"
             ~params:[ ("channel", `String "C123") ]
         in
         Lwt.return_unit));
  let bearer = [ ("Authorization", "Bearer xoxb-t") ] in
  check_bool "GET sends a query string; POST a JSON body" true
    (!seen
    = [
        ( "GET",
          "https://slack.com/api/users.info?user=U123&limit=200",
          bearer,
          "" );
        ( "POST",
          "https://slack.com/api/chat.postMessage",
          bearer,
          {|{"channel":"C123"}|} );
      ])

let test_labels () =
  check_string "access key" "slack:C123" (SO.access_key ~channel_id:"C123");
  check_string "audit label" "standing_order:so_1:plan"
    (SO.audit_label ~order_id:(Some "so_1") ~unit_label:"plan");
  check_string "audit label without an order" "standing_order:none:plan"
    (SO.audit_label ~order_id:None ~unit_label:"plan")

let phase3_methods =
  [
    ("conversations.history", "GET");
    ("conversations.replies", "GET");
    ("files.info", "GET");
    ("canvases.getContent", "POST");
    ("reactions.get", "GET");
    ("files.getUploadURLExternal", "GET");
    ("files.completeUploadExternal", "POST");
    ("files.delete", "POST");
  ]

let test_phase3_audit () =
  List.iter
    (fun (method_, verb) ->
      List.iter
        (fun allowed ->
          let db = S.fresh_db () and m = S.mock () in
          let cfg =
            S.config
              ~strict_rules:
                (if allowed then
                   [ S.slack_rule ~method_:verb ("/api/" ^ method_) ]
                 else [])
              ()
          in
          let snap = snapshot db cfg in
          S.override m method_ (fun _ -> (200, S.ok []));
          let label =
            SO.audit_label ~order_id:(Some "so_1") ~unit_label:"fire_1"
          in
          let call =
            SO.make ~db
              ~current_config:(fun () -> cfg)
              ~snapshot:snap ~channel_id:"C123" ~audit:label ~http:m.http ()
          in
          let params =
            match method_ with
            | "conversations.history" | "conversations.replies"
            | "reactions.get" ->
                [ ("channel", `String "C123") ]
            | "files.completeUploadExternal" ->
                [ ("channel_id", `String "C123") ]
            | _ -> []
          in
          let result = run call method_ params in
          if allowed then ignore (expect_ok method_ result)
          else expect_error method_ (Egress_denied method_) result;
          Alcotest.(check int)
            "HTTP count"
            (if allowed then 1 else 0)
            (List.length !(m.calls));
          check_bool "exact verb" true
            (List.assoc_opt method_ SO.methods = Some verb);
          if allowed then
            check_bool "exact dispatched verb and parameters" true
              (!(m.calls) = [ (verb, method_, `Assoc params) ]);
          match Egress_audit.query ~db () with
          | [ e ] ->
              check_bool "decision" true
                (e.decision = if allowed then Allowed else Denied);
              check_bool "snapshot and tag" true
                (e.snapshot_id = Some snap.id
                && e.tool_name = Some "standing_order:so_1:fire_1");
              check_bool "audited verb" true
                (e.method_redacted = Some (Egress_audit.redact_method verb))
          | _ -> Alcotest.fail "expected one audit row")
        [ true; false ])
    phase3_methods

let test_unpersisted () =
  let db = S.fresh_db () and m = S.mock () and cfg = S.config () in
  let snap =
    Access_snapshot.create ~config:cfg ~work_type:Standing_order
      ~session_key:"slack:C123" ~room_id:"C123" ?standing_order:order_ref ()
  in
  let call =
    SO.make ~db
      ~current_config:(fun () -> cfg)
      ~snapshot:snap ~channel_id:"C123" ~audit:"standing_order:so_1:fire_1"
      ~http:m.http ()
  in
  expect_error "not persisted" Snapshot_unavailable
    (run call "users.info" user_params);
  no_http "unpersisted snapshot costs zero HTTP" m;
  Alcotest.(check int)
    "zero policy checks" 0
    (List.length (Egress_audit.query ~db ()))

let test_channel_params () =
  let db = S.fresh_db () and m = S.mock () in
  let _, call = make db m (ref (S.config ())) in
  List.iter
    (fun method_ ->
      List.iter
        (fun params ->
          expect_error method_ Channel_denied (run call method_ params))
        [
          [ ("channel", `String "C456") ];
          [ ("channel_id", `String "C456") ];
          [ ("channels", `String "C123") ];
          [ ("channel", `Int 123) ];
          [ ("channel", `String "C123"); ("channel", `String "C456") ];
        ])
    (List.map fst SO.methods);
  expect_error "complete must be top level" Channel_denied
    (run call "files.completeUploadExternal"
       [ ("channel_id", `String "C123"); ("thread_ts", `String "1.0") ]);
  no_http "channel selectors cannot escape" m

let test_audit_step_failure () =
  List.iter
    (fun allowed ->
      let db = S.fresh_db () and m = S.mock () in
      let _, call =
        make db m
          (ref
             (S.config
                ~strict_rules:
                  (if allowed then [ S.slack_rule "/api/*" ] else [])
                ()))
      in
      Sql_util.exec_exn db
        "CREATE TRIGGER fail_audit BEFORE INSERT ON egress_audit BEGIN SELECT \
         RAISE(FAIL, 'test audit failure'); END";
      expect_error "failed audit is transient" Audit_unavailable
        (run call "users.info" user_params);
      no_http "audit failure refuses HTTP" m)
    [ true; false ]

let test_raising_config () =
  let db = S.fresh_db () and m = S.mock () in
  let snap = snapshot db (S.config ()) in
  let call =
    SO.make ~db
      ~current_config:(fun () -> failwith "config unavailable")
      ~snapshot:snap ~channel_id:"C123" ~audit:"standing_order:so_1:fire_1"
      ~http:m.http ()
  in
  expect_error "typed config failure" Transport
    (run call "users.info" user_params);
  no_http "config failure refuses HTTP" m

let test_limiter_reload () =
  let db = S.fresh_db () and m = S.mock () in
  let cfg = ref (S.config ()) and limited = ref 0 in
  let snap = snapshot db !cfg in
  let limit () =
    incr limited;
    let open Lwt.Syntax in
    let+ () = Lwt.pause () in
    cfg := S.config ~strict_rules:[] ()
  in
  let call =
    SO.make ~db
      ~current_config:(fun () -> !cfg)
      ~snapshot:snap ~channel_id:"C123" ~audit:"standing_order:so_1:fire_1"
      ~http:m.http ~limit ()
  in
  expect_error "reload after pacing" (Egress_denied "users.info")
    (run call "users.info" user_params);
  Alcotest.(check int) "limiter ran once" 1 !limited;
  no_http "policy reload while pacing stops HTTP" m

let test_mock_assertion () =
  let db = S.fresh_db () and m = S.mock () in
  let _, call = make db m (ref (S.config ())) in
  (* This permitted method deliberately lacks a response in this mock. *)
  ignore (run call "files.delete" []);
  check_bool "unexpected mock call recorded outside transport" true
    (!(m.failures) <> []);
  let failed_outside =
    try
      S.check_mock m;
      false
    with _ -> true
  in
  m.failures := [];
  check_bool "mock failure raises outside SO catch" true failed_outside;
  S.override m "users.info" (fun _ -> assert false);
  ignore (run call "users.info" user_params);
  check_bool "assertion failures recorded too" true (!(m.failures) <> []);
  let failed_outside =
    try
      S.check_mock m;
      false
    with _ -> true
  in
  m.failures := [];
  check_bool "assertion fails outside SO catch" true failed_outside

let test_snapshot_binding () =
  let db = S.fresh_db () and m = S.mock () and cfg = S.config () in
  let check snap =
    let call =
      SO.make ~db
        ~current_config:(fun () -> cfg)
        ~snapshot:snap ~channel_id:"C123" ~audit:"standing_order:so_1:fire_1"
        ~http:m.http ()
    in
    expect_error "snapshot binding" Snapshot_unavailable
      (run call "users.info" user_params)
  in
  let snap = snapshot db cfg in
  List.iter check
    [
      { snap with session_key = Some "slack:C456" };
      { snap with room_id = Some "C456" };
      { snap with work_type = Room_turn };
      {
        snap with
        standing_order = Some { order_id = "so_other"; fire_id = Some "fire_1" };
      };
    ];
  let wrong_type =
    Access_snapshot.create ~config:cfg ~work_type:Room_turn
      ~session_key:"slack:C123" ~room_id:"C123" ()
  in
  Access_snapshot.persist ~db wrong_type;
  check wrong_type;
  let wrong_key =
    Access_snapshot.create ~config:cfg ~work_type:Standing_order
      ~session_key:"slack:C456" ~room_id:"C456" ()
  in
  Access_snapshot.persist ~db wrong_key;
  check wrong_key;
  Sql_util.exec_exn db "DROP TABLE access_snapshots";
  check snap;
  no_http "mismatched or unreadable snapshots never reach HTTP" m;
  Alcotest.(check int)
    "snapshot refusal precedes policy" 0
    (List.length (Egress_audit.query ~db ()))

let test_failure_classification () =
  let module G = Slack_standing_order_grant in
  List.iter
    (fun error ->
      check_bool "grant error is indeterminate" true
        (G.reason_of_error ~user_not_found:G.Grantor_not_member "users.info"
           error
        = G.Indeterminate "users.info");
      check_bool "member error is indeterminate" true
        (G.member_refusal_of_error "users.info" error
        = G.Member_indeterminate "users.info");
      let call _ _ = Lwt.return (Error error) in
      let result =
        Lwt_main.run
          (Slack_standing_order_target.resolve ~call
             ~allowed:[ S.claude_target ] "UCLAUDE")
      in
      check_bool "target preserves typed failure" true
        (result = Error (Slack_standing_order_target.Lookup_failed error)))
    [ SO.Audit_unavailable; SO.Snapshot_unavailable ]

let test_no_http_debug_capture () =
  Test_helpers.with_temp_dir (fun dir ->
      let previous_home = Sys.getenv_opt Dot_dir.env_var in
      let previous_debug = !Http_debug.enabled_ref in
      Fun.protect
        ~finally:(fun () ->
          Unix.putenv Dot_dir.env_var (Option.value previous_home ~default:"");
          Http_debug.enabled_ref := previous_debug)
        (fun () ->
          (* A scratch directory, never the user's Clawq home. *)
          Unix.putenv Dot_dir.env_var dir;
          Http_debug.enabled_ref := true;
          let seen = ref [] in
          let callback _ request body =
            let open Lwt.Syntax in
            let* body = Cohttp_lwt.Body.to_string body in
            seen := (Cohttp.Request.meth request, body) :: !seen;
            Cohttp_lwt_unix.Server.respond_string ~status:`OK
              ~body:"secret-response-body" ()
          in
          Test_policy_http_client.with_http_server callback (fun port ->
              let uri =
                Printf.sprintf
                  "http://127.0.0.1:%d/secret-path?token=secret-query" port
              in
              let transport = SO.api_transport in
              let get_result, post_result =
                Lwt_main.run
                  (let open Lwt.Syntax in
                   let* get_result = transport.get ~uri ~headers:[] in
                   let+ post_result =
                     transport.post ~uri ~headers:[] ~body:"secret-request-body"
                   in
                   (get_result, post_result))
              in
              check_bool "GET response preserved" true
                (get_result = (200, "secret-response-body"));
              check_bool "POST response preserved" true
                (post_result = (200, "secret-response-body")));
          check_bool "both requests reached loopback" true
            (List.rev !seen = [ (`GET, ""); (`POST, "secret-request-body") ]);
          Alcotest.(check (list string))
            "HTTP debug creates no capture" []
            (Array.to_list (Sys.readdir dir))))

let suite =
  [
    S.case "default API adapter never captures HTTP bodies"
      test_no_http_debug_capture;
    S.case "snapshot scope and persisted row remain bound" test_snapshot_binding;
    S.case "audit and snapshot failures remain indeterminate"
      test_failure_classification;
    S.case "all phase3 methods audited" test_phase3_audit;
    S.case "unpersisted snapshot never calls HTTP" test_unpersisted;
    S.case "channel param cannot escape order" test_channel_params;
    S.case "audit step failure refuses dispatch" test_audit_step_failure;
    S.case "raising config returns typed transport error" test_raising_config;
    S.case "limiter reload precedes final preflight" test_limiter_reload;
    S.case "mock assertion is not swallowed" test_mock_assertion;
    S.case "unlisted method" test_unlisted_method;
    S.case "verbs" test_verbs;
    S.case "egress denied before transport" test_egress_denied_before_transport;
    S.case "audit tag" test_audit_tag;
    S.case "unauditable egress is denied" test_unauditable_egress;
    S.case "channel and credentials" test_channel_and_credentials;
    S.case "peer lookup admits DMs without widening standing orders"
      test_peer_lookup_scope;
    S.case "peer lookup checks current policy" test_peer_lookup_policy;
    S.case "peer lookup requires an audit row" test_peer_lookup_unauditable;
    S.case "typed errors" test_typed_errors;
    S.case "error codes" test_error_codes;
    S.case "reads config per call" test_reads_config_per_call;
    S.case "snapshot shape" test_snapshot_shape;
    S.case "snapshot fails closed" test_snapshot_fails_closed;
    S.case "access key and audit label" test_labels;
    S.case "default http shapes" test_default_http;
  ]

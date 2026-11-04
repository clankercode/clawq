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

let order_ref = Some { Access_snapshot.order_id = "so_1"; fire_id = None }

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
  expect_error "unlisted method" (Method_not_permitted "files.delete")
    (run call "files.delete" [ ("file", `String "F1") ]);
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
  check_bool "eight call sites" true (List.length SO.methods = 8)

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
      expect_error "no audit table" (Egress_denied "users.info")
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
      expect_error "peer lookup requires its audit row"
        (Egress_denied "users.info")
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

let suite =
  [
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

(** Tests for private GitHub user-authorization continuation delivery
    (P21.M2.E1.T003). *)

module D = Github_user_auth_delivery
module P = Principal_identity
module Tx = Github_user_auth_tx

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let assert_error = function
  | Error e -> e
  | Ok _ -> Alcotest.fail "expected Error"

let contains hay needle = Test_helpers.string_contains hay needle

let ctx ?(principal_id = "principal:alice")
    ?(continuation_handle = "cont:dm:handle-1") ?(tx_id = "tx_1")
    ?(source = Tx.Room "room-teams-1") ?(flow_kind = Tx.Web_pkce) () =
  assert_ok
    (D.make_delivery_context ~principal_id ~continuation_handle ~tx_id ~source
       ~flow_kind ())

let dm_channel () =
  assert_ok
    (D.make_private_connector_dm ~connector:P.Teams ~handle_id:"dm:alice")

let browser_channel () =
  assert_ok (D.make_principal_browser_continuation ~handle_id:"browser:alice")

let cli_channel () = assert_ok (D.make_initiating_cli ~handle_id:"cli:alice")

let auth_url =
  assert_ok
    (D.make_authorization_url
       ~url:
         "https://github.com/login/oauth/authorize?client_id=Iv1.abc&state=xyz")

let device_codes =
  assert_ok
    (D.make_device_codes ~user_code:"ABCD-1234"
       ~verification_uri:"https://github.com/login/device"
       ~verification_uri_complete:
         "https://github.com/login/device?user_code=ABCD-1234"
       ~device_code:"device_secret_should_stay_private" ())

let callback_err =
  assert_ok
    (D.make_callback_error ~code:"access_denied"
       ~message:"The user denied the request" ())

let account_ctrl =
  assert_ok
    (D.make_account_control ~prompt:"Select a GitHub account"
       ~options:[ "alice"; "alice-work" ] ())

let test_protocol_version () =
  Alcotest.(check int) "protocol_version" 1 D.protocol_version

let test_classify_content () =
  Alcotest.(check string)
    "progress class" "shared_room_progress"
    (D.string_of_content_class (D.classify_content `Progress));
  Alcotest.(check string)
    "url class" "private_auth_material"
    (D.string_of_content_class (D.classify_material_kind D.Authorization_url));
  Alcotest.(check string)
    "device class" "private_auth_material"
    (D.string_of_content_class (D.classify_material_kind D.Device_code));
  Alcotest.(check string)
    "error class" "private_auth_material"
    (D.string_of_content_class (D.classify_material_kind D.Callback_error));
  Alcotest.(check string)
    "account class" "private_auth_material"
    (D.string_of_content_class (D.classify_material_kind D.Account_control))

let test_channel_constructors () =
  Alcotest.(check bool)
    "dm private" true
    (D.delivery_channel_is_private (dm_channel ()));
  Alcotest.(check bool)
    "browser private" true
    (D.delivery_channel_is_private (browser_channel ()));
  Alcotest.(check bool)
    "cli private" true
    (D.delivery_channel_is_private (cli_channel ()));
  Alcotest.(check bool)
    "absent not private" false
    (D.delivery_channel_is_private D.Absent);
  match D.make_private_connector_dm ~connector:P.Slack ~handle_id:"  " with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "empty handle must fail"

let test_room_progress_only () =
  let progress =
    assert_ok
      (D.make_progress ~phase:"awaiting_authorization"
         ~detail:"Waiting for the authorizing Principal to continue" ())
  in
  let plan =
    D.route_delivery ~context:(ctx ()) ~channel:D.Absent
      ~content:(D.Progress progress) ()
  in
  match plan with
  | D.Room_progress rb ->
      Alcotest.(check string) "room id" "room-teams-1" rb.room_id;
      Alcotest.(check string) "phase" "awaiting_authorization" rb.progress.phase;
      Alcotest.(check bool)
        "room message safe" true
        (D.room_message_is_safe rb.rendered);
      Alcotest.(check bool)
        "mentions github auth" true
        (contains (String.lowercase_ascii rb.rendered) "authorization")
  | D.Private _ -> Alcotest.fail "progress must not be private-only"
  | D.Refused e -> Alcotest.fail ("unexpected refuse: " ^ e.message)

let test_progress_rejects_secret_detail () =
  (match
     D.make_progress ~phase:"awaiting"
       ~detail:"https://github.com/login/oauth/authorize?client_id=x" ()
   with
  | Error msg ->
      Alcotest.(check bool)
        "rejects url detail" true
        (contains (String.lowercase_ascii msg) "secret")
  | Ok _ -> Alcotest.fail "url in progress detail must fail");
  match D.make_progress ~phase:"awaiting" ~detail:"code ABCD-1234" () with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "device-shaped code in progress detail must fail"

let test_private_url_via_connector_dm () =
  let plan =
    D.route_delivery ~context:(ctx ()) ~channel:(dm_channel ())
      ~content:(D.Material auth_url) ()
  in
  match plan with
  | D.Private { private_delivery; companion_room } ->
      Alcotest.(check bool)
        "channel private" true
        (D.delivery_channel_is_private private_delivery.channel);
      Alcotest.(check bool)
        "url in private body" true
        (contains private_delivery.rendered
           "https://github.com/login/oauth/authorize");
      (match companion_room with
      | None -> Alcotest.fail "expected companion room progress"
      | Some rb ->
          Alcotest.(check string) "companion room" "room-teams-1" rb.room_id;
          Alcotest.(check bool)
            "companion safe" true
            (D.room_message_is_safe rb.rendered);
          Alcotest.(check bool)
            "no url leak" false
            (D.contains_private_secrets auth_url rb.rendered);
          Alcotest.(check bool)
            "neutral wording" true
            (contains (String.lowercase_ascii rb.rendered) "privately"));
      Alcotest.(check bool)
        "redacted summary no full url query" false
        (contains private_delivery.redacted_summary "client_id=Iv1")
  | D.Room_progress _ -> Alcotest.fail "auth url must not go room-only"
  | D.Refused e -> Alcotest.fail ("unexpected refuse: " ^ e.message)

let test_private_device_codes_via_browser () =
  let plan =
    D.route_delivery
      ~context:(ctx ~flow_kind:Tx.Device ())
      ~channel:(browser_channel ()) ~content:(D.Material device_codes) ()
  in
  match plan with
  | D.Private { private_delivery; companion_room } ->
      Alcotest.(check bool)
        "user code private" true
        (contains private_delivery.rendered "ABCD-1234");
      Alcotest.(check bool)
        "device_code private" true
        (contains private_delivery.rendered "device_secret_should_stay_private");
      (match companion_room with
      | Some rb ->
          Alcotest.(check bool)
            "no user code in room" false
            (contains rb.rendered "ABCD-1234");
          Alcotest.(check bool)
            "no device secret in room" false
            (contains rb.rendered "device_secret_should_stay_private");
          Alcotest.(check bool)
            "room safe" true
            (D.room_message_is_safe rb.rendered)
      | None -> Alcotest.fail "expected companion");
      let summary =
        D.redacted_private_summary ~context:(ctx ())
          ~channel:(browser_channel ()) ~content:(D.Material device_codes)
      in
      Alcotest.(check bool)
        "summary marks user_code present" true
        (contains summary "user_code=present");
      Alcotest.(check bool)
        "summary does not print user code" false
        (contains summary "ABCD-1234")
  | _ -> Alcotest.fail "expected private delivery"

let test_private_callback_error_via_cli () =
  let plan =
    D.route_delivery ~context:(ctx ()) ~channel:(cli_channel ())
      ~content:(D.Material callback_err) ()
  in
  match plan with
  | D.Private { private_delivery; companion_room } -> (
      Alcotest.(check bool)
        "error detail private" true
        (contains private_delivery.rendered "denied");
      match companion_room with
      | Some rb ->
          Alcotest.(check bool)
            "no raw error body forced" false
            (contains rb.rendered "The user denied the request");
          Alcotest.(check string)
            "error phase" "authorization_error" rb.progress.phase
      | None -> Alcotest.fail "expected companion")
  | _ -> Alcotest.fail "expected private delivery"

let test_private_account_control () =
  let plan =
    D.route_delivery ~context:(ctx ()) ~channel:(dm_channel ())
      ~content:(D.Material account_ctrl) ()
  in
  match plan with
  | D.Private { private_delivery; _ } ->
      Alcotest.(check bool)
        "prompt private" true
        (contains private_delivery.rendered "Select a GitHub account");
      Alcotest.(check bool)
        "options private" true
        (contains private_delivery.rendered "alice-work")
  | _ -> Alcotest.fail "expected private delivery"

let test_refuse_absent_channel () =
  let plan =
    D.route_delivery ~context:(ctx ()) ~channel:D.Absent
      ~content:(D.Material auth_url) ()
  in
  match plan with
  | D.Refused e -> (
      Alcotest.(check string)
        "reason" "no_private_channel"
        (D.string_of_refuse_reason e.reason);
      Alcotest.(check bool)
        "message actionable" true
        (contains (String.lowercase_ascii e.message) "private");
      Alcotest.(check bool)
        "no url in refuse message" false
        (contains e.message "client_id=Iv1");
      (match e.room_safe_progress with
      | None -> Alcotest.fail "expected room-safe progress on refuse"
      | Some p ->
          let rendered = D.render_room_progress p in
          Alcotest.(check bool)
            "room refuse safe" true
            (D.room_message_is_safe rendered);
          Alcotest.(check bool)
            "no url leak" false
            (D.contains_private_secrets auth_url rendered);
          Alcotest.(check bool)
            "explains refusal" true
            (contains (String.lowercase_ascii rendered) "private"));
      (* deliver maps refuse to Error *)
      match
        D.deliver ~context:(ctx ()) ~channel:D.Absent
          ~content:(D.Material auth_url) ()
      with
      | Error e2 ->
          Alcotest.(check string)
            "deliver reason" "no_private_channel"
            (D.string_of_refuse_reason e2.reason)
      | Ok _ -> Alcotest.fail "deliver must Error on absent channel")
  | D.Private _ -> Alcotest.fail "absent must not deliver private material"
  | D.Room_progress rb ->
      Alcotest.fail ("must not dump material to room: " ^ rb.rendered)

let test_refuse_invalid_channel_handle () =
  let bad = D.Private_connector_dm { connector = P.Teams; handle_id = "" } in
  match D.assert_private_channel bad with
  | Error e ->
      Alcotest.(check bool)
        "invalid channel" true
        (contains (D.string_of_refuse_reason e.reason) "invalid_channel")
  | Ok _ -> Alcotest.fail "empty handle must fail assert_private_channel"

let test_require_private_for_material () =
  (match
     D.require_private_for_material
       (D.Progress { phase = "x"; detail = None })
       D.Absent
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e.message);
  match D.require_private_for_material (D.Material auth_url) D.Absent with
  | Error e ->
      Alcotest.(check string)
        "no channel" "no_private_channel"
        (D.string_of_refuse_reason e.reason)
  | Ok () -> Alcotest.fail "material requires private channel"

let test_redacted_room_summary_never_secrets () =
  let summary =
    D.redacted_room_summary ~context:(ctx ()) ~content:(D.Material auth_url)
  in
  Alcotest.(check bool) "no full url" false (contains summary "client_id=Iv1");
  Alcotest.(check bool)
    "mentions private not shown" true
    (contains (String.lowercase_ascii summary) "not shown");
  Alcotest.(check bool)
    "room-safe heuristic" true
    (D.room_message_is_safe summary)

let test_redacted_json_export () =
  let plan =
    D.route_delivery ~context:(ctx ()) ~channel:(dm_channel ())
      ~content:(D.Material auth_url) ()
  in
  let json = D.delivery_plan_to_json_redacted plan in
  let s = Yojson.Safe.to_string json in
  Alcotest.(check bool)
    "json has private outcome" true (contains s "\"private\"");
  Alcotest.(check bool)
    "json omits full authorize query" false
    (contains s "client_id=Iv1");
  Alcotest.(check bool)
    "json marks url present" true
    (contains s "authorization_url_present");
  let refused =
    D.route_delivery ~context:(ctx ()) ~channel:D.Absent
      ~content:(D.Material device_codes) ()
  in
  let rs = Yojson.Safe.to_string (D.delivery_plan_to_json_redacted refused) in
  Alcotest.(check bool) "refused outcome" true (contains rs "refused");
  Alcotest.(check bool)
    "no device secret in json" false
    (contains rs "device_secret_should_stay_private");
  Alcotest.(check bool) "no user code in json" false (contains rs "ABCD-1234")

let test_context_of_tx () =
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      Tx.ensure_schema db;
      let actor =
        assert_ok
          (P.make_connector_actor_key ~connector:P.Teams
             ~tenant_or_workspace:"tenant-a" ~immutable_user_id:"user-1")
      in
      let app : Tx.app_client =
        { host = "github.com"; app_id = 1; client_id_handle = "h:cid" }
      in
      let tx =
        assert_ok
          (Tx.create ~db ~flow_kind:Tx.Device ~principal_id:"principal:alice"
             ~connector_actor:actor ~source:(Tx.Room "room-1") ~app
             ~base_revision:"rev1" ~continuation_handle:"cont:1"
             ~now:1_700_000_000.0 ~id:"tx_delivery" ())
      in
      let c = D.context_of_tx tx in
      Alcotest.(check string) "principal" "principal:alice" c.principal_id;
      Alcotest.(check string) "continuation" "cont:1" c.continuation_handle;
      Alcotest.(check (option string)) "tx id" (Some "tx_delivery") c.tx_id;
      let plan =
        D.route_delivery ~context:c ~channel:(cli_channel ())
          ~content:(D.Material device_codes) ()
      in
      match plan with
      | D.Private { companion_room = Some rb; _ } ->
          Alcotest.(check string) "room from tx" "room-1" rb.room_id
      | _ -> Alcotest.fail "expected private with companion from tx source")

let test_session_source_no_forced_room () =
  let context =
    assert_ok
      (D.make_delivery_context ~principal_id:"principal:alice"
         ~continuation_handle:"cont:1" ~source:(Tx.Session "sess-1") ())
  in
  let plan =
    D.route_delivery ~context ~channel:(cli_channel ())
      ~content:(D.Material auth_url) ()
  in
  match plan with
  | D.Private { companion_room = None; private_delivery } ->
      Alcotest.(check bool)
        "url delivered" true
        (contains private_delivery.rendered "oauth/authorize")
  | D.Private { companion_room = Some _; _ } ->
      Alcotest.fail "session source should not invent a Room companion"
  | _ -> Alcotest.fail "expected private delivery"

let test_shared_room_id_override () =
  let progress =
    assert_ok (D.make_progress ~phase:"completed" ~detail:"Linked" ())
  in
  let plan =
    D.route_delivery
      ~context:(ctx ~source:(Tx.Session "s1") ())
      ~channel:D.Absent ~content:(D.Progress progress)
      ~shared_room_id:"room-override" ()
  in
  match plan with
  | D.Room_progress rb ->
      Alcotest.(check string) "override room" "room-override" rb.room_id
  | _ -> Alcotest.fail "expected room progress"

let suite =
  [
    ("protocol version", `Quick, test_protocol_version);
    ("classify content", `Quick, test_classify_content);
    ("channel constructors", `Quick, test_channel_constructors);
    ("room receives neutral progress only", `Quick, test_room_progress_only);
    ( "progress rejects secret detail",
      `Quick,
      test_progress_rejects_secret_detail );
    ("private URL via connector DM", `Quick, test_private_url_via_connector_dm);
    ( "private device codes via browser",
      `Quick,
      test_private_device_codes_via_browser );
    ( "private callback error via CLI",
      `Quick,
      test_private_callback_error_via_cli );
    ("private account control", `Quick, test_private_account_control);
    ("refuse when private channel absent", `Quick, test_refuse_absent_channel);
    ("refuse invalid channel handle", `Quick, test_refuse_invalid_channel_handle);
    ("require private for material", `Quick, test_require_private_for_material);
    ("redacted room summary", `Quick, test_redacted_room_summary_never_secrets);
    ("redacted json export", `Quick, test_redacted_json_export);
    ("context_of_tx wires companion room", `Quick, test_context_of_tx);
    ( "session source no room companion",
      `Quick,
      test_session_source_no_forced_room );
    ("shared_room_id override", `Quick, test_shared_room_id_override);
  ]

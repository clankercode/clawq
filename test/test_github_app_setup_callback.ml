(** Tests for GitHub App setup callback exchange (P19.M2.E1.T002). *)

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Github_app_setup_tx.ensure_schema db;
  Github_app_setup_callback.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let principal =
  Github_app_setup_tx.
    { id = "principal:alice"; kind = "principal"; label = Some "Alice" }

let room_bind = Github_app_setup_tx.Room "room-teams-1"
let other_room = Github_app_setup_tx.Room "room-forged"
let public_base = "https://clawq.example.com"
let base_revision = "rev-config-abc"
let fixed_now = 1_700_000_000.0

let create_tx ?id ?state ?(bind = room_bind) ?scope ?(ttl_seconds = 1800.0)
    ?(now = fixed_now) ~db () =
  Github_app_setup_tx.create ~db ~principal ~bind ~base_revision
    ~public_base_url:public_base ~app_name:"Clawq" ~now ~ttl_seconds ?id ?state
    ?scope ()

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let contains hay needle = Test_helpers.string_contains hay needle

let sample_pem =
  "-----BEGIN RSA PRIVATE KEY-----\n\
   MIIEowIBAAKCAQEA0Z3VS5JJcds3xfn/fake-test-pem-not-a-real-key\n\
   -----END RSA PRIVATE KEY-----\n"

let conversion_json ?pem ?(app_id = 424242) () =
  let pem = match pem with Some p -> p | None -> sample_pem in
  Yojson.Safe.to_string
    (`Assoc
       [
         ("id", `Int app_id);
         ("slug", `String "clawq-test-app");
         ("client_id", `String "Iv1.testclientid");
         ("client_secret", `String "cs_super_secret");
         ("pem", `String pem);
         ("webhook_secret", `String "whsec_test_secret");
         ("html_url", `String "https://github.com/apps/clawq-test-app");
         ( "owner",
           `Assoc
             [
               ("login", `String "alice");
               ("id", `Int 1);
               ("type", `String "User");
             ] );
       ])

let make_store () =
  let stored = ref [] in
  let store_secret ~name ~plaintext =
    let handle = Printf.sprintf "sec:%s:%d" name (List.length !stored) in
    stored := (name, plaintext, handle) :: !stored;
    Ok handle
  in
  (store_secret, stored)

let ok_http ?(body = conversion_json ()) ?(status = 201) () ~url:_ ~headers:_
    ~body:_ =
  Ok (status, body)

let verify_installation ~app_id ~private_key_pem:_ ~installation_id =
  Ok
    (Github_app_installation_scope.with_revision
       {
         installation_id;
         app_id = Some app_id;
         account =
           { login = "acme-corp"; id = 99; account_type = "Organization" };
         selection = Github_app_installation_scope.All_repos;
         repositories = [];
         revoked_repositories = [];
         permissions = [ ("metadata", "read") ];
         status = Github_app_installation_scope.Active;
         revision = "";
         updated_at = Time_util.iso8601_utc ~t:fixed_now ();
       })

let make_req ?(callback_path = Github_app_setup_tx.default_callback_path)
    ?(expected_bind = room_bind) ?(expected_principal_id = principal.id)
    ?(installation_id = 99) ~code ~state () :
    Github_app_setup_callback.exchange_request =
  {
    code;
    state;
    callback_path = Some callback_path;
    expected_bind = Some expected_bind;
    expected_principal_id = Some expected_principal_id;
    installation_id = Some installation_id;
    setup_action = None;
  }

let test_happy_path () =
  with_db @@ fun db ->
  let tx =
    assert_ok (create_tx ~db ~id:"tx_happy" ~state:"state_happy_aaaaaaaa" ())
  in
  let store_secret, stored = make_store () in
  let result =
    assert_ok
      (Github_app_setup_callback.exchange ~db ~http_post:(ok_http ())
         ~verify_installation ~store_secret ~now:fixed_now
         (make_req ~code:"tmp_code_1" ~state:tx.state
            ~callback_path:Github_app_setup_tx.default_callback_path
            ~expected_bind:room_bind ~expected_principal_id:principal.id
            ~installation_id:99 ()))
  in
  Alcotest.(check string)
    "consumed"
    (Github_app_setup_tx.status_to_string Github_app_setup_tx.Consumed)
    (Github_app_setup_tx.status_to_string result.transaction.status);
  Alcotest.(check int) "app_id" 424242 result.app.app_id;
  Alcotest.(check int) "raw_app_id" 424242 result.raw_app_id;
  Alcotest.(check (option int)) "installation" (Some 99) result.installation_id;
  Alcotest.(check int)
    "verified scope app" 424242
    (Option.value result.verified_installation.app_id ~default:0);
  Alcotest.(check (option string))
    "slug" (Some "clawq-test-app") result.app.slug;
  Alcotest.(check bool)
    "client_id handle" true
    (String.starts_with ~prefix:"sec:" result.app.client_id_handle);
  Alcotest.(check bool)
    "private_key handle" true
    (String.starts_with ~prefix:"sec:" result.app.private_key_handle);
  Alcotest.(check int) "four secrets stored" 4 (List.length !stored);
  (* No plaintext secrets in receipt row. *)
  (match Github_app_setup_callback.get_receipt ~db ~id:result.receipt_id with
  | Ok (Some app) ->
      Alcotest.(check string)
        "receipt client_id handle" result.app.client_id_handle
        app.client_id_handle;
      Alcotest.(check int) "receipt app_id" 424242 app.app_id
  | Ok None -> Alcotest.fail "receipt missing"
  | Error e -> Alcotest.fail e);
  (* Stored plaintext includes PEM; handle does not. *)
  List.iter
    (fun (_n, plaintext, handle) ->
      Alcotest.(check bool)
        "handle is not plaintext pem" false
        (contains handle "BEGIN RSA");
      ignore plaintext)
    !stored;
  match Github_app_setup_tx.get ~db ~id:tx.id with
  | Ok (Some stored_tx) ->
      Alcotest.(check string)
        "persisted consumed" "consumed"
        (Github_app_setup_tx.status_to_string stored_tx.status)
  | Ok None -> Alcotest.fail "tx missing"
  | Error e -> Alcotest.fail e

let test_unknown_state () =
  with_db @@ fun db ->
  let store_secret, _ = make_store () in
  match
    Github_app_setup_callback.exchange ~db ~http_post:(ok_http ()) ~store_secret
      ~now:fixed_now
      (make_req ~code:"c" ~state:"no_such_state" ())
  with
  | Error msg ->
      Alcotest.(check bool)
        "unknown" true
        (contains (String.lowercase_ascii msg) "unknown")
  | Ok _ -> Alcotest.fail "expected unknown state failure"

let test_expired () =
  with_db @@ fun db ->
  let ttl = 60.0 in
  let tx =
    assert_ok
      (create_tx ~db ~id:"tx_exp" ~state:"state_expired_bbbb" ~ttl_seconds:ttl
         ())
  in
  let later = fixed_now +. ttl +. 5.0 in
  let store_secret, stored = make_store () in
  (match
     Github_app_setup_callback.exchange ~db ~http_post:(ok_http ())
       ~store_secret ~now:later
       (make_req ~code:"c" ~state:tx.state ())
   with
  | Error msg ->
      Alcotest.(check bool)
        "expired msg" true
        (contains (String.lowercase_ascii msg) "expired")
  | Ok _ -> Alcotest.fail "expected expired failure");
  Alcotest.(check int) "no secrets on expiry" 0 (List.length !stored);
  match Github_app_setup_tx.get ~db ~id:tx.id with
  | Ok (Some stored_tx) ->
      Alcotest.(check string)
        "marked expired" "expired"
        (Github_app_setup_tx.status_to_string stored_tx.status)
  | Ok None -> Alcotest.fail "missing"
  | Error e -> Alcotest.fail e

let test_replay_after_consume () =
  with_db @@ fun db ->
  let tx =
    assert_ok (create_tx ~db ~id:"tx_replay" ~state:"state_replay_cccc" ())
  in
  let store_secret, stored = make_store () in
  let _ =
    assert_ok
      (Github_app_setup_callback.exchange ~db ~http_post:(ok_http ())
         ~verify_installation ~store_secret ~now:fixed_now
         (make_req ~code:"c1" ~state:tx.state ()))
  in
  let before = List.length !stored in
  (match
     Github_app_setup_callback.exchange ~db ~http_post:(ok_http ())
       ~store_secret ~now:fixed_now
       (make_req ~code:"c2" ~state:tx.state ())
   with
  | Error msg ->
      let lower = String.lowercase_ascii msg in
      Alcotest.(check bool)
        "reuse refused" true
        (contains lower "not open" || contains lower "refus"
       || contains lower "consumed" || contains lower "reuse")
  | Ok _ -> Alcotest.fail "replay must fail");
  Alcotest.(check int) "no extra secrets on replay" before (List.length !stored)

let test_bind_mismatch () =
  with_db @@ fun db ->
  let tx =
    assert_ok (create_tx ~db ~id:"tx_bind" ~state:"state_bind_dddd" ())
  in
  let store_secret, stored = make_store () in
  (match
     Github_app_setup_callback.exchange ~db ~http_post:(ok_http ())
       ~store_secret ~now:fixed_now
       (make_req ~code:"c" ~state:tx.state ~expected_bind:other_room ())
   with
  | Error msg ->
      Alcotest.(check bool)
        "bind mismatch" true
        (contains (String.lowercase_ascii msg) "bind")
  | Ok _ -> Alcotest.fail "forged room must fail");
  Alcotest.(check int) "no secrets" 0 (List.length !stored);
  match Github_app_setup_tx.get ~db ~id:tx.id with
  | Ok (Some t) ->
      Alcotest.(check string)
        "still open" "open"
        (Github_app_setup_tx.status_to_string t.status)
  | _ -> Alcotest.fail "tx"

let test_principal_mismatch () =
  with_db @@ fun db ->
  let tx =
    assert_ok (create_tx ~db ~id:"tx_prin" ~state:"state_prin_eeee" ())
  in
  let store_secret, stored = make_store () in
  (match
     Github_app_setup_callback.exchange ~db ~http_post:(ok_http ())
       ~store_secret ~now:fixed_now
       (make_req ~code:"c" ~state:tx.state
          ~expected_principal_id:"principal:bob" ())
   with
  | Error msg ->
      Alcotest.(check bool)
        "principal mismatch" true
        (contains (String.lowercase_ascii msg) "principal")
  | Ok _ -> Alcotest.fail "principal mismatch must fail");
  Alcotest.(check int) "no secrets" 0 (List.length !stored)

let test_partial_missing_pem () =
  with_db @@ fun db ->
  let tx =
    assert_ok (create_tx ~db ~id:"tx_partial" ~state:"state_partial_ffff" ())
  in
  let body =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("id", `Int 1);
           ("client_id", `String "Iv1.x");
           ("client_secret", `String "cs");
           ("webhook_secret", `String "wh");
           (* pem intentionally omitted *)
         ])
  in
  let store_secret, stored = make_store () in
  (match
     Github_app_setup_callback.exchange ~db ~http_post:(ok_http ~body ())
       ~store_secret ~now:fixed_now
       (make_req ~code:"c" ~state:tx.state ())
   with
  | Error msg ->
      Alcotest.(check bool)
        "missing pem" true
        (contains (String.lowercase_ascii msg) "pem")
  | Ok _ -> Alcotest.fail "partial conversion must fail");
  Alcotest.(check int) "no secret handles" 0 (List.length !stored);
  (match Github_app_setup_tx.get ~db ~id:tx.id with
  | Ok (Some t) ->
      Alcotest.(check string)
        "remains open" "open"
        (Github_app_setup_tx.status_to_string t.status)
  | Ok None -> Alcotest.fail "tx missing"
  | Error e -> Alcotest.fail e);
  match Github_app_setup_callback.find_receipt_by_tx ~db ~tx_id:tx.id with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "must not write receipt on partial"
  | Error e -> Alcotest.fail e

let test_malformed_json () =
  with_db @@ fun db ->
  let tx =
    assert_ok (create_tx ~db ~id:"tx_badjson" ~state:"state_badjson_gggg" ())
  in
  let store_secret, stored = make_store () in
  (match
     Github_app_setup_callback.exchange ~db
       ~http_post:(ok_http ~body:"{not-json" ())
       ~store_secret ~now:fixed_now
       (make_req ~code:"c" ~state:tx.state ())
   with
  | Error msg ->
      Alcotest.(check bool)
        "malformed" true
        (contains (String.lowercase_ascii msg) "malformed"
        || contains (String.lowercase_ascii msg) "json")
  | Ok _ -> Alcotest.fail "malformed json must fail");
  Alcotest.(check int) "no secrets" 0 (List.length !stored);
  match Github_app_setup_tx.get ~db ~id:tx.id with
  | Ok (Some t) ->
      Alcotest.(check string)
        "open" "open"
        (Github_app_setup_tx.status_to_string t.status)
  | _ -> Alcotest.fail "tx"

let test_http_error () =
  with_db @@ fun db ->
  let tx =
    assert_ok (create_tx ~db ~id:"tx_http" ~state:"state_http_hhhh" ())
  in
  let store_secret, stored = make_store () in
  (match
     Github_app_setup_callback.exchange ~db
       ~http_post:(ok_http ~status:404 ~body:"{\"message\":\"Not Found\"}" ())
       ~store_secret ~now:fixed_now
       (make_req ~code:"c" ~state:tx.state ())
   with
  | Error msg ->
      Alcotest.(check bool)
        "http error" true
        (contains (String.lowercase_ascii msg) "http"
        || contains (String.lowercase_ascii msg) "status")
  | Ok _ -> Alcotest.fail "http error must fail");
  Alcotest.(check int) "no secrets" 0 (List.length !stored);
  match Github_app_setup_tx.get ~db ~id:tx.id with
  | Ok (Some t) ->
      Alcotest.(check string)
        "open" "open"
        (Github_app_setup_tx.status_to_string t.status)
  | _ -> Alcotest.fail "tx"

let test_concurrent_duplicate_only_one_succeeds () =
  with_db @@ fun db ->
  let tx = assert_ok (create_tx ~db ~id:"tx_dup" ~state:"state_dup_iiii" ()) in
  let store_secret, stored = make_store () in
  let first =
    assert_ok
      (Github_app_setup_callback.exchange ~db ~http_post:(ok_http ())
         ~verify_installation ~store_secret ~now:fixed_now
         (make_req ~code:"c1" ~state:tx.state ()))
  in
  Alcotest.(check int) "app from first" 424242 first.app.app_id;
  (match
     Github_app_setup_callback.exchange ~db ~http_post:(ok_http ())
       ~store_secret ~now:fixed_now
       (make_req ~code:"c2" ~state:tx.state ())
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "second concurrent exchange must fail");
  (* First win is durable; only one receipt. *)
  match Github_app_setup_callback.find_receipt_by_tx ~db ~tx_id:tx.id with
  | Ok (Some (rid, app)) ->
      Alcotest.(check string) "receipt id" first.receipt_id rid;
      Alcotest.(check int) "app" 424242 app.app_id;
      Alcotest.(check bool)
        "at least four secrets from winner" true
        (List.length !stored >= 4)
  | Ok None -> Alcotest.fail "receipt missing"
  | Error e -> Alcotest.fail e

let test_callback_path_mismatch () =
  with_db @@ fun db ->
  let tx =
    assert_ok (create_tx ~db ~id:"tx_path" ~state:"state_path_jjjj" ())
  in
  let store_secret, stored = make_store () in
  (match
     Github_app_setup_callback.exchange ~db ~http_post:(ok_http ())
       ~store_secret ~now:fixed_now
       (make_req ~code:"c" ~state:tx.state ~callback_path:"/wrong/callback/path"
          ())
   with
  | Error msg ->
      Alcotest.(check bool)
        "path mismatch" true
        (contains (String.lowercase_ascii msg) "callback")
  | Ok _ -> Alcotest.fail "wrong path must fail");
  Alcotest.(check int) "no secrets" 0 (List.length !stored)

let test_missing_callback_context_fails_closed () =
  with_db @@ fun db ->
  let tx =
    assert_ok (create_tx ~db ~id:"tx_context" ~state:"state_context_llll" ())
  in
  let store_secret, stored = make_store () in
  let req : Github_app_setup_callback.exchange_request =
    {
      code = "c";
      state = tx.state;
      callback_path = Some Github_app_setup_tx.default_callback_path;
      expected_bind = None;
      expected_principal_id = Some principal.id;
      installation_id = Some 99;
      setup_action = None;
    }
  in
  (match
     Github_app_setup_callback.exchange ~db ~http_post:(ok_http ())
       ~verify_installation ~store_secret ~now:fixed_now req
   with
  | Error msg ->
      Alcotest.(check bool)
        "mentions context" true
        (contains (String.lowercase_ascii msg) "context")
  | Ok _ -> Alcotest.fail "missing trusted callback context must fail");
  Alcotest.(check int) "no secrets" 0 (List.length !stored)

let test_verified_installation_must_match_converted_app () =
  with_db @@ fun db ->
  let tx =
    assert_ok (create_tx ~db ~id:"tx_verify" ~state:"state_verify_mmmm" ())
  in
  let store_secret, stored = make_store () in
  let verify_wrong_app ~app_id:_ ~private_key_pem:_ ~installation_id =
    verify_installation ~app_id:7 ~private_key_pem:"unused" ~installation_id
  in
  (match
     Github_app_setup_callback.exchange ~db ~http_post:(ok_http ())
       ~verify_installation:verify_wrong_app ~store_secret ~now:fixed_now
       (make_req ~code:"c" ~state:tx.state ())
   with
  | Error msg ->
      Alcotest.(check bool)
        "app mismatch" true
        (contains (String.lowercase_ascii msg) "app_id")
  | Ok _ -> Alcotest.fail "mismatched verified App must fail");
  Alcotest.(check int) "no secrets" 0 (List.length !stored);
  match Github_app_setup_tx.get ~db ~id:tx.id with
  | Ok (Some stored_tx) ->
      Alcotest.(check string)
        "transaction remains recoverable" "open"
        (Github_app_setup_tx.status_to_string stored_tx.status)
  | Ok None -> Alcotest.fail "transaction missing"
  | Error e -> Alcotest.fail e

let test_verified_installation_must_match_setup_org () =
  with_db @@ fun db ->
  let requested_scope : Github_app_setup_tx.requested_scope =
    {
      org = Some "acme-corp";
      selection = Github_app_setup_tx.All_repos;
      permissions = Github_app_setup_tx.default_permissions;
      events = Github_app_setup_tx.default_events;
    }
  in
  let tx =
    assert_ok
      (create_tx ~db ~id:"tx_org" ~state:"state_org_oooo" ~scope:requested_scope
         ())
  in
  let verify_other_org ~app_id ~private_key_pem ~installation_id =
    match verify_installation ~app_id ~private_key_pem ~installation_id with
    | Error _ as error -> error
    | Ok scope ->
        Ok
          {
            scope with
            account = { scope.account with login = "other-org"; id = 100 };
          }
  in
  let store_secret, stored = make_store () in
  (match
     Github_app_setup_callback.exchange ~db ~http_post:(ok_http ())
       ~verify_installation:verify_other_org ~store_secret ~now:fixed_now
       (make_req ~code:"c" ~state:tx.state ())
   with
  | Error msg ->
      Alcotest.(check bool)
        "org mismatch" true
        (contains (String.lowercase_ascii msg) "org")
  | Ok _ -> Alcotest.fail "verified installation for another org must fail");
  Alcotest.(check int) "no secrets" 0 (List.length !stored)

let test_reentrant_duplicate_fails_before_secret_store () =
  with_db @@ fun db ->
  let tx =
    assert_ok
      (create_tx ~db ~id:"tx_reentrant" ~state:"state_reentrant_pppp" ())
  in
  let store_secret, stored = make_store () in
  let req = make_req ~code:"c" ~state:tx.state () in
  let duplicate = ref None in
  let http ~url:_ ~headers:_ ~body:_ =
    duplicate :=
      Some
        (Github_app_setup_callback.exchange ~db ~http_post:(ok_http ())
           ~verify_installation ~store_secret ~now:fixed_now req);
    Ok (201, conversion_json ())
  in
  ignore
    (assert_ok
       (Github_app_setup_callback.exchange ~db ~http_post:http
          ~verify_installation ~store_secret ~now:fixed_now req));
  (match !duplicate with
  | Some (Error _) -> ()
  | Some (Ok _) -> Alcotest.fail "duplicate callback unexpectedly succeeded"
  | None -> Alcotest.fail "reentrant callback was not attempted");
  Alcotest.(check int)
    "only winner stored one complete secret set" 4 (List.length !stored)

let test_missing_installation_verifier_fails_closed () =
  with_db @@ fun db ->
  let tx =
    assert_ok
      (create_tx ~db ~id:"tx_missing_verifier" ~state:"state_verifier_nnnn" ())
  in
  let store_secret, stored = make_store () in
  (match
     Github_app_setup_callback.exchange ~db ~http_post:(ok_http ())
       ~store_secret ~now:fixed_now
       (make_req ~code:"c" ~state:tx.state ())
   with
  | Error msg ->
      Alcotest.(check bool)
        "requires authenticated verifier" true
        (contains (String.lowercase_ascii msg) "verifier")
  | Ok _ -> Alcotest.fail "callback without installation verifier must fail");
  Alcotest.(check int) "no secrets" 0 (List.length !stored)

let test_schema_idempotent () =
  with_db @@ fun db ->
  Github_app_setup_callback.ensure_schema db;
  Github_app_setup_callback.ensure_schema db;
  let url =
    Github_app_setup_callback.expected_callback_url ~public_base_url:public_base
  in
  Alcotest.(check string)
    "expected callback" "https://clawq.example.com/github/app/setup/callback"
    url;
  let conv = Github_app_setup_callback.conversion_url ~code:"abc/def" in
  Alcotest.(check bool)
    "conversion url host" true
    (contains conv "api.github.com/app-manifests/")

let test_transport_error_leaves_open () =
  with_db @@ fun db ->
  let tx = assert_ok (create_tx ~db ~id:"tx_tr" ~state:"state_tr_kkkk" ()) in
  let store_secret, stored = make_store () in
  let http ~url:_ ~headers:_ ~body:_ = Error "connection refused" in
  (match
     Github_app_setup_callback.exchange ~db ~http_post:http ~store_secret
       ~now:fixed_now
       (make_req ~code:"c" ~state:tx.state ())
   with
  | Error msg ->
      Alcotest.(check bool)
        "transport" true
        (contains (String.lowercase_ascii msg) "transport"
        || contains (String.lowercase_ascii msg) "connection")
  | Ok _ -> Alcotest.fail "transport error must fail");
  Alcotest.(check int) "no secrets" 0 (List.length !stored)

let suite =
  [
    ("happy path consume + handles", `Quick, test_happy_path);
    ("unknown state fails", `Quick, test_unknown_state);
    ("expired fails", `Quick, test_expired);
    ("replay after consume fails", `Quick, test_replay_after_consume);
    ("bind/room mismatch fails", `Quick, test_bind_mismatch);
    ("principal mismatch fails", `Quick, test_principal_mismatch);
    ("partial missing pem fails open", `Quick, test_partial_missing_pem);
    ("malformed GitHub JSON fails", `Quick, test_malformed_json);
    ("conversion HTTP error fails open", `Quick, test_http_error);
    ( "duplicate concurrent only one succeeds",
      `Quick,
      test_concurrent_duplicate_only_one_succeeds );
    ("callback path mismatch fails", `Quick, test_callback_path_mismatch);
    ( "missing callback context fails closed",
      `Quick,
      test_missing_callback_context_fails_closed );
    ( "verified installation must match converted App",
      `Quick,
      test_verified_installation_must_match_converted_app );
    ( "verified installation must match setup org",
      `Quick,
      test_verified_installation_must_match_setup_org );
    ( "reentrant duplicate fails before secret store",
      `Quick,
      test_reentrant_duplicate_fails_before_secret_store );
    ( "missing installation verifier fails closed",
      `Quick,
      test_missing_installation_verifier_fails_closed );
    ("schema/helpers", `Quick, test_schema_idempotent);
    ("transport error leaves open", `Quick, test_transport_error_leaves_open);
  ]

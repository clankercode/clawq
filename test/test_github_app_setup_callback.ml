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

let create_tx ?id ?state ?(bind = room_bind) ?(ttl_seconds = 1800.0)
    ?(now = fixed_now) ~db () =
  Github_app_setup_tx.create ~db ~principal ~bind ~base_revision
    ~public_base_url:public_base ~app_name:"Clawq" ~now ~ttl_seconds ?id ?state
    ()

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

let make_req ?callback_path ?expected_bind ?expected_principal_id
    ?installation_id ~code ~state () :
    Github_app_setup_callback.exchange_request =
  {
    code;
    state;
    callback_path;
    expected_bind;
    expected_principal_id;
    installation_id;
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
         ~store_secret ~now:fixed_now
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
         ~store_secret ~now:fixed_now
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
         ~store_secret ~now:fixed_now
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
    ("schema/helpers", `Quick, test_schema_idempotent);
    ("transport error leaves open", `Quick, test_transport_error_leaves_open);
  ]

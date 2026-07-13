(** Tests for one-shot PKCE callback verify + code exchange (P21.M2.E2.T002). *)

module Cb = Github_user_auth_pkce_callback
module Pkce = Github_user_auth_pkce
module Tx = Github_user_auth_tx
module V = Github_user_token_vault
module B = Github_account_binding
module S = Github_user_token_store
module P = Principal_identity
module PS = Principal_identity_store

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-pkce-callback-master" ()

let fixed_now = 1_700_000_000.0
let principal_id = "principal:alice"
let base_revision = "rev-policy-1"
let continuation = "cont:dm:handle-1"
let registered = "https://clawq.example/oauth/github/callback"
let client_id = "Iv1.testclientid0001"
let client_secret = "cs_test_secret_never_log"
let access_token = "ghu_access_CALLBACK_PLAINTEXT_x"
let refresh_token = "ghr_refresh_CALLBACK_PLAINTEXT_y"
let github_user_id = 424242L
let github_login = "octocat-alice"

let actor =
  match
    P.make_connector_actor_key ~connector:P.Teams
      ~tenant_or_workspace:"tenant-acme" ~immutable_user_id:"user-alice-1"
  with
  | Ok k -> k
  | Error e -> failwith e

let room = Tx.Room "room-teams-1"

let app : Tx.app_client =
  { host = "github.com"; app_id = 42; client_id_handle = "h:client-id" }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let assert_exchange = function
  | Ok v -> v
  | Error (e : Cb.exchange_error) ->
      Alcotest.fail
        (Printf.sprintf "%s [%s]" e.message (Cb.string_of_failure_kind e.kind))

let contains hay needle = Test_helpers.string_contains hay needle

let make_keys () =
  assert_ok
    (V.make_single_key_provider ~key_id:"mk-pkce-cb-1" ~key_version:1 ~aes_key
       ())

let fresh_store () = S.make_in_memory_secret_store ()

let seed_principal ~db () =
  let pid = assert_ok (P.principal_id_of_string principal_id) in
  let p =
    P.make_principal ~id:pid ~revision:1 ~created_at:"2026-01-01T00:00:00Z"
      ~updated_at:"2026-01-01T00:00:00Z" ()
  in
  assert_ok (PS.insert_principal ~db ~now:fixed_now p)

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Tx.ensure_schema db;
  Pkce.ensure_schema db;
  V.ensure_schema db;
  B.ensure_schema db;
  ignore (seed_principal ~db ());
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let start_flow ?one_time_state ?id ?now ~db ~store () =
  Pkce.start ~db ~store ~principal_id ~connector_actor:actor ~source:room ~app
    ~client_id ~registered_redirect_uri:registered ~base_revision
    ~continuation_handle:continuation
    ~now:(Option.value now ~default:fixed_now)
    ?id ?one_time_state ()

let resolve_client ~client_id_handle:_ = Ok (client_id, client_secret)

let fetch_user ~access_token:tok =
  if tok <> access_token then Error "unexpected access token in fetch_user"
  else
    Ok
      {
        Cb.id = github_user_id;
        login = github_login;
        avatar_url = Some "https://avatars.example/o.png";
      }

let token_json ?(access = access_token) ?(refresh = Some refresh_token)
    ?(expires_in = 28800) ?(scope = "repo read:user") () =
  let fields =
    [
      ("access_token", `String access);
      ("expires_in", `Int expires_in);
      ("token_type", `String "bearer");
      ("scope", `String scope);
    ]
  in
  let fields =
    match refresh with
    | None -> fields
    | Some r -> fields @ [ ("refresh_token", `String r) ]
  in
  Yojson.Safe.to_string (`Assoc fields)

let ok_http ?(body = token_json ()) ?(status = 200) () ~url:_ ~headers:_ ~body:_
    =
  Ok (status, body)

let count_bindings ~db =
  match
    B.list_for_principal ~db
      ~principal_id:(assert_ok (P.principal_id_of_string principal_id))
  with
  | Ok xs -> List.length xs
  | Error e -> Alcotest.fail e

let count_authorized ~db =
  match
    B.list_for_principal ~db
      ~principal_id:(assert_ok (P.principal_id_of_string principal_id))
  with
  | Error e -> Alcotest.fail e
  | Ok xs ->
      List.length
        (List.filter
           (fun b ->
             match b.B.authorization_status with
             | B.Authorized -> true
             | _ -> false)
           xs)

let assert_no_active_binding ~db =
  Alcotest.(check int) "no authorized binding" 0 (count_authorized ~db)

let exchange ~db ~store ~keys ?http_post ?fetch_user:fu ?(now = fixed_now)
    ?binding_id ?vault_id ~callback () =
  Cb.exchange ~db ~store ~keys ?http_post ~resolve_client
    ~fetch_user:(Option.value fu ~default:fetch_user)
    ~now ?binding_id ?vault_id ~callback ()

(* -------------------------------------------------------------------------- *)
(* Parse helpers                                                              *)
(* -------------------------------------------------------------------------- *)

let test_parse_token_json_and_form () =
  let j = assert_ok (Cb.parse_token_response ~body:(token_json ())) in
  Alcotest.(check string) "access" access_token j.access_token;
  Alcotest.(check (option string))
    "refresh" (Some refresh_token) j.refresh_token;
  Alcotest.(check int) "expires" 28800 j.expires_in;
  Alcotest.(check (list string)) "scopes" [ "repo"; "read:user" ] j.scopes;
  let form =
    "access_token=ghu_form&expires_in=100&refresh_token=ghr_form&scope=repo"
  in
  let f = assert_ok (Cb.parse_token_response ~body:form) in
  Alcotest.(check string) "form access" "ghu_form" f.access_token;
  Alcotest.(check int) "form exp" 100 f.expires_in;
  (match
     Cb.parse_token_response ~body:{|{"error":"bad_verification_code"}|}
   with
  | Error msg ->
      Alcotest.(check bool)
        "oauth error" true
        (contains (String.lowercase_ascii msg) "bad_verification")
  | Ok _ -> Alcotest.fail "oauth error object must fail");
  match Cb.parse_token_response ~body:{|{"access_token":"x"}|} with
  | Error msg ->
      Alcotest.(check bool)
        "missing expires" true
        (contains (String.lowercase_ascii msg) "expires_in")
  | Ok _ -> Alcotest.fail "missing expires_in must fail"

(* -------------------------------------------------------------------------- *)
(* Happy path                                                                 *)
(* -------------------------------------------------------------------------- *)

let test_happy_path_exchange_seals_pending_binding () =
  with_db @@ fun db ->
  let store, _ = fresh_store () in
  let keys = make_keys () in
  let started = assert_ok (start_flow ~db ~store ~id:"tx_happy" ()) in
  let callback =
    assert_ok
      (Cb.make_callback_request ~code:"code_once_abc"
         ~state:started.tx.one_time_state ~redirect_uri:registered ())
  in
  let exchange_count = ref 0 in
  let http ~url ~headers:_ ~body =
    incr exchange_count;
    Alcotest.(check bool)
      "token endpoint" true
      (contains url "login/oauth/access_token");
    Alcotest.(check bool) "sends code" true (contains body "code=code_once_abc");
    Alcotest.(check bool) "sends redirect" true (contains body "redirect_uri=");
    Alcotest.(check bool) "sends verifier" true (contains body "code_verifier=");
    Alcotest.(check bool)
      "sends client_id" true
      (contains body ("client_id=" ^ client_id));
    Ok (200, token_json ())
  in
  match
    exchange ~db ~store ~keys ~http_post:http ~callback
      ~binding_id:"ghbind_happy" ~vault_id:"ghvault_happy" ()
  with
  | Error e ->
      Alcotest.fail (e.message ^ " [" ^ Cb.string_of_failure_kind e.kind ^ "]")
  | Ok r ->
      Alcotest.(check int) "one exchange" 1 !exchange_count;
      Alcotest.(check string)
        "tx completed" "completed"
        (Tx.string_of_status r.tx.status);
      Alcotest.(check string)
        "binding pending" "pending"
        (B.string_of_authorization_status r.binding.authorization_status);
      Alcotest.(check bool)
        "not active" false
        (Cb.has_active_binding ~binding:r.binding);
      Alcotest.(check string) "vault id" "ghvault_happy" r.vault.id;
      Alcotest.(check string) "binding id" "ghbind_happy" r.binding.id;
      Alcotest.(check (option string))
        "vault ref" (Some "ghvault_happy") r.binding.vault_ref;
      Alcotest.(check bool)
        "user id" true
        (Int64.equal r.github_user.id github_user_id);
      (* Tokens sealed: readable via vault, not plaintext on binding. *)
      (match V.read ~db ~keys ~id:r.vault.id () with
      | Error d -> Alcotest.fail (V.string_of_denial d)
      | Ok opened ->
          Alcotest.(check string)
            "access sealed" access_token opened.tokens.access_token;
          Alcotest.(check (option string))
            "refresh sealed" (Some refresh_token) opened.tokens.refresh_token);
      (match
         V.row_contains_plaintext ~db ~id:r.vault.id ~plaintext:access_token
       with
      | Ok false -> ()
      | Ok true -> Alcotest.fail "access plaintext in vault row"
      | Error d -> Alcotest.fail (V.string_of_denial d));
      let summary = Cb.redacted_summary r in
      Alcotest.(check bool)
        "summary no access" false
        (contains summary access_token);
      Alcotest.(check bool)
        "summary no secret" false
        (contains summary client_secret);
      assert_no_active_binding ~db;
      (* Pending exists but is not Authorized. *)
      Alcotest.(check int) "one pending binding" 1 (count_bindings ~db)

(* -------------------------------------------------------------------------- *)
(* Failure cases: no active binding                                           *)
(* -------------------------------------------------------------------------- *)

let test_state_mismatch () =
  with_db @@ fun db ->
  let store, _ = fresh_store () in
  let keys = make_keys () in
  ignore (assert_ok (start_flow ~db ~store ~id:"tx_mm" ()));
  let callback =
    assert_ok
      (Cb.make_callback_request ~code:"code_x"
         ~state:"totally_unknown_state_aaaaaaaaaaaaaaaa"
         ~redirect_uri:registered ())
  in
  match exchange ~db ~store ~keys ~http_post:(ok_http ()) ~callback () with
  | Ok _ -> Alcotest.fail "unknown state must fail"
  | Error e ->
      Alcotest.(check string)
        "kind" "state_mismatch"
        (Cb.string_of_failure_kind e.kind);
      assert_no_active_binding ~db;
      Alcotest.(check int) "no bindings" 0 (count_bindings ~db)

let test_replay_after_success () =
  with_db @@ fun db ->
  let store, _ = fresh_store () in
  let keys = make_keys () in
  let started = assert_ok (start_flow ~db ~store ~id:"tx_replay" ()) in
  let callback =
    assert_ok
      (Cb.make_callback_request ~code:"code_1" ~state:started.tx.one_time_state
         ~redirect_uri:registered ())
  in
  let first =
    assert_exchange
      (exchange ~db ~store ~keys ~http_post:(ok_http ()) ~callback
         ~binding_id:"b1" ~vault_id:"v1" ())
  in
  Alcotest.(check string)
    "first completed" "completed"
    (Tx.string_of_status first.tx.status);
  (* Replay same callback. *)
  match
    exchange ~db ~store ~keys ~http_post:(ok_http ()) ~callback ~binding_id:"b2"
      ~vault_id:"v2" ()
  with
  | Ok _ -> Alcotest.fail "replay must fail"
  | Error e ->
      Alcotest.(check bool)
        "replay kind" true
        (match e.kind with
        | Cb.Replay | Cb.Unused_status | Cb.Duplicate_callback -> true
        | _ -> false);
      Alcotest.(check int) "still one binding" 1 (count_bindings ~db);
      assert_no_active_binding ~db

let test_duplicate_callback_one_exchange () =
  with_db @@ fun db ->
  let store, _ = fresh_store () in
  let keys = make_keys () in
  let started = assert_ok (start_flow ~db ~store ~id:"tx_dup" ()) in
  let callback =
    assert_ok
      (Cb.make_callback_request ~code:"code_dup"
         ~state:started.tx.one_time_state ~redirect_uri:registered ())
  in
  let exchange_count = ref 0 in
  let http ~url:_ ~headers:_ ~body:_ =
    incr exchange_count;
    Ok (200, token_json ())
  in
  let first =
    assert_exchange
      (exchange ~db ~store ~keys ~http_post:http ~callback ~binding_id:"bd1"
         ~vault_id:"vd1" ())
  in
  ignore first;
  let second =
    exchange ~db ~store ~keys ~http_post:http ~callback ~binding_id:"bd2"
      ~vault_id:"vd2" ()
  in
  (match second with
  | Ok _ -> Alcotest.fail "duplicate must fail"
  | Error e ->
      Alcotest.(check bool)
        "dup refused" true
        (match e.kind with
        | Cb.Replay | Cb.Duplicate_callback | Cb.Unused_status -> true
        | _ -> false));
  Alcotest.(check int) "exactly one remote exchange" 1 !exchange_count;
  Alcotest.(check int) "exactly one binding" 1 (count_bindings ~db);
  assert_no_active_binding ~db

let test_oauth_denial_cancels_no_binding () =
  with_db @@ fun db ->
  let store, _ = fresh_store () in
  let keys = make_keys () in
  let started = assert_ok (start_flow ~db ~store ~id:"tx_deny" ()) in
  let http_called = ref false in
  let http ~url:_ ~headers:_ ~body:_ =
    http_called := true;
    Ok (200, token_json ())
  in
  let callback =
    assert_ok
      (Cb.make_callback_request ~state:started.tx.one_time_state
         ~redirect_uri:registered ~error:"access_denied"
         ~error_description:"User cancelled" ())
  in
  match exchange ~db ~store ~keys ~http_post:http ~callback () with
  | Ok _ -> Alcotest.fail "denial must fail"
  | Error e -> (
      Alcotest.(check string) "kind" "denial" (Cb.string_of_failure_kind e.kind);
      Alcotest.(check bool) "no http" false !http_called;
      assert_no_active_binding ~db;
      Alcotest.(check int) "no bindings" 0 (count_bindings ~db);
      (match e.tx with
      | None -> Alcotest.fail "expected tx"
      | Some tx ->
          Alcotest.(check string)
            "cancelled" "cancelled"
            (Tx.string_of_status tx.status));
      (* Second denial / callback is replay terminal. *)
      match exchange ~db ~store ~keys ~http_post:http ~callback () with
      | Ok _ -> Alcotest.fail "second denial must fail"
      | Error e2 ->
          Alcotest.(check bool)
            "terminal" true
            (match e2.kind with
            | Cb.Replay | Cb.Unused_status | Cb.Denial -> true
            | _ -> false))

let test_timeout_no_binding () =
  with_db @@ fun db ->
  let store, _ = fresh_store () in
  let keys = make_keys () in
  let started = assert_ok (start_flow ~db ~store ~id:"tx_to" ()) in
  let callback =
    assert_ok
      (Cb.make_callback_request ~code:"code_to" ~state:started.tx.one_time_state
         ~redirect_uri:registered ())
  in
  let http ~url:_ ~headers:_ ~body:_ = Error "connection timed out after 30s" in
  match exchange ~db ~store ~keys ~http_post:http ~callback () with
  | Ok _ -> Alcotest.fail "timeout must fail"
  | Error e -> (
      Alcotest.(check string)
        "kind" "timeout"
        (Cb.string_of_failure_kind e.kind);
      assert_no_active_binding ~db;
      Alcotest.(check int) "no bindings" 0 (count_bindings ~db);
      (match e.tx with
      | Some tx ->
          Alcotest.(check string)
            "claimed terminal" "completed"
            (Tx.string_of_status tx.status)
      | None -> Alcotest.fail "expected claimed tx");
      (* Replay after timeout still refused — one-shot. *)
      match exchange ~db ~store ~keys ~http_post:(ok_http ()) ~callback () with
      | Ok _ -> Alcotest.fail "must not recover after timeout claim"
      | Error e2 ->
          Alcotest.(check bool)
            "still terminal" true
            (match e2.kind with
            | Cb.Replay | Cb.Unused_status | Cb.Duplicate_callback -> true
            | _ -> false);
          Alcotest.(check int) "still no bindings" 0 (count_bindings ~db))

let test_malformed_response_no_binding () =
  with_db @@ fun db ->
  let store, _ = fresh_store () in
  let keys = make_keys () in
  let started = assert_ok (start_flow ~db ~store ~id:"tx_bad" ()) in
  let callback =
    assert_ok
      (Cb.make_callback_request ~code:"code_bad"
         ~state:started.tx.one_time_state ~redirect_uri:registered ())
  in
  let http ~url:_ ~headers:_ ~body:_ = Ok (200, "not-json-and-not-form%%%") in
  match exchange ~db ~store ~keys ~http_post:http ~callback () with
  | Ok _ -> Alcotest.fail "malformed must fail"
  | Error e ->
      Alcotest.(check string)
        "kind" "malformed_response"
        (Cb.string_of_failure_kind e.kind);
      assert_no_active_binding ~db;
      Alcotest.(check int) "no bindings" 0 (count_bindings ~db)

let test_http_denial_no_binding () =
  with_db @@ fun db ->
  let store, _ = fresh_store () in
  let keys = make_keys () in
  let started = assert_ok (start_flow ~db ~store ~id:"tx_httpd" ()) in
  let callback =
    assert_ok
      (Cb.make_callback_request ~code:"code_httpd"
         ~state:started.tx.one_time_state ~redirect_uri:registered ())
  in
  let http =
    ok_http ~status:401 ~body:{|{"error":"incorrect_client_credentials"}|} ()
  in
  match exchange ~db ~store ~keys ~http_post:http ~callback () with
  | Ok _ -> Alcotest.fail "http denial must fail"
  | Error e ->
      Alcotest.(check bool)
        "http denial kind" true
        (match e.kind with Cb.Http_denial 401 -> true | _ -> false);
      assert_no_active_binding ~db;
      Alcotest.(check int) "no bindings" 0 (count_bindings ~db)

let test_partial_exchange_vault_ok_binding_collision () =
  with_db @@ fun db ->
  let store, _ = fresh_store () in
  let keys = make_keys () in
  let started = assert_ok (start_flow ~db ~store ~id:"tx_partial" ()) in
  (* Pre-seed a binding on the same identity so insert collides after vault
     seal — partial path must destroy vault and leave no active binding. *)
  let identity =
    assert_ok (B.make_account_identity ~app_id:42 ~github_user_id ())
  in
  let pre =
    B.make_binding ~id:"pre_existing"
      ~principal_id:(assert_ok (P.principal_id_of_string principal_id))
      ~identity ~authorization_status:B.Pending ()
  in
  ignore (assert_ok (B.insert ~db ~now:fixed_now pre));
  let callback =
    assert_ok
      (Cb.make_callback_request ~code:"code_partial"
         ~state:started.tx.one_time_state ~redirect_uri:registered ())
  in
  match
    exchange ~db ~store ~keys ~http_post:(ok_http ()) ~callback
      ~vault_id:"vault_should_be_destroyed" ()
  with
  | Ok _ -> Alcotest.fail "partial must fail"
  | Error e -> (
      Alcotest.(check string)
        "kind" "partial_exchange"
        (Cb.string_of_failure_kind e.kind);
      assert_no_active_binding ~db;
      (* Only the pre-existing pending binding remains (not Authorized). *)
      Alcotest.(check int) "only pre-existing" 1 (count_bindings ~db);
      (match V.get_meta ~db ~id:"vault_should_be_destroyed" with
      | Ok None -> ()
      | Ok (Some _) -> Alcotest.fail "partial vault must be destroyed"
      | Error d -> Alcotest.fail (V.string_of_denial d));
      match e.tx with
      | Some tx ->
          Alcotest.(check string)
            "tx still terminal" "completed"
            (Tx.string_of_status tx.status)
      | None -> Alcotest.fail "expected tx")

let test_partial_fetch_user_fails () =
  with_db @@ fun db ->
  let store, _ = fresh_store () in
  let keys = make_keys () in
  let started = assert_ok (start_flow ~db ~store ~id:"tx_fuser" ()) in
  let callback =
    assert_ok
      (Cb.make_callback_request ~code:"code_fu" ~state:started.tx.one_time_state
         ~redirect_uri:registered ())
  in
  let fetch_user ~access_token:_ = Error "simulated /user 500" in
  match
    exchange ~db ~store ~keys ~http_post:(ok_http ()) ~fetch_user ~callback ()
  with
  | Ok _ -> Alcotest.fail "fetch_user failure must fail exchange"
  | Error e ->
      Alcotest.(check string)
        "kind" "partial_exchange"
        (Cb.string_of_failure_kind e.kind);
      assert_no_active_binding ~db;
      Alcotest.(check int) "no bindings" 0 (count_bindings ~db)

let test_redirect_mismatch_no_exchange () =
  with_db @@ fun db ->
  let store, _ = fresh_store () in
  let keys = make_keys () in
  let started = assert_ok (start_flow ~db ~store ~id:"tx_redir" ()) in
  let http_called = ref false in
  let http ~url:_ ~headers:_ ~body:_ =
    http_called := true;
    Ok (200, token_json ())
  in
  let callback =
    assert_ok
      (Cb.make_callback_request ~code:"code_r" ~state:started.tx.one_time_state
         ~redirect_uri:"https://evil.example/callback" ())
  in
  match exchange ~db ~store ~keys ~http_post:http ~callback () with
  | Ok _ -> Alcotest.fail "redirect mismatch must fail"
  | Error e -> (
      Alcotest.(check string)
        "kind" "redirect_mismatch"
        (Cb.string_of_failure_kind e.kind);
      Alcotest.(check bool) "no http" false !http_called;
      assert_no_active_binding ~db;
      Alcotest.(check int) "no bindings" 0 (count_bindings ~db);
      (* Tx remains open (retry with correct redirect possible). *)
      match Tx.get ~db ~id:started.tx.id with
      | Ok (Some tx) ->
          Alcotest.(check string)
            "still open" "open"
            (Tx.string_of_status tx.status)
      | _ -> Alcotest.fail "tx missing")

let test_expired_transaction () =
  with_db @@ fun db ->
  let store, _ = fresh_store () in
  let keys = make_keys () in
  let started =
    assert_ok
      (Pkce.start ~db ~store ~principal_id ~connector_actor:actor ~source:room
         ~app ~client_id ~registered_redirect_uri:registered ~base_revision
         ~continuation_handle:continuation ~ttl_seconds:60. ~now:fixed_now
         ~id:"tx_exp" ())
  in
  let callback =
    assert_ok
      (Cb.make_callback_request ~code:"code_exp"
         ~state:started.tx.one_time_state ~redirect_uri:registered ())
  in
  let later = fixed_now +. 120. in
  match
    exchange ~db ~store ~keys ~http_post:(ok_http ()) ~now:later ~callback ()
  with
  | Ok _ -> Alcotest.fail "expired must fail"
  | Error e -> (
      Alcotest.(check string)
        "kind" "expired"
        (Cb.string_of_failure_kind e.kind);
      assert_no_active_binding ~db;
      Alcotest.(check int) "no bindings" 0 (count_bindings ~db);
      match e.tx with
      | Some tx ->
          Alcotest.(check string)
            "expired status" "expired"
            (Tx.string_of_status tx.status)
      | None -> Alcotest.fail "expected expired tx")

let test_verifier_integrity_failure () =
  with_db @@ fun db ->
  let store, table = fresh_store () in
  let keys = make_keys () in
  let started = assert_ok (start_flow ~db ~store ~id:"tx_ver" ()) in
  (* Corrupt sealed verifier so S256 recompute fails. *)
  Hashtbl.replace table started.material.code_verifier_handle
    "corrupted_verifier_not_matching_challenge_at_all_xx";
  let callback =
    assert_ok
      (Cb.make_callback_request ~code:"code_ver"
         ~state:started.tx.one_time_state ~redirect_uri:registered ())
  in
  let http_called = ref false in
  let http ~url:_ ~headers:_ ~body:_ =
    http_called := true;
    Ok (200, token_json ())
  in
  match exchange ~db ~store ~keys ~http_post:http ~callback () with
  | Ok _ -> Alcotest.fail "bad verifier must fail"
  | Error e ->
      Alcotest.(check string)
        "kind" "verifier_invalid"
        (Cb.string_of_failure_kind e.kind);
      Alcotest.(check bool) "no http" false !http_called;
      assert_no_active_binding ~db;
      Alcotest.(check int) "no bindings" 0 (count_bindings ~db)

let suite =
  [
    ("parse token json and form", `Quick, test_parse_token_json_and_form);
    ( "happy path seals pending binding",
      `Quick,
      test_happy_path_exchange_seals_pending_binding );
    ("state mismatch no binding", `Quick, test_state_mismatch);
    ("replay after success", `Quick, test_replay_after_success);
    ( "duplicate callback one exchange",
      `Quick,
      test_duplicate_callback_one_exchange );
    ("oauth denial cancels", `Quick, test_oauth_denial_cancels_no_binding);
    ("timeout no binding", `Quick, test_timeout_no_binding);
    ("malformed response no binding", `Quick, test_malformed_response_no_binding);
    ("http denial no binding", `Quick, test_http_denial_no_binding);
    ( "partial exchange destroys vault",
      `Quick,
      test_partial_exchange_vault_ok_binding_collision );
    ("partial fetch_user fails", `Quick, test_partial_fetch_user_fails);
    ("redirect mismatch no exchange", `Quick, test_redirect_mismatch_no_exchange);
    ("expired transaction", `Quick, test_expired_transaction);
    ("verifier integrity failure", `Quick, test_verifier_integrity_failure);
  ]

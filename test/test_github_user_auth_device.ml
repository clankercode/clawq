(** Tests for GitHub App device authorization start with private code delivery
    (P21.M2.E3.T001). *)

module Dev = Github_user_auth_device
module Tx = Github_user_auth_tx
module D = Github_user_auth_delivery
module V = Github_user_token_vault
module P = Principal_identity

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-device-test-master" ()

let fixed_now = 1_720_000_000.0
let principal_id = "principal:alice"
let base_revision = "rev-policy-device-1"
let continuation = "cont:dm:handle-device-1"
let device_code_secret = "0123456789abcdef0123456789abcdef01234567"
let user_code_secret = "WDJB-MJHT"
let verification_uri = "https://github.com/login/device"

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

let assert_ok = function
  | Ok v -> v
  | Error (e : Dev.refuse_error) -> Alcotest.fail e.message

let assert_error = function
  | Error e -> e
  | Ok _ -> Alcotest.fail "expected Error"

let assert_parse_ok = function Ok v -> v | Error e -> Alcotest.fail e
let contains hay needle = Test_helpers.string_contains hay needle

let make_keys ?(key_id = "mk-device-1") ?(key_version = 1) () =
  assert_parse_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key ())

let dm_channel () =
  assert_parse_ok
    (D.make_private_connector_dm ~connector:P.Teams ~handle_id:"dm:alice")

let cli_channel () =
  assert_parse_ok (D.make_initiating_cli ~handle_id:"cli:alice")

let resolve_client_id ~handle =
  if String.trim handle = "h:client-id" then Ok "Iv1.test_client_id"
  else Error ("unknown client id handle: " ^ handle)

let form_device_body ?(device_code = device_code_secret)
    ?(user_code = user_code_secret) ?(verification_uri = verification_uri)
    ?(expires_in = 900) ?(interval = 5) ?verification_uri_complete () =
  let base =
    Printf.sprintf
      "device_code=%s&user_code=%s&verification_uri=%s&expires_in=%d&interval=%d"
      (Uri.pct_encode device_code)
      (Uri.pct_encode user_code)
      (Uri.pct_encode verification_uri)
      expires_in interval
  in
  match verification_uri_complete with
  | None -> base
  | Some u -> base ^ "&verification_uri_complete=" ^ Uri.pct_encode u

let json_device_body ?(device_code = device_code_secret)
    ?(user_code = user_code_secret) ?(verification_uri = verification_uri)
    ?(expires_in = 900) ?(interval = 5) () =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("device_code", `String device_code);
         ("user_code", `String user_code);
         ("verification_uri", `String verification_uri);
         ("expires_in", `Int expires_in);
         ("interval", `Int interval);
       ])

let http_ok body ~url:_ ~headers:_ ~body:_ = Ok (200, body)

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Dev.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let start_ok ?db ?(device_flow_enabled = true) ?(keys = make_keys ())
    ?(channel = dm_channel ()) ?(http_body = form_device_body ())
    ?(http_post = http_ok http_body) ?(id = "dev_sess_1")
    ?(tx_id = "tx_device_1") () =
  let run db =
    Dev.start ~db ~http_post ~resolve_client_id ~keys ~device_flow_enabled
      ~principal_id ~connector_actor:actor ~source:room ~app ~base_revision
      ~continuation_handle:continuation ~channel ~now:fixed_now ~id ~tx_id ()
  in
  match db with Some db -> run db | None -> with_db run

(* -------------------------------------------------------------------------- *)
(* Parse                                                                      *)
(* -------------------------------------------------------------------------- *)

let test_parse_form_and_json () =
  let form =
    assert_parse_ok (Dev.parse_device_code_response ~body:(form_device_body ()))
  in
  Alcotest.(check string) "form device_code" device_code_secret form.device_code;
  Alcotest.(check string) "form user_code" user_code_secret form.user_code;
  Alcotest.(check string) "form uri" verification_uri form.verification_uri;
  Alcotest.(check int) "form expires" 900 form.expires_in;
  Alcotest.(check int) "form interval" 5 form.interval;
  let json =
    assert_parse_ok (Dev.parse_device_code_response ~body:(json_device_body ()))
  in
  Alcotest.(check string) "json device_code" device_code_secret json.device_code;
  Alcotest.(check string) "json user_code" user_code_secret json.user_code

let test_parse_rejects_missing_fields () =
  match Dev.parse_device_code_response ~body:"expires_in=900&interval=5" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "missing fields must fail"

(* -------------------------------------------------------------------------- *)
(* Feature flag                                                               *)
(* -------------------------------------------------------------------------- *)

let test_refuse_when_device_flow_disabled () =
  with_db @@ fun db ->
  let http_called = ref false in
  let http_post ~url:_ ~headers:_ ~body:_ =
    http_called := true;
    Ok (200, form_device_body ())
  in
  let err =
    assert_error
      (Dev.start ~db ~http_post ~resolve_client_id ~keys:(make_keys ())
         ~device_flow_enabled:false ~principal_id ~connector_actor:actor
         ~source:room ~app ~base_revision ~continuation_handle:continuation
         ~channel:(dm_channel ()) ~now:fixed_now ())
  in
  Alcotest.(check string)
    "reason" "device_flow_disabled"
    (Dev.string_of_refuse_reason err.reason);
  Alcotest.(check bool) "no http" false !http_called;
  Alcotest.(check bool)
    "message mentions disabled" true
    (contains (String.lowercase_ascii err.message) "disabled");
  (* No durable device row. *)
  match Dev.get ~db ~id:"anything" with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "no session expected"
  | Error e -> Alcotest.fail e.message

let test_refuse_absent_private_channel () =
  with_db @@ fun db ->
  let err =
    assert_error
      (Dev.start ~db
         ~http_post:(http_ok (form_device_body ()))
         ~resolve_client_id ~keys:(make_keys ()) ~device_flow_enabled:true
         ~principal_id ~connector_actor:actor ~source:room ~app ~base_revision
         ~continuation_handle:continuation ~channel:D.Absent ~now:fixed_now ())
  in
  Alcotest.(check string)
    "no private" "no_private_channel"
    (Dev.string_of_refuse_reason err.reason)

(* -------------------------------------------------------------------------- *)
(* Happy path                                                                 *)
(* -------------------------------------------------------------------------- *)

let test_start_persists_encrypted_timing_and_private_delivery () =
  with_db @@ fun db ->
  let intended : Tx.intended_account =
    { github_user_id = Some 12345L; login_hint = Some "alice" }
  in
  let keys = make_keys () in
  let result =
    assert_ok
      (Dev.start ~db
         ~http_post:(http_ok (form_device_body ~interval:7 ~expires_in:600 ()))
         ~resolve_client_id ~keys ~device_flow_enabled:true ~principal_id
         ~connector_actor:actor ~source:room ~app ~intended_account:intended
         ~base_revision ~continuation_handle:continuation
         ~channel:(dm_channel ()) ~now:fixed_now ~id:"dev_sess_ok"
         ~tx_id:"tx_ok" ())
  in
  let sess = result.session in
  Alcotest.(check string) "session id" "dev_sess_ok" sess.id;
  Alcotest.(check string) "tx id" "tx_ok" sess.tx_id;
  Alcotest.(check string) "principal" principal_id sess.principal_id;
  Alcotest.(check int) "app_id" 42 sess.app.app_id;
  Alcotest.(check string) "host" "github.com" sess.app.host;
  Alcotest.(check string)
    "client handle" "h:client-id" sess.app.client_id_handle;
  Alcotest.(check (option int64))
    "intended" (Some 12345L) sess.intended_account.github_user_id;
  Alcotest.(check int) "interval" 7 sess.interval_seconds;
  Alcotest.(check string) "key_id" "mk-device-1" sess.key_id;
  Alcotest.(check int) "key_version" 1 sess.key_version;
  (* Server expiry and next_poll from now + server values. *)
  let expected_expires = Time_util.iso8601_utc ~t:(fixed_now +. 600.) () in
  let expected_next = Time_util.iso8601_utc ~t:(fixed_now +. 7.) () in
  Alcotest.(check string) "expires_at" expected_expires sess.expires_at;
  Alcotest.(check string) "next_poll_at" expected_next sess.next_poll_at;
  (* Auth tx bound as Device flow with matching expiry. *)
  Alcotest.(check string)
    "tx flow" "device"
    (Tx.string_of_flow_kind result.tx.flow_kind);
  Alcotest.(check string) "tx expires" expected_expires result.tx.expires_at;
  Alcotest.(check string)
    "tx status" "open"
    (Tx.string_of_status result.tx.status);
  (* Private delivery carries user_code + verification_uri only. *)
  (match result.delivery_plan with
  | D.Private { private_delivery; companion_room } ->
      Alcotest.(check bool)
        "user code private" true
        (contains private_delivery.rendered user_code_secret);
      Alcotest.(check bool)
        "verification uri private" true
        (contains private_delivery.rendered verification_uri);
      Alcotest.(check bool)
        "device_code not in private user body" false
        (contains private_delivery.rendered device_code_secret);
      (match companion_room with
      | None -> Alcotest.fail "expected companion room progress"
      | Some rb ->
          Alcotest.(check bool)
            "no user code in room" false
            (contains rb.rendered user_code_secret);
          Alcotest.(check bool)
            "no verification uri leak" false
            (contains rb.rendered verification_uri);
          Alcotest.(check bool)
            "no device_code in room" false
            (contains rb.rendered device_code_secret);
          Alcotest.(check bool)
            "room message safe" true
            (D.room_message_is_safe rb.rendered));
      Alcotest.(check bool)
        "redacted private summary no user code" false
        (contains private_delivery.redacted_summary user_code_secret)
  | D.Room_progress _ -> Alcotest.fail "must be private delivery"
  | D.Refused e -> Alcotest.fail ("unexpected refuse: " ^ e.message));
  (* No plaintext secrets in durable row or metadata JSON. *)
  List.iter
    (fun secret ->
      match Dev.row_contains_plaintext ~db ~id:sess.id ~plaintext:secret with
      | Ok false -> ()
      | Ok true -> Alcotest.fail ("plaintext found in row: " ^ secret)
      | Error e -> Alcotest.fail e.message)
    [ device_code_secret; user_code_secret; verification_uri; aes_key ];
  let json = Dev.session_to_json sess in
  Alcotest.(check bool)
    "json no device_code" false
    (Dev.json_contains_plaintext ~json ~plaintext:device_code_secret);
  Alcotest.(check bool)
    "json no user_code" false
    (Dev.json_contains_plaintext ~json ~plaintext:user_code_secret);
  let summary = Dev.start_result_redacted_summary result in
  Alcotest.(check bool)
    "start summary no device_code" false
    (contains summary device_code_secret);
  Alcotest.(check bool)
    "start summary no user_code" false
    (contains summary user_code_secret);
  (* Open secrets for later polling. *)
  match Dev.open_secrets ~db ~keys ~id:sess.id () with
  | Error e -> Alcotest.fail e.message
  | Ok (_s, secrets) ->
      Alcotest.(check string)
        "opened device_code" device_code_secret secrets.device_code;
      Alcotest.(check string)
        "opened user_code" user_code_secret secrets.user_code;
      Alcotest.(check string)
        "opened uri" verification_uri secrets.verification_uri

let test_start_with_json_response () =
  with_db @@ fun db ->
  let result =
    assert_ok
      (start_ok ~db
         ~http_body:(json_device_body ~interval:9 ())
         ~id:"dev_json" ~tx_id:"tx_json" ())
  in
  Alcotest.(check int) "interval from json" 9 result.session.interval_seconds

let test_get_by_tx () =
  with_db @@ fun db ->
  let result = assert_ok (start_ok ~db ~id:"dev_by_tx" ~tx_id:"tx_by_tx" ()) in
  match Dev.get_by_tx ~db ~tx_id:result.tx.id with
  | Ok (Some s) -> Alcotest.(check string) "same id" result.session.id s.id
  | Ok None -> Alcotest.fail "missing"
  | Error e -> Alcotest.fail e.message

let test_cli_private_channel () =
  with_db @@ fun db ->
  let result =
    assert_ok
      (start_ok ~db ~channel:(cli_channel ()) ~id:"dev_cli" ~tx_id:"tx_cli" ())
  in
  match result.delivery_plan with
  | D.Private _ -> ()
  | _ -> Alcotest.fail "cli channel must yield private delivery"

let test_http_error_refuses_without_partial_secret_export () =
  with_db @@ fun db ->
  let http_post ~url:_ ~headers:_ ~body:_ =
    Ok (400, "error=device_flow_disabled&error_description=not+enabled")
  in
  let err =
    assert_error
      (Dev.start ~db ~http_post ~resolve_client_id ~keys:(make_keys ())
         ~device_flow_enabled:true ~principal_id ~connector_actor:actor
         ~source:room ~app ~base_revision ~continuation_handle:continuation
         ~channel:(dm_channel ()) ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "http reason" true
    (String.starts_with ~prefix:"http:"
       (Dev.string_of_refuse_reason err.reason));
  match Dev.get ~db ~id:"nope" with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "no session on http failure"
  | Error e -> Alcotest.fail e.message

let test_device_code_url () =
  Alcotest.(check string)
    "default url" "https://github.com/login/device/code"
    (Dev.device_code_url ());
  Alcotest.(check int) "schema" 1 Dev.schema_version

let test_open_fails_wrong_key () =
  with_db @@ fun db ->
  let keys = make_keys ~key_id:"mk-a" () in
  let result =
    assert_ok
      (Dev.start ~db
         ~http_post:(http_ok (form_device_body ()))
         ~resolve_client_id ~keys ~device_flow_enabled:true ~principal_id
         ~connector_actor:actor ~source:room ~app ~base_revision
         ~continuation_handle:continuation ~channel:(dm_channel ())
         ~now:fixed_now ~id:"dev_wrong_key" ~tx_id:"tx_wrong_key" ())
  in
  let other_aes =
    Secret_store.derive_key ~iterations:1 ~passphrase:"other-device-key" ()
  in
  let other_keys =
    assert_parse_ok
      (V.make_single_key_provider ~key_id:"mk-a" ~key_version:1
         ~aes_key:other_aes ())
  in
  match Dev.open_secrets ~db ~keys:other_keys ~id:result.session.id () with
  | Error e ->
      Alcotest.(check string)
        "crypto" "crypto_failure"
        (Dev.string_of_refuse_reason e.reason);
      Alcotest.(check bool)
        "no device secret in error" false
        (contains e.message device_code_secret)
  | Ok _ -> Alcotest.fail "wrong key must fail closed"

(* -------------------------------------------------------------------------- *)
(* Suite                                                                      *)
(* -------------------------------------------------------------------------- *)

let suite =
  [
    ("parse form and json device responses", `Quick, test_parse_form_and_json);
    ("parse rejects missing fields", `Quick, test_parse_rejects_missing_fields);
    ( "refuse when device flow disabled",
      `Quick,
      test_refuse_when_device_flow_disabled );
    ("refuse absent private channel", `Quick, test_refuse_absent_private_channel);
    ( "start persists encrypted timing and private delivery",
      `Quick,
      test_start_persists_encrypted_timing_and_private_delivery );
    ("start with json response", `Quick, test_start_with_json_response);
    ("get by tx", `Quick, test_get_by_tx);
    ("cli private channel", `Quick, test_cli_private_channel);
    ( "http error refuses without partial secret export",
      `Quick,
      test_http_error_refuses_without_partial_secret_export );
    ("device code url and schema", `Quick, test_device_code_url);
    ("open fails wrong key", `Quick, test_open_fails_wrong_key);
  ]

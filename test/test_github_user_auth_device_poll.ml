(** Tests for durable leased GitHub device polling and terminal activation
    routing (P21.M2.E3.T002 + T003). *)

module Dev = Github_user_auth_device
module Poll = Github_user_auth_device_poll
module Tx = Github_user_auth_tx
module D = Github_user_auth_delivery
module A = Github_user_auth_activate
module V = Github_user_token_vault
module B = Github_account_binding
module P = Principal_identity
module PS = Principal_identity_store

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-device-poll-test" ()

let fixed_now = 1_720_000_000.0
let principal_id = "principal:alice"
let base_revision = "rev-policy-device-poll-1"
let continuation = "cont:dm:handle-device-poll-1"
let device_code_secret = "0123456789abcdef0123456789abcdef01234567"
let user_code_secret = "WDJB-MJHT"
let verification_uri = "https://github.com/login/device"
let access_token_secret = "ghu_test_access_token_device_poll_xyz"
let github_user_id = 9_876_543L
let github_login = "octocat-device"

let actor =
  match
    P.make_connector_actor_key ~connector:P.Teams
      ~tenant_or_workspace:"tenant-acme" ~immutable_user_id:"user-alice-1"
  with
  | Ok k -> k
  | Error e -> failwith e

let room = Tx.Room "room-teams-poll-1"

let app : Tx.app_client =
  { host = "github.com"; app_id = 42; client_id_handle = "h:client-id" }

let assert_ok = function
  | Ok v -> v
  | Error (e : Dev.refuse_error) -> Alcotest.fail e.message

let assert_parse_ok = function Ok v -> v | Error e -> Alcotest.fail e

let make_keys ?(key_id = "mk-device-poll-1") ?(key_version = 1) () =
  assert_parse_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key ())

let dm_channel () =
  assert_parse_ok
    (D.make_private_connector_dm ~connector:P.Teams ~handle_id:"dm:alice")

let resolve_client_id ~handle =
  if String.trim handle = "h:client-id" then Ok "Iv1.test_client_id"
  else Error ("unknown client id handle: " ^ handle)

let form_device_body ?(device_code = device_code_secret)
    ?(user_code = user_code_secret) ?(verification_uri = verification_uri)
    ?(expires_in = 900) ?(interval = 5) () =
  Printf.sprintf
    "device_code=%s&user_code=%s&verification_uri=%s&expires_in=%d&interval=%d"
    (Uri.pct_encode device_code)
    (Uri.pct_encode user_code)
    (Uri.pct_encode verification_uri)
    expires_in interval

let http_ok body ~url:_ ~headers:_ ~body:_ = Ok (200, body)

let seed_principal ~db ?(revision = 1) () =
  let pid = assert_parse_ok (P.principal_id_of_string principal_id) in
  let p =
    P.make_principal ~id:pid ~revision ~created_at:"2026-01-01T00:00:00Z"
      ~updated_at:"2026-01-01T00:00:00Z" ()
  in
  ignore (assert_parse_ok (PS.insert_principal ~db ~now:fixed_now p))

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Poll.ensure_schema db;
  A.ensure_schema db;
  seed_principal ~db ();
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fetch_user ~access_token:tok =
  if tok <> access_token_secret then Error "unexpected access token"
  else
    Ok
      {
        A.id = github_user_id;
        login = github_login;
        avatar_url = Some "https://avatars.example/d.png";
      }

let count_authorized ~db =
  match
    B.list_for_principal ~db
      ~principal_id:(assert_parse_ok (P.principal_id_of_string principal_id))
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

let count_bindings ~db =
  match
    B.list_for_principal ~db
      ~principal_id:(assert_parse_ok (P.principal_id_of_string principal_id))
  with
  | Ok xs -> List.length xs
  | Error e -> Alcotest.fail e

let assert_no_active_binding ~db =
  Alcotest.(check int) "no authorized binding" 0 (count_authorized ~db)

let grant_body ?(expires_in = 28800) ?(scope = "repo") () =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("access_token", `String access_token_secret);
         ("token_type", `String "bearer");
         ("scope", `String scope);
         ("expires_in", `Int expires_in);
       ])

let poll_handle ~db ~keys ~session_id ~worker_id ~now ~http_body ?activation_id
    ?vault_id ?binding_id ?plan_id () =
  let http_post ~url:_ ~headers:_ ~body:_ = Ok (200, http_body) in
  match
    Poll.poll_and_prepare ~db ~keys ~http_post ~resolve_client_id ~fetch_user
      ~session_id ~worker_id ~now ?activation_id ?vault_id ?binding_id ?plan_id
      ()
  with
  | Ok r -> r
  | Error e -> Alcotest.fail e.message

let start_session ?(interval = 5) ?(expires_in = 900) ?(id = "dev_poll_1")
    ?(tx_id = "tx_poll_1") ~db () =
  let keys = make_keys () in
  let result =
    assert_ok
      (Dev.start ~db
         ~http_post:(http_ok (form_device_body ~interval ~expires_in ()))
         ~resolve_client_id ~keys ~device_flow_enabled:true ~principal_id
         ~connector_actor:actor ~source:room ~app ~base_revision
         ~continuation_handle:continuation ~channel:(dm_channel ())
         ~now:fixed_now ~id ~tx_id ())
  in
  (keys, result)

let poll_ok ~db ~keys ~session_id ~worker_id ~now ~http_body =
  let http_calls = ref 0 in
  let http_post ~url:_ ~headers:_ ~body:_ =
    incr http_calls;
    Ok (200, http_body)
  in
  let outcome =
    match
      Poll.poll_once ~db ~keys ~http_post ~resolve_client_id ~session_id
        ~worker_id ~now ()
    with
    | Ok o -> o
    | Error e -> Alcotest.fail e.message
  in
  (outcome, !http_calls)

let bound_context_of_tx (tx : Tx.t) : Tx.bound_context =
  {
    principal_id = tx.principal_id;
    connector_actor = tx.connector_actor;
    source = tx.source;
    app_id = tx.app.app_id;
    base_revision = tx.base_revision;
  }

(* -------------------------------------------------------------------------- *)
(* Parse                                                                      *)
(* -------------------------------------------------------------------------- *)

let test_parse_token_success_and_errors () =
  (match
     Poll.parse_token_response
       ~body:
         (Yojson.Safe.to_string
            (`Assoc
               [
                 ("access_token", `String access_token_secret);
                 ("token_type", `String "bearer");
                 ("scope", `String "repo");
                 ("expires_in", `Int 28800);
               ]))
   with
  | Ok (Poll.Token_success t) ->
      Alcotest.(check string) "access" access_token_secret t.access_token;
      Alcotest.(check (option int)) "expires_in" (Some 28800) t.expires_in
  | Ok _ -> Alcotest.fail "expected success"
  | Error e -> Alcotest.fail e);
  (match Poll.parse_token_response ~body:"error=authorization_pending" with
  | Ok (Poll.Token_error e) ->
      Alcotest.(check string) "pending" "authorization_pending" e.error;
      Alcotest.(check (option int)) "no interval" None e.interval
  | _ -> Alcotest.fail "expected pending error");
  match
    Poll.parse_token_response
      ~body:
        (Yojson.Safe.to_string
           (`Assoc [ ("error", `String "slow_down"); ("interval", `Int 12) ]))
  with
  | Ok (Poll.Token_error e) ->
      Alcotest.(check string) "slow" "slow_down" e.error;
      Alcotest.(check (option int)) "interval" (Some 12) e.interval
  | _ -> Alcotest.fail "expected slow_down"

(* -------------------------------------------------------------------------- *)
(* Not before next_poll_at                                                    *)
(* -------------------------------------------------------------------------- *)

let test_never_polls_before_next_poll_at () =
  with_db @@ fun db ->
  let keys, started = start_session ~db ~interval:7 () in
  let sess = started.session in
  let too_early = fixed_now +. 3. in
  let http_calls = ref 0 in
  let http_post ~url:_ ~headers:_ ~body:_ =
    incr http_calls;
    Ok (200, "error=authorization_pending")
  in
  let outcome =
    match
      Poll.poll_once ~db ~keys ~http_post ~resolve_client_id ~session_id:sess.id
        ~worker_id:"w1" ~now:too_early ()
    with
    | Ok o -> o
    | Error e -> Alcotest.fail e.message
  in
  (match outcome with
  | Poll.Not_due { next_poll_at; _ } ->
      Alcotest.(check string) "next" sess.next_poll_at next_poll_at
  | o -> Alcotest.fail ("expected Not_due, got " ^ Poll.redacted_outcome o));
  Alcotest.(check int) "no http before due" 0 !http_calls

(* -------------------------------------------------------------------------- *)
(* Single worker lease                                                        *)
(* -------------------------------------------------------------------------- *)

let test_at_most_one_worker_polls () =
  with_db @@ fun db ->
  let keys, started = start_session ~db ~interval:5 () in
  let sess = started.session in
  let due = fixed_now +. 5. in
  match
    Poll.try_claim ~db ~session_id:sess.id ~worker_id:"worker-a" ~now:due ()
  with
  | Error o -> Alcotest.fail ("claim a: " ^ Poll.redacted_outcome o)
  | Ok lease_a -> (
      Alcotest.(check string) "owner a" "worker-a" lease_a.worker_id;
      (* Second worker cannot claim while lease is live. *)
      (match
         Poll.try_claim ~db ~session_id:sess.id ~worker_id:"worker-b" ~now:due
           ()
       with
      | Error (Poll.Lease_busy { owner = Some o; _ }) ->
          Alcotest.(check string) "busy owner" "worker-a" o
      | Error o ->
          Alcotest.fail ("expected Lease_busy, got " ^ Poll.redacted_outcome o)
      | Ok _ -> Alcotest.fail "second worker must not claim");
      (* poll_once also refuses HTTP when busy. *)
      let http_calls = ref 0 in
      let http_post ~url:_ ~headers:_ ~body:_ =
        incr http_calls;
        Ok (200, "error=authorization_pending")
      in
      (match
         Poll.poll_once ~db ~keys ~http_post ~resolve_client_id
           ~session_id:sess.id ~worker_id:"worker-b" ~now:due ()
       with
      | Ok (Poll.Lease_busy _) -> ()
      | Ok o -> Alcotest.fail ("expected busy, got " ^ Poll.redacted_outcome o)
      | Error e -> Alcotest.fail e.message);
      Alcotest.(check int) "no http from busy worker" 0 !http_calls;
      (* After release, another worker can claim. *)
      ignore
        (assert_ok
           (Poll.release_lease ~db ~session_id:sess.id ~token:lease_a.token
              ~now:due ()));
      match
        Poll.try_claim ~db ~session_id:sess.id ~worker_id:"worker-b" ~now:due ()
      with
      | Ok lease_b ->
          Alcotest.(check string) "b wins" "worker-b" lease_b.worker_id
      | Error o -> Alcotest.fail ("claim b: " ^ Poll.redacted_outcome o))

(* -------------------------------------------------------------------------- *)
(* Restart preserves expiry                                                   *)
(* -------------------------------------------------------------------------- *)

let test_restart_does_not_reset_expiry () =
  with_db @@ fun db ->
  let keys, started =
    start_session ~db ~interval:5 ~expires_in:600 ~id:"dev_restart"
      ~tx_id:"tx_restart" ()
  in
  let sess = started.session in
  let expected_expires = Time_util.iso8601_utc ~t:(fixed_now +. 600.) () in
  Alcotest.(check string) "initial expires" expected_expires sess.expires_at;
  (* Simulate restart: re-load session metadata only. *)
  let reloaded =
    match Dev.get ~db ~id:sess.id with
    | Ok (Some s) -> s
    | Ok None -> Alcotest.fail "missing after restart"
    | Error e -> Alcotest.fail e.message
  in
  Alcotest.(check string)
    "expires unchanged after reload" expected_expires reloaded.expires_at;
  Alcotest.(check string)
    "next_poll preserved" sess.next_poll_at reloaded.next_poll_at;
  let state =
    match Poll.get_poll_state ~db ~session_id:sess.id with
    | Ok (Some st) -> st
    | _ -> Alcotest.fail "poll state missing"
  in
  Alcotest.(check string) "state expires" expected_expires state.expires_at;
  (* Poll after due still uses original expires_at (not extended). *)
  let due = fixed_now +. 5. in
  let outcome, calls =
    poll_ok ~db ~keys ~session_id:sess.id ~worker_id:"w1" ~now:due
      ~http_body:"error=authorization_pending"
  in
  Alcotest.(check int) "polled once" 1 calls;
  (match outcome with
  | Poll.Authorization_pending _ -> ()
  | o -> Alcotest.fail (Poll.redacted_outcome o));
  let after =
    match Dev.get ~db ~id:sess.id with
    | Ok (Some s) -> s
    | _ -> Alcotest.fail "missing"
  in
  Alcotest.(check string)
    "expires still original after poll" expected_expires after.expires_at

(* -------------------------------------------------------------------------- *)
(* authorization_pending keeps interval                                       *)
(* -------------------------------------------------------------------------- *)

let test_authorization_pending_retains_interval () =
  with_db @@ fun db ->
  let keys, started = start_session ~db ~interval:7 () in
  let sess = started.session in
  let due = fixed_now +. 7. in
  let outcome, calls =
    poll_ok ~db ~keys ~session_id:sess.id ~worker_id:"w1" ~now:due
      ~http_body:"error=authorization_pending&error_description=waiting"
  in
  Alcotest.(check int) "http" 1 calls;
  match outcome with
  | Poll.Authorization_pending t -> (
      Alcotest.(check int) "interval retained" 7 t.interval_seconds;
      let expected_next = Time_util.iso8601_utc ~t:(due +. 7.) () in
      Alcotest.(check string)
        "next advanced by interval" expected_next t.next_poll_at;
      Alcotest.(check int) "session interval" 7 t.session.interval_seconds;
      let state =
        match Poll.get_poll_state ~db ~session_id:sess.id with
        | Ok (Some st) -> st
        | _ -> Alcotest.fail "state"
      in
      Alcotest.(check int) "durable interval" 7 state.interval_seconds;
      Alcotest.(check string) "durable next" expected_next state.next_poll_at;
      Alcotest.(check bool) "lease cleared" true (state.poll_lease_token = None);
      (* Immediate re-poll is not due. *)
      let again, calls2 =
        poll_ok ~db ~keys ~session_id:sess.id ~worker_id:"w1" ~now:due
          ~http_body:"error=authorization_pending"
      in
      Alcotest.(check int) "no second http" 0 calls2;
      match again with
      | Poll.Not_due _ -> ()
      | o -> Alcotest.fail (Poll.redacted_outcome o))
  | o -> Alcotest.fail (Poll.redacted_outcome o)

(* -------------------------------------------------------------------------- *)
(* slow_down                                                                  *)
(* -------------------------------------------------------------------------- *)

let test_slow_down_uses_returned_interval () =
  with_db @@ fun db ->
  let keys, started = start_session ~db ~interval:5 () in
  let sess = started.session in
  let due = fixed_now +. 5. in
  let body =
    Yojson.Safe.to_string
      (`Assoc [ ("error", `String "slow_down"); ("interval", `Int 15) ])
  in
  let outcome, _ =
    poll_ok ~db ~keys ~session_id:sess.id ~worker_id:"w1" ~now:due
      ~http_body:body
  in
  match outcome with
  | Poll.Slow_down t ->
      Alcotest.(check int) "server interval" 15 t.interval_seconds;
      let expected_next = Time_util.iso8601_utc ~t:(due +. 15.) () in
      Alcotest.(check string) "next" expected_next t.next_poll_at
  | o -> Alcotest.fail (Poll.redacted_outcome o)

let test_slow_down_adds_five_when_no_interval () =
  with_db @@ fun db ->
  let keys, started = start_session ~db ~interval:5 () in
  let sess = started.session in
  let due = fixed_now +. 5. in
  let outcome, _ =
    poll_ok ~db ~keys ~session_id:sess.id ~worker_id:"w1" ~now:due
      ~http_body:"error=slow_down"
  in
  match outcome with
  | Poll.Slow_down t -> (
      Alcotest.(check int)
        "interval +5"
        (5 + Poll.slow_down_extra_seconds)
        t.interval_seconds;
      let expected_next =
        Time_util.iso8601_utc
          ~t:(due +. float_of_int (5 + Poll.slow_down_extra_seconds))
          ()
      in
      Alcotest.(check string) "next +5" expected_next t.next_poll_at;
      (* Second slow_down without interval adds another 5 to the new base. *)
      let later = due +. float_of_int t.interval_seconds in
      let outcome2, _ =
        poll_ok ~db ~keys ~session_id:sess.id ~worker_id:"w1" ~now:later
          ~http_body:"error=slow_down"
      in
      match outcome2 with
      | Poll.Slow_down t2 ->
          Alcotest.(check int)
            "stacked +5"
            (5 + (2 * Poll.slow_down_extra_seconds))
            t2.interval_seconds
      | o -> Alcotest.fail (Poll.redacted_outcome o))
  | o -> Alcotest.fail (Poll.redacted_outcome o)

(* -------------------------------------------------------------------------- *)
(* Cancel / expiry stop future polls                                          *)
(* -------------------------------------------------------------------------- *)

let test_cancel_stops_future_polls () =
  with_db @@ fun db ->
  let keys, started = start_session ~db ~interval:5 () in
  let sess = started.session in
  let tx = started.tx in
  let due = fixed_now +. 5. in
  let _ =
    match
      Tx.cancel ~db ~id:tx.id ~context:(bound_context_of_tx tx) ~now:due ()
    with
    | Ok t -> t
    | Error e -> Alcotest.fail e
  in
  let http_calls = ref 0 in
  let http_post ~url:_ ~headers:_ ~body:_ =
    incr http_calls;
    Ok (200, "error=authorization_pending")
  in
  let outcome =
    match
      Poll.poll_once ~db ~keys ~http_post ~resolve_client_id ~session_id:sess.id
        ~worker_id:"w1" ~now:due ()
    with
    | Ok o -> o
    | Error e -> Alcotest.fail e.message
  in
  (match outcome with
  | Poll.Stopped { reason = Poll.Cancelled; _ } -> ()
  | o ->
      Alcotest.fail ("expected cancelled stop, got " ^ Poll.redacted_outcome o));
  Alcotest.(check int) "no http after cancel" 0 !http_calls;
  let state =
    match Poll.get_poll_state ~db ~session_id:sess.id with
    | Ok (Some st) -> st
    | _ -> Alcotest.fail "state"
  in
  Alcotest.(check bool) "stopped" true (Poll.is_stopped state);
  (* Subsequent poll also stopped without HTTP. *)
  let later = due +. 60. in
  let outcome2 =
    match
      Poll.poll_once ~db ~keys ~http_post ~resolve_client_id ~session_id:sess.id
        ~worker_id:"w1" ~now:later ()
    with
    | Ok o -> o
    | Error e -> Alcotest.fail e.message
  in
  (match outcome2 with
  | Poll.Stopped { reason = Poll.Cancelled; _ } -> ()
  | o -> Alcotest.fail (Poll.redacted_outcome o));
  Alcotest.(check int) "still no http" 0 !http_calls

let test_local_expiry_stops_future_polls () =
  with_db @@ fun db ->
  let keys, started =
    start_session ~db ~interval:5 ~expires_in:30 ~id:"dev_exp" ~tx_id:"tx_exp"
      ()
  in
  let sess = started.session in
  let past_expiry = fixed_now +. 30. in
  let http_calls = ref 0 in
  let http_post ~url:_ ~headers:_ ~body:_ =
    incr http_calls;
    Ok (200, "error=authorization_pending")
  in
  let outcome =
    match
      Poll.poll_once ~db ~keys ~http_post ~resolve_client_id ~session_id:sess.id
        ~worker_id:"w1" ~now:past_expiry ()
    with
    | Ok o -> o
    | Error e -> Alcotest.fail e.message
  in
  (match outcome with
  | Poll.Stopped { reason = Poll.Expired; _ } -> ()
  | o -> Alcotest.fail ("expected expired, got " ^ Poll.redacted_outcome o));
  Alcotest.(check int) "no http on local expiry" 0 !http_calls;
  let state =
    match Poll.get_poll_state ~db ~session_id:sess.id with
    | Ok (Some st) -> st
    | _ -> Alcotest.fail "state"
  in
  Alcotest.(check bool) "stopped" true (Poll.is_stopped state);
  (* Original expires_at still the durable server value. *)
  Alcotest.(check string)
    "expires durable"
    (Time_util.iso8601_utc ~t:(fixed_now +. 30.) ())
    state.expires_at

let test_github_expired_token_stops_polls () =
  with_db @@ fun db ->
  let keys, started = start_session ~db ~interval:5 () in
  let sess = started.session in
  let due = fixed_now +. 5. in
  let outcome, calls =
    poll_ok ~db ~keys ~session_id:sess.id ~worker_id:"w1" ~now:due
      ~http_body:"error=expired_token&error_description=expired"
  in
  Alcotest.(check int) "http once" 1 calls;
  (match outcome with
  | Poll.Stopped { reason = Poll.Device_code_expired; _ } -> ()
  | o -> Alcotest.fail (Poll.redacted_outcome o));
  let again, calls2 =
    poll_ok ~db ~keys ~session_id:sess.id ~worker_id:"w1" ~now:(due +. 60.)
      ~http_body:"error=authorization_pending"
  in
  Alcotest.(check int) "no further http" 0 calls2;
  match again with
  | Poll.Stopped { reason = Poll.Device_code_expired; _ } -> ()
  | o -> Alcotest.fail (Poll.redacted_outcome o)

let test_access_granted_stops_and_returns_token () =
  with_db @@ fun db ->
  let keys, started = start_session ~db ~interval:5 () in
  let sess = started.session in
  let due = fixed_now +. 5. in
  let body =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("access_token", `String access_token_secret);
           ("token_type", `String "bearer");
           ("scope", `String "repo");
         ])
  in
  let outcome, calls =
    poll_ok ~db ~keys ~session_id:sess.id ~worker_id:"w1" ~now:due
      ~http_body:body
  in
  Alcotest.(check int) "http" 1 calls;
  (match outcome with
  | Poll.Granted g ->
      Alcotest.(check string) "token" access_token_secret g.tokens.access_token;
      let red = Poll.redacted_outcome outcome in
      Alcotest.(check bool)
        "redacted no token" false
        (Test_helpers.string_contains red access_token_secret)
  | o -> Alcotest.fail (Poll.redacted_outcome o));
  let again, calls2 =
    poll_ok ~db ~keys ~session_id:sess.id ~worker_id:"w1" ~now:(due +. 5.)
      ~http_body:body
  in
  Alcotest.(check int) "no re-poll after grant" 0 calls2;
  match again with
  | Poll.Stopped { reason = Poll.Access_granted; _ } -> ()
  | o -> Alcotest.fail (Poll.redacted_outcome o)

let test_device_code_not_logged_in_outcomes () =
  with_db @@ fun db ->
  let keys, started = start_session ~db ~interval:5 () in
  let sess = started.session in
  let due = fixed_now +. 5. in
  let outcome, _ =
    poll_ok ~db ~keys ~session_id:sess.id ~worker_id:"w1" ~now:due
      ~http_body:"error=authorization_pending"
  in
  let red = Poll.redacted_outcome outcome in
  Alcotest.(check bool)
    "no device_code" false
    (Test_helpers.string_contains red device_code_secret);
  let state =
    match Poll.get_poll_state ~db ~session_id:sess.id with
    | Ok (Some st) -> st
    | _ -> Alcotest.fail "state"
  in
  let sred = Poll.redacted_poll_state state in
  Alcotest.(check bool)
    "state no device_code" false
    (Test_helpers.string_contains sred device_code_secret)

let test_access_token_url () =
  Alcotest.(check string)
    "url" "https://github.com/login/oauth/access_token"
    (Poll.access_token_url ());
  Alcotest.(check string)
    "grant" Poll.device_grant_type
    "urn:ietf:params:oauth:grant-type:device_code";
  Alcotest.(check int) "schema" 1 Poll.schema_version;
  Alcotest.(check int) "slow_down extra" 5 Poll.slow_down_extra_seconds

(* -------------------------------------------------------------------------- *)
(* T003: Granted → shared Activate.prepare                                    *)
(* -------------------------------------------------------------------------- *)

let test_granted_routes_through_shared_activation () =
  with_db @@ fun db ->
  let keys, started =
    start_session ~db ~interval:5 ~id:"dev_act" ~tx_id:"tx_act" ()
  in
  let sess = started.session in
  let due = fixed_now +. 5. in
  let result =
    poll_handle ~db ~keys ~session_id:sess.id ~worker_id:"w1" ~now:due
      ~http_body:(grant_body ()) ~activation_id:"act_dev_1"
      ~vault_id:"vault_dev_1" ~binding_id:"bind_dev_1" ~plan_id:"plan_dev_1" ()
  in
  match result with
  | Poll.Prepared p ->
      Alcotest.(check string) "auth tx" "tx_act" p.auth_tx_id;
      Alcotest.(check string)
        "activation pending" "pending_confirmation"
        (A.string_of_activation_status p.prepared.activation.status);
      Alcotest.(check string)
        "binding pending" "pending"
        (B.string_of_authorization_status
           p.prepared.binding.authorization_status);
      Alcotest.(check bool)
        "not active" false
        (A.has_active_binding ~binding:p.prepared.binding);
      Alcotest.(check string) "vault" "vault_dev_1" p.prepared.vault.id;
      Alcotest.(check string) "binding" "bind_dev_1" p.prepared.binding.id;
      Alcotest.(check string) "plan" "plan_dev_1" p.prepared.plan.plan_id;
      Alcotest.(check bool)
        "user id" true
        (Int64.equal p.prepared.github_user.id github_user_id);
      Alcotest.(check int) "no authorized" 0 (count_authorized ~db);
      Alcotest.(check int) "one pending binding" 1 (count_bindings ~db);
      let red = Poll.redacted_handle_result result in
      Alcotest.(check bool)
        "redacted no access" false
        (Test_helpers.string_contains red access_token_secret);
      Alcotest.(check bool)
        "redacted no confirm" false
        (Test_helpers.string_contains red p.prepared.confirmation_token);
      (* Auth tx remains open for activation eligibility. *)
      (match Tx.get ~db ~id:p.auth_tx_id with
      | Ok (Some tx) ->
          Alcotest.(check string)
            "tx still open" "open"
            (Tx.string_of_status tx.status)
      | _ -> Alcotest.fail "tx missing");
      (* Private confirm → Authorized via shared activation. *)
      let activated =
        match
          A.confirm ~db ~keys ~activation_id:p.prepared.activation.id
            ~confirmation_token:p.prepared.confirmation_token
            ~expected_principal_id:principal_id
            ~expected_plan_digest:p.prepared.plan.digest ~now:(due +. 10.) ()
        with
        | Ok v -> v
        | Error e ->
            Alcotest.fail
              (e.message ^ " [" ^ A.string_of_failure_kind e.kind ^ "]")
      in
      Alcotest.(check string)
        "activated" "activated"
        (A.string_of_activation_status activated.activation.status);
      Alcotest.(check int)
        "one authorized after confirm" 1 (count_authorized ~db)
  | Poll.Continuing o ->
      Alcotest.fail
        ("expected Prepared, got continuing " ^ Poll.redacted_outcome o)
  | Poll.Terminated t ->
      Alcotest.fail
        (Printf.sprintf "expected Prepared, got terminated %s: %s"
           (Poll.string_of_stop_reason t.reason)
           t.message)

let test_granted_missing_expires_in_no_binding () =
  with_db @@ fun db ->
  let keys, started =
    start_session ~db ~interval:5 ~id:"dev_noexp" ~tx_id:"tx_noexp" ()
  in
  let sess = started.session in
  let due = fixed_now +. 5. in
  (* Grant without expires_in is still a poll Granted, but prepare fails closed. *)
  let body =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("access_token", `String access_token_secret);
           ("token_type", `String "bearer");
           ("scope", `String "repo");
         ])
  in
  let result =
    poll_handle ~db ~keys ~session_id:sess.id ~worker_id:"w1" ~now:due
      ~http_body:body ()
  in
  (match result with
  | Poll.Terminated t ->
      Alcotest.(check string)
        "invalid credential terminal" "terminal:invalid_credential"
        (Poll.string_of_stop_reason t.reason);
      Alcotest.(check bool)
        "repair mentions expires or restart" true
        (let lower = String.lowercase_ascii t.repair in
         Test_helpers.string_contains lower "expires"
         || Test_helpers.string_contains lower "restart"
         || Test_helpers.string_contains lower "device")
  | Poll.Prepared _ -> Alcotest.fail "missing expires_in must not prepare"
  | Poll.Continuing o -> Alcotest.fail (Poll.redacted_outcome o));
  assert_no_active_binding ~db;
  Alcotest.(check int) "no bindings" 0 (count_bindings ~db);
  match Tx.get ~db ~id:started.tx.id with
  | Ok (Some tx) ->
      Alcotest.(check bool) "tx terminal" true (Tx.status_is_terminal tx.status)
  | _ -> Alcotest.fail "tx missing"

(* -------------------------------------------------------------------------- *)
(* T003: every terminal failure — no partial binding                          *)
(* -------------------------------------------------------------------------- *)

let assert_terminal_no_binding ~db ~keys ~session_id ~tx_id ~now ~http_body
    ~expected_reason_substr =
  let result =
    poll_handle ~db ~keys ~session_id ~worker_id:"w1" ~now ~http_body ()
  in
  (match result with
  | Poll.Terminated t ->
      let reason = Poll.string_of_stop_reason t.reason in
      Alcotest.(check bool)
        ("reason contains " ^ expected_reason_substr)
        true
        (Test_helpers.string_contains reason expected_reason_substr
        || Test_helpers.string_contains
             (String.lowercase_ascii t.message)
             expected_reason_substr);
      Alcotest.(check bool)
        "repair non-empty" true
        (String.length (String.trim t.repair) > 0);
      let red = Poll.redacted_handle_result result in
      Alcotest.(check bool)
        "redacted no device_code" false
        (Test_helpers.string_contains red device_code_secret);
      Alcotest.(check bool)
        "redacted no access" false
        (Test_helpers.string_contains red access_token_secret)
  | Poll.Prepared _ -> Alcotest.fail "terminal must not prepare"
  | Poll.Continuing o ->
      Alcotest.fail ("expected Terminated, got " ^ Poll.redacted_outcome o));
  assert_no_active_binding ~db;
  Alcotest.(check int) "no bindings" 0 (count_bindings ~db);
  (match Poll.get_poll_state ~db ~session_id with
  | Ok (Some st) ->
      Alcotest.(check bool) "poll stopped" true (Poll.is_stopped st)
  | _ -> Alcotest.fail "poll state");
  match Tx.get ~db ~id:tx_id with
  | Ok (Some tx) ->
      Alcotest.(check bool)
        "auth tx terminal" true
        (Tx.status_is_terminal tx.status)
  | _ -> Alcotest.fail "tx missing"

let test_terminal_access_denied_no_binding () =
  with_db @@ fun db ->
  let keys, started =
    start_session ~db ~interval:5 ~id:"dev_deny" ~tx_id:"tx_deny" ()
  in
  assert_terminal_no_binding ~db ~keys ~session_id:started.session.id
    ~tx_id:started.tx.id ~now:(fixed_now +. 5.)
    ~http_body:"error=access_denied&error_description=user+denied"
    ~expected_reason_substr:"access_denied"

let test_terminal_expired_token_no_binding () =
  with_db @@ fun db ->
  let keys, started =
    start_session ~db ~interval:5 ~id:"dev_ghexp" ~tx_id:"tx_ghexp" ()
  in
  assert_terminal_no_binding ~db ~keys ~session_id:started.session.id
    ~tx_id:started.tx.id ~now:(fixed_now +. 5.) ~http_body:"error=expired_token"
    ~expected_reason_substr:"device_code_expired"

let test_terminal_unsupported_grant_no_binding () =
  with_db @@ fun db ->
  let keys, started =
    start_session ~db ~interval:5 ~id:"dev_ug" ~tx_id:"tx_ug" ()
  in
  assert_terminal_no_binding ~db ~keys ~session_id:started.session.id
    ~tx_id:started.tx.id ~now:(fixed_now +. 5.)
    ~http_body:"error=unsupported_grant_type"
    ~expected_reason_substr:"unsupported_grant"

let test_terminal_incorrect_device_code_no_binding () =
  with_db @@ fun db ->
  let keys, started =
    start_session ~db ~interval:5 ~id:"dev_idc" ~tx_id:"tx_idc" ()
  in
  assert_terminal_no_binding ~db ~keys ~session_id:started.session.id
    ~tx_id:started.tx.id ~now:(fixed_now +. 5.)
    ~http_body:"error=incorrect_device_code"
    ~expected_reason_substr:"incorrect_device_code"

let test_terminal_incorrect_client_no_binding () =
  with_db @@ fun db ->
  let keys, started =
    start_session ~db ~interval:5 ~id:"dev_ic" ~tx_id:"tx_ic" ()
  in
  assert_terminal_no_binding ~db ~keys ~session_id:started.session.id
    ~tx_id:started.tx.id ~now:(fixed_now +. 5.)
    ~http_body:"error=invalid_client" ~expected_reason_substr:"incorrect_client"

let test_terminal_disabled_flow_no_binding () =
  with_db @@ fun db ->
  let keys, started =
    start_session ~db ~interval:5 ~id:"dev_dis" ~tx_id:"tx_dis" ()
  in
  assert_terminal_no_binding ~db ~keys ~session_id:started.session.id
    ~tx_id:started.tx.id ~now:(fixed_now +. 5.)
    ~http_body:"error=device_flow_disabled"
    ~expected_reason_substr:"device_flow_disabled"

let test_terminal_malformed_no_binding () =
  with_db @@ fun db ->
  let keys, started =
    start_session ~db ~interval:5 ~id:"dev_mal" ~tx_id:"tx_mal" ()
  in
  assert_terminal_no_binding ~db ~keys ~session_id:started.session.id
    ~tx_id:started.tx.id ~now:(fixed_now +. 5.) ~http_body:"{{{not-json-or-form"
    ~expected_reason_substr:"malformed"

let test_terminal_unknown_error_no_binding () =
  with_db @@ fun db ->
  let keys, started =
    start_session ~db ~interval:5 ~id:"dev_unk" ~tx_id:"tx_unk" ()
  in
  assert_terminal_no_binding ~db ~keys ~session_id:started.session.id
    ~tx_id:started.tx.id ~now:(fixed_now +. 5.)
    ~http_body:"error=some_future_github_error"
    ~expected_reason_substr:"some_future_github_error"

let test_terminal_local_expiry_via_handle () =
  with_db @@ fun db ->
  let keys, started =
    start_session ~db ~interval:5 ~expires_in:30 ~id:"dev_lexp" ~tx_id:"tx_lexp"
      ()
  in
  let past = fixed_now +. 30. in
  let result =
    poll_handle ~db ~keys ~session_id:started.session.id ~worker_id:"w1"
      ~now:past ~http_body:"error=authorization_pending" ()
  in
  (match result with
  | Poll.Terminated t ->
      Alcotest.(check string)
        "expired" "expired"
        (Poll.string_of_stop_reason t.reason)
  | _ -> Alcotest.fail "expected local expiry terminated");
  assert_no_active_binding ~db;
  Alcotest.(check int) "no bindings" 0 (count_bindings ~db)

let test_activation_collision_preserves_prior_authorized () =
  with_db @@ fun db ->
  let keys, started =
    start_session ~db ~interval:5 ~id:"dev_col" ~tx_id:"tx_col" ()
  in
  let identity =
    assert_parse_ok (B.make_account_identity ~app_id:42 ~github_user_id ())
  in
  let vault_ref = assert_parse_ok (B.make_vault_ref "prior_vault") in
  let prior =
    B.make_binding ~id:"prior_authorized"
      ~principal_id:(assert_parse_ok (P.principal_id_of_string principal_id))
      ~identity ~authorization_status:B.Authorized ~vault_ref ()
  in
  ignore (assert_parse_ok (B.insert ~db ~now:fixed_now prior));
  let due = fixed_now +. 5. in
  let result =
    poll_handle ~db ~keys ~session_id:started.session.id ~worker_id:"w1"
      ~now:due ~http_body:(grant_body ()) ~activation_id:"act_col"
      ~vault_id:"vault_should_not_stick" ~binding_id:"bind_should_not_stick" ()
  in
  (match result with
  | Poll.Terminated t ->
      Alcotest.(check bool)
        "activation failed terminal" true
        (Test_helpers.string_contains
           (Poll.string_of_stop_reason t.reason)
           "activation_failed"
        || Test_helpers.string_contains
             (String.lowercase_ascii t.message)
             "collision");
      Alcotest.(check bool)
        "repair mentions collision or unlink" true
        (let lower = String.lowercase_ascii t.repair in
         Test_helpers.string_contains lower "collision"
         || Test_helpers.string_contains lower "unlink"
         || Test_helpers.string_contains lower "binding")
  | Poll.Prepared _ -> Alcotest.fail "collision must not prepare"
  | Poll.Continuing o -> Alcotest.fail (Poll.redacted_outcome o));
  Alcotest.(check int) "prior authorized preserved" 1 (count_authorized ~db);
  Alcotest.(check int) "only prior binding" 1 (count_bindings ~db);
  (match V.get_meta ~db ~id:"vault_should_not_stick" with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "collision must not leave new vault"
  | Error d -> Alcotest.fail (V.string_of_denial d));
  match B.get ~db ~id:"prior_authorized" with
  | Ok (Some b) ->
      Alcotest.(check string)
        "prior still authorized" "authorized"
        (B.string_of_authorization_status b.authorization_status)
  | _ -> Alcotest.fail "prior binding missing"

let test_credential_of_token_success_requires_expires_in () =
  let tok : Poll.token_success =
    {
      access_token = access_token_secret;
      token_type = Some "bearer";
      scope = Some "repo";
      expires_in = None;
      refresh_token = None;
    }
  in
  (match Poll.credential_of_token_success tok with
  | Error msg ->
      Alcotest.(check bool)
        "mentions expires" true
        (Test_helpers.string_contains (String.lowercase_ascii msg) "expires_in")
  | Ok _ -> Alcotest.fail "must require expires_in");
  match
    Poll.credential_of_token_success
      { tok with expires_in = Some 100; scope = Some "repo read:user" }
  with
  | Ok c ->
      Alcotest.(check int) "expires" 100 c.expires_in;
      Alcotest.(check (list string)) "scopes" [ "repo"; "read:user" ] c.scopes
  | Error e -> Alcotest.fail e

(* -------------------------------------------------------------------------- *)
(* Suite                                                                      *)
(* -------------------------------------------------------------------------- *)

let suite =
  [
    ( "parse token success and errors",
      `Quick,
      test_parse_token_success_and_errors );
    ( "never polls before next_poll_at",
      `Quick,
      test_never_polls_before_next_poll_at );
    ("at most one worker polls", `Quick, test_at_most_one_worker_polls);
    ("restart does not reset expiry", `Quick, test_restart_does_not_reset_expiry);
    ( "authorization_pending retains interval",
      `Quick,
      test_authorization_pending_retains_interval );
    ( "slow_down uses returned interval",
      `Quick,
      test_slow_down_uses_returned_interval );
    ( "slow_down adds five when no interval",
      `Quick,
      test_slow_down_adds_five_when_no_interval );
    ("cancel stops future polls", `Quick, test_cancel_stops_future_polls);
    ( "local expiry stops future polls",
      `Quick,
      test_local_expiry_stops_future_polls );
    ( "github expired_token stops polls",
      `Quick,
      test_github_expired_token_stops_polls );
    ( "access granted stops and returns token",
      `Quick,
      test_access_granted_stops_and_returns_token );
    ( "device code not in redacted outcomes",
      `Quick,
      test_device_code_not_logged_in_outcomes );
    ("access token url and constants", `Quick, test_access_token_url);
    (* T003 *)
    ( "granted routes through shared activation",
      `Quick,
      test_granted_routes_through_shared_activation );
    ( "granted missing expires_in no binding",
      `Quick,
      test_granted_missing_expires_in_no_binding );
    ( "terminal access_denied no binding",
      `Quick,
      test_terminal_access_denied_no_binding );
    ( "terminal expired_token no binding",
      `Quick,
      test_terminal_expired_token_no_binding );
    ( "terminal unsupported_grant no binding",
      `Quick,
      test_terminal_unsupported_grant_no_binding );
    ( "terminal incorrect_device_code no binding",
      `Quick,
      test_terminal_incorrect_device_code_no_binding );
    ( "terminal incorrect_client no binding",
      `Quick,
      test_terminal_incorrect_client_no_binding );
    ( "terminal disabled flow no binding",
      `Quick,
      test_terminal_disabled_flow_no_binding );
    ("terminal malformed no binding", `Quick, test_terminal_malformed_no_binding);
    ( "terminal unknown error no binding",
      `Quick,
      test_terminal_unknown_error_no_binding );
    ( "terminal local expiry via handle",
      `Quick,
      test_terminal_local_expiry_via_handle );
    ( "activation collision preserves prior authorized",
      `Quick,
      test_activation_collision_preserves_prior_authorized );
    ( "credential_of_token_success requires expires_in",
      `Quick,
      test_credential_of_token_success_requires_expires_in );
  ]

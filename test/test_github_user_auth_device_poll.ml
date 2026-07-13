(** Tests for durable leased GitHub device polling (P21.M2.E3.T002). *)

module Dev = Github_user_auth_device
module Poll = Github_user_auth_device_poll
module Tx = Github_user_auth_tx
module D = Github_user_auth_delivery
module V = Github_user_token_vault
module P = Principal_identity

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

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Poll.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

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
  ]

(** Tests for state-bound S256 PKCE authorization start (P21.M2.E2.T001). *)

module Pkce = Github_user_auth_pkce
module Tx = Github_user_auth_tx
module D = Github_user_auth_delivery
module S = Github_user_token_store
module P = Principal_identity

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Tx.ensure_schema db;
  Pkce.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let principal_id = "principal:alice"
let base_revision = "rev-policy-1"
let continuation = "cont:dm:handle-1"
let registered = "https://clawq.example/oauth/github/callback"
let client_id = "Iv1.testclientid0001"

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
let contains hay needle = Test_helpers.string_contains hay needle
let fresh_store () = S.make_in_memory_secret_store ()

let start_with_store ?requested_redirect_uri ?challenge_method ?one_time_state
    ?id f =
  with_db @@ fun db ->
  let store, table = fresh_store () in
  let result =
    Pkce.start ~db ~store ~principal_id ~connector_actor:actor ~source:room ~app
      ~client_id ~registered_redirect_uri:registered ?requested_redirect_uri
      ~base_revision ~continuation_handle:continuation ~now:fixed_now ?id
      ?one_time_state ?challenge_method ()
  in
  f db store table result

(* -------------------------------------------------------------------------- *)
(* Generators and S256                                                        *)
(* -------------------------------------------------------------------------- *)

let test_independent_high_entropy_state_and_verifier () =
  Pkce.ensure_rng_initialized ();
  let s1 = Pkce.generate_state () in
  let s2 = Pkce.generate_state () in
  let v1 = Pkce.generate_code_verifier () in
  let v2 = Pkce.generate_code_verifier () in
  Alcotest.(check bool) "state non-empty" true (String.length s1 >= 32);
  Alcotest.(check bool) "state independent" true (s1 <> s2);
  Alcotest.(check bool) "verifier length RFC" true (String.length v1 >= 43);
  Alcotest.(check bool) "verifier independent" true (v1 <> v2);
  Alcotest.(check bool) "state != verifier" true (s1 <> v1)

let test_s256_challenge () =
  (* RFC 7636 Appendix B test vector. *)
  let verifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk" in
  let challenge = Pkce.code_challenge_s256 ~code_verifier:verifier in
  Alcotest.(check string)
    "RFC 7636 S256 vector" "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
    challenge

let test_reject_plain_and_none_method () =
  (match Pkce.challenge_method_of_string "plain" with
  | Error msg ->
      Alcotest.(check bool)
        "mentions plain" true
        (contains (String.lowercase_ascii msg) "plain")
  | Ok _ -> Alcotest.fail "plain must be rejected");
  (match Pkce.challenge_method_of_string "none" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "none must be rejected");
  (match Pkce.challenge_method_of_string "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "empty method must be rejected");
  match Pkce.challenge_method_of_string "S256" with
  | Ok Pkce.S256 -> ()
  | Error e -> Alcotest.fail e

let test_start_rejects_plain_method () =
  with_db @@ fun db ->
  let store, _ = fresh_store () in
  match
    Pkce.start ~db ~store ~principal_id ~connector_actor:actor ~source:room ~app
      ~client_id ~registered_redirect_uri:registered ~base_revision
      ~continuation_handle:continuation ~challenge_method:"plain" ~now:fixed_now
      ()
  with
  | Error msg ->
      Alcotest.(check bool)
        "refuses plain" true
        (contains (String.lowercase_ascii msg) "plain")
  | Ok _ -> Alcotest.fail "start with plain PKCE must fail"

(* -------------------------------------------------------------------------- *)
(* Redirect                                                                   *)
(* -------------------------------------------------------------------------- *)

let test_reject_unregistered_redirect () =
  with_db @@ fun db ->
  let store, _ = fresh_store () in
  match
    Pkce.start ~db ~store ~principal_id ~connector_actor:actor ~source:room ~app
      ~client_id ~registered_redirect_uri:registered
      ~requested_redirect_uri:"https://evil.example/steal" ~base_revision
      ~continuation_handle:continuation ~now:fixed_now ()
  with
  | Error msg ->
      Alcotest.(check bool)
        "unregistered" true
        (contains (String.lowercase_ascii msg) "registered"
        || contains (String.lowercase_ascii msg) "redirect")
  | Ok _ -> Alcotest.fail "unregistered redirect must fail"

let test_reject_mutated_redirect () =
  match
    Pkce.require_exact_redirect ~registered ~requested:(registered ^ "?extra=1")
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "query mutation must fail exact match"

let test_reject_http_registered () =
  Alcotest.(check bool)
    "http invalid" false
    (Pkce.registered_redirect_valid "http://clawq.example/oauth/callback");
  Alcotest.(check bool)
    "https valid" true
    (Pkce.registered_redirect_valid registered)

(* -------------------------------------------------------------------------- *)
(* Happy path                                                                 *)
(* -------------------------------------------------------------------------- *)

let test_start_builds_s256_url_and_protects_verifier () =
  start_with_store @@ fun db store table result ->
  let r = assert_ok result in
  Alcotest.(check string)
    "flow web_pkce" "web_pkce"
    (Tx.string_of_flow_kind r.tx.flow_kind);
  Alcotest.(check string) "status open" "open" (Tx.string_of_status r.tx.status);
  Alcotest.(check string)
    "state bound" r.tx.one_time_state r.material.one_time_state;
  Alcotest.(check string)
    "method S256" "S256"
    (Pkce.string_of_challenge_method r.material.code_challenge_method);
  Alcotest.(check string) "exact redirect" registered r.material.redirect_uri;
  (* URL carries exact redirect and S256 only. *)
  Alcotest.(check bool)
    "authorize endpoint" true
    (contains r.authorization_url "https://github.com/login/oauth/authorize");
  Alcotest.(check bool)
    "redirect in url" true
    (contains r.authorization_url ("redirect_uri=" ^ Uri.pct_encode registered)
    || contains r.authorization_url registered);
  Alcotest.(check bool)
    "S256 method in url" true
    (contains r.authorization_url "code_challenge_method=S256");
  Alcotest.(check bool)
    "challenge in url" true
    (contains r.authorization_url r.material.code_challenge);
  Alcotest.(check bool)
    "no plain method" false
    (contains
       (String.lowercase_ascii r.authorization_url)
       "code_challenge_method=plain");
  (* Verifier is sealed — not stored as plaintext in protected row. *)
  let plaintext =
    assert_ok (Pkce.get_code_verifier ~store ~material:r.material)
  in
  Alcotest.(check bool) "verifier long" true (String.length plaintext >= 43);
  Alcotest.(check bool)
    "handle is not plaintext" true
    (r.material.code_verifier_handle <> plaintext);
  Alcotest.(check bool)
    "handle resolves via store" true
    (Hashtbl.mem table r.material.code_verifier_handle);
  (* Challenge matches sealed verifier. *)
  Alcotest.(check string)
    "challenge matches verifier"
    (Pkce.code_challenge_s256 ~code_verifier:plaintext)
    r.material.code_challenge;
  (* Protected row round-trip. *)
  match Pkce.load_protected ~db ~tx_id:r.tx.id with
  | Ok (Some m) ->
      Alcotest.(check string)
        "loaded handle" r.material.code_verifier_handle m.code_verifier_handle;
      Alcotest.(check string) "loaded redirect" registered m.redirect_uri
  | Ok None -> Alcotest.fail "missing protected material"
  | Error e -> Alcotest.fail e

let test_reject_state_reuse () =
  with_db @@ fun db ->
  let store, _ = fresh_store () in
  let state = "state_reuse_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" in
  let first =
    assert_ok
      (Pkce.start ~db ~store ~principal_id ~connector_actor:actor ~source:room
         ~app ~client_id ~registered_redirect_uri:registered ~base_revision
         ~continuation_handle:continuation ~now:fixed_now ~id:"tx_first"
         ~one_time_state:state ())
  in
  Alcotest.(check string) "first state" state first.tx.one_time_state;
  (* Reuse same state on a new start (different id) must fail. *)
  match
    Pkce.start ~db ~store ~principal_id ~connector_actor:actor ~source:room ~app
      ~client_id ~registered_redirect_uri:registered ~base_revision
      ~continuation_handle:continuation ~now:(fixed_now +. 1.) ~id:"tx_second"
      ~one_time_state:state ()
  with
  | Error msg ->
      Alcotest.(check bool)
        "reuse refused" true
        (contains (String.lowercase_ascii msg) "reuse"
        || contains (String.lowercase_ascii msg) "one-time"
        || contains (String.lowercase_ascii msg) "already")
  | Ok _ -> Alcotest.fail "state reuse must be rejected"

(* -------------------------------------------------------------------------- *)
(* Room secrecy                                                               *)
(* -------------------------------------------------------------------------- *)

let test_room_summary_never_includes_secrets () =
  start_with_store @@ fun _db store _table result ->
  let r = assert_ok result in
  let verifier =
    assert_ok (Pkce.get_code_verifier ~store ~material:r.material)
  in
  let room = Pkce.room_summary r in
  let redacted = Pkce.redacted_summary r in
  Alcotest.(check bool)
    "room safe heuristic" true
    (Pkce.room_output_is_safe room);
  Alcotest.(check bool) "room no url" false (contains room r.authorization_url);
  Alcotest.(check bool) "room no verifier" false (contains room verifier);
  Alcotest.(check bool)
    "room no challenge" false
    (contains room r.material.code_challenge);
  Alcotest.(check bool)
    "room no state" false
    (contains room r.material.one_time_state);
  Alcotest.(check bool)
    "room no client_id plaintext" false (contains room client_id);
  Alcotest.(check bool)
    "redacted no verifier" false
    (contains redacted verifier);
  Alcotest.(check bool)
    "redacted no full url" false
    (contains redacted r.authorization_url);
  Alcotest.(check bool)
    "contains_pkce_secrets detects url" true
    (Pkce.contains_pkce_secrets r r.authorization_url)

let test_private_delivery_companion_room_safe () =
  start_with_store @@ fun _db store _table result ->
  let r = assert_ok result in
  let verifier =
    assert_ok (Pkce.get_code_verifier ~store ~material:r.material)
  in
  let channel =
    assert_ok
      (D.make_private_connector_dm ~connector:P.Teams ~handle_id:"dm:alice")
  in
  let plan = Pkce.plan_private_delivery ~result:r ~channel () in
  match plan with
  | D.Private { private_delivery; companion_room } -> (
      Alcotest.(check bool)
        "url in private body" true
        (contains private_delivery.rendered r.authorization_url);
      match companion_room with
      | None -> Alcotest.fail "expected companion room progress"
      | Some rb ->
          Alcotest.(check string) "room id" "room-teams-1" rb.room_id;
          Alcotest.(check bool)
            "companion safe" true
            (D.room_message_is_safe rb.rendered);
          Alcotest.(check bool)
            "companion no verifier" false
            (contains rb.rendered verifier);
          Alcotest.(check bool)
            "companion no authorize url" false
            (contains rb.rendered "login/oauth/authorize");
          Alcotest.(check bool)
            "room_output_is_safe" true
            (Pkce.room_output_is_safe rb.rendered))
  | D.Room_progress _ -> Alcotest.fail "auth URL must not be room-only"
  | D.Refused e -> Alcotest.fail ("unexpected refuse: " ^ e.message)

let test_refuse_room_when_no_private_channel () =
  start_with_store @@ fun _db _store _table result ->
  let r = assert_ok result in
  let plan = Pkce.plan_private_delivery ~result:r ~channel:D.Absent () in
  match plan with
  | D.Refused e -> (
      Alcotest.(check bool)
        "no private channel" true
        (match e.reason with D.No_private_channel -> true | _ -> false);
      match e.room_safe_progress with
      | None -> Alcotest.fail "expected room-safe refuse progress"
      | Some p ->
          let rendered = D.render_room_progress p in
          Alcotest.(check bool)
            "refuse progress safe" true
            (Pkce.room_output_is_safe rendered);
          Alcotest.(check bool)
            "no url leaked" false
            (contains rendered r.authorization_url))
  | _ -> Alcotest.fail "absent private channel must refuse material"

let test_independent_state_verifier_on_start () =
  with_db @@ fun db ->
  let store, _ = fresh_store () in
  let a =
    assert_ok
      (Pkce.start ~db ~store ~principal_id ~connector_actor:actor ~source:room
         ~app ~client_id ~registered_redirect_uri:registered ~base_revision
         ~continuation_handle:continuation ~now:fixed_now ~id:"tx_a" ())
  in
  (* Supersede with a new open tx for same principal/source. *)
  let b =
    assert_ok
      (Pkce.start ~db ~store ~principal_id ~connector_actor:actor ~source:room
         ~app ~client_id ~registered_redirect_uri:registered ~base_revision
         ~continuation_handle:continuation ~now:(fixed_now +. 10.) ~id:"tx_b" ())
  in
  Alcotest.(check bool)
    "states independent" true
    (a.tx.one_time_state <> b.tx.one_time_state);
  let va = assert_ok (Pkce.get_code_verifier ~store ~material:a.material) in
  let vb = assert_ok (Pkce.get_code_verifier ~store ~material:b.material) in
  Alcotest.(check bool) "verifiers independent" true (va <> vb);
  Alcotest.(check bool)
    "challenges independent" true
    (a.material.code_challenge <> b.material.code_challenge)

let suite =
  [
    ( "independent high-entropy state and verifier",
      `Quick,
      test_independent_high_entropy_state_and_verifier );
    ("S256 challenge RFC vector", `Quick, test_s256_challenge);
    ("reject plain and none method", `Quick, test_reject_plain_and_none_method);
    ("start rejects plain method", `Quick, test_start_rejects_plain_method);
    ("reject unregistered redirect", `Quick, test_reject_unregistered_redirect);
    ("reject mutated redirect", `Quick, test_reject_mutated_redirect);
    ("reject http registered", `Quick, test_reject_http_registered);
    ( "start builds S256 URL and protects verifier",
      `Quick,
      test_start_builds_s256_url_and_protects_verifier );
    ("reject state reuse", `Quick, test_reject_state_reuse);
    ( "room summary never includes secrets",
      `Quick,
      test_room_summary_never_includes_secrets );
    ( "private delivery companion room safe",
      `Quick,
      test_private_delivery_companion_room_safe );
    ( "refuse room when no private channel",
      `Quick,
      test_refuse_room_when_no_private_channel );
    ( "independent state verifier on successive starts",
      `Quick,
      test_independent_state_verifier_on_start );
  ]

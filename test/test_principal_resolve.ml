(** Tests for Principal resolution of adapter-verified Connector actors
    (P21.M1.E1.T003). *)

module P = Principal_identity
module S = Principal_identity_store
module R = Principal_resolve
module B = Principal_bootstrap

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let sample_key ?(connector = P.Teams) ?(tenant = "tenant-acme")
    ?(user = "user-42") () =
  assert_ok
    (P.make_connector_actor_key ~connector ~tenant_or_workspace:tenant
       ~immutable_user_id:user)

let sample_display =
  P.
    {
      display_name = Some "Ada Lovelace";
      avatar_url = None;
      email = None;
      extra = [];
    }

(* -------------------------------------------------------------------------- *)
(* first-seen / second-seen / tenants                                         *)
(* -------------------------------------------------------------------------- *)

let test_first_seen_creates_principal () =
  with_db @@ fun db ->
  let key = sample_key () in
  let pid =
    assert_ok
      (R.resolve_or_create ~db ~actor_key:key ~display:sample_display
         ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "non-empty principal id" true
    (String.length (P.principal_id_to_string pid) > 0);
  match S.get_connector_actor ~db ~key with
  | Error e -> Alcotest.fail e
  | Ok None -> Alcotest.fail "actor not created"
  | Ok (Some actor) -> (
      Alcotest.(check string)
        "actor owns principal"
        (P.principal_id_to_string pid)
        (P.principal_id_to_string actor.principal_id);
      (match S.get_active_identity_link ~db ~key with
      | Error e -> Alcotest.fail e
      | Ok None -> Alcotest.fail "no active link"
      | Ok (Some link) ->
          Alcotest.(check string)
            "link principal"
            (P.principal_id_to_string pid)
            (P.principal_id_to_string link.principal_id));
      match S.get_principal ~db ~id:pid with
      | Error e -> Alcotest.fail e
      | Ok None -> Alcotest.fail "principal missing"
      | Ok (Some p) ->
          Alcotest.(check bool) "active" true (P.principal_is_active p);
          Alcotest.(check (option string))
            "display" (Some "Ada Lovelace") p.display.display_name)

let test_second_seen_same_principal () =
  with_db @@ fun db ->
  let key = sample_key () in
  let first =
    assert_ok
      (R.resolve_or_create ~db ~actor_key:key ~display:sample_display
         ~now:fixed_now ())
  in
  let renamed =
    { sample_display with display_name = Some "A. Lovelace (renamed)" }
  in
  let second =
    assert_ok
      (R.resolve_or_create ~db ~actor_key:key ~display:renamed
         ~now:(fixed_now +. 10.) ())
  in
  Alcotest.(check string)
    "stable principal across rename"
    (P.principal_id_to_string first)
    (P.principal_id_to_string second);
  (* Identity key unchanged; only one actor row. *)
  match S.get_connector_actor ~db ~key with
  | Error e -> Alcotest.fail e
  | Ok None -> Alcotest.fail "missing actor"
  | Ok (Some actor) ->
      Alcotest.(check string)
        "same owner"
        (P.principal_id_to_string first)
        (P.principal_id_to_string actor.principal_id)

let test_different_tenants_different_principals () =
  with_db @@ fun db ->
  let k1 = sample_key ~tenant:"tenant-a" ~user:"U1" () in
  let k2 = sample_key ~tenant:"tenant-b" ~user:"U1" () in
  let p1 =
    assert_ok (R.resolve_or_create ~db ~actor_key:k1 ~now:fixed_now ())
  in
  let p2 =
    assert_ok (R.resolve_or_create ~db ~actor_key:k2 ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check bool) "distinct principals" false (P.principal_id_equal p1 p2);
  Alcotest.(check bool)
    "distinct actor keys" false
    (P.connector_actor_key_equal k1 k2)

let test_different_connectors_different_principals () =
  with_db @@ fun db ->
  let k_teams =
    sample_key ~connector:P.Teams ~tenant:"ws" ~user:"same-external-id" ()
  in
  let k_slack =
    sample_key ~connector:P.Slack ~tenant:"ws" ~user:"same-external-id" ()
  in
  let p1 =
    assert_ok (R.resolve_or_create ~db ~actor_key:k_teams ~now:fixed_now ())
  in
  let p2 =
    assert_ok
      (R.resolve_or_create ~db ~actor_key:k_slack ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check bool)
    "cross-connector not auto-linked" false
    (P.principal_id_equal p1 p2)

(* -------------------------------------------------------------------------- *)
(* bootstrap wiring                                                           *)
(* -------------------------------------------------------------------------- *)

let test_bootstrap_direct_session_rejected () =
  with_db @@ fun db ->
  match
    R.resolve_bootstrap ~db
      ~provenance:(B.Direct_session { session_key = "sess_local" })
      ~now:fixed_now ()
  with
  | R.Principal _ -> Alcotest.fail "direct session must not resolve"
  | R.Rejected { reason } ->
      let r = String.lowercase_ascii reason in
      Alcotest.(check bool)
        "mentions session/direct" true
        (Test_helpers.string_contains r "session"
        || Test_helpers.string_contains r "direct")

let test_bootstrap_unverified_web_rejected_without_persistence () =
  with_db @@ fun db ->
  let provenance =
    B.Web_oidc
      {
        issuer = "https://issuer.example/";
        subject = "sub_alice_01";
        exp = fixed_now +. 3600.;
      }
  in
  match
    R.resolve_bootstrap ~db ~provenance ~display:sample_display ~now:fixed_now
      ()
  with
  | R.Principal _ -> Alcotest.fail "raw web claims must not resolve"
  | R.Rejected { reason } -> (
      let r = String.lowercase_ascii reason in
      Alcotest.(check bool)
        "reason identifies missing verification" true
        (Test_helpers.string_contains r "jwt"
        && Test_helpers.string_contains r "jwks");
      let key =
        assert_ok
          (P.make_connector_actor_key ~connector:P.Web
             ~tenant_or_workspace:"https://issuer.example/"
             ~immutable_user_id:"sub_alice_01")
      in
      match S.get_connector_actor ~db ~key with
      | Ok None -> ()
      | Ok (Some _) -> Alcotest.fail "raw web claims created an actor"
      | Error e -> Alcotest.fail e)

let test_bootstrap_expired_web_rejected () =
  with_db @@ fun db ->
  match
    R.resolve_bootstrap ~db
      ~provenance:
        (B.Web_oidc
           {
             issuer = "https://issuer.example/";
             subject = "sub_alice_01";
             exp = fixed_now -. 1.;
           })
      ~now:fixed_now ()
  with
  | R.Principal _ -> Alcotest.fail "expired web must reject"
  | R.Rejected _ -> ()

let test_bootstrap_cli_enrolled () =
  with_db @@ fun db ->
  let enrolled_pid = assert_ok (P.principal_id_of_string "prin_device_owner") in
  let enrolled ~device_id =
    if String.equal device_id "dev_trusted_1" then Some enrolled_pid else None
  in
  match
    R.resolve_bootstrap ~db
      ~provenance:
        (B.Cli_enrolled
           {
             device_id = "dev_trusted_1";
             principal_id = "prin_device_owner";
             exp = fixed_now +. 86_400.;
           })
      ~now:fixed_now ~enrolled ()
  with
  | R.Principal id ->
      Alcotest.(check string)
        "enrolled principal" "prin_device_owner"
        (P.principal_id_to_string id)
  | R.Rejected { reason } -> Alcotest.failf "cli enrolled rejected: %s" reason

let test_bootstrap_cli_revoked_rejected () =
  with_db @@ fun db ->
  let enrolled ~device_id:_ = None in
  match
    R.resolve_bootstrap ~db
      ~provenance:
        (B.Cli_enrolled
           {
             device_id = "dev_revoked";
             principal_id = "prin_anyone";
             exp = fixed_now +. 86_400.;
           })
      ~now:fixed_now ~enrolled ()
  with
  | R.Principal _ -> Alcotest.fail "revoked cli must reject"
  | R.Rejected _ -> ()

let test_of_bootstrap_maps_anonymous () =
  match R.of_bootstrap (B.Anonymous { reason = "nope" }) with
  | R.Rejected { reason } -> Alcotest.(check string) "reason" "nope" reason
  | R.Principal _ -> Alcotest.fail "expected Rejected"

let test_display_of_name () =
  let d = R.display_of_name (Some "  Bob  ") in
  Alcotest.(check (option string)) "name" (Some "Bob") d.display_name;
  let empty = R.display_of_name None in
  Alcotest.(check (option string)) "empty" None empty.display_name

let suite =
  [
    ("first-seen creates principal", `Quick, test_first_seen_creates_principal);
    ("second-seen same principal", `Quick, test_second_seen_same_principal);
    ( "different tenants different principals",
      `Quick,
      test_different_tenants_different_principals );
    ( "different connectors different principals",
      `Quick,
      test_different_connectors_different_principals );
    ( "bootstrap direct session rejected",
      `Quick,
      test_bootstrap_direct_session_rejected );
    ( "bootstrap unverified web rejected without persistence",
      `Quick,
      test_bootstrap_unverified_web_rejected_without_persistence );
    ( "bootstrap expired web rejected",
      `Quick,
      test_bootstrap_expired_web_rejected );
    ("bootstrap cli enrolled", `Quick, test_bootstrap_cli_enrolled);
    ( "bootstrap cli revoked rejected",
      `Quick,
      test_bootstrap_cli_revoked_rejected );
    ("of_bootstrap maps anonymous", `Quick, test_of_bootstrap_maps_anonymous);
    ("display_of_name", `Quick, test_display_of_name);
  ]

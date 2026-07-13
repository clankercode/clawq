(** Tests for shared verified pending-credential activation (P21.M2.E2.T004). *)

module A = Github_user_auth_activate
module Tx = Github_user_auth_tx
module V = Github_user_token_vault
module B = Github_account_binding
module P = Principal_identity
module PS = Principal_identity_store

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-activate-master" ()

let fixed_now = 1_700_000_000.0
let principal_id = "principal:alice"
let base_revision = "rev-policy-1"
let continuation = "cont:dm:handle-1"
let access_token = "ghu_access_ACTIVATE_PLAINTEXT_x"
let refresh_token = "ghr_refresh_ACTIVATE_PLAINTEXT_y"
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

let assert_prep = function
  | Ok v -> v
  | Error (e : A.failure) ->
      Alcotest.fail
        (Printf.sprintf "%s [%s]" e.message (A.string_of_failure_kind e.kind))

let assert_act = function
  | Ok v -> v
  | Error (e : A.failure) ->
      Alcotest.fail
        (Printf.sprintf "%s [%s]" e.message (A.string_of_failure_kind e.kind))

let contains hay needle = Test_helpers.string_contains hay needle

let make_keys () =
  assert_ok
    (V.make_single_key_provider ~key_id:"mk-act-1" ~key_version:1 ~aes_key ())

let seed_principal ~db ?(revision = 1) () =
  let pid = assert_ok (P.principal_id_of_string principal_id) in
  let p =
    P.make_principal ~id:pid ~revision ~created_at:"2026-01-01T00:00:00Z"
      ~updated_at:"2026-01-01T00:00:00Z" ()
  in
  assert_ok (PS.insert_principal ~db ~now:fixed_now p)

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  A.ensure_schema db;
  ignore (seed_principal ~db ());
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let make_completed_web_tx ~db ?(id = "tx_web_1") ?(now = fixed_now) () =
  let open_tx =
    assert_ok
      (Tx.create ~db ~flow_kind:Tx.Web_pkce ~principal_id ~connector_actor:actor
         ~source:room ~app ~base_revision ~continuation_handle:continuation ~now
         ~id ~ttl_seconds:3600. ())
  in
  let context =
    {
      Tx.principal_id;
      connector_actor = actor;
      source = room;
      app_id = app.app_id;
      base_revision;
    }
  in
  assert_ok
    (Tx.complete ~db ~id:open_tx.Tx.id ~context
       ~one_time_state:open_tx.Tx.one_time_state ~now ())

let make_open_device_tx ~db ?(id = "tx_dev_1") ?(now = fixed_now) () =
  assert_ok
    (Tx.create ~db ~flow_kind:Tx.Device ~principal_id ~connector_actor:actor
       ~source:room ~app ~base_revision ~continuation_handle:continuation ~now
       ~id ~ttl_seconds:3600. ())

let sample_credential ?(access = access_token) ?(expires_in = 28800) () =
  assert_ok
    (A.make_pending_credential ~access_token:access ~refresh_token
       ~scopes:[ "repo"; "read:user" ] ~expires_in ~token_type:"bearer" ())

let fetch_user ~access_token:tok =
  if tok <> access_token then Error "unexpected access token in fetch_user"
  else
    Ok
      {
        A.id = github_user_id;
        login = github_login;
        avatar_url = Some "https://avatars.example/o.png";
      }

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

let count_bindings ~db =
  match
    B.list_for_principal ~db
      ~principal_id:(assert_ok (P.principal_id_of_string principal_id))
  with
  | Ok xs -> List.length xs
  | Error e -> Alcotest.fail e

(* -------------------------------------------------------------------------- *)
(* Credential shape                                                           *)
(* -------------------------------------------------------------------------- *)

let test_credential_shape () =
  (match A.make_pending_credential ~access_token:"" ~expires_in:10 () with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "empty access must fail");
  (match A.make_pending_credential ~access_token:"x" ~expires_in:0 () with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expires_in 0 must fail");
  let c = sample_credential () in
  Alcotest.(check string) "access" access_token c.access_token;
  Alcotest.(check int) "exp" 28800 c.expires_in;
  (* Device grants must supply positive expires_in via make_pending_credential
     (projection lives in device_poll; activate stays flow-neutral). *)
  match A.make_pending_credential ~access_token ~expires_in:0 () with
  | Error msg ->
      Alcotest.(check bool)
        "requires positive expires" true
        (contains (String.lowercase_ascii msg) "expires_in")
  | Ok _ -> Alcotest.fail "expires_in 0 must fail"

(* -------------------------------------------------------------------------- *)
(* Happy path: web + device                                                   *)
(* -------------------------------------------------------------------------- *)

let test_web_prepare_confirm_activates () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let tx = make_completed_web_tx ~db ~id:"tx_web_happy" () in
  let prep =
    assert_prep
      (A.prepare ~db ~keys ~fetch_user ~auth_tx_id:tx.Tx.id
         ~credential:(sample_credential ()) ~now:fixed_now
         ~activation_id:"act_happy" ~vault_id:"vault_happy"
         ~binding_id:"bind_happy" ~plan_id:"plan_happy" ())
  in
  Alcotest.(check string)
    "pending status" "pending_confirmation"
    (A.string_of_activation_status prep.activation.status);
  Alcotest.(check string)
    "binding pending" "pending"
    (B.string_of_authorization_status prep.binding.authorization_status);
  Alcotest.(check bool)
    "not active" false
    (A.has_active_binding ~binding:prep.binding);
  Alcotest.(check int) "no authorized yet" 0 (count_authorized ~db);
  Alcotest.(check string) "vault" "vault_happy" prep.vault.id;
  Alcotest.(check int) "gen 1" 1 prep.vault.generation;
  Alcotest.(check bool)
    "plan digest non-empty" true
    (String.length prep.plan.digest > 10);
  Alcotest.(check string)
    "digest match" prep.plan.digest prep.activation.plan_digest;
  (* Tokens sealed, not in plan/summary. *)
  (match V.read ~db ~keys ~id:prep.vault.id () with
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok opened ->
      Alcotest.(check string)
        "access sealed" access_token opened.tokens.access_token);
  let summary = A.redacted_prepared_summary prep in
  Alcotest.(check bool)
    "summary no access" false
    (contains summary access_token);
  Alcotest.(check bool)
    "summary no confirm" false
    (contains summary prep.confirmation_token);
  let plan_json = Yojson.Safe.to_string (A.redacted_plan_to_json prep.plan) in
  Alcotest.(check bool) "plan no access" false (contains plan_json access_token);
  let activated =
    assert_act
      (A.confirm ~db ~keys ~activation_id:prep.activation.id
         ~confirmation_token:prep.confirmation_token
         ~expected_principal_id:principal_id
         ~expected_plan_digest:prep.plan.digest ~now:(fixed_now +. 10.) ())
  in
  Alcotest.(check string)
    "activated" "activated"
    (A.string_of_activation_status activated.activation.status);
  Alcotest.(check string)
    "binding authorized" "authorized"
    (B.string_of_authorization_status activated.binding.authorization_status);
  Alcotest.(check bool)
    "active binding" true
    (A.has_active_binding ~binding:activated.binding);
  Alcotest.(check int) "one authorized" 1 (count_authorized ~db);
  (* Replay confirm refused. *)
  match
    A.confirm ~db ~keys ~activation_id:prep.activation.id
      ~confirmation_token:prep.confirmation_token ~now:(fixed_now +. 20.) ()
  with
  | Ok _ -> Alcotest.fail "replay confirm must fail"
  | Error e ->
      Alcotest.(check string)
        "already activated" "already_activated"
        (A.string_of_failure_kind e.kind)

let test_device_prepare_confirm () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let tx = make_open_device_tx ~db ~id:"tx_dev_happy" () in
  let prep =
    assert_prep
      (A.prepare ~db ~keys ~fetch_user ~auth_tx_id:tx.Tx.id
         ~credential:(sample_credential ()) ~now:fixed_now
         ~activation_id:"act_dev" ~vault_id:"vault_dev" ~binding_id:"bind_dev"
         ())
  in
  Alcotest.(check string)
    "flow device" "device"
    (Tx.string_of_flow_kind prep.activation.flow_kind);
  let activated =
    assert_act
      (A.confirm ~db ~keys ~activation_id:prep.activation.id
         ~confirmation_token:prep.confirmation_token ~now:(fixed_now +. 5.) ())
  in
  Alcotest.(check bool)
    "authorized" true
    (A.has_active_binding ~binding:activated.binding)

(* -------------------------------------------------------------------------- *)
(* Fail-closed paths                                                          *)
(* -------------------------------------------------------------------------- *)

let test_web_open_refuses_incomplete () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let open_tx =
    assert_ok
      (Tx.create ~db ~flow_kind:Tx.Web_pkce ~principal_id ~connector_actor:actor
         ~source:room ~app ~base_revision ~continuation_handle:continuation
         ~now:fixed_now ~id:"tx_web_open" ~ttl_seconds:3600. ())
  in
  match
    A.prepare ~db ~keys ~fetch_user ~auth_tx_id:open_tx.Tx.id
      ~credential:(sample_credential ()) ~now:fixed_now ()
  with
  | Ok _ -> Alcotest.fail "open web must refuse"
  | Error e ->
      Alcotest.(check string)
        "incomplete" "incomplete_exchange"
        (A.string_of_failure_kind e.kind);
      Alcotest.(check int) "no bindings" 0 (count_bindings ~db)

let test_replay_prepare_same_auth_tx () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let tx = make_completed_web_tx ~db ~id:"tx_replay" () in
  ignore
    (assert_prep
       (A.prepare ~db ~keys ~fetch_user ~auth_tx_id:tx.Tx.id
          ~credential:(sample_credential ()) ~now:fixed_now
          ~activation_id:"act1" ~vault_id:"v1" ~binding_id:"b1" ()));
  match
    A.prepare ~db ~keys ~fetch_user ~auth_tx_id:tx.Tx.id
      ~credential:(sample_credential ()) ~now:fixed_now ~activation_id:"act2"
      ~vault_id:"v2" ~binding_id:"b2" ()
  with
  | Ok _ -> Alcotest.fail "replay prepare must fail"
  | Error e ->
      Alcotest.(check string)
        "replay" "replay"
        (A.string_of_failure_kind e.kind)

let test_confirmation_mismatch_destroys_pending () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let tx = make_completed_web_tx ~db ~id:"tx_mm" () in
  let prep =
    assert_prep
      (A.prepare ~db ~keys ~fetch_user ~auth_tx_id:tx.Tx.id
         ~credential:(sample_credential ()) ~now:fixed_now
         ~activation_id:"act_mm" ~vault_id:"vault_mm" ~binding_id:"bind_mm" ())
  in
  match
    A.confirm ~db ~keys ~activation_id:prep.activation.id
      ~confirmation_token:"totally_wrong_confirmation_token_value"
      ~now:(fixed_now +. 1.) ()
  with
  | Ok _ -> Alcotest.fail "wrong token must fail"
  | Error e -> (
      Alcotest.(check string)
        "mismatch" "confirmation_mismatch"
        (A.string_of_failure_kind e.kind);
      Alcotest.(check int) "no authorized" 0 (count_authorized ~db);
      (match V.get_meta ~db ~id:"vault_mm" with
      | Ok None -> ()
      | Ok (Some _) -> Alcotest.fail "pending vault must be destroyed"
      | Error d -> Alcotest.fail (V.string_of_denial d));
      (match B.get ~db ~id:"bind_mm" with
      | Ok None -> ()
      | Ok (Some b) ->
          Alcotest.fail
            ("pending binding should be deleted, got "
            ^ B.string_of_authorization_status b.authorization_status)
      | Error err -> Alcotest.fail err);
      match e.activation with
      | Some a ->
          Alcotest.(check string)
            "rejected" "rejected"
            (A.string_of_activation_status a.status)
      | None -> Alcotest.fail "expected activation on failure")

let test_collision_preserves_prior_authorized () =
  with_db @@ fun db ->
  let keys = make_keys () in
  (* Seed prior Authorized binding + vault for same identity. *)
  let account =
    assert_ok
      (V.make_account_key ~principal_id ~github_user_id ~app_id:42
         ~host:"github.com" ())
  in
  let prior_tokens : Github_user_token_store.plaintext_tokens =
    {
      access_token = "ghu_PRIOR_AUTHORIZED_TOKEN";
      refresh_token = Some "ghr_PRIOR";
    }
  in
  let prior_vault =
    match
      V.create ~db ~keys ~id:"vault_prior" ~now:fixed_now ~account
        ~tokens:prior_tokens ~scopes:[ "repo" ]
        ~expires_at:"2026-12-01T00:00:00Z" ()
    with
    | Ok v -> v
    | Error d -> Alcotest.fail (V.string_of_denial d)
  in
  let identity =
    assert_ok
      (B.make_account_identity ~host:"github.com" ~app_id:42 ~github_user_id ())
  in
  let prior_bind =
    B.make_binding ~id:"bind_prior"
      ~principal_id:(assert_ok (P.principal_id_of_string principal_id))
      ~identity
      ~display:{ B.login = Some github_login; avatar_url = None }
      ~authorization_status:B.Authorized
      ~vault_ref:(assert_ok (B.make_vault_ref prior_vault.id))
      ()
  in
  ignore (assert_ok (B.insert ~db ~now:fixed_now prior_bind));
  Alcotest.(check int) "prior authorized" 1 (count_authorized ~db);
  let tx = make_completed_web_tx ~db ~id:"tx_collision" () in
  match
    A.prepare ~db ~keys ~fetch_user ~auth_tx_id:tx.Tx.id
      ~credential:(sample_credential ()) ~now:fixed_now ~vault_id:"vault_new"
      ~binding_id:"bind_new" ()
  with
  | Ok _ -> Alcotest.fail "collision must refuse"
  | Error e -> (
      Alcotest.(check string)
        "collision" "collision"
        (A.string_of_failure_kind e.kind);
      Alcotest.(check int) "still one authorized" 1 (count_authorized ~db);
      Alcotest.(check int) "still one binding" 1 (count_bindings ~db);
      (* Prior vault intact with prior token. *)
      match V.read ~db ~keys ~id:"vault_prior" () with
      | Error d -> Alcotest.fail (V.string_of_denial d)
      | Ok opened -> (
          Alcotest.(check string)
            "prior token preserved" "ghu_PRIOR_AUTHORIZED_TOKEN"
            opened.tokens.access_token;
          match V.get_meta ~db ~id:"vault_new" with
          | Ok None -> ()
          | Ok (Some _) -> Alcotest.fail "new vault must not exist"
          | Error d -> Alcotest.fail (V.string_of_denial d)))

let test_identity_pin_mismatch () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let open_tx =
    assert_ok
      (Tx.create ~db ~flow_kind:Tx.Web_pkce ~principal_id ~connector_actor:actor
         ~source:room ~app
         ~intended_account:{ Tx.github_user_id = Some 999L; login_hint = None }
         ~base_revision ~continuation_handle:continuation ~now:fixed_now
         ~id:"tx_pin" ~ttl_seconds:3600. ())
  in
  let context =
    {
      Tx.principal_id;
      connector_actor = actor;
      source = room;
      app_id = app.app_id;
      base_revision;
    }
  in
  let tx =
    assert_ok
      (Tx.complete ~db ~id:open_tx.Tx.id ~context
         ~one_time_state:open_tx.Tx.one_time_state ~now:fixed_now ())
  in
  match
    A.prepare ~db ~keys ~fetch_user ~auth_tx_id:tx.Tx.id
      ~credential:(sample_credential ()) ~now:fixed_now ()
  with
  | Ok _ -> Alcotest.fail "pin mismatch must fail"
  | Error e ->
      Alcotest.(check string)
        "identity mismatch" "identity_mismatch"
        (A.string_of_failure_kind e.kind);
      Alcotest.(check int) "no bindings" 0 (count_bindings ~db)

let test_expiry_destroys_pending () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let tx = make_completed_web_tx ~db ~id:"tx_exp" () in
  let prep =
    assert_prep
      (A.prepare ~db ~keys ~fetch_user ~auth_tx_id:tx.Tx.id
         ~credential:(sample_credential ()) ~now:fixed_now ~ttl_seconds:60.
         ~activation_id:"act_exp" ~vault_id:"vault_exp" ~binding_id:"bind_exp"
         ())
  in
  match
    A.confirm ~db ~keys ~activation_id:prep.activation.id
      ~confirmation_token:prep.confirmation_token ~now:(fixed_now +. 120.) ()
  with
  | Ok _ -> Alcotest.fail "expired must fail"
  | Error e -> (
      Alcotest.(check string)
        "expired" "expired"
        (A.string_of_failure_kind e.kind);
      Alcotest.(check int) "no authorized" 0 (count_authorized ~db);
      match V.get_meta ~db ~id:"vault_exp" with
      | Ok None -> ()
      | Ok (Some _) -> Alcotest.fail "expired vault must be destroyed"
      | Error d -> Alcotest.fail (V.string_of_denial d))

let test_destroy_pending_preserves_nothing_extra () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let tx = make_completed_web_tx ~db ~id:"tx_des" () in
  let prep =
    assert_prep
      (A.prepare ~db ~keys ~fetch_user ~auth_tx_id:tx.Tx.id
         ~credential:(sample_credential ()) ~now:fixed_now
         ~activation_id:"act_des" ~vault_id:"vault_des" ~binding_id:"bind_des"
         ())
  in
  let destroyed =
    assert_ok
      (match
         A.destroy ~db ~keys ~activation_id:prep.activation.id
           ~reason:"user_cancelled" ~now:(fixed_now +. 1.) ()
       with
      | Ok a -> Ok a
      | Error e -> Error e.message)
  in
  Alcotest.(check string)
    "destroyed" "destroyed"
    (A.string_of_activation_status destroyed.status);
  Alcotest.(check int) "no authorized" 0 (count_authorized ~db);
  match V.get_meta ~db ~id:"vault_des" with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "destroyed vault must be gone"
  | Error d -> Alcotest.fail (V.string_of_denial d)

let test_principal_revision_change_at_confirm () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let tx = make_completed_web_tx ~db ~id:"tx_prev" () in
  let prep =
    assert_prep
      (A.prepare ~db ~keys ~fetch_user ~auth_tx_id:tx.Tx.id
         ~credential:(sample_credential ()) ~now:fixed_now
         ~activation_id:"act_prev" ~vault_id:"vault_prev"
         ~binding_id:"bind_prev" ())
  in
  (* Bump Principal revision so confirm CAS fails. *)
  let pid = assert_ok (P.principal_id_of_string principal_id) in
  ignore
    (assert_ok
       (PS.update_principal ~db ~id:pid ~expected_revision:1
          ~now:(fixed_now +. 1.) ()));
  match
    A.confirm ~db ~keys ~activation_id:prep.activation.id
      ~confirmation_token:prep.confirmation_token ~now:(fixed_now +. 2.) ()
  with
  | Ok _ -> Alcotest.fail "stale principal must fail"
  | Error e -> (
      Alcotest.(check string)
        "principal changed" "principal_changed"
        (A.string_of_failure_kind e.kind);
      Alcotest.(check int) "no authorized" 0 (count_authorized ~db);
      match V.get_meta ~db ~id:"vault_prev" with
      | Ok None -> ()
      | Ok (Some _) ->
          Alcotest.fail "pending vault destroyed on principal change"
      | Error d -> Alcotest.fail (V.string_of_denial d))

let test_user_probe_failure_no_seal () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let tx = make_completed_web_tx ~db ~id:"tx_probe" () in
  let bad_fetch ~access_token:_ = Error "github 401" in
  match
    A.prepare ~db ~keys ~fetch_user:bad_fetch ~auth_tx_id:tx.Tx.id
      ~credential:(sample_credential ()) ~now:fixed_now ()
  with
  | Ok _ -> Alcotest.fail "probe fail must refuse"
  | Error e ->
      Alcotest.(check string)
        "user_probe" "user_probe"
        (A.string_of_failure_kind e.kind);
      Alcotest.(check int) "no bindings" 0 (count_bindings ~db)

let suite =
  [
    ("credential shape validation", `Quick, test_credential_shape);
    ("web prepare+confirm activates", `Quick, test_web_prepare_confirm_activates);
    ("device prepare+confirm", `Quick, test_device_prepare_confirm);
    ("web open refuses incomplete", `Quick, test_web_open_refuses_incomplete);
    ("replay prepare same auth tx", `Quick, test_replay_prepare_same_auth_tx);
    ( "confirmation mismatch destroys pending",
      `Quick,
      test_confirmation_mismatch_destroys_pending );
    ( "collision preserves prior authorized",
      `Quick,
      test_collision_preserves_prior_authorized );
    ("identity pin mismatch", `Quick, test_identity_pin_mismatch);
    ("expiry destroys pending", `Quick, test_expiry_destroys_pending);
    ("destroy pending", `Quick, test_destroy_pending_preserves_nothing_extra);
    ( "principal revision change at confirm",
      `Quick,
      test_principal_revision_change_at_confirm );
    ("user probe failure no seal", `Quick, test_user_probe_failure_no_seal);
  ]

(** Tests for Principal / Connector actor / Identity Link SQLite store
    (P21.M1.E1.T002). *)

module P = Principal_identity
module S = Principal_identity_store

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let sample_principal_id ?(s = "prin_01HZX9EXAMPLE000000000001") () =
  assert_ok (P.principal_id_of_string s)

let sample_key ?(connector = P.Teams) ?(tenant = "tenant-acme")
    ?(user = "user-42") () =
  assert_ok
    (P.make_connector_actor_key ~connector ~tenant_or_workspace:tenant
       ~immutable_user_id:user)

let sample_display =
  P.
    {
      display_name = Some "Ada Lovelace";
      avatar_url = Some "https://example.com/a.png";
      email = Some "ada@example.com";
      extra = [ ("title", "Engineer") ];
    }

let sample_principal ?(id = sample_principal_id ()) ?(revision = 1) () =
  P.make_principal ~id ~display:sample_display ~revision
    ~created_at:"2026-07-13T00:00:00Z" ~updated_at:"2026-07-13T00:00:01Z" ()

let sample_actor ?(key = sample_key ()) ?(principal_id = sample_principal_id ())
    ?(revision = 1) () =
  P.make_connector_actor ~key ~principal_id ~display:sample_display ~revision
    ~verified_at:"2026-07-13T00:00:00Z" ~created_at:"2026-07-13T00:00:00Z"
    ~updated_at:"2026-07-13T00:00:01Z" ()

let sample_link ?(id = "link_1") ?(key = sample_key ())
    ?(principal_id = sample_principal_id ()) ?(revision = 1) () =
  P.make_identity_link ~id ~principal_id ~actor_key:key ~revision
    ~linked_at:"2026-07-13T00:00:00Z" ()

(* -------------------------------------------------------------------------- *)
(* insert + get                                                               *)
(* -------------------------------------------------------------------------- *)

let test_insert_get_principal () =
  with_db @@ fun db ->
  let p = sample_principal () in
  let stored = assert_ok (S.insert_principal ~db ~now:fixed_now p) in
  Alcotest.(check string)
    "id"
    (P.principal_id_to_string p.id)
    (P.principal_id_to_string stored.id);
  Alcotest.(check int) "revision" 1 stored.revision;
  match S.get_principal ~db ~id:p.id with
  | Error e -> Alcotest.fail e
  | Ok None -> Alcotest.fail "missing principal"
  | Ok (Some got) ->
      Alcotest.(check string)
        "roundtrip id"
        (P.principal_id_to_string p.id)
        (P.principal_id_to_string got.id);
      Alcotest.(check int) "version" P.schema_version got.version;
      Alcotest.(check (option string))
        "display_name" (Some "Ada Lovelace") got.display.display_name;
      Alcotest.(check (option string))
        "email" (Some "ada@example.com") got.display.email;
      Alcotest.(check bool) "active" true (P.principal_is_active got)

let test_insert_get_connector_actor () =
  with_db @@ fun db ->
  let p = sample_principal () in
  ignore (assert_ok (S.insert_principal ~db ~now:fixed_now p));
  let a = sample_actor ~principal_id:p.id () in
  let stored = assert_ok (S.insert_connector_actor ~db ~now:fixed_now a) in
  Alcotest.(check string)
    "identity key"
    (P.actor_identity_key a.key)
    (P.actor_identity_key stored.key);
  match S.get_connector_actor ~db ~key:a.key with
  | Error e -> Alcotest.fail e
  | Ok None -> Alcotest.fail "missing actor"
  | Ok (Some got) ->
      Alcotest.(check string)
        "principal"
        (P.principal_id_to_string p.id)
        (P.principal_id_to_string got.principal_id);
      Alcotest.(check (option string))
        "verified_at" (Some "2026-07-13T00:00:00Z") got.verified_at;
      Alcotest.(check (option string))
        "display" (Some "Ada Lovelace") got.display.display_name

let test_insert_get_identity_link () =
  with_db @@ fun db ->
  let p = sample_principal () in
  ignore (assert_ok (S.insert_principal ~db ~now:fixed_now p));
  let key = sample_key () in
  ignore
    (assert_ok
       (S.insert_connector_actor ~db ~now:fixed_now
          (sample_actor ~key ~principal_id:p.id ())));
  let l = sample_link ~key ~principal_id:p.id () in
  let stored = assert_ok (S.insert_identity_link ~db ~now:fixed_now l) in
  Alcotest.(check string) "id" "link_1" stored.id;
  (match S.get_identity_link ~db ~id:stored.id with
  | Error e -> Alcotest.fail e
  | Ok None -> Alcotest.fail "missing link"
  | Ok (Some got) ->
      Alcotest.(check string)
        "actor key" (P.actor_identity_key key)
        (P.actor_identity_key got.actor_key);
      Alcotest.(check string)
        "status" "active"
        (P.string_of_identity_link_status got.status));
  match S.get_active_identity_link ~db ~key with
  | Error e -> Alcotest.fail e
  | Ok None -> Alcotest.fail "no active link"
  | Ok (Some got) -> Alcotest.(check string) "active id" "link_1" got.id

(* -------------------------------------------------------------------------- *)
(* collision reject                                                           *)
(* -------------------------------------------------------------------------- *)

let test_connector_actor_collision_reject () =
  with_db @@ fun db ->
  let p1 = sample_principal ~id:(sample_principal_id ~s:"prin_a" ()) () in
  let p2 = sample_principal ~id:(sample_principal_id ~s:"prin_b" ()) () in
  ignore (assert_ok (S.insert_principal ~db ~now:fixed_now p1));
  ignore (assert_ok (S.insert_principal ~db ~now:fixed_now p2));
  let key = sample_key () in
  ignore
    (assert_ok
       (S.insert_connector_actor ~db ~now:fixed_now
          (sample_actor ~key ~principal_id:p1.id ())));
  match
    S.insert_connector_actor ~db ~now:fixed_now
      (sample_actor ~key ~principal_id:p2.id ())
  with
  | Ok _ -> Alcotest.fail "expected collision reject on connector_actor_key"
  | Error msg ->
      let lower = String.lowercase_ascii msg in
      Alcotest.(check bool)
        "mentions collision/exists" true
        (Test_helpers.string_contains lower "collision"
        || Test_helpers.string_contains lower "already")

let test_active_identity_link_collision_reject () =
  with_db @@ fun db ->
  let p = sample_principal () in
  ignore (assert_ok (S.insert_principal ~db ~now:fixed_now p));
  let key = sample_key () in
  ignore
    (assert_ok
       (S.insert_connector_actor ~db ~now:fixed_now
          (sample_actor ~key ~principal_id:p.id ())));
  ignore
    (assert_ok
       (S.insert_identity_link ~db ~now:fixed_now
          (sample_link ~id:"link_a" ~key ~principal_id:p.id ())));
  match
    S.insert_identity_link ~db ~now:fixed_now
      (sample_link ~id:"link_b" ~key ~principal_id:p.id ())
  with
  | Ok _ -> Alcotest.fail "expected active identity_link collision"
  | Error msg ->
      Alcotest.(check bool)
        "collision" true
        (Test_helpers.string_contains (String.lowercase_ascii msg) "collision"
        || Test_helpers.string_contains (String.lowercase_ascii msg) "already")

let test_cross_tenant_same_user_no_collision () =
  with_db @@ fun db ->
  let p = sample_principal () in
  ignore (assert_ok (S.insert_principal ~db ~now:fixed_now p));
  let k1 = sample_key ~tenant:"tenant-a" ~user:"U1" () in
  let k2 = sample_key ~tenant:"tenant-b" ~user:"U1" () in
  ignore
    (assert_ok
       (S.insert_connector_actor ~db ~now:fixed_now
          (sample_actor ~key:k1 ~principal_id:p.id ())));
  let a2 =
    assert_ok
      (S.insert_connector_actor ~db ~now:fixed_now
         (sample_actor ~key:k2 ~principal_id:p.id ()))
  in
  Alcotest.(check string)
    "distinct keys" (P.actor_identity_key k2)
    (P.actor_identity_key a2.key)

let test_first_seen_collision_reject () =
  with_db @@ fun db ->
  let key = sample_key () in
  let p, a, l =
    assert_ok
      (S.create_first_seen ~db ~key ~display:sample_display ~now:fixed_now ())
  in
  Alcotest.(check bool) "principal active" true (P.principal_is_active p);
  Alcotest.(check string)
    "actor key" (P.actor_identity_key key)
    (P.actor_identity_key a.key);
  Alcotest.(check string)
    "link status" "active"
    (P.string_of_identity_link_status l.status);
  match S.create_first_seen ~db ~key ~now:(fixed_now +. 1.) () with
  | Ok _ -> Alcotest.fail "second first-seen must collide"
  | Error msg ->
      Alcotest.(check bool)
        "collision" true
        (Test_helpers.string_contains (String.lowercase_ascii msg) "collision"
        || Test_helpers.string_contains (String.lowercase_ascii msg) "already")

(* -------------------------------------------------------------------------- *)
(* revision bump / CAS                                                        *)
(* -------------------------------------------------------------------------- *)

let test_principal_revision_bump () =
  with_db @@ fun db ->
  let p = sample_principal () in
  ignore (assert_ok (S.insert_principal ~db ~now:fixed_now p));
  let updated =
    assert_ok
      (S.update_principal ~db ~id:p.id ~expected_revision:1
         ~display:{ sample_display with display_name = Some "Alicia" }
         ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check int) "bumped" 2 updated.revision;
  Alcotest.(check (option string))
    "display" (Some "Alicia") updated.display.display_name;
  match
    S.update_principal ~db ~id:p.id ~expected_revision:1 ~lifecycle:P.Disabled
      ~now:(fixed_now +. 2.) ()
  with
  | Ok _ -> Alcotest.fail "stale revision should fail"
  | Error msg ->
      Alcotest.(check bool)
        "conflict" true
        (Test_helpers.string_contains (String.lowercase_ascii msg) "revision"
        || Test_helpers.string_contains (String.lowercase_ascii msg) "conflict"
        )

let test_connector_actor_revision_bump () =
  with_db @@ fun db ->
  let p = sample_principal () in
  ignore (assert_ok (S.insert_principal ~db ~now:fixed_now p));
  let key = sample_key () in
  ignore
    (assert_ok
       (S.insert_connector_actor ~db ~now:fixed_now
          (sample_actor ~key ~principal_id:p.id ())));
  let updated =
    assert_ok
      (S.update_connector_actor ~db ~key ~expected_revision:1
         ~display:{ sample_display with display_name = Some "Ada (renamed)" }
         ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check int) "bumped" 2 updated.revision;
  Alcotest.(check string)
    "identity unchanged" (P.actor_identity_key key)
    (P.actor_identity_key updated.key);
  Alcotest.(check (option string))
    "display renamed" (Some "Ada (renamed)") updated.display.display_name;
  match
    S.update_connector_actor ~db ~key ~expected_revision:1 ~lifecycle:P.Disabled
      ~now:(fixed_now +. 2.) ()
  with
  | Ok _ -> Alcotest.fail "stale revision should fail"
  | Error msg ->
      Alcotest.(check bool)
        "conflict" true
        (Test_helpers.string_contains (String.lowercase_ascii msg) "revision")

let test_identity_link_revision_bump () =
  with_db @@ fun db ->
  let p = sample_principal () in
  ignore (assert_ok (S.insert_principal ~db ~now:fixed_now p));
  let key = sample_key () in
  ignore
    (assert_ok
       (S.insert_connector_actor ~db ~now:fixed_now
          (sample_actor ~key ~principal_id:p.id ())));
  let l =
    assert_ok
      (S.insert_identity_link ~db ~now:fixed_now
         (sample_link ~id:"link_rev" ~key ~principal_id:p.id ()))
  in
  let updated =
    assert_ok
      (S.update_identity_link ~db ~id:l.id ~expected_revision:1
         ~status:P.Unlinked ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check int) "bumped" 2 updated.revision;
  Alcotest.(check string)
    "status" "unlinked"
    (P.string_of_identity_link_status updated.status);
  Alcotest.(check bool)
    "unlinked_at set" true
    (match updated.unlinked_at with Some _ -> true | None -> false);
  match
    S.update_identity_link ~db ~id:l.id ~expected_revision:1
      ~status:P.Superseded ~now:(fixed_now +. 2.) ()
  with
  | Ok _ -> Alcotest.fail "stale revision should fail"
  | Error msg ->
      Alcotest.(check bool)
        "conflict" true
        (Test_helpers.string_contains (String.lowercase_ascii msg) "revision")

let test_merged_into_principal_roundtrip () =
  with_db @@ fun db ->
  let survivor = sample_principal_id ~s:"prin_survivor" () in
  let loser = sample_principal_id ~s:"prin_loser" () in
  ignore
    (assert_ok
       (S.insert_principal ~db ~now:fixed_now
          (P.make_principal ~id:survivor ~created_at:"2026-07-01T00:00:00Z"
             ~updated_at:"2026-07-01T00:00:00Z" ())));
  ignore
    (assert_ok
       (S.insert_principal ~db ~now:fixed_now
          (P.make_principal ~id:loser ~lifecycle:(P.Merged_into survivor)
             ~revision:3 ~created_at:"2026-07-02T00:00:00Z"
             ~updated_at:"2026-07-13T00:00:00Z" ())));
  match S.get_principal ~db ~id:loser with
  | Error e -> Alcotest.fail e
  | Ok None -> Alcotest.fail "missing"
  | Ok (Some got) -> (
      match got.lifecycle with
      | P.Merged_into id ->
          Alcotest.(check string)
            "survivor" "prin_survivor"
            (P.principal_id_to_string id);
          Alcotest.(check bool) "not active" false (P.principal_is_active got)
      | P.Active -> Alcotest.fail "expected Merged_into, got Active"
      | P.Disabled -> Alcotest.fail "expected Merged_into, got Disabled")

let suite =
  [
    ("insert get principal", `Quick, test_insert_get_principal);
    ("insert get connector_actor", `Quick, test_insert_get_connector_actor);
    ("insert get identity_link", `Quick, test_insert_get_identity_link);
    ( "connector_actor collision reject",
      `Quick,
      test_connector_actor_collision_reject );
    ( "active identity_link collision reject",
      `Quick,
      test_active_identity_link_collision_reject );
    ( "cross-tenant same user no collision",
      `Quick,
      test_cross_tenant_same_user_no_collision );
    ("first-seen collision reject", `Quick, test_first_seen_collision_reject);
    ("principal revision bump", `Quick, test_principal_revision_bump);
    ("connector_actor revision bump", `Quick, test_connector_actor_revision_bump);
    ("identity_link revision bump", `Quick, test_identity_link_revision_bump);
    ( "merged_into principal roundtrip",
      `Quick,
      test_merged_into_principal_roundtrip );
  ]

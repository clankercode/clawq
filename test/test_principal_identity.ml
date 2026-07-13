(** Tests for Principal / Connector actor / Identity Link domain model
    (P21.M1.E1.T001). *)

module P = Principal_identity

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let sample_principal_id () =
  assert_ok (P.principal_id_of_string "prin_01HZX9EXAMPLE000000000001")

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

let sample_principal () =
  P.make_principal ~id:(sample_principal_id ()) ~display:sample_display
    ~created_at:"2026-07-13T00:00:00Z" ~updated_at:"2026-07-13T00:00:01Z" ()

let sample_actor () =
  P.make_connector_actor ~key:(sample_key ())
    ~principal_id:(sample_principal_id ()) ~display:sample_display
    ~verified_at:"2026-07-13T00:00:00Z" ~created_at:"2026-07-13T00:00:00Z"
    ~updated_at:"2026-07-13T00:00:01Z" ()

let sample_link () =
  P.make_identity_link ~id:"link_1" ~principal_id:(sample_principal_id ())
    ~actor_key:(sample_key ()) ~linked_at:"2026-07-13T00:00:00Z" ()

let test_schema_version () =
  Alcotest.(check int) "schema_version" 1 P.schema_version

let test_principal_id_opaque_rejects_empty () =
  (match P.principal_id_of_string "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "empty principal_id must be rejected");
  (match P.principal_id_of_string "   " with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "whitespace principal_id must be rejected");
  let id = assert_ok (P.principal_id_of_string "  prin_ok  ") in
  Alcotest.(check string) "trimmed" "prin_ok" (P.principal_id_to_string id)

let test_principal_json_roundtrip () =
  let p = sample_principal () in
  let json = P.principal_to_json p in
  let back = assert_ok (P.principal_of_json json) in
  Alcotest.(check int) "version" p.version back.version;
  Alcotest.(check string)
    "id"
    (P.principal_id_to_string p.id)
    (P.principal_id_to_string back.id);
  Alcotest.(check int) "revision" p.revision back.revision;
  Alcotest.(check (option string))
    "display_name" p.display.display_name back.display.display_name;
  Alcotest.(check (option string)) "email" p.display.email back.display.email;
  Alcotest.(check string) "created_at" p.created_at back.created_at;
  Alcotest.(check bool) "active" true (P.principal_is_active back)

let test_principal_merged_into_roundtrip () =
  let survivor = assert_ok (P.principal_id_of_string "prin_survivor") in
  let p =
    P.make_principal ~id:(sample_principal_id ())
      ~lifecycle:(P.Merged_into survivor) ~revision:3
      ~created_at:"2026-07-01T00:00:00Z" ~updated_at:"2026-07-13T00:00:00Z" ()
  in
  let back = assert_ok (P.principal_of_json (P.principal_to_json p)) in
  (match back.lifecycle with
  | P.Merged_into id ->
      Alcotest.(check string)
        "survivor" "prin_survivor"
        (P.principal_id_to_string id)
  | P.Active -> Alcotest.fail "expected Merged_into, got Active"
  | P.Disabled -> Alcotest.fail "expected Merged_into, got Disabled");
  Alcotest.(check bool) "not active" false (P.principal_is_active back)

let test_connector_actor_json_roundtrip () =
  let a = sample_actor () in
  let back =
    assert_ok (P.connector_actor_of_json (P.connector_actor_to_json a))
  in
  Alcotest.(check int) "version" a.version back.version;
  Alcotest.(check string)
    "identity key"
    (P.actor_identity_key a.key)
    (P.actor_identity_key back.key);
  Alcotest.(check string)
    "principal"
    (P.principal_id_to_string a.principal_id)
    (P.principal_id_to_string back.principal_id);
  Alcotest.(check (option string))
    "display_name" a.display.display_name back.display.display_name;
  Alcotest.(check (option string)) "verified_at" a.verified_at back.verified_at

let test_identity_link_json_roundtrip () =
  let l = sample_link () in
  let back = assert_ok (P.identity_link_of_json (P.identity_link_to_json l)) in
  Alcotest.(check string) "id" l.id back.id;
  Alcotest.(check string)
    "principal"
    (P.principal_id_to_string l.principal_id)
    (P.principal_id_to_string back.principal_id);
  Alcotest.(check string)
    "actor key"
    (P.actor_identity_key l.actor_key)
    (P.actor_identity_key back.actor_key);
  Alcotest.(check string)
    "status"
    (P.string_of_identity_link_status l.status)
    (P.string_of_identity_link_status back.status)

let test_display_name_is_not_identity () =
  let key = sample_key ~user:"U123" () in
  let actor =
    P.make_connector_actor ~key ~principal_id:(sample_principal_id ())
      ~display:{ P.empty_display with display_name = Some "Alice" }
      ()
  in
  let renamed =
    P.with_actor_display actor
      { actor.display with display_name = Some "Alice (renamed)" }
  in
  Alcotest.(check string)
    "identity key unchanged by display rename"
    (P.actor_identity_key actor.key)
    (P.actor_identity_key renamed.key);
  Alcotest.(check bool)
    "keys equal" true
    (P.connector_actor_key_equal actor.key renamed.key);
  (* Principal display rename similarly preserves id. *)
  let prin =
    P.make_principal ~id:(sample_principal_id ())
      ~display:{ P.empty_display with display_name = Some "Alice" }
      ()
  in
  let prin2 =
    P.with_principal_display prin
      { prin.display with display_name = Some "Alicia" }
  in
  Alcotest.(check string)
    "principal id unchanged"
    (P.principal_id_to_string prin.id)
    (P.principal_id_to_string prin2.id)

let test_cross_tenant_same_user_distinct () =
  let a = sample_key ~tenant:"tenant-a" ~user:"U1" () in
  let b = sample_key ~tenant:"tenant-b" ~user:"U1" () in
  Alcotest.(check bool)
    "same user different tenant are distinct" false
    (P.connector_actor_key_equal a b);
  Alcotest.(check bool)
    "keys differ" false
    (String.equal (P.actor_identity_key a) (P.actor_identity_key b))

let test_room_session_are_non_identity () =
  (* Room/session fields live only on the explicit non-identity context type.
     Principal and Connector_actor records have no room_id/session_id fields —
     proven by codecs: identity JSON must not require them, and non-identity
     context carries the flag identity=false. *)
  let ctx : P.non_identity_context =
    {
      room_id = Some "room-teams-1";
      session_id = Some "teams:room-teams-1:alice";
      display_name = Some "Alice";
    }
  in
  let json = P.non_identity_context_to_json ctx in
  let back = assert_ok (P.non_identity_context_of_json json) in
  Alcotest.(check (option string)) "room" (Some "room-teams-1") back.room_id;
  Alcotest.(check (option string))
    "session" (Some "teams:room-teams-1:alice") back.session_id;
  Alcotest.(check (option string)) "display" (Some "Alice") back.display_name;
  (match json with
  | `Assoc fields -> (
      (match List.assoc_opt "identity" fields with
      | Some (`Bool false) -> ()
      | Some _ -> Alcotest.fail "identity flag must be false"
      | None -> Alcotest.fail "identity flag missing");
      (* Must not be consumable as a Principal: missing required principal fields. *)
      (match P.principal_of_json json with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "room/session context must not parse as Principal");
      match P.connector_actor_of_json json with
      | Error _ -> ()
      | Ok _ ->
          Alcotest.fail "room/session context must not parse as Connector_actor"
      )
  | _ -> Alcotest.fail "expected object");
  (* Actor identity key never embeds room or session. *)
  let key = sample_key () in
  let ik = P.actor_identity_key key in
  Alcotest.(check bool)
    "no room in identity key" false
    (Test_helpers.string_contains ik "room-teams-1");
  Alcotest.(check bool)
    "no session in identity key" false
    (Test_helpers.string_contains ik "session")

let test_actor_key_rejects_empty_scope () =
  (match
     P.make_connector_actor_key ~connector:P.Slack ~tenant_or_workspace:""
       ~immutable_user_id:"U1"
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "empty tenant must fail");
  match
    P.make_connector_actor_key ~connector:P.Slack ~tenant_or_workspace:"T"
      ~immutable_user_id:""
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "empty user id must fail"

let test_connectors_roundtrip_strings () =
  List.iter
    (fun c ->
      let s = P.string_of_connector c in
      let back = assert_ok (P.connector_of_string s) in
      Alcotest.(check string)
        s (P.string_of_connector c)
        (P.string_of_connector back))
    [ P.Teams; P.Slack; P.Discord; P.Telegram; P.Web; P.Cli; P.Direct ]

let suite =
  [
    ("schema_version", `Quick, test_schema_version);
    ( "principal_id rejects empty",
      `Quick,
      test_principal_id_opaque_rejects_empty );
    ("principal json roundtrip", `Quick, test_principal_json_roundtrip);
    ( "principal merged_into roundtrip",
      `Quick,
      test_principal_merged_into_roundtrip );
    ( "connector_actor json roundtrip",
      `Quick,
      test_connector_actor_json_roundtrip );
    ("identity_link json roundtrip", `Quick, test_identity_link_json_roundtrip);
    ("display name is not identity", `Quick, test_display_name_is_not_identity);
    ( "cross-tenant same user distinct",
      `Quick,
      test_cross_tenant_same_user_distinct );
    ("room/session are non-identity", `Quick, test_room_session_are_non_identity);
    ("actor key rejects empty scope", `Quick, test_actor_key_rejects_empty_scope);
    ("connector string roundtrip", `Quick, test_connectors_roundtrip_strings);
  ]

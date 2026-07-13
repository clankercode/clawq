(** Tests for immutable Actor snapshots (P21.M1.E3.T001). *)

module P = Principal_identity
module S = Principal_identity_store
module B = Github_account_binding
module A = Actor_snapshot
module M = Principal_merge

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  B.ensure_schema db;
  M.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_785_200_000.0
let pid s = assert_ok (P.principal_id_of_string s)

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

let seed_principal ~db ~id ?(revision = 1)
    ?(created_at = "2026-01-01T00:00:00Z") () =
  let p =
    P.make_principal ~id:(pid id) ~revision ~created_at ~updated_at:created_at
      ()
  in
  ignore (assert_ok (S.insert_principal ~db ~now:fixed_now p))

let seed_actor_and_link ~db ~principal_id ~key ?(link_id = "idlink_1")
    ?(display = sample_display) () =
  let actor =
    P.make_connector_actor ~key ~principal_id ~display
      ~verified_at:"2026-07-01T00:00:00Z" ~created_at:"2026-07-01T00:00:00Z"
      ~updated_at:"2026-07-01T00:00:00Z" ()
  in
  let actor = assert_ok (S.insert_connector_actor ~db ~now:fixed_now actor) in
  let link =
    P.make_identity_link ~id:link_id ~principal_id ~actor_key:key
      ~linked_at:"2026-07-01T00:00:00Z" ()
  in
  let link = assert_ok (S.insert_identity_link ~db ~now:fixed_now link) in
  (actor, link)

let sample_binding ~principal_id ?(id = "ghbind_1") ?(lineage_id = "lineage_1")
    () =
  let identity =
    assert_ok (B.make_account_identity ~app_id:42 ~github_user_id:9001L ())
  in
  B.make_binding ~id ~principal_id ~identity ~lineage_id
    ~authorization_status:B.Authorized
    ~display:{ B.login = Some "octocat"; avatar_url = None }
    ~vault_ref:(assert_ok (B.make_vault_ref "vault_row_opaque_only"))
    ~created_at:"2026-07-01T00:00:00Z" ~updated_at:"2026-07-01T00:00:00Z" ()

(* -------------------------------------------------------------------------- *)
(* Pure create / JSON                                                         *)
(* -------------------------------------------------------------------------- *)

let test_schema_version () =
  Alcotest.(check int) "schema_version" 1 A.schema_version

let test_create_immutable_record () =
  let principal_id = pid "prin_a" in
  let key = sample_key () in
  let ab =
    assert_ok
      (A.make_account_binding_evidence ~binding_id:"ghbind_1"
         ~lineage_id:"lineage_1"
         ~identity:
           (assert_ok
              (B.make_account_identity ~app_id:42 ~github_user_id:9001L ()))
         ())
  in
  let snap =
    assert_ok
      (A.create ~id:"actorsnap_test_1" ~now:fixed_now ~reason:"intent_create"
         ~principal_id ~principal_revision:3 ~actor_key:key ~actor_revision:2
         ~identity_link_id:"idlink_1" ~identity_link_revision:4
         ~display:sample_display
         ~source:
           {
             room_id = Some "room_1";
             session_id = Some "sess_1";
             message_id = Some "msg_9";
           }
         ~account_binding:ab
         ~work_refs:
           {
             intent_id = Some "intent_1";
             confirmation_id = Some "conf_1";
             delayed_job_id = Some "job_1";
           }
         ())
  in
  Alcotest.(check int) "version" 1 snap.version;
  Alcotest.(check string) "id" "actorsnap_test_1" snap.id;
  Alcotest.(check string)
    "principal"
    (P.principal_id_to_string principal_id)
    (P.principal_id_to_string snap.lineage.principal_id);
  Alcotest.(check int) "principal rev" 3 snap.lineage.principal_revision;
  Alcotest.(check int) "link rev" 4 snap.lineage.identity_link_revision;
  Alcotest.(check (option string))
    "account lineage" (Some "lineage_1") snap.lineage.account_lineage_id;
  Alcotest.(check (option string)) "room" (Some "room_1") snap.source.room_id;
  Alcotest.(check (option string))
    "intent" (Some "intent_1") snap.work_refs.intent_id;
  Alcotest.(check (option string))
    "confirmation" (Some "conf_1") snap.work_refs.confirmation_id;
  Alcotest.(check bool) "never authority" false (A.is_authority snap);
  Alcotest.(check (option string))
    "display frozen" (Some "Ada Lovelace") snap.display.display_name

let test_json_roundtrip_and_redaction () =
  let principal_id = pid "prin_a" in
  let key = sample_key () in
  let ab =
    assert_ok
      (A.make_account_binding_evidence ~binding_id:"ghbind_1"
         ~lineage_id:"lineage_1"
         ~identity:
           (assert_ok
              (B.make_account_identity ~app_id:42 ~github_user_id:9001L ()))
         ())
  in
  let snap =
    assert_ok
      (A.create ~id:"actorsnap_json" ~now:fixed_now ~principal_id ~actor_key:key
         ~identity_link_id:"idlink_1" ~display:sample_display
         ~account_binding:ab
         ~work_refs:
           {
             intent_id = Some "i1";
             confirmation_id = None;
             delayed_job_id = Some "j1";
           }
         ())
  in
  let json = A.to_json snap in
  Alcotest.(check bool)
    "no token material in to_json" false
    (A.contains_token_material json);
  (match json with
  | `Assoc fields ->
      Alcotest.(check bool)
        "authority flag false" true
        (List.assoc "authority" fields = `Bool false);
      Alcotest.(check bool)
        "no vault_ref key" true
        (not (List.mem_assoc "vault_ref" fields))
  | _ -> Alcotest.fail "expected object");
  let back = assert_ok (A.of_json json) in
  Alcotest.(check string) "id roundtrip" snap.id back.id;
  Alcotest.(check string)
    "actor key"
    (P.actor_identity_key snap.lineage.actor_key)
    (P.actor_identity_key back.lineage.actor_key);
  Alcotest.(check (option string))
    "display name" snap.display.display_name back.display.display_name;
  (match back.account_binding with
  | None -> Alcotest.fail "expected account binding"
  | Some e ->
      Alcotest.(check string) "binding id" "ghbind_1" e.binding_id;
      Alcotest.(check string) "lineage" "lineage_1" e.lineage_id);
  let redacted = A.to_redacted_json snap in
  (match redacted with
  | `Assoc fields -> (
      Alcotest.(check bool)
        "redacted flag" true
        (List.assoc "redacted" fields = `Bool true);
      match List.assoc "display" fields with
      | `Assoc dfields ->
          Alcotest.(check bool)
            "email stripped" true
            (match List.assoc_opt "email" dfields with
            | None | Some `Null -> true
            | _ -> false)
      | _ -> Alcotest.fail "display object")
  | _ -> Alcotest.fail "redacted object");
  let summary = A.redacted_summary snap in
  Alcotest.(check bool)
    "summary has authority=false" true
    (A.contains_token_material (`String summary) = false
    &&
    let lower = String.lowercase_ascii summary in
    let rec has s sub =
      let n = String.length sub in
      let m = String.length s in
      let rec loop i =
        if i + n > m then false
        else if String.sub s i n = sub then true
        else loop (i + 1)
      in
      loop 0
    in
    has lower "authority=false")

let test_of_json_rejects_token_material () =
  let bad =
    `Assoc
      [
        ("version", `Int 1);
        ("id", `String "x");
        ("reason", `String "intent_create");
        ( "lineage",
          `Assoc
            [
              ("principal_id", `String "prin_a");
              ("principal_revision", `Int 1);
              ( "actor_key",
                `Assoc
                  [
                    ("connector", `String "teams");
                    ("tenant_or_workspace", `String "t");
                    ("immutable_user_id", `String "u");
                  ] );
              ("actor_revision", `Int 1);
              ("identity_link_revision", `Int 1);
            ] );
        ("access_token", `String "ghp_secret_must_not_appear");
      ]
  in
  match A.of_json bad with
  | Error msg ->
      Alcotest.(check bool)
        "mentions token/secret" true
        (let lower = String.lowercase_ascii msg in
         let rec has s sub =
           let n = String.length sub in
           let m = String.length s in
           let rec loop i =
             if i + n > m then false
             else if String.sub s i n = sub then true
             else loop (i + 1)
           in
           loop 0
         in
         has lower "token" || has lower "secret")
  | Ok _ -> Alcotest.fail "token material must be rejected"

let test_account_evidence_drops_vault_ref () =
  let b = sample_binding ~principal_id:(pid "prin_a") () in
  Alcotest.(check bool) "binding has vault_ref" true (b.vault_ref <> None);
  let ev = A.account_binding_evidence_of_binding b in
  let j =
    A.to_json
      (assert_ok
         (A.create ~principal_id:(pid "prin_a") ~actor_key:(sample_key ())
            ~account_binding:ev ()))
  in
  Alcotest.(check bool)
    "snapshot evidence has no vault" false
    (A.contains_token_material j);
  match j with
  | `Assoc fields -> (
      match List.assoc "account_binding" fields with
      | `Assoc ab ->
          Alcotest.(check bool)
            "no vault_ref on evidence" true
            (not (List.mem_assoc "vault_ref" ab))
      | _ -> Alcotest.fail "account_binding object")
  | _ -> Alcotest.fail "object"

(* -------------------------------------------------------------------------- *)
(* Live capture + re-resolve                                                  *)
(* -------------------------------------------------------------------------- *)

let test_create_from_live_and_usable () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let key = sample_key () in
  let _actor, link =
    seed_actor_and_link ~db ~principal_id:(pid "prin_a") ~key ()
  in
  let binding =
    assert_ok
      (B.insert ~db ~now:fixed_now
         (sample_binding ~principal_id:(pid "prin_a") ()))
  in
  let snap =
    assert_ok
      (A.create_from_live ~db ~now:fixed_now ~actor_key:key
         ~account_binding_id:binding.id
         ~source:
           {
             room_id = Some "room_x";
             session_id = Some "sess_x";
             message_id = Some "msg_x";
           }
         ~work_refs:
           {
             intent_id = Some "intent_live";
             confirmation_id = Some "conf_live";
             delayed_job_id = None;
           }
         ())
  in
  Alcotest.(check string)
    "principal" "prin_a"
    (P.principal_id_to_string snap.lineage.principal_id);
  Alcotest.(check (option string))
    "link id" (Some link.id) snap.lineage.identity_link_id;
  Alcotest.(check (option string))
    "display from actor" (Some "Ada Lovelace") snap.display.display_name;
  let auth = assert_ok (A.re_resolve_current_authority ~db snap) in
  Alcotest.(check bool) "usable" true auth.usable;
  Alcotest.(check int) "no breaks" 0 (List.length auth.breaks);
  Alcotest.(check bool) "not merge alias" false auth.followed_merge_alias;
  match auth.live_principal_id with
  | Some p ->
      Alcotest.(check string)
        "live principal" "prin_a"
        (P.principal_id_to_string p)
  | None -> Alcotest.fail "expected live principal"

let test_display_rename_preserves_evidence_and_authority () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let key = sample_key () in
  let _actor, _link =
    seed_actor_and_link ~db ~principal_id:(pid "prin_a") ~key ()
  in
  let snap =
    assert_ok (A.create_from_live ~db ~now:fixed_now ~actor_key:key ())
  in
  let frozen = snap.display.display_name in
  let _ =
    assert_ok
      (S.update_connector_actor ~db ~key ~now:fixed_now
         ~display:{ sample_display with display_name = Some "Augusta Ada King" }
         ())
  in
  Alcotest.(check (option string))
    "snapshot display frozen" frozen snap.display.display_name;
  let auth = assert_ok (A.re_resolve_current_authority ~db snap) in
  Alcotest.(check bool) "still usable after rename" true auth.usable

let test_merge_preserves_evidence_and_follows_alias () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_survivor" ~created_at:"2026-01-01T00:00:00Z" ();
  seed_principal ~db ~id:"prin_loser" ~created_at:"2026-06-01T00:00:00Z" ();
  let key = sample_key ~user:"user-loser" () in
  let _actor, _link =
    seed_actor_and_link ~db ~principal_id:(pid "prin_loser") ~key
      ~link_id:"idlink_loser" ()
  in
  let binding =
    assert_ok
      (B.insert ~db ~now:fixed_now
         (sample_binding ~principal_id:(pid "prin_loser") ~id:"ghbind_loser"
            ~lineage_id:"lineage_loser" ()))
  in
  let snap =
    assert_ok
      (A.create_from_live ~db ~now:fixed_now ~actor_key:key
         ~account_binding_id:binding.id ~reason:"delayed_job" ())
  in
  Alcotest.(check string)
    "snapshot principal is loser" "prin_loser"
    (P.principal_id_to_string snap.lineage.principal_id);
  (match
     M.apply_merge ~db ~now:fixed_now ~left_id:(pid "prin_survivor")
       ~right_id:(pid "prin_loser") ()
   with
  | M.Applied _ | M.Idempotent _ -> ()
  | M.Refused { reason; _ } -> Alcotest.fail ("merge refused: " ^ reason)
  | M.Stale_revision s -> Alcotest.fail ("merge stale: " ^ s));
  (* Evidence immutable *)
  Alcotest.(check string)
    "evidence still loser" "prin_loser"
    (P.principal_id_to_string snap.lineage.principal_id);
  let auth = assert_ok (A.re_resolve_current_authority ~db snap) in
  Alcotest.(check bool) "followed merge alias" true auth.followed_merge_alias;
  (match auth.live_principal_id with
  | Some p ->
      Alcotest.(check string)
        "live is survivor" "prin_survivor"
        (P.principal_id_to_string p)
  | None -> Alcotest.fail "live principal");
  Alcotest.(check bool) "usable after merge adoption" true auth.usable;
  (* Snapshot JSON still records original principal *)
  let back = assert_ok (A.of_json (A.to_json snap)) in
  Alcotest.(check string)
    "json principal preserved" "prin_loser"
    (P.principal_id_to_string back.lineage.principal_id)

let test_split_forces_re_resolution_break () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_source" ();
  let key = sample_key ~user:"user-split" () in
  let _actor, link =
    seed_actor_and_link ~db ~principal_id:(pid "prin_source") ~key
      ~link_id:"idlink_src" ()
  in
  let binding =
    assert_ok
      (B.insert ~db ~now:fixed_now
         (sample_binding ~principal_id:(pid "prin_source") ~id:"ghbind_src"
            ~lineage_id:"lineage_src" ()))
  in
  let snap =
    assert_ok
      (A.create_from_live ~db ~now:fixed_now ~actor_key:key
         ~account_binding_id:binding.id ())
  in
  (* Simulate unlink/split: supersede old link, create new principal + link,
     move actor, leave binding on source. *)
  let new_pid = pid "prin_new_empty" in
  ignore
    (assert_ok
       (S.insert_principal ~db ~now:fixed_now
          (P.make_principal ~id:new_pid ~created_at:"2026-07-13T00:00:00Z"
             ~updated_at:"2026-07-13T00:00:00Z" ())));
  ignore
    (assert_ok
       (S.update_identity_link ~db ~id:link.id ~status:P.Unlinked
          ~unlinked_at:(Some "2026-07-13T12:00:00Z") ~now:fixed_now ()));
  ignore
    (assert_ok
       (S.insert_identity_link ~db ~now:fixed_now
          (P.make_identity_link ~id:"idlink_new" ~principal_id:new_pid
             ~actor_key:key ~linked_at:"2026-07-13T12:00:00Z" ())));
  ignore
    (assert_ok
       (S.update_connector_actor ~db ~key ~principal_id:new_pid ~now:fixed_now
          ()));
  let auth = assert_ok (A.re_resolve_current_authority ~db snap) in
  Alcotest.(check bool) "not usable after split" false auth.usable;
  Alcotest.(check bool)
    "principal changed break" true
    (List.exists
       (function A.Principal_changed _ -> true | _ -> false)
       auth.breaks);
  (* Binding remains on source; account owner mismatch or not authorized path *)
  Alcotest.(check bool)
    "has account or principal break" true
    (List.exists
       (function
         | A.Principal_changed _ | A.Account_owner_mismatch _
         | A.Account_binding_missing ->
             true
         | _ -> false)
       auth.breaks);
  (* Evidence unchanged *)
  Alcotest.(check string)
    "snapshot still source" "prin_source"
    (P.principal_id_to_string snap.lineage.principal_id)

let test_revocation_breaks_account_authority () =
  with_db @@ fun db ->
  seed_principal ~db ~id:"prin_a" ();
  let key = sample_key () in
  let _actor, _link =
    seed_actor_and_link ~db ~principal_id:(pid "prin_a") ~key ()
  in
  let binding =
    assert_ok
      (B.insert ~db ~now:fixed_now
         (sample_binding ~principal_id:(pid "prin_a") ()))
  in
  let snap =
    assert_ok
      (A.create_from_live ~db ~now:fixed_now ~actor_key:key
         ~account_binding_id:binding.id ())
  in
  let _ =
    assert_ok
      (B.update_authorization_status ~db ~id:binding.id ~status:B.Revoked
         ~now:fixed_now ())
  in
  let auth = assert_ok (A.re_resolve_current_authority ~db snap) in
  Alcotest.(check bool) "not usable after revoke" false auth.usable;
  Alcotest.(check bool)
    "account not authorized" true
    (List.exists
       (function
         | A.Account_not_authorized { status = B.Revoked } -> true | _ -> false)
       auth.breaks)

let test_create_rejects_non_positive_revision () =
  match
    A.create ~principal_id:(pid "prin_a") ~actor_key:(sample_key ())
      ~principal_revision:0 ()
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "principal_revision 0 must fail"

let suite =
  [
    ("schema_version", `Quick, test_schema_version);
    ("create immutable record", `Quick, test_create_immutable_record);
    ("json roundtrip and redaction", `Quick, test_json_roundtrip_and_redaction);
    ( "of_json rejects token material",
      `Quick,
      test_of_json_rejects_token_material );
    ( "account evidence drops vault_ref",
      `Quick,
      test_account_evidence_drops_vault_ref );
    ("create from live and usable", `Quick, test_create_from_live_and_usable);
    ( "display rename preserves evidence",
      `Quick,
      test_display_rename_preserves_evidence_and_authority );
    ( "merge preserves evidence follows alias",
      `Quick,
      test_merge_preserves_evidence_and_follows_alias );
    ( "split forces re-resolution break",
      `Quick,
      test_split_forces_re_resolution_break );
    ( "revocation breaks account authority",
      `Quick,
      test_revocation_breaks_account_authority );
    ( "create rejects non-positive revision",
      `Quick,
      test_create_rejects_non_positive_revision );
  ]

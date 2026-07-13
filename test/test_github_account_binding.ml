(** Tests for Principal-owned GitHub account bindings (P21.M1.E2.T001). *)

module P = Principal_identity
module S = Principal_identity_store
module B = Github_account_binding
module M = Principal_merge

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  B.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_785_100_000.0
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let pid s = assert_ok (P.principal_id_of_string s)

let seed_principal ~db ~id ?(created_at = "2026-01-01T00:00:00Z") () =
  let p =
    P.make_principal ~id:(pid id) ~revision:1 ~created_at ~updated_at:created_at
      ()
  in
  assert_ok (S.insert_principal ~db ~now:fixed_now p)

let sample_identity ?(host = B.default_host) ?(app_id = 42)
    ?(github_user_id = 9001L) () =
  assert_ok (B.make_account_identity ~host ~app_id ~github_user_id ())

let sample_binding ~principal_id ?(id = "ghbind_1")
    ?(identity = sample_identity ()) ?(login = Some "octocat")
    ?(avatar = Some "https://avatars.example/o.png") ?(status = B.Authorized)
    ?(vault_ref = Some "ghvault_opaque_1") ?(lineage_id = "lineage_1") () =
  let vault_ref =
    match vault_ref with
    | None -> None
    | Some s -> Some (assert_ok (B.make_vault_ref s))
  in
  B.make_binding ~id ~principal_id ~identity
    ~display:{ B.login; avatar_url = avatar }
    ~authorization_status:status ~lineage_id ?vault_ref ()

(* -------------------------------------------------------------------------- *)
(* Insert / get                                                               *)
(* -------------------------------------------------------------------------- *)

let test_insert_get_roundtrip () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  let b =
    sample_binding ~principal_id:(pid "prin_a")
      ~vault_ref:(Some "ghvault_opaque_1") ()
  in
  let stored = assert_ok (B.insert ~db ~now:fixed_now b) in
  Alcotest.(check string) "id" "ghbind_1" stored.id;
  Alcotest.(check string) "lineage" "lineage_1" stored.lineage_id;
  Alcotest.(check string) "host" "github.com" stored.identity.host;
  Alcotest.(check int) "app" 42 stored.identity.app_id;
  Alcotest.(check bool)
    "user" true
    (Int64.equal stored.identity.github_user_id 9001L);
  Alcotest.(check (option string)) "login" (Some "octocat") stored.display.login;
  Alcotest.(check string)
    "status" "authorized"
    (B.string_of_authorization_status stored.authorization_status);
  Alcotest.(check (option string))
    "vault ref opaque" (Some "ghvault_opaque_1") stored.vault_ref;
  match B.get ~db ~id:stored.id with
  | Error e -> Alcotest.fail e
  | Ok None -> Alcotest.fail "missing"
  | Ok (Some got) ->
      Alcotest.(check string)
        "principal" "prin_a"
        (P.principal_id_to_string got.principal_id);
      Alcotest.(check int) "revision" 1 got.revision;
      Alcotest.(check int) "version" B.schema_version got.version

let test_identity_collision_reject () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  ignore (seed_principal ~db ~id:"prin_b" ());
  let identity = sample_identity ~github_user_id:42L () in
  ignore
    (assert_ok
       (B.insert ~db ~now:fixed_now
          (sample_binding ~id:"b1" ~principal_id:(pid "prin_a") ~identity ())));
  match
    B.insert ~db ~now:fixed_now
      (sample_binding ~id:"b2" ~principal_id:(pid "prin_b") ~identity ())
  with
  | Ok _ -> Alcotest.fail "expected identity collision"
  | Error msg ->
      Alcotest.(check bool)
        "mentions bound" true
        (let lower = String.lowercase_ascii msg in
         let rec contains s sub =
           let n = String.length sub in
           let m = String.length s in
           let rec loop i =
             if i + n > m then false
             else if String.sub s i n = sub then true
             else loop (i + 1)
           in
           loop 0
         in
         contains lower "already bound" || contains lower "unique")

let test_get_by_identity_and_list () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  let i1 = sample_identity ~github_user_id:1L ~app_id:10 () in
  let i2 = sample_identity ~github_user_id:2L ~app_id:10 () in
  ignore
    (assert_ok
       (B.insert ~db ~now:fixed_now
          (sample_binding ~id:"b1" ~principal_id:(pid "prin_a") ~identity:i1 ())));
  ignore
    (assert_ok
       (B.insert ~db ~now:fixed_now
          (sample_binding ~id:"b2" ~principal_id:(pid "prin_a") ~identity:i2
             ~lineage_id:"lineage_2" ())));
  (match B.get_by_identity ~db ~identity:i1 with
  | Ok (Some b) -> Alcotest.(check string) "by identity" "b1" b.id
  | Ok None -> Alcotest.fail "missing"
  | Error e -> Alcotest.fail e);
  let listed =
    assert_ok (B.list_for_principal ~db ~principal_id:(pid "prin_a"))
  in
  Alcotest.(check int) "two bindings" 2 (List.length listed)

(* -------------------------------------------------------------------------- *)
(* Login mutation does not create a new account                               *)
(* -------------------------------------------------------------------------- *)

let test_login_change_preserves_identity_and_lineage () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  let identity = sample_identity ~github_user_id:77L () in
  let stored =
    assert_ok
      (B.insert ~db ~now:fixed_now
         (sample_binding ~id:"b_login" ~principal_id:(pid "prin_a") ~identity
            ~login:(Some "oldlogin") ~lineage_id:"lin_stable" ()))
  in
  let updated =
    assert_ok
      (B.update_display ~db ~now:(fixed_now +. 1.) ~id:stored.id
         ~login:(Some "newlogin")
         ~avatar_url:(Some "https://avatars.example/new.png")
         ~expected_revision:1 ())
  in
  Alcotest.(check string) "same id" stored.id updated.id;
  Alcotest.(check string) "same lineage" "lin_stable" updated.lineage_id;
  Alcotest.(check bool)
    "same user id" true
    (Int64.equal updated.identity.github_user_id 77L);
  Alcotest.(check int) "same app" 42 updated.identity.app_id;
  Alcotest.(check string) "same host" "github.com" updated.identity.host;
  Alcotest.(check (option string))
    "new login" (Some "newlogin") updated.display.login;
  Alcotest.(check int) "revision bumped" 2 updated.revision;
  (* Still a single row for the identity — not a new account. *)
  let listed =
    assert_ok (B.list_for_principal ~db ~principal_id:(pid "prin_a"))
  in
  Alcotest.(check int) "still one binding" 1 (List.length listed);
  match B.get_by_identity ~db ~identity with
  | Ok (Some b) -> Alcotest.(check string) "identity key stable" "b_login" b.id
  | _ -> Alcotest.fail "identity lookup lost"

let test_revision_cas_conflict () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  ignore
    (assert_ok
       (B.insert ~db ~now:fixed_now
          (sample_binding ~id:"b_cas" ~principal_id:(pid "prin_a") ())));
  match
    B.update_authorization_status ~db ~id:"b_cas" ~status:B.Disabled
      ~expected_revision:99 ()
  with
  | Ok _ -> Alcotest.fail "expected CAS conflict"
  | Error msg ->
      Alcotest.(check bool)
        "revision conflict" true
        (let lower = String.lowercase_ascii msg in
         String.length lower >= 17
         &&
         let rec contains s sub =
           let n = String.length sub in
           let m = String.length s in
           let rec loop i =
             if i + n > m then false
             else if String.sub s i n = sub then true
             else loop (i + 1)
           in
           loop 0
         in
         contains lower "revision conflict")

let test_vault_ref_opaque_only () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  let stored =
    assert_ok
      (B.insert ~db ~now:fixed_now
         (sample_binding ~id:"b_vault" ~principal_id:(pid "prin_a")
            ~vault_ref:None ()))
  in
  Alcotest.(check (option string)) "no vault" None stored.vault_ref;
  let vref = assert_ok (B.make_vault_ref "ghvault_only_handle") in
  let updated =
    assert_ok
      (B.set_vault_ref ~db ~id:stored.id ~vault_ref:(Some vref)
         ~expected_revision:1 ())
  in
  Alcotest.(check (option string))
    "opaque handle" (Some "ghvault_only_handle") updated.vault_ref;
  let j = B.binding_to_json updated in
  let s = Yojson.Safe.to_string j in
  (* JSON must not invent token fields. *)
  Alcotest.(check bool)
    "no access_token field" false
    (let rec contains s sub =
       let n = String.length sub in
       let m = String.length s in
       let rec loop i =
         if i + n > m then false
         else if String.sub s i n = sub then true
         else loop (i + 1)
       in
       loop 0
     in
     contains s "access_token" || contains s "refresh_token");
  match B.make_vault_ref "   " with
  | Ok _ -> Alcotest.fail "empty vault ref should fail"
  | Error _ -> ()

(* -------------------------------------------------------------------------- *)
(* Snapshots + transactional Principal adoption                               *)
(* -------------------------------------------------------------------------- *)

let test_snapshot_retains_prior_evidence () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_old" ());
  let stored =
    assert_ok
      (B.insert ~db ~now:fixed_now
         (sample_binding ~id:"b_snap" ~principal_id:(pid "prin_old")
            ~login:(Some "prior_login") ~lineage_id:"lin_snap" ()))
  in
  let snap =
    assert_ok
      (B.snapshot ~db ~now:fixed_now ~reason:"manual" ~related_id:"op_1"
         ~id:stored.id ())
  in
  Alcotest.(check string) "binding_id" "b_snap" snap.binding_id;
  Alcotest.(check string)
    "prior principal" "prin_old"
    (P.principal_id_to_string snap.principal_id_at_snapshot);
  Alcotest.(check string) "lineage" "lin_snap" snap.lineage_id;
  Alcotest.(check string) "reason" "manual" snap.reason;
  match B.binding_of_json (Yojson.Safe.from_string snap.binding_json) with
  | Error e -> Alcotest.fail e
  | Ok evidence ->
      Alcotest.(check (option string))
        "login evidence" (Some "prior_login") evidence.display.login;
      Alcotest.(check string)
        "principal evidence" "prin_old"
        (P.principal_id_to_string evidence.principal_id)

let test_adopt_to_principal_transactional () =
  with_db @@ fun db ->
  ignore
    (seed_principal ~db ~id:"prin_loser" ~created_at:"2026-06-01T00:00:00Z" ());
  ignore
    (seed_principal ~db ~id:"prin_survivor" ~created_at:"2026-01-01T00:00:00Z"
       ());
  let identity = sample_identity ~github_user_id:555L () in
  let stored =
    assert_ok
      (B.insert ~db ~now:fixed_now
         (sample_binding ~id:"b_adopt" ~principal_id:(pid "prin_loser")
            ~identity ~login:(Some "loser_login") ~lineage_id:"lin_adopt"
            ~vault_ref:(Some "ghvault_keep") ()))
  in
  let adopted, snap =
    assert_ok
      (B.adopt_to_principal ~db ~now:(fixed_now +. 2.) ~id:stored.id
         ~to_principal:(pid "prin_survivor") ~reason:"pre_adopt"
         ~related_id:"merge_x" ~expected_revision:1 ())
  in
  Alcotest.(check string)
    "new owner" "prin_survivor"
    (P.principal_id_to_string adopted.principal_id);
  Alcotest.(check string) "same binding id" "b_adopt" adopted.id;
  Alcotest.(check string) "lineage preserved" "lin_adopt" adopted.lineage_id;
  Alcotest.(check (option string))
    "vault ref preserved" (Some "ghvault_keep") adopted.vault_ref;
  Alcotest.(check int) "revision" 2 adopted.revision;
  Alcotest.(check string)
    "snapshot prior principal" "prin_loser"
    (P.principal_id_to_string snap.principal_id_at_snapshot);
  Alcotest.(check string) "snapshot lineage" "lin_adopt" snap.lineage_id;
  (* Live row is under survivor; historical snapshot retains loser evidence. *)
  let on_survivor =
    assert_ok (B.list_for_principal ~db ~principal_id:(pid "prin_survivor"))
  in
  Alcotest.(check int) "survivor owns" 1 (List.length on_survivor);
  let on_loser =
    assert_ok (B.list_for_principal ~db ~principal_id:(pid "prin_loser"))
  in
  Alcotest.(check int) "loser empty" 0 (List.length on_loser);
  match B.binding_of_json (Yojson.Safe.from_string snap.binding_json) with
  | Error e -> Alcotest.fail e
  | Ok prior ->
      Alcotest.(check string)
        "snapshot evidence owner" "prin_loser"
        (P.principal_id_to_string prior.principal_id);
      Alcotest.(check (option string))
        "snapshot login" (Some "loser_login") prior.display.login

let test_adopt_all_reassigns_nonconflicting () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_s" ~created_at:"2026-01-01T00:00:00Z" ());
  ignore (seed_principal ~db ~id:"prin_l" ~created_at:"2026-06-01T00:00:00Z" ());
  (* Distinct host/app/user identities: survivor keeps theirs; loser is adopted. *)
  let i_s = sample_identity ~github_user_id:9L ~app_id:7 () in
  let i_l = sample_identity ~github_user_id:10L ~app_id:8 () in
  ignore
    (assert_ok
       (B.insert ~db ~now:fixed_now
          (sample_binding ~id:"b_surv" ~principal_id:(pid "prin_s")
             ~identity:i_s ~login:(Some "surv") ~vault_ref:(Some "vault_surv")
             ~lineage_id:"lin_s" ())));
  ignore
    (assert_ok
       (B.insert ~db ~now:fixed_now
          (sample_binding ~id:"b_lose" ~principal_id:(pid "prin_l")
             ~identity:i_l ~login:(Some "lose") ~vault_ref:(Some "vault_lose")
             ~lineage_id:"lin_l" ())));
  let results =
    match
      B.adopt_all_for_principal ~db ~now:(fixed_now +. 1.)
        ~from_principal:(pid "prin_l") ~to_principal:(pid "prin_s")
        ~reason:"pre_merge" ()
    with
    | Ok v -> v
    | Error (`Msg e) -> Alcotest.fail e
    | Error (`Conflict c) -> Alcotest.fail ("unexpected conflict: " ^ c)
  in
  Alcotest.(check int) "one adoption" 1 (List.length results);
  (match B.get ~db ~id:"b_lose" with
  | Ok (Some b) ->
      Alcotest.(check string)
        "reassigned owner" "prin_s"
        (P.principal_id_to_string b.principal_id);
      Alcotest.(check string) "lineage preserved" "lin_l" b.lineage_id;
      Alcotest.(check (option string))
        "vault preserved" (Some "vault_lose") b.vault_ref;
      Alcotest.(check int) "revision bumped" 2 b.revision
  | Ok None -> Alcotest.fail "adopted binding missing"
  | Error e -> Alcotest.fail e);
  (match B.get ~db ~id:"b_surv" with
  | Ok (Some b) ->
      Alcotest.(check (option string))
        "survivor vault kept" (Some "vault_surv") b.vault_ref;
      Alcotest.(check int) "survivor revision unchanged" 1 b.revision
  | _ -> Alcotest.fail "survivor missing");
  let snaps =
    assert_ok (B.list_snapshots_for_binding ~db ~binding_id:"b_lose")
  in
  Alcotest.(check bool) "snapshot retained" true (List.length snaps >= 1);
  let snap = List.hd snaps in
  Alcotest.(check string)
    "prior principal on snapshot" "prin_l"
    (P.principal_id_to_string snap.principal_id_at_snapshot);
  let on_s =
    assert_ok (B.list_for_principal ~db ~principal_id:(pid "prin_s"))
  in
  Alcotest.(check int) "survivor holds both" 2 (List.length on_s)

let test_adopt_all_conflict_distinct_same_slot () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_s" ());
  ignore (seed_principal ~db ~id:"prin_l" ());
  (* Same host+app, different users → exclusive slot conflict. *)
  let i_s = sample_identity ~github_user_id:1L ~app_id:99 () in
  let i_l = sample_identity ~github_user_id:2L ~app_id:99 () in
  ignore
    (assert_ok
       (B.insert ~db ~now:fixed_now
          (sample_binding ~id:"bs" ~principal_id:(pid "prin_s") ~identity:i_s ())));
  ignore
    (assert_ok
       (B.insert ~db ~now:fixed_now
          (sample_binding ~id:"bl" ~principal_id:(pid "prin_l") ~identity:i_l ())));
  match
    B.adopt_all_for_principal ~db ~from_principal:(pid "prin_l")
      ~to_principal:(pid "prin_s") ()
  with
  | Ok _ -> Alcotest.fail "expected conflict"
  | Error (`Conflict _) ->
      (* No partial adoption. *)
      let on_l =
        assert_ok (B.list_for_principal ~db ~principal_id:(pid "prin_l"))
      in
      Alcotest.(check int) "loser still owns" 1 (List.length on_l)
  | Error (`Msg e) -> Alcotest.fail e

(* -------------------------------------------------------------------------- *)
(* Merge path wires adoption                                                  *)
(* -------------------------------------------------------------------------- *)

let test_merge_adopts_github_binding_with_snapshot () =
  with_db @@ fun db ->
  M.ensure_schema db;
  let older =
    assert_ok
      (S.insert_principal ~db ~now:fixed_now
         (P.make_principal ~id:(pid "prin_old") ~revision:1
            ~created_at:"2026-01-01T00:00:00Z"
            ~updated_at:"2026-01-01T00:00:00Z" ()))
  in
  let newer =
    assert_ok
      (S.insert_principal ~db ~now:fixed_now
         (P.make_principal ~id:(pid "prin_new") ~revision:1
            ~created_at:"2026-06-01T00:00:00Z"
            ~updated_at:"2026-06-01T00:00:00Z" ()))
  in
  let key_old =
    assert_ok
      (P.make_connector_actor_key ~connector:P.Teams ~tenant_or_workspace:"t"
         ~immutable_user_id:"u-old")
  in
  let key_new =
    assert_ok
      (P.make_connector_actor_key ~connector:P.Slack ~tenant_or_workspace:"ws"
         ~immutable_user_id:"u-new")
  in
  ignore
    (assert_ok
       (S.insert_connector_actor ~db ~now:fixed_now
          (P.make_connector_actor ~key:key_old ~principal_id:older.id
             ~created_at:"2026-01-01T00:00:00Z"
             ~updated_at:"2026-01-01T00:00:00Z" ())));
  ignore
    (assert_ok
       (S.insert_identity_link ~db ~now:fixed_now
          (P.make_identity_link ~id:"link_old" ~principal_id:older.id
             ~actor_key:key_old ~linked_at:"2026-01-01T00:00:00Z" ())));
  ignore
    (assert_ok
       (S.insert_connector_actor ~db ~now:fixed_now
          (P.make_connector_actor ~key:key_new ~principal_id:newer.id
             ~created_at:"2026-06-01T00:00:00Z"
             ~updated_at:"2026-06-01T00:00:00Z" ())));
  ignore
    (assert_ok
       (S.insert_identity_link ~db ~now:fixed_now
          (P.make_identity_link ~id:"link_new" ~principal_id:newer.id
             ~actor_key:key_new ~linked_at:"2026-06-01T00:00:00Z" ())));
  let identity = sample_identity ~github_user_id:12345L ~app_id:55 () in
  ignore
    (assert_ok
       (B.insert ~db ~now:fixed_now
          (sample_binding ~id:"b_merge" ~principal_id:newer.id ~identity
             ~login:(Some "merge_login") ~lineage_id:"lin_merge"
             ~vault_ref:(Some "ghvault_merge") ())));
  match
    M.apply_merge ~db ~left_id:older.id ~right_id:newer.id ~now:fixed_now
      ~merge_id:"pmerge_test_1" ()
  with
  | M.Applied _ | M.Idempotent _ -> (
      let on_old =
        assert_ok (B.list_for_principal ~db ~principal_id:older.id)
      in
      Alcotest.(check int) "survivor owns binding" 1 (List.length on_old);
      let b = List.hd on_old in
      Alcotest.(check string) "id" "b_merge" b.id;
      Alcotest.(check string) "lineage" "lin_merge" b.lineage_id;
      Alcotest.(check (option string))
        "vault" (Some "ghvault_merge") b.vault_ref;
      let snaps =
        assert_ok (B.list_snapshots_for_binding ~db ~binding_id:"b_merge")
      in
      Alcotest.(check bool) "has snapshot" true (List.length snaps >= 1);
      let snap = List.hd snaps in
      Alcotest.(check string)
        "snapshot prior principal" "prin_new"
        (P.principal_id_to_string snap.principal_id_at_snapshot);
      match B.binding_of_json (Yojson.Safe.from_string snap.binding_json) with
      | Error e -> Alcotest.fail e
      | Ok prior ->
          Alcotest.(check (option string))
            "prior login" (Some "merge_login") prior.display.login)
  | M.Refused { reason; _ } -> Alcotest.fail ("refused: " ^ reason)
  | M.Stale_revision s -> Alcotest.fail ("stale: " ^ s)

let suite =
  [
    ("insert get roundtrip", `Quick, test_insert_get_roundtrip);
    ("identity collision reject", `Quick, test_identity_collision_reject);
    ("get by identity and list", `Quick, test_get_by_identity_and_list);
    ( "login change preserves identity and lineage",
      `Quick,
      test_login_change_preserves_identity_and_lineage );
    ("revision CAS conflict", `Quick, test_revision_cas_conflict);
    ("vault ref opaque only", `Quick, test_vault_ref_opaque_only);
    ( "snapshot retains prior evidence",
      `Quick,
      test_snapshot_retains_prior_evidence );
    ( "adopt to principal transactional",
      `Quick,
      test_adopt_to_principal_transactional );
    ( "adopt all reassigns nonconflicting",
      `Quick,
      test_adopt_all_reassigns_nonconflicting );
    ( "adopt all conflict distinct same slot",
      `Quick,
      test_adopt_all_conflict_distinct_same_slot );
    ( "merge adopts github binding with snapshot",
      `Quick,
      test_merge_adopts_github_binding_with_snapshot );
  ]

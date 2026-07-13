(** Tests for unlink/split and identity revocation (P21.M1.E1.T012). *)

module P = Principal_identity
module S = Principal_identity_store
module M = Principal_merge
module U = Principal_unlink_split
module R = Principal_resolve

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  U.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_785_100_000.0
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let pid s = assert_ok (P.principal_id_of_string s)

let key ?(connector = P.Teams) ?(tenant = "tenant-a") ?(user = "user-1") () =
  assert_ok
    (P.make_connector_actor_key ~connector ~tenant_or_workspace:tenant
       ~immutable_user_id:user)

let insert_principal ~db ~id ~created_at ?(revision = 1) ?(now = fixed_now) () =
  let p =
    P.make_principal ~id:(pid id) ~revision ~created_at ~updated_at:created_at
      ()
  in
  assert_ok (S.insert_principal ~db ~now p)

let seed_owned_actor ~db ~principal_id ~key ~link_id ?(now = fixed_now) () =
  let actor =
    P.make_connector_actor ~key ~principal_id ~revision:1
      ~verified_at:"2026-07-13T00:00:00Z" ~created_at:"2026-07-13T00:00:00Z"
      ~updated_at:"2026-07-13T00:00:00Z" ()
  in
  ignore (assert_ok (S.insert_connector_actor ~db ~now actor));
  let link =
    P.make_identity_link ~id:link_id ~principal_id ~actor_key:key ~revision:1
      ~linked_at:"2026-07-13T00:00:00Z" ()
  in
  ignore (assert_ok (S.insert_identity_link ~db ~now link));
  (actor, link)

let status_msg = function
  | U.Applied _ -> "Applied"
  | U.Idempotent _ -> "Idempotent"
  | U.Refused { reason; _ } -> "Refused: " ^ reason
  | U.Stale_revision s -> "Stale: " ^ s

(* -------------------------------------------------------------------------- *)
(* Unlink actor → new empty Principal                                         *)
(* -------------------------------------------------------------------------- *)

let test_unlink_actor_creates_empty_principal () =
  with_db @@ fun db ->
  let source =
    insert_principal ~db ~id:"prin_src" ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let k_keep = key ~user:"keep" () in
  let k_split = key ~connector:P.Slack ~tenant:"ws" ~user:"split" () in
  ignore
    (seed_owned_actor ~db ~principal_id:source.id ~key:k_keep
       ~link_id:"link_keep" ());
  ignore
    (seed_owned_actor ~db ~principal_id:source.id ~key:k_split
       ~link_id:"link_split" ());
  ignore
    (assert_ok
       (M.put_preference ~db ~now:fixed_now ~principal_id:source.id ~key:"theme"
          ~value:"dark" ()));
  ignore
    (assert_ok
       (M.put_external_account ~db ~now:fixed_now
          {
            M.id = "acc_src";
            principal_id = source.id;
            account_kind = "github";
            uniqueness_domain = "github.com:app:1";
            account_identity = "42";
            exclusive_slot = true;
            revision = 1;
            payload_json = "{}";
            created_at = "";
            updated_at = "";
          }));
  ignore
    (assert_ok
       (M.set_pending_authorization_count ~db ~principal_id:source.id ~count:3));
  ignore
    (assert_ok
       (U.put_account_lease ~db ~now:fixed_now
          {
            U.id = "lease_actor";
            principal_id = source.id;
            account_id = Some "acc_src";
            actor_key = Some (P.actor_identity_key k_split);
            status = U.Active;
            revision = 1;
            created_at = "";
            updated_at = "";
          }));
  ignore
    (assert_ok
       (U.put_account_lease ~db ~now:fixed_now
          {
            U.id = "lease_principal";
            principal_id = source.id;
            account_id = Some "acc_src";
            actor_key = None;
            status = U.Active;
            revision = 1;
            created_at = "";
            updated_at = "";
          }));
  match
    U.unlink_actor ~db ~source_principal_id:source.id ~actor_key:k_split
      ~plan_id:"psplit_happy" ~unlink_id:"punlink_happy" ~now:fixed_now ()
  with
  | U.Applied receipt -> (
      Alcotest.(check string)
        "source" "prin_src"
        (P.principal_id_to_string receipt.source_principal_id);
      Alcotest.(check bool)
        "new principal distinct" true
        (not
           (P.principal_id_equal receipt.source_principal_id
              receipt.new_principal_id));
      Alcotest.(check int)
        "pending invalidated" 3 receipt.pending_auth_invalidated;
      Alcotest.(check int) "leases touched" 2 receipt.leases_invalidated;
      Alcotest.(check (list string))
        "no auto account rebind" [] receipt.rebound_account_ids;
      Alcotest.(check (list string))
        "no auto pref rebind" [] receipt.rebound_preference_keys;
      (* Source keeps accounts and prefs. *)
      let accounts =
        assert_ok (M.list_external_accounts ~db ~principal_id:source.id)
      in
      Alcotest.(check int) "account stayed" 1 (List.length accounts);
      let prefs = assert_ok (M.list_preferences ~db ~principal_id:source.id) in
      Alcotest.(check int) "pref stayed" 1 (List.length prefs);
      (* New principal empty. *)
      let new_accs =
        assert_ok
          (M.list_external_accounts ~db ~principal_id:receipt.new_principal_id)
      in
      let new_prefs =
        assert_ok
          (M.list_preferences ~db ~principal_id:receipt.new_principal_id)
      in
      Alcotest.(check int) "new empty accounts" 0 (List.length new_accs);
      Alcotest.(check int) "new empty prefs" 0 (List.length new_prefs);
      (* Actor moved. *)
      (match S.get_connector_actor ~db ~key:k_split with
      | Ok (Some a) ->
          Alcotest.(check string)
            "actor on new"
            (P.principal_id_to_string receipt.new_principal_id)
            (P.principal_id_to_string a.principal_id)
      | _ -> Alcotest.fail "missing split actor");
      (* Keep actor still on source. *)
      (match S.get_connector_actor ~db ~key:k_keep with
      | Ok (Some a) ->
          Alcotest.(check string)
            "keep actor" "prin_src"
            (P.principal_id_to_string a.principal_id)
      | _ -> Alcotest.fail "missing keep actor");
      (* Old link unlinked; new link active. *)
      (match S.get_identity_link ~db ~id:"link_split" with
      | Ok (Some l) -> (
          match l.status with
          | P.Unlinked -> ()
          | _ -> Alcotest.fail "expected Unlinked status")
      | _ -> Alcotest.fail "missing old link");
      (match S.get_active_identity_link ~db ~key:k_split with
      | Ok (Some l) ->
          Alcotest.(check string)
            "active on new"
            (P.principal_id_to_string receipt.new_principal_id)
            (P.principal_id_to_string l.principal_id)
      | _ -> Alcotest.fail "missing active link");
      (* Leases: actor-scoped invalidated; principal-scoped rebind_required. *)
      let leases =
        assert_ok (U.list_account_leases ~db ~principal_id:source.id)
      in
      let lease_actor =
        List.find (fun (l : U.account_lease) -> l.id = "lease_actor") leases
      in
      let lease_prin =
        List.find (fun (l : U.account_lease) -> l.id = "lease_principal") leases
      in
      (match lease_actor.status with
      | U.Invalidated -> ()
      | _ -> Alcotest.fail "actor lease should be invalidated");
      (match lease_prin.status with
      | U.Rebind_required -> ()
      | _ -> Alcotest.fail "principal lease should require rebind");
      (* Pending zeroed on source. *)
      Alcotest.(check int)
        "pending zero" 0
        (assert_ok
           (M.get_pending_authorization_count ~db ~principal_id:source.id));
      (* Live resolve follows new principal. *)
      (match R.resolve_or_create ~db ~actor_key:k_split ~now:fixed_now () with
      | Ok id ->
          Alcotest.(check string)
            "resolve new"
            (P.principal_id_to_string receipt.new_principal_id)
            (P.principal_id_to_string id)
      | Error e -> Alcotest.fail e);
      (* Idempotent replay. *)
      match
        U.unlink_actor ~db ~source_principal_id:source.id ~actor_key:k_split
          ~plan_id:"psplit_happy" ~now:(fixed_now +. 1.) ()
      with
      | U.Idempotent r ->
          Alcotest.(check string) "same receipt" "punlink_happy" r.id
      | other -> Alcotest.fail ("expected idempotent, got " ^ status_msg other))
  | other -> Alcotest.fail (status_msg other)

(* -------------------------------------------------------------------------- *)
(* Reverse merge refused                                                      *)
(* -------------------------------------------------------------------------- *)

let test_reverse_merge_refused () =
  with_db @@ fun db ->
  let survivor =
    insert_principal ~db ~id:"prin_surv" ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let loser =
    insert_principal ~db ~id:"prin_lose" ~created_at:"2026-02-01T00:00:00Z" ()
  in
  ignore
    (seed_owned_actor ~db ~principal_id:survivor.id ~key:(key ~user:"s" ())
       ~link_id:"link_s" ());
  ignore
    (seed_owned_actor ~db ~principal_id:loser.id
       ~key:(key ~connector:P.Slack ~tenant:"w" ~user:"l" ())
       ~link_id:"link_l" ());
  (match
     M.apply_merge ~db ~left_id:survivor.id ~right_id:loser.id ~now:fixed_now ()
   with
  | M.Applied _ -> ()
  | M.Refused { reason; _ } -> Alcotest.fail reason
  | _ -> Alcotest.fail "merge should apply");
  match
    U.refuse_reverse_merge ~db ~survivor_id:survivor.id ~loser_id:loser.id
      ~now:fixed_now ()
  with
  | U.Refused { conflicts; reason; _ } -> (
      Alcotest.(check bool)
        "mentions reverse" true
        (Test_helpers.string_contains (String.lowercase_ascii reason) "reverse");
      match conflicts with
      | U.Reverse_merge_forbidden _ :: _ -> ()
      | _ -> Alcotest.fail "expected Reverse_merge_forbidden")
  | other -> Alcotest.fail ("expected refuse, got " ^ status_msg other)

(* -------------------------------------------------------------------------- *)
(* Explicit revision-bound split plan (admin)                                 *)
(* -------------------------------------------------------------------------- *)

let test_admin_split_plan_confirm_apply () =
  with_db @@ fun db ->
  let source =
    insert_principal ~db ~id:"prin_adm" ~created_at:"2026-01-01T00:00:00Z"
      ~revision:2 ()
  in
  let admin =
    insert_principal ~db ~id:"prin_admin" ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let k = key ~connector:P.Discord ~tenant:"g" ~user:"adm-user" () in
  ignore
    (seed_owned_actor ~db ~principal_id:source.id ~key:k ~link_id:"link_adm" ());
  let plan =
    assert_ok
      (U.make_split_plan ~db ~id:"psplit_admin" ~source_principal_id:source.id
         ~actor_key:k ~admin_principal_id:admin.id ~now:fixed_now ())
  in
  Alcotest.(check string)
    "planned" "planned"
    (U.string_of_plan_status plan.status);
  Alcotest.(check int) "bound source rev" 2 plan.source_revision;
  Alcotest.(check int) "bound actor rev" 1 plan.actor_revision;
  (* Wrong digest fails. *)
  (match
     U.confirm_split_plan ~db ~id:plan.id ~presented_digest:"deadbeef"
       ~confirming_principal:admin.id ~now:fixed_now ()
   with
  | Error msg ->
      Alcotest.(check bool)
        "digest" true
        (Test_helpers.string_contains (String.lowercase_ascii msg) "digest")
  | Ok _ -> Alcotest.fail "bad digest should fail");
  (* Wrong admin fails. *)
  (match
     U.confirm_split_plan ~db ~id:plan.id ~presented_digest:plan.digest
       ~confirming_principal:source.id ~now:fixed_now ()
   with
  | Error msg ->
      Alcotest.(check bool)
        "admin" true
        (Test_helpers.string_contains (String.lowercase_ascii msg) "admin")
  | Ok _ -> Alcotest.fail "wrong admin should fail");
  let confirmed =
    assert_ok
      (U.confirm_split_plan ~db ~id:plan.id ~presented_digest:plan.digest
         ~confirming_principal:admin.id ~now:fixed_now ())
  in
  Alcotest.(check string)
    "confirmed" "confirmed"
    (U.string_of_plan_status confirmed.status);
  match
    U.apply_split_plan ~db ~id:plan.id ~expected_source_revision:2
      ~expected_actor_revision:1 ~now:fixed_now ()
  with
  | U.Applied r -> (
      Alcotest.(check string) "plan id" "psplit_admin" r.plan_id;
      (* Concurrent/idempotent apply. *)
      match U.apply_split_plan ~db ~id:plan.id ~now:(fixed_now +. 1.) () with
      | U.Idempotent r2 -> Alcotest.(check string) "same receipt" r.id r2.id
      | other -> Alcotest.fail ("expected idempotent " ^ status_msg other))
  | other -> Alcotest.fail (status_msg other)

(* -------------------------------------------------------------------------- *)
(* Ownership conflicts                                                        *)
(* -------------------------------------------------------------------------- *)

let test_ownership_conflict_refuse () =
  with_db @@ fun db ->
  let source =
    insert_principal ~db ~id:"prin_own" ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let k = key ~user:"own" () in
  ignore
    (seed_owned_actor ~db ~principal_id:source.id ~key:k ~link_id:"link_own" ());
  match
    U.unlink_actor ~db ~source_principal_id:source.id ~actor_key:k
      ~ownership:
        (U.Explicit_rebind
           { account_ids = [ "missing_acc" ]; preference_keys = [ "nope" ] })
      ~now:fixed_now ()
  with
  | U.Refused { reason; _ } ->
      Alcotest.(check bool)
        "conflict" true
        (Test_helpers.string_contains
           (String.lowercase_ascii reason)
           "conflict")
  | other -> Alcotest.fail ("expected refuse " ^ status_msg other)

let test_explicit_rebind_moves_named_only () =
  with_db @@ fun db ->
  let source =
    insert_principal ~db ~id:"prin_rb" ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let k = key ~connector:P.Web ~tenant:"iss" ~user:"rb" () in
  ignore
    (seed_owned_actor ~db ~principal_id:source.id ~key:k ~link_id:"link_rb" ());
  ignore
    (assert_ok
       (M.put_external_account ~db ~now:fixed_now
          {
            M.id = "acc_move";
            principal_id = source.id;
            account_kind = "github";
            uniqueness_domain = "github.com:app:9";
            account_identity = "77";
            exclusive_slot = true;
            revision = 1;
            payload_json = "{}";
            created_at = "";
            updated_at = "";
          }));
  ignore
    (assert_ok
       (M.put_external_account ~db ~now:fixed_now
          {
            M.id = "acc_stay";
            principal_id = source.id;
            account_kind = "github";
            uniqueness_domain = "github.com:app:10";
            account_identity = "88";
            exclusive_slot = true;
            revision = 1;
            payload_json = "{}";
            created_at = "";
            updated_at = "";
          }));
  ignore
    (assert_ok
       (M.put_preference ~db ~now:fixed_now ~principal_id:source.id
          ~key:"locale" ~value:"en" ()));
  ignore
    (assert_ok
       (M.put_preference ~db ~now:fixed_now ~principal_id:source.id ~key:"theme"
          ~value:"light" ()));
  match
    U.unlink_actor ~db ~source_principal_id:source.id ~actor_key:k
      ~ownership:
        (U.Explicit_rebind
           { account_ids = [ "acc_move" ]; preference_keys = [ "locale" ] })
      ~plan_id:"psplit_rb" ~now:fixed_now ()
  with
  | U.Applied r ->
      Alcotest.(check (list string))
        "moved acc" [ "acc_move" ] r.rebound_account_ids;
      Alcotest.(check (list string))
        "moved pref" [ "locale" ] r.rebound_preference_keys;
      let src_acc =
        assert_ok (M.list_external_accounts ~db ~principal_id:source.id)
      in
      let new_acc =
        assert_ok
          (M.list_external_accounts ~db ~principal_id:r.new_principal_id)
      in
      Alcotest.(check int) "source one acc" 1 (List.length src_acc);
      Alcotest.(check string)
        "source keeps stay" "acc_stay" (List.hd src_acc).M.id;
      Alcotest.(check int) "new one acc" 1 (List.length new_acc);
      Alcotest.(check string) "new has move" "acc_move" (List.hd new_acc).M.id;
      let src_prefs =
        assert_ok (M.list_preferences ~db ~principal_id:source.id)
      in
      let new_prefs =
        assert_ok (M.list_preferences ~db ~principal_id:r.new_principal_id)
      in
      Alcotest.(check bool)
        "source theme" true
        (List.exists (fun (p : M.preference) -> p.key = "theme") src_prefs);
      Alcotest.(check bool)
        "new locale" true
        (List.exists (fun (p : M.preference) -> p.key = "locale") new_prefs)
  | other -> Alcotest.fail (status_msg other)

(* -------------------------------------------------------------------------- *)
(* Concurrent CAS                                                             *)
(* -------------------------------------------------------------------------- *)

let test_concurrent_cas_stale_revision () =
  with_db @@ fun db ->
  let source =
    insert_principal ~db ~id:"prin_cas" ~created_at:"2026-01-01T00:00:00Z"
      ~revision:4 ()
  in
  let k = key ~connector:P.Telegram ~tenant:"bot" ~user:"cas" () in
  ignore
    (seed_owned_actor ~db ~principal_id:source.id ~key:k ~link_id:"link_cas" ());
  (match
     U.unlink_actor ~db ~source_principal_id:source.id ~actor_key:k
       ~expected_source_revision:1 (* stale *)
       ~now:fixed_now ()
   with
  | U.Stale_revision msg ->
      Alcotest.(check bool)
        "mentions revision" true
        (Test_helpers.string_contains (String.lowercase_ascii msg) "revision")
  | other -> Alcotest.fail ("expected stale " ^ status_msg other));
  match
    U.unlink_actor ~db ~source_principal_id:source.id ~actor_key:k
      ~expected_source_revision:4 ~expected_actor_revision:1
      ~plan_id:"psplit_cas" ~now:fixed_now ()
  with
  | U.Applied _ -> ()
  | other -> Alcotest.fail (status_msg other)

(* -------------------------------------------------------------------------- *)
(* History immutable                                                          *)
(* -------------------------------------------------------------------------- *)

let test_history_snapshots_immutable () =
  with_db @@ fun db ->
  let source =
    insert_principal ~db ~id:"prin_hist" ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let k = key ~connector:P.Cli ~tenant:"dev" ~user:"hist" () in
  let actor, _ =
    seed_owned_actor ~db ~principal_id:source.id ~key:k ~link_id:"link_hist" ()
  in
  let receipt =
    match
      U.unlink_actor ~db ~source_principal_id:source.id ~actor_key:k
        ~plan_id:"psplit_hist" ~now:fixed_now ()
    with
    | U.Applied r -> r
    | other -> Alcotest.fail (status_msg other)
  in
  Alcotest.(check bool) "snapshot ids" true (receipt.actor_snapshot_ids <> []);
  let snaps =
    assert_ok
      (M.list_actor_snapshots_for_actor ~db ~actor_key:(P.actor_identity_key k))
  in
  Alcotest.(check bool) "has snap" true (snaps <> []);
  let snap = List.hd snaps in
  Alcotest.(check string)
    "original principal" "prin_hist"
    (P.principal_id_to_string snap.principal_id_at_snapshot);
  Alcotest.(check string) "reason" "pre_unlink" snap.reason;
  (* Snapshot JSON still shows original owner. *)
  (match
     P.connector_actor_of_json (Yojson.Safe.from_string snap.actor_json)
   with
  | Ok a ->
      Alcotest.(check string)
        "json principal" "prin_hist"
        (P.principal_id_to_string a.principal_id);
      Alcotest.(check string)
        "json key"
        (P.actor_identity_key actor.key)
        (P.actor_identity_key a.key)
  | Error e -> Alcotest.fail e);
  (* Live actor moved; snapshot row unchanged if re-read. *)
  match S.get_connector_actor ~db ~key:k with
  | Ok (Some live) ->
      Alcotest.(check string)
        "live new"
        (P.principal_id_to_string receipt.new_principal_id)
        (P.principal_id_to_string live.principal_id);
      let snaps2 =
        assert_ok
          (M.list_actor_snapshots_for_actor ~db
             ~actor_key:(P.actor_identity_key k))
      in
      Alcotest.(check string)
        "snapshot still original" "prin_hist"
        (P.principal_id_to_string (List.hd snaps2).principal_id_at_snapshot)
  | _ -> Alcotest.fail "missing live actor"

(* -------------------------------------------------------------------------- *)
(* Source retains state with zero actors                                      *)
(* -------------------------------------------------------------------------- *)

let test_source_retains_state_when_last_actor_unlinked () =
  with_db @@ fun db ->
  let source =
    insert_principal ~db ~id:"prin_last" ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let k = key ~user:"last" () in
  ignore
    (seed_owned_actor ~db ~principal_id:source.id ~key:k ~link_id:"link_last" ());
  ignore
    (assert_ok
       (M.put_preference ~db ~now:fixed_now ~principal_id:source.id ~key:"theme"
          ~value:"dark" ()));
  match
    U.unlink_actor ~db ~source_principal_id:source.id ~actor_key:k
      ~now:fixed_now ()
  with
  | U.Applied _ -> (
      let actors =
        assert_ok
          (S.list_connector_actors_for_principal ~db ~principal_id:source.id)
      in
      Alcotest.(check int) "no actors left" 0 (List.length actors);
      let prefs = assert_ok (M.list_preferences ~db ~principal_id:source.id) in
      Alcotest.(check int) "prefs retained" 1 (List.length prefs);
      match S.get_principal ~db ~id:source.id with
      | Ok (Some p) ->
          Alcotest.(check bool) "still active" true (P.principal_is_active p)
      | _ -> Alcotest.fail "source missing")
  | other -> Alcotest.fail (status_msg other)

(* -------------------------------------------------------------------------- *)
(* Canonical invalidation is transactional                                    *)
(* -------------------------------------------------------------------------- *)

let test_split_rolls_back_when_invalidation_receipt_fails () =
  with_db @@ fun db ->
  let source =
    insert_principal ~db ~id:"prin_invalidation_tx"
      ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let k = key ~connector:P.Slack ~tenant:"ws" ~user:"invalidation-tx" () in
  ignore
    (seed_owned_actor ~db ~principal_id:source.id ~key:k
       ~link_id:"link_invalidation_tx" ());
  ignore
    (assert_ok
       (M.set_pending_authorization_count ~db ~principal_id:source.id ~count:2));
  Github_user_auth_invalidate.ensure_schema db;
  (match
     Sqlite3.exec db
       {|CREATE TRIGGER reject_connector_split_invalidation_receipt
           BEFORE INSERT ON github_user_auth_invalidate_receipts
           BEGIN
             SELECT RAISE(ABORT, 'test invalidation receipt failure');
           END|}
   with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      Alcotest.fail
        ("could not install invalidation failure trigger: "
       ^ Sqlite3.Rc.to_string rc));
  (match
     U.unlink_actor ~db ~source_principal_id:source.id ~actor_key:k
       ~plan_id:"psplit_invalidation_tx" ~now:fixed_now ()
   with
  | U.Refused { reason; _ } ->
      Alcotest.(check bool)
        "propagates invalidation storage failure" true
        (Test_helpers.string_contains
           (String.lowercase_ascii reason)
           "invalidation")
  | other -> Alcotest.fail ("expected refusal, got " ^ status_msg other));
  (match S.get_connector_actor ~db ~key:k with
  | Ok (Some actor) ->
      Alcotest.(check string)
        "actor ownership rolled back" "prin_invalidation_tx"
        (P.principal_id_to_string actor.principal_id)
  | _ -> Alcotest.fail "actor missing after failed split");
  (match S.get_active_identity_link ~db ~key:k with
  | Ok (Some link) ->
      Alcotest.(check string)
        "identity link rolled back" "prin_invalidation_tx"
        (P.principal_id_to_string link.principal_id)
  | _ -> Alcotest.fail "active link was not rolled back");
  Alcotest.(check int)
    "pending authorization rollback" 2
    (assert_ok (M.get_pending_authorization_count ~db ~principal_id:source.id))

let suite =
  [
    ( "unlink actor creates empty principal revokes authority",
      `Quick,
      test_unlink_actor_creates_empty_principal );
    ("reverse merge refused", `Quick, test_reverse_merge_refused);
    ( "admin split plan confirm apply idempotent",
      `Quick,
      test_admin_split_plan_confirm_apply );
    ("ownership conflict refuse", `Quick, test_ownership_conflict_refuse);
    ( "explicit rebind moves named only",
      `Quick,
      test_explicit_rebind_moves_named_only );
    ("concurrent CAS stale revision", `Quick, test_concurrent_cas_stale_revision);
    ("history snapshots immutable", `Quick, test_history_snapshots_immutable);
    ( "source retains state when last actor unlinked",
      `Quick,
      test_source_retains_state_when_last_actor_unlinked );
    ( "split rolls back when canonical invalidation fails",
      `Quick,
      test_split_rolls_back_when_invalidation_receipt_fails );
  ]

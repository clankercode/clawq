(** Tests for deterministic Principal merge / adoption (P21.M1.E1.T011). *)

module P = Principal_identity
module S = Principal_identity_store
module M = Principal_merge
module L = Principal_link_protocol
module R = Principal_resolve

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  M.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_785_000_000.0
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

(* -------------------------------------------------------------------------- *)
(* Survivor rule                                                              *)
(* -------------------------------------------------------------------------- *)

let test_survivor_rule_created_at_then_id () =
  let older =
    P.make_principal ~id:(pid "prin_b") ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let newer =
    P.make_principal ~id:(pid "prin_a") ~created_at:"2026-06-01T00:00:00Z" ()
  in
  let survivor, loser =
    assert_ok (M.select_survivor ~left:newer ~right:older ())
  in
  Alcotest.(check string)
    "older survives" "prin_b"
    (P.principal_id_to_string survivor.id);
  Alcotest.(check string)
    "newer loses" "prin_a"
    (P.principal_id_to_string loser.id);
  (* Exact created_at tie → lexicographic principal_id. *)
  let p1 =
    P.make_principal ~id:(pid "prin_aaa") ~created_at:"2026-03-01T00:00:00Z" ()
  in
  let p2 =
    P.make_principal ~id:(pid "prin_zzz") ~created_at:"2026-03-01T00:00:00Z" ()
  in
  let survivor2, loser2 = assert_ok (M.select_survivor ~left:p2 ~right:p1 ()) in
  Alcotest.(check string)
    "lex smaller id" "prin_aaa"
    (P.principal_id_to_string survivor2.id);
  Alcotest.(check string)
    "lex larger loses" "prin_zzz"
    (P.principal_id_to_string loser2.id);
  (* Explicit override. *)
  let survivor3, _ =
    assert_ok
      (M.select_survivor ~left:older ~right:newer
         ~selection:(L.Explicit (pid "prin_a"))
         ())
  in
  Alcotest.(check string)
    "explicit" "prin_a"
    (P.principal_id_to_string survivor3.id)

(* -------------------------------------------------------------------------- *)
(* Happy merge                                                                *)
(* -------------------------------------------------------------------------- *)

let test_happy_merge_adopts_links_and_tombstones () =
  with_db @@ fun db ->
  let older =
    insert_principal ~db ~id:"prin_old" ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let newer =
    insert_principal ~db ~id:"prin_new" ~created_at:"2026-06-01T00:00:00Z" ()
  in
  let key_old = key ~connector:P.Teams ~user:"u-old" () in
  let key_new = key ~connector:P.Slack ~tenant:"ws-b" ~user:"u-new" () in
  ignore
    (seed_owned_actor ~db ~principal_id:older.id ~key:key_old
       ~link_id:"link_old" ());
  ignore
    (seed_owned_actor ~db ~principal_id:newer.id ~key:key_new
       ~link_id:"link_new" ());
  ignore
    (assert_ok
       (M.put_preference ~db ~now:fixed_now ~principal_id:newer.id ~key:"theme"
          ~value:"dark" ()));
  ignore
    (assert_ok
       (M.put_preference ~db ~now:fixed_now ~principal_id:older.id ~key:"theme"
          ~value:"light" ()));
  ignore
    (assert_ok
       (M.put_preference ~db ~now:fixed_now ~principal_id:newer.id ~key:"locale"
          ~value:"en" ()));
  ignore
    (assert_ok
       (M.set_pending_authorization_count ~db ~principal_id:newer.id ~count:2));
  (* Survivor already has one exclusive-slot binding. *)
  ignore
    (assert_ok
       (M.put_external_account ~db ~now:fixed_now
          {
            M.id = "acc_old";
            principal_id = older.id;
            account_kind = "github";
            uniqueness_domain = "github.com:app:1";
            account_identity = "42";
            exclusive_slot = true;
            revision = 1;
            payload_json = "{}";
            created_at = "";
            updated_at = "";
          }));
  (* Non-conflicting extra exclusive account on loser (different domain). *)
  ignore
    (assert_ok
       (M.put_external_account ~db ~now:fixed_now
          {
            M.id = "acc_extra";
            principal_id = newer.id;
            account_kind = "github";
            uniqueness_domain = "github.com:app:2";
            account_identity = "99";
            exclusive_slot = true;
            revision = 1;
            payload_json = "{}";
            created_at = "";
            updated_at = "";
          }));
  match
    M.apply_merge ~db ~left_id:older.id ~right_id:newer.id
      ~link_tx_id:"ltx_happy" ~merge_id:"pmerge_happy" ~now:fixed_now ()
  with
  | M.Applied receipt -> (
      Alcotest.(check string)
        "survivor" "prin_old"
        (P.principal_id_to_string receipt.survivor_id);
      Alcotest.(check string)
        "loser" "prin_new"
        (P.principal_id_to_string receipt.loser_id);
      Alcotest.(check bool)
        "adopted loser actor" true
        (List.exists
           (String.equal (P.actor_identity_key key_new))
           receipt.adopted_actor_keys);
      Alcotest.(check bool)
        "adopted link" true
        (List.exists (String.equal "link_new") receipt.adopted_link_ids);
      Alcotest.(check int)
        "pending invalidated" 2 receipt.pending_auth_invalidated;
      (* Tombstone. *)
      (match S.get_principal ~db ~id:newer.id with
      | Ok (Some p) -> (
          match p.lifecycle with
          | P.Merged_into t ->
              Alcotest.(check string)
                "merged_into survivor" "prin_old"
                (P.principal_id_to_string t);
              Alcotest.(check bool) "not active" false (P.principal_is_active p)
          | _ -> Alcotest.fail "expected Merged_into")
      | _ -> Alcotest.fail "missing loser principal");
      (* Actor reassigned. *)
      (match S.get_connector_actor ~db ~key:key_new with
      | Ok (Some a) ->
          Alcotest.(check string)
            "actor owner" "prin_old"
            (P.principal_id_to_string a.principal_id)
      | _ -> Alcotest.fail "missing actor");
      (* Active link reassigned. *)
      (match S.get_active_identity_link ~db ~key:key_new with
      | Ok (Some l) ->
          Alcotest.(check string)
            "link owner" "prin_old"
            (P.principal_id_to_string l.principal_id)
      | _ -> Alcotest.fail "missing active link");
      (* Preference: survivor theme kept; locale adopted. *)
      let prefs = assert_ok (M.list_preferences ~db ~principal_id:older.id) in
      let theme =
        List.find_opt (fun (p : M.preference) -> p.key = "theme") prefs
      in
      let locale =
        List.find_opt (fun (p : M.preference) -> p.key = "locale") prefs
      in
      Alcotest.(check (option string))
        "theme survivor" (Some "light")
        (Option.map (fun p -> p.M.value) theme);
      Alcotest.(check (option string))
        "locale adopted" (Some "en")
        (Option.map (fun p -> p.M.value) locale);
      (* Accounts on survivor: identity 42 once + extra 99. *)
      let accounts =
        assert_ok (M.list_external_accounts ~db ~principal_id:older.id)
      in
      Alcotest.(check int) "two accounts" 2 (List.length accounts);
      Alcotest.(check bool)
        "has 99" true
        (List.exists (fun a -> a.M.account_identity = "99") accounts);
      (* Resolve follows tombstone. *)
      (match R.resolve_or_create ~db ~actor_key:key_new ~now:fixed_now () with
      | Ok id ->
          Alcotest.(check string)
            "resolve live" "prin_old"
            (P.principal_id_to_string id)
      | Error e -> Alcotest.fail e);
      (* Idempotent replay by link_tx_id. *)
      match
        M.apply_merge ~db ~left_id:older.id ~right_id:newer.id
          ~link_tx_id:"ltx_happy" ~now:(fixed_now +. 1.) ()
      with
      | M.Idempotent r ->
          Alcotest.(check string) "same receipt" "pmerge_happy" r.id
      | other ->
          Alcotest.fail
            (match other with
            | M.Applied _ -> "expected idempotent, got Applied"
            | M.Refused { reason; _ } -> "refused: " ^ reason
            | M.Stale_revision s -> "stale: " ^ s
            | M.Idempotent _ -> "unreachable"))
  | M.Idempotent _ -> Alcotest.fail "unexpected idempotent"
  | M.Refused { reason; _ } -> Alcotest.fail ("refused: " ^ reason)
  | M.Stale_revision s -> Alcotest.fail ("stale: " ^ s)

(* -------------------------------------------------------------------------- *)
(* Conflict refusal                                                           *)
(* -------------------------------------------------------------------------- *)

let test_conflict_refusal_exclusive_accounts () =
  with_db @@ fun db ->
  let p1 =
    insert_principal ~db ~id:"prin_c1" ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let p2 =
    insert_principal ~db ~id:"prin_c2" ~created_at:"2026-02-01T00:00:00Z" ()
  in
  ignore
    (seed_owned_actor ~db ~principal_id:p1.id ~key:(key ~user:"c1" ())
       ~link_id:"link_c1" ());
  ignore
    (seed_owned_actor ~db ~principal_id:p2.id
       ~key:(key ~connector:P.Slack ~tenant:"ws" ~user:"c2" ())
       ~link_id:"link_c2" ());
  ignore
    (assert_ok
       (M.put_external_account ~db ~now:fixed_now
          {
            M.id = "acc1";
            principal_id = p1.id;
            account_kind = "github";
            uniqueness_domain = "github.com:app:7";
            account_identity = "100";
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
            M.id = "acc2";
            principal_id = p2.id;
            account_kind = "github";
            uniqueness_domain = "github.com:app:7";
            account_identity = "200";
            exclusive_slot = true;
            revision = 1;
            payload_json = "{}";
            created_at = "";
            updated_at = "";
          }));
  match M.apply_merge ~db ~left_id:p1.id ~right_id:p2.id ~now:fixed_now () with
  | M.Refused { conflicts; preview; _ } ->
      Alcotest.(check bool) "has conflict" true (conflicts <> []);
      (match conflicts with
      | M.External_account_collision { uniqueness_domain; _ } :: _ ->
          Alcotest.(check string) "domain" "github.com:app:7" uniqueness_domain
      | _ -> Alcotest.fail "expected external_account_collision");
      (* No partial adoption: both still active. *)
      (match S.get_principal ~db ~id:p2.id with
      | Ok (Some p) ->
          Alcotest.(check bool) "still active" true (P.principal_is_active p)
      | _ -> Alcotest.fail "missing p2");
      (match S.get_connector_actor ~db ~key:(key ~user:"c1" ()) with
      | Ok (Some a) ->
          Alcotest.(check string)
            "actor still on p1" "prin_c1"
            (P.principal_id_to_string a.principal_id)
      | _ -> Alcotest.fail "missing actor");
      Alcotest.(check bool) "preview present" true (Option.is_some preview)
  | M.Applied _ -> Alcotest.fail "should refuse"
  | M.Idempotent _ -> Alcotest.fail "should refuse"
  | M.Stale_revision s -> Alcotest.fail s

(* -------------------------------------------------------------------------- *)
(* Concurrent / CAS                                                           *)
(* -------------------------------------------------------------------------- *)

let test_concurrent_cas_stale_revision () =
  with_db @@ fun db ->
  let p1 =
    insert_principal ~db ~id:"prin_cas1" ~created_at:"2026-01-01T00:00:00Z"
      ~revision:3 ()
  in
  let p2 =
    insert_principal ~db ~id:"prin_cas2" ~created_at:"2026-02-01T00:00:00Z"
      ~revision:5 ()
  in
  ignore
    (seed_owned_actor ~db ~principal_id:p1.id ~key:(key ~user:"cas1" ())
       ~link_id:"link_cas1" ());
  ignore
    (seed_owned_actor ~db ~principal_id:p2.id
       ~key:(key ~connector:P.Discord ~tenant:"g" ~user:"cas2" ())
       ~link_id:"link_cas2" ());
  (match
     M.apply_merge ~db ~left_id:p1.id ~right_id:p2.id
       ~expected_left_revision:1 (* stale *)
       ~expected_right_revision:5 ~now:fixed_now ()
   with
  | M.Stale_revision msg ->
      Alcotest.(check bool)
        "mentions revision" true
        (Test_helpers.string_contains (String.lowercase_ascii msg) "revision")
  | M.Applied _ -> Alcotest.fail "stale should not apply"
  | M.Idempotent _ -> Alcotest.fail "stale should not be idempotent"
  | M.Refused { reason; _ } -> Alcotest.fail ("refused: " ^ reason));
  (* Fresh CAS succeeds. *)
  match
    M.apply_merge ~db ~left_id:p1.id ~right_id:p2.id ~expected_left_revision:3
      ~expected_right_revision:5 ~link_tx_id:"ltx_cas" ~now:fixed_now ()
  with
  | M.Applied r -> (
      Alcotest.(check string)
        "survivor" "prin_cas1"
        (P.principal_id_to_string r.survivor_id);
      (* Concurrent second apply is idempotent via link_tx. *)
      match
        M.apply_merge ~db ~left_id:p1.id ~right_id:p2.id ~link_tx_id:"ltx_cas"
          ~now:(fixed_now +. 1.) ()
      with
      | M.Idempotent _ -> ()
      | M.Applied _ -> Alcotest.fail "should be idempotent by link_tx"
      | M.Refused { reason; _ } -> Alcotest.fail reason
      | M.Stale_revision _ -> () (* also acceptable under concurrent roots *))
  | other ->
      Alcotest.fail
        (match other with
        | M.Refused { reason; _ } -> reason
        | M.Stale_revision s -> s
        | M.Idempotent _ -> "idempotent"
        | M.Applied _ -> "applied")

(* -------------------------------------------------------------------------- *)
(* Tombstone redirect                                                         *)
(* -------------------------------------------------------------------------- *)

let test_tombstone_redirect () =
  with_db @@ fun db ->
  let p1 =
    insert_principal ~db ~id:"prin_t1" ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let p2 =
    insert_principal ~db ~id:"prin_t2" ~created_at:"2026-03-01T00:00:00Z" ()
  in
  let k2 = key ~connector:P.Telegram ~tenant:"bot" ~user:"t2" () in
  ignore
    (seed_owned_actor ~db ~principal_id:p1.id ~key:(key ~user:"t1" ())
       ~link_id:"link_t1" ());
  ignore
    (seed_owned_actor ~db ~principal_id:p2.id ~key:k2 ~link_id:"link_t2" ());
  (match M.apply_merge ~db ~left_id:p1.id ~right_id:p2.id ~now:fixed_now () with
  | M.Applied _ -> ()
  | M.Refused { reason; _ } -> Alcotest.fail reason
  | M.Stale_revision s -> Alcotest.fail s
  | M.Idempotent _ -> Alcotest.fail "unexpected idempotent");
  match S.get_principal ~db ~id:p2.id with
  | Ok (Some loser) -> (
      match loser.lifecycle with
      | P.Merged_into survivor -> (
          Alcotest.(check string)
            "redirect target" "prin_t1"
            (P.principal_id_to_string survivor);
          (* Live resolve of loser's former actor follows tombstone. *)
          (match R.resolve_or_create ~db ~actor_key:k2 ~now:fixed_now () with
          | Ok id ->
              Alcotest.(check string)
                "live principal" "prin_t1"
                (P.principal_id_to_string id)
          | Error e -> Alcotest.fail e);
          (* Re-merge already-tombstoned pair is idempotent. *)
          match
            M.apply_merge ~db ~left_id:p1.id ~right_id:p2.id
              ~now:(fixed_now +. 2.) ()
          with
          | M.Idempotent _ -> ()
          | M.Applied _ -> Alcotest.fail "should be idempotent after tombstone"
          | M.Refused { reason; _ } -> Alcotest.fail reason
          | M.Stale_revision s -> Alcotest.fail s)
      | _ -> Alcotest.fail "expected tombstone")
  | _ -> Alcotest.fail "missing loser"

(* -------------------------------------------------------------------------- *)
(* History retained                                                           *)
(* -------------------------------------------------------------------------- *)

let test_history_actor_snapshots_retained () =
  with_db @@ fun db ->
  let p1 =
    insert_principal ~db ~id:"prin_h1" ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let p2 =
    insert_principal ~db ~id:"prin_h2" ~created_at:"2026-04-01T00:00:00Z" ()
  in
  let k2 = key ~connector:P.Web ~tenant:"issuer" ~user:"sub-h2" () in
  ignore
    (seed_owned_actor ~db ~principal_id:p1.id ~key:(key ~user:"h1" ())
       ~link_id:"link_h1" ());
  let actor2, _ =
    seed_owned_actor ~db ~principal_id:p2.id ~key:k2 ~link_id:"link_h2" ()
  in
  let receipt =
    match
      M.apply_merge ~db ~left_id:p1.id ~right_id:p2.id ~merge_id:"pmerge_hist"
        ~now:fixed_now ()
    with
    | M.Applied r -> r
    | M.Refused { reason; _ } -> Alcotest.fail reason
    | M.Stale_revision s -> Alcotest.fail s
    | M.Idempotent _ -> Alcotest.fail "unexpected idempotent"
  in
  Alcotest.(check bool)
    "snapshots recorded" true
    (receipt.actor_snapshot_ids <> []);
  let snaps =
    assert_ok
      (M.list_actor_snapshots_for_actor ~db ~actor_key:(P.actor_identity_key k2))
  in
  Alcotest.(check bool) "at least one snap" true (snaps <> []);
  let snap = List.hd snaps in
  Alcotest.(check string)
    "snapshot retains original principal" "prin_h2"
    (P.principal_id_to_string snap.principal_id_at_snapshot);
  Alcotest.(check string) "reason" "pre_merge" snap.reason;
  Alcotest.(check (option string)) "merge id" (Some "pmerge_hist") snap.merge_id;
  (* Snapshot JSON still shows original owner even though live actor moved. *)
  (match
     P.connector_actor_of_json (Yojson.Safe.from_string snap.actor_json)
   with
  | Ok a ->
      Alcotest.(check string)
        "json principal" "prin_h2"
        (P.principal_id_to_string a.principal_id);
      Alcotest.(check string)
        "json key"
        (P.actor_identity_key actor2.key)
        (P.actor_identity_key a.key)
  | Error e -> Alcotest.fail e);
  match S.get_connector_actor ~db ~key:k2 with
  | Ok (Some live) ->
      Alcotest.(check string)
        "live authority survivor" "prin_h1"
        (P.principal_id_to_string live.principal_id)
  | _ -> Alcotest.fail "missing live actor"

(* -------------------------------------------------------------------------- *)
(* adopt_after_verified_link                                                  *)
(* -------------------------------------------------------------------------- *)

let test_adopt_after_verified_link_paths () =
  with_db @@ fun db ->
  let p1 =
    insert_principal ~db ~id:"prin_v1" ~created_at:"2026-01-01T00:00:00Z" ()
  in
  let p2 =
    insert_principal ~db ~id:"prin_v2" ~created_at:"2026-05-01T00:00:00Z" ()
  in
  ignore
    (seed_owned_actor ~db ~principal_id:p1.id ~key:(key ~user:"v1" ())
       ~link_id:"link_v1" ());
  ignore
    (seed_owned_actor ~db ~principal_id:p2.id
       ~key:(key ~connector:P.Cli ~tenant:"dev" ~user:"v2" ())
       ~link_id:"link_v2" ());
  (match
     M.adopt_after_verified_link ~db ~principal_a:p1.id ~principal_b:p2.id
       ~link_tx_id:"ltx_v" ~now:fixed_now ()
   with
  | M.Applied r ->
      Alcotest.(check string)
        "survivor" "prin_v1"
        (P.principal_id_to_string r.survivor_id)
  | M.Refused { reason; _ } -> Alcotest.fail reason
  | M.Stale_revision s -> Alcotest.fail s
  | M.Idempotent _ -> Alcotest.fail "first should apply");
  (* Same principal both sides. *)
  (match
     M.adopt_after_verified_link ~db ~principal_a:p1.id ~principal_b:p1.id
       ~now:fixed_now ()
   with
  | M.Idempotent r ->
      Alcotest.(check bool)
        "note" true
        (List.exists
           (fun s -> Test_helpers.string_contains s "same Principal")
           r.notes)
  | _ -> Alcotest.fail "expected idempotent same-principal");
  (* Neither. *)
  match M.adopt_after_verified_link ~db ~now:fixed_now () with
  | M.Refused _ -> ()
  | _ -> Alcotest.fail "expected refuse neither"

let suite =
  [
    ( "survivor rule created_at then id",
      `Quick,
      test_survivor_rule_created_at_then_id );
    ( "happy merge adopts links tombstones prefs accounts",
      `Quick,
      test_happy_merge_adopts_links_and_tombstones );
    ( "conflict refusal exclusive accounts",
      `Quick,
      test_conflict_refusal_exclusive_accounts );
    ("concurrent CAS stale revision", `Quick, test_concurrent_cas_stale_revision);
    ("tombstone redirect", `Quick, test_tombstone_redirect);
    ( "history actor snapshots retained",
      `Quick,
      test_history_actor_snapshots_retained );
    ( "adopt_after_verified_link paths",
      `Quick,
      test_adopt_after_verified_link_paths );
  ]

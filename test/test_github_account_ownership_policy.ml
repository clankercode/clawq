(** Tests for verified ownership and duplicate-account policy (P21.M1.E2.T002).
*)

module P = Principal_identity
module S = Principal_identity_store
module B = Github_account_binding
module Op = Github_account_ownership_policy
module M = Principal_merge

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  B.ensure_schema db;
  M.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_785_200_000.0
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let pid s = assert_ok (P.principal_id_of_string s)

let seed_principal ~db ~id ?(created_at = "2026-01-01T00:00:00Z")
    ?(revision = 1) () =
  let p =
    P.make_principal ~id:(pid id) ~revision ~created_at ~updated_at:created_at
      ()
  in
  assert_ok (S.insert_principal ~db ~now:fixed_now p)

let sample_identity ?(host = B.default_host) ?(app_id = 42)
    ?(github_user_id = 9001L) () =
  assert_ok (B.make_account_identity ~host ~app_id ~github_user_id ())

let make_assertion ~principal_id ?(principal_revision = 1) ?identity
    ?(verified_at = "2026-07-13T00:00:00Z")
    ?(expires_at = "2026-07-13T01:00:00Z") () =
  let identity =
    match identity with Some i -> i | None -> sample_identity ()
  in
  assert_ok
    (Op.make_identity_assertion ~principal_id ~principal_revision ~identity
       ~verified_at ~expires_at ~now:fixed_now ())

let contains s sub =
  let n = String.length sub in
  let m = String.length s in
  let rec loop i =
    if i + n > m then false
    else if String.sub s i n = sub then true
    else loop (i + 1)
  in
  loop 0

(* -------------------------------------------------------------------------- *)
(* Assertion construction / expiry                                            *)
(* -------------------------------------------------------------------------- *)

let test_assertion_requires_verified_and_unexpired () =
  (match
     Op.make_identity_assertion ~principal_id:(pid "p1")
       ~identity:(sample_identity ()) ~verified_at:""
       ~expires_at:"2026-07-13T01:00:00Z" ()
   with
  | Ok _ -> Alcotest.fail "empty verified_at"
  | Error _ -> ());
  let a =
    make_assertion ~principal_id:(pid "p1") ~verified_at:"2026-07-13T00:00:00Z"
      ~expires_at:"2026-07-13T00:30:00Z" ()
  in
  (* now just after verified_at (2026-07-13T00:00:00Z), before expires_at
     2026-07-13T00:30:00Z. *)
  let now_inside = 1_783_891_200.0 +. 60. in
  Alcotest.(check bool)
    "unexpired while before expires_at" true
    (Op.assertion_is_unexpired ~now:now_inside a);
  Alcotest.(check bool)
    "expired after expires_at" false
    (Op.assertion_is_unexpired ~now:fixed_now a);
  match Op.validate_assertion ~now:fixed_now a with
  | Ok () -> Alcotest.fail "expected expired"
  | Error msg ->
      Alcotest.(check bool) "mentions expired" true (contains msg "expired")

let test_assertion_ttl_default () =
  let now = fixed_now in
  let verified_at = Time_util.iso8601_utc ~t:now () in
  let a =
    assert_ok
      (Op.make_identity_assertion ~principal_id:(pid "p1")
         ~identity:(sample_identity ()) ~verified_at ~now ())
  in
  Alcotest.(check bool)
    "unexpired just after create" true
    (Op.assertion_is_unexpired ~now a);
  Alcotest.(check bool)
    "expired after ttl" false
    (Op.assertion_is_unexpired
       ~now:(now +. Op.default_assertion_ttl_seconds +. 1.)
       a)

(* -------------------------------------------------------------------------- *)
(* Attach requires current Principal lineage                                  *)
(* -------------------------------------------------------------------------- *)

let test_attach_requires_active_principal () =
  with_db @@ fun db ->
  (* Missing principal *)
  let assertion =
    make_assertion ~principal_id:(pid "missing")
      ~expires_at:"2099-01-01T00:00:00Z" ()
  in
  (match Op.attach_account ~db ~assertion ~now:fixed_now () with
  | Op.Attached _ -> Alcotest.fail "missing principal should refuse"
  | Op.Refused { denial = Op.Principal_not_current _; _ } -> ()
  | Op.Refused { denial; _ } ->
      Alcotest.fail ("unexpected denial: " ^ Op.string_of_attach_denial denial));
  ignore (seed_principal ~db ~id:"prin_a" ());
  (* Tombstone cannot attach *)
  ignore
    (assert_ok
       (S.update_principal ~db ~id:(pid "prin_a")
          ~lifecycle:(P.Merged_into (pid "prin_survivor"))
          ~expected_revision:1 ~now:fixed_now ()));
  let assertion =
    make_assertion ~principal_id:(pid "prin_a") ~principal_revision:2
      ~expires_at:"2099-01-01T00:00:00Z" ()
  in
  match Op.attach_account ~db ~assertion ~now:fixed_now () with
  | Op.Attached _ -> Alcotest.fail "tombstone should refuse"
  | Op.Refused { denial = Op.Principal_not_current msg; _ } ->
      Alcotest.(check bool) "mentions merged" true (contains msg "merged_into")
  | Op.Refused { denial; _ } ->
      Alcotest.fail ("unexpected: " ^ Op.string_of_attach_denial denial)

let test_attach_cas_stale_principal_revision () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ~revision:3 ());
  let assertion =
    make_assertion ~principal_id:(pid "prin_a") ~principal_revision:1
      ~expires_at:"2099-01-01T00:00:00Z" ()
  in
  match Op.attach_account ~db ~assertion ~now:fixed_now () with
  | Op.Attached _ -> Alcotest.fail "stale revision should refuse"
  | Op.Refused
      { denial = Op.Principal_revision_conflict { expected; actual }; _ } ->
      Alcotest.(check int) "expected" 1 expected;
      Alcotest.(check int) "actual" 3 actual
  | Op.Refused { denial; _ } ->
      Alcotest.fail ("unexpected: " ^ Op.string_of_attach_denial denial)

let test_attach_success_and_audit () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  let audits = ref [] in
  let assertion =
    make_assertion ~principal_id:(pid "prin_a")
      ~expires_at:"2099-01-01T00:00:00Z" ()
  in
  match
    Op.attach_account ~db ~assertion ~now:fixed_now
      ~display:{ B.login = Some "octo"; avatar_url = None }
      ~id:"bind_ok" ~lineage_id:"lin_ok"
      ~audit_sink:(fun a -> audits := a :: !audits)
      ()
  with
  | Op.Refused { denial; _ } ->
      Alcotest.fail (Op.string_of_attach_denial denial)
  | Op.Attached { binding; audit; reassigned_from } -> (
      Alcotest.(check string) "id" "bind_ok" binding.id;
      Alcotest.(check string)
        "principal" "prin_a"
        (P.principal_id_to_string binding.principal_id);
      Alcotest.(check (option string))
        "login" (Some "octo") binding.display.login;
      Alcotest.(check bool) "no reassign" true (reassigned_from = None);
      Alcotest.(check string)
        "audit kind" "attach_succeeded"
        (Op.string_of_audit_kind audit.kind);
      Alcotest.(check int) "one audit emit" 1 (List.length !audits);
      (* Idempotent re-attach *)
      (match Op.attach_account ~db ~assertion ~now:fixed_now () with
      | Op.Attached { audit; _ } ->
          Alcotest.(check string)
            "idempotent" "attach_idempotent"
            (Op.string_of_audit_kind audit.kind)
      | Op.Refused { denial; _ } ->
          Alcotest.fail (Op.string_of_attach_denial denial));
      match B.get_by_identity ~db ~identity:assertion.identity with
      | Ok (Some b) -> Alcotest.(check string) "still one" "bind_ok" b.id
      | _ -> Alcotest.fail "identity lost")

(* -------------------------------------------------------------------------- *)
(* Duplicate ownership default refuse + admin exception                       *)
(* -------------------------------------------------------------------------- *)

let test_duplicate_ownership_refused_by_default () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  ignore (seed_principal ~db ~id:"prin_b" ());
  let identity = sample_identity ~github_user_id:42L () in
  let a_assert =
    make_assertion ~principal_id:(pid "prin_a") ~identity
      ~expires_at:"2099-01-01T00:00:00Z" ()
  in
  (match
     Op.attach_account ~db ~assertion:a_assert ~id:"b_a" ~now:fixed_now ()
   with
  | Op.Attached _ -> ()
  | Op.Refused { denial; _ } ->
      Alcotest.fail (Op.string_of_attach_denial denial));
  let b_assert =
    make_assertion ~principal_id:(pid "prin_b") ~identity
      ~expires_at:"2099-01-01T00:00:00Z" ()
  in
  match Op.attach_account ~db ~assertion:b_assert ~now:fixed_now () with
  | Op.Attached _ -> Alcotest.fail "duplicate should refuse"
  | Op.Refused
      {
        denial =
          Op.Duplicate_ownership { existing_binding_id; owner_principal_id; _ };
        audit;
      } ->
      Alcotest.(check string) "existing" "b_a" existing_binding_id;
      Alcotest.(check string)
        "owner" "prin_a"
        (P.principal_id_to_string owner_principal_id);
      Alcotest.(check string)
        "audit refused" "attach_refused"
        (Op.string_of_audit_kind audit.kind)
  | Op.Refused { denial; _ } ->
      Alcotest.fail ("unexpected: " ^ Op.string_of_attach_denial denial)

let test_admin_exception_reassigns_with_audit () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  ignore (seed_principal ~db ~id:"prin_b" ());
  ignore (seed_principal ~db ~id:"admin_1" ());
  let identity = sample_identity ~github_user_id:77L () in
  let a_assert =
    make_assertion ~principal_id:(pid "prin_a") ~identity
      ~expires_at:"2099-01-01T00:00:00Z" ()
  in
  ignore
    (match
       Op.attach_account ~db ~assertion:a_assert ~id:"b_own"
         ~lineage_id:"lin_77" ~now:fixed_now ()
     with
    | Op.Attached _ -> ()
    | Op.Refused { denial; _ } ->
        Alcotest.fail (Op.string_of_attach_denial denial));
  let b_assert =
    make_assertion ~principal_id:(pid "prin_b") ~identity
      ~expires_at:"2099-01-01T00:00:00Z" ()
  in
  let admin =
    assert_ok
      (Op.make_admin_exception ~admin_principal_id:(pid "admin_1")
         ~reason:"ticket-123 dual-account repair" ())
  in
  match Op.attach_account ~db ~assertion:b_assert ~admin ~now:fixed_now () with
  | Op.Refused { denial; _ } ->
      Alcotest.fail (Op.string_of_attach_denial denial)
  | Op.Attached { binding; audit; reassigned_from } ->
      Alcotest.(check string)
        "now owned by b" "prin_b"
        (P.principal_id_to_string binding.principal_id);
      Alcotest.(check string) "same lineage" "lin_77" binding.lineage_id;
      Alcotest.(check string) "same binding id" "b_own" binding.id;
      (match reassigned_from with
      | Some from ->
          Alcotest.(check string)
            "from a" "prin_a"
            (P.principal_id_to_string from)
      | None -> Alcotest.fail "expected reassigned_from");
      Alcotest.(check string)
        "admin reassign audit" "admin_exception_reassign"
        (Op.string_of_audit_kind audit.kind);
      Alcotest.(check (option string))
        "admin id" (Some "admin_1") audit.admin_principal_id;
      Alcotest.(check (option string))
        "reason" (Some "ticket-123 dual-account repair") audit.reason;
      (* Snapshot retained prior ownership evidence *)
      let snaps =
        assert_ok (B.list_snapshots_for_binding ~db ~binding_id:"b_own")
      in
      Alcotest.(check bool) "has snapshot" true (List.length snaps >= 1);
      let prior = List.hd snaps in
      Alcotest.(check string)
        "prior principal" "prin_a"
        (P.principal_id_to_string prior.principal_id_at_snapshot)

let test_admin_exception_without_reassign_flag_refuses () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  ignore (seed_principal ~db ~id:"prin_b" ());
  ignore (seed_principal ~db ~id:"admin_1" ());
  let identity = sample_identity ~github_user_id:88L () in
  ignore
    (match
       Op.attach_account ~db
         ~assertion:
           (make_assertion ~principal_id:(pid "prin_a") ~identity
              ~expires_at:"2099-01-01T00:00:00Z" ())
         ~id:"b_x" ~now:fixed_now ()
     with
    | Op.Attached _ -> ()
    | Op.Refused { denial; _ } ->
        Alcotest.fail (Op.string_of_attach_denial denial));
  let admin =
    assert_ok
      (Op.make_admin_exception ~admin_principal_id:(pid "admin_1")
         ~reason:"no reassign" ~allow_reassign:false ())
  in
  match
    Op.attach_account ~db
      ~assertion:
        (make_assertion ~principal_id:(pid "prin_b") ~identity
           ~expires_at:"2099-01-01T00:00:00Z" ())
      ~admin ~now:fixed_now ()
  with
  | Op.Attached _ -> Alcotest.fail "should refuse without allow_reassign"
  | Op.Refused { denial = Op.Duplicate_ownership _; _ } -> ()
  | Op.Refused { denial; _ } ->
      Alcotest.fail ("unexpected: " ^ Op.string_of_attach_denial denial)

(* -------------------------------------------------------------------------- *)
(* Merge / split conflict refuse                                              *)
(* -------------------------------------------------------------------------- *)

let test_merge_ownership_conflict_refuse () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"surv" ~created_at:"2026-01-01T00:00:00Z" ());
  ignore (seed_principal ~db ~id:"lose" ~created_at:"2026-02-01T00:00:00Z" ());
  let i1 = sample_identity ~app_id:10 ~github_user_id:1L () in
  let i2 = sample_identity ~app_id:10 ~github_user_id:2L () in
  ignore
    (match
       Op.attach_account ~db
         ~assertion:
           (make_assertion ~principal_id:(pid "surv") ~identity:i1
              ~expires_at:"2099-01-01T00:00:00Z" ())
         ~id:"bs" ~now:fixed_now ()
     with
    | Op.Attached _ -> ()
    | Op.Refused { denial; _ } ->
        Alcotest.fail (Op.string_of_attach_denial denial));
  ignore
    (match
       Op.attach_account ~db
         ~assertion:
           (make_assertion ~principal_id:(pid "lose") ~identity:i2
              ~expires_at:"2099-01-01T00:00:00Z" ())
         ~id:"bl" ~now:fixed_now ()
     with
    | Op.Attached _ -> ()
    | Op.Refused { denial; _ } ->
        Alcotest.fail (Op.string_of_attach_denial denial));
  match
    Op.evaluate_merge_ownership ~db ~from_principal:(pid "lose")
      ~to_principal:(pid "surv") ~now:fixed_now ()
  with
  | Error e -> Alcotest.fail e
  | Ok (Op.Merge_ok _) -> Alcotest.fail "expected conflict"
  | Ok (Op.Merge_refuse { conflicts; audit }) -> (
      Alcotest.(check bool) "has conflict" true (conflicts <> []);
      Alcotest.(check string)
        "audit" "merge_conflict_refused"
        (Op.string_of_audit_kind audit.kind);
      (* Merge apply also refuses via wired policy *)
      match
        M.apply_merge ~db ~left_id:(pid "surv") ~right_id:(pid "lose")
          ~now:fixed_now ()
      with
      | M.Refused { conflicts = cs; _ } ->
          Alcotest.(check bool) "merge refused" true (cs <> [])
      | other ->
          Alcotest.fail
            (match other with
            | M.Applied _ -> "applied"
            | M.Idempotent _ -> "idempotent"
            | M.Stale_revision s -> s
            | M.Refused _ -> "refused?"))

let test_merge_coalesce_identical_identity () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"surv" ~created_at:"2026-01-01T00:00:00Z" ());
  ignore (seed_principal ~db ~id:"lose" ~created_at:"2026-02-01T00:00:00Z" ());
  let identity = sample_identity ~app_id:5 ~github_user_id:50L () in
  (* Seed via binding insert under each principal would violate uniqueness —
     instead evaluate pure coalesce lists with two in-memory lists. *)
  let b_s =
    B.make_binding ~id:"bs" ~principal_id:(pid "surv") ~identity
      ~lineage_id:"lin_s" ()
  in
  let b_l =
    B.make_binding ~id:"bl" ~principal_id:(pid "lose") ~identity
      ~lineage_id:"lin_l" ()
  in
  let conflicts =
    Op.detect_merge_conflicts ~survivor_bindings:[ b_s ] ~loser_bindings:[ b_l ]
  in
  Alcotest.(check int) "identical identity coalesces" 0 (List.length conflicts)

let test_split_retains_github_bindings_and_refuses_rebind () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"src" ());
  let identity = sample_identity ~github_user_id:123L () in
  ignore
    (match
       Op.attach_account ~db
         ~assertion:
           (make_assertion ~principal_id:(pid "src") ~identity
              ~expires_at:"2099-01-01T00:00:00Z" ())
         ~id:"gh_on_src" ~now:fixed_now ()
     with
    | Op.Attached _ -> ()
    | Op.Refused { denial; _ } ->
        Alcotest.fail (Op.string_of_attach_denial denial));
  (match
     Op.evaluate_split_ownership ~db ~source_principal_id:(pid "src")
       ~now:fixed_now ()
   with
  | Error e -> Alcotest.fail e
  | Ok (Op.Split_ok { retained_binding_ids }) ->
      Alcotest.(check (list string))
        "retained" [ "gh_on_src" ] retained_binding_ids
  | Ok (Op.Split_refuse _) -> Alcotest.fail "default split should be ok");
  match
    Op.evaluate_split_ownership ~db ~source_principal_id:(pid "src")
      ~requested_binding_ids:[ "gh_on_src" ] ~now:fixed_now ()
  with
  | Error e -> Alcotest.fail e
  | Ok (Op.Split_ok _) -> Alcotest.fail "requested rebind must refuse"
  | Ok (Op.Split_refuse { conflicts; audit }) ->
      Alcotest.(check int) "one conflict" 1 (List.length conflicts);
      Alcotest.(check string)
        "audit" "split_conflict_refused"
        (Op.string_of_audit_kind audit.kind)

let test_expired_assertion_refuses_attach () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  let assertion =
    make_assertion ~principal_id:(pid "prin_a")
      ~verified_at:"2020-01-01T00:00:00Z" ~expires_at:"2020-01-01T01:00:00Z" ()
  in
  match Op.attach_account ~db ~assertion ~now:fixed_now () with
  | Op.Attached _ -> Alcotest.fail "expired must refuse"
  | Op.Refused { denial = Op.Assertion_expired _; _ } -> ()
  | Op.Refused { denial = Op.Assertion_invalid msg; _ }
    when contains msg "expired" ->
      ()
  | Op.Refused { denial; _ } ->
      Alcotest.fail ("unexpected: " ^ Op.string_of_attach_denial denial)

let test_audit_json_has_no_secrets () =
  with_db @@ fun db ->
  ignore (seed_principal ~db ~id:"prin_a" ());
  let assertion =
    make_assertion ~principal_id:(pid "prin_a")
      ~expires_at:"2099-01-01T00:00:00Z" ()
  in
  match Op.attach_account ~db ~assertion ~id:"b1" ~now:fixed_now () with
  | Op.Refused { denial; _ } ->
      Alcotest.fail (Op.string_of_attach_denial denial)
  | Op.Attached { audit; _ } ->
      let s = Yojson.Safe.to_string (Op.redacted_audit_to_json audit) in
      Alcotest.(check bool)
        "no access_token" false
        (contains s "access_token" || contains s "refresh_token");
      Alcotest.(check bool)
        "no client_secret" false
        (contains s "client_secret")

let suite =
  [
    ( "assertion requires verified and unexpired",
      `Quick,
      test_assertion_requires_verified_and_unexpired );
    ("assertion ttl default", `Quick, test_assertion_ttl_default);
    ( "attach requires active principal",
      `Quick,
      test_attach_requires_active_principal );
    ( "attach CAS stale principal revision",
      `Quick,
      test_attach_cas_stale_principal_revision );
    ("attach success and audit", `Quick, test_attach_success_and_audit);
    ( "duplicate ownership refused by default",
      `Quick,
      test_duplicate_ownership_refused_by_default );
    ( "admin exception reassigns with audit",
      `Quick,
      test_admin_exception_reassigns_with_audit );
    ( "admin exception without reassign flag refuses",
      `Quick,
      test_admin_exception_without_reassign_flag_refuses );
    ( "merge ownership conflict refuse",
      `Quick,
      test_merge_ownership_conflict_refuse );
    ( "merge coalesce identical identity",
      `Quick,
      test_merge_coalesce_identical_identity );
    ( "split retains github bindings and refuses rebind",
      `Quick,
      test_split_retains_github_bindings_and_refuses_rebind );
    ( "expired assertion refuses attach",
      `Quick,
      test_expired_assertion_refuses_attach );
    ("audit json has no secrets", `Quick, test_audit_json_has_no_secrets);
  ]

(** Tests for private two-sided link proof execution (P21.M1.E1.T010). *)

module P = Principal_identity
module L = Principal_link_protocol
module E = Principal_link_exec

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  E.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_784_000_000.0
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let pid s = assert_ok (P.principal_id_of_string s)

let key ?(connector = P.Teams) ?(tenant = "tenant-a") ?(user = "user-1") () =
  assert_ok
    (P.make_connector_actor_key ~connector ~tenant_or_workspace:tenant
       ~immutable_user_id:user)

let endpoint ?(connector = P.Teams) ?(tenant = "tenant-a") ?(user = "user-1")
    ?principal_id ?(principal_revision = 1) ?(actor_revision = 1)
    ?(verified_at = "2026-07-13T00:00:00Z") () =
  let actor_key = key ~connector ~tenant ~user () in
  match principal_id with
  | None ->
      assert_ok
        (L.make_verified_endpoint ~actor_key ~actor_revision ~verified_at ())
  | Some id ->
      assert_ok
        (L.make_verified_endpoint ~actor_key ~principal_id:id
           ~principal_revision ~actor_revision ~verified_at ())

let make_pair ?(user_a = "user-a") ?(user_b = "user-b") () =
  let endpoint_a =
    endpoint ~connector:P.Teams ~user:user_a ~principal_id:(pid "prin_a")
      ~principal_revision:2 ~actor_revision:3 ()
  in
  let endpoint_b =
    endpoint ~connector:P.Slack ~tenant:"workspace-b" ~user:user_b
      ~principal_id:(pid "prin_b") ~principal_revision:4 ~actor_revision:5 ()
  in
  (endpoint_a, endpoint_b)

let collect_audits () =
  let audits = ref [] in
  let sink e = audits := e :: !audits in
  (audits, sink)

let status_string = function
  | E.Endpoint_proved -> "endpoint_proved"
  | E.Link_completed -> "link_completed"
  | E.Idempotent_replay -> "idempotent_replay"
  | E.Rejected r -> "rejected:" ^ r

(* -------------------------------------------------------------------------- *)
(* Happy path                                                                 *)
(* -------------------------------------------------------------------------- *)

let test_happy_path_two_sided_complete () =
  with_db @@ fun db ->
  let audits, sink = collect_audits () in
  let endpoint_a, endpoint_b = make_pair () in
  let stored, create_audit =
    assert_ok
      (E.create_open_link ~db ~endpoint_a ~endpoint_b ~initiator:`A
         ~initiator_principal_id:(pid "prin_a") ~id:"ltx_happy"
         ~replay_protection_id:"replay_happy" ~proof_challenge_id:"chal_happy"
         ~now:fixed_now ~audit_sink:sink ())
  in
  Alcotest.(check string)
    "open" "open"
    (L.string_of_link_tx_status stored.tx.status);
  Alcotest.(check string)
    "create audit kind" "link_tx_created"
    (L.string_of_audit_kind create_audit.kind);
  Alcotest.(check bool)
    "create audit redacted" true
    (L.audit_event_is_redacted create_audit);
  Alcotest.(check (option string))
    "initiator principal" (Some "prin_a")
    (Option.map P.principal_id_to_string stored.initiator_principal_id);
  (* Side A proves. *)
  let r1 =
    E.present_proof ~db ~id:"ltx_happy" ~side:`A
      ~presented_replay_id:"replay_happy" ~presented_challenge_id:"chal_happy"
      ~presented_actor_key:endpoint_a.actor_key ~presented_actor_revision:3
      ~presented_principal_id:(pid "prin_a") ~presented_principal_revision:2
      ~now:fixed_now ~audit_sink:sink ()
  in
  Alcotest.(check string) "awaiting" "endpoint_proved" (status_string r1.status);
  Alcotest.(check bool) "no ownership" false r1.ownership_changed;
  (match r1.stored with
  | Some s ->
      Alcotest.(check string)
        "awaiting counterpart" "awaiting_counterpart"
        (L.string_of_link_tx_status s.tx.status);
      Alcotest.(check bool) "a proved" true s.tx.a_proved;
      Alcotest.(check bool) "b not" false s.tx.b_proved
  | None -> Alcotest.fail "expected stored after prove A");
  (* Side B proves → complete. *)
  let r2 =
    E.present_proof ~db ~id:"ltx_happy" ~side:`B
      ~presented_replay_id:"replay_happy" ~presented_challenge_id:"chal_happy"
      ~presented_actor_key:endpoint_b.actor_key ~presented_actor_revision:5
      ~presented_principal_id:(pid "prin_b") ~presented_principal_revision:4
      ~now:fixed_now ~audit_sink:sink ()
  in
  Alcotest.(check string) "completed" "link_completed" (status_string r2.status);
  Alcotest.(check bool) "still no ownership merge" false r2.ownership_changed;
  (match (r2.stored, r2.edge) with
  | Some s, Some edge ->
      Alcotest.(check string)
        "status completed" "completed"
        (L.string_of_link_tx_status s.tx.status);
      Alcotest.(check bool) "both proved" true (s.tx.a_proved && s.tx.b_proved);
      Alcotest.(check string) "edge tx" "ltx_happy" edge.link_tx_id;
      Alcotest.(check (option string))
        "edge prin a" (Some "prin_a") edge.principal_a_id;
      Alcotest.(check (option string))
        "edge prin b" (Some "prin_b") edge.principal_b_id
  | _ -> Alcotest.fail "expected completed stored + edge");
  Alcotest.(check string)
    "complete audit" "link_tx_completed"
    (L.string_of_audit_kind r2.audit.kind);
  Alcotest.(check bool) "redacted" true (L.audit_event_is_redacted r2.audit);
  (* Edge readable via store. *)
  (match E.get_edge_by_tx ~db ~link_tx_id:"ltx_happy" with
  | Ok (Some e) -> Alcotest.(check string) "edge id" "pledge_ltx_happy" e.id
  | Ok None -> Alcotest.fail "missing edge"
  | Error e -> Alcotest.fail e);
  Alcotest.(check bool) "audits emitted" true (List.length !audits >= 3)

(* -------------------------------------------------------------------------- *)
(* Replay                                                                     *)
(* -------------------------------------------------------------------------- *)

let test_replay_idempotent_and_reject () =
  with_db @@ fun db ->
  let endpoint_a, endpoint_b = make_pair ~user_a:"ra" ~user_b:"rb" () in
  let stored, _ =
    assert_ok
      (E.create_open_link ~db ~endpoint_a ~endpoint_b ~id:"ltx_replay"
         ~replay_protection_id:"replay_r" ~proof_challenge_id:"chal_r"
         ~now:fixed_now ())
  in
  ignore stored;
  let prove side =
    E.present_proof ~db ~id:"ltx_replay" ~side ~presented_replay_id:"replay_r"
      ~presented_challenge_id:"chal_r" ~now:fixed_now ()
  in
  Alcotest.(check string)
    "a" "endpoint_proved"
    (status_string (prove `A).status);
  Alcotest.(check string)
    "b complete" "link_completed"
    (status_string (prove `B).status);
  (* Replay after complete is idempotent. *)
  let again = prove `A in
  Alcotest.(check string)
    "idempotent" "idempotent_replay"
    (status_string again.status);
  Alcotest.(check bool) "no ownership" false again.ownership_changed;
  Alcotest.(check string)
    "replay audit" "link_tx_replayed"
    (L.string_of_audit_kind again.audit.kind);
  (* Bad replay id rejected. *)
  let bad =
    E.present_proof ~db ~id:"ltx_replay" ~side:`B ~presented_replay_id:"wrong"
      ~presented_challenge_id:"chal_r" ~now:fixed_now ()
  in
  (match bad.status with
  | E.Rejected msg ->
      Alcotest.(check bool)
        "mismatch" true
        (Test_helpers.string_contains msg "mismatch")
  | _ -> Alcotest.fail "expected rejected bad replay");
  Alcotest.(check bool) "ownership unchanged" false bad.ownership_changed

(* -------------------------------------------------------------------------- *)
(* Expiry                                                                     *)
(* -------------------------------------------------------------------------- *)

let test_expiry () =
  with_db @@ fun db ->
  let endpoint_a, endpoint_b = make_pair ~user_a:"ea" ~user_b:"eb" () in
  let stored, _ =
    assert_ok
      (E.create_open_link ~db ~endpoint_a ~endpoint_b ~id:"ltx_exp"
         ~replay_protection_id:"replay_e" ~proof_challenge_id:"chal_e"
         ~ttl_seconds:60. ~now:fixed_now ())
  in
  let later = fixed_now +. 61. in
  let r =
    E.present_proof ~db ~id:"ltx_exp" ~side:`A ~presented_replay_id:"replay_e"
      ~presented_challenge_id:"chal_e" ~now:later ()
  in
  (match r.status with
  | E.Rejected msg ->
      Alcotest.(check bool)
        "expired" true
        (Test_helpers.string_contains msg "expired")
  | other ->
      Alcotest.fail ("expected expired reject, got " ^ status_string other));
  Alcotest.(check bool) "no ownership" false r.ownership_changed;
  Alcotest.(check string)
    "expire audit" "link_tx_expired"
    (L.string_of_audit_kind r.audit.kind);
  (match E.get ~db ~id:"ltx_exp" with
  | Ok (Some s) ->
      Alcotest.(check string)
        "status expired" "expired"
        (L.string_of_link_tx_status s.tx.status)
  | _ -> Alcotest.fail "expected stored expired");
  (* Explicit expire path. *)
  let endpoint_a, endpoint_b = make_pair ~user_a:"ea2" ~user_b:"eb2" () in
  let _, _ =
    assert_ok
      (E.create_open_link ~db ~endpoint_a ~endpoint_b ~id:"ltx_exp2"
         ~replay_protection_id:"replay_e2" ~proof_challenge_id:"chal_e2"
         ~ttl_seconds:30. ~now:fixed_now ())
  in
  let expired, audit =
    assert_ok (E.expire_link ~db ~id:"ltx_exp2" ~now:later ())
  in
  Alcotest.(check string)
    "expired2" "expired"
    (L.string_of_link_tx_status expired.tx.status);
  Alcotest.(check string)
    "audit" "link_tx_expired"
    (L.string_of_audit_kind audit.kind);
  ignore stored

(* -------------------------------------------------------------------------- *)
(* Cancel                                                                     *)
(* -------------------------------------------------------------------------- *)

let test_cancel () =
  with_db @@ fun db ->
  let endpoint_a, endpoint_b = make_pair ~user_a:"ca" ~user_b:"cb" () in
  let _, _ =
    assert_ok
      (E.create_open_link ~db ~endpoint_a ~endpoint_b ~id:"ltx_cancel"
         ~replay_protection_id:"replay_c" ~proof_challenge_id:"chal_c"
         ~now:fixed_now ())
  in
  let cancelled, audit =
    assert_ok
      (E.cancel_link ~db ~id:"ltx_cancel" ~reason:"user_aborted" ~now:fixed_now
         ())
  in
  Alcotest.(check string)
    "cancelled" "cancelled"
    (L.string_of_link_tx_status cancelled.tx.status);
  Alcotest.(check (option string))
    "reason" (Some "user_aborted") cancelled.tx.cancel_reason;
  Alcotest.(check string)
    "audit" "link_tx_cancelled"
    (L.string_of_audit_kind audit.kind);
  let r =
    E.present_proof ~db ~id:"ltx_cancel" ~side:`A
      ~presented_replay_id:"replay_c" ~presented_challenge_id:"chal_c"
      ~now:fixed_now ()
  in
  (match r.status with
  | E.Rejected msg ->
      Alcotest.(check bool)
        "cancel reject" true
        (Test_helpers.string_contains (String.lowercase_ascii msg) "cancel")
  | _ -> Alcotest.fail "proof after cancel must reject");
  Alcotest.(check bool) "no ownership" false r.ownership_changed;
  (* No edge after cancel. *)
  match E.get_edge_by_tx ~db ~link_tx_id:"ltx_cancel" with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "edge must not exist after cancel"
  | Error e -> Alcotest.fail e

(* -------------------------------------------------------------------------- *)
(* Actor change / ambiguity                                                   *)
(* -------------------------------------------------------------------------- *)

let test_actor_change_and_ambiguity () =
  with_db @@ fun db ->
  let endpoint_a, endpoint_b = make_pair ~user_a:"aa" ~user_b:"ab" () in
  let _, _ =
    assert_ok
      (E.create_open_link ~db ~endpoint_a ~endpoint_b ~id:"ltx_actor"
         ~replay_protection_id:"replay_a" ~proof_challenge_id:"chal_a"
         ~now:fixed_now ())
  in
  (* Actor revision change. *)
  let r =
    E.present_proof ~db ~id:"ltx_actor" ~side:`A ~presented_replay_id:"replay_a"
      ~presented_challenge_id:"chal_a" ~presented_actor_key:endpoint_a.actor_key
      ~presented_actor_revision:99 ~now:fixed_now ()
  in
  (match r.status with
  | E.Rejected msg ->
      Alcotest.(check bool)
        "rev change" true
        (Test_helpers.string_contains msg "revision changed")
  | _ -> Alcotest.fail "expected actor revision reject");
  Alcotest.(check bool) "no ownership" false r.ownership_changed;
  (* Wrong side actor key (ambiguity). *)
  let r =
    E.present_proof ~db ~id:"ltx_actor" ~side:`A ~presented_replay_id:"replay_a"
      ~presented_challenge_id:"chal_a" ~presented_actor_key:endpoint_b.actor_key
      ~presented_actor_revision:3 ~now:fixed_now ()
  in
  (match r.status with
  | E.Rejected msg ->
      Alcotest.(check bool)
        "ambiguity" true
        (Test_helpers.string_contains msg "ambiguity"
        || Test_helpers.string_contains msg "counterpart")
  | _ -> Alcotest.fail "expected ambiguity reject");
  (* Principal binding change. *)
  let r =
    E.present_proof ~db ~id:"ltx_actor" ~side:`A ~presented_replay_id:"replay_a"
      ~presented_challenge_id:"chal_a" ~presented_actor_key:endpoint_a.actor_key
      ~presented_actor_revision:3 ~presented_principal_id:(pid "prin_stranger")
      ~presented_principal_revision:2 ~now:fixed_now ()
  in
  (match r.status with
  | E.Rejected msg ->
      Alcotest.(check bool)
        "principal change" true
        (Test_helpers.string_contains msg "principal"
        || Test_helpers.string_contains msg "changed")
  | _ -> Alcotest.fail "expected principal change reject");
  (* Still open; ownership tables untouched (no edge). *)
  match E.get ~db ~id:"ltx_actor" with
  | Ok (Some s) ->
      Alcotest.(check string)
        "still open" "open"
        (L.string_of_link_tx_status s.tx.status)
  | _ -> Alcotest.fail "tx missing"

(* -------------------------------------------------------------------------- *)
(* Concurrent CAS                                                             *)
(* -------------------------------------------------------------------------- *)

let test_concurrent_cas_and_open () =
  with_db @@ fun db ->
  let endpoint_a, endpoint_b = make_pair ~user_a:"co_a" ~user_b:"co_b" () in
  let stored, _ =
    assert_ok
      (E.create_open_link ~db ~endpoint_a ~endpoint_b ~id:"ltx_cas"
         ~replay_protection_id:"replay_cas" ~proof_challenge_id:"chal_cas"
         ~now:fixed_now ())
  in
  Alcotest.(check int) "rev1" 1 stored.tx_revision;
  (* Concurrent second open for same pair fails closed. *)
  (match
     E.create_open_link ~db ~endpoint_a ~endpoint_b ~id:"ltx_cas2"
       ~replay_protection_id:"replay_cas2" ~proof_challenge_id:"chal_cas2"
       ~now:fixed_now ()
   with
  | Ok _ -> Alcotest.fail "expected concurrent open reject"
  | Error msg ->
      Alcotest.(check bool)
        "concurrent open" true
        (Test_helpers.string_contains msg "concurrent"));
  (* First present succeeds with matching CAS revision. *)
  let r1 =
    E.present_proof ~db ~id:"ltx_cas" ~side:`A ~presented_replay_id:"replay_cas"
      ~presented_challenge_id:"chal_cas" ~expected_tx_revision:1 ~now:fixed_now
      ()
  in
  Alcotest.(check string) "proved" "endpoint_proved" (status_string r1.status);
  (match r1.stored with
  | Some s -> Alcotest.(check int) "rev2" 2 s.tx_revision
  | None -> Alcotest.fail "missing stored");
  (* Stale CAS revision fails closed. *)
  let stale =
    E.present_proof ~db ~id:"ltx_cas" ~side:`B ~presented_replay_id:"replay_cas"
      ~presented_challenge_id:"chal_cas" ~expected_tx_revision:1 ~now:fixed_now
      ()
  in
  (match stale.status with
  | E.Rejected msg ->
      Alcotest.(check bool)
        "cas fail" true
        (Test_helpers.string_contains msg "revision conflict"
        || Test_helpers.string_contains msg "CAS")
  | _ -> Alcotest.fail "expected CAS reject");
  Alcotest.(check bool) "no ownership" false stale.ownership_changed;
  (* Still awaiting; only A proved. *)
  (match E.get ~db ~id:"ltx_cas" with
  | Ok (Some s) ->
      Alcotest.(check string)
        "awaiting" "awaiting_counterpart"
        (L.string_of_link_tx_status s.tx.status);
      Alcotest.(check bool) "only a" true (s.tx.a_proved && not s.tx.b_proved)
  | _ -> Alcotest.fail "missing");
  (* Fresh CAS completes. *)
  let r2 =
    E.present_proof ~db ~id:"ltx_cas" ~side:`B ~presented_replay_id:"replay_cas"
      ~presented_challenge_id:"chal_cas" ~expected_tx_revision:2 ~now:fixed_now
      ()
  in
  Alcotest.(check string) "done" "link_completed" (status_string r2.status);
  (* Cancel CAS fail. *)
  let endpoint_a, endpoint_b = make_pair ~user_a:"cx_a" ~user_b:"cx_b" () in
  let s2, _ =
    assert_ok
      (E.create_open_link ~db ~endpoint_a ~endpoint_b ~id:"ltx_cas_cancel"
         ~now:fixed_now ())
  in
  match
    E.cancel_link ~db ~id:"ltx_cas_cancel" ~expected_tx_revision:99
      ~now:fixed_now ()
  with
  | Ok _ -> Alcotest.fail "cancel stale CAS should fail"
  | Error msg ->
      Alcotest.(check bool)
        "cancel cas" true
        (Test_helpers.string_contains msg "revision conflict");
      ignore s2

let test_challenge_mismatch () =
  with_db @@ fun db ->
  let endpoint_a, endpoint_b = make_pair ~user_a:"ch_a" ~user_b:"ch_b" () in
  ignore
    (assert_ok
       (E.create_open_link ~db ~endpoint_a ~endpoint_b ~id:"ltx_chal"
          ~replay_protection_id:"replay_ch" ~proof_challenge_id:"chal_real"
          ~now:fixed_now ()));
  let r =
    E.present_proof ~db ~id:"ltx_chal" ~side:`A ~presented_replay_id:"replay_ch"
      ~presented_challenge_id:"chal_fake" ~now:fixed_now ()
  in
  match r.status with
  | E.Rejected msg ->
      Alcotest.(check bool)
        "challenge" true
        (Test_helpers.string_contains msg "challenge")
  | _ -> Alcotest.fail "expected challenge reject"

let suite =
  [
    ("happy path two-sided complete", `Quick, test_happy_path_two_sided_complete);
    ("replay idempotent and reject", `Quick, test_replay_idempotent_and_reject);
    ("expiry", `Quick, test_expiry);
    ("cancel", `Quick, test_cancel);
    ("actor change and ambiguity", `Quick, test_actor_change_and_ambiguity);
    ("concurrent CAS and open", `Quick, test_concurrent_cas_and_open);
    ("challenge mismatch", `Quick, test_challenge_mismatch);
  ]

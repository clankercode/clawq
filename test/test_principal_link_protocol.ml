(** Tests for private cross-Connector linking and admin repair protocol
    (P21.M1.E1.T004). *)

module P = Principal_identity
module L = Principal_link_protocol

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let assert_error = function
  | Error e -> e
  | Ok _ -> Alcotest.fail "expected Error"

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

let fixed_now = 1_784_000_000.0

let sample_tx ?(user_a = "user-a") ?(user_b = "user-b") ?(connector_a = P.Teams)
    ?(connector_b = P.Slack) () =
  let endpoint_a =
    endpoint ~connector:connector_a ~user:user_a ~principal_id:(pid "prin_a") ()
  in
  let endpoint_b =
    endpoint ~connector:connector_b ~tenant:"workspace-b" ~user:user_b
      ~principal_id:(pid "prin_b") ()
  in
  assert_ok
    (L.make_link_transaction ~id:"ltx_1" ~endpoint_a ~endpoint_b
       ~replay_protection_id:"replay_1" ~proof_challenge_id:"chal_1"
       ~now:fixed_now ())

let test_protocol_version () =
  Alcotest.(check int) "protocol_version" 1 L.protocol_version

let test_happy_path_link_transaction () =
  let tx = sample_tx () in
  assert_ok (L.validate_link_transaction tx);
  Alcotest.(check string)
    "status open" "open"
    (L.string_of_link_tx_status tx.status);
  Alcotest.(check bool) "not a proved" false tx.a_proved;
  Alcotest.(check bool) "not b proved" false tx.b_proved;
  Alcotest.(check bool)
    "not expired" false
    (L.link_transaction_is_expired ~now:fixed_now tx);
  assert_ok (L.assert_link_open_for_proof ~now:fixed_now tx);
  let json = L.link_transaction_to_json tx in
  match json with
  | `Assoc fields ->
      Alcotest.(check bool)
        "has proof_challenge_id field" true
        (List.mem_assoc "proof_challenge_id" fields);
      (* Export carries opaque challenge id only — no secret material field. *)
      Alcotest.(check bool)
        "no proof_secret field" false
        (List.mem_assoc "proof_secret" fields)
  | _ -> Alcotest.fail "expected assoc json"

let test_two_sided_proof_status_machine () =
  let tx = sample_tx () in
  let tx =
    assert_ok (L.mark_endpoint_proved_pure tx ~side:`A ~now:fixed_now ())
  in
  Alcotest.(check string)
    "awaiting" "awaiting_counterpart"
    (L.string_of_link_tx_status tx.status);
  Alcotest.(check bool) "a proved" true tx.a_proved;
  let err =
    assert_error (L.mark_endpoint_proved_pure tx ~side:`A ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "double prove rejected" true
    (Test_helpers.string_contains err "already proved");
  let tx =
    assert_ok (L.mark_endpoint_proved_pure tx ~side:`B ~now:fixed_now ())
  in
  Alcotest.(check string)
    "completed" "completed"
    (L.string_of_link_tx_status tx.status);
  Alcotest.(check bool) "both proved" true (tx.a_proved && tx.b_proved);
  match L.check_replay tx ~presented_replay_id:"replay_1" with
  | L.Idempotent_completed -> ()
  | L.Fresh -> Alcotest.fail "expected Idempotent_completed"
  | L.Rejected e -> Alcotest.fail e

let test_reject_auto_link_display_email () =
  let err =
    assert_error
      (L.reject_auto_link
         {
           basis = L.Auto_display_name;
           display_name = Some "Ada";
           email = None;
           external_account_hint = None;
           left_actor = None;
           right_actor = None;
         })
  in
  Alcotest.(check bool)
    "mentions forbidden" true
    (Test_helpers.string_contains err "forbidden"
    || Test_helpers.string_contains err "auto");
  let err =
    assert_error
      (L.reject_auto_link
         {
           basis = L.Auto_email;
           display_name = None;
           email = Some "ada@example.com";
           external_account_hint = None;
           left_actor = None;
           right_actor = None;
         })
  in
  Alcotest.(check bool) "email auto rejected" true (String.length err > 0);
  let err =
    assert_error
      (L.reject_auto_link
         {
           basis = L.Auto_external_account;
           display_name = None;
           email = None;
           external_account_hint = Some "github:42";
           left_actor = Some (key ~connector:P.Teams ());
           right_actor = Some (key ~connector:P.Slack ~user:"u2" ());
         })
  in
  Alcotest.(check bool)
    "external auto rejected" true
    (Test_helpers.string_contains err "forbidden"
    || Test_helpers.string_contains err "auto");
  (* Display/email alone with allowed basis label still fails without actors. *)
  let err =
    assert_error
      (L.reject_auto_link
         {
           basis = L.Two_sided_private_proof;
           display_name = Some "Ada";
           email = Some "ada@example.com";
           external_account_hint = None;
           left_actor = None;
           right_actor = None;
         })
  in
  Alcotest.(check bool)
    "hints alone rejected" true
    (Test_helpers.string_contains err "never"
    || Test_helpers.string_contains err "verified");
  assert_ok
    (L.reject_auto_link
       {
         basis = L.Two_sided_private_proof;
         display_name = None;
         email = None;
         external_account_hint = None;
         left_actor = Some (key ~connector:P.Teams ());
         right_actor = Some (key ~connector:P.Slack ~user:"u2" ());
       });
  match L.assert_link_basis_allowed L.Auto_display_name with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "auto_display_name must not be allowed"

let test_require_two_verified_endpoints () =
  let a = endpoint ~user:"u1" () in
  let b = endpoint ~user:"u1" () in
  let err = assert_error (L.require_two_verified_endpoints a b) in
  Alcotest.(check bool)
    "same actor rejected" true
    (Test_helpers.string_contains err "distinct");
  let empty_v =
    assert_error
      (L.make_verified_endpoint ~actor_key:(key ()) ~verified_at:"" ())
  in
  Alcotest.(check bool)
    "empty verified_at" true
    (Test_helpers.string_contains empty_v "verified_at");
  let b = endpoint ~connector:P.Discord ~user:"u2" () in
  assert_ok (L.require_two_verified_endpoints a b);
  let err =
    assert_error
      (L.make_link_transaction ~id:"x" ~endpoint_a:a ~endpoint_b:a
         ~replay_protection_id:"r" ~proof_challenge_id:"c" ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "tx rejects same endpoint" true
    (Test_helpers.string_contains err "distinct")

let test_expiry_cancel_replay () =
  let tx = sample_tx () in
  (* Not expired at creation time. *)
  Alcotest.(check bool)
    "fresh" false
    (L.link_transaction_is_expired ~now:fixed_now tx);
  (* Past expiry. *)
  let later = fixed_now +. L.default_link_ttl_seconds +. 1.0 in
  Alcotest.(check bool)
    "expired clock" true
    (L.link_transaction_is_expired ~now:later tx);
  let err = assert_error (L.assert_link_open_for_proof ~now:later tx) in
  Alcotest.(check bool)
    "open-for-proof fails" true
    (Test_helpers.string_contains err "expired");
  let expired = assert_ok (L.expire_link_transaction_pure tx ~now:later ()) in
  Alcotest.(check string)
    "status expired" "expired"
    (L.string_of_link_tx_status expired.status);
  (match L.check_replay expired ~presented_replay_id:"replay_1" with
  | L.Rejected msg ->
      Alcotest.(check bool)
        "replay expired rejected" true
        (Test_helpers.string_contains msg "expired")
  | _ -> Alcotest.fail "expected Rejected for expired replay");
  (* Cancel path. *)
  let tx = sample_tx ~user_a:"ca" ~user_b:"cb" () in
  let cancelled =
    assert_ok
      (L.cancel_link_transaction_pure tx ~reason:"user_aborted" ~now:fixed_now
         ())
  in
  Alcotest.(check string)
    "cancelled" "cancelled"
    (L.string_of_link_tx_status cancelled.status);
  let err = assert_error (L.assert_not_cancelled cancelled) in
  Alcotest.(check bool)
    "cancel reason" true
    (Test_helpers.string_contains err "cancelled");
  (match L.check_replay cancelled ~presented_replay_id:"replay_1" with
  | L.Rejected _ -> ()
  | _ -> Alcotest.fail "cancelled replay must reject");
  (* Mismatched replay id. *)
  let tx = sample_tx ~user_a:"ra" ~user_b:"rb" () in
  (match L.check_replay tx ~presented_replay_id:"other" with
  | L.Rejected msg ->
      Alcotest.(check bool)
        "mismatch" true
        (Test_helpers.string_contains msg "mismatch")
  | _ -> Alcotest.fail "expected mismatch rejection");
  match L.check_replay tx ~presented_replay_id:"replay_1" with
  | L.Fresh -> ()
  | _ -> Alcotest.fail "open matching replay should be Fresh"

let test_private_delivery_no_secrets () =
  let d =
    assert_ok
      (L.make_private_proof_delivery
         ~channel:
           (L.Connector_dm { connector = P.Teams; handle_id = "hdl_dm_1" })
         ~delivery_id:"del_1" ~endpoint_side:`A
         ~created_at:"2026-07-13T00:00:00Z" ())
  in
  Alcotest.(check bool) "export safe" true (L.delivery_is_export_safe d);
  let json = L.private_proof_delivery_to_json d in
  let s = Yojson.Safe.to_string json in
  Alcotest.(check bool)
    "no token in export" false
    (Test_helpers.string_contains s "token");
  let err =
    assert_error
      (L.make_private_proof_delivery
         ~channel:(L.Web_private { handle_id = "" })
         ~delivery_id:"del" ~endpoint_side:`B ())
  in
  Alcotest.(check bool)
    "empty handle rejected" true
    (Test_helpers.string_contains err "handle_id")

let test_admin_repair_plan_confirm_apply () =
  let endpoint_a =
    endpoint ~connector:P.Teams ~user:"admin-a" ~principal_id:(pid "prin_a")
      ~principal_revision:3 ()
  in
  let endpoint_b =
    endpoint ~connector:P.Slack ~tenant:"ws" ~user:"admin-b"
      ~principal_id:(pid "prin_b") ~principal_revision:2 ()
  in
  let preview : L.repair_preview =
    {
      survivor_principal_id = Some (pid "prin_a");
      merged_principal_id = Some (pid "prin_b");
      conflicts =
        [
          L.Preference_conflict
            { key = "default_github_account"; summary = "survivor keeps value" };
          L.Pending_authorization_invalidated { count = 1 };
        ];
      notes = [ "external accounts non-conflicting" ];
    }
  in
  let plan =
    assert_ok
      (L.make_admin_repair_plan ~id:"repair_1" ~endpoint_a ~endpoint_b
         ~admin_principal_id:(pid "prin_admin")
         ~survivor:(L.Explicit (pid "prin_a"))
         ~preview ~now:fixed_now ())
  in
  assert_ok (L.validate_admin_repair_plan plan);
  Alcotest.(check string)
    "planned" "planned"
    (L.string_of_repair_status plan.status);
  Alcotest.(check bool) "digest non-empty" true (String.length plan.digest > 0);
  Alcotest.(check (option int))
    "base rev a" (Some 3) plan.base_principal_a_revision;
  Alcotest.(check (option int))
    "base rev b" (Some 2) plan.base_principal_b_revision;
  (* Wrong digest. *)
  let err =
    assert_error
      (L.confirm_repair_plan_pure plan ~presented_digest:"deadbeef"
         ~confirming_principal:(pid "prin_admin") ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "digest mismatch" true
    (Test_helpers.string_contains err "digest");
  (* Wrong admin. *)
  let err =
    assert_error
      (L.confirm_repair_plan_pure plan ~presented_digest:plan.digest
         ~confirming_principal:(pid "prin_other") ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "principal mismatch" true
    (Test_helpers.string_contains err "principal");
  let confirmed =
    assert_ok
      (L.confirm_repair_plan_pure plan ~presented_digest:plan.digest
         ~confirming_principal:(pid "prin_admin") ~now:fixed_now ())
  in
  Alcotest.(check string)
    "confirmed" "confirmed"
    (L.string_of_repair_status confirmed.status);
  (* Digest stable across status transition. *)
  Alcotest.(check string) "digest stable" plan.digest confirmed.digest;
  let applied =
    assert_ok (L.mark_repair_applied_pure confirmed ~now:fixed_now ())
  in
  Alcotest.(check string)
    "applied" "applied"
    (L.string_of_repair_status applied.status);
  (* Idempotent apply. *)
  let again =
    assert_ok (L.mark_repair_applied_pure applied ~now:fixed_now ())
  in
  Alcotest.(check string)
    "still applied" "applied"
    (L.string_of_repair_status again.status);
  (* Explicit survivor must be one of the endpoints. *)
  let err =
    assert_error
      (L.make_admin_repair_plan ~id:"repair_bad" ~endpoint_a ~endpoint_b
         ~admin_principal_id:(pid "prin_admin")
         ~survivor:(L.Explicit (pid "prin_stranger"))
         ~preview ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "stranger survivor" true
    (Test_helpers.string_contains err "survivor")

let test_admin_repair_forbids_auto_basis_semantics () =
  (* Construction always stamps Admin_repair; auto bases cannot sneak in via
     assert_link_basis_allowed. *)
  Alcotest.(check bool)
    "auto email forbidden" false
    (L.link_basis_is_allowed L.Auto_email);
  Alcotest.(check bool)
    "admin allowed" true
    (L.link_basis_is_allowed L.Admin_repair);
  let endpoint_a = endpoint ~user:"x1" ~principal_id:(pid "p1") () in
  let endpoint_b =
    endpoint ~connector:P.Telegram ~user:"x2" ~principal_id:(pid "p2") ()
  in
  let plan =
    assert_ok
      (L.make_admin_repair_plan ~id:"r2" ~endpoint_a ~endpoint_b
         ~admin_principal_id:(pid "admin") ~survivor:L.By_creation_order
         ~preview:
           {
             survivor_principal_id = None;
             merged_principal_id = None;
             conflicts = [];
             notes = [];
           }
         ~now:fixed_now ())
  in
  Alcotest.(check string)
    "basis" "admin_repair"
    (L.string_of_link_basis plan.basis)

let test_repair_cancel_reject_expire () =
  let endpoint_a = endpoint ~user:"e1" ~principal_id:(pid "p1") () in
  let endpoint_b =
    endpoint ~connector:P.Web ~tenant:"https://issuer.example" ~user:"sub"
      ~principal_id:(pid "p2") ()
  in
  let plan =
    assert_ok
      (L.make_admin_repair_plan ~id:"r3" ~endpoint_a ~endpoint_b
         ~admin_principal_id:(pid "admin") ~survivor:L.By_creation_order
         ~preview:
           {
             survivor_principal_id = Some (pid "p1");
             merged_principal_id = Some (pid "p2");
             conflicts =
               [
                 L.External_account_collision
                   { summary = "distinct github users for same app" };
               ];
             notes = [ "fail closed" ];
           }
         ~now:fixed_now ())
  in
  let rejected =
    assert_ok
      (L.reject_repair_plan_pure plan ~reason:"external_account_collision" ())
  in
  Alcotest.(check string)
    "rejected" "rejected"
    (L.string_of_repair_status rejected.status);
  let plan2 =
    assert_ok
      (L.make_admin_repair_plan ~id:"r4" ~endpoint_a ~endpoint_b
         ~admin_principal_id:(pid "admin") ~survivor:L.By_creation_order
         ~preview:
           {
             survivor_principal_id = None;
             merged_principal_id = None;
             conflicts = [];
             notes = [];
           }
         ~now:fixed_now ())
  in
  let cancelled =
    assert_ok (L.cancel_repair_plan_pure plan2 ~now:fixed_now ())
  in
  Alcotest.(check string)
    "cancelled" "cancelled"
    (L.string_of_repair_status cancelled.status);
  let later = fixed_now +. L.default_repair_ttl_seconds +. 5.0 in
  Alcotest.(check bool)
    "repair expired" true
    (L.admin_repair_is_expired ~now:later plan2)

let test_redacted_audit () =
  let tx = sample_tx () in
  let ev =
    L.audit_from_link_transaction tx ~kind:L.Link_tx_created
      ~details:
        (`Assoc
           [
             ("token", `String "sekrit");
             ("proof_secret", `String "do-not-leak");
             ("email", `String "ada@example.com");
             ("ok_field", `String "visible");
           ])
      ~now:fixed_now ()
  in
  Alcotest.(check bool) "is redacted" true (L.audit_event_is_redacted ev);
  let export = L.redacted_audit_event_to_json ev in
  let s = Yojson.Safe.to_string export in
  Alcotest.(check bool)
    "no sekrit" false
    (Test_helpers.string_contains s "sekrit");
  Alcotest.(check bool)
    "no proof secret value" false
    (Test_helpers.string_contains s "do-not-leak");
  Alcotest.(check bool)
    "email redacted" true
    (Test_helpers.string_contains s "[redacted]");
  Alcotest.(check bool)
    "ok_field kept" true
    (Test_helpers.string_contains s "visible");
  (* Actor keys present, not display names. *)
  Alcotest.(check bool)
    "actor key" true
    (Test_helpers.string_contains s "connector:teams");
  let endpoint_a = endpoint ~user:"au" ~principal_id:(pid "pa") () in
  let endpoint_b =
    endpoint ~connector:P.Cli ~tenant:"device" ~user:"dev1"
      ~principal_id:(pid "pb") ()
  in
  let plan =
    assert_ok
      (L.make_admin_repair_plan ~id:"r_audit" ~endpoint_a ~endpoint_b
         ~admin_principal_id:(pid "admin") ~survivor:L.By_creation_order
         ~preview:
           {
             survivor_principal_id = Some (pid "pa");
             merged_principal_id = Some (pid "pb");
             conflicts = [];
             notes = [];
           }
         ~now:fixed_now ())
  in
  let rev =
    L.audit_from_repair_plan plan ~kind:L.Repair_planned ~now:fixed_now ()
  in
  Alcotest.(check bool)
    "repair audit redacted" true
    (L.audit_event_is_redacted rev);
  let raw =
    assert_ok
      (L.make_redacted_audit_event ~id:"a1" ~kind:L.Auto_link_rejected
         ~subject_id:"proposal" ~endpoint_a_key:"connector:x:tenant:t:user:u"
         ~status:"rejected" ~reason:"auto_email"
         ~details:(`Assoc [ ("password", `String "x"); ("note", `String "n") ])
         ~now:fixed_now ())
  in
  Alcotest.(check bool) "password stripped" true (L.audit_event_is_redacted raw)

let test_endpoint_revision_binding () =
  let e =
    assert_ok
      (L.make_verified_endpoint ~actor_key:(key ()) ~principal_id:(pid "p")
         ~principal_revision:7 ~actor_revision:4
         ~verified_at:"2026-07-13T12:00:00Z" ())
  in
  Alcotest.(check (option int)) "prin rev" (Some 7) e.principal_revision;
  Alcotest.(check int) "actor rev" 4 e.actor_revision;
  let err =
    assert_error
      (L.make_verified_endpoint ~actor_key:(key ()) ~principal_revision:1
         ~verified_at:"2026-07-13T12:00:00Z" ())
  in
  Alcotest.(check bool)
    "rev without principal" true
    (Test_helpers.string_contains err "principal_id")

let suite =
  [
    ("protocol_version", `Quick, test_protocol_version);
    ("happy path link transaction", `Quick, test_happy_path_link_transaction);
    ( "two-sided proof status machine",
      `Quick,
      test_two_sided_proof_status_machine );
    ( "reject auto-link display/email",
      `Quick,
      test_reject_auto_link_display_email );
    ( "require two verified endpoints",
      `Quick,
      test_require_two_verified_endpoints );
    ("expiry cancel replay", `Quick, test_expiry_cancel_replay);
    ("private delivery no secrets", `Quick, test_private_delivery_no_secrets);
    ( "admin repair plan-confirm-apply",
      `Quick,
      test_admin_repair_plan_confirm_apply );
    ( "admin repair forbids auto basis",
      `Quick,
      test_admin_repair_forbids_auto_basis_semantics );
    ("repair cancel reject expire", `Quick, test_repair_cancel_reject_expire);
    ("redacted audit", `Quick, test_redacted_audit);
    ("endpoint revision binding", `Quick, test_endpoint_revision_binding);
  ]

(** Tests for Setup_plan confirm/apply (P19.M1.E1.T002). *)

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Setup_plan_apply.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let with_shared_db f =
  let path = Filename.temp_file "clawq-setup-plan-" ".sqlite" in
  let db1 = Sqlite3.db_open path in
  let db2 = Sqlite3.db_open path in
  Setup_plan_apply.init_schema db1;
  Setup_plan_apply.init_schema db2;
  Fun.protect
    ~finally:(fun () ->
      ignore (Sqlite3.db_close db2);
      ignore (Sqlite3.db_close db1);
      Sys.remove path)
    (fun () -> f db1 db2)

let principal =
  Setup_plan.{ id = "principal:alice"; kind = Principal; label = Some "Alice" }

let other_principal =
  Setup_plan.{ id = "principal:bob"; kind = Principal; label = Some "Bob" }

let source =
  Setup_plan.
    {
      room_id = Some "room-src";
      session_key = Some "teams:room-src:alice";
      connector = Some "teams";
      profile_id = None;
      extra = [];
    }

let dest =
  Setup_plan.
    {
      room_id = Some "room-dest";
      session_key = None;
      connector = Some "teams";
      profile_id = Some "prof";
      extra = [];
    }

let dest_room = "room-dest"

let make_plan ?(base_revision = "rev-1") ?(now = 1_700_000_000.0)
    ?(id = "plan_apply_1") () =
  Setup_plan.make ~principal ~source ~destination:dest
    ~current_state:(`Assoc [ ("model", `String "old") ])
    ~planned_state:(`Assoc [ ("model", `String "new") ])
    ~diff:
      [
        Setup_plan.Update
          {
            path = "room_profiles.prof.model";
            from_ = `String "old";
            to_ = `String "new";
          };
      ]
    ~readiness:[ { name = "ok"; status = Setup_plan.Pass; message = "ready" } ]
    ~warnings:[] ~base_revision ~now ~id
    ~apply_payload:
      {
        kind = Setup_plan.Room_profile;
        ops = `List [ `Assoc [ ("op", `String "set_model") ] ];
        data = `Assoc [];
      }
    ()

let allow_all ~principal:_ ~destination:_ = Ok ()
let deny_all ~principal:_ ~destination:_ = Error "not an admin"
let apply_ok ~plan:_ ~receipt_id:_ = Ok ()
let apply_fail ~plan:_ ~receipt_id:_ = Error "adapter boom"

let do_apply db plan ~revision ~now ~authority ~apply_ops =
  Setup_plan_apply.apply ~db ~plan_id:plan.Setup_plan.id ~digest:plan.digest
    ~principal ~current_base_revision:revision ~destination_room:dest_room ~now
    ~authority ~apply_ops ()

let assert_rejected outcome reason_s =
  match outcome with
  | Setup_plan_apply.Rejected { reason; _ } ->
      Alcotest.(check string)
        "reject reason" reason_s
        (Setup_plan_apply.string_of_reject_reason reason)
  | Setup_plan_apply.Applied _ -> Alcotest.fail "expected rejected"

let test_successful_apply () =
  with_db @@ fun db ->
  let plan = make_plan () in
  Alcotest.(check (result unit string))
    "store" (Ok ())
    (Setup_plan_apply.store_plan ~db plan);
  match
    do_apply db plan ~revision:"rev-1" ~now:1_700_000_000.0 ~authority:allow_all
      ~apply_ops:apply_ok
  with
  | Setup_plan_apply.Applied { receipt_id; first_time } ->
      Alcotest.(check bool) "first time" true first_time;
      Alcotest.(check bool)
        "receipt non-empty" true
        (String.length receipt_id > 0);
      let audits = Setup_plan_apply.list_audit ~db ~plan_id:plan.id () in
      Alcotest.(check bool)
        "has applied audit" true
        (List.exists (fun a -> a.Setup_plan_apply.outcome = "applied") audits)
  | Setup_plan_apply.Rejected { message; _ } -> Alcotest.fail message

let test_retry_idempotent_with_advanced_revision () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_idem" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let calls = ref 0 in
  let counting_ops ~plan:_ ~receipt_id:_ =
    incr calls;
    Ok ()
  in
  let first =
    do_apply db plan ~revision:"rev-1" ~now:1_700_000_000.0 ~authority:allow_all
      ~apply_ops:counting_ops
  in
  (* Post-apply: config revision advanced and plan would be "expired" if
     rechecked — identity match must still return the original receipt. *)
  let second =
    do_apply db plan ~revision:"rev-advanced"
      ~now:(1_700_000_000.0 +. 10_000.0)
      ~authority:deny_all ~apply_ops:counting_ops
  in
  match (first, second) with
  | ( Setup_plan_apply.Applied { receipt_id = r1; first_time = true },
      Setup_plan_apply.Applied { receipt_id = r2; first_time = false } ) ->
      Alcotest.(check string) "same receipt" r1 r2;
      Alcotest.(check int) "apply_ops once" 1 !calls;
      let audits = Setup_plan_apply.list_audit ~db ~plan_id:plan.id () in
      Alcotest.(check bool)
        "idempotent audit" true
        (List.exists
           (fun a -> a.Setup_plan_apply.outcome = "applied_idempotent")
           audits)
  | _ -> Alcotest.fail "expected applied then idempotent applied"

let test_digest_mismatch () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_dig" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let outcome =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:(String.make 64 'a')
      ~principal ~current_base_revision:"rev-1" ~destination_room:dest_room
      ~now:1_700_000_000.0 ~authority:allow_all ~apply_ops:apply_ok ()
  in
  assert_rejected outcome "digest_mismatch"

let test_principal_mismatch () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_prin" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let outcome =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest
      ~principal:other_principal ~current_base_revision:"rev-1"
      ~destination_room:dest_room ~now:1_700_000_000.0 ~authority:allow_all
      ~apply_ops:apply_ok ()
  in
  assert_rejected outcome "principal_mismatch"

let test_expired () =
  with_db @@ fun db ->
  let now = 1_700_000_000.0 in
  let plan = make_plan ~id:"plan_exp" ~now () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let outcome =
    do_apply db plan ~revision:"rev-1" ~now:(now +. 901.0) ~authority:allow_all
      ~apply_ops:apply_ok
  in
  assert_rejected outcome "expired"

let test_stale_revision () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_stale" ~base_revision:"rev-old" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let outcome =
    do_apply db plan ~revision:"rev-new" ~now:1_700_000_000.0
      ~authority:allow_all ~apply_ops:apply_ok
  in
  assert_rejected outcome "stale_revision"

let test_destination_mismatch () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_dest" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let outcome =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:"rev-1" ~destination_room:"other-room"
      ~now:1_700_000_000.0 ~authority:allow_all ~apply_ops:apply_ok ()
  in
  assert_rejected outcome "destination_mismatch"

let test_authority_denied () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_auth" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let outcome =
    do_apply db plan ~revision:"rev-1" ~now:1_700_000_000.0 ~authority:deny_all
      ~apply_ops:apply_ok
  in
  assert_rejected outcome "authority_denied"

let test_apply_failure_audited () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_fail" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let outcome =
    do_apply db plan ~revision:"rev-1" ~now:1_700_000_000.0 ~authority:allow_all
      ~apply_ops:apply_fail
  in
  assert_rejected outcome "apply_error";
  let audits = Setup_plan_apply.list_audit ~db ~plan_id:plan.id () in
  Alcotest.(check bool)
    "failed audited" true
    (List.exists (fun a -> a.Setup_plan_apply.outcome = "failed") audits);
  match
    do_apply db plan ~revision:"rev-1" ~now:1_700_000_010.0 ~authority:allow_all
      ~apply_ops:apply_ok
  with
  | Setup_plan_apply.Applied { first_time = true; _ } -> ()
  | _ -> Alcotest.fail "retry after failure should succeed"

let test_retry_reuses_stable_receipt_after_failed_apply () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_stable_retry_receipt" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let received = ref [] in
  let ambiguous_failure ~plan:_ ~receipt_id =
    received := receipt_id :: !received;
    (* An adapter can have crossed its external mutation boundary before it
       reports failure to this process. A retry must use its same idempotency
       key. *)
    Error "simulated crash after external mutation"
  in
  let first =
    do_apply db plan ~revision:"rev-1" ~now:1_700_000_000.0 ~authority:allow_all
      ~apply_ops:ambiguous_failure
  in
  assert_rejected first "apply_error";
  let succeeding_retry ~plan:_ ~receipt_id =
    received := receipt_id :: !received;
    Ok ()
  in
  match
    do_apply db plan ~revision:"rev-1" ~now:1_700_000_001.0 ~authority:allow_all
      ~apply_ops:succeeding_retry
  with
  | Setup_plan_apply.Applied { first_time = true; _ } -> (
      Alcotest.(check int)
        "apply boundary invoked twice" 2 (List.length !received);
      match List.rev !received with
      | [ first_receipt; retry_receipt ] ->
          Alcotest.(check string)
            "retry uses the same external idempotency key" first_receipt
            retry_receipt
      | _ -> Alcotest.fail "expected exactly two receipt observations")
  | outcome ->
      Alcotest.fail
        ("retry after ambiguous failure should apply, got "
        ^ Setup_plan_apply.string_of_outcome outcome)

let test_concurrency_single_receipt () =
  with_shared_db @@ fun db1 db2 ->
  let plan = make_plan ~id:"plan_race" ~base_revision:"rev-1" () in
  ignore (Setup_plan_apply.store_plan ~db:db1 plan);
  let calls = ref 0 in
  let counting ~plan:_ ~receipt_id:_ =
    incr calls;
    Ok ()
  in
  (match Sqlite3.exec db1 "BEGIN IMMEDIATE" with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      Alcotest.fail ("failed to hold writer lock: " ^ Sqlite3.Rc.to_string rc));
  (* This apply overlaps a separate connection's active writer transaction.
     A zero timeout makes the contention deterministic and proves that no
     stale overwrite or un-audited mutation is attempted. *)
  Sqlite3.busy_timeout db2 0;
  let blocked =
    do_apply db2 plan ~revision:"rev-1" ~now:1_700_000_000.0
      ~authority:allow_all ~apply_ops:counting
  in
  (match blocked with
  | Setup_plan_apply.Rejected { reason = Apply_error; message } ->
      Alcotest.(check bool)
        "required audit failure is explicit" true
        (String_util.contains message "required rejection audit")
  | outcome ->
      Alcotest.fail
        ("writer contention should fail closed, got "
        ^ Setup_plan_apply.string_of_outcome outcome));
  Alcotest.(check int) "contention never runs apply_ops" 0 !calls;
  ignore (Sqlite3.exec db1 "ROLLBACK");
  Sqlite3.busy_timeout db2 5_000;
  match
    do_apply db2 plan ~revision:"rev-1" ~now:1_700_000_001.0
      ~authority:allow_all ~apply_ops:counting
  with
  | Setup_plan_apply.Applied { first_time = true; _ } ->
      Alcotest.(check int) "retry runs apply_ops once" 1 !calls
  | outcome ->
      Alcotest.fail
        ("retry after writer contention should apply once, got "
        ^ Setup_plan_apply.string_of_outcome outcome)

let test_audit_details_redacted () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_redact" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let apply_with_secret ~plan:_ ~receipt_id:_ =
    Error "failed with bot_token=xoxb-should-not-leak"
  in
  let _ =
    do_apply db plan ~revision:"rev-1" ~now:1_700_000_000.0 ~authority:allow_all
      ~apply_ops:apply_with_secret
  in
  let audits = Setup_plan_apply.list_audit ~db ~plan_id:plan.id () in
  List.iter
    (fun a ->
      Alcotest.(check bool)
        "no raw secret in audit details" false
        (String_util.contains a.Setup_plan_apply.details "xoxb-should-not-leak"))
    audits

let test_audit_redacts_private_key_error () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_private_key_redact" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let apply_with_private_key ~plan:_ ~receipt_id:_ =
    Error "private_key: raw-private-key-must-not-persist"
  in
  let _ =
    do_apply db plan ~revision:"rev-1" ~now:1_700_000_000.0 ~authority:allow_all
      ~apply_ops:apply_with_private_key
  in
  let audits = Setup_plan_apply.list_audit ~db ~plan_id:plan.id () in
  List.iter
    (fun audit ->
      Alcotest.(check bool)
        "no raw private key in audit details" false
        (String_util.contains audit.Setup_plan_apply.details
           "raw-private-key-must-not-persist"))
    audits

let test_plan_not_found () =
  with_db @@ fun db ->
  let outcome =
    Setup_plan_apply.apply ~db ~plan_id:"missing" ~digest:(String.make 64 '0')
      ~principal ~current_base_revision:"rev-1" ~destination_room:dest_room
      ~now:1_700_000_000.0 ~authority:allow_all ~apply_ops:apply_ok ()
  in
  assert_rejected outcome "plan_not_found"

let suite =
  [
    ("successful apply", `Quick, test_successful_apply);
    ( "retry idempotent with advanced revision",
      `Quick,
      test_retry_idempotent_with_advanced_revision );
    ("digest mismatch", `Quick, test_digest_mismatch);
    ("principal mismatch", `Quick, test_principal_mismatch);
    ("expired", `Quick, test_expired);
    ("stale revision", `Quick, test_stale_revision);
    ("destination mismatch", `Quick, test_destination_mismatch);
    ("authority denied", `Quick, test_authority_denied);
    ("apply failure audited", `Quick, test_apply_failure_audited);
    ( "retry reuses stable receipt after failed apply",
      `Quick,
      test_retry_reuses_stable_receipt_after_failed_apply );
    ("concurrency single receipt", `Quick, test_concurrency_single_receipt);
    ("audit details redacted", `Quick, test_audit_details_redacted);
    ( "audit private key error redacted",
      `Quick,
      test_audit_redacts_private_key_error );
    ("plan not found", `Quick, test_plan_not_found);
  ]

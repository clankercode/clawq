(** Tests for Setup_plan confirm/apply (P19.M1.E1.T002). *)

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Setup_plan_apply.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

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
  let outcome =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:"rev-1" ~now:1_700_000_000.0 ~authority:allow_all
      ~apply_ops:apply_ok ()
  in
  match outcome with
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

let test_retry_idempotent () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_idem" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let first =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:"rev-1" ~now:1_700_000_000.0 ~authority:allow_all
      ~apply_ops:apply_ok ()
  in
  let second =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:"rev-1" ~now:1_700_000_001.0 ~authority:allow_all
      ~apply_ops:apply_ok ()
  in
  match (first, second) with
  | ( Setup_plan_apply.Applied { receipt_id = r1; first_time = true },
      Setup_plan_apply.Applied { receipt_id = r2; first_time = false } ) ->
      Alcotest.(check string) "same receipt" r1 r2;
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
      ~principal ~current_base_revision:"rev-1" ~now:1_700_000_000.0
      ~authority:allow_all ~apply_ops:apply_ok ()
  in
  assert_rejected outcome "digest_mismatch";
  let audits = Setup_plan_apply.list_audit ~db ~plan_id:plan.id () in
  Alcotest.(check bool)
    "rejected audited" true
    (List.exists (fun a -> a.Setup_plan_apply.outcome = "rejected") audits)

let test_principal_mismatch () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_prin" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let outcome =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest
      ~principal:other_principal ~current_base_revision:"rev-1"
      ~now:1_700_000_000.0 ~authority:allow_all ~apply_ops:apply_ok ()
  in
  assert_rejected outcome "principal_mismatch"

let test_expired () =
  with_db @@ fun db ->
  let now = 1_700_000_000.0 in
  let plan = make_plan ~id:"plan_exp" ~now () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let outcome =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:"rev-1" ~now:(now +. 901.0) ~authority:allow_all
      ~apply_ops:apply_ok ()
  in
  assert_rejected outcome "expired"

let test_stale_revision () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_stale" ~base_revision:"rev-old" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let outcome =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:"rev-new" ~now:1_700_000_000.0 ~authority:allow_all
      ~apply_ops:apply_ok ()
  in
  assert_rejected outcome "stale_revision";
  let audits = Setup_plan_apply.list_audit ~db ~plan_id:plan.id () in
  Alcotest.(check bool)
    "stale audited" true
    (List.exists
       (fun a ->
         a.Setup_plan_apply.outcome = "rejected"
         && a.reason = Some "stale_revision")
       audits)

let test_destination_mismatch () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_dest" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let outcome =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:"rev-1" ~expected_destination_room:"other-room"
      ~now:1_700_000_000.0 ~authority:allow_all ~apply_ops:apply_ok ()
  in
  assert_rejected outcome "destination_mismatch"

let test_authority_denied () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_auth" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let outcome =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:"rev-1" ~now:1_700_000_000.0 ~authority:deny_all
      ~apply_ops:apply_ok ()
  in
  assert_rejected outcome "authority_denied";
  let audits = Setup_plan_apply.list_audit ~db ~plan_id:plan.id () in
  Alcotest.(check bool)
    "denied audited" true
    (List.exists
       (fun a ->
         a.Setup_plan_apply.outcome = "rejected"
         && a.reason = Some "authority_denied")
       audits)

let test_apply_failure_audited () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_fail" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let outcome =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:"rev-1" ~now:1_700_000_000.0 ~authority:allow_all
      ~apply_ops:apply_fail ()
  in
  assert_rejected outcome "apply_error";
  let audits = Setup_plan_apply.list_audit ~db ~plan_id:plan.id () in
  Alcotest.(check bool)
    "failed audited" true
    (List.exists (fun a -> a.Setup_plan_apply.outcome = "failed") audits);
  (* Still pending so a later successful apply can proceed. *)
  match Setup_plan_apply.get_plan ~db ~plan_id:plan.id with
  | None -> Alcotest.fail "plan should remain"
  | Some _ -> (
      let outcome2 =
        Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest
          ~principal ~current_base_revision:"rev-1" ~now:1_700_000_010.0
          ~authority:allow_all ~apply_ops:apply_ok ()
      in
      match outcome2 with
      | Setup_plan_apply.Applied { first_time = true; _ } -> ()
      | _ -> Alcotest.fail "retry after failure should succeed")

let test_concurrency_prevents_stale_overwrite () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_race" ~base_revision:"rev-1" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  (* First apply wins. *)
  let first =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:"rev-1" ~now:1_700_000_000.0 ~authority:allow_all
      ~apply_ops:apply_ok ()
  in
  (match first with
  | Setup_plan_apply.Applied { first_time = true; _ } -> ()
  | _ -> Alcotest.fail "first apply should win");
  (* A concurrent attempt that still holds the old pending view but presents a
     different base revision is rejected as stale, not overwrite. *)
  let stale =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:"rev-2" ~now:1_700_000_001.0 ~authority:allow_all
      ~apply_ops:apply_ok ()
  in
  (* Already applied: retry with matching revision is idempotent; mismatched
     revision fails closed before mutating. *)
  (match stale with
  | Setup_plan_apply.Rejected { reason = Stale_revision; _ } -> ()
  | Setup_plan_apply.Applied { first_time = false; _ } ->
      (* If status applied is checked before revision, we may hit idempotent.
         Acceptance: no stale overwrite of the applied receipt. Either reject
         or idempotent replay is fine; overwrite of receipt is not. *)
      ()
  | other ->
      Alcotest.fail ("unexpected: " ^ Setup_plan_apply.string_of_outcome other));
  (* Confirm receipt unchanged under a successful idempotent retry. *)
  let replay =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:"rev-1" ~now:1_700_000_002.0 ~authority:allow_all
      ~apply_ops:apply_ok ()
  in
  match (first, replay) with
  | ( Setup_plan_apply.Applied { receipt_id = r1; _ },
      Setup_plan_apply.Applied { receipt_id = r2; first_time = false } ) ->
      Alcotest.(check string) "receipt not overwritten" r1 r2
  | _ -> Alcotest.fail "receipt stability failed"

let test_audit_details_redacted () =
  with_db @@ fun db ->
  let plan = make_plan ~id:"plan_redact" () in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let apply_with_secret ~plan:_ ~receipt_id:_ =
    Error "failed with bot_token=xoxb-should-not-leak"
  in
  let _ =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:"rev-1" ~now:1_700_000_000.0 ~authority:allow_all
      ~apply_ops:apply_with_secret ()
  in
  let audits = Setup_plan_apply.list_audit ~db ~plan_id:plan.id () in
  List.iter
    (fun a ->
      Alcotest.(check bool)
        "no raw secret in audit details" false
        (String_util.contains a.Setup_plan_apply.details "xoxb-should-not-leak"))
    audits

let test_plan_not_found () =
  with_db @@ fun db ->
  let outcome =
    Setup_plan_apply.apply ~db ~plan_id:"missing" ~digest:(String.make 64 '0')
      ~principal ~current_base_revision:"rev-1" ~now:1_700_000_000.0
      ~authority:allow_all ~apply_ops:apply_ok ()
  in
  assert_rejected outcome "plan_not_found"

let suite =
  [
    ("successful apply", `Quick, test_successful_apply);
    ("retry idempotent", `Quick, test_retry_idempotent);
    ("digest mismatch", `Quick, test_digest_mismatch);
    ("principal mismatch", `Quick, test_principal_mismatch);
    ("expired", `Quick, test_expired);
    ("stale revision", `Quick, test_stale_revision);
    ("destination mismatch", `Quick, test_destination_mismatch);
    ("authority denied", `Quick, test_authority_denied);
    ("apply failure audited", `Quick, test_apply_failure_audited);
    ( "concurrency prevents stale overwrite",
      `Quick,
      test_concurrency_prevents_stale_overwrite );
    ("audit details redacted", `Quick, test_audit_details_redacted);
    ("plan not found", `Quick, test_plan_not_found);
  ]

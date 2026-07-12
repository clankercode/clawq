(** Tests for setup plan admin consent (P19.M1.E1.T003). *)

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Setup_plan_consent.init_schema db;
  Setup_plan_apply.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let actor_global =
  Setup_plan_consent.
    {
      principal_id = "principal:global";
      role = Global_admin;
      source_room_id = Some "room-a";
    }

let actor_room_a =
  Setup_plan_consent.
    {
      principal_id = "principal:alice";
      role = Room_admin "room-a";
      source_room_id = Some "room-a";
    }

let actor_room_b =
  Setup_plan_consent.
    {
      principal_id = "principal:bob";
      role = Room_admin "room-b";
      source_room_id = Some "room-b";
    }

let actor_room_c =
  Setup_plan_consent.
    {
      principal_id = "principal:carol";
      role = Room_admin "room-c";
      source_room_id = Some "room-c";
    }

let actor_none =
  Setup_plan_consent.
    {
      principal_id = "principal:guest";
      role = None_;
      source_room_id = Some "room-a";
    }

let test_current_room_admin_allowed () =
  match
    Setup_plan_consent.evaluate ~actor:actor_room_a
      ~destination_room_id:(Some "room-a") ()
  with
  | Allow _ -> ()
  | Deny { reason; _ } -> Alcotest.fail reason

let test_guest_denied () =
  match
    Setup_plan_consent.evaluate ~actor:actor_none
      ~destination_room_id:(Some "room-a") ()
  with
  | Deny { code; _ } -> Alcotest.(check string) "not_admin" "not_admin" code
  | Allow _ -> Alcotest.fail "guest should be denied"

let test_global_admin_cross_room () =
  match
    Setup_plan_consent.evaluate ~actor:actor_global
      ~destination_room_id:(Some "room-b") ()
  with
  | Allow _ -> ()
  | Deny { reason; _ } -> Alcotest.fail reason

let test_cross_room_requires_consent () =
  match
    Setup_plan_consent.evaluate ~actor:actor_room_a
      ~destination_room_id:(Some "room-b") ()
  with
  | Deny { code; _ } ->
      Alcotest.(check string)
        "consent required" "cross_room_consent_required" code
  | Allow _ -> Alcotest.fail "cross-room without consent should deny"

let test_nl_and_callback_never_confirm () =
  Alcotest.(check bool)
    "NL" false
    (Setup_plan_consent.signal_counts_as_confirm Natural_language);
  Alcotest.(check bool)
    "callback" false
    (Setup_plan_consent.signal_counts_as_confirm External_callback);
  Alcotest.(check bool)
    "explicit" true
    (Setup_plan_consent.signal_counts_as_confirm Explicit_confirm)

let test_grant_rejects_nl () =
  with_db @@ fun db ->
  match
    Setup_plan_consent.grant_consent ~db ~destination_room_id:"room-b"
      ~actor:actor_room_b ~signal:Natural_language ()
  with
  | Error msg ->
      Alcotest.(check bool)
        "mentions never" true
        (String_util.contains msg "never")
  | Ok _ -> Alcotest.fail "NL consent must be rejected"

let test_grant_rejects_callback () =
  with_db @@ fun db ->
  match
    Setup_plan_consent.grant_consent ~db ~destination_room_id:"room-b"
      ~actor:actor_room_b ~signal:External_callback ()
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "callback consent must be rejected"

let test_cross_room_with_explicit_consent () =
  with_db @@ fun db ->
  let now = 1_700_000_000.0 in
  match
    Setup_plan_consent.grant_consent ~db ~destination_room_id:"room-b"
      ~actor:actor_room_b ~signal:Explicit_confirm ~now ()
  with
  | Error e -> Alcotest.fail e
  | Ok consent -> (
      match
        Setup_plan_consent.evaluate ~actor:actor_room_a
          ~destination_room_id:(Some "room-b") ~consent:(Some consent) ~now ()
      with
      | Allow _ -> ()
      | Deny { reason; _ } -> Alcotest.fail reason)

let test_grant_rejects_non_destination_admin () =
  with_db @@ fun db ->
  match
    Setup_plan_consent.grant_consent ~db ~destination_room_id:"room-b"
      ~actor:actor_room_c ~signal:Explicit_confirm ()
  with
  | Error message ->
      Alcotest.(check bool)
        "actionable destination admin error" true
        (String_util.contains message "destination Room")
  | Ok _ -> Alcotest.fail "wrong-Room admin must not mint destination consent"

let test_grant_rejects_non_admin () =
  with_db @@ fun db ->
  match
    Setup_plan_consent.grant_consent ~db ~destination_room_id:"room-b"
      ~actor:actor_none ~signal:Explicit_confirm ()
  with
  | Error message ->
      Alcotest.(check bool)
        "mentions Room-admin" true
        (String_util.contains message "Room-admin")
  | Ok _ -> Alcotest.fail "non-admin must not mint destination consent"

let test_authority_check_integration () =
  with_db @@ fun db ->
  let now = 1_700_000_000.0 in
  let principal =
    Setup_plan.{ id = "principal:alice"; kind = Principal; label = None }
  in
  let dest_b =
    Setup_plan.
      {
        room_id = Some "room-b";
        session_key = None;
        connector = Some "teams";
        profile_id = None;
        extra = [];
      }
  in
  let auth =
    Setup_plan_consent.authority_check ~db ~actor:actor_room_a ~now ()
  in
  (* Without consent: deny cross-room. *)
  (match auth ~principal ~destination:dest_b with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected deny without consent");
  (* Grant explicit consent and recheck. *)
  (match
     Setup_plan_consent.grant_consent ~db ~destination_room_id:"room-b"
       ~actor:actor_room_b ~signal:Explicit_confirm ~now ()
   with
  | Ok _ -> ()
  | Error e -> Alcotest.fail e);
  match auth ~principal ~destination:dest_b with
  | Ok () -> ()
  | Error e -> Alcotest.fail e

let test_apply_uses_consent_authority () =
  with_db @@ fun db ->
  let now = 1_700_000_000.0 in
  let principal =
    Setup_plan.{ id = "principal:alice"; kind = Principal; label = None }
  in
  let source =
    Setup_plan.
      {
        room_id = Some "room-a";
        session_key = None;
        connector = Some "teams";
        profile_id = None;
        extra = [];
      }
  in
  let dest =
    Setup_plan.
      {
        room_id = Some "room-b";
        session_key = None;
        connector = Some "teams";
        profile_id = None;
        extra = [];
      }
  in
  let plan =
    Setup_plan.make ~principal ~source ~destination:dest
      ~current_state:(`Assoc []) ~planned_state:(`Assoc []) ~diff:[]
      ~readiness:[] ~warnings:[] ~base_revision:"rev-1" ~now ~id:"plan_c1"
      ~apply_payload:
        { kind = Setup_plan.Generic "test"; ops = `List []; data = `Assoc [] }
      ()
  in
  ignore (Setup_plan_apply.store_plan ~db plan);
  let auth =
    Setup_plan_consent.authority_check ~db ~actor:actor_room_a ~now ()
  in
  let denied =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:"rev-1" ~destination_room:"room-b" ~now
      ~authority:auth
      ~apply_ops:(fun ~plan:_ ~receipt_id:_ -> Ok ())
      ()
  in
  (match denied with
  | Setup_plan_apply.Rejected { reason = Authority_denied; _ } -> ()
  | other ->
      Alcotest.fail
        ("expected authority_denied, got "
        ^ Setup_plan_apply.string_of_outcome other));
  ignore
    (Setup_plan_consent.grant_consent ~db ~destination_room_id:"room-b"
       ~actor:actor_room_b ~signal:Explicit_confirm ~now ());
  match
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:"rev-1" ~destination_room:"room-b" ~now
      ~authority:auth
      ~apply_ops:(fun ~plan:_ ~receipt_id:_ -> Ok ())
      ()
  with
  | Setup_plan_apply.Applied { first_time = true; _ } -> ()
  | other ->
      Alcotest.fail
        ("expected applied after consent, got "
        ^ Setup_plan_apply.string_of_outcome other)

let suite =
  [
    ("current room admin allowed", `Quick, test_current_room_admin_allowed);
    ("guest denied", `Quick, test_guest_denied);
    ("global admin cross-room", `Quick, test_global_admin_cross_room);
    ("cross-room requires consent", `Quick, test_cross_room_requires_consent);
    ("NL and callback never confirm", `Quick, test_nl_and_callback_never_confirm);
    ("grant rejects NL", `Quick, test_grant_rejects_nl);
    ("grant rejects callback", `Quick, test_grant_rejects_callback);
    ( "cross-room with explicit consent",
      `Quick,
      test_cross_room_with_explicit_consent );
    ( "grant rejects non-destination admin",
      `Quick,
      test_grant_rejects_non_destination_admin );
    ("grant rejects non-admin", `Quick, test_grant_rejects_non_admin);
    ("authority_check integration", `Quick, test_authority_check_integration);
    ("apply uses consent authority", `Quick, test_apply_uses_consent_authority);
  ]

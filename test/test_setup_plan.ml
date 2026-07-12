(** Tests for reusable typed admin setup plans (P19.M1.E1.T001). *)

let sample_principal =
  Setup_plan.
    { id = "principal:test-user"; kind = Principal; label = Some "Test User" }

let sample_source =
  Setup_plan.
    {
      room_id = Some "room-source";
      session_key = Some "teams:room-source:user";
      connector = Some "teams";
      profile_id = None;
      extra = [];
    }

let sample_dest =
  Setup_plan.
    {
      room_id = Some "room-dest";
      session_key = None;
      connector = Some "teams";
      profile_id = Some "profile-a";
      extra = [ ("note", `String "target room") ];
    }

let make_clean_plan ?now ?id ?(planned_model = "openai:gpt-5.4") () =
  let current =
    `Assoc
      [
        ("profile_id", `String "profile-a"); ("model", `String "openai:gpt-4o");
      ]
  in
  let planned =
    `Assoc
      [ ("profile_id", `String "profile-a"); ("model", `String planned_model) ]
  in
  let diff =
    [
      Setup_plan.Update
        {
          path = "room_profiles.profile-a.model";
          from_ = `String "openai:gpt-4o";
          to_ = `String planned_model;
        };
    ]
  in
  Setup_plan.make ~principal:sample_principal ~source:sample_source
    ~destination:sample_dest ~current_state:current ~planned_state:planned ~diff
    ~readiness:
      [ { name = "Profile ID"; status = Setup_plan.Pass; message = "ok" } ]
    ~warnings:[] ~base_revision:"rev-abc"
    ~apply_payload:
      {
        kind = Setup_plan.Room_profile;
        ops =
          `List
            [
              `Assoc
                [
                  ("op", `String "upsert_profile");
                  ("id", `String "profile-a");
                  ("model", `String planned_model);
                ];
            ];
        data = `Assoc [];
      }
    ?now ?id ()

let test_make_populates_contract_fields () =
  let now = 1_700_000_000.0 in
  let plan = make_clean_plan ~now ~id:"plan_fixed_1" () in
  Alcotest.(check string) "id" "plan_fixed_1" plan.id;
  Alcotest.(check string) "principal id" "principal:test-user" plan.principal.id;
  Alcotest.(check (option string))
    "source room" (Some "room-source") plan.source.room_id;
  Alcotest.(check (option string))
    "dest room" (Some "room-dest") plan.destination.room_id;
  Alcotest.(check bool)
    "current_state set" true
    (match plan.current_state with `Assoc _ -> true | _ -> false);
  Alcotest.(check bool)
    "planned_state set" true
    (match plan.planned_state with `Assoc _ -> true | _ -> false);
  Alcotest.(check int) "diff length" 1 (List.length plan.diff);
  Alcotest.(check int) "readiness length" 1 (List.length plan.readiness);
  Alcotest.(check int) "warnings length" 0 (List.length plan.warnings);
  Alcotest.(check string) "base_revision" "rev-abc" plan.base_revision;
  Alcotest.(check bool)
    "created_at non-empty" true
    (String.length plan.created_at > 0);
  Alcotest.(check bool)
    "expires_at non-empty" true
    (String.length plan.expires_at > 0);
  Alcotest.(check bool) "digest non-empty" true (String.length plan.digest = 64);
  Alcotest.(check bool)
    "apply kind" true
    (match plan.apply_payload.kind with
    | Setup_plan.Room_profile -> true
    | _ -> false)

let test_digest_stable_under_key_reorder () =
  let plan = make_clean_plan ~now:1_700_000_000.0 ~id:"plan_stable" () in
  let d1 = Setup_plan.compute_digest plan in
  (* Rebuild planned_state with reverse key order; sort_json_keys should stabilize. *)
  let reordered =
    {
      plan with
      planned_state =
        `Assoc
          [
            ("model", `String "openai:gpt-5.4");
            ("profile_id", `String "profile-a");
          ];
      digest = "";
    }
  in
  let d2 = Setup_plan.compute_digest reordered in
  Alcotest.(check string) "digest stable" d1 d2;
  Alcotest.(check string) "matches stored" plan.digest d1

let test_digest_changes_when_planned_state_changes () =
  let a = make_clean_plan ~now:1_700_000_000.0 ~id:"plan_a" () in
  let b =
    make_clean_plan ~now:1_700_000_000.0 ~id:"plan_a"
      ~planned_model:"openai:gpt-5.3" ()
  in
  Alcotest.(check bool)
    "digests differ" true
    (not (Setup_plan.digests_equal a.digest b.digest))

let test_digest_changes_when_apply_payload_changes () =
  let a = make_clean_plan ~now:1_700_000_000.0 ~id:"plan_b" () in
  let b =
    {
      a with
      apply_payload =
        { a.apply_payload with data = `Assoc [ ("extra", `String "x") ] };
      digest = "";
    }
  in
  let d = Setup_plan.compute_digest b in
  Alcotest.(check bool)
    "payload changes digest" true
    (not (Setup_plan.digests_equal a.digest d))

let test_digest_excludes_self () =
  let plan = make_clean_plan ~now:1_700_000_000.0 ~id:"plan_c" () in
  let flipped = { plan with digest = String.make 64 'f' } in
  Alcotest.(check string)
    "recompute ignores stored digest" plan.digest
    (Setup_plan.compute_digest flipped)

let json_contains_redacted j =
  let s = Yojson.Safe.to_string j in
  String_util.contains s "***"

let test_redact_strips_secret_keys () =
  let dirty_state =
    `Assoc
      [
        ("bot_token", `String "xoxb-secret-token-value");
        ("api_key", `String "sk-abc");
        ("signing_secret", `String "shh");
        ("model", `String "openai:gpt-5.4");
      ]
  in
  let plan =
    Setup_plan.make ~principal:sample_principal ~source:sample_source
      ~destination:sample_dest ~current_state:dirty_state
      ~planned_state:dirty_state ~diff:[] ~readiness:[] ~warnings:[]
      ~base_revision:"rev" ~now:1_700_000_000.0 ~id:"plan_secret"
      ~apply_payload:
        {
          kind = Setup_plan.Generic "test";
          ops = dirty_state;
          data = dirty_state;
        }
      ()
  in
  let persist = Setup_plan.to_persist_json plan in
  let render = Setup_plan.to_render_json plan in
  Alcotest.(check bool) "persist redacts" true (json_contains_redacted persist);
  Alcotest.(check bool) "render redacts" true (json_contains_redacted render);
  Alcotest.(check bool)
    "no raw bot_token value" false
    (String_util.contains
       (Yojson.Safe.to_string persist)
       "xoxb-secret-token-value");
  Alcotest.(check bool)
    "summary has no raw secret" false
    (String_util.contains
       (Setup_plan.format_summary plan)
       "xoxb-secret-token-value")

let test_redact_preserves_digest_when_clean () =
  let plan = make_clean_plan ~now:1_700_000_000.0 ~id:"plan_clean" () in
  let again = Setup_plan.redact plan in
  Alcotest.(check string) "digest unchanged" plan.digest again.digest

let test_persist_round_trip () =
  let plan = make_clean_plan ~now:1_700_000_000.0 ~id:"plan_rt" () in
  match Setup_plan.of_persist_json (Setup_plan.to_persist_json plan) with
  | Error e -> Alcotest.fail e
  | Ok loaded ->
      Alcotest.(check string) "id" plan.id loaded.id;
      Alcotest.(check string) "digest" plan.digest loaded.digest;
      Alcotest.(check string)
        "base_revision" plan.base_revision loaded.base_revision;
      Alcotest.(check string) "expires_at" plan.expires_at loaded.expires_at;
      Alcotest.(check (option string))
        "dest room" plan.destination.room_id loaded.destination.room_id

let test_expiry () =
  let now = 1_700_000_000.0 in
  let plan =
    make_clean_plan ~now ~id:"plan_exp" () |> fun p ->
    (* Default TTL 900s *)
    p
  in
  Alcotest.(check bool)
    "not expired at create" false
    (Setup_plan.is_expired ~now plan);
  Alcotest.(check bool)
    "not expired just before ttl" false
    (Setup_plan.is_expired ~now:(now +. 899.) plan);
  Alcotest.(check bool)
    "expired after ttl" true
    (Setup_plan.is_expired ~now:(now +. 901.) plan)

let test_readiness_ok () =
  let ok = make_clean_plan ~now:1_700_000_000.0 ~id:"plan_ok" () in
  Alcotest.(check bool) "pass only" true (Setup_plan.readiness_ok ok);
  let fail =
    {
      ok with
      readiness =
        [ { name = "Bundle"; status = Setup_plan.Fail; message = "missing" } ];
    }
  in
  Alcotest.(check bool) "has fail" false (Setup_plan.readiness_ok fail)

let test_digests_equal () =
  Alcotest.(check bool) "equal" true (Setup_plan.digests_equal "abc" "abc");
  Alcotest.(check bool) "unequal" false (Setup_plan.digests_equal "abc" "abd")

let test_base_revision_of_config () =
  let cfg = Config_loader.parse_config (`Assoc []) in
  let expected = Access_snapshot.config_hash cfg in
  Alcotest.(check string)
    "matches access_snapshot" expected
    (Setup_plan.base_revision_of_config cfg)

let test_make_is_pure_no_config_mutation () =
  (* Build two plans; nothing about global config should be required or mutated.
     This is a behavioral guard: make only needs pure inputs. *)
  let p1 = make_clean_plan ~now:1.0 ~id:"p1" () in
  let p2 = make_clean_plan ~now:1.0 ~id:"p1" () in
  Alcotest.(check string) "deterministic digest" p1.digest p2.digest;
  Alcotest.(check bool) "readiness_ok" true (Setup_plan.readiness_ok p1)

let test_redact_path_scalar_secrets_in_diff () =
  let plan =
    Setup_plan.make ~principal:sample_principal ~source:sample_source
      ~destination:sample_dest ~current_state:(`Assoc [])
      ~planned_state:(`Assoc [])
      ~diff:
        [
          Setup_plan.Update
            {
              path = "channels.slack.bot_token";
              from_ = `String "xoxb-old-secret";
              to_ = `String "xoxb-new-secret";
            };
          Setup_plan.Create
            {
              path = "profile.credential_handle";
              value = `String "cred_handle_abc";
            };
        ]
      ~readiness:[] ~warnings:[] ~base_revision:"rev" ~now:1_700_000_000.0
      ~id:"plan_path_secret"
      ~apply_payload:
        {
          kind = Setup_plan.Generic "test";
          ops =
            `List
              [
                `Assoc
                  [
                    ("op", `String "set");
                    ("path", `String "bot_token");
                    ("value", `String "xoxb-apply-secret");
                  ];
              ];
          data = `Assoc [];
        }
      ()
  in
  let persist = Yojson.Safe.to_string (Setup_plan.to_persist_json plan) in
  Alcotest.(check bool)
    "no raw old secret" false
    (String_util.contains persist "xoxb-old-secret");
  Alcotest.(check bool)
    "no raw new secret" false
    (String_util.contains persist "xoxb-new-secret");
  Alcotest.(check bool)
    "no raw apply secret" false
    (String_util.contains persist "xoxb-apply-secret");
  Alcotest.(check bool)
    "credential handle preserved" true
    (String_util.contains persist "cred_handle_abc");
  Alcotest.(check bool)
    "redacted markers present" true
    (String_util.contains persist "***")

let test_digest_mismatch_on_load () =
  let plan = make_clean_plan ~now:1_700_000_000.0 ~id:"plan_bad" () in
  let j = Setup_plan.to_persist_json plan in
  let tampered =
    match j with
    | `Assoc fields ->
        `Assoc
          (List.map
             (fun (k, v) ->
               if k = "base_revision" then (k, `String "tampered") else (k, v))
             fields)
    | other -> other
  in
  match Setup_plan.of_persist_json tampered with
  | Ok _ -> Alcotest.fail "expected digest mismatch"
  | Error msg ->
      Alcotest.(check bool)
        "digest mismatch message" true
        (String_util.contains msg "digest")

let test_diff_ops_round_trip () =
  let diff =
    [
      Setup_plan.Create { path = "a.b"; value = `String "v" };
      Setup_plan.Update { path = "a.c"; from_ = `String "1"; to_ = `String "2" };
      Setup_plan.Delete { path = "a.d"; old = `String "gone" };
      Setup_plan.Bind { path = "binding"; target = "room-1"; active = true };
      Setup_plan.Note { path = "a"; message = "note" };
    ]
  in
  let plan =
    Setup_plan.make ~principal:sample_principal ~source:sample_source
      ~destination:sample_dest ~current_state:(`Assoc [])
      ~planned_state:(`Assoc []) ~diff ~readiness:[] ~warnings:[]
      ~base_revision:"rev" ~now:1_700_000_000.0 ~id:"plan_ops"
      ~apply_payload:
        {
          kind = Setup_plan.Generic "roundtrip";
          ops = `List [];
          data = `Assoc [ ("k", `String "v") ];
        }
      ()
  in
  match Setup_plan.of_persist_json (Setup_plan.to_persist_json plan) with
  | Error e -> Alcotest.fail e
  | Ok loaded ->
      Alcotest.(check int) "diff ops" 5 (List.length loaded.diff);
      Alcotest.(check bool)
        "generic kind" true
        (match loaded.apply_payload.kind with
        | Setup_plan.Generic "roundtrip" -> true
        | _ -> false)

let suite =
  [
    ( "make populates contract fields",
      `Quick,
      test_make_populates_contract_fields );
    ( "digest stable under key reorder",
      `Quick,
      test_digest_stable_under_key_reorder );
    ( "digest changes when planned_state changes",
      `Quick,
      test_digest_changes_when_planned_state_changes );
    ( "digest changes when apply_payload changes",
      `Quick,
      test_digest_changes_when_apply_payload_changes );
    ("digest excludes self", `Quick, test_digest_excludes_self);
    ("redact strips secret keys", `Quick, test_redact_strips_secret_keys);
    ( "redact preserves digest when clean",
      `Quick,
      test_redact_preserves_digest_when_clean );
    ("persist round-trip", `Quick, test_persist_round_trip);
    ("expiry", `Quick, test_expiry);
    ("readiness_ok", `Quick, test_readiness_ok);
    ("digests_equal", `Quick, test_digests_equal);
    ("base_revision_of_config", `Quick, test_base_revision_of_config);
    ("make is pure", `Quick, test_make_is_pure_no_config_mutation);
    ( "redact path scalar secrets in diff",
      `Quick,
      test_redact_path_scalar_secrets_in_diff );
    ("digest mismatch on load", `Quick, test_digest_mismatch_on_load);
    ("diff ops round-trip", `Quick, test_diff_ops_round_trip);
  ]

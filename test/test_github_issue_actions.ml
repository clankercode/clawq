(** Tests for confirmed Issue creation and lifecycle actions (P19.M4.E2.T005).
*)

module S = Github_route_store
module A = Github_issue_actions
module W = Github_action_workflow

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Setup_plan_apply.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let base_revision = "rev-config-1"
let room_id = "room-teams-1"
let room = S.Room room_id
let item_key = "item:acme/widget:issue:7"
let repo = "acme/widget"

let principal =
  Setup_plan.{ id = "principal:alice"; kind = Principal; label = Some "Alice" }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let contains hay needle =
  let hay = String.lowercase_ascii hay in
  let needle = String.lowercase_ascii needle in
  let n = String.length needle in
  let h = String.length hay in
  if n = 0 then true
  else if n > h then false
  else
    let rec loop i =
      if i > h - n then false
      else if String.sub hay i n = needle then true
      else loop (i + 1)
    in
    loop 0

let caps ~close ~create : S.capability_policy =
  {
    allow_reply = false;
    allow_label = false;
    allow_assign = false;
    allow_review = false;
    allow_merge = false;
    allow_close = close;
    extra = (if create then [ ("allow_create", true) ] else []);
  }

let make_route ~id ~policy : S.t =
  {
    id;
    destination = room;
    selector = S.Repo repo;
    filter = S.default_filter;
    comment_mode = S.default_comment_mode;
    capability_policy = policy;
    enabled = true;
    revision = "1";
    managed_bundle_id = None;
    managed_feature_id = None;
    provenance =
      {
        created_by = Some "test";
        created_via = Some "test";
        setup_plan_id = None;
        notes = None;
      };
    created_at = "2024-01-01T00:00:00Z";
    updated_at = "2024-01-01T00:00:00Z";
  }

let pilot_on =
  {
    A.enabled = true;
    pilot_name = "p19-issue-lifecycle-pilot";
    expires_at = Some "2099-01-01T00:00:00Z";
  }

let pilot_off = A.default_pilot_gate

let pilot_expired =
  {
    A.enabled = true;
    pilot_name = "p19-issue-lifecycle-pilot";
    expires_at = Some "2020-01-01T00:00:00Z";
  }

let expect_allowed = function
  | A.Allowed { action = _; capability } -> capability
  | A.Denied { reason } -> Alcotest.fail ("unexpected Denied: " ^ reason)

let expect_denied = function
  | A.Denied { reason } -> reason
  | A.Allowed { capability; _ } ->
      Alcotest.fail ("unexpected Allowed: " ^ capability)

let create_action =
  A.Create
    {
      repo_full_name = repo;
      title = "Flaky CI on main";
      body = Some "Repro steps…";
      labels = [ "bug"; "ci" ];
    }

let close_action =
  A.Close
    { item_key; state_reason = Some "completed"; comment = Some "Fixed in #8" }

(* 1. create denied when pilot off *)
let test_create_denied_when_pilot_off () =
  let route =
    make_route ~id:"rt_create_off" ~policy:(caps ~close:false ~create:true)
  in
  let reason =
    expect_denied
      (A.authorize ~route:(Some route) ~pilot:pilot_off
         ~user_auth_available:false ~action:create_action ~now:fixed_now ())
  in
  Alcotest.(check bool) "mentions pilot" true (contains reason "pilot");
  Alcotest.(check bool)
    "not production-ready" true
    (contains reason "production");
  Alcotest.(check bool)
    "no App/PAT fallback" true
    (contains reason "fallback" || contains reason "user")

(* 2. create allowed when pilot on + allow_create *)
let test_create_allowed_when_pilot_on () =
  let route =
    make_route ~id:"rt_create_on" ~policy:(caps ~close:false ~create:true)
  in
  let cap =
    expect_allowed
      (A.authorize ~route:(Some route) ~pilot:pilot_on
         ~user_auth_available:false ~action:create_action ~now:fixed_now ())
  in
  Alcotest.(check string) "capability" "allow_create" cap

(* 3. create denied without allow_create / no route *)
let test_create_denied_without_capability () =
  let route =
    make_route ~id:"rt_create_cap" ~policy:(caps ~close:true ~create:false)
  in
  let reason =
    expect_denied
      (A.authorize ~route:(Some route) ~pilot:pilot_on
         ~user_auth_available:false ~action:create_action ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "mentions allow_create" true
    (contains reason "allow_create");
  let reason_missing =
    expect_denied
      (A.authorize ~route:None ~pilot:pilot_on ~user_auth_available:false
         ~action:create_action ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "missing route" true
    (contains reason_missing "no route")

(* 4. close/reopen/open require allow_close *)
let test_lifecycle_require_allow_close () =
  let allowed =
    make_route ~id:"rt_close_on" ~policy:(caps ~close:true ~create:false)
  in
  let denied =
    make_route ~id:"rt_close_off" ~policy:(caps ~close:false ~create:true)
  in
  let close_cap =
    expect_allowed
      (A.authorize ~route:(Some allowed) ~pilot:pilot_on
         ~user_auth_available:false ~action:close_action ~now:fixed_now ())
  in
  Alcotest.(check string) "close capability" "allow_close" close_cap;
  let reopen = A.Reopen { item_key; comment = None } in
  let open_act = A.Open { item_key; comment = Some "still needed" } in
  ignore
    (expect_allowed
       (A.authorize ~route:(Some allowed) ~pilot:pilot_on
          ~user_auth_available:false ~action:reopen ~now:fixed_now ()));
  ignore
    (expect_allowed
       (A.authorize ~route:(Some allowed) ~pilot:pilot_on
          ~user_auth_available:false ~action:open_act ~now:fixed_now ()));
  let reason =
    expect_denied
      (A.authorize ~route:(Some denied) ~pilot:pilot_on
         ~user_auth_available:false ~action:close_action ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "mentions allow_close" true
    (contains reason "allow_close")

(* 5. pilot expired denied *)
let test_pilot_expired_denied () =
  let route =
    make_route ~id:"rt_expired" ~policy:(caps ~close:true ~create:true)
  in
  let reason =
    expect_denied
      (A.authorize ~route:(Some route) ~pilot:pilot_expired
         ~user_auth_available:false ~action:close_action ~now:fixed_now ())
  in
  Alcotest.(check bool) "mentions expired" true (contains reason "expired")

(* 6. plan_create stores Generic github_issue_create without secrets *)
let test_plan_create_no_secrets () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_plan_create" ~policy:(caps ~close:false ~create:true)
  in
  let plan =
    assert_ok
      (A.plan_create ~db ~principal ~room_id ~pilot:pilot_on
         ~user_auth_available:false ~repo_full_name:repo ~title:"New bug"
         ~body:"details" ~labels:[ "bug" ] ~base_revision ~route ~now:fixed_now
         ())
  in
  (match plan.apply_payload.kind with
  | Setup_plan.Generic "github_issue_create" -> ()
  | Setup_plan.Generic other ->
      Alcotest.fail ("unexpected generic kind: " ^ other)
  | _ -> Alcotest.fail "expected Generic github_issue_create");
  Alcotest.(check bool) "readiness ok" true (Setup_plan.readiness_ok plan);
  Alcotest.(check string) "base_revision" base_revision plan.base_revision;
  (match Setup_plan_apply.get_plan ~db ~plan_id:plan.id with
  | Some stored ->
      Alcotest.(check string) "stored id" plan.id stored.id;
      Alcotest.(check string) "digest" plan.digest stored.digest
  | None -> Alcotest.fail "plan not stored as pending");
  let persist = Yojson.Safe.to_string (Setup_plan.to_persist_json plan) in
  Alcotest.(check bool) "includes repo" true (contains persist repo);
  Alcotest.(check bool) "includes title" true (contains persist "new bug");
  Alcotest.(check bool)
    "includes pilot" true
    (contains persist "p19-issue-lifecycle-pilot");
  Alcotest.(check bool)
    "not production-ready" true
    (contains persist "production_ready");
  Alcotest.(check bool)
    "no token-like secret keys" false
    (contains persist "bot_token"
    || contains persist "signing_secret"
    || contains persist "api_key"
    || contains persist "private_key"
    || contains persist "ghp_");
  (* Denied path does not create a plan. *)
  match
    A.plan_create ~db ~principal ~room_id ~pilot:pilot_off
      ~user_auth_available:false ~repo_full_name:repo ~title:"Nope"
      ~base_revision ~route ~now:(fixed_now +. 1.) ()
  with
  | Ok _ -> Alcotest.fail "create plan should be denied when pilot off"
  | Error msg ->
      Alcotest.(check bool) "deny mentions pilot" true (contains msg "pilot")

(* 7. plan_close / plan_reopen / plan_open kinds *)
let test_plan_lifecycle_kinds () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_lifecycle" ~policy:(caps ~close:true ~create:false)
  in
  let close_plan =
    assert_ok
      (A.plan_close ~db ~principal ~room_id ~pilot:pilot_on
         ~user_auth_available:false ~item_key ~state_reason:"completed"
         ~comment:"done" ~base_revision ~route ~now:fixed_now ())
  in
  (match close_plan.apply_payload.kind with
  | Setup_plan.Generic "github_issue_close" -> ()
  | _ -> Alcotest.fail "expected github_issue_close");
  let reopen_plan =
    assert_ok
      (A.plan_reopen ~db ~principal ~room_id ~pilot:pilot_on
         ~user_auth_available:false ~item_key ~base_revision ~route
         ~now:(fixed_now +. 1.) ())
  in
  (match reopen_plan.apply_payload.kind with
  | Setup_plan.Generic "github_issue_reopen" -> ()
  | _ -> Alcotest.fail "expected github_issue_reopen");
  let open_plan =
    assert_ok
      (A.plan_open ~db ~principal ~room_id ~pilot:pilot_on
         ~user_auth_available:false ~item_key ~comment:"re-open for QA"
         ~base_revision ~route ~now:(fixed_now +. 2.) ())
  in
  match open_plan.apply_payload.kind with
  | Setup_plan.Generic "github_issue_open" ->
      let s = Yojson.Safe.to_string (Setup_plan.to_persist_json open_plan) in
      Alcotest.(check bool) "has item_key" true (contains s item_key);
      Alcotest.(check bool) "has allow_close" true (contains s "allow_close")
  | _ -> Alcotest.fail "expected github_issue_open"

(* 8. apply receipt path via github_action_workflow (success, replay, stale) *)
let test_workflow_apply_receipt_path () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_apply" ~policy:(caps ~close:true ~create:true)
  in
  let plan =
    assert_ok
      (W.preview ~db ~principal ~room_id ~action:(W.Issue create_action)
         ~base_revision ~route ~issue_pilot:pilot_on ~user_auth_available:false
         ~now:fixed_now ())
  in
  Alcotest.(check bool) "github action plan" true (W.is_github_action_plan plan);
  Alcotest.(check string)
    "label" "issue"
    (W.action_kind_label (W.Issue create_action));
  (match plan.apply_payload.kind with
  | Setup_plan.Generic "github_issue_create" -> ()
  | _ -> Alcotest.fail "expected github_issue_create via workflow");
  match
    assert_ok
      (W.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest ~principal
         ~current_base_revision:base_revision ~now:fixed_now ())
  with
  | Setup_plan_apply.Rejected { reason; message } ->
      Alcotest.fail
        (Printf.sprintf "unexpected reject %s: %s"
           (Setup_plan_apply.string_of_reject_reason reason)
           message)
  | Setup_plan_apply.Applied { receipt_id; first_time } -> (
      Alcotest.(check bool) "first apply" true first_time;
      Alcotest.(check bool)
        "receipt non-empty" true
        (String.length receipt_id > 0);
      (* Idempotent retry / replay. *)
      (match
         assert_ok
           (W.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest
              ~principal ~current_base_revision:base_revision
              ~now:(fixed_now +. 1.) ())
       with
      | Setup_plan_apply.Applied { receipt_id = r2; first_time = false } ->
          Alcotest.(check string) "same receipt" receipt_id r2
      | Setup_plan_apply.Applied { first_time = true; _ } ->
          Alcotest.fail "retry should be idempotent"
      | Setup_plan_apply.Rejected { message; _ } ->
          Alcotest.fail ("retry rejected: " ^ message));
      (* Wrong digest rejects. *)
      let plan2 =
        assert_ok
          (W.preview ~db ~principal ~room_id ~action:(W.Issue close_action)
             ~base_revision ~route ~issue_pilot:pilot_on
             ~user_auth_available:false ~now:(fixed_now +. 2.) ())
      in
      (match
         assert_ok
           (W.apply_confirmed ~db ~plan_id:plan2.id
              ~digest:"deadbeef_wrong_digest" ~principal
              ~current_base_revision:base_revision ~now:(fixed_now +. 2.) ())
       with
      | Setup_plan_apply.Rejected { reason; _ } ->
          Alcotest.(check string)
            "digest" "digest_mismatch"
            (Setup_plan_apply.string_of_reject_reason reason)
      | Setup_plan_apply.Applied _ -> Alcotest.fail "expected digest mismatch");
      (* Stale revision. *)
      let plan3 =
        assert_ok
          (W.preview ~db ~principal ~room_id
             ~action:(W.Issue (A.Reopen { item_key; comment = None }))
             ~base_revision ~route ~issue_pilot:pilot_on
             ~user_auth_available:false ~now:(fixed_now +. 3.) ())
      in
      match
        assert_ok
          (W.apply_confirmed ~db ~plan_id:plan3.id ~digest:plan3.digest
             ~principal ~current_base_revision:"rev-stale"
             ~now:(fixed_now +. 3.) ())
      with
      | Setup_plan_apply.Rejected { reason; _ } ->
          Alcotest.(check string)
            "stale" "stale_revision"
            (Setup_plan_apply.string_of_reject_reason reason)
      | Setup_plan_apply.Applied _ -> Alcotest.fail "expected stale revision")

(* 9. invalid inputs / state_reason *)
let test_validation_denies_bad_inputs () =
  let route =
    make_route ~id:"rt_valid" ~policy:(caps ~close:true ~create:true)
  in
  let reason =
    expect_denied
      (A.authorize ~route:(Some route) ~pilot:pilot_on
         ~user_auth_available:false
         ~action:
           (A.Create
              { repo_full_name = repo; title = "  "; body = None; labels = [] })
         ~now:fixed_now ())
  in
  Alcotest.(check bool) "empty title" true (contains reason "title");
  let reason2 =
    expect_denied
      (A.authorize ~route:(Some route) ~pilot:pilot_on
         ~user_auth_available:false
         ~action:
           (A.Close
              { item_key; state_reason = Some "bogus_reason"; comment = None })
         ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "bad state_reason" true
    (contains reason2 "state_reason")

(* 10. receipt_safe_error redacts tokens (provider failure path) *)
let test_receipt_safe_error_redacts_token () =
  let raw =
    "GitHub rejected issue close: Authorization: Bearer \
     ghp_SUPERSECRETtokenvalue123456 projection failed token=abc123secret"
  in
  let safe = A.receipt_safe_error raw in
  Alcotest.(check bool)
    "redacts ghp_ token" false
    (contains safe "ghp_SUPERSECRETtokenvalue123456");
  Alcotest.(check bool)
    "redacts token= value" false
    (contains safe "abc123secret");
  let plain = A.receipt_safe_error "projection failed: issue already closed" in
  Alcotest.(check bool)
    "plain error preserved" true
    (contains plain "already closed")

let suite =
  [
    ("create denied when pilot off", `Quick, test_create_denied_when_pilot_off);
    ( "create allowed when pilot on + allow_create",
      `Quick,
      test_create_allowed_when_pilot_on );
    ( "create denied without allow_create",
      `Quick,
      test_create_denied_without_capability );
    ("lifecycle require allow_close", `Quick, test_lifecycle_require_allow_close);
    ("pilot expired denied", `Quick, test_pilot_expired_denied);
    ("plan_create stores without secrets", `Quick, test_plan_create_no_secrets);
    ("plan lifecycle kinds", `Quick, test_plan_lifecycle_kinds);
    ("workflow apply receipt path", `Quick, test_workflow_apply_receipt_path);
    ("validation denies bad inputs", `Quick, test_validation_denies_bad_inputs);
    ( "receipt_safe_error redacts token",
      `Quick,
      test_receipt_safe_error_redacts_token );
  ]

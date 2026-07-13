(** Tests for confirmed typed GitHub workflow_dispatch (P19.M4.E2.T006). *)

module S = Github_route_store
module A = Github_workflow_dispatch
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
let repo = "acme/widget"
let workflow_file = "deploy.yml"
let ref_ = "main"

let principal =
  Setup_plan.{ id = "principal:alice"; kind = Principal; label = Some "Alice" }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let caps ?(dispatch = false) () : S.capability_policy =
  {
    allow_reply = false;
    allow_label = false;
    allow_assign = false;
    allow_review = false;
    allow_merge = false;
    allow_close = false;
    extra =
      (if dispatch then [ (A.capability_key, true) ]
       else [ (A.capability_key, false) ]);
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

let pilot_on =
  {
    A.enabled = true;
    pilot_name = "p19-workflow-dispatch-pilot";
    expires_at = Some "2099-01-01T00:00:00Z";
  }

let pilot_off = A.default_pilot_gate

let pilot_expired =
  {
    A.enabled = true;
    pilot_name = "p19-workflow-dispatch-pilot";
    expires_at = Some "2020-01-01T00:00:00Z";
  }

let base_req ?(inputs = [ ("environment", "staging"); ("dry_run", "true") ])
    ?(allowed = Some [ "environment"; "dry_run"; "force" ]) () : A.request =
  {
    repo_full_name = repo;
    workflow_id = workflow_file;
    ref_;
    inputs;
    item_key = Some "item:acme/widget:pr:7";
    allowed_input_names = allowed;
  }

(* 1. authorize allowed when pilot on + capability *)
let test_authorize_allowed_with_capability () =
  let route = make_route ~id:"rt_wd_on" ~policy:(caps ~dispatch:true ()) in
  match
    A.authorize ~route:(Some route) ~pilot:pilot_on ~user_auth_available:false
      ~req:(base_req ()) ~now:fixed_now ()
  with
  | Ok () -> ()
  | Error e -> Alcotest.fail ("unexpected deny: " ^ e)

(* 2. deny without capability *)
let test_deny_without_capability () =
  let route = make_route ~id:"rt_wd_off" ~policy:(caps ~dispatch:false ()) in
  (match
     A.authorize ~route:(Some route) ~pilot:pilot_on ~user_auth_available:false
       ~req:(base_req ()) ~now:fixed_now ()
   with
  | Ok () -> Alcotest.fail "expected deny without workflow_dispatch capability"
  | Error msg ->
      Alcotest.(check bool)
        "mentions workflow_dispatch" true
        (contains msg "workflow_dispatch"));
  match
    A.authorize ~route:None ~pilot:pilot_on ~user_auth_available:false
      ~req:(base_req ()) ~now:fixed_now ()
  with
  | Ok () -> Alcotest.fail "expected deny without route"
  | Error msg ->
      Alcotest.(check bool) "mentions no route" true (contains msg "no route")

(* 3. deny when pilot off *)
let test_deny_when_pilot_off () =
  let route = make_route ~id:"rt_pilot_off" ~policy:(caps ~dispatch:true ()) in
  match
    A.authorize ~route:(Some route) ~pilot:pilot_off ~user_auth_available:false
      ~req:(base_req ()) ~now:fixed_now ()
  with
  | Ok () -> Alcotest.fail "expected deny when pilot off"
  | Error msg ->
      Alcotest.(check bool) "mentions pilot" true (contains msg "pilot");
      Alcotest.(check bool)
        "not production-ready" true
        (contains msg "production");
      Alcotest.(check bool)
        "no App/PAT fallback when user auth unavailable" true
        (contains msg "fallback" || contains msg "user")

(* 4. pilot expired denied *)
let test_pilot_expired_denied () =
  let route = make_route ~id:"rt_expired" ~policy:(caps ~dispatch:true ()) in
  match
    A.authorize ~route:(Some route) ~pilot:pilot_expired
      ~user_auth_available:false ~req:(base_req ()) ~now:fixed_now ()
  with
  | Ok () -> Alcotest.fail "expected deny when pilot expired"
  | Error msg ->
      Alcotest.(check bool) "mentions expired" true (contains msg "expired")

(* 5. unknown inputs fail closed *)
let test_unknown_inputs_fail_closed () =
  let route = make_route ~id:"rt_unknown" ~policy:(caps ~dispatch:true ()) in
  let req =
    base_req
      ~inputs:[ ("environment", "prod"); ("evil_flag", "1") ]
      ~allowed:(Some [ "environment"; "dry_run" ])
      ()
  in
  match
    A.authorize ~route:(Some route) ~pilot:pilot_on ~user_auth_available:true
      ~req ~now:fixed_now ()
  with
  | Ok () -> Alcotest.fail "expected deny for unknown input"
  | Error msg ->
      Alcotest.(check bool) "mentions unknown" true (contains msg "unknown");
      Alcotest.(check bool) "mentions evil_flag" true (contains msg "evil_flag")

(* 6. empty workflow/ref denied *)
let test_empty_workflow_or_ref_denied () =
  let route = make_route ~id:"rt_empty" ~policy:(caps ~dispatch:true ()) in
  let req_empty_wf = { (base_req ()) with workflow_id = "  " } in
  (match
     A.authorize ~route:(Some route) ~pilot:pilot_on ~user_auth_available:true
       ~req:req_empty_wf ~now:fixed_now ()
   with
  | Ok () -> Alcotest.fail "expected deny for empty workflow_id"
  | Error msg ->
      Alcotest.(check bool)
        "mentions workflow_id" true
        (contains msg "workflow_id"));
  let req_empty_ref = { (base_req ()) with ref_ = "" } in
  match
    A.authorize ~route:(Some route) ~pilot:pilot_on ~user_auth_available:true
      ~req:req_empty_ref ~now:fixed_now ()
  with
  | Ok () -> Alcotest.fail "expected deny for empty ref"
  | Error msg -> Alcotest.(check bool) "mentions ref" true (contains msg "ref")

(* 7. plan includes workflow/ref/inputs, secret-free *)
let test_plan_secret_free_inputs () =
  with_db @@ fun db ->
  let route = make_route ~id:"rt_plan" ~policy:(caps ~dispatch:true ()) in
  let req = base_req () in
  let plan =
    assert_ok
      (A.plan_dispatch ~db ~principal ~room_id ~pilot:pilot_on
         ~user_auth_available:false ~req ~base_revision ~route ~now:fixed_now ())
  in
  (match plan.apply_payload.kind with
  | Setup_plan.Generic "github_workflow_dispatch" -> ()
  | Setup_plan.Generic other ->
      Alcotest.fail ("unexpected generic kind: " ^ other)
  | _ -> Alcotest.fail "expected Generic github_workflow_dispatch");
  Alcotest.(check bool) "readiness ok" true (Setup_plan.readiness_ok plan);
  let persist = Yojson.Safe.to_string (Setup_plan.to_persist_json plan) in
  Alcotest.(check bool)
    "includes workflow file" true
    (contains persist workflow_file);
  Alcotest.(check bool) "includes ref" true (contains persist ref_);
  Alcotest.(check bool) "includes repo" true (contains persist repo);
  Alcotest.(check bool)
    "includes environment input" true
    (contains persist "staging");
  Alcotest.(check bool)
    "includes dry_run input" true
    (contains persist "dry_run");
  Alcotest.(check bool)
    "includes pilot name" true
    (contains persist "p19-workflow-dispatch-pilot");
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
  (* Secret-shaped input keys are rejected at authorize/plan time. *)
  let secret_req =
    base_req ~inputs:[ ("api_key", "should-not-plan") ] ~allowed:None ()
  in
  match
    A.plan_dispatch ~db ~principal ~room_id ~pilot:pilot_on
      ~user_auth_available:false ~req:secret_req ~base_revision ~route
      ~now:(fixed_now +. 1.) ()
  with
  | Ok _ -> Alcotest.fail "expected plan deny for secret-shaped input key"
  | Error msg ->
      Alcotest.(check bool)
        "mentions secret-shaped" true
        (contains msg "secret" || contains msg "api_key")

(* 8. plan-confirm-apply via shared workflow *)
let test_plan_confirm_apply_via_shared_workflow () =
  with_db @@ fun db ->
  let route = make_route ~id:"rt_apply" ~policy:(caps ~dispatch:true ()) in
  let req = base_req () in
  let plan =
    assert_ok
      (W.preview ~db ~principal ~room_id ~action:(W.Workflow_dispatch req)
         ~base_revision ~route ~workflow_pilot:pilot_on
         ~user_auth_available:false ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "is github action plan" true
    (W.is_github_action_plan plan);
  Alcotest.(check string)
    "label" "workflow_dispatch"
    (W.action_kind_label (W.Workflow_dispatch req));
  match
    assert_ok
      (W.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest ~principal
         ~current_base_revision:base_revision ~now:(fixed_now +. 1.) ())
  with
  | Setup_plan_apply.Applied { first_time = true; _ } -> ()
  | Setup_plan_apply.Applied { first_time = false; _ } ->
      Alcotest.fail "expected first-time apply"
  | Setup_plan_apply.Rejected { message; _ } -> Alcotest.fail message

(* 9. capability helper defaults off *)
let test_capability_defaults_off () =
  let empty =
    {
      S.allow_reply = false;
      allow_label = false;
      allow_assign = false;
      allow_review = false;
      allow_merge = false;
      allow_close = false;
      extra = [];
    }
  in
  Alcotest.(check bool)
    "absent extra is off" false
    (A.has_workflow_dispatch_capability empty);
  Alcotest.(check bool)
    "false extra is off" false
    (A.has_workflow_dispatch_capability
       { empty with extra = [ (A.capability_key, false) ] });
  Alcotest.(check bool)
    "true extra is on" true
    (A.has_workflow_dispatch_capability
       { empty with extra = [ (A.capability_key, true) ] })

(* 10. receipt_safe_error redacts token *)
let test_receipt_safe_error_redacts_token () =
  let raw =
    "GitHub rejected workflow_dispatch: Authorization: Bearer \
     ghp_SUPERSECRETtokenvalue123456 projection failed token=abc123secret"
  in
  let safe = A.receipt_safe_error raw in
  Alcotest.(check bool)
    "redacts ghp_ token" false
    (contains safe "ghp_SUPERSECRETtokenvalue123456");
  Alcotest.(check bool)
    "redacts token= value" false
    (contains safe "abc123secret");
  let plain = A.receipt_safe_error "projection failed: workflow not found" in
  Alcotest.(check bool)
    "plain error preserved" true
    (contains plain "workflow not found")

let suite =
  [
    ( "authorize allowed with pilot + capability",
      `Quick,
      test_authorize_allowed_with_capability );
    ("deny without capability", `Quick, test_deny_without_capability);
    ("deny when pilot off", `Quick, test_deny_when_pilot_off);
    ("pilot expired denied", `Quick, test_pilot_expired_denied);
    ("unknown inputs fail closed", `Quick, test_unknown_inputs_fail_closed);
    ("empty workflow or ref denied", `Quick, test_empty_workflow_or_ref_denied);
    ("plan secret-free inputs", `Quick, test_plan_secret_free_inputs);
    ( "plan confirm apply via shared workflow",
      `Quick,
      test_plan_confirm_apply_via_shared_workflow );
    ("capability defaults off", `Quick, test_capability_defaults_off);
    ( "receipt_safe_error redacts token",
      `Quick,
      test_receipt_safe_error_redacts_token );
  ]

(** Tests for policy-gated PR reviewer requests and pilot-gated review
    submission (P19.M4.E1.T004). *)

module S = Github_route_store
module A = Github_pr_review_actions

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Setup_plan_apply.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let base_revision = "rev-config-1"
let room_id = "room-teams-1"
let room = S.Room room_id
let item_key = "item:acme/widget:pr:42"
let head_sha = "abc123def4567890abcdef1234567890abcdef12"

let principal =
  Setup_plan.{ id = "principal:alice"; kind = Principal; label = Some "Alice" }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let caps ~review : S.capability_policy =
  {
    allow_reply = false;
    allow_label = false;
    allow_assign = false;
    allow_review = review;
    allow_merge = false;
    allow_close = false;
    extra = [];
  }

let make_route ~id ~policy : S.t =
  {
    id;
    destination = room;
    selector = S.Repo "acme/widget";
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
    pilot_name = "p19-pr-review-pilot";
    expires_at = Some "2099-01-01T00:00:00Z";
  }

let pilot_off = A.default_pilot_gate

let pilot_expired =
  {
    A.enabled = true;
    pilot_name = "p19-pr-review-pilot";
    expires_at = Some "2020-01-01T00:00:00Z";
  }

(* 1. request reviewers allowed with allow_review *)
let test_request_reviewers_allowed_with_allow_review () =
  let route = make_route ~id:"rt_review_on" ~policy:(caps ~review:true) in
  let req =
    { A.item_key; reviewers = [ "bob"; "carol" ]; head_sha = Some head_sha }
  in
  match A.authorize_request_reviewers ~route:(Some route) ~req with
  | Ok () -> ()
  | Error e -> Alcotest.fail ("unexpected deny: " ^ e)

(* 2. request denied without capability *)
let test_request_denied_without_capability () =
  let route = make_route ~id:"rt_review_off" ~policy:(caps ~review:false) in
  let req = { A.item_key; reviewers = [ "bob" ]; head_sha = None } in
  (match A.authorize_request_reviewers ~route:(Some route) ~req with
  | Ok () -> Alcotest.fail "expected deny without allow_review"
  | Error msg ->
      Alcotest.(check bool)
        "mentions allow_review" true
        (contains msg "allow_review"));
  match A.authorize_request_reviewers ~route:None ~req with
  | Ok () -> Alcotest.fail "expected deny without route"
  | Error msg ->
      Alcotest.(check bool) "mentions no route" true (contains msg "no route")

(* 3. submit denied when pilot off *)
let test_submit_denied_when_pilot_off () =
  let route = make_route ~id:"rt_submit_off" ~policy:(caps ~review:true) in
  let req =
    {
      A.item_key;
      kind = A.Approve;
      head_sha;
      body = Some "LGTM";
      actor_login = Some "alice";
    }
  in
  match
    A.authorize_submit_review ~route:(Some route) ~pilot:pilot_off
      ~user_auth_available:false ~req ~now:fixed_now ()
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

(* 4. submit allowed when pilot on + allow_review + head_sha *)
let test_submit_allowed_when_pilot_on () =
  let route = make_route ~id:"rt_submit_on" ~policy:(caps ~review:true) in
  let req =
    {
      A.item_key;
      kind = A.Request_changes;
      head_sha;
      body = Some "Please fix CI";
      actor_login = Some "alice";
    }
  in
  match
    A.authorize_submit_review ~route:(Some route) ~pilot:pilot_on
      ~user_auth_available:false ~req ~now:fixed_now ()
  with
  | Ok () -> ()
  | Error e -> Alcotest.fail ("unexpected deny: " ^ e)

(* 5. submit denied empty head_sha *)
let test_submit_denied_empty_head_sha () =
  let route = make_route ~id:"rt_empty_sha" ~policy:(caps ~review:true) in
  let req =
    {
      A.item_key;
      kind = A.Comment;
      head_sha = "   ";
      body = Some "nit";
      actor_login = None;
    }
  in
  match
    A.authorize_submit_review ~route:(Some route) ~pilot:pilot_on
      ~user_auth_available:true ~req ~now:fixed_now ()
  with
  | Ok () -> Alcotest.fail "expected deny for empty head_sha"
  | Error msg ->
      Alcotest.(check bool) "mentions head_sha" true (contains msg "head_sha")

(* 6. pilot expired denied *)
let test_pilot_expired_denied () =
  let route = make_route ~id:"rt_expired" ~policy:(caps ~review:true) in
  let req =
    {
      A.item_key;
      kind = A.Approve;
      head_sha;
      body = None;
      actor_login = Some "alice";
    }
  in
  match
    A.authorize_submit_review ~route:(Some route) ~pilot:pilot_expired
      ~user_auth_available:false ~req ~now:fixed_now ()
  with
  | Ok () -> Alcotest.fail "expected deny when pilot expired"
  | Error msg ->
      Alcotest.(check bool) "mentions expired" true (contains msg "expired")

(* 7. plan includes head_sha not secrets *)
let test_plan_includes_head_sha_not_secrets () =
  with_db @@ fun db ->
  let route = make_route ~id:"rt_plan" ~policy:(caps ~review:true) in
  let req =
    {
      A.item_key;
      kind = A.Approve;
      head_sha;
      body = Some "Approved after CI green";
      actor_login = Some "alice";
    }
  in
  let plan =
    assert_ok
      (A.plan_submit_review ~db ~principal ~room_id ~pilot:pilot_on
         ~user_auth_available:false ~req ~base_revision ~route ~now:fixed_now ())
  in
  (match plan.apply_payload.kind with
  | Setup_plan.Generic "github_submit_review" -> ()
  | Setup_plan.Generic other ->
      Alcotest.fail ("unexpected generic kind: " ^ other)
  | _ -> Alcotest.fail "expected Generic github_submit_review");
  Alcotest.(check bool) "readiness ok" true (Setup_plan.readiness_ok plan);
  let persist = Yojson.Safe.to_string (Setup_plan.to_persist_json plan) in
  Alcotest.(check bool) "includes head_sha" true (contains persist head_sha);
  Alcotest.(check bool) "includes review kind" true (contains persist "approve");
  Alcotest.(check bool)
    "includes pilot name" true
    (contains persist "p19-pr-review-pilot");
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
  (* request_reviewers plan also works *)
  let req_plan =
    assert_ok
      (A.plan_request_reviewers ~db ~principal ~room_id
         ~req:{ A.item_key; reviewers = [ "bob" ]; head_sha = Some head_sha }
         ~base_revision ~route ~now:(fixed_now +. 1.) ())
  in
  match req_plan.apply_payload.kind with
  | Setup_plan.Generic "github_request_reviewers" ->
      let s = Yojson.Safe.to_string (Setup_plan.to_persist_json req_plan) in
      Alcotest.(check bool) "has bob" true (contains s "bob")
  | _ -> Alcotest.fail "expected Generic github_request_reviewers"

(* 8. receipt_safe_error redacts token *)
let test_receipt_safe_error_redacts_token () =
  let raw =
    "GitHub rejected review: Authorization: Bearer \
     ghp_SUPERSECRETtokenvalue123456 projection failed token=abc123secret"
  in
  let safe = A.receipt_safe_error raw in
  Alcotest.(check bool)
    "redacts ghp_ token" false
    (contains safe "ghp_SUPERSECRETtokenvalue123456");
  Alcotest.(check bool)
    "redacts bearer material" false
    (String_util.contains safe "ghp_SUPERSECRET");
  Alcotest.(check bool)
    "keeps actionable shape" true
    (contains safe "github" || contains safe "rejected"
   || contains safe "redacted");
  Alcotest.(check bool)
    "redacts token= value" false
    (contains safe "abc123secret");
  (* Plain projection error remains readable. *)
  let plain = A.receipt_safe_error "projection failed: head mismatch" in
  Alcotest.(check bool)
    "plain error preserved" true
    (contains plain "head mismatch")

let suite =
  [
    ( "request reviewers allowed with allow_review",
      `Quick,
      test_request_reviewers_allowed_with_allow_review );
    ( "request denied without capability",
      `Quick,
      test_request_denied_without_capability );
    ("submit denied when pilot off", `Quick, test_submit_denied_when_pilot_off);
    ( "submit allowed when pilot on + allow_review + head_sha",
      `Quick,
      test_submit_allowed_when_pilot_on );
    ("submit denied empty head_sha", `Quick, test_submit_denied_empty_head_sha);
    ("pilot expired denied", `Quick, test_pilot_expired_denied);
    ( "plan includes head_sha not secrets",
      `Quick,
      test_plan_includes_head_sha_not_secrets );
    ( "receipt_safe_error redacts token",
      `Quick,
      test_receipt_safe_error_redacts_token );
  ]

(** Tests for shared revision-bound GitHub action plan → confirm → apply
    (P19.M4.E2.T001). *)

module S = Github_route_store
module W = Github_action_workflow
module Collab = Github_collab_actions
module Review = Github_pr_review_actions

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

let caps ~reply ~review : S.capability_policy =
  {
    allow_reply = reply;
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

let pilot_on =
  {
    Review.enabled = true;
    pilot_name = "p19-pr-review-pilot";
    expires_at = Some "2099-01-01T00:00:00Z";
  }

let comment_action =
  W.Collab (Collab.Comment { item_key; body = "Looks good — confirm apply." })

(* 1. preview creates pending plan *)
let test_preview_creates_pending_plan () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_preview" ~policy:(caps ~reply:true ~review:false)
  in
  let plan =
    assert_ok
      (W.preview ~db ~principal ~room_id ~action:comment_action ~base_revision
         ~route ~now:fixed_now ())
  in
  Alcotest.(check bool) "github action plan" true (W.is_github_action_plan plan);
  Alcotest.(check string) "base_revision" base_revision plan.base_revision;
  Alcotest.(check bool) "readiness ok" true (Setup_plan.readiness_ok plan);
  (match Setup_plan_apply.get_plan ~db ~plan_id:plan.id with
  | Some stored ->
      Alcotest.(check string) "stored id" plan.id stored.id;
      Alcotest.(check string) "stored digest" plan.digest stored.digest;
      Alcotest.(check string)
        "stored base_revision" base_revision stored.base_revision
  | None -> Alcotest.fail "plan not stored as pending");
  let summary = Setup_plan.format_summary plan in
  Alcotest.(check bool)
    "summary has base revision" true
    (contains summary base_revision);
  Alcotest.(check bool)
    "summary has destination room" true (contains summary room_id);
  let planned = Yojson.Safe.to_string plan.planned_state in
  Alcotest.(check bool) "planned has target" true (contains planned item_key);
  Alcotest.(check bool)
    "planned has comment effect" true
    (contains planned "comment" || contains planned "looks good");
  Alcotest.(check bool)
    "diff shows create effect" true
    (List.exists
       (function
         | Setup_plan.Create { path; _ } -> contains path item_key | _ -> false)
       plan.diff)

(* 2. apply with wrong digest rejects *)
let test_apply_wrong_digest_rejects () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_digest" ~policy:(caps ~reply:true ~review:false)
  in
  let plan =
    assert_ok
      (W.preview ~db ~principal ~room_id ~action:comment_action ~base_revision
         ~route ~now:fixed_now ())
  in
  match
    assert_ok
      (W.apply_confirmed ~db ~plan_id:plan.id ~digest:"deadbeef_wrong_digest"
         ~principal ~current_base_revision:base_revision ~now:fixed_now ())
  with
  | Setup_plan_apply.Applied _ ->
      Alcotest.fail "expected digest mismatch reject"
  | Setup_plan_apply.Rejected { reason; message } ->
      Alcotest.(check string)
        "reason" "digest_mismatch"
        (Setup_plan_apply.string_of_reject_reason reason);
      Alcotest.(check bool)
        "message mentions digest" true
        (contains message "digest")

(* 3. no dispatcher means apply fails closed before receipt creation *)
let test_apply_fails_closed_without_dispatcher () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_apply" ~policy:(caps ~reply:true ~review:false)
  in
  let plan =
    assert_ok
      (W.preview ~db ~principal ~room_id ~action:comment_action ~base_revision
         ~route ~now:fixed_now ())
  in
  match
    assert_ok
      (W.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest ~principal
         ~current_base_revision:base_revision ~now:fixed_now ())
  with
  | Setup_plan_apply.Applied _ ->
      Alcotest.fail "apply must not create a receipt without a live dispatcher"
  | Setup_plan_apply.Rejected { reason; message } ->
      Alcotest.(check string)
        "reason" "apply_error"
        (Setup_plan_apply.string_of_reject_reason reason);
      Alcotest.(check bool)
        "mentions dispatcher" true
        (contains message "dispatcher")

(* 4. submit_review denied when pilot off at preview *)
let test_submit_review_denied_when_pilot_off () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_pilot_off" ~policy:(caps ~reply:false ~review:true)
  in
  let req =
    {
      Review.item_key;
      kind = Review.Approve;
      head_sha;
      body = Some "LGTM";
      actor_login = Some "alice";
    }
  in
  match
    W.preview ~db ~principal ~room_id ~action:(W.Submit_review req)
      ~base_revision ~route ~pilot:Review.default_pilot_gate
      ~user_auth_available:false ~now:fixed_now ()
  with
  | Ok _ -> Alcotest.fail "expected deny when pilot off"
  | Error msg -> (
      Alcotest.(check bool) "mentions pilot" true (contains msg "pilot");
      Alcotest.(check bool)
        "not production-ready" true
        (contains msg "production");
      match Setup_plan_apply.get_plan ~db ~plan_id:"anything" with
      | None -> ()
      | Some _ -> ())

(* 5. collab comment preview works (target + effects) *)
let test_collab_comment_preview_works () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_comment" ~policy:(caps ~reply:true ~review:false)
  in
  let body = "Ship it after CI is green." in
  let plan =
    assert_ok
      (W.preview ~db ~principal ~room_id
         ~action:(W.Collab (Collab.Comment { item_key; body }))
         ~base_revision ~route ~now:fixed_now ())
  in
  Alcotest.(check string) "label" "collab" (W.action_kind_label comment_action);
  (match plan.apply_payload.kind with
  | Setup_plan.Generic "github_collab_action" -> ()
  | Setup_plan.Generic other -> Alcotest.fail ("unexpected kind: " ^ other)
  | _ -> Alcotest.fail "expected Generic github_collab_action");
  Alcotest.(check (option string))
    "destination room" (Some room_id) plan.destination.room_id;
  let render = Yojson.Safe.to_string (Setup_plan.to_render_json plan) in
  Alcotest.(check bool) "render has item_key" true (contains render item_key);
  Alcotest.(check bool) "render has body" true (contains render "ship it");
  Alcotest.(check bool)
    "render has allow_reply" true
    (contains render "allow_reply");
  Alcotest.(check bool)
    "diff notes confirm required" true
    (List.exists
       (function
         | Setup_plan.Note { message; _ } -> contains message "confirm"
         | _ -> false)
       plan.diff);
  (* Denied without capability. *)
  let route_off =
    make_route ~id:"rt_comment_off" ~policy:(caps ~reply:false ~review:false)
  in
  match
    W.preview ~db ~principal ~room_id
      ~action:(W.Collab (Collab.Comment { item_key; body = "nope" }))
      ~base_revision ~route:route_off ~now:(fixed_now +. 1.) ()
  with
  | Ok _ -> Alcotest.fail "expected deny without allow_reply"
  | Error msg ->
      Alcotest.(check bool) "deny capability" true (contains msg "allow_reply")

(* Bonus: request_reviewers preview + apply via shared workflow *)
let test_request_reviewers_preview_apply () =
  with_db @@ fun db ->
  let route = make_route ~id:"rt_rr" ~policy:(caps ~reply:false ~review:true) in
  let req =
    {
      Review.item_key;
      reviewers = [ "bob"; "carol" ];
      head_sha = Some head_sha;
    }
  in
  let plan =
    assert_ok
      (W.preview ~db ~principal ~room_id ~action:(W.Request_reviewers req)
         ~base_revision ~route ~now:fixed_now ())
  in
  (match plan.apply_payload.kind with
  | Setup_plan.Generic "github_request_reviewers" -> ()
  | _ -> Alcotest.fail "expected github_request_reviewers");
  match
    assert_ok
      (W.apply_confirmed ~db ~plan_id:plan.id ~digest:plan.digest ~principal
         ~current_base_revision:base_revision ~now:fixed_now ())
  with
  | Setup_plan_apply.Applied _ ->
      Alcotest.fail "apply must not create a receipt without a live dispatcher"
  | Setup_plan_apply.Rejected { reason; message } ->
      Alcotest.(check string)
        "reason" "apply_error"
        (Setup_plan_apply.string_of_reject_reason reason);
      Alcotest.(check bool)
        "mentions dispatcher" true
        (contains message "dispatcher")

let suite =
  [
    ("preview creates pending plan", `Quick, test_preview_creates_pending_plan);
    ("apply with wrong digest rejects", `Quick, test_apply_wrong_digest_rejects);
    ( "apply fails closed without dispatcher",
      `Quick,
      test_apply_fails_closed_without_dispatcher );
    ( "submit_review denied when pilot off at preview",
      `Quick,
      test_submit_review_denied_when_pilot_off );
    ("collab comment preview works", `Quick, test_collab_comment_preview_works);
    ( "request_reviewers preview apply",
      `Quick,
      test_request_reviewers_preview_apply );
  ]

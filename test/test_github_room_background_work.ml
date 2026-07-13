(** Tests for Room-anchored background work items (P19.M4.E2.T002). *)

module S = Github_route_store
module A = Github_room_background_work

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Setup_plan_apply.init_schema db;
  Github_work_item.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let base_revision = "rev-config-1"
let room_id = "room-teams-1"
let room = S.Room room_id
let repo = "acme/widget"
let item_key = "issue:acme/widget:9"
let thread_ref = "thread:msg-abc-123"
let dedup_key = "room:room-teams-1:bg:dedup-1"
let prompt = "summarize open discussion and propose next steps"

let principal =
  Setup_plan.{ id = "principal:alice"; kind = Principal; label = Some "Alice" }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let caps ?(background = false) () : S.capability_policy =
  {
    allow_reply = false;
    allow_label = false;
    allow_assign = false;
    allow_review = false;
    allow_merge = false;
    allow_close = false;
    extra =
      (if background then [ (A.capability_key, true) ]
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
    pilot_name = "p19-room-background-work-pilot";
    expires_at = Some "2099-01-01T00:00:00Z";
  }

let pilot_off = A.default_pilot_gate

let pilot_expired =
  {
    A.enabled = true;
    pilot_name = "p19-room-background-work-pilot";
    expires_at = Some "2020-01-01T00:00:00Z";
  }

let base_req ?(dedup = dedup_key) ?(prompt = prompt) ?(runner = Some "codex")
    ?(item = Some item_key) ?(thread = Some thread_ref) () : A.request =
  {
    room_id;
    item_key = item;
    prompt;
    runner_pref = runner;
    thread_ref = thread;
    dedup_key = dedup;
  }

(* 1. pilot off deny *)
let test_pilot_off_deny () =
  let route = make_route ~id:"rt_bg_off" ~policy:(caps ~background:true ()) in
  (match A.authorize ~route:(Some route) ~pilot:pilot_off ~now:fixed_now () with
  | Ok () -> Alcotest.fail "expected deny when pilot off"
  | Error msg ->
      Alcotest.(check bool) "mentions pilot" true (contains msg "pilot");
      Alcotest.(check bool)
        "not production-ready" true
        (contains msg "production");
      Alcotest.(check bool)
        "no App/PAT fallback" true
        (contains msg "fallback" || contains msg "user"));
  (* plan defaults pilot off *)
  with_db @@ fun db ->
  match
    A.plan_background ~db ~principal ~req:(base_req ()) ~base_revision ~route
      ~now:fixed_now ()
  with
  | Ok _ -> Alcotest.fail "expected plan deny when pilot defaults off"
  | Error msg -> (
      Alcotest.(check bool) "plan mentions pilot" true (contains msg "pilot");
      match
        A.authorize ~route:(Some route) ~pilot:pilot_expired ~now:fixed_now ()
      with
      | Ok () -> Alcotest.fail "expected deny when pilot expired"
      | Error msg ->
          Alcotest.(check bool) "mentions expired" true (contains msg "expired")
      )

(* 2. plan when pilot on + capability *)
let test_plan_when_pilot_on_and_capability () =
  with_db @@ fun db ->
  let route = make_route ~id:"rt_bg_on" ~policy:(caps ~background:true ()) in
  (match A.authorize ~route:(Some route) ~pilot:pilot_on ~now:fixed_now () with
  | Ok () -> ()
  | Error e -> Alcotest.fail ("unexpected authorize deny: " ^ e));
  (* deny without capability even with pilot on *)
  let route_off =
    make_route ~id:"rt_bg_cap_off" ~policy:(caps ~background:false ())
  in
  (match
     A.authorize ~route:(Some route_off) ~pilot:pilot_on ~now:fixed_now ()
   with
  | Ok () -> Alcotest.fail "expected deny without background_work capability"
  | Error msg ->
      Alcotest.(check bool)
        "mentions background_work" true
        (contains msg "background_work"));
  let plan =
    assert_ok
      (A.plan_background ~db ~principal ~req:(base_req ()) ~base_revision ~route
         ~pilot:pilot_on ~now:fixed_now ())
  in
  Alcotest.(check bool) "is background plan" true (A.is_background_plan plan);
  (match plan.apply_payload.kind with
  | Setup_plan.Generic "github_room_background_work" -> ()
  | Setup_plan.Generic other -> Alcotest.fail ("unexpected kind: " ^ other)
  | _ -> Alcotest.fail "expected Generic github_room_background_work");
  Alcotest.(check bool) "readiness ok" true (Setup_plan.readiness_ok plan);
  let persist = Yojson.Safe.to_string (Setup_plan.to_persist_json plan) in
  Alcotest.(check bool) "room" true (contains persist room_id);
  Alcotest.(check bool) "dedup" true (contains persist dedup_key);
  Alcotest.(check bool) "prompt" true (contains persist "summarize");
  Alcotest.(check bool) "runner" true (contains persist "codex");
  Alcotest.(check bool) "item_key" true (contains persist item_key);
  Alcotest.(check bool) "thread" true (contains persist thread_ref);
  Alcotest.(check bool)
    "work item semantics" true
    (contains persist "github_work_item");
  Alcotest.(check bool)
    "code change separate" true
    (contains persist "code_change" || contains persist "t007");
  Alcotest.(check bool)
    "webhook correlation" true
    (contains persist "webhook_correlation");
  Alcotest.(check bool)
    "pilot name" true
    (contains persist "p19-room-background-work-pilot")

(* 3. duplicate dedup_key no second work item *)
let test_duplicate_dedup_key_no_second_item () =
  with_db @@ fun db ->
  let req = base_req () in
  let first = assert_ok (A.enqueue_work_item ~db ~req ~now:fixed_now ()) in
  Alcotest.(check string)
    "queued" "queued"
    (Github_work_item.string_of_status first.status);
  Alcotest.(check string) "dedup" dedup_key first.dedup_key;
  Alcotest.(check string) "repo" repo first.repo_full_name;
  Alcotest.(check int) "issue" 9 first.issue_number;
  Alcotest.(check bool) "not pr" false first.is_pr;
  Alcotest.(check (option string)) "runner" (Some "codex") first.runner_pref;
  Alcotest.(check bool)
    "thread in preamble" true
    (contains first.preamble thread_ref);
  Alcotest.(check bool)
    "trigger room_background" true
    (String.equal first.trigger "room_background");
  (* Second enqueue with same dedup must return the same item, not create. *)
  let second =
    assert_ok
      (A.enqueue_work_item ~db
         ~req:
           (base_req ~prompt:"different prompt should not create a new row" ())
         ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check int) "same id" first.id second.id;
  Alcotest.(check string) "original prompt kept" prompt second.prompt;
  Alcotest.(check int) "one row" 1 (List.length (Github_work_item.list ~db ()));
  (* Different dedup creates a second item. *)
  let other =
    assert_ok
      (A.enqueue_work_item ~db
         ~req:(base_req ~dedup:"room:room-teams-1:bg:dedup-2" ())
         ~now:(fixed_now +. 2.) ())
  in
  Alcotest.(check bool) "different id" true (first.id <> other.id);
  Alcotest.(check int) "two rows" 2 (List.length (Github_work_item.list ~db ()))

(* 4. secret-free plan *)
let test_secret_free_plan () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_bg_secret" ~policy:(caps ~background:true ())
  in
  let plan =
    assert_ok
      (A.plan_background ~db ~principal ~req:(base_req ()) ~base_revision ~route
         ~pilot:pilot_on ~now:fixed_now ())
  in
  let persist = Yojson.Safe.to_string (Setup_plan.to_persist_json plan) in
  Alcotest.(check bool)
    "no secret shapes" false
    (contains persist "ghp_"
    || contains persist "bot_token"
    || contains persist "api_key"
    || contains persist "private_key"
    || contains persist "bearer "
    || contains persist "github_pat_");
  Alcotest.(check bool)
    "not production_ready flag" true
    (contains persist "production_ready");
  let redacted =
    A.receipt_safe_error
      "dispatch failed token=ghp_SECRETvalue1234567890 Bearer \
       abcdef0123456789xyz"
  in
  Alcotest.(check bool)
    "redacts ghp secret value" false
    (contains redacted "ghp_secretvalue1234567890");
  Alcotest.(check bool)
    "redacts bearer" false
    (contains redacted "abcdef0123456789xyz")

(* 5. cancel / retry hooks on work_item *)
let test_cancel_retry_hooks () =
  with_db @@ fun db ->
  let item =
    assert_ok (A.enqueue_work_item ~db ~req:(base_req ()) ~now:fixed_now ())
  in
  let running = assert_ok (A.mark_progress ~db ~id:item.id ()) in
  Alcotest.(check string)
    "running" "running"
    (Github_work_item.string_of_status running.status);
  (* Cannot retry while running. *)
  (match A.request_retry ~db ~id:item.id () with
  | Ok _ -> Alcotest.fail "expected retry deny while running"
  | Error msg ->
      Alcotest.(check bool) "mentions running" true (contains msg "running"));
  let cancelled = assert_ok (A.cancel_work_item ~db ~id:item.id ()) in
  Alcotest.(check string)
    "cancelled" "cancelled"
    (Github_work_item.string_of_status cancelled.status);
  Alcotest.(check bool)
    "terminal" true
    (Github_work_item.is_terminal_status cancelled.status);
  (* Idempotent cancel when already cancelled. *)
  let cancelled2 = assert_ok (A.cancel_work_item ~db ~id:item.id ()) in
  Alcotest.(check int) "same cancelled id" cancelled.id cancelled2.id;
  (* Retry re-queues. *)
  let retried = assert_ok (A.request_retry ~db ~id:item.id ()) in
  Alcotest.(check string)
    "requeued" "queued"
    (Github_work_item.string_of_status retried.status);
  (* Blocked + completed paths. *)
  let blocked =
    assert_ok (A.mark_blocked ~db ~id:item.id ~summary:"host unavailable" ())
  in
  Alcotest.(check string)
    "blocked" "blocked"
    (Github_work_item.string_of_status blocked.status);
  let retried2 = assert_ok (A.request_retry ~db ~id:item.id ()) in
  Alcotest.(check string)
    "requeued after block" "queued"
    (Github_work_item.string_of_status retried2.status);
  let _ = assert_ok (A.mark_progress ~db ~id:item.id ()) in
  let done_ =
    assert_ok
      (A.mark_completed ~db ~id:item.id
         ~summary:"posted summary to anchored thread" ())
  in
  Alcotest.(check string)
    "succeeded" "succeeded"
    (Github_work_item.string_of_status done_.status);
  Alcotest.(check (option string))
    "summary" (Some "posted summary to anchored thread") done_.result_summary

let suite =
  [
    ("pilot off deny", `Quick, test_pilot_off_deny);
    ( "plan when pilot on + capability",
      `Quick,
      test_plan_when_pilot_on_and_capability );
    ( "duplicate dedup_key no second work item",
      `Quick,
      test_duplicate_dedup_key_no_second_item );
    ("secret-free plan", `Quick, test_secret_free_plan);
    ("cancel/retry hooks on work_item", `Quick, test_cancel_retry_hooks);
  ]

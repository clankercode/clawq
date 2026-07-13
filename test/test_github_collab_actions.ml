(** Tests for policy-gated GitHub collab actions (P19.M4.E1.T003). *)

module S = Github_route_store
module A = Github_collab_actions

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

let principal =
  Setup_plan.{ id = "principal:alice"; kind = Principal; label = Some "Alice" }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let caps ~reply ~label ~assign : S.capability_policy =
  {
    allow_reply = reply;
    allow_label = label;
    allow_assign = assign;
    allow_review = false;
    allow_merge = false;
    allow_close = false;
    extra = [];
  }

(** In-memory route value for authorization tests (no DB uniqueness needed). *)
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

let expect_allowed = function
  | A.Allowed { action = _; capability } -> capability
  | A.Denied { reason } -> Alcotest.fail ("unexpected Denied: " ^ reason)

let expect_denied = function
  | A.Denied { reason } -> reason
  | A.Allowed { capability; _ } ->
      Alcotest.fail ("unexpected Allowed: " ^ capability)

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

(* 1. comment allowed when allow_reply *)
let test_comment_allowed_when_allow_reply () =
  let route =
    make_route ~id:"rt_reply_on"
      ~policy:(caps ~reply:true ~label:false ~assign:false)
  in
  let action = A.Comment { item_key; body = "LGTM after CI" } in
  let cap = expect_allowed (A.authorize ~route:(Some route) ~action) in
  Alcotest.(check string) "capability" "allow_reply" cap

(* 2. comment denied when not allow_reply / no route *)
let test_comment_denied_when_not_allow_reply () =
  let route =
    make_route ~id:"rt_reply_off"
      ~policy:(caps ~reply:false ~label:true ~assign:true)
  in
  let action = A.Comment { item_key; body = "should not post" } in
  let reason = expect_denied (A.authorize ~route:(Some route) ~action) in
  Alcotest.(check bool)
    "mentions allow_reply" true
    (contains reason "allow_reply");
  let reason_missing = expect_denied (A.authorize ~route:None ~action) in
  Alcotest.(check bool)
    "missing route denied" true
    (contains reason_missing "no route")

(* 3. label allowed / denied *)
let test_label_allowed_and_denied () =
  let allowed_route =
    make_route ~id:"rt_label_on"
      ~policy:(caps ~reply:false ~label:true ~assign:false)
  in
  let denied_route =
    make_route ~id:"rt_label_off"
      ~policy:(caps ~reply:true ~label:false ~assign:false)
  in
  let action =
    A.Label { item_key; add = [ "needs-review" ]; remove = [ "wip" ] }
  in
  let cap = expect_allowed (A.authorize ~route:(Some allowed_route) ~action) in
  Alcotest.(check string) "capability" "allow_label" cap;
  let reason = expect_denied (A.authorize ~route:(Some denied_route) ~action) in
  Alcotest.(check bool)
    "mentions allow_label" true
    (contains reason "allow_label")

(* 4. plan_action produces plan without secrets; no live mutation *)
let test_plan_action_no_secrets_no_mutation () =
  with_db @@ fun db ->
  let route =
    make_route ~id:"rt_plan"
      ~policy:(caps ~reply:true ~label:false ~assign:false)
  in
  let action =
    A.Comment { item_key; body = "Looks good — please land after green CI." }
  in
  let plan =
    assert_ok
      (A.plan_action ~db ~principal ~room_id ~action ~base_revision ~route
         ~now:fixed_now ())
  in
  (match plan.apply_payload.kind with
  | Setup_plan.Generic "github_collab_action" -> ()
  | Setup_plan.Generic other ->
      Alcotest.fail ("unexpected generic kind: " ^ other)
  | _ -> Alcotest.fail "expected Generic github_collab_action");
  Alcotest.(check bool) "readiness ok" true (Setup_plan.readiness_ok plan);
  (match Setup_plan_apply.get_plan ~db ~plan_id:plan.id with
  | Some stored ->
      Alcotest.(check string) "stored id" plan.id stored.id;
      Alcotest.(check string) "digest" plan.digest stored.digest
  | None -> Alcotest.fail "plan not stored as pending");
  (* No GitHub mutation side effects via store: zero routes created. *)
  (match S.list_for_destination ~db ~destination:room with
  | Ok routes -> Alcotest.(check int) "no routes mutated" 0 (List.length routes)
  | Error e -> Alcotest.fail e);
  let persist = Yojson.Safe.to_string (Setup_plan.to_persist_json plan) in
  Alcotest.(check bool)
    "no token-like secret keys" false
    (contains persist "bot_token"
    || contains persist "signing_secret"
    || contains persist "api_key"
    || contains persist "private_key");
  Alcotest.(check bool) "includes item_key" true (contains persist item_key);
  Alcotest.(check bool)
    "includes allow_reply" true
    (contains persist "allow_reply");
  (* Denied path does not create a plan. *)
  match
    A.plan_action ~db ~principal ~room_id
      ~action:(A.Label { item_key; add = [ "x" ]; remove = [] })
      ~base_revision ~route ~now:(fixed_now +. 1.) ()
  with
  | Ok _ -> Alcotest.fail "label plan should be denied"
  | Error msg ->
      Alcotest.(check bool)
        "deny mentions capability" true
        (contains msg "allow_label")

(* 5. assign capability *)
let test_assign_capability () =
  with_db @@ fun db ->
  let allowed =
    make_route ~id:"rt_assign_on"
      ~policy:(caps ~reply:false ~label:false ~assign:true)
  in
  let denied =
    make_route ~id:"rt_assign_off"
      ~policy:(caps ~reply:true ~label:true ~assign:false)
  in
  let action = A.Assign { item_key; add = [ "alice" ]; remove = [ "bob" ] } in
  let cap = expect_allowed (A.authorize ~route:(Some allowed) ~action) in
  Alcotest.(check string) "capability" "allow_assign" cap;
  let reason = expect_denied (A.authorize ~route:(Some denied) ~action) in
  Alcotest.(check bool)
    "mentions allow_assign" true
    (contains reason "allow_assign");
  let plan =
    assert_ok
      (A.plan_action ~db ~principal ~room_id ~action ~base_revision
         ~route:allowed ~now:fixed_now ())
  in
  match plan.apply_payload.kind with
  | Setup_plan.Generic "github_collab_action" ->
      let ops_s = Yojson.Safe.to_string plan.apply_payload.ops in
      Alcotest.(check bool) "ops has assign" true (contains ops_s "assign");
      Alcotest.(check bool) "ops has alice" true (contains ops_s "alice")
  | _ -> Alcotest.fail "expected Generic github_collab_action"

let suite =
  [
    ( "comment allowed when allow_reply",
      `Quick,
      test_comment_allowed_when_allow_reply );
    ( "comment denied when not allow_reply",
      `Quick,
      test_comment_denied_when_not_allow_reply );
    ("label allowed and denied", `Quick, test_label_allowed_and_denied);
    ( "plan_action produces plan without secrets",
      `Quick,
      test_plan_action_no_secrets_no_mutation );
    ("assign capability", `Quick, test_assign_capability);
  ]

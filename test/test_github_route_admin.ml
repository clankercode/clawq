(** Tests for GitHub route plan/inspect/change/disable/remove admin API
    (P19.M2.E3.T002). *)

module S = Github_route_store
module A = Github_route_admin

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Setup_plan_apply.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let base_revision = "rev-config-1"
let room = S.Room "room-teams-1"
let repo_sel = S.Repo "Acme/Widget"

let item_pr =
  S.Item { repo_full_name = "Acme/Widget"; kind = `Pull_request; number = 42 }

let principal =
  Setup_plan.{ id = "principal:alice"; kind = Principal; label = Some "Alice" }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let contains hay needle = Test_helpers.string_contains hay needle

let count_routes db =
  match S.list_for_destination ~db ~destination:room with
  | Ok xs -> List.length xs
  | Error e -> Alcotest.fail e

let test_plan_create_pending_no_route () =
  with_db @@ fun db ->
  let plan =
    assert_ok
      (A.plan_create ~db ~principal ~destination:room ~selector:repo_sel
         ~base_revision ~now:fixed_now ~route_id:"rt_plan_1" ())
  in
  Alcotest.(check bool)
    "kind github_route" true
    (match plan.apply_payload.kind with
    | Setup_plan.Github_route -> true
    | _ -> false);
  Alcotest.(check int) "no routes yet" 0 (count_routes db);
  (match Setup_plan_apply.get_plan ~db ~plan_id:plan.id with
  | Some stored ->
      Alcotest.(check string) "stored id" plan.id stored.id;
      Alcotest.(check string) "digest" plan.digest stored.digest
  | None -> Alcotest.fail "plan not stored as pending");
  let plans = A.list_plans_for_destination ~db ~destination:room () in
  Alcotest.(check int) "list plans" 1 (List.length plans)

let test_apply_create_route () =
  with_db @@ fun db ->
  let plan =
    assert_ok
      (A.plan_create ~db ~principal ~destination:room ~selector:repo_sel
         ~base_revision ~now:fixed_now ~route_id:"rt_apply_1"
         ~comment_mode:S.Threaded
         ~filter:
           {
             include_events = [ "pull_request" ];
             exclude_events = [];
             include_repos = [];
             exclude_repos = [];
           }
         ~capability_policy:
           {
             allow_reply = true;
             allow_label = false;
             allow_assign = false;
             allow_review = false;
             allow_merge = false;
             allow_close = false;
             extra = [];
           }
         ())
  in
  let outcome =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:base_revision ~destination_room:"room-teams-1"
      ~now:fixed_now
      ~authority:(fun ~principal:_ ~destination:_ -> Ok ())
      ~apply_ops:(A.apply_route_ops ~db) ()
  in
  match outcome with
  | Setup_plan_apply.Rejected { message; _ } -> Alcotest.fail message
  | Setup_plan_apply.Applied { first_time; _ } -> (
      Alcotest.(check bool) "first apply" true first_time;
      match S.get ~db ~id:"rt_apply_1" with
      | Error e -> Alcotest.fail e
      | Ok None -> Alcotest.fail "route missing after apply"
      | Ok (Some r) ->
          Alcotest.(check bool) "enabled" true r.enabled;
          Alcotest.(check string)
            "selector" "repo:acme/widget"
            (S.canonical_selector_key r.selector);
          Alcotest.(check bool)
            "threaded" true
            (match r.comment_mode with Threaded -> true | _ -> false);
          Alcotest.(check bool) "reply" true r.capability_policy.allow_reply;
          Alcotest.(check (option string))
            "provenance plan" (Some plan.id) r.provenance.setup_plan_id)

let test_inspect_summary () =
  with_db @@ fun db ->
  ignore
    (assert_ok
       (S.create ~db ~id:"rt_insp" ~destination:room ~selector:item_pr
          ~now:fixed_now ()));
  let view = assert_ok (A.inspect ~db ~id:"rt_insp") in
  Alcotest.(check string) "id" "rt_insp" view.route.id;
  Alcotest.(check bool) "summary has id" true (contains view.summary "rt_insp");
  Alcotest.(check bool)
    "summary has selector" true
    (contains view.summary "item:acme/widget:pr:42");
  Alcotest.(check bool)
    "explain no-fallthrough" true
    (List.exists (fun s -> contains s "No fallthrough") view.explain);
  Alcotest.(check bool)
    "explain filter" true
    (List.exists (fun s -> contains s "Forwarding filter") view.explain);
  Alcotest.(check bool)
    "explain caps" true
    (List.exists (fun s -> contains s "Capabilities") view.explain)

let test_plan_disable_apply () =
  with_db @@ fun db ->
  ignore
    (assert_ok
       (S.create ~db ~id:"rt_dis" ~destination:room ~selector:repo_sel
          ~now:fixed_now ()));
  let plan =
    assert_ok
      (A.plan_disable ~db ~principal ~id:"rt_dis" ~base_revision ~now:fixed_now
         ())
  in
  (* Still enabled before apply. *)
  (match S.get ~db ~id:"rt_dis" with
  | Ok (Some r) -> Alcotest.(check bool) "still enabled" true r.enabled
  | _ -> Alcotest.fail "missing");
  let outcome =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:base_revision ~destination_room:"room-teams-1"
      ~now:fixed_now
      ~authority:(fun ~principal:_ ~destination:_ -> Ok ())
      ~apply_ops:(A.apply_route_ops ~db) ()
  in
  match outcome with
  | Setup_plan_apply.Rejected { message; _ } -> Alcotest.fail message
  | Setup_plan_apply.Applied _ -> (
      match S.get ~db ~id:"rt_dis" with
      | Ok (Some r) -> Alcotest.(check bool) "disabled" false r.enabled
      | _ -> Alcotest.fail "missing after disable")

let test_plan_remove_frees_slot () =
  with_db @@ fun db ->
  ignore
    (assert_ok
       (S.create ~db ~id:"rt_rm_old" ~destination:room ~selector:repo_sel
          ~now:fixed_now ()));
  let plan =
    assert_ok
      (A.plan_remove ~db ~principal ~id:"rt_rm_old" ~base_revision
         ~now:fixed_now ())
  in
  ignore
    (match
       Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest
         ~principal ~current_base_revision:base_revision
         ~destination_room:"room-teams-1" ~now:fixed_now
         ~authority:(fun ~principal:_ ~destination:_ -> Ok ())
         ~apply_ops:(A.apply_route_ops ~db) ()
     with
    | Setup_plan_apply.Applied _ -> ()
    | Setup_plan_apply.Rejected { message; _ } -> Alcotest.fail message);
  (* Slot free: create another active route for same dest+selector. *)
  let r2 =
    assert_ok
      (S.create ~db ~id:"rt_rm_new" ~destination:room ~selector:repo_sel
         ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check bool) "new enabled" true r2.enabled;
  match S.find_active ~db ~destination:room ~selector:repo_sel with
  | Ok (Some active) -> Alcotest.(check string) "winner" "rt_rm_new" active.id
  | _ -> Alcotest.fail "expected active new route"

let test_plan_secret_free () =
  with_db @@ fun db ->
  let plan =
    assert_ok
      (A.plan_create ~db ~principal ~destination:room ~selector:item_pr
         ~base_revision ~now:fixed_now ~managed_bundle_id:"bundle-1"
         ~managed_feature_id:"feat-route-1" ())
  in
  let render = Setup_plan.format_summary plan in
  let persist = Yojson.Safe.to_string (Setup_plan.to_persist_json plan) in
  let payload = Yojson.Safe.to_string plan.apply_payload.ops in
  let blob =
    String.lowercase_ascii (render ^ "\n" ^ persist ^ "\n" ^ payload)
  in
  Alcotest.(check bool)
    "no pem" false
    (contains blob "-----begin" || contains blob "private_key");
  Alcotest.(check bool) "no bearer" false (contains blob "bearer ");
  Alcotest.(check bool) "no xoxb" false (contains blob "xoxb-");
  Alcotest.(check bool) "digest non-empty" true (String.length plan.digest >= 32);
  (* Redact should leave digest unchanged for clean plans. *)
  let redacted = Setup_plan.redact plan in
  Alcotest.(check string) "digest stable" plan.digest redacted.digest

let test_collision_on_create_apply () =
  with_db @@ fun db ->
  ignore
    (assert_ok
       (S.create ~db ~id:"rt_hold" ~destination:room ~selector:repo_sel
          ~now:fixed_now ()));
  let plan =
    assert_ok
      (A.plan_create ~db ~principal ~destination:room ~selector:repo_sel
         ~base_revision ~now:fixed_now ~route_id:"rt_collide"
         ~on_collision:`Reject ())
  in
  let outcome =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:base_revision ~destination_room:"room-teams-1"
      ~now:fixed_now
      ~authority:(fun ~principal:_ ~destination:_ -> Ok ())
      ~apply_ops:(A.apply_route_ops ~db) ()
  in
  match outcome with
  | Setup_plan_apply.Applied _ ->
      Alcotest.fail "expected collision reject on apply"
  | Setup_plan_apply.Rejected { reason; message } -> (
      Alcotest.(check string)
        "apply_error"
        (Setup_plan_apply.string_of_reject_reason Setup_plan_apply.Apply_error)
        (Setup_plan_apply.string_of_reject_reason reason);
      Alcotest.(check bool)
        "mentions collision/active" true
        (contains (String.lowercase_ascii message) "active"
        || contains (String.lowercase_ascii message) "collision");
      (* Original holder intact. *)
      (match S.get ~db ~id:"rt_hold" with
      | Ok (Some r) -> Alcotest.(check bool) "holder enabled" true r.enabled
      | _ -> Alcotest.fail "holder missing");
      match S.get ~db ~id:"rt_collide" with
      | Ok None -> ()
      | Ok (Some _) -> Alcotest.fail "colliding route should not exist"
      | Error e -> Alcotest.fail e)

let test_plan_update_apply () =
  with_db @@ fun db ->
  ignore
    (assert_ok
       (S.create ~db ~id:"rt_upd" ~destination:room ~selector:repo_sel
          ~now:fixed_now ()));
  let plan =
    assert_ok
      (A.plan_update ~db ~principal ~id:"rt_upd" ~comment_mode:S.Off
         ~enabled:true ~base_revision ~now:fixed_now ())
  in
  ignore
    (match
       Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest
         ~principal ~current_base_revision:base_revision
         ~destination_room:"room-teams-1" ~now:fixed_now
         ~authority:(fun ~principal:_ ~destination:_ -> Ok ())
         ~apply_ops:(A.apply_route_ops ~db) ()
     with
    | Setup_plan_apply.Applied _ -> ()
    | Setup_plan_apply.Rejected { message; _ } -> Alcotest.fail message);
  match S.get ~db ~id:"rt_upd" with
  | Ok (Some r) ->
      Alcotest.(check bool)
        "comment off" true
        (match r.comment_mode with Off -> true | _ -> false);
      Alcotest.(check string) "rev bumped" "2" r.revision
  | _ -> Alcotest.fail "missing"

let test_apply_idempotent_retry () =
  with_db @@ fun db ->
  let plan =
    assert_ok
      (A.plan_create ~db ~principal ~destination:room ~selector:item_pr
         ~base_revision ~now:fixed_now ~route_id:"rt_idem" ())
  in
  let apply () =
    Setup_plan_apply.apply ~db ~plan_id:plan.id ~digest:plan.digest ~principal
      ~current_base_revision:base_revision ~destination_room:"room-teams-1"
      ~now:fixed_now
      ~authority:(fun ~principal:_ ~destination:_ -> Ok ())
      ~apply_ops:(A.apply_route_ops ~db) ()
  in
  (match apply () with
  | Setup_plan_apply.Applied { first_time = true; _ } -> ()
  | Setup_plan_apply.Applied { first_time = false; _ } ->
      Alcotest.fail "first should not be idempotent"
  | Setup_plan_apply.Rejected { message; _ } -> Alcotest.fail message);
  match apply () with
  | Setup_plan_apply.Applied { first_time = false; _ } ->
      (* Still a single route. *)
      Alcotest.(check int) "one route" 1 (count_routes db)
  | Setup_plan_apply.Applied { first_time = true; _ } ->
      Alcotest.fail "retry should be idempotent"
  | Setup_plan_apply.Rejected { message; _ } -> Alcotest.fail message

let suite =
  [
    ( "plan_create stores pending without route",
      `Quick,
      test_plan_create_pending_no_route );
    ("apply_route_ops creates route", `Quick, test_apply_create_route);
    ("inspect returns summary and explain", `Quick, test_inspect_summary);
    ("plan_disable + apply disables", `Quick, test_plan_disable_apply);
    ("plan_remove + apply frees slot", `Quick, test_plan_remove_frees_slot);
    ("plan secret-free", `Quick, test_plan_secret_free);
    ( "collision on create returns error in apply",
      `Quick,
      test_collision_on_create_apply );
    ("plan_update + apply", `Quick, test_plan_update_apply);
    ("apply idempotent retry", `Quick, test_apply_idempotent_retry);
  ]

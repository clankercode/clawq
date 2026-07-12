(** Tests for legacy PR subscription → Item route migration (P19.M2.E2.T005). *)

module S = Github_route_store
module M = Github_route_migrate
module Match = Github_route_match
module E = Github_event_envelope

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let legacy ?(id = "1") ?(room = "room-a") ?(repo = "Acme/Widget") ?(pr = 42)
    ?(enabled = true) ?(events = []) ?profile_id ?backlink_ref ?audit_ref
    ?(created_at = Some "2024-01-01T00:00:00Z") () : M.legacy_subscription =
  {
    id;
    room_id = room;
    repo_full_name = repo;
    pr_number = pr;
    enabled;
    events;
    profile_id;
    backlink_ref;
    audit_ref;
    created_at;
  }

let item_sel repo n =
  S.Item { repo_full_name = repo; kind = `Pull_request; number = n }

let active_count_for ~db ~destination ~selector =
  match S.find_active ~db ~destination ~selector with
  | Ok (Some _) -> 1
  | Ok None -> 0
  | Error e -> Alcotest.fail e

let count_active_same_selector ~db ~destination ~selector =
  match S.list_for_destination ~db ~destination with
  | Error e -> Alcotest.fail e
  | Ok rows ->
      let skey = S.canonical_selector_key selector in
      List.fold_left
        (fun acc (r : S.t) ->
          if r.enabled && S.canonical_selector_key r.selector = skey then
            acc + 1
          else acc)
        0 rows

(* --- 1. single sub → creates Item route with correct selector --- *)

let test_single_creates_item_route () =
  with_db @@ fun db ->
  let leg =
    legacy ~id:"sub-1" ~room:"room-teams-1" ~repo:"Acme/Widget" ~pr:42
      ~events:[ "pull_request" ] ~profile_id:"7" ~backlink_ref:"bl-1"
      ~audit_ref:"aud-9" ()
  in
  let report =
    assert_ok (M.migrate_subscriptions ~db ~legacy:[ leg ] ~now:fixed_now ())
  in
  Alcotest.(check int) "one resolution" 1 (List.length report.resolutions);
  Alcotest.(check int) "active routes" 1 report.active_routes;
  match report.resolutions with
  | [ (got_leg, Created route) ] ->
      Alcotest.(check string) "legacy id" "sub-1" got_leg.id;
      Alcotest.(check string)
        "dest" "room:room-teams-1"
        (S.destination_key route.destination);
      Alcotest.(check string)
        "selector" "item:acme/widget:pr:42"
        (S.canonical_selector_key route.selector);
      Alcotest.(check bool) "enabled" true route.enabled;
      Alcotest.(check (list string))
        "events" [ "pull_request" ] route.filter.include_events;
      Alcotest.(check (option string))
        "created_via" (Some "migrate") route.provenance.created_via;
      Alcotest.(check (option string))
        "profile" (Some "7") route.provenance.created_by;
      Alcotest.(check (option string))
        "audit" (Some "aud-9") route.provenance.setup_plan_id;
      Alcotest.(check bool)
        "notes has backlink" true
        (match route.provenance.notes with
        | Some n -> Test_helpers.string_contains n "backlink_ref=bl-1"
        | None -> false);
      Alcotest.(check string)
        "deterministic id"
        (M.route_id_for_legacy leg)
        route.id
  | [ (_, other) ] ->
      Alcotest.failf "expected Created, got %s"
        (match other with
        | Created _ -> "Created"
        | Updated _ -> "Updated"
        | Skipped _ -> "Skipped"
        | Collided _ -> "Collided")
  | _ -> Alcotest.fail "unexpected resolutions"

(* --- 2. re-run migrate → no second active route (idempotent) --- *)

let test_rerun_idempotent () =
  with_db @@ fun db ->
  let leg = legacy ~id:"idem-1" () in
  let r1 =
    assert_ok (M.migrate_subscriptions ~db ~legacy:[ leg ] ~now:fixed_now ())
  in
  Alcotest.(check int) "first active" 1 r1.active_routes;
  let r2 =
    assert_ok
      (M.migrate_subscriptions ~db ~legacy:[ leg ] ~now:(fixed_now +. 10.) ())
  in
  Alcotest.(check int) "still one active" 1 r2.active_routes;
  let dest = M.destination_of_legacy leg in
  let sel = M.selector_of_legacy leg in
  Alcotest.(check int)
    "at most one active per selector" 1
    (count_active_same_selector ~db ~destination:dest ~selector:sel);
  (* Second run should Skip (prefer existing) or Update same id — never create
     a second enabled route. *)
  match r2.resolutions with
  | [ (_, Skipped { winner_route_id = Some wid; _ }) ] ->
      Alcotest.(check string) "winner is first" (M.route_id_for_legacy leg) wid
  | [ (_, Updated r) ] | [ (_, Created r) ] ->
      Alcotest.(check string) "same id" (M.route_id_for_legacy leg) r.id
  | [ (_, Collided { winner; _ }) ] ->
      Alcotest.(check string) "same id" (M.route_id_for_legacy leg) winner.id
  | _ -> Alcotest.fail "unexpected second-run resolution"

(* --- 3. two legacy same room+pr → one winner, collision recorded --- *)

let test_two_legacy_collision () =
  with_db @@ fun db ->
  let older =
    legacy ~id:"old" ~created_at:(Some "2024-01-01T00:00:00Z")
      ~events:[ "pull_request" ] ()
  in
  let newer =
    legacy ~id:"new" ~created_at:(Some "2024-06-01T00:00:00Z")
      ~events:[ "pull_request"; "issue_comment" ]
      ()
  in
  let report =
    assert_ok
      (M.migrate_subscriptions ~db ~legacy:[ older; newer ] ~now:fixed_now ())
  in
  Alcotest.(check int) "two resolutions" 2 (List.length report.resolutions);
  Alcotest.(check int) "one active" 1 report.active_routes;
  let dest = M.destination_of_legacy newer in
  let sel = M.selector_of_legacy newer in
  Alcotest.(check int)
    "one active selector" 1
    (count_active_same_selector ~db ~destination:dest ~selector:sel);
  let collided =
    List.filter_map
      (function
        | _, M.Collided { winner; losers } -> Some (winner, losers) | _ -> None)
      report.resolutions
  in
  Alcotest.(check bool) "collision recorded" true (collided <> []);
  match S.find_active ~db ~destination:dest ~selector:sel with
  | Ok (Some a) ->
      Alcotest.(check string)
        "winner is newest legacy"
        (M.route_id_for_legacy newer)
        a.id;
      Alcotest.(check (list string))
        "events from winner"
        [ "pull_request"; "issue_comment" ]
        a.filter.include_events
  | Ok None -> Alcotest.fail "no active"
  | Error e -> Alcotest.fail e

(* --- 4. existing route + legacy → Prefer_existing_route keeps existing --- *)

let test_prefer_existing () =
  with_db @@ fun db ->
  let dest = S.Room "room-a" in
  let sel = item_sel "Acme/Widget" 42 in
  let existing =
    assert_ok
      (S.create ~db ~id:"rt_preexisting" ~destination:dest ~selector:sel
         ~filter:
           {
             S.include_events = [ "issues" ];
             exclude_events = [];
             include_repos = [];
             exclude_repos = [];
           }
         ~now:fixed_now ())
  in
  let leg = legacy ~id:"leg-1" ~events:[ "pull_request" ] () in
  let report =
    assert_ok
      (M.migrate_subscriptions ~db ~legacy:[ leg ]
         ~policy:M.Prefer_existing_route ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check int) "still one active" 1 report.active_routes;
  (match report.resolutions with
  | [ (_, Skipped { winner_route_id = Some wid; reason }) ] ->
      Alcotest.(check string) "kept existing" existing.id wid;
      Alcotest.(check bool)
        "mentions prefer" true
        (Test_helpers.string_contains (String.lowercase_ascii reason) "prefer")
  | _ -> Alcotest.fail "expected Skipped prefer_existing");
  match S.find_active ~db ~destination:dest ~selector:sel with
  | Ok (Some a) ->
      Alcotest.(check string) "active is preexisting" "rt_preexisting" a.id;
      Alcotest.(check (list string))
        "filter untouched" [ "issues" ] a.filter.include_events
  | Ok None -> Alcotest.fail "missing active"
  | Error e -> Alcotest.fail e

(* --- Prefer_legacy supersedes existing --- *)

let test_prefer_legacy_replaces () =
  with_db @@ fun db ->
  let dest = S.Room "room-a" in
  let sel = item_sel "Acme/Widget" 42 in
  let old =
    assert_ok
      (S.create ~db ~id:"rt_old" ~destination:dest ~selector:sel ~now:fixed_now
         ())
  in
  let leg =
    legacy ~id:"leg-replace" ~events:[ "pull_request_review" ]
      ~created_at:(Some "2025-01-01T00:00:00Z") ()
  in
  let report =
    assert_ok
      (M.migrate_subscriptions ~db ~legacy:[ leg ] ~policy:M.Prefer_legacy
         ~now:(fixed_now +. 5.) ())
  in
  Alcotest.(check int) "one active" 1 report.active_routes;
  (match S.get ~db ~id:old.id with
  | Ok (Some o) -> Alcotest.(check bool) "old disabled" false o.enabled
  | Ok None -> Alcotest.fail "old missing"
  | Error e -> Alcotest.fail e);
  match S.find_active ~db ~destination:dest ~selector:sel with
  | Ok (Some a) ->
      Alcotest.(check string) "new winner" (M.route_id_for_legacy leg) a.id;
      Alcotest.(check (list string))
        "events" [ "pull_request_review" ] a.filter.include_events
  | Ok None -> Alcotest.fail "no active after replace"
  | Error e -> Alcotest.fail e

(* --- 5. disabled legacy → enabled=false route --- *)

let test_disabled_legacy () =
  with_db @@ fun db ->
  let leg = legacy ~id:"dis-1" ~enabled:false () in
  let report =
    assert_ok (M.migrate_subscriptions ~db ~legacy:[ leg ] ~now:fixed_now ())
  in
  Alcotest.(check int) "no active routes" 0 report.active_routes;
  match report.resolutions with
  | [ (_, Created r) ] | [ (_, Updated r) ] ->
      Alcotest.(check bool) "route disabled" false r.enabled
  | [ (_, Collided { winner; _ }) ] ->
      Alcotest.(check bool) "route disabled" false winner.enabled
  | _ -> Alcotest.fail "expected Created/Updated disabled route"

(* --- 6. events mapped into filter --- *)

let test_events_mapped () =
  with_db @@ fun db ->
  let events = [ "pull_request"; "issue_comment"; "check_run" ] in
  let leg = legacy ~id:"ev-1" ~events () in
  let report =
    assert_ok (M.migrate_subscriptions ~db ~legacy:[ leg ] ~now:fixed_now ())
  in
  match report.resolutions with
  | [ (_, Created r) ]
  | [ (_, Updated r) ]
  | [ (_, Collided { winner = r; _ }) ] ->
      Alcotest.(check (list string))
        "include_events" events r.filter.include_events
  | _ -> Alcotest.fail "expected created route with events"

let test_events_of_prefs () =
  let d = Github_pr_subscriptions.default_notification_preferences in
  Alcotest.(check (list string))
    "defaults → empty" []
    (M.events_of_notification_preferences d);
  let partial =
    { d with on_comment = false; on_status = false; on_review = false }
  in
  let ev = M.events_of_notification_preferences partial in
  Alcotest.(check bool) "has pull_request" true (List.mem "pull_request" ev);
  Alcotest.(check bool) "no issue_comment" false (List.mem "issue_comment" ev)

(* --- 7. list_for_destination has at most one active per selector --- *)

let test_at_most_one_active_per_selector () =
  with_db @@ fun db ->
  let legs =
    [
      legacy ~id:"a" ~created_at:(Some "2024-01-01T00:00:00Z") ();
      legacy ~id:"b" ~created_at:(Some "2024-02-01T00:00:00Z") ();
      legacy ~id:"c" ~created_at:(Some "2024-03-01T00:00:00Z") ();
    ]
  in
  ignore
    (assert_ok (M.migrate_subscriptions ~db ~legacy:legs ~now:fixed_now ()));
  (* Also seed an unrelated selector so list has more rows. *)
  ignore
    (assert_ok
       (S.create ~db ~id:"other" ~destination:(S.Room "room-a")
          ~selector:(item_sel "Acme/Widget" 99)
          ~now:fixed_now ()));
  let dest = S.Room "room-a" in
  let rows = assert_ok (S.list_for_destination ~db ~destination:dest) in
  let active_by_sel = Hashtbl.create 4 in
  List.iter
    (fun (r : S.t) ->
      if r.enabled then
        let k = S.canonical_selector_key r.selector in
        let n =
          match Hashtbl.find_opt active_by_sel k with Some x -> x | None -> 0
        in
        Hashtbl.replace active_by_sel k (n + 1))
    rows;
  Hashtbl.iter
    (fun k n -> Alcotest.(check int) ("active count for " ^ k) 1 n)
    active_by_sel

(* --- 8. ensure_schema / migrate transactional enough for retry --- *)

let test_schema_and_retry () =
  with_db @@ fun db ->
  S.ensure_schema db;
  S.ensure_schema db;
  let leg = legacy ~id:"retry-1" () in
  let r1 =
    assert_ok (M.migrate_subscriptions ~db ~legacy:[ leg ] ~now:fixed_now ())
  in
  (* Simulate concurrent/duplicate migration attempt after partial success. *)
  let r2 =
    assert_ok
      (M.migrate_subscriptions ~db ~legacy:[ leg; leg ] ~now:(fixed_now +. 1.)
         ())
  in
  Alcotest.(check int) "first active" 1 r1.active_routes;
  Alcotest.(check int) "retry active" 1 r2.active_routes;
  let dest = M.destination_of_legacy leg in
  let sel = M.selector_of_legacy leg in
  Alcotest.(check int)
    "still one" 1
    (count_active_same_selector ~db ~destination:dest ~selector:sel)

(* --- load_legacy_from_db --- *)

let test_load_legacy_empty_without_table () =
  with_db @@ fun db ->
  match M.load_legacy_from_db ~db with
  | Ok [] -> ()
  | Ok xs -> Alcotest.failf "expected empty, got %d" (List.length xs)
  | Error e -> Alcotest.fail e

let test_load_legacy_from_table () =
  with_db @@ fun db ->
  Github_pr_subscriptions.init_schema db;
  ignore
    (Github_pr_subscriptions.add ~db ~room_id:"room-x" ~repo:"org/repo"
       ~pr_number:7 ~profile_id:3
       ~notification_preferences:
         {
           Github_pr_subscriptions.default_notification_preferences with
           on_comment = false;
           on_status = false;
           on_review = false;
           on_merge = false;
           on_close = false;
         }
       ());
  let legs = assert_ok (M.load_legacy_from_db ~db) in
  Alcotest.(check int) "one sub" 1 (List.length legs);
  let leg = List.hd legs in
  Alcotest.(check string) "room" "room-x" leg.room_id;
  Alcotest.(check string) "repo" "org/repo" leg.repo_full_name;
  Alcotest.(check int) "pr" 7 leg.pr_number;
  Alcotest.(check (option string)) "profile" (Some "3") leg.profile_id;
  Alcotest.(check bool)
    "has pull_request event" true
    (List.mem "pull_request" leg.events);
  let report =
    assert_ok (M.migrate_subscriptions ~db ~legacy:legs ~now:fixed_now ())
  in
  Alcotest.(check int) "migrated active" 1 report.active_routes

(* --- decision / delivery parity: one accepted event per destination --- *)

let test_one_accepted_event_per_destination () =
  with_db @@ fun db ->
  (* Overlapping legacy for same room+PR should leave one active route; match
     accepts at most once per destination. *)
  let legs =
    [
      legacy ~id:"d1" ~created_at:(Some "2024-01-01T00:00:00Z") ();
      legacy ~id:"d2" ~created_at:(Some "2024-02-01T00:00:00Z") ();
    ]
  in
  ignore
    (assert_ok (M.migrate_subscriptions ~db ~legacy:legs ~now:fixed_now ()));
  let env : E.t =
    {
      version = E.envelope_version;
      delivery_id = Some "deliv-parity";
      installation_id = Some 1;
      event = "pull_request";
      action = Some "opened";
      repo_full_name = "acme/widget";
      org = Some "acme";
      item_kind = Some E.Pull_request;
      item_number = Some 42;
      item_node_id = None;
      item_url = None;
      html_url = None;
      family = E.Lifecycle;
      actor = E.empty_actor;
      before = None;
      after = None;
      transfer = None;
      received_at = Some "2024-01-01T00:00:00Z";
      event_at = None;
      head_sha = None;
      unsupported = false;
      skip_reason = None;
    }
  in
  let dest = S.Room "room-a" in
  Match.ensure_schema db;
  let decision = Match.resolve ~db ~destination:dest ~envelope:env () in
  (match decision with
  | Match.Matched { route; _ } ->
      Alcotest.(check bool) "enabled winner" true route.enabled
  | Match.Muted _ -> Alcotest.fail "expected Matched"
  | Match.No_route -> Alcotest.fail "expected Matched");
  (* try_accept proves one accepted delivery per destination. *)
  (match
     Match.try_accept ~db ~destination:dest ~envelope:env ~now:fixed_now ()
   with
  | Match.Accepted (Match.Matched _) -> ()
  | Match.Accepted _ -> Alcotest.fail "accepted non-matched"
  | Match.Duplicate _ -> Alcotest.fail "unexpected duplicate on first accept"
  | Match.Not_accepted _ -> Alcotest.fail "expected Accepted");
  match
    Match.try_accept ~db ~destination:dest ~envelope:env ~now:fixed_now ()
  with
  | Match.Duplicate _ -> ()
  | Match.Accepted _ -> Alcotest.fail "second accept should be Duplicate"
  | Match.Not_accepted _ -> Alcotest.fail "expected Duplicate"

(* --- Prefer_newest keeps newer existing --- *)

let test_prefer_newest_keeps_newer_existing () =
  with_db @@ fun db ->
  let dest = S.Room "room-a" in
  let sel = item_sel "Acme/Widget" 42 in
  let existing =
    assert_ok
      (S.create ~db ~id:"rt_newer" ~destination:dest ~selector:sel
         ~now:1_800_000_000.0 ())
  in
  let leg = legacy ~id:"leg-old" ~created_at:(Some "2020-01-01T00:00:00Z") () in
  let report =
    assert_ok
      (M.migrate_subscriptions ~db ~legacy:[ leg ] ~policy:M.Prefer_newest
         ~now:fixed_now ())
  in
  match report.resolutions with
  | [ (_, Skipped { winner_route_id = Some wid; _ }) ] ->
      Alcotest.(check string) "kept newer existing" existing.id wid
  | _ -> Alcotest.fail "expected skip for newer existing"

(* --- aliases documentation --- *)

let test_cli_aliases () =
  let aliases = M.compatibility_cli_aliases () in
  Alcotest.(check bool) "non-empty" true (aliases <> []);
  Alcotest.(check bool)
    "has subscriptions add" true
    (List.exists (fun (a, _) -> a = "subscriptions add") aliases);
  Alcotest.(check bool)
    "has pr-subscribe" true
    (List.exists (fun (a, _) -> a = "pr-subscribe") aliases);
  List.iter
    (fun (_legacy, modern) ->
      Alcotest.(check bool)
        ("modern " ^ modern) true
        (Test_helpers.string_contains modern "github route"))
    aliases

(* --- rollback-ish: failed mid-group doesn't leave dual actives --- *)

let test_no_dual_active_after_mixed () =
  with_db @@ fun db ->
  (* Existing active + Prefer_existing for one group; create for another. *)
  ignore
    (assert_ok
       (S.create ~db ~id:"keep-me" ~destination:(S.Room "room-a")
          ~selector:(item_sel "Acme/Widget" 1) ~now:fixed_now ()));
  let legs =
    [
      legacy ~id:"g1" ~pr:1 ~events:[ "pull_request" ] ();
      legacy ~id:"g2" ~pr:2 ~events:[ "pull_request" ] ();
    ]
  in
  let report =
    assert_ok
      (M.migrate_subscriptions ~db ~legacy:legs ~policy:M.Prefer_existing_route
         ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check int)
    "two active (pr1 kept + pr2 created)" 2 report.active_routes;
  Alcotest.(check int)
    "pr1 one" 1
    (active_count_for ~db ~destination:(S.Room "room-a")
       ~selector:(item_sel "Acme/Widget" 1));
  Alcotest.(check int)
    "pr2 one" 1
    (active_count_for ~db ~destination:(S.Room "room-a")
       ~selector:(item_sel "Acme/Widget" 2))

let suite =
  [
    ("single sub creates Item route", `Quick, test_single_creates_item_route);
    ("re-run migrate is idempotent", `Quick, test_rerun_idempotent);
    ("two legacy same room+pr collision", `Quick, test_two_legacy_collision);
    ("prefer existing keeps route", `Quick, test_prefer_existing);
    ("prefer legacy replaces existing", `Quick, test_prefer_legacy_replaces);
    ("disabled legacy → disabled route", `Quick, test_disabled_legacy);
    ("events mapped into filter", `Quick, test_events_mapped);
    ("events_of_notification_preferences", `Quick, test_events_of_prefs);
    ( "at most one active per selector",
      `Quick,
      test_at_most_one_active_per_selector );
    ("schema + retry safe", `Quick, test_schema_and_retry);
    ( "load_legacy empty without table",
      `Quick,
      test_load_legacy_empty_without_table );
    ("load_legacy from table + migrate", `Quick, test_load_legacy_from_table);
    ( "one accepted event per destination",
      `Quick,
      test_one_accepted_event_per_destination );
    ( "prefer newest keeps newer existing",
      `Quick,
      test_prefer_newest_keeps_newer_existing );
    ("compatibility CLI aliases", `Quick, test_cli_aliases);
    ( "no dual active after mixed migrate",
      `Quick,
      test_no_dual_active_after_mixed );
  ]

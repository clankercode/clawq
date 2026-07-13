(** Tests for Issue transfer dual-scope match + per-Room accept dedupe
    (P19.M2.E2.T004). *)

module S = Github_route_store
module E = Github_event_envelope
module F = Github_route_filter
module En = Github_filter_enrichment
module M = Github_route_match
module T = Github_route_transfer

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  S.ensure_schema db;
  M.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let room_a = S.Room "room-a"
let room_b = S.Room "room-b"
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let create ~db ?(id = "route-1") ?(enabled = true) ?(filter = S.default_filter)
    ~selector ~destination () =
  assert_ok
    (S.create ~db ~id ~destination ~selector ~filter ~enabled ~now:fixed_now ())

let make_transfer_envelope ?(delivery_id = Some "deliv-xfer-1")
    ?(from_repo = "acme/widgets") ?(to_repo = "acme/platform")
    ?(number = Some 10) ?(org = Some "acme") () : E.t =
  {
    version = E.envelope_version;
    delivery_id;
    installation_id = Some 99;
    event = "issues";
    action = Some "transferred";
    repo_full_name = from_repo;
    org;
    item_kind = Some E.Issue;
    item_number = number;
    item_node_id = Some "I_kwDO_transfer";
    item_url = None;
    html_url = None;
    family = E.Lifecycle;
    actor = E.empty_actor;
    item_author = None;
    before = None;
    after = None;
    transfer = Some { from_repo = Some from_repo; to_repo = Some to_repo };
    received_at = Some "2024-01-01T00:00:00Z";
    event_at = None;
    head_sha = None;
    unsupported = false;
    skip_reason = None;
  }

let normalized_transfer_envelope ~delivery_id ~labels =
  let label name = `Assoc [ ("name", `String name) ] in
  let payload =
    `Assoc
      [
        ("action", `String "transferred");
        ( "repository",
          `Assoc
            [
              ("full_name", `String "acme/widgets");
              ("owner", `Assoc [ ("login", `String "acme") ]);
            ] );
        ("sender", `Assoc [ ("login", `String "transfer-actor") ]);
        ( "issue",
          `Assoc
            [
              ("number", `Int 10);
              ("node_id", `String "I_kwDO_transfer");
              ("state", `String "open");
              ("labels", `List (List.map label labels));
              ("user", `Assoc [ ("login", `String "item-author") ]);
            ] );
        ( "changes",
          `Assoc
            [
              ( "old_repository",
                `Assoc [ ("full_name", `String "acme/widgets") ] );
              ( "new_repository",
                `Assoc [ ("full_name", `String "acme/platform") ] );
            ] );
      ]
  in
  match E.normalize ~delivery_id ~event:"issues" ~payload () with
  | E.Ok_envelope envelope -> envelope
  | E.Unsupported { reason; _ } | E.Error reason -> Alcotest.fail reason

let dest_key = function
  | S.Room id -> "room:" ^ id
  | S.Session k -> "session:" ^ k

let count_accepted results =
  List.fold_left
    (fun acc (_dest, r) -> match r with M.Accepted _ -> acc + 1 | _ -> acc)
    0 results

let count_duplicate results =
  List.fold_left
    (fun acc (_dest, r) -> match r with M.Duplicate _ -> acc + 1 | _ -> acc)
    0 results

let expect_accepted_rooms expected results =
  let accepted =
    List.filter_map
      (fun (dest, r) ->
        match r with M.Accepted _ -> Some (dest_key dest) | _ -> None)
      results
    |> List.sort String.compare
  in
  let expected = List.sort String.compare expected in
  Alcotest.(check (list string)) "accepted rooms" expected accepted

(* 1. Same room: Org on source + Repo on dest → one accept for that room *)
let test_same_room_dual_match_one_accept () =
  with_db @@ fun db ->
  (* Room A: Org "acme" matches both widgets and platform; also Repo on dest. *)
  ignore
    (create ~db ~id:"rt_org_a" ~destination:room_a ~selector:(S.Org "acme") ());
  ignore
    (create ~db ~id:"rt_repo_dest" ~destination:room_a
       ~selector:(S.Repo "acme/platform") ());
  let env = make_transfer_envelope () in
  let plan = T.plan_transfer ~db ~destinations:[ room_a ] ~envelope:env () in
  Alcotest.(check int) "one matched dest" 1 (List.length plan.destinations);
  Alcotest.(check string)
    "room a" (dest_key room_a)
    (dest_key (List.hd plan.destinations));
  (match plan.per_destination with
  | [ (_, M.Matched _) ] -> ()
  | _ -> Alcotest.fail "expected single Matched decision for room A");
  let results =
    T.accept_transfer ~db ~destinations:[ room_a ] ~envelope:env ~now:fixed_now
      ()
  in
  Alcotest.(check int) "one accept" 1 (count_accepted results);
  expect_accepted_rooms [ dest_key room_a ] results

(* 2. Source-only match → one accept *)
let test_source_only_match () =
  with_db @@ fun db ->
  ignore
    (create ~db ~id:"rt_src" ~destination:room_a
       ~selector:(S.Repo "acme/widgets") ());
  let env = make_transfer_envelope () in
  let plan =
    T.plan_transfer ~db ~destinations:[ room_a; room_b ] ~envelope:env ()
  in
  Alcotest.(check int) "one dest" 1 (List.length plan.destinations);
  let results =
    T.accept_transfer ~db ~destinations:[ room_a; room_b ] ~envelope:env
      ~now:fixed_now ()
  in
  Alcotest.(check int) "one accept" 1 (count_accepted results);
  expect_accepted_rooms [ dest_key room_a ] results

(* 3. Dest-only match → one accept *)
let test_dest_only_match () =
  with_db @@ fun db ->
  ignore
    (create ~db ~id:"rt_dst" ~destination:room_a
       ~selector:(S.Repo "acme/platform") ());
  let env = make_transfer_envelope () in
  let plan =
    T.plan_transfer ~db ~destinations:[ room_a; room_b ] ~envelope:env ()
  in
  Alcotest.(check int) "one dest" 1 (List.length plan.destinations);
  let results =
    T.accept_transfer ~db ~destinations:[ room_a; room_b ] ~envelope:env
      ~now:fixed_now ()
  in
  Alcotest.(check int) "one accept" 1 (count_accepted results);
  expect_accepted_rooms [ dest_key room_a ] results

(* 4. Different rooms: A source, B dest → two accepts *)
let test_different_rooms_fanout () =
  with_db @@ fun db ->
  ignore
    (create ~db ~id:"rt_a_src" ~destination:room_a
       ~selector:(S.Repo "acme/widgets") ());
  ignore
    (create ~db ~id:"rt_b_dst" ~destination:room_b
       ~selector:(S.Repo "acme/platform") ());
  let env = make_transfer_envelope () in
  let plan =
    T.plan_transfer ~db ~destinations:[ room_a; room_b ] ~envelope:env ()
  in
  Alcotest.(check int) "two dests" 2 (List.length plan.destinations);
  let results =
    T.accept_transfer ~db ~destinations:[ room_a; room_b ] ~envelope:env
      ~now:fixed_now ()
  in
  Alcotest.(check int) "two accepts" 2 (count_accepted results);
  expect_accepted_rooms [ dest_key room_a; dest_key room_b ] results

(* 5. Neither matches → empty *)
let test_neither_matches_empty () =
  with_db @@ fun db ->
  ignore
    (create ~db ~id:"rt_other" ~destination:room_a
       ~selector:(S.Repo "other/repo") ());
  let env = make_transfer_envelope () in
  let plan =
    T.plan_transfer ~db ~destinations:[ room_a; room_b ] ~envelope:env ()
  in
  Alcotest.(check int) "no dests" 0 (List.length plan.destinations);
  Alcotest.(check int) "no decisions" 0 (List.length plan.per_destination);
  let results =
    T.accept_transfer ~db ~destinations:[ room_a; room_b ] ~envelope:env
      ~now:fixed_now ()
  in
  Alcotest.(check int) "empty results" 0 (List.length results)

(* 6. Second accept_transfer same delivery → duplicates for rooms already
   accepted *)
let test_second_accept_duplicates () =
  with_db @@ fun db ->
  ignore (create ~db ~id:"rt_a" ~destination:room_a ~selector:(S.Org "acme") ());
  ignore
    (create ~db ~id:"rt_b" ~destination:room_b
       ~selector:(S.Repo "acme/platform") ());
  let env = make_transfer_envelope ~delivery_id:(Some "deliv-dup") () in
  let first =
    T.accept_transfer ~db ~destinations:[ room_a; room_b ] ~envelope:env
      ~now:fixed_now ()
  in
  Alcotest.(check int) "first: two accepted" 2 (count_accepted first);
  let second =
    T.accept_transfer ~db ~destinations:[ room_a; room_b ] ~envelope:env
      ~now:fixed_now ()
  in
  Alcotest.(check int) "second: zero accepted" 0 (count_accepted second);
  Alcotest.(check int) "second: two duplicates" 2 (count_duplicate second)

(* Extra: dual Org match same room still one accept (source+dest same org) *)
let test_same_room_org_both_views_one_accept () =
  with_db @@ fun db ->
  ignore
    (create ~db ~id:"rt_org" ~destination:room_a ~selector:(S.Org "acme") ());
  let env = make_transfer_envelope () in
  (* Source and dest views both match Org acme. *)
  let src = T.source_view env in
  let dst = T.dest_view env in
  (match M.resolve ~db ~destination:room_a ~envelope:src () with
  | M.Matched _ -> ()
  | _ -> Alcotest.fail "source view should match Org");
  (match M.resolve ~db ~destination:room_a ~envelope:dst () with
  | M.Matched _ -> ()
  | _ -> Alcotest.fail "dest view should match Org");
  let results =
    T.accept_transfer ~db ~destinations:[ room_a; room_a ] ~envelope:env
      ~now:fixed_now ()
  in
  Alcotest.(check int)
    "one accept despite dual match + dup candidate" 1 (count_accepted results)

(* transfer_stable_item_key uses to_repo *)
let test_stable_item_key_uses_to_repo () =
  let env =
    make_transfer_envelope ~from_repo:"Acme/Widgets" ~to_repo:"Acme/Platform"
      ~number:(Some 42) ()
  in
  let key = T.transfer_stable_item_key env in
  Alcotest.(check string) "key" "issue:acme/platform:42" key

let test_advanced_filter_controls_transfer_delivery_and_accept () =
  with_db @@ fun db ->
  let filter =
    assert_ok
      (F.validate
         {
           F.default with
           issue =
             {
               F.empty_issue with
               labels = Some { op = `In; values = [ "urgent" ] };
             };
         })
  in
  ignore
    (create ~db ~id:"rt-advanced-transfer" ~destination:room_a
       ~selector:(S.Repo "acme/platform") ~filter ());
  let rejected =
    normalized_transfer_envelope ~delivery_id:"transfer-advanced-reject"
      ~labels:[ "bug" ]
  in
  Alcotest.(check int)
    "advanced reject plans no destination" 0
    (List.length
       (T.plan_transfer ~db ~destinations:[ room_a ] ~envelope:rejected ())
         .destinations);
  Alcotest.(check int)
    "advanced reject does not accept" 0
    (count_accepted
       (T.accept_transfer ~db ~destinations:[ room_a ] ~envelope:rejected
          ~now:fixed_now ()));
  let accepted =
    normalized_transfer_envelope ~delivery_id:"transfer-advanced-accept"
      ~labels:[ "urgent" ]
  in
  Alcotest.(check int)
    "advanced accept reaches ledger" 1
    (count_accepted
       (T.accept_transfer ~db ~destinations:[ room_a ] ~envelope:accepted
          ~now:fixed_now ()))

let test_transfer_forwards_safe_enrichment_hooks () =
  with_db @@ fun db ->
  let filter =
    assert_ok
      (F.validate
         {
           F.default with
           issue =
             {
               F.empty_issue with
               team = Some { op = `In; values = [ "acme/triage" ] };
             };
         })
  in
  ignore
    (create ~db ~id:"rt-transfer-team" ~destination:room_a
       ~selector:(S.Repo "acme/platform") ~filter ());
  let calls = ref 0 in
  let fetch_teams ~(envelope : E.t) ~team_slugs =
    incr calls;
    Alcotest.(check (option string))
      "team lookup uses item author" (Some "item-author") envelope.item_author;
    Alcotest.(check (list string))
      "team lookup requests configured scope" [ "acme/triage" ] team_slugs;
    Ok [ "acme/triage" ]
  in
  let cache = En.create_cache () in
  let envelope =
    normalized_transfer_envelope ~delivery_id:"transfer-team" ~labels:[ "bug" ]
  in
  Alcotest.(check int)
    "safe enrichment permits transfer" 1
    (count_accepted
       (T.accept_transfer ~db ~destinations:[ room_a ] ~envelope ~fetch_teams
          ~cache
          ~rate_limited:(fun () -> false)
          ~access_allowed:(fun () -> true)
          ~now:fixed_now ()));
  Alcotest.(check bool) "fetcher used" true (!calls > 0);
  let calls_before_rate_limit = !calls in
  let throttled =
    normalized_transfer_envelope ~delivery_id:"transfer-team-rate"
      ~labels:[ "bug" ]
  in
  Alcotest.(check int)
    "rate limit fails closed" 0
    (List.length
       (T.plan_transfer ~db ~destinations:[ room_a ] ~envelope:throttled
          ~fetch_teams ~cache
          ~rate_limited:(fun () -> true)
          ~access_allowed:(fun () -> true)
          ~now:fixed_now ())
         .destinations);
  Alcotest.(check int) "rate gate blocks fetcher" calls_before_rate_limit !calls

let suite =
  [
    ( "same room dual match one accept",
      `Quick,
      test_same_room_dual_match_one_accept );
    ("source-only match", `Quick, test_source_only_match);
    ("dest-only match", `Quick, test_dest_only_match);
    ("different rooms fanout", `Quick, test_different_rooms_fanout);
    ("neither matches empty", `Quick, test_neither_matches_empty);
    ("second accept_transfer duplicates", `Quick, test_second_accept_duplicates);
    ( "same room org both views one accept",
      `Quick,
      test_same_room_org_both_views_one_accept );
    ("stable item key uses to_repo", `Quick, test_stable_item_key_uses_to_repo);
    ( "advanced filter controls transfer delivery and accept",
      `Quick,
      test_advanced_filter_controls_transfer_delivery_and_accept );
    ( "transfer forwards safe enrichment hooks",
      `Quick,
      test_transfer_forwards_safe_enrichment_hooks );
  ]

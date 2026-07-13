(** Tests for catch-up reconciliation: one current-state intent per item
    (P19.M3.E3.T002). *)

module D = Github_delivery_intent
module O = Github_delivery_outbox
module R = Github_delivery_reconcile
module P = Github_item_projection
module E = Github_event_envelope
module J = Github_room_event_journal

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  J.ensure_schema db;
  P.ensure_schema db;
  O.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let make_envelope ?(event = "pull_request") ?(action = Some "opened")
    ?(repo = "acme/widget") ?(org = Some "acme") ?(kind = Some E.Pull_request)
    ?(number = Some 42) ?(family = E.Lifecycle) ?(delivery_id = Some "deliv-1")
    ?(actor_login = Some "alice") ?(title = Some "Add feature")
    ?(state = Some "open") ?(draft = Some false) ?(merged = None)
    ?(labels = [ "enhancement" ]) ?(assignees = []) ?(head_sha = Some "abc123")
    ?(html_url = Some "https://github.com/acme/widget/pull/42")
    ?(event_at = Some "2024-01-01T00:00:00Z") () : E.t =
  {
    version = E.envelope_version;
    delivery_id;
    installation_id = Some 99;
    event;
    action;
    repo_full_name = repo;
    org;
    item_kind = kind;
    item_number = number;
    item_node_id = Some "PR_kwDOABC";
    item_url = Some "https://api.github.com/repos/acme/widget/pulls/42";
    html_url;
    family;
    actor = { E.empty_actor with login = actor_login };
    item_author = actor_login;
    before = None;
    after =
      Some
        {
          E.empty_safe_state with
          title;
          state;
          draft;
          merged;
          labels;
          assignees;
          head_sha;
        };
    transfer = None;
    received_at = Some "2024-01-01T00:00:00Z";
    event_at;
    head_sha;
    unsupported = false;
    skip_reason = None;
  }

let seed_lifecycle_projection ~db ~room_id ~number ~title ~state ~delivery_id
    ~now =
  let env =
    make_envelope ~number:(Some number) ~title:(Some title) ~state:(Some state)
      ~delivery_id:(Some delivery_id)
      ~html_url:
        (Some (Printf.sprintf "https://github.com/acme/widget/pull/%d" number))
      ()
  in
  let entry = assert_ok (J.append ~db ~room_id ~envelope:env ~now ()) in
  assert_ok (P.reduce_entry ~db ~entry ())

let seed_update_projection ~db ~room_id ~number ~title ~state ~delivery_open
    ~delivery_update ~now =
  let _ =
    seed_lifecycle_projection ~db ~room_id ~number ~title ~state:"open"
      ~delivery_id:delivery_open ~now
  in
  let env =
    make_envelope ~event:"pull_request" ~action:(Some "edited")
      ~number:(Some number) ~family:E.State_update ~title:(Some title)
      ~state:(Some state) ~delivery_id:(Some delivery_update)
      ~html_url:
        (Some (Printf.sprintf "https://github.com/acme/widget/pull/%d" number))
      ()
  in
  let entry =
    assert_ok (J.append ~db ~room_id ~envelope:env ~now:(now +. 1.) ())
  in
  assert_ok (P.reduce_entry ~db ~entry ())

let sample_intent ~id ~room_id ~item_key ~title ~state ~revision ~now : D.intent
    =
  let proj : P.projection =
    {
      room_id;
      item_key;
      title = Some title;
      state = Some state;
      draft = Some false;
      merged = None;
      labels = [ "enhancement" ];
      assignees = [ "alice" ];
      head_sha = Some "abc123";
      html_url = Some "https://github.com/acme/widget/pull/42";
      last_event_at = Some "2024-01-01T00:00:00Z";
      last_family = Some E.Lifecycle;
      comment_count = 0;
      revision;
      card_kind = P.Lifecycle;
    }
  in
  let intent = D.of_projection ~room_id ~projection:proj ~now () in
  { intent with id }

let status_name = function
  | O.Pending -> "pending"
  | O.In_flight -> "in_flight"
  | O.Succeeded -> "succeeded"
  | O.Dead_letter -> "dead_letter"
  | O.Superseded -> "superseded"

let count_status ~db ~room_id ~item_key ~status =
  let sql =
    {|SELECT COUNT(*) FROM github_delivery_outbox
      WHERE room_id = ? AND item_key = ? AND status = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT item_key));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT status));
  let n =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.INT i -> Int64.to_int i
        | _ -> 0)
    | _ -> 0
  in
  ignore (Sqlite3.finalize stmt);
  n

(* 1. multiple pending for same item → reconcile enqueues one *)
let test_multiple_pending_collapse_to_one () =
  with_db @@ fun db ->
  let room_id = "room-1" in
  let proj =
    seed_lifecycle_projection ~db ~room_id ~number:42 ~title:"Add feature"
      ~state:"open" ~delivery_id:"open-1" ~now:fixed_now
  in
  let item_key = proj.item_key in
  (* Three historical pending intents for the same item. *)
  List.iter
    (fun (id, rev) ->
      let intent =
        sample_intent ~id ~room_id ~item_key ~title:"Add feature" ~state:"open"
          ~revision:rev ~now:fixed_now
      in
      ignore
        (assert_ok (O.enqueue ~db ~room_id ~item_key ~intent ~now:fixed_now ())))
    [ ("ghdi_hist_1", 1); ("ghdi_hist_2", 2); ("ghdi_hist_3", 3) ];
  Alcotest.(check int)
    "three open before" 3
    (assert_ok (O.count_open_for_item ~db ~room_id ~item_key));
  let enqueued = assert_ok (R.reconcile_room ~db ~room_id ~now:fixed_now ()) in
  Alcotest.(check int) "one catchup enqueued" 1 enqueued;
  (* Historical pending superseded; exactly one open (the catchup). *)
  Alcotest.(check int)
    "three superseded" 3
    (count_status ~db ~room_id ~item_key ~status:"superseded");
  Alcotest.(check int)
    "one open after" 1
    (assert_ok (O.count_open_for_item ~db ~room_id ~item_key));
  let claimed = assert_ok (O.claim_due ~db ~now:fixed_now ~limit:10 ()) in
  Alcotest.(check int) "one claimable catchup" 1 (List.length claimed);
  match claimed with
  | [ e ] ->
      Alcotest.(check string) "item" item_key e.item_key;
      Alcotest.(check bool)
        "not a historical id" true
        (e.id <> "ghdi_hist_1" && e.id <> "ghdi_hist_2" && e.id <> "ghdi_hist_3")
  | _ -> Alcotest.fail "expected single catchup claim"

(* 2. catchup intent reflects current projection title/state *)
let test_catchup_reflects_current_projection () =
  with_db @@ fun db ->
  let room_id = "room-1" in
  let proj =
    seed_update_projection ~db ~room_id ~number:7 ~title:"Final title"
      ~state:"closed" ~delivery_open:"open-7" ~delivery_update:"edit-7"
      ~now:fixed_now
  in
  Alcotest.(check (option string)) "proj title" (Some "Final title") proj.title;
  Alcotest.(check (option string)) "proj state" (Some "closed") proj.state;
  (* Stale pending with wrong title/state. *)
  let stale =
    sample_intent ~id:"ghdi_stale" ~room_id ~item_key:proj.item_key
      ~title:"Old title" ~state:"open" ~revision:1 ~now:fixed_now
  in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id ~item_key:proj.item_key ~intent:stale
          ~now:fixed_now ()));
  let plan =
    assert_ok (R.plan_catchup_for_room ~db ~room_id ~now:fixed_now ())
  in
  Alcotest.(check int) "one planned" 1 (List.length plan);
  (match plan with
  | [ c ] ->
      Alcotest.(check string) "item_key" proj.item_key c.item_key;
      Alcotest.(check int) "collapsed from 1 pending" 1 c.collapsed_from;
      Alcotest.(check (option string))
        "catchup title" (Some "Final title") c.intent.title;
      Alcotest.(check (option string))
        "catchup state" (Some "closed") c.intent.state;
      Alcotest.(check (option int))
        "revision" (Some proj.revision) c.intent.projection_revision;
      (match c.intent.kind with
      | D.Update_card -> ()
      | D.Create_lifecycle_card ->
          Alcotest.fail "Update projection must plan Update_card"
      | D.Reply_in_thread -> Alcotest.fail "unexpected Reply_in_thread"
      | D.Plain_message -> Alcotest.fail "unexpected Plain_message");
      Alcotest.(check bool)
        "summary mentions title" true
        (String_util.contains c.intent.summary "Final title")
  | _ -> Alcotest.fail "expected one catchup");
  let enqueued = assert_ok (R.reconcile_room ~db ~room_id ~now:fixed_now ()) in
  Alcotest.(check int) "enqueued" 1 enqueued;
  let claimed = assert_ok (O.claim_due ~db ~now:fixed_now ()) in
  match claimed with
  | [ e ] -> (
      match D.of_json e.intent_json with
      | Error err -> Alcotest.fail err
      | Ok intent ->
          Alcotest.(check (option string))
            "delivered title" (Some "Final title") intent.title;
          Alcotest.(check (option string))
            "delivered state" (Some "closed") intent.state)
  | _ -> Alcotest.fail "expected one delivered catchup"

(* 3. empty room → zero *)
let test_empty_room_zero () =
  with_db @@ fun db ->
  let plan =
    assert_ok
      (R.plan_catchup_for_room ~db ~room_id:"empty-room" ~now:fixed_now ())
  in
  Alcotest.(check int) "plan empty" 0 (List.length plan);
  let enqueued =
    assert_ok (R.reconcile_room ~db ~room_id:"empty-room" ~now:fixed_now ())
  in
  Alcotest.(check int) "enqueued zero" 0 enqueued

(* 4. two items → two catchups *)
let test_two_items_two_catchups () =
  with_db @@ fun db ->
  let room_id = "room-multi" in
  let p1 =
    seed_lifecycle_projection ~db ~room_id ~number:1 ~title:"PR One"
      ~state:"open" ~delivery_id:"open-a" ~now:fixed_now
  in
  let p2 =
    seed_lifecycle_projection ~db ~room_id ~number:2 ~title:"PR Two"
      ~state:"open" ~delivery_id:"open-b" ~now:(fixed_now +. 1.)
  in
  (* Pending backlog on each item (optional for planning all projections). *)
  let i1 =
    sample_intent ~id:"ghdi_a1" ~room_id ~item_key:p1.item_key ~title:"PR One"
      ~state:"open" ~revision:1 ~now:fixed_now
  in
  let i2a =
    sample_intent ~id:"ghdi_b1" ~room_id ~item_key:p2.item_key ~title:"PR Two"
      ~state:"open" ~revision:1 ~now:fixed_now
  in
  let i2b =
    sample_intent ~id:"ghdi_b2" ~room_id ~item_key:p2.item_key ~title:"PR Two"
      ~state:"open" ~revision:2 ~now:fixed_now
  in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id ~item_key:p1.item_key ~intent:i1 ~now:fixed_now
          ()));
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id ~item_key:p2.item_key ~intent:i2a ~now:fixed_now
          ()));
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id ~item_key:p2.item_key ~intent:i2b ~now:fixed_now
          ()));
  let plan =
    assert_ok (R.plan_catchup_for_room ~db ~room_id ~now:fixed_now ())
  in
  Alcotest.(check int) "two planned" 2 (List.length plan);
  let keys =
    List.map (fun (c : R.catchup) -> c.item_key) plan
    |> List.sort String.compare
  in
  Alcotest.(check (list string)) "both items" [ p1.item_key; p2.item_key ] keys;
  List.iter
    (fun (c : R.catchup) ->
      if c.item_key = p1.item_key then
        Alcotest.(check int) "p1 collapsed 1" 1 c.collapsed_from
      else if c.item_key = p2.item_key then
        Alcotest.(check int) "p2 collapsed 2" 2 c.collapsed_from
      else Alcotest.fail ("unexpected item " ^ c.item_key))
    plan;
  let enqueued = assert_ok (R.reconcile_room ~db ~room_id ~now:fixed_now ()) in
  Alcotest.(check int) "two enqueued" 2 enqueued;
  Alcotest.(check int)
    "p1 one open" 1
    (assert_ok (O.count_open_for_item ~db ~room_id ~item_key:p1.item_key));
  Alcotest.(check int)
    "p2 one open" 1
    (assert_ok (O.count_open_for_item ~db ~room_id ~item_key:p2.item_key));
  Alcotest.(check int)
    "p1 historical superseded" 1
    (count_status ~db ~room_id ~item_key:p1.item_key ~status:"superseded");
  Alcotest.(check int)
    "p2 historical superseded" 2
    (count_status ~db ~room_id ~item_key:p2.item_key ~status:"superseded");
  let claimed = assert_ok (O.claim_due ~db ~now:fixed_now ~limit:10 ()) in
  Alcotest.(check int) "two claimable" 2 (List.length claimed)

(* Extra: supersede_pending_for_item alone *)
let test_supersede_pending_for_item () =
  with_db @@ fun db ->
  let room_id = "room-1" in
  let item_key = "pr:acme/widget:9" in
  let i1 =
    sample_intent ~id:"ghdi_s1" ~room_id ~item_key ~title:"X" ~state:"open"
      ~revision:1 ~now:fixed_now
  in
  let i2 =
    sample_intent ~id:"ghdi_s2" ~room_id ~item_key ~title:"X" ~state:"open"
      ~revision:2 ~now:fixed_now
  in
  ignore
    (assert_ok (O.enqueue ~db ~room_id ~item_key ~intent:i1 ~now:fixed_now ()));
  ignore
    (assert_ok (O.enqueue ~db ~room_id ~item_key ~intent:i2 ~now:fixed_now ()));
  ignore (assert_ok (O.claim_due ~db ~now:fixed_now ~limit:1 ()));
  let n = assert_ok (R.supersede_pending_for_item ~db ~room_id ~item_key) in
  Alcotest.(check int) "both open statuses superseded" 2 n;
  Alcotest.(check int)
    "none open" 0
    (assert_ok (O.count_open_for_item ~db ~room_id ~item_key));
  Alcotest.(check int)
    "two superseded" 2
    (count_status ~db ~room_id ~item_key ~status:"superseded");
  (* Idempotent: second call supersedes zero. *)
  let n2 = assert_ok (R.supersede_pending_for_item ~db ~room_id ~item_key) in
  Alcotest.(check int) "second supersede zero" 0 n2

let suite =
  [
    ( "multiple pending collapse to one catchup",
      `Quick,
      test_multiple_pending_collapse_to_one );
    ( "catchup reflects current projection title/state",
      `Quick,
      test_catchup_reflects_current_projection );
    ("empty room zero", `Quick, test_empty_room_zero);
    ("two items two catchups", `Quick, test_two_items_two_catchups);
    ("supersede_pending_for_item", `Quick, test_supersede_pending_for_item);
  ]

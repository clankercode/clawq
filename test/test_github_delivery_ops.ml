(** Tests for delivery diagnostics, metrics, repair, and restart reordering
    (P19.M3.E3.T003). *)

module D = Github_delivery_intent
module O = Github_delivery_outbox
module Ops = Github_delivery_ops
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

let sample_intent ?(id = "ghdi_ops_1") ?(room_id = "room-1")
    ?(item_key = "pr:acme/widget:42") ?(now = fixed_now) () : D.intent =
  let proj : P.projection =
    {
      room_id;
      item_key;
      title = Some "Add feature";
      state = Some "open";
      draft = Some false;
      merged = None;
      labels = [ "enhancement" ];
      assignees = [ "alice" ];
      head_sha = Some "abc123";
      html_url = Some "https://github.com/acme/widget/pull/42";
      last_event_at = Some "2024-01-01T00:00:00Z";
      last_family = Some E.Lifecycle;
      comment_count = 0;
      revision = 1;
      card_kind = P.Lifecycle;
    }
  in
  let intent = D.of_projection ~room_id ~projection:proj ~now () in
  { intent with id }

let contains ~needle s =
  let nlen = String.length needle in
  let slen = String.length s in
  let rec loop i =
    if i + nlen > slen then false
    else if String.sub s i nlen = needle then true
    else loop (i + 1)
  in
  if nlen = 0 then true else loop 0

let status_name = function
  | O.Pending -> "pending"
  | O.In_flight -> "in_flight"
  | O.Succeeded -> "succeeded"
  | O.Dead_letter -> "dead_letter"
  | O.Superseded -> "superseded"

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

(* 1. metrics counts after enqueue / success / fail *)
let test_metrics_counts () =
  with_db @@ fun db ->
  let i1 = sample_intent ~id:"ghdi_m_pending" () in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id:"room-1" ~item_key:i1.item_key ~intent:i1
          ~now:fixed_now ()));
  let i2 = sample_intent ~id:"ghdi_m_ok" ~item_key:"pr:acme/widget:43" () in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id:"room-1" ~item_key:i2.item_key ~intent:i2
          ~now:fixed_now ()));
  ignore (assert_ok (O.claim_due ~db ~now:fixed_now ~limit:10 ()));
  assert_ok (O.mark_success ~db ~id:i2.id ~now:fixed_now ());
  (* Pending remains for i1 only if claim took both; claim claims all due. *)
  let i3 = sample_intent ~id:"ghdi_m_dead" ~item_key:"pr:acme/widget:44" () in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id:"room-1" ~item_key:i3.item_key ~intent:i3
          ~now:fixed_now ()));
  ignore (assert_ok (O.claim_due ~db ~now:fixed_now ~limit:10 ()));
  ignore
    (assert_ok
       (O.mark_failure ~db ~id:i3.id ~error:"gone"
          ~now:(fixed_now +. O.default_max_age_seconds +. 1.)
          ()));
  (* Supersede a fresh pending. *)
  let i4 = sample_intent ~id:"ghdi_m_sup" ~item_key:"pr:acme/widget:45" () in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id:"room-1" ~item_key:i4.item_key ~intent:i4
          ~now:fixed_now ()));
  ignore
    (assert_ok
       (O.supersede_pending_for_item ~db ~room_id:"room-1" ~item_key:i4.item_key));
  let m = assert_ok (Ops.metrics ~db ()) in
  (* i1 claimed → in_flight; i2 succeeded; i3 dead; i4 superseded. *)
  Alcotest.(check int) "pending" 0 m.pending;
  Alcotest.(check int) "in_flight" 1 m.in_flight;
  Alcotest.(check int) "succeeded" 1 m.succeeded;
  Alcotest.(check int) "dead_letter" 1 m.dead_letter;
  Alcotest.(check int) "superseded" 1 m.superseded;
  (* Room filter isolates. *)
  let other =
    sample_intent ~id:"ghdi_m_other" ~room_id:"room-2"
      ~item_key:"pr:acme/widget:99" ()
  in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id:"room-2" ~item_key:other.item_key ~intent:other
          ~now:fixed_now ()));
  let m2 = assert_ok (Ops.metrics ~db ~room_id:"room-2" ()) in
  Alcotest.(check int) "room-2 pending" 1 m2.pending;
  Alcotest.(check int) "room-2 others zero" 0 m2.succeeded

(* 2. diagnose nonempty *)
let test_diagnose_nonempty () =
  with_db @@ fun db ->
  let intent = sample_intent ~id:"ghdi_diag_1" () in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id:"room-1" ~item_key:intent.item_key ~intent
          ~now:fixed_now ()));
  ignore (assert_ok (O.claim_due ~db ~now:fixed_now ()));
  ignore
    (assert_ok
       (O.mark_failure ~db ~id:intent.id ~error:"permanent fail"
          ~now:(fixed_now +. O.default_max_age_seconds +. 5.)
          ()));
  let live =
    sample_intent ~id:"ghdi_diag_live" ~item_key:"pr:acme/widget:9" ()
  in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id:"room-1" ~item_key:live.item_key ~intent:live
          ~now:fixed_now ()));
  let lines = Ops.diagnose ~db () in
  Alcotest.(check bool) "nonempty" true (List.length lines > 0);
  let blob = String.concat "\n" lines in
  Alcotest.(check bool) "has pending" true (contains ~needle:"pending:" blob);
  Alcotest.(check bool)
    "has dead_letter" true
    (contains ~needle:"dead_letter:" blob);
  Alcotest.(check bool)
    "has oldest" true
    (contains ~needle:"oldest_pending" blob);
  Alcotest.(check bool)
    "has dead sample" true
    (contains ~needle:"dead_letter sample" blob
    || contains ~needle:"dead_letter_samples" blob);
  Alcotest.(check bool)
    "sample id present" true
    (contains ~needle:"ghdi_diag_1" blob)

(* 3. repair_stale_in_flight requeues *)
let test_repair_stale_in_flight () =
  with_db @@ fun db ->
  let intent = sample_intent ~id:"ghdi_stale_1" () in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id:"room-1" ~item_key:intent.item_key ~intent
          ~now:fixed_now ()));
  let claimed = assert_ok (O.claim_due ~db ~now:fixed_now ~limit:10 ()) in
  Alcotest.(check int) "claimed" 1 (List.length claimed);
  Alcotest.(check string)
    "in_flight" "in_flight"
    (status_name (List.hd claimed).status);
  (* Not yet stale under 300s threshold. *)
  let n0 =
    assert_ok
      (Ops.repair_stale_in_flight ~db ~older_than_seconds:300.
         ~now:(fixed_now +. 10.) ())
  in
  Alcotest.(check int) "not stale yet" 0 n0;
  let m0 = assert_ok (Ops.metrics ~db ()) in
  Alcotest.(check int) "still in_flight" 1 m0.in_flight;
  (* Past threshold: next_attempt_at=fixed_now, now=fixed_now+400, older=300. *)
  let n1 =
    assert_ok
      (Ops.repair_stale_in_flight ~db ~older_than_seconds:300.
         ~now:(fixed_now +. 400.) ())
  in
  Alcotest.(check int) "requeued" 1 n1;
  let m1 = assert_ok (Ops.metrics ~db ()) in
  Alcotest.(check int) "pending after repair" 1 m1.pending;
  Alcotest.(check int) "in_flight cleared" 0 m1.in_flight

(* 4. requeue_dead_letter *)
let test_requeue_dead_letter () =
  with_db @@ fun db ->
  let intent = sample_intent ~id:"ghdi_requeue_1" () in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id:"room-1" ~item_key:intent.item_key ~intent
          ~now:fixed_now ()));
  ignore (assert_ok (O.claim_due ~db ~now:fixed_now ()));
  let dead =
    assert_ok
      (O.mark_failure ~db ~id:intent.id ~error:"exhausted"
         ~now:(fixed_now +. O.default_max_age_seconds +. 1.)
         ())
  in
  Alcotest.(check string) "dead" "dead_letter" (status_name dead.status);
  assert_ok
    (Ops.requeue_dead_letter ~db ~id:intent.id
       ~now:(fixed_now +. O.default_max_age_seconds +. 10.)
       ());
  let m = assert_ok (Ops.metrics ~db ()) in
  Alcotest.(check int) "pending again" 1 m.pending;
  Alcotest.(check int) "no dead" 0 m.dead_letter;
  let claimed =
    assert_ok
      (O.claim_due ~db
         ~now:(fixed_now +. O.default_max_age_seconds +. 10.)
         ~limit:10 ())
  in
  Alcotest.(check int) "claimable" 1 (List.length claimed);
  (* Non-dead id errors. *)
  (match Ops.requeue_dead_letter ~db ~id:intent.id ~now:fixed_now () with
  | Ok () -> Alcotest.fail "expected error requeueing non-dead"
  | Error _ -> ());
  match Ops.requeue_dead_letter ~db ~id:"missing" ~now:fixed_now () with
  | Ok () -> Alcotest.fail "expected missing error"
  | Error msg ->
      Alcotest.(check bool)
        "mentions id" true
        (contains ~needle:"missing" msg || contains ~needle:"dead_letter" msg)

(* 5. restart reorder: claim_due order stable by next_attempt_at *)
let test_restart_reorder_claim_due () =
  with_db @@ fun db ->
  (* Enqueue three intents with different due times via failure backoff.
     Create at staggered times so created_at/next_attempt_at differ. *)
  let mk id t =
    let intent =
      sample_intent ~id
        ~item_key:(Printf.sprintf "pr:acme/widget:%s" id)
        ~now:t ()
    in
    ignore
      (assert_ok
         (O.enqueue ~db ~room_id:"room-1" ~item_key:intent.item_key ~intent
            ~now:t ()));
    intent
  in
  (* Due immediately at different times; claim order should follow next_attempt_at. *)
  let _a = mk "ghdi_ord_c" (fixed_now +. 30.) in
  let _b = mk "ghdi_ord_a" fixed_now in
  let _c = mk "ghdi_ord_b" (fixed_now +. 10.) in
  let claimed =
    assert_ok (O.claim_due ~db ~now:(fixed_now +. 100.) ~limit:10 ())
  in
  Alcotest.(check int) "three claimed" 3 (List.length claimed);
  let ids = List.map (fun (e : O.entry) -> e.id) claimed in
  Alcotest.(check (list string))
    "order by next_attempt_at ASC"
    [ "ghdi_ord_a"; "ghdi_ord_b"; "ghdi_ord_c" ]
    ids;
  (* Simulate restart: rows still in_flight; re-claim preserves order. *)
  let reclaimed =
    assert_ok (O.claim_due ~db ~now:(fixed_now +. 100.) ~limit:10 ())
  in
  let ids2 = List.map (fun (e : O.entry) -> e.id) reclaimed in
  Alcotest.(check (list string))
    "stable after restart reclaim"
    [ "ghdi_ord_a"; "ghdi_ord_b"; "ghdi_ord_c" ]
    ids2;
  (* After failure with different next times, order still by next_attempt_at. *)
  ignore
    (assert_ok
       (O.mark_failure ~db ~id:"ghdi_ord_a" ~error:"a" ~now:(fixed_now +. 100.)
          ()));
  (* attempts=1 → +60s → due at fixed_now+160 *)
  ignore
    (assert_ok
       (O.mark_failure ~db ~id:"ghdi_ord_c" ~error:"c" ~now:(fixed_now +. 100.)
          ()));
  ignore
    (assert_ok
       (O.mark_failure ~db ~id:"ghdi_ord_b" ~error:"b" ~now:(fixed_now +. 100.)
          ()));
  let after_backoff =
    assert_ok (O.claim_due ~db ~now:(fixed_now +. 200.) ~limit:10 ())
  in
  let ids3 = List.map (fun (e : O.entry) -> e.id) after_backoff in
  (* All three have same backoff from same now → same next_attempt_at;
     tie-break by id ASC. *)
  Alcotest.(check (list string))
    "tie-break by id ASC"
    [ "ghdi_ord_a"; "ghdi_ord_b"; "ghdi_ord_c" ]
    ids3

(* 6. reconcile after restart still one per item (light integration) *)
let test_reconcile_after_restart_one_per_item () =
  with_db @@ fun db ->
  let room_id = "room-1" in
  let proj42 =
    seed_lifecycle_projection ~db ~room_id ~number:42 ~title:"PR 42"
      ~state:"open" ~delivery_id:"open-42" ~now:fixed_now
  in
  let proj7 =
    seed_lifecycle_projection ~db ~room_id ~number:7 ~title:"PR 7" ~state:"open"
      ~delivery_id:"open-7" ~now:fixed_now
  in
  (* Flood each item with multiple pending intents. *)
  List.iter
    (fun (id, item_key, n) ->
      let intent =
        sample_intent ~id ~room_id ~item_key
          ~now:(fixed_now +. float_of_int n)
          ()
      in
      ignore
        (assert_ok
           (O.enqueue ~db ~room_id ~item_key ~intent
              ~now:(fixed_now +. float_of_int n)
              ())))
    [
      ("ghdi_rs_42_1", proj42.item_key, 0);
      ("ghdi_rs_42_2", proj42.item_key, 1);
      ("ghdi_rs_42_3", proj42.item_key, 2);
      ("ghdi_rs_7_1", proj7.item_key, 0);
      ("ghdi_rs_7_2", proj7.item_key, 1);
    ];
  (* Simulate worker crash: claim leaves rows in_flight. *)
  let claimed =
    assert_ok (O.claim_due ~db ~now:(fixed_now +. 10.) ~limit:32 ())
  in
  Alcotest.(check int) "all claimed in_flight" 5 (List.length claimed);
  List.iter
    (fun (e : O.entry) ->
      Alcotest.(check string) "in_flight" "in_flight" (status_name e.status))
    claimed;
  (* Restart recovery: reclaim due in_flight, then reconcile. *)
  let reclaimed =
    assert_ok (O.claim_due ~db ~now:(fixed_now +. 10.) ~limit:32 ())
  in
  Alcotest.(check int) "reclaim after restart" 5 (List.length reclaimed);
  let enqueued =
    assert_ok (R.reconcile_room ~db ~room_id ~now:(fixed_now +. 20.) ())
  in
  Alcotest.(check int) "two catchups (one per item)" 2 enqueued;
  Alcotest.(check int)
    "item 42 one open" 1
    (assert_ok (O.count_open_for_item ~db ~room_id ~item_key:proj42.item_key));
  Alcotest.(check int)
    "item 7 one open" 1
    (assert_ok (O.count_open_for_item ~db ~room_id ~item_key:proj7.item_key));
  let open_total =
    let m = assert_ok (Ops.metrics ~db ~room_id ()) in
    m.pending + m.in_flight
  in
  Alcotest.(check int) "exactly two open total" 2 open_total;
  (* Historical flood superseded. *)
  Alcotest.(check int)
    "five superseded historical" 5
    (assert_ok (O.count_status ~db ~status:O.Superseded ~room_id ()))

let suite =
  [
    ("metrics counts after enqueue/success/fail", `Quick, test_metrics_counts);
    ("diagnose nonempty", `Quick, test_diagnose_nonempty);
    ("repair_stale_in_flight requeues", `Quick, test_repair_stale_in_flight);
    ("requeue_dead_letter", `Quick, test_requeue_dead_letter);
    ( "restart reorder claim_due by next_attempt_at",
      `Quick,
      test_restart_reorder_claim_due );
    ( "reconcile after restart still one per item",
      `Quick,
      test_reconcile_after_restart_one_per_item );
  ]

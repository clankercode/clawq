(** Tests for 24h retrying delivery outbox + dead letters (P19.M3.E3.T001). *)

module D = Github_delivery_intent
module O = Github_delivery_outbox
module P = Github_item_projection
module E = Github_event_envelope

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  O.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let sample_intent ?(id = "ghdi_test_1") ?(room_id = "room-1")
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
  (* Stable id for idempotency tests. *)
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

(* 1. enqueue creates pending *)
let test_enqueue_creates_pending () =
  with_db @@ fun db ->
  let intent = sample_intent () in
  let entry =
    assert_ok
      (O.enqueue ~db ~room_id:"room-1" ~item_key:intent.item_key ~intent
         ~now:fixed_now ())
  in
  Alcotest.(check string) "id" intent.id entry.id;
  Alcotest.(check string) "room" "room-1" entry.room_id;
  Alcotest.(check string) "item_key" intent.item_key entry.item_key;
  Alcotest.(check string) "status" "pending" (status_name entry.status);
  Alcotest.(check int) "attempts" 0 entry.attempts;
  Alcotest.(check (option string)) "no error" None entry.last_error;
  Alcotest.(check (option string)) "not dead" None entry.dead_lettered_at;
  Alcotest.(check string)
    "due immediately"
    (Time_util.iso8601_utc ~t:fixed_now ())
    entry.next_attempt_at;
  Alcotest.(check string)
    "created_at"
    (Time_util.iso8601_utc ~t:fixed_now ())
    entry.created_at;
  (* Idempotent re-enqueue returns the same row. *)
  let again =
    assert_ok
      (O.enqueue ~db ~room_id:"room-1" ~item_key:intent.item_key ~intent
         ~now:(fixed_now +. 10.) ())
  in
  Alcotest.(check string) "same id" entry.id again.id;
  Alcotest.(check string) "same created" entry.created_at again.created_at

(* 2. claim_due returns due *)
let test_claim_due_returns_due () =
  with_db @@ fun db ->
  let intent = sample_intent ~id:"ghdi_claim_1" () in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id:"room-1" ~item_key:intent.item_key ~intent
          ~now:fixed_now ()));
  let claimed = assert_ok (O.claim_due ~db ~now:fixed_now ~limit:10 ()) in
  Alcotest.(check int) "one due" 1 (List.length claimed);
  (match claimed with
  | [ e ] ->
      Alcotest.(check string) "claimed id" intent.id e.id;
      Alcotest.(check string) "in_flight" "in_flight" (status_name e.status)
  | _ -> Alcotest.fail "expected single claim");
  (* Second claim while still in-flight and due reclaims for restart recovery. *)
  let reclaimed = assert_ok (O.claim_due ~db ~now:fixed_now ~limit:10 ()) in
  Alcotest.(check int) "reclaim in_flight" 1 (List.length reclaimed);
  (* Not-yet-due after failure: claim empty at early time. *)
  let failed =
    assert_ok
      (O.mark_failure ~db ~id:intent.id ~error:"transient 503" ~now:fixed_now ())
  in
  Alcotest.(check string) "pending retry" "pending" (status_name failed.status);
  Alcotest.(check int) "attempts" 1 failed.attempts;
  let empty = assert_ok (O.claim_due ~db ~now:fixed_now ~limit:10 ()) in
  Alcotest.(check int) "not due yet" 0 (List.length empty)

(* 3. mark_success *)
let test_mark_success () =
  with_db @@ fun db ->
  let intent = sample_intent ~id:"ghdi_ok_1" () in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id:"room-1" ~item_key:intent.item_key ~intent
          ~now:fixed_now ()));
  let claimed = assert_ok (O.claim_due ~db ~now:fixed_now ()) in
  Alcotest.(check int) "claimed" 1 (List.length claimed);
  assert_ok (O.mark_success ~db ~id:intent.id ~now:fixed_now ());
  let again = assert_ok (O.claim_due ~db ~now:(fixed_now +. 10.) ()) in
  Alcotest.(check int) "no more claims" 0 (List.length again);
  (* mark_success on already-succeeded fails. *)
  match O.mark_success ~db ~id:intent.id ~now:fixed_now () with
  | Ok () -> Alcotest.fail "expected error on second success"
  | Error _ -> ()

(* 4. mark_failure schedules retry *)
let test_mark_failure_schedules_retry () =
  with_db @@ fun db ->
  let intent = sample_intent ~id:"ghdi_retry_1" () in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id:"room-1" ~item_key:intent.item_key ~intent
          ~now:fixed_now ()));
  ignore (assert_ok (O.claim_due ~db ~now:fixed_now ()));
  let e1 =
    assert_ok
      (O.mark_failure ~db ~id:intent.id ~error:"connection reset" ~now:fixed_now
         ())
  in
  Alcotest.(check string) "pending" "pending" (status_name e1.status);
  Alcotest.(check int) "attempts 1" 1 e1.attempts;
  (* attempts=1 → backoff min(3600, 30*2^1) = 60s *)
  let expected_next = Time_util.iso8601_utc ~t:(fixed_now +. 60.) () in
  Alcotest.(check string) "next +60s" expected_next e1.next_attempt_at;
  (match e1.last_error with
  | Some err ->
      Alcotest.(check bool)
        "error stored" true
        (contains ~needle:"connection reset" err)
  | None -> Alcotest.fail "expected last_error");
  (* Second failure: attempts=2 → 30*2^2 = 120s from now *)
  ignore (assert_ok (O.claim_due ~db ~now:(fixed_now +. 60.) ()));
  let e2 =
    assert_ok
      (O.mark_failure ~db ~id:intent.id ~error:"timeout" ~now:(fixed_now +. 60.)
         ())
  in
  Alcotest.(check int) "attempts 2" 2 e2.attempts;
  let expected_next2 = Time_util.iso8601_utc ~t:(fixed_now +. 60. +. 120.) () in
  Alcotest.(check string) "next +120s" expected_next2 e2.next_attempt_at

(* 5. after 24h age failures → dead_letter *)
let test_after_24h_dead_letter () =
  with_db @@ fun db ->
  let intent = sample_intent ~id:"ghdi_dead_1" () in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id:"room-1" ~item_key:intent.item_key ~intent
          ~now:fixed_now ()));
  ignore (assert_ok (O.claim_due ~db ~now:fixed_now ()));
  let past_24h = fixed_now +. O.default_max_age_seconds +. 1. in
  let dead =
    assert_ok
      (O.mark_failure ~db ~id:intent.id ~error:"still failing" ~now:past_24h ())
  in
  Alcotest.(check string) "dead" "dead_letter" (status_name dead.status);
  Alcotest.(check int) "attempts" 1 dead.attempts;
  (match dead.dead_lettered_at with
  | Some t ->
      Alcotest.(check string)
        "dead_lettered_at"
        (Time_util.iso8601_utc ~t:past_24h ())
        t
  | None -> Alcotest.fail "expected dead_lettered_at");
  (* Dead letters are not claimable. *)
  let claimed = assert_ok (O.claim_due ~db ~now:past_24h ()) in
  Alcotest.(check int) "not claimable" 0 (List.length claimed);
  (* Exactly at 24h boundary: created_at <= cutoff when now - created = max_age. *)
  let intent2 = sample_intent ~id:"ghdi_dead_boundary" () in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id:"room-1" ~item_key:intent2.item_key
          ~intent:intent2 ~now:fixed_now ()));
  ignore (assert_ok (O.claim_due ~db ~now:fixed_now ()));
  let at_24h = fixed_now +. O.default_max_age_seconds in
  let boundary =
    assert_ok (O.mark_failure ~db ~id:intent2.id ~error:"edge" ~now:at_24h ())
  in
  Alcotest.(check string)
    "boundary dead" "dead_letter"
    (status_name boundary.status)

(* 6. list_dead_letters *)
let test_list_dead_letters () =
  with_db @@ fun db ->
  let mk id t =
    let intent = sample_intent ~id () in
    ignore
      (assert_ok
         (O.enqueue ~db ~room_id:"room-1" ~item_key:intent.item_key ~intent
            ~now:t ()));
    ignore (assert_ok (O.claim_due ~db ~now:t ()));
    assert_ok
      (O.mark_failure ~db ~id ~error:"gone"
         ~now:(t +. O.default_max_age_seconds +. 5.)
         ())
  in
  let d1 = mk "ghdi_list_1" fixed_now in
  let d2 = mk "ghdi_list_2" (fixed_now +. 10.) in
  Alcotest.(check string) "d1 dead" "dead_letter" (status_name d1.status);
  Alcotest.(check string) "d2 dead" "dead_letter" (status_name d2.status);
  (* Also enqueue a live pending that must not appear. *)
  let live = sample_intent ~id:"ghdi_list_live" () in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id:"room-1" ~item_key:live.item_key ~intent:live
          ~now:fixed_now ()));
  let dead = assert_ok (O.list_dead_letters ~db ~limit:10 ()) in
  Alcotest.(check int) "two dead" 2 (List.length dead);
  List.iter
    (fun (e : O.entry) ->
      Alcotest.(check string) "status dead" "dead_letter" (status_name e.status);
      Alcotest.(check bool) "not live id" false (e.id = live.id))
    dead;
  let limited = assert_ok (O.list_dead_letters ~db ~limit:1 ()) in
  Alcotest.(check int) "limit 1" 1 (List.length limited)

(* 7. intent secrets not in error storage (redact errors) *)
let test_intent_secrets_not_in_error_storage () =
  with_db @@ fun db ->
  let intent = sample_intent ~id:"ghdi_secret_1" () in
  ignore
    (assert_ok
       (O.enqueue ~db ~room_id:"room-1" ~item_key:intent.item_key ~intent
          ~now:fixed_now ()));
  ignore (assert_ok (O.claim_due ~db ~now:fixed_now ()));
  let token = "ghp_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let err = Printf.sprintf "delivery failed Authorization: Bearer %s" token in
  let e =
    assert_ok (O.mark_failure ~db ~id:intent.id ~error:err ~now:fixed_now ())
  in
  (match e.last_error with
  | None -> Alcotest.fail "expected redacted last_error"
  | Some stored ->
      Alcotest.(check bool) "no raw token" false (contains ~needle:token stored);
      Alcotest.(check bool)
        "no ghp_ leak" false
        (contains ~needle:"ghp_" stored);
      Alcotest.(check bool)
        "redacted marker" true
        (contains ~needle:"REDACTED" stored || contains ~needle:"***" stored));
  (* Intent JSON itself must remain secret-free (contract of delivery intent). *)
  let intent_s = Yojson.Safe.to_string e.intent_json in
  Alcotest.(check bool)
    "no ghp in intent" false
    (contains ~needle:"ghp_" intent_s);
  Alcotest.(check bool)
    "no bearer in intent" false
    (contains ~needle:"Bearer" intent_s);
  (* Non-secret errors pass through. *)
  ignore (assert_ok (O.claim_due ~db ~now:(fixed_now +. 120.) ()));
  let e2 =
    assert_ok
      (O.mark_failure ~db ~id:intent.id ~error:"HTTP 503 service unavailable"
         ~now:(fixed_now +. 120.) ())
  in
  match e2.last_error with
  | Some s ->
      Alcotest.(check bool) "plain error kept" true (contains ~needle:"503" s)
  | None -> Alcotest.fail "expected plain error"

let suite =
  [
    ("enqueue creates pending", `Quick, test_enqueue_creates_pending);
    ("claim_due returns due", `Quick, test_claim_due_returns_due);
    ("mark_success", `Quick, test_mark_success);
    ("mark_failure schedules retry", `Quick, test_mark_failure_schedules_retry);
    ("after 24h age → dead_letter", `Quick, test_after_24h_dead_letter);
    ("list_dead_letters", `Quick, test_list_dead_letters);
    ( "intent secrets not in error storage",
      `Quick,
      test_intent_secrets_not_in_error_storage );
  ]

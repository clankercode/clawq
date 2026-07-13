(** Tests for GitHub action receipt ↔ webhook reconciliation without loops
    (P19.M4.E2.T004). *)

module E = Github_event_envelope
module J = Github_room_event_journal
module P = Github_item_projection
module O = Github_delivery_outbox
module A = Github_action_reconcile
module R = Github_route_match

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  J.ensure_schema db;
  P.ensure_schema db;
  O.ensure_schema db;
  A.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let room_id = "room-1"
let item_key = "pr:acme/widget:42"
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let make_envelope ?(event = "pull_request") ?(action = Some "closed")
    ?(repo = "acme/widget") ?(org = Some "acme") ?(kind = Some E.Pull_request)
    ?(number = Some 42) ?(family = E.Lifecycle)
    ?(delivery_id = Some "deliv-self-1") ?(actor_login = Some "clawq-bot")
    ?(actor_type = Some "Bot") ?(title = Some "Add feature")
    ?(state = Some "closed") ?(draft = Some false) ?(merged = Some true)
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
    actor = { E.empty_actor with login = actor_login; type_ = actor_type };
    before =
      Some
        {
          E.empty_safe_state with
          title;
          state = Some "open";
          draft;
          merged = Some false;
          labels;
          assignees;
          head_sha;
        };
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

let base_correlation ?(action = "merge") ?(delivery_id = None)
    ?(receipt_id = Some "receipt-1") ?(plan_id = Some "plan-1")
    ?(github_ref = Some "abc123") ?(actor_mode = "pilot") () : A.correlation =
  {
    room_id;
    item_key = Some item_key;
    action;
    plan_id;
    receipt_id;
    delivery_id;
    github_ref;
    actor_mode;
  }

let result_tag = function
  | A.Closed { first_time; _ } ->
      if first_time then "closed_first" else "closed_again"
  | A.No_matching_receipt -> "no_matching_receipt"
  | A.Already_closed -> "already_closed"
  | A.Ignored_human_event -> "ignored_human_event"

let count_outbox ~db =
  let sql = {|SELECT COUNT(*) FROM github_delivery_outbox|} in
  let stmt = Sqlite3.prepare db sql in
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

let count_correlations ~db ?status () =
  let sql =
    match status with
    | None -> {|SELECT COUNT(*) FROM github_action_correlations|}
    | Some st ->
        {|SELECT COUNT(*) FROM github_action_correlations WHERE status = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  (match status with
  | None -> ()
  | Some st -> ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT st)));
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

let stored_fields ~db =
  let sql =
    {|SELECT room_id, item_key, action, plan_id, receipt_id, delivery_id,
             github_ref, actor_mode, status FROM github_action_correlations
      LIMIT 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  let row =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        let text i =
          match Sqlite3.column stmt i with
          | Sqlite3.Data.TEXT s -> s
          | Sqlite3.Data.NULL -> ""
          | _ -> ""
        in
        Some
          ( text 0,
            text 1,
            text 2,
            text 3,
            text 4,
            text 5,
            text 6,
            text 7,
            text 8 )
    | _ -> None
  in
  ignore (Sqlite3.finalize stmt);
  row

(* 1. record + reconcile closes once and updates projection *)
let test_record_and_reconcile_closes_once () =
  with_db @@ fun db ->
  let corr = base_correlation () in
  assert_ok (A.record_correlation ~db ~correlation:corr ~now:fixed_now ());
  Alcotest.(check int)
    "one open correlation" 1
    (count_correlations ~db ~status:"open" ());
  let env = make_envelope ~action:(Some "closed") ~merged:(Some true) () in
  Alcotest.(check string) "item key" item_key (R.canonical_item_key env);
  let outbox_before = count_outbox ~db in
  let r =
    A.reconcile_webhook ~db ~room_id ~envelope:env ~now:(fixed_now +. 1.) ()
  in
  Alcotest.(check string) "closed first" "closed_first" (result_tag r);
  (match r with
  | A.Closed { correlation = c; first_time = true } ->
      Alcotest.(check string) "room" room_id c.room_id;
      Alcotest.(check (option string)) "item" (Some item_key) c.item_key;
      Alcotest.(check string) "action" "merge" c.action;
      Alcotest.(check (option string)) "receipt" (Some "receipt-1") c.receipt_id
  | _ -> Alcotest.fail "expected Closed first_time");
  Alcotest.(check int)
    "no open left" 0
    (count_correlations ~db ~status:"open" ());
  Alcotest.(check int)
    "one closed" 1
    (count_correlations ~db ~status:"closed" ());
  Alcotest.(check int) "outbox unchanged" outbox_before (count_outbox ~db);
  (* Projection updated from the verified webhook. *)
  match assert_ok (P.get ~db ~room_id ~item_key) with
  | None -> Alcotest.fail "expected projection after reconcile"
  | Some proj ->
      Alcotest.(check (option string)) "state closed" (Some "closed") proj.state;
      Alcotest.(check (option bool)) "merged" (Some true) proj.merged

(* 2. second webhook → Already_closed *)
let test_second_webhook_already_closed () =
  with_db @@ fun db ->
  let corr = base_correlation () in
  assert_ok (A.record_correlation ~db ~correlation:corr ~now:fixed_now ());
  let env1 =
    make_envelope ~delivery_id:(Some "deliv-self-1") ~action:(Some "closed") ()
  in
  let r1 =
    A.reconcile_webhook ~db ~room_id ~envelope:env1 ~now:(fixed_now +. 1.) ()
  in
  Alcotest.(check string) "first close" "closed_first" (result_tag r1);
  let env2 =
    make_envelope ~delivery_id:(Some "deliv-self-2") ~action:(Some "closed") ()
  in
  let r2 =
    A.reconcile_webhook ~db ~room_id ~envelope:env2 ~now:(fixed_now +. 2.) ()
  in
  Alcotest.(check string)
    "second already closed" "already_closed" (result_tag r2);
  Alcotest.(check int)
    "still one closed row" 1
    (count_correlations ~db ~status:"closed" ());
  Alcotest.(check int)
    "no open recreated" 0
    (count_correlations ~db ~status:"open" ())

(* 3. human event without correlation remains distinct *)
let test_human_event_without_correlation_distinct () =
  with_db @@ fun db ->
  (* No correlation recorded. *)
  let human =
    make_envelope ~delivery_id:(Some "deliv-human-1")
      ~actor_login:(Some "alice") ~actor_type:(Some "User")
      ~action:(Some "closed") ~merged:(Some false) ()
  in
  let r = A.reconcile_webhook ~db ~room_id ~envelope:human ~now:fixed_now () in
  Alcotest.(check string) "ignored human" "ignored_human_event" (result_tag r);
  Alcotest.(check int) "no correlations created" 0 (count_correlations ~db ());
  (* Bot/app event without correlation is No_matching_receipt (distinct). *)
  let bot =
    make_envelope ~delivery_id:(Some "deliv-bot-orphan")
      ~actor_login:(Some "dependabot[bot]") ~actor_type:(Some "Bot")
      ~action:(Some "opened") ()
  in
  let r2 = A.reconcile_webhook ~db ~room_id ~envelope:bot ~now:fixed_now () in
  Alcotest.(check string) "orphan bot" "no_matching_receipt" (result_tag r2)

(* 4. no re-trigger of work (no new outbox enqueue) *)
let test_no_outbox_retrigger () =
  with_db @@ fun db ->
  let corr = base_correlation ~action:"comment" () in
  assert_ok (A.record_correlation ~db ~correlation:corr ~now:fixed_now ());
  let before = count_outbox ~db in
  Alcotest.(check int) "outbox empty before" 0 before;
  let env =
    make_envelope ~event:"issue_comment" ~action:(Some "created")
      ~family:E.Comment ~delivery_id:(Some "deliv-comment-1")
      ~actor_type:(Some "Bot") ()
  in
  let r =
    A.reconcile_webhook ~db ~room_id ~envelope:env ~now:(fixed_now +. 1.) ()
  in
  Alcotest.(check string) "closed" "closed_first" (result_tag r);
  Alcotest.(check int) "still no outbox rows" 0 (count_outbox ~db);
  (* Replay also must not enqueue. *)
  let r2 =
    A.reconcile_webhook ~db ~room_id ~envelope:env ~now:(fixed_now +. 2.) ()
  in
  Alcotest.(check string) "already closed" "already_closed" (result_tag r2);
  Alcotest.(check int) "outbox still empty" 0 (count_outbox ~db);
  (* Open-count helper also reports zero open work for the item. *)
  let open_n = assert_ok (O.count_open_for_item ~db ~room_id ~item_key) in
  Alcotest.(check int) "no open outbox work" 0 open_n

(* 5. secret-free storage (redaction on write; no secret columns) *)
let test_secret_free_storage () =
  with_db @@ fun db ->
  let dirty : A.correlation =
    {
      room_id;
      item_key = Some item_key;
      action = "merge token=ghp_SECRETvalue1234567890";
      plan_id = Some "plan-1";
      receipt_id = Some "receipt-1";
      delivery_id = Some "deliv-x";
      github_ref = Some "Bearer ghp_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
      actor_mode = "pilot";
    }
  in
  assert_ok (A.record_correlation ~db ~correlation:dirty ~now:fixed_now ());
  match stored_fields ~db with
  | None -> Alcotest.fail "expected stored row"
  | Some (rid, ik, action, plan, receipt, deliv, gref, mode, status) ->
      Alcotest.(check string) "room" room_id rid;
      Alcotest.(check string) "item" item_key ik;
      Alcotest.(check string) "plan" "plan-1" plan;
      Alcotest.(check string) "receipt" "receipt-1" receipt;
      Alcotest.(check string) "delivery" "deliv-x" deliv;
      Alcotest.(check string) "mode" "pilot" mode;
      Alcotest.(check string) "status open" "open" status;
      Alcotest.(check bool)
        "action redacted" true
        (not (String.contains action 'S' && String.contains action 'E'));
      Alcotest.(check bool)
        "no raw ghp_ secret in action" false
        (let needle = "ghp_SECRET" in
         let n = String.length needle in
         let h = String.length action in
         let rec loop i =
           if i > h - n then false
           else if String.sub action i n = needle then true
           else loop (i + 1)
         in
         loop 0);
      Alcotest.(check bool)
        "github_ref redacted" true
        (let needle = "ghp_AAAA" in
         let n = String.length needle in
         let h = String.length gref in
         let rec loop i =
           if i > h - n then false
           else if String.sub gref i n = needle then true
           else loop (i + 1)
         in
         not (loop 0));
      Alcotest.(check bool)
        "github_ref has redaction marker" true
        (let hay = String.lowercase_ascii gref in
         let needle = "redacted" in
         let n = String.length needle in
         let h = String.length hay in
         let rec loop i =
           if i > h - n then false
           else if String.sub hay i n = needle then true
           else loop (i + 1)
         in
         loop 0)

(* Delivery_id direct match when recorded up front *)
let test_delivery_id_match () =
  with_db @@ fun db ->
  let corr =
    base_correlation ~action:"label" ~delivery_id:(Some "deliv-label-9") ()
  in
  assert_ok (A.record_correlation ~db ~correlation:corr ~now:fixed_now ());
  let env =
    make_envelope ~event:"pull_request" ~action:(Some "labeled")
      ~family:E.State_update ~delivery_id:(Some "deliv-label-9")
      ~actor_type:(Some "Bot") ()
  in
  let r =
    A.reconcile_webhook ~db ~room_id ~envelope:env ~now:(fixed_now +. 1.) ()
  in
  Alcotest.(check string) "closed via delivery id" "closed_first" (result_tag r)

let suite =
  [
    ( "record + reconcile closes once",
      `Quick,
      test_record_and_reconcile_closes_once );
    ("second webhook already closed", `Quick, test_second_webhook_already_closed);
    ( "human event without correlation distinct",
      `Quick,
      test_human_event_without_correlation_distinct );
    ("no outbox re-trigger", `Quick, test_no_outbox_retrigger);
    ("secret-free storage", `Quick, test_secret_free_storage);
    ("delivery_id direct match", `Quick, test_delivery_id_match);
  ]

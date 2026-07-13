(** Tests for GitHub action receipt ↔ webhook reconciliation without loops
    (P19.M4.E2.T004), Actor snapshot propagation (P21.M1.E3.T006), and native
    attribution receipt correlation across action families, delayed completion,
    and webhook reordering (P21.M3.E3.T005). *)

module E = Github_event_envelope
module J = Github_room_event_journal
module P = Github_item_projection
module O = Github_delivery_outbox
module A = Github_action_reconcile
module R = Github_route_match
module PI = Principal_identity
module Snap = Actor_snapshot

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
    ?(github_ref = Some "abc123") ?(actor_mode = "pilot")
    ?(requested_mode = None) ?(resolved_mode = None) ?(actor_snapshot = None)
    ?(expected_github_login = None) ?(job_id = None)
    ?(attribution_receipt_id = None) ?(github_user_id = None)
    ?(native_actor_kind = None) () : A.correlation =
  A.make_correlation ~room_id ~action ~actor_mode ?item_key:(Some item_key)
    ?plan_id ?receipt_id ?delivery_id ?github_ref ?requested_mode
    ?resolved_mode:
      (match resolved_mode with Some _ as r -> r | None -> Some actor_mode)
    ?actor_snapshot ?expected_github_login ?job_id ?attribution_receipt_id
    ?github_user_id ?native_actor_kind ()

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
      requested_mode = Some "user";
      resolved_mode = Some "pilot";
      actor_snapshot = None;
      expected_github_login = None;
      job_id = None;
      attribution_receipt_id = None;
      github_user_id = None;
      native_actor_kind = None;
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

(* ---- P21.M1.E3.T006: Actor snapshot on receipts + identity isolation ---- *)

let sample_actor_key ?(user = "user-ada") () =
  assert_ok
    (PI.make_connector_actor_key ~connector:PI.Teams
       ~tenant_or_workspace:"tenant-acme" ~immutable_user_id:user)

let make_snapshot ?(principal = "prin_ada") ?(user = "user-ada")
    ?(display_name = "Ada") ?(principal_revision = 3) ?(actor_revision = 2)
    ?(identity_link_revision = 7) ?(snapshot_id = "actorsnap_receipt_ada") () =
  let principal_id = assert_ok (PI.principal_id_of_string principal) in
  let actor_key = sample_actor_key ~user () in
  let binding =
    assert_ok
      (Snap.make_account_binding_evidence ~binding_id:"ghbind_ada"
         ~lineage_id:"lineage_ada"
         ~identity:
           (assert_ok
              (Github_account_binding.make_account_identity ~app_id:42
                 ~github_user_id:9001L ()))
         ())
  in
  assert_ok
    (Snap.create ~id:snapshot_id ~now:fixed_now ~reason:"receipt_correlation"
       ~principal_id ~principal_revision ~actor_key ~actor_revision
       ~identity_link_id:"idlink_ada" ~identity_link_revision
       ~display:
         {
           display_name = Some display_name;
           avatar_url = None;
           email = None;
           extra = [];
         }
       ~account_binding:binding
       ~work_refs:
         {
           intent_id = Some "plan-1";
           confirmation_id = Some "confirm-1";
           delayed_job_id = None;
         }
       ())

let test_snapshot_retained_through_close () =
  with_db @@ fun db ->
  let snap = make_snapshot () in
  Alcotest.(check bool) "snapshot not authority" false (Snap.is_authority snap);
  let corr =
    base_correlation ~actor_mode:"user" ~requested_mode:(Some "user_required")
      ~resolved_mode:(Some "user") ~actor_snapshot:(Some snap)
      ~expected_github_login:(Some "ada") ()
  in
  Alcotest.(check bool)
    "correlation snapshot not authority" false
    (A.snapshot_is_authority corr);
  assert_ok (A.record_correlation ~db ~correlation:corr ~now:fixed_now ());
  (* Bot self-event closes the open receipt. *)
  let env =
    make_envelope ~action:(Some "closed") ~merged:(Some true)
      ~actor_type:(Some "Bot") ~actor_login:(Some "clawq-bot") ()
  in
  let r =
    A.reconcile_webhook ~db ~room_id ~envelope:env ~now:(fixed_now +. 1.) ()
  in
  Alcotest.(check string) "closed first" "closed_first" (result_tag r);
  (match r with
  | A.Closed { correlation = c; first_time = true } ->
      Alcotest.(check string)
        "resolved attribution" "user" (A.resolved_attribution c);
      Alcotest.(check (option string))
        "requested attribution" (Some "user_required")
        (A.requested_attribution c);
      Alcotest.(check (option string)) "receipt" (Some "receipt-1") c.receipt_id;
      Alcotest.(check (option string)) "plan" (Some "plan-1") c.plan_id;
      (match c.actor_snapshot with
      | None -> Alcotest.fail "expected actor_snapshot on closed correlation"
      | Some s ->
          Alcotest.(check string) "snapshot id" snap.id s.id;
          Alcotest.(check string)
            "principal frozen" "prin_ada"
            (PI.principal_id_to_string s.lineage.principal_id);
          Alcotest.(check int)
            "principal_revision frozen" 3 s.lineage.principal_revision;
          Alcotest.(check int)
            "actor_revision frozen" 2 s.lineage.actor_revision;
          Alcotest.(check int)
            "identity_link_revision frozen" 7 s.lineage.identity_link_revision;
          Alcotest.(check (option string))
            "account lineage" (Some "lineage_ada") s.lineage.account_lineage_id;
          Alcotest.(check (option string))
            "display frozen" (Some "Ada") s.display.display_name;
          Alcotest.(check bool)
            "still not authority" false (Snap.is_authority s));
      Alcotest.(check bool)
        "closed corr not authority" false
        (A.snapshot_is_authority c)
  | _ -> Alcotest.fail "expected Closed first_time");
  (* Durable load by receipt retains the same evidence. *)
  match A.get_by_receipt_id ~db ~receipt_id:"receipt-1" with
  | None -> Alcotest.fail "expected stored correlation by receipt"
  | Some loaded -> (
      Alcotest.(check string)
        "loaded resolved" "user"
        (A.resolved_attribution loaded);
      match loaded.actor_snapshot with
      | None -> Alcotest.fail "loaded snapshot missing"
      | Some s ->
          Alcotest.(check string) "loaded snap id" snap.id s.id;
          Alcotest.(check int)
            "loaded link rev" 7 s.lineage.identity_link_revision)

let test_preserve_identity_across_rename () =
  with_db @@ fun db ->
  let snap = make_snapshot ~display_name:"Ada Lovelace" () in
  let corr =
    base_correlation ~actor_mode:"user" ~actor_snapshot:(Some snap)
      ~requested_mode:(Some "user") ~resolved_mode:(Some "user") ()
  in
  assert_ok (A.record_correlation ~db ~correlation:corr ~now:fixed_now ());
  (* Later display rename does not rewrite the durable receipt snapshot. *)
  let env =
    make_envelope ~action:(Some "closed") ~merged:(Some true)
      ~actor_type:(Some "Bot") ()
  in
  let r =
    A.reconcile_webhook ~db ~room_id ~envelope:env ~now:(fixed_now +. 2.) ()
  in
  match r with
  | A.Closed { correlation = c; _ } -> (
      match c.actor_snapshot with
      | None -> Alcotest.fail "missing snapshot"
      | Some s ->
          Alcotest.(check (option string))
            "historical display retained" (Some "Ada Lovelace")
            s.display.display_name;
          Alcotest.(check string)
            "historical principal retained" "prin_ada"
            (PI.principal_id_to_string s.lineage.principal_id))
  | _ -> Alcotest.fail "expected close"

let test_unrelated_human_cannot_close_receipt () =
  with_db @@ fun db ->
  let snap = make_snapshot () in
  let corr =
    base_correlation ~action:"comment" ~actor_mode:"app"
      ~actor_snapshot:(Some snap) ~expected_github_login:(Some "ada") ()
  in
  assert_ok (A.record_correlation ~db ~correlation:corr ~now:fixed_now ());
  (* Unrelated human comments on the same item — must not close our receipt. *)
  let human =
    make_envelope ~event:"issue_comment" ~action:(Some "created")
      ~family:E.Comment ~delivery_id:(Some "deliv-human-unrelated")
      ~actor_login:(Some "bob") ~actor_type:(Some "User") ()
  in
  let r = A.reconcile_webhook ~db ~room_id ~envelope:human ~now:fixed_now () in
  Alcotest.(check string)
    "unrelated human ignored" "ignored_human_event" (result_tag r);
  Alcotest.(check int)
    "receipt still open" 1
    (count_correlations ~db ~status:"open" ());
  (* Second Principal's GitHub login must not close Ada's receipt. *)
  let other =
    make_envelope ~event:"issue_comment" ~action:(Some "created")
      ~family:E.Comment ~delivery_id:(Some "deliv-human-carol")
      ~actor_login:(Some "carol") ~actor_type:(Some "User") ()
  in
  let r2 = A.reconcile_webhook ~db ~room_id ~envelope:other ~now:fixed_now () in
  Alcotest.(check string)
    "other principal ignored" "ignored_human_event" (result_tag r2);
  Alcotest.(check int)
    "still open after other principal" 1
    (count_correlations ~db ~status:"open" ())

let test_expected_login_human_closes_once () =
  with_db @@ fun db ->
  let snap = make_snapshot () in
  let corr =
    base_correlation ~action:"comment" ~actor_mode:"user"
      ~requested_mode:(Some "user") ~resolved_mode:(Some "user")
      ~actor_snapshot:(Some snap) ~expected_github_login:(Some "ada") ()
  in
  assert_ok (A.record_correlation ~db ~correlation:corr ~now:fixed_now ());
  let native =
    make_envelope ~event:"issue_comment" ~action:(Some "created")
      ~family:E.Comment ~delivery_id:(Some "deliv-ada-comment")
      ~actor_login:(Some "Ada") (* case-insensitive match *)
      ~actor_type:(Some "User") ()
  in
  let r1 =
    A.reconcile_webhook ~db ~room_id ~envelope:native ~now:(fixed_now +. 1.) ()
  in
  Alcotest.(check string) "native user closes" "closed_first" (result_tag r1);
  (match r1 with
  | A.Closed { correlation = c; _ } -> (
      match c.actor_snapshot with
      | None -> Alcotest.fail "snapshot missing after native close"
      | Some s ->
          Alcotest.(check string)
            "principal still ada" "prin_ada"
            (PI.principal_id_to_string s.lineage.principal_id))
  | _ -> Alcotest.fail "expected close");
  let r2 =
    A.reconcile_webhook ~db ~room_id ~envelope:native ~now:(fixed_now +. 2.) ()
  in
  Alcotest.(check string) "closes once" "already_closed" (result_tag r2);
  Alcotest.(check int)
    "one closed row" 1
    (count_correlations ~db ~status:"closed" ())

let test_human_without_expected_login_cannot_claim_open () =
  with_db @@ fun db ->
  (* App/pilot correlation with no expected login: human fingerprint match
     must not close (would associate unrelated human action). *)
  let corr = base_correlation ~action:"comment" ~actor_mode:"pilot" () in
  assert_ok (A.record_correlation ~db ~correlation:corr ~now:fixed_now ());
  let human =
    make_envelope ~event:"issue_comment" ~action:(Some "created")
      ~family:E.Comment ~delivery_id:(Some "deliv-human-no-pin")
      ~actor_login:(Some "stranger") ~actor_type:(Some "User") ()
  in
  let r = A.reconcile_webhook ~db ~room_id ~envelope:human ~now:fixed_now () in
  Alcotest.(check string) "ignored" "ignored_human_event" (result_tag r);
  Alcotest.(check int) "still open" 1 (count_correlations ~db ~status:"open" ());
  (* Bot self-event still closes. *)
  let bot =
    make_envelope ~event:"issue_comment" ~action:(Some "created")
      ~family:E.Comment ~delivery_id:(Some "deliv-bot-comment")
      ~actor_type:(Some "Bot") ()
  in
  let r2 =
    A.reconcile_webhook ~db ~room_id ~envelope:bot ~now:(fixed_now +. 1.) ()
  in
  Alcotest.(check string) "bot closes" "closed_first" (result_tag r2)

let test_two_principals_isolated_receipts () =
  with_db @@ fun db ->
  let snap_ada = make_snapshot ~principal:"prin_ada" ~user:"user-ada" () in
  let snap_bob =
    make_snapshot ~principal:"prin_bob" ~user:"user-bob"
      ~snapshot_id:"actorsnap_receipt_bob" ~display_name:"Bob" ()
  in
  let corr_ada =
    base_correlation ~action:"comment" ~receipt_id:(Some "rcpt_ada")
      ~plan_id:(Some "plan_ada") ~actor_mode:"user"
      ~actor_snapshot:(Some snap_ada) ~expected_github_login:(Some "ada") ()
  in
  let corr_bob =
    base_correlation ~action:"comment" ~receipt_id:(Some "rcpt_bob")
      ~plan_id:(Some "plan_bob") ~actor_mode:"user"
      ~actor_snapshot:(Some snap_bob) ~expected_github_login:(Some "bob") ()
  in
  assert_ok (A.record_correlation ~db ~correlation:corr_ada ~now:fixed_now ());
  assert_ok
    (A.record_correlation ~db ~correlation:corr_bob ~now:(fixed_now +. 0.1) ());
  Alcotest.(check int) "two open" 2 (count_correlations ~db ~status:"open" ());
  let bob_event =
    make_envelope ~event:"issue_comment" ~action:(Some "created")
      ~family:E.Comment ~delivery_id:(Some "deliv-bob-1")
      ~actor_login:(Some "bob") ~actor_type:(Some "User") ()
  in
  let r =
    A.reconcile_webhook ~db ~room_id ~envelope:bob_event ~now:(fixed_now +. 1.)
      ()
  in
  (match r with
  | A.Closed { correlation = c; _ } -> (
      Alcotest.(check (option string))
        "bob receipt" (Some "rcpt_bob") c.receipt_id;
      match c.actor_snapshot with
      | None -> Alcotest.fail "bob snapshot missing"
      | Some s ->
          Alcotest.(check string)
            "bob principal" "prin_bob"
            (PI.principal_id_to_string s.lineage.principal_id);
          Alcotest.(check bool)
            "did not take ada's receipt" true (s.id <> snap_ada.id))
  | _ -> Alcotest.fail "expected bob's receipt closed");
  Alcotest.(check int)
    "ada still open" 1
    (count_correlations ~db ~status:"open" ());
  match A.get_by_receipt_id ~db ~receipt_id:"rcpt_ada" with
  | None -> Alcotest.fail "ada receipt missing"
  | Some ada_open -> (
      match ada_open.actor_snapshot with
      | None -> Alcotest.fail "ada snapshot missing"
      | Some s ->
          Alcotest.(check string)
            "ada principal intact" "prin_ada"
            (PI.principal_id_to_string s.lineage.principal_id))

(* ---- P21.M3.E3.T005: native receipts, all families, delayed, reorder ---- *)

type family_case = {
  action : string;
  event : string;
  env_action : string option;
  family : E.family;
  kind : E.item_kind option;
  number : int option;
  merged : bool option;
  state : string option;
}

let all_family_cases =
  [
    {
      action = "comment";
      event = "issue_comment";
      env_action = Some "created";
      family = E.Comment;
      kind = Some E.Pull_request;
      number = Some 42;
      merged = None;
      state = Some "open";
    };
    {
      action = "label";
      event = "pull_request";
      env_action = Some "labeled";
      family = E.State_update;
      kind = Some E.Pull_request;
      number = Some 42;
      merged = None;
      state = Some "open";
    };
    {
      action = "assign";
      event = "pull_request";
      env_action = Some "assigned";
      family = E.State_update;
      kind = Some E.Pull_request;
      number = Some 42;
      merged = None;
      state = Some "open";
    };
    {
      action = "request_reviewers";
      event = "pull_request";
      env_action = Some "review_requested";
      family = E.Review;
      kind = Some E.Pull_request;
      number = Some 42;
      merged = None;
      state = Some "open";
    };
    {
      action = "submit_review";
      event = "pull_request_review";
      env_action = Some "submitted";
      family = E.Review;
      kind = Some E.Pull_request;
      number = Some 42;
      merged = None;
      state = Some "open";
    };
    {
      action = "issue_create";
      event = "issues";
      env_action = Some "opened";
      family = E.Lifecycle;
      kind = Some E.Issue;
      number = Some 7;
      merged = None;
      state = Some "open";
    };
    {
      action = "issue_close";
      event = "issues";
      env_action = Some "closed";
      family = E.Lifecycle;
      kind = Some E.Issue;
      number = Some 7;
      merged = None;
      state = Some "closed";
    };
    {
      action = "issue_reopen";
      event = "issues";
      env_action = Some "reopened";
      family = E.Lifecycle;
      kind = Some E.Issue;
      number = Some 7;
      merged = None;
      state = Some "open";
    };
    {
      action = "workflow_dispatch";
      event = "workflow_run";
      env_action = Some "requested";
      family = E.Ci;
      kind = None;
      number = None;
      merged = None;
      state = None;
    };
    {
      action = "code_work";
      event = "pull_request";
      env_action = Some "opened";
      family = E.Lifecycle;
      kind = Some E.Pull_request;
      number = Some 42;
      merged = None;
      state = Some "open";
    };
    {
      action = "pr_create";
      event = "pull_request";
      env_action = Some "opened";
      family = E.Lifecycle;
      kind = Some E.Pull_request;
      number = Some 42;
      merged = None;
      state = Some "open";
    };
    {
      action = "merge";
      event = "pull_request";
      env_action = Some "closed";
      family = E.Lifecycle;
      kind = Some E.Pull_request;
      number = Some 42;
      merged = Some true;
      state = Some "closed";
    };
    {
      action = "room_background_work";
      event = "issue_comment";
      env_action = Some "created";
      family = E.Comment;
      kind = Some E.Pull_request;
      number = Some 42;
      merged = None;
      state = Some "open";
    };
  ]

let item_key_for_case (c : family_case) =
  match (c.kind, c.number) with
  | Some E.Issue, Some n -> Some (Printf.sprintf "issue:acme/widget:%d" n)
  | Some E.Pull_request, Some n -> Some (Printf.sprintf "pr:acme/widget:%d" n)
  | _ -> None

let test_every_action_family_native_user_closes_once () =
  with_db @@ fun db ->
  List.iteri
    (fun i (fc : family_case) ->
      let ik = item_key_for_case fc in
      let snap =
        make_snapshot ~snapshot_id:(Printf.sprintf "snap_fam_%d" i) ()
      in
      let attr_id = Printf.sprintf "ghattr_rcpt_%s_%d" fc.action i in
      let receipt_id = Printf.sprintf "receipt_%s_%d" fc.action i in
      let corr =
        assert_ok
          (A.record_from_native_receipt ~db ~room_id ~action:fc.action
             ~actor_mode:"user" ?item_key:ik ~plan_id:("plan_" ^ fc.action)
             ~receipt_id ~attribution_receipt_id:attr_id ~requested_mode:"user"
             ~resolved_mode:"user" ~actor_snapshot:snap
             ~expected_github_login:"ada" ~github_user_id:9001L
             ~native_actor_kind:"user"
             ~now:(fixed_now +. float_of_int i)
             ())
      in
      Alcotest.(check string)
        ("canonical action " ^ fc.action)
        (A.canonicalize_action fc.action)
        corr.action;
      Alcotest.(check (option string))
        "attr receipt linked" (Some attr_id) corr.attribution_receipt_id;
      let env =
        make_envelope ~event:fc.event ~action:fc.env_action ~family:fc.family
          ~kind:fc.kind ~number:fc.number ~merged:fc.merged ~state:fc.state
          ~delivery_id:(Some (Printf.sprintf "deliv_%s_%d" fc.action i))
          ~actor_login:(Some "Ada") ~actor_type:(Some "User")
          ~head_sha:(Some "abc123") ()
      in
      (* Inject actor id for native github_user_id match. *)
      let env =
        {
          env with
          actor = { env.actor with id = Some 9001; login = Some "Ada" };
        }
      in
      let r1 =
        A.reconcile_webhook ~db ~room_id ~envelope:env
          ~now:(fixed_now +. 100. +. float_of_int i)
          ()
      in
      Alcotest.(check string)
        ("family " ^ fc.action ^ " closes")
        "closed_first" (result_tag r1);
      (match r1 with
      | A.Closed { correlation = c; _ } ->
          Alcotest.(check (option string))
            "snapshot retained" (Some snap.id)
            (Option.map (fun s -> s.Snap.id) c.actor_snapshot);
          Alcotest.(check (option string))
            "native attr id immutable" (Some attr_id) c.attribution_receipt_id;
          Alcotest.(check (option int64))
            "github_user_id immutable" (Some 9001L) c.github_user_id;
          Alcotest.(check string)
            "resolved user" "user" (A.resolved_attribution c)
      | _ -> Alcotest.fail ("expected close for " ^ fc.action));
      let r2 =
        A.reconcile_webhook ~db ~room_id ~envelope:env
          ~now:(fixed_now +. 200. +. float_of_int i)
          ()
      in
      Alcotest.(check string)
        ("family " ^ fc.action ^ " once")
        "already_closed" (result_tag r2);
      match
        A.get_by_attribution_receipt_id ~db ~attribution_receipt_id:attr_id
      with
      | None -> Alcotest.fail "lookup by attribution receipt"
      | Some loaded ->
          Alcotest.(check (option string))
            "loaded receipt" (Some receipt_id) loaded.receipt_id)
    all_family_cases

let test_app_native_identity_closes_families () =
  with_db @@ fun db ->
  List.iteri
    (fun i action ->
      let corr =
        base_correlation ~action
          ~receipt_id:(Some (Printf.sprintf "app_rcpt_%d" i))
          ~plan_id:(Some (Printf.sprintf "app_plan_%d" i))
          ~actor_mode:"app" ~resolved_mode:(Some "app")
          ~native_actor_kind:(Some "app")
          ~attribution_receipt_id:(Some (Printf.sprintf "app_attr_%d" i))
          ()
      in
      assert_ok
        (A.record_correlation ~db ~correlation:corr
           ~now:(fixed_now +. float_of_int i)
           ());
      let env =
        match action with
        | "label" ->
            make_envelope ~action:(Some "labeled") ~family:E.State_update
              ~delivery_id:(Some (Printf.sprintf "app_deliv_%d" i))
              ~actor_type:(Some "Bot") ~actor_login:(Some "clawq[bot]") ()
        | "merge" ->
            make_envelope ~action:(Some "closed") ~merged:(Some true)
              ~delivery_id:(Some (Printf.sprintf "app_deliv_%d" i))
              ~actor_type:(Some "Bot") ()
        | "comment" ->
            make_envelope ~event:"issue_comment" ~action:(Some "created")
              ~family:E.Comment
              ~delivery_id:(Some (Printf.sprintf "app_deliv_%d" i))
              ~actor_type:(Some "Bot") ()
        | _ ->
            make_envelope ~action:(Some "opened")
              ~delivery_id:(Some (Printf.sprintf "app_deliv_%d" i))
              ~actor_type:(Some "Bot") ()
      in
      let r =
        A.reconcile_webhook ~db ~room_id ~envelope:env
          ~now:(fixed_now +. 50. +. float_of_int i)
          ()
      in
      Alcotest.(check string) ("app " ^ action) "closed_first" (result_tag r))
    [ "comment"; "label"; "merge"; "pr_create" ]

let test_delayed_completion_preserves_job_and_snapshot () =
  with_db @@ fun db ->
  let snap = make_snapshot ~snapshot_id:"snap_delayed_1" () in
  let corr =
    assert_ok
      (A.record_from_native_receipt ~db ~room_id ~action:"code_work"
         ~actor_mode:"user" ~item_key ~plan_id:"plan_delayed"
         ~receipt_id:"rcpt_delayed" ~job_id:"job_bg_42"
         ~attribution_receipt_id:"ghattr_delayed_1" ~requested_mode:"user"
         ~resolved_mode:"user" ~actor_snapshot:snap ~expected_github_login:"ada"
         ~github_user_id:9001L ~native_actor_kind:"user" ~github_ref:"deadbeef"
         ~now:fixed_now ())
  in
  Alcotest.(check (option string)) "job pin" (Some "job_bg_42") corr.job_id;
  (* Delayed completion surfaces later as PR open by the native user. *)
  let env =
    make_envelope ~action:(Some "opened") ~merged:(Some false)
      ~state:(Some "open") ~delivery_id:(Some "deliv-delayed-pr")
      ~actor_login:(Some "ada") ~actor_type:(Some "User")
      ~head_sha:(Some "deadbeef") ()
  in
  let env =
    { env with actor = { env.actor with id = Some 9001; login = Some "ada" } }
  in
  let r =
    A.reconcile_webhook ~db ~room_id ~envelope:env ~now:(fixed_now +. 3600.) ()
  in
  Alcotest.(check string) "delayed close" "closed_first" (result_tag r);
  (match r with
  | A.Closed { correlation = c; _ } ->
      Alcotest.(check (option string))
        "job retained" (Some "job_bg_42") c.job_id;
      Alcotest.(check (option string))
        "attr retained" (Some "ghattr_delayed_1") c.attribution_receipt_id;
      Alcotest.(check (option string))
        "snap retained" (Some "snap_delayed_1")
        (Option.map (fun s -> s.Snap.id) c.actor_snapshot)
  | _ -> Alcotest.fail "expected delayed close");
  match A.get_by_job_id ~db ~job_id:"job_bg_42" with
  | None -> Alcotest.fail "get_by_job_id"
  | Some loaded ->
      Alcotest.(check (option string))
        "job load receipt" (Some "rcpt_delayed") loaded.receipt_id

let test_webhook_reordering_closes_correct_family () =
  with_db @@ fun db ->
  let snap = make_snapshot () in
  (* Record label then merge (FIFO order). *)
  ignore
    (assert_ok
       (A.record_from_native_receipt ~db ~room_id ~action:"label"
          ~actor_mode:"user" ~item_key ~plan_id:"plan_label"
          ~receipt_id:"rcpt_label" ~attribution_receipt_id:"attr_label"
          ~requested_mode:"user" ~resolved_mode:"user" ~actor_snapshot:snap
          ~expected_github_login:"ada" ~github_user_id:9001L ~now:fixed_now ()));
  ignore
    (assert_ok
       (A.record_from_native_receipt ~db ~room_id ~action:"merge"
          ~actor_mode:"user" ~item_key ~plan_id:"plan_merge"
          ~receipt_id:"rcpt_merge" ~attribution_receipt_id:"attr_merge"
          ~requested_mode:"user_required" ~resolved_mode:"user"
          ~actor_snapshot:snap ~expected_github_login:"ada"
          ~github_user_id:9001L ~github_ref:"abc123" ~now:(fixed_now +. 0.1) ()));
  Alcotest.(check int) "two open" 2 (count_correlations ~db ~status:"open" ());
  (* Merge webhook arrives first (out of order). Must close merge, not label. *)
  let merge_env =
    make_envelope ~action:(Some "closed") ~merged:(Some true)
      ~delivery_id:(Some "deliv-merge-first") ~actor_login:(Some "ada")
      ~actor_type:(Some "User") ~head_sha:(Some "abc123") ()
  in
  let merge_env =
    {
      merge_env with
      actor = { merge_env.actor with id = Some 9001; login = Some "ada" };
    }
  in
  let r_merge =
    A.reconcile_webhook ~db ~room_id ~envelope:merge_env ~now:(fixed_now +. 1.)
      ()
  in
  (match r_merge with
  | A.Closed { correlation = c; _ } ->
      Alcotest.(check string) "closed merge family" "merge" c.action;
      Alcotest.(check (option string))
        "merge receipt" (Some "rcpt_merge") c.receipt_id
  | _ -> Alcotest.fail "expected merge close");
  Alcotest.(check int)
    "label still open" 1
    (count_correlations ~db ~status:"open" ());
  (* Later label webhook closes the remaining label receipt. *)
  let label_env =
    make_envelope ~action:(Some "labeled") ~family:E.State_update
      ~delivery_id:(Some "deliv-label-second") ~actor_login:(Some "ada")
      ~actor_type:(Some "User") ~merged:(Some false) ~state:(Some "open") ()
  in
  let label_env =
    {
      label_env with
      actor = { label_env.actor with id = Some 9001; login = Some "ada" };
    }
  in
  let r_label =
    A.reconcile_webhook ~db ~room_id ~envelope:label_env ~now:(fixed_now +. 2.)
      ()
  in
  (match r_label with
  | A.Closed { correlation = c; _ } ->
      Alcotest.(check string) "closed label family" "label" c.action;
      Alcotest.(check (option string))
        "label receipt" (Some "rcpt_label") c.receipt_id
  | _ -> Alcotest.fail "expected label close");
  Alcotest.(check int) "none open" 0 (count_correlations ~db ~status:"open" ());
  Alcotest.(check int)
    "two closed" 2
    (count_correlations ~db ~status:"closed" ())

let test_webhook_reordering_same_family_ref_disambiguates () =
  with_db @@ fun db ->
  ignore
    (assert_ok
       (A.record_from_native_receipt ~db ~room_id ~action:"submit_review"
          ~actor_mode:"user" ~item_key ~plan_id:"plan_rev_a"
          ~receipt_id:"rcpt_a" ~github_ref:"sha_aaa"
          ~expected_github_login:"ada" ~github_user_id:9001L ~now:fixed_now ()));
  ignore
    (assert_ok
       (A.record_from_native_receipt ~db ~room_id ~action:"submit_review"
          ~actor_mode:"user" ~item_key ~plan_id:"plan_rev_b"
          ~receipt_id:"rcpt_b" ~github_ref:"sha_bbb"
          ~expected_github_login:"ada" ~github_user_id:9001L
          ~now:(fixed_now +. 0.1) ()));
  let env_b =
    make_envelope ~event:"pull_request_review" ~action:(Some "submitted")
      ~family:E.Review ~delivery_id:(Some "deliv-rev-b")
      ~actor_login:(Some "ada") ~actor_type:(Some "User")
      ~head_sha:(Some "sha_bbb") ()
  in
  let env_b =
    {
      env_b with
      actor = { env_b.actor with id = Some 9001; login = Some "ada" };
    }
  in
  let r =
    A.reconcile_webhook ~db ~room_id ~envelope:env_b ~now:(fixed_now +. 1.) ()
  in
  match r with
  | A.Closed { correlation = c; _ } ->
      Alcotest.(check (option string))
        "matched b by ref" (Some "rcpt_b") c.receipt_id;
      Alcotest.(check (option string))
        "ref retained" (Some "sha_bbb") c.github_ref
  | _ -> Alcotest.fail "expected ref-disambiguated close"

let test_request_reviewers_does_not_steal_submit_review () =
  with_db @@ fun db ->
  ignore
    (assert_ok
       (A.record_from_native_receipt ~db ~room_id ~action:"request_reviewers"
          ~actor_mode:"user" ~item_key ~receipt_id:"rcpt_req"
          ~expected_github_login:"ada" ~github_user_id:9001L ~now:fixed_now ()));
  ignore
    (assert_ok
       (A.record_from_native_receipt ~db ~room_id ~action:"submit_review"
          ~actor_mode:"user" ~item_key ~receipt_id:"rcpt_sub"
          ~expected_github_login:"ada" ~github_user_id:9001L
          ~now:(fixed_now +. 0.1) ()));
  let submit_env =
    make_envelope ~event:"pull_request_review" ~action:(Some "submitted")
      ~family:E.Review ~delivery_id:(Some "deliv-submit")
      ~actor_login:(Some "ada") ~actor_type:(Some "User") ()
  in
  let submit_env =
    {
      submit_env with
      actor = { submit_env.actor with id = Some 9001; login = Some "ada" };
    }
  in
  let r =
    A.reconcile_webhook ~db ~room_id ~envelope:submit_env ~now:(fixed_now +. 1.)
      ()
  in
  match r with
  | A.Closed { correlation = c; _ } ->
      Alcotest.(check string) "submit family" "submit_review" c.action;
      Alcotest.(check (option string))
        "submit receipt" (Some "rcpt_sub") c.receipt_id
  | _ -> Alcotest.fail "expected submit_review close"

let test_cross_principal_github_user_id_isolated () =
  with_db @@ fun db ->
  let snap_ada = make_snapshot ~principal:"prin_ada" () in
  let snap_bob =
    make_snapshot ~principal:"prin_bob" ~user:"user-bob"
      ~snapshot_id:"snap_bob_uid" ~display_name:"Bob" ()
  in
  ignore
    (assert_ok
       (A.record_from_native_receipt ~db ~room_id ~action:"comment"
          ~actor_mode:"user" ~item_key ~receipt_id:"rcpt_ada_uid"
          ~attribution_receipt_id:"attr_ada_uid" ~actor_snapshot:snap_ada
          ~github_user_id:1001L ~expected_github_login:"ada" ~now:fixed_now ()));
  ignore
    (assert_ok
       (A.record_from_native_receipt ~db ~room_id ~action:"comment"
          ~actor_mode:"user" ~item_key ~receipt_id:"rcpt_bob_uid"
          ~attribution_receipt_id:"attr_bob_uid" ~actor_snapshot:snap_bob
          ~github_user_id:2002L ~expected_github_login:"bob"
          ~now:(fixed_now +. 0.1) ()));
  let bob_env =
    make_envelope ~event:"issue_comment" ~action:(Some "created")
      ~family:E.Comment ~delivery_id:(Some "deliv-bob-uid")
      ~actor_login:(Some "bob") ~actor_type:(Some "User") ()
  in
  let bob_env =
    {
      bob_env with
      actor = { bob_env.actor with id = Some 2002; login = Some "bob" };
    }
  in
  let r =
    A.reconcile_webhook ~db ~room_id ~envelope:bob_env ~now:(fixed_now +. 1.) ()
  in
  (match r with
  | A.Closed { correlation = c; _ } ->
      Alcotest.(check (option string))
        "bob only" (Some "rcpt_bob_uid") c.receipt_id;
      Alcotest.(check (option int64)) "bob uid" (Some 2002L) c.github_user_id
  | _ -> Alcotest.fail "expected bob close");
  Alcotest.(check int) "ada open" 1 (count_correlations ~db ~status:"open" ())

let test_canonicalize_action_aliases () =
  Alcotest.(check string)
    "collab_comment" "comment"
    (A.canonicalize_action "collab_comment");
  Alcotest.(check string)
    "review_request" "request_reviewers"
    (A.canonicalize_action "review_request");
  Alcotest.(check string)
    "review_submit" "submit_review"
    (A.canonicalize_action "review_submit");
  Alcotest.(check string)
    "code_change" "code_work"
    (A.canonicalize_action "code_change");
  Alcotest.(check string)
    "workflow" "workflow_dispatch"
    (A.canonicalize_action "workflow")

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
    ( "snapshot retained through close",
      `Quick,
      test_snapshot_retained_through_close );
    ( "preserve identity across rename",
      `Quick,
      test_preserve_identity_across_rename );
    ( "unrelated human cannot close receipt",
      `Quick,
      test_unrelated_human_cannot_close_receipt );
    ( "expected login human closes once",
      `Quick,
      test_expected_login_human_closes_once );
    ( "human without expected login cannot claim open",
      `Quick,
      test_human_without_expected_login_cannot_claim_open );
    ( "two principals isolated receipts",
      `Quick,
      test_two_principals_isolated_receipts );
    ( "every action family native user closes once",
      `Quick,
      test_every_action_family_native_user_closes_once );
    ( "app native identity closes families",
      `Quick,
      test_app_native_identity_closes_families );
    ( "delayed completion preserves job and snapshot",
      `Quick,
      test_delayed_completion_preserves_job_and_snapshot );
    ( "webhook reordering closes correct family",
      `Quick,
      test_webhook_reordering_closes_correct_family );
    ( "webhook reordering same family ref disambiguates",
      `Quick,
      test_webhook_reordering_same_family_ref_disambiguates );
    ( "request_reviewers does not steal submit_review",
      `Quick,
      test_request_reviewers_does_not_steal_submit_review );
    ( "cross principal github_user_id isolated",
      `Quick,
      test_cross_principal_github_user_id_isolated );
    ("canonicalize action aliases", `Quick, test_canonicalize_action_aliases);
  ]

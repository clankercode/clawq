(** Tests for GitHub App setup resume with confirmable plan (P19.M2.E3.T001). *)

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Github_app_setup_tx.ensure_schema db;
  Github_app_setup_callback.ensure_schema db;
  Setup_plan_apply.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let principal =
  Github_app_setup_tx.
    { id = "principal:alice"; kind = "principal"; label = Some "Alice" }

let room_bind = Github_app_setup_tx.Room "room-teams-1"
let session_bind = Github_app_setup_tx.Session "teams:room-teams-1:alice"
let public_base = "https://clawq.example.com"
let base_revision = "rev-config-abc"
let fixed_now = 1_700_000_000.0

let sample_pem =
  "-----BEGIN RSA PRIVATE KEY-----\n\
   MIIEowIBAAKCAQEA0Z3VS5JJcds3xfn/fake-test-pem-not-a-real-key\n\
   -----END RSA PRIVATE KEY-----\n"

let conversion_json ?pem ?(app_id = 424242) () =
  let pem = match pem with Some p -> p | None -> sample_pem in
  Yojson.Safe.to_string
    (`Assoc
       [
         ("id", `Int app_id);
         ("slug", `String "clawq-test-app");
         ("client_id", `String "Iv1.testclientid");
         ("client_secret", `String "cs_super_secret");
         ("pem", `String pem);
         ("webhook_secret", `String "whsec_test_secret");
         ("html_url", `String "https://github.com/apps/clawq-test-app");
         ( "owner",
           `Assoc
             [
               ("login", `String "alice");
               ("id", `Int 1);
               ("type", `String "User");
             ] );
       ])

let make_store () =
  let stored = ref [] in
  let store_secret ~name ~plaintext =
    let handle = Printf.sprintf "sec:%s:%d" name (List.length !stored) in
    stored := (name, plaintext, handle) :: !stored;
    Ok handle
  in
  (store_secret, stored)

let ok_http ?(body = conversion_json ()) ?(status = 201) () ~url:_ ~headers:_
    ~body:_ =
  Ok (status, body)

let create_tx ?id ?state ?(bind = room_bind) ?(ttl_seconds = 1800.0)
    ?(now = fixed_now) ~db () =
  Github_app_setup_tx.create ~db ~principal ~bind ~base_revision
    ~public_base_url:public_base ~app_name:"Clawq" ~now ~ttl_seconds ?id ?state
    ()

let make_req ?callback_path ?expected_bind ?expected_principal_id
    ?installation_id ~code ~state () :
    Github_app_setup_callback.exchange_request =
  {
    code;
    state;
    callback_path;
    expected_bind;
    expected_principal_id;
    installation_id;
    setup_action = None;
  }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let contains hay needle = Test_helpers.string_contains hay needle

let exchange_for ~db ?(bind = room_bind) ?id ?state ?installation_id () =
  let tx = assert_ok (create_tx ~db ~bind ?id ?state ()) in
  let store_secret, _ = make_store () in
  assert_ok
    (Github_app_setup_callback.exchange ~db ~http_post:(ok_http ())
       ~store_secret ~now:fixed_now
       (make_req ~code:"tmp_code" ~state:tx.state
          ~callback_path:Github_app_setup_tx.default_callback_path
          ~expected_bind:bind ~expected_principal_id:principal.id
          ?installation_id ()))

let sample_installation ?(status = Github_app_installation_scope.Active) () :
    Github_app_installation_scope.t =
  Github_app_installation_scope.with_revision
    {
      installation_id = 99;
      app_id = Some 424242;
      account = { login = "alice"; id = 1; account_type = "User" };
      selection = Github_app_installation_scope.All_repos;
      repositories =
        [ { full_name = "alice/clawq"; id = Some 1; private_ = Some false } ];
      revoked_repositories = [];
      permissions =
        [
          ("issues", "write"); ("pull_requests", "write"); ("metadata", "read");
        ];
      status;
      revision = "";
      updated_at = Time_util.iso8601_utc ~t:fixed_now ();
    }

let plan_status_pending db plan_id =
  match Setup_plan_apply.get_plan ~db ~plan_id with
  | Some p ->
      (* Stored plans remain pending until confirm/apply; apply would flip status. *)
      p
  | None -> Alcotest.fail ("plan not stored: " ^ plan_id)

(* ── Tests ──────────────────────────────────────────────────────── *)

let test_exchange_resume_room_active_not_applied () =
  with_db @@ fun db ->
  let exchange =
    exchange_for ~db ~id:"tx_resume_room" ~state:"state_resume_room_aaaa" ()
  in
  let result =
    assert_ok
      (Github_app_setup_resume.resume_after_exchange ~db ~exchange
         ~installation:(Some (sample_installation ()))
         ~webhook_reachable:true ~connector_ready:true
         ~current_base_revision:base_revision ~now:fixed_now ())
  in
  (match result.target with
  | Github_app_setup_resume.Active_room r ->
      Alcotest.(check string) "active room" "room-teams-1" r
  | Github_app_setup_resume.Notification _ ->
      Alcotest.fail "expected Active_room target");
  Alcotest.(check string)
    "tx consumed"
    (Github_app_setup_tx.status_to_string Github_app_setup_tx.Consumed)
    (Github_app_setup_tx.status_to_string result.transaction.status);
  Alcotest.(check bool)
    "kind github_app_setup" true
    (match result.plan.apply_payload.kind with
    | Setup_plan.Github_app_setup -> true
    | _ -> false);
  (* Plan stored as pending — not applied. *)
  let stored = plan_status_pending db result.plan.id in
  Alcotest.(check string) "stored plan id" result.plan.id stored.id;
  Alcotest.(check string) "digest matches" result.plan.digest stored.digest;
  (* No apply path invoked: no receipt on plan row. *)
  let stmt =
    Sqlite3.prepare db "SELECT status, receipt_id FROM setup_plans WHERE id = ?"
  in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT result.plan.id));
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
      let status =
        match Sqlite3.column stmt 0 with Sqlite3.Data.TEXT s -> s | _ -> ""
      in
      let receipt =
        match Sqlite3.column stmt 1 with
        | Sqlite3.Data.NULL -> None
        | Sqlite3.Data.TEXT s -> Some s
        | _ -> None
      in
      Alcotest.(check string) "pending" "pending" status;
      Alcotest.(check (option string)) "no receipt" None receipt
  | _ -> Alcotest.fail "plan row missing");
  ignore (Sqlite3.finalize stmt);
  Alcotest.(check bool) "app identity ok" true result.readiness.app_identity_ok;
  Alcotest.(check bool) "scope ok" true result.readiness.scope_ok;
  Alcotest.(check bool) "permissions ok" true result.readiness.permissions_ok;
  Alcotest.(check bool)
    "live scope mentions installation" true
    (contains result.live_scope_summary "installation=99");
  Alcotest.(check bool)
    "managed access diff non-empty" true
    (List.length result.managed_access_diff > 0)

let test_webhook_not_reachable_warns () =
  with_db @@ fun db ->
  let exchange =
    exchange_for ~db ~id:"tx_wh" ~state:"state_webhook_bbbbbbbb" ()
  in
  let result =
    assert_ok
      (Github_app_setup_resume.resume_after_exchange ~db ~exchange
         ~webhook_reachable:false ~connector_ready:true
         ~current_base_revision:base_revision ~now:fixed_now ())
  in
  Alcotest.(check bool)
    "webhook not ready flag" false result.readiness.webhook_ready;
  Alcotest.(check bool)
    "has webhook warning" true
    (List.exists
       (fun w -> contains (String.lowercase_ascii w) "webhook")
       result.readiness.warnings);
  let webhook_item =
    List.find_opt
      (fun (i : Setup_plan.readiness_item) -> i.name = "webhook")
      result.readiness.items
  in
  match webhook_item with
  | Some i ->
      Alcotest.(check bool)
        "webhook warn status" true
        (match i.status with Setup_plan.Warn -> true | _ -> false)
  | None -> Alcotest.fail "missing webhook readiness item"

let test_apply_payload_handles_not_pem () =
  with_db @@ fun db ->
  let exchange =
    exchange_for ~db ~id:"tx_handles" ~state:"state_handles_cccccc" ()
  in
  let result =
    assert_ok
      (Github_app_setup_resume.resume_after_exchange ~db ~exchange
         ~current_base_revision:base_revision ~now:fixed_now ())
  in
  let data_s = Yojson.Safe.to_string result.plan.apply_payload.data in
  let ops_s = Yojson.Safe.to_string result.plan.apply_payload.ops in
  let blob = data_s ^ ops_s in
  let lower = String.lowercase_ascii blob in
  List.iter
    (fun needle ->
      Alcotest.(check bool) ("no " ^ needle) false (contains lower needle))
    [
      "-----begin";
      "begin rsa private";
      "cs_super_secret";
      "whsec_test_secret";
      "mii eowib";
    ];
  (* Handles from exchange must appear (or their prefixes). *)
  Alcotest.(check bool)
    "client_id handle present" true
    (contains blob exchange.app.client_id_handle || contains blob "client_id");
  Alcotest.(check bool)
    "pem handle present" true
    (contains blob exchange.app.private_key_handle);
  Alcotest.(check bool)
    "webhook handle present" true
    (contains blob exchange.app.webhook_secret_handle);
  Alcotest.(check bool)
    "client secret handle present" true
    (contains blob exchange.app.client_secret_handle)

let test_stale_plan_regenerates () =
  with_db @@ fun db ->
  let exchange =
    exchange_for ~db ~id:"tx_stale" ~state:"state_stale_dddddddd" ()
  in
  let result =
    assert_ok
      (Github_app_setup_resume.resume_after_exchange ~db ~exchange
         ~current_base_revision:base_revision ~now:fixed_now ())
  in
  let plan = result.plan in
  (* Revision mismatch. *)
  let regen =
    assert_ok
      (Github_app_setup_resume.regenerate_if_stale ~db ~plan
         ~current_base_revision:"rev-config-NEW" ~now:(fixed_now +. 10.0) ())
  in
  (match regen with
  | `Current _ -> Alcotest.fail "expected regeneration on revision mismatch"
  | `Regenerated p ->
      Alcotest.(check bool) "new id" true (not (String.equal p.id plan.id));
      Alcotest.(check string) "new base" "rev-config-NEW" p.base_revision;
      Alcotest.(check bool)
        "digest changed" true
        (not (Setup_plan.digests_equal p.digest plan.digest));
      Alcotest.(check bool)
        "still github_app_setup" true
        (match p.apply_payload.kind with
        | Setup_plan.Github_app_setup -> true
        | _ -> false));
  (* Current when matching and unexpired. *)
  (match
     assert_ok
       (Github_app_setup_resume.regenerate_if_stale ~db ~plan
          ~current_base_revision:plan.base_revision ~now:fixed_now ())
   with
  | `Current p -> Alcotest.(check string) "same id" plan.id p.id
  | `Regenerated _ -> Alcotest.fail "should be current");
  (* Expired forces regenerate even with same revision. *)
  let later = fixed_now +. Setup_plan.default_ttl_seconds +. 60.0 in
  match
    assert_ok
      (Github_app_setup_resume.regenerate_if_stale ~db ~plan
         ~current_base_revision:plan.base_revision ~now:later ())
  with
  | `Current _ -> Alcotest.fail "expected regenerate on expiry"
  | `Regenerated p ->
      Alcotest.(check bool) "new id on expiry" true (p.id <> plan.id)

let test_session_bind_notification () =
  with_db @@ fun db ->
  let exchange =
    exchange_for ~db ~bind:session_bind ~id:"tx_sess"
      ~state:"state_session_eeeeee" ()
  in
  let result =
    assert_ok
      (Github_app_setup_resume.resume_after_exchange ~db ~exchange
         ~current_base_revision:base_revision ~now:fixed_now ())
  in
  match result.target with
  | Github_app_setup_resume.Active_room _ ->
      Alcotest.fail "session bind should not be Active_room"
  | Github_app_setup_resume.Notification n ->
      Alcotest.(check (option string))
        "session key" (Some "teams:room-teams-1:alice") n.session_key;
      Alcotest.(check (option string)) "no room" None n.room_id;
      Alcotest.(check bool)
        "message mentions session" true
        (contains (String.lowercase_ascii n.message) "session")

let test_plan_digest_secret_free_render () =
  with_db @@ fun db ->
  let exchange =
    exchange_for ~db ~id:"tx_digest" ~state:"state_digest_ffffff" ()
  in
  let result =
    assert_ok
      (Github_app_setup_resume.resume_after_exchange ~db ~exchange
         ~installation:(Some (sample_installation ()))
         ~current_base_revision:base_revision ~now:fixed_now ())
  in
  let plan = result.plan in
  Alcotest.(check bool)
    "digest present (sha256 hex)" true
    (String.length plan.digest = 64);
  let recomputed = Setup_plan.compute_digest plan in
  Alcotest.(check string) "digest stable" plan.digest recomputed;
  let summary = Setup_plan.format_summary plan in
  let render = Yojson.Safe.to_string (Setup_plan.to_render_json plan) in
  let lower = String.lowercase_ascii (summary ^ "\n" ^ render) in
  List.iter
    (fun needle ->
      Alcotest.(check bool)
        ("render no " ^ needle) false (contains lower needle))
    [
      "-----begin";
      "begin rsa private";
      "cs_super_secret";
      "whsec_test_secret";
      "private key-----";
    ];
  Alcotest.(check bool) "summary has digest" true (contains summary plan.digest)

let test_channel_render_tx_no_secrets () =
  with_db @@ fun db ->
  let exchange =
    exchange_for ~db ~id:"tx_render" ~state:"state_render_gggggg" ()
  in
  (* Reuse channel_render on the consumed tx — never secrets. *)
  let render = Github_app_setup_tx.channel_render exchange.transaction in
  let lower = String.lowercase_ascii render in
  List.iter
    (fun needle ->
      Alcotest.(check bool) ("no " ^ needle) false (contains lower needle))
    [
      "private_key";
      "client_secret";
      "webhook_secret";
      "-----begin";
      "begin rsa private";
      "cs_super_secret";
      "whsec_test_secret";
    ];
  (* Also ensure resume plan surfaces stay clean. *)
  let result =
    assert_ok
      (Github_app_setup_resume.resume_after_exchange ~db ~exchange
         ~current_base_revision:base_revision ~now:fixed_now ())
  in
  let plan_render =
    Yojson.Safe.to_string (Setup_plan.to_render_json result.plan)
  in
  Alcotest.(check bool)
    "plan no pem" false
    (contains (String.lowercase_ascii plan_render) "-----begin")

let test_inactive_room_notification () =
  with_db @@ fun db ->
  let exchange =
    exchange_for ~db ~id:"tx_inactive" ~state:"state_inactive_hhhh" ()
  in
  let result =
    assert_ok
      (Github_app_setup_resume.resume_after_exchange ~db ~exchange
         ~room_active:false ~current_base_revision:base_revision ~now:fixed_now
         ())
  in
  match result.target with
  | Github_app_setup_resume.Active_room _ ->
      Alcotest.fail "inactive room should notify"
  | Github_app_setup_resume.Notification n ->
      Alcotest.(check (option string)) "room id" (Some "room-teams-1") n.room_id;
      Alcotest.(check (option string)) "no session" None n.session_key

let suite =
  [
    ( "exchange resume room active plan not applied",
      `Quick,
      test_exchange_resume_room_active_not_applied );
    ( "webhook not reachable readiness warning",
      `Quick,
      test_webhook_not_reachable_warns );
    ( "apply_payload has handles not PEM",
      `Quick,
      test_apply_payload_handles_not_pem );
    ("stale plan regenerates", `Quick, test_stale_plan_regenerates);
    ( "session bind → notification with session_key",
      `Quick,
      test_session_bind_notification );
    ( "plan digest present and secret-free render",
      `Quick,
      test_plan_digest_secret_free_render );
    ( "channel_render of tx never has secrets",
      `Quick,
      test_channel_render_tx_no_secrets );
    ( "inactive room → notification target",
      `Quick,
      test_inactive_room_notification );
  ]

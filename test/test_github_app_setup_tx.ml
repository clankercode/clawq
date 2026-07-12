(** Tests for resumable GitHub App manifest setup transactions (P19.M2.E1.T001).
*)

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Github_app_setup_tx.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let principal =
  Github_app_setup_tx.
    { id = "principal:alice"; kind = "principal"; label = Some "Alice" }

let other_principal =
  Github_app_setup_tx.
    { id = "principal:bob"; kind = "principal"; label = Some "Bob" }

let room_bind = Github_app_setup_tx.Room "room-teams-1"
let session_bind = Github_app_setup_tx.Session "teams:room-teams-1:alice"
let public_base = "https://clawq.example.com"
let base_revision = "rev-config-abc"
let fixed_now = 1_700_000_000.0

let create_tx ?db ?(principal = principal) ?(bind = room_bind)
    ?(base_revision = base_revision) ?(now = fixed_now) ?id ?state ?scope
    ?(ttl_seconds = Github_app_setup_tx.default_ttl_seconds) () =
  let run db =
    Github_app_setup_tx.create ~db ~principal ~bind ~base_revision
      ~public_base_url:public_base ~app_name:"Clawq" ~now ~ttl_seconds ?id
      ?state ?scope ()
  in
  match db with Some db -> run db | None -> with_db run

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let contains hay needle = Test_helpers.string_contains hay needle

let test_create_emits_state_perms_events_url () =
  with_db @@ fun db ->
  let tx = assert_ok (create_tx ~db ()) in
  Alcotest.(check bool) "state non-empty" true (String.length tx.state > 0);
  Alcotest.(check (list (pair string string)))
    "default permissions" Github_app_setup_tx.default_permissions
    tx.scope.permissions;
  Alcotest.(check (list string))
    "default events" Github_app_setup_tx.default_events tx.scope.events;
  Alcotest.(check bool)
    "manifest url is github apps/new" true
    (contains tx.manifest_url "/settings/apps/new");
  Alcotest.(check bool)
    "url embeds state query" true
    (contains tx.manifest_url ("state=" ^ tx.state));
  Alcotest.(check bool)
    "url embeds manifest query" true
    (contains tx.manifest_url "manifest=");
  (* Exact permissions/events also in secret-free manifest JSON. *)
  let open Yojson.Safe.Util in
  let perms = tx.manifest_json |> member "default_permissions" |> to_assoc in
  List.iter
    (fun (k, level) ->
      let got =
        try List.assoc k perms |> to_string
        with Not_found -> Alcotest.fail ("missing permission " ^ k)
      in
      Alcotest.(check string) ("perm " ^ k) level got)
    Github_app_setup_tx.default_permissions;
  let events =
    tx.manifest_json |> member "default_events" |> to_list |> List.map to_string
  in
  Alcotest.(check (list string))
    "manifest events" Github_app_setup_tx.default_events events

let test_bound_to_principal_bind_revision () =
  with_db @@ fun db ->
  let tx_room =
    assert_ok
      (create_tx ~db ~bind:room_bind ~base_revision:"rev-room" ~id:"tx_room" ())
  in
  Alcotest.(check string) "principal" "principal:alice" tx_room.principal.id;
  Alcotest.(check string)
    "bind room" "room:room-teams-1"
    (Github_app_setup_tx.bind_to_string tx_room.bind);
  Alcotest.(check string) "base_revision" "rev-room" tx_room.base_revision;
  let tx_sess =
    assert_ok
      (create_tx ~db ~bind:session_bind ~base_revision:"rev-sess" ~id:"tx_sess"
         ~principal:other_principal ())
  in
  Alcotest.(check string)
    "bind session" "session:teams:room-teams-1:alice"
    (Github_app_setup_tx.bind_to_string tx_sess.bind);
  Alcotest.(check string) "principal bob" "principal:bob" tx_sess.principal.id

let test_expires_at_is_created_plus_ttl () =
  with_db @@ fun db ->
  let ttl = 900.0 in
  let tx = assert_ok (create_tx ~db ~ttl_seconds:ttl ~now:fixed_now ()) in
  let expected_created = Time_util.iso8601_utc ~t:fixed_now () in
  let expected_expires = Time_util.iso8601_utc ~t:(fixed_now +. ttl) () in
  Alcotest.(check string) "created_at" expected_created tx.created_at;
  Alcotest.(check string) "expires_at" expected_expires tx.expires_at;
  Alcotest.(check bool)
    "not expired at create" false
    (Github_app_setup_tx.is_expired ~now:fixed_now tx);
  Alcotest.(check bool)
    "expired after ttl" true
    (Github_app_setup_tx.is_expired ~now:(fixed_now +. ttl +. 1.0) tx)

let test_resume_same_id_and_state () =
  with_db @@ fun db ->
  let tx = assert_ok (create_tx ~db ~id:"tx_resume_1" ()) in
  let resumed =
    assert_ok
      (Github_app_setup_tx.resume ~db ~principal_id:principal.id ~bind:room_bind
         ~now:fixed_now ())
  in
  Alcotest.(check string) "same id" tx.id resumed.id;
  Alcotest.(check string) "same state" tx.state resumed.state;
  Alcotest.(check string) "same url" tx.manifest_url resumed.manifest_url;
  (* Explicit id path. *)
  let by_id =
    assert_ok
      (Github_app_setup_tx.resume ~db ~id:tx.id ~principal_id:principal.id
         ~bind:room_bind ~now:fixed_now ())
  in
  Alcotest.(check string) "by id" tx.id by_id.id

let test_expired_cannot_resume () =
  with_db @@ fun db ->
  let ttl = 60.0 in
  let tx = assert_ok (create_tx ~db ~ttl_seconds:ttl ~now:fixed_now ()) in
  let later = fixed_now +. ttl +. 10.0 in
  match
    Github_app_setup_tx.resume ~db ~principal_id:principal.id ~bind:room_bind
      ~now:later ()
  with
  | Error msg -> (
      Alcotest.(check bool)
        "expired message" true
        (contains (String.lowercase_ascii msg) "expired");
      match Github_app_setup_tx.get ~db ~id:tx.id with
      | Ok (Some stored) ->
          Alcotest.(check string)
            "status marked expired" "expired"
            (Github_app_setup_tx.status_to_string stored.status)
      | Ok None -> Alcotest.fail "row missing"
      | Error e -> Alcotest.fail e)
  | Ok _ -> Alcotest.fail "expected expired resume to fail"

let test_channel_render_redacts_secrets () =
  with_db @@ fun db ->
  let tx = assert_ok (create_tx ~db ()) in
  (* channel_render must never dump PEM / private_key / client_secret /
     webhook_secret. State may appear only inside the manifest URL query. *)
  let render = Github_app_setup_tx.channel_render tx in
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
      "begin private key";
    ];
  (* Standalone "state:" field is not emitted; only state= inside the URL. *)
  Alcotest.(check bool)
    "no standalone state field" false
    (contains lower "\n  state:" || contains lower "\nstate:");
  Alcotest.(check bool)
    "includes manifest url" true
    (contains render tx.manifest_url);
  Alcotest.(check bool) "includes status" true (contains lower "status:")

let test_mismatched_principal_cannot_resume () =
  with_db @@ fun db ->
  let tx = assert_ok (create_tx ~db ~principal ()) in
  (match
     Github_app_setup_tx.resume ~db ~id:tx.id ~principal_id:other_principal.id
       ~bind:room_bind ~now:fixed_now ()
   with
  | Error msg ->
      Alcotest.(check bool)
        "principal mismatch" true
        (contains (String.lowercase_ascii msg) "principal")
  | Ok _ -> Alcotest.fail "bob must not resume alice's tx");
  match
    Github_app_setup_tx.resume ~db ~principal_id:other_principal.id
      ~bind:room_bind ~now:fixed_now ()
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "bob must not find alice's open tx by bind"

let test_schema_idempotent () =
  with_db @@ fun db ->
  Github_app_setup_tx.ensure_schema db;
  Github_app_setup_tx.ensure_schema db;
  let tx = assert_ok (create_tx ~db ()) in
  match Github_app_setup_tx.get ~db ~id:tx.id with
  | Ok (Some _) -> ()
  | Ok None -> Alcotest.fail "missing after idempotent schema"
  | Error e -> Alcotest.fail e

let test_create_supersedes_previous_open () =
  with_db @@ fun db ->
  let first =
    assert_ok (create_tx ~db ~id:"tx_old" ~state:"state_old_aaaaaaaa" ())
  in
  let second =
    assert_ok (create_tx ~db ~id:"tx_new" ~state:"state_new_bbbbbbbb" ())
  in
  Alcotest.(check bool) "ids differ" true (first.id <> second.id);
  (match Github_app_setup_tx.get ~db ~id:first.id with
  | Ok (Some old) ->
      Alcotest.(check string)
        "old superseded" "superseded"
        (Github_app_setup_tx.status_to_string old.status)
  | Ok None -> Alcotest.fail "old missing"
  | Error e -> Alcotest.fail e);
  let resumed =
    assert_ok
      (Github_app_setup_tx.resume ~db ~principal_id:principal.id ~bind:room_bind
         ~now:fixed_now ())
  in
  Alcotest.(check string) "resume newest" second.id resumed.id

let test_org_manifest_url () =
  with_db @@ fun db ->
  let scope =
    Github_app_setup_tx.
      {
        org = Some "acme-corp";
        selection = All_repos;
        permissions = default_permissions;
        events = default_events;
      }
  in
  let tx = assert_ok (create_tx ~db ~scope ()) in
  Alcotest.(check bool)
    "org apps/new path" true
    (contains tx.manifest_url "/organizations/acme-corp/settings/apps/new")

let test_mark_consumed_hook () =
  with_db @@ fun db ->
  let tx = assert_ok (create_tx ~db ()) in
  let consumed =
    assert_ok
      (Github_app_setup_tx.mark_consumed ~db ~id:tx.id
         ~principal_id:principal.id ~now:fixed_now ())
  in
  Alcotest.(check string)
    "consumed" "consumed"
    (Github_app_setup_tx.status_to_string consumed.status);
  (match
     Github_app_setup_tx.resume ~db ~principal_id:principal.id ~bind:room_bind
       ~now:fixed_now ()
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "consumed tx must not resume");
  match Github_app_setup_tx.find_by_state ~db ~state:tx.state with
  | Ok (Some found) -> Alcotest.(check string) "find by state id" tx.id found.id
  | Ok None -> Alcotest.fail "find_by_state"
  | Error e -> Alcotest.fail e

let suite =
  [
    ( "create emits state perms events url",
      `Quick,
      test_create_emits_state_perms_events_url );
    ( "bound to principal room/session base_revision",
      `Quick,
      test_bound_to_principal_bind_revision );
    ("expires_at = created + TTL", `Quick, test_expires_at_is_created_plus_ttl);
    ("resume returns same id and state", `Quick, test_resume_same_id_and_state);
    ("expired cannot resume", `Quick, test_expired_cannot_resume);
    ( "channel_render has no secrets",
      `Quick,
      test_channel_render_redacts_secrets );
    ( "mismatched principal cannot resume",
      `Quick,
      test_mismatched_principal_cannot_resume );
    ("schema ensure is idempotent", `Quick, test_schema_idempotent);
    ( "create supersedes previous open",
      `Quick,
      test_create_supersedes_previous_open );
    ("org manifest url path", `Quick, test_org_manifest_url);
    ("mark_consumed and find_by_state hooks", `Quick, test_mark_consumed_hook);
  ]

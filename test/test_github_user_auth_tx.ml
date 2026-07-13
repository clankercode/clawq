(** Tests for Principal-bound GitHub user authorization transactions
    (P21.M2.E1.T002). *)

module T = Github_user_auth_tx
module P = Principal_identity

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  T.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let principal_id = "principal:alice"
let other_principal = "principal:bob"
let base_revision = "rev-policy-1"
let continuation = "cont:dm:handle-1"

let actor =
  match
    P.make_connector_actor_key ~connector:P.Teams
      ~tenant_or_workspace:"tenant-acme" ~immutable_user_id:"user-alice-1"
  with
  | Ok k -> k
  | Error e -> failwith e

let other_actor =
  match
    P.make_connector_actor_key ~connector:P.Teams
      ~tenant_or_workspace:"tenant-acme" ~immutable_user_id:"user-bob-1"
  with
  | Ok k -> k
  | Error e -> failwith e

let room = T.Room "room-teams-1"
let session = T.Session "teams:room-teams-1:alice"

let app : T.app_client =
  { host = "github.com"; app_id = 42; client_id_handle = "h:client-id" }

let context ?(principal_id = principal_id) ?(connector_actor = actor)
    ?(source = room) ?(app_id = app.app_id) ?(base_revision = base_revision) ()
    : T.bound_context =
  { principal_id; connector_actor; source; app_id; base_revision }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let contains hay needle = Test_helpers.string_contains hay needle

let create_tx ?db ?(flow_kind = T.Web_pkce) ?(principal_id = principal_id)
    ?(connector_actor = actor) ?(source = room) ?(app = app)
    ?(intended_account = T.empty_intended_account)
    ?(base_revision = base_revision) ?(continuation_handle = continuation)
    ?(ttl_seconds = T.default_ttl_seconds) ?(now = fixed_now) ?id
    ?one_time_state () =
  let run db =
    T.create ~db ~flow_kind ~principal_id ~connector_actor ~source ~app
      ~intended_account ~base_revision ~continuation_handle ~ttl_seconds ~now
      ?id ?one_time_state ()
  in
  match db with Some db -> run db | None -> with_db run

let test_create_persists_all_bound_fields () =
  with_db @@ fun db ->
  let intended : T.intended_account =
    { github_user_id = Some 12345L; login_hint = Some "alice" }
  in
  let tx =
    assert_ok
      (create_tx ~db ~id:"tx_fields" ~one_time_state:"state_aaaaaaaa"
         ~intended_account:intended ~flow_kind:T.Device ())
  in
  Alcotest.(check int) "schema version" T.schema_version tx.version;
  Alcotest.(check string) "id" "tx_fields" tx.id;
  Alcotest.(check string) "flow" "device" (T.string_of_flow_kind tx.flow_kind);
  Alcotest.(check string) "principal" principal_id tx.principal_id;
  Alcotest.(check string)
    "actor"
    (P.actor_identity_key actor)
    (T.actor_key_string tx);
  Alcotest.(check string)
    "source" "room:room-teams-1"
    (T.string_of_source tx.source);
  Alcotest.(check string) "host" "github.com" tx.app.host;
  Alcotest.(check int) "app_id" 42 tx.app.app_id;
  Alcotest.(check string) "client handle" "h:client-id" tx.app.client_id_handle;
  Alcotest.(check (option int64))
    "intended user" (Some 12345L) tx.intended_account.github_user_id;
  Alcotest.(check (option string))
    "login hint" (Some "alice") tx.intended_account.login_hint;
  Alcotest.(check string) "one_time_state" "state_aaaaaaaa" tx.one_time_state;
  Alcotest.(check string) "base_revision" base_revision tx.base_revision;
  Alcotest.(check string) "continuation" continuation tx.continuation_handle;
  Alcotest.(check string) "status open" "open" (T.string_of_status tx.status);
  match T.get ~db ~id:tx.id with
  | Ok (Some stored) ->
      Alcotest.(check string) "persisted id" tx.id stored.id;
      Alcotest.(check string)
        "persisted state" tx.one_time_state stored.one_time_state
  | Ok None -> Alcotest.fail "missing row"
  | Error e -> Alcotest.fail e

let test_resume_restart_safe () =
  with_db @@ fun db ->
  let tx =
    assert_ok
      (create_tx ~db ~id:"tx_resume" ~one_time_state:"state_resume_1" ())
  in
  let ctx = context () in
  let resumed =
    assert_ok
      (T.resume ~db ~context:ctx ~flow_kind:T.Web_pkce ~now:fixed_now ())
  in
  Alcotest.(check string) "same id" tx.id resumed.id;
  Alcotest.(check string) "same state" tx.one_time_state resumed.one_time_state;
  Alcotest.(check string)
    "same continuation" tx.continuation_handle resumed.continuation_handle;
  let by_id =
    assert_ok
      (T.resume ~db ~id:tx.id ~context:ctx ~flow_kind:T.Web_pkce ~now:fixed_now
         ())
  in
  Alcotest.(check string) "by id" tx.id by_id.id

let test_complete_happy_path () =
  with_db @@ fun db ->
  let tx =
    assert_ok
      (create_tx ~db ~id:"tx_complete" ~one_time_state:"state_complete_1" ())
  in
  let ctx = context () in
  let done_ =
    assert_ok
      (T.complete ~db ~id:tx.id ~context:ctx ~one_time_state:tx.one_time_state
         ~now:fixed_now ())
  in
  Alcotest.(check string)
    "completed" "completed"
    (T.string_of_status done_.status);
  Alcotest.(check bool) "terminal" true (T.status_is_terminal done_.status);
  (* Resume after complete fails; never reopens. *)
  match
    T.resume ~db ~id:tx.id ~context:ctx ~flow_kind:T.Web_pkce ~now:fixed_now ()
  with
  | Error msg ->
      Alcotest.(check bool)
        "terminal message" true
        (contains (String.lowercase_ascii msg) "terminal")
  | Ok _ -> Alcotest.fail "completed must not resume"

let test_cancel_is_terminal () =
  with_db @@ fun db ->
  let tx = assert_ok (create_tx ~db ~id:"tx_cancel" ()) in
  let ctx = context () in
  let cancelled =
    assert_ok
      (T.cancel ~db ~id:tx.id ~context:ctx ~reason:"user aborted" ~now:fixed_now
         ())
  in
  Alcotest.(check string)
    "cancelled" "cancelled"
    (T.string_of_status cancelled.status);
  (match
     T.complete ~db ~id:tx.id ~context:ctx ~one_time_state:tx.one_time_state
       ~now:fixed_now ()
   with
  | Error msg ->
      Alcotest.(check bool)
        "cannot complete cancelled" true
        (contains (String.lowercase_ascii msg) "terminal")
  | Ok _ -> Alcotest.fail "cancelled must not complete");
  match T.cancel ~db ~id:tx.id ~context:ctx ~now:fixed_now () with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "double cancel must fail"

let test_expire_is_terminal () =
  with_db @@ fun db ->
  let ttl = 60.0 in
  let tx = assert_ok (create_tx ~db ~id:"tx_expire" ~ttl_seconds:ttl ()) in
  let later = fixed_now +. ttl +. 5.0 in
  let expired = assert_ok (T.expire ~db ~id:tx.id ~now:later ()) in
  Alcotest.(check string)
    "expired" "expired"
    (T.string_of_status expired.status);
  match
    T.resume ~db ~id:tx.id ~context:(context ()) ~flow_kind:T.Web_pkce
      ~now:later ()
  with
  | Error msg ->
      Alcotest.(check bool)
        "terminal or expired" true
        (contains (String.lowercase_ascii msg) "terminal"
        || contains (String.lowercase_ascii msg) "expired")
  | Ok _ -> Alcotest.fail "expired must not resume"

let test_resume_auto_expires () =
  with_db @@ fun db ->
  let ttl = 30.0 in
  let tx = assert_ok (create_tx ~db ~ttl_seconds:ttl ()) in
  let later = fixed_now +. ttl +. 1.0 in
  (match
     T.resume ~db ~context:(context ()) ~flow_kind:T.Web_pkce ~now:later ()
   with
  | Error msg ->
      Alcotest.(check bool)
        "auto-expire message" true
        (contains (String.lowercase_ascii msg) "terminal"
        || contains (String.lowercase_ascii msg) "expired")
  | Ok _ -> Alcotest.fail "expected auto-expire on resume");
  match T.get ~db ~id:tx.id with
  | Ok (Some stored) ->
      Alcotest.(check string)
        "status expired" "expired"
        (T.string_of_status stored.status)
  | Ok None -> Alcotest.fail "missing"
  | Error e -> Alcotest.fail e

let test_replay_complete_is_terminal () =
  with_db @@ fun db ->
  let tx =
    assert_ok
      (create_tx ~db ~id:"tx_replay" ~one_time_state:"state_replay_1" ())
  in
  let ctx = context () in
  ignore
    (assert_ok
       (T.complete ~db ~id:tx.id ~context:ctx ~one_time_state:tx.one_time_state
          ~now:fixed_now ()));
  match
    T.complete ~db ~id:tx.id ~context:ctx ~one_time_state:tx.one_time_state
      ~now:fixed_now ()
  with
  | Error msg -> (
      Alcotest.(check bool)
        "replay message" true
        (contains (String.lowercase_ascii msg) "replay"
        || contains (String.lowercase_ascii msg) "completed");
      match T.get ~db ~id:tx.id with
      | Ok (Some stored) ->
          Alcotest.(check string)
            "stays completed" "completed"
            (T.string_of_status stored.status)
      | Ok None -> Alcotest.fail "missing"
      | Error e -> Alcotest.fail e)
  | Ok _ -> Alcotest.fail "replay must fail"

let test_swapped_context_resume () =
  with_db @@ fun db ->
  let tx = assert_ok (create_tx ~db ~id:"tx_swap_resume" ()) in
  let wrong_principal = context ~principal_id:other_principal () in
  (match
     T.resume ~db ~id:tx.id ~context:wrong_principal ~flow_kind:T.Web_pkce
       ~now:fixed_now ()
   with
  | Error msg ->
      Alcotest.(check bool)
        "swapped principal" true
        (contains (String.lowercase_ascii msg) "swapped"
        || contains (String.lowercase_ascii msg) "mismatch")
  | Ok _ -> Alcotest.fail "wrong principal must not resume");
  let wrong_source = context ~source:session () in
  (match
     T.resume ~db ~id:tx.id ~context:wrong_source ~flow_kind:T.Web_pkce
       ~now:fixed_now ()
   with
  | Error msg ->
      Alcotest.(check bool)
        "swapped source" true
        (contains (String.lowercase_ascii msg) "swapped"
        || contains (String.lowercase_ascii msg) "mismatch")
  | Ok _ -> Alcotest.fail "wrong source must not resume");
  let wrong_actor = context ~connector_actor:other_actor () in
  (match
     T.resume ~db ~id:tx.id ~context:wrong_actor ~flow_kind:T.Web_pkce
       ~now:fixed_now ()
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "wrong actor must not resume");
  let wrong_rev = context ~base_revision:"rev-other" () in
  match
    T.resume ~db ~id:tx.id ~context:wrong_rev ~flow_kind:T.Web_pkce
      ~now:fixed_now ()
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "wrong base_revision must not resume"

let test_swapped_context_complete_rejects () =
  with_db @@ fun db ->
  let tx =
    assert_ok
      (create_tx ~db ~id:"tx_swap_complete" ~one_time_state:"state_swap_1" ())
  in
  let wrong = context ~principal_id:other_principal () in
  (match
     T.complete ~db ~id:tx.id ~context:wrong ~one_time_state:tx.one_time_state
       ~now:fixed_now ()
   with
  | Error msg ->
      Alcotest.(check bool)
        "swapped complete" true
        (contains (String.lowercase_ascii msg) "swapped")
  | Ok _ -> Alcotest.fail "swapped context complete must fail");
  match T.get ~db ~id:tx.id with
  | Ok (Some stored) ->
      Alcotest.(check string)
        "rejected terminal" "rejected"
        (T.string_of_status stored.status);
      Alcotest.(check bool)
        "never reopen" true
        (T.status_is_terminal stored.status)
  | Ok None -> Alcotest.fail "missing"
  | Error e -> Alcotest.fail e

let test_competing_completion () =
  with_db @@ fun db ->
  let tx =
    assert_ok (create_tx ~db ~id:"tx_race" ~one_time_state:"state_race_1" ())
  in
  let ctx = context () in
  let first =
    assert_ok
      (T.complete ~db ~id:tx.id ~context:ctx ~one_time_state:tx.one_time_state
         ~now:fixed_now ())
  in
  Alcotest.(check string)
    "first wins" "completed"
    (T.string_of_status first.status);
  match
    T.complete ~db ~id:tx.id ~context:ctx ~one_time_state:tx.one_time_state
      ~now:fixed_now ()
  with
  | Error msg ->
      Alcotest.(check bool)
        "competing or replay" true
        (contains (String.lowercase_ascii msg) "compet"
        || contains (String.lowercase_ascii msg) "replay"
        || contains (String.lowercase_ascii msg) "completed"
        || contains (String.lowercase_ascii msg) "terminal")
  | Ok _ -> Alcotest.fail "second complete must fail"

let test_create_supersedes_previous_open () =
  with_db @@ fun db ->
  let first =
    assert_ok
      (create_tx ~db ~id:"tx_old" ~one_time_state:"state_old_aaaaaaaa" ())
  in
  let second =
    assert_ok
      (create_tx ~db ~id:"tx_new" ~one_time_state:"state_new_bbbbbbbb" ())
  in
  Alcotest.(check bool) "ids differ" true (first.id <> second.id);
  (match T.get ~db ~id:first.id with
  | Ok (Some old) ->
      Alcotest.(check string)
        "old superseded" "superseded"
        (T.string_of_status old.status)
  | Ok None -> Alcotest.fail "old missing"
  | Error e -> Alcotest.fail e);
  let resumed =
    assert_ok
      (T.resume ~db ~context:(context ()) ~flow_kind:T.Web_pkce ~now:fixed_now
         ())
  in
  Alcotest.(check string) "resume newest" second.id resumed.id

let test_schema_idempotent () =
  with_db @@ fun db ->
  T.ensure_schema db;
  T.ensure_schema db;
  let tx = assert_ok (create_tx ~db ()) in
  match T.get ~db ~id:tx.id with
  | Ok (Some _) -> ()
  | Ok None -> Alcotest.fail "missing after idempotent schema"
  | Error e -> Alcotest.fail e

let test_wrong_one_time_state_does_not_mutate () =
  with_db @@ fun db ->
  let tx =
    assert_ok (create_tx ~db ~id:"tx_bad_state" ~one_time_state:"state_good" ())
  in
  (match
     T.complete ~db ~id:tx.id ~context:(context ())
       ~one_time_state:"state_wrong" ~now:fixed_now ()
   with
  | Error msg ->
      Alcotest.(check bool)
        "mismatch" true
        (contains (String.lowercase_ascii msg) "mismatch"
        || contains (String.lowercase_ascii msg) "one_time")
  | Ok _ -> Alcotest.fail "wrong state must fail");
  match T.get ~db ~id:tx.id with
  | Ok (Some stored) ->
      Alcotest.(check string)
        "still open" "open"
        (T.string_of_status stored.status)
  | Ok None -> Alcotest.fail "missing"
  | Error e -> Alcotest.fail e

let test_redacted_summary_has_no_secrets () =
  with_db @@ fun db ->
  let tx = assert_ok (create_tx ~db ~one_time_state:"secretish_state" ()) in
  let s = T.redacted_summary tx in
  let lower = String.lowercase_ascii s in
  Alcotest.(check bool)
    "no one_time_state value" false
    (contains s tx.one_time_state);
  List.iter
    (fun needle ->
      Alcotest.(check bool) ("no " ^ needle) false (contains lower needle))
    [ "client_secret"; "access_token"; "refresh_token"; "code_verifier" ];
  Alcotest.(check bool) "has status" true (contains lower "status:")

let test_redacted_summary_omits_cancellation_reason () =
  with_db @@ fun db ->
  let tx = assert_ok (create_tx ~db ~id:"tx_secret_reason" ()) in
  let secret_reason =
    "provider error_description: access_token=gho_secret_cancellation_reason"
  in
  let cancelled =
    assert_ok
      (T.cancel ~db ~id:tx.id ~context:(context ()) ~reason:secret_reason
         ~now:fixed_now ())
  in
  let summary = T.redacted_summary cancelled in
  Alcotest.(check bool)
    "omits complete cancellation reason" false
    (contains summary secret_reason);
  Alcotest.(check bool)
    "omits embedded secret" false
    (contains summary "gho_secret_cancellation_reason");
  Alcotest.(check bool)
    "reports terminal reason presence" true
    (contains summary "terminal_reason: present")

let test_find_by_one_time_state () =
  with_db @@ fun db ->
  let tx =
    assert_ok (create_tx ~db ~id:"tx_find" ~one_time_state:"state_find_me" ())
  in
  match T.find_by_one_time_state ~db ~one_time_state:"state_find_me" with
  | Ok (Some found) -> Alcotest.(check string) "found" tx.id found.id
  | Ok None -> Alcotest.fail "not found"
  | Error e -> Alcotest.fail e

let suite =
  [
    ( "create persists flow principal actor source app account expiry state \
       revision continuation",
      `Quick,
      test_create_persists_all_bound_fields );
    ("resume is restart-safe", `Quick, test_resume_restart_safe);
    ("complete happy path then terminal", `Quick, test_complete_happy_path);
    ("cancel is terminal", `Quick, test_cancel_is_terminal);
    ("expire is terminal", `Quick, test_expire_is_terminal);
    ("resume auto-expires open past TTL", `Quick, test_resume_auto_expires);
    ("replay complete is terminal", `Quick, test_replay_complete_is_terminal);
    ("swapped context cannot resume", `Quick, test_swapped_context_resume);
    ( "swapped context complete rejects and terminates",
      `Quick,
      test_swapped_context_complete_rejects );
    ("competing completion fails closed", `Quick, test_competing_completion);
    ( "create supersedes previous open",
      `Quick,
      test_create_supersedes_previous_open );
    ("schema ensure is idempotent", `Quick, test_schema_idempotent);
    ( "wrong one_time_state leaves open",
      `Quick,
      test_wrong_one_time_state_does_not_mutate );
    ( "redacted summary has no secrets",
      `Quick,
      test_redacted_summary_has_no_secrets );
    ( "redacted summary omits cancellation reason",
      `Quick,
      test_redacted_summary_omits_cancellation_reason );
    ("find_by_one_time_state", `Quick, test_find_by_one_time_state);
  ]

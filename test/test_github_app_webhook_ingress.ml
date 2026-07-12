(** Tests for verified shared GitHub App webhook ingress (P19.M2.E1.T004). *)

module I = Github_app_webhook_ingress
module S = Github_app_installation_scope

let secret = "test-webhook-secret-xyz"
let fixed_now = 1_700_000_000.0

let sign body =
  "sha256=" ^ Digestif.SHA256.(hmac_string ~key:secret body |> to_hex)

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  I.ensure_schema db;
  S.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let account = S.{ login = "acme-corp"; id = 99; account_type = "Organization" }
let perms = [ ("issues", "write"); ("metadata", "read") ]

let seed_installation ~db ?(installation_id = 1001) ?(status = S.Active)
    ?(selection = S.All_repos) ?(repos = []) () =
  let scope =
    S.with_revision
      {
        installation_id;
        app_id = Some 42;
        account;
        selection;
        repositories = repos;
        revoked_repositories = [];
        permissions = perms;
        status;
        revision = "";
        updated_at = Time_util.iso8601_utc ~t:fixed_now ();
      }
  in
  match S.upsert ~db scope with Ok t -> t | Error e -> Alcotest.fail e

let make_headers ?delivery_id ?(event = "pull_request") ?signature body =
  {
    I.delivery_id;
    event = Some event;
    signature_header =
      (match signature with Some s -> Some s | None -> Some (sign body));
    user_agent = Some "GitHub-Hookshot/test";
  }

let make_request ?path ?delivery_id ?event ?signature ~body () =
  let path = Option.value path ~default:I.default_path in
  { I.body; headers = make_headers ?delivery_id ?event ?signature body; path }

let pr_payload ?(installation_id = 1001) ?(app_id = 42)
    ?(repo = "acme-corp/alpha") ?(action = "opened") () =
  Printf.sprintf
    {|{"action":%S,"installation":{"id":%d,"app_id":%d},"repository":{"full_name":%S,"id":1}}|}
    action installation_id app_id repo

let installation_payload ?(installation_id = 1001) ?(app_id = 42)
    ?(action = "created") () =
  Printf.sprintf
    {|{"action":%S,"installation":{"id":%d,"app_id":%d,"account":{"login":"acme-corp","id":99,"type":"Organization"}}}|}
    action installation_id app_id

let accept_ok = function
  | I.Accepted a -> a
  | I.Rejected { reason; message } ->
      Alcotest.failf "expected Accepted, got Rejected %s: %s"
        (I.reject_reason_to_string reason)
        message
  | I.Duplicate { delivery_id } ->
      Alcotest.failf "expected Accepted, got Duplicate %s" delivery_id

let reject_reason_of = function
  | I.Rejected { reason; _ } -> reason
  | I.Accepted _ -> Alcotest.fail "expected Rejected, got Accepted"
  | I.Duplicate _ -> Alcotest.fail "expected Rejected, got Duplicate"

(* 1. good signature + delivery + active installation + in-scope repo *)
let test_accept_happy_path () =
  with_db @@ fun db ->
  ignore (seed_installation ~db ());
  let body = pr_payload () in
  let delivery_id = "deliv-happy-001" in
  let req = make_request ~delivery_id ~body () in
  let a =
    accept_ok
      (I.verify_and_accept ~db ~webhook_secret:secret ~now:fixed_now
         ~expected_app_id:42 req)
  in
  Alcotest.(check string) "delivery_id" delivery_id a.delivery_id;
  Alcotest.(check string) "event" "pull_request" a.event;
  Alcotest.(check (option int)) "installation" (Some 1001) a.installation_id;
  Alcotest.(check (option int)) "app_id" (Some 42) a.app_id;
  Alcotest.(check (option string))
    "repo" (Some "acme-corp/alpha") a.repo_full_name;
  Alcotest.(check (option string)) "action" (Some "opened") a.action;
  Alcotest.(check bool) "ledger" true (I.was_seen ~db ~delivery_id)

(* 2. bad signature → Rejected Bad_signature; not in ledger *)
let test_bad_signature () =
  with_db @@ fun db ->
  ignore (seed_installation ~db ());
  let body = pr_payload () in
  let delivery_id = "deliv-bad-sig" in
  let req = make_request ~delivery_id ~signature:"sha256=deadbeef" ~body () in
  let reason =
    reject_reason_of
      (I.verify_and_accept ~db ~webhook_secret:secret ~now:fixed_now req)
  in
  Alcotest.(check bool) "bad_signature" true (reason = I.Bad_signature);
  Alcotest.(check bool) "not ledgered" false (I.was_seen ~db ~delivery_id)

(* 3. missing delivery_id → reject *)
let test_missing_delivery_id () =
  with_db @@ fun db ->
  ignore (seed_installation ~db ());
  let body = pr_payload () in
  let req = make_request ~body () in
  (* delivery_id defaults to None *)
  let reason =
    reject_reason_of
      (I.verify_and_accept ~db ~webhook_secret:secret ~now:fixed_now req)
  in
  Alcotest.(check bool)
    "missing_delivery_id" true
    (reason = I.Missing_delivery_id)

(* 4. replay same delivery_id → Duplicate *)
let test_replay_duplicate () =
  with_db @@ fun db ->
  ignore (seed_installation ~db ());
  let body = pr_payload () in
  let delivery_id = "deliv-replay-1" in
  let req = make_request ~delivery_id ~body () in
  ignore
    (accept_ok
       (I.verify_and_accept ~db ~webhook_secret:secret ~now:fixed_now req));
  match I.verify_and_accept ~db ~webhook_secret:secret ~now:fixed_now req with
  | I.Duplicate { delivery_id = d } ->
      Alcotest.(check string) "same id" delivery_id d
  | other ->
      Alcotest.failf "expected Duplicate, got %s"
        (match other with
        | I.Accepted _ -> "Accepted"
        | I.Rejected { message; _ } -> "Rejected: " ^ message
        | I.Duplicate _ -> "Duplicate")

(* 5. durable: reopen db / new connection still Duplicate *)
let test_durable_across_reopen () =
  let path = Filename.temp_file "clawq_gh_wh_" ".db" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () ->
      let body = pr_payload () in
      let delivery_id = "deliv-durable-1" in
      let first = Sqlite3.db_open path in
      I.ensure_schema first;
      S.ensure_schema first;
      ignore (seed_installation ~db:first ());
      let req = make_request ~delivery_id ~body () in
      ignore
        (accept_ok
           (I.verify_and_accept ~db:first ~webhook_secret:secret ~now:fixed_now
              req));
      ignore (Sqlite3.db_close first);
      let second = Sqlite3.db_open path in
      I.ensure_schema second;
      S.ensure_schema second;
      (match
         I.verify_and_accept ~db:second ~webhook_secret:secret ~now:fixed_now
           req
       with
      | I.Duplicate { delivery_id = d } ->
          Alcotest.(check string) "durable dup" delivery_id d
      | I.Accepted _ -> Alcotest.fail "should be Duplicate after reopen"
      | I.Rejected { message; _ } ->
          Alcotest.failf "unexpected reject: %s" message);
      ignore (Sqlite3.db_close second))

(* 6. suspended installation → reject *)
let test_suspended_installation () =
  with_db @@ fun db ->
  ignore
    (seed_installation ~db ~status:(S.Suspended { reason = Some "billing" }) ());
  let body = pr_payload () in
  let delivery_id = "deliv-suspend-1" in
  let req = make_request ~delivery_id ~body () in
  let reason =
    reject_reason_of
      (I.verify_and_accept ~db ~webhook_secret:secret ~now:fixed_now req)
  in
  Alcotest.(check bool)
    "unknown_or_suspended" true
    (reason = I.Unknown_or_suspended_installation);
  Alcotest.(check bool) "not ledgered" false (I.was_seen ~db ~delivery_id)

(* 7. repo not authorized → reject *)
let test_repo_not_authorized () =
  with_db @@ fun db ->
  ignore
    (seed_installation ~db ~selection:S.Selected_repos
       ~repos:
         [
           { full_name = "acme-corp/alpha"; id = Some 1; private_ = Some false };
         ]
       ());
  let body = pr_payload ~repo:"acme-corp/gamma" () in
  let delivery_id = "deliv-repo-scope" in
  let req = make_request ~delivery_id ~body () in
  let reason =
    reject_reason_of
      (I.verify_and_accept ~db ~webhook_secret:secret ~now:fixed_now req)
  in
  Alcotest.(check bool) "repo_not_in_scope" true (reason = I.Repo_not_in_scope);
  Alcotest.(check bool) "not ledgered" false (I.was_seen ~db ~delivery_id)

(* 8. installation event without repo → Accepted when installation active or for create *)
let test_installation_event_no_repo () =
  with_db @@ fun db ->
  (* create event: no prior installation row required *)
  let body = installation_payload ~action:"created" () in
  let delivery_id = "deliv-inst-create" in
  let req = make_request ~delivery_id ~event:"installation" ~body () in
  let a =
    accept_ok
      (I.verify_and_accept ~db ~webhook_secret:secret ~now:fixed_now req)
  in
  Alcotest.(check string) "event" "installation" a.event;
  Alcotest.(check (option string)) "no repo" None a.repo_full_name;
  Alcotest.(check (option int)) "install id" (Some 1001) a.installation_id;
  (* with active installation also accepted *)
  ignore (seed_installation ~db ());
  let body2 = installation_payload ~action:"suspend" () in
  let delivery_id2 = "deliv-inst-suspend" in
  let req2 =
    make_request ~delivery_id:delivery_id2 ~event:"installation" ~body:body2 ()
  in
  ignore
    (accept_ok
       (I.verify_and_accept ~db ~webhook_secret:secret ~now:fixed_now req2));
  Alcotest.(check bool) "ledgered create" true (I.was_seen ~db ~delivery_id);
  Alcotest.(check bool)
    "ledgered suspend" true
    (I.was_seen ~db ~delivery_id:delivery_id2)

(* 9. event not in allowed list → reject *)
let test_event_not_subscribed () =
  with_db @@ fun db ->
  ignore (seed_installation ~db ());
  let body = pr_payload () in
  let delivery_id = "deliv-unsub" in
  let req = make_request ~delivery_id ~event:"star" ~body () in
  let reason =
    reject_reason_of
      (I.verify_and_accept ~db ~webhook_secret:secret ~now:fixed_now
         ~allowed_events:[ "pull_request"; "issues" ]
         req)
  in
  Alcotest.(check bool)
    "event_not_subscribed" true
    (reason = I.Event_not_subscribed);
  Alcotest.(check bool) "not ledgered" false (I.was_seen ~db ~delivery_id)

(* 10. wrong path → reject *)
let test_wrong_path () =
  with_db @@ fun db ->
  ignore (seed_installation ~db ());
  let body = pr_payload () in
  let delivery_id = "deliv-path" in
  let req = make_request ~path:"/github/wrong" ~delivery_id ~body () in
  let reason =
    reject_reason_of
      (I.verify_and_accept ~db ~webhook_secret:secret ~now:fixed_now req)
  in
  Alcotest.(check bool) "wrong_path" true (reason = I.Wrong_path);
  Alcotest.(check bool) "not ledgered" false (I.was_seen ~db ~delivery_id)

(* 11. ping event allowed *)
let test_ping_allowed () =
  with_db @@ fun db ->
  let body = {|{"zen":"Design for failure.","hook_id":1}|} in
  let delivery_id = "deliv-ping" in
  let req = make_request ~delivery_id ~event:"ping" ~body () in
  let a =
    accept_ok
      (I.verify_and_accept ~db ~webhook_secret:secret ~now:fixed_now
         ~allowed_events:[] req)
  in
  Alcotest.(check string) "ping" "ping" a.event;
  Alcotest.(check bool) "ledgered" true (I.was_seen ~db ~delivery_id)

(* 12. ensure_schema idempotent *)
let test_ensure_schema_idempotent () =
  with_db @@ fun db ->
  I.ensure_schema db;
  I.ensure_schema db;
  ignore (seed_installation ~db ());
  let body = pr_payload () in
  let req = make_request ~delivery_id:"deliv-schema" ~body () in
  ignore
    (accept_ok
       (I.verify_and_accept ~db ~webhook_secret:secret ~now:fixed_now req));
  Alcotest.(check bool) "seen" true (I.was_seen ~db ~delivery_id:"deliv-schema");
  match I.record_ack ~db ~delivery_id:"deliv-schema" with
  | Ok () -> ()
  | Error e -> Alcotest.fail e

let suite =
  [
    ("accept happy path + ledger", `Quick, test_accept_happy_path);
    ("bad signature rejected not ledgered", `Quick, test_bad_signature);
    ("missing delivery_id rejected", `Quick, test_missing_delivery_id);
    ("replay same delivery_id Duplicate", `Quick, test_replay_duplicate);
    ("durable reopen still Duplicate", `Quick, test_durable_across_reopen);
    ("suspended installation rejected", `Quick, test_suspended_installation);
    ("repo not authorized rejected", `Quick, test_repo_not_authorized);
    ( "installation event without repo accepted",
      `Quick,
      test_installation_event_no_repo );
    ("event not subscribed rejected", `Quick, test_event_not_subscribed);
    ("wrong path rejected", `Quick, test_wrong_path);
    ("ping always allowed", `Quick, test_ping_allowed);
    ("ensure_schema idempotent", `Quick, test_ensure_schema_idempotent);
  ]

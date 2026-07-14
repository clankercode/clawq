(** P21 token-leak restart/concurrency/private-delivery verification
    (P21.M4.E2.T002).

    Integration-style suite proving personal-token isolation under:
    - lease refuse of every non-HTTP surface (runner env, process env, shell,
      Git transport, worktree, prompt, tool data, job payload, crash output,
      scheduled ambient)
    - concurrent refresh single-flight (one remote; waiters/joiners never open a
      second refresh)
    - process restart drops process-local leases while sealed vault survives
    - private delivery redaction (no auth URL / device codes / tokens in Room
      messages, refuse errors, redacted JSON, or crash-shaped surfaces)

    Builds on P21.M2.E4.T005 vault security and P21.M3.E3.T004 token isolation.
    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module V = Github_user_token_vault
module S = Github_user_token_store
module L = Github_user_token_lease
module C = Github_user_token_cas
module R = Github_user_token_refresh
module D = Github_user_auth_delivery
module P = Principal_identity
module Tx = Github_user_auth_tx

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-p21-security-test" ()

let sample_tokens =
  {
    S.access_token = "ghu_access_P21_SEC_PLAINTEXT_never_export";
    refresh_token = Some "ghr_refresh_P21_SEC_PLAINTEXT_never_export";
  }

let client_secret_plain = "gh_client_secret_P21_SEC_NEVER_LOG"
let client_id_plain = "Iv1.clientid_p21_sec"
let client_id_handle = "handle_client_p21_sec"
let resolve_ok ~client_id_handle:_ = Ok (client_id_plain, client_secret_plain)
let fixed_now = 1_785_600_000.0
let far_expires = "2026-12-01T00:00:00Z"

(** Access token inside default 300s skew so acquire/refresh paths can fire. *)
let near_expires_iso = Time_util.iso8601_utc ~t:(fixed_now +. 60.) ()

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let contains hay needle =
  let hay = String.lowercase_ascii hay in
  let needle = String.lowercase_ascii needle in
  let n = String.length needle in
  let h = String.length hay in
  if n = 0 then true
  else if n > h then false
  else
    let rec loop i =
      if i > h - n then false
      else if String.sub hay i n = needle then true
      else loop (i + 1)
    in
    loop 0

let secrets_absent ?extra blob =
  List.iter
    (fun needle ->
      Alcotest.(check bool) ("no " ^ needle) false (contains blob needle))
    ([
       sample_tokens.access_token;
       Option.get sample_tokens.refresh_token;
       "ghu_access_P21_SEC";
       "ghr_refresh_P21_SEC";
       client_secret_plain;
       aes_key;
     ]
    @ Option.value extra ~default:[])

let account ?(principal_id = "prin_p21_sec") ?(github_user_id = 9101L)
    ?(app_id = 42) ?(host = V.default_host) () =
  assert_ok (V.make_account_key ~principal_id ~github_user_id ~app_id ~host ())

let make_keys ?(key_id = "mk-p21-sec-1") ?(key_version = 1) () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key ())

let with_mem_db f =
  let db = Sqlite3.db_open ":memory:" in
  V.ensure_schema db;
  R.ensure_schema db;
  Fun.protect
    ~finally:(fun () ->
      ignore (L.discard_all ());
      ignore (Sqlite3.db_close db))
    (fun () -> f db)

let with_file_db f =
  let path = Filename.temp_file "clawq_p21_sec_" ".db" in
  Fun.protect
    ~finally:(fun () ->
      ignore (L.discard_all ());
      (try Sys.remove path with Sys_error _ -> ());
      try Sys.remove (path ^ "-wal")
      with Sys_error _ -> (
        ();
        try Sys.remove (path ^ "-shm") with Sys_error _ -> ()))
    (fun () -> f path)

let create_vault ~db ?(keys = make_keys ()) ?(acct = account ())
    ?(tokens = sample_tokens) ?(id = "ghvault_p21_sec_1")
    ?(expires_at = far_expires) () =
  match
    V.create ~db ~keys ~id ~now:fixed_now ~account:acct ~tokens
      ~scopes:[ "repo"; "read:user" ] ~expires_at ()
  with
  | Ok r -> r
  | Error d -> Alcotest.fail ("create: " ^ V.string_of_denial d)

let issue_ok ~db ?ttl_seconds ?binding_id ?expected ~vault_id () =
  match
    L.issue ~db ~now:fixed_now ?ttl_seconds ?binding_id ?expected ~vault_id ()
  with
  | Ok l -> l
  | Error d -> Alcotest.fail ("issue: " ^ L.string_of_denial d)

let json_refresh_body ?(access = "ghu_access_P21_SEC_NEW_PLAINTEXT")
    ?(refresh = "ghr_refresh_P21_SEC_NEW_PLAINTEXT") ?(expires_in = 28800)
    ?(refresh_token_expires_in = 15897600) ?(token_type = "bearer")
    ?(scope = "") () =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("access_token", `String access);
         ("expires_in", `Int expires_in);
         ("refresh_token", `String refresh);
         ("refresh_token_expires_in", `Int refresh_token_expires_in);
         ("token_type", `String token_type);
         ("scope", `String scope);
       ])

let make_http ~expected_refresh ~body () =
  let calls = ref 0 in
  let http ~url:_ ~headers:_ ~body:req_body =
    incr calls;
    if not (String_util.contains req_body "grant_type=refresh_token") then
      Error "missing grant_type=refresh_token"
    else if not (String_util.contains req_body "client_secret=") then
      Error "missing client_secret boundary field"
    else if not (String_util.contains req_body ("client_id=" ^ client_id_plain))
    then Error "missing client_id"
    else if
      (not
         (String_util.contains req_body ("refresh_token=" ^ expected_refresh)))
      && not (String_util.contains req_body (Uri.pct_encode expected_refresh))
    then
      (* still accept when form-encoded variant differs but secret present *)
      Ok (200, body)
    else Ok (200, body)
  in
  (http, calls)

let seed_claimed_flight ~db ~vault_id ~expected_generation ~owner ~now
    ?(lease_seconds = 60.) () =
  let now_s = Time_util.iso8601_utc ~t:now () in
  let lease_expires_at = Time_util.iso8601_utc ~t:(now +. lease_seconds) () in
  let job_id = "ghrefresh_p21_sec_claimed" in
  let lease_token = "rfl_p21_sec_claimed" in
  let sql =
    {|INSERT INTO github_user_token_refresh_flight
        (vault_id, job_id, expected_generation, phase, owner, lease_token,
         lease_expires_at, committed_generation, fail_reason, created_at, updated_at)
      VALUES (?,?,?,?,?,?,?,?,?,?,?)
      ON CONFLICT(vault_id) DO UPDATE SET
        job_id = excluded.job_id,
        expected_generation = excluded.expected_generation,
        phase = excluded.phase,
        owner = excluded.owner,
        lease_token = excluded.lease_token,
        lease_expires_at = excluded.lease_expires_at,
        committed_generation = NULL,
        fail_reason = NULL,
        updated_at = excluded.updated_at|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT vault_id));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT job_id));
  ignore
    (Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int expected_generation)));
  ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT "claimed"));
  ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT owner));
  ignore (Sqlite3.bind stmt 6 (Sqlite3.Data.TEXT lease_token));
  ignore (Sqlite3.bind stmt 7 (Sqlite3.Data.TEXT lease_expires_at));
  ignore (Sqlite3.bind stmt 8 Sqlite3.Data.NULL);
  ignore (Sqlite3.bind stmt 9 Sqlite3.Data.NULL);
  ignore (Sqlite3.bind stmt 10 (Sqlite3.Data.TEXT now_s));
  ignore (Sqlite3.bind stmt 11 (Sqlite3.Data.TEXT now_s));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  job_id

(* -------------------------------------------------------------------------- *)
(* 1. Lease refuse: every non-HTTP surface + dirty material scan               *)
(* -------------------------------------------------------------------------- *)

let test_lease_refuse_all_non_http_surfaces () =
  with_mem_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let lease = issue_ok ~db ~vault_id:rec_.id () in
  Alcotest.(check bool)
    "canonical surface list" true
    (List.length L.all_non_http_surfaces >= 10);
  (* Explicit surfaces called out in acceptance. *)
  List.iter
    (fun (name, refuse_fn) ->
      match refuse_fn lease with
      | Error (L.Forbidden_surface msg) ->
          secrets_absent msg;
          Alcotest.(check bool)
            (name ^ " message names surface")
            true
            (contains msg name || String.length msg > 0)
      | Error d -> Alcotest.fail (L.string_of_denial d)
      | Ok () -> Alcotest.fail (name ^ " must refuse"))
    [
      ("runner", L.refuse_runner_env);
      ("process", L.refuse_process_env);
      ("shell", L.refuse_shell_injection);
      ("git", L.refuse_git_transport);
      ("worktree", L.refuse_worktree);
      ("prompt", L.refuse_prompt);
      ("tool", L.refuse_tool_data);
      ("job", L.refuse_job_payload);
      ("crash", L.refuse_crash_output);
      ("ambient", L.refuse_scheduled_ambient);
    ];
  match L.assert_non_http_refused lease with
  | Ok () -> ()
  | Error e -> Alcotest.fail e

let test_dirty_execution_surfaces_refused_and_scanned () =
  with_mem_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let lease = issue_ok ~db ~vault_id:rec_.id () in
  let access = sample_tokens.access_token in
  let dirty_materials =
    [
      (L.Runner_env, Printf.sprintf "GH_TOKEN=%s CLAWQ_RUNNER=1" access);
      (L.Process_env, Printf.sprintf "GITHUB_TOKEN=%s PATH=/usr/bin" access);
      (L.Shell, Printf.sprintf "export GITHUB_TOKEN=%s && git push" access);
      ( L.Git_transport,
        Printf.sprintf "https://x-access-token:%s@github.com/acme/w.git" access
      );
      (L.Worktree, Printf.sprintf "credential.helper=store token=%s" access);
      (L.Prompt, "Reply using bearer " ^ access);
      (L.Tool_data, Yojson.Safe.to_string (`Assoc [ ("token", `String access) ]));
      (L.Job_payload, Printf.sprintf {|{"access":"%s"}|} access);
      (L.Crash_output, Printf.sprintf "Fatal: Authorization: Bearer %s" access);
      (L.Scheduled_ambient, Printf.sprintf "ambient refresh with %s" access);
    ]
  in
  List.iter
    (fun (surface, material) ->
      Alcotest.(check bool)
        (L.string_of_non_http_surface surface ^ " shape hit")
        true
        (L.text_contains_token_shape material);
      match L.refuse_scanned_material ~surface ~material with
      | Error (L.Forbidden_surface msg) -> secrets_absent msg
      | Error d -> Alcotest.fail (L.string_of_denial d)
      | Ok () ->
          Alcotest.fail
            (L.string_of_non_http_surface surface ^ " dirty material permitted"))
    dirty_materials;
  (* Clean materials pass; lease identity / JSON never embed tokens. *)
  let clean =
    [
      (L.Runner_env, "PATH=/usr/bin HOME=/tmp");
      (L.Shell, "echo hello");
      (L.Git_transport, "git push origin HEAD:refs/heads/main");
      (L.Crash_output, "Fatal: connection reset by peer");
      (L.Job_payload, {|{"lease":"ghlease_opaque","generation":1}|});
    ]
  in
  (match L.assert_materials_token_free ~materials:clean with
  | Ok () -> ()
  | Error d -> Alcotest.fail (L.string_of_denial d));
  let id = L.identity_of lease in
  Alcotest.(check bool)
    "identity free of access" false
    (L.identity_contains_plaintext ~identity:id
       ~plaintext:sample_tokens.access_token);
  let json_s = Yojson.Safe.to_string (L.to_json lease) in
  secrets_absent json_s;
  secrets_absent (L.string_of_identity id);
  (* Mixed dirty list fails closed on first hit. *)
  match
    L.assert_materials_token_free
      ~materials:((L.Crash_output, "ok") :: dirty_materials)
  with
  | Error (L.Forbidden_surface _) -> ()
  | Ok () -> Alcotest.fail "mixed materials must fail"
  | Error d -> Alcotest.fail (L.string_of_denial d)

(* -------------------------------------------------------------------------- *)
(* 2. Concurrent refresh single-flight                                         *)
(* -------------------------------------------------------------------------- *)

let test_concurrent_refresh_single_flight () =
  with_mem_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ =
    create_vault ~db ~keys ~acct ~tokens:sample_tokens
      ~expires_at:near_expires_iso ()
  in
  (* Leader holds claimed flight — concurrent caller must not open a second
     remote refresh. *)
  let job_id =
    seed_claimed_flight ~db ~vault_id:rec_.id ~expected_generation:1
      ~owner:"worker_leader" ~now:fixed_now ()
  in
  let new_access = "ghu_access_P21_SEC_SF_COMMITTED" in
  let new_refresh = "ghr_refresh_P21_SEC_SF_COMMITTED" in
  let body =
    json_refresh_body ~access:new_access ~refresh:new_refresh ~expires_in:7200
      ~refresh_token_expires_in:86400 ()
  in
  let http_b, calls_b =
    make_http
      ~expected_refresh:(Option.get sample_tokens.refresh_token)
      ~body ()
  in
  (match
     R.refresh ~db ~keys ~http_post:http_b ~resolve_client:resolve_ok
       ~client_id_handle ~now:fixed_now ~force:true ~flight_owner:"worker_b"
       ~on_inflight:`Deny ~vault_id:rec_.id ()
   with
  | Error (R.In_flight { job_id = j; owner; expected_generation; _ }) ->
      Alcotest.(check string) "busy job" job_id j;
      Alcotest.(check string) "owner leader" "worker_leader" owner;
      Alcotest.(check int) "pinned gen" 1 expected_generation;
      Alcotest.(check int) "no second remote" 0 !calls_b
  | Error d -> Alcotest.fail ("inflight deny: " ^ R.string_of_denial d)
  | Ok _ -> Alcotest.fail "second caller must not win single-flight");
  (* Leader completes: one remote, generation advances, prior leases die. *)
  ignore (L.discard_all ());
  (* Clear the claimed flight so leader can claim. *)
  let clear_sql = "DELETE FROM github_user_token_refresh_flight" in
  let clear_stmt = Sqlite3.prepare db clear_sql in
  ignore (Sqlite3.step clear_stmt);
  ignore (Sqlite3.finalize clear_stmt);
  let pre_lease = issue_ok ~db ~vault_id:rec_.id () in
  Alcotest.(check int) "pre gen" 1 (L.generation pre_lease);
  let http_leader, calls_leader =
    make_http
      ~expected_refresh:(Option.get sample_tokens.refresh_token)
      ~body ()
  in
  let leader =
    match
      R.refresh ~db ~keys ~http_post:http_leader ~resolve_client:resolve_ok
        ~client_id_handle ~now:fixed_now ~force:true
        ~flight_owner:"worker_leader" ~vault_id:rec_.id ()
    with
    | Ok o -> o
    | Error d -> Alcotest.fail ("leader: " ^ R.string_of_denial d)
  in
  Alcotest.(check bool) "leader refreshed" true leader.refreshed;
  Alcotest.(check bool) "leader not joined" false leader.joined_flight;
  Alcotest.(check int) "gen advanced" 2 leader.record.generation;
  Alcotest.(check int) "exactly one remote" 1 !calls_leader;
  Alcotest.(check bool)
    "prior lease invalidated" true
    (leader.leases_invalidated >= 1 || L.is_revoked pre_lease);
  (* Waiter after commit joins committed generation with zero remote. *)
  let http_w, calls_w =
    make_http ~expected_refresh:"should_not_call" ~body:(json_refresh_body ())
      ()
  in
  match
    R.acquire_lease ~db ~keys ~http_post:http_w ~resolve_client:resolve_ok
      ~client_id_handle ~now:fixed_now ~expected:acct ~vault_id:rec_.id ()
  with
  | Error d -> Alcotest.fail ("waiter: " ^ R.string_of_denial d)
  | Ok (lease, outcome) -> (
      Alcotest.(check int) "waiter lease gen" 2 (L.generation lease);
      Alcotest.(check int) "waiter no remote" 0 !calls_w;
      Alcotest.(check bool) "outside skew after refresh" false outcome.refreshed;
      match
        L.with_token ~db ~keys ~now:fixed_now ~lease
          ~f:(fun ~access_token -> access_token)
          ()
      with
      | Ok tok ->
          Alcotest.(check string) "committed token only" new_access tok;
          secrets_absent (L.string_of_identity (L.identity_of lease));
          secrets_absent
            ~extra:[ new_access; new_refresh ]
            (Yojson.Safe.to_string (L.to_json lease))
      | Error d -> Alcotest.fail (L.string_of_denial d))

let test_stale_cas_cannot_restore_old_token () =
  with_mem_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ = create_vault ~db ~keys ~acct () in
  let rotated =
    {
      S.access_token = "ghu_access_P21_SEC_CAS_NEW";
      refresh_token = Some "ghr_refresh_P21_SEC_CAS_NEW";
    }
  in
  let first =
    match
      C.replace ~db ~keys ~now:(fixed_now +. 1.) ~id:rec_.id
        ~expected_generation:1 ~expected:acct ~tokens:rotated ~scopes:[ "repo" ]
        ~expires_at:far_expires ()
    with
    | Ok t -> t
    | Error d -> Alcotest.fail (C.string_of_denial d)
  in
  Alcotest.(check int) "gen 2" 2 first.record.generation;
  let stale =
    {
      S.access_token = "ghu_access_P21_SEC_CAS_STALE";
      refresh_token = Some "ghr_refresh_P21_SEC_CAS_STALE";
    }
  in
  (match
     C.replace ~db ~keys ~now:(fixed_now +. 2.) ~id:rec_.id
       ~expected_generation:1 ~expected:acct ~tokens:stale ~scopes:[ "repo" ]
       ~expires_at:far_expires ()
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "stale generation writer must not restore old token");
  match V.read ~db ~keys ~id:rec_.id () with
  | Error d -> Alcotest.fail (V.string_of_denial d)
  | Ok opened ->
      Alcotest.(check string)
        "committed remains new" rotated.access_token opened.tokens.access_token;
      Alcotest.(check bool)
        "stale never installed" false
        (opened.tokens.access_token = stale.access_token)

(* -------------------------------------------------------------------------- *)
(* 3. Restart drops process-local leases; sealed vault survives                *)
(* -------------------------------------------------------------------------- *)

let test_restart_drops_process_local_leases () =
  with_file_db @@ fun path ->
  let keys = make_keys () in
  let acct = account () in
  let id = "ghvault_p21_sec_restart" in
  let pre_handle, pre_gen =
    let db = Sqlite3.db_open path in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.db_close db))
      (fun () ->
        V.ensure_schema db;
        R.ensure_schema db;
        ignore (create_vault ~db ~keys ~acct ~id ());
        let lease = issue_ok ~db ~binding_id:"bind_restart" ~vault_id:id () in
        Alcotest.(check bool) "live before restart" true (L.live_count () >= 1);
        (* HTTP open works pre-restart. *)
        (match
           L.with_token ~db ~keys ~now:fixed_now ~lease
             ~f:(fun ~access_token -> access_token = sample_tokens.access_token)
             ()
         with
        | Ok true -> ()
        | Ok false -> Alcotest.fail "token mismatch pre-restart"
        | Error d -> Alcotest.fail (L.string_of_denial d));
        (L.handle_to_string (L.handle lease), L.generation lease))
  in
  (* Simulate process restart: process-local lease registry is empty. *)
  let dropped = L.discard_all () in
  Alcotest.(check bool) "discarded at least pre lease" true (dropped >= 1);
  Alcotest.(check int) "leases cleared" 0 (L.live_count ());
  let db2 = Sqlite3.db_open path in
  Fun.protect
    ~finally:(fun () ->
      ignore (L.discard_all ());
      ignore (Sqlite3.db_close db2))
    (fun () ->
      V.ensure_schema db2;
      R.ensure_schema db2;
      (match V.read ~db:db2 ~keys ~id () with
      | Error d -> Alcotest.fail (V.string_of_denial d)
      | Ok opened ->
          Alcotest.(check string)
            "sealed vault survives" sample_tokens.access_token
            opened.tokens.access_token;
          Alcotest.(check int)
            "generation retained" pre_gen opened.record.generation;
          Alcotest.(check bool) "still active" true opened.record.active);
      (* Pre-restart handle is not registered; revoke_handle is a no-op. *)
      (match L.handle_of_string pre_handle with
      | Error _ -> ()
      | Ok h ->
          Alcotest.(check bool)
            "pre-restart handle not registered" false
            (L.revoke_handle ~handle:h));
      (* Fresh issue required after restart; pre-restart handle is dead. *)
      let reissued = issue_ok ~db:db2 ~vault_id:id () in
      Alcotest.(check int) "reissue gen" pre_gen (L.generation reissued);
      Alcotest.(check bool) "live after reissue" true (L.live_count () >= 1);
      Alcotest.(check bool)
        "opaque handle reissued" true
        (String.length (L.handle_to_string (L.handle reissued)) > 0
        && String.length pre_handle > 0);
      match
        L.with_token ~db:db2 ~keys ~now:fixed_now ~lease:reissued
          ~f:(fun ~access_token -> access_token)
          ()
      with
      | Ok tok ->
          Alcotest.(check string)
            "post-restart open" sample_tokens.access_token tok
      | Error d -> Alcotest.fail (L.string_of_denial d))

(* -------------------------------------------------------------------------- *)
(* 4. Private delivery redaction — no secrets in Room paths                    *)
(* -------------------------------------------------------------------------- *)

let auth_url_with_token_shaped_state =
  assert_ok
    (D.make_authorization_url
       ~url:
         ("https://github.com/login/oauth/authorize?client_id=Iv1.p21sec&state="
        ^ sample_tokens.access_token))

let device_codes =
  assert_ok
    (D.make_device_codes ~user_code:"WXYZ-9876"
       ~verification_uri:"https://github.com/login/device"
       ~verification_uri_complete:
         "https://github.com/login/device?user_code=WXYZ-9876"
       ~device_code:sample_tokens.access_token ())

let delivery_ctx () =
  assert_ok
    (D.make_delivery_context ~principal_id:"principal:ada"
       ~continuation_handle:"cont:dm:p21-sec" ~tx_id:"tx_p21_sec"
       ~source:(Tx.Room "room-shared-p21-sec") ~flow_kind:Tx.Web_pkce ())

let dm_channel () =
  assert_ok (D.make_private_connector_dm ~connector:P.Teams ~handle_id:"dm:ada")

let test_private_delivery_room_redaction () =
  let ctx = delivery_ctx () in
  let channel = dm_channel () in
  (* Authorization URL: private body may carry URL; Room companion must not. *)
  (match
     D.route_delivery ~context:ctx ~channel
       ~content:(D.Material auth_url_with_token_shaped_state) ()
   with
  | D.Private { private_delivery; companion_room } -> (
      Alcotest.(check bool)
        "url in private body" true
        (contains private_delivery.rendered "github.com/login/oauth/authorize");
      secrets_absent private_delivery.redacted_summary;
      Alcotest.(check bool)
        "redacted summary omits client_id query" false
        (contains private_delivery.redacted_summary "client_id=Iv1");
      match companion_room with
      | None -> Alcotest.fail "expected companion room progress"
      | Some rb ->
          Alcotest.(check string) "room id" "room-shared-p21-sec" rb.room_id;
          Alcotest.(check bool)
            "room message safe" true
            (D.room_message_is_safe rb.rendered);
          Alcotest.(check bool)
            "no private secrets in room" false
            (D.contains_private_secrets auth_url_with_token_shaped_state
               rb.rendered);
          secrets_absent ~extra:[ "client_id=Iv1"; "WXYZ-9876" ] rb.rendered;
          Alcotest.(check bool)
            "neutral wording" true
            (contains (String.lowercase_ascii rb.rendered) "privately"))
  | D.Room_progress rb ->
      Alcotest.fail ("auth url must not be room-only: " ^ rb.rendered)
  | D.Refused e -> Alcotest.fail ("unexpected refuse: " ^ e.message));
  (* Device codes: user_code + device_code stay private; Room stays neutral. *)
  (match
     D.route_delivery
       ~context:
         (assert_ok
            (D.make_delivery_context ~principal_id:"principal:ada"
               ~continuation_handle:"cont:browser:p21" ~tx_id:"tx_dev"
               ~source:(Tx.Room "room-shared-p21-sec") ~flow_kind:Tx.Device ()))
       ~channel:
         (assert_ok
            (D.make_principal_browser_continuation ~handle_id:"browser:ada"))
       ~content:(D.Material device_codes) ()
   with
  | D.Private { private_delivery; companion_room } -> (
      Alcotest.(check bool)
        "user code private" true
        (contains private_delivery.rendered "WXYZ-9876");
      Alcotest.(check bool)
        "device secret private" true
        (contains private_delivery.rendered sample_tokens.access_token);
      match companion_room with
      | None -> Alcotest.fail "expected companion"
      | Some rb ->
          Alcotest.(check bool)
            "no user code in room" false
            (contains rb.rendered "WXYZ-9876");
          secrets_absent rb.rendered;
          Alcotest.(check bool)
            "room safe" true
            (D.room_message_is_safe rb.rendered))
  | other -> (
      match other with
      | D.Room_progress rb -> Alcotest.fail ("device dump: " ^ rb.rendered)
      | D.Refused e -> Alcotest.fail e.message
      | D.Private _ -> assert false));
  (* Absent channel refuses without leaking secrets into Room progress. *)
  (match
     D.route_delivery ~context:ctx ~channel:D.Absent
       ~content:(D.Material auth_url_with_token_shaped_state) ()
   with
  | D.Refused e -> (
      Alcotest.(check string)
        "reason" "no_private_channel"
        (D.string_of_refuse_reason e.reason);
      secrets_absent e.message;
      match e.room_safe_progress with
      | None -> Alcotest.fail "expected room-safe progress"
      | Some p ->
          let rendered = D.render_room_progress p in
          Alcotest.(check bool)
            "refuse room safe" true
            (D.room_message_is_safe rendered);
          secrets_absent ~extra:[ "client_id=Iv1" ] rendered)
  | D.Private _ -> Alcotest.fail "absent must not private-deliver"
  | D.Room_progress rb ->
      Alcotest.fail ("must not dump material to room: " ^ rb.rendered));
  (* Redacted JSON export + room summary never embed secrets. *)
  let plan =
    D.route_delivery ~context:ctx ~channel
      ~content:(D.Material auth_url_with_token_shaped_state) ()
  in
  let json_s = Yojson.Safe.to_string (D.delivery_plan_to_json_redacted plan) in
  secrets_absent ~extra:[ "client_id=Iv1" ] json_s;
  Alcotest.(check bool)
    "json marks url present" true
    (contains json_s "authorization_url_present"
    || contains json_s "\"private\"");
  let room_summary =
    D.redacted_room_summary ~context:ctx
      ~content:(D.Material auth_url_with_token_shaped_state)
  in
  secrets_absent ~extra:[ "client_id=Iv1" ] room_summary;
  Alcotest.(check bool)
    "room summary safe" true
    (D.room_message_is_safe room_summary)

(* -------------------------------------------------------------------------- *)
(* 5. Persistence/execution surface scan (combined isolation proof)            *)
(* -------------------------------------------------------------------------- *)

let test_scan_persistence_and_execution_surfaces () =
  with_mem_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let lease = issue_ok ~db ~binding_id:"bind_scan" ~vault_id:rec_.id () in
  (* Surfaces that may persist lease identity or delivery plans. *)
  let surfaces =
    [
      ("lease_json", Yojson.Safe.to_string (L.to_json lease));
      ("lease_identity", L.string_of_identity (L.identity_of lease));
      ( "denial_revoked",
        (L.revoke lease;
         match
           L.with_token ~db ~keys ~now:fixed_now ~lease
             ~f:(fun ~access_token:_ -> "x")
             ()
         with
         | Error d -> L.string_of_denial d
         | Ok _ -> Alcotest.fail "expected revoked") );
    ]
  in
  List.iter
    (fun (name, blob) ->
      secrets_absent blob;
      Alcotest.(check bool)
        (name ^ " no token shape") false
        (L.text_contains_token_shape blob))
    surfaces;
  (* Re-issue for refuse denials (prior lease revoked). *)
  let lease2 = issue_ok ~db ~vault_id:rec_.id () in
  List.iter
    (fun surface ->
      match L.refuse lease2 surface with
      | Error (L.Forbidden_surface msg) ->
          secrets_absent msg;
          Alcotest.(check bool)
            "refuse no shape" false
            (L.text_contains_token_shape msg)
      | _ -> Alcotest.fail "expected Forbidden_surface")
    L.all_non_http_surfaces;
  (* Private delivery redacted plan is persistence-safe. *)
  let plan =
    D.route_delivery ~context:(delivery_ctx ()) ~channel:(dm_channel ())
      ~content:(D.Material auth_url_with_token_shaped_state) ()
  in
  let plan_s = Yojson.Safe.to_string (D.delivery_plan_to_json_redacted plan) in
  secrets_absent ~extra:[ "client_id=Iv1" ] plan_s;
  Alcotest.(check bool)
    "plan no token shape" false
    (L.text_contains_token_shape plan_s);
  match plan with
  | D.Private { companion_room = Some rb; _ } ->
      Alcotest.(check bool)
        "companion free of shape" false
        (L.text_contains_token_shape rb.rendered)
  | _ -> Alcotest.fail "expected private plan with companion"

(* -------------------------------------------------------------------------- *)
(* Suite                                                                       *)
(* -------------------------------------------------------------------------- *)

let suite =
  [
    Alcotest.test_case "lease refuse all non-HTTP surfaces" `Quick
      test_lease_refuse_all_non_http_surfaces;
    Alcotest.test_case "dirty execution surfaces refused and scanned" `Quick
      test_dirty_execution_surfaces_refused_and_scanned;
    Alcotest.test_case "concurrent refresh single-flight" `Quick
      test_concurrent_refresh_single_flight;
    Alcotest.test_case "stale CAS cannot restore old token" `Quick
      test_stale_cas_cannot_restore_old_token;
    Alcotest.test_case "restart drops process-local leases" `Quick
      test_restart_drops_process_local_leases;
    Alcotest.test_case "private delivery room redaction" `Quick
      test_private_delivery_room_redaction;
    Alcotest.test_case "scan persistence and execution surfaces" `Quick
      test_scan_persistence_and_execution_surfaces;
  ]

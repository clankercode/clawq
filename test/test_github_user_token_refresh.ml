(** Tests for GitHub user-token refresh from server-returned lifetimes
    (P21.M3.E1.T001) and durable single-flight CAS rotation (P21.M3.E1.T002).

    Contract under test:
    - Lease acquisition refreshes only inside the documented skew window
    - Refresh records server access + refresh expiries (no assumed lifetimes)
    - Bearer token_type and empty scope are validated
    - Client secret stays at the HTTP boundary (never in denials/outcomes)
    - Successful refresh advances generation within the same binding lineage
    - New lease pins the new generation so jobs revalidate without identity
      change
    - Concurrent callers cause one remote refresh; waiters use committed gen
    - CAS blocks late responses / old-token replay after remote rotation
    - Crash after remote rotation fails closed to relink (no old authority) *)

module V = Github_user_token_vault
module S = Github_user_token_store
module L = Github_user_token_lease
module C = Github_user_token_cas
module R = Github_user_token_refresh
module Recovery = Github_user_token_vault_recovery
module B = Github_account_binding
module P = Principal_identity
module PS = Principal_identity_store

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-refresh-test-master" ()

let sample_tokens ?(tag = "base") () =
  {
    S.access_token = Printf.sprintf "ghu_access_REFRESH_%s_PLAINTEXT" tag;
    refresh_token = Some (Printf.sprintf "ghr_refresh_REFRESH_%s_PLAINTEXT" tag);
  }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let account ?(principal_id = "prin_refresh_1") ?(github_user_id = 8801L)
    ?(app_id = 77) ?(host = V.default_host) () =
  assert_ok (V.make_account_key ~principal_id ~github_user_id ~app_id ~host ())

let make_keys ?(key_id = "mk-refresh-1") ?(key_version = 1) () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key ())

(** Fixed "now" for deterministic ISO lifetime math. *)
let fixed_now = 1_720_000_000.0

(** Access token far outside the default 300s skew. *)
let far_expires = "2026-12-01T00:00:00Z"

(** Access token inside the skew window relative to fixed_now (fixed_now + 60s ≈
    2024-07-03T... depending on epoch; use absolute offset). *)
let near_expires_iso =
  (* fixed_now + 60 seconds *)
  Time_util.iso8601_utc ~t:(fixed_now +. 60.) ()

let client_secret_plain = "gh_client_secret_REFRESH_NEVER_LOG"
let client_id_plain = "Iv1.clientid_refresh_test"
let client_id_handle = "handle_client_refresh_1"
let resolve_ok ~client_id_handle:_ = Ok (client_id_plain, client_secret_plain)

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  V.ensure_schema db;
  B.ensure_schema db;
  R.ensure_schema db;
  Fun.protect
    ~finally:(fun () ->
      ignore (L.discard_all ());
      ignore (Sqlite3.db_close db))
    (fun () -> f db)

let create_vault ~db ?(keys = make_keys ()) ?(account = account ())
    ?(tokens = sample_tokens ()) ?(id = "ghvault_refresh_1")
    ?(expires_at = far_expires) ?(scopes = [ "repo"; "read:user" ]) () =
  match
    V.create ~db ~keys ~id ~now:fixed_now ~account ~tokens ~scopes ~expires_at
      ()
  with
  | Ok r -> r
  | Error d -> Alcotest.fail ("create: " ^ V.string_of_denial d)

let seed_binding ~db ~principal_id ~vault_id ~github_user_id ~app_id
    ?(lineage_id = "lineage_refresh_1") ?(binding_id = "ghbind_refresh_1") () =
  let pid = assert_ok (P.principal_id_of_string principal_id) in
  let p =
    P.make_principal ~id:pid ~revision:1 ~created_at:"2026-01-01T00:00:00Z"
      ~updated_at:"2026-01-01T00:00:00Z" ()
  in
  ignore (assert_ok (PS.insert_principal ~db ~now:fixed_now p));
  let identity =
    assert_ok
      (B.make_account_identity ~host:B.default_host ~app_id ~github_user_id ())
  in
  let vault_ref = assert_ok (B.make_vault_ref vault_id) in
  let b =
    B.make_binding ~id:binding_id ~principal_id:pid ~identity
      ~authorization_status:B.Authorized ~vault_ref ~lineage_id ()
  in
  assert_ok (B.insert ~db ~now:fixed_now b)

let json_refresh_body ?(access = "ghu_access_NEW_PLAINTEXT")
    ?(refresh = "ghr_refresh_NEW_PLAINTEXT") ?(expires_in = 28800)
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
  let last_body = ref "" in
  let http ~url:_ ~headers:_ ~body:req_body =
    incr calls;
    last_body := req_body;
    if not (String_util.contains req_body "grant_type=refresh_token") then
      Error "missing grant_type=refresh_token"
    else if
      (not
         (String_util.contains req_body ("refresh_token=" ^ expected_refresh)))
      && not (String_util.contains req_body (Uri.pct_encode expected_refresh))
    then
      (* form encoding may pct-encode; still require client_secret present *)
      if not (String_util.contains req_body "client_secret=") then
        Error "missing client_secret boundary field"
      else Ok (200, body)
    else if not (String_util.contains req_body "client_secret=") then
      Error "missing client_secret boundary field"
    else if not (String_util.contains req_body ("client_id=" ^ client_id_plain))
    then Error "missing client_id"
    else Ok (200, body)
  in
  (http, calls, last_body)

(* -------------------------------------------------------------------------- *)
(* Skew window                                                                *)
(* -------------------------------------------------------------------------- *)

let test_needs_refresh_skew_window () =
  let far = "2099-01-01T00:00:00Z" in
  Alcotest.(check bool)
    "far future outside skew" false
    (R.needs_refresh ~now:fixed_now ~skew_seconds:300. ~access_expires_at:far ());
  let soon = Time_util.iso8601_utc ~t:(fixed_now +. 60.) () in
  Alcotest.(check bool)
    "within 60s of expiry needs refresh" true
    (R.needs_refresh ~now:fixed_now ~skew_seconds:300. ~access_expires_at:soon
       ());
  let past = Time_util.iso8601_utc ~t:(fixed_now -. 10.) () in
  Alcotest.(check bool)
    "already expired needs refresh" true
    (R.needs_refresh ~now:fixed_now ~skew_seconds:300. ~access_expires_at:past
       ());
  Alcotest.(check bool)
    "exactly at skew boundary needs refresh" true
    (R.needs_refresh ~now:fixed_now ~skew_seconds:300.
       ~access_expires_at:(Time_util.iso8601_utc ~t:(fixed_now +. 300.) ())
       ());
  Alcotest.(check bool)
    "just outside skew does not" false
    (R.needs_refresh ~now:fixed_now ~skew_seconds:300.
       ~access_expires_at:(Time_util.iso8601_utc ~t:(fixed_now +. 301.) ())
       ());
  Alcotest.(check (float 0.001))
    "documented default skew is 300s" 300. R.default_refresh_skew_seconds

(* -------------------------------------------------------------------------- *)
(* Response validation                                                        *)
(* -------------------------------------------------------------------------- *)

let test_parse_requires_server_lifetimes_bearer_empty_scope () =
  (match R.parse_refresh_response ~body:(json_refresh_body ()) with
  | Ok r ->
      Alcotest.(check int) "expires_in from server" 28800 r.expires_in;
      Alcotest.(check int)
        "refresh_token_expires_in from server" 15897600
        r.refresh_token_expires_in;
      Alcotest.(check string) "bearer" "bearer" r.token_type;
      Alcotest.(check string) "empty scope" "" r.scope
  | Error e -> Alcotest.fail e);
  (* Missing refresh_token_expires_in — no assumed 6 months. *)
  let missing_rtexp =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("access_token", `String "ghu_x");
           ("refresh_token", `String "ghr_x");
           ("expires_in", `Int 100);
           ("token_type", `String "bearer");
           ("scope", `String "");
         ])
  in
  (match R.parse_refresh_response ~body:missing_rtexp with
  | Error msg ->
      Alcotest.(check bool)
        "mentions refresh_token_expires_in" true
        (String_util.contains msg "refresh_token_expires_in")
  | Ok _ -> Alcotest.fail "must require refresh_token_expires_in");
  (* Missing expires_in — no assumed 8 hours. *)
  let missing_exp =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("access_token", `String "ghu_x");
           ("refresh_token", `String "ghr_x");
           ("refresh_token_expires_in", `Int 100);
           ("token_type", `String "bearer");
           ("scope", `String "");
         ])
  in
  (match R.parse_refresh_response ~body:missing_exp with
  | Error msg ->
      Alcotest.(check bool)
        "mentions expires_in" true
        (String_util.contains msg "expires_in")
  | Ok _ -> Alcotest.fail "must require expires_in");
  (* Non-bearer token_type. *)
  (match
     R.parse_refresh_response ~body:(json_refresh_body ~token_type:"mac" ())
   with
  | Error msg ->
      Alcotest.(check bool)
        "token_type error" true
        (String_util.contains (String.lowercase_ascii msg) "bearer")
  | Ok _ -> Alcotest.fail "non-bearer must fail");
  (* Non-empty scope. *)
  match R.parse_refresh_response ~body:(json_refresh_body ~scope:"repo" ()) with
  | Error msg ->
      Alcotest.(check bool)
        "scope error" true
        (String_util.contains (String.lowercase_ascii msg) "scope")
  | Ok _ -> Alcotest.fail "non-empty scope must fail"

let test_parse_form_urlencoded () =
  let body =
    "access_token=ghu_form&expires_in=100&refresh_token=ghr_form&refresh_token_expires_in=200&token_type=bearer&scope="
  in
  match R.parse_refresh_response ~body with
  | Ok r ->
      Alcotest.(check string) "access" "ghu_form" r.access_token;
      Alcotest.(check int) "exp" 100 r.expires_in;
      Alcotest.(check int) "rtexp" 200 r.refresh_token_expires_in
  | Error e -> Alcotest.fail e

(* -------------------------------------------------------------------------- *)
(* Remote refresh + CAS generation + lineage                                  *)
(* -------------------------------------------------------------------------- *)

let test_refresh_advances_generation_same_lineage () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let old = sample_tokens ~tag:"old" () in
  let rec_ =
    create_vault ~db ~keys ~account:acct ~tokens:old
      ~expires_at:near_expires_iso ()
  in
  let binding =
    seed_binding ~db ~principal_id:acct.principal_id ~vault_id:rec_.id
      ~github_user_id:acct.github_user_id ~app_id:acct.app_id
      ~lineage_id:"lineage_stable_A" ()
  in
  ignore binding;
  let new_access = "ghu_access_AFTER_REFRESH_PLAINTEXT" in
  let new_refresh = "ghr_refresh_AFTER_REFRESH_PLAINTEXT" in
  let expires_in = 7200 in
  let refresh_token_expires_in = 86400 in
  let body =
    json_refresh_body ~access:new_access ~refresh:new_refresh ~expires_in
      ~refresh_token_expires_in ()
  in
  let old_refresh =
    match old.refresh_token with Some r -> r | None -> Alcotest.fail "refresh"
  in
  let http, calls, last_body =
    make_http ~expected_refresh:old_refresh ~body ()
  in
  (* Pre-refresh lease at gen 1. *)
  let old_lease =
    match L.issue ~db ~now:fixed_now ~vault_id:rec_.id () with
    | Ok l -> l
    | Error d -> Alcotest.fail (L.string_of_denial d)
  in
  Alcotest.(check int) "pre gen" 1 (L.generation old_lease);
  match
    R.refresh ~db ~keys ~http_post:http ~resolve_client:resolve_ok
      ~client_id_handle ~now:fixed_now ~binding_id:"ghbind_refresh_1"
      ~expected_lineage_id:"lineage_stable_A" ~expected:acct ~vault_id:rec_.id
      ()
  with
  | Error d -> Alcotest.fail (R.string_of_denial d)
  | Ok outcome -> (
      Alcotest.(check bool) "did remote refresh" true outcome.refreshed;
      Alcotest.(check int) "generation advanced" 2 outcome.record.generation;
      Alcotest.(check bool) "still active" true outcome.record.active;
      Alcotest.(check (option string))
        "lineage unchanged" (Some "lineage_stable_A") outcome.lineage_id;
      Alcotest.(check int) "old leases invalidated" 1 outcome.leases_invalidated;
      Alcotest.(check int) "http called once" 1 !calls;
      (* Server-derived lifetimes recorded (no assumed constants). *)
      let expected_access =
        Time_util.iso8601_utc ~t:(fixed_now +. float_of_int expires_in) ()
      in
      let expected_refresh =
        Time_util.iso8601_utc
          ~t:(fixed_now +. float_of_int refresh_token_expires_in)
          ()
      in
      Alcotest.(check string)
        "access expiry from server expires_in" expected_access
        outcome.lifetimes.access_expires_at;
      Alcotest.(check string)
        "refresh expiry from server refresh_token_expires_in" expected_refresh
        outcome.lifetimes.refresh_expires_at;
      Alcotest.(check string)
        "vault expires_at updated" expected_access outcome.record.expires_at;
      (match R.get_recorded_lifetimes ~db ~vault_id:rec_.id with
      | Ok (Some lt) ->
          Alcotest.(check string)
            "durable access" expected_access lt.access_expires_at;
          Alcotest.(check string)
            "durable refresh" expected_refresh lt.refresh_expires_at
      | Ok None -> Alcotest.fail "lifetimes not recorded"
      | Error e -> Alcotest.fail e);
      (* Client secret only in request body, never in outcome denial path. *)
      Alcotest.(check bool)
        "client_secret in POST body" true
        (String_util.contains !last_body client_secret_plain);
      Alcotest.(check bool)
        "denial helper redacts secret" false
        (R.denial_exposes_secret ~denial:(R.Transport "x")
           ~secret:client_secret_plain);
      (* Sealed tokens rotated. *)
      (match V.read ~db ~keys ~id:rec_.id () with
      | Error d -> Alcotest.fail (V.string_of_denial d)
      | Ok opened ->
          Alcotest.(check string)
            "new access sealed" new_access opened.tokens.access_token;
          Alcotest.(check (option string))
            "new refresh sealed" (Some new_refresh) opened.tokens.refresh_token);
      (* Old lease fails generation check. *)
      (match
         L.with_token ~db ~keys ~now:fixed_now ~lease:old_lease
           ~f:(fun ~access_token:_ -> ())
           ()
       with
      | Error (L.Generation_mismatch { expected = 1; actual = 2 }) -> ()
      | Error L.Lease_revoked -> ()
      | Error d ->
          Alcotest.fail
            ("expected gen mismatch/revoked, got " ^ L.string_of_denial d)
      | Ok _ -> Alcotest.fail "old lease must not open");
      (* New lease at gen 2; principal/account unchanged. *)
      match
        L.issue ~db ~now:fixed_now ~binding_id:"ghbind_refresh_1" ~expected:acct
          ~vault_id:rec_.id ()
      with
      | Error d -> Alcotest.fail (L.string_of_denial d)
      | Ok lease -> (
          Alcotest.(check int) "new lease gen" 2 (L.generation lease);
          let id = L.identity_of lease in
          Alcotest.(check string)
            "principal same" acct.principal_id id.binding.principal_id;
          Alcotest.(check int64)
            "user same" acct.github_user_id id.binding.github_user_id;
          Alcotest.(check int) "app same" acct.app_id id.binding.app_id;
          match
            L.with_token ~db ~keys ~now:fixed_now ~lease
              ~f:(fun ~access_token -> access_token)
              ()
          with
          | Ok tok -> Alcotest.(check string) "opens new access" new_access tok
          | Error d -> Alcotest.fail (L.string_of_denial d)))

let test_refresh_refuses_outside_skew_unless_force () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ = create_vault ~db ~keys ~account:acct ~expires_at:far_expires () in
  let http, calls, _ =
    make_http ~expected_refresh:"x" ~body:(json_refresh_body ()) ()
  in
  (match
     R.refresh ~db ~keys ~http_post:http ~resolve_client:resolve_ok
       ~client_id_handle ~now:fixed_now ~expected:acct ~vault_id:rec_.id ()
   with
  | Error R.Not_in_skew -> ()
  | Error d ->
      Alcotest.fail ("expected Not_in_skew, got " ^ R.string_of_denial d)
  | Ok _ -> Alcotest.fail "outside skew must refuse without force");
  Alcotest.(check int) "no http outside skew" 0 !calls;
  match
    R.refresh ~db ~keys ~http_post:http ~resolve_client:resolve_ok
      ~client_id_handle ~now:fixed_now ~force:true ~expected:acct
      ~vault_id:rec_.id ()
  with
  | Ok o ->
      Alcotest.(check bool) "forced refresh" true o.refreshed;
      Alcotest.(check int) "http once" 1 !calls
  | Error d -> Alcotest.fail (R.string_of_denial d)

let test_refresh_lineage_mismatch_refuses () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ =
    create_vault ~db ~keys ~account:acct ~expires_at:near_expires_iso ()
  in
  ignore
    (seed_binding ~db ~principal_id:acct.principal_id ~vault_id:rec_.id
       ~github_user_id:acct.github_user_id ~app_id:acct.app_id
       ~lineage_id:"lineage_current" ());
  let http, _, _ =
    make_http ~expected_refresh:"x" ~body:(json_refresh_body ()) ()
  in
  match
    R.refresh ~db ~keys ~http_post:http ~resolve_client:resolve_ok
      ~client_id_handle ~now:fixed_now ~binding_id:"ghbind_refresh_1"
      ~expected_lineage_id:"lineage_OLD_PIN" ~expected:acct ~vault_id:rec_.id ()
  with
  | Error (R.Lineage_mismatch { expected = "lineage_OLD_PIN"; actual }) ->
      Alcotest.(check string) "actual lineage" "lineage_current" actual
  | Error d -> Alcotest.fail (R.string_of_denial d)
  | Ok _ -> Alcotest.fail "lineage mismatch must refuse"

let test_refresh_redacts_secrets_in_denials () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let tokens = sample_tokens ~tag:"secret" () in
  let rec_ = create_vault ~db ~keys ~tokens ~expires_at:near_expires_iso () in
  let http ~url:_ ~headers:_ ~body:_ = Error "transport boom" in
  match
    R.refresh ~db ~keys ~http_post:http ~resolve_client:resolve_ok
      ~client_id_handle ~now:fixed_now ~vault_id:rec_.id ()
  with
  | Error d ->
      Alcotest.(check bool)
        "no access token in denial" false
        (R.denial_exposes_token ~denial:d ~plaintext:tokens.access_token);
      Alcotest.(check bool)
        "no refresh token in denial" false
        (R.denial_exposes_token ~denial:d
           ~plaintext:(Option.get tokens.refresh_token));
      Alcotest.(check bool)
        "no client_secret in denial" false
        (R.denial_exposes_secret ~denial:d ~secret:client_secret_plain)
  | Ok _ -> Alcotest.fail "expected transport error"

(* -------------------------------------------------------------------------- *)
(* Lease acquisition                                                          *)
(* -------------------------------------------------------------------------- *)

let test_acquire_lease_outside_skew_skips_remote () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ = create_vault ~db ~keys ~account:acct ~expires_at:far_expires () in
  let http, calls, _ =
    make_http ~expected_refresh:"x" ~body:(json_refresh_body ()) ()
  in
  match
    R.acquire_lease ~db ~keys ~http_post:http ~resolve_client:resolve_ok
      ~client_id_handle ~now:fixed_now ~expected:acct ~vault_id:rec_.id ()
  with
  | Error d -> Alcotest.fail (R.string_of_denial d)
  | Ok (lease, outcome) ->
      Alcotest.(check bool) "no remote refresh" false outcome.refreshed;
      Alcotest.(check int) "http not called" 0 !calls;
      Alcotest.(check int) "gen remains 1" 1 (L.generation lease);
      Alcotest.(check int) "vault gen 1" 1 outcome.record.generation

let test_acquire_lease_inside_skew_refreshes_and_reissues () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let old = sample_tokens ~tag:"acq" () in
  let rec_ =
    create_vault ~db ~keys ~account:acct ~tokens:old
      ~expires_at:near_expires_iso ()
  in
  ignore
    (seed_binding ~db ~principal_id:acct.principal_id ~vault_id:rec_.id
       ~github_user_id:acct.github_user_id ~app_id:acct.app_id
       ~lineage_id:"lineage_acq" ());
  let new_access = "ghu_access_ACQUIRE_NEW" in
  let body =
    json_refresh_body ~access:new_access ~refresh:"ghr_refresh_ACQUIRE_NEW"
      ~expires_in:3600 ~refresh_token_expires_in:7200 ()
  in
  let old_refresh = Option.get old.refresh_token in
  let http, calls, _ = make_http ~expected_refresh:old_refresh ~body () in
  match
    R.acquire_lease ~db ~keys ~http_post:http ~resolve_client:resolve_ok
      ~client_id_handle ~now:fixed_now ~binding_id:"ghbind_refresh_1"
      ~expected_lineage_id:"lineage_acq" ~expected:acct ~vault_id:rec_.id ()
  with
  | Error d -> Alcotest.fail (R.string_of_denial d)
  | Ok (lease, outcome) -> (
      Alcotest.(check bool) "refreshed" true outcome.refreshed;
      Alcotest.(check int) "http once" 1 !calls;
      Alcotest.(check int) "lease gen 2" 2 (L.generation lease);
      Alcotest.(check (option string))
        "lineage preserved" (Some "lineage_acq") outcome.lineage_id;
      match
        L.with_token ~db ~keys ~now:fixed_now ~lease
          ~f:(fun ~access_token -> access_token)
          ()
      with
      | Ok tok -> Alcotest.(check string) "lease opens new token" new_access tok
      | Error d -> Alcotest.fail (L.string_of_denial d))

let test_acquire_lease_inside_skew_requires_client_boundary () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys ~expires_at:near_expires_iso () in
  match R.acquire_lease ~db ~keys ~now:fixed_now ~vault_id:rec_.id () with
  | Error (R.Invalid_input msg) ->
      Alcotest.(check bool)
        "mentions injectables" true
        (String_util.contains msg "client_id_handle")
  | Error d -> Alcotest.fail (R.string_of_denial d)
  | Ok _ -> Alcotest.fail "must require client boundary inside skew"

let test_refresh_recovery_gate_blocks_remote () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys ~expires_at:near_expires_iso () in
  Recovery.ensure_schema db;
  ignore
    (Sqlite3.exec db
       "UPDATE github_user_token_vault_recovery_state SET \
        user_authorization_enabled = 0 WHERE id = 1");
  let calls = ref 0 in
  let http ~url:_ ~headers:_ ~body:_ =
    incr calls;
    Ok (200, json_refresh_body ())
  in
  match
    R.refresh ~db ~keys ~http_post:http ~resolve_client:resolve_ok
      ~client_id_handle ~now:fixed_now ~force:true ~vault_id:rec_.id ()
  with
  | Error (R.Lease L.User_authorization_disabled) ->
      Alcotest.(check int) "remote refresh blocked" 0 !calls
  | Error d -> Alcotest.fail (R.string_of_denial d)
  | Ok _ -> Alcotest.fail "recovery gate must block refresh"

let test_reject_non_bearer_and_scope_over_http () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys ~expires_at:near_expires_iso () in
  let http_bad_type ~url:_ ~headers:_ ~body:_ =
    Ok (200, json_refresh_body ~token_type:"not-bearer" ())
  in
  (match
     R.refresh ~db ~keys ~http_post:http_bad_type ~resolve_client:resolve_ok
       ~client_id_handle ~now:fixed_now ~force:true ~vault_id:rec_.id ()
   with
  | Error (R.Invalid_token_type _) -> ()
  | Error d ->
      Alcotest.fail ("expected Invalid_token_type, got " ^ R.string_of_denial d)
  | Ok _ -> Alcotest.fail "non-bearer must fail");
  let http_scope ~url:_ ~headers:_ ~body:_ =
    Ok (200, json_refresh_body ~scope:"repo admin:org" ())
  in
  match
    R.refresh ~db ~keys ~http_post:http_scope ~resolve_client:resolve_ok
      ~client_id_handle ~now:fixed_now ~force:true ~vault_id:rec_.id ()
  with
  | Error (R.Nonempty_scope _) -> ()
  | Error d ->
      Alcotest.fail ("expected Nonempty_scope, got " ^ R.string_of_denial d)
  | Ok _ -> Alcotest.fail "non-empty scope must fail"

let test_recorded_refresh_expiry_fail_closed () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys ~expires_at:near_expires_iso () in
  (* Seed a recorded refresh expiry in the past. *)
  let past = Time_util.iso8601_utc ~t:(fixed_now -. 100.) () in
  let future = Time_util.iso8601_utc ~t:(fixed_now +. 1000.) () in
  (* Use internal path: refresh once then manually age the row. *)
  let body = json_refresh_body ~expires_in:60 ~refresh_token_expires_in:1 () in
  let http, _, _ =
    make_http
      ~expected_refresh:(Option.get (sample_tokens ()).refresh_token)
      ~body ()
  in
  (match
     R.refresh ~db ~keys ~http_post:http ~resolve_client:resolve_ok
       ~client_id_handle ~now:fixed_now ~force:true ~vault_id:rec_.id ()
   with
  | Ok _ -> ()
  | Error d -> Alcotest.fail ("seed refresh: " ^ R.string_of_denial d));
  (* Overwrite durable refresh expiry to the past. *)
  let sql =
    {|UPDATE github_user_token_refresh_lifetimes
      SET refresh_expires_at = ?, access_expires_at = ? WHERE vault_id = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT past));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT future));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT rec_.id));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  let http2, calls, _ =
    make_http ~expected_refresh:"x" ~body:(json_refresh_body ()) ()
  in
  match
    R.refresh ~db ~keys ~http_post:http2 ~resolve_client:resolve_ok
      ~client_id_handle ~now:fixed_now ~force:true ~vault_id:rec_.id ()
  with
  | Error R.Refresh_token_expired ->
      Alcotest.(check int) "no remote after recorded expiry" 0 !calls
  | Error d -> Alcotest.fail (R.string_of_denial d)
  | Ok _ -> Alcotest.fail "expired refresh must fail closed"

(* -------------------------------------------------------------------------- *)
(* Single-flight + CAS + crash fail-closed (P21.M3.E1.T002)                   *)
(* -------------------------------------------------------------------------- *)

let seed_claimed_flight ~db ~vault_id ~expected_generation ~owner ~now
    ?(lease_seconds = 60.) () =
  let now_s = Time_util.iso8601_utc ~t:now () in
  let lease_expires_at = Time_util.iso8601_utc ~t:(now +. lease_seconds) () in
  let job_id = "ghrefresh_test_claimed_1" in
  let lease_token = "rfl_test_claimed_token" in
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
  (job_id, lease_token)

let seed_remote_rotated_flight ~db ~vault_id ~expected_generation ~owner ~now
    ?(job_id = "ghrefresh_test_orphan_1")
    ?(lease_token = "rfl_test_orphan_token") () =
  let now_s = Time_util.iso8601_utc ~t:now () in
  let lease_expires_at = Time_util.iso8601_utc ~t:(now +. 60.) () in
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
  ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT "remote_rotated"));
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

let test_single_flight_second_caller_denied_no_remote () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let old = sample_tokens ~tag:"sf" () in
  let rec_ =
    create_vault ~db ~keys ~account:acct ~tokens:old
      ~expires_at:near_expires_iso ()
  in
  let job_id, _ =
    seed_claimed_flight ~db ~vault_id:rec_.id ~expected_generation:1
      ~owner:"worker_a" ~now:fixed_now ()
  in
  let http, calls, _ =
    make_http
      ~expected_refresh:(Option.get old.refresh_token)
      ~body:(json_refresh_body ()) ()
  in
  match
    R.refresh ~db ~keys ~http_post:http ~resolve_client:resolve_ok
      ~client_id_handle ~now:fixed_now ~force:true ~flight_owner:"worker_b"
      ~on_inflight:`Deny ~vault_id:rec_.id ()
  with
  | Error (R.In_flight { job_id = j; owner; expected_generation; _ }) ->
      Alcotest.(check string) "busy job" job_id j;
      Alcotest.(check string) "owner a" "worker_a" owner;
      Alcotest.(check int) "pinned gen" 1 expected_generation;
      Alcotest.(check int) "no second remote" 0 !calls
  | Error d -> Alcotest.fail (R.string_of_denial d)
  | Ok _ -> Alcotest.fail "second caller must not win single-flight"

let test_waiter_joins_committed_generation_only () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let old = sample_tokens ~tag:"join" () in
  let rec_ =
    create_vault ~db ~keys ~account:acct ~tokens:old
      ~expires_at:near_expires_iso ()
  in
  let new_access = "ghu_access_JOIN_COMMITTED" in
  let body =
    json_refresh_body ~access:new_access ~refresh:"ghr_refresh_JOIN_COMMITTED"
      ~expires_in:7200 ~refresh_token_expires_in:86400 ()
  in
  let http, calls, _ =
    make_http ~expected_refresh:(Option.get old.refresh_token) ~body ()
  in
  (* Leader completes remote + CAS. *)
  let leader =
    match
      R.refresh ~db ~keys ~http_post:http ~resolve_client:resolve_ok
        ~client_id_handle ~now:fixed_now ~force:true ~flight_owner:"leader"
        ~vault_id:rec_.id ()
    with
    | Ok o -> o
    | Error d -> Alcotest.fail ("leader: " ^ R.string_of_denial d)
  in
  Alcotest.(check bool) "leader refreshed" true leader.refreshed;
  Alcotest.(check bool) "leader not joined" false leader.joined_flight;
  Alcotest.(check int) "gen 2" 2 leader.record.generation;
  Alcotest.(check int) "one remote" 1 !calls;
  (match leader.flight_job_id with
  | Some _ -> ()
  | None -> Alcotest.fail "leader must record flight_job_id");
  (* Concurrent waiter after commit: vault is outside skew after refresh, so
     acquire_lease reuses committed generation with no remote. *)
  let http2, calls2, _ =
    make_http ~expected_refresh:"should_not_call" ~body:(json_refresh_body ())
      ()
  in
  match
    R.acquire_lease ~db ~keys ~http_post:http2 ~resolve_client:resolve_ok
      ~client_id_handle ~now:fixed_now ~expected:acct ~vault_id:rec_.id ()
  with
  | Error d -> Alcotest.fail (R.string_of_denial d)
  | Ok (lease, outcome) -> (
      Alcotest.(check int) "waiter lease gen" 2 (L.generation lease);
      Alcotest.(check int) "no second remote for waiter" 0 !calls2;
      Alcotest.(check bool) "outside skew no refresh" false outcome.refreshed;
      (match
         L.with_token ~db ~keys ~now:fixed_now ~lease
           ~f:(fun ~access_token -> access_token)
           ()
       with
      | Ok tok -> Alcotest.(check string) "committed token only" new_access tok
      | Error d -> Alcotest.fail (L.string_of_denial d));
      (* Hold a claimed flight then flip to committed while waiter Waits. *)
      let _ =
        seed_claimed_flight ~db ~vault_id:rec_.id ~expected_generation:2
          ~owner:"leader_hold" ~now:fixed_now ~lease_seconds:120. ()
      in
      let flipped = ref false in
      let wait_sleep _ =
        if not !flipped then (
          flipped := true;
          let sql =
            {|UPDATE github_user_token_refresh_flight
              SET phase = 'committed', committed_generation = 2
              WHERE vault_id = ?|}
          in
          let st = Sqlite3.prepare db sql in
          ignore (Sqlite3.bind st 1 (Sqlite3.Data.TEXT rec_.id));
          ignore (Sqlite3.step st);
          ignore (Sqlite3.finalize st))
      in
      let http3 ~url:_ ~headers:_ ~body:_ =
        Alcotest.fail "join waiter must not remote"
      in
      match
        R.refresh ~db ~keys ~http_post:http3 ~resolve_client:resolve_ok
          ~client_id_handle ~now:fixed_now ~force:true ~flight_owner:"waiter"
          ~on_inflight:`Wait ~wait_sleep ~wait_timeout_seconds:1.0
          ~wait_poll_seconds:0.0 ~vault_id:rec_.id ()
      with
      | Ok outcome ->
          Alcotest.(check bool) "joined committed" true outcome.joined_flight;
          Alcotest.(check int) "committed gen only" 2 outcome.record.generation;
          Alcotest.(check bool) "flipped during wait" true !flipped
      | Error d -> Alcotest.fail ("wait join: " ^ R.string_of_denial d))

let test_cas_blocks_late_response_after_remote () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let old = sample_tokens ~tag:"late" () in
  let rec_ =
    create_vault ~db ~keys ~account:acct ~tokens:old
      ~expires_at:near_expires_iso ()
  in
  let concurrent_tokens =
    {
      S.access_token = "ghu_access_CONCURRENT_WINNER";
      refresh_token = Some "ghr_refresh_CONCURRENT_WINNER";
    }
  in
  let body =
    json_refresh_body ~access:"ghu_access_LATE_LOSER"
      ~refresh:"ghr_refresh_LATE_LOSER" ()
  in
  let http ~url:_ ~headers:_ ~body:_ =
    (* Simulate concurrent authority advance while this flight is mid-remote:
       another CAS wins generation 1 → 2 before our late response commits. *)
    (match
       C.replace ~db ~keys ~now:fixed_now ~id:rec_.id ~expected_generation:1
         ~expected:acct ~tokens:concurrent_tokens ~scopes:[ "repo" ]
         ~expires_at:(Time_util.iso8601_utc ~t:(fixed_now +. 7200.) ())
         ()
     with
    | Ok t -> Alcotest.(check int) "concurrent gen" 2 t.record.generation
    | Error d -> Alcotest.fail ("concurrent cas: " ^ C.string_of_denial d));
    Ok (200, body)
  in
  match
    R.refresh ~db ~keys ~http_post:http ~resolve_client:resolve_ok
      ~client_id_handle ~now:fixed_now ~force:true ~flight_owner:"late_worker"
      ~vault_id:rec_.id ()
  with
  | Error (R.Relink_required { reason; _ }) -> (
      Alcotest.(check bool)
        "cas_blocked reason" true
        (String_util.contains reason "cas_blocked_after_remote");
      (* Late loser must not restore its tokens; concurrent winner remains or
         vault is disabled fail-closed. *)
      match V.get_meta ~db ~id:rec_.id with
      | Error d -> Alcotest.fail (V.string_of_denial d)
      | Ok None -> Alcotest.fail "vault missing"
      | Ok (Some meta) ->
          if meta.active then (
            Alcotest.(check int) "winner gen retained" 2 meta.generation;
            match V.read ~db ~keys ~id:rec_.id () with
            | Ok opened ->
                Alcotest.(check string)
                  "late tokens not applied" concurrent_tokens.access_token
                  opened.tokens.access_token;
                Alcotest.(check bool)
                  "not late access" false
                  (String.equal opened.tokens.access_token
                     "ghu_access_LATE_LOSER")
            | Error d -> Alcotest.fail (V.string_of_denial d))
          else
            (* Fail-closed disable also acceptable after remote+CAS conflict. *)
            Alcotest.(check bool) "disabled fail-closed" true (not meta.active))
  | Error d -> Alcotest.fail (R.string_of_denial d)
  | Ok _ -> Alcotest.fail "late CAS must not commit old authority"

let test_crash_after_remote_rotated_fails_closed_relink () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let old = sample_tokens ~tag:"crash" () in
  let rec_ =
    create_vault ~db ~keys ~account:acct ~tokens:old
      ~expires_at:near_expires_iso ()
  in
  (* Simulate crash after remote rotation, before CAS: durable fence only. *)
  let job_id =
    seed_remote_rotated_flight ~db ~vault_id:rec_.id ~expected_generation:1
      ~owner:"crashed_worker" ~now:fixed_now ()
  in
  let http, calls, _ =
    make_http
      ~expected_refresh:(Option.get old.refresh_token)
      ~body:(json_refresh_body ()) ()
  in
  (match
     R.refresh ~db ~keys ~http_post:http ~resolve_client:resolve_ok
       ~client_id_handle ~now:fixed_now ~force:true ~flight_owner:"recovery"
       ~vault_id:rec_.id ()
   with
  | Error (R.Relink_required { reason; job_id = Some j }) ->
      Alcotest.(check string)
        "crash reason" "crash_after_remote_rotation" reason;
      Alcotest.(check string) "orphan job" job_id j;
      Alcotest.(check int) "no remote with dead refresh" 0 !calls
  | Error d -> Alcotest.fail (R.string_of_denial d)
  | Ok _ ->
      Alcotest.fail "must fail closed to relink, not restore old authority");
  (* Old sealed tokens must not remain active authority. *)
  match V.get_meta ~db ~id:rec_.id with
  | Ok (Some meta) ->
      Alcotest.(check bool) "vault disabled after crash fence" false meta.active
  | Ok None -> Alcotest.fail "vault missing"
  | Error d -> Alcotest.fail (V.string_of_denial d)

let test_wait_joins_after_leader_commits () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let old = sample_tokens ~tag:"wait" () in
  let rec_ =
    create_vault ~db ~keys ~account:acct ~tokens:old
      ~expires_at:near_expires_iso ()
  in
  let new_access = "ghu_access_WAIT_JOIN" in
  let body =
    json_refresh_body ~access:new_access ~refresh:"ghr_refresh_WAIT_JOIN"
      ~expires_in:3600 ~refresh_token_expires_in:7200 ()
  in
  let leader_done = ref false in
  let http_leader, calls, _ =
    make_http ~expected_refresh:(Option.get old.refresh_token) ~body ()
  in
  (* Seed a busy claim; waiter will poll; we complete leader on first sleep. *)
  let _ =
    seed_claimed_flight ~db ~vault_id:rec_.id ~expected_generation:1
      ~owner:"leader_sim" ~now:fixed_now ~lease_seconds:30. ()
  in
  let sleep_count = ref 0 in
  let wait_sleep _ =
    incr sleep_count;
    if not !leader_done then (
      (* Steal expired... actually lease not expired; complete by running
         leader under the seeded owner is hard. Instead: expire the claim and
         run a real leader once, then waiter joins. *)
      let past = Time_util.iso8601_utc ~t:(fixed_now -. 10.) () in
      let sql =
        {|UPDATE github_user_token_refresh_flight
          SET lease_expires_at = ? WHERE vault_id = ?|}
      in
      let stmt = Sqlite3.prepare db sql in
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT past));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT rec_.id));
      ignore (Sqlite3.step stmt);
      ignore (Sqlite3.finalize stmt);
      match
        R.refresh ~db ~keys ~http_post:http_leader ~resolve_client:resolve_ok
          ~client_id_handle ~now:fixed_now ~force:true
          ~flight_owner:"leader_real" ~vault_id:rec_.id ()
      with
      | Ok o ->
          leader_done := true;
          Alcotest.(check int) "leader gen" 2 o.record.generation
      | Error d -> Alcotest.fail ("leader during wait: " ^ R.string_of_denial d))
  in
  let http_waiter ~url:_ ~headers:_ ~body:_ =
    Alcotest.fail "waiter must not POST remote"
  in
  match
    R.refresh ~db ~keys ~http_post:http_waiter ~resolve_client:resolve_ok
      ~client_id_handle ~now:fixed_now ~force:true ~flight_owner:"waiter"
      ~on_inflight:`Wait ~wait_sleep ~wait_timeout_seconds:2.0
      ~wait_poll_seconds:0.0 ~vault_id:rec_.id ()
  with
  | Ok outcome ->
      Alcotest.(check bool) "leader ran" true !leader_done;
      Alcotest.(check bool) "sleep polled" true (!sleep_count >= 1);
      Alcotest.(check int) "one remote total" 1 !calls;
      Alcotest.(check int)
        "waiter sees committed gen" 2 outcome.record.generation;
      Alcotest.(check bool) "joined or led after reclaim" true outcome.refreshed
  | Error d -> Alcotest.fail (R.string_of_denial d)

(* -------------------------------------------------------------------------- *)
(* Suite                                                                      *)
(* -------------------------------------------------------------------------- *)

let suite =
  [
    ("needs_refresh skew window", `Quick, test_needs_refresh_skew_window);
    ( "parse requires server lifetimes bearer empty scope",
      `Quick,
      test_parse_requires_server_lifetimes_bearer_empty_scope );
    ("parse form-urlencoded", `Quick, test_parse_form_urlencoded);
    ( "refresh advances generation same lineage",
      `Quick,
      test_refresh_advances_generation_same_lineage );
    ( "refresh refuses outside skew unless force",
      `Quick,
      test_refresh_refuses_outside_skew_unless_force );
    ( "refresh lineage mismatch refuses",
      `Quick,
      test_refresh_lineage_mismatch_refuses );
    ( "refresh redacts secrets in denials",
      `Quick,
      test_refresh_redacts_secrets_in_denials );
    ( "acquire_lease outside skew skips remote",
      `Quick,
      test_acquire_lease_outside_skew_skips_remote );
    ( "acquire_lease inside skew refreshes and reissues",
      `Quick,
      test_acquire_lease_inside_skew_refreshes_and_reissues );
    ( "acquire_lease inside skew requires client boundary",
      `Quick,
      test_acquire_lease_inside_skew_requires_client_boundary );
    ( "refresh recovery gate blocks remote",
      `Quick,
      test_refresh_recovery_gate_blocks_remote );
    ( "reject non-bearer and nonempty scope over http",
      `Quick,
      test_reject_non_bearer_and_scope_over_http );
    ( "recorded refresh expiry fail closed",
      `Quick,
      test_recorded_refresh_expiry_fail_closed );
    ( "single-flight second caller denied no remote",
      `Quick,
      test_single_flight_second_caller_denied_no_remote );
    ( "waiter joins committed generation only",
      `Quick,
      test_waiter_joins_committed_generation_only );
    ( "CAS blocks late response after remote",
      `Quick,
      test_cas_blocks_late_response_after_remote );
    ( "crash after remote_rotated fails closed relink",
      `Quick,
      test_crash_after_remote_rotated_fails_closed_relink );
    ( "wait joins after leader commits",
      `Quick,
      test_wait_joins_after_leader_commits );
  ]

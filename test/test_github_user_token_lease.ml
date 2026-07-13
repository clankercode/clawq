(** Tests for callback-scoped opaque GitHub user-token leases (P21.M2.E4.T003).

    Contract under test:
    - Access snapshots / jobs / receipts / tools carry only opaque handle +
      binding + generation (via identity / JSON).
    - Raw token exists only inside [with_token] / [with_authorization_header].
    - Serialize / log helpers never embed token material.
    - Fail closed on expired, revoked, and wrong generation.
    - Runner / shell / Git transport surfaces are explicitly refused. *)

module V = Github_user_token_vault
module S = Github_user_token_store
module L = Github_user_token_lease

let () = Secret_store.test_iterations_override := Some 1

let aes_key =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-lease-test-master" ()

let sample_tokens =
  {
    S.access_token = "ghu_access_LEASE_PLAINTEXT_never_export";
    refresh_token = Some "ghr_refresh_LEASE_PLAINTEXT_never_export";
  }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let account ?(principal_id = "prin_lease_1") ?(github_user_id = 4242L)
    ?(app_id = 99) ?(host = V.default_host) () =
  assert_ok (V.make_account_key ~principal_id ~github_user_id ~app_id ~host ())

let make_keys ?(key_id = "mk-lease-1") ?(key_version = 1) () =
  assert_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key ())

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  V.ensure_schema db;
  Fun.protect
    ~finally:(fun () ->
      ignore (L.discard_all ());
      ignore (Sqlite3.db_close db))
    (fun () -> f db)

let fixed_now = 1_720_000_000.0

(* Token expiry far in the future relative to fixed_now. *)
let far_expires = "2026-12-01T00:00:00Z"

let create_vault ~db ?(keys = make_keys ()) ?(account = account ())
    ?(tokens = sample_tokens) ?(id = "ghvault_lease_1")
    ?(expires_at = far_expires) () =
  match
    V.create ~db ~keys ~id ~now:fixed_now ~account ~tokens
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

(* -------------------------------------------------------------------------- *)
(* Happy path                                                                 *)
(* -------------------------------------------------------------------------- *)

let test_issue_identity_has_handle_binding_generation () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let acct = account () in
  let rec_ = create_vault ~db ~keys ~account:acct () in
  let lease =
    issue_ok ~db ~binding_id:"bind_abc" ~expected:acct ~vault_id:rec_.id ()
  in
  let id = L.identity_of lease in
  Alcotest.(check bool)
    "handle non-empty" true
    (String.length (L.handle_to_string id.handle) > 0);
  Alcotest.(check string) "principal" acct.principal_id id.binding.principal_id;
  Alcotest.(check int64) "user" acct.github_user_id id.binding.github_user_id;
  Alcotest.(check int) "app" acct.app_id id.binding.app_id;
  Alcotest.(check string) "host" acct.host id.binding.host;
  Alcotest.(check string) "vault_id" rec_.id id.binding.vault_id;
  Alcotest.(check int) "generation" 1 id.binding.generation;
  Alcotest.(check (option string))
    "binding_id" (Some "bind_abc") id.binding.binding_id;
  Alcotest.(check bool) "not revoked" false id.revoked;
  Alcotest.(check bool)
    "not expired at issue" false
    (L.is_expired ~now:fixed_now lease)

let test_with_token_opens_only_inside_callback () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let lease = issue_ok ~db ~vault_id:rec_.id () in
  let seen = ref None in
  (match
     L.with_token ~db ~keys ~now:fixed_now ~lease
       ~f:(fun ~access_token ->
         seen := Some access_token;
         "http-ok")
       ()
   with
  | Ok "http-ok" -> ()
  | Ok other -> Alcotest.fail ("unexpected result: " ^ other)
  | Error d -> Alcotest.fail (L.string_of_denial d));
  Alcotest.(check (option string))
    "callback saw access token" (Some sample_tokens.access_token) !seen;
  (* Lease identity / JSON never carry the token after use. *)
  let id = L.identity_of lease in
  Alcotest.(check bool)
    "identity no access" false
    (L.identity_contains_plaintext ~identity:id
       ~plaintext:sample_tokens.access_token);
  Alcotest.(check bool)
    "identity no refresh" false
    (L.identity_contains_plaintext ~identity:id
       ~plaintext:(Option.get sample_tokens.refresh_token))

let test_with_authorization_header () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let lease = issue_ok ~db ~vault_id:rec_.id () in
  match
    L.with_authorization_header ~db ~keys ~now:fixed_now ~lease
      ~f:(fun ~headers -> headers)
      ()
  with
  | Error d -> Alcotest.fail (L.string_of_denial d)
  | Ok headers ->
      let auth =
        List.find_opt (fun (n, _) -> n = "Authorization") headers
        |> Option.map snd
      in
      Alcotest.(check (option string))
        "bearer"
        (Some ("Bearer " ^ sample_tokens.access_token))
        auth

(* -------------------------------------------------------------------------- *)
(* Serialize / log paths never see token                                      *)
(* -------------------------------------------------------------------------- *)

let test_json_and_log_redact_token_material () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let lease = issue_ok ~db ~vault_id:rec_.id () in
  let json = L.to_json lease in
  Alcotest.(check bool)
    "json no access" false
    (L.json_contains_plaintext ~json ~plaintext:sample_tokens.access_token);
  Alcotest.(check bool)
    "json no refresh" false
    (L.json_contains_plaintext ~json
       ~plaintext:(Option.get sample_tokens.refresh_token));
  Alcotest.(check bool)
    "json no aes" false
    (L.json_contains_plaintext ~json ~plaintext:aes_key);
  let summary = L.string_of_identity (L.identity_of lease) in
  Alcotest.(check bool)
    "log summary no access" false
    (String_util.contains summary sample_tokens.access_token);
  Alcotest.(check bool)
    "log summary no refresh" false
    (String_util.contains summary (Option.get sample_tokens.refresh_token));
  (* Round-trip identity JSON still has no secrets. *)
  match L.identity_of_json json with
  | Error e -> Alcotest.fail e
  | Ok id ->
      Alcotest.(check string)
        "handle roundtrip"
        (L.handle_to_string (L.handle lease))
        (L.handle_to_string id.handle);
      Alcotest.(check int) "gen roundtrip" 1 id.binding.generation;
      Alcotest.(check bool)
        "parsed identity no access" false
        (L.identity_contains_plaintext ~identity:id
           ~plaintext:sample_tokens.access_token)

let test_denial_never_embeds_token () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let lease = issue_ok ~db ~ttl_seconds:1.0 ~vault_id:rec_.id () in
  L.revoke lease;
  match
    L.with_token ~db ~keys ~now:fixed_now ~lease
      ~f:(fun ~access_token -> access_token)
      ()
  with
  | Ok _ -> Alcotest.fail "expected revoked"
  | Error d ->
      Alcotest.(check bool)
        "denial no access" false
        (L.denial_exposes_token ~denial:d ~plaintext:sample_tokens.access_token);
      Alcotest.(check bool)
        "denial no refresh" false
        (L.denial_exposes_token ~denial:d
           ~plaintext:(Option.get sample_tokens.refresh_token))

(* -------------------------------------------------------------------------- *)
(* Fail closed: expired / revoked / wrong generation                          *)
(* -------------------------------------------------------------------------- *)

let test_fail_closed_lease_expired () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let lease = issue_ok ~db ~ttl_seconds:10.0 ~vault_id:rec_.id () in
  match
    L.with_token ~db ~keys ~now:(fixed_now +. 11.) ~lease
      ~f:(fun ~access_token:_ -> "should-not-run")
      ()
  with
  | Error L.Lease_expired -> ()
  | Error d ->
      Alcotest.fail ("expected Lease_expired, got " ^ L.string_of_denial d)
  | Ok _ -> Alcotest.fail "expired lease should fail closed"

let test_fail_closed_revoked () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let lease = issue_ok ~db ~vault_id:rec_.id () in
  L.revoke lease;
  Alcotest.(check bool) "is_revoked" true (L.is_revoked lease);
  match
    L.with_token ~db ~keys ~now:fixed_now ~lease
      ~f:(fun ~access_token:_ -> ())
      ()
  with
  | Error L.Lease_revoked -> ()
  | Error d -> Alcotest.fail (L.string_of_denial d)
  | Ok () -> Alcotest.fail "revoked lease should fail closed"

let test_fail_closed_wrong_generation () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let lease = issue_ok ~db ~vault_id:rec_.id () in
  Alcotest.(check int) "pinned gen" 1 (L.generation lease);
  (* Advance vault generation under CAS (simulates refresh/replace). *)
  let rotated =
    {
      S.access_token = "ghu_rotated_LEASE_PLAINTEXT";
      refresh_token = Some "ghr_rotated_LEASE_PLAINTEXT";
    }
  in
  (match
     V.replace ~db ~keys ~now:(fixed_now +. 1.) ~id:rec_.id
       ~expected_generation:1 ~tokens:rotated ~scopes:[ "repo" ]
       ~expires_at:far_expires ()
   with
  | Ok updated -> Alcotest.(check int) "vault gen advanced" 2 updated.generation
  | Error d -> Alcotest.fail (V.string_of_denial d));
  match
    L.with_token ~db ~keys ~now:fixed_now ~lease
      ~f:(fun ~access_token:_ -> "stale")
      ()
  with
  | Error (L.Generation_mismatch { expected = 1; actual = 2 }) -> ()
  | Error d ->
      Alcotest.fail ("expected Generation_mismatch, got " ^ L.string_of_denial d)
  | Ok _ -> Alcotest.fail "stale generation must fail closed"

let test_fail_closed_token_expired () =
  with_db @@ fun db ->
  let keys = make_keys () in
  (* expires_at in the past relative to fixed_now *)
  let past = "2020-01-01T00:00:00Z" in
  match
    V.create ~db ~keys ~id:"ghvault_expired" ~now:fixed_now
      ~account:(account ()) ~tokens:sample_tokens ~scopes:[] ~expires_at:past ()
  with
  | Ok rec_ -> (
      match L.issue ~db ~now:fixed_now ~vault_id:rec_.id () with
      | Error L.Token_expired -> ()
      | Error d ->
          Alcotest.fail
            ("expected Token_expired at issue, got " ^ L.string_of_denial d)
      | Ok _ -> Alcotest.fail "issuing lease for expired token should fail")
  | Error d -> Alcotest.fail (V.string_of_denial d)

let test_fail_closed_account_mismatch_at_issue () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ =
    create_vault ~db ~keys ~account:(account ~principal_id:"p1" ()) ()
  in
  let wrong = account ~principal_id:"p2" () in
  match L.issue ~db ~now:fixed_now ~expected:wrong ~vault_id:rec_.id () with
  | Error (L.Account_mismatch _) -> ()
  | Error d -> Alcotest.fail (L.string_of_denial d)
  | Ok _ -> Alcotest.fail "account mismatch should fail at issue"

let test_fail_closed_missing_vault () =
  with_db @@ fun db ->
  match L.issue ~db ~now:fixed_now ~vault_id:"does_not_exist" () with
  | Error L.Lease_not_found -> ()
  | Error d -> Alcotest.fail (L.string_of_denial d)
  | Ok _ -> Alcotest.fail "missing vault should fail"

(* -------------------------------------------------------------------------- *)
(* Invalidation hooks + forbidden surfaces                                    *)
(* -------------------------------------------------------------------------- *)

let test_invalidate_generation_and_discard () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let l1 = issue_ok ~db ~vault_id:rec_.id () in
  let l2 = issue_ok ~db ~vault_id:rec_.id () in
  Alcotest.(check bool) "live before" true (L.live_count () >= 2);
  let n = L.invalidate_generation ~vault_id:rec_.id ~generation:1 in
  Alcotest.(check bool) "invalidated some" true (n >= 2);
  Alcotest.(check bool) "l1 revoked" true (L.is_revoked l1);
  Alcotest.(check bool) "l2 revoked" true (L.is_revoked l2);
  match
    L.with_token ~db ~keys ~now:fixed_now ~lease:l1
      ~f:(fun ~access_token:_ -> ())
      ()
  with
  | Error L.Lease_revoked -> ()
  | Error d -> Alcotest.fail (L.string_of_denial d)
  | Ok () -> Alcotest.fail "invalidated lease should fail"

let test_discard_for_vault () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let r1 =
    create_vault ~db ~keys ~id:"v1" ~account:(account ~principal_id:"a" ()) ()
  in
  let r2 =
    create_vault ~db ~keys ~id:"v2"
      ~account:(account ~principal_id:"b" ~github_user_id:99L ())
      ()
  in
  let l1 = issue_ok ~db ~vault_id:r1.id () in
  let l2 = issue_ok ~db ~vault_id:r2.id () in
  let n = L.discard_for_vault ~vault_id:r1.id in
  Alcotest.(check int) "discarded one vault" 1 n;
  Alcotest.(check bool) "l1 revoked" true (L.is_revoked l1);
  Alcotest.(check bool) "l2 still live" false (L.is_revoked l2)

let test_refuse_runner_shell_git_surfaces () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let lease = issue_ok ~db ~vault_id:rec_.id () in
  (match L.refuse_runner_env lease with
  | Error (L.Forbidden_surface s) ->
      Alcotest.(check bool) "runner msg" true (String_util.contains s "runner")
  | _ -> Alcotest.fail "runner must refuse");
  (match L.refuse_shell_injection lease with
  | Error (L.Forbidden_surface s) ->
      Alcotest.(check bool) "shell msg" true (String_util.contains s "shell")
  | _ -> Alcotest.fail "shell must refuse");
  match L.refuse_git_transport lease with
  | Error (L.Forbidden_surface s) ->
      Alcotest.(check bool) "git msg" true (String_util.contains s "Git")
  | _ -> Alcotest.fail "git must refuse"

let test_assert_all_non_http_surfaces_refused () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  let lease = issue_ok ~db ~vault_id:rec_.id () in
  Alcotest.(check bool)
    "all surfaces listed" true
    (List.length L.all_non_http_surfaces >= 10);
  List.iter
    (fun surface ->
      match L.refuse lease surface with
      | Error (L.Forbidden_surface _) -> ()
      | Error d ->
          Alcotest.fail
            ("expected Forbidden_surface for "
            ^ L.string_of_non_http_surface surface
            ^ " got " ^ L.string_of_denial d)
      | Ok () ->
          Alcotest.fail
            ("surface incorrectly permitted: "
            ^ L.string_of_non_http_surface surface))
    L.all_non_http_surfaces;
  match L.assert_non_http_refused lease with
  | Ok () -> ()
  | Error e -> Alcotest.fail e

let test_token_shape_scan_and_refuse_material () =
  let clean = "dispatch ok mode=user lease=ghlease_1 gen=1" in
  let dirty_ghu = "Authorization: Bearer ghu_access_SCAN_PLAINTEXT_never" in
  let dirty_env = "GITHUB_TOKEN=ghu_abc123_token_value" in
  let dirty_pat = "export TOKEN=github_pat_11AAAA_secret" in
  Alcotest.(check bool) "clean free" false (L.text_contains_token_shape clean);
  Alcotest.(check bool) "ghu hit" true (L.text_contains_token_shape dirty_ghu);
  Alcotest.(check bool) "env hit" true (L.text_contains_token_shape dirty_env);
  Alcotest.(check bool) "pat hit" true (L.text_contains_token_shape dirty_pat);
  Alcotest.(check bool)
    "argv hit" true
    (L.argv_contains_token_shape
       [| "git"; "push"; "https://x-access-token:ghu_xyz@github.com/o/r.git" |]);
  Alcotest.(check bool)
    "env list hit" true
    (L.env_entries_contain_token_shape
       [ "PATH=/usr/bin"; "GH_TOKEN=gho_server_token_value" ]);
  (match L.refuse_scanned_material ~surface:L.Runner_env ~material:clean with
  | Ok () -> ()
  | Error d -> Alcotest.fail (L.string_of_denial d));
  (match
     L.refuse_scanned_material ~surface:L.Git_transport ~material:dirty_ghu
   with
  | Error (L.Forbidden_surface s) ->
      Alcotest.(check bool)
        "scan msg" true
        (String_util.contains s "token shape"
        || String_util.contains s "git_transport")
  | _ -> Alcotest.fail "dirty git transport material must refuse");
  match
    L.assert_materials_token_free
      ~materials:
        [
          (L.Job_payload, clean); (L.Shell, dirty_env); (L.Crash_output, clean);
        ]
  with
  | Error (L.Forbidden_surface _) -> ()
  | Ok () -> Alcotest.fail "mixed materials must fail on dirty entry"
  | Error d -> Alcotest.fail (L.string_of_denial d)

let test_issue_from_record_no_decrypt () =
  with_db @@ fun db ->
  let keys = make_keys () in
  let rec_ = create_vault ~db ~keys () in
  (* issue_from_record only needs metadata — no keys provider. *)
  match L.issue_from_record ~db ~now:fixed_now ~record:rec_ () with
  | Error d -> Alcotest.fail (L.string_of_denial d)
  | Ok lease ->
      Alcotest.(check string) "vault" rec_.id (L.vault_id lease);
      Alcotest.(check int) "gen" 1 (L.generation lease)

let suite =
  [
    Alcotest.test_case "issue exposes handle + binding + generation" `Quick
      test_issue_identity_has_handle_binding_generation;
    Alcotest.test_case "with_token opens raw only inside callback" `Quick
      test_with_token_opens_only_inside_callback;
    Alcotest.test_case "with_authorization_header for GitHub HTTP" `Quick
      test_with_authorization_header;
    Alcotest.test_case "JSON and log paths never see token material" `Quick
      test_json_and_log_redact_token_material;
    Alcotest.test_case "denials never embed token" `Quick
      test_denial_never_embeds_token;
    Alcotest.test_case "fail closed on lease expiry" `Quick
      test_fail_closed_lease_expired;
    Alcotest.test_case "fail closed on revoke" `Quick test_fail_closed_revoked;
    Alcotest.test_case "fail closed on wrong generation" `Quick
      test_fail_closed_wrong_generation;
    Alcotest.test_case "fail closed on token expired at issue" `Quick
      test_fail_closed_token_expired;
    Alcotest.test_case "fail closed on account mismatch at issue" `Quick
      test_fail_closed_account_mismatch_at_issue;
    Alcotest.test_case "fail closed on missing vault" `Quick
      test_fail_closed_missing_vault;
    Alcotest.test_case "invalidate_generation revokes live leases" `Quick
      test_invalidate_generation_and_discard;
    Alcotest.test_case "discard_for_vault is selective" `Quick
      test_discard_for_vault;
    Alcotest.test_case "refuse runner/shell/git surfaces" `Quick
      test_refuse_runner_shell_git_surfaces;
    Alcotest.test_case "assert all non-HTTP surfaces refuse lease" `Quick
      test_assert_all_non_http_surfaces_refused;
    Alcotest.test_case "token shape scan refuses dirty materials" `Quick
      test_token_shape_scan_and_refuse_material;
    Alcotest.test_case "issue_from_record needs no decrypt" `Quick
      test_issue_from_record_no_decrypt;
  ]
